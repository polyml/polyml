(*
    Copyright (c) 2016-19 David C.J. Matthews

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

functor X86FOREIGNCALL(

    structure X86CODE: X86CODESIG

    structure X86OPTIMISE:
    sig
        type operation
        type code
        type operations = operation list
        type closureRef

        (* Optimise and code-generate. *)
        val generateCode: {code: code, ops: operations, labelCount: int, resultClosure: closureRef} -> unit

        structure Sharing:
        sig
            type operation = operation
            type code = code
            type closureRef = closureRef
        end
    end

    structure DEBUG: DEBUGSIG
    
    structure CODE_ARRAY: CODEARRAYSIG

    sharing X86CODE.Sharing = X86OPTIMISE.Sharing = CODE_ARRAY.Sharing
): FOREIGNCALLSIG
=
struct
    open X86CODE
    open Address
    open CODE_ARRAY
    
    val memRegSize = 0
   
    val (polyWordOpSize, nativeWordOpSize) =
        case targetArch of
            Native32Bit     => (OpSize32, OpSize32)
        |   Native64Bit     => (OpSize64, OpSize64)
        |   ObjectId32Bit   => (OpSize32, OpSize64)
    
    (* Ebx/Rbx is used for the second argument on the native architectures but
       is replaced by esi on the object ID arch because ebx is used as the
       global base register. *)
    val mlArg2Reg = case targetArch of ObjectId32Bit => esi | _ => ebx
    
    exception InternalError = Misc.InternalError
  
    fun opSizeToMove OpSize32 = Move32 | opSizeToMove OpSize64 = Move64

    val pushR = PushToStack o RegisterArg

    fun moveRR{source, output, opSize} =
        Move{source=RegisterArg source, destination=RegisterArg output, moveSize=opSizeToMove opSize}

    fun loadMemory(reg, base, offset, opSize) =
        Move{source=MemoryArg{base=base, offset=offset, index=NoIndex}, destination=RegisterArg reg, moveSize=opSizeToMove opSize}
    and storeMemory(reg, base, offset, opSize) =
        Move{source=RegisterArg reg, destination=MemoryArg {base=base, offset=offset, index=NoIndex}, moveSize=opSizeToMove opSize}
    
    val loadHeapMemory =
        case targetArch of
            ObjectId32Bit =>
                (
                    fn (reg, base, offset, opSize) => 
                        Move{source=MemoryArg{base=ebx, offset=offset, index=Index4 base},
                             destination=RegisterArg reg, moveSize=opSizeToMove opSize}
                )
        |   _ => loadMemory

    fun createProfileObject _ (*functionName*) =
    let
        (* The profile object is a single mutable with the F_bytes bit set. *)
        open Address
        val profileObject = RunCall.allocateByteMemory(0w1, Word.fromLargeWord(Word8.toLargeWord(Word8.orb(F_mutable, F_bytes))))
        fun clear 0w0 = ()
        |   clear i = (assignByte(profileObject, i-0w1, 0w0); clear (i-0w1))
        val () = clear wordSize
    in
        toMachineWord profileObject
    end

    val makeEntryPoint: string -> machineWord = RunCall.rtsCallFull1 "PolyCreateEntryPointObject"

    datatype abi = X86_32 | X64Win | X64Unix
    
    local
        (* Get the ABI.  On 64-bit Windows and Unix use different calling conventions. *)
        val getABICall: unit -> int = RunCall.rtsCallFast0 "PolyGetABI"
    in
        fun getABI() =
            case getABICall() of
                0 => X86_32
            |   1 => X64Unix
            |   2 => X64Win
            |   n => raise InternalError ("Unknown ABI type " ^ Int.toString n)
    end

    val noException = 1

    (* Full RTS call version.  An extra argument is passed that contains the thread ID.
       This allows the taskData object to be found which is needed if the code allocates
       any ML memory or raises an exception.  It also saves the stack and heap pointers
       in case of a GC. *)
    fun rtsCallFull (functionName, nArgs (* Not counting the thread ID *), debugSwitches) =
    let
        val entryPointAddr = makeEntryPoint functionName

        (* Get the ABI.  On 64-bit Windows and Unix use different calling conventions. *)
        val abi = getABI()

        (* Branch to check for exception. *)
        val exLabel = Label{labelNo=0} (* There's just one label in this function. *)
        
        (* Unix X64.  The first six arguments are in rdi, rsi, rdx, rcx, r8, r9.
                      The rest are on the stack.
           Windows X64. The first four arguments are in rcx, rdx, r8 and r9.  The rest are
                       on the stack.  The caller must ensure the stack is aligned on 16-byte boundary
                       and must allocate 32-byte save area for the register args.
                       rbx, rbp, rdi, rsi, rsp, r12-r15 are saved by the called function.
           X86/32.  Arguments are pushed to the stack.
                   ebx, edi, esi, ebp and esp are saved by the called function.
                   We use esi to hold the argument data pointer and edi to save the ML stack pointer
           Our ML conventions use eax, ebx for the first two arguments in X86/32,
                   rax, ebx, r8, r9, r10 for the first five arguments in X86/64 and
                   rax, rsi, r8, r9 and r10 for the first five arguments in X86/64-32 bit.
        *)
        
        (* Previously the ML stack pointer was saved in a callee-save register.  This works
           in almost all circumstances except when a call to the FFI code results in a callback
           and the callback moves the ML stack.  Instead the RTS callback handler adjusts the value
           in memRegStackPtr and we reload the ML stack pointer from there. *)
        val entryPtrReg = if targetArch <> Native32Bit then r11 else ecx
        
        val stackSpace =
            case abi of
                X64Unix => memRegSize
            |   X64Win => memRegSize + 32 (* Requires 32-byte save area. *)
            |   X86_32 =>
                let
                    (* GCC likes to keep the stack on a 16-byte alignment. *)
                    val argSpace = (nArgs+1)*4
                    val align = argSpace mod 16
                in
                    (* Add sufficient space so that esp will be 16-byte aligned *)
                    if align = 0
                    then memRegSize
                    else memRegSize + 16 - align
                end

        (* The RTS functions expect the real address of the thread Id. *)
        fun loadThreadId toReg =
            if targetArch <> ObjectId32Bit
            then [loadMemory(toReg, ebp, memRegThreadSelf, nativeWordOpSize)]
            else [loadMemory(toReg, ebp, memRegThreadSelf, polyWordOpSize),
                  LoadAddress{output=toReg, offset=0, base=SOME ebx, index=Index4 toReg, opSize=nativeWordOpSize}]

        val code =
            [
                Move{source=AddressConstArg entryPointAddr, destination=RegisterArg entryPtrReg, moveSize=opSizeToMove polyWordOpSize}, (* Load the entry point ref. *)
                loadHeapMemory(entryPtrReg, entryPtrReg, 0, nativeWordOpSize)(* Load its value. *)
            ] @
            (
                (* Save heap ptr.  This is in r15 in X86/64 *)
                if targetArch <> Native32Bit then [storeMemory(r15, ebp, memRegLocalMPointer, nativeWordOpSize)] (* Save heap ptr *)
                else []
            ) @
            (
                if abi = X86_32 andalso nArgs >= 3
                then [moveRR{source=esp, output=edi, opSize=nativeWordOpSize}] (* Needed if we have to load from the stack. *)
                else []
            ) @
            
            [
                (* Have to save the stack pointer to the arg structure in case we need to scan the stack for a GC. *)
                storeMemory(esp, ebp, memRegStackPtr, nativeWordOpSize), (* Save ML stack and switch to C stack. *)
                loadMemory(esp, ebp, memRegCStackPtr, nativeWordOpSize), (*moveRR{source=ebp, output=esp},*) (* Load the saved C stack pointer. *)
                (* Set the stack pointer past the data on the stack.  For Windows/64 add in a 32 byte save area *)
                ArithToGenReg{opc=SUB, output=esp, source=NonAddressConstArg(LargeInt.fromInt stackSpace), opSize=nativeWordOpSize}
            ] @
            (
                case (abi, nArgs) of  (* Set the argument registers. *)
                    (X64Unix, 0) => loadThreadId edi
                |   (X64Unix, 1) => moveRR{source=eax, output=esi, opSize=polyWordOpSize} :: loadThreadId edi
                |   (X64Unix, 2) =>
                        moveRR{source=mlArg2Reg, output=edx, opSize=polyWordOpSize} ::
                        moveRR{source=eax, output=esi, opSize=polyWordOpSize} :: loadThreadId edi
                |   (X64Unix, 3) => 
                        moveRR{source=mlArg2Reg, output=edx, opSize=polyWordOpSize} :: moveRR{source=eax, output=esi, opSize=polyWordOpSize} ::
                        moveRR{source=r8, output=ecx, opSize=polyWordOpSize} :: loadThreadId edi
                |   (X64Win, 0) => loadThreadId ecx
                |   (X64Win, 1) => moveRR{source=eax, output=edx, opSize=polyWordOpSize} :: loadThreadId ecx
                |   (X64Win, 2) =>
                        moveRR{source=eax, output=edx, opSize=polyWordOpSize} ::
                        moveRR{source=mlArg2Reg, output=r8, opSize=polyWordOpSize} :: loadThreadId ecx
                |   (X64Win, 3) =>
                        moveRR{source=eax, output=edx, opSize=polyWordOpSize} :: moveRR{source=r8, output=r9, opSize=polyWordOpSize} ::
                        moveRR{source=mlArg2Reg, output=r8, opSize=polyWordOpSize} :: loadThreadId ecx
                |   (X86_32, 0) => [ PushToStack(MemoryArg{base=ebp, offset=memRegThreadSelf, index=NoIndex}) ]
                |   (X86_32, 1) => [ pushR eax, PushToStack(MemoryArg{base=ebp, offset=memRegThreadSelf, index=NoIndex}) ]
                |   (X86_32, 2) => [ pushR mlArg2Reg, pushR eax, PushToStack(MemoryArg{base=ebp, offset=memRegThreadSelf, index=NoIndex}) ]
                |   (X86_32, 3) =>
                        [
                            (* We need to move an argument from the ML stack. *)
                            PushToStack(MemoryArg{base=edi, offset=4, index=NoIndex}), pushR mlArg2Reg, pushR eax,
                            PushToStack(MemoryArg{base=ebp, offset=memRegThreadSelf, index=NoIndex})
                        ]
                |   _ => raise InternalError "rtsCall: Abi/argument count not implemented"
            ) @
            [
                CallAddress(RegisterArg entryPtrReg), (* Call the function *)
                loadMemory(esp, ebp, memRegStackPtr, nativeWordOpSize) (* Restore the ML stack pointer. *)
            ] @
            (
            if targetArch <> Native32Bit then [loadMemory(r15, ebp, memRegLocalMPointer, nativeWordOpSize) ] (* Copy back the heap ptr *)
            else []
            ) @
            [
                ArithMemConst{opc=CMP, address={offset=memRegExceptionPacket, base=ebp, index=NoIndex}, source=noException, opSize=polyWordOpSize},
                ConditionalBranch{test=JNE, label=exLabel},
                (* Remove any arguments that have been passed on the stack. *)
                ReturnFromFunction(Int.max(case abi of X86_32 => nArgs-2 | _ => nArgs-5, 0)),
                JumpLabel exLabel, (* else raise the exception *)
                loadMemory(eax, ebp, memRegExceptionPacket, polyWordOpSize),
                RaiseException { workReg=ecx }
            ]
 
        val profileObject = createProfileObject functionName
        val newCode = codeCreate (functionName, profileObject, debugSwitches)
        val closure = makeConstantClosure()
        val () = X86OPTIMISE.generateCode{code=newCode, labelCount=1(*One label.*), ops=code, resultClosure=closure}
    in
        closureAsAddress closure
    end

    (* This is a quicker version but can only be used if the RTS entry does
       not allocated ML memory, raise an exception or need to suspend the thread. *)
    datatype fastArgs = FastArgFixed | FastArgDouble | FastArgFloat


    fun rtsCallFastGeneral (functionName, argFormats, (*resultFormat*) _, debugSwitches) =
    let
        val entryPointAddr = makeEntryPoint functionName

        (* Get the ABI.  On 64-bit Windows and Unix use different calling conventions. *)
        val abi = getABI()

        val (entryPtrReg, saveMLStackPtrReg) =
            if targetArch <> Native32Bit then (r11, r13) else (ecx, edi)
        
        val stackSpace =
            case abi of
                X64Unix => memRegSize
            |   X64Win => memRegSize + 32 (* Requires 32-byte save area. *)
            |   X86_32 =>
                let
                    (* GCC likes to keep the stack on a 16-byte alignment. *)
                    val argSpace = List.foldl(fn (FastArgDouble, n) => n+8 | (_, n) => n+4) 0 argFormats
                    val align = argSpace mod 16
                in
                    (* Add sufficient space so that esp will be 16-byte aligned *)
                    if align = 0
                    then memRegSize
                    else memRegSize + 16 - align
                end

        (* The number of ML arguments passed on the stack. *)
        val mlArgsOnStack =
            Int.max(case abi of X86_32 => List.length argFormats - 2 | _ => List.length argFormats - 5, 0)

        val code =
            [
                Move{source=AddressConstArg entryPointAddr, destination=RegisterArg entryPtrReg, moveSize=opSizeToMove polyWordOpSize}, (* Load the entry point ref. *)
                loadHeapMemory(entryPtrReg, entryPtrReg, 0, nativeWordOpSize),(* Load its value. *)
                moveRR{source=esp, output=saveMLStackPtrReg, opSize=nativeWordOpSize}, (* Save ML stack and switch to C stack. *)
                loadMemory(esp, ebp, memRegCStackPtr, nativeWordOpSize),
                (* Set the stack pointer past the data on the stack.  For Windows/64 add in a 32 byte save area *)
                ArithToGenReg{opc=SUB, output=esp, source=NonAddressConstArg(LargeInt.fromInt stackSpace), opSize=nativeWordOpSize}
            ] @
            (
                case (abi, argFormats) of  (* Set the argument registers. *)
                    (_, []) => []
                |   (X64Unix, [FastArgFixed]) => [ moveRR{source=eax, output=edi, opSize=polyWordOpSize} ]
                |   (X64Unix, [FastArgFixed, FastArgFixed]) =>
                        (* Since mlArgs2Reg is esi on 32-in-64 this is redundant. *)
                        [ moveRR{source=mlArg2Reg, output=esi, opSize=polyWordOpSize}, moveRR{source=eax, output=edi, opSize=polyWordOpSize} ]
                |   (X64Unix, [FastArgFixed, FastArgFixed, FastArgFixed]) => 
                        [ moveRR{source=mlArg2Reg, output=esi, opSize=polyWordOpSize}, moveRR{source=eax, output=edi, opSize=polyWordOpSize},
                          moveRR{source=r8, output=edx, opSize=polyWordOpSize} ]
                |   (X64Unix, [FastArgFixed, FastArgFixed, FastArgFixed, FastArgFixed]) => 
                        [ moveRR{source=mlArg2Reg, output=esi, opSize=polyWordOpSize}, moveRR{source=eax, output=edi, opSize=polyWordOpSize},
                          moveRR{source=r8, output=edx, opSize=polyWordOpSize}, moveRR{source=r9, output=ecx, opSize=polyWordOpSize} ]
                |   (X64Win, [FastArgFixed]) => [ moveRR{source=eax, output=ecx, opSize=polyWordOpSize} ]
                |   (X64Win, [FastArgFixed, FastArgFixed]) => [ moveRR{source=eax, output=ecx, opSize=polyWordOpSize}, moveRR{source=mlArg2Reg, output=edx, opSize=polyWordOpSize} ]
                |   (X64Win, [FastArgFixed, FastArgFixed, FastArgFixed]) =>
                        [ moveRR{source=eax, output=ecx, opSize=polyWordOpSize}, moveRR{source=mlArg2Reg, output=edx, opSize=polyWordOpSize} (* Arg3 is already in r8. *) ]
                |   (X64Win, [FastArgFixed, FastArgFixed, FastArgFixed, FastArgFixed]) =>
                        [ moveRR{source=eax, output=ecx, opSize=polyWordOpSize}, moveRR{source=mlArg2Reg, output=edx, opSize=polyWordOpSize} (* Arg3 is already in r8 and arg4 in r9. *) ]
                |   (X86_32, [FastArgFixed]) => [ pushR eax ]
                |   (X86_32, [FastArgFixed, FastArgFixed]) => [ pushR mlArg2Reg, pushR eax ]
                |   (X86_32, [FastArgFixed, FastArgFixed, FastArgFixed]) =>
                        [
                            (* We need to move an argument from the ML stack. *)
                            loadMemory(edx, saveMLStackPtrReg, 4, polyWordOpSize), pushR edx, pushR mlArg2Reg, pushR eax
                        ]
                |   (X86_32, [FastArgFixed, FastArgFixed, FastArgFixed, FastArgFixed]) =>
                        [
                            (* We need to move an arguments from the ML stack. *)
                            loadMemory(edx, saveMLStackPtrReg, 4, polyWordOpSize), pushR edx,
                            loadMemory(edx, saveMLStackPtrReg, 8, polyWordOpSize), pushR edx,
                            pushR mlArg2Reg, pushR eax
                        ]

                    (* One "double" argument.  The value needs to be unboxed. *)
                |   (X86_32, [FastArgDouble]) =>
                     (* eax contains the address of the value.  This must be unboxed onto the stack. *)
                    [
                        FPLoadFromMemory{address={base=eax, offset=0, index=NoIndex}, precision=DoublePrecision},
                        ArithToGenReg{ opc=SUB, output=esp, source=NonAddressConstArg 8, opSize=nativeWordOpSize},
                        FPStoreToMemory{ address={base=esp, offset=0, index=NoIndex}, precision=DoublePrecision, andPop=true }
                    ]

                |   (_, [FastArgDouble]) => [ (* Already in xmm0 *) ]

                |   (X86_32, [FastArgDouble, FastArgDouble]) =>
                     (* eax and ebx contain the addresses of the values.  They must be unboxed onto the stack. *)
                    [
                        FPLoadFromMemory{address={base=ebx, offset=0, index=NoIndex}, precision=DoublePrecision},
                        ArithToGenReg{ opc=SUB, output=esp, source=NonAddressConstArg 8, opSize=nativeWordOpSize},
                        FPStoreToMemory{ address={base=esp, offset=0, index=NoIndex}, precision=DoublePrecision, andPop=true },
                        FPLoadFromMemory{address={base=eax, offset=0, index=NoIndex}, precision=DoublePrecision},
                        ArithToGenReg{ opc=SUB, output=esp, source=NonAddressConstArg 8, opSize=nativeWordOpSize},
                        FPStoreToMemory{ address={base=esp, offset=0, index=NoIndex}, precision=DoublePrecision, andPop=true }
                    ]
                    (* X64 on both Windows and Unix take the first arg in xmm0 and the second in xmm1. They are already there. *)
                |   (_, [FastArgDouble, FastArgDouble]) => [ ]

                    (* X64 on both Windows and Unix take the first arg in xmm0.  On Unix the integer argument is treated
                       as the first argument and goes into edi.  On Windows it's treated as the second and goes into edx.
                       N.B.  It's also the first argument in ML so is in rax. *)
                |   (X64Unix, [FastArgDouble, FastArgFixed]) => [ moveRR{source=eax, output=edi, opSize=nativeWordOpSize} ]
                |   (X64Win, [FastArgDouble, FastArgFixed]) => [ moveRR{source=eax, output=edx, opSize=nativeWordOpSize} ]
                |   (X86_32, [FastArgDouble, FastArgFixed]) =>
                     (* ebx must be pushed to the stack but eax must be unboxed.. *)
                    [
                        pushR ebx,
                        FPLoadFromMemory{address={base=eax, offset=0, index=NoIndex}, precision=DoublePrecision},
                        ArithToGenReg{ opc=SUB, output=esp, source=NonAddressConstArg 8, opSize=nativeWordOpSize},
                        FPStoreToMemory{ address={base=esp, offset=0, index=NoIndex}, precision=DoublePrecision, andPop=true }
                    ]

                    (* One "float" argument.  The value needs to be untagged on X86/64 but unboxed on X86/32. *)
                |   (X86_32, [FastArgFloat]) =>
                     (* eax contains the address of the value.  This must be unboxed onto the stack. *)
                    [
                        FPLoadFromMemory{address={base=eax, offset=0, index=NoIndex}, precision=SinglePrecision},
                        ArithToGenReg{ opc=SUB, output=esp, source=NonAddressConstArg 4, opSize=nativeWordOpSize},
                        FPStoreToMemory{ address={base=esp, offset=0, index=NoIndex}, precision=SinglePrecision, andPop=true }
                    ]
                |   (_, [FastArgFloat]) => []

                    (* Two float arguments.  Untag them on X86/64 but unbox on X86/32 *)
                |   (X86_32, [FastArgFloat, FastArgFloat]) =>
                     (* eax and ebx contain the addresses of the values.  They must be unboxed onto the stack. *)
                    [
                        FPLoadFromMemory{address={base=ebx, offset=0, index=NoIndex}, precision=SinglePrecision},
                        ArithToGenReg{ opc=SUB, output=esp, source=NonAddressConstArg 4, opSize=nativeWordOpSize},
                        FPStoreToMemory{ address={base=esp, offset=0, index=NoIndex}, precision=SinglePrecision, andPop=true },
                        FPLoadFromMemory{address={base=eax, offset=0, index=NoIndex}, precision=SinglePrecision},
                        ArithToGenReg{ opc=SUB, output=esp, source=NonAddressConstArg 4, opSize=nativeWordOpSize},
                        FPStoreToMemory{ address={base=esp, offset=0, index=NoIndex}, precision=SinglePrecision, andPop=true }
                    ]
                |   (_, [FastArgFloat, FastArgFloat]) => [] (* Already in xmm0 and xmm1 *)

                    (* One float argument and one fixed. *)
                |   (X64Unix, [FastArgFloat, FastArgFixed]) => [moveRR{source=mlArg2Reg, output=edi, opSize=polyWordOpSize} ]
                |   (X64Win, [FastArgFloat, FastArgFixed]) => [moveRR{source=mlArg2Reg, output=edx, opSize=polyWordOpSize}]
                |   (X86_32, [FastArgFloat, FastArgFixed]) =>
                     (* ebx must be pushed to the stack but eax must be unboxed.. *)
                    [
                        pushR ebx,
                        FPLoadFromMemory{address={base=eax, offset=0, index=NoIndex}, precision=SinglePrecision},
                        ArithToGenReg{ opc=SUB, output=esp, source=NonAddressConstArg 4, opSize=nativeWordOpSize},
                        FPStoreToMemory{ address={base=esp, offset=0, index=NoIndex}, precision=SinglePrecision, andPop=true }
                    ]

                |   _ => raise InternalError "rtsCall: Abi/argument count not implemented"
            ) @
            [
                CallAddress(RegisterArg entryPtrReg), (* Call the function *)
                moveRR{source=saveMLStackPtrReg, output=esp, opSize=nativeWordOpSize}, (* Restore the ML stack pointer *)
                (* Since this is an ML function we need to remove any ML stack arguments. *)
                ReturnFromFunction mlArgsOnStack
            ]
 
        val profileObject = createProfileObject functionName
        val newCode = codeCreate (functionName, profileObject, debugSwitches)
        val closure = makeConstantClosure()
        val () = X86OPTIMISE.generateCode{code=newCode, labelCount=0, ops=code, resultClosure=closure}
    in
        closureAsAddress closure
    end
    
    
    fun rtsCallFast (functionName, nArgs, debugSwitches) =
        rtsCallFastGeneral (functionName, List.tabulate(nArgs, fn _ => FastArgFixed), FastArgFixed, debugSwitches)
    
    (* RTS call with one double-precision floating point argument and a floating point result. *)
    fun rtsCallFastRealtoReal (functionName, debugSwitches) =
        rtsCallFastGeneral (functionName, [FastArgDouble], FastArgDouble, debugSwitches)
    
    (* RTS call with two double-precision floating point arguments and a floating point result. *)
    fun rtsCallFastRealRealtoReal (functionName, debugSwitches) =
        rtsCallFastGeneral (functionName, [FastArgDouble, FastArgDouble], FastArgDouble, debugSwitches)

    (* RTS call with one double-precision floating point argument, one fixed point argument and a
       floating point result. *)
    fun rtsCallFastRealGeneraltoReal (functionName, debugSwitches) =
        rtsCallFastGeneral (functionName, [FastArgDouble, FastArgFixed], FastArgDouble, debugSwitches)

    (* RTS call with one general (i.e. ML word) argument and a floating point result.
       This is used only to convert arbitrary precision values to floats. *)
    fun rtsCallFastGeneraltoReal (functionName, debugSwitches) =
        rtsCallFastGeneral (functionName, [FastArgFixed], FastArgDouble, debugSwitches)

    (* Operations on Real32.real values. *)

    fun rtsCallFastFloattoFloat (functionName, debugSwitches) =
        rtsCallFastGeneral (functionName, [FastArgFloat], FastArgFloat, debugSwitches)
    
    fun rtsCallFastFloatFloattoFloat (functionName, debugSwitches) =
        rtsCallFastGeneral (functionName, [FastArgFloat, FastArgFloat], FastArgFloat, debugSwitches)

    (* RTS call with one double-precision floating point argument, one fixed point argument and a
       floating point result. *)
    fun rtsCallFastFloatGeneraltoFloat (functionName, debugSwitches) =
        rtsCallFastGeneral (functionName, [FastArgFloat, FastArgFixed], FastArgFloat, debugSwitches)

    (* RTS call with one general (i.e. ML word) argument and a floating point result.
       This is used only to convert arbitrary precision values to floats. *)
    fun rtsCallFastGeneraltoFloat (functionName, debugSwitches) =
        rtsCallFastGeneral (functionName, [FastArgFixed], FastArgFloat, debugSwitches)


    datatype ffiABI =
        FFI_SYSV        (* Unix 32 bit and Windows GCC 32-bit *)
    |   FFI_STDCALL     (* Windows 32-bit system ABI.  Callee clears the stack. *)
    |   FFI_MS_CDECL    (* VS 32-bit.  Same as SYSV except when returning a struct. *)
    |   FFI_WIN64       (* Windows 64 bit *)
    |   FFI_UNIX64      (* Unix 64 bit. libffi also implements this on X86/32. *)
    (* We don't include various other 32-bit Windows ABIs. *)

    local
        (* Get the current ABI list.  N.B.  Foreign.LibFFI.abiList is the ABIs on the platform we built
           the compiler on, not necessarily the one we're running on. *)
        val ffiGeneral = RunCall.rtsCallFull2 "PolyFFIGeneral"
    in
        fun getFFIAbi abi =
        let
            val abis: (string * Foreign.LibFFI.abi) list = ffiGeneral (50, ())
        in
            case List.find (fn ("default", _) => false | (_, a) => a = abi) abis of
                SOME ("sysv", _)        => FFI_SYSV
            |   SOME ("stdcall", _)     => FFI_STDCALL
            |   SOME ("ms_cdecl", _)    => FFI_MS_CDECL
            |   SOME ("win64", _)       => FFI_WIN64
            |   SOME ("unix64", _)      => FFI_UNIX64
            |   _   => raise Foreign.Foreign "Unknown or unsupported ABI"
        end
    end
    
    fun alignUp(s, align) = Word.andb(s + align-0w1, ~ align)

    (* Build a foreign call function.  The arguments are the abi, the list of argument types and the result type.
       The result is the code of the ML function that takes three arguments: the C function to call, the arguments
       as a vector of C values and the address of the memory for the result. *)
    fun foreignCall(abivalue: Foreign.LibFFI.abi, args: Foreign.LibFFI.ffiType list, result: Foreign.LibFFI.ffiType): Address.machineWord =
    let
        val abi = getFFIAbi abivalue
        
        (* 32-bit arguments.  These all go to the stack so we can simply push them.  The arguments go on the
           stack in reverse order. *)
        fun loadArgs32([], stackOffset, _, code) = (code, stackOffset)

        |   loadArgs32(arg::args, stackOffset, argOffset, code) =
            let
                val {size, align, typeCode, ...} = Foreign.LibFFI.extractFFItype arg
                val newArgOffset = alignUp(argOffset, align)
                val baseAddr = {base=mlArg2Reg, offset=Word.toInt newArgOffset, index=NoIndex}
            in
                if typeCode = Foreign.LibFFI.ffiTypeCodeUInt8
                then (* Unsigned char. *)
                    loadArgs32(args, stackOffset+4, newArgOffset+size,
                        Move{source=MemoryArg baseAddr, destination=RegisterArg edx, moveSize=Move8 }
                            :: PushToStack(RegisterArg edx) :: code)
                else if typeCode = Foreign.LibFFI.ffiTypeCodeSInt8
                then  (* Signed char. *)
                    loadArgs32(args, stackOffset+4, newArgOffset+size,
                        Move{source=MemoryArg baseAddr, destination=RegisterArg edx, moveSize=Move8X }
                            :: PushToStack(RegisterArg edx) :: code)
                else if typeCode = Foreign.LibFFI.ffiTypeCodeUInt16
                then  (* Unsigned 16-bits. *)
                    loadArgs32(args, stackOffset+4, newArgOffset+size,
                        Move{source=MemoryArg baseAddr, destination=RegisterArg edx, moveSize=Move16 }
                            :: PushToStack(RegisterArg edx) :: code)
                else if typeCode = Foreign.LibFFI.ffiTypeCodeSInt16
                then  (* Signed 16-bits. *)
                    loadArgs32(args, stackOffset+4, newArgOffset+size,
                        Move{source=MemoryArg baseAddr, destination=RegisterArg edx, moveSize=Move16X }
                            :: PushToStack(RegisterArg edx) :: code)
                else if typeCode = Foreign.LibFFI.ffiTypeCodeUInt32 orelse typeCode = Foreign.LibFFI.ffiTypeCodeSInt32
                        orelse typeCode = Foreign.LibFFI.ffiTypeCodePointer orelse typeCode = Foreign.LibFFI.ffiTypeCodeFloat
                        orelse typeCode = Foreign.LibFFI.ffiTypeCodeInt
                then  (* 32-bits. *)
                    loadArgs32(args, stackOffset+4, newArgOffset+size, PushToStack(MemoryArg baseAddr) :: code)
                else if typeCode = Foreign.LibFFI.ffiTypeCodeDouble
                then  (* Double: push the two words.  High-order word first, then low-order. *)
                    loadArgs32(args, stackOffset+8, newArgOffset+size,
                        PushToStack(MemoryArg{base=mlArg2Reg, offset=Word.toInt newArgOffset+4, index=NoIndex}) ::
                            PushToStack(MemoryArg{base=mlArg2Reg, offset=Word.toInt newArgOffset, index=NoIndex}) :: code)
                else (* Void or struct *) raise Foreign.Foreign "Unimplemented argument passing"
            end

        val win64ArgRegs = [ (rcx, xmm0), (rdx, xmm1), (r8, xmm2), (r9, xmm3) ]

        fun loadWin64Args([], stackOffset, _, _, code) = (code, stackOffset)

        |   loadWin64Args(arg::args, stackOffset, argOffset, regs, code) =
            let
                val {size, align, typeCode, ...} = Foreign.LibFFI.extractFFItype arg
                val newArgOffset = alignUp(argOffset, align)
                val baseAddr = {base=mlArg2Reg, offset=Word.toInt newArgOffset, index=NoIndex}
                val workReg = #1 (hd win64ArgRegs) (* rcx: the last to be loaded. *)
            in
                if typeCode = Foreign.LibFFI.ffiTypeCodeUInt8
                then (* Unsigned char. *)
                (
                    case regs of
                        (areg, _) :: regs' => 
                        loadWin64Args(args, stackOffset, newArgOffset+size, regs',
                            Move{source=MemoryArg baseAddr, destination=RegisterArg areg, moveSize=Move8 } :: code)
                    |   [] =>
                        loadWin64Args(args, stackOffset+8, newArgOffset+size, [],
                            Move{source=MemoryArg baseAddr,
                                   destination=RegisterArg workReg, moveSize=Move8 } :: PushToStack(RegisterArg workReg) :: code)
                )
                else if typeCode = Foreign.LibFFI.ffiTypeCodeSInt8
                then (* Signed char. *)
                (
                    case regs of
                        (areg, _) :: regs' => 
                        loadWin64Args(args, stackOffset, newArgOffset+size, regs',
                            Move{source=MemoryArg baseAddr, destination=RegisterArg areg, moveSize=Move8X } :: code)
                    |   [] =>
                        loadWin64Args(args, stackOffset+8, newArgOffset+size, [],
                            Move{source=MemoryArg baseAddr,
                                   destination=RegisterArg workReg, moveSize=Move8X } :: PushToStack(RegisterArg workReg) :: code)
                )
                else if typeCode = Foreign.LibFFI.ffiTypeCodeUInt16
                then (* Unsigned 16-bits. *)
                (
                    case regs of
                        (areg, _) :: regs' => 
                        loadWin64Args(args, stackOffset, newArgOffset+size, regs',
                            Move{source=MemoryArg baseAddr, destination=RegisterArg areg, moveSize=Move16 } :: code)
                    |   [] =>
                        loadWin64Args(args, stackOffset+8, newArgOffset+size, [],
                            Move{source=MemoryArg baseAddr,
                                   destination=RegisterArg workReg, moveSize=Move16 } :: PushToStack(RegisterArg workReg) :: code)
                )
                else if typeCode = Foreign.LibFFI.ffiTypeCodeSInt16
                then (* Signed 16-bits. *)
                (
                    case regs of
                        (areg, _) :: regs' => 
                        loadWin64Args(args, stackOffset, newArgOffset+size, regs',
                            Move{source=MemoryArg baseAddr, destination=RegisterArg areg, moveSize=Move16X } :: code)
                    |   [] =>
                        loadWin64Args(args, stackOffset+8, newArgOffset+size, [],
                            Move{source=MemoryArg baseAddr,
                                   destination=RegisterArg workReg, moveSize=Move16X } :: PushToStack(RegisterArg workReg) :: code)
                )
                else if typeCode = Foreign.LibFFI.ffiTypeCodeUInt32
                then (* Unsigned 32-bits. *)
                (
                    case regs of
                        (areg, _) :: regs' => 
                        loadWin64Args(args, stackOffset, newArgOffset+size, regs',
                            Move{source=MemoryArg baseAddr, destination=RegisterArg areg, moveSize=Move32 } :: code)
                    |   [] =>
                        loadWin64Args(args, stackOffset+8, newArgOffset+size, [],
                            Move{source=MemoryArg baseAddr,
                                   destination=RegisterArg workReg, moveSize=Move32 } :: PushToStack(RegisterArg workReg) :: code)
                )
                else if typeCode = Foreign.LibFFI.ffiTypeCodeSInt32 orelse typeCode = Foreign.LibFFI.ffiTypeCodeInt
                then (* Signed 32-bits. *)
                (
                    case regs of
                        (areg, _) :: regs' => 
                        loadWin64Args(args, stackOffset, newArgOffset+size, regs',
                            Move{source=MemoryArg baseAddr, destination=RegisterArg areg, moveSize=Move32X } :: code)
                    |   [] =>
                        loadWin64Args(args, stackOffset+8, newArgOffset+size, [],
                            Move{source=MemoryArg baseAddr,
                                   destination=RegisterArg workReg, moveSize=Move32X } :: PushToStack(RegisterArg workReg) :: code)
                )
                else if typeCode = Foreign.LibFFI.ffiTypeCodeUInt64 orelse typeCode = Foreign.LibFFI.ffiTypeCodeSInt64
                        orelse typeCode = Foreign.LibFFI.ffiTypeCodePointer 
                then (* 64-bits. *)
                (
                    case regs of
                        (areg, _) :: regs' => 
                        loadWin64Args(args, stackOffset, newArgOffset+size, regs',
                            Move{source=MemoryArg baseAddr, destination=RegisterArg areg, moveSize=Move64 } :: code)
                    |   [] =>
                        loadWin64Args(args, stackOffset+8, newArgOffset+size, [],
                            PushToStack(MemoryArg baseAddr) :: code)
                )
                else if typeCode = Foreign.LibFFI.ffiTypeCodeFloat
                then
                (
                    case regs of
                        (_, fpReg) :: regs' => 
                        loadWin64Args(args, stackOffset, newArgOffset+size, regs',
                            XMMArith{opc=SSE2MoveFloat, source=MemoryArg baseAddr, output=fpReg } :: code)
                    |   [] =>
                        loadWin64Args(args, stackOffset+8, newArgOffset+size, [],
                            Move{source=MemoryArg baseAddr,
                                   destination=RegisterArg workReg, moveSize=Move32 } :: PushToStack(RegisterArg workReg) :: code)
                )
                else if typeCode = Foreign.LibFFI.ffiTypeCodeDouble
                then
                (
                    case regs of
                        (_, fpReg) :: regs' => 
                        loadWin64Args(args, stackOffset, newArgOffset+size, regs',
                            XMMArith{opc=SSE2MoveDouble, source=MemoryArg baseAddr, output=fpReg } :: code)
                    |   [] =>
                        loadWin64Args(args, stackOffset+8, newArgOffset+size, [],
                            Move{source=MemoryArg baseAddr,
                                   destination=RegisterArg workReg, moveSize=Move64 } :: PushToStack(RegisterArg workReg) :: code)
                )
                
                else raise Foreign.Foreign "Unimplemented argument passing"
            end

        val sysVGenRegs = [rdi, rsi, rdx, rcx, r8, r9]
        and sysVFPRegs = [xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7]

        fun loadSysV64Args([], stackOffset, _, _, _, code) = (code, stackOffset)

        |   loadSysV64Args(arg::args, stackOffset, argOffset, gRegs, fpRegs, code) =
            let
                val {size, align, typeCode, ...} = Foreign.LibFFI.extractFFItype arg
                val newArgOffset = alignUp(argOffset, align)
                val baseAddr = {base=mlArg2Reg, offset=Word.toInt newArgOffset, index=NoIndex}
                val workReg = hd sysVGenRegs (* The last to be loaded. *)
            in
                if typeCode = Foreign.LibFFI.ffiTypeCodeUInt8
                then (* Unsigned char. *)
                (
                    case gRegs of
                        areg :: regs' => 
                        loadSysV64Args(args, stackOffset, newArgOffset+size, regs', fpRegs,
                            Move{source=MemoryArg baseAddr, destination=RegisterArg areg, moveSize=Move8 } :: code)
                    |   [] =>
                        loadSysV64Args(args, stackOffset+8, newArgOffset+size, [], fpRegs,
                            Move{source=MemoryArg baseAddr,
                                   destination=RegisterArg workReg, moveSize=Move8 } :: PushToStack(RegisterArg workReg) :: code)
                )
                else if typeCode = Foreign.LibFFI.ffiTypeCodeSInt8
                then (* Signed char. *)
                (
                    case gRegs of
                        areg :: regs' => 
                        loadSysV64Args(args, stackOffset, newArgOffset+size, regs', fpRegs,
                            Move{source=MemoryArg baseAddr, destination=RegisterArg areg, moveSize=Move8X } :: code)
                    |   [] =>
                        loadSysV64Args(args, stackOffset+8, newArgOffset+size, [], fpRegs,
                            Move{source=MemoryArg baseAddr,
                                   destination=RegisterArg workReg, moveSize=Move8X } :: PushToStack(RegisterArg workReg) :: code)
                )
                else if typeCode = Foreign.LibFFI.ffiTypeCodeUInt16
                then (* Unsigned 16-bits. *)
                (
                    case gRegs of
                        areg :: regs' => 
                        loadSysV64Args(args, stackOffset, newArgOffset+size, regs', fpRegs,
                            Move{source=MemoryArg baseAddr, destination=RegisterArg areg, moveSize=Move16 } :: code)
                    |   [] =>
                        loadSysV64Args(args, stackOffset+8, newArgOffset+size, [], fpRegs,
                            Move{source=MemoryArg baseAddr,
                                   destination=RegisterArg workReg, moveSize=Move16 } :: PushToStack(RegisterArg workReg) :: code)
                )
                else if typeCode = Foreign.LibFFI.ffiTypeCodeSInt16
                then (* Signed 16-bits. *)
                (
                    case gRegs of
                        areg :: regs' => 
                        loadSysV64Args(args, stackOffset, newArgOffset+size, regs', fpRegs,
                            Move{source=MemoryArg baseAddr, destination=RegisterArg areg, moveSize=Move16X } :: code)
                    |   [] =>
                        loadSysV64Args(args, stackOffset+8, newArgOffset+size, [], fpRegs,
                            Move{source=MemoryArg baseAddr,
                                   destination=RegisterArg workReg, moveSize=Move16X } :: PushToStack(RegisterArg workReg) :: code)
                )
                else if typeCode = Foreign.LibFFI.ffiTypeCodeUInt32
                then (* Unsigned 32-bits. *)
                (
                    case gRegs of
                        areg :: regs' => 
                        loadSysV64Args(args, stackOffset, newArgOffset+size, regs', fpRegs,
                            Move{source=MemoryArg baseAddr, destination=RegisterArg areg, moveSize=Move32 } :: code)
                    |   [] =>
                        loadSysV64Args(args, stackOffset+8, newArgOffset+size, [], fpRegs,
                            Move{source=MemoryArg baseAddr,
                                   destination=RegisterArg workReg, moveSize=Move32 } :: PushToStack(RegisterArg workReg) :: code)
                )
                else if typeCode = Foreign.LibFFI.ffiTypeCodeSInt32 orelse typeCode = Foreign.LibFFI.ffiTypeCodeInt
                then (* Signed 32-bits. *)
                (
                    case gRegs of
                        areg :: regs' => 
                        loadSysV64Args(args, stackOffset, newArgOffset+size, regs', fpRegs,
                            Move{source=MemoryArg baseAddr, destination=RegisterArg areg, moveSize=Move32X } :: code)
                    |   [] =>
                        loadSysV64Args(args, stackOffset+8, newArgOffset+size, [], fpRegs,
                            Move{source=MemoryArg baseAddr,
                                   destination=RegisterArg workReg, moveSize=Move32X } :: PushToStack(RegisterArg workReg) :: code)
                )
                else if typeCode = Foreign.LibFFI.ffiTypeCodeUInt64 orelse typeCode = Foreign.LibFFI.ffiTypeCodeSInt64
                        orelse typeCode = Foreign.LibFFI.ffiTypeCodePointer 
                then (* 64-bits. *)
                (
                    case gRegs of
                        areg :: regs' => 
                        loadSysV64Args(args, stackOffset, newArgOffset+size, regs', fpRegs,
                            Move{source=MemoryArg baseAddr, destination=RegisterArg areg, moveSize=Move64 } :: code)
                    |   [] =>
                        loadSysV64Args(args, stackOffset+8, newArgOffset+size, [], fpRegs,
                            PushToStack(MemoryArg baseAddr) :: code)
                )
                else if typeCode = Foreign.LibFFI.ffiTypeCodeFloat
                then
                (
                    case fpRegs of
                        fpReg :: fpRegs' => 
                        loadSysV64Args(args, stackOffset, newArgOffset+size, gRegs, fpRegs',
                            XMMArith{opc=SSE2MoveFloat, source=MemoryArg baseAddr, output=fpReg } :: code)
                    |   [] =>
                        loadSysV64Args(args, stackOffset+8, newArgOffset+size, gRegs, [],
                            Move{source=MemoryArg baseAddr,
                                   destination=RegisterArg workReg, moveSize=Move32 } :: PushToStack(RegisterArg workReg) :: code)
                )
                else if typeCode = Foreign.LibFFI.ffiTypeCodeDouble
                then
                (
                    case fpRegs of
                        fpReg :: fpRegs' => 
                        loadSysV64Args(args, stackOffset, newArgOffset+size, gRegs, fpRegs',
                            XMMArith{opc=SSE2MoveDouble, source=MemoryArg baseAddr, output=fpReg } :: code)
                    |   [] =>
                        loadSysV64Args(args, stackOffset+8, newArgOffset+size, gRegs, [],
                            Move{source=MemoryArg baseAddr,
                                   destination=RegisterArg workReg, moveSize=Move64 } :: PushToStack(RegisterArg workReg) :: code)
                )
                
                else raise Foreign.Foreign "Unimplemented argument passing"
            end

        local
            val (argCode, argStack) =
                case abi of
                    FFI_WIN64 => loadWin64Args(args, 0, 0w0, win64ArgRegs, [])
                |   FFI_UNIX64 => loadSysV64Args(args, 0, 0w0, sysVGenRegs, sysVFPRegs, [])
                |   _ => (* 32-bit APIs *) loadArgs32(args, 0, 0w0, [])
            val align = argStack mod 16
        in
            val argCode = argCode
            (* Always align the stack.  It's not always necessary on 32-bits but GCC prefers it. *)
            val preArgAlign = if align = 0 then 0 else 16-align
        end
                
        local
            val {typeCode, ...} = Foreign.LibFFI.extractFFItype result
            val resultMemory = {base=ecx, offset=0, index=NoIndex}
        in
            val isResult = typeCode <> Foreign.LibFFI.ffiTypeCodeVoid
            (* Code to set the result.  LibFFI always stores at least a 32-bit value in
               32-bit mode and a 64-bit value in 64-bit mode.  That's not necessary for
               our higher levels and actually complicates them. *)
            val getResult =
                if typeCode = Foreign.LibFFI.ffiTypeCodeVoid
                then []
                else if typeCode = Foreign.LibFFI.ffiTypeCodeSInt8 orelse typeCode = Foreign.LibFFI.ffiTypeCodeSInt8
                then (* Single byte *) [Move{source=RegisterArg eax, destination=MemoryArg resultMemory, moveSize=Move8}]
                else if typeCode = Foreign.LibFFI.ffiTypeCodeUInt16 orelse typeCode = Foreign.LibFFI.ffiTypeCodeSInt16
                then (* 16-bits *) [Move{source=RegisterArg eax, destination=MemoryArg resultMemory, moveSize=Move16}]
                else if typeCode = Foreign.LibFFI.ffiTypeCodeUInt32 orelse typeCode = Foreign.LibFFI.ffiTypeCodeSInt32
                        orelse typeCode = Foreign.LibFFI.ffiTypeCodeInt orelse
                        (typeCode = Foreign.LibFFI.ffiTypeCodePointer andalso abi <> FFI_WIN64 andalso abi <> FFI_UNIX64)
                then [Move{source=RegisterArg eax, destination=MemoryArg resultMemory, moveSize=Move32}]
                else if typeCode = Foreign.LibFFI.ffiTypeCodeUInt64 orelse typeCode = Foreign.LibFFI.ffiTypeCodeSInt64
                        orelse typeCode = Foreign.LibFFI.ffiTypeCodePointer
                then [Move{source=RegisterArg eax, destination=MemoryArg resultMemory, moveSize=Move64}]
                else if typeCode = Foreign.LibFFI.ffiTypeCodeFloat
                then
                (
                    if abi = FFI_WIN64 orelse abi = FFI_UNIX64
                    then (* Result is in xxm0 *)
                        [XMMStoreToMemory{toStore=xmm0, address=resultMemory, precision=SinglePrecision}]
                    else (* Result is in FP0 *) [FPStoreToMemory{address=resultMemory, precision=SinglePrecision, andPop=true }]
                )
                else if typeCode = Foreign.LibFFI.ffiTypeCodeDouble
                then
                (
                    if abi = FFI_WIN64 orelse abi = FFI_UNIX64
                    then (* Result is in xxm0 *)
                        [XMMStoreToMemory{toStore=xmm0, address=resultMemory, precision=DoublePrecision}]
                    else (* Result is in FP0 *) [FPStoreToMemory{address=resultMemory, precision=DoublePrecision, andPop=true }]
                )
                else raise Foreign.Foreign "Unknown or unimplemented result type"
        end

        val code =
            (
                if targetArch <> Native32Bit
                then (* Save heap ptr.  Needed in case we have a callback. *)
                    [storeMemory(r15, ebp, memRegLocalMPointer, nativeWordOpSize)]
                else []
            ) @
            (
                if targetArch <> Native32Bit andalso isResult
                then [PushToStack(RegisterArg r8)] (* Push the third argument - location for result. *)
                else []
            ) @
            [
                (* Save the stack pointer. *)
                storeMemory(esp, ebp, memRegStackPtr, nativeWordOpSize), (* Save ML stack and switch to C stack. *)
                loadMemory(esp, ebp, memRegCStackPtr, nativeWordOpSize)  (* Load the saved C stack pointer. *)
            ] @
            (
                if preArgAlign = 0
                then []
                else [ArithToGenReg{opc=SUB, output=esp, source=NonAddressConstArg(LargeInt.fromInt preArgAlign), opSize=nativeWordOpSize}]
            ) @
            (
                (* The second argument is a SysWord containing the address of a malloced area of memory
                   with the actual arguments in it. *)
                if null args
                then []
                else [loadHeapMemory(mlArg2Reg, mlArg2Reg, 0, nativeWordOpSize)]
            ) @ argCode @
            (
                (* Reserve a 32-byte area if we're using Windows on X64. *)
                if abi = FFI_WIN64
                then [ArithToGenReg{opc=SUB, output=esp, source=NonAddressConstArg(LargeInt.fromInt 32), opSize=nativeWordOpSize}]
                else []
            ) @
            [
                let
                    (* The entry point is in a SysWord.word value in RAX. *)
                    val entryPoint =
                        case targetArch of
                            ObjectId32Bit => MemoryArg{base=ebx, offset=0, index=Index4 eax}
                        |   _ => MemoryArg{base=eax, offset=0, index=NoIndex}
                in
                    (* Call the function.  We're discarding the value in rsp so no need to remove args. *)
                    CallAddress entryPoint
                end,
                loadMemory(esp, ebp, memRegStackPtr, nativeWordOpSize) (* Restore the ML stack pointer. *)
            ] @
            (
                (* Reload the heap pointer.  If we've called back to ML this could well have changed. *)
                if targetArch <> Native32Bit then [loadMemory(r15, ebp, memRegLocalMPointer, nativeWordOpSize) ]
                else []
            ) @
            (
                if isResult
                then
                    (* Store the result in the result area.  The third argument is a LargeWord value that contains
                       the address of a piece of C memory where the result is to be stored. *)
                    (if targetArch = Native32Bit
                    then loadMemory(ecx, esp, 4, nativeWordOpSize)
                    else PopR ecx) ::
                    loadHeapMemory(ecx, ecx, 0, nativeWordOpSize) :: getResult
                else []
            ) @
            [
                ReturnFromFunction (if targetArch = Native32Bit then 1 else 0)
            ]
        val functionName = "foreignCall"
        val debugSwitches =
            [(*Universal.tagInject Pretty.compilerOutputTag (Pretty.prettyPrint(print, 70)),
               Universal.tagInject DEBUG.assemblyCodeTag true*)]
        val profileObject = createProfileObject functionName
        val newCode = codeCreate (functionName, profileObject, debugSwitches)
        val closure = makeConstantClosure()
        val () = X86OPTIMISE.generateCode{code=newCode, labelCount=1(*One label.*), ops=code, resultClosure=closure}
    in
        closureAsAddress closure
    end

    (* Build a callback function.  The arguments are the abi, the list of argument types and the result type.
       The result is an ML function that takes an ML function, f, as its argument, registers it as a callback and
       returns the C function as its result.  When the C function is called the arguments are copied into
       temporary memory and the vector passed to f along with the address of the memory for the result.
       "f" stores the result in it when it returns and the result is then passed back as the result of the
       callback. *)
    fun buildCallBack(abivalue: Foreign.LibFFI.abi, args: Foreign.LibFFI.ffiType list, result: Foreign.LibFFI.ffiType): Address.machineWord =
    let
        val abi = getFFIAbi abivalue
    in
        raise Fail "TODO: foreignCall"
    end

end;
