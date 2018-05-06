(*
    Copyright (c) 2016-18 David C.J. Matthews

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
    
    val pushR = PushToStack o RegisterArg

    fun moveRR{source, output, opSize} = MoveToRegister{source=RegisterArg source, output=output, opSize=opSize}

    fun loadMemory(reg, base, offset, opSize) =
        MoveToRegister{source=MemoryArg{base=base, offset=offset, index=NoIndex}, output=reg, opSize=opSize}
    and storeMemory(reg, base, offset, opSize) =
        StoreRegToMemory{toStore=reg, address={base=base, offset=offset, index=NoIndex}, opSize=opSize}
    
    val loadHeapMemory =
        case targetArch of
            ObjectId32Bit =>
                (
                    fn (reg, base, offset, opSize) => 
                        MoveToRegister{source=MemoryArg{base=ebx, offset=offset, index=Index4 base}, output=reg, opSize=opSize}
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
                MoveToRegister{source=AddressConstArg entryPointAddr, output=entryPtrReg, opSize=polyWordOpSize}, (* Load the entry point ref. *)
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
                CallFunction(DirectReg entryPtrReg), (* Call the function *)
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
    fun rtsCallFast (functionName, nArgs, debugSwitches) =
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
                    val argSpace = nArgs*4
                    val align = argSpace mod 16
                in
                    (* Add sufficient space so that esp will be 16-byte aligned *)
                    if align = 0
                    then memRegSize
                    else memRegSize + 16 - align
                end

        val code =
            [
                MoveToRegister{source=AddressConstArg entryPointAddr, output=entryPtrReg, opSize=polyWordOpSize}, (* Load the entry point ref. *)
                loadHeapMemory(entryPtrReg, entryPtrReg, 0, nativeWordOpSize),(* Load its value. *)
                moveRR{source=esp, output=saveMLStackPtrReg, opSize=nativeWordOpSize}, (* Save ML stack and switch to C stack. *)
                loadMemory(esp, ebp, memRegCStackPtr, nativeWordOpSize),
                (* Set the stack pointer past the data on the stack.  For Windows/64 add in a 32 byte save area *)
                ArithToGenReg{opc=SUB, output=esp, source=NonAddressConstArg(LargeInt.fromInt stackSpace), opSize=nativeWordOpSize}
            ] @
            (
                case (abi, nArgs) of  (* Set the argument registers. *)
                    (_, 0) => []
                |   (X64Unix, 1) => [ moveRR{source=eax, output=edi, opSize=polyWordOpSize} ]
                |   (X64Unix, 2) =>
                        (* Since mlArgs2Reg is esi on 32-in-64 this is redundant. *)
                        [ moveRR{source=mlArg2Reg, output=esi, opSize=polyWordOpSize}, moveRR{source=eax, output=edi, opSize=polyWordOpSize} ]
                |   (X64Unix, 3) => 
                        [ moveRR{source=mlArg2Reg, output=esi, opSize=polyWordOpSize}, moveRR{source=eax, output=edi, opSize=polyWordOpSize},
                          moveRR{source=r8, output=edx, opSize=polyWordOpSize} ]
                |   (X64Unix, 4) => 
                        [ moveRR{source=mlArg2Reg, output=esi, opSize=polyWordOpSize}, moveRR{source=eax, output=edi, opSize=polyWordOpSize},
                          moveRR{source=r8, output=edx, opSize=polyWordOpSize}, moveRR{source=r9, output=ecx, opSize=polyWordOpSize} ]
                |   (X64Win, 1) => [ moveRR{source=eax, output=ecx, opSize=polyWordOpSize} ]
                |   (X64Win, 2) => [ moveRR{source=eax, output=ecx, opSize=polyWordOpSize}, moveRR{source=mlArg2Reg, output=edx, opSize=polyWordOpSize} ]
                |   (X64Win, 3) =>
                        [ moveRR{source=eax, output=ecx, opSize=polyWordOpSize}, moveRR{source=mlArg2Reg, output=edx, opSize=polyWordOpSize} (* Arg3 is already in r8. *) ]
                |   (X64Win, 4) =>
                        [ moveRR{source=eax, output=ecx, opSize=polyWordOpSize}, moveRR{source=mlArg2Reg, output=edx, opSize=polyWordOpSize} (* Arg3 is already in r8 and arg4 in r9. *) ]
                |   (X86_32, 1) => [ pushR eax ]
                |   (X86_32, 2) => [ pushR mlArg2Reg, pushR eax ]
                |   (X86_32, 3) =>
                        [
                            (* We need to move an argument from the ML stack. *)
                            loadMemory(edx, saveMLStackPtrReg, 4, polyWordOpSize), pushR edx, pushR mlArg2Reg, pushR eax
                        ]
                |   (X86_32, 4) =>
                        [
                            (* We need to move an arguments from the ML stack. *)
                            loadMemory(edx, saveMLStackPtrReg, 4, polyWordOpSize), pushR edx,
                            loadMemory(edx, saveMLStackPtrReg, 8, polyWordOpSize), pushR edx,
                            pushR mlArg2Reg, pushR eax
                        ]
                |   _ => raise InternalError "rtsCall: Abi/argument count not implemented"
            ) @
            [
                CallFunction(DirectReg entryPtrReg), (* Call the function *)
                moveRR{source=saveMLStackPtrReg, output=esp, opSize=nativeWordOpSize}, (* Restore the ML stack pointer *)
                (* Remove any arguments that have been passed on the stack. *)
                ReturnFromFunction(Int.max(case abi of X86_32 => nArgs-2 | _ => nArgs-5, 0))
            ]
 
        val profileObject = createProfileObject functionName
        val newCode = codeCreate (functionName, profileObject, debugSwitches)
        val closure = makeConstantClosure()
        val () = X86OPTIMISE.generateCode{code=newCode, labelCount=0, ops=code, resultClosure=closure}
    in
        closureAsAddress closure
    end
    
    (* RTS call with one double-precision floating point argument and a floating point result.
       First version.  This will probably be merged into the above code in due
       course.
       Currently ML always uses boxed values for floats.  *)
    fun rtsCallFastFloattoFloat (functionName, debugSwitches) =
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
                    val argSpace = 8 (*nArgs*4*) (* One "double" value. *)
                    val align = argSpace mod 16
                in
                    (* Add sufficient space so that esp will be 16-byte aligned *)
                    if align = 0
                    then memRegSize
                    else memRegSize + 16 - align
                end

        (* Constants for a box for a float *)
        val fpBoxSize = 8 div Word.toInt wordSize
        val fpBoxLengthWord32 = IntInf.orb(IntInf.fromInt fpBoxSize, IntInf.<<(Word8.toLargeInt F_bytes, 0w24))

        val code =
            [
                MoveToRegister{source=AddressConstArg entryPointAddr, output=entryPtrReg, opSize=polyWordOpSize}, (* Load the entry point ref. *)
                loadHeapMemory(entryPtrReg, entryPtrReg, 0, nativeWordOpSize),(* Load its value. *)
                moveRR{source=esp, output=saveMLStackPtrReg, opSize=nativeWordOpSize}, (* Save ML stack and switch to C stack. *)
                loadMemory(esp, ebp, memRegCStackPtr, nativeWordOpSize),
                (* Set the stack pointer past the data on the stack.  For Windows/64 add in a 32 byte save area *)
                ArithToGenReg{opc=SUB, output=esp, source=NonAddressConstArg(LargeInt.fromInt stackSpace), opSize=nativeWordOpSize}
            ] @
            (
                case targetArch of
                    (* X64 on both Windows and Unix take the first arg in xmm0.  We need to
                       unbox the value pointed at by rax. *)
                    Native64Bit => [ XMMArith { opc= SSE2Move, source=MemoryArg{base=eax, offset=0, index=NoIndex}, output=xmm0 } ]
                |   ObjectId32Bit => [ XMMArith { opc= SSE2Move, source=MemoryArg{base=ebx, offset=0, index=Index4 eax}, output=xmm0 } ]
                |   Native32Bit =>
                     (* eax contains the address of the value.  This must be unboxed onto the stack. *)
                    [
                        FPLoadFromMemory{address={base=eax, offset=0, index=NoIndex}, precision=DoublePrecision},
                        ArithToGenReg{ opc=SUB, output=esp, source=NonAddressConstArg 8, opSize=nativeWordOpSize},
                        FPStoreToMemory{ address={base=esp, offset=0, index=NoIndex}, precision=DoublePrecision, andPop=true }
                    ]
            ) @
            [
                CallFunction(DirectReg entryPtrReg), (* Call the function *)
                moveRR{source=saveMLStackPtrReg, output=esp, opSize=nativeWordOpSize} (* Restore the ML stack pointer *)
            ] @
            (
                (* Put the floating point result into a box. *)
                case targetArch of
                   Native32Bit =>
                    [
                        AllocStore{size=fpBoxSize, output=eax, saveRegs=[]},
                        StoreConstToMemory{toStore=fpBoxLengthWord32,
                                address={offset= ~ (Word.toInt wordSize), base=eax, index=NoIndex}, opSize=OpSize32},
                        FPStoreToMemory{ address={base=eax, offset=0, index=NoIndex}, precision=DoublePrecision, andPop=true },
                        StoreInitialised
                    ]
                |   Native64Bit => (* X64 The result is in xmm0 *)
                    [
                        AllocStore{size=fpBoxSize, output=eax, saveRegs=[]},
                        StoreConstToMemory{toStore=LargeInt.fromInt fpBoxSize,
                            address={offset= ~ (Word.toInt wordSize), base=eax, index=NoIndex}, opSize=OpSize64},
                        StoreNonWordConst{size=Size8Bit, toStore=Word8.toLargeInt F_bytes, address={offset= ~1, base=eax, index=NoIndex}},
                        XMMStoreToMemory { address={base=eax, offset=0, index=NoIndex}, precision=DoublePrecision, toStore=xmm0 },
                        StoreInitialised
                    ]                    
                |   ObjectId32Bit => (* X64 with 32-bit object Ids The result is in xmm0 *)
                    [
                        AllocStore{size=fpBoxSize, output=eax, saveRegs=[]},
                        StoreConstToMemory{toStore=fpBoxLengthWord32,
                            address={offset= ~ (Word.toInt wordSize), base=eax, index=NoIndex}, opSize=OpSize32},
                        XMMStoreToMemory { address={base=eax, offset=0, index=NoIndex}, precision=DoublePrecision, toStore=xmm0 },
                        StoreInitialised,
                        ArithToGenReg{ opc=SUB, output=eax, source=RegisterArg ebx, opSize=OpSize64 },
                        ShiftConstant{ shiftType=SHR, output=eax, shift=0w2, opSize=OpSize64 }
                    ]                    
            ) @
            [
                (* Remove any arguments that have been passed on the stack. *)
                ReturnFromFunction 0
            ]
 
        val profileObject = createProfileObject functionName
        val newCode = codeCreate (functionName, profileObject, debugSwitches)
        val closure = makeConstantClosure()
        val () = X86OPTIMISE.generateCode{code=newCode, labelCount=0, ops=code, resultClosure=closure}
    in
        closureAsAddress closure
    end

    (* RTS call with one general (i.e. ML word) argument and a floating point result.
       This is used only to convert arbitrary precision values to floats.
       In due course this will be merged into the other functions. *)
    fun rtsCallFastGeneraltoFloat (functionName, debugSwitches) =
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
                    val argSpace = 4 (* One "PolyWord" value. *)
                    val align = argSpace mod 16
                in
                    (* Add sufficient space so that esp will be 16-byte aligned *)
                    if align = 0
                    then memRegSize
                    else memRegSize + 16 - align
                end

        (* Constants for a box for a float *)
        val fpBoxSize = 8 div Word.toInt wordSize
        val fpBoxLengthWord32 = IntInf.orb(IntInf.fromInt fpBoxSize, IntInf.<<(Word8.toLargeInt F_bytes, 0w24))

        val code =
            [
                MoveToRegister{source=AddressConstArg entryPointAddr, output=entryPtrReg, opSize=polyWordOpSize}, (* Load the entry point ref. *)
                loadHeapMemory(entryPtrReg, entryPtrReg, 0, nativeWordOpSize),(* Load its value. *)
                moveRR{source=esp, output=saveMLStackPtrReg, opSize=nativeWordOpSize}, (* Save ML stack and switch to C stack. *)
                loadMemory(esp, ebp, memRegCStackPtr, nativeWordOpSize),
                (* Set the stack pointer past the data on the stack.  For Windows/64 add in a 32 byte save area *)
                ArithToGenReg{opc=SUB, output=esp, source=NonAddressConstArg(LargeInt.fromInt stackSpace), opSize=nativeWordOpSize}
            ] @
            (
                case abi of
                    X64Unix => [ moveRR{source=eax, output=edi, opSize=polyWordOpSize} ]
                |   X64Win => [ moveRR{source=eax, output=ecx, opSize=polyWordOpSize} ]
                |   X86_32 => [ pushR eax ]
            ) @
            [
                CallFunction(DirectReg entryPtrReg), (* Call the function *)
                moveRR{source=saveMLStackPtrReg, output=esp, opSize=nativeWordOpSize} (* Restore the ML stack pointer *)
            ] @
            (
                (* Put the floating point result into a box. *)
                case targetArch of
                   Native32Bit =>
                    [
                        AllocStore{size=fpBoxSize, output=eax, saveRegs=[]},
                        StoreConstToMemory{toStore=fpBoxLengthWord32,
                                address={offset= ~ (Word.toInt wordSize), base=eax, index=NoIndex}, opSize=OpSize32},
                        FPStoreToMemory{ address={base=eax, offset=0, index=NoIndex}, precision=DoublePrecision, andPop=true },
                        StoreInitialised
                    ]
                |   Native64Bit => (* X64 The result is in xmm0 *)
                    [
                        AllocStore{size=fpBoxSize, output=eax, saveRegs=[]},
                        StoreConstToMemory{toStore=LargeInt.fromInt fpBoxSize,
                            address={offset= ~ (Word.toInt wordSize), base=eax, index=NoIndex}, opSize=OpSize64},
                        StoreNonWordConst{size=Size8Bit, toStore=Word8.toLargeInt F_bytes, address={offset= ~1, base=eax, index=NoIndex}},
                        XMMStoreToMemory { address={base=eax, offset=0, index=NoIndex}, precision=DoublePrecision, toStore=xmm0 },
                        StoreInitialised
                    ]                    
                |   ObjectId32Bit => (* X64 with 32-bit object Ids The result is in xmm0 *)
                    [
                        AllocStore{size=fpBoxSize, output=eax, saveRegs=[]},
                        StoreConstToMemory{toStore=fpBoxLengthWord32,
                            address={offset= ~ (Word.toInt wordSize), base=eax, index=NoIndex}, opSize=OpSize32},
                        XMMStoreToMemory { address={base=eax, offset=0, index=NoIndex}, precision=DoublePrecision, toStore=xmm0 },
                        StoreInitialised,
                        ArithToGenReg{ opc=SUB, output=eax, source=RegisterArg ebx, opSize=OpSize64 },
                        ShiftConstant{ shiftType=SHR, output=eax, shift=0w2, opSize=OpSize64 }
                    ]                    
            ) @
            [
                (* Remove any arguments that have been passed on the stack. *)
                ReturnFromFunction 0
            ]
 
        val profileObject = createProfileObject functionName
        val newCode = codeCreate (functionName, profileObject, debugSwitches)
        val closure = makeConstantClosure()
        val () = X86OPTIMISE.generateCode{code=newCode, labelCount=0, ops=code, resultClosure=closure}
    in
        closureAsAddress closure
    end

end;
