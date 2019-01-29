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

    (* This is now the standard entry call code. *)
    datatype fastArgs = FastArgFixed | FastArgDouble | FastArgFloat


    fun rtsCallFastGeneral (functionName, argFormats, (*resultFormat*) _, debugSwitches) =
    let
        val entryPointAddr = makeEntryPoint functionName

        (* Get the ABI.  On 64-bit Windows and Unix use different calling conventions. *)
        val abi = getABI()

        val entryPtrReg = if targetArch <> Native32Bit then r11 else ecx
        
        val nArgs = List.length argFormats
        
        local
            (* Compute stack space.  The actual number of args passed is nArgs+1. *)
            val argSpace =
                case abi of
                    X64Unix => Int.max(0, nArgs-6)*8
                |   X64Win => Int.max(0, nArgs-4)*8
                |   X86_32 => List.foldl(fn (FastArgDouble, n) => n+8 | (_, n) => n+4) 0 argFormats
            val align = argSpace mod 16
        in
            (* Add sufficient space so that esp will be 16-byte aligned after we
               have pushed any arguments we need to push. *)
            val stackSpace =
                if align = 0
                then memRegSize
                else memRegSize + 16 - align
        end

        (* The number of ML arguments passed on the stack. *)
        val mlArgsOnStack = Int.max(case abi of X86_32 => nArgs - 2 | _ => nArgs - 5, 0)

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
                if abi = X86_32 andalso nArgs >= 3 orelse abi = X64Win andalso nArgs >= 6
                then [moveRR{source=esp, output=edi, opSize=nativeWordOpSize}] (* Needed if we have to load from the stack. *)
                else []
            ) @
            [
                storeMemory(esp, ebp, memRegStackPtr, nativeWordOpSize), (* Save ML stack and switch to C stack. *)
                loadMemory(esp, ebp, memRegCStackPtr, nativeWordOpSize),
                (* Set the stack pointer past the data on the stack.  For Windows/64 add in a 32 byte save area *)
                ArithToGenReg{opc=SUB, output=esp, source=NonAddressConstArg(LargeInt.fromInt stackSpace), opSize=nativeWordOpSize}
            ] @
            (
                case abi of  (* Set the argument registers. *)
                    X86_32 =>
                    let
                        fun pushReg(reg, FastArgFixed) = [pushR reg]
                        |   pushReg(reg, FastArgDouble) = 
                            (* reg contains the address of the value.  This must be unboxed onto the stack. *)
                                [
                                    FPLoadFromMemory{address={base=reg, offset=0, index=NoIndex}, precision=DoublePrecision},
                                    ArithToGenReg{ opc=SUB, output=esp, source=NonAddressConstArg 8, opSize=nativeWordOpSize},
                                    FPStoreToMemory{ address={base=esp, offset=0, index=NoIndex}, precision=DoublePrecision, andPop=true }
                                ]
                        |   pushReg(reg, FastArgFloat) =
                            (* reg contains the address of the value.  This must be unboxed onto the stack. *)
                            [
                                FPLoadFromMemory{address={base=reg, offset=0, index=NoIndex}, precision=SinglePrecision},
                                ArithToGenReg{ opc=SUB, output=esp, source=NonAddressConstArg 4, opSize=nativeWordOpSize},
                                FPStoreToMemory{ address={base=esp, offset=0, index=NoIndex}, precision=SinglePrecision, andPop=true }
                            ]

                        (* The stack arguments have to be copied first followed by the ebx and finally eax. *)
                        fun pushArgs (_, []) = []
                        |   pushArgs (_, [argType]) = pushReg(eax, argType)
                        |   pushArgs (_, [arg2Type, arg1Type]) = pushReg(ebx, arg2Type) @ pushReg(eax, arg1Type)
                        |   pushArgs (n, FastArgFixed :: argTypes) =
                                PushToStack(MemoryArg{base=edi, offset=(nArgs-n+1)* 4, index=NoIndex}) :: pushArgs(n-1, argTypes)
                        |   pushArgs (n, argType :: argTypes) =
                                (* Use esi as a temporary register. *)
                                loadMemory(esi, edi, (nArgs-n+1)* 4, polyWordOpSize) :: pushReg(esi, argType) @ pushArgs(n-1, argTypes)
                    in
                        pushArgs(nArgs, List.rev argFormats)
                    end
                
                |   X64Unix =>
                    (
                        if List.all (fn FastArgFixed => true | _ => false) argFormats
                        then
                        let
                            fun pushArgs 0 = []
                            |   pushArgs 1 = [moveRR{source=eax, output=edi, opSize=polyWordOpSize}]
                            |   pushArgs 2 = moveRR{source=mlArg2Reg, output=esi, opSize=polyWordOpSize} :: pushArgs 1
                            |   pushArgs 3 = moveRR{source=r8, output=edx, opSize=polyWordOpSize} :: pushArgs 2
                            |   pushArgs 4 = moveRR{source=r9, output=ecx, opSize=polyWordOpSize} :: pushArgs 3
                            |   pushArgs 5 =
                                    (* We have to move r8 into edx before we can move r10 into r8 *)
                                    moveRR{source=r8, output=edx, opSize=polyWordOpSize} ::
                                    moveRR{source=r9, output=ecx, opSize=polyWordOpSize} ::
                                    moveRR{source=r10, output=r8, opSize=polyWordOpSize} :: pushArgs 2
                            |   pushArgs 6 =
                                    (* We have to move r9 into edi before we can load r9 from the stack. *)
                                    moveRR{source=r8, output=edx, opSize=polyWordOpSize} ::
                                    moveRR{source=r9, output=ecx, opSize=polyWordOpSize} ::
                                    loadMemory(r9, edi, 8, polyWordOpSize) ::
                                    moveRR{source=r10, output=r8, opSize=polyWordOpSize} :: pushArgs 2
                            |   pushArgs _ = raise InternalError "rtsCall: Abi/argument count not implemented"
                        in
                            pushArgs nArgs
                        end
                        else case argFormats of
                            [] => []
                        |   [FastArgFixed] => [ moveRR{source=eax, output=edi, opSize=polyWordOpSize} ]
                        |   [FastArgFixed, FastArgFixed] =>
                                (* Since mlArgs2Reg is esi on 32-in-64 this is redundant. *)
                                [ moveRR{source=mlArg2Reg, output=esi, opSize=polyWordOpSize}, moveRR{source=eax, output=edi, opSize=polyWordOpSize} ]
                        |   [FastArgFixed, FastArgFixed, FastArgFixed] => 
                                [ moveRR{source=mlArg2Reg, output=esi, opSize=polyWordOpSize}, moveRR{source=eax, output=edi, opSize=polyWordOpSize},
                                  moveRR{source=r8, output=edx, opSize=polyWordOpSize} ]
                        |   [FastArgFixed, FastArgFixed, FastArgFixed, FastArgFixed] => 
                                [ moveRR{source=mlArg2Reg, output=esi, opSize=polyWordOpSize}, moveRR{source=eax, output=edi, opSize=polyWordOpSize},
                                  moveRR{source=r8, output=edx, opSize=polyWordOpSize}, moveRR{source=r9, output=ecx, opSize=polyWordOpSize} ]
                            (* One "double" argument.  The value needs to be unboxed. *)
                        |   [FastArgDouble] => [] (* Already in xmm0 *)
                            (* X64 on both Windows and Unix take the first arg in xmm0 and the second in xmm1. They are already there. *)
                        |   [FastArgDouble, FastArgDouble] => []
                        |   [FastArgDouble, FastArgFixed] => [ moveRR{source=eax, output=edi, opSize=nativeWordOpSize} ]
                        |   [FastArgFloat] => [] (* Already in xmm0 *)
                        |   [FastArgFloat, FastArgFloat] => [] (* Already in xmm0 and xmm1 *)
                                (* One float argument and one fixed. *)
                        |   [FastArgFloat, FastArgFixed] => [moveRR{source=mlArg2Reg, output=edi, opSize=polyWordOpSize} ]
                        
                        |   _ => raise InternalError "rtsCall: Abi/argument count not implemented"
                            
                    )
                
                |   X64Win =>
                    (
                        if List.all (fn FastArgFixed => true | _ => false) argFormats
                        then
                        let
                            fun pushArgs 0 = []
                            |   pushArgs 1 = [moveRR{source=eax, output=ecx, opSize=polyWordOpSize}]
                            |   pushArgs 2 = moveRR{source=mlArg2Reg, output=edx, opSize=polyWordOpSize} :: pushArgs 1
                            |   pushArgs 3 = (* Already in r8 *) pushArgs 2
                            |   pushArgs 4 = (* Already in r9, and r8 *) pushArgs 2
                            |   pushArgs 5 = pushR r10 :: pushArgs 2
                            |   pushArgs 6 = PushToStack(MemoryArg{base=edi, offset=8, index=NoIndex}) :: pushArgs 5
                            |   pushArgs _ = raise InternalError "rtsCall: Abi/argument count not implemented"
                        in
                            pushArgs nArgs
                        end

                        else case argFormats of
                            [FastArgFixed] => [ moveRR{source=eax, output=ecx, opSize=polyWordOpSize} ]
                        |   [FastArgFixed, FastArgFixed] => [ moveRR{source=eax, output=ecx, opSize=polyWordOpSize}, moveRR{source=mlArg2Reg, output=edx, opSize=polyWordOpSize} ]
                        |   [FastArgFixed, FastArgFixed, FastArgFixed] =>
                                [ moveRR{source=eax, output=ecx, opSize=polyWordOpSize}, moveRR{source=mlArg2Reg, output=edx, opSize=polyWordOpSize} (* Arg3 is already in r8. *) ]
                        |   [FastArgFixed, FastArgFixed, FastArgFixed, FastArgFixed] =>
                                [ moveRR{source=eax, output=ecx, opSize=polyWordOpSize}, moveRR{source=mlArg2Reg, output=edx, opSize=polyWordOpSize} (* Arg3 is already in r8 and arg4 in r9. *) ]
                        |   [FastArgDouble] => [ (* Already in xmm0 *) ]
                            (* X64 on both Windows and Unix take the first arg in xmm0 and the second in xmm1. They are already there. *)
                        |   [FastArgDouble, FastArgDouble] => [ ]
                            (* X64 on both Windows and Unix take the first arg in xmm0.  On Unix the integer argument is treated
                               as the first argument and goes into edi.  On Windows it's treated as the second and goes into edx.
                               N.B.  It's also the first argument in ML so is in rax. *)
                        |   [FastArgDouble, FastArgFixed] => [ moveRR{source=eax, output=edx, opSize=nativeWordOpSize} ]
                        |   [FastArgFloat] => []
                        |   [FastArgFloat, FastArgFloat] => [] (* Already in xmm0 and xmm1 *)
                        |   [FastArgFloat, FastArgFixed] => [moveRR{source=mlArg2Reg, output=edx, opSize=polyWordOpSize}]

                        |   _ => raise InternalError "rtsCall: Abi/argument count not implemented"
                    )
            ) @
            (*  For Windows/64 add in a 32 byte save area ater we've pushed any arguments. *)
            (case abi of X64Win => [ArithToGenReg{opc=SUB, output=esp, source=NonAddressConstArg 32, opSize=nativeWordOpSize}] | _ => []) @
            [
                CallFunction(DirectReg entryPtrReg), (* Call the function *)
                loadMemory(esp, ebp, memRegStackPtr, nativeWordOpSize) (* Restore the ML stack pointer. *)
            ] @
            (
            if targetArch <> Native32Bit then [loadMemory(r15, ebp, memRegLocalMPointer, nativeWordOpSize) ] (* Copy back the heap ptr *)
            else []
            ) @
            [
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

end;
