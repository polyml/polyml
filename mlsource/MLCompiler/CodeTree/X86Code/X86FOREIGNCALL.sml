(*
    Copyright (c) 2016 David C.J. Matthews

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

        val optimise: code * operations -> operations

        structure Sharing:
        sig
            type operation = operation
            type code = code
        end
    end

    structure DEBUG: DEBUGSIG

    sharing X86CODE.Sharing = X86OPTIMISE.Sharing
): FOREIGNCALLSIG
=
struct
    open X86CODE
    open Address
    
    exception InternalError = Misc.InternalError

    fun condBranch(test, predict) =
    let
        val label as Labels{uses, ...} = mkLabel()
    in
        uses := 1;
        (ConditionalBranch{test=test, predict=predict, label=label}, label)
    end

    fun loadMemory(reg, base, offset) =
        LoadMemR{source=BaseOffset{base=base, offset=offset, index=NoIndex}, output=reg}
    and storeMemory(reg, base, offset) =
        StoreRegToMemory{toStore=reg, address=BaseOffset{base=base, offset=offset, index=NoIndex}}

    fun createProfileObject _ (*functionName*) =
    let
        (* The profile object is a single mutable with the F_bytes bit set. *)
        open Address
        val profileObject = alloc(0w1, Word8.orb(F_mutable, F_bytes), toMachineWord 0w0)
    in
        toMachineWord profileObject
    end
    
    local
        val ioOp : int -> machineWord = RunCall.run_call1 RuntimeCalls.POLY_SYS_io_operation
    in
        (* Find the address of the function to get the entry point. *)
        val makeEntryPointAddr = ioOp RuntimeCalls.POLY_SYS_make_entry_point
        val makeEntryPoint: string * int -> machineWord =
            RunCall.run_call2 RuntimeCalls.POLY_SYS_make_entry_point
    end

    datatype abi = X86_32 | X64Win | X64Unix

    val noException = 1

    fun rtsCall (functionName, nArgs, debugSwitches) =
    let
        val entryPointAddr = makeEntryPoint(functionName, 0)

        (* Get the ABI.  On 64-bit Windows and Unix use different calling conventions. *)
        val abi =
            case RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (108, 0) of
                1 => X64Unix
            |   2 => X64Win
            |   _ => X86_32

        (* Branch to check for address *)
        val (checkAddr, addrLabel) = condBranch(JNE, PredictTaken)

        (* Branch to check for exception. *)
        val (checkExc, exLabel) = condBranch(JNE, PredictNotTaken)
        
        (* Unix X64.  The first six arguments are in rdi, rsi, rdx, rcx, r8, r9.
                      The rest are on the stack.
           Windows X64. The first four arguments are in rcx, rdx, r8 and r9.  The rest are
                       on the stack.  The caller must ensure the stack is aligned on 16-byte boundary
                       and must allocate 32-byte save area for the register args.
                       rbx, rbp, rdi, rsi, rsp, r12-r15 are saved by the called function.
           X86/32.  Arguments are pushed to the stack.
                   ebx, edi, esi, ebp and esp are saved by the called function.
                   We use esi to hold the argument data pointer and edi to save the ML stack pointer
           Our ML conventions use eax, ebx for the first two arguments in X86/32 and
                   rax, ebx, r8, r9, r10 for the first five arguments in X86/64.
        *)
        
        (* This is a pointer to data that the RTS provides on entry.  Fields
           may be updated in the RTS call.  Where it is stored differs between the ABIs. *)
        val memRegArgumentPtr = case abi of X64Unix => ~32 | X64Win => 16 | X86_32 => 8

        (* The argument pointer and the ML stack pointer need to be
           loaded into registers that will be saved by the callee.
           The entry point doesn't need to be but must be a register
           that isn't used for an argument. *)
        val (entryPtrReg, argPtrReg, saveMLStackPtrReg) =
            if isX64 then (r11, r12, r13) else (ecx, esi, edi)
        
        (* If we have to get the entry point address we need to save the registers
           that contained the original arguments.  How many depends on whether this is
           32-bit or 64-bit. *)
        val regsToSave =
            case (abi, nArgs) of
                (_, 0) => []
            |   (_, 1) => [eax]
            |   (_, 2) => [eax, ebx]
            |   (X86_32, _) => [eax, ebx]
            |   (_, 3) => [eax, ebx, r8]
            |   (_, 4) => [eax, ebx, r8, r9]
            |   _ => [eax, ebx, r8, r9, r10]
            
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
                MoveLongConstR{source=entryPointAddr, output=entryPtrReg}, (* Load the entry point ref. *)
                loadMemory(entryPtrReg, entryPtrReg, 0),(* Load its value. *)
                ArithRConst{opc=CMP, output=entryPtrReg, source=0},(* Has it been set? *)
                checkAddr
            ] @ map PushR regsToSave @
            [
                MoveLongConstR{source=toMachineWord functionName, output=eax},
                MoveLongConstR{source=entryPointAddr, output=ebx},
                CallFunction(ConstantClosure makeEntryPointAddr),
                loadMemory(entryPtrReg, eax, 0) (* Extract the value from the result. *)
            ] @ map PopR (List.rev regsToSave) @
            [
                JumpLabel addrLabel, (* Label to skip to if addr has been set. *)
                loadMemory(argPtrReg, ebp, memRegArgumentPtr), (* Get argument data ptr  - use r12 to save reloading after the call *)
                StoreConstToMemory{toStore=noException, address=BaseOffset{base=argPtrReg, offset=argExceptionPacket, index=NoIndex}} (* Clear exception *)
            ] @
            (
                (* Save heap ptr.  This is in r15 in X86/64 *)
                if isX64 then [storeMemory(r15, argPtrReg, argLocalMpointer)] (* Save heap ptr *)
                else [loadMemory(edx, ebp, memRegLocalMPointer), storeMemory(edx, argPtrReg, argLocalMpointer) ]
            ) @
            [
                MoveRR{source=esp, output=saveMLStackPtrReg}, (* Save ML stack and switch to C stack. *)
                MoveRR{source=ebp, output=esp},
                (* Set the stack pointer past the data on the stack.  For Windows/64 add in a 32 byte save area *)
                ArithRConst{ opc=SUB, output=esp, source= LargeInt.fromInt stackSpace}
            ] @
            (
                case (abi, nArgs) of  (* Set the argument registers. *)
                    (_, 0) => []
                |   (X64Unix, 1) => [ MoveRR{source=eax, output=edi} ]
                |   (X64Unix, 2) =>
                        [ MoveRR{source=eax, output=edi}, MoveRR{source=ebx, output=esi} ]
                |   (X64Unix, 3) => 
                        [ MoveRR{source=eax, output=edi}, MoveRR{source=ebx, output=esi}, MoveRR{source=r8, output=edx} ]
                |   (X64Unix, 4) => 
                        [ MoveRR{source=eax, output=edi}, MoveRR{source=ebx, output=esi}, MoveRR{source=r8, output=edx}, MoveRR{source=r9, output=ecx} ]
                |   (X64Win, 1) => [ MoveRR{source=eax, output=ecx} ]
                |   (X64Win, 2) => [ MoveRR{source=eax, output=ecx}, MoveRR{source=ebx, output=edx} ]
                |   (X64Win, 3) =>
                        [ MoveRR{source=eax, output=ecx}, MoveRR{source=ebx, output=edx} (* Arg3 is already in r8. *) ]
                |   (X64Win, 4) =>
                        [ MoveRR{source=eax, output=ecx}, MoveRR{source=ebx, output=edx} (* Arg3 is already in r8 and arg4 in r9. *) ]
                        (* TODO: We should at a minimum maintain an 8-byte alignment.  GCC like a 16-byte alignment. *)
                |   (X86_32, 1) => [ PushR eax ]
                |   (X86_32, 2) => [ PushR ebx, PushR eax ]
                |   (X86_32, 3) =>
                        [
                            (* We need to move an argument from the ML stack. *)
                            loadMemory(edx, saveMLStackPtrReg, 4), PushR edx, PushR ebx, PushR eax
                        ]
                |   (X86_32, 4) =>
                        [
                            (* We need to move an arguments from the ML stack. *)
                            loadMemory(edx, saveMLStackPtrReg, 4), PushR edx,
                            loadMemory(edx, saveMLStackPtrReg, 8), PushR edx,
                            PushR ebx, PushR eax
                        ]
                |   _ => raise InternalError "rtsCall: Abi/argument count not implemented"
            ) @
            [
                CallFunction(DirectReg entryPtrReg), (* Call the function *)
                MoveRR{source=saveMLStackPtrReg, output=esp} (* Restore the ML stack pointer *)
            ] @
            (
            if isX64 then [loadMemory(r15, argPtrReg, argLocalMpointer) ] (* Copy back the heap ptr *)
            else [loadMemory(edx, esi, argLocalMpointer), storeMemory(edx, ebp, memRegLocalMPointer) ]
            ) @
            [
                loadMemory(edx, argPtrReg, argLocalMbottom), storeMemory(edx, ebp, memRegLocalMbottom), (* and base ptr *)
                ArithMemConst{opc=CMP, offset=argExceptionPacket, base=argPtrReg, source=noException},
                checkExc,
                (* Remove any arguments that have been passed on the stack. *)
                ReturnFromFunction(Int.max(case abi of X86_32 => nArgs-2 | _ => nArgs-5, 0)),
                JumpLabel exLabel, (* else raise the exception *)
                loadMemory(eax, argPtrReg, argExceptionPacket),
                RaiseException
            ]
 
        val profileObject = createProfileObject functionName
        val newCode = codeCreate (functionName, profileObject, debugSwitches)
        val createdCode = createCodeSegment(X86OPTIMISE.optimise(newCode, code), newCode)
        (* Have to create a closure for this *)
        open Address
        val closure = alloc(0w1, Word8.orb (F_mutable, F_words), toMachineWord 0w0)
    in
        assignWord(closure, 0w0, toMachineWord createdCode);
        lock closure;
        closure
    end
end;
