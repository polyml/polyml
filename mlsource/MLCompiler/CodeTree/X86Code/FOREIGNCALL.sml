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

functor FOREIGNCALL(

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
): FOREIGNCALLSIG =
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
    
    fun createWeakEntryPoint () =
    let
        (* We hold the entry point in a weak mutable byte cell.  It is cleared to zero when the
           code is loaded.  The first time it is referenced it is set by a call to
           get_entry_point.  The result is then cached for the remainder of the session. *)
        open Address
        val entryPt = alloc(0w1, List.foldl Word8.orb 0w0 [F_mutable, F_bytes, F_weak], toMachineWord 0w0)
    in
        toMachineWord entryPt
    end

    fun rtsCall2(functionName, debugSwitches) =
    let
        val entryPointAddr = createWeakEntryPoint()
        val ioOp : int -> machineWord = RunCall.run_call1 RuntimeCalls.POLY_SYS_io_operation
        (* Find the address of the function to get the entry point. *)
        val getEntryPoint = ioOp RuntimeCalls.POLY_SYS_get_entry_point
        
        (* Get the ABI.  This is only relevant for X64. *)
        val abi: int = RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (108, 0)

        val noException = 1
        
        (* Branch to check for address *)
        val (checkAddr, addrLabel) = condBranch(JNE, PredictTaken)

        (* Branch to check for exception. *)
        val (checkExc, exLabel) = condBranch(JNE, PredictNotTaken)

        val code =
            case abi of
                1 => (* Unix X64.  The first six arguments are in rdi, rsi, rdx, rcx, r8, r9.
                        The rest are on the stack.
                        rbx, rbp, r12-r15 are saved by the called function. *)
                [
                    MoveLongConstR{source=entryPointAddr, output=r11}, (* Load the entry point ref. *)
                    loadMemory(r11, r11, 0),(* Load its value. *)
                    ArithRConst{opc=CMP, output=r11, source=0},(* Has it been set? *)
                    checkAddr,
                    PushR eax, PushR ebx, (* Save original arguments.  Also r8/r9/r10 if necessary. *)
                    MoveLongConstR{source=toMachineWord functionName, output=eax},
                    CallFunction(ConstantClosure getEntryPoint),
                    MoveLongConstR{source=entryPointAddr, output=r11}, (* Reload the ref. *)
                    loadMemory(eax, eax, 0), (* Extract the value from the large word. *)
                    StoreRegToMemory { toStore=eax, address=BaseOffset{base=r11, offset=0, index=NoIndex} }, (* Save into ref. *)
                    MoveRR{source=eax, output=r11}, (* Move to the call address register. *)
                    PopR ebx, PopR eax,
                    JumpLabel addrLabel, (* Label to skip to if addr has been set. *)
                    loadMemory(r12, ebp, memRegArgumentPtr), (* Get argument data ptr  - use r12 to save reloading after the call *)
                    StoreConstToMemory{toStore=noException, address=BaseOffset{base=r12, offset=argExceptionPacket, index=NoIndex}}, (* Clear exception *)
                    storeMemory(r15, r12, argLocalMpointer), (* Save r15, the heap pointer *)
                    MoveRR{source=esp, output=r13},  (* Save ML stack pointer *)
                    MoveRR{source=ebp, output=esp}, ArithRConst{ opc=SUB, output=esp, source= LargeInt.fromInt memRegSize}, (* Switch to C stack *)
                    MoveRR{source=eax, output=edi}, MoveRR{source=ebx, output=esi}, (* Set the argument registers. *)
                    CallFunction(DirectReg r11), (* Call the C function using the address in r11 *)
                    MoveRR{source=r13, output=esp}, (* Restore the ML stack pointer *)
                    loadMemory(r15, r12, argLocalMpointer), (* Copy heap ptr back. *)
                    loadMemory(edx, r12, argLocalMbottom), storeMemory(edx, ebp, memRegLocalMbottom), (* and base ptr. *)
                    ArithMemConst{opc=CMP, offset=argExceptionPacket, base=r12, source=noException},
                    checkExc, ReturnFromFunction 0, (* Check for exception and return if not. *)
                    JumpLabel exLabel, (* else raise the exception *)
                    loadMemory(eax, r12, argExceptionPacket),
                    RaiseException
                ]

            |   2 =>
                    (* Windows X64. The first four arguments are in rcx, rdx, r8 and r9.  The rest are
                       on the stack.  The caller must ensure the stack is aligned on 16-byte boundary
                       and must allocate 32-byte save area for the register args.
                       rbx, rbp, rdi, rsi, rsp, r12-r15 are saved by the called function. *)
                [
                    MoveLongConstR{source=entryPointAddr, output=r11}, (* Load the entry point ref. *)
                    loadMemory(r11, r11, 0),(* Load its value. *)
                    ArithRConst{opc=CMP, output=r11, source=0},(* Has it been set? *)
                    checkAddr,
                    PushR eax, PushR ebx, (* Save original arguments.  Also r8/r9/r10 if necessary. *)
                    MoveLongConstR{source=toMachineWord functionName, output=eax},
                    CallFunction(ConstantClosure getEntryPoint),
                    MoveLongConstR{source=entryPointAddr, output=r11}, (* Reload the ref. *)
                    loadMemory(eax, eax, 0), (* Extract the value from the large word. *)
                    StoreRegToMemory { toStore=eax, address=BaseOffset{base=r11, offset=0, index=NoIndex} }, (* Save into ref. *)
                    MoveRR{source=eax, output=r11}, (* Move to the call address register. *)
                    PopR ebx, PopR eax,
                    JumpLabel addrLabel, (* Label to skip to if addr has been set. *)
                    loadMemory(esi, ebp, memRegArgumentPtr), (* Get argument data ptr  - use rsi to save reloading after the call *)
                    StoreConstToMemory{toStore=noException, address=BaseOffset{base=esi, offset=argExceptionPacket, index=NoIndex}}, (* Clear exception *)
                    storeMemory(r15, esi, argLocalMpointer), (* Save r15, the heap pointer *)
                    MoveRR{source=esp, output=edi},  (* Save ML stack pointer *)
                    MoveRR{source=ebp, output=esp}, ArithRConst{ opc=SUB, output=esp, source= LargeInt.fromInt memRegSize}, (* Switch to C stack *)
                    MoveRR{source=eax, output=ecx}, MoveRR{source=ebx, output=edx}, (* Set the argument registers. *)
                    ArithRConst{ opc=SUB, output=esp, source=32}, (* Allocate 32 byte save area *)
                    CallFunction(DirectReg r11), (* Call the C function using the address in r11 *)
                    MoveRR{source=edi, output=esp}, (* Restore the ML stack pointer *)
                    loadMemory(r15, esi, argLocalMpointer), (* Copy heap ptr back. *)
                    loadMemory(edx, esi, argLocalMbottom), storeMemory(edx, ebp, memRegLocalMbottom), (* and base ptr. *)
                    ArithMemConst{opc=CMP, offset=argExceptionPacket, base=esi, source=noException},
                    checkExc, ReturnFromFunction 0, (* Check for exception and return if not. *)
                    JumpLabel exLabel, (* else raise the exception *)
                    loadMemory(eax, esi, argExceptionPacket),
                    RaiseException
                ]

            |   _ =>
                (* X86/32.  Arguments are pushed to the stack.
                   ebx, edi, esi, ebp and esp are saved by the called function.
                   We use esi to hold the argument data pointer and edi to save the ML stack pointer *)
                [
                    MoveLongConstR{source=entryPointAddr, output=ecx}, (* Load the entry point ref. *)
                    loadMemory(ecx, ecx, 0),(* Load its value. *)
                    ArithRConst{opc=CMP, output=ecx, source=0},(* Has it been set? *)
                    checkAddr,
                    PushR eax, PushR ebx, (* Save original arguments. *)
                    MoveLongConstR{source=toMachineWord functionName, output=eax},
                    CallFunction(ConstantClosure getEntryPoint),
                    MoveLongConstR{source=entryPointAddr, output=ecx}, (* Reload the ref. *)
                    loadMemory(eax, eax, 0), (* Extract the value from the large word. *)
                    StoreRegToMemory { toStore=eax, address=BaseOffset{base=ecx, offset=0, index=NoIndex} }, (* Save into ref. *)
                    MoveRR{source=eax, output=ecx}, (* Move to the call address register. *)
                    PopR ebx, PopR eax,
                    JumpLabel addrLabel, (* Label to skip to if addr has been set. *)
                    loadMemory(esi, ebp, memRegArgumentPtr), (* Get argument data ptr - use esi to save reloading after the call *)
                    StoreConstToMemory{toStore=noException, address=BaseOffset{base=esi, offset=argExceptionPacket, index=NoIndex}}, (* Clear exception *)
                    loadMemory(edx, ebp, memRegLocalMPointer), storeMemory(edx, esi, argLocalMpointer), (* Copy heap pointer *)
                    MoveRR{source=esp, output=edi},  (* Save ML stack pointer *)
                    MoveRR{source=ebp, output=esp}, ArithRConst{ opc=SUB, output=esp, source= LargeInt.fromInt memRegSize}, (* Switch to C stack *)
                    PushR ebx, PushR eax, (* Push arguments. *)
                    CallFunction(DirectReg ecx), (* Call the C function using the address in ecx *)
                    MoveRR{source=edi, output=esp}, (* Restore the ML stack pointer *)
                    loadMemory(edx, esi, argLocalMpointer), storeMemory(edx, ebp, memRegLocalMPointer), (* Copy heap ptr back. *)
                    loadMemory(edx, esi, argLocalMbottom), storeMemory(edx, ebp, memRegLocalMbottom), (* and base ptr. *)
                    ArithMemConst{opc=CMP, offset=argExceptionPacket, base=esi, source=noException},
                    checkExc, ReturnFromFunction 0, (* Check for exception and return if not. *)
                    JumpLabel exLabel, (* else raise the exception *)
                    loadMemory(eax, esi, argExceptionPacket),
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
