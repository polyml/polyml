(*
    Copyright (c) 2021 David C. J. Matthews

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    Licence version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public Licence for more details.
    
    You should have received a copy of the GNU Lesser General Public
    Licence along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

signature Arm64Assembly =
sig
    type code
    type closureRef
    type instr
    type machineWord = Address.machineWord
    
    (* Create a code value for the function. *)
    val codeCreate: string * Universal.universal list -> code

    type xReg
    val X0:  xReg   and X1:  xReg   and X2:  xReg   and X3: xReg
    and X4:  xReg   and X5:  xReg   and X6:  xReg   and X7: xReg
    and X8:  xReg   and X9:  xReg   and X10: xReg   and X11: xReg
    and X12: xReg   and X13: xReg   and X14: xReg   and X15: xReg
    and X16: xReg   and X17: xReg   and X18: xReg   and X19: xReg
    and X20: xReg   and X21: xReg   and X22: xReg   and X23: xReg
    and X24: xReg   and X25: xReg   and X26: xReg   and X27: xReg
    and X28: xReg   and X29: xReg   and X30: xReg
    
    (* XZero and XSP are both encoded as 31 but the interpretation
       depends on the instruction *)
    val XZero: xReg and XSP: xReg
    
    val X_MLHeapLimit: xReg (* ML Heap limit pointer *)
    and X_MLAssemblyInt: xReg (* ML assembly interface pointer. *)
    and X_MLHeapAllocPtr: xReg (* ML Heap allocation pointer. *)
    and X_MLStackPtr: xReg (* ML Stack pointer. *)
    and X_LinkReg: xReg (* Link reg - return address *)


    (* Jump to the address in the register and put the address of the
       next instruction into X30. *)
    val genBranchAndLinkReg: xReg * code -> unit
    (* Jump to the address in the register. *)
    and genBranchRegister: xReg * code -> unit
    (* Jump to the address in the register and hint this is a return. *)
    and genReturnRegister: xReg * code -> unit

    (* Push a register to the ML stack *)
    val genPushReg: xReg * code -> unit
    (* Pop a register from the ML stack. *)
    val genPopReg: xReg * code -> unit

    (* Move a short constant to a register.  Currently limited to unsigned 16-bits. *)
    val genMoveShortConstToReg: xReg * int * code -> unit
    
    (* Move a long constant to a register. *)
    val genLoadConstant: xReg * machineWord * code -> unit

    (* Add a 12-bit constant, possibly shifted by 12 bits *)
    val genAddRegConstant: {sReg: xReg, dReg: xReg, cValue: int, shifted: bool} * code -> unit
    
    (* Load/Store an aligned word using a 12-bit offset. *)
    val loadRegAligned: {dest: xReg, base: xReg, wordOffset: int} * code -> unit
    and storeRegAligned: {dest: xReg, base: xReg, wordOffset: int} * code -> unit

    (* copyCode - create the vector of code and update the closure reference to
       point to it. *)
    val generateCode: {code: code, maxStack: int, resultClosure: closureRef} -> unit

    exception Fallback (* During development only. *)

    structure Sharing:
    sig
        type code = code
        type closureRef = closureRef
        type instr = instr
        type xReg = xReg
    end
end;
