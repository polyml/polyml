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

signature ARM64SEQUENCES =
sig
    type instr and xReg and vReg
    
    (* Copy a value to another register. *)
    val moveRegToReg: {sReg: xReg, dReg: xReg} -> instr
    
    (* Load a non-address constant.  Tries to use movz/movn/movk if
       that can be done easily, othewise uses loadNonAddressConstant to
       load the value from the non-address constant area. *)
    val loadNonAddress: xReg * Word64.word -> instr list

    structure Sharing:
    sig
        type instr = instr
        type xReg = xReg
        type vReg = vReg
    end
end;
