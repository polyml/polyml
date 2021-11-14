(*
    Copyright (c) 2021 David C.J. Matthews

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

functor Arm64ICodeConflicts(
    structure ICODE: ARM64ICODE
    structure INTSET: INTSET
    structure IDENTIFY: ARM64IDENTIFYREFERENCES
    sharing ICODE.Sharing = IDENTIFY.Sharing = INTSET
): ARM64ICODECONFLICTS =
struct
    open ICODE
    open INTSET
    open IDENTIFY

    type conflictState =
    {
        conflicts: intSet, realConflicts: xReg list
    }
    
    type triple = {instr: arm64ICode, current: intSet, active: intSet}

    exception InternalError = Misc.InternalError
    
    fun getConflictStates _ = raise Fallback "getConflictStates"
 
    structure Sharing =
    struct
        type arm64ICode = arm64ICode
        and xReg = xReg
        and preg = preg
        and intSet = intSet
        and extendedBasicBlock = extendedBasicBlock
    end
end;
