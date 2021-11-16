(*
    Copyright David C. J. Matthews 2016-21

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

functor Arm64AllocateRegisters(
    structure Arm64ICode: ARM64ICODE
    structure Identify: ARM64IDENTIFYREFERENCES
    structure ConflictSets: ARM64ICODECONFLICTS
    structure IntSet: INTSET
    
    sharing Arm64ICode.Sharing = Identify.Sharing = ConflictSets.Sharing = IntSet
): ARM64ALLOCATEREGISTERS =
struct
    open Arm64ICode
    open Identify
    open ConflictSets
    open IntSet

    open Address
    
    datatype allocateResult =
        AllocateSuccess of xReg vector
    |   AllocateFailure of intSet list

    fun allocateRegisters _ = raise Fallback "allocateRegisters"

    structure Sharing =
    struct
        type intSet             = intSet
        and extendedBasicBlock  = extendedBasicBlock
        and regProperty         = regProperty
        and xReg                = xReg
        and allocateResult      = allocateResult
    end

end;
