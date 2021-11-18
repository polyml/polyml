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

    val generalRegisters =
        [XReg 0w0, XReg 0w1, XReg 0w2, XReg 0w3, XReg 0w4, XReg 0w5, XReg 0w6, XReg 0w7,
         XReg 0w8, XReg 0w9, XReg 0w10, XReg 0w11, XReg 0w12, XReg 0w13, XReg 0w14, XReg 0w15,
         XReg 0w19, XReg 0w20, XReg 0w21, XReg 0w22, XReg 0w23] @ (if is32in64 then [] else [XReg 0w24])

    fun allocateRegisters _ = raise Fallback "allocateRegisters"

    val nGenRegs = List.length generalRegisters

    structure Sharing =
    struct
        type intSet             = intSet
        and extendedBasicBlock  = extendedBasicBlock
        and regProperty         = regProperty
        and xReg                = xReg
        and allocateResult      = allocateResult
    end

end;
