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

(* The pre-assembly layer goes below the icode and allows peep-hole optimisation. *)

functor Arm64PreAssembly(

    structure Arm64Assembly: ARM64ASSEMBLY
    structure Debug: DEBUG
    structure Pretty: PRETTY

): ARM64PREASSEMBLY =
struct
    open Arm64Assembly
    
    exception InternalError = Misc.InternalError

    (* Many of the datatypes are inherited from Arm64Assembly *)

    datatype loadType = Load64 | Load32 | Load16 | Load8
    and opSize = OpSize32 | OpSize64
    and logicalOp = LogAnd | LogOr | LogXor
    and floatSize = Float32 | Double64
    and shiftDirection = ShiftLeft | ShiftRightLogical | ShiftRightArithmetic
    and multKind =
        MultAdd32 | MultSub32 | MultAdd64 | MultSub64 |
        SignedMultAddLong (* 32bit*32bit + 64bit => 64Bit *) |
        SignedMultHigh (* High order part of 64bit*64Bit *)
    and fpUnary = NegFloat | NegDouble | AbsFloat | AbsDouble | ConvFloatToDble | ConvDbleToFloat
    and fpBinary = MultiplyFP | DivideFP | AddFP | SubtractFP
    and unscaledType = NoUpdate | PreIndex | PostIndex
    and condSet = CondSet | CondSetIncr | CondSetInvert | CondSetNegate
    and bitfieldKind = BitFieldUnsigned | BitFieldSigned | BitFieldInsert

    datatype precode =
        (* Basic instructions *)
        AddSubImmediate of
            {regN: xReg, regD: xReg, immed: word, shifted: bool, isAdd: bool, opSize: opSize, setFlags: bool}
    |   AddSubShiftedReg of
            {regM: xReg, regN: xReg, regD: xReg, shift: shiftType, isAdd: bool, opSize: opSize, setFlags: bool}
    |   AddSubExtendedReg of
            {regM: xReg, regN: xReg, regD: xReg, extend: Word8.word extend, isAdd: bool, opSize: opSize, setFlags: bool}
    |   MultiplyAndAddSub of
            {regM: xReg, regN: xReg, regA: xReg, regD: xReg, isAdd: bool, opSize: opSize, multKind: multKind}
    |   DivideRegs of
            {regM: xReg, regN: xReg, regD: xReg, isSigned: bool, opSize: opSize}
    |   LogicalShiftedReg of
            {regM: xReg, regN: xReg, regD: xReg, shift: shiftType, logOp: logicalOp, opSize: opSize, setFlags: bool}
    |   LoadRegScaled of
            {regT: xReg, regN: xReg, unitOffset: int, loadType: loadType}
    |   LoadFPRegScaled of
            {regT: vReg, regN: xReg, unitOffset: int, floatSize: floatSize}
    |   StoreRegScaled of
            {regT: xReg, regN: xReg, unitOffset: int, loadType: loadType}
    |   StoreFPRegScaled of
            {regT: vReg, regN: xReg, unitOffset: int, floatSize: floatSize}
    |   LoadRegUnscaled of
            {regT: xReg, regN: xReg, byteOffset: int, loadType: loadType, unscaledType: unscaledType}
    |   StoreRegUnscaled of
            {regT: xReg, regN: xReg, byteOffset: int, loadType: loadType, unscaledType: unscaledType}
    |   LoadFPRegUnscaled of
            {regT: vReg, regN: xReg, byteOffset: int, floatSize: floatSize, unscaledType: unscaledType}
    |   StoreFPRegUnscaled of
            {regT: vReg, regN: xReg, byteOffset: int, floatSize: floatSize, unscaledType: unscaledType}
    |   LoadAcquire of {regN: xReg, regT: xReg, loadType: loadType}
    |   StoreRelease of {regN: xReg, regT: xReg, loadType: loadType}
    |   LoadRegPair of { regT1: xReg, regT2: xReg, regN: xReg, unitOffset: int, unscaledType: unscaledType}
    |   StoreRegPair of{ regT1: xReg, regT2: xReg, regN: xReg, unitOffset: int, unscaledType: unscaledType}
    |   LoadFPRegPair of { regT1: vReg, regT2: vReg, regN: xReg, unitOffset: int, unscaledType: unscaledType}
    |   StoreFPRegPair of { regT1: vReg, regT2: vReg, regN: xReg, unitOffset: int, unscaledType: unscaledType}
    |   ConditionalSet of {regD: xReg, regTrue: xReg, regFalse: xReg, cond: condition, condSet: condSet}
    |   BitField of {immr: word, imms: word, regN: xReg, regD: xReg, opSize: opSize, bitfieldKind: bitfieldKind}
    |   ShiftRegisterVariable of {regM: xReg, regN: xReg, regD: xReg, opSize: opSize, shiftDirection: shiftDirection}
    |   BitwiseLogical of { bits: Word64.word, regN: xReg, regD: xReg, opSize: opSize, setFlags: bool, logOp: logicalOp}
        (* Floating point *)
    |   MoveGeneralToFP of { regN: xReg, regD: vReg, floatSize: floatSize}
    |   MoveFPToGeneral of {regN: vReg, regD: xReg, floatSize: floatSize}
    |   ConvertIntToFP of { regN: xReg, regD: vReg, floatSize: floatSize, opSize: opSize}
    |   ConvertFloatToInt of { round: IEEEReal.rounding_mode, regN: vReg, regD: xReg, floatSize: floatSize, opSize: opSize}
    |   FPBinaryOp of { regM: vReg, regN: vReg, regD: vReg, floatSize: floatSize, fpOp: fpBinary}
    |   FPComparison of { regM: vReg, regN: vReg, floatSize: floatSize}
    |   FPUnaryOp of {regN: vReg, regD: vReg, floatSize: floatSize, fpOp: fpUnary}
        (* Branches and Labels. *)
    |   SetLabel of labels
    |   ConditionalBranch of condition * labels
    |   UnconditionalBranch of labels
    |   BranchAndLink of labels
    |   LoadLabelAddress of xReg * labels
    |   TestBitBranch of { test: xReg, bit: Word8.word, label: labels, onZero: bool }
    |   CompareBranch of { test: xReg, label: labels, onZero: bool, opSize: opSize }
        (* Composite instructions *)
    |   MoveXRegToXReg of {sReg: xReg, dReg: xReg}
    |   LoadNonAddr of xReg * Word64.word
    |   LoadAddr of xReg * machineWord

    fun generateCode
        {instrs, name, parameters, resultClosure, profileObject} = raise Fail "TODO"



    structure Sharing =
    struct
        type closureRef = closureRef
        type precode = precode
        type xReg = xReg
        type vReg = vReg
        type labels = labels
        type condition = condition
        type shiftType = shiftType
        type wordSize = wordSize
        type 'a extend = 'a extend
        type scale = scale
    end

end;
