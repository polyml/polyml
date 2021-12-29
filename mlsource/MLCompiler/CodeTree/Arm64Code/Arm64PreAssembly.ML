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
    and bitfieldKind = BFUnsigned | BFSigned | BFInsert

    datatype precode =
        (* Basic instructions *)
        AddImmediate of {regN: xReg, regD: xReg, immed: word, shifted: bool, opSize: opSize, setFlags: bool}
    |   SubImmediate of {regN: xReg, regD: xReg, immed: word, shifted: bool, opSize: opSize, setFlags: bool}
    |   AddShiftedReg of {regM: xReg, regN: xReg, regD: xReg, shift: shiftType, opSize: opSize, setFlags: bool}
    |   SubShiftedReg of {regM: xReg, regN: xReg, regD: xReg, shift: shiftType, opSize: opSize, setFlags: bool}
    |   AddExtendedReg of {regM: xReg, regN: xReg, regD: xReg, extend: Word8.word extend, opSize: opSize, setFlags: bool}
    |   SubExtendedReg of {regM: xReg, regN: xReg, regD: xReg, extend: Word8.word extend, opSize: opSize, setFlags: bool}
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
    |   LoadAcquireReg of {regN: xReg, regT: xReg, loadType: loadType}
    |   StoreReleaseReg of {regN: xReg, regT: xReg, loadType: loadType}
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
    |   CvtIntToFP of { regN: xReg, regD: vReg, floatSize: floatSize, opSize: opSize}
    |   CvtFloatToInt of { round: IEEEReal.rounding_mode, regN: vReg, regD: xReg, floatSize: floatSize, opSize: opSize}
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
    |   RTSTrap of { rtsEntry: int, work: xReg, save: xReg list }


    fun toAssembler([], code) = code

    |   toAssembler(AddImmediate{regN, regD, immed, shifted, opSize, setFlags} :: rest, code) =
        let
            val instr =
                case (opSize, setFlags) of
                    (OpSize64, false) => addImmediate
                |   (OpSize32, false) => addImmediate32
                |   (OpSize64, true) => addSImmediate
                |   (OpSize32, true) => addSImmediate32
        in
            toAssembler(rest, instr{regN=regN, regD=regD, immed=immed, shifted=shifted} :: code)
        end

    |   toAssembler(SubImmediate{regN, regD, immed, shifted, opSize, setFlags} :: rest, code) =
        let
            val instr =
                case (opSize, setFlags) of
                    (OpSize64, false) => subImmediate
                |   (OpSize32, false) => subImmediate32
                |   (OpSize64, true) => subSImmediate
                |   (OpSize32, true) => subSImmediate32
        in
            toAssembler(rest, instr{regN=regN, regD=regD, immed=immed, shifted=shifted} :: code)
        end

    |   toAssembler(AddShiftedReg{regM, regN, regD, shift, opSize, setFlags} :: rest, code) =
        let
            val instr =
                case (opSize, setFlags) of
                    (OpSize64, false) => addShiftedReg
                |   (OpSize32, false) => addShiftedReg32
                |   (OpSize64, true) => addSShiftedReg
                |   (OpSize32, true) => addSShiftedReg32
        in
            toAssembler(rest, instr{regM=regM, regN=regN, regD=regD, shift=shift} :: code)
        end

    |   toAssembler(SubShiftedReg{regM, regN, regD, shift, opSize, setFlags} :: rest, code) =
        let
            val instr =
                case (opSize, setFlags) of
                    (OpSize64, false) => subShiftedReg
                |   (OpSize32, false) => subShiftedReg32
                |   (OpSize64, true) => subSShiftedReg
                |   (OpSize32, true) => subSShiftedReg32
        in
            toAssembler(rest, instr{regM=regM, regN=regN, regD=regD, shift=shift} :: code)
        end

    |   toAssembler(AddExtendedReg{regM, regN, regD, extend, opSize, setFlags} :: rest, code) =
        (* Add/SubExtended are only used to access XSP. *)
        let
            val instr =
                case (opSize, setFlags) of
                    (OpSize64, false) => addExtendedReg
                |   (OpSize32, false) => raise InternalError "AddExtendedReg; 32"
                |   (OpSize64, true) => addSExtendedReg
                |   (OpSize32, true) => raise InternalError "AddExtendedReg; 32"
        in
            toAssembler(rest, instr{regM=regM, regN=regN, regD=regD, extend=extend} :: code)
        end

    |   toAssembler(SubExtendedReg{regM, regN, regD, extend, opSize, setFlags} :: rest, code) =
        let
            val instr =
                case (opSize, setFlags) of
                    (OpSize64, false) => subExtendedReg
                |   (OpSize32, false) => raise InternalError "AddExtendedReg; 32"
                |   (OpSize64, true) => subSExtendedReg
                |   (OpSize32, true) => raise InternalError "AddExtendedReg; 32"
        in
            toAssembler(rest, instr{regM=regM, regN=regN, regD=regD, extend=extend} :: code)
        end

    |   toAssembler(LoadRegScaled{regT, regN, unitOffset, loadType} :: rest, code) =
        let
            val instr =
                case loadType of
                    Load64 => loadRegScaled
                |   Load32 => loadRegScaled32
                |   Load16 => loadRegScaled16
                |   Load8 => loadRegScaledByte
        in
            toAssembler(rest, instr{regT=regT, regN=regN, unitOffset=unitOffset} :: code)
        end

    |   toAssembler(LoadFPRegScaled{regT, regN, unitOffset, floatSize} :: rest, code) =
        let
            val instr =
                case floatSize of
                    Float32 => loadRegScaledFloat
                |   Double64 => loadRegScaledDouble
        in
            toAssembler(rest, instr{regT=regT, regN=regN, unitOffset=unitOffset} :: code)
        end

    |   toAssembler(StoreRegScaled{regT, regN, unitOffset, loadType} :: rest, code) =
        let
            val instr =
                case loadType of
                    Load64 => storeRegScaled
                |   Load32 => storeRegScaled32
                |   Load16 => storeRegScaled16
                |   Load8 => storeRegScaledByte
        in
            toAssembler(rest, instr{regT=regT, regN=regN, unitOffset=unitOffset} :: code)
        end

    |   toAssembler(StoreFPRegScaled{regT, regN, unitOffset, floatSize} :: rest, code) =
        let
            val instr =
                case floatSize of
                    Float32 => storeRegScaledFloat
                |   Double64 => storeRegScaledDouble
        in
            toAssembler(rest, instr{regT=regT, regN=regN, unitOffset=unitOffset} :: code)
        end

    |   toAssembler(LoadRegUnscaled{regT, regN, byteOffset, loadType, unscaledType} :: rest, code) =
        let
            val instr =
                case (loadType, unscaledType) of
                    (Load64, NoUpdate) => loadRegUnscaled
                |   (Load32, NoUpdate) => loadRegUnscaled32
                |   (Load16, NoUpdate) => loadRegUnscaled16
                |   (Load8, NoUpdate) => loadRegUnscaledByte
                |   (Load64, PreIndex) => loadRegPreIndex
                |   (Load32, PreIndex) => loadRegPreIndex32
                |   (Load16, PreIndex) => raise InternalError "loadRegPreIndex16"
                |   (Load8, PreIndex) => loadRegPreIndexByte
                |   (Load64, PostIndex) => loadRegPostIndex
                |   (Load32, PostIndex) => loadRegPostIndex32
                |   (Load16, PostIndex) => raise InternalError "loadRegPostIndex16"
                |   (Load8, PostIndex) => loadRegPostIndexByte
        in
            toAssembler(rest, instr{regT=regT, regN=regN, byteOffset=byteOffset} :: code)
        end

    |   toAssembler(LoadFPRegUnscaled{regT, regN, byteOffset, floatSize, unscaledType} :: rest, code) =
        let
            val instr =
                case (floatSize, unscaledType) of
                    (Float32, NoUpdate) => loadRegUnscaledFloat
                |   (Double64, NoUpdate) => loadRegUnscaledDouble
                |   _ => raise InternalError "LoadFPRegUnscaled: pre/post indexed"
        in
            toAssembler(rest, instr{regT=regT, regN=regN, byteOffset=byteOffset} :: code)
        end

    |   toAssembler(StoreRegUnscaled{regT, regN, byteOffset, loadType, unscaledType} :: rest, code) =
        let
            val instr =
                case (loadType, unscaledType) of
                    (Load64, NoUpdate) => storeRegUnscaled
                |   (Load32, NoUpdate) => storeRegUnscaled32
                |   (Load16, NoUpdate) => storeRegUnscaled16
                |   (Load8, NoUpdate) => storeRegUnscaledByte
                |   (Load64, PreIndex) => storeRegPreIndex
                |   (Load32, PreIndex) => storeRegPreIndex32
                |   (Load16, PreIndex) => raise InternalError "storeRegPreIndex16"
                |   (Load8, PreIndex) => storeRegPreIndexByte
                |   (Load64, PostIndex) => storeRegPostIndex
                |   (Load32, PostIndex) => storeRegPostIndex32
                |   (Load16, PostIndex) => raise InternalError "storeRegPostIndex16"
                |   (Load8, PostIndex) => storeRegPostIndexByte
        in
            toAssembler(rest, instr{regT=regT, regN=regN, byteOffset=byteOffset} :: code)
        end

    |   toAssembler(StoreFPRegUnscaled{regT, regN, byteOffset, floatSize, unscaledType} :: rest, code) =
        let
            val instr =
                case (floatSize, unscaledType) of
                    (Float32, NoUpdate) => storeRegUnscaledFloat
                |   (Double64, NoUpdate) => storeRegUnscaledDouble
                |   _ => raise InternalError "StoreFPRegUnscaled: pre/post indexed"
        in
            toAssembler(rest, instr{regT=regT, regN=regN, byteOffset=byteOffset} :: code)
        end
 
    |   toAssembler _ = raise Fail "TODO"


    fun toInstr precode =
        case toAssembler([precode], []) of
            [single] => single
        |   _ => raise InternalError "toInstr"

    fun generateFinalCode
        {instrs, name, parameters, resultClosure, profileObject} = raise Fail "TODO"



    structure Sharing =
    struct
        type closureRef = closureRef
        type loadType = loadType
        type opSize = opSize
        type logicalOp = logicalOp
        type floatSize = floatSize
        type shiftDirection = shiftDirection
        type multKind = multKind
        type fpUnary = fpUnary
        type fpBinary = fpBinary
        type unscaledType = unscaledType
        type condSet = condSet
        type bitfieldKind = bitfieldKind
        type precode = precode
        type xReg = xReg
        type vReg = vReg
        type labels = labels
        type condition = condition
        type shiftType = shiftType
        type wordSize = wordSize
        type 'a extend = 'a extend
        type scale = scale
        type instr = instr
    end

end;
