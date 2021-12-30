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

    (* Reversed cons and append to make the code easier to read. *)
    infix 5 <::> <@>
    fun tl <::> hd = hd :: tl
    and snd <@> fst = fst @ snd

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
    and brRegType = BRRBranch | BRRAndLink | BRRReturn

    datatype precode =
        (* Basic instructions *)
        AddImmediate of {regN: xReg, regD: xReg, immed: word, shifted: bool, opSize: opSize, setFlags: bool}
    |   SubImmediate of {regN: xReg, regD: xReg, immed: word, shifted: bool, opSize: opSize, setFlags: bool}
    |   AddShiftedReg of {regM: xReg, regN: xReg, regD: xReg, shift: shiftType, opSize: opSize, setFlags: bool}
    |   SubShiftedReg of {regM: xReg, regN: xReg, regD: xReg, shift: shiftType, opSize: opSize, setFlags: bool}
    |   AddExtendedReg of {regM: xReg, regN: xReg, regD: xReg, extend: Word8.word extend, opSize: opSize, setFlags: bool}
    |   SubExtendedReg of {regM: xReg, regN: xReg, regD: xReg, extend: Word8.word extend, opSize: opSize, setFlags: bool}
    |   MultiplyAndAddSub of {regM: xReg, regN: xReg, regA: xReg, regD: xReg, multKind: multKind}
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
    |   BranchReg of {regD: xReg, brRegType: brRegType }
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
            toAssembler(rest, code <::> instr{regN=regN, regD=regD, immed=immed, shifted=shifted})
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
            toAssembler(rest, code <::> instr{regN=regN, regD=regD, immed=immed, shifted=shifted})
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
            toAssembler(rest, code <::> instr{regM=regM, regN=regN, regD=regD, shift=shift})
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
            toAssembler(rest, code <::> instr{regM=regM, regN=regN, regD=regD, shift=shift})
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
            toAssembler(rest, code <::> instr{regM=regM, regN=regN, regD=regD, extend=extend})
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
            toAssembler(rest, code <::> instr{regM=regM, regN=regN, regD=regD, extend=extend})
        end

    |   toAssembler(MultiplyAndAddSub{regM, regN, regA, regD, multKind} :: rest, code) =
        let
            val instr =
                case multKind of
                    MultAdd32 => multiplyAndAdd32{regM=regM, regN=regN, regA=regA, regD=regD}
                |   MultSub32 => multiplyAndSub32{regM=regM, regN=regN, regA=regA, regD=regD}
                |   MultAdd64 => multiplyAndAdd{regM=regM, regN=regN, regA=regA, regD=regD}
                |   MultSub64 => multiplyAndSub{regM=regM, regN=regN, regA=regA, regD=regD}
                |   SignedMultAddLong => signedMultiplyAndAddLong{regM=regM, regN=regN, regA=regA, regD=regD}
                |   SignedMultHigh => signedMultiplyHigh{regM=regM, regN=regN, regD=regD}
        in
            toAssembler(rest, code <::> instr)
        end

    |   toAssembler(DivideRegs{regM, regN, regD, isSigned, opSize} :: rest, code) =
        let
            val instr =
                case (isSigned, opSize) of
                    (true, OpSize64) => signedDivide
                |   (true, OpSize32) => signedDivide32
                |   (false, OpSize64) => unsignedDivide
                |   (false, OpSize32) => unsignedDivide32
        in
            toAssembler(rest, code <::> instr{regN=regN, regM=regM, regD=regD})
        end

    |   toAssembler(LogicalShiftedReg{regM, regN, regD, shift, logOp, opSize, setFlags} :: rest, code) =
        let
            val instr =
                case (logOp, setFlags, opSize) of
                    (LogAnd, false, OpSize64) => andShiftedReg
                |   (LogAnd, true, OpSize64) => andsShiftedReg
                |   (LogOr, false, OpSize64) => orrShiftedReg
                |   (LogXor, false, OpSize64) => eorShiftedReg

                |   (LogAnd, false, OpSize32) => andShiftedReg32
                |   (LogAnd, true, OpSize32) => andsShiftedReg32
                |   (LogOr, false, OpSize32) => orrShiftedReg32
                |   (LogXor, false, OpSize32) => eorShiftedReg32

                |   _ => raise InternalError "setFlags not valid with OR or XOR"
            (* There are also versions of AND/OR/XOR which operate on a complement (NOT)
               of the shifted register.  It's probably not worth looking for a use for them. *)
        in
            toAssembler(rest, code <::> instr{regN=regN, regM=regM, regD=regD, shift=shift})
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
            toAssembler(rest, code <::> instr{regT=regT, regN=regN, unitOffset=unitOffset})
        end

    |   toAssembler(LoadFPRegScaled{regT, regN, unitOffset, floatSize} :: rest, code) =
        let
            val instr =
                case floatSize of
                    Float32 => loadRegScaledFloat
                |   Double64 => loadRegScaledDouble
        in
            toAssembler(rest, code <::> instr{regT=regT, regN=regN, unitOffset=unitOffset})
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
            toAssembler(rest, code <::> instr{regT=regT, regN=regN, unitOffset=unitOffset})
        end

    |   toAssembler(StoreFPRegScaled{regT, regN, unitOffset, floatSize} :: rest, code) =
        let
            val instr =
                case floatSize of
                    Float32 => storeRegScaledFloat
                |   Double64 => storeRegScaledDouble
        in
            toAssembler(rest, code <::> instr{regT=regT, regN=regN, unitOffset=unitOffset})
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
            toAssembler(rest, code <::> instr{regT=regT, regN=regN, byteOffset=byteOffset})
        end

    |   toAssembler(LoadFPRegUnscaled{regT, regN, byteOffset, floatSize, unscaledType} :: rest, code) =
        let
            val instr =
                case (floatSize, unscaledType) of
                    (Float32, NoUpdate) => loadRegUnscaledFloat
                |   (Double64, NoUpdate) => loadRegUnscaledDouble
                |   _ => raise InternalError "LoadFPRegUnscaled: pre/post indexed"
        in
            toAssembler(rest, code <::> instr{regT=regT, regN=regN, byteOffset=byteOffset})
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
            toAssembler(rest, code <::> instr{regT=regT, regN=regN, byteOffset=byteOffset})
        end

    |   toAssembler(StoreFPRegUnscaled{regT, regN, byteOffset, floatSize, unscaledType} :: rest, code) =
        let
            val instr =
                case (floatSize, unscaledType) of
                    (Float32, NoUpdate) => storeRegUnscaledFloat
                |   (Double64, NoUpdate) => storeRegUnscaledDouble
                |   _ => raise InternalError "StoreFPRegUnscaled: pre/post indexed"
        in
            toAssembler(rest, code <::> instr{regT=regT, regN=regN, byteOffset=byteOffset})
        end

    |   toAssembler(LoadAcquireReg{regN, regT, loadType} :: rest, code) =
        let
            val loadInstr  =
                case loadType of
                    Load64 => loadAcquire
                |   Load32 => loadAcquire32
                |   Load8 => loadAcquireByte
                |   _ => raise InternalError "LoadAcquire: Unsupported size" (* Not used *)
        in
            toAssembler(rest, code <::> loadInstr{regT=regT, regN=regN})
        end

    |   toAssembler(StoreReleaseReg{regN, regT, loadType} :: rest, code) =
        let
            val storeInstr  =
                case loadType of
                    Load64 => storeRelease
                |   Load32 => storeRelease32
                |   Load8 => storeReleaseByte
                |   _ => raise InternalError "StoreRelease: Unsupported size" (* Not used *)
        in
            toAssembler(rest, code <::> storeInstr{regT=regT, regN=regN})
        end

    |   toAssembler(SetLabel label :: rest, code) = toAssembler(rest, code <::> setLabel label)

    |   toAssembler(ConditionalBranch(cond, label) :: rest, code) = toAssembler(rest, code <::> conditionalBranch(cond, label))

        (* Register-register moves - special case for XSP. *)
    |   toAssembler(MoveXRegToXReg{sReg=XSP, dReg} :: rest, code) =
            toAssembler(rest, code <::> addImmediate{regN=XSP, regD=dReg, immed=0w0, shifted=false})
    |   toAssembler(MoveXRegToXReg{sReg, dReg=XSP} :: rest, code) =
            toAssembler(rest, code <::> addImmediate{regN=sReg, regD=XSP, immed=0w0, shifted=false})
    |   toAssembler(MoveXRegToXReg{sReg, dReg} :: rest, code) =
            toAssembler(rest, code <::> orrShiftedReg{regN=XZero, regM=sReg, regD=dReg, shift=ShiftNone})

    |   toAssembler(LoadNonAddr(xReg, value) :: rest, code) =
        let
            (* Load a non-address constant.  Tries to use movz/movn/movk if
               that can be done easily, othewise uses loadNonAddressConstant to
               load the value from the non-address constant area. *)
            fun extW (v, h) = Word.andb(Word.fromLarge(LargeWord.>>(Word64.toLarge v, h*0w16)), 0wxffff)
            val hw0 = extW(value, 0w3) and hw1 = extW(value, 0w2)
            and hw2 = extW(value, 0w1) and hw3 = extW(value, 0w0)
            val nextCode =
                if value < 0wx100000000
                then
                let
                    (* 32-bit constants can be loaded using at most a movz and movk but
                       various cases can be reduced since all 32-bit operations set
                       the top word to zero. *)
                    val hi = hw2
                    and lo = hw3
                in
                    (* 32-bit constants can be loaded with at most a movz and a movk but
                       it may be that there is something shorter. *)
                    if hi = 0w0
                    then code <::> moveZero32{regD=xReg, immediate=lo, shift=0w0}
                    else if hi = 0wxffff
                    then code <::> moveNot32{regD=xReg, immediate=Word.xorb(0wxffff, lo), shift=0w0}
                    else if lo = 0w0
                    then code <::> moveZero32{regD=xReg, immediate=hi, shift=0w16}
                    else if isEncodableBitPattern(value, WordSize32)
                    then code <::> bitwiseOrImmediate32{bits=value, regN=XZero, regD=xReg}
                    else (* Have to use two instructions *)
                        code <::>
                            moveZero32{regD=xReg, immediate=lo, shift=0w0} <::>
                            moveKeep{regD=xReg, immediate=hi, shift=0w16}
                end
                else if hw0 = 0wxffff andalso hw1 = 0wxffff andalso hw2 = 0wxffff
                then code <::> moveNot{regD=xReg, immediate=Word.xorb(0wxffff, hw3), shift=0w0}
                else if hw1 = 0w0 andalso hw2 = 0w0
                then (* This is common for length words with a flags byte *)
                    code <::> moveZero32{regD=xReg, immediate=hw3, shift=0w0} <::>
                        moveKeep{regD=xReg, immediate=hw0, shift=0w48} 
                else code <::> loadNonAddressConstant(xReg, value)
        in
            toAssembler(rest, nextCode)
        end

    |   toAssembler(RTSTrap{ rtsEntry, work, save } :: rest, code) =
        let
            (* Because X30 is used in the branchAndLink it has to be pushed
               across any trap. *)
            val saveX30 = List.exists (fn r => r = X30) save
            val preserve = List.filter (fn r => r <> X30) save
        in
            toAssembler(rest,
                code <@>
                    (if saveX30 then [storeRegPreIndex{regT=X30, regN=X_MLStackPtr, byteOffset= ~8}] else []) <::>
                loadRegScaled{regT=work, regN=X_MLAssemblyInt, unitOffset=rtsEntry} <::>
                branchAndLinkReg work <::>
                registerMask preserve <@>
                (if saveX30 then [loadRegPostIndex{regT=X30, regN=X_MLStackPtr, byteOffset= 8}] else [])
            )
        end

    |   toAssembler _ = raise Fail "TODO"


    fun toInstr precode =
        case toAssembler([precode], []) of
            [single] => single
        |   _ => raise InternalError "toInstr"

    (* Take a forward order sequence of instructions and generate a forward order output sequence. *)
    fun toInstrs precode = List.rev(toAssembler(precode, []))

    (* Constant shifts are encoded in the immr and imms fields of the bit-field instruction. *)
    fun shiftConstant{ direction, regD, regN, shift, opSize } =
    let
        val (bitfieldKind, immr, imms) =
            case (direction, opSize) of
                (ShiftLeft, OpSize64) => (BFUnsigned, Word.~ shift mod 0w64, 0w64-0w1-shift)
            |   (ShiftLeft, OpSize32) => (BFUnsigned, Word.~ shift mod 0w32, 0w32-0w1-shift)
            |   (ShiftRightLogical, OpSize64) => (BFUnsigned, shift, 0wx3f)
            |   (ShiftRightLogical, OpSize32) => (BFUnsigned, shift, 0wx1f)
            |   (ShiftRightArithmetic, OpSize64) => (BFSigned, shift, 0wx3f)
            |   (ShiftRightArithmetic, OpSize32) => (BFSigned, shift, 0wx1f)
    in
        BitField{ regN=regN, regD=regD, opSize=opSize, immr=immr, imms=imms, bitfieldKind=bitfieldKind }
    end

    (* These sequences are used both in the ML code-generator and in the FFI code so it
       is convenient to have them here and share the code. *)
    local
        fun allocateWords(fixedReg, workReg, words, bytes, regMask, code) =
        let
            val label = createLabel()
            val (lengthWord, setLength, flagShift) = if is32in64 then (~4, Load32, 0w24) else (~8, Load64, 0w56)
        in
            code <::>
            (* Subtract the number of bytes required from the heap pointer. *)
            SubImmediate{regN=X_MLHeapAllocPtr, regD=fixedReg, immed=bytes, shifted=false, opSize=OpSize64, setFlags=false} <::>
            (* Compare the result with the heap limit. *)
            SubShiftedReg{regM=X_MLHeapLimit, regN=fixedReg, regD=XZero, shift=ShiftNone, setFlags=true, opSize=OpSize64} <::>
            ConditionalBranch(CondCarrySet, label) <::>
            RTSTrap{rtsEntry=heapOverflowCallOffset, work=X16, save=regMask} <::>
            SetLabel label <::>
            (* Update the heap pointer. *)
            MoveXRegToXReg{sReg=fixedReg, dReg=X_MLHeapAllocPtr} <::>
            LoadNonAddr(workReg,
                    Word64.orb(words, Word64.<<(Word64.fromLarge(Word8.toLarge Address.F_bytes), flagShift))) <::>
            (* Store the length word.  Have to use the unaligned version because offset is -ve. *)
            StoreRegUnscaled{regT=workReg, regN=fixedReg, byteOffset= lengthWord, loadType=setLength, unscaledType=NoUpdate}
        end

        fun absoluteAddressToIndex(reg, code) =
        if is32in64
        then
            code <::>
            SubShiftedReg{regM=X_Base32in64, regN=reg, regD=reg, shift=ShiftNone, opSize=OpSize64, setFlags=false} <::>
            shiftConstant{direction=ShiftRightLogical, regN=reg, regD=reg, shift=0w2, opSize=OpSize64}
        else code
    in
        fun boxDouble({source, destination, workReg, saveRegs}, code) =
            absoluteAddressToIndex(destination,
                allocateWords(destination, workReg, if is32in64 then 0w2 else 0w1, 0w16, saveRegs, code) <::>
                    StoreFPRegScaled{regT=source, regN=destination, unitOffset=0, floatSize=Double64})
                
        and boxSysWord({source, destination, workReg, saveRegs}, code) =
            absoluteAddressToIndex(destination,
                allocateWords(destination, workReg, if is32in64 then 0w2 else 0w1, 0w16, saveRegs, code) <::>
                    StoreRegScaled{regT=source, regN=destination, unitOffset=0, loadType=Load64})

        and boxFloat({source, destination, workReg, saveRegs}, code) =
            absoluteAddressToIndex(destination, 
                allocateWords(destination, workReg, 0w1, 0w8, saveRegs, code) <::>
                    StoreFPRegScaled{regT=source, regN=destination, unitOffset=0, floatSize=Float32})
    end


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
        type brRegType = brRegType
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
