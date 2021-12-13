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

(* Sequences of instructions for various purposes that are
   used both in the main code-generator and the FFI code. *)

functor Arm64Sequences (
    structure Arm64Assembly: ARM64ASSEMBLY
) : ARM64SEQUENCES =

struct
    open Arm64Assembly

    (* Move register.  If we're moving to/from SP we have to use addImmediate *)
    fun moveRegToReg{sReg=XSP, dReg} =
            addImmediate{regN=XSP, regD=dReg, immed=0w0, shifted=false}
    |   moveRegToReg{sReg, dReg=XSP} =
            addImmediate{regN=sReg, regD=XSP, immed=0w0, shifted=false}
    |   moveRegToReg{sReg, dReg} =
            orrShiftedReg{regN=XZero, regM=sReg, regD=dReg, shift=ShiftNone}
     
    (* Load a non-address constant.  Tries to use movz/movn/movk if
       that can be done easily, othewise uses loadNonAddressConstant to
       load the value from the non-address constant area. *)
    local
        fun extW (v, h) = Word.andb(Word.fromLarge(LargeWord.>>(Word64.toLarge v, h*0w16)), 0wxffff)
        (* Use one or more movk instructions to change the current value into
           the target value. *)
        (*fun modifyValue(xReg, current, target) =
            if current = target
            then []
            else
            let
                val curr0 = extW(current, 0w3) and curr1 = extW(current, 0w2)
                and curr2 = extW(current, 0w1)
                and targ0 = extW(target, 0w3) and targ1 = extW(target, 0w2)
                and targ2 = extW(target, 0w1) and targ3 = extW(target, 0w0)
                fun replace(v, s) =
                    Word64.orb(Word64.<<(Word64.fromLarge(Word.toLarge v), s),
                        Word64.andb(current, Word64.notb(Word64.<<(0wxffff, s))))
            in
                if curr0 <> targ0
                then moveKeep{regD=xReg, immediate=targ0, shift=0w48} ::
                    modifyValue(xReg, replace(targ0, 0w48), target)
                else if curr1 <> targ1
                then moveKeep{regD=xReg, immediate=targ1, shift=0w32} ::
                    modifyValue(xReg, replace(targ1, 0w32), target)
                else if curr2 <> targ2
                then moveKeep{regD=xReg, immediate=targ2, shift=0w16} ::
                    modifyValue(xReg, replace(targ2, 0w16), target)
                else [moveKeep{regD=xReg, immediate=targ3, shift=0w0}]
            end*)
    in
        fun loadNonAddress(xReg, value) =
        let
            val hw0 = extW(value, 0w3) and hw1 = extW(value, 0w2)
            and hw2 = extW(value, 0w1) and hw3 = extW(value, 0w0)
        in
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
                then [moveZero32{regD=xReg, immediate=lo, shift=0w0}]
                else if hi = 0wxffff
                then [moveNot32{regD=xReg, immediate=Word.xorb(0wxffff, lo), shift=0w0}]
                else if lo = 0w0
                then [moveZero32{regD=xReg, immediate=hi, shift=0w16}]
                else if isEncodableBitPattern(value, WordSize32)
                then [bitwiseOrImmediate32{bits=value, regN=XZero, regD=xReg}]
                else (* Have to use two instructions *)
                    [
                        moveZero32{regD=xReg, immediate=lo, shift=0w0},
                        moveKeep{regD=xReg, immediate=hi, shift=0w16}
                    ]  
            end
            else if hw0 = 0wxffff andalso hw1 = 0wxffff andalso hw2 = 0wxffff
            then [moveNot{regD=xReg, immediate=Word.xorb(0wxffff, hw3), shift=0w0}]
            else if hw1 = 0w0 andalso hw2 = 0w0
            then (* This is common for length words with a flags byte *)
                [
                    moveZero32{regD=xReg, immediate=hw3, shift=0w0},
                    moveKeep{regD=xReg, immediate=hw0, shift=0w48}
                ]  
            else [loadNonAddressConstant(xReg, value)]

        end
    end

    local
        fun allocateWords(fixedReg, workReg, words, bytes, regMask) =
        let
            val label = createLabel()
            (* Because X30 is used in the branchAndLink it has to be pushed
               across any trap. *)
            val saveX30 = List.exists (fn r => r = X30) regMask
            val preserve = List.filter (fn r => r <> X30) regMask
        in
            [
                (* Subtract the number of bytes required from the heap pointer. *)
                subImmediate{regN=X_MLHeapAllocPtr, regD=fixedReg, immed=bytes, shifted=false},
                (* Compare the result with the heap limit. *)
                subSShiftedReg{regM=X_MLHeapLimit, regN=fixedReg, regD=XZero, shift=ShiftNone},
                conditionalBranch(CondCarrySet, label)
            ] @
            (if saveX30 then [storeRegPreIndex{regT=X30, regN=X_MLStackPtr, byteOffset= ~8}] else []) @
            [
                loadRegScaled{regT=X16, regN=X_MLAssemblyInt, unitOffset=heapOverflowCallOffset},
                branchAndLinkReg X16,
                registerMask preserve
            ] @
            (if saveX30 then [loadRegPostIndex{regT=X30, regN=X_MLStackPtr, byteOffset= 8}] else []) @
            [
                setLabel label,
                (* Update the heap pointer. *)
                moveRegToReg{sReg=fixedReg, dReg=X_MLHeapAllocPtr}
            ] @
                loadNonAddress(workReg,
                    Word64.orb(words, Word64.<<(Word64.fromLarge(Word8.toLarge Address.F_bytes),
                        if is32in64 then 0w24 else 0w56)))
            @
            [
                (* Store the length word.  Have to use the unaligned version because offset is -ve. *)
                if is32in64
                then storeRegUnscaled32{regT=workReg, regN=fixedReg, byteOffset= ~4}
                else storeRegUnscaled{regT=workReg, regN=fixedReg, byteOffset= ~8}
            ]
        end

        fun absoluteAddressToIndex reg =
        if is32in64
        then
        [
            subShiftedReg{regM=X_Base32in64, regN=reg, regD=reg, shift=ShiftNone},
            logicalShiftRight{shift=0w2, regN=reg, regD=reg}
        ]
        else []

    in
        fun boxDouble{source, destination, workReg, saveRegs} =
            allocateWords(destination, workReg, if is32in64 then 0w2 else 0w1, 0w16, saveRegs) @
                storeRegScaledDouble{regT=source, regN=destination, unitOffset=0} ::
                absoluteAddressToIndex destination
        and boxSysWord{source, destination, workReg, saveRegs} =
            allocateWords(destination, workReg, if is32in64 then 0w2 else 0w1, 0w16, saveRegs) @
                storeRegScaled{regT=source, regN=destination, unitOffset=0} ::
                absoluteAddressToIndex destination
        and boxFloat{source, destination, workReg, saveRegs} =
            allocateWords(destination, workReg, 0w1, 0w8, saveRegs) @
                storeRegScaledFloat{regT=source, regN=destination, unitOffset=0} ::
                absoluteAddressToIndex destination
    end

    structure Sharing =
    struct
        type instr = instr
        type xReg = xReg
        type vReg = vReg
    end
end;
