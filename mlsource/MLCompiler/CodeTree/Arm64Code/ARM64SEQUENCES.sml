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

(* Sequences of instructions for various purposes. *)

functor ARM64SEQUENCES (
    structure Arm64Assembly: Arm64Assembly
) : Arm64Sequences =

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
            if value < 0wx10000000
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
    


    structure Sharing =
    struct
        type instr = instr
        type xReg = xReg
        type vReg = vReg
    end
end;
