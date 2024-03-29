(*
    Title:      Standard Basis Library: Conversion from floating point to decimal
    Author:     David Matthews

    The underlying conversion code was translated from the C version of Ryu.
    That code is Copyright 2018 Ulf Adams and is licensed under the terms of
    the Apache License version 2.0 or Boost Software License, Version 1.0

    Boost Software License - Version 1.0 - August 17th, 2003

    Boost Licence
    Permission is hereby granted, free of charge, to any person or organization
    obtaining a copy of the software and accompanying documentation covered by
    this license (the "Software") to use, reproduce, display, distribute,
    execute, and transmit the Software, and to prepare derivative works of the
    Software, and to permit third-parties to whom the Software is furnished to
    do so, all subject to the following:

    The copyright notices in the Software and this entire statement, including
    the above license grant, this restriction and the following disclaimer,
    must be included in all copies of the Software, in whole or in part, and
    all derivative works of the Software, unless such copies or derivative
    works are solely in the form of machine-executable object code generated by
    a source language processor.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE, TITLE AND NON-INFRINGEMENT. IN NO EVENT
    SHALL THE COPYRIGHT HOLDERS OR ANYONE DISTRIBUTING THE SOFTWARE BE LIABLE
    FOR ANY DAMAGES OR OTHER LIABILITY, WHETHER IN CONTRACT, TORT OR OTHERWISE,
    ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
    DEALINGS IN THE SOFTWARE.

    
    The ML translation and related code is copyright David Matthews 2023
    and is released under the Boost Licence.  The remainder of the Poly/ML
    system is licensed under LGPL.
*)
(*
    This uses arbitrary precision arithmetic even for Real32.real (float)
    values.  It would be possible to do some of the arithmetic using fixed
    precision integers but that's difficult on 32-bit platforms even for
    float.
*)
(* RealNumbersAsBits is defined in Poly/ML using a faster version.  This
   is provided as an example. *)
(*
structure RealNumbersAsBits:
sig
    (* Floats.  The mantissa will fit in a short int in both 32- and 64-bits . *)
    val floatSignBit: Real32.real -> bool
    and floatExponent: Real32.real -> int
    and floatMantissa: Real32.real -> int
    and floatFromBinary: {sign: bool, exp: int, mantissa: int} -> Real32.real
    
    (* Doubles.  The mantissa is too large for a fixed int in 32-bit mode. *)
    val doubleSignBit: real -> bool
    and doubleExponent: real -> int
    and doubleMantissa: real -> LargeInt.int
    and doubleFromBinary: {sign: bool, exp: int, mantissa: LargeInt.int} -> real
end =
struct
    local
        open PackReal32Big Word8Vector
    in
        fun floatSignBit r = sub(toBytes r, 0) >= 0w128

        fun floatExponent r =
        let
            val v = toBytes r
        in
            Word8.toInt(Word8.andb(sub(v, 0), 0wx7f)) * 2 +
                Word8.toInt(Word8.>>(sub(v, 1), 0w7))
        end

        fun floatMantissa r =
        let
            val v = toBytes r
        in
            (Word8.toInt(Word8.andb(sub(v, 1), 0wx7f)) * 256 +
                Word8.toInt(sub(v, 2))) * 256 +
                    Word8.toInt(sub(v, 3))
        end
        
        fun floatFromBinary{sign, exp, mantissa} =
        let
            val _ = if mantissa < 0 orelse mantissa > 0x800000 then raise General.Domain else ()
            val mw = Word.fromInt mantissa
            val wordToWord8 = Word8.fromLarge o Word.toLarge
            val b0 = Word8.orb(if sign then 0wx80 else 0w0, Word8.>>(Word8.fromInt exp, 0w1))
            val b1 = Word8.orb(Word8.<<(Word8.andb(Word8.fromInt exp, 0w1), 0w7),
                        wordToWord8(Word.>>(mw, 0w16)))
            val b2 = wordToWord8(Word.>>(mw, 0w8))
            val b3 = wordToWord8 mw
        in
            subVec(Word8Vector.fromList[b0, b1, b2, b3], 0)
        end
    end

    local
        open PackRealBig Word8Vector
    in
        fun doubleSignBit r = sub(toBytes r, 0) >= 0w128

        fun doubleExponent r =
        let
            val v = toBytes r
        in
            Word8.toInt(Word8.andb(sub(v, 0), 0wx7f)) * 16 +
                Word8.toInt(Word8.>>(sub(v, 1), 0w4))
        end
        
        fun doubleMantissa r =
        let
            fun fromBytes (i, byte, acc) =
                if i = 0 then 0
                else if i = 1 then Word8.toLargeInt(Word8.andb(byte, 0wx0f))
                else acc*256 + Word8.toLargeInt byte
        in
            foldli fromBytes 0 (toBytes r)
        end

        fun doubleFromBinary{sign, exp, mantissa} =
        let
            val _ = if mantissa < 0 orelse mantissa > 0x10000000000000 then raise General.Domain else ()
            fun toDigits(n, l, mant) =
                if n = 1
                then Word8.orb(if sign then 0wx80 else 0w0, Word8.fromInt(Int.toInt(Int.quot(exp, 16)))) ::
                        Word8.orb(Word8.<<(Word8.fromInt(Int.toInt exp), 0w4),
                            Word8.andb(Word8.fromLargeInt mant, 0wxf)) :: l
                else
                let
                    val (q, r) = IntInf.quotRem(mant, 256)
                in
                    toDigits(n-1, Word8.fromLargeInt r :: l, q)
                end

        in
            subVec(Word8Vector.fromList(toDigits(7, [], mantissa)), 0)
        end
    end
    
end;
*)
structure RealToDecimalConversion:
sig
    val floatToMinimal:
        Real32.real -> {sign:bool, exponent: int, mantissa: LargeInt.int, class: IEEEReal.float_class}
    val doubleToMinimal:
        real -> {sign:bool, exponent: int, mantissa: LargeInt.int, class: IEEEReal.float_class}
    val decimalToFloat: bool * int * int list -> Real32.real
    and decimalToDouble: bool * int * int list -> real
end
=
struct
    (* Common functions *)

    (* Returns floor(log10(2^e)) for values of e between 0 and 1650. *)
    fun log10Pow2 e =
        if e < 0 orelse e > 1650 then raise General.Domain
        else Int.quot(e * 78913, 0x40000) (* >> 18 *)
    (* Returns floor(log10(5^e)) for values of e between 0 and 2620 *)
    and log10Pow5 e =
        if e < 0 orelse e > 2620 then raise General.Domain
        else Int.quot(e * 732923, 0x100000) (* >> 20 *)

    fun log2Pow5 e =
        if e < 0 orelse e > 3528 then raise General.Domain
        else Int.quot(e * 1217359, 0x80000) (* >> 19 *)

    fun pow5bits e = log2Pow5 e + 1

    local
        (* Keep dividing by 5 while the remainder is zero *)
        fun p5 count value =
            if LargeInt.rem(value, 5) <> 0
            then count
            else p5 (count+1) (LargeInt.quot(value, 5))
    in
        (* Returns whether value is divisible by 5 to the power p. *)
        fun multipleOfPow5(value, e5) =
            p5 0 value >= e5
    end

    fun multipleOfPowerOf2(value, p) =
        IntInf.andb(value, IntInf.<<(1, Word.fromInt p) - 1) = 0

    local
        val posTableSize = 326 and invTableSize = 342
        val pow5BitCount = 125 and pow5InvBitCount = 125

        fun createInvSplit i =
        let
            val pow = IntInf.pow(5, i)
            val pow5len = IntInf.log2 pow + 1 (* Bit length *)
            val j = pow5len - 1 + pow5InvBitCount
            val pow5inv = IntInf.<<(1, Word.fromInt j) div pow + 1
        in
            pow5inv
        end

        and createSplit i =
        let
            val pow = IntInf.pow(5, i)
            val pow5len = IntInf.log2 pow + 1 (* Bit length *)
            val shift = pow5len-pow5BitCount
            val pow5 =
                if shift < 0
                then IntInf.<<(pow, Word.fromInt(~shift))
                else IntInf.~>>(pow, Word.fromInt shift)
        in
            pow5
        end

        val doublePow5InvSplit = Vector.tabulate(invTableSize, createInvSplit)
        and doublePow5Split = Vector.tabulate(posTableSize, createSplit)

        (* The original C code uses 128-bit arithmetic here.  We don't have
           that so this uses arbitrary precision arithmetic.  The result will
           usually (always?) be short on 64-bit platforms.  *)
        fun mulShift(m, factor, shift: int) = IntInf.~>>(factor*m, Word.fromInt shift)
    in
        fun mulPow5InvDivPow2(m, i, j) = mulShift(m, Vector.sub(doublePow5InvSplit, i), j)
        and mulPow5DivPow2(m, i, j) = mulShift(m, Vector.sub(doublePow5Split, i), j)

        val doublePow5InvBitCount = pow5InvBitCount
        and doublePow5BitCount = pow5BitCount
    end

    (* Apart from the first step the remainder is common. *)
    fun computeDecimal(e2: int, m2: LargeInt.int, mmShift, acceptBounds) =
    let
        (* Step 2: Determine the interval of valid decimal representations *)
        val mm = 4 * m2 - 1 - mmShift
        val mv = 4 * m2
        val mp = 4 * m2 + 2

        (* Step 3: Convert to a decimal power base *)
        val (e10, vr, vp, vm, lastRemovedDigit, vrIsTrailingZeros, vmIsTrailingZeros) =
            if e2 >= 0
            then
            let
                val q = log10Pow2 e2
                val e10 = q
                val k = doublePow5InvBitCount + pow5bits q - 1
                val i = ~e2 + q + k
                val vr = mulPow5InvDivPow2(mv, q, i)
                and vp = mulPow5InvDivPow2(mp, q, i)
                and vm = mulPow5InvDivPow2(mm, q, i)
            in
                if q > 21
                then (e10, vr, vp, vm, 0, false, false) (* Too large to be power of 5. *)
                else if LargeInt.rem(mv, 5) = 0
                then (e10, vr, vp, vm, 0, multipleOfPow5(mv, q), false)
                else if acceptBounds
                then (e10, vr, vp, vm, 0, false, multipleOfPow5(mm, q))
                else (e10, vr, vp - (if multipleOfPow5(mp, q) then 1 else 0), vm, 0, false, false)
            end
            else
            let
                val q = log10Pow5(~ e2)
                val e10 = q + e2
                val i = ~e2 - q
                val k = pow5bits i - doublePow5BitCount
                val j = q - k
                val vr = mulPow5DivPow2(mv, i, j)
                and vp = mulPow5DivPow2(mp, i, j)
                and vm = mulPow5DivPow2(mm, i, j)
                val lastRemovedDigit =
                    if q <> 0 andalso LargeInt.quot(vp-1, 10) <= LargeInt.quot(vm, 10)
                    then
                    let
                        val j' = q-1-(pow5bits(i+1)-doublePow5BitCount)
                        val lrm = LargeInt.rem(mulPow5DivPow2(mv, i+1, j'), 10)
                    in
                        lrm
                    end
                    else 0
            in
                if q <= 1
                then if acceptBounds
                then (e10, vr, vp, vm, lastRemovedDigit, true, mmShift = 1)
                else (e10, vr, vp-1, vm, lastRemovedDigit, true, false)
                else if q < 31
                then (e10, vr, vp, vm, lastRemovedDigit, multipleOfPowerOf2(mv, q-1), false)
                else (e10, vr, vp, vm, lastRemovedDigit, false, false)
            end

        (* Step 4: Find the shortest decimal representation in the interval *)
        val (output, removed) =
            if vmIsTrailingZeros orelse vrIsTrailingZeros
            then
            let
                fun removeVrDigits(vr, vp, vm, removed, lastRemovedDigit, vmIsTrailingZeros, vrIsTrailingZeros) =
                let
                    val vpDiv10 = LargeInt.quot(vp, 10)
                    and vmDiv10 = LargeInt.quot(vm, 10)
                in
                    if vpDiv10 > vmDiv10
                    then removeVrDigits(LargeInt.quot(vr, 10), vpDiv10, vmDiv10, removed+1, LargeInt.rem(vr, 10),
                                      vmIsTrailingZeros andalso LargeInt.rem(vm, 10) = 0,
                                      vrIsTrailingZeros andalso lastRemovedDigit = 0)
                    else removeVmDigits(vr, vp, vm, removed, lastRemovedDigit, vmIsTrailingZeros, vrIsTrailingZeros)
                end

                and removeVmDigits(vr, vp, vm, removed, lastRemovedDigit, vmIsTrailingZeros, vrIsTrailingZeros) =
                let
                in
                    if vmIsTrailingZeros andalso LargeInt.rem(vm, 10) = 0
                    then removeVmDigits(LargeInt.quot(vr, 10), LargeInt.quot(vp, 10), LargeInt.quot(vm, 10), removed+1, LargeInt.rem(vr, 10),
                                       vmIsTrailingZeros, vrIsTrailingZeros andalso lastRemovedDigit = 0)
                    else
                    let
                        val lastRemovedDigit2 =
                            if vrIsTrailingZeros andalso lastRemovedDigit = 5 andalso LargeInt.rem(vr, 2) = 0
                            then 4 (* Don't round up  *)
                            else lastRemovedDigit
                        val vrCorrect =
                            (vr = vm andalso (not acceptBounds orelse not vmIsTrailingZeros)) orelse lastRemovedDigit2 >= 5
                    in
                        (vr + (if vrCorrect then 1 else 0), removed)
                    end
                end
            in
                removeVrDigits(vr, vp, vm, 0, lastRemovedDigit, vmIsTrailingZeros, vrIsTrailingZeros)
            end
            else
            let
                fun removeDigits(vr, vp, vm, removed, lastRemovedDigit) =
                let
                    val vpDiv10 = LargeInt.quot(vp, 10)
                    and vmDiv10 = LargeInt.quot(vm, 10)
                in
                    if vpDiv10 > vmDiv10
                    then removeDigits(LargeInt.quot(vr, 10), vpDiv10, vmDiv10, removed+1, LargeInt.rem(vr, 10))
                    else (vr + (if vr = vm orelse lastRemovedDigit >= 5 then 1 else 0), removed)
                end
            in
                removeDigits(vr, vp, vm, 0, lastRemovedDigit)
            end

    in
        {mantissa=output, exponent=e10+removed}
    end

    val doubleBias = 1023 (* This is the exponent value for 1.0 *)
    val doubleMantissaBits = 53 - 1 (* One bit is implicit *)
    val doubleImplicitBit = IntInf.<<(1, Word.fromInt(Int.toInt doubleMantissaBits))

    val floatBias = 127 (* This is the exponent value for 1.0 *)
    val floatMantissaBits = 24 - 1 (* One bit is implicit *)
    val floatImplicitBit = Int.fromLarge(IntInf.<<(1, Word.fromInt(Int.toInt floatMantissaBits)))
    
    fun doubleToMinimal(r: real) =
    let
        open RealNumbersAsBits IEEEReal
        val ieeeSign = doubleSignBit r
        and ieeeExponent = doubleExponent r
        and ieeeMantissa = doubleMantissa r
    in
        if ieeeExponent = 2047
        then  (* Non-finite *)
            {sign=ieeeSign, exponent=0, mantissa=0, class=if ieeeMantissa = 0 then INF else NAN}
        else if ieeeExponent = 0 andalso ieeeMantissa = 0
        then {sign=ieeeSign, exponent=0, mantissa=0, class=ZERO}
        else
        let
            (* Step 1: Normalise the value.  Normalised values, with exponent non-zero,
               have an implicit one in the top bit position. *)
            val (e2, m2) =
                if ieeeExponent = 0
                then (1-doubleBias-doubleMantissaBits-2, ieeeMantissa)
                else (ieeeExponent-doubleBias-doubleMantissaBits-2, ieeeMantissa + doubleImplicitBit)

            val acceptBounds = LargeInt.rem(m2, 2) = 0

            val mmShift =
                if ieeeMantissa <> 0 orelse ieeeExponent <= 1 then 1 else 0
            val {mantissa, exponent} = computeDecimal(Int.toInt e2, m2, mmShift, acceptBounds)
        in
            {sign=ieeeSign, exponent=exponent, mantissa=mantissa, class=if ieeeExponent = 0 then SUBNORMAL else NORMAL}
        end
    end
    
    and floatToMinimal(f: Real32.real) =
    let
        open RealNumbersAsBits IEEEReal
        val ieeeSign = floatSignBit f
        and ieeeExponent = floatExponent f
        and ieeeMantissa = floatMantissa f
    in
        if ieeeExponent = 255
        then (* Non-finite *)
            {sign=ieeeSign, exponent=0, mantissa=0, class=if ieeeMantissa = 0 then INF else NAN}
        else if ieeeExponent = 0 andalso ieeeMantissa = 0
        then {sign=ieeeSign, exponent=0, mantissa=0, class=ZERO}
        else
        let
            (* Step 1: Normalise the value.  Normalised values, with exponent non-zero,
               have an implicit one in the top bit position. *)
            val (e2, m2) =
                if ieeeExponent = 0
                then (1-floatBias-floatMantissaBits-2, ieeeMantissa)
                else (ieeeExponent-floatBias-floatMantissaBits-2, ieeeMantissa + floatImplicitBit)

            val isEven = Int.rem(m2, 2) = 0
            val acceptBounds = isEven

            (* Step 2: Determine the interval of valid decimal representations (??) *)
            val mmShift = if ieeeMantissa <> 0 orelse ieeeExponent <= 1 then 1 else 0
            
            val {mantissa, exponent} = computeDecimal(Int.toInt e2, Int.toLarge m2, mmShift, acceptBounds)
        in
            {sign=ieeeSign, exponent=exponent, mantissa=mantissa, class=if ieeeExponent = 0 then SUBNORMAL else NORMAL}
        end

    end

    val floatPosInf  = RealNumbersAsBits.floatFromBinary{sign=false, exp=255, mantissa = 0}
    and floatNegInf  = RealNumbersAsBits.floatFromBinary{sign=true, exp=255, mantissa = 0}
    and floatPosZero = RealNumbersAsBits.floatFromBinary{sign=false, exp=0, mantissa = 0} (* We haven't defined constants yet. *)
    and floatNegZero = RealNumbersAsBits.floatFromBinary{sign=true, exp=0, mantissa = 0}

    fun decimalToFloat (sign, e10, digits) =
    let
        (* At most 8 digits are significant.  When we encounter the 9th we use it for rounding. *)
        fun digitsToMantissa([], acc, nDigs) = (acc, nDigs)
        |   digitsToMantissa(d :: _, acc, 9) =
                (if d >= 5 then acc+1 else acc, 9)
        |   digitsToMantissa(d :: l, acc, nDigs) =
                if d < 0 orelse d > 9 then raise General.Domain
                else digitsToMantissa(l, acc*10 + Int.fromInt d, nDigs+1)
        val (m10, m10digits) = digitsToMantissa(digits, 0, 0)
    in
        if e10 <= ~46 orelse m10 = 0 (* Number is less than 1e~46. Return +/- 0.0 *)
        then if sign then floatNegZero else floatPosZero
        else if e10 >= 40
        then if sign then floatNegInf else floatPosInf
        else
        let
            val e10 = e10-m10digits
            val m10L = Int.toLarge m10
            val (e2, m2, trailingZeros) =
                if e10 >= 0
                then
                let
                    val l2p5 = log2Pow5 e10
                    val e2 = IntInf.log2 m10L + e10 + l2p5 - (floatMantissaBits + 1)
                    val j = e2 - e10 - l2p5 - 1 + doublePow5BitCount
                    val m2 = mulPow5DivPow2(m10L, e10, j)
                    val trailingZeros = e2 < e10 orelse e2 - e10 < 32 andalso multipleOfPowerOf2(m10L, e2 - e10)
                in
                    (e2, m2, trailingZeros)
                end
                else
                let
                    val cl2p5 = log2Pow5(~e10)+1
                    val e2 = IntInf.log2 m10L + e10 - cl2p5 - (floatMantissaBits + 1)
                    val j = e2 - e10 + cl2p5 - 1 + doublePow5InvBitCount
                    val m2 = mulPow5InvDivPow2(m10L, ~e10, j)
                    val trailingZeros =
                        (e2 < e10 orelse e2 - e10 < 32 andalso multipleOfPowerOf2(m10L, e2 - e10)) andalso
                            multipleOfPow5(m10L, ~e10)
                in
                    (e2, m2, trailingZeros)
                end
            val ieeeExp = Int.max(0, e2+floatBias+IntInf.log2 m2)
            (* The shift should put the top bit into the implicit bit position
               so it is then masked out except when the exponent is zero
               when we have a denormal number. *)
            val shift =
                (if ieeeExp = 0 then 1 else ieeeExp) - e2 - floatBias - floatMantissaBits
            val trailingZeros = trailingZeros andalso
                    Word.andb(Word.fromLargeInt m2, Word.<<(0w1, Word.fromInt shift - 0w1) - 0w1) = 0w0
            val lastRemovedBit = Word.andb(Word.>>(Word.fromLargeInt m2, Word.fromInt shift - 0w1), 0w1)
            val roundup =
                if lastRemovedBit <> 0w0 andalso (not trailingZeros orelse
                    Word.andb(Word.>>(Word.fromLargeInt m2, Word.fromInt shift), 0w1) <> 0w0)
                then 0w1 else 0w0
            val ieeeM2 = Word.toInt(Word.andb(Word.fromInt(floatImplicitBit-1), Word.>>(Word.fromLargeInt m2, Word.fromInt shift) + roundup))
        in
            if ieeeExp > 254 (* Exponent is too large - return infinity *)
            then if sign then floatNegInf else floatPosInf
            else RealNumbersAsBits.floatFromBinary{sign=sign, exp=ieeeExp, mantissa=ieeeM2}
        end
    end

    val doublePosInf  = RealNumbersAsBits.doubleFromBinary{sign=false, exp=2047, mantissa = 0}
    and doubleNegInf  = RealNumbersAsBits.doubleFromBinary{sign=true, exp=2047, mantissa = 0}
    and doublePosZero = RealNumbersAsBits.doubleFromBinary{sign=false, exp=0, mantissa = 0}
    and doubleNegZero = RealNumbersAsBits.doubleFromBinary{sign=true, exp=0, mantissa = 0}

    fun decimalToDouble (sign, e10, digits) =
    let
        (* At most 16 digits are significant.  We need to limit the mantissa to this
           number of digits to avoid a potential subscript exception when accessing the
           pre-built tables.  When we encounter the 17th we use it for rounding. *)
        fun digitsToMantissa([], acc: LargeInt.int, nDigs) = (acc, nDigs)
        |   digitsToMantissa(d :: _, acc, 17) =
                (if d >= 5 then acc+1 else acc, 17)
        |   digitsToMantissa(d :: l, acc, nDigs) =
                if d < 0 orelse d > 9 then raise General.Domain
                else digitsToMantissa(l, acc*10 + LargeInt.fromInt d, nDigs+1)
        val (m10, m10digits) = digitsToMantissa(digits, 0, 0)
    in
        if e10 <= ~324 orelse m10 = 0 (* Number is less than 1e~324. Return +/- 0.0 *)
        then if sign then doubleNegZero else doublePosZero
        else if e10 >= 310
        then if sign then doubleNegInf else doublePosInf
        else
        let
            val e10 = e10-m10digits
            val m10L = m10
            val (e2, m2, trailingZeros) =
                if e10 >= 0
                then
                let
                    val l2p5 = log2Pow5 e10
                    val e2 = IntInf.log2 m10L + e10 + l2p5 - (doubleMantissaBits + 1)
                    val j = e2 - e10 - l2p5 - 1 + doublePow5BitCount
                    val m2 = mulPow5DivPow2(m10L, e10, j)
                    val trailingZeros = e2 < e10 orelse e2 - e10 < 32 andalso multipleOfPowerOf2(m10L, e2 - e10)
                in
                    (e2, m2, trailingZeros)
                end
                else
                let
                    val cl2p5 = log2Pow5(~e10)+1
                    val e2 = IntInf.log2 m10L + e10 - cl2p5 - (doubleMantissaBits + 1)
                    val j = e2 - e10 + cl2p5 - 1 + doublePow5InvBitCount
                    val m2 = mulPow5InvDivPow2(m10L, ~e10, j)
                    val trailingZeros =
                        (e2 < e10 orelse e2 - e10 < 32 andalso multipleOfPowerOf2(m10L, e2 - e10)) andalso
                            multipleOfPow5(m10L, ~e10)
                in
                    (e2, m2, trailingZeros)
                end
            val ieeeExp = Int.max(0, e2 + doubleBias + IntInf.log2 m2)
            (* The shift should put the top bit into the implicit bit position
               so it is then masked out except when the exponent is zero
               when we have a denormal number. *)
            val shift =
                (if ieeeExp = 0 then 1 else ieeeExp) - e2 - doubleBias - doubleMantissaBits
            val trailingZeros = trailingZeros andalso
                    IntInf.andb(m2, IntInf.<<(1, Word.fromInt shift - 0w1) - 1) = 0
            val lastRemovedBit = IntInf.andb(IntInf.~>>(m2, Word.fromInt shift - 0w1), 1)
            val roundup =
                if lastRemovedBit <> 0 andalso (not trailingZeros orelse
                    IntInf.andb(IntInf.~>>(m2, Word.fromInt shift), 1) <> 0)
                then 1 else 0
            val ieeeM2 = IntInf.andb(doubleImplicitBit-1, IntInf.~>>(m2, Word.fromInt shift) + roundup)
        in
            if ieeeExp > 2046 (* Exponent is too large - return infinity *)
            then if sign then doubleNegInf else doublePosInf
            else RealNumbersAsBits.doubleFromBinary{sign=sign, exp=Int.fromInt ieeeExp, mantissa=ieeeM2}
        end
    end


end;
