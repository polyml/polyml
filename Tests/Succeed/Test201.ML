(* This version of the Ryu floating point conversion contained a
   code-generator bug when compiled on X86/32. *)


val floatBias = 127 (* This is the exponent value for 1.0 *)
val floatExponentBits = 8
val floatMantissaBits = Real32.precision - 1 (* One bit is implicit *)
val floatImplicitBit = FixedInt.fromInt(Word.toInt(Word.<<(0w1, Word.fromInt floatMantissaBits)))

(* Returns floor(log10(2^e)) for values of e between 0 and 1650. *)
fun log10Pow2 e =
    if e < 0 orelse e > 1650 then raise General.Domain
    else Int.quot(e * 78913, 0x40000) (* >> 18 *)
(* Returns floor(log10(5^e)) for values of e between 0 and 2620 *)
and log10Pow5 e =
    if e < 0 orelse e > 2620 then raise General.Domain
    else Int.quot(e * 732923, 0x100000) (* >> 20 *)

fun pow5bits e =
    if e < 0 orelse e > 3528 then raise General.Domain
    else Int.quot(e * 1217359, 0x80000) (* >> 19 *) + 1

local
    (* Keep dividing by 5 while the remainder is zero *)
    fun p5 count value =
        if Int.rem(value, 5) <> 0
        then count
        else p5 (count+1) (Int.quot(value, 5))
in
    (* Returns whether value is divisible by 5 to the power p. *)
    fun multipleOfPow5(value, e5) =
        p5 0 value >= e5
end

fun multipleOfPowerOf2(value, p) =
    Word.andb(Word.fromInt value, Word.<<(0w1, Word.fromInt p) - 0w1) = 0w0;

local
    val posTableSize = 47 and invTableSize = 55
    val pow5BitCount = 61 and pow5InvBitCount = 59

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

    val floatPow5InvSplit = Vector.tabulate(invTableSize, createInvSplit)
    and floatPow5Split = Vector.tabulate(posTableSize, createSplit)

    (* We don't have 64-bit arithmetic on 32-bit platforms so this uses arbitrary precision
       arithmetic.  It might be possible to select different versions depending on the
       word length.
       The Java version uses two tables of 31 bit values which would be an
       alternative. *)
    fun mulShift32(m: int, factor, shift: int): int =
        if shift <= 32 then raise Fail "mulShift32"
        else LargeInt.toInt(IntInf.~>>(factor*LargeInt.fromInt m, Word.fromInt shift))
in
    fun mulPow5InvDivPow2(m, q, j) = mulShift32(m, Vector.sub(floatPow5InvSplit, q), j)
    and mulPow5DivPow2(m, i, j) = mulShift32(m, Vector.sub(floatPow5Split, i), j)

    val floatPow5InvBitCount = pow5InvBitCount
    and floatPow5BitCount = pow5BitCount
end;

fun f2d(ieeeMantissa, ieeeExponent) =
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
    (* Presumably this is 4* because we've subtracted 2 from e2. *)
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
            val k = floatPow5InvBitCount + pow5bits q - 1
            val i = ~e2 + q + k
            val vr = mulPow5InvDivPow2(mv, q, i)
            and vp = mulPow5InvDivPow2(mp, q, i)
            and vm = mulPow5InvDivPow2(mm, q, i)
        in
            if q > 9
            then (e10, vr, vp, vm, 0, false, false) (* Too large to be power of 5. *)
            else if Int.rem(mv, 5) = 0
            then (e10, vr, vp, vm, 0, multipleOfPow5(mv, q), false)
            else if acceptBounds
            then (e10, vr, vp, vm, 0, false, multipleOfPow5(mm, q))
            else (e10, vr, vp - (if multipleOfPow5(mp, q) then 1 else 0), vm, 0, false, false)
        end
        else raise Bind

    (* Step 4: Find the shortest decimal representation in the interval *)
    val (output, removed) =
        if vmIsTrailingZeros orelse vrIsTrailingZeros
        then
        let
            fun removeVrDigits(vr, vp, vm, removed, lastRemovedDigit, vmIsTrailingZeros, vrIsTrailingZeros) =
            let
                val vpDiv10 = Int.quot(vp, 10)
                val vmDiv10 = Int.quot(vm, 10)
            in
                if vpDiv10 > vmDiv10
                then removeVrDigits(Int.quot(vr, 10), vpDiv10, vmDiv10, removed+1, Int.rem(vr, 10),
                                  vmIsTrailingZeros andalso Int.rem(vm, 10) = 0,
                                  vrIsTrailingZeros andalso lastRemovedDigit = 0)
                else removeVmDigits(vr, vp, vm, removed, lastRemovedDigit, vmIsTrailingZeros, vrIsTrailingZeros)
            end

            and removeVmDigits(vr, vp, vm, removed, lastRemovedDigit, vmIsTrailingZeros, vrIsTrailingZeros) =
            let
            in
                if vmIsTrailingZeros andalso Int.rem(vm, 10) = 0
                then removeVmDigits(Int.quot(vr, 10), Int.quot(vp, 10), Int.quot(vm, 10), removed+1, Int.rem(vr, 10),
                                   vmIsTrailingZeros, vrIsTrailingZeros andalso lastRemovedDigit = 0)
                else
                let
                    val lastRemovedDigit2 =
                        if vrIsTrailingZeros andalso lastRemovedDigit = 5 andalso Int.rem(vr, 2) = 0
                        then 4 (* ??? *)
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
                val vpDiv10 = Int.quot(vp, 10)
                and vmDiv10 = Int.quot(vm, 10)
            in
                if vpDiv10 > vmDiv10
                then removeDigits(Int.quot(vr, 10), vpDiv10, vmDiv10, removed+1, Int.rem(vr, 10))
                else (vr + (if vr = vm orelse lastRemovedDigit >= 5 then 1 else 0), removed)
            end
        in
            removeDigits(vr, vp, vm, 0, lastRemovedDigit)
        end
in
    {mantissa=output, e10=e10, removed=removed}
end
;
val result = f2d (7221787, 155);

if #removed result = #mantissa result then raise Fail "It's a bug" else ();
