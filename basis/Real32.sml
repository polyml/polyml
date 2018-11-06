(*
    Title:      Real32 structure.
    Author:     David Matthews
    Copyright   David Matthews 2018

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

(*
    This structure implements 32-bit real values, at least on X86.  On other
    platforms it is whatever "float" is.
    N.B.  This uses the X87 floating point instructions on X86/32.  The precision
    on the X87 is set to 64-bits which is correct for the Real.real operations
    but involves an extra stage of rounding for Real32.real.  That means that
    the results may not be strictly accurate.
*)

structure Real32: REAL where type real = Real32.real =
struct
    open Real32 (* Inherit the type and the built-in functions. *)
    open IEEEReal

    fun fromLarge IEEEReal.TO_NEAREST = fromRealRound
    |   fromLarge IEEEReal.TO_ZERO = fromRealTrunc
    |   fromLarge IEEEReal.TO_POSINF = fromRealCeil
    |   fromLarge IEEEReal.TO_NEGINF = fromRealFloor

    (* Defined to use the current rounding mode. *)
    val fromInt = fromReal o Real.fromInt (* TODO *)
    and fromLargeInt = fromReal o Real.fromLargeInt
    
    val zero = fromInt 0 and one = fromInt 1 and four = fromInt 4

    local
        (* The General call is now only used to get constants. *)
        val doFloatFloat : int*unit->real = RunCall.rtsCallFull2 "PolyRealGeneral"
        and doFloatInt  : int*unit->int = RunCall.rtsCallFull2 "PolyRealGeneral"
        fun callFloat n x = doFloatFloat(n, x)
        and callFloatToInt n x = doFloatInt(n, x)
    in
        val radix : int = callFloatToInt 30 ()
        val precision : int = callFloatToInt 31 ()
        val maxFinite : real = callFloat 32 ()
        val minNormalPos : real = callFloat 33 ()
        val minPos : real = callFloat 34()
    end

    val posInf : real = one/zero;
    val negInf : real = ~one/zero;

    infix 4 == != ?=;
    
    val op != : real * real -> bool = not o op ==

    local
    in
        (* NAN values fail any test including equality with themselves. *)
        fun isNan x = x != x
        (* NAN values do not match and infinities when multiplied by 0 produce NAN. *)
        fun isFinite x = x * zero == zero
    
        val copySign : (real * real) -> real = rtsCallFastFF_F "PolyRealFCopySign"
        
        (* Get the sign bit by copying the sign onto a finite value and then
           testing.  This works for non-finite values and zeros. *)
        fun signBit r = copySign(one, r) < zero

        (* If we assume that all functions produce normalised results where
           possible, the only subnormal values will be those smaller than
           minNormalPos. *)
        fun isNormal x = isFinite x andalso abs x >= minNormalPos
    
        fun class x =
            if isFinite x then if x == zero then ZERO
               else if abs x >= minNormalPos then NORMAL
               else SUBNORMAL
            else if isNan x then NAN
               else (* not finite and not Nan *) INF
    
        fun sign x = 
            if isNan x then raise General.Domain
            else if x == zero then 0 else if x < zero then ~1 else 1
    end
        
    fun sameSign (x, y) = signBit x = signBit y
    and unordered (x, y) = isNan x orelse isNan y

    (* Returns the minimum.  In the case where one is a NaN it returns the
       other. In that case the comparison will be false. *)
    fun min (a: real, b: real): real = if a < b orelse isNan b then a else b
    (* Similarly for max. *)
    fun max (a: real, b: real): real = if a > b orelse isNan b then a else b

    fun checkFloat x =
        if isFinite x then x
        else if isNan x then raise General.Div else raise General.Overflow

    (* Use the Real versions for the moment. *)
    fun toManExp r =
        let
            val {man, exp} = Real.toManExp(toLarge r)
        in
            {man = fromRealRound man, exp = exp }
        end

    and fromManExp {man, exp} = fromRealRound(Real.fromManExp{man=toLarge man, exp=exp})
    
    fun compare (r1, r2) =
        if r1 == r2 then General.EQUAL
        else if r1 < r2 then General.LESS
        else if r1 > r2 then General.GREATER
        else raise Unordered

    fun compareReal (r1, r2) =
        if r1 == r2 then EQUAL
        else if r1 < r2 then LESS
        else if r1 > r2 then GREATER
        else UNORDERED

    (* Question: The definition says "bitwise equal, ignoring signs on zeros".
       If we assume that all numbers are normalised, is that the same as "equal"?*)
    fun op ?= (x, y) =
        isNan x orelse isNan y orelse x == y

    (* Although these may be built in in some architectures it's
       probably not worth treating them specially at the moment. *)
    fun *+ (x: real, y: real, z: real): real = x*y+z
    and *- (x: real, y: real, z: real): real = x*y-z

    val realFloor = rtsCallFastF_F "PolyRealFFloor"
    and realCeil  = rtsCallFastF_F "PolyRealFCeil"
    and realTrunc  = rtsCallFastF_F "PolyRealFTrunc"
    and realRound  = rtsCallFastF_F "PolyRealFRound"

    val rem = rtsCallFastFF_F "PolyRealFRem"

    (* Split a real into whole and fractional parts. The fractional part must have
       the same sign as the number even if it is zero. *)
    fun split r =
    let
        val whole = realTrunc r
        val frac = r - whole
    in
        { whole = whole,
          frac =
            if not (isFinite r)
            then if isNan r then r else (* Infinity *) if r < zero then ~zero else zero
            else if frac == zero then if signBit r then ~zero else zero
            else frac }
    end

    (* Get the fractional part of a real. *)
    fun realMod r = #frac(split r)
    
    val nextAfter = rtsCallFastFF_F "PolyRealFNextAfter"
    
    fun toLargeInt mode r = Real.toLargeInt mode (toLarge r)

    local
        (* These are defined to raise Domain rather than Overflow on Nans. *)
        fun checkNan x = if isNan x then raise Domain else x
        (* If int is fixed we use the hardware conversions otherwise we convert
           it to real and use the real to arbitrary conversions. *)
    in
        val floor   =
            if Bootstrap.intIsArbitraryPrecision
            then LargeInt.toInt o toLargeInt IEEEReal.TO_NEGINF else FixedInt.toInt o floorFix o checkNan
        and ceil    =
            if Bootstrap.intIsArbitraryPrecision
            then LargeInt.toInt o toLargeInt IEEEReal.TO_POSINF else FixedInt.toInt o ceilFix o checkNan
        and trunc   =
            if Bootstrap.intIsArbitraryPrecision
            then LargeInt.toInt o toLargeInt IEEEReal.TO_ZERO else FixedInt.toInt o truncFix o checkNan
        and round   =
            if Bootstrap.intIsArbitraryPrecision
            then LargeInt.toInt o toLargeInt IEEEReal.TO_NEAREST else FixedInt.toInt o roundFix o checkNan
    
        fun toInt IEEEReal.TO_NEGINF = floor
         |  toInt IEEEReal.TO_POSINF = ceil
         |  toInt IEEEReal.TO_ZERO = trunc
         |  toInt IEEEReal.TO_NEAREST = round
    end

    fun fmt fm r = Real.fmt fm (toLarge r)
    
    val toString = Real.toString o toLarge
    
    (* Scan input source for a valid number.  The format is the same as
       for double precision.  Convert it using the current rounding mode. *)
    fun scan getc src =
        case Real.scan getc src of
            NONE => NONE
        |   SOME (r, a) => SOME(fromReal r, a)

    val fromString = StringCvt.scanString scan

    val toDecimal = Real.toDecimal o toLarge
    
    (* Convert from decimal.  This is defined to use TO_NEAREST. *)
    val fromDecimal = Option.map fromRealRound o Real.fromDecimal

    structure Math =
    struct
        type real = real

        val sqrt  = rtsCallFastF_F "PolyRealFSqrt"
        and sin   = rtsCallFastF_F "PolyRealFSin"
        and cos   = rtsCallFastF_F "PolyRealFCos"
        and atan  = rtsCallFastF_F "PolyRealFArctan"
        and exp   = rtsCallFastF_F "PolyRealFExp"
        and ln    = rtsCallFastF_F "PolyRealFLog"
        and tan   = rtsCallFastF_F "PolyRealFTan"
        and asin  = rtsCallFastF_F "PolyRealFArcSin"
        and acos  = rtsCallFastF_F "PolyRealFArcCos"
        and log10 = rtsCallFastF_F "PolyRealFLog10"
        and sinh  = rtsCallFastF_F "PolyRealFSinh"
        and cosh  = rtsCallFastF_F "PolyRealFCosh"
        and tanh  = rtsCallFastF_F "PolyRealFTanh"

        val atan2 = rtsCallFastFF_F "PolyRealFAtan2"
        val pow = rtsCallFastFF_F "PolyRealFPow"

        (* Derived values. *)
        val e = exp one
        val pi = four * atan one
    end
    

    (* Converter for literal constants.  Copied from Real. *)
    local
        fun convReal (s: string) : real =
        let
            (* Set the rounding mode to TO_NEAREST whatever the current
               rounding mode.  Otherwise the result of compiling a piece of
               code with a literal constant could depend on what the rounding
               mode was set to. We should always support TO_NEAREST. *)
            val oldRounding = IEEEReal.getRoundingMode()
            val () = IEEEReal.setRoundingMode IEEEReal.TO_NEAREST
            val scanResult = StringCvt.scanString scan s
            val () = IEEEReal.setRoundingMode oldRounding
        in
            case scanResult of
                NONE => raise RunCall.Conversion "Invalid real constant"
              | SOME res => res
        end
    in
        (* Install this as a conversion function for real literals. *)
        val (): unit = RunCall.addOverload convReal "convReal"
    end
   
end;


val () = RunCall.addOverload Real32.>= ">="
and () = RunCall.addOverload Real32.<= "<="
and () = RunCall.addOverload Real32.>  ">"
and () = RunCall.addOverload Real32.<  "<"
and () = RunCall.addOverload Real32.+ "+"
and () = RunCall.addOverload Real32.- "-"
and () = RunCall.addOverload Real32.* "*"
and () = RunCall.addOverload Real32.~ "~"
and () = RunCall.addOverload Real32.abs "abs"
and () = RunCall.addOverload Real32./ "/";


(* Install print function. *)
local
    fun print_real _ _ (r: Real32.real) =
        PolyML.PrettyString(Real32.fmt (StringCvt.GEN(SOME 10)) r)
in
    val () = PolyML.addPrettyPrinter print_real;
end;
