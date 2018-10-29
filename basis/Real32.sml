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
    open Real32
    open IEEEReal

    val fromInt = fromReal o Real.fromInt (* TODO *)
    and fromLargeInt = fromReal o Real.fromLargeInt
    
    val zero = fromInt 0 and one = fromInt 1 and four = fromInt 4
    
    val radix = Real.radix (* TODO *)
    and precision = Real.precision (* TODO *)
    
    val maxFinite = fromReal Real.maxFinite (* TODO *)
    and minPos = fromReal Real.minPos (* TODO *)
    and minNormalPos = fromReal Real.minNormalPos (* TODO *)

    fun unary f = fromReal o f o toLarge
    
    and binary f (x, y) = fromReal(f(toLarge x, toLarge y))
    
    and binary2 f (x, y) = f(toLarge x, toLarge y)
    
    val op + = binary Real.+ (* TODO *)
    and op - = binary Real.- (* TODO *)
    and op * = binary Real.* (* TODO *)
    and op / = binary Real./ (* TODO *)

    val ~ = unary Real.~ (* TODO *)
    and abs = unary Real.abs (* TODO *)

    val posInf : real = one/zero;
    val negInf : real = ~one/zero;
    
    val op < = binary2 Real.<
    and op <= = binary2 Real.<=
    and op > = binary2 Real.>
    and op >= = binary2 Real.>=
    and op == = binary2 Real.==

    and op ?= = binary2 Real.?=

    infix 4 == != ?=;
    
    val op != : real * real -> bool = not o op ==

    local
    in
        (* NAN values fail any test including equality with themselves. *)
        fun isNan x = x != x
        (* NAN values do not match and infinities when multiplied by 0 produce NAN. *)
        fun isFinite x = x * zero == zero
    
        val copySign : (real * real) -> real = binary Real.copySign (* TODO *)
        
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

    fun toManExp r =
        let
            val {man, exp} = Real.toManExp(toLarge r)
        in
            {man = fromReal man, exp = exp }
        end

    and fromManExp {man, exp} = fromReal(Real.fromManExp{man=toLarge man, exp=exp})
    
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

    (* Question: The definition says "bitwise equal, ignoring signs on zeros".
       If we assume that all numbers are normalised, is that the same as "equal"?*)
    fun op ?= (x, y) =
        isNan x orelse isNan y orelse x == y

    (* Although these may be built in in some architectures it's
       probably not worth treating them specially at the moment. *)
    fun *+ (x: real, y: real, z: real): real = x*y+z
    and *- (x: real, y: real, z: real): real = x*y-z

    fun rem (x, y) =
        if not (isFinite y) andalso not (isNan y) then x
        else x - realTrunc(x / y)*y

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
    
    val nextAfter = binary Real.nextAfter (* TODO: Using Real.nextAfter here won't work. *)

    val realFloor = unary Real.realFloor
    and realCeil = unary Real.realCeil
    and realTrunc = unary Real.realTrunc
    and realRound = unary Real.realRound

    val floor = Real.floor o toLarge
    and ceil = Real.ceil o toLarge
    and trunc = Real.trunc o toLarge
    and round = Real.round o toLarge
    
    fun toInt mode r = Real.toInt mode (toLarge r)
    and toLargeInt mode r = Real.toLargeInt mode (toLarge r)

    (* fromReal converts using the current rounding mode.  We need
       to set the rounding mode and then restore the original. *)
    fun fromLarge mode r =
    let
        open IEEEReal
        val original = getRoundingMode()
        val () = setRoundingMode mode
    in
        fromReal r before (setRoundingMode original)
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
    
    val fromDecimal = Option.map fromReal o Real.fromDecimal

    structure Math =
    struct
        type real = real
       
        val sqrt = unary Real.Math.sqrt (* TODO *)
        and sin = unary Real.Math.sin
        and cos = unary Real.Math.cos
        and tan = unary Real.Math.tan
        and asin = unary Real.Math.asin
        and acos = unary Real.Math.acos
        and atan = unary Real.Math.atan
        and atan2 = binary Real.Math.atan2
        and exp = unary Real.Math.exp
        and pow = binary Real.Math.pow
        and ln = unary Real.Math.ln
        and log10 = unary Real.Math.log10
        and sinh = unary Real.Math.sinh
        and cosh = unary Real.Math.cosh
        and tanh = unary Real.Math.tanh

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
