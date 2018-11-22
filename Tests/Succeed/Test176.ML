(* The simplifier converts if X then 0 else 1
   into X, assuming that these are false and true.
   These have to be handled in the optimiser.
   Some are always true or always false. *)

val a = ref 0;
val b = ref 1;

fun check a = if a then () else raise Fail "wrong";

(* = *)
local
    val r = if !a = 0 then 0 else 1
in
    val x = if r = 0 then "yes" else "no"
end;
check(x = "yes");

local
    val r = if !b = 0 then 0 else 1
in
    val x = if r = 0 then "yes" else "no"
end;
check(x = "no");

local
    val r = if !a = 0 then 0 else 1
in
    val x = if r = 1 then "yes" else "no"
end;
check(x = "no");

local
    val r = if !b = 0 then 0 else 1
in
    val x = if r = 1 then "yes" else "no"
end;
check(x = "yes");

(* <> *)
local
    val r = if !a = 0 then 0 else 1
in
    val x = if r <> 0 then "yes" else "no"
end;
check(x = "no");

local
    val r = if !b = 0 then 0 else 1
in
    val x = if r <> 0 then "yes" else "no"
end;
check(x = "yes");

local
    val r = if !a = 0 then 0 else 1
in
    val x = if r <> 1 then "yes" else "no"
end;
check(x = "yes");

local
    val r = if !b = 0 then 0 else 1
in
    val x = if r <> 1 then "yes" else "no"
end;
check(x = "no");

(* > *)
local
    val r = if !a = 0 then 0 else 1
in
    val x = if r > 0 then "yes" else "no"
end;
check(x = "no");

local
    val r = if !b = 0 then 0 else 1
in
    val x = if r > 0 then "yes" else "no"
end;
check(x = "yes");

local
    val r = if !a = 0 then 0 else 1
in
    val x = if r > 1 then "yes" else "no"
end;

check(x = "no");

local
    val r = if !b = 0 then 0 else 1
in
    val x = if r > 1 then "yes" else "no"
end;

check(x = "no");

(* < *)

local
    val r = if !a = 0 then 0 else 1
in
    val x = if r < 0 then "yes" else "no"
end;
check(x = "no");

local
    val r = if !b = 0 then 0 else 1
in
    val x = if r < 0 then "yes" else "no"
end;
check(x = "no");

local
    val r = if !a = 0 then 0 else 1
in
    val x = if r < 1 then "yes" else "no"
end;
check(x = "yes");

local
    val r = if !b = 0 then 0 else 1
in
    val x = if r < 1 then "yes" else "no"
end;
check(x = "no");

(* <= *)

local
    val r = if !a = 0 then 0 else 1
in
    val x = if r <= 0 then "yes" else "no"
end;
check(x = "yes");

local
    val r = if !b = 0 then 0 else 1
in
    val x = if r <= 0 then "yes" else "no"
end;
check(x = "no");

local
    val r = if !a = 0 then 0 else 1
in
    val x = if r <= 1 then "yes" else "no"
end;
check(x = "yes");

local
    val r = if !b = 0 then 0 else 1
in
    val x = if r <= 1 then "yes" else "no"
end;
check(x = "yes");

(* >= *)

local
    val r = if !a = 0 then 0 else 1
in
    val x = if r >= 0 then "yes" else "no"
end;
check(x = "yes");

local
    val r = if !b = 0 then 0 else 1
in
    val x = if r >= 0 then "yes" else "no"
end;
check(x = "yes");

local
    val r = if !a = 0 then 0 else 1
in
    val x = if r >= 1 then "yes" else "no"
end;
check(x = "no");

local
    val r = if !b = 0 then 0 else 1
in
    val x = if r >= 1 then "yes" else "no"
end;
check(x = "yes");

(* Unsigned comparisons. *)

(* = *)
local
    val r = if !a = 0 then 0w0 else 0w1
in
    val x = if r = 0w0 then "yes" else "no"
end;
check(x = "yes");

local
    val r = if !b = 0 then 0w0 else 0w1
in
    val x = if r = 0w0 then "yes" else "no"
end;
check(x = "no");

local
    val r = if !a = 0 then 0w0 else 0w1
in
    val x = if r = 0w1 then "yes" else "no"
end;
check(x = "no");

local
    val r = if !b = 0 then 0w0 else 0w1
in
    val x = if r = 0w1 then "yes" else "no"
end;
check(x = "yes");

(* <> *)
local
    val r = if !a = 0 then 0w0 else 0w1
in
    val x = if r <> 0w0 then "yes" else "no"
end;
check(x = "no");

local
    val r = if !b = 0 then 0w0 else 0w1
in
    val x = if r <> 0w0 then "yes" else "no"
end;
check(x = "yes");

local
    val r = if !a = 0 then 0w0 else 0w1
in
    val x = if r <> 0w1 then "yes" else "no"
end;
check(x = "yes");

local
    val r = if !b = 0 then 0w0 else 0w1
in
    val x = if r <> 0w1 then "yes" else "no"
end;
check(x = "no");

(* > *)
local
    val r = if !a = 0 then 0w0 else 0w1
in
    val x = if r > 0w0 then "yes" else "no"
end;
check(x = "no");

local
    val r = if !b = 0 then 0w0 else 0w1
in
    val x = if r > 0w0 then "yes" else "no"
end;
check(x = "yes");

local
    val r = if !a = 0 then 0w0 else 0w1
in
    val x = if r > 0w1 then "yes" else "no"
end;

check(x = "no");

local
    val r = if !b = 0 then 0w0 else 0w1
in
    val x = if r > 0w1 then "yes" else "no"
end;

check(x = "no");

(* < *)

local
    val r = if !a = 0 then 0w0 else 0w1
in
    val x = if r < 0w0 then "yes" else "no"
end;
check(x = "no");

local
    val r = if !b = 0 then 0w0 else 0w1
in
    val x = if r < 0w0 then "yes" else "no"
end;
check(x = "no");

local
    val r = if !a = 0 then 0w0 else 0w1
in
    val x = if r < 0w1 then "yes" else "no"
end;
check(x = "yes");

local
    val r = if !b = 0 then 0w0 else 0w1
in
    val x = if r < 0w1 then "yes" else "no"
end;
check(x = "no");

(* <= *)

local
    val r = if !a = 0 then 0w0 else 0w1
in
    val x = if r <= 0w0 then "yes" else "no"
end;
check(x = "yes");

local
    val r = if !b = 0 then 0w0 else 0w1
in
    val x = if r <= 0w0 then "yes" else "no"
end;
check(x = "no");

local
    val r = if !a = 0 then 0w0 else 0w1
in
    val x = if r <= 0w1 then "yes" else "no"
end;
check(x = "yes");

local
    val r = if !b = 0 then 0w0 else 0w1
in
    val x = if r <= 0w1 then "yes" else "no"
end;
check(x = "yes");

(* >= *)

local
    val r = if !a = 0 then 0w0 else 0w1
in
    val x = if r >= 0w0 then "yes" else "no"
end;
check(x = "yes");

local
    val r = if !b = 0 then 0w0 else 0w1
in
    val x = if r >= 0w0 then "yes" else "no"
end;
check(x = "yes");

local
    val r = if !a = 0 then 0w0 else 0w1
in
    val x = if r >= 0w1 then "yes" else "no"
end;
check(x = "no");

local
    val r = if !b = 0 then 0w0 else 0w1
in
    val x = if r >= 0w1 then "yes" else "no"
end;
check(x = "yes");
