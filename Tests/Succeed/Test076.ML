(* Assorted tests on flexible records.  Some should fail. *)

let
    val f = #a
    val g = #b

    fun h x =
    let
        val i = f x
        and j = g x
    in
        1
    end

    val _ = h {a = 1, b = 2, c = 3}
    val _ = h {a = true, b = [], c = 0.0 }
in
    1
end;


let
    fun f {a, ...} = a
    fun g {a, b} = a
    
    fun h x =
    let
        val i = f x
        and j = g x
    in
        1
    end

in
    1
end;



local
    val f = #a
    val g = #b

    fun h (x as {a, b, ...}) =
    let
        val i = f x
        and j = g x
    in
        1
    end

    val _ = h {a = 1, b = 2, c = 3}
    val _ = h {a = true, b = [], c = 0.0 }
in
    val h = h
end;


(* flexrecord2 *)
val _ =
   let
      val g = #foo
      val _ = g {foo = 13}
      val _ = g {foo = "yes"}
   in
      ()
   end;
(* flexrecord2 *)

(* flexrecord3 *)
val _ =
   let
      val g = #foo
      val _ = g {foo = 13, goo = 1.0}
      val _ = g {foo = "yes", goo = 1.0}
   in
      ()
   end;
(* flexrecord3 *)

(* flexrecord4 *)
val _ =
   let
      val g = #foo
      val _ = g {foo = 13, goo = 1.0}
      val _ = g {foo = "yes", goo = false}
   in
      ()
   end;
(* flexrecord4 *)

local
    val g = #b
    fun f (x as {a, ...}) = if x = x then g x else g x
    val _ = f{a=1, b=0, c = true}
in
    val f = f
end;

let
    fun g (x as {b,...}) = x
    fun i (x as {a,b,...}) =
        g (if true then g x else g {a=2, b = 1, c=3})
in
    1
end;

(* Setting the generic has an effect on an instance constrained by value polymorphism.
   Both foo and bar in g have types which are free variables. *)
local
    val f = #foo
    val g = valOf o f
    val _ = f {foo=0, bar=1}
in
    val f = f and g = g
end;

local
    val f = #foo
    val g = (valOf: int option -> int) o f
    val _ = f {foo=0, bar=1}
in
    val f = f and g = g
end;


local
    val f = #foo
    val g = valOf o f
    val _ = f {foo=0, bar=1}
    val _ = g {foo=SOME 0, bar = true}
in
    val f = f and g = g
end;

(* Check that equality is inherited correctly. The equality attribute is a
   property of the generic as well as the instance.
   In this example the generic instance for g is an equality type but f
   is not. *)
local
    fun f (x as {a, ...}) = (x=x; a)
    fun g {a, ...} = a
    fun h x = (g x; f x)
    val _ = h{a=1, b=2}
in
    val f = f and g = g and h = h
end;


(* In this example it is the INSTANCE which is an equality type. ???? *)
local
    val f = #foo
    fun g x = (x=x; f x)
    val _ = f{foo=0.0, bar=0.0}
in
    val f = f and g = g
end;


(* This demonstrates that the flexible record can be made rigid anywhere
   withing the topdec. *)
structure S = struct val f = #foo end val _ = S.f{foo=1, goo=2};


(* Shows that we can make the record rigid using the signature. *)
structure S : sig val f : {a: 'a, b: 'b} -> 'a end =
struct val f = #a end;
