(* Record update can be enabled manually. *)

PolyML.Compiler.languageExtensions := true;

(* Basic usage. *)
let
    val r = {a = 0, b = [], c = ""}
    val a = 10
    and b = [20, 21, 22]
    and c = "30"

    (* Update one field *)
    val () =
        let
            val r1 = {r where a = a}
            val r2 = {r where b = b}
            val r3 = {r where c = c}
            val allCorrect : bool = 
                #a r1 = a    andalso #b r1 = #b r andalso #c r1 = #c r andalso
                #a r2 = #a r andalso #b r2 = b    andalso #c r2 = #c r andalso
                #a r3 = #a r andalso #b r3 = #b r andalso #c r3 = c
        in
            if allCorrect then () else raise Fail "Record update: update one field"
        end

    (* Update two fields *)
    val () =
        let
            val r1 = {r where a = a, b = b}
            val r2 = {r where a = a, c = c}
            val r3 = {r where b = b, c = c}
            val allCorrect : bool = 
                #a r1 = a    andalso #b r1 = b    andalso #c r1 = #c r andalso
                #a r2 = a    andalso #b r2 = #b r andalso #c r2 = c andalso
                #a r3 = #a r andalso #b r3 = b    andalso #c r3 = c
        in
            if allCorrect then () else raise Fail "Record update: update two field"
        end

    (* Update all fields *)
    val () =
        let
            val r1 = {r where a = a, b = b, c = c}
            val allCorrect : bool = 
                #a r1 = a    andalso #b r1 = b    andalso #c r1 = c
        in
            if allCorrect then () else raise Fail "Record update: update all field"
        end

    (* Record punning is supported *)
    (* Update all fields *)
    val () =
        let
            val r1 = {r where a}
            val r2 = {r where b}
            val r3 = {r where c}
            val r4 = {r where a, b}
            val r5 = {r where a, c}
            val r6 = {r where b, c}
            val r7 = {r where a, b, c}
            val r8 = {r where a = a, b, c}
            val r9 = {r where a, b = b, c}
            val allCorrect : bool = 
                #a r1 = a    andalso #b r1 = #b r andalso #c r1 = #c r andalso
                #a r2 = #a r andalso #b r2 = b    andalso #c r2 = #c r andalso
                #a r3 = #a r andalso #b r3 = #b r andalso #c r3 = c    andalso
                #a r4 = a    andalso #b r4 = b    andalso #c r4 = #c r andalso
                #a r5 = a    andalso #b r5 = #b r andalso #c r5 = c    andalso
                #a r6 = #a r andalso #b r6 = b    andalso #c r6 = c    andalso
                #a r7 = a    andalso #b r7 = b    andalso #c r7 = c    andalso
                #a r8 = a    andalso #b r8 = b    andalso #c r8 = c    andalso
                #a r9 = a    andalso #b r9 = b    andalso #c r9 = c
        in
            if allCorrect then () else raise Fail "Record update: update all field"
        end
in
  ()
end;

(* Can be used on function parameters with inlined type annotation *)
fun update (r : {a : int, b : int, c : int}) = {r where b = 1};

(* Together, record update and record punning enable terse update of long records. *)
structure S = struct
    type longrec =
        {aaa : int,
         bbb : int * int,
         ccc : int * int * int,
         ddd : int list,
         eee : int list list,
         fff : int -> int,
         ggg : int -> int -> int,
         hhh : {x : int, y : int, z : int}} 

    fun update_aaa (r : longrec) aaa : longrec = {r where aaa}
    fun update_bbb (r : longrec) bbb : longrec = {r where bbb}
    fun update_ccc (r : longrec) ccc : longrec = {r where ccc}
    fun update_ddd (r : longrec) ddd : longrec = {r where ddd}
    fun update_eee (r : longrec) eee : longrec = {r where eee}
    fun update_fff (r : longrec) fff : longrec = {r where fff}
    fun update_ggg (r : longrec) ggg : longrec = {r where ggg}
    fun update_hhh (r : longrec) hhh : longrec = {r where hhh}

end

(* Type checking is performed through unification. *)
structure S :> sig
    type ('a, 'b) t; 
    val getA: ('a, 'b) t -> 'a
    val setA: ('a, 'b) t -> 'a -> ('a, 'b) t
end = struct
    type ('a, 'b) t = {a : 'a, b : 'b}; 
    fun getA{a,...} = a
    fun setA x y = {x where a = y}
end;
