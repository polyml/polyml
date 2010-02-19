(* Code-generator bug in X86 development version.  The stack adjustment is wrong. *)

datatype cterm = Cterm of { thy_ref: int, t: int, T: int, maxidx: int, sorts: int};

PolyML.Compiler.maxInlineSize := 1;
fun read_prop s = Cterm {maxidx = ~1, thy_ref = valOf(Int.fromString s), t = 0, T = 0, sorts = 0};
PolyML.Compiler.maxInlineSize := 40;

val transitive_thm =
  (
  let
    val qqq = ref 0
    val xy = read_prop "123";
    val yz = read_prop "124";
    val xythm =
      let
      val _ =
        let
            val ct as Cterm {thy_ref, t, T, maxidx, sorts} = xy
        in
          if maxidx = ~1 then ct
          else
            Cterm {maxidx = t, thy_ref = thy_ref, t = t, T = T, sorts = sorts}
        end;
      in
        0
      end;
  in case xy of Cterm{sorts=0, ...} => () | _ => raise Fail "bad" end
  ) (*handle Match => raise Match*);
