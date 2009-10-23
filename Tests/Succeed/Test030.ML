(* Bug in CODETREE.ML which resulted in Subscript exception in the compiler. 
   Reported by Lucas Dixon ldixon at inf dot ed dot ac dot uk *)

PolyML.Compiler.maxInlineSize := 80;

structure Output =
struct
fun output_width x = (x, 99)
end;

structure Markup =
struct
  type property = string * string
  type T = string * property list
  val none = ("", []);
end

structure Pretty =
struct
    datatype T =
      Block of Markup.T * T list * int * int |
      String of string * int |
      Break of bool * int;

    fun length (Block (_, _, _, len)) = len
      | length (String (_, len)) = len
      | length (Break (_, wd)) = wd;

    val str = String o Output.output_width;

    fun markup_block m (indent, es) =
      let
        fun sum [] k = k
          | sum (e :: es) k = sum es (length e + k);
      in Block (m, es, indent, sum es 0) end;

    val blk = markup_block Markup.none;
    fun block prts = blk (2, prts);

end;

fun foo l s x i =
    []
    @ map (fn (i,s) => Pretty.block
                       [(case x
                          of NONE => Pretty.str "a"
                           | SOME openi =>
                             (if (openi = i)
                              then Pretty.str "b" else Pretty.str "c")),
                        Pretty.str "d"])
          l;

