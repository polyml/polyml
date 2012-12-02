(* Performance issue.  When dealing with large expressions at the top level the
   code-generator could run extremely slowly.  Fixed by removing "special"
   entries when they are not needed. *)

val n = 1000;

signature KEY =
sig
  type key
  val ord: key * key -> order
end;

signature TABLE =
sig
    type key
    type 'a table
    val empty: 'a table
    val update: key * 'a -> 'a table -> 'a table
    val lookup: 'a table -> key -> 'a option
end;

functor Table(Key: KEY): TABLE =
struct


(* datatype table *)

type key = Key.key;

datatype 'a table =
  Empty |
  Branch2 of 'a table * (key * 'a) * 'a table |
  Branch3 of 'a table * (key * 'a) * 'a table * (key * 'a) * 'a table;

val empty = Empty;

fun lookup_key tab key =
  let
    fun look Empty = NONE
      | look (Branch2 (left, (k, x), right)) =
          (case Key.ord (key, k) of
            LESS => look left
          | EQUAL => SOME (k, x)
          | GREATER => look right)
      | look (Branch3 (left, (k1, x1), mid, (k2, x2), right)) =
          (case Key.ord (key, k1) of
            LESS => look left
          | EQUAL => SOME (k1, x1)
          | GREATER =>
              (case Key.ord (key, k2) of
                LESS => look mid
              | EQUAL => SOME (k2, x2)
              | GREATER => look right));
  in look tab end;

fun lookup tab key = Option.map #2 (lookup_key tab key);
datatype 'a growth =
  Stay of 'a table |
  Sprout of 'a table * (key * 'a) * 'a table;


fun modify key f tab =
  let
    fun modfy Empty = Sprout (Empty, (key, f NONE), Empty)
      | modfy (Branch2 (left, p as (k, x), right)) =
          (case Key.ord (key, k) of
            LESS =>
              (case modfy left of
                Stay left' => Stay (Branch2 (left', p, right))
              | Sprout (left1, q, left2) => Stay (Branch3 (left1, q, left2, p, right)))
          | EQUAL => Stay (Branch2 (left, (k, f (SOME x)), right))
          | GREATER =>
              (case modfy right of
                Stay right' => Stay (Branch2 (left, p, right'))
              | Sprout (right1, q, right2) =>
                  Stay (Branch3 (left, p, right1, q, right2))))
      | modfy (Branch3 (left, p1 as (k1, x1), mid, p2 as (k2, x2), right)) =
          (case Key.ord (key, k1) of
            LESS =>
              (case modfy left of
                Stay left' => Stay (Branch3 (left', p1, mid, p2, right))
              | Sprout (left1, q, left2) =>
                  Sprout (Branch2 (left1, q, left2), p1, Branch2 (mid, p2, right)))
          | EQUAL => Stay (Branch3 (left, (k1, f (SOME x1)), mid, p2, right))
          | GREATER =>
              (case Key.ord (key, k2) of
                LESS =>
                  (case modfy mid of
                    Stay mid' => Stay (Branch3 (left, p1, mid', p2, right))
                  | Sprout (mid1, q, mid2) =>
                      Sprout (Branch2 (left, p1, mid1), q, Branch2 (mid2, p2, right)))
              | EQUAL => Stay (Branch3 (left, p1, mid, (k2, f (SOME x2)), right))
              | GREATER =>
                  (case modfy right of
                    Stay right' => Stay (Branch3 (left, p1, mid, p2, right'))
                  | Sprout (right1, q, right2) =>
                      Sprout (Branch2 (left, p1, mid), p2, Branch2 (right1, q, right2)))));

  in
    (case modfy tab of
      Stay tab' => tab'
    | Sprout br => Branch2 br)
    handle SAME => tab
  end;


fun update (key, x) tab = modify key (fn _ => x) tab;

end;

structure Inttab = Table(type key = int; val ord = Int.compare);


fun fold _ [] y = y
  | fold f (x :: xs) y = fold f xs (f x y);

val table = 
  fold (fn i => Inttab.update (i, ())) (List.tabulate(n-1, fn i=>i+1)) Inttab.empty;

fun the_entry tab i = valOf (Inttab.lookup tab i);

val text =
  "let val tab = table " ^
    String.concatWith "\n" (map
      (fn i => "val entry" ^ Int.toString i ^ " = the_entry tab " ^ Int.toString i) (List.tabulate(n-1, fn i=>i+1)))
  ^ "\nin () end";

fun useString s =
let
    open TextIO PolyML
    val r = openString s
in
    while not (endOfStream r)
    do
        PolyML.compiler(fn () => input1 r, []) ()
end;

useString text;
