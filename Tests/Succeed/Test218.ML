(* Test the String concatenation family, with optional separators and reversal. *)

fun verify name actual expected =
    if actual = expected then
        ()
    else
        raise Fail (name ^ ": expected \"" ^ String.toString expected ^
            "\", got \"" ^ String.toString actual ^ "\"");

val () = verify "String.concat example"
    (String.concat ["abc", "def", "ghi"]) "abcdefghi";

val () = verify "String.concatWith example"
    (String.concatWith " " ["abc", "def", "ghi"]) "abc def ghi";

val () = verify "String.concatRev example"
    (String.concatRev ["abc", "def", "ghi"]) "ghidefabc";

val () = verify "String.concatWithRev example"
    (String.concatWithRev " " ["abc", "def", "ghi"]) "ghi def abc";

structure Specification =
struct
    fun concat [] = ""
      | concat (s :: rest) = s ^ concat rest

    fun concatRev [] = ""
      | concatRev (s :: rest) = concatRev rest ^ s

    fun concatWith _ [] = ""
      | concatWith _ [s] = s
      | concatWith separator (s :: rest) = s ^ separator ^ concatWith separator rest

    fun concatWithRev _ [] = ""
      | concatWithRev _ [s] = s
      | concatWithRev separator (s :: rest) = concatWithRev separator rest ^ separator ^ s
end;

(* Compare each concat function with its specification and some identities. *)
fun check (strings : string list) : unit =
    let
        val forward = String.concat strings
        val backward = String.concatRev strings
        fun checkSeparator separator =
        let val backwardWithSeparator = String.concatWithRev separator strings in
            verify ("String.concatWith \"" ^ String.toString separator ^ "\"")
                (String.concatWith separator strings) (Specification.concatWith separator strings);
            verify ("String.concatWithRev \"" ^ String.toString separator ^ "\"")
                backwardWithSeparator (Specification.concatWithRev separator strings);
            verify ("String.concatWithRev \"" ^ String.toString separator ^
                "\" (identity with String.concatWith \"" ^ String.toString separator ^ "\" o List.rev)")
                backwardWithSeparator (String.concatWith separator (List.rev strings))
        end
    in
        verify "String.concat"
            forward (Specification.concat strings);
        verify "String.concatWith \"\" (identity with String.concat)"
            (String.concatWith "" strings) forward;
        verify "String.concatRev"
            backward (Specification.concatRev strings);
        verify "String.concatRev (identity with String.concat o List.rev)"
            backward (String.concat (List.rev strings));
        verify "String.concatWithRev \"\" (identity with String.concatRev)"
            (String.concatWithRev "" strings) backward;
        List.app checkSeparator
            [" ", "\t", "\n", "\000", ", ", "::", "\r\n", "\000\255",
             "--- a longer separator spanning several machine words ---"]
    end;

local

(* Generate and convert all lists up to the given length, ordered by length.
   With k = length elements, the count is sum(k^i, i = 0..maxLength). *)
fun listsUpTo (convert : 'a list -> 'b) (elements : 'a list) (maxLength : int) : 'b list =
    let
        (* Prepend each element to every tail, converting directly into the result. *)
        fun extend f tails =
            let
                fun addExtendedList element (tail, results) =
                    f (element :: tail) :: results
                fun addListsStartingWith (element, results) =
                    List.foldr (addExtendedList element) results tails
            in
                List.foldr addListsStartingWith [] elements
            end

        (* Build lists of exactly n elements, sharing their tails. *)
        fun ofLength 0 = [[]]
          | ofLength n = extend (fn xs => xs) (ofLength (n - 1))

        (* Apply convert while building lists of exactly n elements. *)
        fun convertedOfLength 0 = [convert []]
          | convertedOfLength n = extend convert (ofLength (n - 1))
    in
        List.concat (List.tabulate (maxLength + 1, convertedOfLength))
    end;

(* Check every list of bounded strings over the given alphabet.
   For alphabet size a, there are s = sum(a^i, i = 0..maxStringLength) strings
   and sum(s^j, j = 0..maxListLength) lists. *)
fun checkAll (alphabet : string) (maxStringLength : int) (maxListLength : int) : unit =
    let val strings = listsUpTo String.implode (String.explode alphabet) maxStringLength in
        List.app check (listsUpTo (fn xs => xs) strings maxListLength)
    end;

in

(* Exhaustively enumerate all strings over each alphabet up to the maximum string
   length, and all lists of those strings up to the maximum list length. *)
val () = checkAll "ab" 2 6;       (* 7 strings; 7^0 + 7^1 + ... + 7^6 = 137257 lists. *)
val () = checkAll "\000\255" 2 6; (* 7 strings; 7^0 + 7^1 + ... + 7^6 = 137257 lists. *)

end

local

val bytes = CharVector.tabulate (256, Char.chr);

in

(* Exercise word boundaries and all byte values. *)
val () = List.app
    (fn n => check [String.substring (bytes, 0, n), "", bytes, "end"])
    (List.tabulate (33, fn i => i));

end

local

(* Build a string cycling through all byte values. *)
fun block (n : int) : string = CharVector.tabulate (n, fn i => Char.chr (i mod 256));

in

(* Copy blocks much larger than a word, with differing lengths and alignments. *)
val () = check [block 4093, "", block 8192, "x", block 16385];

end

local

val emptyStrings = List.tabulate (100, fn _ => "");

in

(* Empty prefixes and suffixes also exercise the zero-length copies. *)
val () = List.app check [emptyStrings, "abc" :: emptyStrings, emptyStrings @ ["abc"]];

end

(* Keep the lists small enough for the references using ^. *)
val () = check (List.tabulate (100, fn i =>
    if i mod 3 = 0 then
        ""
    else
        Int.toString i));

(* Concatenate all byte values as individual strings. *)
val () = check (List.tabulate (256, fn i => String.str (Char.chr i)));

local

(* Use a fresh thread so an already-grown stack cannot defeat the limit. *)
fun withSmallStack f =
    let
        val mutex = Thread.Mutex.mutex ()
        val finished = Thread.ConditionVar.conditionVar ()
        val result : exn option option ref = ref NONE
        fun worker () =
            let
                val outcome = (f (); NONE) handle exn => SOME exn
                (* Remove the limit before reporting the outcome. *)
                val () = Thread.Thread.setAttributes [Thread.Thread.MaximumMLStack NONE]
            in
                Thread.Mutex.lock mutex;
                result := SOME outcome;
                Thread.ConditionVar.signal finished;
                Thread.Mutex.unlock mutex
            end
        val _ = Thread.Thread.fork (worker, [Thread.Thread.MaximumMLStack (SOME 3000)])
        val () = Thread.Mutex.lock mutex
        fun await () =
            case !result of
                NONE => (Thread.ConditionVar.wait (finished, mutex); await ())
              | SOME outcome => outcome
        val outcome = await ()
        val () = Thread.Mutex.unlock mutex
    in
        case outcome of
            NONE => ()
          | SOME exn => raise exn
    end;

val strings = List.tabulate (100000, fn _ => "a");
val expected = CharVector.tabulate (100000, fn _ => #"a");
val expectedSeparated1 = CharVector.tabulate (199999, fn _ => #"a");
val expectedSeparated2 = CharVector.tabulate (299998, fn _ => #"a");

fun checkStackOverflow (name, f) =
    (withSmallStack (fn () => ignore (f strings));
     raise Fail ("Stack limit did not reject Specification." ^ name))
    handle Thread.Thread.Interrupt => ();

fun checkSmallStack (name, f, expected) =
    withSmallStack (fn () =>
        if f strings = expected then
            ()
        else
            raise Fail "Incorrect result")
    handle exn =>
        raise Fail ("String." ^ name ^ " with limited stack: " ^ General.exnMessage exn);

in

(* Verify that the stack limit rejects each non-tail-recursive specification
   on this input, while the library functions succeed under the same limit.
   The specification checks each produce an expected runtime warning:
   "Warning - Unable to increase stack - interrupting thread". *)
val () = List.app checkStackOverflow
    [("concat", Specification.concat),
     ("concatRev", Specification.concatRev),
     ("concatWith empty separator", Specification.concatWith ""),
     ("concatWith one-character separator", Specification.concatWith "a"),
     ("concatWith two-character separator", Specification.concatWith "aa"),
     ("concatWithRev empty separator", Specification.concatWithRev ""),
     ("concatWithRev one-character separator", Specification.concatWithRev "a"),
     ("concatWithRev two-character separator", Specification.concatWithRev "aa")];

val () = List.app checkSmallStack
    [("concat", String.concat, expected),
     ("concatRev", String.concatRev, expected),
     ("concatWith empty separator", String.concatWith "", expected),
     ("concatWith one-character separator", String.concatWith "a", expectedSeparated1),
     ("concatWith two-character separator", String.concatWith "aa", expectedSeparated2),
     ("concatWithRev empty separator", String.concatWithRev "", expected),
     ("concatWithRev one-character separator", String.concatWithRev "a", expectedSeparated1),
     ("concatWithRev two-character separator", String.concatWithRev "aa", expectedSeparated2)];

end

local

(* Count all allocated words, including temporary objects that become garbage. *)
fun allocatedWords (f : string list -> string) (strings : string list) : int =
    let
        val words = ref 0
        val _ = PolyML.Profiling.profileStream
            (fn entries =>
                words := List.foldl (fn ((count, _), total) => total + count) 0 entries)
            PolyML.Profiling.ProfileAllocations f strings
    in
        !words
    end;

(* Build nonempty inputs before profiling; both produce the same 1000-character string. *)
val chunk = CharVector.tabulate (500, fn _ => #"a");
val shorter = [chunk, chunk];
val longer = List.tabulate (1000, fn _ => "a");

(* Both the short and long inputs must allocate exactly the space occupied by
   the result string, without temporary objects. *)
fun checkAllocation (name : string) (f : string list -> string) shorter : unit =
    let
        val expectedWords = PolyML.objSize (f shorter)
    in
        if allocatedWords f shorter = expectedWords andalso
           allocatedWords f longer = expectedWords then
            ()
        else
            raise Fail (name ^ " allocates more than the result string")
    end;

fun checkWithAllocation separator =
    let
        val sepSize = String.size separator
        val resultSize = 1000 + 999 * sepSize
        fun block n = CharVector.tabulate (n, fn _ => #"a")
        val fewer = [block (resultSize - sepSize - 1), "a"]
    in
        checkAllocation ("String.concatWith \"" ^ String.toString separator ^ "\"")
            (String.concatWith separator) fewer;
        checkAllocation ("String.concatWithRev \"" ^ String.toString separator ^ "\"")
            (String.concatWithRev separator) fewer
    end;

in

(* The bytecode interpreter does not count inline allocations. *)
val () =
    if PolyML.architecture () = "Interpreted" then
        ()
    else
        let val reference = String.concat o List.rev in
            (* Verify that the profiler detects an actual intermediate list. *)
            if allocatedWords reference longer > allocatedWords reference shorter then
                (checkAllocation "String.concat" String.concat shorter;
                 checkAllocation "String.concatRev" String.concatRev shorter;
                 List.app checkWithAllocation ["", " ", "::", "a longer separator"])
            else
                raise Fail "Allocation profiler did not detect List.rev"
        end;

end
