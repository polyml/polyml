(* Test String.concat and String.concatRev, which concatenate strings in forward
   and reverse list order respectively, preserving each string. *)

val () =
    if String.concat ["abc", "def", "ghi"] = "abcdefghi" then
        ()
    else
        raise Fail "String.concat";

val () =
    if String.concatRev ["abc", "def", "ghi"] = "ghidefabc" then
        ()
    else
        raise Fail "String.concatRev";

(* Compare with repeated ^ as well as concat o List.rev. *)
fun check (strings : string list) : unit =
    let
        val forward = String.concat strings
        val backward = String.concatRev strings
        val reversedForward = String.concat (List.rev strings)
        val expectedForward = List.foldr (op ^) "" strings
        val expectedBackward = List.foldl (op ^) "" strings
    in
        if backward = reversedForward andalso
           forward = expectedForward andalso
           backward = expectedBackward then
            ()
        else
            raise Fail "String concatenation"
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

(* Allocating an intermediate list would make allocation grow with the input. *)
fun checkAllocation (name : string) (f : string list -> string) : unit =
    if allocatedWords f shorter = allocatedWords f longer then
        ()
    else
        raise Fail (name ^ " allocation grows with list length");

in

(* The bytecode interpreter does not count inline allocations. *)
val () =
    if PolyML.architecture () = "Interpreted" then
        ()
    else
        let val reference = String.concat o List.rev in
            (* Verify that the profiler detects an actual intermediate list. *)
            if allocatedWords reference longer > allocatedWords reference shorter then
                (checkAllocation "String.concat" String.concat;
                 checkAllocation "String.concatRev" String.concatRev)
            else
                raise Fail "Allocation profiler did not detect List.rev"
        end;

end
