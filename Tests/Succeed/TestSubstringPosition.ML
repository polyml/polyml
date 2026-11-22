(* Test Substring.position. *)

fun describe (prefix, suffix) =
    "(\"" ^ String.toString (Substring.string prefix) ^ "\" at " ^
    Int.toString (#2 (Substring.base prefix)) ^ ", \"" ^
    String.toString (Substring.string suffix) ^ "\" at " ^
    Int.toString (#2 (Substring.base suffix)) ^ ")";

(* Compare the position and length of both slices, and their underlying strings. *)
fun verify name (actual as (actualPrefix, actualSuffix)) (expected as (expectedPrefix, expectedSuffix)) =
    if Substring.base actualPrefix = Substring.base expectedPrefix andalso
       Substring.base actualSuffix = Substring.base expectedSuffix then
        ()
    else
        raise Fail (name ^ ": expected " ^ describe expected ^ ", got " ^ describe actual);

val () = verify "Substring.position example"
    (Substring.position "abc" (Substring.full "xxabcyyabc"))
    (Substring.substring ("xxabcyyabc", 0, 2), Substring.substring ("xxabcyyabc", 2, 8));

val () = verify "Substring.position example without match"
    (Substring.position "abd" (Substring.full "xxabcyy"))
    (Substring.full "xxabcyy", Substring.substring ("xxabcyy", 7, 0));

val () = verify "Substring.position example with empty string"
    (Substring.position "" (Substring.full "xxabcyy"))
    (Substring.substring ("xxabcyy", 0, 0), Substring.full "xxabcyy");

val () = verify "Substring.position example on a slice"
    (Substring.position "ab" (Substring.substring ("abxabab", 1, 5)))
    (Substring.substring ("abxabab", 1, 2), Substring.substring ("abxabab", 3, 3));

structure Specification =
struct
    (* Split at the first position where s occurs within the slice, or at its
       end if there is none. *)
    fun position s slice =
        let
            val (base, start, length) = Substring.base slice
            val m = String.size s
            fun search k =
                if k + m > length then
                    length
                else if String.substring (base, start + k, m) = s then
                    k
                else
                    search (k + 1)
            val k = search 0
        in
            (Substring.substring (base, start, k),
             Substring.substring (base, start + k, length - k))
        end
end;

fun describeSlice slice =
    let val (base, start, length) = Substring.base slice in
        "slice (" ^ Int.toString start ^ ", " ^ Int.toString length ^ ") of \"" ^
        String.toString base ^ "\""
    end;

(* Compare Substring.position with its specification. *)
fun check (s : string) (slice : Substring.substring) : unit =
    verify ("Substring.position \"" ^ String.toString s ^ "\" on " ^ describeSlice slice)
        (Substring.position s slice) (Specification.position s slice);

local

(* All strings over the alphabet up to the given length, ordered by length. *)
fun stringsUpTo (alphabet : string) (maxLength : int) : string list =
    let
        val chars = String.explode alphabet
        fun ofLength 0 = [""]
          | ofLength n =
            List.concat (List.map (fn s => List.map (fn c => String.str c ^ s) chars) (ofLength (n - 1)))
    in
        List.concat (List.tabulate (maxLength + 1, ofLength))
    end;

(* All slices of the string, including the empty ones. *)
fun slicesOf (base : string) : Substring.substring list =
    List.concat (List.tabulate (String.size base + 1, fn i =>
        List.tabulate (String.size base - i + 1, fn n => Substring.substring (base, i, n))));

(* Check every pattern up to the maximum pattern length against every slice of
   every string up to the maximum string length. For alphabet size a, there are
   sum(a^i, i = 0..maxPatternLength) patterns and
   sum(a^n * (n + 1) * (n + 2) / 2, n = 0..maxStringLength) slices. *)
fun checkAll (alphabet : string) (maxPatternLength : int) (maxStringLength : int) : unit =
    let
        val patterns = stringsUpTo alphabet maxPatternLength
        val slices = List.concat (List.map slicesOf (stringsUpTo alphabet maxStringLength))
    in
        List.app (fn s => List.app (check s) slices) patterns
    end;

in

(* Exhaustively enumerate patterns that are both shorter and longer than the
   slices, and slices that do not extend to either end of their string. *)
val () = checkAll "ab" 5 10;      (* 63 patterns; 114687 slices. *)
val () = checkAll "\000\255" 4 6; (* 31 patterns; 2815 slices. *)

end

local

val padding = CharVector.tabulate (16384, fn _ => #"a");
val text = padding ^ "b";
val pattern = String.substring (padding, 0, 300) ^ "b";

in

(* Compare a pattern much longer than a word at every position and alignment.
   Before the end, the pattern differs from the text only in its last byte. *)
val () = check pattern (Substring.full text);
val () = check pattern (Substring.substring (text, 0, String.size text - 1));
val () = check pattern (Substring.extract (text, String.size text - String.size pattern, NONE));

end

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

(* Equivalent to Specification.position, but not tail recursive: The recursive
   call returns the distance to the first match. *)
fun nonTailRecursivePosition s slice =
    let
        val (base, start, length) = Substring.base slice
        val m = String.size s
        fun distance k =
            if k + m > length then
                length - k
            else if String.substring (base, start + k, m) = s then
                0
            else
                1 + distance (k + 1)
        val k = distance 0
    in
        (Substring.substring (base, start, k),
         Substring.substring (base, start + k, length - k))
    end;

val length = 100000;
val text = CharVector.tabulate (length, fn _ => #"a") ^ "b";
val slice = Substring.full text;

(* Each pattern is compared at every position of the slice. *)
val patterns =
    [("match at the end", "ab"),
     ("no match", "ac"),
     ("long pattern with match at the end", String.extract (text, length - 99, NONE))];

fun checkStackOverflow (name, s) =
    (withSmallStack (fn () => ignore (nonTailRecursivePosition s slice));
     raise Fail ("Stack limit did not reject nonTailRecursivePosition with " ^ name))
    handle Thread.Thread.Interrupt => ();

fun checkSmallStack (name, s) =
    withSmallStack (fn () => check s slice)
    handle exn =>
        raise Fail ("Substring.position with " ^ name ^ " and limited stack: " ^
            General.exnMessage exn);

in

(* Verify that the stack limit rejects a non-tail-recursive search on this
   input, while the library function succeeds under the same limit. The
   non-tail-recursive checks each produce an expected runtime warning:
   "Warning - Unable to increase stack - interrupting thread". *)
val () = List.app checkStackOverflow patterns;
val () = List.app checkSmallStack patterns;

end

local

(* Count all allocated words, including temporary objects that become garbage. *)
fun allocatedWords (f : Substring.substring -> 'a) (slice : Substring.substring) : int =
    let
        val words = ref 0
        val _ = PolyML.Profiling.profileStream
            (fn entries =>
                words := List.foldl (fn ((count, _), total) => total + count) 0 entries)
            PolyML.Profiling.ProfileAllocations f slice
    in
        !words
    end;

val length = 4096;
val text = CharVector.tabulate (length, fn _ => #"a") ^ "b";

(* Return a short slice for which the search ends after a single comparison and
   a long one for which it ends after "length" more, both with the same result
   shape for a pattern of the given size. *)
fun inputs (patternSize : int) : Substring.substring * Substring.substring =
    (Substring.extract (text, length + 1 - patternSize, NONE), Substring.full text);

(* The search must not allocate, so both slices allocate the same space for the
   result. Comparing them avoids depending on the rounding of allocations. *)
fun checkAllocation (name : string) (pattern : string) : unit =
    let
        val f = Substring.position pattern
        val (shorter, longer) = inputs (String.size pattern)
    in
        if allocatedWords f shorter = allocatedWords f longer then
            ()
        else
            raise Fail (name ^ " allocates during the search")
    end;

in

val () =
    if PolyML.architecture () = "Interpreted" then
        (* The bytecode interpreter does not count inline allocations. *)
        ()
    else
        let
            val reference = Specification.position "ab"
            val (shorter, longer) = inputs 2
        in
            (* Verify that the profiler detects allocations during the search. *)
            if allocatedWords reference longer > allocatedWords reference shorter then
                (checkAllocation "Substring.position with match" "ab";
                 checkAllocation "Substring.position without match" "ac";
                 checkAllocation "Substring.position with long pattern"
                     (String.extract (text, length - 99, NONE)))
            else
                raise Fail "Allocation profiler did not detect String.substring"
        end;

end
