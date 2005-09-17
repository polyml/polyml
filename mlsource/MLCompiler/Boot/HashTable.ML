(*
	Copyright (c) 2000
		Cambridge University Technical Services Limited

	This library is free software; you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public
	License as published by the Free Software Foundation; either
	version 2.1 of the License, or (at your option) any later version.
	
	This library is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
	Lesser General Public License for more details.
	
	You should have received a copy of the GNU Lesser General Public
	License along with this library; if not, write to the Free Software
	Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

(* Hash table type - Creates a hash table of specified initial size. This
   version expands the hash table and rehashes when the table gets too full *)

(*
Changes:
  25/03/94 SPF bugfix (off-by-one error in hashOver/next function)

*)


(* HACKS: Misc, Array, Int.rem *)

structure HashTable:

(*****************************************************************************)
(*                  HashTable export signature                               *)
(*****************************************************************************)
sig
  type 'a hash
  type 'a iter
  
  val hashMake: int -> '_a hash
  val hashSet: '_a hash * string * '_a -> unit
  val hashSub: 'a hash * string -> 'a
  
  (* An iterator over the non-empty entries in the table. *)
  val hashOver: 'a hash -> (string * 'a) iter
  val hashFold: 'a hash -> (string -> 'a -> 'b -> 'b) -> 'b -> 'b
end =

(*****************************************************************************)
(*                  HashTable structure body                                 *)
(*****************************************************************************)
struct

local
  open Misc;
  open Array; infix 8 sub;
  val op mod = Int.rem; (* SPF 24/9/94 *)

  (* Each entry in the table is a pair containing the key and the value. *)
  
  (* make namedOption local, because that means the new compiler can
     optimise its representation. SPF 11/5/95 *)
  datatype 'a namedOption = None | Some of (string * 'a);
in
  
  type 'a iter = 'a iter;

  local
  	val op mod = Word.mod
  in
    fun hashValue (N : int) (str: string) : int =
	  	Word.toInt(
		  	(CharVector.foldr(
				fn (ch: char, n: Word.word) =>
					Word.fromInt(Char.ord ch) + 0w7*n)
				) 0w0 str mod (Word.fromInt N))
  end

(* The above function is the quickest and simplest way of computing the
   hash value now that we have Word.* and Word.mod compiled inline.
   They aren't implemented in all code-generators so it could be worth
   retaining the old code.  DCJM 26/2/01. *)
(*
  local
    (* A specialized version of mod - must have: 0 <= x andalso 0 < n *)
    fun natMod (x : int, n : int) : int =
      if x < n then x
      else let
	val y = natMod (x, 2 * n);
	(* y = x mod (2 * n) *)
	val z = if y < n then y else y - n;
	(* z = x mod n *)
      in
	z
      end

    (* Must have 0 < n; gives 256 <= grow n andalso grow n mod n = 0 *)
    fun grow n = if n < 256 then grow (2 * n) else n;


    (* 0 <= sum < n andalso 256 <= n *)
    fun hashLoop (0,   sum, str, n) = sum
      | hashLoop (ind, sum, str, n) = 
    let
(* The following is slightly slower on the SPARC,
    but MUCH quicker on the HP than simply saying:
      val sum7 = 7 * sum;
    That's because the HP version isn't very good at
    inlining multiplications yet. (Must fix!)
    SPF 27/3/97
 *)
      val sum7 = sum + (2 * (sum + 2 * sum));
      (* 0 <= sum7 < 7 * n *)
      
      val w = Char.ord(String.sub(str, ind-1)) + sum7;
      (* 0 <= w < 8 * n *)
      
      (* It's probably quicker to recalculate these values
         here rather than fetching them from a closure. *)
      val n2 = 2 * n;
      val n4 = 2 * n2;
      
      val x = if w < n4 then w else w - n4;
      (* 0 <= x < 4 * n andalso x mod n = w mod n *)
      
      val y = if x < n2 then x else x - n2;
      (* 0 <= y < 2 * n andalso y mod n = w mod n *)
      
      val z = if y < n then y else y - n;
      (* 0 <= z < n andalso z mod n = w mod n *)
      (* i.e. z = w mod n *)
    in
      hashLoop (ind - 1, z, str, n)
    end;
  in
    (* 0 < N *)
    (* Calculate the hash value from a string.
       Returns a value in the range 0..(N-1). *)
    fun hashValue (N : int) : string -> int =
    let
      val bigN  = grow N;
      (* 256 <= bigN andalso bigN mod N = 0 *)
  
      fun hash (str : string) : int =
        natMod (hashLoop (size str, 0, str, bigN), N);
    in
      hash 
    end;
  end;

*)
(* old stuff ...
  (* Calculate the hash value from a string.
  Returns a value in the range 0..(N-1). *)
  fun hashValue N str =
  let
    (* This hash function was suggested by Mike Crawley. *)
    (* It may look rather inefficient to apply "mod" each time rather
       than at the end but it generally means that the arithmetic can
       be done using short precision rather than long, and so works
       much faster. *)
    (* This code has been *very* carefully crafted (reading assembler
       output etc. and is performance-critical to the compiler, so
       modify it at your peril. SPF 14/10/94 *)

    fun hash str N sum 0   = sum
      | hash str N sum ind = 
    let
      val newSum = (ordOf (str, ind) + sum * 7) mod N;
    in
      hash str N newSum (ind - 1)
    end;
  in
    hash str N 0 (size str) 
  end;
... *)

  datatype 'a hash =
    Hash of
      { 
        used: int ref,
        entries: 'a namedOption array ref,
        hash: (string -> int) ref
      };
  
  fun hashMake (n : int) =
    Hash
      {
        used    = ref 0,
        entries = ref (array (n, None)),
        hash    = ref (hashValue n)
      }
     handle (*Array.*)Size => 
       raise InternalError ("HashTable.hash: " ^ Int.toString n);
    
  fun hashSub (table : 'a hash, name : string) : 'a =
  (* Searches the table starting from the position given by the hash value. *)
  let
    (* Loops until it either finds an empty entry - in which case it
       raises an exception, or it finds the name it is looking for.
       There will always be several empty entries because we rehash
       if it gets too full. *)

    val Hash {entries = ref A, hash = ref hashN, ...} = table;
    
    fun find i =
    let
      val h = A sub i;
    in
      case h of
        None       => raise ValueMissing name
      | Some (n,v) => 
         if name = n then v else find ((if i = 0 then length A else i) - 1)
    end;
  in
    find (hashN name)
  end;

  fun hashSet (table : '_a hash, name : string, value :  '_a) : unit =
  let
    (* Enters the value at the first free entry at or after the
       one pointed to by the hash value. *)
    fun enterTab (A : '_a namedOption array, i : int, None : '_a namedOption) = ()
      | enterTab (A, i, entry as Some (name,_)) =
    let
      fun enter (i : int) : unit =
        (* Loops until it either finds an empty entry - in which case it
           enters the value in there, or it finds the string. *)
        case (A sub i) of
          None        => update (A, i, entry)
        | Some (n : string, _) => 
           if n = name
           then update (A, i, entry) (* Same name as previous - overwrite it *)
           else enter ((if i = 0 then length A else i) - 1);
    in
      enter i
    end 

    val Hash {entries, used, hash} = table;
    
    val A : '_a namedOption array = !entries;
    val N : int                   = length A;
    val hashN : string -> int     = !hash
    
    val U : unit = enterTab (A, hashN name, Some (name, value));
    val U : unit = used := !used + 1;
  in
    (* Do we need to rehash ? *)
    if !used * 5 > N * 4 (* More than 80% full so rehash *)
    then let
      val newN  : int                   = N * 2; (* Double the size *)
      val newA  : '_a namedOption array = array (newN, None);
      val hashNewN : string -> int      = hashValue newN;
      
      fun copyOver (index : int) : unit =
        if index < 0 then ()
        else
        (
          case (A sub index) of
            None                => ()
          | entry as Some (name : string,_) =>
              enterTab (newA, hashNewN name, entry);
          
          copyOver (index - 1)
        );
        
      val U : unit = entries := newA;
      val U : unit = hash := hashNewN;
    in
      copyOver (length A - 1)
    end
    else ()
  end;
  
  fun hashOver (Hash {entries,...}) =
  let
    (* An iterator over the non-empty entries in the table. *)
      
    val entries = !entries;
    val length  = length entries;
    
    fun continue i = i < length;
  
    fun value i =
      (* Must check for limit since value can be called explicitly
         even if continue is false. *)
      
      if continue i then
        case (entries sub i) of
          None      => raise InternalError "HashTable.hashOver.value (None)"
        | Some pair => pair
      else
        raise InternalError "HashTable.hashOver.value (limit passed)"

    (* Return the next non-empty entry. *)
    fun next i =
    let
      val n = i + 1;
    in
      if n >= length
      then
        makeIter n (* stop if we've reached the end (bugfixed SPF 25/3/94) *)
      else
        case (entries sub n) of
          None   => next n
        | Some _ => makeIter n
    end
  
    
    and makeIter i = 
      Iter
        {
          continue = continue i,
          next     = fn () => next i,
          value    = fn () => value i
        };
  in
    next ~1
  end;

  fun hashFold table f =
  let
    fun foldF i acc =
    (
      if not (Continue i) then acc else
      let
        val (name,alpha) = Value i;
      in
        foldF (Next i) (f name alpha acc)
      end
    );
  in
    foldF (hashOver table)
  end;

end (* local *);

end (* HashTable *);
