(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Modified David C.J. Matthews 2008

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

structure HashArray:

(*****************************************************************************)
(*                  HashArray export signature                               *)
(*****************************************************************************)
sig
    type 'a hash
    
    val hash: int -> 'a hash
    val update: 'a hash * string * 'a -> unit
    val sub: 'a hash * string -> 'a option
    
    val fold: (string * 'a * 'b -> 'b) -> 'b -> 'a hash -> 'b
end =

(*****************************************************************************)
(*                  HashArray structure body                                 *)
(*****************************************************************************)
struct

local
  infix 8 sub

  (* Each entry in the table is a pair containing the key and the value. *)
  
  (* make namedOption local, because that means the new compiler can
     optimise its representation. SPF 11/5/95 *)
  datatype 'a namedOption = None | Some of (string * 'a);
in

  fun hashValue (N : int) (str: string) : int =
        Word.toInt(
            Word.mod(
                CharVector.foldr
                    (fn (ch: char, n: Word.word) => Word.fromInt(Char.ord ch) + 0w7*n)
                    0w0 str,
                (Word.fromInt N)))

(* The above function is the quickest and simplest way of computing the
   hash value now that we have Word.* and Word.mod compiled inline.
   They aren't implemented in all code-generators so it could be worth
   retaining the old code.  DCJM 26/2/01. *)

  datatype 'a hash =
    Hash of
      { 
        used: int ref,
        entries: 'a namedOption array ref,
        hash: (string -> int) ref
      }
  
    fun hash (n : int) =
        Hash
          {
            used    = ref 0,
            entries = ref (Array.array (n, None)),
            hash    = ref (hashValue n)
          }
     
    fun op sub (Hash {entries = ref A, hash = ref hashN, ...}, name : string) : 'a option =
      (* Searches the table starting from the position given by the hash value. *)
    let
        open Array
        (* Loops until it either finds an empty entry - in which case it
           returns NONE, or it finds the name it is looking for.
           There will always be several empty entries because we rehash
           if it gets too full. *)
        fun find i =
        let
            val h = A sub i;
        in
            case h of
                None => NONE
            |   Some (n,v) => 
                    if name = n then SOME v else find ((if i = 0 then length A else i) - 1)
        end
    in
        find (hashN name)
    end

    fun update (Hash {entries, used, hash}, name : string, value :  '_a) =
    let
        open Array
        (* Enters the value at the first free entry at or after the
           one pointed to by the hash value. *)
        fun enterTab (_ : '_a namedOption array, _ : int, None : '_a namedOption) = ()
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
        
        val A : '_a namedOption array = !entries;
        val N : int                   = length A;
        val hashN : string -> int     = !hash
        
        val () = enterTab (A, hashN name, Some (name, value));
        val () = used := !used + 1;
    in
        (* Do we need to rehash ? *)
        if !used * 5 > N * 4 (* More than 80% full so rehash *)
        then
        let
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
            
            val () = entries := newA;
            val () = hash := hashNewN;
        in
            copyOver (length A - 1)
        end
        else ()
    end;

    fun fold f init (Hash { entries = ref e, ...}) =
    let
        val length = Array.length e

        fun foldF i acc =
            if i >= length then acc
            else foldF (i+1)
                (case Array.sub(e, i) of
                     None => acc
                 |   Some (name,alpha) => f (name,alpha,acc)
                )
    in
        foldF 0 init
    end;

end (* local *);

end (* HashArray *);
