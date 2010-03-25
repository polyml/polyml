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

structure Misc :
(*****************************************************************************)
(*                  Misc exports signature                                  *)
(*****************************************************************************)
sig
  (* standard exceptions used by the compiler *) 
  exception Interrupt; 

  (* These are handled in the compiler *)
  exception Conversion of string;     (* string to int conversion failure *)

  (* This isn't handled at all (except generically) *)
  exception InternalError of string; (* compiler error *)

  val quickSort : ('a -> 'a -> bool) -> 'a list -> 'a list

  val unescapeString : string -> string

  type 'a iter;
  
  val Iter : {next: unit -> 'a iter, value: unit -> 'a, continue: bool} -> 
                 'a iter
       
  val Continue : 'a iter -> bool
  val Value    : 'a iter -> 'a
  val Next     : 'a iter -> 'a iter

  val iterList : 'a iter -> 'a list
  val upto     : int -> int -> int iter
  val downto   : int -> int -> int iter
  
  val for    : 'a iter -> ('a -> 'b) -> unit

  val forsucc:  ('a -> 'a) -> ('a * 'b -> bool) -> 
                      'a -> 'b -> ('a -> 'c) -> unit
  val first  : ('a -> bool) -> ('a -> 'b) -> (unit -> 'b) -> 'a iter -> 'b

  val mapIterator     : ('a -> 'b) -> 'a iter -> 'b list
  val revfoldIterator : ('a -> 'b -> 'b) -> 'b -> 'a iter -> 'b
  
  val lookupDefault : ('a -> 'b option) -> ('a -> 'b option) -> 'a -> 'b option
end  =

(*****************************************************************************)
(*                  Misc structure body                                      *)
(*****************************************************************************)
struct
  open RuntimeCalls; (* for POLY_SYS and EXC numbers *)
  exception Interrupt = SML90.Interrupt; 

  (* These are handled in the compiler *)
  (* Conversion may be also raised by run-time Conversion routines *)
  exception Conversion = RunCall.Conversion

  (* This isn't handled at all (except generically) *)
  exception InternalError of string; (* compiler error *)

    (* Simple definition of quicksort.  This is fairly efficient on time but is very
       bad for memory allocation and is only suitable for small cases.
       The call to List.partition builds O(N.log N) cells, if the list is random or
       O(N.N) if it is already sorted.  It has been improved from the original
       which used append (@) to build the sorted list which added a second O(N.N)
       phase. *)
    fun quickSort (leq: 'a -> 'a -> bool) (l: 'a list) =
    let
        fun qs ([], tail) = tail
        |   qs ([h], tail) = h :: tail
        |   qs (h::t, tail) =
            let
                val (after, befor) = List.partition (leq h) t
            in
                qs(befor, h :: qs(after, tail))
            end
    in
        qs(l, [])
    end

    (* Convert a string, recognising and converting escape codes.  This is
       installed as the initial string conversion function and replaced
       when the String structure is compiled.
       We can't use fromString because it stops on invalid input and
       returns as much as it can rather than raising an exception. *)
    fun unescapeString(s: string) : string =
        let
        val len = size s
        fun rdr i =
            if i = len then NONE
            else SOME(String.sub(s, i), i+1)
        (* Repeatedly convert escape sequences and accumulate the
           results in a list. *)
        fun convChars i =
            if i = len then [] (* Finished *)
            else case Char.scan rdr i of
                NONE => (* Bad conversion *)
                    raise Conversion "Invalid string constant"
              | SOME(res, j) => res :: convChars j
        in
            String.implode(convChars 0)
        end
  
  fun forsucc succ (op <=) =
  let
    fun for first last f =
      if first <= last then (f first; for (succ first) last f) else ();
  in
    for
  end;

  datatype 'a iter = Iter of { continue: bool,
                   value:    unit -> 'a,
                   next:     unit -> 'a iter };
  
  fun Continue (Iter {continue,...}) = continue;
  fun Value    (Iter {value,...})    = value();
  fun Next     (Iter {next,...})     = next();
  
  fun iterList i = if Continue i then Value i :: iterList (Next i) else [];
  
  fun first condition (* The condition to search for *)
        thenpart  (* Executed if found *)
        elsepart  (* Executed if not *) =
  let
    fun search i = if Continue i
           then
             let
               val v = Value i;
             in
               if condition v then thenpart v else search (Next i)
             end
           else elsepart()
  in
    search
  end;
  
  fun for i body = if Continue i then (body (Value i); for (Next i) body) else ();
  
  fun mapIterator     f   i = map     f   (iterList i);
  (*fun revfoldIterator f x i = revfold f x (iterList i);*)
  fun revfoldIterator f x i =
    if Continue i
    then revfoldIterator f (f (Value i) x) (Next i)
    else x
  
  fun upto start finish =
  let
    fun succ n = Iter { continue = n <= finish,
            next     = fn () => succ (n+1),
            value    = fn () => n };
  in
    succ start
  end;
  
  fun downto start finish =
  let
    fun pred n = Iter { continue = n >= finish,
            next     = fn () => pred (n-1),
            value    = fn () => n };
  in
    pred start
  end;

  fun lookupDefault first second = (fn key => case first key of NONE => second key | v => v);

end (* Misc *);
