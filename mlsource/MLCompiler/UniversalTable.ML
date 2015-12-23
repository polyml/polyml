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

structure UniversalTable :
sig
  type universal;
  type 'a tag;
  type univTable;
  
  val makeUnivTable:    unit -> univTable;
  
  val univEnter:        univTable * 'a tag * string * 'a -> unit;
  val univLookup:       univTable * 'a tag * string -> 'a option;
  val univDelete:       univTable * 'a tag * string -> unit;

    val fold: (string * universal * 'a -> 'a) -> 'a -> univTable -> 'a
    val app: (string * universal -> unit) -> univTable -> unit
  
  val univFold:         univTable * (string * universal * 'a -> 'a) * 'a -> 'a
  
  (* Freeze a mutable table so it is immutable. *)
  val univFreeze:       univTable -> univTable

end =

struct
    open Misc;
    open HashTable;
    open Universal;

    type universal = universal

    datatype univTable = Table of universal list hash

    fun makeUnivTable() = Table (hashMake 10); (* Choose a number. *)
      
    fun univLookup (Table tab, t:'a tag, name:string) : 'a option =
    let
        fun search []     = NONE
        |   search (H::T) = if tagIs t H then SOME (tagProject t H) else search T;
    in
        case hashSub (tab, name) of
            NONE => NONE
        |   SOME t => search t
    end
      
      fun univEnter (Table tab, t:'a tag, name:string, v:'a) : unit =
      let
        val u = tagInject t v;
        
        (* If there is already an entry we add it to the list,
           otherwise we start a new list. *)
  
        fun search []     = [u]
        |   search (H::T) = if tagIs t H then u :: T else H :: search T;
        
        val newList =
            case hashSub (tab, name) of
                SOME v => search v
            |   NONE => [u]
      in
        hashSet (tab, name, newList)
      end;
      
      fun univDelete (Table tab, t:'a tag, name:string) : unit  =
      let
        fun search []     = []
        |   search (H::T) = if tagIs t H then T else H :: search T;
      in
         case hashSub (tab, name) of
             SOME l => hashSet (tab, name, search l)
         |   NONE => ()
      end;
      

    fun fold f acc (Table table) =
    let
        fun foldHashEntry (s, l, acc) =
            List.foldl (fn (c, a) => f (s, c, a)) acc l
    in
        HashTable.fold foldHashEntry acc table
    end

    fun app f = fold (fn (s, c, ()) => f (s, c)) () 

    (* Backwards compatibility. *)
    fun univFold (t, f, acc) = fold f acc t

    fun univFreeze (Table h) = Table(hashFreeze h)

end (* UniversalTable *);
