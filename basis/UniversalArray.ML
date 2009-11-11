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

structure UniversalArray :>
sig
  type universal = Universal.universal;
  type 'a tag = 'a Universal.tag;
  type univArray;

  val univArray: int -> univArray;
  
  val update:    univArray * 'a tag * string * 'a -> unit;
  val sub:       univArray * 'a tag * string -> 'a option;
  val delete:    univArray * 'a tag * string -> unit;
  val fold:      (string * universal * 'a -> 'a) -> 'a -> univArray-> 'a;

end =

struct
    open HashArray;
    open Universal;

    type universal = universal
    type 'a tag = 'a tag;
  
    datatype univArray = Table of universal list hash

    fun univArray n = Table (hash n);
    
    fun op sub (Table tab, t:'a tag, name:string) : 'a option =
    let
        fun search []     = NONE
        |   search (H::T) = if tagIs t H then SOME (tagProject t H) else search T;
    in
        case HashArray.sub (tab, name) of
            NONE => NONE
        |   SOME t => search t
    end;
    
    fun update (Table tab, t:'a tag, name:string, v:'a) : unit =
    let
        val u = tagInject t v;
        
        (* If there is already an entry we add it to the list,
           otherwise we start a new list. *)
        
        fun search []     = [u]
        |   search (H::T) = if tagIs t H then u :: T else H :: search T;
        
        val newList =
            case HashArray.sub (tab, name) of
                SOME v => search v
            |   NONE => [u]
    in
        HashArray.update (tab, name, newList)
    end;
      
    fun delete (Table tab, t:'a tag, name:string) : unit  =
    let
        fun search []     = []
        |   search (H::T) = if tagIs t H then T else H :: search T;
    in
        case HashArray.sub (tab, name) of
            SOME l => HashArray.update (tab, name, search l)
        |   NONE => ()
    end;

    fun fold f init (Table t) =
         HashArray.fold (fn (s, l, i) => List.foldl (fn (x,y) => f(s, x, y)) i l) init t

end (* UniversalArray *);
