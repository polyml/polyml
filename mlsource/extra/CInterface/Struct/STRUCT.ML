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

(**********************************************************************
 *
 *  Functor Definition
 *
 **********************************************************************)

functor STRUCT
    (structure LowerLevel : LowerLevelSig)
     : StructSig =
struct

open LowerLevel
open Ctype
    
local
fun offsets _   []          = []
  | offsets acc (t::ts)     = let val pos = align acc t
                  in pos :: offsets (pos + sizeof t) ts
                  end
in val offsets = offsets 0
end


exception Zip;
fun zip [] [] = []
  | zip (x::xs) (y::ys) = (x,y) :: zip xs ys
  | zip _ _ = raise Zip;
    

fun unzip [] = ([],[])
  | unzip ((x,y)::xys) = let val (xs,ys) = unzip xys
             in (x::xs,y::ys)
             end;


fun assigns (t::ts) (l::ls) (r::rs) = (assign t l r; assigns ts ls rs)
  | assigns _ _ _ = ();
    
fun make_struct xs =
    let
    val (types,values) = unzip xs
    val space = alloc 1 (Cstruct types)
    val locations = map (fn n => offset n Cchar space) (offsets types)
    in
    assigns types locations values;
    space
    end


fun break_struct types v =
    map (fn n => offset n Cchar v) (offsets types)



end (* struct *)

