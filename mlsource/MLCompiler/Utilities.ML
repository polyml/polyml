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


structure Utilities:
sig
    val noDuplicates: (string * 'a * 'a -> unit) -> 
                       { apply: (string * 'a -> unit) -> unit,
                         enter:  string * 'a -> unit,
                         lookup: string -> 'a option };

    val searchList: unit -> { apply: (string * 'a -> unit) -> unit,
                            enter:  string * 'a -> unit,
                            lookup: string -> 'a option};
    val splitString: string -> { first:string,second:string }
end =
struct
  (* A search list in which an identifier may only be entered once.
     Entering it a second time gives an error message (""fail"" is called) *)
   
    fun noDuplicates fail =
    let
        val variable = ref ([]);

        (* Works down the list from the first declaration to the last, applying
           a function to each element. *)
        fun apply f = List.app  f (!variable)
        (* Add entry to the list if it is not already there. If it is then  call 'fail'. *)

        fun enter (name : string, value) = 
        let
            fun look []         = variable := (name,value) :: !variable
            |   look ((n,old)::T) = if n = name then fail(name, old, value) else look T;
        in
            look (!variable)
        end
   
        (* Look up name *)
        fun lookup (name : string) = Option.map #2 (List.find(fn (n, _) => n = name) (!variable))
    in
        { apply = apply, enter = enter, lookup = lookup }
    end

    fun searchList () =
    let
        val variable = ref [];
    
        (* Works down the list from the first declaration to the last, applying
           a function to each element. *)
        fun apply f = List.app  f (!variable)

        (* Add to list. *)
        fun enter (name : string, value) = variable := (name,value) :: !variable
   
        (* Look up name *)
        fun lookup (name : string) = Option.map #2 (List.find(fn (n, _) => n = name) (!variable))
    in
        { apply = apply, enter = enter, lookup = lookup }
    end (* searchList *)

    fun splitString s =
       (* Divides a name into its components.
         Returns the string split at the last ".". The first part is empty
         if there are no dots in it. *)
    let
        val (first, second) = Substring.splitr(fn c => c <> #".") (Substring.full s)
    in
        if Substring.isEmpty first
        then {first  = "", second = s}
        else {first  = Substring.string(Substring.trimr 1 first),
            second = Substring.string second }
    end

end;
   
