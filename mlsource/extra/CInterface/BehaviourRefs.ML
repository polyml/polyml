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

structure BehaviourRefs : BehaviourRefsSig = struct

(**********************************************************************
 * Couple of refs to control behaviour.
 **********************************************************************)

(***
 Quantity of creation info strored:
 If this is too big, space usage will increase causing excessive GC.
 At the moment, we can only recreate (reload) symbols, so most of the
 time this info is only used for debugging.
***) 
val max_volatile_creation_info_depth = ref 5;

(***
 If true, we print a message to inform the user that
 a symbol is being reloaded.
 This is probably only useful as debuging info.
***) 
val inform_about_symbol_reload = ref false;


end;
