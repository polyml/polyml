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
    
functor VOL_BASIC
    (structure Ctype : CtypeSig
     structure BehaviourRefs : BehaviourRefsSig
     structure Dispatch : DispatchSig

     sharing type Dispatch.RawCtype = Ctype.RawCtype
     ) : VolatileSig =
struct

    structure BehaviourRefs = BehaviourRefs;
    structure Ctype = Ctype
    structure Union = Dispatch.Union;

    open Dispatch
    type vol = rawvol
    open Union
    open Ctype
    
    fun offset i ct v = Dispatch.offset v (i * sizeof ct)
    fun alloc i ct = Dispatch.alloc (i * sizeof ct)
    fun assign ct v w = Dispatch.assign v w (sizeof ct)


    fun ID x = x;
    fun call_sym_and_convert v args rt =
    Dispatch.call_sym_and_convert
        v
        (map (mapDirectedArg makeRaw ID) args)
        (mapUnionChoice makeRaw rt)

    (* These both take an ML function and create a vol.  They can and should
       be applied to the type information before they are applied to the ML
       function.  It actually doesn't help much because
       VOLS_THAT_HOLD_REFS().toCfunction etc sapply them to all the arguments. *)
    fun toCfunction argType resType =
        Dispatch.toCfunction (map makeRaw argType) (makeRaw resType)
    
    fun toPascalfunction argType resType =
        Dispatch.toPascalfunction (map makeRaw argType) (makeRaw resType)

end;
