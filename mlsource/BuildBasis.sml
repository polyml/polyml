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

(* Install initial conversion functions. These can be replaced later on. *)
val () = RunCall.addOverload Bootstrap.convString Bootstrap.convStringName;
val () = RunCall.addOverload Bootstrap.convInt "convInt";
val () = RunCall.addOverload Bootstrap.convWord "convWord";

(* Build the main basis library. *)
val () = Bootstrap.use "basis/build.sml";

(* We've now set up the new name space so everything has to be
   compiled into that rather than the old space. *)
PolyML.make "mlsource/extra/CInterface";
PolyML.use "mlsource/extra/CInterface/clean";

(* XWindows/Motif *)
let
   val xcall: int*int->int*int =
    RunCall.run_call1 RuntimeCalls.POLY_SYS_XWindows;
   (* See if the RTS supports the X GetTimeOfDay call. *)
   val isX = (xcall(30, 0); true) handle _ => false
in
   if isX
   then
    (
    PolyML.make "mlsource/extra/XWindows";
    PolyML.make "mlsource/extra/Motif"
    )
   else ()
end;

PolyML.print_depth 10;
