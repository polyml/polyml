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

(* Install initial conversion functions. These can be replaced later on. *)
PolyML.addOverload PolyML.convString PolyML.convStringName;
PolyML.addOverload PolyML.convInt "convInt";
PolyML.addOverload PolyML.convWord "convWord";

PolyML.Inner.print_depth 1;
PolyML.Inner.use "mlsource/prelude/RunCall";
PolyML.Inner.use "mlsource/prelude/RuntimeCalls";
PolyML.Inner.use "mlsource/prelude/prelude";
PolyML.Inner.use "mlsource/prelude/prelude2";

PolyML.use "mlsource/prelude/Signal";
PolyML.use "mlsource/prelude/Universal";
PolyML.use "mlsource/prelude/processes";

(* Build the main basis library. *)
PolyML.use "basis/build";

PolyML.use "mlsource/prelude/Address";
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
   	(* We have to compile this in ML90 mode because it assumes
   	   that the xcall function can be polymorphic. *)
   	PolyML.Compiler.ml90 := true;
   	PolyML.make "mlsource/extra/XWindows"
   		handle exn => (PolyML.Compiler.ml90 := false; raise exn);
   	PolyML.Compiler.ml90 := false;
   	PolyML.make "mlsource/extra/Motif"
   	)
   else ()
end;

PolyML.print_depth 100;

(* Set the inline level to 40 which seems optimal. *)
PolyML.Compiler.maxInlineSize := 40;

(* Do this last. *)
PolyML.use "mlsource/prelude/ReplaceStdio";
