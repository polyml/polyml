(*
    Title:      Standard Basis Library: Commands to build the library
    Copyright   David C.J. Matthews 2000, 2005

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

PolyML.make "basis";

(* Build Windows or Unix structure as appropriate. *)
let
	val getOS: int =
		RunCall.run_call2 RuntimeCalls.POLY_SYS_os_specific (0, 0)
in
	if getOS = 0 then PolyML.make "basis/Unix"
	else if getOS = 1 then PolyML.make "basis/Windows"
	else ()
end;

(* Clean out structures and functors which are only used to build
   the library. *)
PolyML.Compiler.forgetStructure "LibrarySupport";
PolyML.Compiler.forgetStructure "LibraryIOSupport";
PolyML.Compiler.forgetStructure "MachineConstants";
PolyML.Compiler.forgetFunctor "BasicStreamIO";
PolyML.Compiler.forgetFunctor "VectorOperations";
PolyML.Compiler.forgetFunctor "PolyVectorOperations";
PolyML.Compiler.forgetFunctor "VectorSliceOperations";
