(*
	Copyright (c) 2006  David C. J. Matthews

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

(* Compiler the compiler and export it as a portable file.  The rest of the
   bootstrap process is then done when the portable file is imported. *)

PolyML.print_depth 0;

PolyML.use "mlsource/prelude/RuntimeCalls";
PolyML.use "mlsource/prelude/Address";

PolyML.make "mlsource/MLCompiler/Boot";
PolyML.make "mlsource/MLCompiler";

(* Set the debug level to 50 while compiling the basis.  This makes
   sure that String.foldr can be expanded inline which reduces the
   garbage in the hashValue function. *)
Debug.maxInlineSize := 50;

(* Set the current stdOut to unbuffered before we save the heap. stdOut
   will be replaced when we compile the basis library but if we don't do
   this we end up with extra prompts when we first start. *)
TextIO.StreamIO.setBufferMode(TextIO.getOutstream TextIO.stdOut, IO.NO_BUF);

PolyML.shareCommonData MLCompiler.shell;

let
   val architecture = PolyML.architecture();
   val arch =
      if architecture <> "Interpreted"
	  then String.map Char.toLower architecture
	  else if Address.wordSize = 8
	  then "int64"
	  else "int";	  
   val name = "imports/polyml" ^ arch
in
   PolyML.exportPortable(name, MLCompiler.shell)
end;
