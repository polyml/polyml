(*
    Copyright (c) 2006-10  David C. J. Matthews

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

PolyML.use "basis/RuntimeCalls";

PolyML.make "mlsource/MLCompiler/Boot";
PolyML.make "mlsource/MLCompiler";

PolyML.shareCommonData MLCompiler.shell;

PolyML.exportPortable("polytemp",
    fn () => (MLCompiler.shell(); OS.Process.exit OS.Process.success) handle _ => OS.Process.exit OS.Process.failure);
