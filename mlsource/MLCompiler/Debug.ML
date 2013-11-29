(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Modified David C.J. Matthews 2008, 2013.

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


(*
    Title:  Control debugging output.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)


structure Debug =

(*****************************************************************************)
(*                  Body of DEBUG                                            *)
(*****************************************************************************)
struct
    local
        open Universal
    in
        (* Get the current line number. *)
        val lineNumberTag: (unit->int) tag           = tag()
        (* Get the current offset (position on line or in file). *)
        val offsetTag: (unit->int) tag               = tag()
        (* File name. *)
        val fileNameTag: string tag                  = tag()

        (* Print times for compilation and execution: default false. *)
        val timingTag: bool tag                      = tag()
        (* Profile executed code?  default 0. *)
        val profilingTag: int tag                    = tag()
        (* How much to print in error messages?  default 6 *)
        val errorDepthTag: int tag                   = tag()
        (* Control print depth in PolyML.print. *)
        val printDepthFunTag: (unit->int) tag            = tag()
        (* Length of line in PolyML.print. error messages etc. *)
        val lineLengthTag: int tag                   = tag()
        (* Compile in debugging code?  default false *)
        val debugTag: bool tag                       = tag()
        (* Compilation fine tuning. *)
        (* Should functors be made inline? default true. *)
        val inlineFunctorsTag: bool tag              = tag()
        (* Control how big functions should be before they're not inlined. *)
        val maxInlineSizeTag: int tag                = tag()
        (* Add profile information to each allocation?  default zero.
           At the moment this is effectively a bool but having an int allows
           for the possibility of recording different information. *)
        val profileAllocationTag: int tag            = tag()

        (* Compiler debugging. *)
        (* Trace exceptions generated within the compiler? default false. *)
        val traceCompilerTag: bool tag               = tag()
        (* Print parsetree after parsing? default false *)
        val parsetreeTag: bool tag                   = tag()
        (* Print codetree after compiling? default false *)
        val codetreeTag: bool tag                    = tag()
        (* Print the optimised code after compiling? default false *)
        val codetreeAfterOptTag: bool tag            = tag()
        (* Print pseudo-stack in code-generator? default false *)
        val pstackTraceTag: bool tag                 = tag()
        (* Switch on/off low-level optimisation. *)
        val lowlevelOptimiseTag: bool tag            = tag()
        (* Print assembly code in code-generator? default false *)
        val assemblyCodeTag: bool tag                = tag()
        (* Report unreferenced identifiers as warnings *)
        val reportUnreferencedIdsTag: bool tag       = tag()
        (* Report catch-all handlers as warnings *)
        val reportExhaustiveHandlersTag: bool tag    = tag()
        (* Use a narrow context to resolve overloading and flexible records. *)
        val narrowOverloadFlexRecordTag: bool tag    = tag()
        
        (* To avoid circularity of dependencies a few tags are defined
           elsewhere:  *)
        (* ValueOps.printSpaceTag: ValueOps.nameSpace tag
           Debugger.debuggerFunTag: Debugger.debugger tag
           Pretty.printOutputTag: (pretty->unit) tag
           Pretty.compilerOutputTag: (pretty->unit) tag
           Lex.errorMessageProcTag: (pretty * bool * int -> unit) tag
           ExportTreeString.rootTreeTag: (unit -> exportTree) tag
         *)


        val defaults =
        [
            tagInject lineNumberTag (fn () => 0), (* Zero line number *)
            tagInject offsetTag (fn () => 0), (* Zero offset *)
            tagInject fileNameTag "",
            tagInject inlineFunctorsTag true,
            tagInject maxInlineSizeTag 80,
            tagInject profileAllocationTag 0,
            tagInject parsetreeTag false,
            tagInject codetreeTag false,
            tagInject pstackTraceTag false,
            tagInject lowlevelOptimiseTag true,
            tagInject assemblyCodeTag false,
            tagInject codetreeAfterOptTag false,
            tagInject timingTag false,
            tagInject profilingTag 0,
            tagInject errorDepthTag 6,
            tagInject printDepthFunTag (fn () => 0),
            tagInject lineLengthTag 77,
            tagInject traceCompilerTag false,
            tagInject debugTag false,
            tagInject reportUnreferencedIdsTag false,
            tagInject reportExhaustiveHandlersTag false,
            tagInject narrowOverloadFlexRecordTag false
        ]
    
        fun getParameter (t:'a tag) (tagList: universal list) :'a =
        case List.find (tagIs t) tagList of
            SOME a => tagProject t a
        |   NONE => (* Use the default *)
            (
                case List.find (tagIs t) defaults of
                    SOME a => tagProject t a
                |   NONE => raise Misc.InternalError "tag missing"
            )
    end
        
end;
