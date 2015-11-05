(*
    Title:      Search for module
    Author:     David Matthews
    Copyright   David Matthews 2015

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

(* We need separate versions of this for Windows and Unix because
   in Windows we want to check in the the registry. *)

(* Unix version *)
structure PolyML =
struct
    open PolyML
    structure SaveState =
    struct
        fun loadModuleBasic (fileName: string): Universal.universal list =
        let
            val sysCall: string -> Universal.universal list =
                fn args => RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (32, args)
        in
            (* If there is a path separator use the name and don't search further. *)
            if CharVector.exists(fn #"/" => true | _ => false) fileName
            then sysCall fileName
            else
            let
                (* Unix path elements are separated by colons. *)
                val pathList =
                    case OS.Process.getEnv "POLYMODPATH" of
                        NONE => []
                    |   SOME s => String.fields (fn #":" => true | _ => false) s

                fun findFile [] = NONE
                |   findFile (hd::tl) =
                    (* Try actually loading the file.  That way we really check we have a module. *)
                    SOME(sysCall (OS.Path.joinDirFile{dir=hd, file=fileName}))
                        handle Fail _ => findFile tl | OS.SysErr _ => findFile tl      
            in
                case findFile pathList of
                    SOME l => l (* Found *)
                |   NONE => raise Fail("Unable to find module " ^ fileName)
            end
        end
    end
end;
 