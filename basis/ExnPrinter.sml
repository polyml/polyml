(*
    Title:      Install a pretty printer for the exn type
    Author:     David Matthews
    Copyright   David Matthews 2009

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
local
    open PolyML
    open RunCall
    (* Print exception packet. Run-time system exceptions have
       to be processed specially because the IDs don't have printer functions. *)
    fun exnPrint depth _ exn =
    let
        val (exnId, exnName, exnArg, exnLoc) = unsafeCast exn
        fun nullaryException s = PrettyString s
        and parameterException(s, param) =
            PrettyBlock(1, false, [],
                [
                    PrettyString s,
                    PrettyBreak(1, 1),
                    PrettyString "of",
                    PrettyBreak(1, 1),
                    param
                ])
        fun stringException(s, arg) = parameterException(s, PrettyString arg)
    in
        if run_call1 RuntimeCalls.POLY_SYS_is_short exnId
        then
            case exn of
                Conversion s => stringException(exnName, s)
            |   Fail s => stringException(exnName, s)
            |   Foreign s => stringException(exnName, s)
            |   Thread s => stringException(exnName, s)
            |   XWindows s => stringException(exnName, s)
            |   OS.SysErr(s, err) =>
                    parameterException("SysErr",
                        PrettyBlock(1, false, [],
                            [
                                PrettyString "(",
                                PrettyBreak(0, 0),
                                PrettyString s,
                                PrettyBreak(0, 0),
                                PrettyString ",",
                                PrettyBreak(1, 0),
                                (
                                case err of
                                    NONE => PrettyString "NONE"
                                |   SOME syserr =>
                                    PrettyBlock(1, false, [],
                                            [
                                                PrettyString "SOME",
                                                PrettyBreak(1, 0),
                                                PrettyString(OS.errorName syserr)
                                            ]
                                        )
                                ),
                                PrettyBreak(0, 0),
                                PrettyString ")"
                            ]
                        )
                    )
            |   exn => (* Anything else is nullary. *)
                    nullaryException exnName
        else 
            (
                (* Exceptions generated within ML contain a printer function. *)
                case !exnId of
                    NONE => nullaryException exnName
                |   SOME printFn => parameterException(exnName, printFn(exnArg, depth-1))
            )
    end
in
    val () = addPrettyPrinter exnPrint
end
