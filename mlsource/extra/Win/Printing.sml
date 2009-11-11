(*
    Copyright (c) 2001
        David C.J. Matthews

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

structure Printing :
  sig
    type HDC
    type DOCINFO = { docName: string, output: string option, dType: string option}

    val StartDoc : HDC * DOCINFO -> int
    val StartPage : HDC -> unit
    val AbortDoc : HDC -> unit
    val EndDoc : HDC -> unit
    val EndPage : HDC -> unit

    datatype WMPrintOption = 
        PRF_CHECKVISIBLE | PRF_NONCLIENT | PRF_CLIENT | PRF_ERASEBKGND |
        PRF_CHILDREN | PRF_OWNED
  end =
struct
    local
        open CInterface Base
    in
        type HDC = HDC
        type DOCINFO = { docName: string, output: string option, dType: string option}

        (* PRINTING AND SPOOLING. *)
        fun StartDoc(hdc: HDC, info: DOCINFO as {docName, output, dType}): int =
        let
            val DOCINFO = STRUCT5(INT, STRING, STRINGOPT, STRINGOPT, WORD)
            val (_,_,dinfo) = breakConversion DOCINFO
            val startdoc = call2(gdi "StartDocA")(HDC, DOCINFO) INT
            val res = startdoc(hdc, (sizeof dinfo, docName, output, dType, 0w0))
        in
            checkResult(res > 0);
            res
        end

        local
            fun checkSuccess res = checkResult(res > 0)
        in
            val EndDoc      = checkSuccess o call1(gdi "EndDoc") HDC INT
            val StartPage   = checkSuccess o call1(gdi "StartPage") HDC INT
            val EndPage     = checkSuccess o call1(gdi "EndPage") HDC INT
            val AbortDoc    = checkSuccess o call1(gdi "AbortDoc") HDC INT
        end

        datatype WMPrintOption = datatype MessageBase.WMPrintOption

        (*
        Other printing functions:
            DeviceCapabilities  
            Escape  
            ExtEscape  
            SetAbortProc  
        *)

    end
end;
