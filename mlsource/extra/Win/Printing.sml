(*
    Copyright (c) 2001, 2015
        David C.J. Matthews

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
        open Foreign Base
    in
        type HDC = HDC
        type DOCINFO = { docName: string, output: string option, dType: string option}

        (* PRINTING AND SPOOLING. *)
        local
            val DOCINFO = cStruct5(cInt, cString, STRINGOPT, STRINGOPT, cDWORDw)
            val {ctype={size=sizeDI, ...}, ...} = breakConversion DOCINFO
            val startdoc = call2(gdi "StartDocA")(cHDC, DOCINFO) cInt
        in
            
            fun StartDoc(hdc: HDC, {docName, output, dType}): int =
            let
                val res = startdoc(hdc, (Word.toInt sizeDI, docName, output, dType, 0w0))
            in
                checkResult(res > 0);
                res
            end
        end

        local
            fun checkSuccess res = checkResult(res > 0)
        in
            val EndDoc      = checkSuccess o call1(gdi "EndDoc") cHDC cInt
            val StartPage   = checkSuccess o call1(gdi "StartPage") cHDC cInt
            val EndPage     = checkSuccess o call1(gdi "EndPage") cHDC cInt
            val AbortDoc    = checkSuccess o call1(gdi "AbortDoc") cHDC cInt
        end

        datatype WMPrintOption = datatype Message.WMPrintOption

        (*
        Other printing functions:
            DeviceCapabilities  
            Escape  
            ExtEscape  
            SetAbortProc  
        *)

    end
end;
