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

structure Cursor:
  sig
    type HCURSOR and HINSTANCE
    type POINT = { x : int, y: int }
    type RECT =  { left: int, top: int, right: int, bottom: int }
    val hcursorNull : HCURSOR
    val isHcursorNull : HCURSOR -> bool

    datatype
      CursorId =
          OCR_APPSTARTING
        | OCR_CROSS
        | OCR_IBEAM
        | OCR_NO
        | OCR_NORMAL
        | OCR_SIZEALL
        | OCR_SIZENESW
        | OCR_SIZENS
        | OCR_SIZENWSE
        | OCR_SIZEWE
        | OCR_UP
        | OCR_WAIT

    val ClipCursor : RECT -> unit
    val CopyCursor : HCURSOR -> HCURSOR
    val DestroyCursor : HCURSOR -> unit
    val GetClipCursor : unit -> RECT
    val GetCursor : unit -> HCURSOR
    val GetCursorPos : unit -> POINT
    val LoadCursor : HINSTANCE * Resource.RESID -> HCURSOR
    val LoadCursorFromFile : string -> HCURSOR
    val LoadSystemCursor : CursorId -> HCURSOR
    val LoadSystemCursorFromFile : CursorId -> HCURSOR
    val SetCursor : HCURSOR -> HCURSOR
    val SetCursorPos : int * int -> unit
    val SetSytemCursor : HCURSOR * CursorId -> unit
    val ShowCursor : bool -> int
  end
 =
struct
    local
        open CInterface
        open Base
        open Resource
    in
        type HCURSOR = HCURSOR and HINSTANCE = HINSTANCE
        type RECT = RECT and POINT = POINT
        val hcursorNull = hgdiObjNull
        and isHcursorNull = isHgdiObjNull

        datatype CursorId =
            OCR_APPSTARTING     (* Standard arrow and small hourglass *)
        |   OCR_NORMAL          (* Standard arrow *)
        |   OCR_CROSS           (* Crosshair *)
        |   OCR_IBEAM           (* I-beam  *)
        |   OCR_NO              (* Slashed circle *)
        |   OCR_SIZEALL         (* Four-pointed arrow pointing north, south, east, and west *)
        |   OCR_SIZENESW        (* Double-pointed arrow pointing northeast and southwest *)
        |   OCR_SIZENS          (* Double-pointed arrow pointing north and south *)
        |   OCR_SIZENWSE        (* Double-pointed arrow pointing northwest and southeast *)
        |   OCR_SIZEWE          (* Double-pointed arrow pointing west and east *)
        |   OCR_UP              (* Vertical arrow *)
        |   OCR_WAIT            (* Hourglass *)

        local
            fun idToInt OCR_APPSTARTING = 32650
            |   idToInt OCR_NORMAL = 32512
            |   idToInt OCR_CROSS = 32515
            |   idToInt OCR_IBEAM = 32513
            |   idToInt OCR_NO = 32648
            |   idToInt OCR_SIZEALL = 32646
            |   idToInt OCR_SIZENESW = 32643
            |   idToInt OCR_SIZENS = 32645
            |   idToInt OCR_SIZENWSE = 32642
            |   idToInt OCR_SIZEWE = 32644
            |   idToInt OCR_UP = 32516
            |   idToInt OCR_WAIT = 32514

            fun intToId _ = raise Fail "intToId"
        in
            val CURSORID = absConversion {abs=intToId, rep=idToInt} INT
        end

        val SetSytemCursor =
            call2 (user "SetSytemCursor") (HCURSOR, CURSORID) (SUCCESSSTATE "SetSytemCursor")

        fun checkCursor c = (checkResult(not(isHcursorNull c)); c);

        val LoadCursorFromFile =
            checkCursor o
            call1 (user "LoadCursorFromFileA") (STRING) HCURSOR

        (* ML extension - simpler than having a separate function. *)
        val LoadSystemCursorFromFile =
            checkCursor o
            call1 (user "LoadCursorFromFileA") (CURSORID) HCURSOR

        val ClipCursor =
            call1 (user "ClipCursor") (POINTERTO RECT) (SUCCESSSTATE "ClipCursor")

        val CopyCursor =
            checkCursor o
            call1 (user "CopyCursor") (HCURSOR) HCURSOR

        val DestroyCursor =
            call1 (user "DestroyCursor") (HCURSOR) (SUCCESSSTATE "DestroyCursor")

        fun GetClipCursor () =
        let
            val (toRect, _, _) = breakConversion RECT
            val buff = alloc 4 Clong
            val res = call1 (user "GetClipCursor") (POINTER) BOOL (address buff)
        in
            checkResult res;
            toRect buff
        end

        val GetCursor = call0 (user "GetCursor") () HCURSOR

        fun GetCursorPos () =
        let
            val (toPoint, _, _) = breakConversion POINT
            val buff = alloc 2 Clong
            val res = call1 (user "GetCursorPos") (POINTER) BOOL (address buff)
        in
            checkResult res;
            toPoint buff
        end

        val SetCursor = call1 (user "SetCursor") HCURSOR HCURSOR

        val SetCursorPos =
            call2 (user "SetCursorPos") (INT,INT) (SUCCESSSTATE "SetCursorPos")

        val ShowCursor = call1 (user "ShowCursor") BOOL INT

        val LoadCursor =
            checkCursor o
            call2 (user "LoadCursorA") (HINSTANCE, RESID) HCURSOR

        fun LoadSystemCursor(id: CursorId) =
            checkCursor
                ((call2 (user "LoadCursorA") (HINSTANCE, CURSORID) HCURSOR) (hinstanceNull, id))
(*
TODO:
CreateCursor
    a little complicated because it includes bit maps.
*)
    end
end;
