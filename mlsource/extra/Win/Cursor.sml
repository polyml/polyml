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
    val SetSystemCursor : HCURSOR * CursorId -> unit
    val ShowCursor : bool -> int
  end
 =
struct
    local
        open Foreign
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
            val CURSORID = absConversion {abs=intToId, rep=idToInt} cDWORD
        end

        val SetSystemCursor =
            winCall2 (user "SetSystemCursor") (cHCURSOR, CURSORID) (successState "SetSystemCursor")

        fun checkCursor c = (checkResult(not(isHcursorNull c)); c)

        val LoadCursorFromFile =
            checkCursor o
            winCall1 (user "LoadCursorFromFileA") (cString) cHCURSOR

        (* ML extension - simpler than having a separate function. *)
        (* I found a note suggesting that it was better to use the Unicode version
           because not all implementations handle this properly. *)
        val LoadSystemCursorFromFile =
            checkCursor o
            winCall1 (user "LoadCursorFromFileW") (CURSORID) cHCURSOR

        val ClipCursor =
            winCall1 (user "ClipCursor") (cConstStar cRect) (successState "ClipCursor")

        val CopyCursor =
            checkCursor o
            winCall1 (user "CopyCursor") (cHCURSOR) cHCURSOR

        val DestroyCursor =
            winCall1 (user "DestroyCursor") (cHCURSOR) (successState "DestroyCursor")

        local
            val getClipCursor =
                winCall1 (user "GetClipCursor") (cStar cRect) (successState "GetClipCursor")
        in
            fun GetClipCursor (): RECT =
            let
                val r = ref { top = 0, bottom = 0, left = 0, right = 0 }
            in
                getClipCursor r;
                !r
            end
        end

        val GetCursor = winCall0 (user "GetCursor") () cHCURSOR

        local
            val getCursorPos =
                winCall1 (user "GetCursorPos") (cStar cPoint) (successState "GetCursorPos")
        in
            fun GetCursorPos (): POINT =
            let
                val r = ref { x = 0, y = 0 }
            in
                getCursorPos r;
                !r
            end
        end

        val SetCursor = winCall1 (user "SetCursor") cHCURSOR cHCURSOR

        val SetCursorPos =
            winCall2 (user "SetCursorPos") (cInt, cInt) (successState "SetCursorPos")

        val ShowCursor = winCall1 (user "ShowCursor") cBool cInt

        (* Superseded by LoadImage *)
        val LoadCursor =
            checkCursor o
                winCall2 (user "LoadCursorA") (cHINSTANCE, cRESID) cHCURSOR

        local
            val loadCursor =
                checkCursor o winCall2 (user "LoadCursorA") (cHINSTANCE, CURSORID) cHCURSOR
        in
            fun LoadSystemCursor(id: CursorId) = loadCursor(hinstanceNull, id)
        end
(*
TODO:
CreateCursor
    a little complicated because it includes bit maps.
*)
    end
end;
