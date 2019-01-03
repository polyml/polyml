(*
    Copyright (c) 2001, 2015, 2019
        David C.J. Matthews

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License version 2.1 as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

(* Caret functions. *)
structure Caret :
  sig
    type HWND and HBITMAP
    type POINT = { x: int, y: int }
    datatype
      caretShape =
          CaretBitmap of HBITMAP
        | CaretBlack of {width: int, height: int}
        | CaretGrey of {width: int, height: int}
    val CreateCaret : HWND * caretShape -> unit
    val DestroyCaret : unit -> unit
    val GetCaretBlinkTime : unit -> Time.time
    val GetCaretPos : unit -> POINT
    val HideCaret : HWND -> unit
    val SetCaretBlinkTime : Time.time -> unit
    val SetCaretPos : POINT -> unit
    val ShowCaret : HWND -> unit
  end
 =
struct
    local
        open Foreign
        open Base
        open GdiBase
    in
        type HWND = HWND and HBITMAP = HBITMAP and POINT = POINT
        datatype caretShape =
            CaretBlack of {height: int, width: int}
        |   CaretGrey  of {height: int, width: int}
        |   CaretBitmap of HBITMAP

        local
            val createCaret = winCall4 (user "CreateCaret") (cHWND, cHBITMAP, cInt, cInt)
                (successState "CreateCaret")
            val intAsHgdi = handleOfVoidStar o Memory.sysWord2VoidStar o SysWord.fromInt
        in
            (* The x and y value are only used if the bitmap is not 0 or 1. *)
            fun CreateCaret(hw, CaretBlack{height, width}) =
                    createCaret(hw, hNull, width, height)
            |   CreateCaret(hw, CaretGrey{height, width}) =
                    createCaret(hw, intAsHgdi 1, width, height)
            |   CreateCaret(hw, CaretBitmap hb) =
                    createCaret(hw, hb, 0, 0)
        end

        val DestroyCaret = winCall0 (user "DestroyCaret") () (successState "DestroyCaret")

        val GetCaretBlinkTime = Time.fromMilliseconds o LargeInt.fromInt o (winCall0 (user "GetCaretBlinkTime") () cUint)

        val HideCaret = winCall1 (user "HideCaret") (cHWND) (successState "HideCaret")

        val SetCaretBlinkTime =
            (winCall1 (user "SetCaretBlinkTime") cUint (successState "SetCaretBlinkTime")) o
                LargeInt.toInt o Time.toMilliseconds

        (* The result of ShowCaret may be false either if there was an error or
           if HideCaret was called more than once. *)
        val ShowCaret = winCall1 (user "ShowCaret") (cHWND) (successState "ShowCaret")

        local
            val getCaretPos =
                winCall1 (user "GetCaretPos") (cStar cPoint) (successState "GetCaretPos")
            val setCaretPos =
                winCall2 (user "SetCaretPos") (cInt, cInt) (successState "SetCaretPos")
        in
            fun GetCaretPos() = let val v = ref {x=0, y=0 } in getCaretPos v; !v  end
            and SetCaretPos({x, y}: POINT) = setCaretPos(x, y)
        end
    end
end;
