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
    val ShowCaret : HWND -> bool
  end
 =
struct
    local
        open CInterface
        open Base
    in
        type HWND = HWND and HBITMAP = HBITMAP and POINT = POINT
        datatype caretShape =
            CaretBlack of {height: int, width: int}
        |   CaretGrey  of {height: int, width: int}
        |   CaretBitmap of HBITMAP

        local
            val createCaret = call4 (user "CreateCaret") (HWND, HBITMAP, INT, INT)
                (SUCCESSSTATE "CreateCaret")
        in
            (* The x and y value are only used if the bitmap is not 0 or 1. *)
            fun CreateCaret(hw, CaretBlack{height, width}) =
                    createCaret(hw, intAsHgdi 0, width, height)
            |   CreateCaret(hw, CaretGrey{height, width}) =
                    createCaret(hw, intAsHgdi 1, width, height)
            |   CreateCaret(hw, CaretBitmap hb) =
                    createCaret(hw, hb, 0, 0)
        end

        val DestroyCaret = call0 (user "DestroyCaret") () (SUCCESSSTATE "DestroyCaret")

        val GetCaretBlinkTime = Time.fromMilliseconds o (call0 (user "GetCaretBlinkTime") () UINT)

        val HideCaret = call1 (user "HideCaret") (HWND) (SUCCESSSTATE "HideCaret")

        val SetCaretBlinkTime =
            (call1 (user "SetCaretBlinkTime") UINT (SUCCESSSTATE "SetCaretBlinkTime")) o
                Time.toMilliseconds

        (* The result of ShowCaret may be false either if there was an error or
           if HideCaret was called more than once. *)
        val ShowCaret = call1 (user "ShowCaret") (HWND) BOOL

        local
            val getCaretPos =
                call1 (user "GetCaretPos") (POINTER) (SUCCESSSTATE "GetCaretPos")
            val setCaretPos =
                call2 (user "SetCaretPos") (INT, INT) (SUCCESSSTATE "SetCaretPos")
            val (fromCpt, _, ptStruct) = breakConversion POINT
        in
            fun GetCaretPos(): POINT =
            let
                val v = alloc 1 ptStruct
                val _: unit = getCaretPos(address v)
            in
                fromCpt v
            end
            and SetCaretPos({x, y}: POINT) = setCaretPos(x, y)
        end
    end
end;
