(*
    Title:      Standard Basis Library: STREAM_IO signature
    Copyright   David C.J. Matthews 2000

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

(* G&R 2004 status: checked.  No change *)

signature STREAM_IO =
sig
    type elem
    type vector
    type instream
    type outstream
    type out_pos
    type reader
    type writer
    type pos
    val input : instream -> vector * instream
    val input1 : instream -> (elem * instream) option
    val inputN : instream * int -> vector * instream
    val inputAll : instream -> vector * instream
    val canInput : instream * int -> int option
    val closeIn : instream -> unit
    val endOfStream : instream -> bool
    val output : outstream * vector -> unit
    val output1 : outstream * elem -> unit
    val flushOut : outstream -> unit
    val closeOut : outstream -> unit
    val mkInstream : reader * vector -> instream
    val getReader : instream -> reader * vector
    val filePosIn : instream -> pos
    val setBufferMode : outstream * IO.buffer_mode -> unit
    val getBufferMode : outstream -> IO.buffer_mode
    val mkOutstream : writer * IO.buffer_mode -> outstream
    val getWriter : outstream -> writer * IO.buffer_mode
    val getPosOut : outstream -> out_pos
    val setPosOut : out_pos -> outstream
    val filePosOut : out_pos -> pos
end;
