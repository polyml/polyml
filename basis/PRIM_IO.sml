(*
    Title:      Standard Basis Library: Primitive IO Signature
    Author:     David Matthews
    Copyright   David Matthews 1999, 2005

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

(* G&R 2004 done. *)


signature PRIM_IO =
sig
    type elem
    type vector
    type vector_slice
    type array
    type array_slice
    eqtype pos

    val compare : pos * pos -> General.order

    datatype reader
    = RD of {
        name : string,
        chunkSize : int,
        readVec : (int -> vector) Option.option,
        readArr : (array_slice -> int) Option.option,
        readVecNB : (int -> vector Option.option) Option.option,
        readArrNB : (array_slice -> int Option.option) Option.option,
        block : (unit -> unit) Option.option,
        canInput : (unit -> bool) Option.option,
        avail : unit -> int Option.option,
        getPos : (unit -> pos) Option.option,
        setPos : (pos -> unit) Option.option,
        endPos : (unit -> pos) Option.option,
        verifyPos : (unit -> pos) Option.option,
        close : unit -> unit,
        ioDesc : OS.IO.iodesc Option.option
    }

    datatype writer = WR of {
        name : string,
        chunkSize : int,
        writeVec : (vector_slice -> int) Option.option,
        writeArr : (array_slice -> int) Option.option,
        writeVecNB : (vector_slice -> int Option.option) Option.option,
        writeArrNB : (array_slice -> int Option.option) Option.option,
        block : (unit -> unit) Option.option,
        canOutput : (unit -> bool) Option.option,
        getPos : (unit -> pos) Option.option,
        setPos : (pos -> unit) Option.option,
        endPos : (unit -> pos) Option.option,
        verifyPos : (unit -> pos) Option.option,
        close : unit -> unit,
        ioDesc : OS.IO.iodesc Option.option
    }

    val openVector: vector -> reader
    val nullRd: unit -> reader
    val nullWr: unit -> writer

    val augmentReader : reader -> reader
    val augmentWriter : writer -> writer
end;
