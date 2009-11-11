(*
    Title:      Standard Basis Library: IO Structure and Signature
    Author:     David Matthews
    Copyright   David Matthews 2000, 2005

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

(* G&R 2004 status: done.  Removed TerminatedStream exception. *)
signature IO =
  sig
    exception Io of {name : string, function : string, cause : exn}
    exception BlockingNotSupported
    exception NonblockingNotSupported
    exception RandomAccessNotSupported
    exception ClosedStream
    datatype buffer_mode = NO_BUF | LINE_BUF | BLOCK_BUF
  end;

structure IO: IO =
  struct
    exception Io of {name : string, function : string, cause : exn}
    exception BlockingNotSupported
    exception NonblockingNotSupported
    exception RandomAccessNotSupported
    exception ClosedStream
    datatype buffer_mode = NO_BUF | LINE_BUF | BLOCK_BUF
  end;
