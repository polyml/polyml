(*
    Title:      Standard Basis Library: Top level make
    Author:     David Matthews
    Copyright   David Matthews 1999-2008

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
(* Several modules are made as a result of being dependencies
   of modules in this list. *)
(* Thread, Weak and Signal are Poly/ML extensions. *)
structure g = General
structure o = Option
structure s = String
structure ss = Substring
(* structure tt = Text*) (* Declares Char, String, CharArray, CharVector *)
structure b = Bool (* Depends on Text *)
structure lp = ListPair
structure i = Int
structure a = Array
structure w = LargeWord
structure w8 = Word8
structure w32 = Word32
structure ii = IntInf
structure i32 = Int32
structure w8a = Word8Array (* Depends on Word8, Word, String, Substring *)
structure by = Byte (* Depends on Word8Array among others. *)
structure bv = BoolArray
structure iv = IntArray
structure rv = RealArray
structure rl = Real (* Depends on IEEEReal*)
structure d = Date (* Depends on Time, Int, String, Char ... *)
structure th = Thread (* Non-standard. May not actually need to include explicitly. *)
structure t = Timer
structure c = CommandLine
structure o = OS (* Depends on Time *)
structure t = TextIO
structure b = BinIO
structure nh = NetHostDB
structure np = NetProtDB
structure ns = NetServDB
structure sk = Socket
structure isk = INetSock
structure usk = UnixSock
structure pr = PackRealBig (* also declares PackRealLittle *)
structure pw = PackWord8Big (* also declares Pack8Little. ...*)
structure a2 = Array2
structure i2 = IntArray2

structure ml90 = SML90

structure we = Weak
structure si = Signal

(* Unix and Windows structures are built separately. *)
in
end;
