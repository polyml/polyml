(*
    Title:      Foreign Function Interface: main part
    Author:     David Matthews
    Copyright   David Matthews 2015-16, 2018, 2020

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

(* A subset of the main Foreign structure for booting.  We need
   memoise in the interpreter. *)

structure Foreign =
struct
    exception Foreign = RunCall.Foreign
    
    structure Memory :>
    sig
        eqtype volatileRef
        val volatileRef: SysWord.word -> volatileRef
        val setVolatileRef: volatileRef * SysWord.word -> unit
        val getVolatileRef: volatileRef -> SysWord.word
        
        eqtype voidStar
        (* Remember an address except across loads. *)
        val memoise: ('a -> voidStar) ->'a -> unit -> voidStar
    end
    =
    struct
        open ForeignConstants
    
        (* Both volatileRef and SysWord.word are the ADDRESSes of the actual value. *)
        type volatileRef = word ref

        val memMove: SysWord.word * SysWord.word * word * word* word -> unit = RunCall.moveBytes
   
        fun volatileRef init =
        let
            (* Allocate a single word marked as mutable, weak, no-overwrite, byte. *)
            (* A weak byte cell is cleared to zero when it is read in either from the
               executable or from a saved state.  Using the no-overwrite bit ensures
               that if it is contained in the executable it won't be changed by loading
               a saved state but there's a problem if it is contained in a parent state.
               Then loading a child state will clear it because we reload all the parents
               when we load a child. *)
            val v = RunCall.allocateWordMemory(sysWordSize div wordSize, 0wx69, 0w0)
            (* Copy the SysWord into it. *)
            val () = memMove(init, RunCall.unsafeCast v, 0w0, 0w0, sysWordSize)
        in
            v
        end

        fun setVolatileRef(v, i) = memMove(i, RunCall.unsafeCast v, 0w0, 0w0, sysWordSize)

        fun getVolatileRef var =
        let
            (* Allocate a single word marked as mutable, byte. *)
            val v = RunCall.allocateByteMemory(sysWordSize div wordSize, 0wx41)
            val () = memMove(RunCall.unsafeCast var, v, 0w0, 0w0, sysWordSize)
            val () = RunCall.clearMutableBit v
        in
            v
        end

        type voidStar = SysWord.word

        fun 'a memoise(f: 'a -> voidStar) (a: 'a) : unit -> voidStar =
        let
            (* Initialise to zero.  That means the function won't be
               executed until we actually want the result. *)
            val v = volatileRef 0w0
        in
            (* If we've reloaded the volatile ref it will have been reset to zero.
               We need to execute the function and set it. *)
            fn () => (case getVolatileRef v of 0w0 => let val r = f a in setVolatileRef(v, r); r end | r => r)
        end
    end
end;
