(*
	Copyright (c) 2000
		Cambridge University Technical Services Limited

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

(* Structure equality function.  This is the fall-back when it is not
   possible to a create type-specific equality function.
   
   It would be preferable to have this as an RTS function since that would
   allow calls to avoid tupling the arguments.  The problem is that structure
   equality requires an unbounded amount of stack space and that isn't
   possible in the RTS. 
*)

structure StructureEquality:
sig
    type machineWord = Address.machineWord
    val structureEq: machineWord * machineWord -> bool;
end
=
struct
    open Address
    val andb = Word8.andb and orb = Word8.orb
    infix 6 andb;
    infix 7 orb;
    val objLength = Address.length;
  
   (* Compare two values for equality of their structures. Values with
       the mutable bit set are assumed to be references and are only equal
       if they are the same address. We assume that this will not be applied
       to code segments or stacks. The address of this function is put
       in the code to do the equality testing. *)

    fun structureEq (a:machineWord, b:machineWord) =
        if wordEq (a, b) then true
        else 
        (* If either is a short, then they cannot be equal unless a = b. 
           Other values are addresses so we have to use structural
           equality. *)
        
        if isShort a then false else
        if isShort b then false (* nil is a short now SPF 14/7/94 *)
        else
        let
            (* we promise to be very careful! *)
            val toAddress : 'a -> address = unsafeCast;
            val toShort   : 'a -> short   = unsafeCast;
            
            (* Both addresses *)
            val aa  : address = toAddress a;
            val bb  : address = toAddress b;
            val alw : Word.word   = objLength aa;
            val blw : Word.word   = objLength bb;
        in
            if not (wordEq (alw, blw))
            then (* Must be same size and type. *) false
            
            else
            let
                val orFlags: Word8.word = flags aa orb flags bb
            in
                if orFlags andb F_mutable <> 0w0
                (* In ML mutable objects are equal only if addresses are.
                   This avoids the need to check for circularity. *)
                then false
                
                else if orFlags andb F_bytes <> 0w0
                then
                let
                    (* Byte vector. Each value is a byte and treated as equal
                       only if each byte is equal. Can also arise when the garbage
                       collector turns vectors of small integers into byte vectors
                       to save repeated scans. *)
                    (* Would it be more efficient to compare a word at a time?
                       No, because byte-segments contain bit-patterns that 
                       aren't proper "words"  SPF 14/7/94 *)
                    fun compBytes i finish =
                    if i = finish then true
                    else loadByte (aa, i) = loadByte (bb, i) andalso compBytes (i + 0w1) finish;
                in
                    compBytes 0w0 (*(alw * Word.fromInt wordSize) *)
                        (* Temporary word-around for a bug in the i386 code-generator involving
                           word multiplication by 4. *)
                            (Word.fromInt(Word.toInt alw * wordSize))
                end
                
                else if alw = 0w1
                then structureEq (loadWord (aa, 0w0), loadWord (bb, 0w0))
                
                else if alw = 0w2
                then
                let
                    (* Most objects are list cells or tagged values. *)
                    (* We want to avoid recursing if we can easily see that the
                       objects differ. Short integers differing mean that the
                       objects do differ, otherwise if they are different pointers
                       we have to follow them. *)
                    val a0 : machineWord = loadWord (aa, 0w0);
                    val a1 : machineWord = loadWord (aa, 0w1);
                    val b0 : machineWord = loadWord (bb, 0w0);
                    val b1 : machineWord = loadWord (bb, 0w1);
                in
                    if isShort a0 
                    then wordEq (a0, b0) andalso structureEq (a1, b1)
                    
                    else if isShort b0
                    then false
                    
                    else if isShort a1
                    then wordEq (a1, b1) andalso structureEq (a0, b0)
                    
                    else if isShort b1
                    then false
                    
                    else structureEq (a0, b0) andalso structureEq (a1, b1)
                end
                
                else
                let
                    (* Larger objects. *)
                    (* Compare the words of the vector.  Work from the end back - this
                    is more efficient for tagged values. *)
                    (* compWords is compiled down to a loop to check all but the first word. *)
                    fun compWords 0w0 = true
                    |   compWords i =
                        structureEq (loadWord (aa, i), loadWord (bb, i))
                             andalso compWords (i - 0w1);
                in
                    compWords (alw - 0w1) andalso
                    (* Tail recurse on last entry. *)
                    structureEq (loadWord (aa, 0w0), loadWord (bb, 0w0))
                end
            end
        end (* structureEq *);

end; (* StructureEquality. *)
