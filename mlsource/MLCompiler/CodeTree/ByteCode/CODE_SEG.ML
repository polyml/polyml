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

(*
    Title: 	Code vector operations.
    Author: 	Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)


(*
   This module constructs and operates on segments for machine code.
  "csegMake" constructs and returns a segment of the specified size with
  give flag codes.
  The segment can be read and written a byte at a time using the "csegGet"
  procedure, and the code can be entered using "csegCall" (this is only used
  if the code is an "early" procedure which must be called by the compiler
  or is a declaration or expression at the outer level. More usually
  "csegAddr" is used to return the address of the segment which is then
  compiled into another procedure as a constant.
  When a procedure has been compiled "csegLock" is called to remove the
  "mutable" bit. The segment then becomes a constant and cannot be
  changed.
  It is used in two ways. Initially the code is assembled into a segment which
  expands as necessary so that there is always room for the code. When the
  code has been completely assembled and we know how much there is, it is
  copied into a segment of the correct size. 
*)


structure CODE_SEG :
(*****************************************************************************)
(*                  CODESEG export signature                                 *)
(*****************************************************************************)
(* We use "int" rather than "short" for the indexes, to try to keep *)
(* the interface complexity in this module. SPF 14/7/94             *)
sig
  type machineWord;    (* added 14/7/94 SPF *)
  type short;   (* added 15/7/94 SPF *)
  type address; (* added 14/7/94 SPF *)
  type cseg;

  val csegMake:          int  -> cseg;
  val csegConvertToCode: cseg -> unit;
  val csegLock:          cseg -> unit;
  val csegGet:           cseg * int -> Word8.word;
  val csegSet:           cseg * int * Word8.word -> unit;
  val csegGetWord:       cseg * int -> machineWord;
  val csegPutWord:       cseg * int * machineWord -> unit;
  val csegCopySeg:       cseg * cseg * int * int -> unit;
  val csegAddr:          cseg -> address;
  val csegPutConstant:	 cseg * int * machineWord * 'a -> unit;
end = 

(*****************************************************************************)
(*                  CODESEG functor body                                     *)
(*****************************************************************************)
struct
  open Address;
  open Misc;
  
  datatype csegStatus =
    Bytes
  | UnlockedCode
  | LockedCode
  
  type cseg = (csegStatus * address) ref;
  
  fun objLength (a : address) : int = Word.toInt (length a);
  
  val expansionFactor = 3; (* Factor by which to increase size of segment *)

  val F_mutable_bytes = Word8.orb (F_mutable, F_bytes);
  val F_mutable_code  = Word8.orb (F_mutable, F_code);

  fun csegMake (size : int) : cseg =
  let
    val vec : address = alloc (toShort size, F_mutable_bytes, toMachineWord 0);
  in
    ref (Bytes, vec)
  end;

  fun csegConvertToCode (r as ref (Bytes, addr)) : unit = 
  let
    val () = setFlags (addr, F_mutable_code);
  in
    r := (UnlockedCode, addr)
  end
   | csegConvertToCode _ = 
       raise InternalError "csegConvertToCode: not a byte segment";
        
  fun csegLock (r as ref (UnlockedCode, addr)) : unit = 
  let
    val () = setFlags (addr, F_code);
  in
    r := (LockedCode, addr)
  end
   | csegLock _ = 
       raise InternalError "csegConvertToCode: not an unlocked code segment";
   
  fun csegAddr (ref (status, addr)) : address =
    if status = Bytes
    then raise InternalError "csegAddr: not a code segment"
    else addr;
    
  (* The old comment said:
        Copies the first "length" words from the start of "fromVec" to the
        "offset" words in "toVec".
     Which is right, except that it copies the first length BYTES, not words,
     even though "offset" is in words!
  *)
  fun csegCopySeg (ref (Bytes, fromAddr), ref (Bytes, toAddr), lengthBytes : int, offsetWords : int) =
  let
    val fromLengthWords : int = objLength fromAddr;
    val toLengthWords   : int = objLength toAddr;
    
    val fromLengthBytes : int = wordSize * fromLengthWords;
    val toLengthBytes   : int = wordSize * toLengthWords;
    val offsetBytes     : int = wordSize * offsetWords
 
    (* Copy the data a byte at a time (rather than a word) because the
       bytes copied might look like bad addresses.
       
       Note: this makes the rather rash assumption that we're copying
       the "code" part of the cseg, not the "constants" part, which *should*
       be copied a word at a time, since otherwise we could have a garbage
       collection while we're half-way through copying a pointer. We need
       to completely redesign this module to fix this problem. SPF 14/7/94.
       
       The redesign is that you shouldn't put ML constants into a codeseg
       without calling csegConvertToCode first. SPF 13/2/97
    *)
    fun copyBytes (fromPtr:int) (toPtr:int) =
      if fromPtr = lengthBytes then ()
      else let
        val byte = loadByte (fromAddr, toShort fromPtr);
      in
        assignByte (toAddr, toShort toPtr, byte);
        copyBytes (fromPtr + 1) (toPtr + 1)
      end
      
    (* must check:
         0 <= lengthBytes <= fromLengthBytes
         0 <= offsetWords
         offsetBytes + lengthBytes <= toLengthBytes
    *)  
    val maxOffsetBytes = toLengthBytes - lengthBytes;
  in
    (* Check the lengths are in the range. *)
    if lengthBytes < 0 orelse fromLengthBytes < lengthBytes 
    then let
      val msg = 
        concat
          [
            "csegCopySeg: Byte length ",
             Int.toString lengthBytes,
             " out of range 0..", 
             Int.toString fromLengthBytes
          ]
    in
      raise InternalError msg
    end
      
    else if offsetBytes < 0 orelse maxOffsetBytes < offsetBytes
    then let
      val msg = 
        concat
          [
            "csegCopySeg: Byte offset ",
             Int.toString offsetBytes,
             " out of range 0..",
             Int.toString maxOffsetBytes
          ]
    in
      raise InternalError msg
    end      
          
    else copyBytes 0 offsetBytes
  end

  | csegCopySeg _ =
    raise InternalError "csegCopySeg: can only copy between byte segements"
  
  (* Returns a value from the vector. *)
  fun csegGet (ref (_, addr), byteIndex : int) : Word8.word =
  let
    val lengthWords = objLength addr;
    val lengthBytes = wordSize * lengthWords;
  in
    if 0 <= byteIndex andalso byteIndex < lengthBytes
    then loadByte (addr, toShort byteIndex)
    else let
      val msg = 
        concat
          [
            "csegGet: Index ",
             Int.toString byteIndex,
             " out of range 0..", 
             Int.toString lengthBytes
          ]
    in
      raise InternalError msg
    end
  end;
       
  fun csegSet (ref (LockedCode, _), _, _) : unit =
    raise InternalError "csegSet: can't change locked code segement"
   
    | csegSet (r as (ref (status, addr)), byteIndex:int, value:Word8.word) : unit =
  let
    val lengthWords : int = objLength addr;
    val lengthBytes : int = wordSize * lengthWords;
  in
    if byteIndex < 0
      then
      let
        val msg = concat
            [
                "csegSet: Index ",
                Int.toString byteIndex,
                " out of range 0..infinity" 
            ]
      in
        raise InternalError msg
      end
    
    else if byteIndex < lengthBytes then
      assignByte (addr, toShort byteIndex, value)
      
    else if status <> Bytes then
      raise InternalError "csegSet: can't extend code segment"
      
    else let (* length <= byteIndex; construct a new segment*)
      val oldSize : int = lengthWords; (* size in words *)
      val expSize : int = expansionFactor * oldSize;
      
      val wordIndex  : int = byteIndex div wordSize;
      val minSize    : int = wordIndex + 1;
      
      val newSize    : int = Int.max(expSize, minSize);
      
      (* create temporary codeseg (with larger byte-vector) *)
      val newVec = alloc (toShort newSize, F_bytes, toMachineWord 0);
      val newSeg = (Bytes, newVec);
    in
      (* copy the old code into the new codeseg *)
      csegCopySeg (r, ref newSeg, lengthBytes, 0);
      
      (* assign the byte to the new codeseg's byte-vector *)
      assignByte (newVec, toShort byteIndex, value);

      (* update the old segment to point at the new byte-vector *)
      r := newSeg
    end
  end;

  (* Puts in a word at a given word offset. Does not expand the segment. *)
  fun csegPutWord (ref (UnlockedCode, addr), wordIndex:int, value:machineWord) : unit =
  let
    val lengthWords : int = objLength addr;
  in
    if wordIndex < 0 orelse lengthWords <= wordIndex
    then let
      val msg = 
	concat
	  [
            "csegPutWord: Index ",
             Int.toString wordIndex,
             " out of range 0..", 
             Int.toString lengthWords
          ]
    in
      raise InternalError msg
    end
    else assignWord (addr, toShort wordIndex, value)
  end
    | csegPutWord _ =
        raise InternalError "csegPutWord: not an unlocked code segment"
  
  (* Gets a word at a given word offset. Does not expand the segment. *)
  fun csegGetWord (ref (status, addr), wordIndex : int) : machineWord =
  let
    val lengthWords : int = objLength addr;
  in
    if status = Bytes 
      then raise InternalError "csegGetWord: can't load a a word from a byte segment"
    else if wordIndex < 0 orelse lengthWords <= wordIndex
    then let
      val msg = 
	concat
	  [
            "csegGetWord: Index ",
             Int.toString wordIndex,
             " out of range 0..", 
             Int.toString lengthWords
          ]
    in
      raise InternalError msg
    end
    else loadWord (addr, toShort wordIndex)
  end;

  (* csegPutConstant: store a constant into the code.  Exactly how this is
     done is architecture dependent: on the i386 it's simply a store, on
	 other architectures the constant may have to be split between instructions.
	 This means that it has to be done by the RTS so as to be certain that
	 we don't have a garbage collection with an invalid address.  DCJM 2/1/01. *)
  fun csegPutConstant (ref (UnlockedCode, addr), byteIndex:int,
  					   value:machineWord, data: 'a) : unit =
  let
    val lengthWords : int = objLength addr;
    val lengthBytes : int = wordSize * lengthWords;
  in
    if byteIndex < 0 orelse lengthBytes <= byteIndex
    then let
      val msg = 
		concat
		  [
            "csegPutConstant: Index ", Int.toString byteIndex,
             " out of range 0..",  Int.toString lengthWords
          ]
    in
      raise InternalError msg
    end
    else RunCall.run_call4 RuntimeCalls.POLY_SYS_set_code_constant
			(addr, toShort byteIndex, value, data)
  end
    | csegPutConstant _ =
        raise InternalError "csegPutConstant: not an unlocked code segment"

end; (* CODE_SEG *)
