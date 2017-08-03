(*
    Copyright (c) 2017 David C.J. Matthews
    
    Copyright (c) 2000
        Cambridge University Technical Services Limited

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License version 2.1 as published by the Free Software Foundation
    version 2.1 of the License.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

structure CODE_ARRAY :
sig
    type machineWord
    type address
    type cseg

    val csegMake:          word  -> cseg
    val csegConvertToCode: cseg -> cseg
    val csegGet:           cseg * word -> Word8.word
    val csegSet:           cseg * word * Word8.word -> unit
    val csegGetWord:       cseg * word -> machineWord
    val csegPutWord:       cseg * word * machineWord -> unit
    val csegAddr:          cseg -> address
    val csegLockAndGetExecutable: cseg -> address
    val csegPutConstant:   cseg * word * machineWord * word -> unit
end = 
struct
    open Address
    open Misc

    datatype csegStatus =
        Bytes
    |   UnlockedCode

    datatype cseg = CodeArray of { status: csegStatus, vector: address }

    val objLength: address -> word = length

    val F_mutable_bytes =  Word.fromLargeWord(Word8.toLargeWord(Word8.orb (F_mutable, F_bytes)))
    val wordSize = Word.fromInt Address.wordSize
    
    fun allocateAndClear size =
    let
        val vec : address = RunCall.allocateByteMemory(size, F_mutable_bytes)
        (* allocateByteMemory does not clear the area.  We have to do that at least
           to ensure that the constant area is cleared before we copy it into a
           real code area.  In many cases we could get away with clearing less
           but for the moment this is the safest way. *)
        val byteLength = size * wordSize
        fun clear n =
            if n < byteLength then (assignByte(vec, n, 0w0); clear (n+0w1)) else ()
        val () = clear 0w0
    in
        vec
    end

    fun csegMake (size : word) : cseg = CodeArray{status=Bytes, vector=allocateAndClear size}

    local
        (* These use specific C functions to do the work. *)
        val polySpecificGeneralCall = RunCall.rtsCallFull2 "PolySpecificGeneral"
    in
        fun csegConvertToCode(CodeArray{status=Bytes, vector=addr}) =
            let
                (* Convert the byte data into code. *)
                val newAddr = polySpecificGeneralCall (107, addr)
            in
                CodeArray{status=UnlockedCode, vector=newAddr}
            end
        |   csegConvertToCode _ = raise InternalError "csegConvertToCode: not a byte segment"

        and csegLockAndGetExecutable (CodeArray{status=UnlockedCode, vector=addr}): address =
            (* Lock the code and return the executable address.  This currently just returns
               the original address. *)
            polySpecificGeneralCall (106, addr)

        |   csegLockAndGetExecutable _ = raise InternalError "csegLockAndGetExecutable: not an unlocked code segment"
    end
 
    fun csegAddr(CodeArray{status=UnlockedCode, vector}) = vector
    |   csegAddr(CodeArray{status=Bytes, ...}) = raise InternalError "csegAddr: not a code segment"
  
    (* Returns a value from the vector. *)
    fun csegGet (CodeArray{vector=addr, ...}, byteIndex : word) : Word8.word =
    let
        val lengthWords = objLength addr
        val lengthBytes = wordSize * lengthWords
    in
        if byteIndex < lengthBytes
        then loadByte (addr, byteIndex)
        else raise Subscript
    end
       
    fun csegSet (CodeArray{vector=addr, ...}, byteIndex, value: Word8.word) : unit =
        let
            val lengthWords = objLength addr
            val lengthBytes = wordSize * lengthWords
        in
            if byteIndex < lengthBytes then assignByte (addr, byteIndex, value)
            else raise Subscript
     end

    (* Puts in a word at a given word offset. Does not expand the segment. *)
    fun csegPutWord (CodeArray{status=UnlockedCode, vector=addr}, wordIndex, value:machineWord) : unit =
    let
        val lengthWords = objLength addr
    in
        if lengthWords <= wordIndex
        then raise Subscript
        else assignWord (addr, wordIndex, value)
    end
    | csegPutWord _ = raise InternalError "csegPutWord: not an unlocked code segment"
  
    (* Gets a word at a given word offset. Does not expand the segment. *)
    fun csegGetWord (CodeArray{status=UnlockedCode, vector=addr}, wordIndex) : machineWord =
    let
        val lengthWords = objLength addr
    in
        if lengthWords <= wordIndex
        then raise Subscript
        else loadWord (addr, wordIndex)
    end
    | csegGetWord (CodeArray{status=Bytes, ...}, _)  =
            raise InternalError "csegGetWord: can't load a word from a byte segment"

    local
        val setCodeConstantCall = RunCall.rtsCallFast4 "PolySetCodeConstant"
    in
        (* csegPutConstant: store a constant into the code. *)
        fun csegPutConstant (CodeArray{status=UnlockedCode, vector=addr}, byteIndex, value:machineWord, option: word) : unit =
        let
            val lengthWords = objLength addr
            val lengthBytes = wordSize * lengthWords
        in
            if lengthBytes <= byteIndex
            then raise Subscript
            else setCodeConstantCall(addr, byteIndex, value, option)
        end
        | csegPutConstant _ = raise InternalError "csegPutConstant: not an unlocked code segment"
    end

end; (* CODE_SEG *)
