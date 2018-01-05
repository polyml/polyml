(*
    Copyright (c) 2017 David C.J. Matthews
    
    Copyright (c) 2000
        Cambridge University Technical Services Limited

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

structure CODE_ARRAY :> CODEARRAYSIG = 
struct
    open Address
    open Misc

    datatype csegStatus =
        Bytes
    |   UnlockedCode

    type byteVec = address
    and codeVec = address
    and closureRef = address

    val objLength: address -> word = length

    val F_mutable_bytes =  Word.fromLargeWord(Word8.toLargeWord(Word8.orb (F_mutable, F_bytes)))

    fun makeConstantClosure (): closureRef =
    let
        open Address
    in
        if nativeWordSize <> wordSize
        then (* 32-in-64 *) allocWordData(nativeWordSize div wordSize, Word8.orb(F_mutable, F_closure), toMachineWord 0w0)
        else allocWordData(0w1, Word8.orb(F_mutable, F_words), toMachineWord 0w0)
    end

    fun codeAddressFromClosure closure =
        if nativeWordSize <> wordSize
        then raise InternalError "codeAddressFromClosure" (* Not valid in 32-in-64 *)
        else loadWord(closure, 0w0)
    
    fun closureAsAddress closure = toMachineWord closure

    fun byteVecMake size =
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

    (* codeVec is a way of referring to the code in a mutable form.
       We now use the closure itself.  *)

    local
        val byteVecToClosure = RunCall.rtsCallFull2 "PolyCopyByteVecToClosure"
    in
        fun byteVecToCodeVec(bvec, closure) =
        (
            byteVecToClosure (bvec, closure);
            closure
        )
    end
    
    local
        val cvecLock = RunCall.rtsCallFull1 "PolyLockMutableClosure"
    in
        fun codeVecLock(_, closure) = cvecLock closure
    end

    (* Return the address of the segment. Used when putting in "self" addresses.
       Only used in native 32-bit where we don't have relative addresses. *)
    val codeVecAddr = toAddress o codeAddressFromClosure
 
    (* Set a byte. Used when setting the byte data. *)
    fun byteVecSet (addr, byteIndex, value: Word8.word) =
    let
        val lengthWords = objLength addr
        val lengthBytes = wordSize * lengthWords
    in
        if byteIndex < lengthBytes then assignByte (addr, byteIndex, value)
        else raise Subscript
    end

    val codeVecGet = RunCall.rtsCallFast2 "PolyGetCodeByte"
    and codeVecSet = RunCall.rtsCallFast3 "PolySetCodeByte"

    datatype constantType = ConstAbsolute | ConstX86Relative

    local
        val setCodeConstantCall = RunCall.rtsCallFast4 "PolySetCodeConstant"
    in
        (* Store a constant into the code.  This must be used if the constant is
           not on a word boundary or if it needs special treatment. *)
        fun codeVecPutConstant (addr, byteIndex, value:machineWord, option: constantType) =
        let
            val optValue =
                case option of ConstAbsolute => 0w0 | ConstX86Relative => 0w1
        in
            setCodeConstantCall(addr, byteIndex, value, optValue)
        end
        
        fun codeVecPutWord(addr, wordIndex, value) =
            codeVecPutConstant(addr, wordIndex * wordSize, value, ConstAbsolute)
    end

    structure Sharing =
    struct
        type byteVec = byteVec
        and  codeVec = codeVec
        and  closureRef = closureRef
        and  constantType = constantType
    end

end;
