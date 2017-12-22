(*
    Copyright (c) 2017 David C.J. Matthews

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

signature CODEARRAYSIG =
sig
    type machineWord = Address.machineWord
    and  address = Address.address
    
    type byteVec and codeVec and closureRef

    val byteVecMake:            word -> byteVec
    val byteVecSet:             byteVec * word * Word8.word -> unit
    val byteVecToCodeVec:       byteVec * closureRef -> codeVec
    
    val codeVecGet:             codeVec * word -> Word8.word
    val codeVecSet:             codeVec * word * Word8.word -> unit
    val codeVecAddr:            codeVec -> address
    val codeVecPutWord:         codeVec * word * machineWord -> unit
    
    datatype constantType = ConstAbsolute | ConstX86Relative
    
    val codeVecPutConstant:     codeVec * word * machineWord * constantType -> unit
    
    val codeVecLock: codeVec * closureRef -> unit
    
    val makeConstantClosure: unit -> closureRef
    val codeAddressFromClosure: closureRef -> machineWord
    val closureAsAddress: closureRef -> machineWord

    structure Sharing:
    sig
        type byteVec = byteVec
        and  codeVec = codeVec
        and  closureRef = closureRef
        and  constantType = constantType
    end

end;
