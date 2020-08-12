(*
    Title:      Standard Basis Library: Windows signature and structure
    Author:     David Matthews
    Copyright   David Matthews 2000, 2005, 2012, 2018, 2019

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

signature WINDOWS =
sig
    structure Key :
    sig
        include BIT_FLAGS
        val allAccess : flags
        val createLink : flags
        val createSubKey : flags
        val enumerateSubKeys : flags
        val execute : flags
        val notify : flags
        val queryValue : flags
        val read : flags
        val setValue : flags
        val write : flags
    end
    structure Reg :
    sig
        eqtype hkey
        val classesRoot  : hkey
        val currentUser  : hkey
        val localMachine : hkey
        val users        : hkey
        val performanceData : hkey
        val currentConfig : hkey
        val dynData : hkey
  
        datatype create_result =
              CREATED_NEW_KEY of hkey
            | OPENED_EXISTING_KEY of hkey
        val createKeyEx : hkey * string * Key.flags -> create_result
        val openKeyEx : hkey * string * Key.flags -> hkey
        val closeKey : hkey -> unit
        val deleteKey : hkey * string -> unit
        val deleteValue : hkey * string -> unit
        val enumKeyEx : hkey * int -> string option
        val enumValueEx : hkey * int -> string option
        datatype value =
              SZ of string
            | DWORD of SysWord.word
            | BINARY of Word8Vector.vector
            | MULTI_SZ of string list
            | EXPAND_SZ of string
        val queryValueEx : hkey * string -> value option
        val setValueEx : hkey * string * value -> unit
    end

    structure Config:
    sig
        val platformWin32s : SysWord.word
        val platformWin32Windows : SysWord.word
        val platformWin32NT : SysWord.word
        val platformWin32CE : SysWord.word

        val getVersionEx: unit ->
            { majorVersion: SysWord.word, minorVersion: SysWord.word,
              buildNumber: SysWord.word, platformId: SysWord.word,
              csdVersion: string }

        val getWindowsDirectory: unit -> string
        val getSystemDirectory: unit -> string
        val getComputerName: unit -> string
        val getUserName: unit -> string
    end

    structure DDE :
    sig
        type info
        val startDialog : string * string -> info
        val executeString : info * string * int * Time.time -> unit
        val stopDialog : info -> unit
    end

    val getVolumeInformation :
                string -> {
                            volumeName : string,
                            systemName : string,
                            serialNumber : SysWord.word,
                            maximumComponentLength : int
                          }

    val findExecutable : string -> string option
    val launchApplication : string * string -> unit
    val openDocument : string -> unit
    val simpleExecute : string * string -> OS.Process.status
    type ('a,'b) proc
    val execute : string * string -> ('a, 'b) proc
    val textInstreamOf : (TextIO.instream, 'a) proc -> TextIO.instream
    val binInstreamOf  : (BinIO.instream, 'a) proc -> BinIO.instream
    val textOutstreamOf : ('a, TextIO.outstream) proc -> TextIO.outstream
    val binOutstreamOf  : ('a, BinIO.outstream) proc -> BinIO.outstream
    val reap : ('a, 'b) proc -> OS.Process.status

    structure Status :
    sig
        type status = SysWord.word
        val accessViolation        : status
        val arrayBoundsExceeded    : status
        val breakpoint             : status
        val controlCExit           : status
        val datatypeMisalignment   : status
        val floatDenormalOperand   : status
        val floatDivideByZero      : status
        val floatInexactResult     : status
        val floatInvalidOperation  : status
        val floatOverflow          : status
        val floatStackCheck        : status
        val floatUnderflow         : status
        val guardPageViolation     : status
        val integerDivideByZero    : status
        val integerOverflow        : status
        val illegalInstruction     : status
        val invalidDisposition     : status
        val invalidHandle          : status
        val inPageError            : status
        val noncontinuableException: status
        val pending                : status
        val privilegedInstruction  : status
        val singleStep             : status
        val stackOverflow          : status
        val timeout                : status
        val userAPC                : status
    end
    val fromStatus : OS.Process.status -> Status.status
    val exit : Status.status -> 'a

end;

(* Provide an empty version for bootstrapping.  It uses the FFI but that has changed. *)

structure Windows = struct end;
