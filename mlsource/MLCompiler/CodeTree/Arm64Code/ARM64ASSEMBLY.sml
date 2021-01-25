(*
    Copyright (c) 2021 David C. J. Matthews

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    Licence version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public Licence for more details.
    
    You should have received a copy of the GNU Lesser General Public
    Licence along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

functor ARM64ASSEMBLY (
    structure Debug: DEBUG
    and       Pretty: PRETTYSIG
    and       CodeArray: CODEARRAYSIG
) : Arm64Assembly =

struct
    open CodeArray Address
    
    exception InternalError = Misc.InternalError

    infix 5 << <<+ <<- >> >>+ >>- ~>> ~>>+ ~>>- (* Shift operators *)
    infix 3 andb orb xorb andbL orbL xorbL andb8 orb8 xorb8
    
    val op << = Word.<< and op >> = Word.>> and op ~>> = Word.~>>

    val wordToWord8 = Word8.fromLargeWord o Word.toLargeWord
    and word8ToWord = Word.fromLargeWord o Word8.toLargeWord

    datatype code =
    Code of 
    {
        constVec:       machineWord list ref, (* Constant area constant values. *)
        functionName:   string,               (* Name of the function. *)
        printAssemblyCode:bool,               (* Whether to print the code when we finish. *)
        printStream:    string->unit          (* The stream to use *)
    }

    fun codeCreate (name, parameters) = 
    let
        val printStream = Pretty.getSimplePrinter(parameters, [])
    in
        Code
        { 
            constVec         = ref [],
            functionName     = name,
            printAssemblyCode = Debug.getParameter Debug.assemblyCodeTag parameters,
            printStream    = printStream
        }
    end

    val retCode = 0wxD65F03C0
    and nopCode = 0wxD503201F
    and mov1ToX0 = 0wxD2800020
    
    fun codeSize _ = 1 (* Number of 32-bit words *)

    fun foldCode startIc foldFn ops =
    let
        fun doFold(oper :: operList, ic) =
            doFold(operList,
                (* Get the size BEFORE any possible change. *)
                ic + Word.fromInt(codeSize oper) * 0w4 before foldFn(oper, ic))
        |   doFold(_, ic) = ic
    in
        doFold(ops, startIc)
    end

    
    fun genCode(ops, Code {constVec, ...}) =
    let
    
        (* First pass - set the labels. *)
        fun setLabelsAndSizes ops = Word.fromInt(List.length ops)
        val codeSize = setLabelsAndSizes ops (* Number of 32-bit instructions *)
        val wordsOfCode = (codeSize + 0w1) div 0w2 (* Round up to 64-bits *)
        val paddingWord = if Word.andb(codeSize, 0w1) = 0w1 then [nopCode] else []

        val segSize   = wordsOfCode + Word.fromInt(List.length(! constVec)) + 0w4 (* 4 extra words *)
        val codeVec = byteVecMake segSize


        fun genCodeWords(code, byteNo) =
        (
            (* Little-endian order *)
            byteVecSet(codeVec, byteNo, wordToWord8 code);
            byteVecSet(codeVec, byteNo+0w1, wordToWord8(code >> 0w8));
            byteVecSet(codeVec, byteNo+0w2, wordToWord8(code >> 0w16));
            byteVecSet(codeVec, byteNo+0w3, wordToWord8(code >> 0w24))
        )
        
    in
        foldCode 0w0 genCodeWords (ops @ paddingWord);
        (codeVec (* Return the completed code. *), wordsOfCode (* And the size in 64-bit words. *))
    end

    (* Store a 64-bit value in the code *)
    fun set64(value, wordNo, seg) =
    let
        val addrs = wordNo * 0w8
        fun putBytes(value, a, seg, i) =
        if i = 0w8 then ()
        else
        (
            byteVecSet(seg, a+i, Word8.fromInt(value mod 256));
            putBytes(value div 256, a, seg, i+0w1)
        )
    in
        putBytes(value, addrs, seg, 0w0)
    end
    
   

    fun printCode (codeVec, functionName, wordsOfCode, printStream) =
        (printStream "Code for "; printStream functionName)

    (* Adds the constants onto the code, and copies the code into a new segment *)
    fun copyCode {code as
                    Code{ printAssemblyCode, printStream,
                           functionName, constVec, ...}, maxStack, numberOfArguments, resultClosure} =
    let
        local
            val codeList = [mov1ToX0, retCode]
            (* Add a stack check.  This is only needed if the
               function needs more than 128 words since the call and tail functions
               check for this much. *)
        in
            val codeList =
                if maxStack < 128
                then codeList
                else raise InternalError "TODO" (* SimpleCode[opcode_stackSize16, Word8.fromInt maxStack, Word8.fromInt(maxStack div 256)] :: codeList *)
        end

        val (byteVec, wordsOfCode) = genCode(codeList, code)

        (* +3 for profile count, function name and constants count *)
        val numOfConst = List.length(! constVec)
        val segSize   = wordsOfCode + Word.fromInt numOfConst + 0w4
        val firstConstant = wordsOfCode + 0w3 (* Add 3 for no of consts, fn name and profile count. *)
    
        (* Put in the number of constants. This must go in before
           we actually put in any constants. *)
        local
            val lastWord = segSize - 0w1
        in
            val () = set64(numOfConst + 2, wordsOfCode, byteVec)
            (* Set the last word of the code to the (negative) byte offset of the start of the code area
               from the end of this word. *)
            val () = set64((numOfConst + 3) * ~8, lastWord, byteVec) 
        end

        (* Now we've filled in all the size info we need to convert the segment
           into a proper code segment before it's safe to put in any ML values. *)
        val codeVec = byteVecToCodeVec(byteVec, resultClosure)

        local
            val name     : string = functionName
            val nameWord : machineWord = toMachineWord name
        in
            val () = codeVecPutWord (codeVec, wordsOfCode+0w1, nameWord)
        end
        (* Profile ref.  A byte ref used by the profiler in the RTS. *)
        local
            val v = RunCall.allocateByteMemory(0w1, Word.fromLargeWord(Word8.toLargeWord(Word8.orb(F_mutable, F_bytes))))
            fun clear 0w0 = ()
            |   clear i = (assignByte(v, i-0w1, 0w0); clear (i-0w1))
            val () = clear(wordSize)
        in
            val () = codeVecPutWord (codeVec, wordsOfCode+0w2, toMachineWord v)
        end

        (* and then copy the constants from the constant list. *)
        local
            fun setConstant(value, num) =
            (
                codeVecPutWord (codeVec, firstConstant + num, value);
                num+0w1
            )
        in
            val _ = List.foldl setConstant 0w0 (!constVec)
        end
    in
        if printAssemblyCode orelse true
        then (* print out the code *)
            (printCode (codeVec, functionName, wordsOfCode, printStream); printStream"\n")
        else ();
        codeVecLock(codeVec, resultClosure)
    end (* copyCode *)


    structure Sharing =
    struct
        type code = code
        type closureRef = closureRef
    end
end;

