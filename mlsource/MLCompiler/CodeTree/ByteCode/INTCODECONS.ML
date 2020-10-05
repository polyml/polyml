(*
    Copyright (c) 2015-18, 2020 David C.J. Matthews
    
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

functor INTCODECONS (
structure DEBUG: DEBUGSIG

structure PRETTY: PRETTYSIG

) : INTCODECONSSIG =

struct
    open CODE_ARRAY
    open DEBUG
    open Address
    open Misc

    infix 5 << <<+ <<- >> >>+ >>- ~>> ~>>+ ~>>- (* Shift operators *)
    infix 3 andb orb xorb andbL orbL xorbL andb8 orb8 xorb8
    
    val op << = Word.<< and op >> = Word.>> and op ~>> = Word.~>>

    val wordToWord8 = Word8.fromLargeWord o Word.toLargeWord
    and word8ToWord = Word.fromLargeWord o Word8.toLargeWord
    
    (* Typically the compiler is built on a little-endian machine but it could
       be run on a machine with either endian-ness.  We have to find out the
       endian-ness when we run.  There are separate versions of the compiler
       for 32-bit and 64-bit so that can be a constant.  *)
    local
        val isBigEndian: unit -> bool = RunCall.rtsCallFast1 "PolyIsBigEndian"
    in
        val isBigEndian = isBigEndian()
    end

    val opcode_jump              = 0wx02    (* 8-bit unsigned jump forward. *)
    and opcode_jumpFalse         = 0wx03    (* Test top of stack. Take 8-bit unsigned jump if false. *)
    and opcode_loadMLWord        = 0wx04
    and opcode_storeMLWord       = 0wx05
    and opcode_alloc_ref         = 0wx06
    and opcode_blockMoveWord     = 0wx07
    and opcode_loadUntagged      = 0wx08
    and opcode_storeUntagged     = 0wx09
    and opcode_case16            = 0wx0a
    and opcode_containerW        = 0wx0b
    and opcode_callClosure       = 0wx0c
    and opcode_returnW           = 0wx0d
    and opcode_containerB        = 0wx0e
    and opcode_callConstAddr32   = 0wx0f
    and opcode_raiseEx           = 0wx10
    and opcode_callConstAddr16   = 0wx11
    and opcode_callConstAddr8    = 0wx12
    and opcode_localW            = 0wx13
    and opcode_indirectW         = 0wx14
    and opcode_moveToVecW        = 0wx15
    and opcode_callLocalB        = 0wx16
    and opcode_setStackValW      = 0wx17
    and opcode_resetW            = 0wx18
    and opcode_resetR_w          = 0wx19
    and opcode_constAddr16       = 0wx1a
    and opcode_constIntW         = 0wx1b
    and opcode_callFastRTSRRtoR  = 0wx1c (* 2 byte *)
    and opcode_callFastRTSRGtoR  = 0wx1d (* 2 byte *)
    and opcode_jumpBack8         = 0wx1e   (* 8-bit unsigned jump backwards - relative to end of instr. *)
    and opcode_returnB           = 0wx1f
    and opcode_jumpBack16        = 0wx20    (* 16-bit unsigned jump backwards - relative to end of instr. *)
    and opcode_indirectLocalBB   = 0wx21
    and opcode_localB            = 0wx22
    and opcode_indirectB         = 0wx23
    and opcode_moveToVecB        = 0wx24
    and opcode_setStackValB      = 0wx25
    and opcode_resetB            = 0wx26
    and opcode_resetRB           = 0wx27
    and opcode_constIntB         = 0wx28
    and opcode_local_0           = 0wx29
    and opcode_local_1           = 0wx2a
    and opcode_local_2           = 0wx2b
    and opcode_local_3           = 0wx2c
    and opcode_local_4           = 0wx2d
    and opcode_local_5           = 0wx2e
    and opcode_local_6           = 0wx2f
    and opcode_local_7           = 0wx30
    and opcode_local_8           = 0wx31
    and opcode_local_9           = 0wx32
    and opcode_local_10          = 0wx33
    and opcode_local_11          = 0wx34
    and opcode_indirect_0        = 0wx35
    and opcode_indirect_1        = 0wx36
    and opcode_indirect_2        = 0wx37
    and opcode_indirect_3        = 0wx38
    and opcode_indirect_4        = 0wx39
    and opcode_indirect_5        = 0wx3a
    and opcode_const_0           = 0wx3b
    and opcode_const_1           = 0wx3c
    and opcode_const_2           = 0wx3d
    and opcode_const_3           = 0wx3e
    and opcode_const_4           = 0wx3f
    and opcode_const_10          = 0wx40
    (*and opcode_return_0          = 0wx41 *) (* Legacy *)
    and opcode_return_1          = 0wx42
    and opcode_return_2          = 0wx43
    and opcode_return_3          = 0wx44
    and opcode_local_12          = 0wx45
    and opcode_reset_1           = 0wx50
    and opcode_reset_2           = 0wx51
    and opcode_resetR_1          = 0wx64
    and opcode_resetR_2          = 0wx65
    and opcode_resetR_3          = 0wx66
    and opcode_tupleW            = 0wx67
    and opcode_tupleB            = 0wx68
    and opcode_tuple_2           = 0wx69
    and opcode_tuple_3           = 0wx6a
    and opcode_tuple_4           = 0wx6b
    and opcode_lock              = 0wx6c
    and opcode_ldexc             = 0wx6d

    and opcode_pushHandler       = 0wx78
    and opcode_tailbb            = 0wx7b
    and opcode_tail              = 0wx7c
    and opcode_setHandler        = 0wx81

    and opcode_callFastRTS0      = 0wx83
    and opcode_callFastRTS1      = 0wx84
    and opcode_callFastRTS2      = 0wx85
    and opcode_callFastRTS3      = 0wx86
    and opcode_callFastRTS4      = 0wx87
    and opcode_callFastRTS5      = 0wx88

    and opcode_callFullRTS0      = 0wx89
    and opcode_callFullRTS1      = 0wx8a
    and opcode_callFullRTS2      = 0wx8b
    and opcode_callFullRTS3      = 0wx8c
    and opcode_callFullRTS4      = 0wx8d
    and opcode_callFullRTS5      = 0wx8e

    and opcode_notBoolean        = 0wx91
    and opcode_isTagged          = 0wx92
    and opcode_cellLength        = 0wx93
    and opcode_cellFlags         = 0wx94
    and opcode_clearMutable      = 0wx95
    and opcode_atomicIncr        = 0wx97
    and opcode_atomicDecr        = 0wx98
    
    and opcode_equalWord         = 0wxa0
    and opcode_lessSigned        = 0wxa2
    and opcode_lessUnsigned      = 0wxa3
    and opcode_lessEqSigned      = 0wxa4
    and opcode_lessEqUnsigned    = 0wxa5
    and opcode_greaterSigned     = 0wxa6
    and opcode_greaterUnsigned   = 0wxa7
    and opcode_greaterEqSigned   = 0wxa8
    and opcode_greaterEqUnsigned = 0wxa9

    and opcode_fixedAdd          = 0wxaa
    and opcode_fixedSub          = 0wxab
    and opcode_fixedMult         = 0wxac
    and opcode_fixedQuot         = 0wxad
    and opcode_fixedRem          = 0wxae
    and opcode_wordAdd           = 0wxb1
    and opcode_wordSub           = 0wxb2
    and opcode_wordMult          = 0wxb3
    and opcode_wordDiv           = 0wxb4
    and opcode_wordMod           = 0wxb5
    and opcode_wordAnd           = 0wxb7
    and opcode_wordOr            = 0wxb8
    and opcode_wordXor           = 0wxb9
    and opcode_wordShiftLeft     = 0wxba
    and opcode_wordShiftRLog     = 0wxbb
    and opcode_allocByteMem      = 0wxbd
    and opcode_getThreadId       = 0wxd9
    and opcode_allocWordMemory   = 0wxda
    and opcode_loadMLByte        = 0wxdc
    and opcode_storeMLByte       = 0wxe4
    and opcode_blockMoveByte     = 0wxec
    and opcode_blockEqualByte    = 0wxed
    and opcode_blockCompareByte  = 0wxee
    and opcode_deleteHandler     = 0wxf1 (* Just deletes the handler - no jump. *)
    and opcode_jump32            = 0wxf2 (* 32-bit signed jump, forwards or backwards. *)
    and opcode_jump32False       = 0wxf3 (* Test top item. Take 32-bit signed jump if false. *)
    and opcode_constAddr32       = 0wxf4 (* Followed by a 32-bit offset. Load a constant at that address. *)
    and opcode_setHandler32      = 0wxf5 (* Setup a handler whose address is given by the 32-bit signed offset. *)
    and opcode_case32            = 0wxf6 (* Indexed case with 32-bit offsets *)
    and opcode_jump16            = 0wxf7
    and opcode_jump16False       = 0wxf8
    and opcode_setHandler16      = 0wxf9
    and opcode_constAddr8        = 0wxfa
    (*and opcode_stackSize8        = 0wxfb*)
    and opcode_stackSize16       = 0wxfc
    and opcode_escape            = 0wxfe (* For two-byte opcodes. *)
    and opcode_enterIntX86       = 0wxff (* Reserved - this is the first byte of a call *)
    
(*    
    val opcode_loadMLWord        = 0wxdb
    and opcode_storeMLWord       = 0wxe3
    and opcode_blockMoveWord     = 0wxeb
    and opcode_loadUntagged      = 0wxef
    and opcode_storeUntagged     = 0wxf0 *)

    
    (* Extended opcodes - preceded by 0xfe escape *)
    val ext_opcode_floatAbs          = 0wx56
    and ext_opcode_floatNeg          = 0wx57
    and ext_opcode_fixedIntToFloat   = 0wx58
    and ext_opcode_floatToReal       = 0wx59
    and ext_opcode_realToFloat       = 0wx5a
    and ext_opcode_floatEqual        = 0wx5b
    and ext_opcode_floatLess         = 0wx5c
    and ext_opcode_floatLessEq       = 0wx5d
    and ext_opcode_floatGreater      = 0wx5e
    and ext_opcode_floatGreaterEq    = 0wx5f
    and ext_opcode_floatAdd          = 0wx60
    and ext_opcode_floatSub          = 0wx61
    and ext_opcode_floatMult         = 0wx62
    and ext_opcode_floatDiv          = 0wx63
    and ext_opcode_realToInt         = 0wx6e
    and ext_opcode_floatToInt        = 0wx6f
    and ext_opcode_callFastRTSFtoF   = 0wx70
    and ext_opcode_callFastRTSGtoF   = 0wx71
    and ext_opcode_callFastRTSFFtoF  = 0wx72
    and ext_opcode_callFastRTSFGtoF  = 0wx73
    and ext_opcode_realUnordered     = 0wx79
    and ext_opcode_floatUnordered    = 0wx7a
    and ext_opcode_callFastRTSRtoR   = 0wx8f
    and ext_opcode_callFastRTSGtoR   = 0wx90
    and ext_opcode_atomicReset       = 0wx99
    and ext_opcode_longWToTagged     = 0wx9a
    and ext_opcode_signedToLongW     = 0wx9b
    and ext_opcode_unsignedToLongW   = 0wx9c
    and ext_opcode_realAbs           = 0wx9d
    and ext_opcode_realNeg           = 0wx9e
    and ext_opcode_fixedIntToReal    = 0wx9f
    and ext_opcode_fixedDiv          = 0wxaf
    and ext_opcode_fixedMod          = 0wxb0
    and ext_opcode_wordShiftRArith   = 0wxbc
    and ext_opcode_lgWordEqual       = 0wxbe
    and ext_opcode_lgWordLess        = 0wxc0
    and ext_opcode_lgWordLessEq      = 0wxc1
    and ext_opcode_lgWordGreater     = 0wxc2
    and ext_opcode_lgWordGreaterEq   = 0wxc3
    and ext_opcode_lgWordAdd         = 0wxc4
    and ext_opcode_lgWordSub         = 0wxc5
    and ext_opcode_lgWordMult        = 0wxc6
    and ext_opcode_lgWordDiv         = 0wxc7
    and ext_opcode_lgWordMod         = 0wxc8
    and ext_opcode_lgWordAnd         = 0wxc9
    and ext_opcode_lgWordOr          = 0wxca
    and ext_opcode_lgWordXor         = 0wxcb
    and ext_opcode_lgWordShiftLeft   = 0wxcc
    and ext_opcode_lgWordShiftRLog   = 0wxcd
    and ext_opcode_lgWordShiftRArith = 0wxce
    and ext_opcode_realEqual         = 0wxcf
    and ext_opcode_realLess          = 0wxd1
    and ext_opcode_realLessEq        = 0wxd2
    and ext_opcode_realGreater       = 0wxd3
    and ext_opcode_realGreaterEq     = 0wxd4
    and ext_opcode_realAdd           = 0wxd5
    and ext_opcode_realSub           = 0wxd6
    and ext_opcode_realMult          = 0wxd7
    and ext_opcode_realDiv           = 0wxd8
    and ext_opcode_loadC8            = 0wxdd
    and ext_opcode_loadC16           = 0wxde
    and ext_opcode_loadC32           = 0wxdf
    and ext_opcode_loadC64           = 0wxe0
    and ext_opcode_loadCFloat        = 0wxe1
    and ext_opcode_loadCDouble       = 0wxe2
    and ext_opcode_storeC8           = 0wxe5
    and ext_opcode_storeC16          = 0wxe6
    and ext_opcode_storeC32          = 0wxe7
    and ext_opcode_storeC64          = 0wxe8
    and ext_opcode_storeCFloat       = 0wxe9
    and ext_opcode_storeCDouble      = 0wxea
   

    (* A Label is a ref that is later set to the location. *)
    type labels = {destination: Word.word ref }

    (* Used for jump, jumpFalse, setHandler and delHandler. *)
    datatype jumpTypes = Jump | JumpFalse | SetHandler

    datatype opcode =
        SimpleCode of Word8.word list           (* Bytes that don't need any special treatment *)
    |   LabelCode of labels                     (* A label - forwards or backwards. *)
    |   JumpInstruction of { label: labels, jumpType: jumpTypes, size : jumpSize ref }   (* Jumps or SetHandler. *)
    |   PushConstant of { constNum: int, size : jumpSize ref, isCall: bool }
    |   IndexedCase of { labels: labels list, size : jumpSize ref }
    |   LoadLocal of Word8.word (* Locals - simplifies peephole optimisation. *)
    
    and jumpSize = Size8 | Size16 | Size32

    and code = Code of 
    {
        constVec:       machineWord list ref, (* Vector of words to be put at end *)
        procName:       string,         (* Name of the procedure. *)
        printAssemblyCode:bool,            (* Whether to print the code when we finish. *)
        printStream:    string->unit,    (* The stream to use *)
        stage1Code:     opcode list ref
    }

    (* create and initialise a code segment *)
    fun codeCreate (name : string, parameters) = 
    let
        val printStream = PRETTY.getSimplePrinter(parameters, []);
    in
        Code
        { 
            constVec         = ref [],
            procName         = name,
            printAssemblyCode = DEBUG.getParameter DEBUG.assemblyCodeTag parameters,
            printStream    = printStream,
            stage1Code       = ref []
        }
    end

    (* Find the offset in the constant area of a constant. *)
    (* The first has offset 0.                             *)
    fun addConstToVec (valu, Code{constVec, ...}) =
    let
        (* Search the list to see if the constant is already there. *)
        fun findConst valu [] num =
            (* Add to the list *)
            (
                constVec    := ! constVec @ [valu];
                num
            )
        |   findConst valu (h :: t) num =
                if wordEq (valu, h)
                then num
                else findConst valu t (num + 1) (* Not equal *)
    in
        findConst valu (! constVec) 0
    end

    fun printCode (seg: codeVec, procName: string, endcode, printStream) =
    let
        val () = printStream "\n";
        val () = if procName = "" (* No name *) then printStream "?" else printStream procName;
        val () = printStream ":\n";

        (* prints a string representation of a number *)
        fun printHex (v) = printStream(Word.fmt StringCvt.HEX v);
 
        val ptr = ref 0w0;
 
        (* To make sure we do not print branch extensions as though they
           were instructions we keep a list of all indirect forward references
           and print values at those addresses as addresses.
           This list is sorted with the lowest address first. *)
 
        val indirections = ref [];
 
        local
            fun addL (n, []) = [n]
            |   addL (n, l as (x :: xs)) =
                    if n < x then n :: l
                    else if n = x then l
                    else x :: addL (n, xs)
        in
            fun addInd (ind) = indirections := addL (ind, !indirections)
        end
        
        (* Gets "length" bytes from locations "addr", "addr"+1...
           Returns an unsigned number. *)
        fun getB (0, _, _) = 0w0
        |   getB (length, addr, seg) =
                (getB (length - 1, addr + 0w1, seg) << 0w8) + word8ToWord (codeVecGet (seg, addr))

        (* Prints a relative address. *)
        fun printDisp (len, spacer: string, addToList: bool) =
        let
            val ad = getB(len, !ptr, seg) + !ptr + Word.fromInt len
            val () = if addToList then addInd ad else ();
            val () = printStream spacer;
            val () = printHex ad;
        in
            ptr := !ptr + Word.fromInt len
        end

        (* Prints an operand of an instruction *)
        fun printOp (len, spacer : string) =
        let
            val () = printStream spacer;
            val () = printHex (getB (len, !ptr, seg))
        in
            ptr := !ptr + Word.fromInt len
        end;

    in
        while !ptr < endcode do
        let
            val addr = !ptr
        in
            printHex addr; (* The address. *)

            if (case !indirections of v :: _ => v = addr | [] => false)
            then
            let (* It's an address. *)
                val () = printDisp (2, "\t", false);
            in
                case !indirections of
                    _ :: vs => indirections := vs
                |   _       => raise InternalError "printCode: indirection list confused"
            end
      
            else
            let (* It's an instruction. *)
                val ()  = printStream "\t"
                val opc = codeVecGet (seg, !ptr) (* opcode *)
                val ()  = ptr := !ptr + 0w1
            in
                case opc of
                    0wx02 => (printStream "jump"; printDisp (1, "\t", false))
                |   0wx03 => (printStream "jumpFalse"; printDisp (1, "\t", false))
                |   0wx04 => printStream "loadMLWord"
                |   0wx05 => printStream "storeMLWord"
                |   0wx06 => printStream "alloc_ref"
                |   0wx07 => printStream "blockMoveWord"
                |   0wx08 => printStream "loadUntagged"
                |   0wx09 => printStream "storeUntagged"
                |   0wx0a =>
                    let
                        (* Have to find out how many items there are. *)
                        val limit = getB (2, !ptr, seg);
                        val () = printOp (2, "case16\t");
                        val base = !ptr;
        
                        fun printEntry _ = (printStream "\n\t"; printHex(base + getB(2, !ptr, seg)); ptr := !ptr + 0w2)
        
                        fun forLoop f i n = if i >= n then () else (f i; forLoop f (i + 0w1) n)
                    in
                        forLoop printEntry 0w0 limit
                    end
                |   0wx0b => printStream "containerW"
                |   0wx0c => printStream "callClosure"
                |   0wx0d => printOp(2, "returnW\t")
                |   0wx0e => printStream "containerB"
                |   0wx0f => printDisp (4, "callConstAddr32\t", false)
                |   0wx10 => printStream "raiseEx"
                |   0wx11 => printDisp (2, "callConstAddr16\t", false)
                |   0wx12 => printDisp (1, "callConstAddr8\t", false)
                |   0wx13 => printOp(2, "localW\t")
                |   0wx14 => printOp(2, "indirectW\t")
                |   0wx15 => printOp(2, "moveToVecW\t")
                |   0wx17 => printOp(2, "setStackValW\t")
                |   0wx16 => printOp(1, "callLocalB\t")
                |   0wx18 => printOp(2, "resetW\t")
                |   0wx19 => printOp(2, "resetR_w\t")
                |   0wx1a => (printStream "constAddr16"; printDisp (2, "\t", false))
                |   0wx1b => printOp(2, "constIntW\t")
                |   0wx1c => printStream "callFastRTSRRtoR"
                |   0wx1d => printStream "callFastRTSRGtoR"
                |   0wx1e =>
                    ((* Should be negative *)
                        printStream "jumpBack8\t";
                        printHex((!ptr - 0w1) - getB(1, !ptr, seg));
                        ptr := !ptr + 0w1
                    )
                |   0wx1f => printOp(1, "returnB\t")
                |   0wx20 =>
                    (
                        printStream "jumpBack16\t";
                        printHex((!ptr - 0w1) - getB(2, !ptr, seg));
                        ptr := !ptr + 0w2
                    )
                |   0wx21 => (printOp(1, "indirectLocalBB\t"); printOp(1, ","))
                |   0wx22 => printOp(1, "localB\t")
                |   0wx23 => printOp(1, "indirectB\t")
                |   0wx24 => printOp(1, "moveToVecB\t")
                |   0wx25 => printOp(1, "setStackValB\t")
                |   0wx26 => printOp(1, "resetB\t")
                |   0wx27 => printOp(1, "resetRB\t")
                |   0wx28 => printOp(1, "constIntB\t")
                |   0wx29 => printStream "local_0"
                |   0wx2a => printStream "local_1"
                |   0wx2b => printStream "local_2"
                |   0wx2c => printStream "local_3"
                |   0wx2d => printStream "local_4"
                |   0wx2e => printStream "local_5"
                |   0wx2f => printStream "local_6"
                |   0wx30 => printStream "local_7"
                |   0wx31 => printStream "local_8"
                |   0wx32 => printStream "local_9"
                |   0wx33 => printStream "local_10"
                |   0wx34 => printStream "local_11"
                |   0wx35 => printStream "indirect_0"
                |   0wx36 => printStream "indirect_1"
                |   0wx37 => printStream "indirect_2"
                |   0wx38 => printStream "indirect_3"
                |   0wx39 => printStream "indirect_4"
                |   0wx3a => printStream "indirect_5"
                |   0wx3b => printStream "const_0"
                |   0wx3c => printStream "const_1"
                |   0wx3d => printStream "const_2"
                |   0wx3e => printStream "const_3"
                |   0wx3f => printStream "const_4"
                |   0wx40 => printStream "const_10"
                |   0wx41 => printStream "return_0"
                |   0wx42 => printStream "return_1"
                |   0wx43 => printStream "return_2"
                |   0wx44 => printStream "return_3"
                |   0wx45 => printStream "local_12"
                |   0wx50 => printStream "reset_1"
                |   0wx51 => printStream "reset_2"
                |   0wx52 => printStream "getStore_2"
                |   0wx53 => printStream "getStore_3"
                |   0wx54 => printStream "getStore_4"
                |   0wx64 => printStream "resetR_1"
                |   0wx65 => printStream "resetR_2"
                |   0wx66 => printStream "resetR_3"
                |   0wx67 => printOp(2, "tupleW\t")
                |   0wx68 => printOp(1, "tupleB\t")
                |   0wx69 => printStream "tuple_2"
                |   0wx6a => printStream "tuple_3"
                |   0wx6b => printStream "tuple_4"
                |   0wx6c => printStream "lock"
                |   0wx6d => printStream "ldexc"
                |   0wx78 => printStream "pushHandler"
                |   0wx7b => (printOp (1, "tailbb\t"); printOp (1, ","))
                |   0wx7c => (printOp (2, "tail\t"); printOp (2, ","))
                |   0wx7d => printOp(1, "tail3b\t")
                |   0wx7e => printOp(1, "tail4b\t")
                |   0wx7f => printStream "tail3_2"
                |   0wx80 => printStream "tail3_3"
                |   0wx81 => (printStream "setHandler"; printDisp (1, "\t", false))
                |   0wx83 => printStream "callFastRTS0"
                |   0wx84 => printStream "callFastRTS1"
                |   0wx85 => printStream "callFastRTS2"
                |   0wx86 => printStream "callFastRTS3"
                |   0wx87 => printStream "callFastRTS4"
                |   0wx88 => printStream "callFastRTS5"
                |   0wx89 => printStream "callFullRTS0"
                |   0wx8a => printStream "callFullRTS1"
                |   0wx8b => printStream "callFullRTS2"
                |   0wx8c => printStream "callFullRTS3"
                |   0wx8d => printStream "callFullRTS4"
                |   0wx8e => printStream "callFullRTS5"
                |   0wx91 => printStream "notBoolean"
                |   0wx92 => printStream "isTagged"
                |   0wx93 => printStream "cellLength"
                |   0wx94 => printStream "cellFlags"
                |   0wx95 => printStream "clearMutable"
                |   0wx97 => printStream "atomicIncr"
                |   0wx98 => printStream "atomicDecr"
                |   0wxa0 => printStream "equalWord"
                |   0wxa2 => printStream "lessSigned"
                |   0wxa3 => printStream "lessUnsigned"
                |   0wxa4 => printStream "lessEqSigned"
                |   0wxa5 => printStream "lessEqUnsigned"
                |   0wxa6 => printStream "greaterSigned"
                |   0wxa7 => printStream "greaterUnsigned"
                |   0wxa8 => printStream "greaterEqSigned"
                |   0wxa9 => printStream "greaterEqUnsigned"
                |   0wxaa => printStream "fixedAdd"
                |   0wxab => printStream "fixedSub"
                |   0wxac => printStream "fixedMult"
                |   0wxad => printStream "fixedQuot"
                |   0wxae => printStream "fixedRem"
                |   0wxb1 => printStream "wordAdd"
                |   0wxb2 => printStream "wordSub"
                |   0wxb3 => printStream "wordMult"
                |   0wxb4 => printStream "wordDiv"
                |   0wxb5 => printStream "wordMod"
                |   0wxb7 => printStream "wordAnd"
                |   0wxb8 => printStream "wordOr"
                |   0wxb9 => printStream "wordXor"
                |   0wxba => printStream "wordShiftLeft"
                |   0wxbb => printStream "wordShiftRLog"
                |   0wxbd => printStream "allocByteMem"
                |   0wxd9 => printStream "getThreadId"
                |   0wxda => printStream "allocWordMemory"
                |   0wxdc => printStream "loadMLByte"
                |   0wxe4 => printStream "storeMLByte"
                |   0wxec => printStream "blockMoveByte"
                |   0wxed => printStream "blockEqualByte"
                |   0wxee => printStream "blockCompareByte"
                |   0wxf1 => printStream "deleteHandler"
                |   0wxf2 => printDisp (4, "jump32\t", false)
                |   0wxf3 => printDisp (4, "jump32False\t", false)
                |   0wxf4 => printDisp (4, "constAddr32\t", false)
                |   0wxf5 => printDisp (4, "setHandler32\t", false)
                |   0wxf6 =>
                    let
                        (* Have to find out how many items there are. *)
                        val limit = getB (2, !ptr, seg);
                        val () = printOp (2, "case32\t");
                        val base = !ptr;
        
                        fun printEntry _ = (printStream "\n\t"; printHex(base + getB(4, !ptr, seg)); ptr := !ptr + 0w4)
        
                        fun forLoop f i n = if i >= n then () else (f i; forLoop f (i + 0w1) n)
                    in
                        forLoop printEntry 0w0 limit
                    end
                |   0wxf7 => printStream "jump16"
                |   0wxf8 => printStream "jump16False"
                |   0wxf9 => printStream "setHandler16"
                |   0wxfa => printDisp (1, "constAddr8\t", false)
                |   0wxfb => printOp(1, "stackSize8\t")
                |   0wxfc => printOp(2, "stackSize16\t")
                |   0wxff => printStream "enterIntX86"
                
                |   0wxfe =>
                    (
                        case codeVecGet (seg, !ptr) before ptr := !ptr + 0w1 of
                            0wx56 => printStream "floatAbs"
                        |   0wx57 => printStream "floatNeg"
                        |   0wx58 => printStream "fixedIntToFloat"
                        |   0wx59 => printStream "floatToReal"
                        |   0wx5a => printOp(1, "realToFloat\t")
                        |   0wx5b => printStream "floatEqual"
                        |   0wx5c => printStream "floatLess"
                        |   0wx5d => printStream "floatLessEq"
                        |   0wx5e => printStream "floatGreater"
                        |   0wx5f => printStream "floatGreaterEq"
                        |   0wx60 => printStream "floatAdd"
                        |   0wx61 => printStream "floatSub"
                        |   0wx62 => printStream "floatMult"
                        |   0wx63 => printStream "floatDiv"
                        |   0wx6e => printOp(1, "realToInt\t")
                        |   0wx6f => printOp(1, "floatToInt\t")
                        |   0wx70 => printStream "callFastRTSFtoF"
                        |   0wx71 => printStream "callFastRTSGtoF"
                        |   0wx72 => printStream "callFastRTSFFtoF"
                        |   0wx73 => printStream "callFastRTSFGtoF"
                        |   0wx79 => printStream "realUnordered"
                        |   0wx7a => printStream "floatUnordered"
                        |   0wx8f => printStream "callFastRTSRtoR"
                        |   0wx90 => printStream "callFastRTSGtoR"
                        |   0wx99 => printStream "atomicReset"
                        |   0wx9a => printStream "longWToTagged"
                        |   0wx9b => printStream "signedToLongW"
                        |   0wx9c => printStream "unsignedToLongW"
                        |   0wx9d => printStream "realAbs"
                        |   0wx9e => printStream "realNeg"
                        |   0wx9f => printStream "fixedIntToReal"
                        |   0wxaf => printStream "fixedDiv"
                        |   0wxb0 => printStream "fixedMod"
                        |   0wxbc => printStream "wordShiftRArith"
                        |   0wxbe => printStream "lgWordEqual"
                        |   0wxc0 => printStream "lgWordLess"
                        |   0wxc1 => printStream "lgWordLessEq"
                        |   0wxc2 => printStream "lgWordGreater"
                        |   0wxc3 => printStream "lgWordGreaterEq"
                        |   0wxc4 => printStream "lgWordAdd"
                        |   0wxc5 => printStream "lgWordSub"
                        |   0wxc6 => printStream "lgWordMult"
                        |   0wxc7 => printStream "lgWordDiv"
                        |   0wxc8 => printStream "lgWordMod"
                        |   0wxc9 => printStream "lgWordAnd"
                        |   0wxca => printStream "lgWordOr"
                        |   0wxcb => printStream "lgWordXor"
                        |   0wxcc => printStream "lgWordShiftLeft"
                        |   0wxcd => printStream "lgWordShiftRLog"
                        |   0wxce => printStream "lgWordShiftRArith"
                        |   0wxcf => printStream "realEqual"
                        |   0wxd1 => printStream "realLess"
                        |   0wxd2 => printStream "realLessEq"
                        |   0wxd3 => printStream "realGreater"
                        |   0wxd4 => printStream "realGreaterEq"
                        |   0wxd5 => printStream "realAdd"
                        |   0wxd6 => printStream "realSub"
                        |   0wxd7 => printStream "realMult"
                        |   0wxd8 => printStream "realDiv"
                        |   0wxdd => printStream "loadC8"
                        |   0wxde => printStream "loadC16"
                        |   0wxdf => printStream "loadC32"
                        |   0wxe0 => printStream "loadC64"
                        |   0wxe1 => printStream "loadCFloat"
                        |   0wxe2 => printStream "loadCDouble"
                        |   0wxe5 => printStream "storeC8"
                        |   0wxe6 => printStream "storeC16"
                        |   0wxe7 => printStream "storeC32"
                        |   0wxe8 => printStream "storeC64"
                        |   0wxe9 => printStream "storeCFloat"
                        |   0wxea => printStream "storeCDouble"
                        |  _ => printStream ("unknown:0xfe 0x" ^ Word8.toString opc)
                    )

                |   opc => printStream("unknown:0x" ^ Word8.toString opc)

            end; (* an instruction. *)

            printStream "\n"
        end (* main loop *)
    end (* printCode *)
    
    fun codeSize (SimpleCode l) = List.length l
    |   codeSize (LabelCode _) = 0
    |   codeSize (JumpInstruction{size=ref Size8, ...}) = 2
    |   codeSize (JumpInstruction{size=ref Size16, ...}) = 3
    |   codeSize (JumpInstruction{size=ref Size32, ...}) = 5
    |   codeSize (PushConstant{size=ref Size8, ...}) = 2
    |   codeSize (PushConstant{size=ref Size16, ...}) = 3
    |   codeSize (PushConstant{size=ref Size32, ...}) = 5
    |   codeSize (IndexedCase{labels, size=ref Size32, ...}) = 3 + List.length labels * 4 
    |   codeSize (IndexedCase{labels, size=ref Size16, ...}) = 3 + List.length labels * 2 
    |   codeSize (IndexedCase{labels=_, size=ref Size8, ...}) = raise InternalError "codeSize"
    |   codeSize (LoadLocal w) = if w <= 0w12 then 1 else 2
   
    (* General function to process the code.  ic is the byte counter within the original code. *)
    fun foldCode foldFn n ops =
    let
        fun doFold(oper :: operList, ic, acc) =
            doFold(operList, ic + Word.fromInt(codeSize oper),
                foldFn(oper, ic, acc))
        |   doFold(_, _, n) = n
    in
        doFold(ops, 0w0, n)
    end

    (* Process the code, setting the destination of any labels.  Return the length of the code. *)    
    fun setLabels(LabelCode{destination, ...} :: ops, ic) = (destination := ic; setLabels(ops, ic))
    |   setLabels(oper :: ops, ic) = setLabels(ops, ic + Word.fromInt(codeSize oper))
    |   setLabels([], ic) = ic
    
    (* Set the sizes of branches depending on the distance to the destination. *)
    fun setLabelsAndSizes ops =
    let
        val wordLength = wordSize

        (* Set the labels and adjust the sizes, repeating until it never gets smaller*)
        fun setLabAndSize(ops, lastSize) =
        let
            (* Calculate offsets for constants. *)
            val endIC = Word.andb(lastSize + wordLength - 0w1, ~ wordLength)
            val firstConstant = endIC + wordLength * 0w3
            (* Because the constant area is word aligned we have to allow for
               the possibility that the distance between a "load constant"
               instruction and the target could actually increase. *)
            val alignment = wordLength - 0w1
        
            fun adjust(JumpInstruction{size as ref Size32, label={destination=ref dest}, ...}, ic, _) =
                let
                    val diff =
                        if dest <= ic (* N.B. Include infinite loops as backwards. *)
                        then ic - dest (* Backwards - Counts from start of instruction. *)
                        else dest - (ic + 0w5) (* Forwards - Relative to the current end. *)
                in
                    if diff < 0wx100
                    then size := Size8
                    else if diff < 0wx10000
                    then size := Size16
                    else ()
                end

            |   adjust(JumpInstruction{size as ref Size16, label={destination=ref dest}, ...}, ic, _) =
                if dest <= ic
                then if ic - dest < 0wx100 then size := Size8 else ()
                else if dest - (ic + 0w3)  < 0wx100 then size := Size8 else ()
        
            |   adjust(IndexedCase{size as ref Size32, labels}, ic, _) =
                let
                    val startAddr = ic+0w3
                    (* Use 16-bit case if all the offsets are 16-bits. *)
                    fun is16bit{destination=ref dest} =
                        dest > startAddr andalso dest < startAddr+0wx10000
                in
                    if List.all is16bit labels
                    then size := Size16
                    else ()
                end

            |   adjust(PushConstant{size as ref Size32, constNum, ...}, ic, _) =
                let
                    val constAddr = firstConstant + Word.fromInt constNum * wordLength
                    val offset = constAddr - (ic + 0w5)
                in
                    if offset < 0wx100-alignment then size := Size8
                    else if offset < 0wx10000-alignment then size := Size16
                    else ()
                end

            |   adjust(PushConstant{size as ref Size16, constNum, ...}, ic, _) =
                let
                    val constAddr = firstConstant + Word.fromInt constNum * wordLength
                    val offset = constAddr - (ic + 0w3)
                in
                    if offset < 0wx100-alignment then size := Size8
                    else ()
                end

            |   adjust _ = ()

            val () = foldCode adjust () ops
            val nextSize = setLabels(ops, 0w0)
        in
            if nextSize < lastSize then setLabAndSize(ops, nextSize)
            else if nextSize = lastSize then lastSize
            else raise InternalError "setLabAndSize - size increased"
        end
    in
        setLabAndSize(ops, setLabels(ops, 0w0))
    end
    
    fun genCode(ops, Code {constVec, ...}) =
    let
        (* First pass - set the labels. *)
        val codeSize = setLabelsAndSizes ops
        val wordSize = wordSize
        (* Align to wordLength. *)
        val endIC = Word.andb(codeSize + wordSize - 0w1, ~ wordSize)
        val paddingBytes = List.tabulate(Word.toInt(endIC - codeSize), fn _ => SimpleCode[opcode_const_0])
        val endOfCode = endIC div wordSize
        val firstConstant = endIC + wordSize * 0w3 (* Add 3 for fn name, unused and profile count. *)
        val segSize   = endOfCode + Word.fromInt(List.length(! constVec)) + 0w4
        val codeVec = byteVecMake segSize

        val ic = ref 0w0
        
        fun genByte b = byteVecSet(codeVec, !ic, b) before ic := !ic + 0w1

        fun genByteCode(SimpleCode bytes, _, _) =
            (* Simple code - just generate the bytes. *)
                List.app genByte bytes

        |   genByteCode(LabelCode _, _, _) = ()

        |   genByteCode(JumpInstruction{label={destination=ref dest}, jumpType, size=ref Size32, ...}, ic, _) =
            let
                val opc =
                    case jumpType of
                        SetHandler => opcode_setHandler32
                    |   JumpFalse => opcode_jump32False
                    |   Jump => opcode_jump32
                val diff = dest - (ic + 0w5)
            in
                genByte opc;
                genByte(wordToWord8 diff);
                (* This may be negative so we must use an arithmetic shift. *)
                genByte(wordToWord8(diff ~>> 0w8));
                genByte(wordToWord8(diff ~>> 0w16));
                genByte(wordToWord8(diff ~>> 0w24))
            end

        |   genByteCode(JumpInstruction{label={destination=ref dest}, jumpType, size=ref Size16, ...}, ic, _) =
            if dest <= ic
            then (* Jump back. *)
            let
                val _ = jumpType = Jump orelse raise InternalError "genByteCode - back jump"
                val diff = ic - dest
                val _ = diff < 0wx10000 orelse raise InternalError "genByteCode - jump range"
            in
                genByte opcode_jumpBack16;
                genByte(wordToWord8 diff);
                genByte(wordToWord8(diff >> 0w8))
            end
            else
            let
                val opc =
                    case jumpType of
                        SetHandler => opcode_setHandler16
                    |   JumpFalse => opcode_jump16False
                    |   Jump => opcode_jump16
                val diff = dest - (ic + 0w3)
                val _ = diff < 0wx10000 orelse raise InternalError "genByteCode - jump range"
            in
                genByte opc;
                genByte(wordToWord8 diff);
                genByte(wordToWord8(diff >> 0w8))
            end

        |   genByteCode(JumpInstruction{label={destination=ref dest}, jumpType, size=ref Size8, ...}, ic, _) =
            if dest <= ic
            then (* Jump back. *)
            let
                val _ = jumpType = Jump orelse raise InternalError "genByteCode - back jump"
                val diff = ic - dest
                val _ = diff < 0wx100 orelse raise InternalError "genByteCode - jump range"
            in
                genByte opcode_jumpBack8;
                genByte(wordToWord8 diff)
            end
            else
            let
                val opc =
                    case jumpType of
                        SetHandler => opcode_setHandler
                    |   JumpFalse => opcode_jumpFalse
                    |   Jump => opcode_jump
                val diff = dest - (ic + 0w2)
                val _ = diff < 0wx100 orelse raise InternalError "genByteCode - jump range"
            in
                genByte opc;
                genByte(wordToWord8 diff)
            end

        |   genByteCode(PushConstant{ constNum, size=ref Size32, isCall, ... }, ic, _) =
            let
                val constAddr = firstConstant + Word.fromInt constNum * wordSize
                (* Offsets are calculated from the END of the instruction *)
                val offset = constAddr - (ic + 0w5)
            in
                genByte(if isCall then opcode_callConstAddr32 else opcode_constAddr32);
                genByte(wordToWord8 offset);
                genByte(wordToWord8(offset >> 0w8));
                genByte(wordToWord8(offset >> 0w16));
                genByte(wordToWord8(offset >> 0w24))
            end

        |   genByteCode(PushConstant{ constNum, size=ref Size16, isCall, ... }, ic, _) =
            let
                val constAddr = firstConstant + Word.fromInt constNum * wordSize
                val offset = constAddr - (ic + 0w3)
                val _ = offset < 0wx10000 orelse raise InternalError "genByteCode - constant range"
            in
                genByte(if isCall then opcode_callConstAddr16 else opcode_constAddr16);
                genByte(wordToWord8 offset);
                genByte(wordToWord8(offset >> 0w8))
            end

        |   genByteCode(PushConstant{ constNum, size=ref Size8, isCall, ... }, ic, _) =
            let
                val constAddr = firstConstant + Word.fromInt constNum * wordSize
                val offset = constAddr - (ic + 0w2)
                val _ = offset < 0wx100 orelse raise InternalError "genByteCode - constant range"
            in
                genByte(if isCall then opcode_callConstAddr8 else opcode_constAddr8);
                genByte(wordToWord8 offset)
            end

        |   genByteCode(IndexedCase{labels, size=ref Size32, ...}, ic, _) =
            let
                val nCases = List.length labels
                val () = genByte(opcode_case32)
                val () = genByte(Word8.fromInt nCases)
                val () = genByte(Word8.fromInt (nCases div 256))
                val startOffset = ic+0w3 (* Offsets are relative to here. *)

                fun putLabel{destination=ref dest} =
                let
                    val diff = dest - startOffset
                    val _ = dest > startOffset orelse raise InternalError "genByteCode - indexed case"
                in
                    genByte(wordToWord8 diff);
                    genByte(wordToWord8(diff >> 0w8));
                    genByte(wordToWord8(diff >> 0w16));
                    genByte(wordToWord8(diff >> 0w24))
                end
            in
                List.app putLabel labels
            end
        
        |   genByteCode(IndexedCase{labels, size=ref Size16, ...}, ic, _) =
            let
                val nCases = List.length labels
                val () = genByte(opcode_case16)
                val () = genByte(Word8.fromInt nCases)
                val () = genByte(Word8.fromInt (nCases div 256))
                val startOffset = ic+0w3 (* Offsets are relative to here. *)

                fun putLabel{destination=ref dest} =
                let
                    val diff = dest - startOffset
                    val _ = dest > startOffset orelse raise InternalError "genByteCode - indexed case"
                    val _ = diff < 0wx10000 orelse raise InternalError "genByteCode - indexed case"
                in
                    genByte(wordToWord8 diff);
                    genByte(wordToWord8(diff >> 0w8))
                end
            in
                List.app putLabel labels
            end
        
        |   genByteCode(IndexedCase{size=ref Size8, ...}, _, _) = raise InternalError "genByteCode - IndexedCase byte"
        
        |   genByteCode(LoadLocal 0w0, _, _) = genByte opcode_local_0
        |   genByteCode(LoadLocal 0w1, _, _) = genByte opcode_local_1
        |   genByteCode(LoadLocal 0w2, _, _) = genByte opcode_local_2
        |   genByteCode(LoadLocal 0w3, _, _) = genByte opcode_local_3
        |   genByteCode(LoadLocal 0w4, _, _) = genByte opcode_local_4
        |   genByteCode(LoadLocal 0w5, _, _) = genByte opcode_local_5
        |   genByteCode(LoadLocal 0w6, _, _) = genByte opcode_local_6
        |   genByteCode(LoadLocal 0w7, _, _) = genByte opcode_local_7
        |   genByteCode(LoadLocal 0w8, _, _) = genByte opcode_local_8
        |   genByteCode(LoadLocal 0w9, _, _) = genByte opcode_local_9
        |   genByteCode(LoadLocal 0w10, _, _) = genByte opcode_local_10
        |   genByteCode(LoadLocal 0w11, _, _) = genByte opcode_local_11
        |   genByteCode(LoadLocal 0w12, _, _) = genByte opcode_local_12
        |   genByteCode(LoadLocal w, _, _) = (genByte opcode_localB; genByte w)
            
    in
        foldCode genByteCode () (ops @ paddingBytes);
        (codeVec (* Return the completed code. *), endIC (* And the size. *))
    end

    fun setLong (value, addrs, seg) =
    let
        val wordLength = wordSize
        
        fun putBytes(value, a, seg, i) =
        if i = wordLength then ()
        else
        (
            byteVecSet(seg,
                if not isBigEndian then a+i else a+wordLength-i-0w1,
                Word8.fromInt(value mod 256));
            putBytes(value div 256, a, seg, i+0w1)
        )
    in
        putBytes(value, addrs, seg, 0w0)
    end

    (* Adds the constants onto the code, and copies the code into a new segment *)
    fun copyCode (cvec as
                    Code{ printAssemblyCode, printStream,
                           procName, constVec, stage1Code, ...}, maxStack, resultClosure) =
    let
        local
            val revCode = List.rev(!stage1Code)
            (* Add a stack check.  This is only needed if the
               function needs more than 128 words since the call and tail functions
               check for this much. *)
        in
            val codeList =
                if maxStack < 128
                then revCode
                else SimpleCode[opcode_stackSize16, Word8.fromInt maxStack, Word8.fromInt(maxStack div 256)] :: revCode
        end
        val (byteVec, endIC) = genCode(codeList, cvec)
        val wordLength = wordSize
  
        (* +3 for profile count, function name and constants count *)
        val numOfConst = List.length(! constVec)
        val endOfCode = endIC div wordLength
        val segSize   = endOfCode + Word.fromInt numOfConst + 0w4
        val firstConstant = endIC + wordLength * 0w3 (* Add 3 for fn name, unused and profile count. *)
    
        (* Put in the number of constants. This must go in before
           we actually put in any constants. *)
        local
            val addr = ((segSize - 0w1) * wordLength)
        in
            val () = setLong (numOfConst + 3, addr, byteVec) 
        end

        (* Now we've filled in all the size info we need to convert the segment
           into a proper code segment before it's safe to put in any ML values. *)
        val codeVec = byteVecToCodeVec(byteVec, resultClosure)

        local
            val name     : string = procName
            val nameWord : machineWord = toMachineWord name
        in
            val () = codeVecPutWord (codeVec, endOfCode, nameWord)
        end
        (* This used to be used on X86 for the register mask. *)
        val () = codeVecPutWord (codeVec, endOfCode+0w1, toMachineWord 1)
        (* Profile ref.  A byte ref used by the profiler in the RTS. *)
        local
            val v = RunCall.allocateByteMemory(0w1, Word.fromLargeWord(Word8.toLargeWord(Word8.orb(F_mutable, F_bytes))))
            fun clear 0w0 = ()
            |   clear i = (assignByte(v, i-0w1, 0w0); clear (i-0w1))
            val () = clear(wordSize)
        in
            val () = codeVecPutWord (codeVec, endOfCode+0w2, toMachineWord v)
        end

        (* and then copy the constants from the constant list. *)
        local
            fun setConstant(value, num) =
            let
                val constAddr = (firstConstant div wordLength) + num
            in
                codeVecPutWord (codeVec, constAddr, value);
                num+0w1
            end
        in
            val _ = List.foldl setConstant 0w0 (!constVec)
        end
    in
        if printAssemblyCode
        then (* print out the code *)
            (printCode (codeVec, procName, endIC, printStream); printStream"\n")
        else ();
        codeVecLock(codeVec, resultClosure)
    end (* copyCode *)
    
    fun addItemToList(item, Code{stage1Code, ...}) = stage1Code := item :: !stage1Code

    val genOpcode = addItemToList
    
    fun putBranchInstruction(brOp, label, cvec) =
        addItemToList(JumpInstruction{label=label, jumpType=brOp, size = ref Size32}, cvec)

    fun setLabel(label, cvec) = addItemToList(LabelCode label, cvec)
    
    fun createLabel () = { destination=ref 0w0 }
    
    local
        fun genOpc(opc, cvec) = addItemToList(SimpleCode [opc], cvec)
        and genExtOpc(opc, cvec) = addItemToList(SimpleCode [opcode_escape, opc], cvec)
        and genOpcByte(opc, arg1, cvec) = addItemToList(SimpleCode [opc, Word8.fromInt arg1], cvec)
        and genExtOpcByte(opc, arg1, cvec) = addItemToList(SimpleCode [opcode_escape, opc, Word8.fromInt arg1], cvec)
        and genOpcWord(opc, arg1, cvec) =
            addItemToList(SimpleCode[opc, Word8.fromInt arg1, Word8.fromInt (arg1 div 256)], cvec)
        
        open IEEEReal
        
        fun encodeRound TO_NEAREST = 0
        |   encodeRound TO_NEGINF = 1
        |   encodeRound TO_POSINF = 2
        |   encodeRound TO_ZERO = 3
    in
        fun genRaiseEx cvec = genOpc (opcode_raiseEx, cvec)
        fun genLock cvec = genOpc (opcode_lock, cvec)
        fun genLdexc cvec = genOpc (opcode_ldexc, cvec)
        fun genPushHandler cvec = genOpc (opcode_pushHandler, cvec)
    
        fun genRTSCallFast(0, cvec) = genOpc (opcode_callFastRTS0, cvec)
        |   genRTSCallFast(1, cvec) = genOpc (opcode_callFastRTS1, cvec)
        |   genRTSCallFast(2, cvec) = genOpc (opcode_callFastRTS2, cvec)
        |   genRTSCallFast(3, cvec) = genOpc (opcode_callFastRTS3, cvec)
        |   genRTSCallFast(4, cvec) = genOpc (opcode_callFastRTS4, cvec)
        |   genRTSCallFast(5, cvec) = genOpc (opcode_callFastRTS5, cvec)
        |   genRTSCallFast(_, _) = raise InternalError "genRTSFastCall"

        fun genRTSCallFull(0, cvec) = genOpc (opcode_callFullRTS0, cvec)
        |   genRTSCallFull(1, cvec) = genOpc (opcode_callFullRTS1, cvec)
        |   genRTSCallFull(2, cvec) = genOpc (opcode_callFullRTS2, cvec)
        |   genRTSCallFull(3, cvec) = genOpc (opcode_callFullRTS3, cvec)
        |   genRTSCallFull(4, cvec) = genOpc (opcode_callFullRTS4, cvec)
        |   genRTSCallFull(5, cvec) = genOpc (opcode_callFullRTS5, cvec)
        |   genRTSCallFull(_, _) = raise InternalError "genRTSCallFull"

        fun genContainer (size, cvec) =
            if size < 256
            then genOpcByte(opcode_containerB, size, cvec)
            else genOpcWord(opcode_containerW, size, cvec)

        fun genCase (nCases, cvec) =
        let
            val labels = List.tabulate(nCases, fn _ => createLabel())
        in
            addItemToList(IndexedCase{labels=labels, size=ref Size32}, cvec);
            labels
        end
        
        (* For the moment don't try to merge stack resets. *)
        fun resetStack(0, _, _) = ()

        |   resetStack(1, true, cvec) =
                addItemToList(SimpleCode[opcode_resetR_1], cvec)
        |   resetStack(2, true, cvec) =
                addItemToList(SimpleCode[opcode_resetR_2], cvec)
        |   resetStack(3, true, cvec) =
                addItemToList(SimpleCode[opcode_resetR_3], cvec)

        |   resetStack(offset, true, cvec) =
            if offset < 0 then raise InternalError "resetStack"
            else if offset > 255
            then genOpcWord(opcode_resetR_w, offset, cvec)
            else genOpcByte(opcode_resetRB, offset, cvec)
            
        |   resetStack(1, false, cvec) =
                addItemToList(SimpleCode[opcode_reset_1], cvec)
        |   resetStack(2, false, cvec) =
                addItemToList(SimpleCode[opcode_reset_2], cvec)
        
        |   resetStack(offset, false, cvec) =
            if offset < 0 then raise InternalError "resetStack"
            else if offset > 255
            then genOpcWord(opcode_resetW, offset, cvec)
            else genOpcByte(opcode_resetB, offset, cvec)

        fun genCallClosure(Code{stage1Code as ref(PushConstant{constNum, size, isCall=false} :: tail), ...}) =
            stage1Code := PushConstant{constNum=constNum, size=size, isCall=true} :: tail
        
        |   genCallClosure(Code{stage1Code as ref(LoadLocal w :: tail), ...}) =
            stage1Code := SimpleCode [opcode_callLocalB, w] :: tail

        |   genCallClosure(Code{stage1Code, ...}) =
            stage1Code := SimpleCode [opcode_callClosure] :: !stage1Code

        fun genTailCall (toslide, slideby, cvec) =
        if toslide < 256 andalso slideby < 256
        then (* General byte case *)
            addItemToList(SimpleCode[opcode_tailbb, Word8.fromInt toslide, Word8.fromInt slideby], cvec)          
        else (* General case. *)
                addItemToList(
                    SimpleCode[opcode_tail, Word8.fromInt toslide, Word8.fromInt(toslide div 256),
                               Word8.fromInt slideby, Word8.fromInt (slideby div 256)], cvec)

        fun pushConst (value : machineWord, cvec) =
            if isShort value andalso toShort value < 0w32768
            then
            let
                val iVal = Word.toInt (toShort value);
            in
                case iVal of
                    10 => genOpc (opcode_const_10, cvec)
                |   0  => genOpc (opcode_const_0, cvec)
                |   1  => genOpc (opcode_const_1, cvec)
                |   2  => genOpc (opcode_const_2, cvec)
                |   3  => genOpc (opcode_const_3, cvec)
                |   4  => genOpc (opcode_const_4, cvec)
       
                |   _ => if iVal < 256
                    then genOpcByte (opcode_constIntB, iVal, cvec)
                    else genOpcWord (opcode_constIntW, iVal, cvec)
            end

            else (* address or large short *)
                addItemToList(PushConstant{constNum = addConstToVec(value, cvec), size=ref Size32, isCall=false}, cvec)

        fun genRTSCallFastRealtoReal cvec = genExtOpc (ext_opcode_callFastRTSRtoR, cvec)
        and genRTSCallFastRealRealtoReal cvec = genOpc (opcode_callFastRTSRRtoR, cvec)
        and genRTSCallFastGeneraltoReal cvec = genExtOpc (ext_opcode_callFastRTSGtoR, cvec)
        and genRTSCallFastRealGeneraltoReal cvec = genOpc (opcode_callFastRTSRGtoR, cvec)
        
        and genRTSCallFastFloattoFloat cvec = genExtOpc (ext_opcode_callFastRTSFtoF, cvec)
        and genRTSCallFastFloatFloattoFloat cvec = genExtOpc (ext_opcode_callFastRTSFFtoF, cvec)
        and genRTSCallFastGeneraltoFloat cvec = genOpc (ext_opcode_callFastRTSGtoF, cvec)
        and genRTSCallFastFloatGeneraltoFloat cvec = genExtOpc (ext_opcode_callFastRTSFGtoF, cvec)
        
        fun genDoubleToFloat(SOME rnding, cvec) = genExtOpcByte(ext_opcode_realToFloat, encodeRound rnding, cvec)
        |   genDoubleToFloat(NONE, cvec) = genExtOpcByte(ext_opcode_realToFloat, 5, cvec)

        and genRealToInt(rnding, cvec) = genExtOpcByte(ext_opcode_realToInt, encodeRound rnding, cvec)
        and genFloatToInt(rnding, cvec) = genExtOpcByte(ext_opcode_floatToInt, encodeRound rnding, cvec)
    end
    
    local
        fun gen1 (opW, opB, opF, first, arg1, cvec) =
            if first <= arg1 andalso arg1 < first+List.length opF
            then addItemToList(SimpleCode[List.nth(opF, arg1 - first)], cvec)

            else if 0 <= arg1 andalso arg1 <= 255
            then addItemToList(SimpleCode [opB, Word8.fromInt arg1], cvec)

            else addItemToList(
                SimpleCode [opW, Word8.fromInt arg1, Word8.fromInt(arg1 div 256)], cvec)
    in
        fun genReturn(1, cvec) = addItemToList(SimpleCode[opcode_return_1], cvec)
        |   genReturn(2, cvec) = addItemToList(SimpleCode[opcode_return_2], cvec)
        |   genReturn(3, cvec) = addItemToList(SimpleCode[opcode_return_3], cvec)
        |   genReturn(arg1, cvec) =
                addItemToList(SimpleCode(
                    if 0 <= arg1 andalso arg1 <= 255
                    then [opcode_returnB, Word8.fromInt arg1]
                    else [opcode_returnW, Word8.fromInt arg1, Word8.fromInt(arg1 div 256)]),
                    cvec)

        (*fun genLocal (arg1, cvec) =
        let
            val ops = [opcode_local_0, opcode_local_1, opcode_local_2, opcode_local_3, opcode_local_4,
                       opcode_local_5, opcode_local_6, opcode_local_7, opcode_local_8, opcode_local_9,
                       opcode_local_10, opcode_local_11, opcode_local_12]
        in
            gen1 (opcode_localW, opcode_localB, ops, 0, arg1, cvec)
        end*)
        
        fun genLocal (arg1, cvec) =
            if 0 <= arg1 andalso arg1 <= 256 then addItemToList(LoadLocal(Word8.fromInt arg1), cvec)
            else addItemToList(SimpleCode[opcode_localW, Word8.fromInt arg1, Word8.fromInt(arg1 div 256)], cvec)
        
        fun genIndirectSimple(arg1, cvec) =
        let
            val ops = [opcode_indirect_0, opcode_indirect_1, opcode_indirect_2, opcode_indirect_3,
                       opcode_indirect_4, opcode_indirect_5]
        in
            gen1 (opcode_indirectW, opcode_indirectB, ops, 0, arg1, cvec)
        end
        
        fun genIndirect (arg1, cvec as Code{stage1Code as ref(LoadLocal w :: tail), ...}) =
            if 0 <= arg1 andalso arg1 <= 255
            then stage1Code := SimpleCode [opcode_indirectLocalBB, w, Word8.fromInt arg1] :: tail
            else genIndirectSimple(arg1, cvec)

        |   genIndirect (arg1, cvec) = genIndirectSimple(arg1, cvec)

        
        (* genMoveToVec is now only used for mutually recursive closures. *)
        fun genMoveToVec (arg1, cvec) =
            gen1 (opcode_moveToVecW, opcode_moveToVecB, [], 0, arg1, cvec)

        fun genSetStackVal (arg1, cvec) =
            gen1 (opcode_setStackValW, opcode_setStackValB, [], 0, arg1, cvec)

        fun genTuple (arg1, cvec) =
        let
            val ops = [opcode_tuple_2, opcode_tuple_3, opcode_tuple_4]
        in
            gen1 (opcode_tupleW, opcode_tupleB, ops, 2, arg1, cvec)
        end
    end
    
    fun genEnterIntCatch _ = ()
    and genEnterIntCall _ = ()

    val opcode_notBoolean       = SimpleCode [opcode_notBoolean]
    val opcode_isTagged         = SimpleCode [opcode_isTagged]
    and opcode_cellLength       = SimpleCode [opcode_cellLength]
    and opcode_cellFlags        = SimpleCode [opcode_cellFlags]
    and opcode_clearMutable     = SimpleCode [opcode_clearMutable]
    and opcode_atomicIncr       = SimpleCode [opcode_atomicIncr]
    and opcode_atomicDecr       = SimpleCode [opcode_atomicDecr]
    and opcode_atomicReset      = SimpleCode [opcode_escape, ext_opcode_atomicReset]
    and opcode_longWToTagged    = SimpleCode [opcode_escape, ext_opcode_longWToTagged]
    and opcode_signedToLongW    = SimpleCode [opcode_escape, ext_opcode_signedToLongW]
    and opcode_unsignedToLongW  = SimpleCode [opcode_escape, ext_opcode_unsignedToLongW]
    and opcode_realAbs          = SimpleCode [opcode_escape, ext_opcode_realAbs]
    and opcode_realNeg          = SimpleCode [opcode_escape, ext_opcode_realNeg]
    and opcode_fixedIntToReal   = SimpleCode [opcode_escape, ext_opcode_fixedIntToReal]
    and opcode_fixedIntToFloat  = SimpleCode [opcode_escape, ext_opcode_fixedIntToFloat]
    and opcode_floatToReal      = SimpleCode [opcode_escape, ext_opcode_floatToReal]
    
    val opcode_equalWord        = SimpleCode [opcode_equalWord]
    and opcode_lessSigned       = SimpleCode [opcode_lessSigned]
    and opcode_lessUnsigned     = SimpleCode [opcode_lessUnsigned]
    and opcode_lessEqSigned     = SimpleCode [opcode_lessEqSigned]
    and opcode_lessEqUnsigned   = SimpleCode [opcode_lessEqUnsigned]
    and opcode_greaterSigned    = SimpleCode [opcode_greaterSigned]
    and opcode_greaterUnsigned  = SimpleCode [opcode_greaterUnsigned]
    and opcode_greaterEqSigned  = SimpleCode [opcode_greaterEqSigned]
    and opcode_greaterEqUnsigned = SimpleCode [opcode_greaterEqUnsigned]

    val opcode_fixedAdd         = SimpleCode [opcode_fixedAdd]
    val opcode_fixedSub         = SimpleCode [opcode_fixedSub]
    val opcode_fixedMult        = SimpleCode [opcode_fixedMult]
    val opcode_fixedQuot        = SimpleCode [opcode_fixedQuot]
    val opcode_fixedRem         = SimpleCode [opcode_fixedRem]
    val opcode_fixedDiv         = SimpleCode [opcode_escape, ext_opcode_fixedDiv]
    val opcode_fixedMod         = SimpleCode [opcode_escape, ext_opcode_fixedMod]
    val opcode_wordAdd          = SimpleCode [opcode_wordAdd]
    val opcode_wordSub          = SimpleCode [opcode_wordSub]
    val opcode_wordMult         = SimpleCode [opcode_wordMult]
    val opcode_wordDiv          = SimpleCode [opcode_wordDiv]
    val opcode_wordMod          = SimpleCode [opcode_wordMod]
    val opcode_wordAnd          = SimpleCode [opcode_wordAnd]
    val opcode_wordOr           = SimpleCode [opcode_wordOr]
    val opcode_wordXor          = SimpleCode [opcode_wordXor]
    val opcode_wordShiftLeft    = SimpleCode [opcode_wordShiftLeft]
    val opcode_wordShiftRLog    = SimpleCode [opcode_wordShiftRLog]
    val opcode_wordShiftRArith  = SimpleCode [opcode_escape, ext_opcode_wordShiftRArith]
    val opcode_allocByteMem     = SimpleCode [opcode_allocByteMem]
    val opcode_lgWordEqual      = SimpleCode [opcode_escape, ext_opcode_lgWordEqual]
    val opcode_lgWordLess       = SimpleCode [opcode_escape, ext_opcode_lgWordLess]
    val opcode_lgWordLessEq     = SimpleCode [opcode_escape, ext_opcode_lgWordLessEq]
    val opcode_lgWordGreater    = SimpleCode [opcode_escape, ext_opcode_lgWordGreater]
    val opcode_lgWordGreaterEq  = SimpleCode [opcode_escape, ext_opcode_lgWordGreaterEq]
    val opcode_lgWordAdd        = SimpleCode [opcode_escape, ext_opcode_lgWordAdd]
    val opcode_lgWordSub        = SimpleCode [opcode_escape, ext_opcode_lgWordSub]
    val opcode_lgWordMult       = SimpleCode [opcode_escape, ext_opcode_lgWordMult]
    val opcode_lgWordDiv        = SimpleCode [opcode_escape, ext_opcode_lgWordDiv]
    val opcode_lgWordMod        = SimpleCode [opcode_escape, ext_opcode_lgWordMod]
    val opcode_lgWordAnd        = SimpleCode [opcode_escape, ext_opcode_lgWordAnd]
    val opcode_lgWordOr         = SimpleCode [opcode_escape, ext_opcode_lgWordOr]
    val opcode_lgWordXor        = SimpleCode [opcode_escape, ext_opcode_lgWordXor]
    val opcode_lgWordShiftLeft  = SimpleCode [opcode_escape, ext_opcode_lgWordShiftLeft]
    val opcode_lgWordShiftRLog  = SimpleCode [opcode_escape, ext_opcode_lgWordShiftRLog]
    val opcode_lgWordShiftRArith = SimpleCode [opcode_escape, ext_opcode_lgWordShiftRArith]
    val opcode_realEqual        = SimpleCode [opcode_escape, ext_opcode_realEqual]
    val opcode_realLess         = SimpleCode [opcode_escape, ext_opcode_realLess]
    val opcode_realLessEq       = SimpleCode [opcode_escape, ext_opcode_realLessEq]
    val opcode_realGreater      = SimpleCode [opcode_escape, ext_opcode_realGreater]
    val opcode_realGreaterEq    = SimpleCode [opcode_escape, ext_opcode_realGreaterEq]
    val opcode_realUnordered    = SimpleCode [opcode_escape, ext_opcode_realUnordered]
    val opcode_realAdd          = SimpleCode [opcode_escape, ext_opcode_realAdd]
    val opcode_realSub          = SimpleCode [opcode_escape, ext_opcode_realSub]
    val opcode_realMult         = SimpleCode [opcode_escape, ext_opcode_realMult]
    val opcode_realDiv          = SimpleCode [opcode_escape, ext_opcode_realDiv]
    and opcode_floatAbs         = SimpleCode [opcode_escape, ext_opcode_floatAbs]
    and opcode_floatNeg         = SimpleCode [opcode_escape, ext_opcode_floatNeg]
    val opcode_floatEqual       = SimpleCode [opcode_escape, ext_opcode_floatEqual]
    val opcode_floatLess        = SimpleCode [opcode_escape, ext_opcode_floatLess]
    val opcode_floatLessEq      = SimpleCode [opcode_escape, ext_opcode_floatLessEq]
    val opcode_floatGreater     = SimpleCode [opcode_escape, ext_opcode_floatGreater]
    val opcode_floatGreaterEq   = SimpleCode [opcode_escape, ext_opcode_floatGreaterEq]
    val opcode_floatUnordered   = SimpleCode [opcode_escape, ext_opcode_floatUnordered]
    val opcode_floatAdd         = SimpleCode [opcode_escape, ext_opcode_floatAdd]
    val opcode_floatSub         = SimpleCode [opcode_escape, ext_opcode_floatSub]
    val opcode_floatMult        = SimpleCode [opcode_escape, ext_opcode_floatMult]
    val opcode_floatDiv         = SimpleCode [opcode_escape, ext_opcode_floatDiv]
    val opcode_getThreadId      = SimpleCode [opcode_getThreadId]
    val opcode_allocWordMemory  = SimpleCode [opcode_allocWordMemory]
    val opcode_alloc_ref        = SimpleCode [opcode_alloc_ref]
    val opcode_loadMLWord       = SimpleCode [opcode_loadMLWord]
    val opcode_loadMLByte       = SimpleCode [opcode_loadMLByte]
    val opcode_loadC8           = SimpleCode [opcode_escape, ext_opcode_loadC8]
    val opcode_loadC16          = SimpleCode [opcode_escape, ext_opcode_loadC16]
    val opcode_loadC32          = SimpleCode [opcode_escape, ext_opcode_loadC32]
    val opcode_loadC64          = SimpleCode [opcode_escape, ext_opcode_loadC64]
    val opcode_loadCFloat       = SimpleCode [opcode_escape, ext_opcode_loadCFloat]
    val opcode_loadCDouble      = SimpleCode [opcode_escape, ext_opcode_loadCDouble]
    val opcode_loadUntagged     = SimpleCode [opcode_loadUntagged]
    val opcode_storeMLWord      = SimpleCode [opcode_storeMLWord]
    val opcode_storeMLByte      = SimpleCode [opcode_storeMLByte]
    val opcode_storeC8          = SimpleCode [opcode_escape, ext_opcode_storeC8]
    val opcode_storeC16         = SimpleCode [opcode_escape, ext_opcode_storeC16]
    val opcode_storeC32         = SimpleCode [opcode_escape, ext_opcode_storeC32]
    val opcode_storeC64         = SimpleCode [opcode_escape, ext_opcode_storeC64]
    val opcode_storeCFloat      = SimpleCode [opcode_escape, ext_opcode_storeCFloat]
    val opcode_storeCDouble     = SimpleCode [opcode_escape, ext_opcode_storeCDouble]
    val opcode_storeUntagged    = SimpleCode [opcode_storeUntagged]
    val opcode_blockMoveWord    = SimpleCode [opcode_blockMoveWord]
    val opcode_blockMoveByte    = SimpleCode [opcode_blockMoveByte]
    val opcode_blockEqualByte   = SimpleCode [opcode_blockEqualByte]
    val opcode_blockCompareByte = SimpleCode [opcode_blockCompareByte]
    val opcode_deleteHandler    = SimpleCode [opcode_deleteHandler]

    structure Sharing =
    struct
        type code = code
        type opcode = opcode
        type labels = labels
        type closureRef = closureRef
    end

end;

