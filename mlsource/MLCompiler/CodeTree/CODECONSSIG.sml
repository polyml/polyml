(*
    Copyright (c) 2009 David C.J. Matthews
    
    Derived from original code:

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

signature CODECONSSIG =
sig
    type machineWord = Address.machineWord;
    type short = Address.short;
    type address = Address.address;
    type code;
    type reg;   (* Machine registers *)

    val regNone:     reg option;
    val regResult:   reg;
    val regClosure:  reg;
    val regStackPtr: reg;
    val regHandler:  reg;
    val regReturn:   reg option;

    val regs:    int;     (* No of registers. *)
    val argRegs: int;     (* No of args in registers. *)

    val regN:   int -> reg;
    val nReg:   reg -> int;
    val argReg: int -> reg;

    val regEq:    reg * reg -> bool;
    val regNeq:   reg * reg -> bool;

    val regRepr: reg -> string;

    type addrs
    val addrZero: addrs

    (* Operations. *)
    datatype instrs = 
        instrMove
    |   instrAddA
    |   instrSubA
    |   instrRevSubA
    |   instrMulA
    |   instrAddW
    |   instrSubW
    |   instrRevSubW
    |   instrMulW
    |   instrDivW
    |   instrModW
    |   instrOrW
    |   instrAndW
    |   instrXorW
    |   instrLoad
    |   instrLoadB
    |   instrVeclen
    |   instrVecflags
    |   instrPush
    |   instrUpshiftW    (* logical shift left *)
    |   instrDownshiftW  (* logical shift right *)
    |   instrDownshiftArithW  (* arithmetic shift right *)
    |   instrGetFirstLong
    |   instrStringLength
    |   instrSetStringLength
    |   instrBad;

    (* Can the we use the same register as the source and destination
     of an instructions? *)
    val canShareRegs : bool;

    (* Enquire about operations. *)
    val instrIsRR: instrs -> bool;         (* Is the general form implemented? *)
    val instrIsRI: instrs * machineWord -> bool; (* Is the immediate value ok? *)

    type tests;

    val testNeqW:  tests;
    val testEqW:   tests;
    val testGeqW:  tests;
    val testGtW:   tests;
    val testLeqW:  tests;
    val testLtW:   tests;
    val testNeqA:  tests;
    val testEqA:   tests;
    val testGeqA:  tests;
    val testGtA:   tests;
    val testLeqA:  tests;
    val testLtA:   tests;
    val Short:     tests;
    val Long:      tests;

    type labels; (* The source of a jump. *)

    val noJump: labels;

    (* Compare and branch for fixed and arbitrary precision. *)

    val isCompRR: tests -> bool;
    val isCompRI: tests * machineWord -> bool;

    datatype storeWidth = STORE_WORD | STORE_BYTE

    val isIndexedStore: storeWidth -> bool
    val isStoreI:       machineWord * storeWidth * bool -> bool;
    val preferLoadPush: bool;

    datatype callKinds =
        Recursive
    |   ConstantFun of machineWord * bool
    |   CodeFun of code
    |   FullCall

    val procName:   code -> string;      (* Name of the procedure. *)

    val useIndexedCase: { min: word, max: word, number: int, isExhaustive: bool } -> bool;

    type cases

    val constrCases : word * addrs ref -> cases;

    datatype operations =
        OpRR of { instr: instrs, left: reg, right: reg, output: reg }
    |   OpRI of { instr: instrs, left: reg, right: machineWord, output: reg }
    |   CondBranchRR of { left: reg, right: reg, test: tests, label: labels ref }
    |   CondBranchRI of { left: reg, right: machineWord, test: tests, label: labels ref }
    |   LoadMemory of { offset: int, base: reg, output: reg }
    |   PushRegister of reg
    |   PushMemory of { offset: int, base: reg }
    |   StoreMemory of { toStore: reg, offset: int, base: reg, width: storeWidth, index: reg option }
    |   StoreConstant of { toStore: machineWord, offset: int, base: reg, width: storeWidth, index: reg option }
    |   AllocStore of { size: int, flags: Word8.word, output: reg }
    |   SetFlag of { base: reg, flags: Word8.word }
    |   CallFunction of { callKind: callKinds }
    |   JumpToFunction of { callKind: callKinds, returnReg: reg option }
    |   ReturnFromFunction of { returnReg: reg option, argsToRemove: int }
    |   RaiseException
    |   UncondBranch of { label: labels ref }
    |   LoadCodeRef of { refCode: code, output: reg }
    |   LoadStackAddress of { offset: int, output: reg }
    |   ResetStack of int
    |   BackJumpLabel of { dest: addrs ref }
    |   JumpBack of { dest: addrs ref, addStackCheck: bool }
    |   ForwardJumpLabel of { label: labels ref }
    |   LoadHandlerAddress of { handlerLab: addrs ref, output: reg }
    |   StartHandler of { handlerLab: addrs ref }
    |   IndexedCase of
            { testReg: reg, workReg: reg, minCase: word, maxCase: word,
              isArbitrary: bool, isExhaustive: bool, tableAddrRef: addrs ref }
    |   FillJumpTable of
            { tableAddr: addrs ref, cases: cases list, default: addrs ref, min: word, max: word }

    (* Code generate operations. *)
    val codeGenerate: operations list * code -> unit
 
    val copyCode: code * int * reg list -> address;
    val codeCreate: bool * string * Universal.universal list -> code;  (* makes the initial segment. *)
    val completeSegment: code -> unit;

    val inlineAssignments: bool

    val codeAddress: code -> address option

    val traceContext: code -> string;

    structure Sharing:
    sig
        type code       = code
        and  instrs     = instrs
        and  reg        = reg
        and  tests      = tests
        and  addrs      = addrs
        and  storeWidth = storeWidth
        and  operations = operations
    end
end;
