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
    type machineWord = Address.machineWord
    type short = Address.short
    type address = Address.address
    type code
    type pretty
    eqtype reg   (* Machine registers *)

    val regNone:     reg option
    val regResult:   reg
    val regClosure:  reg
    val regStackPtr: reg
    val regHandler:  reg
    val regReturn:   reg option

    val regs:    int     (* No of registers. *)
    val argRegs: int     (* No of args in registers. *)

    val regN:   int -> reg
    val nReg:   reg -> int
    val argReg: int -> reg

    val regRepr: reg -> string

    structure RegSet:
    sig
        type regSet
        val singleton: reg -> regSet
        val allRegisters: regSet
        val noRegisters: regSet
        val isAllRegs: regSet->bool
        val regSetUnion: regSet*regSet -> regSet
        val listToSet: reg list -> regSet
        val inverseSet: regSet -> regSet
        val inSet: reg * regSet -> bool
        val cardinality: regSet -> int
    end

    type addrs
    val addrZero: addrs

    (* Operations. *)
    type instrs
    val instrVeclen: instrs
    val instrVecflags: instrs
    val instrGetFirstLong: instrs
    val instrStringLength: instrs

    val instrAddA: instrs
    and instrSubA: instrs
    and instrMulA: instrs
    and instrAddW: instrs
    and instrSubW: instrs
    and instrMulW: instrs
    and instrDivW: instrs
    and instrModW: instrs
    and instrOrW: instrs
    and instrAndW: instrs
    and instrXorW: instrs
    and instrLoad: instrs
    and instrLoadB: instrs
    and instrUpshiftW: instrs    (* logical shift left *)
    and instrDownshiftW: instrs  (* logical shift right *)
    and instrDownshiftArithW: instrs  (* arithmetic shift right *)
    and instrSetStringLength: instrs
    and instrThreadSelf: instrs
    and instrAtomicIncr: instrs
    and instrAtomicDecr: instrs
    and instrStoreW: instrs
    and instrStoreB: instrs
    and instrLockSeg: instrs
    and instrAddFP: instrs
    and instrSubFP: instrs
    and instrMulFP: instrs
    and instrDivFP: instrs
    and instrNegFP: instrs
    and instrIntToRealFP: instrs
    and instrRealToIntFP: instrs
    and instrSqrtFP: instrs
    and instrSinFP: instrs
    and instrCosFP: instrs
    and instrAtanFP: instrs
    and instrExpFP: instrs
    and instrLnFP: instrs

    (* Check whether an operation is implemented and, if appropriate, remove
       constant values into the instruction part. *)
    val checkAndReduce: instrs * 'a list * ('a -> machineWord option) -> (instrs * 'a list) option

    val isPushI: machineWord -> bool

    type tests
    val testNeqW:  tests
    val testEqW:   tests
    val testGeqW:  tests
    val testGtW:   tests
    val testLeqW:  tests
    val testLtW:   tests
    val testNeqA:  tests
    val testEqA:   tests
    val testGeqA:  tests
    val testGtA:   tests
    val testLeqA:  tests
    val testLtA:   tests
    val Short:     tests
    val Long:      tests
    val testNeqFP: tests
    val testEqFP:  tests
    val testGeqFP: tests
    val testGtFP:  tests
    val testLeqFP: tests
    val testLtFP:  tests

    type labels (* The source of a jump. *)

    val noJump: labels

    (* Compare and branch for fixed and arbitrary precision. *)
    val checkAndReduceBranches: tests * 'a list * ('a -> machineWord option) -> (tests * 'a list) option

(*    val isIndexedStore: unit -> bool
    val isStoreI:       machineWord * storeWidth * bool -> bool*)
    val preferLoadPush: bool

    datatype callKinds =
        Recursive
    |   ConstantFun of machineWord * bool
    |   CodeFun of code
    |   FullCall

    val procName:   code -> string      (* Name of the procedure. *)

    val useIndexedCase: { min: word, max: word, number: int, isExhaustive: bool } -> bool

    type cases

    val constrCases : word * addrs ref -> cases

    datatype source =
        LiteralSource of machineWord
    |   InRegister of reg
    |   BaseOffset of reg * int
    |   CodeRefSource of code (* The address of another function *)
    |   StackAddress of int (* Offset within the stack. *)

    type operations

    val moveOp: { source: source, output: reg } -> operations
    val pushToStack: source -> operations
    val storeToMemory: { toStore: source, offset: int, base: reg, index: reg option } -> operations
    val allocStore: { size: int, flags: Word8.word, output: reg } -> operations
    val callFunction: { callKind: callKinds } -> operations
    val jumpToFunction: { callKind: callKinds, returnReg: reg option } -> operations
    val returnFromFunction: { returnReg: reg option, argsToRemove: int } -> operations
    val raiseException: operations
    val uncondBranch: unit -> operations * labels ref
    val resetStack: int -> operations
    val backJumpLabel: unit -> operations * addrs ref
    val jumpBack: { dest: addrs ref, addStackCheck: bool } -> operations
    val forwardJumpLabel: { label: labels ref } -> operations
    val loadHandlerAddress: { handlerLab: addrs ref, output: reg } -> operations
    val startHandler: { handlerLab: addrs ref } -> operations
    val indexedCase:
            { testReg: reg, workReg: reg, minCase: word, maxCase: word,
              isArbitrary: bool, isExhaustive: bool, tableAddrRef: addrs ref } -> operations
    val fillJumpTable:
            { tableAddr: addrs ref, cases: cases list, default: addrs ref, min: word, max: word } -> operations

    val prettyOperation: operations * int -> pretty

    datatype regHint = UseReg of reg | NoHint | NoResult

    type argReq = source * bool

    datatype argAction =
        ActionDone of (* The output register if any and the final operation. *)
            { outReg: reg option, operation: operations }
    |   ActionLockRegister of (* Lock the register of an argument. *)
            { argNo: int, reg: reg, willOverwrite: bool, next: nextAction }
    |   ActionLoadArg of (* Load an argument into a register. *)
            { argNo: int, regSet: RegSet.regSet, willOverwrite: bool, next: nextAction }
    |   ActionGetWorkReg of (* Get a work/result register. *)
            { regSet: RegSet.regSet, setReg: reg -> nextAction }

    withtype nextAction = argReq list -> argAction

    (* Negotiate arguments *)
    val negotiateArguments: instrs * regHint -> nextAction
    val negotiateTestArguments: tests -> nextAction * labels ref

    val codeCreate: bool * string * Universal.universal list -> code  (* makes the initial segment. *)
    (* Code generate operations and construct the final code. *)
    val copyCode: code * operations list * int * reg list -> address

    val codeAddress: code -> address option

    val traceContext: code -> string

    structure Sharing:
    sig
        type code       = code
        and  instrs     = instrs
        and  reg        = reg
        and  tests      = tests
        and  addrs      = addrs
        and  operations = operations
        and  source     = source
        and  regHint    = regHint
        and  argAction  = argAction
        and  regSet     = RegSet.regSet
        and  pretty     = pretty
    end
end;
