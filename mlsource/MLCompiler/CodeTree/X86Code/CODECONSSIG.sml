(*
    Copyright (c) David C.J. Matthews 2009, 2012
    
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
    eqtype reg   (* Machine registers *)
    
    datatype argType = ArgGeneral | ArgFP

    val sameCode: code * code -> bool

    val regNone:     reg option
    val regClosure:  reg
    val regStackPtr: reg

    (* For vector indexing we provide a numbering for the registers. *)
    val regs:   int
    val regN:   int -> reg
    val nReg:   reg -> int

    val regRepr: reg -> string

    val argRegs: argType list -> reg option list
    val resultReg: argType -> reg

    structure RegSet:
    sig
        eqtype regSet
        val singleton: reg -> regSet
        val allRegisters: regSet (* All registers: data, address, floating pt. *)
        val generalRegisters: regSet (* Registers checked by the GC. *)
        val floatingPtRegisters: regSet
        val noRegisters: regSet
        val isAllRegs: regSet->bool
        val regSetUnion: regSet * regSet -> regSet
        val regSetIntersect: regSet * regSet -> regSet
        val listToSet: reg list -> regSet
        val setToList: regSet -> reg list
        val regSetMinus: regSet * regSet -> regSet
        val inSet: reg * regSet -> bool
        val cardinality: regSet -> int
        val regSetRepr: regSet -> string
        val oneOf: regSet -> reg
    end
    val getRegisterSet: Word.word -> RegSet.regSet

    type addrs
    val addrZero: addrs

    (* Operations. *)
    type 'a instrs
    val instrVeclen: 'a instrs
    val instrVecflags: 'a instrs
    val instrGetFirstLong: 'a instrs
    val instrStringLength: 'a instrs

    val instrAddA: 'a instrs
    and instrSubA: 'a instrs
    and instrMulA: 'a instrs
    and instrAddW: 'a instrs
    and instrSubW: 'a instrs
    and instrMulW: 'a instrs
    and instrDivW: 'a instrs
    and instrModW: 'a instrs
    and instrOrW: 'a instrs
    and instrAndW: 'a instrs
    and instrXorW: 'a instrs
    and instrLoad: 'a instrs
    and instrLoadB: 'a instrs
    and instrUpshiftW: 'a instrs    (* logical shift left *)
    and instrDownshiftW: 'a instrs  (* logical shift right *)
    and instrDownshiftArithW: 'a instrs  (* arithmetic shift right *)
    and instrSetStringLength: 'a instrs
    and instrThreadSelf: 'a instrs
    and instrAtomicIncr: 'a instrs
    and instrAtomicDecr: 'a instrs
    and instrStoreW: 'a instrs
    and instrStoreB: 'a instrs
    and instrLockSeg: 'a instrs
    and instrAddFP: 'a instrs
    and instrSubFP: 'a instrs
    and instrMulFP: 'a instrs
    and instrDivFP: 'a instrs
    and instrAbsFP: 'a instrs
    and instrNegFP: 'a instrs
    and instrIntToRealFP: 'a instrs
    and instrRealToIntFP: 'a instrs
    and instrSqrtFP: 'a instrs
    and instrSinFP: 'a instrs
    and instrCosFP: 'a instrs
    and instrAtanFP: 'a instrs
    and instrExpFP: 'a instrs
    and instrLnFP: 'a instrs
    and instrAllocStore: 'a instrs
    and instrMoveBytes: 'a instrs
    and instrMoveWords: 'a instrs

    (* Check whether an operation is implemented and, if appropriate, remove
       constant values into the instruction part. *)
    type negotiation
    val checkAndReduce: 'a instrs * 'a list * ('a -> machineWord option) -> (negotiation * 'a list) option

    val isPushI: machineWord -> bool

    type 'a tests
    val testNeqW:  'a tests
    val testEqW:   'a tests
    val testGeqW:  'a tests
    val testGtW:   'a tests
    val testLeqW:  'a tests
    val testLtW:   'a tests
    val testNeqA:  'a tests
    val testEqA:   'a tests
    val testGeqA:  'a tests
    val testGtA:   'a tests
    val testLeqA:  'a tests
    val testLtA:   'a tests
    val Short:     'a tests
    val Long:      'a tests
    val testNeqFP: 'a tests
    val testEqFP:  'a tests
    val testGeqFP: 'a tests
    val testGtFP:  'a tests
    val testLeqFP: 'a tests
    val testLtFP:  'a tests
    val byteVecEq: 'a tests
    and byteVecNe: 'a tests

    type forwardLabel
    and  backwardLabel

    (* Compare and branch for fixed and arbitrary precision. *)
    type negotiateTests
    val checkAndReduceBranches: 'a tests * 'a list * ('a -> machineWord option) -> (negotiateTests * 'a list) option

    datatype callKinds =
        Recursive
    |   ConstantClosure of machineWord
    |   ConstantCode of machineWord
    |   CodeFun of code
    |   FullCall

    val procName:   code -> string      (* Name of the procedure. *)

    type operation
    type operations = operation list

    val moveRegisterToRegister: reg(*source*) * reg(*dest*) -> operations
    and moveMemoryToRegister: reg (*base*) * int (*offset*) * reg (*dest*) -> operations
    and moveConstantToRegister: machineWord * reg -> operations
    and moveCodeRefToRegister: code * reg -> operations (* The address of another function *)
    and moveStackAddress: int * reg -> operations (* Offset within the stack. *)

    val pushRegisterToStack: reg -> operations
    and pushConstantToStack: machineWord -> operations
    and pushMemoryToStack: reg * int -> operations
    
    val storeRegisterToStack: reg * int -> operations
    and storeConstantToStack: machineWord * int -> operations

    val allocStore: { size: int, flags: Word8.word, output: reg } -> operations
    val allocationComplete: operations
    val callFunction: callKinds -> operations
    val jumpToFunction: callKinds  -> operations
    val returnFromFunction: int -> operations
    val raiseException: operations
    val uncondBranch: unit -> operations * forwardLabel
    val resetStack: int -> operations
    val backJumpLabel: unit -> operations * backwardLabel
    val jumpBack: backwardLabel -> operations
    val interruptCheck: operations
    val forwardJumpLabel: forwardLabel -> operations
    val loadHandlerAddress: { handlerLab: addrs ref, output: reg } -> operations
    val startHandler: { handlerLab: addrs ref } -> operations
    val indexedCase:
            { testReg: reg, workReg: reg, minCase: word, maxCase: word,
              isArbitrary: bool, isExhaustive: bool } -> operations * forwardLabel list * forwardLabel
    val activeRegister: reg -> operations
    val freeRegister: reg -> operations
    val pushToReserveSpace: operations
    val loadCurrentHandler: reg -> operations
    val storeToHandler: reg -> operations
    val pushCurrentHandler: operations

    val printOperation: operation * (string -> unit) -> unit

    datatype regHint = UseReg of RegSet.regSet | NoHint | NoResult

    (* These are almost the same as source values except that a value
       may be in more than one register. *)
    datatype actionSource =
        ActLiteralSource of machineWord
    |   ActInRegisterSet of { modifiable: RegSet.regSet, readable: RegSet.regSet}
    |   ActBaseOffset of reg * int
    |   ActCodeRefSource of code (* The address of another function *)
    |   ActStackAddress of int (* Offset within the stack. *)

    datatype argAction =
        ActionDone of (* The output register if any and the final operation. *)
            { outReg: reg option, operation: operations }
    |   ActionLockRegister of (* Lock the register of an argument. *)
            { argNo: int, reg: reg, willOverwrite: bool, next: nextAction }
    |   ActionLoadArg of (* Load an argument into a register. *)
            { argNo: int, regSet: RegSet.regSet, willOverwrite: bool, next: nextAction }
    |   ActionGetWorkReg of (* Get a work/result register. *)
            { regSet: RegSet.regSet, setReg: reg -> nextAction }

    withtype nextAction = actionSource list -> argAction

    (* Negotiate arguments *)
    val negotiateArguments: negotiation * regHint -> nextAction
    val negotiateTestArguments: negotiateTests -> nextAction * forwardLabel

    val codeCreate: bool * string * machineWord * Universal.universal list -> code  (* makes the initial segment. *)
    (* Code generate operations and construct the final code. *)
    val copyCode: code * operations * int * RegSet.regSet * bool -> address

    val codeAddress: code -> address option
    val addCompletionHook: code * (code * machineWord -> unit) -> unit

    structure Sharing:
    sig
        type code           = code
        and  'a instrs      = 'a instrs
        and  negotiation    = negotiation
        and  negotiateTests = negotiateTests
        and  reg            = reg
        and  'a tests       = 'a tests
        and  addrs          = addrs
        and  operation      = operation
        and  regHint        = regHint
        and  argAction      = argAction
        and  regSet         = RegSet.regSet
        and  backwardLabel  = backwardLabel
        and  forwardLabel  = forwardLabel
    end
end;
