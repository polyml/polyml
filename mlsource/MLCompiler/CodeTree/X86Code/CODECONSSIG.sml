(*
    Copyright (c) David C.J. Matthews 2015

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

signature CODECONSSIG =
sig
    type code
    eqtype reg
    val regClosure:  reg
    val regStackPtr: reg
    datatype argType = ArgGeneral | ArgFP
    val argRegs: argType list -> reg option list
    val resultReg: argType -> reg

    structure RegSet:
    sig
        eqtype regSet
        val singleton: reg -> regSet
        val allRegisters: regSet
        val generalRegisters: regSet
        val floatingPtRegisters: regSet
        val noRegisters: regSet
        val regSetUnion: regSet * regSet -> regSet
        val inSet: reg * regSet -> bool
        val listToSet: reg list -> regSet
    end

    datatype regHint = UseReg of RegSet.regSet | NoHint | NoResult
    type operation
    type operations = operation list
    type forwardLabel
    type backwardLabel

    val activeRegister: reg -> operations
    val returnFromFunction: int -> operations
    val resetStack: int -> operations
    val raiseException: operations
    val pushRegisterToStack: reg -> operations
    val pushCurrentHandler: operations
    val storeToHandler: reg -> operations
    val allocStore: { size: int, flags: Word8.word, output: reg } -> operations
    val allocationComplete: operations
    val backJumpLabel: unit -> operations * backwardLabel
    val forwardJumpLabel: forwardLabel -> operations
    val indexedCase:
            { testReg: reg, workReg: reg, minCase: word, maxCase: word,
              isArbitrary: bool, isExhaustive: bool } -> operations * forwardLabel list * forwardLabel
    
    datatype callKinds =
        Recursive
    |   ConstantClosure of Address.machineWord
    |   ConstantCode of Address.machineWord
    |   CodeFun of code
    |   FullCall
    
    val callFunction: callKinds -> operations
    val jumpToFunction: callKinds  -> operations

    val codeCreate: bool * string * Address.machineWord * Universal.universal list -> code  (* makes the initial segment. *)
    val copyCode: code * operations * int * RegSet.regSet * bool -> Address.address

    (*val codeAddress: code -> address option*)
    val addCompletionHook: code * (code * Address.machineWord -> unit) -> unit

    type negotiateTests
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

    type negotiation
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

    val checkAndReduce: 'a instrs * 'a list * ('a -> Address.machineWord option) -> (negotiation * 'a list) option

    val checkAndReduceBranches: 'a tests * 'a list * ('a -> Address.machineWord option) -> (negotiateTests * 'a list) option

    type regSet = RegSet.regSet
    type machineWord = Address.machineWord
    type ttab
    type loopPush
    type addrs
    type savedState

    val ttabCreate: int * Universal.universal list -> ttab

    (* Register allocation *)
    val getRegister:    ttab * reg -> operation list
    val getRegisterInSet: ttab * regSet -> reg * operation list
    val freeRegister:   ttab * reg -> operation list
    val addRegUse:      ttab * reg -> operation list
    val removeRegistersFromCache: ttab * regSet -> operation list

    (* Stack handling *)
    eqtype stackIndex

    val noIndex: stackIndex

    (* Push entries *)
    val pushReg:      ttab * reg  -> stackIndex;
    val pushStack:    ttab * int  -> stackIndex
    val pushConst:    ttab * machineWord -> stackIndex
    val pushCodeRef:  ttab * code -> stackIndex;
    val pushAllBut:   ttab * ((stackIndex -> unit) -> unit) * regSet -> operation list
    val pushNonArguments: ttab * stackIndex list * regSet -> reg list * operation list
    val pushSpecificEntry: ttab * stackIndex -> operation list
    val incsp:        ttab -> stackIndex;
    val decsp:        ttab*int -> unit;
    val reserveStackSpace: ttab * int -> stackIndex * operation list

    (* Code entries *)
    val loadEntryToSet:    ttab * stackIndex * regSet * bool -> reg * stackIndex * operation list
    val loadToSpecificReg: ttab * reg * stackIndex * bool -> stackIndex * operation list
    val lockRegister:      ttab * reg -> unit
    val unlockRegister:    ttab * reg -> operation list
    val loadIfArg:         ttab * stackIndex -> stackIndex * operation list
    val indirect:          int * stackIndex * ttab -> stackIndex * operation list
    val moveToVec:         stackIndex * stackIndex * int * ttab -> operation list
    val lockVector:        ttab * stackIndex -> operation list
    val ensureNoAllocation: ttab * stackIndex -> stackIndex * operation list

    val removeStackEntry: ttab*stackIndex -> operation list

    val resetButReload:   ttab * int -> operation list
    val pushValueToStack: ttab * stackIndex * int -> stackIndex * operation list
    val storeInStack:     ttab * stackIndex * int -> stackIndex * operation list
    val realstackptr:     ttab -> int
    val maxstack:         ttab -> int
    val parameterInRegister: reg * int * ttab -> stackIndex
    val incrUseCount:     ttab * stackIndex * int -> operation list

    val setLifetime:      ttab * stackIndex * int -> unit

    type stackMark
    val markStack: ttab -> stackMark
    val unmarkStack: ttab * stackMark -> unit

    type labels;

    val noJump: labels
    val isEmptyLabel: labels -> bool

    datatype mergeResult = NoMerge | MergeIndex of stackIndex;

    val unconditionalBranch: mergeResult * ttab -> labels * operation list
    val makeLabels: mergeResult * forwardLabel * savedState -> labels
    val jumpBack: backwardLabel * ttab -> operation list

    val fixup: labels * ttab -> operation list
    val merge: labels * ttab * mergeResult * stackMark -> mergeResult * operation list

    type handler;

    val pushAddress: ttab * int -> stackIndex * handler * operation list
    val fixupH:      handler * int * ttab -> operation list
    val reloadHandler: ttab * stackIndex -> operation list

    val exiting: ttab -> unit
    val haveExited: ttab -> bool

    val dataOp: stackIndex list * negotiation * ttab * regHint -> stackIndex * operation list

    val compareAndBranch: stackIndex list * negotiateTests * ttab -> labels * operation list

    val saveState : ttab -> savedState
    val compareLoopStates: ttab * savedState * stackIndex list -> regSet * loopPush list
    val restoreLoopState: ttab * savedState * regSet * loopPush list -> operation list

    (* Temporary checking that the stack has been emptied. *)
    val checkBlockResult: ttab * mergeResult -> unit

    val chooseRegister : ttab -> reg option

    val getRegisterSetForFunction: machineWord -> regSet
    val addModifiedRegSet: ttab * regSet -> unit

    val getModifedRegSet: ttab -> regSet

    datatype argdest = ArgToRegister of reg | ArgToStack of int | ArgDiscard
    val getLoopDestinations: stackIndex list * ttab -> argdest list * operation list

    val callCode: stackIndex * bool * ttab -> operation list
    val jumpToCode: stackIndex * bool * ttab -> operation list

    (* Get constants or code refs, which are really constants. *)
    datatype constEntry = ConstLit of machineWord | ConstCode of code | NotConst

    val isConstant: stackIndex * ttab -> constEntry
    val isRegister: stackIndex * ttab -> reg option
    val isContainer: stackIndex * ttab -> bool

    val createStackClosure: ttab * stackIndex list -> stackIndex * operation list
    val setRecursiveClosureEntry: stackIndex * stackIndex * int * ttab -> operation list

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
        and  regSet         = RegSet.regSet
        and  backwardLabel  = backwardLabel
        and  forwardLabel  = forwardLabel
    end
end;
