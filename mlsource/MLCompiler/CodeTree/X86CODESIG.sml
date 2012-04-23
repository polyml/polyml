(*
    Copyright David C. J. Matthews 2010, 2012

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

signature X86CODESIG =
sig
    type machineWord = Address.machineWord
    type short = Address.short
    type address = Address.address

    type code

    val sameCode: code * code -> bool

    datatype reg =
        GenReg of Word8.word * bool
    |   FPReg of Word8.word

    val isX64: bool

    val eax: reg and ebx: reg and ecx: reg and edx: reg
    and edi: reg and esi: reg and esp: reg and ebp: reg
    and fp0: reg and fp1: reg and fp2: reg and fp3: reg
    and fp4: reg and fp5: reg and fp6: reg and fp7: reg
    and r8:  reg and r9:  reg and r10: reg and r11: reg
    and r12: reg and r13: reg and r14: reg and r15: reg
    (* For vector indexing we provide a numbering for the registers. *)
    val regs:   int
    val regN:   int -> reg
    val nReg:   reg -> int

    val regRepr: reg -> string
    
    type addrs and labList
    val addrZero: addrs

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

    datatype arithOp = ADD | OR (*|ADC | SBB*) | AND | SUB | XOR | CMP
    and      shiftType = SLL | SRL | SRA
    and      repOps = CMPSB | MOVSB | MOVSL | STOSB | STOSL
    and      fpOps = FADD | FMUL | FCOM | FCOMP | FSUB | FSUBR | FDIV | FDIVR
    and      fpUnaryOps = FABS | FCHS | FSQRT | FSIN | FCOS | FPATAN | FLD1 | FLDZ
    and      group3Ops = NOT | NEG | MUL | IMUL | DIV | IDIV

    datatype branchOps = JO | JNO | JE | JNE | JL | JGE | JLE | JG | JB | JNB | JNA | JA

    datatype callKinds =
        Recursive
    |   ConstantFun of machineWord * bool
    |   CodeFun of code
    |   FullCall

    datatype label =
        Labels of
        {
            forward: labList ref,
            reverse: addrs ref,
            labId: int ref,
            uses: int ref,
            chain: label option ref
        }

    val mkLabel: unit -> label

    datatype indexType =
        NoIndex | Index1 of reg | Index2 of reg | Index4 of reg | Index8 of reg

    datatype memoryAddress =
        BaseOffset of { base: reg, offset: int, index: indexType }
    |   ConstantAddress of machineWord

    datatype branchPrediction = PredictNeutral | PredictTaken | PredictNotTaken

    datatype operation =
        MoveRR of { source: reg, output: reg }
    |   MoveConstR of { source: int, output: reg }
    |   MoveLongConstR of { source: machineWord, output: reg }
    |   LoadMemR of { source: memoryAddress, output: reg }
    |   LoadByteR of { source: memoryAddress, output: reg }
    |   PushR of reg
    |   PushConst of int
    |   PushLongConst of machineWord
    |   PushMem of { base: reg, offset: int }
    |   PopR of reg
    |   ArithRR of { opc: arithOp, output: reg, source: reg }
    |   ArithRConst of { opc: arithOp, output: reg, source: int }
    |   ArithRLongConst of { opc: arithOp, output: reg, source: machineWord }
    |   ArithRMem of { opc: arithOp, output: reg, offset: int, base: reg }
    |   ArithMemConst of { opc: arithOp, offset: int, base: reg, source: int }
    |   ArithMemLongConst of { opc: arithOp, offset: int, base: reg, source: machineWord }
    |   ShiftConstant of { shiftType: shiftType, output: reg, shift: Word8.word }
    |   ShiftVariable of { shiftType: shiftType, output: reg } (* Shift amount is in ecx *)
    |   ConditionalBranch of { test: branchOps, label: label, predict: branchPrediction }
    |   LockMutableSegment of reg
    |   LoadAddress of { output: reg, offset: int, base: reg option, index: indexType }
    |   LoadCodeRef of { output: reg, code: code }
    |   TestTagR of reg
    |   TestByteMem of { base: reg, offset: int, bits: word }
    |   CallRTS of int
    |   StoreRegToMemory of { toStore: reg, address: memoryAddress }
    |   StoreConstToMemory of { toStore: int, address: memoryAddress }
    |   StoreLongConstToMemory of { toStore: machineWord, address: memoryAddress }
    |   StoreByteRegToMemory of { toStore: reg, address: memoryAddress }
    |   StoreByteConstToMemory of { toStore: Word8.word, address: memoryAddress }
    |   AllocStore of { size: int, output: reg }
    |   AllocStoreVariable of reg
    |   StoreInitialised
    |   CallFunction of { callKind: callKinds }
    |   JumpToFunction of { callKind: callKinds, returnReg: reg option }
    |   ReturnFromFunction of { returnReg: reg option, argsToRemove: int }
    |   RaiseException
    |   UncondBranch of label
    |   ResetStack of int
    |   InterruptCheck
    |   JumpLabel of label
    |   TagValue of { source: reg, output: reg }
        (* Some of these operations are higher-level and should be reduced. *)
    |   LoadHandlerAddress of { handlerLab: addrs ref, output: reg }
    |   StartHandler of { handlerLab: addrs ref }
    |   IndexedCase of { testReg: reg, workReg: reg, min: word, cases: label list }
    |   FreeRegisters of RegSet.regSet
    |   MakeSafe of reg
    |   RepeatOperation of repOps
    |   Group3Ops of reg * group3Ops
    |   AtomicXAdd of {base: reg, output: reg}
    |   FPLoadFromGenReg of reg
    |   FPLoadFromFPReg of { source: reg, lastRef: bool }
    |   FPLoadFromConst of real
    |   FPStoreToFPReg of { output: reg, andPop: bool }
    |   FPStoreToMemory of { base: reg, offset: int, andPop: bool }
    |   FPArithR of { opc: fpOps, source: reg }
    |   FPArithConst of { opc: fpOps, source: machineWord }
    |   FPArithMemory of { opc: fpOps, base: reg, offset: int }
    |   FPUnary of fpUnaryOps
    |   FPStatusToEAX
    |   FPLoadIntAndPop
    |   FPFree of reg

    type operations = operation list
    val printOperation: operation * (string -> unit) -> unit

    val codeCreate: bool * string * machineWord * Universal.universal list -> code  (* makes the initial segment. *)
    (* Code generate operations and construct the final code. *)
    val createCodeSegment: operations * RegSet.regSet * code -> address

    val addCompletionHook: code * (code * machineWord -> unit) -> unit

    val codeAddress: code -> address option
    val procName:   code -> string      (* Name of the procedure. *)

    val memRegHandlerRegister: int
    and memRegRaiseDiv: int
    and memRegArbEmulation: int
    and memRegThreadSelf: int
    and memRegStackLimit: int
    and memRegStackOverflowCall: int
    and memRegStackOverflowCallEx: int

    (* Debugging controls and streams for optimiser. *)
    val lowLevelOptimise: code -> bool
    val printLowLevelCode: operation list * code -> unit

    structure Sharing:
    sig
        type code           = code
        and  reg            = reg
        and  addrs          = addrs
        and  operation      = operation
        and  regSet         = RegSet.regSet
        and  label          = label
        and  labList        = labList
    end
end;
