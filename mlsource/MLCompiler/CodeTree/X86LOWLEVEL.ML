(*
    Copyright David C. J. Matthews 2010

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

(* Produce the low-level code for the X86. *)

functor X86LOWLEVEL (

structure X86CODE: X86CODESIG

structure X86OPTIMISE:
sig
    type operation
    type code
    type operations = operation list

    val optimise: code * operations -> operations

    structure Sharing:
    sig
        type operation = operation
        type code = code
    end
end

sharing X86CODE.Sharing = X86OPTIMISE.Sharing

) : CODECONSSIG =

struct
    open Address
    open Misc
    
    open X86CODE
    open RegSet

    infix 5 << <<+ <<- >> >>+ >>- ~>> ~>>+ ~>>- (* Shift operators *)
    infix 3 andb orb xorb andbL orbL xorbL andb8 orb8 xorb8
    
    val op orb = Word.orb
    val op andb8 = Word8.andb

    val wordToWord8 = Word8.fromLargeWord o Word.toLargeWord
    and word8ToWord = Word.fromLargeWord o Word8.toLargeWord

    val exp2_30 =  0x40000000

    (* This actually checks that the value will fit in 31 bits because we normally have to
       tag it later. *)
    fun is31bitSigned a =
        isShort a andalso let val aI = Word.toIntX(toShort a) in ~exp2_30 <= aI andalso aI < exp2_30 end

    (* tag a short constant *)
    fun tag c = 2 * c + 1;
  
    (* shift a short constant, but don't set tag bit *)
    fun semitag c = 2 * c;

     (* Not real registers. *)
    val regNone    = NONE;  

    val regClosure  = edx; (* Addr. of closure for fn. call goes here. *)
    
    val regStackPtr = esp;
    val regReturn   = NONE; (* Return address is on the stack. *)
 
    datatype argType = ArgGeneral | ArgFP

    (* The first two general arguments are passed in eax and ebx (X86/32) or the
       first five arguments (X86/64), with the first three floating point
       args in fp0, fp1, fp2.  The cost of pushing floating point values to the stack is high
       so it's almost certainly better to use registers if possible. *)
    fun argRegs l =
    let
        fun allocReg ([], _, _) = []
        |   allocReg (ArgGeneral :: l, genReg :: genRegs, fpRegs) =
                SOME genReg :: allocReg(l, genRegs, fpRegs)
        |   allocReg (ArgFP :: l, genRegs, fpReg :: fpRegs) =
                SOME fpReg :: allocReg(l, genRegs, fpRegs)
        |   allocReg (_ :: l, genRegs, fpRegs) = NONE :: allocReg(l, genRegs, fpRegs)
    in
        allocReg(l, if isX64 then [eax, ebx, r8, r9, r10] else [eax, ebx], [fp0, fp1, fp2])
    end

    fun resultReg ArgGeneral = eax (* Result is in eax *)
    |   resultReg ArgFP = fp0 (* Result is in fp0 *)


    infix 7 ** infix 6 ++ -- infix 4 ins
    val op ** = regSetIntersect and op ++ = regSetUnion and op -- = regSetMinus and op ins = inSet

    fun allocStore{size, flags, output} =
    if isX64 andalso flags <> 0w0
    then
        [StoreByteConstToMemory{toStore=flags, address=BaseOffset{offset= ~1, base=output, index=NoIndex}},
         StoreConstToMemory{toStore=size, address=BaseOffset{offset= ~wordSize, base=output, index=NoIndex}},
         AllocStore{size=size, output=output}]
    else
    let
        val lengthWord = IntInf.orb(size, IntInf.<<(Word8.toInt flags, 0w24))
    in
        [StoreConstToMemory{toStore=lengthWord, address=BaseOffset{offset= ~wordSize, base=output, index=NoIndex}},
         AllocStore{size=size, output=output}]
    end

    (* Code to load a floating point constant. *)
    fun loadFPConstant source =
    (* Real constants are the addresses of 64-bit quantities on the heap. *)
    let
        val realConst: real =
            if isShort source
            then (* This can occur when the higher level puts a dummy zero value
                    on after raising an exception. *)
            if toShort source = 0w0 then 0.0 else raise InternalError "moveConstantToRegister: short real value"
            else if getFlags(toAddress source) <> F_bytes
                    orelse length(toAddress source) <> 0w8 div Word.fromInt wordSize
            then raise InternalError "moveConstantToRegister to fp reg: invalid source"
            else unsafeCast source
    in
        FPLoadFromConst realConst
    end

    datatype implement = ImplementGeneral | ImplementLiteral of machineWord

(*    local
        (* Multiplies and divides by powers of two can be implemented as shifts. *)
        fun getPower2 n =
        let
            fun p2(i, log) = if i < n then p2 (i*0w2, log+0w1) else if i = n then SOME log else NONE
        in
            p2(0w1, 0w0)
        end
    in

        |   checkAndReduce(InstrMulA, args as [arg1, arg2], mapper) =
            (
                (* The only special case we recognise is multiplication by 2. *)
                case List.map mapper args of
                    [_, SOME lit] =>
                        if isShort lit  andalso toShort lit = 0w2
                        then SOME(InstrMulAConst2, [arg1])
                        else SOME(InstrMulA, args)
                |   [SOME lit, _] =>
                        if isShort lit  andalso toShort lit = 0w2
                        then SOME(InstrMulAConst2, [arg2])
                        else SOME(InstrMulA, args)
                |   _ => SOME(InstrMulA, args)
            )
    end*)

    (* Argument negotiation.  The idea is to get the arguments into the "best" locations
       for the instruction. *)

    datatype regHint = UseReg of regSet | NoHint | NoResult

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
            { argNo: int, regSet: regSet, willOverwrite: bool, next: nextAction }
    |   ActionGetWorkReg of (* Get a work/result register. *)
            { regSet: regSet, setReg: reg -> nextAction }

    withtype nextAction = actionSource list -> argAction

    type negotiation = regHint -> nextAction
    and negotiateTests = unit -> nextAction * label
    type 'a instrs = 'a list * ('a -> machineWord option) -> (negotiation * 'a list) option
    and 'a tests = 'a list * ('a -> machineWord option) -> (negotiateTests * 'a list) option

    type  forwardLabel = label
    and  backwardLabel = label

    fun negotiateArguments(perform, pref) = perform pref
    and negotiateTestArguments perform = perform()

    (* See if the instruction is implemented but otherwise do nothing else.  Nearly all
       the operations are implemented on this architecture. *)
    fun checkAndReduce(instr, args, f) = instr(args, f)
    and checkAndReduceBranches(tests, args, f) = tests(args, f)

    (* Exported versions of operations datatype *)
    fun callFunction v = [CallFunction v]
    and jumpToFunction v = [JumpToFunction v]
    and returnFromFunction v = [ReturnFromFunction v]

    and indexedCase { testReg, workReg, minCase, maxCase, isArbitrary, isExhaustive } =
    let
        val defaultLab as Labels{uses=defUses, ...} = mkLabel()
        fun makeLab _ =
        let
            val lab as Labels{uses, ...} = mkLabel()
        in
            uses := 1;
            lab
        end
        val indexLabels =
            List.tabulate(Word.toInt(maxCase-minCase+0w1), makeLab)
        val testCode =
            if isExhaustive
            then []
            else
            let
                (* If this is an arbitrary precision int we need to check it's short
                   and go to the default if it isn't. *)
                val testTag =
                    if isArbitrary
                    then
                    (
                        defUses := 3;
                        [ConditionalBranch{test=JE, predict=PredictNotTaken, label=defaultLab},
                         TestTagR testReg]
                    )
                    else (defUses := 2; [])
                (* Range checks. *)
                (* TODO: In 64-bit mode we could have constants that won't fit in
                   32-bit words. *)
                val range1 =
                    [ConditionalBranch{test=JB, predict=PredictNotTaken, label=defaultLab},
                     ArithRConst{ opc=CMP, output=testReg, source=tag(Word.toInt minCase) }]
                val range2 =
                    [ConditionalBranch{test=JA, predict=PredictNotTaken, label=defaultLab},
                     ArithRConst{ opc=CMP, output=testReg, source=tag(Word.toInt maxCase) }]
            in
                range2 @ range1 @ testTag
            end
        val code = [IndexedCase{testReg=testReg, workReg=workReg, min=minCase, cases=indexLabels}] @ testCode
    in
        (code, indexLabels, defaultLab)
    end

    and forwardJumpLabel v = [JumpLabel v]

    and jumpBack (v as Labels{uses, ...}) = (uses := !uses+1; [UncondBranch v])

    (* We're only interested in when floating point registers are freed in order to
       optimise the code.  For the moment at least we don't use FFree here because
       that actually generates code and the higher levels call freeRegister
       after branches which puts them in the wrong position. *)
    and activeRegister _ = []
    and freeRegister r =
        if r ins floatingPtRegisters then [FreeRegisters(singleton r)] else []

    and moveRegisterToRegister(source as GenReg _, output as GenReg _) =
        [MoveRR{source=source, output=output}]

    |   moveRegisterToRegister(source as GenReg _, output as FPReg _) =
        [FPStoreToFPReg{output=output, andPop=true}, FPLoadFromGenReg source]

    |   moveRegisterToRegister(source as FPReg _, output as FPReg _) =
        [FPStoreToFPReg{output=output, andPop=true}, FPLoadFromFPReg{source=source, lastRef=false}]

    |   moveRegisterToRegister(source as FPReg _, output as GenReg _) =
        (* We need to allocate memory to contain the value. *)
        [FPStoreToMemory{ base=output, offset=0, andPop=true },
         FPLoadFromFPReg{source=source, lastRef=false}]
            @ allocStore{size=8 div wordSize, flags=F_bytes, output=output}

    and moveMemoryToRegister(base, offset, output) =
        (* Destination can only be a general register not a FP reg. *)
        [LoadMemR{source=BaseOffset{base=base, offset=offset, index=NoIndex}, output=output}]

    and moveConstantToRegister(lit, output as GenReg _) =
        if isShort lit
        then [MoveConstR{source=tag(Word.toIntX(toShort lit)), output=output}]
        else [MoveLongConstR{source=lit, output=output}]

    |   moveConstantToRegister(source, output as FPReg _) =
        (* Real constants are the addresses of 64-bit quantities on the heap. *)
            [FPStoreToFPReg{output=output, andPop=true}, loadFPConstant source]

    and moveCodeRefToRegister(code, output) = (* The address of another function *)
        [LoadCodeRef{code=code, output=output}]

    (* Set a register to an address in the stack.  Just a reg-reg move if
       offset is zero otherwise a load-address. *)
    and moveStackAddress(0, output) =
            [MoveRR{source=esp, output=output}]
    |   moveStackAddress(stackAddr, output) =
            [LoadAddress{base=SOME esp, offset=stackAddr*wordSize, index=NoIndex, output=output}]

    and storeRegisterToStack(reg, loc) =
        if loc < 0 then raise InternalError "storeRegisterToStack: Negative stack offset"
        else [StoreRegToMemory{toStore=reg, address=BaseOffset{offset=loc, base=esp, index=NoIndex}}]

    and storeConstantToStack(lit, loc) =
    let
        val addr = BaseOffset{offset=loc, base=regStackPtr, index=NoIndex}
    in
        if loc < 0 then raise InternalError "storeRegisterToStack: Negative stack offset"
        else if isShort lit
        then [StoreConstToMemory{toStore=tag(Word.toIntX(toShort lit)), address=addr}]
        else [StoreLongConstToMemory{toStore=lit, address=addr}]
    end
 
    fun pushRegisterToStack v = [PushR v]
    and pushConstantToStack lit =
            if isShort lit then [PushConst(tag(Word.toIntX(toShort lit)))] else [PushLongConst lit]
    and pushMemoryToStack(reg, offset) = [PushMem{base=reg, offset=offset}]

    fun resetStack 0 = []
    |   resetStack n =
        if n < 0 then raise InternalError "resetStack: negative" else [ResetStack n]

    val raiseException = [RaiseException]
    val interruptCheck = [InterruptCheck]
    
    val pushToReserveSpace = pushConstantToStack(toMachineWord 0)

    fun uncondBranch() =
    let
        val label as Labels{uses, ...} = mkLabel()
    in
        uses := 1;
        ([UncondBranch label], label)
    end

    fun condBranch(test, predict) =
    let
        val label as Labels{uses, ...} = mkLabel()
    in
        uses := 1;
        ([ConditionalBranch{test=test, predict=predict, label=label}], label)
    end

    fun backJumpLabel() =
    let
        val loopLabel = mkLabel()
    in
        ([JumpLabel loopLabel], loopLabel)
    end

    fun loadCurrentHandler output =
        [LoadMemR{ source=BaseOffset{base=ebp, offset=memRegHandlerRegister, index=NoIndex}, output=output }]
    and storeToHandler reg =
        [StoreRegToMemory{
            toStore=reg, address=BaseOffset{offset=memRegHandlerRegister, base=ebp, index=NoIndex}}]
    
    val pushCurrentHandler = [PushMem{base=ebp, offset=memRegHandlerRegister}]

    fun loadHandlerAddress v = [LoadHandlerAddress v]

    (* Start of handler.  Set the label, reload esp from the handler register, remove the handler
       address and restore old handler. *)
    and startHandler v =
        storeToHandler ebx @
        [ ResetStack 2, LoadMemR{ source=BaseOffset{base=esp, offset=wordSize, index=NoIndex}, output=ebx }]
        @ loadCurrentHandler esp @ [StartHandler v]

    (* Default action for all operations. *)
    local
        datatype source =
            InRegister of reg
        |   LiteralSource of machineWord
        |   BaseOffsetSource of { base: reg, offset: int }

        fun getArg(instr, regSet, outputReg, operands) args =
        let
            (* Take each NONE in the operand list and replace it with the appropriate operand. *)
            fun nextArg([], [], _) =
                (* All done *)
                ActionDone{
                    outReg=outputReg,
                    operation=instr(List.map valOf operands, outputReg)
                    }
            |   nextArg(NONE :: otherOps, arg :: _, argNo) =
                (
                    case arg of (* Look at the next argument *)
                        (ActInRegisterSet{ readable, ...}) =>
                        if regSetIntersect(readable, regSet) <> noRegisters
                        then
                        let
                            val aReg = oneOf(regSetIntersect(readable, regSet))
                        in
                            (* It's in a register.  Lock it, record it and go on to the next arg. *)
                            ActionLockRegister{argNo=argNo, reg=aReg, willOverwrite=false,
                                next=getArg(instr, regSet, outputReg,
                                        List.take(operands, argNo) @ (SOME(InRegister aReg) :: otherOps))}
                        end
                        else ActionLoadArg{argNo=argNo, regSet=regSet, willOverwrite=false,
                                (* Load into a register - won't modify it afterwards.  Next action is
                                   to look at this again after it's been loaded. *)
                                next=getArg(instr, regSet, outputReg, operands)}
                    |   _ => (* If the value is not in a register we need to load it first to a
                                general register even if the eventual destination is a floating pt reg. *)
                            ActionLoadArg{argNo=argNo, regSet=generalRegisters, willOverwrite=false,
                                (* Load into a register - won't modify it afterwards.  Next action is
                                   to look at this again after it's been loaded. *)
                                next=getArg(instr, regSet, outputReg, operands)}
                )
            |   nextArg(SOME _ :: otherOps, _ :: otherArgs, argNo) =
                    nextArg(otherOps, otherArgs, argNo+1)
            |   nextArg _ = raise Empty
        in
            nextArg(operands, args, 0)
        end

        (* Replace an entry in the operand list, which should be NONE, with SOME operand. *)
        fun replaceOperand(n, repWith) (operands, results) =
            case List.nth(operands, n) of
                NONE =>
                    (List.take(operands, n) @ SOME repWith :: List.drop(operands, n+1), results)
            |   SOME _ => raise InternalError "replaceOperand: Operand already present"

        fun generateInstruction genInstr (operands, results) _ =
            ActionDone{ outReg=results, operation=genInstr(List.map valOf operands, results) }

        (* If we have specified a preferred register set use one of those unless they're already in
           use. *)
        fun prefAsSet(UseReg regs, regSet, operands) =
            let
                fun inUse(SOME(InRegister r), s) = s ++ singleton r
                |   inUse(_, s) = s
                val available = (regs ** regSet) -- List.foldl inUse noRegisters operands
            in
                if available <> noRegisters
                then singleton (oneOf available) else regSet
            end
        |   prefAsSet(_, regSet, _) = regSet

        (* Get a register for the results. In some cases, e.g. loadWord/byte we may
           already have a result register. *)
        fun getResultRegister(regSet, pref: regHint, whenDone) (operands, NONE) _ =
            ActionGetWorkReg{regSet=prefAsSet(pref, regSet, operands), (* Use the preferred destination. *)
                setReg = fn reg => whenDone (operands, SOME reg)}
        |   getResultRegister(_, _, whenDone) operandAndResults args = whenDone operandAndResults args

        fun getWorkingRegister(regSet, whenDone) opAndResults _ =
            ActionGetWorkReg{regSet=regSet, setReg = fn reg => whenDone reg opAndResults}

        (* Load the argument to one of a set of possible registers. *)
        fun loadToOneOf(regSet, overWrite, argNo, whenDone) opAndResults args =
            case List.nth(args, argNo) of
                (ActInRegisterSet{ modifiable, readable, ...}) =>
                    let
                        val chooseFrom = if overWrite then modifiable else readable
                    in
                        if regSetIntersect(regSet, chooseFrom) <> noRegisters
                        then
                        let
                            val aReg = oneOf(regSetIntersect(chooseFrom, regSet))
                        in
                            ActionLockRegister{argNo=argNo, reg=aReg, willOverwrite=overWrite,
                                                next=whenDone (replaceOperand(argNo, InRegister aReg) opAndResults)}
                        end
                        else (* Not in the right register - move it. *)
                                ActionLoadArg{argNo=argNo, regSet=regSet, willOverwrite=overWrite,
                                    next=loadToOneOf(regSet, overWrite, argNo, whenDone) opAndResults}
                    end
                |   _=>
                    let
                        (* We can't load directly into a floating point register so if the set
                           does not include any general registers we first need to load the
                           value into a general register and then once it's in a
                           register move it over.  Actually, we're not loading from the general
                           register to the floating point register: the "move" involves loading
                           the boxed value that contains the real number into the fp register. *)
                        val (loadSet, write) =
                            if regSetIntersect(regSet, generalRegisters) <> noRegisters
                            then (regSet, overWrite) else (generalRegisters, false)
                    in
                        ActionLoadArg{argNo=argNo, regSet=loadSet, willOverwrite=write,
                             next=loadToOneOf(regSet, overWrite, argNo, whenDone) opAndResults}
                    end

        (* Load to a register (read-only) or, if it's a literal leave it.  In
           64-bit mode we load anything that won't fit in 32-bits when tagged. *)
        fun loadToRegOrLiteral(regSet, argNo, whenDone) opAndResults args =
            case List.nth(args, argNo) of
                ActLiteralSource lit => 
                    if not isX64 orelse is31bitSigned lit
                    then whenDone (replaceOperand(argNo, LiteralSource lit) opAndResults) args
                    else loadToOneOf(regSet, false, argNo, whenDone) opAndResults args
            |   _ => loadToOneOf(regSet, false, argNo, whenDone) opAndResults args

        (* Load a base address for a load or store operation. We can use a constant source but
           only if the index is zero.  The base address is the first argument and the index is the
           second.  We can't use a constant base address in 64-bit mode because inline addresses
           aren't possible (the equivalent code means PC-relative). *)
        fun loadBaseAddress (pref, whenDone) (opAndResults as (_, resReg))
                    (args as (ActLiteralSource base :: ActLiteralSource index :: _)) =
            if isShort index andalso toShort index = 0w0 andalso not isX64
            then whenDone (replaceOperand(0, LiteralSource base) opAndResults) args
            else ActionLoadArg{argNo=0, regSet=generalRegisters,
                        willOverwrite=
                            case (pref, resReg) of (NoResult, _) => false | (_, SOME _) => false | _ => true,
                        next=loadBaseAddress(pref, whenDone) opAndResults}

        |   loadBaseAddress (pref, whenDone) (opAndResults as (ops, resReg))
                    (ActInRegisterSet{modifiable, readable} :: _) =
                if modifiable ** generalRegisters <> noRegisters
                then
                let
                    val reg = oneOf(prefAsSet(pref, modifiable ** generalRegisters, ops))
                    (* We can use this for the result if we want one and don't already have one. *)
                    val (outReg, modify) =
                        case (resReg, pref) of
                            (_, NoResult) => (resReg, false)
                        |   (SOME _, _) => (resReg, false)
                        |   _ => (SOME reg, true)
                in
                    ActionLockRegister { argNo=0, reg=reg, willOverwrite=modify,
                        next=whenDone (replaceOperand(0, InRegister reg) (ops, outReg)) }
                end
                else if readable ** generalRegisters <> noRegisters (* The base register isn't modifiable. *)
                then
                let
                    val reg = oneOf(prefAsSet(pref, readable ** generalRegisters, ops))
                in
                    ActionLockRegister { argNo=0, reg=reg, willOverwrite=false,
                        next=whenDone (replaceOperand(0, InRegister reg) opAndResults) }
                end
                else ActionLoadArg { argNo=0, regSet=generalRegisters, willOverwrite=true,
                                next=loadBaseAddress (pref, whenDone) opAndResults }

        |   loadBaseAddress (pref, whenDone) (opAndResults as (_, resReg)) _ =
                (* It's not a literal or in a register - load it. *)
                ActionLoadArg{argNo=0, regSet=generalRegisters,
                             willOverwrite=
                                case (pref, resReg) of (NoResult, _) => false | (_, SOME _) => false | _ => true,
                             next=loadBaseAddress(pref, whenDone) opAndResults}

        (* Process all the remaining arguments i.e. those with NONE in the argument list. *)
        fun allArgs (eachArg, whenDone) (operands as (opers, _)) =
        let
            (* Take each NONE in the operand list and replace it with the appropriate operand. *)
            fun nextArg([], _) = (* All done *) whenDone operands
            |   nextArg(NONE :: _, argNo) =
                    eachArg (argNo, fn operands => allArgs (eachArg, whenDone) operands) operands
            |   nextArg(SOME _ :: otherOps, argNo) = nextArg(otherOps, argNo+1)
        in
            nextArg(opers, 0)
        end

        (* This deals with any remaining operands and puts them into registers.  It is the fall-back
           in a lot of cases. *)
        fun allInRegisters (regSet, whenDone) =
            allArgs(fn (argNo, whenDone) => loadToOneOf(regSet, false, argNo, whenDone), whenDone)

        fun allInRegsOrLiterals (regSet, whenDone) =
            allArgs(fn (argNo, whenDone) => loadToRegOrLiteral(regSet, argNo, whenDone), whenDone)

        (* Some cases require arguments in specific registers. *)
        fun loadToSpecificReg(specReg, overWrite, argNo, whenDone) =
            loadToOneOf(singleton specReg, overWrite, argNo, whenDone)
            
    in
        (* Default if we don't need a result register. *)
        fun noresultNegotiator instr args =
            allInRegisters(generalRegisters, generateInstruction instr) (List.map(fn _ => NONE) args, NONE) args

        (* Default if we need a result which is different from the argument registers. *)
        fun generalNegotiator(instr, pref) args =
            getResultRegister(generalRegisters, pref,
                allInRegisters(generalRegisters, generateInstruction instr)
                ) (List.map(fn _ => NONE) args, NONE) args


        local (* Single argument case *)
            fun loadDestArg(instr, pref) [ActInRegisterSet{ modifiable, ...}] =
                    if regSetIntersect(modifiable, generalRegisters) <> noRegisters
                    then
                    let
                        val aReg = oneOf(regSetIntersect(modifiable, generalRegisters))
                    in
                        ActionLockRegister{argNo=0, reg=aReg, willOverwrite=true,
                                       next=getArg(instr, generalRegisters, SOME aReg, [SOME(InRegister aReg)])}
                    end
                    else ActionLoadArg{argNo=0, regSet=prefAsSet(pref, generalRegisters, []), willOverwrite=true,
                                  next=loadDestArg(instr, pref)}
            |   loadDestArg(instr, pref) [_] = (* Not in a register. *)
                    ActionLoadArg{argNo=0, regSet=prefAsSet(pref, generalRegisters, []), willOverwrite=true,
                                  next=loadDestArg(instr, pref)}
            |   loadDestArg _ _ = raise InternalError "loadDestArg: not a single argument"
        in
            val sharedSingleArgNegotiator = loadDestArg
        end
       
        local
            fun genStoreWord([base, index, value], NONE) =
            let
                val address =
                    case (base, index) of
                        (InRegister address, InRegister indexR) =>
                            if wordSize = 4
                            then BaseOffset{offset= ~2, base=address, index=Index2 indexR} (* Index is tagged so double *)
                            else (*8*) BaseOffset{offset= ~4, base=address, index=Index4 indexR}
                    |   (InRegister address, LiteralSource offset) =>
                            BaseOffset{offset=Word.toInt(toShort offset)*wordSize, base=address, index=NoIndex}
                    |   (LiteralSource address, LiteralSource index) =>
                            if isShort index andalso toShort index = 0w0 andalso not isX64
                            then ConstantAddress address
                            else raise InternalError "genStoreWord"
                    |   _ => raise InternalError "genStoreWord"
            in
                case value of
                    InRegister storeReg =>
                        [StoreRegToMemory{toStore=storeReg, address=address}]
                |   LiteralSource storeConst =>
                        if isShort storeConst
                        then [StoreConstToMemory{ toStore=tag(Word.toIntX(toShort storeConst)), address=address}]
                        else [StoreLongConstToMemory{ toStore=storeConst, address=address}]
                |   BaseOffsetSource _ => raise InternalError "genStoreWord"
            end
            | genStoreWord _ = raise InternalError "genStoreWord"

            fun storeWordNegotiator _ =
                (* The first argument, the address needs to be in a register even if
                   it's a constant.  The others may be literals.  Actually the first
                   argument could be a constant as well if the offset is zero. *)
                loadBaseAddress(NoResult,
                    loadToRegOrLiteral(generalRegisters, 1,
                        loadToRegOrLiteral(generalRegisters, 2,
                            generateInstruction genStoreWord
                            ))) ([NONE, NONE, NONE], NONE)
        in
            val instrStoreW: 'a instrs = fn(args, _) => SOME(storeWordNegotiator, args)
        end

        local
            (* Remove the mutable bit from the flag byte. *)
            fun genLockSeg([InRegister reg], NONE) = [LockMutableSegment reg]
            |   genLockSeg _ = raise InternalError "genLockSeg"
        in
            val instrLockSeg: 'a instrs = fn (args, _) => SOME(fn _ => noresultNegotiator genLockSeg, args)
        end

        local
            (* Load word or load byte.  Load word with a constant offset is very common since it's used
               for refs.  Load byte is more likely to be with an index and since we have to untag the
               index it's better to reuse it for the result if we can. *)
            fun loadIndex (pref, isWord, whenDone) (opAndResults as (ops, resReg))
                    (_ :: ActInRegisterSet{modifiable, readable} ::_) =
                if modifiable ** generalRegisters <> noRegisters
                then (* It's in a modifiable register. *)
                let
                    val reg = oneOf(prefAsSet(pref, modifiable ** generalRegisters, ops))
                    (* We can use this for the result if we don't already have one. *)
                    val outReg = case resReg of NONE => SOME reg | resReg => resReg
                in
                    ActionLockRegister { argNo=1, reg=reg, willOverwrite=true,
                        next=whenDone (replaceOperand(1, InRegister reg) (ops, outReg)) }
                end
                else if isWord andalso readable ** generalRegisters <> noRegisters
                    (* We don't have a writable index register. If this is a word operation we
                        can use a readable one but if it's a byte operation we need to use a
                        writable one. *)
                then
                let
                    val reg = oneOf(readable ** generalRegisters)
                in
                    ActionLockRegister { argNo=1, reg=reg, willOverwrite=false,
                        next=whenDone (replaceOperand(1, InRegister reg) opAndResults) }
                end
                else ActionLoadArg { argNo=1, regSet=generalRegisters, willOverwrite=true,
                                    next=loadIndex (pref, isWord, whenDone) opAndResults }

            |   loadIndex (_, _, whenDone) opAndResults (args as [_, ActLiteralSource offset]) =
                    (* Constant index - record it and go on to the base register *)
                    whenDone (replaceOperand(1, LiteralSource offset) opAndResults) args

            |   loadIndex (pref, isWord, whenDone) opAndResults _ =
                    (* Not in a register or a literal.  Load it. *)
                    ActionLoadArg { argNo=1, regSet=generalRegisters, willOverwrite=true,
                                    next=loadIndex (pref, isWord, whenDone) opAndResults }

            fun genLoadInstr isWord ([LiteralSource base, LiteralSource offset], SOME destReg) =
                    if isShort offset andalso toShort offset = 0w0 andalso not isX64
                    then [(if isWord then LoadMemR else LoadByteR)
                            {source=ConstantAddress base, output=destReg}]
                    else raise InternalError "genLoadWord: not zero"

            |   genLoadInstr true (*Word*) ([InRegister base, LiteralSource offset], SOME destReg) =
                    [LoadMemR{
                            source=BaseOffset{base=base, offset=Word.toInt(toShort offset)*wordSize, index=NoIndex},
                            output=destReg}]

            |   genLoadInstr false (*Byte*) ([InRegister base, LiteralSource offset], SOME destReg) =
                    (* Byte values need to be tagged.  The offset is a byte offset. *)
                    [TagValue{source=destReg, output=destReg},
                     LoadByteR{source=BaseOffset{base=base, offset=Word.toInt(toShort offset), index=NoIndex},
                               output=destReg}]

            |   genLoadInstr true (*Word*) ([InRegister base, InRegister indexR], SOME destReg) =
                let
                    val (offset, scale) =
                    (* The index is tagged: Multiply by the wordSize/2 and subtract the scaled tag. *)
                        if wordSize = 4
                        then (~2, Index2 indexR)
                        else (~4, Index4 indexR)
                in
                    [LoadMemR{source=BaseOffset{base=base, offset=offset, index=scale}, output=destReg}]
                end

            |   genLoadInstr false (*Byte*) ([InRegister base, InRegister indexR], SOME destReg) =
                    (* The index needs to be made safe unless it's actually the result. *)
                let
                    val safeIndex =
                        if indexR = destReg then [] else [MakeSafe indexR] (* Retag the index. *)
                in
                    safeIndex @
                        [TagValue{source=destReg, output=destReg},
                         LoadByteR{source=BaseOffset{base=base, offset=0, index=Index1 indexR}, output=destReg},
                         ShiftConstant{shiftType=SRL, output=indexR, shift=0w1} (* Untag. *)]
                end

            |   genLoadInstr _ _ = raise InternalError "genLoadInstr"

            fun genLoadWord isWord pref =
                loadIndex(pref, isWord,
                    loadBaseAddress(pref,
                        getResultRegister(generalRegisters, pref,
                            generateInstruction(genLoadInstr isWord))
                        )
                    ) ([NONE, NONE], NONE)
        in
            fun instrLoad(args, _) = SOME(genLoadWord true, args)
            and instrLoadB(args, _) = SOME(genLoadWord false, args)
        end

        local
            (* The allocStore operation takes three arguments: a number of words to allocate,
               the flags byte to go into the newly allocated store, and the initial value
               for each of the words of the memory.  We implement some common cases here
               and leave the rest to the allocator in the RTS. *)
            fun genFixedAlloc(length, flags) ([InRegister initReg], SOME output) =
                let
                    fun initialiser n =
                        StoreRegToMemory{
                            toStore=initReg, address=BaseOffset{offset=n * wordSize, base=output, index=NoIndex}}
                in
                    List.tabulate(length, initialiser) @
                    allocStore{size=length, flags=flags, output=output}
                end
            |   genFixedAlloc _ _ = raise InternalError "genFixedAlloc"

            (* Allocate memory whose length isn't known at compile time and initialise it. *)
            fun genVarAlloc flags ([InRegister _(*ecx*), InRegister _(*eax*)], SOME _ (* edi*)) =
                let
                    val initialiser =
                        if (flags andb8 F_bytes) <> 0w0
                        then (* Initialise it as bytes. *)
                            [MakeSafe eax, RepeatOperation STOSB,
                             (* Untag the initialiser in eax. *)
                             ShiftConstant{ shiftType=SRL, output=eax, shift=0w1},
                             (* Convert the length, which is in words, to bytes. *)
                             ShiftConstant{shiftType=SLL, output=ecx,
                                shift=if wordSize=4 then 0w2 else 0w3}]
                        else [RepeatOperation STOSL]
                in
                    [PopR edi] @ (* Pop the saved edi *)
                    initialiser @
                    [
                        (* Now initialise the memory. *)
                        (* Save edi before we start. *)
                        PushR edi,
                        (* Set the flags.  At least the mutable bit should be set. *)
                        StoreByteConstToMemory{
                            toStore=flags,
                            address=BaseOffset{offset= ~1, base=edi, index=NoIndex}},
                        (* Store it as the length field. *)
                        StoreRegToMemory{toStore=ecx,
                            address=BaseOffset{base=edi, offset= ~wordSize, index=NoIndex}},
                        (* It's now safe to untag ecx *)
                        ShiftConstant{ shiftType=SRL, output=ecx, shift=0w1},
                        (* Allocate the memory *)
                        AllocStoreVariable edi,
                        (* Compute the number of bytes into edi. The length in ecx is the number
                           of words as a tagged value so we need to multiply it, add wordSize to
                           include one word for the header then subtract the, multiplied, tag. *)
                        if wordSize = 4
                        then LoadAddress{output=edi, base=SOME ecx, offset=wordSize-2, index=Index1 ecx }
                        else LoadAddress{output=edi, base=NONE, offset=wordSize-4, index=Index4 ecx }
                    ]
                end
            |   genVarAlloc _ _ = raise InternalError "genVarAlloc"
        in
            (* Decide if we can implement it. *)
            fun instrAllocStore([lengthArg, flagArg, initValArg], mapper) =
            let
                (* This is used to allocate memory for refs, arrays, strings etc.  The first
                   arg is the length, the second the flags byte and the third the initial
                   value used to initialise each word, for word segments, or byte for
                   byte segs. *)
                val len = (* We only use the fixed length version for small segments of words
                             (possibly with the noOverwrite/weak flag as well). *)
                    case (mapper lengthArg, mapper flagArg) of
                        (SOME constLength, SOME constFlags) =>
                            if toShort constLength < 0w5 andalso
                                   (wordToWord8(toShort constFlags) andb8 0w3) = F_words
                            then SOME(toShort constLength)
                            else NONE
                    |   _ => NONE
            in
                case mapper flagArg of
                    NONE => NONE (* Non-constant flags: leave it to the RTS. *)

                |   SOME constFlags =>
                    let
                        val flags = wordToWord8(toShort constFlags)
                        fun allocStoreFixedLength(length, flags) prefs =
                                (* Get a result reg and put the initial value in a reg. *)
                            generalNegotiator(genFixedAlloc(length, flags), prefs)
                        fun allocStoreVarLength flags pref =
                            (* When allocating vectors/arrays we have to initialise the store and the easiest way to
                                do that is to put the length in ecx, the initialiser in eax and get the result in edi. *)
                            loadToSpecificReg(ecx, true, 0,
                                (* We don't actually need this to be modifiable if we're initialising with words. *)
                                loadToSpecificReg(eax, true, 1,
                                    getResultRegister(singleton edi, pref, generateInstruction(genVarAlloc flags))))
                                        ([NONE, NONE], NONE)
                    in
                        case len of
                            SOME len => SOME(allocStoreFixedLength(Word.toInt len, flags), [initValArg])
                        |   NONE => SOME(allocStoreVarLength flags, [lengthArg, initValArg])
                    end
            end
            |   instrAllocStore _ = raise InternalError "instrAllocStore: Wrong number of args"
        end

        local
            fun makeTest(InRegister reg) = TestTagR reg
            |   makeTest(BaseOffsetSource{base, offset}) = TestByteMem{base=base, offset=offset,bits=0w1}
            |   makeTest _ = raise InternalError "makeTest"

            (* Test tags for the arguments and call the emulator if either is long. Unless the
               operation is just a comparison check for overflow and jump to the emulator
               if the operation overflowed.  Rather than try to follow the static pattern
               we just generate the shortest code. *)

           fun testTags checkOverflow (operations, tag1, tag2) =
                let
                    fun checkTag(LiteralSource l) =
                        if isShort l then ([], [])
                        else (* Long constant - shouldn't be code-generating. *)
                             raise InternalError "testTags: long constant"
                    |   checkTag tag =
                        let
                            val (code, lab) = condBranch(JE, PredictNotTaken)
                        in
                            (code @ [makeTest tag], forwardJumpLabel lab)
                        end
                    val (tag1Check, tag1Lab) = checkTag tag1
                    and (tag2Check, tag2Lab) = checkTag tag2
                                
                    val (overflowCheck, overflowLab) =
                        if checkOverflow
                        then
                        let
                            val (code2, lab2) = condBranch(JO, PredictNotTaken)
                        in
                            (code2, (* Emulate if overflow. *) forwardJumpLabel lab2)
                        end
                        else ([], [])
                    val (skipEmulatorCode, skipEmulatorLabel) = uncondBranch()
                    val (backToInstrCode, backToInstrLabel) = backJumpLabel()
                in
                    forwardJumpLabel skipEmulatorLabel @ jumpBack backToInstrLabel @ [CallRTS memRegArbEmulation] @
                    overflowLab @ tag2Lab @ tag1Lab @ skipEmulatorCode @ (* Skip the emulator call. *)
                    overflowCheck @ operations (* The actual instruction *) @
                    backToInstrCode @ tag2Check @ tag1Check
                end
        in
            val testTagsForArithmetic = testTags true

            fun reverseTestOp JE = JE
            |   reverseTestOp JNE = JNE
            |   reverseTestOp JA = JB
            |   reverseTestOp JB = JA
            |   reverseTestOp JNA = JNB
            |   reverseTestOp JNB = JNA
            |   reverseTestOp JL = JG
            |   reverseTestOp JG = JL
            |   reverseTestOp JLE = JGE
            |   reverseTestOp JGE = JLE
            |   reverseTestOp _ = raise InternalError "reverseTestOp: unknown branch"

            (* If we're comparing a value with a short constant we don't need to
               emulate the comparison even if the value is actually long.  We just
               need to look at the sign to decide if it's less or greater because
               every positive long form value is greater than any short value and
               every negative long form value is less than any short value. *)
            fun testTagsForComparison (shortTestAndJump, tag1, LiteralSource tag2Const, opc, destLab) =
                let
                    (* The constant should always be short. *)
                    val _ = isShort tag2Const orelse raise InternalError "testTagsForComparison: long"
                    val (jumpOnLongCode, longLab) = condBranch(JE, PredictNotTaken)
                    val (skipLongCode, skipLongLabel) = uncondBranch()
                    val tag1Reg =
                        case tag1 of
                            InRegister reg => reg
                        |   _ => raise InternalError "testTagsForComparison: not reg"
                    (* If we're jumping on greater (equal) we jump if the value is
                       positive (sign bit clear).  If less (equal) we jump if it's
                       negative.  We need to increment the reference count for the
                       destination label. *)
                    val Labels{uses, ...} = destLab
                    val () = uses := !uses + 1
                    val skipOnSign =
                        case opc of
                            JG => ConditionalBranch{test=JE, predict=PredictNeutral, label=destLab}
                        |   JGE => ConditionalBranch{test=JE, predict=PredictNeutral, label=destLab}
                        |   JL => ConditionalBranch{test=JNE, predict=PredictNeutral, label=destLab}
                        |   JLE => ConditionalBranch{test=JNE, predict=PredictNeutral, label=destLab}
                        |   _ => raise InternalError "testTagsForComparison: opc"
                in
                    forwardJumpLabel skipLongLabel @
                    [skipOnSign, TestByteMem{base=tag1Reg, offset= ~1, bits=word8ToWord F_negative}] @
                    forwardJumpLabel longLab @ skipLongCode @ shortTestAndJump @ jumpOnLongCode @ [makeTest tag1]
                end
            |   testTagsForComparison (operations, tag1 as LiteralSource _, tag2, opc, destLab) =
                    (* First argument is the constant - reverse the arguments and the test. *)
                    testTagsForComparison(operations, tag2, tag1, reverseTestOp opc, destLab)
            |   testTagsForComparison (operations, tag1, tag2, _, _) =
                    testTags false (operations, tag1, tag2)

            (* Because short integers are normalised we can skip the tag tests
               completely when comparing short constants for equality.  Otherwise we
               only need emulation if both arguments are long. *)
            fun eqTestTags(operations, LiteralSource l1, _, _, _) =
                    if isShort l1 then operations else operations @ [CallRTS memRegArbEmulation]
            |   eqTestTags(operations, _, LiteralSource l2, _, _) =
                    if isShort l2 then operations else operations @ [CallRTS memRegArbEmulation]
            |   eqTestTags(operations, tag1, tag2, _, _) =
                let
                    val (code1, lab1) = condBranch(JNE, PredictTaken)
                    and (code2, lab2) = condBranch(JNE, PredictTaken)
                in
                    operations @ forwardJumpLabel lab1 @ forwardJumpLabel lab2 @
                    [CallRTS memRegArbEmulation] @ code2 @ [makeTest tag2] @ code1 @ [makeTest tag1]
                end

            fun noTagTest (operations, _, _) = operations (* When we don't need a test. *)
        end

        local
            (* Various instructions also affect the tag and that has to be reinstated after
               the operation. *)
            fun tagAdjust(ADD, reg) = [ArithRConst{opc=SUB, source=1, output=reg}]
            |   tagAdjust(SUB, reg) = [ArithRConst{opc=ADD, source=1, output=reg}]
            |   tagAdjust(OR, _) = [] (* Doesn't affect the tag. *)
            |   tagAdjust(AND, _) = [] (* Doesn't affect the tag. *)
            |   tagAdjust(XOR, reg) = [ArithRConst{opc=ADD, source=1, output=reg}]
            |   tagAdjust(CMP, _) = raise InternalError "tagAdjust"

            (* TODO: If we have the same argument on both sides (e.g. a+a) this seems to
               force them into different registers.  That also involves a double tag test. *)

            (* First select an output register.  Unless this is a SUB instruction we can choose
               a modifiable register from either argument. *)
            fun wordSelectDest (opT as (opcode, _)) pref [ActInRegisterSet{ modifiable=mod1, ... },
                                            ActInRegisterSet{ modifiable=mod2, ...}] =
                let
                    (* Assume that these are in general registers because of the type.
                       If this is subtraction we can only use a register from the
                       first argument. *)
                    val options = case opcode of SUB => mod1 | _ => mod1 ++ mod2
                in
                    if options = noRegisters (* No suitable modifiable register is available. *)
                    then ActionLoadArg{ argNo=0, regSet=generalRegisters, willOverwrite=true,
                                        next = wordSelectDest opT pref }
                    else (* There is one available. Pick a prefered reg is possible otherwise any. *)
                    let
                        val choice = oneOf(prefAsSet(pref, options, []))
                    in
                        if choice ins mod1
                        then ActionLockRegister { argNo=0, reg=choice, willOverwrite=true,
                                next=wordSelectedLeft (opT, choice) }
                        else ActionLockRegister { argNo=1, reg=choice, willOverwrite=true,
                                next=wordSelectedRight (opT, choice) }
                    end
                end

            |   wordSelectDest opT pref [ActInRegisterSet{ modifiable, ... }, _] =
                if modifiable = noRegisters (* No suitable modifiable register is available. *)
                then ActionLoadArg{ argNo=0, regSet=generalRegisters, willOverwrite=true,
                                    next = wordSelectDest opT pref }
                else (* There is one available. Pick a prefered reg is possible otherwise any. *)
                let
                    val choice = oneOf(prefAsSet(pref, modifiable, []))
                in
                    ActionLockRegister { argNo=0, reg=choice, willOverwrite=true,
                            next=wordSelectedLeft(opT, choice) }
                end

            |   wordSelectDest (opT as (SUB, _)) pref _ = (* Not reversible - load first arg. *)
                    ActionLoadArg{ argNo=0, regSet=generalRegisters, willOverwrite=true,
                                   next = wordSelectDest opT pref }

            |   wordSelectDest opT pref [_, ActInRegisterSet{ modifiable, ... }] =
                if modifiable = noRegisters (* No suitable modifiable register is available. *)
                then ActionLoadArg{ argNo=0, regSet=generalRegisters, willOverwrite=true,
                                    next = wordSelectDest opT pref }
                else (* There is one available. Pick a prefered reg is possible otherwise any. *)
                let
                    val choice = oneOf(prefAsSet(pref, modifiable, []))
                in
                    ActionLockRegister { argNo=1, reg=choice, willOverwrite=true,
                            next=wordSelectedRight(opT, choice) }
                end

            |   wordSelectDest opT pref _ = (* Neither in reg - load first arg. *)
                    ActionLoadArg{ argNo=0, regSet=generalRegisters, willOverwrite=true,
                                   next = wordSelectDest opT pref }

            (* We have selected a result register.  Get the other argument. *)
            and wordSelectedLeft ((opcode, tagTest), result) [_, ActInRegisterSet{ readable, ...}] =
                let
                    val argReg = oneOf readable
                in
                    ActionDone{ outReg=SOME result,
                                operation=tagAdjust(opcode, result) @
                                    tagTest([ArithRR{opc=opcode, source=argReg, output=result}],
                                            InRegister result, InRegister argReg)}
                end

            |   wordSelectedLeft (opRes as ((opcode, tagTest), result)) [_, ActLiteralSource lit] =
                if isShort lit
                then
                let
                    val intArg = Word.toIntX(toShort lit)
                    val source =
                        case opcode of
                            ADD => semitag intArg (* Shift but don't tag. *)
                        |   SUB => semitag intArg
                        |   OR  => tag intArg (* Could use either tag or semitag *)
                        |   AND => tag intArg (* Must include the tag bit *)
                        |   XOR => semitag intArg (* Leave the tag bit unchanged. *)
                        |   _ => raise InternalError "addWordArg: opcode"
                in
                    ActionDone{ outReg=SOME result,
                                operation=if intArg = 0 andalso opcode <> AND then [] (* Discard *)
                                          else tagTest([ArithRConst{opc=opcode, source=source, output=result}],
                                                  InRegister result, LiteralSource lit)}
                end
                else (* Long literal - put it into a register. We'd have been better not code-generating
                        this at all. *)
                    ActionLoadArg{ argNo=1, regSet=generalRegisters, willOverwrite=false,
                                   next = wordSelectedLeft opRes }

            |   wordSelectedLeft ((opcode, tagTest), result) [_, ActBaseOffset(base, offset)] =
                    ActionDone{ outReg=SOME result,
                            operation=tagAdjust(opcode, result) @
                                      tagTest([ArithRMem{opc=opcode, base=base, offset=offset, output=result}],
                                               InRegister result, BaseOffsetSource{base=base, offset=offset})}

            |   wordSelectedLeft _ _  = raise InternalError "addWordArg"

            and wordSelectedRight opRes (args as [ActLiteralSource lit, _]) =
                if isShort lit then wordSelectedLeft opRes (List.rev args)
                else (* Long literal - put it into a register. *)
                    ActionLoadArg{ argNo=0, regSet=generalRegisters, willOverwrite=false,
                                   next = wordSelectedRight opRes }

            |   wordSelectedRight opRes args = wordSelectedLeft opRes (List.rev args)

            (* If either of the arguments are long constants we don't code-generate and
               instead fall back to the RTS.  This is likely to be more efficient. *)
            fun checkShort (opc, args, mapper) =
                if List.exists(fn a => case mapper a of SOME n => not(isShort n) | NONE => false) args
                then NONE
                else SOME(wordSelectDest(opc, testTagsForArithmetic), args)
        in
            val instrAddA = fn (args, mapper) => checkShort(ADD, args, mapper)
            and instrSubA = fn (args, mapper) => checkShort(SUB, args, mapper)
            and instrAddW = fn (args, _) => SOME(wordSelectDest(ADD, noTagTest), args)
            and instrSubW = fn (args, _) => SOME(wordSelectDest(SUB, noTagTest), args)
            and instrOrW  = fn (args, _)  => SOME(wordSelectDest(OR, noTagTest), args)
            and instrAndW = fn (args, _) => SOME(wordSelectDest(AND, noTagTest), args)
            and instrXorW = fn (args, _) => SOME(wordSelectDest(XOR, noTagTest), args)
        end

        local
            (* If the source or the index are in registers they have to be untagged first and
               then retagged afterwards. *)
            fun genStoreByte([base, index, toStore], NONE) =
                let
                    val (untagIndex, retagIndex) =
                        case index of
                            InRegister indexReg =>
                                ([ShiftConstant{shiftType=SRL, output=indexReg, shift=0w1}],
                                 [TagValue{source=indexReg, output=indexReg }])
                        |   _ => ([], [])
                    val address =
                        case (base, index) of
                            (InRegister address, InRegister index) =>
                                BaseOffset{offset=0, base=address, index=Index1 index}
                        |   (InRegister address, LiteralSource offset) =>
                                BaseOffset{offset=Word.toInt(toShort offset), base=address, index=NoIndex }
                        |   (LiteralSource address, LiteralSource offset) =>
                                if isShort offset andalso toShort offset = 0w0 andalso not isX64
                                then ConstantAddress address
                                else raise InternalError "genStoreByte"
                        |   _ => raise InternalError "genStoreByte"
                in
                    case toStore of
                        InRegister toStore =>
                            retagIndex @
                            [
                                TagValue{source=toStore, output=toStore },
                                StoreByteRegToMemory{ toStore=toStore, address=address },
                                ShiftConstant{shiftType=SRL, output=toStore, shift=0w1}(* Untag value to store *)
                            ] @ untagIndex
                    |   LiteralSource toStore =>
                            retagIndex @
                                [ StoreByteConstToMemory{ toStore=wordToWord8(toShort toStore), address=address}] @
                            untagIndex
                    |   _ => raise InternalError "genStoreByte"
                            
                end
            |   genStoreByte _ = raise InternalError "genStoreByte"

            (* We store from the low order byte of a register so we want the value in one of
               the registers that has a low-byte form i.e. AL. BL, CL, DL. *)
            fun storeByte _ =
               loadToRegOrLiteral(listToSet[eax, ebx, ecx, edx], 2,
                    loadBaseAddress(NoResult,
                        loadToRegOrLiteral(generalRegisters, 1,
                            generateInstruction genStoreByte))) ([NONE, NONE, NONE], NONE)
        in
            fun instrStoreB(args, _) = SOME(storeByte, args)
        end

        local
            (* Load the flags byte from a memory segment.  The result must be tagged. *)
            (* We could reuse the base register for the output. *)
            fun genVecFlags([InRegister baseReg], SOME outReg) =
                [TagValue{source=outReg, output=outReg},
                 LoadByteR{source=BaseOffset{base=baseReg, offset= ~1, index=NoIndex}, output=outReg}]
            |   genVecFlags _ = raise InternalError "genVecFlags"
        in
            fun instrVecflags(args, _) = SOME(fn pref => generalNegotiator(genVecFlags, pref), args)
        end

        local
            (* Get the first word of a long integer.  Used when converting to "word". *)
            fun genFirstLong([InRegister baseReg], SOME outReg) =
                let
                    val (code, lab) = condBranch(JE, PredictNeutral)
                in
                    [TagValue{source=outReg, output=outReg} ] @ forwardJumpLabel lab @
                    [Group3Ops(outReg, NEG)] @ (* Negate if the sign was set. *)
                    code @
                    [
                        (* Test the the sign bit in the header. *)
                        TestByteMem{offset= ~1, base=baseReg, bits=0w16 },
                        LoadMemR{source=BaseOffset{base=baseReg, offset=0, index=NoIndex}, output=outReg}
                    ]
                end
            |   genFirstLong _ = raise InternalError "genFirstLong"
        in
            (* This is another case where we could reuse the argument for the result. *)
            fun instrGetFirstLong(args, _) = SOME(fn pref => generalNegotiator(genFirstLong, pref), args)
        end

        local (* Get the length of a string. *)
            fun genStringLength([InRegister rs], SOME rd) =
                let
                    val (condBr, lab1) = condBranch(JE, PredictTaken(*More likely long*))
                    and (uncondBr, lab2) = uncondBranch()
                in
                    forwardJumpLabel lab2 @
                    [
                        TagValue{source=rd, output=rd},
                        LoadMemR{source=BaseOffset{base=rs, offset=0, index=NoIndex}, output=rd} (* It's an address. *)
                    ] @ forwardJumpLabel lab1 @ uncondBr @
                    [MoveConstR{source=tag 1, output=rd}] @ (* It's a byte: result is 1 *) condBr @
                    [TestTagR rs (* Is it a single byte? *)]
                end
            |   genStringLength _ = raise InternalError "genStringLength"

            (* We could use the same register for the result and the argument.  For the
               moment we force it to use different regs. *)
        in
            fun instrStringLength(args, _) =
                    SOME(fn pref => generalNegotiator(genStringLength, pref), args)
        end

        local (* Set the length word of a string. *)
            fun genSetStringStringLength ([InRegister base, InRegister length], NONE) =
                [
                    TagValue{ source=length, output=length }, (* Retag it *)
                    StoreRegToMemory{ toStore=length, address=BaseOffset{offset=0, base=base, index=NoIndex }},
                    ShiftConstant{shiftType=SRL, output=length, shift=0w1 } (* Untag it *)
                ]
            |   genSetStringStringLength _ = raise InternalError "genSetStringStringLength"
        in
            fun instrSetStringLength(args, _) = SOME(fn _ => noresultNegotiator genSetStringStringLength, args)
        end

        local
            (* Shift operations: shifting by a variable amount requires ecx.
               Some special cases of multiplication can be implemented as shifts. *)
            (* Variable shifts always use the cl register i.e. the low order byte of ecx. *)
            fun loadShiftArg (instr, pref) opAndResult =
                loadToSpecificReg(ecx, true, 1, loadResultArg(instr, pref)) opAndResult

            (* Load the first argument into a register we can use for the result. *)
            and loadResultArg (instr, pref) opAndResult [arg1, _] =
            (
                case arg1 of
                    (ActInRegisterSet{ modifiable, ...}) =>
                        if regSetIntersect(modifiable, generalRegisters) <> noRegisters
                        then
                        let
                            val aReg = oneOf(regSetIntersect(modifiable, generalRegisters))
                        in
                            ActionLockRegister{argNo=0, reg=aReg, willOverwrite=true,
                                next=finished(instr, aReg)}
                        end
                        else (* Can't clobber this register - move the value. *)
                            ActionLoadArg{argNo=0, regSet=generalRegisters, willOverwrite=true,
                                next=loadResultArg(instr, pref) opAndResult}
                |   _ =>
                        ActionLoadArg{argNo=0, regSet=generalRegisters, willOverwrite=true,
                             next=loadResultArg(instr, pref) opAndResult}
            )
            |   loadResultArg _ _ _ = raise InternalError "loadResultArg: bad arguments"
            
            and finished (genInstr, resReg) _ =
                ActionDone{
                    outReg=SOME resReg,
                    operation=genInstr([InRegister resReg, InRegister ecx], SOME resReg)
                    }

            fun shiftNegotiator (instr, pref) = loadShiftArg (instr, pref) ([NONE, NONE], NONE)

            (* We have to consider what happens to the tag.  Shifts are word values so are
               always non-negative.  N.B. This is the untagged value i.e. 31 or 63. *)
            val maxShift = Word.fromInt wordSize * 0w8 - 0w1

            fun genConstShift(_, 0w0) _ = [] (* Zero shifts do nothing *)

            |   genConstShift(SLL, shift) (_, SOME reg) =
                    (* Left shift. Use LEAL for some cases otherwise perform the
                       shift and then subtract 1 << shift -1.  This removes the existing shifted
                       tag and sets the bottom bit. *)
                    if shift > maxShift
                    then [MoveConstR{source=tag 0, output=reg}](* This is defined to return zero. *)
                    else
                    (
                        case shift of
                            0w1 => [LoadAddress{output=reg, base=SOME reg, index=Index1 reg, offset= ~1}]
                        |   0w2 => [LoadAddress{output=reg, base=NONE, index=Index4 reg, offset= ~3}]
                        |   0w3 => [LoadAddress{output=reg, base=NONE, index=Index8 reg, offset= ~7}]
                        |   _ =>
                            if shift < 0w32
                            then (* Can remove the shifted tag and set the new tag. *)
                                [ArithRConst{opc=SUB, output=reg, source=IntInf.<<(1, shift) -1 },
                                 ShiftConstant{shiftType=SLL, output=reg, shift=wordToWord8 shift}]
                            else (* 64-bit only.  If the shift is more than 32 bits we remove the
                                    tag beforehand and put it back afterwards. *)
                                [ArithRConst{opc=OR, output=reg, source=1 },
                                 ShiftConstant{shiftType=SLL, output=reg, shift=wordToWord8 shift},
                                 ArithRConst{opc=SUB, output=reg, source=1 }]
                    )

            |   genConstShift(SRL, shift) (_, SOME reg) = (* Right logical shift. *)
                    if shift > maxShift
                    then [MoveConstR{source=tag 0, output=reg}](* This is defined to return zero. *)
                    else (* The original tag will have been shifted out but we have to
                            set the bottom bit as the tag. *)
                        [ArithRConst{opc=OR, output=reg, source=1 },
                          ShiftConstant{shiftType=SRL, output=reg, shift=wordToWord8 shift}]

            |   genConstShift(SRA, shift) (_, SOME reg) = (* Right shifts absorb the tag. *)
                    (* If the shift is more than the word length this returns 0 for positive values
                       and all ones for negative.  We just limit the shift to 31/63. *)
                    [ArithRConst{opc=OR, output=reg, source=1 },
                     ShiftConstant{shiftType=SRA, output=reg,
                        shift=wordToWord8(Word.max(maxShift, shift))}]

            |   genConstShift _ _ = raise InternalError "genConstShift"

            (* The X86 masks the shift value but the ML basis library requires shift values
               greater than the word size to set the value to 0/-1 as appropriate.
               We set the shift to the maximum if it is larger rather than trying to
               set the result to the appropriate value. The shift value is always in ecx. *)
            and genVarShift shift ([InRegister r, InRegister shiftR], SOME reg) =
                let
                    val _ = r = reg orelse raise InternalError "genVarShift: different regs"
                    val _ = shiftR = ecx orelse raise InternalError "genVarShift: shift not in ecx"
                    val (test, lab) = condBranch(JNA, PredictTaken)
                in
                    [
                        MakeSafe ecx, (* ecx may be invalid. *)
                        ArithRConst{ opc=OR, output=reg, source= 1 }, (* Set dest tag. *)
                        ShiftVariable { shiftType=shift, output=reg }, (* Do the shift *)
                        ShiftConstant{shiftType=SRL, output=ecx, shift=0w1}(* Untag ecx *)
                    ] @ forwardJumpLabel lab @
                    [ MoveConstR{ source=tag(Word.toInt maxShift), output=ecx } ] @ test @
                    [ ArithRConst{ opc=CMP, output=ecx, source=tag(Word.toInt maxShift) }
                    ] @
                    (
                        (* If this is a left shift we have to remove the tag.  That isn't needed
                           for right shifts.  Use a subtraction because we may be able to merge
                           this with preceding operations. *)
                        case shift of SLL => [ArithRConst{ opc=SUB, output=reg, source= 1}]
                        |   _ => []
                    )
                end
            |   genVarShift _ _ = raise InternalError "genVarShift"

            fun constantShift(opc, shift) pref = sharedSingleArgNegotiator(genConstShift(opc, shift), pref)
            and variableShift opc pref = shiftNegotiator(genVarShift opc, pref)

            fun shiftInstr opc (args as [arg1, arg2], mapper) =
            (
                case mapper arg2 of
                    SOME lit => SOME(constantShift(opc, toShort lit), [arg1])
                |   NONE => SOME(variableShift opc, args)
            )
            |   shiftInstr _ _ = raise InternalError "shiftInstr"

            fun getPower2 n =
            let
                fun p2(i, log) = if i < n then p2 (i*0w2, log+0w1) else if i = n then SOME log else NONE
            in
                p2(0w1, 0w0)
            end

            local
                (* Multiply always places the result in EDX:EAX.  Since we're going to modify those
                   registers the best arrangement for the arguments is to get the first argument into
                   eax and the second into edx. *)
                val eaxSet = singleton eax and edxSet = singleton edx

                fun loadArg1 instr [(ActInRegisterSet{ modifiable, ...}), _] =
                    if regSetIntersect(modifiable, eaxSet) <> noRegisters
                    then ActionLockRegister{argNo=0, reg=eax, willOverwrite=true, next=loadArg2 instr}
                    else (* Not in the right register - move it. *)
                        ActionLoadArg{argNo=0, regSet=eaxSet, willOverwrite=true, next=loadArg1 instr}

                |   loadArg1 instr [_, _] =
                        ActionLoadArg{argNo=0, regSet=eaxSet, willOverwrite=true, next=loadArg1 instr}

                |   loadArg1 _ _ = raise InternalError "loadArg1: bad arguments"
            
                and loadArg2 instr [_, (ActInRegisterSet{ modifiable, ...})] =
                    if regSetIntersect(modifiable, edxSet) <> noRegisters
                    then ActionLockRegister{argNo=1, reg=edx, willOverwrite=true, next=finished instr}
                    else (* Not in the right register - move it. *)
                        ActionLoadArg{argNo=1, regSet=edxSet, willOverwrite=true, next=loadArg2 instr}
                    
                |   loadArg2 instr [_, _] =
                        ActionLoadArg{argNo=1, regSet=edxSet, willOverwrite=true, next=loadArg2 instr}

                |   loadArg2 _ _ = raise InternalError "loadArg1: bad arguments"

                and finished genInstr _ =
                    ActionDone{
                        outReg=SOME eax,
                        operation=genInstr([InRegister eax, InRegister edx], SOME eax)
                        }
            in
                val multiplyNegotiator = loadArg1
            end

            fun genMultiplyUnsigned _ = (* The arguments are in eax and edx and result in eax. *)
            [
                MakeSafe edx, (* Could just copy eax in here *)
                (* Add back the tag, but don't shift. *)
                ArithRConst{opc=ADD, output=eax, source=1},
                Group3Ops(edx, MUL),
                (* Shift down the multiplier. *)
                ShiftConstant{shiftType=SRL, output=edx, shift=0w1 },
                (* Untag, but don't shift the multiplicand. *)
                ArithRConst{opc=SUB, output=eax, source=1}
            ]

            (* The only case we treat specially is multiplication by two because the overflow
               flag isn't defined for larger shifts. *)
            fun genAddSelf(_, SOME output) =
                let
                    val (skipEmulator, skipEmulatorLab) = uncondBranch()
                    val (lab1Code, lab1) = condBranch(JE, PredictNotTaken)
                    val (lab2Code, lab2) = condBranch(JO, PredictNotTaken)
                    val (backToInstr, backToInstrLab) = backJumpLabel()
                in
                    [ArithRConst{opc=SUB, source=1, output=output}] @
                        forwardJumpLabel skipEmulatorLab @ jumpBack backToInstrLab @
                    [CallRTS memRegArbEmulation] @ forwardJumpLabel lab1 @
                    forwardJumpLabel lab2 @ skipEmulator @ lab2Code @
                    [ArithRR{opc=ADD, source=output, output=output}] @ backToInstr @ lab1Code @
                    [TestTagR output]
                end
            |   genAddSelf _ = raise InternalError "genAddSelf"

            fun mulAConst2 pref = sharedSingleArgNegotiator(genAddSelf, pref)
         in
            fun instrUpshiftW am = shiftInstr SLL am
            and instrDownshiftW am = shiftInstr SRL am
            and instrDownshiftArithW am = shiftInstr SRA am

            fun instrMulW(args as [arg1, arg2], mapper) =
                (
                   (* This is implemented specially for powers of 2.  It could also
                      be implemented specially for some other cases as well. *)
                    case List.map (Option.composePartial(getPower2 o toShort, mapper)) args of
                        [_, SOME shift] => SOME(constantShift(SLL, shift), [arg1])
                    |   [SOME shift, _] => SOME(constantShift(SLL, shift), [arg2])
                    |   _ => SOME(fn _ => multiplyNegotiator genMultiplyUnsigned, args)
                )
            |   instrMulW _ = raise InternalError "instrMulW"

            fun instrMulA(args as [arg1, arg2], mapper) =
            (
                (* The only special case we recognise is multiplication by 2. *)
                case List.map mapper args of
                    [_, SOME lit] =>
                        if isShort lit andalso toShort lit = 0w2
                        then SOME(mulAConst2, [arg1])
                        else NONE(*SOME(InstrMulA, args)*)
                |   [SOME lit, _] =>
                        if isShort lit andalso toShort lit = 0w2
                        then SOME(mulAConst2, [arg2])
                        else NONE(*SOME(InstrMulA, args)*)
                |   _ => NONE(*SOME(InstrMulA, args)*)
            )
            |   instrMulA _ = raise InternalError "instrMulA"

            local (* Unsigned word div and mod. *)
                (* Division by a power of two is just a right shift. *)
                fun shiftDown shift = constantShift(SRL, shift)
                (* Modulo by a power of two is a mask. *)
                fun maskBits shift pref =
                let
                    fun genMask(_, SOME resReg) =
                        if shift > maxShift
                        then []
                        else (* Because we're going to mask a tagged value we need to add 1 to the shift. *)
                           [ArithRConst{opc=AND, output=resReg, source=IntInf.<<(1, shift+0w1) -1 }]
                    |   genMask _ = raise InternalError "genMask"
                in
                    sharedSingleArgNegotiator(genMask, pref)
                end

                (* If the dividend is not a power of two we have to use the DIV instruction.
                   This takes the divisor in the EDX/EAX pair and the dividend in a separate
                   register.  We get EDX as a work register and put the divisor in EAX. *)
                fun genDivMod isDiv ([_, InRegister dividend], _) =
                    let
                        val (notZero, notZeroLab) = condBranch(JNE, PredictTaken)
                    in
                        (* Because we didn't shift the dividend and the divisor we don't
                           need to shift the remainder but just add in the tag.  We do have
                           to shift and tag the quotient.  Either way we have to make the
                           other register safe. *)
                        (if isDiv
                        then [MakeSafe edx, TagValue{source=eax, output=eax}]
                        else [MakeSafe eax, ArithRConst{opc=ADD, output=edx, source=1}]) @
                        [
                            (* Clear the high order of the accumulator and do the division. *)
                            Group3Ops(dividend, DIV),
                            ArithRR{opc=XOR, source=edx, output=edx},
                            (* Untag, but don't shift, the divisor and dividend.  At the
                               same time check for zero and raise an exception if it is. *)
                            ArithRConst{opc=SUB, source=1, output=eax}
                        ] @ forwardJumpLabel notZeroLab @ [CallRTS memRegRaiseDiv ] @ notZero @
                        [ArithRConst{opc=SUB, source=1, output=dividend}]
                    end

                |   genDivMod _ _ = raise InternalError "genDivMod"

                fun divModCode isDiv _ =
                    getWorkingRegister(singleton edx,
                        fn _ => loadToSpecificReg(eax, true, 0,
                            loadToOneOf(generalRegisters, false, 1,
                                generateInstruction(genDivMod isDiv)
                            )
                        )
                    ) ([NONE, NONE], SOME(if isDiv then eax else edx))

                fun checkPower2 (p2Code, genCode, args as [arg1, arg2], mapper) =
                    ( (* Special case where the dividend is a power of two.  We could maybe
                         take out the special case of zero here and just raise an exception but
                         that's hardly worth it. *)
                        case Option.composePartial(getPower2 o toShort, mapper) arg2 of
                            SOME n => SOME(p2Code n, [arg1])
                        |   NONE => SOME(genCode, args)
                    )
                |   checkPower2 _ = raise InternalError "checkPower2"
            in
                fun instrDivW(args, mapper) = checkPower2(shiftDown, divModCode true, args, mapper)
                and instrModW(args, mapper) = checkPower2(maskBits, divModCode false, args, mapper)
            end
        end

        local
            (* Load the length word from a memory cell.  We could reuse the base for the
               result rather than getting a new register here. *)
            val mask = IntInf.<<(1, (Word.fromInt wordSize-0w1)*0w8) - 1

            fun vecLen([InRegister base], SOME output) =
                [TagValue{source=output, output=output},
                 ArithRMem{opc=AND, output=output, base=base, offset= ~ wordSize },
                 MoveConstR{source= mask, output=output }]
            |   vecLen _ = raise InternalError "vecLen"
        in
            fun instrVeclen(args, _) = SOME(fn prefs => generalNegotiator(vecLen, prefs), args)
        end

        local
            (* Load the "self" entry from the thread-specific block. *)
            fun threadSelf([], SOME output) =
                [LoadMemR{source=BaseOffset{base=ebp, offset=memRegThreadSelf, index=NoIndex}, output=output}]
            |   threadSelf _ = raise InternalError "threadSelf"
        in
            fun instrThreadSelf(args, _) = SOME(fn prefs => generalNegotiator(threadSelf, prefs), args)
        end

        local
            fun atomicOp incr ([InRegister rs], SOME rd) =
                [
                    (* Since xadd returns the original value we have to add in the value again. *)
                    ArithRConst{opc=ADD, output=rd, source=semitag incr},
                    AtomicXAdd{base=rs, output=rd},
                    MoveConstR{source=semitag incr, output=rd}
                ]
            |   atomicOp _ _ = raise InternalError "atomicOp"
        in
            fun instrAtomicIncr(args, _) = SOME(fn pref => generalNegotiator(atomicOp 1, pref), args)
            and instrAtomicDecr(args, _) = SOME(fn pref => generalNegotiator(atomicOp ~1, pref), args)
        end
        
        local
            fun byteMemMove([InRegister _, sourceOffset, InRegister _, destOffset, InRegister _], _) =
                let (* Byte move.  The offsets are byte offsets. *)
                    fun getOffset(InRegister offsetReg, baseReg) =
                        [
                            (* If it's in a register untag it, add it and then retag. *)
                            TagValue{ source=offsetReg, output=offsetReg},
                            ArithRR{ opc=ADD, output=baseReg, source=offsetReg },
                            ShiftConstant{ shiftType=SRL, output=offsetReg, shift=0w1}
                        ]
                        |   getOffset(LiteralSource lit, baseReg) =
                            let
                                val shortLit = toShort lit
                            in
                                if shortLit = 0w0 then [] else
                                [ArithRConst{opc=ADD, output=baseReg, source=Word.toInt shortLit}]
                            end
                        |   getOffset _ = raise InternalError "getOffset"
                in
                    [MakeSafe ecx, MakeSafe esi, MakeSafe edi, RepeatOperation MOVSB] @
                    getOffset(destOffset, edi) @ getOffset(sourceOffset, esi) @
                    (* Untag the length. *)
                    [ShiftConstant{ shiftType=SRL, output=ecx, shift=0w1}]
                end

            |   byteMemMove _ = raise InternalError "byteMemMove"
            
            and wordMemMove([InRegister _, sourceOffset, InRegister _, destOffset, InRegister _], _) =
                let
                    (* Word move.  The offsets and the count are the number of words.
                       We can compute the offset by using the LEAL instruction. *)
                    fun getOffset(InRegister offsetReg, baseReg) =
                        if wordSize = 4
                        then [LoadAddress{output=baseReg, offset= ~2, base=SOME baseReg, index=Index2 offsetReg }]
                        else [LoadAddress{output=baseReg, offset= ~4, base=SOME baseReg, index=Index4 offsetReg }]
                    |   getOffset(LiteralSource lit, baseReg) =
                        let
                            val shortLit = toShort lit
                        in
                            if shortLit = 0w0 then [] else
                            [ArithRConst{opc=ADD, output=baseReg, source=Word.toInt shortLit * wordSize}]
                        end
                    |   getOffset _ = raise InternalError "getOffset"
                in
                    [MakeSafe ecx, MakeSafe esi, MakeSafe edi, RepeatOperation MOVSL] @
                    getOffset(destOffset, edi) @ getOffset(sourceOffset, esi) @
                    (* Untag the length. *)
                    [ShiftConstant{ shiftType=SRL, output=ecx, shift=0w1}]
                end
            |   wordMemMove _ = raise InternalError "byteMemMove"

            fun genMemMove memMove _ =
                (* Memory moves: The source must be in esi, the destination in edi and the count in ecx.
                   The source and destination offsets can be any registers.  We can't add the offsets in
                   advance because that would create invalid addresses. *)
                loadToSpecificReg(esi, true, 0,
                    loadToSpecificReg(edi, true, 2,
                        loadToSpecificReg(ecx, true, 4,
                            allInRegsOrLiterals(generalRegisters, generateInstruction memMove)
                            ))) ([NONE, NONE, NONE, NONE, NONE], NONE)
        in
            fun instrMoveBytes(args, _) = SOME(genMemMove byteMemMove, args)
            and instrMoveWords(args, _) = SOME(genMemMove wordMemMove, args)
        end

        local
            (* Pick an argument register from the general register set. *)
            fun chooseGenReg{readable, ...} = oneOf(regSetIntersect(readable, generalRegisters))

            (* Test to see if this is testing arbitrary precision for ordering.  Because of
               the way we deal with long-precision comparisons we can't use the memory-constant
               form for arbitrary precision valuess. *)
            fun isIntOrdering JG = true
            |   isIntOrdering JGE = true
            |   isIntOrdering JL = true
            |   isIntOrdering JLE = true
            |   isIntOrdering _ = false

            (* Word comparisons are used both for the word type itself but also for tests
               such as tag tests and pointer comparisons.  The x86 allows most combinations
               of registers, memory and constants. *)

            fun testActions (testType, destLabel, tagTest) ([ActBaseOffset _, ActBaseOffset _]) =
                    (* Both in memory: Load one. *)
                    ActionLoadArg{argNo=0, regSet=generalRegisters, willOverwrite=false,
                                  next=testActions(testType, destLabel, tagTest)}

            |   testActions (testType, destLabel, tagTest) ([ActLiteralSource _, ActLiteralSource _]) =
                    (* Both constants: Should have been optimised. *)
                    ActionLoadArg{argNo=0, regSet=generalRegisters, willOverwrite=false,
                                  next=testActions(testType, destLabel, tagTest)}
                    
            |   testActions (testType, destLabel, tagTest) ([ActInRegisterSet s1, ActInRegisterSet s2]) =
                let
                    val reg1 = chooseGenReg s1 and reg2 = chooseGenReg s2
                    val Labels{uses, ...} = destLabel
                    val () = uses := !uses+1
                in
                    ActionDone{ outReg=NONE,
                                operation=ConditionalBranch { test=testType, label=destLabel, predict=PredictNeutral }::
                                           tagTest([ArithRR{opc=CMP, output=reg1, source=reg2}],
                                                   InRegister reg1, InRegister reg2, testType, destLabel) }
                end

            |   testActions (testType, destLabel, tagTest) ([ActInRegisterSet s1, ActLiteralSource constnt]) =
                let
                    val reg1 = chooseGenReg s1
                    val Labels{uses, ...} = destLabel
                    val () = uses := !uses+1
                in
                    ActionDone{ outReg=NONE,
                                operation=
                                    tagTest(
                                           [ConditionalBranch { test=testType, label=destLabel, predict=PredictNeutral },
                                           if isShort constnt
                                           then ArithRConst{opc=CMP, output=reg1,
                                                            source=tag(Word.toIntX(toShort constnt))}
                                           else ArithRLongConst{opc=CMP, output=reg1, source=constnt}],
                                           InRegister reg1, LiteralSource constnt, testType, destLabel) }
                end

            |   testActions (testType, destLabel, tagTest) ([ActLiteralSource constnt, ActInRegisterSet s2]) =
                let
                    val reg2 = chooseGenReg s2
                    val Labels{uses, ...} = destLabel
                    val () = uses := !uses+1
                    val revTest = reverseTestOp testType
                in
                    ActionDone{ outReg=NONE,
                                operation=
                                    tagTest(
                                           [ConditionalBranch
                                                { test=revTest, label=destLabel, predict=PredictNeutral },
                                           if isShort constnt
                                           then ArithRConst{opc=CMP, output=reg2,
                                                            source=tag(Word.toIntX(toShort constnt))}
                                           else ArithRLongConst{opc=CMP, output=reg2, source=constnt}],
                                           InRegister reg2, LiteralSource constnt, revTest, destLabel) }
                end

            |   testActions (testType, destLabel, tagTest) ([ActInRegisterSet s1, ActBaseOffset(base, offset)]) =
                let
                    val reg1 = chooseGenReg s1
                    val Labels{uses, ...} = destLabel
                    val () = uses := !uses+1
                in
                    ActionDone{ outReg=NONE,
                                operation=
                                    tagTest([ConditionalBranch { test=testType, label=destLabel, predict=PredictNeutral },
                                             ArithRMem{opc=CMP, output=reg1, base=base, offset=offset}],
                                           InRegister reg1, BaseOffsetSource{base=base, offset=offset},
                                           testType, destLabel)}
                end

            |   testActions (testType, destLabel, tagTest) ([ActBaseOffset(base, offset), ActInRegisterSet s2]) =
                let
                    val reg2 = chooseGenReg s2
                    val Labels{uses, ...} = destLabel
                    val () = uses := !uses+1
                    val revTest = reverseTestOp testType
                in
                    ActionDone{ outReg=NONE,
                                operation=
                                    tagTest([ConditionalBranch
                                                { test=revTest, label=destLabel, predict=PredictNeutral },
                                             ArithRMem{opc=CMP, output=reg2, base=base, offset=offset}],
                                           InRegister reg2, BaseOffsetSource{base=base, offset=offset},
                                           revTest, destLabel)}
                end

            |   testActions (testType, destLabel, tagTest) ([ActBaseOffset(base, offset), ActLiteralSource constnt]) =
                if not (isIntOrdering testType) andalso (not isX64 orelse is31bitSigned constnt)
                then (* In 32-bit mode we can put all constants inline but in 64-bit mode anything
                        that won't fit in 32-bits as well as all addresses need to go in the
                        constant area.  That requires memory addressing and we don't have
                        memory-memory comparison operations. *)
                let
                    val Labels{uses, ...} = destLabel
                    val () = uses := !uses+1
                in
                    ActionDone{ outReg=NONE,
                                operation=
                                    tagTest(
                                           [ConditionalBranch { test=testType, label=destLabel, predict=PredictNeutral },
                                           if isShort constnt
                                           then ArithMemConst{opc=CMP, base=base, offset=offset,
                                                              source=tag(Word.toIntX(toShort constnt))}
                                           else ArithMemLongConst{opc=CMP, base=base, offset=offset, source=constnt}],
                                           BaseOffsetSource{base=base, offset=offset}, LiteralSource constnt,
                                           testType, destLabel)}
                end
                else (* If it won't fit in 32-bits we have to load one of the args. *)
                    ActionLoadArg{argNo=0, regSet=generalRegisters, willOverwrite=false,
                                  next=testActions(testType, destLabel, tagTest)}

            |   testActions (testType, destLabel, tagTest) ([ActLiteralSource constnt, ActBaseOffset(base, offset)]) =
                if not (isIntOrdering testType) andalso (not isX64 orelse is31bitSigned constnt)
                then
                let
                    val Labels{uses, ...} = destLabel
                    val () = uses := !uses+1
                    val revTest = reverseTestOp testType
                in
                    ActionDone{ outReg=NONE,
                                operation=
                                      tagTest(
                                           [ConditionalBranch
                                                { test=revTest, label=destLabel, predict=PredictNeutral },
                                           if isShort constnt
                                           then ArithMemConst{opc=CMP, base=base, offset=offset,
                                                              source=tag(Word.toIntX(toShort constnt))}
                                           else ArithMemLongConst{opc=CMP, base=base, offset=offset, source=constnt}],
                                           BaseOffsetSource{base=base, offset=offset}, LiteralSource constnt,
                                           revTest, destLabel) }
                end
                else (* If it won't fit in 32-bits we have to load one of the args. *)
                    ActionLoadArg{argNo=1, regSet=generalRegisters, willOverwrite=false,
                                  next=testActions(testType, destLabel, tagTest)}

            |   testActions _ _  = raise InternalError "testActions"
                

            fun makeBranch (testType, tagTest) () =
            let
                val destLabel = mkLabel()
            in
                (testActions(testType, destLabel, tagTest), destLabel)
            end

            fun lengthTest test () =
            let
                val destLabel as Labels{uses, ...} = mkLabel()
                fun testActions ([ActInRegisterSet regs]) =
                    (
                        uses := !uses+1;
                        ActionDone { outReg=NONE,
                            operation=[ConditionalBranch { test=test, label=destLabel, predict=PredictNeutral },
                                       TestTagR(chooseGenReg regs)]}
                    )
                |   testActions([ActBaseOffset(base, offset)]) =
                    (
                        uses := !uses+1;
                        ActionDone { outReg=NONE,
                            operation=[ConditionalBranch { test=test, label=destLabel, predict=PredictNeutral },
                                       TestByteMem{base=base, offset=offset, bits=0w1}]}
                    )
                |   testActions([ActLiteralSource _]) =
                        (* This should have been optimised away. *)
                        ActionLoadArg{argNo=0, regSet=generalRegisters, willOverwrite=false,
                                      next=testActions}
                |   testActions _ = raise InternalError "lengthTestActions"
            in
                (testActions: nextAction, destLabel)
            end

            (* If either of the arguments are long constants we don't code-generate and
               instead fall back to the RTS.  This is likely to be more efficient. *)
            fun checkShort (opc, test) (args, mapper) =
                if List.exists(fn a => case mapper a of SOME n => not(isShort n) | NONE => false) args
                then NONE
                else SOME(makeBranch(opc, test), args)
            fun noTagTest (operations, _, _, _, _) = operations (* When we don't need a test. *)
         in
            fun testNeqW(args, _) = SOME(makeBranch(JNE, noTagTest), args)
            fun testEqW(args, _)  = SOME(makeBranch(JE, noTagTest), args)
            fun testGeqW(args, _) = SOME(makeBranch(JNB, noTagTest), args) (* These are unsigned. *)
            fun testGtW (args, _) = SOME(makeBranch(JA, noTagTest), args)
            fun testLeqW(args, _) = SOME(makeBranch(JNA, noTagTest), args)
            fun testLtW(args, _)  = SOME(makeBranch(JB, noTagTest), args)
            fun testNeqA(args, mapper)   = checkShort(JNE, eqTestTags)(args, mapper)
            and testEqA(args, mapper)    = checkShort(JE, eqTestTags)(args, mapper)
            and testGeqA(args, mapper)   = checkShort(JGE, testTagsForComparison)(args, mapper)
            and testGtA(args, mapper)    = checkShort(JG, testTagsForComparison)(args, mapper)
            and testLeqA(args, mapper)   = checkShort(JLE, testTagsForComparison)(args, mapper)
            and testLtA(args, mapper)    = checkShort(JL, testTagsForComparison)(args, mapper)
            fun Short(args, _)    = SOME(lengthTest JNE, args)
            fun Long(args, _)     = SOME(lengthTest JE, args)
        end

        local
            fun genByteVec test () =
            let
                val destLabel as Labels{uses, ...} = mkLabel()

                fun generateByteVec([InRegister addr1, offset1, InRegister addr2, offset2, InRegister len]) =
                let
                    val () = if addr1 = esi andalso addr2 = edi andalso len = ecx then ()
                             else raise InternalError "Incorrect registers"
                    fun getOffset(InRegister offsetReg, baseReg) =
                        [
                            (* If it's in a register untag it, add it and then retag. *)
                            TagValue{ source=offsetReg, output=offsetReg },
                            ArithRR{opc=ADD, output=baseReg, source=offsetReg},
                            ShiftConstant{shiftType=SRL, output=offsetReg, shift=0w1}
                        ]
                    |   getOffset(LiteralSource lit, baseReg) =
                        let
                            val shortLit = toShort lit
                        in
                            if shortLit = 0w0 then []
                            else [ArithRConst{opc=ADD, output=baseReg, source=Word.toInt shortLit}]
                        end
                    |   getOffset _ = raise InternalError "getOffset"
                    val () = uses := !uses+1
                in
                    [
                        ConditionalBranch { test=test, label=destLabel, predict=PredictNeutral },
                        MakeSafe ecx, (* These will now be invalid *)
                        MakeSafe esi,
                        MakeSafe edi,
                        RepeatOperation CMPSB, (* Compare the bytes. *)
                        ArithRR{opc=CMP, output=ecx, source=ecx}, (* Set the Z flag before we start. *)
                        ShiftConstant{shiftType=SRL, output=ecx, shift=0w1} (* Untag the length. *)
                    ] @ getOffset(offset2, edi) @ getOffset(offset1, esi)
                end

                |   generateByteVec _ = raise InternalError "generateByteVec"
                fun generateCode (operands, _) _ =
                    ActionDone{ outReg=NONE, operation=generateByteVec(List.map valOf operands) }
                val testActions =
                    (* The vectors to compare are in esi and edi and the count in ecx.  Since it doesn't
                       matter which is in esi and which in edi it would be nice if we could leave that open
                       but for the moment we require the first in esi and the second in edi.
                       The offsets can be in any register. Edi, esi and ecx are marked as modifiable
                       and are left as values that need to be made safe.  Offset values are
                       read-only and need to be restored in case they're reused. *)
                    loadToSpecificReg(esi, true, 0,
                        loadToSpecificReg(edi, true, 2,
                            loadToSpecificReg(ecx, true, 4,
                                loadToRegOrLiteral(generalRegisters, 1,
                                    loadToRegOrLiteral(generalRegisters, 3, generateCode)))))
                                        ([NONE, NONE, NONE, NONE, NONE], NONE)
            in
                (testActions: nextAction, destLabel)
            end
        in
            fun byteVecEq(args, _)  = SOME(genByteVec JE, args)
            and byteVecNe(args, _)  = SOME(genByteVec JNE, args)
        end

        local
            fun loadBaseOffsets genInstr ops (ActBaseOffset _ :: _) = (* Unary or binary case. *)
                (* Base-offset values need to be loaded into a general register because we
                   don't have a doubly indirect load, at least on X86/32. *)
                    ActionLoadArg{argNo=0, regSet=generalRegisters, willOverwrite=false,
                                  next=loadBaseOffsets genInstr ops}
            |   loadBaseOffsets genInstr ops [_, ActBaseOffset _] =
                    ActionLoadArg{argNo=1, regSet=generalRegisters, willOverwrite=false,
                                  next=loadBaseOffsets genInstr ops}
            |   loadBaseOffsets genInstr (_, outputReg) args =
                let
                    fun argToSource(ActInRegisterSet { readable, modifiable }) =
                        (* Choose the best register.  First choice is a modifiable FP reg,
                           then a non-modifiable FP reg, then a general register. *)
                        if regSetIntersect(modifiable, floatingPtRegisters) <> noRegisters
                        then InRegister(oneOf(regSetIntersect(modifiable, floatingPtRegisters)))
                        else if regSetIntersect(readable, floatingPtRegisters) <> noRegisters
                        then InRegister(oneOf(regSetIntersect(readable, floatingPtRegisters)))
                        else InRegister(oneOf readable)
                    |   argToSource(ActLiteralSource m) = LiteralSource m
                    |   argToSource _ = raise InternalError "negotiateFP: not reg or literal"
                    val ops = map argToSource args
                in
                    ActionDone{ outReg=outputReg, operation=genInstr(ops, outputReg) }
                end

            fun genTest(opc, destLabel as Labels{uses, ...}) ([arg1, arg2], _) =
                let
                    (* We need one argument at the top of the stack.  The second
                       can be anywhere.  The order depends on the operation. *)
                    val (stackArg, secondArg) =
                        case opc of
                            JNA => (arg2, arg1) (* Reverse *)
                        |   JB  => (arg2, arg1) (* Reverse *)
                        |   JA  => (arg1, arg2) (* Forward *)
                        |   JNB => (arg1, arg2) (* Forward *)
                            (* JE/JNE can be in either order.  It might be better to
                               choose an argument based on which is more likely to be at
                               the top of the stack but it's probably not worth it. *)
                        |   _ => (arg1, arg2)
                    val loadToStack =
                        case stackArg of
                            InRegister(reg as FPReg _) => FPLoadFromFPReg{source=reg, lastRef=false}
                        |   InRegister(reg as GenReg _) => FPLoadFromGenReg reg
                        |   LiteralSource lit => loadFPConstant lit
                        |   _ => raise InternalError "testActions"
                    val fpOperation =
                        case secondArg of
                            InRegister(reg as FPReg _) => FPArithR{opc=FCOMP, source=reg}
                        |   InRegister(reg as GenReg _) => FPArithMemory{opc=FCOMP, base=reg, offset=0}
                        |   LiteralSource lit => FPArithConst{opc=FCOMP, source=lit}
                        |   _ => raise InternalError "testActions"

                    val c3 = 0wx4000 and c2 = 0wx0400 and c0 = 0wx0100
                    val mask =
                        case opc of
                            JE => c2 orb c3
                        |   JNE => c2 orb c3
                        |   JB => c0 orb c2 orb c3
                        |   JA => c0 orb c2 orb c3
                        |   JNB => c0 orb c2
                        |   JNA => c0 orb c2
                        |   _ => raise InternalError "testActions"

                    val () = uses := !uses+1
                in
                    [
                        ConditionalBranch { test=if opc = JNE then JNE else JE, label=destLabel, predict=PredictNeutral },
                        MakeSafe eax (* Has an unsafe value in it. *)
                    ] @
                    (* We use these operations to set the condition codes.
                       Do we need to put something in to indicate to an optimiser
                       that we're using the condition code "result" here? *)
                    (if opc = JE orelse opc = JNE
                    then [ArithRConst{ opc=XOR, output=eax, source=Word.toInt c3 }]
                    else []) @
                    [
                        (* Mask and set the condition codes. *)
                        ArithRConst{ opc=AND, output=eax, source=Word.toInt mask },
                        FPStatusToEAX,
                        fpOperation, (* Compare and pop. *)
                        loadToStack (* First arg to stack*)
                    ]
                end
            | genTest _ _ = raise InternalError "genTest"

            fun fpTests opc () =
            let
                val destLabel = mkLabel()
            in
                (* We need eax as a work register. *)
                (fn _ => ActionGetWorkReg{regSet=singleton eax,
                            setReg = fn _ => loadBaseOffsets (genTest(opc, destLabel)) ([NONE, NONE], NONE)}, destLabel)
            end
            
            fun getFP genInstr pref =
                getResultRegister(floatingPtRegisters, pref, loadBaseOffsets genInstr)
                    ([NONE, NONE], NONE)
            and getUnaryFP genInstr pref =
                getResultRegister(floatingPtRegisters, pref, loadBaseOffsets genInstr)
                    ([NONE], NONE)
            and getArctanFP genInstr pref =
                (* Arctan requires two items on the stack.  The effect is to overwrite fp6 *)
                getWorkingRegister(singleton fp6,
                        fn _ =>
                            getResultRegister(floatingPtRegisters, pref, loadBaseOffsets genInstr)
                            ) ([NONE], NONE)


            val unimplementedInstr = fn _ => NONE
            
            fun genBinaryFP(forward, reverse) ([op1, op2], SOME output) =
                let
                    val (stackArg, secondArg, opc) =
                        case (op1, op2) of
                            (InRegister(FPReg fp1), InRegister(FPReg fp2)) =>
                                (* Choose the reg nearer the top. *)
                                if fp1 <= fp2 then (op1, op2, forward) else (op2, op1, reverse)
                        |   (_, InRegister(FPReg _)) => (op2, op1, reverse)
                        |   _ => (op1, op2, forward)
                    val loadToStack =
                        case stackArg of
                            InRegister(reg as FPReg _) => FPLoadFromFPReg{source=reg, lastRef=false}
                        |   InRegister(reg as GenReg _) => FPLoadFromGenReg reg
                        |   LiteralSource lit => loadFPConstant lit
                        |   _ => raise InternalError "genBinaryFP"
                    val fpOperation =
                        case secondArg of
                            InRegister(reg as FPReg _) => FPArithR{opc=opc, source=reg}
                        |   InRegister(reg as GenReg _) => FPArithMemory{opc=opc, base=reg, offset=0}
                        |   LiteralSource lit => FPArithConst{opc=opc, source=lit}
                        |   _ => raise InternalError "genBinaryFP"
                in
                    [
                        FPStoreToFPReg{output=output, andPop=true},
                        fpOperation, (* Do operation. *)
                        loadToStack (* First arg to stack*)
                    ]
                end
            |   genBinaryFP _ _ = raise InternalError "genBinaryFP"

            fun genUnaryFP opc ([opn], SOME output) =
                let
                    val loadToStack =
                        case opn of
                            InRegister(reg as FPReg _) => FPLoadFromFPReg{source=reg, lastRef=false}
                        |   InRegister(reg as GenReg _) => FPLoadFromGenReg reg
                        |   LiteralSource lit => loadFPConstant lit
                        |   _ => raise InternalError "genUnaryFP"
                in
                    case opc of
                        FPATAN =>
                            (* The FPATAN function takes two arguments so we need to push 1.0.
                               We must invalidate FP7 before we push the second argument otherwise
                               we may get a stack overflow. *)
                            [FPStoreToFPReg{output=output, andPop=true},
                            FPUnary FPATAN,
                            (* Because we have pushed an extra value this appears as
                               modifying fp6. *)
                            FreeRegisters(singleton fp6),
                            FPUnary FLD1,
                            FPFree fp7,
                            loadToStack
                            ]
                    |   _ => (* The rest are much simpler. *)
                        [FPStoreToFPReg{output=output, andPop=true},
                         FPUnary opc,
                         loadToStack]
                end
            |   genUnaryFP _ _ = raise InternalError "genUnaryFP"
        in
            fun instrAddFP(args, _) = SOME(getFP(genBinaryFP(FADD, FADD)), args)
            and instrSubFP(args, _) = SOME(getFP(genBinaryFP(FSUB, FSUBR)), args)
            and instrMulFP(args, _) = SOME(getFP(genBinaryFP(FMUL, FMUL)), args)
            and instrDivFP(args, _) = SOME(getFP(genBinaryFP(FDIV, FDIVR)), args)

            fun instrNegFP(args, _) = SOME(getUnaryFP(genUnaryFP FCHS), args)
            and instrSqrtFP(args, _)= SOME(getUnaryFP(genUnaryFP FSQRT), args)
            and instrSinFP(args, _) = SOME(getUnaryFP(genUnaryFP FSIN), args)
            and instrCosFP(args, _) = SOME(getUnaryFP(genUnaryFP FCOS), args)
            and instrAtanFP(args, _)= SOME(getArctanFP(genUnaryFP FPATAN), args)

            local
                fun genIntToFPInstr([InRegister source], SOME output) =
                    let
                         (* This is a bit complicated because we can only load from memory
                            but the memory needs to be modifiable. We have a special location
                            we can load from. *)
                        val (test, skipEmulator) = condBranch(JNE, PredictTaken)
                    in
                        [
                            FPStoreToFPReg{output=output, andPop=true}, (* Store into dest. *)
                            FPLoadIntAndPop (* Do instr. *)
                        ] @ forwardJumpLabel skipEmulator @
                        [ CallRTS memRegArbEmulation ] @
                        test @ [TestTagR source, PushR source (* Push and test tag. *) ]
                    end
                |   genIntToFPInstr _ = raise InternalError "genIntToFPInstr"
                fun genIntToFP pref =
                    (* Real.fromInt generates its result in a FP reg but takes its arg in a general reg. *)
                    getResultRegister(floatingPtRegisters, pref,
                        loadToOneOf(generalRegisters, true (* Will modify *), 0, generateInstruction genIntToFPInstr)
                        ) ([NONE], NONE)
            in
                fun instrIntToRealFP(args, _) = SOME(genIntToFP, args)
            end

            val instrExpFP = unimplementedInstr
            and instrLnFP = unimplementedInstr
            and instrRealToIntFP = unimplementedInstr

            fun testNeqFP(args, _)= SOME(fpTests JNE, args)
            fun testEqFP(args, _) = SOME(fpTests JE, args)
            fun testGeqFP(args, _)= SOME(fpTests JNB, args)
            fun testGtFP(args, _) = SOME(fpTests JA, args)
            fun testLeqFP(args, _)= SOME(fpTests JNA, args)
            fun testLtFP(args, _) = SOME(fpTests JB, args)
        end
    end

    fun isPushI _ = true

    (* The stack limit register is set at least twice this far from the
       end of the stack so we can simply compare the stack pointer with
       the stack limit register if we need less than this much. Setting
       it at twice this value means that procedures which use up to this
       much stack and do not call any other procedures do not need to
       check the stack at all. *)
    val minStackCheck = 20
  
    (* Adds the constants onto the code, and copies the code into a new segment *)
    fun copyCode (cvec, operations, stackRequired, registerSet, callsAProc) : address =
    let
        (* Prelude consists of stack checking code.  N.B.  This is added on
           after the main list is reversed so the code is not reversed. *)
        val preludeCode =
        let
            fun testRegAndTrap(reg, entryPt) =
            let
                (* Normally we won't have a stack overflow so we will skip the check. *)
                val (skipCheck, skipCheckLab) = condBranch(JNB, PredictTaken)
            in
                [ArithRMem{ opc=CMP, output=reg, offset=memRegStackLimit, base=ebp }] @
                skipCheck @ [CallRTS entryPt] @ forwardJumpLabel skipCheckLab
            end
        in
            if stackRequired >= minStackCheck
            then
            let
                (* Compute the necessary amount in edi and compare that. *)
                val stackByteAdjust = ~wordSize * stackRequired
                val testEdiCode =
                    testRegAndTrap (edi, memRegStackOverflowCallEx)
            in
                 [LoadAddress{output=edi, base=SOME esp, index=NoIndex, offset=stackByteAdjust}] @ testEdiCode
            end
         
            else if callsAProc (* check for interrupt *)
            then testRegAndTrap (esp, memRegStackOverflowCall)
               
            else [] (* no stack check required *)
        end

        (* The code is in reverse order.  Reverse it and put the prelude first. *)
    in
        createCodeSegment(X86OPTIMISE.optimise(cvec, preludeCode @ List.rev operations), registerSet, cvec)
    end

    structure Sharing =
    struct
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
        and  forwardLabel   = forwardLabel
        and  backwardLabel  = backwardLabel
    end

end (* struct *) (* CODECONS *);
