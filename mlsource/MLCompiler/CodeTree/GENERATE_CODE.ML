(*
    Copyright David C. J. Matthews 1991, 2009-10

    Title:      General purpose code generator.
    Author:     Dave Matthews, Edinburgh University / Prolingua Ltd.
    Copyright   D.C.J. Matthews 1991

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

functor GENERATE_CODE (
    structure CODECONS : CODECONSSIG
    structure CODEGEN_TABLE : CODEGEN_TABLESIG where type machineWord = Address.machineWord
    structure BASECODETREE: BaseCodeTreeSig

    sharing CODECONS.Sharing = CODEGEN_TABLE.Sharing = BASECODETREE.Sharing
) :  
  
(*****************************************************************************)
(*                  GCODE export signature                                   *)
(*****************************************************************************)
sig
  type codetree
  type machineWord
  val gencode: codetree * Universal.universal list * int -> unit -> machineWord;
end =

(*****************************************************************************)
(*                  GCODE functor body                                       *)
(*****************************************************************************)
struct
    open CODECONS;
    open CODEGEN_TABLE;
    open Address;
    open Misc; (* after address, so we get Misc.length, not Address.length *)
    open RuntimeCalls; (* for POLY_SYS numbers *)
    open BASECODETREE;
    
    open RegSet

    val F_mutable_words = Word8.orb (F_mutable, F_words);

    val objLength = Address.length;

    infix 7 regEq regNeq;

(*************************** end of copied code *****************************)  
   
    (* gets a value from the run-time system; 
       usually this is a closure, but sometimes it's an int.  *)
    val ioOp : int -> machineWord = RunCall.run_call1 POLY_SYS_io_operation;

    (* minor HACKS *)
    fun forLoop f i n = if i > n then () else (f i; forLoop f (i + 1) n);
   
    val word0 = toMachineWord 0;
    val word1 = toMachineWord 1;

    val DummyValue : machineWord = word0; (* used as result of "raise e" etc. *)
    val False : machineWord = word0;     (* false *)
    val True  : machineWord = word1;     (* true *)
    val Zero  : machineWord = word0;     (* 0 *)

    val constntTrue  = Constnt True;
    val constntFalse = Constnt False;

    fun isNoResult NoResult     = true | isNoResult _ = false;

    (* Are we at the end of the procedure. *)
    datatype tail = EndOfProc of reg | NotEnd

    fun isEndOfProc (EndOfProc _) = true | isEndOfProc _ = false;

    fun chooseMergeRegister(_, EndOfProc res) = UseReg(singleton res)
    |   chooseMergeRegister(NoHint, _) = UseReg generalRegisters
    |   chooseMergeRegister(whereto, _) = whereto

    fun codeToCgType GeneralType = ArgGeneral | codeToCgType FloatingPtType = ArgFP

    (* Code generate a procedure or global declaration *)
    fun codegen
       (pt               : codetree,
        declOnPrevLevel  : int * (unit -> stackIndex * operations) * ttab -> stackIndex * operations,
        isStaticLink     : int -> bool,
        loadStaticLink   : int * (unit -> stackIndex * operations) * ttab -> stackIndex * stackIndex * operations,
        staticLinkRegSet : int -> regSet,
        closureLifetime  : int,
        argTypes         : argumentType list,
        argLifetimes     : int list,
        resultType       : argumentType,
        localCount       : int,
        debugSwitches    : Universal.universal list) : operations * int * regSet * bool =
    let
        val numOfArgs = List.length argTypes
        val cvec: operations ref = ref []
        val callsAFunction = ref false
        fun codeGenerate(ops: operations, cvec) = cvec := ops @ ! cvec

        fun matchFailed _ = raise InternalError "codegen: unhandled pattern-match failure"

        (* make the translation table *)
        val transtable = ttabCreate(localCount, debugSwitches)
        (* Map from declaration location to pstack entry. *)
        val decToPstack  = Array.array (localCount, noIndex)
        (* Indicates if a declaration location is a static-link function and contains
           a function to free the static link entries if it is. *)
        val staticLinkEntries: (unit->operation list) option array = Array.array (localCount, NONE)

        fun localDeclaration(index, locn, lifeTime, slRelease) =
        (
            Array.update (staticLinkEntries, locn, slRelease);
            Array.update (decToPstack, locn, index);
            (* If the lifetime is zero remove the item. *)
            if lifeTime = 0
            then incrUseCount (transtable, index, ~1)
            else (setLifetime(transtable, index, lifeTime); [])
        )

        (* Header code for procedure. *)

        (* Push the return address - may have multiple references because
           we may exit at any of the "tails". *)
        val returnAddress =
            case regReturn of
                NONE =>
                let
                    (* The return address has already been pushed onto the stack,
                       probably because the normal call sequence does it. *)
                    val addr = incsp transtable
                in
                    addr
                end
            |   SOME regReturn => 
                let
                    (* The return address has already been pushed onto the stack,
                       probably because the normal call sequence does it. *)
                    val addr = parameterInRegister(regReturn, 1, transtable)
                    val () = codeGenerate(activeRegister regReturn, cvec)
                in
                    addr
                end

        (* If discardClosure is true, all uses of the closure are
           directly-recursive calls which will be handled as "Recursive".
           This doesn't require the function closure as a parameter.
           SPF 22/5/95 
       
           Unfortunately, this is not quite true - we can still embed
           the function in a datatype, so we still require access to
           the closure. However, this is handled by storing the closure
           in the constants section (it *is* a constant) if we have
           any such uses of it.
           SPF 30/5/95 
       
           Note that it's important for correctness that we load "embedded"
           uses of an empty closure from the constants section. If we
           tried to be clever and use the value that we find in closureReg
           at function entry, we would generate bad code. That's because 
           functions with empty closures may get called using the PureCode
           calling convention, which doesn't actually initialise closureReg.
       
           Note also that it's the *calls* to codegen that have to be right,
           since the function that loads the closure is actually a parameter
           to codegen.
           SPF 2/1/97
        *)
        val closureOrSlAddr = parameterInRegister(regClosure, closureLifetime, transtable)
        val () = codeGenerate(activeRegister regClosure, cvec)


        (* Find out which arguments are in which registers. *)
        val argLocations = argRegs (List.map codeToCgType argTypes)
        val numberOfArgsOnStack = List.length(List.filter(not o isSome) argLocations)
        (* Create a vector the actual argument locations.  Those in registers are marked as entries
           on the pstack.  The values may be pushed to the real stack or moved to other registers
           but this will keep track of them.  Those on the stack are represented by negative values. *)
        datatype argLocation = ArgInReg of stackIndex | ArgOnStack of int
        local
            fun mapArgs ([], []) = ([], 0)
            |   mapArgs(SOME reg :: l, life:: lives) =
                    let
                        val (l', n) = mapArgs(l, lives)
                        val () = codeGenerate(activeRegister reg, cvec)
                    in
                        (ArgInReg(parameterInRegister (reg, life, transtable)) :: l', n)
                    end
            |   mapArgs(NONE :: l, _::lives) = let val (l', n) = mapArgs(l, lives) in (ArgOnStack(n-1) :: l', n-1) end
            |   mapArgs _ = raise InternalError "Mismatched argument types/lifetimes"
            val (args, _) = mapArgs(argLocations, argLifetimes)
        in
            val argRegTab = Vector.fromList args
        end
    
        fun exit () =
        let
            val stackArgs = List.length(List.filter(not o isSome) argLocations)
            val exitCode =
                case regReturn of
                    NONE => (* Reset to just above the return address. *)
                            returnFromFunction{returnReg=NONE, argsToRemove=stackArgs} @
                            resetStack (realstackptr transtable - 1)
                |   SOME _ =>
                    let
                        val (returnReg, returnOffset, returnCode) =
                            loadEntryToSet (transtable, returnAddress, RegSet.generalRegisters, false);
                        val freeCode = removeStackEntry (transtable, returnOffset)
                    in
                        returnFromFunction{returnReg=SOME returnReg, argsToRemove=stackArgs} @
                        resetStack (realstackptr transtable) @ freeCode @
                        returnCode
                    end;
        in
            exiting transtable;
            exitCode
        end

        (* Allocate a segment of the required size. *)
        fun callgetvec (csize, flag : Word8.word, whereto) : stackIndex * operations =
        let
            (* Get a register for the result. *)
            val (resultReg, regCode) =
                getRegisterInSet(transtable, case whereto of UseReg rr => rr | _ => generalRegisters)
        
            val resAddr = pushReg (transtable, resultReg);
        in
            (resAddr, allocStore {size=csize, flags=flag, output=resultReg} @ regCode)
        end;

        (*infix 9 sub;*)

        (* Loads a local, argument or closure value; translating local
           stack addresses to real stack offsets.
           N.B. In the case of non-local variables lastRef is true only for
           the last non-local variable, not the last use of this particular
           variable. *)
        fun locaddr ({ addr, fpRel, lastRef, ...}) =
        if fpRel 
        then if addr < 0 (* Args. *)
        then
        ( (* The arguments are numbered from -n upto -1.  The first few arguments are
               in registers and the rest on the stack. *)
            case Vector.sub(argRegTab, numOfArgs + addr) of
                ArgInReg regEntry =>
                (
                    (* If this is NOT the last reference we need to increment the
                       use count on the entry. *)
                    if lastRef then () else (incrUseCount(transtable, regEntry, 1); ());
                    (regEntry, [])
                )
            |   ArgOnStack actualAddr => (pushStack (transtable, actualAddr), [])
        )
        (* positive address - reference to entry on the pstack. *)
        else
        let
            val resIndex = Array.sub(decToPstack, addr)
            val freeCode =
                if lastRef
                then
                    (* Last reference.  When we've finished with this entry it will be discarded.
                       If this is a statically linked function we discard the closure entries.
                       This case will only occur in a "kill" entry.  The only real references
                       to statically linked functions is to call them and that case is handled
                       in genEval. *)
                (
                    case Array.sub(staticLinkEntries, addr) of
                        NONE => []
                    |   SOME discard => discard()
                )
                else (* There's at least one more reference after this. *)
                    incrUseCount(transtable, resIndex, 1)
        in
            (resIndex, freeCode)
        end

        else  (* cp relative *)
        let
            (* If this is the last reference to the closure we want
               it to be removed afterwards.  makeSl is not always called
               if, for example, the value is constant.  To ensure the
               use-count is correct we increment it if it is used and
               then decrement it afterwards.  DCJM 2/12/99. *)
            val (dec, code) =
                declOnPrevLevel(addr,
                    fn () => (incrUseCount(transtable, closureOrSlAddr, 1); (closureOrSlAddr, [])),
                    transtable)
            val freeCode =
                if lastRef andalso closureLifetime <> 0 
                then incrUseCount(transtable, closureOrSlAddr, ~1) else []
        in
            (dec, freeCode @ code)
        end
         (* locaddr *);
    
        (* For each load of a local in the tree it calls the `add' procedure. *)
        fun identifyLoads expList add =
        let 
            (* Need to identify declarations within the current block.  This was originally
               there because declaration addresses could at one time be reused.  That shouldn't
               happen now. *)
            val newDecs : bool StretchArray.stretchArray =
               StretchArray.stretchArray (4, false)
           
            fun loads pt =
            case pt of
                MatchFail => ()
            
            |   AltMatch (exp1, exp2) => (loads exp1; loads exp2 )
          
            |   Extract {fpRel = true, addr = locn, lastRef, ...} =>
                if locn < 0 (* args - only look at those in the registers. *)
                then
                (
                    case Vector.sub(argRegTab, numOfArgs + locn) of
                        ArgInReg regEntry => if lastRef then add regEntry else ()
                    |   _ => ()
                )
                else if not (StretchArray.sub (newDecs,locn)) andalso lastRef
                    (* Ignore new declarations. *)
                then add (Array.sub(decToPstack, locn))
                else ()

               (* If discardClosure is true, then we've already zeroed the
                  use-count for closureOrSlAddr, so don't adjust it now.
                  SPF 22/5/95 *)
            |   Extract {fpRel = false, addr = _, level = _, lastRef, ...} =>
                if closureLifetime <> 0 (* Non-local *) andalso lastRef
                then add closureOrSlAddr (* Reference to the closure. *)
                else ()
          
            |   Eval {function, argList, ...} =>
                (
                    loads function;
                    List.app (fn (l, _) => loads l) argList
                )
            
            |   Declar {addr, value, ...} =>
                (
                    (* Indicate that this is a new declaration. *)
                    StretchArray.update (newDecs, addr, true);
                    loads value (* Check the expression. *)
                )
            
            |   Indirect {base, ...} => loads base
            
            |   Newenv vl => List.app loads vl
            
            |   Recconstr vl => List.app loads vl

            |   BeginLoop{loop, arguments, ...} => (List.app (fn (l, _) => loads l) arguments; loads loop)

            |   Loop argList => List.app (fn (l, _) => loads l) argList

            |   Handle{exp, handler} => (loads exp; loads handler)

            |   MutualDecs decs =>
                (
                    (* First process the declarations to ensure that new declarations
                       are marked as such then process the values being declared. *)
                    List.app(
                        fn Declar{addr, ...} => StretchArray.update (newDecs, addr, true)
                        |   _ => raise InternalError "MutualDecs: not Declar") decs;
                    List.app loads decs
                )

            |   _ => ()
        in
            List.app loads expList
        end
      
    (* code-generates code from the tree *)
    (* SPF 2/5/95 - primBoolOps added to prevent loop when
       trying to inline unsupported boolean primitives. We might
       get the calling sequence:
       
         genEval -> genCond -> genTest -> genOtherTests -> gencde -> genEval
         
       where both versions of genEval are for the same (unsupported)
       boolean comparison. If this occurs, the second call will have
       primBoolOps set to false, and will generate a call to the RTS.
       
       Note that "whereto" is only a HINT. There is no guarantee that specifying
       "UseReg r" will actually get the value loaded into that register. For example,
       the code that handles constants completely ignores this hint.
       SPF 15/8/96
     *)
    fun gencde (pt, primBoolOps, whereto, tailKind, matchFailFn, loopAddr) : mergeResult =
    let 
      val needsResult : bool = not (isNoResult whereto)
      
      val result : mergeResult = 
        case pt of
          MatchFail => (* A bit like Raise *)
          let
                val matchFailCode = matchFailFn ()
                val () = codeGenerate(matchFailCode, cvec)
          in
                if needsResult
                then MergeIndex(pushConst (transtable, DummyValue))
                else NoMerge (* Unused. *)
          end
        
        | AltMatch (exp1, exp2) => (* A bit like Cond *)
          let
            val mark1 : stackMark = markStack transtable;
            val mark2 : stackMark = markStack transtable;
              
            val failLabs = ref ([] : labels list);
            fun newMatchFailFn () = 
            let
                val (thisFailure, code) = unconditionalBranch (NoMerge, transtable)
            in
                failLabs := thisFailure :: !failLabs;
                code
            end;
            
            val whereto = chooseMergeRegister(whereto, tailKind)

            val exp1Result = 
               genToRegister (exp1, whereto, tailKind, newMatchFailFn, loopAddr);
              
            (* Optimisation: return immediately, if possible, rather than
               jumping and then returning. This may turn the following
               unconditional branch into dead code, in which case it
               will be removed by the lower-level code generator.
               SPF 25/11/96
             *)
            val () =
              if (isEndOfProc tailKind) andalso not (haveExited transtable)
              then codeGenerate(exit(), cvec)
              else ();

            (* If exp1 succeeded, we skip exp2 *)
            val (suceedLab, code) = unconditionalBranch (exp1Result, transtable);
            (* If exp1 failed, we come here (with NO result). *)
            val (_, mergeCode) = mergeList (!failLabs, transtable, NoMerge, mark2)
            val () = codeGenerate(mergeCode @ code, cvec)
            
            (* Compile exp2 using the OLD matchFailFn *)
            val exp2result = 
               genToRegister (exp2, whereto, tailKind, matchFailFn, loopAddr);
            (* If exp1 succeeded, we merge back in here. *)
            val (mergeRes, mergeCode2) = merge (suceedLab, transtable, exp2result, mark1)
            val () = codeGenerate(mergeCode2, cvec)
          in
            mergeRes
          end
            
        | Eval {function, argList, resultType, ...} =>
            genEval (function, argList, resultType, primBoolOps, whereto, tailKind, matchFailFn)
          
        | Declar {addr, value, references} =>
            (
                case value of
                    Lambda lam =>
                    let
                        fun nextMutual(dec, slRelease) =
                            codeGenerate(localDeclaration (dec, addr, references, slRelease), cvec)
                        val _ = genProc (lam, nextMutual, true, whereto, matchFailFn)
                    in
                        ()
                    end
                |   _ =>
                    let
                        val decl = genToStack (value, matchFailFn)
                    in
                        codeGenerate(localDeclaration (decl, addr, references, NONE), cvec)
                    end;
                NoMerge
            )

        | Extract ext =>
            let
                val (loc, locCode) = locaddr ext
                val () = codeGenerate(locCode, cvec)
            in
                if needsResult
                then MergeIndex loc
                else (* If the result is not required discard it.  This is used
                        to remove variables which are not used on this path. *)
                (
                    codeGenerate(removeStackEntry(transtable, loc), cvec);
                    NoMerge
                )
            end

        | Indirect {base, offset} =>
            let
                val baseCode = genToStack (base, matchFailFn)
                val (index, indCode) = indirect (offset, baseCode, transtable)
                val () = codeGenerate(indCode, cvec)
            in  (* Get the value to be indirected on. *)
                MergeIndex index
            end

        | Lambda lam => MergeIndex(genProc (lam, fn _ => (), true, whereto, matchFailFn))

        | Constnt w => MergeIndex(pushConst (transtable, w))

        | Cond (testPart, thenPart, elsePart) =>
            genCond (testPart, thenPart, elsePart, whereto, tailKind, matchFailFn, loopAddr)

        | Newenv vl =>
            let (* Processes a list of entries. *)
                val startMark = markStack transtable
                (* We may have the situation where we want the result in a specific register
                   but we actually have a Decl entry followed by an Extract.
                   Don't do this unless we've asked for a specific register. *)
                val destAddr =
                    if null vl then 0 (* 0 can't occur as a valid address *)
                    else case (List.last vl, whereto) of
                        (Extract{addr, level=0, fpRel=true, ...}, UseReg _) => addr
                    |   _ => 0

                fun codeList ([], _) =
                (* Either the list is empty or the previous entry was a 
                   declaration. Generate a value to represent void.empty so
                   that there is something on the stack. *)
                    if needsResult
                    then MergeIndex(pushConst (transtable, DummyValue))
                    else NoMerge (* Ignored *)
                
                | codeList (Declar{addr, value, references} :: valus, whereto) =
                  (* Declaration. *)
                    let
                        val dest = if addr = destAddr then whereto else NoHint
                    in
                        case value of
                            Lambda lam =>
                            let
                                fun nextMutual(dec, slRelease) =
                                    codeGenerate(localDeclaration (dec, addr, references, slRelease), cvec)
                                val _ = genProc (lam, nextMutual, true, dest, matchFailFn)
                            in
                                ()
                            end
                        |   _ =>
                            let
                                val res = gencde (value, true, dest, NotEnd, matchFailFn, NONE)
                                val decl =
                                    case res of
                                        MergeIndex index => index
                                    |   NoMerge => raise InternalError "genToStack: no result"
                            in
                                codeGenerate(localDeclaration (decl, addr, references, NONE), cvec)
                            end;
                        codeList(valus, whereto)
                    end

                |   codeList ([valu], whereto) =
                        (* Last entry is an expression. *)
                        gencde (valu, true, whereto, tailKind, matchFailFn, loopAddr)
                
                |   codeList (valu :: valus, whereto) =
                    (* Expression in a sequence. *)
                    (
                        gencde (valu, true, NoResult, NotEnd, matchFailFn, loopAddr);
                        codeList(valus, whereto)
                    )
                val resultPosn = codeList(vl, whereto)
                val () = checkBlockResult(transtable, resultPosn)
                val () = unmarkStack(transtable, startMark)
            in
                resultPosn
            end

        | BeginLoop{loop=body, arguments=args} =>
          let
            (* Execute the body which will contain at least one Loop instruction.
               There will also be path(s) which don't contain Loops and these
               will drop through. *)
            (* Load the arguments.  We put them into registers at this stage
               to ensure that constants and "direct" entries are loaded.  They
               may go onto the stack, which is fine. It could be worth doing
               this in two passes, the first simply evaluating the arguments
               onto the pstack, the second loading them into registers since
               that would generate better code when some arguments are constants
               but others are expressions that push those constants onto the stack. *)
            fun genLoopArg (Declar {addr, value, references}, argType) =
            let
                (* This is almost the same as a normal declaration except
                   that we have to make sure that we use a new location, stack or
                   register, since we're going to be changing the contents of
                   this location.  The easiest way to do that is to load it into
                   a register.  We could do better if we are loading the last
                   reference to the initial value in which case we could reuse
                   its location. *)
                val index = genToStack(value, matchFailFn)
                (* Put this in a floating point register if it is a floating point value
                   otherwise a fixed point register. *)
                val prefSet =
                    case argType of
                        GeneralType => RegSet.generalRegisters
                    |   FloatingPtType => RegSet.floatingPtRegisters
                val (_, decl, decCode) = loadEntryToSet(transtable, index, prefSet, true)
                val () = codeGenerate(decCode, cvec)
                (* It should not be a static-link function - just check. *) 
                val _ = 
                  case value of
                    Lambda {makeClosure = false, ...} => 
                        raise InternalError "LoopArg: static link function"
                  | _ => ()
            in
                localDeclaration (decl, addr, references, NONE);
                (* Normally "references" will be non-zero but it does seem that we
                   can get loop variables that are never used.  This may happen as a
                   result of multiple levels of inline function expansion.  If it's zero
                   we won't have a location for the loop argument. *)
                if references = 0 then noIndex else decl
            end
            |   genLoopArg _ = raise InternalError "genLoopArg: not a declaration"
            val argIndexList = map genLoopArg args;
            (* We need to ensure that the state we return to after the loop is the same
               as it was at the start.  If we find inside the loop that we need to spill
               values from registers that were declared outside we need to move those
               spills to before the loop.  We first process the loop optimistically and
               then reprocess it we find we've had to spill. *)
            fun reprocessLoop n =
            let
                (* Include a check that we don't loop too many times. *)
                val _ = n > 20 andalso raise InternalError "reprocessLoop"
                (* Record the code at the start.  If we have to reprocess we discard everything
                   after this. *)
                val codeAtStart = !cvec
                val initialState = saveState transtable
                (* Now we have loaded the registers we can find out the destinations
                   i.e. the register or stack location they were in at the start of
                   the loop.  We have to do this after we've loaded all the arguments
                   because we may have pushed some onto the stack as we loaded the
                   later ones.  That's fine so long as when we loop we put the new
                   values in the same place.  *)
                val argDestList = getLoopDestinations(argIndexList, transtable)
                (* Start of loop.  This is where we jump to if the loop is taken. *)
                val (startLoopCode, startLoop) = backJumpLabel()
                val () = codeGenerate(startLoopCode, cvec)
                val startSp = realstackptr transtable
                val cacheSet = ref noRegisters and pushList = ref []

                fun onLoop () =
                (* This function is called whenever we loop.  The state here is the
                   state at the point we take the loop.  We need to record the state
                   at each of those points to produce a composite.  *)
                let
                    val (caches, pushes) = compareLoopStates(transtable, initialState, argIndexList)
                    val () = cacheSet := regSetUnion(caches, !cacheSet)
                    and () = pushList := pushes @ !pushList
                in
                    (* We have to make sure that the real stack pointer is consistent.
                       We may have pushed local values within the loop and these need
                       to be removed. *)
                    codeGenerate(resetStack (realstackptr transtable - startSp), cvec)
                end
                (* Compile the loop with the jumps back to the start. *)
                val runLoop =
                    gencde (body, true, whereto, tailKind, matchFailFn,
                        SOME(startLoop, onLoop, argDestList))
                (* The state we have here is the state when we haven't taken the loop. *)
            in
                if ! cacheSet = noRegisters andalso null (! pushList) then runLoop
                else
                (
                    cvec := codeAtStart;
                    codeGenerate(restoreLoopState(transtable, initialState, ! cacheSet, ! pushList), cvec);
                    reprocessLoop(n+1)
                )
            end
          in
            reprocessLoop 0
          end

        | Loop argList =>
            let
                val (startLoop, onLoop, argDestList) =
                    case loopAddr of
                        SOME l => l
                    |   NONE =>
                        raise InternalError "No BeginLoop for Loop instr"
                (* Evaluate the arguments.  Try to put them in the destination
                   register if we can.  It doesn't matter at this stage too much. *)
                fun evalArg((arg, _), dest) =
                let
                    val whereto =
                      case dest of
                            ArgToRegister reg => UseReg (singleton reg)
                        |   ArgToStack _ => NoHint
                        |   ArgDiscard => NoHint
                    val res = gencde (arg, true, whereto, NotEnd, matchFailFn, NONE)
                in
                    case res of
                        MergeIndex index => index
                    |   NoMerge => raise InternalError "evalArg: no result"
                end
                    
                val argsOnPstack : stackIndex list =
                    ListPair.map evalArg (argList, argDestList)

                fun moveArgs([], []) = []
                |   moveArgs(arg :: args, ArgToRegister reg :: dests) =
                    let
                        (* Do it in reverse order so that we can delay locking
                           the register arguments. *)
                        val argEntries = moveArgs(args, dests)
                        val (argEntry, argCode) =
                            loadToSpecificReg (transtable, reg, arg, false)
                        val () = codeGenerate(argCode, cvec)
                    in
                        lockRegister(transtable, reg);
                        argEntry :: argEntries
                    end
                |   moveArgs(arg :: args, ArgToStack offset :: dests) =
                    let
                        val (argEntry, code) = storeInStack(transtable, arg, offset)
                    in
                        codeGenerate(code, cvec);
                        argEntry :: moveArgs(args, dests)
                    end
                |   moveArgs(arg :: args, ArgDiscard :: dests) =
                        (* If we're just discarding it return the location so we will
                           remove it from the stack. *)
                        arg :: moveArgs(args, dests)
                |   moveArgs _ =
                        raise InternalError "moveArgs: Mismatched arguments"

                (* the arguments are now all in their rightful places. *)
                val argEntries = moveArgs(argsOnPstack, argDestList);
            in
                (* Remove the entries and unlock the registers.  It may
                   be unnecessary to remove the entries because we're about
                   to fix up a jump but there's no harm in it. *)
                List.app (
                    fn (ArgToRegister reg) => codeGenerate(unlockRegister(transtable, reg), cvec)
                      | _ => ()) argDestList;
                List.app (fn index => codeGenerate(removeStackEntry(transtable, index), cvec))
                    argEntries;
                onLoop();
            
                (* Repeat. *)
                codeGenerate(jumpBack (startLoop, transtable), cvec);
                (* Put on a dummy result. *)
                if needsResult
                then MergeIndex(pushConst (transtable, DummyValue))
                else NoMerge (* Unused. *)
            end

        | Raise exp =>
          let (* movl <exception>,resultReg; jmp raisex *)
            val () =
               (* Ensure the return address is on the stack in case
                  we are tracing exceptions. *)
               codeGenerate(pushSpecificEntry (transtable, returnAddress), cvec);
               
            val excVal = genToStack (exp, matchFailFn);
            
            val (resultIndex, resultCode) = 
               loadToSpecificReg (transtable, resultReg ArgGeneral, excVal, true);

          in
            codeGenerate(raiseException @ resultCode, cvec);
            codeGenerate(removeStackEntry(transtable, resultIndex), cvec);
            exiting transtable; (* Nothing further *)

            (* Put a dummy value on the stack so that subsequent merge code works
               It really ought to ignore this since we've exited. *)
            if needsResult
            then MergeIndex(pushConst (transtable, DummyValue))
            else NoMerge (* Unused. *)
          end

        | Handle {exp, handler} =>
            let
                (* Push all regs - we don't know what the state will be when 
                   we reach the handler. *)
                (* i.e. Push all registers except those whose last use occurs in the expression
                   we're handling. *) 
                val () = 
                    codeGenerate(pushAllBut (transtable, identifyLoads[exp], allRegisters), cvec);
                (* It's not clear what registers will be modified as a result of raising
                 and handling an exception.  Many functions may result in exceptions
                 being raised and rather than add the registers to the register set of
                 those functions it's probably better to include them in the modification
                 set here. DCJM 26/11/00. *)
                val _ = addModifiedRegSet(transtable, allRegisters);

                (* This is the real stack state at the start of the handler *)
                val startOfHandler = realstackptr transtable;
          
                (* Remember this pseudo-stack position for later merge *)
                val mark = markStack transtable

                (* Save old handler - push regHandler *)
                val () = codeGenerate(pushCurrentHandler, cvec)
                val oldIndex = incsp transtable
          
                (* Now it's on the real stack we can remove it from the pstack. *)
                local
                    (* Push address of new handler. *)
                    val rsp         = realstackptr transtable
                    val (handlerEntry, handlerLab, handlerCode)  = pushAddress (transtable, rsp + 1)
                    val () = codeGenerate(handlerCode, cvec)
    
                    (* Set the current handler to the stack pointer after these items. *)
                    val () = codeGenerate(storeToHandler regStackPtr, cvec)
                in
                    val handlerLab = handlerLab
                    and handlerEntry = handlerEntry
                end

                val whereto = chooseMergeRegister(whereto, tailKind)
 
                (* Code generate body, putting the result in result register. *)
                (* "NotEnd" because we have to come back to remove the handler. *)
                val bodyResult = genToRegister (exp, whereto, NotEnd, matchFailFn, loopAddr);
                (* Reload the old value of regHandler i.e. remove handler. *)
                (* Remove the handler entries. *)
                val () = codeGenerate(removeStackEntry(transtable, handlerEntry), cvec)
                val () = codeGenerate(reloadHandler(transtable, oldIndex), cvec)

                (* Optimisation: return immediately, if possible, rather than
                   jumping and then returning. This may turn the following
                   unconditional branch into dead code, in which case it
                   will be removed by the lower-level code generator. *)
                val () =
                    if isEndOfProc tailKind andalso not (haveExited transtable)
                    then codeGenerate(exit (), cvec)
                    else ()
    
                (* Skip over the handler. *)
                val (skipHandler, skipCode) = unconditionalBranch (bodyResult, transtable)
                val () = codeGenerate(skipCode, cvec)
          
                (* Remove any result at the start of the handler.
                   Need this because fixupH does not do setState.
                   (It probably should do, though the state is fairly simple). *)
                val () =
                    case bodyResult of
                        MergeIndex bodyIndex => codeGenerate(removeStackEntry(transtable, bodyIndex), cvec)
                    |   NoMerge => ()
     
                (* Fix up the handler entry point - this resets the stack pointer
                   and clears the cache since the state is not known. *)
                val () = codeGenerate(fixupH (handlerLab, startOfHandler, transtable), cvec)

                (* The code for the handler body itself *)
                val handlerRes =  genToRegister (handler, whereto, tailKind, matchFailFn, loopAddr)
                (* Merge the results. *)
                val (mergeRes, mergeCode) = merge (skipHandler, transtable, handlerRes, mark)
                val () = codeGenerate(mergeCode, cvec)
            in
                mergeRes
            end
        
        | Ldexc =>
            let
                val regResult = resultReg ArgGeneral
                (* Exception packet is returned in result register. *)
            in
                codeGenerate(getRegister (transtable, regResult), cvec);
                codeGenerate(activeRegister regResult, cvec);
                MergeIndex(pushReg (transtable, regResult))
            end

        | Case {cases, test, default, caseType} =>
            let
                (* Cases are constructed by the optimiser out of if-then-else expressions. *)                
                val whereto = chooseMergeRegister(whereto, tailKind)
                
                (* Sort the cases into ascending order.  It's possible that we may have
                   duplicates if this came from an if-then-else construction so we
                   need to retain the ordering for items with the same case label. *)
                local
                    val labelCount = List.length cases
                    (* Add an extra field before sorting which retains the ordering for
                       equal labels. *)
                    val ordered = ListPair.zipEq (cases, List.tabulate(labelCount, fn n=>n))
                    fun leq ((_, w1: word), n1: int) ((_, w2), n2) =
                        if w1 = w2 then n1 <= n2 else w1 < w2
                    val sorted = List.map #1 (Misc.quickSort leq ordered)
                    (* Filter out any duplicates. *)
                    fun filter [] = []
                    |   filter [p] = [p]
                    |   filter ((p as (_, lab1)) :: (q as (_, lab2)) :: tl) =
                            if lab1 = lab2
                            then p :: filter tl
                            else p :: filter (q :: tl)
                in
                    val cases = filter sorted
                end

                val (isExhaustive, min, max) =
                    case caseType of
                        CaseTag max => (true, 0w0, max)
                    |   _ =>
                        let
                            val (_, aLabel) = hd cases
                            fun foldCases((_, w), (min, max)) = (Word.min(w, min), Word.max(w, max))
                            val (min, max) = List.foldl foldCases (aLabel, aLabel) cases
                        in
                            (false, min, max)
                        end
                val testValue = genToStack (test, matchFailFn)
                val mark = markStack transtable

                (* Get exclusive use so that
                   indexedCase can modify the registers. *)
                val (testReg, testIndex, testCode)  =
                    loadEntryToSet (transtable, testValue, RegSet.generalRegisters, true);
                (* Need a work register. *)
                val (workReg, regCode) = getRegisterInSet(transtable, generalRegisters)

                val (indexCaseInstr, caseLabels, defaultLabel) =
                    indexedCase{testReg=testReg, workReg=workReg, minCase=min, maxCase=max, 
                            isArbitrary = case caseType of CaseInt => true | _ => false,
                            isExhaustive=isExhaustive}

                val () = codeGenerate(indexCaseInstr @ regCode @ testCode, cvec)
                val () = codeGenerate(removeStackEntry (transtable, testIndex), cvec)
                val () = codeGenerate(freeRegister (transtable, workReg), cvec)

                val startOfCase = saveState transtable

                (* Put in the default case.  Even when the case is exhaustive one entry is
                   always treated as a default and not included in the list of cases. *)
                local
                    (* We have to set "branched" to true before calling fixup. *)
                    val () = exiting transtable
                    val startCode = fixup(makeLabels(NoMerge, defaultLabel, startOfCase), transtable)
                    val () = codeGenerate(startCode, cvec)

                    (* Go down the list of cases and fix up any default labels to come here.
                       Default entries are represented by "holes" in the case list. *)
                    fun genDefaults(indexVal, label :: labelList, cl as ((_, caseLabel) :: cps)) =
                        if indexVal = caseLabel
                        then genDefaults(indexVal+0w1, labelList, cps)
                        else
                        (
                            codeGenerate(forwardJumpLabel label, cvec);
                            genDefaults(indexVal+0w1, labelList, cl)
                        ) 
                    |   genDefaults(indexVal, label :: labelList, []) =
                        (
                            codeGenerate(forwardJumpLabel label, cvec);
                            genDefaults(indexVal+0w1, labelList, [])
                        )
                    |   genDefaults(_, [], _) = ()
                    
                    val () = genDefaults(min, caseLabels, cases)

                    val defaultRes =
                        genToRegister (default, whereto, tailKind, matchFailFn, loopAddr);

                    (* Optimisation: return immediately, if possible, rather than
                       jumping and then returning. This may turn the following
                       unconditional branch into dead code, in which case it
                       will be removed by the lower-level code generator. *)
                    val () =
                       if isEndOfProc tailKind andalso not (haveExited transtable)
                       then codeGenerate(exit (), cvec)
                       else ();

                    val (lab, branchCode) = unconditionalBranch (defaultRes, transtable)
                    val () = codeGenerate(branchCode, cvec)

                    val () =
                        case defaultRes of
                            MergeIndex defaultIndex =>
                                codeGenerate(removeStackEntry (transtable, defaultIndex), cvec)
                        |   NoMerge => ()
                in
                    val exitDefault = lab
                end

                (* Generate the cases. *)
                fun genCases(indexVal, label :: labelList, (caseExp, caseLabel) :: cps) =
                    if indexVal <> caseLabel
                    then (* We have a hole.  Skip this item. *)
                        genCases(indexVal+0w1, labelList, (caseExp, caseLabel) :: cps)
                    else (* The index value corresponds to a label. *)
                    let
                        val startCode = fixup(makeLabels(NoMerge, label, startOfCase), transtable)
                        val () = codeGenerate(startCode, cvec)
                        val mark = markStack transtable

                        (* Generate this case and exit if tail-recursive. *)
                        val expResult =
                            genToRegister (caseExp, whereto, tailKind, matchFailFn, loopAddr);

                        val () =
                            if isEndOfProc tailKind andalso not (haveExited transtable)
                            then codeGenerate(exit (), cvec)
                            else ();
                    in
                        if null cps
                        then (*  Finished. *) expResult (* Last expression. *)
                        else
                        let
                            val (lab, branchCode) = unconditionalBranch (expResult, transtable)
                            val () = codeGenerate(branchCode, cvec)

                            val () =
                                case expResult of
                                    MergeIndex expIndex =>
                                        codeGenerate(removeStackEntry(transtable, expIndex), cvec)
                                |   NoMerge => ();

                            val lastResult = genCases(indexVal+0w1, labelList, cps)
                            val (mergeRes, mergeCode) = (* Now fix up the exit label. *)
                                merge (lab, transtable, lastResult, mark)
                            val () = codeGenerate(mergeCode, cvec)
                        in
                            mergeRes
                        end
                    end
                | genCases _ = raise InternalError "genCase - null case list"

                val caseResult = genCases(min, caseLabels, cases)
                val (mergeRes, mergeCode) = merge (exitDefault, transtable, caseResult, mark)
                val () = codeGenerate(mergeCode, cvec)
            in
                mergeRes
            end      

        | MutualDecs dl =>
            let
                (* Mutually recursive declarations. These can only be functions.
                   Recurse down the list
                   pushing the addresses of the closure vectors or forward
                   references to the code, then unwind the recursion and fill
                   in closures or compile the code. *)
                local
                    (* We now use the fact that decToPstack contains noindex to detect
                        mutual recursion in genProc.*)
                    fun setToEmpty(Declar{addr, ...}) = Array.update (decToPstack, addr, noIndex)
                    |   setToEmpty _ = raise InternalError "not dec"
                in
                    val () = List.app setToEmpty dl
                end

                fun genMutualDecs []      = ()
                |   genMutualDecs ((Declar{value=dec, addr, references, ...})::ds) =
                (
                    case dec of
                        Lambda lam =>
                        let
                            (* This procedure is called once the closure has been
                               created but before the entries have been filled in. *) 
                            fun nextMutual(r, slEntries) =
                            let
                                val () = codeGenerate(localDeclaration (r, addr, references, slEntries), cvec)
                            in (* Now time to do the other closures. *)
                                genMutualDecs ds
                            end
                            val _ =
                                genProc(lam, nextMutual, null ds, (* Last one? *) NoHint, matchFailFn)
                        in
                            ()
                        end
                        |   _ =>
                        let (* should only be constants i.e. procedures already compiled. *)
                            val decEntry = genToStack (dec, matchFailFn)
                            val () = codeGenerate(localDeclaration (decEntry, addr, references, NONE), cvec)
                        in
                          genMutualDecs ds
                        end
                ) (* genMutualDecs *)
                | genMutualDecs _ =
                    raise InternalError "genMutualDecs - Not a declaration";
              
                val () = genMutualDecs dl;
            in
                NoMerge (* Unused. *)
            end

        | Recconstr reclist =>
            let
                val vecsize = List.length reclist
                val () =
                    if vecsize = 0 (* shouldn't occur *)
                    then raise InternalError "Zero sized vector"
                    else ()
                (* Since the vector is immutable, we have to evaluate
                   all the values before we can allocate it. *)
                val entries = List.map(fn h => genToStack (h, matchFailFn)) reclist
                val asConstants = List.map(fn i => isConstant(i, transtable)) entries
            in
                if List.exists(fn NotConst => true | _ => false) asConstants
                then
                let
                    fun loadSmallVector ([], _) = callgetvec (vecsize, F_words, whereto)
                    |   loadSmallVector (v::t, wordOffset) =
                        let
                            val (vec, vecCode) = loadSmallVector (t, wordOffset + 1)
                            val moveCode = moveToVec (vec, v, wordOffset, transtable)
                        in
                            (vec, moveCode @ vecCode)
                        end;
                    val (vec, code) = loadSmallVector(entries, 0)
                    val () = codeGenerate(code, cvec)
                in
                    MergeIndex vec
                end
                else
                let
                    (* The higher levels of the code generator attempt to remove tuples of
                       constants but some still slip through.  One particular case that
                       can't be handled in the higher levels is a tuple that contains a
                       recursive reference.  That does occur with equality functions. *)
                    (* Construct a mutable object and fill it in. *)
                    val toFill = ref vecsize
                    val vec : address = alloc(toShort vecsize, F_mutable_words, toMachineWord 0)

                    (* Set the element in the address.  If this is a forward reference to a
                       code segment it won't be called until the code has been completed. *)
                    fun setItem n (_, addr: machineWord) =
                    (
                        assignWord(vec, toShort n, addr);
                        toFill := !toFill - 1;
                        if !toFill = 0 then lock vec else ()
                    )

                    fun addItem(ConstLit lit, n) = (setItem n ((), lit); n+1)
                    |   addItem(ConstCode code, n) = (addCompletionHook(code, setItem n); n+1)
                    |   addItem(NotConst, _) = raise InternalError "addItem: NotConst"
                    val _ = List.foldl addItem 0 asConstants
                    (* Remove the entries which aren't actually used. *)
                    val () = List.app(fn n => codeGenerate(incrUseCount(transtable, n, ~1), cvec)) entries
                in
                    MergeIndex(pushConst(transtable, toMachineWord vec))
                end
            end
        
        | Container size =>
            (* Reserve a number of words on the stack for use as a tuple on the
               stack.  The result is the address of this space. *)
            let
                val (reserveEntry, reserveCode) = reserveStackSpace(transtable, size)
            in
                codeGenerate(reserveCode, cvec);
                MergeIndex reserveEntry
            end

        | SetContainer{container, tuple, size} =>
            (* Copy the contents of a tuple into a container. *)
            let
                val vec = genToStack (container, matchFailFn);
            in
                case tuple of
                    Recconstr cl =>
                        (* Simply set the container from the values. *)
                    let
                        fun setValue(v, wordOffset) =
                        let
                            val entry = genToStack (v, matchFailFn)
                        in
                            (* Move the entry into the container.  Does not affect the
                               use count for the container entry. *)
                            codeGenerate(moveToVec (vec, entry, wordOffset, transtable), cvec);
                            wordOffset + 1
                        end
                    in
                        List.foldl setValue 0 cl;
                        ()
                    end

                |   _ =>
                    let
                        val tup = genToStack (tuple, matchFailFn);
        
                        fun copy n =
                        if n = size
                        then ()
                        else
                        let
                            (* We need to ensure that the tuple entry is only removed
                               when we load the last item from it. *)
                            val wordOffset = n
                            val _ =
                                if n = size - 1
                                then ()
                                else codeGenerate(incrUseCount(transtable, tup, 1), cvec)
                            val (entry, entryCode) = indirect (wordOffset, tup, transtable)
                            val () = codeGenerate(entryCode, cvec)
                        in
                            codeGenerate(moveToVec (vec, entry, wordOffset, transtable), cvec);
                            copy (n+1)
                        end
                    in
                        copy 0
                    end;

                codeGenerate(removeStackEntry(transtable, vec), cvec); (* Free the container entry. *)
                (* Return a void result if necessary. *)
                if isNoResult whereto then NoMerge
                else MergeIndex(pushConst (transtable, DummyValue))
            end

        | TupleFromContainer(container, size) =>
            (* Create a tuple from the contents of a container. *)
            let
                val vec = genToStack (container, matchFailFn);
                val (tup, tupleCode) = callgetvec (size, F_words, whereto);
                val () = codeGenerate(tupleCode, cvec)

                fun copy n =
                    if n = size
                    then ()
                    else
                    let
                        (* We need to ensure that the container entry is only removed
                           when we load the last item from it. *)
                        val _ =
                            if n = size - 1
                            then ()
                            else codeGenerate(incrUseCount(transtable, vec, 1), cvec)
                        val (entry, entryCode) = indirect (n, vec, transtable)
                        val () = codeGenerate(entryCode, cvec)
                    in
                        codeGenerate(moveToVec (tup, entry, n, transtable), cvec);
                        copy (n+1)
                    end
            in
                copy 0;
                MergeIndex tup (* Result is the tuple. *)
            end

        |   TagTest { test, tag, ... } =>
            let
                (* Convert this into a simple equality function. *)
                val code =
                    Eval {
                        function = Constnt(ioOp POLY_SYS_word_eq),
                        argList=[(test, GeneralType), (Constnt(toMachineWord tag), GeneralType)],
                        earlyEval = false, resultType=GeneralType }
            in
                gencde (code, true(* Try to put in-line *), whereto, tailKind, matchFailFn, loopAddr)
            end

        |   KillItems { expression, killSet, killBefore } =>
            let 
                (* This is inserted by the higher level code to get the use-counts
                   correct.  Kill entries are Extract entries with lastRef true. *)
                fun cgKill toKill =
                    (gencde(toKill, true, NoResult, NotEnd, matchFailFn, loopAddr); ())
            in
                if killBefore
                then (* Process the kill set before the expression. *)
                (
                    List.app cgKill killSet;
                    gencde (expression, primBoolOps, whereto, tailKind, matchFailFn, loopAddr)
                )
                else (* Process the expression first, then kill the items *)
                let
                    val result =
                        gencde (expression, primBoolOps, whereto, tailKind, matchFailFn, loopAddr)
                in
                    List.app cgKill killSet;
                    result
                end
            end


        |   IndirectVariable{ base=_, offset=_, ... } =>
                raise InternalError "TODO: IndirectVariable"

        |   TupleVariable(_(* varList*), _(* length *)) =>
                raise InternalError "TODO: TupleVariable"

        | CodeNil => 
            raise InternalError "gencde: can't code-generate CodeNil value"

        | Global _ =>
            raise InternalError "gencde: can't code-generate Global value"
    in
        (* Various cases create results even if they're not required.  Remove them. *)
        case (result, whereto) of
            (NoMerge, NoResult) => NoMerge
        |   (NoMerge, _) => raise InternalError "gencde: Result wanted but none supplied"
        |   (MergeIndex m, NoResult) => (incrUseCount(transtable, m, ~1); NoMerge)
        |   (MergeIndex _, _) => result
    end (* gencde *) 

    (* Generate an expression putting the result in any register, and return
       the location of it on the stack. *)
    and genToStack (pt : codetree, matchFailFn) : stackIndex =
        let
            val res = gencde (pt, true, NoHint, NotEnd, matchFailFn, NONE)
        in
            case res of
                MergeIndex index => index
              | NoMerge => raise InternalError "genToStack: no result"
        end
(* ...
   (* Used when the result must be put in a register. *)
   and genToResult (pt, whereto, tailKind, matchFailFn, loopAddr) : unit =
   let
     (* Stack results are forced into result register *)
     val toWhere = if isToPstack whereto then UseReg regResult else whereto;
     
     val result = gencde (pt, true, toWhere, tailKind, matchFailFn, loopAddr);
   in
     (* If we need a result put it in the result reg.  We request exclusive use
    of it because otherwise there is a problem when merging the results
    of an if-then-else if the result register is somewhere else on the
    pstack (e.g. let a == ...; if ... then a else ...) *)
      case toWhere of
        UseReg rr => loadToSpecificReg (cvec, transtable, rr, result, true)
      | _        => ()
   end (* genToResult *)
... *)

   (* Used when the result must be put in a register. *)
    and genToRegister (pt, whereto, tailKind, matchFailFn, loopAddr) : mergeResult =
    let
        val result = gencde (pt, true, whereto, tailKind, matchFailFn, loopAddr)
    in
        case (whereto, result) of
            (NoResult, _) => NoMerge
        |   (UseReg rr, MergeIndex index) =>
                if haveExited transtable (* If we've raised an exception we can ignore this. *)
                then MergeIndex index
                else
                let
                    (* If we need a result put it in the result reg.  We request exclusive use
                       of it because otherwise there is a problem when merging the results
                       of an if-then-else if the result register is somewhere else on the
                       pstack (e.g. val a = ...; if ... then a else ...),
    
                       If we're at the end of a function, we're not merging, so we don't need
                       exclusive use. However, I don't think we actually save anything by trying
                       to make use of this fact so let's just be naive. SPF 27/11/96 *)
                    val (_, mergeItem, mergeCode) = loadEntryToSet (transtable, index, rr, true)
                in
                    codeGenerate(mergeCode, cvec);
                    MergeIndex mergeItem
                end
        |   (UseReg _, NoMerge) => raise InternalError "genToRegister: no result"
        |   (NoHint, _) => raise InternalError "genToRegister: not a register"
    end (* genToRegister *)

    (* `mutualRecursive' is only used for mutually recursive procedures
       where a procedure may not be able to fill in its closure if it does
       not procedure address has been pushed but before the code is generated. 
       `lastDec' is true if there are no more mutually recursive declarations.
    *)
    and genProc ({ closure=closureList, makeClosure, name=lambdaName, body=lambdaBody,
                   argTypes, resultType, closureRefs, argLifetimes, localCount, ... },
                 mutualRecursive: stackIndex * (unit->operation list) option -> unit, _, whereto, matchFailFn) =
    let
        fun allConstnt [] = true
        |   allConstnt (Constnt _ :: t) = allConstnt t
        |   allConstnt _ = false;

        (* Finds the nth. item in the closure and returns the entry *)
        fun findClosure l n = List.nth(l, n-1)
        
    in
        if not makeClosure
        then
        let (* static link form *)
            (* If a procedure can be called by static link references then
               non-locals can be loaded by following the static chain. The offset
               is the entry in the (pseudo-)closure as with a procedure that
               requires a closure, but these can be translated into real stack
               offsets. A stack value which is loaded into a real or pseudo closure
               always has that load treated as one reference as far as the use
               counts are concerned, even though it may be loaded several times
               in an inner procedure or by different calls. `pushNonLocal' must
               not decrement the use count so that the stack values remain on the
               stack to be referenced by the procedure, and are only removed when
               the containing block is removed. *)
            val newCode = codeCreate (true (* just the code *), lambdaName, debugSwitches);
        
            (* Generates code for non-local references or recursive references. *)
            fun previous (prevloc, makeSl: unit->stackIndex * operations, newtab): stackIndex * operations =
            (* Although directly-recursive references do not involve calls to
              `previous' (the only directly-recursive references are recursive
              calls and they are dealt with in `loadSl') they may be produced
              as "kill entries" whose only effect is to indicate that the
              closure/static-link register is no longer required on a particular
              flow of control. *)
            if prevloc = 0 then makeSl()
            else
            let
                (* The closure entry will usually be an Extract, but may have been
                   compiled down to a real constant. This wouldn't happen if earlier
                   phases were better at keeping constants out of closures (but that
                   requires "sophisticated" analysis of mutually recursive declarations). *)
                val closureEntry = findClosure closureList prevloc;
            in
                case closureEntry of
                    Constnt w => (pushConst (newtab, w), [])
          
                |   Extract {fpRel = true, addr = locn, ...} => (* argument or on stack *)
                    if locn < 0 (* argument *)
                    then
                    (
                        case Vector.sub (argRegTab, numOfArgs + locn) of
                            ArgInReg regEntry => pushNonLocal (transtable, newtab, regEntry, makeSl)
                        |   ArgOnStack actualAddr =>
                            let
                                val (sl, slCode) = makeSl ()
                                val (entry, indCode) = indirect (~actualAddr, sl, newtab)
                            in
                                (entry, indCode @ slCode)
                            end
                    )
                    else (* on the stack *)
                        pushNonLocal (transtable, newtab, Array.sub(decToPstack, locn), makeSl)
 
                |   Extract {fpRel = false, addr = locn, ...} => (* Try the next level *)
                        declOnPrevLevel(locn,
                            fn () => pushNonLocal (transtable, newtab, closureOrSlAddr, makeSl),
                            newtab)
     
                |   _ => raise InternalError "previous: bad codetree in closure"
            end

            (* Returns true if the procedure is to be called with a static link *)
            fun isSl prevloc =
            if prevloc = 0 then true (* Recursive call. It's this procedure *)
            else
            let (* Not directly recursive. *)
                val closureEntry = findClosure closureList prevloc;
            in
                (* We may have already compiled a "mutually recursive" function
                   to a constant, if it doesn't actually depend on this one.
                   (This wouldn't occur if the earlier stages were better at
                   removing such fake dependencies.) *)
                case closureEntry of
                    Constnt (w : machineWord) => isCode (toAddress w)

                |   Extract {fpRel = true, addr = correctedLoc, ...} =>
                        correctedLoc > 0 andalso isSome(Array.sub(staticLinkEntries, correctedLoc))
              
                |   Extract {fpRel = false, addr = correctedLoc, ...} =>
                        isStaticLink correctedLoc (* Non-local *)
            
                | _ => raise InternalError "isSl: bad codetree in function closure"
            end
         
            (* Loads the static link if the procedure is called with one
               and returns the entry point of the procedure on the stack. *)
            fun loadSl (0, makeSl, callingTab): stackIndex*stackIndex * operations =
                let (* Recursive call. *)
                    val (sl, slCode) = makeSl(); (* Push the static link. *)
                    val (closureIndex, loadClosure) =  (* Load into regClosure *)
                        loadToSpecificReg (callingTab, regClosure, sl, false);
                in
                    (* And push the address of this procedure as the entry point. *)
                    (pushCodeRef (callingTab, newCode), closureIndex, loadClosure @ slCode)
                end

            |   loadSl (prevloc, makeSl, callingTab) = (* Non-recursive. *)
                case findClosure closureList prevloc of
                    Extract { addr=correctedLoc, fpRel, ...} =>
                    if fpRel
                    then (* On this level *)
                        let
                            val (sl, slCode) = makeSl()
                            val (closureIndex, closureCode) =
                                (* Load closure/sl register with the static link. *)
                                loadToSpecificReg (callingTab, regClosure, sl, false)
                            val (procAddr, prevCode) = (* Get the address of the procedure. *)
                                previous (prevloc, makeSl, callingTab)
                        in
                            (procAddr, closureIndex, prevCode @ closureCode @ slCode)
                        end
                    else (* Non-local *)
                        let
                            val (procAddr, staticLink, code) =
                                loadStaticLink(correctedLoc,
                                    fn () => pushNonLocal (transtable, callingTab, closureOrSlAddr, makeSl),
                                    callingTab)
                        in
                            (procAddr, staticLink, code)
                        end
                |   _ => raise InternalError "loadSl - closure not extract"

            (* Returns the register set for a static link function. *)
            fun slRegSet prevloc =
            if prevloc = 0 then allRegisters (* Recursive call - all registers. *)
            else
            let (* Not directly recursive. *)
                val closureEntry = findClosure closureList prevloc;
            in
                case closureEntry of
                    Constnt (w : machineWord) => getRegisterSetForFunction w
                |   Extract {fpRel = true, addr, ...} =>
                    if addr > 0
                    then getFunctionRegSet(Array.sub(decToPstack, addr), transtable)
                    else raise InternalError "slRegSet: argument"
                |   Extract {fpRel = false, addr, ...} =>
                    staticLinkRegSet addr (* Non-local *)
                |   _ => raise InternalError "slRegSet: bad codetree in function closure"
            end
        
            (* Make sure all the closure values in registers are on the stack,
               in case they are used as non-locals.  Changed this from the
               original code which pushed everything. *)
            local
                fun pushClosure (Extract{fpRel=true, addr, ...}) =
                    (* Local *)
                    if addr < 0
                    then
                    (
                        case Vector.sub (argRegTab, numOfArgs + addr) of
                            ArgInReg regEntry => codeGenerate(pushSpecificEntry(transtable, regEntry), cvec)
                        |   _ => ()
                    )
                    else (* on the stack *)
                        codeGenerate(pushSpecificEntry(transtable, Array.sub(decToPstack, addr)), cvec)
                |   pushClosure (Extract{fpRel=false, ...}) =
                        (* Non-local or recursive reference: make sure the closure/static
                           link pointer is on the stack. *)
                        if closureLifetime = 0 
                        then () (* May not have a closure/sl. *)
                        else codeGenerate(pushSpecificEntry(transtable, closureOrSlAddr), cvec)
                | pushClosure _ = () (* Constant. *)
            in
                val () = List.app pushClosure closureList
            end

            (* This function is called when the last reference is found to this function.
               Loading an entry into the closure counts as one reference but we don't
               consider any final references at the point when the function is declared
               because we need the entries to remain until the last call to the function. *)
            fun releaseClosure () =
            let
                fun releaseEntry(Extract(ext as {lastRef=true, ...}), l) =
                       incrUseCount(transtable, #1(locaddr ext), ~1) @ l
                |   releaseEntry (_, l) = l
            in
                List.foldl releaseEntry [] closureList
            end

            (* Push a forward reference to the code in case of mutually
               recursive references.  This is left as the result for normal
               references. *)
            val result = pushCodeRef (transtable, newCode)
            (* Make this declaration and handle any recursive references. *)
            val () = mutualRecursive(result, SOME releaseClosure)

            (* Now code-generate the procedure, throwing away the result which
               will be put into the forward reference. *)
            val _ = closureRefs = 0 andalso raise InternalError "Static link but unreferenced cp"
            val (ops, maxStack, regList, callsAFunction) = 
              codegen(lambdaBody, previous, isSl, loadSl, slRegSet, closureRefs,
                    argTypes, argLifetimes, resultType, localCount, debugSwitches)
            val _ = copyCode (newCode, ops, maxStack, regList, callsAFunction)
        in
            result
        end

      else (* Requires a closure but this may be a constant. *)
        let
            (* Frequently the closure is actually empty but it may be that there are
               constants or values that are now constants that haven't been picked out
               by previous passes.  First try loading all the items of the closure.  If
               there are mutually recursive references we may not be able to load them
               at this point.  *)
            fun loadClosure(c as Extract{fpRel=true, addr, ...}) =
                if addr >= 0 andalso Array.sub(decToPstack, addr) = noIndex
                then noIndex
                else genToStack (c, matchFailFn)
            |   loadClosure c = genToStack (c, matchFailFn)
            val initialLocs = List.map loadClosure closureList
            (* Extract any constants. *)
            val constants =
                List.map(fn i => if i = noIndex then NotConst else isConstant(i, transtable)) initialLocs
            val nonConstCount = List.foldl(fn (NotConst, n) => n+1 | (_, n) => n) 0 constants
        in
            if nonConstCount = 0
            then (* All the entries that are there are constants.  We can avoid constructing a
                    closure at run-time.  Instead we construct a single word item containing the address
                    of the code that can be used if a full closure call is used.  As far as possible,
                    though, calls to this function are made using the PureCode convention which
                    bypasses the closure altogether.  That means that any constants that are there must be
                    passed back via "previous". *)
            let
                (* Create a one word item for the closure.  This is returned for recursive references
                   and filled in with the address of the code when we've finished. *)
                val newCode = codeCreate (false (* make a closure *), lambdaName, debugSwitches)

                fun previous (0, _, newtab) = (* load the address of the closure itself *)
                    (pushCodeRef(newtab, newCode), [])
                |   previous (locn, _, newtab) =
                        (* load a constant (item locn of the logical closure) *)
                        case List.nth(constants, locn-1) of
                            ConstLit lit => (pushConst (newtab, lit), [])
                        |   ConstCode code => (pushCodeRef(newtab, code), [])
                        |   NotConst => raise InternalError "previous: NotConst"
          
                val (ops, maxStack, regList, callsAFunction) = 
                        codegen (lambdaBody, previous, fn _ => false,
                            fn _ => raise InternalError "Not static link",
                            fn _ => raise InternalError "Not static link",
                            0, (* Discard regClosure *) argTypes, argLifetimes, resultType, localCount, debugSwitches)
                
                val closureAddr = copyCode (newCode, ops, maxStack, regList, callsAFunction)
                val result = pushConst (transtable, toMachineWord closureAddr);
                (* Clear off the constant entries. *)
                val () = List.app(fn n => codeGenerate(incrUseCount(transtable, n, ~1), cvec)) initialLocs
                (* Handle any other recursive functions. *)
                val () = mutualRecursive(result, NONE)
            in
                result
            end

            else (* There's at least one non-constant so we're going to have to build a closure. *)
            let
                local
                    (* Convert the original index to a new index with the constants skipped. *)
                    fun makeIndex(NotConst :: t, n) = SOME n :: makeIndex(t, n+1)
                    |   makeIndex(_ :: t, n) = NONE :: makeIndex(t, n)
                    |   makeIndex([], _) = []
                in
                    val closureIndexes = Vector.fromList(makeIndex(constants, 1(*Starts from 1*)))
                end

                fun previous(0, makeSl, _) =
                        makeSl () (* load the address of the closure itself *)
                |   previous(locn, makeSl, newtab) =
                        case List.nth(constants, locn-1) of
                            ConstLit lit => (pushConst (newtab, lit), [])
                        |   ConstCode code => (pushCodeRef(newtab, code), [])
                        |   NotConst =>
                            let
                                val newLocn = valOf(Vector.sub(closureIndexes, locn-1))
                                val (sl, closureCode) = makeSl() (* load the closure *)
                                val (entry, indCode) = indirect(newLocn, sl, newtab) (* load value from the closure *)
                            in
                                (entry, indCode @ closureCode)
                            end

                val newCode = codeCreate (true (* just the code *), lambdaName, debugSwitches)
        
                val (ops, maxStack, regList, callsAFunction) = (* code-gen procedure *)
                    codegen (lambdaBody, previous, fn _ => false,
                        fn _ => raise InternalError "Not static link",
                        fn _ => raise InternalError "Not static link",
                        closureRefs, argTypes, argLifetimes, resultType, localCount, debugSwitches)

                val codeAddr = copyCode (newCode, ops, maxStack, regList, callsAFunction)
        
                val res = toMachineWord codeAddr

                (* Build the closure.  If there are outstanding entries it has to be mutable and we
                   can't complete it until we've done the other mutually recursive entries.  *)
                val incomplete = List.exists(fn i => i = noIndex) initialLocs
                val (vector, vecCode) =
                    callgetvec (nonConstCount+1, if incomplete then F_mutable_words else F_words, whereto)
                val () = codeGenerate(vecCode, cvec)
                (* First word is the address of the code. *)
                val () = codeGenerate(moveToVec (vector, pushConst (transtable, res), 0, transtable), cvec)
                (* Put in everything else *)
                fun fillClosure(index::indices, NotConst::constEntries, n) =
                        let
                            val indexOrDummy =
                                if index = noIndex
                                then (* Recursive entry.  This has to be initialised to avoid problems if we GC
                                        when allocating other closures. *)
                                    pushConst (transtable, DummyValue)
                                else index
                            val vecAddr = valOf(Vector.sub(closureIndexes, n))
                        in
                            codeGenerate(moveToVec(vector, indexOrDummy, vecAddr, transtable), cvec);
                            fillClosure(indices, constEntries, n+1)
                        end
                |   fillClosure(index::indices, _::constEntries, n) =
                    (
                        (* It was a constant.  Remove it. *)
                        codeGenerate(incrUseCount(transtable, index, ~1), cvec);
                        fillClosure(indices, constEntries, n+1)
                    )
                |   fillClosure _ = ()
                val () = fillClosure(initialLocs, constants, 0)

                (* Have to ensure that the closure remains on the psuedo-stack until
                   we've filled in all uses of it. The only references may be in the
                   closures of other procedures so it's possible that its use-count
                   could be zero when `mutualRecursive' returns. Have to  increment
                   the use-count and then decrement it afterwards to make sure it
                    is still on the stack. *)
                val () = codeGenerate(incrUseCount (transtable, vector, 1), cvec)
    
                  (* Any mutually recursive references. *)
                val () = mutualRecursive(vector, NONE)

                (* We should now be able to fill in the recursive references. *)
                val () =
                    if incomplete
                    then
                    let
                        fun fillRecursive(index::indices, entry::entries, n) =
                            (
                                if index = noIndex (* Deferred entry*)
                                then
                                let
                                    val loadEntry = genToStack(entry, matchFailed)
                                    val addr = valOf(Vector.sub(closureIndexes, n))
                                    val moveCode = moveToVec(vector, loadEntry, addr, transtable)
                                in
                                    codeGenerate(moveCode, cvec)
                                end
                                else ();
                                fillRecursive(indices, entries, n+1)
                            )
                        |   fillRecursive _ = ()
                        val () = fillRecursive(initialLocs, closureList, 0)

                        (* Finally we can lock this. *)
                        (* Increment the use count before the lock. *)
                        val () = codeGenerate(incrUseCount (transtable, vector, 1), cvec)
                        val lockInstr =
                            case checkAndReduce(instrLockSeg, [], fn _ => NONE) of
                                SOME(lockInstr, _) => lockInstr
                            |   NONE => raise InternalError "Lock instruction not implemented"
                        val (_, lockCode) = dataOp([vector], lockInstr, transtable, NoResult)
                        val () = codeGenerate(lockCode, cvec)
                    in
                        ()
                    end
                    else ()
                (* Restore the use count *)
                val () = codeGenerate(incrUseCount (transtable, vector, ~1), cvec)
            in
                vector
            end
        end
(*
      else let (* Full closure required. *)
        (* Item n of the logical closure is which item of the physical closure? *)
        fun translateClosureIndex (Constnt _ :: _) 1 =
              raise InternalError "translateClosureIndex: constants don't belong in physical closure"

          | translateClosureIndex (_ :: _) 1 = 1
          
          | translateClosureIndex (Constnt _ :: t) n =
              translateClosureIndex t (n - 1)

          | translateClosureIndex (_ :: t) n =
              translateClosureIndex t (n - 1) + 1
              
          | translateClosureIndex [] _ = 
              raise InternalError "translateClosureIndex: bad index into logical closure"
      
    (* Some of the non-local references will be references to the
       closure itself (for example, to embed it into a data-structure).
       We have to treat these slightly specially. They're still handled
       in the normal way by the reference-counting mechanism, so
       we don't have to do anything *too* clever here.
       SPF 2/1/97
       
       If we're accessing a known constant in the closure, load it
       from the constants section rather than from the closure itself.
     *)
        fun previous (locn, makeSl: unit-> stackIndex * operations, newtab) =
          if locn = 0
          then makeSl () (* load the address of the closure itself *)
          else let
            val closureItem = findClosure closureList locn;
          in
            case closureItem of
                Constnt cval =>
                    (pushConst (newtab, cval), []) (* load the value as a constant *)
              | _ =>
                let
                    val newLocn : int = translateClosureIndex closureList locn 
                    val (sl, closureCode) = makeSl () (* load the closure *)
                    val (entry, indCode) = indirect (newLocn, sl, newtab) (* load value from the closure *)
                in
                    (entry, indCode @ closureCode)
                end
          end;
        
        val newCode = codeCreate (true (* just the code *), lambdaName, debugSwitches)
        
        val _ = closureRefs = 0 andalso raise InternalError "Closure function but unreferenced cp"
        
        val (ops, maxStack, regList, callsAFunction) = (* code-gen procedure *)
          codegen (lambdaBody, previous, fn _ => false,
            fn _ => raise InternalError "Not static link",
            fn _ => raise InternalError "Not static link",
            closureRefs, argTypes, argLifetimes, resultType, localCount, debugSwitches)

        val codeAddr = copyCode (newCode, ops, maxStack, regList, callsAFunction)
        
        val res = toMachineWord codeAddr
      in
        if lastDec
        then let
          (* Can avoid having to set and clear the mutable bit. *)
          (* Load items for the closure. *)
          val allWereConsts = ref true
          val allWereConstsOrCode = ref true
          
          (* Compare with the code for Recconstr *)  
          fun loadItems [] offset =
            let
              (* get store for closure *)
              val (vector, vectorCode) = callgetvec (offset, F_words, whereto)
              val () = codeGenerate(vectorCode, cvec)
            in
              (* Put code address into closure *)
              codeGenerate(moveToVec (vector, pushConst (transtable, res), 0, transtable), cvec);
              vector
            end
            
            | loadItems (Constnt _ :: t) offset =
                (* constants don't belong in the physical closure *)
                loadItems t offset

            | loadItems (h :: t) offset =
              let
                val valIndex = genToStack (h, matchFailFn)
                val _ =
                    case isConstant(valIndex, transtable) of
                        NotConst => (allWereConsts := false; allWereConstsOrCode := false)
                    |   ConstCode _ => allWereConsts := false
                    |   _ => ()
                (*
                val _ = case isCodeRef(valIndex, transtable) of SOME s => print("****Code Ref in closure - " ^ procName s ^ "\n") | NONE => ()
                val _ = case isConstant(valIndex, transtable) of SOME _ => print("****Constant in closure - \n") | NONE => ()
                *)
                val vec = loadItems t (offset + 1)
              in
                codeGenerate(moveToVec (vec, valIndex, offset, transtable), cvec);
                vec
              end;

          val vector = loadItems closureList 1
          
          val _ = if ! allWereConsts then print "***Closure - all constants\n"
                  else if ! allWereConstsOrCode then print "***Closure - all constants or code\n" else ()
        in
          (* Have to call this mutualRecursive to register the address. *)
          mutualRecursive(vector, NONE); 
          vector
        end
         
        else
        let
            (* More mutually recursive declarations. We have to allocate as a
               mutable segment and then clear the mutable bit. *)
            fun nonConstntCount [] = 0
            |   nonConstntCount (Constnt _ :: t) = nonConstntCount t
            |   nonConstntCount (_ :: t) = nonConstntCount t + 1;
           
            val closureSize = nonConstntCount closureList + 1;
          
            (* get store for closure *)
            val (vector, vecCode)  = callgetvec (closureSize, F_mutable_words, whereto)
            val () = codeGenerate(vecCode, cvec)
          
            (* Put code address into closure *)
            local
                val locn = pushConst (transtable, res);
            in
                val () = codeGenerate(moveToVec (vector, locn, 0, transtable), cvec)
            end;

            local
                (* Must clear each word of the closure in case we get a
                   garbage collection. *)
                val locn = pushConst (transtable, DummyValue);
                val wordsToClear : int = closureSize - 1;
                val () = codeGenerate(incrUseCount (transtable, locn, wordsToClear -1), cvec)
           
                (* N.B. moveToVec doesn't count as a use of vector. *)
                fun storeWord i = 
                    codeGenerate(moveToVec (vector, locn, i, transtable), cvec)
            in
                val () = forLoop storeWord 1 wordsToClear
            end;
          
            (* Have to ensure that the closure remains on the psuedo-stack until
               we've filled in all uses of it. The only references may be in the
               closures of other procedures so it's possible that its use-count
               could be zero when `mutualRecursive' returns. Have to  increment
               the use-count and then decrement it afterwards to make sure it
                is still on the stack. *)
            val () = codeGenerate(incrUseCount (transtable, vector, 1), cvec)
    
              (* Any mutually recursive references. *)
            val () = mutualRecursive(vector, NONE)
           
              (* Load items for the closure. *)
            fun loadItems []     _ = ()
            |   loadItems (Constnt _ ::t) addr = loadItems t addr (* constants don't belong in the physical closure *)
            |   loadItems (h::t) addr =
            let
                val loadEntry = genToStack (h, matchFailed)
                val moveCode = moveToVec (vector, loadEntry, addr, transtable)
                val () = codeGenerate(moveCode, cvec)
            in
                loadItems t (addr + 1)
            end;
    
            val () = loadItems closureList 1
            val () = codeGenerate(incrUseCount (transtable, vector, 1), cvec) (* Increment the use count before the lock. *)
            val lockInstr =
                case checkAndReduce(instrLockSeg, [], fn _ => NONE) of
                    SOME(lockInstr, _) => lockInstr
                |   NONE => raise InternalError "Lock instruction not implemented"
            val (_, lockCode) = dataOp([vector], lockInstr, transtable, NoResult)
            val () = codeGenerate(lockCode, cvec)
            val () = codeGenerate(incrUseCount (transtable, vector, ~1), cvec)
        in
            vector
        end
      end*)
    end (* genProc *)

    (* Generates test for if..then..else or while..do. Returns address of address field of jump.
       If jumpOn is true the jump is taken if the condition is true,
       if false it is taken if the condition is false. *)
    and genTest (pt, jumpOn, matchFailFn) : labels =
    let (* See if we can generate a conditional instruction. *)
        (* Those we can't deal with specially are evaluated to the stack and tested. *)
        fun genOtherTests () =
            case checkAndReduceBranches(if jumpOn then testNeqW else testEqW, [pt, Constnt False],
                                        fn (Constnt w) => SOME w | _ => NONE) of
                SOME (tst, args) =>
                    let
                        (* We can't use genToStack here because we need primBoolOps to be false. *)
                        fun cgArg arg = 
                            case gencde (arg, false (* primBoolOps *), NoHint, NotEnd, matchFailFn, NONE) of
                                MergeIndex index => (index, [])
                            |   NoMerge => raise InternalError "genTest: no result"
                        val argsAndCode = List.map cgArg args
                        val argLocns = List.map #1 argsAndCode
                        (* Return the code ordered with earlier arguments later in the list. *)
                        val argCode = List.foldl (fn ((_, argCode), code) => argCode @ code) [] argsAndCode
                        val (label, testCode) = compareAndBranch (argLocns, tst, transtable)
                    in
                        codeGenerate(testCode @ argCode, cvec);
                        label
                    end
                (* Should consider the possibility that checkAndReduceBranches might return two args. *)
            |   NONE => raise InternalError "compareAndBranch returned failure"
    in
      case pt of
        Cond (testPart, thenPart, elsePart) =>
        let
          val mark1 = markStack transtable
          val mark2 = markStack transtable
          
          (* Test the condition part. *)
          val a : labels = genTest (testPart, false, matchFailFn)
        in
          if isEmptyLabel a
          then (* The test evaluated to true.  We must only generate
                  the then-part.  This is more than an optimisation.
                  "Nojump" does not set the correct state for the
                  else-part which can cause problems. *)
             (
             unmarkStack(transtable, mark2);
             unmarkStack(transtable, mark1);
             genTest (thenPart, jumpOn, matchFailFn)
             )
          else if haveExited transtable
          then (* Unconditional jump.  Only need the else-part. *)
             (
             unmarkStack(transtable, mark2);
             unmarkStack(transtable, mark1);
             codeGenerate(fixup (a, transtable), cvec);
             genTest (elsePart, jumpOn, matchFailFn)
             )
          else
          let
              (* Now the `then-part' *)
              val b : labels = genTest (thenPart, jumpOn, matchFailFn);
              
              (* Put in an unconditional jump round the `else-part'.
                 This will be taken if the `then-part' drops through. *)
              val (notB, notCode) = unconditionalBranch (NoMerge, transtable)
              val () = codeGenerate(notCode, cvec)
              
              (* Fill in the label for the then-part part. *)
              val () = codeGenerate(fixup (a, transtable), cvec);
              
              (* Now do the `else-part' and jump on the inverse of the condition. *)
              val notC = genTest (elsePart, not jumpOn, matchFailFn);
              
              (* i.e. we drop though if the condition is the one we should have
                 jumped on. Now merge in the first label so we have both cases
                 when we should jump together, *)
              val (_, mergeBCode) = merge (b, transtable, NoMerge, mark2)
              val () = codeGenerate(mergeBCode, cvec)
              
              (* and now take the jump. *)
              val (resultLab, resultCode) = unconditionalBranch (NoMerge, transtable)
              val () = codeGenerate(resultCode, cvec)
              
              (* Come here if we are not jumping. *)
              val () = codeGenerate(fixup (notB, transtable), cvec);
              val (_, mergeCCode) = merge (notC, transtable, NoMerge, mark1)
              val () = codeGenerate(mergeCCode, cvec)
            in 
              resultLab
            end
        end

        (* Simple Cases generate better jumping code like this,
           rather than creating a boolean return value, then testing it
           and jumping on the result. We could be less special-case here,
           but this particular case is exceptionally important for
           handling inlined selector functions. SPF 24/2/1998
        *)
            (* Previously Cases were generated from almost all simple comparisons.
               Now that they are only generated if there are sufficient numbers of
               branches this can probably be removed. *)
      | Case {cases = [(result, tag)], test, default, ...} =>
        let
          val equalFun  : codetree = Constnt (ioOp POLY_SYS_equala);
          val arguments = [(test, GeneralType), (Constnt (toMachineWord tag), GeneralType)];
          val eqTest    : codetree = 
             Eval {function = equalFun, argList = arguments, earlyEval = true, resultType=GeneralType};
        in
          genTest (Cond (eqTest, result, default), jumpOn, matchFailFn) 
        end

      (* Constants - primarily for andalso/orelse. *)
      | Constnt w =>
          (* If true and we jump on true or false and jump on false *)
          (* then put in an unconditional jump. *)
          if wordEq (w, True) = jumpOn
          then
            let
                val (lab, code) = unconditionalBranch (NoMerge, transtable)
                val () = codeGenerate(code, cvec)
            in
                lab
            end
          else noJump (* else drop through. *)

        |   Newenv vl =>
            let (* Blocks and particularly inline procedures. *)
                (* Process the list up to the last item with "gencde",
                   and the last item with "genTest". *)
                fun codeBlock []       = noJump
                |   codeBlock [h]      = genTest (h, jumpOn, matchFailFn)
                |   codeBlock (h :: t) = 
                    (
                        gencde (h, true, NoResult, NotEnd, matchFailFn, NONE);
                        codeBlock t
                    )
            in
                codeBlock vl 
            end

        |   TagTest { test, tag, ... } =>
            let
                (* Convert this into a simple equality function. *)
                val code =
                    Eval {
                        function = Constnt(ioOp POLY_SYS_word_eq),
                        argList=[(test, GeneralType), (Constnt(toMachineWord tag), GeneralType)],
                        earlyEval = false, resultType=GeneralType }
            in
                genTest(code, jumpOn, matchFailFn)
            end

      | Eval {function = Constnt oper, argList = args, ...} =>
      (* May be an interface operation which can be put in line. *)
      let
        (* Generate a compare instruction. *)
        fun genCompare (args, t, f) =
        let
            val test    = if jumpOn then t else f;
        in
            (* Check that the instruction is implemented. *)
            case checkAndReduceBranches(test, args, fn (Constnt w) => SOME w | _ => NONE) of
                SOME (test, args) =>
                   let (* Generate the instruction and get the direction. *)
                        (* Code generate each argument to the pstack. *)
                        val argLocns =
                            List.map (fn arg => genToStack (arg, matchFailFn)) args
                        val (label, testCode) = compareAndBranch (argLocns, test, transtable)
                        val () = codeGenerate(testCode, cvec)
                   in
                     label
                   end
            |   NONE => genOtherTests () 
        end (* genCompare *);

      in
        case args of
          [] => (* We don't currently have any nullary special cases *)
             genOtherTests ()
          
        | [(arg, _)] =>
            (* unary special cases *)
            if wordEq (oper,ioOp POLY_SYS_not_bool)
              then genTest (arg, not jumpOn, matchFailFn)
    
            else if wordEq (oper,ioOp POLY_SYS_is_short)
            then
            (
              case arg of
                Constnt (w : machineWord) =>
                  if isShort w
                  then genTest (constntTrue,  jumpOn, matchFailFn)
                  else genTest (constntFalse, jumpOn, matchFailFn)
        
              | _ =>
                (
                    case checkAndReduceBranches(if jumpOn then Short else Long, [arg],
                                                fn (Constnt w) => SOME w | _ => NONE) of
                        SOME (testOp, [arg]) =>
                        let
                            val locnOfArg1 = genToStack (arg, matchFailFn);
                            val (label, testCode) = compareAndBranch([locnOfArg1], testOp, transtable)
                            val () = codeGenerate(testCode, cvec)
                        in
                            label
                        end
                    |   _ => genOtherTests ()
                )
            )
            
            else (* Non-special unary function.*)
                  genOtherTests ()
           
        | [(arg1, _), (arg2, _)] =>
            (* binary special cases *)
            if wordEq (oper,ioOp POLY_SYS_word_eq)
            then genCompare ([arg1, arg2], testEqW, testNeqW)
         
            else if wordEq (oper,ioOp POLY_SYS_word_neq)
            then genCompare ([arg1, arg2], testNeqW, testEqW)

            else if wordEq (oper,ioOp POLY_SYS_equala)
            then genCompare ([arg1, arg2], testEqA, testNeqA)

            else if wordEq (oper,ioOp POLY_SYS_int_geq)
            then genCompare ([arg1, arg2], testGeqA, testLtA)

            else if wordEq (oper,ioOp POLY_SYS_int_leq)
            then genCompare ([arg1, arg2], testLeqA, testGtA)

            else if wordEq (oper,ioOp POLY_SYS_int_gtr)
            then genCompare ([arg1, arg2], testGtA, testLeqA)

            else if wordEq (oper,ioOp POLY_SYS_int_lss)
            then genCompare ([arg1, arg2], testLtA, testGeqA)

            else if wordEq (oper,ioOp POLY_SYS_word_geq)
            then genCompare ([arg1, arg2], testGeqW, testLtW)

            else if wordEq (oper,ioOp POLY_SYS_word_leq)
            then genCompare ([arg1, arg2], testLeqW, testGtW)

            else if wordEq (oper,ioOp POLY_SYS_word_gtr)
            then genCompare ([arg1, arg2], testGtW, testLeqW)

            else if wordEq (oper,ioOp POLY_SYS_word_lss)
            then genCompare ([arg1, arg2], testLtW, testGeqW)

            else if wordEq (oper,ioOp POLY_SYS_Real_eq)
            then genCompare ([arg1, arg2], testEqFP, testNeqFP)
         
            else if wordEq (oper,ioOp POLY_SYS_Real_neq)
            then genCompare ([arg1, arg2], testNeqFP, testEqFP)

            else if wordEq (oper,ioOp POLY_SYS_Real_geq)
            then genCompare ([arg1, arg2], testGeqFP, testLtFP)

            else if wordEq (oper,ioOp POLY_SYS_Real_leq)
            then genCompare ([arg1, arg2], testLeqFP, testGtFP)

            else if wordEq (oper,ioOp POLY_SYS_Real_gtr)
            then genCompare ([arg1, arg2], testGtFP, testLeqFP)

            else if wordEq (oper,ioOp POLY_SYS_Real_lss)
            then genCompare ([arg1, arg2], testLtFP, testGeqFP)

            else genOtherTests () (* Non-special binary function. *)

        |   [(arg1, _), (arg2, _), (arg3, _), (arg4, _), (arg5, _)] =>
            if wordEq (oper,ioOp POLY_SYS_bytevec_eq)
            then genCompare ([arg1, arg2, arg3, arg4, arg5], byteVecEq, byteVecNe)
            else genOtherTests () (* Non-special function. *)

        | _ => (* Functions with more than 2 arguments. *)
            genOtherTests ()
      end (* constant functions *)

      | _ => (* Anything else *)
         genOtherTests ()

    end

    (* if/then/else, cand and cor. NB if/then/else may be translated
       into a CASE by the optimiser and code-generated there. *)
    and genCond (testExp, thenExp, CodeNil, whereto, tailKind, matchFailFn, loopAddr) : mergeResult =
        (* No else-part.  This is used only in pattern matching bindings. *)
        let
            val needsResult = not (isNoResult whereto)
            val mark = markStack transtable
            val lab  = genTest (testExp, false, matchFailFn) (* code for condition *)
             (* code for "then part" - noResult 'cos we generate "void" below*)
            val _ = genToRegister (thenExp, NoResult, tailKind, matchFailFn, loopAddr);
            val (_, mergeCode)  = merge (lab, transtable, NoMerge, mark)
            val () = codeGenerate(mergeCode, cvec)
        in
            if needsResult
            then MergeIndex(pushConst (transtable, DummyValue)) (* Generate a void result. *)
            else NoMerge (* Unused *) 
        end
    
    |   genCond (testExp, thenPt, elsePt, whereto, tailKind, matchFailFn, loopAddr) =
        let
            val mark = markStack transtable
            (* We use the then-part to determine the register for the result so if
               it's simple we probably want to swap the else- and then-parts  *)
            val reverse =
                case thenPt of
                    Constnt _ => true
                |   Extract _ => true
                |   Raise _ => true
                |   _ => false
            val (direction, thenExp, elseExp) =
                if reverse
                then (true, elsePt, thenPt)
                else (false, thenPt, elsePt)
            val lab  = genTest (testExp, direction, matchFailFn) (* code for condition *)
            (* There used to be code in here to handle specially the case where the
             test expression was a constant.  I've taken that out, partly because
             the simple cases are dealt with by the optimiser but more seriously
             because it's necessary to deal with the slightly more general case
             where the test expression results in a constant (e.g. "if not false"
             or "if (print "something"; true)" ).  There was a bug in the case
             where the expression resulted in "true" since "lab" becomes "noJump"
             if the jump is never taken.  "fixup" leaves "exited" as true so no
             code is generated for the else-part but it doesn't set the pseudo-stack
             properly which can cause problems while processing the else-part.
             DCJM 27 June 2000. *)
        in
            if isEmptyLabel lab
            then
            ( (* Only the "then" part will be executed.  Don't generate the else-part. *)
                unmarkStack(transtable, mark);
                gencde (thenExp, true, whereto, tailKind, matchFailFn, loopAddr)
            )
            else if haveExited transtable
            then
            ( (* Jump was unconditional - just generate the else-part. *)
                unmarkStack(transtable, mark);
                codeGenerate(fixup (lab, transtable), cvec);
                gencde (elseExp, true, whereto, tailKind, matchFailFn, loopAddr)
            )
            else
            let
                (* Generate the then-part and see where the result is.  We need it in a
                   register but we don't want to decide in advance which register to use.
                   In particular, if the result is in a floating point register we don't
                   want to move it to a general register. *)
                val (thenResult, whereto) =
                    case (whereto, tailKind) of
                        (NoHint, NotEnd) =>
                        let
                            (* We don't have any preferences. *)
                            val initialThenResult =
                                gencde(thenExp, true, whereto, tailKind, matchFailFn, loopAddr)
                        in
                            if haveExited transtable (* If we've raised an exception we can ignore this. *)
                            then (initialThenResult, NoHint)
                            else case initialThenResult of
                                MergeIndex res =>
                                let
                                    (* Is it in a register?  Merging requires exclusive use
                                       of the result register and it may be that this register
                                       is required elsewhere.  Use it as a hint for the register
                                       type we require and then load it.  If it's not required
                                       elsewhere this will just return the register it's in. *)
                                    val regSet =
                                        case isRegister(res, transtable) of
                                            SOME reg =>
                                                if inSet(reg, floatingPtRegisters)
                                                then floatingPtRegisters
                                                else generalRegisters
                                        |   NONE => generalRegisters
                                    val (_, mergeItem, mergeCode) = loadEntryToSet (transtable, res, regSet, true)
                                    val () = codeGenerate(mergeCode, cvec)
                                in
                                    (MergeIndex mergeItem,
                                        UseReg(singleton(valOf(isRegister(mergeItem, transtable)))))
                                end
                            |   NoMerge => raise InternalError "genCond: no result"
                        end
                    |   (_, EndOfProc res) =>
                        let
                            (* We want the result in the result reg. *)
                            val whereto = UseReg(singleton res)
                        in
                            (genToRegister (thenExp, whereto, tailKind, matchFailFn, loopAddr), whereto)
                        end
                    |   (whereto, _) => (* No result or we have a specific register. *)
                            (genToRegister (thenExp, whereto, tailKind, matchFailFn, loopAddr), whereto)

                val () = 
                    if isEndOfProc tailKind andalso not (haveExited transtable)
                    then codeGenerate(exit(), cvec)
                    else ()
              
                val (lab1, branchCode) = unconditionalBranch (thenResult, transtable)
                val () = codeGenerate(branchCode, cvec)
            
                (* Get rid of the result from the stack. If there is a result
                   then the "else-part" will push it. *)
                val () =
                    case thenResult of
                        MergeIndex thenIndex => codeGenerate(removeStackEntry(transtable, thenIndex), cvec)
                      | NoMerge => ()
              
                (* start of "else part" *)
                val () = codeGenerate(fixup (lab, transtable), cvec);
                val elseResult =
                    case whereto of
                        NoHint => (* Only if the then-part raised an exception *)
                            gencde(elseExp, true, whereto, tailKind, matchFailFn, loopAddr)
                    |   _ => genToRegister (elseExp, whereto, tailKind, matchFailFn, loopAddr)
                val (mergeRes, mergeCode) = merge (lab1, transtable, elseResult, mark)
                val () = codeGenerate(mergeCode, cvec)
            in 
                mergeRes
            end
        end (* genCond *)

        (* Call a function. Detects special cases of calls to the run-time system
           to do simple operations such as int arithmetic and generates the
           instructions directly. For ordinary calls it has to distinguish between
           those called with a static-link and those called with a closure. *) 
        and genEval (evalFun, argList: (codetree * argumentType) list, resultType, primBoolOps, whereto, tailKind, matchFailFn) : mergeResult =
        let
            (* This is used if we have a real function call rather than an RTS function we can
               code-generate directly. *)
            fun realFunctionCall(procLocn, modifiedRegisters, loadProc, argLocations, canTail) =
            let
                val needsResult = not (isNoResult whereto)
                val regResult = resultReg(codeToCgType resultType)
                (* Can use a jump if we're at the end, not in a local static-link function and
                   and the result is in the right register (we don't need to convert floating point
                   to fixed point or vice versa). *)
                val isTail =
                    case tailKind of
                        EndOfProc reg => canTail andalso regResult = reg
                    |   _ => false

                (* Code-generate each entry to the pstack.  If this is a tail recursive call we have to
                   load any values that are currently used as arguments because we may overwrite them later. *)
                local
                    fun loadArg((arg, _), argLocn) =
                    let
                        val originalLocn =
                            case argLocn of
                                SOME argReg =>
                                let (* Put into a register. *)
                                  (* If we are evaluating an expression we might as well put the
                                     result in the register we want to use. They may not stay
                                     there because loading other arguments may involve function
                                     calls which will use these registers. For that reason we
                                     don't put constants in yet. *)
                                  val whereto = case arg of Constnt _ => NoHint | _ => UseReg(singleton argReg)
                                in
                                    case gencde (arg, true, whereto, NotEnd, matchFailFn, NONE) of
                                        MergeIndex index => index
                                     |  NoMerge => raise InternalError "ldArgs: No result"
                                end
                            |   NONE => (* On the stack *) genToStack (arg, matchFailFn)
                    in
                        if isTail
                        then
                        let
                            val (safeLocn, safeCode)= loadIfArg (transtable, originalLocn)
                            val () = codeGenerate(safeCode, cvec)
                        in
                            safeLocn
                        end
                        else originalLocn
                    end
                in
                    val argsOnPstack = ListPair.mapEq loadArg(argList, argLocations)
                end
            in
                if isTail
                then  (* Enter a procedure by jumping rather than calling. *)
                let
                    (* Now move the arguments to their final destination. argAddr is a negative value and
                       is the address of the arguments in the original stack. *)
                    fun moveArgs ([], [], _) = []
                    |   moveArgs (arg::args, SOME argReg :: argTypes, argAddr) =
                        let
                            (* Do it in reverse order so that we can delay locking the register arguments. *)
                            val argEntries = moveArgs(args, argTypes, argAddr)
                            val (argEntry, argCode) = loadToSpecificReg (transtable, argReg, arg, false);
                        in
                            codeGenerate(argCode, cvec);
                            lockRegister (transtable, argReg);
                            argEntry :: argEntries
                        end
                    |   moveArgs (arg::args, NONE :: argTypes, argAddr) =
                        let
                            (* Store it in the stack, reloading anything it displaces. *)
                            val (argEntry, argCode) = storeInStack(transtable, arg, argAddr)
                            val () = codeGenerate(argCode, cvec)
                        in
                            argEntry :: moveArgs(args, argTypes, argAddr+1)
                        end
                    |   moveArgs _ = raise InternalError "moveArgs: Length mismatch"

                    (* the arguments are now all in their rightful places *)
                    val argEntries = moveArgs(argsOnPstack, argLocations, ~numberOfArgsOnStack)
    
                    (* Now load regClosure as appropriate. We delay this
                       until now, because we don't want to zap regCode before
                       we've loaded all the constant arguments. *)
                    val (codeAddrOpt, isIndirect, callEntries, registersLocked) = loadProc ()
                    (* Compute the number of stack arguments we're passing. *)
                    val stackArgCount = List.length(List.filter(not o isSome) argLocations)
        
                    (* Get the return address. *)
                    val returnReg : reg option =
                    case regReturn of
                        NONE =>
                            (* The return address is on the stack.  Do we need to load it? *)
                            (* Only if we're passing a different number of arguments on
                               stack - this would change the offset of the return address. *)
                            if stackArgCount = numberOfArgsOnStack
                            then NONE (* Leave it there. *)
                            else
                            let
                                val (reg, regIndex, loadCode) =
                                    loadEntryToSet (transtable, returnAddress, RegSet.generalRegisters, false)
                                val () = codeGenerate(loadCode, cvec)
                            in
                                codeGenerate(removeStackEntry(transtable, regIndex), cvec);
                                SOME reg
                            end
                    |   SOME regReturn =>
                            let
                                (* Reload the return address into the return register. *)
                                val (regIndex, regCode) =
                                    loadToSpecificReg (transtable, regReturn, returnAddress, false);
                                val () = codeGenerate(regCode, cvec)
                            in
                                codeGenerate(removeStackEntry(transtable, regIndex), cvec);
                                SOME regReturn
                            end

                    local
                        (* Move the stack pointer if necessary. *)           
                        val diffInArgs = numberOfArgsOnStack - stackArgCount
                        (* One more "arg" if the return address is passed on the stack. *)
                        val adjust = case returnReg of NONE => 1 | SOME _ => 0
                    in
                        val stackMove= realstackptr transtable + diffInArgs - adjust
                    end
                in
                    codeGenerate(resetStack stackMove, cvec);
                    (* Call the function.  If it's not recursive we have to get the
                       entry point. *)
                    (* We have to include a stack check in this function to ensure that
                       it's interruptible even though a tail jump doesn't require any
                       more stack. *)
                    callsAFunction := true; (* Don't really need this for RTS calls. *)
                    case codeAddrOpt of
                        NONE =>
                           codeGenerate(jumpToFunction{callKind=Recursive, returnReg=returnReg}, cvec)
                    |   SOME codeAddr => 
                            codeGenerate(jumpToCode(codeAddr, isIndirect, returnReg, transtable), cvec);

                    (* Unlock any registers we locked. *)
                    List.app (fn r => codeGenerate(unlockRegister (transtable, r), cvec)) registersLocked;
                    (* Remove the arguments and code/closure registers. *)
                    List.app (fn index => codeGenerate(removeStackEntry(transtable, index), cvec))
                        (argEntries @ callEntries)
                    (* Since we've exited we don't need to clear the cache. *)
                end  (* jumpToProc *)

                else (* Call a function.  Used in cases when it's not tail-recursive. *)
                let
                    (* Save any values to the stack other than those that are being
                       used in this call.  Values in registers not modified by the
                       call are locked in their current registers. *)
                    val (lockedRegs, pushInstrs) =
                        pushNonArguments(transtable, procLocn @ argsOnPstack, modifiedRegisters);
                    val () = codeGenerate(pushInstrs, cvec)

                    (* Push the arguments onto the real stack and/or load them
                       into the argument registers. *)
                    (* Second phase of argument evaluation.  Push the values onto the real stack
                         or load them into the argument registers.  The result is the stack base
                         for stack arguments together with a list of pseudo-stack entries for
                         the arguments. *)
                    fun pushArgs (argList : stackIndex list) : int * stackIndex list =
                    let
                        fun ldArgs ([], stackAddr, []) = (stackAddr, [])
                        |   ldArgs (argLoc :: t, stackAddr, SOME argReg :: t') =
                            let (* Put into a register. *)
                                (* Load the first before putting these into the registers. *)
                                val (rAddr : int, others) = ldArgs(t, stackAddr, t');
                                val (regEntry, regCode) = loadToSpecificReg (transtable, argReg, argLoc, false);
                            in
                                codeGenerate(regCode, cvec);
                                lockRegister (transtable, argReg);
                                (rAddr, regEntry :: others)
                            end
                        |   ldArgs (argLoc::t, stackAddr : int, NONE :: t') =
                            let (* Store on the real stack. *)
                                (* We take the current stack pointer as the base for the stack args. *)
                                val sAddr : int = 
                                if stackAddr < 0 then realstackptr transtable else stackAddr;
                                val (pushedEntry, pushCode) = pushValueToStack (transtable, argLoc, sAddr + 1)
                                val () = codeGenerate(pushCode, cvec)
                                val (rAddr, others) = ldArgs(t, sAddr + 1, t')
                            in
                                (rAddr, pushedEntry :: others)
                            end (* ldArgs *)
                        |   ldArgs _ = raise InternalError "ldArgs: Length mismatch"
                    in
                        ldArgs(argList, ~1, argLocations)
                    end (* pushArgs *)

                    val (endOfArgs, argEntries) = pushArgs argsOnPstack
              
                    (* load regClosure *)
                    val (codeAddrOpt, isIndirect, codeEntries, regsLocked) = loadProc ();
          
                    val checkContiguous =
                      (* Make sure that the arguments are contiguous on the
                         stack and that there is nothing beyond them on it. *)
                      if endOfArgs >= 0 then resetButReload (transtable, endOfArgs) else []
                    (* Record that we've called a function. *)
                    val () = callsAFunction := true; 
                    val callCode =
                      case codeAddrOpt of
                         NONE => callFunction{callKind=Recursive}
                      |  SOME codeAddr => callCode(codeAddr, isIndirect, transtable)
                in
                    codeGenerate(callCode @ checkContiguous, cvec);

                    (* Unlock any registers we locked. *)
                    List.app (fn r => codeGenerate(unlockRegister (transtable, r), cvec)) (lockedRegs @ regsLocked);
                    (* Remove the arguments and code/closure registers. *)
                    List.app (fn index => codeGenerate(removeStackEntry(transtable, index), cvec))
                        (codeEntries @ argEntries);

                    (* Remove any registers from the cache which may have been modified
                       by the function. *)
                    codeGenerate(removeRegistersFromCache(transtable, modifiedRegisters), cvec)
                end (* callProc *);

                (* Set up the results of the function call. *)
                (* Unlock  the argument registers. *)
                List.app(fn SOME r => codeGenerate(unlockRegister (transtable, r), cvec) | NONE => ()) argLocations;

                (* Remove any stack arguments. For tail-calls, we mustn't do this, because
                   the stack arguments have already vanished from the pstack - that's
                   because storeInStack consumes its argument. Conversely, pushValueToStack,
                   used to set up stack arguments normal calls, does NOT consume its pstack
                   argument. This means that we mustn't call decsp for tail-calls, which
                   in turn means that the real stack pointer doesn't get correctly adjusted.
                   Fortunately, this is (just about) OK because this code is unreachable,
                   and CODEGEN_TABLE.mergeLab is clever enough to avoid merging unreachable states. 
                   This whole area is a right mess, which I must sort out some time.
                   SPF 13/3/97
                   Agreed.  I've tried to tidy this up a bit.  decsp now ONLY affects the
                   real stack pointer rather than popping items from the pstack as
                   well.  It still needs more work.
                   DCJM 25/11/99
                 *)
                if isTail then exiting transtable else ();

                List.app(fn SOME _ => () | NONE => decsp(transtable, 1)) argLocations;

                if not needsResult
                then NoMerge (* Unused *)
                else
                ( (* Result is returned in regResult. *)
                    codeGenerate(addRegUse (transtable, regResult), cvec); (* Needed? *)
                    MergeIndex(pushReg (transtable, regResult))
                )
            end (* realFunctionCall *)

            (* Call a closure function, i.e. not one that requires a static link. *)
            fun callClosure (clos : codetree option): mergeResult =
            let
                (* If we're actually calling the function where do the arguments go? *)
                val argLocations = argRegs (List.map (codeToCgType o #2)  argList)
                val modifiedArgRegs = List.map valOf (List.filter isSome argLocations)

                (* Get the set of registers modified by this call.  We have to include
                   the argument, closure and code registers even if they're not actually
                   modified because otherwise we may find that we've locked them. *)
                val registerSet =
                    case clos of
                        SOME (Constnt w) =>
                            regSetUnion(listToSet(regClosure :: modifiedArgRegs), getRegisterSetForFunction w)
                      | _ (* Recursive or not a constant. *) => allRegisters;

                (* Add the registers to the set modified by this function.
                   We don't need to do this for recursive calls.  In that
                   case we must push all the registers (so we set registerSet
                   to allRegisters) but the modification set for this function
                   is simply the registers modified by everything else. *)
                val _ =
                    case clos of
                        NONE => ()
                    |   _ => addModifiedRegSet(transtable, registerSet);

                (* Have to guarantee that the expression to return
                   the procedure is evaluated before the arguments. *)
                (* In the recursive case the use count for closureOrSlAddr
                   is set by the caller. DCJM 1/12/99. *)
                val procLocn = 
                  case clos of
                    SOME(Constnt _) => noIndex (* Unused. *)
                  | SOME c          => genToStack (c, matchFailFn) (* the closure *)
                  | NONE            => noIndex  (* Unused. *);

                local
                    fun loadReg reg addr : stackIndex =
                        let
                          (* We don't need exclusive use of this value, because it
                             only gets modified by the function call itself, not
                             here. We either don't return from the function
                             (tail-call: we set exited) or we explicitly clear
                             the cache in setUpResult. *)
                          val (regIndex, regCode) =
                                loadToSpecificReg(transtable, reg, addr, false (* was bodyCall *));
                        in
                            codeGenerate(regCode, cvec);
                          (* Lock the register down so that it doesn't get
                             used to move values onto the stack. *)
                          lockRegister (transtable, reg);
                          regIndex
                        end
                in
                    fun loadClosureProc (): (stackIndex option * bool * stackIndex list * reg list) =
                      case clos of
                         SOME(Constnt w) =>
                            (* Do we need to load the closure register? *)
                            let
                                val addr = toAddress w;
                            in
                                if isIoAddress addr
                                then (* We don't need the closure register but we can't
                                        do the indirection here.  That's because the
                                        code address isn't valid.  We have to do the
                                        indirection at run time. *)
                                    (SOME(pushConst(transtable, w)), true, [], [])
                                else
                                let
                                    val code : machineWord = loadWord (addr, 0w0)
                                    val codeLocn = pushConst(transtable, code)
                                in
                                    if objLength addr = 0w1
                                    then (* The closure is just one word - we don't need to
                                            put it in the closure register since the function
                                            won't need it.  Do the indirection now. *)
                                        (SOME codeLocn, false, [], [])
                                    else (* We need to load the closure register. 
                                        We have a choice here.  We could either return
                                        the closure register as the address as we do
                                        in the general case, in which case we would do
                                        an indirect call through the closure register,
                                        or we can do the indirection here and do a
                                        direct call.  On the i386 the latter is definitely
                                        better but on the PPC it will generate longer
                                        code, although possibly no slower if there was
                                        a pipeline stall. *)
                                        (SOME codeLocn, false,
                                            [loadReg regClosure (pushConst(transtable, w))],
                                            [regClosure])
                                end
                            end
                       | SOME _ =>
                            (* Calling a non-constant - load the closure register and
                               set the code address as this with the "indirection"
                               flag set to true. *)
                            (SOME(loadReg regClosure procLocn), true, [], [regClosure])
                       | NONE => (* Recursive *)
                          (* If this function requires a closure we need to reload
                             the closure register with our original closure. *)
                          if closureLifetime = 0 then (NONE, false, [], [])
                          else (NONE, false, [loadReg regClosure closureOrSlAddr], [regClosure])
                end
            in
                realFunctionCall([procLocn], registerSet, loadClosureProc, argLocations, true)
            end (* callClosure *)

            fun codeRTSFunction(instr, arguments, whereto) =
                case checkAndReduce(instr, arguments, fn (Constnt w) => SOME w | _ => NONE) of
                    SOME(i, args) =>
                    let
                        (* Code generate each argument to the pstack. *)
                        val argLocns = List.map (fn arg => genToStack (arg, matchFailFn)) args
                        val (opRes, opCode) = dataOp (argLocns, i, transtable, whereto)
                        val () = codeGenerate(opCode, cvec)
                    in
                        (* Put in the result. *)
                        case whereto of
                            NoResult => NoMerge (* Unused. *)
                        |   _ => MergeIndex opRes
                    end
                |   NONE => (* Have to use a function call *) callClosure (SOME evalFun)

        in (* body of genEval *)
            case evalFun of
                Constnt (oper : machineWord) =>
                let
                    val args = List.map #1 argList
                    val addr = toAddress oper
                in
                    if isIoAddress addr
                    then
                    (
                        if wordEq (oper,ioOp POLY_SYS_thread_self)
                        then codeRTSFunction(instrThreadSelf, args, whereto)

                        else if wordEq (oper,ioOp POLY_SYS_get_length)
                        then codeRTSFunction(instrVeclen, args, whereto)

                        else if wordEq (oper,ioOp POLY_SYS_get_flags)
                        then codeRTSFunction(instrVecflags, args, whereto)

                        else if wordEq (oper,ioOp POLY_SYS_get_first_long_word)
                        then codeRTSFunction(instrGetFirstLong, args, whereto)

                        else if wordEq (oper,ioOp POLY_SYS_string_length)
                        then codeRTSFunction(instrStringLength, args, whereto)

                        else if wordEq (oper,ioOp POLY_SYS_set_string_length)
                        then codeRTSFunction(instrSetStringLength, args, whereto)

                        else if wordEq (oper,ioOp POLY_SYS_aplus)
                        then codeRTSFunction(instrAddA, args, whereto)

                        else if wordEq (oper,ioOp POLY_SYS_aminus)
                        then codeRTSFunction(instrSubA, args, whereto)

                        else if wordEq (oper,ioOp POLY_SYS_amul)
                        then codeRTSFunction(instrMulA, args, whereto)

                        (* Negation is coded as 0 - X. *)
                        else if wordEq (oper,ioOp POLY_SYS_aneg)
                        then codeRTSFunction(instrSubA, Constnt Zero :: args, whereto)

                        (* Boolean "not" is coded as xor with "true" *)
                        else if wordEq (oper,ioOp POLY_SYS_not_bool)
                        then codeRTSFunction(instrXorW, args @ [Constnt True], whereto)

                        else if  wordEq (oper,ioOp POLY_SYS_or_word)
                        then codeRTSFunction(instrOrW, args, whereto)

                        else if  wordEq (oper,ioOp POLY_SYS_and_word)
                        then codeRTSFunction(instrAndW, args, whereto)

                        else if wordEq (oper,ioOp POLY_SYS_xor_word)
                        then codeRTSFunction(instrXorW, args, whereto)
        
                        else if wordEq (oper,ioOp POLY_SYS_shift_left_word)
                        then codeRTSFunction(instrUpshiftW, args, whereto)
        
                        else if wordEq (oper,ioOp POLY_SYS_shift_right_word)
                        then codeRTSFunction(instrDownshiftW, args, whereto)
        
                        else if wordEq (oper,ioOp POLY_SYS_shift_right_arith_word)
                        then codeRTSFunction(instrDownshiftArithW, args, whereto)
        
                        else if wordEq (oper,ioOp POLY_SYS_xor_word)
                        then codeRTSFunction(instrXorW, args, whereto)
        
                        else if wordEq (oper,ioOp POLY_SYS_mul_word)
                        then codeRTSFunction(instrMulW, args, whereto)
        
                        else if wordEq (oper,ioOp POLY_SYS_plus_word)
                        then codeRTSFunction(instrAddW, args, whereto)
        
                        else if wordEq (oper,ioOp POLY_SYS_minus_word)
                        then codeRTSFunction(instrSubW, args, whereto)
        
                        else if wordEq (oper,ioOp POLY_SYS_div_word)
                        then codeRTSFunction(instrDivW, args, whereto)
        
                        else if wordEq (oper,ioOp POLY_SYS_mod_word)
                        then codeRTSFunction(instrModW, args, whereto)
        
                        else if wordEq (oper,ioOp POLY_SYS_load_byte)
                        then codeRTSFunction(instrLoadB, args, whereto)

                        else if wordEq (oper,ioOp POLY_SYS_load_word)
                        then codeRTSFunction(instrLoad, args, whereto)

                        else if wordEq (oper,ioOp POLY_SYS_atomic_incr)
                        then codeRTSFunction(instrAtomicIncr, args, whereto)

                        else if wordEq (oper,ioOp POLY_SYS_atomic_decr)
                        then codeRTSFunction(instrAtomicDecr, args, whereto)

                        else if wordEq (oper,ioOp POLY_SYS_alloc_store)
                        then codeRTSFunction(instrAllocStore, args, whereto)

                        else if wordEq (oper,ioOp POLY_SYS_assign_word)
                        then codeRTSFunction(instrStoreW, args, whereto)

                        else if wordEq (oper,ioOp POLY_SYS_assign_byte)
                        then codeRTSFunction(instrStoreB, args, whereto)
                    
                        else if wordEq(oper, ioOp POLY_SYS_lockseg)
                        then codeRTSFunction(instrLockSeg, args, whereto)

                        else if wordEq (oper,ioOp POLY_SYS_Add_real)
                        then codeRTSFunction(instrAddFP, args, whereto)

                        else if wordEq (oper,ioOp POLY_SYS_Sub_real)
                        then codeRTSFunction(instrSubFP, args, whereto)

                        else if wordEq (oper,ioOp POLY_SYS_Mul_real)
                        then codeRTSFunction(instrMulFP, args, whereto)

                        else if wordEq (oper,ioOp POLY_SYS_Div_real)
                        then codeRTSFunction(instrDivFP, args, whereto)

                        else if wordEq (oper,ioOp POLY_SYS_Neg_real)
                        then codeRTSFunction(instrNegFP, args, whereto)

                        else if wordEq (oper,ioOp POLY_SYS_int_to_real)
                        then codeRTSFunction(instrIntToRealFP, args, whereto)

                        else if wordEq (oper,ioOp POLY_SYS_real_to_int)
                        then codeRTSFunction(instrRealToIntFP, args, whereto)

                        else if wordEq (oper,ioOp POLY_SYS_sqrt_real)
                        then codeRTSFunction(instrSqrtFP, args, whereto)

                        else if wordEq (oper,ioOp POLY_SYS_sin_real)
                        then codeRTSFunction(instrSinFP, args, whereto)

                        else if wordEq (oper,ioOp POLY_SYS_cos_real)
                        then codeRTSFunction(instrCosFP, args, whereto)

                        else if wordEq (oper,ioOp POLY_SYS_arctan_real)
                        then codeRTSFunction(instrAtanFP, args, whereto)

                        else if wordEq (oper,ioOp POLY_SYS_exp_real)
                        then codeRTSFunction(instrExpFP, args, whereto)

                        else if wordEq (oper,ioOp POLY_SYS_ln_real)
                        then codeRTSFunction(instrLnFP, args, whereto)

                        else if wordEq (oper,ioOp POLY_SYS_move_bytes)
                        then codeRTSFunction(instrMoveBytes, args, whereto)

                        else if wordEq (oper,ioOp POLY_SYS_move_words)
                        then codeRTSFunction(instrMoveWords, args, whereto)

                        (* The point of the following code is to call genCond, which will call genTest
                           which will hopefully use machine instructions for these operations.
                           We could avoid this by duplicating most of the body of genTest
                           (the "jumping" boolean code generator) here, but we would like to
                           avoid that. *)
                        else if primBoolOps andalso
                        (wordEq (oper,ioOp POLY_SYS_word_eq)  orelse
                         wordEq (oper,ioOp POLY_SYS_word_neq) orelse
                         wordEq (oper,ioOp POLY_SYS_equala)   orelse
                         wordEq (oper,ioOp POLY_SYS_int_geq)  orelse
                         wordEq (oper,ioOp POLY_SYS_int_leq)  orelse
                         wordEq (oper,ioOp POLY_SYS_int_gtr)  orelse
                         wordEq (oper,ioOp POLY_SYS_int_lss)  orelse
                         wordEq (oper,ioOp POLY_SYS_word_geq)  orelse
                         wordEq (oper,ioOp POLY_SYS_word_leq)  orelse
                         wordEq (oper,ioOp POLY_SYS_word_gtr)  orelse
                         wordEq (oper,ioOp POLY_SYS_word_lss) orelse
                         wordEq (oper,ioOp POLY_SYS_Real_eq) orelse
                         wordEq (oper,ioOp POLY_SYS_Real_neq) orelse
                         wordEq (oper,ioOp POLY_SYS_Real_geq) orelse
                         wordEq (oper,ioOp POLY_SYS_Real_leq) orelse
                         wordEq (oper,ioOp POLY_SYS_Real_gtr) orelse
                         wordEq (oper,ioOp POLY_SYS_Real_lss) orelse
                         wordEq (oper,ioOp POLY_SYS_is_short) orelse
                         wordEq (oper,ioOp POLY_SYS_bytevec_eq))
                        then
                            genCond
                                (Eval {function = evalFun, argList = argList, earlyEval = false, resultType=resultType},
                                 constntTrue, constntFalse, whereto, tailKind, matchFailFn, NONE)
                        else (* unoptimised I/O call *)
                            callClosure (SOME evalFun)
                    )
  
                    else (* All other constant functions. *) callClosure (SOME evalFun)
                end

            |   Extract {fpRel, addr, lastRef, ...} =>
                let (* Local function with non-empty closure. *)
                    (* The procedure is being loaded from the stack or closure
                       so it may be a static-link procedure. *)

                    (* DCJM 1/12/99.  TODO: We have a problem if the function is a
                     closure reference (e.g. a recursive call) and the last reference
                     to the closure is one of the arguments.  In some cases at any rate
                     we don't process the function until after we've processed the
                     arguments resulting in us not being able to find something.
                     I think it's probably only if we have static-linked functions
                     that there is a problem when the something is start of the static
                     link.  I think if the function requires a closure then the closure
                     is evaluated first even if it's a simple load. 
                     No, it's also needed in the recursive case at least. 
         
                     There was a definite bug in the case of a static-link call to
                     a function where the last reference to the function was within
                     the argument.  DCJM 21/12/00.
                     *)

                    val selfCall = not fpRel andalso addr = 0 
      
        ;            val staticCall = 
                        if fpRel (* Local or parameter *)
                        then addr > 0 (* If not it must be a parameter - must be closure *)
                        andalso isSome(Array.sub(staticLinkEntries, addr)) (* local - look in table *)
                        else isStaticLink addr  (* Non-local or recursive. *)
                in 
                     (* Is this a static link call? *)
                    if staticCall
                    then
                    let  
                        (* Load and lock regClosure. Returns the indexes of
                           these entries in the stack. *)
                        fun loadStaticLinkProc (): (stackIndex option * bool * stackIndex list * reg list) =
                        if selfCall (* recursive *)
                        then
                        let
                            (* Do we really need *exclusive* use of this register? 
                               Perhaps this is to force the old value onto the stack if
                               we haven't saved it yet, but we should have done this
                               in pushAllBut (below) unless we're in a tail-call in
                               which case we're not coming back. SPF 23/5/95 *)
                            val SL = closureOrSlAddr
                            val (closureIndex, closureCode) =
                              loadToSpecificReg (transtable, regClosure, SL, false);
                            val () = codeGenerate(closureCode, cvec)
                            val () = lockRegister (transtable, regClosure);
                        in
                            (NONE, false, [closureIndex], [regClosure])
                        end
                 
                        else if fpRel (* Local *)
                        then
                        let
                            (* Load entry point - this must be a local not an argument. *)
                            val entryPt = Array.sub(decToPstack, addr);
                            (* We have already incremented the reference count when
                               this is not the last reference so we don't need to do
                               anything here.  DCJM 21/12/00. *)
            
                            (* Get the static link register. Will set its value later. *)
                            val () = codeGenerate(getRegister (transtable, regClosure), cvec);
                            val closureIndex = pushReg (transtable, regClosure);
                            val () = lockRegister (transtable, regClosure);
                        in 
                            (* Set value of static link register now. The static link entry
                               is now the address of the frame.  DCJM 2/1/01. *)
                            codeGenerate(
                                moveStackAddress(realstackptr transtable - 1, regClosure),
                                cvec);
                            (SOME entryPt, false, [closureIndex], [regClosure])
                        end
          
                        else
                        let (* Non-local or recursive. *)
                            fun pushIt () = (closureOrSlAddr, [])
                            (* load SL register and return code address *)
                            val (entryPt, slIndex, code) = loadStaticLink (addr, pushIt, transtable)
                            val () = codeGenerate(code, cvec)
                        in
                            lockRegister (transtable, regClosure);
                            (SOME entryPt, false, [slIndex], [regClosure])
                        end (* loadStaticLinkProc *)

                        (* If we're actually calling the function where do the arguments go? *)
                        val argLocations = argRegs (List.map (codeToCgType o #2)  argList)

                        local
                            val modifiedArgRegs = List.map valOf (List.filter isSome argLocations)
                            val argRegs = listToSet(regClosure :: modifiedArgRegs)
                        in
                            val registerSet =
                                if selfCall then allRegisters (* Have to push everything. *)
                                else if fpRel (* Local *)
                                then if addr < 0
                                then raise InternalError "static link function is an argument"
                                else regSetUnion(argRegs,
                                        getFunctionRegSet(Array.sub(decToPstack, addr), transtable))
                                else (* Non local *) regSetUnion(argRegs, staticLinkRegSet addr)
                        end
                        (* Add the registers modified by the function we're calling to those
                           modified by this function.  Don't need to do that in the recursive
                           case. *)
                        val () =
                            if selfCall then ()
                            else addModifiedRegSet(transtable, registerSet)
                        (* Set the use count on the static link.  I don't know whether this
                           is needed in all cases so it could result in us incrementing it
                           unnecessarily.  It isn't needed if this function is local.
                           Increment the reference on the code if it's local and this is
                           not the last reference.  If it's non-local we don't actually
                           change the reference count when we load it. *)
                        val () =
                            if lastRef
                            then ()
                            else if fpRel
                            then codeGenerate(incrUseCount(transtable, Array.sub(decToPstack, addr), 1), cvec)
                            else (* Non-local *) codeGenerate(incrUseCount(transtable, closureOrSlAddr, 1), cvec)
                        (* Cannot use a jump to local static-link procedures because 
                           we may want local declarations on the current stack. *)
                        val result =
                            realFunctionCall([], registerSet, loadStaticLinkProc, argLocations, not fpRel);
                        (* If this is the last reference we can decrement the uses on
                           all the entries in the "closure".  We need to do that last in case the
                           entries are also used as parameters and accidentaly get popped. *)
                    in
                        if fpRel andalso lastRef
                        then codeGenerate(valOf(Array.sub(staticLinkEntries, addr)) (), cvec)
                        else ();
                        result
                    end
   
                    (* Closure call - check for recursive calls. *)
                    else
                    (
                        (* Set the use count on the closure register if this is a
                           recursive call.  We have to do that for the recursive case
                           because we don't pass the Extract entry in to callClosure.
                           DCJM 1/12/99. *)
                        if selfCall andalso not lastRef andalso closureLifetime <>0 
                        then codeGenerate(incrUseCount(transtable, closureOrSlAddr, 1), cvec)
                        else ();
                        callClosure (if selfCall then NONE else SOME evalFun)
                    )
                end (* Extract *)

            |   _ => (* The procedure is not being found by simply loading a value
                        from the stack or the closure and is not a constant. *)
                    callClosure (SOME evalFun)
        end (* genEval *)


        val resReg = resultReg(codeToCgType resultType)
        val _ = genToRegister (pt, UseReg(singleton resReg), EndOfProc resReg, matchFailed, NONE);

        val () = if not (haveExited transtable) then codeGenerate(exit (), cvec) else ()
    in
    
        (* Having code generated the body of the procedure,
          it is copied into a new data segment. *)
        (!cvec, maxstack transtable, getModifedRegSet transtable, !callsAFunction)
    end (* codegen *)

    fun gencode (Lambda { name, body, argTypes, resultType, argLifetimes, localCount, ...}, debugSwitches, _) =
        let (* We are compiling a procedure. *)
            (* It is not essential to treat this specially, but it saves generating
             a piece of code whose only function is to return the address of the
             procedure. *)
       
            (* make the code buffer for the new procedure. *)
            val newCode = codeCreate (false (* don't make a closure *), name, debugSwitches); 

         (* The only non-local references will be references to the
            closure itself. We have to fetch these from the constants
            section because:
            (1) we don't save the closure register in the function body
            (2) we don't even initialise it if we use the PureCode
            calling convention
            SPF 2/1/97
          *)
            val (ops, maxStack, regList, callsAFunction) =
                codegen
                    (body,
                    fn (_ , _, newtab) => (pushCodeRef (newtab, newCode), []),
                    fn _ => false,
                    fn (_,  _, _) => raise InternalError "Not static link",
                    fn _ => raise InternalError "Not static link",
                    0, (* Discard regClosure *)
                    argTypes, argLifetimes, resultType, localCount, debugSwitches)

            val closureAddr = copyCode (newCode, ops, maxStack, regList, callsAFunction)
        in
            (* Result is a procedure which returns the address of the procedure. *)
            (fn () => toMachineWord closureAddr)
        end
 
    |   gencode (pt, debugSwitches, localCount) =
        let (* Compile a top-level expression. *)
            val newCode = codeCreate (false (* make a closure *), "<top level>", debugSwitches);

            (* There should be *no* non-local references. *)
            val (ops, maxStack, regList, callsAFunction) =
                codegen 
                    (pt,
                    fn _ => raise InternalError "top level reached",
                    fn _ => false,
                    fn _ => raise InternalError "Not static link",
                    fn _ => raise InternalError "Not static link",
                    0,  (* Discard regClosure *)
                    [], [], (* No args. *) GeneralType, (* General result (if any?) *)
                    localCount,
                    debugSwitches)
            val closureAddr = copyCode (newCode, ops, maxStack, regList, callsAFunction)
        in (* Result is a procedure to execute the code. *)
            fn () => call (closureAddr, toMachineWord ())
        end (* gencode *)

end; (* GCODE functor body *)
