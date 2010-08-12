(*
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

(*
    Title:      General purpose code generator.
    Author:     Dave Matthews, Edinburgh University / Prolingua Ltd.
    Copyright   D.C.J. Matthews 1991
*)

(* Code is generated into a vector which is handled largely by the
   "codecons" module, and then copied into a vector of the correct size when
   complete.
   There are two kinds of linkage conventions according to whether a procedure
   requires a closure or only a static link. Procedures with no non-local
   references are considered as closure calls but are optimised by combining
   the closure into the code itself.

  Note that the garbage collector assumes that all registers contain either
  numbers (tagged) or valid addresses (the word before contains
  length and flags).
*)


functor G_CODE (

(*****************************************************************************)
(*                  CODECONS                                                 *)
(*****************************************************************************)
structure CODECONS :
sig
  type machineWord = Address.machineWord;
  type short = Word.word;
  type address = Address.address;
  type addrs; (* NB this is *not* the same as "address" *)
  type code;
  type reg;   (* Machine registers *)
  datatype storeWidth = STORE_WORD | STORE_BYTE
       
  val regNone:     reg;
  val regResult:   reg;
  val regClosure:  reg;
  val regStackPtr: reg;
  val regHandler:  reg;
  val regReturn:   reg;
  
  val argReg:  int -> reg;
  val argRegs: int;     (* No of args in registers. *)
  
  val regEq  : reg * reg -> bool
  val regNeq : reg * reg -> bool

  val codeCreate: bool * string * Universal.universal list -> code;  (* create - Makes the initial segment. *)
  val copyCode:   code * int * reg list -> address;

  val resetStack: int * code -> unit; (* Set a pending reset *)

  (* Comparison operations. *)
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
  
  val isCompRR: tests -> bool; (* Is the general form implemented? *)
  val isCompRI: tests * machineWord -> bool; (* Is the immediate value ok? *)

  (* Binary operations. veclen isn't binary but it makes things easier. *)
  type instrs;
  
  val instrMove:    instrs;
  val instrAddA:    instrs;
  val instrSubA:    instrs;
  val instrRevSubA: instrs;
  val instrMulA:    instrs;
  val instrOrW:     instrs;
  val instrAndW:    instrs;
  val instrXorW:    instrs;
  val instrAddW:    instrs;
  val instrSubW:    instrs;
  val instrRevSubW: instrs;
  val instrMulW:    instrs;
  val instrDivW:    instrs;
  val instrModW:    instrs;
  val instrLoad:    instrs;
  val instrLoadB:   instrs;
  val instrVeclen:  instrs;
  val instrVecflags:   instrs;
  val instrUpshiftW:   instrs;
  val instrDownshiftW: instrs;
  val instrDownshiftArithW: instrs;
  val instrGetFirstLong:    instrs;
  val instrStringLength: instrs;
  val instrSetStringLength: instrs;
  val instrBad:     instrs;

  val instrIsRR: instrs -> bool; (* Is the general form implemented? *)
  val instrIsRI: instrs * machineWord -> bool; (* Is the immediate value ok? *)
  
  val genRR: instrs * reg * reg * reg * code -> unit;

  val genLoad:        int * reg * reg * code -> unit;
  val genPush:        reg * code -> unit;

 (* Store allocation. *)
  val allocStore:      int  * Word8.word * reg * code -> unit;
  val setFlag:         reg * code * Word8.word -> unit;
  val completeSegment: code -> unit;

   (* Backward jumps. *)
  val ic: code -> addrs;

  (* Function call and linkage. *)
  datatype callKinds =
        Recursive
    |   ConstantFun of machineWord * bool
    |   CodeFun of code
    |   FullCall
  
  val callFunction:       callKinds * code -> unit;
  val jumpToFunction:     callKinds * reg * code -> unit;
  val returnFromFunction: reg * int * code -> unit;
  val genStackOffset:     reg * int * code -> unit;
  val raiseException:     code -> unit;
  
  type cases
  type jumpTableAddrs
  val constrCases : int * addrs -> cases;
  val useIndexedCase: int * int * int * bool -> bool;
  val indexedCase : reg * reg * int * int * bool * code -> jumpTableAddrs;
  val makeJumpTable : jumpTableAddrs * cases list * addrs * int * int * code -> unit;
  val inlineAssignments: bool;
  val isIndexedStore: storeWidth ->bool
  val traceContext: code -> string
end (* CODECONS *);


(*****************************************************************************)
(*                  TRANSTAB                                                 *)
(*****************************************************************************)
structure TRANSTAB :
sig
  type machineWord = Address.machineWord;
  type ttab;
  type reg;
  type code;
  type tests;
  type instrs;
  type addrs;
  type storeWidth;
  type regSet;
  
  val ttabCreate: Universal.universal list -> ttab;
  
  (* Register allocation *)
  val getRegister:    ttab * code * reg -> unit;
  val getAnyRegister: ttab * code -> reg;
  val freeRegister:   ttab * reg -> unit;
  val clearCache:     ttab -> unit;
  val removeRegistersFromCache: ttab * regSet -> unit;

  (* Stack handling *)
  type stackIndex;
  
  val noIndex: stackIndex;   
  
  (* Push entries *)
  val pushReg:      ttab * reg -> stackIndex;
  val pushStack:    ttab * int  -> stackIndex;
  val pushConst:    ttab * machineWord -> stackIndex;
  val pushCodeRef:  ttab * code -> stackIndex;
  val pushNonLocal: ttab * ttab * stackIndex * 
                       (unit -> stackIndex) * code -> stackIndex;
  val incsp:        ttab -> stackIndex;
  val decsp:        ttab * int -> unit;
  val pushAll:      ttab * code -> unit;
  val pushAllBut:   ttab * code * ((stackIndex -> unit) -> unit) * regSet -> unit;
  val pushNonArguments: ttab * code * stackIndex list * regSet -> reg list;
  val pushSpecificEntry: ttab * code * stackIndex -> unit;
  val reserveStackSpace: ttab * code * int -> stackIndex;

  (* Code entries *)
  val loadToSpecificReg: code * ttab * reg * stackIndex * bool -> stackIndex;
  val containsLocal:     ttab * reg  -> unit;
  val loadEntry:         code * ttab * stackIndex * bool -> reg*stackIndex;
  val lockRegister:      ttab * reg -> unit;
  val unlockRegister:    ttab * reg -> unit;
  val loadIfArg:         code * ttab * stackIndex -> stackIndex
  val indirect:          int * stackIndex * code * ttab -> stackIndex;
  val moveToVec:         stackIndex * stackIndex * int * storeWidth * code * ttab -> unit;

  val removeStackEntry: ttab*stackIndex -> unit;

  val resetButReload:   code * ttab * int -> unit;
  val pushValueToStack: code * ttab * stackIndex * int -> stackIndex;
  val storeInStack:     code * ttab * stackIndex * int -> unit;
  val isProcB:          ttab * int  -> bool;
  val pstackForDec:     ttab * int  -> stackIndex;
  val realstackptr:     ttab -> int;
  val maxstack:         ttab -> int;
  val makeEntry:        ttab * code * stackIndex * int * int * bool -> unit;
  val incrUseCount:     ttab * stackIndex * int -> unit;
  
(* ...
  (* for debugging *)
  val printStack:       ttab -> string -> string -> unit;
... *)

  type stackMark;
   
  val markStack: ttab -> stackMark;
  val unmarkStack: ttab * stackMark -> unit;
  
  type labels;

  val noJump: labels;
  val isEmptyLabel: labels -> bool
  
  datatype mergeResult = NoMerge | MergeIndex of stackIndex;
  val unconditionalBranch: mergeResult * ttab * code -> labels;
  val jumpBack: addrs * ttab * code -> unit;
  
  val fixup: labels * ttab * code -> unit;
  val merge: labels * ttab * code * mergeResult * stackMark -> mergeResult;
  val mergeList: labels list * ttab * code * mergeResult * stackMark -> mergeResult;
   
  type handler;

  val pushAddress: ttab * code * int -> handler;
  val fixupH:      handler * int * ttab * code -> unit;

  val exiting: ttab -> unit;
  val haveExited: ttab -> bool;

  datatype regHint = UseReg of reg | NoHint;
  val binaryOp: stackIndex * stackIndex * instrs * instrs * ttab * code * regHint -> stackIndex;
  val assignOp: stackIndex * stackIndex * stackIndex * storeWidth * ttab * code -> unit;
  val compareAndBranch: stackIndex * stackIndex * tests * tests * ttab * code -> labels;

  type savedState;
  val saveState : ttab * code -> savedState
  val startCase : ttab * code * savedState -> addrs
  
  val chooseRegister : ttab -> reg
  val addRegUse : ttab * reg -> unit

  val getRegisterSet: machineWord -> regSet
  val allRegisters : regSet
  val regSetUnion: regSet * regSet -> regSet
  val listToSet: reg list -> regSet
  val getFunctionRegSet: stackIndex * ttab -> regSet
  val addModifiedRegSet: ttab * regSet -> unit

  val getModifedRegSet: ttab -> reg list

  datatype argdest = ArgToRegister of reg | ArgToStack of int | ArgDiscard
  val getLoopDestinations: stackIndex list * ttab -> argdest list

  val callCode: stackIndex * bool * ttab * code -> unit
  val jumpToCode: stackIndex * bool * reg * ttab * code -> unit

end (* TRANSTAB *);

structure BASECODETREE: BaseCodeTreeSig

(*****************************************************************************)
(*                  GCODE sharing constraints                                *)
(*****************************************************************************)
sharing type
  CODECONS.code
= TRANSTAB.code
  
sharing type
  CODECONS.instrs
= TRANSTAB.instrs
  
sharing type
  CODECONS.reg
= TRANSTAB.reg
  
sharing type
  CODECONS.tests
= TRANSTAB.tests 
  
sharing type
  CODECONS.addrs
= TRANSTAB.addrs  

sharing type
  CODECONS.storeWidth
= TRANSTAB.storeWidth

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
  open TRANSTAB;
  open Address;
  open Misc; (* after address, so we get Misc.length, not Address.length *)
  open RuntimeCalls; (* for POLY_SYS numbers *)
  open BASECODETREE;
  
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
  val UnitValue : machineWord = word0; (* unit *)
  val False : machineWord = word0;     (* false *)
  val True  : machineWord = word1;     (* true *)
  val Zero  : machineWord = word0;     (* 0 *)
  
  val constntTrue  = Constnt True;
  val constntFalse = Constnt False;
  (* Where the result, if any, should go *)
  datatype whereTo =
    NoResult     (* discard result *)
  | ToReg of reg (* put result in a specific register *)
  | ToPstack     (* Need a result but it can stay on the pseudo-stack *);
  
  fun isNoResult NoResult     = true | isNoResult _ = false;
  
  (* Are we at the end of the procedure. *)
  datatype tail =
    EndOfProc
  | NotEnd;
  
  fun isEndOfProc EndOfProc = true | isEndOfProc _ = false;

  fun chooseMergeRegister (transtable: ttab, whereto : whereTo, tailKind : tail) : whereTo =
    case tailKind of
      EndOfProc => ToReg regResult
    | NotEnd    =>
       (case whereto of
      ToPstack => 
      let
        val rr : reg = chooseRegister transtable;
      in
        if rr regEq regNone  (* No register is free *)
        then ToReg regResult (* So choose an arbitrary register *)
        else ToReg rr
      end
       | _ => whereto);
 
  (* We're marking the pstack prior to splitting and (probably) rejoining
     the code streams. If the code streams produce a value, try to ensure
     that we have a register free prior to the split, as otherwise we may
     not have a register free to put the return value into, which will
     cause mergeState (in TRANSTAB) to fail.
     SPF 13/11/1998
  *)
  (* I've reverted to the original markStack in order to test my changes
     to Transtab which should fix this problem along with others.
     DCJM 28/6/2000 *)
  fun markStack (transtable : ttab, _ : code, _ : bool) : stackMark =
  let
(*    val () =
      if carry 
      then let
        val freeReg : reg = getAnyRegister (transtable, cvec);
      in
        freeRegister (transtable, freeReg)
      end
      else ();
*)
  in
    TRANSTAB.markStack transtable
  end;

  (* Code generate a procedure or global declaration *)
  fun codegen
       (pt               : codetree,
        cvec             : code,
        declOnPrevLevel  : int * (unit -> stackIndex) * ttab * code -> stackIndex,
        isStaticLink     : int -> bool,
        loadStaticLink   : int * (unit -> stackIndex) * ttab * code -> stackIndex*stackIndex,
        staticLinkRegSet : int -> regSet,
        discardClosure   : bool,
        numOfArgs        : int,
        debugSwitches    : Universal.universal list) : address =
  let
    fun matchFailed _ = raise InternalError "codegen: unhandled pattern-match failure"

    (* make the translation table *)
    val transtable = ttabCreate debugSwitches;

    (* Header code for procedure. *)
    
(* Put the arguments and closure/static link register onto the pseudo-stack. *)
    fun registerArg reg uses =
      if uses > 0
      then let
        val () = getRegister (transtable, cvec, reg);
        val addrInd  = pushReg (transtable, reg);
      in
        incrUseCount (transtable, addrInd, uses - 1);
        addrInd
      end
      else noIndex;

    (* Push the return address - may have multiple references because
       we may exit at any of the "tails". *)
    val returnAddress = 
      if regReturn regEq regNone
      then let
        (* The return address has already been pushed onto the stack,
            probably because the normal call sequence does it. *)
         val addr = incsp transtable;
         val () = incrUseCount (transtable, addr, 1000000);
       in
         addr
       end
       else registerArg regReturn 1000000;

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
    val closureOrSlAddr = registerArg regClosure (if discardClosure then 0 else 1)
      
    (* A vector to map argument numbers into offsets on the pseudo stack. *)
    val argRegTab : stackIndex Array.array =
      Array.array (argRegs:int, noIndex: stackIndex);

    (* off-by-ones adjusted SPF 7/6/94 *)
    local
      fun pushArgRegs i =
        if i < numOfArgs andalso i < argRegs
        then let
        (* DCJM 29/11/99.  Changed to use lastRef rather than reference counts. *)
          val () = Array.update (argRegTab, i, registerArg (argReg i) 1);
        in
          pushArgRegs (i + 1) 
        end
        else ();
    in  
      val () = pushArgRegs 0;
    end;
    
    fun exit () =
    let
      val stackArgs = if numOfArgs < argRegs then 0 else numOfArgs - argRegs;

    in
      if regReturn regEq regNone
      then let
          (* Reset to just above the return address. *)
          val ()  =  resetStack (realstackptr transtable - 1,  cvec);
      in
          returnFromFunction (regNone, stackArgs, cvec)
      end
      else let
          val (returnReg, returnOffset) =
            loadEntry (cvec, transtable, returnAddress, false);
          val () = removeStackEntry (transtable, returnOffset)
          val ()  = resetStack (realstackptr transtable, cvec);
      in
          returnFromFunction (returnReg, stackArgs, cvec)
      end;
      exiting transtable
    end
      

    (* Allocate a segment of the required size. *)
    fun callgetvec (csize, flag : Word8.word, whereto) : stackIndex =
    let
       (* Get a register for the result. We cannot use the local ptr. for the
          result because it's supposed always to point to the bottom of the
          local area. If we get a persistent store trap which results
           in a garbage-collection the newly allocated object might well be
           moved. The local ptr would be updated to point to this new location
           but would then be set to the bottom of store. *)
      (* Use the preferred reg if we can. *)
      val resultReg = 
        case whereto of
          ToReg rr => ( getRegister (transtable, cvec, rr); rr )
        | _ => getAnyRegister (transtable, cvec);
        
      val () = allocStore (csize, flag, resultReg, cvec);
      val resAddr = pushReg (transtable, resultReg);
      val () = containsLocal (transtable, resultReg); (* Not persistent address. *)
    in
      resAddr
    end;

    (* Remove the mutable bit without affecting the use-count. *)
    fun lockSegment (entry, flag) : unit =
    let
      val () = incrUseCount (transtable, entry, 1);
      val (baseReg, baseIndex) = loadEntry (cvec, transtable, entry, false);
    in
      CODECONS.setFlag (baseReg, cvec, flag);
      removeStackEntry(transtable, baseIndex)
    end;
    
    infix 9 sub; (* was only 5 - gave subtle bugs. SPF 11/8/94 *)
    
    (* Loads a local, argument or closure value; translating local
       stack addresses to real stack offsets.
       N.B. In the case of non-local variables lastRef is true only for
       the last non-local variable, not the last use of this particular
       variable. *)
    fun locaddr ({ addr, fpRel, lastRef, ...}): stackIndex =
      if fpRel 
      then 
        if addr < 0 (* Args. *)
        then let (* First four args are on the stack. *)
          val argOffset = numOfArgs + addr;
        in
          if argOffset < argRegs
          then
             let
                val regEntry = Array.sub (argRegTab, argOffset)
             in
                (* If this is NOT the last reference we need to increment the
                   use count on the entry. *)
                if lastRef then () else incrUseCount(transtable, regEntry, 1);
                regEntry
             end
          else pushStack (transtable, addr * ~wordSize)
        end
        
        (* positive address - reference to entry on the pstack. *)
        else
            let
                val resIndex = pstackForDec (transtable, addr)
            in
                if lastRef then () else incrUseCount(transtable, resIndex, 1);
                resIndex
            end
          
      else  (* cp relative *)
         let
         (* If this is the last reference to the closure we want
            it to be removed afterwards.  makeSl is not always called
            if, for example, the value is constant.  To ensure the
            use-count is correct we increment it if it is used and
            then decrement it afterwards.  DCJM 2/12/99. *)
         val dec = declOnPrevLevel 
           (addr,
            fn () => (
                incrUseCount(transtable, closureOrSlAddr, 1);
                closureOrSlAddr),
            transtable,
            cvec)
          in
          if lastRef andalso not discardClosure
          then incrUseCount(transtable, closureOrSlAddr, ~1) else ();
          dec
          end
     (* locaddr *);
    
    (* For each load of a local in the tree it calls the `add' procedure. *)
    fun identifyLoads (expList : codetree list, transtable) : (stackIndex -> unit) -> unit =
      fn (add : stackIndex -> unit) =>
      let 
       (* Need to identify declarations within the current block.
          The declaration numbers are reused so we have to identify
          new declarations. *)
        val newDecs : bool StretchArray.stretchArray =
           StretchArray.stretchArray (4, false);
           
        fun loads (pt: codetree) : unit =
          case pt of
            MatchFail => ()
            
          | AltMatch (exp1, exp2) =>
            let
              val () = loads exp1;
            in
              loads exp2
            end
          
          | Extract {fpRel = true, addr = locn, lastRef, ...} =>
             (* DCJM 29/11/99.  Only call add if this is the last reference. *)
              if locn < 0 (* args - only look at those in the registers. *)
              then let
                val argOffset = numOfArgs + locn;
              in
                if argOffset < argRegs andalso lastRef
                then add (Array.sub (argRegTab, argOffset)) (* SPF 7/6/94 *)
                else ()
              end
              else if not (StretchArray.sub (newDecs,locn)) andalso lastRef
                (* Ignore new declarations. *)
                then add (pstackForDec (transtable, locn))
              else ()

       (* If discardClosure is true, then we've already zeroed the
          use-count for closureOrSlAddr, so don't adjust it now.
          SPF 22/5/95 *)
          | Extract {fpRel = false, addr = _, level = _, lastRef, ...} =>
              if not discardClosure  (* Non-local *) andalso lastRef (* DCJM 1/12/99. *)
              then add closureOrSlAddr (* Reference to the closure. *)
              else ()
          
          | Eval {function, argList, ...} =>
            let
              val () = loads function;
            in
              List.app (loads o #1) argList
            end
            
          | Declar {addr, value, ...} =>
            let
               (* Indicate that this is a new declaration. *)
              val () = StretchArray.update (newDecs, addr, true);
            in
              loads value (* Check the expression. *)
            end
            
          | Indirect {base, ...} => loads base
            
          | Newenv vl => List.app loads vl
            
          | Recconstr vl => List.app loads vl

          | BeginLoop{loop, arguments} => (List.app (loads o #1) arguments; loads loop)

          | Loop argList => List.app (loads o #1) argList

          | Handle{exp, handler} => (loads exp; loads handler)

          | MutualDecs decs =>
                (
                 (* First process the declarations to ensure that new declarations
                    are marked as such then process the values being declared. *)
                 List.app(
                    fn Declar{addr, ...} => StretchArray.update (newDecs, addr, true)
                     | _ => raise InternalError "MutualDecs: not Declar") decs;
                 List.app loads decs
                )

          | _ => ();
      in
        List.app loads expList
      end;
      
    (* code-generates code from the tree *)
    (* SPF 2/5/95 - primBoolOps added to prevent loop when
       trying to inline unsupported boolean primitives. We might
       get the calling sequence:
       
         genEval -> genCond -> genTest -> genOtherTests -> gencde -> genEval
         
       where both versions of genEval are for the same (unsupported)
       boolean comparison. If this occurs, the second call will have
       primBoolOps set to false, and will generate a call to the RTS.
       
       Note that "whereto" is only a HINT. There is no guarantee that specifying
       "ToReg r" will actually get the value loaded into that register. For example,
       the code that handles constants completely ignores this hint.
       SPF 15/8/96
     *)
    fun gencde (pt, primBoolOps, whereto, tailKind, matchFailFn, loopAddr) : mergeResult =
    let 
      val needsResult : bool = not (isNoResult whereto);
      
      val result : mergeResult = 
        case pt of
          MatchFail => (* A bit like Raise *)
          let
            val () = matchFailFn ();
          in
            if needsResult
            then MergeIndex(pushConst (transtable, DummyValue))
            else NoMerge (* Unused. *)
          end
        
        | AltMatch (exp1, exp2) => (* A bit like Cond *)
          let
            val mergeResult : bool = needsResult andalso not (isEndOfProc tailKind);
            val mark1 : stackMark = markStack (transtable, cvec, mergeResult);
            val mark2 : stackMark = markStack (transtable, cvec, mergeResult);
              
            val failLabs = ref ([] : labels list);
            fun newMatchFailFn () = 
            let
              val thisFailure : labels = 
                unconditionalBranch (NoMerge, transtable, cvec);
            in
              failLabs := thisFailure :: !failLabs
            end;
            
            (* SPF 27/11/96 - merged values don't necessarily go into regResult *)
            val whereto : whereTo = chooseMergeRegister (transtable, whereto, tailKind);

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
              then exit ()
              else ();

            (* If exp1 succeeded, we skip exp2 *)
            val suceedLab : labels =
                unconditionalBranch (exp1Result, transtable, cvec);
            
            (* If exp1 failed, we come here (with NO result). *)
            val _ = 
              mergeList (!failLabs, transtable, cvec, NoMerge, mark2)
            
            (* Compile exp2 using the OLD matchFailFn *)
            val exp2result = 
               genToRegister (exp2, whereto, tailKind, matchFailFn, loopAddr);
 
          in
            (* If exp1 succeeded, we merge back in here. *)
            merge (suceedLab, transtable, cvec, exp2result, mark1)
          end
            
        | Eval {function, argList, ...} =>
            genEval (function, List.map #1 argList, primBoolOps, whereto, tailKind, matchFailFn)
          
        | Declar {addr, value, references} =>
          let
            val decl : stackIndex = genToStack (value, matchFailFn);

            (* Put the entry for this declaration in the table and set its use-count. *)
            (* Is it a procedure which can be called with a static link? *)
            val slProc : bool = 
              case value of
                Lambda {makeClosure = false, ...} => true
              | _ => false;
              
            val ()= 
              makeEntry (transtable, cvec, decl, addr, 
                if references = 0 then 0 else 1, (* DCJM 29/11/99. *)
                slProc);
          in
            MergeIndex decl
          end

        | Extract ext =>
            let
                val loc = locaddr ext
            in
                if needsResult
                then MergeIndex loc
                else (* If the result is not required discard it.  This is used
                        to remove variables which are not used on this path. *)
                    (
                    removeStackEntry(transtable, loc);
                    NoMerge
                    )
            end

        | Indirect {base, offset} =>
          let
            val byteOffset : int        = offset * wordSize;
            val baseCode   : stackIndex = genToStack (base, matchFailFn);
          in  (* Get the value to be indirected on. *)
            MergeIndex(indirect (byteOffset, baseCode, cvec, transtable))
          end

        | Lambda lam =>
            MergeIndex(genProc (lam, fn _ => (), true, whereto, matchFailFn))

        | Constnt w =>
            MergeIndex(pushConst (transtable, w))

        | Cond (testPart, thenPart, elsePart) =>
            genCond (testPart, thenPart, elsePart, whereto, tailKind, matchFailFn, loopAddr)

        | Newenv vl =>
          let (* Processes a list of entries. *)
            fun codeList []    _ =
              (* Either the list is empty or the previous entry was a 
                declaration. Generate a value to represent void.empty so
                that there is something on the stack. *)
                if needsResult
                then MergeIndex(pushConst (transtable, DummyValue))
                else NoMerge (* Ignored *)
                
              | codeList ((valu as Declar _) :: valus) whereto =
                  (* Declaration. *)
                  let
                    val _ =
                      gencde (valu, true, NoResult, NotEnd, matchFailFn, loopAddr);
                  in
                    codeList valus whereto
                  end

              | codeList [valu] whereto =
                  (* Last entry is an expression. *)
                  gencde (valu, true, whereto, tailKind, matchFailFn, loopAddr)
                
              | codeList (valu :: valus) whereto =
                  (* Expression in a sequence. *)
                  let
                    val _ =
                      gencde (valu, true, NoResult, NotEnd, matchFailFn, loopAddr);
                  in
                    codeList valus whereto
                  end

          in
            codeList vl whereto
          end

        | BeginLoop{loop=body, arguments} =>
          let
            val args = List.map #1 arguments
            (* Execute the body which will contain at least one Loop instruction.
               There will also be path(s) which don't contain Loops and these
               will drop through. *)
            (* We must ensure that everything apart from the arguments has been
               pushed onto the stack.  This may be unnecessary if the loop body
               is simple but is the only way to ensure that when we jump back to
               the start we have the same state as when we started. *)
            val () =
                pushAllBut(transtable, cvec, identifyLoads (args, transtable), allRegisters);
            (* Load the arguments.  We put them into registers at this stage
               to ensure that constants and "direct" entries are loaded.  They
               may go onto the stack, which is fine. It could be worth doing
               this in two passes, the first simply evaluating the arguments
               onto the pstack, the second loading them into registers since
               that would generate better code when some arguments are constants
               but others are expressions that push those constants onto the stack. *)
            fun genLoopArg (Declar {addr, value, references}) =
            let
                (* This is almost the same as a normal declaration except
                   that we have to make sure that we use a new location, stack or
                   register, since we're going to be changing the contents of
                   this location.  The easiest way to do that is to load it into
                   a register.  We could do better if we are loading the last
                   reference to the initial value in which case we could reuse
                   its location. *)
                val index = genToStack(value, matchFailFn)
                val (_, decl) = loadEntry(cvec, transtable, index, true)
                (* It should not be a static-link function - just check. *) 
                val _ = 
                  case value of
                    Lambda {makeClosure = false, ...} => 
                        raise InternalError "LoopArg: static link function"
                  | _ => ()
            in
                makeEntry (transtable, cvec, decl, addr, 
                    if references = 0 then 0 else 1, (* DCJM 29/11/99. *)
                    false);
                if references = 0 then noIndex else decl
            end
            |   genLoopArg _ = raise InternalError "genLoopArg: not a declaration"
            val argIndexList = map genLoopArg args;
            (* Now we have loaded the registers we can find out the destinations
               i.e. the register or stack location they were in at the start of
               the loop.  We have to do this after we've loaded all the arguments
               because we may have pushed some onto the stack as we loaded the
               later ones.  That's fine so long as when we loop we put the new
               values in the same place.  *)
            val () = clearCache transtable;
            val argDestList = getLoopDestinations(argIndexList, transtable)
            (* Start of loop *)
            val startLoop (* L1 *) = ic cvec;
            val startSp            = realstackptr transtable;
          in
            gencde (body, true, whereto, tailKind, matchFailFn,
                SOME(startLoop, startSp, argDestList))
          end

        | Loop args =>
            let
                val argList = List.map #1 args
                val (startLoop, startSp, argDestList) =
                    case loopAddr of
                        SOME l => l
                    |   NONE =>
                        raise InternalError "No BeginLoop for Loop instr"
                (* Evaluate the arguments.  Try to put them in the destination
                   register if we can.  It doesn't matter at this stage too much. *)
                fun evalArg(arg, dest) =
                let
                    val whereto =
                      case dest of
                            ArgToRegister reg => ToReg reg
                        |   ArgToStack _ => ToPstack
                        |   ArgDiscard => ToPstack
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
                        val argEntry =
                            loadToSpecificReg (cvec, transtable, reg, arg, false)
                    in
                        lockRegister(transtable, reg);
                        argEntry :: argEntries
                    end
                |   moveArgs(arg :: args, ArgToStack offset :: dests) =
                    (
                        storeInStack (cvec, transtable, arg, offset);
                        moveArgs(args, dests) (* storeInStack removes its table entry *)
                    )
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
                    fn (ArgToRegister reg) => unlockRegister(transtable, reg)
                      | _ => ()) argDestList;
                List.app (fn index => removeStackEntry(transtable, index))
                    argEntries;
                (* We have to make sure that the real stack pointer is consistent.
                   Don't have to record any change on the pseudo-stack because we
                   are about to do a fixup and that will set
                   the state to whatever it was when the test was done. *)
                resetStack (realstackptr transtable - startSp, cvec);
            
                (* Repeat. *)
                jumpBack (startLoop, transtable, cvec);
                (* Put on a dummy result. *)
                if needsResult
                then MergeIndex(pushConst (transtable, DummyValue))
                else NoMerge (* Unused. *)
            end

        | Raise exp =>
          let (* movl <exception>,resultReg; jmp raisex *)
            val _ =
               (* Ensure the return address is on the stack in case
                  we are tracing exceptions. *)
               pushSpecificEntry (transtable, cvec, returnAddress);
               
            val excVal = genToStack (exp, matchFailFn);
            
            val resultIndex = 
               loadToSpecificReg (cvec, transtable, regResult, excVal, true);

          in
            raiseException cvec;
            
            removeStackEntry(transtable, resultIndex);
            
            exiting transtable; (* Nothing further *)

            (* Generate a value to represent void.empty so that there *)
            (* is something on the stack. *)
            if needsResult
            then MergeIndex(pushConst (transtable, DummyValue))
            else NoMerge (* Unused. *)
          end

        | Handle {exp, handler} =>
        let
          (* Push all regs - we don't know what the state will be when 
             we reach the handler. *)
(* ...    val () = pushAll (transtable, cvec);    ... *)
          (* Experiment: don't push registers that aren't used in the handler. SPF 25/11/96 *)
          (* i.e. Push all registers except those whose last use occurs in the expression
             we're handling or in the set of exceptions we're catching. *) 
          val () = 
            pushAllBut (transtable, cvec, identifyLoads ([exp], transtable),
                        allRegisters);
          (* It's not clear what registers will be modified as a result of raising
             and handling an exception.  Many functions may result in exceptions
             being raised and rather than add the registers to the register set of
             those functions it's probably better to include them in the modification
             set here. DCJM 26/11/00. *)
          val _ = addModifiedRegSet(transtable, allRegisters);

          (* This is the real stack state at the start of the handler *)
          val startOfHandler = realstackptr transtable;
          
          (* Remember this pseudo-stack position for later merge *)
          val mark : stackMark = markStack (transtable, cvec, needsResult);

          (* Save old handler - push regHandler *)
          val () = genPush (regHandler, cvec);
          val oldIndex = incsp transtable;
          
          (* Now it's on the real stack we can remove it from the pstack. *)
          val () = removeStackEntry(transtable, oldIndex);
          
          val genTag: TRANSTAB.handler =
          let
            (* Push address of new handler. *)
            val rsp         = realstackptr transtable;
            val handlerLab  = pushAddress (transtable, cvec, rsp + 1);
    
            (* The old format of exception handler consisted of pairs of an
               exception tag and a handler address.  Zero was the default handler. *)
            val locn        = pushConst(transtable, toMachineWord 0)
            val stackLocn   = pushValueToStack (cvec, transtable, locn, rsp + 2);
    
            (* Now it's on the real stack we can remove it from the pstack. *)
            val ()    = removeStackEntry(transtable, stackLocn);
          in
            handlerLab
          end

          val handlerList : TRANSTAB.handler list = [genTag]
      
          (* Initialise regHandler from regStackPtr *)
          val () = genRR (instrMove, regStackPtr, regNone, regHandler, cvec);

          (* SPF 27/11/96 - merged values don't necessarily go into regResult *)
          val whereto : whereTo = chooseMergeRegister (transtable, whereto, tailKind);
 
          (* Code generate body, putting the result in result register. *)
          (* "NotEnd" because we have to come back to remove the handler. *)
          val bodyResult = genToRegister (exp, whereto, NotEnd, matchFailFn, loopAddr);
 
          (* Reload the old value of regHandler i.e. remove handler. *)
          val () =
            genLoad ((realstackptr transtable - startOfHandler - 1) * wordSize,
               regStackPtr, regHandler, cvec)

          (* Optimisation: return immediately, if possible, rather than
             jumping and then returning. This may turn the following
             unconditional branch into dead code, in which case it
             will be removed by the lower-level code generator.
             SPF 25/11/96
           *)
          val () =
            if (isEndOfProc tailKind) andalso not (haveExited transtable)
            then exit ()
            else ();
    
          (* Skip over the handler. *)
          val skipHandler = unconditionalBranch (bodyResult, transtable, cvec);
          
          (* Remove any result at the start of the handler.
             Need this because fixupH does not do setState.
             (It probably should do, though the state is fairly simple). *)
          val () =
            case bodyResult of
                MergeIndex bodyIndex => removeStackEntry(transtable, bodyIndex)
              | NoMerge => ();
     
          (* Fix up the handler entry point - this resets the stack pointer
             and clears the cache since the state is not known. *)
          val _ = 
            map (fn handlerLab => fixupH (handlerLab, startOfHandler, transtable, cvec))
              handlerList;
          
          (* The code for the handler body itself *)
          val handlerRes =
            genToRegister (handler, whereto, tailKind, matchFailFn, loopAddr);
          
          in
          (* Merge the results. SPF 25/11/96 *)
          merge (skipHandler, transtable, cvec, handlerRes, mark)
          end
        
        | Ldexc =>
          ((* Exception packet is returned in result register. *)
            getRegister (transtable, cvec, regResult);
            MergeIndex(pushReg (transtable, regResult))
          )

        | Case {cases, test, default, caseType} =>
          let
            (* Cases are constructed by the optimiser out of if-then-else
               expressions.
               There was a previous comment which suggested that an empty
               default case meant the case was exhaustive.  I think this
               is a mistake which probably came from an early attempt at
               improving the handling of ML pattern matching.  In fact
               an expression such as
               case x of Red=>.. | Green=>... | Blue => ...
               which is exhaustive will result in the Blue clause being the
               default case.  *)
            val noDefault  = case default of CodeNil => true | _ => false;
            val mergeResult = needsResult andalso not (isEndOfProc tailKind);

            val (_, min:int, max:int) =
                case caseType of
                    CaseTag max => (true, 0, Word.toInt max)
                |   _ =>
                    let
                        val (_, aLabel) = hd cases
                        fun foldCases((_, w), (min, max)) = (Word.min(w, min), Word.max(w, max))
                        val (min, max) = List.foldl foldCases (aLabel, aLabel) cases
                    in
                        (false, Word.toInt min, Word.toInt max)
                    end
            
            (* Don't bother if the default is a constant and we don't
               actually want a result.  This occurs as a result of ifs
               without elses (in Poly) being converted into cases. *)
            val needsDefaultCase =
              not noDefault (* andalso
               (needsResult orelse not (isConstnt default)) *);
                
            (* SPF 14/9/94; for ML:  needsDefaultCase = not exhaustive *)
            (* DCJM: I think this is wrong.  The Case codetree entry is
               constructed by the codetree optimiser out of if-then-else
               expressions.  There are a very few instances of the ML code
               producing the equivalent of if-then without an else but they
               are all cas*)
            val () = if noDefault andalso needsResult
                then raise InternalError "Case - no default" else ();
                
            val testValue = genToStack (test, matchFailFn);

            (* SPF 27/11/96 - merged values don't necessarily go into regResult *)
            val whereto : whereTo = chooseMergeRegister (transtable, whereto, tailKind);

        (* This procedure decides whether to use a case instruction
           or a comparison depending on whether the cases are sparse.
           A more efficient algorithm, possibly using a binary chop,
           should probably be used for the sparse cases. *)
        fun caseCode (_:int) (_:int) (_:int) [] : mergeResult =
          (* Put in the default case. *)
           if needsDefaultCase
           then genToRegister (default, whereto, tailKind, matchFailFn, loopAddr)
           else NoMerge (* Assume that we don't have a result. *)
           
         | caseCode (min:int) (max:int) (numberOfCases:int) ((cp: codetree * word)::cps) =
           if useIndexedCase (min, max, numberOfCases, noDefault)
           then let
         val mark = markStack (transtable, cvec, mergeResult);
         
         (* Get exclusive use so that
            indexedCase can modify the registers. *)
         val (testReg, testIndex)  =
            loadEntry (cvec, transtable, testValue, true);
         val () = removeStackEntry (transtable, testIndex);
      
         (* Need a work register. *)
         val () = lockRegister (transtable, testReg);
         val workReg = getAnyRegister(transtable, cvec);
         val () = freeRegister (transtable, workReg);
         val () = unlockRegister (transtable, testReg);
      
         val caseInstr : jumpTableAddrs = 
           indexedCase (testReg, workReg, min, max, noDefault, cvec);
           
         val startOfCase = saveState (transtable, cvec);
      
         (* Put in the default case, if there is one. *)
         val defaultCase = startCase (transtable, cvec, startOfCase);
         
         val exitDefault =
           if needsDefaultCase
           then let
             val defaultRes =
               genToRegister (default, whereto, tailKind, matchFailFn, loopAddr);
               
            (* Optimisation: return immediately, if possible, rather than
               jumping and then returning. This may turn the following
               unconditional branch into dead code, in which case it
               will be removed by the lower-level code generator.
               SPF 25/11/96
             *)
             val () =
               if (isEndOfProc tailKind) andalso not (haveExited transtable)
               then exit ()
               else ();
              
             val lab =
               unconditionalBranch (defaultRes, transtable, cvec);
               
             val () =
               case defaultRes of
                  MergeIndex defaultIndex =>
                    removeStackEntry (transtable, defaultIndex)
                | NoMerge => ()
           in
             lab
           end
           else unconditionalBranch (NoMerge, transtable, cvec);

         (* Generate the cases.  N.B.  We generate the list of
            cases in reverse order.  makeJumpTable relies on this
            if we have any duplicates, which could arise if the
            higher level has turned an if-then-else into a case.
            e.g. if x = 1 then a else if x = 1 then b else c. *)
         fun genCases ((caseExp, caseLabel: word) :: cps) ccl =
         let
           val caseAddr = startCase (transtable, cvec, startOfCase);
           val mark = markStack (transtable, cvec, mergeResult);
           
           (* For each case label make an entry in the list.  Add
              new entries at the beginning. *)
           val newCaseList = constrCases(Word.toInt caseLabel, caseAddr) :: ccl
              
           (* Generate this case and exit if tail-recursive. *)
           val expResult =
             genToRegister (caseExp, whereto, tailKind, matchFailFn, loopAddr);
           
           val () =
             if (isEndOfProc tailKind) andalso not (haveExited transtable)
             then exit ()
             else ();
         in
           if null cps
           then
              (
                (*  Finished. *)
                makeJumpTable (caseInstr, newCaseList, defaultCase, min, max, cvec);
                expResult (* Last expression. *)
              )
           else let
             val lab = unconditionalBranch (expResult, transtable, cvec);
               
             val () =
                case expResult of
                    MergeIndex expIndex => removeStackEntry(transtable, expIndex)
                  | NoMerge => ();
      
             val lastResult = genCases cps newCaseList;
           in
             (* Now fix up the exit label. *)
             merge (lab, transtable, cvec, lastResult, mark)
           end
         end
         | genCases [] _ = 
              raise InternalError "genCase - null case list"; (* genCases *)

         val caseResult = genCases (cp::cps) []
           in
         merge (exitDefault, transtable, cvec, caseResult, mark)
           end (* useIndexedCase *)
      
          else let
        (* Don't use indexing. *)
        val mark  = markStack (transtable, cvec, mergeResult);
        
        val lastCase = noDefault;
        val lastTest = null cps;
      
        (* If this is not the last test we increment the use count for
           the case expression so that it will not be thrown away.
           We need to do this because we're converting the case
           expression, which has a single "last reference" marker
           into a series of if-then-elses. *)
        val () =
          if not lastTest
          then incrUseCount (transtable, testValue, 1)
          else ()
           
        val (caseExp : codetree, caseLabel) = cp;
        
        val lab =
         let
           val locn = pushConst (transtable, toMachineWord caseLabel);
         in
           (* should do arbitrary precision test here? Yes. *)
           compareAndBranch (testValue, locn,
         testNeqA, testNeqA, transtable, cvec)
         end 

        (* If we have incremented the use count on the
           test value we need to decrement it here.  That's
           because on this branch we are not going to test it
           again.  DCJM 7/12/00. *)
        val () =
          if not lastTest
          then incrUseCount (transtable, testValue, ~1)
          else ()
      
        (* Generate this case and exit if tail-recursive. *)
        val thisCaseRes =
            genToRegister (caseExp, whereto, tailKind, matchFailFn, loopAddr);
      
        val () = 
          if isEndOfProc tailKind andalso not (haveExited transtable)
          then exit () 
          else ();
          
        (* Jump round the other cases. *)
        val lab1 = unconditionalBranch (thisCaseRes, transtable, cvec);
        
        (* remove result of this case from pstack *)
        val () =
          case thisCaseRes of
            MergeIndex resIndex => removeStackEntry(transtable, resIndex)
          | NoMerge => ();
      
        (* Do the other cases. *)
        val () = fixup (lab, transtable, cvec);
           
        val caseResult =
            if lastCase
            then thisCaseRes
            else caseCode min max (numberOfCases - 1) cps
          in
        (* Merge all the results together. *)
        merge (lab1, transtable, cvec, caseResult, mark)
          end (* caseCode *);

            val result = caseCode min max (List.length cases) cases;

            (* v2.08 code-generator no longer clears the cache here *)
            (* val () = clearCache transtable; *)
         in
           result 
         end      

        | MutualDecs dl =>
          let
            (* Mutually recursive declarations. For the moment assume
               that these can only be procedures. Recurse down the list
               pushing the addresses of the closure vectors or forward
               references to the code, then unwind the recursion and fill
               in closures or compile the code. *)
            fun genMutualDecs []      = ()
              | genMutualDecs ((Declar{value=dec, addr, references, ...})::ds) =
              (
                case dec of
                    Lambda (lam as { makeClosure,...}) =>
                  let
                  val _ =
                    genProc
                      (lam,
                       (* This procedure is called once the closure has been
                          created but before the entries have been filled in. *) 
                       fn (r : stackIndex) =>
                         let
                           val () =
                             makeEntry (transtable, cvec, r, addr,
                                    references, not makeClosure);
                         in (* Now time to do the other closures. *)
                            genMutualDecs ds
                         end,
                       null ds, (* Last one? *)
                       ToPstack,
                       matchFailFn)
                    in
                      ()
                    end
                 | _ =>
                    let (* should only be constants i.e. procedures already compiled. *)
                      val () =
                       makeEntry (transtable, cvec, genToStack (dec, matchFailFn),
                               addr, references, false);
                    in
                      genMutualDecs ds
                    end
              ) (* genMutualDecs *)
              | genMutualDecs (_) =
                raise InternalError "genMutualDecs - Not a declaration";
              
            val () = genMutualDecs dl;
          in
            NoMerge (* Unused. *)
          end

        | Recconstr reclist =>
            let
              val vecsize = List.length reclist;
            in
              if vecsize = 0 (* shouldn't occur *)
              then MergeIndex(pushConst (transtable, UnitValue))
                
              (* This code used to allocate a mutable vector for
                 large (more than 5 word) allocations. The idea
                 of this was to avoid calculating the values onto
                 the stack. It didn't work, because the values
                 have already been calculated by the time we get
                 here. Furthermore, mutable allocations are
                 more expensive (especially when we move the
                 responsiblity for zeroing the heap from the RTS
                 to the compiler), so I've now deleted this code.
                 SPF 22/10/96
              *)
              else let 
                    (* Since the vector is immutable, we have to evaluate
                       all the values before we can allocate it. *)    
                fun loadSmallVector []     _ = 
                       callgetvec (vecsize, F_words, whereto)
                       
                  | loadSmallVector (h::t) byteOffset =
                  let
                    val v   = genToStack (h, matchFailFn);
                    val vec = loadSmallVector t (byteOffset + wordSize)
                    val () =
                      moveToVec (vec, v, byteOffset, STORE_WORD, cvec, transtable)
                  in
                    vec
                  end;
                val vec : stackIndex = loadSmallVector reclist 0;
                        (* we have to make sure that the code-generator is not going to
                          reorder the instructions so an instruction which might trap
                          is put in the sequence of loads. *)
                val () = completeSegment cvec;
              in
                MergeIndex vec
              end
            end
        
        | Container size =>
            (* Reserve a number of words on the stack for use as a tuple on the
               stack.  The result is the address of this space. *)
            MergeIndex(reserveStackSpace(transtable, cvec, size))

        | SetContainer{container, tuple, size} =>
            (* Copy the contents of a tuple into a container. *)
            let
                val vec = genToStack (container, matchFailFn);
            in
                case tuple of
                    Recconstr cl =>
                        (* Simply set the container from the values. *)
                    let
                        fun setValue(v, byteOffset) =
                        let
                            val entry = genToStack (v, matchFailFn)
                        in
                            (* Move the entry into the container.  Does not affect the
                               use count for the container entry. *)
                            moveToVec (vec, entry, byteOffset, STORE_WORD, cvec, transtable);
                            byteOffset + wordSize
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
                            val byteOffset = n * wordSize
                            val _ =
                                if n = size - 1
                                then ()
                                else incrUseCount(transtable, tup, 1)
                            val entry = indirect (byteOffset, tup, cvec, transtable)
                        in
                            moveToVec (vec, entry, byteOffset, STORE_WORD, cvec, transtable);
                            copy (n+1)
                        end
                    in
                        copy 0
                    end;

                removeStackEntry(transtable, vec); (* Free the container entry. *)
                (* Return a void result if necessary. *)
                if isNoResult whereto then NoMerge
                else MergeIndex(pushConst (transtable, DummyValue))
            end

        | TupleFromContainer(container, size) =>
            (* Create a tuple from the contents of a container. *)
            let
                val vec = genToStack (container, matchFailFn);
                val tup = callgetvec (size, F_words, whereto);

                fun copy n =
                    if n = size
                    then ()
                    else
                    let
                        (* We need to ensure that the container entry is only removed
                           when we load the last item from it. *)
                        val byteOffset = n * wordSize
                        val _ =
                            if n = size - 1
                            then ()
                            else incrUseCount(transtable, vec, 1)
                        val entry = indirect (byteOffset, vec, cvec, transtable)
                    in
                        moveToVec (tup, entry, byteOffset, STORE_WORD, cvec, transtable);
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
            raise InternalError "gencde: can't code-generate Global value";
    in 
      result
    end (* gencde *) 

    (* Generate an expression putting the result in any register, and return
       the location of it on the stack. *)
    and genToStack (pt : codetree, matchFailFn) : stackIndex =
        let
            val res = gencde (pt, true, ToPstack, NotEnd, matchFailFn, NONE)
        in
            case res of
                MergeIndex index => index
              | NoMerge => raise InternalError "genToStack: no result"
        end

    (* Generate an expression, with a hint that it should go into
       an argument register. SPF 15/8/96 *) 
    and genArg (argNo : int, pt, matchFailFn) : stackIndex =
    let
      val whereto = if argNo < argRegs then ToReg (argReg argNo) else ToPstack
      val res = gencde (pt, true, whereto, NotEnd, matchFailFn, NONE)
    in
            case res of
                MergeIndex index => index
              | NoMerge => raise InternalError "genArg: no result"
    end

(* ...
   (* Used when the result must be put in a register. *)
   and genToResult (pt, whereto, tailKind, matchFailFn, loopAddr) : unit =
   let
     (* Stack results are forced into result register *)
     val toWhere = if isToPstack whereto then ToReg regResult else whereto;
     
     val result = gencde (pt, true, toWhere, tailKind, matchFailFn, loopAddr);
   in
     (* If we need a result put it in the result reg.  We request exclusive use
    of it because otherwise there is a problem when merging the results
    of an if-then-else if the result register is somewhere else on the
    pstack (e.g. let a == ...; if ... then a else ...) *)
      case toWhere of
        ToReg rr => loadToSpecificReg (cvec, transtable, rr, result, true)
      | _        => ()
   end (* genToResult *)
... *)

   (* Used when the result must be put in a register. *)
   and genToRegister (pt, whereto, tailKind, matchFailFn, loopAddr) : mergeResult =
   let
     val result : mergeResult =
        gencde (pt, true, whereto, tailKind, matchFailFn, loopAddr);
   in
     (* If we need a result put it in the result reg.  We request exclusive use
    of it because otherwise there is a problem when merging the results
    of an if-then-else if the result register is somewhere else on the
    pstack (e.g. let a == ...; if ... then a else ...),
    
    If we're at the end of a function, we're not merging, so we don't need
    exclusive use. However, I don't think we actually save anything by trying
    to make use of this fact so let's just be naive.
    SPF 27/11/96
      *)
      case whereto of
        NoResult => NoMerge
      | ToReg rr =>
          (
            case result of
                MergeIndex index =>
                    MergeIndex(loadToSpecificReg (cvec, transtable, rr, index, true))
              | NoMerge => raise InternalError "genToRegister: no result"
          )
      | ToPstack => raise InternalError "genToRegister: not a register"
   end (* genToRegister *)

    (* `mutualRecursive' is only used for mutually recursive procedures
       where a procedure may not be able to fill in its closure if it does
       not procedure address has been pushed but before the code is generated. 
       `lastDec' is true if there are no more mutually recursive declarations.
    *)
    and genProc ({ closure=closureList, makeClosure, name=lambdaName, body=lambdaBody, argLifetimes, ... },
                 mutualRecursive, lastDec, whereto, matchFailFn) =
    let
        val numArgs = List.length argLifetimes
      fun allConstnt [] = true
        | allConstnt (Constnt _ :: t) = allConstnt t
        | allConstnt _ = false;

      (* Finds the nth. item in the closure and returns the entry *)
      fun findClosure (h::_) 1 = (* found it *) h
        | findClosure (_::t) n = findClosure t (n - 1) 
        | findClosure _      _ = raise InternalError "findClosure";
    in
      if not makeClosure
      then let (* static link form *)
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
        fun previous (prevloc, makeSl, newtab, cvec) =
        (* Although directly-recursive references do not involve calls to
          `previous' (the only directly-recursive references are recursive
          calls and they are dealt with in `loadSl') they may be produced
          as "kill entries" whose only effect is to indicate that the
          closure/static-link register is no longer required on a particular
          flow of control.  DCJM 2/12/99. *)
        if prevloc = 0 then makeSl()
        else
        let
          (* The closure entry will usually be an Extract, but may have been
             compiled down to a real constant. This wouldn't happen if earlier
             phases were better at keeping constants out of closures (but that
             requires "sophisticated" analysis of mutually recursive
             declarations). SPF 8/3/96
             
             We have to ensure that the constant value gets pushed onto
             "newtab" NOT "transtable", as otherwise we get very confusing
             bugs - as I found out the hard way! SPF 12/3/96
          *)
          val closureEntry = findClosure closureList prevloc;
        in
          case closureEntry of
            Constnt w =>
          (* Should we decrement the use count for closureOrSlAddr here?
             Probably, but I'm not yet absolutely convinced that it's
             safe, so I'm going to do nothing (carefully). I'll have
             to come back and look at this again later.
             SPF 2/5/97
           *)
              pushConst (newtab, w) (* SPF 8/3/96 *)
              
          | Extract {fpRel = true, addr = locn, ...} => (* argument or on stack *)
              if locn < 0 (* argument *)
              then let
                val argOffset = numOfArgs + locn;
              in
                if argOffset < argRegs
                then pushNonLocal (transtable, newtab, Array.sub (argRegTab, argOffset), makeSl, cvec)
                else indirect (~locn * wordSize, makeSl (), cvec, newtab)
              end
              else (* on the stack *)
                pushNonLocal (transtable, newtab, pstackForDec (transtable, locn), makeSl, cvec)
         
          | Extract {fpRel = false, addr = locn, ...} => (* Try the next level *)
              declOnPrevLevel
                (locn,
                 fn () =>
                    pushNonLocal (transtable, newtab, closureOrSlAddr, makeSl, cvec),
                 newtab,
                 cvec)
         
          | _ =>
             raise InternalError "previous: bad codetree in closure"
        end;

        (* Returns true if the procedure is to be called with a static link *)
        fun isSl prevloc =
          if prevloc = 0 then true (* Recursive call. It's this procedure *)
          else let (* Not directly recursive. *)
            val closureEntry = findClosure closureList prevloc;
          in
            (* 
               We may have already compiled a "mutually recursive" function
               to a constant, if it doesn't actually depend on this one.
               (This wouldn't occur if the earlier stages were better at
               removing such fake dependencies.)
               SPF 8/3/96
               
               If the constant is a closure, it doesn't need a static link;
               if it's a pure code segment, it may do. Is it safe to
               assume that it does?
               SPF 10/4/96
            *)
            case closureEntry of
              Constnt (w : machineWord) => isCode (toAddress w)

            | Extract {fpRel = true, addr = correctedLoc, ...} =>
                correctedLoc > 0 andalso isProcB (transtable, correctedLoc)
              
            | Extract {fpRel = false, addr = correctedLoc, ...} =>
                isStaticLink correctedLoc (* Non-local *)
            
            | _ =>
               raise InternalError "isSl: bad codetree in function closure"
          end;
         
        (* Loads the static link if the procedure is called with one
           and returns the entry point of the procedure on the stack. *)
        fun loadSl (prevloc, makeSl, callingTab, callingCvec): stackIndex*stackIndex =
          if prevloc = 0
          then let (* Recursive call. *)
            val sl = makeSl(); (* Push the static link. *)
            val closureIndex =  (* Load into regClosure *)
              loadToSpecificReg (callingCvec, callingTab, regClosure, sl, false);
          in
             (* And push the address of this procedure as the entry point. *)
            (pushCodeRef (callingTab, newCode), closureIndex)
          end

          else (* Non-recursive. *)
            case findClosure closureList prevloc of
                Extract { addr=correctedLoc, fpRel, ...} =>
                if fpRel
                then (* On this level *)
                    let
                        val closureIndex =
                            (* Load closure/sl register with the static link. *)
                            loadToSpecificReg (callingCvec, callingTab,
                                regClosure, makeSl(), false)
                        val procAddr =
                            (* Get the address of the procedure. *)
                            previous (prevloc, makeSl, callingTab, callingCvec)
                    in
                        (procAddr, closureIndex)
                    end
                else (* Non-local *)
                  loadStaticLink 
                    (correctedLoc,
                     fn () => pushNonLocal (transtable, callingTab, 
                                 closureOrSlAddr, makeSl, callingCvec),
                     callingTab,
                     callingCvec)
            | _ => raise InternalError "loadSl - closure not extract"
          (* loadSl *);

        (* Returns the register set for a static link function. *)
        fun slRegSet prevloc =
          if prevloc = 0 then allRegisters (* Recursive call - all registers. *)
          else let (* Not directly recursive. *)
            val closureEntry = findClosure closureList prevloc;
          in
            case closureEntry of
              Constnt (w : machineWord) => getRegisterSet w
            | Extract {fpRel = true, addr, ...} =>
                if addr > 0
                then getFunctionRegSet(pstackForDec (transtable, addr), transtable)
                else raise InternalError "slRegSet: argument"
            | Extract {fpRel = false, addr, ...} =>
                staticLinkRegSet addr (* Non-local *)
            | _ =>
               raise InternalError "slRegSet: bad codetree in function closure"
          end;
        
        (* Make sure all the closure values in registers are on the stack,
           in case they are used as non-locals.  Changed this from the
           original code which pushed everything. DCJM 30/11/00. *)
        local
            fun pushClosure (Extract{fpRel=true, addr, ...}) =
                (* Local *)
              if addr < 0
              then let
                val argOffset = numOfArgs + addr;
              in
                if argOffset < argRegs
                then pushSpecificEntry(transtable, cvec,
                            Array.sub (argRegTab, argOffset))
                else ()
              end
              else (* on the stack *)
                pushSpecificEntry(transtable, cvec,
                        pstackForDec (transtable, addr))
            | pushClosure (Extract{fpRel=false, ...}) =
                    (* Non-local or recursive reference: make sure the closure/static
                       link pointer is on the stack. *)
                (
                if discardClosure
                then () (* May not have a closure/sl. *)
                else pushSpecificEntry(transtable, cvec, closureOrSlAddr)
                )
            | pushClosure _ = () (* Constant. *)
            
        in
            val _ = List.app pushClosure closureList
        end;
         
        (* Push a forward reference to the code in case of mutually
           recursive references.  This is left as the result for normal
           references. *)
        val result = pushCodeRef (transtable, newCode);
        val () = mutualRecursive result; (* Any recursive references. *)
        (* Now code-generate the procedure, throwing away the result which
           will be put into the forward reference. *)
        val _ : address = 
          codegen
           (lambdaBody,
            newCode,
            previous,
            isSl,
            loadSl,
            slRegSet,
            false,  (* Presumably we need the static link, so don't discard regClosure. *)
            numArgs, 
            debugSwitches);
        (* Note: we could sometimes discard the static link, but it's difficult
           to work out when this would be safe. That's because it would be unsafe
           if loadSl were ever called. This is in contrast to the "closure" case below,
           where loadSl is never called, so we can just check that the immediate closure
           contains only constants. 
           SPF 2/5/97
        *)
      in
        result
      end
     
      (* This is how DCJM optimises self-calls - rather than use
         "previous" to find out-of-scope references, he knows
         that the only such reference can be the closure itself,
         and so returns it as a codeRef. Treating the closure as
         a constant allows him to release closureReg (by setting
         its useCount to zero). I've removed that optimisation,
         and pick up self-references directly. This should allow
         a more general treatement eventually. SPF 20/5/95.
         
         Oops - we can have recursive references to the function
         that are not just simple calls (e.g. embedding it in
         a data-object). In such cases, we must store the
         closure in the constants section (since we've thrown
         away the copy that was in the closure register!).
         SPF 30/5/95.
         
         I've replaced the "isNil (lambdaClosure lam)" test with
         a test that all the items in the closure are constants.
         I've had to modify the "previous" code slightly, so that
         it can tell the difference between a load of a constant
         from the closure (translated into a load from the constants
         section) and a recursive reference to the closure itself.
         Later, we might want extend this by allowing codeRefs, not
         just pre-compiled constants and indirections, in the (logical)
         closure. SPF 2/5/97
       *)  
      else if allConstnt closureList
        (* Procedure with no non-local refererences. *)
        then let 
         (* The only non-local references will be constants and references
            to the closure itself. We have to fetch these from the constants
            section because:
              (1) we don't save the closure register in the function body
              (2) we don't even initialise it if we use the PureCode
                  calling convention
            SPF 2/1/97
          *)
          val newCode = codeCreate (false (* make a closure *), lambdaName, debugSwitches);

          (* Should "previous" decrement the reference count for closureOrSlAddr?
             Probably, but I'm not quite sure that it's safe yet. It would be
             better to set discardClosure anyway so that we don't tie up a register
             in the first place, but for now I'll do nothing (carefully).
             SPF 2/5/97
          *)
          fun previous (locn, _, newtab, _) =
            if locn = 0
            then  (* load the address of the closure itself *)
              pushCodeRef (newtab, newCode)
            else (* load a constant (item locn of the logical closure) *)
                case findClosure closureList locn of
                    Constnt cval => pushConst (newtab, cval)
                 | _ => raise InternalError "previous: closure not constant";
          
          val closureAddr : address = 
            codegen 
             (lambdaBody,
              newCode,
              previous,
              fn _ => false,
              fn (_,  _, _, _) => raise InternalError "Not static link",
              fn _ => raise InternalError "Not static link",
              true, (* Discard regClosure *)
              numArgs,
              debugSwitches);
                
          val result = pushConst (transtable, toMachineWord closureAddr);
          
          val () = mutualRecursive result;
        in
          result
        end
        
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
       Should we decrement the reference count for closureOrSlAddr
       here? Probably, but I'm not yet entirely sure that it would be safe.
       SPF 6/5/97
     *)
        fun previous (locn, makeSl, newtab, cvec) =
          if locn = 0
          then makeSl () (* load the address of the closure itself *)
          else let
            val closureItem = findClosure closureList locn;
          in
            case closureItem of
                Constnt cval =>
                    pushConst (newtab, cval) (* load the value as a constant *)
              | _ =>
            let
              val newLocn : int = translateClosureIndex closureList locn 
              val sl : stackIndex = makeSl (); (* load the closure *)
            in
              indirect (newLocn * wordSize, sl, cvec, newtab) (* load value from the closure *)
            end
          end;
        
        val newCode = codeCreate (true (* just the code *), lambdaName, debugSwitches);
        
        val codeAddr : address = (* code-gen procedure *)
          codegen 
           (lambdaBody,
            newCode,
            previous,
            fn _ => false,
            fn (_ ,  _, _, _) => raise InternalError "Not static link",
            fn _ => raise InternalError "Not static link",
            false, (* We need regClosure *)
            numArgs,
            debugSwitches);
        
        val res : machineWord = toMachineWord codeAddr;
      in
        if lastDec
        then let
          (* Can avoid having to set and clear the mutable bit. *)
          (* Load items for the closure. *)
          
          (* Compare with the code for Recconstr *)  
          fun loadItems [] offset =
            let
              (* get store for closure *)
              val vector = callgetvec (offset, F_words, whereto)
            in
              (* Put code address into closure *)
              moveToVec (vector, pushConst (transtable, res), 0, STORE_WORD, cvec, transtable);
              vector
            end
            
            | loadItems (Constnt _ :: t) offset =
                (* constants don't belong in the physical closure *)
                loadItems t offset

            | loadItems (h :: t) offset =
              let
                val valIndex : stackIndex = genToStack (h, matchFailFn);
                val vec = loadItems t (offset + 1)
              in
                moveToVec (vec, valIndex, offset * wordSize, STORE_WORD, cvec, transtable);
                vec
              end;
              
          val vector = loadItems closureList 1;
          
        in
          (* Prevent any traps before the last store. *)
          completeSegment cvec;
          (* Have to call this mutualRecursive to register the address. *)
          mutualRecursive vector; 
          vector
        end
         
        else let
         (* 
            More mutually recursive declarations. We have to allocate as a
            mutable segment and then clear the mutable bit. We no longer need
            to explicitly clear the closure, because that is now handled by the
            allocation routine in the low-level code generator. SPF 21/11/96
           *)
          
          fun nonConstntCount [] = 0
            | nonConstntCount (Constnt _ :: t) = nonConstntCount t
            | nonConstntCount (_ :: t) = nonConstntCount t + 1;
           
          val closureSize = nonConstntCount closureList + 1;
          
          (* get store for closure *)
          val vector  = callgetvec (closureSize, F_mutable_words, whereto);
          
          (* Put code address into closure *)
          local
            val locn = pushConst (transtable, res);
          in
            val () = moveToVec (vector, locn, 0, STORE_WORD, cvec, transtable);
          end;

          local
        (*
           Must clear each word of the closure in case we get a
           garbage collection. We didn't use to need this on AHL RTS,
           because the RTS initialised the store itself, but we've
           now removed this major overhead. Then we didn't need it
           because CODECONS always initialised mutable allocations,
           but that's not a good way to do refs, so I've reinstated
           this code. SPF 11/12/96
           
           We would like use binary zero rather than DummyValue (tagged 0),
           here since we could generate better code for it on some
           machines (e.g. the SPARC), but the lower-level code generator
           doesn't expect to see this (non) value, and it actually causes
           a core dump. SPF 11/12/96
        *)
            val locn = pushConst (transtable, DummyValue);
            val wordsToClear : int = closureSize - 1;
            val () = incrUseCount (transtable, locn, wordsToClear -1);
           
            (* N.B. moveToVec doesn't count as a use of vector. *)
            fun storeWord i = 
              moveToVec (vector, locn, i * wordSize, STORE_WORD, cvec, transtable)
          in
            val () = forLoop storeWord 1 wordsToClear
          end;
          
          (* Have to ensure that the closure remains on the psuedo-stack until
             we've filled in all uses of it. The only references may be in the
             closures of other procedures so it's possible that its use-count
             could be zero when `mutualRecursive' returns. Have to  increment
             the use-count and then decrement it afterwards to make sure it
              is still on the stack. *)
          val () = incrUseCount (transtable, vector, 1);
    
              (* Any mutually recursive references. *)
          val () = mutualRecursive vector;
           
              (* Load items for the closure. *)
          fun loadItems []     _ = ()
            | loadItems (Constnt _ ::t) addr =
                (* constants don't belong in the physical closure *)
                loadItems t addr
            | loadItems (h::t) addr =
            let 
              val () =
                moveToVec (vector, genToStack (h, matchFailed), addr, STORE_WORD, cvec, transtable);
            in
              loadItems t (addr + wordSize)
            end;
    
          val () = loadItems closureList wordSize;
          val () = lockSegment (vector, F_words);
          val () = incrUseCount (transtable, vector, ~1);
        in
          vector
        end
      end
    end (* genProc *)

    (* Generates test for if..then..else or while..do. Returns address of address field of jump.
       If jumpOn is true the jump is taken if the condition is true,
       if false it is taken if the condition is false. *)
    and genTest (pt, jumpOn, matchFailFn) : labels =
    let (* See if we can generate a conditional instruction. *)
      (* Those we can't deal with specially are evaluated to the stack and tested. *)
      fun genOtherTests () =
      let
        val top = gencde (pt, false, ToPstack, NotEnd, matchFailFn, NONE);
         (* Compare the result with false (tagged (0))
            and skip if it does not match. *)
        val tst = if jumpOn then testNeqW else testEqW;
        val constFalse = pushConst (transtable, False);
      in
        case top of
            MergeIndex topIndex =>
                compareAndBranch (topIndex, constFalse, tst, tst, transtable, cvec)
          | NoMerge => raise InternalError "genTest: No result"
      end (* genOtherTests *);
    in
      case pt of
        Cond (testPart, thenPart, elsePart) =>
        let
          val mark1 = markStack (transtable, cvec, false);
          val mark2 = markStack (transtable, cvec, false);
          
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
             fixup (a, transtable, cvec);
             genTest (elsePart, jumpOn, matchFailFn)
             )
          else
          let
              
              (* Now the `then-part' *)
              val b : labels = genTest (thenPart, jumpOn, matchFailFn);
              
              (* Put in an unconditional jump round the `else-part'.
                 This will be taken if the `then-part' drops through. *)
              val notB = unconditionalBranch (NoMerge, transtable, cvec);
              
              (* Fill in the label for the then-part part. *)
              val () = fixup (a, transtable, cvec);
              
              (* Now do the `else-part' and jump on the inverse of the condition. *)
              val notC = genTest (elsePart, not jumpOn, matchFailFn);
              
              (* i.e. we drop though if the condition is the one we should have
                 jumped on. Now merge in the first label so we have both cases
                 when we should jump together, *)
              val _ = merge (b, transtable, cvec, NoMerge, mark2);
              
              (* and now take the jump. *)
              val resultLab = unconditionalBranch (NoMerge, transtable, cvec);
              
              (* Come here if we are not jumping. *)
              val () = fixup (notB, transtable, cvec);
              val _ = merge (notC, transtable, cvec, NoMerge, mark1);
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
          then unconditionalBranch (NoMerge, transtable, cvec)
          else noJump (* else drop through. *)

      | Newenv vl =>
      let (* Blocks and particularly inline procedures. *)
        (* Process the list up to the last item with "gencde",
           and the last item with "genTest". *)
        fun codeBlock []       = noJump
          | codeBlock [h]      = genTest (h, jumpOn, matchFailFn)
          | codeBlock (h :: t) = 
          let
            val _ : mergeResult =
                gencde (h, true, NoResult, NotEnd, matchFailFn, NONE);
          in
            codeBlock t
          end;
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

      | Eval {function = Constnt oper, argList=evalArgList, ...} =>
      (* May be an interface operation which can be put in line. *)
      let
        val args = List.map #1 evalArgList
        (* Generate a compare instruction. *)
        fun genCompare (arg1, arg2, t, f, ti, fi) =
        let
          val test    = if jumpOn then t  else f;
          val revTest = if jumpOn then ti else fi;
        in
          (* Check that the instruction is implemented.
             N.B. if the first argument is a constant we will use
             the reversed instruction.  It may only be implemented
             for constant values so it is not sufficient to check that
             the general form is implemented. *)
           if isCompRR test orelse
             (case arg1 of Constnt w => isCompRI (revTest, w) | _ => false) orelse
             (case arg2 of Constnt w => isCompRI (test, w)    | _ => false)
           then let (* Generate the instruction and get the direction. *)
             val locnOfArg1 = genToStack (arg1, matchFailFn);
             val locnOfArg2 = genToStack (arg2, matchFailFn);
           in
             compareAndBranch (locnOfArg1, locnOfArg2, test, revTest, transtable, cvec)
           end
           else genOtherTests () 
        end (* genCompare *);

      in
        case args of
          [] => (* We don't currently have any nullary special cases *)
             genOtherTests ()
          
        | [arg] =>
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
        
          (* Since "isShort" is a monadic operation we pretend that
         it has a second argument of 0. *)
          | _ =>
        if isCompRI (Short, Zero)
        then let
          val locnOfArg1 = genToStack (arg, matchFailFn);
          val locnOfArg2 = pushConst (transtable, Zero);
          val testOp     = if jumpOn then Short else Long;
        in
          compareAndBranch
            (locnOfArg1, locnOfArg2, testOp, testOp, transtable, cvec)
        end
        else genOtherTests ()
        )
            
        else (* Non-special unary function.*)
              genOtherTests ()
           
        | [arg1, arg2] =>
            (* binary special cases *)
        if wordEq (oper,ioOp POLY_SYS_word_eq)
          then genCompare (arg1, arg2, testEqW, testNeqW, testEqW, testNeqW)
          
        else if wordEq (oper,ioOp POLY_SYS_word_neq)
          then genCompare (arg1, arg2, testNeqW, testEqW, testNeqW, testEqW)
          
        else if wordEq (oper,ioOp POLY_SYS_equala)
          then genCompare (arg1, arg2, testEqA, testNeqA, testEqA, testNeqA)
          
        else if wordEq (oper,ioOp POLY_SYS_int_geq)
          then genCompare (arg1, arg2, testGeqA, testLtA, testLeqA, testGtA)
          
        else if wordEq (oper,ioOp POLY_SYS_int_leq)
          then genCompare (arg1, arg2, testLeqA, testGtA, testGeqA, testLtA)
          
        else if wordEq (oper,ioOp POLY_SYS_int_gtr)
          then genCompare (arg1, arg2, testGtA, testLeqA, testLtA, testGeqA)
          
        else if wordEq (oper,ioOp POLY_SYS_int_lss)
          then genCompare (arg1, arg2, testLtA, testGeqA, testGtA, testLeqA)
          
        else if wordEq (oper,ioOp POLY_SYS_word_geq)
          then genCompare (arg1, arg2, testGeqW, testLtW, testLeqW, testGtW)
          
        else if wordEq (oper,ioOp POLY_SYS_word_leq)
          then genCompare (arg1, arg2, testLeqW, testGtW, testGeqW, testLtW)
          
        else if wordEq (oper,ioOp POLY_SYS_word_gtr)
          then genCompare (arg1, arg2, testGtW, testLeqW, testLtW, testGeqW)
          
        else if wordEq (oper,ioOp POLY_SYS_word_lss)
          then genCompare (arg1, arg2, testLtW, testGeqW, testGtW, testLeqW)
          
        else (* Non-special binary function. *) 
          genOtherTests ()
      
     | _ => (* Functions with more than 2 arguments. *)
       genOtherTests ()
      end (* constant functions *)

      | _ => (* Anything else *)
         genOtherTests ()

    end

    (* if/then/else, cand and cor. NB if/then/else may be translated
       into a CASE by the optimiser and code-generated there. *)
    and genCond (testExp, thenExp, elseExp, whereto, tailKind, matchFailFn, loopAddr) : mergeResult =
    let
      val needsResult = not (isNoResult whereto);
      val mergeResult : bool = needsResult andalso not (isEndOfProc tailKind);
      val mark = markStack (transtable, cvec, mergeResult);
      val lab  = genTest (testExp, false, matchFailFn); (* code for condition *)
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
    in  (* brb L1; ..then.. brb L2; L1: ..else..; L2: *)
      case elseExp of
        CodeNil => (* No else-part - used for pattern-matching too *)
          let
            (* code for "then part" - noResult 'cos we generate "void" below*)
            val _ =
                genToRegister (thenExp, NoResult, tailKind, matchFailFn, loopAddr);
            val _ = merge (lab, transtable, cvec, NoMerge, mark);
          in
            if needsResult
            then MergeIndex(pushConst (transtable, DummyValue)) (* Generate a void result. *)
            else NoMerge (* Unused *) 
          end
      
      | _ => 
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
            fixup (lab, transtable, cvec);
            gencde (elseExp, true, whereto, tailKind, matchFailFn, loopAddr)
            )
        else
          let
            (* SPF 27/11/96 - merged values don't necessarily go into regResult *)
            val whereto : whereTo = chooseMergeRegister (transtable, whereto, tailKind);
    
            (* code for "then part" *)
            val thenResult =
                genToRegister (thenExp, whereto, tailKind, matchFailFn, loopAddr);
        
            val () = 
              if isEndOfProc tailKind andalso not (haveExited transtable)
              then exit()
              else ();
              
            val lab1     = unconditionalBranch (thenResult, transtable, cvec);
            
            (* Get rid of the result from the stack. If there is a result
               then the "else-part" will push it. *)
            val () =
                case thenResult of
                    MergeIndex thenIndex => removeStackEntry(transtable, thenIndex)
                  | NoMerge => ();
              
            (* start of "else part" *)
            val () = fixup (lab, transtable, cvec);
            val elseResult =
                genToRegister (elseExp, whereto, tailKind, matchFailFn, loopAddr)
          in 
            merge (lab1, transtable, cvec, elseResult, mark)
          end
    end (* genCond *)

    (* Call a function. Detects special cases of calls to the run-time system
       to do simple operations such as int arithmetic and generates the
       instructions directly. For ordinary calls it has to distinguish between
       those called with a static-link and those called with a closure. *) 
    and genEval (evalFun, argList, primBoolOps, whereto, tailKind, matchFailFn) : mergeResult =
    let
      val needsResult : bool = not (isNoResult whereto);
      val argsToPass  : int  = List.length argList;

      (* First evaluate all the arguments to the pseudo stack.  This returns
         a list of pseudo-stack indexes for the registers. *)
      fun evalArgs (argList : codetree list) : stackIndex list =
      let
        fun ldArgs []     _             = []
          | ldArgs (h::t) (argNo : int) =
            let
                val argLocn =
                    if argNo < argRegs
                    then let (* Put into a register. *)
                      val argReg = argReg argNo;
                      (* If we are evaluating an expression we might as well put the
                         result in the register we want to use. They may not stay
                         there because loading other arguments may involve function
                         calls which will use these registers. For that reason we
                         don't put constants in yet. *)
                      val whereto = case h of Constnt _ => ToPstack | _ => ToReg argReg
                    in
                        case gencde (h, true, whereto, NotEnd, matchFailFn, NONE) of
                            MergeIndex index => index
                         |  NoMerge => raise InternalError "ldArgs: No result"
                    end
                    else genToStack (h, matchFailFn)
            in
               argLocn :: ldArgs t (argNo + 1)
            end (* ldArgs *);
      in
        ldArgs argList 0
      end (* evalArgs *);

      (* Second phase of argument evaluation.  Push the values onto the real stack
         or load them into the argument registers.  The result is the stack base
         for stack arguments together with a list of pseudo-stack entries for
         the arguments. *)
      fun pushArgs (argList : stackIndex list) : int * stackIndex list =
      let
        fun ldArgs []     (stackAddr : int) _ = (stackAddr, [])
          | ldArgs (argLoc::t) (stackAddr : int) (argNo : int) =
            if argNo < argRegs
            then let (* Put into a register. *)
              val argReg = argReg argNo;
              (* Load the first before putting these into the registers. *)
              val (rAddr : int, others) = ldArgs t stackAddr (argNo + 1);
              val regEntry = loadToSpecificReg (cvec, transtable, argReg, argLoc, false);
            in
              lockRegister (transtable, argReg);
              (rAddr, regEntry :: others)
            end
            else let (* Store on the real stack. *)
              (* We take the current stack pointer as the base for the stack args. *)
              val sAddr : int = 
                if stackAddr < 0 then realstackptr transtable else stackAddr;
              val pushedEntry =
                pushValueToStack (cvec, transtable, argLoc, sAddr + 1);
              val (rAddr, others) = ldArgs t (sAddr + 1) (argNo + 1)
            in
              (rAddr, pushedEntry :: others)
            end (* ldArgs *);
      in
        ldArgs argList ~1 0
      end (* pushArgs *);

      (* Load arguments for a normal call. First argRegs arguments go into
         registers, the rest are pushed onto the stack. Returns the stack offset
         of the last argument if there are more than argRegs. *)
      (* Called after a function call, to reflect the results of the function call. *)
      fun setupResult 
         (argsPassed     : int,
          needsResult    : bool,
          tailCall       : bool,
          transtable     : ttab) : mergeResult =
      let
        val argRegsUsed : int =
          if argsPassed < argRegs then argsPassed else argRegs;
      in
        (* Unlock  the argument registers *)
        forLoop (fn argNo => unlockRegister (transtable, argReg argNo))
                0 (argRegsUsed - 1);
    
        (* Remove any stack arguments. For tail-calls, we mustn't do this, because
           the stack arguments have already vanished from the pstack - that's
           because storeInStack consumes its argument. Conversely, pushValueToStack,
           used to set up stack arguments normal calls, does NOT consume its pstack
           argument. This means that we mustn't call decsp for tail-calls, which
           in turn means that the real stack pointer doesn't get correctly adjusted.
           Fortunately, this is (just about) OK because this code is unreachable,
           and TRANSTAB.mergeLab is clever enough to avoid merging unreachable states. 
           This whole area is a right mess, which I must sort out some time.
           SPF 13/3/97
           Agreed.  I've tried to tidy this up a bit.  decsp now ONLY affects the
           real stack pointer rather than popping items from the pstack as
           well.  It still needs more work.
           DCJM 25/11/99
         *)
        if tailCall then exiting transtable else ();

        if argsPassed > argRegsUsed
        then decsp(transtable, argsPassed-argRegsUsed)
        else ();

        if not needsResult
        then NoMerge (* Unused *)
        else
        ( (* Result is returned in regResult. *)
          addRegUse (transtable, regResult); (* Needed? *)
          MergeIndex(pushReg (transtable, regResult))
        )
      end;

      (* Call a function.  Used in cases when it's not tail-recursive. *)
      fun callProc (argList, procLocn, modifiedRegisters,
                    loadProc: unit->(stackIndex option * bool * stackIndex list * reg list)) =
        let
          (* evaluate the arguments *)
          val evaluatedArgs = evalArgs argList
          (* Save any values to the stack other than those that are being
             used in this call.  Values in registers not modified by the
             call are locked in their current registers. *)
          val lockedRegs =
             pushNonArguments(transtable, cvec,
                    procLocn @ evaluatedArgs, modifiedRegisters);

          (* Push the arguments onto the real stack and/or load them
             into the argument registers. *)
          val (endOfArgs, argEntries) = pushArgs evaluatedArgs;
              
          (* load regClosure *)
          val (codeAddrOpt, isIndirect, codeEntries, regsLocked) = loadProc ();
              
        in
          (* Make sure that the arguments are contiguous on the
             stack and that there is nothing beyond them on it. *)
          if endOfArgs >= 0
          then resetButReload (cvec, transtable, endOfArgs)
          else ();

          case codeAddrOpt of
             NONE => callFunction (Recursive, cvec)
          |  SOME codeAddr => callCode(codeAddr, isIndirect, transtable, cvec);

          (* Unlock any registers we locked. *)
          List.app (fn r => unlockRegister (transtable, r)) (lockedRegs @ regsLocked);
          (* Remove the arguments and code/closure registers. *)
          List.app (fn index => removeStackEntry(transtable, index))
            (codeEntries @ argEntries);

          (* Remove any registers from the cache which may have been modified
             by the function. *)
          removeRegistersFromCache(transtable, modifiedRegisters);

          setupResult (argsToPass, needsResult, false, transtable)
        end; (* callProc *)

      (* Enter a procedure by jumping rather than calling. *)
      fun jumpToProc (argList,
            loadProc: unit->(stackIndex option * bool * stackIndex list * reg list)) =
      let
    (* Compute the arguments, loading them into registers if they are
       in the argument area, and therefore could be overwritten. Values
       elsewhere on the stack will not be overwritten.
       
       (Of course, we might have to spill the registers again, but if that
       occurs the arguments will go into the reference-counted part of the
       real stack, so we can still guaranteee that moveArgs - below - won't
       zap old arguments while we still need them to initialise the new
       arguments.)
       
       Now try to generate the argument into the RIGHT register, to
       minimise the moveArgs-generated register-shuffling. SPF 15/8/96
     *)
        fun genArgList _ []            = [] : stackIndex list
          | genArgList n (arg :: args) =
        let
          val unsafelocn : stackIndex      = genArg (n, arg, matchFailFn);
          val safeLocn   : stackIndex      = loadIfArg (cvec, transtable, unsafelocn);
          val safeLocns  : stackIndex list = genArgList (n + 1) args;
        in 
          safeLocn :: safeLocns
        end;
        
        val argsOnPstack : stackIndex list = genArgList 0 argList;

        (* Now move the arguments to their final destination. *)
        fun moveArgs []          _     = []
          | moveArgs (arg::args) argNo =
          if argNo < argRegs
          then let
            (* Do it in reverse order so that we can delay locking
               the register arguments. *)
             val argEntries = moveArgs args (argNo + 1);
             val argReg = argReg argNo;
             val argEntry = loadToSpecificReg (cvec, transtable, argReg, arg, false);
          in  
            lockRegister (transtable, argReg);
            argEntry :: argEntries
          end
          else let
            val offset =
              if numOfArgs < argRegs
              then argNo - argRegs
              else argNo - numOfArgs;
              
            (* Store it in the stack, reloading anything it displaces. *)
            val () = storeInStack (cvec, transtable, arg, offset);
          in
            moveArgs args (argNo + 1);
            [] (* storeInStack removes its table entry *)
          end;

             (* the arguments are now all in their rightful places *)
         val argEntries = moveArgs argsOnPstack 0;
    
         (* Now load regClosure as appropriate. We delay this
            until now, because we don't want to zap regCode before
            we've loaded all the constant arguments. *)
         val (codeAddrOpt, isIndirect, callEntries, registersLocked) = loadProc ();
        
         (* Get the return address. *)
         val returnReg : reg =
           if regReturn regEq regNone
           then
             (* The return address is on the stack.  Do we need to load it? *)
             (* Only if we're passing a different number of arguments on
                stack - this would change the offset of the return address. *)
             if argsToPass = numOfArgs orelse 
             (numOfArgs <= argRegs andalso argsToPass <= argRegs)
             then regNone (* Leave it there. *)
             else let
               val (reg, regIndex) = loadEntry (cvec, transtable, returnAddress, false)
             in
               removeStackEntry(transtable, regIndex);
               reg
             end
           else let
             (* Reload the return address into the return register. *)
             val regIndex =
               loadToSpecificReg (cvec, transtable, regReturn, returnAddress, false);
           in
               removeStackEntry(transtable, regIndex);
               regReturn
           end;

         (* Move the stack pointer if necessary. *)
         val stackArgs =
            if numOfArgs  <= argRegs then 0 else numOfArgs  - argRegs;
            
         val stackArgsToPass =
            if argsToPass <= argRegs then 0 else argsToPass - argRegs;
           
         val diffInArgs = stackArgs - stackArgsToPass;
         
         (* One more "arg" if the return address is passed on the stack. *)
         val adjust    : int = if returnReg regEq regNone then 1 else 0;
         val stackMove : int = realstackptr transtable + diffInArgs - adjust;
      in
         resetStack (stackMove, cvec);

         (* Call the function.  If it's not recursive we have to get the
            entry point. *)
         case codeAddrOpt of
            NONE => jumpToFunction (Recursive, returnReg, cvec)
         |  SOME codeAddr => 
                jumpToCode(codeAddr, isIndirect, returnReg, transtable, cvec);

         (* Unlock any registers we locked. *)
         List.app (fn r => unlockRegister (transtable, r))
            registersLocked;
         (* Remove the arguments and code/closure registers. *)
         List.app (fn index => removeStackEntry(transtable, index))
            (argEntries @ callEntries);
         (* Since we've exited we don't need to clear the cache. *)

         setupResult (argsToPass, needsResult, true, transtable)
      end;  (* jumpToProc *)
          
          
      (* Call a closure function, i.e. not one that requires a static link. *)
      fun callClosure (clos : codetree option): mergeResult =
      let
        val tailCall = isEndOfProc tailKind;

        local
            fun getArgRegs n =
                if n >= argRegs orelse n >= argsToPass then []
                else argReg n :: getArgRegs(n+1)
        in
            val argRegs = getArgRegs 0
        end

        (* Get the set of registers modified by this call.  We have to include
           the argument, closure and code registers even if they're not actually
           modified because otherwise we may find that we've locked them. *)
        val registerSet =
            case clos of
                SOME (Constnt w) =>
                    regSetUnion(listToSet(regClosure :: argRegs), getRegisterSet w)
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
                  val regIndex =
                    loadToSpecificReg 
                      (cvec, transtable, reg, addr, false (* was bodyCall *));
                in  
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
                  if discardClosure then (NONE, false, [], [])
                  else (NONE, false, [loadReg regClosure closureOrSlAddr], [regClosure])
        end;

      in
        if tailCall
        then jumpToProc (argList, loadClosureProc)
        else callProc (argList, [procLocn], registerSet, loadClosureProc)
      end (* callClosure *)

    in (* body of genEval *)
      case evalFun of
        Constnt (oper : machineWord) =>
    let
      val args = argList;
      val addr = toAddress oper;
  
      (* Unary operations are generated as binary operations where the second
         argument is a constant.  e.g. neg(x) is generated as revsub(x, 0). *)
      fun genU i ri (constnt:machineWord): mergeResult =
        case args of
          [arg] => 
           (* Check that the instruction is implemented. *)
        if instrIsRR i orelse instrIsRI (i, constnt)
        then let
          val locnOfArg1 : stackIndex = genToStack (arg, matchFailFn);
          val locnOfArg2 : stackIndex = pushConst (transtable, constnt);
          val regHint =
            case whereto of
              ToReg prefReg => UseReg prefReg
            | _             => NoHint
        in
          MergeIndex(binaryOp (locnOfArg1, locnOfArg2, i, ri, transtable, cvec, regHint))
        end
        else callClosure (SOME evalFun) (* Have to use a function call *)
        | _ => raise InternalError 
               "genU: compiling unary operator (argcount <> 1)";
  
      fun genB i ri : mergeResult =
        case args of
          [arg1,arg2] =>
         (* Check that the instruction is implemented.  N.B. if the
        first argument is a constant we  will use the reversed
        instruction. It may only be implemented for constant values
        so it  is not sufficient to check that the general form is
        implemented. *)
        if instrIsRR i orelse
          (case arg1 of Constnt w => instrIsRI (ri, w) | _ => false) orelse
          (case arg2 of Constnt w => instrIsRI (i, w)  | _ => false)
        then let
          val locnOfArg1 : stackIndex = genToStack (arg1, matchFailFn);
          val locnOfArg2 : stackIndex = genToStack (arg2, matchFailFn);
          val regHint =
            case whereto of
              ToReg prefReg => UseReg prefReg
            | _             => NoHint
        in
          MergeIndex(binaryOp (locnOfArg1, locnOfArg2, i, ri, transtable, cvec, regHint))
        end
        else (* Have to use a function call *) callClosure (SOME evalFun)
        | _ => raise InternalError "genB: compiling binary operator (argcount <> 2)";

      fun genAllocStore () : mergeResult =
        case args of
          [Constnt lengthCnst, Constnt flagsCnst, value] =>
         if isShort lengthCnst andalso isShort flagsCnst
         then let
           (* Allocstore always constructs mutable segments and sets the mutable bit. *)
           val flags = Word8.orb(Word8.fromLargeWord(Word.toLargeWord(toShort flagsCnst)), F_mutable);
         in
           if flags = F_mutable_words
              (* Add byte segments if/when we have byte assignment. *)
             (* orelse
              wordEq (flags, F_mutable_bytes) *)
           then let (* only do easy cases *)
             val length : int = Word.toInt (toShort lengthCnst);
           in
             (* only in-line small allocations (principally refs) *)
             if 0 < length andalso length < 5
             then let (* do it *)
               val locn     = genToStack (value, matchFailFn);
               val () = incrUseCount (transtable, locn, length - 1) 
               val vec      = callgetvec (length, flags, whereto);
               val (storeKind, unitSize) =
                  if wordEq (flags, F_mutable_words)
                  then (STORE_WORD, wordSize)
                  else (STORE_BYTE, 1)

               fun fillVec byteOffset =
                 if byteOffset < 0 then ()
                 else
                    (
                    moveToVec (vec, locn, byteOffset, storeKind, cvec, transtable);
                    fillVec (byteOffset - unitSize)
                    )

             in
               fillVec ((length - 1) * wordSize);
               MergeIndex vec
             end
             else (* too big to in-line (could use loop?) *)
               callClosure (SOME evalFun)
           end
           else (* byte/code segments are too tricky to in-line *) 
             callClosure (SOME evalFun)
         end
         
         else (* crazy length or flag *)
           callClosure (SOME evalFun)
  
        | _ => (* probably non-constant length *)
           callClosure (SOME evalFun);

      fun genAssign isWord : mergeResult =
        case args of
          [addr,offset,value] =>
            if isIndexedStore isWord orelse
                (case offset of
                    Constnt w =>
                        (* The index ought always to be short.  If it is we
                           can use the normal store functions, which are always
                           provided. *)
                        isShort w
                    | _ => false)
            then
            let
              val locnOfAddr : stackIndex = genToStack (addr, matchFailFn);
              val locnOfOffset : stackIndex = genToStack (offset, matchFailFn);
              val locnOfValue : stackIndex = genToStack (value, matchFailFn);
            in
              assignOp (locnOfAddr, locnOfOffset, locnOfValue, isWord, transtable, cvec);
              (* Put in a unit result if necessary. *)
              if needsResult
              then MergeIndex(pushConst (transtable, DummyValue))
              else NoMerge (* Unused. *)
            end
            else (* Have to use a function call *) callClosure (SOME evalFun)
        | _ => raise InternalError "genAssign: argcount <> 3)";

    in
      if isIoAddress addr
      then
        (
  
          if wordEq (oper,ioOp POLY_SYS_get_length)
         then genU instrVeclen  instrBad     Zero (* dummy argument *)

          else if wordEq (oper,ioOp POLY_SYS_get_flags)
         then genU instrVecflags  instrBad   Zero (* dummy argument *)

         else if wordEq (oper,ioOp POLY_SYS_get_first_long_word)
         then genU instrGetFirstLong  instrBad  Zero (* dummy argument *)

          else if wordEq (oper,ioOp POLY_SYS_string_length)
         then genU instrStringLength instrBad   Zero (* dummy argument *)
         
          else if wordEq (oper,ioOp POLY_SYS_set_string_length)
        then genB instrSetStringLength    instrBad
        
          else if wordEq (oper,ioOp POLY_SYS_aplus)
        then genB instrAddA    instrAddA
        
          else if wordEq (oper,ioOp POLY_SYS_aminus)
        then genB instrSubA    instrRevSubA
        
          else if wordEq (oper,ioOp POLY_SYS_amul)
        then genB instrMulA    instrMulA 
        
          else if wordEq (oper,ioOp POLY_SYS_aneg)
        then genU instrRevSubA instrSubA    Zero
        
          else if wordEq (oper,ioOp POLY_SYS_not_bool)
        then genU instrXorW    instrXorW    True (* xor with "true" *)
        
          else if  wordEq (oper,ioOp POLY_SYS_or_word)
        then genB instrOrW  instrOrW 
        
          else if  wordEq (oper,ioOp POLY_SYS_and_word)
        then genB instrAndW  instrAndW 
        
          else if wordEq (oper,ioOp POLY_SYS_xor_word)
        then genB instrXorW  instrXorW
        
          else if wordEq (oper,ioOp POLY_SYS_shift_left_word)
        then genB instrUpshiftW instrBad
        
          else if wordEq (oper,ioOp POLY_SYS_shift_right_word)
        then genB instrDownshiftW instrBad
        
          else if wordEq (oper,ioOp POLY_SYS_shift_right_arith_word)
        then genB instrDownshiftArithW instrBad
        
          else if wordEq (oper,ioOp POLY_SYS_xor_word)
        then genB instrXorW  instrXorW
        
          else if wordEq (oper,ioOp POLY_SYS_mul_word)
        then genB instrMulW  instrMulW
        
          else if wordEq (oper,ioOp POLY_SYS_plus_word)
        then genB instrAddW  instrAddW
        
          else if wordEq (oper,ioOp POLY_SYS_minus_word)
        then genB instrSubW  instrRevSubW
        
          else if wordEq (oper,ioOp POLY_SYS_div_word)
        then genB instrDivW  instrBad
        
          else if wordEq (oper,ioOp POLY_SYS_mod_word)
        then genB instrModW  instrBad
        
          else if wordEq (oper,ioOp POLY_SYS_load_byte)
        then genB instrLoadB instrBad
        
          else if wordEq (oper,ioOp POLY_SYS_load_word)
        then genB instrLoad instrBad
      
          else if wordEq (oper,ioOp POLY_SYS_alloc_store)
        then genAllocStore ()

          else if wordEq (oper,ioOp POLY_SYS_assign_word) andalso inlineAssignments
          then genAssign STORE_WORD

          else if wordEq (oper,ioOp POLY_SYS_assign_byte) andalso inlineAssignments
          then genAssign STORE_BYTE

  
  (* The point of the following code is to call genCond, which will call genTest
     which will hopefully use machine instructions for these operations.
     We could avoid this by duplicating most of the body of genTest
     (the "jumping" boolean code generator) here, but we would like to
     avoid that. SPF 21/11/96
  *)
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
         wordEq (oper,ioOp POLY_SYS_word_lss))
        then
          genCond
            (Eval {function = evalFun, argList = List.map(fn a => (a, GeneralType)) argList,
                   earlyEval = false, resultType=GeneralType},
             constntTrue, constntFalse, whereto, tailKind, matchFailFn, NONE)
          else (* unoptimised I/O call *)
        callClosure (SOME evalFun)
        )
  
      else (* All other constant functions. *) callClosure (SOME evalFun)
    end

      | Extract {fpRel, addr, level, lastRef, ...} =>
    let
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

      (* SPF 20/5/95 *)
      val selfCall = not fpRel andalso level = 0 andalso addr = 0;
      
      val staticCall = 
        if fpRel (* Local or parameter *)
        then addr > 0 (* If not it must be a parameter - must be closure *)
        andalso isProcB (transtable, addr) (* local - look in table *)
        else isStaticLink addr;  (* Non-local or recursive. *)
    in 
      (* Is this a static link call? *)
      if staticCall
      then let
         (* Cannot use a jump to local static-link procedures because 
        we may want local declarations on the current stack. *)
        val tailCall = isEndOfProc tailKind andalso not fpRel;
  
        (* Load and lock regClosure. Returns the indexes of
           these entries in the stack. *)
        fun loadStaticLinkProc (): (stackIndex option * bool * stackIndex list * reg list) =
          if selfCall (* recursive *)
          then let
            (* Do we really need *exclusive* use of this register? 
               Perhaps this is to force the old value onto the stack if
               we haven't saved it yet, but we should have done this
               in pushAllBut (below) unless we're in a tail-call in
               which case we're not coming back. SPF 23/5/95 *)
            val SL = closureOrSlAddr
            val closureIndex =
              loadToSpecificReg (cvec, transtable, regClosure, SL, false);
            val () = lockRegister (transtable, regClosure);
          in
            (NONE, false, [closureIndex], [regClosure])
          end
                 
          else if fpRel (* Local *)
          then let
            (* Load entry point - this must be a local not an argument. *)
            val entryPt = pstackForDec (transtable, addr);
            (* We have already incremented the reference count when
               this is not the last reference so we don't need to do
               anything here.  DCJM 21/12/00. *)
            
            (* Get the static link register. Will set its value later. *)
            val () = getRegister (transtable, cvec, regClosure);
            val closureIndex = pushReg (transtable, regClosure);
            val () = lockRegister (transtable, regClosure);
          in 
            (* Set value of static link register now. The static link entry
               is now the address of the frame.  DCJM 2/1/01. *)
            genStackOffset (regClosure, (realstackptr transtable - 1)*wordSize, cvec);
            (SOME entryPt, false, [closureIndex], [regClosure])
          end
          
          else let (* Non-local or recursive. *)
            fun pushIt () = closureOrSlAddr;
            
            (* load SL register and return code address *)
            val (entryPt, slIndex) = 
              loadStaticLink (addr, pushIt, transtable, cvec);
          in
            lockRegister (transtable, regClosure);
            (SOME entryPt, false, [slIndex], [regClosure])
          end (* loadStaticLinkProc *);

        local
            fun getArgRegs n =
                if n >= argRegs orelse n >= argsToPass then []
                else argReg n :: getArgRegs(n+1)
            val argRegs = listToSet(regClosure :: getArgRegs 0)
        in
            val registerSet =
                if selfCall then allRegisters (* Have to push everything. *)
                else if fpRel (* Local *)
                then if addr < 0
                then raise InternalError "static link function is an argument"
                else regSetUnion(argRegs,
                        getFunctionRegSet(pstackForDec (transtable, addr), transtable))
                else (* Non local *) regSetUnion(argRegs, staticLinkRegSet addr)
        end
      in
        (* Add the registers modified by the function we're calling to those
           modified by this function.  Don't need to do that in the recursive
           case. *)
        if selfCall then ()
        else addModifiedRegSet(transtable, registerSet);
        (* Set the use count on the static link.  I don't know whether this
           is needed in all cases so it could result in us incrementing it
           unnecessarily.  It isn't needed if this function is local.
           DCJM 1/12/99. *)
        (* Increment the reference on the code if it's local and this is
           not the last reference.  If it's non-local we don't actually
           change the reference count when we load it.
           DCJM 21/12/00. *)
        if lastRef 
        then ()
        else if fpRel (* It's local. *)
        then incrUseCount(transtable, pstackForDec (transtable, addr), 1)
        else incrUseCount(transtable, closureOrSlAddr, 1);

        if tailCall
        then jumpToProc (argList, loadStaticLinkProc)
        else callProc(argList, [], registerSet, loadStaticLinkProc)
      end
   
      (* Closure call - check for recursive calls. *)
      else
        (
        (* Set the use count on the closure register if this is a
           recursive call.  We have to do that for the recursive case
           because we don't pass the Extract entry in to callClosure.
           DCJM 1/12/99. *)
        if selfCall andalso not lastRef andalso not discardClosure
        then incrUseCount(transtable, closureOrSlAddr, 1)
        else();
        callClosure (if selfCall then NONE else SOME evalFun)
        )
    end

      | _ => (* The procedure is not being found by simply loading a value
              from the stack or the closure and is not a constant. *)
        callClosure (SOME evalFun)
    end (* genEval *);


    val _ = genToRegister (pt, ToReg regResult, EndOfProc, matchFailed, NONE);

    val () = if not (haveExited transtable) then exit () else ()
  in
    
    (* Having code generated the body of the procedure,
       it is copied into a new data segment. *)
    copyCode (cvec, maxstack transtable, getModifedRegSet transtable)
  end (* codegen *);

  fun gencode (Lambda { name, body, argTypes, ...}, debugSwitches, _) =
    let (* We are compiling a procedure. *)
        val numArgs = List.length argTypes
      (* It is not essential to treat this specially, but it saves generating
         a piece of code whose only function is to return the address of the
         procedure. *)
       
      (* make the code buffer for the new procedure. *)
      val newCode = codeCreate (false (* make a closure *), name, debugSwitches); 

     (* The only non-local references will be references to the
        closure itself. We have to fetch these from the constants
        section because:
        (1) we don't save the closure register in the function body
        (2) we don't even initialise it if we use the PureCode
        calling convention
        SPF 2/1/97
      *)
      val closureAddr : address =
        codegen
            (body,
            newCode,
            fn (_ , _, newtab, _) => pushCodeRef (newtab, newCode),
            fn _ => false,
            fn (_,  _, _, _) => raise InternalError "Not static link",
            fn _ => raise InternalError "Not static link",
            true, (* Discard regClosure *)
            numArgs,
            debugSwitches);

      val res : machineWord = toMachineWord closureAddr;
    in
      (* Result is a procedure which returns the address of the procedure. *)
      (fn () => res)
    end
 
  | gencode (pt, debugSwitches, _) =
    let (* Compile a top-level expression. *)
      val newCode = codeCreate (false (* make a closure *), "<top level>", debugSwitches);

     (* There should be *no* non-local references. SPF 2/1/97 *)
      val closureAddr : address =
        codegen 
            (pt,
            newCode,
            fn (_, _, _, _) => raise InternalError "top level reached",
            fn _ => false,
            fn (_, _, _, _) => raise InternalError "Not static link",
            fn _ => raise InternalError "Not static link",
            true,  (* Discard regClosure *)
            0,    (* No args. *)
            debugSwitches);
    in (* Result is a procedure to execute the code. *)
      fn () => call (closureAddr, toMachineWord ())
    end (* gencode *);
    
end; (* GCODE functor body *)
