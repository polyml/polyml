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
    Title:      Pseudo-stack Operations for Code Generator.
    Author:     Dave Matthews, Edinburgh University / Prolingua Ltd.
    Copyright   D.C.J. Matthews 1991
*)

(*
    Modified and improved.  David C.J. Matthews 2000-2001.
*)
(* This part of the code-generator deals with the pseudo-stack and the
  translation of addresses into stack offsets. *)

functor TRANS_TAB (

(*****************************************************************************)
(*                  CODECONS                                                 *)
(*****************************************************************************)
structure CODECONS :
sig
  type machineWord;
  type code;
  type reg;   (* Machine registers *)
  type addrs;
  type address;
  datatype storeWidth = STORE_WORD | STORE_BYTE
  
  val canShareRegs : bool; (* True if we have genuine 3-address instructions. *)
  
  val regNone:     reg;
  val regStackPtr: reg;
  val regClosure:  reg;
  
  val regs:    int;     (* No of registers. *)
  
  val regN:   int -> reg;
  val nReg:   reg -> int;
  
  val regEq:    reg * reg -> bool;
  val regNeq:   reg * reg -> bool;
  
  val regRepr: reg -> string;

  val genLoad:        int * reg * reg * code -> unit;
  val genStore:       reg * int * reg * storeWidth * reg * code -> unit;
  val isStoreI:       machineWord * storeWidth * bool -> bool;
  val genStoreI:      machineWord * int * reg * storeWidth * reg * code -> unit;
  val genLoadCoderef: code * reg * code -> unit;
  val genLoadPush:    int * reg * code -> unit;
  val genPush:        reg * code -> unit;
  val preferLoadPush: bool;
  val genStackOffset: reg * int * code -> unit;

  type instrs;
  
  val instrMove:    instrs;
  val instrPush:    instrs;

  val instrIsRI: instrs * machineWord -> bool; (* Is the immediate value ok? *)

  val genRR: instrs * reg * reg * reg * code -> unit;
  val genRI: instrs * reg * machineWord * reg * code -> unit;

  type tests;
  
  val isCompRI: tests * machineWord -> bool;
  
  val resetStack: int * code -> unit;  (* Set a pending reset *)
  val procName:   code -> string;      (* Name of the procedure. *)

  type labels; (* The source of a jump. *)

  val noJump: labels;
  
  val compareAndBranchRR: reg * reg * tests * code -> labels;
  val compareAndBranchRI: reg * machineWord * tests * code -> labels;

  val unconditionalBranch: code -> labels;

  val jumpback: addrs * bool * code -> unit; (* Backwards jump. *)

  val fixup: labels * code -> unit; (* Fix up a jump. *)

  type handlerLab;
  
  val loadHandlerAddress:  reg * code -> handlerLab;
  val fixupHandler: handlerLab * code -> unit;
  
  val ic : code -> addrs

  val codeAddress: code -> address option

  datatype callKinds =
        Recursive
    |   ConstantFun of machineWord * bool
    |   CodeFun of code
    |   FullCall
  
  val callFunction:       callKinds * code -> unit;
  val jumpToFunction:     callKinds * reg * code -> unit;

  val traceContext: code -> string;
end (* CODECONS *);

(*****************************************************************************)
(*                  DEBUG                                                    *)
(*****************************************************************************)
structure DEBUG :
sig
    (* Produce debugging output. *)
    val pstackTraceTag : bool Universal.tag
    val getParameter :
       'a Universal.tag -> Universal.universal list -> 'a
end;

structure PRETTY: PRETTYSIG

(*****************************************************************************)
(*                  MISC                                                     *)
(*****************************************************************************)
structure MISC :
sig
  exception InternalError of string;
  
  type 'a iter
   
   val Iter :
       { 
         continue: bool,
         value:    unit -> 'a,
         next:     unit -> 'a iter
       } -> 'a iter

  val for : 'a iter -> ('a -> 'b) -> unit;
  val revfoldIterator : ('a -> 'b -> 'b) -> 'b -> 'a iter -> 'b
end;

(*****************************************************************************)
(*                  ADDRESS                                                  *)
(*****************************************************************************)
structure ADDRESS :
sig
  type machineWord;
  type short = Word.word;
  type address;

  val isShort : 'a -> bool;
  val toShort : 'a  -> short;
  val toMachineWord:   'a     -> machineWord;
  val toAddress:   machineWord -> address;
  val loadWord:    address * short -> machineWord;
  val wordEq  : machineWord * machineWord -> bool;
  val wordSize: int
  val length:      address -> Word.word;
end;


(*****************************************************************************)
(*                  TRANSTAB sharing constraints                             *)
(*****************************************************************************)

sharing type
  ADDRESS.machineWord
= CODECONS.machineWord
) : 
   
(*****************************************************************************)
(*                  TRANSTAB export signature                                *)
(*****************************************************************************)
sig
  type machineWord;

  type ttab;
  type code;
  type reg;   (* Machine registers *)
  type tests;
  type instrs;
  type addrs;
  type savedState;
  type storeWidth;
  type regSet
  
  val ttabCreate: Universal.universal list -> ttab;
  
  (* Register allocation *)
  val getRegister:    ttab * code * reg -> unit;
  val getAnyRegister: ttab * code -> reg;
  val freeRegister:   ttab * reg -> unit;
  val addRegUse:      ttab * reg -> unit;
  val clearCache:     ttab -> unit;
  val removeRegistersFromCache: ttab * regSet -> unit;

  (* Stack handling *)
  type stackIndex;
  
  val noIndex: stackIndex;
  
  (* For debugging only - not used in "official" builds *)
  val printStack:   ttab -> string -> string -> unit
  
  (* Push entries *)
  val pushReg:      ttab * reg  -> stackIndex;
  val pushStack:    ttab * int  -> stackIndex;
  val pushConst:    ttab * machineWord -> stackIndex;
  val pushCodeRef:  ttab * code -> stackIndex;
  val pushNonLocal: ttab * ttab * stackIndex * (unit -> stackIndex) * code -> stackIndex;
  val pushAllBut:   ttab * code * ((stackIndex -> unit) -> unit) * regSet -> unit;
  val pushNonArguments: ttab * code * stackIndex list * regSet -> reg list;
  val pushAll:      ttab * code -> unit;
  val pushSpecificEntry: ttab * code * stackIndex -> unit;
  val incsp:        ttab -> stackIndex;
  val decsp:        ttab*int -> unit;
  val reserveStackSpace: ttab * code * int -> stackIndex;

  (* Code entries *)
  val loadEntry:         code * ttab * stackIndex * bool -> reg*stackIndex;
  val loadToSpecificReg: code * ttab * reg * stackIndex * bool -> stackIndex;
  val containsLocal:     ttab * reg  -> unit;
  val lockRegister:      ttab * reg -> unit;
  val unlockRegister:    ttab * reg -> unit;
  val loadIfArg:         code * ttab * stackIndex -> stackIndex
  val indirect:          int * stackIndex * code * ttab -> stackIndex;
  val moveToVec:         stackIndex * stackIndex * int * storeWidth * code * ttab -> unit;

  val removeStackEntry: ttab*stackIndex -> unit;

  val resetButReload:   code * ttab * int -> unit;
  val pushValueToStack: code * ttab * stackIndex * int -> stackIndex;
  val storeInStack:     code * ttab * stackIndex * int -> unit;
  val isProcB:          ttab * int -> bool;
  val realstackptr:     ttab -> int;
  val maxstack:         ttab -> int;
  val pstackForDec:     ttab * int -> stackIndex;
  val makeEntry:        ttab * code * stackIndex * int * int * bool -> unit;
  val incrUseCount:     ttab * stackIndex * int -> unit;
  
  type stackMark;
  
  val markStack: ttab -> stackMark;
  val unmarkStack: ttab * stackMark -> unit
  
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
  val haveExited: ttab -> bool

  datatype regHint = UseReg of reg | NoHint;
  val binaryOp: stackIndex * stackIndex * instrs * instrs * ttab * code * regHint -> stackIndex;
  val assignOp: stackIndex * stackIndex * stackIndex * storeWidth * ttab * code -> unit;

  val compareAndBranch: stackIndex * stackIndex * tests * tests * ttab * code -> labels;
  
  val saveState : ttab * code -> savedState
  val startCase : ttab * code * savedState -> addrs
  
  val chooseRegister : ttab -> reg

  val getRegisterSet: machineWord -> regSet
  val getRegisterSetForCode: code -> regSet
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
end (* TRANSTAB export signature*) =

let

(*****************************************************************************)
(*                  ARRAY                                                    *)
(*****************************************************************************)
structure ARRAY :
sig
  type 'a array;

  val array  : int * '_a -> '_a array;
  val sub    : 'a array * int -> 'a;
  val update : 'a array * int * 'a -> unit;
end = Array;

(*****************************************************************************)
(*                  STRETCHARRAY                                             *)
(*****************************************************************************)
structure STRETCHARRAY :
sig
  type 'a stretchArray;

  val stretchArray  : int * '_a -> '_a stretchArray;
  val sub    : 'a stretchArray * int -> 'a;
  val update : '_a stretchArray * int * '_a -> unit;
end = StretchArray;

in

(*****************************************************************************)
(*                  TRANSTAB functor body                                    *)
(*****************************************************************************)
struct

  open CODECONS; infix 5 regEq regNeq;
  open DEBUG;
  open MISC;
  open ADDRESS;
  open ARRAY;

  datatype regHint = 
    UseReg of reg
  | NoHint;

  (* The set of registers modified by a function.  AllRegisters occurs
     so frequently that it's worth treating specially. *)
  abstype regSet = AllRegisters | SomeRegisters of reg list
  with
      (* This is the set of all registers and is used as the default if
         we are calling a non-constant function. *)
      local
          val listOfAllRegisters =
          let
             fun getRegs i l =
                if i < 0 then l else getRegs (i-1) (regN i :: l)
          in
             getRegs(regs-1) []
          end
    
          fun merge(a, []) = a
           |  merge([], a) = a
           |  merge(a as h::t, b as h'::t') =
                if h regEq h' then h :: merge(t, t')
                else if nReg h < nReg h'
                then h :: merge(t, b) else h' :: merge(a, t')
      in
          fun singleton r = SomeRegisters [r]
          val allRegisters = AllRegisters
          val noRegisters = SomeRegisters []

          fun isAllRegs AllRegisters = true | isAllRegs _ = false

          fun regSetUnion(AllRegisters, _) = AllRegisters
            | regSetUnion(_, AllRegisters) = AllRegisters
            | regSetUnion(SomeRegisters a, SomeRegisters b) =
                let
                    val merged = merge(a, b)
                in
                    if List.length merged = regs
                    then AllRegisters
                    else SomeRegisters merged
                end
    
          fun listToSet l =
             let
                (* Sort and remove duplicates. *)
                val merged = List.foldl (fn(a,b) => merge([a], b)) [] l
             in
                if List.length merged = regs
                then AllRegisters
                else SomeRegisters merged
             end
    
          fun inverseSet AllRegisters = SomeRegisters []
           |  inverseSet (SomeRegisters []) = AllRegisters
           |  inverseSet (SomeRegisters r) =
                let
                    fun diff (a as h::t, b as h'::t') =
                        if h regEq h' then diff(t, t')
                        else if nReg h < nReg h'
                        then h :: diff(t, b)
                        else diff(a, t')
                    |   diff (a, _) = a
                in
                    SomeRegisters(diff(listOfAllRegisters, r))
                end
    
          fun inSet(_, AllRegisters) = true
           |  inSet(r, SomeRegisters l) =
                let
                    fun inset [] = false
                     |  inset (h::t) = h regEq r orelse inset t
                in
                    inset l
                end
      end
    end;
 

local
 
  val cFixup  = CODECONS.fixup;
  val cFixupH = CODECONS.fixupHandler;
  
  fun forLoop f i n = if i > n then () else (f i; forLoop f (i + 1) n);
  
  nonfix sub; (* was only 5 - gave subtle bugs (SPF 11/8/94 *)

  (* local definitions, for brevity *)
  type 'a stretchArray = 'a STRETCHARRAY.stretchArray;
  val stretchArray  = STRETCHARRAY.stretchArray;
  val stretchSub    = STRETCHARRAY.sub;
  val stretchUpdate = STRETCHARRAY.update;
  
(* ...
  type baseOffset = {base: reg, offset: int};
  
  fun base   ({base  ,...}:baseOffset) = base;
  fun offset ({offset,...}:baseOffset) = offset; 
  
  fun makeBaseOffset (base: reg) (offset: int) = {base = base, offset = offset};
... *)

  abstype stackIndex = StackIndex of int 
  with
   (* fixes off-by-one problem for array lookup *)
    fun getIndex (StackIndex n) = n;
    fun stackIndex n = StackIndex n;
    val first = stackIndex 0;
    val noIndex = stackIndex ~1; (* An invalid index. *)
  
    infix 4 indexEq indexNeq;
    
    fun (StackIndex a) indexEq  (StackIndex b) = a = b;
    fun (StackIndex a) indexNeq (StackIndex b) = a <> b;
  
    infix 5 indexGt indexGeq indexLt indexPlus indexMinus;
    
    fun (StackIndex a) indexGt  (StackIndex b) = a >  b;
    fun (StackIndex a) indexGeq (StackIndex b) = a >= b;
    fun (StackIndex a) indexLt  (StackIndex b) = a <  b;
    
    fun (StackIndex a) indexPlus  n = StackIndex (a + n);
    fun (StackIndex a) indexMinus n = StackIndex (a - n);
  end; 
  

  datatype stackUnion = 
    Register  of reg         (* In a register *)
  | Literal   of machineWord        (* A constant (was "int") *)
  | CodeRef   of code        (* Forward reference to code *)
  | Direct    of {base: reg, offset: int}  (* Register/Offset *)
  | Stack     of int         (* On the real stack. *)
  | Container of stackIndex list (* A group of stack entries. *)

  datatype stackEntry =
     NoStackEntry
   | StackEntry of 
      {
        ent:   stackUnion,
        cache: reg, (* regNone means not cached. *)
        uses:  int,
        destStack: int
            (* destStack is used to indicate where on the stack this
               entry must be placed if it has to be spilled.  A value
               of ~1 means that we don't care.  The reason for having
               this is to ensure that if we split a flow of control
               (e.g. the then- and else-parts of an "if") with a value
               in a register and have to spill the register in one branch
               then we spill it to the same location on the other branch.
               This ensures that when we merge the flows of control we don't
               have to mess around with the stack.  *)
      };
  
    fun makeStackEntry union reg i dest =
      StackEntry { ent = union, cache = reg, uses  = i, destStack = dest };

  val initTrans = 5; (* Initial size of tables. *)
  val initStack = 10;

    fun forIndDownTo(i, finishInd, perform: stackIndex->unit) =
        if i indexGeq finishInd
        then (perform i; forIndDownTo (i indexMinus 1, finishInd, perform))
        else ()

    fun revfoldIndDownTo(f, x, i, finishInd) =
        if i indexGeq finishInd
        then revfoldIndDownTo(f, f i x, i indexMinus 1, finishInd)
        else x

  (* The set of available registers. A register is free if its use-count
     is zero. A register has a reference for each occurence in an entry
     on the pseudo-stack or cache field (even if the use-count of the
     stack entry is zero). *)
  (* Added modification set to indicate if a register has been changed.
     We assume that if a register is allocated it has been modified.
     DCJM 26/11/00. *)
     
  type rset = {vec: int array, nextr: int ref, modSet: regSet ref, freeRegs: int ref};

  fun Vec   ({vec  ,...}:rset) = vec;
    
  val vecSize = regs;

  fun nextRegNo ({nextr,...}:rset) : int = !nextr;
  fun setNextRegNo ({nextr,...}:rset,n : int) : unit = nextr := n;
  
  (* Returns the first free register.  The start of the sequence is different
     each time to try to achieve a spread of usage. *)
  (* findFree seems to be biggest hot-spot in the whole compiler so we
     have to code it quite carefully. *)
  fun getAcceptableRegister ({vec,nextr,modSet,freeRegs}:rset, acceptable: regSet) =
  let
    val nextReg = ! nextr; 

    fun isAcceptable r = inSet(r, acceptable)       
    
    fun next n = if n = 0 then (vecSize - 1) else (n - 1);
    
    fun allocReg r =
    (* Mark the register as in use. *)
    let
        val i = nReg r
    in
        update (vec, i, 1); (* Set the register use-count to 1 *)
        modSet := regSetUnion(singleton r, ! modSet); (* Mark as used *)
        (* Next time start looking at the register before this. *)
        nextr := next i;
        freeRegs := !freeRegs - 1;
        r
    end
    
    (* SPF 7/6/94 fixed off-by-one problem *)
    (* Pick a modified register if possible to keep the
       set small. DCJM 26/11/00 *)
    fun findFree (i : int, firstChoice) : reg =
    let
        val useCount = sub (vec, i)
        and isModified = inSet(regN i, !modSet)
        and acceptable = isAcceptable(regN i)
    in
      if useCount = 0 andalso isModified andalso acceptable
      then (* Free and it's been modified already - use it. *)
         allocReg (regN i)
      else let (* Either it's in use or it hasn't been modified yet. *)
        val n = next i;
        val nextChoice =
            (* If we haven't found a register before and this is
               free pick this one. *)
            if firstChoice regEq regNone andalso useCount = 0 andalso acceptable
            then regN i
            else firstChoice
      in
        if n = nextReg
        then (* None free or the only one we found was unmodified. *)
            if nextChoice regEq regNone then regNone (* Not found. *)
            else allocReg nextChoice
        else findFree(n, nextChoice)
      end
    end;

  in
     if !freeRegs = 0
     then regNone
     else findFree (nextReg, regNone)
  end;

  (* Get any register. *)
  fun getReg rset = getAcceptableRegister(rset, allRegisters)

  (* Print the allocated registers. *)
  fun printRegs printStream regs =
  let
    fun printReg i =
    let
      val useCount = sub (Vec regs, i)
    in
      if useCount > 0
      then
        (
          printStream " "; printStream (regRepr (regN i));
          printStream "="; printStream(Int.toString useCount);
          {}
        )
      else ()
    end (* printReg *);
  in
    forLoop printReg 0 (vecSize - 1)
  end (* printRegs *);


  fun getRset ({vec=v, modSet, freeRegs, ...}: rset) reg =
  let
    val r = nReg reg; (* SPF 7/6/94 - fixed off-by-one *)
    val useCount = sub (v, r)
  in
    if useCount = 0
    then
        (
        update (v, r, 1);
        modSet := regSetUnion(singleton reg, ! modSet);
        freeRegs := !freeRegs - 1
        )
    else raise InternalError ("getRset: can't get register #" ^ Int.toString r)
  end;

  fun free (regs as {freeRegs, ...}) reg =
  let
    val r = nReg reg; (* SPF 7/6/94 - fixed off-by-one *)
    val v = Vec regs;
    val useCount = sub (v, r)
  in
    if useCount = 0
    then raise InternalError ("free: register already free #" ^ Int.toString r)
    else
        (
        if useCount = 1
        then freeRegs := !freeRegs + 1
        else ();
        update (v, r, useCount - 1)
        )
  end;
  
  (* Increment use count. *)
  fun incr (regs as {freeRegs, ...}) reg =
  let
    val r = nReg reg; (* SPF 7/6/94 - fixed off-by-one *)
    val v = Vec regs;
    val useCount = sub (v, r)
  in
    if useCount = 0
    then freeRegs := !freeRegs - 1 else ();
    update (v, r, useCount + 1)
  end;

  (* SPF 7/6/94 - fixed off-by-one *)
  fun rsetMake () : rset =
     {vec = array (vecSize, 0),
      nextr = ref (vecSize - 1),
      modSet = ref noRegisters,
      freeRegs = ref regs}

  fun usage regs reg =
  let
    val r = nReg reg; (* SPF 7/6/94 - fixed off-by-one *)
    val v = Vec regs;
    val useCount = sub (v, r)
  in
    useCount
  end;
  
 (* This table maps declaration numbers for a particular procedure or      *)
 (* inline procedure block into pseudo-stack offsets. The pseudo-stack     *)
 (* simulates the real stack and gives the real locations of objects       *)
 (* which may be in store, on the real stack or in registers. It maintains *)
 (* use-counts for values and allows the stack to be contracted and        *)
 (* registers to be re-used when they are no longer required.              *)
 datatype ttab = 
   Ttab of
     {
       decToPstack:  stackIndex stretchArray,
       isProc:       bool stretchArray,
       regset:       rset,
       pstack:       stackEntry stretchArray, (* Pseudo-stack *)
       pstackptr:    stackIndex ref,
       realstackptr: int ref,
       maxstack:     int ref,
       exited:       bool ref,
       branched:     bool ref,
       marker:       stackIndex ref,
       lowestDirect: stackIndex ref,
       pstackTrace:  bool,
       printStream:  string->unit
     };
     
    fun ttabCreate debugSwitches =
    let
        val printStream = PRETTY.getSimplePrinter debugSwitches
    in
        Ttab
        {
            decToPstack  = stretchArray (initTrans,noIndex),
            isProc       = stretchArray (initTrans,false),
            regset       = rsetMake(),
            pstack       = stretchArray (initStack,NoStackEntry),
            pstackptr    = ref first,
            realstackptr = ref 0,
            maxstack     = ref 1,
            exited       = ref false,
            branched     = ref false,
            marker       = ref first,
            lowestDirect = ref first,
            pstackTrace  = DEBUG.getParameter DEBUG.pstackTraceTag debugSwitches,
            printStream  = printStream
            }
    end

  fun decToPstack  (Ttab {decToPstack ,...}) = decToPstack;
  fun isProc       (Ttab {isProc      ,...}) = isProc;
  fun regset       (Ttab {regset      ,...}) = regset;
  (*fun pstack       (Ttab {pstack      ,...}) = pstack;*)
  fun pstackptr    (Ttab {pstackptr   ,...}) = pstackptr;
  fun realstackptr (Ttab {realstackptr,...}) = realstackptr;
  fun maxstack     (Ttab {maxstack    ,...}) = maxstack;
  fun exited       (Ttab {exited      ,...}) = exited;
  fun branched     (Ttab {branched    ,...}) = branched;
  fun marker       (Ttab {marker      ,...}) = marker;
  fun lowestDirect (Ttab {lowestDirect,...}) = lowestDirect;
  
  val pstackVal    = ! o pstackptr;
  val realstackVal = ! o realstackptr;
  val maxstackVal  = ! o maxstack;
  val exitedVal    = ! o exited;
  val branchedVal  = ! o branched;
  val markerVal    = ! o marker;

  fun pstackEntry (Ttab{pstack, ...}) (locn:stackIndex) : stackEntry = 
    stretchSub (pstack, getIndex locn);

  (* Returns the real entry from a chain of "copy" entries.
     Now that "copy" entries have been removed this just returns
     the entry. *)
  fun pstackRealEntry (Ttab{pstack, ...}) (locn:stackIndex) =
      case (stretchSub (pstack, getIndex locn)) of
        NoStackEntry                    =>
           raise InternalError ("pstackRealEntry: not entry: " ^ Int.toString(getIndex locn))
           
      | StackEntry record => (locn, record)


   (* An iterator over the pseudo stack. Returns only entries which are not nil,
      but it will return entries whose use counts have gone to zero, but which
      still have useful values in their caches. *)
  fun overStack table downwards =
    if downwards
    then let
      (* The basis is an index to the stack *)
      fun next i =
      let
        val nxt = i - 1;
      in (* At the bottom ? *)
        if nxt < 0
        then nxt
        else 
          case (pstackEntry table (stackIndex nxt)) of
            NoStackEntry => 
              next nxt (* Empty entry - must continue *)
              
          | StackEntry _ =>
              nxt
      end;

      fun mkIter i =
        Iter
          {
             continue = i >= 0,
             value    = fn () => stackIndex i,
             next     = fn () => mkIter (next i)
          };
    in
      mkIter (next (getIndex (pstackVal table)))
    end

    else let (* upwards *)
      fun next i =
      let
        val nxt = i + 1;
      in 
        if nxt >= getIndex (pstackVal table)
          then nxt
        else 
          case (pstackEntry table (stackIndex nxt)) of
            NoStackEntry => 
              next nxt (* Empty entry - must continue *)
              
          | StackEntry _ =>
              nxt
      end;
  
      fun mkIter i = Iter { continue = i < getIndex (pstackVal table),
                            value    = fn () => stackIndex i,
                            next     = fn () => mkIter (next i) };
    in
      mkIter (next ~1)
    end (* overStack *);
   
   fun printStackUnion printStream stackun =
      case stackun of
        Register reg => 
          printStream(regRepr reg)
          
      | Literal w =>
          if isShort w
          then printStream(Int.toString (Word.toIntX (toShort w)))
          else printStream "?" (* ??? *)      
      
      | CodeRef si =>
          (
            printStream "(";
            printStream (procName si);
            printStream ")"
          )

     | Direct {base, offset} =>
         (
           printStream(regRepr base);
           printStream "@(";
           printStream(Int.toString offset);
           printStream ")"
         ) 

     | Stack i =>
        (
          printStream "base@(";
          printStream(Int.toString i);
          printStream ")"
        )

     | Container l =>
        (
          printStream "[";
          List.app (fn i => (printStream(Int.toString(getIndex i)); printStream " ")) l;
          printStream "]"
        )

  fun printEntry _ NoStackEntry _ = ()
    | printEntry printStream (StackEntry {ent, uses, cache, destStack}) entry =
    (
      printStream(Int.toString(getIndex entry));
      printStream " ";
      
      printStream(Int.toString uses);
      printStream " ";
      
      printStackUnion printStream ent;
      
      if cache regNeq regNone
      then (printStream " in "; printStream(regRepr cache)) 
      else ();

      if destStack = ~1 then ()
      else (printStream " to base@("; printStream(Int.toString(destStack* ~wordSize)); printStream")");
        
      printStream "\n"
    );

  fun printStack (table as Ttab {printStream, ...}) why whereTo =
  ( 
    printStream ("\n" ^ why ^ " (" ^ whereTo ^ ")\n"); 
    printStream "psp=";
    printStream(Int.toString(getIndex (pstackVal table)));
    printStream " lim=";
    printStream(Int.toString(getIndex (markerVal table)));
    printStream " rsp="; 
    printStream (Int.toString(realstackVal table));
    printStream "\n";
    printStream "regs=";
    printRegs printStream (regset table);
    printStream "\n";
    for (overStack table true (* downwards *))
        (fn entry => printEntry printStream (pstackEntry table entry) entry)
  );
  
  (* Removes empty entries from the top of the stack. *)
  fun clearOff table =
  let
    val newIndex = (pstackVal table) indexMinus 1
  in
    if newIndex indexGeq (markerVal table)
     then
       case (pstackEntry table newIndex) of
         NoStackEntry => 
           (
             pstackptr table := newIndex;
             clearOff table 
           )
       | StackEntry _ => ()
     else ()
  end;  (* clearOff *)

  (* Removes an entry which is no longer required. If the entry is cached it *)
  (* may be retained unless it refers to the stack or another register when  *)
  (* it MUST be removed. *)
  fun removeEntry (table as Ttab{pstack, ...}) entry keepIfCache =
    case (pstackEntry table entry) of
      NoStackEntry => () 
       (* now handles this case, so we don't have to check 
          before calling "removeEntry" (SPF 19/11/94) *)
      
    | StackEntry {ent = stacken, cache, ...} =>
        (* If we are removing an entry from the real stack it must not be
           retained in the cache since we may push something else into that
           location. Actual parameters to procedures are not use-counted in
           the same way as locals so it is worth keeping them cached. *)
        let
          val cacheReg = 
            if cache regEq regNone
              then regNone
            else if not keepIfCache orelse 
                 (case stacken of
                    Register _ => true
                  | Stack i      => i < 0
                  | _            => false)
            
               then (* Clear cache. *)
                ( 
                  free (regset table) cache;
                  regNone
                )
            else (* Retain cache value. *) cache;
        in
          if cacheReg regEq regNone
          then (* If the cache is (now) empty we can remove the entry completely. *)
            ( 
             (* clobber the entry. *)
              stretchUpdate (pstack, getIndex entry, NoStackEntry);
              
              case stacken of
                Register reg       => free (regset table) reg
              | Direct {base, ...} => free (regset table) base
              | Container l        => List.app (fn i => incrUseCount(table, i, ~1)) l
              | _                  => ()
              ;
              
                
              clearOff table
            )
         
          (* otherwise we just leave the entry there. *)
          else stretchUpdate (pstack, getIndex entry,
                       makeStackEntry stacken cacheReg 0 ~1)
        end (* end removeEntry *)

  (* Add the number of uses to the use count of an item on the stack. *)
  and incrUseCount (table as Ttab{pstack, ...}, entry, incr) : unit =
    case (pstackEntry table entry) of
      NoStackEntry => raise InternalError ("incrUseCount: no entry " ^ Int.toString(getIndex entry))
      
    | StackEntry {ent, cache, uses, destStack} =>
      let
        val newUses = uses + incr;
      in
        if newUses <=  0
        then removeEntry table entry true
        else 
          stretchUpdate (pstack, getIndex entry,
             makeStackEntry ent cache newUses destStack)
      end;

  (* True if this is the last reference to this entry. i.e. the use-count *)
  (* of this entry is 1. *)
  fun lastReference table entry =
    case (pstackEntry table entry) of
      NoStackEntry => raise InternalError "lastReference: no entry"
      
    | StackEntry {uses, ...} => uses = 1
    ;

  (* Push a value on the stack and return its location. *)
  fun pushPstack (table as Ttab{pstack, pstackTrace, ...}) entry name =
  let
    val stacktop = pstackVal table;
    val psp      = getIndex stacktop;
    val destStack =
        case entry of
            Stack addr => addr div (~wordSize)
        |   _ => ~1
  in
    stretchUpdate (pstack, psp, makeStackEntry entry regNone 1 destStack);
    pstackptr table := stacktop indexPlus 1;
    if pstackTrace then printStack table name "" else ();
    stacktop
  end;

in (* local definitions *)
  type stackIndex  = stackIndex;
  type ttab        = ttab;
  val ttabCreate   = ttabCreate;
  val noIndex      = noIndex;
  val incrUseCount = incrUseCount;
  val printStack   = printStack;

  (* Push a value onto the real stack. *)
  fun incsp table =
  let
    val stackaddr = realstackVal table * ~wordSize;
  in
    realstackptr table := realstackVal table + 1;
    
    if realstackVal table > maxstackVal table
    then maxstack table := realstackVal table
    else ();
    
    pushPstack table (Stack stackaddr) "incsp"
  end;

  (* The top of the pseudo-stack is held in a register *)
  fun pushReg (table, reg) : stackIndex = 
     pushPstack table (Register reg) "pushReg";

  (* The top of the pseudo-stack is a constant *)
  fun pushConst (table, v : machineWord) : stackIndex = 
    pushPstack table (Literal v) "pushConst";


  (* The top of the pseudo-stack is a forward reference to a procedure. *)
  fun pushCodeRef (table, rf : code) : stackIndex =
    pushPstack table (CodeRef rf) "pushCodeRef";

  fun addRegUse (table, reg) : unit = incr (regset table) reg;

  (* If we load a value into the last available register and then need to
     load another value (e.g. a second argument), it is possible for the
     first to be pushed onto the stack and the register to be re-used.  To
     avoid this we increment the use count on the first register before
     we attempt to load the second value.  This doesn't prevent the register
     being pushed onto the stack but it does prevent the register being
     reused. *)
   fun lockRegister (table as Ttab{pstackTrace, ...}, reg) = 
   let
     val () = addRegUse (table, reg);
   in
     if pstackTrace then printStack table "lockRegister" "" else ()
   end;

   fun unlockRegister (table as Ttab{pstackTrace, ...}, reg) : unit =
   let
     val () = free (regset table) reg;
   in
     if pstackTrace then printStack table "unlockRegister" "" else ()
   end;

  (* Puts a value in the real stack onto the pseudo-stack.
     Used for references to arguments that have not been
     passed in registers. *)
  fun pushStack (table, addr : int) : stackIndex =
  let (* Enter it only if it is not already there. *)
    fun search s =
      if s indexGeq pstackVal table
        then pushPstack table (Stack addr) "pushStack"
        
      else 
        case (pstackEntry table s) of
          StackEntry {ent = Stack index, ...} =>
            if index = addr
            then
              (
                incrUseCount (table, s, 1);
                s
              )
            else search (s indexPlus 1)
            
       | _ => search (s indexPlus 1)
  in
    search first
  end;

  (* Remove registers from the cache. *)
  fun removeFromCache (table as Ttab{pstack, ...}) regSet (continue: unit -> bool) =
  let
    fun ok r = inSet(r, regSet)

    fun clear (entry: stackIndex) (limit: stackIndex) =
      if entry indexLt limit
      then let
        val stackent = pstackEntry table entry;
      in
        case stackent of
          NoStackEntry => ()
          
        | StackEntry {ent = stacken, cache = cacheReg, uses, destStack} =>
          if cacheReg regEq regNone
          then ()
          else
            (* Remove the entry if the wanted register is the cache *)
            (* or the value being cached. *)
            if ok cacheReg orelse
              (case stacken of
                 Register reg      => ok reg
               | Direct {base,...} => ok base
               | _                 => false)
            then 
              (* If the use-count is positive, simply set the cache to *)
              (* empty, otherwise remove the entry. *)
              if uses = 0
              then removeEntry table entry false
              else
                ( 
                  free (regset table) cacheReg;
                  stretchUpdate (pstack, getIndex entry,
                        makeStackEntry stacken regNone uses destStack)
                )
            else ();
        
        if continue () then clear (entry indexPlus 1) limit else ()
      end
      else (); (* end clear *)
      
    val second = first indexPlus 1;
  in
   (* Do the constant pointer last.  If we are flushing the cache to free
      registers it's probably more use than the others. *)
   clear second (pstackVal table);
   if continue () then clear first second else ()
  end;

  (* Remove everything from the cache. *)
  fun clearCache table =
    removeFromCache table allRegisters (fn () => true);

  fun removeRegistersFromCache (table, regs) =
     removeFromCache table regs (fn () => true);

 
  (* The value on the stack is no longer required.
     This now just decrements the use count. *)
  fun removeStackEntry (table, index) =
    incrUseCount(table, index, ~1)

  (* Reset the real stack stack pointer after a function call. *)
  fun decsp (table as Ttab{pstackTrace, ...}, args) = 
  ( realstackptr table := realstackVal table - args;
    if pstackTrace then printStack table "decsp" "" else () 
  );

  (* Returns the pstack offset for a particular declaration number. *)
  fun pstackForDec (table, locn) : stackIndex = 
    stretchSub (decToPstack table, locn);

  (* Called when a value has been newly created and so must be local. *)
  fun containsLocal (_, _) : unit = ();

  (* Register to register move. *)
  fun moveRR rs rd cvec = genRR (instrMove, rs, regNone, rd, cvec);

  (* Frees registers by pushing values onto the stack or moving them to
     other registers.  `selectRegister' selects which registers are
     affected, `selectEntry' selects which entries are affected. Used
     either to clear all registers or just to free a particular one.
     `loadIfPoss' is true if it is sufficient to move the entry to
     another register. *)
  fun pushRegisters (table as Ttab{pstack, pstackTrace, ...}) cvec regSet selectEntry loadIfPoss =
  let
    
    fun pushEntries entry =
      if entry indexLt pstackVal table
      then let
        val stackent = pstackEntry table entry;
      in
        case stackent of
           StackEntry {uses, ent = Direct {base, offset}, cache = cacheReg,
                       destStack} =>
            (* Values which are cached but are otherwise not needed
               have a zero use-count. There is no need to push them.
               In fact, we have to explicitly clear the entry as
               otherwise getRegister could fail because the register
               could be locked by a zero-use pstack entry. SPF 26/9/95
             *)
            (* I don't understand this - we clear that register from the
               cache - try uncommenting this.  DCJM 30/11/00 *)
            (* Was uncommenting this the cause of the bug in loadEntry
               I fixed on 17/1/01?  If so the current code is better.
               DCJM 25/1/01. *)
            if (* *) uses = 0 orelse (* *) not (selectEntry entry)
            then () 
            else let 
              (* Push reg onto the stack without changing the use count.*)
              fun saveDirectOnStack () =
                (
                  alignStack(table, cvec, destStack);

                  if cacheReg regNeq regNone
                  then genPush (cacheReg, cvec)
                  else genLoadPush (offset, base, cvec);
                  
                  (free (regset table) base) : unit;
                  
                  (* Overwrite this entry with a reference to the stack. *)
                  let
                     val stackAddr = realstackVal table
                  in
                     stretchUpdate (pstack, getIndex entry,
                        makeStackEntry (Stack(stackAddr * ~wordSize))
                                cacheReg uses stackAddr)
                  end;
                            
                  realstackptr table := realstackVal table + 1;
                    
                  if realstackVal table > maxstackVal table
                  then maxstack table := realstackVal table
                  else ()
                );
              
              fun saveDirectInReg destReg =
                ( 
                  (free (regset table) base) : unit;
                  
                  if cacheReg regEq destReg
                    then () (* already cached in destination register *)
                  else if cacheReg regEq regNone
                    then genLoad (offset, base, destReg, cvec)
                    else ( (* Cached in a different register - move it there
                            and free the cache. *)
                        moveRR cacheReg destReg cvec;
                        free (regset table) cacheReg
                    );
                  
                  (* Clear out the cache and overwrite this entry with a
                      reference to the register. *)
                  stretchUpdate (pstack, getIndex entry,
                          makeStackEntry (Register destReg) regNone
                            uses destStack)
                )
            in
              if not (inSet(base, regSet))
                then ()
              else if uses = 0
                then (* discardDirect () *) removeEntry table entry false
              else if not loadIfPoss
                (* Not allowed to move it to another register. *)
                then saveDirectOnStack ()
              else if cacheReg regNeq regNone andalso not (inSet(cacheReg, regSet))
                (* It's cached in an acceptable register. *)
                then saveDirectInReg cacheReg
              else let
                (* Is there an acceptable register free?  If so load it into
                   that. *)
                val destReg =
                    getAcceptableRegister (regset table, inverseSet regSet);
              in
                if destReg regNeq regNone
                then saveDirectInReg destReg
                else saveDirectOnStack ()
              end
            end

        | StackEntry {uses, ent = Register reg, cache = cacheReg, destStack} =>
            if (* uses = 0 orelse *) not (selectEntry entry)
            then () 
            else let 
              (* Push reg onto the stack without changing the use count.*)
              fun saveRegOnStack () =
                ( 
                  alignStack(table, cvec, destStack);

                  genPush (reg, cvec) : unit;
                  
                  let
                    val newCache =
                      (* Have pushed a register - can treat the register as caching
                         the stack location we have pushed it into. *)
                      if cacheReg regEq regNone
                      then reg
                      else (free (regset table) reg; cacheReg);
                    val stackAddr = realstackVal table
                  in
                    (* Overwrite this entry with a reference to the stack. *)
                    stretchUpdate (pstack, getIndex entry,
                            makeStackEntry (Stack(stackAddr * ~wordSize))
                                newCache uses stackAddr);

                    realstackptr table := realstackVal table + 1;
                    
                    if realstackVal table > maxstackVal table
                    then maxstack table := realstackVal table
                    else ()
                  end
                );
              
              (* If we have any direct references using this register
                 we can adjust them to use the new register. This is
                 particularly important if we are moving values
                 out of this register because we want to load it with
                 something else. *)
              fun saveRegInReg destReg =
              let
                fun regChanged entry =
                  if entry indexLt pstackVal table
                  then
                    (
                      case (pstackEntry table entry) of
                        StackEntry {ent = Direct {base, offset},
                                        cache, uses, destStack} =>
                          if base regEq reg
                          then
                            ( (* Decrement the use count for the source reg 
                                 and increment it for the destination. *)
                              free (regset table) reg;
                              addRegUse (table, destReg);
                              stretchUpdate (pstack, getIndex entry, 
                                 makeStackEntry (Direct {base = destReg, offset = offset})
                                    cache uses destStack)
                            )
                          else ()
                        
                      | _ => ();
                      
                      regChanged (entry indexPlus 1)
                    )
                  else ();
              in
                free (regset table) reg : unit;
                
                if cacheReg regEq destReg
                then () (* already cached in destination register *)
                else moveRR reg destReg cvec;
                  
                regChanged entry; (* Start from this entry not from the bottom *)

                (* Clear out the cache and overwrite this entry with a
                   reference to the register. *)
                stretchUpdate (pstack, getIndex entry,
                     makeStackEntry (Register destReg) regNone uses destStack)

              end;
            in
              if not (inSet(reg, regSet))
                then ()
              else if uses = 0
                then (* discardReg () *) removeEntry table entry false
              else if not loadIfPoss
                then saveRegOnStack ()
              else if cacheReg regNeq regNone andalso not (inSet(cacheReg, regSet))
                then saveRegInReg cacheReg
              else let
                val destReg =
                    getAcceptableRegister (regset table, inverseSet regSet);
              in
                if destReg regNeq regNone
                then saveRegInReg destReg
                else saveRegOnStack ()
              end
            end (* let for saveReg etc. *)
            
        | _ => (); (* neither Direct nor Register *) 

        pushEntries (entry indexPlus 1)
      end (* let for stackent *)
      else () (* end pushEntries *);
  in
    pushEntries first;
    if pstackTrace then printStack table "pushRegisters" "" else ()
  end

  and pushAnyEntryAtCurrentSP(table, cvec): bool =
  (* Check that the next stack location is not the destination of an entry
     which has not yet been pushed and pushes it if it is. *)
  let
     val currentSp = realstackVal table
     fun selectEntry addr =
         case (pstackEntry table addr) of
           NoStackEntry => 
             raise InternalError "pushAnyEntryAtCurrentSP: no entry"
             
         | StackEntry {ent=Stack addr, ...} =>
             (* Ok if already pushed.  Check that we don't have an entry
                above the stack pointer. *)
             if addr > currentSp
             then raise InternalError "pushAnyEntryAtCurrentSP: entry above rsp"
             else false

         | StackEntry {destStack, ...} =>
             (* Consistency check to make sure that we haven't got an unpushed
                entry below the current sp. *)
             if destStack >= 0 andalso destStack < currentSp
             then raise InternalError "pushAnyEntryAtCurrentSP: unpushed entry"
             else destStack = currentSp (* Push it if we're there. *)
  in
     pushRegisters table cvec allRegisters (* Any register *) selectEntry false;
     (* Return true if the stack pointer has changed. *)
     realstackVal table <> currentSp
  end

  and alignStack (table, cvec, addr): unit =
    if addr < 0 (* Can use any offset. *)
     (* There is a problem when we have gaps where we have
        reserved addresses which are not consecutive.
        This can arise if we have something like:
        val a = ... val b = ...
        val c = if ...then ...(*X*)[push a]; [push b] a(last ref)
                else (if ...
                    then (*Y*)[push b because we need its register}
                    else (*Z*)[push a into the unused addr ???];
                              [push b to its explicit addr]...;
                    a(last ref)
                )
        in ... b ... end.
        At X a and b are pushed and given explicit addresses but
        a is removed at the end of the branch.  At Y we've lost
        the explicit address for "a" so we have a gap.  What should
        we put in the gap?  We might be lucky and push a into it but
        what if we put something else in there?  All this is only a
        problem if, when we merge the states, we only try to push
        entries.  If we could store into the stack we'd be fine.
        We can store registers into the stack but not "direct"
        entries.
        
        For the moment, use the lowest value above the current sp
        which is not currently reserved.  
        DCJM 25/1/01.*)
    then
     let
        fun minReserved s i =
            case pstackEntry table s of
                StackEntry {destStack, ...} => Int.max(destStack+1, i)
            |   _ => i
        val newAddr =
            revfoldIndDownTo(minReserved, realstackVal table, (pstackVal table) indexMinus 1, first)
     in
        alignStack (table, cvec, newAddr)
     end
    else (* We have an explicit offset *)
     (
     if addr < realstackVal table
     then raise InternalError "pushRegisters: unpushed register"
     else ();
     if addr = realstackVal table
     then () (* Got there. *)
     else
        (
        (* If there is another entry for this address push it. *)
        if pushAnyEntryAtCurrentSP(table, cvec) then ()
        else (* Push any register simply to align the stack. *)
         (
         genPush (regN 0, cvec);
         realstackptr table := realstackVal table + 1
         );
        alignStack(table, cvec, addr) (* Keep going. *)
        )
    );


  (* Push all registers.  Now only used before a while-loop.  Also used
     when we have run out of registers.  That should be changed. *)
  fun pushAll (table, cvec) : unit =
    pushRegisters table cvec allRegisters (fn _ => true )(* all entries *) false;

  (* Push a specific entry.  This should really be incorporated into
     pushRegisters since at the moment it processes all the entries
     and only selects the particular one.  *)
  fun pushSpecificEntry (table, cvec, entry) : unit =
    pushRegisters table cvec allRegisters (fn e => e indexEq entry ) false;

  (* Used particularly before procedure calls where we want to ensure
     that anything in a register is pushed onto the stack unless its
     last reference is in the call itself.  Also used before a handler. *)
  fun pushAllBut (table as Ttab{pstack, ...}, cvec, but, pushTheseRegs) : unit =
  let
    val useTab = stretchArray (initStack, 0);
  in
    but
     (fn addr => 
      let
        val (realAddr,_) = pstackRealEntry table addr
        val ind          = getIndex realAddr
      in
        stretchUpdate (useTab, ind,
           stretchSub (useTab, ind) + 1)
      end);
        
    pushRegisters table cvec
      (* registers that are modified *)
      pushTheseRegs
      (* entries with more uses than this *)
      (fn addr =>
         case (pstackEntry table addr) of
           NoStackEntry => 
             raise InternalError "pushAllBut: no entry"
             
         | StackEntry {uses, destStack, ent, cache} =>
             if uses > stretchSub (useTab, getIndex addr)
             then true
             else
                (
                (* Set the destination stack for this entry to "undefined".
                   That's safe because we're going to remove this entry.
                   We do this because we may be about to push some arguments
                   or exception handlers and destStack may be in that area. *)
                if destStack <= 0 then ()
                else stretchUpdate (pstack, getIndex addr,
                        makeStackEntry ent cache uses ~1);
                false
                )
       )
      false
  end;

  (* Ensures that all values which need to be preserved across a function
     call are pushed onto the stack or are in registers that will not
     be modified. *)
  fun pushNonArguments (table as Ttab{pstack, ...}, cvec, args, pushTheseRegs) : reg list =
  let

    fun checkAddress [] _ = false
      | checkAddress (h::t) addr = h indexEq addr orelse checkAddress t addr
    val onList = checkAddress args

    (* Get the list of registers which weren't pushed.  We need to lock
       them so that they don't get pushed onto the stack while we are
       pushing the arguments.  Actually I'm not sure this achieves what
       we want. *)
    fun getRegisterList entry regs =
      if entry indexLt pstackVal table
      then if onList entry (* Is it an argument? *)
      then (* Ignore this. *) getRegisterList (entry indexPlus 1) regs
      else let
        val stackent = pstackEntry table entry
        val nextRegs = 
            case stackent of
               StackEntry {uses, ent = Direct {base, ...}, ...} =>
                   if uses = 0 then regs
                   else (lockRegister(table, base); base::regs)
            |  StackEntry {uses, ent = Register reg, ...} =>
                   if uses = 0 then regs
                   else (lockRegister(table, reg); reg::regs)
            | _ => (* neither Direct nor Register *) regs
      in
        getRegisterList (entry indexPlus 1) nextRegs
      end
      else regs

  in
    pushRegisters table cvec
      (* registers that are modified *)
      pushTheseRegs
      (* Ignore entries corresponding to the arguments but only if they
         have a use count of exactly one, *)
      (fn addr =>
         case (pstackEntry table addr) of
           NoStackEntry => 
             raise InternalError "pushNonArguments: no entry"
             
         | StackEntry {uses, destStack, ent, cache} =>
             if uses > 1 orelse not (onList addr)
             then true (* Must push it now if the register is modified. *)
             else (* Don't need to save it because it's an argument. *)
                (
                (* Set the destination stack for this entry to "undefined".
                   That's safe because we're going to remove this entry.
                   We do this because we may be about to push some arguments
                   and destStack may be in that area.
                   There may not be the same need for this as in pushAllBut
                   but it shouldn't do any harm. *)
                if destStack <= 0 then ()
                else stretchUpdate (pstack, getIndex addr,
                        makeStackEntry ent cache uses ~1);
                false
                )
       )
      (* If all the registers must be pushed there's no point in trying to
         move to another register. *)
      (not(isAllRegs pushTheseRegs));
      getRegisterList first []
  end;


(* bugfixed makeEntry added 30/3/95 *)
  fun makeEntry (table as Ttab{pstackTrace, printStream, ...}, _ : code, index, locn, use, isP) : unit =
  ( 
    stretchUpdate (isProc table, locn, isP);
    stretchUpdate (decToPstack table, locn, index);
    
   (* There is a bug which can happen if we make a reference to a copy entry
      which refers to a register.  If the value is used in a function call
      but also used after it, it is possible that pushAllBut will fail
      to save it because the use-count has been incremented on the copy
      entry but NOT on the register entry. This happens very rarely, so
      to avoid it, we explicitly push the register in that case. 
      
      Extra comment (25/9/95):
      This appears also to happen if we have a direct entry, 
      so we also save it in that case as well.
      
      What we really need is a complete rewrite of this module - 
      it uses the wrong data-structures. SPF 25/9/95.
      
      DCJM 26/11/99. Copy entries have been removed, so this is
      no longer required.
    *)
    (* Add the number of uses (less one since the use-count will
       normally be one) to the use count. *)
    incrUseCount (table, index, use - 1);
    if pstackTrace
    then
       (
        printStream "makeEntry: index=";
        printStream(Int.toString(getIndex index));
        printStream " locn=";
        printStream(Int.toString locn);
        printStream " use=";
        printStream(Int.toString use);
        printStream "\n"
        )
    else ()
  );

  type savedState =
    {
      pStackPtr: stackIndex,
      realStackPtr: int,
      pStack: stackEntry stretchArray,
      nextRegNo : int,
      context: string
    };
  
  fun pStackPtr    ({pStackPtr   ,...}:savedState) = pStackPtr;
  fun realStackPtr ({realStackPtr,...}:savedState) = realStackPtr;
  fun pStack       ({pStack      ,...}:savedState) = pStack;
 
  (* compare with the similar functions for ttabs!!! *)
  fun pStackEntry (table : savedState) (locn:stackIndex) : stackEntry = 
    stretchSub (pStack table, getIndex locn);
  
  fun printState printStream (save: savedState as {pStackPtr, realStackPtr, context, ... }) name =
  ( printStream name;
    printStream "\n";
    printStream "context="; printStream context;
    printStream " psp=";
    printStream(Int.toString(getIndex pStackPtr));
    printStream " rsp=";
    printStream(Int.toString realStackPtr);
    printStream "\n";
    forIndDownTo(pStackPtr, first, fn i => printEntry printStream (pStackEntry save i) i)
  );

  (* Returns the real entry from a chain of "copy" entries. 
     Now that "copy" entries have been removed this just returns
     the entry. *)
  fun pStackRealEntry (table:savedState) (locn:stackIndex) =
  let
    val pstack = pStack table
  in
      case (stretchSub (pstack, getIndex locn)) of
        NoStackEntry =>
          raise InternalError "pStackRealEntry: no entry"
          
      | StackEntry record => (locn,record)
  end;


  (* Sets the pseudo stack into a state to which it can be restored later.
     It is used when there are conditional branches to ensure that the state
     is the same if the branch falls through or is taken. *)
  fun saveState (table as Ttab{pstackTrace, printStream, ...}, cvec) : savedState =
  let
    val maxIndex  = getIndex (pstackVal table);
    val saveStack = stretchArray (maxIndex, NoStackEntry);
     
    fun copyState s =   (* copy the state. *)
    ( if s indexLt pstackVal table
      then
      ( stretchUpdate (saveStack, getIndex s, pstackEntry table s);
        copyState (s indexPlus 1)
      )
      else ()
    );
    
    val state =
        { 
          pStackPtr    = pstackVal table, 
          realStackPtr = realstackVal table,
          pStack       = saveStack,
          nextRegNo    = nextRegNo (regset table),
          context      = traceContext cvec
        }
  in
    copyState first;
    if pstackTrace then printState printStream state "saveState" else ();
    state
  end;

  (* Tries first of all, then pushes all the registers and tries again. *)
  fun getAnyRegister (table as Ttab{pstackTrace, ...}, cvec) : reg =
  (let 
    (* First see if there is one free and grab that. *)
    val r = getReg (regset table);
    val r =
      if r regNeq regNone then r
      else let
    (* We seem to have run out.  First flush the cache, then if that
       fails push all the registers. On the whole it seems to work better
       to free a lot of registers rather then just freeing a few. We tend
       to either have more than enough registers, or much too few. *) 
    fun untilSomethingFree () : bool =
    let
      val rs = regset table;
      val r = getReg rs;
      val () = 
        if r regNeq regNone
        then free rs r
        else ()
    in
      r regEq regNone
    end;
  
    val () = removeFromCache table allRegisters untilSomethingFree;
    val r = getReg (regset table);
      in
      if r regNeq regNone then r
      else let
        val () = pushAll (table, cvec);
        
        (* Pushed values stay in the cache. *)
        val () = 
          removeFromCache table allRegisters untilSomethingFree;
        val r = getReg (regset table);
      in (* If we still haven't found anything we are in big trouble. *)
        if r regNeq regNone then r
         else raise InternalError "No free registers"
      end
      end
  in 
    if pstackTrace
    then printStack table "getAnyRegister" (traceContext cvec )
    else ();
    r
  end) handle SML90.Interrupt => raise SML90.Interrupt
         | exn => (printStack table "getAnyRegister" (traceContext cvec); raise exn);
         (* getAnyRegister *)


  (* Resets the stack to the value given by removing any entries with
     non-zero use counts above it. This is fairly rare so does not have
     to be particularly efficient. Assumes that there are enough data
     registers to hold all the values. *)
  (* We use the stack for saving values, for function parameters and for
     handler entries.  Function parameters and handler entries have specific
     formats with multiple words which must be contiguous.  If we have to
     spill a register after, say, pushing one parameter and while computing
     another, we must reload any spilled values and set the real stack pointer
     correctly before continuing. *)
  fun resetButReload (cvec, table as Ttab{pstack, pstackTrace, ...}, stackOffset) : unit =
  let
    val oldSp = realstackVal table;
    
    (* Load any values above "stackOffset". *)
    fun loadEntries entry =
      if entry indexLt first
      then ()
      else let
        val stackent = pstackEntry table entry;
      in
        case stackent of
          StackEntry {ent = Stack addr, cache, uses, ...} =>
            if addr < ((stackOffset - 1) * ~wordSize) (* Above the limit on the stack. *)
            then let
               (* Load it without changing the use count. *)
              val reg = 
                if cache regNeq regNone
                then cache
                else let
                  val reg      = getAnyRegister (table, cvec);
                  val stackPtr = (realstackVal table - 1) * ~wordSize;
                in
                  genLoad (addr - stackPtr, regStackPtr, reg, cvec);
                  reg
                end;
            in (* Clear out the cache and overwrite this entry with a 
                  reference to the register. *)
              stretchUpdate (pstack, getIndex entry,
                 makeStackEntry (Register reg) regNone uses ~1)
            end
            else ()
            
          | _ => ();
      
        loadEntries (entry indexMinus 1)
      end (* loadEntries *);
  in
    loadEntries ((pstackVal table) indexMinus 1);
    
     (* If the real stack ptr has changed we must have pushed something,
        so our check has been useless. *)
     if  realstackVal table <> oldSp
     then raise InternalError "resetButReload: must have pushed something more"
     else ();

    (* Now reset the stack pointer. *)
    resetStack (realstackVal table - stackOffset, cvec);
    realstackptr table := stackOffset;
    
    if pstackTrace then printStack table "resetButReload" (traceContext cvec) else ()
  end;

  fun getRegister (table as Ttab{pstackTrace, ...}, cvec, reg) : unit =
  let (* Get it out of wherever it is being used. *)
    fun inUse ()  = usage (regset table) reg > 0
    val thisReg = singleton reg
  in
    pushRegisters table cvec thisReg (fn _ => true) (* any entry *) true;
    (* Clear it out of the cache (if it is there) *)
    removeFromCache table thisReg inUse;
    (* Should now be free. *)
    getRset (regset table) reg;
    
    if pstackTrace then printStack table "getRegister" (traceContext cvec) else ()
  end;

  fun freeRegister (table, reg) : unit = free (regset table) reg;

  fun chooseRegister (table : ttab) : reg =
  let
    fun chooseReg ({vec,nextr, ...}:rset) =
    let
      val nextReg = ! nextr;
      
      fun next n = if n = 0 then (vecSize - 1) else (n - 1);
      
      (* SPF 7/6/94 fixed off-by-one problem *)
      fun findFree (i : int) : reg =
    if sub (vec, i) = 0
    then
      (
        (* Next time start looking at the register before this. *)
        nextr := next i; (* Do we really want to do this? *)
        regN i
      )
    else let
      val n = next i;
    in
      if n = nextReg then (* None free. *) regNone
      else findFree n
    end;
    in
      findFree nextReg
    end;
  in
    chooseReg (regset table)
  end

  (* Return the set of modified registers for this function. *)
  fun getModifedRegSet (Ttab{regset={modSet=ref modSet, ...}, ...}) : reg list =
  let
      fun getRegs(i, rset) =
         if i < 0 then rset
         else if inSet(regN i, modSet) then getRegs(i-1, regN i :: rset)
         else getRegs(i-1, rset)
  in
      getRegs(regs-1, [])
  end

  (* Add a set of registers to those modified by this function.
     This will be the set of registers modified by a function
     called by this one. *)
  fun addModifiedRegSet (transtable: ttab, regs: regSet): unit =
  let
      val Ttab{regset={modSet, ...}, ...} = transtable
  in
      modSet := regSetUnion(!modSet, regs)
  end

  (* Generates code for an entry on the pseudo-stack. *)
  (* Moves the entry (at locn) into destReg, decrementing the
     use-count for entry. Doesn't push anything new on the pstack. *)
  fun loadPstackEntry (table as Ttab{pstackTrace, ...}) locn (* Offset on the stack *) destReg cvec =
    let
      val (_, {cache = cacheReg, ent, ...}) = pstackRealEntry table locn
    in
      if cacheReg regNeq regNone
      then
        (
         if cacheReg regNeq destReg
         then moveRR cacheReg destReg cvec
         else ()
        )
      else
        case ent of
          Register reg =>
            if reg regNeq destReg
            then moveRR reg destReg cvec
            else ()
          
        | Literal lit =>
            genRI (instrMove, regNone, lit, destReg, cvec)

        | CodeRef code =>
              genLoadCoderef (code, destReg, cvec)

        | Direct {base, offset} =>
            genLoad (offset, base, destReg, cvec)

        | Stack index =>
          let
            val stackPtr = (realstackVal table - 1) * ~wordSize;
          in
            genLoad (index - stackPtr, regStackPtr, destReg, cvec)
          end
    
        | Container l =>
          (* The first entry in the container gives us the address. *)
            case pstackRealEntry table (hd l) of
                (_, {ent = Stack index, ...}) =>
                  let
                    val stackPtr = (realstackVal table - 1) * ~wordSize;
                  in
                    genStackOffset (destReg, index - stackPtr, cvec)
                  end
            |   _ => raise InternalError "loadPstackEntry: container entry is not on stack";
            

       (* Decrement use count and remove if done. *)
       incrUseCount (table, locn, ~1);

       if pstackTrace then printStack table "loadPstackEntry" (traceContext cvec) else ()
    end (* loadPstackEntry *) 

  (* Pushes a new pstack entry; loads value into register;
     decrements the use count of old pstack entry.
  *)
  and loadEntry (cvec, table as Ttab{pstack, ...}, entry, willTrample) : reg*stackIndex =
  let
    val (realLoc, {ent = stackEntry, cache = cacheReg, uses, destStack}) =
      pstackRealEntry table entry;
      
    (* If we find an entry in the cache or already in a register we can use
       it provided it will not be modified or this is its last use. Otherwise
       we must make a copy of it. *)
    val lastRef = lastReference table entry;
    
   fun useCacheRegister () : reg*stackIndex =
     let
       (* The value is being cached and we can safely use the register. *)
       (* Get the register, increment its use count and put it on the stack *)
       (* If we are going to trample on the register we must remove it
          from the cache. If this is the last real reference that will
          not matter, but if this is actually a reference to a parameter
          which could be loaded onto the stack again we have to be careful
          that the cache does not indicate a register which has been changed. *)
       val () = 
           if willTrample
           then stretchUpdate (pstack, getIndex realLoc,
                      makeStackEntry stackEntry regNone uses destStack)
           else addRegUse (table, cacheReg);
     
       val newEntry = pushReg (table, cacheReg)
     in
     
       (* Must decrement the use-count of the entry we are loading as though
          we had actually loaded it. *)
       incrUseCount (table, entry, ~1);
       (cacheReg, newEntry)
     end
    
    fun useNewRegister () : reg*stackIndex =
    let
      (* It is loaded into a register. This is complicated because we want
         to put entries into the cache if we can. They must not be put into
         the cache until after they have been loaded otherwise the load
         instruction will simply copy the new cache value. It is possible
         that a value might be cached in a data register when it is needed
         in an address register or vice-versa. *)
      val resultReg = getAnyRegister (table, cvec);
    in 
      (* Get the entry again - getAnyRegister could have forced the
         entry onto the stack if it had run out of registers. *)
      case (pstackEntry table realLoc) of
        NoStackEntry => raise InternalError "loadEntry: entry deleted"

      | StackEntry {ent, uses, cache, destStack} =>
         let
           (* If the value is already cached, keep it in the old
              cache register, rather than the new one. This should
              help to minimise register-register moves when we have
              to merge branches. SPF 5/6/95 *)
           val cacheIt =
             not willTrample andalso cache regEq regNone andalso
           case ent of
              Direct   _    => true
         (* | Literal   _   => true  *) (* constants are not cached *)
           (* Cannot cache it if we are about to pop it. *)
           | Stack index   => (0 <= index orelse not lastRef)
           | _             => false
            (* If we are going to cache it we musn't let it be removed. *)
            val () = 
                if cacheIt
                then incrUseCount (table, entry, 1) 
                else ();
            
            val () = loadPstackEntry table entry resultReg cvec;
            val newEntry = pushReg (table, resultReg);
      
         in
            if cacheIt
            then
              (
                (* First remove any register already in the cache.
                   This should now be a no-op. SPF 5/6/95 *)
                if cache regNeq regNone
                then freeRegister (table, cache)
                else ();
                
                (* put in the cache and restore use-count. *)
                stretchUpdate (pstack, getIndex realLoc,
                        makeStackEntry ent resultReg uses destStack);
                        
                addRegUse (table, resultReg);
                
                incrUseCount (table, entry, ~1)
              ) 
            else ();
            
            (resultReg, newEntry)
         end 
    end; (* useNewRegister *)
    
  in
    case stackEntry of
      Register reg =>
        if not willTrample
        then (reg, entry)

        else if lastRef
        then
            (
            (* We are going to trample on it but this is the last reference
               so we can use it. It may, though, be caching a value so
               we must remove it from the cache before we return it.
               DCJM 17/1/01. *)
            removeRegistersFromCache(table, listToSet [reg]);
            (reg, entry)
            )
        
        else if cacheReg regNeq regNone (* Should not happen now. *)
          then useCacheRegister ()

        else useNewRegister () (* Must copy it. *)
        
    | _ =>
        if cacheReg regNeq regNone
          then useCacheRegister ()
        else useNewRegister ()  (* Not in a register. *)
  end;

  (* Load a value into a specific register.
     Used for loading argument regs etc.
     Pushes a new entry onto the pstack (why?). *)
  fun loadToSpecificReg (cvec, table, reg, entry, needExclusive) : stackIndex =
  let
    val (_, {ent = stackEntry, cache = cacheReg, ...})
        = pstackRealEntry table entry;
        
    val lastRef  = lastReference table entry;
  in
    if (case stackEntry of Register r => r regEq reg | _ => false)
    then (* It's already in the right register. *)
      if needExclusive andalso not lastRef
      (* The value is in the register but we are going to 
         change it - have to push the previous contents. *)
      then
        ( 
          getRegister (table, cvec, reg);
          let
            val newEntry : stackIndex = pushReg (table, reg);
          in
            incrUseCount (table, entry, ~1);
            newEntry
          end
        )
      
      (* Already there - use that entry. *)
      else entry

    else if cacheReg regEq reg
    then (* The register we want is caching the value - use it. *)
      ( 
        (* Get the register, increment its use count and put it on the stack *)
        if needExclusive andalso not lastRef
        then getRegister (table, cvec, reg)
        else addRegUse (table, reg);
      
        let
          val newEntry : stackIndex = pushReg (table, reg);
        in
           (* Must decrement the use-count of the entry we are loading
              as though we had actually loaded it. *)
          incrUseCount (table, entry, ~1);
          newEntry
        end
      )

    else 
      ( (* Not in the register or must copy it. *)
      (* This entry may contain a reference to a storage location with
         the register we want as the index register. If this is the only
         reference to it then we can increment the use count and use the
         register. This saves us from copying the value into a second
         register in order to free this one, and then copying it back.
         i.e. can generate lw rn,0(rn) *)
         
        (* Remove it from the cache first - we have to check that its use
           count is 1 (i.e. it is not being used as base register for
           something else we want), but we don't care if it is only being
           cached. It's not clear that we want to do this for non-direct
           entries, but I would rather not change this code yet. 
           SPF 25/5/95 *)
        removeFromCache
            table
            (singleton reg)
            (fn () => usage (regset table) reg > 0);
            
        (* We have now removed all "cache" uses of the register, but
           there may still be several "direct" uses of it. For the
           optimisation to work, there should only be one reference
           left - to this entry, which must be a "direct". *)
        if lastRef andalso 
              usage (regset table) reg = 1 andalso
              (case stackEntry of
                Direct {base,...} => base regEq reg
               | _ => false)
        then addRegUse (table, reg)
        else getRegister (table, cvec, reg); (* sets usage to 1 *)
           
        loadPstackEntry table entry reg cvec;
        pushReg (table, reg)
      )
  end; (* loadToSpecificReg *)


  (* Checks if we are going to overwrite the stack, and loads the entry
     into a register. *)
  fun loadEntryBeforeOverwriting (cvec:code) (table as Ttab{pstack, ...}) (offset:int) =
    if 0 <= offset andalso offset < realstackVal table
    then let (* May have to reload something. *)
      fun findTheEntry (entry: stackIndex) =
        if entry indexLt first then () (* finish *)
        else let
          val stackent = pstackEntry table entry;
        in
          case stackent of
            StackEntry {ent = Stack addr, cache, uses, ...} =>
              if addr = offset * ~wordSize
              then let (* This is the entry. *)
                (* Load it without changing the use count. *)
                val reg =
                  if cache regNeq regNone
                  then cache
                  else let
                    val reg = getAnyRegister (table, cvec);
                    val off = (realstackVal table - 1) * wordSize;
                    val () = 
                       genLoad  (addr + off, regStackPtr, reg, cvec);
                  in
                    reg
                  end;
                  
                  val newStackent =
                    (* Make a new entry with a NEW stack destination.
                       If we have to push it we have to use a new location.
                       I don't like this but it's safe because this only occurs
                       for a tail-recursive value or for a temporary value
                       in an exception handler. *)
                    makeStackEntry (Register reg) regNone uses ~1;
               in
                 (* Clear out the cache and overwrite this entry with a
                    reference to the register. *)
                 stretchUpdate (pstack, getIndex entry, newStackent)
               end
               else () (* not this entry *)
               
            | _ => ();
        
          findTheEntry (entry indexMinus 1)
        end (*  findTheEntry *) 
      in
        findTheEntry ((pstackVal table) indexMinus 1)
      end
      else (); (* end of loadEntryBeforeOverwriting *)
      
  (* Store a pseudo-stack entry at a given location on the real stack.  Used
    when making a tail-recursive call.  The problem is that the old entry
    in the real stack may be in use, so we may have to reload it first.
    We load all the values before storing any, so there is no danger of
    overwriting entries in the argument area, but we may have had to push
    some of the registers while doing the load, so those entries will have
    to be saved. *)
  fun storeInStack (cvec, table, entry, locn) : unit =
  let
    (* Move it to the stack, using a move-immediate if possible. *)
    fun inc x = (x := !x + 1);

    fun generalStoreInStack () = (* General case. *)
    let
      val (reg, regEntry) = loadEntry (cvec, table, entry, false)
    in
      (* Lock the register, otherwise it might be used to load an entry. *)
      lockRegister (table, reg);
      
      loadEntryBeforeOverwriting cvec table locn;
      
      (* N.B. loadEntry may push values onto the stack,
         so we cannot use isPush. *)
      if (realstackVal table) = locn
        then
            (
            genPush (reg, cvec);
            inc (realstackptr table)
            )
        else let
          val loc = ((realstackVal table) - locn - 1) * wordSize
        in
          genStore (reg, loc, regStackPtr, STORE_WORD, regNone, cvec)
        end;

      unlockRegister (table, reg);
      removeStackEntry(table, regEntry)
    end;

    val isPush = (realstackVal table = locn);

    val (_,{ent = valEnt, cache = cacheReg, ...}) = 
       pstackRealEntry table entry;
  in   
    (* Select the best instruction to use. The default is to load it
      into a register and store or push that. *)
    case valEnt of
      Literal lit =>
        if isPush andalso instrIsRI (instrPush, lit)
        then
         ( (* Push-immediate. *)
          loadEntryBeforeOverwriting cvec table locn;
          genRI (instrPush, regNone, lit, regNone, cvec);
          incrUseCount (table, entry, ~1);
          inc (realstackptr table)
         )
       
        else if isStoreI(lit, STORE_WORD, false)
        then let (* Store immediate. *)
          val () = loadEntryBeforeOverwriting cvec table locn;
             
          val locn = ((realstackVal table) - locn - 1) * wordSize
          val () = genStoreI (lit, locn, regStackPtr, STORE_WORD, regNone, cvec);
        in
          (* Remove the entry for the literal. *)
          incrUseCount (table, entry, ~1)
        end

        else generalStoreInStack () 
  
    | Direct {base, offset} =>
        if preferLoadPush andalso isPush andalso cacheReg regEq regNone
        then let (* Push memory. *)
          val () = loadEntryBeforeOverwriting cvec table locn;
          val () = genLoadPush (offset, base, cvec);
        in
          incrUseCount (table, entry, ~1);
          inc (realstackptr table)
        end
          
        else generalStoreInStack ()


    | Stack index =>
        if preferLoadPush andalso isPush andalso cacheReg regEq regNone
        then let (* Push stack entry. *)
          val () = loadEntryBeforeOverwriting cvec table locn;
          val locn     = index + (realstackVal table - 1) * wordSize;
          val () = genLoadPush (locn, regStackPtr, cvec);
        in
          incrUseCount (table, entry, ~1);
          inc (realstackptr table)
        end
          
        else generalStoreInStack ()

    | _ => generalStoreInStack ()
    ;
        
    if realstackVal table > maxstackVal table
    then maxstack table := realstackVal table
    else ()
  end (* storeInStack *);

  (* Ensures that the top of the pseudo stack has been copied onto the
     real stack and is at the correct position. stackOffset contains the
     stack offset it should have. Primarily used to push arguments to
     procedures. *)
  fun pushValueToStack (cvec, table, entry, stackOffset) : stackIndex =
  let 
    val () = storeInStack (cvec, table, entry, stackOffset - 1)
  
    val () = 
      (* Remove any entries above the stack offset we need. *)
      if realstackVal table > stackOffset
      then resetButReload (cvec, table, stackOffset)
      else ();
  
    val stackAddr = (stackOffset - 1) * ~wordSize;
    val result : stackIndex =
      pushPstack table (Stack stackAddr) "pushValueToStack";
  in
    (* The stack pointer should now be the required value. *)
    if realstackVal table <> stackOffset
    then raise InternalError "pushValueToStack: Couldn't push to stack"
    else ();
    result
  end;

  fun reserveStackSpace(table: ttab, cvec: code, space: int): stackIndex =
  (* Reserve space on the stack for a tuple. *)
  let
    (* We must first make sure that the space we're going to allocate
       hasn't been reserved for a register. *)
    val _ = alignStack(table, cvec, ~1)
    (* Initialise the store so that the garbage collector doesn't
       accidentally pick up an invalid pointer. *)
    (* The stack grows downwards so we want the entries in reverse order.
       The first entry must be lowest address. *)
    fun pushEntries 0 = []
     |  pushEntries n =
        let
            val pushRest = pushEntries (n-1)
            (* We could push a constant but on many architectures it's easier to
               push a register.  It doesn't matter which provided it contains a
               valid tagged value or pointer. *)
            val _ = genPush (regN 0, cvec);
            val stackLocn = incsp table
        in
            stackLocn :: pushRest
        end;
    val entries = pushEntries space
  in
    pushPstack table (Container entries) "reserveStackSpace"
  end
    
  (* Generates an indirection on an item on the pseudo-stack. *)
  fun indirect (offSet, entry, cvec, table) : stackIndex =
      case pstackRealEntry table entry of
         (_, {ent = Container l, ...}) =>
            (* If we are indirecting off a container we can simply load the entry. *)
            let
                val resIndex = List.nth(l, offSet div wordSize)
            in
                (* Increment its use count. *)
                incrUseCount (table, resIndex, 1);
                removeStackEntry(table, entry); (* Remove the container entry. *)
                resIndex
            end

      | (_, {ent = Literal i, ...}) =>
          (* We won't normally get this because it will have been optimised out.
             The exception is when we have SetContainer with a tuple which is a constant. 
             For safety we check that we have a valid address here although
             unlike in findEntryInBlock we should never actually get an invalid one. *)
          (* Actually, we can, in cases such as val (a,b) = raise ...  where we will
             do an indirection on the dummyValue put on the pstack to represent the
             non-existent result of the "raise".  In that case we put in a dummy result
             of zero. *)
            if isShort i andalso toShort i = 0w0
            then pushConst(table, toMachineWord 0)
            else if isShort i orelse ADDRESS.length (toAddress i) <= Word.fromInt(offSet div wordSize)
            then raise InternalError "indirect - invalid constant address"
            else
               (
               removeStackEntry(table, entry); (* Remove the container entry. *)
               pushConst (table, loadWord (toAddress i, toShort (offSet div wordSize)))
               )

      |  _ =>
      let
        val (topReg, topEntry) = loadEntry (cvec, table, entry, false);
        val () = removeStackEntry(table, topEntry); (* Remove the entry for the register. *)
        (* and push the indirection *)
        (* Profiling shows that this search is where the compiler can spend most
           of its time. To speed it up we keep a lower limit pointer which saves
           us searching below the lowest direct entry. *)
        (* See if it is already on the stack. *)
        fun search s max foundD =
          if s indexGeq max
          then
          ( 
            (* Not there. *)
            addRegUse (table, topReg);
            (* If this is below the previous lower limit we need to reset it. *)
            if !(lowestDirect table) indexGt (pstackVal table)
            then lowestDirect table := pstackVal table
            else ();
            
            pushPstack table (Direct {base = topReg, offset = offSet}) "indirect"
          )
          
          else 
            case (pstackEntry table s) of
              StackEntry {ent = Direct {base, offset}, ...} =>
                (
                  (* If we found no direct entries below here 
                     then remember this as the first. *)
                  if not foundD then lowestDirect table := s else ();
                  
                  if base regEq topReg andalso offset = offSet
                  then (* Found it *)
                    ( 
                      incrUseCount (table, s, 1);
                      s
                    )
                  else search (s indexPlus 1) max true (* Found one *)
                )
         
            | _ =>
                search (s indexPlus 1) max foundD; (* end search *)
      in
        search (! (lowestDirect table)) (pstackVal table) false
      end;

  (* Copies an item which is in another procedure, and therefore on a
     different table, onto the local stack. If the value is relative
     to the frame pointer we must generate a static-link entry and make
     the address relative to that. The only values should be either
     literals (i.e. the address of a static-link called procedure)
     or a stack value. *)
  fun pushNonLocal (fromTable, toTable, locn, makeSl, cvec) : stackIndex =
  let
    val (_,{ent = stacken, uses = use, ...}) = 
      pstackRealEntry fromTable locn;
  in
    if use <= 0 
    then raise InternalError "pushNonLocal: zero use count"
    else ();
    
    (* Don't decrement the use-count because there will only be one
       count for all the references from this closure. *)
    case stacken of
      Literal lit =>
        pushConst (toTable, lit)
        
    | CodeRef code =>
        pushCodeRef (toTable, code)
        
    | Stack index =>
      let
        (* Load static link entry (this points to the first
           word in the frame, not to the base of the frame). *)
        val sl = makeSl ();                          
      in (* Indirect to get the particular entry in the frame. *)
        indirect (index, sl, cvec, toTable)
      end    
       
    | _ => raise InternalError "pushNonLocal: not Literal, CodeRef or Stack"
  end;

  fun isProcB (table, locn) : bool = 
    stretchSub (isProc table, locn);

  (* Moves an expression into a newly created vector or into a container. *)
  fun moveToVec (vecEntry, valueEntry, addr, width, cvec, table) : unit =
  (* Vector index; Value to put in; Vector Offset, code; Translation table *)
    (* We have to load the vector address if we have just used the
       last free register. We could flush the registers and reuse topReg
       for valReg. Increment the use count on the register just to be sure. *)
    let
      val () = incrUseCount (table, vecEntry, 1);
      val (topReg, topEntry)   = loadEntry (cvec, table, vecEntry, false);
      
     (* We have to be careful if we have just used the last free register to
        load the vector address. We could flush the registers and reuse topReg
        for valReg. Increment the use count on the register just to be sure. *)
      val ()  = lockRegister (table, topReg);
      
      fun storeViaRegister () =
      let
        val (valReg, regEntry) = loadEntry (cvec, table, valueEntry, false);
      in
        genStore (valReg, addr, topReg, width, regNone, cvec);
        removeStackEntry(table, regEntry)
      end;
      
     in
        case (pstackRealEntry table valueEntry) of 
          (* Can we simply move it? *)
          (_,{ent = Literal lit,...}) =>
            if isStoreI(lit, width, false)
            then
            (
                genStoreI (lit, addr, topReg, width, regNone, cvec);
                (* Remove the entry for the literal. *)
                incrUseCount (table, valueEntry, ~1)
            )
            else storeViaRegister ()
            
        | _ => storeViaRegister ()
        ;
      
       (* Release it. *)
       unlockRegister (table,  topReg);
       removeStackEntry(table, topEntry)
     end;
     
  (* Loads a value into a register if it is in the argument area.  Used
     for tail-recursive calls. "storeInStack" checks for overwriting
     entries elsewhere on the stack, but because the argument area is not
     represented by entries on the pstack it won't work for them. *)
   fun loadIfArg (cvec, table, entry) : stackIndex =
     case (pstackRealEntry table entry) of
       (_,{ent = Stack index, ...}) =>
         if index > 0
         then let
           val (_, newEntry) = loadEntry (cvec, table, entry, false)
         in
           newEntry
         end
         else entry
         
     | _ => entry  (* return the original. *)

  fun getRegisterSet (addr: machineWord): regSet =
    (* The set of register modified by a function. *)
  let
     val doCall: int*machineWord -> Word.word
        = RunCall.run_call2 RuntimeCalls.POLY_SYS_process_env

     val rSet = doCall(103, addr) (* Get the bit pattern from the function. *)
     (* It would be much better to use Word.word here rather than
        this Address type.  The only reason we still use it is just
        in case we ever need to bootstrap from an old database. *)
     val andb = Word.andb and << = Word.<<
     infix andb <<

     fun decodeBits (i: int) (l: reg list) =
        if i < 0 then l
        else if (rSet andb (0w1 << Word.fromInt i)) <> 0w0
        then decodeBits (i-1) (regN i :: l)
        else decodeBits (i-1) l

     val registers = decodeBits (regs-1) []
  in
     listToSet registers
  end

  fun getRegisterSetForCode (cvec: code) : regSet =
  (* Get the register set for a forward reference which may or may not
     have already been compiled. *)
    case codeAddress cvec of
        SOME addr => (* Now compiled - return the register set. *)
            getRegisterSet (toMachineWord addr)
    |   NONE =>
         (* We haven't compiled this yet: assume worst case. *) allRegisters

  (* Get the register set for an entry on the stack which will be the entry
     point of a function.  If it's not a constant we have to assume it
     modifies any of the registers. *)
  fun getFunctionRegSet(index: stackIndex, transtab: ttab) : regSet =
  let
    val (_,{ent = stacken, ...}) = pstackRealEntry transtab index;
  in    
    case stacken of
      Literal lit => getRegisterSet lit
    | CodeRef code => getRegisterSetForCode code
    | _ => allRegisters
  end;


  (* An optional result. i.e. if the code before the jump has returned a result
     this is the offset in the table of the result. *)
  datatype mergeResult = NoMerge | MergeIndex of stackIndex;

  (* A code label packaged up with a saved state. *)
  abstype labels =
     NoLabels
   | Labels of {result: mergeResult, lab: CODECONS.labels, state: savedState}
  with
    val noJump = NoLabels;
    fun isEmptyLabel NoLabels = true | isEmptyLabel _ = false;
    fun makeLabels res cLab sState = Labels {result=res, lab = cLab, state = sState};
  
    fun labs  (Labels {lab  ,...}) = lab   | labs  _ = raise Match;
    fun state (Labels {state,...}) = state | state _ = raise Match;
    fun result (Labels {result,...}) = result | result _ = raise Match;
  end;


  (* Set the state to the saved values. *) 
  fun setState (save : savedState, table as Ttab{pstack, pstackTrace, printStream, ...}, cvec, carry, mark, isMerge): mergeResult =
  let 
    val () = 
      if pstackTrace then printState printStream save "setState" else ();
    
    (* This is logically unnecessary, but increases the likelihood
       that values moved out of resultReg (for example) are moved
       into the *same* register in the different branches. That in
       turn decreases the work we have to do when we merge the
       branches back again. SPF 5/6/95 *)
    val () =
      setNextRegNo (regset table, #nextRegNo save);
    
    val topReg =
        case carry of
            NoMerge => regNone (* Unused *)
          | MergeIndex savedTop =>
            (
                case (pStackRealEntry save savedTop) of
                  (_,{ent = Register reg, ...}) =>
                    reg
                    
                | (_,{cache = cacheReg, ...}) =>
                   if cacheReg regNeq regNone
                   then cacheReg
                   else raise InternalError "setState: not a register"
            )
  in
    (* Clobber all entries above the "mark".
        This will remove the result register if there is one. *)
    (* TODO: I don't like this.  I think we should explicitly remove it.
       DCJM 30/11/99. *)
    forIndDownTo(pstackVal table indexMinus 1, mark, fn s => removeEntry table s true);
      
    let
      (* Set up the saved state. Need to set the register set.
         Free the registers from the table. *)
      fun frees s =
        if s indexGeq pstackVal table
        then ()
        else let
          val stacken = pstackEntry table s;
        in
          case stacken of
            NoStackEntry => ()
          
         |  StackEntry {ent, cache, ...} =>
              (
                case ent of 
                  Register reg =>
                    freeRegister (table, reg)
                    
                | Direct {base, ...} =>
                    freeRegister (table, base)
                  
                | _ => ()
                ;
                
                if cache regNeq regNone
                then freeRegister (table, cache)
                else ()
              )
          ;
          
          frees (s indexPlus 1)
        end;
    in
      frees first 
    end;

    realstackptr table := realStackPtr save;
    
    let
      val oldPstackptr = pstackVal table;
      val () = pstackptr table := pStackPtr save;

      (* Go up the entries putting them onto the table from the saved
         state, then come back setting the use-counts where appropriate.
         We have to do it this way because of copy entries. *)
      (* But we don't have copy entries any longer so this could
         be improved. DCJM 30/11/99. *)
      fun putOnEntries s =
        if s indexGeq pStackPtr save
        then ()
        else let
          val saveEntry = pStackEntry save s;
          val (tabUseCount, tabDestStack) =
            (* Get the use-count and stack destination in the table. *)
            if s indexGeq oldPstackptr then (0, ~1)
            else 
              case (pstackEntry table s) of
                NoStackEntry           => (0, ~1)
              | StackEntry {uses, destStack, ...} =>
                    (uses, if exitedVal table then ~1 else destStack)
              ;
        in
          (* Put the saved entry into the table. *)
          stretchUpdate (pstack, getIndex s, saveEntry);
          
          case saveEntry of
            NoStackEntry =>
              putOnEntries (s indexPlus 1)
          
          | StackEntry {ent, cache, destStack, uses, ...} =>
              (
                (* Compute the new register set. *)
                case ent of
                  Register reg => 
                    incr (regset table) reg
                    
                | Direct {base, ...} =>
                    incr (regset table) base
                    
                | _ => ()
                ;
                
                if cache regNeq regNone
                then incr (regset table) cache 
                else ();

              (* Propagate the destination stack offset information.  If the
                 saved offset is non-negative it must agree with the current
                 information.  *)
              if tabDestStack < 0 then ()
              else if destStack >= 0
              then (if destStack <> tabDestStack
                    then raise InternalError "Different stack destinations" else ()
                   )
              else stretchUpdate (pstack, getIndex s,
                    makeStackEntry ent cache uses tabDestStack);
              
              putOnEntries (s indexPlus 1);
              
              (* Can now set the use counts. The use-counts may have changed
                 and entries may have been removed because the use-counts of
                 copy entries have been decremented. *)
              (* This no longer applies now that copy entries have been
                 removed.  Continue to do it that way for the moment.
                 Note that with the change from use-counts to last-references
                 we no longer reduce the use count to the lower of the
                 saved and current values in the case where we are setting
                 the state at the start of a parallel flow of control (e.g.
                 at the start of the else-part of an if-then-else) but only
                 when this is being used to "merge" flows of control where
                 one flow has actually exited.  In that case the use counts should
                 normally agree but there may be cases where they don't, maybe
                 associated with statically-linked functions. *)
              if isMerge
              then
                  let
                    val currUseCount =
                      if s indexGeq (pstackVal table) then 0
                      else 
                        case (pstackEntry table s) of
                          NoStackEntry           => 0
                        | StackEntry {uses, ...} => uses
                  in
                    if tabUseCount < currUseCount
                    then incrUseCount (table, s, tabUseCount - currUseCount)
                    else ()
                  end
              else ()
            )
        end;
      val () = putOnEntries first;

      val result: mergeResult =
          case carry of
            MergeIndex _ =>(* Put the result register onto the stack. *)
              ( getRegister (table, cvec, topReg);
                MergeIndex(pushReg (table, topReg))
              )
            | NoMerge => NoMerge
    in
      if pstackTrace then printStack table "setState" (traceContext cvec) else ();
      result
    end
  end;


  fun unconditionalBranch (result, table, cvec) : labels =
    if branchedVal table then noJump
    else let
      val state = saveState (table, cvec);
    in
      branched table := true;
      makeLabels result (CODECONS.unconditionalBranch cvec) state
    end;

  fun jumpBack(start, table, cvec): unit =
    (
    jumpback(start, true, cvec);
    branched table := true
    )

  (* Record the stack limit when we diverge and then use it when we merge
     back again. *)
  type stackMark = { newMark: stackIndex, oldMark: stackIndex };
  
  fun newMark ({newMark,...}: stackMark) = newMark;
  fun oldMark ({oldMark,...}: stackMark) = oldMark;
  
  fun makeStackMark (newM: stackIndex) (oldM: stackIndex) = 
    { newMark = newM, oldMark = oldM };
  
  fun markStack table =
  let
    val oldMark = markerVal table;
    val newMark = pstackVal table;
  in
    marker table := newMark;
    makeStackMark newMark oldMark
  end;

  fun unmarkStack(table, mark) = marker table := oldMark mark;

  (* mergeState is used when two flows of control merge e.g. at the end of
     the else-part of an if-then-else when the state saved at the end of the
     then-part has to be merged with the state resulting from the else-part.
     This function first tries to do what it can to make the current state
     match the saved state.  If it can't do it it may require a "reverse merge"
     where we swap over the saved and current states.  Ideally we would simply
     patch in extra code in the then-part but that's too complicated.  Instead
     "fixup" does it by generating an unconditional branch, fixing up the original
     branch and then calling mergeState to try and merge again.  This should only
     require one reverse to converge.
     I've virtually rewritten this function since it was the source of a number
     of bugs, particularly some identified by Simon Finn.  The aim now is to
     converge by having a (partial) ordering on the types of entries:
        Stack > Register/Cached > Direct.
     We never load a stack entry into a register.
     DCJM 29/6/2000.
     *)
  fun mergeState (save : savedState, savedResult: mergeResult,
                  table as Ttab{pstack, pstackTrace, printStream, ...}, currentResult: mergeResult, cvec, mark) : bool*mergeResult =
  let
    val needOtherWay = ref false;
  in
    if pstackTrace
    then
    ( printStack table ("mergeState") (traceContext cvec);
      printState printStream save "saved state" 
    ) 
    else ();
    if (markerVal table) indexNeq (newMark mark)
    then raise InternalError "Marker"
    else ();
    
    (* Merge the tables together. The only complication is that if both
       sides are returning values they may be at different locations on
       the pseudo stack. We load the top
       of the current stack into the register that was used for the top
       of the saved state and then remove it. There is no need to remove
       the top of the saved state because those entries will correspond
       to zero-use count entries in the current stack. *)
    let
      val topReg =
        case (savedResult, currentResult) of
            (MergeIndex savedTop, MergeIndex currentTop) =>
        let
          val sTopReg = 
            case (pStackRealEntry save savedTop) of
              (_,{ent = Register reg, ...}) =>
                 reg
                 
            | (_,{cache = cacheReg, ...}) =>
                if cacheReg regNeq regNone
                then cacheReg
                else raise InternalError "Not a register";
            
            (* Load the value on the top of "table" into the same register
               (it ought to be there anyway). *)
           val regEntry =
            loadToSpecificReg (cvec, table, sTopReg, currentTop, true);
        in
           (* Because this register will be at a different offset in
              the table from in the saved state it is easier to remove
              the register and put it on later. *)
           removeStackEntry(table, regEntry);
           getRegister (table, cvec, sTopReg);
           sTopReg
         end
         | (NoMerge, NoMerge) => regNone (* Unused *)
         | _ => (* They should agree on whether they will return a result or not. *)
            raise InternalError "mergeState - Mismatched result states"
     in
       (* Clobber all entries above the "mark". These are values which are
          local to the block since the split and so are no longer required.
          They should normally have been removed as soon as they were no
          longer required.  *)
       forIndDownTo (pstackVal table indexMinus 1, newMark mark, fn s => removeEntry table s true);
         
       (* First pass: get rid of entries which are no longer required.
          Also propagate stack destination info.  That probably isn't
          required because it should already have happened (the saved
          state represents a previous state) but shouldn't be a problem. *)
       (* The entries on the stack will only be those that were there
         before we split the instruction streams we are now merging.
         All those pushed since then will be in different positions
         in the saved state and current state and so will be removed
         from the merged state. The common entries may differ if we
         have had to push some values that were in registers onto the
         real stack. *)
       forIndDownTo (pstackVal table indexMinus 1, first,
         fn s =>
            case (pstackEntry table s, pStackEntry save s) of
                (NoStackEntry, _) => () (* No entry in table. *)

            |   (StackEntry _, NoStackEntry) =>
                     (* table entry could be non-empty if it is a cache entry
                        or if we are doing a backwards merge. If we do a
                        backwards merge we can have entries in the table 
                        with non-zero use counts, but those can be removed. *)                 
                      removeEntry table s false

            |   (StackEntry {uses = tabUses, cache = tabCache, ent = tabEnt,
                             destStack = tabDest},
                 StackEntry {uses = saveUses, cache = saveCache, ent = saveEnt,
                             destStack = saveDest}) =>
                    let
                      val mergedDest =
                        if tabDest >= 0
                        then
                          (if saveDest >= 0 andalso tabDest <> saveDest
                           then raise InternalError "merge: mismatched destination"
                           else ();
                           tabDest
                           )
                        else saveDest
                    in
                      if tabUses = 0 orelse saveUses = 0
                       (* The use-counts may be zero if we have retained an
                          entry because it is cached in a register. We remove
                          these entries unless it is the same value and cached 
                          in the same register *)
                      then
                        if tabCache regNeq saveCache
                        then removeEntry table s false
                        else 
                          case (tabEnt, saveEnt) of 
                            (Direct {base = tabBase,  offset = tabOffset},
                             Direct {base = saveBase, offset = saveOffset}) =>
                               if tabBase regEq saveBase andalso tabOffset = saveOffset
                               then ()
                               else removeEntry table s false
                               
                          | (Stack tabIndex, Stack saveIndex) =>
                                if tabIndex = saveIndex
                                then ()
                                else removeEntry table s false
                                
                          | _ => 
                                removeEntry table s false
            
                      else (* We need to retain this entry. *)
                         (
                         if tabDest <> mergedDest
                         then stretchUpdate (pstack, getIndex s,
                                makeStackEntry tabEnt tabCache tabUses mergedDest)
                         else ()
                         )
                    end
         );

       (* Try to align the real stack pointer by popping unused values.
          We MUST remove entries which have been pushed onto the stack
          in the saved state but not in the current state since we'll
          have to push them here.  We must not remove entries which
          are currently in use.  One further complication is that we
          may have exception handler(s) on the real stack so we can't
          simply pop everything above the highest used stack position.
          It would probably be better if we recorded handler locations
          on the pstack - maybe change this. *)
       let
         (* Find the highest stack value which is actually in use. *)
         fun getInUse s i =
            case pstackEntry table s of
                StackEntry {ent = Stack addr, ...} =>
                    (* The stack pointer must be one more
                       than the highest value in use. *)
                    Int.max(addr div ~wordSize+1, i)
            |   _ => i

         val stackInUse =
         (* This is the highest used stack location, but we may have a
            handler above it so we can't necessarily reset the stack
            to here. *)
            revfoldIndDownTo(getInUse, 0, (pstackVal table) indexMinus 1, first)

         (* Examine the saved stack to see those entries which have
            been pushed in the saved state but not in the current
            state. We need to reset the stack below this.  If
            there are no such entries we return the stack pointer
            from the saved state. *)
         fun getMinStack s i =
            case (pstackEntry table s, pStackEntry save s) of
                (StackEntry {ent = Stack _, ...}, _) => i
            |   (StackEntry _, StackEntry{ent = Stack addr, ...}) =>
                    (* We have an entry which has been pushed in
                       the saved state but not in the current state.
                       We have to set sp below this. *)
                    let
                        val minStack = Int.min(addr div ~wordSize, i)
                    in
                        if minStack < stackInUse
                        (* Check that we don't have entries we're going
                           to have to push below those we've already
                           pushed. DCJM 25/1/01. *)
                        then raise InternalError "mergeState: unpushed entries"
                        else minStack
                    end
            |   _ => i;
         val minStack =
            revfoldIndDownTo(getMinStack, realStackPtr save, (pstackVal table) indexMinus 1, first)

         (* We can reset the stack to the maximum of the entries
            currently in use and those which need to be pushed or
            the saved sp if there aren't any. *)
         val maxStack = Int.max(stackInUse, minStack)
       in
        if maxStack < realstackVal table
        then
           (
            resetStack (realstackVal table - maxStack, cvec);
            realstackptr table := maxStack
           )
        else ()
       end;

       (* Second pass: push any entry which was pushed in the saved state. *)
       (* We have a choice here about what to do when we have a value
         which is in a register on one branch and on the stack in the
         other.  The original approach was to get both values back
         into the register by reloading the register from the stack.
         That worked well on the Sparc where there were plenty of
         registers but less well on the i386.  The advantage is that
         if we have a branch which is small and frequently taken
         we don't incur any cost.
         e.g. "val x = ...; val y = if ... then 1 else f();"
         The register containing x has to be pushed before we call
         f but not before 1.  If the then-branch is most frequently
         taken we don't want to incur extra cost by pushing x on that
         branch as well.
         There are two disadvantages of trying to reload registers.
         The first is that we may have to spill other registers as
         we do it and end up thrashing around trying to get the
         values into the correct registers.  The other is that if
         we have to push the registers anyway we've incurred extra
         cost.
         The current approach is to move values to the stack.  *)
       let
          fun mustPush s =
            case (pstackEntry table s, pStackEntry save s) of
                (StackEntry {ent = Stack _, ...}, StackEntry {ent = Stack _, ...}) =>
                    false (* both on stack *)
            |   (StackEntry _, StackEntry {ent = Stack _, ...}) =>
                    true (* Saved value is on stack but current value isn't. *)
            |   _ => false
       in
          (* Pushing one entry may result in others being pushed if
             they have a lower "destStack". *)
          pushRegisters table cvec allRegisters mustPush false
       end;
 
       (* Third pass: Load any entry which is in a register in the saved state
          and ensure that values in registers in the current state are moved
          into the same register as before. *)
       let
            (* Put the table entry in a specified register and
               make it a register entry. *)
            fun loadToReg (s, prefReg, tabEnt, tabCache, tabUses, tabDest) =
            let
              val dReg =
                if prefReg regEq regStackPtr
                then (* No preference. *)
                let
                  val reg = getAnyRegister (table, cvec);
                in
                  loadPstackEntry table s reg cvec;
                  reg
                end
                else 
                  (* Put it in the preferred register. If it's already there
                     we need to increment the use count because we will
                     decrement it in "removeEntry". *)
                  if (case tabEnt of Register reg => reg regEq prefReg | _ => false) orelse
                     tabCache regEq prefReg  (* Already there. *)
                  then (addRegUse (table, prefReg); prefReg) (* Already there. *)
                  else
                    (
                      getRegister (table, cvec, prefReg);
                      loadPstackEntry table s prefReg cvec;
                      prefReg
                    )
            in 
              (* loadPstackEntry will have decremented the use count and may
                 have completely removed the entry.  If it hasn't we need to
                 remove it before we replace it with the loaded register.
                 If we didn't call loadPstackEntry (because we already had
                 the value in the correct register) we have to call removeEntry
                 to decrement the register use count (we incremented it above)
                 and so restore it to the original value. *)
              case (pstackEntry table s) of
                NoStackEntry => ()
              | StackEntry _ => removeEntry table s false
              ;
              stretchUpdate (pstack, getIndex s,
                      makeStackEntry (Register dReg) regNone
                        tabUses tabDest)
            end (* loadToReg *)

          fun loadEntries s =
             case (pstackEntry table s, pStackEntry save s) of
                (StackEntry {ent = Stack _, ...}, _) =>
                    (* If it's in the stack we don't try reloading it. *) ()

             |  (StackEntry {ent = tabEnt, cache, uses, destStack, ...},
                 StackEntry {ent = Register savedReg, ...}) =>
                    loadToReg(s, savedReg, tabEnt, cache, uses, destStack)

             |  (StackEntry{ent = tabEnt as Direct{base = tabBase, offset = tabOffset},
                            cache = tabCache, uses, destStack, ...},
                 StackEntry{ent = Direct{base = saveBase, offset = saveOffset},
                            cache = savedCache, ...}) =>
                    (
                    if tabOffset <> saveOffset
                    then raise InternalError "merge: mismatched offsets"
                    else ();
                    (* If the base registers are different (which might
                       happen if the original reg was required) we need
                       to load this entry.  We will probably also need
                       to do a reverse merge and load the corresponding
                       entry in the saved state. *)
                    if tabBase regNeq saveBase
                    then
                        let
                            val prefReg =
                                if savedCache regNeq regNone
                                then savedCache
                                else if tabCache regNeq regNone
                                then tabCache
                                else regStackPtr; (* No preference. *)
                        in
                          loadToReg (s, prefReg, tabEnt, tabCache, uses, destStack)
                        end
                    else ()
                    )
             |  _ => ()
 
       in
          forIndDownTo (pstackVal table indexMinus 1, first, loadEntries)
       end;

       (* Final pass: Check to see if we need to do a "reverse merge" i.e.
          operations that have to be done on the saved state before we
          can finally merge.  Also flush mismatched items from the cache. *)
       let
          fun checkEntries s =
             case (pstackEntry table s, pStackEntry save s) of
                (StackEntry {uses = tabUses, cache = tabCache, ent = tabEnt,
                             destStack = tabDest},
                 StackEntry {cache = saveCache, ent = saveEnt, ...}) =>
                    let
                        fun flushCache () =
                            if tabCache regNeq regNone andalso tabCache regNeq saveCache
                            then
                               (
                               free (regset table) tabCache;
                               stretchUpdate (pstack, getIndex s,
                                  makeStackEntry tabEnt regNone tabUses tabDest)
                               )
                            else ()
                    in
                    case tabEnt of
                        Register tabReg =>
                            (
                            (* It's fine if the saved value was cached in that
                               register. *)
                            if saveCache regEq tabReg then ()
                            else case saveEnt of
                                Register saveReg =>
                                    (* We should have moved these into the same
                                       register.  It's possible it got moved again
                                       as a result of loading something else. *)
                                    if tabReg regNeq saveReg
                                    then needOtherWay := true else ()
                            |   Stack _ =>
                                    (* We should have pushed it in the second pass. *)
                                    raise InternalError "merge: unpushed entry"
                            |   _ => (* Maybe a Direct entry which has to be
                                        loaded in a reverse merge. *)
                                    needOtherWay := true
                            )
                    |   Literal _ =>
                            (
                            case saveEnt of
                                Literal _ => flushCache()
                            |   _ => raise InternalError "Literal mismatch"
                            )
                    |   CodeRef _ =>
                            (
                            case saveEnt of
                                CodeRef _ => flushCache()
                            |   _ => raise InternalError "Coderef mismatch"
                            )
                    |   Direct {base = tabBase, ...} =>
                            (
                            (* As with register entries these should have been
                               merged but might have diverged again. *)
                            if saveCache regEq tabCache then ()
                            else
                                case saveEnt of
                                    Direct{base=saveBase, ...} =>
                                       if tabBase regEq saveBase
                                       then flushCache() (* Ok but must flush cache. *)
                                       else needOtherWay := true
                                |   _ =>
                                        raise InternalError "merge: mismatched Direct"
                            )
                    |   Stack tabIndex =>
                            (
                            case saveEnt of
                                Stack saveIndex =>
                                    (
                                    (* Consistency check. *)
                                    if tabIndex = saveIndex then ()
                                    else raise InternalError "merge: mismatched stack entries";
                                    flushCache()
                                    )
                                | _ => (* Need to push this in a reverse merge. *)
                                    needOtherWay := true
                            )
                    |   Container _ =>
                            (
                            case saveEnt of
                                Container _ => ()
                            |   _ => raise InternalError "merge: mismatched Container"
                            )
                    end
              | _ => ()
       in
          forIndDownTo (pstackVal table indexMinus 1, first, checkEntries)
       end;

       (* Last of all, try to align the stack. If the current stack pointer
          is greater than the saved value we must have live values on the
          stack and have to do a reverse merge.  If the saved stack pointer
          was greater than the current but otherwise everything is fine
          we just push some dummy values rather than doing a reverse merge.
          I may change this later. *)
       if realStackPtr save < realstackVal table
       then needOtherWay := true
       else ();

       if ! needOtherWay then ()
       else while realStackPtr save > realstackVal table
        do (* Have to push something in order to align the stack. *)
        if pushAnyEntryAtCurrentSP(table, cvec) then ()
        else (* Push a register just to align the stack.  It would
                be better to push a register that wasn't currently
                saved but this will do for the moment. *)
             (
             genPush (regN 0, cvec);
             realstackptr table := realstackVal table + 1
             );

        (* Push any result. *)
        let
            val result =
                case currentResult of
                    MergeIndex _ => MergeIndex(pushReg (table, topReg))
                  | NoMerge => NoMerge
        in
        if pstackTrace then printStack table "mergeState" (traceContext cvec) else ();
 
        (!needOtherWay, result)
        end
     end
  end
    handle SML90.Interrupt => raise SML90.Interrupt
      | e =>
      (
        printStack table "mergeState" (traceContext cvec);
        printState printStream save "saved state";
        raise e
     );

  (* Fix up a label after an unconditional branch. *)
  fun fixup (lab, table, cvec) : unit =
    if not (branchedVal table) then raise InternalError "Not branched"
    else if isEmptyLabel lab then ()
    else
    ( 
      setState (state lab, table, cvec, NoMerge, pstackVal table, false);
      branched table := false;
      exited table := false;
      cFixup (labs lab, cvec)
    );
    
  local
    (* Fix up a label.  If this follows an unconditional branch we replace the
       existing state with the saved state, otherwise we have to merge in. *)
    fun mergeLab (lab, table, cvec, currentResult: mergeResult, mark) : mergeResult =
      if isEmptyLabel lab then currentResult
      else
    if ! (branched table)
    then let
        val newResult = setState (state lab, table, cvec, result lab, newMark mark, true);
      in
        branched table := false;
        exited table := false;
        cFixup (labs lab, cvec);
        newResult
      end
    else let
      val (otherWay, mergeRes) =
        mergeState (state lab, result lab, table, currentResult, cvec, mark);
    in (* We can generate code before we fix up the label, but if we
          want to add code to the other arm we have to put in an
          unconditional branch and make the changes after it. *)
      if otherWay
      then let
        (* Have to jump round to get the states the same. *)
        val lab1 = unconditionalBranch (mergeRes, table, cvec);
        val newResult = setState (state lab, table, cvec, result lab, newMark mark, true)
      in
        exited table := false;
        branched table := false;
        cFixup (labs lab, cvec);
        mergeLab (lab1, table, cvec, newResult, mark) (* Merge the other way. *)
      end
      else
          (cFixup (labs lab, cvec); mergeRes)
    end;
  in
    (* Fix up a label.  If this follows an unconditional branch we replace the
       existing state with the saved state, otherwise we have to merge in. *)
    fun merge (lab, table, cvec, carry, mark) : mergeResult =
    let
       val res = mergeLab (lab, table, cvec, carry, mark);
    in (* Reset the marker even if we have not actually done any merging. *)
      unmarkStack(table, mark);
      res
    end;

    (* Fix up a list of labels, using the same stack mark *)
    fun mergeList (labs, table, cvec, carry, mark) : mergeResult =
    let
      fun mergeLabs (l, carry) = mergeLab (l, table, cvec, carry, mark)
    
      val mergeRes = List.foldl mergeLabs carry labs
    in
      unmarkStack(table, mark);
      mergeRes
    end;
  end;
  
  type handler = { lab: handlerLab, oldps: stackIndex };
  
  (* Push the address of a handler. *)
  fun pushAddress (table, cvec, offset) : handler =
  let
    (* This is just after a mark. *)
    val reg = getAnyRegister(table, cvec)
    val oldps = pstackVal table

    (* Load the address of the handler into a register. *)
    val handlerLab = loadHandlerAddress(reg, cvec);
    val regEntry = pushReg(table, reg)

    (* Push it onto the stack at the specific offset. *)
    val pushedEntry = pushValueToStack (cvec, table, regEntry, offset);
  in
    (* Remove the pstack entry because we don't want it. *)
    removeStackEntry(table, pushedEntry);
    {lab = handlerLab, oldps = oldps}
  end;
  
  (* Fixup the address at the start of a handler. *)
  fun fixupH ({lab, oldps}, oldsp, table, cvec) : unit =
  ( clearCache table; (* Don't know the registers here. *)
    realstackptr table := oldsp;
    exited table := false;
    branched table := false;

   (* Remove any entries above the old pstack pointer. If the expression
      whose exceptions we are handling contained static-link functions
      there may be entries whose use-counts have not gone to zero. *)
   forIndDownTo (pstackVal table indexMinus 1, oldps,
      fn (s: stackIndex) => removeEntry table s false);

    cFixupH (lab, cvec)
  );

  (* Generate a binary operation. *)
  fun binaryOp (arg1, arg2, instr, revinstr, table as Ttab{pstackTrace, ...}, cvec, hint) : stackIndex =
  let (* If the operand is a short constant we don't need to put it in a register. *)
    val (_,{ent = firstEnt, ...})  = 
       pstackRealEntry table arg1;
       
    val (_,{ent = secondEnt,...}) =
       pstackRealEntry table arg2;
       
    fun genBinaryIR lit =   
    let
      val (initialReg2, initialReg2Loc) = loadEntry (cvec, table, arg2, false);
      
      (* Lock the register to make sure that
         we don't accidentally reuse it for the result. *)
      val () = lockRegister (table, initialReg2);

      (* Get a result register. It's a shame that if the value is
         already in the desired result register, then we have to
         choose a different register for the result. There are two
         reasons for this:
        (1) The i386 code generator can't share argument and result registers.
        (2) Even for the other code-generators, we have a problem because
        we requested non-exclusive use of the argument, so we can't
        zap it with the instruction we're about to execute, because its
        use-count might be greater than 1. One day I'll come back and improve
        this code further.
     
     What we want to do is to reserve the result register first, then load the
     argument. Unfortunately, this doesn't quite work - if the value is cached
     in the result register, this code will move it out, which we don't want
     (unless there's more than a single use required). I guess I'll need to
     write another low-level register allocation function.
     SPF 17/2/97
     
     Current attempt: load the argument first, but try to resuse this register
     for the result if it's safe to do so. This is still not a perfect
     approach, but it should generate better code than the previous version.
     SPF 6/3/97
      *)

      val (arg2Reg, arg2RegLoc, resReg) : reg * stackIndex * reg = 
        case hint of
          NoHint         => (initialReg2, initialReg2Loc, getAnyRegister (table, cvec))
        | UseReg prefReg => 
            if prefReg regEq initialReg2
            then if canShareRegs
              then let (* We WANT to reuse the argument register for the result *)
                val ()      = unlockRegister (table, initialReg2);
                val (arg2Reg, arg2RegLoc) = loadEntry (cvec, table, initialReg2Loc, true);
                val ()      = addRegUse (table, arg2Reg); (* For use as result reg. *)
                val ()      = lockRegister (table, arg2Reg);
              in
                (arg2Reg, arg2RegLoc, arg2Reg)
              end
              else (initialReg2, initialReg2Loc, getAnyRegister (table, cvec))
            else let
              val () = getRegister (table, cvec, prefReg);
            in
              (initialReg2, initialReg2Loc, prefReg)
            end;
      
      (* Generate the code. Since we've reversed the operation *)
      (* we have to use the reverse instruction. *)
      val ()   =  genRI (revinstr, arg2Reg, lit, resReg, cvec);
      
      (* Push the result onto the stack. *)
      val rreg       = pushReg (table, resReg); 
      val ()   = unlockRegister (table, arg2Reg);
    in
      (* Remove the argument register. *)
      incrUseCount (table, arg2RegLoc, ~1);
      incrUseCount (table, arg1, ~1);
      rreg
    end;       
       
    fun genBinaryRI lit =   
    let
      val (initialReg1, initialReg1Loc) = loadEntry (cvec, table, arg1, false);
      val () = lockRegister (table, initialReg1);

      val (arg1Reg, arg1RegLoc, resReg) : reg * stackIndex * reg = 
        case hint of
          NoHint         => (initialReg1, initialReg1Loc, getAnyRegister (table, cvec))
        | UseReg prefReg => 
            if prefReg regEq initialReg1
            then if canShareRegs
              then let (* We WANT to reuse the argument register for the result *)
                val ()      = unlockRegister (table, initialReg1);
                val (arg1Reg, arg1RegLoc) = loadEntry (cvec, table, initialReg1Loc, true);
                    val ()      = addRegUse (table, arg1Reg); (* For use as result reg. *)
                    val ()      = lockRegister (table, arg1Reg);
              in
                (arg1Reg, arg1RegLoc, arg1Reg)
              end
              else (initialReg1, initialReg1Loc, getAnyRegister (table, cvec))
            else let
              val () = getRegister (table, cvec, prefReg);
            in
              (initialReg1, initialReg1Loc, prefReg)
            end;
      
      val ()    = genRI (instr, arg1Reg, lit, resReg, cvec);
      val rreg        = pushReg (table, resReg);
      val ()    = unlockRegister (table, arg1Reg);
    in
      incrUseCount (table, arg1RegLoc, ~1);
      incrUseCount (table, arg2, ~1);
      rreg
    end;

    fun genBinaryRR () = 
    let
      val (initialReg1, initialReg1Loc) = loadEntry (cvec, table, arg1, false);
      val ()       = lockRegister (table, initialReg1);
      
      val (initialReg2, initialReg2Loc) = loadEntry (cvec, table, arg2, false);
      val ()       = lockRegister (table, initialReg2);
      
      (* We could improve this by considering what happens if arg1Reg and arg2Reg both share with prefReg,
         but that's getting too obscure to be worthe considering, and would require the
         construction of new low-level code, which I want to avoid.
         
         Note that even in the current code, initialReg1 = prefReg does NOT imply that arg1Reg = prefReg,
         because we might have needed to copy the value to another register if its use-count was
         more than 1.
         SPF 6/3/97
      *)
      val (arg1Reg, arg1RegLoc, arg2Reg, arg2RegLoc, resReg) : reg * stackIndex * reg * stackIndex * reg = 
        case hint of
          NoHint         => (initialReg1, initialReg1Loc, initialReg2, initialReg2Loc, getAnyRegister (table, cvec))
        | UseReg prefReg => 
            if prefReg regEq initialReg1
            then if canShareRegs
              then let (* We WANT to reuse the argument register for the result. *)
                val ()      = unlockRegister (table, initialReg1);
                val (arg1Reg, arg1RegLoc) = loadEntry (cvec, table, initialReg1Loc, true);
                    val ()      = addRegUse (table, arg1Reg); (* For use as result reg. *)
                    val ()      = lockRegister (table, arg1Reg);
              in
                (arg1Reg, arg1RegLoc, initialReg2, initialReg2Loc, arg1Reg)
              end
              else (initialReg1, initialReg1Loc, initialReg2, initialReg2Loc, getAnyRegister (table, cvec))
              
            else if prefReg regEq initialReg2
            then if canShareRegs
              then let (* We WANT to reuse the argument register for the result *)
                val ()      = unlockRegister (table, initialReg2);
                val (arg2Reg, arg2RegLoc) = loadEntry (cvec, table, initialReg2Loc, true);
                    val ()      = addRegUse (table, arg2Reg); (* For use as result reg. *)
                    val ()      = lockRegister (table, arg2Reg);
              in
                (initialReg1, initialReg1Loc, arg2Reg, arg2RegLoc, arg2Reg)
              end
              else (initialReg1, initialReg1Loc, initialReg2, initialReg2Loc, getAnyRegister (table, cvec))
            
            else let
              val () = getRegister (table, cvec, prefReg);
            in
              (initialReg1, initialReg1Loc, initialReg2, initialReg2Loc, prefReg)
            end;

      val ()   = genRR (instr, arg1Reg, arg2Reg, resReg, cvec);
      val rreg = pushReg (table, resReg);
      val ()   = unlockRegister (table, arg1Reg);
      val ()   = unlockRegister (table, arg2Reg);
    in
      incrUseCount (table, arg2RegLoc, ~1); (* Remove the register entries. *)
      incrUseCount (table, arg1RegLoc, ~1);
      rreg
    end;
    
   val result : stackIndex =
     (* If it is a constant we have to check that it is in the range *)
     (* for the instruction we want to use. *)
     case (firstEnt,secondEnt) of
       (Literal lit1, Literal lit2) =>
          (* optimisation should have already dealt with this? *)
          if instrIsRI (revinstr, lit1)
             then genBinaryIR lit1
          else if instrIsRI (instr, lit2)
            then genBinaryRI lit2
          else genBinaryRR ()
          
     | (Literal lit1, _) =>
          if instrIsRI (revinstr, lit1)
             then genBinaryIR lit1
          else genBinaryRR ()
          
     | (_,Literal lit2) =>
          if instrIsRI (instr, lit2)
            then genBinaryRI lit2
          else genBinaryRR ()
          
     | (_,_) =>
          genBinaryRR ();

    val () = if pstackTrace then printStack table "binaryOp" "" else ();
  in 
    result
  end;


  fun assignOp (addr: stackIndex, offset: stackIndex, value: stackIndex,
                width:storeWidth, table: ttab, cvec: code) =
  let
    val (_,{ent = offsetEnt,...}) = pstackRealEntry table offset
    and (_,{ent = valueEnt,...}) = pstackRealEntry table value

    val (addrReg, addrEntry)   = loadEntry (cvec, table, addr, false)
    (* This register must be locked so that we don't reuse it for the
       index or value. *)
    val ()  = lockRegister (table, addrReg);

    (* The values are indexes but if we use a constant offset it must be in bytes. *)
    val unitSize = case width of STORE_WORD => wordSize | STORE_BYTE => 1

    fun storeViaRegister offset =
      let
        val (valReg, regEntry) = loadEntry (cvec, table, value, false);
      in
        genStore (valReg, offset, addrReg, width, regNone, cvec);
        removeStackEntry(table, regEntry) (* Remove value entry. *)
      end;

    fun storeIndexedViaRegister () =
      let
        val (indexReg, indexRegEntry) = loadEntry (cvec, table, offset, false);
        val ()  = lockRegister (table, indexReg);
        val (valReg, regEntry) = loadEntry (cvec, table, value, false);
      in
        genStore (valReg, 0, addrReg, width, indexReg, cvec);
        removeStackEntry(table, regEntry); (* Remove value entry. *)
        unlockRegister (table, indexReg); (* Release the index register. *)
        removeStackEntry(table, indexRegEntry)
      end

  in
        case (offsetEnt, valueEnt) of
            (Literal litOffset, Literal litValue) =>
                (* Constant offset and constant value to be assigned. *)
                let
                    val offsetInt = Word.toInt (toShort litOffset) * unitSize
                in
                    if isStoreI(litValue, width, false)
                    then
                        (
                        genStoreI(litValue, offsetInt, addrReg, width, regNone, cvec);
                        (* Remove the entry for the value. *)
                        removeStackEntry(table, value)
                        )
                    else storeViaRegister offsetInt;
                    (* Remove the index. *)
                    removeStackEntry(table, offset)
                end

        |   (Literal litOffset, _) =>
                let
                    val offsetInt = Word.toInt (toShort litOffset) * unitSize
                in
                    (* Constant offset but general value to be assigned. *)
                    storeViaRegister offsetInt;
                    (* Remove the index. *)
                    removeStackEntry(table, offset)
                end

        |   (_, Literal litValue) =>
                if isStoreI(litValue, STORE_WORD, true)
                then 
                (* Constant value but index value in register. *)
                let
                    val (indexReg, indexRegEntry) =
                        loadEntry (cvec, table, offset, false)
                in
                    genStoreI(litValue, 0, addrReg, width, indexReg, cvec);
                    (* Remove the entry for the value. *)
                    removeStackEntry(table, value);
                    (* Remove the index. *)
                    removeStackEntry(table, indexRegEntry)
                end
                else storeIndexedViaRegister ()

        |   _ => storeIndexedViaRegister ()
        ;

       (* Release the base register. *)
       unlockRegister (table, addrReg);
       removeStackEntry(table, addrEntry)
    end


  (* Generate a binary compare and jump operation. *)
  fun compareAndBranch (arg1, arg2, t, revt, table as Ttab{pstackTrace, ...}, cvec) : labels =
  let
    val (_,{ent = firstEnt,...})  = pstackRealEntry table arg1;
    val (_,{ent = secondEnt,...}) = pstackRealEntry table arg2;

    fun genCompIR test lit =
    let
      val (arg2Reg, arg2RegLoc) = loadEntry (cvec, table, arg2, false);
    in
      (* Remove the register entry from the stack. *)
      incrUseCount (table, arg2RegLoc, ~1);
      incrUseCount (table, arg1, ~1);
      (* Generate the code. We have reversed it - Use the reversed test. *)
      compareAndBranchRI (arg2Reg, lit, test, cvec) 
    end;

    fun genCompRI test lit =
    let
      val (arg1Reg, arg1RegLoc) = loadEntry (cvec, table, arg1, false);
    in
      incrUseCount (table, arg1RegLoc, ~1);
      incrUseCount (table, arg2, ~1);
      compareAndBranchRI (arg1Reg, lit, test, cvec) 
    end;
      
    fun genCompRR test =
    let
      val (arg1Reg, arg1RegLoc) = loadEntry (cvec, table, arg1, false);
      val ()   = lockRegister (table, arg1Reg);
      val (arg2Reg, arg2RegLoc) = loadEntry (cvec, table, arg2, false);
      val ()   = unlockRegister (table, arg1Reg);
    in
      (* Remove the register entries. *)
      incrUseCount (table, arg1RegLoc, ~1);
      incrUseCount (table, arg2RegLoc, ~1);
      compareAndBranchRR (arg1Reg, arg2Reg, test, cvec)
    end;
      
    val lab = 
      case (firstEnt, secondEnt) of
        (Literal lit1, Literal lit2) =>
           if isCompRI (revt, lit1)
             then genCompIR revt lit1
           else if isCompRI (t, lit2)
             then genCompRI t lit2
           else genCompRR t
    
      | (Literal lit1, _) =>
           if isCompRI (revt, lit1)
             then genCompIR revt lit1
           else genCompRR t
    
      | (_, Literal lit2) =>
           if isCompRI (t, lit2)
             then genCompRI t lit2
           else genCompRR t
    
      | (_, _) =>
           genCompRR t
  in
    if pstackTrace then printStack table "compareAndBranch" "" else ();

    (* Package the label up with the state. *)
    makeLabels NoMerge lab (saveState (table, cvec))
  end 

  (* Tail recursive jump to a function. *)
  fun jumpToCode(codeAddr, isIndirect, returnReg, transtable, cvec) =
  let
     val (_, {ent, ...}) = pstackRealEntry transtable codeAddr 
  in
     case ent of
        Literal lit =>
            jumpToFunction(ConstantFun(lit, isIndirect), returnReg, cvec)

     |  CodeRef code =>
            if isIndirect
            then raise InternalError "jumpToCode: indirect call to codeRef"
            else jumpToFunction(CodeFun code, returnReg, cvec)

     |  Register reg => (* Should only be the closure register and only in
                           the indirect case. *)
            if isIndirect andalso reg regEq regClosure
            then jumpToFunction(FullCall, returnReg, cvec)
            else raise InternalError "jumpToCode: Not indirection through closure reg"

     |  _ => (* Anything else shouldn't happen. *)
            raise InternalError "jumpToCode: Not a constant or register";

    incrUseCount (transtable, codeAddr, ~1)
  end;

  (* Call a function. *)
  fun callCode(codeAddr, isIndirect, transtable, cvec) =
  let
     val (_, {ent, ...}) = pstackRealEntry transtable codeAddr 
  in
     case ent of
        Literal lit => callFunction(ConstantFun(lit, isIndirect), cvec)

     |  CodeRef code =>
            if isIndirect
            then raise InternalError "callCode: indirect call to codeRef"
            else callFunction(CodeFun code, cvec)

     |  Register reg => (* Should only be the closure register and only in
                           the indirect case. *)
            if isIndirect andalso reg regEq regClosure
            then callFunction(FullCall, cvec)
            else raise InternalError "callCode: Not indirection through closure reg"

     |  _ => (* Anything else shouldn't happen. *)
            raise InternalError "callCode: Not a constant or register";
    incrUseCount (transtable, codeAddr, ~1)
  end;

  fun startCase (table, cvec, state) : addrs =
    (
      setState (state, table, cvec, NoMerge, pstackVal table, false);
      exited table := false;
      branched table := false;
      ic cvec
    );

  (* These are exported as read-only. *)
  val maxstack     = maxstackVal;
  val realstackptr = realstackVal;
  val haveExited = exitedVal

  (* This is called when we have either made a tail-recursive call,
     returned from a function or raised an exception. *)
  fun exiting table =
  (
    branched table := true;
    exited table := true
  )

val allRegisters = allRegisters


datatype argdest = ArgToRegister of reg | ArgToStack of int | ArgDiscard

(* Get the destination for the argument of a loop instruction.  This
   finds out where the argument was loaded at the start of the loop
   so that it can be put back there at the end. *)
fun getLoopDestinations(indices, transtable) =
    let
        fun getLoopDest entry =
            if entry indexEq noIndex
            then ArgDiscard
            else case pstackEntry transtable entry of
                NoStackEntry => raise InternalError "getLoopDest: no entry"
            |   StackEntry{ent = Stack index, ...} => ArgToStack index
            |   StackEntry{ent = Register reg, ...} => ArgToRegister reg
            |   _ => raise InternalError "getLoopDest: wrong entry type"
    in
        map getLoopDest indices
    end

end; (* local for stackUnion datatype declaration *)

end (* TRANSTAB *)

end; (* let for body of TRANSTAB *)
