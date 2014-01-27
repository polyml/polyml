(*
    Copyright David C. J. Matthews 1991, 2000-2001, 2009-10, 2012-13

    Derived from code

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

(* This part of the code-generator deals with the pseudo-stack and the
  translation of addresses into stack offsets. *)

(* TODO: Many of the functions involve complete scans of the pseudo-stack.
   This leads to quadratic increase in compile-time with large numbers
   of bindings.  Hotspots are marked in the code. *)

functor CODEGEN_TABLE (

    structure CODECONS : CODECONSSIG

    structure DEBUG :
    sig
        (* Produce debugging output. *)
        val pstackTraceTag : bool Universal.tag
        val getParameter :
           'a Universal.tag -> Universal.universal list -> 'a
    end;

    structure PRETTY: PRETTYSIG

    structure MISC :
    sig
        exception InternalError of string;
        val quickSort : ('a -> 'a -> bool) -> 'a list -> 'a list
    end
    
    sharing PRETTY.Sharing = CODECONS.Sharing

) : CODEGEN_TABLESIG =

struct

    open CODECONS;
    open DEBUG;
    open MISC;
    open Address;
    structure StretchArray = StretchArray

    open RegSet
    

    fun forLoop f i n = if i > n then () else (f i; forLoop f (i + 1) n);
 
    type stackIndex = int 

    val first: stackIndex = 0
    val noIndex: stackIndex = ~1 (* An invalid index. *)

    datatype stackUnion = 
        Register  of reg * int         (* In a register.  The int is the stack offset if
                                          the value was originally a container. *)
    |   Literal   of machineWord        (* A constant (was "int") *)
    |   CodeRef   of code        (* Forward reference to code *)
    |   Direct    of {base: reg, offset: int}  (* Register/Offset *)
    |   StackW    of int         (* On the real stack. *)
        (* Container entries are a group of items on the stack.  They are used
           for tuples and also for function closures.  A function closure
           can contain the addresses of other function closures so these
           must be retained until the last references to the closure
           that references them.  *)
    |   Container of { items: stackIndex list, dependencies: stackIndex list}

    datatype stackEntry =
        NoStackEntry
    |   StackEntry of 
        {
            ent:   stackUnion,
            cache: regSet,
            uses:  int,
            destStack: int option,
                (* destStack is used to indicate where on the stack this
                   entry must be placed if it has to be spilled.  A value
                   of NONE means that we don't care.  The reason for having
                   this is to ensure that if we split a flow of control
                   (e.g. the then- and else-parts of an "if") with a value
                   in a register and have to spill the register in one branch
                   then we spill it to the same location on the other branch.
                   This ensures that when we merge the flows of control we don't
                   have to mess around with the stack.  *)
            lifeTime: int
                (* lifeTime is a measure of how long the item will live.
                   Because these values are derived from a depth first
                   scan in preCode lower values mean longer life except
                   that zero means temporary.  The lifeTime is used to
                   decide which items to spill from registers if we
                   need to. *)
        }

    fun forIndDownTo(i, finishInd, perform: stackIndex->unit) =
        if i >= finishInd
        then (perform i; forIndDownTo (i - 1, finishInd, perform))
        else ()

    fun revfoldIndDownTo(f, x, i, finishInd) =
        if i >= finishInd
        then revfoldIndDownTo(f, f i x, i - 1, finishInd)
        else x

  (* The set of available registers. A register is free if its use-count
     is zero. A register has a reference for each occurence in an entry
     on the pseudo-stack or cache field (even if the use-count of the
     stack entry is zero). *)
  (* Added modification set to indicate if a register has been changed.
     We assume that if a register is allocated it has been modified.
     DCJM 26/11/00. *)
     
    type rset = {vec: int array, modSet: regSet ref, freeRegs: regSet ref};
    
    val vecSize = regs;
  
    (* Returns the first free register. *)
    (* This is a major hotspot in the compiler. *)
    fun getAcceptableRegister ({vec, modSet, freeRegs}:rset, acceptable: regSet) =
    let
        val available = regSetIntersect(!freeRegs, acceptable)
    in
        if available = noRegisters
        then NONE
        else (* Mark the register as in use. *)
            let
                val r = oneOf available
                val i = nReg r
            in
                Array.update (vec, i, 1); (* Set the register use-count to 1 *)
                modSet := regSetUnion(singleton r, ! modSet); (* Mark as used *)
                freeRegs := regSetMinus(!freeRegs, singleton r);
                SOME r
            end
    end

    (* Print the allocated registers. *)
    fun printRegs printStream ({vec, ...}) =
    let
        fun printReg i =
        let
            val useCount = Array.sub (vec, i)
        in
            if useCount > 0
            then
            (
                printStream " "; printStream (regRepr (regN i));
                printStream "="; printStream(Int.toString useCount)
            )
            else ()
        end (* printReg *);
    in
        forLoop printReg 0 (vecSize - 1)
    end

    fun free ({freeRegs, vec, ...}) reg =
    let
        val r = nReg reg
        val useCount = Array.sub (vec, r)
    in
        if useCount = 0
        then raise InternalError ("free: register already free:" ^ regRepr(regN r))
        else
        (
            Array.update (vec, r, useCount - 1);
            if useCount = 1
            then
            (
                freeRegs := regSetUnion(!freeRegs, singleton reg);
                freeRegister reg
            )
            else []
        )
    end

    fun freeSet(regs, regSet) =
        List.foldl (fn (r, l) => free regs r @ l) [] (setToList regSet)
  
    (* Increment use count. *)
    fun incr ({freeRegs, vec, ...}) reg =
    let
        val r = nReg reg
        val useCount = Array.sub (vec, r)
    in
        Array.update (vec, r, useCount + 1);
        if useCount = 0
        then
        (
            freeRegs := regSetMinus(!freeRegs, singleton reg);
            activeRegister reg
        )
        else []
    end

    fun lastRegRef({vec, ...}, reg) = Array.sub (vec, nReg reg) = 1

    fun rsetMake () : rset = {vec = Array.array (vecSize, 0), modSet = ref noRegisters, freeRegs = ref allRegisters}
  
    (* This table maps declaration numbers for a particular function
       into pseudo-stack offsets. The pseudo-stack simulates the real stack and
       gives the real locations of objects which may be in store, on the real stack
       or in registers. It maintains use-counts for values and allows the stack
       to be contracted and registers to be re-used when they are no longer required.              *)

    datatype ttab = Ttab of
    {
        regset:       rset,
        pstack:       stackEntry StretchArray.stretchArray, (* Pseudo-stack *)
        pstackptr:    stackIndex ref,
        realstackptr: int ref,     (* The number of items on the real stack. *)
        maxstack:     int ref,     (* The maximum number of items on the real stack. *)
        exited:       bool ref,
        branched:     bool ref,
        marker:       stackIndex ref,
        markReal:     int ref,
        lowestDirect: stackIndex ref,
        pstackTrace:  bool,
        printStream:  string->unit
    }
     
    fun ttabCreate(localCount, debugSwitches) =
    let
        val printStream = PRETTY.getSimplePrinter debugSwitches
    in
        Ttab
        {
            regset       = rsetMake(),
            pstack       = StretchArray.stretchArray ((* Hint *)localCount, NoStackEntry),
            pstackptr    = ref first,
            realstackptr = ref 0,
            maxstack     = ref 1,
            exited       = ref false,
            branched     = ref false,
            marker       = ref first,
            markReal     = ref 0,
            lowestDirect = ref first,
            pstackTrace  = DEBUG.getParameter DEBUG.pstackTraceTag debugSwitches,
            printStream  = printStream
        }
    end

    fun pstackEntry (Ttab{pstack, ...}, locn) = StretchArray.sub(pstack, locn)
    and setPstackEntry(Ttab{pstack, ...}, locn, entry) = StretchArray.update(pstack, locn, entry)

    (* Returns the entry after removing the option type. *)
    fun pstackRealEntry (ttab, locn) =
    case pstackEntry(ttab, locn) of
        StackEntry record => record
    |   NoStackEntry                    =>
           raise InternalError ("pstackRealEntry: not entry: " ^ Int.toString(locn))

    fun printStackUnion printStream stackun =
      case stackun of
        Register(reg, _) => printStream(regRepr reg)
          
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

     |  StackW i =>
        (
            printStream "base@(";
            printStream(Int.toString i);
            printStream ")"
        )

     | Container{items, ...} =>
        (
            printStream "[";
            List.app (fn i => (printStream(Int.toString(i)); printStream " ")) items;
            printStream "]"
        )

    fun printEntry _ NoStackEntry _ = ()
    |   printEntry printStream (StackEntry {ent, uses, cache, destStack, lifeTime}) entry =
    (
        printStream(Int.toString(entry));
        printStream " ";
      
        printStream(Int.toString uses);
        printStream " ";
      
        printStackUnion printStream ent;
      
        if cache <> noRegisters
        then (printStream " in "; printStream(regSetRepr cache)) 
        else ();
        
        if lifeTime = 0 then printStream " temp "
        else printStream(" life " ^ Int.toString lifeTime ^ " ");

        case destStack of
            NONE => ()
        |   SOME stack => (printStream " to base@("; printStream(Int.toString stack); printStream")");
        
        printStream "\n"
    )

    fun printStack (table as Ttab {printStream, realstackptr=ref realstackVal, pstackptr, marker, regset, ...},
                    why, recentInstrs) =
    ( 
        printStream ("\n" ^ why ^"\n");
        List.app(fn i => printOperation(i, printStream)) (List.rev recentInstrs);
        printStream "psp=";
        printStream(Int.toString(! pstackptr));
        printStream " lim=";
        printStream(Int.toString(!marker));
        printStream " rsp="; 
        printStream (Int.toString realstackVal);
        printStream "\n";
        printStream "regs=";
        printRegs printStream regset;
        printStream "\n";
        let
            fun pEntry i =
                if i <= 0 then ()
                else
                (
                    printEntry printStream (pstackEntry(table, i)) i;
                    pEntry (i-1)
                )
        in
            pEntry (! pstackptr)
        end;
        printStream "-\n" (* Extra line break between entries. *)
    )
  
    (* Removes empty entries from the top of the stack. *)
    fun clearOff(table as Ttab{pstackptr, marker, ...}) =
    let
        val newIndex = ! pstackptr - 1
    in
        if newIndex >= ! marker
        then
        case pstackEntry(table, newIndex) of
            NoStackEntry => 
            (
                pstackptr := newIndex;
                clearOff table 
            )
        |   StackEntry _ => ()
        else ()
    end  (* clearOff *)

    (* Reset the real stack pointer iff there are items that are no longer referenced.
       This has been added since much of the other code was written and that assumed
       that the stack wouldn't be popped so there are only a few places where this
       is safe to use. *)
    (* TODO: When there are a lot of bindings this becomes a major hotspot. *)
    fun removeOldItemsFromRealStack(
           table as Ttab{realstackptr as ref current, markReal=ref markReal, pstackptr=ref pstackptr, ...}) =
    let
        fun maxStack(i, n) =
        if i = pstackptr then n
        else case pstackEntry(table, i) of (* HOTSPOT - 510 *)
            StackEntry{ent=StackW addr, ...} => maxStack(i+1, Int.max(addr+1, n))
        |   StackEntry{ent=Register(_, addr), ...} => maxStack(i+1, Int.max(addr+1, n))
        |   _ => maxStack(i+1, n)
        val stackMax = maxStack(first, markReal)
    in
        if current > stackMax
        then (realstackptr := stackMax; resetStack (current-stackMax))
        else []
    end

    (* Removes an entry which is no longer required. If the entry is cached it
       may be retained unless it refers to the stack or another register when
       it MUST be removed. *)
    fun removeEntry (table as Ttab{regset, ...}, entry, keepIfCache): operation list =
    case pstackEntry(table, entry) of
      NoStackEntry => []
      
    | StackEntry {ent = stacken, cache, lifeTime, ...} =>
        (* If we are removing an entry from the real stack it must not be
           retained in the cache since we may push something else into that
           location. Actual parameters to procedures are not use-counted in
           the same way as locals so it is worth keeping them cached. *)
        let
            val (cacheRegs, code) =
                if cache = noRegisters
                then (noRegisters, [])
                else if not keepIfCache orelse 
                     (case stacken of
                        Register _ => true
                      | StackW i   => i > 0 (* Parameters on the stack have -ve address. *)
                      | _          => false)
                then (* Clear cache. *)
                    (noRegisters, freeSet(regset, cache))
                else (* Retain cache value. *) (cache, [])
        in
            if cacheRegs = noRegisters
            then (* If the cache is (now) empty we can remove the entry completely. *)
            let
                (* clobber the entry. *)
                val () = setPstackEntry (table, entry, NoStackEntry)
                val freeCode =
                    case stacken of
                        Register(reg, _)   => free regset reg
                    |   Direct {base, ...} => free regset base
                    |   Container{items, dependencies} =>
                        let
                            (* Release the dependencies.  The normal use count mechanism
                               doesn't remove mutually recursive closures and so they may be
                               forcibly removed at the end of a block.  Since the dependencies
                               form a loop when we come back to the start we will find an
                               entry that has already been clobbered. *)
                            fun releaseDep(i, f) =
                                case pstackEntry(table, i) of
                                    NoStackEntry => f
                                |   _ => incrUseCount(table, i, ~1) @ f

                            val releaseList = List.foldl releaseDep
                        in
                            releaseList (releaseList [] items) dependencies
                        end
                    |   _                  => []
                val () = clearOff table
            in
                code @ freeCode
            end
         
            (* otherwise we just leave the entry there. *)
            else
            (
                setPstackEntry (table, entry,
                StackEntry { ent = stacken, cache = cacheRegs, uses = 0, destStack = NONE, lifeTime=lifeTime });
                code
            )
        end (* end removeEntry *)

    (* Add the number of uses to the use count of an item on the stack. *)
    and incrUseCount (table, entry, incr) : operation list =
        case pstackEntry(table, entry) of
            NoStackEntry => raise InternalError ("incrUseCount: no entry " ^ Int.toString(entry))
      
        |   StackEntry {ent, cache, uses, destStack, lifeTime} =>
            let
                val newUses = uses + incr;
            in
                if newUses <=  0
                then removeEntry(table, entry, true)
                else
                (
                    setPstackEntry (table, entry,
                        StackEntry {ent=ent, cache=cache, uses=newUses, destStack=destStack, lifeTime=lifeTime});
                    []
                )
            end

    and setLifetime(table, entry, life) =
        case pstackEntry(table, entry) of
            NoStackEntry => raise InternalError ("setLifetime: no entry " ^ Int.toString(entry))
        |   StackEntry {ent, cache, uses, destStack, ...} =>
                setPstackEntry (table, entry,
                    StackEntry {ent=ent, cache=cache, uses=uses, destStack=destStack, lifeTime=life})
                

  (* True if this is the last reference to this entry. i.e. the use-count *)
  (* of this entry is 1. *)
  fun lastReference table entry =
    case pstackEntry(table, entry) of
      NoStackEntry => raise InternalError "lastReference: no entry"
      
    | StackEntry {uses, ...} => uses = 1


    (* Push a value on the stack and return its location. *)
    fun pushPstack (table as Ttab{pstackTrace, pstackptr, ...}, entry, name, ops) =
    let
        val stacktop = ! pstackptr;
        val psp      = stacktop;
        val destStack =
            case entry of
                StackW addr => SOME addr
            |   _ => NONE
    in
        setPstackEntry (table, psp,
            StackEntry {ent=entry, cache=noRegisters, uses=1, destStack=destStack, lifeTime=0});
        pstackptr := stacktop + 1;
        if pstackTrace then printStack(table, name, ops) else ();
        stacktop
    end


    (* Push a value onto the real stack. *)
    fun incsp(table as Ttab{maxstack, realstackptr, ...}) =
    let
        val stackaddr = ! realstackptr
    in
        realstackptr := ! realstackptr + 1;
    
        if ! realstackptr > ! maxstack
        then maxstack := ! realstackptr
        else ();
    
        pushPstack(table, StackW stackaddr, "incsp", [])
    end;

    (* The top of the pseudo-stack is held in a register *)
    fun pushRegFromContainer(table, reg, offset) =
        pushPstack(table, Register(reg, offset), "pushReg", [])
    fun pushReg (table, reg) = pushRegFromContainer(table, reg, 0)

    (* The top of the pseudo-stack is a constant *)
    fun pushConst (table, v : machineWord) = pushPstack(table, Literal v, "pushConst", [])


    (* The top of the pseudo-stack is a forward reference to a procedure. *)
    fun pushCodeRef (table, rf : code) = pushPstack(table, CodeRef rf, "pushCodeRef", [])

    fun addRegUse (Ttab{regset, ...}, reg) = incr regset reg

  (* If we load a value into the last available register and then need to
     load another value (e.g. a second argument), it is possible for the
     first to be pushed onto the stack and the register to be re-used.  To
     avoid this we increment the use count on the first register before
     we attempt to load the second value.  This doesn't prevent the register
     being pushed onto the stack but it does prevent the register being
     reused. *)
   fun lockRegister (table as Ttab{pstackTrace, ...}, reg) = 
   let
     val _ = addRegUse (table, reg);
   in
     if pstackTrace then printStack(table, "lockRegister:"^regRepr reg, []) else ()
   end;

   fun unlockRegister (table as Ttab{pstackTrace, regset, ...}, reg) : operation list =
   let
        val code = free regset reg
   in
        if pstackTrace then printStack(table, "unlockRegister:"^regRepr reg, []) else ();
        code
   end

    (* Puts a value in the real stack onto the pseudo-stack.
       Used for references to arguments that have not been passed in registers. *)
    fun pushStack (table as Ttab{pstackptr, ...}, addr : int) : stackIndex =
    let (* Enter it only if it is not already there. *)
        fun search s =
        if s >= ! pstackptr
        then pushPstack(table, StackW addr, "pushStack", [])
        
        else 
        case pstackEntry(table, s) of
            StackEntry {ent = StackW index, ...} =>
            if index = addr
            then (incrUseCount (table, s, 1); s)
            else search (s + 1)
            
       |    _ => search (s + 1)
    in
        search first
    end

    fun clearCacheEntry(table as Ttab{regset, ...}, regSet, entry): operation list =
        case pstackEntry(table, entry) of (* HOTSPOT - 198 *)
            NoStackEntry => []
      
        |   StackEntry {ent = stacken, cache, uses, destStack, lifeTime} =>
            if cache = noRegisters
            then []
            else
            let
                val keep = regSetMinus(cache, regSet)
                val remove = regSetIntersect(cache, regSet)
            in
                if uses = 0 andalso
                    (* If it has a zero use count we are keeping it only because of the cache.
                       We completely remove an entry, which frees all the registers both in the
                       cache and the entry, either if the cache is now empty or if the entry
                       itself contains a register we want. *)
                    (keep = noRegisters orelse
                    (case stacken of
                         Register(reg, _)   => inSet(reg, regSet)
                       | Direct {base,...} => inSet(base, regSet)
                       | _                 => false)
                    )
                then removeEntry(table, entry, false)
                else
                (
                    setPstackEntry (table, entry,
                        StackEntry {ent = stacken, cache=keep, uses=uses,
                                    destStack=destStack, lifeTime=lifeTime});
                    freeSet(regset, remove)
                )
            end

    (* Remove registers from the cache. *)
    fun removeFromCache (table as Ttab{pstackTrace, pstackptr, ...}, regSet, continue: unit -> bool) =
    let
        fun clear (entry: stackIndex, limit: stackIndex) =
        if entry < limit
        then clearCacheEntry(table, regSet, entry) @
                (if continue () then clear (entry + 1, limit) else [])
        else []
        val clearRegs = clear (first, ! pstackptr);
    in
        if pstackTrace then printStack(table, "removeFromCache"^regSetRepr regSet, clearRegs) else ();
        clearRegs
    end

    (* Remove everything from the cache. *)
    fun clearCache table = removeFromCache(table, allRegisters, fn () => true)

    fun removeRegistersFromCache (table, regs) = removeFromCache(table, regs, fn () => true);

    (* The value on the stack is no longer required.
     This now just decrements the use count. *)
    fun removeStackEntry (table, index) =
        incrUseCount(table, index, ~1)

    (* Reset the real stack stack pointer after a function call. *)
    fun decsp (table as Ttab{pstackTrace, realstackptr, ...}, args) = 
    (
        realstackptr := ! realstackptr - args;
        if pstackTrace then printStack(table, "decsp", []) else () 
    )

    (* Frees registers by pushing values onto the stack or moving them to
       other registers.  `selectRegister' selects which registers are
       affected, `selectEntry' selects which entries are affected. Used
       either to clear all registers or just to free a particular one.
       `loadIfPoss' is true if it is sufficient to move the entry to
       another register. *)
    fun pushRegisters (table as Ttab{pstackTrace, maxstack, realstackptr, pstackptr, regset, ...},
                       setToFree, selectEntry, loadIfPoss, dontCache) =
    let
        (*fun freeSet rset = List.app (free regset) (setToList rset)*)

        (* Sort the items by the longest lifetime first.  The idea is to push items with a
           longer time to go before those with shorter lifetimes. *)
        local
            fun enumerate(n, l) =
                if n >= ! pstackptr
                then l
                else case pstackEntry(table, n) of (* HOTSPOT - 103 *)
                    StackEntry{lifeTime, ent=Direct _, ...} => enumerate(n+1, (lifeTime, n) :: l)
                |   StackEntry{lifeTime, ent=Register _, ...} => enumerate(n+1, (lifeTime, n) :: l)
                |   _ => enumerate(n+1, l)
            (* Temporary values with a lifetime of zero should go after those with a finite
               lifetime but otherwise we put smaller values first because those represent longer
               lives. *)
            fun leq (_, _) (0, _) = true
            |   leq (0, _) (_, _) = false
            |   leq (l1, _) (l2, _) = l1 <= l2
        in
            val lifeList = quickSort leq (enumerate(first, []))
        end

        fun pushEntries([], instrList) = (* Finished *) instrList
        |   pushEntries((_, entry)::entries, instrList: operation list) =
        let
            val stackent = pstackEntry(table, entry);
            
            val pushThis: operation list =
            case stackent of
               StackEntry {uses, ent = Direct {base, offset}, cache = cacheReg,
                           destStack, lifeTime} =>
                (* Values which are cached but are otherwise not needed
                   have a zero use-count. There is no need to push them. *)
                if uses = 0 orelse not (selectEntry entry)
                then [] 
                else
                let 
                    (* Push reg onto the stack without changing the use count.*)
                    fun saveDirectOnStack () =
                    let
                        val alignInstrs = alignStack(table, [], destStack);

                        val pushInstr =
                            if regSetIntersect(cacheReg, generalRegisters) <> noRegisters
                            then pushRegisterToStack(oneOf(regSetIntersect(cacheReg, generalRegisters)))
                            else pushMemoryToStack(base, offset)
                  
                        val freeCode1 = free regset base
                        val (newCache, freeCode2) =
                            if dontCache then (noRegisters, freeSet(regset, cacheReg)) else (cacheReg, [])
                  
                        (* Overwrite this entry with a reference to the stack. *)
                        val stackAddr = ! realstackptr
                    
                        val () = 
                            setPstackEntry (table, entry,
                                StackEntry{ent=StackW stackAddr, cache=newCache, uses=uses,
                                           destStack=SOME stackAddr, lifeTime=lifeTime})
                    in  
                        realstackptr := ! realstackptr + 1;
                    
                        if ! realstackptr > !maxstack
                        then maxstack := ! realstackptr
                        else ();
                        freeCode2 @ freeCode1 @ pushInstr @ alignInstrs
                    end
              
                    fun saveDirectInReg destReg =
                    let
                        (* Free the base register. *)
                        val freeCode1 = free regset base
                        (* Free all cache registers except the destination, if it's in there. *)
                        val freeCode2 = freeSet(regset, regSetMinus(cacheReg, singleton destReg))

                        val loadInstrs =
                            if inSet(destReg, cacheReg)
                            then [] (* already cached in destination register. *)
                            else if regSetIntersect(cacheReg, generalRegisters) <> noRegisters
                            then (* Cached in a different register - move it there and free the cache. *)
                                moveRegisterToRegister(oneOf(regSetIntersect(cacheReg, generalRegisters)), destReg)
                            else (* Not in a suitable cache register. *)
                                moveMemoryToRegister(base, offset, destReg)
                    in
                        (* Clear out the cache and overwrite this entry with a
                           reference to the register. *)
                        setPstackEntry (table, entry,
                                StackEntry{ent=Register(destReg, 0), cache=noRegisters, uses=uses,
                                           destStack=destStack, lifeTime=lifeTime});
                        freeCode2 @ freeCode1 @ loadInstrs
                    end
                in
                    if not (inSet(base, setToFree)) then []
                    else if uses = 0 then (* discardDirect () *) (removeEntry(table, entry, false); [])
                    else if not loadIfPoss (* Not allowed to move it to another register. *)
                    then saveDirectOnStack ()
                    else if regSetMinus(cacheReg, setToFree) <> noRegisters
                    (* It's cached in an acceptable register i.e. one NOT in setToFree. *)
                    then saveDirectInReg(oneOf(regSetMinus(cacheReg, setToFree)))
                    else (* Is there an acceptable register free?  If so load it into that. *)
                        case getAcceptableRegister (regset, regSetMinus(generalRegisters, setToFree)) of
                            SOME destReg => saveDirectInReg destReg
                        |   NONE => saveDirectOnStack ()
                end

            | StackEntry {uses, ent = Register(reg, _), cache = cacheReg, destStack, lifeTime} =>
                if (* uses = 0 orelse *) not (selectEntry entry)
                then [] 
                else
                let 
                    (* Push reg onto the stack without changing the use count.*)
                    fun saveRegOnStack () =
                    let
                        val alignInstrs = alignStack(table, [], destStack);

                        val pushInstr = pushRegisterToStack reg

                        (* Have pushed a register - can treat the register as caching
                           the stack location we have pushed it into. *)
                        val (newCache, freeCode) =
                            if dontCache
                            then (noRegisters, freeSet(regset, regSetUnion(cacheReg, singleton reg)))
                            else (regSetUnion(cacheReg, singleton reg), [])
                        val stackAddr = ! realstackptr
                    in
                        (* Overwrite this entry with a reference to the stack. *)
                        setPstackEntry (table, entry,
                            StackEntry {ent=StackW stackAddr, cache=newCache, uses=uses,
                                        destStack=SOME stackAddr, lifeTime=lifeTime});

                        realstackptr := ! realstackptr + 1;
                    
                        if ! realstackptr > ! maxstack
                        then maxstack := ! realstackptr
                        else ();

                        freeCode @ pushInstr @ alignInstrs
                    end
              
                    (* If we have any direct references using this register we can adjust them to
                       use the new register. This is particularly important if we are moving values
                       out of this register because we want to load it with something else. *)
                    fun saveRegInReg destReg =
                    let
                        fun regChanged entry =
                        if entry < ! pstackptr
                        then
                        let
                            val freeCode =
                                case pstackEntry(table, entry) of
                                    StackEntry {ent = Direct {base, offset}, cache, uses, destStack, lifeTime} =>
                                        if base = reg
                                        then
                                        let(* Decrement the use count for the source reg 
                                             and increment it for the destination. *)
                                            val freeCode = free regset reg;
                                            val _ = addRegUse (table, destReg);
                                        in
                                            setPstackEntry (table, entry,
                                                StackEntry {ent = Direct {base = destReg, offset = offset},
                                                    cache=cache, uses=uses, destStack=destStack, lifeTime=lifeTime});
                                            freeCode
                                        end
                                        else []
                        
                                |   _ => []
                        in
                            regChanged (entry + 1) @ freeCode
                        end
                        else []

                        val moveInstrs =
                            if inSet(destReg, cacheReg)
                            then [] (* already cached in destination register *)
                            else moveRegisterToRegister(reg, destReg)
                        (* Free this register *)
                        val freeCode1 = free regset reg
                        (* Free all cache registers except the destination, if it's in there. *)
                        val freeCode2 = freeSet(regset, regSetMinus(cacheReg, singleton destReg))
                        val freeCode3 = regChanged entry (* Start from this entry not from the bottom *)
                    in

                        (* Clear out the cache and overwrite this entry with a
                           reference to the register. *)
                        setPstackEntry (table, entry,
                            StackEntry {ent = Register(destReg, 0), cache=noRegisters, uses=uses,
                                        destStack=destStack, lifeTime=lifeTime});
                        freeCode3 @ freeCode2 @ freeCode1 @ moveInstrs
                    end
                in
                    if not (inSet(reg, setToFree)) then []
                    else if uses = 0
                    then (* discardReg () *) (removeEntry(table, entry, false); [])
                    else if not loadIfPoss then saveRegOnStack ()
                    else if regSetMinus(cacheReg, setToFree) <> noRegisters
                        (* It's cached in a register that we don't need - save it there. *)
                    then saveRegInReg(oneOf(regSetMinus(cacheReg, setToFree)))
                    else
                    let
                        val prefSet =
                            if inSet(reg, floatingPtRegisters)
                            then floatingPtRegisters else generalRegisters
                    in
                        case getAcceptableRegister (regset, regSetMinus(prefSet, setToFree)) of
                                SOME destReg => saveRegInReg destReg
                            |   NONE =>saveRegOnStack ()
                    end
                end (* let for saveReg etc. *)
            
            | _ => [] (* neither Direct nor Register *) 

        in
            pushEntries (entries, pushThis @ instrList)
        end

        val code = pushEntries(lifeList, [])
    in
        if pstackTrace then printStack(table, "pushRegisters"^regSetRepr setToFree, code) else ();
        code
    end

    and pushAnyEntryAtCurrentSP(table as Ttab{realstackptr, ...}): (bool * operation list) =
    (* Check that the next stack location is not the destination of an entry
       which has not yet been pushed and pushes it if it is. *)
    let
        val currentSp = ! realstackptr
        fun selectEntry addr =
            case pstackEntry(table, addr) of
                NoStackEntry => raise InternalError "pushAnyEntryAtCurrentSP: no entry"
             
            |   StackEntry {ent=StackW addr, ...} =>
                (* Ok if already pushed.  Check that we don't have an entry above the stack pointer. *)
                if addr > currentSp
                then raise InternalError "pushAnyEntryAtCurrentSP: entry above rsp"
                else false

         |      StackEntry {destStack=NONE, ...} => false

         |      StackEntry {destStack=SOME destStack, ...} =>
                (* Consistency check to make sure that we haven't got an unpushed
                   entry below the current sp. *)
                if destStack < currentSp
                then raise InternalError "pushAnyEntryAtCurrentSP: unpushed entry"
                else destStack = currentSp (* Push it if we're there. *)
        val pushInstrs =
            pushRegisters(table, allRegisters (* Any register *), selectEntry, false, false)
    in
        (* Return true if the stack pointer has changed. *)
        (! realstackptr <> currentSp, pushInstrs)
    end

    and alignStack (table as Ttab{realstackptr, pstackptr, ...}, previous, NONE) = (* Can use any offset. *)
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
    let
        fun minReserved s i =
            case pstackEntry(table, s) of (* HOTSPOT - 117 *)
                StackEntry {destStack=NONE, ...} => i
            |   StackEntry {destStack=SOME destStack, ...} => Int.max(destStack+1, i)
            |   _ => i
        val newAddr =
            revfoldIndDownTo(minReserved, ! realstackptr, ! pstackptr - 1, first)
    in
        alignStack (table, previous, SOME newAddr)
    end
    | alignStack (table as Ttab{realstackptr, ...}, previous, destAddr as SOME addr) = (* Can use any offset. *)
    (* We have an explicit offset *)
    (
        if addr < ! realstackptr
        then raise InternalError "pushRegisters: unpushed register"
        else ();

        if addr = ! realstackptr
        then previous (* Got there. *)
        else
        let
            val (pushIt, pushInstrs) = pushAnyEntryAtCurrentSP table
            val alignInstrs =
                (* If there is another entry for this address push it. *)
                if pushIt then pushInstrs @ previous
                else (* Push any register simply to align the stack. *)
                (
                    realstackptr := ! realstackptr + 1;
                    pushToReserveSpace @ pushInstrs @ previous
                )
        in
            alignStack(table, alignInstrs, destAddr) (* Keep going. *)
        end
    )

    (* Push a specific entry.  This should really be incorporated into
       pushRegisters since at the moment it processes all the entries
       and only selects the particular one.  *)
    fun pushSpecificEntry (table, entry) =
        pushRegisters(table, allRegisters, fn e => e = entry, false, false)

    (* Used particularly before procedure calls where we want to ensure
     that anything in a register is pushed onto the stack unless its
     last reference is in the call itself.  Also used before a handler. *)
    fun pushAllBut (table as Ttab{pstackptr, ...}, but, pushTheseRegs) =
    let
        val useTab = Array.array(!pstackptr, 0)
    in
        but
         (fn addr => 
          let
                val ind = addr
          in
                Array.update (useTab, ind, Array.sub (useTab, ind) + 1)
          end);

        pushRegisters(table,
            (* registers that are modified *)
            pushTheseRegs,
            (* entries with more uses than this *)
            fn addr =>
                case pstackEntry(table, addr) of
                    NoStackEntry => 
                        raise InternalError "pushAllBut: no entry"
             
                |   StackEntry {uses, destStack, ent, cache, lifeTime} =>
                        if uses > Array.sub (useTab, addr)
                        then true
                        else
                        (
                            (* Set the destination stack for this entry to "undefined".
                               That's safe because we're going to remove this entry.
                               We do this because we may be about to push some arguments
                               or exception handlers and destStack may be in that area. *)
                            case destStack of
                                NONE => ()
                            |   SOME _ =>
                                    setPstackEntry (table, addr,
                                        StackEntry {ent=ent, cache=cache, uses=uses, destStack=NONE, lifeTime=lifeTime});
                            false
                        ),
            false, false)
  end;

  (* Ensures that all values which need to be preserved across a function
     call are pushed onto the stack or are in registers that will not
     be modified. *)
    fun pushNonArguments (table as Ttab{pstackptr, ...}, args, pushTheseRegs) : (reg list * operation list) =
    let

        fun checkAddress [] _ = false
          | checkAddress (h::t) addr = h = addr orelse checkAddress t addr
        val onList = checkAddress args

        (* Get the list of registers which weren't pushed.  We need to lock
           them so that they don't get pushed onto the stack while we are
           pushing the arguments.  Actually I'm not sure this achieves what
           we want. *)
        fun getRegisterList entry regs =
        if entry < ! pstackptr
        then if onList entry (* Is it an argument? *)
        then (* Ignore this. *) getRegisterList (entry + 1) regs
        else
        let
            val stackent = pstackEntry(table, entry) (* HOTSPOT - 90 *)
            val nextRegs = 
                case stackent of
                   StackEntry {uses, ent = Direct {base, ...}, ...} =>
                       if uses = 0 then regs
                       else (lockRegister(table, base); base::regs)
                |  StackEntry {uses, ent = Register(reg, _), ...} =>
                       if uses = 0 then regs
                       else (lockRegister(table, reg); reg::regs)
                | _ => (* neither Direct nor Register *) regs
        in
            getRegisterList (entry + 1) nextRegs
        end
        else regs

        val instrs =
            pushRegisters(table,
                (* registers that are modified *)
                pushTheseRegs,
                (* Ignore entries corresponding to the arguments but only if they
                     have a use count of exactly one, *)
                fn addr =>
                    case pstackEntry(table, addr) of
                        NoStackEntry => raise InternalError "pushNonArguments: no entry"
         
                    |   StackEntry {uses, destStack, ent, cache, lifeTime} =>
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
                                case destStack of NONE => ()
                                |   SOME _ =>
                                        setPstackEntry (table, addr,
                                            StackEntry {ent=ent, cache=cache, uses=uses,
                                                        destStack=NONE, lifeTime=lifeTime });
                                false
                            ),
                (* If all the registers must be pushed there's no point in trying to
                 move to another register. *)
                not (isAllRegs pushTheseRegs), false)
    in
        (getRegisterList first [], instrs)
    end

    type savedState = { pStackPtr: stackIndex, realStackPtr: int, pStack: stackEntry Vector.vector }
  
    fun pStackPtr    ({pStackPtr   ,...}:savedState) = pStackPtr;
    fun realStackPtr ({realStackPtr,...}:savedState) = realStackPtr;
    fun pStack       ({pStack      ,...}:savedState) = pStack;
 
    fun saveStateEntry ({pStack, ...}: savedState, locn) = 
        if locn >= Vector.length pStack then NoStackEntry
        else Vector.sub(pStack, locn)
  
  fun printState printStream (save: savedState as {pStackPtr, realStackPtr, ... }) name =
  ( printStream name;
    printStream "\n";
    printStream " psp=";
    printStream(Int.toString(pStackPtr));
    printStream " rsp=";
    printStream(Int.toString realStackPtr);
    printStream "\n";
    forIndDownTo(pStackPtr, first, fn i => printEntry printStream (saveStateEntry(save, i)) i)
  );

    fun pStackRealEntry (table:savedState, locn:stackIndex) =
    let
        val pstack = pStack table
    in
      case (Vector.sub(pstack, locn)) of
        NoStackEntry =>
          raise InternalError "pStackRealEntry: no entry"
          
      | StackEntry record => (locn,record)
    end


  (* Sets the pseudo stack into a state to which it can be restored later.
     It is used when there are conditional branches to ensure that the state
     is the same if the branch falls through or is taken. *)
    fun saveState (table as Ttab{pstackTrace, printStream, realstackptr, pstackptr, ...}) : savedState =
    let
        val maxIndex  = ! pstackptr
    
        val state =
            { 
              pStackPtr    = ! pstackptr, 
              realStackPtr = ! realstackptr,
              pStack       = Vector.tabulate(maxIndex, fn n => pstackEntry(table, n))
            }
    in
        if pstackTrace then printState printStream state "saveState" else ();
        state
    end


    (* Extract one of the acceptable registers. *)
    fun getRegisterInSet(table as Ttab{pstackTrace, regset, ...}, rSet) : reg * operation list =
    let
        fun getReg() = getAcceptableRegister(regset, rSet)
        (* First see if there is one free and grab that. *)
        val rAndCode =
            case getReg () of
                SOME r => (r, [])
            |   NONE =>
                let
                    (* We seem to have run out.  First flush the cache, then if that
                       fails push a register. *) 
                    fun untilSomethingFree () =
                        case getReg () of NONE => true | SOME r => let val _ = free regset r in false end
                    val clearAll = removeFromCache(table, rSet, untilSomethingFree)
                in
                    case getReg () of
                        SOME r => (r, clearAll)
                    |   NONE =>
                        let
                            val pushInstrs =
                                pushRegisters(table, rSet,
                                    fn _ => untilSomethingFree(), true (* Allow moves *),
                                    true (* Free it once it's been pushed *))
                            (* Pushed values stay in the cache. *)
                            val clearPush = removeFromCache(table, rSet, untilSomethingFree)
                        in (* If we still haven't found anything we are in big trouble. *)
                            case getReg () of
                                SOME r => (r, clearPush @ pushInstrs @ clearAll)
                            |   NONE => raise InternalError("No free registers: " ^ regSetRepr rSet)
                        end
                end
    in 
        if pstackTrace
        then printStack(table, "getRegisterInSet", #2 rAndCode)
        else ();
        rAndCode
    end

    fun getRegister(table, reg) = #2(getRegisterInSet(table, singleton reg))
    and getAnyRegister table = getRegisterInSet(table, generalRegisters)

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
    fun resetButReload (
            table as Ttab{pstackTrace, realstackptr, pstackptr, regset, ...}, stackOffset) : operation list =
    let
        val oldSp = ! realstackptr
    
        (* Load any values above "stackOffset". *)
        fun loadEntries(entry, otherCode): operation list =
            if entry < first
            then otherCode
            else
            let
                val stackent = pstackEntry(table, entry);
                val thisCode =
                    case stackent of
                        StackEntry {ent = StackW addr, cache, uses, lifeTime, ...} =>
                        if addr >= stackOffset (* Above the limit on the stack. *)
                        then
                        let
                            (* Load it without changing the use count. *)
                            val (reg, regCode) =
                                if cache <> noRegisters
                                then (oneOf cache, [])
                                else
                                    let
                                        val (reg, code) = getAnyRegister table
                                        val stackOffset = (addr - ! realstackptr + 1) * ~wordSize
                                        val fullCode =
                                            moveMemoryToRegister(regStackPtr, stackOffset, reg) @ code
                                    in
                                        (reg, fullCode)
                                    end
                            (* Free all cache registers except the destination, if it's in there. *)
                            val freeCode = freeSet(regset, regSetMinus(cache, singleton reg))
                        in (* Clear out the cache and overwrite this entry with a reference to the register. *)
                            setPstackEntry (table, entry,
                                StackEntry {ent = Register(reg, 0), cache=noRegisters, uses=uses,
                                            destStack=NONE, lifeTime=lifeTime});
                            freeCode @ regCode
                        end
                        else []
            
                    |   _ => []
            in
                loadEntries (entry - 1, thisCode @ otherCode)
            end (* loadEntries *);

        val loadCode = loadEntries ((! pstackptr) - 1, []);
    
         (* If the real stack ptr has changed we must have pushed something,
            so our check has been useless. *)
        val () =
            if ! realstackptr <> oldSp
            then raise InternalError "resetButReload: must have pushed something more"
            else ();

        val finalCode = resetStack (! realstackptr - stackOffset) @ loadCode
    in
        (* Now reset the stack pointer. *)
        realstackptr := stackOffset;
        if pstackTrace then printStack(table, "resetButReload", finalCode) else ();
        finalCode
    end;

    fun freeRegister (Ttab{regset, ...}, reg) = free regset reg

    (* Choose a register to be used to hold the result e.g. of a "case". *)
    fun chooseRegister (Ttab{regset, ...}) : reg option =
    let
        fun chooseReg ({vec, ...}:rset) =
        let
            val nextReg = 0
      
            fun next n = if n = 0 then (vecSize - 1) else (n - 1);
      
            fun findFree (i : int) : reg option =
            if Array.sub (vec, i) = 0 andalso inSet(regN i, generalRegisters)
            then SOME(regN i)
            else
            let
                val n = next i;
            in
                if n = nextReg then (* None free. *) NONE
                else findFree n
            end
        in
            findFree nextReg
        end
    in
        chooseReg regset
    end

    (* Return the set of modified registers for this function. *)
    fun getModifedRegSet (Ttab{regset={modSet=ref modSet, ...}, ...}) = modSet

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
    fun loadPstackEntry (table as Ttab{pstackTrace, realstackptr, regset, ...}, locn (* Offset on the stack *), destReg) =
    let
        val {cache = cacheReg, ent, ...} = pstackRealEntry(table, locn)
        
        fun moveFromMemory(base, offset, destReg) =
            if inSet(destReg, floatingPtRegisters)
            then
                (* We can't load directly to the FP regs - put first into a general reg.
                   This is X86-specific and really should be in a negotiator but since
                   we only have FP regs on the X86 leave it. *)
            let
                val (aReg, regCode) = getAnyRegister table
            in
                free regset aReg @ moveRegisterToRegister(aReg, destReg) @
                    moveMemoryToRegister(base, offset, aReg) @ regCode
            end
            else moveMemoryToRegister(base, offset, destReg)
 
        val loadCode =
            if inSet(destReg, cacheReg)
            then []
            else if cacheReg <> noRegisters
            then moveRegisterToRegister(oneOf cacheReg, destReg)
            else
            case ent of
               Register(reg, _) =>
                    if reg <> destReg
                    then moveRegisterToRegister(reg, destReg)
                    else []
            |   Literal lit => moveConstantToRegister(lit, destReg)
            |   CodeRef code => moveCodeRefToRegister(code, destReg)
            |   Direct {base, offset} => moveFromMemory(base, offset, destReg)
            |   StackW index =>
                    moveFromMemory(regStackPtr, (! realstackptr - index - 1) * wordSize, destReg)
            |   Container{items, ...} => (* The first entry in the container gives us the address. *)
                case pstackRealEntry(table, hd items) of
                    {ent = StackW index, ...} => moveStackAddress(! realstackptr - index -1, destReg)
                |   _ => raise InternalError "loadPstackEntry: container entry is not on stack";
       (* Decrement use count and remove if done. *)
       val codeAndRelease = incrUseCount (table, locn, ~1) @ loadCode
    in
       if pstackTrace then printStack(table, "loadPstackEntry", codeAndRelease) else ();
       codeAndRelease
    end (* loadPstackEntry *)

    (* Pushes a new pstack entry; loads value into register;
       decrements the use count of old pstack entry. *)
    fun loadEntryToSet (table as Ttab{regset, ...}, entry, rSet, willTrample) =
    let
        val realLoc = entry
        val {ent = stackEntry, cache = cacheReg, uses, destStack, lifeTime} =
                pstackRealEntry(table, entry)
      
        (* If we find an entry in the cache or already in a register we can use
           it provided it will not be modified or this is its last use. Otherwise
           we must make a copy of it. *)
        val lastRef = lastReference table entry

        val acceptable =
            case stackEntry of
                Register(reg, _) =>
                    if inSet(reg, rSet)
                    then if not willTrample (* It's acceptable. *)
                    then SOME (reg, entry)

                    else if lastRef
                    then
                    (
                        (* We are going to trample on it but this is the last reference
                           so we can use it. It may, though, be caching a value so
                           we must remove it from the cache before we return it. *)
                        removeRegistersFromCache(table, listToSet [reg]);
                        SOME (reg, entry)
                    )

                    else NONE (* Must copy it. *)
                            (* TODO: Won't this result in double copying?  Once to free this and again to move the value
                               into the now free register. *)
                    else NONE (* Not in the right register. *)
        
            |   _ => (* May be cached. *)
                if regSetIntersect(cacheReg, rSet) <> noRegisters
                then (* There's an acceptable register in the cache. *)
                let
                    val destReg = oneOf(regSetIntersect(cacheReg, rSet))
                    (* Get the register, increment its use count and put it on the stack *)
                    (* If we are going to trample on the register we must remove it
                       from the cache. If this is the last real reference that will
                       not matter, but if this is actually a reference to a parameter
                       which could be loaded onto the stack again we have to be careful
                       that the cache does not indicate a register which has been changed. *)
                    val () = 
                        if willTrample
                        then setPstackEntry (table, realLoc,
                                StackEntry{ent=stackEntry, cache=regSetMinus(cacheReg, singleton destReg),
                                           uses=uses, destStack=destStack, lifeTime=lifeTime})
                        else (addRegUse (table, destReg); ())
                    val newEntry = pushReg (table, destReg)
                in
                    (* Must decrement the use-count of the entry we are loading as though
                       we had actually loaded it. *)
                    incrUseCount (table, entry, ~1);
                    SOME (destReg, newEntry)
                end
                else NONE 
    in
        case acceptable of
            SOME (reg, entry) => (reg, entry, []) (* Can use what we have. *)
        |   NONE =>
            let
                (* It is loaded into a register. This is complicated because we want
                 to put entries into the cache if we can. They must not be put into
                 the cache until after they have been loaded otherwise the load
                 instruction will simply copy the new cache value. It is possible
                 that a value might be cached in a data register when it is needed
                 in an address register or vice-versa. *)
                val (resultReg, regCode) =
                    (* If we have a Direct entry which currently uses the register we want to load into
                       as the base register the default code will load this into a different register
                       in order to free the base register and then move this into the destination.
                       We can avoid that if this is the last reference to the base register and the
                       Direct entry.  This is a very common case. *)
                    case stackEntry of
                        Direct{base, ...} =>
                            if cacheReg = noRegisters andalso inSet(base, rSet) andalso lastRef andalso
                                    lastRegRef(regset, base)
                            then (base, incr regset base)
                            else getRegisterInSet(table, rSet)
                        |   _ => getRegisterInSet(table, rSet)(* N.B. May side-effect table. *)
            in
                (* Get the entry again - getAnyRegister could have forced the
                   entry onto the stack if it had run out of registers. *)
                case pstackEntry(table, realLoc) of
                    NoStackEntry => raise InternalError "loadEntry: entry deleted"

                |   StackEntry {ent, uses, cache, destStack, lifeTime} =>
                    let
                        (* If the value is already cached, keep it in the old
                           cache register, rather than the new one. This should
                           help to minimise register-register moves when we have
                           to merge branches. *)
                        val cacheIt =
                            not willTrample andalso
                               case ent of
                                  Direct{base, ...} => base <> resultReg
                               (* Cannot cache it if we are about to pop it. *)
                               | StackW index  => (0 >= index orelse not lastRef)
                               | _             => false
                        (* If we are going to cache it we musn't let it be removed. *)
                        val freeCode1 = if cacheIt then incrUseCount (table, entry, 1) else []
                        (* If we're loading the address of a container we need to ensure that it
                           stays on the stack as long as the register is in use. This is only a
                           problem if we're calling a function which returns its result via a
                           container but in this particular case its result is discarded.  In that
                           case loading its address in order to pass it into the function will be
                           the last reference so the container entry will be removed from the
                           pstack and removeOldItemsFromRealStack could remove the container from
                           the real stack before the function is called. *)
                        val stackOffset =
                            case ent of
                                Container{items, ...} =>
                                (
                                    case pstackRealEntry(table, hd items) of
                                        {ent = StackW index, ...} => index
                                    |   _ => raise InternalError "loadPstackEntry: container entry is not on stack"
                                )
                            |   _ => 0
                        val loadEntry = loadPstackEntry(table, entry, resultReg)
                        val newEntry = pushRegFromContainer(table, resultReg, stackOffset)
                        val freeCode2 =
                            if cacheIt
                            then
                            (
                                (* put in the cache and restore use-count. *)
                                setPstackEntry (table, realLoc,
                                    StackEntry {ent=ent, cache=regSetUnion(cache, singleton resultReg),
                                                uses=uses, destStack=destStack, lifeTime=lifeTime});
                                addRegUse (table, resultReg);
                                incrUseCount (table, entry, ~1)
                            ) 
                            else []
                        (* If we call removeOldItemsFromRealStack now we may be able to
                           change a load into a pop. *)
                        val removeCode = removeOldItemsFromRealStack table
                    in
                        (resultReg, newEntry, removeCode @ freeCode2 @ loadEntry @ freeCode1 @ regCode)
                    end
             end (* useNewRegister *)
    end

    fun loadToSpecificReg (table, reg, entry, needExclusive) =
    let
        val (_, regIndex, ops) = loadEntryToSet(table, entry, singleton reg, needExclusive)
    in
        (regIndex, ops)
    end

    (* Make sure that the entry will not require an allocation in order to put it into
       memory.  This really means that the value must not be in a floating point register.
       This is used when evaluating expressions that will be stored into a newly allocated
       piece of memory.  A value in a FP register first has to be stored into memory
       and this has to be done first. *)
    fun ensureNoAllocation(table, entry) =
        case pstackRealEntry(table, entry) of
            { ent = Register(reg, _), ... } =>
                if inSet(reg, floatingPtRegisters)
                then
                let
                    val (_, regIndex, ops) =
                        loadEntryToSet(table, entry, generalRegisters, false)
                in
                    (regIndex, ops)
                end
                else (entry, []) (* Safe *)
        |   _ => (entry, []) (* Safe *)

    (* Checks if we are going to overwrite the stack, and loads the entry
       into a register. *)
    fun loadEntryBeforeOverwriting (table as Ttab{realstackptr, pstackptr, regset, ...}, offset:int) =
    if 0 <= offset andalso offset < ! realstackptr
    then
    let (* May have to reload something. *)
        fun findTheEntry (entry: stackIndex, previousCode) =
        if entry < first then previousCode (* finish *)
        else
        let
            val stackent = pstackEntry(table, entry)
            val thisCode =
                case stackent of
                    StackEntry {ent = StackW addr, cache, uses, lifeTime, ...} =>
                        if addr = offset
                        then
                        let (* This is the entry. *)
                            (* Load it without changing the use count. *)
                            val (reg, code) =
                                if cache <> noRegisters
                                then (oneOf cache, [])
                                else
                                    let
                                        val (reg, regCode) = getAnyRegister table
                                        val off = (! realstackptr - 1 - addr) * wordSize;
                                    in
                                        (reg, moveMemoryToRegister(regStackPtr, off, reg) @ regCode)
                                    end;
                  
                            (* Free all cache registers except the destination, if it's in there. *)
                            val freeCode = freeSet(regset, regSetMinus(cache, singleton reg))
                            val newStackent =
                                (* Make a new entry with a NEW stack destination.
                                   If we have to push it we have to use a new location.
                                   I don't like this but it's safe because this only occurs
                                   for a tail-recursive value or for a temporary value
                                   in an exception handler.
                                   It does have implications when setting the state which is
                                   why we no longer check that destinations match in "setState". *)
                                StackEntry {ent = Register(reg, 0), cache=noRegisters, uses=uses,
                                            destStack=NONE, lifeTime=lifeTime}
                        in
                            (* Clear out the cache and overwrite this entry with a
                               reference to the register. *)
                            setPstackEntry (table, entry, newStackent);
                            freeCode @ code
                        end
                        else [] (* not this entry *)
               
                |   _ => []
        in
            findTheEntry (entry - 1, thisCode @ previousCode)
        end (*  findTheEntry *) 
    in
        findTheEntry ((! pstackptr) - 1, [])
    end
    else [] (* end of loadEntryBeforeOverwriting *)
      
    (* Store a pseudo-stack entry at a given location on the real stack.  Used
       when making a tail-recursive call.  The problem is that the old entry
       in the real stack may be in use, so we may have to reload it first.
       We load all the values before storing any, so there is no danger of
       overwriting entries in the argument area, but we may have had to push
       some of the registers while doing the load, so those entries will have
       to be saved. *)
    fun storeInStack (table as Ttab{maxstack, realstackptr, markReal as ref initMark, ...}, entry, locn) =
    let
        (* Make sure we don't reset the stack below this point. *)
        val () = markReal := Int.max(locn, initMark)

        (* Move it to the stack, using a move-immediate if possible. *)
        fun inc x = (x := !x + 1);

        fun generalStoreInStack () = (* General case. *)
        let
            val (reg, regEntry, code) = loadEntryToSet(table, entry, generalRegisters, false)
            (* Lock the register, otherwise it might be used to load an entry. *)
            val () = lockRegister (table, reg)
      
            val loadCode = loadEntryBeforeOverwriting(table, locn)
      
            (* N.B. loadEntry may push values onto the stack,
               so we cannot use isPush. *)
            val pushCode =
                if ! realstackptr = locn
                then
                (
                    inc realstackptr;
                    pushRegisterToStack reg
                )
                else
                let
                    val loc = (! realstackptr - locn - 1) * wordSize
                in
                    storeRegisterToStack(reg, loc)
                end
        in
            unlockRegister (table, reg);
            removeStackEntry(table, regEntry);
            pushCode @ loadCode @ code
        end;

        val isPush = ! realstackptr = locn

        val {ent = valEnt, cache = cacheReg, ...} = pstackRealEntry(table, entry);

        (* Select the best instruction to use. The default is to load it
           into a register and store or push that. *)
        val code =
        case valEnt of
            Literal lit =>
                if isPush andalso isPushI lit
                then
                let
                    val loadCode = loadEntryBeforeOverwriting(table, locn)
                in
                    (* Push-immediate. *)
                    incrUseCount (table, entry, ~1);
                    inc realstackptr;
                    pushConstantToStack lit @ loadCode
                end
       
                else if false (* isStoreI(lit, false) *) (* TEMPORARILY *)
                then
                let (* Store immediate. *)
                    val loadCode = loadEntryBeforeOverwriting(table, locn)
                    val locn = (! realstackptr - locn - 1) * wordSize
                in
                    (* Remove the entry for the literal. *)
                    incrUseCount (table, entry, ~1);
                    storeConstantToStack(lit, locn) @ loadCode
                end

                else generalStoreInStack () 
  
        |   Direct {base, offset} =>
                if isPush andalso cacheReg = noRegisters
                then
                let (* Push memory. *)
                    val loadCode = loadEntryBeforeOverwriting(table, locn)
                in
                    incrUseCount (table, entry, ~1);
                    inc realstackptr;
                    pushMemoryToStack(base, offset) @ loadCode
                end
                else generalStoreInStack ()


        |   StackW index =>
                if isPush andalso cacheReg = noRegisters
                then
                let (* Push stack entry. *)
                    val loadCode = loadEntryBeforeOverwriting(table, locn)
                    val locn     = (! realstackptr - 1 - index) * wordSize;
                in
                    incrUseCount (table, entry, ~1);
                    inc realstackptr;
                    pushMemoryToStack(regStackPtr, locn) @ loadCode
                end
          
                else generalStoreInStack ()

        |   _ => generalStoreInStack ();

        val stackEntry = pushPstack(table, StackW locn, "pushValueToStack", [])
    in 
        if ! realstackptr > ! maxstack
        then maxstack := ! realstackptr
        else ();
        markReal := initMark;
        (stackEntry, code)
    end (* storeInStack *);

    fun genError() = raise InternalError "pushValueToStack: Couldn't push to stack"

    (* Ensures that the top of the pseudo stack has been copied onto the
     real stack and is at the correct position. stackOffset contains the
     stack offset it should have. Primarily used to push arguments to
     procedures. *)
    fun pushValueToStack (table as Ttab{realstackptr, ...}, entry, stackOffset) : stackIndex * operation list =
    let 
        val stackAddr = stackOffset - 1
        val (storeLocn, storeCode) = storeInStack (table, entry, stackAddr)
  
        val clearCode = (* Remove any entries above the stack offset we need. *)
            if ! realstackptr > stackOffset
            then resetButReload (table, stackOffset)
            else []
    in
        (* The stack pointer should now be the required value. *)
        if ! realstackptr <> stackOffset
        then genError()
        else ();
        (storeLocn, clearCode @ storeCode)
    end;

    fun reserveStackSpace(table: ttab, space: int): stackIndex * operation list =
    (* Reserve space on the stack for a tuple. *)
    let
        (* We must first make sure that the space we're going to allocate
           hasn't been reserved for a register. *)
        val alignCode = alignStack(table, [], NONE)
        (* Initialise the store so that the garbage collector doesn't
           accidentally pick up an invalid pointer. *)
        (* The stack grows downwards so we want the entries in reverse order.
           The first entry must be lowest address. *)
        fun pushEntries(0, code) = ([], code)
        |   pushEntries(n, code) =
            let
                val (pushRest, pushCode) = pushEntries (n-1, code)
                val stackLocn = incsp table
            in
                (* Reserve space on the stack. *)
                (stackLocn :: pushRest, pushToReserveSpace @ pushCode)
            end;
        val (entries, code) = pushEntries(space, alignCode)
    in
        (pushPstack(table, Container{items=entries, dependencies=[]}, "reserveStackSpace", code), code)
    end


    (* Generates an indirection on an item on the pseudo-stack. *)
    fun indirect (wordOffset, entry, table as Ttab{pstackptr, lowestDirect, ...}) : stackIndex * operation list =
        case pstackRealEntry(table, entry) of
            {ent = Container {items, ...}, ...} =>
                (* If we are indirecting off a container we can simply load the entry. *)
                let
                    val resIndex = List.nth(items, wordOffset)
                in
                    (* Increment its use count. *)
                    incrUseCount (table, resIndex, 1);
                    removeStackEntry(table, entry); (* Remove the container entry. *)
                    (resIndex, [])
                end

        |   {ent = Literal i, ...} =>
            (
                removeStackEntry(table, entry); (* Remove the container entry. *)
                (* We won't normally get this because it will have been optimised out.
                   The exception is when we have SetContainer with a tuple which is a constant. 
                   For safety we check that we have a valid address here although
                   unlike in findEntryInBlock we should never actually get an invalid one. *)
                (* Actually, we can, in cases such as val (a,b) = raise ...  where we will
                   do an indirection on the dummyValue put on the pstack to represent the
                   non-existent result of the "raise".  In that case we put in a dummy result
                   of zero. *)
                if isShort i andalso toShort i = 0w0
                then (pushConst(table, toMachineWord 0), [])
                else if isShort i orelse Address.length (toAddress i) <= Word.fromInt wordOffset
                then raise InternalError "indirect - invalid constant address"
                else (pushConst (table, loadWord (toAddress i, Word.fromInt wordOffset)), [])
            )

      |  _ =>
            let
                val (topReg, topEntry, code) = loadEntryToSet(table, entry, generalRegisters, false)
                val _ = removeStackEntry(table, topEntry) (* Remove the entry for the register. *)
                (* and push the indirection *)
                (* Profiling shows that this search is where the compiler can spend most
                   of its time. To speed it up we keep a lower limit pointer which saves
                   us searching below the lowest direct entry. *)
                (* See if it is already on the stack. *)
                fun search(s, max, foundD) =
                if s >= max
                then
                ( 
                    (* Not there. *)
                    addRegUse (table, topReg);
                    (* If this is below the previous lower limit we need to reset it. *)
                    if ! lowestDirect > ! pstackptr
                    then lowestDirect := ! pstackptr
                    else ();
            
                    pushPstack(table, Direct {base = topReg, offset = wordOffset * wordSize}, "indirect", code)
                )
          
                else case pstackEntry(table, s) of
                    StackEntry {ent = Direct {base, offset}, ...} =>
                    (
                        (* If we found no direct entries below here 
                         then remember this as the first. *)
                        if not foundD then lowestDirect := s else ();
              
                        if base = topReg andalso offset = wordOffset * wordSize
                        then (* Found it *)
                        ( 
                            incrUseCount (table, s, 1);
                            s
                        )
                        else search(s + 1, max, true (* Found one *))
                    )
     
                |   _ => search (s + 1, max, foundD); (* end search *)
            in
                (search (! lowestDirect, ! pstackptr, false), code)
            end
         
    (* Loads a value into a register if it is in the argument area.  Used
     for tail-recursive calls. "storeInStack" checks for overwriting
     entries elsewhere on the stack, but because the argument area is not
     represented by entries on the pstack it won't work for them. *)
    fun loadIfArg (table, entry) : stackIndex * operation list =
        case pstackRealEntry(table, entry) of
            {ent = StackW index, ...} =>
            if index < 0
            then
            let
                val (_, newEntry, loadCode) = loadEntryToSet(table, entry, generalRegisters, false)
            in
                (newEntry, loadCode)
            end
            else (entry, [])
         
        |   _ => (entry, [])  (* return the original. *)

    (* Get the register set for a function that has already been compiled or for an RTS function. *)
    fun getRegisterSetForFunction addr =
    let
        val doCall: int*machineWord -> Word.word
            = RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific

        val rSet = doCall(103, addr) (* Get the bit pattern from the function. *)
    in
        getRegisterSet rSet
    end

    fun getRegisterSetForCode (cvec: code) : regSet =
    (* Get the register set for a forward reference which may or may not
     have already been compiled. *)
    case codeAddress cvec of
        SOME addr => (* Now compiled - return the register set. *)
            getRegisterSetForFunction (toMachineWord addr)
    |   NONE =>
         (* We haven't compiled this yet: assume worst case. *) allRegisters

    (* Get the register set for an entry on the stack which will be the entry
       point of a function.  If it's not a constant we have to assume it
       modifies any of the registers. *)
    fun getFunctionRegSet(index: stackIndex, transtab: ttab) : regSet =
    let
        val {ent = stacken, ...} = pstackRealEntry(transtab, index)
    in    
        case stacken of
            Literal lit => getRegisterSetForFunction lit
        |   CodeRef code => getRegisterSetForCode code
        |   _ => allRegisters
    end

    (* An optional result. i.e. if the code before the jump has returned a result
       this is the offset in the table of the result. *)
    datatype mergeResult = NoMerge | MergeIndex of stackIndex;

    (* A code label packaged up with a saved state. *)
    abstype labels =
        NoLabels
    |   Labels of {result: mergeResult, lab: CODECONS.forwardLabel, state: savedState}
    with
        val noJump = NoLabels;
        fun isEmptyLabel NoLabels = true | isEmptyLabel _ = false;
        fun makeLabels(res, cLab, sState) = Labels {result=res, lab = cLab, state = sState};
  
        fun labs  (Labels {lab  ,...}) = lab   | labs  _ = raise Match;
        fun state (Labels {state,...}) = state | state _ = raise Match;
        fun result (Labels {result,...}) = result | result _ = raise Match;
    end

    (* Set the state to the saved values.  This could almost certainly be radically simplified.
       Most of the complicating factors have now been removed. *) 
    fun setState (save : savedState,
                  table as
                    Ttab{pstackTrace, printStream, pstackptr, realstackptr, regset, markReal, ...},
                  carry, mark, isMerge, isRestoreLoop): mergeResult * operation list =
    let 
        val () = if pstackTrace then printState printStream save "setState" else ()
        val topReg: reg =
            case carry of
                NoMerge => regClosure (* Unused *)
              | MergeIndex savedTop =>
                (
                    case pStackRealEntry(save, savedTop) of
                      (_,{ent = Register(reg, _), ...}) => reg
                
                    | (_,{cache, ...}) =>
                        if cache = noRegisters then raise InternalError "setState: not a register"
                        else oneOf cache
                )
        (* Clobber all entries above the "mark".
            This will remove the result register if there is one. *)
        (* TODO: I don't like this.  I think we should explicitly remove it. *)
        fun freeAbove i =
            if i < mark then []
            else removeEntry(table, i, true) @ freeAbove(i-1)
        val freeTop = freeAbove(! pstackptr - 1)

          (* Set up the saved state. Need to set the register set.
             Free the registers from the table. *)
        fun frees s =
            if s >= ! pstackptr
            then []
            else
            let
                val stacken = pstackEntry(table, s)
                val freeEntry =
                 case stacken of
                    NoStackEntry => []
      
                 |  StackEntry {ent, cache, ...} =>
                    let
                        val freeThis =
                            case ent of 
                                Register(reg, _) => freeRegister (table, reg)
                            |   Direct {base, ...} => freeRegister (table, base)
                            |   _ => []
                    in
                        freeSet(regset, cache) @ freeThis
                    end
            in
                frees(s + 1) @ freeEntry
            end

        val freeTable = frees first 

        val () = realstackptr := realStackPtr save;
    
        local
            val oldPstackptr = ! pstackptr
            val () = pstackptr := pStackPtr save

            (* Go up the entries putting them onto the table from the saved
             state, then come back setting the use-counts where appropriate.
             We have to do it this way because of copy entries. *)
            (* But we don't have copy entries any longer so this could
             be improved. DCJM 30/11/99. *)
            fun putOnEntries s =
                if s >= pStackPtr save
                then []
                else
                let
                    val saveEntry = saveStateEntry(save, s)
                    val (tabUseCount, tabDestStack) =
                        (* Get the use-count and stack destination in the table. *)
                        if s >= oldPstackptr then (0, NONE)
                        else 
                          case pstackEntry(table, s) of
                            NoStackEntry           => (0, NONE)
                          | StackEntry {uses, destStack, ...} =>
                                (uses, (*if ! exited then NONE else*) destStack)
                in
                    (* Put the saved entry into the table. *)
                    setPstackEntry (table, s, saveEntry);
          
                    case saveEntry of
                        NoStackEntry => putOnEntries (s + 1)
      
                    |   StackEntry {ent, cache, uses, lifeTime, ...} =>
                        let
                            (* Compute the new register set. *)
                            val loadReg =
                                case ent of
                                    Register(reg, _) => incr regset reg
                                |   Direct {base, ...} => incr regset base
                                |   _ => []
                            val cacheRegs = List.foldl(fn (r, l) => incr regset r @ l) [] (setToList cache)

                            (* The destination stack information is intended to avoid problems
                               when a value is pushed to the stack in different flows of control.
                               The first flow of control sets the destination stack with the
                               actual location where the value was pushed and then when we set
                               the state we record the destination stack location as the
                               location where subsequent flows of control should push it.
                               This goes wrong if we are using setState to restore the loop
                               state rather than set the initial state for a new flow of control.
                               In the first case we need to ensure that the destination stack
                               values are inherited from the current state and override those in
                               the saved state, because we may have popped items during the first
                               flow and subsequently pushed different items in their place,
                               whereas if we're restoring the state we want to throw away any
                               destination stack information in the current state and take
                               the information only from the saved state. *)
                            val () =
                                if isRestoreLoop
                                then ()
                                else setPstackEntry (table, s,
                                            StackEntry {ent=ent, cache=cache, uses=uses,
                                                        destStack=tabDestStack, lifeTime=lifeTime})
                            val otherEntries = putOnEntries (s + 1)
              
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
                            val () =
                                if isMerge
                                then
                                let
                                    val currUseCount =
                                    if s >= (! pstackptr) then 0
                                    else 
                                    case pstackEntry(table, s) of
                                        NoStackEntry           => 0
                                    |   StackEntry {uses, ...} => uses
                                in
                                    if tabUseCount < currUseCount
                                    then (incrUseCount (table, s, tabUseCount - currUseCount); ())
                                    else ()
                                end
                                else ()
                        in
                            otherEntries @ cacheRegs @ loadReg
                        end
                end
            (* If the state we're setting into has a destination location for an entry we don't
               override it with an entry for the saved state but instead merge in.  However we
               could have pushed something else on route to the saved state and even though the
               entry is no longer on the pstack we could now find that the real stack pointer is
               above the current destination location.  We need to reset the stack if that happens
               so that we can push the entry if we need to.  *)
            fun checkUnPushed s min =
                case pstackEntry(table, s) of
                    StackEntry{ent=StackW _, ...} => min
                |   StackEntry{destStack=SOME dest, ...} => Int.min(dest, min)
                |   _ => min
        in
            val addRegs = putOnEntries first
            val minUnpushed = revfoldIndDownTo(checkUnPushed, ! realstackptr, ! pstackptr - 1, first)
            val andReset =
                if minUnpushed < ! realstackptr
                then if minUnpushed < !markReal
                then raise InternalError "Unpushed entry below stack mark"
                else
                let
                    val current = !realstackptr;
                in
                    realstackptr := minUnpushed;
                    resetStack(current-minUnpushed)
                end
                else []
        end

        val regLoadFree = andReset @ addRegs @ freeTable @ freeTop
        val result =
            case carry of
                MergeIndex _ =>(* Put the result register onto the stack. *)
                    let
                        val regCode = getRegister (table, topReg)
                    in
                        (MergeIndex(pushReg (table, topReg)), activeRegister topReg @ regCode @ regLoadFree)
                    end
            |   NoMerge => (NoMerge, regLoadFree)
    in
        if pstackTrace then printStack(table, "setState", #2 result) else ();
        result
    end

    fun unconditionalBranch (result, table as Ttab { branched, ...}) : labels * operation list =
    if ! branched then (noJump, [])
    else
    let
        val state = saveState table
        val (branchCode, label) = uncondBranch()
    in
        branched := true;
        (makeLabels(result, label, state), branchCode)
    end;

    fun jumpBack(start, Ttab { branched, ...}): operation list =
    (
        branched := true;
        (* Check for interrupts when jumping backwards. *)
        CODECONS.jumpBack start @ interruptCheck
    )

    (* Record the stack limit when we diverge and then use it when we merge
       back again. *)
    type stackMark =
    {
        newMark: stackIndex,
        oldMark: stackIndex,
        oldReal: int
    }
  
    fun newMark ({newMark,...}: stackMark) = newMark
    
    fun markStack(Ttab{pstackptr, marker, markReal, realstackptr, ...}) =
        { newMark = ! pstackptr, oldMark = ! marker, oldReal = ! markReal } before
            (marker := ! pstackptr; markReal := ! realstackptr)

    fun unmarkStack(Ttab{marker, markReal, ...}, {oldMark, oldReal, ...}) =
        ( marker := oldMark; markReal := oldReal )

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
                    table as
                        Ttab{pstackTrace, printStream, realstackptr, pstackptr, marker, regset, ...},
                    currentResult: mergeResult, mark) : bool * mergeResult * operation list =
    let
        val needOtherWay = ref false;

        val () =
            if pstackTrace
            then
            (
                printStack(table, "mergeState", []);
                printState printStream save "saved state" 
            ) 
            else ();
        val () =
            if ! marker <> newMark mark
            then raise InternalError "Marker"
            else ();
    
        (* Merge the tables together. The only complication is that if both
           sides are returning values they may be at different locations on
           the pseudo stack. We load the top
           of the current stack into the register that was used for the top
           of the saved state and then remove it. There is no need to remove
           the top of the saved state because those entries will correspond
           to zero-use count entries in the current stack. *)
        val (topReg, topRegCode) =
            case (savedResult, currentResult) of
                (MergeIndex savedTop, MergeIndex currentTop) =>
                let
                    val sTopReg = 
                        case pStackRealEntry (save, savedTop) of
                            (_,{ent = Register(reg, _), ...}) => reg
                        |   (_,{cache, ...}) =>
                                if cache = noRegisters then raise InternalError "Not a register"
                                else oneOf cache
            
                    (* Load the value on the top of "table" into the same register
                        (it ought to be there anyway). *)
                    val (regEntry, loadCode) = loadToSpecificReg (table, sTopReg, currentTop, true);
                    (* Because this register will be at a different offset in
                      the table from in the saved state it is easier to remove
                      the register and put it on later. *)
                    val removeEntry = removeStackEntry(table, regEntry);
                in
                   (sTopReg, removeEntry @ getRegister (table, sTopReg) @ loadCode)
                end
             | (NoMerge, NoMerge) => (regClosure (* Unused *), [])
             | _ => (* They should agree on whether they will return a result or not. *)
                raise InternalError "mergeState - Mismatched result states"

        (* Clobber all entries above the "mark". These are values which are
           local to the block since the split and so are no longer required.
           They should normally have been removed as soon as they were no
           longer required.  *)
        val freeEntries0 =
            revfoldIndDownTo (fn s => fn l => removeEntry(table, s, true) @ l, [], ! pstackptr - 1, newMark mark)
         
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
        fun fixEntry s l =
        let
            val entry =
            case (pstackEntry(table, s), saveStateEntry(save, s)) of
                (NoStackEntry, _) => [] (* No entry in table. *)

            |   (StackEntry _, NoStackEntry) =>
                     (* table entry could be non-empty if it is a cache entry
                        or if we are doing a backwards merge. If we do a
                        backwards merge we can have entries in the table 
                        with non-zero use counts, but those can be removed. *)                 
                      removeEntry(table, s, false)

            |   (StackEntry {uses = tabUses, cache = tabCache, ent = tabEnt,
                             destStack = tabDest, lifeTime},
                 StackEntry {uses = saveUses, cache = saveCache, ent = saveEnt,
                             destStack = saveDest, ...}) =>
                    let
                      val mergedDest =
                        case (tabDest, saveDest) of
                            (tb as SOME tabDest, SOME saveDest) =>
                                if tabDest <> saveDest
                                then raise InternalError("merge: mismatched destination "^Int.toString tabDest^ " " ^ Int.toString saveDest)
                                else tb
                        |   (NONE, saveDest) => saveDest
                        |   (tabDest, _) => tabDest
                    in
                      if tabUses = 0 orelse saveUses = 0
                       (* The use-counts may be zero if we have retained an
                          entry because it is cached in a register. We remove
                          these entries unless it is the same value and cached 
                          in the same register *)
                      then
                        if tabCache <> saveCache (* TODO: Handle non-empty intersection. *)
                        then removeEntry(table, s, false)
                        else 
                          case (tabEnt, saveEnt) of 
                            (Direct {base = tabBase,  offset = tabOffset},
                             Direct {base = saveBase, offset = saveOffset}) =>
                               if tabBase = saveBase andalso tabOffset = saveOffset
                               then []
                               else removeEntry(table, s, false)
                           
                          | (StackW tabIndex, StackW saveIndex) =>
                                if tabIndex = saveIndex
                                then []
                                else removeEntry(table, s, false)
                            
                          | _ => 
                                removeEntry(table, s, false)
        
                      else (* We need to retain this entry. *)
                         (
                         if tabDest <> mergedDest
                         then setPstackEntry (table, s,
                                StackEntry{ent=tabEnt, cache=tabCache, uses=tabUses,
                                           destStack=mergedDest, lifeTime=lifeTime})
                         else ();
                         []
                         )
                    end
        in
            entry @ l
        end

        val freeEntries1 = revfoldIndDownTo (fixEntry, [], ! pstackptr - 1, first)

        local
           (* Try to align the real stack pointer by popping unused values.
              We MUST remove entries which have been pushed onto the stack
              in the saved state but not in the current state since we'll
              have to push them here.  We must not remove entries which
              are currently in use.  One further complication is that we
              may have exception handler(s) on the real stack so we can't
              simply pop everything above the highest used stack position.
              It would probably be better if we recorded handler locations
              on the pstack - maybe change this. *)
             (* Find the highest stack value which is actually in use. *)
             fun getInUse s i =
                case pstackEntry(table, s) of
                    StackEntry {ent = StackW addr, ...} =>
                        (* The stack pointer must be one more
                           than the highest value in use. *)
                        Int.max(addr+1, i)
                |   _ => i

             val stackInUse =
             (* This is the highest used stack location, but we may have a
                handler above it so we can't necessarily reset the stack
                to here. *)
                revfoldIndDownTo(getInUse, 0, (! pstackptr) - 1, first)

             (* Examine the saved stack to see those entries which have
                been pushed in the saved state but not in the current
                state. We need to reset the stack below this.  If
                there are no such entries we return the stack pointer
                from the saved state. *)
             fun getMinStack s i =
                case (pstackEntry(table, s), saveStateEntry(save, s)) of
                    (StackEntry {ent = StackW _, ...}, _) => i
                |   (StackEntry _, StackEntry{ent = StackW addr, ...}) =>
                        (* We have an entry which has been pushed in
                           the saved state but not in the current state.
                           We have to set sp below this. *)
                        let
                            val minStack = Int.min(addr, i)
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
                revfoldIndDownTo(getMinStack, realStackPtr save, (! pstackptr) - 1, first)

             (* We can reset the stack to the maximum of the entries
                currently in use and those which need to be pushed or
                the saved sp if there aren't any. *)
            val maxStack = Int.max(stackInUse, minStack)
       in
            val resetCode =
                if maxStack < ! realstackptr
                then
                let
                    val reset = resetStack (! realstackptr - maxStack)
                in
                    realstackptr := maxStack;
                    reset
                end
                else []
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
        local
            fun mustPush s =
                case (pstackEntry(table, s), saveStateEntry(save, s)) of
                    (StackEntry {ent = StackW _, ...}, StackEntry {ent = StackW _, ...}) =>
                        false (* both on stack *)
                |   (StackEntry _, StackEntry {ent = StackW _, ...}) =>
                        true (* Saved value is on stack but current value isn't. *)
                |   _ => false
        in
            (* Pushing one entry may result in others being pushed if
                they have a lower "destStack". *)
            val pushCode = pushRegisters(table, allRegisters, mustPush, false, false)
        end;

        (* Third pass: Load any entry which is in a register in the saved state
          and ensure that values in registers in the current state are moved
          into the same register as before. *)
        local
            (* Put the table entry in a specified register and
               make it a register entry. *)
            fun loadToReg (s, prefReg: reg option, tabEnt, tabCache: regSet, tabUses, tabDest, tabLife) =
            let
                (* First get the register to put the value in. *)
                val (dReg, getRegCode) =
                    case prefReg of
                        NONE => getAnyRegister table
                    |   SOME prefReg =>
                            if (case tabEnt of Register(reg, _) => reg = prefReg | _ => false) orelse
                                inSet(prefReg, tabCache) (* Already there. *)
                            then (addRegUse (table, prefReg); (prefReg, [])) (* Already there. *)
                            else (prefReg, getRegister (table, prefReg))

                (* Now load it.  However, we have to be careful.  We only call loadToReg
                   if an entry was in a register or a "Direct".  But in the process of getting
                   the destination register we could have ended up pushing it onto the stack.
                   This can happen if the destination register was in use.  We always
                   push unpushed entries with a lower "destStack" first so if the register we
                   want was in an entry with a higher "destStack" and this entry has a lower
                   "destStack" we could have pushed it.  In that case we MUST leave it where
                   it is and NOT overwrite it. (See Test145.ML) *)
            in
                case pstackEntry(table, s) of
                    StackEntry {ent = StackW _, ...} => free regset dReg @ getRegCode

                |   _ =>
                    let
                        val loadCode = loadPstackEntry(table, s, dReg)
                        (* loadPstackEntry will have decremented the use count and may
                         have completely removed the entry.  If it hasn't we need to
                         remove it before we replace it with the loaded register. *)
                        val freeCode =
                            case (pstackEntry(table, s)) of
                                NoStackEntry => []
                            |   StackEntry _ => removeEntry(table, s, false)
                    in
                        setPstackEntry (table, s,
                            StackEntry{ent=Register(dReg, 0), cache=noRegisters, uses=tabUses,
                                       destStack=tabDest, lifeTime=tabLife});
                        freeCode @ loadCode @ getRegCode
                    end
            end

(*

                val (dReg, dRegCode) =
                    case prefReg of
                        NONE =>
                        let
                            val (reg, regCode) = getAnyRegister table
                        in
                            (reg, loadPstackEntry(table, s, reg) @ regCode)
                        end
                    |   SOME prefReg => (* Put it in the preferred register. If it's already there
                                           we need to increment the use count because we will
                                           decrement it in "removeEntry". *)
                        if (case tabEnt of Register(reg, _) => reg = prefReg | _ => false) orelse
                            inSet(prefReg, tabCache) (* Already there. *)
                        then (addRegUse (table, prefReg); (prefReg, [])) (* Already there. *)
                        else
                        let
                            val getRegCode = getRegister (table, prefReg)
                        in
                            (prefReg, loadPstackEntry(table, s, prefReg) @ getRegCode)
                        end
                  (* loadPstackEntry will have decremented the use count and may
                     have completely removed the entry.  If it hasn't we need to
                     remove it before we replace it with the loaded register.
                     If we didn't call loadPstackEntry (because we already had
                     the value in the correct register) we have to call removeEntry
                     to decrement the register use count (we incremented it above)
                     and so restore it to the original value. *)
                val freeCode =
                    case (pstackEntry(table, s)) of
                        NoStackEntry => []
                    |   StackEntry _ => removeEntry(table, s, false);
            in 
                setPstackEntry (table, s,
                    StackEntry{ent=Register(dReg, 0), cache=noRegisters, uses=tabUses,
                               destStack=tabDest, lifeTime=tabLife});
                freeCode @ dRegCode
            end (* loadToReg *)
*)
            fun loadEntries(s, others) =
            if s >= first
            then
            let
                val thisItem =
                    case (pstackEntry(table, s), saveStateEntry(save, s)) of
                        (StackEntry {ent = StackW _, ...}, _) =>
                        (* If it's in the stack we don't try reloading it. *) []

                    |  (StackEntry {ent = tabEnt, cache, uses, destStack, lifeTime, ...},
                        StackEntry {ent = Register(savedReg, _), ...}) =>
                        loadToReg(s, SOME savedReg, tabEnt, cache, uses, destStack, lifeTime)

                    |  (StackEntry{ent = tabEnt as Direct{base = tabBase, offset = tabOffset},
                                cache = tabCache, uses, destStack, lifeTime, ...},
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
                            if tabBase <> saveBase
                            then
                                let
                                    val prefReg =
                                        if regSetIntersect(savedCache, tabCache) <> noRegisters
                                        then SOME(oneOf(regSetIntersect(savedCache, tabCache)))
                                        else NONE (* No preference. *)
                                in
                                    loadToReg (s, prefReg, tabEnt, tabCache, uses, destStack, lifeTime)
                                end
                            else []
                        )
                    |  _ => []
             in
                loadEntries(s - 1, thisItem @ others)
             end
            else others
       in
            val loadRegCode = loadEntries (! pstackptr - 1, [])
       end;

           (* Final pass: Check to see if we need to do a "reverse merge" i.e.
              operations that have to be done on the saved state before we
              can finally merge.  Also flush mismatched items from the cache. *)
        local
            fun checkEntries s l =
            let
                val entry =
                case (pstackEntry(table, s), saveStateEntry(save, s)) of
                    (StackEntry {uses = tabUses, cache = tabCache, ent = tabEnt,
                                 destStack = tabDest, lifeTime},
                     StackEntry {cache = saveCache, ent = saveEnt, ...}) =>
                        let
                            fun flushCache () =
                            let
                                (* Remove all entries from the current cache that are not also in
                                   the saved cache. *)
                                val keep = regSetIntersect(tabCache, saveCache)
                            in
                                setPstackEntry (table, s,
                                    StackEntry {ent=tabEnt, cache=keep, uses=tabUses,
                                                destStack=tabDest, lifeTime=lifeTime});
                                freeSet(regset, regSetMinus(tabCache, keep))
                            end
                        in
                        case tabEnt of
                            Register(tabReg, _) =>
                                (
                                    (* It's fine if the saved value was cached in that register. *)
                                    if inSet(tabReg, saveCache)
                                    then ()
                                    else case saveEnt of
                                        Register(saveReg, _) =>
                                            (* We should have moved these into the same
                                               register.  It's possible it got moved again
                                               as a result of loading something else. *)
                                            if tabReg <> saveReg
                                            then needOtherWay := true else ()
                                    |   StackW _ =>
                                            (* We should have pushed it in the second pass. *)
                                            raise InternalError "merge: unpushed entry"
                                    |   _ => (* Maybe a Direct entry which has to be
                                                loaded in a reverse merge. *)
                                            needOtherWay := true;
                                    []
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
                                if saveCache = tabCache (* TODO: Should this be an intersection? *)
                                then []
                                else
                                    case saveEnt of
                                        Direct{base=saveBase, ...} =>
                                           if tabBase = saveBase
                                           then flushCache() (* Ok but must flush cache. *)
                                           else (needOtherWay := true; [])
                                    |   _ =>
                                            raise InternalError "merge: mismatched Direct"
                                )
                        |   StackW tabIndex =>
                                (
                                case saveEnt of
                                    StackW saveIndex =>
                                        (
                                        (* Consistency check. *)
                                        if tabIndex = saveIndex then ()
                                        else raise InternalError "merge: mismatched stack entries";
                                        flushCache()
                                        )
                                    | _ => (* Need to push this in a reverse merge. *)
                                        (needOtherWay := true; [])
                                )
                        |   Container _ =>
                                (
                                case saveEnt of
                                    Container _ => []
                                |   _ => raise InternalError "merge: mismatched Container"
                                )
                        end
                  | _ => []
            in
                entry @ l
            end
        in
              val freeEntries2 = revfoldIndDownTo (checkEntries, [], ! pstackptr - 1, first)
        end;
           (* Last of all, try to align the stack. If the current stack pointer
              is greater than the saved value we must have live values on the
              stack and have to do a reverse merge.  If the saved stack pointer
              was greater than the current but otherwise everything is fine
              we just push some dummy values rather than doing a reverse merge.
              I may change this later. *)
        val () =
            if realStackPtr save < ! realstackptr
            then needOtherWay := true
            else ();

        val alignCode =
            if ! needOtherWay then []
            else
            let
                fun addAlign others =
                if realStackPtr save <= ! realstackptr
                then others
                else
                let
                    val (pushIt, pushInstrs) = pushAnyEntryAtCurrentSP table
                in
                    if pushIt then addAlign(pushInstrs @ others)
                    else  (* Push a register just to align the stack.  It would
                        be better to push a register that wasn't currently
                        saved but this will do for the moment. *)
                    (
                        realstackptr := ! realstackptr + 1;
                        addAlign pushToReserveSpace @ pushInstrs @ others
                    )
                end
            in
                addAlign []
            end

        val (result, resultCode) = (* Push any result. *)
            case currentResult of
                MergeIndex _ => (MergeIndex(pushReg (table, topReg)), activeRegister topReg)
              | NoMerge => (NoMerge, [])
    in
        if pstackTrace then printStack(table, "mergeState", []) else ();
        (!needOtherWay, result,
            resultCode @ alignCode @ freeEntries2 @ loadRegCode @ pushCode @ resetCode @
            freeEntries1 @ freeEntries0 @ topRegCode)
    end

    (* Fix up a label after an unconditional branch. *)
    fun fixup (lab, table as Ttab { branched, exited, pstackptr, ...}) : operation list =
    if not (! branched) then raise InternalError "Not branched"
    else if isEmptyLabel lab then []
    else
    let 
        val (_, code) = setState (state lab, table, NoMerge, ! pstackptr, false, false)
    in
        branched := false;
        exited := false;
        code @ forwardJumpLabel(labs lab)
    end
    
    local
        (* Fix up a label.  If this follows an unconditional branch we replace the
           existing state with the saved state, otherwise we have to merge in. *)
        fun mergeLab (lab, table as Ttab { branched, exited, ...}, currentResult: mergeResult, mark) =
            if isEmptyLabel lab then (currentResult, [])
            else if ! branched
            then
            let
                val (newResult, newCode) =
                    setState (state lab, table, result lab, newMark mark, true, false)
            in
                branched := false;
                exited := false;
                (newResult, newCode @ forwardJumpLabel(labs lab))
            end
            else
            let
                val (otherWay, mergeRes, mergeCode) =
                    mergeState (state lab, result lab, table, currentResult, mark);
            in (* We can generate code before we fix up the label, but if we
                  want to add code to the other arm we have to put in an
                  unconditional branch and make the changes after it. *)
                if otherWay
                then
                let
                    (* Have to jump round to get the states the same. *)
                    val (lab1, lab1Code) = unconditionalBranch (mergeRes, table)
                    val (newResult, newCode) = setState (state lab, table, result lab, newMark mark, true, false)
                    val () = exited := false;
                    val () = branched := false;
                    val (mergeResult, otherMerge) = (* Merge the other way. *)
                        mergeLab (lab1, table, newResult, mark)
                in
                    (mergeResult, otherMerge @
                        newCode @ forwardJumpLabel(labs lab) @ lab1Code @ mergeCode)
                end
                else (mergeRes, forwardJumpLabel(labs lab) @ mergeCode)
            end
    in
        (* Fix up a label.  If this follows an unconditional branch we replace the
           existing state with the saved state, otherwise we have to merge in. *)
        fun merge (lab, table, carry, mark) =
        let
            val resCode = mergeLab (lab, table, carry, mark);
        in (* Reset the marker even if we have not actually done any merging. *)
            unmarkStack(table, mark);
            resCode
        end;

        (* Fix up a list of labels, using the same stack mark *)
        fun mergeList (labs, table, carry, mark) =
        let
            fun mergeLabs (l, (carry, otherCode)) =
            let
                val (newCarry, code) = mergeLab (l, table, carry, mark)
            in
                (newCarry, code @ otherCode)
            end
  
            val resCode = List.foldl mergeLabs (carry, []) labs
        in
            unmarkStack(table, mark);
            resCode
        end;
    end;
  
    type handler = { lab: addrs ref, oldps: stackIndex };
  
    (* Push the address of a handler. *)
    fun pushAddress (table as Ttab{pstackptr, ...}, offset) =
    let
        (* This is just after a mark. *)
        val (reg, regCode) = getAnyRegister table
        val oldps = ! pstackptr
        val labelRef = ref addrZero

        (* Load the address of the handler into a register. *)
        val loadCode = loadHandlerAddress{ handlerLab=labelRef, output=reg}
        val regEntry = pushReg(table, reg)

        (* Push it onto the stack at the specific offset. *)
        val (pushedEntry, pushCode) = pushValueToStack (table, regEntry, offset)
    in
        (pushedEntry, {lab = labelRef, oldps = oldps}, pushCode @ loadCode @ regCode)
    end

    (* Fixup the address at the start of a handler. *)
    fun fixupH ({lab, oldps}, oldsp, table as Ttab{branched, exited, pstackptr, realstackptr, ...}) =
    let 
        val clear = clearCache table (* Don't know the registers here. *)
    in
        realstackptr := oldsp;
        exited := false;
        branched := false;

        (* Remove any entries above the old pstack pointer. If the expression
           whose exceptions we are handling contained static-link functions
           there may be entries whose use-counts have not gone to zero. *)
        startHandler{ handlerLab=lab} @
            revfoldIndDownTo(fn s => fn l => removeEntry(table, s, false) @ l, [], ! pstackptr - 1, oldps) @
                clear
    end

    (* Reload the handler "register" from an entry on the real stack. *)
    fun reloadHandler(table, hIndex) =
    let
        val (reg, entry, loadCode) = loadEntryToSet(table, hIndex, generalRegisters, false)
        val storeCode = storeToHandler reg
        val clear = incrUseCount (table, entry, ~1)
        val clearCode = removeOldItemsFromRealStack table
    in
        clearCode @ clear @ storeCode @ loadCode
    end

    (* Generate operations.  Negotiates the arguments and results with the machine-specific
       code-generator. This module knows where the arguments are as a result of previous operations
       but the machine-dependent code-generator will have requirements depending on the available
       instructions e.g. some or all the arguments may have to be in registers.  *)
    local
        (* We have to re-evaluate this after each action because we may have
           had to push values to the stack that were previously in registers. *)
        fun argsAsSources(table as Ttab{realstackptr, ...}, args) =
        let
            fun argAsSource entry =
            let
                val {ent = stackEntry, cache, ...} = pstackRealEntry(table, entry);
                val lastRef = lastReference table entry;
            in
                if cache <> noRegisters
                then
                (
                    if lastRef
                    then ActInRegisterSet{modifiable=cache, readable=cache}
                    else ActInRegisterSet{modifiable=noRegisters, readable=cache}
                )
                else case stackEntry of
                    Register(reg, _) =>
                        if lastRef
                        then ActInRegisterSet{modifiable=singleton reg, readable=singleton reg}
                        else ActInRegisterSet{modifiable=noRegisters, readable=singleton reg}

                |   Literal m => ActLiteralSource m

                |   Direct{base, offset} => ActBaseOffset(base, offset)

                |   StackW index =>
                            ActBaseOffset(regStackPtr, (! realstackptr - 1 - index) * wordSize)

                |   Container{items, ...} =>
                    (
                        case pstackRealEntry(table, hd items) of
                            {ent = StackW index, ...} =>
                                 ActStackAddress((! realstackptr - index -1) * wordSize)
                        |   _ => raise InternalError "argAsSource: container entry is not on stack"
                    )

                |   CodeRef code => ActCodeRefSource code

            end
        in
            List.map argAsSource args
        end

        fun performActions(start, finish, args, table as Ttab{pstackTrace, ...}) =
        let
            (* Process each action and then get the next action until we are finally finished. *)
            fun untilDone(ActionDone{outReg, operation}, args, loadOps, lockedRegs) =
                let
                    (* Finished.  Unlock the registers, push the result and return. *)
                    (* If we have an output register increment its use-count first. *)
                    val () = case outReg of SOME reg => lockRegister(table, reg) | NONE => ()
                    val result = finish outReg (* Set the result. *)
                    val freeLock = List.foldl (fn(reg, l) => unlockRegister(table, reg) @ l) [] lockedRegs
                    val freeEntries = List.foldl (fn(arg, l) => incrUseCount (table, arg, ~1) @ l) [] args
                    val () = if pstackTrace then printStack(table, "performActions", operation @ loadOps) else ()
                in
                    (result, freeEntries @ freeLock @ operation @ loadOps)
                end

            |   untilDone(ActionLockRegister{argNo=_, reg, willOverwrite, next}, args, loadOps, lockedRegs) =
                let (* Lock a register that's currently being used for an argument so that it won't
                       be reused for a different argument. *)
                    (* We only use ActionLockRegister if the entry has been detected as being in a
                       register but this may be because the entry is a Register or a cache entry. *)
                    val () = lockRegister (table, reg)
                    val clear =
                        if willOverwrite
                        then removeRegistersFromCache(table, listToSet [reg])
                        else []
                in
                    untilDone(next(argsAsSources(table, args)), args, clear @ loadOps, reg :: lockedRegs)
                end

            |   untilDone(ActionLoadArg{argNo, regSet, willOverwrite, next}, args, loadOps, lockedRegs) =
                let
                    val arg = List.nth(args, argNo)
                    val (_, regLoc, loadCode) = loadEntryToSet (table, arg, regSet, willOverwrite)
                    (* Replace the argument by the location that refers to the register. *)
                    val repArgs = List.take(args, argNo) @ [regLoc] @ List.drop(args, argNo+1)
                in
                    untilDone(next(argsAsSources(table, repArgs)), repArgs, loadCode @ loadOps, lockedRegs)
                end

            |   untilDone(ActionGetWorkReg{regSet, setReg}, args, loadOps, lockedRegs) =
                let
                    val (reg, regCode) = getRegisterInSet(table, regSet)
                in
                    untilDone(setReg reg (argsAsSources(table, args)), args, regCode @ loadOps, reg :: lockedRegs)
                end
        in
            untilDone(start (argsAsSources(table, args)), args, [], [])
        end
    in
        (* Data operations that return a result in a register. *)
        fun dataOp (args, instr, table, hint) =
        let
            (* Match up the result that the instruction has provided with what we
               need for the context.  Generally these will match already but we
               have to consider the case where a result is being discarded or
               where we want the unit result from an assignment function. *)
            fun finalOp outReg =
                case (outReg, hint) of
                    (NONE, NoResult) => noIndex
                |   (NONE, _) => (* Want a unit result. *) pushConst (table, toMachineWord 0)
                |   (SOME outReg, NoResult) => (* Discard *)
                        (unlockRegister(table, outReg); noIndex)
                |   (SOME outReg, _) => (* Want this result. *) pushReg (table, outReg)
        in
            performActions(negotiateArguments(instr, hint), finalOp, args, table)
        end

        (* Comparison and test operations that make a conditional branch. *)
        and compareAndBranch(args, test, table) : labels * operation list =
            let
                val (startActions, destLabel) = negotiateTestArguments test
                val ((), code) = performActions(startActions, fn _ => (), args, table)
            in
                (makeLabels(NoMerge, destLabel, saveState table), code)
            end


        (* Moves an expression into a newly created vector or into a container.
           The offset is the number of bytes if this a byte store and the number of
           words if this is word store. *)
        and moveToVec (vecEntry, valueEntry, offset: int, table) : operation list =
            let
                val freeCode1 = incrUseCount (table, vecEntry, 1) (* This must be kept. *)
                (* Turn this into an assignment operation. *)
                val arguments = [vecEntry, pushConst(table, toMachineWord offset), valueEntry]
                fun mapLiteral ent =
                    case pstackRealEntry(table, ent) of
                        {ent = Literal lit, ...} => SOME lit
                    |   _ => NONE
            in
                case checkAndReduce(instrStoreW, arguments, mapLiteral) of
                    SOME(ins, args) =>
                        let
                            (* Decrement the use counts for entries that we're not using. *)
                            val freeCode2 = List.foldl(fn (a, l) => incrUseCount (table, a, 1) @ l) [] args
                            val freeCode3 = List.foldl(fn (a, l) => incrUseCount (table, a, ~1) @ l) [] arguments
                        in
                            #2(dataOp(args, ins, table, NoResult)) @ freeCode3 @ freeCode2 @ freeCode1
                        end
                |   NONE => raise InternalError "moveToVec: Not implemented"
            end
    end

    (* Tail recursive jump to a function. *)
    fun jumpToCode(codeAddr, isIndirect, transtable) =
    let
        val {ent, ...} = pstackRealEntry(transtable, codeAddr)
        val code =
        case ent of
            Literal lit =>
                jumpToFunction(if isIndirect then ConstantClosure lit else ConstantCode lit)

        |  CodeRef code =>
            if isIndirect
            then raise InternalError "jumpToCode: indirect call to codeRef"
            else jumpToFunction(CodeFun code)

        |  Register(reg, _) => (* Should only be the closure register and only in
                           the indirect case. *)
            if isIndirect andalso reg = regClosure
            then jumpToFunction FullCall
            else raise InternalError "jumpToCode: Not indirection through closure reg"

        |  _ => (* Anything else shouldn't happen. *)
            raise InternalError "jumpToCode: Not a constant or register";
    in
        incrUseCount (transtable, codeAddr, ~1);
        code
    end;

    (* Call a function. *)
    fun callCode(codeAddr, isIndirect, transtable) =
    let
        val {ent, ...} = pstackRealEntry(transtable, codeAddr)
        val code =
        case ent of
            Literal lit =>
                callFunction(if isIndirect then ConstantClosure lit else ConstantCode lit)

        |  CodeRef code =>
            if isIndirect
            then raise InternalError "callCode: indirect call to codeRef"
            else callFunction(CodeFun code)

        |  Register(reg, _) => (* Should only be the closure register and only in
                           the indirect case. *)
            if isIndirect andalso reg = regClosure
            then callFunction FullCall
            else raise InternalError "callCode: Not indirection through closure reg"

        |  _ => (* Anything else shouldn't happen. *)
            raise InternalError "callCode: Not a constant or register";

    in
        incrUseCount (transtable, codeAddr, ~1);
        code
    end;

    datatype argdest = ArgToRegister of reg | ArgToStack of int | ArgDiscard

    (* Get the destination for the argument of a loop instruction.  This
       finds out where the argument was loaded at the start of the loop
       so that it can be put back there at the end. *)
    fun getLoopDestinations(indices, transtable) =
        let
            fun getLoopDest entry =
                if entry = noIndex
                then ArgDiscard
                else case pstackRealEntry(transtable, entry) of
                    {ent = StackW index, ...} => ArgToStack index
                |   {ent = Register(reg, _), ...} => ArgToRegister reg
                |   _ => raise InternalError "getLoopDest: wrong entry type"
            (* Remove cache registers from loop variables.  There was a bug where a
               loop variable was not being reloaded when the corresponding stack
               entry had been modified within the loop. *)
            fun removeCache(entry, ops) =
                if entry = noIndex
                then ops
                else clearCacheEntry(transtable, allRegisters, entry) @ ops
        in
            (map getLoopDest indices, List.foldl removeCache [] indices)
        end

    type loopPush = stackIndex

    (* Compare the saved state at the start of the loop with the current state at the
       point we're looping and see whether we need to modify the original state and
       reprocess the loop.  We look for differences in the cache and values that have
       been pushed to the stack. *)
    fun compareLoopStates(table as Ttab{ pstackptr, ...}, state, argIndexes) =
    let
        fun processTables(entry, cacheSet, pushList: loopPush list) =
            if entry = ! pstackptr
            then (cacheSet, pushList)
            else if List.exists(fn e => entry = e) argIndexes
            then (* It's an argument.  These can be modified in the loop. *)
                processTables(entry+1, cacheSet, pushList)
            else case (pstackEntry(table, entry), saveStateEntry(state, entry)) of
                (StackEntry{ent=tabEnt, cache=tabCache, ...}, StackEntry{ent=saveEnt, cache=saveCache, ...}) =>
                let
                    (* Add any registers that were cached before but aren't now.  It's ok if we have loaded
                       something in the loop and are currently caching it. *)
                    val newCacheSet =
                        regSetUnion(cacheSet, regSetMinus(saveCache, tabCache))
                    (* Put this on the push list if there are entries that were in registers or
                       were direct entries and are now on the stack or in registers.
                       TODO: If we just loaded an entry or moved it to a different register
                       we might be able to put it in that register rather than having to
                       push it to the stack. *)
                    val newPushList =
                        case (tabEnt, saveEnt) of
                            (StackW _, Register _) => entry :: pushList
                        |   (StackW _, Direct _) => entry :: pushList
                        |   (Register _, Direct _) => entry :: pushList
                        |   (Register tabReg, Register saveReg) => (* Was moved to a different reg. *)
                                if tabReg = saveReg then pushList else entry :: pushList
                        |   (StackW _, StackW _) => pushList
                        |   (Literal _, Literal _) => pushList
                        |   (CodeRef _, CodeRef _) => pushList
                        |   (Direct _, Direct _) => pushList
                        |   (Container _, Container _) => pushList
                        |   _ => raise InternalError "Funny entries"
                in
                    processTables(entry+1, newCacheSet, newPushList)
                end
            |   (NoStackEntry, StackEntry{cache, ...}) =>
                (
                    (* We could have entries that started out being cached but which were
                       subsquently cleared out.  We add them here if only to indicate that
                       we need to reprocess the loop. *)
                    processTables(entry+1, regSetUnion(cacheSet, cache), pushList)
                )
            |   _ => processTables(entry+1, cacheSet, pushList)
    in
        processTables(first, noRegisters, [])
    end

    (* Restore the state if we're reprocessing a loop. *)
    fun restoreLoopState(table as Ttab{branched, exited, pstackptr, ...}, state, cacheSet, pushes: loopPush list) =
    let
        (* We're restoring the state to what it was before the loop and we don't generate
           the previous loop code so we can simply discard the "free" instructions here. *)
        val (_, _) = setState (state, table, NoMerge, ! pstackptr, false, true)
        (* Remove entries from the cache if they've been changed in the loop. *)
        val _ = removeFromCache(table, cacheSet, fn () => true);
        (* Push items to the stack that are pushed in the loop.  Don't move them into other
           registers but allow them to be cached. *)
        val pushes = pushRegisters(table, allRegisters, fn e => List.exists(fn e' => e=e') pushes, false, false);
        val () = exited := false;
        val () = branched := false;
    in
        pushes
    end

    (* These are exported as read-only. *)
    fun maxstack(Ttab{maxstack=ref maxstackVal, ...}) = maxstackVal
    fun realstackptr(Ttab{realstackptr=ref realstackVal, ...}) = realstackVal
    fun haveExited(Ttab{exited=ref exitedVal, ...}) = exitedVal

    (* This is called when we have either made a tail-recursive call,
     returned from a function or raised an exception. *)
    fun exiting(Ttab{branched, exited, ...}) =
    (
        branched := true;
        exited := true
    )

    (* Put the arguments and closure/static link register onto the pseudo-stack.
       If the lifeTime is zero the parameter/closure is never used and we don't need
       to do anything. *)
    fun parameterInRegister(reg, lifeTime, transtable as Ttab{pstackTrace, printStream, ...}) =
    if lifeTime > 0
    then
    let
        val code = getRegister (transtable, reg)
        (* Code is only generated if we have to save something to get the register
           and that shouldn't happen here. *)
        val () = case code of [] => () | _ => raise InternalError "registerArg: non-empty code"
        val addrInd  = pushReg (transtable, reg)
        val () = setLifetime(transtable, addrInd, lifeTime)
    in
        if pstackTrace
        then
           (
            printStream "parameterInRegister: locn=";
            printStream(Int.toString addrInd);
            printStream " lifeTime=";
            printStream(Int.toString lifeTime);
            printStream "\n"
            )
        else ();
        addrInd
    end
    else noIndex

    (* Check that the only item on the stack after the block is the result.
       We could have removed items and replaced them with something else.
       We could have pushed values that were previously in registers.
       This is purely validation and could be removed. *)
    fun checkBlockResult(table as Ttab{pstackptr, marker, ...}, result) =
    let
        fun checkStack entry =
        if entry = ! pstackptr
        then ()
        else
        (
            case pstackEntry(table, entry) of
                NoStackEntry => ()
            |   StackEntry {uses = 0, ...} => () (* May be a cache entry. *)
            |   StackEntry _ =>
                if (case result of NoMerge => true | MergeIndex m => m <> entry)
                (* Comment out for the moment.  Container entries from mutually
                   recursive stack closures will never get their use counts zero
                   so they and their stack entries will not be removed. *)
                then () (*print "checkBlockResult: Entry not removed\n"*)
                else ();
            checkStack(entry+1)
        )
    in
        checkStack(!marker)
    end

    datatype constEntry = ConstLit of machineWord | ConstCode of code | NotConst

    fun isConstant(entry, table) =
        case pstackRealEntry(table, entry) of
            {ent = Literal l, ...} => ConstLit l
        |   {ent = CodeRef c, ...} => ConstCode c
        |   _ => NotConst

    fun isRegister(entry, table) =
        case pstackRealEntry(table, entry) of
            {ent = Register(reg, _), ...} => SOME reg
        |   _ => NONE

    fun isContainer(entry, table) =
        case pstackRealEntry(table, entry) of
            {ent = Container _, ...} => true
        |   _ => false

    (* Create a closure on the stack.  There may be entries in the list which have
       not yet been set. *)
    fun createStackClosure(table, entries) =
    let
        (* If any entries are containers they need to be added to the dependencies. *)
        local
            fun depFold (index, (code, deps)) =
                if index = noIndex
                then (code, deps)
                else case pstackEntry(table, index) of
                        StackEntry { ent = Container _, ...} =>
                            (incrUseCount(table, index, 1) @ code, index :: deps)
                    |   _ => (code, deps)
        in
            val (incCode, depends) = List.foldl depFold ([], []) entries
        end

        (* We must first make sure that the space we're going to allocate
           hasn't been reserved for a register. *)
        val alignCode = alignStack(table, [], NONE)
        (* There's a potential problem if some of the entries are base+offset.  We
           need to load them into a register before we can push them but that
           may involve pushing a register to get a free one.  Make sure there
           is at least one free register. *)
        val (aReg, getRegCode) = getAnyRegister table
        val freeCode = freeRegister(table, aReg)

        (* Push all entries.  There may be entries that refer to other
           closures in the same mutually recursive set.  Use a zero for
           these and fill them in later. *)
        fun pushEntry(index :: indices) =
            let
                (* Push the later entries first. *)
                val (pushEntries, pushTail) = pushEntry indices

                val sp = realstackptr table
                val indexOrDummy =
                    if index = noIndex
                    then (* Recursive entry. *) pushConst (table, toMachineWord 0)
                    else index
                val (pushedEntry, pushThis) = pushValueToStack (table, indexOrDummy, sp + 1)
            in
                (pushedEntry :: pushEntries, pushThis @ pushTail)
            end
        |  pushEntry [] = ([], [])

        val (fillEntries, fillCode) = pushEntry entries

        val container =
            pushPstack(table, Container{items=fillEntries, dependencies=depends}, "createStackClosure", fillCode)
    in
        (container, fillCode @ freeCode @ getRegCode @ alignCode @ incCode)
    end

    (* If we are creating mutually recursive closures on the stack we
       will have to complete the loop by updating earlier closures
       with the addresses of later ones. *)
    (* TODO: This effectively prevents the closures from ever being
       removed.  If we have two mutually recursive closures then the
       use-count of neither will ever go to zero. *)
    fun setRecursiveClosureEntry(vecEntry, valueEntry, offset, table) =
    let
        (* This is a dependency of the container. *)
        val stackent = pstackEntry(table, vecEntry)
        (* Increment the use count so it's not thrown away in moveToVec but
           instead add it to the dependencies of the container. *)
        val incCode = incrUseCount(table, valueEntry, 1)
        val _ =
            case stackent of
                StackEntry{ ent = Container{items, dependencies}, uses, cache, destStack, lifeTime} =>
                    setPstackEntry(table, vecEntry,
                        StackEntry{ ent=Container{items=items, dependencies = valueEntry:: dependencies},
                                    uses=uses, cache=cache, destStack=destStack, lifeTime=lifeTime})
            |   _ => raise InternalError "setRecursiveClosureEntry: not container"
    in
        moveToVec(vecEntry, valueEntry, offset, table) @ incCode
    end

   structure Sharing =
    struct
        type code       = code
        and  negotiation = negotiation
        and  reg        = reg
        and  negotiateTests = negotiateTests
        and  addrs      = addrs
        and  operation  = operation 
        and  machineWord = machineWord
        and  ttab       = ttab
        and  savedState = savedState
        and  regSet     = regSet
        and  stackIndex = stackIndex
        and  stackMark  = stackMark
        and  labels     = labels
        and  handler    = handler
        and  regHint    = regHint
        and  argdest    = argdest
        and  loopPush   = loopPush
        and  forwardLabel = forwardLabel
        and  backwardLabel = backwardLabel
        and  constEntry = constEntry
    end

end;

