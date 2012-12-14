(*
    Copyright (c) 2012 David C.J. Matthews

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

functor CODETREE_OPTIMISER(
    structure BASECODETREE: BaseCodeTreeSig

    structure CODETREE_FUNCTIONS: CodetreeFunctionsSig

    structure REMOVE_REDUNDANT:
    sig
        type codetree
        type loadForm = { addr : int, level: int, fpRel: bool }
        val cleanProc : (codetree * (loadForm * int * int -> codetree) * int * bool array) -> codetree
        structure Sharing: sig type codetree = codetree end
    end

    structure DEBUG :
    sig
        val codetreeTag:            bool Universal.tag (* If true then print the original code. *)
        val maxInlineSizeTag:       int  Universal.tag
        val getParameter : 'a Universal.tag -> Universal.universal list -> 'a
    end

    structure PRETTY : PRETTYSIG

    structure BACKEND:
    sig
        type codetree
        type machineWord = Address.machineWord
        val codeGenerate: codetree * int * Universal.universal list -> unit -> machineWord
        structure Sharing : sig type codetree = codetree end
    end

    sharing 
        BASECODETREE.Sharing 
    =   CODETREE_FUNCTIONS.Sharing
    =   REMOVE_REDUNDANT.Sharing
    =   PRETTY.Sharing
    =   BACKEND.Sharing

) :
    sig
        type codetree and optVal
        val codetreeOptimiser: codetree  * Universal.universal list -> optVal * int
        structure Sharing: sig type codetree = codetree and optVal = optVal end
    end
=
struct
    open BASECODETREE
    open Address
    open CODETREE_FUNCTIONS
    open StretchArray
    
    infix 9 sub
    
    exception InternalError = Misc.InternalError

    val isConstnt    = fn (Constnt _)    => true | _ => false

    type loadForm = 
    { (* Load a value. *)
        addr : int, 
        level: int, 
        fpRel: bool
    }

    (* gets a value from the run-time system *)
    val ioOp : int -> machineWord = RunCall.run_call1 RuntimeCalls.POLY_SYS_io_operation;

    val word0 = toMachineWord 0;
    val word1 = toMachineWord 1;
  
    val False = word0;   
    val True  = word1;   

    (* since code generator relies on these representations,
       we may as well export them *)
    val rtsFunction = mkConst o ioOp

    fun mkGenLoad  (i1, i2, bl) =
        Extract {addr  = i1, level = i2, fpRel = bl}

(************************************************************************)

  val initTrans = 10; (* Initial size of arrays. *)

(*****************************************************************************)
(*                  changeLevel                                              *)
(*****************************************************************************)
  (* Change the level of an entry if necessary. This *)
  (* happens if we have a function inside an inline function. *)
  fun changeLevel entry 0 = entry (* No change needed*)
   | changeLevel entry correction =
      let
      fun changeL(ext as Extract{addr, level, fpRel, ...}, nesting) =
            if level >= 0 andalso level < nesting
                (* We enter declarations with level = ~1 for recursive
                   calls when processing mutual recursion. *)
            then ext (* It's local to the function(s) we're processing. *)
            else mkGenLoad (addr, level + correction, fpRel)

        | changeL(Lambda{body, isInline, name, closure, argTypes, resultType,
                         level, localCount, ...}, nesting) =
            Lambda{body = changeL(body, nesting+1), isInline = isInline,
                   name = name, closure = closure, argTypes = argTypes, resultType=resultType,
                   level = level + correction, localCount=localCount}

        | changeL(Eval{function, argList, resultType}, nesting) =
            Eval{function = changeL(function, nesting),
                 argList = map (fn (a, t) => (changeL(a, nesting), t)) argList,
                 resultType=resultType}

        | changeL(Indirect{ base, offset }, nesting) =
            Indirect{base = changeL(base, nesting), offset = offset }

        | changeL(Newenv(decs,exp), nesting) =
            let
                fun changeLDec(Declar{value, addr, ...}, nesting) = mkDec(addr, changeL(value, nesting))
                |   changeLDec(NullBinding exp, nesting) = NullBinding(changeL(exp, nesting))
                |   changeLDec _ = raise InternalError "changeLDec: Unknown code"
            in
                Newenv(map(fn d => changeLDec(d, nesting)) decs, changeL(exp, nesting))
            end

        | changeL(c as Container _, _) = c

        | changeL(TupleFromContainer(container, size), nesting) =
            TupleFromContainer(changeL(container, nesting), size)

        | changeL(_, _) =
            (* The code we produce in these inline functions is very limited. *)
               raise InternalError "changeL: Unknown code"

    in
        case optGeneral entry of
            gen as Extract _ =>
                optVal 
                {
                    general = changeL(gen, 0),
                    special = optSpecial entry,
                    environ = optEnviron entry,
                    decs    = []
                }

        |   Constnt _ => entry

        |   gen as Lambda _ =>
                optVal
                {
                    general = changeL(gen, 0),
                    special = optSpecial entry,
                    environ = optEnviron entry,
                    decs    = []
                }
            
        | _ => raise InternalError "changeLevel: Unknown entry"
    end
  (* end changeLevel *)

    (* Code-generate a function or set of mutually recursive functions that contain no free variables
       and run the code to return the address.  This allows us to further fold the address as
       a constant if, for example, it is used in a tuple. *)
    local
        exception Interrupt = Thread.Thread.Interrupt
    in
        fun codeGenerateToConstant debugSwitches (pt, localCount) =
        let
            val () =
                if DEBUG.getParameter DEBUG.codetreeTag debugSwitches
                then PRETTY.getCompilerOutput debugSwitches (BASECODETREE.pretty pt) else ()

            val code = BACKEND.codeGenerate(pt, localCount, debugSwitches)
        in
            Constnt (code()) (* Evaluate it and convert any exceptions into Raise instrs. *)
                handle Interrupt => raise Interrupt (* Must not handle this *)
                | exn => Raise (Constnt(toMachineWord exn))
        end
    end

    (* Call and RTS function to fold constants.  The function must be safe to evaluate "early". *)
    fun callRTSFunction(rtsCall: machineWord, argList: machineWord list) =
    let
        exception Interrupt = Thread.Thread.Interrupt
        val _ = (isIoAddress(toAddress rtsCall) andalso earlyRtsCall rtsCall)
                    orelse raise InternalError "not early rts"

        (* Turn the arguments into a vector.  *)
        val argVector = Vector.fromList argList

        (* Call the function.  If it raises an
           exception pass back the exception packet.  We've got a problem
           here if the code happens to raise Interrupt.  We assume that
           Interrupt can only occur through user intervention during
           compilation rather than as a result of executing the code.
           It would be better to use the Thread.Thread functions to
           mask off interrupts. *)
    in
        Constnt (call(toAddress rtsCall, toMachineWord argVector))
            handle exn as Interrupt => raise exn (* Must not handle this *)
            | exn as Misc.InternalError _ => raise exn
            | exn => Raise (Constnt(toMachineWord exn))
    end

    (* Optimiser context.  The optimiser may add or remove bindings so the "name" space
       is different for codetree elements before and after processing.  The "Old" environment
       is the space for addresses before processing and the "New" environment the space
       for addresses after processing.  Normally items are looked up in the "Old" environment
       but when we create a "special" entry the context (environ) for it may be the Old or
       New lookup function depending on whether we use the unprocessed or processed
       codetree as the "special" entry. *)
    type nameEnv = 
    { 
        (* Look-up.  This takes a "load" entry as used in Extract, the nesting depth of
           the point where the result value will be used and the "level" count in
           the loadForm. *)
        lookup: (*{ load: loadForm, callerNesting: int, level: int }*) loadForm * int * int -> optVal,
        enter: int * optVal -> unit
    }
    
    fun errorEnter (_:int * optVal): unit = raise InternalError "errorEnter"

    type optContext =
    {
         oldEnv: nameEnv,
         newEnv: nameEnv,
         nestingOfThisFunction : int,
         spval : int ref,
         recursiveExpansions:
            ((codetree * argumentType) list * bool * int -> (codetree * argumentType) list) option,
         loopFilter: (codetree * argumentType) list -> (codetree * argumentType) list,
         debugArgs: Universal.universal list,
         inlineExpansionDepth: int
    }

    fun loopFilterError _ = raise InternalError "Loop instruction without BeginLoop"

    fun getGeneral ov =
        (
        case optDecs ov of
          []   => optGeneral ov
        | decs => mkEnv (decs, optGeneral ov)
        )

    (* Replace the decs in a value.  Do this in a way that avoids creating unnecessary garbage. *)
    fun repDecs [] (gen as JustTheVal _) = gen
    |   repDecs [] (ValWithDecs {general, ...}) = JustTheVal general
    |   repDecs decs (JustTheVal gen) = ValWithDecs {general = gen, decs = decs}
    |   repDecs decs (ValWithDecs {general, ...}) = ValWithDecs {general = general, decs = decs}
    |   repDecs decs (OptVal{general, special, environ, ...}) =
            OptVal{general=general, special=special, environ=environ, decs=decs}

    val stripDecs = repDecs []

    (* Prepares a binding for entry into a look-up table.  Returns the entry
       to put into the table together with any bindings that must be made.
       If the general part of the optVal is a constant we can just put the
       constant in the table. If it is a load (Extract) it is just renaming
       an existing entry so we can return it.  Otherwise we have to make
       a new binding and return a load (Extract) entry for it. *)
    fun makeNewDecl(ins, { spval, ...}: optContext) =
        case optGeneral ins of
            Constnt _ =>
                (* No need to create a binding for a constant. *)
                (stripDecs ins, optDecs ins)

        |   Extract { level = 0, ...} =>
                (* Declaration is simply giving a new name to a local
                   - can ignore this declaration. *) 
                (stripDecs ins, optDecs ins)

        |   gen =>
            let (* Create a binding for this value. *)
                val decSpval = ! spval before (spval := !spval + 1)
            
                (* The table entry is similar to the result of the expression except
                    that the declarations are taken off and put into the containing
                    block, and the general value is put into a local variable and
                    replaced by an instruction to load from there. If the special
                    is a non-inline function it is removed. *)              
                val optV = 
                    optVal 
                    {
                        general = mkLoad (decSpval, 0),
                        special = optSpecial ins,
                        environ = optEnviron ins,
                        decs    = []
                    }
            in
                (optV, optDecs ins @ [mkDec(decSpval, gen)])
            end

    (* Handle an Indirect node or the equivalent in a variable tuple. *)
    fun doIndirection(source, offset,
            {nestingOfThisFunction,  spval, recursiveExpansions, loopFilter, debugArgs, 
             inlineExpansionDepth, ... }) =
        case (optSpecial source, optGeneral source) of
          (SOME(spec as Recconstr _), _) =>
            let
                (* The "special" entry we've found is a tuple.  That means that
                   we are taking a field from a tuple we made earlier and so we
                   should be able to get the original code we used when we made
                   the tuple.  That might mean the tuple is never used and
                   we can optimise away the construction of it completely. The
                   entry we get back from findEntryInBlock will either be a
                   constant or a load.  In that case we need to look it up in
                   the environment we used for the record to give us an optVal.
                   The previous code in newDecl, commented out by AHL, also put
                   indirections into the table.  To save having the various cases
                   in here we simply call "optimise" which will deal with them. *)
                val newCode =
                 optimise(findEntryInBlock spec offset, false,
                    {
                    newEnv = { lookup = errorEnv, enter = errorEnter }, (* We must always look up old addresses. *)
                    oldEnv = { lookup = optEnviron source, enter = errorEnter },
                    nestingOfThisFunction= nestingOfThisFunction,
                    spval= spval,
                    recursiveExpansions= recursiveExpansions,
                    loopFilter= loopFilter,
                    debugArgs= debugArgs,
                    inlineExpansionDepth = inlineExpansionDepth})
            in
                repDecs (optDecs source @ optDecs newCode) newCode
            end

        | (_ , gen as Constnt _ ) => (* General is a constant -  Do the selection now. *)
            simpleOptVal(findEntryInBlock gen offset)
                           
        | (_, _) => (* No special case possible. *)
            simpleOptVal(mkInd (offset, optGeneral source))

     (* The main optimisation routine. *)
    (* Returns only the general value from an expression. *)
    and generalOptimise(pt, tailCall, context) = getGeneral(optimise(pt, tailCall, context))

    and general context pt = generalOptimise(pt, false, context)

    and optimise (pt as MatchFail, _, _) = simpleOptVal pt

     |  optimise (AltMatch(a, b), _, context) =
            simpleOptVal(AltMatch(general context a, general context b))
       
     |  optimise (Eval {function, argList, resultType}, tailCall,
                    context as {nestingOfThisFunction, recursiveExpansions, inlineExpansionDepth, ...}) =
        let
            (* Function call - This may involve inlining the function. *)

            (* Get the function to be called and see if it is inline or
               a lambda expression. *)
            val funct : optVal = optimise(function, false, context)
            
(*            val () = case (function, optGeneral funct) of
                (Extract{addr=0, fpRel = false, level = fLevel, ...}, Extract{addr=0, fpRel = false, level = gLevel, ...}) =>
                    if fLevel = gLevel then () else print "different levels\n"
            |   (Extract{addr=0, fpRel = false, ...}, _) =>
                    raise InternalError "recursive before but not after\n"
            |   (_, Extract{addr=0, fpRel = false, ...}) =>
                    print "recursive after but not before\n"
            |   _ => ()*)
 
            (* There are essentially two cases to consider - the function
               may be "inline" in which case it must be expanded as a block,
               or it must be called. *)
        in
            case (function, optSpecial funct, optGeneral funct) of
                (Extract{addr=0, fpRel = false, level = 0, ...}, _, _) =>
                     (* We're already expanding this function - don't recursively expand
                        it.  Either loop or generate a function call. *)
                let
                    val copiedArgs = map (fn (arg, argType) => (general context arg, argType)) argList
                    val evCopiedCode = 
                        case recursiveExpansions of
                            (* This is a recursive call to a function we're expanding.
                               Is it tail recursive?  We may have several levels of
                               expansion. *)
                            SOME filterArgs =>
                                if tailCall
                                then Loop (filterArgs(copiedArgs, true, nestingOfThisFunction))
                                else Eval {function = optGeneral funct,
                                        (* N.B. We still need to use "filterArgs" even if this is a call. *)
                                        argList = filterArgs(copiedArgs, false, nestingOfThisFunction),
                                        resultType=resultType}
                            (* Not a recursive expansion. *)
                        |   NONE => Eval {function = optGeneral funct, argList = copiedArgs, resultType=resultType}
                in
                    repDecs (optDecs funct) (simpleOptVal evCopiedCode)
                end

            |   (Extract{addr=0, fpRel = false, ...}, _, _) =>
                    (* Recursive call to a function at an outer level.  Cannot loop but
                       we mustn't expand it inline. *)
                let
                    val copiedArgs = map (fn (arg, argType) => (general context arg, argType)) argList
                    val evCopiedCode =
                        Eval {function = optGeneral funct, argList = copiedArgs, resultType=resultType}
                in
                    repDecs (optDecs funct) (simpleOptVal evCopiedCode)
                end

            |   (_, SOME(Lambda (lambda as { isInline = MaybeInline, ...})), gen) =>
                    if inlineExpansionDepth < 5
                    then inlineInlineOnlyFunction(funct, lambda, argList, tailCall, context)
                    else
                    let
                        val copiedArgs = map (fn (arg, argType) => (general context arg, argType)) argList
                        val evCopiedCode = 
                            Eval {function = gen, argList = copiedArgs, resultType=resultType}
                    in
                        repDecs (optDecs funct) (simpleOptVal evCopiedCode)
                    end

            |   (_, SOME(Lambda (lambda as { isInline = OnlyInline, ...})), _) =>
                    inlineInlineOnlyFunction(funct, lambda, argList, tailCall, context)

            |   (_, SOME(Lambda lambda), gen) =>
                    if inlineExpansionDepth < 5
                    then inlineSmallFunction(funct, lambda, argList, resultType, context)
                    else
                    let
                        val copiedArgs = map (fn (arg, argType) => (general context arg, argType)) argList
                        val evCopiedCode = 
                            Eval {function = gen, argList = copiedArgs, resultType=resultType}
                    in
                        repDecs (optDecs funct) (simpleOptVal evCopiedCode)
                    end

            | (_, _, gen as Constnt w) => (* Not inlinable - constant function. *)
                let
                    val copiedArgs = map (fn (arg, argType) => (general context arg, argType)) argList
                    (* If the function is an RTS call that is safe to evaluate immediately and all the
                       arguments are constants evaluate it now. *)
                    val evCopiedCode = 
                        if isIoAddress(toAddress w) andalso earlyRtsCall w andalso List.all (isConstnt o #1) copiedArgs
                        then callRTSFunction(w, List.map(fn (Constnt w, _) => w | _ => raise InternalError "not const") copiedArgs)
                        else Eval {function = gen, argList = copiedArgs, resultType=resultType}
                in
                    repDecs (optDecs funct) (simpleOptVal evCopiedCode)
                end

            | (_, _, gen) => (* Anything else. *)
                let
                    val copiedArgs = map (fn (arg, argType) => (general context arg, argType)) argList
                    val evCopiedCode = 
                        Eval {function = gen, argList = copiedArgs, resultType=resultType}
                in
                    repDecs (optDecs funct) (simpleOptVal evCopiedCode)
                end

        end (* Eval { } *)
        
     |  optimise (Extract(ext as {level, ...}), _, {oldEnv = {lookup=lookupOldAddr, ...}, nestingOfThisFunction, ...}) =
            lookupOldAddr (ext, nestingOfThisFunction, level)

     |  optimise (original as Lambda({isInline=OnlyInline, ...}), _, { oldEnv = {lookup=lookupOldAddr, ...}, ...} ) =
            (* Used only for functors.  Leave unchanged. *)
            optVal 
                {
                  general = CodeZero,
                  (* Changed from using CodeNil here to CodeZero.  This avoids a problem
                     which only surfaced with the changes to ML97 and the possibility of
                     mixing functor and value declarations in the same "program" (i.e.
                     top-level declarations with a terminating semicolon.
                     OnlyInline is used for functors which can only ever be called,
                     never passed as values, so the "general" value is not really
                     required.  It can though appear in the result tuple of the "program"
                     from which the (value) results of the program are extracted.

                     Further change: This previously returned the processed body but the effect of
                     that was to prevent small functions inside the functor from becoming
                     inline functions when the functor was applied.  Instead we just return
                     the original code. *)
                  special = SOME original,
                  environ = lookupOldAddr, (* Old addresses with unprocessed body. *)
                  decs    = []
                }

     |  optimise (original as Lambda({body=lambdaBody, isInline=lambdaInline, name=lambdaName,
                          argTypes, resultType, ...}), _,
                 { nestingOfThisFunction, oldEnv = {lookup=lookupOldAddr, ...}, newEnv = {lookup=lookupNewAddr, ...},
                   debugArgs, spval, inlineExpansionDepth, ... }: optContext) =
        let
            (* The nesting of this new function is the current nesting level
             plus one. Normally this will be the same as lambda.level except
             when we have a function inside an inline function. *)
            val nesting = nestingOfThisFunction + 1;
          
            (* A new table for the new function. *)
            val oldAddrTab = stretchArray (initTrans, NONE)
            and newAddrTab = stretchArray (initTrans, NONE)

            local
                fun localOldAddr ({ addr=index, level, ...}, depth, 0) =
                    (* local declaration or argument. *)
                    if index > 0 
                    (* Local declaration. *)
                    then
                    case  oldAddrTab sub index of
                        SOME v => changeLevel v (depth - nesting)
                    |   NONE => raise InternalError(
                                        concat["localOldAddr: Not found. Addr=", Int.toString index,
                                               " Level=", Int.toString level])
                    (* Argument or closure. *)
                    else simpleOptVal (mkGenLoad (index, depth - nesting, index <> 0))
                |   localOldAddr (ptr, depth, levels) = lookupOldAddr (ptr, depth, levels - 1)
                and setTab (index, v) = update (oldAddrTab, index, SOME v)
            in
                val oldEnv = { lookup = localOldAddr, enter = setTab }
            end

            local
                (* localNewAddr is used as the environment of inline functions within
                   the function we're processing.  All the entries in this table will
                   have their "general" entries as simply Extract entries with the
                   original address.  Their "special" entries may be different. The
                   only entries in the table will be those which correspond to
                   declarations in the original code so there may be new declarations
                   which aren't in the table. *)
                fun localNewAddr ({ addr=index, ...}, depth, 0) =
                    if index > 0 
                    then case newAddrTab sub index of
                        NONE => (* Return the original entry if it's not there. *)
                            simpleOptVal(mkGenLoad (index, depth - nesting, true))
                    |   SOME v => changeLevel v (depth - nesting) 
                    else simpleOptVal (mkGenLoad (index, depth - nesting, index <> 0))
                |   localNewAddr (ptr, depth, levels) = lookupNewAddr (ptr, depth, levels - 1);

                fun setNewTab (addr, v) = update (newAddrTab, addr, SOME v)
            in
                val newEnv = {lookup=localNewAddr, enter=setNewTab}
            end

            val newAddressAllocator = ref 1      val newCode =
                optimise (lambdaBody, false,
                {
                newEnv=newEnv,
                oldEnv=oldEnv,
                nestingOfThisFunction=nesting,
                spval=newAddressAllocator,
                recursiveExpansions=NONE,
                loopFilter=loopFilterError,
                debugArgs=debugArgs,
                inlineExpansionDepth=inlineExpansionDepth})

          (* nonLocals - a list of the non-local references made by this
             function.  If this is empty the function can be code-generated
             immediately and returned as a constant.  If it is non-empty it
             is set as the closure for the function.  This is then used
             when processing mutually recursive functions to find the
             dependencies. *)
             
          val nonLocals = ref nil;
          fun addNonLocal({addr, level, fpRel, ...}, depth) =
          let
             (* The level will be correct relative to the use, which may be
                in an inner function.  We want the level relative to the
                scope in which this function is declared. *)
             val correctedLevel = level - (depth - nestingOfThisFunction)
             fun findNonLocal(Extract{addr=addr', level=level', fpRel=fpRel', ...}) =
                    addr = addr' andalso correctedLevel = level' andalso fpRel=fpRel'
              |  findNonLocal _ = raise InternalError "findNonLocal: not Extract"
          in
             if List.exists findNonLocal (!nonLocals)
             then () (* Already there. *)
             else nonLocals := mkGenLoad(addr, correctedLevel, fpRel) :: ! nonLocals
          end

          fun checkRecursion(ext as {fpRel=oldfpRel, ...}, levels, depth) =
              case optGeneral(lookupNewAddr (ext, depth, levels)) of
                 (res as Extract(ext as {addr=0, fpRel=false, ...})) =>
                     (
                     (* If this is just a recursive call it doesn't count
                        as a non-local reference.  This only happens if
                        we turned a reference to a local into a recursive
                        reference (i.e. fpRel was previously true). *)
                     if levels = 0 andalso oldfpRel
                     then ()
                     else addNonLocal(ext, depth);
                     res
                     )
              |  res as Extract ext =>
                    (
                     addNonLocal(ext, depth);
                     res
                    )

              |  res => res (* We may have a constant in this table. *)

          val cleanedBody =
            REMOVE_REDUNDANT.cleanProc(getGeneral newCode, checkRecursion, nesting,
                      Array.array (! newAddressAllocator + 1, false))

          val resultCode =
            case lambdaInline of
                MaybeInline => (* Explicitly inlined functions. *)
                    (* We return the processed version of the function as
                       the general value but the unprocessed version as
                       the special value. *)
                    optVal 
                    {
                      general =
                        Lambda 
                          {
                           body          = cleanedBody,
                           isInline      = MaybeInline,
                           name          = lambdaName,
                           closure       = !nonLocals, (* Only looked at in MutualDecs. *)
                           argTypes      = argTypes,
                           resultType    = resultType,
                           level         = nesting,
                           localCount    = ! newAddressAllocator + 1
                         },
                      special = SOME original,
                      environ = lookupOldAddr, (* Old addresses with unprocessed body. *)
                      decs    = []
                    }

            |   _ => (* "Normal" function.  If the function is small we mark it as
                        inlineable.  If the body has no free variables we compile it
                        now so that we can propagate the resulting constant, otherwise
                        we return the processed body.  We return the processed body as
                        the special value so that it can be inlined.  We do this even
                        in the case where the function isn't small because it is just
                        possible we're going to apply the function immediately and in
                        that case it's worth inlining it anyway. *)
              let
                    val inlineType =
                        if lambdaInline = NonInline
                        then if (* Is it small? *) codeSize(cleanedBody, true) < 
                                DEBUG.getParameter DEBUG.maxInlineSizeTag debugArgs
                        then SmallFunction else NonInline
                        else lambdaInline

                  val copiedLambda =
                    Lambda 
                      {
                       body          = cleanedBody,
                       isInline      = inlineType,
                       name          = lambdaName,
                       closure       = !nonLocals, (* Only looked at in MutualDecs. *)
                       argTypes      = argTypes,
                       resultType    = resultType,
                       level         = nesting,
                       localCount    = ! newAddressAllocator + 1
                     };
                 val general = 
                   (* If this has no free variables we can code-generate it now. *)
                   if null (!nonLocals)
                   then codeGenerateToConstant debugArgs (copiedLambda, !spval+1)
                   else copiedLambda
              in
                  optVal 
                    {
                      general = general,
                      special =
                        (* If this function may be inlined include it in the special entry
                           otherwise return CodeNil here. *)
                        if inlineType = NonInline
                        then NONE
                        else
                        SOME(Lambda 
                          {
                           body          = cleanedBody,
                           isInline      = inlineType,
                           name          = lambdaName,
                           closure       = [],
                           argTypes      = argTypes,
                           resultType    = resultType,
                           level         = nesting,
                           localCount    = ! newAddressAllocator + 1
                         }),
                      environ = lookupNewAddr,
                      decs    = []
                    }
            end
        in
            StretchArray.freeze oldAddrTab;
            StretchArray.freeze newAddrTab;
            resultCode
        end (* Lambda{...} *)
           
     |  optimise (pt as Constnt _, _, _) =
            simpleOptVal pt (* Return the original constant. *)
           
     |  optimise (BeginLoop{loop=body, arguments=args, ...}, tailCall,
              context as { newEnv, oldEnv as {enter = enterOldDec, ...}, nestingOfThisFunction, spval,
                            recursiveExpansions, debugArgs, inlineExpansionDepth, ...}) =
        let
             (* We could try extracting redundant loop variables but for
                the time being we just see whether we actually need a loop
                or not.  This is needed if we have already constructed a loop
                from a recursive inline function and then expand it in another
                function.  If some of the loop variables are now constants we may
                optimise away the loop altogether. e.g. equality for lists where
                we actually have   if x = nil then ... *)
             val loops = ref false
             fun filterArgs a = (loops := true; a)

             (* First process as though it was not a BeginLoop but just a
                set of declarations followed by an expression. *)
             val _ =
                optimise
                  (mkEnv(List.map (Declar o #1) args, body), true,
                  {newEnv = newEnv, oldEnv = oldEnv,
                   nestingOfThisFunction=nestingOfThisFunction,
                   spval=spval,
                   recursiveExpansions=recursiveExpansions,
                   loopFilter=filterArgs,
                   debugArgs=debugArgs,
                   inlineExpansionDepth=inlineExpansionDepth})
        in
            if not (! loops)
            then (* The Loop instructions have been optimised away.  Since there's
                    no BeginLoop we can reprocess it with the surrounding
                    tail recursion. *)
                optimise(mkEnv(List.map (Declar o #1) args, body), tailCall, context)
            else (* It loops - have to reprocess. *)
            let
                (* The arguments to the functions are Declar entries but they
                   must not be optimised. *)
                fun declArg({addr, value, ...}, typ) =
                    let
                        val optVal = optimise(value, false, context)
                        val decSpval = ! spval
                        val _ = spval := decSpval + 1
                        val optV = simpleOptVal(mkLoad (decSpval, 0))
                    in
                        enterOldDec(addr, optV);
                        ({addr = decSpval, value = getGeneral optVal}, typ)
                    end
                 val declArgs = map declArg args
                 val beginBody =
                    optimise(body, true,
                      {newEnv = newEnv, oldEnv = oldEnv,
                       nestingOfThisFunction=nestingOfThisFunction, spval=spval,
                       recursiveExpansions=recursiveExpansions,
                       loopFilter=filterArgs,
                       debugArgs=debugArgs,
                       inlineExpansionDepth=inlineExpansionDepth})
            in
                simpleOptVal (BeginLoop {loop=getGeneral beginBody, arguments=declArgs})
            end
        end

     |  optimise (Loop args, _, context as { loopFilter, ... }) =
            (* The Loop instruction should be at the tail of the
               corresponding BeginLoop. *)
            let
                fun gen(c, t) = (general context c, t)
            in
                simpleOptVal (Loop (loopFilter(map gen args)))
            end
          
     |  optimise (Raise x, _, context) = simpleOptVal (Raise (general context x))
         
     |  optimise (Cond(condTest, condThen, condElse), tailCall, context as { spval, nestingOfThisFunction, ...}) =
        let
            val insFirst = general context condTest
        in
            (* If the condition is a constant we need only
               return the appropriate arm. *)
            case insFirst of
                Constnt testResult =>
                    if wordEq (testResult, False) (* false - return else-part *)
                    then (* if false then x else y == y *)
                        optimise(condElse, tailCall, context)
                    (* if true then x else y == x *)
                    else optimise(condThen, tailCall, context)  (* return then-part *)
            
            |   _ => (* Condition is not a constant. *)
                let
                    (* Perhaps the "if" is really a simpler expression?
                       Unfortunately, we don't know whether we're returning
                       a boolean result here so we can't optimise to
                       andalso/orelse but we can at least look for the
                       case where both results are constants. *)
                    val insSecond = optimise(condThen, tailCall, context)
                    val insThird  = optimise(condElse, tailCall, context)

                    (* If we have tuples on both arms we can probably combine them.  If we have
                       a container on a branch we will have declared it at some point so we
                       have to remove that declaration and "lift" it outside the "if-". *)
                    fun combineTuples(containerAddr, thenAddr, elseAddr, thenRec, elseRec, size) =
                    let
                        val thenDecs = optDecs insSecond and elseDecs = optDecs insThird

                        fun replaceContainerDec([], _) =
                            raise InternalError "replaceContainerDec"
                         |  replaceContainerDec((hd as Declar{addr, ...})::tl, ad)=
                                if addr = ad
                                then
                                    (* Found the declaration. If we are using this
                                       container address we remove this declaration
                                       because we are using the container created outside
                                       the if-.  If we have containers on both branches
                                       and we are using a different container on this branch
                                       we replace the creation of the continer here with a
                                       reference to the outer container. *)
                                    if addr = containerAddr
                                    then tl
                                    else mkDec(addr, mkLoad(containerAddr, 0)) :: tl
                                else hd :: replaceContainerDec(tl, ad) 
                        | replaceContainerDec(hd :: tl, ad) =
                                hd :: replaceContainerDec(tl, ad)

                        fun createBranch(recEntries, decEntries, cAddr) =
                            case cAddr of
                                SOME ad => (* We have a container on that branch ... *)
                                    (* TODO: We add a CodeZero here to balance things up but
                                       really we have a NullBinding(SetContainer ...) at the
                                       end of the list which we could extract. *)
                                    mkEnv(replaceContainerDec(decEntries, ad), CodeZero)
                            |   NONE => 
                                    mkEnv(decEntries,
                                        mkSetContainer(
                                            mkLoad(containerAddr, 0), Recconstr recEntries,
                                            size))

                        val thenPart = createBranch(thenRec, thenDecs, thenAddr)
                        and elsePart = createBranch(elseRec, elseDecs, elseAddr)
                        (* The result is a block which declares the container, side-effects it
                           in the "if" and makes a tuple from the result.  If we're lucky
                           the resulting tuple will be optimised away. *)
                        (* This code is the same as that used to optimise TupleFromContainer
                           and is designed to allow us to optimise away the tuple creation
                           if we use the individual fields. *)
                        val baseAddr = !spval
                        val _ = spval := baseAddr + size
                        val specialDecs =
                            List.tabulate(size,
                                fn n => mkDec(n+baseAddr, mkInd(n, mkLoad(containerAddr, 0))))
                        val specialEntries = List.tabulate(size, fn n => mkLoad(n+baseAddr, 0))
                        fun env (l:loadForm, depth, _) : optVal =
                            changeLevel (simpleOptVal(Extract l)) (depth - nestingOfThisFunction)
                    in
                        optVal 
                            {
                              general = TupleFromContainer(mkLoad(containerAddr, 0), size),
                              special = SOME(Recconstr specialEntries),
                              environ = env,
                              decs    =
                                  mkDec(containerAddr, Container size) ::
                                  NullBinding(mkIf(insFirst, thenPart, elsePart)) :: specialDecs
                            }
                    end (* combineTuples *)
                in
                    (* Optimise various cases depending on what the then- and else-parts return. *)
                    case (optGeneral insSecond, optDecs insSecond, optGeneral insThird, optDecs insThird) of
                        (second as Constnt c2, [], third as Constnt c3, []) =>
                            (* The results of the then- and else-parts are just constants. *)
                            (* if x then y else y == (x; y) *)
                        if wordEq (c2, c3)
                        then if sideEffectFree insFirst
                        then insSecond
                        else (* Must put insFirst in decs, so it gets executed *)
                            repDecs [NullBinding insFirst] (simpleOptVal second)
                  
                        (* if x then true else false == x *)
                        else if wordEq (c2, True) andalso wordEq (c3, False)
                        then simpleOptVal insFirst
              
                        (* if x then false else y == not x *)
                        else if wordEq (c2, False) andalso wordEq (c3, True)
                        then simpleOptVal (mkNot insFirst)
              
                        else (* can't optimise *) simpleOptVal (mkIf (insFirst, second, third))

                    |   (Recconstr thenRec, _, Recconstr elseRec, _) =>
                        (* Both tuples - are they the same size?  They may not be if they
                           are actually datatypes. *)
                        if List.length thenRec = List.length elseRec
                        then (* We can transform this into an operation which creates space
                                on the stack, side-effects it and then picks up the result
                                from it. *)
                        let
                            val size = List.length thenRec (* = List.length elseRec *)
                            (* Create a new address for the container. *)
                            val containerAddr = let val ad = !spval in spval := ad + 1; ad end
                        in
                            combineTuples(containerAddr, NONE, NONE, thenRec, elseRec, size)
                        end
                        else (* Different sizes - use default. *)
                           simpleOptVal (mkIf (insFirst, getGeneral insSecond, getGeneral insThird))

                    |   (TupleFromContainer(Extract{addr=thenAddr,level=0,fpRel=true, ...}, thenSize), _,
                         TupleFromContainer(Extract{addr=elseAddr,level=0,fpRel=true, ...}, elseSize), _) =>
                        (* Have both been converted already.  If we are returning a tuple from
                           a container the container must be declared locally. *)
                        if thenSize = elseSize
                        then (* We can combine the containers.  We can't if these are actually
                                datatypes in which case they could be different sizes. *)
                        let
                            (* If we have already transformed this we will have a
                               declaration of a container somewhere in the list. *)
                            (* Use the address which has already been allocated for the else part.
                               That makes it easier for the subsequent pass to convert this into
                               a "case" if appropriate. *)
                            val containerAddr = elseAddr
                        in
                            combineTuples(containerAddr, SOME thenAddr, SOME elseAddr, [], [], thenSize)
                        end
                        else (* Different sizes - use default. *)
                           simpleOptVal (mkIf (insFirst, getGeneral insSecond, getGeneral insThird))

                    |   (TupleFromContainer(Extract{addr=thenAddr,level=0,fpRel=true, ...}, thenSize), _,
                         Recconstr elseRec, _) =>
                        (* The then-part has already been converted *)
                        if thenSize = List.length elseRec
                        then combineTuples(thenAddr, SOME thenAddr, NONE, [], elseRec, thenSize)
                        else (* Different sizes - use default. *)
                           simpleOptVal (mkIf (insFirst, getGeneral insSecond, getGeneral insThird))

                    |   (Recconstr thenRec, _,
                         TupleFromContainer(Extract{addr=elseAddr,level=0,fpRel=true, ...}, elseSize), _) =>
                        (* The else-part has already been converted *)
                        if elseSize = List.length thenRec
                        then
                            combineTuples(elseAddr, NONE, SOME elseAddr, thenRec, [], elseSize)
                        else (* Different sizes - use default. *)
                           simpleOptVal (mkIf (insFirst, getGeneral insSecond, getGeneral insThird))

                     |   _ => (* Not constants or records. *)
                         simpleOptVal (mkIf (insFirst, getGeneral insSecond, getGeneral insThird))
                end
        end (* Cond ... *)
         
     |  optimise (Newenv(envDecs, envExp), tailCall,
                  context: optContext as 
                    {oldEnv = {enter = enterOldDec, lookup = lookupOldAddr },
                     newEnv = { enter =  enterNewDec, ...}, 
                     spval, nestingOfThisFunction, debugArgs, ...}) =
        let
            (* Recurses down the list of declarations and expressions processing
               each, and then reconstructs the list on the way back. *)
            fun copyDeclarations []  =
                (* End of the list - process the result expression. *)
                    optimise(envExp, tailCall, context)

            |   copyDeclarations (Declar{addr=caddr, value, ...} :: vs) = 
                let
                    (* Add the declaration to the table. *)
                    val (optV, dec) = makeNewDecl(optimise(value, false, context), context)
                    val () = enterOldDec(caddr, optV)                  
                    (* Deal with the rest of the block. *)
                    val rest = copyDeclarations vs
                in
                    repDecs (dec @ optDecs rest) rest
                end

            |   copyDeclarations (RecDecs mutualDecs :: vs) = 
                (* Mutually recursive declarations. Any of the declarations may
                   refer to any of the others. They should all be lambdas.

                   The front end generates functions with more than one argument
                   (either curried or tupled) as pairs of mutually recursive
                   functions.  The main function body takes its arguments on
                   the stack (or in registers) and the auxiliary inline function,
                   possibly nested, takes the tupled or curried arguments and
                   calls it.  If the main function is recursive it will first
                   call the inline function which is why the pair are mutually
                   recursive.
                   As far as possible we want to use the main function since that
                   uses the least memory.  Specifically, if the function recurses
                   we want the recursive call to pass all the arguments if it
                   can.  We force the inline functions to be macros while
                   processing the non-inline functions and then process the
                   inlines. DCJM 23/1/01. *)
                let
                    (* Split the inline and non-inline functions. *)
                    val (inlines, nonInlines) =
                        List.partition (
                            fn {lambda = { isInline=MaybeInline, ...}, ... } => true | _ => false) mutualDecs
                    
                    (* Go down the non-inline functions creating new addresses
                       for them and entering them in the table. *)
                    val startAddr = !spval
                    val addresses =
                        map (fn {addr, ... } =>
                            let
                                val decSpval   = !spval;
                            in
                                enterOldDec (addr, simpleOptVal (mkLoad (decSpval, 0)));
                                spval := !spval + 1;
                                decSpval
                            end)
                        nonInlines
                    val endAddr = !spval
                    (* We can now process the inline functions.  Since these
                       can't be directly recursive we don't need to do anything
                       special. *)
                    val _ =
                        List.app (fn { lambda, addr, ... } =>
                                    enterOldDec (addr, optimise(Lambda lambda, false, context)))
                        inlines

                    (* Next process the non-inlines.  We really want to be able to
                       compile the functions now if we can and get a constant for
                       the code address.  We can do that for functions which make
                       no non-local references or whose non-local references are
                       by means of constants.  For non-recursive declarations this
                       is easy since an earlier declaration cannot depend on a later
                       one but for mutually recursive declarations we don't know
                       the dependencies.
                       The simple case is where we have a function which does not
                       depend on anything and so can be code-generated in the Lambda
                       case.  Code-generating that may allow others to be code-generated.
                       Another case is where the functions depend on each other but not
                       on anything else.  We can compile them together but not
                       individually.  There are various versions of this second case.
                       The only one we consider here is if all the (non-constant)
                       functions are of that form in which case we process the
                       whole mutually-recursive declaration. *)
                    val hasNonLocalReference = ref false

                    fun checkClosure (Extract{addr, level=0, fpRel=true, ...}) =
                        if addr >= startAddr andalso addr < endAddr
                        then ()
                        else hasNonLocalReference := true
                    |   checkClosure _ = hasNonLocalReference := true

                    fun processNonInlines ({ lambda, addr = decAddr, ... }, decSpval, (decs, otherChanges)) =
                    (* Have a look at the old entry to see if it's a constant. *)
                    let
                        val oldEntry =
                            lookupOldAddr(
                                    {addr=decAddr, level=0, fpRel=true},
                                    nestingOfThisFunction, 0)
                    in
                        case optGeneral oldEntry of
                            oldGen as Constnt _ =>
                                ({addr=decSpval, value=oldGen} :: decs, otherChanges) (* It's already a constant - don't reprocess. *)
                        |   _ =>
                            let
                                (* Set this entry to create a recursive call if we load
                                   the address while processing the function. The recursive
                                   call may come about as a result of expanding an inline
                                   function which then makes the recursive call. *)
                                local
                                    val recursive = simpleOptVal (mkGenLoad (0, ~1, false))
                                in
                                    val _ = enterOldDec(decAddr, recursive);
                                    val _ = enterNewDec(decSpval, recursive)
                                end;
                       
                                (* Now copy this entry. *)
                                val ins  = optimise(Lambda lambda, false, context)

                                val gen  = optGeneral ins;
                                val spec = optSpecial ins;

                                (* The general value is either a reference to the
                                   declaration or a constant if the function has just
                                   been compiled into a code segment. *)
                                val isConstant = isConstnt gen
                                val optGen =
                                    case gen of
                                        Constnt _ => gen
                                    |   Lambda{closure, ...} => (
                                            List.app checkClosure closure;
                                            mkLoad (decSpval, 0)
                                        )
                                    |   _ => raise InternalError "processNonInlines: Not a function";

                                (* Explicitly reset the entry in the new table. *)
                                val _  = enterNewDec(decSpval, simpleOptVal optGen);
              
                                (* If this is a small function we leave the special
                                   value so it can be inserted inline.  Otherwise
                                   we clear it. *)
                                val optSpec =
                                     case spec of
                                        SOME(Lambda{ isInline=NonInline, ...}) => NONE
                                       | _ => optSpecial ins;
                                val nowInline =
                                    isSome optSpec andalso not (isSome(optSpecial oldEntry))
                                (* If this is now a constant or it is a small function when it
                                   wasn't before we need to reprocess everything
                                   which depends on it to try to get the constant inserted
                                   everywhere it can be. *)
                          in
                                  enterOldDec 
                                    (decAddr,
                                     optVal 
                                        {
                                          general = optGen,
                                          special = optSpec,
                                          environ = optEnviron ins,
                                          decs    = optDecs ins (* Should be nil. *)
                                        });
                                (
                                 {addr=decSpval, value=gen} :: decs,
                                 otherChanges orelse isConstant orelse nowInline
                                )
                          end
                   end

                  fun repeatProcess () =
                  let
                      val (decs, haveChanged) =
                         (* Use foldr here to keep the result in the same order
                            in case we can compile them immediately below. *)
                         ListPair.foldr processNonInlines
                            ([], false) (nonInlines, addresses);
                  in
                     if haveChanged
                     then repeatProcess ()
                     else decs
                  end

                  val decs = repeatProcess ()

                  val allAreConstants =
                    List.foldl
                        (fn({value=Constnt _, ...}, others) => others
                          | _ => false) true decs

                    fun convertDecs(decs: simpleBinding list): codeBinding list =
                    let
                        (* Separate out those entries that have been converted
                           to constants from those that are still lambdas. *)
                        fun isLambda {value=Lambda _, ...} = true
                        |   isLambda {value=Constnt _, ...} = false
                        |   isLambda _ = raise InternalError "isLambda: not a lambda or a constant"
                        val (lambdas, constants) = List.partition isLambda decs

                        fun toLambda{addr, value=Lambda lambda} =
                            {addr=addr, lambda=lambda}
                        |   toLambda _ = raise InternalError "toLambda: not a lambda"
                    in
                        map Declar constants @ (if null lambdas then nil else [RecDecs(map toLambda lambdas)])
                    end

                  (* If hasNonLocalReference is still false we can code-generate
                     the mutual declarations. *)
                  val decs =
                     if ! hasNonLocalReference orelse allAreConstants
                     then decs
                     else
                        let
                            (* Create a tuple of Extract entries to get the result. *)
                            val extracts =
                                List.map (
                                    fn ({addr, ...}) => mkLoad(addr, 0))
                                    decs
                            val code = mkEnv(convertDecs decs, mkTuple extracts)
                            (* Code generate it. *)
                            val results = codeGenerateToConstant debugArgs (code, !spval+1)

                            fun reprocessDec({addr=decAddr, ...}, decSpval, (offset, others)) =
                            let
                                val oldEntry =
                                    lookupOldAddr(
                                            {addr=decAddr, level=0, fpRel=true},
                                            nestingOfThisFunction, 0)
                            in
                                let
                                    val newConstant = findEntryInBlock results offset
                                in
                                    (* Replace the entry by an entry with a constant. *)
                                    enterNewDec(decSpval, simpleOptVal newConstant);
                                    enterOldDec 
                                        (decAddr,
                                         optVal 
                                            {
                                              general = newConstant,
                                              special = optSpecial oldEntry,
                                              environ = optEnviron oldEntry,
                                              decs    = optDecs oldEntry (* Should be nil. *)
                                            });
                                    (offset+1, {addr=decSpval, value=newConstant} :: others)
                                end
                            end
                        
                            val (_, newDecs) = ListPair.foldl reprocessDec (0, []) (nonInlines, addresses);
                        in
                            newDecs (* We've converted them all to constants. *)
                        end

                    (* Deal with the rest of the block *)
                    val rest = copyDeclarations vs
                in
                    (* and put these declarations onto the list. *)
                    repDecs (convertDecs decs @ optDecs rest) rest
                end

            |   copyDeclarations (NullBinding v :: vs) =
                let (* Not a declaration - process this and the rest.*)
                    val copiedNode = optimise(v, false, context)
                    val rest = copyDeclarations vs
                in  (* This must be a statement whose
                       result is ignored. Put it into the declaration list. *)
                    repDecs (optDecs copiedNode @ 
                            (NullBinding(optGeneral copiedNode) :: optDecs rest)) rest
                end (* copyDeclarations *)

        in
            copyDeclarations envDecs
        end (* Newenv(... *)
          
    |   optimise (Recconstr entries, _, context as { nestingOfThisFunction, ...}) =
         (* The main reason for optimising record constructions is that they
            appear as tuples in ML. We try to ensure that loads from locally
            created tuples do not involve indirecting from the tuple but can
            get the value which was put into the tuple directly. If that is
            successful we may find that the tuple is never used directly so
            the use-count mechanism will ensure it is never created. *)
        let
            (* The record construction is treated as a block of local
               declarations so that any expressions which might have side-effects
               are done exactly once. *)
            val (fieldEntries, bindings) =
                ListPair.unzip(
                    List.map(fn h => makeNewDecl(optimise(h, false, context), context)) entries)

            val generalFields = List.map optGeneral fieldEntries
            (* Special fields - Create a list of "load" (Extract) entries.
               The reason for this is to allow us to get the full "optVal" for the field.
               The Extract entry will be looked up in "env" to return the "optVal" *)
            val specialFields = List.tabulate(List.length entries, fn addr => mkLoad(addr+1, 0))

            fun env ({addr, ...}:loadForm, depth, _) =
                changeLevel (List.nth(fieldEntries, addr-1)) (depth - nestingOfThisFunction)

            val newRec  = Recconstr generalFields
        in
            optVal 
            {
                (* If all the general values are constants we can create the tuple now. *)
                general = if List.all isConstnt generalFields then makeConstVal newRec else newRec,
                special = SOME(Recconstr specialFields),
                environ = env,
                decs    = List.foldr(op @) [] bindings
            }
        end
          
      |  optimise (Indirect{ base, offset }, _, context) = (* Try to do the selection now if possible. *)
            doIndirection(optimise(base, false, context), offset, context)
       
      |  optimise (pt as Ldexc, _, _) =
            simpleOptVal pt (* just a constant so return it *)
        
      |  optimise (Handle { exp, handler }, tailCall, context) =
        simpleOptVal 
          (Handle {exp     = general context exp,
                   handler = generalOptimise(handler, tailCall, context)}
          )

      |  optimise (c as Container _, _, _) = simpleOptVal c

      |  optimise (TupleFromContainer(container, size), _, context as { spval, nestingOfThisFunction, ...}) =
        let
            (* If possible we want to optimise this away in the same way as
               a tuple made with Recconstr.  We have to be careful, though,
               that we have no references to the container after we return.
               We first make declarations for all the fields and then return 
               a special entry which when we apply the "env" environment
               function to it gives us returns.  That way if we never actually
               use this tuple as a single entity it won't be created.
               If we don't actually use a field the corresponding declaration
               will be removed in cleanCode. *)
            val optCont = optimise(container, false, context)
            (* Since "container" will always be an Extract entry we can have multiple
               references to it in the declarations.  Include an assertion to that
               effect just in case future changes make that no longer true. *)
            val _ =
                case optGeneral optCont of
                   Extract _ => ()
                | _ => raise InternalError "optimise - container is not Extract"
            val baseAddr = !spval
            val _ = spval := baseAddr + size
            val specialDecs =
                List.tabulate(size, fn n => mkDec(n+baseAddr, mkInd(n, optGeneral optCont)))
            val specialEntries = List.tabulate(size, fn n => mkLoad(n+baseAddr, 0))
            fun env (l:loadForm, depth, _) : optVal =
                changeLevel (simpleOptVal(Extract l)) (depth - nestingOfThisFunction)
        in
            optVal 
                {
                  general = TupleFromContainer(optGeneral optCont, size),
                  special = SOME(Recconstr specialEntries),
                  environ = env,
                  decs    = optDecs optCont @ specialDecs
                }
        end

      |  optimise (SetContainer{container, tuple as Cond _ , size}, _, context) =
            (* mkSetContainer transforms this. *)
            optimise(mkSetContainer(container, tuple, size), false, context)

      |  optimise (SetContainer{container, tuple as Newenv _ , size}, _, context) =
            (* mkSetContainer transforms this. *)
            optimise(mkSetContainer(container, tuple, size), false, context)

      |  optimise (SetContainer{container, tuple, size}, _, context) =
            (* Push the set-container down the tree and then process it. If we've
               expanded an inline function we want to be able to find any
               tuple we're creating. *)
            let
                val optCont = general context container
                and optTuple = general context tuple
                val () =
                    case optCont of
                        Extract{level=0, fpRel=true, ...} => ()
                    |   _ => raise InternalError "optimise: SetContainer - not local"
                
                (* If the "tuple" is an expanded inline function it may well
                   contain an if-expression.  If both branches were tuples
                   we will have expanded it already and the result will be
                   a TupleFromContainer. *)
                fun pushSetContainer(Cond(ifpt, thenpt, elsept), decs): codetree =
                    mkEnv(List.rev decs,
                           Cond(ifpt, pushSetContainer(thenpt, []), pushSetContainer(elsept, [])))

                |   pushSetContainer(Newenv(envDecs, envExp), decs) =
                        pushSetContainer(envExp, List.rev envDecs @ decs)

                |   pushSetContainer(tuple as
                        TupleFromContainer(
                            Extract{addr=innerAddr, level=0, fpRel=true, ...}, innerSize),
                        decs) =
                    if innerSize = size
                    then
                        (
                        case optCont of
                            Extract{addr=containerAddr, level=0, fpRel=true, ...} =>
                            let
                                (* We can remove the inner container and replace it by
                                   a reference to the outer. *)
                                fun replaceContainerDec [] =
                                    raise InternalError "replaceContainerDec"
                                  | replaceContainerDec ((hd as Declar{addr, ...}) :: tl) =
                                        if addr = innerAddr
                                        then mkDec(addr, mkLoad(containerAddr, 0)) :: tl
                                        else hd :: replaceContainerDec tl 
                                  | replaceContainerDec(hd :: tl) =
                                        hd :: replaceContainerDec tl
                                (* Sometimes the TupleFromContainer is preceded by some
                                   Declar entries.  We need to remove them so that the
                                   last entry in the list is the NullBinding that contains the
                                   code that sets the container, usually as the result of
                                   passing it as an argument to a function.  For some reason
                                   just adding a dummy CodeZero at the end results in a
                                   reference to a non-local container.  I don't understand this. *)
                                fun removeFinalDecs(Declar _ :: tl) = removeFinalDecs tl
                                |   removeFinalDecs(RecDecs _ :: tl) = removeFinalDecs tl
                                |   removeFinalDecs(NullBinding c :: tl) = (c, tl)
                                |   removeFinalDecs [] = raise InternalError "removeFinalDecs: Empty"
                                val (exp, rdecs) = removeFinalDecs(replaceContainerDec decs)
                            in
                                (* Just replace the declaration. *)
                                mkEnv(List.rev rdecs, exp)
                            end
                        | _ => mkEnv(List.rev decs, mkSetContainer(optCont, tuple, size))
                        )
                    else mkEnv(List.rev decs, mkSetContainer(optCont, tuple, size))

                |   pushSetContainer(tuple, decs) =
                        mkEnv(List.rev decs, mkSetContainer(optCont, tuple, size))
              
            in
                simpleOptVal(pushSetContainer(optTuple, []))
            end

      |  optimise (cval as ConstntWithInline _, _, _) =
            (* At the moment we need to convert these into normal constants and the "spec" value. *)
            let
                fun convertEnv(ConstntWithInline(gval, spec, env)) =
                    optVal { general = Constnt gval, special = SOME spec, environ = convertEnv o env, decs=[] }
                |   convertEnv(c as Constnt _) = simpleOptVal c
                |   convertEnv _ = raise InternalError "convertEnv"
            in
                convertEnv cval
            end

      | optimise (ExtractWithInline _, _, _) = raise InternalError "ExtractWithInline"

      |  optimise (TagTest{test, tag, maxTag}, _, context) =
            let
                val optTest = general context test
            in
                case optTest of
                    Constnt testResult =>
                        if isShort testResult andalso toShort testResult = tag
                        then simpleOptVal CodeTrue
                        else simpleOptVal CodeFalse
                |   _ => simpleOptVal(TagTest{test=optTest, tag=tag, maxTag=maxTag})
            end

      |   optimise(IndirectVariable{base, offset}, tailCall, context) =
            (* If this is a constant offset turn it into a simple Indirect instr.
               TODO: If the "base" is actually a TupleVariable we may be able to
               optimise it away in the same way as with fixed-size tuples. *)
            let
                val optOffset = general context offset
            in
                case optOffset of
                    Constnt offs =>
                        optimise(Indirect{base=base, offset=Word.toInt(toShort offs)}, tailCall, context)
                |   _ =>
                    simpleOptVal(IndirectVariable{base=general context base, offset=optOffset})
            end

      |   optimise(TupleVariable(vars, length), _, context as { nestingOfThisFunction, ...}) =
            (* Try to turn this into a fixed size tuple. *)
            let
                val optLength = general context length
            in
                case optLength of
                    Constnt _ =>
                        (* The total length is the sum of all the individual lengths so if it is
                           a constant then all the offsets and lengths must be.  We can turn this
                           into a fixed-size tuple.  We need new declarations at least for the multiple
                           entries so that they are used only once.  Doing that requires us to do the
                           optimisation at this stage rather than generating a Reccons and calling
                           "optimise" recursively so this duplicates that code. *)
                        let
                            val newTab = stretchArray(List.length vars+1, NONE)
                            fun setTab (i, v) = StretchArray.update (newTab, i, SOME v);
                            (* The record construction is treated as a block of local
                               declarations so that any expressions which might have side-effects
                               are done exactly once. *)
                            fun makeOneEntry(optSource, addr) =
                            let
                                (* Put the source into a local variable. *)
                                val (thisArg, newDecs) = makeNewDecl(optSource, context)
                                val () = setTab(addr, thisArg)
                                val gen = optGeneral thisArg
                                (* If there's no "special" and the general is a constant use that for
                                   the special entry otherwise load the original value. *)
                                val specArg =
                                    case (optSpecial thisArg, gen) of
                                        (NONE, Constnt _) => gen
                                    |   _ => mkLoad (addr, 0)
                            in
                                { gen=gen, spec=specArg, decs=newDecs }
                            end

                            fun makeDecs([], _) = {decs = [], gen = [], spec = []}

                            |   makeDecs (VarTupleSingle{source, ...} :: t, addr) =
                                let
                                    val {gen, spec, decs} = makeOneEntry(optimise(source, false, context), addr)
                                    val {decs=restDecs, gen=genRest, spec=specRest} = makeDecs(t, addr + 1)
                                in
                                    {gen = gen :: genRest, spec = spec :: specRest, decs = decs @ restDecs }
                                end

                            |   makeDecs (VarTupleMultiple{base, length, sourceOffset, ...} :: t, addr) =
                                let
                                    val len =
                                        case general context length of
                                            Constnt l => Word.toInt(toShort l)
                                        |   _ => raise InternalError "makeDecs: not constant"
                                    (* Put the base address into a local variable.  We must do this because
                                       the base code could have side-effects which must be done exactly once. *)
                                    val (thisArg, baseDecs) = makeNewDecl(optimise(base, false, context), context)
                                    val () = setTab(addr, thisArg)
                                    val {decs=restDecs, gen=genRest, spec=specRest} = makeDecs(t, addr + len+ 1)

                                    (* Each entry in the result is found by indirection on the base. *)
                                    fun makeEntries n =
                                        if n = len then {decs = [], gen = [], spec = []}
                                        else
                                        let
                                            (* The offset could be a variable even if the resulting size is
                                               a constant. *)
                                            val indirectEntry =
                                                case general context sourceOffset of
                                                    Constnt offset => doIndirection(thisArg, n + Word.toInt(toShort offset), context)
                                                |   varOffset =>
                                                    let
                                                        (* This isn't currently generated so leave this in until it is. *)
                                                        val _ = raise InternalError "Untested"
                                                        (* Add the offsets *)
                                                        val newOffset =
                                                            if n = 0 then varOffset
                                                            else mkEval(rtsFunction RuntimeCalls.POLY_SYS_plus_word,
                                                                        [varOffset, mkConst(toMachineWord n)])
                                                    in
                                                        simpleOptVal(
                                                            IndirectVariable{base=optGeneral thisArg, offset=newOffset})
                                                    end
                                            val thisEntry = makeOneEntry(indirectEntry, addr+n+1)   
                                            val {decs, gen, spec} = makeEntries(n+1)
                                        in
                                            {decs = #decs thisEntry @ decs, gen = #gen thisEntry :: gen,
                                             spec = #spec thisEntry :: spec}
                                        end

                                    val {gen, spec, decs} = makeEntries 0
                                in
                                    {gen = gen @ genRest, spec = spec @ specRest, decs = baseDecs @ decs @ restDecs }
                                end
          
                            val {gen, spec, decs} = makeDecs(vars, 1)
                            val newRec  = Recconstr gen
                            (* Package up the declarations so we can extract the special values. *)
                            val vec = StretchArray.vector newTab
                            fun env ({addr, ...}:loadForm, depth, _) : optVal =
                                changeLevel (valOf (Vector.sub(vec, addr))) (depth - nestingOfThisFunction)
                        in
                            optVal 
                            {
                                (* If all the general values are constants we can create the tuple now. *)
                                general = if List.all isConstnt gen then makeConstVal newRec else newRec,
                                special = SOME(Recconstr spec),
                                environ = env,
                                decs    = decs
                            }
                        end
                |   _ => (* Can't convert it to a fixed-size tuple.
                           TODO: We should treat this in the same way as a fixed record so that selection can be
                           optimised.  This isn't quite so important as for fixed-size tuples.  Fixed-size tuples
                           are used for structures and there we definitely need to be able to extract inline
                           functions.  *)
                    let
                        fun optTuple(VarTupleSingle{source, destOffset}) =
                                VarTupleSingle{source=general context source, destOffset=general context destOffset}
                        |   optTuple(VarTupleMultiple{base, length, destOffset, sourceOffset}) =
                                VarTupleMultiple{base=general context base, length=general context length,
                                                 destOffset=general context destOffset, sourceOffset=general context sourceOffset}
                        val optFields = map optTuple vars
                    in
                        simpleOptVal(TupleVariable(optFields, optLength))
                    end
            end

    (* Inline a function that has been explicitly marked by the frontend.  These are
       either functors or auxiliary functions in curried or tupled fun bindings.
       They are never directly recursive but could contain "small" functions that are. *)
    and inlineInlineOnlyFunction(funct, {body=lambdaBody, name=lambdaName, ...}, argList, tailCall,
            context as {spval, nestingOfThisFunction, recursiveExpansions, newEnv = { lookup = lookupNewAddr, ...}, debugArgs, inlineExpansionDepth, ...}) =
        let
            (* Calling inline proc or a lambda expression which is just called.
               The function is replaced with a block containing declarations
               of the parameters.  We need a new table here because the addresses
               we use to index it are the addresses which are local to the function.
               New addresses are created in the range of the surrounding function. *)
            val localVec = stretchArray (initTrans, NONE);
            val localNewVec = stretchArray (initTrans, NONE);

            local
                val (params, bindings) =
                    ListPair.unzip(
                        List.map (fn (h, _) => makeNewDecl(optimise(h, false, context), context)) argList)

                val paramVec = Vector.fromList params
                val nArgs = List.length argList
            in
                (* The arguments are numbered ~1, ~2 etc. *)
                fun getParameter n = Vector.sub(paramVec, nArgs+n)

                (* Bindings necessary for the arguments *)
                val copiedArgs = List.foldr (op @) [] bindings
            end

            val newEnv =
            { 
                enter = fn (addr, v) => update (localNewVec, addr, SOME v),
                lookup = 
                    fn (ext as { addr, ...}, depth, levels) =>
                        (* It may be local to this function or to the surrounding scope. *)
                    if levels <> 0 orelse addr <= 0
                    then lookupNewAddr(ext, depth, levels)
                    else case localNewVec sub addr of
                            SOME v => changeLevel v (depth - nestingOfThisFunction)
                        |   NONE => lookupNewAddr(ext, depth, levels)
            }

            local
                (* The environment for the expansion of this function
                   is the table for local declarations and the original
                   environment in which the function was declared for
                   non-locals. *)
                fun lookupDec ({ addr=0, ...}, _, 0) =
                   (* Recursive reference - shouldn't happen. *)
                   raise InternalError ("lookupDec: Inline function recurses " ^ lambdaName)
                |   lookupDec ({ addr=index, level, ...}, depth, 0) =
                     let (* On this level. *)
                        val optVal =
                            if index > 0
                            then localVec sub index (* locals *)
                            else SOME(getParameter index) (* parameters *)
                        val value =
                            case optVal of
                                SOME value => value
                            |   NONE => raise InternalError(
                                                concat["Missing value address=", Int.toString index,
                                                       " level= ", Int.toString level, " while expanding ", lambdaName])
                     in
                       changeLevel value (depth - nestingOfThisFunction)
                     end
                |  lookupDec (ptr, depth, levels) =
                     (optEnviron funct) (ptr, depth, levels - 1)
            in
                val oldEnv = 
                    { lookup = lookupDec, enter = fn (index, v) => update (localVec, index, SOME v) }
            end

            val copiedBody =
                optimise(lambdaBody, tailCall,
                    {newEnv = newEnv, oldEnv = oldEnv,
                     nestingOfThisFunction=nestingOfThisFunction,
                     spval=spval,
                     recursiveExpansions=recursiveExpansions,
                     loopFilter=loopFilterError,
                     debugArgs=debugArgs,
                     inlineExpansionDepth=inlineExpansionDepth+1})
        in
            StretchArray.freeze localVec;
            StretchArray.freeze localNewVec;
            (* The result is the result of the body of the inline function.
               The declarations needed for the inline function, the
               declarations used to load the arguments and the declarations
               in the expanded function are all concatenated together. *)
            repDecs (optDecs funct @ (copiedArgs @ optDecs copiedBody)) copiedBody
        end

    (* Expand inline a "small" function i.e. a normal function that appears to be small
       enough to expand inline. *)
    and inlineSmallFunction(funct, {body=lambdaBody, name=lambdaName, argTypes, ...}, argList, resultType,
            context as {spval, nestingOfThisFunction, newEnv = { lookup = lookupNewAddr, ...}, debugArgs, 
            inlineExpansionDepth, ...}) =
        let
            val _ = inlineExpansionDepth < 5 orelse raise InternalError "too deep"
            (* Calling inline proc or a lambda expression which is just called.
               The function is replaced with a block containing declarations
               of the parameters.  We need a new table here because the addresses
               we use to index it are the addresses which are local to the function.
               New addresses are created in the range of the surrounding function. *)
            val localVec = stretchArray (initTrans, NONE)
            val localNewVec = stretchArray (initTrans, NONE)

            val nArgs = List.length argList

            local
                val (params, bindings) =
                    ListPair.unzip(
                        List.map (fn (h, _) => makeNewDecl(optimise(h, false, context), context)) argList)

                val paramVec = Vector.fromList params
            in
                (* The arguments are numbered ~1, ~2 etc. *)
                fun getParameter n = Vector.sub(paramVec, nArgs+n)

                (* Bindings necessary for the arguments *)
                val copiedArgs = List.foldr (op @) [] bindings
            end

            fun lookupLocalNewAddr (ext as { addr, ...}, depth, levels) =
                    (* It may be local to this function or to the surrounding scope. *)
                if levels <> 0 orelse addr <= 0
                then lookupNewAddr(ext, depth, levels)
                else case localNewVec sub addr of
                        SOME v => changeLevel v (depth - nestingOfThisFunction)
                    |   NONE => lookupNewAddr(ext, depth, levels);

            (* Now load the function body itself.  We first process it assuming
               that we won't need to treat any of the arguments specially.  If
               we find that we generate a Loop instruction somewhere we have
               to make sure that any arguments we change in the course of the
               loop are taken out.  For example:
               fun count'(n, []) = n | count' (n, _::t) = count'(n+1, t);
               fun count l = count'(0, l).
               In this case we would start by expanding count' using 0 for n
               throughout, since it's a constant.  When we find the recursive
               call in which n becomes n+1 we find we have to take n out of the
               loop and treat it as a variable.
               We don't need to do this if the argument is passed through unchanged
               e.g. fun foldl f b [] = b  | foldl f b (x::y) = foldl f (f(x, b)) y;
               where the same value for f is used everywhere and by treating it
               specially we can expand its call. 
               This two-pass (it will normally be two-pass but could be more) approach
               allows us to optimise cases where we have a recursive function which
               happens to be non-recursive with particular constant values of the
               arguments.
               e.g. if x = nil ... generates a general recursive function for
               equality on lists but because of the nil argument this optimises
               down to a simple test. *)
            (*
               I'm now extending this to the general recursive case not just tail
               recursion.  If we discover a recursive call while processing the
               function we turn this expansion into a function call and give up
               trying to inline it.  Instead we create a special-purpose function
               for this call but with only the arguments that change as a
               result of the recursive calls actually passed as arguments.  Other
               arguments can be inserted inline in function.
               e.g. fun map f [] = [] | map f (a::b) = f a :: map f b
               where when we map a function over a list we compile a specialised
               mapping function with the actual value of f inserted in it.
            *)
                val needsBeginLoop = ref false
                val needsRecursiveCall = ref false
                val argModificationVec = Array.array(nArgs, false);
                (* Create addresses for the new variables for modified arguments.
                   If newdecl created a variable we might be able to reuse that
                   but it's easier to create new ones. *)
                val argBaseAddr = ! spval; val _ = spval := argBaseAddr + nArgs

                (* filterArgs is called whenever a recursive call is made to
                   this function. *)
                fun filterArgs (argList: (codetree * argumentType) list, isTail, depth) =
                let
                    fun filterArgs' 0 [] = []
                      | filterArgs' _ [] =
                            raise InternalError "filterArgs: wrong number of args"
                      | filterArgs' n ((arg, argType) :: rest) =
                        let
                            (* Is this simply passing the original argument value?  *)
                            val original = getParameter(~n)
                            val unChanged =
                                case (arg, optGeneral original) of
                                    (Constnt w, Constnt w') =>
                                        (* These may well be functions so don't use
                                           structure equality. *)
                                        wordEq(w, w')
                                | (Extract {addr=aA, level=aL, fpRel=aFp, ...},
                                   Extract {addr=bA, level=bL, fpRel=bFp, ...}) =>
                                    aA = bA andalso aFp = bFp andalso
                                    aL = bL+depth-nestingOfThisFunction
                                | _ => false

                        in
                            if unChanged
                            then ()
                            else Array.update(argModificationVec, n-1, true);
                            (* If any recursive call has changed it we need
                               to include this argument even if it didn't
                               change on this particular call. *)
                            if Array.sub(argModificationVec, n-1)
                            then (arg, argType) :: filterArgs' (n-1) rest
                            else (* Not modified *) filterArgs' (n-1) rest
                        end
                in
                    needsBeginLoop := true; (* Indicate we generated a Loop instr. *)
                    (* If this isn't tail recursion we need a full call. *)
                    if isTail then () else needsRecursiveCall := true;
                    (* If we have a recursive inline function containing a
                       local recursive inline function which calls the outer
                       function
                       (e.g. fun f a b = .... map (f a) ....) we may process
                       the body of the inner function twice, once as a lambda
                       and once when we attempt to expand it inline.  That
                       means we will process the recursive call to the outer
                       function twice.  The first call may filter out redundant
                       arguments (e.g. "a" in the above example).  *)
                    if List.length argList <> nArgs
                    then argList
                    else filterArgs' nArgs argList
                end

                (* See how many arguments changed. *)
                fun countSet() =
                    Array.foldl(fn (true, n) => n+1 | (false, n) => n) 0 argModificationVec

                fun checkForLoopInstrs lastModCount =
                (* Try inlining the function.  Called initially or while we only have
                   tail recursive calls.  Returns SOME code if this is possible or NONE
                   if we will have to construct an auxiliary function. *)
                let
                    (* There's still something strange going on where we end up with
                       a BeginLoop that doesn't contain any Loop.  See regression
                       test Succeed/Test108.ML. *)
                    val _ = needsBeginLoop := false

                    fun localOldAddr({ addr=0, ...}, depth, 0) =
                         (* Recursive reference - return the copied function after removing
                            the declarations.  These will be put on in the surrounding
                            scope.  Since we're not actually inside the function (we're
                            expanding it) we can't return a recursive reference so we
                            return the reference to the function that was passed in.
                            This cannot be a recursive or tail recursive call because
                            that case is dealt with specially in the Eval case of
                            "optimise". *)
                         changeLevel (stripDecs funct) (depth - nestingOfThisFunction)

                    |   localOldAddr ({ addr=index, ...}, depth, 0) =
                        let (* On this level. *)
                            val optVal =
                                if index > 0
                                then valOf (localVec sub index)  (* locals *)
                                else (* index < 0 *) if Array.sub(argModificationVec, ~index-1)
                                then (* This argument changes - must use a variable even if
                                        the original argument was a constant. *)
                                    simpleOptVal(mkLoad(argBaseAddr + nArgs + index, 0))
                                else getParameter index (* parameters *)
                        in
                            changeLevel optVal (depth - nestingOfThisFunction)
                        end

                    | localOldAddr (ptr, depth, levels) = (* On another level. *)
                            (optEnviron funct) (ptr, depth, levels - 1)

                    (* All declarations should be of positive addresses. *)
                    fun setNewTabForInline (addr, v) = update (localNewVec, addr, SOME v)
                    and setTabForInline (index, v) = update (localVec, index, SOME v)

                    val copiedBody =
                         optimise(lambdaBody, true,
                          {newEnv = {lookup=lookupLocalNewAddr, enter = setNewTabForInline },
                           oldEnv = {lookup=localOldAddr, enter =setTabForInline },
                           nestingOfThisFunction=nestingOfThisFunction,
                           spval=spval,
                           recursiveExpansions=SOME(filterArgs),
                           loopFilter=loopFilterError,
                           debugArgs=debugArgs,
                           inlineExpansionDepth=inlineExpansionDepth+1})
                  
                 in
                    if ! needsRecursiveCall
                    then (* We need a fully recursive call. *)
                        NONE (* *)
                    else if ! needsBeginLoop 
                    then (* We've found at least one recursive call which changes its
                            argument value. *)
                    let
                        val newModCount = countSet()
                    in
                        if newModCount > lastModCount
                        then checkForLoopInstrs newModCount
                        else SOME copiedBody
                    end
                    else SOME copiedBody
                end

                fun checkRecursiveCalls lastModCount =
                (* Called if a simple inline expansion won't work.  We're
                   going to have to generate this function as a function and
                   a call to that function.  However we may well gain by inserting
                   in line arguments which don't change as a result of recursion.
                   TODO: If all the arguments are modified we could just as
                   well fall back to calling the general function. *)
                let
                    val nesting = nestingOfThisFunction + 1
                    (* Find the parameter we're actually going to use. *)
                    fun getParamNo n =
                        if n = 0 then 0
                        else if Array.sub(argModificationVec, ~n -1)
                        then getParamNo (n+1) - 1
                        else getParamNo (n+1)

                    fun localOldAddr ({ addr=0, ...}, depth, 0) =
                         (* Recursive reference.  We're going to generate this
                            as a function so return a reference to the closure. *)
                         simpleOptVal(mkGenLoad (0, depth - nesting, false))

                    |   localOldAddr ({ addr=index, ...}, depth, 0) =
                         if index > 0  (* locals *)
                         then changeLevel(valOf (localVec sub index)) (depth - nesting)
                         else (* index < 0 - parameters *)
                            if Array.sub(argModificationVec, ~index -1)
                         then (* This argument has changed - find the corresponding
                                 actual argument. *)
                            simpleOptVal(mkLoad(getParamNo index, depth-nesting))
                         else (* Unchanged - get the entry from the table, converting
                                 the level because it's in the surrounding scope. *)
                            changeLevel (getParameter index) (depth-nesting+1)

                    |   localOldAddr (ptr, depth, levels) =
                            (optEnviron funct) (ptr, depth, levels - 1)

                    val newAddrTab = stretchArray (initTrans, NONE);

                    (* localNewAddr is used as the environment of inline functions within
                       the function we're processing. *)
                    fun localNewAddr ({ addr=index, ...}, depth, 0) =
                        if index > 0 
                        then case newAddrTab sub index of
                            NONE => (* Return the original entry if it's not there. *)
                                simpleOptVal(
                                    mkGenLoad (index, depth - nesting, true))
                        |   SOME v => changeLevel v (depth - nesting) 
                        else simpleOptVal (mkGenLoad (index, depth - nesting, index <> 0))
                    |   localNewAddr (ptr, depth, levels) =
                            lookupNewAddr (ptr, depth, levels - 1)

                    fun setNewTab (addr, v) = update (newAddrTab, addr, SOME v)
                    and setTab (index, v) = update (localVec, index, SOME v)

                    val newAddressAllocator = ref 1 (* Starts at one. *)

                    val copiedBody =
                         optimise(lambdaBody, false, (* Don't generate loop instructions. *)
                          {newEnv = {lookup=localNewAddr, enter=setNewTab },
                           oldEnv = {lookup=localOldAddr, enter=setTab },
                           nestingOfThisFunction=nesting,
                           spval=newAddressAllocator,
                           recursiveExpansions=SOME filterArgs,
                           loopFilter=loopFilterError,
                           debugArgs=debugArgs,
                           inlineExpansionDepth = inlineExpansionDepth+1})

                    val newModCount = countSet()
                in
                    if newModCount > lastModCount
                    then (* We have some (more) arguments to include. *)
                        checkRecursiveCalls newModCount
                    else
                    let
                        fun filterArgTypes([], _) = []
                        |   filterArgTypes(hd:: tl, n) =
                                if Array.sub(argModificationVec, n-1)
                                then hd :: filterArgTypes(tl, n-1)
                                else filterArgTypes(tl, n-1)
                        val newArgList = filterArgTypes(argTypes, nArgs)
                    in
                        simpleOptVal(Lambda {
                                body = getGeneral copiedBody,
                                isInline = NonInline,
                                name = lambdaName,
                                closure = [],
                                argTypes = newArgList, resultType=resultType,
                                level = nestingOfThisFunction + 1,
                                localCount = ! newAddressAllocator + 1 })
                    end
                end

            (* If we need to make the declarations put them in at the
               beginning of the loop. *)
            fun makeDecs(0, []) = []
              | makeDecs(n, (_, typ):: rest) =
                    if not (Array.sub(argModificationVec, n-1))
                    then makeDecs (n-1, rest)
                    else
                    let
                        val argVal = getGeneral(getParameter(~n))
                        val argDec =
                            (* Include the address at this stage even if it's a call *)
                            {value = argVal, addr=argBaseAddr+nArgs-n}
                    in
                        (argDec, typ) :: makeDecs (n-1, rest)
                    end
             |  makeDecs _ = raise InternalError "Unequal lengths"

        in
            case checkForLoopInstrs 0 of
                SOME procBody =>
                let
                    (* Simple inline expansion is possible. *)
                    val newDecs = makeDecs(nArgs, argList)
                    val copiedBody =
                        if ! needsBeginLoop
                        then simpleOptVal(BeginLoop{loop=getGeneral procBody, arguments=newDecs})
                        else if Array.exists(fn x => x) argModificationVec
                        then (* We could have reset needsBeginLoop to false and ended up not needing a loop.
                                If we have declarations that we made earlier we have to include them. *)
                            repDecs (List.map (Declar o #1) newDecs @ optDecs procBody) procBody
                        else procBody
                in
                    StretchArray.freeze localVec;
                    StretchArray.freeze localNewVec;
                    (* The result is the result of the body of the inline function.
                       The declarations needed for the inline function, the
                       declarations used to load the arguments and the declarations
                       in the expanded function are all concatenated together. *)
                    repDecs (optDecs funct @ (copiedArgs @ optDecs copiedBody)) copiedBody
                end

            |   NONE =>
                let
                    (* Need to build a function. *)
                    val procBody = checkRecursiveCalls (countSet())
                    val newDecs = makeDecs(nArgs, argList)
                    (* Put the function into the declarations. *)
                    val addr = ! spval before spval := !spval+1
                in
                    optVal{
                        general =
                            Eval {function = mkLoad(addr, 0), 
                                  argList = List.map (fn({value, ...}, t) => (value, t)) newDecs,
                                  resultType=resultType},
                        special = NONE,
                        decs = (optDecs funct @ (copiedArgs @  [mkDec(addr, getGeneral procBody)])),
                        environ = errorEnv
                    }
                end
        end

    fun codetreeOptimiser(pt, debugSwitches) =
    let
        val localAddressAllocator = ref 1
        val oldAddrTab = stretchArray (initTrans, NONE)
        val newAddrTab = stretchArray (initTrans, NONE)
        
        local
            (* Return the entry from the "old" table with the depth adjusted if necessary.
               The entry must exist. *)
            fun lookupOldAddr ({addr, level, fpRel, ...}: loadForm, depth, 0) =
            (
                case oldAddrTab sub addr of
                    SOME v => changeLevel v depth
                |   NONE =>
                    let
                        val msg =
                            concat["lookupOldAddr: outer level. (Addr=", Int.toString addr,
                                " Level=", Int.toString level, " Fprel=", Bool.toString fpRel, ")"]
                    in
                        raise InternalError msg
                    end
            )
            |   lookupOldAddr ({addr, level, fpRel, ...}, _, _) =
                let
                    val msg =
                        concat["lookupOldAddr: outer level. (Addr=", Int.toString addr,
                            " Level=", Int.toString level, " Fprel=", Bool.toString fpRel, ")"]
                in
                    raise InternalError msg
                end

            (* Make an entry in the "old" table *)
            and enterOldDec (addr, tab) = update (oldAddrTab, addr, SOME tab)
        in
            val oldEnv = { lookup = lookupOldAddr, enter = enterOldDec }
        end

        local
            (* Return the entry from the "new" table.  If the entry does not exist
               just return a "load" entry with the depth adjusted as necessary. *)
            fun lookupNewAddr ({addr, ...}: loadForm, depth, 0) =
            (
                case newAddrTab sub addr of
                    NONE => simpleOptVal(mkGenLoad (addr, depth, true))
                |   SOME v => changeLevel v depth 
                )
            |  lookupNewAddr _ = raise InternalError "outer level reached in lookupNewAddr"

            (* Make an entry in the "new" table. *)
            fun enterNewDec (addr, tab) = update (newAddrTab, addr, SOME tab)
        in
            val newEnv = { lookup = lookupNewAddr, enter = enterNewDec }
        end

        val resultCode =
          optimise(pt, false,
            {newEnv = newEnv, oldEnv = oldEnv,
             nestingOfThisFunction=0, spval=localAddressAllocator,
             recursiveExpansions=NONE, loopFilter=loopFilterError, debugArgs=debugSwitches,
             inlineExpansionDepth = 0})

    in
        (* Turn the arrays into vectors. *)
        StretchArray.freeze oldAddrTab;
        StretchArray.freeze newAddrTab;
        (resultCode, ! localAddressAllocator + 1)

    end

    structure Sharing = struct type codetree = codetree and optVal = optVal end

end;
