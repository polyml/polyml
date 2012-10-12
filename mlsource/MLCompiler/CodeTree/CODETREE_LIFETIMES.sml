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

functor CODETREE_LIFETIMES(
    structure BASECODETREE: BaseCodeTreeSig
    structure CODETREE_FUNCTIONS: CodetreeFunctionsSig

    sharing BASECODETREE.Sharing = CODETREE_FUNCTIONS.Sharing

) :
sig 
    type codetree
    val lifeTimes: codetree * int -> codetree
    structure Sharing: sig type codetree = codetree end
end
=
struct
    open BASECODETREE
    open Address
    open CODETREE_FUNCTIONS

    exception InternalError = Misc.InternalError

    (* This function transforms the codetree to add "lifetime" information
       for bindings ("declarations").  The idea is to aid the code-generator
       by indicating when a binding is no longer required and also to
       distinguish short-lived bindings from longer-lived bindings
       when deciding on register allocation.
       It also causes unused bindings to be discarded if they are
       not used and have no side-effects.  The previous passes may
       generate extra bindings and rely on this pass to remove them
       if they are not actually used. *)

    fun lifeTimes (pt, localAddressCount) =
    let
        fun copyCode (pt, argUses, localCount) =
        let
            (* Tables for local declarations. "localUses" is the last reference
               for the declaration.  *)
            val localUses         = Array.array(localCount, 0);
            (* "Closure" tables for statically-linked functions.  If we have a
               function called with a static link we need the non-locals it refers
               to to remain on the stack until the last CALL of the function.  That
               requires setting the last-reference to those non-locals to at least
               the last call.  This is transitive.  If one of the functions calls
               another function then the called function and its "closure" are
               added to the calling function's "closure". *)
            val slClosureTable    = Array.array(localCount, nil)

            (* Because we count instructions from the end smaller values mean
               further away and we want to count the smallest non-zero value. *)
            fun maxUse(m, 0) = m
            |   maxUse(0, m) = m
            |   maxUse(m, n) = Int.min(m, n)
      
            (* If we are inside a loop these entries indicate that
               the declaration was made outside so the entries must
               not be killed there. *)
            val outsideLoop       = Array.array (localCount, false)
            (* This also applies to all the parameters of the function which
               could be passed in registers. *)
            val outsideLoopRef = ref false

            (* This counts the number of "instructions" from the end of the code (because
               we process it depth first) to give a measure of how long a declaration is in
               use.  This is used to control register spilling in the code-generator. *)
            val instrCount = ref 1

            abstype usageSet = UsageSet of {locals: int Vector.vector, args: int Vector.vector}
            with
              (* Used to give us a "kill set" for an expression.
                 In the case of parallel flows of control (e.g. then- and else-parts
                 of an if-then-else) we can explicitly kill variables if they
                 appear in the kill set for one branch but not in another.
                 e.g. in  if x then y else z  assuming that x, y, z are not
                 used in subsequent expressions we can kill z in the then-branch
                 and y in the else-branch.  The advantage of this is that we don't
                 need to save variables if they are never used. *)
                fun saveUsages() =
                    UsageSet{locals=Array.vector localUses, args=Array.vector argUses}

                (* Restore the table to the saved values. *)
                fun setToSaved(UsageSet{locals, args}): unit =
                (
                    Array.copyVec{src=locals, dst=localUses, di=0};
                    Array.copyVec{src=args, dst=argUses, di=0}
                )

                (* Similar to setToSaved except that it sets the current set
                   to the union of the current set and the saved set. *)
                fun addFromSaved(UsageSet{locals=locals, args=args}): unit =
                (
                    Array.modifyi(fn(i, v) => maxUse(v, Vector.sub(args, i))) argUses;
                    Array.modifyi(fn(i, v) => maxUse(v, Vector.sub(locals, i))) localUses
                )

                (* Compute the differences between usage sets as kill entries. *)
                fun computeKillSetLists usages =
                let
                    (* We want to find the cases where the value is zero in at least one branch and
                       non-zero in at least one other branch.  Sum the zeros.  If the result is zero
                       the variable is not referenced at all in the set and we can ignore it.  Similarly
                       if it is the length of the list then it is referenced in all the branches
                       and again we can ignore it. *)
                    fun getAllLocals i =
                        List.foldl(fn(UsageSet{locals, ...}, l) => if Vector.sub(locals, i) = 0 then l+1 else l) 0 usages
                    val sumLocalSet = Vector.tabulate(localCount, getAllLocals)
                    val argCount = Array.length argUses
                    fun getAllArgs i =
                        List.foldl(fn(UsageSet{args, ...}, l) => if Vector.sub(args, i) = 0 then l+1 else l) 0 usages
                    val sumArgSet = Vector.tabulate(argCount, getAllArgs)
                    val fullSet = List.length usages

                    fun computeKills(UsageSet{locals, args}) =
                    let
                        (* Create lists of Extract entries with lastRef true to indicate that the
                           item is no longer required. *)
                        val killArgs =
                            Vector.foldli (fn (addr, n, l) =>
                                if n = 0 orelse n = fullSet orelse Vector.sub(args, addr) <> 0 orelse ! outsideLoopRef
                                then l 
                                else if addr = 0 then mkClosLoad(0, true) :: l
                                else mkGenLoad(~addr, 0, true, true) :: l) [] sumArgSet
                    in
                        Vector.foldli (fn (addr, n, l) =>
                                if n = 0 orelse n = fullSet orelse Vector.sub(locals, addr) <> 0 orelse Array.sub(outsideLoop, addr)
                                then l 
                                else mkGenLoad(addr, 0, true, true) :: l) killArgs sumLocalSet
                    end
                in
                    List.map computeKills usages
                end
            end

            fun addKillSet(original, []) = original (* No change *)
            |   addKillSet(Newenv decs, killSet) = Newenv(killSet @ decs)
            |   addKillSet(original, killSet) = Newenv(killSet @ [original]);

            (* returns the translated node *)
            fun locaddr { addr=laddr, fpRel=true, ...} =
            (
                instrCount := !instrCount+1;

                if laddr < 0
                then
                let (* parameters *)
                    val argNo = ~ laddr;
                    val lastReference =
                        Array.sub(argUses, argNo) = 0 andalso not (!outsideLoopRef)
                in 
                    (* Mark the argument as used. *)
                    Array.update (argUses, argNo, maxUse(! instrCount, Array.sub(argUses, argNo)));
                    mkGenLoad (laddr, 0, true, lastReference)
                end
          
                (* isOnstack *)
                else
                let 
                    (* If this was outside a loop we can't mark this as the last
                       reference because it needs to be retained for the next time round. *)
                    val lastReference =
                        Array.sub(localUses, laddr) = 0 andalso not (Array.sub(outsideLoop, laddr))
                in
                    Array.update (localUses, laddr, maxUse(! instrCount, Array.sub(localUses, laddr)));
                    mkGenLoad (laddr, 0, true, lastReference)
                end
            )
            | locaddr { addr=laddr, fpRel=false, ...} =
                let
                    val () = instrCount := !instrCount+1;
                    val lastReference = Array.sub(argUses, 0) = 0 andalso not (!outsideLoopRef)
                in
                    (* Mark the closure as used. *)
                    Array.update (argUses, 0, maxUse(! instrCount, Array.sub(argUses, 0)));
                    mkGenLoad (laddr, 0, false, lastReference)
                end
          (* locaddr *)

            (* Map f onto a list tail first.  N.B. It doesn't reverse the list.
               Generally used to map "insert" over a list where we need to
               ensure that last references to variables are detected correctly. *)
            fun mapright _ [] = []
            |   mapright f (a::b) =
                let
                    val rest = mapright f b
                in
                    f a :: rest
                end

            fun insert (pt as MatchFail) = pt
          
            |   insert(AltMatch(x, y)) =
                let
                    val insY = insert y
                    val insX = insert x
                in
                    AltMatch (insX, insY)
                end
       
            |   insert CodeNil = CodeNil

            |   insert(Eval { function as Extract{addr, fpRel=true, ...}, argList, resultType, ...}) =
                let
                    (* If this is a statically-linked function make references to the closure.
                       If this is actually the last reference this may result in returning
                       kill entries for some of the closure entries.  If we're in a loop we will
                       still have made a reference and the kill entries will be put at the end of
                       the loop. *)
                    val closureKills =
                        if addr > 0
                        then
                        let
                            fun getKill (Extract ext) =
                                (
                                    case locaddr ext of
                                        ext as Extract{lastRef=true, ...} => SOME ext
                                    |   _ => NONE
                                )
                            |   getKill _ = NONE
                        in
                            List.mapPartial getKill (Array.sub(slClosureTable, addr))
                        end
                        else []
                    val () = instrCount := !instrCount+1
                    (* Process the arguments first. *)
                    val newargs = mapright(fn (c, t) => (insert c, t)) argList
                    val eval =
                        Eval {function = insert function, argList = newargs, earlyEval = false, resultType=resultType}
                in
                    if null closureKills then eval
                    else KillItems{expression=eval, killSet=closureKills, killBefore=false}
                end

            |   insert(Eval { function, argList, resultType, ...}) =
                let
                    (* Process the arguments first. *)
                    val newargs = mapright(fn (c, t) => (insert c, t)) argList
                    (* Then the body. *)
                    val func = insert function
                in
                    Eval {function = func, argList = newargs, earlyEval = false, resultType=resultType}
                end

            |   insert(Extract ext) = locaddr ext

            |   insert(Indirect {base, offset}) = Indirect {base = insert base, offset = offset}

            |   insert(pt as Constnt _) = 
                    pt (* Constants can be returned untouched. *)

            |   insert(BeginLoop{loop=body, arguments=argList, ...}) = (* Start of tail-recursive inline function. *)
                let
                    (* If we have declarations outside the loop that are last used inside it we
                       must make sure they're not marked as last used within the loop.  That would
                       cause the register containing the value to become available for reuse which
                       could mean that when we jumped back to the start of the loop it was no
                       longer there.  We lift all such last-uses out of the loop and add them
                       after the loop. *)
                    val () = instrCount := !instrCount+1
                    val loopEndPosition = !instrCount
                    (* Save the usage state before we process the loop.  Because we're processing
                       the block tail first this indicates all the declarations that are in use
                       AFTER the loop. *)
                    val usagesAfterLoop = saveUsages()
                    val oldLoopEntries = Array.vector outsideLoop
                    (* Set every entry to the "outsideLoop" array to true. *)
                    val () = Array.modify (fn _ => true) outsideLoop
                    val wasInLoop = ! outsideLoopRef
                    val () = outsideLoopRef := true;

                    (* Make entries in the tables for the arguments. I'm not sure
                       if this is essential. *)
                    fun declareArg(Declar{addr=caddr, ...}, _) =
                    (
                        Array.update (localUses, caddr, 0);
                        Array.update (outsideLoop, caddr, false) (* Must do this. *)
                    )
                    |   declareArg _ = raise InternalError "declareArg: not a declaration"
                    val _ = List.app declareArg argList

                    (* Process the body. *)
                    val insBody = insert body

                    (* We want to set the arguments to "unreferenced".  These are effectively local to
                       the loop so they can safely be killed inside it.  However we want to record
                       the final references so we can attach them to the declarations. *)
                    local
                        fun processDec(Declar{addr, ...}, _) =
                            Array.sub(localUses, addr) before Array.update (localUses, addr, 0)
                        |   processDec _ = raise InternalError "processDec"
                    in
                        val loopArgUses = List.map processDec argList
                    end

                    val usagesBeforeLoop = saveUsages()
                    (* Restore the state. *)
                    val () = outsideLoopRef := wasInLoop
                    val () = Array.copyVec{src=oldLoopEntries, dst=outsideLoop, di=0}
                    val (killAfter, killBefore) =
                        case computeKillSetLists [usagesAfterLoop, usagesBeforeLoop] of
                            [thenKill, elseKill] => (thenKill, elseKill)
                        |   _ => raise InternalError "computeKillSets"
                    val _ = null killBefore orelse raise InternalError "Not killBefore"
                    (* Set the lifetime of everything in the killAfter set to be the
                       end of the loop.  Since their last references are inside the loop
                       this means extending the lifetime until the end. *)
                    local
                        fun extendLife(Extract{addr, level=0, fpRel=true, ...}) =
                                if addr < 0 then Array.update (argUses, ~addr, loopEndPosition)
                                else Array.update (localUses, addr, loopEndPosition)
                        |   extendLife(Extract{addr=0, level=0, fpRel=false, ...}) =
                                Array.update (argUses, 0, loopEndPosition)
                        |   extendLife _ = raise InternalError "Not an Extract"
                    in
                        val () = List.app extendLife killAfter
                    end
                    (* Finally the initial argument values. *)
                    local
                        fun copyDec((Declar{addr, value, ...}, _), uses) =
                                mkDecRef(insert value, addr, uses)
                        |   copyDec _ = raise InternalError "copyDec: not a declaration"
                    in
                        val newargs = mapright copyDec (ListPair.zipEq(argList, loopArgUses))
                    end
                    val loop =
                        BeginLoop{loop=insBody, arguments=ListPair.zipEq(newargs, List.map #2 argList)}
                in
                    (* Add the kill entries on after the loop. *)
                    if null killAfter then loop
                    else KillItems{expression=loop, killSet=killAfter, killBefore=false}
                end
    
            |   insert(Loop argList) = (* Jump back to start of tail-recursive function. *)
                        Loop(mapright(fn (c, t) => (insert c, t)) argList)

            |   insert(Raise x) = Raise (insert x)

                (* See if we can use a case-instruction. Arguably this belongs
                   in the optimiser but it is only really possible when we have
                   removed redundant declarations. *)
            |   insert(Cond(condTest, condThen, condElse)) =
                        copyCond (condTest, condThen, condElse)

            |   insert(Newenv ptElist) =
                let
                    (* Process the body. Recurses down the list of declarations
                       and expressions processing each, and then reconstructs the
                       list on the way back. *)
                    fun copyDeclarations ([])   = []

                    |   copyDeclarations ((Declar{addr=caddr, value = pt, ...}) :: vs)  =
                        let
                            (* Set the table entries.  We don't reuse entries so this is just a check. *)
                            val _ = Array.sub(localUses, caddr) <> 0
                                       andalso raise InternalError "copyDeclarations: Already used"
                            val () = Array.update (outsideLoop, caddr, false) (* It's local *)
                            val () =
                                case pt of
                                    Lambda{makeClosure=false, closure, ...} =>
                                    let
                                        (* It's a statically-linked function: set the closure
                                           array entry.  If the closure entries themselves are
                                           statically linked function we have to include the
                                           items from those closures in this set. *)
                                        fun closureTrans (Extract{fpRel=true, addr, ...}, l) =
                                            if addr > 0 then Array.sub(slClosureTable, addr) @ l else l
                                        |   closureTrans (_, l) = l
                                        val trans = List.foldl closureTrans [] closure
                                    in
                                        Array.update(slClosureTable, caddr, trans @ closure)
                                    end
                                |   _ => ()
                            val rest = copyDeclarations vs
                            val wasUsed = Array.sub(localUses, caddr)
                            val () = instrCount := !instrCount+1
                        in
                            (* It is never used and it has no side-effects so we can ignore it. *)
                            if wasUsed = 0 andalso sideEffectFree pt
                            then rest
                            else
                            let
                                val dec = insert pt
                            in
                                (* Set the use count back to free otherwise this local
                                   declaration would become part of the kill set for the
                                   surrounding expression. *)
                                Array.update (localUses, caddr, 0);
                                mkDecRef(dec, caddr, wasUsed) :: rest
                            end
                        end (* copyDeclarations.isDeclar *)

                    |   copyDeclarations (MutualDecs mutualDecs :: vs)  =
                        let
                            (* Mutually recursive declarations. *)
                            (* This is a bit messy.  For static-link functions we need to
                               make sure the last reference to the closure entries is after
                               the last call to the function.  For full-closure functions
                               the last reference must be after the closures have all been
                               built. *)
                            (* Get the closure lists for all the declarations.  We assume that
                               any of these can call any of the others so we just accumulate
                               them into a single list. *)
                            local
                                fun getClosure(
                                        Declar{value=Lambda{makeClosure, closure, ...}, ...},
                                        (slClosures, fcClosures)) =
                                    if makeClosure
                                    then (slClosures, closure @ fcClosures) else (closure @ slClosures, fcClosures)
                                |   getClosure(_, (slClosures, fcClosures)) = (slClosures, fcClosures)
                                val (slClosures, fcClosures) = List.foldl getClosure ([], []) mutualDecs
                                (* Include any statically linked functions this references. *)
                                fun closureTrans (Extract{fpRel=true, addr, ...}, l) =
                                    if addr > 0 then Array.sub(slClosureTable, addr) @ l else l
                                |   closureTrans (_, l) = l
                                val trans = List.foldl closureTrans [] slClosures
                            in
                                val staticCl = trans @ slClosures
                                val fullClosureList = fcClosures
                            end

                            (* Make the declarations. *)
                            local
                                fun applyFn (Declar{addr=caddr, ...}) =     
                                    (
                                        Array.sub(localUses, caddr) <> 0 andalso raise InternalError "applyFn: Already used";
                                        Array.update(slClosureTable, caddr, staticCl);
                                        Array.update (outsideLoop, caddr, false) (* It's local *)
                                    )
                                |   applyFn _ = raise InternalError "applyFn: not a Declar"
                            in
                                val () = List.app applyFn mutualDecs
                            end
                  
                            (* Process the rest of the block. Identifies all other
                               references to these declarations. *)
                            val restOfBlock = copyDeclarations vs

                            val () = instrCount := !instrCount+1

                            (* Process the closure entries and extract the ones that are the last refs. *)
                            val lastRefsForClosure =
                                List.filter (fn Extract{lastRef=true, ...} => true | _ => false)
                                    (map insert fullClosureList)

                            fun copyDec (Declar{addr=caddr, value=dv, ...}) = mkDec (caddr, insert dv)
                            |   copyDec _ = raise InternalError "copyDec: not a Declar"               

                            val copiedDecs = map copyDec mutualDecs;
           
                            (* Now we know all the references we can complete
                               the declaration and put on the use-count. *)
                            fun copyEntries []      = []
                            |   copyEntries (Declar{ addr, value, ...} ::ds) =
                                let
                                    val wasUsed = Array.sub(localUses, addr)
                                in
                                    if wasUsed = 0 andalso sideEffectFree value
                                    then copyEntries ds
                                    else 
                                    (
                                        (* Set the use count back to false otherwise this
                                           entry would become part of the kill set for the
                                           surrounding expression. *)
                                        Array.update(localUses, addr, 0);
                                        mkDecRef(value, addr, wasUsed) :: copyEntries ds
                                    )
                                end
                            |  copyEntries (_::_) =
                                    raise InternalError "copyEntries: Not a Declar";
                  
                            val decs = copyEntries copiedDecs
                        in
                            (* Return the mutual declarations and the rest of the block. *)
                            case decs of
                                []   => lastRefsForClosure @ restOfBlock         (* None left *)
                            |   [d]  => d :: (lastRefsForClosure @ restOfBlock)    (* Just one *)
                            |   _    => mkMutualDecs decs :: (lastRefsForClosure @ restOfBlock)
                        end (* copyDeclarations.isMutualDecs *)
          
                    |   copyDeclarations (v :: vs)  =
                        let (* Not a declaration - process this and the rest. *)
                           (* Must process later expressions before earlier
                               ones so that the last references to variables
                               are found correctly. *)
                            val copiedRest = copyDeclarations vs;
                            val copiedNode = insert v;
                        in
                            (* Expand out blocks *)
                            case copiedNode of
                                Newenv decs => decs @ copiedRest
                            |   _ => copiedNode :: copiedRest
                        end (* copyDeclarations *)

                    val insElist = copyDeclarations ptElist
                in
                    (* If there is only one item then return that item (unless it is
                       a declaration - this can occur if we have a block which contains
                       a declaration to discard the result of a function call and 
                       only do its side-effects). *)
                    wrapEnv insElist
                end (* isNewEnv *)
                
            |   insert(Recconstr recs) = (* perhaps it's a constant now? *)
                    mkTuple (mapright insert recs) 

            |   insert(pt as Ldexc) = pt (* just a constant so return it *)
      
            |   insert(Lambda{body=lambdaBody, level=nesting, argTypes, isInline,
                             name=lambdaName, resultType, localCount, closure, makeClosure, ...}) = 
                let
                    val numArgs = List.length argTypes
                    (* The size is one more than the number of arguments because the
                       arguments are numbered from ~1 .. ~n and we use the entries
                       as ~arg.  Entry zero is used for the closure. *)
                    val argUses      = Array.array(numArgs+1, 0);

                    (* process the body *)
                    val insertedCode = copyCode (lambdaBody, argUses, localCount);
                    val copiedClosure = mapright insert closure
                
                    val argUseList = Array.foldr(op ::) [] argUses
                in
                    Lambda 
                    {
                        body          = insertedCode,
                        isInline      = isInline,
                        name          = lambdaName,
                        closure       = copiedClosure,
                        argTypes      = argTypes,
                        resultType    = resultType,
                        level         = nesting,
                        closureRefs   = hd argUseList,
                        localCount    = localCount,
                        makeClosure   = makeClosure,
                        argLifetimes  = List.rev(tl argUseList)
                    }
                end
    
            |   insert(Handle { exp, handler }) =
                let
                    (* The order here is important.  We want to make sure that
                       the last reference to a variable really is the last. *)
                   val hand = insert handler
                   val exp = insert exp
                in
                  Handle {exp = exp, handler = hand}
                end

            |   insert(c as Container _) = c

            |   insert(SetContainer {container, tuple, size}) =
                    SetContainer{container = insert container, tuple = insert tuple, size = size}

            |   insert(TupleFromContainer(container, size)) =
                    TupleFromContainer(insert container, size)
         
            |   insert(Global g) =
                   (* Should have been taken off by the optimiser. *)
                   optGeneral g : codetree

            |   insert(TagTest{test, tag, maxTag}) = TagTest{test=insert test, tag=tag, maxTag=maxTag}
            
            |   insert(Case{cases, test, caseType, default}) =
                let
                    (* We need to compute the usages for each of the branches: i.e. the
                       default plus each of the cases.  Because they are done in parallel
                       any of the branches contains the last reference of a variable then
                       we need to add kill entries to the other branches so that every
                       branch contains either a "real" final usage or a kill entry. *)
                    val usagesAfterCase = saveUsages()
                    val insDefault = insert default
                    val defaultUsage = saveUsages()
                    val () = setToSaved usagesAfterCase
                    fun processCase(c, tag) =
                    let
                        val () = setToSaved usagesAfterCase
                        val insCase = insert c
                        val caseUsage = saveUsages()
                    in
                        ((insCase, tag), caseUsage)
                    end
                    val (caseList, usageList) = ListPair.unzip (List.map processCase cases)
                    val kills = computeKillSetLists(defaultUsage :: usageList)
                    val casePlusKills = ListPair.mapEq(fn ((c, t), k) => (addKillSet(c, k), t)) (caseList, tl kills)
                    (* Restore the overall usage by setting the reference to the union of all the branches. *)
                    val () = List.app addFromSaved(defaultUsage :: usageList)
                in
                    Case{cases=casePlusKills, test=insert test, caseType=caseType,
                         default=addKillSet(insDefault, hd kills)}
                end

            |   insert(IndirectVariable{base, offset}) =
                    IndirectVariable{base=insert base, offset=insert offset}

            |   insert(TupleVariable(vars, length)) =
                let
                    fun insertTuple(VarTupleSingle{source, destOffset}) =
                            VarTupleSingle{source=insert source, destOffset=insert destOffset}
                    |   insertTuple(VarTupleMultiple{base, length, destOffset, sourceOffset}) =
                            VarTupleMultiple{base=insert base, length=insert length,
                                             destOffset=insert destOffset, sourceOffset=insert sourceOffset}
                in
                    TupleVariable(mapright insertTuple vars, insert length)
                end

            |   insert(Declar _) = raise InternalError "insert:Declar"
            |   insert(MutualDecs _) = raise InternalError "insert:MutualDecs"
            |   insert(KillItems _) = raise InternalError "insert:KillItems"

          and copyCond (condTest, condThen, condElse) =
            let
                (* Process each of the arms, computing the kill sets for
                 each arm. *)
                (* Save the current usage set.  Because we process the
                   codetree in reverse order to the control flow entries
                   in here show the variables which are in use after the
                   if-expression has completed. *)
                val usagesAfterIf = saveUsages();

                (* Process the then-part.  Save the usage set which
                   corresponds to variables which are in use in the
                   flow of control through the then-part and afterwards. *)
                val insThen = insert condThen;
                val thenUsage = saveUsages();

                (* Reset the use-counts to the saved value. *)
                val () = setToSaved usagesAfterIf;

                (* Process the else-part. *)
                val insElse = insert condElse;
                val elseUsage = saveUsages();

                (* Now compute the differences of the sets.
                   The differences are returned as Extract codetree entries. *)
                val (killElseOnly, killThenOnly) =
                    case computeKillSetLists [thenUsage, elseUsage] of
                        [thenKill, elseKill] => (thenKill, elseKill)
                    |   _ => raise InternalError "computeKillSets"
                (* Now ensure that all the variables that were used in the
                 then-part are marked as used.  It may be that they have already
                 been set if they also appeared in the else-part.
                 This sets the usage sets to the union of the then-part,
                 the else-part and code after the if-expression. *)
                val () = addFromSaved thenUsage

                (* Add kill entries to the other branch.  We simply add
                   Extract entries with lastRef=true before the appropriate
                   branch.  This does what we want since the code-generator
                   does not generate any code for them but it might make
                   the intermediate code easier to read if we used a different
                   instruction. *)

                (* Process the condition AFTER the then- and else-parts. *)
                val insFirst = insert condTest
            in
                mkIf (insFirst, addKillSet(insThen, killElseOnly), addKillSet(insElse, killThenOnly))
            end
        in     
            insert pt
        end (* copyCode *)
         
        val insertedCode = copyCode (pt, Array.array(0, 0), localAddressCount);
    in
        insertedCode
    end (* lifeTimes *)

    structure Sharing = struct type codetree = codetree end
end;
