(*
    Copyright (c) 2012-13 David C.J. Matthews

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

functor CODETREE_STATIC_LINK_AND_CASES(
    structure BASECODETREE: BaseCodeTreeSig
    structure CODETREE_FUNCTIONS: CodetreeFunctionsSig

    structure GCODE :
    sig
      type backendIC
      type machineWord = Address.machineWord
      val gencode: backendIC * Universal.universal list * int -> unit -> machineWord
      structure Sharing: sig type backendIC = backendIC end
    end

    structure DEBUG :
    sig
        val codetreeAfterOptTag:    bool Universal.tag (* If true then print the optimised code. *)
        val maxInlineSizeTag:       int  Universal.tag
        val getParameter : 'a Universal.tag -> Universal.universal list -> 'a
    end

    structure PRETTY : PRETTYSIG

    structure BACKENDTREE: BackendIntermediateCodeSig

    sharing
        BASECODETREE.Sharing
    =   CODETREE_FUNCTIONS.Sharing
    =   GCODE.Sharing
    =   PRETTY.Sharing
    =   BACKENDTREE.Sharing
) : 
sig
    type codetree
    type machineWord = Address.machineWord
    val codeGenerate: codetree * int * Universal.universal list -> unit -> machineWord
    structure Sharing : sig type codetree = codetree end
end
=
struct

    open BASECODETREE
    open Address

    datatype caseType = datatype BACKENDTREE.caseType

    exception InternalError = Misc.InternalError

    (* Internal version of code-tree used as the result of the earlier (closure/case)
       pass and the input to the later (variable lifetimes) pass. *)

    datatype p2Codetree =
        P2MatchFail    (* Pattern-match failure *)
    
    |   P2AltMatch of p2Codetree * p2Codetree(* Pattern-match alternative choices *)

    |   P2Newenv of p2CodeBinding list * p2Codetree (* Set of bindings with an expression. *)

    |   P2Constnt of machineWord (* Load a constant *)

    |   P2Extract of p2LoadForm (* Get a local variable, an argument or a closure value *)
    
    |   P2Field of {base: p2Codetree, offset: int }
         (* Load a field from a tuple *)
    
    |   P2Eval of (* Evaluate a function with an argument list. *)
        {
            function:  p2Codetree,
            argList:   (p2Codetree * argumentType) list,
            resultType: argumentType
        }
    
    |   P2Lambda of p2LambdaForm (* Lambda expressions. *)

    |   P2Cond of p2Codetree * p2Codetree * p2Codetree (* If-statement *)

    |   P2Case of (* Case expressions *)
        {
            cases   : (p2Codetree * word) list,
            test    : p2Codetree,
            caseType: caseType,
            default : p2Codetree
        }
    
    |   P2BeginLoop of (* Start of tail-recursive inline function. *)
        { loop: p2Codetree, arguments: (int * p2Codetree * argumentType) list }

    |   P2Loop of (p2Codetree * argumentType) list (* Jump back to start of tail-recursive function. *)

    |   P2Raise of p2Codetree (* Raise an exception *)

    |   P2Ldexc (* Load the exception (used at the start of a handler) *)

    |   P2Handle of (* Exception handler. *) { exp: p2Codetree, handler: p2Codetree }

    |   P2Tuple of p2Codetree list (* Records (tuples) *)

    |   P2Container of int (* Create a container for a tuple on the stack. *)

    |   P2SetContainer of (* Copy a tuple to a container. *)
        {
            container: p2Codetree,
            tuple:     p2Codetree,
            size:      int
        }

    |   P2TupleFromContainer of p2Codetree * int (* Make a tuple from the contents of a container. *)

    |   P2TagTest of { test: p2Codetree, tag: word, maxTag: word }

    |   P2IndirectVariable of { base: p2Codetree, offset: p2Codetree }
        (* Similar to Indirect except the offset is a variable. *)

    |   P2TupleVariable of p2VarTuple list * p2Codetree (* total length *)
        (* Construct a tuple using one or more multi-word items. *)
 
    and p2CodeBinding =
        P2Declar  of int * p2Codetree (* Make a local declaration or push an argument *)
    |   P2RecDecs of (int * p2LambdaForm) list (* Set of mutually recursive declarations. *)
    |   P2NullBinding of p2Codetree (* Just evaluate the expression and discard the result. *)

    and p2VarTuple =
        P2VarTupleSingle of { source: p2Codetree, destOffset: p2Codetree }
    |   P2VarTupleMultiple of
            { base: p2Codetree, length: p2Codetree, destOffset: p2Codetree, sourceOffset: p2Codetree }

    and p2LoadForm =
        P2LoadLocal of int (* Local binding *)
    |   P2LoadArgument of int (* Argument - 0 is first arg etc.*)
    |   P2LoadClosure of int (* Closure - 0 is first closure item etc *)
    |   P2LoadRecursive (* Recursive call *)

    withtype p2LambdaForm =
    { (* Lambda expressions. *)
        body          : p2Codetree,
        name          : string,
        closure       : p2Codetree list,
        argTypes      : argumentType list,
        resultType    : argumentType,
        level         : int,
        localCount    : int,
        makeClosure   : bool
    }

    val ioOp : int -> machineWord = RunCall.run_call1 RuntimeCalls.POLY_SYS_io_operation

    local
        open BACKENDTREE

        fun mkDecRef(ct, i1, i2) =
            BICDeclar{value = ct, addr = i1, references = i2}

        fun sideEffectFree (P2Lambda _) = true
        |   sideEffectFree (P2Constnt _) = true
        |   sideEffectFree (P2Extract _) = true
        |   sideEffectFree (P2Cond(i, t, e)) =
              sideEffectFree i andalso
              sideEffectFree t andalso
              sideEffectFree e
        |   sideEffectFree (P2Newenv(decs, exp)) = List.all sideEffectBinding decs andalso sideEffectFree exp
        |   sideEffectFree (P2Handle { exp, handler }) =
              sideEffectFree exp andalso sideEffectFree handler
        | sideEffectFree (P2Tuple recs) = testList recs
        | sideEffectFree (P2Field{base, ...}) = sideEffectFree base

            (* An RTS call, which may actually be code which is inlined
               by the code-generator, may be side-effect free.  This can
               occur if we have, for example, "if exp1 orelse exp2"
               where exp2 can be reduced to "true", typically because it's
               inside an inline function and some of the arguments to the
               function are constants.  This then gets converted to
               (exp1; true) and we can eliminate exp1 if it is simply
               a comparison. *)
        | sideEffectFree (P2Eval{function=P2Constnt w, argList, ...}) =
            isIoAddress(toAddress w) andalso CODETREE_FUNCTIONS.sideEffectFreeRTSCall w
            andalso List.all (fn (c, _) => sideEffectFree c) argList

        | sideEffectFree(P2Container _) = true
            (* But since SetContainer has a side-effect we'll always create the
               container even if it isn't used.  *)

        | sideEffectFree(P2TupleFromContainer(c, _)) = sideEffectFree c

        | sideEffectFree(P2IndirectVariable{base, ...}) =
                (* Offset is always side-effect free. *)
                sideEffectFree base

        | sideEffectFree(P2TupleVariable(vars, _ (* length - always side-effect free *))) =
            let
                fun testTuple(P2VarTupleSingle{source, ...}) = sideEffectFree source
                |   testTuple(P2VarTupleMultiple{base, ...}) = sideEffectFree base
            in
                List.all testTuple vars
            end

        | sideEffectFree _ = false
                 (* Rest are unsafe (or too rare to be worth checking) *)

        and testList t = List.all sideEffectFree t
    
        and sideEffectBinding(P2Declar(_, value)) = sideEffectFree value
        |   sideEffectBinding(P2RecDecs _) = true (* These should all be lambdas *)
        |   sideEffectBinding(P2NullBinding c) = sideEffectFree c

    in
        (* This function transforms the codetree to add "lifetime" information
           for bindings ("declarations").  The idea is to aid the code-generator
           by indicating when a binding is no longer required and also to
           distinguish short-lived bindings from longer-lived bindings
           when deciding on register allocation.
           It also causes unused bindings to be discarded if they are
           not used and have no side-effects.  The previous passes may
           generate extra bindings and rely on this pass to remove them
           if they are not actually used. *)

        fun lifeTimes (pt: p2Codetree, localAddressCount): backendIC =
        let
            fun copyCode (pt: p2Codetree, argUses, localCount): backendIC =
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
                                    else if addr = 0 then BICExtract(BICLoadRecursive, true) :: l
                                    else BICExtract(BICLoadArgument(addr-1), true) :: l) [] sumArgSet
                        in
                            Vector.foldli (fn (addr, n, l) =>
                                    if n = 0 orelse n = fullSet orelse Vector.sub(locals, addr) <> 0 orelse Array.sub(outsideLoop, addr)
                                    then l 
                                    else BICExtract(BICLoadLocal addr, true) :: l) killArgs sumLocalSet
                        end
                    in
                        List.map computeKills usages
                    end
                end

                fun addKillSet(original, []): backendIC = original (* No change *)
                |   addKillSet(BICNewenv(decs, exp), killSet) = BICNewenv(map BICNullBinding killSet @ decs, exp)
                |   addKillSet(original, killSet) = BICNewenv(map BICNullBinding killSet, original)

                (* returns the translated node *)
                fun locaddr (P2LoadArgument argNo) =
                    let (* parameters *)
                        val () = instrCount := !instrCount+1;
                        val lastReference =
                            Array.sub(argUses, argNo+1 (* Zero is used for recursive *)) = 0 andalso not (!outsideLoopRef)
                    in 
                        (* Mark the argument as used. *)
                        Array.update (argUses, argNo+1, maxUse(! instrCount, Array.sub(argUses, argNo+1)));
                        BICExtract(BICLoadArgument argNo, lastReference)
                    end

                | locaddr (P2LoadLocal laddr) =
                    let 
                        val () = instrCount := !instrCount+1;
                        (* If this was outside a loop we can't mark this as the last
                           reference because it needs to be retained for the next time round. *)
                        val lastReference =
                            Array.sub(localUses, laddr) = 0 andalso not (Array.sub(outsideLoop, laddr))
                    in
                        Array.update (localUses, laddr, maxUse(! instrCount, Array.sub(localUses, laddr)));
                        BICExtract(BICLoadLocal laddr, lastReference)
                    end

                | locaddr P2LoadRecursive =
                    let
                        val () = instrCount := !instrCount+1;
                        val lastReference = Array.sub(argUses, 0) = 0 andalso not (!outsideLoopRef)
                    in
                        (* Mark the closure as used. *)
                        Array.update (argUses, 0, maxUse(! instrCount, Array.sub(argUses, 0)));
                        BICExtract (BICLoadRecursive, lastReference)
                    end

                | locaddr (P2LoadClosure laddr) =
                    let
                        val () = instrCount := !instrCount+1;
                        val lastReference = Array.sub(argUses, 0) = 0 andalso not (!outsideLoopRef)
                    in
                        (* Mark the closure as used. *)
                        Array.update (argUses, 0, maxUse(! instrCount, Array.sub(argUses, 0)));
                        BICExtract (BICLoadClosure laddr, lastReference)
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

                fun insert P2MatchFail = BICMatchFail
          
                |   insert(P2AltMatch(x, y)) =
                    let
                        val insY = insert y
                        val insX = insert x
                    in
                        BICAltMatch (insX, insY)
                    end

                |   insert(P2Eval { function as P2Extract(P2LoadLocal addr), argList, resultType, ...}) =
                    let
                        (* If this is a statically-linked function make references to the closure.
                           If this is actually the last reference this may result in returning
                           kill entries for some of the closure entries.  If we're in a loop we will
                           still have made a reference and the kill entries will be put at the end of
                           the loop. *)
                        val closureKills =
                            let
                                fun getKill (P2Extract ext) =
                                    (
                                        case locaddr ext of
                                            ext as BICExtract(_, true) => SOME ext
                                        |   _ => NONE
                                    )
                                |   getKill _ = NONE
                            in
                                List.mapPartial getKill (Array.sub(slClosureTable, addr))
                            end
                        val () = instrCount := !instrCount+1
                        (* Process the arguments first. *)
                        val newargs = mapright(fn (c, t) => (insert c, t)) argList
                        val eval =
                            BICEval {function = insert function, argList = newargs, resultType=resultType}
                    in
                        if null closureKills then eval
                        else BICKillItems{expression=eval, killSet=closureKills, killBefore=false}
                    end

                |   insert(P2Eval { function, argList, resultType, ...}) =
                    let
                        (* Process the arguments first. *)
                        val newargs = mapright(fn (c, t) => (insert c, t)) argList
                        (* Then the body. *)
                        val func = insert function
                    in
                        BICEval {function = func, argList = newargs, resultType=resultType}
                    end

                |   insert(P2Extract ext) = locaddr ext

                |   insert(P2Field {base, offset}) = BICField {base = insert base, offset = offset}

                |   insert(P2Constnt c) = BICConstnt c (* Constants can be returned untouched. *)

                |   insert(P2BeginLoop{loop=body, arguments=argList, ...}) = (* Start of tail-recursive inline function. *)
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
                        fun declareArg(caddr, _, _) =
                        (
                            Array.update (localUses, caddr, 0);
                            Array.update (outsideLoop, caddr, false) (* Must do this. *)
                        )
                        val _ = List.app declareArg argList

                        (* Process the body. *)
                        val insBody = insert body

                        (* We want to set the arguments to "unreferenced".  These are effectively local to
                           the loop so they can safely be killed inside it.  However we want to record
                           the final references so we can attach them to the declarations. *)
                        local
                            fun processDec(addr, _, _) =
                                Array.sub(localUses, addr) before Array.update (localUses, addr, 0)
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
                            fun extendLife(BICExtract(BICLoadArgument addr, _)) =
                                    Array.update (argUses, addr+1, loopEndPosition)
                            |   extendLife(BICExtract(BICLoadLocal addr, _)) =
                                    Array.update (localUses, addr, loopEndPosition)
                            |   extendLife(BICExtract(BICLoadRecursive, _)) =
                                    Array.update (argUses, 0, loopEndPosition)
                            |   extendLife _ = raise InternalError "Not an Extract"
                        in
                            val () = List.app extendLife killAfter
                        end
                        (* Finally the initial argument values. *)
                        local
                            fun copyDec((addr, value, _), uses) =
                                {addr=addr, value=insert value, references=uses}
                        in
                            val newargs = mapright copyDec (ListPair.zipEq(argList, loopArgUses))
                        end
                        val loop =
                            BICBeginLoop{loop=insBody, arguments=ListPair.zipEq(newargs, List.map #3 argList)}
                    in
                        (* Add the kill entries on after the loop. *)
                        if null killAfter then loop
                        else BICKillItems{expression=loop, killSet=killAfter, killBefore=false}
                    end
    
                |   insert(P2Loop argList) = (* Jump back to start of tail-recursive function. *)
                            BICLoop(mapright(fn (c, t) => (insert c, t)) argList)

                |   insert(P2Raise x) = BICRaise (insert x)

                    (* See if we can use a case-instruction. Arguably this belongs
                       in the optimiser but it is only really possible when we have
                       removed redundant declarations. *)
                |   insert(P2Cond(condTest, condThen, condElse)) =
                            copyCond (condTest, condThen, condElse)

                |   insert(P2Newenv(ptElistDecs, ptExp)) =
                    let
                        (* Process the body. Recurses down the list of declarations
                           and expressions processing each, and then reconstructs the
                           list on the way back. *)
                        fun copyDeclarations ([])   = []

                        |   copyDeclarations ((P2Declar(caddr, pt)) :: vs)  =
                            let
                                (* Set the table entries.  We don't reuse entries so this is just a check. *)
                                val _ = Array.sub(localUses, caddr) <> 0
                                           andalso raise InternalError "copyDeclarations: Already used"
                                val () = Array.update (outsideLoop, caddr, false) (* It's local *)
                                val () =
                                    case pt of
                                        P2Lambda{makeClosure=false, closure, ...} =>
                                        let
                                            (* It's a statically-linked function: set the closure
                                               array entry.  If the closure entries themselves are
                                               statically linked function we have to include the
                                               items from those closures in this set. *)
                                            fun closureTrans (P2Extract(P2LoadLocal addr), l) = Array.sub(slClosureTable, addr) @ l
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

                        |   copyDeclarations (P2RecDecs mutualDecs :: vs)  =
                            let
                                (* Recursive declarations. *)
                                (* This is a bit messy.  For static-link functions we need to
                                   make sure the last reference to the closure entries is after
                                   the last call to the function.  For full-closure functions
                                   the last reference must be after the closures have all been
                                   built. *)
                                (* Get the closure lists for all the declarations.  We assume that
                                   any of these can call any of the others so we just accumulate
                                   them into a single list. *)
                                local
                                    fun getClosure((_, {makeClosure, closure, ...}),
                                            (slClosures, fcClosures)) =
                                        if makeClosure
                                        then (slClosures, closure @ fcClosures) else (closure @ slClosures, fcClosures)
                                    val (slClosures, fcClosures) = List.foldl getClosure ([], []) mutualDecs
                                    (* Include any statically linked functions this references. *)
                                    fun closureTrans (P2Extract(P2LoadLocal addr), l) = Array.sub(slClosureTable, addr) @ l
                                    |   closureTrans (_, l) = l
                                    val trans = List.foldl closureTrans [] slClosures
                                in
                                    val staticCl = trans @ slClosures
                                    val fullClosureList = fcClosures
                                end

                                (* Make the declarations. *)
                                local
                                    fun applyFn(caddr, _) =     
                                        (
                                            Array.sub(localUses, caddr) <> 0 andalso raise InternalError "applyFn: Already used";
                                            Array.update(slClosureTable, caddr, staticCl);
                                            Array.update (outsideLoop, caddr, false) (* It's local *)
                                        )
                                in
                                    val () = List.app applyFn mutualDecs
                                end
                  
                                (* Process the rest of the block. Identifies all other
                                   references to these declarations. *)
                                val restOfBlock = copyDeclarations vs

                                val () = instrCount := !instrCount+1

                                (* Process the closure entries and extract the ones that are the last refs. *)
                                val lastRefsForClosure =
                                    List.map BICNullBinding
                                        (List.filter (fn BICExtract(_, true) => true | _ => false)
                                            (map insert fullClosureList))

                                val copiedDecs =
                                    map (fn (addr, lambda) => {addr=addr, lambda=copyLambda lambda, references= 0})
                                        mutualDecs
           
                                (* Now we know all the references we can complete
                                   the declaration and put on the use-count. *)
                                fun copyEntries []      = []
                                |   copyEntries ({ addr, lambda, ...} ::ds) =
                                    let
                                        val wasUsed = Array.sub(localUses, addr)
                                    in
                                        if wasUsed = 0 (*andalso sideEffectFree value*)
                                        then copyEntries ds
                                        else 
                                        (
                                            (* Set the use count back to false otherwise this
                                               entry would become part of the kill set for the
                                               surrounding expression. *)
                                            Array.update(localUses, addr, 0);
                                            {lambda=lambda, addr=addr, references=wasUsed} :: copyEntries ds
                                        )
                                    end

                                val decs = copyEntries copiedDecs
                            in
                                (* Return the mutual declarations and the rest of the block. *)
                                if null decs
                                then lastRefsForClosure @ restOfBlock
                                else BICRecDecs decs :: (lastRefsForClosure @ restOfBlock)
                            end (* copyDeclarations.isMutualDecs *)

                        |   copyDeclarations (P2NullBinding v :: vs)  =
                            let (* Not a declaration - process this and the rest. *)
                               (* Must process later expressions before earlier
                                   ones so that the last references to variables
                                   are found correctly. *)
                                val copiedRest = copyDeclarations vs
                                val copiedNode = insert v
                            in
                                (* Expand out blocks *)
                                case copiedNode of
                                    BICNewenv(decs, exp) => decs @ (BICNullBinding exp :: copiedRest)
                                |   _ => BICNullBinding copiedNode :: copiedRest
                            end (* copyDeclarations *)

                        val insElist = copyDeclarations(ptElistDecs @ [P2NullBinding ptExp])

                        (* TODO: Tidy this up. *)
                        fun splitLast _ [] = raise InternalError "decSequenceWithFinalExp: empty"
                        |   splitLast decs [BICNullBinding exp] = (List.rev decs, exp)
                        |   splitLast _ [_] = raise InternalError "decSequenceWithFinalExp: last is not a NullDec"
                        |   splitLast decs (hd::tl) = splitLast (hd:: decs) tl

                    in
                        case splitLast [] insElist of
                            ([], exp) => exp
                        |   (decs, exp) => BICNewenv(decs, exp)
                    end (* isNewEnv *)
                
                |   insert(P2Tuple recs) = (* perhaps it's a constant now? *)
                        BICTuple (mapright insert recs) 

                |   insert P2Ldexc = BICLdexc (* just a constant so return it *)
      
                |   insert(P2Lambda lambda) = BICLambda(copyLambda lambda)
    
                |   insert(P2Handle { exp, handler }) =
                    let
                        (* The order here is important.  We want to make sure that
                           the last reference to a variable really is the last. *)
                       val hand = insert handler
                       val exp = insert exp
                    in
                      BICHandle {exp = exp, handler = hand}
                    end

                |   insert(P2Container c) = BICContainer c

                |   insert(P2SetContainer {container, tuple, size}) =
                        BICSetContainer{container = insert container, tuple = insert tuple, size = size}

                |   insert(P2TupleFromContainer(container, size)) =
                        BICTupleFromContainer(insert container, size)

                |   insert(P2TagTest{test, tag, maxTag}) = BICTagTest{test=insert test, tag=tag, maxTag=maxTag}
            
                |   insert(P2Case{cases, test, caseType, default}) =
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
                        BICCase{cases=casePlusKills, test=insert test, caseType=caseType,
                             default=addKillSet(insDefault, hd kills)}
                    end

                |   insert(P2IndirectVariable{base, offset}) =
                        BICIndirectVariable{base=insert base, offset=insert offset}

                |   insert(P2TupleVariable(vars, length)) =
                    let
                        fun insertTuple(P2VarTupleSingle{source, destOffset}) =
                                BICVarTupleSingle{source=insert source, destOffset=insert destOffset}
                        |   insertTuple(P2VarTupleMultiple{base, length, destOffset, sourceOffset}) =
                                BICVarTupleMultiple{base=insert base, length=insert length,
                                                 destOffset=insert destOffset, sourceOffset=insert sourceOffset}
                    in
                        BICTupleVariable(mapright insertTuple vars, insert length)
                    end

                and copyLambda{body=lambdaBody, level=nesting, argTypes,
                                 name=lambdaName, resultType, localCount, closure, makeClosure, ...} = 
                let
                    val numArgs = List.length argTypes
                    (* The size is one more than the number of arguments because the
                       first item is used to represent the closure.  Argument n is represented
                       by index n+1.  This is a historical anomaly and ought to be fixed
                       by using a separate ref for the closure. *)
                    val argUses      = Array.array(numArgs+1, 0)

                    (* process the body *)
                    val insertedCode: backendIC = copyCode (lambdaBody, argUses, localCount)
                    (* All the closure entries ought to be loads but there used to be cases
                       where functions were compiled late in the process and then appeared
                       as constants in the closure.  Include this check for the moment. *)
                    val copiedClosure =
                        map(fn BICExtract a => BICExtract a | _ => raise InternalError "map closure") (mapright insert closure)
            
                    val argUseList = Array.foldr(op ::) [] argUses
                in
                    {
                        body          = insertedCode,
                        name          = lambdaName,
                        closure       = copiedClosure,
                        argTypes      = argTypes,
                        resultType    = resultType,
                        level         = nesting,
                        closureRefs   = hd argUseList,
                        localCount    = localCount,
                        makeClosure   = makeClosure,
                        argLifetimes  = tl argUseList (* Exclusde the first item used for the closure itself. *)
                    }
                end


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
                    BICCond (insFirst, addKillSet(insThen, killElseOnly), addKillSet(insElse, killThenOnly))
                end
            in     
                insert pt
            end (* copyCode *)
         
            val insertedCode: backendIC = copyCode (pt, Array.array(0, 0), localAddressCount);
        in
            insertedCode
        end (* lifeTimes *)
    
    end (* local *)

    fun codeGenAndPrint debugSwitches (code, localCount) =
    let
        val backendCode = lifeTimes(code, localCount)
    in
        if DEBUG.getParameter DEBUG.codetreeAfterOptTag debugSwitches
        then PRETTY.getCompilerOutput debugSwitches (BACKENDTREE.pretty backendCode) else ();
        GCODE.gencode(backendCode, debugSwitches, localCount)
    end

    fun staticLinkAndCases (pt, localAddressCount, debugArgs) =
    let
        fun mkEval (ct, clist)   =
        P2Eval {
            function = ct,
            argList = List.map(fn c => (c, GeneralType)) clist,
            resultType=GeneralType
        }

        val rtsFunction = P2Constnt o ioOp

        fun copyCode (pt, previous, localCount, localAddresses, numberOfArgs) =
        let
            (* "closuresForLocals" is a flag indicating that if the declaration
               is a function a closure must be made for it. *)
            val closuresForLocals = Array.array(localCount, false);

            (* used to indicate whether a local declaration is really
             a constant, so that we can in-line it. *)
            val localConsts       = Array.array (localCount, NONE)
            
            val newLocalAddresses = Array.array (localCount, 0)

            fun locaddr (LoadLegacy{ addr, level = 0, fpRel = true, ...}, closure) =
                if addr < 0
                then P2Extract(P2LoadArgument(numberOfArgs + addr))
          
                (* isOnstack *)
                else locaddr(LoadLocal addr, closure)

            |   locaddr (ptr as LoadLegacy{ level, ...}, closure) = previous (ptr, level, closure)
            
            |   locaddr (LoadLocal addr, closure) =
                (
                    case Array.sub(localConsts, addr) of
                        SOME c => c (* just return the constant *)
                    |   NONE =>
                        let
                            val () =
                                if closure then Array.update (closuresForLocals, addr, true) else ()
                            val newAddr = Array.sub(newLocalAddresses, addr)
                            val _ = newAddr <> 0 orelse raise InternalError "copyCode: Not set"
                        in
                            P2Extract(P2LoadLocal newAddr)
                        end
                )

            |   locaddr _ = raise InternalError "locaddr: TODO"

            fun makeDecl addr =
            (let
                val newAddr = ! localAddresses before (localAddresses := !localAddresses+1)
                val () = Array.update (closuresForLocals, addr, false)
                val () = Array.update (newLocalAddresses, addr, newAddr)
            in
                newAddr
            end handle Subscript => raise InternalError("makeDecl " ^ Int.toString addr ^ " of " ^ Int.toString localCount))

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

            fun insert MatchFail = P2MatchFail
          
            |   insert(AltMatch(x, y)) =
                let
                    val insY = insert y
                    val insX = insert x
                in
                    P2AltMatch (insX, insY)
                end
        
            |   insert(Eval { function, argList, resultType, ...}) =
                let
                    (* Process the arguments first. *)
                    val newargs = mapright(fn (c, t) => (insert c, t)) argList
                    val func  =
                    case function of
                        Extract ext => locaddr(ext, (* closure = *) false)
                    |   first => insert first
                in
                    (* If we are calling a function which has been declared this
                       does not require it to have a closure. Any other use of the
                       function would. *) 
                    P2Eval {function = func, argList = newargs, resultType=resultType}
                end

            |   insert(Extract ext) =
                    (* Load the value bound to an identifier. The closure flag is
                       set to true since the only cases where a closure is not needed,
                       eval and load-andStore, are handled separately. *)
                    locaddr(ext, (* closure = *) true)

            |   insert(ExtractWithInline(ext, _, _)) = locaddr(ext, true)

            |   insert(Indirect {base, offset}) = P2Field {base = insert base, offset = offset}

            |   insert(Constnt w) = P2Constnt w (* Constants can be returned untouched. *)

            |   insert(ConstntWithInline(g, _, _)) = P2Constnt g

            |   insert(BeginLoop{loop=body, arguments=argList, ...}) = (* Start of tail-recursive inline function. *)
                let
                    (* Make entries in the tables for the arguments. *)
                    val newAddrs = List.map (fn ({addr, ...}, _) => makeDecl addr) argList

                    (* Process the body. *)
                    val insBody = insert body
                    (* Finally the initial argument values. *)
                    local
                        fun copyDec(({value, ...}, t), addr) =
                                (addr, insert value, t)
                    in
                        val newargs = ListPair.map copyDec (argList, newAddrs)
                    end
                in
                    (* Add the kill entries on after the loop. *)
                    P2BeginLoop{loop=insBody, arguments=newargs}
                end
    
            |   insert(Loop argList) = (* Jump back to start of tail-recursive function. *)
                        P2Loop(List.map(fn (c, t) => (insert c, t)) argList)

            |   insert(Raise x) = P2Raise (insert x)

                (* See if we can use a case-instruction. Arguably this belongs
                   in the optimiser but it is only really possible when we have
                   removed redundant declarations. *)
            |   insert(Cond(condTest, condThen, condElse)) =
                        reconvertCase(copyCond (condTest, condThen, condElse))

            |   insert(Newenv(ptElist, ptExp)) =
                let
                    (* Process the body. Recurses down the list of declarations
                       and expressions processing each, and then reconstructs the
                       list on the way back. *)
                    fun copyDeclarations ([])   = []

                    |   copyDeclarations (Declar({addr=caddr, value = pt, ...}) :: vs)  =
                        let
                            val newAddr = makeDecl caddr
                            val () =
                                case pt of
                                    Constnt w => Array.update (localConsts, caddr, SOME(P2Constnt w))
                                |  _ => ()

                            (* This must be done first, even for non-lambdas -  why? *)
                            (* The first declarative might be a set of mutual declarations,
                               and we have to copy all their uses before we can successfully
                               compile them because we need to know whether they will
                               require closures. *)
                            val rest = copyDeclarations vs
                        in
                            let
                                val dec =
                                    case pt of
                                        Lambda lam =>
                                        let 
                                            val closure      = ref (Array.sub(closuresForLocals, caddr));
                                            val (copiedLambda, newClosure) = copyLambda(lam, closure)
                                        in
                                            (* Note: copyLambda may have set closure *)
                                            copyProcClosure(copiedLambda, newClosure, ! closure)
                                        end
                                    |   _ => insert pt
                            in
                                P2Declar(newAddr, dec) :: rest
                            end
                        end (* copyDeclarations.isDeclar *)

                    |   copyDeclarations (RecDecs mutualDecs :: vs)  =
                        let
                          (* Mutually recursive declarations. Any of the declarations
                             may refer to any of the others. This causes several problems
                             in working out the use-counts and whether the functions 
                             (they should be functions) need closures. A function will
                             need a closure if any reference would require one (i.e. does
                             anything other than call it). The reference may be from one
                             of the other mutually recursive declarations and may be 
                             because that function requires a full closure. This means
                             that once we have dealt with any references in the rest of
                             the containing block we have to repeatedly scan the list of
                             declarations removing those which need closures until we
                             are left with those that do not. The use-counts can only be
                             obtained when all the non-local lists have been copied. *)
                  
                           (* First go down the list making a declaration for each entry.
                              This makes sure there is a table entry for all the
                              declarations. *)

                            val _ = List.map (fn {addr, ...} => makeDecl addr) mutualDecs

                            (* Process the rest of the block. Identifies all other
                               references to these declarations. *)
                            val restOfBlock = copyDeclarations vs

                            (* We now want to find out which of the declarations require
                               closures. First we copy all the declarations, except that
                               we don't copy the non-local lists of functions. *)
                            fun copyDec ({addr=caddr, lambda, ...}) = 
                                let
                                    val closure = ref (Array.sub(closuresForLocals, caddr))
                                    val (dec, newClosure)     = copyLambda(lambda, closure)
                                    (* Check whether we now have a constant *)
                                    val () =
                                        case dec of
                                            P2Constnt _ => Array.update (localConsts, caddr, SOME dec)
                                        |   _ => Array.update (localConsts, caddr, NONE); (* needed? *)

                                    (* copyLambda may set "closure" to true. *)
                                    val () = Array.update (closuresForLocals, caddr, !closure);
                                in
                                    (caddr, dec, newClosure)
                                end             

                            val copiedDecs = map copyDec mutualDecs
                   
                            (* We now have identified all possible references to the
                               functions apart from those of the closures themselves.
                               Any of closures may refer to any other function so we must 
                               iterate until all the functions which need full closures
                               have been processed. *)
                            fun processClosures([], outlist, true) =
                                (* Sweep completed. - Must repeat. *)
                                processClosures(outlist, [], false)
            
                            |   processClosures([], outlist, false) =
                                (* We have processed the whole of the list without finding
                                   anything which needs a closure. The remainder do not
                                   need full closures. *)
                                let
                                    fun mkLightClosure ((addr, value, newClosure)) = 
                                        let
                                            val clos = copyProcClosure(value, newClosure, false)
                                            val newAddr = Array.sub(newLocalAddresses, addr)
                                        in
                                            (newAddr, clos)
                                        end          
                                in
                                    map mkLightClosure outlist
                                end
                  
                            |   processClosures((h as (caddr, value, newClosure))::t, outlist, someFound) =
                                if Array.sub(closuresForLocals, caddr)
                                then
                                let (* Must copy it. *)
                                    val clos = copyProcClosure(value, newClosure, true)
                                    val newAddr = Array.sub(newLocalAddresses, caddr)
                                in
                                    (newAddr, clos) :: processClosures(t, outlist, true)
                                end
                                    (* Leave it for the moment. *)
                                else processClosures(t, h :: outlist, someFound)
                  
                            val decs = processClosures(copiedDecs, [], false)

                            local
                                fun isLambda(_, P2Lambda _) = true
                                |   isLambda _ = false
                            in
                                val (lambdas, nonLambdas) = List.partition isLambda decs
                            end
                            fun asMutual(addr, P2Lambda lambda) = (addr, lambda)
                            |   asMutual _ = raise InternalError "asMutual"
                        in
                            (* Return the mutual declarations and the rest of the block. *)
                            if null lambdas
                            then map P2Declar nonLambdas @ restOfBlock         (* None left *)
                            else P2RecDecs (map asMutual lambdas) :: (map P2Declar nonLambdas @ restOfBlock)
                        end (* copyDeclarations.isMutualDecs *)
          
                    |   copyDeclarations (NullBinding v :: vs)  =
                        let (* Not a declaration - process this and the rest. *)
                           (* Must process later expressions before earlier
                               ones so that the last references to variables
                               are found correctly. DCJM 30/11/99. *)
                            val copiedRest = copyDeclarations vs;
                            val copiedNode = insert v
                        in
                            (* Expand out blocks *)
                            case copiedNode of
                                P2Newenv(decs, exp) => decs @ (P2NullBinding exp :: copiedRest)
                            |   _ => P2NullBinding copiedNode :: copiedRest
                        end (* copyDeclarations *)

                    val insElist = copyDeclarations(ptElist @ [NullBinding ptExp])

                    fun mkEnv([], exp) = exp
                    |   mkEnv(decs, exp) = P2Newenv(decs, exp)

                    fun decSequenceWithFinalExp decs =
                    let
                        fun splitLast _ [] = raise InternalError "decSequenceWithFinalExp: empty"
                        |   splitLast decs [P2NullBinding exp] = (List.rev decs, exp)
                        |   splitLast _ [_] = raise InternalError "decSequenceWithFinalExp: last is not a NullDec"
                        |   splitLast decs (hd::tl) = splitLast (hd:: decs) tl
                    in
                        mkEnv(splitLast [] decs)
                    end
                in
                    (* TODO: Tidy this up. *)
                    decSequenceWithFinalExp insElist
                end (* isNewEnv *)
                
            |   insert(Recconstr recs) = (* perhaps it's a constant now? *)
                    P2Tuple (mapright insert recs)

            |   insert Ldexc = P2Ldexc (* just a constant so return it *)
      
            |   insert(Lambda lam)=
                let
                    val (copiedLambda, newClosure) = copyLambda(lam, ref true)
                    (* Must make a closure for this function because
                        it is not a simple declaration. *)
                in
                    copyProcClosure (copiedLambda, newClosure, true)
                end

            |   insert(LambdaWithInline(lam, _, _)) = insert(Lambda lam)

            |   insert(Handle { exp, handler }) =
                let
                    (* The order here is important.  We want to make sure that
                       the last reference to a variable really is the last. *)
                    val hand = insert handler
                    val exp = insert exp
                in
                    P2Handle {exp = exp, handler = hand}
                end

            |   insert(Container c) = P2Container c

            |   insert(SetContainer {container, tuple, size}) =
                    P2SetContainer{container = insert container, tuple = insert tuple, size = size}

            |   insert(TupleFromContainer(container, size)) =
                    P2TupleFromContainer(insert container, size)

            |   insert(TagTest{test, tag, maxTag}) = P2TagTest{test=insert test, tag=tag, maxTag=maxTag}

            |   insert(IndirectVariable{base, offset, ...}) =
                (* Convert this into a Load instruction. *)
                    insert(CODETREE_FUNCTIONS.mkEval(Constnt(ioOp RuntimeCalls.POLY_SYS_load_word), [base, offset]))

            |   insert(TupleVariable(vars, length)) =
                (* Convert this into a set of RTS calls.  This currently uses POLY_SYS_alloc_store
                   which initialises the store before the copy operations.  It may be possible to
                   avoid this duplication somehow. *)
                let
                    val newAddr = ! localAddresses before (localAddresses := !localAddresses+1)
                    val mutableFlags = Word8.orb(F_words, F_mutable)
                    val allocTuple =
                        P2Declar(newAddr,
                            mkEval(rtsFunction RuntimeCalls.POLY_SYS_alloc_store,
                                [insert length, P2Constnt (toMachineWord mutableFlags), P2Constnt(toMachineWord 0)])
                        )

                    val loadLocal = P2Extract(P2LoadLocal newAddr)

                     fun copyTuple(VarTupleSingle{source, destOffset}) =
                            mkEval(rtsFunction RuntimeCalls.POLY_SYS_assign_word,
                                [loadLocal, insert destOffset, insert source])
                    |   copyTuple(VarTupleMultiple{base, length, destOffset, sourceOffset}) =
                            mkEval(rtsFunction RuntimeCalls.POLY_SYS_move_words,
                                [insert base, insert sourceOffset, loadLocal,
                                 insert destOffset, insert length])
                    (* Remove the mutable bit (needed by alloc_store). *)
                    val lock = mkEval(rtsFunction RuntimeCalls.POLY_SYS_lockseg, [loadLocal])
                 in
                    P2Newenv(allocTuple :: (map P2NullBinding (mapright copyTuple vars @ [lock])), loadLocal)
                end

          and copyCond (condTest, condThen, condElse) =
            let
              (* Process the then-part. *)
              val insThen = insert condThen
              (* Process the else-part.  If it's a conditional process it here. *)
              val insElse =
                case condElse of
                    Cond(i, t, e) => copyCond(i, t, e)
                |   _ => insert condElse
              (* Process the condition after the then- and else-parts. *)
              val insFirst = insert condTest
          
              type caseVal =
                { tag: word, test: codetree, caseType: caseType } option;
        
              (* True if both instructions are loads or indirections with the
                 same effect. More complicated cases could be considered but
                 function calls must always be treated as different.
                 Note: the reason we consider Indirect entries here
                 as well as Extract is because we (used to) defer Indirect entries.  *)
              datatype similarity = Different | Similar of p2LoadForm

              fun similar (P2Extract a, P2Extract b) = if a = b then Similar a else Different
              
               |  similar (P2Field{offset=aOff, base=aBase}, P2Field{offset=bOff, base=bBase}) =
                    if aOff <> bOff then Different else similar (aBase, bBase)
              
               |  similar _ = Different;

                (* If we have a call to the int equality operation *)
                (* then we may be able to use a case statement. *)
                fun findCase (P2Eval{ function=P2Constnt cv, argList, ... }) =
                let
                    val isArbitrary = wordEq (cv, ioOp RuntimeCalls.POLY_SYS_equala)
                    val isWord = wordEq (cv, ioOp RuntimeCalls.POLY_SYS_word_eq)
                in
                    if isArbitrary orelse isWord
                    then  (* Should be just two arguments. *)
                    case argList of
                        [(P2Constnt c1, _), (arg2, _)] =>
                        if isShort c1
                        then SOME{tag=toShort c1, test=arg2, caseType = if isArbitrary then CaseInt else CaseWord}
                        else NONE (* Not a short constant. *)
                    
                     | [(arg1, _), (P2Constnt c2, _)] =>
                        if isShort c2
                        then SOME{tag=toShort c2, test=arg1, caseType = if isArbitrary then CaseInt else CaseWord}
                        else NONE (* Not a short constant. *)
                    
                    | _ => NONE
                       (* Wrong number of arguments - should raise exception? *)
                
                    else NONE (* Function is not a comparison. *)
                end

             |  findCase(P2TagTest { test, tag, maxTag }) =
                    SOME { tag=tag, test=test, caseType=CaseTag maxTag }

             |  findCase _ = NONE
        
              val testCase = findCase insFirst
            in

              case testCase of
                    NONE => (* Can't use a case *)
                        P2Cond (insFirst, insThen, reconvertCase insElse)
                |   SOME { tag=caseTags, test=caseTest, caseType=caseCaseTest } =>
                        (* Can use a case. Can we combine two cases?
                          If we have an expression like 
                               "if x = a then .. else if x = b then ..."
                          we can combine them into a single "case". *)
                        case insElse of
                            P2Case { cases=nextCases, test=nextTest, default=nextDefault, caseType=nextCaseType } =>
                            (
                                case (similar(nextTest, caseTest), caseCaseTest = nextCaseType) of
                                  (* Note - it is legal (though completely redundant) for the
                                     same case to appear more than once in the list. This is not
                                      checked for at this stage. *)
                                    (Similar _, true) =>
                                        P2Case 
                                        {
                                            cases   = (insThen, caseTags) ::
                                                        map (fn (c, l) => (c, l)) nextCases,
                                            test    = nextTest,
                                            default = nextDefault,
                                            caseType = caseCaseTest
                                        }

                                    | _ => (* Two case expressions but they test different
                                              variables. We can't combine them. *)
                                        P2Case
                                        {
                                            cases   = [(insThen, caseTags)],
                                            test    = caseTest,
                                            default = reconvertCase insElse,
                                            caseType=caseCaseTest
                                        }
                            )
                            | _ => (* insElse is not a case *)
                                P2Case
                                {
                                    cases   = [(insThen, caseTags)],
                                    test    = caseTest,
                                    default = insElse,
                                    caseType=caseCaseTest
                                }
            end

            (* Check something that's been created as a Case and see whether it is sparse.
               If it is turn it back into a sequence of conditionals.  This was previously
               done at the bottom level and the choice of when to use an indexed case was
               made by the architecture-specific code-generator.  That's probably unnecessary
               and complicates the code-generator. *)
            and reconvertCase(P2Case{cases, test, default, caseType}) =
                let
                    (* Count the number of cases and compute the maximum and minimum. *)
                    (* If we are testing on integers we could have negative values here.
                       Because we're using "word" here any negative values are treated as
                       large positive values and so we won't use a "case".
                       If this is a case on constructor tags we know the range.  There
                       will always be a "default" which may be anywhere in the range but
                       if we construct a jump table that covers all the values we don't need
                       the range checks. *)
                    val useIndexedCase =
                        case caseType of
                            CaseTag _ => (* Exhaustive *) List.length cases > 4
                        |   _ =>
                            let
                                val (_, aLabel) = hd cases
                                fun foldCases((_, w), (min, max)) = (Word.min(w, min), Word.max(w, max))
                                val (min, max) = List.foldl foldCases (aLabel, aLabel) cases
                                val numberOfCases = List.length cases
                            in
                                numberOfCases > 7 andalso numberOfCases >= (Word.toInt max - Word.toInt min) div 3
                            end
                in
                    if useIndexedCase
                    then P2Case{cases=cases, test=test, default=default, caseType=caseType}
                    else
                    let
                        fun reconvert [] = default
                        |   reconvert ((c, t) :: rest) =
                            let
                                val test =
                                    case caseType of
                                        CaseInt =>
                                            mkEval(P2Constnt(ioOp RuntimeCalls.POLY_SYS_equala),
                                                   [test, P2Constnt(toMachineWord t)])
                                    |   CaseWord =>
                                            mkEval(P2Constnt(ioOp RuntimeCalls.POLY_SYS_word_eq),
                                                   [test, P2Constnt(toMachineWord t)])
                                    |   CaseTag maxTag => P2TagTest { test=test, tag=t, maxTag=maxTag }
                            in
                                P2Cond(test, c, reconvert rest)
                            end
                    in
                        reconvert cases
                    end
                end
            |   reconvertCase t = t (* Just a simple conditional. *)
            

            (* If "makeClosure" is true the function will need a full closure.
               It may need a full closure even if makeClosure is false if it
               involves a recursive reference which will need a closure. *)
            and copyLambda ({body=lambdaBody, level=nesting, argTypes,
                             name=lambdaName, resultType, localCount, ...}, makeClosure) =
            let
              val newGrefs: loadForm list ref      = ref [] (* non-local references *)
              val newNorefs     = ref 0  (* number of non-local refs *)
       
              (* A new table for the new function. *)
              fun prev (ptr as LoadLegacy{ addr, fpRel, ...}, lev, closure: bool) =
              let 
                    (* Returns the closure address of the non-local *)
                    fun makeClosureEntry([], _) = (* not found - construct new entry *)
                        let
                            val () = newGrefs :=  LoadLegacy{addr = addr, level = lev - 1, fpRel = fpRel} ::  !newGrefs;
                            val newAddr = !newNorefs + 1;
                        in
                            newNorefs := newAddr; (* increment count *)
                            P2Extract(P2LoadClosure(newAddr-1))
                        end
        
                    |   makeClosureEntry(LoadLegacy{addr=loadAddr, level=loadLevel, fpRel=loadFpRel, ...} :: t, newAddr) =
                        if loadAddr = addr andalso loadLevel = lev - 1 andalso loadFpRel = fpRel
                        then P2Extract(P2LoadClosure(newAddr-1))
                        else makeClosureEntry(t, newAddr - 1)

                    |   makeClosureEntry _ = raise InternalError "makeClosureEntry: TODO"

              in
                (* If we use a function on another level in a way that will
                   require it to have a real closure we must make one for it.
                   (i.e. we must set the "closure" flag.) This is necessary
                   because we may, for example, pass an outer function as a
                   parameter from within an inner function. The inner function
                   may not itself need a closure so the non-local references 
                   it makes will not be forced to have closures, but the outer
                   function will need one. *)
                if lev = 0 (* Reference to the closure itself. *)
                then
                let
                    val () =
                        if addr <> 0 orelse fpRel
                        then raise InternalError "prev: badly-formed load"
                        else ();
            
                    val () = if closure then makeClosure := true else ();
                in
                    P2Extract P2LoadRecursive
                end
        
                else if lev = 1 andalso addr > 0
                then
                let (* local at this level *)
                    val () =
                        if not fpRel
                        then raise InternalError "prev: badly-formed load"
                        else ()
                in 
                    case Array.sub(localConsts, addr) of
                        SOME c => c (* propagate constant, rather than using closure *)
                    |   NONE  =>
                        let
                            val () =
                                if closure 
                                then Array.update (closuresForLocals, addr, true)
                                else ()
                        in
                            makeClosureEntry (!newGrefs, !newNorefs)
                        end
                end
        
                else  if lev = 1 andalso addr < 0
                then
                let (* parameter at this level *)
                    val () =
                        if not fpRel
                        then raise InternalError "prev: badly-formed load"
                        else ();
                in 
                    makeClosureEntry (!newGrefs, !newNorefs)
                end
        
                else
                let (* lev > 1 orelse (lev = 1 andalso addr = 0) *)
                    (* Discard the result, unless it's a constant.
                       We fix up the old (fp-relative) address in the
                       closure list on the second pass. Why not now?
                       That would make it harder to set the makeClosure
                       flag for the closure lists of mutually-recursive
                       definitions. But doesn't doing it this way risks
                       making the refsToClosure count too high? SPF 19/5/95 *)
                    val outerLoad = previous (ptr, lev - 1, closure)
                in
                    case outerLoad of
                        P2Constnt _ => outerLoad
                        |   _ => makeClosureEntry (!newGrefs, !newNorefs)
               end
              end (* prev *)
              | prev _ = raise InternalError "prev: TODO"
      
              (* process the body *)
              val newLocalAddresses = ref 1
              val insertedCode =
                copyCode (lambdaBody, prev, localCount, newLocalAddresses, List.length argTypes)
            in
                case !newGrefs of
                    [] => (* no external references *)
                    let
                        val copiedProc =
                        P2Lambda
                        {
                            body          = insertedCode,
                            name          = lambdaName,
                            closure       = [],
                            argTypes      = argTypes,
                            resultType    = resultType,
                            level         = nesting,
                            localCount    = ! newLocalAddresses,
                            makeClosure   = true
                        }

                        val cnstnt = codeGenAndPrint debugArgs (copiedProc, 1) ()
                    in
                        (* Code generate it now so we get a constant. *)
                        (P2Constnt cnstnt, [])
                    end
      
                |   globalRefs =>
                    (* External references present. The closure will be copied
                        later with copyProcClosure. *)
                    (P2Lambda 
                    {
                        body          = insertedCode,
                        name          = lambdaName,
                        closure       = [],
                        argTypes      = argTypes,
                        resultType    = resultType,
                        level         = nesting,
                        localCount    = ! newLocalAddresses,
                        makeClosure   = false
                    },
                    globalRefs)
            end (* copyLambda *)

                (* Copy the closure of a function which has previously been
                processed by copyLambda. *)
            and copyProcClosure (P2Lambda{ body, name, argTypes, level,
                                           resultType, localCount, ...}, newClosure, makeClosure) =
                let
                    (* process the non-locals in this function *)
                    (* If a closure is needed then any functions referred to
                       from the closure also need closures.*)
                    fun makeLoads ext = locaddr(ext, makeClosure)
               
                    val copyRefs = rev (map makeLoads newClosure)
                in
                    P2Lambda
                      {
                        body          = body,
                        name          = name,
                        closure       = copyRefs,
                        argTypes      = argTypes,
                        resultType    = resultType,
                        level         = level,
                        localCount    = localCount,
                        makeClosure   = makeClosure
                      }
                end
            |  copyProcClosure(pt, _, _) = pt (* may now be a constant *)
            (* end copyProcClosure *)
        in     
            insert pt
        end (* copyCode *)

        val outputAddresses = ref 1
        val insertedCode = 
            copyCode (pt, fn _ => raise InternalError "outer level reached in copyCode",
                      localAddressCount, outputAddresses, 0)
    in
        (insertedCode, !outputAddresses)
    end (* staticLinkAndCases *)

    fun codeGenerate(code, localCount, debugArgs) =
        codeGenAndPrint debugArgs (staticLinkAndCases(code, localCount, debugArgs))

    structure Sharing = struct type codetree = codetree end
end;
