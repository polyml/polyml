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

functor CODETREE_STATIC_LINK_AND_CASES(
    structure BASECODETREE: BaseCodeTreeSig
    structure CODETREE_FUNCTIONS: CodetreeFunctionsSig

    sharing BASECODETREE.Sharing = CODETREE_FUNCTIONS.Sharing

) : 
sig
    type codetree
    val staticLinkAndCases: (codetree * int -> unit -> Address.machineWord) * codetree * int -> codetree
    structure Sharing : sig type codetree = codetree end
end
=
struct
    open BASECODETREE
    open CODETREE_FUNCTIONS
    
    open Address
    
    exception InternalError = Misc.InternalError

    val ioOp : int -> machineWord = RunCall.run_call1 RuntimeCalls.POLY_SYS_io_operation
    val rtsFunction = mkConst o ioOp

    fun staticLinkAndCases (codegen, pt, localAddressCount) =
    let
        fun copyCode (pt, previous, localCount, localAddresses) =
        let
            (* "closuresForLocals" is a flag indicating that if the declaration
               is a procedure a closure must be made for it. *)
            val closuresForLocals = Array.array(localCount, false);

            (* used to indicate whether a local declaration is really
             a constant, so that we can in-line it. *)
            val localConsts       = Array.array (localCount, NONE)
            
            val newLocalAddresses = Array.array (localCount, 0)

            (* returns the translated node *)
            fun locaddr (ptr as { addr=laddr, level=lev, ...}, closure) =
            (
                if lev <> 0 orelse laddr = 0
                then (* non-local *) previous (ptr, lev, closure)
           
                else if laddr < 0
                then mkGenLoad (laddr, 0, true, false)
          
                (* isOnstack *)
                else case Array.sub(localConsts, laddr) of
                    SOME c => c (* just return the constant *)
                |   NONE =>
                    let
                        val () =
                            if closure then Array.update (closuresForLocals, laddr, true) else ()
                        val newAddr = Array.sub(newLocalAddresses, laddr)
                        val _ = newAddr <> 0 orelse raise InternalError "copyCode: Not set"
                    in
                        mkGenLoad (newAddr, 0, true, false)
                    end
            )

            fun makeDecl addr =
            (let
                val newAddr = ! localAddresses before (localAddresses := !localAddresses+1)
                val () = Array.update (closuresForLocals, addr, false)
                val () = Array.update (newLocalAddresses, addr, newAddr)
            in
                newAddr
            end handle Subscript => raise InternalError("makeDecl " ^ Int.toString addr ^ " of " ^ Int.toString localCount))
            
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
        
            |   insert(Eval { function, argList, resultType, ...}) =
                let
                    (* Process the arguments first. *)
                    val newargs = mapright(fn (c, t) => (insert c, t)) argList
                    val func  =
                    case function of
                        Extract ext => locaddr(ext, (* closure = *) false)
                    |   first => insert first
                in
                    (* If we are calling a procedure which has been declared this
                       does not require it to have a closure. Any other use of the
                       procedure would. *) 
                    Eval {function = func, argList = newargs, resultType=resultType}
                end

            |   insert(Extract ext) =
                    (* Load the value bound to an identifier. The closure flag is
                       set to true since the only cases where a closure is not needed,
                       eval and load-andStore, are handled separately. *)
                    locaddr(ext, (* closure = *) true)

            |   insert(Indirect {base, offset}) = Indirect {base = insert base, offset = offset}

            |   insert(pt as Constnt _) = 
                    pt (* Constants can be returned untouched. *)

            |   insert(BeginLoop{loop=body, arguments=argList, ...}) = (* Start of tail-recursive inline function. *)
                let
                    (* Make entries in the tables for the arguments. *)
                    val newAddrs = List.map (fn ({addr, ...}, _) => makeDecl addr) argList

                    (* Process the body. *)
                    val insBody = insert body
                    (* Finally the initial argument values. *)
                    local
                        fun copyDec(({value, ...}, t), addr) =
                                ({value = insert value, addr = addr, references = 0}, t)
                    in
                        val newargs = ListPair.map copyDec (argList, newAddrs)
                    end
                in
                    (* Add the kill entries on after the loop. *)
                    BeginLoop{loop=insBody, arguments=newargs}
                end
    
            |   insert(Loop argList) = (* Jump back to start of tail-recursive function. *)
                        Loop(List.map(fn (c, t) => (insert c, t)) argList)

            |   insert(Raise x) = Raise (insert x)

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
                                    Constnt _ => Array.update (localConsts, caddr, SOME pt)
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
                                            val copiedLambda = copyLambda lam closure;
                                        in
                                            (* Note: copyLambda may have set closure *)
                                            copyProcClosure copiedLambda (! (closure))
                                        end
                                    |   _ => insert pt
                            in
                                mkDec(newAddr, dec) :: rest
                            end
                        end (* copyDeclarations.isDeclar *)

                    |   copyDeclarations (RecDecs mutualDecs :: vs)  =
                        let
                          (* Mutually recursive declarations. Any of the declarations
                             may refer to any of the others. This causes several problems
                             in working out the use-counts and whether the procedures 
                             (they should be procedures) need closures. A procedure will
                             need a closure if any reference would require one (i.e. does
                             anything other than call it). The reference may be from one
                             of the other mutually recursive declarations and may be 
                             because that procedure requires a full closure. This means
                             that once we have dealt with any references in the rest of
                             the containing block we have to repeatedly scan the list of
                             declarations removing those which need closures until we
                             are left with those that do not. The use-counts can only be
                             obtained when all the non-local lists have been copied. *)
                  
                           (* First go down the list making a declaration for each entry.
                              This makes sure there is a table entry for all the
                              declarations. *)

                            val _ = List.map (fn {addr, ...} => makeDecl addr) mutualDecs

(*                            fun applyFn ({addr=caddr, value=dv, ...}) =     
                                (
                                    case dv of
                                        Constnt _ => Array.update (localConsts, caddr, SOME dv) 
                                    |   _ => ()
                                )

                            val () = List.app applyFn mutualDecs;*)
                  
                            (* Process the rest of the block. Identifies all other
                               references to these declarations. *)
                            val restOfBlock = copyDeclarations vs

                            (* We now want to find out which of the declarations require
                               closures. First we copy all the declarations, except that
                               we don't copy the non-local lists of procedures. *)
                            fun copyDec ({addr=caddr, lambda, ...}) = 
                                let
                                    val closure = ref (Array.sub(closuresForLocals, caddr))
                                    val dec     = copyLambda lambda closure
                                    (* Check whether we now have a constant *)
                                    val () =
                                        case dec of
                                            Constnt _ => Array.update (localConsts, caddr, SOME dec)
                                        |   _ => Array.update (localConsts, caddr, NONE); (* needed? *)

                                    (* copyLambda may set "closure" to true. *)
                                    val () = Array.update (closuresForLocals, caddr, !closure);
                                in
                                    {addr=caddr, value = dec, references = 0}
                                end             

                            val copiedDecs: simpleBinding list = map copyDec mutualDecs
                   
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
                                    fun mkLightClosure ({value, addr, ...}) = 
                                        let
                                            val clos = copyProcClosure value false
                                            val newAddr = Array.sub(newLocalAddresses, addr)
                                        in
                                            {value=clos, addr=newAddr, references=0}
                                        end          
                                in
                                    map mkLightClosure outlist
                                end
                  
                            |   processClosures((h as {addr=caddr, value, ...})::t, outlist, someFound) =
                                if Array.sub(closuresForLocals, caddr)
                                then
                                let (* Must copy it. *)
                                    val clos = copyProcClosure value true
                                    val newAddr = Array.sub(newLocalAddresses, caddr)
                                in
                                    {value=clos, addr=newAddr, references=0} :: processClosures(t, outlist, true)
                                end
                                    (* Leave it for the moment. *)
                                else processClosures(t, h :: outlist, someFound)
                  
                            val decs = processClosures(copiedDecs, [], false)

                            local
                                fun isLambda{value=Lambda _, ...} = true
                                |   isLambda _ = false
                            in
                                val (lambdas, nonLambdas) = List.partition isLambda decs
                            end
                            fun asMutual{addr, value=Lambda lambda, references} = {addr=addr, lambda=lambda, references=references}
                            |   asMutual _ = raise InternalError "asMutual"
                        in
                            (* Return the mutual declarations and the rest of the block. *)
                            if null lambdas
                            then map Declar nonLambdas @ restOfBlock         (* None left *)
                            else RecDecs (map asMutual lambdas) :: (map Declar nonLambdas @ restOfBlock)
                        end (* copyDeclarations.isMutualDecs *)
          
                    |   copyDeclarations (NullBinding v :: vs)  =
                        let (* Not a declaration - process this and the rest. *)
                           (* Must process later expressions before earlier
                               ones so that the last references to variables
                               are found correctly. DCJM 30/11/99. *)
                            val copiedRest = copyDeclarations vs;
                            val copiedNode = insert v;
                        in
                            (* Expand out blocks *)
                            case copiedNode of
                                Newenv(decs, exp) => decs @ (NullBinding exp :: copiedRest)
                            |   _ => NullBinding copiedNode :: copiedRest
                        end (* copyDeclarations *)

                    val insElist = copyDeclarations(ptElist @ [NullBinding ptExp])
                in
                    (* TODO: Tidy this up. *)
                    decSequenceWithFinalExp insElist
                end (* isNewEnv *)
                
            |   insert(Recconstr recs) = (* perhaps it's a constant now? *)
                    mkTuple (mapright insert recs)

            |   insert(pt as Ldexc) = pt (* just a constant so return it *)
      
            |   insert(Lambda lam)=
                    (* Must make a closure for this procedure because
                        it is not a simple declaration. *)
                    copyProcClosure (copyLambda lam (ref true)) true

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

            |   insert(IndirectVariable{base, offset, ...}) =
                (* Convert this into a Load instruction. *)
                    insert(mkEval(rtsFunction RuntimeCalls.POLY_SYS_load_word, [base, offset]))

            |   insert(TupleVariable(vars, length)) =
                (* Convert this into a set of RTS calls.  This currently uses POLY_SYS_alloc_store
                   which initialises the store before the copy operations.  It may be possible to
                   avoid this duplication somehow. *)
                let
                    val newAddr = ! localAddresses before (localAddresses := !localAddresses+1)
                    val mutableFlags = Word8.orb(F_words, F_mutable)
                    val allocTuple =
                        mkDec(newAddr,
                            mkEval(rtsFunction RuntimeCalls.POLY_SYS_alloc_store,
                                [insert length, mkConst (toMachineWord mutableFlags), CodeZero])
                        )
                    fun copyTuple(VarTupleSingle{source, destOffset}) =
                            mkEval(rtsFunction RuntimeCalls.POLY_SYS_assign_word,
                                [mkLoad(newAddr, 0), insert destOffset, insert source])
                    |   copyTuple(VarTupleMultiple{base, length, destOffset, sourceOffset}) =
                            mkEval(rtsFunction RuntimeCalls.POLY_SYS_move_words,
                                [insert base, insert sourceOffset, mkLoad(newAddr, 0),
                                 insert destOffset, insert length])
                    (* Remove the mutable bit (needed by alloc_store). *)
                    val lock = mkEval(rtsFunction RuntimeCalls.POLY_SYS_lockseg, [mkLoad(newAddr, 0)])
                 in
                    mkEnv(allocTuple :: (map NullBinding (mapright copyTuple vars @ [lock])), mkLoad(newAddr, 0))
                end

            |   insert(Case _) = raise InternalError "insert:Case"
            |   insert(KillItems _) = raise InternalError "insert:KillItems"

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
                 Now returns the variable, choosing the one which has lastRef set
                 if possible.  Note: the reason we consider Indirect entries here
                 as well as Extract is because we (used to) defer Indirect entries.  *)
              datatype similarity =
                Different | Similar of { addr : int, level: int, fpRel: bool, lastRef: bool }

              fun similar (Extract (a as {addr=aAddr, level=aLevel, fpRel=aFpRel, lastRef=aRef}),
                           Extract (b as {addr=bAddr, level=bLevel, fpRel=bFpRel, lastRef=_})) =
                    if aAddr = bAddr andalso aLevel = bLevel andalso aFpRel = bFpRel
                    then if aRef then Similar a else Similar b
                    else Different
              
               |  similar (Indirect{offset=aOff, base=aBase}, Indirect{offset=bOff, base=bBase}) =
                    if aOff <> bOff then Different else similar (aBase, bBase)
              
               |  similar _ = Different;

                (* If we have a call to the int equality operation *)
                (* then we may be able to use a case statement. *)
                fun findCase (Eval{ function=Constnt cv, argList, ... }) : caseVal =
                let
                    val isArbitrary = wordEq (cv, ioOp RuntimeCalls.POLY_SYS_equala)
                    val isWord = wordEq (cv, ioOp RuntimeCalls.POLY_SYS_word_eq)
                in
                    if isArbitrary orelse isWord
                    then  (* Should be just two arguments. *)
                    case argList of
                        [(Constnt c1, _), (arg2, _)] =>
                        if isShort c1
                        then SOME{tag=toShort c1, test=arg2, caseType = if isArbitrary then CaseInt else CaseWord}
                        else NONE (* Not a short constant. *)
                    
                     | [(arg1, _), (Constnt c2, _)] =>
                        if isShort c2
                        then SOME{tag=toShort c2, test=arg1, caseType = if isArbitrary then CaseInt else CaseWord}
                        else NONE (* Not a short constant. *)
                    
                    | _ => NONE
                       (* Wrong number of arguments - should raise exception? *)
                
                    else NONE (* Function is not a comparison. *)
                end

             |  findCase(TagTest { test, tag, maxTag }) =
                    SOME { tag=tag, test=test, caseType=CaseTag maxTag }

             |  findCase _ = NONE
        
              val testCase  : caseVal  = findCase insFirst
            in

              case testCase of
                    NONE => (* Can't use a case *)
                        mkIf (insFirst, insThen, reconvertCase insElse)
                |   SOME { tag=caseTags, test=caseTest, caseType=caseCaseTest } =>
                        (* Can use a case. Can we combine two cases?
                          If we have an expression like 
                               "if x = a then .. else if x = b then ..."
                          we can combine them into a single "case". *)
                        case insElse of
                            Case { cases=nextCases, test=nextTest, default=nextDefault, caseType=nextCaseType } =>
                            (
                                case (similar(nextTest, caseTest), caseCaseTest = nextCaseType) of
                                  (* Note - it is legal (though completely redundant) for the
                                     same case to appear more than once in the list. This is not
                                      checked for at this stage. *)
                                    (Similar _, true) =>
                                        Case 
                                        {
                                            cases   = (insThen, caseTags) ::
                                                        map (fn (c, l) => (c, l)) nextCases,
                                            test    = nextTest,
                                            default = nextDefault,
                                            caseType = caseCaseTest
                                        }

                                    | _ => (* Two case expressions but they test different
                                              variables. We can't combine them. *)
                                        Case
                                        {
                                            cases   = [(insThen, caseTags)],
                                            test    = caseTest,
                                            default = reconvertCase insElse,
                                            caseType=caseCaseTest
                                        }
                            )
                            | _ => (* insElse is not a case *)
                                Case
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
            and reconvertCase(Case{cases, test, default, caseType}) =
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
                    then Case{cases=cases, test=test, default=default, caseType=caseType}
                    else
                    let
                        fun reconvert [] = default
                        |   reconvert ((c, t) :: rest) =
                            let
                                val test =
                                    case caseType of
                                        CaseInt =>
                                            mkEval(Constnt(ioOp RuntimeCalls.POLY_SYS_equala),
                                                   [test, Constnt(toMachineWord t)])
                                    |   CaseWord =>
                                            mkEval(Constnt(ioOp RuntimeCalls.POLY_SYS_word_eq),
                                                   [test, Constnt(toMachineWord t)])
                                    |   CaseTag maxTag => TagTest { test=test, tag=t, maxTag=maxTag }
                            in
                                Cond(test, c, reconvert rest)
                            end
                    in
                        reconvert cases
                    end
                end
            |   reconvertCase t = t (* Just a simple conditional. *)
            

            (* If "makeClosure" is true the procedure will need a full closure. *)
            (* It may need a full closure even if makeClosure is false if it    *)
            (* involves a recursive reference which will need a closure.        *)
            and copyLambda ({body=lambdaBody, level=nesting, argTypes, isInline,
                             name=lambdaName, resultType, localCount, ...}) makeClosure =
            let
              val newGrefs      = ref [] (* non-local references *)
              val newNorefs     = ref 0  (* number of non-local refs *)
       
              (* A new table for the new procedure. *)
              fun prev (ptr as { addr, fpRel, ...}, lev, closure: bool) : codetree =
              let 
                    (* Returns the closure address of the non-local *)
                    fun makeClosureEntry([], _) = (* not found - construct new entry *)
                        let
                            val () = newGrefs := mkGenLoad (addr, lev - 1, fpRel, false) ::  !newGrefs;
                            val newAddr = !newNorefs + 1;
                        in
                            newNorefs := newAddr; (* increment count *)
                            mkClosLoad(newAddr, false)
                        end
        
                    |   makeClosureEntry(Extract{addr=loadAddr, level=loadLevel, fpRel=loadFpRel, ...} :: t,
                                         newAddr) =
                        if loadAddr = addr andalso loadLevel = lev - 1 andalso loadFpRel = fpRel
                        then mkClosLoad(newAddr, false)
                        else makeClosureEntry(t, newAddr - 1)

                    | makeClosureEntry _ =
                        raise InternalError "makeClosureEntry: closure is not Extract";
              in
                (* If we use a procedure on another level in a way that will
                   require it to have a real closure we must make one for it.
                   (i.e. we must set the "closure" flag.) This is necessary
                   because we may, for example, pass an outer procedure as a
                   parameter from within an inner procedure. The inner procedure
                   may not itself need a closure so the non-local references 
                   it makes will not be forced to have closures, but the outer
                   procedure will need one. *)
                if lev = 0 (* Reference to the closure itself. *)
                then
                let
                    val () =
                        if addr <> 0 orelse fpRel
                        then raise InternalError "prev: badly-formed load"
                        else ();
            
                    val () = if closure then makeClosure := true else ();
                in
                    mkClosLoad(0, false)
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
                        Constnt _ => outerLoad
                        |   _ => makeClosureEntry (!newGrefs, !newNorefs)
               end
              end (* prev *);
      
              (* process the body *)
              val newLocalAddresses = ref 1
              val insertedCode = copyCode (lambdaBody, prev, localCount, newLocalAddresses)
            in
              if null (!newGrefs) (* no external references *)
              then
              let
                val copiedProc =
                  Lambda
                    {
                      body          = insertedCode,
                      isInline      = isInline,
                      name          = lambdaName,
                      closure       = [],
                      argTypes      = argTypes,
                      resultType    = resultType,
                      level         = nesting,
                      closureRefs   = 0,
                      localCount    = ! newLocalAddresses,
                      makeClosure   = true,
                      argLifetimes  = []
                    }
              in
               (* Code generate it now so we get a constant. *)
                evaluate(copiedProc, codegen, 1)
              end
      
              else
                (* External references present. The closure will be copied
                   later with copyProcClosure. *)
                Lambda 
                  {
                    body          = insertedCode,
                    isInline      = isInline,
                    name          = lambdaName,
                    closure       = !newGrefs,
                    argTypes      = argTypes,
                    resultType    = resultType,
                    level         = nesting,
                    closureRefs   = 0,
                    localCount    = ! newLocalAddresses,
                    makeClosure   = false,
                    argLifetimes  = []
                  }
            end (* copyLambda *)

                (* Copy the closure of a procedure which has previously been
                processed by copyLambda. *)
            and copyProcClosure (Lambda{ body, isInline, name, argTypes, level, closureRefs,
                                         closure, resultType, localCount, argLifetimes, ...}) makeClosure =
                let
                    (* process the non-locals in this procedure *)
                    (* If a closure is needed then any procedures referred to
                       from the closure also need closures.*)
                    fun makeLoads (Extract ext) = locaddr(ext, makeClosure)
                     |  makeLoads _ = raise InternalError "makeLoads - not an Extract"
               
                    val copyRefs = rev (map makeLoads closure);
                in
                    Lambda
                      {
                        body          = body,
                        isInline      = isInline,
                        name          = name,
                        closure       = copyRefs,
                        argTypes      = argTypes,
                        resultType    = resultType,
                        level         = level,
                        closureRefs   = closureRefs,
                        localCount    = localCount,
                        makeClosure   = makeClosure,
                        argLifetimes  = argLifetimes
                      }
                end
            |  copyProcClosure pt _ = pt (* may now be a constant *)
            (* end copyProcClosure *)
        in     
            insert pt
        end (* copyCode *)
         
        val insertedCode = 
            copyCode (pt, fn _ => raise InternalError "outer level reached in copyCode",
                      localAddressCount, ref 1)
    in
        insertedCode
    end (* staticLinkAndCases *)

    structure Sharing = struct type codetree = codetree end
end;
