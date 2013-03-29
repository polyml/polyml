(*
    Copyright (c) 2013 David C.J. Matthews

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
    Derived from the original parse-tree

    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Further development:
    Copyright (c) 2000-13 David C.J. Matthews

    Title:      Parse Tree Structure and Operations.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985

*)

functor MATCH_COMPILER (
    structure BASEPARSETREE : BaseParseTreeSig
    structure PRINTTREE: PrintParsetreeSig
    structure LEX : LEXSIG
    structure CODETREE : CODETREESIG
    structure DEBUGGER : DEBUGGERSIG
    structure TYPETREE : TYPETREESIG
    structure TYPEIDCODE: TYPEIDCODESIG
    structure STRUCTVALS : STRUCTVALSIG
    structure VALUEOPS : VALUEOPSSIG
    structure DATATYPEREP: DATATYPEREPSIG

    structure DEBUG :
    sig
        val debugTag: bool Universal.tag
        val errorDepthTag : int Universal.tag
        val fileNameTag: string Universal.tag
        val reportUnreferencedIdsTag: bool Universal.tag
        val reportExhaustiveHandlersTag: bool Universal.tag
        val getParameter :
               'a Universal.tag -> Universal.universal list -> 'a 
    end

    structure MISC :
    sig
        (* These are handled in the compiler *)
        exception Conversion of string     (* string to int conversion failure *)
  
        (* This isn't handled at all (except generically) *)
        exception InternalError of string (* compiler error *)
    end

    structure ADDRESS :
    sig
        type machineWord;    (* any legal bit-pattern (tag = 0 or 1) *)
        val toMachineWord: 'a -> machineWord
        type short = Word.word
        val isShort: 'a -> bool
        val toShort: 'a -> short
    end

    sharing BASEPARSETREE.Sharing
    =       PRINTTREE.Sharing
    =       LEX.Sharing
    =       CODETREE.Sharing
    =       DEBUGGER.Sharing
    =       TYPETREE.Sharing
    =       TYPEIDCODE.Sharing
    =       STRUCTVALS.Sharing
    =       VALUEOPS.Sharing
    =       DATATYPEREP.Sharing
    =       ADDRESS
): MatchCompilerSig =
struct
    open BASEPARSETREE
    open PRINTTREE
    open CODETREE
    open TYPEIDCODE
    open LEX
    open TYPETREE
    open DEBUG
    open STRUCTVALS
    open VALUEOPS
    open ADDRESS
    open MISC
    open DATATYPEREP
    open TypeVarMap

    open RuntimeCalls; (* for POLY_SYS numbers *)

    datatype environEntry = datatype DEBUGGER.environEntry

    (* To simplify passing the context it is wrapped up in this type. *)
    type cgContext =
        {
            decName: string, debugEnv: environEntry list * (level->codetree), mkAddr: int->int,
            level: level, typeVarMap: typeVarMap, lex: lexan, lastDebugLine: int ref,
            isOuterLevel: bool (* Used only to decide if we need to report non-exhaustive matches. *)
        }

    fun repDebugEnv debugEnv ({decName, mkAddr, level, typeVarMap, lex, lastDebugLine, isOuterLevel, ...}: cgContext) =
        { debugEnv=debugEnv, mkAddr=mkAddr, level=level, typeVarMap=typeVarMap,
          decName=decName, lex=lex, lastDebugLine=lastDebugLine, isOuterLevel = isOuterLevel}: cgContext

    (* Try this pipeline function *)
    infix |>
    fun a |> f = f a

    type debugenv = environEntry list * (level->codetree)
  
    (* Debugging control and debug function. *)

    fun createDebugEntry (v: values, loadVal, {mkAddr, level, debugEnv=(ctEnv, rtEnv: level -> codetree), lex, ...}: cgContext) =
        if not (getParameter debugTag (debugParams lex))
        then { dec = [], rtEnv = rtEnv, ctEnv = ctEnv }
        else let
                val newEnv =
                (* Create a new entry in the environment. *)
                      mkTuple [ loadVal (* Value. *), rtEnv level ]
                val { dec, load } = multipleUses (newEnv, fn () => mkAddr 1, level)
                val ctEntry =
                    case v of
                        Value{class=Exception, name, typeOf, locations, ...} =>
                            EnvException(name, typeOf, locations)
                    |   Value{class=Constructor{nullary, ofConstrs, ...}, name, typeOf, locations, ...} =>
                            EnvVConstr(name, typeOf, nullary, ofConstrs, locations)
                    |   Value{name, typeOf, locations, ...} =>
                            EnvValue(name, typeOf, locations)
            in
                { dec = dec, rtEnv = load, ctEnv = ctEntry :: ctEnv}
            end

    (* Devised by Mike Fourman, Nick Rothwell and me (DCJM).  First coded
       up by Nick Rothwell for the Kit Compiler. First phase of the match
       compiler. The purpose of this phase is to take a match (a set of
       patterns) and bring together the elements that will be discriminated
       by testing any particular part of the value.  Where a pattern is a
       tuple, for example, it is possible to discriminate on each of the
       fields independently, but it may be more efficient to discriminate
       on one of the fields first, and then on the others. The aim is to
       produce a set of tests that discriminate between the patterns 
       quickly. *)
   
    abstype patSet = PatSet of int list

    with           
      (* Each leaf in the tree contains a number which identifies the
         pattern it came from. As well as linking back to the patterns,
         these numbers represent an ordering, because earlier patterns
         mask out later ones. *)
      (* A set of pattern identifiers. *)
      val empty       = PatSet [];
      fun singleton i = PatSet [i];
  
      fun list (PatSet p) = p;
  
      infix 3 :::;
  
      fun a ::: b = PatSet (a :: list b);

      fun isEmptySet (PatSet p) = null p;
      fun first   (PatSet p) = hd p; 
      fun next    (PatSet p) = PatSet (tl p); 
  
      (* Set from i to j inclusive. *)
      fun from i j = if i > j then empty else i ::: from (i + 1) j;

      infix 3 plus;
      infix 4 inside;
      infix 5 intersect;
      infix 6 diff;
      infix 7 eq;
      infix 8 eqSc
      infix 9 neq;
  
          (* Union of sets. *)
      fun a plus b =
       if isEmptySet a then b
       else if isEmptySet b then a
       else if first a = first b then first a ::: (next a plus next b)
       else if first a < first b then first a ::: (next a plus b)
       else first b ::: (a plus next b);

          (* Set membership. *)
      fun i inside a =
        if isEmptySet a then false
        else if i = first a then true
        else if i < first a then false
        else i inside next a; 
  
      (* Intersection of sets. *) 
      fun a intersect b =
        if isEmptySet a orelse isEmptySet b
          then empty
        else if first a = first b 
          then first a ::: ((next a) intersect (next b))
        else if first a < first b 
          then (next a) intersect b
        else a intersect next b;

      (* Set difference. *)
      fun a diff b =
        if isEmptySet a 
          then empty
        else if isEmptySet b
          then a
        else if first a = first b
          then (next a) diff (next b) 
        else if first a < first b
          then first a ::: ((next a) diff b)
        else a diff next b;

          (* Set equality. *)
      fun a eq b =
        if isEmptySet a
           then isEmptySet b
        else if isEmptySet b
          then false
        else first a = first b andalso next a eq next b;

    end (* patSet *);

    datatype aot = 
        Aot of 
        { 
            patts:    aots,       (* Choices made at this point. *)
            defaults: patSet,     (* Patterns that do not discriminate on this node. *)
            vars:     values list (* The variables bound at this point. *)
        }
                        
    and aots = 
        TupleField of aot list       (* Each element of the list is a field of the tuple. *)
    |   Cons       of consrec list * int   (* List of constructors and the number of different constructors. *)
    |   Excons     of consrec list   (* Exception constructors. *)
    |   Scons      of sconsrec list  (* Int, char, string, real. *)
    |   Wild                         (* Patterns that do not discriminate at all. *) 

    (* Datatype constructors and exception constructors. *)
    withtype consrec =
        {
            constructor: values, (* The constructor itself. *)
            patts: patSet,       (* Patterns that use this constructor *)
            appliedTo: aot,      (* Patterns this constructor was applied to. *)
            polyVars: types list (* If this was polymorphic, the matched types. *)
        } 

    and sconsrec =
        {
            eqFun:   codetree,    (* Equality functions for this type*)
            specVal: machineWord option,    (* The constant value. NONE here means we had a conversion error. *)
            patts:   patSet       (* Patterns containing this value. *)
        }

    fun makeAot(patts, defaults, vars) =
        Aot 
        { 
            patts    = patts,
            defaults = defaults,
            vars     = vars
        }
                                            
    fun makeConsrec(constructor, patts, appliedTo, polyVars): consrec = 
        {
            constructor = constructor,
            patts       = patts, 
            appliedTo   = appliedTo,
            polyVars    = polyVars
        }
                                                      
    fun makeSconsrec(eqFun, specVal, patts) : sconsrec =
        {
            eqFun    = eqFun,
            specVal  = specVal,
            patts    = patts
        }

    (* An empty wild card - can be expanded as required. *)
    val aotEmpty = makeAot(Wild, empty, [])

    (* A new wild card entry with the same defaults as a previous entry. *)
    fun wild (Aot {defaults, ...}) = makeAot(Wild, defaults, [])

    local
        (* Add a default (wild card or variable) to every node in the tree. *)
        fun addDefault (Aot {patts, defaults, vars}) patNo =
        let
            fun addDefaultToConsrec {constructor, patts, appliedTo, polyVars} =
                makeConsrec(constructor, patts, addDefault appliedTo patNo, polyVars)
    
            val newPatts =
                case patts of
                    TupleField pl => 
                        TupleField (map (fn a => addDefault a patNo) pl)
            
                |   Cons(cl, width) => Cons (map addDefaultToConsrec cl, width)
                     
                |   Excons cl => Excons (map addDefaultToConsrec cl)
          
                |   otherPattern => (* Wild, Scons *) otherPattern
        in
            makeAot(newPatts, defaults plus singleton patNo, vars)
        end (* addDefault *)

        fun addVar (Aot {patts, defaults, vars}) var = makeAot(patts, defaults, var :: vars)

        (* Add a constructor to the tree.  It can only be added to a
           cons node or a wild card. *)
        fun addConstr(cons, noOfConstrs, doArg, tree as Aot {patts = Wild, defaults, vars, ...}, patNo, polyVars) =
            let (* Expand out the wildCard into a constructor node. *)          
                val cr = 
                    makeConsrec(cons, singleton patNo, (* Expand the argument *) doArg (wild tree), polyVars);
            in
                makeAot(Cons([cr], noOfConstrs), defaults, vars)
            end

        |   addConstr(cons, _, doArg, tree as Aot {patts = Cons(pl, width), defaults, vars}, patNo, polyVars) =
            let
                (* Merge this constructor with other occurences. *)
                fun addClist [] = (* Not there - add this on the end. *)
                    [makeConsrec(cons, singleton patNo, doArg (wild tree), polyVars)]
          
                |   addClist (ccl::ccls) =
                    if valName (#constructor ccl) = valName cons
                    then (* Merge in. *)
                        makeConsrec(cons, singleton patNo plus #patts ccl, doArg (#appliedTo ccl), polyVars)
                            :: ccls
                    else (* Carry on looking. *) ccl :: addClist ccls;
            in
                makeAot (Cons (addClist pl, width), defaults, vars)
            end

        |   addConstr _ = raise InternalError "addConstr: badly-formed and-or tree"

            (* Add a special constructor to the tree.  Very similar to preceding. *)
        fun addSconstr(eqFun, cval, Aot {patts = Wild, defaults, vars, ...}, patNo, _) =
             (* Expand out the wildCard into a constructor node. *)
            makeAot (Scons [makeSconsrec(eqFun, cval, singleton patNo)], defaults, vars)
            
        |   addSconstr(eqFun, cval, Aot {patts = Scons pl, defaults, vars, ...}, patNo, lex) =
            let (* Must be scons *)
                (* Merge this constructor with other occurrences. *)
                (* Special constants may be overloaded so we don't have a fixed set of types
                   here.  We need to use the type-specific equality function to test.
                   Since only the basis library overloads constants we can assume that
                   eqFun is a constant. *)
                fun equalSpecials(SOME a, SOME b) =
                    let
                        val eqCode = mkEval(eqFun, [mkTuple[mkConst a, mkConst b]])
                    in
                        RunCall.unsafeCast(valOf(evalue(genCode(eqCode, debugParams lex, 0)())))
                    end
                |   equalSpecials _ = false

                fun addClist [] = (* Not there - add this on the end. *)
                        [makeSconsrec(eqFun, cval, singleton patNo)]
                |   addClist (ccl :: ccls) =
                        if equalSpecials(cval, #specVal ccl)
                        then (* Merge in. *)
                            makeSconsrec(eqFun, cval, singleton patNo plus #patts ccl) :: ccls
                        else (* Carry on looking. *) ccl :: addClist ccls;
            in
                makeAot (Scons (addClist pl), defaults, vars)
            end

        |   addSconstr _ = raise InternalError "addSconstr: badly-formed and-or tree"

        (* Add an exception constructor to the tree.  Similar to the above
           except that exception constructors must be kept in order. *)
        fun addExconstr(cons, doArg, tree as Aot {patts = Wild, defaults, vars, ...}, patNo) =
                (* Expand out the wildCard into a constructor node. *)
            let
                val cr =
                    makeConsrec (cons, singleton patNo, doArg(wild tree), [])
            in
                makeAot (Excons [cr], defaults, vars)
            end
    
    
        |   addExconstr(cons, doArg, tree as Aot {patts = Excons (cl as (h::t)), defaults, vars, ...}, patNo) =
            let
          (* The exception constructor list is maintained in reverse order.
             We have to be careful about merging exception constructors.
             Two exceptions may have different names but actually have the
             same exception value, or have the same (short) name but come
             from different structures.  We only add to the last entry in
             the list if we can tell that it is the same exception. We could
             be more sophisticated and allow merging with other entries if
             we could show that the entries we were skipping over were
             definitely different, but it's probably not worth it. *)
            val newList =
              if isTheSameException (#constructor h, cons)
              then 
                 makeConsrec(cons, (singleton patNo) plus (#patts h), doArg(#appliedTo h), []) :: t
              else
                 makeConsrec(cons, singleton patNo, doArg(wild tree), []) :: cl;
          in
            makeAot (Excons newList, defaults, vars)
          end
      
        |   addExconstr _ = raise InternalError "addExconstr: badly-formed and-or tree"
    in

        (* Take a pattern and merge it into an andOrTree. *)
        fun buildAot (Ident {value=ref ident, expType=ref expType, ... }, tree, patNo, line, context as { typeVarMap, ...} ) =
            let
                val polyVars =
                    List.map #value (getPolymorphism (ident, expType, typeVarMap))
                fun doArg a = buildAot(WildCard nullLocation, a, patNo, line, context)
            in
                case ident of
                    Value{class=Constructor {ofConstrs, ...}, ...} =>
                      (* Only nullary constructors. Constructors with arguments
                         will be dealt with by ``isApplic'. *)
                        addConstr(ident, ofConstrs, doArg, tree, patNo, polyVars)
                |    Value{class=Exception, ...} =>
                          addExconstr(ident, doArg, tree, patNo)
                |   _ => (* variable - matches everything. Defaults here and pushes a var. *)
                          addVar (addDefault tree patNo) ident
            end

        |   buildAot (TupleTree{fields, location, ...},
                  tree as Aot {patts = Wild, defaults = treeDefaults, vars = treeVars, ...},
                  patNo, _, context) =
                (* Adding tuple to existing wild-card *)
            let
                val tlist = map (fn el => buildAot(el, wild tree, patNo, location, context)) fields
            in
                makeAot (TupleField tlist, treeDefaults, treeVars)
            end

        |   buildAot (TupleTree{fields, ...},
                  Aot {patts = TupleField pl, defaults = treeDefaults, vars = treeVars, ...},
                  patNo, line, context) =
            let (* Adding tuple to existing tuple. *)
                (* Merge each field of the tuple in with the corresponding
                   field of the existing tree. *)
                val tlist =
                    ListPair.mapEq (fn(t, a) => buildAot(t, a, patNo, line, context)) (fields, pl)
            in
                makeAot (TupleField tlist, treeDefaults, treeVars)
            end


        |   buildAot (TupleTree _, _, _, _, _) =
                raise InternalError "pattern is not a tuple in a-o-t"

        |   buildAot (vars as Labelled {recList, expType=ref expType, location, ...},
                      tree, patNo, _, context as { lex, ...}) =
            let
                (* Treat as a tuple, but in the order of the record entries.
                   Missing entries are replaced by wild-cards. The order of
                   the patterns given may bear no relation to the order in
                   the record which will be matched.
                   e.g. case X of (a = 1, ...) => ___ | (b = 2, a = 3) => ___ *)

                (* Check that the type is frozen. *)
                (* This check is probably redundant since we now check at the
                   point when we generalise the type (except for top-level
                   expressions - those could be detected in
                   checkForFreeTypeVariables).  Retain it for the moment.
                   DCJM 15/8/2000. *)
                val () =
                    if recordNotFrozen expType
                    then errorNear (lex, true, vars, location,
                          "Can't find a fixed record type.")
                    else ()

                (* Get the maximum number of patterns. *)
                val wilds = List.tabulate(recordWidth expType, fn _ => WildCard nullLocation)

                (* Now REPLACE entries from the actual pattern, leaving
                   the defaulting ones behind. *)
                (* Take a pattern and add it into the list. *)
                fun mergen (_ :: t) 0 pat = pat :: t
                |   mergen (h :: t) n pat = h :: mergen t (n - 1) pat
                |   mergen []       _ _   = raise InternalError "mergen";

                fun enterLabel ({name, valOrPat, ...}, l) = 
                    (* Put this label in the appropriate place in the tree. *)
                    mergen l (entryNumber (name, expType)) valOrPat
      
                val tupleList = List.foldl enterLabel wilds recList
            in
                (* And process it as a tuple. *)
                buildAot(TupleTree{fields=tupleList, location=location, expType=ref expType}, tree, patNo, location, context)
            end

        |   buildAot (Applic{f = Ident{value = ref applVal, expType = ref expType, ...}, arg, location, ...},
                      tree, patNo, _, context as { typeVarMap, ...}) =
            let
                val polyVars = List.map #value (getPolymorphism (applVal, expType, typeVarMap))
                fun doArg atree = buildAot(arg, atree, patNo, location, context)
            in
                case applVal of
                     Value{class=Constructor{ofConstrs, ...}, ...} =>
                        addConstr(applVal, ofConstrs, doArg, tree, patNo, polyVars)

                |    Value{class=Exception, ...} => addExconstr(applVal, doArg, tree, patNo)

                |    _ => tree (* Only if error *)
            end

        |   buildAot (Applic _ , tree, _, _, _) = tree (* Only if error *)

        |   buildAot (Unit _, tree, patNo, _, _) =
                (* There is only one value so it matches everything. *)
                addDefault tree patNo
      
        |   buildAot (WildCard _, tree, patNo, _, _) = addDefault tree patNo (* matches everything *)
      
        |   buildAot (List{elements, location, expType=ref expType, ...},
                      tree, patNo, _, context) =
            let (* Generate suitable combinations of cons and nil.
                e.g [1,2,3] becomes ::(1, ::(2, ::(3, nil))). *)
                (* Get the base type. *)
                val elementType = mkTypeVar (generalisable, false, false, false)
                val listType = mkTypeConstruction ("list", tsConstr listConstr, [elementType], [DeclaredAt inBasis])
                val _ = unifyTypes(listType, expType)
                val polyVars = [elementType]

                fun processList [] tree = 
                    (* At the end put in a nil constructor. *)
                    addConstr(nilConstructor, 2,
                        fn a => buildAot (WildCard nullLocation, a, patNo, location, context), tree, patNo, polyVars)
                | processList (h :: t) tree = (* Cons node. *)
                    let
                        fun mkConsPat (Aot {patts = TupleField [hPat, tPat], defaults, vars, ...}) =  
                            let   (* The argument is a pair consisting of the
                                     list element and the rest of the list. *)
                                val tlist = [buildAot(h, hPat, patNo, location, context), processList t tPat];
                            in
                                makeAot (TupleField tlist, defaults, vars)
                            end
                       | mkConsPat (tree  as Aot {patts = Wild, defaults, vars, ...}) =  
                            let
                                val hPat  = wild tree;
                                val tPat  = wild tree;
                                val tlist = [buildAot(h, hPat, patNo, location, context), processList t tPat];
                            in
                                makeAot (TupleField tlist, defaults, vars)
                            end
                        | mkConsPat _ = 
                            raise InternalError "mkConsPat: badly-formed parse-tree"
                    in
                        addConstr(consConstructor, 2, mkConsPat, tree, patNo, polyVars)
                    end
                (* end processList *);
            in
                processList elements tree
            end

        |   buildAot (vars as Literal{converter, literal, expType=ref expType, location},
                      tree, patNo, _, {lex, level, ...}) =
            let
                (* At the same time we have to get the equality function
                   for this type to plug into the code.  Literals are overloaded
                   so this may require first resolving the overload to the
                   preferred type. *)
                val constr = typeConstrFromOverload(expType, true)
                val equality =
                    equalityForType(
                        mkTypeConstruction(tcName constr, constr, [], []), level,
                        defaultTypeVarMap(fn _ => raise InternalError "equalityForType", baseLevel) (* Should never be used. *))
                val litValue: machineWord option =
                    getLiteralValue(converter, literal, expType, fn s => errorNear(lex, true, vars, location, s))
            in
                addSconstr(equality, litValue, tree, patNo, lex)
             end
    
        |   buildAot (Constraint {value, location, ...}, tree, patNo, _, context) = (* process the pattern *)
                buildAot(value, tree, patNo, location, context)
      
        |   buildAot (Layered {var, pattern, location}, tree, patNo, _, context) =(* process the pattern *)
            let  
                (* A layered pattern may involve a constraint which
                   has to be removed. *)
                fun getVar (Ident {value, ...}) = !value
                |   getVar (Constraint {value, ...}) = getVar value
                |   getVar _ = undefinedValue (* error *)
            in
                addVar (buildAot(pattern, tree, patNo, location, context)) (getVar var)
            end

        |   buildAot (Parenthesised(p, location), tree, patNo, _, context) =
               buildAot(p, tree, patNo, location, context)

        |   buildAot (_, tree, _, _, _) = tree (* error cases *)
    end


    fun buildTree (patts: matchtree list, context) =
    let   (* Merge together all the patterns into a single tree. *)
        fun maket []     _ tree = tree
        |   maket ((MatchTree{vars, location, ...})::t) patNo tree =
                maket t (patNo + 1) (buildAot(vars, tree, patNo, location, context))
    in
        maket patts 1 aotEmpty 
    end

    (* The code and the pattern from which it came, 0 if the default,
       ~1 if more than one pattern. This is used to remove redundant
       tests that are sometimes put in where we have a wild card above
       a constructor. *)

    type patcode = {decs: codeBinding list, exp: codetree, pat: int};

    fun makePatcode code pat : patcode = { decs = [], exp = code, pat = pat }
  
    fun patCodeBlock {decs, exp, pat=_} = mkEnv(decs, exp)

    val matchFailCode  : patcode = makePatcode MatchFail 0

    fun bindPattVars(arg, vars, context as { mkAddr, level, debugEnv, ...}) =
    let
        val addressOfVar = mkAddr 1
        val dec = mkDec (addressOfVar, arg)
        and load = mkLoadLocal addressOfVar

        (* Set the addresses of the variables and create debug entries. *)
        fun setAddr (v as Value{access=Local{addr=lvAddr, level=lvLevel}, ...}, (oldDec, oldEnv) ) =
            let (* Set the address of the variable to this and create
                   debug environment entries if required. *)
                val {dec=nextDec, ctEnv, rtEnv} =
                    createDebugEntry(v, load, context |> repDebugEnv oldEnv)
            in
                lvAddr  := addressOfVar;
                lvLevel := level;
                (oldDec @ nextDec, (ctEnv, rtEnv))
            end

        | setAddr _ = raise InternalError "setAddr"

        val (envDec, newEnv) = List.foldl setAddr ([], debugEnv) vars
     in
        (load, dec :: envDec, newEnv)
     end

    local
        (* Find the "depth" of pattern i.e. the position of
           any defaults. If one of the fields is itself a
           tuple find the maximum depth of its fields, since
           if we decide to discriminate on this field we will
           come back and choose the deepest in that tuple. *)
        fun pattDepth (Aot {patts=TupleField pl, ...}, active) =
            List.foldl (fn (t, d) => Int.max(pattDepth(t, active), d)) 0 pl

        |   pattDepth (Aot {patts, defaults,...}, active) =
            let (* Wild cards, constructors etc. *)
                val activeDefaults = defaults intersect active
            in
                if not (isEmptySet activeDefaults)
                then first activeDefaults
                else
                    (* No default - the depth is the number of
                       patterns that will be discriminated. Apart
                       from Cons which could be a complete match,
                       all the other cases will only occur
                       if the match is not exhaustive. *)
                case patts of 
                    Cons (cl, _) => length cl + 1
                |   Excons cl => length cl + 1
                |   Scons  sl => length sl + 1
                |   _         => 0 (* Error? *)
            end
    in
        fun bestColumn(colsToDo, noOfCols, asTuples, active) =
        let
            fun findDeepest(column, bestcol, depth) =
            if column = noOfCols (* Finished. *)
            then bestcol
            else if column inside colsToDo
            then
            let
                val thisDepth = pattDepth (List.nth(asTuples, column), active)
            in
                if thisDepth > depth
                then findDeepest (column + 1, column, thisDepth)
                else findDeepest (column + 1, bestcol, depth)
            end
            else findDeepest (column + 1, bestcol, depth)
        in
            findDeepest(0, 0, 0)
        end
    end

    (* Examine the match to check for exhaustiveness and redundancy and
       also to look to see whether it is sparse. *)
    local
        fun testMatch(Aot {patts, defaults, ...}, active, nextMatch) =
        let
            (* Get the set of defaults which are active. *)
            val activeDefaults = defaults intersect active
        in
            (* If the active set is empty (match is not exhaustive) or
               everything will default we can skip further checks.  *)
            if isEmptySet active orelse active eq activeDefaults
            then nextMatch active
            else case patts of
                TupleField fields =>
                let
                    (* Process a field and do all the discrimination on it.  We pass this
                       function as the continuation to process the next field
                       using the set of patterns that have matched. *)
                    (* For exhaustiveness and redundancy checking it doesn't
                       matter what order we use but to get the correct count
                       of references to patterns we need to use the same
                       selection criterion as codePatt uses. *)
                    val noOfCols = length fields
                    
                    fun despatch colsToDo active =
                        if isEmptySet colsToDo orelse isEmptySet active
                        then nextMatch active
                        else
                        let
                            val bestcol = bestColumn(colsToDo, noOfCols, fields, active)
                            val doNextCol = despatch(colsToDo diff (singleton bestcol))
                        in
                            testMatch(List.nth(fields, bestcol), active, doNextCol)
                        end
                in
                    despatch (from 0 (noOfCols-1)) active
                end

            |   Cons(cl, width) =>
                let
                    (* Process the constructors.  We can exhaust the possibilities
                       either by considering each of them or by firing the default.
                       To get the use counts the same as codePatt this code is
                       a bit more complicated than it would otherwise be. *)
                    fun processConstrs(_, 0) = () (* Listed all of them *)

                    |   processConstrs([], _) =
                        (* Have come to the end without matching everything - 
                           fire the defaults. *) nextMatch activeDefaults

                    |   processConstrs({patts, appliedTo, ...} :: next, toDo) =
                        let
                            val newActive = patts intersect active
                        in
                            if newActive eq empty (* This test avoids firing the default more than once. *)
                            then processConstrs(next, toDo)
                            else
                            (
                                testMatch(appliedTo, activeDefaults plus newActive, nextMatch);
                                processConstrs(next, toDo-1)
                            )
                        end
                in
                    processConstrs(cl, width)
                end
        
            |   Excons cl =>
                let
                    fun processConstr{patts, appliedTo, ...} =
                        if isEmptySet (patts intersect active)
                        then ()
                        else testMatch(appliedTo, activeDefaults plus (patts intersect active), nextMatch)
                in
                    List.app processConstr cl;
                    (* We always have to consider the defaults. *)
                    nextMatch activeDefaults
                end
 
            |   Scons sl =>
                (
                    (* A value containing a particular constant value will match patterns
                       containing that constant and also any defaults. *)
                    List.app(fn {patts, ...} =>
                            if isEmptySet (patts intersect active)
                            then ()
                            else nextMatch(activeDefaults plus (patts intersect active))) sl;
                    (* Anything else will match the defaults. *)
                    nextMatch activeDefaults
                )

            |   Wild => nextMatch active
        end
    in
        fun evaluateMatch(tree, nPats) =
        let
            open IntArray
            val exhaustive = ref true
            val pattRefs = array(nPats, 0)
            
            (* Called when all the discrimination has been done.  If the
               set is empty then there must be a value for which no pattern
               matches and the match is not exhaustive.  Otherwise there are
               one or more patterns and the one that will fire is the
               first.  *)
            fun firePatt active =
                if isEmptySet active
                then exhaustive := false
                else
                let
                    val patIndex = first active - 1
                in
                    update(pattRefs, patIndex, sub(pattRefs, patIndex)+1)
                end
            val () = testMatch(tree, from 1 nPats, firePatt)
            (* Create a set of the patterns that were never referenced i.e.
               the value in the array is zero *)
            val redundant =
                foldri(fn (patt, 0, l) => singleton(patt+1) plus l | (_, _, l) => l) empty pattRefs
            (* If there are more than twice as many expression firings than
               patterns the pattern is considered to be sparse and we
               use the naive match. *)
            val useNaiveMatch = foldl(op +) 0 pattRefs > 2 * nPats
        in
            { exhaustive = !exhaustive, redundant = redundant, useNaive = useNaiveMatch }
        end
    end

      (* Code generate a set of patterns.  tree is the aot we are working
         on, arg is the code representing the argument to take apart.
         The set of patterns which are active are held in "active", and
         "othermatches" is a continuation of other patterns when we have
         done this one. "default" is the default code executed if no
         pattern matches and is needed only because of problems with
         exceptions. "isBind" is a flag indicating whether we are
         processing a variable binding. The set of patterns is needed
         primarily for tuples. If we have patterns like
           (A, A) => ..| (B,B) => ... | _ => ... 
         when we have tested that the first field is actually A we are
         only interested in patterns 1 and 3, so that testing for the
         second field being B is unnecessary in this case. Similarly
         when we test for the second field being B we can eliminate
         pattern 1. Actually this does not work properly for exceptions
         because of exception aliasing. e.g.
            X 1 => ... | Y _ => ... | _ => ...
         It is possible that X and Y might be the same exception, so that
         the fact that the constructor matches X does not imply that it
         cannot also match Y.  *)
    fun codePatt 
           (Aot {patts, defaults, vars, ...},
           arg : codetree,
           active : patSet,
           othermatches : (patSet * (unit->patcode) * cgContext) -> patcode,
           default : unit -> patcode,
           context: cgContext as {level, typeVarMap, ...}
           )
           : patcode =
    let
        (* Put the arg into a local declaration and set the address of any
           variables to it. We declare all the variables that can be
           declared at this point, even though they could be in different
           patterns. *)
        val (declLoad, declDecs, declEnv) = bindPattVars(arg, vars, context)
    
        (* Get the set of defaults which are active. *)
        val activeDefaults : patSet = defaults intersect active;
  
        (* Look at the kinds of pattern. - If there is nothing left
           (match is not exhaustive) or if all the active patterns will
           default, we can skip any checks. *)
        val pattCode = 
            if isEmptySet active orelse active eq activeDefaults
            then othermatches(active, default, context |> repDebugEnv declEnv)
            else
            case patts of
                TupleField [patt] =>
                    codePatt(patt, declLoad, (* optimise unary tuples - no indirection! *)
                        active, othermatches, default, context |> repDebugEnv declEnv)
      
            |   TupleField asTuples =>
                let
                    (* A simple-minded scheme would despatch the first column
                       and then do the others. The scheme used here tries to do
                       better by choosing the column that has any wild card
                       furthest down the column. *)
                    val noOfCols = length asTuples
      
                    fun despatch colsToDo (active, def, env) =
                        (* If we have done all the columns we can stop. (Or if
                           the active set is empty). *)
                        if isEmptySet colsToDo orelse isEmptySet active
                        then othermatches(active, def, env)
                        else
                        let
                            val bestcol = bestColumn(colsToDo, noOfCols, asTuples, active)
                        in
                            codePatt(List.nth(asTuples, bestcol), mkInd (bestcol, declLoad), active,
                               despatch (colsToDo diff (singleton bestcol)), def, env)
                        end
                in
                    despatch (from 0 (noOfCols-1)) (active, default, context |> repDebugEnv declEnv)
                end (* TupleField. *)

            |   Cons(cl, width) =>
                let
                    (* Code-generate a list of constructors. "constrsLeft" is the
                       number of constructors left to deal with. If this gets to 1
                       we have dealt with all the rest. *)
                    fun genConstrs ([]:consrec list, _) = 
                         (* Come to the end without exhausting the datatype. *)
                          othermatches(activeDefaults, default, context |> repDebugEnv declEnv)
          
                      | genConstrs (({patts, constructor, appliedTo, polyVars, ...}:consrec):: ps, constrsLeft) =
                        let
                            (* If this is not in the active set we skip it. *)
                            val newActive = patts intersect active;
                        in
                            (* If the set is empty we don't bother with this constructor. *)
                            if newActive eq empty
                            then genConstrs(ps, constrsLeft) (* N.B. NOT "(constrsLeft - 1)", since we haven't matched! *)
                            else if constrsLeft = 1
                            then 
                                (* We have put all the other constructors in this
                                   datatype out so there is no need to test for this case. *)
                                codePatt(appliedTo, makeInverse (constructor, polyVars, declLoad, level, typeVarMap),
                                    newActive plus activeDefaults, othermatches, default,
                                    context |> repDebugEnv declEnv)
                            else
                            let
                                (* Code generate the choice. *)
                                val testCode = makeGuard (constructor, polyVars, declLoad, level, typeVarMap);
          
                                (* If it succeeds we have to take apart the argument of the
                                   constructor. *)
                                val thenCode : patcode = 
                                    codePatt(appliedTo, makeInverse (constructor, polyVars, declLoad, level, typeVarMap),
                                        newActive plus activeDefaults,
                                        othermatches, default, context |> repDebugEnv declEnv)
               
                                (* Otherwise we look at the next constructor in the list. *)
                                val elseCode : patcode = genConstrs(ps, constrsLeft - 1);
                            in
                                if #pat thenCode = #pat elseCode andalso #pat thenCode >= 0
                                then elseCode (* This didn't actually do any discrimination,
                                              probably because a default was above a constructor. *)
                                else makePatcode (mkIf (testCode, patCodeBlock thenCode, patCodeBlock elseCode)) ~1
                            end
                        end
                in
                    genConstrs (cl, width)
                end

            |   Excons cl =>
                let
                    fun genExnConstrs ([]:consrec list)= 
                         (* Process the default matches, if any. *)
                        othermatches(activeDefaults, default, context |> repDebugEnv declEnv)
          
                    |   genExnConstrs ({patts, constructor, appliedTo, ...}:: ps) =
                        let
                            (* If this is not in the active set we skip it. *)
                            val newActive = patts intersect active;
                        in
                            (* If the set is empty we don't bother with this constructor. *)
                            if newActive eq empty
                            then genExnConstrs ps
                            else
                            let (* Code generate the choice. *)
                               (* Called if this exception constructor matches, but
                                  none of the active patterns match, either because
                                  the values in the datatype do not match (e.g. value
                                  is A 2, but pattern is A 1), or because of other
                                  fields in the tuple (e.g. value is (A, 2) but
                                  pattern is (A, 1)). If this were an ordinary
                                  constructor we would go straight to the default,
                                  because if it matches this constructor it could not
                                  match any of the others, but with exceptions it can
                                  match other exceptions, so we have to test them.
      
                                  We do this by generating MatchFail, which jumps
                                  to the "handler" of the enclosing AltMatch construct.
                               *)

                                (* If the match fails we look at the next constructor in the list. *)
                                val elseCode : patcode = genExnConstrs ps;

                                fun codeDefault () = matchFailCode
              
                                val testCode = makeGuard (constructor, [], declLoad, level, typeVarMap);
          
                                (* If it succeeds we have to take apart the argument of the
                                   constructor. *)
                                val thenCode : patcode = 
                                    codePatt (appliedTo, makeInverse (constructor, [], declLoad, level, typeVarMap),
                                        newActive, othermatches, codeDefault,
                                        context |> repDebugEnv declEnv)
               
                            in
                                (* Needed? *)
                                if #pat thenCode = #pat elseCode andalso #pat thenCode >= 0
                                then elseCode
            
                                else
                                    makePatcode
                                    (
                                        mkAltMatch
                                        (
                                            mkIf (testCode, patCodeBlock thenCode, MatchFail),
                                            patCodeBlock elseCode
                                        )
                                    ) ~1
                            end
                        end
                in
                  (* Must reverse the list because exception constructors are
                     in reverse order from their order in the patterns, and
                     ordering matters for exceptions. *)
                    genExnConstrs (rev cl)
                end

            |   Scons sl =>
                let (* Int, char, string *)
        
                    (* Generate if..then..else for each of the choices. *)
                    fun foldConstrs ([]: sconsrec list) =
                        othermatches(activeDefaults, default, context |> repDebugEnv declEnv)
 
                    |   foldConstrs (v :: vs) =
                        let 
                            (* If this pattern is in the active set
                               we discriminate on it. *)
                            val newActive = (#patts v) intersect active;
  
                        in
                            if isEmptySet newActive
                            then (* No point *) foldConstrs vs
                            else
                            let
                                val constVal = case #specVal v of NONE => CodeZero | SOME w => mkConst w
                                (* Compare for equality.  The order of the arguments ought to be irrelevant but
                                   this works better for string comparisons.  See comment on stringEquality
                                   in TYPE_TREE. *)
                                val testCode = mkEval(#eqFun v, [mkTuple[declLoad, constVal]])
                   
                                (* If it is a binding we turn the test round - see
                                   comment in genConstrs. *)
                                val rest: patcode = 
                                    othermatches(newActive plus activeDefaults, default, context |> repDebugEnv declEnv);
            
                                val elsept = foldConstrs vs
                            in
                                if (#pat rest) = (#pat elsept) andalso (#pat rest) >= 0
                                then elsept
                                else makePatcode (mkIf (testCode, patCodeBlock rest, patCodeBlock elsept)) ~1
                            end 
                        end (* foldConstrs *);
                in
                    foldConstrs sl
                end

            |   _ =>  (* wild - no choices to make here. *)
                    othermatches(activeDefaults, default, context |> repDebugEnv declEnv)
    in
        { decs = declDecs @ #decs pattCode, exp = #exp pattCode, pat = #pat pattCode}
    end (* codePatt *)

    (* Naive pattern matching. *)
    local
        infix 6 andAlso

        fun isTrue c =
            case evalue c of
                SOME v => isShort v andalso toShort v = 0w1
            |   NONE => false

        fun a andAlso b =
            if isTrue a then b
            else if isTrue b then a
            else mkCand(a, b)
    in
        fun codePattTests(Aot {patts, defaults, ...}, arg, selected, context: cgContext as { level, typeVarMap, mkAddr, ...}) =
            if selected inside defaults
            then CodeTrue (* The pattern we want defaults here so always matches *)
            else case patts of
                TupleField [patt] =>
                    (* Optimised unary tuple/record - no indirection. *)
                    codePattTests(patt, arg, selected, context)

            |   TupleField asTuples =>
                let
                    (* Tuples - test each field - bind to a variable
                       to avoid duplicating the code for "arg". *)
                    val addressOfVar = mkAddr 1
                    val bind = mkDec (addressOfVar, arg)
                    and load = mkLoadLocal addressOfVar

                    fun testField(tuple :: next, n) =
                        codePattTests(tuple, mkInd(n, load), selected, context)
                            andAlso testField(next, n+1)
                    |   testField([], _) = CodeTrue
                in
                    mkEnv([bind], testField(asTuples, 0))
                end

            |   Cons(cl, _) =>
                let
                    (* Find the constructor that contains this pattern.  It must be
                       there somewhere since it's not in the defaults. *)
                    val { constructor, appliedTo, polyVars, ...} =
                        valOf(List.find(fn{patts, ...} => selected inside patts) cl)
                    (* Bind to a variable to avoid duplicating code for "arg". *)
                    val addressOfVar = mkAddr 1
                    val bind = mkDec (addressOfVar, arg)
                    and load = mkLoadLocal addressOfVar
                    val testCode = makeGuard (constructor, polyVars, load, level, typeVarMap)
                    and inverse = makeInverse (constructor, polyVars, load, level, typeVarMap)
                    val testResult = codePattTests(appliedTo, inverse, selected, context)
                in
                    mkEnv([bind], testCode andAlso testResult)
                end

            |   Excons cl =>
                let
                    val addressOfVar = mkAddr 1
                    val bind = mkDec (addressOfVar, arg)
                    and load = mkLoadLocal addressOfVar
                    val { constructor, appliedTo, ...} =
                        valOf(List.find(fn{patts, ...} => selected inside patts) cl)
                    val testCode = makeGuard (constructor, [], load, level, typeVarMap)
                    and inverse = makeInverse (constructor, [], load, level, typeVarMap)
                    val testResult = codePattTests(appliedTo, inverse, selected, context)
                in
                    mkEnv([bind], testCode andAlso testResult)
                end

            |   Scons cl =>
                let
                    (* Special constant e.g. 1, "test".  Just test the value. *)
                    val { eqFun, specVal, ...} =
                        valOf(List.find(fn{patts, ...} => selected inside patts) cl)
                    val constVal = case specVal of NONE => CodeZero | SOME w => mkConst w
                in
                    mkEval(eqFun, [mkTuple[arg, constVal]])
                end

            |   Wild => raise InternalError "Wild card but pattern is not in defaults"

        (* Generate the bindings for pattern variables along with the debugging environment. *)
        fun codePattVars(Aot {patts, defaults, vars, ...}, arg, selected, context as { level, typeVarMap, ...}) =
        let
            val (declLoad, declDecs, declEnv) = bindPattVars(arg, vars, context)
            val nextContext = context |> repDebugEnv declEnv
        in
            if selected inside defaults
            then (declDecs, nextContext) (* Nothing more *)
            else case patts of
                TupleField [patt] =>
                let
                    val (subDecs, resContext) = codePattVars(patt, declLoad, selected, nextContext)
                in
                    (declDecs @ subDecs, resContext)
                end

            |   TupleField asTuples =>
                let
                    fun decField(tuple :: next, n, nContext) =
                        let
                            val (decs, rContext) = codePattVars(tuple, mkInd(n, declLoad), selected, nContext)
                            val (rDecs, cContext) = decField(next, n+1, rContext)
                        in
                            (decs @ rDecs, cContext)
                        end
                    |   decField([], _, nContext) = ([], nContext)

                    val (subDecs, resContext) = decField(asTuples, 0, nextContext)
                in
                    (declDecs @ subDecs, resContext)
                end
                
            |   Cons(cl, _) =>
                let
                    val { constructor, appliedTo, polyVars, ...} =
                        valOf(List.find(fn{patts, ...} => selected inside patts) cl)
                    val inverse = makeInverse (constructor, polyVars, declLoad, level, typeVarMap)
                    val (subDecs, resContext) = codePattVars(appliedTo, inverse, selected, nextContext)
                in
                    (declDecs @ subDecs, resContext)
                end

            |   Excons cl =>
                let
                    val { constructor, appliedTo, ...} =
                        valOf(List.find(fn{patts, ...} => selected inside patts) cl)
                    val inverse = makeInverse (constructor, [], declLoad, level, typeVarMap)
                    val (subDecs, resContext) = codePattVars(appliedTo, inverse, selected, nextContext)
                in
                    (declDecs @ subDecs, resContext)
                end

            |   Scons _ => (declDecs, nextContext) (* No further variables. *)

            |   Wild => raise InternalError "Wild card but pattern is not in defaults"
        end
    end

    local
         (* Raises an exception. *)
        fun raiseException(exName, exIden, line) =
            mkRaise (mkTuple [exIden, mkStr exName, CodeZero, codeLocation line]);
        (* Create exception values - Small integer values are used for
           run-time system exceptions. *)
        val bindExceptionVal  = mkConst (toMachineWord EXC_Bind);
        val matchExceptionVal = mkConst (toMachineWord EXC_Match);
    in
        (* Raise match and bind exceptions. *)
        fun raiseMatchCode line : patcode =
            makePatcode (raiseException("Match", matchExceptionVal, line)) 0
        
        fun raiseBindException line = raiseException("Bind", bindExceptionVal, line)
        and raiseMatchException line = raiseException("Match", matchExceptionVal, line)
    end

    (* Process a pattern in a binding. *)
    (* This previously used codePatt with special options to generate the correct
       structure for a binding.  This does the test separately from loading
       the variables.  If the pattern is not exhaustive this may do more work
       since the pattern is taken apart both in the test and for loading.  *)
    fun codeBindingPattern(vbDec, arg, line, context) =
    let
        (* Build the tree. *)
        val andortree = buildAot(vbDec, aotEmpty, 1, line, context)
        (* Generate any tests.  If this actually does anything the
           pattern is not exhaustive. *)
        val testCode = codePattTests(andortree, arg, 1, context)
        (* Load the variables.  The debug environment is discarded and instead
           created by the calling code. *)
        val (codeDecs, _) = codePattVars(andortree, arg, 1, context)
        
        val { exhaustive, ...} = evaluateMatch(andortree, 1)
    in
        if exhaustive
        then (codeDecs, true)
        else (mkNullDec(mkIf(testCode, CodeZero, raiseBindException line)) :: codeDecs, false)
    end


    (* Process a set of patterns in a match. *)
    (* Naive match code.  Doesn't check for exhaustiveness or redundancy. *)
    fun codeMatchPatterns(alt, arg, isHandlerMatch, lineNo, codePatternExpression, context) =
    let
        val noOfPats  = length alt
        val andortree = buildTree(alt, context)
        val { exhaustive, redundant, useNaive } = evaluateMatch(andortree, noOfPats)
    in
        if useNaive
        then
        let
            (* Naive match.  Generate if test then exp for each pattern.
               This is used if the pattern is sparse.  *)
            fun allPatts patsToDo =
                if isEmptySet patsToDo
                then
                (
                    if isHandlerMatch
                    then mkRaise arg
                    else raiseMatchException lineNo
                )
                else
                let
                    val thisPatt = first patsToDo
                in
                    if singleton thisPatt eq patsToDo andalso exhaustive
                    then (* We don't need the last test. *)
                    let
                        val (codeDecs, newContext) = codePattVars(andortree, arg, thisPatt, context)
                        val expCode = codePatternExpression(thisPatt-1, newContext)
                    in
                        mkEnv(codeDecs, expCode)
                    end
                    else
                    let
                        val testCode = codePattTests(andortree, arg, thisPatt, context)
                        val (codeDecs, newContext) = codePattVars(andortree, arg, thisPatt, context)
                        val expCode = codePatternExpression(thisPatt-1, newContext)
                        val nextPatts = allPatts(patsToDo diff singleton thisPatt)
                    in
                        mkIf(testCode, mkEnv(codeDecs, expCode), nextPatts)
                    end
                end
        in
            (* We must remove the redundant patterns because redundancy
               is reported in the caller. *)
            (allPatts ((from 1 noOfPats) diff redundant), exhaustive)
        end
        else
        let
            (* Make some code to insert at defaults.  If this is a handler reraise the
               original exception otherwise raise Match *)
            fun codeDefault () =
            (
                exhaustive andalso raise InternalError "codeDefault called but exhaustive";
                if isHandlerMatch
                then makePatcode (mkRaise arg) 0
                else raiseMatchCode lineNo
            )

            (* This function is called when we done all the discrimination
               we can. We fire off the first pattern in the set. *)
            fun firePatt(pattsLeft: patSet, default, newContext) =
            if isEmptySet pattsLeft
            then default ()
            else
            let
                val pattChosen = first pattsLeft
                (* Process the pattern.  The context we pass includes the debugging environment
                   of all the variables in the pattern. *)
                val expCode = codePatternExpression(pattChosen - 1, newContext)
            in
                makePatcode expCode pattChosen
            end

            val code = codePatt(andortree, arg, from 1 noOfPats, firePatt, codeDefault, context)
        in
            (mkEnv(#decs code, #exp code), exhaustive)
        end

    end

    (* Types that can be shared. *)
    structure Sharing =
    struct
        type parsetree = parsetree
        type typeVarMap = typeVarMap
        type level = level
        type codetree = codetree
        type matchtree = matchtree
        type codeBinding = codeBinding
        type environEntry = environEntry
        type lexan = lexan
    end

end;

