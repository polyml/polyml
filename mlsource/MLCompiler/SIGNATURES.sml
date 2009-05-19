(*
    Copyright David C. J. Matthews 2009
    Largely extracted from STRUCTURES_.ML

    Copyright (c) 2000
        Cambridge University Technical Services Limited
        
    Modified D.C.J. Matthews 2001-2009

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
    Title:      Module Structure and Operations.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)

functor SIGNATURES (
    structure LEX : LEXSIG
    structure STRUCTVALS : STRUCTVALSIG;
    structure EXPORTTREE: EXPORTTREESIG
    structure PRETTY : PRETTYSIG
    structure COPIER: COPIERSIG
    structure TYPETREE : TYPETREESIG
    structure PARSETREE : PARSETREESIG
    structure VALUEOPS : VALUEOPSSIG;

    structure UNIVERSALTABLE:
    sig
        type universal = Universal.universal
        type univTable
        type 'a tag = 'a Universal.tag

        val univEnter:  univTable * 'a tag * string * 'a -> unit;
        val univLookup: univTable * 'a tag * string -> 'a option;
        val univFold:   univTable * (string * universal * 'a -> 'a) * 'a -> 'a;
    end;

    structure DEBUG :
    sig
        val errorDepthTag : int Universal.tag
        val getParameter :
               'a Universal.tag -> Universal.universal list -> 'a 
    end;

    structure UTILITIES :
    sig
        val noDuplicates: (string * 'a * 'a -> unit) -> 
             { apply: (string * 'a -> unit) -> unit,
               enter:  string * 'a -> unit,
               lookup: string -> 'a option };

        val searchList: unit -> { apply: (string * 'a -> unit) -> unit,
                                enter:  string * 'a -> unit,
                                lookup: string -> 'a option };
    end;

    sharing LEX.Sharing = TYPETREE.Sharing = PARSETREE.Sharing
        = PRETTY.Sharing = EXPORTTREE.Sharing = STRUCTVALS.Sharing = COPIER.Sharing
        = VALUEOPS.Sharing = UNIVERSALTABLE

) : SIGNATURESSIG =
struct
    open Misc (* Open this first because it contains Value. *)
    open LEX STRUCTVALS EXPORTTREE PRETTY COPIER TYPETREE PARSETREE UNIVERSALTABLE DEBUG
    open VALUEOPS UTILITIES Universal

    datatype sigs =
        SignatureIdent of string * location  (* A signature name *)

    |   StructureSig   of structSigBind list * location

    |   SigDec         of sigs list *location (* sig ... end *)

    |   ValSig         of (* Signature of a value. *)
            { name: string * location, typeof: typeParsetree, line: location }

    |   ExSig          of (* Signature of an exception.  May be a nullary exception. *)
            { name: string * location, typeof: typeParsetree option, line: location }

    |   CoreType      of (* Any other decln. *)
        {
            dec:   parsetree,           (* The value *)
            location: location
        }

    |   Sharing        of shareConstraint    (* Sharing constraints. *)
    |   WhereType       of whereTypeStruct    (* type realisation. *)
    |   IncludeSig     of sigs list       (* Include. *)
    |   EmptySig    (* Error cases. *)

  withtype shareConstraint =
      {
        isType: bool,
        shares: string list,
        line:   location
      }

  and structSigBind =
      {
        name:      string,         (* The name of the structure *)
        nameLoc:   location,
        sigStruct: sigs * bool * location,
        line:      location
      }

  and whereTypeStruct =
      {
        sigExp: sigs,
        typeVars: typeVarForm list,
        typeName: string,
        realisation: types,
        line: location
      }

    val mkSigIdent = SignatureIdent;
  
    fun mkCoreType (dec, location) =
        CoreType { dec = dec, location = location };
  
    fun mkValSig (nameLoc, typeof, line) = 
      ValSig 
        {
          name    = nameLoc,
          typeof  = typeof,
          line    = line
        };
  
    fun mkExSig (nameLoc, typeof, line) = 
       ExSig
        {
          name    = nameLoc,
          typeof  = typeof,
          line    = line
        };
  
    fun mkSharing (isType, shares, line) = 
        Sharing {
          isType = isType,
          shares = shares,
          line   = line
        };

    fun mkWhereType (sigexp, typeVars, name, types, line) = 
        WhereType {
          sigExp      = sigexp,
		  typeVars    = typeVars,
          typeName    = name,
          realisation = types,
          line        = line
        };

	val mkInclude = IncludeSig
    and mkStructureSig = StructureSig
    and mkSig = SigDec

    fun mkStructureSigBinding ((name, nameLoc), signat, fullLoc):structSigBind  =
        {
            name      = name,
            nameLoc   = nameLoc,
            sigStruct = signat,
            line      = fullLoc
        }

    val emptySig = EmptySig

    (* Make a signature for initialisating variables and for
       undeclared signature variables. *)
    val noLocation =
        { file="", startLine=0, startPosition=0, endLine=0, endPosition=0 }
    val undefinedSignature =
       makeSignature("UNDEFINED", makeSignatureTable(),
                0, 0, noLocation, fn _ => raise Subscript);

    fun displayList ([], separator, depth) dodisplay = []
    
    |   displayList ([v], separator, depth) dodisplay =
            if depth <= 0
            then [PrettyString "..."]
            else [dodisplay (v, depth)]
      
    |   displayList (v::vs, separator, depth) dodisplay =
            if depth <= 0
            then [PrettyString "..."]
            else
            let
                val brk = if separator = "," orelse separator = ";" then 0 else 1
            in
                PrettyBlock (0, false, [],
                    [
                        dodisplay (v, depth),
                        PrettyBreak (brk, 0),
                        PrettyString separator
                    ]
                ) ::
                PrettyBreak (1, 0) ::
                displayList (vs, separator, depth - 1) dodisplay
            end (* displayList *)

    val displayType = TYPETREE.display;

    fun displaySigs (str, depth) =
        if depth <= 0 (* elide further text. *)
        then PrettyString "..."

        else
        case str of
            StructureSig (structList : structSigBind list, _) =>
            let
                    fun displaySigsBind (
                            {name, sigStruct=(sigStruct, opaque, _), ...}: structSigBind, depth) =
                        PrettyBlock (3, false, [],
                            [
                                PrettyString name,
                                PrettyString (if opaque then " :>" else " :"),
                                PrettyBreak (1, 0),
                                displaySigs (sigStruct, depth - 1)
                            ]
                        )
            in
                PrettyBlock (3, false, [],
                    PrettyString "structure" ::
                    PrettyBreak (1, 0) ::
                    displayList (structList, "and", depth) displaySigsBind
                )
            end

        |   SignatureIdent (name : string, _) =>
            PrettyString name

        |   SigDec (structList : sigs list, _) =>
            PrettyBlock (1, true, [],
                PrettyString "sig" ::
                PrettyBreak (1, 0) ::
                displayList (structList, "", depth) displaySigs @
                [ PrettyBreak (1, 0), PrettyString "end"]
            )

        |   ValSig {name = (name, _), typeof, ...} =>
            PrettyBlock (0, false, [],
                [
                    PrettyString "val",
                    PrettyBreak (1, 1),
                    PrettyString (name ^ " :"),
                    PrettyBreak (1, 0),
                    displayType (typeFromTypeParse typeof, depth - 1, emptyTypeEnv)
                ]
            )

        |   ExSig {name = (name, _), typeof = NONE, ...} =>
            PrettyBlock (0, false, [],
                [
                    PrettyString "exception",
                    PrettyBreak (1, 1),
                    PrettyString (name)
                ]
            )
 
        |   ExSig {name = (name, _), typeof = SOME typeof, ...} =>
            PrettyBlock (0, false, [],
                [
                    PrettyString "exception",
                    PrettyBreak (1, 1),
                    PrettyString (name ^ " :"),
                    PrettyBreak (1, 0),
                    displayType (typeFromTypeParse typeof, depth - 1, emptyTypeEnv)
                ]
            )

        |   Sharing { isType, shares, ... } =>
            PrettyBlock (3, false, [],
                PrettyString "sharing" ::
                PrettyBreak (1, 0) ::
                (
                    if not isType then []
                    else [ PrettyString "type", PrettyBreak (1, 0) ]
                ) @
                displayList (shares, "=", depth)
                    (fn (name, depth) => PrettyString name)
            )

        |   WhereType { sigExp, typeVars, typeName, realisation, ... } =>
            PrettyBlock (3, false, [],
                displaySigs (sigExp, depth) ::
                PrettyBreak (1, 0) ::
                PrettyString "where" ::
                PrettyBreak (1, 0) ::
                PrettyString "type" ::
                PrettyBreak (1, 0) ::
                displayTypeVariables (typeVars, depth) @
                [
                    PrettyString typeName,
                    PrettyBreak (1, 0),
                    PrettyString "=",
                    PrettyBreak (1, 0),
                    displayType (realisation, depth - 1, emptyTypeEnv)
                ]
            )

        |   IncludeSig (structList : sigs list) =>
            PrettyBlock (3, true, [],
                PrettyString "include" ::
                PrettyBreak (1, 0) ::
                displayList (structList, "", depth - 1) displaySigs
            )

        |   CoreType {dec, ...} =>
                ptDisplay (dec, depth - 1)

        | EmptySig =>
            PrettyString "<bad>"
      (* End displaySigs *)

    fun sigExportTree(navigation, s: sigs) =
    let
         (* Common properties for navigation and printing. *)
        val commonProps =
            PTprint(fn d => displaySigs(s, d)) ::
            exportNavigationProps navigation

        fun asParent () = sigExportTree(navigation, s)
    in
        case s of
            StructureSig(sbl, location) =>
            let
                fun exportSB(navigation, sb as {name, nameLoc, sigStruct=(theSig, _, _), line, ...}) =
                    let
                        fun exportThis () = exportSB(navigation, sb)
                        fun getName () =
                            getStringAsTree({parent=SOME exportThis, previous=NONE, next=SOME getSigStruct}, name, nameLoc, [])
                        
                        and getSigStruct () =
                            sigExportTree({parent=SOME exportThis, previous=SOME getName, next=NONE}, theSig)
                    in
                        (line, PTfirstChild getName :: exportNavigationProps navigation)
                    end

                val expChild = exportList(exportSB, SOME asParent) sbl
            in
                (location, expChild @ commonProps)
            end

        |   SignatureIdent _ => (nullLocation, commonProps)

        |   SigDec(structList, location) =>
                (location, exportList(sigExportTree, SOME asParent) structList @ commonProps)

        |   ValSig{name=(name, nameLoc), typeof, line, ...} =>
            let
                (* The first position is the value name, the second the type. *)
                (* TODO: Include the actual type as PTtype? *)
                fun getName () =
                    getStringAsTree({parent=SOME asParent, previous=NONE, next=SOME getType}, name, nameLoc, [])
                and getType () =
                    typeExportTree({parent=SOME asParent, previous=SOME getName, next=NONE}, typeof)
            in
                (line, PTfirstChild getName :: commonProps)
            end

        |   ExSig{name=(name, nameLoc), typeof = NONE, line, ...} =>
            let
                (* The first position is the value name, the second the type. *)
                fun getName () =
                    getStringAsTree({parent=SOME asParent, previous=NONE, next=NONE}, name, nameLoc, [])
            in
                (line, PTfirstChild getName :: commonProps)
            end

        |   ExSig{name=(name, nameLoc), typeof = SOME typeof, line, ...} =>
            let
                (* The first position is the value name, the second the type. *)
                (* TODO: Include the actual type as PTtype? *)
                fun getName () =
                    getStringAsTree({parent=SOME asParent, previous=NONE, next=SOME getType}, name, nameLoc, [])
                and getType () =
                    typeExportTree({parent=SOME asParent, previous=SOME getName, next=NONE}, typeof)
            in
                (line, PTfirstChild getName :: commonProps)
            end

        |   CoreType {dec, ...} => (* A value parse-tree entry. *)
                getExportTree(navigation, dec)

        |   Sharing _ => (nullLocation, commonProps)

        |   WhereType _ => (nullLocation, commonProps)

        |   IncludeSig _ => (nullLocation, commonProps)

        |   EmptySig => (nullLocation, commonProps)
 
    end

    (* Puts out an error message and then prints the piece of tree. *)
    fun errorMsgNear (lex, hard, near, lno, message) : unit =
    let
        val parameters = debugParams lex
        val errorDepth = getParameter errorDepthTag parameters
    in
        reportError lex
        {
            hard = hard, location = lno, message = message,
            context = SOME(near errorDepth)
        }
    end

    fun errorNear(lex, hard, near, lno, message: string) =
        errorMsgNear (lex, hard, near, lno,
            PrettyBlock (0, false, [], [PrettyString message]))

    fun giveError (sVal : sigs, lno : LEX.location, lex : lexan) : string -> unit =
        fn (message : string) => errorNear (lex, true, fn n => displaySigs(sVal, n), lno, message);

    val makeEnv = fn x => let val Env e = makeEnv x in e end;

    (* Formal paramater to a functor - either value or exception. *)
    fun mkFormal (name : string, class, typ, addr, locations) =
  	    Value{class=class, name=name, typeOf=typ, access=Formal addr, locations=locations}

      (* Get the value from a signature-returning expression
         (either the name of a signature or sig ... end.
         The type IDs in the signature are bound names. *)
    fun sigVal(str           : sigs,
             initTypeId    : int,
             outerTypeIdEnv: int->typeId,
             Env globalEnv : env,
             lex,
             lno           : LEX.location
            ) : signatures =
    let
        datatype varId =
            SharedWith of int (* Index of shared ID, always less than current index. *)
        |   VariableSlot of { isDatatype: bool, boundId: typeId, descriptions: string list }
        |   FreeSlot of typeId (* Bound to a Free type ID. *)
        |   Unset

        val idCount = ref initTypeId
        val mapArray =
            StretchArray.stretchArray(10 (* Guess initial size. *), Unset)

        fun makeVariableId(isEq, description: typeIdDescription) =
        let
            (* Make a new bound ID after any existing ones. *)
            val newIdNumber = !idCount before (idCount := !idCount+1)
            val newId =
                makeBoundIdWithEqUpdate(Formal 0 (* Not used. *), newIdNumber, isEq, description)
            (* Enter a variable entry in the array. *)
            val arrayEntry =
                VariableSlot{ isDatatype=false, boundId=newId, descriptions = [#name description] }
            val () = StretchArray.update(mapArray, newIdNumber-initTypeId, arrayEntry)
        in
            newId
        end

        (* Follow a chain of shared IDs.  This should terminate because we always
           point down the array. *)
        fun realId n =
            case StretchArray.sub(mapArray, n) of
                SharedWith m =>
                    if m >= n
                    then raise InternalError "realId: Sharing loop"
                    else realId m
            |   id => id

        fun isVariableId(Bound{offset, ...}) =
            if offset < initTypeId then false (* Outside the signature. *)
            else
            (
                case realId(offset-initTypeId) of
                    VariableSlot _ => true
                |   FreeSlot _ => false
                |   _ => raise InternalError "isVar"
            )
        |   isVariableId _ (* Free or TypeFunction *) = false
        
        fun linkFlexibleTypeIds(typeId1, typeId2) =
        (* Link together and share two IDs.  The result is an equality type if either
           was an equality type and a datatype if either was a datatype. *)
        case (typeId1, typeId2) of
            (Bound{offset=offset1, ...}, Bound{offset=offset2, ...}) =>
        (
            case (realId offset1, realId offset2) of
                (VariableSlot{isDatatype=isDatatype1, descriptions = desc1,
                              boundId=Bound{eqType=eqType1, offset=off1, description, ...}},
                 VariableSlot{isDatatype=isDatatype2, descriptions = desc2,
                              boundId=Bound{eqType=eqType2, offset=off2, ...}}) =>
            if off1 = off2
            then () (* They may already share. *)
            else
            let
                val resOffset = Int.min(off1, off2)
                val setOffset = Int.max(off1, off2)
                val newId =
                    makeBoundId(Formal 0, resOffset, pling eqType1 orelse pling eqType2, description (* Not used *))
                val newEntry =
                    VariableSlot{ isDatatype=isDatatype1 orelse isDatatype2, boundId=newId, descriptions = desc1 @ desc2 }
            in
                StretchArray.update(mapArray, resOffset-initTypeId, newEntry);
                StretchArray.update(mapArray, setOffset-initTypeId, SharedWith(resOffset-initTypeId))
            end
            |   _ => raise InternalError "linkFlexibleTypeIds: not variable"
        )
        |   _ => raise InternalError "linkFlexibleTypeIds: not bound"

        (* Process a sharing constraint. *)
        fun applySharingConstraint 
              ({shares = tlist, isType, line}: shareConstraint,
               Env tEnv    : env,
               near        : sigs)
              : unit =
        let
            fun shareTypes(typeA, typeB, lno) =
            let
                fun cantShare reason =
                let
                    fun showTypeCons t =
                    let
                        val context =
                            case List.find(fn DeclaredAt _ => true | _ => false) (tcLocations t) of
                                SOME(DeclaredAt loc) => [ContextLocation loc]
                            |   _ => []
                    in
                        PrettyBlock(0, false, context, [PrettyString(tcName t)])
                    end
                in
                    errorMsgNear (lex, true, fn n => displaySigs(near, n), lno,
                        PrettyBlock(0, false, [],
                            [
                                PrettyString "Cannot share type",
                                PrettyBreak(1, 2),
                                showTypeCons typeA,
                                PrettyBreak(1, 0),
                                PrettyString "with type",
                                PrettyBreak(1, 0),
                                showTypeCons typeB,
                                PrettyBreak(0, 0),
                                PrettyString ".",
                                PrettyBreak(1, 0),
                                PrettyString reason
                            ]))
                end
            in
                if isUndefinedTypeConstr typeA orelse isUndefinedTypeConstr typeB
                then ()
                else if tcArity typeA <> tcArity typeB (* Check arity. *)
                then cantShare "The type constructors take different numbers of arguments."
                
                (* The type constructors are only looked up in the signature but they
                   already may be set to another type through a "where type" or they may
                   have been created with Free IDs through type t=s declarations.  This
                   could be a free identifier or a type function.  *)
                else if not (isVariableId (tcIdentifier typeA))
                then cantShare (tcName typeA ^ " is already defined as another type.")

                else if not (isVariableId (tcIdentifier typeB))
                then cantShare (tcName typeB ^ " is already defined as another type.")
                else linkFlexibleTypeIds(tcIdentifier typeA, tcIdentifier typeB)
            end (* shareTypes *);


(********************* Start of SPF's rewrite (incomplete!) **********************)

        (* The purpose of the following code was to fix some bugs in my
           original structure sharing code for ML90 and also to simplify it.  In
           particular it detected cyclic sharing constraints more accurately.
           These were cases of "sharing A = A.B" which were illegal in ML90
           but are legal in ML97 (it's a short-hand for sharing type A.t = A.B.t).
           Much of it is no longer relevant since we are only interested in
           sharing types in ML97. I've simplified it somewhat but it
           might be worth simplifying it further. DCJM 27/7/00. *)

        (* useful stuff *)
        (* sets as unordered lists *)
        fun member (eq : 'a * 'a -> bool) x []       = false
          | member (eq : 'a * 'a -> bool) x (h :: t) =
              eq (x, h) orelse member eq x t;
        
        fun addToSet (eq : 'a * 'a -> bool) x l =
          if member eq x l then l else x :: l;
        
        fun union (eq : 'a * 'a -> bool) []       l = l
          | union (eq : 'a * 'a -> bool) (h :: t) l =
              if member eq h l then union eq t l else h :: union eq t l;
              
        fun unionMap (eq : 'b * 'b -> bool) (f : 'a -> 'b list) ([] : 'a list) : 'b list = []
          | unionMap (eq : 'b * 'b -> bool) (f : 'a -> 'b list) (h :: t) =
              union eq (f h) (unionMap eq f t)
      
        type virtStruct = signatures list;
        
        (* Find all the substructure names occurring in a single structure *)
        fun subStructureNames (sigVal : signatures) : string list = 
           univFold
            (sigTab sigVal,
             fn (structName, dVal, names) =>
               if tagIs structVar dVal then structName :: names else names,
             []);
  
        (* Find all the type constructor names occurring in a single structure *)
        fun typeConstrNames (sigVal : signatures) : string list = 
           univFold
            (sigTab sigVal,
             fn (typeName, dVal, names) =>
               if tagIs typeConstrVar dVal then typeName :: names else names,
             []);
      
        (* Find all the substructure names occurring in a virtual structure. *)
        fun virtSubStructureNames sigs : string list =
          unionMap (op =) subStructureNames sigs;
         
        (* Find all the type constructor names occurring in a virtual structure. *)
        fun virtTypeConstrNames sigs : string list =
          unionMap (op =) typeConstrNames sigs;
         
        (* Find the named virtual substructure of a virtual structure. *)
        fun getVirtSubStructure sigs (strName : string) : virtStruct =
        let
           (* 
              Look up the name of the substructure. It may not
              be there because not every substructure occurs
              in every structure of the virtual structure.
           *)
          val substrList : signatures list =
            List.foldr
              (fn (sigVal : signatures, res : signatures list) =>
                  case univLookup (sigTab sigVal, structVar, strName) of
                     SOME str => structSignat str :: res
                  |  NONE => res)
             []
             sigs;
        in
          substrList
        end;
        
        (* Find the named typed constructors of a virtual structure. *)
        fun getVirtTypeConstrs sigs (typeName : string) : typeConstrs list =
        let
           fun funForFold (sigVal : signatures, res : typeConstrs list) : typeConstrs list =
                case univLookup (sigTab sigVal, typeConstrVar, typeName) of
                 SOME r => r :: res
              |  NONE => res
        in
          List.foldr funForFold [] sigs
        end;
                
        (* Find all the substructure names occurring in a list of virtual structures *)
        fun listVirtSubStructureNames (virts : virtStruct list) : string list = 
           unionMap (op =) virtSubStructureNames virts;
        
        (* Find all the type constructor names occurring in a list of virtual structures *)
        fun listVirtTypeConstrNames (virts : virtStruct list) : string list = 
           unionMap (op =) virtTypeConstrNames virts;
      
        (* Find all the named virtual substructures occurring in a list of virtual structures *)
        fun listVirtSubStructures (virts : virtStruct list) (strName : string) : virtStruct list = 
        let
          fun funForFold (vs : virtStruct, res : virtStruct list) : virtStruct list = 
            getVirtSubStructure vs strName :: res
        in
          List.foldr funForFold [] virts 
        end;
        
        (* Find all the named virtual type constructors occurring in a list of virtual structures *)
        fun listVirtTypeConstrs (virts : virtStruct list) (strName : string) : typeConstrs list = 
        let
          fun funForFold (vs : virtStruct, res : typeConstrs list) : typeConstrs list = 
            (getVirtTypeConstrs vs strName) @ res
        in
          List.foldr funForFold [] virts 
        end;
        
        fun shareVirtStructs ([], _)      = raise InternalError "Empty sharing list"
          | shareVirtStructs (virts,  _)  = 
         let
           (* Share the types *)
           val typeConstrNames : string list = listVirtTypeConstrNames virts;
           
           fun shareVirtTypeConstr (typeName : string) : unit = 
           let
             (* Find all the type constructors with this name *)
             val tcs : typeConstrs list = listVirtTypeConstrs virts typeName;
             
             fun shareWith (tc : typeConstrs) ([] : typeConstrs list) = ()
               | shareWith tc (h :: t) = 
             let
               val U : unit = shareTypes (tc, h, lno);
             in
               shareWith tc t
             end;
             
             fun shareAll ([] : typeConstrs list) = ()
               | shareAll (h :: t) =
             let 
               val U : unit = shareWith h t
             in
               shareAll t
             end;
           in  
             (* Share them all pair-wise (inefficient!) *)
             shareAll tcs
           end;
           
           val U : unit list = map shareVirtTypeConstr typeConstrNames;
           
           (* Share the substructures *)
           val subStrNames : string list = listVirtSubStructureNames virts;
           
           fun shareVirtSubstruct (strName : string) : unit =
             shareVirtStructs (listVirtSubStructures virts strName, lno);
           
         in
            map shareVirtSubstruct subStrNames;
            ()
         end;
         
        
         fun shareStructures (shareList : signatures list, lno : LEX.location) : unit =
           shareVirtStructs (map (fn strVal => [strVal]) shareList, lno);

        (* When looking up the structure and type names we look only
           in the signature in ML97.  We add this to make it clear that
           we are only looking up in the signature otherwise we get
           confusing messages such as "type (int) has not been declared". *)
         fun lookupFailure msg =
             giveError (str, line, lex) (msg ^ " in signature.")

        in
              if isType
              then let (* Type sharing. *)
                fun lookupSharing (name: string) = 
                  lookupTyp
                   ({ 
                      lookupType   = #lookupType   tEnv,
                      lookupStruct = #lookupStruct tEnv
                    },
                    name,
                    lookupFailure)
                      
                val first  = lookupSharing (hd tlist);
              in
                if not (isUndefinedTypeConstr first)
                then
                  List.app
                    (fn typ => shareTypes (lookupSharing typ, first, line))
                    (tl tlist)
                 else ()
              end

              else let (* structure sharing. *)
                fun getStructSignat (name: string) : signatures =
                let
                  val subStr : structVals =
                    lookupStructureAsSignature (#lookupStruct tEnv, name, lookupFailure);
                in
                      structSignat subStr
                end
              in  (* Now share all these signatures. *)
                shareStructures (map getStructSignat tlist, line)
              end
        end (* applySharingConstraint *);

(**************************** End of SPF's rewrite *************************)

        (* Look up a signature.  Signatures can only be in the global environment. *)
        fun lookSig (name : string, lno : LEX.location) : signatures =
            case #lookupSig globalEnv name of
                SOME v => v
            |   NONE =>
                    (
                        giveError (str, lno, lex)("Signature (" ^ name ^ ") has not been declared");
                        undefinedSignature
                    )

        (* Construct a signature.  All the type IDs within the signature are variables. *)
        fun sigValue (str : sigs, Env env : env, lno : LEX.location, structPath) =
        let
            (* Make a new signature. *)
            val (sigName, loc) =
                case str of
                    SignatureIdent nameLoc => nameLoc
                |    _ => ("", lno)
            val newTable = makeSignatureTable();
            (* Copy everything into the new signature. *)
            val structEnv = makeEnv newTable;

            (* ML 97 does not allow multiple declarations in a signature. *)
            fun checkAndEnter enter lookup kind (s: string, v) =
                case lookup s of
                   SOME _ => (* Already there. *)
                     errorNear (lex, true, fn n => displaySigs(str, n), lno, 
                         kind ^ " (" ^ s ^ ") is already present in this signature.")
                |  NONE => enter(s, v)

            val checkedStructEnv = 
             {
              lookupVal     = #lookupVal    structEnv,
              lookupType    = #lookupType   structEnv,
              lookupFix     = #lookupFix    structEnv,
              lookupStruct  = #lookupStruct structEnv,
              lookupSig     = #lookupSig    structEnv,
              lookupFunct   = #lookupFunct  structEnv,
              enterVal      =
                  checkAndEnter (#enterVal structEnv) (#lookupVal structEnv) "Value",
              enterType     =
                  checkAndEnter (#enterType structEnv) (#lookupType structEnv) "Type",
              enterStruct   =
                  checkAndEnter (#enterStruct structEnv) (#lookupStruct structEnv) "Structure",
              (* These next three can't occur. *)
              enterFix      =
                  checkAndEnter (#enterFix structEnv) (#lookupFix structEnv) "Fixity",
              enterSig      =
                  checkAndEnter (#enterSig structEnv) (#lookupSig structEnv) "Signature",
              enterFunct    =
                  checkAndEnter (#enterFunct structEnv) (#lookupFunct structEnv) "Functor"
             }
            (* Create the signature and return the next entry to use in the result vector. *)
            val nextOffset = makeSigInto(str, Env checkedStructEnv, Env env, lno, 0, structPath);
            (* Make a copy to freeze it as immutable.*)
            (* TODO: Check these.  Aren't these always zero? *)
            val resultSig = makeSignature(sigName, newTable, 0, 0, lno, fn _ => raise Subscript)
        in
            (resultSig, nextOffset)
        end

        (* Constructs a signature and inserts it into an environment at a given offset.
           Generally offset will be zero except if we are including a signature.
           All the type IDs corresponding to local types are variables.  There may be free
           IDs (and bound IDs?) as a result of "where type" constraints. *)
        and makeSigInto(str: sigs,
                        Env structEnv, (* The immediately enclosing sig. *)
                        Env globalEnv, (* The surrounding environment excluding this sig. *)
                        lno: LEX.location,
                        offset: int,
                        structPath): int =
          (* Either a named signature or sig ... end or one of
             these with possibly multiple where type realisations. *)
          case str of
            SignatureIdent nameLoc =>
            let
                (* Look up the signature and copy it to turn bound IDs into variables.
                   This is needed because we may have sharing. *)
                val sourceSig = lookSig nameLoc;

                (* Create a new variable ID for each bound ID.  We must only create
                   one for each and must return the same variable ID for each bound ID. *)
                fun makeNewId n =
                let
                    val oldId = sigTypeIdMap sourceSig n
                    val desc =
                        case oldId of
                            Bound { description, ...} => description
                        |   _ => raise InternalError "Map does not return Bound Id"
                in
                    makeVariableId(isEquality oldId, desc)
                end;
                
                val minOffset = sigMinTypes sourceSig and maxOffset = sigMaxTypes sourceSig
  
                val v = Vector.tabulate (maxOffset-minOffset, fn n => makeNewId(n+minOffset))
                fun typeMap id = Vector.sub (v, id - minOffset)
                fun copyId(id as Bound{ offset, ...}) = SOME(typeMap offset)
                |   copyId id = NONE

                (* Renumber the values and structures.  We don't need to do the types
                   because they will be renumbered at the end. *)
                val address = ref offset
                fun newAccess(Formal _) =
                    let val addr = !address in address := addr+1; Formal addr end
                |   newAccess _ = raise InternalError "newAccess: Not Formal"
  
                val tsvEnv =
                {
                    enterType   = #enterType structEnv,
                    enterStruct =
                        fn (name, str) =>
                            #enterStruct structEnv
                                (name, Struct{ name = structName str, signat = structSignat str,
                                        access = newAccess(structAccess str),
                                        locations = structLocations str}),
                    enterVal =
                        fn (dName, Value { name, typeOf, access, class, locations }) =>
                            #enterVal structEnv (dName,
                                Value{name=name, typeOf = typeOf, access=newAccess access,
                                      class=class, locations=locations})
                }
                (* Copy the signature into the result. *)
                val () = fullCopySig(sourceSig, tsvEnv, copyId, "")
            in
                ! address
            end
  
          | SigDec (sigsList : sigs list, _) =>  (* sig .... end *)
          let
            (* Process the entries in the signature and allocate an address
               to each. *)
            fun processSig (signat, offset : int, lno : LEX.location) : int =
              case signat of
                StructureSig (structList : structSigBind list, _) =>
                let
                  (* Each element in the list should be a structure binding. *)
                  fun pStruct [] offset = offset
                    | pStruct (({name, sigStruct = (sigStruct, _, _), line, ...}: structSigBind) :: t) offset =
                    let
                      (* Create a new surrounding environment to include the surrounding
                         structure.  This is the scope for any structures or types.
                         Specifically, if we look up a type defined by a "where type"
                         we use this environment and not the signature we're creating. *)
                      val newEnv = 
                         {
                          lookupVal     = #lookupVal    structEnv,
                          lookupType    =
                            lookupDefault (#lookupType structEnv) (#lookupType globalEnv),
                          lookupFix     = #lookupFix    structEnv,
                          lookupStruct  =
                            lookupDefault (#lookupStruct structEnv) (#lookupStruct globalEnv),
                          lookupSig     = #lookupSig    structEnv,
                          lookupFunct   = #lookupFunct  structEnv,
                          enterVal      = #enterVal structEnv,
                          enterType     = #enterType structEnv,
                          enterStruct   = #enterStruct structEnv,
                          enterFix      = #enterFix structEnv,
                          enterSig      = #enterSig structEnv,
                          enterFunct    = #enterFunct structEnv
                         };
                      val (resSig, _) = sigValue (sigStruct, Env newEnv, line, structPath ^ name ^ ".");
                      (* Process the rest of the list before declaring
                         the structure. *)
                      val result = pStruct t (offset + 1);
                      (* Make a structure. *)
                      val resStruct = makeFormalStruct (name, resSig, offset, [DeclaredAt lno]);
                      val U : unit = #enterStruct structEnv (name, resStruct);
                    in
                      result (* One slot for each structure. *)
                    end
                in
                  pStruct structList offset
                end
                
              | ValSig {name=(name, nameLoc), typeof, line, ...} =>
                let
                    val typeof = typeFromTypeParse typeof
                  val errorFn = giveError (signat, line, lex);
                
                  fun lookup(s, locn) =
                    lookupTyp
                      ({
                        lookupType   =
                            lookupDefault (#lookupType structEnv) (#lookupType globalEnv),
                        lookupStruct =
                            lookupDefault (#lookupStruct structEnv) (#lookupStruct globalEnv)
                       },
                     s,
                     giveError (signat, locn, lex));
                in  (* If the type is not found give an error. *)
                  (* Check for rebinding of built-ins.  "it" is allowed here. *)
                    if name = "true" orelse name = "false" orelse name = "nil"
                    orelse name = "::" orelse name = "ref"
                  then errorFn("Specifying \"" ^ name ^ "\" is illegal.")
                  else ();
                  assignTypes (typeof, lookup, lex);
                  (* The type is copied before being entered in the environment.
                     This isn't logically necessary but has the effect of removing
                     ref we put in for type constructions. *)
                  #enterVal structEnv (name,
                    mkFormal (name, SimpleValue,
                        copyType (typeof, fn x => x, fn x => x), offset, [DeclaredAt nameLoc]));
                  (offset + 1)
                end
               
              | ExSig {name=(name, nameLoc), typeof, line, ...} =>
                let
                  val errorFn = giveError (signat, line, lex);
                
                  fun lookup(s,locn) =
                    lookupTyp
                      ({
                        lookupType   =
                            lookupDefault (#lookupType structEnv) (#lookupType globalEnv),
                        lookupStruct =
                            lookupDefault (#lookupStruct structEnv) (#lookupStruct globalEnv)
                       },
                     s,
                     errorFn);

                  val exType =
                    case typeof of
                        NONE => exnType
                    |   SOME typeof =>
                        let
                            val ty = typeFromTypeParse typeof
                        in
                            assignTypes (ty, lookup, lex);
                            mkFunctionType (ty, exnType)
                        end
                in  (* If the type is not found give an error. *)
                  (* Check for rebinding of built-ins. "it" is not allowed. *)
                    if name = "true" orelse name = "false" orelse name = "nil"
                  orelse name = "::" orelse name = "ref" orelse name = "it"
                  then errorFn("Specifying \"" ^ name ^ "\" is illegal.")
                  else ();
                  #enterVal structEnv (name, mkFormal (name, Exception, exType, offset, [DeclaredAt nameLoc]));
                  (offset + 1)
                end
               
              | IncludeSig (structList : sigs list) =>
              let
                (* include sigid ... sigid or include sigexp.  For
                   simplicity we handle the slightly more general case
                   of a list of signature expressions.
                  The contents of the signature are added to the environment. *)
                fun includeSigExp (str: sigs, offset) =
                    makeSigInto(str, Env structEnv, Env globalEnv, lno, offset, structPath)
              in
                List.foldl includeSigExp offset structList
              end

              | Sharing (share : shareConstraint) =>
              (* Sharing constraint. *)
              let
                 (* In ML90 it was possible to share with any identifier
                    in scope.  In ML97 sharing is restricted to identifiers
                    in the "spec". *)
                   val envForSharing = Env structEnv
              in
                 applySharingConstraint (share, envForSharing, str);
                 offset (* No entry *)
              end
                
              | CoreType {dec, ...} =>
              let (* datatype or type binding(s) *)
                (* This pass puts the data constructors into the environment. *)
                val addrs = ref offset
                (* Pass2 creates value constructors of datatypes as global values.
                   Rather than complicate pass2 by trying to make formal values
                   in this case it's easier to trap the value constructors at
                   this point. N.B. We may get constructors from a datatype
                   declaration or from datatype replication. *)
                fun enterVal(name, Value{class=class, typeOf, locations, ...}) =
                    let
                        val addr = !addrs
                        val _ = addrs := addr+1
                    in
                        (#enterVal structEnv)(name,
                            Value{class=class, typeOf=typeOf, access=Formal addr, name=name, locations=locations})
                    end

                (* Record all the datatypes we declare. *)
                val datatypeList = ref []
                fun enterType(name, tyCons) =
                (
                    if null (tcConstructors tyCons)
                    then ()
                    else datatypeList := tyCons :: !datatypeList;
                    #enterType structEnv (name, tyCons)
                )

               val newEnv = 
                 {
                  lookupVal     = #lookupVal    structEnv,
                  lookupType    =
                    lookupDefault (#lookupType structEnv) (#lookupType globalEnv),
                  lookupFix     = #lookupFix    structEnv,
                  lookupStruct  =
                    lookupDefault (#lookupStruct structEnv) (#lookupStruct globalEnv),
                  lookupSig     = #lookupSig    structEnv,
                  lookupFunct   = #lookupFunct  structEnv,
                  enterVal      = enterVal,
                  enterType     = enterType,
                  enterStruct   = #enterStruct structEnv,
                  enterFix      = #enterFix structEnv,
                  enterSig      = #enterSig structEnv,
                  enterFunct    = #enterFunct structEnv
                 };

                fun makeId (eq, { location, name, description }) =
                    makeVariableId(eq,
                        { location = location, name = structPath ^ name, description = description })
                val t : types = pass2 (dec, makeId, Env newEnv, lex);
                (* Replace the constructor list for the datatype with the modified
                   constructors.  All the constructors should be in the set.  Is
                   it possible that one might not be because of an error? *)
                fun findConstr(v: values): values =
                    getOpt((#lookupVal structEnv)(valName v), v)
                fun updateConstrList tyCons =
                (
                    (* Update the entry in the array to indicate that this is a datatype.  Should
                       only be called immediately after the datatype has been created.
                       If we are replicating a datatype the ID will be the same as the original
                       datatype so we mustn't do this in that case. *)
                    case tcIdentifier tyCons of
                        id as Bound{ offset, description, ... } =>
                            if offset >= initTypeId
                            then StretchArray.update(mapArray, offset-initTypeId,
                                    VariableSlot{isDatatype=true, boundId = id, descriptions = [#name description]})
                            else ()
                    |   _ => ();
                    (* We need to record that this is a datatype for well-formedness checking.
                       i.e. we can't share it with a type-function. *)
                    tcSetConstructors(tyCons, List.map findConstr (tcConstructors tyCons))
                )
                val _ = List.app updateConstrList (!datatypeList)
              in
                ! addrs
              end
              
              | _ =>
                 raise InternalError "processSig: not a signature"
            (* end processSig *);
          in
              List.foldl
                (fn (signat, offset) => 
                   processSig (signat, offset, lno))
                offset sigsList
          end

          | WhereType { sigExp, typeVars, typeName, realisation, line } =>
            let
                (* We construct the signature into the result signature.  When we apply the
                   "where" we need to look up the types (and structures) only within the
                   signature constrained by the "where" and not in the surrounding signature.
                   e.g. If we have sig type t include S where type t = ... end
                   we need to generate an error if S does not include t.  Of course
                   if it does that's also an error since t would be rebound!
                   Equally, we must look up the right hand side of a where type
                   in the surrounding scope, which will consist of the global environment
                   and the signature excluding the entries we're adding here. *)
                val findTypes = searchList() and findStructs = searchList()
                val newEnv =
                {
                    lookupVal     = #lookupVal    structEnv,
                    lookupType    =
                        lookupDefault (#lookup findTypes)
                            (lookupDefault (#lookupType structEnv) (#lookupType globalEnv)),
                    lookupFix     = #lookupFix    structEnv,
                    lookupStruct  =
                        lookupDefault (#lookup findStructs)
                            (lookupDefault (#lookupStruct structEnv) (#lookupStruct globalEnv)),
                    lookupSig     = #lookupSig    structEnv,
                    lookupFunct   = #lookupFunct  structEnv,
                    enterVal      = #enterVal structEnv,
                    enterType     = #enter findTypes,
                    enterFix      = #enterFix structEnv,
                    enterStruct   = #enter findStructs,
                    enterSig      = #enterSig structEnv,
                    enterFunct    = #enterFunct structEnv
                }

               val resAddr = makeSigInto(sigExp, Env newEnv, Env globalEnv, lno, offset, structPath)

              fun lookupFailure msg =
                 giveError (str, line, lex) (msg ^ " in signature.")

              (* Look up the type constructor in the signature. *)
              val typeConstr =
                    lookupTyp
                      ({
                        lookupType   = #lookup findTypes,
                        lookupStruct = #lookup findStructs
                       },
                     typeName,
                     lookupFailure);
              (* The type, though, is looked up in the surrounding environment. *)
              fun lookupGlobal(s, locn) =
                    lookupTyp
                      ({
                        lookupType   =
                            lookupDefault (#lookupType structEnv) (#lookupType globalEnv),
                        lookupStruct =
                            lookupDefault (#lookupStruct structEnv) (#lookupStruct globalEnv)
                       },
                     s,
                     giveError (str, locn, lex))

                (* Process the type, looking up any type constructors. *)
                val () = assignTypes (realisation, lookupGlobal, lex);
                val cantSet = giveError (str, line, lex)
         in
                (* Now try to set the target type to the type function. *)
                if isUndefinedTypeConstr typeConstr
                then () (* Probably because looking up the type constructor name failed. *)
                else if not (isVariableId(tcIdentifier typeConstr))
                then (* May have been declared as type t=int or bound by a where type already.
                        TODO: Display the type it's bound to. *)
                    cantSet("Cannot apply type realisation: (" ^
                                tcName typeConstr ^ ") has already been set.")
                else
                case tcIdentifier typeConstr of
                    Bound { offset, ... } =>
                    (
                        case realId(offset-initTypeId) of
                            VariableSlot { isDatatype, boundId=varId as Bound{eqType, offset, ...}, ... } =>
                            (
                               (* The rule for "where type" says that we must check that an eqtype
                                  is only set to a type that permits equality and that the result
                                  is "well-formed".  This seems to mean that if the type we're
                                  setting is a datatype (has constructors) it can only be set to
                                  a type that is a type name and not a general type function. *)
                                if pling eqType andalso not(typePermitsEquality realisation)
                                then cantSet ("Cannot apply type realisation: (" ^ tcName typeConstr ^
                                      ") is an eqtype but the type does not permit equality.")
                                else case typeNameRebinding (typeVars, realisation) of
                                    SOME typeId =>
                                        (* Renaming an existing constructor e.g. type t = s.  Propagate the id.
                                           "s" may be free or it may be within the signature and equivalent to
                                           a sharing constraint.
                                           e.g. sig type t structure S: sig type s end where type s = t end. *)
                                        if isVariableId typeId
                                        then linkFlexibleTypeIds(typeId, varId)
                                        else StretchArray.update(mapArray, offset-initTypeId, FreeSlot typeId)
                                |   NONE =>
                                        if isDatatype
                                            (* The type we're trying to set is a datatype but the type
                                               we're setting it to isn't. *)
                                        then cantSet ("Cannot apply type realisation: (" ^ tcName typeConstr ^
                                            ") is a datatype but the type is not a simple type.")
                                        else
                                            StretchArray.update(mapArray, offset-initTypeId,
                                                FreeSlot(TypeFunction(typeVars, realisation)))
                            )
                        |   _ => (* Already checked. *) raise InternalError "setWhereType"
                    )
                |   _ => (* Already checked. *) raise InternalError "setWhereType";
              (* Finally we can safely add the new declarations to the surrounding scope. *)
              #apply findTypes (#enterType structEnv);
              #apply findStructs (#enterStruct structEnv);
              resAddr
          end

          | _ =>
            raise InternalError "makeSigInto: not a SigIdent nor a SigDec"; (* end makeSigInto *)
      in
        case str of 
            SignatureIdent nameLoc =>
                (* We can speed things up because the stamps are already bound. Also in this
                   case if this is being used as the result signature of a functor we can't
                   have sharing with the arguments so we don't have to renumber any bound IDs. *)
                lookSig nameLoc
        
        | _ =>
            let
                (* Anything else has to be copied.  We first build the signature with variable
                   type IDs so that any local types can be shared. *)
                val (resultSig, nextAddress) = sigValue (str, Env globalEnv, lno, "");
        
                (* After the signature has been built and any sharing or "where type"
                   constraints have been applied we replace the remaining variable stamps
                   by bound stamps. We may not start at zero
                   if this is the result signature of a functor because there
                   may be sharing between the argument and the result. *) 
                val typeCounter = ref initTypeId;
                val addrCounter = ref nextAddress
    
 
                (* Construct final bound IDs for each distinct type ID in the array. *)
                local
                    fun mapIds n =
                    if n = !idCount-initTypeId
                    then ([], [])
                    else
                    (
                        (* Process lowest numbered IDs first since they represent
                           the result of any sharing. *)
                        case realId n of
                            VariableSlot {
                                boundId = Bound{eqType, description = { name, location, description}, ... },
                                descriptions, ...} =>
                            let (* Need to make a new ID. *)
                                (* If we have sharing we want to produce a description that expresses that. *)
                                val descript =
                                    case descriptions of
                                        descs as _ :: _ :: _ => "sharing " ^ String.concatWith "," descs
                                    |   _ => description (* Original description. *)
                                val newId =
                                let
                                    (* For each ID we need a new entry in the ID vector.  We also
                                       need an entry in the run-time vector for the structure so that
                                       we can pass the equality/print value at run-time. *)
                                    val n = !typeCounter
                                    val () = typeCounter := n + 1
                                    val addr = ! addrCounter
                                    val () = addrCounter := addr + 1
                                    val description =
                                        { name = name, location = location, description = descript }
                                in
                                    makeBoundId(Formal addr, n, pling eqType, description)
                                end
                                (* Update the entry for any sharing. *)
                                val () = StretchArray.update(mapArray, n, FreeSlot newId)
                                val (distinctIds, mappedIds) = mapIds (n+1)
                            in
                                (newId :: distinctIds, newId :: mappedIds)
                            end
                        |   FreeSlot id => (* Free or shares with existing type ID. *)
                            let
                                val (distinctIds, mappedIds) = mapIds (n+1)
                            in
                                (distinctIds, id :: mappedIds)
                            end
                        |   _ => raise InternalError "mapIds"
                    )
                    val (distinctIds, mappedIds) = mapIds 0
                    val mapVector = Vector.fromList mappedIds
                    and resVector = Vector.fromList distinctIds
                in
                    fun mapFunction n = Vector.sub(mapVector, n-initTypeId)
                    fun typeIDMap n =
                        if n < initTypeId
                        then outerTypeIdEnv n
                        else Vector.sub(resVector, n-initTypeId)
                end
                (* Copy the signature containing the full range of unshared IDs to replace
                   them with the shared IDs. *)
                val resSig =
                    copySig (makeSignature(sigName resultSig, sigTab resultSig, initTypeId,
                                          ! idCount, sigDeclaredAt resultSig, mapFunction),
                             fn n => n >= initTypeId, mapFunction)
            in
                (* Set the size of the type table for the signature we return. *)
                makeSignature (sigName resSig, sigTab resSig, initTypeId, !typeCounter,
                               sigDeclaredAt resSig, typeIDMap)
            end (* not (isSignatureIdent str) *)
    end (* sigVal *);

    fun functorArgSigval(argSig, initTypeId, outerTypeIdEnv, globalEnv, lex, line) =
    (* If it is a "spec" it must be wrapped up in sig...end. *)
    let
	  	val spec =
			case argSig of
				SignatureIdent _ => argSig
			|	SigDec _ => argSig
			|	WhereType _ => argSig
			|	_ => mkSig([argSig], line)
    in
        sigVal (spec, initTypeId, outerTypeIdEnv, globalEnv, lex, line)
    end


    structure Sharing =
    struct
        type sigs           = sigs
        type structSigBind  = structSigBind
        type types          = types
        type parsetree      = parsetree
        type typeParsetree  = typeParsetree
        type typeVarForm    = typeVarForm
        type pretty         = pretty
        type ptProperties   = ptProperties
        type env            = env
        type signatures     = signatures
        type lexan          = lexan
        type typeId         = typeId
    end

end;

