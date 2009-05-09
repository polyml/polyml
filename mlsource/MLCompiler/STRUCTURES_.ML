(*
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

functor STRUCTURES_ (

(*****************************************************************************)
(*                  LEX                                                      *)
(*****************************************************************************)
structure LEX : LEXSIG

(*****************************************************************************)
(*                  CODETREE                                                 *)
(*****************************************************************************)
structure CODETREE :
sig
  type machineWord;
  type codetree;
  
  val CodeNil:      codetree;
  val CodeZero:     codetree;
  val mkLoad:       int * int -> codetree;
  val mkConst:      machineWord -> codetree;
  val mkDec:        int * codetree  -> codetree;
  val mkInd:        int * codetree  -> codetree;
  val mkProc:       codetree * int * int * string -> codetree;
  val mkMacroProc:  codetree * int * int * string -> codetree;
  val mkStr:        string   -> codetree;
  val mkRaise:      codetree -> codetree;
  val mkEval:       codetree * codetree list * bool -> codetree;
  val mkTuple:      codetree list -> codetree;
  val mkEnv:        codetree list -> codetree;
  val multipleUses: codetree * (unit -> int) * int -> {load: int -> codetree, dec: codetree list};
end (* CODETREE *);


(*****************************************************************************)
(*                  STRUCTVALS                                               *)
(*****************************************************************************)
structure STRUCTVALS : STRUCTVALSIG;

(*****************************************************************************)
(*                  VALUEOPS                                                 *)
(*****************************************************************************)
structure VALUEOPS : VALUEOPSSIG;

(*****************************************************************************)
(*                  EXPORTTREE                                               *)
(*****************************************************************************)
structure EXPORTTREE: EXPORTTREESIG

(*****************************************************************************)
(*                  TYPETREE                                                 *)
(*****************************************************************************)
structure TYPETREE : TYPETREESIG

(*****************************************************************************)
(*                  PARSETREE                                                *)
(*****************************************************************************)
structure PARSETREE : PARSETREESIG

(*****************************************************************************)
(*                  UTILITIES                                                *)
(*****************************************************************************)
structure UTILITIES :
sig
  val noDuplicates: (string * 'a * 'a -> unit) -> 
         { apply: (string * 'a -> unit) -> unit,
           enter:  string * 'a -> unit,
           lookup: string -> 'a option };

  val searchList: unit -> { apply: (string * 'a -> unit) -> unit,
                            enter:  string * 'a -> unit,
                            lookup: string -> 'a option };
  val splitString: string -> { first:string,second:string }
end;

(*****************************************************************************)
(*                  UNIVERSALTABLE                                           *)
(*****************************************************************************)
structure UNIVERSALTABLE:
sig
  type universal = Universal.universal
  type univTable
  type 'a tag = 'a Universal.tag
  
  val univEnter:  univTable * 'a tag * string * 'a -> unit;
  val univLookup: univTable * 'a tag * string -> 'a option;
  val univFold:   univTable * (string * universal * 'a -> 'a) * 'a -> 'a;
end;

(*****************************************************************************)
(*                  DEBUG                                                    *)
(*****************************************************************************)
structure DEBUG :
sig
    val inlineFunctorsTag: bool Universal.tag
    val errorDepthTag : int Universal.tag
    val getParameter :
           'a Universal.tag -> Universal.universal list -> 'a 
end;

(*****************************************************************************)
(*                  PRETTY                                                   *)
(*****************************************************************************)
structure PRETTY : PRETTYSIG

structure COPIER: COPIERSIG

structure TYPEIDCODE: TYPEIDCODESIG

sharing LEX.Sharing = VALUEOPS.Sharing = TYPETREE.Sharing = PARSETREE.Sharing
    = PRETTY.Sharing = EXPORTTREE.Sharing = STRUCTVALS.Sharing = COPIER.Sharing
    = CODETREE = UNIVERSALTABLE = TYPEIDCODE.Sharing

) : STRUCTURESSIG =

(*****************************************************************************)
(*                  STRUCTURES functor body                                  *)
(*****************************************************************************)
struct
  open Misc; 
  open PRETTY;

  open COPIER;
  open LEX;
  open CODETREE;
  open STRUCTVALS;
  open VALUEOPS;
  open TYPETREE;
  open PARSETREE;
  open UTILITIES;
  open DEBUG;
  open UNIVERSALTABLE;
  open Universal; (* for tag record selectors *)
  open EXPORTTREE;
  open TYPEIDCODE

  val displayType = TYPETREE.display;

  (* Union of the various kinds of core language declaration.  Structures are included
     because they can be declared by opening a structure with substructures. *)
  datatype coreDeclaration =
  	CoreValue       of values
  | CoreType        of typeConstrs
  | CoreFix         of string*fixStatus (* Include the name because it isn't part of fixStatus. *)
  | CoreStruct      of structVals

  (* "structs" is the abstract syntax for the module language. *)
  datatype structs =
    StructureDec   of structBind list * location       (* List of structure decs *)

  | StructureSig   of structSigBind list * location

  | StructureIdent of structureIdentForm (* A structure name *)

  | StructDec      of (* struct ... end *)
      {
        alist: structs list, (* List of items in it. *)
        location: location,
        value: univTable (* Value *),
        localIds: { source: typeId, dest: typeId } list ref,
        resultSig: signatures ref
      }

  | SignatureIdent of string * location  (* A signature name *)

  | SigDec         of structs list *location (* sig ... end *)

  | ValSig         of (* Signature of a value. *)
        { name: string * location, typeof: typeParsetree, line: location }

  | ExSig          of (* Signature of an exception.  May be a nullary exception. *)
        { name: string * location, typeof: typeParsetree option, line: location }

  | FunctorAppl    of functorApplForm    (* Appln of a functor *)

  | CoreLang      of (* Any other decln. *)
    {
        dec:   parsetree,           (* The value *)
        vars:  coreDeclaration list ref, (* The declarations *)
        location: location
    }

  | Sharing        of shareConstraint    (* Sharing constraints. *)
  | WhereType	   of whereTypeStruct    (* type realisation. *)
  | Localdec       of localdecStruct     (* Local/Let. *)
  | IncludeSig     of structs list       (* Include. *)
  | SigConstraint  of                    (* Constraint of str to match sig. *)
       {
          str: structs,  (* Structure to constain *)
		  csig: structs, (* Constraining signature *)
          opaque: bool,   (* True if opaque, false if transparent. *)
          sigLoc: location,
          opaqueIds: { source : typeId, dest: typeId } list ref,
          resultSig: signatures ref
	   }
  | EmptyStruct                          (* Error cases. *)

  withtype structBind =
      {
        name:      string,         (* The name of the structure *)
        nameLoc:   location,
        haveSig:   bool, (* Whether we moved an explicit signature to the value. *)
        value:     structs,        (* And its value *)
        valRef:    structVals ref, (* The structure variable declared. *)
        opaqueIds: { source: typeId, dest: typeId } list ref, (* Extra type IDs needed for opaque matching. *)
        line:      location
      }
  
  and structSigBind =
      {
        name:      string,         (* The name of the structure *)
        nameLoc:   location,
        sigStruct: structs * bool * location,
        line:      location
      }

  (* A reference to a name *)
  and structureIdentForm =
      {
        name:   string,         (* The name *)
        valRef: structVals ref, (* The variable found. *)
        location: location
      } 
  
  (* Application of a functor. *)
  and functorApplForm =
      {
        name:   string,
        arg:    structs,
        valRef: functors ref,      (* The functor looked up. *)
        nameLoc: location,      (* The location of the name itself. *)
        fullLoc: location,      (* The location of the full application. *)
        argHolder: structVals ref, (* A dummy structure that holds the actual argument *)
        argIds:  { source: typeId, dest: typeId } list ref, (* The IDs that are required in the arguments. *)
        resIds:  { source: typeId, dest: typeId } list ref, (* Generative IDs in the result. *)
        resultSig: signatures ref
      }

  and shareConstraint =
      {
        isType: bool,
        shares: string list,
        line:   location
      } 

  and whereTypeStruct =
      {
        sigExp: structs,
		typeVars: typeVarForm list,
        typeName: string,
        realisation: types,
		line: location
      }


  (* Used for local strdec in strdec and let strdec in strexp. *)
  and localdecStruct =
      {
        decs:     structs list,
        body:     structs list,
        localDec: bool,
        line:     location
      }
   
    fun isSignatureIdent (SignatureIdent x) = true | isSignatureIdent _ = false;
    fun isEmptyStruct     EmptyStruct       = true | isEmptyStruct    _ = false;
    
    val noLocation =
        { file="", startLine=0, startPosition=0, endLine=0, endPosition=0 }
    (* Make a signature for initialisating variables and for
       undeclared signature variables. *)
    val undefinedSignature =
       makeSignature("UNDEFINED", makeSignatureTable(),
                0, 0, noLocation, fn _ => raise Subscript);
  
    (* Construction functions called by the parser. *)
    val emptyStruct    = EmptyStruct; (* added SPF 8/2/94 *)
  
    fun mkStructIdent (name, location) =
      StructureIdent
        {
          name   = name,
          valRef = ref undefinedStruct,
          location = location
        };
  
  
    (* For struct...end, make a signature to accept the values. *)
    fun mkStruct(alist, location) =
      StructDec
        {
          alist = alist,
          location = location,
          value = makeSignatureTable (),
          localIds = ref nil,
          resultSig = ref undefinedSignature
        };
  
    val mkSigIdent = SignatureIdent;
  
    val mkSig = SigDec;
  
    fun mkCoreLang (dec, location) =
        CoreLang
        {
            dec   = dec,
            vars  = ref [],
            location = location
        };

    fun mkFunctorAppl (name, arg, nameLoc, fullLoc) =
      FunctorAppl
        {
          name   = name,
          arg    = arg,
          valRef = ref undefinedFunctor,
          nameLoc = nameLoc,
          fullLoc = fullLoc,
          argHolder = ref undefinedStruct,
          argIds  = ref nil,
          resIds  = ref nil,
          resultSig = ref undefinedSignature
        };
  
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
  
    fun mkFormalArg (name, signat) =
        {
          name      = name,
          sigStruct = signat,
          valRef    = ref undefinedStruct
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
  

    fun mkLocaldec (decs, body, localDec, line) =
      Localdec 
         {
           decs     = decs,
           body     = body,
           localDec = localDec,
           line     = line
         };

	val mkInclude = IncludeSig;

	fun mkSigConstraint(str, csig, opaque, sigLoc) =
        SigConstraint
        {
            str=str, csig=csig, opaque=opaque, sigLoc=sigLoc,
            opaqueIds=ref nil, resultSig = ref undefinedSignature
        }

    val mkStructureDec = StructureDec
    and mkStructureSig = StructureSig

    fun mkStructureBinding ((name, nameLoc), signat, value, fullLoc) =
    let
        (* If there's an explicit signature move that to a constraint. *)
        val value =
            case signat of
                NONE => value
            |   SOME (csig, opaque, sigLoc) =>
                    mkSigConstraint(value, csig, opaque, sigLoc)
    in
        { 
            name      = name,
            nameLoc   = nameLoc,
            haveSig   = isSome signat,
            value     = value,
            valRef    = ref undefinedStruct,
            opaqueIds = ref [],
            line      = fullLoc
        }
    end;

   fun mkStructureSigBinding ((name, nameLoc), signat, fullLoc):structSigBind  =
        {
            name      = name,
            nameLoc   = nameLoc,
            sigStruct = signat,
            line      = fullLoc
        }


    type formalArgStruct =
      {
        name:      string,
        sigStruct: structs,
        valRef:    structVals ref
      } (* The structure variable. *) 

    (* Top level declarations and program. *)
    datatype topdec =
        StrDec          of structs * typeId list ref (* Structure decs and core lang. *)
    |   FunctorDec      of functorBind list * location      (* List of functor decs. *)
    |   SignatureDec    of sigBind list * location  (* List of signature decs *)

    withtype   (* Functor binding. *)
        functorBind =
        {
            name:      string,
            nameLoc:   location,
            haveSig:   bool, (* Whether we moved an explicit signature to the value. *)
            body:      structs,
            arg:       formalArgStruct,
            valRef:    functors ref,    (* The functor variable declared. *)
            resIds:    { source: typeId, dest: typeId } list ref,
            line:      location
        } 

    and sigBind =
        {
            name:      string, (* The name of the signature *)
            nameLoc:   location,
            sigStruct: structs,(* Its value *)
    		sigRef:    signatures ref, (* The "value" of the signature. *)
            line:      location
        }

    fun mkTopDec t = StrDec(t, ref nil)
    and mkFunctorDec s = FunctorDec s
    and mkSignatureDec s = SignatureDec s;
  
    fun mkFunctorBinding (name, nameLoc, signat, body, arg, line) =
    let
        (* If there's an explicit signature move that to a constraint. *)
        val body =
            case signat of
                NONE => body
            |   SOME (csig, opaque, sigLoc) =>
                    mkSigConstraint(body, csig, opaque, sigLoc)
    in
        {
          name      = name,
          nameLoc   = nameLoc,
          haveSig   = isSome signat,
          body      = body,
          arg       = arg,
          valRef    = ref undefinedFunctor,
          resIds    = ref nil,
          line      = line
        }
    end

    and mkSignatureBinding ((name, nameLoc), sg, ln) =
        { 
          name     = name,
          nameLoc  = nameLoc,
          sigStruct = sg,
          line      = ln,
		  sigRef   = ref undefinedSignature
        }

    type program = topdec list * location
    fun mkProgram tl = tl

  (* Pretty printing *)

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

    fun displayStruct (str, depth) =
        if depth <= 0 (* elide further text. *)
        then PrettyString "..."

        else
        case str of
            StructureDec (structList : structBind list, _) =>
		    let
			    fun displayStructBind (
					    {name, haveSig, value, ...}: structBind, depth) =
                let
                    (* If we desugared this before, return it to its original form. *)
                    val (sigStruct, value) =
                        case (haveSig, value) of
                            (true, SigConstraint{str, csig, opaque, sigLoc, ...}) =>
                                (SOME(csig, opaque, sigLoc), str)
                        |   _ => (NONE, value)
                in
		            PrettyBlock (3, false, [],
		                PrettyString name ::
                        (
                            case sigStruct of (* Signature is optional *)
                                NONE => []
                            |   SOME (sigStruct, opaque, _) =>
                                [
    		                        PrettyString (if opaque then " :>" else " :"),
    		                        PrettyBreak (1, 0),
    		                        displayStruct (sigStruct, depth - 1)
                                ]
                        ) @
                            [
		                        PrettyString " =",
		                        PrettyBreak (1, 0),
		                        displayStruct (value, depth - 1)
                            ]
		            )
                end
    		in
                PrettyBlock (3, false, [],
                    PrettyString "structure" ::
                    PrettyBreak (1, 0) ::
                    displayList (structList, "and", depth) displayStructBind
                )
            end

        | StructureSig (structList : structSigBind list, _) =>
		    let
			    fun displayStructBind (
					    {name, sigStruct=(sigStruct, opaque, _), ...}: structSigBind, depth) =
		            PrettyBlock (3, false, [],
                        [
		                    PrettyString name,
	                        PrettyString (if opaque then " :>" else " :"),
	                        PrettyBreak (1, 0),
	                        displayStruct (sigStruct, depth - 1)
                        ]
		            )
		in
            PrettyBlock (3, false, [],
                PrettyString "structure" ::
                PrettyBreak (1, 0) ::
                displayList (structList, "and", depth) displayStructBind
            )
        end

      | StructureIdent {name, ...} =>
            PrettyString name

      | StructDec {alist, ...} =>
            PrettyBlock (1, true, [],
                PrettyString "struct" ::
                PrettyBreak (1, 0) ::
                displayList (alist, "", depth) displayStruct @
                [ PrettyBreak (1, 0), PrettyString "end"]
            )

      | SignatureIdent (name : string, _) =>
            PrettyString name

      | SigDec (structList : structs list, _) =>
            PrettyBlock (1, true, [],
                PrettyString "sig" ::
                PrettyBreak (1, 0) ::
                displayList (structList, "", depth) displayStruct @
                [ PrettyBreak (1, 0), PrettyString "end"]
            )

      | ValSig {name = (name, _), typeof, ...} =>
            PrettyBlock (0, false, [],
                [
                    PrettyString "val",
                    PrettyBreak (1, 1),
                    PrettyString (name ^ " :"),
                    PrettyBreak (1, 0),
                    displayType (typeFromTypeParse typeof, depth - 1, NONE)
                ]
            )

      | ExSig {name = (name, _), typeof = NONE, ...} =>
            PrettyBlock (0, false, [],
                [
                    PrettyString "exception",
                    PrettyBreak (1, 1),
                    PrettyString (name)
                ]
            )
 
      | ExSig {name = (name, _), typeof = SOME typeof, ...} =>
            PrettyBlock (0, false, [],
                [
                    PrettyString "exception",
                    PrettyBreak (1, 1),
                    PrettyString (name ^ " :"),
                    PrettyBreak (1, 0),
                    displayType (typeFromTypeParse typeof, depth - 1, NONE)
                ]
            )

      | FunctorAppl {name, arg, ...} =>
            PrettyBlock (1, false, [],
                [
                    PrettyString (name ^ "("),
                    PrettyBreak (0, 0),
                    displayStruct (arg, depth),
                    PrettyBreak (0, 0),
                    PrettyString ")"
                ]
            )

      | Sharing { isType, shares, ... } =>
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

      | WhereType { sigExp, typeVars, typeName, realisation, ... } =>
			PrettyBlock (3, false, [],
                displayStruct (sigExp, depth) ::
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
                    displayType (realisation, depth - 1, NONE)
                ]
            )

      | Localdec {decs, body, localDec, ...} =>
            PrettyBlock (3, false, [],
                PrettyString (if localDec then "local" else "let") ::
                PrettyBreak (1, 0) ::
                displayList (decs, ";", depth - 1) displayStruct @
                [ PrettyBreak (1, 0), PrettyString "in", PrettyBreak (1, 0)] @
                displayList (body, ";", depth - 1) displayStruct @
                [ PrettyBreak (1, 0), PrettyString "end" ]
            )

      | IncludeSig (structList : structs list) =>
            PrettyBlock (3, true, [],
                PrettyString "include" ::
                PrettyBreak (1, 0) ::
                displayList (structList, "", depth - 1) displayStruct
            )

      | CoreLang {dec, ...} =>
          ptDisplay (dec, depth - 1)

      | SigConstraint{str, csig, opaque, ...} =>
            PrettyBlock (0, false, [],
                [
                    displayStruct (str, depth - 1),
                    PrettyString (if opaque then " :>" else " :"),
                    PrettyBreak (1, 0),
                    displayStruct (csig, depth - 1)
                ]
            )

      | EmptyStruct =>
          PrettyString "<bad>"
      (* End displayStruct *)
    
    fun displayTopDec(top, depth) =
        if depth <= 0 (* elide further text. *)
        then PrettyString "..."

        else
        case top of
            StrDec(s, _) => displayStruct(s, depth)

        |   SignatureDec (structList : sigBind list, _) =>
            let
    			fun displaySigBind ({name, sigStruct, ...}: sigBind, depth) =
    		        PrettyBlock (3, false, [],
                        [
    		                PrettyString (name ^ " ="),
    		                PrettyBreak (1, 0),
    		                displayStruct (sigStruct, depth - 1)
                        ]
                    )
    		in 
                PrettyBlock (3, false, [],
                    PrettyString "signature" ::
                    PrettyBreak (1, 0) ::
                    displayList (structList, "and", depth) displaySigBind
                )
            end

        |   FunctorDec (structList : functorBind list, _) =>
            let
    			fun displayFunctBind (
    					{name, arg={name=argName, sigStruct=argStruct, ...}, haveSig, body, ...}, depth) =
                let
                    val (sigStruct, body) =
                        case (haveSig, body) of
                            (true, SigConstraint{str, csig, opaque, sigLoc, ...}) =>
                                (SOME(csig, opaque, sigLoc), str)
                        |   _ => (NONE, body)
                in
    		        PrettyBlock (3, false, [],
                        PrettyString (name ^ "(") ::
    		            PrettyBreak (1, 0) ::
    		            PrettyBlock (1, false, [],
                            (
                                if argName = "" then []
                                else [ PrettyString (argName ^ " :"), PrettyBreak (1, 2)]
                            ) @
                            [displayStruct (argStruct, depth - 1)]
                        ) ::
    		            PrettyString ")" ::
                        (
    		                case sigStruct of
                                NONE  => [] (* Signature is optional *)
                            |   SOME (sigStruct, opaque, _) =>
                                [
                                    PrettyString(if opaque then " :>" else " :"),
        		                    PrettyBreak (1, 0),
        		                    displayStruct (sigStruct, depth - 1)
                                ]
    		            ) @
    		            [
                            PrettyBreak (1, 0),
    		                PrettyString "=",
    		                PrettyBreak (1, 0),
    		                displayStruct (body, depth - 1)
                        ]
                    )
                end
    		in 
                PrettyBlock (3, false, [],
                    PrettyString "functor" ::
                    PrettyBreak (1, 0) ::
                    displayList (structList, "and", depth) displayFunctBind
                )
            end
        (* End displayTopDec *)
 
    fun displayProgram ((sl, _), d) =
        PrettyBlock(0, true, [],
            displayList (sl, "", d) displayTopDec
        )
    
    
    fun structExportTree(navigation, s: structs) =
    let
         (* Common properties for navigation and printing. *)
        val commonProps =
            PTprint(fn d => displayStruct(s, d)) ::
            exportNavigationProps navigation

        fun asParent () = structExportTree(navigation, s)
    in
        case s of
            StructureDec(sbl, location) =>
            let
                fun exportSB(navigation, sb as {name, nameLoc, haveSig, value, line, ...}) =
                    let
                        (* If we desugared this before, return it to its original form. *)
                        val (sigStruct, value) =
                            case (haveSig, value) of
                                (true, SigConstraint{str, csig, opaque, sigLoc, ...}) =>
                                    (SOME(csig, opaque, sigLoc), str)
                            |   _ => (NONE, value)
                        fun exportThis () = exportSB(navigation, sb)
                        (* Three groups: name, signature and structures.
                           It's all complicated because the signature
                           may not be present. *)
                        fun getName () =
                        let
                            val next =
                                case sigStruct of
                                    SOME _ => getSigStruct
                                |   NONE => getValue
                        in
                            getStringAsTree({parent=SOME exportThis, previous=NONE, next=SOME next}, name, nameLoc, [])
                        end
                        
                        and getSigStruct () =
                        let
                            val next = SOME getValue
                            val (theSig, _, _) = valOf sigStruct
                        in
                            structExportTree({parent=SOME exportThis, previous=SOME getName, next=next}, theSig)
                        end

                        and getValue () =
                        let
                            val previous =
                                case sigStruct of
                                    NONE => getName
                                |   SOME _ => getSigStruct
                        in
                            structExportTree({parent=SOME exportThis, previous=SOME previous, next=NONE}, value)
                        end
                    in
                        (line, PTfirstChild getName :: exportNavigationProps navigation)
                    end

                val expChild = exportList(exportSB, SOME asParent) sbl
            in
                (location, expChild @ commonProps)
            end

        |   StructureSig(sbl, location) =>
            let
                fun exportSB(navigation, sb as {name, nameLoc, sigStruct=(theSig, _, _), line, ...}) =
                    let
                        fun exportThis () = exportSB(navigation, sb)
                        fun getName () =
                            getStringAsTree({parent=SOME exportThis, previous=NONE, next=SOME getSigStruct}, name, nameLoc, [])
                        
                        and getSigStruct () =
                            structExportTree({parent=SOME exportThis, previous=SOME getName, next=NONE}, theSig)
                    in
                        (line, PTfirstChild getName :: exportNavigationProps navigation)
                    end

                val expChild = exportList(exportSB, SOME asParent) sbl
            in
                (location, expChild @ commonProps)
            end

        |   StructureIdent _ => (nullLocation, commonProps)

        |   StructDec{ location, alist, ...} =>
                (location, exportList(structExportTree, SOME asParent) alist @ commonProps)

        |   SignatureIdent _ => (nullLocation, commonProps)

        |   SigDec(structList, location) =>
                (location, exportList(structExportTree, SOME asParent) structList @ commonProps)

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

        |   FunctorAppl _ => (nullLocation, commonProps)

        |   CoreLang {dec, ...} => (* A value parse-tree entry. *)
                getExportTree(navigation, dec)

        |   Sharing _ => (nullLocation, commonProps)

        |   WhereType _ => (nullLocation, commonProps)

        |   Localdec {decs, body, line, ...} =>
                (line, exportList(structExportTree, SOME asParent) (decs @ body) @ commonProps)

        |   IncludeSig _ => (nullLocation, commonProps)

        |   SigConstraint _ => (nullLocation, commonProps)

        |   EmptyStruct => (nullLocation, commonProps)
 
    end
    
    fun topDecExportTree(navigation, top: topdec) =
    let
         (* Common properties for navigation and printing. *)
        val commonProps =
            PTprint(fn d => displayTopDec(top, d)) ::
            exportNavigationProps navigation

        fun asParent () = topDecExportTree(navigation, top)
    in
        case top of
            StrDec(s, _) => structExportTree(navigation, s)

        |   SignatureDec(sigs, location) =>
            let
                fun exportSB(navigation, sb as {name, nameLoc, sigStruct, line, ...}) =
                    let
                        fun exportThis () = exportSB(navigation, sb)
                        fun getName () =
                            getStringAsTree({parent=SOME exportThis, previous=NONE, next=SOME getSig}, name, nameLoc, [])
                        and getSig () =
                            structExportTree({parent=SOME exportThis, previous=SOME getName, next=NONE}, sigStruct)
                    in
                        (line, PTfirstChild getName :: exportNavigationProps navigation)
                    end
            in
                (location, exportList(exportSB, SOME asParent) sigs @ commonProps)
            end

        |   FunctorDec(fbl, location) =>
            let
                fun exportFB(navigation, fb as {name, haveSig, arg={sigStruct=argStruct, ...}, body, line, ...}) =
                    let
                        val (sigStruct, body) =
                            case (haveSig, body) of
                                (true, SigConstraint{str, csig, opaque, sigLoc, ...}) =>
                                    (SOME(csig, opaque, sigLoc), str)
                            |   _ => (NONE, body)
                        val sbProps = exportNavigationProps navigation
                        fun exportThis () = exportFB(navigation, fb)
                        (* TODO: Visit the functor name here? *)
                        val sigEntry: structs list =
                            case sigStruct of
                                NONE => []
                            |   SOME (sigStruct, _, _) => [sigStruct]
                        val asChild = exportList(structExportTree, SOME exportThis) (argStruct :: (sigEntry @ [body]))
                    in
                        (line, asChild @ sbProps)
                    end

                val expChild = exportList(exportFB, SOME asParent) fbl
            in
                (location, expChild @ commonProps)
            end
    end

    (* Convert a "program" into a navigable tree. *)
    fun structsExportTree (parentOpt, trees: program) =
    let
        (* If a parent tree has been provided include it here. *)
        val parentTree =
            case parentOpt of
                NONE => []
            |   SOME getParent => [PTparent getParent]

        (* The top level is actually a list. *)
        fun exportTree(([], location)) = (location, parentTree)
        |   exportTree(topdec as (sl, location)) =
        let
            fun getEntry(this as (s :: sl), getPrevious) (): exportTree =
                topDecExportTree(
                    {
                        parent = SOME(fn () => exportTree topdec), (* Parent is this. *)
                        previous = getPrevious,
                        (* If we have a successor then that is the entry and
                           its predecessor returns here. *)
                        next =
                        case sl of
                            [] => NONE
                        |   t  => SOME(getEntry(t, SOME(getEntry(this, getPrevious))))
                    },
                    s
                    )
            |   getEntry _ () = raise Empty
        in
            (location, parentTree @ [PTfirstChild(getEntry(sl, NONE))])
        end
    in
        exportTree trees
    end
   
    (* Formal paramater to a functor - either value or exception. *)
    fun mkFormal (name : string, class, typ, addr, locations) =
  	    Value{class=class, name=name, typeOf=typ, access=Formal addr, locations=locations}

    (* Apply a function repeatedly to a range of arguments.  This doesn't seem to be in a library. *)
    fun tabulate(from, upto, f: int -> unit) =
        if from >= upto
        then ()
        else (f from; tabulate(from+1, upto, f))

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
    end;

    (* TODO: If the item being errored is in a substructure it currently doesn't report
       the name of the substructure. *)
    (* Report an error about signature-structure matching. *)
    fun sigStructMatchMsg (lex, near, lno) (doDisplay: 'a -> pretty)
                (structValue: 'a, sigValue: 'a, reason) =
        let
            val message =
                PrettyBlock(0, true, [],
                    [
                        PrettyString "Structure does not match signature.",
                        PrettyBreak(1, 0),
                        PrettyBlock(3, false, [],
                            [
                                PrettyString "Signature:",
                                PrettyBreak(1, 0),
                                doDisplay sigValue
                            ]),
                        PrettyBreak(1, 0),
                        PrettyBlock(3, false, [],
                            [
                                PrettyString "Structure:",
                                PrettyBreak(1, 0),
                                doDisplay structValue
                            ]),
                        PrettyBreak(1, 0),
                        PrettyBlock(3, false, [],
                            [
                                PrettyString "Reason:",
                                PrettyBreak(1, 0),
                                reason
                            ])
                    ])
        in
            errorMsgNear(lex, true, near, lno, message)
        end

    fun sigStructMissingMsg (lex, near, lno) (doDisplay: 'a -> pretty) (sigValue: 'a) =
        let
            val message =
                PrettyBlock(0, true, [],
                    [
                        PrettyString "Structure does not match signature.",
                        PrettyBreak(1, 0),
                        PrettyBlock(3, false, [],
                            [
                                PrettyString "Signature:",
                                PrettyBreak(1, 0),
                                doDisplay sigValue
                            ]),
                        PrettyBreak(1, 0),
                        PrettyBlock(3, false, [],
                            [
                                PrettyString "Structure:",
                                PrettyBreak(1, 0),
                                PrettyString "Not present"
                            ])
                    ])
        in
            errorMsgNear(lex, true, near, lno, message)
        end

    (* Older version: prints just a string message. *)
    fun errorNear(lex, hard, near, lno, message: string) =
        errorMsgNear (lex, hard, near, lno,
            PrettyBlock (0, false, [], [PrettyString message]))

    fun errorDepth lex =
    let
        open DEBUG
        val parameters = LEX.debugParams lex
    in
        getParameter errorDepthTag parameters
    end

    (* Returns a function which can be passed to typetree.match to
       print a bit of context information. *)
    fun foundNear (sVal : structs, name : string, lex) (): pretty =
        PrettyBlock(0, false, [],
            [
                PrettyString ("While checking (" ^ name ^ ") near"),
                PrettyBreak (1, 2),
                displayStruct(sVal, errorDepth lex)
            ]
        )

    (* Error message routine for lookupType and lookupStructure. *)
    fun giveError (sVal : structs, lno : LEX.location, lex : lexan) : string -> unit =
        fn (message : string) => errorNear (lex, true, fn n => displayStruct(sVal, n), lno, message);

    (* Turn a result from matchTypes into a pretty structure so that it
       can be included in a message.
       TODO: Pass in the appropriate environments to print type constructors. *)
    fun matchErrorReport lex (alpha, beta, reason) =
        (* This previously used a single type variable sequence for
           both types.  It may be that this is needed to make
           sensible error messages. *)
        PrettyBlock(3, false, [],
            [
                PrettyString "Can't match",
                PrettyBreak (1, 0),
                display (alpha, errorDepth lex, NONE),
                PrettyBreak (1, 0),
                PrettyString "to",
                PrettyBreak (1, 0),
                display (beta, errorDepth lex, NONE),
                PrettyBreak (1, 0),
                PrettyString reason                        
            ])

    (* Check that two types match. *)
    fun matchTypes (candidate, target, targMap: int -> typeId option) =
    let
        fun copyId(id as Bound{ offset, ...}) =
                getOpt(targMap offset, id)
        |   copyId id = id
        fun copyATypeConstr tcon = copyTypeConstr(tcon, copyId, fn x => x, "")
        fun copyTarget t = (* Don't bother with type variables. *)
            copyType (t, fn x => x, copyATypeConstr);
        val copiedTarget = copyTarget target
        (* Do the match to a version of the candidate with copies of the
           type variables so that we can instantiate them.  We could do
           this by passing in a mapping function but the problem is that
           if we have a type variable that gets unified to another variable
           we will not map it properly if it occurs again (we call "eventual"
           and get the second tv before calling the map function so we get a
           second copy and not the first copy). *)
        val copiedCandidate : types = generalise candidate;
    in
        unifyTypes (copiedCandidate, copiedTarget)
    end;

    (* Check that a matching has succeeded, and check the value
       constructors if they are datatypes. *)
    fun checkTypeConstrs (candid, target, targTypeMap: int -> typeId option, lex, near, lno) =
    let
        val candidName : string = tcName candid;
        val targetName : string = tcName target;
        val tvars = List.map TypeVar (tcTypeVars target); (* either will do *)
        (* If we get an error in the datatype itself print the full datatype. *)
        val errorInDatatype =
            sigStructMatchMsg(lex, near, lno)(fn t => displayTypeConstrs(t, errorDepth lex))
        (* If we get an error with the constructors print just the constructor.
           TODO: Use the signature or structure environment to print properly. *)
        val errorInConstructor =
            let
                fun displayConstructor value =
                    PrettyBlock(0, false, [],
                        [
                            PrettyString "constructor",
                            PrettyBreak(1, 3),
                            PrettyString(valName value ^ ":"),
                            PrettyBreak(1, 0),
                            display (valTypeOf value, errorDepth lex, NONE)
                        ])
            in
                sigStructMatchMsg(lex, near, lno) displayConstructor
            end
    in
        if tcArity candid <> tcArity target
        then () (* Have already given the error message. *)
        else (* Check the type constructors themselves first. This checks
                that the sharing constraints have been satisfied. *)
        case matchTypes (mkTypeConstruction (candidName, candid, tvars, []),
                         mkTypeConstruction (targetName, target, tvars, []), 
                         targTypeMap) of
                SOME error => errorInDatatype(candid, target, matchErrorReport lex error) (* Report the error. *)
            |   NONE =>
                let (* No error when matching the types: check the value constructors. *)
                    val candidConstrs : values list = tcConstructors candid;
                    val targetConstrs : values list = tcConstructors target;
                in 
                    (* We have already checked for matching a type in the structure
                       to a datatype in the signature. *)
                    if null targetConstrs orelse null candidConstrs
                    then ()
                    else if not (ListPair.allEq
                                    (fn(cVal, tVal) => valName cVal = valName tVal)
                                    (candidConstrs, targetConstrs))
                    then errorInDatatype(candid, target,
                                PrettyString "Mismatched constructor names or different numbers of constructors")
                    else
                    let
                        (* Fold this so we only report a single error even if there are multiple errors. *)
                        fun matchConstructors(cVal, tVal, false) =
                        (
                            case matchTypes (valTypeOf cVal, valTypeOf tVal, targTypeMap) of
                                NONE => false
                            |   SOME error =>
                                (
                                    errorInConstructor(cVal, tVal, matchErrorReport lex error);
                                    true
                                )
                        )
                                
                        |   matchConstructors(_, _, true) = true
                    in
                        ListPair.foldl matchConstructors false (candidConstrs, targetConstrs);
                        ()
                    end
                end
    end (* checkTypeConstrs *);

    (* Check that a candidate signature (actually the environment part of
       a structure) matches a target signature. The direction is important
       because a candidate is allowed to have more components and more
       polymorphism than the target.  As part of the matching process we
       build up a map of typeIDs in the target to type IDs in the candidate
       and that is returned as the result. *)
    fun matchSigs(candidate, target, near, lno, lex): int -> typeId =
    let
        local
            val minTarget = sigMinTypes target
            and maxTarget = sigMaxTypes target
            (* All the Bound type IDs in the target are in this range.  We create an array
               to contain the matched IDs from the candidate. *)
            val matchArray = Array.array(maxTarget-minTarget, NONE)
        in
            (* These two functions are used during the match process. *)
            (* When looking up a Bound ID we return NONE if it is out of the range.
               Bound IDs below the minimum are treated as global at this level and so
               only match if they are the same in the target and candidate. *)
            fun lookupType n =
                if n < minTarget then NONE else Array.sub(matchArray, n-minTarget)
            and enterType (n, id) = 
                if n < minTarget then () else Array.update(matchArray, n-minTarget, SOME id)

            (* This is the result function.  If everything is right every entry in the
               array will be SOME but if we have had an error there may be entries that
               are still NONE.  To prevent an exception we return the undefined type in
               that case. *)
            fun resultType n = getOpt(Array.sub(matchArray, n-minTarget), tcIdentifier undefType)
        end
    (* Match typeIDs for types. This is slightly more
	   complicated than simply assigning the stamps. *)
    fun matchNames (candidate, target) : unit =
        univFold (sigTab target,
            fn (dName, dVal, ()) =>
            if tagIs typeConstrVar dVal
            then
            let (* See if there is one with the same name. *)
                val target = tagProject typeConstrVar dVal;
                fun displayType t = displayTypeConstrs(t, errorDepth lex)
                val typeError = sigStructMatchMsg(lex, near, lno) displayType
            in (* Match up the types. This does certain checks but
                  does not check sharing. Equality is checked for. *)
                case univLookup (sigTab candidate, typeConstrVar, dName) of
                    SOME candid =>
                        if not (isUndefinedTypeConstr target) (* just in case *)
                        then
                        ( 
                            (* Check for arity and equality - value constructors 
                               are checked later. If the target is a bound identifier
                               in the range it can be matched by a candidate. *)
                            case tcIdentifier target of
                                Bound { offset, ...} => enterType (offset, tcIdentifier candid)
                            |   _ => ();
                    
                            if tcArity target <> tcArity candid
                            then typeError(candid, target,
                                        PrettyString "Types take different numbers of type arguments.")

                            (* Check that it's a datatype before checking for eqtype. *)
                            else if not (null (tcConstructors target)) andalso
                                    null (tcConstructors candid)
                            then typeError(candid, target, 
                                        PrettyString "Type in structure is not a datatype")

                            else if tcEquality target andalso not (permitsEquality candid)
                            then typeError(candid, target, 
                                        PrettyString "Type in structure is not an equality type")
                           
                            else () 
                        )
                        else ()
                |   NONE => sigStructMissingMsg(lex, near, lno) displayType target
            end
             
            else if tagIs structVar dVal
            then
            let (* and sub-structures. *)
                val target = (tagProject structVar) dVal;
                (* For each target structure: find a candidate with the 
                   same name and recursively check them. *)
            in
                case univLookup (sigTab candidate, structVar, dName) of
                   SOME candid => matchNames (structSignat candid, structSignat target)
                |  NONE =>
                    let
                        fun displayStructure s =
                            PrettyBlock(0, false, [],
                                [PrettyString "structure" , PrettyBreak(1, 3), PrettyString(structName s)])
                    in
                        sigStructMissingMsg(lex, near, lno) displayStructure target
                    end
            end
            else (), (* not a type or structure *)
          ()  (* default value for fold *)
        ) (* matchNames *);
      
    val U : unit = matchNames (candidate, target);
       
    (* Match the values and exceptions in the signatures.
       This actually does the checking of types. *)
    fun matchVals (candidate, target) : unit =
    (* Map the identifiers first, returning the originals if they are
         not in the map. *)
    let
      val checkTypesAndStructures : unit =
          univFold 
           (sigTab target,
            (fn (dName, dVal, ()) =>
              if tagIs typeConstrVar dVal
              then let (* For each type in the target ... *)
                val target = tagProject typeConstrVar dVal;
                
                (* Find a candidate with the same name. *)
              in
                  case univLookup (sigTab candidate, typeConstrVar, dName) of
                     SOME candid =>
                        (* Now check that the types match. *)
                        checkTypeConstrs(candid, target, lookupType, lex, near, lno)
                  | NONE => () (* If the lookup failed ignore
                              the error - we've already reported it in matchNames *)
              end
               
              else if tagIs structVar dVal
              then let (* and each sub-structure *)
                val target = tagProject structVar dVal;
              in
                (* For each target structure: find a candidate with the same
                   name and recursively check them. *)
                case univLookup (sigTab candidate, structVar, dName) of
                   SOME candid => matchVals (structSignat candid, structSignat target)
                |  NONE => () (* Ignore the error - we've already reported it in matchNames *)
              end
  
              else ()
            ), (* fn *)
           ()
          );

        val checkValuesAndExceptions : unit =
            (* Finally the values and exceptions. *)
            univFold 
                (sigTab target,
                    (fn (dName, dVal, ()) =>
                        if tagIs valueVar dVal
                        then case tagProject valueVar dVal of
                            Value{class=Constructor _, ...} => () (* Already checked with datatype. *)
                        |   (v as Value{class=Exception, ...}) => (* The candidate must be an exception. *)
                            let
                                fun displayException value =
                                    PrettyBlock(0, false, [],
                                            PrettyString "exception" ::
                                            PrettyBreak(1, 3) ::
                                            PrettyString(valName value) ::
                                        (
                                            case getFnArgType (valTypeOf value) of
                                               NONE => []
                                            |  SOME excType =>
                                                [
                                                    PrettyBreak (1, 1), PrettyString "of",
                                                    PrettyBreak (1, 3), display (excType, errorDepth lex, NONE) ]
                                        ))
                            in
                                case univLookup (sigTab candidate, valueVar, dName) of
                                    (* Look up a corresponding value and check the type. *)
                                    SOME candid =>
                                    let
                                    in
                                        (* Check that exceptions have matched with exceptions 
                                           and values with values, and have not mixed. *)
                                        case candid of
                                            Value {class = Exception, ...} =>
                                            (
                                                case matchTypes (valTypeOf candid, valTypeOf v, lookupType) of
                                                    NONE => ()
                                                |   SOME error =>
                                                        sigStructMatchMsg(lex, near, lno)
                                                            displayException (candid, v, matchErrorReport lex error) 
                                            )
                                        |   _ =>
                                            (* It's there but it's not an exception. *)
                                            sigStructMissingMsg(lex, near, lno) displayException candid
                                    end
                                |   NONE =>
                                        sigStructMissingMsg(lex, near, lno) displayException v
                            end
		                |   v => (* An ordinary value *)
                            let
                                fun displayValue value =
                                    PrettyBlock(0, false, [],
                                        [
                                            PrettyString "val",
                                            PrettyBreak(1, 3),
                                            PrettyString(valName value ^ ":"),
                                            PrettyBreak(1, 0),
                                            display (valTypeOf value, errorDepth lex, NONE)
                                        ])
                            in
                                case univLookup (sigTab candidate, valueVar, dName) of
                                    (* Look up a corresponding value and check the type. *)
                                    SOME candid =>
                                    (
                                        case matchTypes (valTypeOf candid, valTypeOf v, lookupType) of
                                            NONE => ()
                                        |   SOME error =>
                                                sigStructMatchMsg(lex, near, lno)
                                                    displayValue (candid, v, matchErrorReport lex error)
                                    )
                                |   NONE => sigStructMissingMsg(lex, near, lno) displayValue v
                            end
                        else ()
                    ),
                    ()
                )
      in
         ()
      end (* matchVals *);
    in 
        matchVals (candidate, target); (* Do the match. *)
        resultType (* Return the function to look up the results. *)
    end (* matchSigs *);

    val makeEnv = fn x => let val Env e = makeEnv x in e end;

    (* Turn a locally created ID into a Formal which can be returned in a structure. *)
    fun convertIdToFormal(n, Bound { offset, eqType, ...}) =
            Bound { access = Formal n, offset = offset, eqType = eqType }
    |   convertIdToFormal(n, Free { uid, allowUpdate, ... }) =
            Free { access = Formal n, uid = uid, allowUpdate = allowUpdate }
    |   convertIdToFormal _ = raise InternalError "Not bound or free"

  (* Second pass - identify names with values and type-check *)
 
      (* Get the value from a signature-returning expression
         (either the name of a signature or sig ... end.
         The type IDs in the signature are bound names. *)
    fun sigVal(str           : structs,
             initTypeId    : int,
             Env globalEnv : env,
             lex,
             lno           : LEX.location,
			 strName	   : string
            ) : signatures =
    let
       (* Process a sharing constraint. *)
        fun applySharingConstraint 
              ({shares = tlist, isType, line}: shareConstraint,
               Env tEnv    : env,
               near        : structs)
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
                    errorMsgNear (lex, true, fn n => displayStruct(near, n), lno,
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
                   already may be set to another type through a "where type".  This
                   could be a free identifier or a type function.  *)
                else if not (isVariableId (tcIdentifier typeA))
            	then cantShare (tcName typeA ^ " is already defined as another type.")

                else if not (isVariableId (tcIdentifier typeB))
                then cantShare (tcName typeB ^ " is already defined as another type.")
                else if sameTypeId (tcIdentifier typeA, tcIdentifier typeB)
                        (* Are they the same already? If so skip all this. *)
                then ()
                else
                    (* Link the types together.  This propagates the equality and
                       "datatype" properties. *)
                    linkFlexibleTypeIds (tcIdentifier typeA, tcIdentifier typeB)
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
        fun sigValue (str : structs, Env env : env, lno : LEX.location, structName) =
		let
			(* Make a new signature. *)
			val (sigName, loc) =
				case str of
					SignatureIdent nameLoc => nameLoc
				|	_ => ("", lno)
			val newTable = makeSignatureTable();
			(* Copy everything into the new signature. *)
            val structEnv = makeEnv newTable;

			(* ML 97 does not allow multiple declarations in a signature. *)
			fun checkAndEnter enter lookup kind (s: string, v) =
                case lookup s of
                   SOME _ => (* Already there. *)
				     errorNear (lex, true, fn n => displayStruct(str, n), lno, 
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
			val nextOffset = makeSigInto(str, Env checkedStructEnv, Env env, lno, structName, 0);
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
		and makeSigInto(str: structs,
						Env structEnv, (* The immediately enclosing sig. *)
						Env globalEnv, (* The surrounding environment excluding this sig. *)
						lno: LEX.location, strName: string,
					    offset: int): int =
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
                in
                    makeVariableId(isEquality oldId)
                end;
                
                val minOffset = sigMinTypes sourceSig and maxOffset = sigMaxTypes sourceSig
  
                val v = Vector.tabulate (maxOffset-minOffset, fn n => makeNewId(n+minOffset))
                fun typeMap id = Vector.sub (v, id - minOffset)
                fun copyId(id as Bound{ offset, ...}) = typeMap offset
                |   copyId id = id

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
				val () = fullCopySig(sourceSig, tsvEnv, copyId, strName)
            in
                ! address
            end
  
          | SigDec (sigsList : structs list, _) =>  (* sig .... end *)
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
                      val (resSig, _) = sigValue (sigStruct, Env newEnv, line, name ^ ".");
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
               
              | IncludeSig (structList : structs list) =>
              let
                (* include sigid ... sigid or include sigexp.  For
				   simplicity we handle the slightly more general case
				   of a list of signature expressions.
				  The contents of the signature are added to the environment. *)
                fun includeSigExp (str: structs, offset) =
					makeSigInto(str, Env structEnv, Env globalEnv, lno, strName, offset)
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
                
              | CoreLang {dec, ...} =>
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

                val t : types =
                  pass2 (dec, makeVariableId, Env newEnv, lex, strName);
				(* Replace the constructor list for the datatype with the modified
				   constructors.  All the constructors should be in the set.  Is
				   it possible that one might not be because of an error? *)
				fun findConstr(v: values): values =
					getOpt((#lookupVal structEnv)(valName v), v)
				fun updateConstrList tyCons =
					tcSetConstructors(tyCons, List.map findConstr (tcConstructors tyCons))
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

 			  val resAddr = makeSigInto(sigExp, Env newEnv, Env globalEnv, lno, strName, offset)

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
		  in
              (* Now try to set the target type to the type function. *)
              setWhereType(typeConstr, typeVars, realisation, giveError (str, line, lex));
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
                val typeIds = ref []
    
                fun makeTypeId eq =
                let
                    (* For each ID we need a new entry in the ID vector.  We also
                       need an entry in the run-time vector for the structure so that
                       we can pass the equality/print value at run-time. *)
                    val n = !typeCounter
                    val () = typeCounter := n + 1
                    val addr = ! addrCounter
                    val () = addrCounter := addr + 1
                    val newId = makeBoundId(Formal addr, n, eq)
                in
                    typeIds := newId :: ! typeIds;
                    newId
                end
    
                fun setStamps source =
                    univFold 
                        (sigTab source,
                            (fn (dName, dVal, ()) =>
                                if tagIs structVar dVal
                                then setStamps (structSignat (tagProject structVar dVal))
              
                                else if tagIs typeConstrVar dVal
                                then
                                let
                                    val typConstr = tagProject typeConstrVar dVal
                                in
                                    setTypeConstr (typConstr, fn eq => makeTypeId eq)
                                end
                                else ()
                            ),
                        ()
                    );
                val () = setStamps resultSig;

                (* Convert this into an indexing function to return the type ID. *)
                val typeIdVec = Vector.fromList(List.rev(! typeIds))
                fun typeIDMap n = Vector.sub(typeIdVec, n-initTypeId)
            in
                (* Set the size of the type table for the signature we return. *)
                makeSignature (sigName resultSig, sigTab resultSig, initTypeId, !typeCounter, sigDeclaredAt resultSig, typeIDMap)
            end (* not (isSignatureIdent str) *)
    end (* sigVal *);
                  
      (* Process structure-returning expressions i.e. structure names,
         struct..end values and functor applications. *)
    fun structValue(str, newLocalTypeId: (bool*bool)->typeId, newFormalTypeId: (bool*bool*int)->typeId, currentTypeCount, Env env, lex, lno, strName) =
    let (* Look up a structure name. *)         
    in
        case str of
          StructureIdent {name, valRef, location} =>
          let (* Look up the name and save the value. *)
                val result =
                    lookupStructure ("Structure", {lookupStruct = #lookupStruct env}, 
                               name, giveError (str, location, lex));
          in
                if isUndefinedStruct result
                then undefinedSignature 
                else (valRef := result; structSignat result ) 
          end

        | FunctorAppl {name, arg, valRef, nameLoc, fullLoc, argHolder, argIds, resIds, resultSig, ... } =>
          (* The result structure must be copied to generate a new
             environment. This will make new types so that different
             applications of the functor yield different types. There may be 
             dependencies between the parameters and result signatures so
             copying may have to take that into account. *)
          let 
            (* Look up the functor name.  ML doesn't allow functors to be in structures. *)
            val functr: functors =
                case #lookupFunct env name of
                    SOME f => f
                |   NONE =>
                    (
                        giveError (str, nameLoc, lex) ("Functor (" ^ name ^ ") has not been declared");
                        undefinedFunctor
                    )
          in
            if isUndefinedFunctor functr
            then undefinedSignature
            else
            let
                val () = valRef := functr; (* save it till later. *)
              
                val functorResSig : signatures = functorResult functr;
                val argStruct : structVals = functorArg functr;
                val formalArgSig = structSignat argStruct
                
                (* This provides information about the arguments. *)
                (* Get the actual parameter value. *)
                val actualArgSig = structValue(arg, newLocalTypeId, newFormalTypeId, currentTypeCount, Env env, lex, fullLoc, "");
                (* Check that the actual arguments match formal arguments,
                   and instantiate the variables. *)
                val matchResults = matchSigs (actualArgSig, formalArgSig, fn n => displayStruct(str, n), fullLoc, lex);
                (* Create a list of the type IDs that the argument must supply. *)
                local
                    val args = ref nil
                    fun makeList n =
                        args := { source = matchResults n, dest = sigTypeIdMap formalArgSig n } :: ! args
                in
                    val () = tabulate(sigMinTypes formalArgSig, sigMaxTypes formalArgSig, makeList)
                    val () = argIds := List.rev(! args); (* Save for code-generation. *)
                end
                
                (* Create a holding structure for the actual argument.  This allows us to have something
                   to extract typeIDs from if there is sharing between the argument and the result. *)
                val holdingStruct = makeLocalStruct("holder", actualArgSig, [])
                val () = argHolder := holdingStruct
                
                (* Now create the generative typeIDs.  There are, currently, two cases here.  If the
                   result signature of the functor was a named signature and used opaque matching
                   sigMinTypes will be zero and all the type IDs are generative.  There can be no
                   sharing with the arguments so we need to overwrite the argument entries in "v".
                   In all other cases there may be some sharing so sigMinTypes functorResSig will
                   be sigMaxTypes argSig + 1.  The generative IDs will follow on from the arguments. *)
                fun makeNewTypeId n =
                let
                    val oldId = sigTypeIdMap functorResSig(n+sigMinTypes functorResSig)
                    val newId = newLocalTypeId(false, isEquality oldId)
                    val newAccess = idAccess oldId
                    val () = resIds := { source = oldId, dest = newId } :: ! resIds
                in
                    case newId of
                        Free { uid, allowUpdate, ...} =>
                            Free { uid=uid, allowUpdate=allowUpdate, access=newAccess }
                    |   Bound { offset, eqType, ... } =>
                            Bound { offset = offset, eqType = eqType, access = newAccess }
                    |   _ => raise InternalError "makeLocalTypeId: Not Bound or Free"
                end
                val resVector =
                    Vector.tabulate(sigMaxTypes functorResSig - sigMinTypes functorResSig, makeNewTypeId)
                (* We need to extract the argument type IDs from the holder. *)
                val minCopy = Int.min(sigMinTypes formalArgSig, sigMinTypes functorResSig)
                fun convertArg n =
                    case matchResults n of
                        Free { uid, allowUpdate, access=Formal addr } =>
                            Free { uid=uid, allowUpdate=allowUpdate, access=makeSelected(addr, holdingStruct)}
                    |   Bound { offset = offset, eqType = eqType, access = Formal addr } =>
                            Bound { offset = offset, eqType = eqType, access = makeSelected(addr, holdingStruct) }
                    |   others => others
                    
                val convertedArgs = Vector.tabulate(sigMaxTypes formalArgSig-minCopy, convertArg)
                fun getFinalTypeId n =
                    if n < sigMinTypes functorResSig
                    then Vector.sub(convertedArgs, n-minCopy)
                    else Vector.sub(resVector, n - sigMinTypes functorResSig)
                (* Build a modified signature for the result that begins at the smaller of the result or
                   argument. *)
                val resSig =
                    copySig (makeSignature(sigName functorResSig, sigTab functorResSig, minCopy,
                                        sigMaxTypes functorResSig, sigDeclaredAt functorResSig, sigTypeIdMap functorResSig),
                             fn n => n >= minCopy, getFinalTypeId, strName)
            in
                resultSig := resSig;
                resSig
            end
          end
                           
        | StructDec {alist, value = structTable, location, localIds, resultSig, ...} =>
          let
            (* Collection of declarations packaged into a structure
                or a collection of signatures. *)
            (* Some of the environment, the types and the value constructors,
               is generated during the first pass. Get the environment from
               the structure. *)
            val structEnv = makeEnv structTable
            val initialBoundCount = currentTypeCount()

            val addrs = ref 0; (* Entries in the run-time vector. *)

            (* When making new IDs remember them in the list. *)
            fun makeLocalTypeId (isVar, isEq) =
            let
                val newId = newLocalTypeId (isVar, isEq)
                (* We add an entry to the run-time vector ready for this to be returned. *)
	            val a = !addrs;
                val () = addrs := a + 1
                val convertId = convertIdToFormal(a, newId)
            in
                localIds := { source = newId, dest = convertId } :: ! localIds;
                newId
            end

            (* When we enter a structure into the environment we have to add any
               type IDs that were marked as Formal to the id list.  This lifts the
               IDs up to the top level ensuring that any if we have a Formal id
               in the result signature they are relative to the vector for this
               "struct end" block not relative to the internal structure.
               This is only a matter of the run-time type ID data.  The compile-time
               ID is unaffected. *)
            fun enterStructure(name, str) =
            let
                val strSig = structSignat str
                fun convertId n =
                let
                    val id = sigTypeIdMap strSig n
                in
                    if (isFreeId id orelse isBoundId id) andalso isFormal(idAccess id)
                    then
                    case List.find(fn {source, ...} => sameTypeId(source, id)) (!localIds) of
                        NONE => (* Not there already: have to add it. *)
                        let
                            (* The source has to be Selected on the structure. *)
                            val newId =
                                case id of
                                    Bound { offset, eqType, access = Formal addr } =>
                                        Bound { access = makeSelected(addr, str), offset = offset, eqType = eqType }
                                |   Free { uid, allowUpdate, access = Formal addr } =>
                                        Free{uid=uid, allowUpdate=allowUpdate, access=makeSelected(addr, str)}
                                |   id => id
            	            val a = !addrs;
                            val () = addrs := a + 1
                            val convertId = convertIdToFormal(a, id)
                        in
                            localIds := { source = newId, dest = convertId } :: ! localIds
                        end
                            
                    |   SOME _ => ()
                    else ()
                end
                val () = tabulate(sigMinTypes strSig, sigMaxTypes strSig, convertId)
            in
                #enterStruct structEnv(name, str)
            end

            val newEnv =
            {
                enterType = #enterType structEnv,
                enterVal = #enterVal structEnv,
                enterStruct = enterStructure,
                enterSig = fn _ => raise InternalError "Signature in Struct End",
                enterFunct = fn _ => raise InternalError "Functor in Struct End",
                lookupVal = lookupDefault (#lookupVal structEnv) (#lookupVal env),
                lookupType = lookupDefault (#lookupType structEnv) (#lookupType env),
                lookupStruct = lookupDefault (#lookupStruct structEnv) (#lookupStruct env),
                lookupSig    = #lookupSig   env, (* Global *)
                lookupFunct  = #lookupFunct env, (* Global *)
                lookupFix    = #lookupFix   env,
			    (* Fixity declarations are dealt with in the parsing process.  They
			       are only processed again in this pass in order to get declarations
				   in the right order. *)
                enterFix     = fn _ => ()
            }

            (* process body of structure *)
            val () = pass2Struct (alist, makeLocalTypeId, newFormalTypeId, currentTypeCount, Env newEnv, lex, lno, strName);

            (* Copy the result signature.  We need all entries to be Formals.  This signature is really
               only used if we have a structure or functor without an explicit result signature. *)
            val idVec = Vector.fromList(List.rev(!localIds)) (* This has local IDs *)
            val finalTable = makeSignatureTable();
            val finalEnv = makeEnv finalTable

            fun copyId oldId =
                case Vector.find (fn { dest, ...} => sameTypeId(dest, oldId)) idVec of
                    SOME { dest, ...} => dest
                |   NONE => oldId
            val _ =
                COPIER.fullCopySig 
                (makeSignature("", structTable, initialBoundCount, initialBoundCount+Vector.length idVec, location,
                                fn n => #source(Vector.sub(idVec, n-initialBoundCount))),
                {
                    enterType   = fn (s,v) => #enterType finalEnv (s, v),
                    enterStruct =
                        fn (name, strVal) =>
                        let
                            val strSig = structSignat strVal;
                            val locations = structLocations strVal
                            val a = !addrs;
                            (* Set the id range to empty as the bound IDs have now been lifted.
                               This maintains the rule that all bound IDs are included in the top
                               level of the signature and substructures have no bound IDs of their own. *)
                            val resSig =
                                makeSignature (sigName strSig, sigTab strSig,
                                               sigMaxTypes strSig (* Min set to max. *), sigMaxTypes strSig,
                                               sigDeclaredAt strSig, fn _ => raise Subscript)
                        in
                            #enterStruct finalEnv (name, makeFormalStruct (name, resSig, a, locations));
                            addrs := a + 1
                        end,
                    enterVal    =
                            (* If this is a type-dependent function such as PolyML.print we must put in the
                               original type-dependent version not the version which will have frozen
                               its type as 'a. *)
                        fn (name, value as Value{access = Overloaded _, typeOf, locations, ...}) =>
                            #enterVal finalEnv (name, value)
                        |  (name, Value{class, typeOf, locations, ...}) =>
                        let
                            val a = ! addrs
                        in
                            #enterVal finalEnv (name, mkFormal (name, class, typeOf, a, locations));
                            addrs := a + 1
                        end
                },
                copyId, "");
            (* The value for maxTypes is set to the length of the vector plus minTypes.  This works
               both for Bound IDs in a functor and also for Free IDs where currentTypeCount()
               always returns zero. *)
            fun resMapFun n = #dest(Vector.sub(idVec, n-initialBoundCount))
            val resSig =
                makeSignature("", finalTable, initialBoundCount, initialBoundCount+Vector.length idVec, location, resMapFun)
          in
            resultSig := resSig;
            resSig
          end
                
        | Localdec {decs, body = [localStr], line, ...} =>
          let (* let strdec in strexp end *)
            val newEnv = makeEnv (makeSignatureTable());
                   
            (* The environment for the local declarations. *)
            val localEnv =
             { 
              lookupVal     =
                lookupDefault (#lookupVal    newEnv) (#lookupVal    env),
              lookupType    =
                lookupDefault (#lookupType   newEnv) (#lookupType   env),
              lookupFix     = #lookupFix    newEnv,
              lookupStruct  =
                lookupDefault (#lookupStruct newEnv) (#lookupStruct env),
              lookupSig     = #lookupSig    env,
              lookupFunct   = #lookupFunct  env,  (* Sigs and functs are global *)
              enterVal      = #enterVal     newEnv,
              enterType     = #enterType    newEnv,
			  (* Fixity declarations are dealt with in the parsing process.  At
			     this stage we simply need to make sure that local declarations
				 aren't entered into the global environment. *)
              enterFix      = fn _ => (),
              enterStruct   = #enterStruct  newEnv,
              enterSig      = #enterSig     newEnv,
              enterFunct    = #enterFunct   newEnv
             };
             
            (* Process the local declarations. *)
            val U : unit =
              pass2Struct (decs, newLocalTypeId, newFormalTypeId, currentTypeCount, Env localEnv, lex, line, strName);
                   
          in
            (* There should just be one entry in the "body" list. *)
            structValue(localStr, newLocalTypeId, newFormalTypeId, currentTypeCount, Env localEnv, lex, line, strName)
          end
		  
        | SigConstraint { str, csig, opaque, sigLoc, opaqueIds, resultSig, ... } =>
            (* TODO: This code is copied from the structure binding.  It should be merged into a
                single function. *)
            let
                val bodyIds = ref []
                fun sconstraintMakeLocalTypeId (isVar, eq) =
                let
                    val newId = newLocalTypeId(isVar, eq)
                in
                    bodyIds := newId :: ! bodyIds;
                    newId
                end

                val resSig =
                    structValue(str, sconstraintMakeLocalTypeId, newFormalTypeId, currentTypeCount, Env env, lex, lno, strName);
                (* Get the explicit signature. If we are inside a functor
                   we have to make any new bound names outside the range
                   we have already used. *)
                val startTypes = currentTypeCount()
                val explicitSig  : signatures = 
                    sigVal(csig, startTypes, Env env, lex, sigLoc, strName);
                val minExplicitSig = sigMinTypes explicitSig and maxExplicitSig = sigMaxTypes explicitSig                 

                (* Match the signature.  This instantiates entries in typeMap. *)
                val matchResults = matchSigs (resSig, explicitSig, fn n => displayStruct(str, n), sigLoc, lex);
                val rSig =
                    if opaque
                    then
                    let
                        val opaques = ref nil
                        val startResIds = currentTypeCount()
                        (* Construct new IDs for the generic IDs.  For each ID in the signature
                           we need to make a new Local ID.  The access values are copied from
                           the original signature (they should all be Formals). *)
                        fun makeNewId n =
                        let
                            val oldId = sigTypeIdMap explicitSig (n+minExplicitSig)
                            val newId = newLocalTypeId(false, isEquality oldId)
                            val oldAccess = idAccess oldId
                            (* Construct a list of the newIds and their source. *)
                            val () = opaques := { dest = newId, source = matchResults(n+minExplicitSig) } :: ! opaques
                        in
                            case newId of
                                Bound { offset, eqType, ... } =>
                                    Bound { access = oldAccess, offset = offset, eqType = eqType }
                            |   Free { uid, allowUpdate, ... } =>
                                    Free { access = oldAccess, uid = uid, allowUpdate = allowUpdate }
                            |   _ => raise InternalError "Not bound or free"
                        end
;
                        (* Create new IDs for all the bound IDs in the signature. *)
                        val v = Vector.tabulate (maxExplicitSig-minExplicitSig, makeNewId) 
                        fun resTypeMap id = Vector.sub (v, id - minExplicitSig)

                        (* Add the matching IDs to a list.  When we create the code for
                           the structure we need to create new run-time ID values using
                           the original equality code and a new ref to hold the printer. *)
                        val () = opaqueIds := List.rev (!opaques)
                        (* And copy it to put in the names from the structure. *)
                        val copiedSig = copySig (explicitSig, fn s => s >= minExplicitSig, resTypeMap, strName)
                        fun getNewId n = Vector.sub(v, n-startResIds)
                     in
                        makeSignature (sigName explicitSig, sigTab copiedSig, startResIds,
                                       startResIds + maxExplicitSig-minExplicitSig, sigDeclaredAt explicitSig,
                                       getNewId)
                    end
                    else (* Transparent: Use the IDs from the structure. *)
                    let
                        (* We need to return ALL the IDs that were created by the structure value
                           in case they're used in the signature.  Convert them into Formal IDs. *)
                        (* Local IDs are put into run-time offsets 0..N. *)
                        val bodyVec = Vector.mapi convertIdToFormal (Vector.fromList(List.rev(!bodyIds)))
                        val nIDs = Vector.length bodyVec
                        (* When we copy the result we want to replace any IDs that were local with
                           references to these new entries.  But matchResults may return a mixture
                           of types, some local, some not. *)
                        fun resTypeMap n =
                        let
                            val matched = matchResults n
                        in
                            (* If we have the typeID in the vector, ignoring the changed access,
                               then we use that entry. *)
                            case Vector.find(fn id => sameTypeId(id, matched)) bodyVec of
                                SOME newId => newId
                            |   NONE => matched
                        end
                        val resSig =
                            copySigAndRenumber (explicitSig, fn s => s >= sigMinTypes explicitSig, resTypeMap,
                                nIDs, (* Start the run-time offsets after the type IDs *) strName)
                        fun transResId n = Vector.sub(bodyVec, n-startTypes)
                    in
                        (* The result signature.  This needs to be able to enumerate the type IDs
                           including those we've added. *)
                        makeSignature (sigName resSig, sigTab resSig,
                                        startTypes, startTypes+nIDs,
                                        sigDeclaredAt resSig, transResId)
                    end
            in
                resultSig := rSig;
                rSig
            end
                
        | _ =>
           raise InternalError "structValue: not a value"
    end (* structValue *)

    and pass2Struct 
        (strs     : structs list,
         makeLocalTypeId : (bool * bool) -> typeId,
         makeFormalTypeId: (bool * bool * int) -> typeId,
         makeCurrentTypeCount: unit -> int,
         Env env  : env,
         lex,
         lno      : LEX.location,
		 strName  : string
         ) : unit =
    let
      fun pass2StructureDec (str : structs, structList : structBind list) : unit =
      let (* Declaration of structures. *)
        (* The declarations must be made in parallel. i.e.
            structure A = struct ... end and B = A; binds B to the A
            in the PREVIOUS environment, not the A being declared. *)
        val sEnv =  (* The new names. *)
          noDuplicates 
            (fn(name, _, _) => 
              errorNear (lex, true, fn n => displayStruct(str, n), lno, 
                         "Structure " ^ name ^ 
                         " has already been bound in this declaration")
            );
         (* Put the new names into this environment. *)
         
        fun pass2StructureBind ({name, value, valRef, line, opaqueIds, ...}) : unit=
          let (* Each element in the list is a structure binding. *)
            (* Keep a list of the type IDs created in the body.  They're created as Local but if
               they're exported as part of the signature they have to be made Formal. *)
            val bodyIds = ref []
            fun sbindMakeLocalTypeId (isVar, eq) =
            let
                val newId = makeLocalTypeId(isVar, eq)
            in
                bodyIds := newId :: ! bodyIds;
                newId
            end

            val resSig =
				structValue(value, sbindMakeLocalTypeId, makeFormalTypeId, makeCurrentTypeCount,
                            Env env, lex, line, strName ^ name ^".");
              
             (* Now make a local structure variable using this signature. *)
             val var = makeLocalStruct (name, resSig, [DeclaredAt line])
          in
            #enter sEnv (name, var);
            valRef := var
          end
 
      in 
        List.app pass2StructureBind structList;
        (* Put them into the enclosing env. *)
        #apply sEnv (#enterStruct env)
      end; (* pass2StructureDec *)

       fun pass2Localdec (decs : structs list, body : structs list) : unit =
       let
         val newEnv = makeEnv (makeSignatureTable());
             
         (* The environment for the local declarations. *)
         val localEnv =
           {
            lookupVal     =
              lookupDefault (#lookupVal    newEnv) (#lookupVal    env),
            lookupType    =
              lookupDefault (#lookupType   newEnv) (#lookupType   env),
            lookupFix     = #lookupFix    newEnv,
            lookupStruct  =
              lookupDefault (#lookupStruct newEnv) (#lookupStruct env),
            lookupSig     = #lookupSig    env,
            lookupFunct   = #lookupFunct  env,
            enterVal      = #enterVal     newEnv,
            enterType     = #enterType    newEnv,
            enterFix      = fn _ => (),
            enterStruct   = #enterStruct  newEnv,
            enterSig      = #enterSig     newEnv,
            enterFunct    = #enterFunct   newEnv
           };
        
        (* Process the local declarations. *)
        val () = pass2Struct (decs, makeLocalTypeId, makeFormalTypeId, makeCurrentTypeCount, Env localEnv, lex, lno, strName);
             
        (* This is the environment used for the body of the declaration.
           Declarations are added both to the local environment and to
           the surrounding scope. *)
           
        (* Look-ups come from the local env *)
        val bodyEnv = 
          {
           lookupVal     = #lookupVal    localEnv,
           lookupType    = #lookupType   localEnv,
           lookupFix     = #lookupFix    localEnv,
           lookupStruct  = #lookupStruct localEnv,
           lookupSig     = #lookupSig    localEnv,
           lookupFunct   = #lookupFunct  localEnv,
           enterVal      =
             fn (pair as (name, v)) =>
               (
                #enterVal newEnv pair;
                #enterVal env    pair
               ),
           enterType     =
             fn (pair as (name, v)) =>
               (
                #enterType newEnv pair;
                #enterType env    pair
               ),
           enterFix      = #enterFix     localEnv,
           enterStruct   =
             fn (pair as (name, v)) =>
               (
                #enterStruct newEnv pair;
                #enterStruct env    pair
               ),
           enterSig      =
             fn (pair as (name, v)) =>
               (
                #enterSig newEnv pair;
                #enterSig env    pair
               ),
           enterFunct    = #enterFunct   localEnv
          };
      in 
        (* Now the body. *)
        pass2Struct (body, makeLocalTypeId, makeFormalTypeId, makeCurrentTypeCount, Env bodyEnv, lex, lno, strName)
      end; (* pass2Localdec *)
      
      fun pass2Singleton (dec : parsetree, vars) : unit =
      let (* Single declaration - may declare several names. *)
        (* As well as entering the declarations we must keep a list
            of the value and exception declarations. *)
         val newEnv = 
           {
             lookupVal     = #lookupVal    env,
             lookupType    = #lookupType   env,
             lookupFix     = #lookupFix    env,
             lookupStruct  = #lookupStruct env,
             lookupSig     = #lookupSig    env,
             lookupFunct   = #lookupFunct  env,
             (* Must add the entries onto the end in case a declaration
                with the same name is made. e.g.
                   local ... in val a=1; val a=2 end. *)
             enterVal      =
               fn (pair as (name,v)) =>
                 (
                   #enterVal env pair;
                   vars := !vars @ [CoreValue v]
                 ),
             enterType     =
               fn (pair as (name,t)) =>
                 (
                   #enterType env pair;
                   vars := !vars @ [CoreType t]
                 ),
             enterFix      =
               fn (pair as (name,f)) =>
                 (
                   #enterFix env pair;
                   vars := !vars @ [CoreFix pair]
                 ),
             (* This will only be used if we do `open A' where A
                contains sub-structures. *)
             enterStruct   =
               fn (pair as (name,v)) =>
                 (
                   #enterStruct env pair;
                   vars := !vars @ [CoreStruct v]
                 ), 
             enterSig      = #enterSig     env,
             enterFunct    = #enterFunct   env
           };

            (* Create a new type ID for each new datatype. *)
            fun makeId eq = makeLocalTypeId(true, eq)
            (* Process the body and discard the type. *)
            val _ : types = pass2 (dec, makeId, Env newEnv, lex, strName);
       in
         ()
       end; (* pass2Singleton *)

        fun pass2Dec (str as StructureDec (structList : structBind list, _)) =
                pass2StructureDec (str, structList)

        |   pass2Dec(Localdec {decs, body, ...}) =
                pass2Localdec (decs, body)
        
        |   pass2Dec(CoreLang {dec, vars, ...}) =
                pass2Singleton (dec, vars)
        
        |   pass2Dec _ = raise InternalError "pass2Dec"
    in        
        List.app pass2Dec strs (* Process all the top level entries. *)
    end (* pass2Struct *)


    fun pass2Structs ((strs, _): program, lex : lexan, Env globals : env) : unit =
    let
        (* Create a local environment to capture declarations.
           We don't want to add them to the global environment at this point. *)
        val newValEnv   = UTILITIES.searchList()
        and newTypeEnv  = UTILITIES.searchList()
        and newStrEnv   = UTILITIES.searchList()
        and newSigEnv   = UTILITIES.searchList()
        and newFuncEnv  = UTILITIES.searchList()

        val lookupVal =
            lookupDefault (#lookup newValEnv)  (#lookupVal globals)
        and lookupType =
            lookupDefault (#lookup newTypeEnv) (#lookupType globals)
        and lookupStruct =
            lookupDefault (#lookup newStrEnv)  (#lookupStruct globals)
        and lookupSig =
            lookupDefault (#lookup newSigEnv)  (#lookupSig globals)
        and lookupFunct =
            lookupDefault (#lookup newFuncEnv) (#lookupFunct globals)

        val env = 
        {
            lookupVal     = lookupVal,
            lookupType    = lookupType,
            lookupFix     = #lookupFix globals,
            lookupStruct  = lookupStruct,
            lookupSig     = lookupSig,
            lookupFunct   = lookupFunct,
            enterVal      = #enter newValEnv,
            enterType     = #enter newTypeEnv,
            enterFix      = fn _ => (), (* ?? Already entered by the parser. *)
            enterStruct   = #enter newStrEnv,
            enterSig      = #enter newSigEnv,
            enterFunct    = #enter newFuncEnv
        };

        (* Check for free type-variables. *)
		(* Check the type of a value. *)
		fun checkValueForFreeTypeVariables(name: string, v: values) =
			checkForFreeTypeVariables(name, valTypeOf v, lex, codeForUniqueId)

		(* Find all the values in the structure. *)
		fun checkStructSigForFreeTypeVariables(name: string, s: signatures) =
		let
			fun checkEntry(dName: string, dVal: universal, ()) =
				if tagIs structVar dVal
				then checkStructSigForFreeTypeVariables(name ^ dName ^ ".",
						structSignat((tagProject structVar) dVal))
				else if tagIs valueVar dVal
				then checkValueForFreeTypeVariables(name ^ dName, (tagProject valueVar) dVal)
				else ()
		in
			univFold(sigTab s, checkEntry, ())
		end

        fun pass2TopDec (StrDec(str, typeIds)) =
            let
                (* Top level IDs are Free. *)
                fun makeLocalId(isVar, eq) =
                let
                    val newId =
                    (if isVar then makeFreeIdEqUpdate else makeFreeId)
                        (Local{addr = ref 0, level = ref 0}, eq)
                in
                    typeIds := newId :: ! typeIds;
                    newId
                end
                fun makeFormalId(isVar, eq, addr) =
                    (if isVar then makeFreeIdEqUpdate else makeFreeId) (Formal addr, eq)
            in
                (* strdec: structure or core-language topdec. *)
                pass2Struct([str], makeLocalId, makeFormalId, fn () => 0, Env env, lex, location lex, "");
                (* Check for free type variables.  We have to do this after unification
                   within the strdec because later declarations may freeze earlier but
                   we must do this for each topdec.  Otherwise it's possible to associate
                   a global ref with a functor argument and break the type system. *)
                if errorOccurred lex then ()
    		  	else
                (
            		#apply newValEnv (fn (s: string, v: values) => checkValueForFreeTypeVariables(s, v));
            		#apply newStrEnv (
            			fn (n: string, s: structVals) =>
            				checkStructSigForFreeTypeVariables(n^".", structSignat s))
                )
            end
   
        |   pass2TopDec(topdec as FunctorDec (structList : functorBind list, lno)) =
            let
                (* There is a restriction that the same name may not be bound twice.
        		   As with other bindings functor bindings happen in parallel.
        		   DCJM 6/1/00. *)
                val sEnv =  (* The new names. *)
                  noDuplicates 
                    (fn (name, _, _) =>
                        errorNear(lex, true, fn n => displayTopDec(topdec, n), lno,
                            "Functor " ^ name ^ " has already been bound in this declaration")
                    );

                (* Put the new names into this environment. *)
                fun pass2FunctorBind
                    {name,
                     arg = {name = argName, sigStruct = argSig, valRef = argVal},
                     body, valRef, resIds, line, ...} =
                let
                    (* We must copy the signatures to ensure that arguments with
                       the same signature are different. Make an environment to
                       contain the arguments. *)
                    val argEnv = makeEnv (makeSignatureTable());
             
                    (* If it is a "spec" it must be wrapped up in sig...end. *)
                    val signat : signatures =
                    let
        			  	val spec =
        					case argSig of
        						SignatureIdent _ => argSig
        					|	SigDec _ => argSig
        					|	WhereType _ => argSig
        					|	_ => mkSig([argSig], line)
                    in
                        sigVal (spec, 0, Env env, lex, line, "")
                    end;
              
                    val resArg = makeLocalStruct (argName, signat, [DeclaredAt line]);
                    (* Put the copied version in. *)
                    
                    val () = 
                        if argName <> ""
                        then #enterStruct argEnv (argName, resArg)
                        else
                        let (* Open the dummy argument. Similar to "open" in treestruct. *)
                            (* We have to convert typeIds from Formal into Selected.  This is simpler
                               than open in treestruct because all the IDs are Bound. *)
                            fun copyId(id as Bound { offset = offset, eqType = eqType, access = Formal addr }) =
                                    if offset >= sigMinTypes signat andalso offset < sigMaxTypes signat
                                    then Bound { offset = offset, eqType = eqType, access = makeSelected(addr, resArg) }
                                    else id
                            |   copyId id = id                        
                            val _ =
                                COPIER.fullCopySig 
                                (signat,
                                {
                                  enterType   =
                                    fn (s,v) => #enterType argEnv (s, v),
                                  enterStruct =
                                    fn (name, strVal) =>
                                        #enterStruct argEnv (name, makeSelectedStruct (strVal, resArg, [])),
                                  enterVal    =
                                    fn (name, value) =>
                                        #enterVal argEnv (name, mkSelectedVar (value, resArg, []))
                                },
                                copyId, "");
                        in
                            ()
                        end
             
                    val () = argVal := resArg;
             
                    (* Now process the body of the functor using the environment of
                           the arguments to the functor and the global environment. *)
                    val envWithArgs = 
                    {
                        lookupVal     =
                          lookupDefault (#lookupVal    argEnv) (#lookupVal    env),
                        lookupType    =
                          lookupDefault (#lookupType   argEnv) (#lookupType   env),
                        lookupFix     = #lookupFix    env,
                        lookupStruct  =
                          lookupDefault (#lookupStruct argEnv) (#lookupStruct env),
                        lookupSig     = #lookupSig    env,
                        lookupFunct   = #lookupFunct  env,
                        enterVal      = #enterVal     env,
                        enterType     = #enterType    env,
                        enterFix      = fn _ => (),
                        enterStruct   = #enterStruct  env,
                        enterSig      = #enterSig     env,
                        enterFunct    = #enterFunct   env
                    };
               
                    (* When we apply a functor we share type IDs with the argument if they
                       have an ID less than sigMinTypes for the result signature and treat
                       other IDs as generative.  If we don't have an explicit result
                       signature or if we have a transparent signature the type IDs in the
                       result are those returned from the body.  To keep the argument IDs
                       separate from newly created IDs we start creating local IDs with
                       offsets after the args. *)
                    val startTypes = sigMaxTypes signat;
                    val typeStamps = ref startTypes;
                    val localStamps = ref []

                    local
                        (* Create local IDs for any datatypes declared in the body or any generative
                           functor applications. *)
                        fun newLocalTypeId(isVar, eq) =
                        let
                            val n = !typeStamps
                            val () = typeStamps := n + 1;
                            val newId =
                                (if isVar then makeBoundIdWithEqUpdate else makeBoundId)
                                    (Local{addr = ref 0, level = ref 0}, n, eq)
                        in
                            localStamps := newId :: !localStamps;
                            newId
                        end
                        fun newFormalTypeId(isVar, eq, addr) =
                        let
                            val n = !typeStamps
                            val () = typeStamps := n + 1
                            val newId =
                                (if isVar then makeBoundIdWithEqUpdate else makeBoundId)
                                    (Formal addr, n, eq)
                        in                            
                            localStamps := newId :: !localStamps;
                            newId
                        end
                    in
                        val resSig =
                            structValue(body, newLocalTypeId, newFormalTypeId, fn () => !typeStamps,
                                        Env envWithArgs, lex, line, name ^ "().")
                        val () =
                            if errorOccurred lex then ()
		  	                else checkStructSigForFreeTypeVariables(name^"().", resSig)
                    end;

                    val resultIds = ref []

                    val functorSig =
                        let
                            val minSig = sigMinTypes resSig and maxSig = sigMaxTypes resSig
                            (* Make a vector for the type IDs created in the body.  We have an entry here for
                               every type ID whether or not it is mentioned directly in the signature.  We
                               could have types that are mentioned indirectly e.g. we have type t = s * s
                               with "s" being local to the functor and only "t" mentioned in the signature.
                               We need to ensure that "t" is generative. *)
                            fun convertId(n, id as Bound { offset, ...}) =
                                    (* Either inherited from the argument or a new type ID. *)
                                    makeBoundId (Formal n, offset, isEquality id)
                            |   convertId (_, id) = id (* Free or TypeFunction. *)
                            val localVector = Vector.fromList(List.rev(!localStamps))
                            val bodyVec = Vector.mapi convertId localVector
                            fun resTypeMap n = convertId(n-minSig, sigTypeIdMap resSig n)
                            (* Now copy bound IDs that don't share with IDs in the argument and replace them either by entries
                               from the functor body or newly created IDs.  This also ensures that when we apply the
                               functor we can distinguish generative IDs from those that are inherited from the argument.
                               Any bound ID with an offset < sigMinTypes resSig is assumed to be inherited from the
                               argument while new IDs are created for any with offset >= sigMinTypes resSig. *)
                            val resSig =
                                copySigAndRenumber (resSig, fn s => s >= minSig, resTypeMap,
                                    !typeStamps-startTypes (* Start the run-time offsets after the type IDs *),
                                    name ^ "().")
                            (* Record the ID map for code-generation. *)
                            fun makeId n =
                                resultIds :=
                                    { dest = Vector.sub(bodyVec, n), source = Vector.sub(localVector, n) } :: !resultIds
                            val () = tabulate(0, !typeStamps-startTypes, makeId)
                        in
                            (* The resulting signature includes both the type IDs from the arguments and generative type
                               IDs from the body (transparent matching) or result signature (opaque matching). *)
                            makeSignature (sigName resSig, sigTab resSig, startTypes, !typeStamps,
                                            sigDeclaredAt resSig, fn n => Vector.sub(bodyVec, n-startTypes))
                        end

                     (* Now make a local functor variable and put it in the
                        name space. Because functors can only be declared at
                        the top level the only way it can be used is if we have 
                        functor F(..) = ... functor G() = ..F.. with no semicolon
                        between them. They will then be taken as a single
                        declaration and F will be picked up as a local. *)
                      (* Set the size of the type map. *)
                    val var = makeFunctor (name, resArg, functorSig, makeLocal (), line);
                in
                    resIds := List.rev(!resultIds);
                    #enter sEnv (name, var);
                    valRef := var
                end
            in
                (* Each element in the list is a functor binding. *)
                List.app pass2FunctorBind structList;
                (* Put them into the enclosing env. *)
                #apply sEnv (#enterFunct env)		
            end (* FunctorDec *)

        |   pass2TopDec(topdec as SignatureDec (structList : sigBind list, lno)) =
            let
                (* There is a restriction that the same name may not be bound twice.
    		       As with other bindings functor bindings happen in parallel.
    		       DCJM 6/1/00. *)
                val sEnv =  (* The new names. *)
                    noDuplicates 
                    (fn (name, _, _) => 
                      errorNear (lex, true, fn n => displayTopDec(topdec, n), lno, 
                                 "Signature " ^ name ^ " has already been bound in this declaration")
                    );

                fun pass2SignatureBind ({name, sigStruct, line, sigRef, ...}) =
                let (* Each element in the list is a signature binding. *)
                    val resSig : signatures =
				        sigVal (sigStruct, 0, Env env, lex, line, name ^ ".");
                    (* Generate a signature with the new name and put it in the table *)
			        val copiedSig =
                        makeSignature (name, sigTab resSig, sigMinTypes resSig, sigMaxTypes resSig, line, sigTypeIdMap resSig)
                in
		            sigRef := copiedSig; (* Remember the signature for pass4. *)
                    #enter sEnv (name, copiedSig)
                end
            in
                List.app pass2SignatureBind structList;
                (* Put them into the enclosing env. *)
                #apply sEnv (#enterSig env)		
            end
    in 
        List.app pass2TopDec strs
    end (*pass2Structs *);
    
    
    


  (*							*
   *     Code-generation phase.	*
   *							*)

  (* Generate code from the expressions and arrange to return the results
      so that "pass4" can find them. *)
  fun gencodeStructs ((strs, _), lex) =
  let
    (* Each top level declaration is assigned a distinct address. *)
    val addresses = ref 1;
    fun mkAddr ()  = 
      let
        val addr = !addresses
      in
        addresses := addr + 1;
        addr
      end;
      
    val level = ref 0; (* Only non-zero inside a functor body *)
    
    (* Apply a function which returns a pair of codelists to a list of structs.
	   This now threads the debugging environment through the functions so
	   the name is no longer really appropriate.  DCJM 23/2/01. *)
    fun mapPair
		(f: 'a * debugenv -> {code: codetree list, load: codetree list, debug: debugenv})
		[] debug =
          {
            code = [],
            load = [],
			debug = debug
          }
          
      | mapPair f (h::t) debug =
    let
      (* Process the list in order. In the case of a declaration sequence
         later entries in the list may refer to earlier ones. *)
        val this = f (h, debug);
        val rest = mapPair f t (#debug this);
     in  (* Return the combined code. *)
       { 
         code = #code this @ #code rest,
         load = #load this @ #load rest,
		 debug = #debug rest
       }
     end;

    fun matchStructure (code : codetree, source : univTable, sourceIds: int->valAccess, dest : signatures) =
    let
        (* Generate a new structure which will match the given signature.
           A structure is represented by a vector of entries, and its
           signature is a map which gives the offset in the vector of 
           each value. When we match a signature the candidate structure
           will in general not have its entries in the same positions as
           the target. We have to construct a new structure from it with
           the entries in the correct positions. In most cases the optimiser
           will simplify this code considerably so there is no harm in using
           a general mechanism.  Nevertheless, we check for the case when
           we are building a structure which is a direct copy of the original
           and use the original code if possible. *)
        fun matchSubStructure (code, source, sourceIds, dest) =
        let
            val decs = multipleUses (code, mkAddr, !level);
            val load = #load decs (!level); (* All local *)
          
            (* To save taking apart a structure and then rebuilding it, if the
               structure has not changed we just copy it. *)
            val useOriginal = ref true;
          
            (* We put the entries into this vector and then flatten it. *)
            val resVec   = StretchArray.stretchArray (10 (* Guess *), NONE);
            val maxEntry = ref 0;
          
            fun addToList code addr =
            (
                case StretchArray.sub(resVec, addr) of
                    NONE => StretchArray.update (resVec, addr, SOME code)
                |   SOME _ => raise InternalError ("addToList: Duplicate entry " ^ Int.toString addr ^ "\n");
                if addr >= !maxEntry then maxEntry := addr + 1 else ()
            );
        
            fun foldEntry(dName, dVal, ()) =
                if tagIs structVar dVal (* Structures. *)
                then
                let
                    val dval = tagProject structVar dVal;
                in
                    if isFormal (structAccess dval)
                    then
                    let
                        val destAddr     = vaFormal (structAccess dval);
                        val sourceStruct =
                            valOf(univLookup (source, structVar, dName));
             
                        val access = structAccess sourceStruct;
                        (* Since these have come from a signature we might expect all
                           the entries to be "formal". However if the structure is
                           global the entries in the signature may be global, and if
                           the structure is in a "struct .. end" it may be local. *)
                        val code = 
                        if isFormal access
                        then
                        let (* select from the code. *)
                            val () = 
                                if vaFormal access <> destAddr
                                then useOriginal := false
                                else ()
                        in
                            mkInd (vaFormal access, load)
                        end
                        else
                        (
                            useOriginal := false;
                            codeStruct (sourceStruct, !level)
                        );
                        val sourceSig = structSignat sourceStruct
                        val (matched, unchanged) =
                            matchSubStructure (code, sigTab sourceSig, NONE, structSignat dval);
            
                        val () = if not unchanged then useOriginal := false else ();
                    in
                        addToList matched destAddr
                    end
                    else ()
                end

                else if tagIs valueVar dVal
                then
                let (* values. *)
                    val dval = tagProject valueVar dVal;
                in
                    case dval of
                        Value{access=Formal addr, ...} =>
                        let
                            val sourceVal =
                                valOf(univLookup (source, valueVar, dName));
           
                            (* If we have an exception matching a value we have to
                               generate a packet or a function yielding a packet. *)
                            val excBecomesVal =
                                case (dval, sourceVal) of
                                    (Value{class=SimpleValue, ...}, Value{class=Exception, ...}) =>
                                        true
                                |    _ => false
                            (* Similarly, if we have a constructor which becomes a value we
                               have to extract the injection function or the value. *)
                            val constrBecomesVal =
                                case (dval, sourceVal) of
                                    (Value{class=SimpleValue, ...}, Value{class=Constructor _, ...}) =>
                                        true
                                |    _ => false
           
                            (* If the entry is from a signature select from the code. *)
                            val code =
                                case sourceVal of
                                    Value{access=Formal svAddr, ...} =>
                                    (
                                        if svAddr <> addr orelse excBecomesVal orelse constrBecomesVal
                                        then useOriginal := false
                                        else ();
                                        if excBecomesVal
                                        then (* Have to make a packet or a function returning a packet. *)
                                            if not (isSome(getFnArgType (valTypeOf dval)))
                                        then 
                                            mkTuple [mkInd (svAddr, load), mkStr (valName dval), CodeZero]
                                        else
                                            mkProc 
                                            (mkTuple
                                               [mkInd (svAddr, #load decs (!level + 1)),
                                                mkStr (valName dval),
                                                mkLoad (~1, 0)],
                                            1, 1, "")
                                        else if constrBecomesVal
                                        then mkInd(1, mkInd (svAddr, load))
                                        else mkInd (svAddr, load)
                                    )

                                | _ =>
                                    (
                                        useOriginal := false;
                                        if excBecomesVal
                                        then codeExFunction(sourceVal, !level, valTypeOf dval, lex, location nullLex)
                                        else
                                        let
                                            val valu =
                                                codeVal (sourceVal, !level, valTypeOf dval, lex, location nullLex)
                                        in
                                            if constrBecomesVal
                                            then mkInd(1, valu)
                                            else valu
                                        end
                                    );
                        in
                            addToList code addr
                        end
                    |   _ => ()
                end

            else ()
         in  
            (* Structures, values and datatypes. *)
            univFold (sigTab dest, foldEntry, () );
            
            (* Type Ids.  Only at the top-level.*)
            case sourceIds of
                NONE => ()
            |   SOME sourceIds =>
                let
                    (* Process the type IDs in the signature.  We're only interested in typeIDs that are
                       marked as Formal because those are the only ones that need to be extracted from
                       the structure. *)
                    fun doTypeID n =
                    let
                        val typeId = sigTypeIdMap dest n
                    in
                        if isFreeId typeId orelse isBoundId typeId
                        then 
                        case idAccess typeId of
                            Formal addr =>
                            let
                                (* Get the corresponding source ID. *)
                                val codedId =
                                    case sourceIds(n-sigMinTypes dest) of
                                        Formal svAddr =>
                                        (
                                            if svAddr <> addr then useOriginal := false else ();
                                            mkInd (svAddr, load)
                                        )
                                    |   access => (useOriginal := false; codeAccess(access, !level))
                            in
                                addToList codedId addr
                            end
                        |   _ => ()
                        else ()
                    end
                in
                    tabulate (sigMinTypes dest, sigMaxTypes dest, doTypeID)
                end;
             
            (* If we have copied the original we can use it unchanged. *)
            if !useOriginal
            then (code, true)
            else (* Put the entries into a list for the vector. *)
            let
                val codeList =
                      List.tabulate (!maxEntry,
                        fn i => getOpt(StretchArray.sub (resVec,i), CodeZero))
            in
                (mkEnv (#dec decs @ [mkTuple codeList]), false)
            end
        end
    in
        #1 (matchSubStructure (code, source, SOME sourceIds, dest))
    end

    (* If we are declaring a structure with an opaque signature we need to create
       the run-time IDs for newly generated IDs. *)
    fun loadOpaqueIds(getFormal, opaqueIds) =
    let
        fun decId { dest, source } =
        let
            val { addr=idAddr, level=idLevel } = vaLocal(idAccess dest)
            val addr = mkAddr();
            val () = idAddr := addr and () = idLevel := ! level;
            val idCode = codeGenerativeId(source, getFormal, !level)
        in
            mkDec(addr, idCode)
        end
    in
        List.map decId opaqueIds
    end

    (* Code-generate a structure value. *)
    fun structureCode (str, strName, debugEnv): codetree * signatures =
    case str of
        FunctorAppl {name, arg, valRef = ref functs, argHolder=ref argHolder,
                     argIds=ref argIds, resIds=ref resIds, resultSig=ref resultSig, ...} =>
        let
            val (argCodeSource, argCodeSig) =
                structureCode (arg, strName, debugEnv)
            (* Match the actual argument to the required arguments. *)
            local
                val startIds = sigMinTypes(structSignat(functorArg functs))
            in
                fun getMatchedId n =
                let
                    val id = #source (List.nth (argIds, n-startIds))
                in
                    if isFreeId id orelse isBoundId id
                    then idAccess id
                    else Global(codeId(id, !level))
                end
            end
            val argCode =
                matchStructure(argCodeSource, sigTab argCodeSig, getMatchedId, structSignat (functorArg functs))
            (* We need to save the actual argument in a local variable.  It will be passed to
               the functor but if type IDs in the argument are shared with the result structure
               we need to make sure the actual argument is around. *)
            val argAddr = mkAddr ()
            val var = vaLocal(structAccess argHolder)
            val () = #addr var := argAddr and () = #level var := !level
            (* Similarly we need to retain the result in order to produce the generative type IDs. *)
            val resAddr = mkAddr()
            val ids = List.rev resIds
            val loadIds = loadOpaqueIds(fn(n, l) => mkInd(n, mkLoad(resAddr, l - !level)), ids)
            val offset = sigMinTypes(functorResult functs)
            fun getTypeId n = idAccess(#dest (List.nth (ids, n-offset)))
        in
            (* Evaluate the functor. *)
            (
                mkEnv(
                    mkDec(argAddr, argCode) ::
                    mkDec(resAddr, mkEval (codeAccess (functorAccess functs, !level), [mkLoad(argAddr, 0)], false)) ::
                    loadIds @
                    [matchStructure(mkLoad(resAddr, 0), sigTab(functorResult functs), getTypeId, resultSig)]
                ),
                resultSig
            )
        end

    |   StructureIdent {valRef = ref v, ...} =>
            ( codeStruct (v, !level), structSignat v)

    |   Localdec {decs, body = [localStr], ...} =>
        let (* let strdec in strexp end *)
         (* Generate the declarations but throw away the loads. *)
		 (* TODO: Get the debug environment correct here. *)
          val coded =
		  	mapPair (fn (str, debug) => codeStrdecs (strName, str, debug))
				decs debugEnv;
            val (bodyCode, bodySig) = structureCode (localStr, strName, #debug coded)
        in
            (mkEnv (#code coded @ [bodyCode]), bodySig)
        end

      | StructDec {alist, value, localIds=ref localIds, resultSig=ref resultSig, ...} =>
        let
            val coded = mapStrdecs alist strName debugEnv;
            val ids = List.rev localIds
            (* We match to the dummy signature here.  If there is a signature outside
               we will match again.  This results in double copying but that should
               all be sorted out by the optimiser.
               The advantage is that we make sure that all IDs created in the body are
               included in the result signature. *)

            (* Return the type IDs to match the result. *)
            fun getTypeId n =
            let
                val id = #source (List.nth (ids, n))
            in
                if isFreeId id orelse isBoundId id
                then idAccess id
                else Global(codeId(id, !level))
            end
       in 
          (* The result is a block containing the declarations and
             code to load the results. *)
          (
            mkEnv (#code coded @
                [matchStructure (mkTuple (#load coded), value, getTypeId, resultSig)]),
            resultSig
          )
        end

    |   SigConstraint { str, opaqueIds=ref opaqueIds, resultSig = ref resultSig, ... } =>
            let
                (* TODO: Copied from structure binding code. *)
 			    val (strCode, strSig) = structureCode (str, strName, debugEnv)
                val tempDecs = multipleUses (strCode, mkAddr, !level);
                val ids = loadOpaqueIds(fn(n, l) => mkInd(n, #load tempDecs l), opaqueIds)
                fun loadId n =
                let
                    val id =
                        case opaqueIds of
                            [] => sigTypeIdMap strSig (n+sigMinTypes strSig)
                        |   _  => #dest(List.nth(opaqueIds, n))
                in
                    if isFreeId id orelse isBoundId id
                    then idAccess id
                    else Global(codeId(id, !level))
                end
                val matchedCode =
                    matchStructure (#load tempDecs (!level), sigTab strSig, loadId, resultSig)
            in
                (mkEnv(#dec tempDecs @ ids @ [matchedCode]), resultSig)
            end

    |   _ =>
         raise InternalError "structureCode: not a structure"
        (* structureCode *)

    (* We need to generate code for the declaration and then code to load
       the results into a tuple. *)
    and codeStrdecs (strName, str, debugEnv: debugenv):
		{ code: codetree list, load: codetree list, debug: debugenv} =
      case str of
        StructureDec (structList : structBind list, _) =>
        let
          fun codeStructureBind ({name, value, valRef, opaqueIds=ref opaqueIds, ...}: structBind, debug) =
            let
                (* This is a bit complicated because of the opaque IDs.
                   1. Get the code for the structure expression and save that in a local.
                   2. Create any opaque IDs using the saved local address to get
                      references to existing IDs.
                   3. Match this to the result signature *)
			    val sName = strName ^ name ^ "."
 			    val (strCode, strSig) = structureCode (value, sName, debug)
                val tempDecs = multipleUses (strCode, mkAddr, !level);
                val ids = loadOpaqueIds(fn(n, l) => mkInd(n, #load tempDecs l), opaqueIds)
                val resultSig = structSignat (!valRef)
                fun loadId n =
                let
                    val id =
                        case opaqueIds of
                            [] =>
                                (* If there are no opaque IDs we get the IDs from the source signature.
                                   matchStructure has substracted sigMinTypes resultSig so we need to
                                   add that back. *)
                                sigTypeIdMap strSig (n + sigMinTypes resultSig)
                        |   _  => #dest(List.nth(opaqueIds, n))
                in
                    if isFreeId id orelse isBoundId id
                    then idAccess id
                    else Global(codeId(id, !level))
                end
                val matchedCode =
                    matchStructure (#load tempDecs (!level), sigTab strSig, loadId, resultSig)
                val addr = mkAddr(); 
                val var  = vaLocal (structAccess (!valRef));
                val () = #addr var  := addr; 
                val () = #level var := !level;
            in (* Get the code and save the result in the variable. *)
              {
                code = #dec tempDecs @ ids @ [mkDec (addr, matchedCode)],
                (* Load the variable. *)
                load = [mkLoad (addr, 0)],
				debug = debug (* We don't do structures at the moment. *)
              }
            end
        in
          (* Code-generate each declaration. *)
          mapPair codeStructureBind structList debugEnv
        end
         
 
      | Localdec {decs, body, ...} =>
        let (* Accumulate the code from the declarations,
               and the code to load the results. *)
		  (* TODO: Get the environment right here. *)
          val codeDecs = mapStrdecs decs strName debugEnv;
          val codeBody = mapStrdecs body strName (#debug codeDecs);
        in  (* Combine the lists. *)
          { 
            code = #code codeDecs @ #code codeBody,
            load = #load codeBody, (* Result is the body. *)
			debug = #debug codeBody (* TODO: This isn't correct. *)
          }
        end
 
      | CoreLang {dec, vars=ref vars, ...} =>
        let
            (* Code generate the declaration.  This also assigns addresses so must be
               done before we load the results. *)
		    val (code, newDebug) =
		  	    gencode (dec, lex, debugEnv, !level, addresses, strName)

          (* Load each variable, exception and type ID (i.e. equality & print function)
             that has been declared.
             Since value declarations may be mutually recursive we have
             to code-generate the declarations first then return the values. *)
		    fun filterVals (CoreValue v)  = SOME(codeVal (v, !level, badType, nullLex, location nullLex))
            |   filterVals (CoreStruct s) = SOME(codeStruct (s, !level))
		    |   filterVals _              = NONE
            val loadVals = List.mapPartial filterVals vars
        in
            { 
                code = code,
                load = loadVals,
			    debug = newDebug
            }
        end
        
      | _ => (* signature decs *)
        { 
          code = [],
          load = [],
		  debug = debugEnv
        }
    (* end codeStrdecs *)

    and mapStrdecs strs strName debugEnv =
          mapPair (fn (str, debug) => codeStrdecs (strName, str, debug))
		  	strs debugEnv;

    fun codeTopdecs (StrDec(str, ref typeIds), debugEnv) =
        let
            val { code, load, debug } = codeStrdecs("", str, debugEnv)
            (* Load all the IDs created in this topdec even if they're not directly referenced. *)
            fun loadIds id = codeId(id, !level)
        in
            { code = code, load = List.map loadIds typeIds @ load, debug = debug }
        end

    |   codeTopdecs (FunctorDec (structList : functorBind list, _), debugEnv) =
        let
            fun codeFunctorBind ({name, arg = {valRef = ref argVal, ...}, body, valRef, resIds=ref resIds, ...}, debugEnv) =
            let
                (* Go up one level. *)
                val () = level := !level + 1;
              
                (* Save the value and set to 1 *)
                val addr = !addresses;
                val () = addresses := 1;
                val arg = vaLocal (structAccess argVal);
                val () = #addr  arg := ~1; 
                val () = #level arg := !level;
                val func = !valRef;
                (* Process the body and make a procedure out of it. *)
                local
                    val (strCode, strSig) = structureCode (body, name ^ "().", debugEnv)
                    fun getIds n =
                    let
                        val id = #source (List.nth(resIds, n))
                    in
                        if isFreeId id orelse isBoundId id
                        then idAccess id
                        else Global(codeId(id, !level))
                    end
                    val matchedCode =
                        matchStructure (strCode, sigTab strSig, getIds, functorResult func)
                in
                    val functorCode = (* The function that implements the functor. *)
                        (if getParameter inlineFunctorsTag (debugParams lex) then mkMacroProc else mkProc)
                            (matchedCode, !level, 1, name);
                end

                (* Go back down a level and add 1 to address. *)
                val () = level := !level - 1; 
                val () = addresses := addr + 1;

                (* Set the address of this variable. Because functors can only
                   be declared at the top level the only way it can be used is
                   if we have 
                    functor F(..) = ... functor G() = ..F..
                   with no semicolon between them. They will then be taken as
                   a single declaration and F will be picked up as a local. *)
                val var = vaLocal (functorAccess func);
                val ()  = #addr  var := addr;
                val ()  = #level var := !level;
            in
                {
                    code = [mkDec (addr, functorCode)],
                    load = [mkLoad (addr, 0)], (* Load the variable. *)
				    debug = debugEnv
                }
            end
        in
          mapPair codeFunctorBind structList debugEnv
        end

    |   codeTopdecs(SignatureDec _, debugEnv) = { code = [], load = [], debug = debugEnv }
    
    val coded = (* Process top level list. *)
        mapPair (fn (str, debug) => codeTopdecs (str, debug))
		  	strs ([], fn _ => CodeZero)
  in 
    (* The result is code for a vector containing the results of the
       declarations which pass4 can use to pull out the values after
       the code has been run. *)
    mkEnv (#code coded @ [mkTuple (#load coded)])
  end (* gencodeStructs *);

  (* Once the code has been executed the declarations must be added to
     the global scope. The type and infix status environments have already
     been processed so they can be dumped into the global environment
     unchanged. The values and exceptions, however, have to be picked out
     the compiled code. Note: The value constructors are actually produced
     at the same time as their types but are dumped out by enterGlobals. *)
  (* This previously only processed declarations which required some code-generation and
     evaluation (structures, values and functors).  It now includes types, signatures and
	 fixity so that all declarations can be printed in the order of declaration.  DCJM 6/6/02. *)
    fun pass4Structs (results, (strs: topdec list, _)) =
    let
        fun extractStruct(str, mapTypeIds, args as (addr, { fixes, values, structures, signatures, functors, types } )) =
        case str of
            StructureDec (structList : structBind list, _) =>
            let
                fun extractStructureBind ({name, valRef, line, ...}: structBind, (addr, structures)) =
                let
                    val source = structSignat (!valRef);
			        val structCode = mkInd (addr, results);
                    (* We need to replace type IDs with their Global versions. *)
                    val tab = makeSignatureTable ();
                    val tsvEnv =
                    {
                        enterType   = fn (s,v) => univEnter (tab, typeConstrVar, s, v),
                        enterStruct = fn (s,v) => univEnter (tab, structVar, s, v),
                        enterVal = fn (s, v) => univEnter (tab, valueVar, s, v)
                    }
                    (* Copy everything into the new signature. *)
                    val () = COPIER.fullCopySig (source, tsvEnv, mapTypeIds, "");
                    val resultSig =
                        makeSignature(sigName source, tab, sigMinTypes source,
                                      sigMaxTypes source, sigDeclaredAt source, sigTypeIdMap source)
                in
                    (* Make a global structure. *)
                    (addr + 1, (name, makeGlobalStruct (name, resultSig, structCode, line)) :: structures)
                end

                val (newAddr, newstructures) = List.foldl extractStructureBind (addr, structures) structList
            in
                (newAddr, { structures=newstructures, functors=functors, signatures=signatures,
                      fixes=fixes, values=values, types=types })
            end
 
      | Localdec {body, ...} =>
            List.foldl (fn(s, a) => extractStruct(s, mapTypeIds, a))args body          
 
      (* Value, exception or type declaration at the top level. *)
      | CoreLang {vars=ref vars, ...} =>
        let (* Enter the values and exceptions. *)
            (* Copy the types to replace the type IDs with the versions with Global access. *)
            fun replaceTypes t =
            let
                fun replaceTypeConstrs tcon = copyTypeConstr (tcon, mapTypeIds, fn x => x, "")
            in
                copyType(t, fn tv=>tv, replaceTypeConstrs)
            end

		    fun makeDecs (CoreValue(dec as Value{class, name, typeOf, locations, access, ...}),
                          (addr, { fixes, values, structures, signatures, functors, types } )) =
                let
                    (* Extract the value from the result vector except if we have a type-dependent
                       function e.g. PolyML.print where we must use the type-dependent version. *)
                    val newAccess =
                        case access of
                            Overloaded _ => access
                        |   _ => Global(mkInd (addr, results))
                    (* Replace the typeIDs. *)
                    val newVal =
                        Value{class=class, name=name, typeOf=replaceTypes typeOf, access=newAccess, locations=locations}
                in
                    (addr+1, { fixes=fixes, values=(name, newVal) :: values, structures=structures,
                               signatures=signatures, functors=functors, types=types } )
                end

		    |   makeDecs (CoreStruct dec, (addr, {fixes, values, structures, signatures, functors, types})) =
	          (* If we open a structure we've created in the same "program" we may have a non-global
                 substructure.  We have to process any structures. *)
                let
                    val name = structName dec
                    val newStruct =
                        Struct { name = name, signat = structSignat dec,
                                 access = Global(mkInd (addr, results)), locations = structLocations dec }
                in
                 (addr+1, { fixes=fixes, values=values, structures=(name, newStruct) :: structures,
                          signatures=signatures, functors=functors, types=types } )
                end

		    |   makeDecs (CoreFix pair, (addr, {fixes, values, structures, signatures, functors, types})) =
                 (addr, { fixes=pair :: fixes, values=values, structures=structures,
                          signatures=signatures, functors=functors, types=types } )

		    |   makeDecs (CoreType tc, (addr, {fixes, values, structures, signatures, functors, types})) =
                let
                    val copiedTC = fullCopyDatatype(tc, mapTypeIds, "")
                    val newName = #second(splitString(tcName tc))
                in
                    (addr, { fixes=fixes, values=values, structures=structures,
                             signatures=signatures, functors=functors, types=(newName, copiedTC) :: types } )
                end
        in
            List.foldl makeDecs args vars
        end

     | _ => raise InternalError "extractStruct"; (* end extractStruct *)
    
    fun extractTopDec(str, (addr, env as { fixes, values, structures, signatures, functors, types }, mapPrevTypIds)) =
    case str of
        StrDec(str, ref typeIds) =>
        let
            fun loadId(Free{uid, allowUpdate, ...}, (n, ids)) =
                (n+1, Free{uid=uid, allowUpdate=allowUpdate, access=Global(mkInd(n, results))} :: ids)
            |   loadId _ = raise InternalError "Not Free"
            val (newAddr, mappedIds) = List.foldl loadId (addr, []) typeIds
            fun mapTypeIds id =
                case List.find(fn n => sameTypeId(id, n)) mappedIds of
                    SOME newId => newId
                |   NONE => mapPrevTypIds id
            val (resAddr, resEnv) = extractStruct (str, mapTypeIds, (newAddr, env))
        in
            (resAddr, resEnv, mapTypeIds)
        end

    |   FunctorDec (structList : functorBind list, _) =>
        let
          fun extractFunctorBind ({name, valRef, ...}: functorBind, (addr, funcs)) =
            let
              val code = mkInd (addr, results);
              val func = !valRef;
              val funcTree = 
                makeFunctor(functorName func, functorArg func, functorResult func,
                            makeGlobal code, functorDeclaredAt func);
            in
              (addr + 1, (name, funcTree) :: funcs)
            end
          (* Get the functor values. *)
          (* TODO: It's possible that we may have references to types created in previous
             topdecs within this "program".  We should copy the signatures to replace these
             with globals.  *)
          val (newAddr, newfunctors ) = List.foldl extractFunctorBind (addr, functors) structList
        in
          (newAddr, { functors=newfunctors, fixes=fixes, values=values,
                      signatures=signatures, structures=structures, types=types }, mapPrevTypIds)
        end

     | SignatureDec (structList : sigBind list, _) =>
		let
            val newSigs = List.map (fn ({sigRef=ref s, name, ...}: sigBind) => (name, s)) structList
          (* TODO: It's possible that we may have references to types created in previous
             topdecs within this "program".  We should copy the signatures to replace these
             with globals.  *)
		in
           (addr, { fixes=fixes, values=values, structures=structures,
                    signatures=newSigs @ signatures, functors=functors, types=types }, mapPrevTypIds)
		end
    
    val empty = { fixes=[], values=[], structures=[], functors=[], types=[], signatures=[] }

    fun defaultMap(id as Free{access=Global _, ...}) = id
    |   defaultMap _ = raise InternalError "Not there"

    val (lastAddr, result, _) = List.foldl extractTopDec (0, empty, fn id => id) strs;
    (* The entries in "result" are in reverse order of declaration and may contain duplicates.
       We need to reverse and filter the lists so that we end up with the lists in order
       and with duplicates removed. *)
    fun revFilter result [] = result
     |  revFilter result ((nameValue as (name, _)) ::rest) =
    let
        (* Remove any entries further down the list if they have the same name. *)
        val filtered = List.filter (fn (n,_) => name <> n) rest
    in
        revFilter (nameValue :: result) filtered
    end
  in
    { fixes=revFilter [] (#fixes result), values=revFilter [] (#values result), structures=revFilter [] (#structures result),
      functors=revFilter [] (#functors result), types=revFilter [] (#types result), signatures=revFilter [] (#signatures result) }
  end (* pass4Structs *)

    structure Sharing =
    struct
        type structs        = structs
        type structVals     = structVals
        type types          = types
        type parsetree      = parsetree
        type lexan          = lexan
        type pretty         = pretty
        type values         = values
        type typeConstrs    = typeConstrs
        type codetree       = codetree
        type signatures     = signatures
        type functors       = functors
        type env            = env
        type sigBind        = sigBind
        and  functorBind    = functorBind
        and  structBind     = structBind
        type machineWord    = machineWord
        type fixStatus      = fixStatus
        type topdec         = topdec
        type program        = program
        type typeParsetree  = typeParsetree
        type formalArgStruct= formalArgStruct
        type ptProperties   = ptProperties
        type structSigBind  = structSigBind
        type typeVarForm    =typeVarForm
    end
end;
