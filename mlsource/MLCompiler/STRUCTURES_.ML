(*
	Copyright (c) 2000-9
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
    where type exportTree = EXPORTTREE.exportTree
    and   type navigation = EXPORTTREE.navigation;

(*****************************************************************************)
(*                  PARSETREE                                                *)
(*****************************************************************************)
structure PARSETREE :
sig
  type parsetree;
  type types;
  type lexan;
  type pretty;
  type typeId;
  type env;
  type codetree;
  type environEntry
  type fixStatus
  type values
  type exportTree = EXPORTTREE.exportTree
  type navigation = EXPORTTREE.navigation
  type location =
        { file: string, startLine: int, startPosition: int, endLine: int, endPosition: int }

  val ptDisplay: parsetree * int -> pretty;

  val pass2: parsetree * (bool -> typeId) * env * lexan * string -> types;

  type debugenv = environEntry list * (int->codetree)

  val gencode: parsetree * lexan * debugenv * int * int ref * string -> codetree list * debugenv

    val getExportTree: navigation * parsetree -> exportTree
end;

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

(*****************************************************************************)
(*                  STRUCTURES sharing constraints                           *)
(*****************************************************************************)

sharing type
  LEX.lexan
= VALUEOPS.lexan
= TYPETREE.lexan
= PARSETREE.lexan

sharing type
  LEX.pretty
= TYPETREE.pretty
= PARSETREE.pretty
= PRETTY.pretty
= EXPORTTREE.pretty

sharing type
  CODETREE.codetree
= VALUEOPS.codetree
= PARSETREE.codetree
= STRUCTVALS.codetree

sharing type
  STRUCTVALS.types
= VALUEOPS.types
= TYPETREE.types
= PARSETREE.types
= EXPORTTREE.types

sharing type
  STRUCTVALS.values
= VALUEOPS.values
= PARSETREE.values
= TYPETREE.values

sharing type
  STRUCTVALS.typeId
= TYPETREE.typeId
= PARSETREE.typeId

sharing type
  STRUCTVALS.structVals
= VALUEOPS.structVals
= TYPETREE.structVals

sharing type
  STRUCTVALS.valAccess
= VALUEOPS.valAccess

sharing type
  STRUCTVALS.typeConstrs
= VALUEOPS.typeConstrs
= TYPETREE.typeConstrs
 
sharing type
  STRUCTVALS.env 
= PARSETREE.env

sharing type
  UNIVERSALTABLE.univTable
= STRUCTVALS.univTable

sharing type
  STRUCTVALS.fixStatus
= PARSETREE.fixStatus
= VALUEOPS.fixStatus

sharing type
  STRUCTVALS.signatures
= VALUEOPS.signatures

sharing type
  STRUCTVALS.functors
= VALUEOPS.functors

) : STRUCTURESSIG =

(*****************************************************************************)
(*                  STRUCTURES functor body                                  *)
(*****************************************************************************)
struct
  open Misc; 
  open PRETTY;
  
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

  | StructureIdent of structureIdentForm (* A structure name *)

  | StructDec      of (* struct ... end *)
      { alist: structs list, (* List of items in it. *) location: location,
        value: univTable (* Value *), localIds: typeId list ref }

  | SignatureDec   of sigBind list * location  (* List of signature decs *)

  | SignatureIdent of string * location  (* A signature name *)

  | SigDec         of structs list *location (* sig ... end *)

  | ValSig         of (* Signature of a value. *)
        { name: string * location, typeof: typeParsetree, line: location }

  | ExSig          of (* Signature of an exception.  May be a nullary exception. *)
        { name: string * location, typeof: typeParsetree option, line: location }

  | FunctorDec     of functorBind list * location       (* List of functor decs. *)
  | FunctorAppl    of functorApplForm    (* Appln of a functor *)

  | Singleton      of (* Any other decln. *)
    {
        dec:   parsetree,           (* The value *)
        vars:  coreDeclaration list ref, (* The declarations *)
        typeIds: typeId list ref, (* Type IDs created in the declaration. *)
        location: location
    }

  | FormalArg      of formalArgStruct    (* Functor arg. *)
  | Sharing        of shareConstraint    (* Sharing constraints. *)
  | WhereType	   of whereTypeStruct    (* type realisation. *)
  | Localdec       of localdecStruct     (* Local/Let. *)
  | IncludeSig     of structs list       (* Include. *)
  | SigConstraint  of                    (* Constraint of str to match sig. *)
       {
          str: structs,  (* Structure to constain *)
		  csig: structs, (* Constraining signature *)
          opaque: bool,   (* True if opaque, false if transparent. *)
          sigLoc: location
	   }
  | EmptyStruct                          (* Error cases. *)

  (* List of structures. *)
  withtype structBind =
      {
        name:      string,         (* The name of the structure *)
        nameLoc:   location,
        sigStruct: (structs * bool * location) option,
                                   (* Its signature and whether it's opaque. *)
        value:     structs option,        (* And its value *)
        valRef:    structVals ref, (* The structure variable declared. *)
        typeIds:   typeId list ref,
        line:      location
      }
   (* The constraint could be removed from here and instead the parser could
      desugar the structure binding.  i.e. structure S: SIG = STREXP becomes
      structure S = STREXP: SIG.  structBind is also used for structures
      within signatures where this wouldn't work so a separate data structure
      for structure specifications would be needed. *)

  and sigBind =
      {
        name:      string, (* The name of the signature *)
        nameLoc:   location,
        sigStruct: structs,(* Its value *)
		sigRef:    signatures ref, (* The "value" of the signature. *)
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
        fullLoc: location       (* The location of the full application. *)
      }

  (* Functor binding. *)
  and functorBind =
      {
        name:      string,
        nameLoc:   location,
        sigStruct: (structs * bool * location) option,
        body:      structs,
        arg:       structs,
        valRef:    functors ref,    (* The functor variable declared. *)
        line:      location
      } 

  and formalArgStruct =
      {
        name:      string,
        sigStruct: structs,
        valRef:    structVals ref
      } (* The structure variable. *) 

  and shareConstraint =
      {
        isType: bool,
        shares: string list,
        line:   location
      } 

  and whereTypeStruct =
      {
        sigExp: structs,
		typeVars: types list,
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
      
    and topdec = structs list * location
  
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
    
    val mkStructureDec = StructureDec;
    
    fun mkStructureBinding ((name, nameLoc), signat, value, fullLoc) = 
        { 
          name      = name,
          nameLoc   = nameLoc,
          sigStruct = signat,
          value     = value,
          valRef    = ref undefinedStruct,
          typeIds   = ref [],
          line      = fullLoc
        };
  
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
          localIds = ref nil
        };
  
    val mkSignatureDec = SignatureDec;
  
    fun mkSignatureBinding ((name, nameLoc), sg, ln) =
        { 
          name     = name,
          nameLoc  = nameLoc,
          sigStruct = sg,
          line      = ln,
		  sigRef   = ref undefinedSignature
        };
  
    val mkSigIdent = SignatureIdent;
  
    val mkSig = SigDec;
  
    fun mkTopLevel (dec, location) =
        Singleton
        {
            dec   = dec,
            vars  = ref [],
            typeIds = ref [],
            location = location
        };
  
    val mkFunctorDec = FunctorDec;
  
    fun mkFunctorBinding (name, nameLoc, signat, body, arg, line) =
        {
          name      = name,
          nameLoc   = nameLoc,
          sigStruct = signat,
          body      = body,
          arg       = arg,
          valRef    = ref undefinedFunctor,
          line      = line
        };

    fun mkFunctorAppl (name, arg, nameLoc, fullLoc) =
      FunctorAppl
        {
          name   = name,
          arg    = arg,
          valRef = ref undefinedFunctor,
          nameLoc = nameLoc,
          fullLoc = fullLoc
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
      FormalArg
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
	   SigConstraint{str=str, csig=csig, opaque=opaque, sigLoc=sigLoc}
      
  (*  end; structs abstype *)

  (* Pretty printing *)

    fun displayStructs 
        (strs : structs list, 
         depth : int
        ) : pretty =
    let (* Prints a list of items. *)
    fun displayList ([], separator, depth) dodisplay = []
    
      | displayList ([v], separator, depth) dodisplay =
            if depth <= 0
            then [PrettyString "..."]
            else [dodisplay (v, depth)]
      
      | displayList (v::vs, separator, depth) dodisplay =
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

    fun display (str, depth) =
        if depth <= 0 (* elide further text. *)
        then PrettyString "..."

        else
        case str of
            StructureDec (structList : structBind list, _) =>
		    let
			    fun displayStructBind (
					    {name, sigStruct, value, ...}: structBind, depth) =
		            PrettyBlock (3, false, [],
		                PrettyString name ::
                        (
                            case sigStruct of (* Signature is optional *)
                                NONE => []
                            |   SOME (sigStruct, opaque, _) =>
                                [
    		                        PrettyString (if opaque then " :>" else " :"),
    		                        PrettyBreak (1, 0),
    		                        display (sigStruct, depth - 1)
                                ]
                        ) @
                        (
                            (* Structure value is missing in a signature. *)
                            case value of
                                NONE => []
		                    |   SOME value =>
                                [
    		                        PrettyString " =",
    		                        PrettyBreak (1, 0),
    		                        display (value, depth - 1)
                                ]
                        )
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
                displayList (alist, "", depth) display @
                [ PrettyBreak (1, 0), PrettyString "end"]
            )

      | SignatureDec (structList : sigBind list, _) =>
        let
			fun displaySigBind ({name, sigStruct, ...}: sigBind, depth) =
		        PrettyBlock (3, false, [],
                    [
		                PrettyString (name ^ " ="),
		                PrettyBreak (1, 0),
		                display (sigStruct, depth - 1)
                    ]
                )
		in 
            PrettyBlock (3, false, [],
                PrettyString "signature" ::
                PrettyBreak (1, 0) ::
                displayList (structList, "and", depth) displaySigBind
            )
        end

      | SignatureIdent (name : string, _) =>
            PrettyString name

      | SigDec (structList : structs list, _) =>
            PrettyBlock (1, true, [],
                PrettyString "sig" ::
                PrettyBreak (1, 0) ::
                displayList (structList, "", depth) display @
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

      | FunctorDec (structList : functorBind list, _) =>
        let
			fun displayFunctBind (
					{name, arg, sigStruct, body, ...}: functorBind, depth) =
		        PrettyBlock (3, false, [],
                    PrettyString (name ^ "(") ::
		            PrettyBreak (1, 0) ::
		            PrettyBlock (3, true, [], [ display (arg, depth - 1) ]) ::
		            PrettyString ")" ::
                    (
		                case sigStruct of
                            NONE  => [] (* Signature is optional *)
                        |   SOME (sigStruct, opaque, _) =>
                            [
                                PrettyString(if opaque then " :>" else " :"),
    		                    PrettyBreak (1, 0),
    		                    display (sigStruct, depth - 1)
                            ]
		            ) @
		            [
                        PrettyBreak (1, 0),
		                PrettyString "=",
		                PrettyBreak (1, 0),
		                display (body, depth - 1)
                    ]
                )
		in 
            PrettyBlock (3, false, [],
                PrettyString "functor" ::
                PrettyBreak (1, 0) ::
                displayList (structList, "and", depth) displayFunctBind
            )
        end

      | FunctorAppl {name, arg, ...} =>
            PrettyBlock (1, false, [],
                [
                    PrettyString (name ^ "("),
                    PrettyBreak (0, 0),
                    display (arg, depth),
                    PrettyBreak (0, 0),
                    PrettyString ")"
                ]
            )

      | FormalArg {name, sigStruct, ...} =>
            PrettyBlock (1, false, [],
                (
                    if name = "" then []
                    else [ PrettyString (name ^ " :"), PrettyBreak (1, 2)]
                ) @
                [display (sigStruct, depth - 1)]
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
                display (sigExp, depth) ::
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
                displayList (decs, ";", depth - 1) display @
                [ PrettyBreak (1, 0), PrettyString "in", PrettyBreak (1, 0)] @
                displayList (body, ";", depth - 1) display @
                [ PrettyBreak (1, 0), PrettyString "end" ]
            )

      | IncludeSig (structList : structs list) =>
            PrettyBlock (3, true, [],
                PrettyString "include" ::
                PrettyBreak (1, 0) ::
                displayList (structList, "", depth - 1) display
            )

      | Singleton {dec, ...} =>
          ptDisplay (dec, depth - 1)

      | SigConstraint{str, csig, opaque, ...} =>
            PrettyBlock (0, false, [],
                [
                    display (str, depth - 1),
                    PrettyString (if opaque then " :>" else " :"),
                    PrettyBreak (1, 0),
                    display (csig, depth - 1)
                ]
            )

      | EmptyStruct =>
          PrettyString "<bad>"
    in
        PrettyBlock(0, true, [],
            displayList (strs, "", depth) display
        )
    end (* displayStructs *);
 
    fun displayTopdec((sl, _), d) = displayStructs (sl, d)
    
    
    fun structExportTree(navigation, s: structs) =
    let
         (* Common properties for navigation and printing. *)
        val commonProps =
            PTprint(fn d => displayStructs([s], d)) ::
            exportNavigationProps navigation

        fun asParent () = structExportTree(navigation, s)
    in
        case s of
            StructureDec(sbl, location) =>
            let
                fun exportSB(navigation, sb as {name, nameLoc, sigStruct, value, line, ...}) =
                    let
                        fun exportThis () = exportSB(navigation, sb)
                        (* Three groups: name, signature and structures.
                           It's all complicated because the signature and/or
                           the structure may not be present. *)
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
                            val next =
                                case value of
                                    NONE => NONE
                                |   SOME _ => SOME getValue
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
                            val str = valOf value
                        in
                            structExportTree({parent=SOME exportThis, previous=SOME previous, next=NONE}, str)
                        end
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

        |   FunctorDec(fbl, location) =>
            let
                fun exportFB(navigation, fb as {name, sigStruct, arg, body, line, ...}) =
                    let
                        val sbProps = exportNavigationProps navigation
                        fun exportThis () = exportFB(navigation, fb)
                        (* TODO: Visit the functor name here? *)
                        val sigEntry: structs list =
                            case sigStruct of
                                NONE => []
                            |   SOME (sigStruct, _, _) => [sigStruct]
                        val asChild = exportList(structExportTree, SOME exportThis) (arg :: (sigEntry @ [body]))
                    in
                        (line, asChild @ sbProps)
                    end

                val expChild = exportList(exportFB, SOME asParent) fbl
            in
                (location, expChild @ commonProps)
            end

        |   FunctorAppl _ => (nullLocation, commonProps)

        |   Singleton {dec, ...} => (* A value parse-tree entry. *)
                getExportTree(navigation, dec)

        |   FormalArg _ => (nullLocation, commonProps)

        |   Sharing _ => (nullLocation, commonProps)

        |   WhereType _ => (nullLocation, commonProps)

        |   Localdec {decs, body, line, ...} =>
                (line, exportList(structExportTree, SOME asParent) (decs @ body) @ commonProps)

        |   IncludeSig _ => (nullLocation, commonProps)

        |   SigConstraint _ => (nullLocation, commonProps)

        |   EmptyStruct => (nullLocation, commonProps)
 
    end

    fun structsExportTree (parentOpt, trees) =
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
                structExportTree(
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
            (location, parentTree @ [PTfirstChild(getEntry(sl, NONE)), PTprint(fn d => displayStructs(sl, d))])
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
            context = SOME(displayStructs ([near], errorDepth))
        }
    end;

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
                displayStructs ([sVal], errorDepth lex)
            ]
        )

    (* Error message routine for lookupType and lookupStructure. *)
    fun giveError (sVal : structs, lno : LEX.location, lex : lexan) : string -> unit =
        fn (message : string) => errorNear (lex, true, sVal, lno, message);

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
        fun copyATypeConstr tcon =
            copyTypeConstr
                (tcon, isSome o targMap, (* Copy it if it is in the map. *)
                    valOf o targMap, fn x => x, "")
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
        val tvars : types list = tcTypeVars target; (* either will do *)
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
     polymorphism than the target.  Along with the candidate and target
     signatures we also pass a map which is applied to the
     type IDs before the matching process. *)

  type typeIdMap =
    {
      lookup: int -> typeId option,
      enter:  int * typeId -> unit
    };

  fun matchSigs 
       (candidate     : signatures,
        target        : signatures,
        { lookup = lookupType, enter = enterType} : typeIdMap, 
        near,
        lno           : LEX.location,
        lex           : lexan
       ) : unit =
  let
    (* Match names (unique ids) for types. This is slightly more
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
     matchVals (candidate, target)
  end (* matchSigs *);

    fun typeMatchTab (v: typeId option Array.array, minOffset: int, maxOffset: int) : typeIdMap =
    { 
        lookup =
            fn id =>
                if id < minOffset orelse id >= maxOffset
                (* It is possible for the offset to be >= size if the type
                   is being shared inside the result signature of a functor. It
                   will only happen for types inside structures which are shared.
                   ??? Is this still possible ??? *)
                then NONE (* Not present if it isn't in the range. *)
                else (* Must be a bound stamp. *) Array.sub (v, id),
            
        enter = fn (id, value) => Array.update (v, id, SOME value)
    }

  type tsvEnv = { enterType:   string * typeConstrs -> unit,
                  enterStruct: string * structVals  -> unit,
                  enterVal   : string * values      -> unit };
  
  fun tsvEnv (Env E) = {enterType   = #enterType   E,
                        enterStruct = #enterStruct E,
                        enterVal    = #enterVal    E};

    (* Copy the signature so that types in different signatures are distinct.
       This is actually only used for structures. *)
    fun copySig 
        (source       : signatures,
         wantCopy     : int -> bool,
         mapTypeId    : int -> typeId,
         startValues  : int,
		 strName	  : string)
        : signatures = 
    let
      (* Make a new signature. *)
      val tab = makeSignatureTable ();
      (* Copy everything into the new signature. *)
      val lastAddr =
              fullCopySig 
                (startValues, source,
                {
                  enterType   = fn (s,v) => univEnter (tab, typeConstrVar, s, v),
                  enterStruct = fn (s,v) => univEnter (tab, structVar,     s, v),
                  enterVal    = fn (s,v) => univEnter (tab, valueVar,      s, v)
                },
                wantCopy, mapTypeId, strName);
    in
        (* TODO: What should sigMaxTypes be here?  It depends on what mapTypeId returns. *)
	    makeSignature(sigName source, tab, 0, 0 (* sigMaxTypes source - sigMinType source *), sigDeclaredAt source, mapTypeId)
    end (* copySig *)

    (* Generate new entries for all the elements of the signature. *)
    and fullCopySig 
        (offset        : int, 
         source        : signatures,
         resEnv        : tsvEnv,
         wantCopy      : int -> bool,
         mapTypeId     : int -> typeId,
		 strName	   : string) 
        : int =
    let
        fun copyTypeCons (tcon : typeConstrs) : typeConstrs =
            copyTypeConstr (tcon, wantCopy, mapTypeId, fn x => x, strName);

        fun copyTyp (t : types) : types =
            copyType (t, fn x => x, (* Don't bother with type variables. *) copyTypeCons);

	    (* First copy the type constructors in this signature and any substructures.
	      It's inefficient but harmless to do this again for substructures.
	      TODO: Tidy this up. *)
	    val () = copyTypeConstructors(source, wantCopy, mapTypeId, strName)
    in
    univFold
     (sigTab source,
      (fn (dName: string, dVal: universal, num) =>
        (if tagIs structVar dVal
         then let
           val oldStruct = tagProject structVar dVal;
           val oldSig     = structSignat oldStruct;
           
           (* Make a new sub-structure. *)
           val newSig = copySig (oldSig, wantCopy, mapTypeId, 0, strName ^ dName ^ ".");
               
           val addr = 
             if isFormal (structAccess oldStruct) 
              then vaFormal (structAccess oldStruct) + offset
              else num (* From   sig ... open Global; ... end *);
              
           val newStruct =
             makeFormalStruct (structName oldStruct, newSig, addr, structLocations oldStruct);
         in
           #enterStruct resEnv (dName, newStruct);
           Int.max(num, addr+1)
         end (* structures *)
                 
         else if tagIs typeConstrVar dVal
         then let (* Types *)
		  val address = ref num
          (* Make a new constructor. *)
           val oldConstr = tagProject typeConstrVar dVal;
           
           (* 
              The new type constructor will use the NEW polymorphic
              type variables. This is because copyTypeCons uses the
              table built by matchSigs which maps OLD constructors to
              NEW ones, and the NEW constructors contain NEW type variables.
           *)
           val newConstr = copyTypeCons oldConstr;
           
           (* We must copy the datatype if any of the value
              constructors have to be copied. The datatype may
              be rigid but some of the value constructors may
              refer to flexible type names. *)
           val mustCopy = ref (not (identicalConstr (newConstr, oldConstr)));
           
           local
             val oldTypeVars : types list = tcTypeVars oldConstr;
             val newTypeVars : types list = tcTypeVars newConstr;
(* 
   We CAN legitimately get different numbers of type variables here,
   it we're trying to recover from a user error that we've already
   diagnosed. We'll just ignore the extra variables. SPF 26/6/96
*)
             fun zipTypeVars (x::xs) (y::ys) = (x, y) :: zipTypeVars xs ys
               | zipTypeVars _  _   = []
                 
             val typeVarTable : (types * types) list = 
               zipTypeVars oldTypeVars newTypeVars;
             
             fun copyTypeVar (t : types) : types =
             let
               fun search [] = t
                 | search ((oldTypeVar, newTypeVar) :: rest) =
                    if sameTypeVar (t, oldTypeVar) then newTypeVar else search rest
             in
               search typeVarTable
             end;
           in
             (* 
                 Dave was wrong - we DO need to copy the polymorphic type variables -
                  at least, we do here! This version hides the old version of
                  copyTyp, which is in the enclosing environment. The entire
                  type/signature matching code needs a thorough overhaul.
                  SPF 16/4/96
             *)
			 (* TODO: If SPF is right we also need to redefine
			 	copyTypeCons. DCJM 17/2/00.  *)
             fun copyTyp (t : types) : types =
               copyType (t, copyTypeVar, copyTypeCons);
           end;
           
           (* 
              Now copy the value constructors. The equality status
              and any equivalence (i.e. type t = ...) will have been
              processed when the constructor was copied.
              
              What's going on here? Copying the type constructor will
              use the NEW polymorphic variables, but copying the rest of
              the type will use the OLD ones, since copyTyp doesn't copy
              individual type variables - what a MESS! I think this means
              that we end up with OLD variables throughout.
              SPF 15/4/96
           *)
           val copiedConstrs =
             map 
              (fn (v as Value{name, typeOf, class, access, locations}) =>
               let
                 (* Copy its type and make a new constructor if the type
                    has changed. *)
                 val newType = copyTyp typeOf;
                 val typeChanged  = not (identical (newType, typeOf));
				 val (newAccess, addressChanged) =
				 	case access of
						Formal addr =>
						let
							val newAddr = addr+offset
						in
							address := Int.max(newAddr+1, !address);
							(Formal newAddr, offset <> 0)
						end
					  | access => (access, false)
				 (* If this datatype shares with another one we will already have
				    constructors available.  This can happen, in particular, if
					we have a signature constraining the result of a structure.
					There will be sharing between the datatype in the implementing
					structure and the result signature. *)
                 val copy =
                   if typeChanged orelse addressChanged
                   then let
					 val v' =
                        Value{name=name, typeOf=newType, class=class,
                              access=newAccess, locations = locations}
					 (* See if the constructor already exists. *)
                   in
				     let
					 	val original = findValueConstructor v'
					 in
					 	(* We try to use the original if it is global since that
						   allows us to print values of the datatype.  If it is
						   not global we MUSTN'T use the copy.  It may be local
						   and so may not exist later on. *)
					    case original of
							Value{access=Global _, ...} => original
						|	_ => v'
					 end
                   end
                   else v;
               in
                 if typeChanged orelse addressChanged then mustCopy := true else ();
                 copy (* Return the copy. *)
               end)
              (tcConstructors oldConstr);
          in
            if !mustCopy
            then let
              (* If the copied datatype already has constructors on it
                 we must have two datatypes which share. They need not
                 necessarily have the same constructors e.g. datatype 
                 t = X of int t   can share with datatype t = X of int * int
                 or even with datatype t = X of bool . We have to make a new
                 type constructor in that case. We don't need to put this
                 in the typeMap table because we can always return the
                 type that is already in there. This will also work correctly
                 if we have a type constructor which does not itself need to
                  be copied (e.g. it is rigid) but at least one of whose
                  value constructors involves a flexible type. Another  
                  case could be where we have a structure containing a datatype.
                  The type in the signature may be either a datatype or a type. *)
                  
              val newType =
                if not (null (tcConstructors newConstr))
                then (* Matched to a datatype. Use the NEW types throughout *)
                  makeDatatypeConstr (* Necessary? *)
                      (tcName newConstr, tcTypeVars newConstr,
                       tcIdentifier newConstr, 0, tcLocations newConstr)
                else newConstr;
            in
              (* Put the new constructors on the result type *)
              if not (null copiedConstrs)
              then tcSetConstructors (newType, copiedConstrs)
              else ();
              (* and put it into the table. *)
              #enterType resEnv (dName, newType)
            end
            else #enterType resEnv (dName, newConstr);
            
            Int.max(num, !address)
          end
            
          (* Finally the values and exceptions. *)
          else if tagIs valueVar dVal
            then let
              val v = tagProject valueVar dVal;
            in
			  case v of
			   Value {typeOf=oldType, class, name, access=Formal addr, locations, ...} =>
				    let
	                  val newType = copyTyp oldType;
	                  val newAddr = addr + offset;
	                  
	                  (* Make a new entry if the address or type have changed. *)
	                  val res =
	                    if addr <> newAddr orelse not (identical (newType, oldType))
	                    then mkFormal (name, class, newType, newAddr, locations)
	                    else v;
	                in
	                  #enterVal resEnv (name, res);
	                  Int.max(num, newAddr+1)
	                end

			  | Value {typeOf, class, name, access, locations, ...} =>
			  	    (* Values in the result signature of a structure may be globals
					   as a result of a call to extractValsToSig.  This applies
					   if we have a functor which returns a global structure
					   e.g. structure S = ...; functor F() = S.
					   We still have to consider the possibility that the types might
					   be different due to an opaque signature e.g. structure S1 :> SIG = S2. *)
				    let
	                  val newType = copyTyp typeOf;
	                  (* Can save creating a new object if the address and type
					     are the same as they were. *)
	                  val res =
	                    if not (identical (newType, typeOf))
	                    then Value {typeOf=newType, class=class, name=name,
                                    access=access,locations=locations}
	                    else v
	                in
	                  #enterVal resEnv (name, res);
					  num
	                end
            end 
          else num
        ) 
      ),
      offset
     )
  end (* fullCopySig *)

  (* Make entries for all the type constructors.  The only reason for
     doing this separately from fullCopySig is to try to ensure that the
	 names we give the types are appropriate.  If we do this as part of
	 fullCopySig we could get the wrong name in cases such as
	 sig structure S: sig type t end structure T : sig val x: S.t end end.
	 If fullCopySig happens to process "x" before "S" it will copy "t"
	 and give it the name "T.t" rather than "S.t". *)
    and copyTypeConstructors(source: signatures, mustCopyType: int -> bool, typeMap: int -> typeId, strName: string): unit =
    let
        fun copyTypeCons (tcon : typeConstrs) : typeConstrs =
            copyTypeConstr (tcon, mustCopyType, typeMap, fn x => x, strName);
    in
    univFold
     (sigTab source,
      (fn (dName: string, dVal: universal, ()) =>
        (if tagIs structVar dVal
         then let
           val oldStruct = tagProject structVar dVal;
           val oldSig     = structSignat oldStruct;
		 in
		   copyTypeConstructors(oldSig, mustCopyType, typeMap, strName ^ dName ^ ".")
         end (* structures *)
                 
         else if tagIs typeConstrVar dVal
         then let (* Types *)
          (* Make a new constructor.  It will be entered in the match table
		     and picked up when we copy the signature. *)
           val oldConstr = tagProject typeConstrVar dVal;
           val newConstr = copyTypeCons oldConstr
           fun idNumber tc =
            case tcIdentifier tc of
                Bound { offset, ...} => "(" ^ Int.toString offset ^ ")"
            |   _ => "(Not bound)"
           val _ = if tcEquality oldConstr andalso (isBoundId(tcIdentifier newConstr) orelse isFreeId(tcIdentifier newConstr))
                        andalso not (tcEquality newConstr)
           then TextIO.print (concat["Equality mismatch ", tcName oldConstr, idNumber oldConstr, 
                                " ", tcName newConstr, idNumber newConstr, "\n"])
           else ();
          in
           ()
          end
            
		else ()
        ) 
      ),
      ()
     )
	 end;


  val makeEnv = fn x => let val Env e = makeEnv x in e end;  

  (* Second pass - identify names with values and type-check *)
  fun pass2Structs ((strs, _): topdec, lex : lexan, Env env : env) : unit =
  let 
    fun pass2Struct 
        (strs     : structs list,
         makeTypeId : bool -> bool -> typeId,
         makeCurrentTypeCount: unit -> int,
         Env env  : env, 
         lno      : LEX.location,
		 strName  : string
         ) : unit =
    let
      (* Get the value from a signature-returning expression
         (either the name of a signature or sig ... end.
         The type IDs in the signature are bound names. *)
      fun sigVal 
            (str           : structs,
             initTypeId    : int,
             Env globalEnv : env,
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
                    errorMsgNear (lex, true, near, lno,
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
				    lookupStructureDirectly 
				      ("Structure" ,
				       {lookupStruct = #lookupStruct tEnv}, 
				       name,
				       lookupFailure);
					in
					  structSignat subStr
					end
              in  (* Now share all these signatures. *)
                shareStructures (map getStructSignat tlist, line)
              end
        end (* applySharingConstraint *);

(**************************** End of SPF's rewrite *************************)

        (* Look up a signature. Strictly a signature cannot be contained
           in a structure but this allows a structure to be used as a
           general name space. *)
        fun lookSig (name : string, lno : LEX.location) : signatures =
        let
          val errorFn    = giveError (str, lno, lex);
          fun lookupFn s = #lookupSig (makeEnv(sigTab (structSignat s)));
        in
          lookupAny 
            (name,
             #lookupSig globalEnv,
             #lookupStruct globalEnv, 
             lookupFn,
             "Signature",
             undefinedSignature,
             errorFn)
        end

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
				     errorNear (lex, true, str, lno, 
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
            in
				(* Copy the signature into the result. *)
				fullCopySig(offset, sourceSig, tsvEnv (Env structEnv), 
                        fn _ => true, typeMap, strName)
            end
  
          | SigDec (sigsList : structs list, _) =>  (* sig .... end *)
          let
            (* Process the entries in the signature and allocate an address
               to each. *)
            fun processSig (signat, offset : int, lno : LEX.location) : int =
              case signat of
                StructureDec (structList : structBind list, _) =>
                let
                  (* Each element in the list should be a structure binding. *)
                  fun pStruct [] offset = offset
                    | pStruct (({name, sigStruct = SOME(sigStruct, _, _), line, ...}: structBind) :: t) offset =
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
                    | pStruct ({sigStruct = NONE, ...} :: _) offset =
                        raise InternalError "Structure in signature with no signature"
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
                
              | Singleton {dec, ...} =>
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
      fun structValue(str, newTypeId: bool->bool->typeId, currentTypeCount, Env env, lno, strName) =
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
                
        | FunctorAppl {name, arg, valRef, nameLoc, fullLoc} =>
          (* The result structure must be copied to generate a new
             environment. This will make new types so that different
             applications of the functor yield different types. There may be 
             dependencies between the parameters and result signatures so
             copying may have to take that into account. *)
          let 
            (* Look up the name, and copy the structure. Strictly functors
               cannot be contained in structures but this allows structures
               to be used as general name spaces. *)
            val functr: functors =
              lookupAny 
                (name,
                 #lookupFunct env, 
                 #lookupStruct env,
                (fn s => #lookupFunct (makeEnv(sigTab (structSignat s)))),
                "Functor",
                undefinedFunctor,
                giveError (str, nameLoc, lex));
          in
            if isUndefinedFunctor functr
            then undefinedSignature
            else
            let
                val () = valRef := functr; (* save it till later. *)
              
                val resultSig : signatures = functorResult functr;
                val argStruct : structVals = functorArg functr;

                val v = Array.array (Int.max(0, sigMaxTypes resultSig), NONE);
                val typeMap: typeIdMap =
                    typeMatchTab (v, sigMinTypes resultSig, sigMaxTypes resultSig);
               
                (* Get the actual parameter value. *)
                val argSig = structValue(arg, newTypeId, currentTypeCount, Env env, fullLoc, "");

                (* Check that the actual arguments match formal arguments,
                   and instantiate the variables. *)
                val () = matchSigs (argSig, structSignat argStruct, typeMap, str, fullLoc, lex);

                (* The array contains SOME entries for type IDs that are present in
                   the argument and passed to the result.  We need to create new entries
                   for all the others. *)
                fun fillTable n =
                    if n >= sigMaxTypes resultSig
                    then ()
                    else
                    (
                        case Array.sub(v, n) of
                            NONE =>
                                Array.update(v, n, SOME(newTypeId false (isEquality(sigTypeIdMap resultSig n))))
                        |   SOME _ => (); (* Already there. *)
                        fillTable (n+1)
                    )
                val () = fillTable(sigMinTypes resultSig)
            in
                (* Finally copy the result signature, incorporating the actual
                   args, and creating new stamps for any generative stamps
                   (i.e. stamps that were not put in the table before. *)
                copySig (resultSig, fn _ => true, fn n => valOf(Array.sub(v, n)), 0, strName)
            end
          end
                           
        | StructDec {alist, value = structTable, location, localIds, ...} =>
          let
            (* Collection of declarations packaged into a structure
                or a collection of signatures. *)
            (* Some of the environment, the types and the value constructors,
               is generated during the first pass. Get the environment from
               the structure. *)
            val structEnv = makeEnv structTable;
            
            (* The result of processing these declarations are two "signatures", one
               aassociated with the "value" field of the StructDec and a second "resultSig"
               is returned as the result.
               The contents of the two are much the same with one crucial difference:
               values and structure values in the "value" signature represent the source
               for the values (often Local declarations but possibly Selected or Globals)
               whereas the entries in the "resultSig" are a sequence of Formal entries.
               The purpose of constructing these is to provide a default signature if
               there is no explicit signature. *)
            val initialBoundCount = currentTypeCount()
            val resultTable = makeSignatureTable();
            val resultEnv = makeEnv resultTable
            val addrs = ref 0;
            val formalTypeIds = ref []
            
            fun makeLocalTypeId isVar isEq =
            let
                val newId = newTypeId isVar isEq
	            val a = !addrs;
                val () = addrs := a + 1
                val formalId =
                    case newId of
                        Free{ uid, allowUpdate, ...} =>
                            Free { uid=uid, allowUpdate=allowUpdate, access = ref(Formal a)}
                    |   Bound { offset, eqType, ... } =>
                            Bound { offset = offset, eqType = eqType, access = Formal a}
                    |   _ => raise InternalError "makeLocalTypeId: Not Bound or Free"
            in
                localIds := newId :: ! localIds;
                formalTypeIds := formalId :: ! formalTypeIds;
                newId
            end
            
            (* This environment receives the declarations from calling pass2. *)
            val newEnv = 
              {
                enterType =
                  fn (pair as (name,v)) =>
                    (
                     #enterType structEnv pair;
                     #enterType resultEnv pair
                    ),
                    
               (* Should never occur. *)
                enterSig = 
                  fn (pair as (name,v)) =>
                    (
                     #enterSig  structEnv pair;
                     #enterSig  resultEnv pair
                    ),
                    
               (* Never occurs in normal ML, might happen if we opened a
                  structure that was actually a name space. *)
               enterFunct =
                  fn (pair as (name,v)) =>
                   (
                     #enterFunct structEnv pair;
                     #enterFunct resultEnv pair
                   ),
                   
               (* Enter the value in the environment of the struct..end,
                  but turn local declarations  or selections from other
                  structures into entries for the result vector. *)
              enterVal =
                  fn (pair as (name,v)) =>
                  (
                    #enterVal structEnv pair;

                    case v of
						Value{access=Overloaded _, ...} =>
							#enterVal resultEnv pair (* Just copy. *)
					|	Value{class, typeOf, locations, ...} =>
	                    let (* Local or selected. *)
	                      val isVal = not (isConstructor v);
	                        
	                      val a = !addrs;
	                    in
	                      #enterVal resultEnv (name, mkFormal (name, class, typeOf, a, locations));
	                      addrs := a + 1
	                    end
                  ),
                  
              (* Make entries in the result vector. *)
              enterStruct =
                fn (pair as (name, str)) => 
                let
                  val U = #enterStruct structEnv pair;
                  val resSig = structSignat str;
                  val locations = structLocations str
                  val a = !addrs;
                in
                  #enterStruct resultEnv (name, makeFormalStruct (name, resSig, a, locations));
                  addrs := a + 1
                end,
                
              lookupVal =
                lookupDefault (#lookupVal    structEnv) (#lookupVal    env),
                
              lookupType =
                lookupDefault (#lookupType   structEnv) (#lookupType   env),
                
              lookupStruct =
                lookupDefault (#lookupStruct structEnv) (#lookupStruct env),
                 
              lookupSig    = #lookupSig   env, (* Global *)
              
              lookupFunct  = #lookupFunct env, (* Global *)
              
              lookupFix    = #lookupFix   env,

			  (* Fixity declarations are dealt with in the parsing process.  They
			     are only processed again in this pass in order to get declarations
				 in the right order. *)
              enterFix     = fn _ => ()
            };
            
            (* process body of structure *)
            val () = pass2Struct (alist, makeLocalTypeId, currentTypeCount, Env newEnv, lno, strName);
            (* Save the list of the type IDs created in this structure. *)
            val v = Vector.fromList(List.rev(!formalTypeIds))
          in
            makeSignature("", resultTable, initialBoundCount, currentTypeCount(), location,
                fn n => Vector.sub(v, n-initialBoundCount))
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
              pass2Struct (decs, newTypeId, currentTypeCount, Env localEnv, line, strName);
                   
          in
            (* There should just be one entry in the "body" list. *)
            structValue(localStr, newTypeId, currentTypeCount, Env localEnv, line, strName)
          end
		  
        | SigConstraint { str, csig, opaque, sigLoc } =>
          let (* struct: sig or struct :> sig *)
            val resSig = structValue(str, newTypeId, currentTypeCount, Env env, lno, strName);
            (* Create the signature using bound Id values that are distinct from
               those we've previously used but at this stage don't increment
               the counters. *)
            val explicitSig  : signatures = 
              sigVal(csig, currentTypeCount(), Env env, sigLoc, strName);
                     
            (* Make tables to contain the matched ids. *)
            val a = Array.array (Int.max(0, sigMaxTypes explicitSig), NONE) ;
            val typeMap  : typeIdMap =
                typeMatchTab (a, sigMinTypes explicitSig, sigMaxTypes explicitSig);

            (* Match the structure to the signature.  This will fill in entries
               in the array. *)
            val () = matchSigs (resSig, explicitSig, typeMap, str, sigLoc, lex);
              
			(* These maps are used to construct the resultant signature.
			   If we are using transparent matching we use the map constructed
			   from matching the signature to the structure but if we are
			   using opaque matching we need to make new instances of
			   every type. *)
			val resTypeMap = 
				if opaque
				then
                let
                    fun makeNewId n =
                    let
                        val oldId = sigTypeIdMap explicitSig n
                    in
                        (* TODO: We should use the "access" value from the values in the structure. *)
                        newTypeId false(isEquality oldId)
                    end;
                    (* Create new IDs for all the bound IDs in the signature. *)
                    val minOffset = sigMinTypes explicitSig and maxOffset = sigMaxTypes explicitSig
                    val v = Vector.tabulate (maxOffset-minOffset, fn n => makeNewId (n+minOffset))
                in
                    fn id => Vector.sub (v, id + minOffset)
                end
				else (* Transparent: Use the IDs from the structure. *)
                    (* TODO: If we have had an error and a type has not been matched we will
                       still have a NONE here. We need to put something in here to avoid an
                       exception in the compiler. *)
                    fn n => valOf(Array.sub(a, n))
             
          in
             (* Copy the signature to ensure that any "names" from the structure
                value are copied into the result signature.
                These are distinct from those in the surrounding space and,
                because we're now incrementing the counter, distinct from any
                we make anywhere else. *)
             copySig (explicitSig, fn s => s >= sigMinTypes explicitSig, resTypeMap, 0, strName)
         end
                
        | _ =>
           raise InternalError "structValue: not a value"
      end (* structValue *);
      
      fun pass2StructureDec (str : structs, structList : structBind list) : unit =
      let (* Declaration of structures. *)
        (* The declarations must be made in parallel. i.e.
            structure A = struct ... end and B = A; binds B to the A
            in the PREVIOUS environment, not the A being declared. *)
        val sEnv =  (* The new names. *)
          noDuplicates 
            (fn(name, _, _) => 
              errorNear (lex, true, str, lno, 
                         "Structure " ^ name ^ 
                         " has already been bound in this declaration")
            );
         (* Put the new names into this environment. *)
         
        fun pass2StructureBind ({name, sigStruct, value = SOME value, valRef, line, typeIds, ...}) : unit=
          let (* Each element in the list is a structure binding. *)
            (* Keep a list of the local type IDs.  If this is a top-level declaration they have to
               be turned into globals. *)
            fun sbindMakeTypeId isVar eq =
            let
                val newId = makeTypeId isVar eq
            in
                typeIds := newId :: ! typeIds;
                newId
            end;

            val resSig =
				structValue(value, sbindMakeTypeId, makeCurrentTypeCount, Env env, line, strName ^ name ^".");
              
            val resultSig =
                case sigStruct of
                    NONE => resSig (* No signature to match. *)
                |   SOME (sigStruct, opaque, sigLoc) =>
                  let
                   (* Get the explicit signature. If we are inside a functor
                       we have to make any new bound names outside the range
                       we have already used. *)
                    val explicitSig  : signatures = 
                      sigVal(sigStruct, makeCurrentTypeCount(), Env env, sigLoc, strName ^ name ^ ".");
                     
                    (* Make tables to contain the matched ids. *)
                    val a = Array.array (Int.max(0, sigMaxTypes explicitSig), NONE) ;
                    val typeMap  : typeIdMap =
                      typeMatchTab (a, sigMinTypes explicitSig, sigMaxTypes explicitSig);

                    (* Match the signature.  This instantiates entries in typeMap. *)
                    val () = matchSigs (resSig, explicitSig, typeMap, str, sigLoc, lex);    
              
    				(* These maps are used to construct the resultant signature.
    				   If we are using transparent matching we use the map constructed
    				   from matching the signature to the structure but if we are
    				   using opaque matching we need to make new instances of
    				   every type. *)
        			val resTypeMap = 
        				if opaque
        				then
                        let
                            fun makeNewId n =
                            let
                                val oldId = sigTypeIdMap explicitSig n
                            in
                                sbindMakeTypeId false(isEquality oldId)
                            end;
                            (* Create new IDs for all the bound IDs in the signature. *)
                            val minOffset = sigMinTypes explicitSig and maxOffset = sigMaxTypes explicitSig
                            val v = Vector.tabulate (maxOffset-minOffset, fn n => makeNewId (n+minOffset))
                        in
                            fn id => Vector.sub (v, id - minOffset)
                        end
        				else (* Transparent: Use the IDs from the structure. *)
                            fn n => valOf(Array.sub(a, n))
                  in
                    (* And copy it to put in the names from the structure. *)
                    copySig (explicitSig, fn s => s >= sigMinTypes explicitSig, resTypeMap, 0, strName ^ name ^ ".")
                  end;
               
            (* Now make a local structure variable using this signature. *)
            val var = makeLocalStruct (name, resultSig, [DeclaredAt line]);
            
          in
            #enter sEnv (name, var);
            valRef := var
          end

        | pass2StructureBind ({value = NONE, ...}) =
            raise InternalError "Structure binding with no structure"
 
      in 
        List.app pass2StructureBind structList;
        (* Put them into the enclosing env. *)
        #apply sEnv (#enterStruct env)
      end; (* pass2StructureDec *)
      
      fun pass2FunctorDec (s: structs, structList : functorBind list) : unit =
      let
        (* There is a restriction that the same name may not be bound twice.
		   As with other bindings functor bindings happen in parallel.
		   DCJM 6/1/00. *)
        val sEnv =  (* The new names. *)
          noDuplicates 
            (fn (name, _, _) => 
              errorNear (lex, true, s, lno, 
                         "Functor " ^ name ^ 
                         " has already been bound in this declaration")
            );

         (* Put the new names into this environment. *)
        fun pass2FunctorBind
                {name,
                 arg = FormalArg {name = argName, sigStruct = argSig, valRef = argVal},
                 body, sigStruct, valRef, line, ...} =
          let
            (* We must copy the signatures to ensure that arguments with
               the same signature are different. Make an environment to
               contain the arguments. *)
            val argEnv = makeEnv (makeSignatureTable());
             
            (* If it is a "spec" it must be wrapped up in sig...end.
               We can't have a functor declaration in another functor
               (at least in the current definition of ML) so we can 
               start this at 0, rather than !typeNo. *)
            val signat : signatures =
              let
			  	val spec =
					case argSig of
						SignatureIdent _ => argSig
					|	SigDec _ => argSig
					|	WhereType _ => argSig
					|	_ => mkSig([argSig], line (* TODO: What's this? *))
              in
                sigVal (spec, 0, Env env, line, "")
              end;
              
            val resArg = makeLocalStruct (argName, signat, [DeclaredAt line]);
          in (* Put the copied version in. *)
            if argName <> ""
            then #enterStruct argEnv (argName, resArg)
            else
              (* Open the dummy argument. Similar to "open" in treestruct. *)
              univFold
               (sigTab signat,
                (fn (dName, dVal, ()) =>
                   if tagIs typeConstrVar dVal
                     then
                       #enterType argEnv
                         (dName, tagProject typeConstrVar dVal)
                       
                   else if tagIs valueVar dVal
                     then
                       #enterVal argEnv 
                         (dName, mkSelectedVar (tagProject valueVar dVal, resArg, []))
                          
                   else if tagIs structVar dVal
                     then
                       #enterStruct argEnv 
                         (dName, makeSelectedStruct (tagProject structVar dVal, resArg, []))
                         
                   else ()
                 ),
                ()
               );
             
            argVal := resArg;
             
            (* Now process the body of the functor using the environment of
               the arguments to the functor and the global environment. *)
            let
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
               
              (* In sigVal we will have allocated a range of bound stamps
                 for the argument signature. We need to extend the range
                 for stamps in the body. *)
              val typeStamps = ref (sigMaxTypes signat);

              val localStamps = ref []

             
            local
                (* Create local IDs for any datatypes declared in the body or any generative
                   functor applications. *)
                fun newTypeId isVar eq =
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
            in
                val resSig =
                    structValue(body, newTypeId, fn () => !typeStamps, Env envWithArgs, line, strName ^ name ^ "().")
            end;
                      
              val functorSig =
                case sigStruct of
                    NONE => (* No signature to match. *)
                        makeSignature (sigName resSig, sigTab resSig, 0, !typeStamps, sigDeclaredAt resSig, 
                            fn n => if n < sigMaxTypes signat then sigTypeIdMap signat n else sigTypeIdMap resSig n)
                |   SOME (sigStruct, opaque, sigLoc) =>
                 let (* Get the explicit result signature. *)
                  val startTypes = sigMaxTypes signat;
                  
                    val explicitSig : signatures =
                        sigVal (sigStruct, startTypes, Env envWithArgs, line, strName ^ name ^ "().");
                    (* explicitSig is the only circumstances in which sigMinTypes of a signature
                       may be non-zero.  In all other cases it will be zero.  If the explicit
                       signature was simply the name of a signature sigMinTypes will be zero
                       because we don't copy it in sigVal.  If it is anything else it will
                       be startTypes.  When we come to copy the signature we have to maintain
                       any sharing with the argument (as a result of sharing or "where type")
                       and we can only do that if we can distinguish the IDs that have been
                       inherited from resSig from those that are new to explicitSig.  A simple
                       named signature can't share with resSig. *)
                  
                    val minExplicitSig = sigMinTypes explicitSig and maxExplicitSig = sigMaxTypes explicitSig
                    val a = Array.array (Int.max(0, maxExplicitSig), NONE) ;
                    val typeMap = typeMatchTab(a, minExplicitSig, maxExplicitSig);
                     
                    (* Match the signature. *)
                    val () = matchSigs (resSig, explicitSig, typeMap, s, sigLoc, lex);
				    (* These maps are used to construct the resultant signature.
				       If we are using transparent matching we use the map constructed
				       from matching the signature to the structure but if we are
				       using opaque matching we need to make new instances of
				       every type. *)
        			val (resTypeMap, finalMap) = 
        				if opaque
        				then
                        let
                            (* Reset the type stamp count so that type IDs in the result signature
                               follow immediately from those in the args.  There could already be
                               sharing between types in the result with types in the args as a result
                               of explicit "where types" but unlike with transparent matching we
                               don't inherit any type IDs from the functor body. *)
                            val () = typeStamps := startTypes
                            local
                                fun makeNewId n =
                                let
                                    val oldId = sigTypeIdMap explicitSig (n+minExplicitSig)
                                    val nId = !typeStamps
                                    val () = typeStamps := nId + 1;
                                    (* We have one entry in the run-time result vector for each type ID,
                                       starting with a copy of the IDs in argument and following with
                                       each of the generative type IDs. *)
                                    val newId = makeBoundId (Formal(n+startTypes), nId, isEquality oldId)
                                in
                                    newId
                                end
                            in
                                (* Create new IDs for all the bound IDs in the signature. *)
                                val v = Vector.tabulate (maxExplicitSig-minExplicitSig, makeNewId)
                            end
                            (* Create new IDs for the arguments.  They will have the same "offset" and
                               so share with the arguments but the run-time address may be different.
                               Because these are located at the start of the run-time vector the
                               run-time address (argument to Formal) is the same as the "offset". *)
                            val argv =
                                Vector.tabulate(sigMaxTypes signat,
                                    fn n => makeBoundId (Formal n, n, isEquality(sigTypeIdMap signat n)))
                        in
                            (
                            fn n => if n < minExplicitSig then Vector.sub(argv, n) else Vector.sub (v, n - minExplicitSig),
                            fn n => if n < startTypes then Vector.sub(argv, n) else Vector.sub (v, n - startTypes)
                            )
                        end
        				else (* Transparent: Use the IDs from the structure. *)
                        let
                            (* Make a vector for the type IDs created in the body.  We have an entry here for
                               every type ID whether or not it is mentioned directly in the signature.  We
                               could have types that are mentioned indirectly e.g. we have type t = s * s
                               with "s" being local to the functor and only "t" mentioned in the signature.
                               We need to ensure that "t" is generative. *)
                            fun convertId(n, id as Bound { offset, ...}) =
                                    (* Either inherited from the argument or a new type ID. *)
                                    makeBoundId (Formal(n+startTypes), offset, isEquality id)
                            |   convertId (_, id) = id (* Free or TypeFunction. *)
                            local
                                val bodyVec = Vector.fromList(List.rev(!localStamps))
                                fun makeNewId n = convertId(n, Vector.sub(bodyVec, n))
                            in
                                val bodyVec = Vector.tabulate(Vector.length bodyVec, makeNewId)
                            end
                            (* Arguments as for the opaque case. *)
                            val argv =
                                Vector.tabulate(sigMaxTypes signat,
                                    fn n => makeBoundId (Formal n, n, isEquality(sigTypeIdMap signat n)))
                        in
                            (
                            fn n =>
                                (* Normally every ID in the explicit signature will have an
                                   entry in the structure and so each item in "a" will be SOME.
                                   If we have had an error and an entry in the structure is missing
                                   we need to use a default. *)
                                if n < minExplicitSig
                                then Vector.sub(argv, n) 
                                else convertId(n-minExplicitSig, getOpt(Array.sub(a, n), tcIdentifier undefType)),
                            fn n => if n < startTypes then Vector.sub(argv, n) else Vector.sub(bodyVec, n-startTypes)
                            )
                        end
                    (* Now copy bound IDs that don't share with IDs in the argument and replace them either by entries
                       from the functor body or newly created IDs.  This also ensures that when we apply the
                       functor we can distinguish generative IDs from those that are inherited from the argument.
                       Any bound ID with an offset < sigMaxTypes argSig is assumed to be inherited from the
                       argument while new IDs are created for any with offset >= sigMaxTypes argSig. *)
                    val resSig =
                        copySig (explicitSig, fn s => true, resTypeMap,
                            !typeStamps (* Start the run-time offsets after the type IDs*),
                            strName ^ name ^ "().")
                in
                    (* The resulting signature includes both the type IDs from the arguments and generative type
                       IDs from the body (transparent matching) or result signature (opaque matching). *)
                    makeSignature (sigName resSig, sigTab resSig, 0, !typeStamps, sigDeclaredAt resSig, finalMap)
                end;
                
             (* Now make a local functor variable and put it in the
                name space. Because functors can only be declared at
                the top level the only way it can be used is if we have 
                functor F(..) = ... functor G() = ..F.. with no semicolon
                between them. They will then be taken as a single
                declaration and F will be picked up as a local. *)
              (* Set the size of the type map. *)
              
              val var = makeFunctor (name, resArg, functorSig, makeLocal (), line);
              
            in
              #enter sEnv (name, var);
              valRef := var
            end
          end
        | pass2FunctorBind _ =
			raise InternalError "pass2FunctorBind"
      in
        (* Each element in the list is a functor binding. *)
        List.app pass2FunctorBind structList;
        (* Put them into the enclosing env. *)
        #apply sEnv (#enterFunct env)		
      end; (* pass2FunctorDec *)
      
      fun pass2SignatureDec (str: structs, structList : sigBind list) : unit =
      let
        (* There is a restriction that the same name may not be bound twice.
		   As with other bindings functor bindings happen in parallel.
		   DCJM 6/1/00. *)
        val sEnv =  (* The new names. *)
          noDuplicates 
            (fn (name, _, _) => 
              errorNear (lex, true, str, lno, 
                         "Signature " ^ name ^ 
                         " has already been bound in this declaration")
            );

        fun pass2SignatureBind ({name, sigStruct, line, sigRef, ...}) =
          let (* Each element in the list is a signature binding. *)
            (* Get the signature.  We can't have a declaration of a
               signature inside a functor so we can start at 0
               rather than !typeNo. *)
            val resSig : signatures =
				sigVal (sigStruct, 0, Env env, line, strName ^ name ^ ".");
            (* Generate a signature with the new name and put it
               in the table *)
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
      end; (* pass2SignatureDec *)

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
        val () = pass2Struct (decs, makeTypeId, makeCurrentTypeCount, Env localEnv, lno, strName);
             
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
        pass2Struct (body, makeTypeId, makeCurrentTypeCount, Env bodyEnv, lno, strName)
      end; (* pass2Localdec *)
      
      fun pass2Singleton (dec : parsetree, vars, typeIds) : unit =
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

            (* Create a new type ID for each new datatype but record them here. *)
            fun makeId eq =
            let
                val newId = makeTypeId true eq
            in
                typeIds := newId :: ! typeIds;
                newId
            end

         val _ : types = pass2 (dec, makeId, Env newEnv, lex, strName);
       in
         ()
       end; (* pass2Singleton *)

      fun pass2Dec (str : structs) : unit =
        case str of
          StructureDec (structList : structBind list, _) =>
            pass2StructureDec (str, structList)
   
        | FunctorDec (structList : functorBind list, _) =>
            pass2FunctorDec (str, structList)

        | SignatureDec (structList : sigBind list, _) =>
            pass2SignatureDec (str, structList)

        | Localdec {decs, body, ...} =>
            pass2Localdec (decs, body)
        
        | Singleton {dec, vars, typeIds, ...} =>
            pass2Singleton (dec, vars, typeIds)
        
        | _ => (* empty (we should check this!!!) *)
            ();
    in        
       List.app pass2Dec strs (* Process all the top level entries. *)
    end (* pass2Struct *)

        fun makeId isVar eq =
            (if isVar then makeFreeIdEqUpdate else makeFreeId)
                (Local{addr = ref 0, level = ref 0}, eq)
    in (* Structures and types at the top level are free identifiers. *)
        pass2Struct (strs, makeId, fn () => 0, Env env, location lex, "")
    end (*pass2Structs *);


  (*							*
   *     Code-generation phase.	*
   *							*)

  (* When we have done all the unification we can we need to check all
     the values for free type variables.  The arguments to this function
	 are functions which yield all the values, structures and functors
	 in the top-level environment. *)
  fun checkForFreeTypeVars(applyVal, applyStruct, applyFunc, lex : lexan) =
  let
		(* Check the type of a value. *)
		fun checkValue(name: string, v: values) =
			checkForFreeTypeVariables(name, valTypeOf v, lex)

		(* Find all the values in the structure. *)
		fun checkStruct(name: string, s: signatures) =
		let
			fun checkEntry(dName: string, dVal: universal, ()) =
				if tagIs structVar dVal
				then checkStruct(name ^ dName ^ ".",
						structSignat((tagProject structVar) dVal))
				else if tagIs valueVar dVal
				then checkValue(name ^ dName, (tagProject valueVar) dVal)
				else ()
		in
			univFold(sigTab s, checkEntry, ())
		end
  in
		applyVal(fn (s: string, v: values) => checkValue(s, v));
		applyStruct(
			fn (n: string, s: structVals) =>
				checkStruct(n^".", structSignat s));
		(* Look at the result signature of the functor. *)
		applyFunc(
			fn (n: string, f: functors) => checkStruct(n^"().", functorResult f))
  end;


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

   (* Code-generate a structure value, and return the result 
      after matching it to the required signature. *)
    fun structureCode (str, resultSig, strName, debugEnv) =
    let
       (* Generate a new structure which will match the given signature.
          A structure is represented by a vector of entries, and its
          signature is a map which gives the offset in the vector of 
          each value. When we match a signature the candidate structure
          will in general not have its entries in the same positions as
          the target. We have to construct a new structure from it with
          the entries in the correct positions. In most cases the optimiser
          will simplify this code considerably so there is no harm in using
          a general mechanism. *)
      fun matchStructure (code : codetree, source : univTable, sourceIds: int->typeId, dest : signatures) =
      let
          val decs = multipleUses (code, mkAddr, !level);
          val load = #load decs (!level); (* All local *)
          
          (* To save taking apart a structure and then rebuilding it, if the
             structure has not changed we just copy it. *)
          val useOriginal = ref true;
          
          (* We put the entries into this vector and then flatten it. *)
          val resVec   = StretchArray.stretchArray (10 (* Guess *), CodeZero);
          val maxEntry = ref 0;
          
          fun addToList code addr =
            (
              StretchArray.update (resVec, addr, code); 
              if addr >= !maxEntry then maxEntry := addr + 1 else ()
            );
        
          val () =      (* Structures. *)
            univFold
             (sigTab dest,
              (fn (dName, dVal, ()) =>
                if tagIs structVar dVal
                then let
                  val dval = tagProject structVar dVal;
                in
                  if isFormal (structAccess dval)
                  then let
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
                      then let (* select from the code. *)
                        val U : unit = 
                          if vaFormal access <> destAddr
                          then useOriginal := false
                          else ()
                      in
                        mkInd (vaFormal access, load)
                      end
                      else let
                        val U : unit = useOriginal := false;
                      in
                        codeStruct (sourceStruct, !level)
                      end;
                    val sourceSig = structSignat sourceStruct
                    val matched =
                      matchStructure (code, sigTab sourceSig, sigTypeIdMap sourceSig, structSignat dval);
                        
                    val U : unit = 
                      if not (#unchanged matched)
                      then useOriginal := false
                      else ();
                  in
                    addToList (#code matched) destAddr
                  end
                  else ()
                end

                else if tagIs valueVar dVal
                then let (* values. *)
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
							|	_ => false
					 (* Similarly, if we have a constructor which becomes a value we
					    have to extract the injection function or the value. *)
					 val constrBecomesVal =
					 	case (dval, sourceVal) of
							(Value{class=SimpleValue, ...}, Value{class=Constructor _, ...}) =>
								true
							|	_ => false
                       
                      (* If the entry is from a signature select from the code. *)
                     val code =
					   case sourceVal of
					   	Value{access=Formal svAddr, ...} =>
						   let
	                         val UUU =
	                           if svAddr <> addr orelse excBecomesVal orelse constrBecomesVal
	                           then useOriginal := false
	                           else ();
	                       in
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
	                       end

						 | _ =>
	                         let
	                           val UUU = useOriginal := false;
	                         in
	                           if excBecomesVal
							   then codeExFunction(sourceVal, !level, valTypeOf dval, lex, location nullLex)
							   else let
								  val valu =
									 	codeVal (sourceVal, !level, valTypeOf dval, lex, location nullLex)
							   in
							   	  if constrBecomesVal
								  then mkInd(1, valu)
								  else valu
							   end
	                         end;
                  in
		             addToList code addr
                  end
				  | _ => ()
                  end

                else if tagIs typeConstrVar dVal
				then (* We need to process the value constructors.  We may well process them
				        as values anyway but we don't always. *)
					let
		            	val tcons = tagProject typeConstrVar dVal;
						val matchedType = valOf(univLookup (source, typeConstrVar, dName))

						fun processConstructor(Value{access=Formal dstAddr, ...},
											   Value{access=Formal svAddr, ...}) =
							(* Selecting from a signature. *)
			                  (
							  if svAddr <> dstAddr then useOriginal := false else (); 
					          addToList (mkInd (svAddr, load)) dstAddr
			                  )

						  | processConstructor(Value{access=Formal dstAddr, typeOf, ...}, sourceVal) =
						  	  (* Any other source. *)
						 	  (
							  useOriginal := false;
							  addToList (codeVal (sourceVal, !level, typeOf, lex, location nullLex)) dstAddr
							  )

						  | processConstructor _ = ()
					in
						ListPair.app processConstructor (tcConstructors tcons, tcConstructors matchedType)
					end

                else ()
               ),
               ()
              );
            
            (* Process the type IDs in the signature. *)
            fun doTypeID n =
            let
                val typeId = sigTypeIdMap dest n
            in
                case idAccess typeId of
                    Formal addr =>
                    let
                        (* TODO: Do this properly.  This is just temporary. *)
                        (*val id = sourceIds n*)
                        val id = makeFreeId(defaultEqAndPrintCode(), false)
                    in
                        case idAccess id of
                            Formal svAddr =>
                            (
                                if svAddr <> addr
                                then useOriginal := false
                                else ();
                                addToList (mkInd (svAddr, load)) addr
                            )
                        |   access =>
                            (
                                useOriginal := false;
                                addToList (codeAccess(access, !level)) addr
                            )
                    end
                |   _ => raise InternalError "loadTypeId: Not formal"
            end
            val () = tabulate (sigMinTypes dest, sigMaxTypes dest, doTypeID)

        in  
          (* If we have copied the original we can use it unchanged. *)
          if !useOriginal
          then
            {
              code = code,
              unchanged = true
            }
          else (* Put the entries into a list for the vector. *)
          let
              val codeList =
                  List.tabulate (!maxEntry, fn i => StretchArray.sub (resVec,i))
          in
            {
              code      = mkEnv (#dec decs @ [mkTuple codeList]),
              unchanged = false
            }
          end
        end
        
    in
      case str of
        FunctorAppl {name, arg, valRef = ref functs, ...} =>
        let
          val applyCode : codetree =
            mkEval 
              (codeAccess (functorAccess functs, !level),
               [structureCode (arg, structSignat (functorArg functs), strName, debugEnv)],
               false);
            val sourceSig = functorResult functs
        in
          (* Evaluate the functor and match to the result. *)
          #code (matchStructure (applyCode, sigTab sourceSig, sigTypeIdMap sourceSig, resultSig))
        end

      | StructureIdent {valRef = ref v, ...} =>
        let
            val valCode = codeStruct (v, !level); (* Get the structure. *)
            val sourceSig = structSignat v
        in
          #code (matchStructure (valCode, sigTab sourceSig, sigTypeIdMap sourceSig, resultSig))
        end

      | Localdec {decs, body = [localStr], ...} =>
        let (* let strdec in strexp end *)
         (* Generate the declarations but throw away the loads. *)
		 (* TODO: Get the debug environment correct here. *)
          val coded =
		  	mapPair (fn (str, debug) => codeStrdecs (strName, str, debug))
				decs debugEnv;
        in
          mkEnv (#code coded @
                 [structureCode (localStr, resultSig, strName, #debug coded)])
        end

      | StructDec {alist, value, localIds=ref localIds, ...} =>
        let
          val coded = mapStrdecs alist strName debugEnv;
        in 
          (* The result is a block containing the declarations and
             code to load the results. *)
          mkEnv (#code coded @
            [#code (matchStructure (mkTuple (#load coded), value, fn n => List.nth(localIds, n), resultSig))])
        end

      | SigConstraint { str, ... } => structureCode (str, resultSig, strName, debugEnv)

      | _ =>
         raise InternalError "structureCode: not a structure"
    end (* structureCode *)

    (* We need to generate code for the declaration and then code to load
       the results into a tuple. *)
    and codeStrdecs (strName, str, debugEnv: debugenv):
		{ code: codetree list, load: codetree list, debug: debugenv} =
      case str of
        StructureDec (structList : structBind list, _) =>
        let
          fun codeStructureBind ({name, value = SOME value, valRef, typeIds=ref typeIds, ...}: structBind, debug) =
            let
                (* Set the address of the variable representing this structure. *)
                val addr = mkAddr(); 
                val var  = vaLocal (structAccess (!valRef));
                val () = #addr var  := addr; 
                val () = #level var := !level;
			    val sName = strName ^ name ^ "."
			    val strCode =
			  	    structureCode (value, structSignat (!valRef), sName, debug)
            in (* Get the code and save the result in the variable. *)
              {
                code = [mkDec (addr, strCode)],
                (* Load the variable. *)
                load = [mkLoad (addr, 0)],
				debug = debug (* We don't do structures at the moment. *)
              }
            end
          | codeStructureBind ({value = NONE, ...}: structBind, debug) =
                raise InternalError "Structure binding with no structure"
        in
          (* Code-generate each declaration. *)
          mapPair codeStructureBind structList debugEnv
        end
         
      | FunctorDec (structList : functorBind list, _) =>
        let
          fun codeFunctorBind ({name, arg = FormalArg {valRef = ref argVal, ...}, body, valRef, ...}: functorBind,
		  					   debugEnv) =
            let
            
              (* Go down one level. *)
              val () = level := !level + 1;
              
              (* Save the value and set to 1 *)
              val addr = !addresses;
              val () = addresses := 1;
              val arg = vaLocal (structAccess argVal);
              val () = #addr  arg := ~1; 
              val () = #level arg := !level;
              val func = !valRef;
              
              val name : string = strName ^ name;
              
              (* Process the body and make a procedure out of it. *)
              val functorCode : codetree =
                (if getParameter inlineFunctorsTag (debugParams lex) then mkMacroProc else mkProc)
                (structureCode (body, functorResult func, name ^ "().", debugEnv),
                 !level, 1, name);
                  
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
          | codeFunctorBind _ =
              raise InternalError "codeFunctorBind: not a FunctorBind"
        in
          mapPair codeFunctorBind structList debugEnv
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
            load = #load codeBody, (* Result is just the body. *)
			debug = #debug codeBody (* TODO: This isn't correct. *)
          }
        end
 
      | Singleton {dec, vars=ref vars, typeIds=ref typeIds, ...} =>
        let
            (* Code generate the declaration.  This also assigns addresses so must be
               done before we load the results. *)
		    val (code, newDebug) =
		  	    gencode (dec, lex, debugEnv, !level, addresses, strName)

          (* Load each variable, exception and type ID (i.e. equality & print function)
             that has been declared.
             Since value declarations may be mutually recursive we have
             to code-generate the declarations first then return the values. *)
		    fun filterVals (CoreValue v) = SOME(codeVal (v, !level, badType, nullLex, location nullLex))
		    |   filterVals _             = NONE
            val loadVals = List.mapPartial filterVals vars
            val loadIDs = List.map(fn v => codeAccess(idAccess v, !level)) typeIds
        in
            { 
                code = code,
                load = loadVals @ loadIDs,
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

    val coded = mapStrdecs strs "" ([], fn _ => CodeZero); (* Process top level list. *)

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
  fun pass4Structs (results, (strs, _)) =
  let
	(* Process the datatypes in the structure and turn their value constructors
	   into Global entries.  We only need this in order to be able to print values
	   of datatypes which have been produced in structures or functors with
	   opaque signatures.  We could do this for other values as well but it's not
	   really necessary.
	   Because of sharing, value constructors from structures or functors
	   without opaque matching will already be global. *)
    fun extractValsToSig (results: codetree, signat: signatures): unit =
    (
      univFold
       (sigTab signat,
        (fn (dName, dVal, ()) =>
         if tagIs structVar dVal
         then let (* Structures in the signature. *)
           val subStr = tagProject structVar dVal;
           
           (* Process this signature. *)
		   val base =
		   	case structAccess subStr of
				Formal addr => mkInd (addr, results)
			|	Global code => code
			|	_ => raise InternalError "extractValsToSig: bad access";
         in
		 	(* We could create a global substructure here and enter it. *)
           extractValsToSig (base, structSignat subStr)
         end
         
(*         else if tagIs valueVar dVal
         then let (* Values. *)
           val v = tagProject valueVar dVal;
         in
		   case v of
		   	  Value {name, typeOf, class, access = Formal addr } =>
			  	let
					val ind = mkInd (addr, results);
					val globalVal =
						Value{name=name, typeOf=typeOf, class=class, access=Global ind}
				in
			  		univEnter (sigTab signat, valueVar, dName, globalVal)
				end
			| _ => ()
         end
*)
		 else if tagIs typeConstrVar dVal
		 then let (* Types. *)
            val tcons = tagProject typeConstrVar dVal;
			(* Update the constructor list. *)
			fun copyAConstructor(Value{name=cName, typeOf, class, access = Formal addr, locations}) =
				Value{name=cName, typeOf=typeOf, class=class,
                      access=Global(mkInd(addr, results)), locations = locations}
			|	copyAConstructor c = c (* Already a global. *)
         in
		 	if null (tcConstructors tcons)
            then () (* In a few cases (open PolyML perhaps) tcons may be frozen. *)
            else tcSetConstructors(tcons, map copyAConstructor (tcConstructors tcons))
        end

         else () (* Anything else *)
        ), (* end fn *)
       ()
      )
    ) (* end extractValsToSig *)
      
    fun extractStruct(str, args as (addr, { fixes, values, structures, signatures, functors, types } )) =
      case str of
        FunctorDec (structList : functorBind list, _) =>
        let
          fun extractFunctorBind ({name, valRef, ...}: functorBind, (addr, funcs)) =
            let
              val code = mkInd (addr, results);
              val func = !valRef;
              val funcTree = 
                makeFunctor 
                 (functorName func,
                  functorArg func,
                  functorResult func,
                  makeGlobal code,
                  functorDeclaredAt func);
            in
              (addr + 1, (name, funcTree) :: funcs)
            end
          (* Get the functor values. *)
          val (newAddr, newfunctors ) = List.foldl extractFunctorBind (addr, functors) structList
        in
          (newAddr, { functors=newfunctors, fixes=fixes, values=values,
                      signatures=signatures, structures=structures, types=types })
        end
 
      | StructureDec (structList : structBind list, _) =>
        let
          fun extractStructureBind ({name, valRef, line, typeIds=ref typeIds, ...}: structBind, (addr, structures)) =
            let
              val resultSig = structSignat (!valRef);
			  val structCode = mkInd (addr, results)
            in
			  (* Convert the values to global.  More importantly convert any datatypes
			     with non-global value constructors. *)
			  extractValsToSig (structCode, resultSig);
                let
                    (* Process the type IDs *)
                    fun convertTypeID typeId =
                    let
                        val codeID =
                            case idAccess typeId of
                                Formal addr => Global(mkInd(addr, results))
                            |   Local _ => defaultEqAndPrintCode()
                                    (* TODO: This shouldn't happen but at the moment we don't correctly convert
                                       Local typeIDs into Formals. *)
                            |   access => access
                    in
                        setTypeAccess(typeId, codeID)
                    end
                in
                    List.app convertTypeID typeIds
                end;
              (* Make a global structure. *)
              (addr + 1, (name, makeGlobalStruct (name, resultSig, structCode, line)) :: structures)
            end
          val (newAddr, newstructures) = List.foldl extractStructureBind (addr, structures) structList
        in
          (newAddr, { structures=newstructures, functors=functors, signatures=signatures,
                      fixes=fixes, values=values, types=types })
        end
 
      | Localdec {body, ...} =>
          List.foldl extractStruct args body
 
      (* Value, exception or type declaration at the top level. *)
      | Singleton {vars=ref vars, typeIds=ref typeIds, ...} =>
        let (* Enter the values and exceptions. *)
		  fun makeDecs (CoreValue dec, (addr, { fixes, values, structures, signatures, functors, types } )) =
                 ( case dec of
				 	Value{access=Overloaded _, ...} => (* enter it as it is. *) 
                     	(addr+1, { fixes=fixes, values=(valName dec, dec) :: values, structures=structures,
                                   signatures=signatures, functors=functors, types=types })
					| Value{class, name, typeOf, locations, ...} =>
						(* take the value out of the result vector. *)
	                   let
	                     val codeVal = mkInd (addr, results);
	                     val name    = valName dec;
	                     val val' =
						 	Value{class=class, name=name, typeOf=typeOf, access=Global codeVal, locations=locations}
	                   in
                         (addr+1, { fixes=fixes, values=(name, val') :: values, structures=structures,
                                    signatures=signatures, functors=functors, types=types } )
	                   end
                 )
		  |  makeDecs (CoreStruct dec, (addr, {fixes, values, structures, signatures, functors, types})) =
	          (* Any structures will only come from "open A" where A contains
	             sub-structures. "A" must be global otherwise the open would
	             not be global, so all the sub-structures must be global. *)
                 (addr, { fixes=fixes, values=values, structures=(structName dec, dec) :: structures,
                          signatures=signatures, functors=functors, types=types } )
		  |   makeDecs (CoreFix pair, (addr, {fixes, values, structures, signatures, functors, types})) =
                 (addr, { fixes=pair :: fixes, values=values, structures=structures,
                          signatures=signatures, functors=functors, types=types } )
		  |   makeDecs (CoreType dec, (addr, {fixes, values, structures, signatures, functors, types})) =
                 (addr, { fixes=fixes, values=values, structures=structures,
                          signatures=signatures, functors=functors, types=(#second(splitString(tcName dec)), dec) :: types } )

            val newDecs = List.foldl makeDecs args vars

            (* Extract the equality/print value from the result vector and set this in the type ID.  Unlike
               values we can't simply return a new value here because the type ID is pointed at by types
               within the values and type constructors. *)
            fun convertTypeID(typeId, (addr, decs)) =
            let
                val codeID = mkInd(addr, results)
            in
                setTypeAccess(typeId, Global codeID);
                (addr+1, decs)
            end

            val newIDs = List.foldl convertTypeID newDecs typeIds
        in
            newIDs
        end

     | SignatureDec (structList : sigBind list, _) =>
		let
            val newSigs = List.map (fn ({sigRef=ref s, name, ...}: sigBind) => (name, s)) structList
		in
           (addr, { fixes=fixes, values=values, structures=structures,
                    signatures=newSigs @ signatures, functors=functors, types=types } )
		end

     | _ => args; (* end extractStruct *)

    val empty = { fixes=[], values=[], structures=[], functors=[], types=[], signatures=[] }
    val (lastAddr, result) = List.foldl extractStruct (0, empty) strs;
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
end;
