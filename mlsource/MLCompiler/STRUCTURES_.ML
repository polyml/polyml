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

structure LEX : LEXSIG
structure CODETREE : CODETREESIG
structure STRUCTVALS : STRUCTVALSIG;
structure VALUEOPS : VALUEOPSSIG;
structure EXPORTTREE: EXPORTTREESIG
structure TYPETREE : TYPETREESIG
structure PARSETREE : PARSETREESIG
structure PRETTY : PRETTYSIG
structure COPIER: COPIERSIG
structure TYPEIDCODE: TYPEIDCODESIG
structure SIGNATURES: SIGNATURESSIG
structure DEBUGGER : DEBUGGERSIG

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
    val inlineFunctorsTag: bool Universal.tag
    val errorDepthTag : int Universal.tag
    val debugTag: bool Universal.tag
    val getParameter :
           'a Universal.tag -> Universal.universal list -> 'a 
end;

sharing LEX.Sharing = VALUEOPS.Sharing = TYPETREE.Sharing = PARSETREE.Sharing
    = PRETTY.Sharing = EXPORTTREE.Sharing = STRUCTVALS.Sharing = COPIER.Sharing
    = CODETREE = UNIVERSALTABLE = TYPEIDCODE.Sharing = SIGNATURES.Sharing = DEBUGGER.Sharing

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
  open SIGNATURES
  open DEBUGGER

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
      {
        alist: structs list, (* List of items in it. *)
        location: location,
        value: univTable (* Value *),
        resultSig: signatures ref
      }

  | FunctorAppl    of functorApplForm    (* Appln of a functor *)

  | CoreLang      of (* Any other decln. *)
    {
        dec:   parsetree,           (* The value *)
        vars:  coreDeclaration list ref, (* The declarations *)
        location: location
    }

  | Localdec       of localdecStruct     (* Local/Let. *)

  | SigConstraint  of                    (* Constraint of str to match sig. *)
       {
          str: structs,  (* Structure to constain *)
          csig: sigs, (* Constraining signature *)
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
        argIds:  { source: typeId, dest: typeId } list ref, (* The IDs that are required in the arguments. *)
        resIds:  { source: typeId, dest: typeId } list ref, (* Generative IDs in the result. *)
        resultSig: signatures ref
      }

  (* Used for local strdec in strdec and let strdec in strexp. *)
  and localdecStruct =
      {
        decs:     structs list,
        body:     structs list,
        localDec: bool,
        line:     location
      }
   
    fun isEmptyStruct     EmptyStruct       = true | isEmptyStruct    _ = false;
  
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
          resultSig = ref undefinedSignature
        };
  
  
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
          argIds  = ref nil,
          resIds  = ref nil,
          resultSig = ref undefinedSignature
        };
  
    fun mkFormalArg (name, signat) =
        {
          name      = name,
          sigStruct = signat,
          valRef    = ref undefinedStruct
        };

    fun mkLocaldec (decs, body, localDec, line) =
      Localdec 
         {
           decs     = decs,
           body     = body,
           localDec = localDec,
           line     = line
         };

    fun mkSigConstraint(str, csig, opaque, sigLoc) =
        SigConstraint
        {
            str=str, csig=csig, opaque=opaque, sigLoc=sigLoc,
            opaqueIds=ref nil, resultSig = ref undefinedSignature
        }

    val mkStructureDec = StructureDec
 
    fun mkStructureBinding ((name, nameLoc), signat, value, fullLoc): structBind =
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
            line      = fullLoc
        }
    end;

    type formalArgStruct =
      {
        name:      string,
        sigStruct: sigs,
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
            sigStruct: sigs,(* Its value *)
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

    fun displayList ([], _, _) _ = []
    
    |   displayList ([v], _, depth) dodisplay =
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
                                    displaySigs (sigStruct, depth - 1)
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

      | StructureIdent {name, ...} =>
            PrettyString name

      | StructDec {alist, ...} =>
            PrettyBlock (1, true, [],
                PrettyString "struct" ::
                PrettyBreak (1, 0) ::
                displayList (alist, "", depth) displayStruct @
                [ PrettyBreak (1, 0), PrettyString "end"]
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

      | Localdec {decs, body, localDec, ...} =>
            PrettyBlock (3, false, [],
                PrettyString (if localDec then "local" else "let") ::
                PrettyBreak (1, 0) ::
                displayList (decs, ";", depth - 1) displayStruct @
                [ PrettyBreak (1, 0), PrettyString "in", PrettyBreak (1, 0)] @
                displayList (body, ";", depth - 1) displayStruct @
                [ PrettyBreak (1, 0), PrettyString "end" ]
            )

      | CoreLang {dec, ...} =>
          ptDisplay (dec, depth - 1)

      | SigConstraint{str, csig, opaque, ...} =>
            PrettyBlock (0, false, [],
                [
                    displayStruct (str, depth - 1),
                    PrettyString (if opaque then " :>" else " :"),
                    PrettyBreak (1, 0),
                    displaySigs (csig, depth - 1)
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
                            displaySigs (sigStruct, depth - 1)
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
                            [displaySigs (argStruct, depth - 1)]
                        ) ::
                        PrettyString ")" ::
                        (
                            case sigStruct of
                                NONE  => [] (* Signature is optional *)
                            |   SOME (sigStruct, opaque, _) =>
                                [
                                    PrettyString(if opaque then " :>" else " :"),
                                    PrettyBreak (1, 0),
                                    displaySigs (sigStruct, depth - 1)
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
                            sigExportTree({parent=SOME exportThis, previous=SOME getName, next=next}, theSig)
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

        |   StructureIdent { valRef = ref var, location, ... } =>
                (* Get the location properties for the identifier. *)
                (location, mapLocationProps (structLocations var) @ commonProps)

        |   StructDec{ location, alist, ...} =>
                (location, exportList(structExportTree, SOME asParent) alist @ commonProps)

        |   FunctorAppl { valRef = ref fnctr, name, nameLoc, fullLoc, arg, ... } =>
            let
                (* Navigate between the functor name and the argument. *)
                (* The first position is the expression, the second the type *)
                fun getFunctorName () =
                    getStringAsTree({parent=SOME asParent, previous=NONE, next=SOME getFunctorArg},
                        name, nameLoc, [PTdeclaredAt(functorDeclaredAt fnctr)])
                and getFunctorArg () =
                    structExportTree({parent=SOME asParent, previous=SOME getFunctorName, next=NONE}, arg)
            in
                (fullLoc, PTfirstChild getFunctorName :: commonProps)
            end

        |   CoreLang {dec, ...} => (* A value parse-tree entry. *)
                getExportTree(navigation, dec)

        |   Localdec {decs, body, line, ...} =>
                (line, exportList(structExportTree, SOME asParent) (decs @ body) @ commonProps)

        |   SigConstraint { str, csig, sigLoc, ... } =>
            let
                (* Navigate between the functor name and the argument. *)
                (* The first position is the expression, the second the type *)
                fun getStructure () =
                    structExportTree({parent=SOME asParent, previous=NONE, next=SOME getSignature}, str)
                and getSignature () =
                    sigExportTree({parent=SOME asParent, previous=SOME getStructure, next=NONE}, csig)
            in
                (sigLoc, PTfirstChild getStructure :: commonProps)
            end

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
                            sigExportTree({parent=SOME exportThis, previous=SOME getName, next=NONE}, sigStruct)
                    in
                        (line, PTfirstChild getName :: exportNavigationProps navigation)
                    end
            in
                (location, exportList(exportSB, SOME asParent) sigs @ commonProps)
            end

        |   FunctorDec(fbl, location) =>
            let
                fun exportFB(navigation,
                        fb as {name, nameLoc, haveSig, arg={sigStruct=argStruct, ...}, body, line, ...}) =
                    let
                        val (sigStruct, body) =
                            case (haveSig, body) of
                                (true, SigConstraint{str, csig, opaque, sigLoc, ...}) =>
                                    (SOME(csig, opaque, sigLoc), str)
                            |   _ => (NONE, body)
                        val fbProps = exportNavigationProps navigation
                        fun exportThis () = exportFB(navigation, fb)
                        (* Because the signature is optional navigation on the arg and body depends on
                           whether there's a signature. *)
                        fun getName() =
                            getStringAsTree({parent=SOME exportThis, previous=NONE, next=SOME getArg},
                                name, nameLoc, [])
                        
                        and getArg() =
                        let
                            val next =
                                if isSome sigStruct then getSig else getBody
                        in
                            sigExportTree({parent=SOME exportThis, previous=SOME getName, next=SOME next},
                                        argStruct)
                        end
            
                        and getSig() =
                            sigExportTree({parent=SOME exportThis, previous=SOME getArg, next=SOME getBody},
                                        #1(valOf sigStruct))
            
                        and getBody() =
                        let
                            val previous = if isSome sigStruct then getSig else getArg
                        in
                            structExportTree({parent=SOME exportThis, previous=SOME previous, next=NONE}, body)
                        end
                    in
                        (line, PTfirstChild getName :: fbProps)
                    end

                val expChild = exportList(exportFB, SOME asParent) fbl
            in
                (location, expChild @ commonProps)
            end
    end

    (* Convert a "program" into a navigable tree. *)
    fun structsExportTree (parentTree, trees: program) =
    let
        val parentTreeNav = exportNavigationProps parentTree
        (* The top level is actually a list. *)
        fun exportTree(([], location)) = (location, parentTreeNav)
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
            (location, parentTreeNav @ [PTfirstChild(getEntry(sl, NONE))])
        end
    in
        exportTree trees
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
    end;

    (* TODO: If the item being errored is in a substructure it currently doesn't report
       the name of the substructure. *)
    (* Report an error about signature-structure matching. *)
    fun sigStructMatchMsg (lex, near, lno, structName) (doDisplay: 'a -> pretty)
                (structValue: 'a, sigValue: 'a, reason) =
        let
            val message =
                PrettyBlock(3, true, [],
                    [
                        PrettyString
                            ("Structure does not match signature" ^
                                (if structName = "" then "." else " in sub-structure " ^ structName)),
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

    fun sigStructMissingMsg (lex, near, lno, structName) (doDisplay: 'a -> pretty) (sigValue: 'a) =
        let
            val message =
                PrettyBlock(3, true, [],
                    [
                        PrettyString
                            ("Structure does not match signature" ^
                                (if structName = "" then "." else " in sub-structure " ^ structName)),
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

    (* Error message routine for lookupType and lookupStructure. *)
    fun giveError (sVal : structs, lno : LEX.location, lex : lexan) : string -> unit =
        fn (message : string) => errorNear (lex, true, fn n => displayStruct(sVal, n), lno, message);

    (* Turn a result from matchTypes into a pretty structure so that it
       can be included in a message. *)
    (* TODO: When reporting type messages from inside the structure we should use
       the environment from within the structure and for type within the signature the signature env. *)
    fun matchErrorReport(lex, structTypeEnv, sigTypeEnv) =
        unifyTypesErrorReport(lex, structTypeEnv, sigTypeEnv, "match")

    (* Check that two types match. *)
    fun matchTypes (candidate, target, targMap: int -> typeId option) =
    let
        fun copyId(TypeId{idKind=Bound{ offset, ...}, ...}) = targMap offset
        |   copyId _ = NONE
        fun copyATypeConstr tcon = copyTypeConstr(tcon, copyId, fn x => x, fn s => s)
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
        val (copiedCandidate : types, _) = generalise candidate;
    in
        unifyTypes (copiedCandidate, copiedTarget)
    end;

    (* Check that a matching has succeeded, and check the value
       constructors if they are datatypes. *)
    fun checkTypeConstrs (candid, target, targTypeMap: int -> typeId option, lex, near, lno, typeEnv, structPath) =
    let
        val candidName : string = tcName candid;
        val targetName : string = tcName target;
        val tvars = List.map TypeVar (tcTypeVars target); (* either will do *)
        (* If we get an error in the datatype itself print the full datatype. *)
        val printTypeEnv = { lookupType = fn _ => NONE, lookupStruct = fn _ => NONE }
        val errorInDatatype =
            sigStructMatchMsg(lex, near, lno, structPath)(fn t => displayTypeConstrs(t, errorDepth lex, printTypeEnv))
    in
        if tcArity candid <> tcArity target
        then () (* Have already given the error message. *)
        else (* Check the type constructors themselves first. This checks
                that the sharing constraints have been satisfied. *)
        case matchTypes (mkTypeConstruction (candidName, candid, tvars, []),
                         mkTypeConstruction (targetName, target, tvars, []), 
                         targTypeMap) of
                SOME error => errorInDatatype(candid, target, matchErrorReport(lex, typeEnv, typeEnv) error) (* Report the error. *)
            |   NONE => () (* We have already checked for matching a type in the structure to a datatype in the signature.
                              In ML97 we can't rebind an identifier in a signature so each constructor for this datatype
                              must be present in the signature i.e. it can't be hidden by a constructor for another datatype.
                              So we can check the constructors when we check the values. *)
    end (* checkTypeConstrs *);

    (* Check that a candidate signature (actually the environment part of
       a structure) matches a target signature. The direction is important
       because a candidate is allowed to have more components and more
       polymorphism than the target.  As part of the matching process we
       build up a map of typeIDs in the target to type IDs in the candidate
       and that is returned as the result.
       N.B. the map function takes an argument between minTarget and maxTarget. *)
    fun matchSigs(originalCandidate, originalTarget, near, lno, lex, typeIdEnv, typeEnv): int -> typeId =
    let
        val candidate = (* The structure. *)
        let
            val Signatures { typeIdMap, minTypes, boundIds, ... } = originalCandidate
            val _ =
                case boundIds of
                    [] => ()
                |   _ => raise InternalError "Candidate structure with non-empty bound ID list"
        in
            if isUndefinedSignature originalCandidate
            then undefinedSignature
            else replaceMap(originalCandidate, typeIdMap, minTypes, [], typeIdEnv)
        end
        
        val target = (* The signature. *)
        let
            val Signatures { typeIdMap, minTypes, boundIds, ... } = originalTarget
            fun newMap n =
            if n < minTypes then typeIdEnv n
            else List.nth(boundIds, n-minTypes)
        in
            replaceMap(originalTarget, typeIdMap, minTypes, boundIds, newMap)
        end

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
        fun matchNames (candidate, target, structPath) : unit =
        if isUndefinedSignature candidate
        then () (* Suppress unnecessary messages. *)
        else univFold (sigTab target,
            fn (dName, dVal, ()) =>
            if tagIs typeConstrVar dVal
            then
            let (* See if there is one with the same name. *)
                val target = tagProject typeConstrVar dVal;
                val printTypeEnv = { lookupType = fn _ => NONE, lookupStruct = fn _ => NONE }
                fun displayType t = displayTypeConstrs(t, errorDepth lex, printTypeEnv)
                val typeError = sigStructMatchMsg(lex, near, lno, structPath) displayType
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
                                TypeId{idKind=Bound { offset, ...}, ...} => enterType (offset, tcIdentifier candid)
                            |   _ => ();
                    
                            if tcArity target <> tcArity candid
                            then typeError(candid, target,
                                        PrettyString "Types take different numbers of type arguments.")

                            (* Check that it's a datatype before checking for eqtype. *)
                            else if not (null (tcConstructors target)) andalso
                                    null (tcConstructors candid)
                            then typeError(candid, target, 
                                        PrettyString "Type in structure is not a datatype")

                            else if not(tcIsAbbreviation target) andalso tcEquality target
                                    andalso not (permitsEquality candid)
                            then typeError(candid, target, 
                                        PrettyString "Type in structure is not an equality type")
                           
                            else () 
                        )
                        else ()
                |   NONE => sigStructMissingMsg(lex, near, lno, structPath) displayType target
            end
             
            else if tagIs structVar dVal
            then
            let (* and sub-structures. *)
                val target = (tagProject structVar) dVal;
                (* For each target structure: find a candidate with the 
                   same name and recursively check them. *)
            in
                case univLookup (sigTab candidate, structVar, dName) of
                   SOME candid =>
                    matchNames (structSignat candid, structSignat target, structPath ^ dName ^ ".")
                |  NONE =>
                    let
                        fun displayStructure s =
                            PrettyBlock(0, false, [],
                                [PrettyString "structure" , PrettyBreak(1, 3), PrettyString(structName s)])
                    in
                        sigStructMissingMsg(lex, near, lno, structPath) displayStructure target
                    end
            end
            else (), (* not a type or structure *)
          ()  (* default value for fold *)
        ) (* matchNames *);
      
        val () = matchNames (candidate, target, "");
       
        (* Match the values and exceptions in the signatures.
           This actually does the checking of types. *)
        fun matchVals (candidate, target, structPath) : unit =
        if isUndefinedSignature candidate
        then () (* Suppress unnecessary messages. *)
        else (* Map the identifiers first, returning the originals if they are
           not in the map. *)
        let
            val () : unit =
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
                            checkTypeConstrs(candid, target, lookupType, lex, near, lno, typeEnv, structPath)
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
                       SOME candid =>
                            matchVals (structSignat candid, structSignat target, structPath ^ dName ^ ".")
                    |  NONE => () (* Ignore the error - we've already reported it in matchNames *)
                  end
  
                  else ()
                ), (* fn *)
               ()
              );

            fun displayValue value =
            let
                fun dispVal(kind, typeof) =
                    PrettyBlock(0, false, [],
                        [
                            PrettyString kind,
                            PrettyBreak(1, 3),
                            PrettyString(valName value ^ ":"),
                            PrettyBreak(1, 0),
                            display (typeof, errorDepth lex, typeEnv)
                        ])
            in
                case value of
                    Value{class=Constructor _, ...} =>
                        (* When displaying the constructor show the function type.  We may have rebound
                           the constructor in the candidate structure so that it creates a different datatype. *)
                        dispVal("constructor", valTypeOf value)
                |   Value{class=Exception, ...} =>
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
                                        PrettyBreak (1, 3), display (excType, errorDepth lex, typeEnv) ]
                            ))
                |   _ => dispVal("val", valTypeOf value)    
            end

            val () : unit =
            (* Finally the values and exceptions. *)
            univFold 
                (sigTab target,
                    (fn (dName, dVal, ()) =>
                        if tagIs valueVar dVal
                        then
                        let
                            val v = tagProject valueVar dVal
                        in
                            case univLookup (sigTab candidate, valueVar, dName) of
                                NONE => sigStructMissingMsg(lex, near, lno, structPath) displayValue v
                            |   SOME candid =>
                                let
                                    (* If the target is a constructor or exception the candidate must be
                                       similar. *)
                                    val matchKind =
                                        case (v, candid) of
                                            (Value{class = Constructor _, ...}, Value {class = Constructor _, ...}) => NONE
                                        |   (Value{class = Constructor _, ...}, _) => SOME(PrettyString "Value is not a constructor")
                                        |   (Value{class = Exception, ...}, Value {class = Exception, ...}) => NONE
                                        |   (Value{class = Exception, ...}, _) => SOME(PrettyString "Value is not an exception")
                                        |   _ => NONE
                                in
                                    case matchKind of
                                        SOME error =>
                                            sigStructMatchMsg(lex, near, lno, structPath)
                                                displayValue (candid, v, error)
                                    |   NONE =>
                                        case matchTypes (valTypeOf candid, valTypeOf v, lookupType) of
                                            NONE => ()
                                        |   SOME error =>
                                                sigStructMatchMsg(lex, near, lno, structPath)
                                                    displayValue (candid, v,
                                                        matchErrorReport(lex, typeEnv, typeEnv) error);
                                    (* If it matches an entry in the signature it counts being exported. *)
                                    case candid of
                                        Value { references=SOME{exportedRef, ...}, ...} =>
                                           exportedRef := true
                                    |   _ => () 
                                end
                        end
                        else ()
                    ),
                    ()
                )
        in
            ()
        end (* matchVals *);
    in 
        matchVals (candidate, target, ""); (* Do the match. *)
        resultType (* Return the function to look up the results. *)
    end (* matchSigs *);

    val makeEnv = fn x => let val Env e = makeEnv x in e end;

    (* Any values in the signature are counted as exported. *)
    fun markValsAsExported resSig =
    let
        fun refVals(_, dVal, ()) =
        if tagIs valueVar dVal
        then
        (
            case tagProject valueVar dVal of
                Value {references=SOME{exportedRef, ...}, ...} =>
                    exportedRef := true
            |   _ => ()
        )
        else ()
    in
        univFold(sigTab resSig, refVals, ())
    end

  (* Second pass - identify names with values and type-check *)
 
      (* Process structure-returning expressions i.e. structure names,
         struct..end values and functor applications. *)
    fun structValue(str, newTypeId: (bool*bool*bool*typeIdDescription)->typeId, currentTypeCount,
                    newTypeIdEnv: unit -> int->typeId, Env env, lex, lno, structPath) =
    let
        val typeEnv =
        {
            lookupType =
                fn s => case #lookupType env s of NONE => NONE | SOME t => SOME(t, SOME(newTypeIdEnv())),
            lookupStruct =
                fn s => case #lookupStruct env s of NONE => NONE | SOME t => SOME(t, SOME(newTypeIdEnv()))
        }
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

        | FunctorAppl {name, arg, valRef, nameLoc, fullLoc, argIds, resIds, resultSig, ... } =>
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
                    (* Apply a functor to an argument.  The result structure contains a mixture of IDs
                       from the argument structure and generative IDs from the result structure.
                       There are two parts to this process.
                       1.  We have to match the actual argument structure to the formal argument to
                           ensure that IDs are in the right place for the functor.
                       2.  We have to take the actual argument structure and the functor result structure
                           and produce a combination of this as a structure. *)
                    (* IDs:
                       argIDs: A list of pairs of IDs as Selected/Local/Global values and Formal values.
                          This contains the IDs that must be passed into the functor.
                       resIDs: A list of pairs of IDs as Local values and Formal values.  The Local value
                          is the location where a new generative ID is stored and the Formal offset is the
                          offset within the run-time vector returned by the signature where the source ID
                          for the generative ID is to be found. *)
                    val functorResSig : signatures = functorResult functr;
                    val argStruct : structVals = functorArg functr;
                    val formalArgSig = structSignat argStruct

                    (* This provides information about the arguments. *)
                    (* Get the actual parameter value. *)
                    val actualArgSig =
                        structValue(arg, newTypeId, currentTypeCount, newTypeIdEnv, Env env, lex, fullLoc, structPath);

                    (* Check that the actual arguments match formal arguments,
                       and instantiate the variables. *)
                    val matchResults =
                        matchSigs (actualArgSig, formalArgSig,
                            fn n => displayStruct(str, n), fullLoc, lex, newTypeIdEnv(), typeEnv);
                    (* Create a list of the type IDs that the argument must supply. *)
                    local
                        val maxT = sigMaxTypes formalArgSig and minT = sigMinTypes formalArgSig
                        val results = List.tabulate(maxT-minT, fn n => matchResults(n+minT))
                        val args = ListPair.mapEq(fn(s, d) => { source = s, dest = d })(results, sigBoundIds formalArgSig)
                    in
                        val () = argIds := args; (* Save for code-generation. *)
                    end

                    (* Now create the generative typeIDs.  These are IDs that are in the bound ID range of
                       the result signature.  Any type IDs inherited from the argument will have type ID
                       values less than sigMinTypes functorResSig. *)
                    local
                        fun makeNewTypeId(
                            oldId as TypeId{idKind=Bound{isDatatype, ...}, description = { name=oldName, ...}, ...}) =
                        let
                            val description =
                                { location = fullLoc, name = oldName, description = "Created from applying functor " ^ name }
                            val newId = newTypeId(false, isEquality oldId, isDatatype, description)
                        in
                            { source = oldId, dest = newId }
                        end
                        |   makeNewTypeId _ = raise InternalError "Not Bound"
                        (* The resIds list tells the code-generator where to find the source of each
                           ID in the result structure and where to save the generative ID. *)
                        val sdList = List.map makeNewTypeId (sigBoundIds functorResSig)
                        val _ = resIds := sdList (* Save for code-generation. *)
                    in
                        (* This vector contains the resulting type IDs.  They all have Local access. *)
                        val resVector = Vector.fromList(List.map(fn { dest, ...} => dest) sdList)
                    end
                
                    (* Construct a result signature using copySigAndRenumber.  This will contain all the
                       IDs created here i.e. IDs in the argument and generative IDs at the start and then
                       all the values and structures returned from the functor.
                       When we come to code-generate we need to
                       1. Use loadOpaqueIds over the resIDs to create the opaque IDs.
                       2. Basically, do the same as StructDec to match to the result signature.
                       We don't need to do anything about type IDs from the argument.  Processing
                       the argument will ensure that type IDs created in the argument are declared
                       as Locals and if we pass localIDs to matchStructure we will load IDs from
                       both the argument and generative IDs created by loadOpaqueIds. *)
                    val minCopy = Int.min(sigMinTypes formalArgSig, sigMinTypes functorResSig)
                    val idEnv = newTypeIdEnv()
                    fun getCombinedTypeId n =
                        if n < minCopy then idEnv n
                        else if n >= sigMinTypes functorResSig
                        then Vector.sub(resVector, n - sigMinTypes functorResSig)
                        else if n >= sigMinTypes formalArgSig
                        then matchResults n
                        else sigTypeIdMap formalArgSig n

                    val resSig =
                        let
                            val Signatures { name, tab, declaredAt, ... } = functorResSig
                        in
                            makeSignature(name, tab, currentTypeCount(), currentTypeCount(), declaredAt,
                                composeMaps(sigTypeIdMap functorResSig, getCombinedTypeId), [])
                        end
                in
                    resultSig := resSig;
                    resSig
                end
            end
                           
        | StructDec {alist, value = structTable, location, resultSig, ...} =>
          let
            (* Collection of declarations packaged into a structure
                or a collection of signatures. *)
            (* Some of the environment, the types and the value constructors,
               is generated during the first pass. Get the environment from
               the structure. *)
            val structEnv = makeEnv structTable

            val makeLocalTypeId = newTypeId
            val makeLocalTypeIdEnv = newTypeIdEnv

            val newEnv =
            {
                enterType = #enterType structEnv,
                enterVal = #enterVal structEnv,
                enterStruct = #enterStruct structEnv,
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
            val () =
                pass2Struct (alist, makeLocalTypeId, currentTypeCount, makeLocalTypeIdEnv, Env newEnv, lex, lno, structPath);

            (* We need to make a signature for the result in the form that can be used if there is no
               explicit signature, for example if this is used as the result of a functor.  That means
               creating Formal values for all the values and structures.  These Formal entries define
               the position in the run-time vector where each of the values and sub-structures are
               located.  We don't include typeIDs in this.  Any typeIDs that need to be included in
               the run-time vector are added by the functor declaration code. *)
            val finalTable = makeSignatureTable();
            val finalEnv = makeEnv finalTable
            
            fun enterItem(dName, dVal, addrs) =
                if tagIs typeConstrVar dVal
                then (#enterType finalEnv (dName, tagProject typeConstrVar dVal); addrs)
                else if tagIs structVar dVal
                then 
                let
                    val strVal = tagProject structVar dVal
                    val locations = structLocations strVal
                in
                    #enterStruct finalEnv (dName, makeFormalStruct (dName, structSignat strVal, addrs, locations));
                    addrs + 1
                end
                else if tagIs valueVar dVal
                then
                let
                    val valVal = tagProject valueVar dVal
                in
                    (* If this is a type-dependent function such as PolyML.print we must put in the
                       original type-dependent version not the version which will have frozen
                       its type as 'a. *)
                    case valVal of
                        value as Value{access = Overloaded _, ...} =>
                        (
                            #enterVal finalEnv (dName, value);
                            addrs
                        )
                    |   Value{class, typeOf, locations, references, ...} =>
                        (
                            #enterVal finalEnv (dName,
                                Value{class=class, name=dName, typeOf=typeOf, access=Formal addrs,
                                      locations=locations, references=references});
                            addrs + 1
                        )
                end
                else addrs
            
            val _ = univFold(structTable, enterItem, 0)
            val resSig =
                makeSignature("", finalTable, currentTypeCount(), currentTypeCount(), location, newTypeIdEnv(), [])
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
            val () =
              pass2Struct (decs, newTypeId, currentTypeCount, newTypeIdEnv, Env localEnv, lex, line, structPath);
                   
          in
            (* There should just be one entry in the "body" list. *)
            structValue(localStr, newTypeId, currentTypeCount, newTypeIdEnv, Env localEnv, lex, line, structPath)
          end
          
        | SigConstraint { str, csig, opaque, sigLoc, opaqueIds, resultSig, ... } =>
            let
                val bodyIds = ref []
                val startTypes = currentTypeCount()
                val startTypeEnv = newTypeIdEnv()
                fun sconstraintMakeTypeId (isVar, eq, isdt, desc) =
                let
                    val newId = newTypeId(isVar, eq, isdt, desc)
                in
                    bodyIds := newId :: ! bodyIds;
                    newId
                end
                fun sconstraintTypeIdEnv () n =
                    if n < startTypes then startTypeEnv n
                    else valOf(
                        List.find(fn TypeId{idKind=Bound{offset, ...}, ...} => offset = n | _ => raise Subscript) (!bodyIds))

                val resSig =
                    structValue(str, sconstraintMakeTypeId, currentTypeCount, sconstraintTypeIdEnv, Env env, lex, lno, structPath);
                (* Get the explicit signature. *)
                val explicitSig = sigVal(csig, startTypes, startTypeEnv, Env env, lex, sigLoc)

                val minExplicitSig = sigMinTypes explicitSig and maxExplicitSig = sigMaxTypes explicitSig                 

                (* Match the signature.  This instantiates entries in typeMap. *)
                val matchResults = matchSigs (resSig, explicitSig, fn n => displayStruct(str, n), sigLoc, lex, startTypeEnv, typeEnv);
                val rSig =
                    if opaque
                    then
                    let
                        (* Construct new IDs for the generic IDs.  For each ID in the signature
                           we need to make a new Local ID. *)
                        fun makeNewId(oldId as TypeId{idKind=Bound{ isDatatype, ...}, description = { name, ...}, ...}) =
                        let
                            val description =
                                { location = sigLoc, name = name, description = "Created from opaque signature" }
                        in
                            newTypeId(false, isEquality oldId, isDatatype, description)
                        end
                        |   makeNewId _ = raise InternalError "Not Bound"

                        val sources = List.tabulate(maxExplicitSig-minExplicitSig, fn n => matchResults(n+minExplicitSig))
                        val dests = List.map makeNewId (sigBoundIds explicitSig)
                        (* Add the matching IDs to a list.  When we create the code for
                           the structure we need to create new run-time ID values using
                           the original equality code and a new ref to hold the printer. *)
                        val () = opaqueIds := ListPair.mapEq (fn (s, d) => { source=s, dest=d }) (sources, dests)
                        (* Create new IDs for all the bound IDs in the signature. *)
                        val v = Vector.fromList dests

                        (* And copy it to put in the names from the structure. *)
                        val currentEnv = newTypeIdEnv()
                        fun oldMap n =
                            if n < minExplicitSig
                            then currentEnv n
                            else Vector.sub (v, n - minExplicitSig)
                    in
                        makeSignature(sigName explicitSig, sigTab explicitSig, currentTypeCount(), currentTypeCount(),
                                        sigDeclaredAt explicitSig, composeMaps(sigTypeIdMap explicitSig, oldMap), [])
                    end
                    else (* Transparent: Use the IDs from the structure. *)
                    let
                        val newIdEnv = newTypeIdEnv ()
                        fun matchedIds n = if n < sigMinTypes explicitSig then newIdEnv n else matchResults n
                    in
                        (* The result signature.  This needs to be able to enumerate the type IDs
                           including those we've added. *)
                        makeSignature(sigName explicitSig, sigTab explicitSig, currentTypeCount(), currentTypeCount(),
                                        sigDeclaredAt explicitSig, composeMaps(sigTypeIdMap explicitSig, matchedIds), [])
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
         makeLocalTypeId : (bool * bool * bool * typeIdDescription) -> typeId,
         makeCurrentTypeCount: unit -> int,
         makeTypeIdEnv: unit -> int -> typeId,
         Env env  : env,
         lex,
         lno      : LEX.location,
         structPath: string
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
            fun pass2StructureBind ({name, value, valRef, line, ...}) : unit=
            let (* Each element in the list is a structure binding. *)
                val resSig =
                    structValue(value, makeLocalTypeId, makeCurrentTypeCount, makeTypeIdEnv,
                                Env env, lex, line, structPath ^ name ^ ".");
                (* Any values in the signature are counted as exported. *)
                val () = markValsAsExported resSig;
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
        val () =
            pass2Struct (decs, makeLocalTypeId, makeCurrentTypeCount, makeTypeIdEnv, Env localEnv, lex, lno, structPath);
             
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
             fn pair =>
               (
                #enterVal newEnv pair;
                #enterVal env    pair
               ),
           enterType     =
             fn pair =>
               (
                #enterType newEnv pair;
                #enterType env    pair
               ),
           enterFix      = #enterFix     localEnv,
           enterStruct   =
             fn pair =>
               (
                #enterStruct newEnv pair;
                #enterStruct env    pair
               ),
           enterSig      =
             fn pair =>
               (
                #enterSig newEnv pair;
                #enterSig env    pair
               ),
           enterFunct    = #enterFunct   localEnv
          };
      in 
        (* Now the body. *)
        pass2Struct (body, makeLocalTypeId, makeCurrentTypeCount, makeTypeIdEnv, Env bodyEnv, lex, lno, structPath)
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
               fn (pair as (_,v)) =>
                 (
                   #enterVal env pair;
                   vars := !vars @ [CoreValue v]
                 ),
             enterType     =
               fn (pair as (_,t)) =>
                 (
                   #enterType env pair;
                   vars := !vars @ [CoreType t]
                 ),
             enterFix      =
               fn pair =>
                 (
                   #enterFix env pair;
                   vars := !vars @ [CoreFix pair]
                 ),
             (* This will only be used if we do `open A' where A
                contains sub-structures. *)
             enterStruct   =
               fn (pair as (_,v)) =>
                 (
                   #enterStruct env pair;
                   vars := !vars @ [CoreStruct v]
                 ), 
             enterSig      = #enterSig     env,
             enterFunct    = #enterFunct   env
           };

            (* Create a new type ID for each new datatype.  Add the structure path to the
               name. *)
            (* Temporarily, if this is a type abbreviation always make a free ID and don't include
               it in any lists. *)
            fun makeId (eq, isdt, (_, EmptyType), { location, name, description }) =
                    makeLocalTypeId(true, eq, isdt,
                        { location = location, name = structPath ^ name, description = description })
            |   makeId (_, _, (typeVars, decType), { location, name, description }) =
                    makeFreeId(Global CodeZero, false,
                        { location = location, name = structPath ^ name, description = description },
                        (typeVars, decType))

            (* Process the body and discard the type. *)
            val _ : types = pass2 (dec, makeId, Env newEnv, lex, fn _ => false);
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
        (* Create the free identifiers.  These are initially Bound but are replaced
           by Free after the code has been executed and we have the values for the
           printer and equality functions.  They are only actually created in
           strdecs but functor or signature topdecs in the same program could
           refer to them. *)
        local
            val typeIds = ref []
        in
            fun topLevelId(isVar, eq, isdt, description) =
            let
                val idNumber = topLevelIdNumber()
                val newId =
                    (if isVar then makeBoundIdWithEqUpdate else makeBoundId)
                        (Local{addr = ref 0, level = ref 0}, idNumber, eq, isdt, description, ([], EmptyType))
            in
                typeIds := newId :: ! typeIds;
                newId
            end

            and topLevelIdNumber() = List.length(!typeIds)

            and makeTopLevelIdEnv() =
                (* When we process a topdec we create a top-level ID environment which
                   matches any ID numbers we've already created in this "program" to the
                   actual ID.  Generally this will be empty. *)
                let
                    val typeVec = Vector.fromList(List.rev(!typeIds))
                in
                    fn n => Vector.sub(typeVec, n)
                end
        end

        (* We have to check that a type does not contain free variables and turn them into
           unique monotypes if they exist.  This is a bit messy.  We have to allow subsequent
           structure declarations to freeze the types (there's an example on p90 of the
           Definition) but we can't functors to get access to unfrozen free variables because
           that can break the type system.  *)
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

        fun checkVariables (newValEnv, newStrEnv) =
        (
            #apply newValEnv
                (fn (s: string, v: values) => checkValueForFreeTypeVariables(s, v));
            #apply newStrEnv (
                fn (n: string, s: structVals) =>
                    checkStructSigForFreeTypeVariables(n^".", structSignat s))
        )
            
        fun pass2TopDec ([], envs) = List.app checkVariables envs

        |   pass2TopDec (StrDec(str, typeIds)::rest, envs) =
            let
                (* Remember the top-level Ids created in this strdec. *)
                fun makeId(isVar, eq, isdt, desc) =
                let
                    val newId = topLevelId(isVar, eq, isdt, desc)
                in
                    typeIds := newId :: ! typeIds;
                    newId
                end
            in
                (* strdec: structure or core-language topdec. *)
                pass2Struct([str], makeId, topLevelIdNumber, makeTopLevelIdEnv, Env env, lex, location lex, "");

                pass2TopDec(rest, if errorOccurred lex then [] else (newValEnv, newStrEnv) :: envs)
            end
   
        |   pass2TopDec((topdec as FunctorDec (structList : functorBind list, lno)) :: rest, envs) =
            let
                val () = List.app checkVariables envs (* Check previous variables. *)

                (* There is a restriction that the same name may not be bound twice.
                   As with other bindings functor bindings happen in parallel.
                   DCJM 6/1/00. *)
                val sEnv =  (* The new names. *)
                  noDuplicates 
                    (fn (name, _, _) =>
                        errorNear(lex, true, fn n => displayTopDec(topdec, n), lno,
                            "Functor " ^ name ^ " has already been bound in this declaration")
                    );

                val startTopLevelIDs = topLevelIdNumber()
                and topLevelEnv = makeTopLevelIdEnv()

                (* Put the new names into this environment. *)
                fun pass2FunctorBind
                    {name,
                     arg = {name = argName, sigStruct = argSig, valRef = argVal},
                     body, valRef, resIds, line, ...} =
                let
                    (* When we apply a functor we share type IDs with the argument if they
                       have an ID less than sigMinTypes for the result signature and treat
                       other IDs as generative.  If we don't have an explicit result
                       signature or if we have a transparent signature the type IDs in the
                       result are those returned from the body.  To keep the argument IDs
                       separate from newly created IDs we start creating local IDs with
                       offsets after the args. *)
                    val typeStamps = ref startTopLevelIDs;
                    val localStamps = ref []

                    val argumentSignature =
                        sigVal (argSig, startTopLevelIDs, topLevelEnv, Env env, lex, line)

                    val resArg = makeLocalStruct (argName, argumentSignature, [DeclaredAt line]);

                    (* sigVal will have numbered the bound IDs to start at startTopLevelIDs.  We
                       need to enter these bound IDs into the local environment but as Selected entries. *)
                    local
                        fun mkId(TypeId{idKind=Bound{ eqType, isDatatype, offset, ...},
                                        description={ location, name, description}, access = Formal addr, typeFn, ...}) =
                            TypeId{idKind=Bound { offset = offset, eqType = eqType, isDatatype = isDatatype },
                                    description =
                                    {
                                        location=location,
                                        (* Add the structure name to the argument type IDs. *)
                                        name=if argName = "" then name else argName^"."^name,
                                        description=description
                                    },
                                    access = makeSelected(addr, resArg),
                                    typeFn = typeFn }
                        |   mkId _ = raise InternalError "mkId: Not Bound or not Formal"                            
                    in
                        (* argIDVector is part of the environment now. *)
                        val argIDVector = Vector.fromList(List.map mkId (sigBoundIds argumentSignature))
                        val () = typeStamps := !typeStamps + List.length(sigBoundIds argumentSignature) 
                    end

                    val startBodyIDs = ! typeStamps; (* After the arguments. *)

                    local
                        (* We also have to apply the above map to the signature.  Structures may not
                           have Formal entries for their type IDs so we must map them to the
                           Selected items.  Actually, there's a nasty sort of circularity here;
                           we'd like the Selected entries to use structArg as the base but we
                           can't create it until we have its signature...which uses structArg. *)
                        val argSigWithSelected =
                        let
                            val Signatures { tab, name, declaredAt, typeIdMap, ...} = argumentSignature
                            fun mapToSelected n =
                                if n < startTopLevelIDs then topLevelEnv n
                                else Vector.sub(argIDVector, n-startTopLevelIDs)
                        in
                            makeSignature(name, tab, startBodyIDs, startBodyIDs, declaredAt,
                                composeMaps(typeIdMap, mapToSelected), [])
                        end
                    in
                        val argEnv = makeEnv (makeSignatureTable()); (* Local name space. *)

                        (* We may either have a single named structure in which case that is the argument or
                           effectively a sig...end block in which case we have to open a dummy structure. *)
                        val () = 
                            if argName <> ""
                            then (* Named structure. *)
                            let
                                val structArg =
                                    Struct { name = argName, signat = argSigWithSelected, access = structAccess resArg,
                                             locations=structLocations resArg }
                            in
                                #enterStruct argEnv (argName, structArg)
                            end
                            else (* Open the dummy argument. Similar to "open" in treestruct. *)
                                COPIER.openSignature 
                                (argSigWithSelected,
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
                                "")
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

                    local
                        (* Create local IDs for any datatypes declared in the body or any generative
                           functor applications. *)
                        fun newTypeId(isVar, eq, isdt, desc) =
                        let
                            val n = !typeStamps
                            val () = typeStamps := n + 1;
                            val newId =
                                (if isVar then makeBoundIdWithEqUpdate else makeBoundId)
                                    (Local{addr = ref 0, level = ref 0}, n, eq, isdt, desc, ([], EmptyType))
                        in
                            localStamps := newId :: !localStamps;
                            newId
                        end

                        fun typeIdEnv () =
                        let
                            val localIds = Vector.fromList(List.rev(! localStamps))
                        in
                            fn n =>
                                if n < startTopLevelIDs
                                then topLevelEnv n
                                else if n < startBodyIDs
                                then Vector.sub(argIDVector, n-sigMinTypes argumentSignature)
                                else Vector.sub(localIds, n-startBodyIDs)
                        end
                    in
                        val resSig =
                            structValue(body, newTypeId, fn () => !typeStamps, typeIdEnv,
                                        Env envWithArgs, lex, line, "")                        
                        val () =
                            if errorOccurred lex then ()
                            else checkStructSigForFreeTypeVariables(name^"().", resSig)
                        (* This counts as a reference. *)
                        val () = markValsAsExported resSig
                    end;

                    local
                        val startRunTimeOffsets = getNextRuntimeOffset resSig

                        fun convertId(n, id as TypeId{idKind=Bound { offset, isDatatype, ...}, description, typeFn, ...}) =
                                (* Either inherited from the argument or a new type ID. *)
                                makeBoundId (Formal(n + startRunTimeOffsets), offset, isEquality id, isDatatype, description, typeFn)
                        |   convertId (_, id) = id (* Free or TypeFunction. *)

                        val localVector = Vector.fromList(List.rev(!localStamps))
                        val bodyVec = Vector.mapi convertId localVector

                        val Signatures { name, tab, typeIdMap, declaredAt, ...} = resSig
                        (* These local IDs are included in the bound ID range for the result
                           signature.  Since they were created in the functor new instances
                           will be generated when the functor is applied. *)
                        val newBoundIds = Vector.foldr (op ::) [] bodyVec

                        (* Record the ID map for code-generation. *)
                        val () =
                            resIds :=
                                Vector.foldri(fn (n, b, r) => { source=b, dest=Vector.sub(bodyVec, n)} :: r) [] localVector

                        fun resTypeMap n =
                            if n < startTopLevelIDs
                            then topLevelEnv n
                            else if n < startBodyIDs
                            then Vector.sub(argIDVector, n-sigMinTypes argumentSignature)
                            else Vector.sub(bodyVec, n-startBodyIDs)
                    in
                        val functorSig =
                            makeSignature(name, tab, startBodyIDs, startBodyIDs+Vector.length bodyVec,
                                declaredAt, composeMaps(typeIdMap, resTypeMap), newBoundIds)
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
                    #enter sEnv (name, var);
                    valRef := var
                end
            in
                (* Each element in the list is a functor binding. *)
                List.app pass2FunctorBind structList;
                (* Put them into the enclosing env. *)
                #apply sEnv (#enterFunct env);
                pass2TopDec(rest, [])       
            end (* FunctorDec *)

        |   pass2TopDec((topdec as SignatureDec (structList : sigBind list, lno)) :: rest, envs) =
            let
                val () = List.app checkVariables envs (* Check previous variables. *)

                (* There is a restriction that the same name may not be bound twice.
                   As with other bindings functor bindings happen in parallel.
                   DCJM 6/1/00. *)
                val sEnv =  (* The new names. *)
                    noDuplicates 
                    (fn (name, _, _) => 
                      errorNear (lex, true, fn n => displayTopDec(topdec, n), lno, 
                                 "Signature " ^ name ^ " has already been bound in this declaration")
                    );

                val startTopLevelIDs = topLevelIdNumber()
                and topLevelEnv = makeTopLevelIdEnv()

                fun pass2SignatureBind ({name, sigStruct, line, sigRef, ...}) =
                let (* Each element in the list is a signature binding. *)
                    val Signatures { tab, typeIdMap, minTypes, maxTypes, boundIds, ...} =
                       sigVal (sigStruct, startTopLevelIDs, topLevelEnv, Env env, lex, line)
                    val namedSig = (* Put in the signature name. *)
                        makeSignature(name, tab, minTypes, maxTypes, line, typeIdMap, boundIds)
                in
                    sigRef := namedSig; (* Remember for pass4. *)
                    #enter sEnv (name, namedSig)
                end
            in
                List.app pass2SignatureBind structList;
                (* Put them into the enclosing env. *)
                #apply sEnv (#enterSig env) ;
                pass2TopDec(rest, [])
            end
    in 
        pass2TopDec(strs, []);
        (* Mark any exported values as referenced. *)
        #apply newValEnv
            (fn (s: string, _: values) =>
                (
                    (* If we have exported the value we need to mark it as a
                       reference.  But if the identifier has been rebound we
                       only want to mark the last reference.  Looking the
                       identifier up will return only the last reference. *)
                    case #lookup newValEnv s of
                        SOME(Value { references=SOME{exportedRef, ...}, ...}) =>
                            exportedRef := true
                    |   _ => ()
                )
            )
    end (*pass2Structs *);
    
    
    


  (*                            *
   *     Code-generation phase. *
   *                            *)

  (* Generate code from the expressions and arrange to return the results
      so that "pass4" can find them. *)
    fun gencodeStructs ((strs, _), lex) =
    let
        val debugging = getParameter debugTag (debugParams lex)

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
        (_: 'a * debugenv -> {code: codetree list, load: codetree list, debug: debugenv})
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

    fun matchStructure (code : codetree, source : univTable, sourceIds: int*bool->valAccess, dest : signatures) =
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
                            mkInd (vaFormal access, #load decs (!level))
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
                        Value{access=Formal addr, typeOf=destTypeOf, ...} =>
                        let
                            val sourceVal as Value{typeOf=sourceTypeOf, ...} =
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

                            (* This may have a different level of polymorphism in which case we
                               have to apply it to the arguments and if necessary add a function in there. *)
                            val (_, destPolyVars) = generalise destTypeOf
                            val destPolymorphism = List.length destPolyVars
                            val (_, sourcePolyVars) = generalise sourceTypeOf
                            val sourcePolymorphism = List.length sourcePolyVars

                            (* If we have to generate a function then the level will be higher.*)
                            val oldLevel = !level
                            val () =
                                if destPolymorphism = sourcePolymorphism orelse destPolymorphism = 0
                                then ()
                                else level := !level+1;
                                
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
                                            mkTuple [mkInd (svAddr, #load decs (!level)), mkStr (valName dval), CodeZero]
                                        else
                                            mkProc 
                                            (mkTuple
                                               [mkInd (svAddr, #load decs (!level + 1)),
                                                mkStr (valName dval),
                                                mkLoad (~1, 0)],
                                            1, 1, "")
                                        else if constrBecomesVal
                                        then mkInd(1, mkInd (svAddr, #load decs (!level)))
                                        else mkInd (svAddr, #load decs (!level))
                                    )

                                | _ =>
                                    (
                                        useOriginal := false;
                                        if excBecomesVal
                                        then codeExFunction(sourceVal, !level, [], lex, location nullLex)
                                        else
                                        let
                                            val valu =
                                                codeVal (sourceVal, !level, [], lex, location nullLex)
                                        in
                                            if constrBecomesVal
                                            then mkInd(1, valu)
                                            else valu
                                        end
                                    )

                            val () = level := oldLevel;

                            fun applyToInstance([], code) = code
                            |   applyToInstance(polyVars, code) = mkEval(code, List.map(fn _ => CodeZero) polyVars, true)

                            val polyCode =
                                if destPolymorphism = sourcePolymorphism
                                then code (* Nothing to do. *)
                                else if destPolymorphism = 0
                                then (* Destination is monomorphic. *)
                                (
                                    useOriginal := false;
                                    applyToInstance(sourcePolyVars, code)
                                )
                                else (* Destination is polymorphic. *)
                                (
                                    useOriginal := false;
                                    (* N.B.  Normally we remove polymorphism by matching a polymorphic value
                                       to a monomorphic signature item but we CAN add polymorphism through
                                       type abbreviations e.g. type 'a t = int. *)
                                    mkProc(applyToInstance(sourcePolyVars, code), !level+1, destPolymorphism, dName^"(P)")
                                )
                        in
                            addToList polyCode addr
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
                    fun doTypeID (typeId as TypeId{ access = Formal addr, ...}, n) =
                        let
                            (* Get the corresponding source ID. *)
                            val codedId = codeAccess(sourceIds(n, isEquality typeId), !level)
                        in
                            useOriginal := false;
                            addToList codedId addr;
                            n+1
                        end
                    |   doTypeID _ = raise InternalError "doTypeID: Not Formal"
                in
                    List.foldl doTypeID 0 (sigBoundIds dest);
                    ()
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
    fun loadOpaqueIds opaqueIds =
    let
        fun decId { dest, source } =
        let
            val { addr=idAddr, level=idLevel } = vaLocal(idAccess dest)
            val addr = mkAddr();
            val () = idAddr := addr and () = idLevel := ! level;
            val idCode = codeGenerativeId(source, isEquality dest, !level)
        in
            mkDec(addr, idCode)
        end
    in
        List.map decId opaqueIds
    end

    (* Code-generate a structure value. *)
    fun structureCode (str, strName, debugEnv): { code: codetree list, load: codetree, ressig: signatures } =
    case str of
        FunctorAppl {arg, valRef = ref functs,
                     argIds=ref argIds, resIds=ref resIds, resultSig=ref resultSig, ...} =>
        let
            val {code = argCodeSource, load = argLoadSource, ressig = argCodeSig} =
                structureCode (arg, strName, debugEnv)
            (* Match the actual argument to the required arguments. *)
            fun getMatchedId(n, isEq) =
            let
                val id = #source(List.nth (argIds, n))
            in
                case id of
                    TypeId{typeFn=(_, EmptyType), ...} => idAccess id
                |   _ => (* Have to generate a function here. *)
                        Global(codeGenerativeId(id, isEq, !level))
            end
            val argCode =
                matchStructure(argLoadSource, sigTab argCodeSig, getMatchedId, structSignat (functorArg functs))

            (* To produce the generative type IDs we need to save the result vector returned by the
               functor application and then generate the new type IDs from the IDs in it.  To make valid
               source IDs we have to turn the Formal entries in the signature into Selected entries. *)
            val resAddr = mkAddr()
            local 
                val dummyResStruct = makeLocalStruct("", resultSig, []) (* Dummy structure. *)
                val resl = vaLocal (structAccess dummyResStruct);
                val () = #addr  resl := resAddr; 
                val () = #level resl := !level
                fun mkSelected {
                        source = TypeId{idKind, access = Formal addr, description, typeFn}, dest } =
                    { source = TypeId{idKind=idKind, access = makeSelected(addr, dummyResStruct), description = description, typeFn=typeFn },
                      dest = dest }
                |   mkSelected _ = raise InternalError "makeSelected: Not Bound or not Formal"
                val resultIds = List.map mkSelected resIds
            in
                val loadIds = loadOpaqueIds resultIds
            end
        in
            (* Evaluate the functor. *)
            {
                code =
                    argCodeSource @
                    (mkDec(resAddr, mkEval (codeAccess (functorAccess functs, !level), [argCode], false)) ::
                     loadIds),
                load = matchStructure(mkLoad(resAddr, 0), sigTab(functorResult functs), fn _ => raise Subscript, resultSig),
                ressig = resultSig
            }
        end

    |   StructureIdent {valRef = ref v, ...} =>
            { code = [], load = codeStruct (v, !level), ressig = structSignat v }

    |   Localdec {decs, body = [localStr], ...} =>
        let (* let strdec in strexp end *)
            (* Generate the declarations but throw away the loads. *)
            (* TODO: Get the debug environment correct here. *)
            val coded = 
                mapPair (fn (str, debug) => codeStrdecs (strName, str, debug)) decs debugEnv;
            val {code = bodyCode, load = bodyLoad, ressig = bodySig} =
                structureCode (localStr, strName, #debug coded)
        in
            {
                code = #code coded @ bodyCode,
                load = bodyLoad,
                ressig = bodySig
            }
        end

      | StructDec {alist, value, resultSig=ref resultSig, ...} =>
        let
            val coded = mapStrdecs alist strName debugEnv;
            (* We match to the dummy signature here.  If there is a signature outside
               we will match again.  This results in double copying but that should
               all be sorted out by the optimiser. *)
        in 
          (* The result is a block containing the declarations and
             code to load the results. *)
            {
                code = #code coded,
                load = matchStructure (mkTuple (#load coded), value, fn _ => raise Subscript, resultSig),
                ressig = resultSig
            }
        end

    |   SigConstraint { str, opaqueIds=ref opaqueIds, resultSig = ref resultSig, ... } =>
        let
            val {code = strCode, load = strLoad, ressig = strSig} = structureCode (str, strName, debugEnv)
            val tempDecs = multipleUses (strLoad, mkAddr, !level);
            val ids = loadOpaqueIds opaqueIds
        in
            {
                code = strCode @ #dec tempDecs @ ids,
                load = matchStructure (#load tempDecs (!level), sigTab strSig, fn _ => raise Subscript, resultSig),
                ressig = resultSig
            }
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
                fun codeStructureBind ({name, value, valRef, ...}: structBind, debug) =
                let
                    val sName = strName ^ name ^ "."
                    val {code = strCode, load = strLoad, ...} = structureCode (value, sName, debug)
                    val addr = mkAddr(); 
                    val var  = vaLocal (structAccess (!valRef));
                    val () = #addr var  := addr; 
                    val () = #level var := !level;
                in (* Get the code and save the result in the variable. *)
                    {
                        code = strCode @ [mkDec (addr, strLoad)],
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
            fun filterVals (CoreValue v)  = SOME(codeVal (v, !level, [], nullLex, location nullLex))
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

                (* The debug environment has to contain at a minimum the type IDs from the arguments. *)
                val (fBindDebugDecs, fBindDebugEnv) =
                    if not debugging then ([], debugEnv)
                    else
                    let
                        val argIds = sigBoundIds (structSignat argVal)

                        fun loadTypeId(id as TypeId { access = Formal addr, ... }, (ctEnv, rtEnv)) =
                        let
                            (* This code will build a cons cell containing the run-time value
                               associated with the type Id as the hd and the rest of the run-time
                               environment as the tl. *)
                            val loadTypeId = mkInd(addr, mkLoad (~1, 0))
                            val newEnv = mkTuple [ loadTypeId, rtEnv(!level) ]
                            val { dec, load } = multipleUses (newEnv, mkAddr, !level)
                        in
                            (dec, (envTypeId id :: ctEnv, load))
                        end
                        |   loadTypeId _ = raise InternalError "loadTypeId: Not bound"

                        fun foldIds(id::ids, inEnv) =
                            let
                                val (dec, outEnv) = loadTypeId(id, inEnv)
                                val (decs, newEnv) = foldIds(ids, outEnv)
                            in
                                (dec @ decs, newEnv)
                            end
                        |   foldIds([], debugEnv) = ([], debugEnv)
                    in
                        foldIds(argIds, debugEnv)
                    end

                (* Process the body and make a procedure out of it. *)
                local
                    val {code = strCode, load = strLoad, ressig = strSig} = structureCode (body, name ^ "().", fBindDebugEnv)
                    fun getIds(n, isEq) =
                    let
                        val id = #source(List.nth(resIds, n))
                    in
                        case id of
                            TypeId{typeFn=(_, EmptyType), ...} => idAccess id
                        |   _ => (* Have to generate a function here. *)
                                Global(codeGenerativeId(id, isEq, !level))
                    end
                    val matchedCode = matchStructure (strLoad, sigTab strSig, getIds, functorResult func)
                in
                    val functorCode = (* The function that implements the functor. *)
                        (if getParameter inlineFunctorsTag (debugParams lex) then mkMacroProc else mkProc)
                            (mkEnv(fBindDebugDecs @ strCode @ [matchedCode]), !level, 1, name);
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
                    val structCode = mkInd (addr, results);
                    (* We need to replace type IDs with their Global versions. *)
                    
                    local
                        val Signatures { name, declaredAt, typeIdMap, tab, ...} = structSignat (!valRef);
                    in
                        val resultSig =
                            makeSignature(name, tab, 0, 0, declaredAt, composeMaps(typeIdMap, mapTypeIds), [])
                    end
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
                fun copyId(TypeId{idKind=Bound{ offset, ...}, ...}) = SOME(mapTypeIds offset)
                |   copyId _ = NONE
                fun replaceTypeConstrs tcon = copyTypeConstr (tcon, copyId, fn x => x, fn s => s)
            in
                copyType(t, fn tv=>tv, replaceTypeConstrs)
            end

            fun makeDecs (CoreValue(Value{class, name, typeOf, locations, access, ...}),
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
                        Value{class=class, name=name, typeOf=replaceTypes typeOf, access=newAccess,
                              locations=locations, references = NONE}
                in
                    (addr+1, { fixes=fixes, values=(name, newVal) :: values, structures=structures,
                               signatures=signatures, functors=functors, types=types } )
                end

            |   makeDecs (CoreStruct dec, (addr, {fixes, values, structures, signatures, functors, types})) =
                (* If we open a structure we've created in the same "program" we may have a non-global
                   substructure.  We have to process any structures and also map any type IDs. *)
                let
                    local
                        val Signatures { name, declaredAt, typeIdMap, tab, ...} = structSignat dec
                    in
                        val resultSig =
                            makeSignature(name, tab, 0, 0, declaredAt, composeMaps(typeIdMap, mapTypeIds), [])
                    end
                    val strName = structName dec
                    val newStruct =
                        Struct { name = strName, signat = resultSig,
                                 access = Global(mkInd (addr, results)), locations = structLocations dec }
                in
                 (addr+1, { fixes=fixes, values=values, structures=(strName, newStruct) :: structures,
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
    
    fun extractTopDec(str, (addr, env as { fixes, values, structures, signatures, functors, types }, nIds, mapPrevTypIds)) =
    case str of
        StrDec(str, ref typeIds) =>
        let
            (* Create new Free IDs for top-level IDs. *)            
            fun loadId(TypeId{idKind=Bound{eqType, ...}, description, typeFn, ...}, (n, ids)) =
            let
                val newId = makeFreeId(Global(mkInd(n, results)), pling eqType, description, typeFn)
            in
                (n+1, newId :: ids)
            end
            |   loadId _ = raise InternalError "Not Bound"

            (* Construct the IDs and reverse the list so the first ID is first*)
            val (newAddr, mappedIds) = List.foldl loadId (addr, []) typeIds
            val idMap = Vector.fromList mappedIds
            fun mapTypeIds n =
                if n < nIds then mapPrevTypIds n else Vector.sub(idMap, n-nIds)
            val (resAddr, resEnv) = extractStruct (str, mapTypeIds, (newAddr, env))
        in
            (resAddr, resEnv, nIds + Vector.length idMap, mapTypeIds)
        end

    |   FunctorDec (structList : functorBind list, _) =>
        let
            (* Get the functor values. *)
            fun extractFunctorBind ({name, valRef, ...}: functorBind, (addr, funcs)) =
            let
                val code = mkInd (addr, results);
                val func = !valRef;
                (* We need to convert any references to typeIDs created in strdecs in the
                   same "program". *)
                (* The result signature shares with the argument so we only copy IDs less than
                   the min for the argument signature. *)
                
                local
                    val fnArg = functorArg func
                    val fnArgSig = structSignat fnArg
                    val fnArgName = structName fnArg
                    val Signatures { name, tab, typeIdMap, boundIds, minTypes, maxTypes, declaredAt, ... } = fnArgSig
                    fun newMap n =
                    if n < minTypes
                    then mapPrevTypIds n
                    else List.nth(boundIds, n-minTypes)
                in
                    val functorArgSig =
                        makeSignature(name, tab, minTypes, maxTypes, declaredAt, composeMaps(typeIdMap, newMap), boundIds)
                    val copiedArg =
                        Struct{name=fnArgName, signat=functorArgSig,
                               access=structAccess fnArg, locations=structLocations fnArg}
                end
                local
                    val Signatures { name, tab, typeIdMap, boundIds, minTypes, maxTypes, declaredAt, ... } =
                            functorResult func
                    val Signatures { boundIds=argBoundIds, minTypes=argMinTypes, ...} = functorArgSig
                    val fnArgName = structName(functorArg func)
                    fun newMap n =
                    if n >= minTypes
                    then List.nth(boundIds, n-minTypes)
                    else if n >= argMinTypes
                    then case List.nth(argBoundIds, n-argMinTypes) of
                        (* Add the argument structure name onto the name of type IDs in the argument. *)
                        TypeId{ access, idKind, typeFn, description={location, name, description}} =>
                            TypeId { access=access, idKind=idKind, typeFn=typeFn,
                                    description=
                                        {
                                            location=location, description=description,
                                            name=if fnArgName = "" then name else fnArgName^"."^name
                                        }
                                   }
                    else mapPrevTypIds n
                in
                    val functorSigResult =
                        makeSignature(name, tab, minTypes, maxTypes, declaredAt, composeMaps(typeIdMap, newMap), boundIds)
                end
                val funcTree = 
                    makeFunctor(functorName func, copiedArg, functorSigResult,
                            makeGlobal code, functorDeclaredAt func);
            in
                (addr + 1, (name, funcTree) :: funcs)
            end
            val (newAddr, newfunctors ) = List.foldl extractFunctorBind (addr, functors) structList
        in
            (newAddr, { functors=newfunctors, fixes=fixes, values=values,
                      signatures=signatures, structures=structures, types=types }, nIds, mapPrevTypIds)
        end

     | SignatureDec (structList : sigBind list, _) =>
        let
            (* We need to convert any references to typeIDs created in strdecs in the same "program". *)
            fun copySignature fnSig =
            let
                val Signatures { name, tab, typeIdMap, minTypes, maxTypes, boundIds, declaredAt, ... } = fnSig
                fun mapIDs n =
                    if n < minTypes
                    then mapPrevTypIds n
                    else List.nth(boundIds, n-minTypes)
            in   
                makeSignature(name, tab, minTypes, maxTypes, declaredAt, composeMaps(typeIdMap, mapIDs), boundIds)
            end
            val newSigs = List.map (fn ({sigRef=ref s, name, ...}: sigBind) => (name, copySignature s)) structList
        in
           (addr, { fixes=fixes, values=values, structures=structures,
                    signatures=newSigs @ signatures, functors=functors, types=types }, nIds, mapPrevTypIds)
        end
    
    val empty = { fixes=[], values=[], structures=[], functors=[], types=[], signatures=[] }

    val (_, result, _, _) = List.foldl extractTopDec (0, empty, 0, fn _ => raise Subscript) strs;
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
        type typeVarForm    = typeVarForm
        type sigs           = sigs
    end
end;
