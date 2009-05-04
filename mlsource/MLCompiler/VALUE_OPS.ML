(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Modified David C.J. Matthews 2008-9.

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
    Title:      Operations on global and local values.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1986
*)

functor VALUE_OPS (

(*****************************************************************************)
(*                  LEX                                                      *)
(*****************************************************************************)
structure LEX : LEXSIG;

(*****************************************************************************)
(*                  CODETREE                                                 *)
(*****************************************************************************)
structure CODETREE :
sig
  type machineWord
  type codetree
  
  val CodeNil:          codetree;
  val CodeTrue:         codetree;
  val CodeZero:         codetree;
  val isCodeNil:        codetree -> bool;
  val mkTuple:          codetree list -> codetree;
  val mkRecLoad:        int -> codetree;
  val mkLoad:           int * int -> codetree;
  val mkInd:            int * codetree -> codetree;
  val mkConst:          machineWord -> codetree;
  val mkEnv:            codetree list -> codetree;
  val mkProc:           codetree * int * int * string -> codetree;
  val mkInlproc:        codetree * int * int * string -> codetree;
  val mkEval:           codetree * codetree list * bool -> codetree;
  val mkStr:            string   -> codetree;
  val mkRaise:          codetree -> codetree;
  val mkNot:            codetree -> codetree;
  val mkTestnull:       codetree -> codetree;
  val mkTestnotnull:    codetree -> codetree;
  val mkTestinteq:      codetree * codetree -> codetree;
  val mkTestptreq:      codetree * codetree -> codetree;
  val mkCand:           codetree * codetree -> codetree;
  val mkCor:              codetree * codetree -> codetree;
  val mkMutualDecs:       codetree list -> codetree;
  val mkIf:               codetree * codetree * codetree -> codetree;
  val mkDec:              int * codetree -> codetree;
  val evalue:           codetree -> machineWord;
  
  val structureEq:      machineWord * machineWord -> bool

  val genCode:          codetree * Universal.universal list -> unit -> codetree
end;
    
(*****************************************************************************)
(*                  STRUCTVALS                                               *)
(*****************************************************************************)
structure STRUCTVALS : STRUCTVALSIG;

(*****************************************************************************)
(*                  TYPESTRUCT                                               *)
(*****************************************************************************)
structure TYPESTRUCT : TYPETREESIG

(*****************************************************************************)
(*                  PRINTTABLE                                               *)
(*****************************************************************************)
structure PRINTTABLE :
sig
  type typeConstrs
  type codetree
  
  val addOverload: string * typeConstrs * codetree -> unit
  val getOverloads: string -> (typeConstrs * codetree) list
  val getOverload: string * typeConstrs * (unit->codetree) -> codetree
end;

(*****************************************************************************)
(*                  UNIVERSALTABLE                                           *)
(*****************************************************************************)
structure UNIVERSALTABLE:
sig
  type universal = Universal.universal
  type univTable
  type 'a iter

  val univOver: univTable -> (string * universal) iter;
end;

(*****************************************************************************)
(*                  DEBUG                                                    *)
(*****************************************************************************)
structure DEBUG :
sig
    val printDepthFunTag : (unit->int) Universal.tag
    val errorDepthTag: int Universal.tag
    val getParameter :
       'a Universal.tag -> Universal.universal list -> 'a
end;

(*****************************************************************************)
(*                  MISC                                                     *)
(*****************************************************************************)
structure MISC :
sig
  exception InternalError of string; (* compiler error *)
  
  val quickSort : ('a -> 'a -> bool) -> 'a list -> 'a list
  
  type 'a iter;
  val for    : 'a iter -> ('a -> 'b) -> unit
  val iterList : 'a iter -> 'a list
end;


(*****************************************************************************)
(*                  PRETTYPRINTER                                            *)
(*****************************************************************************)
structure PRETTY : PRETTYSIG;

structure ADDRESS :
sig
  type machineWord;      (* any legal bit-pattern (tag = 0 or 1) *)
  type address;   (* an object that's represented as a pointer *) 
  type short = Word.word;   (* an object that's represented as a 30-bit int *)

  val wordEq : 'a * 'a -> bool

  val unsafeCast : 'a -> 'b;
  
  val isShort   : 'a -> bool;

  (* The following casts are always safe *)
  val toMachineWord : 'a    -> machineWord;
  
  (* The following casts are checked at run-time *)
  val toShort   : 'a -> short;
  val toAddress : 'a -> address;
  
  val alloc:  (short * Word8.word * machineWord)  -> address;

  val loadByte:   (address * short) -> Word8.word;
  val loadWord:   (address * short) -> machineWord;
  val assignWord: (address * short * machineWord)  -> unit
  val lock:       address -> unit

  val length: address -> short;
   
  val F_words     : Word8.word;
  val F_bytes     : Word8.word;
  val F_mutable   : Word8.word;
end;

(*****************************************************************************)
(*                  UTILITIES                                                *)
(*****************************************************************************)
structure UTILITIES :
sig
  val splitString: string -> { first:string,second:string }
end;

structure COPIER: COPIERSIG

structure TYPEIDCODE: TYPEIDCODESIG

(*****************************************************************************)
(*                  VALUEOPS sharing constraints                             *)
(*****************************************************************************)

sharing STRUCTVALS.Sharing = TYPESTRUCT.Sharing = LEX.Sharing = PRETTY.Sharing
      = COPIER.Sharing = CODETREE = PRINTTABLE = ADDRESS = UNIVERSALTABLE = MISC
      = TYPEIDCODE.Sharing

) : VALUEOPSSIG =

(*****************************************************************************)
(*                  VALUEOPS functor body                                    *)
(*****************************************************************************)
struct
  open MISC; 
  open PRETTY;
  
  open LEX;
  open CODETREE;
  open TYPESTRUCT; (* Open this first because unitType is in STRUCTVALS as well. *)
  open Universal; (* for tag etc. *)
  open STRUCTVALS;
  open PRINTTABLE;
  open DEBUG;
  open ADDRESS;
  open UNIVERSALTABLE;
  open RuntimeCalls; (* for POLY_SYS and EXC numbers *)
  open UTILITIES;
  open TYPEIDCODE;
  
  val lengthW  = ADDRESS.length;
  val length   = List.length;
  
  (* gets a value from the run-time system; 
    usually this is a closure, but sometimes it's an int.  *)
  val ioOp : int -> machineWord = RunCall.run_call1 POLY_SYS_io_operation;

  val andb = Word8.andb and orb = Word8.orb
  infix 6 andb;
  infix 7 orb;

(************* "types" constructors copied here to reduce garbage *********)
  fun isTypeVar          (TypeVar          _) = true
    | isTypeVar          _ = false;
     
  fun isTypeConstruction (TypeConstruction _) = true
    | isTypeConstruction _ = false;
     
  fun isFunctionType     (FunctionType     _) = true
    | isFunctionType     _ = false;
    
  fun isLabelled         (LabelledType         _) = true
    | isLabelled         _ = false;
    
  fun isEmpty             EmptyType           = true
    | isEmpty            _ = false;
    
  val emptyType            = EmptyType;
  
  val badType              = BadType;

  type typeConstructionForm = 
      {
        name:  string,
        value: typeConstrs ref,
        args:  types list
      }
         

  (* A function type takes two types, the argument and the result. *)
      
  and functionTypeForm = 
    { 
      arg: types,
      result: types
    }
      
  (* An entry in a labelled record type. *)
      
  and labelledRecEntry = 
    { 
      name: string,
      typeOf: types
    }
      
  (* A fixed labelled record. *)
      
  and labelledRecForm = 
    { 
      recList: { name: string, typeof: types } list,
      frozen: bool
    };

  fun typesTypeVar          (TypeVar          x) = x 
    | typesTypeVar          _ = raise Match;
    
  fun typesTypeConstruction (TypeConstruction x) = x 
    | typesTypeConstruction _ = raise Match;
    
  fun typesFunctionType     (FunctionType     x) = x
     | typesFunctionType     _ = raise Match;
     
  fun typesLabelled         (LabelledType         x) = x
    | typesLabelled         _ = raise Match;
  
  fun lreName   ({name,...}   : labelledRecEntry) = name;
  fun lreTypeOf ({typeOf,...} : labelledRecEntry) = typeOf;

  fun lrfFrozen  ({frozen,...} : labelledRecForm)  = frozen;
  fun lrfRecList ({recList,...} : labelledRecForm) = recList;
  
  fun tcfArgs  ({args,...}  : typeConstructionForm)  = args;
  fun tcfName  ({name,...}  : typeConstructionForm)  = name;
  fun tcfValue ({value,...} : typeConstructionForm) = !value;
(*************)

     (* Functions to construct the values. *)

    fun mkGconstr (name, typeof, code, nullary, constrs, location) =
       makeValueConstr (name, typeof, nullary, constrs, Global code, location);

    (* Global variable *)
    val mkGvar = makeGlobalV

    (* Local variable - Generated by the second pass. *)
    fun mkVar (name, typeof, locations) =  makeLocalV (name, typeof, ref 0, ref 0, locations);

    (* Value in a local structure or a functor argument.  May be simple value, exception
        or constructor. *)
    fun mkSelectedVar (Value { access = Formal addr, name, typeOf, class, locations}, base, openLocs) =
        (* If the argument is "formal" set the base to the base structure. *)
        Value{name=name, typeOf=typeOf, class=class,
            access=Selected{addr=addr, base=base}, locations=openLocs @ locations}

    |   mkSelectedVar (Value { access = Global code, name, typeOf, class, locations}, _, openLocs) =
        (* Global: We need to add the location information. *)
        Value{name=name, typeOf=typeOf, class=class, access=Global code, locations=openLocs @ locations}

    |   mkSelectedVar(selected, _, _) = selected (* Overloaded? *);

    (* Construct a global exception. *)
    fun mkGex (name, typeof, code, locations) =
        Value{ name = name, typeOf = typeof, access = Global code,
           class = Exception, locations = locations }
 
    (* Construct a local exception. *)
    fun mkEx (name, typeof, locations) = 
        Value{ name = name, typeOf = typeof,
           access = Local{addr = ref 0, level = ref 0},
           class = Exception, locations=locations }

 (* Copy a datatype (if necessary), converting the constructors to selections on
    a base structure.  This is used both when opening a structure and also for
    replicating a datatype. *)
 fun mkSelectedType(tcons: typeConstrs, newName: string, baseStruct: structVals option, locations): typeConstrs =
    let
        (* Create a new constructor with the same unique ID. *)
        val typeID = tcIdentifier tcons;
        val newTypeCons =
            makeDatatypeConstr(newName, tcTypeVars tcons, typeID,
                            tcLetDepth tcons, locations);
        
        (* Copy the value constructors. *)
        fun copyAConstructor(Value{name=cName, typeOf, class, access, ...}) =
            let
                (* Copy the types of value constructors replacing
                   occurrences of the old type with the new one.
                   This is not strictly necessary but improves printing.
                   e.g. local datatype X = A | B in datatype Y = datatype X end;
                   A; prints  A: Y rather than A: X *)
                fun copyTypeCons (tcon : typeConstrs) : typeConstrs =
                    if sameTypeId(tcIdentifier tcon, typeID)
                    then newTypeCons
                    else tcon;
                fun copyTyp (t : types) : types =
                   copyType (t, fn x => x, (* Don't bother with type variables. *)
                       copyTypeCons);
                val newType = copyTyp typeOf;
                val newAccess =
                    case (access, baseStruct) of
                        (* If we are opening a structure we must have a base structure
                           and we turn Formal entries into Selected.  If we are replicating
                           a datatype within a signature the original constructors will
                           be Formal. *)
                        (Formal addr, SOME base) => Selected{base=base, addr=addr}
                    |    (Formal _, NONE) => access
                    |    _ => access; (* Probably already a global. *)
            in
                Value{name=cName, typeOf=newType, class=class, access=newAccess, locations=locations}
            end
    
        val newValConstrs = map copyAConstructor (tcConstructors tcons)
    in
        tcSetConstructors(newTypeCons, newValConstrs);
        newTypeCons
    end

(*****************************************************************************)
(*             Representation of Exceptions (incomplete!)                    *)
(*****************************************************************************)
  fun exnId (value : exn) : machineWord = 
    loadWord (toAddress value, 0w0);

  fun exnName (value : exn) : string = 
    unsafeCast (loadWord (toAddress value, 0w1));

  fun exnValue (value : exn) : machineWord = 
    unsafeCast (loadWord (toAddress value, 0w2));

  val toExn : machineWord -> exn = unsafeCast;

(*****************************************************************************)
(*             Representation of Datatype Constructors                       *)
(*****************************************************************************)
  (* These are the possible representations of a value constructor. *)
  datatype representations = 
    RefForm            (* As for OnlyOne but must be a monotype. *)
  | UnitForm           (* If the only value in an enumeration. *)
  | OnlyOne            (* If only one constructor, there is no tag or box. *)
                       (* Could be replaced by "UnboxedForm"? *)
    
  | EnumForm of int    (* Enumeration - argument is the number. (short?) *)
  | BoxedForm          (* Boxed but not tagged (only unary constructor) *)
  | UnboxedForm        (* Unboxed and untagged (only unary constructor) *)

  | ConstForm of machineWord         (* Constant - argument is a tagged value. *)
  | TaggedBox of int          (* Union - tagged and boxed.  i.e. the representation is a
                                 pair whose first word is the tag and second is the value. *)
  | TaggedTuple of int * int  (* Union - tagged but with in-line tuple. i.e. for a
                                   tuple of size n the representation is a tuple of size n+1
                                 whose first word contains the tag. *)


(* This makes the isConsTest "fn x => not (isShort x)" rather than
   "fn x => x <> 0", but that shouldn't give any worse code.
   If it does, we'll have to reinstate ConsForm as a primitive. *)
  val NilForm  = EnumForm 0;
  val ConsForm = UnboxedForm;
  
(* Similarly, this makes the tests for UnitForm and OnlyOne more
   expensive, but we shouldn't be generating them anyway (especially
   as the tests seem buggy). That's what fancy compilation
   of pattern-matching does for you! Of course, if it doesn't work,
   we can always reinstate them. SPF 22/10/94 *)
   
(* Don't do this ...
  val UnitForm = EnumForm 0;
... because it breaks the (hacky!) pretty-printing of arrays.
    (Array values are represented by a UnitForm value constructor;
    if we use an EnumForm instead, constrMatches fails, so we
    get a (handled) exception inside the pretty-printer. This
    is all very sad. The right fix would be to generalise
    use-defined pretty-printers to handle parameterised types, and
    add one of these for arrays.
    SPF 2/7/96 *)
(* I've done that although whether it fixes this particular problem
   or not I don't know. DCJM Sept 2000. *)

  val arg1     = mkLoad (~1, 0); (* saves a lot of garbage *)
  val arg2     = mkLoad (~2, 0);

(* Don't do this ...
    val OnlyOne  = UnboxedForm;
... because that gives the wrong test if the value isn't boxed. SPF 22/10/94 *)

  local
    val mutableFlags = F_words orb F_mutable;
    
    fun abstract (doIt: codetree -> codetree) (name : string) : codetree =
      mkInlproc (doIt arg1, 0, 1, name);
  
    (* we use toShort to check that the tag actually fits in a 30-bit integer *)
    fun mkTag (tag:int) : codetree = mkConst (toMachineWord (toShort tag));

    (* tag now moved into first word SPF 22/10/94 *)
    (* get the tag from a TaggedBox or ConstForm *)
    fun loadTag (u: machineWord) : machineWord = 
      loadWord (toAddress u, 0w0); (* tag is first field *)

    (* get the data from a TaggedBox *)
    fun loadTaggedBoxedValue (u: machineWord) : machineWord = 
      loadWord (toAddress u, 0w1); (* contents is second field *)

    (* get the data from a BoxedForm or a RefForm *)
    fun loadBoxedValue (u: machineWord) : machineWord = loadWord (toAddress u, 0w0);
    
    fun loadTaggedTupleValue (n: int) (u : machineWord) : machineWord =
    let
      val vec = alloc (toShort n, mutableFlags, toMachineWord 0);
      
      fun copyField i =
      let
        val w : machineWord = loadWord (toAddress u, toShort (i + 1));
      in
        assignWord (toAddress vec, toShort i, w) 
      end;
      
      fun copyFields i =
        if i < n then (copyField i; copyFields (i + 1)) else ();
    
      val U : unit = copyFields 0;
      val U : unit = lock vec;
    in
      toMachineWord vec
    end;

    fun identityApplyCode v = v;  (* no-op *)
    fun boxApplyCode v    = mkTuple [v];

    (* Inject into a union. Generate as mkTuple(arg, tag). *)
    (* we use toShort to check that the tag actually fits in a 30-bit integer *)
    fun tagBoxApplyCode (tag:int) (v : codetree) =
      mkTuple [mkTag tag, v];
              
    (* Inject into a tagged tuple - we could improve this if we knew that
       arg is an explicit tuple already, but the optimiser should do this
       anyway. I have also coded this using inline procedures with explicit
       applications because I can't work out how to make a well-formed
       block (because declartion addresses are *not* local to a block?)
       SPF 25/10/94 *)
    fun tagTupleCode (tag:int) (n:int) (name:string) : codetree =
    let
      (* copy n data fields out of ordinary record *)
      fun getFields i =
    if i < n then mkInd (i, arg1) :: getFields (i + 1) else [];
    in  
      mkInlproc (mkTuple (mkTag tag :: getFields 0), 0, 1, name)
    end;

    fun tagTupleApplyCode (tag:int) (n:int) (arg:codetree) : codetree =
      mkEval (tagTupleCode tag n "", [arg], true);

    fun taggedTupleDestructCode (n:int) (arg:codetree) : codetree =
    let
      (* copy n data fields out of tagged record *)
      fun getFields i =
    if i <= n then mkInd (i, arg1) :: getFields (i + 1) else [];
    
      val proc = mkInlproc (mkTuple (getFields 1), 0, 1, "")
    in  
      mkEval (proc, [arg], true)
    end;

    (* allocate 1 mutable word, initialise to "v"; do not evaluate "early" *)
    fun refApplyCode (v: codetree) =
      mkEval
        (mkConst (ioOp POLY_SYS_alloc_store),
        [mkConst (toMachineWord 1), mkConst (toMachineWord mutableFlags), v],
        false);

    (* If we need to make a "real" functions, here's how *)
    val refCode      = abstract refApplyCode "ref";
(*  val consCode     = abstract identityApplyCode "::" *);
    val identityCode = abstract identityApplyCode;
    val boxCode      = abstract boxApplyCode;
    
    fun tagBoxCode (tag:int) = 
      abstract (tagBoxApplyCode tag);
      
    fun tagTupleCode (tag:int) (n:int) =
       abstract (tagTupleApplyCode tag n);

    fun testTag (tag: int) (v: codetree) =
      mkTestinteq (mkTag tag, v);
      
    fun testBoxedTag (tag: int) (v: codetree) : codetree =
      mkTestinteq (mkTag tag, mkInd (0, v)); (* tag is first field *)
       
    fun taggedBoxDestructCode (box : codetree) : codetree = 
      mkInd (1, box); (* contents is second field of record *)

    (* Use loadWord not indirect because the optimiser reorders indirections. *)
    fun refDestructCode (v : codetree) : codetree =
       mkEval 
         (mkConst (ioOp POLY_SYS_load_word),
         [v, CodeZero],
         false) (* NOT early *)
       
    fun testBoxed (v: codetree) : codetree = (* not (isShort v) *)
      mkEval
        (mkConst (ioOp POLY_SYS_not_bool),
        [mkEval (mkConst (ioOp POLY_SYS_is_short), [v], true)],
        true);
        
    fun boxedDestructCode (box : codetree) : codetree =
      mkInd (0, box);

  (* The run-time representation of the constructor's value *)
  fun constrMake (test: representations) (name: string) : codetree =
    case test of
      UnboxedForm          => identityCode name
    | BoxedForm            => boxCode name
    | TaggedBox tag        => tagBoxCode tag name
    | TaggedTuple (tag, n) => tagTupleCode tag n name
    | RefForm              => refCode
    | ConstForm c          => mkConst c (* tagged value. *)
    | EnumForm tag         => mkConst (toMachineWord (toShort tag))
    | OnlyOne              => identityCode name
    | UnitForm             => CodeZero
    ;

  (* How to apply the constructor at run-time *)
  fun constrApply (test: representations) : codetree -> codetree =
    case test of
      UnboxedForm          => identityApplyCode
    | BoxedForm            => boxApplyCode
    | RefForm              => refApplyCode
    | TaggedBox tag        => tagBoxApplyCode tag
    | TaggedTuple (tag, n) => tagTupleApplyCode tag n
    | OnlyOne              => identityApplyCode
    | _                    => 
        (fn arg => raise InternalError "constant can't be applied")

  (* The run-time test whether a value matches a constructor. *)
  fun constrMatch (test: representations) (value:codetree) : codetree =
    case test of
      UnboxedForm          => testBoxed value
    | BoxedForm            => testBoxed value
    | RefForm              => CodeTrue
    | EnumForm tag         => testTag tag value 
    | TaggedBox tag        => testBoxedTag tag value
    | TaggedTuple (tag, n) => testBoxedTag tag value
    | ConstForm c          => testBoxedTag (Word.toIntX (* May be signed. *) (toShort (loadTag c))) value
    | OnlyOne              => CodeTrue
    | UnitForm             => CodeTrue
    ;

  (* The compile-time test whether a value matches a constructor. *)
(*  fun constrMatches (test: representations) (value: word) : bool =
    case test of
      UnboxedForm          => not (isShort value)
    | BoxedForm            => not (isShort value)
    | RefForm              => true
    | EnumForm tag         => wordEq (value, toMachineWord tag)
    | TaggedBox tag        => wordEq (loadTag value, toMachineWord tag)
    | TaggedTuple (tag, n) => wordEq (loadTag value, toMachineWord tag)
    | ConstForm c          => wordEq (loadTag value, loadTag c)
    | OnlyOne              => true
    | UnitForm             => true
    ;
*)

  (* The run-time code to destruct a construction. *)
  (* shouldn't the CodeZero's raise an exception instead? *)
  fun constrDestruct (test: representations) (value: codetree) : codetree =
    case test of
      UnboxedForm          => identityApplyCode value
    | BoxedForm            => boxedDestructCode value
    | RefForm              => refDestructCode value
    | TaggedBox tag        => taggedBoxDestructCode value
    | TaggedTuple (tag, n) => taggedTupleDestructCode n value
    | OnlyOne              => identityApplyCode value
    | EnumForm tag         => CodeZero (* To keep optimiser happy. *)
    | ConstForm c          => CodeZero (* (rather than raising an exception) *)
    | UnitForm             => CodeZero
    ;

  (* The compile-time function to destruct a construction. *)
(*  fun constrFetch (test: representations) (value: machineWord) : machineWord =
    case test of
      UnboxedForm         => value
    | BoxedForm           => loadBoxedValue value
    | RefForm             => loadBoxedValue value
    | TaggedBox tag       => loadTaggedBoxedValue value
    | TaggedTuple (tag,n) => loadTaggedTupleValue n value
    | OnlyOne             => value
    | EnumForm tag        => loadTag value (* shouldn't occur, but ... *)
    | ConstForm c         => value         (* shouldn't occur, but ... *)
    | UnitForm            => value         (* occurs for arrays! *)
    ;
*)

  in
     (* Constructors are now represented as run-time values.  A nullary constructor is
        a pair consisting of a test function and the constructor value.  A unary
        constructor is a triple: a test function, an injection function and a
        projection function.
        Previously constructors were handled entirely at compile with the appropriate
        functions inserted whenever a constructor was used.  This worked fine except
        in one case: when a datatype was used in a structure we couldn't use the optimal
        representation because it might match a datatype in a signature and we needed
        the same representation in both cases.  This arises if we have a datatype
        such as
            type t = int * int datatype s = X | Y of t
        We can use an optimised representation because we know that Y constructors are
        always boxed.  If we have
            type t = int datatype s = X | Y of t
        we can't do that and have to create tagged pairs for Y values.  Unfortunately
        we could pass either of these to a functor expecting a signature of the form
            sig type t datatype s = X | Y of t end
        If we handle constructors entirely at compile time we are forced to use the
        same representation for Y constructors in both cases. By passing the
        constructors as run-time values we can use different representations.
        This actually costs very little at run-time because functor are inlined
        so the constructor functions become inserted inline.  DCJM 18/5/01. *)

      fun createNullaryConstructor (test: representations, name: string): codetree =
      let
          val code =
              mkTuple[mkInlproc(constrMatch test arg1, 0, 1, name), (* Test function. *)
                    constrMake test name (* Value. *)]
      in
        (* Code generate the tuple now.  This saves us having multiple occurrences of
           the code but more importantly allows us to be able to print values of
           this datatype (printstruct uses evalue and that only works if we have
           a constant). *)
          genCode (code, [] (* No debugging output *)) ()
      end
    
      fun createUnaryConstructor(test: representations, name: string): codetree =
      let
          val code =
          mkTuple[mkInlproc(constrMatch test arg1, 0, 1, name), (* Test function. *)
                mkInlproc(constrApply test arg1, 0, 1, name), (* Injection function. *)
                mkInlproc(constrDestruct test arg1, 0, 1, name) (* Projection function. *)]
      in
        (* Code generate the tuple now. *)
          genCode (code, [] (* No debugging output *)) ()
      end

      (* Construct a nex execption identifier - it's really just a unit ref. *)
      fun mkExIden () : codetree = refApplyCode CodeZero;
  end;

(* 
     RefForm is used for "ref" (only). We use various representations
     of datatype / abstype constructors.
   
     Nullary constructors are represented as:
        UnitForm     (if it's the only constructor in the datatype)
        EnumForm     (if there's no more than one unary constructor)
         ConstForm    (otherwise)
    
     Unary constructors are represented as:
        OnlyOne      (if it's the only constructor in the datatype)
        UnboxedForm  (if it's the only unary constructor, applied to a tuple)
        TaggedTuple  (if it's not the only unary constructor, applied to a tuple, and we don't need backwards compatibility)
        BoxedForm    (if it's the only unary constructor)
        TaggedBox    (otherwise)
        
     Note that we can't use UnboxedForm/TaggedTuple if the datatype
     might appear in a signature, because the signature might be ambiguous
     about whether the argument is a tuple or not. In these cases, we always
     use BoxedForm/TaggedBox instead.
      
     Note that we use ConstForm, not EnumForm, for nullary constructors
     when the unary constructors are represented as TaggedTuple/TaggedBox
     because that allows the TaggedBox test to be:
       
         fn w => wordEq (loadWord (w,0), tag)
     
     rather than:
     
         fn w => not (isShort w) andalso wordEq (loadWord (w,0), tag)
     
     In my ignorance, I tried combining EnumForms with TaggedBoxes
     *without* changing the TaggedBox test and got a lot of
     core dumps as my reward (the machine didn't like fetching the
     tag from address 5!).
     
     SPF 20/10/94
  *)
  
  
  datatype constructorKind =
    Nullary             (* a nullary constructor *)
  | UnaryGeneric        (* a normal unary constructor *)
  | UnaryFunction       (* unary constructor applied to a function *)
  | UnaryTupled of int  (* a unary constructor applied to a tuple of size n  *)
  ;

  (* Make an object with the appropriate tag. Doing it here means we
     only do it once for this object. *)
  fun genConstForm (n :int) : representations =
  let
    (* In the new datatype format, I've moved the tag word, so
       we only need a one-word object. SPF 26/5/95 *)
    val vec : address = alloc (0w1, F_words, toMachineWord n);
    val U : unit      = lock vec;
  in
    ConstForm (toMachineWord vec)
  end;
    
  (* Choose tags in the sequence 0, ~1, 1, ~2, 2, ... because that
     maximises the chance of the tag being implemented by "immediate"
     data in the low-level generator. Note this means that "bool"
     gets a *different* representation than what its natural datatype
     declaration would give! Of course, we can't do this for the
     "Old" representations anyway. SPF 18/2/1998
  *)
  fun nextTag n = if n < 0 then ~ n else ~ (n + 1);
    
  (* We use this version if all the constructors are nullary (i.e. constants)
     except possibly one.  The (at most one) unary constructor is represented
     by the boxed value and the nullary constructors by untagged integers. *)
  (* Note that "UnaryTupled 0" (which would arise as a result of a declaration of the
     form  datatype t = A of () | ... ) can't be represented as "UnboxedForm"
     because "{}" is represented as a short (unboxed) integer. *)
  fun chooseOptimisedRepr1 n [] = []
    | chooseOptimisedRepr1 n (h :: t) = 
       case h of
         (Nullary,       name) =>
             createNullaryConstructor(EnumForm n, name) :: chooseOptimisedRepr1 (nextTag n) t
       | (UnaryGeneric,  name) =>
               createUnaryConstructor(BoxedForm, name) :: chooseOptimisedRepr1 n t
       | (UnaryFunction, name) =>
               createUnaryConstructor(UnboxedForm, name)  :: chooseOptimisedRepr1 n t
       | (UnaryTupled 0, name) =>
               createUnaryConstructor(BoxedForm, name)    :: chooseOptimisedRepr1 n t
       | (UnaryTupled _, name) =>
               createUnaryConstructor(UnboxedForm, name)  :: chooseOptimisedRepr1 n t

  (* We use this version there's more than 1 unary constructor. *)
  (* With this representation constructors of small tuples make tuples of
     size n+1 whose first word is the tag.  Nullary constructors are represented
     by single word objects containing the tag. *)
  fun chooseOptimisedRepr2 n [] = []
    | chooseOptimisedRepr2 n (h :: t) = 
  let
    val repr = 
      case h of
        (Nullary,       name) => createNullaryConstructor(genConstForm n, name)
      | (UnaryGeneric,  name) => createUnaryConstructor(TaggedBox n, name)
      | (UnaryFunction, name) => createUnaryConstructor(TaggedBox n, name)
      | (UnaryTupled i, name) =>
            createUnaryConstructor(
                  if i <= 4 (*!maxPacking*) then TaggedTuple (n, i) else TaggedBox n, name)
  in
    repr :: chooseOptimisedRepr2 (nextTag n) t
  end;

    fun getTupleKind t =
      case t of
        LabelledType {recList = [{typeof=t', ...}], frozen = true, ...} =>
            (* Singleton records are always represented simply by the value. *)
          getTupleKind t'
      
      | LabelledType {recList, frozen = true, ...} =>
          UnaryTupled (length recList)
      
      | FunctionType _ => 
          UnaryFunction
    
      | TypeConstruction {name, value, args, ...} =>
          let
            val cons = pling value
        in
              (* We may have a type equivalence or this may be a datatype. *)
            if tcIsAbbreviation cons
            then getTupleKind (makeEquivalent(cons, args))
            else if sameTypeId (tcIdentifier cons, tcIdentifier refType)
            then UnaryGeneric (* A tuple ref is NOT the same as the tuple. *)
            else (* Datatype.  For the moment we only consider datatypes with a
                    single constructor since we want to find the width of the
                    tuple.  At present we simply return UnaryGeneric for all
                    other cases but it might be helpful to return a special
                    result when we have a datatype which we know will always
                    be boxed. *)
                case tcConstructors cons of
                    [Value{typeOf, class=Constructor{nullary=false, ...}, ...}] =>
                        (* This may be a polymorphic datatype in which case
                           we have to invert the constructor to find the base type.
                           e.g. we may have an instance (int*int) t where t was
                           declared as datatype 'a t = X of 'a .*)
                        getTupleKind(constructorResult(typeOf, args))
                |    _ => UnaryGeneric
        end
    
      | _ =>
          UnaryGeneric

  (* This now creates the functions as well as choosing the representation. *)
    fun chooseConstrRepr cs =
    let
        fun checkArgKind (name, EmptyType) = (Nullary, name)
         |  checkArgKind (name, argType) = (getTupleKind argType, name)
        val kinds = map checkArgKind cs;

        fun chooseRepr [(Nullary, name)]       = [createNullaryConstructor(UnitForm, name)]
        | chooseRepr [(UnaryGeneric, name)]  = [createUnaryConstructor(OnlyOne, name)]
        | chooseRepr [(UnaryFunction, name)] = [createUnaryConstructor(OnlyOne, name)]
        | chooseRepr [(UnaryTupled _, name)] = [createUnaryConstructor(OnlyOne, name)]
        | chooseRepr l =
        let
          val unaryCount = List.foldl(fn((Nullary, _), n) => n | (_,n) => n+1) 0 l
        in
          (* tags now allocated from 0 (SPF 22/10/94) *)
          if unaryCount <= 1
          then chooseOptimisedRepr1 0 l (* can save the box *)
          else chooseOptimisedRepr2 0 l (* can use tagged tuples *)
        end;

       in
        chooseRepr kinds
    end;
 
   (* RefForm, NilForm and ConsForm are only used for built-in types *)


(*****************************************************************************)
(*             Standard values and exceptions.                               *)
(*****************************************************************************)

  (* Nil and :: are used in parsetree for lists constructed
     using [ ... ] and are also used for initialisation. *)
  local
    val listTypeVars  = tcTypeVars listType;
    val alpha         = hd listTypeVars;
    val alphaList     = mkTypeConstruction ("list", listType, listTypeVars, [DeclaredAt inBasis]);
    val consType      = mkFunctionType (mkProductType [alpha, alphaList], alphaList);
  in
    val nilConstructor  =
        mkGconstr ("nil", alphaList, createNullaryConstructor(NilForm, "nil"),  true, 2, [DeclaredAt inBasis]);
    val consConstructor =
        mkGconstr ("::",  consType,  createUnaryConstructor(ConsForm, "::"), false, 2, [DeclaredAt inBasis]);
  end;
  
  (* Create exception values - Small integer values are used for
     run-time system exceptions. *)
  val bindExceptionVal  = mkConst (toMachineWord EXC_Bind);
  val matchExceptionVal = mkConst (toMachineWord EXC_Match);

    (* Locations in exception packets.  In order to have a defined ordering of the fields,
       when we put the location in an exception packet we use this datatype rather than
       the "location" type. *)
     (* *)
    datatype RuntimeLocation =
        NoLocation
    |   SomeLocation of
            (* file: *) string * 
            (*startLine:*) int *  (*startPosition:*) int *
            (*endLine:*) int * (*endPosition:*) int

    fun codeLocation({file="", startLine=0, startPosition=0, ...}) =
        mkConst(toMachineWord NoLocation) (* No useful information *)
    |   codeLocation({file, startLine, startPosition, endLine, endPosition}) =
        mkConst(toMachineWord(file, startLine, startPosition, endLine, endPosition))

(*****************************************************************************)
  (* Look-up functions. *)

    (* Turn a typeId from Formal into Selected if it is from the given structure.
       In most cases Formals will be from the given structure but if we have a
       substructure we could have Formal that is relative to the parent or to the
       substructure and we have to distinguish those cases. *)
    fun idFromStructure str : typeId -> typeId =
    let
        val strSig = structSignat str
        fun isInStructure id =
        let
            fun check n =
                if n = sigMaxTypes strSig
                then false
                else sameTypeId(id, sigTypeIdMap strSig n) orelse check(n+1)
        in
            check(sigMinTypes strSig)
        end
    in
        fn (id as Free{ uid, allowUpdate, access=ref(Formal addr) }) =>
                if isInStructure id (* Have to search. *)
                then Free { uid=uid, allowUpdate=allowUpdate, access=ref(makeSelected(addr, str))}
                else id
        |   (id as Bound { offset = offset, eqType = eqType, access = Formal addr }) =>
                if offset >= sigMinTypes strSig andalso offset < sigMaxTypes strSig
                then Bound { offset = offset, eqType = eqType, access = makeSelected(addr, str) }
                else id
        |   id => id
    end
    
  
    (* Look up a structure. *)
    (* TODO: This copies the signature of the structure to ensure that type IDs within
       the structure become Selected entries. lookupStructure is also used when we are
       looking up a value, type or substructure so this will result in unnecessary
       copying when we have A.B.x . *)
    fun lookupStructure (kind, {lookupStruct:string -> structVals option},
               name, errorMessage) =
    let
        val {first = prefix, second = suffix} = splitString name;
        val strLookedUp =
            if prefix = ""
            then lookupStruct suffix
            else
            let  (* Look up the first part in the structure environment. *)
                val str =
                    lookupStructure
                        ("Structure", {lookupStruct=lookupStruct}, prefix, errorMessage);
            in 
                if isUndefinedStruct str
                then SOME undefinedStruct (* Error somewhere else? *)
                else 
                let
                    val parentSig = structSignat str
                    val Env{lookupStruct, ...} = makeEnv(sigTab parentSig)
                    (* If we have a DeclaredAt location for the structure use this as the StructureAt.*)
                    val baseLoc =
                    case List.find (fn DeclaredAt _ => true | _ => false) (structLocations str) of
                        SOME (DeclaredAt loc) => [StructureAt loc]
                    |   _ => []
                in
                    case lookupStruct suffix of
                        SOME structFound =>
                        let
                            (* We need to copy this structure so that any type IDs are converted
                               from Formal to Selected. *)
                            val tab = makeSignatureTable ();
                            (* Copy everything into the new signature. *)
                            val source = structSignat structFound
                            val Env sigEnv = makeEnv tab
                            val copyId = idFromStructure str
                            val lastAddr =
                                COPIER.fullCopySig 
                                    (source,
                                    {
                                      enterType   = fn (s,v) => #enterType sigEnv (s, v),
                                      enterStruct = fn (s,v) => #enterStruct sigEnv (s, v),
                                      enterVal    = fn (s,v) => #enterVal sigEnv (s, v)
                                    },
                                    copyId, "");
                            val copiedSig = 
                                makeSignature(sigName source, tab, sigMinTypes source, sigMaxTypes source,
                                              sigDeclaredAt source, copyId o sigTypeIdMap source)
                            (* Convert Formal access to Selected and leave the others (Global?). *)
                            val newAccess =
                                case structAccess structFound of
                                    Formal sel => makeSelected (sel, str)
                                |   access => access
                            
                            val newStruct =
                                Struct { name = structName structFound, signat = copiedSig,
                                         access = newAccess, locations = baseLoc @ structLocations structFound}
                        in
                            SOME newStruct
                        end
                    |   NONE => NONE
                end
            end
    in
        case strLookedUp of
           SOME s => s
        |  NONE =>
             (* Not declared? *)
                (errorMessage (kind ^ " (" ^ suffix ^  ") has not been declared" ^
                   (if prefix = "" then "" else " in structure " ^ prefix));
                undefinedStruct)
    end;

  fun mkEnv x = let val Env e = makeEnv x in e end

    (* Look up a structure but ignore the access. This is used in sharing constraints
       where we're only interested in the signature. *)
    fun lookupStructureAsSignature (lookupStruct, name, errorMessage) =
    let
        val {first = prefix, second = suffix} = splitString name;
        val found =
            if prefix = "" then lookupStruct suffix
            else
            let (* Look up the first part in the structure environment. *)
                val str =
                    lookupStructureAsSignature(lookupStruct, prefix, errorMessage);
            in
                if isUndefinedStruct str
                then SOME undefinedStruct (* Error detected earlier. *)
                else #lookupStruct (mkEnv(sigTab(structSignat str))) suffix
            end
    in
        case found of
            SOME v => v
        |   NONE => (* Not declared? *)
            (
                errorMessage ("Structure" ^ " (" ^ suffix ^ ") has not been declared" ^
                (if prefix = "" then "" else " in structure " ^ prefix));
                undefinedStruct
            )
    end (* lookupStructureAsSignature *) 

    (* Look up a value, possibly in a structure. If it is in
       a structure we may have to apply a selection. *)
    fun lookupValue (kind, {lookupVal,lookupStruct}, name, errorMessage) =
    let
        val {first = prefix, second = suffix} = splitString name;
        val found =
        if prefix = "" then lookupVal suffix
        else
        let (* Look up the first part in the structure environment. *)
            val baseStruct =
                lookupStructure
                    ("Structure", {lookupStruct=lookupStruct}, prefix, errorMessage);
        in
            (* If the structure is not there the value can't be. *)
            if isUndefinedStruct baseStruct
            then SOME undefinedValue
            else
            let
                val parentSig = structSignat baseStruct
                (* If we have a DeclaredAt location for the structure use this as the StructureAt.*)
                val baseLoc =
                    case List.find (fn DeclaredAt _ => true | _ => false) (structLocations baseStruct) of
                        SOME (DeclaredAt loc) => [StructureAt loc]
                    |   _ => []
            in
                case #lookupVal (mkEnv(sigTab parentSig)) suffix of
                    SOME (Value{ name, typeOf, access, class, locations }) =>
                    let
                        val copiedType =
                            copyType (typeOf, fn x => x,
                                fn tcon => copyTypeConstr (tcon, idFromStructure baseStruct, fn x => x, ""));
                    in
                        SOME(mkSelectedVar (
                                Value{ name=name, typeOf=copiedType, access=access, class=class, locations=locations },
                                baseStruct, baseLoc))
                    end
                |   NONE => NONE
            end
        end
    in
        case found of
            SOME v => v
        |   NONE => (* Not declared? *)
            (
                errorMessage (kind ^ " (" ^ suffix ^ ") has not been declared" ^
                    (if prefix = "" then "" else " in structure " ^ prefix));
                undefinedValue
            )
    end

    fun lookupTyp ({lookupType,lookupStruct}, name, errorMessage) =
    let
        val {first = prefix, second = suffix} = splitString name;
        val found =
        if prefix = "" then lookupType suffix
        else
        let (* Look up the first part in the structure environment. *)
            val str =
                lookupStructure
                    ("Structure", {lookupStruct=lookupStruct}, prefix, errorMessage);
        in
            (* If the structure is not there the value can't be. *)
            if isUndefinedStruct str
            then SOME undefType
            else
                case #lookupType (mkEnv(sigTab (structSignat str))) suffix of
                    SOME typeConstr => SOME(COPIER.fullCopyDatatype(typeConstr, idFromStructure str, ""))
                |   NONE => NONE
        end
    in
        case found of
            SOME v => v
        |   NONE => (* Not declared? *)
            (
                errorMessage ("Type constructor" ^ " (" ^ suffix ^ ") has not been declared" ^
                (if prefix = "" then "" else " in structure " ^ prefix));
                undefType
            )
    end 

 
      (* Printing. *)

    type nameSpace =
      { 
        lookupVal:    string -> values option,
        lookupType:   string -> typeConstrs option,
        lookupFix:    string -> fixStatus option,
        lookupStruct: string -> structVals option,
        lookupSig:    string -> signatures option,
        lookupFunct:  string -> functors option,

        enterVal:     string * values      -> unit,
        enterType:    string * typeConstrs -> unit,
        enterFix:     string * fixStatus   -> unit,
        enterStruct:  string * structVals  -> unit,
        enterSig:     string * signatures  -> unit,
        enterFunct:   string * functors    -> unit,

        allVal:       unit -> (string*values) list,
        allType:      unit -> (string*typeConstrs) list,
        allFix:       unit -> (string*fixStatus) list,
        allStruct:    unit -> (string*structVals) list,
        allSig:       unit -> (string*signatures) list,
        allFunct:     unit -> (string*functors) list
      };

  val nullEnvironment : nameSpace =
     {
        lookupVal = fn (s: string) => NONE,
        lookupType = fn (s: string) => NONE,
        lookupFix = fn (s: string) => NONE,
        lookupStruct = fn (s: string) => NONE,
        lookupSig = fn (s: string) => NONE,
        lookupFunct = fn (s: string) => NONE,
        enterVal = fn _ => (),
        enterType = fn _ => (),
        enterFix = fn _ => (),
        enterStruct = fn _ => (),
        enterSig = fn _ => (),
        enterFunct = fn _ => (),
        allVal = fn () => [],
        allType = fn () => [],
        allFix = fn () => [],
        allStruct = fn () => [],
        allSig = fn () => [],
        allFunct = fn () => []
        }

    (* Debug tag for the name space for the printer environment. *)
    val printSpaceTag: nameSpace tag = Universal.tag()
    
    fun getPrintSpace parameters =
        case List.find (Universal.tagIs printSpaceTag) parameters of
            SOME a => Universal.tagProject printSpaceTag a
        |   NONE => nullEnvironment
    

  (* Checks to see whether a labelled record is in the form of
     a product i.e. 1=, 2=   We only need this for prettyprinting. *)
  fun isProductType(LabelledType{recList, frozen=true, ...}) =
      let
        fun isRec [] n = true
         |  isRec ({name, typeof} :: l) n =
                 name = Int.toString n andalso isRec l (n+1)
    in
        isRec recList 1
    end
    | isProductType _ = false;

    (* Find an exception with the given identifier. *)
    fun findException (allValues: values list, allStructs: structVals list) (w: machineWord): values option =
    let
        fun searchList f [] = NONE
        |   searchList f (hd::tl) =
            case f hd of
                NONE => searchList f tl
            |    result => result

          open ADDRESS

        (* Test a value to see if it's the exception we want. *)
        fun testValue base valu =
        case valu of
            (* Top-level exception *)
            Value{class = Exception, access = Global code, ...} =>
                if wordEq(evalue code, w) then SOME valu else NONE

            (* Exception in a structure. *)
        |    Value{class = Exception, access=Formal addr, ...} =>
                if wordEq(loadWord(toAddress base, toShort addr), w)
                then SOME valu else NONE

        |    _ => NONE

        (* Search for the exception in a structure. *)
        fun searchSpace(u: univTable, base: machineWord) =
        let
            (* Get a list of all the entries in this space. *)
            val decList = iterList (univOver u)

          fun findItem (s: string, u: universal): values option =
               if tagIs valueVar u (* Values *)
             then testValue base (tagProject valueVar u)

             else if tagIs structVar u
             then (* Search this structure recursively. *)
             let
                 val str = tagProject structVar u
                val access = structAccess str
             in
                 case access of
                    Global code => (* Top-level structures. *)
                        searchSpace(sigTab (structSignat str), evalue code)

                |    Formal addr => (* Sub-structures. *)
                        searchSpace(sigTab (structSignat str),
                            loadWord(toAddress base, toShort addr))

                |    _ => NONE
             end

             else NONE (* Not a structure or a value. *)
      in
            searchList findItem decList
      end

        val globalBase = ADDRESS.toMachineWord 0 (* Unused. *)
    in
        (* First try the global values. *)
        case searchList (testValue globalBase) allValues of
            ex as SOME _ => ex (* found *)
        |   NONE => (* Not found; try in the structures. *)
                searchList (
                    fn s =>
                        case structAccess s of
                            Global code =>
                                searchSpace(sigTab (structSignat s), evalue code)
                        | _ => NONE (* Should just be globals. *))
                    allStructs
    end

    (* This module prints a structure by following the type structure. *)
    fun printStruct (value:machineWord, types, depth, nameSpace: nameSpace): pretty =
    let
        fun exceptionSearch (w: machineWord) : values option =
        let
            val values = #allVal nameSpace ()
            and strs   = #allStruct nameSpace ()
            (* Filter out the strings.  Order doesn't matter. *)
            fun getVal []            l = l
            |   getVal ((_, v) :: r) l = getVal r (v::l)
        in
            findException(getVal values [], getVal strs []) w
        end

        val { lookupFix, lookupExnById} = { lookupFix = #lookupFix nameSpace, lookupExnById = exceptionSearch }
 
        fun pVec (num, value : machineWord, [], separator, leftPrec, rightPrec, depth) doPrint = []
      
        |   pVec (num, value : machineWord, [t], separator, leftPrec, rightPrec, depth) doPrint =
            if num = 0 (* optimised unary tuples - no indirection! *)
            then [doPrint (value, t, depth, rightPrec)]
            else
            let
                val addr : address = toAddress value;
                val entryValue : machineWord = loadWord (addr, toShort num);
            in
                [doPrint (entryValue, t, depth, rightPrec)]
            end
      
        |   pVec (num, value, t::ts, separator, leftPrec, rightPrec, depth) doPrint =
            if depth <= 0
            then [PrettyString "..."]
            else
            let
                val addr : address = toAddress value;
                val entryValue : machineWord = loadWord (addr, toShort num);
            in
                doPrint (entryValue, t, depth, leftPrec) ::
                (
                (* Precede infix ops by a space. *)
                (if separator <> "," then [PrettyBreak (1, 0)] else []) @
                [PrettyString separator, PrettyBreak (1, 0)] @
                pVec (num + 1, value, ts, separator, leftPrec, rightPrec, depth-1) doPrint
                )
            end (* pVec *);

        fun prints (value : machineWord, types, depth, precedence, objList) : pretty =
        let (* Print out the contents of a tuple or labelled record. *)
      
            (* Print the constructor in infix notation if appropriate. *)
            fun printInfixed(constrName, argType, args, objList): pretty =
            let
                val maxPrec = 999;
                val thisPrecedence = getOpt(lookupFix constrName, Nonfix);

                (* Some of these need to be parenthesised.  We replace values at
                   level 1 by "..." rather than printing "(...)". *)
                fun mayParenthesise true (f: int -> pretty list): pretty list =
                    if depth <= 1
                    then [PrettyString "..."]
                    else PrettyString "(" :: (f (depth-1) @ [PrettyString ")"])
                |    mayParenthesise false f = f depth

            in
                PrettyBlock(3, false, [],
                case (thisPrecedence, argType) of
                    (Infix precNo,
                        LabelledType{recList=recList as [{name="1", ...}, {name="2", ...}], ...}) =>
                      mayParenthesise (precNo < precedence)
                           (fn depth =>
                          pVec (0, args, recList, constrName, precNo, precNo + 1, depth)
                              (fn (value, {name, typeof}, depth, precedence) =>
                                  prints (value, typeof, depth, precedence, objList)
                            ))

                  | (InfixR precNo,
                        LabelledType{recList=recList as [{name="1", ...}, {name="2", ...}], ...}) =>
                      mayParenthesise (precNo < precedence)
                           (fn depth =>
                          pVec (0, args, recList, constrName, precNo + 1, precNo, depth)
                              (fn (value, {name, typeof}, depth, precedence) =>
                                  prints (value, typeof, depth, precedence, objList)
                            ))

                  | _ =>
                      (* This constructor is not infix - print it in prefix notation.
                         If the constructor is already applied to something we must
                         parenthesise it. The argument precedence is set to infinity
                         - i.e. any constructors must be in parentheses. *)
                      mayParenthesise (precedence = maxPrec)
                           (fn depth =>
                               (
                              (* Must precede infix constructors by ``op''. *)
                              (case thisPrecedence of
                                Nonfix => []
                                | _ => [PrettyString "op", PrettyBreak (1, 0)]
                              ) @
                              [
                                  PrettyString constrName,
                                  PrettyBreak (1, 0),
                                  prints (args, argType, depth, maxPrec, objList)
                              ]
                             ))
                )
            end (* printInfixed *);
 
            (* Prints out a type construction by undoing the value constructors *)
            fun printConstruction typeArgs []: pretty =
                raise InternalError "none matches" (* Shouldn't happen *)
         
            | printConstruction typeArgs
                (Value{name, typeOf, access=Global code, class = Constructor{nullary, ...}, ...} :: constrs) =
                let   (* Try this constructor *)
                    open ADDRESS
                    val base = toAddress (evalue code)
                    val test = loadWord(base, 0w0) (* First word is the test. *)
                    val matches: bool = unsafeCast test value
                in
                    if not matches (* try the next *) then printConstruction typeArgs constrs
            
                    (* matches *)
                    else if nullary then (* Just a constant *) PrettyString name
            
                    (* Not just a constant. *) 
                    else if depth <= 0
                    then PrettyString "..."
                    else
                    let
                        (* The test succeeded so this is the constructor that made
                           this value  - get the value out. *)
                        val project = loadWord(base, 0w2) (* Third word is projection fn. *)
                        val v : machineWord = unsafeCast project value
           
                        (* Find the argument type which gives this result when the
                           constructor is applied. If we have, for example, a value of
                           type int list and we have discovered that this is a `::' node
                           we have to work back by comparing the type of `::' 
                           ('a * 'a list -> 'a list) to find the argument of the
                           constructor (int * int list) and hence how to print it.
                           (Actually `list' is treated specially). *)
                        val resType = constructorResult (typeOf, typeArgs);
                        (* If the value we get back from undoing the constructor
                           is the same as the constructed value, i.e. applying the
                           constructor simply returns the argument, we don't want to
                           add this value to the list.  If we do we won't be able to
                           print the constructed value. *)
                        val newList =
                            if ADDRESS.wordEq(v, value) then objList
                            else value :: objList
                    in
                        printInfixed(name, resType, v, newList)
                    end
                end

                  (* Normally a datatype constructor will be global. If, though, we
                   call PolyML.print within a functor on a datatype passed in as
                   a functor argument the code to test for constructor will be
                   in the actual argument.  We could generate code to handle that
                   case but it's probably not worth it. *)
            | printConstruction typeArgs _ = PrettyString "?"
                  (* printConstruction *)

        in (* prints *)
            (* If we have a circular structure we could end up looping until
               we reach the maximum depth.  Instead we check for any structure
               we've seen before and just print .... We really only need to include
               mutable structures in the list because only they can form loops
               but it's easier to include everything. *)
            if List.exists (fn v => ADDRESS.wordEq(value, v)) objList
            then PrettyString "..."
    
            else case types of
                TypeVar tyVar =>
                let
                  (* The type variable may be bound to something *)
                  val tyVal = tvValue tyVar
                in
                  if isEmpty tyVal then PrettyString "?"
                  else prints (value, tyVal, depth, precedence, objList)
                end
    
            | TypeConstruction{value=tval, args, ...} =>
                let
                    val constr = pling tval
                in
                  (* Type-specific printing is all handled by the libraries now. *)
                  (* Although unit is a type construction it is treated as
                     equivalent to the empty labelled record.  That means we
                     can't install a pretty printer for it using addPrettyPrinter. *)
                  if sameTypeId (tcIdentifier constr, tcIdentifier unitType)
                    then PrettyString "()"
        
                  (* Leave this one, at least for the moment, since we need to be able
                     to look the exception up in the environment. *)
                  else if sameTypeId (tcIdentifier constr, tcIdentifier exnType)
                    then let (* Exception. *)
                      val exn  : exn    = unsafeCast value;
                      val name : string = exnName exn;
                      (* In order to be able to print this exception we need to find the type of
                         any arguments.  Previously we used the name to search for a global
                         exception with that name but that doesn't help if the exception is in
                         a structure.  We now do a search of the complete name space.  *)
                    in
                      case lookupExnById (exnId exn) of
                          SOME exc =>
                        let
                            val typeof = valTypeOf exc
                        in
                            case getFnArgType typeof of
                                NONE => PrettyString name
                            |   SOME typeof => printInfixed(name, typeof, exnValue exn, value::objList)
                        end
                      |    NONE => PrettyString name (* Just put the name. *)
                    end
                
                  else (* All the others. *)
                    let    (* Use the given print function if it is in the table,
                              otherwise use the default. *)
                        (* If we have a print function installed for this type constructor
                           we have to pass it the functions to print the argument types
                           (if any). *)
                        fun makeArg argType =
                            let
                                fun printArg(v, depth) =
                                let
                                    (* If we have an identity constructor (datatype 'a t = T of 'a)
                                       then we mustn't add this item to the list because we'll see
                                       it again. *)
                                    val newList =
                                        if ADDRESS.wordEq(v, value) then objList
                                        else value :: objList
                                in
                                    prints(v, argType, depth, ~1, newList)
                                end
                            in
                                mkConst(toMachineWord printArg)
                            end
                        (* The easiest way to make a tuple is to use Codetree.mkTuple
                           which makes a tuple immediately if all the arguments
                           are constants. *)
                        val argTuple =
                            case args of
                                [] => CodeZero
                              | [t] => makeArg t
                              | args => mkTuple(map makeArg args)
                        fun getPrint typeId =
                            case idAccess typeId of
                                Global code => ! (unsafeCast(evalue(mkInd(1, code))))
                            |   _ => raise Subscript
                    in
                        if tcIsAbbreviation constr  (* May be an alias *)
                        then prints (value, makeEquivalent (constr, args), depth, precedence, objList)
                        else
                        (
                            (* Get the "print" function out of the type ID if it's there. *)
                            case idAccess(tcIdentifier constr) of
                                Global code =>
                                let
                                    val printer: int -> machineWord -> machineWord -> pretty =
                                        ! (unsafeCast(evalue(mkInd(1, code))))
                                in
                                    printer depth (evalue argTuple) value
                                end
                            |   _ => PrettyString "?"
                        )
                    end
                    (* isTypeConstruction *)
                end
      
            | FunctionType _ => PrettyString "fn"
      
            | LabelledType {recList, ...} =>
                if depth <= 0
                then PrettyString "..."
                else if isProductType types
                then (* If it is a record of the form {1=, 2=, ... } *)
                    PrettyBlock (1, false, [], (* Print them as (t1, t2, t3) .... *)
                        PrettyString "(" ::
                        (
                            pVec (0, value, recList, ",",  ~1, ~1, depth)
                                (fn (value, {name, typeof}, depth, precedence) =>
                                    prints (value, typeof, depth, precedence, objList)
                                ) @
                            [PrettyString ")"]
                        )
                    )
     
                else
                    PrettyBlock (1, false, [], (* Print them as ( a = X, b = Y ... ) *)
                        PrettyString "{" ::
                        (
                          pVec (0, value, recList, ",", ~1, ~1, depth)
                              (fn (value : machineWord, {name, typeof}, depth, precedence) =>
                                PrettyBlock (0, false, [],
                                    [
                                        PrettyString (name ^ " ="),
                                        PrettyBreak (1, 0),
                                        (* Don't add the current value to objList here.  We may have an
                                           optimised unary tuple in which case the value will be the
                                           same as the one we've just had. Since we're only really
                                           concerned about references making loops that should be fine. *)
                                        prints (value, typeof, depth - 1, ~1, objList)
                                    ]
                                )
                            ) @
                            [PrettyString "}"]
                         )
                    )
    
            | _ => PrettyString "<empty>"
        end  (* prints *);
    in
        prints (value, types, depth, ~1, [])
    end (* printStruct *);

    fun displayFixStatus Nonfix = PrettyString "nonfix"
    | displayFixStatus (Infix prec) =
        PrettyBlock(0, false, [],
            [ PrettyString "infix", PrettyBreak (1, 0), PrettyString (Int.toString prec) ])
    | displayFixStatus (InfixR prec) =
        PrettyBlock(0, false, [],
            [ PrettyString "infixr", PrettyBreak (1, 0), PrettyString (Int.toString prec) ])

    (* Returns the declaration location as the location for the context. *)
    fun getLocation locations =
        case List.find(fn DeclaredAt _ => true | _ => false) locations of
            SOME(DeclaredAt loc) => [ContextLocation loc]
        |   _ => []


    (* displays value as a block, with no external formatting *)
    fun displayValues (Value{name, typeOf, class, access, locations}, depth, nameSpace): pretty =
    let
        (* Create the "val X =" part. *)
        fun valPart equOrColon =
            PrettyBlock (0, false, [],
                [
                    PrettyString "val",
                    PrettyBreak (1, 0),
                    PrettyBlock(0, false, getLocation locations, [PrettyString name]),
                    PrettyBreak (1, 0),
                    PrettyString equOrColon
                ]
            )
        val typeEnv = (* Environment to check for type constructors. *)
            SOME { lookupType = #lookupType nameSpace, lookupStruct = #lookupStruct nameSpace}
    in
  
        if depth <= 0 
        then PrettyString "..."

        else
        case (class, access) of
        (SimpleValue, Global code) => (* Normal top-level values *)
             PrettyBlock (0, false, [],
                [
                    valPart "=",
                     PrettyBreak (1, 3),
                    (printStruct (evalue code, typeOf, depth, nameSpace)
                          handle InternalError _ => PrettyString "<undefined>"),
                          (* evalue will fail for "undefined". *)
                          (* I think "undefined" here was an old dummy-functor which has since been removed. *)
                    PrettyBreak (1, 3),
                    PrettyBlock (0, false, [],
                        [ PrettyString ":", PrettyBreak (1, 3), display (typeOf, depth, typeEnv) ])
                 ])

        |    (SimpleValue, _) => (* Values in structures and overloaded values (from PolyML). *)
                (* We can't get a value to print in this case. *)
             PrettyBlock (0, false, [],
                [ valPart ":", PrettyBreak (1, 3), display (typeOf, depth, typeEnv) ])

        |    (Exception, _) => (* exceptions *)
             PrettyBlock (0, false, [],
                PrettyBlock (0, false, [],
                    [
                        PrettyString "exception",
                        PrettyBreak (1, 0),
                        PrettyBlock(0, false, getLocation locations, [PrettyString name])
                    ]
                )
                ::
                (
                    case getFnArgType typeOf of
                       NONE => []
                    |  SOME excType =>
                        [ PrettyBreak (1, 1), PrettyString "of", PrettyBreak (1, 3), display (excType, depth, typeEnv) ]
                )
            )
      
        | _ => PrettyString "" (* ??? *)
    end;

  (* Print global values.  This is passed through the bootstrap and used in the debugger. *)
  fun printValues (Value{name, typeOf, class, access, ...}, depth, nameSpace) =
        case (class, access) of
            (SimpleValue, Global code) =>
                printStruct (evalue code, typeOf, depth, nameSpace)
        | _ => PrettyString "" (* Probably shouldn't occur. *)

    (* Prints "sig ... end" as a block, with no external formatting *)
    fun displaySig (str, depth : int, space : int,
                    nameSpace as { lookupType, lookupStruct, ...}) : pretty =
    let
        (* Construct an environment for the types. *)

        val Env { lookupType = strType, lookupStruct = strStr, ...} = makeEnv str
        val compositeEnv =
        {
            lookupType   = fn s => case strType s of NONE => lookupType s | sm => sm,
            lookupStruct = fn s => case strStr s of NONE => lookupStruct s | sm => sm,
            lookupVal    = #lookupVal nameSpace,
            lookupFix    = #lookupFix nameSpace,
            lookupSig    = #lookupSig nameSpace,
            lookupFunct  = #lookupFunct nameSpace,
            enterVal     = #enterVal nameSpace,
            enterType    = #enterType nameSpace,
            enterFix     = #enterFix nameSpace,
            enterStruct  = #enterStruct nameSpace,
            enterSig     = #enterSig nameSpace,
            enterFunct   = #enterFunct nameSpace,
            allVal       = #allVal nameSpace,
            allType      = #allType nameSpace,
            allFix       = #allFix nameSpace,
            allStruct    = #allStruct nameSpace,
            allSig       = #allSig nameSpace,
            allFunct     = #allFunct nameSpace
        }

        fun displaySpec (name, value) : pretty list =
        if (tagIs signatureVar value)
        then (* Not legal ML97 *)
            [ PrettyBreak(1,2), displaySignatures (tagProject signatureVar value, depth - 1, compositeEnv)]
           
        else if (tagIs structVar value)
        then
            [ PrettyBreak(1,2), displayStructures (tagProject structVar value, depth - 1, compositeEnv)]
               
        else if (tagIs typeConstrVar value)
        then 
            [ PrettyBreak(1,2), displayTypeConstrs (tagProject typeConstrVar value, depth) ]
      
        else if (tagIs valueVar value)
        then
        let
            (* Only print variables. Constructors are printed with their type. *)
            val value = tagProject valueVar value;
        in
            case value of
                Value{class = Constructor _, ...} => []
            |    _ =>
              [ PrettyBreak(1,2),
              (* We lookup the infix status and any exception in the global environment
                 only.  Infix status isn't a property of a structure and it's too
                 much trouble to look up exceptions in the structure. *)
                displayValues (value, depth, compositeEnv)
              ]
        end
      
        else if (tagIs fixVar value)
        then  (* Not legal ML97 *)
            [ PrettyBreak(1,2), displayFixStatus (tagProject fixVar value) ]
 
        else []
       (* end displaySpec *)
    in
        PrettyBlock (0, true, [],
            PrettyString "sig" ::
            (
                (
                    if depth <= 1 (* If the depth is 1 each of the calls to displaySpec will
                                     print "..." so we replace them all by a single "..." here. *)
                    then [PrettyBreak (1, 0), PrettyString "..."]
                    else
                    let
                        val declist = ref nil : (string * universal) list ref
                        fun addToList nv = declist := nv :: !declist
                        (* For the moment order them by name.  We may change this to
                           order primarily by kind and secondarily by name. *)
                        fun order (s1: string, _) (s2: string, _) = s1 <= s2
                    in
                        (* Put all the entries into a list. *)
                        for (univOver str) addToList;
                        (* Sort the list and print it. *)
                        List.foldl
                            (fn (a, l) => displaySpec a @ l)
                            [] (quickSort order (!declist))
                    end
                )
                @ [PrettyBreak (1, 0), PrettyString "end"]
            )
        )
        end (* displaySig *)

  (* Print: signature S = sig .... end *)
  and displaySignatures (str, depth : int, nameSpace) : pretty =
    if depth <= 0 then PrettyString "..."
    else
        PrettyBlock(0, false, [],
            [
                PrettyBlock(0, false, [],
                    [
                        PrettyString "signature",
                        PrettyBreak(1, 0),
                        PrettyBlock(0, false,
                            [ContextLocation(sigDeclaredAt str)],
                            [PrettyString(sigName str)]
                        ),
                        PrettyBreak(1, 0),
                        PrettyString "="
                    ]
                ),
            PrettyBreak (1, 2),
            displaySig (sigTab str, depth, 1, nameSpace)
            ])

  (* print structure in a block (no external spacing) *)
    and displayStructures (str, depth, nameSpace): pretty =
    if depth <= 0 then PrettyString "..."
    else if isUndefinedStruct str then PrettyString "<bad>"
    else
    let
        val structureName = structName str;
        val signatureName = sigName (structSignat str);
    in
        PrettyBlock (0, false, [],
        [
            PrettyBlock(0, false, [],
                [
                    PrettyString "structure",
                    PrettyBreak(1, 0),
                    PrettyBlock(0, false,
                        getLocation(structLocations str),
                        [PrettyString structureName]
                    ),
                    PrettyBreak(1, 0),
                    PrettyString ":"
                ]
            ),
            PrettyBreak(1, 2),
            if signatureName <> ""
            then PrettyString signatureName
            else displaySig (sigTab(structSignat str), depth - 1, 1, nameSpace)
        ])
    end

    fun displayFunctors (funct, depth, nameSpace) =
    if depth <= 0 then PrettyString "..."
    else
        PrettyBlock (0, false, [],
            PrettyBlock(0, false, [],
                [
                    PrettyString "functor",
                    PrettyBreak(1, 0),
                    PrettyBlock(0, false,
                        [ContextLocation(functorDeclaredAt funct)],
                        [PrettyString(functorName funct)]
                    ),
                    PrettyBreak(1, 0),
                    PrettyString "("
                ]) ::
            PrettyBreak(0, 0) ::
            (
            (if structName (functorArg funct) <> ""
            then [ PrettyString((structName (functorArg funct)) ^ " :"), PrettyBreak(1, 3) ]
            else []) @
            [
                displaySig (sigTab(structSignat (functorArg funct)), depth - 1, 0, nameSpace),
                PrettyString ") :",
                PrettyBreak(1, 3),
                displaySig (sigTab(functorResult funct), depth - 1, 1, nameSpace)
            ]
            )
        )
   
  (* Code-generation. *)

     (* Code-generate the values. *) 
 fun codeStruct (str, level) =
     (* Global structures have no code value. Instead the
        values are held in the values of the signature. *)
   if isUndefinedStruct str
   then CodeNil
   else codeAccess (structAccess str, level)

 and codeAccess (Global code, _) = code
      
 |  codeAccess (Local{addr=ref locAddr, level=ref locLevel}, level) =
     let
       val levelDiff = level - locLevel;
     in
       if locAddr = 0
       then mkRecLoad (levelDiff - 1)   (* Recursive *)
       else mkLoad (locAddr, levelDiff) (* Argument or local *)
     end
     
 | codeAccess (Selected{addr, base}, level) = (* Select from a structure. *)
       mkInd (addr, codeStruct (base, level))
     
 | codeAccess (acc, level) = raise InternalError "No access"
     (* codeAccess *);

     (* Raises an exception. *)
 fun raiseException exName exIden parm =
   mkRaise (mkTuple [exIden,mkStr exName,parm]);

 (* Raise match and bind exceptions. *)
 val raiseMatch     = raiseException "Match" matchExceptionVal CodeZero;
 val raiseBind      = raiseException "Bind" bindExceptionVal CodeZero;

(*****************************************************************************)
(*                  Equality function compiler                               *)
(*****************************************************************************)

(*
    This generates code to handle equality (i.e. = and <>) by generating functions
    based on the type of the arguments to be compared.  For parameterised datatypes
    (e.g. 'a list) that means generating functions which are parameterised on the
    equality function for 'a.  In a few cases (e.g where the type is a functor
    argument) we can't generate the function and we fall back to the general purpose
    structure equality function.  The optimiser (codetree) does a good job of optimising
    the code and turning the functions into loops. 
*)

 fun genEqualityFunction(instance: types, level: int): codetree =
 let
    (* To reduce the size of the code we pass down the kind of
       result we want. *)
     datatype reskind =
        ApplyFun of ({level:int, myAddr: int}->codetree)*({level:int, myAddr: int}->codetree)
    |   MakeFun
    (* If we get a function back it may take a pair as an argument or
       it may take two arguments. *)
    datatype resfun =
        PairArg of {level:int, myAddr: int} -> codetree
    |    TwoArgs of {level:int, myAddr: int} -> codetree

    val baseLevel = level+1
    local
        val addrs = ref 0
    in
        fun mkaddrs () = (addrs := !addrs + 1; !addrs)
    end

    (* The list of functions.  These are potentially mutually recursive. *)
    val generatedFuns: codetree list ref = ref []
    (* The list of addresses of functions for datatypes.  This allows us
       to make recursive calls for recursive datatypes and also to avoid
       generating the same function twice.
       e.g. datatype t = A of s | B and s = C of t | D. *)
    val datatypeList: (int * typeId) list ref = ref []

    (* If we have a function we either return it or we apply it.  The
       function will always take a single argument as a tuple. *)
    fun returnFun (f: resfun, MakeFun) = f
     |  returnFun (TwoArgs f, ApplyFun(a1, a2)) =
            PairArg(fn lA => mkEval(f lA, [a1 lA, a2 lA], true))
     |  returnFun (PairArg f, ApplyFun(a1, a2)) =
            PairArg(fn lA => mkEval(f lA, [mkTuple[a1 lA, a2 lA]], true))

    (* If we have a piece of code we may need to wrap it in a function.
       This is generally used to create the code for handling tuples.
       When creating a function this previously attempted to add the function to
       the generatedFuns list but that turned out to have a bug.  If we have
       a tuple inside a parameterised datatype e.g. datatype 'a t = X of 'a * ... then
       we must make sure that we create the tuple equality function inside the equality
       function for t otherwise it won't be able to find the equality function for 'a.  *)
    fun returnCode(mkCode, ApplyFun(a1, a2)) =
            PairArg(fn l => mkCode(a1, a2, l))
      | returnCode(mkCode, MakeFun) =
          let
            fun wrappedCode {level, myAddr} =
            let
                val addr = mkaddrs() (* Should never be used since this isn't directly recursive. *)
                val newLevel = level+1
    
                val code = mkCode(fn {level=l, ...} => mkLoad(~1, l-newLevel),
                                  fn {level=l, ...} => mkLoad(~2, l-newLevel),
                                  {level=newLevel, myAddr=addr});
            in
                mkProc(code, newLevel, 2, "eq{...}(2)")
            end
        in
            TwoArgs wrappedCode
        end

     val default = PairArg(fn _ => mkConst (toMachineWord structureEq))

     fun makeEq(ty: types, resKind: reskind,
               findTyVars: typeVarForm -> resfun): resfun =
    let

        fun equalityForDatatype(constr, vConstrs) : {level:int, myAddr: int} -> codetree =
        let
            val id = tcIdentifier constr
            val typeName = tcName constr
            val addr = mkaddrs()
            (* We need to record this address in the list. *)
            val _ = datatypeList := (addr, id) :: !datatypeList;
            (* If this is a polymorphic type constructor (e.g. 'a list)
               we have to pass the equality functions for the argument
               type (e.g. int if we have int list) as arguments to the
               equality function. *)
            val constructorTypeVars = tcTypeVars constr
            val nTypeVars = List.length constructorTypeVars

            val outerFunLevel = baseLevel+1
            val newLevel =
                if nTypeVars = 0 then outerFunLevel else outerFunLevel+1

            fun newTvFun tv =
            let
                fun findTv [] n = findTyVars tv (* Not in this list. *)
                 |  findTv (TypeVar tv' :: tvs) n =
                         if sameTv(tv, tv')
                        then TwoArgs(fn {level, ...} => mkLoad(n, level-outerFunLevel))
                        else findTv tvs (n+1)
                 |  findTv _ _ =
                         raise InternalError "findTv: not a type variable"
            in
                findTv constructorTypeVars (~nTypeVars)
            end

            (* Filter out the EnumForm constructors.  They arise
               in situations such as datatype t = A of int*int | B | C
               i.e. where we have only one non-nullary constructor
               and it is a tuple.  In this case we can deal with all
               the nullary constructors simply by testing whether
               the two arguments are the same.  We don't have to
               discriminate the individual cases. *)
            fun isEnum(Value{class=Constructor{nullary=true, ...}, access=Global code, ...}) =
                let
                    open ADDRESS
                in
                    (* If the value is a short integer then we can check
                       for equality using pointer equality. *)
                    isShort(loadWord(toAddress(evalue code), 0w1))
                end
              | isEnum _ = false

            fun processConstrs [] =
                    (* The last of the alternatives is false *) CodeZero

             |    processConstrs ((vConstr as Value{class, access, typeOf, name=tempConstrName, ...}) ::rest) =
                 if isEnum vConstr then processConstrs rest
                else
                 let
                    val base = codeAccess(access, newLevel)
                    fun matches arg =
                        mkEval(mkInd(0, base) (* Test function. *), [arg], true)
                in
                    case class of
                        Constructor{nullary=true, ...} =>
                            mkIf(matches arg1, matches arg2, processConstrs rest)
                    |    _ => (* We have to unwrap the value. *)
                        let
                            (* Get the constructor argument given
                               the result type.  We might actually be
                               able to take the argument type off directly
                               but there's some uncertainty about whether
                               we use the same type variables for the
                               constructors as for the datatype. (This only
                               applies for polytypes). *)
                            val resType =
                                constructorResult(typeOf, constructorTypeVars)

                            (* Code to extract the value. *)
                            fun destruct argNo {level=l, ...} =
                                mkEval(mkInd(2, codeAccess(access, l)) (* projection function. *),
                                    [mkLoad(argNo, l-newLevel)], true)

                            (* Test whether the values match. *)
                            val eqValue =
                                applyEq(resType, destruct ~1, destruct ~2,
                                        {level=newLevel, myAddr=addr}, newTvFun)    
                        in
                            (* We have equality if both values match
                               this constructor and the values within
                               the constructor match. *)
                            mkIf(matches arg1,
                                mkCand(matches arg2, eqValue),
                                processConstrs rest)
                        end
                end

            (* We previously only tested for bit-wise (pointer) equality if we had
               at least one "enum" constructor in which case the test would eliminate
               all the enum constructors.  I've now extended this to all cases where
               there is more than one constructor.  The idea is to speed up equality
               between identical data structures. *)
            val eqCode =
                case vConstrs of
                   [vcons] => (* Single constructor. *)
                       if isEnum vcons
                       then CodeTrue (* Return true here: processConstrs would return false. *)
                       else processConstrs vConstrs
                 |  _ => (* More than one constructor: should never be zero. *)
                        mkCor(mkTestptreq(arg1, arg2), processConstrs vConstrs)
            val eqFun =
                mkProc(eqCode, newLevel, 2, "eq-" ^ typeName ^ "(2)")
            (* If this is a monotype we can return it directly otherwise we
               need to wrap it up in a function to take the equality functions
               for the argument types. *)
            val resFun =
                if nTypeVars = 0 then eqFun
                else mkProc(eqFun, outerFunLevel, nTypeVars, "eq-" ^ typeName ^ "(1)")
        in
            generatedFuns := !generatedFuns @ [mkDec(addr, resFun)];
            fn {level=l, myAddr} => mkLoad(addr, l-baseLevel)
        end
        
        fun equalityForConstruction(constr, args, vConstrs): resfun =
        (* Generate an equality function for a datatype construction. *)
        let
            (* See if we are currently making this function or
               have already made it.  If this is recursive we may
               be able to optimise it. *)
            val id = tcIdentifier constr
            val constrName = tcName constr

            fun searchList [] = NONE
            |    searchList ((addr, t) :: rest) =
                    if sameTypeId(t, id) then SOME addr else searchList rest
            val alreadyAddr = searchList (!datatypeList)
            (* Get the equality functions for the argument types.
               These want to be functions taking two arguments.
               This applies only to polytypes. *)
            fun getArg (lA as {level, myAddr}) ty : codetree =
            let
                val eqFun = makeEq(ty, MakeFun, findTyVars)
            in
                case eqFun of
                    PairArg f =>
                            (* Have to make a function which takes two arguments. *)
                            mkInlproc(
                                mkEval(f{level=level+1, myAddr=myAddr}, [mkTuple[arg1, arg2]], true),
                                level+1, 2, "eq-"^constrName^"(...)")
                |    TwoArgs f => f lA
            end
            (* If we are compiling a recursive polytype (e.g. list) and
               we find a recursive call we can generate better code by
               calling the inner function directly, provided the recursive
               call involves the polymorphic type.  This isn't true if we
               have datatype 'a t = X of int t | Y of 'a where the
               recursive call is not polymorphic. *)
             fun sameTypeVars(TypeVar tv, TypeVar tv') = sameTv(tv, tv')
             |   sameTypeVars _ = false

             fun recursiveEq (addr, []) {level:int, myAddr: int} =
                if addr = myAddr
                then mkRecLoad(level-baseLevel-1)
                else mkLoad(addr, level-baseLevel)

             |    recursiveEq (addr, _) (lA as {level:int, myAddr: int}) =
                if addr <> myAddr
                then mkEval(mkLoad(addr, level-baseLevel),
                        map (getArg lA) args, true)
                else if ListPair.foldl
                            (fn(a, b, t) => t andalso sameTypeVars(a,b))
                            true (args, tcTypeVars constr)
                then mkRecLoad(level-baseLevel-2)
                else mkEval(mkRecLoad(level-baseLevel-1),
                        map (getArg lA) args, true)
        in
            (* Apply the function we obtained to any type arguments. *)
            case searchList (!datatypeList) of
                SOME addr => TwoArgs(recursiveEq(addr, args))
            |    NONE =>
                    if isFreeId(tcIdentifier constr) andalso isGlobal(idAccess(tcIdentifier constr))
                    then TwoArgs(fn l =>
                            mkEval(mkInd(0, vaGlobal(idAccess(tcIdentifier constr))), map (getArg l) args, true))
                    else
                    let
                        val eq = equalityForDatatype(constr, vConstrs)
                    in
                        case args of
                            [] => TwoArgs eq
                        |    _ => 
                            TwoArgs(fn l =>
                                mkEval(eq l, map (getArg l) args, true))
                    end
        end
    in
        case ty of
            TypeVar tyVar =>
                let
                  (* The type variable may be bound to something. *)
                  val tyVal = tvValue tyVar
                in
                  (* If we have an unbound type variable it may either
                     be a type constructor argument or it may be a free
                     equality type variable. *)
                  if isEmpty tyVal
                  then returnFun(findTyVars tyVar, resKind)
                  else makeEq(tyVal, resKind, findTyVars)
                end

        |    TypeConstruction{value, args, ...} =>
            let
                val constr = pling value
                val id = tcIdentifier constr
            in
                if tcIsAbbreviation constr  (* May be an alias *)
                then makeEq (makeEquivalent (constr, args), resKind, findTyVars)
                (* TODO: For the moment we can only get the equality function for globals. *)
                else if isFreeId(tcIdentifier constr) andalso isGlobal(idAccess(tcIdentifier constr))
                then returnFun(equalityForConstruction(constr, args, tcConstructors constr), resKind)
                else returnFun(default, resKind)
            end

        |   LabelledType {recList=[{typeof=singleton, ...}], ...} =>
                (* Unary tuples are optimised - no indirection. *)
                    makeEq(singleton, resKind, findTyVars)

        |   LabelledType {recList, ...} =>
                (* Combine the entries.
                    fun eq(a,b) = #1 a = #1 b andalso #2 a = #2 b ... *)
                let
                    fun eqTuple(arg1, arg2, lA) =
                    let
                        fun combineEntries ([], n) = CodeTrue
                        |    combineEntries ({typeof, name=_}::t, n) =
                                mkCand
                                (applyEq(typeof, fn l => mkInd(n, arg1 l),
                                         fn l => mkInd(n, arg2 l),
                                         lA, findTyVars),
                                 combineEntries (t, n+1))
                    in
                        combineEntries(recList, 0)
                    end
                in
                    returnCode(eqTuple, resKind)
                end

        |    _ => (* It is actually possible to get an equality function
                    on functions in ML97 as a result of sharing constraints
                    in a functor. The signature would not be matchable by
                    a real structure so the functor could never be applied.
                    Nevertheless the functor must compile so we just put
                    in structure equality here. *)
                returnFun(default, resKind)
    end

    (* Make an equality function and apply it to the arguments. *)
    and applyEq(ty, arg1, arg2, lA, findTyVars): codetree =
        case makeEq(ty, ApplyFun(arg1, arg2), findTyVars) of
            PairArg c => c lA
        |    TwoArgs _ => raise InternalError "applyEq: wrong result"    

     (* The instance type is a function so we have to get the first
       argument. *)
    val argType = firstArg instance
    (* Get the final equality function and generate any which are needed
       as a side effect. *)
     val resultCode = makeEq(argType, MakeFun, fn _ => default)
 in
     (* The final result function must take a single argument.  If we have
       generated a function the result must be one which takes two arguments.
       If we have not generated it it must have come from somewhere else so
       it must take a pair. *)
     case resultCode of
        PairArg c => c {level=baseLevel,myAddr=0}
    |    TwoArgs c =>
            let
                (* Must call c BEFORE we dereference generatedFuns because
                   the call may generate new functions. *)
                val code = c {level=baseLevel,myAddr=0}
                val funs = ! generatedFuns
                val wrappedFuns =
                    case funs of
                        [singleton] => singleton
                    |    funs => mkMutualDecs funs
            in
                (* We need to wrap this up in a new inline function for two reasons.
                   One is that it needs to take a single pair argument, the other is
                   that we have allocated the addresses from 1 and we may get conflicts
                   with addresses in the surrounding scope. *)
                mkInlproc(CODETREE.mkEnv[wrappedFuns,
                          mkEval(code, [mkInd(0, arg1), mkInd(1, arg1)], true)],
                          baseLevel, 1, "equality")
            end
 end


(*****************************************************************************)
(*                  datatype access functions                                *)
(*****************************************************************************)

 (* Get the appropriate instance of an overloaded function.  If the
    overloading has not resolved to a single type it finds the preferred
    type if possible (i.e. int for most overloadings, but possibly real,
    word, string or char for conversion functions.) *)
 fun getOverloadInstance(name, instance, isConv, lex, lineno): codetree*string =
      let
     val constr = typeConstrFromOverload(instance, isConv)
     (* If there is no matching type produce a message. That should only
        happen if we are running in ML90 mode and the overloading has not
        reduced to a single type.  *)
     fun notFound () =
         if name = "=" (* Special case *)
        then mkConst (toMachineWord structureEq)
        else
         let
            val ops = getOverloads name
            (* Construct a list of the current overloadings. *)
            fun makeOverloadList [] = ""
             |  makeOverloadList [(last, _)] = tcName last
             |  makeOverloadList ((h, _)::l) =
                     tcName h ^ "/" ^ makeOverloadList l
            val overloads = makeOverloadList ops
        in
           overloadError (instance, name, overloads, lex, lineno);
           CodeNil
        end

     in
        (getOverload(name, constr, notFound), tcName constr)
     end

    (* This is only used in addPrettyPrint and install_pp.  There's no point in
       producing a lot of detailed information. *)
    fun checkPPType (instanceType, matchType, fnName, lex, location, moreInfo) =
        case unifyTypes (instanceType, matchType) of
            NONE => ()
        |   SOME(typeA, typeB, reason) =>
            let
                open DEBUG
                val parameters = LEX.debugParams lex
                val errorDepth = getParameter errorDepthTag parameters
            in
                reportError lex
                {
                    location = location,
                    hard = true,
                    message =
                        PrettyBlock(0, true, [],
                            [
                                PrettyString ("Argument for " ^ fnName),
                                PrettyBreak (1, 3),
                                PrettyBlock(0, false, [],
                                    [
                                        PrettyString "Required type:",
                                        PrettyBreak (1, 0),
                                        display (matchType, errorDepth, NONE)
                                    ]),
                                PrettyBreak (1, 3),
                                PrettyBlock(0, false, [],
                                    [
                                        PrettyString "Argument type:",
                                        PrettyBreak (1, 0),
                                        display (instanceType, errorDepth, NONE)
                                    ]),
                                PrettyBreak (1, 3),
                                PrettyBlock(0, false, [],
                                    [
                                        PrettyString "Reason:",
                                        PrettyBreak (1, 0),
                                        PrettyString reason
                                    ])                        
                            ]),
                    context = SOME (moreInfo ())
               }
            end;

 (* Code-generate an identifier matched to a value.  N.B. If the value is a
    constructor it returns the pair or triple representing the functions on the
    constructor. *)
 fun codeVal (Value{access = Global code, ...}, _, _, _, _) = code

  |  codeVal (Value{access = Local{addr=ref locAddr, level=ref locLevel}, ...}, level, _, _, _) =
     let
       (* Load the variable. *)
       val levelDiff = level - locLevel;
     in
       if locAddr = 0
       then mkRecLoad (levelDiff - 1)   (* Recursive *)
       else mkLoad (locAddr, levelDiff) (* Argument or local *)
     end

  |  codeVal (Value{access = Selected{addr, base}, ...}, level, _, _, _) =
       (* Select from a structure. *)
       mkInd (addr, codeStruct (base, level))

  |  codeVal (Value{access = Formal _, ...}, level, _, _, _) =
          raise InternalError "codeVal - Formal"

  |  codeVal (Value{access = Overloaded Print, ...}, level, instance, lex, _) =
        let 
         (* "instance" should be 'a -> 'a. We need to get the 'a. *)
         val argType = case getFnArgType instance of NONE => badType | SOME t => t;
         open DEBUG
         (* The parameter is the reference used to control the print depth
            when the value is actually printed. *)
         val printDepthFun = getParameter printDepthFunTag (LEX.debugParams lex)
         and prettyOut = getPrintOutput (LEX.debugParams lex)
         and printSpace = getPrintSpace (LEX.debugParams lex)

         fun printProc value =
         (
            prettyOut(printStruct (value, argType, printDepthFun(), printSpace));
            value
         );
           (* Coerce the procedure so that it can be put into the code. *)
       in
            mkConst (toMachineWord printProc)
       end 

  |  codeVal (Value{access = Overloaded PrintSpace, ...}, level, instance, _, _) =
        let 
            (* "instance" should be 'a * namespace * printer * int -> 'a. We need to get the 'a. *)
            val argType =
                case getFnArgType instance of
                    SOME (LabelledType { recList = {typeof, ...} ::_, ...}) => typeof
                |   _ => badType;

            fun printProc (value, namespace, printer, depth): unit =
                printer(printStruct (value, argType, depth, namespace))
         in
            mkConst (toMachineWord printProc)
        end 

  |  codeVal (Value{access = Overloaded MakeString, ...}, level, instance, lex, _) =
       let 
         (* "instance" should be 'a -> string. We need to get the 'a. *)
         val argType = case getFnArgType instance of NONE => badType | SOME t => t;

         val printSpace = getPrintSpace (LEX.debugParams lex)

         fun makeString value =
            uglyPrint (printStruct (value, argType, 10000, printSpace));
       in
         mkConst (toMachineWord makeString)
       end

  |  codeVal (Value{access = Overloaded MakeStringSpace, ...}, level, instance, lex, _) =
        let 
            (* "instance" should be 'a * namespace -> string. We need to get the 'a. *)
            val argType =
                case getFnArgType instance of
                    SOME (LabelledType { recList = {typeof, ...} ::_, ...}) => typeof
                |   _ => badType;

            fun printProc (value, namespace) =
                uglyPrint (printStruct (value, argType, 10000, namespace))
        in
            mkConst (toMachineWord printProc)
        end

  |  codeVal (Value{access = Overloaded AddPretty, ...}, level, instance, lex, loc) =
        let 
         (* "instance" should be (int-> 'a -> 'b -> pretty) -> unit.
             We need to get the 'a and 'b.  This function installs a
             pretty printer against the type which matches 'b.
             The type 'a is related to type of 'b as follows:
             If 'b is a monotype t then 'a is ignored.
             If 'b is a unary type constructor 'c t then 'a must have
             type 'c * int -> pretty.
             If 'b is a binary or higher type constructor e.g. ('c, 'd, 'e) t
             then 'a must be a tuple of functions of the form
             ('c * int -> pretty, 'd * int -> pretty, 'e * int -> pretty).
             When the installed function is called it will be passed the
             appropriate argument functions which it can call to print the
             argument types.  *)
            val argPrints  = mkTypeVar (generalisable, false, false, false);
            val installType   = mkTypeVar (generalisable, false, false, false);
            val pretty = mkTypeVar (generalisable, false, false, false);
            (* Build a pattern type and unify it to get the type variables.
               Unification should not fail here because type checking has already
               been done by the time we get here. *)
            val addPPType =
                mkFunctionType(
                    mkFunctionType (TYPESTRUCT.intType,
                        mkFunctionType(argPrints, mkFunctionType(installType, pretty))),
                    TYPESTRUCT.unitType);
            val () = checkPPType(instance, addPPType, "addPrettyPrinter", lex, loc, fn () => PrettyString "");

            (* Find the last type constructor in the chain. We have to install
                 this against the last in the chain because type constructors in
                 different modules may be at different points in the chain. *)
              (* This does mean that it's not possible to install a
                 pretty printer for a type constructor rather than a datatype. *)
            fun followTypes (TypeConstruction{value, args, ...}) =
            let
                val constr = pling value
            in
                if not (tcIsAbbreviation constr)
                then
                let
                    val typeId = tcIdentifier constr
                in
                    (* Check that it's a top-level datatype (NOT in a functor) *)
                    if isFreeId typeId
                    then SOME(typeId, constr, List.length args)
                    else
                    (
                        errorMessage(lex, loc,
                            "A pretty printer cannot be installed for a type within a functor");
                        NONE
                    )
                end
                else followTypes (makeEquivalent (constr, args))
            end
            |   followTypes (TypeVar tv) =
                (
                    case tvValue tv of
                        EmptyType => NONE (* Unbound type variable *)
                    |   t => followTypes t
                )
            |   followTypes _ = NONE;

            val constrId = followTypes installType
            
            val () =
                case constrId of
                    NONE => ()
                |   SOME (_, constr, arity) =>
                    let
                        (* Check that the function tuple matches the arguments of the type
                           we're installing for. *)
                        (* Each entry should be a function of type 'a * int -> pretty *)
                        fun mkFn arg = mkFunctionType(mkProductType[arg, TYPESTRUCT.intType], pretty)
                        (* Create non-unifiable type vars to ensure this is properly polymorphic. *)
                        val typeVars = List.tabulate(arity, fn _ => mkTypeVar (0, false, true, false))
                        val tupleType =
                            case typeVars of
                                [] => (* No arg so we can have anything here. *)
                                    mkTypeVar (generalisable, false, false, false)
                            |   [arg] => mkFn arg (* Just a single function. *)
                            |   args => mkProductType(List.map mkFn args)
                        val addPPType = mkFunctionType(argPrints, mkFunctionType(installType, pretty))
                        val testType = mkFunctionType(tupleType,
                            mkFunctionType(
                                mkTypeConstruction(tcName constr, constr, typeVars, [DeclaredAt loc]),
                                pretty))
                    in
                        checkPPType(addPPType, testType, "addPrettyPrint", lex, loc,
                            fn () =>
                                PrettyString "addPrettyPrint element functions must have type 'a * int -> pretty")
                    end;

            (* Only report the error when the function is run.  Because addPrettyPrint is
               contained in the PolyML structure we may compile a reference to a polymorphic
               version of this for the structure record.  It's replaced in the final structure
               by this version. *)
        in
            case constrId of
                SOME (typeId, _, _) =>
                    (* Generate a function that will set the "print" ref for the type to
                       the argument function. *)
                    mkProc(
                        mkEval(
                            mkConst (ioOp POLY_SYS_assign_word),
                            [mkInd(1, codeAccess(idAccess typeId, level+1)), CodeZero, arg1],
                            false), 1, 1, "addPP")
                  (*mkConst (toMachineWord (fn _ => ()))*)
            |   NONE =>
                    mkConst (toMachineWord
                        (fn _ => raise Fail "addPrettyPrint: The argument type was not a simple type construction"))
        end

  |  codeVal (Value{access = Overloaded InstallPP, ...}, level, instance, lex, loc) =
        let
         (* This is the old function to install a pretty printer.  It has
            been retained for backwards compatibility.
            Since it will eventually be removed there's not much point in
            avoiding duplication with the code for AddPretty. *)
         (* "instance" should be ((,,,) -> int-> 'a -> 'b -> unit) -> unit.
             We need to get the 'a and 'b.  This function installs a
             pretty printer against the type which matches 'b.
             The type 'a is related to type of 'b as follows:
             If 'b is a monotype t then 'a is ignored.
             If 'b is a unary type constructor 'c t then 'a must have
             type 'c * int -> unit.
             If 'b is a binary or higher type constructor e.g. ('c, 'd, 'e) t
             then 'a must be a tuple of functions of the form
             ('c * int -> unit, 'd * int -> unit, 'e * int -> unit).
             When the installed function is called it will be passed the
             appropriate argument functions which it can call to print the
             argument types.  *)
            val argPrints  = mkTypeVar (generalisable, false, false, false);
            val installType   = mkTypeVar (generalisable, false, false, false);
            (* Build a pattern type and unify it to get the type variables.
               Unification should not fail here because type checking has already
               been done by the time we get here. *)
            (* The first type variable matches the set of pretty-printer functions
               we pass in and isn't needed for type info. *)
            val installPPType =
                mkFunctionType(
                    mkFunctionType(mkTypeVar (generalisable, false, false, false),
                        mkFunctionType (TYPESTRUCT.intType,
                            mkFunctionType(argPrints,
                                mkFunctionType(installType, TYPESTRUCT.unitType)))),
                    TYPESTRUCT.unitType);
            val () = checkPPType(instance, installPPType, "install_pp", lex, loc, fn () => PrettyString "");

            (* Find the last type constructor in the chain. We have to install
                 this against the last in the chain because type constructors in
                 different modules may be at different points in the chain. *)
              (* This does mean that it's not possible to install a
                 pretty printer for a type constructor rather than a datatype. *)
            fun followTypes (TypeConstruction{value, args, ...}) =
            let
                val constr = pling value
            in
                if not (tcIsAbbreviation constr)
                then
                let
                    val typeId = tcIdentifier constr
                in
                    (* Check that it's a top-level datatype (NOT in a functor) *)
                    if isFreeId typeId
                    then SOME(typeId, constr, List.length args)
                    else
                    (
                        errorMessage(lex, loc,
                            "A pretty printer cannot be installed for a type within a functor");
                        NONE
                    )
                end
                else followTypes (makeEquivalent (constr, args))
            end
            |   followTypes (TypeVar tv) =
                (
                    case tvValue tv of
                        EmptyType => NONE (* Unbound type variable *)
                    |   t => followTypes t
                )
            |   followTypes _ = NONE;

            val constrId = followTypes installType
            
            val () =
                case constrId of
                    NONE => ()
                |   SOME (_, constr, arity) =>
                    let
                        (* Check that the function tuple matches the arguments of the type
                           we're installing for. *)
                        (* Each entry should be a function of type 'a * int -> unit *)
                        fun mkFn arg = mkFunctionType(mkProductType[arg, TYPESTRUCT.intType], TYPESTRUCT.unitType)
                        (* Create non-unifiable type vars to ensure this is properly polymorphic. *)
                        val typeVars = List.tabulate(arity, fn _ => mkTypeVar (0, false, true, false))
                        val tupleType =
                            case typeVars of
                                [] => (* No arg so we can have anything here. *)
                                    mkTypeVar (generalisable, false, false, false)
                            |   [arg] => mkFn arg (* Just a single function. *)
                            |   args => mkProductType(List.map mkFn args)
                        val installPPType =
                            mkFunctionType(argPrints, mkFunctionType(installType, TYPESTRUCT.unitType))
                        val testType =
                            mkFunctionType(tupleType,
                                mkFunctionType(
                                mkTypeConstruction(tcName constr, constr, typeVars, [DeclaredAt loc]),
                                    TYPESTRUCT.unitType))
                    in
                        checkPPType(installPPType, testType, "install_pp", lex, loc,
                            fn () =>
                                PrettyString "install_pp element functions must have type 'a * int -> unit")
                    end;

            (* This is the type of the pretty-printer as seen by the user *)
            type pp =
                (* addString *)  (string -> unit) *
                (* beginBlock *) (int * bool -> unit) *
                (* break *)      (int * int -> unit) *
                (* endBlock *)   (unit -> unit);

            fun installPp (pprint:pp -> int -> machineWord -> machineWord -> unit) =
            case constrId of
                NONE => raise Fail "install_pp: The argument type was not a simple type construction"
            |   SOME (typeToInstallFor, _, tupleWidth) =>
                let
                    (* Convert the old imperative form into the new pretty datatype. *)
                    fun createPretty depth (elemFns: machineWord) (valToPrint: machineWord): pretty =
                    let
                        (* Result stack. This contains open Begins and also accumulates the result. *)
                        val resultStack = ref []
                        (* Add an entry to the top block in the stack. *)
                        fun addEntry p =
                            case ! resultStack of
                                [] => (* Nothing there so far. *)
                                    resultStack := [p]
                            |   PrettyBlock(i, b, c, l) :: tail  =>
                                    (* Add to current block. *)
                                    resultStack := PrettyBlock(i, b, c, l @ [p]) :: tail
                            |   t => (* Something there but not a Begin. *)
                                    raise Fail "Missing Begin"
                        fun addString s =
                            addEntry(PrettyString s)
                        and beginBlock(i: int, b: bool) =
                            addEntry(PrettyBlock(i, b, [], []))
                        and break (i: int, j: int) : unit =
                            addEntry(PrettyBreak(i, j))
                        and endBlock () =
                            case ! resultStack of
                                [] => raise Fail "End found with no Begin"
                            |   hd :: tl =>
                                    (* Pop the current block from the stack and
                                       add it as an entry to the immediately containing block. *)
                                (
                                    resultStack := tl;
                                    addEntry hd
                                )

                        val processElement: machineWord =
                        (* This is the single function or tuple of functions to process
                           the elements when this is a polytype. e.g. for "int" if this
                           is "int list". *)
                        case tupleWidth of
                            0 => toMachineWord (* Not used for monotypes. *)
                                    (fn _ => raise Fail "Monotype has no type argument")
                        |   1 => toMachineWord (* Single argument polytypes e.g. list *)
                                    (fn (v, depth) => addEntry(RunCall.unsafeCast elemFns (v, depth)))
                        |   n => (* Polytypes of more than one type e.g. ('a, 'b) pair. *)
                            let (* We have to construct a tuple of functions each of which calls
                                   the corresponding function in the tuple that is passed in. *)
                                fun getElem n (v, depth) =
                                let
                                    val prettyFn =
                                        RunCall.unsafeCast(loadWord(toAddress elemFns, toShort n))
                                in
                                    addEntry(prettyFn(v, depth))
                                end
                                (* As with the print code in printConstruction we use CodeTree
                                   to create the tuple. *)
                                val fns = List.tabulate (n, fn n => mkConst(toMachineWord(getElem n)))
                            in
                                evalue (mkTuple fns)
                            end
                    in
                        pprint (addString, beginBlock, break, endBlock) depth processElement valToPrint;
                        case ! resultStack of
                            [] => PrettyBlock(0, false, [], [])
                        |   [one] => one
                        |   _ => raise Fail "Incorrect nesting of begin...end blocks"
                    end
                in
                    case idAccess typeToInstallFor of
                        Global code =>
                                unsafeCast(evalue(mkInd(1, code))) := createPretty
                        |   _ => () (* Do nothing silently at the moment. *)
                end (* installPp *)

        in
            mkConst (toMachineWord installPp)
        end

  |  codeVal (value as Value{access = Overloaded _, ...}, level, instance, lex, lineno) =
       (* AddOverload, Equal, NotEqual, TypeDep *)
         mkProc
           (applyFunction (value, arg1, level, instance, lex, lineno),
           1, 1, "")
     (* codeVal *)

 (* Some of these have a more efficient way of calling them as functions. *)
 and applyFunction (value as Value{class=Exception, ...}, argument, level, instance, lex, lineno) =
     let
       (* If we are applying it as a function we cannot be after the
          exception id, we must be constructing an exception packet. *)
       (* Get the exception id, put it in the packet with the exception name
          the argument and, currently, an empty location as the exception location. *)
         val exIden = codeVal (value, level, instance, lex, lineno);
     in
         mkTuple (exIden :: mkStr (valName value) :: argument :: [mkConst(toMachineWord NoLocation)])
     end

  | applyFunction(value as Value{class=Constructor _, ...},
                    argument, level, instance, lex, lineno) =
     let
        (* If this is a value constructor we need to get the construction
           function and use that. *)
         val constrTriple = codeVal (value, level, instance, lex, lineno);
     in
         (* Don't apply this "early".  It might be the ref constructor and that
           must not be applied until run-time.  The optimiser should take care
           of any other cases. *)
        mkEval (mkInd(1, constrTriple), [argument], false)
     end

  | applyFunction (value as Value{access = Overloaded oper, name = valName, ...},
                     argument, level, instance, lex, lineno) =
     (
       case oper of
             Equal =>
            (* See if we have a special implementation for equality on
               this type.  If not we have to fall back to the default
               structure equality. *)
            (* Note: the overloadings will normally be inline functions
               which will unwrap the argument tuple and so elide it away.
               structureEq, though, is passed here as a pointer to
               the code so no such optimisation is possible and we will
               always make a tuple which will then be unwrapped inside
               structureEq. Two solutions are possible: we could build
               structureEq into the RTS in which case it would take an
               argument pair (usually in registers) or we could write it
               in the prelude and set it as an overload with some special
               type so that getOverload would return it as the default. *)
            let
                val code = genEqualityFunction(instance, level)
            in
                mkEval (code, [argument], true) (* evaluate early *)
            end
        
        | NotEqual =>
           let
                (* Use the "=" function to provide inequality as well as
                 equality. *)
             val code = genEqualityFunction(instance, level)
             val isEqual =
                 mkEval (code, [argument], true) (* evaluate early *)
           in
             mkNot isEqual
           end
       
        | TypeDep =>
           let
             val (code, _) =
                 getOverloadInstance(valName, instance, false, lex, lineno)
           in
             mkEval (code, [argument], true) (* evaluate early *)
           end

       | AddOverload =>
           (* AddOverload is only intended for use by writers of library modules.
           It only does limited checking and should be regarded as "unsafe". *)
           let
        (* instance should be ('a->'b) -> string -> unit.  For overloadings
           on most functions (e.g. abs and +) we are looking for the 'a, which
           may be a pair, but in the case of conversion functions we want the 'b. *)
           (* rmvars removes type variables put on by unification. *)
            fun rmvars (TypeVar tv) = rmvars(tvValue tv)
             | rmvars t = t

            fun followTypes(TypeConstruction{value, args, ...}):typeConstrs =
                let
                    val constr = pling value
                in
                   if not (tcIsAbbreviation constr)
                   then constr
                   else followTypes (makeEquivalent (constr, args))
                end
             | followTypes _ =
                     raise Fail "Invalid type (not a type construction) (addOverload)"

           (* In normal use the instance type would be a function and
              everything would be fine.  It is possible though that we
              might have something of the form val a = addOverload in
              which case we want to leave the error until runtime.  This
              particular case seems to happen as a result of open PolyML
              when PolyML contains addOverload. *)
           val (argType, resultType) =
               case rmvars instance of
                FunctionType{arg,...} =>
                (
                case arg of
                    (* We could do some checking of the type of the
                       function such as checking that we either have
                       something of the form t->t, t*t->t or t*t->bool
                       or string->t in the case of conversion functions.
                       It's probably not worth it since adding overloads
                       is only intended to be done by writers of libraries. *)
                    FunctionType{arg, result} =>
                        (
                        case (rmvars arg) of
                            LabelledType{recList=[{typeof, ...}, _], ...} =>
                                (rmvars typeof, rmvars result)
                          | t => (rmvars t, rmvars result)
                        )
                  | _ => (badType, badType)
                )
              | _ => (badType, badType)

            fun addOverloading (argCode: codetree) (name: string) =
                let
                    val typeToUse =
                        if size name > 4 andalso
                            String.substring(name, 0, 4) = "conv"
                        (* For conversion functions it's the result
                           type we're interested in. For everything
                           else it's the argument type. *)
                        then resultType
                        else argType
                    val tcons = followTypes typeToUse
                in
                    addOverload(name, tcons, argCode)
                end

            (* This function is used if we can't get the codetree at
               compile time. *)
            fun addOverloadGeneral (arg: machineWord) =
                addOverloading(mkConst arg)
        in
        (* This is messy but necessary for efficiency.  If we simply treat
           addOverload as a function we would be able to pick up the
           additional overloading as a pointer to a function.  Most overloads
           are small functions or wrapped calls to RTS functions and so
           we need to get the inline code for them. *)
           (
               (* evalue raises an exception if "argument" is not a constant,
                  or more usefully, a global value containing a constant and
                  possibly a piece of codetree to inline. *)
               evalue(argument);
               mkConst (toMachineWord (addOverloading argument))
           )
           handle SML90.Interrupt => raise SML90.Interrupt
             | _ =>
               mkEval (mkConst (toMachineWord addOverloadGeneral), [argument], false)
        end

      | _ => (* Print, MakeString, InstallPP *)
         (* Just call as functions. *) (* not early *)
            mkEval (codeVal (value, level, instance, lex, lineno), [argument], false)
            
     ) (* overloaded *)
     
  | applyFunction (value, argument, level, instance, lex, lineno) =
   (* Just call as functions. *)
       mkEval (codeVal (value, level, instance, lex ,lineno), [argument], false)
   (* end applyFunction *)

  (* If the exception is being used as a value we want an exception packet
     or a function to make a packet. If it is a nullary constructor make
     an exception packet now, otherwise generate a function to construct
     an exception packet. *)
 fun codeExFunction (value, level, instance, lex, lineno) =
    case getFnArgType(valTypeOf value) of (* N.B. Not "instance" *)
        NONE => applyFunction (value, CodeZero, level, instance, lex, lineno)
    |   SOME _ =>
            mkProc 
              (applyFunction (value, arg1, level + 1, instance, lex, lineno),
                1, 1, ""); (* shouldn't this function be in-lined??? SPF 20/10/94 *)

    (* Operations to compile code from the representation of a constructor. *)
    (* Code to test whether a value matches a constructor. *)
    fun makeGuard (value as Value{class=Constructor _, ...}, testing, level) =
        mkEval(mkInd(0, codeVal (value, level, emptyType, nullLex, location nullLex)),
            [testing], true)

    |   makeGuard (value as Value{class=Exception, ...}, testing, level) =
     (* Should only be an exception. Get the value of the exception identifier 
        and compare with the identifier in the exception packet. *)
     mkTestptreq 
        (mkInd (0, testing),
         codeVal (value, level, emptyType, nullLex, location nullLex))

    |   makeGuard _ = raise InternalError "makeGuard"

    (* Code to invert a constructor. i.e. return the value originally
    used as the argument. *)
    fun makeInverse(value as Value{class=Constructor{nullary=false, ...}, ...}, arg, level): codetree =
        mkEval(mkInd(2, codeVal (value, level, emptyType, nullLex, location nullLex)),
            [arg], false) (* NOT "early" - this may be the "ref" constructor. *)

    |  makeInverse(value as Value{class=Constructor{nullary=true, ...}, ...}, arg, level): codetree =
        (* makeInverse is called even on nullary constructors.  Return zero to keep the
           optimiser happy. *) CodeZero

    |   makeInverse (value as Value{class=Exception, ...}, arg, level) =
      (* Exceptions. - Get the parameter from third word *) mkInd (2,arg)

    |   makeInverse _ = raise InternalError "makeInverse"

 (* Get the current overload set for the function and return a new
    instance of the type containing the overload set. *)
 fun overloadType(Value{typeOf, access = Overloaded TypeDep, name, ...}, isConv) =
     let
        fun getTypes [] = []
         |  getTypes ((t, _) :: l) = t :: getTypes l

    in
        generaliseOverload(typeOf, getTypes(getOverloads name), isConv)
    end

 |  overloadType(Value{typeOf, ...}, isConv) =  generalise typeOf

  (* True if the arguments are definitely the same exception.  Used in the
     match compiler to see if we can merge adjacent exception patterns.
     Unlike the constructors of a datatype we can't assume that having the
     same (short) name means that the exceptions are the same, we have to
     look at the address. *)
   fun isTheSameException(Value{access = Global aCode, ...},
                             Value{access = Global bCode, ...}) : bool =
        wordEq(evalue aCode, evalue bCode)
       
   |  isTheSameException(Value{access = Local{addr=ref aAddr, level=ref aLevel}, ...},
                            Value{access = Local{addr=ref bAddr, level=ref bLevel}, ...}) : bool =
        (* I don't like this. It assumes that the address and level have
           already been set. *)
        aAddr = bAddr andalso aLevel = bLevel

  |  isTheSameException _ = false (* Forget about "selected" for the moment. *)

    (* Types that can be shared. *)
    structure Sharing =
    struct
        type machineWord    = machineWord
        type lexan          = lexan
        type codetree       = codetree
        type types          = types
        type values         = values
        type structVals     = structVals
        type functors       = functors
        type valAccess      = valAccess
        type typeConstrs    = typeConstrs
        type signatures     = signatures
        type fixStatus      = fixStatus
        type univTable      = univTable
        type pretty         = pretty
        type locationProp   = locationProp
        type typeId         = typeId
    end

end (* body of VALUEOPS *);


