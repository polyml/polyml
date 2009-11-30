(*
    Copyright (c) 2009 David C.J. Matthews

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

functor DATATYPE_REP (

structure CODETREE : CODETREESIG
structure STRUCTVALS : STRUCTVALSIG;
structure TYPESTRUCT : TYPETREESIG

structure MISC :
sig
  exception InternalError of string; (* compiler error *)
  
  val quickSort : ('a -> 'a -> bool) -> 'a list -> 'a list
end;

structure ADDRESS :
sig
  type machineWord;      (* any legal bit-pattern (tag = 0 or 1) *)
  type address;   (* an object that's represented as a pointer *) 
  type short = Word.word;   (* an object that's represented as a 30-bit int *)
  
  val isShort   : 'a -> bool;

  (* The following casts are always safe *)
  val toMachineWord : 'a    -> machineWord;
  
  (* The following casts are checked at run-time *)
  val toShort   : 'a -> short;
  val toAddress : 'a -> address;
  
  val alloc:  (short * Word8.word * machineWord)  -> address;

  val loadWord:   (address * short) -> machineWord;
  val assignWord: (address * short * machineWord)  -> unit
  val lock:       address -> unit

  val length: address -> short;
   
  val F_words     : Word8.word;
  val F_bytes     : Word8.word;
  val F_mutable   : Word8.word;
end;

structure COPIER: COPIERSIG
structure TYPEIDCODE: TYPEIDCODESIG

sharing STRUCTVALS.Sharing = TYPESTRUCT.Sharing
      = COPIER.Sharing = CODETREE.Sharing = ADDRESS = MISC
      = TYPEIDCODE.Sharing

) : DATATYPEREPSIG =

struct
  open MISC; 

  open CODETREE;
  open TYPESTRUCT; (* Open this first because unitType is in STRUCTVALS as well. *)
  open Universal; (* for tag etc. *)
  open STRUCTVALS;
  open ADDRESS;
  open RuntimeCalls; (* for POLY_SYS and EXC numbers *)
  open TYPEIDCODE
  open COPIER
  
  val length   = List.length;
  
  (* gets a value from the run-time system; 
    usually this is a closure, but sometimes it's an int.  *)
  val ioOp : int -> machineWord = RunCall.run_call1 POLY_SYS_io_operation;

  val orb = Word8.orb
  infix 7 orb;

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

    val mutableFlags = F_words orb F_mutable;

    (* allocate 1 mutable word, initialise to "v"; do not evaluate "early" *)
    fun refApplyCode (v: codetree) =
      mkEval
        (mkConst (ioOp POLY_SYS_alloc_store),
        [mkConst (toMachineWord 1), mkConst (toMachineWord mutableFlags), v],
        false);

(* Don't do this ...
    val OnlyOne  = UnboxedForm;
... because that gives the wrong test if the value isn't boxed. SPF 22/10/94 *)

  local
    
    fun abstract (doIt: codetree -> codetree) (name : string) : codetree =
      mkInlproc (doIt arg1, 0, 1, name);
  
    (* we use toShort to check that the tag actually fits in a 30-bit integer *)
    fun mkTag (tag:int) : codetree = mkConst (toMachineWord (toShort tag));

    (* tag now moved into first word SPF 22/10/94 *)
    (* get the tag from a TaggedBox or ConstForm *)
    fun loadTag (u: machineWord) : machineWord = 
      loadWord (toAddress u, 0w0); (* tag is first field *)

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
        (fn _ => raise InternalError "constant can't be applied")

  (* The run-time test whether a value matches a constructor. *)
  fun constrMatch (test: representations) (value:codetree) : codetree =
    case test of
      UnboxedForm          => testBoxed value
    | BoxedForm            => testBoxed value
    | RefForm              => CodeTrue
    | EnumForm tag         => testTag tag value 
    | TaggedBox tag        => testBoxedTag tag value
    | TaggedTuple (tag, _) => testBoxedTag tag value
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
    | TaggedBox _        => taggedBoxDestructCode value
    | TaggedTuple (_, n) => taggedTupleDestructCode n value
    | OnlyOne              => identityApplyCode value
    | EnumForm _         => CodeZero (* To keep optimiser happy. *)
    | ConstForm _          => CodeZero (* (rather than raising an exception) *)
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

        fun createNullaryConstructor (test, [], name) =
                mkTuple[mkInlproc(constrMatch test arg1, 0, 1, name), (* Test function. *)
                    constrMake test name (* Value. *)]
        |   createNullaryConstructor (test, tvs, name): codetree =
                mkTuple[mkInlproc(mkInlproc(constrMatch test arg1, 1, 1, name), 0, List.length tvs, name),
                    mkInlproc(constrMake test name, 0, List.length tvs, name)]
    
        fun createUnaryConstructor(test: representations, [], name: string): codetree =
                mkTuple[mkInlproc(constrMatch test arg1, 0, 1, name), (* Test function. *)
                    mkInlproc(constrApply test arg1, 0, 1, name), (* Injection function. *)
                    mkInlproc(constrDestruct test arg1, 0, 1, name) (* Projection function. *)]
        |   createUnaryConstructor(test: representations, tvs, name: string): codetree =
                mkTuple[mkInlproc(mkInlproc(constrMatch test arg1, 1, 1, name), 0, List.length tvs, name),
                    mkInlproc(mkInlproc(constrApply test arg1, 1, 1, name), 0, List.length tvs, name),
                    mkInlproc(mkInlproc(constrDestruct test arg1, 1, 1, name), 0, List.length tvs, name)]
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
    val () = lock vec;
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
    fun chooseOptimisedRepr1(_, _, []) = []
    |   chooseOptimisedRepr1(n, tvs, (Nullary, name) :: t) =
             createNullaryConstructor(EnumForm n, tvs, name) :: chooseOptimisedRepr1 (nextTag n, tvs, t)
    |   chooseOptimisedRepr1(n, tvs, (UnaryGeneric, name) :: t) =
               createUnaryConstructor(BoxedForm, tvs, name) :: chooseOptimisedRepr1(n, tvs, t)
    |   chooseOptimisedRepr1(n, tvs, (UnaryFunction, name) :: t) =
               createUnaryConstructor(UnboxedForm, tvs, name)  :: chooseOptimisedRepr1(n, tvs, t)
    |   chooseOptimisedRepr1(n, tvs, (UnaryTupled 0, name) :: t) =
               createUnaryConstructor(BoxedForm, tvs, name)    :: chooseOptimisedRepr1(n, tvs, t)
    |   chooseOptimisedRepr1(n, tvs, (UnaryTupled _, name) :: t) = 
               createUnaryConstructor(UnboxedForm, tvs, name)  :: chooseOptimisedRepr1(n, tvs, t)

    (* We use this version there's more than 1 unary constructor. *)
    (* With this representation constructors of small tuples make tuples of
     size n+1 whose first word is the tag.  Nullary constructors are represented
     by single word objects containing the tag. *)
    fun chooseOptimisedRepr2(_, _, []) = []
    |   chooseOptimisedRepr2(n, tvs, h :: t) = 
        let
            val repr = 
                case h of
                    (Nullary,       name) => createNullaryConstructor(genConstForm n, tvs, name)
                |   (UnaryGeneric,  name) => createUnaryConstructor(TaggedBox n, tvs, name)
                |   (UnaryFunction, name) => createUnaryConstructor(TaggedBox n, tvs, name)
                |   (UnaryTupled i, name) =>
                        createUnaryConstructor(
                            if i <= 4 (*!maxPacking*) then TaggedTuple (n, i) else TaggedBox n, tvs, name)
        in
            repr :: chooseOptimisedRepr2(nextTag n, tvs, t)
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
    
      | TypeConstruction {value, args, ...} =>
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
    fun chooseConstrRepr(cs, tvs) =
    let
        fun checkArgKind (name, EmptyType) = (Nullary, name)
         |  checkArgKind (name, argType) = (getTupleKind argType, name)
        val kinds = map checkArgKind cs;

        fun chooseRepr [(Nullary, name)]       = [createNullaryConstructor(UnitForm, tvs, name)]
        | chooseRepr [(UnaryGeneric, name)]  = [createUnaryConstructor(OnlyOne, tvs, name)]
        | chooseRepr [(UnaryFunction, name)] = [createUnaryConstructor(OnlyOne, tvs, name)]
        | chooseRepr [(UnaryTupled _, name)] = [createUnaryConstructor(OnlyOne, tvs, name)]
        | chooseRepr l =
        let
          val unaryCount = List.foldl(fn((Nullary, _), n) => n | (_,n) => n+1) 0 l
        in
          (* tags now allocated from 0 (SPF 22/10/94) *)
          if unaryCount <= 1
          then chooseOptimisedRepr1(0, tvs, l) (* can save the box *)
          else chooseOptimisedRepr2(0, tvs, l) (* can use tagged tuples *)
        end;

       in
        chooseRepr kinds
    end;
 
   (* RefForm, NilForm and ConsForm are only used for built-in types *)


(*****************************************************************************)
(*             Standard values and exceptions.                               *)
(*****************************************************************************)

    (* Build a datatype within the basis. *)
    fun buildBasisDatatype(tcName, tIdPath, tyVars, isEqType: bool,
            mkValConstrs: typeConstrs -> (values * codetree) list) =
    let
        (* Create a temporary datatype.  The "name" we put in here is usually the
           same as the type constructor name except for datatypes in the PolyML structure
           which have the PolyML prefix. *)
        val description = basisDescription tIdPath
        val id =
            makeBoundId(Local{addr = ref 0, level = ref 0}, 0 (* IdNumber*),
                isEqType, true, description, ([], EmptyType))
        val dtype = makeDatatypeConstr (tcName, tyVars, id, 0, [DeclaredAt inBasis]);
        (* Build the constructors. *)
        val valConstrsAndDecs = mkValConstrs dtype
        (* The constructors have to be ordered as in genValueConstrs in PARSE_TREE. *)
        fun leq (Value{name=xname, ...}, _) (Value{name=yname, ...}, _) = xname < yname;
        val sortedConstrs = quickSort leq valConstrsAndDecs;
        val () = tcSetConstructors(dtype, (List.map #1 valConstrsAndDecs))
        val addrs = ref 0
        fun mkAddrs() = ! addrs before (addrs := !addrs+1)

        fun declConstr(Value{access=Local{addr, level}, ...}, repr) =
            let
                val newAddr = mkAddrs()
            in
                addr := newAddr;
                level := 0;
                (mkDec(newAddr, repr), mkLoad(newAddr, 0))
            end
        |   declConstr _ = raise InternalError "declConstr: not local"
        val (declConstrs, loadConstrs) = ListPair.unzip(List.map declConstr sortedConstrs)

        (* Create the datatype.  Sets the address of the local in "id". *)
        val dtCode = createDatatypeFunctions([dtype], [isEqType], mkAddrs, 0)

        (* Compile and execute the code to build the functions and extract the result. *)
        val globalCode = genCode(mkEnv(declConstrs @ dtCode @ [mkTuple(codeId(id, 0) :: loadConstrs)]), [])()
        val newId = makeFreeId(Global(mkInd(0, globalCode)), isEqType, description, ([], EmptyType))
        
        (* Get the value constructors out as globals. *)
        fun mkGlobal((Value{name, typeOf, class, locations,  ...}, _), (decs, offset)) =
            (decs @ [Value{name=name, typeOf=typeOf, class=class, locations=locations, references=NONE,
                     access=Global(mkInd(offset, globalCode))}], offset+1)
        val (gConstrs, _) = List.foldl mkGlobal ([], 1 (* Offset 0 is the type ID *)) sortedConstrs
        val () = tcSetConstructors(dtype, gConstrs)

        (* Finally copy the datatype to put in the code. *)
    in
        fullCopyDatatype(dtype, fn 0 => newId | _ => raise Subscript, "")
    end

    (* Nil and :: are used in parsetree for lists constructed
       using [ ... ] and are also used for initialisation. *)
    local
        fun makeConsAndNil listType =
        let
            val listTypeVars  = tcTypeVars listType;
            val alpha         = TypeVar(hd listTypeVars);
            val alphaList     = mkTypeConstruction ("list", listType, [alpha], [DeclaredAt inBasis]);
            val consType      = mkFunctionType (mkProductType [alpha, alphaList], alphaList);
            val nilConstructor  =
                makeValueConstr ("nil", alphaList, true, 2, Local{addr=ref 0, level=ref 0}, [DeclaredAt inBasis])
            val consConstructor =
                makeValueConstr ("::",  consType,  false, 2, Local{addr=ref 0, level=ref 0}, [DeclaredAt inBasis])
            val nilRepresentation = createNullaryConstructor(NilForm, [alpha], "nil")
            val consRepresentation = createUnaryConstructor(ConsForm, [alpha], "::")
        in
            [(nilConstructor, nilRepresentation), (consConstructor, consRepresentation)]
        end
    in
        val listType =
            buildBasisDatatype("list", "list",
                [makeTv (EmptyType, generalisable, false, false)], true, makeConsAndNil)
        val (nilConstructor, consConstructor) =
            case tcConstructors listType of
                [consC as Value{name="::", ...}, nilC as Value{name="nil", ...}] => (nilC, consC)
            |   _ => raise InternalError "nil and cons in wrong order"
    end

    local
        fun makeNoneAndSome optionType =
        let
            val optionTypeVars  = tcTypeVars optionType;
            val alpha         = TypeVar(hd optionTypeVars);
            val alphaOption   = mkTypeConstruction ("option", optionType, [alpha], [DeclaredAt inBasis]);
            val someType      = mkFunctionType (alpha, alphaOption);
            val noneConstructor  =
                makeValueConstr ("NONE", alphaOption, true, 2, Local{addr=ref 0, level=ref 0}, [DeclaredAt inBasis]);
            val someConstructor =
                makeValueConstr ("SOME", someType, false, 2, Local{addr=ref 0, level=ref 0}, [DeclaredAt inBasis]);
            val noneRepresentation = createNullaryConstructor(EnumForm 0, [alpha], "NONE")
            and someRepresentation = createUnaryConstructor(BoxedForm, [alpha], "SOME")
        in
            [(noneConstructor, noneRepresentation), (someConstructor, someRepresentation)]
        end
    in
        val optionType =
            buildBasisDatatype("option", "option",
                [makeTv (EmptyType, generalisable, false, false)], true, makeNoneAndSome)
        val (noneConstructor, someConstructor) =
            case tcConstructors optionType of
                [noneC as Value{name="NONE", ...}, someC as Value{name="SOME", ...}] => (noneC, someC)
            |   _ => raise InternalError "NONE and SOME in wrong order"
    end

    local
        fun listConstr (base : types) : types =
            mkTypeConstruction ("list", listType, [base], [DeclaredAt inBasis]);

        val intTypeConstr    = TYPESTRUCT.intType
        val stringTypeConstr = TYPESTRUCT.stringType
        val boolTypeConstr   = TYPESTRUCT.boolType

        fun makeTypeAbbreviation(name, typeVars, typeResult, locations) =
            makeFrozenTypeConstrs(
                name, typeVars, makeFreeId(Global CodeZero, false, basisDescription name, (typeVars, typeResult)),
                0, locations)
    in
        local
            val fields =
                [
                    mkLabelEntry("file", stringTypeConstr), mkLabelEntry("startLine", intTypeConstr),
                    mkLabelEntry("startPosition", intTypeConstr), mkLabelEntry("endLine", intTypeConstr),
                    mkLabelEntry("endPosition", intTypeConstr)
                ]
        in
            val locationConstr =
                makeTypeAbbreviation("location", [], mkLabelled(sortLabels fields, true), [DeclaredAt inBasis]);
        end

        local
            (* Pretty print context information. *)
            fun makeConstructors typeconstr =
            let
                val contextType = mkTypeConstruction ("context", typeconstr, [], [DeclaredAt inBasis])
                val locationType = mkTypeConstruction ("location", locationConstr, [], [DeclaredAt inBasis])
                val constrs =
                   [ ("ContextLocation", locationType),
                     ("ContextParentStructure", mkProductType[stringTypeConstr, listConstr contextType]),
                     ("ContextProperty", mkProductType[stringTypeConstr, stringTypeConstr])];
                (* We rely on chooseConstrRepr giving us the same representation for the
                   constructors as when context was compiled by the previous compiler. *)
                val numConstrs = List.length constrs
                fun makeCons(s,t) =
                    makeValueConstr(s, mkFunctionType(t, contextType), false, numConstrs,
                        Local{addr=ref 0, level=ref 0}, [DeclaredAt inBasis])
            in
                ListPair.zipEq(List.map makeCons constrs, chooseConstrRepr(constrs, []))
            end
        in
            val contextConstr =
                buildBasisDatatype("context", "PolyML.context", [], false, makeConstructors)
        end

        local
            fun makeConstructors typeconstr =
            let
                val prettyType = mkTypeConstruction ("pretty", typeconstr, [], [DeclaredAt inBasis])
                val contextType = mkTypeConstruction ("context", contextConstr, [], [DeclaredAt inBasis])

                val constrs =
                   [ ("PrettyBlock",
                        mkProductType[intTypeConstr, boolTypeConstr, listConstr contextType, listConstr prettyType]),
                     ("PrettyBreak", mkProductType[intTypeConstr, intTypeConstr]),
                     ("PrettyString", stringTypeConstr)];
                (* We rely on chooseConstrRepr giving us the same representation for the
                   constructors as when PrettyPrint was compiled by the previous compiler. *)
                val numConstrs = List.length constrs
                fun makeCons(s,t) =
                    makeValueConstr(s, mkFunctionType(t, prettyType), false, numConstrs,
                        Local{addr=ref 0, level=ref 0}, [DeclaredAt inBasis])
            in
                ListPair.zipEq(List.map makeCons constrs, chooseConstrRepr(constrs, []))
            end
        in
            val prettyConstr =
                buildBasisDatatype("pretty", "PolyML.pretty", [], false, makeConstructors)
        end
    end

    (* Construct an exception identifier - This is a ref (so we can uniquely
       identify it) containing a print function for the type. *)
    local
        (* The exception identifier contains a value of type (exn*int->pretty) option. *)
        val exnPrinter =
            mkTypeConstruction ("option", optionType,
                [
                    mkFunctionType(
                        mkProductType[TYPESTRUCT.exnType, TYPESTRUCT.intType],
                        mkTypeConstruction ("pretty", prettyConstr, [], [DeclaredAt inBasis])
                    )
                ],
                [DeclaredAt inBasis])
    in
        fun mkExIden(ty, level, tvMap) =
        let
            (* Get the constructor tuple, select the constructor operation, apply it to the type. *)
            val makeSome =
                applyToInstance(
                    [exnPrinter], level, tvMap,
                        fn _ => mkInd(1, case someConstructor of Value { access, ...} => vaGlobal access))
            val makeNone =
                applyToInstance(
                    [exnPrinter], level, tvMap,
                        fn _ => mkInd(1, case noneConstructor of Value { access, ...} => vaGlobal access))
            val printerCode =
                case ty of
                    FunctionType { arg, ...} =>
                        mkEval(makeSome, [printerForType(arg, level, tvMap)], true)
                |   _ => makeNone
        in
            refApplyCode printerCode
        end
    end

    (* Types that can be shared. *)
    structure Sharing =
    struct
        type codetree       = codetree
        type types          = types
        type values         = values
        type typeConstrs    = typeConstrs
        type typeId         = typeId
        type typeVarForm    = typeVarForm
        type typeVarMap     = typeVarMap
    end

end;



