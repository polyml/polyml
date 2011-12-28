(*
    Copyright (c) 2000
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
    Title:      Machine-independent Code and Optimisation.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)

(*
    Substantially modified.
    Changes copyright. David C.J. Matthews 2001.
*)
functor CODETREE (

(*****************************************************************************)
(*                  GCODE                                                    *)
(*****************************************************************************)
structure GCODE :
sig
    type machineWord = Address.machineWord
    type codetree
    val gencode: codetree * Universal.universal list * int -> unit -> machineWord;
end (* GCODE *);

(*****************************************************************************)
(*                  DEBUG                                                    *)
(*****************************************************************************)
structure DEBUG :
sig
    val codetreeTag:            bool Universal.tag (* If true then print the original code. *)
    val codetreeAfterOptTag:    bool Universal.tag (* If true then print the optimised code. *)
    val maxInlineSizeTag:       int  Universal.tag
    val getParameter :
       'a Universal.tag -> Universal.universal list -> 'a
end (* DEBUG *);

(*****************************************************************************)
(*                  Pretty data structure                                    *)
(*****************************************************************************)
structure PRETTY : PRETTYSIG

structure BASECODETREE: BaseCodeTreeSig

(*****************************************************************************)
(*                  CODETREE sharing constraints                             *)
(*****************************************************************************)
sharing type
    PRETTY.pretty
=   BASECODETREE.pretty

sharing type
  BASECODETREE.codetree
= GCODE.codetree
) : CODETREESIG =

(*****************************************************************************)
(*                  CODETREE functor body                                    *)
(*****************************************************************************)
struct
  open GCODE;
  open Address;
  open StretchArray;
  open RuntimeCalls; (* for POLY_SYS numbers and EXC_nil *)
  open BASECODETREE;
  open PRETTY;
  
  exception InternalError = Misc.InternalError
  and Interrupt = Thread.Thread.Interrupt
  
  infix 9 sub;

  val isConstnt    = fn (Constnt _)    => true | _ => false;
  val isCodeNil    = fn CodeNil        => true | _ => false; (* Exported *)

    type loadForm = 
    { (* Load a value. *)
        addr : int, 
        level: int, 
        fpRel: bool,
        lastRef: bool
    }
    
    and declarForm = 
    { (* Declare a value or push an argument. *)
        value:      codetree,
        addr:       int,
        references: int
    }
   
    and lambdaForm =
    { (* Lambda expressions. *)
        body          : codetree,
        isInline      : inlineStatus,
        name          : string,
        closure       : codetree list,
        argTypes      : argumentType list,
        resultType    : argumentType,
        level         : int,
        closureRefs   : int,
        localCount    : int,
        makeClosure   : bool,
        argLifetimes  : int list
    }


(*****************************************************************************)
(*                         optVal functions                                  *)
(*****************************************************************************)
  (* Processing each expression results in a "optVal" value. This contains a 
     "general" value which can be used anywhere and a "special" value which
     provides optimisations of inline procedures and tuples. "environ" is a
     procedure for mapping addresses in "special" if it is used and "decs" is 
     any declarations needed by either "general" or "special". The reason for
     returning both "general"  and "special" is so that we only create a
     tuple or a procedure once. In the case of a tuple "special" contains
     code to generate the tuple from its elements and is provided so that
     operations which select from the tuple can be optimised into loading
     the element. "General" will contain code to generate the tuple, or in
     the case of a declaration of a tuple, will contain a "load" instruction 
     to get the value.
  *)
  
    fun errorEnv (_: loadForm, _: int, _: int) : optVal =
      raise InternalError "error env";
  
    fun optGeneral (OptVal {general,...})       = general 
      | optGeneral (ValWithDecs {general, ...}) = general
      | optGeneral (JustTheVal ct)              = ct;
      
    fun optSpecial (OptVal {special,...}) = special
      | optSpecial _                      = CodeNil;
      
    fun optEnviron (OptVal {environ,...}) = environ
      | optEnviron _                      = errorEnv;
      
    fun optDecs    (OptVal {decs,...})       = decs
      | optDecs    (ValWithDecs {decs, ...}) = decs
      | optDecs    (JustTheVal _)           = [];
  
    fun optRec     (OptVal {recCall,...})       = recCall
      | optRec     _ = ref false; (* Generate a temporary. *)
  
    val simpleOptVal : codetree -> optVal = JustTheVal;

    fun optVal{special=CodeNil, decs=[], general, ...} = JustTheVal general
    |   optVal{special=CodeNil, decs, general, ...} = ValWithDecs {general = general, decs = decs}
    |   optVal ov = OptVal ov

  (* gets a value from the run-time system *)
  val ioOp : int -> machineWord = RunCall.run_call1 POLY_SYS_io_operation;

  val word0 = toMachineWord 0;
  val word1 = toMachineWord 1;
  
  val False = word0;   
  val True  = word1;   

  (* since code generator relies on these representations,
     we may as well export them *)
  val mkConst: machineWord->codetree   = Constnt;
  val CodeFalse = mkConst False;
  val CodeTrue  = mkConst True;
  val CodeZero  = mkConst word0;
  val rtsFunction = mkConst o ioOp
  
  val F_mutable_words : Word8.word = Word8.orb (F_words, F_mutable);

  type casePair = 
       (* Expressions and corresponding list of labels. *)
       codetree * int list

(*************************** end of copied code *****************************)  
  

    fun mkAltMatch (m1, m2) = AltMatch (m1, m2);

    fun mkDecRef(ct, i1, i2) = Declar{value = ct, addr = i1, references = i2};
    fun mkGenLoad  (i1, i2, bl, lf) =
        Extract {addr  = i1, level = i2, fpRel = bl, lastRef = lf};
  
  (* Used for recursive functions - setting the "closure" flag
     is a real hack. We also have to adjust the level number by
     one because we don't really create an extra level. I'm not sure
     whether this adjustment should really be here or in VALUEOPS.ML -
     it's currently in the latter, because I think it's a parser-related
     hack!  SPF 11/4/96
   *)
  fun mkRecLoad level =
    Extract {level = level, addr = 0, fpRel = false, lastRef = false};
  
    fun mkLoad (addr,level) =
    if level < 0 then raise InternalError "mkLoad: level must be non-negative"
    else Extract {level = level, addr = addr, fpRel = true, lastRef = false}

    fun mkClosLoad(addr, last) = Extract {level = 0, addr = addr, fpRel = false, lastRef = last}

    val mkEnv    = Newenv;

  (* Wrap up multiple entries.  Return a single item unless it is a
     declaration. *)
  fun wrapEnv (l as [Declar _]) = mkEnv l
    | wrapEnv (l as [MutualDecs _]) = mkEnv l
    | wrapEnv [singleton] = singleton
    | wrapEnv multiple = mkEnv multiple
  
  (* generates a declaration operation *)
  fun mkDec (laddr, res) = mkDecRef(res, laddr, 0)

    (* Old form operations for backwards compatibility.  These all create
       default GeneralType arguments and results. *)

    (* lambda operation: returns a procedure *)
    fun mkProc (lval, args, name) =
    Lambda
      {
        body          = lval,
        isInline      = NonInline,
        name          = if name = "" then "<anon>" else name,
        closure       = [],
        argTypes      = List.tabulate(args, fn _ => GeneralType),
        resultType    = GeneralType,
        level         = 0,
        closureRefs   = 0,
        localCount    = 0,
        makeClosure   = false,
        argLifetimes  = []
      }                     

    (* inline lambda operation: returns an inline procedure *)
    fun mkInlproc (lval, args, name) =
    Lambda
      {
        body          = lval,
        isInline      = MaybeInline,
        name          = if name = "" then "<anon>" else name,
        closure       = [],
        argTypes      = List.tabulate(args, fn _ => GeneralType),
        resultType    = GeneralType,
        level         = 0,
        closureRefs   = 0,
        localCount    = 0,
        makeClosure   = false,
        argLifetimes  = []
      };

    (* OnlyInline: used for functors unless inlineFunctors is false. *)
    fun mkMacroProc (lval, args, name) =
    Lambda
      {
        body          = lval,
        isInline      = OnlyInline,
        name          = if name = "" then "<anon>" else name,
        closure       = [],
        argTypes      = List.tabulate(args, fn _ => GeneralType),
        resultType    = GeneralType,
        level         = 0,
        closureRefs   = 0,
        localCount    = 0,
        makeClosure   = false,
        argLifetimes  = []
      };

    fun mkEval (ct, clist, bl)   =
    Eval {
        function = ct,
        argList = List.map(fn c => (c, GeneralType)) clist,
        earlyEval = bl,
        resultType=GeneralType
    }

    fun mkFunction(body, argsAndTypes, resultType, name) =
    Lambda
      {
        body          = body,
        isInline      = NonInline,
        name          = if name = "" then "<anon>" else name,
        closure       = [],
        argTypes      = argsAndTypes,
        resultType    = resultType,
        level         = 0,
        closureRefs   = 0,
        localCount    = 0,
        makeClosure   = false,
        argLifetimes  = []
      }

    fun mkInlineFunction(body, argsAndTypes, resultType, name) =
    Lambda
      {
        body          = body,
        isInline      = MaybeInline,
        name          = if name = "" then "<anon>" else name,
        closure       = [],
        argTypes      = argsAndTypes,
        resultType    = resultType,
        level         = 0,
        closureRefs   = 0,
        localCount    = 0,
        makeClosure   = false,
        argLifetimes  = []
      }

    fun mkCall(func, argsAndTypes, resultType) =
    Eval {
        function = func,
        argList = argsAndTypes,
        earlyEval = false,
        resultType=resultType
    }

  fun mkNot arg = mkEval (rtsFunction POLY_SYS_not_bool, [arg], true);


  val testptreqFunction    = rtsFunction POLY_SYS_word_eq
  val testptrneqFunction   = rtsFunction POLY_SYS_word_neq
  
  fun mkTestptreq  (xp1, xp2) = mkEval (testptreqFunction, [xp1,xp2], true);
  fun mkTestptrneq (xp1, xp2) = mkEval (testptrneqFunction, [xp1,xp2], true);
  fun mkTestnull xp1       = mkTestptreq  (xp1, CodeZero);
  fun mkTestnotnull xp1    = mkTestptrneq (xp1, CodeZero);

    val mkIf = Cond

    (* For the moment limit these to general arguments. *)
    fun mkLoop args = Loop (List.map(fn c => (c, GeneralType)) args)
    and mkBeginLoop(exp, args) = BeginLoop{loop=exp, arguments=List.map(fn c => (c, GeneralType)) args}

    fun mkWhile(b, e) = (* Generated as   if b then (e; <loop>) else (). *)
        mkBeginLoop(mkIf(b, mkEnv[e, mkLoop[]], CodeZero), [])

  (* We previously had conditional-or and conditional-and as separate
     instructions.  I've taken them out since they can be implemented
     just as efficiently as a normal conditional.  In addition they
     were interfering with the optimisation where the second expression
     contained the last reference to something.  We needed to add a
     "kill entry" to the other branch but there wasn't another branch
     to add it to.   DCJM 7/12/00. *)
  fun mkCor(xp1, xp2)  = mkIf(xp1, CodeTrue, xp2);
  fun mkCand(xp1, xp2)  = mkIf(xp1, xp2, CodeZero);

  (* N.B. int equality is SHORT integer equality *)
  fun mkTestinteq (xp1, xp2) = 
    mkEval (rtsFunction POLY_SYS_word_eq, [xp1,xp2], true);

    (* Test a tag value. *)
    fun mkTagTest(test: codetree, tagValue: word, maxTag: word) =
        TagTest {test=test, tag=tagValue, maxTag=maxTag }
        (*mkEval (rtsFunction POLY_SYS_word_eq, [test, mkConst(toMachineWord tagValue)], true);*)

  fun mkHandle (exp, handler) = Handle {exp = exp, handler = handler};
  
  val mkRaise      = Raise;
  val mkMutualDecs = MutualDecs;

  fun mkStr (strbuff:string) = mkConst (toMachineWord strbuff);

  val mkContainer  = Container

    val mkIndirectVariable = IndirectVariable
    and mkTupleVariable = TupleVariable

    (* Construct a new tuple from a sub-section of an existing one. *)
    fun mkTupleSlice{ base, offset, length } =
        TupleVariable(
            [VarTupleMultiple{base=base, length=length, destOffset=CodeZero, sourceOffset=offset}],
            length)

  (* If we have multiple references to a piece of code we may have to save
     it in a temporary and then use it from there. If the code has side-effects
      we certainly must do that to ensure that the side-effects are done
      exactly once and in the correct order, however if the code is just a
      constant or a load we can reduce the amount of code we generate by
      simply returning the original code. *)
  fun multipleUses (code as Constnt _, _, _) = 
      {load = (fn _ => code), dec = []}

   |  multipleUses (code as Extract{addr, level=loadLevel, ...}, _, level) = 
    let (* May have to adjust the level. *)
      fun loadFn lev =
        if lev = level
        then code 
        else mkLoad (addr, loadLevel + (lev - level))
    in
      {load = loadFn, dec = []}
    end
    
   |  multipleUses (code, nextAddress, level) = 
    let
      val addr       = nextAddress();
      fun loadFn lev = mkLoad (addr, lev - level);
    in
      {load = loadFn, dec = [mkDec (addr, code)]}
    end (* multipleUses *);
  
    fun identityFunction (name : string) : codetree = 
        mkInlproc (mkLoad (~1, 0), 1, name) (* Returns its argument. *);

  (* Set the container to the fields of the record.  Try to push this
     down as far as possible. *)
  fun mkSetContainer(container, Cond(ifpt, thenpt, elsept), size) =
    Cond(ifpt, mkSetContainer(container, thenpt, size),
         mkSetContainer(container, elsept, size))

  |  mkSetContainer(container, Newenv entries, size) =
    let
        fun applyLast [] = raise List.Empty
        |   applyLast [last] =
                [mkSetContainer(container, last, size)]
        |   applyLast (hd::tl) = hd :: applyLast tl
    in
        Newenv(applyLast entries)
    end

  |  mkSetContainer(_, r as Raise _, _) =
        r (* We may well have the situation where one branch of an "if" raises an
             exception.  We can simply raise the exception on that branch. *)

  | mkSetContainer(container, tuple, size) =
        SetContainer{container = container, tuple = tuple, size = size }

  (* Create a tuple from a container. *)
  val mkTupleFromContainer = TupleFromContainer

  (* Makes a constant value from an expression which is known to be *)
  (* constant but may involve inline procedures, types etc.         *)
  fun makeConstVal (cVal:codetree) =
  let
    fun makeVal (Constnt c) = c
      (* should just be a tuple  *)
      (* Get a vector, copy the entries into it and return it as a constant. *)
    | makeVal (Recconstr []) = word0 (* should have been optimised already! *)
    | makeVal (Recconstr xp) =
        let
          val vec : address = alloc (toShort (List.length xp), F_mutable_words, word0);
          
          fun copyToVec []       _ = ()
            | copyToVec (h :: t) locn =
              (
            assignWord (vec, toShort locn, makeVal h);
            copyToVec t (locn + 1)
              );
        in
          copyToVec xp 0;
          lock vec;
          toMachineWord vec
        end
    | makeVal _ = raise InternalError "makeVal - not constant or record"
  in
    mkConst (makeVal cVal)
  end;

  local
    fun allConsts []       = true
      | allConsts (Constnt _ :: t) = allConsts t
      | allConsts _ = false
  in  
    fun mkTuple xp =
    let
       val tuple = Recconstr xp
    in
      if allConsts xp
      then (* Make it now. *) makeConstVal tuple
      else tuple
    end;
  end;

  (* Look for an entry in a tuple. Used in both the optimiser and in mkInd. *)
  fun findEntryInBlock (Recconstr recs) offset =
      if offset < List.length recs
      then List.nth(recs, offset)
      else (* This can arise if we're processing a branch of a case discriminating on
              a datatype which won't actually match at run-time. *)
          mkRaise (mkTuple [mkConst (toMachineWord EXC_Bind), mkStr "Bind", CodeZero, CodeZero])

  |  findEntryInBlock (Constnt b) offset =
    ( 
      (* The ML compiler may generate loads from invalid addresses as a
         result of a val binding to a constant which has the wrong shape.
         e.g. val a :: b = nil
         It will always result in a Bind exception being generated 
         before the invalid load, but we have to be careful that the
         optimiser does not fall over.  *)
      if isShort b
      orelse not (Address.isWords (toAddress b)) (* DCJM's bugfix SPF 25/1/95 *)
      orelse Address.length (toAddress b) <= Word.fromInt offset
      then mkRaise (mkTuple [mkConst (toMachineWord EXC_Bind), mkStr "Bind", CodeZero, CodeZero])
      else mkConst (loadWord (toAddress b, toShort offset))
    )
    
  |  findEntryInBlock (Global glob) offset =
    (* Do the selection now - it makes the code-tree much more readable if *)
    (* we don't print the whole of the int type whenever we have int.+. *)
      (
      case optSpecial glob of
        recc as Recconstr _ =>
        (
        case findEntryInBlock recc offset of
           (* Most entries in the list are load instructions, however if 
           the entry we want is in a type which has been extended we
           will return an indirection.
           DCJM 28/11/99.  That may not apply to ML. *)
            Extract (ext as {level, ...}) =>
              Global ((optEnviron glob) (ext, 0, (* global *) level))
          | Indirect{base=Extract (ext as {level, ...}), offset} =>
            let 
              (* Must be indirecting on a local value. Look it up and do the
                 indirection recursively. *)
              val newBase =
                 Global ((optEnviron glob) (ext, 0, (* global *) level))
            in
              findEntryInBlock newBase offset
            end
        
        | selection => selection (* constants *)
        )
      
      | _ => findEntryInBlock (optGeneral glob) offset
      )
    
  |  findEntryInBlock base offset =
        Indirect {base = base, offset = offset} (* anything else *)
     (* end findEntryInBlock *);
        
  (* indirect load operation *)
  fun mkInd (addr, base as Global _ ) = findEntryInBlock base addr
   |  mkInd (addr, base as Constnt _) = findEntryInBlock base addr
   |  mkInd (addr, base) = Indirect {base = base, offset = addr};
        
    (* Get the value from the code. *)
    fun evalue (Constnt c) = SOME c
    |   evalue (Global g) = evalue(optGeneral g)
    |   evalue _ = NONE

  (* Test for possible side effects. If an expression has no side-effect
     and its result is not used then we don't need to generate it. An
     expresssion is side-effect free if it does not call a procedure or
     involve an instruction which could raise an exception. Only the more
     common instructions are included. There may be some safe expressions
     which this procedure thinks are unsafe. *)
  (* Calls which could raise an exception must not be included.
     Most arbitrary precision operations, word operations and
     real operations don't raise exceptions (we don't get overflow
     exceptions) so are safe.  *)
  (* The application of ioOp has been moved out of the isInList since it
     turned out to be a hot-spot. *)
  val safeRTSCalls = map ioOp
    [POLY_SYS_get_length,
     POLY_SYS_teststrgtr,
     POLY_SYS_teststrlss, POLY_SYS_teststrgeq, POLY_SYS_teststrleq,
     POLY_SYS_is_short, POLY_SYS_aplus, POLY_SYS_aminus, POLY_SYS_amul,
     POLY_SYS_aneg, POLY_SYS_xora,
     POLY_SYS_equala, POLY_SYS_ora, POLY_SYS_anda,
     POLY_SYS_Real_str, POLY_SYS_Real_geq, POLY_SYS_Real_leq,
     POLY_SYS_Real_gtr, POLY_SYS_Real_lss, POLY_SYS_Real_eq,
     POLY_SYS_Real_neq, POLY_SYS_Add_real, POLY_SYS_Sub_real,
     POLY_SYS_Mul_real, POLY_SYS_Div_real, POLY_SYS_Abs_real, POLY_SYS_Neg_real,
     POLY_SYS_sqrt_real, POLY_SYS_sin_real, POLY_SYS_cos_real,
     POLY_SYS_arctan_real, POLY_SYS_exp_real, POLY_SYS_ln_real,
     POLY_SYS_io_operation, POLY_SYS_shift_right_arith_word,
     POLY_SYS_is_big_endian, POLY_SYS_bytes_per_word,
     POLY_SYS_shift_right_word, POLY_SYS_word_neq, POLY_SYS_not_bool,
     POLY_SYS_string_length,
     POLY_SYS_int_geq, POLY_SYS_int_leq, POLY_SYS_int_gtr, POLY_SYS_int_lss,
     POLY_SYS_mul_word, POLY_SYS_plus_word,
     POLY_SYS_minus_word, POLY_SYS_or_word,
     POLY_SYS_and_word, POLY_SYS_xor_word, POLY_SYS_shift_left_word,
     POLY_SYS_word_geq, POLY_SYS_word_leq,
     POLY_SYS_word_gtr, POLY_SYS_word_lss, POLY_SYS_word_eq,
     POLY_SYS_get_first_long_word,
     POLY_SYS_bytevec_eq]
  val divisionOperations = map ioOp
   [POLY_SYS_adiv, POLY_SYS_amod, POLY_SYS_div_word, POLY_SYS_mod_word]

    (* Operations that may raise exceptions. *)
    val exceptionOperations = map ioOp
        [POLY_SYS_conv_real, POLY_SYS_real_to_int, POLY_SYS_int_to_real]
    val safeForImmutable =
        map ioOp [POLY_SYS_get_flags, POLY_SYS_load_byte, POLY_SYS_load_word]

  (* Note: This simply returns true or false.  For complex expressions,
     such as an RTS call whose argument has a side-effect, we could
     reduce the code by extracting the sub-expressions with side-effects
     and returning just those. *)
  fun sideEffectFree CodeNil = true
    | sideEffectFree (Lambda _) = true
    | sideEffectFree (Constnt _) = true
    | sideEffectFree (Extract _) = true
    | sideEffectFree (Declar{value, ...}) = sideEffectFree value
    | sideEffectFree (Cond(i, t, e)) =
          sideEffectFree i andalso
          sideEffectFree t andalso
          sideEffectFree e
    | sideEffectFree (Newenv decs) = testList decs
    | sideEffectFree (Handle { exp, handler }) =
          sideEffectFree exp andalso sideEffectFree handler
    | sideEffectFree (Recconstr recs) = testList recs
    | sideEffectFree (Indirect{base, ...}) = sideEffectFree base
    | sideEffectFree (MutualDecs decs) = testList decs

        (* An RTS call, which may actually be code which is inlined
           by the code-generator, may be side-effect free.  This can
           occur if we have, for example, "if exp1 orelse exp2"
           where exp2 can be reduced to "true", typically because it's
           inside an inline function and some of the arguments to the
           function are constants.  This then gets converted to
           (exp1; true) and we can eliminate exp1 if it is simply
           a comparison. *)
    | sideEffectFree (Eval{function=Constnt w, argList, ...}) =
        isIoAddress(toAddress w) andalso sideEffectFreeRTSCall(w, argList)
        andalso List.all (fn (c, _) => sideEffectFree c) argList

    | sideEffectFree(Container _) = true
        (* But since SetContainer has a side-effect we'll always create the
           container even if it isn't used.  *)

    | sideEffectFree(TupleFromContainer(c, _)) = sideEffectFree c

    | sideEffectFree(IndirectVariable{base, ...}) =
            (* Offset is always side-effect free. *)
            sideEffectFree base

    | sideEffectFree(TupleVariable(vars, _ (* length - always side-effect free *))) =
        let
            fun testTuple(VarTupleSingle{source, ...}) = sideEffectFree source
            |   testTuple(VarTupleMultiple{base, ...}) = sideEffectFree base
        in
            List.all testTuple vars
        end

    | sideEffectFree _ = false
             (* Rest are unsafe (or too rare to be worth checking) *)

    and testList t = List.all sideEffectFree t

  and sideEffectFreeRTSCall(function: machineWord, args: (codetree * argumentType) list): bool =
    let
      fun isInList(ioCall, sofar) = sofar orelse wordEq (function, ioCall)
    in
      List.foldl isInList false safeRTSCalls orelse
        List.foldl isInList false safeForImmutable orelse
        (* Division operations are safe if we know that the second argument
           is not zero. If it's long it can't be zero and we can't have
           long arguments for the word operations. *)
        (List.foldl isInList false divisionOperations andalso
            (case args of
               [_, (Constnt c, _)] => not (isShort c) orelse toShort c <> 0w0
             | _ => false)
        )
    end;


    (* Tests whether an RTS call in which all the arguments are constants can
   be evaluated immediately.  Normally this will be clear from the RTS
   call itself but in a few cases we need to look at the arguments.
   It's quite safe to evaluate a function which results in an exception.
   It isn't safe to evaluate a function which might have a side-effect. *)

   fun earlyRtsCall(function: machineWord, args: (codetree * argumentType) list): bool =
   let
      fun isInList(ioCall, sofar) = sofar orelse wordEq (function, ioCall)
      fun isImmutable ((Constnt w, _), sofar) =
            sofar andalso (isShort w orelse not(isMutable(toAddress w)))
        | isImmutable _ = raise InternalError "isImmutable: arg not constant"
    in
      if List.foldl isInList false safeRTSCalls orelse
            List.foldl isInList false exceptionOperations
      then true
      else if List.foldl isInList false safeForImmutable
      then (* These are safe if the first argument is immutable.  If it's
              mutable we might find that the value has changed when we
              come to run the program. *)
         List.foldl isImmutable true args
      else false
    end

    (* Evaluates expressions by code-generating and running them.
       "resultCode" is a copied code expression. The result is either
       a constant or an exception. *)
    local
        fun evaluateDefault(resultCode, codegen, localCount) =
            let
                (* Compile the expression. *)
                val code = codegen(resultCode, localCount)
            in
                mkConst (code()) (* Evaluate it and convert any exceptions into Raise instrs. *)
                    handle Interrupt => raise Interrupt (* Must not handle this *)
                    | exn => Raise (Constnt(toMachineWord exn))
            end
    in
        fun evaluate (resultCode as Constnt _, _, _) =
            (* May already have been reduced to a constant. *)
            resultCode

        |   evaluate (resultCode as Eval { function=evalFunction, argList, resultType, ...}, codegen, localCount) =
            (* It's a function call - generate a call. This should only be
              as a result of "early" evaluation when all the arguments are
              constants or inline procedures. *)
            if List.all(fn (_, GeneralType) => true | _ => false) argList andalso
                (case resultType of GeneralType => true | _ => false)
            then
            (
                case evaluate(evalFunction, codegen, localCount) of
                    function as Raise _ => function (* Could be an exception. *)

                |   function =>
                    let (* Evaluate each argument. *)
                        val funcAddress =
                            case function of
                                Constnt addr =>
                                    if isShort addr
                                    then raise InternalError "Code address is not an address"
                                    else toAddress addr
                            |   _ => raise InternalError "Code address is not a constant";
       
                        (* Finished loading the args; call the function.  If it raises an
                           exception pass back the exception packet.  We've got a problem
                           here if the code happens to raise Interrupt.  We assume that
                           Interrupt can only occur through user intervention during
                           compilation rather than as a result of executing the code.
                           It would be better to use the Thread.Thread functions to
                           mask off interrupts. *)
                        fun callFunction (argTuple:machineWord) = 
                            mkConst (call (funcAddress, argTuple))
                                handle Interrupt => raise Interrupt (* Must not handle this *)
                                | exn as InternalError _ => raise exn
                                | exn => Raise (Constnt(toMachineWord exn))

                        fun loadArgs (argVec : address, [],  _) =
                            ( lock argVec; callFunction (toMachineWord argVec) )
               
                        |   loadArgs (argVec : address, (h, _) :: t, locn) =
                            case evaluate(h, codegen, localCount) of
                                arg as Raise _ => arg
                                (* if argument is a "raise" expression, so is final result *)
                            |   Constnt cv =>
                                ( 
                                    assignWord (argVec, toShort locn, cv);
                                    loadArgs(argVec, t, locn + 1)
                                )
                            |   _ => raise InternalError "Result of evaluate is not a constant"

                    in
                        case argList of
                            []      => callFunction word0  (* empty tuple - don't allocate *)
          
                        |   argList =>
                            let 
                                val argVec = alloc (toShort (List.length argList), F_mutable_words, word0);
                            in
                                loadArgs(argVec, argList, 0)
                            end
                    end
                )
                else evaluateDefault(resultCode, codegen, localCount)
        |   evaluate(resultCode, codegen, localCount) = evaluateDefault(resultCode, codegen, localCount)
    end
   
(************************************************************************)
(*    preCode                                                           *)
(************************************************************************)

  (* This phase generates closures, decides if a procedure can be called *)
  (* with a static link, and calculates use-counts for declarations. *)
(************************************************************************
The main point of this phase is to change the Loads to be closure-relative.
At the start of the phase, they are mostly of the form:

  Extract {level = n, addr = m, fpRel = true} (m <> 0)
  
which means: go out n (>= 0) levels of lambda binding, then get either
   (1) the m'th local (m > 0)
   (2) the ~m'th most recent parameter (m < 0)
   
with a few of the form:

  Extract {level = n, addr = 0, fpRel = false}

which means: load the n'th enclosing procedure (n = 0 means the current
procedure).
   
At the end of the phase, we have three possible forms:

  Extract {level = 0, addr = m, fpRel = true}  (m <> 0)
  Extract {level = 0, addr = 0, fpRel = false}
  
which are restricted forms of the above, and

  Extract {level = 0, addr = k, fpRel = false}
  
which means extract the k'th (k > 0) element of the procedure's closure.
The phase also constructs code to load the appropriate value into
the procedure's closure.

In addition to this, we want to work out when a procedure can be
virtual i.e. when we can call it via a static link. We can do this
if the procedure is never passed as a parameter, returned as a
result or used in the closure of a non static-link procedure.
The process starts by being optimistic, then marks each item as needing
a closure when a "difficult" use is encountered.

SPF 19/5/95
*************************************************************************)
(*
This previously used a use-counting mechanism to allow the code-generator
to decide when a value, particularly one in a register, is no longer required.
That the the disadvantage that it took no account of control flow so that
in a function such as
fun f (a, b) = if a then g a + 1 else b
b was marked as in use in the then-part and saved across the
call to g even though it is not actually required.
This has been changed to add information about when the last reference
to a variable occurs in any particular flow of control.  Extra "kill"
references are added to alternative flow of control, so for, example
the above function would be rewritten as something like
fun f (a, b) = if a then (b<last>; g (a<last>) + 1) else (a<last>; b<last>)
DCJM 2000.
*)
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

            fun makeDecl(Declar{addr, ...}) =
            (let
                val newAddr = ! localAddresses before (localAddresses := !localAddresses+1)
                val () = Array.update (closuresForLocals, addr, false)
                val () = Array.update (newLocalAddresses, addr, newAddr)
            in
                newAddr
            end handle Subscript => raise InternalError("makeDecl " ^ Int.toString addr ^ " of " ^ Int.toString localCount))
            |   makeDecl _ = raise InternalError "makeDecl: not Declar"
            
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
                    Eval {function = func, argList = newargs, earlyEval = false, resultType=resultType}
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
                    val newAddrs = List.map (fn (d, _) => makeDecl d) argList

                    (* Process the body. *)
                    val insBody = insert body
                    (* Finally the initial argument values. *)
                    local
                        fun copyDec((Declar{value, ...}, t), addr) =
                                (mkDecRef(insert value, addr, 0), t)
                        |   copyDec _ = raise InternalError "copyDec: not a declaration"
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

            |   insert(Newenv ptElist) =
                let
                    (* Process the body. Recurses down the list of declarations
                       and expressions processing each, and then reconstructs the
                       list on the way back. *)
                    fun copyDeclarations ([])   = []

                    |   copyDeclarations ((dec as Declar{addr=caddr, value = pt, ...}) :: vs)  =
                        let
                            val newAddr = makeDecl dec
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

                    |   copyDeclarations (MutualDecs mutualDecs :: vs)  =
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

                            val _ = List.map makeDecl mutualDecs

                            fun applyFn (Declar{addr=caddr, value=dv, ...}) =     
                                (
                                    case dv of
                                        Constnt _ => Array.update (localConsts, caddr, SOME dv) 
                                    |   _ => ()
                                )
                            |   applyFn _ = raise InternalError "applyFn: not a Declar"               
              
                            val () = List.app applyFn mutualDecs;
                  
                            (* Process the rest of the block. Identifies all other
                               references to these declarations. *)
                            val restOfBlock = copyDeclarations vs

                            (* We now want to find out which of the declarations require
                               closures. First we copy all the declarations, except that
                               we don't copy the non-local lists of procedures. *)
                            fun copyDec (Declar{addr=caddr, value=dv, ...}) = 
                                let
                                    val closure = ref (Array.sub(closuresForLocals, caddr))
                                    val dec     =
                                        case dv of
                                            Lambda lam => copyLambda lam closure
                                        |   _ => insert dv;
                                    (* Check whether we now have a constant *)
                                    val () =
                                        case dec of
                                            Constnt _ => Array.update (localConsts, caddr, SOME dec)
                                        |   _ => Array.update (localConsts, caddr, NONE); (* needed? *)

                                    (* copyLambda may set "closure" to true. *)
                                    val () = Array.update (closuresForLocals, caddr, !closure);
                                in
                                    mkDec (caddr, dec)
                                end
                            |   copyDec _ = raise InternalError "copyDec: not a Declar"               

                            val copiedDecs = map copyDec mutualDecs;
                   
                            (* We now have identified all possible references to the
                               procedures apart from those of the closures themselves.
                               Any of closures may refer to any other procedure so we must 
                               iterate until all the procedures which need full closures
                               have been processed. *)
                            fun processClosures([], outlist, true) =
                                (* Sweep completed. - Must repeat. *)
                                processClosures(outlist, [], false)
            
                            |   processClosures([], outlist, false) =
                                (* We have processed the whole of the list without finding
                                   anything which needs a closure. The remainder do not
                                   need full closures. *)
                                let
                                    fun mkLightClosure (Declar{value, addr, ...}) = 
                                        let
                                            val clos = copyProcClosure value false
                                            val newAddr = Array.sub(newLocalAddresses, addr)
                                        in
                                            mkDec (newAddr, clos)
                                        end
                                    | mkLightClosure _ = raise InternalError "mkLightClosure: not a Declar"               
                                in
                                    map mkLightClosure outlist
                                end
                  
                            |   processClosures((h as Declar{addr=caddr, value, ...})::t, outlist, someFound) =
                                if Array.sub(closuresForLocals, caddr)
                                then
                                let (* Must copy it. *)
                                    val clos = copyProcClosure value true
                                    val newAddr = Array.sub(newLocalAddresses, caddr)
                                in
                                    mkDec (newAddr, clos) :: processClosures(t, outlist, true)
                                end
                                    (* Leave it for the moment. *)
                                else processClosures(t, h :: outlist, someFound)

                            |   processClosures _ =
                                    raise InternalError "processClosures: not a Declar"               
                  
                            val decs = processClosures(copiedDecs, [], false)
                        in
                            (* Return the mutual declarations and the rest of the block. *)
                            case decs of
                                []   => restOfBlock         (* None left *)
                            |   [d]  => d :: restOfBlock    (* Just one *)
                            |   _    => mkMutualDecs decs :: restOfBlock
                        end (* copyDeclarations.isMutualDecs *)
          
                    |   copyDeclarations (v :: vs)  =
                        let (* Not a declaration - process this and the rest. *)
                           (* Must process later expressions before earlier
                               ones so that the last references to variables
                               are found correctly. DCJM 30/11/99. *)
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
                let
                    val processedFields = mapright insert recs
(*                    fun allConsts []       = true
                      | allConsts (Constnt _ :: t) = allConsts t
                      | allConsts _ = false*)
                    (* This does occasionally happen. *)
                in
                    mkTuple processedFields
                end

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
                    insert(mkEval(rtsFunction POLY_SYS_load_word, [base, offset], false))

            |   insert(TupleVariable(vars, length)) =
                (* Convert this into a set of RTS calls.  This currently uses POLY_SYS_alloc_store
                   which initialises the store before the copy operations.  It may be possible to
                   avoid this duplication somehow. *)
                let
                    val newAddr = ! localAddresses before (localAddresses := !localAddresses+1)
                    val mutableFlags = Word8.orb(F_words, F_mutable)
                    val allocTuple =
                        mkDec(newAddr,
                            mkEval(rtsFunction POLY_SYS_alloc_store,
                                [insert length, mkConst (toMachineWord mutableFlags), CodeZero], false)
                        )
                    fun copyTuple(VarTupleSingle{source, destOffset}) =
                            mkEval(rtsFunction POLY_SYS_assign_word,
                                [mkLoad(newAddr, 0), insert destOffset, insert source], false)
                    |   copyTuple(VarTupleMultiple{base, length, destOffset, sourceOffset}) =
                            mkEval(rtsFunction POLY_SYS_move_words,
                                [insert base, insert sourceOffset, mkLoad(newAddr, 0),
                                 insert destOffset, insert length], false)
                    (* Remove the mutable bit (needed by alloc_store). *)
                    val lock = mkEval(rtsFunction POLY_SYS_lockseg, [mkLoad(newAddr, 0)], false)
                 in
                    mkEnv(allocTuple :: (mapright copyTuple vars @ [lock, mkLoad(newAddr, 0)]))
                end

            |   insert(Declar _) = raise InternalError "insert:Declar"
            |   insert(MutualDecs _) = raise InternalError "insert:MutualDecs"
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
              datatype similarity = Different | Similar of loadForm;

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
                    val isArbitrary = wordEq (cv, ioOp POLY_SYS_equala)
                    val isWord = wordEq (cv, ioOp POLY_SYS_word_eq)
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
                                            mkEval(Constnt(ioOp POLY_SYS_equala),
                                                   [test, Constnt(toMachineWord t)], true)
                                    |   CaseWord =>
                                            mkEval(Constnt(ioOp POLY_SYS_word_eq),
                                                   [test, Constnt(toMachineWord t)], true)
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
                             name=lambdaName, resultType, localCount, ...}: lambdaForm) makeClosure =
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

    (* Look for static link functions and convert conditionals to cases then
       compute the lifetimes of declarations. *)
    fun preCode (codegen, pt, localAddressCount) =
    let
        fun pass1Cg(pt, localCount) = codegen(lifeTimes(pt, localCount), localCount)
        val pass1 = staticLinkAndCases(pass1Cg, pt, localAddressCount)
    in
        lifeTimes(pass1, localAddressCount)
    end

  (* Remove redundant declarations from the code.  This reduces
     the size of the data structure we retain for inline functions
     and speeds up compilation.  More importantly it removes internal
     functions which have been completely inlined.  These can mess up
     the optimisation which detects which parameters to a recursive
     function are unchanged.   It actually duplicates work that is
     done later in preCode but adding this function significantly
     reduced compilation time by reducing the amount of garbage
     created through inline expansion. DCJM 29/12/00. *)
  (* This also ensures that recursive references are converted into
     the correct CLOS(0,0) form. DCJM 23/1/01. *)
   fun cleanProc (pt: codetree, prev: loadForm * int * int -> codetree,
                  nestingDepth, locals): codetree =
   let
        fun cleanLambda(myAddr,
            {body, isInline, name, argTypes, resultType, level=nestingDepth, localCount, ...}) =
        let
            (* Start a new level. *)
            fun lookup(ext as {addr, fpRel, ...}, level, depth) =
                if level = 0
                then if addr = myAddr andalso fpRel
                then (* It's a recursive reference. *)
                        mkRecLoad(depth-nestingDepth)
                else 
                (
                    if addr >= 0 andalso fpRel
                    then Array.update(locals, addr, true)
                    else (); (* Recursive *)
                    Extract ext
                )
                else prev(ext, level-1, depth);

            val newLocals = Array.array (localCount (* Initial size. *), false);
            val bodyCode = cleanProc(body, lookup, nestingDepth, newLocals)
        in
            Lambda{body=bodyCode, isInline=isInline, name=name,
                   closure=[], argTypes=argTypes, resultType=resultType, level=nestingDepth,
                   closureRefs=0, makeClosure=false, localCount=localCount, argLifetimes = []}
        end

        and cleanCode (Newenv decs) =
            let
                fun cleanDec(myAddr, Lambda lam) = cleanLambda(myAddr, lam)
                |   cleanDec(_, d) = cleanCode d;

                (* Process the declarations in reverse order. *)
                fun processDecs [] = []
                 |  processDecs(Declar{value, addr, references} :: rest) =
                    let
                        (* Clear the entry.  I think it's possible that
                           addresses have been reused in other blocks
                           so do this just in case. *)
                        val _ = Array.update(locals, addr, false)
                        val processedRest = processDecs rest
                    in
                        (* If this is used or if it has side-effects we
                           must include it otherwise we can ignore it. *)
                        if Array.sub(locals, addr) orelse not (sideEffectFree value)
                        then Declar{value=cleanDec(addr, value), addr=addr,
                                    references=references} :: processedRest
                        else processedRest
                    end

                 |  processDecs(MutualDecs decs :: rest) =
                    let
                        (* Clear the entries just in case the addresses are reused. *)
                        fun setEntry(Declar{addr, ...}) = Array.update(locals, addr, false)
                          | setEntry _ = raise InternalError "setEntry: unknown instr"
                        val _ = List.app setEntry decs
                        val processedRest = processDecs rest

                        (* We now know the entries that have actually been used
                           in the rest of the code.  We need to include those
                           declarations and any that they use. *)
                        fun processMutuals [] excluded true =
                                (* If we have included a function in this
                                   pass we have to reprocess the list of
                                   those we excluded before. *)
                                processMutuals excluded [] false
                         |  processMutuals [] _ false =
                                (* We didn't add anything more - finish *) []
                         |  processMutuals(
                                (this as Declar{addr, value, references}) :: rest)
                                        excluded added =
                            if not (Array.sub(locals, addr))
                            then (* Put this on the excluded list. *)
                                processMutuals rest (this::excluded) added
                            else
                            let
                                (* Process this entry - it may cause other
                                   entries to become "used". *)
                                val newEntry =
                                    Declar{value=cleanDec(addr, value), addr=addr,
                                        references=references}
                            in
                                newEntry :: processMutuals rest excluded true
                            end
                          | processMutuals _ _ _ = 
                                raise InternalError "processMutual: unknown instr"
                        val processedDecs = processMutuals decs nil false
                    in
                        case processedDecs of
                            [] => processedRest (* None at all. *)
                        |   [oneDec] => oneDec :: processedRest
                        |   mutuals => MutualDecs mutuals :: processedRest
                    end
                 
                 |  processDecs(Newenv decs :: rest) = (* Expand out blocks. *)
                    let
                        val processedRest = processDecs rest
                        val processedDecs = processDecs decs
                    in
                        processedDecs @ processedRest
                    end

                 |  processDecs(exp :: rest) =
                    let
                        (* Either the result expression or part of an expression
                           being evaluated for its side-effects.  We can
                           eliminate it if it doesn't actually have a side-effect
                           except if it is the result.
                           Note: we have to process the rest of the list first
                           because the code for SetContainer checks whether the
                           container is used. *)
                        val processedRest = processDecs rest
                        val newExp = cleanCode exp
                    in
                        if sideEffectFree newExp andalso not(null processedRest)
                        then processedRest
                        else newExp :: processedRest
                    end

                val res = processDecs decs
            in
                (* We need a Newenv entry except for singleton expressions. *)
                wrapEnv res
            end (* Newenv *)

         |  cleanCode (dec as Extract(ext as {addr, level, fpRel, ...})) =
                (* If this is a local we need to mark it as used. *)
                if level = 0
                then
                    (
                    (* On this level. *)
                    if addr >= 0 andalso fpRel
                    then (* Local *) Array.update(locals, addr, true)
                    else (); (* Argument or recursive - ignore it. *)
                    dec
                    )
                else (* Non-local.  This may be a recursive call. *)
                    prev(ext, level-1, nestingDepth)

         |  cleanCode (Lambda lam) = cleanLambda(0, lam)

            (* All the other case simply map cleanCode over the tree. *)
         |  cleanCode MatchFail = MatchFail

         |  cleanCode (AltMatch(a, b)) = AltMatch(cleanCode a, cleanCode b)

         |  cleanCode (c as Constnt _) = c

         |  cleanCode (Indirect{base, offset}) =
                Indirect{base=cleanCode base, offset=offset}

         |  cleanCode (Eval{function, argList, earlyEval, resultType}) =
                Eval{function=cleanCode function, argList = map (fn (c, t) => (cleanCode c, t)) argList,
                     earlyEval=earlyEval, resultType=resultType}

         |  cleanCode(Cond(test, thenpt, elsept)) =
                Cond(cleanCode test, cleanCode thenpt, cleanCode elsept)

         |  cleanCode(BeginLoop{loop=body, arguments=argList, ...}) =
            let
                val processedBody = cleanCode body
                fun copyDec(Declar{addr, value, ...}, typ) =
                        (mkDec(addr, cleanCode value), typ)
                  | copyDec _ = raise InternalError "copyDec: not a declaration"
                val newargs = map copyDec argList
            in
                BeginLoop{loop=processedBody, arguments=newargs}
            end

         |  cleanCode(Loop args) = Loop(map (fn (c, t) => (cleanCode c, t)) args)

         |  cleanCode(Raise r) = Raise(cleanCode r)

         |  cleanCode(Ldexc) = Ldexc

         |  cleanCode(Handle{exp, handler}) =
                Handle{exp = cleanCode exp, handler = cleanCode handler}

         |  cleanCode(Recconstr decs) = Recconstr(map cleanCode decs)

         |  cleanCode(c as Container _) = c

         |  cleanCode(SetContainer {container, tuple, size}) =
               let
                (* If the container is unused we don't need to set it.
                   The container won't be created either. *)
                  val used =
                      case container of
                        Extract{addr, level=0, fpRel=true, ...} =>
                            addr <= 0 orelse Array.sub(locals, addr)
                      | _ => true (* Assume it is. *)
               in
                (* Disable this for the moment - it's probably not very useful
                   anyway.  It doesn't work at the moment because we sometimes
                   make a local declaration point at another variable (in
                   pushContainer and replaceContainerDec).  The
                   new variable may be set but not used while the old variable
                   is used but not set.  *)
                if not used andalso false
                then CodeZero (* Return something. *)
                else
                (* Push the container down the tree and then process it. If we've
                   expanded an inline function we want to be able to find any
                   tuple we're creating. *)
                case tuple of
                    Cond _ => cleanCode(mkSetContainer(container, tuple, size))
                |   Newenv _ => cleanCode(mkSetContainer(container, tuple, size))
                |   r as Raise _ =>
                        (* If we're raising an exception we don't need to set the container. *)
                        cleanCode r
                |   _ => SetContainer{container = cleanCode container,
                            tuple = cleanCode tuple, size = size}
               end

         |  cleanCode(TupleFromContainer(container, size)) =
               TupleFromContainer(cleanCode container, size)

         |  cleanCode CodeNil = CodeNil

         |  cleanCode (TagTest{test, tag, maxTag}) =
                TagTest{test=cleanCode test, tag=tag, maxTag=maxTag}

         |  cleanCode(IndirectVariable{base, offset}) =
                IndirectVariable{base=cleanCode base, offset=cleanCode offset}

         |  cleanCode(TupleVariable(vars, length)) =
            let
                fun cleanTuple(VarTupleSingle{source, destOffset}) =
                        VarTupleSingle{source=cleanCode source, destOffset=cleanCode destOffset}
                |   cleanTuple(VarTupleMultiple{base, length, destOffset, sourceOffset}) =
                        VarTupleMultiple{base=cleanCode base, length=cleanCode length,
                                         destOffset=cleanCode destOffset, sourceOffset=cleanCode sourceOffset}
            in
                TupleVariable(map cleanTuple vars, cleanCode length)
            end

         |  cleanCode (Declar _) = raise InternalError "cleanCode: Declar"

         |  cleanCode (MutualDecs _) = raise InternalError "cleanCode: MutualDecs"

         |  cleanCode (Case _) = raise InternalError "cleanCode: Case"

         |  cleanCode (Global _) = raise InternalError "cleanCode: Global"
         
         |  cleanCode (KillItems _) = raise InternalError "cleanCode: StaticLinkCall"
   in
        cleanCode pt
   end (* cleanProc *);



(************************************************************************)

  val initTrans = 10; (* Initial size of arrays. *)

(*****************************************************************************)
(*                  changeLevel                                              *)
(*****************************************************************************)
  (* Change the level of an entry if necessary. This *)
  (* happens if we have a function inside an inline function. *)
  fun changeLevel entry 0 = entry (* No change needed*)
   | changeLevel entry correction =
      let
      fun changeL(ext as Extract{addr, level, fpRel, ...}, nesting) =
            if level >= 0 andalso level < nesting
                (* We enter declarations with level = ~1 for recursive
                   calls when processing mutual recursion. *)
            then ext (* It's local to the function(s) we're processing. *)
            else mkGenLoad (addr, level + correction, fpRel, false)

        | changeL(Lambda{body, isInline, name, closure, argTypes, resultType,
                         level, closureRefs, makeClosure, localCount, ...}, nesting) =
            Lambda{body = changeL(body, nesting+1), isInline = isInline,
                   name = name, closure = closure, argTypes = argTypes, resultType=resultType,
                   level = level + correction, closureRefs = closureRefs,
                   makeClosure = makeClosure, localCount=localCount, argLifetimes = []}

        | changeL(Eval{function, argList, earlyEval, resultType}, nesting) =
            Eval{function = changeL(function, nesting),
                 argList = map (fn (a, t) => (changeL(a, nesting), t)) argList,
                 earlyEval = earlyEval, resultType=resultType}

        | changeL(Indirect{ base, offset }, nesting) =
            Indirect{base = changeL(base, nesting), offset = offset }

        | changeL(Declar{value, addr, ...}, nesting) =
            mkDec(addr, changeL(value, nesting))

        | changeL(Newenv l, nesting) =
            Newenv(map(fn d => changeL(d, nesting)) l)

        | changeL(c as Container _, _) = c

        | changeL(TupleFromContainer(container, size), nesting) =
            TupleFromContainer(changeL(container, nesting), size)

        | changeL(_, _) =
            (* The code we produce in these inline functions is very limited. *)
               raise InternalError "changeL: Unknown code"

    in
        case optGeneral entry of
            gen as Extract _ =>
                optVal 
                {
                    general = changeL(gen, 0),
                    special = optSpecial entry,
                    environ = optEnviron entry,
                    decs    = [],
                    recCall = optRec entry
                }

        |   Constnt _ => entry

        |   gen as Lambda _ =>
                optVal
                {
                    general = changeL(gen, 0),
                    special = optSpecial entry,
                    environ = optEnviron entry,
                    decs    = [],
                    recCall = optRec entry
                }
            
        | _ => raise InternalError "changeLevel: Unknown entry"
    end
  (* end changeLevel *)

(*****************************************************************************)
(*                  optimiseProc                                             *)
(*****************************************************************************)
  fun optimiseProc
    {pt : codetree,
     lookupNewAddr : loadForm * int * int -> optVal,
     lookupOldAddr : loadForm * int * int -> optVal,
     enterDec : int * optVal -> unit,
     enterNewDec : int * optVal -> unit,
     nestingOfThisProcedure : int,
     spval : int ref,
     earlyInline : bool,
     evaluate : codetree * int -> codetree,
     tailCallEntry: bool ref option,
     recursiveExpansions:
        (((codetree * argumentType) list * bool * int -> (codetree * argumentType) list) * bool ref) list,
     maxInlineSize: int} =
  (* earlyInline is true if we are expanding a procedure declared 
     "early inline". *)
  (* spval is the Declaration counter. Normally ref(1) except when expanding
     an inline procedure. *)
  (* tailCallEntry is NONE if this is not an inline function and SOME r if
     it is.  r is set to true if a tail recursive LOOP instruction is generated. *)
  let
(*****************************************************************************)
(*                  newDecl (inside optimiseProc)                            *)
(*****************************************************************************)
    (* Puts a new declaration into a table. Used for both local declarations
       and also parameters to inline procedures. "setTab" is the table to
       put the entry in and "pt" is the value to be put in the table. We try
       to optimise various cases, such as constants, where a value is declared 
       but where it is more efficient when it is used to return the value
       itself rather than an instruction to load the value. *)
    fun stripDecs (ov : optVal) : optVal =
      case optDecs ov of 
        [] => ov
      | _  =>
        optVal 
          {
             general = optGeneral ov,
             special = optSpecial ov,
             environ = optEnviron ov,
             decs    = [],
             recCall = optRec ov
          };
       
    fun newDecl (setTab, ins, addrs, pushProc) : codetree list =
    let
        val gen  = optGeneral ins;
    in
        case gen of
            Constnt _ => 
            let (* No need to generate code. *)
                val spec = optSpecial ins
                val ov = 
                    (* If it is a non-inline procedure it must be declared. *)
                    case (spec, pushProc) of
                        (Lambda{isInline=NonInline, ...}, true) => simpleOptVal gen
                    |   _ => stripDecs ins  (* Remove the declarations before entering it. *)
                val () = setTab (addrs, ov)
            in
                (* Just return the declarations. *)
                optDecs ins 
            end

        |   Extract { level = 0, ...} =>
            let
                (* Declaration is simply giving a new name to a local
                   - can ignore this declaration. *) 
                val optVal = stripDecs ins (* Simply copy the entry.  *)
                val () = setTab (addrs, optVal)
            in
                optDecs ins
            end

            (* This next case was commented out by AHL.  It could probably be reinstated. *)
(*        |   Indirect {base, offset} =>
            let
                (* It is safe to defer an indirection if we can. For instance,
                   in ML fun f (a as (b,c)) will generate declarations of b and c
                   as indirections on a. If b and c are not used immediately there
                   is no point in loading them  (it only uses up the registers).
                   Once they are actually used they will be loaded into registers
                   and those registers will be cached by the normal register caching
                   scheme, so that if used again soon after they will not be
                   reloaded. *)
                fun optSetTab (i, v) =
                    setTab
                    (i,
                    optVal 
                    { (* Add on the indirection. *)
                        general = mkInd (offset, optGeneral v),
                        special = optSpecial v,
                        environ = optEnviron v,
                        decs    = optDecs v,
                        recCall = optRec v
                    })
            in
                (* Take off the indirection from the value to be declared and add
                   it to the load instruction. This causes the indirection to be
                   deferred until the value is actually used. *)
                newDecl(optSetTab,
                    optVal 
                    {
                        general = base,
                        special = optSpecial ins,
                        environ = optEnviron ins,
                        decs    = optDecs ins,
                        recCall = optRec ins
                    }, addrs, pushProc)
            end*)

        |   _ =>
            let (* Declare an identifier to have this value. *)
                val decSpval = ! spval; 
                val ()       = spval := decSpval + 1 ;
            
                (* The table entry is similar to the result of the expression except
                    that the declarations are taken off and put into the containing
                    block, and the general value is put into a local variable and
                    replaced by an instruction to load from there. If the special
                    is a non-inline procedure it is removed. Non-inline procedures
                    are returned by copyLambda so that they can be inserted inline
                    if they are immediately called (e.g. a catch phrase) but if they
                    are declared they are created as normal procedures. We don't do
                    this for parameters to inline procedures so that lambda-expressions
                    passed to inline procedures will be expanded inline if they are
                    only called inside the inline procedure.
                    e.g. for(..., proc(..)(...)) will be expanded inline. *)
                val spec = optSpecial ins;
                val optSpec =
                    case (spec, pushProc) of
                        (Lambda{isInline=NonInline, ...}, true) => CodeNil
                    |   _ => spec
              
                val optV = 
                    optVal 
                    {
                        general = mkLoad (decSpval, 0),
                        special = optSpec,
                        environ = optEnviron ins,
                        decs    = [],
                        recCall = optRec ins
                    }
            
                val () = setTab (addrs, optV)
            in
                optDecs ins @ [mkDecRef(gen, decSpval, 0)]
            end
        end (* newDecl *)

    (* Handle an Indirect node or the equivalent in a variable tuple. *)
    fun doIndirection(source, offset) =
        case (optSpecial source, optGeneral source) of
          (spec as Recconstr _, _) =>
            let
                (* The "special" entry we've found is a record.  That means that
                   we are taking a field from a record we made earlier and so we
                   should be able to get the original code we used when we made
                   the record.  That might mean the record is never used and
                   we can optimise away the construction of it completely. The
                   entry we get back from findEntryInBlock will either be a
                   constant or a load.  In that case we need to look it up in
                   the environment we used for the record to give us an optVal.
                   The previous code in newDecl, commented out by AHL, also put
                   indirections into the table.  To save having the various cases
                   in here we simply call optimiseProc which will deal with them.
                   DCJM 9/1/01. *)
                val specEntry = findEntryInBlock spec offset;
                val newCode =
                 optimiseProc 
                   {pt=specEntry,
                    lookupNewAddr=errorEnv, (* We must always look up old addresses. *)
                    lookupOldAddr=optEnviron source,
                    enterDec=enterDec, (* should not be used *)
                    enterNewDec=enterNewDec, (* should not be used *)
                    nestingOfThisProcedure=nestingOfThisProcedure,
                    spval=spval,
                    earlyInline=earlyInline,
                    evaluate=evaluate,
                    tailCallEntry=NONE,
                    recursiveExpansions=recursiveExpansions,
                    maxInlineSize=maxInlineSize}
            in
              optVal 
                {
                  general = optGeneral newCode,
                  special = optSpecial newCode,
                  environ = optEnviron newCode,
                  decs    = optDecs source @ optDecs newCode,
                  recCall = optRec newCode
                }
            end

        | (_ , gen as Constnt _ ) => (* General is a constant -  Do the selection now. *)
            optVal 
              {
                general = findEntryInBlock gen offset,
                special = CodeNil,
                environ = errorEnv,
                decs    = optDecs source,
                recCall = ref false
              }
                           
        | (_, _) => (* No special case possible. *)
              optVal 
            {
              general = mkInd (offset, optGeneral source),
              special = CodeNil,
              environ = errorEnv,
              decs    = optDecs source,
              recCall = ref false
            }

(*****************************************************************************)
(*                  optimise (inside optimiseProc)                           *)
(*****************************************************************************)
    fun getGeneral ov =
        (
        case optDecs ov of
          []   => optGeneral ov
        | decs => mkEnv (decs @ [optGeneral ov])
        )

    (* The main optimisation routine. *)
    (* Returns only the general value from an expression. *)
    fun generalOptimise(pt, tailCall) = getGeneral(optimise(pt, tailCall))

    and general pt = generalOptimise(pt, NONE)

    and optimise (pt as MatchFail, _) = simpleOptVal pt

     |  optimise (AltMatch(a, b), _) =
        simpleOptVal(AltMatch(general a, general b))
    
     |  optimise (CodeNil, _) = simpleOptVal CodeNil
        
     |  optimise (Eval{function, argList, earlyEval, resultType}, tailCall) =
        let
          (* Get the function to be called and see if it is inline or
             a lambda expression. *)
          val funct : optVal = optimise(function, NONE);
          val foptRec = optRec funct
               
          (* There are essentially two cases to consider - the procedure
             may be "inline" in which case it must be expanded as a block,
             or it must be called. *)
          fun notInlineCall(recCall:( ((codetree * argumentType) list * bool * int -> (codetree * argumentType) list) * bool ref)option) = 
            let
            val argsAreConstants = ref true;

            fun copyArg(arg, argType) =
              let
                 val copied = general arg;
              in
                (* Check for early evaluation. *)
                if not (isConstnt copied) then argsAreConstants := false else ();
                (copied, argType)
             end
   
            val copiedArgs = map copyArg argList;
            val gen = optGeneral funct
            and early = earlyEval orelse earlyInline

            (* If the procedure was declared as early or is inside an inline
               procedure declared as early we can try to evaluate it now.
               Also if it is a call to an RTS function (which may actually
               be code-generated inline by G_CODE) we can evaluate it if
               it's safe. *)
            val evalEarly =
                ! argsAreConstants andalso isConstnt (optGeneral funct) andalso
                (early orelse
                 (case optGeneral funct of
                    Constnt w =>
                        isIoAddress(toAddress w) andalso earlyRtsCall(w, copiedArgs)
                  | _ => false
                 )
                )
            
            val evCopiedCode = 
              if evalEarly
              then evaluate (Eval {function = gen, argList = copiedArgs, earlyEval = early, resultType=resultType}, !spval+1)
              else case recCall of
                (* This is a recursive call to a function we're expanding.
                   Is it tail recursive?  We may have several levels of
                   expansion. *)
                SOME (filterArgs, optr) =>
                    if (case tailCall of
                            SOME tCall => optr = tCall (* same reference? *)
                        | NONE => false)
                    then Loop (filterArgs(copiedArgs, true, nestingOfThisProcedure))
                    else Eval {function = gen,
                            argList = filterArgs(copiedArgs, false, nestingOfThisProcedure),
                            earlyEval = early, resultType=resultType}
                 (* Not a recursive expansion. *)
               | NONE => Eval {function = gen, argList = copiedArgs, earlyEval = early, resultType=resultType}
         in
            optVal 
              {
                general = evCopiedCode,
                special = CodeNil,
                environ = errorEnv,
                decs    = optDecs funct,
                recCall = ref false
              }
          end (* notInlineCall *)

        in
          case (List.find (fn (_, r) => r = foptRec) recursiveExpansions, optSpecial funct) of
             (recCall as SOME _, _) =>
                 (* We're already expanding this function - don't recursively expand
                    it.  Either loop or generate a function call. *)
                notInlineCall recCall
         | (_,
            Lambda { isInline, body=lambdaBody, name=lambdaName, argTypes, ...}) =>
            let
           (* Calling inline proc or a lambda expression which is just called.
              The procedure is replaced with a block containing declarations
              of the parameters.  We need a new table here because the addresses
              we use to index it are the addresses which are local to the function.
              New addresses are created in the range of the surrounding function. *)
            val localVec = stretchArray (initTrans, NONE);
            val paramVec = stretchArray (initTrans, NONE);
            val localNewVec = stretchArray (initTrans, NONE);
            
                (* copies the argument list. *)
            fun copy []     _          = [] : codetree list
              | copy ((h, _)::t) argAddress =
              let
			    fun setTab (index, v) = update (paramVec, ~index, SOME v);
                (* Make the declaration, picking out constants, inline
                   procedures and load-and-stores. These are entered in the
                   table, but nil is returned by "newDecl". *)
                val lapt = newDecl (setTab, optimise(h, NONE), argAddress, false);
              in (* Now process the rest of the declarations. *)
                lapt @ copy t (argAddress + 1)
              end (* end copy *)

            val nArgs = List.length argList
            val copiedArgs = copy argList (~nArgs);
            (* Create an immutable vector from the parameter array to reduce the
               amount of mutable data, *)
            val frozenParams = StretchArray.vector paramVec

            (* All declarations should be of positive addresses. *)
            fun setNewTabForInline (addr, v) = update (localNewVec, addr, SOME v)

            fun setTabForInline (index, v) =
                (
                    case optGeneral v of
                        Extract{addr, ...} =>
                            if addr <= 0 then ()
                            else setNewTabForInline(addr, v)
                    |   _ => ();
                    update (localVec, index, SOME v)
                )

            fun lookupLocalNewAddr (ext as { addr, ...}, depth, levels) =
                    (* It may be local to this function or to the surrounding scope. *)
                if levels <> 0 orelse addr <= 0
                then lookupNewAddr(ext, depth, levels)
                else case localNewVec sub addr of
                        SOME v => changeLevel v (depth - nestingOfThisProcedure)
                    |   NONE => lookupNewAddr(ext, depth, levels);

            val copiedBody =
                if isInline = MaybeInline orelse isInline = OnlyInline
                then(* It's a function the front-end has explicitly inlined.
                       It can't be directly recursive.  If it turns out to
                       be indirectly recursive (i.e. it calls a function which
                       then calls it recursively) that's fine - the recursive
                       expansion will be stopped by the other function. *)
                let            
                    (* The environment for the expansion of this procedure
                       is the table for local declarations and the original
                       environment in which the function was declared for
                       non-locals. *)
                    fun lookupDec ({ addr=0, ...}, _, 0) =
                       (* Recursive reference - shouldn't happen. *)
                       raise InternalError ("lookupDec: Inline function recurses " ^ lambdaName)
                    |   lookupDec ({ addr=index, level, ...}, depth, 0) =
                         let (* On this level. *)
                            val optVal =
                                if index > 0
                                then localVec sub index (* locals *)
                                else Vector.sub(frozenParams, ~index) (* parameters *)
                            val value =
                                case optVal of
                                    SOME value => value
                                |   NONE => raise InternalError(
                                                    concat["Missing value address=", Int.toString index,
                                                           " level= ", Int.toString level, " while expanding ", lambdaName])
                         in
                           changeLevel value (depth - nestingOfThisProcedure)
                         end
                    |  lookupDec (ptr, depth, levels) =
                         (optEnviron funct) (ptr, depth, levels - 1);

                 in
                     optimiseProc 
                      {pt=lambdaBody,
                       lookupNewAddr=lookupLocalNewAddr,
                       lookupOldAddr=lookupDec, 
                       enterDec=setTabForInline,
                       enterNewDec=setNewTabForInline,
                       nestingOfThisProcedure=nestingOfThisProcedure,
                       spval=spval,
                       earlyInline=earlyInline orelse earlyEval,
                       evaluate=evaluate,
                       tailCallEntry=tailCall,
                       recursiveExpansions=recursiveExpansions,
                       maxInlineSize=maxInlineSize}
                  end

                else (* It's a "small" function. *)
                let
                (* Now load the procedure body itself.  We first process it assuming
                   that we won't need to treat any of the arguments specially.  If
                   we find that we generate a Loop instruction somewhere we have
                   to make sure that any arguments we change in the course of the
                   loop are taken out.  For example:
                   fun count'(n, []) = n | count' (n, _::t) = count'(n+1, t);
                   fun count l = count'(0, l).
                   In this case we would start by expanding count' using 0 for n
                   throughout, since it's a constant.  When we find the recursive
                   call in which n becomes n+1 we find we have to take n out of the
                   loop and treat it as a variable.
                   We don't need to do this if the argument is passed through unchanged
                   e.g. fun foldl f b [] = b  | foldl f b (x::y) = foldl f (f(x, b)) y;
                   where the same value for f is used everywhere and by treating it
                   specially we can expand its call. 
                   This two-pass (it will normally be two-pass but could be more) approach
                   allows us to optimise cases where we have a recursive function which
                   happens to be non-recursive with particular constant values of the
                   arguments.
                   e.g. if x = nil ... generates a general recursive function for
                   equality on lists but because of the nil argument this optimises
                   down to a simple test. *)
                (*
                   I'm now extending this to the general recursive case not just tail
                   recursion.  If we discover a recursive call while processing the
                   function we turn this expansion into a function call and give up
                   trying to inline it.  Instead we create a special-purpose function
                   for this call but with only the arguments that change as a
                   result of the recursive calls actually passed as arguments.  Other
                   arguments can be inserted inline in function.
                   e.g. fun map f [] = [] | map f (a::b) = f a :: map f b
                   where when we map a function over a list we compile a specialised
                   mapping function with the actual value of f inserted in it.
                *)
                    val needsBeginLoop = ref false
                    val needsRecursiveCall = ref false
                    val argModificationVec = Array.array(nArgs, false);
                    (* Create addresses for the new variables for modified arguments.
                       If newdecl created a variable we might be able to reuse that
                       but it's easier to create new ones. *)
                    val argBaseAddr = ! spval; val _ = spval := argBaseAddr + nArgs
    
                    (* filterArgs is called whenever a recursive call is made to
                       this function. *)
                    fun filterArgs (argList: (codetree * argumentType) list, isTail, depth) =
                    let
                        fun filterArgs' 0 [] = []
                          | filterArgs' _ [] =
                                raise InternalError "filterArgs': wrong number of args"
                          | filterArgs' n ((arg, argType) :: rest) =
                            let
                                (* Is this simply passing the original argument value?  *)
                                val original = valOf (Vector.sub(frozenParams, n))
                                val unChanged =
                                    case (arg, optGeneral original) of
                                        (Constnt w, Constnt w') =>
                                            (* These may well be functions so don't use
                                               structure equality. *)
                                            wordEq(w, w')
                                    | (Extract {addr=aA, level=aL, fpRel=aFp, ...},
                                       Extract {addr=bA, level=bL, fpRel=bFp, ...}) =>
                                        aA = bA andalso aFp = bFp andalso
                                        aL = bL+depth-nestingOfThisProcedure
                                    | _ => false

                            in
                                if unChanged
                                then ()
                                else Array.update(argModificationVec, n-1, true);
                                (* If any recursive call has changed it we need
                                   to include this argument even if it didn't
                                   change on this particular call. *)
                                if Array.sub(argModificationVec, n-1)
                                then (arg, argType) :: filterArgs' (n-1) rest
                                else (* Not modified *) filterArgs' (n-1) rest
                            end
                    in
                        needsBeginLoop := true; (* Indicate we generated a Loop instr. *)
                        (* If this isn't tail recursion we need a full call. *)
                        if isTail then () else needsRecursiveCall := true;
                        (* If we have a recursive inline function containing a
                           local recursive inline function which calls the outer
                           function
                           (e.g. fun f a b = .... map (f a) ....) we may process
                           the body of the inner function twice, once as a lambda
                           and once when we attempt to expand it inline.  That
                           means we will process the recursive call to the outer
                           function twice.  The first call may filter out redundant
                           arguments (e.g. "a" in the above example).  *)
                        if List.length argList <> nArgs
                        then argList
                        else filterArgs' nArgs argList
                    end

                    (* See how many arguments changed. *)
                    fun countSet() =
                    let
                        fun countSet' n 0 = n
                          | countSet' n i =
                                if Array.sub(argModificationVec, i-1) then countSet' (n+1) (i-1)
                                else countSet' n (i-1)
                    in
                        countSet' 0 nArgs
                    end

                    fun checkRecursiveCalls lastModCount =
                    (* We've found at least one non-tail recursive call so we're
                       going to have to generate this function as a function and
                       a call to that function.  However we may well gain by inserting
                       in line arguments which don't change as a result of recursion. *)
                    let
                        val nesting = nestingOfThisProcedure + 1
                        (* Find the parameter we're actually going to use. *)
                        fun getParamNo n =
                            if n = 0 then 0
                            else if Array.sub(argModificationVec, ~n -1)
                            then getParamNo (n+1) - 1
                            else getParamNo (n+1)

                        fun prev ({ addr=0, ...}, depth, 0) =
                             (* Recursive reference.  We're going to generate this
                                as a function so return a reference to the closure.
                                I've ensured that we pass the appropriate value for
                                recCall here although I don't know if it's necessary. *)
                             optVal {
                                general = mkGenLoad (0, depth - nesting, false, false),
                                (* This is a bit of a mess.  We need a non-nil value for
                                   special here in order to pass recCall correctly
                                   because optVal filters it otherwise. *)
                                special = (*optSpecial funct *)
                                    mkGenLoad (0, depth - nesting, false, false),
                                decs = [],
                                recCall = foptRec,
                                environ = errorEnv
                                }
                        |   prev ({ addr=index, ...}, depth, 0) =
                             if index > 0  (* locals *)
                             then changeLevel(valOf (localVec sub index)) (depth - nesting)
                             else (* index < 0 - parameters *)
                                if Array.sub(argModificationVec, ~index -1)
                             then (* This argument has changed - find the corresponding
                                     actual argument. *)
                                simpleOptVal(mkLoad(getParamNo index, depth-nesting))
                             else (* Unchanged - get the entry from the table, converting
                                     the level because it's in the surrounding scope. *)
                                changeLevel (valOf (Vector.sub(frozenParams, ~index))) (depth-nesting+1)
                        |  prev (ptr, depth, levels) =
                                (optEnviron funct) (ptr, depth, levels - 1);

                        val newAddrTab = stretchArray (initTrans, NONE);

                        (* localNewAddr is used as the environment of inline functions within
                           the function we're processing. *)
                        fun localNewAddr ({ addr=index, ...}, depth, 0) =
                          if index > 0 
                          then case newAddrTab sub index of
                                NONE => (* Return the original entry if it's not there. *)
                                    simpleOptVal(
                                        mkGenLoad (index, depth - nesting, true, false))
                            |   SOME v => changeLevel v (depth - nesting) 
                          else simpleOptVal (mkGenLoad (index, depth - nesting, index <> 0, false))
                        | localNewAddr (ptr, depth, levels) =
                            lookupNewAddr (ptr, depth, levels - 1);
            
                        fun setNewTab (addr, v) = update (newAddrTab, addr, SOME v)

                        fun setTab (index, v) =
                          (
                          case optGeneral v of
                              Extract{addr, ...} =>
                                if addr <= 0 then () else setNewTab(addr, v)
                            |   _ => ();
                          update (localVec, index, SOME v)
                          )

                        val newAddressAllocator = ref 1 (* Starts at one. *)

                        val copiedBody =
                             optimiseProc 
                              {pt=lambdaBody,
                               lookupNewAddr=localNewAddr,
                               lookupOldAddr=prev, 
                               enterDec=setTab,
                               enterNewDec=setNewTab,
                               nestingOfThisProcedure=nesting,
                               spval=newAddressAllocator,
                               earlyInline=earlyInline orelse earlyEval,
                               evaluate=evaluate,
                               tailCallEntry=NONE, (* Don't generate loop instructions. *)
                               recursiveExpansions=(filterArgs, foptRec) :: recursiveExpansions,
                               maxInlineSize=maxInlineSize}

                        val newModCount = countSet()
                    in
                        if newModCount > lastModCount
                        then (* We have some (more) arguments to include. *)
                            checkRecursiveCalls newModCount
                        else
                        let
                            fun filterArgTypes([], _) = []
                            |   filterArgTypes(hd:: tl, n) =
                                    if Array.sub(argModificationVec, n-1)
                                    then hd :: filterArgTypes(tl, n-1)
                                    else filterArgTypes(tl, n-1)
                            val newArgList = filterArgTypes(argTypes, nArgs)
                        in
                            optVal {
                                general = Lambda {
                                    body = getGeneral copiedBody,
                                    isInline = NonInline,
                                    name = lambdaName,
                                    closure = [],
                                    argTypes = newArgList, resultType=resultType,
                                    level = nestingOfThisProcedure + 1,
                                    closureRefs = 0,
                                    localCount = ! newAddressAllocator + 1,
                                    makeClosure = false, argLifetimes = [] },
                                special = CodeNil,
                                decs = [],
                                recCall = ref false,
                                environ = localNewAddr
                            }
                        end
                    end

                    fun checkForLoopInstrs lastModCount =
                    (* Called initially or while we only have tail recursive
                       calls.  We can inline the function. *)
                    let
                        (* There's still something strange going on where we end up with
                           a BeginLoop that doesn't contain any Loop.  See regression
                           test Succeed/Test108.ML. *)
                        val _ = needsBeginLoop := false;
                        fun prev ({ addr=index, ...}, depth, 0) : optVal =
                         let (* On this level. *)
                           val optVal =
                             if index = 0
                             (* Recursive reference - return the copied function after removing
                                the declarations.  These will be put on in the surrounding
                                scope.  We can't at this stage tell whether it's a call or
                                some other recursive use (e.g. passing it as an argument) and
                                it could be that it's an argument to an inline function which
                                actually calls it.  Since we include the original optRec value
                                it can be sorted out later. *)
                             then stripDecs funct
                             else if index > 0
                             then valOf (localVec sub index)  (* locals *)
                             else (* index < 0 *) if Array.sub(argModificationVec, ~index-1)
                             then (* This argument changes - must use a variable even if
                                     the original argument was a constant. *)
                                simpleOptVal(mkLoad(argBaseAddr + nArgs + index, 0))
                             else valOf (Vector.sub(frozenParams, ~index)) (* parameters *)
                         in
                           changeLevel optVal (depth - nestingOfThisProcedure)
                         end
                        | prev (ptr, depth, levels) : optVal =
                            (* On another level. *)
                            (optEnviron funct) (ptr, depth, levels - 1);

                        val copiedBody =
                             optimiseProc 
                              {pt=lambdaBody,
                               lookupNewAddr=lookupLocalNewAddr,
                               lookupOldAddr=prev, 
                               enterDec=setTabForInline,
                               enterNewDec=setNewTabForInline,
                               nestingOfThisProcedure=nestingOfThisProcedure,
                               spval=spval,
                               earlyInline=earlyInline orelse earlyEval,
                               evaluate=evaluate,
                               tailCallEntry=SOME foptRec,
                               recursiveExpansions=(filterArgs, foptRec) :: recursiveExpansions,
                               maxInlineSize=maxInlineSize}
                    in
                        if ! needsRecursiveCall
                        then (* We need a fully recursive call. *)
                            checkRecursiveCalls (countSet())
                        else if ! needsBeginLoop 
                        then (* We've found at least one recursive call which changes its
                                argument value. *)
                        let
                            val newModCount = countSet()
                        in
                            if newModCount > lastModCount
                            then checkForLoopInstrs newModCount
                            else copiedBody
                        end
                        else copiedBody
                    end
        
                    val procBody = checkForLoopInstrs 0

                    (* If we need to make the declarations put them in at the
                       beginning of the loop. *)
                    fun makeDecs(0, [], _) = []
                      | makeDecs(n, (_, typ):: rest, isCall) =
                            if not (Array.sub(argModificationVec, n-1))
                            then makeDecs (n-1, rest, isCall)
                            else
                            let
                                val argVal = getGeneral(valOf (Vector.sub(frozenParams, n)))
                                val argDec =
                                (* If we are calling a function we just put the
                                   argument values in. *)
                                    if isCall
                                    then argVal
                                    else mkDec(argBaseAddr+nArgs-n, argVal)
                            in
                                (argDec, typ) :: makeDecs (n-1, rest, isCall)
                            end
                     |  makeDecs _ = raise InternalError "Unequal lengths"
                    fun mkDecs isCall = makeDecs(nArgs, argList, isCall)
                in
                    if ! needsRecursiveCall
                    then (* We need to put in a call to this function. *)
                        let
                            (* Put the function into the declarations. *)
                            val addr = ! spval
                        in
                            spval := addr + 1;
                            optVal{
                                general =
                                    Eval {function = mkLoad(addr, 0), argList = mkDecs true,
                                          earlyEval = false, resultType=resultType},
                                special = CodeNil,
                                decs = [mkDec(addr, getGeneral procBody)],
                                recCall = ref false,
                                environ = lookupNewAddr
                            }
                        end
                    else if ! needsBeginLoop
                    then simpleOptVal(BeginLoop{loop=getGeneral procBody, arguments=mkDecs false})
                    else if Array.exists(fn x => x) argModificationVec
                    then (* We could have reset needsBeginLoop to false and ended up not needing a loop.
                            Ifwe have declarations that we made earlier we have to include them. *)
                        optVal{general = optGeneral procBody, special = optSpecial procBody,
                               environ = optEnviron procBody, recCall = optRec procBody,
                               decs = List.map #1 (mkDecs false) @ optDecs procBody}
                    else procBody
                end           
          in
            StretchArray.freeze localVec;
            StretchArray.freeze localNewVec;
           (* The result is the result of the body of the inline procedure. *)
           (* The declarations needed for the inline procedure, the         *)
           (* declarations used to load the arguments and the declarations  *)
           (* in the expanded procedure are all concatenated together. We   *)
           (* do not attempt to evaluate "early inline" procedures. Instead *)
           (* we try to ensure that all procedures inside are evaluated     *)
           (*"early". *)
          optVal 
          {
            general = optGeneral copiedBody,
            special = optSpecial copiedBody,
            environ = optEnviron copiedBody,
            decs    = optDecs funct @ (copiedArgs @ optDecs copiedBody),
            recCall = optRec copiedBody
          }
          end
                
          | _ => notInlineCall NONE (* Not a Lambda and not recursive. *)
        end (* Eval { } *)
        
     |  optimise (Extract(ext as {level, ...}), _) =
            lookupOldAddr (ext, nestingOfThisProcedure, level)

     |  optimise (original as Lambda({isInline=OnlyInline, ...}), _) =
            (* Used only for functors.  Leave unchanged. *)
            optVal 
                {
                  general = CodeZero,
                  (* Changed from using CodeNil here to CodeZero.  This avoids a problem
                     which only surfaced with the changes to ML97 and the possibility of
                     mixing functor and value declarations in the same "program" (i.e.
                     top-level declarations with a terminating semicolon.
                     OnlyInline is used for functors which can only ever be called,
                     never passed as values, so the "general" value is not really
                     required.  It can though appear in the result tuple of the "program"
                     from which the (value) results of the program are extracted.

                     Further change: This previously returned the processed body but the effect of
                     that was to prevent small functions inside the functor from becoming
                     inline functions when the functor was applied.  Instead we just return
                     the original code. *)
                  special = original,
                  environ = lookupOldAddr, (* Old addresses with unprocessed body. *)
                  decs    = [],
                  recCall = ref false
                }

     |  optimise (original as Lambda({body=lambdaBody, isInline=lambdaInline, name=lambdaName,
                          argTypes, resultType, ...}), _) =
        let
          (* The nesting of this new procedure is the current nesting level
             plus one. Normally this will be the same as lambda.level except
             when we have a procedure inside an inline procedure. *)
          val nesting = nestingOfThisProcedure + 1;
          
          (* A new table for the new procedure. *)
          val oldAddrTab = stretchArray (initTrans, NONE);
          val newAddrTab = stretchArray (initTrans, NONE);

          fun localOldAddr ({ addr=index, level, ...}, depth, 0) =
              (* local declaration or argument. *)
              if index > 0 
              (* Local declaration. *)
              then
                case  oldAddrTab sub index of
                    SOME v => changeLevel v (depth - nesting)
                |   NONE => raise InternalError(
                                    concat["localOldAddr: Not found. Addr=", Int.toString index,
                                           " Level=", Int.toString level])
              (* Argument or closure. *)
              else simpleOptVal (mkGenLoad (index, depth - nesting, index <> 0, false))
            | localOldAddr (ptr, depth, levels) = lookupOldAddr (ptr, depth, levels - 1);

          (* localNewAddr is used as the environment of inline functions within
             the function we're processing.  All the entries in this table will
             have their "general" entries as simply Extract entries with the
             original address.  Their "special" entries may be different. The
             only entries in the table will be those which correspond to
             declarations in the original code so there may be new declarations
             which aren't in the table. *)
          fun localNewAddr ({ addr=index, ...}, depth, 0) =
              if index > 0 
              then case newAddrTab sub index of
                    NONE => (* Return the original entry if it's not there. *)
                        simpleOptVal(mkGenLoad (index, depth - nesting, true, false))
                |   SOME v => changeLevel v (depth - nesting) 
              else simpleOptVal (mkGenLoad (index, depth - nesting, index <> 0, false))
            | localNewAddr (ptr, depth, levels) = lookupNewAddr (ptr, depth, levels - 1);

          fun setNewTab (addr, v) = update (newAddrTab, addr, SOME v)

          fun setTab (index, v) =
            (
            case optGeneral v of
                Extract{addr, ...} =>
                    if addr <= 0 then () else setNewTab(addr, v)
            |   _ => ();
            update (oldAddrTab, index, SOME v)
            )

          val newAddressAllocator = ref 1

          val newCode =
             optimiseProc 
               {pt=lambdaBody,
                lookupNewAddr=localNewAddr,
                lookupOldAddr=localOldAddr, 
                enterDec=setTab,
                enterNewDec=setNewTab,
                nestingOfThisProcedure=nesting,
                spval=newAddressAllocator,
                earlyInline=false,
                evaluate=evaluate,
                tailCallEntry=NONE,
                recursiveExpansions=recursiveExpansions,
                maxInlineSize=maxInlineSize}

          (* nonLocals - a list of the non-local references made by this
             function.  If this is empty the function can be code-generated
             immediately and returned as a constant.  If it is non-empty it
             is set as the closure for the function.  This is then used
             when processing mutually recursive functions to find the
             dependencies. *)
             
          val nonLocals = ref nil;
          fun addNonLocal({addr, level, fpRel, ...}, depth) =
          let
             (* The level will be correct relative to the use, which may be
                in an inner function.  We want the level relative to the
                scope in which this function is declared. *)
             val correctedLevel = level - (depth - nestingOfThisProcedure)
             fun findNonLocal(Extract{addr=addr', level=level', fpRel=fpRel', ...}) =
                    addr = addr' andalso correctedLevel = level' andalso fpRel=fpRel'
              |  findNonLocal _ = raise InternalError "findNonLocal: not Extract"
          in
             if List.exists findNonLocal (!nonLocals)
             then () (* Already there. *)
             else nonLocals := mkGenLoad(addr, correctedLevel, fpRel, false) :: ! nonLocals
          end

          fun checkRecursion(ext as {fpRel=oldfpRel, ...}, levels, depth) =
              case optGeneral(lookupNewAddr (ext, depth, levels)) of
                 (res as Extract(ext as {addr=0, fpRel=false, ...})) =>
                     (
                     (* If this is just a recursive call it doesn't count
                        as a non-local reference.  This only happens if
                        we turned a reference to a local into a recursive
                        reference (i.e. fpRel was previously true). *)
                     if levels = 0 andalso oldfpRel
                     then ()
                     else addNonLocal(ext, depth);
                     res
                     )
              |  res as Extract ext =>
                    (
                     addNonLocal(ext, depth);
                     res
                    )

              |  res => res (* We may have a constant in this table. *)

          val cleanedBody =
            cleanProc(getGeneral newCode, checkRecursion, nesting,
                      Array.array (! newAddressAllocator + 1, false))

          val resultCode =
            case lambdaInline of
                MaybeInline => (* Explicitly inlined functions. *)
                    (* We return the processed version of the function as
                       the general value but the unprocessed version as
                       the special value. *)
                    optVal 
                    {
                      general =
                        Lambda 
                          {
                           body          = cleanedBody,
                           isInline      = MaybeInline,
                           name          = lambdaName,
                           closure       = !nonLocals, (* Only looked at in MutualDecs. *)
                           argTypes      = argTypes,
                           resultType    = resultType,
                           level         = nesting,
                           closureRefs   = 0,
                           localCount    = ! newAddressAllocator + 1,
                           makeClosure   = false, argLifetimes = []
                         },
                      special = original,
                      environ = lookupOldAddr, (* Old addresses with unprocessed body. *)
                      decs    = [],
                      recCall = ref false (* *** REF HOTSPOT; Contributes many refs to the environment. *)
                    }

            |   _ => (* "Normal" function.  If the function is small we mark it as
                        inlineable.  If the body has no free variables we compile it
                        now so that we can propagate the resulting constant, otherwise
                        we return the processed body.  We return the processed body as
                        the special value so that it can be inlined.  We do this even
                        in the case where the function isn't small because it is just
                        possible we're going to apply the function immediately and in
                        that case it's worth inlining it anyway. *)
              let
                    val inlineType =
                        if lambdaInline = NonInline
                        then if (* Is it small? *) codeSize(cleanedBody, true) < maxInlineSize
                        then SmallFunction else NonInline
                        else lambdaInline

                  val copiedLambda =
                    Lambda 
                      {
                       body          = cleanedBody,
                       isInline      = inlineType,
                       name          = lambdaName,
                       closure       = !nonLocals, (* Only looked at in MutualDecs. *)
                       argTypes      = argTypes,
                       resultType    = resultType,
                       level         = nesting,
                       closureRefs   = 0,
                       localCount    = ! newAddressAllocator + 1,
                       makeClosure   = false, argLifetimes = []
                     };
                 val general = 
                   (* If this has no free variables we can code-generate it now. *)
                   if null (!nonLocals)
                   then evaluate(copiedLambda, !spval+1)
                   else copiedLambda
              in
                  optVal 
                    {
                      general = general,
                      special =
                        Lambda 
                          {
                           body          = cleanedBody,
                           isInline      = inlineType,
                           name          = lambdaName,
                           closure       = [],
                           argTypes      = argTypes,
                           resultType    = resultType,
                           level         = nesting,
                           closureRefs   = 0,
                           localCount    = ! newAddressAllocator + 1,
                           makeClosure   = false, argLifetimes = []
                         },
                      environ = lookupNewAddr,
                      decs    = [],
                      recCall = ref false (* *** REF HOTSPOT; Contributes many refs to the environment. *)
                    }
            end
        in
            StretchArray.freeze oldAddrTab;
            StretchArray.freeze newAddrTab;
            resultCode
        end (* Lambda{...} *)
           
     |  optimise (pt as Constnt _, _) =
            simpleOptVal pt (* Return the original constant. *)
           
     |  optimise (BeginLoop{loop=body, arguments=args, ...}, tailCall) =
        let
             (* We could try extracting redundant loop variables but for
                the time being we just see whether we actually need a loop
                or not.  This is needed if we have already constructed a loop
                from a recursive inline function and then expand it in another
                function.  If some of the loop variables are now constants we may
                optimise away the loop altogether. e.g. equality for lists where
                we actually have   if x = nil then ... *)
             val loops = ref false
             fun filterArgs (a, _, _) = (loops := true; a)

             val foptRec = ref false
             (* First process as though it was not a BeginLoop but just a
                set of declarations followed by an expression. *)
             val _ =
                optimiseProc 
                  {pt=mkEnv(List.map #1 args @ [body]), lookupNewAddr=lookupNewAddr, lookupOldAddr=lookupOldAddr,
                   enterDec=enterDec, enterNewDec=enterNewDec, nestingOfThisProcedure=nestingOfThisProcedure,
                   spval=spval, earlyInline=earlyInline, evaluate=evaluate, tailCallEntry=SOME foptRec,
                   recursiveExpansions=(filterArgs, foptRec) :: recursiveExpansions,
                   maxInlineSize=maxInlineSize}
        in
            if not (! loops)
            then (* The Loop instructions have been optimised away.  Since there's
                    no BeginLoop we can reprocess it with the surrounding
                    tail recursion. *)
                optimise(mkEnv(List.map #1 args @ [body]), tailCall)
            else (* It loops - have to reprocess. *)
            let
                (* The arguments to the functions are Declar entries but they
                   must not be optimised. *)
                fun declArg(Declar{addr, value, ...}, typ) =
                    let
                        val optVal = optimise(value, NONE)
                        val decSpval = ! spval
                        val _ = spval := decSpval + 1
                        val optV = simpleOptVal(mkLoad (decSpval, 0))
                    in
                        enterDec(addr, optV);
                        (mkDec(decSpval, getGeneral optVal), typ)
                    end
                 |  declArg _ = raise InternalError "declArg: not Declar"
                 val declArgs = map declArg args
                 val beginBody =
                    optimiseProc 
                      {pt=body, lookupNewAddr=lookupNewAddr, lookupOldAddr=lookupOldAddr, enterDec=enterDec,
                       enterNewDec=enterNewDec, nestingOfThisProcedure=nestingOfThisProcedure, spval=spval,
                       earlyInline=earlyInline, evaluate=evaluate, tailCallEntry=SOME foptRec,
                       recursiveExpansions=(filterArgs, foptRec) :: recursiveExpansions,
                       maxInlineSize=maxInlineSize}
            in
                simpleOptVal (BeginLoop {loop=getGeneral beginBody, arguments=declArgs})
            end
        end

     |  optimise (Loop args, tailCall) =
        (
            (* The Loop instruction should be at the tail of the
               corresponding BeginLoop. *)
            case (tailCall, recursiveExpansions) of
                (SOME fopt, (filterArgs, fopt') :: _) =>
                    let
                        fun gen(c, t) = (general c, t)
                    in
                        if fopt <> fopt'
                        then raise InternalError "Loop: mismatched BeginLoop"
                        else simpleOptVal (Loop ((filterArgs(map gen args, true, nestingOfThisProcedure))))
                    end
            |   _ => raise InternalError "Loop: not at tail of BeginLoop"
        )
          
     |  optimise (Raise x, _) = simpleOptVal (Raise (general x))
         
     |  optimise (Cond(condTest, condThen, condElse), tailCall) =
        let
          val insFirst = general condTest;
        in
          (* If the condition is a constant we need only
             return the appropriate arm. *)
          case insFirst of
            Constnt testResult =>
            if wordEq (testResult, False) (* false - return else-part *)
            then 
          (* if false then x else y == y *)
              if isCodeNil condElse (* May be nil. (Pattern-matching) *)
              then simpleOptVal (mkEnv [])
              else optimise(condElse, tailCall)
          (* if true then x else y == x *)
            else optimise(condThen, tailCall)  (* return then-part *)
            
           | _ =>
          let
            (* Perhaps the "if" is really a simpler expression?
               Unfortunately, we don't know whether we're returning
               a boolean result here so we can't optimise to
               andalso/orelse but we can at least look for the
               case where both results are constants. *)
            val insSecond = optimise(condThen, tailCall)
            val insThird  = optimise(condElse, tailCall)

            (* If we have tuples on both arms we can probably combine them. *)
            fun combineTuples(containerAddr, thenAddr, elseAddr, thenRec, elseRec, size) =
            let
                val thenDecs = optDecs insSecond and elseDecs = optDecs insThird

                fun replaceContainerDec([], _) =
                    raise InternalError "replaceContainerDec"
                 |  replaceContainerDec((hd as Declar{addr, ...})::tl, ad)=
                        if addr = ad
                        then (* Found the declaration. If we are using this
                                container address we remove this declaration.
                                If we have containers on both branches we
                                need to make them both point to the same
                                container. *)
                            if addr = containerAddr
                            then tl
                            else mkDec(addr, mkLoad(containerAddr, 0)) :: tl
                        else hd :: replaceContainerDec(tl, ad) 
                | replaceContainerDec(hd :: tl, ad) =
                    hd :: replaceContainerDec(tl, ad)

                fun createBranch(recEntries, decEntries, cAddr) =
                    case cAddr of
                        SOME ad => (* We have a container on that branch ... *)
                           wrapEnv(replaceContainerDec(decEntries, ad))
                    |   NONE => 
                            wrapEnv(decEntries @
                                [mkSetContainer(
                                    mkLoad(containerAddr, 0), Recconstr recEntries,
                                    size)])

                val thenPart = createBranch(thenRec, thenDecs, thenAddr)
                and elsePart = createBranch(elseRec, elseDecs, elseAddr)
                (* The result is a block which declares the container, side-effects it
                   in the "if" and makes a tuple from the result.  If we're lucky
                   the resulting tuple will be optimised away. *)
                (* This code is the same as that used to optimise TupleFromContainer
                   and is designed to allow us to optimise away the tuple creation
                   if we use the individual fields. *)
                val baseAddr = !spval
                val _ = spval := baseAddr + size
                val specialDecs =
                    List.tabulate(size,
                        fn n => mkDec(n+baseAddr, mkInd(n, mkLoad(containerAddr, 0))))
                val specialEntries = List.tabulate(size, fn n => mkLoad(n+baseAddr, 0))
                fun env (l:loadForm, depth, _) : optVal =
                    changeLevel (simpleOptVal(Extract l)) (depth - nestingOfThisProcedure)
            in
                optVal 
                    {
                      general = TupleFromContainer(mkLoad(containerAddr, 0), size),
                      special = Recconstr specialEntries,
                      environ = env,
                      decs    =
                          mkDec(containerAddr, Container size) ::
                          mkIf(insFirst, thenPart, elsePart) :: specialDecs,
                      recCall = ref false
                    }
            end (* combineTuples *)
          in
            case (optGeneral insSecond, optDecs insSecond,
                  optGeneral insThird, optDecs insThird) of
                (second as Constnt c2, [], third as Constnt c3, []) =>
              (* if x then y else y == (x; y) *)
              if wordEq (c2, c3)
              then if sideEffectFree insFirst
                  then insSecond
                  else
                  (* Must put insFirst in decs, so it gets executed *) 
                  optVal 
                    {
                      general = second,
                      special = CodeNil,
                      environ = errorEnv,
                      decs    = [insFirst],
                      recCall = ref false
                    }
                  
              (* if x then true else false == x *)
              else if wordEq (c2, True) andalso wordEq (c3, False)
                then simpleOptVal insFirst
              
              (* if x then false else y == not x *)
              else if wordEq (c2, False) andalso wordEq (c3, True)
                then simpleOptVal (mkNot insFirst)
              
              else (* can't optimise *)
                simpleOptVal (mkIf (insFirst, second, third))

           | (Recconstr thenRec, _, Recconstr elseRec, _) =>
                (* Both tuples - are they the same size?  They may not be if they
                   are actually datatypes. *)
                if List.length thenRec = List.length elseRec
                then (* We can transform this into an operation which creates space
                        on the stack, side-effects it and then picks up the result
                        from it. *)
                let
                    val size = List.length thenRec (* = List.length elseRec *)
                    (* Create a new address for the container. *)
                    val containerAddr = let val ad = !spval in spval := ad + 1; ad end
                in
                    combineTuples(containerAddr, NONE, NONE, thenRec, elseRec, size)
                end
                else (* Different sizes - use default. *)
                   simpleOptVal (mkIf (insFirst, getGeneral insSecond, getGeneral insThird))

           | (TupleFromContainer(Extract{addr=thenAddr,level=0,fpRel=true, ...}, thenSize), _,
              TupleFromContainer(Extract{addr=elseAddr,level=0,fpRel=true, ...}, elseSize), _) =>
                (* Have both been converted already.  If we are returning a tuple from
                   a container the container must be declared locally. *)
                if thenSize = elseSize
                then (* We can combine the containers.  We can't if these are actually
                        datatypes in which case they could be different sizes. *)
                let
                    (* If we have already transformed this we will have a
                       declaration of a container somewhere in the list. *)
                    (* Use the address which has already been allocated for the else part.
                       That makes it easier for the subsequent pass to convert this into
                       a "case" if appropriate. *)
                    val containerAddr = elseAddr
                in
                    combineTuples(containerAddr, SOME thenAddr, SOME elseAddr, [], [], thenSize)
                end
                else (* Different sizes - use default. *)
                   simpleOptVal (mkIf (insFirst, getGeneral insSecond, getGeneral insThird))

           | (TupleFromContainer(Extract{addr=thenAddr,level=0,fpRel=true, ...}, thenSize), _,
              Recconstr elseRec, _) =>
                (* The then-part has already been converted *)
                if thenSize = List.length elseRec
                then combineTuples(thenAddr, SOME thenAddr, NONE, [], elseRec, thenSize)
                else (* Different sizes - use default. *)
                   simpleOptVal (mkIf (insFirst, getGeneral insSecond, getGeneral insThird))

           | (Recconstr thenRec, _,
              TupleFromContainer(Extract{addr=elseAddr,level=0,fpRel=true, ...}, elseSize), _) =>
                (* The else-part has already been converted *)
                if elseSize = List.length thenRec
                then
                    combineTuples(elseAddr, NONE, SOME elseAddr, thenRec, [], elseSize)
                else (* Different sizes - use default. *)
                   simpleOptVal (mkIf (insFirst, getGeneral insSecond, getGeneral insThird))

           | _ => (* Not constants or records. *)
            simpleOptVal (mkIf (insFirst, getGeneral insSecond, getGeneral insThird))
          end
        end (* isCond pt *)
         
     |  optimise (Newenv envDecs, tailCall) =
        let (* Process the body. *)
          (* Recurses down the list of declarations and expressions processing
             each, and then reconstructs the list on the way back. *)

          (* Only if we have an empty block or a block containing only
             declarations i.e. a declaration is used to discard the result
             of a function and only perform its side-effects. *)
          fun copyDeclarations []  = simpleOptVal (mkEnv [])
            | copyDeclarations (Declar{addr=caddr, value, ...} :: vs) = 
              let
                (* Add the declaration to the table. *)
                val dec =
                  newDecl (enterDec, optimise(value, NONE), caddr, true);
                  
                (* Deal with the rest of the block. *)
                val rest = copyDeclarations vs;
             in
                case dec of
                  [] => rest
                | _  => (* Must put these declarations onto the list. *)
                  optVal 
                    {
                      general = optGeneral rest,
                      special = optSpecial rest,
                      environ = optEnviron rest,
                      decs    = dec @ optDecs rest,
                      recCall = optRec rest
                    }
            end

            | copyDeclarations (MutualDecs mutualDecs :: vs) = 
            (* Mutually recursive declarations. Any of the declarations may
               refer to any of the others. They should all be lambdas.

               The front end generates functions with more than one argument
               (either curried or tupled) as pairs of mutually recursive
               functions.  The main function body takes its arguments on
               the stack (or in registers) and the auxiliary inline function,
               possibly nested, takes the tupled or curried arguments and
               calls it.  If the main function is recursive it will first
               call the inline function which is why the pair are mutually
               recursive.
               As far as possible we want to use the main function since that
               uses the least memory.  Specifically, if the function recurses
               we want the recursive call to pass all the arguments if it
               can.  We force the inline functions to be macros while
               processing the non-inline functions and then process the
               inlines. DCJM 23/1/01. *)
            let
                (* Split the inline and non-inline functions. *)
                val (inlines, nonInlines) =
                    List.partition (
                        fn Declar{value = Lambda{ isInline=MaybeInline, ...}, ... } => true | _ => false) mutualDecs
                    
                (* Go down the non-inline functions creating new addresses
                   for them and entering them in the table. *)
                val startAddr = !spval
                val addresses =
                    map (fn Declar{addr, ... } =>
                        let
                            val decSpval   = !spval;
                        in
                            enterDec (addr, simpleOptVal (mkLoad (decSpval, 0)));
                            spval := !spval + 1;
                            decSpval
                        end
                      | _ => raise InternalError "mutualDecs: not Declar")
                    nonInlines
                val endAddr = !spval
                (* We can now process the inline functions.  Since these
                   can't be directly recursive we don't need to do anything
                   special. *)
                val _ =
                    List.app (fn Declar{ value, addr, ... } =>
                                enterDec (addr, optimise(value, NONE))
                          | _ => raise InternalError "mutualDecs: not Declar")
                    inlines

                (* Next process the non-inlines.  We really want to be able to
                   compile the functions now if we can and get a constant for
                   the code address.  We can do that for functions which make
                   no non-local references or whose non-local references are
                   by means of constants.  For non-recursive declarations this
                   is easy since an earlier declaration cannot depend on a later
                   one but for mutually recursive declarations we don't know
                   the dependencies.
                   The simple case is where we have a function which does not
                   depend on anything and so can be code-generated in the Lambda
                   case.  Code-generating that may allow others to be code-generated.
                   Another case is where the functions depend on each other but not
                   on anything else.  We can compile them together but not
                   individually.  There are various versions of this second case.
                   The only one we consider here is if all the (non-constant)
                   functions are of that form in which case we process the
                   whole mutually-recursive declaration. *)
                val hasNonLocalReference = ref false

                fun checkClosure (Extract{addr, level=0, fpRel=true, ...}) =
                    if addr >= startAddr andalso addr < endAddr
                    then ()
                    else hasNonLocalReference := true
                |   checkClosure _ = hasNonLocalReference := true

                fun processNonInlines (Declar{ value = decVal, addr = decAddr, ... },
                                       decSpval, (decs, otherChanges)) =
                (* Have a look at the old entry to see if it's a constant. *)
                let
                    val oldEntry =
                        lookupOldAddr(
                                {addr=decAddr, level=0, fpRel=true, lastRef=false},
                                nestingOfThisProcedure, 0)
                in
                    case optGeneral oldEntry of
                        oldGen as Constnt _ =>
                            (mkDec (decSpval, oldGen) :: decs, otherChanges) (* It's already a constant - don't reprocess. *)
                    |   _ =>
                        let
                            (* Set this entry to create a recursive call if we load
                               the address while processing the function. The recursive
                               call may come about as a result of expanding an inline
                               function which then makes the recursive call. *)
                            local
                                val recursive = simpleOptVal (mkGenLoad (0, ~1, false, false))
                            in
                                val _ = enterDec(decAddr, recursive);
                                val _ = enterNewDec(decSpval, recursive)
                            end;
                       
                            (* Now copy this entry. *)
                            val ins  = optimise(decVal, NONE)

                            val gen  = optGeneral ins;
                            val spec = optSpecial ins;

                            (* The general value is either a reference to the
                               declaration or a constant if the function has just
                               been compiled into a code segment. *)
                            val isConstant = isConstnt gen
                            val optGen =
                                case gen of
                                    Constnt _ => gen
                                |   Lambda{closure, ...} => (
                                        List.app checkClosure closure;
                                        mkLoad (decSpval, 0)
                                    )
                                |   _ => raise InternalError "processNonInlines: Not a function";

                            (* Explicitly reset the entry in the new table. *)
                            val _  = enterNewDec(decSpval, simpleOptVal optGen);
              
                            (* If this is a small function we leave the special
                               value so it can be inserted inline.  Otherwise
                               we clear it. *)
                            val optSpec =
                                 case spec of
                                    Lambda{ isInline=NonInline, ...} => CodeNil
                                   | _ => optSpecial ins;
                            val nowInline =
                                not (isCodeNil optSpec) andalso isCodeNil(optSpecial oldEntry)
                            (* If this is now a constant or it is a small function when it
                               wasn't before we need to reprocess everything
                               which depends on it to try to get the constant inserted
                               everywhere it can be. *)
                      in
                              enterDec 
                                (decAddr,
                                 optVal 
                                    {
                                      general = optGen,
                                      special = optSpec,
                                      environ = optEnviron ins,
                                      decs    = optDecs ins, (* Should be nil. *)
                                      recCall = optRec ins
                                    });
                            (
                             mkDec (decSpval, gen) :: decs,
                             otherChanges orelse isConstant orelse nowInline
                            )
                      end
               end

              | processNonInlines _ =
                    raise InternalError "processNonInlines: not Declar"

              fun repeatProcess () =
              let
                  val (decs, haveChanged) =
                     (* Use foldr here to keep the result in the same order
                        in case we can compile them immediately below. *)
                     ListPair.foldr processNonInlines
                        ([], false) (nonInlines, addresses);
              in
                 if haveChanged
                 then repeatProcess ()
                 else decs
              end

              val decs = repeatProcess ()

              val allAreConstants =
                List.foldl
                    (fn(Declar{value=Constnt _, ...}, others) => others
                      | _ => false) true decs

              (* If hasNonLocalReference is still false we can code-generate
                 the mutual declarations. *)
              val decs =
                 if ! hasNonLocalReference orelse allAreConstants
                 then decs
                 else
                    let
                        (* Create a tuple of Extract entries to get the result. *)
                        val extracts =
                            List.map (
                                fn (Declar{addr, ...}) => mkLoad(addr, 0)
                                 | _ => raise InternalError "extracts: not Declar")
                                decs
                        val code = mkEnv[mkMutualDecs decs, mkTuple extracts]
                        (* Code generate it. *)
                        val results = evaluate(code, !spval+1)

                        fun reprocessDec(Declar{addr=decAddr, ...}, decSpval, (offset, others)) =
                        let
                            val oldEntry =
                                lookupOldAddr(
                                        {addr=decAddr, level=0, fpRel=true, lastRef=false},
                                        nestingOfThisProcedure, 0)
                        in
                            let
                                val newConstant = findEntryInBlock results offset
                            in
                                (* Replace the entry by an entry with a constant. *)
                                enterNewDec(decSpval, simpleOptVal newConstant);
                                enterDec 
                                    (decAddr,
                                     optVal 
                                        {
                                          general = newConstant,
                                          special = optSpecial oldEntry,
                                          environ = optEnviron oldEntry,
                                          decs    = optDecs oldEntry, (* Should be nil. *)
                                          recCall = optRec oldEntry
                                        });
                                (offset+1, mkDec(decSpval, newConstant) :: others)
                            end
                        end
                        |   reprocessDec _ = raise InternalError "reprocessDec: not Declar"
                        
                        val (_, newDecs) = ListPair.foldl reprocessDec (0, []) (nonInlines, addresses);
                    in
                        newDecs (* We've converted them all to constants. *)
                    end

              (* Deal with the rest of the block *)
              val rest = copyDeclarations vs

                (* Separate out the constants.  They don't need to be mutually declared and putting them earlier
                   may mean that they can be inserted into the code. *)
                val (constnts, nonConsts) =
                    List.partition(fn Declar{value=Constnt _, ...} => true | _ => false) decs

                val mutuals =
                    case nonConsts of
                        [] => []
                    |   [singleton] => [singleton]
                    |   multiple => [mkMutualDecs multiple]
            in
              (* and put these declarations onto the list. *)
              optVal
                {
                  general = optGeneral rest,
                  special = optSpecial rest,
                  environ = optEnviron rest,
                  decs    = constnts @ mutuals @ optDecs rest,
                  recCall = optRec rest
                }
            end
          
            | copyDeclarations [v] =
                (* Last expression. *) optimise(v, tailCall)

            | copyDeclarations (v :: vs) = 
            let (* Not a declaration - process this and the rest.*)
                val copiedNode = optimise(v, NONE);
                val rest = copyDeclarations vs;
            in  (* This must be a statement whose
                   result is ignored. Put it into the declaration list. *)
                optVal 
                    {
                      general = optGeneral rest,
                      special = optSpecial rest,
                      environ = optEnviron rest,
                      decs    = optDecs copiedNode @ 
                        (optGeneral copiedNode :: optDecs rest),
                      recCall = optRec rest
                    }
          end; (* copyDeclarations *)
        in
          copyDeclarations envDecs
        end (* isNewenv *)
          
    |   optimise (Recconstr entries, _) =
         (* The main reason for optimising record constructions is that they
            appear as tuples in ML. We try to ensure that loads from locally
            created tuples do not involve indirecting from the tuple but can
            get the value which was put into the tuple directly. If that is
            successful we may find that the tuple is never used directly so
            the use-count mechanism will ensure it is never created. *)
        let
            val newTab = Array.array(List.length entries+1, NONE)
            fun setTab (i, v) = Array.update (newTab, i, SOME v);
            (* The record construction is treated as a block of local
               declarations so that any expressions which might have side-effects
               are done exactly once. *)
            fun makeDecs []     _ = {decs = [], gen = [], spec = []}
            |   makeDecs (h::t) addr =
                let
                    (* Declare this value. If it is anything but a constant
                       there will be some code. *)
                    val newDecs = newDecl (setTab, optimise(h, NONE), addr, true)
                    val thisArg = valOf (Array.sub(newTab, addr)) (* Get the value back. *)
                    val {decs=restDecs, gen=genRest, spec=specRest} = makeDecs t (addr + 1)
                    val gen = optGeneral thisArg
                    (* If there's no "special" and the general is a constant use that for
                       the special entry otherwise load the original value. *)
                    val specArg =
                        case (optSpecial thisArg, gen) of
                            (CodeNil, Constnt _) => gen
                        |   _ => mkLoad (addr, 0)
                in
                    {gen = gen :: genRest, spec = specArg :: specRest, decs = newDecs @ restDecs }
                end
          
            val {gen, spec, decs} = makeDecs entries 1
            val newRec  = Recconstr gen
            (* Package up the declarations so we can extract the special values. *)
            val vec = Array.vector newTab
            fun env ({addr, ...}:loadForm, depth, _) : optVal =
                changeLevel (valOf (Vector.sub(vec, addr))) (depth - nestingOfThisProcedure)
        in
            optVal 
            {
                (* If all the general values are constants we can create the tuple now. *)
                general = if List.all isConstnt gen then makeConstVal newRec else newRec,
                special = Recconstr spec,
                environ = env,
                decs    = decs,
                recCall = ref false
            }
        end
          
      |  optimise (Indirect{ base, offset }, _) = (* Try to do the selection now if possible. *)
            doIndirection(optimise(base, NONE), offset)
       
      |  optimise (pt as Ldexc, _) =
            simpleOptVal pt (* just a constant so return it *)
        
      |  optimise (Handle { exp, handler }, tailCall) =
        simpleOptVal 
          (Handle {exp     = general exp,
                   handler = generalOptimise(handler, tailCall)}
          )

      |  optimise (c as Container _, _) = simpleOptVal c

      |  optimise (TupleFromContainer(container, size), _) =
        let
            (* If possible we want to optimise this away in the same way as
               a tuple made with Recconstr.  We have to be careful, though,
               that we have no references to the container after we return.
               We first make declarations for all the fields and then return 
               a special entry which when we apply the "env" environment
               function to it gives us returns.  That way if we never actually
               use this tuple as a single entity it won't be created.
               If we don't actually use a field the corresponding declaration
               will be removed in cleanCode. *)
            val optCont = optimise(container, NONE)
            (* Since "container" will always be an Extract entry we can have multiple
               references to it in the declarations.  Include an assertion to that
               effect just in case future changes make that no longer true. *)
            val _ =
                case optGeneral optCont of
                   Extract _ => ()
                | _ => raise InternalError "optimise - container is not Extract"
            val baseAddr = !spval
            val _ = spval := baseAddr + size
            val specialDecs =
                List.tabulate(size, fn n => mkDec(n+baseAddr, mkInd(n, optGeneral optCont)))
            val specialEntries = List.tabulate(size, fn n => mkLoad(n+baseAddr, 0))
            fun env (l:loadForm, depth, _) : optVal =
                changeLevel (simpleOptVal(Extract l)) (depth - nestingOfThisProcedure)
        in
            optVal 
                {
                  general = TupleFromContainer(optGeneral optCont, size),
                  special = Recconstr specialEntries,
                  environ = env,
                  decs    = optDecs optCont @ specialDecs,
                  recCall = ref false
                }
        end

      |  optimise (SetContainer{container, tuple, size}, _) =
            (
                (* Push the set-container down the tree and then process it. If we've
                   expanded an inline function we want to be able to find any
                   tuple we're creating. *)
                case tuple of
                    Cond _ => optimise(mkSetContainer(container, tuple, size), NONE)
                |   Newenv _ => optimise(mkSetContainer(container, tuple, size), NONE)
                |   _ =>
                    let
                        val optCont = general container
                        and optTuple = general tuple
                        (* If the "tuple" is an expanded inline function it may well
                           contain an if-expression.  If both branches were tuples
                           we will have expanded it already and the result will be
                           a TupleFromContainer. *)
                        fun pushSetContainer(Cond(ifpt, thenpt, elsept), decs) =
                            Cond(ifpt,
                                wrapEnv(List.rev(pushSetContainer(thenpt, []))),
                                wrapEnv(List.rev(pushSetContainer(elsept, [])))
                            ) :: decs

                        |   pushSetContainer(Newenv env, decs) =
                            let
                                (* Get the declarations off the block and apply
                                   pushSetContainer to the last. *)
                                fun applyToLast (_, []) = raise List.Empty
                                  | applyToLast (d, [last]) = pushSetContainer(last, d)
                                  | applyToLast (d, hd :: tl) =
                                        applyToLast(hd :: d, tl)
                            in
                                applyToLast(decs, env)
                            end

                        |   pushSetContainer(tuple as
                                TupleFromContainer(
                                    Extract{addr=innerAddr, level=0, fpRel=true, ...}, innerSize),
                                decs) =
                            if innerSize = size
                            then
                                (
                                case optCont of
                                    Extract{addr=containerAddr, level=0, fpRel=true, ...} =>
                                    let
                                        (* We can remove the inner container and replace it by
                                           a reference to the outer. *)
                                        fun replaceContainerDec [] =
                                            raise InternalError "replaceContainerDec"
                                          | replaceContainerDec ((hd as Declar{addr, ...}) :: tl) =
                                                    if addr = innerAddr
                                                    then mkDec(addr, mkLoad(containerAddr, 0)) :: tl
                                                    else hd :: replaceContainerDec tl 
                                          | replaceContainerDec(hd :: tl) =
                                                hd :: replaceContainerDec tl
                                    in
                                        (* Just replace the declaration. *)
                                        replaceContainerDec decs 
                                    end
                                | _ => SetContainer{container = optCont, tuple = tuple, size = size}
                                         :: decs
                                )
                            else SetContainer{container = optCont, tuple = tuple, size = size} :: decs

                        |   pushSetContainer(tuple, decs) =
                                SetContainer{container = optCont, tuple = tuple, size = size} :: decs
                    in
                        simpleOptVal(wrapEnv(List.rev(pushSetContainer(optTuple, []))))
                    end
            )

      |  optimise (Global g, _) = g

      |  optimise (TagTest{test, tag, maxTag}, _) =
            let
                val optTest = general test
            in
                case optTest of
                    Constnt testResult =>
                        if isShort testResult andalso toShort testResult = tag
                        then simpleOptVal CodeTrue
                        else simpleOptVal CodeFalse
                |   _ => simpleOptVal(TagTest{test=optTest, tag=tag, maxTag=maxTag})
            end

      |   optimise(IndirectVariable{base, offset}, tailCall) =
            (* If this is a constant offset turn it into a simple Indirect instr.
               TODO: If the "base" is actually a TupleVariable we may be able to
               optimise it away in the same way as with fixed-size tuples. *)
            let
                val optOffset = general offset
            in
                case optOffset of
                    Constnt offs =>
                        optimise(Indirect{base=base, offset=Word.toInt(toShort offs)}, tailCall)
                |   _ =>
                    simpleOptVal(IndirectVariable{base=general base, offset=optOffset})
            end

      |   optimise(TupleVariable(vars, length), _) =
            (* Try to turn this into a fixed size tuple. *)
            let
                val optLength = general length
            in
                case optLength of
                    Constnt _ =>
                        (* The total length is the sum of all the individual lengths so if it is
                           a constant then all the offsets and lengths must be.  We can turn this
                           into a fixed-size tuple.  We need new declarations at least for the multiple
                           entries so that they are used only once.  Doing that requires us to do the
                           optimisation at this stage rather than generating a Reccons and calling
                           "optimise" recursively so this duplicates that code. *)
                        let
                            val newTab = stretchArray(List.length vars+1, NONE)
                            fun setTab (i, v) = StretchArray.update (newTab, i, SOME v);
                            (* The record construction is treated as a block of local
                               declarations so that any expressions which might have side-effects
                               are done exactly once. *)
                            fun makeOneEntry(optSource, addr) =
                            let
                                (* Put the source into a local variable. *)
                                val newDecs = newDecl (setTab, optSource, addr, true)
                                val thisArg = valOf (StretchArray.sub(newTab, addr)) (* Get the value back. *)
                                val gen = optGeneral thisArg
                                (* If there's no "special" and the general is a constant use that for
                                   the special entry otherwise load the original value. *)
                                val specArg =
                                    case (optSpecial thisArg, gen) of
                                        (CodeNil, Constnt _) => gen
                                    |   _ => mkLoad (addr, 0)
                            in
                                { gen=gen, spec=specArg, decs=newDecs }
                            end

                            fun makeDecs([], _) = {decs = [], gen = [], spec = []}

                            |   makeDecs (VarTupleSingle{source, ...} :: t, addr) =
                                let
                                    val {gen, spec, decs} = makeOneEntry(optimise(source, NONE), addr)
                                    val {decs=restDecs, gen=genRest, spec=specRest} = makeDecs(t, addr + 1)
                                in
                                    {gen = gen :: genRest, spec = spec :: specRest, decs = decs @ restDecs }
                                end

                            |   makeDecs (VarTupleMultiple{base, length, sourceOffset, ...} :: t, addr) =
                                let
                                    val len =
                                        case general length of
                                            Constnt l => Word.toInt(toShort l)
                                        |   _ => raise InternalError "makeDecs: not constant"
                                    (* Put the base address into a local variable.  We must do this because
                                       the base code could have side-effects which must be done exactly once. *)
                                    val baseDecs = newDecl (setTab, optimise(base, NONE), addr, true)
                                    val thisArg = valOf (StretchArray.sub(newTab, addr)) (* Get the value back. *)
                                    val {decs=restDecs, gen=genRest, spec=specRest} = makeDecs(t, addr + len+ 1)

                                    (* Each entry in the result is found by indirection on the base. *)
                                    fun makeEntries n =
                                        if n = len then {decs = [], gen = [], spec = []}
                                        else
                                        let
                                            (* The offset could be a variable even if the resulting size is
                                               a constant. *)
                                            val indirectEntry =
                                                case general sourceOffset of
                                                    Constnt offset => doIndirection(thisArg, n + Word.toInt(toShort offset))
                                                |   varOffset =>
                                                    let
                                                        (* This isn't currently generated so leave this in until it is. *)
                                                        val _ = raise InternalError "Untested"
                                                        (* Add the offsets *)
                                                        val newOffset =
                                                            if n = 0 then varOffset
                                                            else mkEval(rtsFunction POLY_SYS_plus_word,
                                                                        [varOffset, mkConst(toMachineWord n)], true)
                                                    in
                                                        simpleOptVal(
                                                            IndirectVariable{base=optGeneral thisArg, offset=newOffset})
                                                    end
                                            val thisEntry = makeOneEntry(indirectEntry, addr+n+1)   
                                            val {decs, gen, spec} = makeEntries(n+1)
                                        in
                                            {decs = #decs thisEntry @ decs, gen = #gen thisEntry :: gen,
                                             spec = #spec thisEntry :: spec}
                                        end

                                    val {gen, spec, decs} = makeEntries 0
                                in
                                    {gen = gen @ genRest, spec = spec @ specRest, decs = baseDecs @ decs @ restDecs }
                                end
          
                            val {gen, spec, decs} = makeDecs(vars, 1)
                            val newRec  = Recconstr gen
                            (* Package up the declarations so we can extract the special values. *)
                            val vec = StretchArray.vector newTab
                            fun env ({addr, ...}:loadForm, depth, _) : optVal =
                                changeLevel (valOf (Vector.sub(vec, addr))) (depth - nestingOfThisProcedure)
                        in
                            optVal 
                            {
                                (* If all the general values are constants we can create the tuple now. *)
                                general = if List.all isConstnt gen then makeConstVal newRec else newRec,
                                special = Recconstr spec,
                                environ = env,
                                decs    = decs,
                                recCall = ref false
                            }
                        end
                |   _ => (* Can't convert it to a fixed-size tuple.
                           TODO: We should treat this in the same way as a fixed record so that selection can be
                           optimised.  This isn't quite so important as for fixed-size tuples.  Fixed-size tuples
                           are used for structures and there we definitely need to be able to extract inline
                           functions.  *)
                    let
                        fun optTuple(VarTupleSingle{source, destOffset}) =
                                VarTupleSingle{source=general source, destOffset=general destOffset}
                        |   optTuple(VarTupleMultiple{base, length, destOffset, sourceOffset}) =
                                VarTupleMultiple{base=general base, length=general length,
                                                 destOffset=general destOffset, sourceOffset=general sourceOffset}
                        val optFields = map optTuple vars
                    in
                        simpleOptVal(TupleVariable(optFields, optLength))
                    end
            end

      |  optimise (Case _, _) = raise InternalError "optimise: Case"

      |  optimise (Declar _, _) = raise InternalError "optimise: Declar"

      |  optimise (MutualDecs _, _) = raise InternalError "optimise: MutualDecs"

      |  optimise (KillItems _, _) = raise InternalError "optimise: StaticLinkCall"

    (* optimise *);

(*****************************************************************************)
(*                  body of optimiseProc                                     *)
(*****************************************************************************)
  in
    optimise(pt, tailCallEntry)
  end (* optimiseProc *);


(*****************************************************************************)
(*                  genCode                                                  *)
(*****************************************************************************)
  fun genCode(pt, debugSwitches) =
  let
    val printCodeTree      = DEBUG.getParameter DEBUG.codetreeTag debugSwitches
    and printCodeTreeAfter = DEBUG.getParameter DEBUG.codetreeAfterOptTag debugSwitches
    and maxInlineSize      = DEBUG.getParameter DEBUG.maxInlineSizeTag debugSwitches
    and compilerOut        = PRETTY.getCompilerOutput debugSwitches

    val () = if printCodeTree then compilerOut(pretty pt) else ();

    (* This ensures that everything is printed just before
       it is code-generated. *)
    fun codeGenAndPrint(code, localCount) =
    (
        if printCodeTreeAfter then compilerOut(pretty code) else ();
        GCODE.gencode(code, debugSwitches, localCount)
    )
    
    fun preCodeAndPrint(code, localCount) =
    (
        if printCodeTree then compilerOut(pretty code) else ();
        preCode (codeGenAndPrint, code, localCount)
    )

    val localAddressAllocator = ref 1

    (* Optimise it. *)
    val oldAddrTab = stretchArray (initTrans, NONE);
    val newAddrTab = stretchArray (initTrans, NONE);

    val insertedCode =
    let
      (* Strip off a surrounding declaration. *)
      val pt =
        case pt of Declar{value, ...} => value | _ => pt;
        
      fun lookupOldAddr ({addr, level, fpRel, ...}: loadForm, depth, 0) =
            (
                case oldAddrTab sub addr of
                    SOME v => changeLevel v depth
                |   NONE =>
                    let
                        val msg =
                            concat["lookupOldAddr: outer level. (Addr=", Int.toString addr,
                                " Level=", Int.toString level, " Fprel=", Bool.toString fpRel, ")"]
                    in
                        raise InternalError msg
                    end
            )
      |   lookupOldAddr ({addr, level, fpRel, ...}, _, _) =
            let
                val msg =
                    concat["lookupOldAddr: outer level. (Addr=", Int.toString addr,
                        " Level=", Int.toString level, " Fprel=", Bool.toString fpRel, ")"]
            in
                raise InternalError msg
            end;

      fun lookupNewAddr ({addr, ...}: loadForm, depth, 0) =
        (
        case newAddrTab sub addr of
            NONE => simpleOptVal(mkGenLoad (addr, depth, true, false))
        |   SOME v => changeLevel v depth 
        )
      |  lookupNewAddr _ = raise InternalError "outer level reached in lookupNewAddr";

      fun enterNewDec (addr, tab) = update (newAddrTab, addr, SOME tab)

      fun enterDec (addr, tab) =
        (
        (* If the general part is an Extract entry we need to add an entry to
           the new address table as well as the old one.  This is sufficient
           while newDecl does not treat Indirect entries specially. *)
        case optGeneral tab of
            Extract{addr=newAddr, ...} => enterNewDec (newAddr, tab)
        |   _ => ();
        update (oldAddrTab, addr, SOME tab)
        );
      
      fun eval(pt, localCount) = evaluate (preCodeAndPrint(pt, localCount), codeGenAndPrint, localCount)

      val resultCode =
          optimiseProc
            {pt=pt, lookupNewAddr=lookupNewAddr, lookupOldAddr=lookupOldAddr, enterDec=enterDec,
             enterNewDec=enterNewDec, nestingOfThisProcedure=0, spval=localAddressAllocator, earlyInline=false,
             evaluate=eval, tailCallEntry=NONE, recursiveExpansions=[], maxInlineSize=maxInlineSize }
    in
        (* Turn the arrays into vectors. *)
        StretchArray.freeze oldAddrTab;
        StretchArray.freeze newAddrTab;
        resultCode
    end; (* insertedCode *)
    
    val gen  = optGeneral insertedCode;
    val spec = optSpecial insertedCode;
    val decs = optDecs insertedCode;
    
    (* No longer treat top-level tuples as special (experiment!).
       This avoids building an extra environment around the tuple
       containing the values needed to build it. SPF 1/5/95 *)
(* ...
    val notSpecial =
      if isLambda spec
      then (lambdaInline (cLambda spec) = NonInline)
      else (isCodeNil spec orelse isRecconstr spec) (* SPF 1/5/95 *)
... *)
    (*
       Treat top-level tuples as "special" again. Why?
       Suppose we have the declaration:
          val foo = fn (x,y) => ...
       This generates a tuple (a 1-tuple, actually), containing
       the naive code (arguments as a pair) for foo. However,
       if foo is small enough to in-line, it will be treated
       as "special" and the "special" code to construct the
       tuple will contain the "special" code to call the function
       with arguments in registers. Since we are keen to get the
       latter code (VERY keen for RTS calls), we just have to pay
       the cost of building the second (environment) tuple. SPF 9/5/95 *)
    val notSpecial =
        case spec of
            Lambda{isInline, ...} => isInline = NonInline
          | CodeNil => true
          | _ => false
  in
    if notSpecial 
    then let
      (* Nothing special or it is a non-inline procedure - Code-generate it. *)
      val optCode = wrapEnv(decs @ [gen])
       ;
    in
        if isConstnt optCode (* Save code-generating it. *)
        then (fn () => optCode)
        else
        let
            val localCount = !localAddressAllocator+1
            val code = codeGenAndPrint (preCodeAndPrint(optCode, localCount), localCount)
        in (* Return procedure to execute it *)
            fn () => Global (simpleOptVal (mkConst (code ())))
        end
    end
        
    else (* There is something in "special". *)
      if null decs
      then  (* Simply return it - it can have no non-local references. *)
        (fn () => Global insertedCode)
        
      else let
        (* We have some declarations to evaluate but we can't do that until
           we execute the code. Expand out any mutual declarations and
           remove any expressions which are being evaluated only for their
           side-effects. *)
        fun expandMutual [] = []
          | expandMutual (MutualDecs dec :: decs) =
                expandMutual dec @ expandMutual decs
          | expandMutual ((dec as Declar _) :: decs) =
                dec :: expandMutual decs
          | expandMutual (_ :: decs) =
                expandMutual decs; (* expression *)
             
        (* There seems to be a problem with this code - we put declarations
           in the tuple even if those declarations are unused. In fact we can't
           tell whether the declarations are used, because we haven't computed
           their reference counts yet. This means that we can generate a LOT
           of junk if someone writes "open Motif" without first constraining
           it with a signature. I'll have to come back and look at this some
           time. SPF 3/4/97
        *)
        (* It's more difficult than that.  We need the declarations for
           the "special" entries so the reference counts won't help.  Because
           of the optimisations we may well have declarations which are unused
           in the general entries but which are referred to by special entries.
           The purpose of this vector is to provide the "general" value (always
           a constant because it's been evaluated) for any declarations used
           in the special values.  
           DCJM 19/3/01. *)
             
        (* For each declaration in the sequence generate a corresponding load
           instruction to get its value. The declarations will normally be in
           ascending order but there may be gaps if a declaration contains
           a block with declarations in it. The gaps are replaced with zero
           values. However mutually recursive declarations may be in a random
           order so the list may have to be sorted. *)
        fun getValues ([]: codetree list) _ : codetree list =
              [] (* Last of all the general value. *)
              
          | getValues (decs as (Declar{addr=declAddr, ...} :: vs)) (addr: int): codetree list =
            if declAddr < addr (* Already done? *)
            then getValues vs addr (* remove *)
            else let
              fun findEntry [] = CodeZero (* Not found. *)
                | findEntry (Declar{addr=dAddr, value, ...} :: rest) =
                    if dAddr <> addr
                    then findEntry rest
                    else (* Found the declaration. *)
                    (
                    case value of
                        Container size =>
                            (* We mustn't put container values in the result since
                               they won't persist after the code that creates them
                               has exited.  We replace them with TupleFromContainer
                               entries. *)
                            TupleFromContainer(mkLoad (addr, 0), size)
                    |   _ => mkLoad (addr, 0) (* Found - put in a load. *)
                    )
                | findEntry _ =
                    raise InternalError "findEntry: not Declar"
            in
              findEntry decs  :: getValues decs (addr + 1)
            end

          | getValues _ _ =
                raise InternalError "getValues: not a Declar";
        
        val ext     = gen :: getValues (expandMutual decs) 1;
        val newDecs = mkEnv (decs @ [mkTuple ext]);
        val localCount = !localAddressAllocator+1
        val code    = codeGenAndPrint (preCodeAndPrint(newDecs, localCount), localCount)
      in (* We now have the values of the declarations. *)
        fn () =>
          let
           (* Execute the code - the result is a vector with the
             declarations in it. *)
            val decVals : address =
              let
                val res = code ()
              in
                if isShort res
                then raise InternalError "Result vector is not an address"
                else toAddress res
              end;
            
            (* Construct a new environment so that when an entry is looked 
               up the corresponding constant is returned. *) 
            fun newEnviron oldEnv (lval, depth, levels) =
            let
              val oldVal = oldEnv (lval, depth, levels);
              
              (* Get the constant out of the table. *)
              fun look (Extract{addr, ...}) : codetree = 
                let
                  val base   = decVals;
                  val offset = toShort addr;
                in
                  mkConst (loadWord (base, offset))
                end
                
               | look (g as Indirect{base, offset}) : codetree = 
                let
                  val v   = look base;
                in
                  case v of
                    Constnt caddr =>
                      let
                        val base   = toAddress caddr;
                        val offset = toShort offset;
                      in
                        mkConst (loadWord (base, offset))
                      end
                  | _ => g
                end
                
               | look g = g; (* end look *)
               
               val specVal = optSpecial oldVal;
               
               val envVal = (* SPF 10/12/96 *)
                 if isCodeNil specVal
                 then errorEnv
                 else newEnviron (optEnviron oldVal)
            in                       
              optVal 
                {
                  general = look (optGeneral oldVal),
                  special = specVal,
                  environ = envVal,
                  decs    = optDecs oldVal, (* should be nil *)
                  recCall = optRec oldVal
                }
            end (* newEnviron *);
                
            (* Get the general value, the zero'th entry in the vector. *)
            val generalVal = loadWord (decVals, toShort 0);
          in 
            (* and return the whole lot as a global value. *)
            Global
              (optVal 
                 {
                   general = mkConst generalVal,
                   special = spec, (* <> CodeNil *)
                   environ = newEnviron (optEnviron insertedCode),
                   decs    = [],
                   recCall = optRec insertedCode
                 })
          end
      end
  end; (* genCode *)


    structure Sharing =
    struct
        type machineWord = machineWord
        type codetree    = codetree
        type pretty      = pretty
        type argumentType=argumentType
    end

end (* CODETREE functor body *);
