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
    val gencode: codetree * Universal.universal list -> unit -> machineWord;
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
  
  val InternalError = Misc.InternalError
  
  infix 9 sub;

  val isConstnt    = fn (Constnt _)    => true | _ => false;
  val isCodeNil    = fn CodeNil        => true | _ => false; (* Exported *)

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
    
    fun optVal (ov as {general, special, decs, ...}) : optVal =
      if isCodeNil special
      then
        case decs of 
          [] => JustTheVal general
        | _  => ValWithDecs {general = general, decs = decs}
          else OptVal ov;

  (* minor HACKS *)
  type casePair = codetree * int list;
  
  val codegen = gencode;

  (* gets a value from the run-time system *)
  val ioOp : int -> machineWord = RunCall.run_call1 POLY_SYS_io_operation;

  fun apply _ [] = () | apply f (h::t) = (f h; apply f t);

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

  type evalForm = 
     { (* Evaluate a function with an argument list. *)
       function:  codetree,
       argList:   codetree list,
       earlyEval: bool
     }

  and declarForm = 
     { (* Declare a value or push an argument. *)
       value:      codetree,
       addr:       int,
       references: int
      }

   and indForm = 
     { (* Indirect off a value. *)
       base:   codetree,
       offset: int
      }

   and diadic = 
     codetree * codetree

   and triadic = 
     codetree * codetree * codetree

   and lambdaForm =
     { (* Lambda expressions. *)
       body          : codetree,
       isInline      : inlineStatus,
       name          : string,
       closure       : codetree list,
       numArgs       : int,
       level         : int,
       closureRefs   : int,
       makeClosure   : bool
      } 
   
   and casePair = 
       (* Expressions and corresponding list of labels. *)
       codetree * int list
   
   and caseForm =
     { (* Case expressions *)
       cases   : (codetree * int list) list,
       test    : codetree,
       default : codetree,
       min     : int,
       max     : int
      }
   
   and handleForm =
     { (* Exception handler. *)
       exp     : codetree,
       taglist : codetree list,
       handler : codetree
     };

(*************************** end of copied code *****************************)  
  

  fun mkAltMatch (m1, m2) = AltMatch (m1, m2);

  fun mkDecRef ct i1 i2 = Declar  {value = ct, addr = i1, references = i2};
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
    Extract {level = level, addr = addr, fpRel = true, lastRef = false};
  fun mkClosLoad addr last =
    Extract {level = 0, addr = addr, fpRel = false, lastRef = last};
  val mkEnv    = Newenv;

  (* Wrap up multiple entries.  Return a single item unless it is a
     declaration. *)
  fun wrapEnv (l as [Declar _]) = mkEnv l
    | wrapEnv (l as [MutualDecs _]) = mkEnv l
    | wrapEnv [singleton] = singleton
    | wrapEnv multiple = mkEnv multiple
  
  (* generates a declaration operation *)
  fun mkDec (laddr, res) = mkDecRef res laddr 0;


  (* lambda operation: returns a procedure *)
  fun mkProc (lval, level, args, name) =
    Lambda
      {
        body          = lval,
        isInline      = (*if isSmall lval then SmallFunction else *) NonInline,
        name          = name,
        closure       = [],
        numArgs       = args,
        level         = level,
        closureRefs   = 0,
        makeClosure   = false
      }                     

  (* inline lambda operation: returns an inline procedure *)
  fun mkInlproc (lval, level, args, name) =
    Lambda
      {
        body          = lval,
        isInline      = MaybeInline,
        name          = name,
        closure       = [],
        numArgs       = args,
        level         = level,
        closureRefs   = 0,
        makeClosure   = false
      };

  fun mkMacroProc (lval, level, args, name) =
    Lambda
      {
        body          = lval,
        isInline      = OnlyInline,
        name          = name,
        closure       = [],
        numArgs       = args,
        level         = level,
        closureRefs   = 0,
        makeClosure   = false
      };

  fun mkEval (ct, clist, bl)   =
    Eval {function = ct, argList = clist, earlyEval = bl};

  fun mkNot arg = mkEval (rtsFunction POLY_SYS_not_bool, [arg], true);


  val testptreqFunction    = rtsFunction POLY_SYS_word_eq
  val testptrneqFunction   = rtsFunction POLY_SYS_word_neq
  
  fun mkTestptreq  (xp1, xp2) = mkEval (testptreqFunction, [xp1,xp2], true);
  fun mkTestptrneq (xp1, xp2) = mkEval (testptrneqFunction, [xp1,xp2], true);
  fun mkTestnull xp1       = mkTestptreq  (xp1, CodeZero);
  fun mkTestnotnull xp1    = mkTestptrneq (xp1, CodeZero);

  val mkIf = Cond ;
  fun mkWhile(b, e) =
    (* Generated as   if b then (e; <loop>) else (). *)
    BeginLoop(mkIf(b, mkEnv[e, Loop[]], CodeZero), [])

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
    mkEval (rtsFunction POLY_SYS_int_eq, [xp1,xp2], true);

  fun mkHandle (exp, taglist, handler) = Handle {exp = exp, taglist = taglist, handler = handler};
  
  val mkRaise      = Raise;
  val mkMutualDecs = MutualDecs;

  fun mkStr (strbuff:string) = mkConst (toMachineWord strbuff);

  val mkContainer  = Container

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
    mkInlproc (mkLoad (~1, 0), 0, 1, name) (* Returns its argument. *);

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
  fun evalue (Constnt c) : machineWord = c
   |  evalue (Global g) : machineWord = evalue(optGeneral g)
   |  evalue _ =
        raise InternalError "evalue: Not a constant"

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
     POLY_SYS_get_flags, (* POLY_SYS_alloc_store, - can raise Size *)
     POLY_SYS_teststreq, POLY_SYS_teststrneq, POLY_SYS_teststrgtr,
     POLY_SYS_teststrlss, POLY_SYS_teststrgeq, POLY_SYS_teststrleq,
     POLY_SYS_is_short, POLY_SYS_aplus, POLY_SYS_aminus, POLY_SYS_amul,
     POLY_SYS_aneg, POLY_SYS_xora,
     POLY_SYS_equala, POLY_SYS_ora, POLY_SYS_anda,
     POLY_SYS_Real_str, POLY_SYS_Real_geq, POLY_SYS_Real_leq,
     POLY_SYS_Real_gtr, POLY_SYS_Real_lss, POLY_SYS_Real_eq,
     POLY_SYS_Real_neq, POLY_SYS_Add_real, POLY_SYS_Sub_real,
     POLY_SYS_Mul_real, POLY_SYS_Div_real, POLY_SYS_Neg_real,
     POLY_SYS_sqrt_real, POLY_SYS_sin_real, POLY_SYS_cos_real,
     POLY_SYS_arctan_real, POLY_SYS_exp_real, POLY_SYS_ln_real,
     POLY_SYS_io_operation, POLY_SYS_shift_right_arith_word,
     POLY_SYS_is_big_endian, POLY_SYS_bytes_per_word,
     POLY_SYS_shift_right_word, POLY_SYS_word_neq, POLY_SYS_not_bool,
     POLY_SYS_string_length, POLY_SYS_int_eq, POLY_SYS_int_neq,
     POLY_SYS_int_geq, POLY_SYS_int_leq, POLY_SYS_int_gtr, POLY_SYS_int_lss,
     POLY_SYS_mul_word, POLY_SYS_plus_word,
     POLY_SYS_minus_word, POLY_SYS_or_word,
     POLY_SYS_and_word, POLY_SYS_xor_word, POLY_SYS_shift_left_word,
     POLY_SYS_word_geq, POLY_SYS_word_leq,
     POLY_SYS_word_gtr, POLY_SYS_word_lss, POLY_SYS_word_eq,
     POLY_SYS_load_byte, POLY_SYS_load_word, POLY_SYS_get_first_long_word]
  val divisionOperations = map ioOp
   [POLY_SYS_adiv, POLY_SYS_amod, POLY_SYS_div_word, POLY_SYS_mod_word]

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
    | sideEffectFree (Handle { exp, taglist, handler }) =
          sideEffectFree exp andalso 
          testList taglist andalso
          sideEffectFree handler
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
        andalso testList argList

    | sideEffectFree(Container _) = true
        (* But since SetContainer has a side-effect we'll always create the
           container even if it isn't used.  *)

    | sideEffectFree(TupleFromContainer(c, _)) = sideEffectFree c

    | sideEffectFree _ = false
             (* Rest are unsafe (or too rare to be worth checking) *)

  and testList []       = true
    | testList (h :: t) = sideEffectFree h andalso testList t

  and sideEffectFreeRTSCall(function: machineWord, args: codetree list): bool =
    let
      fun isInList(ioCall, sofar) = sofar orelse wordEq (function, ioCall)
    in
      List.foldl isInList false safeRTSCalls orelse
        (* Division operations are safe if we know that the second argument
           is not zero. If it's long it can't be zero and we can't have
           long arguments for the word operations. *)
        (List.foldl isInList false divisionOperations andalso
            (case args of
               [_, Constnt c] => not (isShort c) orelse toShort c <> 0w0
             | _ => false)
        )
    end;


(************************************************************************)
(*    earlyRtsCall                                                      *)
(************************************************************************)
(* Tests whether an RTS call in which all the arguments are constants can
   be evaluated immediately.  Normally this will be clear from the RTS
   call itself but in a few cases we need to look at the arguments.
   It's quite safe to evaluate a function which results in an exception.
   It isn't safe to evaluate a function which might have a side-effect. *)

   fun earlyRtsCall(function: machineWord, args: codetree list): bool =
   let
      val safeForImmutable =
        [POLY_SYS_get_flags, POLY_SYS_load_byte, POLY_SYS_load_word]
      val safeCalls =
        [POLY_SYS_get_length,
         POLY_SYS_teststreq, POLY_SYS_teststrneq, POLY_SYS_teststrgtr,
         POLY_SYS_teststrlss, POLY_SYS_teststrgeq, POLY_SYS_teststrleq,
         POLY_SYS_is_short, POLY_SYS_aplus, POLY_SYS_aminus, POLY_SYS_amul,
         POLY_SYS_adiv, POLY_SYS_amod, POLY_SYS_aneg, POLY_SYS_xora,
         POLY_SYS_equala, POLY_SYS_ora, POLY_SYS_anda,
         POLY_SYS_Real_str, POLY_SYS_Real_geq, POLY_SYS_Real_leq,
         POLY_SYS_Real_gtr, POLY_SYS_Real_lss, POLY_SYS_Real_eq,
         POLY_SYS_Real_neq, POLY_SYS_Add_real, POLY_SYS_Sub_real,
         POLY_SYS_Mul_real, POLY_SYS_Div_real, POLY_SYS_Neg_real,
         POLY_SYS_conv_real, POLY_SYS_real_to_int, POLY_SYS_int_to_real,
         POLY_SYS_sqrt_real, POLY_SYS_sin_real, POLY_SYS_cos_real,
         POLY_SYS_arctan_real, POLY_SYS_exp_real, POLY_SYS_ln_real,
         POLY_SYS_io_operation, POLY_SYS_shift_right_arith_word,
         POLY_SYS_is_big_endian, POLY_SYS_bytes_per_word,
         POLY_SYS_shift_right_word, POLY_SYS_word_neq, POLY_SYS_not_bool,
         POLY_SYS_string_length, POLY_SYS_int_eq, POLY_SYS_int_neq,
         POLY_SYS_int_geq, POLY_SYS_int_leq, POLY_SYS_int_gtr, POLY_SYS_int_lss,
         POLY_SYS_mul_word, POLY_SYS_plus_word,
         POLY_SYS_minus_word, POLY_SYS_div_word, POLY_SYS_or_word,
         POLY_SYS_and_word, POLY_SYS_xor_word, POLY_SYS_shift_left_word,
         POLY_SYS_mod_word, POLY_SYS_word_geq, POLY_SYS_word_leq,
         POLY_SYS_word_gtr, POLY_SYS_word_lss, POLY_SYS_word_eq,
         POLY_SYS_get_first_long_word]

      fun isInList(ioCall, sofar) = sofar orelse wordEq (function, ioOp ioCall)
      fun isImmutable (Constnt w, sofar) =
            sofar andalso (isShort w orelse not(isMutable(toAddress w)))
        | isImmutable _ = raise InternalError "isImmutable: arg not constant"
    in
      if List.foldl isInList false safeCalls
      then true
      else if List.foldl isInList false safeForImmutable
      then (* These are safe if the first argument is immutable.  If it's
              mutable we might find that the value has changed when we
              come to run the program. *)
         List.foldl isImmutable true args
      else false
    end
  
(************************************************************************)
(*    evaluate                                                          *)
(************************************************************************)
  (* Evaluates expressions by code-generating and running them. *)
  (* "resultCode" is a copied code expression. The result is either *)
  (* a constant or an exception. *)
  fun evaluate (resultCode as Constnt _) _ =
      (* May already have been reduced to a constant. *)
      resultCode

   | evaluate (resultCode as Eval { function=evalFunction, argList, ...}) codegen =
       (* It's a function call - generate a call. This should only be
          as a result of "early" evaluation when all the arguments are
          constants or inline procedures. *)
    (
      case evaluate evalFunction codegen of
        function as Raise _ => (* Could be an exception. *)
            function

        | function =>
      let (* Evaluate each argument. *)
(* NB This version of loadArgs is DIFFERENT from the old Poly version.
      The Poly version stores the arguments in reverse order, then uses
      "callcode" (run-time call POLY_SYS_callcode = 16) to apply the function.
      The ML version stores the parameters in the usual order, then uses
      "callcode_tupled" (run-time call POLY_SYS_callcode_tupled = 204) to apply
      the function. [The other difference is that "callcode" expects its
      two arguments in registers (Poly calling convention) but "callcode_tupled"
      expects to receive an ML pair (ML calling convention).] These interface
      differences are actually implicit in the fact that we use Address.call
      rather than address$call.  SPF 8/7/94
*)

       val funcAddress =
        case function of
            Constnt addr =>
              if isShort addr
               then raise InternalError "Code address is not an address"
               else toAddress addr
          | _ => raise InternalError "Code address is not a constant";
       
       (* Finished loading the args; call the function. This may raise *)
       (* an exception, in which case we just return the original code *)
       (* rather than trying to sort out the exception packet.         *)
       (* We would have a problem if the code we were executing could  *)
       (* raise Interrupt ("is it the user, or is it the code?") but   *)
       (* this isn't a problem because we never execute an explicit    *)
       (* "raise" expression and none of the built-in functions can    *)
       (* raise Interrupt.                                             *)
       fun callFunction (argTuple:machineWord) = 
            mkConst (call (funcAddress, argTuple))
              handle SML90.Interrupt => raise SML90.Interrupt (* Must not handle this *)
                   | _         => resultCode

       fun loadArgs (argVec : address) ([]:codetree list) _ =
          (
            lock argVec;
            callFunction (toMachineWord argVec)
          )
               
         | loadArgs (argVec : address) (h :: t) locn =
            (
            case evaluate h codegen of
                arg as Raise _ => arg
            (* if argument is a "raise" expression, so is final result *)
              | Constnt cv =>
                 ( 
                  assignWord (argVec, toShort locn, cv);
                  loadArgs argVec t (locn + 1)
                 )
              | _ => raise InternalError "Result of evaluate is not a constant"
            )
      in
        case argList of
          []      => callFunction word0  (* empty tuple - don't allocate *)
          
        | argList =>
           let 
             val argVec = alloc (toShort (List.length argList), F_mutable_words, word0);
           in
             loadArgs argVec argList 0
           end
      end
    )
    
   | evaluate resultCode codegen =
   (* Compile the expression, evaluate it, and catch any exceptions. *)
    let
      val compiledCode = codegen resultCode;
    in
      mkConst (compiledCode ())
          handle SML90.Interrupt => raise SML90.Interrupt (* Must not handle this *)
               | _         => resultCode
    end (* evaluate *);
    
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
  fun preCode (codegen, pt) =
  let
    val initTrans = 5; (* Initial size of arrays. *)

    (* preCode.copyCode *)
    fun copyCode (pt, previous, argUses, closureUses) =
    let 
       
      (* Tables for local declarations. "localUses" is the use count,
         and "closuresForLocals" a flag indicating that if the declaration
         is a procedure a closure must be made for it. *)
      val localUses         = stretchArray (initTrans, false);
      val closuresForLocals = stretchArray (initTrans, false);

      (* used to indicate whether a local declaration is really
         a constant, so that we can in-line it. SPF 16/5/95 *)
      val localConsts       = stretchArray (initTrans, NONE);

      abstype usageSet =
        UsageSet of {locals: int list ref, args: int list ref, clos: bool ref}
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
            let
                fun tabulate(size, vec) =
                let
                    fun tabul n l =
                        if n = size
                        then l
                        else if StretchArray.sub(vec, n)
                        then tabul (n+1) (n::l)
                        else tabul (n+1) l
                in
                    tabul 0 []
                end
                val localLength = StretchArray.length localUses
                and argLength = StretchArray.length argUses
                val localSaved = tabulate(localLength, localUses)
                and argSaved = tabulate(argLength, argUses)
            in
                UsageSet{locals=ref localSaved, args=ref argSaved, clos=ref(!closureUses)}
            end

          (* Restore the table to the saved values. *)
          fun setToSaved(UsageSet{locals=ref locals, args=ref args, clos}): unit =
            let
                fun copyArray i f t =
                    if i < 0
                        (* Put in a check here temporarily. *)
                    then (case f of [] => () | _ => raise InternalError "setToSaved: not empty")
                    else case f of
                        [] =>
                            (
                                StretchArray.update(t, i, false);
                                copyArray (i-1) [] t
                            )
                    |   head :: tail =>
                        if head = i
                        then 
                            (
                                StretchArray.update(t, i, true);
                                copyArray (i-1) tail t
                            )
                        else
                            (
                                StretchArray.update(t, i, false);
                                copyArray (i-1) f t
                            )
            in
                copyArray (StretchArray.length argUses -1) args argUses;
                copyArray (StretchArray.length localUses -1) locals localUses;
                closureUses := !clos
            end;

          (* Similar to setToSaved except that it sets the current set
             to the union of the current set and the saved set. *)
          fun addFromSaved(UsageSet{locals=ref locals, args=ref args, clos}): unit =
            let
                fun addArray [] _ = ()
                 |  addArray (head::tail) t =
                        (
                        StretchArray.update(t, head, true);
                        addArray tail t
                        )
            in
                addArray args argUses;
                addArray locals localUses;
                if !clos then closureUses := true else ()
            end;

          fun inSet(UsageSet{locals=ref locals, args=ref args, clos}, addr, level) =
            if level > 0 then !clos
            else if addr < 0
            then (* Argument *) List.exists(fn n => n = ~addr) args
            else (* Local *) List.exists(fn n => n = addr) locals;

          fun removeItem(UsageSet{locals, args, clos}, addr, level) =
                if level > 0 then clos := false
                else if addr < 0
                then args := List.filter (fn n => n <> ~addr) (!args)
                else locals := List.filter (fn n => n <> addr) (!locals)

          (* Differences of two sets, used as kill entries.
             The differences are returned as Extract codetree entries. *)
          fun computeKillSets(
                (* then*) UsageSet{locals=ref thenLoc, args=ref thenArgs, clos=thenClos},
                (* else *) UsageSet{locals=ref elseLoc, args=ref elseArgs, clos=elseClos}) =
          let
              fun killSets _ [] [] inThenOnly inElseOnly = (inThenOnly, inElseOnly)

                | killSets f [] (inElseH::inElseT) inThenOnly inElseOnly =
                    killSets f [] inElseT inThenOnly
                         (mkGenLoad(f inElseH, 0, true, true) :: inElseOnly)

                | killSets f (inThenH::inThenT) [] inThenOnly inElseOnly =
                    killSets f inThenT []
                        (mkGenLoad(f inThenH, 0, true, true) ::inThenOnly) inElseOnly

                | killSets f (inThen as inThenH::inThenT)
                             (inElse as inElseH::inElseT) inThenOnly inElseOnly =
                    if inThenH = inElseH
                    then (* In both sets *)
                        killSets f inThenT inElseT inThenOnly inElseOnly
                    else if inThenH < inElseH
                    then (* Only in Else part. *)
                        killSets f inThen inElseT inThenOnly
                         (mkGenLoad(f inElseH, 0, true, true) :: inElseOnly)
                    else killSets f inThenT inElse
                        (mkGenLoad(f inThenH, 0, true, true) ::inThenOnly) inElseOnly
              val (argThen, argElse) =
                 killSets (op ~) thenArgs elseArgs [] []

              val (localThen, localElse) =
                 killSets (fn x=>x) thenLoc elseLoc argThen argElse

              val (closThen, closElse) =
                    if !thenClos = !elseClos then (localThen, localElse)
                    else if !thenClos
                    then (mkClosLoad 0 true :: localThen, localElse)
                    else (localThen, mkClosLoad 0 true :: localElse)

          in
              (closThen, closElse)
          end;
      end;

      (* returns the translated node *)
      fun locaddr (ptr as { addr=laddr, level=lev, ...}: loadForm) (closure : bool) : codetree =
        if lev <> 0 orelse laddr = 0
           then (* non-local *) previous (ptr, lev, closure)
           
        else if laddr < 0
        then let (* parameters *)
          val argNo = ~ laddr;
          val wasInUse = argUses sub argNo;
        in 
          (* Mark the argument as used. *)
          update (argUses, argNo, true);
          mkGenLoad (laddr, 0, true, not wasInUse)
        end
          
        (* isOnstack *)
        else case (localConsts sub laddr) of (* SPF 16/5/95 *)
          SOME c => c (* just return the constant *)
        | NONE =>
          let 
           (* Mark as used and set closure flag if necessary. *)
          val wasInUse = localUses sub laddr
          in
            if closure then update (closuresForLocals, laddr, true) else ();
            update (localUses, laddr, true);
            mkGenLoad (laddr, 0, true, not wasInUse)
          end
      (* locaddr *);

     (* Map f onto a list tail first.  N.B. It doesn't reverse the list.
        Generally used to map "insert" over a list where we need to
        ensure that last references to variables are detected correctly. *)
     fun revmap _ [] = []
       | revmap f (a::b) =
            let
                val rest = revmap f b
            in
                f a :: rest
            end

     (* preCode.copyCode.insert *)
      fun insert (pt:codetree) : codetree =
      let
        (* If "makeClosure" is true the procedure will need a full closure. *)
        (* It may need a full closure even if makeClosure is false if it    *)
        (* involves a recursive reference which will need a closure.        *)
        (* preCode.copyCode.insert.copyLambda *)
        fun copyLambda ({body=lambdaBody, level=nesting, numArgs, isInline,
                         name=lambdaName, ...}: lambdaForm) makeClosure =
        let
          val newGrefs      = ref []; (* non-local references *)
          val newNorefs     = ref 0;  (* number of non-local refs *)
          val refsToClosure = ref false;  (* Number of references to the closure. *)
           
          (* A new table for the new procedure. *)
          (* preCode.copyCode.insert.copyLambda.prev *)
          fun prev (ptr as { addr, fpRel, ...} : loadForm, lev : int, closure: bool) : codetree =
          let 
            (* Returns the closure address of the non-local *)
            (* preCode.copyCode.insert.copyLambda.prev.makeClosureEntry *)
            fun makeClosureEntry [] _ wasRefed = (* not found - construct new entry *)
              let
                val () =
                  newGrefs := mkGenLoad (addr, lev - 1, fpRel, false) ::  !newGrefs;
                val newAddr = !newNorefs + 1;
              in
                newNorefs := newAddr; (* increment count *)
                mkClosLoad newAddr (not wasRefed)
              end
            
            | makeClosureEntry
                (Extract{addr=loadAddr, level=loadLevel, fpRel=loadFpRel, ...} :: t)
                newAddr wasRefed =
                if loadAddr = addr andalso loadLevel = lev - 1 andalso loadFpRel = fpRel
                then mkClosLoad newAddr (not wasRefed)
                else makeClosureEntry t (newAddr - 1) wasRefed

            | makeClosureEntry (_ ::_) _ _ =
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
            then let
              val () =
                if addr <> 0 orelse fpRel
                then raise InternalError "prev: badly-formed load"
                else ();
                
              val () = 
                if closure then makeClosure := true else ();
              val wasRefed = ! refsToClosure
            in
              refsToClosure := true;
              mkClosLoad 0 (not wasRefed)
            end
            
            else if lev = 1 andalso addr > 0
            then let (* local at this level *)
              val () =
               if not fpRel
               then raise InternalError "prev: badly-formed load"
               else ();
            in 
              case localConsts sub addr of
                SOME c => c (* propagate constant, rather than using closure *)
              | NONE  =>
                let
                  val () =
                    if closure 
                    then update (closuresForLocals, addr, true)
                    else ();
                val wasRefed = ! refsToClosure
                in
                   refsToClosure := true;
                   makeClosureEntry (!newGrefs) (!newNorefs) wasRefed
                end
            end
            
            else if lev = 1 andalso addr < 0
            then let (* parameter at this level *)
              val () =
                if not fpRel
                then raise InternalError "prev: badly-formed load"
                else ();
              val wasRefed = ! refsToClosure
            in 
              refsToClosure := true;
              makeClosureEntry (!newGrefs) (!newNorefs) wasRefed
            end
            
            else let (* lev > 1 orelse (lev = 1 andalso addr = 0) *)
              (* Discard the result, unless it's a constant.
                 We fix up the old (fp-relative) address in the
                 closure list on the second pass. Why not now?
                 That would make it harder to set the makeClosure
                 flag for the closure lists of mutually-recursive
                 definitions. But doesn't doing it this way risks
                 making the refsToClosure count too high? SPF 19/5/95 *)
               val outerLoad : codetree = 
                 previous (ptr, lev - 1, closure)
            in
              case outerLoad of
                Constnt _ => outerLoad
                | _ =>
                let
                  val wasRefed = ! refsToClosure
                in
                  refsToClosure := true;
                  makeClosureEntry (!newGrefs) (!newNorefs) wasRefed
                end
            end
          end (* prev *);
                
          (* This could be a fixed array rather than stretchArray.  The
             size is one more than the number of arguments because the
             arguments are numbered from ~1 .. ~n and we use the entries
             as ~arg. *)
          val argUses      = stretchArray (numArgs+1, false);
          
          (* process the body *)
          val bodyCode = copyCode (lambdaBody, prev, argUses, refsToClosure);

          (* Add kill entries for unused arguments.  Typically a function
             taking a unit argument will not use it. *)
          fun addKills n =
             if n > numArgs then nil
             else if not (StretchArray.sub(argUses, n))
             then mkGenLoad(~n, 0, true, true) :: addKills (n+1)
             else addKills (n+1)

          val insertedCode = (* Wrap the lot up if necessary. *)
            case addKills 1 of
                nil => bodyCode
            |   kills => Newenv(kills @ [bodyCode])

        in  (* body of preCode.copyCode.insert.copyLambda *)
          if null (!newGrefs) (* no external references *)
          then let
           (* 
              Since we are code-generating the procedure before we
              have done a full analysis of any mutually-recursive
              functions, we have to conservatively assume that it
              will require a closure, even if all the uses of the
              procedure within its body are kosher. SPF 20/5/95.
              
              We now want recursive calls to be code-generated using
              the saved closure register, rather than by loading the
              address of the closure from the constants vector. This
              means that we can no longer set closureRefs to 0, since
              we will actually be *using* the closure. SPF 20/5/95
            *) 
            val copiedProc =
              Lambda
                {
                  body          = insertedCode,
                  isInline      = isInline,
                  name          = lambdaName,
                  closure       = [],
                  numArgs       = numArgs,
                  level         = nesting,
                  closureRefs   = if !refsToClosure then 1 else 0 (* was set to 0 *), 
                  makeClosure   = true
                };
          in
           (* Code generate it now so we get a constant. *)
            evaluate copiedProc codegen
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
                numArgs       = numArgs,
                level         = nesting,
                closureRefs   = if !refsToClosure then 1 else 0,
                makeClosure   = false
              }
        end (* copyLambda *);

        (* Copy the closure of a procedure which has previously been
            processed by copyLambda. *)
         (* preCode.copyCode.insert.copyProcClosure *)
        fun copyProcClosure (Lambda{ body, isInline, name, numArgs, level,
                                     closureRefs, closure, ...}) makeClosure =
          let
            (* process the non-locals in this procedure *)
            (* If a closure is needed then any procedures referred to
               from the closure also need closures.*)
            fun makeLoads (Extract ext) = locaddr ext makeClosure 
             |  makeLoads _ = raise InternalError "makeLoads - not an Extract"
               
            val copyRefs = rev (map makeLoads closure);
          in
            Lambda
              {
                body          = body,
                isInline      = isInline,
                name          = name,
                closure       = copyRefs,
                numArgs       = numArgs,
                level         = level,
                closureRefs   = closureRefs,
                makeClosure   = makeClosure
              }
           end
        |  copyProcClosure pt _ = pt (* may now be a constant *)
        (* end copyProcClosure *);

      in  (* body of preCode.copyCode.insert *)
        case pt of
            (pt as MatchFail) => pt : codetree
          
        | AltMatch(x, y) =>
            let
                val insY = insert y
                val insX = insert x
            in
                AltMatch (insX, insY) : codetree
            end
       
        | CodeNil => CodeNil
        
        | Eval { function, argList, ...} =>
            let
              (* Process the arguments first. *)
              val newargs = revmap insert argList
              val func  =
                case function of
                    Extract ext => locaddr ext (* closure = *) false
                  | first => insert first
            in
              (* If we are calling a procedure which has been declared this
                 does not require it to have a closure. Any other use of the
                 procedure would. *) 
              mkEval (func, newargs, false)
            end : codetree

        | Extract ext =>
          (* Load the value bound to an identifier. The closure flag is
             set to true since the only cases where a closure is not needed,
             eval and load-andStore, are handled separately. *)
          locaddr ext (* closure = *) true  : codetree

        | Indirect {base, offset} => Indirect {base = insert base, offset = offset}

        | pt as Constnt _ => 
            pt : codetree (* Constants can be returned untouched. *)

        | BeginLoop(body, argList) => (* Start of tail-recursive inline function. *)
            let
                (* Make entries in the tables for the arguments. I'm not sure
                   if this is essential. *)
                fun declareArg(Declar{addr=caddr, ...}) =
                    (
                    update (localUses, caddr, false);
                    update (closuresForLocals, caddr, false);
                    update (localConsts, caddr, NONE)
                    )
                |   declareArg _ = raise InternalError "declareArg: not a declaration"
                val _ = List.app declareArg  argList

                (* Process the body. *)
                val insBody = insert body
                (* Then the initial argument values. *)
                fun copyDec(Declar{addr, value, ...}) =
                        (
                        (* Reset the uses for this entry since it's local. *)
                        update (localUses, addr, false);
                        mkDecRef (insert value) addr 1
                        )
                  | copyDec _ = raise InternalError "copyDec: not a declaration"
                val newargs = revmap copyDec argList
                (* TODO: Perhaps we should modify this so that any "last
                   references" we find in the loop body are moved out to
                   beyond the loop. *)
            in
                BeginLoop(insBody, newargs)
            end
    
        | Loop argList => (* Jump back to start of tail-recursive function. *)
            Loop(revmap insert argList)

        | Raise x => Raise (insert x) : codetree

        (* See if we can use a case-instruction. Arguably this belongs
           in the optimiser but it is only really possible when we have
           removed redundant declarations. *)
        | Cond (condTest, condThen, condElse) =>
            copyCond (condTest, condThen, condElse)

        | Newenv ptElist =>
        let
                
          (* Process the body. Recurses down the list of declarations
             and expressions processing each, and then reconstructs the
             list on the way back. *)
         (* body of preCode.copyCode.insert(isNewenv).copyDeclarations *)
         fun copyDeclarations ([]: codetree list) : codetree list  = []

           | copyDeclarations ((Declar{addr=caddr, value = pt, ...}) :: vs) : codetree list =
            let
              (* Set the table entries. *)
              (* DCJM 1/12/99.  I think the reason for this is in case we have
                 reused the address in a different block. *)
              val () = update (localUses, caddr, false); (* needed? *)
              val () = update (closuresForLocals, caddr, false);

              val () =
                case pt of
                    Constnt _ => update (localConsts, caddr, SOME pt)
                |  _ => update (localConsts, caddr, NONE); (* needed? *)

              (* This must be done first, even for non-lambdas -  why? *)
              (* The first declarative might be a set of mutual declarations,
                 and we have to copy all their uses before we can successfully
                 compile them because we need to know whether they will
                 require closures. SPF 13/5/95 *)
              val rest = copyDeclarations vs;
              val wasUsed = localUses sub caddr;
            in
                  (* It is never used and it has no side-effects
                     so we can ignore it. *)
               if not wasUsed andalso sideEffectFree pt
               then rest
               else let
                  val dec =
                    case pt of
                        Lambda lam =>
                      let 
                        val closure      = ref (closuresForLocals sub caddr);
                        val copiedLambda = copyLambda lam closure;
                      in
                        (* Note: copyLambda may have set closure *)
                        copyProcClosure copiedLambda (! (closure))
                      end
                    | _ => insert pt
                in
                   (* Set the use count back to free otherwise this local
                      declaration would become part of the kill set for the
                      surrounding expression. *)
                   update (localUses, caddr, false);
                   mkDecRef dec caddr (if wasUsed then 1 else 0) :: rest
                end
            end (* copyDeclarations.isDeclar *)

           | copyDeclarations (MutualDecs mutualDecs :: vs) : codetree list =
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
                  
              fun applyFn (Declar{addr=caddr, value=dv, ...}) =     
                (
                  (* SPF 16/5/95 *)
                  case dv of
                    Constnt _ => update (localConsts, caddr, SOME dv) 
                  | _ => ();
                   
                  update (localUses, caddr, false);
                  update (closuresForLocals, caddr, false)
                )
                | applyFn _ = raise InternalError "applyFn: not a Declar"               
                  
              val () = apply applyFn mutualDecs;
                      
              (* Process the rest of the block. Identifies all other
                 references to these declarations. *)
              val restOfBlock = copyDeclarations vs;
                      
              (* We now want to find out which of the declarations require
                 closures. First we copy all the declarations, except that
                 we don't copy the non-local lists of procedures. *)
              fun copyDec (Declar{addr=caddr, value=dv, ...}) = 
              let
                val closure = ref (closuresForLocals sub caddr);
                val dec     =
                    case dv of
                        Lambda lam => copyLambda lam closure
                      | _ => insert dv;
                (* SPF 18/5/95 - check whether we now have a constant *)
                val () =
                    case dec of
                        Constnt _ => update (localConsts, caddr, SOME dec)
                      | _ => update (localConsts, caddr, NONE); (* needed? *)

                (* copyLambda may set "closure" to true. *)
                 val () =
                   update (closuresForLocals, caddr, !closure);
               in
                 mkDec (caddr, dec)
               end
                | copyDec _ = raise InternalError "copyDec: not a Declar"               

              val copiedDecs = map copyDec mutualDecs;
                       
              (* We now have identified all possible references to the
                 procedures apart from those of the closures themselves.
                 Any of closures may refer to any other procedure so we must 
                 iterate until all the procedures which need full closures
                 have been processed. *)
              fun processClosures [] outlist true =
                  (* Sweep completed. - Must repeat. *)
                  processClosures outlist [] false
                
                | processClosures [] outlist false =
                (* We have processed the whole of the list without finding
                  anything which needs a closure. The remainder do not
                  need full closures. *)
                let
                  fun mkLightClosure (Declar{value, addr, ...}) = 
                    let
                      val clos = copyProcClosure value false;
                    in
                      mkDec (addr, clos)
                    end
                   | mkLightClosure _ = 
                        raise InternalError "mkLightClosure: not a Declar"               
                in
                  map mkLightClosure outlist
                end
                      
               | processClosures ((h as Declar{addr=caddr, value, ...})::t) outlist someFound =
                  if closuresForLocals sub caddr 
                  then let (* Must copy it. *)
                    val clos = copyProcClosure value true
                  in
                    mkDec (caddr, clos) :: processClosures t outlist true
                  end
                  
                  (* Leave it for the moment. *)
                  else processClosures t  (h :: outlist) someFound

               | processClosures _ _ _ =
                    raise InternalError "processClosures: not a Declar"               
              
              (* Now we know all the references we can complete
                 the declaration and put on the use-count. *)
              fun copyEntries []      = []
               |  copyEntries (Declar{ addr, value, ...} ::ds) =
                let
                  val wasUsed = localUses sub addr;
                in
                  if not wasUsed andalso sideEffectFree value
                  then copyEntries ds
                  else 
                     (
                     (* Set the use count back to false otherwise this
                        entry would become part of the kill set for the
                        surrounding expression. *)
                     update(localUses, addr, false);
                     mkDecRef value addr (if wasUsed then 1 else 0) :: copyEntries ds
                     )
                end
               |  copyEntries (_::_) =
                    raise InternalError "copyEntries: Not a Declar";
                      
              val decs = copyEntries (processClosures copiedDecs [] false);
            in
              (* Return the mutual declarations and the rest of the block. *)
              case decs of
                []   => restOfBlock         (* None left *)
              | [d]  => d :: restOfBlock    (* Just one *)
              | _    => mkMutualDecs decs :: restOfBlock
            end (* copyDeclarations.isMutualDecs *)
              
           | copyDeclarations (v :: vs) : codetree list =
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
               | _ => copiedNode :: copiedRest
            end (* copyDeclarations *);

         (* Can we optimise a tuple to a constant? It would be nice
             to adjust the local use counts when we find a constant,
             but I think we're already committed to the values we
             found on the previous pass. SPF 13/6/95. *)
         fun recopyValue (Recconstr recs) : codetree = 
            mkTuple (map recopyValue recs)
           
          | recopyValue (v as Extract{level=lev, addr=laddr, ...}) : codetree = 
             if lev = 0 andalso laddr > 0
             then 
               case (localConsts sub laddr) of
                 SOME c => c
               | NONE   => v
             else v
          | recopyValue v = v;

         fun recopyDeclaration (Declar{addr=caddr, value=pt as Recconstr _, references}) : codetree = 
             let
               val pt' = recopyValue pt
             in
               if isConstnt pt'
               then update (localConsts, caddr, SOME pt')
               else update (localConsts, caddr, NONE); (* needed? *)
               mkDecRef pt' caddr references
             end
     
          | recopyDeclaration (v as Declar _) : codetree = v

          | recopyDeclaration (MutualDecs decs) : codetree = 
             mkMutualDecs (map recopyDeclaration decs)
             
          | recopyDeclaration (v : codetree) : codetree = recopyValue v;

          (* recopy declarations to remove constant tuples SPF 13/6/95 *)
          val insElist = map recopyDeclaration (copyDeclarations ptElist);
        in
          (* If there is only one item then return that item (unless it is
            a declaration - this can occur if we have a block which contains
            a declaration to discard the result of a function call and 
            only do its side-effects). *)
          wrapEnv insElist
        end : codetree (* isNewEnv *)
                
        | Recconstr recs => (* perhaps it's a constant now? *)
          (* Recconstr (map insert (cRecconstr pt)) : codetree *)
             mkTuple (revmap insert recs) : codetree 

        | (pt as Ldexc) => pt : codetree (* just a constant so return it *)
      
        | Lambda lam =>
           (* Must make a closure for this procedure because
              it is not a simple declaration. *)
           copyProcClosure (copyLambda lam (ref true)) true : codetree
     
        | Handle { exp, taglist, handler } =>
            let
                (* The order here is important.  We want to make sure that
                   the last reference to a variable really is the last. *)
               val hand = insert handler;
               val exp = insert exp;
               val tags = map insert taglist
            in
              Handle {exp = exp, taglist = tags, handler = hand}
            end
      
        | Case { cases, test, default, min, max } =>
            let
              fun insertCasePair ((e,l) : casePair) : casePair =
                (insert e, l);
            in
              Case
                {
                  cases   = revmap insertCasePair cases,
                  default = insert default,
                  test    = insert test, (* Must be called last. *)
                  min     = min,
                  max     = max
                }
            end : codetree

        | c as Container _ => c

        | SetContainer {container, tuple, size} =>
            SetContainer{container = insert container, tuple = insert tuple, size = size}

        | TupleFromContainer(container, size) =>
            TupleFromContainer(insert container, size)
         
        | Global g =>
           (* Should have been taken off by the optimiser. *)
           optGeneral g : codetree

        | _ => raise InternalError "unknown instruction"

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
          val (killThenOnly, killElseOnly) =
            computeKillSets(thenUsage, elseUsage);
          (* Now ensure that all the variables that were used in the
             then-part are marked as used.  It may be that they have already
             been set if they also appeared in the else-part.
             This sets the usage sets to the union of the then-part,
             the else-part and code after the if-expression. *)
          val () = addFromSaved thenUsage;

          (* Add kill entries to the other branch.  We simply add
             Extract entries with lastRef=true before the appropriate
             branch.  This does what we want since the code-generator
             does not generate any code for them but it might make
             the intermediate code easier to read if we used a different
             instruction. *)
          fun addKillSet(original, []) = original (* No change *)
           |  addKillSet(Newenv decs, killSet) = Newenv(killSet @ decs)
           |  addKillSet(original, killSet) = Newenv(killSet @ [original]);

          (* Process the condition AFTER the then- and else-parts. *)
          val insFirst = insert condTest;
          
          datatype caseVal =
             NoCaseVal
           | CaseVal of
              { tags: int list, 
                min:  int, 
                max:  int, 
                test: codetree };
        
          (* True if both instructions are loads or indirections with the
             same effect. More complicated cases could be considered but
             function calls must always be treated as different.
             Now returns the variable, choosing the one which has lastRef set
             if possible.  Note: the reason we consider Indirect entries here
             as well as Extract is because we (used to) defer Indirect entries.  *)
          datatype similarity = Different | Similar of loadForm;

          fun similar (Extract (a as {addr=aAddr, level=aLevel, fpRel=aFpRel, lastRef=aRef}))
                      (Extract (b as {addr=bAddr, level=bLevel, fpRel=bFpRel, lastRef=_})) =
                if aAddr = bAddr andalso aLevel = bLevel andalso aFpRel = bFpRel
                then if aRef then Similar a else Similar b
                else Different
              
           |  similar (Indirect{offset=aOff, base=aBase})
                      (Indirect{offset=bOff, base=bBase}) =
                if aOff <> bOff then Different else similar aBase bBase
              
           |  similar (_:codetree) (_:codetree) = Different;

         (* If we have a call to the int equality operation *)
         (* then we may be able to use a case statement. *)
         (* preCode.copyCode.insert.findCase *)
         fun findCase (Eval{ function=Constnt cv, argList, ... }) : caseVal =
              (* Since we are comparing for equality with constants we
                 can do a case for short integers (tags) or arbitrary
                 precision integers.  This will certainly work with the
                 new (Sparc, Mips, I386) code-generator. *)
                 
   (* The above comment is an oversimplification, because we can't be
      sure that the constant is a short. If it isn't, then word equality
      is *not* the same as integer equality, so I've added a test for
      this. I've also added code for all three types of equality test:
          int_eq   229 (used for case tags - but not necessarily short)
          equala   113 (arbitrary precision arithmetic)
          word_eq  251 (used for shorts)
      rather than just for the first two, since although the last case
      may not arise (says who?) it does no harm to test for it. SPF 12/10/94 *)
              if wordEq (cv, ioOp POLY_SYS_int_eq) orelse
                  wordEq (cv, ioOp POLY_SYS_equala) orelse
                  wordEq (cv, ioOp POLY_SYS_word_eq)
              then  (* Should be just two arguments. *)
                case argList of
                  [Constnt c1, arg2] =>
                    if isShort c1
                    then let
                      val value : int = Word.toIntX (toShort c1);
                    in
                      CaseVal{tags=[value], min=value, max=value, test=arg2}
                    end
                    else NoCaseVal (* Not a short constant. *)
                    
                 | [arg1, Constnt c2] =>
                    if isShort c2
                    then let
                      val value : int = Word.toIntX (toShort c2);
                    in
                      CaseVal{tags=[value], min=value, max=value, test=arg1}
                    end
                    else NoCaseVal (* Not a short constant. *)
                    
                | _ => NoCaseVal
                   (* Wrong number of arguments - should raise exception? *)
                
              else NoCaseVal (* Function is not a comparison. *)
            
         |  findCase (Case{cases, min=minC, max=maxC, test, default}) : caseVal =
            (* If we have an expression like x = 1 cor x = 2
               we can make a case-expression out of it. *)
            (* This is more complicated now that we've removed
               conditional-or and replaced it by a normal conditional.
               If the first expression is suitable for inclusion in the
               case it will already have been converted into a case. *)
            let
              (* To match the other cases we need to have the same
                 test variable, all the cases must return CodeTrue and
                 the default must be of the form caseVar = constant.
                 i.e. it is of the form
                    case x of n1 => true | n2 => true | _ => x = n3.
               *)
              val defCase: caseVal = findCase default

              (* Extract the tags and check that the results are all "true". *)
              fun checkCases [] = SOME []
                | checkCases ((c, tags) :: t) =
                    if
                        case c of
                            Constnt w => wordEq(w, True)
                        |   _  => false
                    then
                        case checkCases t of
                            NONE => NONE
                          | SOME tags' => SOME(tags @ tags')
                    else NONE

            in 
              case (defCase, checkCases cases) of
                (CaseVal{test=testDef, min=minDef, max=maxDef, tags=tagsDef},
                 SOME newTags) =>
                  if similar testDef test <> Different
                  then
                     CaseVal{tags = newTags @ tagsDef, min=Int.min(minC, minDef),
                             max=Int.max(maxC, maxDef),
                             (* Return the default test rather than the original because
                                that might contain the last reference. *)
                             test=testDef}
                  else NoCaseVal
              | _ => NoCaseVal
            end (* isCor *)

         |  findCase _ = NoCaseVal (* Neither of those instructions *) 
           (* end findCase *);
        
          val testCase  : caseVal  = findCase insFirst;
          (*val insSecond : codetree = insThen;
          val insThird  : codetree = insElse; *)
        in  (* body of preCode.copyCode.insert(isCond) *)

          case testCase of
            NoCaseVal => (* Can't use a case *)
                mkIf (insFirst,
                      addKillSet(insThen, killElseOnly),
                      addKillSet(insElse, killThenOnly))
            | CaseVal { tags=caseTags, min=caseMin, max=caseMax, test=caseTest } =>

            (* Can use a case. Can we combine two cases?
              If we have an expression like 
                   "if x = a then .. else if x = b then ..."
              we can combine them into a single "case". *)
            case insElse of
                Case { cases=nextCases, min=nextMin, max=nextMax, test=nextTest,
                       default=nextDefault } =>
                (
                case similar nextTest caseTest of
                  (* Note - it is legal (though completely redundant) for the
                     same case to appear more than once in the list. This is not
                      checked for at this stage. *)
                  (* We have to take care to get the "last-reference" flags
                     and the kill-set right when "nextTest" is the last
                     reference to the variable.  (N.B. This means that it does
                     not appear anywhere else in the case). 
                     There are two cases to consider:
                     1. If the then-part contains the variable
                        e.g. if a = 1 then a<lastRef>
                             else (case a<lastRef> of 2 => x | _ => y)
                        Note: because the last reference for a appears in
                        both the branches it will not be in either kill set.
                     2. If the then-part does not contain the variable
                        e.g. if a = 1 then x
                             else (case a<lastRef> of 2 => x | _ => y)
                        Note: because the last reference to a appears only
                        in the else-part it will be in killElseOnly.
                     In 1 we transform this into
                        case a of 1 => a<lastRef> 
                        | 2 => <kill a> x | 3 => <kill a> y
                     i.e. the test is NOT the last reference but a must be
                     killed on the other branches.
                     In 2 we transform it into
                         case a<last> of 1 => x | 2 => y | 3 => z
                     i.e. the test IS the last reference.
                   *)
                 Similar({lastRef = true, addr, level, ...}) =>
                    let (* Contains the last reference to the test variable. *)
                    (* Remove this variable from the usage table for the else-part
                       since we are lifting it up to the test. *)
                    val () = removeItem(elseUsage, addr, level);
                    (* Compute the new kill sets. *)
                    val (killThenOnly, killElseOnly) =
                        computeKillSets(thenUsage, elseUsage);
                    in
                    if inSet(thenUsage, addr, level)
                    then (* Must be in the then-part. *)
                      Case 
                        {
                          cases   = (addKillSet(insThen, killElseOnly), caseTags) ::
                                        map (fn (c, l) =>
                                            (addKillSet(c, killThenOnly), l)) nextCases,
                          test    = caseTest, (* This will not be a last-ref *)
                          default = addKillSet(nextDefault, killThenOnly),
                          min     = Int.min(caseMin, nextMin),
                          max     = Int.max(caseMax, nextMax)
                        }
                    else (* It was in killElseOnly *)
                      Case 
                        {
                          cases   = (addKillSet(insThen, killElseOnly), caseTags) ::
                                        map (fn (c, l) =>
                                            (addKillSet(c, killThenOnly), l)) nextCases,
                          test    = nextTest, (* Last reference *)
                          default = addKillSet(nextDefault, killThenOnly),
                          min     = Int.min(caseMin, nextMin),
                          max     = Int.max(caseMax, nextMax)
                        }
                    end

                | Similar _ =>
                    (* Does not contain the last reference to the test variable or
                       the test variable is non-local.
                       Add the kill sets to appropriate cases. *)
                  Case 
                    {
                      cases   = (addKillSet(insThen, killElseOnly), caseTags) ::
                                map (fn (c, l) =>
                                    (addKillSet(c, killThenOnly), l)) nextCases,
                      test    = nextTest,
                      default = addKillSet(nextDefault, killThenOnly),
                      min     = Int.min(caseMin, nextMin),
                      max     = Int.max(caseMax, nextMax)
                    }
    
                | Different => (* Two case expressions but they test different
                                  variables. We can't combine them. *)
                  Case
                    {
                      cases   = [(addKillSet(insThen, killElseOnly), caseTags)],
                      test    = caseTest,
                      default = addKillSet(insElse, killThenOnly),
                      min     = caseMin,
                      max     = caseMax
                    }
                 )
            | _ => (* insElse is not a case *)
                  (* A nil entry for the default indicates that
                     the case is exhaustive (ML only), so if we are
                     converting an "if" without an "else" (Poly only)
                     we put in a dummy "else". *)
                  (* DCJM 28/11/99.  Not true: I think this comment was
                     left over from an old version of the compiler in which
                     case instructions were actually generated by the ML->codetree
                     code-generator. I think the idea was to optimise the case
                     expression when it was applied to a datatype rather than to
                     integers.  For integers we always have to test that the
                     value is in the range for which we have entries in the indexed
                     case before we try computing the jump.  If we know that the
                     value can only correspond to jump table entries we can avoid
                     that. 
                     At present the Case entries in codetree are only produced
                     here. *)
                  Case
                    {
                      cases   = [(addKillSet(insThen, killElseOnly), caseTags)],
                      test    = caseTest,
                      default =
                        addKillSet(if isCodeNil insElse then
                                   CodeZero else insElse, killThenOnly),
                      min     = caseMin,
                      max     = caseMax
                    }
        end  : codetree (* body of preCode.copyCode.insert(isCond) *)

    in     
      insert pt
    end (* copyCode *);
         
    val insertedCode = 
      copyCode (pt,
                fn _ => raise InternalError "outer level reached in copyCode",
                stretchArray (initTrans, false),
                ref false);
  in
    insertedCode
  end (* preCode *);

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
                  nestingDepth): codetree =
   let
        val locals = stretchArray (5 (* Initial size. *), false);

        fun cleanLambda(myAddr,
            {body, isInline, name, numArgs, level=nestingDepth, ...}) =
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
                    then update(locals, addr, true)
                    else (); (* Recursive *)
                    Extract ext
                )
                else prev(ext, level-1, depth);

            val bodyCode = cleanProc(body, lookup, nestingDepth)
        in
            Lambda{body=bodyCode, isInline=isInline, name=name,
                   closure=[], numArgs=numArgs, level=nestingDepth,
                   closureRefs=0, makeClosure=false}
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
                        val _ = update(locals, addr, false)
                        val processedRest = processDecs rest
                    in
                        (* If this is used or if it has side-effects we
                           must include it otherwise we can ignore it. *)
                        if locals sub addr orelse not (sideEffectFree value)
                        then Declar{value=cleanDec(addr, value), addr=addr,
                                    references=references} :: processedRest
                        else processedRest
                    end

                 |  processDecs(MutualDecs decs :: rest) =
                    let
                        (* Clear the entries just in case the addresses are reused. *)
                        fun setEntry(Declar{addr, ...}) = update(locals, addr, false)
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
                            if not (locals sub addr)
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
                    then (* Local *) update(locals, addr, true)
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

         |  cleanCode (Eval{function, argList, earlyEval}) =
                Eval{function=cleanCode function, argList = map cleanCode argList,
                     earlyEval=earlyEval}

         |  cleanCode(Cond(test, thenpt, elsept)) =
                Cond(cleanCode test, cleanCode thenpt, cleanCode elsept)

         |  cleanCode(BeginLoop(body, argList)) =
            let
                val processedBody = cleanCode body
                fun copyDec(Declar{addr, value, ...}) =
                        mkDec(addr, cleanCode value)
                  | copyDec _ = raise InternalError "copyDec: not a declaration"
                val newargs = map copyDec argList
            in
                BeginLoop(processedBody, newargs)
            end

         |  cleanCode(Loop args) = Loop(map cleanCode args)

         |  cleanCode(Raise r) = Raise(cleanCode r)

         |  cleanCode(Ldexc) = Ldexc

         |  cleanCode(Handle{exp, taglist, handler}) =
                Handle{exp = cleanCode exp, taglist = map cleanCode taglist,
                       handler = cleanCode handler}

         |  cleanCode(Recconstr decs) = Recconstr(map cleanCode decs)

         |  cleanCode(c as Container _) = c

         |  cleanCode(SetContainer {container, tuple, size}) =
               let
                (* If the container is unused we don't need to set it.
                   The container won't be created either. *)
                  val used =
                      case container of
                        Extract{addr, level=0, fpRel=true, ...} =>
                            addr <= 0 orelse locals sub addr
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

         |  cleanCode _ = raise InternalError "cleanCode: unknown instr"
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

        | changeL(Lambda{body, isInline, name, closure, numArgs,
                         level, closureRefs, makeClosure}, nesting) =
            Lambda{body = changeL(body, nesting+1), isInline = isInline,
                   name = name, closure = closure, numArgs = numArgs,
                   level = level + correction, closureRefs = closureRefs,
                   makeClosure = makeClosure }

        | changeL(Eval{function, argList, earlyEval}, nesting) =
            Eval{function = changeL(function, nesting),
                 argList = map (fn a => changeL(a, nesting)) argList,
                 earlyEval = earlyEval}

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
        | Constnt _ => entry
        | gen as Lambda _ =>
            optVal {
              general = changeL(gen, 0),
              special = optSpecial entry,
              environ = optEnviron entry,
              decs    = [],
              recCall = optRec entry
            }
        | _ => raise InternalError "changeLevel: Unknown entry"
       end
  (* end changeLevel *);

(*****************************************************************************)
(*                  optimiseProc                                             *)
(*****************************************************************************)
  fun optimiseProc
    (pt : codetree,
     lookupNewAddr : loadForm * int * int -> optVal,
     lookupOldAddr : loadForm * int * int -> optVal,
     enterDec : int * optVal -> unit,
     enterNewDec : int * optVal -> unit,
     nestingOfThisProcedure : int,
     spval : int ref,
     earlyInline : bool,
     evaluate : codetree -> codetree,
     tailCallEntry: bool ref option,
     recursiveExpansions:
        ((codetree list * bool * int -> codetree list) * bool ref) list,
     maxInlineSize: int) =
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
            val spec = optSpecial ins;
            val ov = 
              (* If it is a non-inline procedure it must be declared. *)
              case (spec, pushProc) of
                (Lambda{isInline=NonInline, ...}, true) => simpleOptVal gen
              | _ => stripDecs ins  (* Remove the declarations before entering it. *)
            val () = setTab (addrs, ov);
          in
            (* Just return the declarations. *)
            optDecs ins 
          end

       | Extract { level = 0, ...} =>
          let
            (* Declaration is simply giving a new name to a local
               - can ignore this declaration. *) 
            val optVal = stripDecs ins (* Simply copy the entry.  *)
            val () = setTab (addrs, optVal);
          in
            optDecs ins
          end

(* old ...
      else if isIndirect gen
      then let
        (* It is safe to defer an indirection if we can. For instance,
           in ML fun f (a as (b,c)) will generate declarations of b and c
           as indirections on a. If b and c are not used immediately there
           is no point in loading them  (it only uses up the registers).
           Once they are actually used they will be loaded into registers
           and those registers will be cached by the normal register caching
           scheme, so that if used again soon after they will not be
           reloaded. *)
        val ind = cIndirect gen;
        fun optSetTab (i, v) =
      setTab
        (i,
         optVal 
           { (* Add on the indirection. *)
         general = mkInd (indOffset ind, optGeneral v),
         special = optSpecial v,
         environ = optEnviron v,
         decs    = optDecs v
           });
      in
        (* Take off the indirection from the value to be declared and add
           it to the load instruction. This causes the indirection to be
           deferred until the value is actually used. *)
        newDecl
          (optSetTab,
       optVal 
         {
           general = indBase ind,
           special = optSpecial ins,
           environ = optEnviron ins,
           decs    = optDecs ins
         },
           addrs,
           pushProc)
      end
... *)

        | _ =>
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
              | _ => spec
              
            val optV = 
              optVal 
            {
              general = mkLoad (decSpval, 0),
              special = optSpec,
              environ = optEnviron ins,
              decs    = [],
              recCall = optRec ins
            };
            
            val () = setTab (addrs, optV);
          in
            optDecs ins @ [mkDecRef gen decSpval 0]
          end
    end (* newDecl *);

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
        
     |  optimise (Eval{function, argList, earlyEval}, tailCall) =
        let
          (* Get the function to be called and see if it is inline or
             a lambda expression. *)
          val funct : optVal = optimise(function, NONE);
          val foptRec = optRec funct
               
          (* There are essentially two cases to consider - the procedure
             may be "inline" in which case it must be expanded as a block,
             or it must be called. *)
          fun notInlineCall recCall = 
            let
            val argsAreConstants = ref true;

            fun copyArg arg =
              let
                 val copied = general arg;
              in
                (* Check for early evaluation. *)
                if not (isConstnt copied) then argsAreConstants := false else ();
                copied 
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
                        isIoAddress(toAddress w) andalso
                                    earlyRtsCall(w, copiedArgs)
                  | _ => false
                 )
                )
            
            val evCopiedCode = 
              if evalEarly
              then evaluate (mkEval (gen, copiedArgs, early))
              else case recCall of
                (* This is a recursive call to a function we're expanding.
                   Is it tail recursive?  We may have several levels of
                   expansion. *)
                SOME (filterArgs, optr) =>
                    if (case tailCall of
                            SOME tCall => optr = tCall (* same reference? *)
                        | NONE => false)
                    then Loop (filterArgs(copiedArgs, true, nestingOfThisProcedure))
                    else mkEval (gen,
                            filterArgs(copiedArgs, false, nestingOfThisProcedure), early)
                 (* Not a recursive expansion. *)
               | NONE => mkEval (gen, copiedArgs, early)
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
          case (List.find (fn (_, r) => r = foptRec) recursiveExpansions,
                optSpecial funct) of
            (recCall as SOME _, _) =>
                 (* We're already expanding this function - don't recursively expand
                    it.  Either loop or generate a function call. *)
                notInlineCall recCall
          | (_,
            Lambda { isInline, body=lambdaBody, name=lambdaName, ...}) =>
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
              | copy (h::t) argAddress =
              let
                fun setTab (index, v) = update (paramVec, ~index, SOME v);
                (* Make the declaration, picking out constants, inline
                   procedures and load-and-stores. These are entered in the
                   table, but nil is returned by "newDecl". *)
                val lapt = newDecl (setTab, optimise(h, NONE), argAddress, false);
              in (* Now process the rest of the declarations. *)
                lapt @ copy t (argAddress + 1)
              end (* end copy *);

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
                      (lambdaBody,
                       lookupLocalNewAddr,
                       lookupDec, 
                       setTabForInline,
                       setNewTabForInline,
                       nestingOfThisProcedure,
                       spval,
                       earlyInline orelse earlyEval,
                       evaluate,
                       tailCall,
                       recursiveExpansions,
                       maxInlineSize)
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
                    val argModificationVec = stretchArray (initTrans, false);
                    (* Create addresses for the new variables for modified arguments.
                       If newdecl created a variable we might be able to reuse that
                       but it's easier to create new ones. *)
                    val argBaseAddr = ! spval; val _ = spval := argBaseAddr + nArgs
    
                    (* filterArgs is called whenever a recursive call is made to
                       this function. *)
                    fun filterArgs (argList, isTail, depth) =
                    let
                        fun filterArgs' 0 [] = []
                          | filterArgs' _ [] =
                                raise InternalError "filterArgs': wrong number of args"
                          | filterArgs' n (arg :: rest) =
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
                                else update(argModificationVec, n, true);
                                (* If any recursive call has changed it we need
                                   to include this argument even if it didn't
                                   change on this particular call. *)
                                if argModificationVec sub n
                                then arg :: filterArgs' (n-1) rest
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
                    fun countSet n 0 = n
                      | countSet n i =
                            if argModificationVec sub i then countSet (n+1) (i-1)
                            else countSet n (i-1)

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
                            else if argModificationVec sub (~n)
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
                                if argModificationVec sub ~index
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

                        val copiedBody =
                             optimiseProc 
                              (lambdaBody,
                               localNewAddr,
                               prev, 
                               setTab,
                               setNewTab,
                               nesting,
                               ref 1,
                               earlyInline orelse earlyEval,
                               evaluate,
                               NONE, (* Don't generate loop instructions. *)
                               (filterArgs, foptRec) :: recursiveExpansions,
                               maxInlineSize)

                        val newModCount = countSet 0 nArgs
                    in
                        if newModCount > lastModCount
                        then (* We have some (more) arguments to include. *)
                            checkRecursiveCalls newModCount
                        else optVal {
                                general = Lambda {
                                    body = getGeneral copiedBody,
                                    isInline = NonInline,
                                    name = lambdaName,
                                    closure = [],
                                    numArgs = lastModCount,
                                    level = nestingOfThisProcedure + 1,
                                    closureRefs = 0,
                                    makeClosure = false },
                                special = CodeNil,
                                decs = [],
                                recCall = ref false,
                                environ = localNewAddr
                            }
                    end

                    fun checkForLoopInstrs lastModCount =
                    (* Called initially or while we only have tail recursive
                       calls.  We can inline the function. *)
                    let
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
                             else (* index < 0 *) if argModificationVec sub ~index
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
                              (lambdaBody,
                               lookupLocalNewAddr,
                               prev, 
                               setTabForInline,
                               setNewTabForInline,
                               nestingOfThisProcedure,
                               spval,
                               earlyInline orelse earlyEval,
                               evaluate,
                               SOME foptRec,
                               (filterArgs, foptRec) :: recursiveExpansions,
                               maxInlineSize)
                    in
                        if ! needsRecursiveCall
                        then (* We need a fully recursive call. *)
                            checkRecursiveCalls (countSet 0 nArgs)
                        else if ! needsBeginLoop 
                        then (* We've found at least one recursive call which changes its
                                argument value. *)
                        let
                            val newModCount = countSet 0 nArgs
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
                    fun makeDecs 0 _ = []
                      | makeDecs n isCall =
                            if not (argModificationVec sub n)
                            then makeDecs (n-1) isCall
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
                                argDec :: makeDecs (n-1) isCall
                            end
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
                                    mkEval(mkLoad(addr, 0), makeDecs nArgs true, false),
                                special = CodeNil,
                                decs = [mkDec(addr, getGeneral procBody)],
                                recCall = ref false,
                                environ = lookupNewAddr
                            }
                        end
                    else if ! needsBeginLoop
                    then simpleOptVal(BeginLoop(getGeneral procBody, makeDecs nArgs false))
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

     |  optimise (original as Lambda({body=lambdaBody, isInline=lambdaInline, name=lambdaName,
                          numArgs, ...}), _) =
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

          val newCode =
             optimiseProc 
               (lambdaBody,
                localNewAddr,
                localOldAddr, 
                setTab,
                setNewTab,
                nesting,
                ref 1,
                false,
                evaluate,
                NONE,
                recursiveExpansions,
                maxInlineSize);

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
            cleanProc(getGeneral newCode, checkRecursion, nesting)

          val resultCode =
            case lambdaInline of
                OnlyInline =>
                (* Used only for functors.  Don't compile now. Return the processed
                   version as the special value. *)
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
                         DCJM 6/1/00 *)
                      special =
                        Lambda 
                          {
                           body          = cleanedBody,
                           isInline      = OnlyInline,
                           name          = lambdaName,
                           closure       = [],
                           numArgs       = numArgs,
                           level         = nesting,
                           closureRefs   = 0,
                           makeClosure   = false
                         },
                      environ = lookupNewAddr, (* new addresses with cleanedBody. *)
                      decs    = [],
                      recCall = ref false
                    }

            |   MaybeInline => (* Explicitly inlined functions. *)
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
                           numArgs       = numArgs,
                           level         = nesting,
                           closureRefs   = 0,
                           makeClosure   = false
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
                    if lambdaInline = NonInline andalso isSmall(cleanedBody, maxInlineSize)
                    then SmallFunction
                    else lambdaInline

                  val copiedLambda =
                    Lambda 
                      {
                       body          = cleanedBody,
                       isInline      = inlineType,
                       name          = lambdaName,
                       closure       = !nonLocals, (* Only looked at in MutualDecs. *)
                       numArgs       = numArgs,
                       level         = nesting,
                       closureRefs   = 0,
                       makeClosure   = false
                     };
                 val general = 
                   (* If this has no free variables we can code-generate it now. *)
                   if null (!nonLocals)
                   then evaluate copiedLambda
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
                           numArgs       = numArgs,
                           level         = nesting,
                           closureRefs   = 0,
                           makeClosure   = false
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
           
     |  optimise (BeginLoop(body, args), tailCall) =
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
                  (mkEnv(args @ [body]), lookupNewAddr, lookupOldAddr,
                   enterDec, enterNewDec, nestingOfThisProcedure,
                   spval, earlyInline, evaluate, SOME foptRec,
                   (filterArgs, foptRec) :: recursiveExpansions,
                   maxInlineSize)
        in
            if not (! loops)
            then (* The Loop instructions have been optimised away.  Since there's
                    no BeginLoop we can reprocess it with the surrounding
                    tail recursion. *)
                optimise(mkEnv(args @ [body]), tailCall)
            else (* It loops - have to reprocess. *)
            let
                (* The arguments to the functions are Declar entries but they
                   must not be optimised. *)
                fun declArg(Declar{addr, value, ...}) =
                    let
                        val optVal = optimise(value, NONE)
                        val decSpval = ! spval
                        val _ = spval := decSpval + 1
                        val optV = simpleOptVal(mkLoad (decSpval, 0))
                    in
                        enterDec(addr, optV);
                        mkDec(decSpval, getGeneral optVal)
                    end
                 |  declArg _ = raise InternalError "declArg: not Declar"
                 val declArgs = map declArg args
                 val beginBody =
                    optimiseProc 
                      (body, lookupNewAddr, lookupOldAddr, enterDec, enterNewDec,
                       nestingOfThisProcedure, spval, earlyInline, evaluate, SOME foptRec,
                       (filterArgs, foptRec) :: recursiveExpansions, maxInlineSize)
            in
                simpleOptVal (BeginLoop (getGeneral beginBody, declArgs))
            end
        end

     |  optimise (Loop args, tailCall) =
        (
            (* The Loop instruction should be at the tail of the
               corresponding BeginLoop. *)
            case (tailCall, recursiveExpansions) of
                (SOME fopt, (filterArgs, fopt') :: _) =>
                    if fopt <> fopt'
                    then raise InternalError "Loop: mismatched BeginLoop"
                    else simpleOptVal (Loop (filterArgs((map general args),
                            true, nestingOfThisProcedure)))
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
                 List.foldl (
                    fn (d, (inlines, nonInlines)) =>
                        case d of
                            Declar{value = Lambda{ isInline=MaybeInline, ...}, ... } =>
                                (d::inlines, nonInlines)
                        |   _ => (inlines, d::nonInlines)) ([], []) mutualDecs;
                    
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
                    nonInlines;
              val endAddr = !spval
              (* We can now process the inline functions.  Since these
                 can't be directly recursive we don't need to do anything
                 special. *)
              val _ =
                 List.app (fn Declar{ value, addr, ... } =>
                                enterDec (addr, optimise(value, NONE))
                          | _ => raise InternalError "mutualDecs: not Declar")
                    inlines;

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
                                     decSpval,
                                     (decs, otherChanges)) =
                (* Have a look at the old entry to see if it's a constant. *)
                let
                    val oldEntry =
                        lookupOldAddr(
                                {addr=decAddr, level=0, fpRel=true, lastRef=false},
                                nestingOfThisProcedure, 0)
                    val oldGen = optGeneral oldEntry
                in
                if isConstnt oldGen
                then (mkDec (decSpval, oldGen) :: decs, otherChanges) (* It's already a constant - don't reprocess. *)
                else let
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
                        val results = evaluate code

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

              val result =
                case decs of
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
                  decs    = result @ optDecs rest,
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
          
     |  optimise (Recconstr entries, _) =
         (* The main reason for optimising record constructions is that they
            appear as tuples in ML. We try to ensure that loads from locally
            created tuples do not involve indirecting from the tuple but can
            get the value which was put into the tuple directly. If that is
            successful we may find that the tuple is never used directly so
            the use-count mechanism will ensure it is never created. *)
      let
        val newTab = stretchArray (initTrans, NONE);
        fun setTab (i, v) = update (newTab, i, SOME v);
        (* The record construction is treated as a block of local
           declarations so that any expressions which might have side-effects
           are done exactly once. *)
        val allConsts = ref true;
        
        fun makeDecs []     _ = {decs = [], gen_args = [], spec_args = []}
          | makeDecs (h::t) addr =
          let
            (* Declare this value. If it is anything but a constant
              there will be some code. *)
            val newDecs = newDecl (setTab, optimise(h, NONE), addr, true);
            val thisArg = valOf (newTab sub addr); (* Get the value back. *)
            val rest    = makeDecs t (addr + 1);
            val gen     = optGeneral thisArg;
            val spec    = optSpecial thisArg;
            val ()      =
              if not (isConstnt gen) then allConsts := false else ();
              
            val specArgs =
              if isCodeNil spec andalso isConstnt gen
              then gen :: #spec_args rest
              else mkLoad (addr, 0) :: #spec_args rest
          in
            {gen_args  = gen :: #gen_args rest,
             spec_args = specArgs,
             decs      = newDecs @ #decs rest }
          end;
          
        val newDecs = makeDecs entries 1;
        val newRec  = Recconstr (#gen_args newDecs);

        val gen  = if !allConsts then makeConstVal newRec else newRec;
        val spec = Recconstr (#spec_args newDecs);
        
        val vec = StretchArray.vector newTab
        
        fun env ({addr, ...}:loadForm, depth, _) : optVal =
          changeLevel
            (valOf (Vector.sub(vec, addr)))
            (depth - nestingOfThisProcedure)
      
      in
        optVal 
          {
            general = gen,
            special = spec,
            environ = env,
            decs    = #decs newDecs,
            recCall = ref false
          }
      end
          
      |  optimise (Indirect{ base, offset }, _) =
     let (* Try to do the selection now if possible. *)
        val source = optimise(base, NONE)
      in
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
                   (specEntry,
                    errorEnv, (* We must always look up old addresses. *)
                    optEnviron source,
                    enterDec, (* should not be used *)
                    enterNewDec, (* should not be used *)
                    nestingOfThisProcedure,
                    spval,
                    earlyInline,
                    evaluate,
                    NONE,
                    recursiveExpansions,
                    maxInlineSize);
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
      end
       
      |  optimise (pt as Ldexc, _) =
            simpleOptVal pt (* just a constant so return it *)
        
      |  optimise (Handle { exp, taglist, handler }, tailCall) =
        simpleOptVal 
          (Handle {exp     = general exp,
                   taglist = map general taglist,
                   handler = generalOptimise(handler, tailCall)}
          )

        (* Case expressions are generated only in preCode from if-then-else. *)
(*      |  optimise (Case { cases, test, default, min, max }) =
      let
        
       fun optimiseCasePair ((e,l):casePair) : casePair =
         (general e, l);
      in
        simpleOptVal
          (Case
        {
          cases   = map optimiseCasePair cases,
          test    = general test,
          default = general default,
          min     = min,
          max     = max
        }
          )
      end
*)          
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
      
      |  optimise _ = raise InternalError "unknown instruction"
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

    (* This ensures that everything is printed just before
       it is code-generated. *)
    val codeGenAndPrint =
        if printCodeTreeAfter
        then fn code => (compilerOut(pretty code); codegen(code, debugSwitches))
        else fn code => codegen(code, debugSwitches);
    
    fun preCodeAndPrint code =
    (
        if printCodeTree then compilerOut(pretty code) else ();
        preCode (codeGenAndPrint, code)
    )

    (* Optimise it. *)
    val oldAddrTab = stretchArray (initTrans, NONE);
    val newAddrTab = stretchArray (initTrans, NONE);

    val insertedCode =
    let
      (* Strip off a surrounding declaration. *)
      val pt =
        case pt of Declar{value, ...} => value | _ => pt;
        
      fun lookupOldAddr ({addr, ...}: loadForm, depth, 0) =
            changeLevel (valOf (oldAddrTab sub addr)) depth
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
      
      fun eval pt = evaluate (preCodeAndPrint pt) codeGenAndPrint;

      val resultCode =
          optimiseProc
            (pt,
             lookupNewAddr,
             lookupOldAddr,
             enterDec,
             enterNewDec,
             0, (* nesting *)
             ref 1,
             false, (*Not inline*)
             eval,
             NONE,
             [],
             maxInlineSize)
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
      else let
        val code = codeGenAndPrint (preCodeAndPrint optCode);
      in (* Return procedure to execute it *)
        (fn () => Global (simpleOptVal (mkConst (code ()))))
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
        val code    = codeGenAndPrint (preCodeAndPrint(newDecs));
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
    end

end (* CODETREE functor body *);
