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

(* Miscellaneous construction and operation functions on the code-tree. *)

functor CODETREE_FUNCTIONS(
    structure BASECODETREE: BaseCodeTreeSig
) : CodetreeFunctionsSig
=
struct
    open BASECODETREE
    open Address
    exception InternalError = Misc.InternalError

    fun mkGenLoad  (i1, i2, bl, lf) =
        Extract {addr  = i1, level = i2, fpRel = bl, lastRef = lf};

    fun mkClosLoad(addr, last) = Extract {level = 0, addr = addr, fpRel = false, lastRef = last}
 
    fun mkDecRef(ct, i1, i2) = Declar{value = ct, addr = i1, references = i2};
    fun mkDec (laddr, res) = mkDecRef(res, laddr, 0)

    fun mkMutualDecs l =
    let
        fun convertDec(a, Lambda lam) = {lambda = lam, addr = a, references = 0}
        |   convertDec _ = raise InternalError "mkMutualDecs: Recursive declaration is not a function"
    in
        RecDecs(List.map convertDec l)
    end

    val mkNullDec = NullBinding

    val mkIf                = Cond
    and mkConst             = Constnt
    and mkRaise             = Raise
    and mkContainer         = Container
    and mkIndirectVariable  = IndirectVariable
    and mkTupleVariable     = TupleVariable
    
    
    fun mkEnv([], exp) = exp
    |   mkEnv(decs, exp) = Newenv(decs, exp)

    val word0 = toMachineWord 0
    and word1 = toMachineWord 1

    val False = word0  
    and True  = word1 

    val F_mutable_words : Word8.word = Word8.orb (F_words, F_mutable)

    val CodeFalse = mkConst False
    and CodeTrue  = mkConst True
    and CodeZero  = mkConst word0

    (* For the moment limit these to general arguments. *)
    fun mkLoop args = Loop (List.map(fn c => (c, GeneralType)) args)
    and mkBeginLoop(exp, args) =
        BeginLoop{loop=exp, arguments=List.map(fn(i, v) => ({value=v, addr=i, references=0}, GeneralType)) args}

    fun mkWhile(b, e) = (* Generated as   if b then (e; <loop>) else (). *)
        mkBeginLoop(mkIf(b, mkEnv([NullBinding e], mkLoop[]), CodeZero), [])

    (* We previously had conditional-or and conditional-and as separate
       instructions.  I've taken them out since they can be implemented
       just as efficiently as a normal conditional.  In addition they
       were interfering with the optimisation where the second expression
       contained the last reference to something.  We needed to add a
       "kill entry" to the other branch but there wasn't another branch
       to add it to.   DCJM 7/12/00. *)
    fun mkCor(xp1, xp2)  = mkIf(xp1, CodeTrue, xp2);
    fun mkCand(xp1, xp2)  = mkIf(xp1, xp2, CodeZero);

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

    local
        val doCall: int*machineWord -> Word.word
            = RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific
    in
        (* The RTS has a table of properties for RTS functions.  The 103 call
           returns these Or-ed into the register mask. *)
        val PROPWORD_NORAISE  = 0wx40000000
        and PROPWORD_NOUPDATE = 0wx20000000
        and PROPWORD_NODEREF  = 0wx10000000

        fun rtsProperties ioCall = doCall(103, ioCall)
    end

    (* RTS calls that can be evaluated at compile-time i.e. they always return the
       same result and have no side-effects but may raise an exception for
       particular arguments. *)
    fun earlyRtsCall function =
    let
        val props = rtsProperties function
        val noUpdateNoDeref = Word.orb(PROPWORD_NOUPDATE, PROPWORD_NODEREF)
    in
        Word.andb(props, noUpdateNoDeref) = noUpdateNoDeref
    end

    (* RTS calls that have have no side-effects and do not raise exceptions.
       They may return different results for different calls but that doesn't
       matter if the result is going to be discarded. *)
    and sideEffectFreeRTSCall function =
    let
        val props = rtsProperties function
        val noUpdateNoRaise = Word.orb(PROPWORD_NOUPDATE, PROPWORD_NORAISE)
    in
        Word.andb(props, noUpdateNoRaise) = noUpdateNoRaise
    end

  (* Note: This simply returns true or false.  For complex expressions,
     such as an RTS call whose argument has a side-effect, we could
     reduce the code by extracting the sub-expressions with side-effects
     and returning just those. *)
  fun sideEffectFree CodeNil = true
    | sideEffectFree (Lambda _) = true
    | sideEffectFree (Constnt _) = true
    | sideEffectFree (Extract _) = true
    | sideEffectFree (Cond(i, t, e)) =
          sideEffectFree i andalso
          sideEffectFree t andalso
          sideEffectFree e
    | sideEffectFree (Newenv(decs, exp)) = List.all sideEffectBinding decs andalso sideEffectFree exp
    | sideEffectFree (Handle { exp, handler }) =
          sideEffectFree exp andalso sideEffectFree handler
    | sideEffectFree (Recconstr recs) = testList recs
    | sideEffectFree (Indirect{base, ...}) = sideEffectFree base

        (* An RTS call, which may actually be code which is inlined
           by the code-generator, may be side-effect free.  This can
           occur if we have, for example, "if exp1 orelse exp2"
           where exp2 can be reduced to "true", typically because it's
           inside an inline function and some of the arguments to the
           function are constants.  This then gets converted to
           (exp1; true) and we can eliminate exp1 if it is simply
           a comparison. *)
    | sideEffectFree (Eval{function=Constnt w, argList, ...}) =
        isIoAddress(toAddress w) andalso sideEffectFreeRTSCall w
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
    
    and sideEffectBinding(Declar{value, ...}) = sideEffectFree value
    |   sideEffectBinding(RecDecs _) = true (* These should all be lambdas *)
    |   sideEffectBinding(NullBinding c) = sideEffectFree c

    (* Makes a constant value from an expression which is known to be *)
    (* constant but may involve inline procedures, types etc.         *)
    fun makeConstVal (cVal:codetree) =
    let
        fun makeVal (Constnt c) = c
            (* should just be a tuple  *)
            (* Get a vector, copy the entries into it and return it as a constant. *)
        |   makeVal (Recconstr []) = word0 (* should have been optimised already! *)
        |   makeVal (Recconstr xp) =
            let
                val vec : address = alloc (toShort (List.length xp), F_mutable_words, word0);
      
                fun copyToVec []       _ = ()
                |   copyToVec (h :: t) locn =
                    (
                        assignWord (vec, toShort locn, makeVal h);
                        copyToVec t (locn + 1)
                    )
            in
                copyToVec xp 0;
                lock vec;
                toMachineWord vec
            end
        |   makeVal _ = raise InternalError "makeVal - not constant or record"
    in
        mkConst (makeVal cVal)
    end

    local
        fun allConsts []       = true
        |   allConsts (Constnt _ :: t) = allConsts t
        |   allConsts _ = false
    in  
        fun mkTuple xp =
        let
            val tuple = Recconstr xp
        in
            if allConsts xp
            then (* Make it now. *) makeConstVal tuple
            else tuple
        end;
    end



    (* Evaluates expressions by code-generating and running them.
       "resultCode" is a copied code expression. The result is either
       a constant or an exception. *)
    local
        exception Interrupt = Thread.Thread.Interrupt

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

    fun mkAltMatch (m1, m2) = AltMatch (m1, m2);

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
  

    (* Old form operations for backwards compatibility.  These all create
       default GeneralType arguments and results. *)


    fun mkEval (ct, clist)   =
    Eval {
        function = ct,
        argList = List.map(fn c => (c, GeneralType)) clist,
        resultType=GeneralType
    }

    local
        open RuntimeCalls
        val ioOp : int -> machineWord = RunCall.run_call1 POLY_SYS_io_operation
        val rtsFunction = mkConst o ioOp
    in
        fun mkNot arg = mkEval (rtsFunction POLY_SYS_not_bool, [arg])
        val testptreqFunction    = rtsFunction POLY_SYS_word_eq
        val testptrneqFunction   = rtsFunction POLY_SYS_word_neq

        (* N.B. int equality is SHORT integer equality *)
        fun mkTestinteq (xp1, xp2) = 
            mkEval (rtsFunction POLY_SYS_word_eq, [xp1,xp2]);
    end
  
    fun mkTestptreq  (xp1, xp2) = mkEval (testptreqFunction, [xp1,xp2]);
    fun mkTestptrneq (xp1, xp2) = mkEval (testptrneqFunction, [xp1,xp2]);
    fun mkTestnull xp1       = mkTestptreq  (xp1, CodeZero);
    fun mkTestnotnull xp1    = mkTestptrneq (xp1, CodeZero);
  
    (* Test a tag value. *)
    fun mkTagTest(test: codetree, tagValue: word, maxTag: word) =
        TagTest {test=test, tag=tagValue, maxTag=maxTag }
        (*mkEval (rtsFunction POLY_SYS_word_eq, [test, mkConst(toMachineWord tagValue)], true);*)

    fun mkHandle (exp, handler) = Handle {exp = exp, handler = handler};

    fun mkStr (strbuff:string) = mkConst (toMachineWord strbuff);

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

    |   multipleUses (code as Extract{addr, level=loadLevel, ...}, _, level) = 
        let (* May have to adjust the level. *)
            fun loadFn lev =
                if lev = level
                then code 
                else mkLoad (addr, loadLevel + (lev - level))
        in
            {load = loadFn, dec = []}
        end
    
   |    multipleUses (code, nextAddress, level) = 
        let
            val addr       = nextAddress();
            fun loadFn lev = mkLoad (addr, lev - level);
        in
            {load = loadFn, dec = [mkDec (addr, code)]}
        end (* multipleUses *);

    (* Set the container to the fields of the record.  Try to push this
       down as far as possible. *)
    fun mkSetContainer(container, Cond(ifpt, thenpt, elsept), size) =
        Cond(ifpt, mkSetContainer(container, thenpt, size),
            mkSetContainer(container, elsept, size))

    |  mkSetContainer(container, Newenv(decs, exp), size) =
            Newenv(decs, mkSetContainer(container, exp, size))

    |  mkSetContainer(_, r as Raise _, _) =
        r (* We may well have the situation where one branch of an "if" raises an
             exception.  We can simply raise the exception on that branch. *)

    |   mkSetContainer(container, tuple, size) =
            SetContainer{container = container, tuple = tuple, size = size }

    (* Create a tuple from a container. *)
    val mkTupleFromContainer = TupleFromContainer

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
  
    fun errorEnv (_,  _, _) : optVal = raise InternalError "error env"
  
    fun optGeneral (OptVal {general,...})       = general 
    |   optGeneral (ValWithDecs {general, ...}) = general
    |   optGeneral (JustTheVal ct)              = ct
      
    fun optSpecial (OptVal {special,...}) = special
      | optSpecial _                      = CodeNil
      
    fun optEnviron (OptVal {environ,...}) = environ
      | optEnviron _                      = errorEnv
      
    fun optDecs    (OptVal {decs,...})       = decs
      | optDecs    (ValWithDecs {decs, ...}) = decs
      | optDecs    (JustTheVal _)           = [];
  
    fun optRec     (OptVal {recCall,...})       = recCall
      | optRec     _ = ref false; (* Generate a temporary. *)
  
    val simpleOptVal : codetree -> optVal = JustTheVal

    fun optVal{special=CodeNil, decs=[], general, ...} = JustTheVal general
    |   optVal{special=CodeNil, decs, general, ...} = ValWithDecs {general = general, decs = decs}
    |   optVal ov = OptVal ov


    local
        val except: exn = InternalError "Invalid load encountered in compiler"
        (* Exception value to use for invalid cases.  We put this in the code
           but it should never actually be executed.  *)
        val raiseError = mkRaise (mkConst (toMachineWord except))
    in
        (* Look for an entry in a tuple. Used in both the optimiser and in mkInd. *)
        fun findEntryInBlock (Recconstr recs) offset =
            if offset < List.length recs
            then List.nth(recs, offset)
            (* This can arise if we're processing a branch of a case discriminating on
               a datatype which won't actually match at run-time. e.g. Tests/Succeed/Test030. *)
            else raiseError

        |  findEntryInBlock (Constnt b) offset =
              (* The ML compiler may generate loads from invalid addresses as a
                 result of a val binding to a constant which has the wrong shape.
                 e.g. val a :: b = nil
                 It will always result in a Bind exception being generated 
                 before the invalid load, but we have to be careful that the
                 optimiser does not fall over.  *)
            if isShort b
            orelse not (Address.isWords (toAddress b))
            orelse Address.length (toAddress b) <= Word.fromInt offset
            then raiseError
            else mkConst (loadWord (toAddress b, toShort offset))
    
        |  findEntryInBlock (Global glob) offset =
            (* Do the selection now.  This is especially useful if we
               have a global structure  *)
            (
                case optSpecial glob of
                    recc as Recconstr _ =>
                    (
                        case findEntryInBlock recc offset of
                            Extract (ext as {level, ...}) =>
                                Global (optEnviron glob (ext, 0, (* global *) level))
                        |   selection => selection (* Normally a constant *)
                    )
      
                |   _ => findEntryInBlock (optGeneral glob) offset
            )
    
        |   findEntryInBlock base offset =
                Indirect {base = base, offset = offset} (* anything else *)
     end
        
    (* Exported indirect load operation i.e. load a field from a tuple.
       We can't use  findEntryInBlock in every case since that discards
       unused entries in a tuple and at this point we haven't checked
       that the unused entries don't have
       side-effects/raise exceptions e.g. #1 (1, raise Fail "bad") *)
    fun mkInd (addr, base as Global _ ) = findEntryInBlock base addr
    |   mkInd (addr, base as Constnt _) = findEntryInBlock base addr
    |   mkInd (addr, base) = Indirect {base = base, offset = addr};
        
    (* Get the value from the code. *)
    fun evalue (Constnt c) = SOME c
    |   evalue (Global g) = evalue(optGeneral g)
    |   evalue _ = NONE

    (* This is really to simplify the change from mkEnv taking a codetree list to
       taking a codeBinding list * code.  This extracts the last entry which must
       be a NullBinding and packages the declarations with it. *)
    fun decSequenceWithFinalExp decs =
    let
        fun splitLast _ [] = raise InternalError "decSequenceWithFinalExp: empty"
        |   splitLast decs [NullBinding exp] = (List.rev decs, exp)
        |   splitLast _ [_] = raise InternalError "decSequenceWithFinalExp: last is not a NullDec"
        |   splitLast decs (hd::tl) = splitLast (hd:: decs) tl
    in
        mkEnv(splitLast [] decs)
    end

    structure Sharing =
    struct
        type codetree = codetree
        and  optVal = optVal
        and  argumentType = argumentType
        and  varTuple = varTuple
        and  codeBinding = codeBinding
    end

end;
