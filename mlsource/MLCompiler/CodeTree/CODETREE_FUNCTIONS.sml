(*
    Copyright (c) 2012,13 David C.J. Matthews

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
    
    fun mkEnv([], exp) = exp
    |   mkEnv(decs, exp) = Newenv(decs, exp)

    val word0 = toMachineWord 0
    and word1 = toMachineWord 1

    val False = word0  
    and True  = word1 

    val F_mutable_words : Word8.word = Word8.orb (F_words, F_mutable)

    val CodeFalse = Constnt False
    and CodeTrue  = Constnt True
    and CodeZero  = Constnt word0

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
  fun sideEffectFree (Lambda _) = true
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

    | sideEffectFree _ = false
             (* Rest are unsafe (or too rare to be worth checking) *)

    and testList t = List.all sideEffectFree t
    
    and sideEffectBinding(Declar{value, ...}) = sideEffectFree value
    |   sideEffectBinding(RecDecs _) = true (* These should all be lambdas *)
    |   sideEffectBinding(NullBinding c) = sideEffectFree c

    (* Makes a constant value from an expression which is known to be
       constant but may involve inline procedures, tuples etc.         *)
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
        Constnt (makeVal cVal)
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

    (* These are very frequently used and it might be worth making
       special bindings for values such as 0, 1, 2, 3 etc to reduce
       garbage. *)
    fun checkNonZero n = if n < 0 then raise InternalError "mkLoadxx: argument negative" else n
    val mkLoadLocal = Extract o LoadLocal o checkNonZero
    and mkLoadArgument = Extract o LoadArgument o checkNonZero
    and mkLoadClosure = Extract o LoadClosure o checkNonZero

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

    local
        val except: exn = InternalError "Invalid load encountered in compiler"
        (* Exception value to use for invalid cases.  We put this in the code
           but it should never actually be executed.  *)
        val raiseError = Raise (Constnt (toMachineWord except))
    in
        (* Look for an entry in a tuple. Used in both the optimiser and in mkInd. *)
        fun findEntryInBlock (Recconstr recs, offset, isVar) =
            if offset < List.length recs
            then List.nth(recs, offset)
            (* This can arise if we're processing a branch of a case discriminating on
               a datatype which won't actually match at run-time. e.g. Tests/Succeed/Test030. *)
            else if isVar
            then raiseError
            else raise InternalError "findEntryInBlock: invalid address"

        |  findEntryInBlock (Constnt b, offset, isVar) =
              (* The ML compiler may generate loads from invalid addresses as a
                 result of a val binding to a constant which has the wrong shape.
                 e.g. val a :: b = nil
                 It will always result in a Bind exception being generated 
                 before the invalid load, but we have to be careful that the
                 optimiser does not fall over.  *)
            if isShort b
            orelse not (Address.isWords (toAddress b))
            orelse Address.length (toAddress b) <= Word.fromInt offset
            then if isVar
            then raiseError
            else raise InternalError "findEntryInBlock: invalid address"
            else Constnt (loadWord (toAddress b, toShort offset))

        |  findEntryInBlock (ConstntWithInline(_, EnvSpecTuple(_, env)), offset, _) =
            (* Do the selection now.  This is especially useful if we
               have a global structure  *)
            (
                case env offset of
                    (EnvGenConst w, EnvSpecNone) => Constnt w
                |   (EnvGenConst w, spec) => ConstntWithInline(w, spec)
                (* The general value from selecting a field from a constant tuple must be a constant. *)
                |   _ => raise InternalError "findEntryInBlock: not constant"
            )
 
        |   findEntryInBlock (ConstntWithInline(general, _), offset, isVar) =
                (* Is this possible?  If it's inline it should be a tuple. *)
                 findEntryInBlock (Constnt general, offset, isVar)
    
        |   findEntryInBlock(base, offset, isVar) =
                Indirect {base = base, offset = offset, isVariant = isVar} (* anything else *)
     end
        
    (* Exported indirect load operation i.e. load a field from a tuple.
       We can't use  findEntryInBlock in every case since that discards
       unused entries in a tuple and at this point we haven't checked
       that the unused entries don't have
       side-effects/raise exceptions e.g. #1 (1, raise Fail "bad") *)
    local
        fun mkIndirect isVar (addr, base as ConstntWithInline _ ) = findEntryInBlock(base, addr, isVar)
        |   mkIndirect isVar (addr, base as Constnt _) = findEntryInBlock(base, addr, isVar)
        |   mkIndirect isVar (addr, base) = Indirect {base = base, offset = addr, isVariant = isVar}
    
    in
        val mkInd = mkIndirect false and mkVarField = mkIndirect true
    end
        
    (* Get the value from the code. *)
    fun evalue (Constnt c) = SOME c
    |   evalue (ConstntWithInline(g, _)) = SOME g
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
        and codeBinding = codeBinding
    end

end;
