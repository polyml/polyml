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

structure DEBUG :
sig
    val codetreeTag:            bool Universal.tag (* If true then print the original code. *)
    val maxInlineSizeTag:       int  Universal.tag
    val getParameter : 'a Universal.tag -> Universal.universal list -> 'a
end

structure PRETTY : PRETTYSIG

structure BASECODETREE: BaseCodeTreeSig

structure CODETREE_FUNCTIONS: CodetreeFunctionsSig

structure BACKEND:
sig
    type codetree
    type machineWord = Address.machineWord
    val codeGenerate: codetree * int * Universal.universal list -> unit -> machineWord
    structure Sharing : sig type codetree = codetree end
end

structure OPTIMISER:
sig
    type codetree  and envSpecial and codeBinding
    val codetreeOptimiser: codetree  * Universal.universal list * int ->
        { numLocals: int, general: codetree, bindings: codeBinding list, special: envSpecial }
    structure Sharing: sig type codetree = codetree and envSpecial = envSpecial and codeBinding = codeBinding end
end

sharing type
    PRETTY.pretty
=   BASECODETREE.pretty

sharing
    BASECODETREE.Sharing
=   CODETREE_FUNCTIONS.Sharing
=   BACKEND.Sharing
=   OPTIMISER.Sharing

) : CODETREESIG =

(*****************************************************************************)
(*                  CODETREE functor body                                    *)
(*****************************************************************************)
struct
    open Address;
    open StretchArray;
    open RuntimeCalls; (* for POLY_SYS numbers and EXC_nil *)
    open BASECODETREE;
    open PRETTY;
    open CODETREE_FUNCTIONS
  
    exception InternalError = Misc.InternalError
    and Interrupt = Thread.Thread.Interrupt
  
    infix 9 sub;

    (* gets a value from the run-time system *)
    val ioOp : int -> machineWord = RunCall.run_call1 POLY_SYS_io_operation;
    
    (* since code generator relies on these representations,
       we may as well export them *)
    val rtsFunction = mkConst o ioOp

    (* Transform a function so that free variables are converted to closure form.  Returns the
       maximum local address used. *)
    fun transformAndMaxAddress(code, numArgs) =
    let
        val maxAddr = ref 0 and closureList: (int * int * int) list ref = ref []
        
        fun deExtract(Extract ext) = ext | deExtract _ = raise InternalError "deExtract"
    
        (* Convert the base codetree into the optimiser version. *)
        fun transformClosure MatchFail = MatchFail
    
        |   transformClosure(AltMatch(a, b)) = AltMatch(transformClosure a, transformClosure b)

        |   transformClosure(Newenv(decs, exp)) =
            let
                fun checkAddr addr = (maxAddr := Int.max(!maxAddr, addr); addr)

                and transformBindings(Declar{ value, addr}) =
                        Declar{value = transformClosure value, addr = checkAddr addr }
                |   transformBindings(RecDecs l) =
                        RecDecs(map(fn{addr, lambda } => {addr = checkAddr addr, lambda = transformLambda lambda}) l)

                |   transformBindings(NullBinding c) = NullBinding(transformClosure c)
            in
                Newenv(map transformBindings decs, transformClosure exp)
            end

        |   transformClosure(c as Constnt _) = c

        |   transformClosure(Extract(LoadLegacy{level = 0, addr, fpRel = true, ...})) =
                if addr >= 0 then Extract(LoadLocal addr)
                else (* Negative - argument *) if ~addr > numArgs
                then raise InternalError "transformClosure: Bad addr for argument"
                else Extract(LoadArgument(numArgs+addr))

        |   transformClosure(Extract(LoadLegacy{fpRel = false, ...})) =
                raise InternalError "transformClosure: already closure relative"

        |   transformClosure(Extract(LoadClosure _)) =
                raise InternalError "transformClosure: already closure relative"

        |   transformClosure(Extract LoadRecursive) =
                raise InternalError "transformClosure: already closure relative"

        |   transformClosure(ext as Extract(LoadLocal _)) = ext (* Local *)

        |   transformClosure(ext as Extract(LoadArgument _)) = ext (* Argument *)

        |   transformClosure(Extract(LoadLegacy{addr, level, ...})) =
                (* Non-local *)
            let
                val closure = !closureList
                val (result, newClosure) =   
                    case (List.find(fn(_, a, l) => addr=a andalso level = l) closure, closure) of
                        (SOME(n, _, _), _) => (* Already there. *)
                            (Extract(LoadClosure n), closure)
                    |   (NONE, []) => (* Not there and this is the first item for the closure. *)
                            (Extract(LoadClosure 0), [(0, addr, level)])
                    |   (NONE, (n, _, _) :: _) => (* Not there - add an item to the closure. *)
                            (Extract(LoadClosure(n+1)), (n+1, addr, level) :: closure)
            in
                closureList := newClosure;
                result
            end

        |   transformClosure(Indirect{base, offset}) = Indirect{base = transformClosure base, offset = offset }
    
        |   transformClosure(Eval{ function, argList, resultType}) = 
                Eval{function = transformClosure function,
                        argList = map (fn(c, t) => (transformClosure c, t)) argList,
                        resultType = resultType }
    
        |   transformClosure(Lambda lambda) = Lambda(transformLambda lambda)

        |   transformClosure(Cond(i, t, e)) = Cond(transformClosure i, transformClosure t, transformClosure e)
    
        |   transformClosure(BeginLoop{ loop, arguments }) =
                BeginLoop
                    {loop = transformClosure loop,
                     arguments = map (fn ({value, addr}, t) => ({value = transformClosure value, addr = addr}, t)) arguments}

        |   transformClosure(Loop l) = Loop(map(fn(c,t) => (transformClosure c, t)) l)

        |   transformClosure(Raise c) = Raise(transformClosure c)

        |   transformClosure Ldexc = Ldexc

        |   transformClosure(Handle{exp, handler}) =
                Handle{exp = transformClosure exp, handler = transformClosure handler }

        |   transformClosure(Recconstr l) = Recconstr(map transformClosure l)

        |   transformClosure(Container i) = Container i

        |   transformClosure(SetContainer { container, tuple, size }) =
                SetContainer{ container = transformClosure container, tuple = transformClosure tuple, size = size }

        |   transformClosure(TupleFromContainer(c, n)) = TupleFromContainer(transformClosure c, n)

        |   transformClosure(TagTest{ test, tag, maxTag }) = TagTest{ test = transformClosure test, tag = tag, maxTag = maxTag }

        |   transformClosure(ConstntWithInline args) = ConstntWithInline args

        and transformLambda (lambda as {closure = [], ... }) = lambda
            (* The body should already have been processed - all we need to do is the closure. *)

        |   transformLambda { body, isInline, name, closure, argTypes, resultType, localCount } =
            { body = body, isInline = isInline, name = name,
              closure = map (deExtract o transformClosure o Extract) closure, argTypes = argTypes,
              resultType = resultType, localCount = localCount }

        val resultCode = transformClosure code
    in
        (resultCode, !maxAddr,
            (* Convert to Extract entries and reverse the list. *)
            List.foldl (fn((_, addr, level), l) => deExtract(mkLoad(addr, level-1)) :: l) [] (!closureList))
    end

    fun genCode(pt, debugSwitches) =
    let
        val printCodeTree      = DEBUG.getParameter DEBUG.codetreeTag debugSwitches
        and compilerOut        = PRETTY.getCompilerOutput debugSwitches
        
(*        val printCodeTree = true
        and compilerOut = PRETTY.prettyPrint(TextIO.print, 70) *)

        (* If required, print it first.  This is the code that the front-end
           has produced. *)
        val () = if printCodeTree then compilerOut(pretty pt) else ()

        (* This ensures that everything is printed just before
           it is code-generated. *) 
        fun codeAndPrint(code, localCount) =
        let
            val () = if printCodeTree then compilerOut (BASECODETREE.pretty code) else ();
        in
            BACKEND.codeGenerate(code, localCount, debugSwitches)
        end

        (* Just get the max address - everything else should be the same. *)
        val (transCode, maxAddr, nonLocals) = transformAndMaxAddress(pt, 0)
        val _ = null nonLocals orelse raise InternalError "Non-locals at the top level"

        (* Optimise it. *)
        val { numLocals = localCount, general = gen, bindings = decs, special = spec } =
            OPTIMISER.codetreeOptimiser(transCode, debugSwitches, maxAddr+1)

        (* At this stage we have a "general" value and also, possibly a "special"
           value.  We could simply create mkEnv(decs, gen) and run preCode
           and genCode on that.  However, we would lose the ability to insert
           any inline functions from this code into subsequent top-level
           expressions.  We can't simply retain the "special" entry either
           because that may refer to values that have to be created once when
           the code is run.  Such values will be referenced by "load" entries
           which refer to entries in the "decs".  We construct a tuple which
           will contain the actual values after the code is run.  Then if
           we want the value at some time in the future when we use something
           from the "special" entry we can extract the corresponding value
           from this tuple.
           We check to see if the special entry contains a function and if
           it does not we simply generate the version without any special
           entry at all.  This speeds up compilation in certain pathological
           cases. *)
        (* See if there is an inline function in there. *)
        fun containsFunction (_, true) = true
        |   containsFunction (EnvSpecNone, _) = false
        |   containsFunction (EnvSpecInlineFunction _, _) = true
        |   containsFunction (EnvSpecTuple(size, env), _) =
            let
                val fields = List.tabulate(size, fn n=>n)
            in
                List.foldl (fn (c, f) => containsFunction(#2 (env c), f)) false fields
            end

    in
        if not (containsFunction(spec, false))
        then
        let
            val code = codeAndPrint (mkEnv(decs, gen), localCount)
        in
            fn () => mkConst(code())
        end
        else
        let
            val decArray = Array.array(localCount, CodeZero)
    
            fun addDecs(Declar{addr, value = Container size, ...}) =
                    Array.update(decArray, addr, TupleFromContainer(mkLoad (addr, 0), size))
            |   addDecs(Declar{addr, ...}) = Array.update(decArray, addr, mkLoad (addr, 0))
            |   addDecs(RecDecs decs) = List.app(fn {addr, ...} => Array.update(decArray, addr, mkLoad (addr, 0))) decs
            |   addDecs(NullBinding _) = ()

            val () = List.app addDecs decs

            (* Construct the tuple and add the "general" value at the start. *)
            val resultTuple = mkTuple(gen :: Array.foldr(op ::) nil decArray)
            (* Now generate the machine code and return it as a function that can be called. *)
            val code = codeAndPrint (mkEnv (decs, resultTuple), localCount)
        in
            (* Return a function that executes the compiled code and then creates the
               final "global" value as the result. *)
            fn () =>
                let
                    local
                        (* Execute the code.  This will perform any side-effects the user
                           has programmed and may raise an exception if that is required. *)
                        val resVector = code ()

                        (* The result is a vector containing the "general" value as the
                           first word and the evaluated declarations for any "special"
                           entries in subsequent words. *)
                        val decVals : address =
                            if isShort resVector
                            then raise InternalError "Result vector is not an address"
                            else toAddress resVector
                    in
                        fun resultWordN n = loadWord (decVals, toShort n)
                        (* Get the general value, the zero'th entry in the vector. *)
                        val generalVal = resultWordN 0
                    end

                    (* Construct a new environment so that when an entry is looked 
                       up the corresponding constant is returned. *) 
                    fun newEnviron (oldEnv) args =
                    let
                        val (oldGeneral, oldSpecial) = oldEnv args
            
                        val genVal =
                            case oldGeneral of
                                EnvGenLoad(LoadLocal addr) => resultWordN(addr+1)
                            |   EnvGenConst c => c
                            |   _ => raise InternalError "codetree newEnviron: Not Extract or Constnt"
               
                        val specVal = mapSpec oldSpecial
                    in
                        (EnvGenConst genVal, specVal)
                    end
                    and mapSpec EnvSpecNone = EnvSpecNone
                    |   mapSpec (EnvSpecTuple(size, env)) = EnvSpecTuple(size, newEnviron env)
                    |   mapSpec (EnvSpecInlineFunction(spec, env)) = EnvSpecInlineFunction(spec, (newEnviron env))
                in 
                    (* and return the whole lot as a global value. *)
                    ConstntWithInline(generalVal, mapSpec spec)
                end
        end
    end (* genCode *)


    (* Constructor functions for the front-end of the compiler. *)

    local
        fun mkSimpleFunction inlineType (lval, args, name) =
        let
            val (transCode, maxAddr, nonLocals) = transformAndMaxAddress(lval, args)
        in
            Lambda
              {
                body          = transCode,
                isInline      = inlineType,
                name          = if name = "" then "<anon>" else name,
                closure       = nonLocals,
                argTypes      = List.tabulate(args, fn _ => GeneralType),
                resultType    = GeneralType,
                localCount    = maxAddr + 1
              }
        end
    in
        val mkProc = mkSimpleFunction NonInline (* Normal function *)
        and mkInlproc = mkSimpleFunction MaybeInline (* Explicitly inlined by the front-end *)
        and mkMacroProc = mkSimpleFunction OnlyInline (* Functors *)
    end

    local
        fun mkFunWithTypes inlineType (body, argsAndTypes, resultType, name) =
        let
            val (transCode, maxAddr, nonLocals) =
                transformAndMaxAddress(body, List.length argsAndTypes)
        in
            Lambda
            {
                body          = transCode,
                isInline      = inlineType,
                name          = if name = "" then "<anon>" else name,
                closure       = nonLocals,
                argTypes      = argsAndTypes,
                resultType    = resultType,
                localCount    = maxAddr + 1
            }
        end

    in
        val mkFunction = mkFunWithTypes NonInline
        and mkInlineFunction = mkFunWithTypes MaybeInline
    end

    fun mkCall(func, argsAndTypes, resultType) =
    Eval {
        function = func,
        argList = argsAndTypes,
        resultType=resultType
    }

    fun identityFunction (name : string) : codetree = 
        mkInlproc (mkLoad (~1, 0), 1, name) (* Returns its argument. *);

    structure Sharing =
    struct
        type machineWord = machineWord
        type codetree    = codetree
        type pretty      = pretty
        type argumentType=argumentType
        type codeBinding     = codeBinding
    end

end (* CODETREE functor body *);
