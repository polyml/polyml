(*
    Copyright (c) 2012,13,15 David C.J. Matthews

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
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
    val codeGenerate:
        codetree * int * Universal.universal list -> (unit -> machineWord) * Universal.universal list
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
    fun rtsFunction v = Constnt(ioOp v, [])
 
    fun mkDec (laddr, res) = Declar{value = res, addr = laddr, use=[]}

    fun deExtract(Extract ext) = ext | deExtract _ = raise InternalError "deExtract"
 
    datatype level =
        Level of { lev: int, closure: createClosure, lookup: int * int * bool -> loadForm }

    local
        (* We can have locals at the outer level. *)
        fun bottomLevel(addr, 0, false) =
            if addr < 0 then raise InternalError "load: negative"
            else LoadLocal addr
        |   bottomLevel _ = (* Either the level is wrong or it's a parameter. *)
                raise InternalError "bottom level"
    in
        val baseLevel =
            Level { lev = 0, closure = makeClosure(), lookup = bottomLevel }
    end
  
    fun newLevel (Level{ lev, lookup, ...}) =
    let
        val closureList = makeClosure()
        val makeClosure = addToClosure closureList

        fun check n = if n < 0 then raise InternalError "load: negative" else n

        fun thisLevel(addr, level, isParam) =
        if level < 0 then raise InternalError "mkLoad: level must be non-negative"
        else if level > 0
        then makeClosure(lookup(addr, level-1, isParam))
        else (* This level *) if isParam
        then LoadArgument(check addr)
        else LoadLocal(check addr)
    in
        Level { lev = lev+1, closure = closureList, lookup = thisLevel }
    end
    
    fun getClosure(Level{ closure, ...})  = List.map Extract (extractClosure closure)
 
    fun mkLoad (addr, Level { lev = newLevel, lookup, ... } , Level { lev = oldLevel, ... }) =
        Extract(lookup(addr, newLevel - oldLevel, false))

    and mkLoadParam(addr, Level { lev = newLevel, lookup, ... } , Level { lev = oldLevel, ... }) =
        Extract(lookup(addr, newLevel - oldLevel, true))

    (* Transform a function so that free variables are converted to closure form.  Returns the
       maximum local address used. *)

    fun genCode(pt, debugSwitches, numLocals) =
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

        (* Optimise it. *)
        val { numLocals = localCount, general = gen, bindings = decs, special = spec } =
            OPTIMISER.codetreeOptimiser(pt, debugSwitches, numLocals)

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
           Previously, this code always generated a tuple containing every
           declaration.  That led to some very long compilation times because
           the back-end has some code which is quadratic in the number of entries
           on the stack.  We now try to prune bindings by only generating the tuple
           if we have an inline function somewhere and only generating bindings
           we actually need. *)
        fun simplifySpec (EnvSpecTuple(size, env)) =
            let
                (* Get all the field entries. *)
                fun simpPair (gen, spec) = (gen, simplifySpec spec)
                val fields = List.tabulate(size, simpPair o env)
            in
                if List.all(fn (_, EnvSpecNone) => true | _ => false) fields
                then EnvSpecNone
                else EnvSpecTuple(size, fn n => List.nth(fields, n))
            end
        |   simplifySpec s = s (* None or inline function. *)

    in
        case simplifySpec spec of
            EnvSpecNone =>
            let
                val (code, props) = codeAndPrint (mkEnv(decs, gen), localCount)
            in
                fn () => Constnt(code(), props)
            end

        |   simpleSpec  =>
            let
                (* The bindings are marked using a three-valued mark.  A binding is needed
                   if it is referenced in any way.  During the scan to find the references
                   we need to avoid processing an entry that has already been processed but
                   it is possible that a binding may be referenced as a general value only
                   (e.g. from a function closure) and separately as a special value.  See
                   Test148.ML *)
                datatype visit = UnVisited | VisitedGeneral | VisitedSpecial

                local
                    val refArray = Array.array(localCount, UnVisited)

                    fun findDecs EnvSpecNone = ()

                    |   findDecs (EnvSpecTuple(size, env)) =
                        let
                            val fields = List.tabulate(size, env)
                        in
                            List.app processGenAndSpec fields
                        end

                    |   findDecs (EnvSpecInlineFunction({closure, ...}, env)) =
                        let
                            val closureItems = List.tabulate(List.length closure, env)
                        in
                            List.app processGenAndSpec closureItems
                        end

                    and processGenAndSpec (gen, spec) =
                        (* The spec part needs only to be processed if this entry has
                           not yet been visited, *)
                        case gen of
                            EnvGenLoad(LoadLocal addr) =>
                                let
                                    val previous = Array.sub(refArray, addr)
                                in
                                    case (previous, spec) of
                                        (VisitedSpecial, _) => () (* Fully done *)
                                    |   (VisitedGeneral, EnvSpecNone) => () (* Nothing useful *)
                                    |   (_, EnvSpecNone) =>
                                            (* We need this entry but we don't have any special
                                               entry to process.  We could find another reference with a
                                               special entry. *)
                                            Array.update(refArray, addr, VisitedGeneral)
                                    |   (_, _) =>
                                            (
                                                (* This has a special entry.  Mark it and process. *)
                                                Array.update(refArray, addr, VisitedSpecial);
                                                findDecs spec
                                            )
                                end
                        |   EnvGenConst _ => ()
                        |   _ => raise InternalError "doGeneral: not LoadLocal or Constant"

                    val () = findDecs simpleSpec
                in
                    (* Convert to an immutable data structure.  This will continue
                       to be referenced in any inline function after the code has run. *)
                    val refVector = Array.vector refArray
                end
 
                val decArray = Array.array(localCount, CodeZero)
                
                fun addDec(addr, dec) =
                    if Vector.sub(refVector, addr) <> UnVisited then Array.update(decArray, addr, dec) else ()
    
                fun addDecs(Declar{addr, ...}) = addDec(addr, mkLoadLocal addr)
                |   addDecs(RecDecs decs) = List.app(fn {addr, ...} => addDec(addr, mkLoadLocal addr)) decs
                |   addDecs(NullBinding _) = ()
                |   addDecs(Container{addr, size, ...}) = addDec(addr, mkTupleFromContainer(addr, size))

                val () = List.app addDecs decs

                (* Construct the tuple and add the "general" value at the start. *)
                val resultTuple = mkTuple(gen :: Array.foldr(op ::) nil decArray)
                (* Now generate the machine code and return it as a function that can be called. *)
                val (code, codeProps) = codeAndPrint (mkEnv (decs, resultTuple), localCount)
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
                               first word and the evaluated bindings for any "special"
                               entries in subsequent words. *)
                            val decVals : address =
                                if isShort resVector
                                then raise InternalError "Result vector is not an address"
                                else toAddress resVector
                        in
                            fun resultWordN n = loadWord (decVals, n)
                            (* Get the general value, the zero'th entry in the vector. *)
                            val generalVal = resultWordN 0w0
                            (* Get the properties for a field in the tuple.  Because the result is
                               a tuple all the properties should be contained in a tupleTag entry. *)
                            val fieldProps =
                                case Option.map (Universal.tagProject CodeTags.tupleTag)
                                        (List.find(Universal.tagIs CodeTags.tupleTag) codeProps) of
                                    NONE => (fn _ => [])
                                |   SOME p => (fn n => List.nth(p, n))
                            val generalProps = fieldProps 0
                        end

                        (* Construct a new environment so that when an entry is looked 
                           up the corresponding constant is returned. *) 
                        fun newEnviron (oldEnv) args =
                        let
                            val (oldGeneral, oldSpecial) = oldEnv args
            
                            val genPair =
                                case oldGeneral of
                                    EnvGenLoad(LoadLocal addr) =>
                                        (
                                            (* For the moment retain this check.  It's better to have an assertion
                                               failure than a segfault. *)
                                            Vector.sub(refVector, addr) <> UnVisited orelse raise InternalError "Reference to non-existent binding";
                                            (resultWordN(Word.fromInt addr+0w1), fieldProps(addr+1))
                                        )
                                |   EnvGenConst c => c
                                |   _ => raise InternalError "codetree newEnviron: Not Extract or Constnt"
               
                            val specVal = mapSpec oldSpecial
                        in
                            (EnvGenConst genPair, specVal)
                        end
                        and mapSpec EnvSpecNone = EnvSpecNone
                        |   mapSpec (EnvSpecTuple(size, env)) = EnvSpecTuple(size, newEnviron env)
                        |   mapSpec (EnvSpecInlineFunction(spec, env)) = EnvSpecInlineFunction(spec, (newEnviron env))
                    in 
                        (* and return the whole lot as a global value. *)
                        Constnt(generalVal, setInline(mapSpec simpleSpec) generalProps)
                    end
            end
    end (* genCode *)


    (* Constructor functions for the front-end of the compiler. *)
    local
        fun mkSimpleFunction inlineType (lval, args, name, closure, numLocals) =
              {
                body          = lval,
                isInline      = inlineType,
                name          = if name = "" then "<anon>" else name,
                closure       = map deExtract closure,
                argTypes      = List.tabulate(args, fn _ => (GeneralType, [])),
                resultType    = GeneralType,
                localCount    = numLocals,
                recUse        = []
              }
    in
        val mkProc = Lambda o mkSimpleFunction NonInline (* Normal function *)
        and mkInlproc = Lambda o mkSimpleFunction Inline (* Explicitly inlined by the front-end *)

        (* Unless Compiler.inlineFunctor is false functors are treated as macros and expanded
           when they are applied.  Unlike core-language functions they are not first-class
           values so if they are inline the "value" returned in the initial binding can just
           be zero except if there is something in the closure. Almost always
           the closure will be empty since free variables will come from previous topdecs and will
           be constants,  The exception is if a structure and a functor using the structure appear
           in the same topdec (no semicolon between them).  In that case we can't leave it.  We
           would have to update the closure even if we leave the body untouched but we could
           have closure entries that are constants.
           e.g. structure S = struct val x = 1 end functor F() = struct open S end *)
        fun mkMacroProc (args as (_, _, _, [], _)) =
            Constnt(toMachineWord 0,
                setInline (
                    EnvSpecInlineFunction(mkSimpleFunction Inline args,
                        fn _ => raise InternalError "mkMacroProc: closure")) [])

        |   mkMacroProc args = Lambda(mkSimpleFunction Inline args)
    end

    (* Used if we import a functor into the name space. *)
    fun importFunctor(Lambda{body, name, closure, argTypes, localCount, ...}) =
            mkMacroProc(body, List.length argTypes, name, map Extract closure, localCount)
    |   importFunctor _ = raise Fail "importFunctor: code is not a function"

    local
        fun mkFunWithTypes inlineType { body, argTypes=argsAndTypes, resultType, name, closure, numLocals } =
            Lambda
            {
                body          = body,
                isInline      = inlineType,
                name          = if name = "" then "<anon>" else name,
                closure       = map deExtract closure,
                argTypes      = map (fn t => (t, [])) argsAndTypes,
                resultType    = resultType,
                localCount    = numLocals,
                recUse        = []
            }
    in
        val mkFunction = mkFunWithTypes NonInline
        and mkInlineFunction = mkFunWithTypes Inline
    end

    fun mkEval (ct, clist)   =
    Eval {
        function = ct,
        argList = List.map(fn c => (c, GeneralType)) clist,
        resultType=GeneralType
    }

    fun mkCall(func, argsAndTypes, resultType) =
    Eval {
        function = func,
        argList = argsAndTypes,
        resultType=resultType
    }

    local
        open RuntimeCalls
    in
        fun mkNot arg = mkEval (rtsFunction POLY_SYS_not_bool, [arg])
        val testptreqFunction    = rtsFunction POLY_SYS_word_eq

        (* N.B. int equality is SHORT integer equality *)
        fun mkTestinteq (xp1, xp2) = 
            mkEval (rtsFunction POLY_SYS_word_eq, [xp1,xp2]);

        fun mkTestptreq  (xp1, xp2) = mkEval (testptreqFunction, [xp1,xp2]);
    end

    fun identityFunction (name : string) : codetree = 
        mkInlproc (mkLoadArgument 0, 1, name, [], 0) (* Returns its argument. *);
  
    (* Test a tag value. *)
    fun mkTagTest(test: codetree, tagValue: word, maxTag: word) =
        TagTest {test=test, tag=tagValue, maxTag=maxTag }
        (*mkEval (rtsFunction POLY_SYS_word_eq, [test, Constnt(toMachineWord tagValue)], true);*)

    fun mkHandle (exp, handler) = Handle {exp = exp, handler = handler};

    fun mkStr (strbuff:string) = Constnt (toMachineWord strbuff, [])

  (* If we have multiple references to a piece of code we may have to save
     it in a temporary and then use it from there. If the code has side-effects
      we certainly must do that to ensure that the side-effects are done
      exactly once and in the correct order, however if the code is just a
      constant or a load we can reduce the amount of code we generate by
      simply returning the original code. *)
    fun multipleUses (code as Constnt _, _, _) = 
        {load = (fn _ => code), dec = []}
(*
    |   multipleUses (code as Extract(LoadLegacy{addr, level=loadLevel, ...}), _, level) = 
        let (* May have to adjust the level. *)
            fun loadFn lev =
                if lev = level
                then code 
                else mkLoad (addr, loadLevel + lev, level))
        in
            {load = loadFn, dec = []}
        end

    |   multipleUses (code as Extract(LoadLocal addr), _, level) = 
        let (* May have to adjust the level. *)
            fun loadFn lev =
                if lev = level
                then code 
                else mkLoad (addr, lev - level)
        in
            {load = loadFn, dec = []}
        end

    |   multipleUses (code as Extract(LoadArgument _), _, level) = 
        let (* May have to adjust the level. *)
            fun loadFn lev =
                if lev = level
                then code
                else raise InternalError "multipleUses: different level"
                (*else mkLoad (addr, lev - level)*)
        in
            {load = loadFn, dec = []}
        end

    |   multipleUses (Extract _, _, _) = raise InternalError "multipleUses: TODO"
*)
    |   multipleUses (code, nextAddress, level) = 
        let
            val addr       = nextAddress();
            fun loadFn lev = mkLoad (addr, lev, level);
        in
            {load = loadFn, dec = [mkDec (addr, code)]}
        end (* multipleUses *);

    fun mkMutualDecs [] = raise InternalError "mkMutualDecs: empty declaration list"
    |   mkMutualDecs l =
    let
        fun convertDec(a, Lambda lam) = {lambda = lam, addr = a, use=[]}
        |   convertDec _ = raise InternalError "mkMutualDecs: Recursive declaration is not a function"
    in
        RecDecs(List.map convertDec l)
    end

    val mkNullDec = NullBinding
    
    fun mkContainer(addr, size, setter) =
        Container{addr=addr, size=size, use=[], setter=setter}

    val mkIf                = Cond
    and mkRaise             = Raise

    fun mkConst v = Constnt(v, [])

    (* For the moment limit these to general arguments. *)
    fun mkLoop args = Loop (List.map(fn c => (c, GeneralType)) args)
    and mkBeginLoop(exp, args) =
        BeginLoop{loop=exp, arguments=List.map(fn(i, v) => ({value=v, addr=i, use=[]}, GeneralType)) args}

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

    val mkSetContainer =
        fn (container, tuple, size) => mkSetContainer(container, tuple, BoolVector.tabulate(size, fn _ => true))

    local
        open Asn1

        val currentVersion = 1
        
        fun mapEnc _ [] = []
        |   mapEnc f (hd :: tl) = f hd @ mapEnc f tl
    in

        fun encodeBinary (c: codetree) =
        let
            fun encodeArgType at = [encodeInt(case at of GeneralType => 0 | FloatingPtType => 1)]

            fun encode (Newenv(decs, exp)) =
                let
                    fun encodeBinding(Declar{value, addr, ...}) =
                            encodeItem(Application(11, Constructed),
                                encode value @ encodeItem(Context(1, Primitive), [encodeInt addr]))

                    |   encodeBinding(RecDecs l) =
                        let
                            (* Encode as pairs of lambda and address. *)
                            fun processDec [] = []
                            |   processDec({ addr, lambda, ... }::rest) =
                                    encode(Lambda lambda) @ encodeItem(Context(1, Primitive), [encodeInt addr]) @ processDec rest
                        in
                            encodeItem(Application(12, Constructed), processDec l)
                        end

                    |   encodeBinding(NullBinding exp) = encode exp

                    |   encodeBinding(Container{addr, size, setter, ...}) =
                            encodeItem(Application(13, Constructed),
                                encode setter @ 
                                encodeItem(Context(1, Primitive), [encodeInt addr]) @ 
                                encodeItem(Context(2, Primitive), [encodeInt size]))
                in
                    encodeItem(Universal(16, Constructed), mapEnc encodeBinding(decs @ [NullBinding exp]))
                end

            |   encode (Constnt(v, m)) =
                (
                    case findInline m of
                        EnvSpecInlineFunction(lambda as {closure=[], ...}, _) =>
                            (* If it's been compiled the closure should be empty (unless it's mutually recursive?)
                               but because we aren't looking at the context we should probably check. *)
                            encode(Lambda lambda)
                    |   _ =>
                        if isShort v
                        then encodeItem(asn1Integer, [encodeInt(Word.toInt(toShort v))])

                        else
                        let
                            val addr = toAddress v
                        in
                            if isIoAddress addr
                            then (* RTS call - This can be exported. *)
                            let
                                fun matchIo n =
                                    if n = 256 then raise Fail "Unknown RTS entry"
                                    else if wordEq (v, ioOp n)
                                    then encodeItem(Application(17, Primitive), [encodeInt n])
                                    else matchIo (n+1)
                            in
                                matchIo 0
                            end
                            
                            else if isMutable addr
                            then raise Fail ("Cannot encode address-" ^ stringOfWord v)

                            else if isWords addr
                            then
                            let
                                (* Unwrap tuples *)
                                val len = Word.toInt(Address.length addr)
                                val fields = List.tabulate(len, fn n => mkConst(loadWord(addr, Word.fromInt n)))
                            in
                                encode(Tuple { isVariant = false, fields = fields })
                            end

                            else if isBytes addr andalso Address.length addr >= 0w2
                                (* Byte data is often but not always strings.  Assume it
                                   is a string if it looks right.  If it doesn't it could be
                                   real or long precision but in that case just fail. *)
                            then
                            let
                                val bytes = Word.toInt(Address.length addr) * wordSize
                                val asString: string = RunCall.unsafeCast addr
                                val possSize = String.size asString
                            in
                                if possSize <= bytes - wordSize andalso possSize > bytes - 2*wordSize
                                then encodeItem(Universal(4, Primitive), [Byte.stringToBytes asString])
                                else raise Fail ("Cannot encode address-" ^ stringOfWord v)
                            end

                            else raise Fail ("Cannot encode address-" ^ stringOfWord v)
                        end
                )

            |   encode (Extract(LoadArgument i)) =
                    encodeItem(Application(6, Primitive), [encodeInt i])

            |   encode (Extract(LoadLocal i)) =
                    encodeItem(Application(7, Primitive), [encodeInt i])

            |   encode (Extract(LoadClosure i)) =
                    encodeItem(Application(8, Primitive), [encodeInt i])

            |   encode (Extract LoadRecursive) =
                    encodeItem(Application(9, Primitive), [])

            |   encode (Indirect{base, offset, isVariant}) =
                    encodeItem(Application(10, Constructed),
                        encode base @
                        encodeItem(Context(1, Primitive), [encodeInt offset]) @
                        (if isVariant
                        then encodeItem(Context(2, Primitive), [encodeBool isVariant])
                        else []))

            |   encode (Eval { function, argList, resultType}) =
                    encodeItem(Application(16, Constructed),
                        encode function @
                        encodeItem(Context(1, Primitive), encodeArgType resultType) @
                        mapEnc (fn (a, at) =>
                            encodeItem(Context(2, Constructed),
                                encode a @ encodeItem(Universal(2, Primitive), encodeArgType at))
                            ) argList
                    )

            |   encode (Lambda { body, isInline, name, closure, argTypes, resultType, localCount, ...}) =
                let
                    (* When encoding the sequence of arg types we have to wrap each item. *)
                    fun encodeAt at =
                        encodeItem(Universal(2, Primitive), encodeArgType at)
                in
                    encodeItem(Application(15, Constructed),
                        encode body @
                        encodeItem(Context(1, Primitive), [encodeBool(isInline = Inline)]) @
                        encodeItem(Context(2, Primitive), [encodeString name]) @
                        encodeItem(Context(3, Constructed), mapEnc (encode o Extract) closure) @
                        encodeItem(Context(4, Constructed), mapEnc (encodeAt o #1) argTypes) @
                        encodeItem(Context(5, Primitive), encodeArgType resultType) @
                        encodeItem(Context(6, Primitive), [encodeInt localCount])
                        )
                end

            |   encode (Cond(i, t, e)) =
                    encodeItem(Application(3, Constructed), encode i @ encode t @ encode e)

            |   encode (BeginLoop{loop, arguments}) =
                let
                    fun encodeArg ({addr, value, ...}, at) =
                        encodeItem(Context(1, Constructed),
                            encode value @
                            encodeItem(Context(2, Primitive), [encodeInt addr]) @
                            encodeItem(Universal(2, Primitive), encodeArgType at)
                        )
                in
                    encodeItem(Application(19, Constructed), encode loop @ mapEnc encodeArg arguments)
                end

            |   encode (Loop l) =
                let
                    fun encodeArg(c, at) =
                        encodeItem(Context(1, Constructed),
                            encode c @
                            encodeItem(Universal(2, Primitive), encodeArgType at)
                        )                        
                in
                    encodeItem(Application(20, Constructed), mapEnc encodeArg l)
                end

            |   encode (Raise r) = encodeItem(Application(4, Constructed), encode r)

            |   encode (Ldexc) = encodeItem(Application(2, Primitive), [])

            |   encode (Handle{exp, handler}) =
                    encodeItem(Application(5, Constructed), encode exp @ encode handler)

            |   encode (Tuple { fields, isVariant }) =
                let
                    val encFields = mapEnc encode fields
                in
                    encodeItem(Application(14, Constructed),
                        if isVariant
                        then encodeItem(Context(1, Primitive), [encodeBool isVariant]) @ encFields
                        else encFields
                    )
                end

            |   encode (SetContainer { container, tuple, filter, ...}) =
                    encodeItem(Application(21, Constructed), 
                        encode container @ encode tuple @ 
                        encodeItem(Context(1, Primitive), [encodeInt(BoolVector.length filter)]))

            |   encode (TagTest{test, tag, maxTag}) =
                    encodeItem(Application(18, Constructed),
                        encodeItem(Context(1, Primitive), [encodeInt(Word.toInt tag)]) @
                        encodeItem(Context(2, Primitive), [encodeInt(Word.toInt maxTag)]) @
                        encode test)

            val encodeVersion =
                encodeItem(Application(0, Primitive), [encodeInt currentVersion])
        in
            Word8Vector.concat(encodeVersion @ encode c)
        end

        (* Parse a sequence of items until the data are used up. *)
        fun splitSequence v =
            case decodeItem v of
                SOME{tag, data, remainder} =>
                    (tag, data) :: splitSequence remainder
            |   NONE => []

        fun decodeArgType d =
            if decodeInt d = 0 then GeneralType else FloatingPtType

        fun decodeCodetree(Universal(2, _), data) =
            let
                val v = decodeInt data
                val mv = toMachineWord v
            in
                (* We don't know if this is an integer or a word so it's only safe
                   to import it as a short-precision value.  That's only an issue if
                   it has been exported on a machine with a longer word length. *)
                if not (isShort mv)
                then raise Fail ("Constant " ^ Int.toString v ^ " is too large")
                else mkConst mv
            end

        |   decodeCodetree(Application(2, _), _) = Ldexc

        |   decodeCodetree(Application(3, _), data) =
            let
                val (i, t, e) =
                    case splitSequence data of
                        [a, b, c] => (decodeCodetree a, decodeCodetree b, decodeCodetree c)
                    |   _ => raise Fail "Format Error - Cond"
            in
                Cond (i, t, e)
            end

        |   decodeCodetree(Application(4, _), data) =
            let
                val r =
                    case splitSequence data of
                        [a] => decodeCodetree a
                    |   _ => raise Fail "Format Error - Raise"
            in
                Raise r
            end

        |   decodeCodetree(Application(5, _), data) =
            let
                val (e, h) =
                    case splitSequence data of
                        [a, b] => (decodeCodetree a, decodeCodetree b)
                    |   _ => raise Fail "Format Error - Handle"
            in
                Handle{exp=e, handler=h}
            end

        |   decodeCodetree(Application(6, _), data) =
                Extract(LoadArgument(decodeInt data))

        |   decodeCodetree(Application(7, _), data) =
                Extract(LoadLocal(decodeInt data))

        |   decodeCodetree(Application(8, _), data) =
                Extract(LoadClosure(decodeInt data))

        |   decodeCodetree(Application(9, _), _) =
                Extract LoadRecursive

        |   decodeCodetree(Application(10, _), data) =
            let
                val (base, offset, isVar) =
                    case splitSequence data of
                        a :: (Context(1, _), d1) :: tl =>
                        let
                            val isVar =
                                case tl of
                                    [] => false
                                |   [(Context(2, _), d2)] => decodeBool d2
                                |   _ => raise Fail "Format Error - Indirect"
                        in
                            (decodeCodetree a, decodeInt d1, isVar)
                        end
                    |   _ => raise Fail "Format Error - Indirect"
            in
                (if isVar then mkVarField else mkInd)(offset, base)
            end

        |   decodeCodetree(Universal(16, _), data) =  (* ASN1 sequence - Newenv *)
            let
                fun processItem(Application(11, _), data) =
                    let
                        val (v, a) =
                            case splitSequence data of
                                [v, (Context(1, _), ad)] => (decodeCodetree v, decodeInt ad)
                            |   _ => raise Fail "Format Error - Declar"
                    in
                        Declar{value=v, addr=a, use=[]}
                    end

                |   processItem(Application(12, _), data) =
                    let
                        fun processItems [] = []
                        |   processItems (lamb :: (Context(1, _), ad) :: rest) =
                            (
                                case decodeCodetree lamb of
                                    Lambda lambda =>
                                        {lambda=lambda, addr=decodeInt ad, use=[]} :: processItems rest
                                |   _ => raise Fail "Format error - Recdec"
                            )
                        |   processItems _ = raise Fail "Format error - Recdec"
                    in
                        RecDecs (processItems (splitSequence data))
                    end

                |   processItem(Application(13, _), data) =
                    let
                        val (set, a, siz) =
                            case splitSequence data of
                                [v, (Context(1, _), ad), (Context(2, _), sd)] =>
                                    (decodeCodetree v, decodeInt ad, decodeInt sd)
                            |   _ => raise Fail "Format Error - Container"
                    in
                        Container{addr=a, size=siz, setter=set, use=[]}
                    end

                |   processItem d = NullBinding(decodeCodetree d)

                val processedBindings = map processItem (splitSequence data)

                fun splitLast _ [] = raise Fail "decSequenceWithFinalExp: empty"
                |   splitLast decs [NullBinding exp] = (List.rev decs, exp)
                |   splitLast _ [_] = raise Fail "decSequenceWithFinalExp: last is not a NullDec"
                |   splitLast decs (hd::tl) = splitLast (hd:: decs) tl

            in
                mkEnv(splitLast [] processedBindings)
            end

        |   decodeCodetree(Application(14, _), data) =
            let
                (* If the "variant" tag is missing treat it as false. *)
                val (isVar, fields) = 
                    case splitSequence data of
                        (Context(1, _), v) :: tl => (decodeBool v, map decodeCodetree tl)
                    |   d => (false, map decodeCodetree d)
            in
                if isVar then mkDatatype fields else mkTuple fields
            end

        |   decodeCodetree(Application(15, _), data) =
            let
                fun deExtract(Extract e) = e | deExtract _ = raise Fail "Format Error - closure"

                val (body, isInl, name, closure, at, rt, lc) =
                    case splitSequence data of
                        [bd, (Context(1, _), id), (Context(2, _), nd), (Context(3, _), cd),
                        (Context(4, _), ad), (Context(5, _), rd), (Context(6, _), ld)] =>
                        (decodeCodetree bd,
                         if decodeBool id then Inline else NonInline,
                         decodeString nd,
                         map (deExtract o decodeCodetree) (splitSequence cd),
                         map (fn (_, n) => (decodeArgType n, [])) (splitSequence ad),
                         decodeArgType rd,
                         decodeInt ld)
                    |   _ => raise Fail "Format Error - Tuple"
            in
                Lambda { body = body, isInline = isInl, name = name, closure = closure,
                         argTypes = at, resultType = rt, localCount = lc, recUse = [] }
            end

        |   decodeCodetree(Application(16, _), data) =
            let
                fun decodeArg(Context(2, _), ad) =
                    (
                        case splitSequence ad of
                            [argd, (_, argtd)] => (decodeCodetree argd, decodeArgType argtd)
                        |   _ => raise Fail "Format Error - Eval arg"
                    )
                |   decodeArg _ = raise Fail "Format Error - Eval arg"

                val (func, rt, args) =
                    case splitSequence data of
                        fd :: (Context(1, _), ad) :: args =>
                        (decodeCodetree fd,
                        decodeArgType ad,
                        map decodeArg args
                        )
                    |   _ => raise Fail "Format Error - Eval"
            in
                Eval { function = func, resultType = rt, argList = args }
            end

        |   decodeCodetree(Application(17, _), data) = mkConst(ioOp(decodeInt data))

        |   decodeCodetree(Application(18, _), data) =
            let
                val (tag, maxTag, test) =
                    case splitSequence data of
                        [(Context(1, _), td), (Context(2, _), mtd), testd] =>
                            (Word.fromInt(decodeInt td), Word.fromInt(decodeInt mtd), decodeCodetree testd)
                    |   _ => raise Fail "Format Error - TagTest"
            in
                mkTagTest(test, tag, maxTag)
            end

        |   decodeCodetree(Universal(4, _), data) = (* A string *)
                mkConst(toMachineWord(Byte.bytesToString(Word8VectorSlice.vector data)))

        |   decodeCodetree(Application(19, _), data) =
            let
                fun decodeArg(Context(1, _), ad) =
                    (
                        case splitSequence ad of
                            [argd, (_, addrd), (_, argtd)] =>
                                ({ value=decodeCodetree argd, addr=decodeInt addrd, use=[] }, decodeArgType argtd)
                        |   _ => raise Fail "Format Error - BeginLoop arg"
                    )
                |   decodeArg _ = raise Fail "Format Error - BeginLoop arg"

                val (loop, args) =
                    case splitSequence data of
                        loopD :: argData => (decodeCodetree loopD, map decodeArg argData)
                    |   _ => raise Fail "Format Error - BeginLoop"
            in
                BeginLoop{loop=loop, arguments=args}
            end

        |   decodeCodetree(Application(20, _), data) =
            let
                fun decodeArg(Context(1, _), ad) =
                    (
                        case splitSequence ad of
                            [argd, (_, argtd)] => (decodeCodetree argd, decodeArgType argtd)
                        |   _ => raise Fail "Format Error - Loop arg"
                    )
                |   decodeArg _ = raise Fail "Format Error - Loop arg"
            in
                Loop (map decodeArg (splitSequence data))
            end

        |   decodeCodetree(Application(21, _), data) =
            let
                val (container, tuple, size) =
                    case splitSequence data of
                        [cData, tData, (Context(1, _), sData)] =>
                            (decodeCodetree cData, decodeCodetree tData, decodeInt sData)
                    |   _ => raise Fail "Format Error - SetContainer"
            in
                mkSetContainer(container, tuple, size)
            end

        |   decodeCodetree(t, _) = raise Fail ("Format error - unknown tag: " ^ PolyML.makestring t)

        fun decodeBinary (v: Word8Vector.vector) =
            case decodeItem(Word8VectorSlice.full v) of
                SOME{tag=Application(0, _), data, remainder} =>
                let
                    val v = decodeInt data
                in
                    if v = currentVersion
                    then
                    (
                        case splitSequence remainder of
                            item :: _ => decodeCodetree item
                        |   [] => raise Fail "Format error"
                    )
                    else raise Fail (concat["Incorrect version ", Int.toString v, "<>", Int.toString currentVersion])
                end

            |   _ => raise Fail "Format error"

    end

    structure Sharing =
    struct
        type machineWord = machineWord
        type codetree    = codetree
        type pretty      = pretty
        type argumentType=argumentType
        type codeBinding     = codeBinding
        type level       = level
    end

end (* CODETREE functor body *);
