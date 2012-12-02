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

structure DEBUG :
sig
    val codetreeTag:            bool Universal.tag (* If true then print the original code. *)
    val codetreeAfterOptTag:    bool Universal.tag (* If true then print the optimised code. *)
    val maxInlineSizeTag:       int  Universal.tag
    val getParameter :
       'a Universal.tag -> Universal.universal list -> 'a
end (* DEBUG *);

structure PRETTY : PRETTYSIG

structure BASECODETREE: BaseCodeTreeSig

structure CODETREE_FUNCTIONS: CodetreeFunctionsSig

structure LIFETIMES:
sig
    type codetree
    val lifeTimes: codetree * int -> codetree
    structure Sharing: sig type codetree = codetree end
end

structure STATIC_LINK_AND_CASES:
sig
    type codetree
    val staticLinkAndCases: (codetree * int -> unit -> Address.machineWord) * codetree * int -> codetree
    structure Sharing: sig type codetree = codetree end
end

structure OPTIMISER:
sig
    type codetree and optVal
    val codetreeOptimiser: codetree  * (codetree * int -> codetree) * int -> optVal * int
    structure Sharing: sig type codetree = codetree and optVal = optVal end
end

sharing type
    PRETTY.pretty
=   BASECODETREE.pretty

sharing type
  BASECODETREE.codetree
= GCODE.codetree

sharing
    BASECODETREE.Sharing 
=   CODETREE_FUNCTIONS.Sharing
=   LIFETIMES.Sharing
=   STATIC_LINK_AND_CASES.Sharing
=   OPTIMISER.Sharing

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
    open CODETREE_FUNCTIONS
  
    exception InternalError = Misc.InternalError
    and Interrupt = Thread.Thread.Interrupt
  
    infix 9 sub;

    val isCodeNil    = fn CodeNil        => true | _ => false; (* Exported *)

    (* gets a value from the run-time system *)
    val ioOp : int -> machineWord = RunCall.run_call1 POLY_SYS_io_operation;
    
    (* since code generator relies on these representations,
       we may as well export them *)
    val rtsFunction = mkConst o ioOp

    type casePair = 
       (* Expressions and corresponding list of labels. *)
       codetree * int list

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

    (* Look for static link functions and convert conditionals to cases then
       compute the lifetimes of declarations. *)
    fun preCode (codegen, pt, localAddressCount) =
    let
    
        val lifeTimes = LIFETIMES.lifeTimes
        and staticLinkAndCases = STATIC_LINK_AND_CASES.staticLinkAndCases

        fun pass1Cg(pt, localCount) = codegen(lifeTimes(pt, localCount), localCount)
        val pass1 = staticLinkAndCases(pass1Cg, pt, localAddressCount)
    in
        lifeTimes(pass1, localAddressCount)
    end

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

        (* Optimise it. *)

        val (optimisedCode, localCount) =
        let
            (* Function to code-generate and run expressions that can be evaluated
               at compile-time. *)
            fun eval(pt, localCount) = evaluate (preCodeAndPrint(pt, localCount), codeGenAndPrint, localCount)
        in
            (* Optimise it. *)
            OPTIMISER.codetreeOptimiser(pt, eval, maxInlineSize)
        end

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
        val gen  = optGeneral optimisedCode
        val spec = optSpecial optimisedCode
        val decs = optDecs optimisedCode

        local
            (* See if there is an inline function in there. *)
            fun containsFunction _ (_, true) = true
            |   containsFunction _ (Lambda _, _) = true
            |   containsFunction _ (Constnt _, _) = false
            |   containsFunction _ (CodeNil, _) = false
            |   containsFunction optEnv (Recconstr tuple, _) = List.foldl(containsFunction optEnv) false tuple
            |   containsFunction optEnv (Extract ext, _) =
                let
                    val newVal = optEnv(ext, 0, 0)
                in
                    containsFunction(optEnviron newVal) (optSpecial newVal, false)
                end
            |   containsFunction _ _ = raise InternalError "containsFunction"

        in
            val hasFunction = containsFunction(optEnviron optimisedCode) (spec, false)
        end

    in
        if not hasFunction
        then
        let
            val code = codeGenAndPrint (preCodeAndPrint(mkEnv(decs, gen), localCount), localCount)
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

            (* Construct the tuple and drop the first entry which is never used.  Replace
               it by the "general" value. *)
            val tupleEntries = List.tl(Array.foldr(op ::) nil decArray)
 
            val resultTuple = mkTuple(gen :: tupleEntries)
            (* Now generate the machine code and return it as a function that can be called. *)
            val code = codeGenAndPrint (preCodeAndPrint(mkEnv (decs, resultTuple), localCount), localCount)
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
                    fun newEnviron oldEnv args =
                    let
                        val oldVal = oldEnv args
                    
                        val genVal =
                            case optGeneral oldVal of
                                Extract{addr, ...} => mkConst (resultWordN addr)
                            |   c as Constnt _ => c
                            |   _ => raise InternalError "codetree newEnviron: Not Extract or Constnt"
         
                        val specVal = optSpecial oldVal

                        (* If we return a special value here we need to construct a
                           suitable environment for it which wraps the original environment
                           with this one. *)
                        val envVal =
                            if isCodeNil specVal
                            then errorEnv
                            else newEnviron (optEnviron oldVal)
                   
                        val () = (* Check the decs are empty. *)
                            case optDecs oldVal of
                                [] => ()
                            |   _ => raise InternalError "codetree decVal: Not empty"
                    in                       
                        optVal 
                        {
                            general = genVal,
                            special = specVal,
                            environ = envVal,
                            decs    = [],
                            recCall = optRec oldVal
                        }
                    end (* newEnviron *)
        
                in 
                    (* and return the whole lot as a global value. *)
                    Global
                      (optVal 
                         {
                           general = mkConst generalVal,
                           special = spec,
                           environ = newEnviron (optEnviron optimisedCode),
                           decs    = [],
                           recCall = optRec optimisedCode
                         })
                end
        end

    end (* genCode *)


    (* Constructor functions for the front-end of the compiler. *)

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
