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

    val isConstnt    = fn (Constnt _)    => true | _ => false;
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

        val (insertedCode, localCount) =
        let
            (* Function to code-generate and run expressions that can be evaluated
               at compile-time. *)
            fun eval(pt, localCount) = evaluate (preCodeAndPrint(pt, localCount), codeGenAndPrint, localCount)
        in
            (* Optimise it. *)
            OPTIMISER.codetreeOptimiser(pt, eval, maxInlineSize)
        end
    
        val gen  = optGeneral insertedCode;
        val spec = optSpecial insertedCode;
        val decs = optDecs insertedCode;
    
        val notSpecial =
            case spec of
                Lambda{isInline, ...} => isInline = NonInline
           |    CodeNil => true
           |    _ => false
    in
        if notSpecial 
        then
        let
            (* Nothing special or it is a non-inline procedure - Code-generate it. *)
            val optCode = mkEnv(decs, gen)
        in
            if isConstnt optCode (* Save code-generating it. *)
            then (fn () => optCode)
            else
            let
                val code = codeGenAndPrint (preCodeAndPrint(optCode, localCount), localCount)
            in (* Return procedure to execute it *)
                fn () => Global (simpleOptVal (mkConst (code ())))
            end
        end
        
        else (* There is something in "special". *)
            if null decs
        then  (* Simply return it - it can have no non-local references. *)
            (fn () => Global insertedCode)
    
        else
        let
            (* We have some declarations to evaluate but we can't do that until
               we execute the code. Expand out any mutual declarations and
               remove any expressions which are being evaluated only for their
               side-effects. *)
         
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

            fun getValues [] _ : codetree list =
                [] (* Last of all the general value. *)
          
            |   getValues (decs as ((declAddr, _) :: vs)) (addr: int): codetree list =
                if declAddr < addr (* Already done? *)
                then getValues vs addr (* remove *)
                else
                let
                    fun findEntry [] = CodeZero (* Not found. *)
                    |   findEntry ((dAddr, value) :: rest) =
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
                in
                    findEntry decs  :: getValues decs (addr + 1)
                end

             fun expandMutual [] = []
              | expandMutual (RecDecs dec :: decs) = map(fn{addr, lambda, ...} => (addr, Lambda lambda)) dec @ expandMutual decs
              | expandMutual ((Declar{addr, value, ...}) :: decs) =
                    (addr, value) :: expandMutual decs
              | expandMutual (_ :: decs) =
                    expandMutual decs; (* expression *)
   
            val ext     = gen :: getValues (expandMutual decs) 1
            val newDecs = mkEnv (decs, mkTuple ext)
            val code    = codeGenAndPrint (preCodeAndPrint(newDecs, localCount), localCount)
        in (* We now have the values of the declarations. *)
            fn () =>
                let
                    (* Execute the code - the result is a vector with the
                       declarations in it. *)
                    val res = code ()

                    val decVals : address =
                        if isShort res
                        then raise InternalError "Result vector is not an address"
                        else toAddress res
        
                    (* Construct a new environment so that when an entry is looked 
                       up the corresponding constant is returned. *) 
                    fun newEnviron oldEnv (lval, depth, levels) =
                    let
                        val oldVal = oldEnv (lval, depth, levels)
          
                        (* Get the constant out of the table. *)
                        fun look (Extract{addr, ...}) = 
                            mkConst (loadWord (decVals, toShort addr))
            
                        |   look (g as Indirect{base, offset}) =
                            let
                                val v   = look base
                            in
                                case v of
                                    Constnt caddr => mkConst(loadWord (toAddress caddr, toShort offset))
                                | _ => g
                            end
            
                        |   look g = g
           
                        val specVal = optSpecial oldVal
           
                        val envVal =
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
                    end (* newEnviron *)
            
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
