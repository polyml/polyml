(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Modified David C. J. Matthews 2008-2010

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

(* Signature for the basic codetree types and operations. *)
signature BaseCodeTreeSig =
sig
    type machineWord = Address.machineWord
    
    datatype inlineStatus =
        NonInline
    |   MaybeInline
    |   SmallFunction
    |   OnlyInline

    datatype argumentType =
        GeneralType
    |   FloatingPtType

    datatype codetree =
        MatchFail    (* Pattern-match failure *)
    
    |   AltMatch of codetree * codetree(* Pattern-match alternative choices *)

    |   Newenv of codeBinding list * codetree (* Set of bindings with an expression. *)

    |   Constnt of machineWord (* Load a constant *)

    |   Extract of (* Get a local variable, an argument or a closure value *)
        { (* Load a value. *)
            addr : int, 
            level: int, 
            fpRel: bool
        }
    
    |   Indirect of {base: codetree, offset: int }
        (* Load a value from a heap record *)

    |   Eval of (* Evaluate a function with an argument list. *)
        {
            function:  codetree,
            argList:   (codetree * argumentType) list,
            resultType: argumentType
        }
    
    |   Lambda of lambdaForm (* Lambda expressions. *)

    |   Cond of codetree * codetree * codetree (* If-statement *)
    
    |   BeginLoop of (* Start of tail-recursive inline function. *)
        { loop: codetree, arguments: (simpleBinding * argumentType) list }

    |   Loop of (codetree * argumentType) list (* Jump back to start of tail-recursive function. *)

    |   Raise of codetree (* Raise an exception *)

    |   Ldexc (* Load the exception (used at the start of a handler) *)
    
    |   Handle of (* Exception handler. *) { exp: codetree, handler: codetree }

    |   Recconstr of codetree list (* Records (tuples) *)

    |   Container of int (* Create a container for a tuple on the stack. *)
    
    |   SetContainer of { container: codetree, tuple: codetree, size: int}
         (* Copy a tuple to a container. *)
    
    |   TupleFromContainer of codetree * int (* Make a tuple from the contents of a container. *)

    |   TagTest of { test: codetree, tag: word, maxTag: word }

    |   IndirectVariable of { base: codetree, offset: codetree }
        (* Similar to Indirect except the offset is a variable. *)

    |   TupleVariable of varTuple list * codetree (* total length *)
        (* Construct a tuple using one or more multi-word items. *)

    |   Global of globalVal (* Global value *)

    |   CodeNil

    and globalVal =
        (* A global value is a constant but it may also contain the code for an
           inline function or a tuple of (tuples of) inline functions along
           with an environment to map the free variables.  We could get rid of
           the environment by transforming the inlinable code so that it
           had no free variables (they're always constants after the code
           has been run). *)
        GVal of machineWord * (codetree * (loadForm * int * int -> globalVal)) option

    and codeBinding =
        Declar  of simpleBinding (* Make a local declaration or push an argument *)
    |   RecDecs of { addr: int, lambda: lambdaForm } list (* Set of mutually recursive declarations. *)
    |   NullBinding of codetree (* Just evaluate the expression and discard the result. *)

    and varTuple =
        VarTupleSingle of { source: codetree, destOffset: codetree }
    |   VarTupleMultiple of
            { base: codetree, length: codetree, destOffset: codetree, sourceOffset: codetree }

    withtype loadForm = 
    { (* Load a value. *)
        addr : int, 
        level: int, 
        fpRel: bool
    }
    
    and simpleBinding = 
    { (* Declare a value or push an argument. *)
        value:      codetree,
        addr:       int
    }

    and lambdaForm =
    { (* Lambda expressions. *)
        body          : codetree,           (* The body of the function. *)
        isInline      : inlineStatus,       (* Whether it's inline - modified by optimiser *)
        name          : string,             (* Text name for profiling etc. *)
        closure       : codetree list,      (* List of items for closure/static link.  Added by preCode. *)
        argTypes      : argumentType list,  (* "Types" of arguments. *)
        resultType    : argumentType,       (* Result "type" of the function. *)
        level         : int,                (* Nesting depth.  Added by optimiser. *)
        localCount    : int                (* Maximum (+1) declaration address for locals.  Added by optimiser. *)
    }

    (* Return the "size" of the codetree used as a way of estimating whether to insert
       the body inline.  If the bool is true this includes the size of sub-functions
       in the calculation, if false they are excluded. *)
    val codeSize : codetree * bool -> int

    type pretty
    val pretty : codetree -> pretty

    structure Sharing:
    sig
        type codetree = codetree
        and  globalVal = globalVal
        and  pretty = pretty
        and  inlineStatus = inlineStatus
        and  argumentType = argumentType
        and  varTuple = varTuple
        and  codeBinding = codeBinding
        and  simpleBinding = simpleBinding
    end

end;
