(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Modified David C. J. Matthews 2008

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
    
    |  AltMatch of codetree * codetree(* Pattern-match alternative choices *)

    | Declar of declarForm (* Make a local declaration or push an argument *)
     
    | Newenv of codetree list (* Start a block *)

    | Constnt of machineWord (* Load a constant *)

    | Extract of loadForm (* Get a local variable, an argument or a closure value *)
    
    | Indirect of 
    { (* Load a value from a heap record *)
        base:   codetree,
        offset: int
    }
    
    | Eval of (* Evaluate a function with an argument list. *)
    {
        function:  codetree,
        argList:   (codetree * argumentType) list,
        resultType: argumentType,
        earlyEval: bool
    }
    
    | Lambda of lambdaForm (* Lambda expressions. *)
    
    | MutualDecs of codetree list (* Set of mutually recursive declarations. *)

    | Cond of codetree * codetree * codetree (* If-statement *)

    | Case of (* Case expressions *)
        {
            cases   : (codetree * word) list,
            test    : codetree,
            caseType: caseType,
            default : codetree
        }
    
    | BeginLoop of codetree * (codetree * argumentType) list(* Start of tail-recursive inline function. *)

    | Loop of (codetree * argumentType) list (* Jump back to start of tail-recursive function. *)

    | Raise of codetree (* Raise an exception *)

    | Ldexc (* Load the exception (used at the start of a handler) *)
    
    | Handle of (* Exception handler *)
    { (* Exception handler. *)
        exp      : codetree,
        taglist  : codetree list,
        handler  : codetree
    }
    
    | Recconstr of codetree list (* Records (tuples) *)

    | Container of int (* Create a container for a tuple on the stack. *)
    
    | SetContainer of (* Copy a tuple to a container. *)
    {
        container: codetree,
        tuple:     codetree,
        size:      int
    }
    
    | TupleFromContainer of codetree * int (* Make a tuple from the contents of a container. *)

    | TagTest of { test: codetree, tag: word, maxTag: word }

    | Global of optVal (* Global value *)

    | CodeNil
    
    and optVal = (* Global values - Also used in the optimiser. *)
        JustTheVal of codetree
    
    |   ValWithDecs of {general : codetree, decs : codetree list}
    
    |   OptVal of
    {
        (* Expression to load this value - always a constant in global values. *)
        general : codetree,
        (* If it is not CodeNil it is the code which generated the general
           value - either an inline procedure, a type constructor or a tuple. *)
        special : codetree,
        (* Environment for the special value. *)
        environ : loadForm * int * int -> optVal,
        (* Declarations to precede the value - Always nil for global values. *)
        decs : codetree list,
        (* A reference which is used to detect recursive inline expansions. *)
        recCall: bool ref
    }

    and caseType =
        CaseInt
    |   CaseWord
    |   CaseTag of word
    
    withtype loadForm = 
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
    
    and diadic = codetree * codetree
    
    and triadic =  codetree * codetree * codetree
    
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
        makeClosure   : bool
    }

    val isSmall : codetree * int -> bool

    type pretty
    val pretty : codetree -> pretty

    structure Sharing:
    sig
        type codetree = codetree
        and  optVal = optVal
        and  caseType = caseType
        and  pretty = pretty
    end

end;
