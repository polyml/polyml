(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Modified David C.J. Matthews 2008.

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
    Title:      Poly Make Program.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)

(* This code is now only used during the bootstrap process.  *)

functor MAKE_ (

(*****************************************************************************)
(*                  COMPILERBODY                                             *)
(*****************************************************************************)

structure COMPILERBODY :
sig
    type values;
    type typeConstrs;
    type fixStatus;
    type structVals;
    type signatures;
    type functors;

    type nameSpace =
      { 
        lookupVal:    string -> values option,
        lookupType:   string -> typeConstrs option,
        lookupFix:    string -> fixStatus option,
        lookupStruct: string -> structVals option,
        lookupSig:    string -> signatures option,
        lookupFunct:  string -> functors option,

        enterVal:     string * values      -> unit,
        enterType:    string * typeConstrs -> unit,
        enterFix:     string * fixStatus   -> unit,
        enterStruct:  string * structVals  -> unit,
        enterSig:     string * signatures  -> unit,
        enterFunct:   string * functors    -> unit,

        allVal:       unit -> (string*values) list,
        allType:      unit -> (string*typeConstrs) list,
        allFix:       unit -> (string*fixStatus) list,
        allStruct:    unit -> (string*structVals) list,
        allSig:       unit -> (string*signatures) list,
        allFunct:     unit -> (string*functors) list
      };

    (* The completed compiler. *)
    val compiler :
        nameSpace * (unit->char option) * Universal.universal list -> unit ->
       { fixes: (string * fixStatus) list, values: (string * values) list,
         structures: (string * structVals) list, signatures: (string * signatures) list,
         functors: (string * functors) list, types: (string * typeConstrs) list };

end;

(*****************************************************************************)
(*                  HASHTABLE                                                *)
(*****************************************************************************)
structure HASHTABLE:
sig
  type 'a hash
  type 'a iter
  
  val hashMake: int -> '_a hash
  val hashSet: '_a hash * string * '_a -> unit
  val hashSub: 'a hash * string -> 'a option
  
  (* An iterator over the non-empty entries in the table. *)
  val hashOver: 'a hash -> (string * 'a) iter
end;

(*****************************************************************************)
(*                  UNIVERSALTABLE                                           *)
(*****************************************************************************)
structure UNIVERSALTABLE :
sig
  type 'a tag = 'a Universal.tag;
  type univTable;
  type 'a iter;
  
  val makeUnivTable:  unit -> univTable
  val univEnter:  univTable * 'a tag * string * 'a -> unit;
  val univLookup: univTable * 'a tag * string -> 'a option;
  val univDelete: univTable * 'a tag * string -> unit;
  val univOverSpecific: univTable * 'a tag -> (string * 'a) iter;
end;

(*****************************************************************************)
(*                  STRUCTVALS                                               *)
(*****************************************************************************)
structure STRUCTVALS :
sig
  type 'a tag = 'a Universal.tag;
  
  type types;
  type values;
  type typeConstrs;
  type fixStatus;
  type structVals;
  type signatures;
  type functors;
  
  datatype env = 
    Env of 
      { 
        lookupVal:    string -> values option,
        lookupType:   string -> typeConstrs option,
        lookupFix:    string -> fixStatus option,
        lookupStruct: string -> structVals option,
        lookupSig:    string -> signatures option,
        lookupFunct:  string -> functors option,

        enterVal:     string * values      -> unit,
        enterType:    string * typeConstrs -> unit,
        enterFix:     string * fixStatus   -> unit,
        enterStruct:  string * structVals  -> unit,
        enterSig:     string * signatures  -> unit,
        enterFunct:   string * functors    -> unit
      };

  val valueVar:      values      tag;
  val typeConstrVar: typeConstrs tag;
  val fixVar:        fixStatus   tag;
  val structVar:     structVals  tag;
  val signatureVar:  signatures  tag;
  val functorVar:    functors    tag;
end;

structure MISC :
sig
  type 'a iter
  exception InternalError of string; (* compiler bugs *)

  val revfoldIterator : ('a -> 'b -> 'b) -> 'b -> 'a iter -> 'b
  val mapIterator     : ('a -> 'b) -> 'a iter -> 'b list
  
  val quickSort       : ('a -> 'a -> bool) -> 'a list -> 'a list
  val iterList : 'a iter -> 'a list
end;

structure DEBUG:
sig
    val errorMessageProcTag: (string * bool * int -> unit) Universal.tag
    val compilerOutputTag: (string->unit) Universal.tag
    val lineNumberTag: (unit->int) Universal.tag
    val maxInlineSizeTag: int Universal.tag
end

(*****************************************************************************)
(*                  MAKE sharing constraints                                 *)
(*****************************************************************************)

sharing type
  MISC.iter
= HASHTABLE.iter
= UNIVERSALTABLE.iter

sharing type
  STRUCTVALS.values
= COMPILERBODY.values

sharing type
  STRUCTVALS.structVals
= COMPILERBODY.structVals

sharing type
  STRUCTVALS.typeConstrs
= COMPILERBODY.typeConstrs

sharing type
  STRUCTVALS.fixStatus
= COMPILERBODY.fixStatus

sharing type
  STRUCTVALS.signatures
= COMPILERBODY.signatures

sharing type
  STRUCTVALS.functors
= COMPILERBODY.functors
)
 :

(*****************************************************************************)
(*                  MAKE export signature                                    *)
(*****************************************************************************)
sig
  type env
  type gEnv
  
    type values;
    type typeConstrs;
    type fixStatus;
    type structVals;
    type signatures;
    type functors;

    type nameSpace =
      { 
        lookupVal:    string -> values option,
        lookupType:   string -> typeConstrs option,
        lookupFix:    string -> fixStatus option,
        lookupStruct: string -> structVals option,
        lookupSig:    string -> signatures option,
        lookupFunct:  string -> functors option,

        enterVal:     string * values      -> unit,
        enterType:    string * typeConstrs -> unit,
        enterFix:     string * fixStatus   -> unit,
        enterStruct:  string * structVals  -> unit,
        enterSig:     string * signatures  -> unit,
        enterFunct:   string * functors    -> unit,

        allVal:       unit -> (string*values) list,
        allType:      unit -> (string*typeConstrs) list,
        allFix:       unit -> (string*fixStatus) list,
        allStruct:    unit -> (string*structVals) list,
        allSig:       unit -> (string*signatures) list,
        allFunct:     unit -> (string*functors) list
      };
      
    val compiler : nameSpace * (unit->char option) * Universal.universal list -> unit ->
       { fixes: (string * fixStatus) list, values: (string * values) list,
         structures: (string * structVals) list, signatures: (string * signatures) list,
         functors: (string * functors) list, types: (string * typeConstrs) list };

    val makeGEnv   : unit -> gEnv
    val gEnvAsEnv  : gEnv -> env
    val gEnvAsNameSpace: gEnv -> nameSpace
    val useIntoEnv   : gEnv -> string -> unit       
    val shellProc   : gEnv -> unit -> unit    (* The command processor *)
 end =

let
in

(*****************************************************************************)
(*                  MAKE functor body                                        *)
(*****************************************************************************)
struct
    open MISC;
    type univTable  = UNIVERSALTABLE.univTable;
    type values = STRUCTVALS.values
    type typeConstrs = STRUCTVALS.typeConstrs
    type fixStatus = STRUCTVALS.fixStatus
    type structVals = STRUCTVALS.structVals
    type signatures = STRUCTVALS.signatures
    type functors = STRUCTVALS.functors
    type env = STRUCTVALS.env

    open COMPILERBODY
 
    val eofChar         = Char.chr 4; (* ctrl/D *)

    local
        open UNIVERSALTABLE
        open Thread.Thread
        open Thread.Mutex
    in
        (* Create an environment with a mutex to protect concurrent access. *)
        datatype gEnv = DbEnv of mutex * univTable
       
        (* Lock the mutex during any lookup or entry.  This is primarily to
           avoid the underlying hash table from being rehashed by different
           threads at the same time.  This code should be in a library. *)
        fun protect mutx f =
        let
            (* Turn off interrupts while we have the lock. *)
            val oldAttrs = getAttributes()
            val () = setAttributes[InterruptState InterruptDefer]
              val () = lock mutx
            val result = f()
                handle exn => (unlock mutx; setAttributes oldAttrs; raise exn)
        in
            unlock mutx;
            setAttributes oldAttrs;
            result
        end

        (* Create an environment *)
        fun makeGEnv () : gEnv = DbEnv (mutex(), makeUnivTable()); 

        (* enter a value into an environment *)
        fun dbEnvEnter (DbEnv(mutx, db)) (t : 'a tag) (s : string, v : 'a) : unit =
          protect mutx (fn () => univEnter (db, t, s, v))

        (* find a value in an environment *)
        fun dbEnvLookup (DbEnv(mutx, db)) (t : 'a tag) (s : string) : 'a option =
            protect mutx(fn () => univLookup (db, t, s))

        (* delete an entry, but only from the top-level table *)
        fun dbEnvForget (DbEnv(mutx, db)) (s : string, t : 'a tag) : unit =
          protect mutx (fn () => univDelete (db, t, s))

        fun dbEnvAll (DbEnv(mutx, db)) (t : 'a tag) () : (string * 'a) list =
           protect mutx (fn () => iterList (univOverSpecific (db, t)))

        fun gEnvAsEnv gEnv =
             STRUCTVALS.Env {
                lookupFix    = dbEnvLookup gEnv STRUCTVALS.fixVar,
                lookupVal    = dbEnvLookup gEnv STRUCTVALS.valueVar,
                lookupType   = dbEnvLookup gEnv STRUCTVALS.typeConstrVar,
                lookupSig    = dbEnvLookup gEnv STRUCTVALS.signatureVar,
                lookupStruct = dbEnvLookup gEnv STRUCTVALS.structVar,
                lookupFunct  = dbEnvLookup gEnv STRUCTVALS.functorVar,
                
                enterFix     = dbEnvEnter gEnv STRUCTVALS.fixVar,
                enterVal     = dbEnvEnter gEnv STRUCTVALS.valueVar,
                enterType    = dbEnvEnter gEnv STRUCTVALS.typeConstrVar,
                enterSig     = dbEnvEnter gEnv STRUCTVALS.signatureVar,
                enterStruct  = dbEnvEnter gEnv STRUCTVALS.structVar,
                enterFunct   = dbEnvEnter gEnv STRUCTVALS.functorVar
                };

        fun gEnvAsNameSpace gEnv: nameSpace =
              {
                lookupFix    = dbEnvLookup gEnv STRUCTVALS.fixVar,
                lookupVal    = dbEnvLookup gEnv STRUCTVALS.valueVar,
                lookupType   = dbEnvLookup gEnv STRUCTVALS.typeConstrVar,
                lookupSig    = dbEnvLookup gEnv STRUCTVALS.signatureVar,
                lookupStruct = dbEnvLookup gEnv STRUCTVALS.structVar,
                lookupFunct  = dbEnvLookup gEnv STRUCTVALS.functorVar,

                enterFix     = dbEnvEnter gEnv STRUCTVALS.fixVar,
                enterVal     = dbEnvEnter gEnv STRUCTVALS.valueVar,
                enterType    = dbEnvEnter gEnv STRUCTVALS.typeConstrVar,
                enterSig     = dbEnvEnter gEnv STRUCTVALS.signatureVar,
                enterStruct  = dbEnvEnter gEnv STRUCTVALS.structVar,
                enterFunct   = dbEnvEnter gEnv STRUCTVALS.functorVar,
                
                allFix     = dbEnvAll gEnv STRUCTVALS.fixVar,
                allVal     = dbEnvAll gEnv STRUCTVALS.valueVar,
                allType    = dbEnvAll gEnv STRUCTVALS.typeConstrVar,
                allSig     = dbEnvAll gEnv STRUCTVALS.signatureVar,
                allStruct  = dbEnvAll gEnv STRUCTVALS.structVar,
                allFunct   = dbEnvAll gEnv STRUCTVALS.functorVar
                };
 
    end;

    (*****************************************************************************)
    (*                  useIntoEnv (runcompiler with ML compiler bound in)       *)
    (*****************************************************************************)
    fun useIntoEnv (globalEnv : gEnv) : string -> unit =
    let
        
        val useEnv : nameSpace =
        { 
            lookupFix    = dbEnvLookup globalEnv STRUCTVALS.fixVar,
            lookupVal    = dbEnvLookup globalEnv STRUCTVALS.valueVar,
            lookupType   = dbEnvLookup globalEnv STRUCTVALS.typeConstrVar,
            lookupSig    = dbEnvLookup globalEnv STRUCTVALS.signatureVar,
            lookupStruct = dbEnvLookup globalEnv STRUCTVALS.structVar,
            lookupFunct  = dbEnvLookup globalEnv STRUCTVALS.functorVar,
            enterFix     = dbEnvEnter globalEnv STRUCTVALS.fixVar,
            enterVal     = dbEnvEnter globalEnv STRUCTVALS.valueVar,
            enterType    = dbEnvEnter globalEnv STRUCTVALS.typeConstrVar,
            enterStruct  = dbEnvEnter globalEnv STRUCTVALS.structVar,
            enterSig     = dbEnvEnter globalEnv STRUCTVALS.signatureVar,
            enterFunct   = dbEnvEnter globalEnv STRUCTVALS.functorVar,
            allFix       = dbEnvAll globalEnv STRUCTVALS.fixVar,
            allVal       = dbEnvAll globalEnv STRUCTVALS.valueVar,
            allType      = dbEnvAll globalEnv STRUCTVALS.typeConstrVar,
            allSig       = dbEnvAll globalEnv STRUCTVALS.signatureVar,
            allStruct    = dbEnvAll globalEnv STRUCTVALS.structVar,
            allFunct     = dbEnvAll globalEnv STRUCTVALS.functorVar
        };

        (*****************************************************************************)
        (*                  use                                                      *)
        (*****************************************************************************)
        fun use (fileName : string) =
        let
            val inStream =
                if fileName = ""
                then TextIO.stdIn
                else TextIO.openIn fileName;
            
            val lineNo   = ref 1;
            val eof      = ref false;
            
            fun getChar () : char option =
            case TextIO.input1 inStream of
                eoln as SOME #"\n" =>
                (
                    lineNo := !lineNo + 1;                  
                    eoln
                )
            |   NONE => (eof := true; NONE)
            |   c => c

            fun errorProc (message, hard, line) =
               TextIO.print(concat
                   [if hard then "Error-" else "Warning-",
                    " in '", fileName, "', line ", Int.toString line, ".\n",
                    message])
        in
            (
                while not (! eof) do
                let
                    open DEBUG Universal
                    
                    (* Compile  the code *)
                    val code = COMPILERBODY.compiler
                        (useEnv, getChar,
                          [
                             tagInject compilerOutputTag print,
                             tagInject lineNumberTag (fn () => !lineNo),
                             tagInject errorMessageProcTag errorProc,
                             (* Set the debug level to 50 while compiling the basis.  This makes
                                sure that String.foldr can be expanded inline which reduces the
                                garbage in the hashValue function. *)
                             tagInject maxInlineSizeTag 50
                             (* Need printer environment so that when we compile General.exnMessage
                                it all works. *)
                          ] )
                    (* execute the code and get the resulting declarations. *)
                    val { fixes, values, structures, signatures, functors, types } = code ()
                in
                    (* Just enter the values in the environment without printing. *)
                    List.app (#enterFix useEnv) fixes;
                    List.app (#enterVal useEnv) values;
                    List.app (#enterStruct useEnv) structures;
                    List.app (#enterSig useEnv) signatures;
                    List.app (#enterFunct useEnv) functors;
                    List.app (#enterType useEnv) types
                end
            )
            handle Fail s => (* E.g. syntax error. *)
            (
                TextIO.closeIn inStream;
                raise Fail s
            )
            | exn => (* close inStream if an error occurs *)
            (
                print ("Exception- " ^ General.exnName exn ^ " raised\n");
                TextIO.closeIn inStream;
                raise exn
            );
            
            TextIO.closeIn inStream
        end (* use *)
    in
        use
    end; (* scope of useIntoEnv *)

fun shellProc (globalEnv : gEnv) () : unit = useIntoEnv (globalEnv : gEnv) ""

end (* MAKE *)

end; (* structure-level let *)


