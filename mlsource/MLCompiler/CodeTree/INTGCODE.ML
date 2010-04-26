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
    Title:      Generate interpretable code for Poly system from the code tree.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)

(* This code-generator is primarily intended as a porting aid from one
   machine-code to another. It is modelled on the VAX code-generator and has
   a similar structure. Note: It does not use the use-count information on
   declarations. *)
functor INTGCODE (

(*****************************************************************************)
(*                  CODECONS                                                 *)
(*****************************************************************************)
structure CODECONS :
sig
  type machineWord = Address.machineWord;
  type address = Address.address;
  type code;
  type opcode;
  eqtype addrs; (*hacky! *)
  type labels;
  
  val noJump: labels;
  
  val jumpFalse  : opcode;
  val jump       : opcode;
  val setHandler : opcode;
  val delHandler : opcode;
  
  val addrPlus  : addrs * int -> addrs;
  val addrMinus : addrs * addrs -> int;
  
  val codeCreate: bool * string * Universal.universal list -> code;  (* makes the initial segment. *)
      
  (* ic - Address for the next instruction in the segment. *)
  val ic: code -> addrs;
      
  (* putBytes : puts "length" bytes of "val" into locations "addr", "addr"+1 *)
  val putBytes : int * int * addrs * code -> unit;    
  
  (* GEN- routines all put a value at the instruction counter and add
     an appropriate amount to it. *)

   (* genWord - Puts 2 bytes. *)
   val genWord : int * code -> unit;
      
   (* gen... - put instructions and their operands. *)
   val genCallClosure : code -> unit;
   val genRaiseEx     : code -> unit;
   val genLock        : code -> unit;
   val genLdexc       : code -> unit;
   val genPushHandler : code -> unit;
      
   val genReturn      : int * code -> unit;
   val genGetStore    : int * code -> unit;
   val genLocal       : int * code -> unit;
   val genIndirect    : int * code -> unit;
   val genMoveToVec   : int * code -> unit;
   val genSetStackVal : int * code -> unit;
   val genCase        : int * code -> unit;
   val genTuple       : int * code -> unit;
   
   val genTailCall    : int * int * code -> unit;
   val genNonLocal    : int * int * int * code -> unit;

   (* genEnter instructions are only needed when machine-code routines
      can call interpreted routines or vice-versa. The enterInt instruction
      causes the interpreter to be entered and the argument indicates the
      reason. *)
      
   val genEnterIntCatch : code -> unit;
   val genEnterIntProc  : code * int -> unit;
   val genEnterIntCall  : code * int -> unit;
      
   (* pushConst - Generates code to push a constant. *)
   val pushConst        : machineWord * code -> unit;

   (* genCallSl - Generate callSl instructions which refer to either
       constants or are forward references to procedures which have not yet
       been compiled. *)
   val genCallSl  : int * int * code * code -> unit;
       
   (* genRecRef - Recursive reference to a procedure. *)
   val genRecRef: code * code -> unit

   (* Create a container on the stack *)
   val genContainer : int * code -> unit;

   (* Copy a tuple into a container. *)
   val genSetContainer : int * code -> unit;
 
   (* Create a tuple from a container. *)
   val genTupleFromContainer : int * code -> unit;
     
   (* copyCode - Finish up after compiling a procedure. *)
   val copyCode : code -> address;
   
   (* getBytes - gets "length" bytes from locations "addr", "addr"+1...
      Returns a negative number if the first bit was set. *)
   val getBytes: int * addrs * code -> int;

   (* putBranchInstruction puts in an instruction which involves
      a forward reference. *)
   val putBranchInstruction: opcode * code -> labels;
   
   (* Instruction to delete a handler and skip round it. *)
   val fixup: labels * code -> unit; (* Fix up a forward reference. *)
   
   val linkLabels: labels * labels * code -> labels; (* Link label lists. *)
   val jumpback: addrs * code -> unit; (* Backwards jump. *)
   val resetStack: int * bool * code -> unit; (* Set a pending reset *)
   val alignOffWord: code * int -> unit; (* Add a pad byte if the value would
                                            be word-aligned. *)
end (* CODECONS *);

(*****************************************************************************)
(*                  MISC                                                     *)
(*****************************************************************************)
structure MISC :
sig
  exception InternalError of string;
end;

structure BASECODETREE: BaseCodeTreeSig

) :  

(*****************************************************************************)
(*                  GCODE export signature                                   *)
(*****************************************************************************)
sig
   type codetree
  type machineWord
  val gencode: codetree * Universal.universal list * int -> unit -> machineWord;
end =

let

(*****************************************************************************)
(*                  STRETCHARRAY                                             *)
(*****************************************************************************)
structure STRETCHARRAY :
sig
  type 'a stretchArray;
  
  val stretchArray : int * '_a -> '_a stretchArray;
  val update : '_a stretchArray * int * '_a -> unit;
  val sub    : 'a stretchArray * int -> 'a;
end = StretchArray;

in

(*****************************************************************************)
(*                  GCODE functor body                                       *)
(*****************************************************************************)
struct

  open CODECONS;
  open Address;
  open BASECODETREE;
  open MISC;


  open RuntimeCalls; (* for POLY_SYS numbers *)

  (* gets a value from the run-time system; 
    usually this is a closure, but sometimes it's an int.  *)
  val ioOp : int -> machineWord = RunCall.run_call1 POLY_SYS_io_operation;

  (* minor HACKS *)
  fun forLoop f i n = if i > n then () else (f i; forLoop f (i + 1) n);
  fun apply _ [] = () | apply f (h::t) = (f h; apply f t);
      
  val word0 = toMachineWord 0;
  
  val DummyValue : machineWord = word0; (* used as result of "raise e" etc. *)

  type caseForm =
        {
            cases   : (codetree * word) list,
            test    : codetree,
            caseType: caseType,
            default : codetree
        }
   
   (* Where the result, if any, should go *)
  datatype whereto =
    NoResult     (* discard result *)
  | ToStack     (* Need a result but it can stay on the pseudo-stack *);
  
  (* Are we at the end of the procedure. *)
  datatype tail =
    EndOfProc
  | NotEnd;

  datatype slValue =
    Address    of int  * int (* Address of an entry on the stack. *)
  | StaticLink of code * int (* A static linked procedure. *)
  | Recursive  of code       (* A recursive reference to a closure. *)

  (* Code generate a procedure or global declaration *)
  fun codegen
       (pt : codetree,
        cvec : code,
        loadStaticLink : int * int -> slValue,
        numOfArgs: int, parameters) : address =
  let
    fun matchFailed _ = raise InternalError "codegen: unhandled pattern-match failure"

    val initTrans = 5; (* Initial size of tables. *)
    
    datatype decEntry =
      StackAddr of int
    | ProcConst of code
    | Empty;
    
    val decVec : decEntry STRETCHARRAY.stretchArray =
      STRETCHARRAY.stretchArray (initTrans, Empty);
    
    (* Count of number of items on the stack. *)
    val realstackptr = ref 1; (* The static-link/closure ptr is already there *)
 
    (* Exited - set to true if we have jumped out. *)
    val exited = ref false;
 
    (* Push a value onto the stack. For the moment this merely involves
       adding corresponding values to the real and pseudo-stacks. *)
    fun incsp () : unit = realstackptr := !realstackptr + 1;
 
    (* An entry has been removed from the stack. *)
    fun decsp ()  : unit= realstackptr := !realstackptr - 1;
 
    (* Pushes a local or non-local stack value. *)
    fun pushStackValue (addr : int, level : int) : unit =
    let
      val () =
        if level > 0
        then (* Non-local *)
          genNonLocal(!realstackptr, level, addr, cvec)
        else (* Locals and references to the closure. *)
           genLocal(!realstackptr + addr, cvec);
    in
      incsp ()
    end;


    (* Loads a local, argument or closure value; translating local
       stack addresses to real stack offsets. *)
   fun locaddr ext =
   let
     val locn = #addr ext;
   in
     if #fpRel ext
     then 
       if locn < 0 (* Args. *)
       then pushStackValue (~locn, 0)
       else (* positive address - on the stack. *)
     case STRETCHARRAY.sub (decVec, locn) of
       StackAddr n => pushStackValue (~ n, 0)
 
     | _ => (* Should be on the stack, not a procedure. *)
       raise InternalError "locaddr: bad stack address"
         
     else (* closure-pointer relative *)
    case loadStaticLink (locn, 1) of
      Recursive code =>
      let
        val () = genRecRef (code, cvec)
      in
        incsp ()
      end
      
       | Address (addr, level) =>
       pushStackValue (addr, level)
       
       | StaticLink _ =>
       raise InternalError "locaddr: illegal use of static-link function"
   end; (* locaddr *)

   (* generates code from the tree *)
   fun gencde (pt : codetree, whereto : whereto, tailKind : tail, matchFailFn : unit -> unit, loopAddr) : unit =
   let
     (* Save the stack pointer value here. We may want to reset the stack. *)
     val oldsp = !realstackptr;

     val () =
       case pt of
         MatchFail =>
           (* Leave stack adjustments until later *)
       matchFailFn ()

       | AltMatch (exp1, exp2) => (* A bit like Cond *)
     let
       val failLabs = ref ([] : labels list);
       fun newMatchFailFn () = 
       let
         (* Cut back the stack and branch *)
         val adjustment = !realstackptr - oldsp;
         
         val () =
               if adjustment < 0
                 then raise InternalError ("gencde (AltMatch): bad adjustment " ^ Int.toString adjustment)
               else if !exited orelse adjustment = 0
                 then ()
               else
                 resetStack (adjustment, false, cvec);
                 
             val () = realstackptr := oldsp;

         val thisFailure : labels = putBranchInstruction (jump, cvec);
         val () = exited := true
       in
             failLabs := thisFailure :: !failLabs
       end;
       
       val () = 
          gencde (exp1, whereto, tailKind, newMatchFailFn, loopAddr);
         
       (* Get rid of the result from the stack.
          If there is a result then exp2 will push it. *)
       val () = case whereto of ToStack => decsp () | NoResult => ();
       val exp1Exited : bool = !exited;

       (* If exp1 succeeded, we skip exp2 *)
       val suceedLab : labels = 
         if exp1Exited then noJump else putBranchInstruction (jump, cvec);
         
       val () =
         if !realstackptr = oldsp then ()
         else raise InternalError "gencde: bad stack value"
       
       (* If exp1 failed, we come here (with NO result). *)
       val () = exited := false; (* Don't try to be too clever *)
       val () = apply (fn (lab : labels) => fixup (lab, cvec)) (!failLabs);
       
       (* Compile exp2 using the OLD matchFailFn *)
       val () = gencde (exp2, whereto, tailKind, matchFailFn, loopAddr);

       (* If exp1 succeeded, we merge back in here. *)
       val () = fixup (suceedLab, cvec);
     in
       exited := (!exited andalso exp1Exited)
     end
            
       | Eval evl =>
           genEval (evl, tailKind, matchFailFn)
         
       (* Declarations are not necessarily nested in Newenv *)
       | Declar {value, addr, ...} =>
       (* Put the entry for this declaration in the table. *)
       (case value of
          Lambda (lam as {makeClosure = false, name, ...}) => 
          let
        val newCode : code = codeCreate (true, name, parameters);
        val () =
          STRETCHARRAY.update (decVec, addr, ProcConst newCode);
          in
        genSlProc (lam, newCode)
          end
          
        | _ => (* Other declaration - to the stack. *)
          let
        val () = gencde (value, ToStack, NotEnd, matchFailFn, loopAddr);
          in
        STRETCHARRAY.update (decVec, addr, StackAddr(!realstackptr))
          end
       )

        (* MutualDecs should nested in Newenv??? *)
      | MutualDecs _ =>
            raise InternalError "gencde: MutualDecs only allowed inside Newenv"

       | Extract ext =>
           (* This may just be being used to discard a value which isn't
              used on this branch. *)
           if whereto = NoResult then ()
           else locaddr ext
       
       | Indirect {base, offset} =>
         let
           val () = gencde (base, ToStack, NotEnd, matchFailFn, loopAddr);
         in
           genIndirect (offset, cvec)
         end
       
       | Lambda lam =>
           genProc (lam, false, fn () => (), matchFailFn)
           
       | Constnt w =>
         let
           val () = pushConst (w, cvec);
         in
           incsp ()
         end
  
       | Cond (testPart, thenPart, elsePart) =>
           genCond (testPart, thenPart, elsePart, whereto, tailKind, matchFailFn, loopAddr)
  
       | Newenv vl =>
         let
           (* We can't just call "gencde" on all the elements of the list,
              because declarations must persist as long as they're in scope.
              We treat Declar and MutualDecs specially here, but just naively
              call gencde for expressions.
              SPF 7/1/97
           *)
         
            (* Processes a list of entries. *)
            fun codeList [] = ()
              | codeList (valu :: valus) = 
            let
              val () =
                case valu of
                  MutualDecs dl => genMutualDecs (dl, matchFailFn)
                | Declar _ => gencde (valu, ToStack, NotEnd, matchFailFn, loopAddr)
                | _ =>
                (* If this is the last one it is the result of the block
                   so it is generated to the stack if the block needs a
                   result. *)
                   case valus of
                     [] => gencde (valu, whereto, tailKind, matchFailFn, loopAddr)
                   | _  => gencde (valu, NoResult, NotEnd, matchFailFn, loopAddr)
            in (* process the tail *)
              codeList valus
            end
          in
            codeList vl
          end
          
         | BeginLoop {loop=body, arguments} =>
            (* Execute the body which will contain at least one Loop instruction.
               There will also be path(s) which don't contain Loops and these
               will drop through. *)
            let
                val args = List.map #1 arguments
                (* Evaluate each of the arguments, pushing the result onto the stack. *)
                fun genLoopArg (Declar {addr, value, ...}) =
                    (
                     gencde (value, ToStack, NotEnd, matchFailFn, loopAddr);
                     STRETCHARRAY.update (decVec, addr, StackAddr (!realstackptr));
                     !realstackptr (* Return the posn on the stack. *)
                    )
                |   genLoopArg _ = raise InternalError "genLoopArg: not a declaration"
                val argIndexList = map genLoopArg args;

                val startSp = ! realstackptr; (* Remember the current top of stack. *)
                val startLoop : addrs = ic cvec; (* Start of loop *)
            in
                (* Process the body, passing the jump-back address down for the Loop instruction(s). *)
                gencde (body, whereto, tailKind, matchFailFn, SOME(startLoop, startSp, argIndexList))
                (* Leave the arguments on the stack.  They can be cleared later if needed. *)
            end

         | Loop argList => (* Jump back to the enclosing BeginLoop. *)
            let
                val (startLoop, startSp, argIndexList) =
                    case loopAddr of
                        SOME l => l
                    |   NONE => raise InternalError "No BeginLoop for Loop instr"
                (* Evaluate the arguments.  First push them to the stack because evaluating
                   an argument may depend on the current value of others.  Only when we've
                   evaluated all of them can we overwrite the original argument positions. *)
                fun loadArgs ([], []) = !realstackptr - startSp (* The offset of all the args. *)
                  | loadArgs (arg:: argList, _ :: argIndexList) =
                    let
                        (* Evaluate all the arguments. *)
                        val () = gencde (arg, ToStack, NotEnd, matchFailFn, NONE);
                        val argOffset = loadArgs(argList, argIndexList);
                    in
                        genSetStackVal(argOffset, cvec); (* Copy the arg over. *)
                        decsp(); (* The argument has now been popped. *)
                        argOffset
                    end
                  | loadArgs _ = raise InternalError "loadArgs: Mismatched arguments";

                val _: int = loadArgs(List.map #1 argList, argIndexList)
            in
                if !realstackptr <> startSp
                then resetStack (!realstackptr - startSp, false, cvec) (* Remove any local variables. *)
                else ();
            
                (* Jump back to the start of the loop. *)
                jumpback (startLoop, cvec)
            end
  
       | Raise exp =>
         let
           val () = gencde (exp, ToStack, NotEnd, matchFailFn, loopAddr);
           val () = genRaiseEx cvec;
         in
           exited := true
         end
  
       | Handle {exp, handler} =>
         let
           type handler = labels;
           
           (* Save old handler *)
           val () = genPushHandler cvec;
           val () = incsp ();
  
           val genTag : handler =
           let
             (* Push address of new handler. *)
             val handlerLab : labels = putBranchInstruction (setHandler, cvec);
                 val () = incsp ();
 
             (* Push the exception to be caught.  This is now always zero *)
             val () = gencde (Constnt(toMachineWord 0), ToStack, NotEnd, matchFailFn, loopAddr)
           in
             handlerLab
           end

            val handlerList : handler list = [genTag]

          (* Code generate the body; "NotEnd" because we have to come back
             to remove the handler; "ToStack" because delHandler needs
             a result to carry down. *)
          val () = gencde (exp, ToStack, NotEnd, matchFailFn, loopAddr);
      
          (* Now get out of the handler and restore the old one. *)
          val skipHandler : labels = putBranchInstruction (delHandler, cvec);
          
          (* Now process the handler itself. First we have to reset the stack.
             Note that we have to use "ToStack" again to be consistent with
             the stack-handling in the body-part. If we actually wanted "NoResult",
             the stack adjustment code at the end of gencde will take care
             of this. This means that I don't want to do any clever "end-of-function"
             optimisation either. SPF 6/1/97

         The exception handler must be aligned to an odd word boundary
         so that the garbage collector does not get confused when it
         finds the address on the stack. Values on full-word
         boundaries always point to the start of objects whereas
         values off full-word boundaries (either return addresses or
         catch-phrases) point into code-segments.
          *)
          val () = realstackptr := oldsp;
          val () = exited := false;
            val () = alignOffWord (cvec, 0);
          val () = apply (fn handlerLab => fixup (handlerLab, cvec)) handlerList;
      (* If we were executing machine code we must re-enter the interpreter. *)
          val () = genEnterIntCatch cvec;
          val () = gencde (handler, ToStack, NotEnd, matchFailFn, loopAddr);
          
          (* Finally fix-up the jump around the handler *)
          val () = fixup (skipHandler, cvec);
         in
           exited := false
         end
  
       | Ldexc =>
         let
           (* Get the name of the exception. *)
           val () = genLdexc cvec
         in
           incsp ()
         end
  
       | Case (cas as {cases, caseType, ...}) =>
         let
           val numberOfCases = List.length cases;
            val (isExhaustive, min:int, max:int) =
                case caseType of
                    CaseTag max => (true, 0, Word.toInt max)
                |   _ =>
                    let
                        val (_, aLabel) = hd cases
                        fun foldCases((_, w), (min, max)) = (Word.min(w, min), Word.max(w, max))
                        val (min, max) = List.foldl foldCases (aLabel, aLabel) cases
                    in
                        (false, Word.toInt min, Word.toInt max)
                    end
         in
           if 3 * numberOfCases < max - min + 5
           then genSparseCase(cas, whereto, tailKind, matchFailFn, loopAddr)
           else genDenseCase (cas, whereto, tailKind, matchFailFn, loopAddr)
         end
  
       | Recconstr recList =>
         let
           (* Move the fields into the vector. *)
           fun loadItems [] = ()
             | loadItems (v :: vs) =
             let
               val () = gencde (v, ToStack, NotEnd, matchFailFn, loopAddr);
             in
               loadItems vs
             end;
             
           val size : int = List.length recList;
  
           val () = loadItems recList;
           val () = genTuple (size, cvec);
         in
           realstackptr := !realstackptr - (size - 1)
         end

            (* Containers are supposed to be implemented on the stack for efficiency.
               Unfortunately they cause problems in this code because we assume
               that if we generate a value to the stack it only occupies a single
               word.  Rather than change this code and risk breaking something I'm
               simply generating them as mutable tuples on the heap. *)
        | Container size =>
            (* Reserve a number of words on the stack for use as a tuple on the
               stack.  The result is the address of this space. *)
            (
            genContainer(size, cvec); (* Push the address of this container. *)
            incsp() (* Pushes a single word. *)
            )

        | SetContainer{container, tuple, size} =>
            (* Copy the contents of a tuple into a container.  If the tuple is a
               Recconstr instruction we can avoid generating the tuple and then
               unpacking it and simply copy the fields that make up the tuple
               directly into the container. *)
            (
                (* Load the address of the container. *)
                gencde (container, ToStack, NotEnd, matchFailFn, loopAddr);
            
                case tuple of
                    Recconstr cl =>
                        (* Simply set the container from the values. *)
                    let
                        fun setValue(v, offset) =
                        (
                            gencde (v, ToStack, NotEnd, matchFailFn, loopAddr);
                            (* Move the entry into the container. This instruction
                               pops the value to be moved but not the destination. *)
                            genMoveToVec(offset, cvec);
                            decsp();
                            offset + 1
                        )
                    in
                        List.foldl setValue 0 cl;
                        () (* The container address is still on the stack. *)
                    end

                |   _ =>
                    ( (* General case. *)
                        gencde (tuple, ToStack, NotEnd, matchFailFn, loopAddr);
                        genSetContainer(size, cvec);
                        decsp(); decsp()
                    )

            )

        | TupleFromContainer(container, _) =>
            (* Create a tuple from the contents of a container. *)
            (
                (* TODO: This returns a MUTABLE record which is different from the
                   other code-generators.  That could cause problems if we expect
                   an immutable object. *)
                gencde(container, ToStack, NotEnd, matchFailFn, loopAddr)
            )

        |   TagTest { test, tag, ... } =>
            let
                (* Convert this into a simple equality function. *)
                val code =
                    Eval {
                        function = Constnt(ioOp POLY_SYS_word_eq),
                        argList=[(test, GeneralType), (Constnt(toMachineWord tag), GeneralType)],
                        earlyEval = false, resultType=GeneralType }
            in
                gencde (code, whereto, tailKind, matchFailFn, loopAddr)
            end

        |   KillItems { expression, ... } =>
                (* This is inserted by the higher level code to get the use-counts
                   correct.  Kill entries are Extract entries with lastRef true. *)
                    gencde (expression, whereto, tailKind, matchFailFn, loopAddr)

        |   IndirectVariable _ => 
                raise InternalError "gencde: IndirectVariable"

        |   TupleVariable _ => 
                raise InternalError "gencde: TupleVariable"

        |   CodeNil => 
                raise InternalError "gencde: can't code-generate CodeNil value"
  
        |   Global _ =>
                raise InternalError "gencde: can't code-generate Global value"

    in (* body of gencde *) 

      (* This ensures that there is precisely one item on the stack if
         whereto = ToStack and no items if whereto = NoResult. 
         There are two points to note carefully here:
           (1) Negative stack adjustments are legal if we have exited.
               This is because matchFailFn can cut the stack back too
               far for its immediately enclosing expression. This is
               harmless because the code actually exits that expression.
           (2) A stack adjustment of ~1 is legal if we're generating
               a declaration in "ToStack" mode, because not all declarations
               actually generate the dummy value that we expect. This
               used to be handled in resetStack itself, but it's more
               transparent to do it here. (In addition, there was a bug in
               resetStack - it accumulated the stack resets, but didn't
               correctly accumulate these "~1" dummy value pushes.)
               It's all much better now.
               SPF 9/1/97
     *)
      case whereto of
        ToStack =>
        let
          val newsp = oldsp + 1;
          val adjustment = !realstackptr - newsp;

          val () =
            if !exited orelse adjustment = 0
              then ()
            else if adjustment < ~1
              then raise InternalError ("gencde: bad adjustment " ^ Int.toString adjustment)
            (* Hack for declarations that should push values, but don't *)
            else if adjustment = ~1
              then pushConst (DummyValue, cvec)
            else
              resetStack (adjustment, true, cvec);
        in
          realstackptr := newsp
        end
          
      | NoResult =>
        let
          val adjustment = !realstackptr - oldsp;

          val () =
            if !exited orelse adjustment = 0
              then ()
            else if adjustment < 0
              then raise InternalError ("gencde: bad adjustment " ^ Int.toString adjustment)
            else
              resetStack (adjustment, false, cvec);
        in
          realstackptr := oldsp
        end
    end (* gencde *)

   (* doNext is only used for mutually recursive procedures where a
     procedure may not be able to fill in its closure if it does not have
     all the remaining declarations. *)
   and genProc (lam,
               mutualDecs: bool,
               doNext: unit -> unit,
               matchFailFn : unit -> unit) : unit =
      if #makeClosure lam = false (* static link form *)
      then raise InternalError "static link not allowed here"

      else if (case #closure lam of [] => true | _ => false)
      then let
        val newCode : code = codeCreate(false, #name lam, parameters);
        
        (* The only global references are recursive ones (?) *)
        fun loadRecLink (_ : int, _ : int) : slValue =
          Recursive newCode;
        
        (* Code-gen procedure. No non-local references. *)
         val res : address =
           codegen (#body lam, newCode, loadRecLink, List.length (#argLifetimes lam), parameters);

        val () = pushConst(toMachineWord res, cvec);
        val () = incsp();
      in
        if mutualDecs then doNext () else ()
      end

      else let (* Full closure required. *)
    (* If there is a closure we must fetch all non-local references
       from it. *)

    (* Loads the value from the closure. *)
    fun loadSl (addr : int, level : int) : slValue =
      if addr = 0
      then (* recursive - return address of closure. *)
        Address (~1, level - 1)
      else (* Return an entry in the closure. *)
        Address (addr, level);

        val newCode : code = codeCreate (true, #name lam, parameters);

        (* Code-gen procedure. *)
        val res : address =
          codegen (#body lam, newCode, loadSl, List.length (#argLifetimes lam), parameters);

        val sizeOfClosure = List.length (#closure lam) + 1;
      in
         if mutualDecs
         then let (* Have to make the closure now and fill it in later. *)
           val () = genGetStore (sizeOfClosure, cvec);
           val () = incsp ();
           
           (* Put code address into closure *)
           val () = pushConst(toMachineWord res, cvec); 
           val () = genMoveToVec(0, cvec);
           
           val entryAddr : int = !realstackptr;

           val () = doNext (); (* Any mutually recursive procedures. *)

           (* Push the address of the vector - If we have processed other
              closures the vector will no longer be on the top of the stack. *)
           val () = pushStackValue (~ entryAddr, 0);

           (* Load items for the closure. *)
           fun loadItems ([], _) = ()
             | loadItems (v :: vs, addr : int) =
             let
               (* Generate an item and move it into the vector *)
               val () = gencde (v, ToStack, NotEnd, matchFailed, NONE);
               val () = genMoveToVec(addr, cvec);
               val () = decsp ();
             in
               loadItems (vs, addr + 1)
             end;
             
           val () = loadItems (#closure lam, 1);
           val () = genLock cvec; (* Lock it. *)
           
           (* Remove the extra reference. *)
           val () = resetStack (1, false, cvec);
         in
           realstackptr := !realstackptr - 1
         end
         
         else let
           (* Put it on the stack. *)
           val () = pushConst (toMachineWord res, cvec);
           val () = incsp ();
           
           val () =
             apply (fn (pt: codetree) => gencde (pt, ToStack, NotEnd, matchFailFn, NONE)) (#closure lam);
                
           val () = genTuple (sizeOfClosure, cvec);
         in
           realstackptr := !realstackptr - (sizeOfClosure - 1)
         end
      end

   (* Generate a procedure to be called with a static link. *)
   and genSlProc (lam, newCode: code) : unit =
   let
     (* If a procedure can be called by static link references then
        non-locals can be loaded by following the static chain. The offset
        is the entry in the (pseudo-)closure as with a procedure that
        requires a closure, but these can be translated into real stack
        offsets. *)
     val closureList = #closure lam;

     (* Finds the nth. item in the closure and returns the entry *)
     fun findClosure ((Extract ext) :: _, 1) = ext
       | findClosure (_ :: t, n) = findClosure (t, n - 1)
       | findClosure (_, _) = raise InternalError "findClosure: bad Closure";

     (* Get static link level and offset. *)
     fun loadSl (prevloc : int, level: int) : slValue =
       if prevloc = 0 (* recursive call *)
       then StaticLink (newCode, level)
       else let
     val closureEntry = findClosure (closureList, prevloc);
     val locn = #addr closureEntry; (* Get new address *)
       in
     if #fpRel closureEntry
     then (* This level *)
        if locn < 0 (* Address  *)
        then Address (~ locn, level)
        else (* Local - is it a procedure? *)
          case STRETCHARRAY.sub (decVec, locn) of
        StackAddr n => Address (~ n, level)
          | ProcConst c => StaticLink (c, level)
          | Empty       => raise InternalError "loadSl: missing decVec entry"
      else (* Try the next level *) 
        loadStaticLink (locn, level + 1)
       end; (* loadSl *)
       
     (* Now code-generate the procedure. We can throw away the result because
        it will be assigned into the value we have just returned. *)
     val _ : address =
       codegen (#body lam, newCode, loadSl, List.length (#argLifetimes lam), parameters)
   in
     ()
   end (* genSlProc*)

   and genCond (first: codetree,
                second: codetree,
                third: codetree,
                whereto: whereto,
                tailKind: tail,
                matchFailFn : unit -> unit,
                loopAddr) : unit =
   let
     val () = gencde (first, ToStack, NotEnd, matchFailFn, loopAddr);
     val toElse : labels = putBranchInstruction(jumpFalse, cvec);
     val () = decsp();
   in
     case third of 
       CodeNil => (* No else-part *)
       let
        (* If there is no else-part then the then-part must return a value
           of type void for it to type-check. To save having to pop this
           value to get the stack level right we generate it with ``noresult''
           and the stack resetting mechanism will ensure that a result is
           pushed if it is needed. *)
         val () = gencde (second, NoResult, tailKind, matchFailFn, loopAddr);
         val () = fixup (toElse, cvec) (* Skipped the then-part. *)
       in
         exited := false (* If the test failed we won't have exited. *)
       end
       
     | _ =>
       let
     val () = gencde (second, whereto, tailKind, matchFailFn, loopAddr);
     (* Get rid of the result from the stack. If there is a result then the
        ``else-part'' will push it. *)
     val () = case whereto of ToStack => decsp () | NoResult => ();
     
     val thenExited : bool = !exited;
 
     val toExit : labels =
       if thenExited then noJump
       else putBranchInstruction (jump, cvec);
    
     (* start of "else part" *)
     val () = fixup (toElse, cvec);
     val () = exited := false;
     val () = gencde (third, whereto, tailKind, matchFailFn, loopAddr);
 
     val elseExited : bool= !exited;
     
     val () = fixup (toExit, cvec);
       in
     exited := (thenExited andalso elseExited) (* Only exited if both sides did. *)
       end
   end (* genCond *)

   and genEval (eval, tailKind : tail, matchFailFn : unit -> unit) : unit =
   let
     val argList : codetree list = List.map #1 (#argList eval)
     val argsToPass : int = List.length argList;

     (* Load arguments *)
     fun loadArgs [] = ()
       | loadArgs (v :: vs) =
       let (* Push each expression onto the stack. *)
         val () = gencde(v, ToStack, NotEnd, matchFailFn, NONE);
       in
         loadArgs vs
       end;

     (* Called after the args and the closure to call have been pushed
        onto the stack. *)
     fun callClosure () : unit =
       case tailKind of
         NotEnd => (* Normal call. *)
           genCallClosure cvec
         
       | EndOfProc => (* Tail recursive call. *)
         let
           (* Get the return address onto the top of the stack. *)
           val () = pushStackValue (0, 0);
           
           (* Slide the return address, closure and args over the
              old closure, return address and args, and reset the
              stack. Then jump to the closure. *)
           val () =
             genTailCall(argsToPass + 2, !realstackptr - 1 + (numOfArgs - argsToPass), cvec);
            (* It's "-1" not "-2", because we didn't bump the realstackptr
               when we pushed the return address. SPF 3/1/97 *)
         in
           exited := true
         end;
         
      val () =
    case #function eval of
       (* The procedure is being loaded from the stack or closure so it
          may be a static-link procedure. *)
      Extract ext =>
      let
        (* Since the procedure is on the stack there can be no side-effects
           in loading it. Can therefore load the arguments now. *)
        val () = loadArgs argList;
  
        val staticLinkValue =
          if #fpRel ext
          then let (* Local *)
            val addr : int = #addr ext;
          in
        if addr < 0 (* Address *)
        then Address (~ addr, 0)
        else (* Local - is it a procedure? *)
          case STRETCHARRAY.sub (decVec, addr) of
            StackAddr n => Address (~ n, 0)
          | ProcConst c => StaticLink (c, 0)
          | Empty       => raise InternalError "staticLinkValue: missing decVec entry"
          end
          else (* Non-local or recursive. *) 
        loadStaticLink (#addr ext, 1);
      in
        case staticLinkValue of
          Address (addr, level) =>
          let
            val () = pushStackValue (addr, level);
          in
            callClosure ()
          end
          
          (* recursive reference to a procedure - not static link. *)
        | Recursive code =>
          let
            val () = genRecRef (code, cvec);
            val () = incsp();
          in
            callClosure ()
          end
          
         (* Static link *)
        | StaticLink (code, level) =>
          let
            val () = genCallSl(!realstackptr, level, code, cvec);
          in
            incsp ()
          end
      end (* Extract *)
    
    | _ => (* The procedure is not being found by simply loading a value
          from the stack or the closure. *)
      let
        (* Must load the closure and call. *)
        (* Have to guarantee that the expression to return the procedure
          is evaluated before the arguments. *)
    
        (* Returns true if evaluating it later is safe. *)
        fun safeToLeave (node: codetree) : bool =
          case node of
        Constnt _ => true
          | Lambda _  => true
          | Indirect {base, ...} => 
        (* Safe only if the expression (always a type) being indirected
           is. This is put in because calling procedures in a type is a
           common occurence and should be made reasonably efficient. *)
           safeToLeave base
          | _        => false
    
        val () =
          if (case argList of [] => true | _ => safeToLeave (#function eval))
          then let
        (* Can load the args first. *)
        val () = loadArgs argList;
          in 
        gencde (#function eval, ToStack, NotEnd, matchFailFn, NONE)
          end
          
          else let
        (* The expression for the procedure is too complicated to
           risk leaving. It might have a side-effect and we must
           ensure that any side-effects it has are done before the
           arguments are loaded. *)
        val () = gencde(#function eval, ToStack, NotEnd, matchFailFn, NONE);
        val () = loadArgs(argList);
        (* Load the procedure again. *)
        val () = genLocal(argsToPass, cvec);
          in
        incsp ()
          end
     in
       (* Call the procedure. *)
       callClosure ()
     end; (* Not Extract *)

       (* Make sure we interpret when we return from the call *)
       val () = genEnterIntCall (cvec, argsToPass);

     in (* body of genEval *)
       realstackptr := !realstackptr - argsToPass (* Args popped by caller. *)
     end

   (* This is used for cases which are not sparse i.e. the values occupy
      a narrow enough range to make indexing efficient. *)
   and genDenseCase 
        (pt as { cases, ...}: caseForm,
         whereto: whereto,
         tailKind: tail,
         matchFailFn : unit -> unit,
         loopAddr) : unit =
   let
     (* If there is no default case the results are all
        void for the type-checking to work so we don't
        generate them. *)
     val whereto = case #default pt of CodeNil => NoResult | _ => whereto;
     
     val () = gencde (#test pt, ToStack, NotEnd, matchFailFn, loopAddr);

     (* The exit jumps are chained together. *)
     val lastEndJump : labels ref = ref noJump;

    val (isExhaustive, min:int, max:int) =
        case #caseType pt of
            CaseTag max => (true, 0, Word.toInt max)
        |   _ =>
            let
                val (_, aLabel) = hd cases
                fun foldCases((_, w), (min, max)) = (Word.min(w, min), Word.max(w, max))
                val (min, max) = List.foldl foldCases (aLabel, aLabel) cases
            in
                (false, Word.toInt min, Word.toInt max)
            end

     val limit : int = max - min

     val () =
       if min = 0 then ()
       else let (* Subtract lower limit. *)
         val () = pushConst(toMachineWord min, cvec);
         val () = pushConst(ioOp POLY_SYS_aminus, cvec);
         val () = genCallClosure cvec;
       in
         genEnterIntCall (cvec, 2) (* added SPF 28/6/95 *)
       end;

     val () = genCase (limit, cvec);
     val () = decsp ();
 
     (* Addresses are relative to the first entry in the vector. *)
     val startVec : addrs = ic cvec;
     
     (* Set each address in the vector to point to the default - they
         will be overwritten by the actual address later. *)
     val defaultAddr : int = (limit + 1) * 2;
     
     val () =
       forLoop (fn (_ : int) => genWord (defaultAddr, cvec)) 0 limit;

     (* The default case, if any, follows the case statement. *)
     val () =
       case #default pt of
         CodeNil => ()
       | c       => gencde (c, whereto, tailKind, matchFailFn, loopAddr);

     val () = exited := false;

     (* Now generate the code for each of the cases. *)
     fun genEachCase ([] : (codetree * word) list) : unit = ()
       | genEachCase ((body : codetree, matches : word) :: otherCases) : unit =
       let
     (* First exit from the previous case or the default if
        this is the first. *)
     val () = 
       lastEndJump :=
         linkLabels
           (!lastEndJump,
        if !exited then noJump else putBranchInstruction(jump, cvec),
        cvec);
        
     (* Remove the result - the last case will leave it. *)
     val () = case whereto of ToStack => decsp () | NoResult => ();
     
     (* Now put the address of this code into the table if
        an entry has not already been set. If it has the new
        entry should be ignored. It is NOT an error (just
        redundant). *)
     local
       val entryAddr : addrs = addrPlus(startVec, (Word.toInt matches - min) * 2)
     in
        val () =
       if getBytes(2, entryAddr, cvec) = defaultAddr
       then putBytes(addrMinus(ic cvec, startVec), 2, entryAddr, cvec)
       else ()
     end

     (* Generate code for this case *)
     val () = exited := false;
     val () = gencde (body, whereto, tailKind, matchFailFn, loopAddr);
       in
     genEachCase otherCases
       end; (* genEachCase *)
         
     val () = genEachCase (#cases pt);
     
     (* Finally go down the list of exit labels pointing them to here. *)
     val () = fixup (!lastEndJump, cvec);
   in
     exited := false
   end (* genDenseCase *)

   (* Generate a sparse case. *)
   and genSparseCase
        (pt : caseForm,
         whereto : whereto,
         tailKind : tail,
         matchFailFn : unit -> unit,
         loopAddr) : unit =
   let
     (* Have already dealt with cases where there is only one case. *)
     (* If there is no default case the results are all
        void for the type-checking to work so we don't
        generate them. *)
     val whereto = case #default pt of CodeNil => NoResult | _ => whereto;

      (* The exit jumps are chained together. *)
      val lastEndJump = ref noJump;

      fun caseCode ([], othersExited : bool) : bool = othersExited
        | caseCode ((body : codetree, matches : word) :: otherCases, othersExited : bool) : bool =
      let
        local
          (* Is this really safe? What about multiple side-effects? SPF *)
          val () = gencde (#test pt, ToStack, NotEnd, matchFailFn, loopAddr);
            
          (* Push the value to be compared. *)
          val () = pushConst(toMachineWord matches, cvec);
            
          (* Compare them. *)
          val () =
            pushConst (ioOp (POLY_SYS_word_eq), cvec);
               
          val () = genCallClosure cvec;
          val () = genEnterIntCall(cvec, 2); (* added SPF 28/6/95 *)
        in
          val lab : labels = putBranchInstruction (jumpFalse, cvec);
          val ()     = decsp (); (* Remove result of test. *)
        end
        
        (* Now the expression. *)
        val () = gencde(body, whereto, tailKind, matchFailFn, loopAddr);
        val thisHasExited : bool = !exited;
        
        (* Remove the result - the default case will leave it. *)
        val () = case whereto of ToStack => decsp () | NoResult => ();
        
        val () = 
          lastEndJump :=
             linkLabels
               (!lastEndJump,
                if thisHasExited then noJump else putBranchInstruction(jump, cvec),
                cvec);
                
        (* Now the next case. *)
        val () = fixup (lab, cvec);
        val () = exited := false;
      in
        caseCode (otherCases, othersExited andalso thisHasExited)
      end; (* caseCode *)

      (* First the cases. *)
      val casesExited : bool = caseCode (#cases pt, true);
      
      val () = exited := false;
      
      val () =
        case #default pt of
          CodeNil => ()
        | _ => (* put in the default *)
        let
          val () = gencde(#default pt, whereto, tailKind, matchFailFn, loopAddr);
        in
          exited := (!exited andalso casesExited)
        end;
    in
      (* Finally go down the list of exit labels pointing them to here. *)
      fixup (!lastEndJump, cvec)
    end

   (* Mutually recursive declarations. May be either procedures, constants
      or reccons (from type constructors). Recurse down the list pushing the
      addresses of the closure vectors, then unwind the recursion and fill them in. *)
   and genMutualDecs ([], _) : unit = ()
     | genMutualDecs (Declar decl :: otherDecs, matchFailFn) : unit =
     (
       case #value decl of
         Lambda lam =>
       if not (#makeClosure lam)
       then let (* Static link. *)
         (* Create a code-segment and put it in the table in case of
         mutually recursive references. *)
         val newCode : code = codeCreate(true, #name lam, parameters);
         val () = STRETCHARRAY.update (decVec, #addr decl, ProcConst newCode);
         (* Deal with any other possible references. *)
         val () = genMutualDecs (otherDecs, matchFailFn);
       in
         (* Can now process this procedure since we have made an entry
        in the table for everything it could refer to. *)
          genSlProc (lam, newCode)
       end
       
       else let (* Closure. *)
         fun doRest () : unit  = 
         let
           val () = STRETCHARRAY.update (decVec, #addr decl, StackAddr (! realstackptr));
         in
           (* Now time to do the other closures. *)
           genMutualDecs (otherDecs, matchFailFn)
         end
       in
         genProc (lam, true, doRest, matchFailFn)
       end
     
       | dec => (* constants or reccons.*)
     let
       val () = gencde (dec, ToStack, NotEnd, matchFailFn, NONE);
       val () = STRETCHARRAY.update (decVec, #addr decl, StackAddr (!realstackptr));
     in
       genMutualDecs (otherDecs, matchFailFn)
     end
     )
     | genMutualDecs _ : unit =
        raise InternalError "genMutualDecs: mutual declaration list contains non-declaration";
     
    (* Closure procedures must start with ``enterIntProc''. This is actually a
       break-point instruction in the machine-code instruction set to make sure
       that the code is interpreted. It is a no-op if we are already
       interpreting. *)
    val () = genEnterIntProc (cvec, numOfArgs); (* SPF 23/6/95 *)

   (* Generate the procedure. *)
   (* Assume we always want a result. There is otherwise a problem if the
      called routine returns a result of type void (i.e. no result) but the
      caller wants a result (e.g. the identity function). *)
    val () = gencde (pt, ToStack, EndOfProc, matchFailed, NONE);

    val () = if !exited then () else genReturn (numOfArgs, cvec);
  in (* body of codegen *)
   (* Having code-generated the body of the procedure, it is copied
      into a new data segment. *)
   copyCode cvec
 end (* codegen *);


  fun gencode (pt: codetree, parameters, _) : unit -> machineWord =
    case pt of
      Lambda lam => (* We are compiling a procedure. *)
      let
        (* It is not essential to treat this specially, but it saves generating
           a piece of code whose only function is to return the address of the
           procedure. *)
        (* make the code buffer for the new procedure. *)
        val newCode : code = codeCreate (false, #name lam, parameters);

        (* The only global references are recursive ones (?) *)
        fun loadRecLink (_ : int, _ : int) : slValue =
          Recursive newCode;

        (* This procedure must have no non-local references. *)
        val closureAddr : address =
          codegen (#body lam, newCode, loadRecLink, List.length (#argLifetimes lam), parameters);
            
        val res : machineWord = toMachineWord closureAddr;
      in
        (* Result is a procedure which returns the address of the procedure. *)
        fn () => res
      end
      
   | _ =>
     let (* Compile a top-level expression. *)
       val newCode : code = codeCreate(false, "<top level>", parameters);

        (* There ane *no*  global references at all *)
        fun loadRecLink (_ : int, _ : int) : slValue =
          raise InternalError "top level reached";

       val closureAddr : address =
     codegen (pt, newCode, loadRecLink, 0, parameters)
     in
       (* Result is a procedure to execute the code. *)
       fn () => call (closureAddr, toMachineWord ())
     end; (* gencode *)
end (* GCODE functor body *)

end; (* structure-level let *)
