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
    Title:      Poly Make Program.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)

(* Compiles a Poly program by compiling only those parts which have changed
  since the last compilation. Works by calling the compiler with an
  environment arranged so that when an item is looked up the compiler is
  entered recursively to (re)compile it if it has changed (or has never
  been compiled before). *)

functor MAKE_ (

(*****************************************************************************)
(*                  DEBUG                                                    *)
(*****************************************************************************)
structure DEBUG :
sig
  val suffixes : string list ref;
  val bindName : string ref;
  val useRCS   : bool ref;
  val prompt1  : string ref;
  val prompt2  : string ref;
  val printString : (string->unit) ref;
  val inputChar : (unit -> char option) ref;
  val nextIsNewline : (unit->bool) ref;
  val flushInput : (unit->unit) ref;
end;

(*****************************************************************************)
(*                  LEX                                                      *)
(*****************************************************************************)
structure LEX :
sig
  type lexan
  val initial: (unit -> char) * string * (unit -> int) * (string -> unit) -> lexan;
  val flushLexan:    lexan -> unit;
end;


(*****************************************************************************)
(*                  COMPILERBODY                                             *)
(*****************************************************************************)

structure COMPILERBODY :
sig
  type env
  type lexan
  type values
  type word
  val compiler :
      lexan *               (* the I/O environment *)
      env *                (* the current global environment *)
	  (word -> values option) -> (* Look-up function for top-level exceptions. *)
      unit -> unit;         (* the function to actually execute *)
end;

(*****************************************************************************)
(*                  UNIVERSAL                                                *)
(*****************************************************************************)
structure UNIVERSAL :
sig
  type 'a tag
  val tag : unit -> '_a tag
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
  val hashSub: 'a hash * string -> 'a
  
  (* An iterator over the non-empty entries in the table. *)
  val hashOver: 'a hash -> (string * 'a) iter
end;

(*****************************************************************************)
(*                  UNIVERSALTABLE                                           *)
(*****************************************************************************)
structure UNIVERSALTABLE :
sig
  type 'a tag;
  type univTable;
  type 'a iter;
  
  val makeUnivTable:  unit -> univTable
  val univEnter:  univTable * 'a tag * string * 'a -> unit;
  val univLookup: univTable * 'a tag * string -> 'a;
  val univDelete: univTable * 'a tag * string -> unit;
  val univOverSpecific: univTable * 'a tag -> (string * 'a) iter;
end;

(*****************************************************************************)
(*                  STRUCTVALS                                               *)
(*****************************************************************************)
structure STRUCTVALS :
sig
  type 'a tag;
  
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
	lookupVal:    string -> values,
	lookupType:   string -> typeConstrs,
	lookupFix:    string -> fixStatus,
	lookupStruct: string -> structVals,
	lookupSig:    string -> signatures,
	lookupFunct:  string -> functors,
	lookupTvars:  string -> types,
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

(* DCJM 8/8/00.  Previously Misc was a global but we aren't allowed
   to have sharing constraints with globals in ML97.  We could use a
   "where type" constraint but then we couldn't bootstrap from ML90. *)
structure MISC :
sig
  type 'a iter
  exception ValueMissing of string;  (* table lookup failure *)
  exception InternalError of string; (* compiler bugs *)
  exception Failure of string;       (* static errors or run-time error *)

  val revfold        : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b

  val revfoldIterator : ('a -> 'b -> 'b) -> 'b -> 'a iter -> 'b
  val mapIterator     : ('a -> 'b) -> 'a iter -> 'b list
  
  val quickSort       : ('a -> 'a -> bool) -> 'a list -> 'a list

end;

(*****************************************************************************)
(*                  VALUE_OPS                                                *)
(*****************************************************************************)
structure VALUE_OPS :
sig
  type univTable
  type word
  type values
  val findException: univTable -> word -> values option
end;


(*****************************************************************************)
(*                  MAKE sharing constraints                                 *)
(*****************************************************************************)

sharing type
  UNIVERSAL.tag
= UNIVERSALTABLE.tag
= STRUCTVALS.tag

sharing type
   COMPILERBODY.env
 = STRUCTVALS.env

sharing type
   LEX.lexan
 = COMPILERBODY.lexan

sharing type
  MISC.iter
= HASHTABLE.iter
= UNIVERSALTABLE.iter

sharing type
  STRUCTVALS.values
= COMPILERBODY.values
= VALUE_OPS.values

sharing type
  COMPILERBODY.word
= VALUE_OPS.word

sharing type
  UNIVERSALTABLE.univTable
= VALUE_OPS.univTable
) :

(*****************************************************************************)
(*                  MAKE export signature                                    *)
(*****************************************************************************)
sig
  type dbEnv
  type univTable
  type values (* I don't really want this here but it's easier that way. *)
  
  val makeDbEnv   : univTable -> dbEnv
  val makeSubshellDbEnv : dbEnv -> dbEnv
  val makeIntoEnv  : dbEnv -> string -> unit
  val useIntoEnv   : dbEnv -> string -> unit
  val dependsInEnv : dbEnv -> string -> unit
  val dependenciesInEnv : dbEnv -> string -> string list
  val runCompiler : dbEnv -> 
       (unit -> string) * (string -> unit) * (unit-> int) * string -> unit -> unit
       
  val shellProc   : (dbEnv * string ref * string ref) -> 
    {
     shell:      unit -> unit,    (* The command processor *)
	 debugger:	 (string -> values) * (unit->bool) -> unit
    };
    
  val forgetMakeInfo  : dbEnv -> string -> unit;
  val forgetSignature : dbEnv -> string -> unit;
  val forgetStructure : dbEnv -> string -> unit;
  val forgetFunctor   : dbEnv -> string -> unit;
  val forgetValue     : dbEnv -> string -> unit;
  val forgetType      : dbEnv -> string -> unit;
  val forgetFixity    : dbEnv -> string -> unit;
  
  val signatureNames : dbEnv -> unit -> string list;
  val structureNames : dbEnv -> unit -> string list;
  val functorNames   : dbEnv -> unit -> string list;
  val valueNames     : dbEnv -> unit -> string list;
  val typeNames      : dbEnv -> unit -> string list;
  val fixityNames    : dbEnv -> unit -> string list;
end =

let
in

(*****************************************************************************)
(*                  MAKE functor body                                        *)
(*****************************************************************************)
struct
  open MISC;
  open DEBUG;
  type univTable  = UNIVERSALTABLE.univTable;
  type values = STRUCTVALS.values

  (* Define a local version of "max", because it causes a lot of
     emulation traps, and I want to be sure it's *this* module
     that's the culprit. (Timestamps are *long* integers.)
     SPF 19/12/95
  *)
  fun max (x : Time.time) (y : Time.time) : Time.time = 
    if x < y then y else x;

  val eofChar         = Char.chr 4; (* ctrl/D *)
  
  (* A VERY dangerous function - use with extreme care *) 
  fun shrink_stack (newsize : int) : unit = 
    RunCall.run_call1 RuntimeCalls.POLY_SYS_shrink_stack newsize

  exception ObjNotFile;
    
  type 'a tag = 'a UNIVERSAL.tag;
  
  fun splitFilename (name: string) : string * string =
  	 let
	 	val {dir, file } = OS.Path.splitDirFile name
	 in
	 	(dir, file)
	 end

(*****************************************************************************)
(*                  dbEnv abstype                                            *)
(*****************************************************************************)
(* The aim of this is to allow a local name space to be added over the top
   of an existing one.  It is used primarily in the debugger but also in the
   secondary shell (if that hasn't been removed!) *)
  local
    open UNIVERSALTABLE
	open Thread.Thread
	open Thread.Mutex
  in
    abstype dbEnv = DbEnv of mutex * univTable list
    with
      (* turn a univTable into an environment *)
      fun makeDbEnv (utab : univTable) : dbEnv = DbEnv (mutex(), [utab]); 

		(* Lock the mutex during any lookup or entry.  This is primarily to
		   avoid the underlying hash table from being rehashed by different
		   threads at the same time.  This code should be in a library.

		   This isn't completely right.  If an asynchronous interrupt
		   arrives while we've set the thread to handle interrupts
		   synchronously we won't see the interrupt at least not
		   until we next go into the RTS. *)
	  	fun protect mutx f =
		let
		    (* Set this to handle interrupts synchronously while we have
			   the lock.  An asynchronous interrupt could arrive at the
			   wrong moment. *)
		    val oldAttrs = getAttributes()
			val () = setAttributes[InterruptState InterruptSynch]
	  		val () = lock mutx
			val result = f()
				handle exn => (unlock mutx; setAttributes oldAttrs; raise exn)
		in
			unlock mutx;
			setAttributes oldAttrs;
			result
		end
        
      (* copy an environment, adding an extra level for new definitions *)
      fun makeSubshellDbEnv (DbEnv(mutx, dbl) : dbEnv) : dbEnv =
      let
        val utab = makeUnivTable ();
      in
        DbEnv (mutx, utab :: dbl)
      end;
        
      (* enter a value into an environment *)
      fun dbEnvEnter (DbEnv(mutx, db), t : 'a tag, s : string, v : 'a) : unit =
	  	protect mutx
			(fn () =>
		         case db of
		            [] => raise InternalError "Environment initialisation error"
		          | (thisLevel :: _) => univEnter (thisLevel, t, s, v)
			)

      (* find a value in an environment *)
		fun dbEnvLookup (DbEnv(mutx, db), t : 'a tag, s : string) : 'a =
		let
			fun lookupList []                         = raise ValueMissing s
			| lookupList (thisLevel :: otherLevels) =
				univLookup (thisLevel, t, s)
					handle ValueMissing _ => lookupList otherLevels;
      	in
			protect mutx(fn () => lookupList db)
      	end;
  
      (* delete an entry, but only from the top-level table *)
      (* This may raise ValueMissing *)
      fun dbEnvForget (DbEnv(mutx, db), s : string, t : 'a tag) : unit =
	      protect mutx
		  	(fn () =>
		        case db of
		             [] => raise InternalError "Environment initialisation error"
		          | (thisLevel :: _) => univDelete (thisLevel, t, s)
			)

	  (* This previously returned only the names in the current database.
	     Now expanded to include parents so that it really shows everything.
		 We should remove duplicates since there may be entries in parent
		 databases with the same names as items in the children. *)
      fun dbEnvContents (DbEnv(mutx, db), t : 'a tag) : string list =
	  let
	  	 fun eachLevel(u: univTable, s: string list): string list =
		 	s @ mapIterator (fn (n : string, v : 'a) => n) (univOverSpecific (u, t))
	  in
	  	 protect mutx (fn () => List.foldl eachLevel [] db)
	  end

	  (* In order to print out an exception packet we have to be able to find the
	     type of the exception.  It may be in the top-level environment or it may
		 be in a structure.  We need to search through the name space in order to
		 find it. *)
	  fun dbEnvFindException (DbEnv(mutx, db)) (w: COMPILERBODY.word) : values option =
	  let
	  	  open UNIVERSAL
		  open STRUCTVALS

		  fun searchList f [] = NONE
		    | searchList f (hd::tl) =
				case f hd of
					NONE => searchList f tl
				|	result => result
	  in
	  	protect mutx (fn () => 
	  	  searchList (fn u => VALUE_OPS.findException u w) db)
	  end
    end;
  end;



(*****************************************************************************)
(*                  Utility functions                                        *)
(*****************************************************************************)
  (* There are three possible states - The object may have been checked,
     it may be currently being compiled, or it may not have been
     processed yet. *)
  datatype compileState =
    NotProcessed
  | Searching
  | Checked
  ;

  (* isDir raises an exception if the file does not exist so this is
     an easy way to test for the file. *)
  fun fileReadable (name : string) : bool =
    (OS.FileSys.isDir name; true) handle _ => false

  datatype 'a option = None | Some of 'a;

  datatype RCSstate = NormalFile | RCSFile | RCSDirectory;
  
  (* This is almost the same as OS.Path.joinDirFile except that that function uses
     the separator compiled in when the structure was compiled which means that the
	 saved heaps are not portable from Windows to Unix. *)
  fun joinDirFile{dir, file} =
     let
		local
			val doCall: int*int -> string
				 = RunCall.run_call2 RuntimeCalls.POLY_SYS_process_env
		in
			val separator = doCall (7, 0)
		end
		local
			val doCall: int*Char.char -> bool
				 = RunCall.run_call2 RuntimeCalls.POLY_SYS_process_env
		in
			fun isSeparator ch = doCall (8, ch)
		end
	 in
    	if dir = "" then file (* Return the file name unchanged *)
    	else if isSeparator(String.sub(dir, size dir - 1))
    	then dir ^ file
    	else dir ^ separator ^ file
	 end


  fun longName (directory, file, NormalFile) =
  		joinDirFile{dir=directory, file = file}
        
    | longName (directory, file, RCSFile) =
  		joinDirFile{dir=directory, file = file ^ ",v"}

    | longName (directory, file, RCSDirectory) =
		joinDirFile{dir=joinDirFile{dir=directory, file = "DIR"},
						    file = file ^ ",v"}

  fun findFileTuple (directory, object, rcsMode) [] = None
    | findFileTuple (directory, object, rcsMode) (suffix :: suffixes) =
    let
      val fileName  = object ^ suffix
      val fileTuple = (directory, fileName, rcsMode)
    in
      if fileReadable (longName fileTuple)
      then Some fileTuple
      else findFileTuple (directory, object, rcsMode) suffixes
    end;

  fun filePresent (directory : string, object : string) =
  let
    val suffList = !suffixes
  in
    case findFileTuple (directory, object, NormalFile) suffList of
      Some res => Some res
    | None =>
       if not (!useRCS) then None
       else
         case findFileTuple (directory, object, RCSFile) suffList of
           Some res => Some res
         | None =>
             findFileTuple (directory, object, RCSDirectory) suffList
   end;

  (* See if the corresponding file is there and if it is a directory. *)
  fun testForDirectory (name: string) : bool =
  	OS.FileSys.isDir name handle _ => false (* No such file. *)

  fun executeCommand (command: string) : unit =
   	(OS.Process.system command; ())
   
   
(*****************************************************************************)
(*                  Timestamps                                               *)
(*****************************************************************************)
    type timeStamp = Time.time;
     
    val firstTimeStamp : timeStamp = Time.zeroTime;

    (* Get the current time. *)
    val newTimeStamp : unit -> timeStamp = Time.now
       
    (* Get the date of a file. *)
    val fileTimeStamp : string -> timeStamp = OS.FileSys.modTime

    (* String representation - includes trailing "\n"! *)
    fun stringOfTimeStamp (t : timeStamp) : string =
		Date.toString(Date.fromTimeLocal t) ^ "\n"


(*****************************************************************************)
(*                  New tags                                                 *)
(*****************************************************************************)
  local
    open UNIVERSAL
  in
    val timeStampTagMethods    : timeStamp tag   = tag ();
    val dependenciesTagMethods : string list tag = tag ();
  end;

(*****************************************************************************)
(*                  makeIntoEnv                                              *)
(*****************************************************************************)
fun makeIntoEnv (globalEnv : dbEnv) : string -> unit =
let
  local
    open UNIVERSAL
    open UNIVERSALTABLE
  in
    fun lookupGlobalEnv (t: 'a tag) (name: string) : 'a =
      dbEnvLookup (globalEnv, t, name);
       
    fun enterGlobalEnv (t: 'a tag, name: string, v: 'a) : unit =
      dbEnvEnter (globalEnv, t, name, v);
  end; (* local for global environment *)

  fun lastMade (objectName : string) : timeStamp =
    lookupGlobalEnv timeStampTagMethods objectName
      handle ValueMissing _ => (* never created *) 
        firstTimeStamp; (* so say it's out of date *)


(*****************************************************************************)
(*                  The main make function                                   *)
(*****************************************************************************)
  fun make (targetName: string) : unit =
  let
   (* This serves two purposes. It provides a list of objects which have been
      re-made to prevent them being made more than once, and it also prevents
      circular dependencies from causing infinite loops (e.g. let x = f(x)) *)
    local
      open HASHTABLE;
      val htab : compileState hash = hashMake 10;
    in
      fun lookupStatus (name: string) : compileState =
        hashSub (htab, name)
          handle ValueMissing _ => NotProcessed;
          
      fun setStatus (name: string, cs: compileState) : unit =
        hashSet (htab, name, cs)
    end;

    (* Remove leading directory names to get the name of the object itself.
       e.g. "../compiler/parsetree/gencode" yields simply "gencode". *)
    val (dirName,objectName) = splitFilename targetName;

(*****************************************************************************)
(*                  remakeObj (nested in make)                               *)
(*****************************************************************************)
 
    (* Looks to see if the file is in the current directory. If it is and
       the file is newer than the corresponding object then it must be
       remade. If it is a directory then we attempt to remake the
       directory by compiling the "bind" file. This will only actually be
       executed if it involves some identifier which is newer than the
       result object. *)
    fun remakeObj (objName: string) (findDirectory: string -> string) =
    let
      (* Find a directory that contains this object. An exception will be
         raised if it is not there. *)
      val directory = findDirectory objName;
      val fullName  =
	  		if directory = "" (* Work around for bug. *)
			then objName
			else joinDirFile{dir=directory, file=objName};

      val objIsDir  = testForDirectory fullName;
      val here      = fullName;
      
      (* Look to see if the file exists, possibly with an extension,
         and get the extended version. *)
      val fileTuple =
        let
          (* If the object is a directory the source is in the bind file. *)
          val (dir : string, file : string) =
            if objIsDir
            then (here, !bindName)
            else (directory, objName);
        in
          case filePresent (dir, file) of
            Some res' => res'
          | None      => 
              raise Failure ("No such file or directory ("^file^","^dir^")")
        end ;
 
      val fileName = longName fileTuple;

      val newFindDirectory : string -> string =
        if objIsDir
        then let
          (* Look in this directory then in the ones above. *)
          fun findDirectoryHere (name: string) : string =
            case filePresent (here, name) of
              None => findDirectory name (* not in this directory *)
            | _    => here;
        in
          findDirectoryHere
        end
        else findDirectory;
    
(*****************************************************************************)
(*                  remakeCurrentObj (nested in remakeObj)                   *)
(*****************************************************************************)
      (* Compiles a file. *)
      fun remakeCurrentObj () =
      let
        val UUU:unit = (!printString) ("Making " ^ objName ^ "\n");
  
(*****************************************************************************)
(*                  The local make environment (nested in remakeCurrentObj)  *)
(*****************************************************************************)
        local
          open HASHTABLE;

          (* Keep a list of the dependencies. *)
          val deps : bool hash = hashMake 10;
       
          fun addDep name =
            if (hashSub (deps, name) handle ValueMissing _ => true)
            then hashSet (deps, name, true)
            else ();
         
          (* Called by the compiler to look-up a global identifier. *)
          fun lookupMakeEnv (t: 'a tag) (name: string) : 'a =
            (
              let
                (* Have we re-declared it ? *)
                val res = lookupStatus name;
              in
                case res of
                  NotProcessed  =>
                   (
                     (* Compile the dependency. *)
                     remakeObj name newFindDirectory;
                     
                     (* Add this to the dependencies. *)
                     addDep name
                   )
                   
               |  Searching => (* In the process of making it *)
                    (!printString)
                     ("Circular dependency: " ^ name ^  " depends on itself\n")
                     
                | Checked =>
                    (* Add this to the dependencies. *)
                    addDep name
             end;
               
             (* If the dependency has an error we say the identifier
                 could not be found so as to generate faults all the
                 way back. *)
              lookupGlobalEnv t name
                handle Failure _ =>
                  raise ValueMissing name
            ); (* lookupMakeEnv *)
      
         (* Enter the declared value in the table. Usually this will be the
            target we are making. Also set the state to "Checked". The
            state is set to checked when we finish making the object but
            setting it now suppresses messages about circular dependencies
            if we use the identifier within the file. *)
          fun enterMakeEnv (kind : string, t: 'a tag) (name: string, v: 'a) : unit =
            (
              (* Put in the value. *)
              enterGlobalEnv (t, name, v);
              (!printString) ("Created " ^ kind ^ " " ^ name ^ "\n");
              
              (* File does not depend on this. *)
              hashSet (deps, name, false);
      
              if name = objName
              then let
                (* Put in the dependencies. *)
                val depends =
                  revfoldIterator
                    (fn (n,v) => fn l => if v then n :: l else l)
                    []
                    (hashOver deps);
                       
                (* Put in a time stamp for the new object.  We need to make
                   sure that it is no older than the newest object it depends
                   on.  In theory that should not be a problem but clocks on
                   different machines can get out of step leading to objects
                   made later having earlier time stamps. *)
                val newest =
                  revfold
                    (fn (s: string) => fn (t: timeStamp) =>
                         max (lastMade s) t)
                    (fileTimeStamp fileName)
                    depends;
      
                val timeStamp = max newest (newTimeStamp());
              in         
                setStatus (name, Checked);
                enterGlobalEnv (dependenciesTagMethods, name, depends);
                enterGlobalEnv (timeStampTagMethods, name, timeStamp)
              end
              else ()
            ) (* enter *);
            
 
          fun enterGlobalEnv (t: 'a tag) (name: string, v: 'a) : unit =
            dbEnvEnter (globalEnv, t, name, v);

          fun lookupTvars (name: string) = 
            raise MISC.ValueMissing ("free type variable: " ^ name);
         
          val lookupFix    = lookupGlobalEnv STRUCTVALS.fixVar;
          val lookupVal    = lookupGlobalEnv STRUCTVALS.valueVar;
          val lookupType   = lookupGlobalEnv STRUCTVALS.typeConstrVar;

          val lookupSig    = lookupMakeEnv STRUCTVALS.signatureVar;
          val lookupStruct = lookupMakeEnv STRUCTVALS.structVar;
          val lookupFunct  = lookupMakeEnv STRUCTVALS.functorVar;

          val enterFix     = enterGlobalEnv STRUCTVALS.fixVar;
          val enterVal     = enterGlobalEnv STRUCTVALS.valueVar;
          val enterType    = enterGlobalEnv STRUCTVALS.typeConstrVar;
          
          val enterSig     = enterMakeEnv ("signature", STRUCTVALS.signatureVar);
          val enterStruct  = enterMakeEnv ("structure", STRUCTVALS.structVar);
          val enterFunct   = enterMakeEnv ("functor",   STRUCTVALS.functorVar);

        in
          val makeEnv : STRUCTVALS.env =
            STRUCTVALS.Env
              { 
                lookupTvars  = lookupTvars,
                lookupFix    = lookupFix,
                lookupVal    = lookupVal,
                lookupType   = lookupType,
                lookupSig    = lookupSig,
                lookupStruct = lookupStruct,
                lookupFunct  = lookupFunct,
                enterFix     = enterFix,
                enterVal     = enterVal,
                enterType    = enterType,
                enterStruct  = enterStruct,
                enterSig     = enterSig,
                enterFunct   = enterFunct
               };
        end; (* local for makeEnv *)

(*****************************************************************************)
(*                  RCS check-out if necessary                               *)
(*****************************************************************************)

        val checkedOutFile =
          case fileTuple of
            (directory,file,_) =>
				(* Temporary work-around for bug. *)
				if directory = ""
				then file
				else joinDirFile{dir=directory, file=file}

        (* Check out the file if necessary. *)
        fun beginRCS () =
          if checkedOutFile <> fileName
          then executeCommand (concat ["co ", fileName, " ", checkedOutFile])
          else ();

        fun endRCS () =
          if checkedOutFile <> fileName
          then 
            (
              (!printString) ("Deleting " ^ checkedOutFile ^ "\n");
              executeCommand ("rm -f " ^ checkedOutFile)
            )
          else ();

(*****************************************************************************)
(*                  File I/O routines (nested in remake)                     *)
(*****************************************************************************)

        val U : unit = beginRCS ();
        val U : unit = 
	  let (* scope of exception handler to delete checked-out file *)
	    val inStream = TextIO.openIn checkedOutFile;
	    val U : unit =
	      let (* scope of exception handler to close inStream *)
		val endOfStream = ref false;
		val lineNo     = ref 1;
		val charBuffer = ref ""; (* Current buffer *)
		val ptrInBuf  = ref 0;    (* next character position in buffer *)
		
		fun getChar () : char =
		let
		  fun inc r = r := !r + 1;
		  val chars = !charBuffer;
		  val pos   = !ptrInBuf;
		in
		  if pos < size chars
		  then
		  	let
				val nextch = String.sub(chars, pos)
			in
				if nextch = #"\n"
				then inc lineNo else ();
				inc ptrInBuf;
				nextch
		  	end
		  else let (* text used up - get another chunk *)
		    val chars = TextIO.input inStream
		  in
		    if chars = ""
			then (endOfStream := true; eofChar) (* End of file *)
		    else
				(
				charBuffer := chars;
				ptrInBuf  := 0;
				getChar() (* Get the character from the buffer. *)
				)
		  end
		end (* getChar *);
	   
		(* Writes out any error messages *)
		fun writeout (message: string) =
		let
		  val U : unit =
		    if message = "Error-"
		    then
		      (
			(!printString) ("\n\"" ^ fileName ^ "\", line ");
			(!printString) (Int.toString (!lineNo));
			(!printString) "\n"
		      )
		    else ();
		in    
		  (!printString) message
		end; (* writeout *)
     
                (* The lexer for this file *)
        val lex : LEX.lexan =
          LEX.initial (getChar, fileName, 
                       (fn () => !lineNo),
					   (fn s => ! printString s));
                               
(*****************************************************************************)
(*                  body of remake                                           *)
(*****************************************************************************)
	      in
		while not (!endOfStream) do
		let
		  val code = COMPILERBODY.compiler (lex, makeEnv, dbEnvFindException globalEnv);
		in
		  code ()
		end
	      end (* body of scope of inStream *)
		handle exn => (* close inStream if an error occurs *)
		(
		  TextIO.closeIn inStream;
		  raise exn
		);
          in (* normal termination *)
            TextIO.closeIn inStream
          end (* scope of inStream *)
	    handle exn => (* delete checked-out file if an error occurs *)
	    (
	      endRCS ();
	      raise exn
	    );
      in (* remake normal termination *)
        endRCS ()
      end; (* remake *)

(*****************************************************************************)
(*                  body of remakeObj                                        *)
(*****************************************************************************)
    in
  
      setStatus (objName, Searching);
 
     (* If the file is newer than the object then we definitely must remake it.
       Otherwise we look at the dependency list and check those. If the result
       of that check is that one of the dependencies is newer than the object
       (probably because it has just been recompiled) we have to recompile
       the file. Compiling a file also checks the dependencies and recompiles
       them, generating a new dependency list. That is why we don't check the
       dependency list if the object is out of date with the file. Also if the
       file has been changed it may no longer depend on the things it used to
       depend on. *)
 
     let
       val objDate = lastMade objName
       
       fun maybeRemake (s:string) : unit =
         case lookupStatus s of
           NotProcessed => (* see if it's a file. *)
             (* Compile the dependency. *)
             remakeObj s newFindDirectory
              
         | Searching => (* In the process of making it *)
             (!printString) ("Circular dependency: " ^ s ^ " depends on itself\n")
             
         |  Checked => (* do nothing *)
             ();
         
       (* Process each entry and return true if
          any is newer than the target. *)
       val processChildren =
         revfold
           (fn (child:string) => fn (parentNeedsMake:bool) =>
              (
                maybeRemake child;
          
               (* Find its date and see if it is newer. *)
               parentNeedsMake orelse lastMade child > objDate
              )
           )
         false;
     in
       if objDate < fileTimeStamp fileName orelse
        (
          (* Get the dependency list. There may not be one if
             this object has not been compiled with "make". *) 
          processChildren (lookupGlobalEnv dependenciesTagMethods objName)
               (* No dependency list - must use "make" on it. *)
                handle ValueMissing _ => true
        )       
       then remakeCurrentObj ()
       else ()
     end;
     
     
     (* Mark it as having been checked. *)
     setStatus (objName, Checked)
   end (* body of remakeObj *)
    (* If the object is not a file mark it is checked. It may be a
       pervasive or it may be missing. In either case mark it as checked
       to save searching for it again. *)
     handle
       ObjNotFile =>
          setStatus (objName, Checked)
          
     |  exn => (* Compilation (or execution) error. *)
        (
          (* Mark as checked to prevent spurious messages. *)
          setStatus (objName, Checked);
          raise exn
        );
        
(*****************************************************************************)
(*                  body of make                                             *)
(*****************************************************************************)
  in
    (* Check that the target exists. *)
    case filePresent (dirName, objectName) of
      None =>
        let
		  val dir =
		  	if dirName = "" then ""
		  	else " (directory "^dirName^")";
          val s = "File "^objectName^" not found" ^ dir
        in
          (!printString) (s ^ "\n");
          raise Failure s
        end
           
    | _ =>
      let
        val targetIsDir = testForDirectory targetName;
      
        (* If the target we are making is a directory all the objects
           must be in the directory. If it is a file we allow references
           to other objects in the same directory. Objects not found must
           be pervasive. *)
        fun findDirectory (s: string) : string =
          if (not targetIsDir orelse s = objectName) andalso
             filePresent(dirName,  s) <> None
          then dirName
          else raise ObjNotFile;
      in
        remakeObj objectName findDirectory
          handle exn  => 
           (
             (!printString) (targetName ^ " was not declared\n");
             raise exn
            )
      end
  end (* make *)
in
  (* Convert compiler exceptions into the standard Fail exception. *)
  fn name => (make name handle Failure s => raise Fail s)
end (* makeIntoEnv *);
  
(*****************************************************************************)
(*                  useIntoEnv (runcompiler with ML compiler bound in)       *)
(*****************************************************************************)
fun useIntoEnv (globalEnv : dbEnv) : string -> unit =
let

(*****************************************************************************)
(*                  useEnv                                                   *)
(*****************************************************************************)
  local
    open UNIVERSAL
    open UNIVERSALTABLE

    fun lookupGlobalEnv (t: 'a tag) (name: string) : 'a =
      dbEnvLookup (globalEnv, t, name);
       
    fun enterGlobalEnv (t: 'a tag) (name: string, v: 'a) : unit =
      dbEnvEnter (globalEnv, t, name, v);

    fun lookupTvars (name: string) = 
      raise MISC.ValueMissing ("free type variable: " ^ name);
       
    val lookupFix    = lookupGlobalEnv STRUCTVALS.fixVar;
    val lookupVal    = lookupGlobalEnv STRUCTVALS.valueVar;
    val lookupType   = lookupGlobalEnv STRUCTVALS.typeConstrVar;

    val lookupSig    = lookupGlobalEnv STRUCTVALS.signatureVar;
    val lookupStruct = lookupGlobalEnv STRUCTVALS.structVar;
    val lookupFunct  = lookupGlobalEnv STRUCTVALS.functorVar;

    val enterFix     = enterGlobalEnv STRUCTVALS.fixVar;
    val enterVal     = enterGlobalEnv STRUCTVALS.valueVar;
    val enterType    = enterGlobalEnv STRUCTVALS.typeConstrVar;
    
    val enterSig     = enterGlobalEnv STRUCTVALS.signatureVar;
    val enterStruct  = enterGlobalEnv STRUCTVALS.structVar;
    val enterFunct   = enterGlobalEnv STRUCTVALS.functorVar;

  in
    val useEnv : STRUCTVALS.env =
      STRUCTVALS.Env
        { 
          lookupTvars  = lookupTvars,
          lookupFix    = lookupFix,
          lookupVal    = lookupVal,
          lookupType   = lookupType,
          lookupSig    = lookupSig,
          lookupStruct = lookupStruct,
          lookupFunct  = lookupFunct,
          enterFix     = enterFix,
          enterVal     = enterVal,
          enterType    = enterType,
          enterStruct  = enterStruct,
          enterSig     = enterSig,
          enterFunct   = enterFunct
         };
  end; (* local for useEnv *)

(*****************************************************************************)
(*                  use                                                      *)
(*****************************************************************************)
  fun use (targetName : string) =
  let
    val (dirName,objectName) = splitFilename targetName;
  in
    case findFileTuple (dirName, objectName, NormalFile) (!suffixes) of
      None => raise Fail("File "^targetName^" not found")
      
    | Some fileTuple => 
    let
      val fileName = longName fileTuple;
      val inStream = TextIO.openIn fileName;
  
      val lineNo   = ref 1;
      val prompt   = ref (!prompt1);

      local
        val interactive = fileName = "";
      in  
        fun getChar () : char =
			case TextIO.input1 inStream of
				NONE => (* end of file *) eofChar
			|	SOME #"\n" =>
	              (
	               if interactive
	               then (!printString) (!prompt)
	               else lineNo := !lineNo + 1;
	               
	               prompt := !prompt2;
	               
	               #"\n"
	              )
           |	SOME c => c
      end; (* local for getChar *)
      
      (* The lexer to use *)
      val lex : LEX.lexan = 
        LEX.initial (getChar, fileName,
                     (fn () => !lineNo),
					 (fn s => ! printString s));        
    in
     (while not (TextIO.endOfStream inStream) do
      let
        (* initialise code *)
        val U : unit = (prompt := !prompt1); 
          
        (* Compile  the code *)
        val code : unit -> unit = COMPILERBODY.compiler (lex, useEnv, dbEnvFindException globalEnv);
      in
        (* execute the code *)
        code ()
      end)
        handle Failure s => (* E.g. syntax error. *)
          (
            TextIO.closeIn inStream;
            raise Fail s
          )
 		| exn => (* close inStream if an error occurs *)
		(
		  TextIO.closeIn inStream;
		  raise exn
		);
     
      TextIO.closeIn inStream
    end (* file present *)
  end (* use *)
in
  use
end; (* scope of useIntoEnv *)

(*****************************************************************************)
(*                  dependenciesInEnv (list dependency information)          *)
(*****************************************************************************)
fun dependenciesInEnv (globalEnv : dbEnv) (name : string) : string list =
let
  open MISC;
  
  val dependencies : string list =
    dbEnvLookup (globalEnv, dependenciesTagMethods, name)
      handle ValueMissing _ => [];
      
  fun lt (s1: string) (s2: string) : bool =
     s1 < s2;
in
  quickSort lt dependencies
end;

(*****************************************************************************)
(*                  dependsInEnv (print dependency information)              *)
(*****************************************************************************)
fun dependsInEnv (globalEnv : dbEnv) : string -> unit =
let
  open MISC;
  
  fun print (s : string) : unit = (!printString) s;
  
  fun printDate (name : string) : unit =
  let
    val date : timeStamp = dbEnvLookup (globalEnv, timeStampTagMethods, name);
  in
    print (stringOfTimeStamp date)
  end
    handle ValueMissing _ => print "(No date)\n";
  
  fun printDependencies (dependencies : string list) : unit =
  let
  	fun max i j : int = if i > j then i else j

    val maxSize : int = 
      revfold max 0 (map size dependencies);
      
    (* width of one column *)
    val colWidth : int = 
      maxSize + 1;
      
    (* Width of screen *)
    val screenWidth : int = 72; 
    
    (* Work out number of columns so that there is room for
       the longest word and at least one space between. *)
    val cols : int =
      if screenWidth < maxSize (* It *could* happen *)
      then 1
      else (screenWidth + 1) div colWidth;
    
    fun pad 0 = () | pad n = (print " "; pad (n - 1));
    
    fun printNames n [] = 
	  if (n - 1) mod cols <> 0 then print "\n" else ()
    
      | printNames n (name :: names) =
      (
	print name;
	
	if n mod cols = 0
	then print "\n"
	else pad (colWidth - size name);
	
	printNames (n + 1) names
      )
  in
    printNames 1 dependencies
  end  
  
  fun depends (name : string) : unit =
  let
    val sortedDependencies : string list =
      dependenciesInEnv globalEnv name;
  in
    print name;
    print " compiled at ";
    printDate name;
    printDependencies sortedDependencies
  end;
in
  depends
end; (* dependsInEnv *)

(*****************************************************************************)
(*                  runCompiler (runcomp with ML compiler bound in)          *)
(*****************************************************************************)
fun runCompiler (globalEnv : dbEnv) : 
  (unit -> string) * (string -> unit) * (unit -> int) * string -> unit -> unit =
let

(*****************************************************************************)
(*                  useEnv                                                   *)
(*****************************************************************************)
  local
    open UNIVERSAL
    open UNIVERSALTABLE

    (* Don't eta-reduce theses definitions 'cos they've got to
       work properly if the current environment is changed within
       a bind file. *)
    fun lookupGlobalEnv (t: 'a tag) (name: string) : 'a =
      dbEnvLookup (globalEnv, t, name);
       
    fun enterGlobalEnv (t: 'a tag) (name: string, v: 'a) : unit =
      dbEnvEnter (globalEnv, t, name, v);

    fun lookupTvars (name: string) = 
      raise MISC.ValueMissing ("free type variable: " ^ name);
       
    val lookupFix    = lookupGlobalEnv STRUCTVALS.fixVar;
    val lookupVal    = lookupGlobalEnv STRUCTVALS.valueVar;
    val lookupType   = lookupGlobalEnv STRUCTVALS.typeConstrVar;

    val lookupSig    = lookupGlobalEnv STRUCTVALS.signatureVar;
    val lookupStruct = lookupGlobalEnv STRUCTVALS.structVar;
    val lookupFunct  = lookupGlobalEnv STRUCTVALS.functorVar;

    val enterFix     = enterGlobalEnv STRUCTVALS.fixVar;
    val enterVal     = enterGlobalEnv STRUCTVALS.valueVar;
    val enterType    = enterGlobalEnv STRUCTVALS.typeConstrVar;
    
    val enterSig     = enterGlobalEnv STRUCTVALS.signatureVar;
    val enterStruct  = enterGlobalEnv STRUCTVALS.structVar;
    val enterFunct   = enterGlobalEnv STRUCTVALS.functorVar;

  in
    val useEnv : STRUCTVALS.env =
      STRUCTVALS.Env
        { 
          lookupTvars  = lookupTvars,
          lookupFix    = lookupFix,
          lookupVal    = lookupVal,
          lookupType   = lookupType,
          lookupSig    = lookupSig,
          lookupStruct = lookupStruct,
          lookupFunct  = lookupFunct,
          enterFix     = enterFix,
          enterVal     = enterVal,
          enterType    = enterType,
          enterStruct  = enterStruct,
          enterSig     = enterSig,
          enterFunct   = enterFunct
         };
  end; (* local for useEnv *)

(*****************************************************************************)
(*                  use                                                      *)
(*****************************************************************************)
  fun runComp (get : unit -> string, put : string -> unit,
               lineNo: unit -> int, fileName: string) : unit -> unit =
  let
    val lastString = ref ""; (* Current command text *)
    val textPtr    = ref 0;  (* Position in this text *)
  
    fun readin () =
    (
      if !textPtr >= size (!lastString)
      then (* Read some more from the input text. *)
        (
          lastString := get ();
          textPtr    := 0
        )
      else ();
      
      if !lastString = ""  then eofChar
      else
        (
          textPtr := !textPtr + 1;
		  String.sub(!lastString, !textPtr-1)
         )
    ); (* readin *)
    
    (* The lexer for this compilation *)
    val lex : LEX.lexan = LEX.initial (readin, fileName, lineNo, put);
    val code : unit -> unit = COMPILERBODY.compiler (lex, useEnv, dbEnvFindException globalEnv);
  in
    code
  end
in 
  runComp
end; (* scope of runCompiler *)


(*****************************************************************************)
(*                  Forget functions                                         *)
(*****************************************************************************)
  local
   (* Needed to remove make system information for defunct
      structure-level declarations *)
    fun forgetMakeDate (makeEnv : dbEnv, name : string) : unit =
      dbEnvForget (makeEnv, name, timeStampTagMethods)
	handle ValueMissing _ => ();
	
    fun forgetMakeDependencies (makeEnv : dbEnv, name : string) : unit =
      dbEnvForget (makeEnv, name, dependenciesTagMethods)
	handle ValueMissing _ => ();
	
    open STRUCTVALS; (* for the various tag *)
  in
    fun forgetMakeInfo (makeEnv : dbEnv) (name : string) : unit =
      (
	forgetMakeDate (makeEnv, name);
	forgetMakeDependencies (makeEnv, name)
      );
      
    fun forgetSignature (makeEnv : dbEnv) (name : string) : unit =
      (
	dbEnvForget (makeEnv, name, signatureVar);
	forgetMakeInfo makeEnv name
      );
    
    fun forgetStructure (makeEnv : dbEnv) (name : string) : unit =
      (
	dbEnvForget (makeEnv, name, structVar);
	forgetMakeInfo makeEnv name
      );
      
    fun forgetFunctor (makeEnv : dbEnv) (name : string) : unit =
      (
	dbEnvForget (makeEnv, name, functorVar);
	forgetMakeInfo makeEnv name
      );
    
    fun forgetValue (makeEnv : dbEnv) (name : string) : unit =
	   dbEnvForget (makeEnv, name, valueVar);
    
    fun forgetType (makeEnv : dbEnv) (name : string) : unit =
	  dbEnvForget (makeEnv, name, typeConstrVar);
    
    fun forgetFixity (makeEnv : dbEnv) (name : string) : unit =
	  dbEnvForget (makeEnv, name, fixVar);
	
	
    fun signatureNames (makeEnv : dbEnv) : unit -> string list =
      fn () => dbEnvContents (makeEnv, signatureVar);
	
    fun structureNames (makeEnv : dbEnv) : unit -> string list =
      fn () => dbEnvContents (makeEnv, structVar);
	
    fun functorNames (makeEnv : dbEnv) : unit -> string list =
      fn () => dbEnvContents (makeEnv, functorVar);
	
    fun valueNames (makeEnv : dbEnv) : unit -> string list =
      fn () => dbEnvContents (makeEnv, valueVar);
	
    fun typeNames (makeEnv : dbEnv) : unit -> string list =
      fn () => dbEnvContents (makeEnv, typeConstrVar);
	
    fun fixityNames (makeEnv : dbEnv) : unit -> string list =
      fn () => dbEnvContents (makeEnv, fixVar);

  end

(*****************************************************************************)
(*                  shellProc                                                *)
(*****************************************************************************)
fun shellProc (globalEnv : dbEnv,
               firstPrompt : string ref,
               secondPrompt: string ref) :
  {
    shell:      unit -> unit,    (* The command processor *)
	debugger:	(string->values) * (unit->bool) -> unit} =
let

(*****************************************************************************)
(*                  shellEnv                                                 *)
(*****************************************************************************)
  local
    open UNIVERSAL
    open UNIVERSALTABLE

    (* Don't eta-reduce theses definitions 'cos they've got to
       work properly if the current environment is changed within
       a bind file. *)
    fun lookupGlobalEnv (t: 'a tag) (name: string) : 'a =
      dbEnvLookup (globalEnv, t, name);
       
    fun enterGlobalEnv (t: 'a tag) (name: string, v: 'a) : unit =
      dbEnvEnter (globalEnv, t, name, v);

    fun lookupTvars (name: string) = 
      raise MISC.ValueMissing ("free type variable: " ^ name);
       
    val lookupFix    = lookupGlobalEnv STRUCTVALS.fixVar;
    val lookupVal    = lookupGlobalEnv STRUCTVALS.valueVar;
    val lookupType   = lookupGlobalEnv STRUCTVALS.typeConstrVar;

    val lookupSig    = lookupGlobalEnv STRUCTVALS.signatureVar;
    val lookupStruct = lookupGlobalEnv STRUCTVALS.structVar;
    val lookupFunct  = lookupGlobalEnv STRUCTVALS.functorVar;

    val enterFix     = enterGlobalEnv STRUCTVALS.fixVar;
    val enterVal     = enterGlobalEnv STRUCTVALS.valueVar;
    val enterType    = enterGlobalEnv STRUCTVALS.typeConstrVar;
    
    val enterSig     = enterGlobalEnv STRUCTVALS.signatureVar;
    val enterStruct  = enterGlobalEnv STRUCTVALS.structVar;
    val enterFunct   = enterGlobalEnv STRUCTVALS.functorVar;

  in
    fun shellEnv (debugEnv: string-> values) : STRUCTVALS.env =
      STRUCTVALS.Env
        { 
          lookupTvars  = lookupTvars,
          lookupFix    = lookupFix,
          lookupVal    = fn s => debugEnv s handle ValueMissing _ => lookupVal s,
          lookupType   = lookupType,
          lookupSig    = lookupSig,
          lookupStruct = lookupStruct,
          lookupFunct  = lookupFunct,
          enterFix     = enterFix,
          enterVal     = enterVal,
          enterType    = enterType,
          enterStruct  = enterStruct,
          enterSig     = enterSig,
          enterFunct   = enterFunct
         };
  end; (* local for useEnv *)


(*****************************************************************************)
(*                  shell                                                    *)
(*****************************************************************************)
  fun topLevel isDebug (debugEnv, exitLoop) : unit =
  let
    (* Don't use the end_of_stream because it may have been set by typing
       EOT to the command we were running. *)
    val endOfFile    = ref false;
    val realDataRead = ref false;
    val lastWasEol   = ref true;
	val lineNo     = ref 1;
    
    val thisCommand = ref ""; (* Current command text  *)
    val textPtr     = ref 0;  (* Position in this text *)
    
    (* Each character typed is fed into the compiler but leading
       blank lines result in the prompt remaining as firstPrompt until
       significant characters are typed. *)
    fun readin () : char =
    let
      val setPrompt : unit =
        if !lastWasEol (* Start of line *)
        then 
          if !realDataRead
          then (lineNo := !lineNo +1; (!printString) (!secondPrompt))
          else (lineNo := 1; (!printString) (!firstPrompt))
        else ();

      val readChar : unit =
	if !textPtr >= size (!thisCommand)
	then let (* Read some more from the input text. *)
	  (* Get next char or up to end of line or end of text. This used to
	     read more and buffer it internally but this has an unfortunate
	     effect if the user types   PolyML.commit(); PolyML.quit(); on one line. *)
	  val nextin = ! inputChar ();
	in
	  case nextin of
	  	 NONE => endOfFile := true
	   | SOME ch =>
	    (
	      thisCommand := !thisCommand ^ str ch;
	       
	      (* If the next character is end-of-line add it to the buffer now.
		     This gives much better behaviour if we have an interrupt. *)
	      if ch <> #"\n" andalso (!nextIsNewline)()
	      then thisCommand := !thisCommand ^ str(valOf(! inputChar ()))
	      else ()
	    )
	end
	else ();
    in
       if !endOfFile then eofChar (* control-D = End of file *)
       else
	   	let
		 val U : unit = textPtr := !textPtr + 1;
		  
		 (* Get next character *)
		 val ch = String.sub(!thisCommand, !textPtr-1)
	   in
		 if ch = eofChar (* Simulate end of file *)
		 then endOfFile := true
		 else
		   (
		      (* Check for newlines *)
		      lastWasEol := ch = #"\n"; 
			  
		      if ch <> #"\n" andalso ch <> #" "
		      then realDataRead := true
		      else ()
		   );
		    
		 ch
       end
     end; (* readin *)
	 
     (* The lexer to use *)
     val lex : LEX.lexan =
	 	LEX.initial (readin, "", (fn () => !lineNo), fn s => ! printString s);
     
	 fun flushin() : unit =
	 (* Flush the input if there was a failure.
	    Larry Paulson asked for this and I think it's a good idea. *)
		( (! flushInput)(); LEX.flushLexan lex; lastWasEol := true )

     fun loop () : unit =
     let
       (* 
         Tidy-up the stack. This promises that no function that's
         currently active on the stack ever asked to extend the
         stack more than 8000 words beyond the current stack pointer.
         If this promise is wrong, we'll get very surprising core
         dumps. (That's why we're only prepared to make this promise
         here, and not in the code for PolyML.compiler, PolyML.make or
         PolyML.use) SPF 9/12/96
       *)
       val U : unit =
	   	 if isDebug then () else shrink_stack 8000;
     in
       if !endOfFile orelse exitLoop() then ()
       else
		(
		  realDataRead := false;
		  
		  (* Get the rest of the text. *)
		  if !textPtr < size (!thisCommand)
		  then
		    thisCommand :=
		      String.substring (!thisCommand, !textPtr, size (!thisCommand) - !textPtr)
		  else thisCommand := "";
		  
		  textPtr := 0;
		  
		  let
		    val code : unit -> unit =
			   COMPILERBODY.compiler (lex, shellEnv debugEnv, dbEnvFindException globalEnv);
		  in  
		    code ()
		      handle Failure s =>
			      (* Catch any exceptions it may have raised. *)
			    (
				   (!printString)(s ^ "\n");
				   flushin()
				)
		  end
		    handle Failure s =>
		      (
                 (!printString)(s ^ "\n");
                 flushin()
		      );
		  
		  loop ()
		)
      end; (* loop *)

	 (* If we are debugging we may pass exceptions back to the
	    debugged function. *) 
	 fun handleException () =
	 let
		val _ =
			! printString "Pass exception to function being debugged (y/n)?";
	 in
		! flushInput ();
		case ! inputChar () of
			NONE => false
		|   SOME #"y" => false
		|	SOME #"n" => true
		|	_ => handleException()
	 end

     fun handledLoop () : unit =
	 	(loop ())
			handle exn =>
				if not isDebug orelse handleException()
				then handledLoop ()
				else raise exn;
	 
   in
     handledLoop ()  
   end (* shell *)

   (* Normal, non-debugging shell. *)
   fun shell () =
   	  topLevel false (fn _ => raise ValueMissing "No debug", fn _ => false)
   	  
in
  {
    shell      = shell,
    debugger   = topLevel true
   }
end; (* scope of shellProc *)



end (* MAKE *)

end; (* structure-level let *)


