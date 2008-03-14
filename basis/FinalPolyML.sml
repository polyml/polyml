(*
    Title:      Final version of the PolyML structure
    Author:     David Matthews
    Copyright   David Matthews 2008

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
Based on:

    Title:      Poly Make Program.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)


(*
This is the version of the PolyML structure that can be compiled after we
have the rest of the basis library.  In particular it binds in TextIO.stdIn
and TextIO.stdOut.

This contains the top-level read-eval-print loop as well as "use" and
Poly/ML's "make".
*)

local

    (*****************************************************************************)
    (*                  top-level name space                                     *)
    (*****************************************************************************)
    val globalTable = UniversalArray.univArray 10 (* Choose a number for the initial size. *)
    and tableMutex = Thread.Mutex.mutex() (* Lock to protect the table. *)
 
    local
        open PolyML.NameSpace Universal UniversalArray Thread.Thread Thread.Mutex
        (* Create universal tags for the name space. *)
        (* Should these be kept private here or included in the PolyML
           structure? *)
        val valTag: valueVal tag = tag()
        and fixTag: fixityVal tag = tag()
        and functorTag: functorVal tag = tag()
        and signatureTag: signatureVal tag = tag()
        and structureTag: structureVal tag = tag()
        and typeTag: typeVal tag = tag()
        
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
        
        fun lookup t s = protect tableMutex (fn () => sub(globalTable, t, s));
        fun enter t (s,v) = protect tableMutex (fn () => update(globalTable, t, s, v));
        fun all t () = protect tableMutex (fn () => 
           fold (fn (s, v, l) => if tagIs t v then (s, tagProject t v)::l else l)
           [] globalTable)
        fun forget t tag s = protect tableMutex (fn () => delete(t, tag, s))
    in
        val globalNameSpace: PolyML.NameSpace.nameSpace =
            {
            lookupFix    = lookup fixTag,
            lookupSig    = lookup signatureTag,
            lookupVal    = lookup valTag,
            lookupType   = lookup typeTag,
            lookupFunct  = lookup functorTag,
            lookupStruct = lookup structureTag,
            enterFix     = enter fixTag,
            enterSig     = enter signatureTag,
            enterVal     = enter valTag,
            enterType    = enter typeTag,
            enterFunct   = enter functorTag,
            enterStruct  = enter structureTag,
            allFix       = all fixTag,
            allSig       = all signatureTag,
            allVal       = all valTag,
            allType      = all typeTag,
            allFunct     = all functorTag,
            allStruct    = all structureTag
            }

        val forgetFix    = forget globalTable fixTag
        and forgetSig    = forget globalTable signatureTag
        and forgetVal    = forget globalTable valTag
        and forgetType   = forget globalTable typeTag
        and forgetFunct  = forget globalTable functorTag
        and forgetStruct = forget globalTable structureTag
    end

    val bindName = ref "ml_bind";
    val archSuffix = "." ^ String.map Char.toLower (PolyML.architecture())
    (* The architecture-specific suffixes take precedence. *)
    val suffixes = ref [archSuffix, "",archSuffix^".ML", ".ML", archSuffix^".sml", ".sml"];

    (* This character is used to indicate end-of-file.  This is not legal in ML
       so if it occurs within the input the lexer will report an error and stop. *)
    val eofChar         = Char.chr 4; (* ctrl/D *)

    (* isDir raises an exception if the file does not exist so this is
       an easy way to test for the file. *)
    fun fileDirExists (name : string) : bool =
       (OS.FileSys.isDir name; true) handle OS.SysErr _ => false

    fun findFileTuple (directory, object) [] = NONE
    |   findFileTuple (directory, object) (suffix :: suffixes) =
    let
        val fileName  = object ^ suffix
    in
        if fileDirExists (OS.Path.joinDirFile{dir=directory, file = fileName})
        then SOME (directory, fileName)
        else findFileTuple (directory, object) suffixes
    end;

    (*****************************************************************************)
    (*                  "use": compile from a file.                              *)
    (*****************************************************************************)

    fun use originalName =
    let
        (* use "f" first tries to open "f" but if that fails it tries "f.ML", "f.sml" etc. *)
        fun trySuffixes [] =
            (* Not found - attempt to open the original and pass back the
               exception. *)
            (TextIO.openIn originalName, originalName)
         |  trySuffixes (s::l) =
            (TextIO.openIn (originalName ^ s), originalName ^ s)
                handle IO.Io _ => trySuffixes l
        (* First in list is the name with no suffix. *)
        val (inStream, fileName) = trySuffixes("" :: ! suffixes)
        val lineNo   = ref 1;
        fun getChar () : char =
            case TextIO.input1 inStream of
                NONE => (* end of file *) eofChar
            |   SOME #"\n" =>
                (
                    lineNo := !lineNo + 1;
                    #"\n"
                )
            |   SOME c => c
    in
        while not (TextIO.endOfStream inStream) do
        let
            val code = PolyML.compiler
                    { nameSpace = globalNameSpace, getChar = getChar, fileName = fileName,
                      lineNumber = fn () => !lineNo, putString = TextIO.print }
                handle exn => (TextIO.closeIn inStream; raise exn)
        in
            code() handle exn =>
            (
                (* Report exceptions in running code.
                   N.B. General.exnMessage uses the environment set by setPrintEnv *)
                TextIO.print ("Exception- " ^ General.exnMessage exn ^ " raised\n");
                TextIO.closeIn inStream;
                raise exn
            )
        end;
        (* Normal termination: close the stream. *)
        TextIO.closeIn inStream

    end (* use *)
 
    fun maxTime (x : Time.time, y : Time.time) : Time.time = 
        if x < y then y else x;

    exception ObjNotFile;
    
    type 'a tag = 'a Universal.tag;
  
    fun splitFilename (name: string) : string * string =
       let
         val {dir, file } = OS.Path.splitDirFile name
     in
         (dir, file)
     end

    (* Make *)
    (* There are three possible states - The object may have been checked,
     it may be currently being compiled, or it may not have been
     processed yet. *)
    datatype compileState = NotProcessed | Searching | Checked;

    fun longName (directory, file) = OS.Path.joinDirFile{dir=directory, file = file}
    
    fun fileReadable (fileTuple as (directory, object)) =
        (* Use OS.FileSys.isDir just to test if the file/directory exists. *)
        if (OS.FileSys.isDir (longName fileTuple); false) handle _ => true
        then false
        else
        let
            (* Check that the object is present in the directory with the name
             given and not a case-insensitive version of it.  This avoids
             problems with "make" attempting to recursively make Array etc
             because they contain signatures ARRAY. *)
            open OS.FileSys
            val d = openDir (if directory = "" then "." else directory)
            fun searchDir () =
              case readDir d of
                 NONE => false
              |  SOME f => f = object orelse searchDir ()
            val present = searchDir()
        in
            closeDir d;
            present
        end
    
    fun findFileTuple (directory, object) [] = NONE
    |   findFileTuple (directory, object) (suffix :: suffixes) =
    let
        val fileName  = object ^ suffix
        val fileTuple = (directory, fileName)
    in
        if fileReadable fileTuple
        then SOME fileTuple
        else findFileTuple (directory, object) suffixes
    end;
    
    fun filePresent (directory : string, object : string) =
        findFileTuple (directory, object) (!suffixes)
    
    (* See if the corresponding file is there and if it is a directory. *)
    fun testForDirectory (name: string) : bool =
        OS.FileSys.isDir name handle _ => false (* No such file. *)

    (* Time stamps. *)
    type timeStamp = Time.time;
    val firstTimeStamp : timeStamp = Time.zeroTime;
    (* Get the current time. *)
    val newTimeStamp : unit -> timeStamp = Time.now
    (* Get the date of a file. *)
    val fileTimeStamp : string -> timeStamp = OS.FileSys.modTime
    (* String representation - includes trailing "\n"! *)
    fun stringOfTimeStamp (t : timeStamp) : string =
        Date.toString(Date.fromTimeLocal t) ^ "\n"
    
    local
        open Universal
    in
        val timeStampTagMethods    : timeStamp tag   = tag ();
        val dependenciesTagMethods : string list tag = tag ();
    end;

    fun lastMade (objectName : string) : timeStamp =
        getOpt(UniversalArray.sub(globalTable, timeStampTagMethods, objectName), firstTimeStamp);

    (* Main make function *)
    fun make (targetName: string) : unit =
    let
        (* This serves two purposes. It provides a list of objects which have been
           re-made to prevent them being made more than once, and it also prevents
           circular dependencies from causing infinite loops (e.g. let x = f(x)) *)
            local
                open HashArray;
                val htab : compileState hash = hash 10;
            in
                fun lookupStatus (name: string) : compileState =
                    getOpt(sub (htab, name), NotProcessed);
                  
                fun setStatus (name: string, cs: compileState) : unit =
                    update (htab, name, cs)
            end;

        (* Remove leading directory names to get the name of the object itself.
           e.g. "../compiler/parsetree/gencode" yields simply "gencode". *)
        val (dirName,objectName) = splitFilename targetName;
 
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
                else OS.Path.joinDirFile{dir=directory, file=objName};

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
                        SOME res' => res'
                    |   NONE      => raise Fail ("No such file or directory ("^file^","^dir^")")
                end ;
            
            val fileName = longName fileTuple;

            val newFindDirectory : string -> string =
                if objIsDir
                then
                let
                    (* Look in this directory then in the ones above. *)
                    fun findDirectoryHere (name: string) : string =
                        case filePresent (here, name) of
                          NONE => findDirectory name (* not in this directory *)
                        | _    => here;
                in
                    findDirectoryHere
                end
                else findDirectory;
    
            (* Compiles a file. *)
            fun remakeCurrentObj () =
            let
                val () = print ("Making " ^ objName ^ "\n");
                local
                    (* Keep a list of the dependencies. *)
                    val deps : bool HashArray.hash = HashArray.hash 10;
                    
                    fun addDep name =
                        if getOpt(HashArray.sub (deps, name), true)
                        then HashArray.update(deps, name, true)
                        else ();
                    
                    (* Called by the compiler to look-up a global identifier. *)
                    fun lookupMakeEnv globalLook (name: string) : 'a option =
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
                           print("Circular dependency: " ^ name ^  " depends on itself\n")

                        | Checked => addDep name; (* Add this to the dependencies. *)

                        (* There was previously a comment about returning NONE here if
                           we had a problem remaking a dependency. *)
                        globalLook name
                    end; (* lookupMakeEnv *)

                     (* Enter the declared value in the table. Usually this will be the
                        target we are making. Also set the state to "Checked". The
                        state is set to checked when we finish making the object but
                        setting it now suppresses messages about circular dependencies
                        if we use the identifier within the file. *)
                    fun enterMakeEnv (kind : string, enterGlobal) (name: string, v: 'a) : unit =
                    (
                        (* Put in the value. *)
                        enterGlobal (name, v);
                        print ("Created " ^ kind ^ " " ^ name ^ "\n");
                        
                        (* The name we're declaring may appear to be a dependency
                           but isn't, so don't include it in the list. *)
                        HashArray.update (deps, name, false);
                        
                        if name = objName
                        then
                        let
                            (* Put in the dependencies i.e. those names set to true in the table. *)
                            val depends =
                                HashArray.fold (fn (s, v, l) => if v then s :: l else l) [] deps;
                            
                            (* Put in a time stamp for the new object.  We need to make
                               sure that it is no older than the newest object it depends
                               on.  In theory that should not be a problem but clocks on
                               different machines can get out of step leading to objects
                               made later having earlier time stamps. *)
                            val newest =
                                List.foldl (fn (s: string, t: timeStamp) =>
                                    maxTime (lastMade s, t)) (fileTimeStamp fileName) depends;
                            
                            val timeStamp = maxTime(newest, newTimeStamp());
                        in         
                            setStatus (name, Checked);
                            UniversalArray.update(globalTable, dependenciesTagMethods, name, depends);
                            UniversalArray.update(globalTable, timeStampTagMethods, name, timeStamp)
                        end
                        else ()
                    ) (* enterMakeEnv *);
     
                in
                    val makeEnv =
                        { 
                            lookupFix    = #lookupFix globalNameSpace,
                            lookupVal    = #lookupVal globalNameSpace,
                            lookupType   = #lookupType globalNameSpace,
                            lookupSig    = lookupMakeEnv (#lookupSig globalNameSpace),
                            lookupStruct = lookupMakeEnv (#lookupStruct globalNameSpace),
                            lookupFunct  = lookupMakeEnv (#lookupFunct globalNameSpace),
                            enterFix     = #enterFix globalNameSpace,
                            enterVal     = #enterVal globalNameSpace,
                            enterType    = #enterType globalNameSpace,
                            enterStruct  = enterMakeEnv ("signature", #enterStruct globalNameSpace),
                            enterSig     = enterMakeEnv ("signature", #enterSig globalNameSpace),
                            enterFunct   = enterMakeEnv ("signature", #enterFunct globalNameSpace),
                            allFix       = #allFix globalNameSpace,
                            allVal       = #allVal globalNameSpace,
                            allType      = #allType globalNameSpace,
                            allSig       = #allSig globalNameSpace,
                            allStruct    = #allStruct globalNameSpace,
                            allFunct     = #allFunct globalNameSpace
                        };
                end; (* local for makeEnv *)

                val inputFile = OS.Path.joinDirFile{dir= #1 fileTuple, file= #2 fileTuple}
    
                val inStream = TextIO.openIn inputFile;
    
                val () =
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
                in
                    while not (!endOfStream) do
                    let
                        val code = PolyML.compiler
                           { nameSpace = makeEnv, getChar = getChar, fileName = fileName,
                             lineNumber = fn () => !lineNo, putString = print }
                    in
                        code ()
                            handle exn as Fail _ => raise exn
                            |  exn =>
                            (
                                print ("Exception- " ^ General.exnMessage exn ^ " raised\n");
                                raise exn
                            )
                    end
                end (* body of scope of inStream *)
                    handle exn => (* close inStream if an error occurs *)
                    (
                        TextIO.closeIn inStream;
                        raise exn
                    )
            in (* remake normal termination *)
                TextIO.closeIn inStream
            end (* remakeCurrentObj *)
            
        in (* body of remakeObj *)
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
                        print ("Circular dependency: " ^ s ^ " depends on itself\n")
                    
                    |  Checked => () (* do nothing *)
                    
                
                (* Process each entry and return true if
                   any is newer than the target. *)
                val processChildren =
                    List.foldl
                    (fn (child:string, parentNeedsMake:bool) =>
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
                        case UniversalArray.sub(globalTable, dependenciesTagMethods, objName) of
                            SOME d => processChildren d
                        |   NONE => true (* No dependency list - must use "make" on it. *)
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
                ObjNotFile => setStatus (objName, Checked)
            
            |   exn => (* Compilation (or execution) error. *)
                (
                    (* Mark as checked to prevent spurious messages. *)
                    setStatus (objName, Checked);
                    raise exn
                )
    in (*  body of make *)
        (* Check that the target exists. *)
        case filePresent (dirName, objectName) of
            NONE =>
            let
                val dir =
                    if dirName = "" then ""
                    else " (directory "^dirName^")";
                val s = "File "^objectName^" not found" ^ dir
            in
                print (s ^ "\n");
                raise Fail s
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
                    filePresent(dirName,  s) <> NONE
                then dirName
                else raise ObjNotFile;
        in
            remakeObj objectName findDirectory
                handle exn  => 
                (
                    print (targetName ^ " was not declared\n");
                    raise exn
                )
        end
    end (* make *)
 

    (*****************************************************************************)
    (*                  shell                                                    *)
    (*****************************************************************************)
    fun topLevel isDebug (debugEnv, exitLoop) : unit =
    let
        (* This is used as the main read-eval-print loop.  It is also invoked
           by running code that has been compiled with the debug option on
           when it stops at a breakpoint.  In that case debugEnv contains an
           environment formed from the local variables.  This is placed in front
           of the normal top-level environment. *)
        val compositeNameSpace = (* Compose any debugEnv with the global environment *)
        let
            infix ++
            (* Compose lookup functions. *)
            fun (a ++ b) s = case a s of NONE => b s | v => v
        in
            {
            lookupFix    = (#lookupFix debugEnv) ++ (#lookupFix globalNameSpace),
            lookupSig    = (#lookupSig debugEnv) ++ (#lookupSig globalNameSpace),
            lookupVal    = (#lookupVal debugEnv) ++ (#lookupVal globalNameSpace),
            lookupType   = (#lookupType debugEnv) ++ (#lookupType globalNameSpace),
            lookupFunct  = (#lookupFunct debugEnv) ++ (#lookupFunct globalNameSpace),
            lookupStruct = (#lookupStruct debugEnv) ++ (#lookupStruct globalNameSpace),
            enterFix     = #enterFix globalNameSpace,
            enterSig     = #enterSig globalNameSpace,
            enterVal     = #enterVal globalNameSpace,
            enterType    = #enterType globalNameSpace,
            enterFunct   = #enterFunct globalNameSpace,
            enterStruct  = #enterStruct globalNameSpace,
            allFix       = #allFix globalNameSpace,
            allSig       = #allSig globalNameSpace,
            allVal       = #allVal globalNameSpace,
            allType      = #allType globalNameSpace,
            allFunct     = #allFunct globalNameSpace,
            allStruct    = #allStruct globalNameSpace
            }
        end
        
        (* Don't use the end_of_stream because it may have been set by typing
           EOT to the command we were running. *)
        val endOfFile    = ref false;
        val realDataRead = ref false;
        val lastWasEol   = ref true;
        
        val firstPrompt = if isDebug then "debug >" else "> "
        and secondPrompt = if isDebug then "debug #" else "# "

        (* Each character typed is fed into the compiler but leading
           blank lines result in the prompt remaining as firstPrompt until
           significant characters are typed. *)
        fun readin () : char =
        let
            val setPrompt : unit =
                if !lastWasEol (* Start of line *)
                then if !realDataRead
                then print (secondPrompt)
                else print (firstPrompt)
                else ();
         in
            case TextIO.input1 TextIO.stdIn of
                NONE => (endOfFile := true; eofChar (* control-D = End of file *))
            |   SOME #"\n" => ( lastWasEol := true; #"\n" )
            |   SOME ch =>
                   (
                       lastWasEol := false;
                       if ch <> #" "
                       then realDataRead := true
                       else ();
                       ch
                   )
        end; (* readin *)

        (* Remove all buffered but unread input. *)
        fun flushInput () =
            case TextIO.canInput(TextIO.stdIn, 1) of
                SOME 1 => (TextIO.inputN(TextIO.stdIn, 1); flushInput())
            |   _ => (* No input waiting or we're at EOF. *) ()

        fun readEvalPrint () : unit =
        let
            (* If we have executed a deeply recursive function the stack will have
               extended to be very large.  It's better to reduce the stack if we
               can.  This is RISKY.  Each function checks on entry that the stack has
               sufficient space for everything it will allocate and assumes the stack
               will not shrink.  It's unlikely that any of the functions here will
               have asked for very much but as a precaution we allow for an extra 8k words. *)
            fun shrink_stack (newsize : int) : unit = 
                RunCall.run_call1 RuntimeCalls.POLY_SYS_shrink_stack newsize
            val () = if isDebug then () else shrink_stack 8000;
        in
            realDataRead := false;
            (* Compile and then run the code. *)
            let
                val code =
                    PolyML.compiler
                        { nameSpace = compositeNameSpace, getChar = readin, fileName = "",
                          lineNumber = fn () => 0, putString = print }
                    handle Fail s => 
                    (
                        print(s ^ "\n");
                        flushInput();
                        lastWasEol := true;
                        raise Fail s
                    )
            in
                code ()
                (* Report exceptions in running code.
                   N.B. General.exnMessage uses the environment set by setPrintEnv *)
                    handle  exn =>
                    (
                        print ("Exception- " ^ General.exnMessage exn ^ " raised\n");
                        raise exn
                    )
            end
        end; (* readEvalPrint *)
        
        (* If we are debugging we may pass exceptions back to the
           debugged function. *) 
        fun handleDebuggingException () =
        let
            val _ = print "Pass exception to function being debugged (y/n)?";
        in
            flushInput ();
            case TextIO.input1 TextIO.stdIn of
                NONE => false
            |   SOME #"y" => false
            |   SOME #"n" => true
            |   _ => handleDebuggingException()
        end
        
        fun handledLoop () : unit =
        (
            (* Process a single top-level command. *)
            readEvalPrint()
                handle exn =>
                    if not isDebug orelse handleDebuggingException()
                    then ()
                    else raise exn;
            (* Exit if we've seen end-of-file or we're in the debugger
               and we've run "continue". *)
            if !endOfFile orelse exitLoop() then ()
            else handledLoop ()
        )
    in
        handledLoop ()  
    end

    (* Normal, non-debugging top-level loop. *)
    fun shell () = 
        topLevel false
            ({ lookupFix = fn _ => NONE, lookupSig = fn _ => NONE, lookupVal = fn _ => NONE,
               lookupType = fn _ => NONE, lookupFunct = fn _ => NONE, lookupStruct = fn _ => NONE},
             fn _ => false)

    val () = Bootstrap.setDebugShell (topLevel true)

    (* This is the root function to run the Poly/ML top level. *)
    fun rootShell () =
	let
        val argList = CommandLine.arguments();
		fun rtsRelease() = RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (10, ())
		fun rtsCopyright() = RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (11, ())
		fun rtsHelp() = RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (19, ())
    in
	    if List.exists(fn s => s = "-v") argList
		then (* -v option : Print version information and exit *)
    		print (String.concat ["Poly/ML ", PolyML.Compiler.compilerVersion, 
			                     "    RTS version: ", rtsRelease(), "\n"])

		else if List.exists(fn s => s = "--help") argList
		then (* --help option: Print argument information and exit. *)
		   (
     		print (String.concat ["Poly/ML ", PolyML.Compiler.compilerVersion, "\n"]);
			print "Compiler arguments:\n";
			print "\n";
			print "-v        Print the version of Poly/ML and exit\n";
			print "--help    Print this message and exit\n";
			print "-q        Suppress the start-up message\n";
			print "\nRun time system arguments:\n";
			print (rtsHelp())
		   )
		else (* Enter Poly/ML. *)
        let
            open Signal;
            val () =
        	    if List.exists(fn s => s = "-q") (CommandLine.arguments())
        		then ()
        		else print (String.concat ["Poly/ML ", PolyML.Compiler.compilerVersion, "\n"]);
            (* Set up a handler for SIGINT if that is currently set to SIG_DFL.
               If a handler has been set up by an initialisation function don't replace it. *)
            val () =
                case signal(2, SIG_IGN) of
                   SIG_IGN => ()
                |  SIG_DFL => (signal(2, SIG_HANDLE(fn _ => Thread.Thread.broadcastInterrupt())); ())
                |  oldHandle => (signal(2, oldHandle); ())
        in
            shell();
            OS.Process.exit OS.Process.success (* Run any "atExit" functions and then quit. *)
        end
	end;

in
    structure PolyML =
    struct
        open PolyML
        (* We must not have a signature on the result otherwise print and makestring
           will be given polymorphic types and will only produce "?" *)

        val globalNameSpace = globalNameSpace
        (* This is used to look up exceptions and infixes. *)
        val () = setPrintEnv globalNameSpace

        val use = use and make = make
        val suffixes = suffixes

        (* Main root function: run the main loop. *)
        val rootFunction: unit->unit = rootShell

        (* Original print_depth etc functions. *)
        fun profiling   i = Compiler.profiling := i
        and timing      b = Compiler.timing := b
        and print_depth i = Compiler.printDepth := i
        and error_depth i = Compiler.errorDepth := i
        and line_length i = Compiler.lineLength := i

        structure Compiler =
        struct
            open Compiler
            val forgetSignature: string -> unit = forgetSig
            and forgetStructure: string -> unit = forgetStruct
            and forgetFunctor: string -> unit = forgetFunct
            and forgetValue: string -> unit = forgetVal
            and forgetType: string -> unit = forgetType
            and forgetFixity: string -> unit = forgetFix

            fun signatureNames (): string list = #1(ListPair.unzip (#allSig globalNameSpace ()))
            and structureNames (): string list = #1(ListPair.unzip (#allStruct globalNameSpace ()))
            and functorNames (): string list = #1(ListPair.unzip (#allFunct globalNameSpace ()))
            and valueNames (): string list = #1(ListPair.unzip (#allVal globalNameSpace ()))
            and typeNames (): string list = #1(ListPair.unzip (#allType globalNameSpace ()))
            and fixityNames (): string list = #1(ListPair.unzip (#allFix globalNameSpace ()))
             
        end
    end
end (* PolyML. *);


(* Copy everything out of the original name space. *)
(* Do this AFTER we've finished compiling PolyML. *)
val () = List.app (#enterVal PolyML.globalNameSpace) (#allVal Bootstrap.globalSpace ())
and () = List.app (#enterFix PolyML.globalNameSpace) (#allFix Bootstrap.globalSpace ())
and () = List.app (#enterSig PolyML.globalNameSpace) (#allSig Bootstrap.globalSpace ())
and () = List.app (#enterType PolyML.globalNameSpace) (#allType Bootstrap.globalSpace ())
and () = List.app (#enterFunct PolyML.globalNameSpace) (#allFunct Bootstrap.globalSpace ())
and () = List.app (#enterStruct PolyML.globalNameSpace) (#allStruct Bootstrap.globalSpace ())

(* We don't want Bootstrap copied over. *)
val () = PolyML.Compiler.forgetStructure "Bootstrap";

val use = PolyML.use;


