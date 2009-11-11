(*
    Title:      Root function for the PolyML structure
    Author:     David Matthews
    Copyright   David Matthews 2009

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

(* This contains the code for the IDE protocol as well as the normal
   Poly/ML top-level loop. *)

local
    (* Parse trees for topdecs in current file. *)
    val parseTree = ref ("", []) (* Parsetree ID and parsetrees as a list. *)
    (* "parseTree is not completely interlocked against parallel access. *)

    fun runIDEProtocol () =
    let
        (* Save the last parsetree here. *)
        val lastParsetree =
            ref (case parseTree of ref(_, hd::_) => SOME hd | _ => NONE)

        val parseLock = Thread.Mutex.mutex()

        (* Access the parse tree and other information with the lock held. *)
        fun withLock f =
        let
            open Thread.Thread Thread.Mutex
            val originalState = getAttributes()
            val () = setAttributes[InterruptState InterruptDefer]
            val () = lock parseLock
            val result = f ()
            val () = unlock parseLock
            val () = setAttributes originalState
        in
            result
        end

        local
            (* Separate out the output stream.  We need to interlock access to stdOut
               to avoid user code outputing within a packet. *)
            open TextIO TextIO.StreamIO
            val outStream = getOutstream stdOut
            val (writer, buffMode) = getWriter outStream
            val TextPrimIO.WR
                { name, chunkSize, writeVec, writeArr, block, canOutput, ioDesc, ... } = writer
            val outputLock = Thread.Mutex.mutex()
            (* Create a version of the stream that locks before actually sending output. *)
            val lockedWriteVec =
                case writeVec of
                    NONE => NONE
                |   SOME writeVec =>
                        SOME(fn a => LibraryIOSupport.protect outputLock writeVec a)
            val lockedWriteArray =
                case writeArr of
                    NONE => NONE
                |   SOME writeArr =>
                        SOME(fn a => LibraryIOSupport.protect outputLock writeArr a)
            val lockedWriter =
                TextPrimIO.WR { name = name, chunkSize = chunkSize,
                                writeVec = lockedWriteVec, writeArr = lockedWriteArray,
                                writeVecNB = NONE, writeArrNB = NONE, block = block, canOutput = canOutput,
                                getPos = NONE, setPos = NONE, endPos = NONE, verifyPos = NONE,
                                close = fn () => raise Fail "stdOut must not be closed", ioDesc = ioDesc }
            (* Use this locked version for normal stdOut. *)
            val () = setOutstream(stdOut,
                        StreamIO.mkOutstream(TextPrimIO.augmentWriter lockedWriter, buffMode))
            (* Create an unlocked version for use within the IDE code.  When writing to this
               stream the IDE code will first get a lock, then output the whole packet before
               releasing the lock.  Because mutexes are not recursive we can't use the locking
               version. *)
            val unLockedWriter =
                TextPrimIO.WR { name = name, chunkSize = chunkSize, writeVec = writeVec, writeArr = writeArr,
                                writeVecNB = NONE, writeArrNB = NONE, block = block, canOutput = canOutput,
                                getPos = NONE, setPos = NONE, endPos = NONE, verifyPos = NONE,
                                close = fn () => raise Fail "stdOut must not be closed", ioDesc = ioDesc }
        in
            val unlockedStream = StreamIO.mkOutstream(TextPrimIO.augmentWriter unLockedWriter, buffMode)
            val outputLock = outputLock
        end

        type basicLoc = (* Locations in request packets. *) { startOffset: int, endOffset: int }
        type compileError = { hardError: bool, location: PolyML.location, message: string }

        datatype request =
            (* Requests sent by the IDE to Poly/ML. *)
            PropertyRequest (* O *)
                of { requestId: string, parseTreeId: string, location: basicLoc }
        |   MoveRequest (* M *)
                of { requestId: string, parseTreeId: string, location: basicLoc, direction: string }
        |   TypeRequest (* T *)
                of { requestId: string, parseTreeId: string, location: basicLoc }
        |   DecRequest (* I *)
                of { requestId: string, parseTreeId: string, location: basicLoc, decType: string }
        |   RefRequest (* V *)
                of { requestId: string, parseTreeId: string, location: basicLoc }
        |   CompileRequest (* R *)
                of { requestId: string, fileName: string, startPosition: int,
                     preludeCode: string, sourceCode: string }
        |   KillRequest (* K *)
                of { requestId: string }
        |   UnknownRequest (* Provided for upwards compatibility. *)
                of { startCh: char }

        and response =
            (* Replies sent from Poly/ML to the IDE. *)
            PropertyResponse (* O *)
                of { requestId: string, parseTreeId: string, location: basicLoc, commands: string list }
        |   MoveResponse  (* M *)
                of { requestId: string, parseTreeId: string, location: basicLoc }
        |   TypeResponse (* T *)
                of { requestId: string, parseTreeId: string, location: basicLoc, typeRes: string option }
        |   DecResponse (* I *)
                of { requestId: string, parseTreeId: string, location: basicLoc,
                     decLocation: PolyML.location option }
        |   RefResponse (* V *)
                of { requestId: string, parseTreeId: string, location: basicLoc, references: basicLoc list }
        |   CompilerResponse (* R *)
                of { requestId: string, parseTreeId: string, finalOffset: int, result: compileResult }
        |   UnknownResponse (* Provided for upwards compatibility. *)
                of { startCh: char }

        and compileResult =
            Succeeded of compileError list
        |   RuntimeException of string * compileError list
        |   PreludeFail of string
        |   CompileFail of compileError list
        |   CompileCancelled of compileError list

        fun protocolError error =
        let
            open OS.Process
        in
            TextIO.print ("Protocol error: " ^ error) handle _ => ();
            exit failure;
            raise Fail "bad" (* Never called but sets return type as 'a *)
        end

        (* Reads a request.  Calls OS.Process.exit at end-of-file or on a protocol error. *)
        fun readRequest (): request =
        let
            open TextIO

            (* Returns the string as far as the next ESC and the terminator. *)
            fun readToEscape (soFar: string, terminator) : string =
            case input1 stdIn of
                SOME #"\u001b" =>
                (
                    case input1 stdIn of
                        NONE => protocolError "End of file"
                    |   SOME ch =>
                            if ch = terminator
                            then soFar
                            else if ch = #"\u001b" (* Escaped ESC. *)
                            then readToEscape(soFar ^ str #"\u001b", terminator)
                            else protocolError(str ch ^ " not " ^ str terminator)
                )
            |   SOME ch => readToEscape(soFar ^ str ch, terminator)
            |   NONE => protocolError "End of file"

            (* Parse an integer.  Returns zero if it isn't a valid int. *)
            fun getInt termCh : int =
                case Int.fromString (readToEscape("", termCh)) of
                    NONE => 0
                |   SOME i => i

            val () =
                case input1 stdIn of
                    NONE => OS.Process.exit OS.Process.success (* Close down. *)
                |   SOME #"\u001b" => () (* Escape- start of packet. *)
                |   SOME ch => protocolError(str ch ^ " not ESCAPE at start of packet")
            val startCh = (* Request code *)
                case input1 stdIn of
                    NONE => protocolError "End of file"
                |   SOME ch => ch
        in
            case startCh of
                #"R" =>
                let (* Compile request. *)
                    (* Begin a new compilation. *)
                    val requestId = readToEscape("", #",")
                    val fileName = readToEscape("", #",")
                    val startPosition = getInt #","
                    (* The next two are the lengths *)
                    val preludeLength = getInt #","
                    val sourceLength = getInt #","
                    (* *)
                    val preludeCode = TextIO.inputN(TextIO.stdIn, preludeLength)
                    val _ = readToEscape("", #",") (* Should be empty - check? *)
                    val sourceText = TextIO.inputN(TextIO.stdIn, sourceLength)
                    val _ = readToEscape("", #"r") (* Should be empty - check? *)
                in
                    CompileRequest { requestId = requestId, fileName = fileName, startPosition = startPosition,
                             preludeCode = preludeCode, sourceCode = sourceText }
                end

                (* Navigation functions. *)
                
            |   #"M" =>
                let
                    val requestId = readToEscape("", #",")
                    val parseTreeId = readToEscape("", #",")
                    val startOffset = getInt #","
                    val endOffset = getInt #","
                    val requestType = readToEscape("", #"m")
                in
                    MoveRequest{
                        requestId = requestId, parseTreeId = parseTreeId, direction= requestType,
                        location = { startOffset = startOffset, endOffset = endOffset }
                        }
                end

                (* Print the type of the selected node. *)
            |   #"T" =>
                let
                    val requestId = readToEscape("", #",")
                    val parseTreeId = readToEscape("", #",")
                    val startOffset = getInt #","
                    val endOffset = getInt #"t"
                in
                    TypeRequest{
                        requestId = requestId, parseTreeId = parseTreeId,
                        location = { startOffset = startOffset, endOffset = endOffset }
                        }
                end

                (* Print the declaration location of the selected node. *)
            |   #"I" =>
                let
                    val requestId = readToEscape("", #",")
                    val parseTreeId = readToEscape("", #",")
                    val startOffset = getInt #","
                    val endOffset = getInt #","
                    val decType = readToEscape("", #"i")
                in
                    DecRequest{
                        requestId = requestId, parseTreeId = parseTreeId, decType = decType,
                        location = { startOffset = startOffset, endOffset = endOffset }
                        }
                end

                (* Return the local references to the given identifier. *)
            |   #"V" =>
                let
                    val requestId = readToEscape("", #",")
                    val parseTreeId = readToEscape("", #",")
                    val startOffset = getInt #","
                    val endOffset = getInt #"v"
                in
                    RefRequest{
                        requestId = requestId, parseTreeId = parseTreeId,
                        location = { startOffset = startOffset, endOffset = endOffset }
                        }
                end

            |   #"O" => (* Print list of valid commands. *)
                let
                    val requestId = readToEscape("", #",")
                    val parseTreeId = readToEscape("", #",")
                    val startOffset = getInt #","
                    val endOffset = getInt #"o"
                in
                    PropertyRequest{
                        requestId = requestId, parseTreeId = parseTreeId,
                        location = { startOffset = startOffset, endOffset = endOffset }
                        }
                end

            |   #"K" => (* Cancel request. *)
                    KillRequest { requestId = readToEscape ("", #"k") }

            |   ch => (* Something else.  Reply with empty response. *)
                let
                    (* Unlike the other cases we don't know what may follow ESCAPE. *)
                    val terminator = Char.toLower ch
                    fun skipToTerminator () =
                    case input1 stdIn of
                        SOME #"\u001b" =>
                        (
                            case input1 stdIn of
                                NONE => protocolError "End of file"
                            |   SOME ch =>
                                    if ch = terminator
                                    then () (* Found the end. *)
                                    else (* Some internal escape code. *) skipToTerminator()
                        )
                    |   SOME _ => skipToTerminator ()
                    |   NONE => protocolError "End of file"
                in
                    skipToTerminator ();
                    UnknownRequest { startCh = ch }
                end
        end

        fun sendStartedMessage () = 
        let 
            fun print s = TextIO.StreamIO.output(unlockedStream, s)
            fun printEsc ch = print (String.concat["\u001b", String.str ch])
            fun sendResponse () =
            ( (* send the version number of the protocol *)
                printEsc #"H"; print "1.0.0"; printEsc #"h";
                TextIO.StreamIO.flushOut unlockedStream
            )
        in
            LibraryIOSupport.protect outputLock sendResponse ()
        end
            
        (* Send a reply packet. *)
        fun sendResponse response =
        let
            fun print s = TextIO.StreamIO.output(unlockedStream, s)
            fun printEsc ch = print (String.concat["\u001b", String.str ch])

            fun printLocation {startOffset, endOffset } =
                print (String.concat[Int.toString startOffset, "\u001b,", Int.toString endOffset])

            and printFullLocation { file, startLine, startPosition, endPosition, ...} =
            (
                print file; (* TODO double any escapes. *) printEsc #",";
                print (Int.toString startLine); printEsc #",";
                print (Int.toString startPosition); printEsc #",";
                print (Int.toString endPosition)
            )

            fun makeResponse (PropertyResponse { requestId, parseTreeId, location, commands }) =
                let
                    fun printCommand comm = (printEsc #","; print comm)
                in
                    printEsc #"O";
                    print requestId; printEsc #",";
                    print parseTreeId; printEsc #",";
                    printLocation location;
                    List.app printCommand commands;
                    printEsc #"o"
                end

            |   makeResponse (MoveResponse { requestId, parseTreeId, location }) =
                (
                    printEsc #"M";
                    print requestId; printEsc #",";
                    print parseTreeId; printEsc #",";
                    printLocation location;
                    printEsc #"m"
                )

            |   makeResponse (TypeResponse { requestId, parseTreeId, location, typeRes }) =
                (
                    printEsc #"T";
                    print requestId; printEsc #",";
                    print parseTreeId; printEsc #",";
                    printLocation location;
                    case typeRes of
                        NONE => ()
                    |   SOME typeRes =>
                        (
                            printEsc #",";
                            print typeRes
                        );
                    printEsc #"t"
                )

            |   makeResponse (DecResponse { requestId, parseTreeId, location, decLocation }) =
                (
                    printEsc #"I";
                    print requestId; printEsc #",";
                    print parseTreeId; printEsc #",";
                    printLocation location;
                    case decLocation of
                        SOME location => (printEsc #","; printFullLocation location)
                    |   NONE => ();
                    printEsc #"i"
                )

            |   makeResponse (RefResponse { requestId, parseTreeId, location, references }) =
                (
                    printEsc #"V";
                    print requestId; printEsc #",";
                    print parseTreeId;  printEsc #",";
                    printLocation location;
                    List.app (fn loc => (printEsc #","; printLocation loc)) references;
                    printEsc #"v"
                )

            |   makeResponse (CompilerResponse { requestId, parseTreeId, finalOffset, result }) =
                let
                    fun printError { hardError, location, message } =
                    (
                        printEsc #"E";
                        if hardError then print "E" else print "W";
                        printEsc #",";
                        printFullLocation location;
                        printEsc #";"; (* N.B. Semicolon here, not comma. *)
                        print message; (* May include markup *)
                        printEsc #"e"
                    )
                    fun printOffset() = (printEsc #","; print (Int.toString finalOffset))
                    fun printErrors errors = (List.app printError errors)
                in
                    printEsc #"R";
                    print requestId; printEsc #",";
                    print parseTreeId; printEsc #",";
                    case result of
                        Succeeded errors => (print "S"; printOffset(); printEsc #";"; printErrors errors)
                    |   RuntimeException (s, errors) =>
                        (
                            print "X"; printOffset(); 
                            printEsc #";";
                            printEsc #"X"; print s; (* May include markup *)
                            printEsc #"x"; 
                            printErrors errors
                        )
                    |   PreludeFail s =>
                        ( print "L"; printOffset(); printEsc #";"; print s (* May include markup *) )
                    |   CompileFail errors =>
                        ( print "F"; printOffset(); printEsc #";"; printErrors errors )
                    |   CompileCancelled errors =>
                        ( print "C"; printOffset(); printEsc #";"; printErrors errors );
                    printEsc #"r"
                 end

            |   makeResponse (UnknownResponse { startCh: char }) =
                (* Response to unknown command - return empty result. *)
                ( printEsc startCh; printEsc (Char.toLower startCh))

            fun sendResponse () =
            (
                makeResponse response handle _ => protocolError "Exception";
                TextIO.StreamIO.flushOut unlockedStream
            )
        in
            (* Sending the response packet must be atomic with respect to any other
               output to stdOut. *)
            LibraryIOSupport.protect outputLock sendResponse ()
        end

        (* Get the current parse tree and identifier. *)
        fun getCurrentParse() =
            withLock (fn () => let val (id, trees) = ! parseTree in (trees, ! lastParsetree, id) end)
        (* Update lastParsetree if the id is still valid. *)
        fun updateLastParse(id, pt) =
        let
            fun f () =
            if id = #1 (! parseTree) then lastParsetree := pt else ()
        in
            withLock f
        end
        (* Set parse tree and ID as a result of a compilation.  Sets lastParsetree to the
           head of the updated parse tree. *)
        fun setParseTree(pt, id) =
        let
            fun f () =
            (
                parseTree := (id, pt); 
                case pt of
                    [] => lastParsetree := NONE
                |   hd :: _ => lastParsetree := SOME hd
            )
        in
            withLock f
        end

        (* The source text may consist of several "programs".  When we compile a "program" we
           have to provide a way for the parsetree for this "program" to navigate to others
           even though they won't have been compiled yet.  This enables it to work. *)
        (* We have to return functions for the parent, for the next sibling even if there
           isn't one and for the previous sibling. *)
        fun toplevelParseTree (parseRootRef as ref currentList) =
        let
            open PolyML
            (* This is called when we have processed the previous "programs" but
               not yet processed this one. *)
            fun makelist([], _) = (* Shouldn't happen *) raise Fail "Null list"
            |   makelist(l as (locn, props) :: tl, previous) =
                let
                    fun this () = makelist(l, previous)
                    (* If there is another item in the list we need a
                       property that moves there whose "previous" property
                       comes here. *)
                    val next =
                        case tl of
                            [] => []
                        |   _ => [PTnextSibling(
                                    fn () => makelist(tl, [PTpreviousSibling this]))]
                in
                    (locn, previous @ next @ props)
                end
            fun parent () =
                case ! parseRootRef of
                    [] => raise Fail "Empty Tree"
                |   trees as (hd :: _) =>
                    let
                        (* Navigation for one or more topdecs. *)
                        val fullLoc =
                            case (hd, List.last trees) of
                                (({ file, startLine, startPosition, ... }, _),
                                 ({ endLine, endPosition, ... }, _)) =>
                                {
                                    file=file, startLine=startLine,
                                    startPosition=startPosition,
                                    endLine=endLine, endPosition=endPosition
                                }
                    in
                        (fullLoc, [PTfirstChild(fn () => makelist(trees, []))])
                    end

            val itemCount = List.length currentList

            fun moveToNth n =
            let
                fun move (tree, 0) = tree
                |   move ((loc, opts), n) =
                    case List.find(fn PTnextSibling _ => true | _ => false) opts of
                        NONE =>
                        let
                            (* We have to put a dummy item in at the end since when we
                               created the parent properties for the last "program" we will
                               have passed in a "next" entry even though there wasn't
                               actually a "next". *)
                            val { file, startLine, startPosition, ... } = loc
                            val lastPos =
                                { file = file, startLine = startLine, endLine = startLine,
                                  startPosition = startPosition, endPosition = startPosition }
                            val opts =
                                List.filter(fn PTparent _ => true | PTpreviousSibling _ => true | _ => false) opts
                        in
                            (lastPos, opts)
                        end
                    |   SOME (PTnextSibling f) => move(f(), n-1)
                    |   SOME _ => raise Match (* Shouldn't happen *)
            in
                case ! parseRootRef of
                    [] => raise Fail "Empty Tree"
                |   trees => move(makelist(trees, []), n)
            end
            val previous =
                case currentList of
                    [] => NONE (* This is the first. *)
                |   _ => SOME(fn () => moveToNth(itemCount-1))
            fun next () = moveToNth(itemCount+1)
        in
            { parent = SOME parent, next = SOME next, previous = previous }
        end

        (* Move in the selected direction.  Returns the tree as the result of the move. *)
        fun navigateTo(searchLocation as {startOffset, endOffset}, lastParsetree) =
        case lastParsetree of
            NONE => NONE
        |   SOME({ startPosition, endPosition, ... }, tree) =>
            let
                open PolyML
                datatype direction = Up | Down | Left | Right
                fun find([], _) = NONE (* No change *)
                |   find(PTparent p :: _, Up) = SOME p
                |   find(PTpreviousSibling p :: _, Left) = SOME p
                |   find(PTnextSibling p :: _, Right) = SOME p
                |   find(PTfirstChild p :: _, Down) = SOME p
                |   find(_ :: tl, dir) = find (tl, dir)
            in
                if startOffset = startPosition andalso endOffset = endPosition
                then (* We're there already. *) lastParsetree
                else if startOffset >= startPosition andalso endOffset <= endPosition
                then (* It's this node or a child. *)
                    let
                        val child = find(tree, Down)
                    in
                        (* See if the element we want is actually a child. *)
                        case child of
                            SOME child =>
                            let
                                (* See which child it is. *)
                                fun findChild(location as {startPosition, endPosition, ...}, child) =
                                    if startOffset >= startPosition andalso endOffset <= endPosition
                                    then SOME (location, child)
                                    else
                                    case find(child, Right) of
                                        NONE => NONE
                                    |   SOME next => findChild(next())
                            in
                                case findChild(child()) of
                                    NONE => lastParsetree (* In this *)
                                |   SOME child => navigateTo(searchLocation, SOME child)
                            end
                        |   NONE => lastParsetree (* No children. *)
                    end
                else (* Must go out. *)
                (
                    case find(tree, Up) of
                        SOME p => navigateTo(searchLocation, SOME(p()))
                    |   NONE => NONE (* Not found *)
                )
            end

        (* Main protocol loop. *)
        fun runProtocol currentCompilation =
        let
            (* Return the location of the given tree. *)
            fun treeLocation NONE = {startOffset = 0, endOffset = 0}
            |   treeLocation (SOME ({startPosition, endPosition, ...}, _)) =
                        {startOffset = startPosition, endOffset = endPosition}

            (* Pretty print a message and return the output string. *)
            fun prettyAsString message =
            let
                val result = ref []
                fun doPrint s = result := s :: ! result
                val () = PolyML.prettyPrint(doPrint, !PolyML.Compiler.lineLength) message
            in
                String.concat(List.rev(! result))
            end
            and prettyMarkupAsString message =
            let
                val result = ref []
                fun doPrint s = result := s :: ! result
                val () = PolyML.prettyPrintWithIDEMarkup(doPrint, !PolyML.Compiler.lineLength) message
            in
                String.concat(List.rev(! result))
            end
        in
            case readRequest () of
                PropertyRequest { requestId: string, parseTreeId: string, location } =>
                let (* Properties of selected node. *)
                    (* Get the current parse tree and check the ID matches *)
                    val (_, lastParsetree, currentParseID) = getCurrentParse()
                    val (commands, location) =
                        if parseTreeId = currentParseID
                        then
                        let
                            val newTree = navigateTo(location, lastParsetree)
                            (* Update the last tree if it's still valid. *)
                            val () = updateLastParse(currentParseID, newTree)
                            val commands =
                                case newTree of
                                    NONE => []
                                |   (SOME(_, tree)) =>
                                    let
                                        open PolyML
                                        fun printCode(PTparent _, rest) = "U" :: rest
                                        |   printCode(PTpreviousSibling _, rest) = "P" :: rest
                                        |   printCode(PTnextSibling _, rest) = "N" :: rest
                                        |   printCode(PTfirstChild _, rest) = "C" :: rest
                                        |   printCode(PTtype _, rest) = "T" :: rest
                                        |   printCode(PTdeclaredAt _, rest) = "I" :: rest
                                        |   printCode(PTopenedAt _, rest) = "J" :: rest
                                        |   printCode(PTstructureAt _, rest) = "S" :: rest
                                        |   printCode(PTreferences(_, _::_), rest) = "V" :: rest
                                                (* Only include references if there is at least one
                                                   local reference. *)
                                        |   printCode(PTreferences(_, []), rest) = rest
                                        |   printCode(PTprint _, rest) = rest
                                    in
                                        List.foldl printCode [] tree
                                    end
                        in
                            (commands, treeLocation newTree)
                        end
                        else ([], { startOffset = 0, endOffset = 0 }) (* Wrong ID. *)
                in
                    sendResponse(
                        PropertyResponse {
                            requestId = requestId, parseTreeId = currentParseID,
                            location = location, commands = commands
                        });
                    runProtocol currentCompilation
                end

            |   MoveRequest { requestId, parseTreeId, location, direction } =>
                let  (* Get location after a move relative to a selected node. *)
                    val (_, lastParsetree, currentParseID) = getCurrentParse()
                    val newLocation =
                        if parseTreeId = currentParseID
                        then
                        let
                            (* Move to the given location, then move in the required direction. *)
                            val newTree = 
                                case navigateTo(location, lastParsetree) of
                                    NONE => NONE
                                |   SOME(location, tree) =>
                                    let
                                        open PolyML
                                        fun find([], _) = (location, tree) (* No change *)
                                        |   find(PTparent p :: _, "U" (* Up *)) = p()
                                        |   find(PTpreviousSibling p :: _, "P" (*Left*)) = p()
                                        |   find(PTnextSibling p :: _, "N" (*Right*)) = p()
                                        |   find(PTfirstChild p :: _, "C" (* Down *)) = p()
                                        |   find(_ :: tl, dir) = find (tl, dir)
                
                                    in
                                        SOME(find(tree, direction))
                                    end
                            (* Update the last tree if it's still valid. *)
                            val () = updateLastParse(currentParseID, newTree)
                        in
                            treeLocation newTree (* Return the location of the updated tree. *)
                        end
                        else { startOffset = 0, endOffset = 0 } (* *)
                in
                    sendResponse(
                        MoveResponse {
                            requestId = requestId, parseTreeId = currentParseID, location = newLocation
                        });
                    runProtocol currentCompilation
                end

            |   TypeRequest { requestId, parseTreeId, location } =>
                let (* Type of value at selected node. *)
                    val (_, lastParsetree, currentParseID) = getCurrentParse()
                    val (typeRes, location) =
                        if parseTreeId = currentParseID
                        then
                        let
                            (* Move to the required location. *)
                            val newTree = navigateTo(location, lastParsetree)
                            val () = updateLastParse(currentParseID, newTree)
                            (* If it has a type return it. *)
                            val typeRes =
                                case newTree of
                                    NONE => NONE
                                |   (SOME(_, tree)) =>
                                    (
                                        (* Print the type if it's there.  Don't include any mark-up. *)
                                        (* TODO: This uses the global name space to find types and structures.
                                           It really should use the local name space but that requires adding
                                           an environment to the parse tree. *)
                                        case List.find (fn (PolyML.PTtype _) => true | _ => false) tree of
                                            SOME(PolyML.PTtype t) =>
                                                SOME(prettyAsString(
                                                    PolyML.NameSpace.displayTypeExpression(t, 100, PolyML.globalNameSpace)))
                                        |   _ => NONE
                                    )
                        in
                           (typeRes, treeLocation newTree)
                        end
                        else (NONE, { startOffset = 0, endOffset = 0 })
                in
                    sendResponse(
                        TypeResponse {
                            requestId = requestId, parseTreeId = currentParseID,
                            location = location, typeRes = typeRes
                        });
                    runProtocol currentCompilation
                end

            |   DecRequest { requestId, parseTreeId, location, decType } =>
                let (* Information about declaration location of identifier at selected node. *)
                    val (_, lastParsetree, currentParseID) = getCurrentParse()
                    val (decLocation, location) =
                        if parseTreeId = currentParseID
                        then
                        let
                            (* Move to the required location. *)
                            val newTree = navigateTo(location, lastParsetree)
                            val () = updateLastParse(currentParseID, newTree)
                            val decLocation =
                                (* If it has the right kind of property return it. *)
                                case newTree of
                                    NONE => NONE
                                |   (SOME(_, tree)) =>
                                    let
                                        open PolyML
                                        val getLoc =
                                            case decType of
                                                "I" => (fn (PTdeclaredAt p) => SOME p | _ => NONE)
                                            |   "J" => (fn (PTopenedAt p) => SOME p | _ => NONE)
                                            |   "S" => (fn (PTstructureAt p) => SOME p | _ => NONE)
                                            |   _   => (fn _ => NONE (* Unknown request type. *))
                                        (* Seatch in the properties of the current node for the property we want. *)
                                        fun findLoc [] = NONE
                                        |   findLoc (hd::tl) =
                                            case getLoc hd of
                                                SOME location => SOME location
                                            |   NONE => (* Keep trying. *) findLoc tl
                                    in
                                        findLoc tree
                                    end
                        in
                            (decLocation, treeLocation newTree)
                        end
                        else (NONE, { startOffset = 0, endOffset = 0 })
                in
                    sendResponse(
                        DecResponse {
                            requestId = requestId, parseTreeId = currentParseID,
                            location = location, decLocation = decLocation
                        });
                    runProtocol currentCompilation
                end

            |   RefRequest { requestId, parseTreeId, location } =>
                let (* Type of value at selected node. *)
                    val (_, lastParsetree, currentParseID) = getCurrentParse()
                    val (references, location) =
                        if parseTreeId = currentParseID
                        then
                        let
                            (* Move to the required location. *)
                            val newTree = navigateTo(location, lastParsetree)
                            val () = updateLastParse(currentParseID, newTree)
                            (* Find the local references. *)
                            val references =
                                case newTree of
                                    NONE => []
                                |   SOME(_, tree) =>
                                    (
                                        case List.find (fn (PolyML.PTreferences _) => true | _ => false) tree of
                                            SOME(PolyML.PTreferences(_, l)) =>
                                                List.map (fn {startPosition, endPosition, ...} =>
                                                            { startOffset=startPosition, endOffset=endPosition}) l
                                        |   _ => []
                                    )
                        in
                           (references, treeLocation newTree)
                        end
                        else ([], { startOffset = 0, endOffset = 0 })
                in
                    sendResponse(
                        RefResponse {
                            requestId = requestId, parseTreeId = currentParseID,
                            location = location, references = references
                        });
                    runProtocol currentCompilation
                end

            |   CompileRequest { requestId, fileName, startPosition, preludeCode, sourceCode } =>
                (* Unlike the other requests this is done asynchronously. *)
                let
                    fun compileThread () =
                    let
                        type errorMsg =
                            { message: PolyML.pretty, hard: bool, location: PolyML.location,
                              context: PolyML.pretty option }
                        (* Even success may include warning messages. *)
                        datatype compileResult =
                            Success
                        |   Exception of exn
                        |   Interrupted
                        |   Errors

                        local
                            open PolyML.NameSpace
                            (* Put in the results without printing. *)
                            fun resultFun
                                { fixes: (string * fixityVal) list, values: (string * valueVal) list,
                                  structures: (string * structureVal) list, signatures: (string * signatureVal) list,
                                  functors: (string * functorVal) list, types: (string * typeVal) list} =
                            let
                                open PolyML
                            in
                                List.app (#enterFix globalNameSpace) fixes;
                                List.app (#enterType globalNameSpace) types;
                                List.app (#enterSig globalNameSpace) signatures;
                                List.app (#enterStruct globalNameSpace) structures;
                                List.app (#enterFunct globalNameSpace) functors;
                                List.app (#enterVal globalNameSpace) values
                            end
                        in
                            (* Compile the prelude.  Simply returns true if it succeeded and false on any error.
                               Note: Unlike the main compilation this is run with the interlock held and
                               interrupts deferred. *)
                            fun compilePreludeString stringInput: string option =
                            let
                                val stringStream = TextIO.openString stringInput

                                fun compilerResultFun (_, codeOpt) =
                                    case codeOpt of
                                        SOME code => (fn () => resultFun(code()))
                                     |  NONE => raise Fail "Static Errors"

                                fun compilerLoop () =
                                (* Compile each "program" until either we get to the end or an exception. *)
                                if TextIO.endOfStream stringStream
                                then NONE (* Reached the end of the input without error. *)
                                else 
                                let
                                    (* Compile the code and get the result. *)
                                    open PolyML PolyML.Compiler
                                    val (code, result) =
                                        (PolyML.compiler(fn () => TextIO.input1 stringStream,
                                            [CPOutStream TextIO.print, CPCompilerResultFun compilerResultFun]),
                                         NONE)
                                         handle exn => (fn() => (), SOME(exnMessage exn))
                                 in
                                    case result of
                                        NONE =>
                                        (
                                            (* No exception in compiler: run the code and check that it
                                               runs successfully. *)
                                            case ((code(); NONE) handle exn => SOME(exnMessage exn)) of
                                                NONE => compilerLoop () (* Continue. *)
                                            |   exn => exn
                                        )
                                    |   error => error
                                end
                                
                                fun runloop () =
                                let
                                    val res = compilerLoop()
                                in
                                    (* The prelude may update the current parse tree. *)
                                    case !parseTree of
                                        (_, []) => lastParsetree := NONE
                                    |   (_, hd :: _) => lastParsetree := SOME hd;
                                    res
                                end
                            in
                                (* This is run with the lock held. *)
                                withLock runloop
                            end

                            (* Compile the main source code. *)
                            fun compileString(stringInput, startPosition) =
                            let
                                val errorList = ref []
                                val stringPosition = ref 0
                                val stringSize = String.size stringInput
                                val resultTrees : PolyML.parseTree list ref = ref []
                                val lastTreePosition = ref 0
                                fun readIn () =
                                let
                                    val posn = ! stringPosition
                                in
                                    if posn >= stringSize
                                    then NONE
                                    else SOME(String.sub(stringInput, posn)) before (stringPosition := posn+1)
                                end
                                (* We need to define our own compilerResultFun in order to capture the parse trees. *)
                                fun compilerResultFun (parsetree, codeOpt) =
                                (
                                    (* Add the parsetree to the list.  Record this as the position of the last valid tree. *)
                                    case parsetree of
                                        SOME pt =>
                                            (resultTrees := ! resultTrees @ [pt]; lastTreePosition := !stringPosition)
                                    |   NONE => (); (* Not if parse failed. *)
                                    case codeOpt of
                                        SOME code => (fn () => resultFun(code()))
                                     |  NONE => raise Fail "Static Errors"
                                )

                                fun compilerLoop () =
                                (* Compile each "program" until either we get to the end or an exception. *)
                                if ! stringPosition >= stringSize
                                then Success (* Reached the end of the input without error. *)
                                else
                                let
                                    open PolyML PolyML.Compiler
                                    val (code, result) =
                                        (PolyML.compiler(readIn,
                                            [CPOutStream TextIO.print, CPLineOffset (fn () => startPosition + !stringPosition),
                                             CPErrorMessageProc (fn msg => errorList := !errorList @ [msg]),
                                             CPCompilerResultFun compilerResultFun, CPFileName fileName,
                                             CPRootTree (toplevelParseTree resultTrees)]),
                                         Success)
                                         handle Fail _ => (fn() => (), Errors)
                                         |  _ (* E.g. Interrupted *) => (fn() => (), Interrupted)
                                in
                                    case result of
                                        Success => (* Compilation succeeded. *)
                                        (
                                            (* Run the code.  If it raised an exception pass that back. *)
                                            case (code(); Success) handle exn => Exception exn of
                                                Success => compilerLoop () (* Continue. *)
                                            |   fault => fault
                                        )
                                    |   error => error
                                end
                            in
                                (compilerLoop (), startPosition + !lastTreePosition,
                                 ! resultTrees, ! errorList)
                            end
                        end
                    in
                        if
                        (* First run the prelude.  If there are any errors report them and stop. *)
                            case compilePreludeString preludeCode of
                                NONE => true (* Succeeded - continue *)
                            |   SOME preludeError =>  (* Error - stop *)
                                let
                                    (* Leave the parse tree unchanged. *)
                                    val (_, _, currentId) = getCurrentParse()
                                in
                                    sendResponse(
                                        CompilerResponse {
                                            requestId = requestId, parseTreeId = currentId,
                                            finalOffset = startPosition, result = PreludeFail preludeError
                                        });
                                    false
                                end
                        then (* We can do the main compilation. *)
                        let
                            local
                                open Thread.Thread
                            in
                                (* The rest of this code is interruptible
                                   TODO: Multiple interrupts could result in not sending a
                                   result packet. *)
                                val () =
                                    setAttributes [EnableBroadcastInterrupt true, InterruptState InterruptAsynch]
                            end;
                            val (result, finalPosition, resultTrees, errors) =
                                compileString(sourceCode, startPosition)
                            fun makeErrorPacket
                                {message: PolyML.pretty, hard: bool, location, ...} =
                                {
                                    hardError = hard,
                                    location = location,
                                    message = prettyMarkupAsString message
                                }
                            val errorPackets = List.map makeErrorPacket errors
                            val compileResult  =
                                case result of
                                    Success => Succeeded errorPackets (* May be warning messages. *)
                                |   Exception exn =>
                                    let
                                        open PolyML
                                        val exLoc =
                                            case exceptionLocation exn of
                                                SOME loc => [ContextLocation loc]
                                            |   NONE => []
                                        val exceptionString =
                                            prettyMarkupAsString
                                                (PrettyBlock(0, false, exLoc,
                                                    [ prettyRepresentation(exn, !PolyML.Compiler.printDepth) ]))
                                    in
                                        RuntimeException(exceptionString, errorPackets)
                                    end
                                |   Interrupted => CompileCancelled errorPackets
                                |   Errors => CompileFail errorPackets
                            (* Update the tree unless parsing failed and we don't have one. *)
                            val parseTreeId =
                                case resultTrees of
                                    [] => #3 (getCurrentParse()) (* Return existing tree. *)
                                |   _ => (setParseTree(resultTrees, requestId); requestId)
                        in
                            (* Send the response. *)
                            sendResponse(
                                CompilerResponse {
                                    requestId = requestId, parseTreeId = parseTreeId,
                                    finalOffset = finalPosition, result = compileResult
                                })
                        end
                        else () (* Prelude failed. *)
                    end (* compileThread *)

                    open Thread.Thread
                in
                    (* First see if the last compilation has terminated. *)
                    case currentCompilation of
                        NONE => ()
                    |   SOME (_, lastCompileThread) =>
                            if isActive lastCompileThread
                            then protocolError "Multiple Compilations"
                            else ();
                    let
                        (* The compile thread is run with interrupts deferred initially. *)
                        val thread = fork(compileThread, [InterruptState InterruptDefer])
                    in
                        runProtocol (SOME(requestId, thread))
                    end
                end

            |   KillRequest { requestId: string } => (* Kill compilation. *)
                (
                    case currentCompilation of
                        NONE => () (* No compilation. *)
                    |   SOME (id, thread) =>
                        if requestId = id
                        then Thread.Thread.interrupt thread
                        else () (* Different ID running. *);
                    runProtocol currentCompilation
                )

            |   UnknownRequest req => (* Respond with an empty response. *)
                (
                    sendResponse(UnknownResponse req);
                    runProtocol currentCompilation
                )
        end
    in
        let
            (* Turn off interrupts for the interface thread. *)
            open Thread.Thread
        in
            setAttributes[EnableBroadcastInterrupt false, InterruptState InterruptDefer]
        end;
        sendStartedMessage(); 
        runProtocol NONE (* No compilation. *)
    end (* runIDEProtocol. *)
in
    structure PolyML =
    struct
        (* This is the root function to run the Poly/ML top level. *)
        fun rootFunction () : unit =
        let
            val argList = CommandLine.arguments();
            fun rtsRelease() = RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (10, ())
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
                print "-v             Print the version of Poly/ML and exit\n";
                print "--help         Print this message and exit\n";
                print "-q             Suppress the start-up message\n";
                print "--with-markup  Include extra mark-up information when printing\n";
                print "--ideprotocol  Run the IDE communications protocol\n";
                print "\nRun time system arguments:\n";
                print (rtsHelp())
               )
           
            else if List.exists(fn s => s = "--ideprotocol") argList
            then runIDEProtocol() (* Run the IDE communication protocol. *)

            else (* Enter normal Poly/ML top-level. *)
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
                PolyML.shell ();
                OS.Process.exit OS.Process.success (* Run any "atExit" functions and then quit. *)
            end
        end;

        structure IDEInterface =
        struct
            val parseTree = parseTree
            val runIDEProtocol = runIDEProtocol
        end;

        open PolyML (* Add this to the PolyML structure. *)
    end
end;
