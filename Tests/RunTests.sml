(* Run the regression tests. *)

fun runTests parentDir =
let
    val defaultInlineSize = ! PolyML.Compiler.maxInlineSize

    fun runTests (dirName, expectSuccess) =
    let
        (* Run a file.  Returns true if it succeeds, false if it fails. *)
        fun runTest fileName =
        let
            open PolyML.Compiler
            (* Max inline size is not available as a CP parameter and some tests
               adjust it.  Set it to the default before each test. *)
            val () = maxInlineSize := defaultInlineSize (* Set it to the default *)
            val () = debug := false (* Reset this *)
            (* First in list is the name with no suffix. *)
            val inStream = TextIO.getInstream(TextIO.openIn fileName)
            val stream = ref inStream

            val lineNo   = ref 1;
            fun getChar () : char option =
                case TextIO.StreamIO.input1 (! stream) of
                    NONE => NONE
                |   SOME (eoln as #"\n", strm) =>
                    (
                        lineNo := !lineNo + 1;
                        stream := strm;
                        SOME eoln
                    )
                |   SOME(c, strm) => (stream := strm; SOME c)
            (* Create a private name space for each test otherwise declarations in one
               could affect another. *)
            fun makeSpace(globalLook, globalAll) =
            let
                open HashArray
                val table = hash 10
                infix 8 sub
                fun lookup s =
                    case table sub s of
                        NONE => globalLook s
                    |   SOME r => SOME r
                fun enter(s, v) = update(table, s, v)
                fun all () = fold (fn(s, v, l) => (s, v) :: l) (globalAll()) table
            in
                { lookup = lookup, enter = enter, all = all }
            end
            val { lookupFix, lookupSig, lookupVal, lookupType, lookupFunct, lookupStruct,
                  allFix, allSig, allVal, allType, allFunct, allStruct, ...} = PolyML.globalNameSpace;
            val fixSpace = makeSpace(lookupFix, allFix)
            val sigSpace = makeSpace(lookupSig, allSig)
            val valSpace = makeSpace(lookupVal, allVal)
            val typeSpace = makeSpace(lookupType, allType)
            val funSpace = makeSpace(lookupFunct, allFunct)
            val strSpace = makeSpace(lookupStruct, allStruct)

            val localNameSpace: PolyML.NameSpace.nameSpace =
            {
                lookupFix    = #lookup fixSpace,
                lookupSig    = #lookup sigSpace,
                lookupVal    = #lookup valSpace,
                lookupType   = #lookup typeSpace,
                lookupFunct  = #lookup funSpace,
                lookupStruct = #lookup strSpace,
                enterFix     = #enter fixSpace,
                enterSig     = #enter sigSpace,
                enterVal     = #enter valSpace,
                enterType    = #enter typeSpace,
                enterFunct   = #enter funSpace,
                enterStruct  = #enter strSpace,
                allFix       = #all fixSpace,
                allSig       = #all sigSpace,
                allVal       = #all valSpace,
                allType      = #all typeSpace,
                allFunct     = #all funSpace,
                allStruct    = #all strSpace
            }

            (* The tests in the Fail directory should all raise exceptions
               in the compiler as a result of detecting errors. *)
            exception CompilerException
        in
            (
                while not (TextIO.StreamIO.endOfStream(!stream)) do
                let
                    fun discardOut _ = ()
                    val nameSpace = PolyML.globalNameSpace
    
                    val code =
                        PolyML.compiler(getChar, [CPOutStream discardOut, CPNameSpace localNameSpace])
                            handle Fail "Static Errors" => raise CompilerException
                in
                    code()
                end;
                (* Normal termination: close the stream. *)
                TextIO.StreamIO.closeIn (! stream);
                expectSuccess (* OK if we expected success. *)
            ) handle
                CompilerException => (TextIO.StreamIO.closeIn(!stream); not expectSuccess)
                | exn => (TextIO.StreamIO.closeIn(!stream); false)

        end;

        open OS.FileSys OS.Path
        val testPath = joinDirFile{dir=parentDir, file=dirName}
        val dir = openDir testPath
        fun runDir (fails: string list) =
            case readDir dir of
                NONE => fails (* Finished *)
            |   SOME f =>
                if String.isSuffix "ML" f
                then
                (
                    print f; print " => ";
                    if runTest(joinDirFile{dir=testPath, file=f})
                    then (print "Passed\n"; runDir fails)
                    else (print "Failed!!\n"; runDir(fails @ [joinDirFile{dir=dirName, file=f}]))
                )
                else runDir fails
        val failedTests = runDir []
    in
        closeDir dir;
        failedTests
    end;
in
    (* Each test in the Succeed directory should succeed and those in the Fail directory should fail. *)
    case runTests("Succeed", true) @ runTests("Fail", false) of
        [] => true (* All succeeded *)
    |   failedTests => (print "\nFailed Tests: "; print(String.concatWith " " failedTests); print "\n"; false)
end;
