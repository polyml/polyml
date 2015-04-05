(*
    Title:      Modified version of the "use" function which saves state
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

(*
    This is an example version of the "use" function that the IDE may call
    in the prelude before a build.  It takes a directory name and returns a "use"
    function that saves the state and dependencies in ".save" and ".deps" files
    within that directory.  It should be called as e.g.
        val use = ideUse ".polysave"
    to define a version of "use" for the rest of the compilation.
*)

fun ideUse saveDirName =
let
    (* If we are building under the IDE we need to record the dependencies
       and also save the state before each file we "use". *)
    val saveDirectory: string option ref = ref NONE
    val dependencies: string list ref = ref []
    
    open OS.Path
    (* Get the root directory and save directory (typically .polysave).  Do this once
       when this function is called and convert them to absolute paths using the
       current directory when this is called.
       Assume that the root directory is the parent of the save directory.
       N.B. Because the directory may not yet exist we can't use OS.FileSys.fullPath. *)
    val saveDirPath = mkAbsolute { path = saveDirName, relativeTo = OS.FileSys.getDir() }
    (* The root directory is the directory that is assumed to be the root of the project.
       For each source file within this directory with path a/b/c.ML there will be a
       corresponding saved state file .polysave/a/b/c.ML .
       If "use" is called on a file that is not within the root directory no information
       will be saved for that file. *)
    val { dir = rootPath, ...} = splitDirFile saveDirPath

    fun preUse fileName =
         let
            open OS.Path
            (* Create a directory hierarchy. *)
            fun createDirs path =
                if path = "" orelse (OS.FileSys.isDir path handle OS.SysErr _ => false)
                then ()
                else
                (
                    createDirs (OS.Path.dir path);
                    OS.FileSys.mkDir path
                );
            (* Compute the full path to the actual file taking account of any
               change of directory then make it relative to the root. *)
            val filePathRelativeToRoot =
                let
                    val fullFileName = OS.FileSys.fullPath fileName
                    val pathFromRoot = mkRelative { path = fullFileName, relativeTo = rootPath }
                    (* Is the file in the root directory or a sub-directory or is it in
                       some other directory? *)
                    val { arcs, ...} = fromString pathFromRoot
                in
                    case arcs of
                        topArc :: _ =>
                            (* If the first part of the path is ".." then it's in some other directory. *)
                            if topArc = parentArc then NONE else SOME pathFromRoot
                    |   _ => NONE (* No path at all? *)
                end handle Path => NONE (* Different volumes: can't make relative path. *)
                          | OS.SysErr _ => NONE (* If fileName doesn't actually exist. *)
        in
            case filePathRelativeToRoot of
                NONE => () (* Do nothing: we can't save it. *)
            |   SOME fileName =>
                let
                    local
                        val baseName = joinDirFile { dir = saveDirPath, file = fileName }
                    in
                        val saveFile =
                            mkCanonical (joinBaseExt{ base = baseName, ext = SOME "save"})
                        val depFile =
                            mkCanonical (joinBaseExt{ base = baseName, ext = SOME "deps"})
                    end
                    (* Reset the save directory before we save so that it isn't set in the saved
                       state.  That means that "use" won't save the state unless it's explicitly
                       asked to. *)
                    val saveSave = ! saveDirectory
               in
                    (* Create any containing directories. *)
                    createDirs(OS.Path.dir saveFile);
                    saveDirectory := NONE;
        
                    (* Save the state. *)
                    PolyML.SaveState.saveChild (saveFile, List.length(PolyML.SaveState.showHierarchy()));
                    (* Restore the ref. *)
                    saveDirectory := saveSave;
        
                    (* Write out the dependencies. *)
                    let
                        open TextIO
                        val f = openOut depFile
                    in
                        List.app(fn s => output(f, s ^ "\n")) (!dependencies);
                        closeOut f
                    end;
                    (* Add this file to the dependency list. *)
                    dependencies := ! dependencies @ [fileName]
                end handle (ex as OS.SysErr args) =>
                    (print (String.concat["Exception SysErr(", PolyML.makestring args, ") raised for ", fileName, "\n"]); raise ex)
        end
in
    fn originalName =>
    let
        (* Find the actual file name by following the suffixes.  This mirrors what "use" will do. *)
        (* use "f" first tries to open "f" but if that fails it tries "f.ML", "f.sml" etc. *)
        fun trySuffixes [] =
            (* Not found - attempt to open the original and pass back the
               exception. *)
            (TextIO.openIn originalName, originalName)
         |  trySuffixes (s::l) =
            (TextIO.openIn (originalName ^ s), originalName ^ s)
                handle IO.Io _ => trySuffixes l
        (* First in list is the name with no suffix. *)
        val (inStream, fileName) = trySuffixes("" :: ! PolyML.suffixes)
    
        val () = preUse fileName
    in
        PolyML.use fileName (* Now call the underlying use to do the work. *)
    end
end;


