(* Issue #278: saveModule assertion failure after loadState with multi-hierarchy heap.
   Creating a 2-layer saved state and then calling saveModule on it
   previously crashed with assertion failure in RunExport. *)
val dir = OS.FileSys.getDir ();
val tmp = OS.Path.joinDirFile {dir = dir, file = "tmp_issue278"};
val _ = OS.FileSys.mkDir tmp handle _ => ();
val layer0 = OS.Path.joinDirFile {dir = tmp, file = "layer0"};
val layer1 = OS.Path.joinDirFile {dir = tmp, file = "layer1"};
val modFile = OS.Path.joinDirFile {dir = tmp, file = "mod"};

(* Create a 2-layer saved state *)
PolyML.SaveState.saveChild(layer0, 0);
PolyML.SaveState.loadState layer0;
PolyML.SaveState.saveChild(layer1, 1);

(* Load the 2-layer state and try saveModule - this used to crash *)
PolyML.SaveState.loadState layer1;
PolyML.SaveState.saveModule(modFile, {structs = ["List"], functors = [], sigs = [], onStartup = NONE});

(* Clean up *)
OS.FileSys.remove layer0 handle _ => ();
OS.FileSys.remove layer1 handle _ => ();
OS.FileSys.remove modFile handle _ => ();
OS.FileSys.rmDir tmp handle _ => ();
