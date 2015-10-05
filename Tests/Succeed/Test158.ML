(* Various tests for the OS.Path structure.  The Unix tests are taken largely
   from the documentation.  The Windows tests are inferred from the documentation
   which isn't always clear. *)

datatype sys = Windows | Posix;

open OS.Path;

fun check s true = () | check s false = raise Fail ("check " ^ s);

val check' = check "";

val sys = if isRoot "/" then Posix else Windows;

(* Windows uses either / or \ as a separator but the
   output routines generally produce \.  To simplify the
   checks map backslashes into forward slashes in some cases. *)
val mapSep =
    case sys of
        Posix => (fn c => c)
    |   Windows => String.map (fn #"\\" => #"/" | c => c);

(* fromString *)
val testFromTo =
[
    {
        s = "",
        p = {isAbs=false, vol="", arcs=[]},
        w = {isAbs=false, vol="", arcs=[]}
    },
    {
        s = "/",
        p = {isAbs=true, vol="", arcs=[""]},
        (* In Windows this means the "root" directory on the current drive. *)
        w = {isAbs = false, vol = "", arcs = ["", ""]}
    },
    {
        s = "//",
        p = {isAbs=true, vol="", arcs=["", ""]},
        w = {isAbs = false, vol = "", arcs = ["", "", ""]}
    },
    {
        s = "a",
        p = {isAbs=false, vol="", arcs=["a"]},
        w = {isAbs=false, vol="", arcs=["a"]}
    },
    {
        s = "/a",
        p = {isAbs=true, vol="", arcs=["a"]},
        w = {isAbs=false, vol="", arcs=["", "a"]}
    },
    {
        s = "//a",
        p = {isAbs=true, vol="", arcs=["","a"]},
        w = {isAbs=false, vol="", arcs=["","", "a"]}
    },
    {
        s = "a/",
        p = {isAbs=false, vol="", arcs=["a", ""]},
        w = {isAbs=false, vol="", arcs=["a", ""]}
    },
    {
        s = "a//",
        p = {isAbs=false, vol="", arcs=["a", "", ""]},
        w = {isAbs=false, vol="", arcs=["a", "", ""]}
    },
    {
        s = "a/b",
        p = {isAbs=false, vol="", arcs=["a", "b"]},
        w = {isAbs=false, vol="", arcs=["a", "b"]}
    },
    {
        s = "c:\\",
        p = {isAbs=false, vol="", arcs=["c:\\"]},
        w = {isAbs = true, vol = "c:", arcs = [""]}
    },
    {
        s = "c:",
        p = {isAbs=false, vol="", arcs=["c:"]},
        w = {isAbs=false, vol = "c:", arcs = [""]}
    },
    {
        s = "c:\\abc",
        p = {isAbs=false, vol="", arcs=["c:\\abc"]},
        w = {isAbs=true, vol = "c:", arcs = ["abc"]}
    }


];

fun test { s, p, w } =
let
    val res = fromString s
    val ts = toString res
in
    case sys of
        Posix => check s (res = p)
    |   Windows => check s (res = w);
    check s (ts = s orelse mapSep ts = s)
end;

val () = List.app test testFromTo;

(* validVolume *)
val v1 = validVolume{isAbs = true, vol = ""};
case sys of Posix => check' v1 | Windows => check' (not v1);

val v2 = validVolume{isAbs = false, vol = ""};
check' v2;

val v3 = validVolume{isAbs = true, vol = "C:"};
case sys of Posix => check'(not v3) | Windows => check' v3;

val v4 = validVolume{isAbs = false, vol = "C:"};
case sys of Posix => check'(not v4) | Windows => check' v4;

val v5 = validVolume{isAbs = false, vol = "\\\\server\\share" };
case sys of Posix => check'(not v5) | Windows => check' v5;


(* getParent *)
val testGetParent =
[
    { s = "/", p = "/", w = "/" },
    { s = "a", p = ".", w = "." },
    { s = "a/", p = "a/..", w = "a/.." },
    { s = "a///", p = "a///..", w = "a///.." },
    { s = "a/b", p = "a", w = "a" },
    { s = "a/b/", p = "a/b/..", w = "a/b/.." },
    { s = "..", p = "../..", w = "../.." },
    { s = ".", p = "..", w = ".." },
    { s = "C:\\", p = ".", w = "C:\\" },
    { s = "\\\\server\\share\\", p = ".", w = "\\\\server\\share\\" }
];

fun test { s, p, w } =
let
    val res = getParent s
in
    case sys of
        Posix => check s (res = p)
    |   Windows => check s (res = w orelse mapSep res = mapSep w)
end;

val () = List.app test testGetParent;


(* splitDirFile *)
val testSplitFile =
[
    {
        s = "",
        p = {dir = "", file = ""},
        w = {dir = "", file = ""}
    },
    {
        s = ".",
        p = {dir = "", file = "."},
        w = {dir = "", file = "."}
    },
    {
        s = "b",
        p = {dir = "", file = "b"},
        w = {dir = "", file = "b"}
    },
    {
        s = "b/",
        p = {dir = "b", file = ""},
        w = {dir = "b", file = ""}
    },
    {
        s = "a/b",
        p = {dir = "a", file = "b"},
        w = {dir = "a", file = "b"}
    },
    {
        s = "/a",
        p = {dir = "/", file = "a"},
        w = {dir = "/", file = "a"}
    },
    {
        s = "c:\\a",
        p = {dir = "", file = "c:\\a"},
        w = {dir = "c:\\", file = "a"}
    }

];

fun test { s, p, w } =
let
    val res as { dir, file } = splitDirFile s
in
    case sys of
        Posix => check s (res = p)
    |   Windows => check s (res = w orelse mapSep dir = #dir w andalso mapSep file = #file w)
end;

val () = List.app test testSplitFile;


(* OS.Path.joinDirFile should raise InvalidArc if the file name does not correspond to an arc *)
(OS.Path.joinDirFile {dir="abc/def", file="ghi/jkl"}; raise Fail "incorrect") handle OS.Path.InvalidArc => ();


