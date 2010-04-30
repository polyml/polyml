(*
    Title:      Standard Basis Library: Commands to build the library
    Copyright   David C.J. Matthews 2000, 2005

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

(* Thread, Weak and Signal are Poly/ML extensions. *)

Bootstrap.use "basis/RuntimeCalls.ML";
Bootstrap.use "basis/InitialBasis.ML";
Bootstrap.use "basis/InitialPolyML.ML";
Bootstrap.use "basis/Universal.ML";
Bootstrap.use "basis/General.sml";
Bootstrap.use "basis/LibrarySupport.sml";
Bootstrap.use "basis/Option.sml";
Bootstrap.use "basis/VectorOperations.sml";
Bootstrap.use "basis/PolyVectorOperations.sml";
Bootstrap.use "basis/VectorSliceOperations.sml";
Bootstrap.use "basis/StringCvt.sml";
Bootstrap.use "basis/MONO_VECTOR.sml";
Bootstrap.use "basis/MONO_VECTOR_SLICE.sml";
Bootstrap.use "basis/MONO_ARRAY.sml";
Bootstrap.use "basis/MONO_ARRAY_SLICE.sml";
Bootstrap.use "basis/Vector.sml";
Bootstrap.use "basis/Array.sml";
Bootstrap.use "basis/List.sml";
Bootstrap.use "basis/String.sml";
Bootstrap.use "basis/Text.sml"; (* Declares Char, String, CharArray, CharVector *)
Bootstrap.use "basis/Bool.sml";
Bootstrap.use "basis/ListPair.sml";
Bootstrap.use "basis/Int.sml";
Bootstrap.use "basis/LargeWord.sml";
Bootstrap.use "basis/Word8.sml";
if PolyML.architecture() = "X86_64"
then Bootstrap.use "basis/Word32.x86_64.sml"
else Bootstrap.use "basis/Word32.sml";
Bootstrap.use "basis/INTEGER.sml";
Bootstrap.use "basis/IntInf.sml";
Bootstrap.use "basis/Int32.sml";
Bootstrap.use "basis/Word8Array.sml";
Bootstrap.use "basis/Byte.sml";
Bootstrap.use "basis/BoolArray.sml";
Bootstrap.use "basis/IntArray.sml";
Bootstrap.use "basis/RealArray.sml";
Bootstrap.use "basis/IEEE_REAL.sml";
Bootstrap.use "basis/IEEEReal.sml";
Bootstrap.use "basis/MATH.sml";
Bootstrap.use "basis/Real.sml";
Bootstrap.use "basis/Time.sml";
Bootstrap.use "basis/Date.sml";
Bootstrap.use "basis/Thread.sml"; (* Non-standard. *)
Bootstrap.use "basis/Timer.sml";
Bootstrap.use "basis/CommandLine.sml";
Bootstrap.use "basis/OS.sml";
Bootstrap.use "basis/ExnPrinter.sml"; (* Relies on OS. *)
Bootstrap.use "basis/IO.sml";
Bootstrap.use "basis/PRIM_IO.sml";
Bootstrap.use "basis/PrimIO.sml";
Bootstrap.use "basis/TextPrimIO.sml";
Bootstrap.use "basis/BinPrimIO.sml";
Bootstrap.use "basis/LibraryIOSupport.sml";
Bootstrap.use "basis/STREAM_IO.sml";
Bootstrap.use "basis/BasicStreamIO.sml";
Bootstrap.use "basis/IMPERATIVE_IO.sml";
Bootstrap.use "basis/ImperativeIO.sml";
Bootstrap.use "basis/TextIO.sml";
Bootstrap.use "basis/BinIO.sml";
Bootstrap.use "basis/NetHostDB.sml";
Bootstrap.use "basis/NetProtDB.sml";
Bootstrap.use "basis/NetServDB.sml";
Bootstrap.use "basis/Socket.sml";
Bootstrap.use "basis/GenericSock.sml";
Bootstrap.use "basis/INetSock.sml";
Bootstrap.use "basis/UnixSock.sml";
Bootstrap.use "basis/PackRealBig.sml"; (* also declares PackRealLittle *)
Bootstrap.use "basis/PackWord8Big.sml"; (* also declares Pack8Little. ...*)
Bootstrap.use "basis/Array2.sml";
Bootstrap.use "basis/IntArray2.sml";
Bootstrap.use "basis/SML90.sml";
Bootstrap.use "basis/Weak.sml";
Bootstrap.use "basis/Signal.sml";
Bootstrap.use "basis/SysWord.sml";
Bootstrap.use "basis/BIT_FLAGS.sml";
Bootstrap.use "basis/SingleAssignment.sml";

(* Build Windows or Unix structure as appropriate. *)
let
    val getOS: int =
        RunCall.run_call2 RuntimeCalls.POLY_SYS_os_specific (0, 0)
in
    if getOS = 0 then ( Bootstrap.use "basis/Posix.sml"; Bootstrap.use "basis/Unix.sml")
    else if getOS = 1 then Bootstrap.use "basis/Windows.sml"
    else ()
end;

(* Build the Process structure for backwards compatibility. *)
Bootstrap.use "basis/processes.ML";

Bootstrap.use "basis/HashArray.ML";
Bootstrap.use "basis/UniversalArray.ML";
Bootstrap.use "basis/PrettyPrinter.sml"; (* Add PrettyPrinter to PolyML structure. *)
Bootstrap.use "basis/FinalPolyML.sml";
Bootstrap.use "basis/TopLevelPolyML.sml"; (* Add rootFunction to Poly/ML. *)

val use = PolyML.use;

(* Copy everything out of the original name space. *)
(* Do this AFTER we've finished compiling PolyML and after adding "use". *)
val () = List.app (#enterVal PolyML.globalNameSpace) (#allVal Bootstrap.globalSpace ())
and () = List.app (#enterFix PolyML.globalNameSpace) (#allFix Bootstrap.globalSpace ())
and () = List.app (#enterSig PolyML.globalNameSpace) (#allSig Bootstrap.globalSpace ())
and () = List.app (#enterType PolyML.globalNameSpace) (#allType Bootstrap.globalSpace ())
and () = List.app (#enterFunct PolyML.globalNameSpace) (#allFunct Bootstrap.globalSpace ())
and () = List.app (#enterStruct PolyML.globalNameSpace) (#allStruct Bootstrap.globalSpace ())

(* We don't want Bootstrap copied over. *)
val () = PolyML.Compiler.forgetStructure "Bootstrap";

(* Clean out structures and functors which are only used to build
   the library. *)
PolyML.Compiler.forgetStructure "LibrarySupport";
PolyML.Compiler.forgetStructure "LibraryIOSupport";
PolyML.Compiler.forgetStructure "MachineConstants";
PolyML.Compiler.forgetFunctor "BasicStreamIO";
PolyML.Compiler.forgetFunctor "VectorOperations";
PolyML.Compiler.forgetFunctor "PolyVectorOperations";
PolyML.Compiler.forgetFunctor "VectorSliceOperations";

(* Now we've created the new name space we must use PolyML.make/use. N.B. Unlike Bootstrap.use
   these don't automatically look at the -I option. *)

