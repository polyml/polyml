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

val () = Bootstrap.use "basis/RuntimeCalls.ML";
val () = Bootstrap.use "basis/InitialBasis.ML";
val () = Bootstrap.use "basis/InitialPolyML.ML";
val () = Bootstrap.use "basis/Universal.ML";
val () = Bootstrap.use "basis/General.sml";
val () = Bootstrap.use "basis/LibrarySupport.sml";
val () = Bootstrap.use "basis/Option.sml";
val () = Bootstrap.use "basis/VectorOperations.sml";
val () = Bootstrap.use "basis/PolyVectorOperations.sml";
val () = Bootstrap.use "basis/VectorSliceOperations.sml";
val () = Bootstrap.use "basis/StringCvt.sml";
val () = Bootstrap.use "basis/MONO_VECTOR.sml";
val () = Bootstrap.use "basis/MONO_VECTOR_SLICE.sml";
val () = Bootstrap.use "basis/MONO_ARRAY.sml";
val () = Bootstrap.use "basis/MONO_ARRAY_SLICE.sml";
val () = Bootstrap.use "basis/Vector.sml";
val () = Bootstrap.use "basis/Array.sml";
val () = Bootstrap.use "basis/List.sml";
val () = Bootstrap.use "basis/String.sml";
val () = Bootstrap.use "basis/Text.sml"; (* Declares Char, String, CharArray, CharVector *)
val () = Bootstrap.use "basis/Bool.sml";
val () = Bootstrap.use "basis/ListPair.sml";
val () = Bootstrap.use "basis/Int.sml";
val () = Bootstrap.use "basis/LargeWord.sml";
val () = Bootstrap.use "basis/Word8.sml";
val () =
if PolyML.architecture() = "X86_64"
then Bootstrap.use "basis/Word32.x86_64.sml"
else Bootstrap.use "basis/Word32.sml";
val () = Bootstrap.use "basis/INTEGER.sml";
val () = Bootstrap.use "basis/IntInf.sml";
val () = Bootstrap.use "basis/Int32.sml";
val () = Bootstrap.use "basis/Word8Array.sml";
val () = Bootstrap.use "basis/Byte.sml";
val () = Bootstrap.use "basis/BoolArray.sml";
val () = Bootstrap.use "basis/IntArray.sml";
val () = Bootstrap.use "basis/RealArray.sml";
val () = Bootstrap.use "basis/IEEE_REAL.sml";
val () = Bootstrap.use "basis/IEEEReal.sml";
val () = Bootstrap.use "basis/MATH.sml";
val () = Bootstrap.use "basis/Real.sml";
val () = Bootstrap.use "basis/Time.sml";
val () = Bootstrap.use "basis/Date.sml";
val () = Bootstrap.use "basis/Thread.sml"; (* Non-standard. *)
val () = Bootstrap.use "basis/Timer.sml";
val () = Bootstrap.use "basis/CommandLine.sml";
val () = Bootstrap.use "basis/OS.sml";
val () = Bootstrap.use "basis/ExnPrinter.sml"; (* Relies on OS. *)
val () = Bootstrap.use "basis/IO.sml";
val () = Bootstrap.use "basis/PRIM_IO.sml";
val () = Bootstrap.use "basis/PrimIO.sml";
val () = Bootstrap.use "basis/TextPrimIO.sml";
val () = Bootstrap.use "basis/BinPrimIO.sml";
val () = Bootstrap.use "basis/LibraryIOSupport.sml";
val () = Bootstrap.use "basis/STREAM_IO.sml";
val () = Bootstrap.use "basis/BasicStreamIO.sml";
val () = Bootstrap.use "basis/IMPERATIVE_IO.sml";
val () = Bootstrap.use "basis/ImperativeIO.sml";
val () = Bootstrap.use "basis/TextIO.sml";
val () = Bootstrap.use "basis/BinIO.sml";
val () = Bootstrap.use "basis/NetHostDB.sml";
val () = Bootstrap.use "basis/NetProtDB.sml";
val () = Bootstrap.use "basis/NetServDB.sml";
val () = Bootstrap.use "basis/Socket.sml";
val () = Bootstrap.use "basis/GenericSock.sml";
val () = Bootstrap.use "basis/INetSock.sml";
val () = Bootstrap.use "basis/UnixSock.sml";
val () = Bootstrap.use "basis/PackRealBig.sml"; (* also declares PackRealLittle *)
val () = Bootstrap.use "basis/PackWord8Big.sml"; (* also declares Pack8Little. ...*)
val () = Bootstrap.use "basis/Array2.sml";
val () = Bootstrap.use "basis/IntArray2.sml";
val () = Bootstrap.use "basis/SML90.sml";
val () = Bootstrap.use "basis/Weak.sml";
val () = Bootstrap.use "basis/Signal.sml";
val () = Bootstrap.use "basis/SysWord.sml";
val () = Bootstrap.use "basis/BIT_FLAGS.sml";
val () = Bootstrap.use "basis/SingleAssignment.sml";


(* Build Windows or Unix structure as appropriate. *)
local
    val getOS: int =
        RunCall.run_call2 RuntimeCalls.POLY_SYS_os_specific (0, 0)
in
    val () =
    if getOS = 0 then ( Bootstrap.use "basis/Posix.sml"; Bootstrap.use "basis/Unix.sml")
    else if getOS = 1 then Bootstrap.use "basis/Windows.sml"
    else ()
end;

(* Build the Process structure for backwards compatibility. *)
val () = Bootstrap.use "basis/processes.ML";

val () = Bootstrap.use "basis/HashArray.ML";
val () = Bootstrap.use "basis/UniversalArray.ML";
val () = Bootstrap.use "basis/PrettyPrinter.sml"; (* Add PrettyPrinter to PolyML structure. *)
val () = Bootstrap.use "basis/FinalPolyML.sml";
val () = Bootstrap.use "basis/TopLevelPolyML.sml"; (* Add rootFunction to Poly/ML. *)

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
PolyML.Compiler.forgetValue "it";
PolyML.Compiler.forgetStructure "LibrarySupport";
PolyML.Compiler.forgetStructure "LibraryIOSupport";
PolyML.Compiler.forgetStructure "MachineConstants";
PolyML.Compiler.forgetFunctor "BasicStreamIO";
PolyML.Compiler.forgetFunctor "VectorOperations";
PolyML.Compiler.forgetFunctor "PolyVectorOperations";
PolyML.Compiler.forgetFunctor "VectorSliceOperations";

(* Now we've created the new name space we must use PolyML.make/use. N.B. Unlike Bootstrap.use
   these don't automatically look at the -I option. *)



