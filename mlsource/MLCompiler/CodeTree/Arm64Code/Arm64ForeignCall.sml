(*
    Copyright (c) 2021 David C. J. Matthews

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    Licence version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public Licence for more details.
    
    You should have received a copy of the GNU Lesser General Public
    Licence along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

functor Arm64ForeignCall(
    structure FallBackCG: GENCODESIG
    and       CodeArray: CODEARRAYSIG
    and       Arm64Assembly: Arm64Assembly
    and       Debug: DEBUG

    sharing FallBackCG.Sharing = CodeArray.Sharing = Arm64Assembly.Sharing
): FOREIGNCALLSIG
=
struct

    open CodeArray Arm64Assembly

    val rtsCallFast = FallBackCG.Foreign.rtsCallFast
    val rtsCallFastRealtoReal = FallBackCG.Foreign.rtsCallFastRealtoReal
    val rtsCallFastRealRealtoReal = FallBackCG.Foreign.rtsCallFastRealRealtoReal
    val rtsCallFastGeneraltoReal = FallBackCG.Foreign.rtsCallFastGeneraltoReal
    val rtsCallFastRealGeneraltoReal = FallBackCG.Foreign.rtsCallFastRealGeneraltoReal
    val rtsCallFastFloattoFloat = FallBackCG.Foreign.rtsCallFastFloattoFloat
    val rtsCallFastFloatFloattoFloat = FallBackCG.Foreign.rtsCallFastFloatFloattoFloat
    val rtsCallFastGeneraltoFloat = FallBackCG.Foreign.rtsCallFastGeneraltoFloat
    val rtsCallFastFloatGeneraltoFloat = FallBackCG.Foreign.rtsCallFastFloatGeneraltoFloat
    
    (* There is only one ABI value. *)
    datatype abi = ARM64Abi
    fun abiList () = [("default", ARM64Abi)]

    (* This must match the type in Foreign.LowLevel.  Once this is bootstrapped we could use that
       type but note that this is the type we use within the compiler and we build Foreign.LowLevel
       AFTER compiling this. *)
    datatype cTypeForm =
        CTypeFloatingPt | CTypePointer | CTypeSignedInt | CTypeUnsignedInt
    |   CTypeStruct of cType list | CTypeVoid
    withtype cType = { typeForm: cTypeForm, align: word, size: word }

    fun foreignCall(_: abi, args: cType list, result: cType): Address.machineWord =
    let
        (* TODO: Just raise an exception for now. *)
        val exceptionPacket = Foreign.Foreign "TODO: ARM64 FFI call"
        val instructions = [
            loadAddressConstant(X0, Address.toMachineWord exceptionPacket),
            loadRegScaled{regT=X_MLStackPtr, regN=X_MLAssemblyInt, unitOffset=exceptionHandlerOffset},
            loadRegScaled{regT=X1, regN=X_MLStackPtr, unitOffset=0},
            genBranchRegister X1
        ]

        val functionName = "foreignCall"
        val debugSwitches =
            [(*Universal.tagInject Pretty.compilerOutputTag (Pretty.prettyPrint(print, 70)),
               Universal.tagInject DEBUG.assemblyCodeTag true*)]
        val closure = makeConstantClosure()
        val () = generateCode{instrs=instructions, name=functionName, parameters=debugSwitches, resultClosure=closure}
    in
        closureAsAddress closure
    end

    (* Build a callback function.  The arguments are the abi, the list of argument types and the result type.
       The result is an ML function that takes an ML function, f, as its argument, registers it as a callback and
       returns the C function as its result.  When the C function is called the arguments are copied into
       temporary memory and the vector passed to f along with the address of the memory for the result.
       "f" stores the result in it when it returns and the result is then passed back as the result of the
       callback.
       N.B.  This returns a closure cell which contains the address of the code.  It can be used as a
       SysWord.word value except that while it exists the code will not be GCed.  *)
    fun buildCallBack(_: abi, args: cType list, result: cType): Address.machineWord =
    let
        val _ = raise Foreign.Foreign "TODO: ARM64 FFI callback"
        val instructions =
            []

        val functionName = "foreignCallBack(2)"
        val debugSwitches =
            [(*Universal.tagInject Pretty.compilerOutputTag (Pretty.prettyPrint(print, 70)),
               Universal.tagInject DEBUG.assemblyCodeTag true*)]

        val closure = makeConstantClosure()
        val () = generateCode{instrs=instructions, name=functionName, parameters=debugSwitches, resultClosure=closure}
        val stage2Code = closureAsAddress closure

        fun resultFunction f =
        let
            (* Generate a small function to load the address of f into a register and then jump to stage2.
               The idea is that it should be possible to generate this eventually in a single RTS call.
               That could be done by using a version of this as a model. *)
            val instructions =
                []
            val functionName = "foreignCallBack(1)"
            val debugSwitches =
                [(*Universal.tagInject Pretty.compilerOutputTag (Pretty.prettyPrint(print, 70)),
                   Universal.tagInject DEBUG.assemblyCodeTag true*)]
            val closure = makeConstantClosure()
            val () = generateCode{instrs=instructions, name=functionName, parameters=debugSwitches, resultClosure=closure}
            val res = closureAsAddress closure
            (*val _ = print("Address is " ^ (LargeWord.toString(RunCall.unsafeCast res)) ^ "\n")*)
        in
            res
        end
    in
        Address.toMachineWord resultFunction
    end

end;
