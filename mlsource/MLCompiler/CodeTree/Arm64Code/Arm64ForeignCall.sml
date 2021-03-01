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
    
    exception InternalError = Misc.InternalError

    val rtsCallFastRealtoReal = FallBackCG.Foreign.rtsCallFastRealtoReal
    val rtsCallFastRealRealtoReal = FallBackCG.Foreign.rtsCallFastRealRealtoReal
    val rtsCallFastGeneraltoReal = FallBackCG.Foreign.rtsCallFastGeneraltoReal
    val rtsCallFastRealGeneraltoReal = FallBackCG.Foreign.rtsCallFastRealGeneraltoReal
    val rtsCallFastFloattoFloat = FallBackCG.Foreign.rtsCallFastFloattoFloat
    val rtsCallFastFloatFloattoFloat = FallBackCG.Foreign.rtsCallFastFloatFloattoFloat
    val rtsCallFastGeneraltoFloat = FallBackCG.Foreign.rtsCallFastGeneraltoFloat
    val rtsCallFastFloatGeneraltoFloat = FallBackCG.Foreign.rtsCallFastFloatGeneraltoFloat

    datatype fastArgs = FastArgFixed | FastArgDouble | FastArgFloat

    val makeEntryPoint: string -> machineWord = RunCall.rtsCallFull1 "PolyCreateEntryPointObject"


    fun rtsCallFastGeneral (functionName, argFormats, resultFormat, debugSwitches) =
    let
        val entryPointAddr = makeEntryPoint functionName
        val nArgs = List.length argFormats
        (* The maximum we currently have is five. *)
        val _ = nArgs < 8 orelse raise InternalError "rtsCallFastGeneral: more than 8 args"
        val _ = List.all(fn FastArgFixed => true | _ => false) argFormats orelse
                    raise InternalError "rtsCallFastGeneral: TODO: floating point"
        val _ = resultFormat = FastArgFixed orelse raise InternalError "rtsCallFastGeneral: TODO: floating point"
        val noRtsException = createLabel()
        
        (* Temporarily we need to check for RTS exceptions here.  The interpreter assumes they
           are checked for as part of the RST call. *)
        val instructions =
            [
                (* Move X30 to X24, a callee-save register. *)
                orrShiftedReg{regN=XZero, regM=X_LinkReg, regD=X24, shift=ShiftNone},
                (* Clear the RTS exception before we enter.  "Full" RTS calls clear it anyway
                   but "fast" calls don't. *)
                loadNonAddressConstant(X8, 0w1),
                storeRegScaled{regT=X8, regN=X_MLAssemblyInt, unitOffset=exceptionPacketOffset},
                (* TODO: For floating pt we'll need to load and reorder the args here. *)
                loadAddressConstant(X16, entryPointAddr), (* Load entry point *)
                loadRegScaled{regT=X16, regN=X16, unitOffset=0}, (* Load the actual address. *)
                (* Store the current heap allocation pointer. *)
                storeRegScaled{regT=X_MLHeapAllocPtr, regN=X_MLAssemblyInt, unitOffset=heapAllocPtrOffset},
                (* For the moment save and restore the ML stack pointer.  No RTS call should change
                   it and it's callee-save but just in case... *)
                storeRegScaled{regT=X_MLStackPtr, regN=X_MLAssemblyInt, unitOffset=mlStackPtrOffset},
                branchAndLinkReg X16, (* Call the function. *)
                (* Restore the ML stack pointer. *)
                loadRegScaled{regT=X_MLStackPtr, regN=X_MLAssemblyInt, unitOffset=mlStackPtrOffset},
                (* Load the heap allocation ptr and limit.  We could have GCed in the RTS call. *)
                loadRegScaled{regT=X_MLHeapAllocPtr, regN=X_MLAssemblyInt, unitOffset=heapAllocPtrOffset},
                loadRegScaled{regT=X_MLHeapLimit, regN=X_MLAssemblyInt, unitOffset=heapLimitPtrOffset},
                (* Check for RTS exception. *)
                loadRegScaled{regT=X8, regN=X_MLAssemblyInt, unitOffset=exceptionPacketOffset},
                subSImmediate{regN=X8, regD=XZero, immed=0w1, shifted=false},
                conditionalBranch(condEqual, noRtsException),
                (* If it isn't then raise the exception. *)
                orrShiftedReg{regN=XZero, regM=X8, regD=X0, shift=ShiftNone},
                loadRegScaled{regT=X_MLStackPtr, regN=X_MLAssemblyInt, unitOffset=exceptionHandlerOffset},
                loadRegScaled{regT=X1, regN=X_MLStackPtr, unitOffset=0},
                branchRegister X1,
                setLabel noRtsException,
                (* TODO: For floating point we need to box/tag the result. *)
                (* Return *)
                returnRegister X24
            ]
        val closure = makeConstantClosure()
        val () = generateCode{instrs=instructions, name=functionName, parameters=debugSwitches, resultClosure=closure}
    in
        closureAsAddress closure
    end


    fun rtsCallFast (functionName, nArgs, debugSwitches) =
        rtsCallFastGeneral (functionName, List.tabulate(nArgs, fn _ => FastArgFixed), FastArgFixed, debugSwitches)
(*
    (* RTS call with one double-precision floating point argument and a floating point result. *)
    fun rtsCallFastRealtoReal (functionName, debugSwitches) =
        rtsCallFastGeneral (functionName, [FastArgDouble], FastArgDouble, debugSwitches)
    
    (* RTS call with two double-precision floating point arguments and a floating point result. *)
    fun rtsCallFastRealRealtoReal (functionName, debugSwitches) =
        rtsCallFastGeneral (functionName, [FastArgDouble, FastArgDouble], FastArgDouble, debugSwitches)

    (* RTS call with one double-precision floating point argument, one fixed point argument and a
       floating point result. *)
    fun rtsCallFastRealGeneraltoReal (functionName, debugSwitches) =
        rtsCallFastGeneral (functionName, [FastArgDouble, FastArgFixed], FastArgDouble, debugSwitches)

    (* RTS call with one general (i.e. ML word) argument and a floating point result.
       This is used only to convert arbitrary precision values to floats. *)
    fun rtsCallFastGeneraltoReal (functionName, debugSwitches) =
        rtsCallFastGeneral (functionName, [FastArgFixed], FastArgDouble, debugSwitches)

    (* Operations on Real32.real values. *)

    fun rtsCallFastFloattoFloat (functionName, debugSwitches) =
        rtsCallFastGeneral (functionName, [FastArgFloat], FastArgFloat, debugSwitches)
    
    fun rtsCallFastFloatFloattoFloat (functionName, debugSwitches) =
        rtsCallFastGeneral (functionName, [FastArgFloat, FastArgFloat], FastArgFloat, debugSwitches)

    (* RTS call with one double-precision floating point argument, one fixed point argument and a
       floating point result. *)
    fun rtsCallFastFloatGeneraltoFloat (functionName, debugSwitches) =
        rtsCallFastGeneral (functionName, [FastArgFloat, FastArgFixed], FastArgFloat, debugSwitches)

    (* RTS call with one general (i.e. ML word) argument and a floating point result.
       This is used only to convert arbitrary precision values to floats. *)
    fun rtsCallFastGeneraltoFloat (functionName, debugSwitches) =
        rtsCallFastGeneral (functionName, [FastArgFixed], FastArgFloat, debugSwitches)
*)
    
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
            branchRegister X1
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
