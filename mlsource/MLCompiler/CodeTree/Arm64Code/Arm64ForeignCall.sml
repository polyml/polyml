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
    structure CodeArray: CODEARRAYSIG
    and       Arm64Assembly: Arm64Assembly
    and       Debug: DEBUG
    and       Arm64Sequences: Arm64Sequences

    sharing CodeArray.Sharing = Arm64Assembly.Sharing = Arm64Sequences.Sharing
): FOREIGNCALLSIG
=
struct

    open CodeArray Arm64Assembly Arm64Sequences
    
    exception InternalError = Misc.InternalError
    and Foreign = Foreign.Foreign

    datatype fastArgs = FastArgFixed | FastArgDouble | FastArgFloat

    val makeEntryPoint: string -> machineWord = RunCall.rtsCallFull1 "PolyCreateEntryPointObject"

    (* Store a double into memory. *)
    fun boxDouble(floatReg, fixedReg, workReg) =
    let
        val label = createLabel()
    in
        [
            (* Subtract the number of bytes required from the heap pointer. *)
            subImmediate{regN=X_MLHeapAllocPtr, regD=fixedReg, immed=0w16, shifted=false},
            (* Compare the result with the heap limit. *)
            subSShiftedReg{regM=X_MLHeapLimit, regN=fixedReg, regD=XZero, shift=ShiftNone},
            conditionalBranch(condCarrySet, label),
            loadRegScaled{regT=X16, regN=X_MLAssemblyInt, unitOffset=heapOverflowCallOffset},
            branchAndLinkReg X16,
            registerMask [], (* Not used at the moment. *)
            setLabel label,
            (* Update the heap pointer. *)
            moveRegToReg{sReg=fixedReg, dReg=X_MLHeapAllocPtr}
        ] @
            loadNonAddress(workReg,
                Word64.orb(0w1, Word64.<<(Word64.fromLarge(Word8.toLarge Address.F_bytes), 0w56)))
        @
        [
            (* Store the length word.  Have to use the unaligned version because offset is -ve. *)
            storeRegUnscaled{regT=workReg, regN=fixedReg, byteOffset= ~8},
            (* Store the floating pt reg. *)
            storeRegScaledDouble{regT=floatReg, regN=fixedReg, unitOffset=0}
        ]
    end

    fun rtsCallFastGeneral (functionName, argFormats, resultFormat, debugSwitches) =
    let
        val entryPointAddr = makeEntryPoint functionName
        (* The maximum we currently have is five so we don't need to worry about stack args. *)

        fun loadArgs([], _, _, _) = []
        |   loadArgs(FastArgFixed :: argTypes, srcReg :: srcRegs, fixed :: fixedRegs, fpRegs) =
                if srcReg = fixed
                then loadArgs(argTypes, srcRegs, fixedRegs, fpRegs) (* Already in the right reg *)
                else moveRegToReg{sReg=srcReg, dReg=fixed} ::
                        loadArgs(argTypes, srcRegs, fixedRegs, fpRegs)
        |   loadArgs(FastArgDouble :: argTypes, srcReg :: srcRegs, fixedRegs, fp :: fpRegs) =
                (* Unbox the value into a fp reg. *)
                loadRegScaledDouble{regT=fp, regN=srcReg, unitOffset=0} ::
                loadArgs(argTypes, srcRegs, fixedRegs, fpRegs)
        |   loadArgs(FastArgFloat :: argTypes, srcReg :: srcRegs, fixedRegs, fp :: fpRegs) =
                (* Untag and move into the fp reg *)
                logicalShiftRight{wordSize=WordSize64, shift=0w32, regN=srcReg, regD=srcReg} ::
                moveGeneralToFloat{regN=srcReg, regD=fp} ::
                loadArgs(argTypes, srcRegs, fixedRegs, fpRegs)
        |   loadArgs _ = raise InternalError "rtsCall: Too many arguments"

        val instructions =
            loadArgs(argFormats,
                (* ML Arguments *) [X0, X1, X2, X3, X4, X5, X6, X7],
                (* C fixed pt args *) [X0, X1, X2, X3, X4, X5, X6, X7],
                (* C floating pt args *) [V0, V1, V2, V3, V4, V5, V6, V7]) @
            [
                (* Move X30 to X24, a callee-save register. *)
                (* Note: maybe we should push X24 just in case this is the only
                   reachable reference to the code. *)
                orrShiftedReg{regN=XZero, regM=X_LinkReg, regD=X24, shift=ShiftNone},
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
                loadRegScaled{regT=X_MLHeapLimit, regN=X_MLAssemblyInt, unitOffset=heapLimitPtrOffset}
            ] @
            (
                case resultFormat of
                    FastArgFixed => []
                |   FastArgDouble => (* This must be boxed. *) boxDouble(V0, X0, X1)
                |   FastArgFloat => (* This must be tagged *)
                    [
                        moveFloatToGeneral{regN=V0, regD=X0},
                        logicalShiftLeft{wordSize=WordSize64, shift=0w32, regN=X0, regD=X0},
                        bitwiseOrImmediate{regN=X0, regD=X0, wordSize=WordSize64, bits=0w1}
                    ]
            ) @
            [
                returnRegister X24
            ]
        val closure = makeConstantClosure()
        val () = generateCode{instrs=instructions, name=functionName, parameters=debugSwitches, resultClosure=closure}
    in
        closureAsAddress closure
    end


    fun rtsCallFast (functionName, nArgs, debugSwitches) =
        rtsCallFastGeneral (functionName, List.tabulate(nArgs, fn _ => FastArgFixed), FastArgFixed, debugSwitches)

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

    (* There is only one ABI value. *)
    datatype abi = ARM64Abi
    fun abiList () = [("default", ARM64Abi)]

    fun alignUp(s, align) = Word.andb(s + align-0w1, ~ align)
    fun intAlignUp(s, align) = Word.toInt(alignUp(Word.fromInt s, align))
    
    val getThreadDataCall = makeEntryPoint "PolyArm64GetThreadData"

    (* This must match the type in Foreign.LowLevel.  Once this is bootstrapped we could use that
       type but note that this is the type we use within the compiler and we build Foreign.LowLevel
       AFTER compiling this. *)
    datatype cTypeForm =
        CTypeFloatingPt | CTypePointer | CTypeSignedInt | CTypeUnsignedInt
    |   CTypeStruct of cType list | CTypeVoid
    withtype cType = { typeForm: cTypeForm, align: word, size: word }

    val generalArgRegs = [X0, X1, X2, X3, X4, X5, X6, X7]
    and fpArgRegs = [V0, V1, V2, V3, V4, V5, V6, V7]

    (* Load a value into a register.  Normally the size will be 1, 2, 4 or 8 bytes and
       this will just involve a simple load.  Structs, though, can be of any size up
       to 8 bytes. *)
    fun loadUpTo8(reg, base, offset, size, workReg) =
    let
        (* The argument is put in the least-significant bits implying we don't sign-extend. *)
        val loadOp =
            if size = 0w8
            then loadRegUnscaled
            else if size >= 0w4
            then loadRegUnscaled32
            else if size >= 0w2
            then loadRegUnscaled16
            else loadRegUnscaledByte
    in
        [loadOp{regT=reg, regN=base, byteOffset=Word.toInt offset}]
    end @
    (
        if size = 0w6 orelse size = 0w7
        then
        [
            loadRegUnscaled16{regT=workReg, regN=base, byteOffset=Word.toInt offset + 4},
            orrShiftedReg{regM=workReg, regN=reg, regD=reg, shift=ShiftLSL 0w32}
        ]
        else []
    ) @
    (
        if size = 0w3 orelse size = 0w5 orelse size = 0w7
        then
        [
            loadRegUnscaledByte{regT=workReg, regN=base, byteOffset=Word.toInt offset + Word.toInt(size-0w1)},
            orrShiftedReg{regM=workReg, regN=reg, regD=reg, shift=ShiftLSL((size-0w1)*0w8)}
        ]
        else []
    )

    (* Store a register into upto 8 bytes.  Most values will involve a single store but odd-sized
       structs can require shifts and multiple stores.  N.B.  May modify the source register. *)
    and storeUpTo8(reg, base, offset, size) =
    let
        val storeOp =
            if size = 0w8 then storeRegUnscaled else if size >= 0w4 then storeRegUnscaled32
            else if size >= 0w2 then storeRegUnscaled16 else loadRegUnscaledByte
    in
        [storeOp{regT=reg, regN=base, byteOffset=offset}]
    end @
    (
        if size = 0w6 orelse size = 0w7
        then
        [
            logicalShiftRight{ wordSize=WordSize64, regN=reg, regD=reg, shift=0w32 },
            storeRegUnscaled16{regT=reg, regN=base, byteOffset=offset+4}
        ]
        else []
    ) @
    (
        if size = 0w3 orelse size = 0w5 orelse size = 0w7
        then
        [
            logicalShiftRight{ wordSize=WordSize64, regN=reg, regD=reg, shift=(size-0w1)*0w8 },
            storeRegUnscaledByte{regT=reg, regN=base, byteOffset=Word.toInt(size-0w1)}
        ]
        else []
    )

    (* This builds a piece of code that takes three arguments and returns a unit result.
       All three arguments are SysWord.word values i.e. ML addresses containing the address
       of the actual C value.
       The first argument (X0) is the address of the function to call.
       The second argument (X1) points to a struct that contains the argument(s) for the
       function.  The arguments have to be unpacked from the struct into the appropriate
       registers or to the C stack.
       The third argument (X2) points to a piece of memory to receive the result of the call.
       It may be empty if the function returns void.  It may only be as big as required
       for the result type. *)
    fun foreignCall(_: abi, args: cType list, result: cType): Address.machineWord =
    let
        val resultAreaPtr = X19 (* Unboxed value from X2 - This is callee save. *)
        val argPtrReg = X9 (* A scratch register that isn't used for arguments. *)
        val entryPtReg = X16 (* Contains the address of the function to call. *)
        val argWorkReg = X10 (* Used in loading arguments if necessary. *)
        and argWorkReg2 = X11

        fun loadArgs([], stackOffset, _, _, _, code, preCode) = (code, stackOffset, preCode)

        |   loadArgs(arg::args, stackOffset, argOffset, gRegs, fpRegs, code, preCode) =
            let
                val {size, align, typeForm, ...} =  arg
                val newArgOffset = alignUp(argOffset, align)
            in
                case (typeForm, gRegs, fpRegs) of
                    (CTypeFloatingPt, _, _) => raise Foreign "TODO: Floating point"
                |   (CTypeStruct _, _, _) => raise Foreign "TODO: struct"

                |   (_, gReg::gRegs', fpRegs') =>
                        loadArgs(args, stackOffset, newArgOffset+size, gRegs', fpRegs',
                            loadUpTo8(gReg, argPtrReg, newArgOffset, size, argWorkReg) @ code, preCode)

                |   (_, [], fpRegs') =>
                        loadArgs(args, alignUp(stackOffset+size, 0w8), newArgOffset+size, [], fpRegs',
                            loadUpTo8(argWorkReg2, argPtrReg, newArgOffset, size, argWorkReg) @
                            (storeRegUnscaled{regT=argWorkReg2, regN=XSP, byteOffset=Word.toInt stackOffset} :: code), preCode)
            end
    
        local
            val {size, typeForm, ...} =  result
            (* Store a result register into the result area.  In almost all cases
               this is very simple: the only complication is with structs of odd sizes. *)
            fun storeResult(reg, offset, size) = storeUpTo8(reg, resultAreaPtr, offset, size)
        in
            val (getResult, passArgAddress) =
                if typeForm = CTypeVoid
                then ([], false)
                else case typeForm of
                    CTypeFloatingPt => raise Foreign "TODO: Floating point"
                |   CTypeStruct _ => raise Foreign "TODO: struct"
                |   _ => (storeResult(X0, 0, size), false)
        end

        val (argCode, argStack, preArgCode) =
            loadArgs(args, 0w0, 0w0, generalArgRegs, fpArgRegs,
                    if passArgAddress (* If we have to pass the address of the result struct it goes in X8. *)
                    then [moveRegToReg{sReg=resultAreaPtr, dReg=X8}]
                    else [], [])

        val stackSpaceRequired = alignUp(argStack, 0w16)

        val instructions =
            [(* Push the return address to the stack.  We could put it in a callee-save register but
                there's a very small chance that this could be the last reference to a piece of code. *)
             storeRegPreIndex{regT=X30, regN=X_MLStackPtr, byteOffset= ~8},
             (* Save heap ptr.  Needed in case we have a callback. *)
             storeRegScaled{regT=X_MLHeapAllocPtr, regN=X_MLAssemblyInt, unitOffset=heapAllocPtrOffset},
             (* Load the entry point address. *)
             loadRegScaled{regT=entryPtReg, regN=X0, unitOffset=0}] @
            (
                (* Unbox the address of the result area into a callee save resgister.  This is where
                   the result will be stored on return if it is anything other than a struct.
                   We have to put the C address in there now because an ML address wouldn't be updated
                   by a possible GC in a callback. *)
                if #typeForm(result) <> CTypeVoid
                then [loadRegScaled{regT=resultAreaPtr, regN=X2, unitOffset=0}]
                else []
            ) @
            [storeRegScaled{regT=X_MLStackPtr, regN=X_MLAssemblyInt, unitOffset=mlStackPtrOffset}] @ (* Save the stack pointer. *)
            (
                if stackSpaceRequired = 0w0
                then []
                else [subImmediate{regN=XSP, regD=XSP, immed=stackSpaceRequired, shifted=false}]
            ) @
            (
                (* The second argument is a SysWord containing the address of a malloced area of memory
                   with the actual arguments in it. *)
                if null args
                then []
                else [loadRegScaled{regT=argPtrReg, regN=X1, unitOffset=0}]
            ) @ preArgCode @ argCode @
            [branchAndLinkReg X16] @ (* Call the function. *)
            (* Restore the C stack value in case it's been changed by a callback. *)
            (
                if stackSpaceRequired = 0w0
                then []
                else [addImmediate{regN=XSP, regD=XSP, immed=stackSpaceRequired, shifted=false}]
            ) @
            [
                (* Reload the ML stack pointer even though it's callee save.  If we've made a callback
                   the ML stack could have grown and so moved to a different address. *)
                loadRegScaled{regT=X_MLStackPtr, regN=X_MLAssemblyInt, unitOffset=mlStackPtrOffset},
                (* Load the heap allocation ptr and limit in case of a callback. *)
                loadRegScaled{regT=X_MLHeapAllocPtr, regN=X_MLAssemblyInt, unitOffset=heapAllocPtrOffset},
                loadRegScaled{regT=X_MLHeapLimit, regN=X_MLAssemblyInt, unitOffset=heapLimitPtrOffset}
            ] @ (* Store the result in the destination. *) getResult @
            (* Pop the return address and return. *)
            [ loadRegPostIndex{regT=X30, regN=X_MLStackPtr, byteOffset= 8}, returnRegister X30 ]

        val functionName = "foreignCall"
        val debugSwitches =
            [(*Universal.tagInject Pretty.compilerOutputTag (Pretty.prettyPrint(print, 70)),
               Universal.tagInject Debug.assemblyCodeTag true*)]
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
