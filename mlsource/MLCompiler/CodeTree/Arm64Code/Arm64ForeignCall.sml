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

    local
        (* Both doubles and sysword values occupy a single heap cell. *)
        fun allocateAWord(fixedReg, workReg) =
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
                storeRegUnscaled{regT=workReg, regN=fixedReg, byteOffset= ~8}
            ]
        end
    in
        fun boxDouble(floatReg, fixedReg, workReg) =
            allocateAWord(fixedReg, workReg) @
                [storeRegScaledDouble{regT=floatReg, regN=fixedReg, unitOffset=0}]
        and boxSysWord(toBoxReg, fixedReg, workReg) =
            allocateAWord(fixedReg, workReg) @
                [storeRegScaled{regT=toBoxReg, regN=fixedReg, unitOffset=0}]
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
                logicalShiftRight{shift=0w32, regN=srcReg, regD=srcReg} ::
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
                        logicalShiftLeft{shift=0w32, regN=X0, regD=X0},
                        bitwiseOrImmediate{regN=X0, regD=X0, bits=0w1}
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
    
    val getThreadDataCall = makeEntryPoint "PolyArm64GetThreadData"

    (* This must match the type in Foreign.LowLevel.  Once this is bootstrapped we could use that
       type but note that this is the type we use within the compiler and we build Foreign.LowLevel
       AFTER compiling this. *)
    datatype cTypeForm =
        CTypeFloatingPt | CTypePointer | CTypeSignedInt | CTypeUnsignedInt
    |   CTypeStruct of cType list | CTypeVoid
    withtype cType = { typeForm: cTypeForm, align: word, size: word }

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
            logicalShiftRight{regN=reg, regD=reg, shift=0w32 },
            storeRegUnscaled16{regT=reg, regN=base, byteOffset=offset+4}
        ]
        else []
    ) @
    (
        if size = 0w3 orelse size = 0w5 orelse size = 0w7
        then
        [
            logicalShiftRight{regN=reg, regD=reg, shift=(size-0w1)*0w8 },
            storeRegUnscaledByte{regT=reg, regN=base, byteOffset=Word.toInt(size-0w1)}
        ]
        else []
    )

    (* Extract the elements of structures. *)
    fun unwrap(CTypeStruct ctypes, _) = List.foldr(fn({typeForm, size, ...}, l) => unwrap(typeForm, size) @ l) [] ctypes
    |   unwrap (ctype, size) = [(ctype, size)]

    (* Structures of up to four floating point values of the same precision are treated specially. *)
    datatype argClass =
        ArgClassHFA of Word8.word * bool (* 1 - 4 floating pt values *)
    |   ArgLargeStruct          (* > 16 bytes and not an HFA *)
    |   Arg9To16Bytes           (* Two X registers *)
    |   Arg1To8Bytes            (* One X register *)

    fun classifyArg(ctype, size) =
        case unwrap (ctype, size) of
            [(CTypeFloatingPt, 0w4)] =>
                ArgClassHFA(0w1, false)
        |   [(CTypeFloatingPt, 0w4), (CTypeFloatingPt, 0w4)] =>
                ArgClassHFA(0w2, false)
        |   [(CTypeFloatingPt, 0w4), (CTypeFloatingPt, 0w4), (CTypeFloatingPt, 0w4)] =>
                ArgClassHFA(0w3, false)
        |   [(CTypeFloatingPt, 0w4), (CTypeFloatingPt, 0w4), (CTypeFloatingPt, 0w4), (CTypeFloatingPt, 0w4)] =>
                ArgClassHFA(0w4, false)
        |   [(CTypeFloatingPt, 0w8)] =>
                ArgClassHFA(0w1, true)
        |   [(CTypeFloatingPt, 0w8), (CTypeFloatingPt, 0w8)] =>
                ArgClassHFA(0w2, true)
        |   [(CTypeFloatingPt, 0w8), (CTypeFloatingPt, 0w8), (CTypeFloatingPt, 0w8)] =>
                ArgClassHFA(0w3, true)
        |   [(CTypeFloatingPt, 0w8), (CTypeFloatingPt, 0w8), (CTypeFloatingPt, 0w8), (CTypeFloatingPt, 0w8)] =>
                ArgClassHFA(0w4, true)
        |   _ =>
                if size > 0w16 then ArgLargeStruct
                else if size > 0w8 then Arg9To16Bytes
                else Arg1To8Bytes

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
        and structSpacePtr = X12
        and argWorkReg3 = X13
        and argWorkReg4 = X14

        fun loadArgs([], stackOffset, _, _, _, code, largeStructSpace) =
                (code, stackOffset, largeStructSpace)

        |   loadArgs(arg::args, stackOffset, argOffset, gRegNo, fpRegNo, code, largeStructSpace) =
            let
                val {size, align, typeForm, ...} =  arg
                val newArgOffset = alignUp(argOffset, align)
            in
                case classifyArg(typeForm, size) of
                    ArgClassHFA(numItems, isDouble) =>
                        if fpRegNo + numItems <= 0w8
                        then
                        let
                            val scale = if isDouble then 0w8 else 0w4
                            (* Load the values to the floating point registers. *)
                            fun loadFPRegs(0w0, _, _) = []
                            |   loadFPRegs(0w1, fpRegNo, offset) =
                                [(if isDouble then loadRegScaledDouble else loadRegScaledFloat)
                                    {regT=VReg fpRegNo, regN=argPtrReg, unitOffset=offset}]
                            |   loadFPRegs(n, fpRegNo, offset) =
                                (if isDouble then loadPairOffsetDouble else loadPairOffsetFloat)
                                    {regT1=VReg fpRegNo, regT2=VReg(fpRegNo+0w1), regN=argPtrReg, unitOffset=offset} ::
                                        loadFPRegs(n-0w2, fpRegNo+0w2, offset+2)
                        in
                            loadArgs(args, stackOffset, newArgOffset+size, gRegNo, fpRegNo+numItems,
                                loadFPRegs(numItems, fpRegNo, Word.toInt(newArgOffset div scale)) @ code,
                                largeStructSpace)
                        end
                        else
                        let
                            (* If we have insufficient number of registers we discard any that are
                               left and push the argument to the stack. *)
                            (* The floating point value or structure is copied to the stack
                               as a contiguous area.  Use general registers to copy the data.
                               It could be on a 4-byte alignment.  In the typical case of a single
                               floating point value this will just be a single load and store. *)
                            fun copyData(0w0, _, _) = []
                            |   copyData(n, srcOffset, stackOffset) =
                                if isDouble
                                then loadRegScaled{regT=argWorkReg2, regN=argPtrReg, unitOffset=srcOffset} ::
                                     storeRegScaled{regT=argWorkReg2, regN=XSP, unitOffset=stackOffset} ::
                                     copyData(n-0w1, srcOffset+1, stackOffset+1)
                                else loadRegScaled32{regT=argWorkReg2, regN=argPtrReg, unitOffset=srcOffset} ::
                                     storeRegScaled32{regT=argWorkReg2, regN=XSP, unitOffset=stackOffset} ::
                                     copyData(n-0w1, srcOffset+1, stackOffset+1)

                            val copyToStack =
                                if isDouble
                                then copyData(numItems, Word.toInt(newArgOffset div 0w8), stackOffset)
                                else copyData(numItems, Word.toInt(newArgOffset div 0w4), stackOffset*2)
                            (* The overall size is rounded up to a multiple of 8 *)
                            val newStackOffset = stackOffset + Word.toInt(alignUp(size, 0w8) div 0w8)
                        in
                            loadArgs(args, newStackOffset, newArgOffset+size, gRegNo, 0w8,
                                copyToStack @ code, largeStructSpace)
                        end

                |   ArgLargeStruct => (* Larger structures are passed by reference. *)
                    let
                        (* Structures of other sizes are passed by reference.  They are first
                           copied into new areas on the stack.  This ensures that the called function
                           can update the structure without changing the original values. *)
                        val newStructSpace = alignUp(largeStructSpace + size, 0w16)
                        val loopLabel = createLabel()
                        val argRegNo = if gRegNo < 0w8 then XReg gRegNo else argWorkReg
                        (* Copy from the end back to the start. *)
                        val argCode =
                        [
                            addImmediate{regN=structSpacePtr, regD=argRegNo, immed=largeStructSpace, shifted=false}, 
                            addImmediate{regN=argRegNo, regD=argWorkReg2, immed=size, shifted=false}, (* End of dest area *)
                            addImmediate{regN=argPtrReg, regD=argWorkReg3, immed=newArgOffset+size, shifted=false}, (* end of source *)
                            setLabel loopLabel,
                            loadRegPreIndexByte{regT=argWorkReg4, regN=argWorkReg3, byteOffset= ~1},
                            storeRegPreIndexByte{regT=argWorkReg4, regN=argWorkReg2, byteOffset= ~1},
                            subSShiftedReg{regM=argWorkReg2, regN=argRegNo, regD=XZero, shift=ShiftNone}, (* At start? *)
                            conditionalBranch(condNotEqual, loopLabel)
                        ]
                    in
                        if gRegNo < 0w8
                        then loadArgs(args, stackOffset, newArgOffset+size, gRegNo+0w1, fpRegNo, argCode @ code, newStructSpace)
                        else loadArgs(args, stackOffset+1, newArgOffset+size, 0w8, fpRegNo,
                                argCode @ storeRegScaled{regT=argWorkReg, regN=XSP, unitOffset=stackOffset} :: code,
                                newStructSpace)
                    end

                |   Arg9To16Bytes =>
                    (* Structs between 8 and 16 bytes require two registers. *)
                        (* Note: If this is a struct the source value may not be aligned on any particular alignment.
                           We ignore that and use 64-bit loads.  However that means we can't necessarily use a load-pair. *)
                    (
                        if gRegNo <= 0w6
                        then loadArgs(args, stackOffset, newArgOffset+size, gRegNo + 0w2, fpRegNo,
                                loadRegUnscaled{regT=XReg gRegNo, regN=argPtrReg, byteOffset=Word.toInt newArgOffset} ::
                                loadUpTo8(XReg(gRegNo+0w1), argPtrReg, newArgOffset+0w8, size-0w8, argWorkReg) @ code, largeStructSpace)

                        else loadArgs(args, stackOffset+2, newArgOffset+size, 0w8, fpRegNo,
                                loadRegUnscaled{regT=argWorkReg2, regN=argPtrReg, byteOffset=Word.toInt newArgOffset} ::
                                storeRegScaled{regT=argWorkReg2, regN=XSP, unitOffset=stackOffset} ::
                                loadUpTo8(argWorkReg2, argPtrReg, newArgOffset+0w8, size-0w8, argWorkReg) @
                                storeRegScaled{regT=argWorkReg2, regN=XSP, unitOffset=stackOffset + 1} :: code, largeStructSpace)
                    )

                |   Arg1To8Bytes =>
                    (
                        if gRegNo <= 0w7
                        then loadArgs(args, stackOffset, newArgOffset+size, gRegNo + 0w1, fpRegNo,
                                loadUpTo8(XReg gRegNo, argPtrReg, newArgOffset, size, argWorkReg) @ code, largeStructSpace)
                        else loadArgs(args, stackOffset+1, newArgOffset+size, 0w8, fpRegNo,
                                loadUpTo8(argWorkReg2, argPtrReg, newArgOffset, size, argWorkReg) @
                                storeRegScaled{regT=argWorkReg2, regN=XSP, unitOffset=stackOffset} :: code, largeStructSpace)
                    )
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
                else case classifyArg(typeForm, size) of
                    (* Floating point values are returned in s0-sn, d0-dn. *)
                    ArgClassHFA(numItems, isDouble) =>
                        let
                            fun storeFPRegs(0w0, _, _) = []
                            |   storeFPRegs(0w1, fpRegNo, offset) =
                                [(if isDouble then storeRegScaledDouble else storeRegScaledFloat)
                                    {regT=VReg fpRegNo, regN=resultAreaPtr, unitOffset=offset}]
                            |   storeFPRegs(n, fpRegNo, offset) =
                                (if isDouble then storePairOffsetDouble else storePairOffsetFloat)
                                    {regT1=VReg fpRegNo, regT2=VReg(fpRegNo+0w1), regN=resultAreaPtr, unitOffset=offset} ::
                                        storeFPRegs(n-0w2, fpRegNo+0w2, offset+2)
                        in
                            (storeFPRegs(numItems, 0w0 (* V0-Vn*), 0), false)
                        end

                |   ArgLargeStruct => ([], true) (* Structures larger than 16 bytes are passed by reference. *)
                    (* Structures between 8 and 16 bytes are returned in X0/X1. *)
                |   Arg9To16Bytes =>
                        if size = 0w16
                        then ([storePairOffset{regT1=X0, regT2=X1, regN=resultAreaPtr, unitOffset=0}], false)
                        else (storeRegScaled{regT=X0, regN=resultAreaPtr, unitOffset=0} :: storeResult(X1, 8, size-0w8), false)
                |   Arg1To8Bytes => (storeResult(X0, 0, size), false)
        end

        val (argCode, argStack, largeStructSpace) =
            loadArgs(args, 0, 0w0, 0w0, 0w0,
                    if passArgAddress (* If we have to pass the address of the result struct it goes in X8. *)
                    then [moveRegToReg{sReg=resultAreaPtr, dReg=X8}]
                    else [], 0w0)

        val stackSpaceRequired = alignUp(Word.fromInt argStack * 0w8, 0w16) + largeStructSpace

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
                (* If we need to copy a struct load a register with a pointer to the area for it. *)
                if largeStructSpace = 0w0
                then []
                else [addImmediate{regN=XSP, regD=structSpacePtr, immed=stackSpaceRequired-largeStructSpace, shifted=false}]
            ) @
            (
                (* The second argument is a SysWord containing the address of a malloced area of memory
                   with the actual arguments in it. *)
                if null args
                then []
                else [loadRegScaled{regT=argPtrReg, regN=X1, unitOffset=0}]
            ) @ argCode @
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

        (* Store the argument values to the structure that will be passed to the ML callback function. *)
        (* Note.  We've loaded the frame pointer with the original stack ptr-96 so we can
           access any stack arguments from that. *)
        fun moveArgs([], _, _, _, _, moveFromStack) = moveFromStack
        
        |   moveArgs(arg::args, stackSpace, argOffset, gRegNo, fpRegNo, moveFromStack) =
            let
                val {size, align, typeForm, ...} =  arg
                val newArgOffset = alignUp(argOffset, align)
            in
                case classifyArg(typeForm, size) of
                    ArgClassHFA(numItems, isDouble) => raise Foreign "TODO: ArgClassHFA"
                |   ArgLargeStruct =>  raise Foreign "TODO: ArgLargeStruct"
                |   Arg9To16Bytes =>  raise Foreign "TODO: Arg9To16Bytes"

                |   Arg1To8Bytes =>
                    (
                        if gRegNo <= 0w7
                        then moveArgs(args, stackSpace, newArgOffset+size, gRegNo + 0w1, fpRegNo,
                                storeUpTo8(XReg gRegNo, XSP, Word.toInt newArgOffset, size) @ moveFromStack)
                        else (*moveArgs(args, stackOffset+1, newArgOffset+size, 0w8, fpRegNo,
                                loadUpTo8(argWorkReg2, argPtrReg, newArgOffset, size, argWorkReg) @
                                storeRegScaled{regT=argWorkReg2, regN=XSP, unitOffset=stackOffset} :: code, moveFromStack)*)
                            raise Foreign "TODO: Stack arguments"
                    )
            end

        val copyArgsFromRegsAndStack = moveArgs(args, 0, 0w0, 0w0, 0w0, [])

        local
            fun getNextSize (arg, argOffset) =
            let val {size, align, ...} =  arg in alignUp(argOffset, align) + size end
        in
            val argumentSpace = alignUp(List.foldl getNextSize 0w0 args, 0w16)
        end
        (* After the argument space there is a 32 byte area for the results. *)

        local
            val {size, typeForm, ...} =  result
        in
            val (loadResults, resultByReference) =
                if typeForm = CTypeVoid
                then ([], false)
                else case classifyArg(typeForm, size) of
                    ArgClassHFA(numItems, isDouble) => raise Foreign "TODO ArgClassHFA"
                |   ArgLargeStruct => ([], true) (* Structures larger than 16 bytes are passed by reference. *)
                |   Arg9To16Bytes => raise Foreign "TODO Arg9To16Bytes"
                |   Arg1To8Bytes => (loadUpTo8(X0, XSP, argumentSpace, size, X2),false)
        end

        val instructions =
            [ (* Push LR, FP and the callee-save registers. *)
                storePairPreIndexed{regT1=X29, regT2=X30, regN=XSP, unitOffset= ~12},
                moveRegToReg{sReg=XSP, dReg=X29},
                storePairOffset{regT1=X19, regT2=X20, regN=X29, unitOffset=2},
                storePairOffset{regT1=X21, regT2=X22, regN=X29, unitOffset=4},
                storePairOffset{regT1=X23, regT2=X24, regN=X29, unitOffset=6},
                storePairOffset{regT1=X25, regT2=X26, regN=X29, unitOffset=8},
                storePairOffset{regT1=X27, regT2=X28, regN=X29, unitOffset=10},
                (* Reserve an area for any results returned in registers.  This could
                   be up to 32 bytes.  Save X8 in it in case we're actually returning
                   a result by reference. *)
                storeRegPreIndex{regT=X8, regN=XSP, byteOffset= ~32},
                (* Reserve space for the arguments. *)
                subImmediate{regN=XSP, regD=XSP, immed=argumentSpace, shifted=false},
                (* We passed the function we're calling in X9 but we need to move
                   it to a callee-save register. *)
                moveRegToReg{sReg=X9, dReg=X20}
            ] @ copyArgsFromRegsAndStack @
            [
                (* Call into the RTS to get the thread data ptr. *)
                loadAddressConstant(X0, getThreadDataCall),
                loadRegScaled{regT=X0, regN=X0, unitOffset=0},
                branchAndLinkReg X0,
                moveRegToReg{sReg=X0, dReg=X_MLAssemblyInt},
                (* Load the ML regs. *)
                loadRegScaled{regT=X_MLHeapLimit, regN=X_MLAssemblyInt, unitOffset=heapLimitPtrOffset},
                loadRegScaled{regT=X_MLHeapAllocPtr, regN=X_MLAssemblyInt, unitOffset=heapAllocPtrOffset},
                loadRegScaled{regT=X_MLStackPtr, regN=X_MLAssemblyInt, unitOffset=mlStackPtrOffset},
                (* Prepare the arguments.  They are both syswords so have to be boxed. *)
                moveRegToReg{sReg=XSP, dReg=X2} (* Move to X2 before boxing *)
            ] @ boxSysWord(X2, X0, X3) @ (* Address of arguments. *)
            (
                (* Result area pointer.  If we're returning by reference this is the original
                   value of X8 otherwise it's the address of the 32 bytes we've reserved. *)
                if resultByReference
                then [loadRegScaled{regT=X2, regN=XSP, unitOffset=Word.toInt(argumentSpace div 0w8)}]
                else [addImmediate{regN=XSP, regD=X2, immed=argumentSpace, shifted=false}]
            ) @ boxSysWord(X2, X1, X3) @
            [
                moveRegToReg{sReg=X20, dReg=X8},
                loadRegScaled{regT=X16, regN=X8, unitOffset=0},
                branchAndLinkReg X16,
                (* Save the ML stack and heap pointers.  We could have allocated or
                   grown the stack.  The limit pointer is maintained by the RTS. *)
                storeRegScaled{regT=X_MLHeapAllocPtr, regN=X_MLAssemblyInt, unitOffset=heapAllocPtrOffset},
                storeRegScaled{regT=X_MLStackPtr, regN=X_MLAssemblyInt, unitOffset=mlStackPtrOffset}
            ] @ loadResults @ (* Load the return values *)
            [
                (* Restore the callee-save registers and return. *)
                moveRegToReg{sReg=X29, dReg=XSP},
                loadPairOffset{regT1=X19, regT2=X20, regN=X29, unitOffset=2},
                loadPairOffset{regT1=X21, regT2=X22, regN=X29, unitOffset=4},
                loadPairOffset{regT1=X23, regT2=X24, regN=X29, unitOffset=6},
                loadPairOffset{regT1=X25, regT2=X26, regN=X29, unitOffset=8},
                loadPairOffset{regT1=X27, regT2=X28, regN=X29, unitOffset=10},
                loadPairPostIndexed{regT1=X29, regT2=X30, regN=XSP, unitOffset=12},
                returnRegister X30
            ]

        val functionName = "foreignCallBack(2)"
        val debugSwitches =
            [(*Universal.tagInject Pretty.compilerOutputTag (Pretty.prettyPrint(print, 70)),
               Universal.tagInject Debug.assemblyCodeTag true*)]

        val closure = makeConstantClosure()
        val () = generateCode{instrs=instructions, name=functionName, parameters=debugSwitches, resultClosure=closure}
        val stage2Code = closureAsAddress closure

        fun resultFunction f =
        let
            (* Generate a small function to load the address of f into a register and then jump to stage2.
               The idea is that it should be possible to generate this eventually in a single RTS call.
               That could be done by using a version of this as a model. *)
            val codeAddress = Address.loadWord(Address.toAddress stage2Code, 0w0)
            val instructions =
                [
                    loadAddressConstant(X9, Address.toMachineWord f),
                    loadAddressConstant(X16, codeAddress),
                    branchRegister X16
                ]
            val functionName = "foreignCallBack(1)"
            val debugSwitches =
                [(*Universal.tagInject Pretty.compilerOutputTag (Pretty.prettyPrint(print, 70)),
                   Universal.tagInject Debug.assemblyCodeTag true*)]
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
