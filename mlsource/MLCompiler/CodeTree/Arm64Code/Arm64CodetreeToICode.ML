(*
    Copyright David C. J. Matthews 2021

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

functor Arm64CodetreeToICode(
    structure BackendTree: BACKENDINTERMEDIATECODE
    structure Arm64ICode: ARM64ICODE
    structure Debug: DEBUG
    structure Arm64Foreign: FOREIGNCALL
    structure ICodeTransform: ARM64ICODETRANSFORM
    structure CodeArray: CODEARRAY
    
    structure Arm64Fallback: GENCODE

    sharing Arm64ICode.Sharing = ICodeTransform.Sharing = CodeArray.Sharing = Arm64Fallback.Sharing = BackendTree.Sharing
): GENCODE =
struct
    open BackendTree
    open Address
    open Arm64ICode
    open CodeArray
    
    exception InternalError = Misc.InternalError
    
    datatype blockStruct =
        BlockSimple of arm64ICode
    |   BlockExit of arm64ICode
    |   BlockLabel of int
    |   BlockFlow of controlFlow
    |   BlockBegin of { regArgs: (preg * xReg) list, stackArgs: stackLocn list }
    |   BlockRaiseAndHandle of arm64ICode * int
    |   BlockOptionalHandle of {call: arm64ICode, handler: int, label: int }

    fun codeFunctionToArm64({body, localCount, name, argTypes, resultType=fnResultType, closure, ...}:bicLambdaForm, debugSwitches, resultClosure) =
    let
        (* Pseudo-registers are allocated sequentially and the properties added to the list. *)
        val pregCounter = ref 0
        val pregPropList = ref []
        fun newPReg() =
        let
            val regNo = !pregCounter before pregCounter := !pregCounter + 1
            val () = pregPropList := RegPropGeneral :: !pregPropList
        in
            PReg regNo
        end
        
        and newUReg() =
        let
            val regNo = !pregCounter before pregCounter := !pregCounter + 1
            val () = pregPropList := RegPropUntagged :: !pregPropList
        in
            PReg regNo
        end
        
        and newStackLoc size =
        let
            val regNo = !pregCounter before pregCounter := !pregCounter + 1
            val () = pregPropList := RegPropStack size :: !pregPropList
        in
            StackLoc{size=size, rno=regNo}
        end
        
        and newMergeReg() =
        let
            val regNo = !pregCounter before pregCounter := !pregCounter + 1
            val () = pregPropList := RegPropMultiple :: !pregPropList
        in
            PReg regNo
        end
        
        datatype locationValue =
            NoLocation
        |   PregLocation of preg
        |   ContainerLocation of { container: stackLocn, stackOffset: int }

        val locToPregArray = Array.array(localCount, NoLocation)
        val labelCounter = ref 1 (* Start at 1.  Zero is used for the root. *)
        fun newLabel() = !labelCounter before labelCounter := !labelCounter + 1
        val ccRefCounter = ref 0
        fun newCCRef() = CcRef(!ccRefCounter) before ccRefCounter := !ccRefCounter + 1

        (* The profile object is a single mutable with the F_bytes bit set. *)
        local
            val v = RunCall.allocateByteMemory(0w1, Word.fromLargeWord(Word8.toLargeWord(Word8.orb(F_mutable, F_bytes))))
            fun clear 0w0 = ()
            |   clear i = (assignByte(v, i-0w1, 0w0); clear (i-0w1))
            val () = clear wordSize
        in
            val profileObject = toMachineWord v
        end

        datatype destination =
            SpecificPReg of preg
        |   NoResult
        (*|   Allowed of allowedArgument*)

        datatype argLoc =
            ArgInReg of { realReg: xReg, argReg: preg }
        |   ArgOnStack of { stackOffset: int, stackReg: stackLocn }

        (* Pseudo-regs for the result, the closure and the args that were passed in real regs. *)
        val resultTarget = newPReg()
        val closureRegAddr = newPReg()
        val returnAddrReg = newPReg()
        
        val generalArgRegs = [X0, X1, X2, X3, X4, X5, X6, X7]

        (* Create a map for the arguments indicating their register or stack location. *)
        local
            (* Select the appropriate argument register depending on the argument type. *)
            fun argTypesToArgEntries([], _, _) = ([], [], [], [])

            |   argTypesToArgEntries(_ :: tl, gReg :: gRegs, n) =
                (* This deals with general arguments but also with extra floating point arguments.
                   They are boxed as usual. *)
                let
                    val (argTypes, argCode, argRegs, stackArgs) =
                        argTypesToArgEntries(tl, gRegs, n-1)
                    val argReg=newPReg()
                in
                    (ArgInReg{realReg=gReg, argReg=argReg} :: argTypes, argCode, (argReg, gReg) :: argRegs, stackArgs)
                end

            |   argTypesToArgEntries(_ :: tl, [], n) =
                let
                    val (argTypes, argCode, argRegs, stackArgs) = argTypesToArgEntries(tl, [], n-1)
                    val stackLoc = newStackLoc 1
                in
                    (ArgOnStack {stackOffset=n, stackReg = stackLoc } :: argTypes, argCode, argRegs, stackLoc :: stackArgs)
                end

            val (argEntries, argCode, argRegs, stackArguments) =
                argTypesToArgEntries(argTypes, generalArgRegs, List.length argTypes)
            val clReg = case closure of [] => [] | _ => [(closureRegAddr, X8)]
            val retReg = [(returnAddrReg, X30)]
        in
            val argumentVector = Vector.fromList argEntries

            (* Start code for the function. *)
            val beginInstructions = argCode @
                [BlockBegin{regArgs=retReg @ clReg @ argRegs, stackArgs=stackArguments @ [returnAddressEntry]}]

            (* The number of arguments on the stack.  Needed in return instrs and tail calls. *)
            val currentStackArgs = List.length stackArguments
        end
        
        fun codeToICodeRev(BICExtract(BICLoadArgument a), {stackPtr, ...}, _, destination, tailCode) =
            (
                case Vector.sub(argumentVector, a) of
                    ArgInReg{argReg, ...} => (* It was originally in a register.  It's now in a preg. *)
                        moveIfNotAllowedRev(destination, tailCode, RegisterArgument argReg)
                |   ArgOnStack{stackOffset, stackReg} => (* Pushed before call. *)
                            raise Fallback "codeToICodeRev"
                    (*let
                        val target = asTarget destination
                    in
                        (BlockSimple(LoadArgument{
                            source=StackLocation{wordOffset=stackOffset+stackPtr, container=stackReg, field=0, cache=NONE},
                            dest=target, kind=moveNativeWord}) :: tailCode,
                         RegisterArgument target, false)
                    end*)
            )
        
        |   codeToICodeRev _ = raise Fallback "codeToICodeRev"

        (*Turn the codetree structure into icode. *)
        val bodyContext = {loopArgs=NONE, stackPtr=0, currHandler=NONE, overflowBlock=ref NONE}
        val (bodyCode, _, bodyExited) =
            (*codeToICodeRev(body, bodyContext, true, SpecificPReg resultTarget, beginInstructions) *)
                raise Fallback "bodyCode"
        val icode = if bodyExited then bodyCode else raise Fallback "returnInstruction" (*returnInstruction(bodyContext, resultTarget, bodyCode)*)
        
        (* Turn the icode list into basic blocks.  The input list is in reverse so as part of
           this we reverse the list. *)
        local
            val resArray = Array.array(!labelCounter, BasicBlock{ block=[], flow=ExitCode })
            
            fun createEntry (blockNo, block, flow) =
                Array.update(resArray, blockNo, BasicBlock{ block=block, flow=flow})
            
            fun splitCode([], _, _) = 
                (* End of code.  We should have had a BeginFunction. *)
                raise InternalError "splitCode - no begin"
            
            |   splitCode(BlockBegin args :: _, sinceLabel, flow) =
                    (* Final instruction.  Create the initial block and exit. *)
                    raise Fallback "BeginFunction"
                    (*createEntry(0, BeginFunction args ::sinceLabel, flow)*)
            
            |   splitCode(BlockSimple instr :: rest, sinceLabel, flow) =
                    splitCode(rest, instr :: sinceLabel, flow)

            |   splitCode(BlockLabel label :: rest, sinceLabel, flow) =
                    (* Label - finish this block and start another. *)
                (
                    createEntry(label, sinceLabel, flow);
                    (* Default to a jump to this label.  That is used if we have
                       assumed a drop-through. *)
                    splitCode(rest, [], Unconditional label)
                )
            
            |   splitCode(BlockExit instr :: rest, _, _) =
                    splitCode(rest, [instr], ExitCode)

            |   splitCode(BlockFlow flow :: rest, _, _) =
                    splitCode(rest, [], flow)
            
            |   splitCode(BlockRaiseAndHandle(instr, handler) :: rest, _, _) =
                    splitCode(rest, [instr], UnconditionalHandle handler)

            |   splitCode(BlockOptionalHandle{call, handler, label} :: rest, sinceLabel, flow) =
                let
                    (* A function call within a handler.  This could go to the handler but
                       if there is no exception will go to the next instruction.
                       Also includes JumpLoop since the stack check could result in an
                       Interrupt exception. *)
                in
                    createEntry(label, sinceLabel, flow);
                    splitCode(rest, [call], ConditionalHandle{handler=handler, continue=label})
                end

        in
            val () = splitCode(icode, [], ExitCode)
            val resultVector = Array.vector resArray
        end
      
        open ICodeTransform
        
        val pregProperties = Vector.fromList(List.rev(! pregPropList))
    in
        codeICodeFunctionToArm64{blocks = resultVector, functionName = name, pregProps = pregProperties,
            ccCount= ! ccRefCounter, debugSwitches = debugSwitches, resultClosure = resultClosure,
            profileObject = profileObject}
    end

    fun gencodeLambda(lambda, debugSwitches, closure) =
    let
        open Debug Universal
        (*val debugSwitches =
            [tagInject Pretty.compilerOutputTag (Pretty.prettyPrint(print, 70)),
            tagInject assemblyCodeTag true] @ debugSwitches*)
    in
        codeFunctionToArm64(lambda, debugSwitches, closure)
            handle Fallback _ => Arm64Fallback.gencodeLambda(lambda, debugSwitches, closure)
    end

    structure Foreign = Arm64Foreign
    
    structure Sharing =
    struct
        type backendIC = backendIC
        and  bicLoadForm = bicLoadForm
        and argumentType = argumentType
        and closureRef = closureRef
    end
    
end;
