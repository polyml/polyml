(*
    Signature for the high-level ARM64 code

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

signature ARM64ICODE =
sig
    type machineWord = Address.machineWord
    type address = Address.address
    
    type closureRef
    
    exception Fallback of string (* Temporarily for development. *)

    (* Registers. *)
    datatype xReg = XReg of Word8.word | XZero | XSP
    and vReg = VReg of Word8.word

    val X0:  xReg   and X1:  xReg   and X2:  xReg   and X3: xReg
    and X4:  xReg   and X5:  xReg   and X6:  xReg   and X7: xReg
    and X8:  xReg   and X9:  xReg   and X10: xReg   and X11: xReg
    and X12: xReg   and X13: xReg   and X14: xReg   and X15: xReg
    and X16: xReg   and X17: xReg   and X18: xReg   and X19: xReg
    and X20: xReg   and X21: xReg   and X22: xReg   and X23: xReg
    and X24: xReg   and X25: xReg   and X26: xReg   and X27: xReg
    and X28: xReg   and X29: xReg   and X30: xReg

    val V0:  vReg   and V1:  vReg   and V2:  vReg   and V3: vReg
    and V4:  vReg   and V5:  vReg   and V6:  vReg   and V7: vReg
    
    val is32in64: bool and isBigEndian: bool
    
    (* Condition for conditional branches etc. *)
    datatype condition =
        CondEqual            (* Z=1 *)
    |   CondNotEqual         (* Z=0 *)
    |   CondCarrySet         (* C=1 *)
    |   CondCarryClear       (* C=0 *)
    |   CondNegative         (* N=1 *)
    |   CondPositive         (* N=0 imcludes zero *)
    |   CondOverflow         (* V=1 *)
    |   CondNoOverflow       (* V=0 *)
    |   CondUnsignedHigher   (* C=1 && Z=0 *)
    |   CondUnsignedLowOrEq  (* ! (C=1 && Z=0) *)
    |   CondSignedGreaterEq  (* N=V *)
    |   CondSignedLess       (* N<>V *)
    |   CondSignedGreater    (* Z==0 && N=V *)
    |   CondSignedLessEq     (* !(Z==0 && N=V) *)

    datatype preg = PReg of int (* A pseudo-register - an abstract register. *)
    
    (* A location on the stack.  May be more than word if this is a container or a handler entry. *)
    datatype stackLocn = StackLoc of {size: int, rno: int }
    
    (* This combines pregKind and stackLocn.  *)
    datatype regProperty =
        RegPropGeneral      (* A general register. *)
    |   RegPropUntagged     (* An untagged general register. *)
    |   RegPropStack of int (* A stack location or container. *)
    |   RegPropCacheTagged
    |   RegPropCacheUntagged
    |   RegPropMultiple     (* The result of a conditional or case. May be defined at multiple points. *)
    
    (* The reference to a condition code. *)
    datatype ccRef = CcRef of int

    datatype loadType = Load64 | Load32 | Load16 | Load8
    and arithLength = Arith64 | Arith32
    and logicalOp = LogAnd | LogOr | LogXor
    and callKinds = Recursive | ConstantCode of machineWord | FullCall

    (* Function calls can have an unlimited number of arguments so it isn't always
       going to be possible to load them into registers. *)
    datatype fnarg = ArgInReg of preg | ArgOnStack of { wordOffset: int, container: stackLocn, field: int }

    datatype arm64ICode =
        (* Move the contents of one preg to another.  These are always 64-bits. *)
        MoveRegister of { source: preg, dest: preg }

        (* Numerical constant. *)
    |   LoadNonAddressConstant of { source: Word64.word, dest: preg }

        (* Address constant. *)
    |   LoadAddressConstant of { source: machineWord, dest: preg }

        (* Load a value into a register using a constant, signed, byte offset.  The offset
           is in the range of -256 to (+4095*unit size). *)
    |   LoadWithConstantOffset of { base: preg, dest: preg, byteOffset: int, loadType: loadType }

        (* Load a value into a register using an index register. *)
    |   LoadWithIndexedOffset of { base: preg, dest: preg, index: preg, loadType: loadType }

        (* Convert a 32-in-64 object index into an absolute address. *)
    |   ObjectIndexAddressToAbsolute of { source: preg, dest: preg }

        (* Convert an absolute address into an object index. *)
    |   AbsoluteToObjectIndex of { source: preg, dest: preg }

        (* Allocate a fixed sized piece of memory and puts the absolute address into dest.
           bytesRequired is the total number of bytes including the length word and any alignment
           necessary for 32-in-64. saveRegs is the list of registers that need to be saved if we
           need to do a garbage collection. *)
    |   AllocateMemoryOperation of { bytesRequired: Word64.word, dest: preg, saveRegs: preg list }

        (* Store a register using a constant, signed, byte offset.  The offset
           is in the range of -256 to (+4095*unit size). *)
    |   StoreWithConstantOffset of { source: preg, base: preg, byteOffset: int, loadType: loadType }

        (* Store a register using an index register. *)
    |   StoreWithIndexedOffset of { source: preg, base: preg, index: preg, loadType: loadType }

        (* Add/Subtract immediate.  The destination is optional in which case XZero is used.
           ccRef is optional.  If it is NONE the version of the instruction that does not generate
           a condition code is used. immed must be < 0wx1000. *)
    |   AddSubImmediate of { source: preg, dest: preg option, ccRef: ccRef option, immed: word,
                             isAdd: bool, length: arithLength }

        (* Add/Subtract register.  As with AddSubImmediate, both the destination and cc are optional. *)
    |   AddSubRegister of { operand1: preg, operand2: preg, dest: preg option, ccRef: ccRef option,
                            isAdd: bool, length: arithLength }

        (* Bitwise logical operations.  The immediate value must be a valid bit pattern.  ccRef can
           only be SOME if logOp is LogAnd. *)
    |   LogicalImmediate of { source: preg, dest: preg option, ccRef: ccRef option, immed: Word64.word,
                              logOp: logicalOp, length: arithLength }

        (* Register logical operations.  ccRef can only be SOME if logOp is LogAnd.*)
    |   LogicalRegister of { operand1: preg, operand2: preg, dest: preg option, ccRef: ccRef option,
                             logOp: logicalOp, length: arithLength }

        (* Start of function.  Set the register arguments.  stackArgs is the list of
           stack arguments.  If the function has a real closure regArgs includes the
           closure register (X8).  The register arguments include the return register
           (X30). *)
    |   BeginFunction of { regArgs: (preg * xReg) list, stackArgs: stackLocn list }

        (* Call a function.  If the code address is a constant it is passed here.
           Otherwise the address is obtained by indirecting through X8 which has been loaded
           as one of the argument registers.  The result is stored in the destination register. *)
    |   FunctionCall of
            { callKind: callKinds, regArgs: (fnarg * xReg) list,
              stackArgs: fnarg list, dest: preg, saveRegs: preg list}

        (* Jump to a tail-recursive function.  This is similar to FunctionCall
           but complicated for stack arguments because the stack and the return
           address need to be overwritten.
           stackAdjust is the number of words to remove (positive) or add
           (negative) to the stack before the call.
           currStackSize contains the number of items currently on the stack. *)
    |   TailRecursiveCall of
            { callKind: callKinds, regArgs: (fnarg * xReg) list,
              stackArgs: {src: fnarg, stack: int} list,
              stackAdjust: int, currStackSize: int }

        (* Return from the function.  resultReg is the preg that contains the result,
           returnReg is the preg that contains the return address. *)
    |   ReturnResultFromFunction of { resultReg: preg, returnReg: preg, numStackArgs: int }

        (* Raise an exception.  The packet is always loaded into X0. *)
    |   RaiseExceptionPacket of { packetReg: preg }

        (* Push a register to the stack.  This is used both for a normal push, copies=1, and
           also to reserve a container. *)
    |   PushToStack of { source: preg, copies: int, container: stackLocn }

        (* Load a register from the stack.  The container is the stack location identifier,
           the field is an offset in a container. *)
    |   LoadStack of { dest: preg, wordOffset: int, container: stackLocn, field: int }

        (* Store a value into the stack. *)
    |   StoreToStack of { source: preg, container: stackLocn, field: int, stackOffset: int }

        (* Set the register to the address of the container i.e. a specific offset on the stack. *)
    |   ContainerAddress of { dest: preg, container: stackLocn, stackOffset: int }

        (* Remove items from the stack.  Used to remove containers or
           registers pushed to the stack.. *)
    |   ResetStackPtr of { numWords: int }

        (* Destinations at the end of a basic block. *)
    and controlFlow =
        (* Unconditional branch to a label - should be a merge point. *)
        Unconditional of int
        (* Conditional branch. Jumps to trueJump if the condional is false, falseJump if false. *)
    |   Conditional of { ccRef: ccRef, condition: condition, trueJump: int, falseJump: int }
        (* Exit - the last instruction of the block is a return, raise or tailcall. *)
    |   ExitCode
        (* Indexed case - this branches to one of a number of labels *)
    |   IndexedBr of int list
        (* Set up a handler.  This doesn't cause an immediate branch but the state at the
           start of the handler is the state at this point. *)
    |   SetHandler of { handler: int, continue: int }
        (* Unconditional branch to a handler.  If an exception is raised explicitly
           within the scope of a handler. *)
    |   UnconditionalHandle of int
        (* Conditional branch to a handler.  Occurs if there is a call to a
           function within the scope of a handler.  It may jump to the handler. *)
    |   ConditionalHandle of { handler: int, continue: int }

    and basicBlock = BasicBlock of { block: arm64ICode list, flow: controlFlow }
    
    (* Return the successor blocks from a control flow. *)
    val successorBlocks: controlFlow -> int list

    val printICodeAbstract: basicBlock vector * (string -> unit) -> unit
    
    structure Sharing:
    sig
        type xReg           = xReg
        and  vReg           = vReg
        and  condition      = condition
        and  arm64ICode     = arm64ICode
        and  preg           = preg
        and  controlFlow    = controlFlow
        and  basicBlock     = basicBlock
        and stackLocn       = stackLocn
        and regProperty     = regProperty
        and ccRef           = ccRef
        and closureRef      = closureRef
        and loadType        = loadType
   end
end;
