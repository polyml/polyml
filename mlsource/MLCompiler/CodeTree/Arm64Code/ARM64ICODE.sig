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
    
    val is32in64: bool
    
    type condition
    (* Condition for conditional branches etc. *)
    val condEqual: condition
    and condNotEqual: condition
    and condCarrySet: condition
    and condCarryClear: condition
    and condNegative: condition
    and condPositive: condition
    and condOverflow: condition
    and condNoOverflow: condition
    and condUnsignedHigher: condition
    and condUnsignedLowOrEq: condition
    and condSignedGreaterEq: condition
    and condSignedLess: condition
    and condSignedGreater: condition
    and condSignedLessEq: condition

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

    datatype arm64ICode =
        PlaceHolder

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
   end
end;
