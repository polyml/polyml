(*
    Copyright (c) 2021 David C.J. Matthews

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

functor Arm64IdentifyReferences(
    structure Arm64ICode: ARM64ICODE
    structure Debug: DEBUG
    structure IntSet: INTSET
): ARM64IDENTIFYREFERENCES =
struct
    open Arm64ICode
    open IntSet

    type regState =
    { 
        active: int, refs: int, pushState: bool, prop: regProperty
    }

    (* CC states before and after.  The instruction may use the CC or ignore it. The only
       instructions to use the CC is X87FPGetCondition.  Conditional branches
       are handled at the block level.
       The result of executing the instruction may be to set the condition code to a
       defined state, an undefined state or leave it unchanged.
       N.B. Some "instructions" may involve a stack reset that could affect the CC. *)
    datatype outCCState = CCSet of ccRef | CCIndeterminate | CCUnchanged
    and inCCState = CCNeeded of ccRef | CCUnused
    
    datatype extendedBasicBlock =
        ExtendedBasicBlock of
        {
            block: {instr: arm64ICode, current: intSet, active: intSet, kill: intSet } list,
            flow: controlFlow,
            locals: intSet, (* Defined and used entirely within the block. *)
            imports: intSet, (* Defined outside the block, used inside it, but not needed afterwards. *)
            exports: intSet, (* Defined within the block, possibly used inside, but used outside. *)
            passThrough: intSet, (* Active throughout the block. May be referred to by it but needed afterwards. *)
            loopRegs: intSet, (* Destination registers for a loop.  They will be updated by this block. *)
            initialStacks: intSet, (* Stack items required at the start i.e. imports+passThrough for stack items. *)
            inCCState: ccRef option, (* The state this block assumes.  If SOME _ all predecessors must set it. *)
            outCCState: ccRef option (* The condition code set by this block.  SOME _ if at least one successor needs it. *)
        }
    
    exception InternalError = Misc.InternalError
    
    fun identifyRegisters _ = raise Fallback "identifyRegisters"
    
    (* Get the registers for an instruction.  *)
    fun getInstructionRegisters _ = raise Fallback "getInstructionRegisters"
    fun getInstructionCC _ = raise Fallback "getInstructionCC"


    structure Sharing =
    struct
        type arm64ICode         = arm64ICode
        and preg                = preg
        and intSet              = intSet
        and basicBlock          = basicBlock
        and extendedBasicBlock  = extendedBasicBlock
        and controlFlow         = controlFlow
        and regProperty         = regProperty
        and ccRef               = ccRef
        and outCCState          = outCCState
    end
end;
