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

functor Arm64ICodeConflicts(
    structure Arm64ICode: ARM64ICODE
    structure IntSet: INTSET
    structure Identify: ARM64IDENTIFYREFERENCES
    sharing Arm64ICode.Sharing = Identify.Sharing = IntSet
): ARM64ICODECONFLICTS =
struct
    open Arm64ICode
    open IntSet
    open Identify

    type conflictState =
    {
        conflicts: intSet, realConflicts: reg list
    }
    
    type triple = {instr: arm64ICode, current: intSet, active: intSet}

    exception InternalError = Misc.InternalError

    (* Get the conflict sets.  This code was originally part of identifyRegisterState and
       was split off. *)
    fun getConflictStates (blocks: extendedBasicBlock vector, maxPRegs) =
    let
        (* Other registers that conflict with this i.e. cannot share the same
           real register. *)
        val regConflicts = Array.array(maxPRegs, emptySet)
        (* Real registers that cannot be used for this because they are needed for
           an instruction.  Only X30 in calls and RTS traps. *)
        and regRealConflicts = Array.array(maxPRegs, []: reg list)
 
        fun addConflictsTo(addTo, conflicts) =
            List.app(fn aReg => Array.update(regConflicts, aReg, union(Array.sub(regConflicts, aReg), conflicts))) addTo

        (* To reserve a register we need to add the real register to the
           real conflict sets of all the abstract conflicts. *)
        local
            fun isInset reg set = List.exists (fn r => r = reg) set
        in
            fun reserveRegister(reserveFor, reg) =
            let
                fun reserveAReg r =
                let
                    val absConflicts = Array.sub(regConflicts, r)
                    fun addConflict i =
                        if isInset i reserveFor then () else addRealConflict (i, reg)
                in
                    List.app addConflict (setToList absConflicts)
                end
            in
                List.app reserveAReg reserveFor
            end
        
            and addRealConflict (i, reg) =
            let
                val currentConflicts = Array.sub(regRealConflicts, i)
            in
                if isInset reg currentConflicts
                then ()
                else Array.update(regRealConflicts, i, reg :: currentConflicts)
            end
        end
      
        fun conflictsForInstr passThrough {instr, current, ...} =
        let
            val {sources, dests} = getInstructionRegisters instr
            fun regNo(PReg i) = i
            val destRegNos = map regNo dests
            and sourceRegNos = map regNo sources
            val destSet = listToSet destRegNos
            val afterRemoveDests = minus(current, destSet)
            
            local
                (* In almost all circumstances the destination and sources don't
                   conflict and the same register can be used as a destination and
                   a source.  The exceptions are AllocateMemoryVariable and BoxValues
                   which can only store the size or the contents after the memory has
                   been allocated.  We also have to make sure that the test and
                   work registers are different in IndexedCase. *)
                val postInstruction =
                    case instr of
                        BoxLarge _ => destRegNos @ sourceRegNos
                    |   BoxTagFloat _ => destRegNos @ sourceRegNos (* Not sure about this. *)
                    |   _ => destRegNos
            in
                (* If there is more than one destination they conflict with each other. *)
                val () = addConflictsTo(postInstruction, listToSet postInstruction);
                (* Mark conflicts for the destinations, i.e. after the instruction.
                   The destinations conflict with the registers that are used
                   subsequently. *)
                val () = addConflictsTo(postInstruction, current);
                val () = addConflictsTo(postInstruction, passThrough);

                (* Mark conflicts for the sources i.e. before the instruction. *)
                (* Sources must be set up as conflicts with each other i.e. when we
                   come to allocate registers we must choose different real registers
                   for different abstract registers. *)
                val () = addConflictsTo(sourceRegNos, listToSet sourceRegNos)
                val () = addConflictsTo(sourceRegNos, afterRemoveDests);
                val () = addConflictsTo(sourceRegNos, passThrough)
            end
            
            (* I'm not sure if this is needed.  There was a check in the old code to ensure that
               different registers were used for loop variables even if they were actually unused.
               This may happen anyway.
               Comment and code copied from X86 version.  Retain it for the moment. *)
            val () =
                case instr of
                    JumpLoop{regArgs, ...} =>
                    let
                        val destRegs = List.foldl(fn ({dst=PReg loopReg, ...}, dests) => loopReg :: dests) [] regArgs
                    in
                        addConflictsTo(destRegs, listToSet destRegs);
                        addConflictsTo(destRegs, current);
                        addConflictsTo(destRegs, passThrough)
                    end
                |   _ => ()

            (* Certain instructions are specific as to the real registers. *)
            val () =
                case instr of
                    ReturnResultFromFunction{ returnReg=PReg retReg, ... } =>
                        (* We're going to put the return value in X0 so we can't use that for
                           the return address. *)
                        addRealConflict(retReg, GenReg X0)
                
                |   FunctionCall { dest, regArgs, ...} =>
                        (* This is only needed if we are saving the registers rather
                           than marking them as "must push". *)
                    (
                        reserveRegister([regNo dest], GenReg X0);
                        (* The argument registers also conflict.  In order to execute this call we need to load
                           the arguments into specific registers so we can't use them for values that we want after
                           the call.   We use regNo dest here because that will conflict with everything
                           immediately afterwards. *)
                        List.app(fn (_, r) => reserveRegister([regNo dest], GenReg r)) regArgs
                    )

                |   _ => ()
        in
            ()
        end
        
        (* Process the block. *)
        fun conflictsForBlock(ExtendedBasicBlock{block, passThrough, exports, ...}) =
        let
            (* We need to establish conflicts between all the registers active at
               the end of the block since they may not be established elsewhere.
               This isn't necessary for an unconditional branch since the
               same registers will be included in the block that is the target
               of the branch, possibly along with others.  However if this is
               a conditional or indexed branch we may have different sets at
               each of the targets and we have to ensure that all the registers
               differ. *)
            val united = union(exports, passThrough)
            val () = addConflictsTo(setToList united, united)

            val () = List.app (conflictsForInstr passThrough) block
        in
            ()
        end
        
        
        val () = Vector.app conflictsForBlock blocks
        
        val conflictState: conflictState vector =
            Vector.tabulate(maxPRegs,
                fn i => {
                    conflicts = Array.sub(regConflicts, i),
                    realConflicts = Array.sub(regRealConflicts, i)
                }
            )
    in
        conflictState
    end
 
    structure Sharing =
    struct
        type arm64ICode = arm64ICode
        and reg = reg
        and preg = preg
        and intSet = intSet
        and extendedBasicBlock = extendedBasicBlock
    end
end;
