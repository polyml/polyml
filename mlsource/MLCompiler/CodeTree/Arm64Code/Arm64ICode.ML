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

functor Arm64ICode(

    structure Arm64Code: ARM64ASSEMBLY

): ARM64ICODE =

struct
    open Arm64Code
    
    exception Fallback of string (* Temporarily for development. *)

    open Address
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

    datatype arithLength = Arith64 | Arith32

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

        (* Add/Subtract immediate.  The destination is optional in which case XZero is used.
           ccRef is optional.  If it is NONE the version of the instruction that does not generate
           a condition code is used. immed must be < 0wx1000. *)
    |   AddSubImmediate of { source: preg, dest: preg option, ccRef: ccRef option, immed: word,
                             isAdd: bool, length: arithLength }

        (* Add/Subtract register.  As with AddSubImmediate, both the destination and cc are optional. *)
    |   AddSubRegister of { operand1: preg, operand2: preg, dest: preg option, ccRef: ccRef option,
                            isAdd: bool, length: arithLength }

        (* Start of function.  Set the register arguments.  stackArgs is the list of
           stack arguments.  If the function has a real closure regArgs includes the
           closure register (X8).  The register arguments include the return register
           (X30). *)
    |   BeginFunction of { regArgs: (preg * xReg) list, stackArgs: stackLocn list }

        (* Return from the function.  resultReg is the preg that contains the result,
           returnReg is the preg that contains the return address. *)
    |   ReturnResultFromFunction of { resultReg: preg, returnReg: preg, numStackArgs: int }

        (* Raise an exception.  The packet is always loaded into X0. *)
    |   RaiseExceptionPacket of { packetReg: preg }

        (* Remove items from the stack.  Used to remove containers or
           registers pushed to the stack.. *)
    |   ResetStackPtr of { numWords: int }

        (* Destinations at the end of a basic block. *)
    and controlFlow =
        (* Unconditional branch to a label - should be a merge point. *)
        Unconditional of int
        (* Conditional branch. Jumps to trueJump if the condional is true, falseJump if false. *)
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

    (* Return the list of blocks that are the immediate successor of this. *)
    fun successorBlocks(Unconditional l) = [l]
    |   successorBlocks(Conditional{trueJump, falseJump, ...}) = [trueJump, falseJump]
    |   successorBlocks ExitCode = []
    |   successorBlocks(IndexedBr cases) = cases
    |   successorBlocks(SetHandler{handler, continue, ...}) = [handler, continue]
        (* We only need "handler" in SetHandler because we may have a handler that is never actually jumped to. *)
    |   successorBlocks(UnconditionalHandle handler) = [handler]
    |   successorBlocks(ConditionalHandle{handler, continue, ...}) = [handler, continue]

    local
        fun printReg(PReg i, stream) = stream("R" ^ Int.toString i)
        and printCC(CcRef ccRef, stream) = stream ("CC" ^ Int.toString ccRef)
         
        fun printStackLoc(StackLoc{size, rno}, stream) =
            (stream "S"; stream(Int.toString rno); stream "("; stream(Int.toString size); stream ")")
        
        fun regRepr(XReg w) = "X" ^ Int.toString(Word8.toInt w)
        |   regRepr XZero = "XZ"
        |   regRepr XSP = "SP"

        fun arithRepr Arith64 = "64"
        |   arithRepr Arith32 = "32"
        
        fun printLoadType(Load64, stream) = stream "64"
        |   printLoadType(Load32, stream) = stream "32"
        |   printLoadType(Load16, stream) = stream "16"
        |   printLoadType(Load8, stream) = stream "8"
   
        fun printICode(MoveRegister{ source, dest }, stream) =
            (
                stream "\tMove\t";
                printReg(source,  stream);
                stream " => ";
                printReg(dest, stream)
            )

        |   printICode(LoadNonAddressConstant{ source, dest }, stream) =
            (
                stream "\tLoadNonAddress\t"; stream(Word64.toString source);
                stream " => ";
                printReg(dest, stream)
            )

        |   printICode(LoadAddressConstant{ source, dest }, stream) =
            (
                stream "\tLoadAddress\t"; stream(Address.stringOfWord source);
                stream " => ";
                printReg(dest, stream)
            )

        |   printICode(LoadWithConstantOffset{ base, dest, byteOffset, loadType }, stream) =
            (
                stream "\tLoadConstOffset"; printLoadType(loadType, stream); stream "\t";
                printReg(base, stream); stream "+";
                stream(Int.toString byteOffset);
                stream " => ";
                printReg(dest, stream)
            )

        |   printICode(LoadWithIndexedOffset{ base, dest, index, loadType }, stream) =
            (
                stream "\tLoadIndexed"; printLoadType(loadType, stream); stream "\t";
                printReg(base, stream); stream "+"; printReg(index, stream);
                stream " => "; printReg(dest, stream)
            )

        |   printICode(ObjectIndexAddressToAbsolute{ source, dest }, stream) =
            (
                stream "\tObjectAddrToAbs\t";
                printReg(source, stream); stream " => "; printReg(dest, stream)
            )

        |   printICode(AddSubImmediate{ source, dest, ccRef, immed, isAdd, length }, stream) =
            (
                stream (if isAdd then "\tAddImmediate" else "\tSubImmediate"); stream(arithRepr length);
                stream "\t"; printReg(source, stream); stream ",0x"; stream(Word.toString immed);
                stream " => "; case dest of NONE => stream "_" | SOME reg => printReg(reg, stream);
                stream ", ";
                case ccRef of NONE => stream "_" | SOME cc => printCC(cc, stream)
            )

        |   printICode(AddSubRegister{ operand1, operand2, dest, ccRef, isAdd, length }, stream) =
            (
                stream (if isAdd then "\tAddRegister" else "\tSubRegister"); stream(arithRepr length);
                stream "\t"; printReg(operand1, stream); stream ", ";
                printReg(operand2, stream);
                stream " => "; case dest of NONE => stream "_" | SOME reg => printReg(reg, stream);
                stream ", ";
                case ccRef of NONE => stream "_" | SOME cc => printCC(cc, stream)
            )

        |   printICode(BeginFunction{ regArgs, stackArgs }, stream) =
            (
                stream "\tBeginFunction\t";
                List.app(fn (arg, r) => (stream(regRepr r); stream "="; printReg(arg, stream); stream " ")) regArgs;
                List.app(fn s => printStackLoc(s, stream)) stackArgs
            )

        |   printICode(ReturnResultFromFunction{ resultReg, returnReg, numStackArgs }, stream) =
            (
                stream "\tReturnFromFunction\t";
                printReg(returnReg, stream); stream " with "; printReg(resultReg, stream);
                stream("," ^ Int.toString numStackArgs)
            )

        |   printICode(RaiseExceptionPacket{ packetReg }, stream) =
            ( stream "\tRaiseException\t"; printReg(packetReg, stream) )

        |   printICode(ResetStackPtr{ numWords }, stream) =
            ( stream "\tResetStackPtr\t"; stream(Int.toString numWords) )

        and printCondition(cond,  stream)  = stream(condToString cond)

        (* Print a basic block. *)
        fun printBlock stream (blockNo, BasicBlock{block, flow, ...}) =
        (
            (* Put a label on all but the first. *)
            if blockNo <> 0 then stream("L" ^ Int.toString blockNo ^ ":") else ();
            List.app (fn icode => (printICode(icode, stream); stream "\n")) block;
            case flow of
                Unconditional l => stream("\tJump\tL" ^ Int.toString l ^ "\n")
            |   Conditional {condition, trueJump, falseJump, ccRef, ...} =>
                (
                    stream "\tJump"; printCondition(condition, stream);
                    stream "\t"; printCC(ccRef, stream);
                    stream " L"; stream (Int.toString trueJump);
                    stream " else L"; stream (Int.toString falseJump); stream "\n"
                )
            |   ExitCode => ()
            |   IndexedBr _ => ()
            |   SetHandler{handler, continue} =>
                    stream(concat["\tSetHandler\tH", Int.toString handler, "\n",
                                  "\tJump\tL", Int.toString continue, "\n"])
            |   UnconditionalHandle handler => stream("\tJump\tH" ^ Int.toString handler ^ "\n")
            |   ConditionalHandle{handler, continue} =>
                    stream(concat["\tJump\tL", Int.toString continue, " or H", Int.toString handler, "\n"])
        )
    in
        fun printICodeAbstract(blockVec, stream) =
            Vector.appi(printBlock stream) blockVec
    end

    structure Sharing =
    struct
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

