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

functor Arm64PushRegisters(
    structure Arm64ICode: ARM64ICODE
    structure IntSet: INTSET
    structure Identify: ARM64IDENTIFYREFERENCES
    sharing Arm64ICode.Sharing = Identify.Sharing = IntSet
) : ARM64PUSHREGISTERS
=
struct
    open Arm64ICode
    open IntSet
    open Identify
    
    (* Curried subscript functions *)
    fun asub a i = Array.sub(a, i)
    and vsub v i = Vector.sub(v, i)
    
    exception InternalError = Misc.InternalError

    (* Each preg in the input is mapped to either a new preg or the stack. *)
    datatype pregMapType = Unset | ToPReg of preg | ToStack of int * stackLocn

    (* The stack contains both entries in the input code and entries added here.
       It is really used to ensure that the stack at run time is the same size
       at the start of a block whichever block has jumped to it. *)
    datatype stackEntry =
        NewEntry of {pregNo: int} (* pregNo is the original preg that has been pushed here. *)
    |   OriginalEntry of { stackLoc: stackLocn }
    |   HandlerEntry

    fun addRegisterPushes { code, pregProps, ...} =
    let
        fun toBasic (ExtendedBasicBlock{ block, flow, ...}) =
            BasicBlock{ block = map (fn { instr, ...} => instr) block, flow = flow }
    in
        { code = Vector.map toBasic code, pregProps=pregProps, maxStack = 0}
    end


    structure Sharing =
    struct
        type extendedBasicBlock = extendedBasicBlock
        and basicBlock = basicBlock
        and regProperty = regProperty
    end
end;
