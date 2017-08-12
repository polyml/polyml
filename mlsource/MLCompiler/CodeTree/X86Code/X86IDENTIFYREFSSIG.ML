(*
    Copyright (c) 2016-17 David C.J. Matthews

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

signature X86IDENTIFYREFSSIG =
sig
    type 'reg x86ICode and reg and abstract and 'reg basicBlock and controlFlow

    datatype pushState = MustPush | MustNotPush | MayPush
    
    type intSet

    type regState =
    { 
        active: int, refs: int, pushState: pushState
    }
    
    (* Simple basic block with register usage information. *)
    datatype extendedBasicBlock =
        ExtendedBasicBlock of
        {
            block: {instr: abstract x86ICode, current: intSet, active: intSet } list,
            flow: controlFlow,
            locals: intSet, (* Defined and used entirely within the block. *)
            imports: intSet, (* Defined outside the block, used inside it, but not needed afterwards. *)
            exports: intSet, (* Defined within the block, possibly used inside, but used outside. *)
            passThrough: intSet (* Active throughout the block. May be referred to by it but needed afterwards. *)
        }

    val identifyRegisters: abstract basicBlock vector * int -> extendedBasicBlock vector * regState vector

    structure Sharing:
    sig
        type 'reg x86ICode = 'reg x86ICode
        and reg = reg
        and pushState = pushState
        and abstract = abstract
        and intSet = intSet
        and 'reg basicBlock = 'reg basicBlock
        and extendedBasicBlock = extendedBasicBlock
        and controlFlow = controlFlow
    end;
end;
