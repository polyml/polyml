(*
    Copyright (c) 2017 David C.J. Matthews

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

signature X86GETCONFLICTSETSIG =
sig
    type 'reg x86ICode and reg and abstract and blockSource and controlFlow
    
    type intSet
    
    type conflictState =
    {
        conflicts: intSet, realConflicts: reg list
    }

    type extendedBasicBlock =
    {
        block: {instr: abstract x86ICode, current: intSet, active: intSet } list,
        source: blockSource,
        flow: controlFlow,
        (*
        locals: intSet, (* Defined and used entirely within the block. *)
        imports: intSet, (* Defined outside the block, used inside it, but not needed afterwards. *)
        exports: intSet, (* Defined within the block, possibly used inside, but used outside. *)
        passThrough: intSet, (* Active throughout the block. May be referred to by it but needed afterwards. *)
        *)
        exportPassThrough: intSet (* Used after the block but we don't know yet where defined. *)
    }
   
    val getConflictStates: extendedBasicBlock vector * int -> conflictState vector

    structure Sharing:
    sig
        type 'reg x86ICode = 'reg x86ICode
        and reg = reg
        and abstract = abstract
        and intSet = intSet
        and blockSource = blockSource
        and controlFlow = controlFlow
    end;
end;
