(*
    Copyright David C. J. Matthews 2016-18

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

signature X86ICODETRANSFORMSIG =
sig
    type reg and preg and basicBlock and regProperty
    type closureRef

    val codeICodeFunctionToX86: {blocks: basicBlock vector, functionName: string, pregProps: regProperty vector,
            argRegsUsed: reg list, hasFullClosure: bool, currentStackArgs: int, ccCount: int,
            debugSwitches: Universal.universal list, resultClosure: closureRef } -> unit

    structure Sharing:
    sig
        type preg       = preg
        and reg         = reg
        and basicBlock  = basicBlock
        and regProperty = regProperty
        and closureRef  = closureRef
    end
end;
