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
    
    fun addRegisterPushes _ = raise Fallback "addRegisterPushes"

    structure Sharing =
    struct
        type extendedBasicBlock = extendedBasicBlock
        and basicBlock = basicBlock
        and regProperty = regProperty
    end
end;
