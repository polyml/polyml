(*
    Copyright David C. J. Matthews 2017

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

signature X86PUSHREGISTERSIG =
sig
    type abstract
    type 'reg x86ICode
    type intSet

    val addRegisterPushes :
        {instr: abstract x86ICode, active: intSet, current: intSet} list * bool vector * int * int ->
            abstract x86ICode list * int * int * int

    structure Sharing:
    sig
        type 'reg x86ICode = 'reg x86ICode
        and abstract = abstract
        and intSet = intSet
    end
end;
