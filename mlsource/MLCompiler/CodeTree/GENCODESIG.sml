(*
    Copyright (c) 2016 David C.J. Matthews

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

signature GENCODESIG =
sig
    type backendIC and argumentType and machineWord
    type bicLambdaForm =
    {
        body          : backendIC,
        name          : string,
        closure       : backendIC list,
        argTypes      : argumentType list,
        resultType    : argumentType,
        closureRefs   : int,
        localCount    : int,
        heapClosure   : bool,
        argLifetimes  : int list
    }
    val gencodeLambda: bicLambdaForm * Universal.universal list * Address.address -> unit
   
    structure Foreign: FOREIGNCALLSIG

    structure Sharing: sig type backendIC = backendIC and argumentType = argumentType end
end;
