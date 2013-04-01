(*
    Copyright (c) 2013 David C.J. Matthews

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

signature MatchCompilerSig =
sig
    type parsetree
    type typeVarMap
    type level
    type codetree
    type matchtree
    type codeBinding
    type environEntry
    type lexan

    type location =
        { file: string, startLine: int, startPosition: int, endLine: int, endPosition: int }
    and debugEnv = environEntry list * (level->codetree)

    type cgContext =
        {
            decName: string, debugEnv: debugEnv, mkAddr: int->int,
            level: level, typeVarMap: typeVarMap, lex: lexan, lastDebugLine: int ref,
            isOuterLevel: bool
        }

    val codeMatchPatterns:
        matchtree list * codetree * bool * location * (int * cgContext -> codetree) * cgContext -> codetree * bool

    val codeBindingPattern:
        parsetree * codetree * location * cgContext -> codeBinding list * bool

    structure Sharing:
    sig
        type parsetree = parsetree
        type typeVarMap = typeVarMap
        type level = level
        type codetree = codetree
        type matchtree = matchtree
        type codeBinding = codeBinding
        type environEntry = environEntry
        type lexan = lexan
    end
end;
