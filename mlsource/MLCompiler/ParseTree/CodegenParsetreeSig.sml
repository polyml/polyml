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

signature CodegenParsetreeSig =
sig
    type parsetree
    type lexan
    type level
    type typeVarMap
    type codetree
    type codeBinding
    type environEntry

    type debugenv = environEntry list * (level->codetree)

    val gencode:
        parsetree * lexan * debugenv * level * (int->int) * typeVarMap * string *
            (codeBinding list * debugenv * typeVarMap -> codeBinding list * debugenv)
            -> codeBinding list * debugenv

    structure Sharing:
    sig
        type parsetree = parsetree
        and  lexan = lexan
        and  codetree = codetree
        and  environEntry = environEntry
        and  level = level
        and  typeVarMap = typeVarMap
        and  codeBinding = codeBinding
    end
end
