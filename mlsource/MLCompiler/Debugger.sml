(*
    Title:      Source level debugger for Poly/ML
    Author:     David Matthews
    Copyright  (c)   David Matthews 2000

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

structure Debugger =
    DEBUGGER_(
        structure STRUCTVALS = StructVals
        structure CODETREE = CodeTree
        structure VALUEOPS = ValueOps
        structure TYPETREE = TypeTree
        structure ADDRESS = Address
        structure COPIER = CopierStruct
        structure TYPEIDCODE = TypeIDCodeStruct
        structure LEX = Lex
        structure UTILITIES  = Utilities
        structure DEBUG = Debug
    );
