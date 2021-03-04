(*
    Copyright (c) 2021 David C. J. Matthews

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    Licence version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public Licence for more details.
    
    You should have received a copy of the GNU Lesser General Public
    Licence along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

local
    structure Arm64Assembly =
        ARM64ASSEMBLY (
            structure Debug = Debug
            and       Pretty = Pretty
            and       CodeArray = CODE_ARRAY
        )

    structure Arm64Foreign =
        Arm64ForeignCall (
            structure CodeArray   = CODE_ARRAY
            and       Arm64Assembly = Arm64Assembly
            and       Debug = Debug
        )
in
    structure Arm64Code =
        Arm64GenCode(
            structure BackendTree = BackendIntermediateCode
            and       CodeArray   = CODE_ARRAY
            and       Arm64Assembly = Arm64Assembly
            and       Arm64Foreign = Arm64Foreign
            and       Debug = Debug
        )
end;
