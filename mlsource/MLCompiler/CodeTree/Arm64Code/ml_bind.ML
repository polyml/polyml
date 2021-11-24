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
        Arm64Assembly (
            structure Debug = Debug
            and       Pretty = Pretty
            and       CodeArray = CodeArray
        )
    
    structure Arm64Sequences =
        Arm64Sequences (
            structure Arm64Assembly = Arm64Assembly
        )

    structure Arm64Foreign =
        Arm64ForeignCall (
            structure CodeArray   = CodeArray
            and       Arm64Assembly = Arm64Assembly
            and       Arm64Sequences = Arm64Sequences
            and       Debug = Debug
        )

    structure Arm64ICode =
        Arm64ICode (
            structure Arm64Code       = Arm64Assembly
        )

    structure Arm64ICodeIdentify =
        Arm64IdentifyReferences (
            structure Debug         = Debug
            structure Arm64ICode         = Arm64ICode
            structure IntSet        = IntSet
        )

    structure Arm64ICodeConflicts =
        Arm64ICodeConflicts (
            structure Arm64ICode         = Arm64ICode
            structure IntSet        = IntSet
            structure Identify      = Arm64ICodeIdentify
        )

    structure Arm64PushRegs =
        Arm64PushRegisters (
            structure Arm64ICode         = Arm64ICode
            structure IntSet        = IntSet
            structure Identify      = Arm64ICodeIdentify
        )

    structure Arm64Opt =
        Arm64ICodeOptimise (
            structure Arm64ICode    = Arm64ICode
            structure IntSet        = IntSet
            structure Identify      = Arm64ICodeIdentify
            structure Debug         = Debug
            structure Pretty        = Pretty
        )

    structure Arm64IAllocate =
        Arm64AllocateRegisters (
            structure Arm64ICode    = Arm64ICode
            structure Identify      = Arm64ICodeIdentify
            structure ConflictSets  = Arm64ICodeConflicts
            structure IntSet        = IntSet
        )

    structure Arm64ICodeGenerate =
        Arm64ICodeToArm64Code (
            structure Debug         = Debug
            structure Arm64ICode    = Arm64ICode
            structure Identify      = Arm64ICodeIdentify
            structure Pretty        = Pretty
            structure IntSet        = IntSet
            structure Arm64Assembly = Arm64Assembly
            structure Arm64Sequences = Arm64Sequences
            structure Strongly      = StronglyConnected
        )
        
    structure Arm64ICodeTransform =
        Arm64ICodeTransform (
            structure Debug         = Debug
            structure Arm64ICode    = Arm64ICode
            structure Identify      = Arm64ICodeIdentify
            structure ConflictSets  = Arm64ICodeConflicts
            structure Allocate      = Arm64IAllocate
            structure PushRegisters = Arm64PushRegs
            structure Optimise      = Arm64Opt
            structure Pretty        = Pretty
            structure IntSet        = IntSet
            structure Codegen       = Arm64ICodeGenerate
        )

    structure Arm64Fallback =
        Arm64GenCode(
            structure BackendTree = BackendIntermediateCode
            and       CodeArray   = CodeArray
            and       Arm64Assembly = Arm64Assembly
            and       Arm64Foreign = Arm64Foreign
            and       Arm64Sequences = Arm64Sequences
            and       Debug = Debug
        )

in

    structure Arm64Code = 
        Arm64CodetreeToICode (
            structure BackendTree   = BackendIntermediateCode
            structure Debug         = Debug
            structure Arm64ICode         = Arm64ICode
            structure Arm64Foreign  = Arm64Foreign
            structure ICodeTransform = Arm64ICodeTransform
            structure CodeArray    = CodeArray
            
            structure Arm64Fallback = Arm64Fallback
        )

end;
