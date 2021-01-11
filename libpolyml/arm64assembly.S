//
//  Assembly code for the ARM64 for Poly/ML
//  Author:    David Matthews
//  Copyright (c) David C. J. Matthews 2021
//
//  This library is free software; you can redistribute it and/or
//  modify it under the terms of the GNU Lesser General Public
//  License version 2.1 as published by the Free Software Foundation.
//  
//  This library is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//  Lesser General Public License for more details.
//  
//  You should have received a copy of the GNU Lesser General Public
//  License along with this library// if not, write to the Free Software
//  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

// The syntax of directives in the GNU assembler and in the MS ARMASM
// are somewhat different.  ARMASMSYNTAX is defined in the VS project files.

#ifdef ARMASMSYNTAX
#define LABEL(x) x
#else
#define LABEL(x) x:
#endif

#ifdef ARMASMSYNTAX
    AREA |.text|, CODE, READONLY
#else
    .section .text,"x"
    .balign 4
#endif

// POLYUNSIGNED Arm64AsmAtomicExchangeAndAdd(PolyObject*, POLYSIGNED);
// This is not actually used with the VS build.
#ifdef ARMASMSYNTAX
    EXPORT Arm64AsmAtomicExchangeAndAdd
Arm64AsmAtomicExchangeAndAdd PROC
#else
    .global Arm64AsmAtomicExchangeAndAdd
Arm64AsmAtomicExchangeAndAdd:
#endif

// The easiest way to do this is with ldaddal but that is only available
// in ARM 8.1 and above.  For the moment we use the old version.
//  ldaddal x1,x0,[x0]
LABEL(aaea1)
    ldaxr   x3,[x0]
    add     x2,x3,x1
    stlxr   w4,x2,[x0]
    cbnz    w4,aaea1
    dmb     ish
    mov     x0,x3
    ret

#ifdef ARMASMSYNTAX
    ENDP
    END
#endif
