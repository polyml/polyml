;
;  Title:  Assembly code routines for the poly system.
;  Author:    David Matthews
;  Copyright (c) David C. J. Matthews 2000-2016
;
;  This library is free software; you can redistribute it and/or
;  modify it under the terms of the GNU Lesser General Public
;  License version 2.1 as published by the Free Software Foundation.
;  
;  This library is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;  Lesser General Public License for more details.
;  
;  You should have received a copy of the GNU Lesser General Public
;  License along with this library; if not, write to the Free Software
;  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;

;
; Registers used :-
;
;  rax:  First argument to function.  Result of function call.
;  rbx:  Second argument to function.
;  rcx:  General register
;  rdx:  Closure pointer in call.
;  rbp:  Points to memory used for extra registers
;  rsi:  General register.
;  rdi:  General register.
;  rsp:  Stack pointer.
;  r8:   Third argument to function
;  r9:   Fourth argument to function
;  r10:  Fifth argument to function
;  r11:  General register
;  r12:  General register
;  r13:  General register
;  r14:  General register
;  r15:  Memory allocation pointer

; Include RTS call numbers
#include "sys.h"

;
; Tagged values.   A few operations, such as shift assume that the tag bit
; is the bottom bit.
;
#define TAG         1
#define TAGSHIFT    1
#define TAGGED(i)   (i*2+1)

#define POLYWORDSIZE    8

#define NIL     TAGGED(0)
#define UNIT    TAGGED(0)
#define ZERO    TAGGED(0)
#define FALSE   TAGGED(0)
#define TRUE    TAGGED(1)
#define MINUS1  TAGGED(0-1)

#define B_bytes         01h
#define B_typeBits      03h
#define B_mutable       40h
#define Max_Length      00ffffffffffffffh              

; The "stack frame" pointed to by rbp acts as the "memory registers".
Fr_RaiseOverflow    EQU     -120

Fr_HeapOverflow     EQU     -104
Fr_StackOverflow    EQU     -96
Fr_StackOverflowEx  EQU     -88
Fr_HandlerRegister  EQU     -80
Fr_LocalMbottom     EQU     -72
Fr_StackLimit       EQU     -64
Fr_Argument         EQU     16

; Extra entries on the C stack
Fr_SavedMLSP        EQU     -128

Fr_Size             EQU     144         ; Must be multiple of 16 to get alignment correct

; This is the argument vector passed in to X86AsmSwitchToPoly
; It is used to initialise the frame.  A few values are updated
; when ML returns.
ArgVector STRUCT
LocalMPointer       QWORD   ?
HandlerRegister     QWORD   ?
LocalMbottom        QWORD   ?
StackLimit          QWORD   ?
ExceptionPacket     QWORD   ?       ; Address of packet to raise
UnusedRequestCode   DB      ?       ; Byte: Io function to call.
UnusedFlag          DB      ?
ReturnReason        DB      ?       ; Byte: Reason for returning from ML.
FullRestore         DB      ?       ; Byte: Full/partial restore
UnusedAlign         DWORD   ?
UnusedPStack        QWORD   ?       ; Current stack base
ThreadId            QWORD   ?       ; My thread id
StackPtr            QWORD   ?       ; Stack pointer
ProgramCtr          QWORD   ?
SaveRAX             QWORD   ?
SaveRBX             QWORD   ?
SaveRCX             QWORD   ?
SaveRDX             QWORD   ?
SaveRSI             QWORD   ?
SaveRDI             QWORD   ?
SaveR8              QWORD   ?
SaveR9              QWORD   ?
SaveR10             QWORD   ?
SaveR11             QWORD   ?
SaveR12             QWORD   ?
SaveR13             QWORD   ?
SaveR14             QWORD   ?
SaveXMM0            QWORD   ?
SaveXMM1            QWORD   ?
SaveXMM2            QWORD   ?
SaveXMM3            QWORD   ?
SaveXMM4            QWORD   ?
SaveXMM5            QWORD   ?
SaveXMM6            QWORD   ?
ArgVector ENDS

RETURN_HEAP_OVERFLOW        EQU 1
RETURN_STACK_OVERFLOW       EQU 2
RETURN_STACK_OVERFLOWEX     EQU 3
RETURN_CALLBACK_RETURN      EQU 6
RETURN_CALLBACK_EXCEPTION   EQU 7
RETURN_RAISE_OVERFLOW       EQU 8
RETURN_KILL_SELF            EQU 9

;
; CODE STARTS HERE
;
    .CODE

; Define standard call macro.

CALL_EXTRA  MACRO   index
    push    rcx
    mov     rcx,[Fr_Argument+rbp]
    mov     byte ptr [ArgVector.ReturnReason+rcx],index
    pop     rcx
    jmp     SaveFullState
ENDM

#if (0)
PRERTSCALL MACRO entryName
; The initial sequence - this is standard for all calls.
entryName PROC FRAME
    .PUSHREG rbp                    ; Fake the frame.  It has been set up in X86AsmSwitchToPoly
    .SETFRAME rbp,0
    .PUSHREG rbx
    .PUSHREG r12
    .PUSHREG r13
    .PUSHREG r14
    .PUSHREG r15
    .PUSHREG rdi
    .PUSHREG rsi
    .endprolog
    mov     r12,[Fr_Argument+rbp]                           ; Load arg into r12 - saved across call
    mov     qword ptr [ArgVector.ExceptionPacket+r12],1           ; Clear exception
    mov     [ArgVector.LocalMpointer+r12],r15                     ; Save heap pointer
    mov     r13,rsp                                         ; Save ML stack ptr in r13 - also saved
    mov     rsp,rbp                                         ; Switch to C stack
    sub     rsp,Fr_size
    ; X86-64 Windows calling conventions.
    mov     rcx,[ArgVector.ThreadId+r12]                          ; First argument - thread Id
ENDM

POSTRTSCALL MACRO  entryName,rtsName
; This is the second part after the arguments have been set up
    sub     rsp,32                                          ; Create save area
EXTRN   rtsName:PROC
    call    rtsName
    mov     rsp,r13                                         ; Restore the ML stack ptr
    mov     r15,[ArgVector.LocalMpointer+r12]                     ; Update heap pointer
    mov     rdx,[ArgVector.LocalMbottom+r12]
    mov     [Fr_LocalMbottom+rbp],rdx                       ; and heap base because we may have GCd
    cmp     qword ptr [ArgVector.ExceptionPacket+r12],1
    jne     rtsraisex
    ret
rtsraisex:
    mov     rax,[ArgVector.ExceptionPacket+r12]
    jmp     raisex
entryName ENDP
ENDM

CALL_RTS1 MACRO entryName,rtsName
; Macro to call a function with a single argument.  The RTS function is called with two
; arguments, the first being the thread ID.  That can be used to find the taskData pointer.
    PRERTSCALL entryName
    mov     rdx,rax                                         ; Second argument
    POSTRTSCALL entryName,rtsName
ENDM

CALL_RTS2 MACRO entryName,rtsName
; Macro to call a function with two arguments.
    PRERTSCALL entryName
    mov     rdx,rax                                         ; Second argument
    mov     r8,rbx
    POSTRTSCALL entryName,rtsName
ENDM

CALL_RTS3 MACRO entryName,rtsName
; Macro to call a function with three arguments.
    PRERTSCALL entryName
    mov     rdx,rax                                         ; Second argument
    mov     r9,r8                                           ; Fourth argument
    mov     r8,rbx                                          ; Third argument
    POSTRTSCALL entryName,rtsName
ENDM
#endif

; Load the registers from the ML stack and jump to the code.
; This is used to start ML code.
; The argument is the address of the MemRegisters struct and goes into rbp.
; This is the general code for switching control to ML.  There are a number of cases to consider:
; 1.  Initial entry to root function or a new thread.  Needs to load EDX at least.
; 2.  Normal return from an RTS call.  Could just do a simple return.
; 3.  Exception raised in RTS call.
; 4.  Callback from C to an ML function.  In effect this is a coroutine. Similar to 1.
; 5.  Return from "trap" i.e. Heap/Stack overflow.  Stack-overflow can result in an exception
;     either because the stack can't be grown or because Interrupt has been raised.

; Switch from C code to ML.  This code uses the X86/64 Windows calling conventions.  It
; saves the callee-save registers and records this information so that debuggers can find
; the call stack and exception handlers can unwind the frame.  We don't actually use the
; ability to raise C++ exceptions across ML.  N.B. Any calls from ML to C need to
; duplicate the frame information.
X86AsmSwitchToPoly  PROC FRAME
    mov     [rsp+8],rcx                     ; Save argument for use later
    push    rbp                             ; Standard entry sequence
    .PUSHREG rbp
    .SETFRAME rbp,0
    mov     rbp,rsp                         
    push    rbx                             ; Save callee--save registers
    .PUSHREG rbx
    push    r12
    .PUSHREG r12
    push    r13
    .PUSHREG r13
    push    r14
    .PUSHREG r14
    push    r15
    .PUSHREG r15
    push    rdi                             ; Callee save in Windows
    .PUSHREG rdi
    push    rsi
    .PUSHREG rsi
    .endprolog
    sub     rsp,(Fr_size-56)
    mov     rax,[ArgVector.StackLimit+rcx]
    mov     [Fr_StackLimit+rbp],rax
    mov     rax,[ArgVector.LocalMbottom+rcx]
    mov     [Fr_LocalMbottom+rbp],rax
    mov     rax,[ArgVector.HandlerRegister+rcx]
    mov     [Fr_HandlerRegister+rbp],rax
    mov     rax,offset X86AsmCallExtraRETURN_STACK_OVERFLOWEX
    mov     [Fr_StackOverflowEx+rbp],rax
    mov     rax,offset X86AsmCallExtraRETURN_STACK_OVERFLOW
    mov     [Fr_StackOverflow+rbp],rax
    mov     rax,offset X86AsmCallExtraRETURN_HEAP_OVERFLOW
    mov     [Fr_HeapOverflow+rbp],rax
    mov     rax,offset X86AsmCallExtraRETURN_RAISE_OVERFLOW
    mov     [Fr_RaiseOverflow+rbp],rax
    mov     r15,[ArgVector.LocalMpointer+rcx]
    mov     rsp,[ArgVector.StackPtr+rcx]
    mov     rax,[ArgVector.ExceptionPacket+rcx]
    cmp     rax,1                                           ; Did we raise an exception?
    jnz     raisex
    test    byte ptr [rcx+ArgVector.FullRestore],1          ; Should we restore or clear the regs?
    jnz     sw2polyfull
; We're returning from an RTS call.
    mov     rax,[ArgVector.SaveRAX+rcx]
    cld                                                     ; Clear this just in case
    jmp     [ArgVector.ProgramCtr+rcx]                      ; Jump to code address

; Everything up to here is considered as part of the X86AsmSwitchToPoly proc
X86AsmSwitchToPoly ENDP

sw2polyfull:
    movsd   xmm0,[ArgVector.SaveXMM0+rcx]
    movsd   xmm1,[ArgVector.SaveXMM1+rcx]
    movsd   xmm2,[ArgVector.SaveXMM2+rcx]
    movsd   xmm3,[ArgVector.SaveXMM3+rcx]
    movsd   xmm4,[ArgVector.SaveXMM4+rcx]
    movsd   xmm5,[ArgVector.SaveXMM5+rcx]
    movsd   xmm6,[ArgVector.SaveXMM6+rcx]
    mov     rax,[ArgVector.SaveRAX+rcx]
    mov     rbx,[ArgVector.SaveRBX+rcx]
    mov     rdx,[ArgVector.SaveRDX+rcx]
    mov     rsi,[ArgVector.SaveRSI+rcx]
    mov     rdi,[ArgVector.SaveRDI+rcx]
    mov     r8,[ArgVector.SaveR8+rcx]
    mov     r9,[ArgVector.SaveR9+rcx]
    mov     r10,[ArgVector.SaveR10+rcx]
    mov     r11,[ArgVector.SaveR11+rcx]
    mov     r12,[ArgVector.SaveR12+rcx]
    mov     r13,[ArgVector.SaveR13+rcx]
    mov     r14,[ArgVector.SaveR14+rcx]
    cld                                     ; Clear this just in case
    push    [ArgVector.ProgramCtr+rcx]
    mov     rcx,[ArgVector.SaveRCX+rcx]
    ret                                     ; Jump to code address


; Code to save the state and switch to C
; This saves the full register state.
SaveFullState:
    push    rax                ; Save rax
    mov     rax,[Fr_Argument+rbp]
    mov     [ArgVector.SaveRBX+rax],rbx
    mov     [ArgVector.SaveRCX+rax],rcx
    mov     [ArgVector.SaveRDX+rax],rdx
    mov     [ArgVector.SaveRSI+rax],rsi
    mov     [ArgVector.SaveRDI+rax],rdi
    movsd   [ArgVector.SaveXMM0+rax],xmm0
    movsd   [ArgVector.SaveXMM1+rax],xmm1
    movsd   [ArgVector.SaveXMM2+rax],xmm2
    movsd   [ArgVector.SaveXMM3+rax],xmm3
    movsd   [ArgVector.SaveXMM4+rax],xmm4
    movsd   [ArgVector.SaveXMM5+rax],xmm5
    movsd   [ArgVector.SaveXMM6+rax],xmm6
    mov     [ArgVector.SaveR8+rax],r8
    mov     [ArgVector.SaveR9+rax],r9
    mov     [ArgVector.SaveR10+rax],r10
    mov     [ArgVector.SaveR11+rax],r11
    mov     [ArgVector.SaveR12+rax],r12
    mov     [ArgVector.SaveR13+rax],r13
    mov     [ArgVector.SaveR14+rax],r14
    pop     rbx                             ; Get old rax value
    mov     [ArgVector.SaveRAX+rax],rbx
    mov     [ArgVector.StackPtr+rax],rsp
    mov     rax,[Fr_Argument+rbp]
    mov     [ArgVector.LocalMpointer+rax],r15     ; Save back heap pointer
    mov     rbx,[Fr_HandlerRegister+rbp]    ; Copy back handler pointer
    mov     [ArgVector.HandlerRegister+rax],rbx
    mov     rsp,rbp                         ; Restore C stack pointer
    sub     rsp,56
    pop     rsi
    pop     rdi
    pop     r15                            ; Restore callee-save registers
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    pop     rbp
    ret

; As X86AsmSaveFullState but only save what is necessary for an RTS call.
PUBLIC  X86AsmSaveStateAndReturn
X86AsmSaveStateAndReturn:
SaveStateAndReturnLocal: ; This is necessary so that the jmps use a PC-relative address
jmp     SaveFullState

; Used when entering new code.  The argument and closure are on the stack
; in case there is a GC before we enter the code.
PUBLIC X86AsmPopArgAndClosure
X86AsmPopArgAndClosure:
    pop     rdx
    pop     rax
    jmp     qword ptr [rdx]

; This is used if the RTS sets up an exception.  It's probably no longer relevant.
PUBLIC  X86AsmRaiseException
X86AsmRaiseException:
raisex:
    mov     rcx,Fr_HandlerRegister[rbp]
    jmp     qword ptr [rcx]

; RTS call to kill the current thread. 
PUBLIC  X86AsmKillSelf
X86AsmKillSelf:
    CALL_EXTRA RETURN_KILL_SELF

PUBLIC  X86AsmCallbackReturn
X86AsmCallbackReturn:
    CALL_EXTRA RETURN_CALLBACK_RETURN

PUBLIC  X86AsmCallbackException
X86AsmCallbackException:
    CALL_EXTRA RETURN_CALLBACK_EXCEPTION

; This implements atomic addition in the same way as atomic_increment
PUBLIC  X86AsmAtomicIncrement
X86AsmAtomicIncrement:
    mov     rax,rcx
; Use rcx and rax because they are volatile (unlike rbx on X86/64/Unix)
    mov     rcx,2
    lock xadd [rax],rcx
    add     rcx,2
    mov     rax,rcx
    ret

CREATE_EXTRA_CALL MACRO index
PUBLIC  X86AsmCallExtra&index&
X86AsmCallExtra&index&:
    CALL_EXTRA index
    ENDM

CREATE_EXTRA_CALL RETURN_HEAP_OVERFLOW
CREATE_EXTRA_CALL RETURN_STACK_OVERFLOW
CREATE_EXTRA_CALL RETURN_STACK_OVERFLOWEX
CREATE_EXTRA_CALL RETURN_RAISE_OVERFLOW

END
