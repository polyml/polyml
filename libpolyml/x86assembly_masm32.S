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
;  eax: First argument to function.  Result of function call.
;  ebx: Second argument to function.
;  ecx: General register
;  edx: Closure pointer in call.
;  ebp: Points to memory used for extra registers
;  esi: General register.
;  edi: General register.
;  esp: Stack pointer.

; Include RTS call numbers
#include "sys.h"

.486
    .model  flat,c

; Set the register mask entry

;
; Macro to begin the hand-coded functions
;

;
; Tagged values.   A few operations, such as shift assume that the tag bit
; is the bottom bit.
;

#define TAG         1
#define TAGSHIFT    1
#define TAGGED(i)   (i*2+1)

#define POLYWORDSIZE    4

#define NIL     TAGGED(0)
#define UNIT    TAGGED(0)
#define ZERO    TAGGED(0)
#define FALSE   TAGGED(0)
#define TRUE    TAGGED(1)
#define MINUS1  TAGGED(0-1)

#define B_bytes         01h
#define B_typeBits      03h
#define B_mutable       40h
#define Max_Length      0ffffffh

; The "stack frame" pointed to by ebp acts as the "memory registers".
Fr_RaiseOverflow    EQU     -48
Fr_HeapOverflow     EQU     -40
Fr_StackOverflow    EQU     -36
Fr_StackOverflowEx  EQU     -32
Fr_LocalMpointer    EQU     -28
Fr_HandlerRegister  EQU     -24
Fr_LocalMbottom     EQU     -20
Fr_StackLimit       EQU     -16
Fr_Argument         EQU     8

; Extra entries on the C stack
Fr_SavedMLSP        EQU     -52

Fr_Size             EQU     56

; This is the argument vector passed in to X86AsmSwitchToPoly
; It is used to initialise the frame.  A few values are updated
; when ML returns.
ArgVector STRUCT
LocalMPointer       DWORD   ?
HandlerRegister     DWORD   ?
LocalMbottom        DWORD   ?
StackLimit          DWORD   ?
ExceptionPacket     DWORD   ?       ; Address of packet to raise
UnusedRequestCode   DB      ?       ; Byte: Io function to call.
UnusedFlag          DB      ?
ReturnReason        DB      ?       ; Byte: Reason for returning from ML.
FullRestore         DB      ?       ; Byte: Full/partial restore
UnusedPStack        DWORD   ?       ; Current stack base
ThreadId            DWORD   ?       ; My thread id
StackPtr            DWORD   ?       ; Stack pointer
ProgramCtr          DWORD   ?
SaveRAX             DWORD   ?
SaveRBX             DWORD   ?
SaveRCX             DWORD   ?
SaveRDX             DWORD   ?
SaveRSI             DWORD   ?
SaveRDI             DWORD   ?
SaveFP              WORD   ?         ; Actually larger
ArgVector ENDS


; Codes to indicate the reason for return.  Zero indicates an RTS call.
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
; These are defined as Masm macros because there are multiple instructions.

CALL_EXTRA  MACRO   index
    push    ecx
    mov     ecx,[Fr_Argument+ebp]
    mov     byte ptr [ArgVector.ReturnReason+ecx],index
    pop     ecx
    jmp     SaveFullState
ENDM

; Load the registers from the ML stack and jump to the code.
; This is used to start ML code.
; The argument is the address of the MemRegisters struct and goes into ebp.
; This is the general code for switching control to ML.  There are a number of cases to consider:
; 1.  Initial entry to root function or a new thread.  Needs to load EDX at least.
; 2.  Normal return from an RTS call.  Could just do a simple return.
; 3.  Exception raised in RTS call.
; 4.  Callback from C to an ML function.  In effect this is a coroutine. Similar to 1.
; 5.  Return from "trap" i.e. Heap/Stack overflow.  Stack-overflow can result in an exception
;     either because the stack can't be grown or because Interrupt has been raised.
PUBLIC  X86AsmSwitchToPoly
X86AsmSwitchToPoly:
    push    ebp                             ; Standard entry sequence
    mov     ebp,esp
    push    ebx                             ; Push callee-save registers
    push    edi
    push    esi
    sub     esp,(Fr_size-12)                ; Allocate frame
    mov     ecx,[Fr_Argument+ebp]           ; Address of argument vector
    mov     eax,[ArgVector.StackLimit+ecx]
    mov     [Fr_StackLimit+ebp],eax
    mov     eax,[ArgVector.LocalMbottom+ecx]
    mov     [Fr_LocalMbottom+ebp],eax
    mov     eax,[ArgVector.HandlerRegister+ecx]
    mov     [Fr_HandlerRegister+ebp],eax
    mov     eax,[ArgVector.LocalMpointer+ecx]
    mov     [Fr_LocalMpointer+ebp],eax
    mov     eax,offset X86AsmCallExtraRETURN_STACK_OVERFLOWEX
    mov     [Fr_StackOverflowEx+ebp],eax
    mov     eax,offset X86AsmCallExtraRETURN_STACK_OVERFLOW
    mov     [Fr_StackOverflow+ebp],eax
    mov     eax,offset X86AsmCallExtraRETURN_HEAP_OVERFLOW
    mov     [Fr_HeapOverflow+ebp],eax
    mov     eax,offset X86AsmCallExtraRETURN_RAISE_OVERFLOW
    mov     [Fr_RaiseOverflow+ebp],eax
    mov     esp,[ArgVector.StackPtr+ecx]
    mov     eax,[ArgVector.ExceptionPacket+ecx]
    cmp     eax,1                                           ; Did we raise an exception?
    jnz     raisex

    test    byte ptr [ecx+ArgVector.FullRestore],1          ; Should we restore or clear the regs?

    jnz     sw2polyfull
; We're returning from an RTS call.
    mov     eax,[ArgVector.SaveRAX+ecx]                 ; The return value
    cld                                                 ; Clear this just in case
    jmp     [ArgVector.ProgramCtr+ecx]                  ; Jump to code address

sw2polyfull:
    frstor  [ArgVector.SaveFP+ecx]
    mov     eax,[ArgVector.SaveRAX+ecx]
    mov     ebx,[ArgVector.SaveRBX+ecx]
    mov     edx,[ArgVector.SaveRDX+ecx]
    mov     esi,[ArgVector.SaveRSI+ecx]
    mov     edi,[ArgVector.SaveRDI+ecx]
    cld                                     ; Clear this just in case
    push    [ArgVector.ProgramCtr+ecx]
    mov     ecx,[ArgVector.SaveRCX+ecx]
    ret                                     ; Jump to code address

; Code to save the state and switch to C
; This saves the full register state.
SaveFullState:
    push    eax                             ; Save eax
    mov     eax,[Fr_Argument+ebp]
    mov     [ArgVector.SaveRBX+eax],ebx
    mov     [ArgVector.SaveRCX+eax],ecx
    mov     [ArgVector.SaveRDX+eax],edx
    mov     [ArgVector.SaveRSI+eax],esi
    mov     [ArgVector.SaveRDI+eax],edi
    FNSAVE  [ArgVector.SaveFP+eax]          ; Save FP state.  Also resets the state so...
    FLDCW   [ArgVector.SaveFP+eax]          ; ...load because we need the same rounding mode in the RTS
    pop     ebx                             ; Get old eax value
    mov     [ArgVector.SaveRAX+eax],ebx
    mov     [ArgVector.StackPtr+eax],esp                 ; Save ML stack pointer
    mov     eax,[Fr_Argument+ebp]
    mov     ebx,[Fr_LocalMpointer+ebp]      ; Copy back heap pointer
    mov     [ArgVector.LocalMpointer+eax],ebx
    mov     ebx,[Fr_HandlerRegister+ebp]    ; and handler pointer
    mov     [ArgVector.HandlerRegister+eax],ebx
    mov     esp,ebp                         ; Restore C stack pointer
    sub     esp,12                          ; Sp is just before the registers
    pop     esi                             ; Restore saved registers
    pop     edi
    pop     ebx
    pop     ebp
    ret

; As X86AsmSaveFullState but only save what is necessary for an RTS call.
PUBLIC  X86AsmSaveStateAndReturn
X86AsmSaveStateAndReturn:
SaveStateAndReturnLocal: ; This is necessary so that the jmps use a PC-relative address
    jmp     SaveFullState
IFDEF NOTNOW
    push   eax                ; Save eax
    mov     eax,PolyStack[ebp]
    mov     EBX_OFF[eax],ebx
    mov     EDX_OFF[eax],edx
    FSTCW   FPREGS_OFF[eax]
    FNINIT                     ; Reset the FP state.
    FLDCW   FPREGS_OFF[eax]   ; But reload the rounding mode
    pop    ebx                ; Get old eax value
    mov     EAX_OFF[eax],ebx
    mov     SP_OFF[eax],esp
    mov     byte ptr [InRTS+ebp],1
    mov     esp,SavedSp[ebp]
    popad
    ret
ENDIF

; Used when entering new code.  The argument and closure are on the stack
; in case there is a GC before we enter the code.
PUBLIC X86AsmPopArgAndClosure
X86AsmPopArgAndClosure:
    pop     edx
    pop     eax
    jmp     dword ptr [edx]

; This is used if the RTS sets up an exception.  It's probably no longer relevant.
PUBLIC  X86AsmRaiseException
X86AsmRaiseException:
raisex:
    mov     ecx,Fr_HandlerRegister[ebp]
    jmp     dword ptr [ecx]

; Additional assembly code routines

; Terminate the current thread
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
; N.B. It is called from the RTS so uses C linkage conventions.
PUBLIC  X86AsmAtomicIncrement
X86AsmAtomicIncrement:
    mov     eax,4[esp]
; Use ecx and eax because they are volatile (unlike ebx on X86/64/Unix)
    mov     ecx,2
    lock xadd [eax],ecx
    add     ecx,2
    mov     eax,ecx
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
