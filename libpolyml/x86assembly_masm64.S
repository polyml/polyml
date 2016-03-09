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

; Register mask entries - must match coding used in I386CODECONS.ML
M_Reax      EQU     000001H
M_Recx      EQU     000002H
M_Redx      EQU     000004H
M_Rebx      EQU     000008H
M_Resi      EQU     000010H
M_Redi      EQU     000020H
M_R8        EQU     64
M_R9        EQU     128
M_R10       EQU     256
M_R11       EQU     512
M_R12       EQU     1024
M_R13       EQU     2048
M_R14       EQU     4096
M_FP0       EQU     002000H
M_FP1       EQU     004000H
M_FP2       EQU     008000H
M_FP3       EQU     010000H
M_FP4       EQU     020000H
M_FP5       EQU     040000H
M_FP6       EQU     080000H
M_FP7       EQU     100000H

Mask_all    EQU     1FFFFFH

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

#define POLYWORDSIZE    8

#define NIL     TAGGED(0)
#define UNIT    TAGGED(0)
#define ZERO    TAGGED(0)
#define FALSE   TAGGED(0)
#define TRUE    TAGGED(1)
#define MINUS1  TAGGED(0-1)

#define B_bytes         01h
#define B_mutablebytes  41h
#define B_mutable       40h
#define Max_Length      00ffffffffffffffh              

; The "stack frame" pointed to by rbp acts as the "memory registers".
Fr_RaiseOverflow    EQU     -120
Fr_ThreadId         EQU     -112
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
Arg_LocalMpointer   EQU     0
Arg_HandlerRegister EQU     8
Arg_LocalMbottom    EQU     16
Arg_StackLimit      EQU     24
Arg_ExceptionPacket EQU     32  ;# Address of packet to raise
Arg_RequestCode     EQU     40  ;# Byte: Io function to call.
Arg_ReturnReason    EQU     42  ;# Byte: Reason for returning from ML.
Arg_FullRestore     EQU     43  ;# Byte: Full/partial restore
Arg_PolyStack       EQU     48  ;# Current stack base
Arg_ThreadId        EQU     64  ;# My thread id

;LocalMpointer       EQU     0
;HandlerRegister     EQU     -80
;LocalMbottom        EQU     -72
;ExceptionPacket     EQU     32  ; Address of packet to raise
;RequestCode         EQU     40  ; Byte: Io function to call.
;InRTS               EQU     41  ; Byte: Set when in the RTS
;ReturnReason        EQU     42  ; Byte: Reason for returning from ML.
;FullRestore         EQU     43  ; Byte: Full/partial restore
;PolyStack           EQU     48  ; Current stack base
;SavedSp             EQU     56  ; Saved stack pointer
;ThreadId            EQU     -112 ; My thread id
;RealTemp            EQU     128 ; Space for int-real conversions
;RaiseOverflow       EQU     -120 ; Call to raise the Overflow exception
;Mr_ESP              EQU     144 ; Saved esp value

RETURN_HEAP_OVERFLOW        EQU 1
RETURN_STACK_OVERFLOW       EQU 2
RETURN_STACK_OVERFLOWEX     EQU 3
RETURN_CALLBACK_RETURN      EQU 6
RETURN_CALLBACK_EXCEPTION   EQU 7
RETURN_RAISE_OVERFLOW       EQU 8

;
; Starting offsets
PC_OFF      EQU     8
SP_OFF      EQU     16
EAX_OFF     EQU     40
EBX_OFF     EQU     48
ECX_OFF     EQU     56
EDX_OFF     EQU     64
ESI_OFF     EQU     72
EDI_OFF     EQU     80
R8_OFF      EQU     88
R9_OFF      EQU     96
R10_OFF     EQU     104
R11_OFF     EQU     112
R12_OFF     EQU     120
R13_OFF     EQU     128
R14_OFF     EQU     136
FPREGS_OFF  EQU     144

;
; CODE STARTS HERE
;
    .CODE

; Define standard call macro. CALL_IO ioCallNo  where ioCallNo is the io function to call.
; These are defined as Masm macros because there are multiple instructions.

CALL_IO    MACRO   index
    mov     rcx,[Fr_Argument+rbp]
    mov     byte ptr [Arg_RequestCode+rcx],index
    jmp     SaveStateAndReturnLocal
ENDM

CALL_EXTRA  MACRO   index
    push    rcx
    mov     rcx,[Fr_Argument+rbp]
    mov     byte ptr [Arg_ReturnReason+rcx],index
    pop     rcx
    jmp     SaveFullState
ENDM



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
PUBLIC  X86AsmSwitchToPoly
X86AsmSwitchToPoly:
    mov     [rsp+8],rcx                     ; Save argument for use later
    push    rbp                             ; Standard entry sequence
    mov     rbp,rsp                         
    push    rbx                             ; Save callee--save registers
    push    r12
    push    r13
    push    r14
    push    r15
    push    rdi                             ; Callee save in Windows
    push    rsi
    sub     esp,(Fr_size-56)
    mov     rax,[Arg_StackLimit+rcx]
    mov     [Fr_StackLimit+rbp],rax
    mov     rax,[Arg_LocalMbottom+rcx]
    mov     [Fr_LocalMbottom+rbp],rax
    mov     rax,[Arg_HandlerRegister+rcx]
    mov     [Fr_HandlerRegister+rbp],rax
    mov     rax,offset X86AsmCallExtraRETURN_STACK_OVERFLOWEX
    mov     [Fr_StackOverflowEx+rbp],rax
    mov     rax,offset X86AsmCallExtraRETURN_STACK_OVERFLOW
    mov     [Fr_StackOverflow+rbp],rax
    mov     rax,offset X86AsmCallExtraRETURN_HEAP_OVERFLOW
    mov     [Fr_HeapOverflow+rbp],rax
    mov     rax,[Arg_ThreadId+rcx]
    mov     [Fr_ThreadId+rbp],rax
    mov     rax,offset X86AsmCallExtraRETURN_RAISE_OVERFLOW
    mov     [Fr_RaiseOverflow+rbp],rax
    mov     r15,[Arg_LocalMpointer+rcx]
    mov     rax,Arg_PolyStack[rcx]
    mov     rsp,SP_OFF[rax]
    push    PC_OFF[rax]                    ; Push the code address
    test    byte ptr [rcx+Arg_FullRestore],1   ; Should we restore or clear the regs?

    jnz     sw2polyfull
; We're returning from an RTS call.
    mov     rax,EAX_OFF[rax]
    cld                                     ; Clear this just in case
    ret                                     ; Jump to code address

sw2polyfull:
    FRSTOR  FPREGS_OFF[rax]
    mov     rbx,EBX_OFF[rax]
    mov     rcx,ECX_OFF[rax]
    mov     rdx,EDX_OFF[rax]
    mov     rsi,ESI_OFF[rax]
    mov     rdi,EDI_OFF[rax]
    mov     r8,R8_OFF[rax]
    mov     r9,R9_OFF[rax]
    mov     r10,R10_OFF[rax]
    mov     r11,R11_OFF[rax]
    mov     r12,R12_OFF[rax]
    mov     r13,R13_OFF[rax]
    mov     r14,R14_OFF[rax]
    cld                                     ; Clear this just in case
    mov     rax,EAX_OFF[rax]
    ret                                     ; Jump to code address


; Code to save the state and switch to C
; This saves the full register state.
SaveFullState:
    push    rax                ; Save rax
    mov     rax,[Fr_Argument+rbp]
    mov     rax,[Arg_PolyStack+rax]
    mov     EBX_OFF[rax],rbx
    mov     ECX_OFF[rax],rcx
    mov     EDX_OFF[rax],rdx
    mov     ESI_OFF[rax],rsi
    mov     EDI_OFF[rax],rdi
    FNSAVE  FPREGS_OFF[rax]          ; Save FP state.  Also resets the state so...
    FLDCW   FPREGS_OFF[rax]          ; ...load because we need the same rounding mode in the RTS
    mov     R8_OFF[rax],r8
    mov     R9_OFF[rax],r9
    mov     R10_OFF[rax],r10
    mov     R11_OFF[rax],r11
    mov     R12_OFF[rax],r12
    mov     R13_OFF[rax],r13
    mov     R14_OFF[rax],r14
    pop     rbx                             ; Get old rax value
    mov     EAX_OFF[rax],rbx
    mov     SP_OFF[rax],rsp
    mov     rax,[Fr_Argument+rbp]
    mov     [Arg_LocalMpointer+rax],r15     ; Save back heap pointer
    mov     rbx,[Fr_HandlerRegister+rbp]    ; Copy back handler pointer
    mov     [Arg_HandlerRegister+rax],rbx
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
IFDEF NOTNOW
    push    rax                ; Save rax
    mov     rax,PolyStack[rbp]
    mov     EBX_OFF[rax],rbx
    mov     EDX_OFF[rax],rdx
    FSTCW   FPREGS_OFF[rax]
    FNINIT                     ; Reset the FP state.
    FLDCW   FPREGS_OFF[rax]   ; But reload the rounding mode
    mov     R8_OFF[rax],r8
    mov     R9_OFF[rax],r9
    mov     R10_OFF[rax],r10
    mov     LocalMpointer[rbp],r15
    pop     rbx                ; Get old rax value
    mov     EAX_OFF[rax],rbx
    mov     SP_OFF[rax],rsp
    mov     byte ptr [InRTS+rbp],1
    mov     rsp,SavedSp[rbp]
    pop     rsi
    pop     rdi
    pop     r15                            ; Restore callee-save registers
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    pop     rbp
    ret
ENDIF

; CallcodeTupled.  This is currently only used to "fold" RTS functions that are applied
; to constant arguments.  Oddly, compared with other RTS functions, it takes a single
; argument that is a pair containing the function closure and an argument vector.
PUBLIC  callcodeTupled
callcodeTupled:
    mov     rdx,[rax]
    mov     rsi,POLYWORDSIZE[rax]
    cmp     rsi,1
    je      cct2
    mov     rcx,Max_Length
    and     rcx,(-POLYWORDSIZE)[rsi]
    jz      cct2
    mov     rax,[rsi]
    add     rsi,POLYWORDSIZE
    sub     rcx,1
    jz      cct2
    mov     rbx,[rsi]
    add     rsi,POLYWORDSIZE
    sub     rcx,1
    jz      cct2
    mov     r8,[rsi]
    add     rsi,POLYWORDSIZE
    sub     rcx,1
    jz      cct2
    mov     r9,[rsi]
    add     rsi,POLYWORDSIZE
    sub     rcx,1
    jz      cct2
    mov     r10,[rsi]
    add     rsi,POLYWORDSIZE
    sub     rcx,1
    jz      cct2
    pop     rdi                                ; Get the return address
cct1:                                           ; Push the remaining args to the stack
    push    [rsi]
    add     rsi,POLYWORDSIZE
    LOOP    cct1
    push    rdi                                ; Push the return address

cct2:                                           ; Finished - enter function
    mov     rsi,1
    mov     rcx,1
    jmp     qword ptr [rdx]


Mask_callcodeTupled      EQU     Mask_all
;
; A number of functions implemented in Assembly for efficiency reasons
;

PUBLIC  int_to_word
int_to_word:
 ; Extract the low order bits from a word.
            test    rax,TAG
    jz      get_first_long_word_a1
    ret                 ; Return the argument

Mask_int_to_word      EQU     (M_Reax)

 ; This is now used in conjunction with isShort in Word.fromInt.
PUBLIC  get_first_long_word_a
get_first_long_word_a:
get_first_long_word_a1:
    test    byte ptr [rax-1],16  ; 16 is the "negative" bit
            mov     rax,[rax]
    jz      gfw1
    neg    rax            ; We can ignore overflow
gfw1:
            lea     rax,1[rax*2]
    ret

Mask_get_first_long_word      EQU     (M_Reax)

PUBLIC  move_bytes
move_bytes:
 ; Move a segment of memory from one location to another.
 ; Must deal with the case of overlapping segments correctly.
 ; (source, sourc_offset, destination, dest_offset, length)

 ; Assume that the offsets and length are all short integers.
            mov     rdi,r8
            mov     rcx,r9
            shr     rcx,TAGSHIFT
            add     rdi,rcx
            mov     rsi,rax
            shr     rbx,TAGSHIFT
            add     rsi,rbx
            mov     rcx,r10
            shr     rcx,TAGSHIFT
    cld                             ; Default to increment rdi,rsi
            cmp     rsi,rdi
 ; If dest > src then use decrementing moves else
 ; use incrementing moves.
    ja      mvb1
    std                             ; Decrement rdi,rsi
            lea     rsi,(-1)[rsi+rcx]
            lea     rdi,(-1)[rdi+rcx]
mvb1:
    rep movsb                       ; Copy the bytes
            mov     rax,1
            mov     rbx,rax
            mov     rcx,rax
            mov     rdi,rax
            mov     rsi,rax
 ; Visual Studio 5 C++ seems to assume that the direction flag
 ; is cleared.  I think that`s a bug but we have to go along with it.
    cld
    ret

Mask_move_bytes      EQU     Mask_all


PUBLIC  move_words
move_words:
 ; Move a segment of memory from one location to another.
 ; Must deal with the case of overlapping segments correctly.
 ; (source, source_offset, destination, dest_offset, length)
 ; Assume that the offsets and length are all short integers.
            mov     rdi,r8
            mov     rcx,r9
            lea     rdi,(-4)[rdi+rcx*4]
            lea     rsi,(-4)[rax+rbx*4]
            mov     rcx,r10
            shr     rcx,TAGSHIFT
    cld                             ; Default to increment rdi,rsi
            cmp     rsi,rdi
 ; If dest > src then use decrementing moves else
 ; use incrementing moves.
    ja      mvw1
    std                             ; Decrement rdi,rsi

            lea     rsi,(-POLYWORDSIZE)[rsi+rcx*POLYWORDSIZE]
            lea     rdi,(-POLYWORDSIZE)[rdi+rcx*POLYWORDSIZE]

mvw1:
    rep movsq                       ; Copy the words
            mov     rax,1
            mov     rcx,rax
            mov     rdi,rax
            mov     rsi,rax
 ; Visual Studio 5 C++ seems to assume that the direction flag
 ; is cleared.  I think that`s a bug but we have to go along with it.
    cld
    ret

Mask_move_words      EQU     Mask_all
;

RetFalse:
    mov     rax,FALSE
    ret

RetTrue:
    mov     rax,TRUE
    ret

PUBLIC  not_bool
not_bool:
    xor     rax,(TRUE-TAG)
    ret

Mask_not_bool      EQU     (M_Reax)

; or, and, xor shift etc. assume the values are tagged integers
PUBLIC  or_word
or_word:
    or      rax,rbx
    ret

Mask_or_word      EQU     (M_Reax)

PUBLIC  and_word
and_word:
    and     rax,rbx
    ret

Mask_and_word      EQU     (M_Reax)

PUBLIC  xor_word
xor_word:
    xor     rax,rbx
    or      rax,TAG
    ret

Mask_xor_word      EQU     (M_Reax)

PUBLIC  shift_left_word
shift_left_word:
; Assume that both args are tagged integers
; Word.<<(a,b) is defined to return 0 if b > Word.wordSize

    cmp     rbx,TAGGED(63)
    jb      slw1
    mov     rax,1
    ret
slw1:
    mov     rcx,rbx
    shr     rcx,TAGSHIFT
    sub     rax,TAG
    shl     rax,cl
    or      rax,TAG
    mov     rcx,rax
    ret

Mask_shift_left_word      EQU     (M_Reax OR M_Recx)

PUBLIC  shift_right_word
shift_right_word:
; Word.>>(a,b) is defined to return 0 if b > Word.wordSize
    cmp     rbx,TAGGED(63)
    jb      srw1
    mov     rax,1
    ret
srw1:
    mov     rcx,rbx
    shr     rcx,TAGSHIFT
    shr     rax,cl
    or      rax,TAG
    mov     rcx,rax
    ret

Mask_shift_right_word      EQU     (M_Reax OR M_Recx)

PUBLIC  shift_right_arith_word
shift_right_arith_word:
 ; Word.~>>(a,b) is defined to return 0 or ~1 if b > Word.wordSize
 ; The easiest way to do that is to set the shift to 31.
    cmp     rbx,TAGGED(63)
    jb      sra1
    mov     rbx,TAGGED(63)
sra1:
    mov     rcx,rbx
    shr     rcx,TAGSHIFT
    sar     rax,cl
    or      rax,TAG
    mov     rcx,rax
    ret

Mask_shift_right_arith_word      EQU     (M_Reax OR M_Recx)

; Clears the "mutable" bit on a segment
PUBLIC  locksega
locksega:
    and     byte ptr    [rax-1],(0ffh-B_mutable)
    mov     rax,TAGGED(0)
    ret

Mask_lockseg      EQU     M_Reax

PUBLIC  get_length_a
get_length_a:
    mov     rax,(-POLYWORDSIZE)[rax]
    shl     rax,8
    shr     rax,(8-TAGSHIFT)
    or      rax,TAG
    ret

Mask_get_length      EQU     (M_Reax)


PUBLIC  is_shorta
is_shorta:
; Returns true if the argument is tagged
    and     rax,TAG
    jz      RetFalse
    jmp     RetTrue

Mask_is_short      EQU     (M_Reax)

PUBLIC  string_length
string_length:
    test    rax,TAG
    jnz     RetOne      ; character.
    mov     rax,[rax]
    lea     rax,1[rax*2]
    ret
RetOne:
    mov     rax,TAGGED(1)
    ret

Mask_string_length      EQU     (M_Reax)

 ; Store the length of a string in the first word.
PUBLIC  set_string_length_a
set_string_length_a:
    shr     rbx,TAGSHIFT
    mov     [rax],rbx
    mov     rax,UNIT            ; Return unit
    mov     rbx,rax             ; Clobber untagged value
    ret

Mask_set_string_length      EQU     (M_Reax OR M_Rebx)

; raisex is only used in "raiseWithLocation" to bypass the normal ML "raise".
PUBLIC  raisex
raisex:
    mov     rcx,Fr_HandlerRegister[rbp]
    jmp     qword ptr [rcx]

PUBLIC  load_byte
load_byte:
    mov     rdi,rbx
    shr     rdi,TAGSHIFT
    movzx   rdi, byte ptr [rax][rdi]
    lea     rax,1[rdi*2]
    mov     rdi,rax
    ret

Mask_load_byte      EQU     (M_Reax OR M_Redi)

PUBLIC  load_word
load_word:
    mov     rax,(-4)[rax+rbx*4]
    mov     rbx,rax
    ret

Mask_load_word      EQU     (M_Reax)

PUBLIC  assign_byte
assign_byte:
; We can assume that the data value will not overflow 30 bits (it is only 1 byte!)
    mov     rcx,r8
    shr     rcx,TAGSHIFT

; We can assume that the index will not overflow 30 bits i.e. it is a tagged short
    shr     rbx,TAGSHIFT
    mov     [rax+rbx],cl

    mov     rax,1
    mov     rbx,rax
    mov     rcx,rax
    ret

Mask_assign_byte      EQU     (M_Reax OR M_Rebx OR M_Recx)


PUBLIC  assign_word
assign_word:
    mov     (-4)[rax+rbx*4],r8
    mov     rax,1
    ret

Mask_assign_word      EQU     (M_Reax)

; Allocate a piece of memory that does not need to be initialised.
; We can't actually risk leaving word objects uninitialised so for the
; moment we always initialise.
PUBLIC  alloc_uninit
alloc_uninit:
            mov     r8,1
; Drop through into alloc_store

Mask_alloc_uninit      EQU     Mask_all

; alloc(size, flags, initial).  Allocates a segment of a given size and
; initialises it.
;
; This is primarily used for arrays and for strings.  Refs are
; allocated using inline code.
PUBLIC  alloc_store
alloc_store:
allsts:
 ; alloc(size, flags, initial).  Allocates a segment of a given size and
 ; initialises it.
 ; First check that the length is acceptable
    test    rax,TAG
    jz      alloc_in_rts                ; Get the RTS to raise an exception
    mov     rdi,rax
    shr     rdi,TAGSHIFT                ; Remove tag
    mov     rdx,Max_Length              ; Length field must fit in 56 bits
    cmp     rdi,rdx
    ja      alloc_in_rts                ; Get the RTS to raise an exception
    add     rdi,1                       ; Add 1 word
    shl     rdi,3                       ; Get length in bytes
    mov     rdx,r15
    sub     rdx,rdi                     ; Allocate the space
    mov     rdi,rax                     ; Clobber bad value in rdi
    cmp     rdx,Fr_LocalMbottom[rbp]       ; Check for free space
    jb      alloc_in_rts
; Normally the above test is sufficient but if LocalMpointer is near the bottom of
; memory and the store requested is very large the value in rdx can be negative
; which is greater, unsigned, than LocalMbottom.  We have to check it is less
; than, unsigned, the allocation pointer.
    cmp     rdx,r15
    jnb     alloc_in_rts
    mov     r15,rdx                     ; Put back in the heap ptr
    shr     rax,TAGSHIFT
    mov     (-8)[rdx],rax               ; Put in length
    shr     rbx,TAGSHIFT                ; remove tag from flag
    or      rbx,B_mutable               ; set mutable bit
    mov     (-1)[rdx],bl                ; and put it in.
; Initialise the store.
    mov     rcx,rax                     ; Get back the no. of words.
    mov     rax,r8                      ; Get initial value.
    cmp     rbx,B_mutablebytes
    jne     allst2

; If this is a byte seg
    shr     rax,TAGSHIFT                ; untag the initialiser
    shl     rcx,3                       ; Convert to bytes
    mov     rdi,rdx
    rep stosb
    jmp     allst3

 ; If this is a word segment
allst2:
    mov     rdi,rdx
    rep stosq

allst3:
    mov     rax,rdx                 ; Result to rax

    mov     rcx,rax                 ; Clobber these
    mov     rdx,rax
    mov     rbx,rax
    mov     rdi,rax
    ret

Mask_alloc_store      EQU     Mask_all

; This is used if we have reached the store limit and need to garbage-collect.
alloc_in_rts:
    mov     rdx,rax         ; Clobber these first
    mov     rdi,rax
    CALL_IO    POLY_SYS_alloc_store

PUBLIC  touch_final
touch_final:
; This is really a pseudo-op
    mov     rax,UNIT
    ret

Mask_touch_final      EQU     (M_Reax)

PUBLIC  add_long
add_long:
    mov     rdi,rax
    and     rdi,rbx
    and     rdi,TAG
    jz      add_really_long
    lea     rdi,(-TAG)[rax]
    add     rdi,rbx
    jo      add_really_long
    mov     rax,rdi
    ret
add_really_long:
    mov     rdi,rax
    CALL_IO    POLY_SYS_aplus

Mask_aplus      EQU     (M_Reax OR M_Redi OR Mask_all)

PUBLIC  sub_long
sub_long:
    mov     rdi,rax
    and     rdi,rbx
    and     rdi,TAG
    jz      sub_really_long
    mov     rdi,rax
    sub     rdi,rbx
    jo      sub_really_long
    lea     rax,TAG[rdi]
    mov     rdi,rax
    ret
sub_really_long:
    mov     rdi,rax
    CALL_IO    POLY_SYS_aminus

Mask_aminus      EQU     (M_Reax OR M_Redi OR Mask_all)

PUBLIC  mult_long
mult_long:
    mov     rdi,rax
    and     rdi,rbx
    and     rdi,TAG
    jz      mul_really_long
    mov     rdi,rbx
    sar     rdi,TAGSHIFT            ; Shift multiplicand
    mov     rsi,rax
    sub     rsi,TAG                 ; Just subtract off the tag off multiplier
    imul    rsi,rdi
    jo      mul_really_long
    add     rsi,TAG
    mov     rax,rsi
    mov     rdi,rax
    ret
mul_really_long:
    mov     rsi,rax                 ; Clobber this
    mov     rdi,rax
    CALL_IO    POLY_SYS_amul

Mask_amul      EQU     (M_Reax OR M_Redi OR M_Resi OR Mask_all)

PUBLIC  div_long
div_long:
    mov     rdi,rax
    and     rdi,rbx
    and     rdi,TAG                     ; %rdi now contains $0 or $1 (both legal!)
    jz      div_really_long
    cmp     rbx,TAGGED(0)
    jz      div_really_long             ; We don't want a trap.
    ; The only case of overflow is dividing the smallest negative number by -1
    cmp     rbx,TAGGED((-1))
    jz      div_really_long
    sar     rax,TAGSHIFT
    mov     rdi,rbx
    sar     rdi,TAGSHIFT
    cqo
    idiv    rdi
    lea     rax,1[rax*2]
    mov     rdx,rax
    mov     rdi,rax
    ret
div_really_long:
    mov     rdi,rax
    CALL_IO    POLY_SYS_adiv

Mask_adiv      EQU     (M_Reax OR M_Redi OR M_Redx OR Mask_all)

PUBLIC  rem_long
rem_long:
    mov     rdi,rax
    and     rdi,rbx
    and     rdi,TAG                 ; %rdi now contains $0 or $1 (both legal!
    jz      rem_really_long
    cmp     rbx,TAGGED(0)                   ; Check that it's non-zero
    jz      rem_really_long                 ; We don't want a trap.
    ; The only case of overflow is dividing the smallest negative number by -1
    cmp     rbx,TAGGED((-1))
    jz      rem_really_long
    sar     rax,TAGSHIFT
    mov     rdi,rbx
    sar     rdi,TAGSHIFT
    cqo
    idiv    rdi
    lea     rax,1[rdx*2]
    mov     rdx,rax
    mov     rdi,rax
    ret
rem_really_long:
    mov     rdi,rax
    CALL_IO    POLY_SYS_amod

Mask_amod      EQU     (M_Reax OR M_Redi OR M_Redx OR Mask_all)

 ; Combined quotient and remainder.  We have to use the long form
 ; if the arguments are long or there's an overflow.  The first two
 ; arguments are the values to be divided.  The third argument is the
 ; address where the results should be placed. 
PUBLIC  quotrem_long
quotrem_long:
    mov     rdi,rax
    and     rdi,rbx
    and     rdi,TAG
    jz      quotrem_really_long
    cmp     rbx,TAGGED(0)
    jz      quotrem_really_long
    ; The only case of overflow is dividing the smallest negative number by -1
    cmp     rbx,TAGGED((-1))
    jz      quotrem_really_long

    ; Get the address for the result.
    mov     rcx,r8
    ; Do the division
    sar     rax,TAGSHIFT
    mov     rdi,rbx
    sar     rdi,TAGSHIFT
    cqo
    idiv    rdi
    lea     rax,1[rax*2]
    lea     rdx,1[rdx*2]
    mov     rdi,rax
    mov     [rcx],rax
    mov     POLYWORDSIZE[rcx],rdx
    mov     rax,rcx
    ret

mem_for_remquot1:  ; Not enough store: clobber bad value in rcx.
    mov     rcx,1

quotrem_really_long:
    mov     rdi,rax
    CALL_IO    POLY_SYS_quotrem

Mask_quotrem      EQU     (M_Reax OR M_Redi OR M_Redx OR Mask_all)

; TODO: Isn't this byte equality?
PUBLIC  equal_long
equal_long:
    cmp     rbx,rax
    je      RetTrue
    mov     rcx,rax
    or      rax,rbx
    and     rax,TAG
    jnz     RetFalse
    mov     rax,rcx
    CALL_IO    POLY_SYS_equala

Mask_equala      EQU     (M_Reax OR M_Recx OR Mask_all)


PUBLIC  or_long
or_long:
    CALL_IO    POLY_SYS_ora

Mask_ora      EQU     (M_Reax OR M_Redi OR Mask_all)

PUBLIC  xor_long
xor_long:
    CALL_IO    POLY_SYS_xora

Mask_xora      EQU     (M_Reax OR M_Redi OR Mask_all)

PUBLIC  and_long
and_long:
   CALL_IO    POLY_SYS_anda

Mask_anda      EQU     (M_Reax OR M_Redi OR Mask_all)

PUBLIC  neg_long
    neg_long:
    test    rax,TAG
    jz      neg_really_long
    mov     rdi,(TAGGED(0)+TAG)
    sub     rdi,rax
    jo      neg_really_long
    mov     rax,rdi
    ret
neg_really_long:
    mov     rdi,rax
    CALL_IO    POLY_SYS_aneg

Mask_aneg      EQU     (M_Reax OR M_Redi OR Mask_all)

PUBLIC  int_geq
int_geq:
    test    rax,TAG
    jz      igeq2
    test    rbx,TAG
    jz      igeq1
    cmp     rax,rbx
    jge     RetTrue
    jmp     RetFalse
igeq1:
 ; First arg is short, second isn't
    test    byte ptr [rbx-1],16  ; 16 is the "negative" bit
    jnz     RetTrue     ; Negative - always less
    jmp     RetFalse

igeq2:
; First arg is long
    test    rbx,TAG
    jz      igeq3
; First arg is long, second is short
    test    byte ptr [rax-1],16  ; 16 is the "negative" bit
    jz      RetTrue    ; Positive - always greater
    jmp     RetFalse

igeq3:
; Both long
   CALL_IO    POLY_SYS_int_geq

Mask_int_geq      EQU     (M_Reax OR Mask_all)


PUBLIC  int_leq
int_leq:
    test    rax,TAG
    jz      ileq2
    test    rbx,TAG
    jz      ileq1
    cmp     rax,rbx
    jle     RetTrue
    jmp     RetFalse
ileq1:
 ; First arg is short, second isn't
    test    byte ptr [rbx-1],16  ; 16 is the "negative" bit
    jz      RetTrue     ; Negative - always less
    jmp     RetFalse

ileq2:
 ; First arg is long
    test    rbx,TAG
    jz      ileq3
 ; First arg is long, second is short
    test    byte ptr [rax-1],16  ; 16 is the "negative" bit
    jnz     RetTrue    ; Positive - always greater
    jmp     RetFalse

ileq3:
    CALL_IO    POLY_SYS_int_leq

Mask_int_leq      EQU     (M_Reax OR M_Recx OR Mask_all)


PUBLIC  int_gtr
int_gtr:
    test    rax,TAG
    jz      igtr2
    test    rbx,TAG
    jz      igtr1
    cmp     rax,rbx
    jg      RetTrue
    jmp     RetFalse
igtr1:
 ; First arg is short, second isn't
    test    byte ptr [rbx-1],16  ; 16 is the "negative" bit
    jnz     RetTrue     ; Negative - always less
    jmp     RetFalse

igtr2:
 ; First arg is long
    test    rbx,TAG
    jz      igtr3
 ; First arg is long, second is short
    test    byte ptr [rax-1],16  ; 16 is the "negative" bit
    jz      RetTrue    ; Positive - always greater
    jmp     RetFalse

igtr3:
    CALL_IO    POLY_SYS_int_gtr

Mask_int_gtr      EQU     (M_Reax OR M_Recx OR Mask_all)


PUBLIC  int_lss
int_lss:
    test    rax,TAG
    jz      ilss2
    test    rbx,TAG
    jz      ilss1
    cmp     rax,rbx
    jl      RetTrue
    jmp     RetFalse
ilss1:
 ; First arg is short, second isn't
    test    byte ptr [rbx-1],16  ; 16 is the "negative" bit
    jz      RetTrue     ; Negative - always less
    jmp     RetFalse

ilss2:
 ; First arg is long
    test    rbx,TAG
    jz      ilss3
 ; First arg is long, second is short
    test    byte ptr [rax-1],16  ; 16 is the "negative" bit
    jnz     RetTrue    ; Positive - always greater
    jmp     RetFalse

ilss3:
    CALL_IO    POLY_SYS_int_lss

Mask_int_lss      EQU     (M_Reax OR M_Recx OR Mask_all)

PUBLIC  offset_address
offset_address:
 ; This is needed in the code generator, but is a very risky thing to do.
    shr     rbx,TAGSHIFT
    add     rax,rbx
    mov     rbx,rax
    ret

Mask_offset_address      EQU     (M_Reax OR M_Rebx)

; General test routine.  Returns with the condition codes set
; appropriately.

teststr:
    test    rax,TAG                 ; Is arg1 short
    jz      tststr1
    test    rbx,TAG                 ; Yes: is arg2 also short?
    jz      tststr0a
    ; Both are short - just compare the characters
    cmp     rax,rbx
    ret

tststr0a:
    mov     rdi,1                   ; Is arg2 the null string ?
    cmp     rdi,[rbx]
    jg      tststr4                 ; Return with "gtr" set if it is
    shr     rax,TAGSHIFT
    cmp     al,POLYWORDSIZE[rbx]
    jne     tststr4                 ; If they're not equal that's the result
    cmp     rax,256                 ; But if they're equal set "less" because A is less than B
    jmp     tststr4

tststr1: ; arg2 is not short.  Is arg1 ?
    test    rbx,TAG
    jz      tststr2
    mov     rdi,[rax]           ; Is arg1 the null string
    cmp     rdi,1
    jl      tststr4             ; Return with "less" set if it is
    shr     rbx,TAGSHIFT
    mov     cl,POLYWORDSIZE[rax]
    cmp     cl,bl
    jne     tststr4             ; If they're not equal that's the result
    cmp     rdi,0               ; But if they're equal set "greater" because A is greater than B
    jmp     tststr4

tststr2:
    mov     rdi,[rax]
    mov     rcx,[rbx]
    cmp     rdi,rcx
    jge     tststr3
    mov     rcx,rdi
tststr3:
    lea     rsi,POLYWORDSIZE[rax]       ; Load ptrs for cmpsb
    lea     rdi,POLYWORDSIZE[rbx]
    cld                 ; Make sure we increment
    cmp     rax,rax
    repe cmpsb          ; Compare while equal and rcx > 0
    jnz     tststr4
; Strings are equal as far as the shorter of the two.  Have to compare
; the lengths.
    mov     rdi,[rax]
    cmp     rdi,[rbx]
tststr4:
    mov     rax,1                   ; Clobber these
    mov     rbx,rax
    mov     rcx,rax
    mov     rsi,rax
    mov     rdi,rax
    ret

 ; These functions compare strings for lexical ordering.  This version, at
 ; any rate, assumes that they are UNSIGNED bytes.

PUBLIC  str_compare
str_compare:
    call    teststr
    ja      RetTrue         ; Return TAGGED(1) if it's greater
    je      RetFalse        ; Return TAGGED(0) if it's equal
    mov     rax,MINUS1
    ret

Mask_str_compare      EQU     (M_Reax OR M_Recx OR M_Redi OR M_Resi)


PUBLIC  teststrgeq
teststrgeq:
    call    teststr
    jnb     RetTrue
    jmp     RetFalse

Mask_teststrgeq      EQU     (M_Reax OR M_Recx OR M_Redi OR M_Resi)

PUBLIC  teststrleq
teststrleq:
    call    teststr
    jna     RetTrue
    jmp     RetFalse

Mask_teststrleq      EQU     (M_Reax OR M_Recx OR M_Redi OR M_Resi)

PUBLIC  teststrlss
teststrlss:
    call    teststr
    jb      RetTrue
    jmp     RetFalse

Mask_teststrlss      EQU     (M_Reax OR M_Recx OR M_Redi OR M_Resi)

PUBLIC  teststrgtr
teststrgtr:
    call    teststr
    ja      RetTrue
    jmp     RetFalse

Mask_teststrgtr      EQU     (M_Reax OR M_Recx OR M_Redi OR M_Resi)


PUBLIC  bytevec_eq
bytevec_eq:
 ; Compare arrays of bytes.  The arguments are the same as move_bytes.
 ; (source, sourc_offset, destination, dest_offset, length)

 ; Assume that the offsets and length are all short integers.
    mov     rdi,r8
    mov     rcx,r9
    shr     rcx,TAGSHIFT
    add     rdi,rcx
    mov     rsi,rax
    shr     rbx,TAGSHIFT
    add     rsi,rbx
    mov     rcx,r10
    shr     rcx,TAGSHIFT

    cld                     ; Make sure we increment
    cmp     rax,rax
    repe    cmpsb
    mov     rsi,rax             ; Make these valid
    mov     rcx,rax
    mov     rdi,rax
    jz      bvTrue
    mov     rax,FALSE
    jmp     bvRet
bvTrue:
    mov     rax,TRUE
bvRet:
    ret

Mask_bytevec_eq      EQU     (M_Reax OR M_Recx OR M_Redi OR M_Resi)


PUBLIC  is_big_endian
is_big_endian:
    jmp     RetFalse    ; I386/486 is little-endian

Mask_is_big_endian      EQU     (M_Reax)

PUBLIC  bytes_per_word
bytes_per_word:
    mov     rax,TAGGED(POLYWORDSIZE)
    ret

Mask_bytes_per_word      EQU     (M_Reax)

 ; Word functions.  These are all unsigned and do not raise Overflow
 
PUBLIC  mul_word
mul_word:
    shr     rbx,TAGSHIFT
    sub     rax,TAG
    mul     rbx                ; unsigned multiplication
    add     rax,TAG
    mov     rdx,rax
    mov     rbx,rax
    ret

Mask_mul_word      EQU     (M_Reax OR M_Rebx OR M_Redx)

PUBLIC  plus_word
plus_word:
    lea     rax,(-TAG)[rax+rbx]
    ret

Mask_plus_word      EQU     (M_Reax)

PUBLIC  minus_word
minus_word:
    sub     rax,rbx
    add     rax,TAG
    ret

Mask_minus_word      EQU     (M_Reax)

PUBLIC  div_word
div_word:
    shr     rbx,TAGSHIFT
    shr     rax,TAGSHIFT
    mov     rdx,0
    div     rbx
    lea     rax,1[rax*2]
    mov     rdx,rax
    mov     rbx,rax
    ret

Mask_div_word      EQU     (M_Reax OR M_Rebx OR M_Redx)

PUBLIC  mod_word
mod_word:
    shr     rbx,TAGSHIFT
    shr     rax,TAGSHIFT
    mov     rdx,0
    div     rbx
    lea     rax,1[rdx*2]
    mov     rdx,rax
    mov     rbx,rax
    ret

Mask_mod_word      EQU     (M_Reax OR M_Rebx OR M_Redx)

PUBLIC  word_eq
word_eq:
    cmp     rax,rbx
    jz      RetTrue         ; True if they are equal.
    jmp     RetFalse

Mask_word_eq      EQU     (M_Reax)

PUBLIC  word_geq
word_geq:
    cmp     rax,rbx
    jnb     RetTrue
    jmp     RetFalse

Mask_word_geq      EQU     (M_Reax)

PUBLIC  word_leq
word_leq:
    cmp     rax,rbx
    jna     RetTrue
    jmp     RetFalse

Mask_word_leq      EQU     (M_Reax)

PUBLIC  word_gtr
word_gtr:
    cmp     rax,rbx
    ja      RetTrue
    jmp     RetFalse

Mask_word_gtr      EQU     (M_Reax)

PUBLIC  word_lss
word_lss:
    cmp     rax,rbx
    jb      RetTrue
    jmp     RetFalse

Mask_word_lss      EQU     (M_Reax)

PUBLIC  fixed_geq
fixed_geq:
    cmp     rax,rbx
    jge     RetTrue
    jmp     RetFalse

Mask_fixed_geq      EQU     (M_Reax)

PUBLIC  fixed_leq
fixed_leq:
    cmp     rax,rbx
    jle     RetTrue
    jmp     RetFalse

Mask_fixed_leq      EQU     (M_Reax)

PUBLIC  fixed_gtr
fixed_gtr:
    cmp     rax,rbx
    jg      RetTrue
    jmp     RetFalse

Mask_fixed_gtr      EQU     (M_Reax)

PUBLIC  fixed_lss
fixed_lss:
    cmp     rax,rbx
    jl      RetTrue
    jmp     RetFalse

Mask_fixed_lss      EQU     (M_Reax)

PUBLIC  fixed_add
fixed_add:
    lea     rax,(-TAG)[rax]
    add     rax,rbx
    jo      raiseOverflowEx
    ret

Mask_fixed_add      EQU     (M_Reax)

PUBLIC  fixed_sub
fixed_sub:
    sub     rax,rbx
    jo      raiseOverflowEx
    add     rax,TAG
    ret

Mask_fixed_sub      EQU     (M_Reax)

PUBLIC  fixed_mul
fixed_mul:
    sar     rbx,TAGSHIFT
    sub     rax,TAG
    imul    rax,rbx
    jo      raiseOverflowEx
    add     rax,TAG
    mov     rdx,rax
    mov     rbx,rax
    ret

Mask_fixed_mul      EQU     (M_Reax OR M_Rebx OR M_Redx)

PUBLIC  fixed_quot
fixed_quot:
; Checking for overflow and zero is done in ML
    sar     rbx,TAGSHIFT
    sar     rax,TAGSHIFT
    cqo
    idiv    rbx
    lea     rax,1[rax*2]
    mov     rdx,rax
    mov     rbx,rax
    ret

Mask_fixed_quot      EQU     (M_Reax OR M_Rebx OR M_Redx)

PUBLIC  fixed_rem
fixed_rem:
    sar     rbx,TAGSHIFT
    sar     rax,TAGSHIFT
    cqo
    idiv    rbx
    lea     rax,1[rdx*2]
    mov     rdx,rax
    mov     rbx,rax
    ret

Mask_fixed_rem      EQU     (M_Reax OR M_Rebx OR M_Redx)

; TODO: This needs to be fixed.
PUBLIC  fixed_div
fixed_div:
    ; Checking for overflow and zero is done in ML
    sar     rbx,TAGSHIFT
    sar     rax,TAGSHIFT
    cqo
    idiv    rbx
    cmp     rdx,0
    jz      fixed_div1      ; If the remainder if non-zero ...
    xor     rbx,rdx
    jns     fixed_div1
    sub     rax,1
fixed_div1:
    lea     rax,1[rax*2]
    mov     rdx,rax
    mov     rbx,rax
    ret

Mask_fixed_div      EQU     (M_Reax OR M_Rebx OR M_Redx)

PUBLIC  fixed_mod
fixed_mod:
    sar     rbx,TAGSHIFT
    sar     rax,TAGSHIFT
    cqo
    idiv     rbx
; Result is in rdx.  We have to change the result so that it has the sign as the divisor.
    cmp     rdx,0
    jz      fixed_mod1  ; Result is zero - no change
    xor     rbx,rdx
    jns     fixed_mod1  ; Skip if they had the same signs
    xor     rbx,rdx
    add     rdx,rbx
fixed_mod1:
    lea     rax,1[rdx*2]
    mov     rdx,rax
    mov     rbx,rax
    ret

Mask_fixed_mod      EQU     (M_Reax OR M_Rebx OR M_Redx)

raiseOverflowEx:
; Build the exception packet.
; Allocate four word item.
; Set first word to TAGGED(5)
; Set second word to the string "Overflow" - That's going to need allocation.
; Set third and fourth words to TAGGED(0)
; Raise the exception.
    jmp     qword ptr [Fr_RaiseOverflow+rbp]

; Atomically increment the value at the address of the arg and return the
; updated value.  Since the xadd instruction returns the original value
; we have to increment it.
PUBLIC  atomic_increment
atomic_increment:
atomic_incr:                    ; Internal name in case "atomic_increment" is munged.
    mov     rbx,2
    lock xadd [rax],rbx
    add     rbx,2
    mov     rax,rbx
    ret

Mask_atomic_incr      EQU     (M_Reax OR M_Rebx)

; Atomically decrement the value at the address of the arg and return the
; updated value.  Since the xadd instruction returns the original value
; we have to decrement it.
PUBLIC  atomic_decrement
atomic_decrement:
atomic_decr:
    mov     rbx,-2
    lock xadd [rax],rbx
    mov     rax,rbx
    sub     rax,2
    ret

Mask_atomic_decr      EQU     (M_Reax OR M_Rebx)

; Reset a mutex to (tagged) one.  Because the increment and decrements
; are atomic this doesn't have to do anything special.
PUBLIC  atomic_reset
atomic_reset:
    mov     qword ptr [rax],3 
    mov     rax,1
    ret

Mask_atomic_reset      EQU     M_Reax

; Return the thread id object for the current thread
PUBLIC  thread_self
thread_self:
    mov     rax,Fr_ThreadId[rbp]
    ret

Mask_thread_self      EQU     (M_Reax)



; Memory for LargeWord.word values.  This is the same as mem_for_real on
; 64-bits but only a single word on 32-bits.
; ********************************
; Some of this code is temporary.  The final version should compute the result and
; simply jump here to box it.  That requires the heap-overflow code to save the
; registers across the trap but not to examine them for pointers.  Temporarily we
; don't do that but instead clear all the registers across a trap.
mem_for_largeword:
mem_for_real:
; Allocate memory for the result.
    mov     rcx,r15
    sub     rcx,16
    cmp     rcx,Fr_LocalMbottom[rbp]
    jnb      mem_for_real1
; ********************************
; Temporarily: push these registers to the stack
; and pop them afterwards.  This isn't the final version of this code
; but is useful as a test.
    push     rax
    push    rbx
    call     X86AsmCallExtraRETURN_HEAP_OVERFLOW
    pop     rbx
    pop     rax
mem_for_real1:
    mov     r15,rcx
    mov    qword ptr (-8)[rcx],1   ; One word
    mov    byte ptr (-1)[rcx],B_bytes  ; Set the byte flag.
    ret


PUBLIC  real_add
real_add:
    call    mem_for_real
; Do the operation and put the result in the allocated
; space.
    fld     qword ptr [rax]
    fadd    qword ptr [rbx]
    fstp    qword ptr [rcx]
    mov     rax,rcx
    ret

; The mask includes FP7 rather than FP0 because this pushes a value which
; overwrites the bottom of the stack.

Mask_real_add      EQU     (M_Reax OR M_Recx OR M_Redx OR M_FP7)

PUBLIC  real_sub
real_sub:
   call    mem_for_real
; Do the operation and put the result in the allocated
; space.
    fld     qword ptr [rax]
    fsub    qword ptr [rbx]
    fstp    qword ptr [rcx]
    mov     rax,rcx
    ret

Mask_real_sub      EQU     (M_Reax OR M_Recx OR M_Redx OR M_FP7)


PUBLIC  real_mul
real_mul:
    call    mem_for_real
; Do the operation and put the result in the allocated
; space.
    fld     qword ptr [rax]
    fmul    qword ptr [rbx]
    fstp    qword ptr [rcx]
    mov     rax,rcx
    ret

Mask_real_mul      EQU     (M_Reax OR M_Recx OR M_Redx OR M_FP7)


PUBLIC  real_div
real_div:
        call    mem_for_real
; Do the operation and put the result in the allocated
; space.
    fld     qword ptr [rax]
    fdiv    qword ptr [rbx]
    fstp    qword ptr [rcx]
    mov     rax,rcx
    ret

Mask_real_div      EQU     (M_Reax OR M_Recx OR M_Redx OR M_FP7)


; For all values except NaN it's possible to do this by a test such as
; "if x < 0.0 then ~ x else x" but the test always fails for NaNs

PUBLIC  real_abs
real_abs:
    mov     rbx,rax             ; Put a valid value in rbx
    call    mem_for_real
; Do the operation and put the result in the allocated
; space.
; N.B. Real.~ X is not the same as 0.0 - X.  Real.~ 0.0 is ~0.0;
    fld     qword ptr [rax]
    fabs
    fstp    qword ptr [rcx]
    mov     rax,rcx
    ret

Mask_real_abs      EQU     (M_Reax OR M_Rebx OR M_Recx OR M_Redx OR M_FP7)


PUBLIC  real_neg
real_neg:
            mov     rbx,rax
    call    mem_for_real
; Do the operation and put the result in the allocated
; space.
; N.B. Real.~ X is not the same as 0.0 - X.  Real.~ 0.0 is ~0.0;
    fld     qword ptr [rax]
    fchs
    fstp    qword ptr [rcx]
    mov     rax,rcx
    ret

Mask_real_neg      EQU     (M_Reax OR M_Rebx OR M_Recx OR M_Redx OR M_FP7)



PUBLIC  real_eq
real_eq:
    fld     qword ptr [rax]
    fcomp   qword ptr [rbx]
    fnstsw  ax
; Not all 64-bit processors support SAHF.
; The result is true if the zero flag is set and parity flag clear.  
    and     rax,17408           ; 0x4400
    cmp     rax,16384           ; 0x4000
    je      RetTrue
    jmp     RetFalse

Mask_real_eq      EQU     (M_Reax OR M_FP7)


PUBLIC  real_neq
real_neq:
    fld     qword ptr [rax]
    fcomp   qword ptr [rbx]
    fnstsw  ax
    and     rax,17408
    cmp     rax,16384
    jne     RetTrue
    jmp     RetFalse

Mask_real_neq      EQU     (M_Reax OR M_FP7)

PUBLIC  real_lss
real_lss:
; Compare rbx > rax
    fld     qword ptr [rbx]
    fcomp   qword ptr [rax]
    fnstsw  ax

; True if the carry flag (C0), zero flag (C3) and parity (C2) are all clear
    and     rax,17664

    je      RetTrue
    jmp     RetFalse

Mask_real_lss      EQU     (M_Reax OR M_FP7)


PUBLIC  real_gtr
real_gtr:
    fld     qword ptr [rax]
    fcomp   qword ptr [rbx]
    fnstsw  ax

; True if the carry flag (C0), zero flag (C3) and parity (C2) are all clear
    and     rax,17664
    je      RetTrue
    jmp     RetFalse

Mask_real_gtr      EQU     (M_Reax OR M_FP7)


PUBLIC  real_leq
real_leq:
; Compare rbx > rax
    fld     qword ptr [rbx]
    fcomp   qword ptr [rax]
    fnstsw  ax
; True if the carry flag (C0) and parity (C2) are both clear
    and     rax,1280

    je      RetTrue
    jmp     RetFalse

Mask_real_leq      EQU     (M_Reax OR M_FP7)


PUBLIC  real_geq
real_geq:
    fld     qword ptr [rax]
    fcomp   qword ptr [rbx]
    fnstsw  ax
; True if the carry flag (C0) and parity (C2) are both clear
    and     rax,1280

    je      RetTrue
    jmp     RetFalse

Mask_real_geq      EQU     (M_Reax OR M_FP7)

PUBLIC  real_from_int
real_from_int:
    test    rax,TAG
    jnz     fixed_to_real

real_float_1:
    CALL_IO    POLY_SYS_int_to_real

Mask_real_from_int      EQU     (M_Reax OR M_Recx OR M_Redx OR M_FP7 OR Mask_all)

PUBLIC  fixed_to_real
fixed_to_real:
    mov     rbx,rax                 ; Put a valid value in rbx
    call    mem_for_real
    sar     rax,TAGSHIFT
    push    rax                     ; Push it to the stack.  We can only load it from memory
    fild    qword ptr [rsp]
    pop     rax
    fstp    qword ptr [rcx]
    mov     rax,rcx
    ret

Mask_fixed_to_real      EQU     (M_Reax OR M_Rebx OR M_Recx OR M_Redx OR M_FP7)

PUBLIC  set_exception_trace
set_exception_trace:
; The argument is the closure to call.  The return address is already on the stack.
; We need to push the addresses of some code.  To make it position-independent we use
; CALLs in a non-standard way.
    mov     rdx,rax
    push    Fr_HandlerRegister[rbp]        ; Previous handler
    call    setexct1                    ; Jump to setexct1 pushing the next addr to the stack
    ; This is the code that is called if we get an exception.
    ; The exception packet is the first argument.
    CALL_IO POLY_SYS_give_ex_trace_fn

setexct1:
    mov     Fr_HandlerRegister[rbp],rsp
    call    setexct2                    ; Jump to setexct2 pushing the next addr to the stack
    ; This is the code that is called if we return without raising an exception
    add     rsp,POLYWORDSIZE
    pop     Fr_HandlerRegister[rbp]
    RET
setexct2:
    mov     rax,UNIT
    jmp     qword ptr [rdx]         ; Jump to the function

; Additional assembly code routines

; RTS call to kill the current thread. 
PUBLIC  X86AsmKillSelf
X86AsmKillSelf:
    CALL_IO POLY_SYS_kill_self

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

; LargeWord.word operations.  These are 32 or 64-bit values in a single-word byte
; memory cell.
PUBLIC  eq_longword
eq_longword:
    mov     rax,[rax]
    cmp     rax,[rbx]
    jz      RetTrue         ; True if they are equal.
    jmp     RetFalse

Mask_eq_longword      EQU     (M_Reax)

PUBLIC  geq_longword
geq_longword:
    mov     rax,[rax]
    cmp     rax,[rbx]
    jnb     RetTrue
    jmp     RetFalse

Mask_geq_longword      EQU     (M_Reax)

PUBLIC  leq_longword
leq_longword:
    mov     rax,[rax]
    cmp     rax,[rbx]
    jna     RetTrue
    jmp     RetFalse

Mask_leq_longword      EQU     (M_Reax)

PUBLIC  gt_longword
gt_longword:
    mov     rax,[rax]
    cmp     rax,[rbx]
    ja      RetTrue
    jmp     RetFalse

Mask_gt_longword      EQU     (M_Reax)

PUBLIC  lt_longword
lt_longword:
    mov     rax,[rax]
    cmp     rax,[rbx]
    jb      RetTrue
    jmp     RetFalse

Mask_lt_longword      EQU     (M_Reax)

PUBLIC  longword_to_tagged
longword_to_tagged:
; Load the value and tag it, discarding the top bit
    mov     rax,[rax]
    lea     rax,1[rax*2]
    ret

Mask_longword_to_tagged      EQU     (M_Reax)

PUBLIC  signed_to_longword
signed_to_longword:
; Shift the value to remove the tag and store it.
    mov     rbx,rax                     ; mem_for_largeword may push rebx
    call    mem_for_largeword
    sar     rax,TAGSHIFT                ; Arithmetic shift, preserve sign
    mov     [rcx],rax
    mov     rax,rcx
    ret

Mask_signed_to_longword      EQU     (M_Reax OR M_Rebx OR M_Recx)

PUBLIC  unsigned_to_longword
unsigned_to_longword:
; Shift the value to remove the tag and store it.
    mov     rbx,rax                     ; mem_for_largeword may push rebx
    call    mem_for_largeword
    shr     rax,TAGSHIFT                ; Logical shift, zero top bit
    mov     [rcx],rax
    mov     rax,rcx
    ret

Mask_unsigned_to_longword      EQU     (M_Reax OR M_Rebx OR M_Recx)

PUBLIC  plus_longword
plus_longword:
    call    mem_for_largeword
    mov     rax,[rax]
    add     rax,[rbx]
    mov     [rcx],rax
    mov     rax,rcx
    ret

Mask_plus_longword      EQU     (M_Reax OR M_Recx)

PUBLIC  minus_longword
minus_longword:
    call    mem_for_largeword
    mov     rax,[rax]
    sub     rax,[rbx]
    mov     [rcx],rax
    mov     rax,rcx
    ret

Mask_minus_longword      EQU     (M_Reax OR M_Recx)

PUBLIC  mul_longword
mul_longword:
    call    mem_for_largeword
    mov     rax,[rax]
    mul     qword ptr [rbx]
    mov     [rcx],rax
    mov     rax,rcx
    mov     rdx,rax             ; clobber this which has the high-end result
    ret

Mask_mul_longword      EQU     (M_Reax OR M_Recx OR M_Redx)

PUBLIC  div_longword
div_longword:
; Check for division by zero is done in ML
    call    mem_for_largeword
    mov     rax,[rax]
    mov     rdx,0
    div     qword ptr [rbx]
    mov     [rcx],rax           ; Store the quotient
    mov     rax,rcx
    mov     rdx,rax             ; clobber this which has the remainder
    ret

Mask_div_longword      EQU     (M_Reax OR M_Recx OR M_Redx)

PUBLIC  mod_longword
mod_longword:
; Check for division by zero is done in ML
    call    mem_for_largeword
    mov     rax,[rax]
    mov     rdx,0
    div     qword ptr [rbx]
    mov     [rcx],rdx           ; Store the remainder
    mov     rax,rcx
    mov     rdx,rax             ; clobber this which has the remainder
    ret

Mask_mod_longword      EQU     (M_Reax OR M_Recx OR M_Redx)

PUBLIC  andb_longword
andb_longword:
    call    mem_for_largeword
    mov     rax,[rax]
    and     rax,[rbx]
    mov     [rcx],rax
    mov     rax,rcx
    ret

Mask_andb_longword      EQU     (M_Reax OR M_Recx)

PUBLIC  orb_longword
orb_longword:
    call    mem_for_largeword
    mov     rax,[rax]
    or      rax,[rbx]
    mov     [rcx],rax
    mov     rax,rcx
    ret

Mask_orb_longword      EQU     (M_Reax OR M_Recx)

PUBLIC  xorb_longword
xorb_longword:
    call    mem_for_largeword
    mov     rax,[rax]
    xor     rax,[rbx]
    mov     [rcx],rax
    mov     rax,rcx
    ret

Mask_xorb_longword      EQU     (M_Reax OR M_Recx)

PUBLIC  shift_left_longword
shift_left_longword:
    call    mem_for_largeword
    mov     rdx,rcx         ; We need rcx for the shift
; The shift value is always a Word.word value i.e. tagged
; LargeWord.<<(a,b) is defined to return 0 if b > LargeWord.wordSize

    cmp     rbx,TAGGED(64)
    jb      sllw1
    mov     rax,0
    jmp     sllw2
sllw1:
    mov     rcx,rbx
    shr     rcx,TAGSHIFT
    mov     rax,[rax]
    shl     rax,cl
sllw2:
    mov     [rdx],rax
    mov     rax,rdx
    mov     rcx,rax             ; Clobber rcx
    ret

Mask_shift_left_longword      EQU     (M_Reax OR M_Recx OR M_Redx)

PUBLIC  shift_right_longword
shift_right_longword:
    call    mem_for_largeword
    mov     rdx,rcx             ; We need rcx for the shift
; The shift value is always a Word.word value i.e. tagged
; LargeWord.>>(a,b) is defined to return 0 if b > LargeWord.wordSize
    cmp     rbx,TAGGED(64)
    jb      srlw1
    mov     rax,0
    jmp     srlw2
srlw1:
    mov     rcx,rbx
    shr     rcx,TAGSHIFT            ; remove tag
    mov     rax,[rax]
    shr     rax,cl
srlw2:
    mov     [rdx],rax
    mov     rax,rdx
    mov     rcx,rax                 ; Clobber rcx
    ret

Mask_shift_right_longword      EQU     (M_Reax OR M_Recx OR M_Redx)

PUBLIC  shift_right_arith_longword
shift_right_arith_longword:
    call    mem_for_largeword
    mov     rdx,rcx
; The shift value is always a Word.word value i.e. tagged
; LargeWord.~>>(a,b) is defined to return 0 or ~1 if b > LargeWord.wordSize
    cmp     rbx,TAGGED(64)
    jb      sralw1
; Setting the shift to 31/63 propagates the sign bit
    mov     rbx,TAGGED(63)
sralw1:
    mov     rcx,rbx
    shr     rcx,TAGSHIFT
    mov     rax,[rax]
    sar     rax,cl
    mov     [rdx],rax
    mov     rax,rdx
    mov     rcx,rax
    ret

Mask_shift_right_arith_longword      EQU     (M_Reax OR M_Rebx OR M_Recx OR M_Redx)

; C-memory operations.
PUBLIC  cmem_load_asm_8
cmem_load_asm_8:
    mov     rax,[rax]
    sar     rbx,TAGSHIFT
    add     rax,rbx
    mov     rbx,r8
    sar     rbx,TAGSHIFT
    movzx   rax, byte ptr [rax][rbx]
    lea     rax,1[rax*2]
    mov     rbx,rax
    ret

Mask_cmem_load_8      EQU     (M_Reax OR M_Rebx)

PUBLIC  cmem_load_asm_16
cmem_load_asm_16:
    mov     rax,[rax]
    sar     rbx,TAGSHIFT
    add     rax,rbx
    mov     rbx,r8
    ; The index is tagged but since we want to multiply by two we don't need anything here.
    movzx   rax, word ptr [rax-1][rbx]
    lea     rax,1[rax*2]
    ret

Mask_cmem_load_16      EQU     (M_Reax OR M_Rebx)

PUBLIC  cmem_load_asm_32
cmem_load_asm_32:
; 64-bit mode - the result is tagged
    mov     rax,[rax]
    sar     rbx,TAGSHIFT
    add     rax,rbx
    mov     eax, dword ptr [rax-2][r8*2]
    lea     rax,1[rax*2]
    mov     rbx,rax
    ret

Mask_cmem_load_32      EQU     (M_Reax OR M_Rebx)


PUBLIC  cmem_load_asm_64
cmem_load_asm_64:
    push    r8                      ; Save r8 which isn't saved in mem_for_largeword
    call    mem_for_largeword
    pop r8
    mov     rax,[rax]
    sar     rbx,TAGSHIFT
    add     rax,rbx
    mov     rax,(-4)[rax+r8*4]
    mov     [rcx],rax
    mov     rax,rcx
    mov     rbx,rax
    ret

Mask_cmem_load_64      EQU     (M_Reax OR M_Rebx OR M_Recx)

PUBLIC  cmem_load_asm_float
cmem_load_asm_float:
    push    r8                      ; Save r8 which isn't saved in mem_for_largeword
    call    mem_for_real
    pop r8                      ; Save r8 which isn't saved in mem_for_largeword
    mov     rax,[rax]
    sar     rbx,TAGSHIFT
    add     rax,rbx
    mov     rbx,r8
    fld     dword ptr [rax-2][rbx*2]
    fstp    qword ptr [rcx]
    mov     rax,rcx
    ret

Mask_cmem_load_float      EQU     (M_Reax OR M_Rebx OR M_Recx OR M_FP7)

PUBLIC  cmem_load_asm_double
cmem_load_asm_double:
    push    r8                      ; Save r8 which isn't saved in mem_for_largeword
    call    mem_for_real
    pop     r8
    mov     rax,[rax]
    sar     rbx,TAGSHIFT
    add     rax,rbx
    mov     rbx,r8
    fld     qword ptr [rax-4][rbx*4]
    fstp    qword ptr [rcx]
    mov     rax,rcx
    ret

Mask_cmem_load_double      EQU     (M_Reax OR M_Rebx OR M_Recx OR M_FP7)
   
PUBLIC  cmem_store_asm_8
cmem_store_asm_8:
    mov     rax,[rax]
    sar     rbx,TAGSHIFT
    add     rax,rbx
    mov     rbx,r8
    mov     rcx,r9
    sar     rbx,TAGSHIFT
    sar     rcx,TAGSHIFT
    mov     [rax+rbx],cl
    mov     rax,UNIT
    mov     rbx,rax             ; Clobber bad value in rbx
    mov     rcx,rax             ; and rcx
    ret

Mask_cmem_store_8      EQU     (M_Reax OR M_Rebx OR M_Recx)

PUBLIC  cmem_store_asm_16
cmem_store_asm_16:
    mov     rax,[rax]
    sar     rbx,TAGSHIFT
    add     rax,rbx
    mov     rbx,r8
    mov     rcx,r9
    sar     rcx,TAGSHIFT
    mov     word ptr [rax-1][rbx],cx
    mov     rax,UNIT
    mov     rcx,rax
    ret

Mask_cmem_store_16      EQU     (M_Reax OR M_Rebx OR M_Recx)

PUBLIC  cmem_store_asm_32
cmem_store_asm_32:
    mov     rax,[rax]
    sar     rbx,TAGSHIFT
    add     rax,rbx
    mov     rbx,r8
    mov     rcx,r9
    sar     rcx,TAGSHIFT
    mov     dword ptr [rax-2][rbx*2],ecx
    mov     rax,UNIT
    mov     rcx,rax
    ret

Mask_cmem_store_32      EQU     (M_Reax OR M_Rebx OR M_Recx)

PUBLIC  cmem_store_asm_64       ; The value is boxed in 64-bit mode. Not implemented in 32-bit mode
cmem_store_asm_64:
    mov     rax,[rax]                   ; The address is boxed.
    sar     rbx,TAGSHIFT                ; The offset is a signed tagged value
    add     rax,rbx                     ; Add it in
    mov     rbx,[r9]                    ; Value to store
    mov     (-4)[rax+r8*4],rbx          ; Store it
    mov     rax,UNIT                    ; The function returns unit
    mov     rbx,rax                     ; Bad value in rbx
    ret

Mask_cmem_store_64      EQU     (M_Reax OR M_Rebx OR M_Recx)

PUBLIC  cmem_store_asm_float
cmem_store_asm_float:
    mov     rax,[rax]                   ; The address is boxed.
    sar     rbx,TAGSHIFT                ; The offset is a signed tagged value
    add     rax,rbx                     ; Add it in
    mov     rbx,r8                      ; Get the index.
    mov     rcx,r9                      ; Get the address of the real
    fld     qword ptr [rcx] 
    fstp    dword ptr [rax-2][rbx*2]
    mov     rax,UNIT
    mov     rbx,rax
    ret

Mask_cmem_store_float      EQU     (M_Reax OR M_Rebx OR M_Recx OR M_FP7)

PUBLIC  cmem_store_asm_double
cmem_store_asm_double:
    mov     rax,[rax]                   ; The address is boxed.
    sar     rbx,TAGSHIFT                ; The offset is a signed tagged valu
    add     rax,rbx                     ; Add it in
    mov     rbx,r8                      ; Get the index.
    mov     rcx,r9                      ; Get the address of the real
    fld     qword ptr [rcx] 
    fstp    qword ptr [rax-4][rbx*4]
    mov     rax,UNIT                    ; The function returns unit
    mov     rbx,rax                     ; Bad value in rbx
    ret

Mask_cmem_store_double      EQU     (M_Reax OR M_Rebx OR M_Recx OR M_FP7)

; IO call and extra-call macros.  POLY_SYS_xx entries are #defined in sys.h so we have to
; separate out the entry points from the call numbers.
CREATE_IO_CALL MACRO entrypt,index
PUBLIC  entrypt
entrypt:
    CALL_IO index
    ENDM

CREATE_EXTRA_CALL MACRO index
PUBLIC  X86AsmCallExtra&index&
X86AsmCallExtra&index&:
    CALL_EXTRA index
    ENDM


CREATE_IO_CALL CallPOLY_SYS_exit,POLY_SYS_exit
;  CREATE_IO_CALL CallPOLY_SYS_chdir,POLY_SYS_chdir
 CREATE_IO_CALL CallPOLY_SYS_get_flags,POLY_SYS_get_flags
 CREATE_IO_CALL CallPOLY_SYS_profiler,POLY_SYS_profiler
 CREATE_IO_CALL CallPOLY_SYS_Real_str,POLY_SYS_Real_str
 CREATE_IO_CALL CallPOLY_SYS_Real_Dispatch,POLY_SYS_Real_Dispatch
 CREATE_IO_CALL CallPOLY_SYS_conv_real,POLY_SYS_conv_real
 CREATE_IO_CALL CallPOLY_SYS_real_to_int,POLY_SYS_real_to_int
 CREATE_IO_CALL CallPOLY_SYS_sqrt_real,POLY_SYS_sqrt_real
 CREATE_IO_CALL CallPOLY_SYS_sin_real,POLY_SYS_sin_real
 CREATE_IO_CALL CallPOLY_SYS_signal_handler,POLY_SYS_signal_handler
 CREATE_IO_CALL CallPOLY_SYS_os_specific,POLY_SYS_os_specific
 CREATE_IO_CALL CallPOLY_SYS_network,POLY_SYS_network
 CREATE_IO_CALL CallPOLY_SYS_io_dispatch,POLY_SYS_io_dispatch
 CREATE_IO_CALL CallPOLY_SYS_poly_specific,POLY_SYS_poly_specific
 CREATE_IO_CALL CallPOLY_SYS_set_code_constant,POLY_SYS_set_code_constant
 CREATE_IO_CALL CallPOLY_SYS_code_flags,POLY_SYS_code_flags
 CREATE_IO_CALL CallPOLY_SYS_shrink_stack,POLY_SYS_shrink_stack
 CREATE_IO_CALL CallPOLY_SYS_process_env,POLY_SYS_process_env
 CREATE_IO_CALL CallPOLY_SYS_foreign_dispatch,POLY_SYS_foreign_dispatch
 CREATE_IO_CALL CallPOLY_SYS_ffi,POLY_SYS_ffi
 CREATE_IO_CALL CallPOLY_SYS_stack_trace,POLY_SYS_stack_trace
 CREATE_IO_CALL CallPOLY_SYS_full_gc,POLY_SYS_full_gc
 CREATE_IO_CALL CallPOLY_SYS_XWindows,POLY_SYS_XWindows
 CREATE_IO_CALL CallPOLY_SYS_timing_dispatch,POLY_SYS_timing_dispatch
 CREATE_IO_CALL CallPOLY_SYS_kill_self,POLY_SYS_kill_self
 CREATE_IO_CALL CallPOLY_SYS_thread_dispatch,POLY_SYS_thread_dispatch
 CREATE_IO_CALL CallPOLY_SYS_io_operation,POLY_SYS_io_operation
 CREATE_IO_CALL CallPOLY_SYS_ln_real,POLY_SYS_ln_real
 CREATE_IO_CALL CallPOLY_SYS_exp_real,POLY_SYS_exp_real
 CREATE_IO_CALL CallPOLY_SYS_arctan_real,POLY_SYS_arctan_real
 CREATE_IO_CALL CallPOLY_SYS_cos_real,POLY_SYS_cos_real

 CREATE_EXTRA_CALL RETURN_HEAP_OVERFLOW
 CREATE_EXTRA_CALL RETURN_STACK_OVERFLOW
 CREATE_EXTRA_CALL RETURN_STACK_OVERFLOWEX
 CREATE_EXTRA_CALL RETURN_RAISE_OVERFLOW

; Direct calls to the RTS

; chdir.  May raise an exception which means it may allocate space for the exception packet.
; The caller is supposed to preserve RBP
EXTRN   X86ChDir:PROC
PUBLIC  CallPOLY_SYS_chdir
CallPOLY_SYS_chdir:
    mov     rcx,[Fr_Argument+rbp]
    mov     qword ptr [Arg_ExceptionPacket+rcx],1           ; Clear exception
    mov     r15,[Arg_LocalMpointer+rcx]
    mov     rcx,rsp                                         ; ML stack ptr
    mov     rsp,rbp                                         ; Switch to C stack
    sub     rsp,Fr_size
    mov     [Fr_SavedMLSP+rbp],rcx
    ; X86-64 Windows calling conventions.
    mov     rdx,rax                                         ; Second argument
    sub     rsp,32                                          ; Create save area
    mov     rcx,[Fr_Argument+rbp]                           ; First argument
    call    X86ChDir
    mov     rsp,[Fr_SavedMLSP+rbp]                          ; Restore the ML stack ptr
    mov     ecx,[Fr_Argument+rbp]
    mov     r15,[Arg_LocalMpointer+rcx]                     ; Update heap pointer
    mov     rdx,[Arg_LocalMbottom+rcx]
    mov     [Fr_LocalMbottom+rbp],rdx                       ; and heap base because we may have GCd
    cmp     qword ptr [Arg_ExceptionPacket+rcx],1
    jne     cpsdraisex
    ret

cpsdraisex:
    mov     rax,[Arg_ExceptionPacket+rcx]
    jmp     raisex


; Register mask vector. - extern int registerMaskVector[];
; Each entry in this vector is a set of the registers modified
; by the function.  It is an untagged bitmap with the registers
; encoded in the same way as the 
    align   4
    PUBLIC  registerMaskVector
registerMaskVector  dd  Mask_all                ; 0 is unused
    dd  Mask_all                 ; 1
    dd  Mask_all                 ; 2
    dd  Mask_all                 ; 3 is unused
    dd  Mask_all                 ; 4 is unused
    dd  Mask_all                 ; 5 is unused
    dd  Mask_all                 ; 6
    dd  Mask_all                 ; 7 is unused
    dd  Mask_all                 ; 8 is unused
    dd  Mask_all                 ; 9
    dd  Mask_all                 ; 10 is unused
    dd  Mask_alloc_store         ; 11
    dd  Mask_alloc_uninit        ; 12
    dd  Mask_all                 ; return = 13
    dd  Mask_all                 ; raisex = 14
    dd  Mask_get_length          ; 15
    dd  Mask_all                 ; 16 is unused
    dd  Mask_all                 ; 17
    dd  Mask_all                 ; 18 is no longer used
    dd  Mask_all                 ; 19 is no longer used
    dd  Mask_all                 ; 20 is no longer used
    dd  Mask_all                 ; 21 is unused
    dd  Mask_all                 ; 22 is unused
    dd  Mask_str_compare         ; 23
    dd  Mask_all                 ; 24 is unused
    dd  Mask_all                 ; 25 is unused
    dd  Mask_teststrgtr          ; 26
    dd  Mask_teststrlss          ; 27
    dd  Mask_teststrgeq          ; 28
    dd  Mask_teststrleq          ; 29
    dd  Mask_all                 ; 30
    dd  Mask_all                 ; 31 is no longer used
    dd  Mask_all                 ; exception_trace_fn 32 - Calls unknown function
    dd  Mask_all                 ; 33 is no longer used
    dd  Mask_all                 ; 34 is no longer used
    dd  Mask_all                 ; 35 is no longer used
    dd  Mask_all                 ; 36 is no longer used
    dd  Mask_all                 ; 37 is unused
    dd  Mask_all                 ; 38 is unused
    dd  Mask_all                 ; 39 is unused
    dd  Mask_all                 ; 40
    dd  Mask_all                 ; 41 is unused
    dd  Mask_all                 ; 42
    dd  Mask_all                 ; 43
    dd  Mask_all                 ; 44 is no longer used
    dd  Mask_all                 ; 45 is no longer used
    dd  Mask_all                 ; 46
    dd  Mask_lockseg             ; 47
    dd  Mask_all                 ; nullorzero = 48
    dd  Mask_all                 ; 49 is no longer used
    dd  Mask_all                 ; 50 is no longer used
    dd  Mask_all                 ; 51
    dd  Mask_all                 ; 52
    dd  Mask_eq_longword         ; 53
    dd  Mask_all                 ; 54 is no longer used
    dd  Mask_geq_longword        ; 55
    dd  Mask_leq_longword        ; 56
    dd  Mask_gt_longword         ; 57
    dd  Mask_lt_longword         ; 58
    dd  Mask_all                 ; 59 is unused
    dd  Mask_all                 ; 60 is unused
    dd  Mask_all                 ; 61
    dd  Mask_all                 ; 62
    dd  Mask_all                 ; 63 is unused
    dd  Mask_all                 ; 64 is unused
    dd  Mask_all                 ; 65 is unused
    dd  Mask_all                 ; 66 is unused
    dd  Mask_all                 ; 67 is unused
    dd  Mask_all                 ; 68 is unused
    dd  Mask_atomic_reset        ; 69
    dd  Mask_atomic_incr         ; 70
    dd  Mask_atomic_decr         ; 71
    dd  Mask_thread_self         ; 72
    dd  Mask_all                 ; 73
    dd  Mask_plus_longword       ; 74
    dd  Mask_minus_longword      ; 75
    dd  Mask_mul_longword        ; 76
    dd  Mask_div_longword        ; 77
    dd  Mask_mod_longword        ; 78
    dd  Mask_andb_longword       ; 79
    dd  Mask_orb_longword        ; 80
    dd  Mask_xorb_longword       ; 81
    dd  Mask_all                 ; 82 is unused
    dd  Mask_all                 ; 83 is now unused
    dd  Mask_all                 ; 84
    dd  Mask_shift_left_longword ; 85
    dd  Mask_shift_right_longword ; 86
    dd  Mask_shift_right_arith_longword ; 87
    dd  Mask_all                 ; 88
    dd  Mask_longword_to_tagged  ; 89
    dd  Mask_signed_to_longword  ; 90
    dd  Mask_unsigned_to_longword ; 91
    dd  Mask_all                 ; 92
    dd  Mask_all                 ; 93
    dd  Mask_all                 ; 94
    dd  Mask_all                 ; 95 is unused
    dd  Mask_all                 ; 96 is unused
    dd  Mask_all                 ; 97 is unused
    dd  Mask_all                 ; 98
    dd  Mask_all                 ; 99
    dd  Mask_all                 ; 100
    dd  Mask_all                 ; 101 is unused
    dd  Mask_all                 ; 102 is unused
    dd  Mask_all                 ; 103
    dd  Mask_quotrem             ; 104
    dd  Mask_is_short            ; 105
    dd  Mask_aplus               ; 106
    dd  Mask_aminus              ; 107
    dd  Mask_amul                ; 108
    dd  Mask_adiv                ; 109
    dd  Mask_amod                ; 110
    dd  Mask_aneg                ; 111
    dd  Mask_xora                ; 112
    dd  Mask_equala              ; 113
    dd  Mask_ora                 ; 114
    dd  Mask_anda                ; 115
    dd  Mask_all                 ; 116 is unused
    dd  Mask_all                 ; 117
    dd  Mask_real_geq            ; 118
    dd  Mask_real_leq            ; 119
    dd  Mask_real_gtr            ; 120
    dd  Mask_real_lss            ; 121
    dd  Mask_real_eq             ; 122
    dd  Mask_real_neq            ; 123
    dd  Mask_all                 ; 124
    dd  Mask_real_add            ; 125
    dd  Mask_real_sub            ; 126
    dd  Mask_real_mul            ; 127
    dd  Mask_real_div            ; 128
    dd  Mask_real_abs            ; 129
    dd  Mask_real_neg            ; 130
    dd  Mask_all                 ; 131 is unused
    dd  Mask_all                 ; 132
    dd  Mask_all                 ; 133
    dd  Mask_all                 ; 134
    dd  Mask_real_from_int       ; 135
    dd  Mask_all                 ; 136
    dd  Mask_all                 ; 137
    dd  Mask_all                 ; 138
    dd  Mask_all                 ; 139
    dd  Mask_all                 ; 140
    dd  Mask_all                 ; 141
    dd  Mask_fixed_to_real       ; 142
    dd  Mask_all                 ; 143 is unused
    dd  Mask_all                 ; 144 is unused
    dd  Mask_all                 ; 145 is unused
    dd  Mask_all                 ; 146 is unused
    dd  Mask_all                 ; 147 is unused
    dd  Mask_all                 ; stdin = 148
    dd  Mask_all                 ; stdout= 149
    dd  Mask_all                 ; 150
    dd  Mask_set_string_length   ; 151
    dd  Mask_get_first_long_word ; 152
    dd  Mask_all                 ; poly_specific = 153
    dd  Mask_bytevec_eq          ; 154
    dd  Mask_all                 ; 155 is unused
    dd  Mask_all                 ; 156 is unused
    dd  Mask_all                 ; 157 is unused
    dd  Mask_all                 ; 158 is unused
    dd  Mask_all                 ; 159 is unused
    dd  Mask_cmem_load_8         ; 160
    dd  Mask_cmem_load_16        ; 161
    dd  Mask_cmem_load_32        ; 162
    dd  Mask_cmem_load_64        ; 163
    dd  Mask_cmem_load_float     ; 164
    dd  Mask_cmem_load_double    ; 165
    dd  Mask_cmem_store_8        ; 166
    dd  Mask_cmem_store_16       ; 167
    dd  Mask_cmem_store_32       ; 168
    dd  Mask_cmem_store_64       ; 169
    dd  Mask_cmem_store_float    ; 170
    dd  Mask_cmem_store_double   ; 171
    dd  Mask_all                 ; 172 is unused
    dd  Mask_all                 ; 173 is unused
    dd  Mask_all                 ; 174 is unused
    dd  Mask_all                 ; 175 is unused
    dd  Mask_all                 ; 176 is unused
    dd  Mask_all                 ; 177 is unused
    dd  Mask_all                 ; 178 is unused
    dd  Mask_all                 ; 179 is unused
    dd  Mask_fixed_add           ; 180
    dd  Mask_fixed_sub           ; 181
    dd  Mask_fixed_mul           ; 182
    dd  Mask_fixed_quot          ; 183
    dd  Mask_fixed_rem           ; 184
    dd  Mask_fixed_div           ; 185
    dd  Mask_fixed_mod           ; 186
    dd  Mask_all                 ; 187 is unused
    dd  Mask_all                 ; 188 is unused
    dd  Mask_all                 ; 189
    dd  Mask_all                 ; 190
    dd  Mask_all                 ; 191 is no longer used
    dd  Mask_all                 ; 192 is unused
    dd  Mask_move_words          ; 193
    dd  Mask_all                 ; 194
    dd  Mask_move_words          ; 195
    dd  Mask_shift_right_arith_word  ; 196
    dd  Mask_int_to_word         ; 197
    dd  Mask_move_bytes          ; 198
    dd  Mask_move_bytes          ; 199
    dd  Mask_all                 ; 200
    dd  Mask_all                 ; 201
    dd  Mask_all                 ; stderr = 202
    dd  Mask_all                 ; 203 now unused
    dd  Mask_callcodeTupled      ; 204 - callcode
    dd  Mask_all                 ; 205
    dd  Mask_all                 ; 206
    dd  Mask_all                 ; 207 is unused
    dd  Mask_all                 ; 208 now unused
    dd  Mask_all                 ; 209
    dd  Mask_all                 ; 210 is unused
    dd  Mask_all                 ; 211 is unused
    dd  Mask_all                 ; 212 is unused
    dd  Mask_is_big_endian       ; 213
    dd  Mask_bytes_per_word      ; 214
    dd  Mask_offset_address      ; 215
    dd  Mask_shift_right_word    ; 216
    dd  Mask_all                 ; 217 - no longer used
    dd  Mask_not_bool            ; 218
    dd  Mask_fixed_geq           ; 219
    dd  Mask_fixed_leq           ; 220
    dd  Mask_fixed_gtr           ; 221
    dd  Mask_fixed_lss           ; 222
    dd  Mask_string_length       ; 223
    dd  Mask_all                 ; 224 is unused
    dd  Mask_all                 ; 225 is unused
    dd  Mask_all                 ; 226 is unused
    dd  Mask_all                 ; 227 is unused
    dd  Mask_touch_final         ; 228
    dd  Mask_all                 ; 229 - no longer used
    dd  Mask_all                 ; 230 - no longer used
    dd  Mask_int_geq             ; 231
    dd  Mask_int_leq             ; 232
    dd  Mask_int_gtr             ; 233
    dd  Mask_int_lss             ; 234
    dd  Mask_load_byte           ; load_byte_immut = 235
    dd  Mask_load_word           ; load_word_immut = 236
    dd  Mask_all                 ; 237 is unused
    dd  Mask_mul_word            ; 238
    dd  Mask_plus_word           ; 239
    dd  Mask_minus_word          ; 240
    dd  Mask_div_word            ; 241
    dd  Mask_or_word             ; 242
    dd  Mask_and_word            ; 243
    dd  Mask_xor_word            ; 244
    dd  Mask_shift_left_word     ; 245
    dd  Mask_mod_word            ; 246
    dd  Mask_word_geq            ; 247
    dd  Mask_word_leq            ; 248
    dd  Mask_word_gtr            ; 249
    dd  Mask_word_lss            ; 250
    dd  Mask_word_eq             ; 251
    dd  Mask_load_byte           ; 252
    dd  Mask_load_word           ; 253
    dd  Mask_assign_byte         ; 254
    dd  Mask_assign_word         ; 255

END
