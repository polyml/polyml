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
Arg_LocalMpointer   EQU     0
Arg_HandlerRegister EQU     4
Arg_LocalMbottom    EQU     8
Arg_StackLimit      EQU     12
Arg_ExceptionPacket EQU     16  ;# Address of packet to raise
Arg_RequestCode     EQU     20  ;# Byte: Io function to call.
Arg_ReturnReason    EQU     22  ;# Byte: Reason for returning from ML.
Arg_FullRestore     EQU     23  ;# Byte: Full/partial restore
Arg_PolyStack       EQU     24  ;# Current stack base
Arg_ThreadId        EQU     28  ;# My thread id

; Codes to indicate the reason for return.  Zero indicates an RTS call.
RETURN_HEAP_OVERFLOW        EQU 1
RETURN_STACK_OVERFLOW       EQU 2
RETURN_STACK_OVERFLOWEX     EQU 3
RETURN_CALLBACK_RETURN      EQU 6
RETURN_CALLBACK_EXCEPTION   EQU 7
RETURN_RAISE_OVERFLOW       EQU 8

;
; Starting offsets
PC_OFF      EQU     4
SP_OFF      EQU     8
EAX_OFF     EQU     20
EBX_OFF     EQU     24
ECX_OFF     EQU     28
EDX_OFF     EQU     32
ESI_OFF     EQU     36
EDI_OFF     EQU     40
FPREGS_OFF  EQU     44

;
; CODE STARTS HERE
;
    .CODE

; Define standard call macro. CALL_IO ioCallNo  where ioCallNo is the io function to call.
; These are defined as Masm macros because there are multiple instructions.

CALL_IO    MACRO   index
    mov     ecx,[Fr_Argument+ebp]
    mov     byte ptr [Arg_RequestCode+ecx],index
    jmp     SaveStateAndReturnLocal
ENDM

CALL_EXTRA  MACRO   index
    push    ecx
    mov     ecx,[Fr_Argument+ebp]
    mov     byte ptr [Arg_ReturnReason+ecx],index
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
    mov     eax,[Arg_StackLimit+ecx]
    mov     [Fr_StackLimit+ebp],eax
    mov     eax,[Arg_LocalMbottom+ecx]
    mov     [Fr_LocalMbottom+ebp],eax
    mov     eax,[Arg_HandlerRegister+ecx]
    mov     [Fr_HandlerRegister+ebp],eax
    mov     eax,[Arg_LocalMpointer+ecx]
    mov     [Fr_LocalMpointer+ebp],eax
    mov     eax,offset X86AsmCallExtraRETURN_STACK_OVERFLOWEX
    mov     [Fr_StackOverflowEx+ebp],eax
    mov     eax,offset X86AsmCallExtraRETURN_STACK_OVERFLOW
    mov     [Fr_StackOverflow+ebp],eax
    mov     eax,offset X86AsmCallExtraRETURN_HEAP_OVERFLOW
    mov     [Fr_HeapOverflow+ebp],eax
    mov     eax,offset X86AsmCallExtraRETURN_RAISE_OVERFLOW
    mov     [Fr_RaiseOverflow+ebp],eax
    mov     eax,Arg_PolyStack[ecx]
    mov     esp,SP_OFF[eax]
    push    PC_OFF[eax]                    ; Push the code address
    test    byte ptr [ecx+Arg_FullRestore],1   ; Should we restore or clear the regs?

    jnz     sw2polyfull
; We're returning from an RTS call.
    mov     eax,EAX_OFF[eax]                ; The return value
    cld                                     ; Clear this just in case
    ret                                     ; Jump to code address

sw2polyfull:
    frstor  FPREGS_OFF[eax]
    mov     ebx,EBX_OFF[eax]
    mov     ecx,ECX_OFF[eax]
    mov     edx,EDX_OFF[eax]
    mov     esi,ESI_OFF[eax]
    mov     edi,EDI_OFF[eax]
    cld                                     ; Clear this just in case
    mov     eax,EAX_OFF[eax]
    ret                                     ; Jump to code address

; Code to save the state and switch to C
; This saves the full register state.
SaveFullState:
    push    eax                             ; Save eax
    mov     eax,[Fr_Argument+ebp]
    mov     eax,[Arg_PolyStack+eax]
    mov     EBX_OFF[eax],ebx
    mov     ECX_OFF[eax],ecx
    mov     EDX_OFF[eax],edx
    mov     ESI_OFF[eax],esi
    mov     EDI_OFF[eax],edi
    FNSAVE  FPREGS_OFF[eax]                 ; Save FP state.  Also resets the state so...
    FLDCW   FPREGS_OFF[eax]                 ; ...load because we need the same rounding mode in the RTS
    pop     ebx                             ; Get old eax value
    mov     EAX_OFF[eax],ebx
    mov     SP_OFF[eax],esp                 ; Save ML stack pointer
    mov     eax,[Fr_Argument+ebp]
    mov     ebx,[Fr_LocalMpointer+ebp]      ; Copy back heap pointer
    mov     [Arg_LocalMpointer+eax],ebx
    mov     ebx,[Fr_HandlerRegister+ebp]    ; and handler pointer
    mov     [Arg_HandlerRegister+eax],ebx
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

; CallcodeTupled.  This is currently only used to "fold" RTS functions that are applied
; to constant arguments.  Oddly, compared with other RTS functions, it takes a single
; argument that is a pair containing the function closure and an argument vector.
PUBLIC  callcodeTupled
callcodeTupled:
    mov     edx,[eax]
    mov     esi,POLYWORDSIZE[eax]
    cmp     esi,1
    je      cct2
    mov     ecx,Max_Length
    and     ecx,(-POLYWORDSIZE)[esi]
    jz      cct2
    mov     eax,[esi]
    add     esi,POLYWORDSIZE
    sub     ecx,1
    jz      cct2
    mov     ebx,[esi]
    add     esi,POLYWORDSIZE
    sub     ecx,1
    jz      cct2
    pop    edi                                ; Get the return address
cct1:                                           ; Push the remaining args to the stack
    push   [esi]
    add     esi,POLYWORDSIZE
    LOOP    cct1
    push   edi                                ; Push the return address

cct2:                                           ; Finished - enter function
    mov     esi,1
    mov     ecx,1
    jmp     dword ptr [edx]

;
; A number of functions implemented in Assembly for efficiency reasons
;

PUBLIC  int_to_word
int_to_word:
 ; Extract the low order bits from a word.
    test    eax,TAG
    jz      get_first_long_word_a1
    ret                 ; Return the argument

 ; This is now used in conjunction with isShort in Word.fromInt.
PUBLIC  get_first_long_word_a
get_first_long_word_a:
get_first_long_word_a1:
    test    byte ptr [eax-1],16  ; 16 is the "negative" bit
    mov     eax,[eax]
    jz      gfw1
    neg    eax            ; We can ignore overflow
gfw1:
    lea     eax,1[eax*2]
    ret

PUBLIC  move_bytes
move_bytes:
 ; Move a segment of memory from one location to another.
 ; Must deal with the case of overlapping segments correctly.
 ; (source, sourc_offset, destination, dest_offset, length)

 ; Assume that the offsets and length are all short integers.
    mov     edi,12[esp]                ; Destination address
    mov     ecx,8[esp]                 ; Destination offset, untagged
    shr     ecx,TAGSHIFT
    add     edi,ecx
    mov     esi,eax
    shr     ebx,TAGSHIFT
    add     esi,ebx
    mov     ecx,4[esp]
    shr     ecx,TAGSHIFT
    cld                             ; Default to increment edi,esi
    cmp     esi,edi
 ; If dest > src then use decrementing moves else
 ; use incrementing moves.
    ja      mvb1
    std                             ; Decrement edi,esi
    lea     esi,(-1)[esi+ecx]
    lea     edi,(-1)[edi+ecx]
mvb1:
    rep movsb                       ; Copy the bytes
    mov     eax,1
    mov     ebx,eax
    mov     ecx,eax
    mov     edi,eax
    mov     esi,eax
 ; Visual Studio 5 C++ seems to assume that the direction flag
 ; is cleared.  I think that`s a bug but we have to go along with it.
    cld
    ret     12

PUBLIC  move_words
move_words:
 ; Move a segment of memory from one location to another.
 ; Must deal with the case of overlapping segments correctly.
 ; (source, source_offset, destination, dest_offset, length)
 ; Assume that the offsets and length are all short integers.
    mov     edi,12[esp]                ; Destination address
    mov     ecx,8[esp]                  ; Destination offset, untagged
    lea     edi,(-2)[edi+ecx*2]
    lea     esi,(-2)[eax+ebx*2]
    mov     ecx,4[esp]
    shr     ecx,TAGSHIFT
    cld                             ; Default to increment edi,esi
    cmp     esi,edi
 ; If dest > src then use decrementing moves else
 ; use incrementing moves.
    ja      mvw1
    std                             ; Decrement edi,esi
    lea     esi,(-POLYWORDSIZE)[esi+ecx*POLYWORDSIZE]
    lea     edi,(-POLYWORDSIZE)[edi+ecx*POLYWORDSIZE]
mvw1:
    rep movsd                       ; Copy the words
    mov     eax,1
    mov     ecx,eax
    mov     edi,eax
    mov     esi,eax
 ; Visual Studio 5 C++ seems to assume that the direction flag
 ; is cleared.  I think that`s a bug but we have to go along with it.
    cld
    ret     12
;

RetFalse:
    mov     eax,FALSE
    ret

RetTrue:
    mov     eax,TRUE
    ret

PUBLIC  not_bool
not_bool:
    xor     eax,(TRUE-TAG)
    ret

; or, and, xor shift etc. assume the values are tagged integers
PUBLIC  or_word
or_word:
    or      eax,ebx
    ret



PUBLIC  and_word
and_word:
    and     eax,ebx
    ret

PUBLIC  xor_word
xor_word:
    xor     eax,ebx
    or      eax,TAG
    ret

PUBLIC  shift_left_word
shift_left_word:
; Assume that both args are tagged integers
; Word.<<(a,b) is defined to return 0 if b > Word.wordSize

    cmp     ebx,TAGGED(31)
    jb      slw1
    mov     eax,1
    ret
slw1:
    mov     ecx,ebx
    shr     ecx,TAGSHIFT
    sub     eax,TAG
    shl     eax,cl
    or      eax,TAG
    mov     ecx,eax
    ret

PUBLIC  shift_right_word
shift_right_word:
; Word.>>(a,b) is defined to return 0 if b > Word.wordSize
    cmp     ebx,TAGGED(31)
    jb      srw1
    mov     eax,1
    ret
srw1:
    mov     ecx,ebx
    shr     ecx,TAGSHIFT
    shr     eax,cl
    or      eax,TAG
    mov     ecx,eax
    ret



PUBLIC  shift_right_arith_word
shift_right_arith_word:
 ; Word.~>>(a,b) is defined to return 0 or ~1 if b > Word.wordSize
 ; The easiest way to do that is to set the shift to 31.
    cmp     ebx,TAGGED(31)
    jb      sra1
    mov     ebx,TAGGED(31)
sra1:
    mov     ecx,ebx
    shr     ecx,TAGSHIFT
    sar     eax,cl
    or      eax,TAG
    mov     ecx,eax
    ret



; Clears the "mutable" bit on a segment
PUBLIC  locksega
locksega:
    and     byte ptr    [eax-1],(0ffh-B_mutable)
    mov     eax,TAGGED(0)
    ret



PUBLIC  get_length_a
get_length_a:
    mov     eax,(-POLYWORDSIZE)[eax]
    shl     eax,8
    shr     eax,(8-TAGSHIFT)
    or      eax,TAG
    ret




PUBLIC  is_shorta
is_shorta:
; Returns true if the argument is tagged
    and     eax,TAG
    jz      RetFalse
    jmp     RetTrue



PUBLIC  string_length
string_length:
    test    eax,TAG
    jnz     RetOne      ; character.
    mov     eax,[eax]
    lea     eax,1[eax*2]
    ret
RetOne:
    mov     eax,TAGGED(1)
    ret



 ; Store the length of a string in the first word.
PUBLIC  set_string_length_a
set_string_length_a:
    shr     ebx,TAGSHIFT
    mov     [eax],ebx
    mov     eax,UNIT            ; Return unit
    mov     ebx,eax             ; Clobber untagged value
    ret



; raisex is only used in ML as "raiseWithLocation" to bypass the normal ML "raise".
; It is also used as the return address if an RTS function wants to raise an
; exception.
PUBLIC  raisex
raisex:
    mov     ecx,Fr_HandlerRegister[ebp]
    jmp     dword ptr [ecx]

PUBLIC  load_byte
load_byte:
    mov     edi,ebx
    shr     edi,TAGSHIFT
    movzx   edi, byte ptr [eax][edi]
    lea     eax,1[edi*2]
    mov     edi,eax
    ret



PUBLIC  load_word
load_word:
    mov     eax,(-2)[eax+ebx*2]
    mov     ebx,eax
    ret



PUBLIC  assign_byte
assign_byte:
; We can assume that the data value will not overflow 30 bits (it is only 1 byte!)
    mov     ecx,4[esp]
    shr     ecx,TAGSHIFT
; We can assume that the index will not overflow 30 bits i.e. it is a tagged short
    shr     ebx,TAGSHIFT
    mov     [eax+ebx],cl
    mov     eax,1
    mov     ebx,eax
    mov     ecx,eax
    ret     4




PUBLIC  assign_word
assign_word:
    mov     ecx,4[esp]
    mov     (-2)[eax+ebx*2],ecx
    mov     eax,1
    ret     4



; Allocate a piece of memory that does not need to be initialised.
; We can't actually risk leaving word objects uninitialised so for the
; moment we always initialise.
PUBLIC  alloc_uninit
alloc_uninit:
    pop     ecx         ;# Get the return address
    push    ZERO        ;# Push the initial value - zero
    push    ecx        ;# Restore the return address
; Drop through into alloc_store



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
    test    eax,TAG
    jz      alloc_in_rts                ; Get the RTS to raise an exception
    mov     edi,eax
    shr     edi,TAGSHIFT                ; Remove tag
    cmp     edi,Max_Length              ; Length field must fit in 56 bits
    ja      alloc_in_rts                ; Get the RTS to raise an exception
    inc     edi                         ; Add 1 word
    shl     edi,2                       ; Get length in bytes
    mov     edx,Fr_LocalMpointer[ebp]
    sub     edx,edi                     ; Allocate the space
    mov     edi,eax                     ; Clobber bad value in edi
    cmp     edx,Fr_LocalMbottom[ebp]       ; Check for free space
    jb      alloc_in_rts
; Normally the above test is sufficient but if LocalMpointer is near the bottom of
; memory and the store requested is very large the value in edx can be negative
; which is greater, unsigned, than LocalMbottom.  We have to check it is less
; than, unsigned, the allocation pointer.
    cmp     edx,Fr_LocalMpointer[ebp]
    jnb     alloc_in_rts
    mov     Fr_LocalMpointer[ebp],edx     ; Put back in the heap ptr
    shr     eax,TAGSHIFT
    mov     (-4)[edx],eax               ; Put in length
    shr     ebx,TAGSHIFT                ; remove tag from flag
    or      ebx,B_mutable               ; set mutable bit
    mov     (-1)[edx],bl                ; and put it in.
; Initialise the store.
    mov     ecx,eax                     ; Get back the no. of words.
    mov     eax,4[esp]                  ; Get initial value.
    and     bl,B_typeBits              ; Mask off mutable, weak etc.
    cmp     bl,B_bytes
    jne     allst2

; If this is a byte seg
    shr     eax,TAGSHIFT                ; untag the initialiser
    shl     ecx,2                       ; Convert to bytes
    mov     edi,edx
    rep stosb
    jmp     allst3

 ; If this is a word segment
allst2:
    mov     edi,edx
    rep stosd

allst3:
    mov     eax,edx                 ; Result to eax

    mov     ecx,eax                 ; Clobber these
    mov     edx,eax
    mov     ebx,eax
    mov     edi,eax
    ret     4



; This is used if we have reached the store limit and need to garbage-collect.
alloc_in_rts:
    mov     edx,eax         ; Clobber these first
    mov     edi,eax
    CALL_IO    POLY_SYS_alloc_store

PUBLIC  touch_final
touch_final:
; This is really a pseudo-op
    mov     eax,UNIT
    ret



PUBLIC  add_long
add_long:
    mov     edi,eax
    and     edi,ebx
    and     edi,TAG
    jz      add_really_long
    lea     edi,(-TAG)[eax]
    add     edi,ebx
    jo      add_really_long
    mov     eax,edi
    ret
add_really_long:
    mov     edi,eax
    CALL_IO    POLY_SYS_aplus



PUBLIC  sub_long
sub_long:
    mov     edi,eax
    and     edi,ebx
    and     edi,TAG
    jz      sub_really_long
    mov     edi,eax
    sub     edi,ebx
    jo      sub_really_long
    lea     eax,TAG[edi]
    mov     edi,eax
    ret
sub_really_long:
    mov     edi,eax
    CALL_IO    POLY_SYS_aminus



PUBLIC  mult_long
mult_long:
    mov     edi,eax
    and     edi,ebx
    and     edi,TAG
    jz      mul_really_long
    mov     edi,ebx
    sar     edi,TAGSHIFT            ; Shift multiplicand
    mov     esi,eax
    sub     esi,TAG                 ; Just subtract off the tag off multiplier
    imul    esi,edi
    jo      mul_really_long
    add     esi,TAG
    mov     eax,esi
    mov     edi,eax
    ret
mul_really_long:
    mov     esi,eax                 ; Clobber this
    mov     edi,eax
    CALL_IO    POLY_SYS_amul



PUBLIC  div_long
div_long:
    mov     edi,eax
    and     edi,ebx
    and     edi,TAG                     ; %edi now contains $0 or $1 (both legal!)
    jz      div_really_long
    cmp     ebx,TAGGED(0)
    jz      div_really_long             ; We don't want a trap.
    ; The only case of overflow is dividing the smallest negative number by -1
    cmp     ebx,TAGGED((-1))
    jz      div_really_long
    sar     eax,TAGSHIFT
    mov     edi,ebx
    sar     edi,TAGSHIFT
    cdq
    idiv    edi
    lea     eax,1[eax*2]
    mov     edx,eax
    mov     edi,eax
    ret
div_really_long:
    mov     edi,eax
    CALL_IO    POLY_SYS_adiv



PUBLIC  rem_long
rem_long:
    mov     edi,eax
    and     edi,ebx
    and     edi,TAG                 ; %edi now contains $0 or $1 (both legal!
    jz      rem_really_long
    cmp     ebx,TAGGED(0)                   ; Check that it's non-zero
    jz      rem_really_long                 ; We don't want a trap.
    ; The only case of overflow is dividing the smallest negative number by -1
    cmp     ebx,TAGGED((-1))
    jz      rem_really_long
    sar     eax,TAGSHIFT
    mov     edi,ebx
    sar     edi,TAGSHIFT
    cdq
    idiv    edi
    lea     eax,1[edx*2]
    mov     edx,eax
    mov     edi,eax
    ret
rem_really_long:
    mov     edi,eax
    CALL_IO    POLY_SYS_amod



 ; Combined quotient and remainder.  We have to use the long form
 ; if the arguments are long or there's an overflow.  The first two
 ; arguments are the values to be divided.  The third argument is the
 ; address where the results should be placed. 
PUBLIC  quotrem_long
quotrem_long:
    mov     edi,eax
    and     edi,ebx
    and     edi,TAG
    jz      quotrem_really_long
    cmp     ebx,TAGGED(0)
    jz      quotrem_really_long
    ; The only case of overflow is dividing the smallest negative number by -1
    cmp     ebx,TAGGED((-1))
    jz      quotrem_really_long

    ; Get the address for the result.
    mov     ecx,4[esp]
    ; Do the division
    sar     eax,TAGSHIFT
    mov     edi,ebx
    sar     edi,TAGSHIFT
    cdq
    idiv    edi
    lea     eax,1[eax*2]
    lea     edx,1[edx*2]
    mov     edi,eax
    mov     [ecx],eax
    mov     POLYWORDSIZE[ecx],edx
    mov     eax,ecx
    ret     4

mem_for_remquot1:  ; Not enough store: clobber bad value in ecx.
    mov     ecx,1

quotrem_really_long:
    mov     edi,eax
    CALL_IO    POLY_SYS_quotrem



; TODO: Isn't this byte equality?
PUBLIC  equal_long
equal_long:
    cmp     ebx,eax
    je      RetTrue
    mov     ecx,eax
    or      eax,ebx
    and     eax,TAG
    jnz     RetFalse
    mov     eax,ecx
    CALL_IO    POLY_SYS_equala




PUBLIC  or_long
or_long:
    CALL_IO    POLY_SYS_ora



PUBLIC  xor_long
xor_long:
    CALL_IO    POLY_SYS_xora



PUBLIC  and_long
and_long:
   CALL_IO    POLY_SYS_anda



PUBLIC  neg_long
    neg_long:
    test    eax,TAG
    jz      neg_really_long
    mov     edi,(TAGGED(0)+TAG)
    sub     edi,eax
    jo      neg_really_long
    mov     eax,edi
    ret
neg_really_long:
    mov     edi,eax
    CALL_IO    POLY_SYS_aneg



PUBLIC  int_geq
int_geq:
    test    eax,TAG
    jz      igeq2
    test    ebx,TAG
    jz      igeq1
    cmp     eax,ebx
    jge     RetTrue
    jmp     RetFalse
igeq1:
 ; First arg is short, second isn't
    test    byte ptr [ebx-1],16  ; 16 is the "negative" bit
    jnz     RetTrue     ; Negative - always less
    jmp     RetFalse

igeq2:
; First arg is long
    test    ebx,TAG
    jz      igeq3
; First arg is long, second is short
    test    byte ptr [eax-1],16  ; 16 is the "negative" bit
    jz      RetTrue    ; Positive - always greater
    jmp     RetFalse

igeq3:
; Both long
   CALL_IO    POLY_SYS_int_geq




PUBLIC  int_leq
int_leq:
    test    eax,TAG
    jz      ileq2
    test    ebx,TAG
    jz      ileq1
    cmp     eax,ebx
    jle     RetTrue
    jmp     RetFalse
ileq1:
 ; First arg is short, second isn't
    test    byte ptr [ebx-1],16  ; 16 is the "negative" bit
    jz      RetTrue     ; Negative - always less
    jmp     RetFalse

ileq2:
 ; First arg is long
    test    ebx,TAG
    jz      ileq3
 ; First arg is long, second is short
    test    byte ptr [eax-1],16  ; 16 is the "negative" bit
    jnz     RetTrue    ; Positive - always greater
    jmp     RetFalse

ileq3:
    CALL_IO    POLY_SYS_int_leq




PUBLIC  int_gtr
int_gtr:
    test    eax,TAG
    jz      igtr2
    test    ebx,TAG
    jz      igtr1
    cmp     eax,ebx
    jg      RetTrue
    jmp     RetFalse
igtr1:
 ; First arg is short, second isn't
    test    byte ptr [ebx-1],16  ; 16 is the "negative" bit
    jnz     RetTrue     ; Negative - always less
    jmp     RetFalse

igtr2:
 ; First arg is long
    test    ebx,TAG
    jz      igtr3
 ; First arg is long, second is short
    test    byte ptr [eax-1],16  ; 16 is the "negative" bit
    jz      RetTrue    ; Positive - always greater
    jmp     RetFalse

igtr3:
    CALL_IO    POLY_SYS_int_gtr




PUBLIC  int_lss
int_lss:
    test    eax,TAG
    jz      ilss2
    test    ebx,TAG
    jz      ilss1
    cmp     eax,ebx
    jl      RetTrue
    jmp     RetFalse
ilss1:
 ; First arg is short, second isn't
    test    byte ptr [ebx-1],16  ; 16 is the "negative" bit
    jz      RetTrue     ; Negative - always less
    jmp     RetFalse

ilss2:
 ; First arg is long
    test    ebx,TAG
    jz      ilss3
 ; First arg is long, second is short
    test    byte ptr [eax-1],16  ; 16 is the "negative" bit
    jnz     RetTrue    ; Positive - always greater
    jmp     RetFalse

ilss3:
    CALL_IO    POLY_SYS_int_lss



PUBLIC  offset_address
offset_address:
 ; This is needed in the code generator, but is a very risky thing to do.
    shr     ebx,TAGSHIFT
    add     eax,ebx
    mov     ebx,eax
    ret



; General test routine.  Returns with the condition codes set
; appropriately.

teststr:
    test    eax,TAG                 ; Is arg1 short
    jz      tststr1
    test    ebx,TAG                 ; Yes: is arg2 also short?
    jz      tststr0a
    ; Both are short - just compare the characters
    cmp     eax,ebx
    ret

tststr0a:
    mov     edi,1                   ; Is arg2 the null string ?
    cmp     edi,[ebx]
    jg      tststr4                 ; Return with "gtr" set if it is
    shr     eax,TAGSHIFT
    cmp     al,POLYWORDSIZE[ebx]
    jne     tststr4                 ; If they're not equal that's the result
    cmp     eax,256                 ; But if they're equal set "less" because A is less than B
    jmp     tststr4

tststr1: ; arg2 is not short.  Is arg1 ?
    test    ebx,TAG
    jz      tststr2
    mov     edi,[eax]           ; Is arg1 the null string
    cmp     edi,1
    jl      tststr4             ; Return with "less" set if it is
    shr     ebx,TAGSHIFT
    mov     cl,POLYWORDSIZE[eax]
    cmp     cl,bl
    jne     tststr4             ; If they're not equal that's the result
    cmp     edi,0               ; But if they're equal set "greater" because A is greater than B
    jmp     tststr4

tststr2:
    mov     edi,[eax]
    mov     ecx,[ebx]
    cmp     edi,ecx
    jge     tststr3
    mov     ecx,edi
tststr3:
    lea     esi,POLYWORDSIZE[eax]       ; Load ptrs for cmpsb
    lea     edi,POLYWORDSIZE[ebx]
    cld                 ; Make sure we increment
    cmp     eax,eax
    repe cmpsb          ; Compare while equal and ecx > 0
    jnz     tststr4
; Strings are equal as far as the shorter of the two.  Have to compare
; the lengths.
    mov     edi,[eax]
    cmp     edi,[ebx]
tststr4:
    mov     eax,1                   ; Clobber these
    mov     ebx,eax
    mov     ecx,eax
    mov     esi,eax
    mov     edi,eax
    ret

 ; These functions compare strings for lexical ordering.  This version, at
 ; any rate, assumes that they are UNSIGNED bytes.

PUBLIC  str_compare
str_compare:
    call    teststr
    ja      RetTrue         ; Return TAGGED(1) if it's greater
    je      RetFalse        ; Return TAGGED(0) if it's equal
    mov     eax,MINUS1
    ret




PUBLIC  teststrgeq
teststrgeq:
    call    teststr
    jnb     RetTrue
    jmp     RetFalse



PUBLIC  teststrleq
teststrleq:
    call    teststr
    jna     RetTrue
    jmp     RetFalse



PUBLIC  teststrlss
teststrlss:
    call    teststr
    jb      RetTrue
    jmp     RetFalse



PUBLIC  teststrgtr
teststrgtr:
    call    teststr
    ja      RetTrue
    jmp     RetFalse




PUBLIC  bytevec_eq
bytevec_eq:
 ; Compare arrays of bytes.  The arguments are the same as move_bytes.
 ; (source, sourc_offset, destination, dest_offset, length)

 ; Assume that the offsets and length are all short integers.
    mov     edi,12[esp]
    mov     ecx,8[esp]
    shr     ecx,TAGSHIFT
    add     edi,ecx
    mov     esi,eax
    shr     ebx,TAGSHIFT
    add     esi,ebx
    mov     ecx,4[esp]
    shr     ecx,TAGSHIFT
    cld                     ; Make sure we increment
    cmp     eax,eax
    repe    cmpsb
    mov     esi,eax             ; Make these valid
    mov     ecx,eax
    mov     edi,eax
    jz      bvTrue
    mov     eax,FALSE
    jmp     bvRet
bvTrue:
    mov     eax,TRUE
bvRet:
    ret     12




PUBLIC  is_big_endian
is_big_endian:
    jmp     RetFalse    ; I386/486 is little-endian



PUBLIC  bytes_per_word
bytes_per_word:
    mov     eax,TAGGED(POLYWORDSIZE)
    ret



 ; Word functions.  These are all unsigned and do not raise Overflow
 
PUBLIC  mul_word
mul_word:
    shr     ebx,TAGSHIFT
    sub     eax,TAG
    mul     ebx                ; unsigned multiplication
    add     eax,TAG
    mov     edx,eax
    mov     ebx,eax
    ret



PUBLIC  plus_word
plus_word:
    lea     eax,(-TAG)[eax+ebx]
    ret



PUBLIC  minus_word
minus_word:
    sub     eax,ebx
    add     eax,TAG
    ret



PUBLIC  div_word
div_word:
    shr     ebx,TAGSHIFT
    shr     eax,TAGSHIFT
    mov     edx,0
    div     ebx
    lea     eax,1[eax*2]
    mov     edx,eax
    mov     ebx,eax
    ret



PUBLIC  mod_word
mod_word:
    shr     ebx,TAGSHIFT
    shr     eax,TAGSHIFT
    mov     edx,0
    div     ebx
    lea     eax,1[edx*2]
    mov     edx,eax
    mov     ebx,eax
    ret



PUBLIC  word_eq
word_eq:
    cmp     eax,ebx
    jz      RetTrue         ; True if they are equal.
    jmp     RetFalse



PUBLIC  word_geq
word_geq:
    cmp     eax,ebx
    jnb     RetTrue
    jmp     RetFalse



PUBLIC  word_leq
word_leq:
    cmp     eax,ebx
    jna     RetTrue
    jmp     RetFalse



PUBLIC  word_gtr
word_gtr:
    cmp     eax,ebx
    ja      RetTrue
    jmp     RetFalse



PUBLIC  word_lss
word_lss:
    cmp     eax,ebx
    jb      RetTrue
    jmp     RetFalse



PUBLIC  fixed_geq
fixed_geq:
    cmp     eax,ebx
    jge     RetTrue
    jmp     RetFalse



PUBLIC  fixed_leq
fixed_leq:
    cmp     eax,ebx
    jle     RetTrue
    jmp     RetFalse



PUBLIC  fixed_gtr
fixed_gtr:
    cmp     eax,ebx
    jg      RetTrue
    jmp     RetFalse



PUBLIC  fixed_lss
fixed_lss:
    cmp     eax,ebx
    jl      RetTrue
    jmp     RetFalse



PUBLIC  fixed_add
fixed_add:
    lea     eax,(-TAG)[eax]
    add     eax,ebx
    jo      raiseOverflowEx
	ret



PUBLIC  fixed_sub
fixed_sub:
    sub     eax,ebx
    jo      raiseOverflowEx
    add     eax,TAG
	ret



PUBLIC  fixed_mul
fixed_mul:
    sar     ebx,TAGSHIFT
    sub     eax,TAG
    imul    eax,ebx
    jo      raiseOverflowEx
    add     eax,TAG
    mov     edx,eax
    mov     ebx,eax
    ret



PUBLIC  fixed_quot
fixed_quot:
; Checking for overflow and zero is done in ML
    sar     ebx,TAGSHIFT
    sar     eax,TAGSHIFT
    cdq
    idiv    ebx
    lea     eax,1[eax*2]
    mov     edx,eax
    mov     ebx,eax
    ret



PUBLIC  fixed_rem
fixed_rem:
    sar     ebx,TAGSHIFT
    sar     eax,TAGSHIFT
    cdq
    idiv    ebx
    lea     eax,1[edx*2]
    mov     edx,eax
    mov     ebx,eax
    ret



; TODO: This needs to be fixed.
PUBLIC  fixed_div
fixed_div:
    ; Checking for overflow and zero is done in ML
    sar     ebx,TAGSHIFT
    sar     eax,TAGSHIFT
    cdq
    idiv    ebx
    cmp     edx,0
    jz		fixed_div1		; If the remainder if non-zero ...
    xor     ebx,edx
    jns		fixed_div1
    sub     eax,1
fixed_div1:
    lea     eax,1[eax*2]
    mov     edx,eax
    mov     ebx,eax
    ret



PUBLIC  fixed_mod
fixed_mod:
    sar     ebx,TAGSHIFT
    sar     eax,TAGSHIFT
    cdq
    idiv     ebx
; Result is in edx.  We have to change the result so that it has the sign as the divisor.
    cmp     edx,0
    jz      fixed_mod1	; Result is zero - no change
    xor     ebx,edx
    jns		fixed_mod1	; Skip if they had the same signs
    xor     ebx,edx
    add     edx,ebx
fixed_mod1:
    lea     eax,1[edx*2]
    mov     edx,eax
    mov     ebx,eax
    ret



raiseOverflowEx:
; Build the exception packet.
; Allocate four word item.
; Set first word to TAGGED(5)
; Set second word to the string "Overflow" - That's going to need allocation.
; Set third and fourth words to TAGGED(0)
; Raise the exception.
    jmp     dword ptr [Fr_RaiseOverflow+ebp]

; Atomically increment the value at the address of the arg and return the
; updated value.  Since the xadd instruction returns the original value
; we have to increment it.
PUBLIC  atomic_increment
atomic_increment:
atomic_incr:                    ; Internal name in case "atomic_increment" is munged.
    mov     ebx,2
    lock xadd [eax],ebx
    add     ebx,2
    mov     eax,ebx
    ret



; Atomically decrement the value at the address of the arg and return the
; updated value.  Since the xadd instruction returns the original value
; we have to decrement it.
PUBLIC  atomic_decrement
atomic_decrement:
atomic_decr:
    mov     ebx,-2
    lock xadd [eax],ebx
    mov     eax,ebx
    sub     eax,2
    ret



; Reset a mutex to (tagged) one.  Because the increment and decrements
; are atomic this doesn't have to do anything special.
PUBLIC  atomic_reset
atomic_reset:
    mov     dword ptr [eax],3 
    mov     eax,1
    ret



; Return the thread id object for the current thread
PUBLIC  thread_self
thread_self:
    mov     eax,[Fr_Argument+ebp]
    mov     eax,[Arg_ThreadId+eax]
    ret





; Memory for LargeWord.word values.  This is the same as mem_for_real on
; 64-bits but only a single word on 32-bits.
; ********************************
; Some of this code is temporary.  The final version should compute the result and
; simply jump here to box it.  That requires the heap-overflow code to save the
; registers across the trap but not to examine them for pointers.  Temporarily we
; don't do that but instead clear all the registers across a trap.
mem_for_largeword:
    mov     ecx,Fr_LocalMpointer[ebp]
    sub     ecx,8                       ; Length word (4 bytes) + 4 bytes
    cmp     ecx,Fr_LocalMbottom[ebp]
    jnb     mem_for_largeword1
; ********************************
; Temporarily: push these registers to the stack
; and pop them afterwards.  This isn't the final version of this code
; but is useful as a test.
	push	eax
	push    ebx
    call    X86AsmCallExtraRETURN_HEAP_OVERFLOW
	pop     ebx
	pop     eax
mem_for_largeword1:
    mov     Fr_LocalMpointer[ebp],ecx             ; Updated allocation pointer
    mov     dword ptr (-4)[ecx],01000001h      ; Length word:
    ret

mem_for_real:
; Allocate memory for the result.
    mov     ecx,Fr_LocalMpointer[ebp]
    sub     ecx,12                       ;# Length word (4 bytes) + 8 bytes
    cmp     ecx,Fr_LocalMbottom[ebp]
    jnb     mem_for_real1
; ********************************
; Temporarily: push these registers to the stack
; and pop them afterwards.  This isn't the final version of this code
; but is useful as a test.
    push    eax
    push    ebx
    call    X86AsmCallExtraRETURN_HEAP_OVERFLOW
    pop     ebx
    pop     eax
mem_for_real1:
    mov     Fr_LocalMpointer[ebp],ecx
    mov     dword ptr (-4)[ecx],01000002h       ; Length word:
    ret


PUBLIC  real_add
real_add:
    call    mem_for_real
; Do the operation and put the result in the allocated
; space.
    fld     qword ptr [eax]
    fadd    qword ptr [ebx]
    fstp    qword ptr [ecx]
    mov     eax,ecx
    ret

; The mask includes FP7 rather than FP0 because this pushes a value which
; overwrites the bottom of the stack.



PUBLIC  real_sub
real_sub:
   call    mem_for_real
; Do the operation and put the result in the allocated
; space.
    fld     qword ptr [eax]
    fsub    qword ptr [ebx]
    fstp    qword ptr [ecx]
    mov     eax,ecx
    ret




PUBLIC  real_mul
real_mul:
    call    mem_for_real
; Do the operation and put the result in the allocated
; space.
    fld     qword ptr [eax]
    fmul    qword ptr [ebx]
    fstp    qword ptr [ecx]
    mov     eax,ecx
    ret




PUBLIC  real_div
real_div:
        call    mem_for_real
; Do the operation and put the result in the allocated
; space.
    fld     qword ptr [eax]
    fdiv    qword ptr [ebx]
    fstp    qword ptr [ecx]
    mov     eax,ecx
    ret




; For all values except NaN it's possible to do this by a test such as
; "if x < 0.0 then ~ x else x" but the test always fails for NaNs

PUBLIC  real_abs
real_abs:
    mov     ebx,eax             ; Put a valid value in ebx
    call    mem_for_real
; Do the operation and put the result in the allocated
; space.
; N.B. Real.~ X is not the same as 0.0 - X.  Real.~ 0.0 is ~0.0;
    fld     qword ptr [eax]
    fabs
    fstp    qword ptr [ecx]
    mov     eax,ecx
    ret




PUBLIC  real_neg
real_neg:
    mov     ebx,eax
    call    mem_for_real
; Do the operation and put the result in the allocated
; space.
; N.B. Real.~ X is not the same as 0.0 - X.  Real.~ 0.0 is ~0.0;
    fld     qword ptr [eax]
    fchs
    fstp    qword ptr [ecx]
    mov     eax,ecx
    ret



PUBLIC  real_eq
real_eq:
    fld     qword ptr [eax]
    fcomp   qword ptr [ebx]
    fnstsw  ax
; Not all 64-bit processors support SAHF.
; The result is true if the zero flag is set and parity flag clear.  
    and     eax,17408           ; 0x4400
    cmp     eax,16384           ; 0x4000
    je      RetTrue
    jmp     RetFalse




PUBLIC  real_neq
real_neq:
    fld     qword ptr [eax]
    fcomp   qword ptr [ebx]
    fnstsw  ax
    and     eax,17408
    cmp     eax,16384
    jne     RetTrue
    jmp     RetFalse



PUBLIC  real_lss
real_lss:
; Compare ebx > eax
    fld     qword ptr [ebx]
    fcomp   qword ptr [eax]
    fnstsw  ax

; True if the carry flag (C0), zero flag (C3) and parity (C2) are all clear
    and     eax,17664

    je      RetTrue
    jmp     RetFalse




PUBLIC  real_gtr
real_gtr:
    fld     qword ptr [eax]
    fcomp   qword ptr [ebx]
    fnstsw  ax

; True if the carry flag (C0), zero flag (C3) and parity (C2) are all clear
    and     eax,17664
    je      RetTrue
    jmp     RetFalse




PUBLIC  real_leq
real_leq:
; Compare ebx > eax
    fld     qword ptr [ebx]
    fcomp   qword ptr [eax]
    fnstsw  ax
; True if the carry flag (C0) and parity (C2) are both clear
    and     eax,1280

    je      RetTrue
    jmp     RetFalse




PUBLIC  real_geq
real_geq:
    fld     qword ptr [eax]
    fcomp   qword ptr [ebx]
    fnstsw  ax
; True if the carry flag (C0) and parity (C2) are both clear
    and     eax,1280

    je      RetTrue
    jmp     RetFalse



PUBLIC  real_from_int
real_from_int:
    test    eax,TAG
    jnz     fixed_to_real

    CALL_IO    POLY_SYS_int_to_real



PUBLIC  fixed_to_real
fixed_to_real:
    mov     ebx,eax                 ; Put a valid value in ebx
    call    mem_for_real
    sar     eax,TAGSHIFT
    push    eax                     ; Push it to the stack.  We can only load it from memory
    fild    dword ptr [esp]
    pop     eax
    fstp    qword ptr [ecx]
    mov     eax,ecx
    ret



PUBLIC  set_exception_trace
set_exception_trace:
; The argument is the closure to call.  The return address is already on the stack.
; We need to push the addresses of some code.  To make it position-independent we use
; CALLs in a non-standard way.
    mov     edx,eax
    push    Fr_HandlerRegister[ebp]		; Previous handler
    call    setexct1                    ; Jump to setexct1 pushing the next addr to the stack
    ; This is the code that is called if we get an exception.
    ; The exception packet is the first argument.
    CALL_IO POLY_SYS_give_ex_trace_fn

setexct1:
    mov     Fr_HandlerRegister[ebp],esp
    call    setexct2                    ; Jump to setexct2 pushing the next addr to the stack
    ; This is the code that is called if we return without raising an exception
    add     esp,POLYWORDSIZE
    pop     Fr_HandlerRegister[ebp]
    RET
setexct2:
    mov     eax,UNIT
    jmp     dword ptr [edx]         ; Jump to the function

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

; LargeWord.word operations.  These are 32 or 64-bit values in a single-word byte
; memory cell.
PUBLIC  eq_longword
eq_longword:
    mov     eax,[eax]
    cmp     eax,[ebx]
    jz      RetTrue         ; True if they are equal.
    jmp     RetFalse



PUBLIC  geq_longword
geq_longword:
    mov     eax,[eax]
    cmp     eax,[ebx]
    jnb     RetTrue
    jmp     RetFalse



PUBLIC  leq_longword
leq_longword:
    mov     eax,[eax]
    cmp     eax,[ebx]
    jna     RetTrue
    jmp     RetFalse



PUBLIC  gt_longword
gt_longword:
    mov     eax,[eax]
    cmp     eax,[ebx]
    ja      RetTrue
    jmp     RetFalse



PUBLIC  lt_longword
lt_longword:
    mov     eax,[eax]
    cmp     eax,[ebx]
    jb      RetTrue
    jmp     RetFalse



PUBLIC  longword_to_tagged
longword_to_tagged:
; Load the value and tag it, discarding the top bit
    mov     eax,[eax]
    lea     eax,1[eax*2]
    ret



PUBLIC  signed_to_longword
signed_to_longword:
; Shift the value to remove the tag and store it.
    mov     ebx,eax                     ; mem_for_largeword may push rebx
    call    mem_for_largeword
    sar     eax,TAGSHIFT                ; Arithmetic shift, preserve sign
    mov     [ecx],eax
    mov     eax,ecx
    ret



PUBLIC  unsigned_to_longword
unsigned_to_longword:
; Shift the value to remove the tag and store it.
    mov     ebx,eax                     ; mem_for_largeword may push rebx
    call    mem_for_largeword
    shr     eax,TAGSHIFT                ; Logical shift, zero top bit
    mov     [ecx],eax
    mov     eax,ecx
    ret



PUBLIC  plus_longword
plus_longword:
    call    mem_for_largeword
    mov     eax,[eax]
    add     eax,[ebx]
    mov     [ecx],eax
    mov     eax,ecx
    ret



PUBLIC  minus_longword
minus_longword:
    call    mem_for_largeword
    mov     eax,[eax]
    sub     eax,[ebx]
    mov     [ecx],eax
    mov     eax,ecx
    ret



PUBLIC  mul_longword
mul_longword:
    call    mem_for_largeword
    mov     eax,[eax]
    mul     dword ptr [ebx]
    mov     [ecx],eax
    mov     eax,ecx
    mov     edx,eax             ; clobber this which has the high-end result
    ret



PUBLIC  div_longword
div_longword:
; Check for division by zero is done in ML
    call    mem_for_largeword
    mov     eax,[eax]
    mov     edx,0
    div     dword ptr [ebx]
    mov     [ecx],eax           ; Store the quotient
    mov     eax,ecx
    mov     edx,eax             ; clobber this which has the remainder
    ret



PUBLIC  mod_longword
mod_longword:
; Check for division by zero is done in ML
    call    mem_for_largeword
    mov     eax,[eax]
    mov     edx,0
    div     dword ptr [ebx]
    mov     [ecx],edx           ; Store the remainder
    mov     eax,ecx
    mov     edx,eax             ; clobber this which has the remainder
    ret



PUBLIC  andb_longword
andb_longword:
    call    mem_for_largeword
    mov     eax,[eax]
    and     eax,[ebx]
    mov     [ecx],eax
    mov     eax,ecx
    ret



PUBLIC  orb_longword
orb_longword:
    call    mem_for_largeword
    mov     eax,[eax]
    or      eax,[ebx]
    mov     [ecx],eax
    mov     eax,ecx
    ret



PUBLIC  xorb_longword
xorb_longword:
    call    mem_for_largeword
    mov     eax,[eax]
    xor     eax,[ebx]
    mov     [ecx],eax
    mov     eax,ecx
    ret



PUBLIC  shift_left_longword
shift_left_longword:
    call    mem_for_largeword
    mov     edx,ecx         ; We need ecx for the shift
; The shift value is always a Word.word value i.e. tagged
; LargeWord.<<(a,b) is defined to return 0 if b > LargeWord.wordSize
    cmp     ebx,TAGGED(32)
    jb      sllw1
    mov     eax,0
    jmp     sllw2
sllw1:
    mov     ecx,ebx
    shr     ecx,TAGSHIFT
    mov     eax,[eax]
    shl     eax,cl
sllw2:
    mov     [edx],eax
    mov     eax,edx
    mov     ecx,eax             ; Clobber ecx
    ret



PUBLIC  shift_right_longword
shift_right_longword:
    call    mem_for_largeword
    mov     edx,ecx             ; We need ecx for the shift
; The shift value is always a Word.word value i.e. tagged
; LargeWord.>>(a,b) is defined to return 0 if b > LargeWord.wordSize
    cmp     ebx,TAGGED(32)
    jb      srlw1
    mov     eax,0
    jmp     srlw2
srlw1:
    mov     ecx,ebx
    shr     ecx,TAGSHIFT            ; remove tag
    mov     eax,[eax]
    shr     eax,cl
srlw2:
    mov     [edx],eax
    mov     eax,edx
    mov     ecx,eax                 ; Clobber ecx
    ret



PUBLIC  shift_right_arith_longword
shift_right_arith_longword:
    call    mem_for_largeword
    mov     edx,ecx
; The shift value is always a Word.word value i.e. tagged
; LargeWord.~>>(a,b) is defined to return 0 or ~1 if b > LargeWord.wordSize
    cmp     ebx,TAGGED(32)
    jb      sralw1
; Setting the shift to 31/63 propagates the sign bit
    mov     ebx,TAGGED(31)
sralw1:
    mov     ecx,ebx
    shr     ecx,TAGSHIFT
    mov     eax,[eax]
    sar     eax,cl
    mov     [edx],eax
    mov     eax,edx
    mov     ecx,eax
    ret



; C-memory operations.
PUBLIC  cmem_load_asm_8
cmem_load_asm_8:
    mov     eax,[eax]
    sar     ebx,TAGSHIFT
    add     eax,ebx
    mov     ebx,4[esp]
    sar     ebx,TAGSHIFT
    movzx   eax, byte ptr [eax][ebx]
    lea     eax,1[eax*2]
    mov     ebx,eax
    ret     4



PUBLIC  cmem_load_asm_16
cmem_load_asm_16:
    mov     eax,[eax]
    sar     ebx,TAGSHIFT
    add     eax,ebx
    mov     ebx,4[esp]
    ; The index is tagged but since we want to multiply by two we don't need anything here.
    movzx   eax, word ptr [eax-1][ebx]
    lea     eax,1[eax*2]
    ret     4



PUBLIC  cmem_load_asm_32
cmem_load_asm_32:
;# 32-bit mode - the result is boxed
    call    mem_for_largeword
    mov     eax,[eax]               ; The address is boxed.
    sar     ebx,TAGSHIFT            ; The offset is a signed tagged value
    add     eax,ebx                 ; Add it in
    mov     ebx,4[esp]              ; Get the index.
    mov     eax,[eax+ebx*2-1]
    mov     [ecx],eax               ; Save in the new memory
    mov     eax,ecx                 ; Copy the result address
    ret     4



PUBLIC  cmem_load_asm_float
cmem_load_asm_float:
    call    mem_for_real
    mov     eax,[eax]
    sar     ebx,TAGSHIFT
    add     eax,ebx
    mov     ebx,4[esp]
    fld     dword ptr [eax-2][ebx*2]
    fstp    qword ptr [ecx]
    mov     eax,ecx
    ret     4



PUBLIC  cmem_load_asm_double
cmem_load_asm_double:
    call    mem_for_real
    mov     eax,[eax]
    sar     ebx,TAGSHIFT
    add     eax,ebx
    mov     ebx,4[esp]
    fld     qword ptr [eax-4][ebx*4]
    fstp    qword ptr [ecx]
    mov     eax,ecx
    ret     4


   
PUBLIC  cmem_store_asm_8
cmem_store_asm_8:
    mov     eax,[eax]
    sar     ebx,TAGSHIFT
    add     eax,ebx
    mov     ebx,8[esp]
    mov     ecx,4[esp]
    sar     ebx,TAGSHIFT
    sar     ecx,TAGSHIFT
    mov     [eax+ebx],cl
    mov     eax,UNIT
    mov     ebx,eax             ; Clobber bad value in ebx
    mov     ecx,eax             ; and ecx
    ret     8



PUBLIC  cmem_store_asm_16
cmem_store_asm_16:
    mov     eax,[eax]
    sar     ebx,TAGSHIFT
    add     eax,ebx
    mov     ebx,8[esp]
    mov     ecx,4[esp]
    sar     ecx,TAGSHIFT
    mov     word ptr [eax-1][ebx],cx
    mov     eax,UNIT
    mov     ecx,eax
    ret     8



PUBLIC  cmem_store_asm_32
cmem_store_asm_32:
    mov     eax,[eax]
    sar     ebx,TAGSHIFT
    add     eax,ebx
    mov     ebx,8[esp]
    mov     ecx,4[esp]
    mov     ecx,[ecx]
    sar     ecx,TAGSHIFT
    mov     dword ptr [eax-2][ebx*2],ecx
    mov     eax,UNIT
    mov     ecx,eax
    ret     8



PUBLIC  cmem_store_asm_float
cmem_store_asm_float:
    mov     eax,[eax]                   ; The address is boxed.
    sar     ebx,TAGSHIFT                ; The offset is a signed tagged value
    add     eax,ebx                     ; Add it in
    mov     ebx,8[esp]                  ; Get the index.
    mov     ecx,4[esp]                  ; Get the address of the real
    fld     qword ptr [ecx] 
    fstp    dword ptr [eax-2][ebx*2]
    mov     eax,UNIT
    mov     ebx,eax
    ret     8

PUBLIC  cmem_store_asm_double
cmem_store_asm_double:
    mov     eax,[eax]                   ; The address is boxed.
    sar     ebx,TAGSHIFT                ; The offset is a signed tagged valu
    add     eax,ebx                     ; Add it in
    mov     ebx,8[esp]                  ; Get the index.
    mov     ecx,4[esp]                  ; Get the address of the real
    fld     qword ptr [ecx] 
    fstp    qword ptr [eax-4][ebx*4]
    mov     eax,UNIT                    ; The function returns unit
    mov     ebx,eax                     ; Bad value in ebx
    ret     8

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
CREATE_IO_CALL CallPOLY_SYS_chdir,POLY_SYS_chdir
CREATE_IO_CALL CallPOLY_SYS_get_entry_point,POLY_SYS_get_entry_point
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

#if (0)
; chdir.  May raise an exception which means it may allocate space for the exception packet.
CallPOLY_SYS_chdir PROC
; N.B.  We don't actually need to remove any arguments from the C stack because
; we always set the C stack pointer to the value when we were first called.
    mov     ecx,[Fr_Argument+ebp]
    mov     dword ptr [Arg_ExceptionPacket+ecx],1       ; Clear exception
    mov     edx,[Fr_LocalMpointer+ebp]                  ; Copy heap pointer
    mov     [Arg_LocalMpointer+ecx],edx
    mov     [Fr_SavedMLSP+ebp],esp                      ; ML stack ptr
    mov     esp,ebp                                     ; Switch to C stack
    sub     esp,Fr_size
    push    eax                                         ; Push argument
    push    dword ptr [Arg_ThreadId+ecx]                ; Push thread id
    EXTRN   X86ChDir:PROC
    call    X86ChDir
    mov     esp,[Fr_SavedMLSP+ebp]                      ; Restore the ML stack ptr
    mov     ecx,[Fr_Argument+ebp]
    mov     edx,[Arg_LocalMpointer+ecx]                 ; Update heap pointer
    mov     [Fr_LocalMpointer+ebp],edx
    mov     edx,[Arg_LocalMbottom+ecx]
    mov     [Fr_LocalMbottom+ebp],edx                   ; and heap base because we may have GCd
    cmp     dword ptr [Arg_ExceptionPacket+ecx],1       ; Do we want to raise an exception?
    jne     cpsdraisex
    ret

cpsdraisex:
    mov     eax,[Arg_ExceptionPacket+ecx]
    jmp     raisex
CallPOLY_SYS_chdir ENDP

#endif

END
