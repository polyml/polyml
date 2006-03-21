;#
;#  Title: 	Assembly code routines for the poly system.
;#  Author:    David Matthews
;#  Copyright (c) Cambridge University Technical Services Limited 2000
;#	This library is free software; you can redistribute it and/or
;#	modify it under the terms of the GNU Lesser General Public
;#	License as published by the Free Software Foundation; either
;#	version 2.1 of the License, or (at your option) any later version.
;#	
;#	This library is distributed in the hope that it will be useful,
;#	but WITHOUT ANY WARRANTY; without even the implied warranty of
;#	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;#	Lesser General Public License for more details.
;#	
;#	You should have received a copy of the GNU Lesser General Public
;#	License along with this library; if not, write to the Free Software
;#	Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;#
;#
;#
;#
;# *********************************************************************
;# * IMPORTANT                                                         *
;# * This file is used directly by MASM but is also converted          *
;# * for use by gas on Unix.  For reasons best known to the respective *
;# * developers the assembly language accepted by gas is different     *
;# * from that used by MASM.  This file uses a sort of half-way house  *
;# * between the two versions in that the gas instruction ordering is  *
;# * used i.e. the first argument is the source and the second the     *
;# * destination, but the MASM format of addresses is used.            * 
;# * After making any changes to this file ensure that it can be       *
;# * successfully converted and compiled under both MASM and gas.      *
;# * DCJM January 2000                                                 *
;# *********************************************************************

;#
;# Registers used :-
;#
;#  %Reax: First argument to function.  Result of function call.
;#  %Rebx: Second argument to function.
;#  %Recx: Code pointer in call.
;#  %Redx: Closure pointer in call.
;#  %Rebp: Points to memory used for extra registers
;#  %Resi: General register.
;#  %Redi: General register.
;#  %Resp: Stack pointer.
;#

;#
;#  Mapping between names in different versions:
;#
;#  Usage               Dave`s i386     Dave`s SPARC    AHL SPARC               AHL i386
;#  -----               -----------     ------------    ---------               --------
;#  stack pointer       %Resp            %g4             %g4                     %Resp
;#  stack limit         mem_sl          %g5             %g5                     mem_sl
;#  heap pointer        mem_hp          %g6             %g6                     mem_hp
;#  heap limit          mem_hl          %g7             %g7                     mem_hl
;#  handler ptr         mem_hr          %g3             %g3                     mem_hr
;#  C heap pointer      newptr          newptr          localMpointer           localMpointer
;#  C heap bottom       locallimit      locallimit      localMbottom            localMbottom
;#  C heap top          ?               ?               localMtop               localMtop
;#
;# Note also that in the SPARC version the heap limit register (%g7) contains
;# an integer (the number of unallocated bytes in the heap) whereas in the
;# i386 version, the heap limit (mem_hl) contains a pointer (to the bottom
;# byte of the heap). SPF 26/10/94
;#
;# Microsoft i386 compiler expects all arguments to be on the stack, with
;# the first argument the most recently pushed. When the function returns,
;# it is the caller`s responsibility to remove the arguments. For a non-void
;# function, the return value will be in %Reax.
;#
;# Poly/ML compiler expects the first two arguments to be in %Reax and %Rebx,
;# with any remaining arguments on the stack, with the last argument the
;# most recently pushed. When the function returns, it will have removed any
;# stack arguments from the stack and the return value will be in %Reax.
;#
;# Just to make life more confusing, the run-time system expects the parameters
;# in the reverse order from Poly/ML (this would work well on a system that
;# didn`t cache the first few parameters in registers, but is very confusing
;# here).
;#
;#   I`ve just found a very subtle bug in this RTS code. If a routine such
;#   as assign_word is called via the general function call interface
;#   (rather than as a known RTS function), it will be entered with
;#   regCode (%o4 on the SPARC) pointing at the RTS machine code in
;#   this file. If we don`t explicitly zap regCode, then it will still
;#   point at the RTS code when we exit the routine. If we then do
;#   an ML process switch and the new process attempts to commit to
;#   the database, the copygc code will detect the illegal (because unportable
;#   between RTS versions) pointer and produces a core dump. I *think*
;#   this is the cause of the bug James found. 
;#   
;#   This problem doesn`t occur with Dave`s version of the compiler because
;#   it always explictly zaps regCode at the start of each code segment,
;#   before a process switch could possibly occur. My version of the
;#   compiler tries to optimise away this overhead, which only works if we
;#   change the RTS to ensure that regCode always contains a legal value.
;#  
;#   I`m about to do this! We may or may not need changes to the i386
;#   and PowerPC versions too.
;#   
;#   SPF 17/7/96
 
   

;#
;#  The IOCALL (calling C functions from assembly) macros do the following:
;#
;#	CALL_IOn - n is number of arguments to function
;#
;#	CALL_IO0
;# 		save ML state,
;#      call C function,
;#		restore ML state,
;#		return
;#
;#	CALL_IO1
;#	    save ML state,
;#		push %Reax,			ML arg1, C arg1
;#		call C function,
;#		pop 1 C argument,
;#		restore Poly state,
;#		return
;#
;#	CALL_IO2
;#		save ML state,
;#		push %Reax,			ML arg1, C arg2
;#		push %Rebx,			ML arg2, C arg1
;#		call C function,
;#		pop 2 C arguments
;#		restore ML state,
;#		return
;#
;#	CALL_IO3
;#		save ML state
;#		push %Reax,			ML arg1, C arg3
;#		push %Rebx,			ML arg2, C arg2
;#		push 3rd argument,		ML arg3, C arg1
;#		call C function,
;#		pop 3 C arguments,
;#		restore ML state
;#		pop 1 ML argument
;#		return
;#
;#	CALL_IO4 and above
;#		are not implemented
;#
;# except that, just to make life complicated, we have to remove indirections too.
;# Note that an ML function may be suspended when it makes a run-time system call
;# (ML may switch processes), so we have to save the ML state so that the run-time
;# call can be re-executed from scratch when the process is resumed.
;#
;#
IFDEF WINDOWS
.486

	.model	flat,c

;# No name munging needed in MASM
EXTNAME		TEXTEQU	<>

;# CALLMACRO is used to indicate to the converter that we have a macro
;# since macros have to be converted into C preprocessor macros.
CALLMACRO		TEXTEQU	<>

Reax		TEXTEQU	<eax>
Rebx		TEXTEQU	<ebx>
Recx		TEXTEQU	<ecx>
Redx		TEXTEQU	<edx>
Resi		TEXTEQU	<esi>
Redi		TEXTEQU	<edi>
Resp		TEXTEQU	<esp>
Rebp		TEXTEQU	<ebp>
R_cl		TEXTEQU	<cl>
R_bl		TEXTEQU	<bl>

CONST		TEXTEQU	<>

;#  gas-style instructions
;#  These are the reverse order from MASM.
movl		MACRO	f,t
			mov		t,f
			ENDM

movb		MACRO	f,t
			mov		t,f
			ENDM

addl		MACRO	f,t
			add		t,f
			ENDM

subl		MACRO	f,t
			sub		t,f
			ENDM

xorl		MACRO	f,t
			xor		t,f
			ENDM

orl			MACRO	f,t
			or		t,f
			ENDM

andl		MACRO	f,t
			and		t,f
			ENDM

cmpl		MACRO	f,t
			cmp		t,f
			ENDM

leal		MACRO	f,t
			lea		t,f
			ENDM

shrl		MACRO	f,t
			shr		t,f
			ENDM

sarl		MACRO	f,t
			sar		t,f
			ENDM

shll		MACRO	f,t
			shl		t,f
			ENDM

testl		MACRO	f,t
			test	t,f
			ENDM

imull		MACRO	f,t
			imul	t,f
			ENDM

mull		TEXTEQU	<mul>

incl		TEXTEQU	<inc>
negl		TEXTEQU	<neg>
pushl		TEXTEQU	<push>
popl		TEXTEQU	<pop>
popfl		TEXTEQU	<popfd>

;# boundl does not reverse its arguments.
boundl		TEXTEQU	<bound>

ELSE

;# External names in FreeBSD have a leading underscore.
;# This appears only to be the case in older versions.
#if defined(__FreeBSD__) && (__FreeBSD__ < 3)
#define EXTNAME(x)	_##x
#else
#define EXTNAME(x)	x
#endif

#define	Reax		%eax
#define	Rebx		%ebx
#define	Recx		%ecx
#define	Redx		%edx
#define	Resi		%esi
#define	Redi		%edi
#define	Resp		%esp
#define	Rebp		%ebp
#define R_cl		%cl
#define R_bl		%bl

#define CONST		$

#define END			


ENDIF


;# Register mask entries - must match coding used in codeCons.ML
IFDEF WINDOWS
M_Reax		EQU		1
M_Recx		EQU		2
M_Redx		EQU		4
M_Rebx		EQU		8
M_Resi		EQU		16
M_Redi		EQU		32

;# Set the register mask entry
RegMask		MACRO	name,mask
Mname		TEXTEQU	<Mask_&name&>
%Mname		EQU		mask
			ENDM

ELSE
;# Register mask entries - must match coding used in codeCons.ML
#define		M_Reax		1
#define		M_Recx		2
#define		M_Redx		4
#define		M_Rebx		8
#define		M_Resi		16
#define		M_Redi		32

#define		RegMask(name,mask) \
.set		Mask_##name,	mask

#define		OR	|

ENDIF
;# Default mask for unused entries.  This is set to all the registers
;# for safety in case a new function is added without adding an entry
;# here.
CALLMACRO	RegMask all,(M_Reax OR M_Rebx OR M_Recx OR M_Redx OR M_Resi OR M_Redi)


;# 
;# Specify the external variables.
;#
;#

IFDEF WINDOWS

;# MASM
MD_trap_handler1	PROTO	C

EXTERN end_of_stack:DWORD
EXTERN interface_map:DWORD
EXTERN poly_stack:DWORD
EXTERN processes:DWORD
EXTERN in_run_time_system:DWORD
EXTERN interrupted:DWORD
EXTERN store_profiling:DWORD
EXTERN save_vec:DWORD
EXTERN save_vec_addr:DWORD


ELSE

;# gas
.extern	EXTNAME(MD_trap_handler1)
.extern EXTNAME(end_of_stack)
.extern EXTNAME(interface_map)
.extern EXTNAME(poly_stack)
.extern EXTNAME(processes)
.extern EXTNAME(in_run_time_system)
.extern EXTNAME(interrupted)
.extern EXTNAME(store_profiling)
.extern EXTNAME(save_vec)
.extern EXTNAME(save_vec_addr)

ENDIF

IFDEF WINDOWS
INLINE_ROUTINE	MACRO	id
PUBLIC	id
id:
	xor	Recx,Recx	;# zap regCode
ENDM

ELSE

#define INLINE_ROUTINE(id) \
.global	EXTNAME(id); \
EXTNAME(id): 	xorl %ecx,%ecx

ENDIF

IFDEF WINDOWS
;#
;# LOCAL MEMORY
;#
LocalMemory	STRUCT
	DWORD	?
local_Mbottom	DWORD	?
local_Mpointer	DWORD	?
local_Mtop	DWORD	?
LocalMemory	ENDS

EXTERN A:LocalMemory

localMbottom	EQU		A.local_Mbottom
localMpointer	EQU		A.local_Mpointer
localMtop		EQU		A.local_Mtop

ELSE
;# gas version
.extern EXTNAME(A)

#define localMbottom	EXTNAME(A)+4
#define	localMpointer	EXTNAME(A)+8
#define	localMtop		EXTNAME(A)+12


ENDIF

IFDEF WINDOWS

;#
;# We must have room for WNT signal handles on the ML stack.
;# 4 bytes for length word and 1024 bytes for handlers (enough?)
;#
EXTRA_STACK	EQU	(4+1024)

;#
;# Tagged values.   A few operations, such as shift assume that the tag bit
;# is the bottom bit.
;#


TAG			EQU	1
TAGSHIFT	EQU	1
TAGMULT		EQU	2

TAGGED		MACRO	i
			LOCAL	t
			t	TEXTEQU	<i*2+1>
			EXITM	%t
ENDM

MAKETAGGED	MACRO	f,t
			lea		t,1[f*2]
ENDM

ELSE

.set	EXTRA_STACK,	(4+1024)

.set	TAG,		1
.set	TAGSHIFT,	1
.set	TAGMULT,	(1 << TAGSHIFT)

#define TAGGED(i) ((i << TAGSHIFT) | TAG)
#define MAKETAGGED(from,to) 	leal	TAG(,from,2),to

ENDIF


IFDEF WINDOWS

NIL			TEXTEQU		TAGGED(0)
UNIT		TEXTEQU		TAGGED(0)
ZERO		TEXTEQU		TAGGED(0)
FALSE		TEXTEQU		TAGGED(0)
TRUE		TEXTEQU		TAGGED(1)
MINUS1		TEXTEQU		TAGGED(0-1)
PSBITS		EQU			c0000000h
F_bytes		EQU			01000000h
F_mutable	EQU			40000000h
B_mutablebytes	EQU		41h
B_mutable	EQU			40h
Max_Length	EQU			00ffffffh

ELSE

.set	NIL,		TAGGED(0)
.set	UNIT,		TAGGED(0)
.set	ZERO,		TAGGED(0)
.set	FALSE,		TAGGED(0)
.set	TRUE,		TAGGED(1)
.set	MINUS1,		TAGGED(0-1)
.set	PSBITS,		0xc0000000
.set	F_bytes,	0x01000000
.set	F_mutable,	0x40000000
.set	B_mutable,	0x40
.set	B_mutablebytes,	0x41
.set	Max_Length,	0x00ffffff

ENDIF

;#
;# DATA SECTION
;#

IFDEF WINDOWS

.data
	PUBLIC	stack_limit
;	.align	2
saved_sp	dd	0
;mem_regs
;# The offsets in this segment are built into the code-generator.
mem_hp		dd	0		; Heap ptr
mem_regs	EQU	mem_hp
mem_hr		dd	0		; Handler register
mem_hl		dd	0		; Heap limit
			dd	7fffffffh
stack_limit	dd	0			; Stack limit - This is used to force a trap.
mem_sl		EQU	stack_limit
			dd	7fffffffh
;# End of section built into the code-generator.


saved_bp	dd	0
.CODE

ELSE

.data
;# gas
saved_sp:	.long	0


;# The offsets in this segment are built into the code-generator.

EXTNAME(mem_hp):		.long	0		;# Heap ptr
EXTNAME(mem_hr):		.long	0		;# Handler register
EXTNAME(mem_hl):		.long	0		;# Heap limit
		.long	0x7fffffff

.global EXTNAME(stack_limit)
EXTNAME(stack_limit):	.long	0		;# This is used to force a trap.
					;# Stack limit
		.long	0x7fffffff

.set	mem_regs,	EXTNAME(mem_hp)
.set	mem_sl,		EXTNAME(stack_limit)

;# End of section built into the code-generator.

saved_bp:	.long	0

.text

ENDIF

;#
;# Stack format from objects.h is:
;#	typedef struct
;#	{				 byte offset of start
;#	  word  p_space ;			 0
;#	  byte *p_pc ;				 4
;#	  word *p_sp ;				 8
;#	  word *p_hr ;				12
;#	  word  p_nreg ;			16 = no of checked registers (always CHECKED_REGS)
;#	  word  p_reg[1] ;			20
;#	} StackObject ;
;#
;# #defines from WNT_DEP.c:
;#	Register fields in poly_stack.
;#	#define EAX	poly_stack->p_reg[0]	20
;#	#define EBX	poly_stack->p_reg[1]	24
;#	#define ECX	poly_stack->p_reg[2]	28
;#	#define EDX	poly_stack->p_reg[3]	32
;#	#define ESI	poly_stack->p_reg[4]	36
;#	#define EDI	poly_stack->p_reg[5]	40
;#
;#	#define CHECKED_REGS	6
;#	The unchecked reg field is used for the condition codes.	
;#	#define UNCHECKED_REGS	1
;#
;#	#define EFLAGS	poly_stack->p_reg[CHECKED_REGS+1]
;#
 
;#
;# Starting offsets
;# What`s at offset 44? The number of unchecked registers (always UNCHECKED_REGS).
;# This field is used by the garbage collector
;#

IFDEF WINDOWS

SPACE_OFF	EQU	0
PC_OFF		EQU		4
SP_OFF		EQU		8
HR_OFF		EQU		12
EAX_OFF		EQU	20
EBX_OFF		EQU	24
ECX_OFF		EQU	28
EDX_OFF		EQU	32
ESI_OFF		EQU	36
EDI_OFF		EQU	40
FLAGS_OFF	EQU	48

ELSE

.set	SPACE_OFF,	0
.set	PC_OFF,		4
.set	SP_OFF,		8
.set	HR_OFF,		12
.set	EAX_OFF,	20
.set	EBX_OFF,	24
.set	ECX_OFF,	28
.set	EDX_OFF,	32
.set	ESI_OFF,	36
.set	EDI_OFF,	40
.set	FLAGS_OFF,	48

ENDIF


;#
;# CODE STARTS HERE
;#
	
;#Take the values from the stack and reloads the registers.
IFDEF WINDOWS
	PUBLIC	MD_switch_to_poly_X
MD_switch_to_poly_X:
ELSE
.global	EXTNAME(MD_switch_to_poly_X)
EXTNAME(MD_switch_to_poly_X):		;# Entry point from C
ENDIF
	addl	CONST 4,Resp		;# %Resp:=%Resp+4 - Return address is not needed. (Near call)
MD_switch_to_poly1:				;# Entry point from return_from_io

;#	cmpl	CONST 0,saved_sp	;# if saved_sp<>0 goto MDstp0 - Save on first time only
;#	jne		MDstp0
	movl	Resp,saved_sp		;# saved_sp:=%Resp - Save the system stack pointer.
	movl	Rebp,saved_bp		;# saved_bp:=%Rebp - added SPF 4/11/94
MDstp0:
	movl	EXTNAME(poly_stack),Rebp		;# %Rebp:=poly_stack
	movl	SPACE_OFF[Rebp],Reax	;# %Reax:=%Rebp - Set up stack limit
	leal	[Rebp+Reax*4],Reax	;# load effective address
	movl	Reax,mem_sl			;# mem_sl:=%Reax - set stack limit 

	  ;# Now we have set mem_sl (stack limit) we can be
	  ;# interrupted by setting it to the end of the stack.
 ;# While we were in the run-time system we may have had an interrupt which we
 ;# have not handled.  We test the flag here and set up a6 so that we will get
 ;# a trap as soon as we next test it.
	cmpl	CONST 0,EXTNAME(interrupted)		;# if interrupted==0 goto MDstr1 
					;# It may already have been set.
	je	MDstp1
	movl	(-4)[Rebp],Reax		;# Point to end of vector.
	andl	CONST Max_Length,Reax	;# Mask out flags
	leal	(-4)[Rebp+Reax*4],Reax	;# This will cause an interrupt soon.
	movl	Reax,mem_sl		;# mem_sl:=%Reax - set stack limit
MDstp1:
	movl	HR_OFF[Rebp],Reax
	movl	Reax,EXTNAME(mem_hr)		;# hr

	movl	localMbottom,Reax	 		;# Lower limit of locals	
	addl	CONST 4,Reax			
	movl	Reax,EXTNAME(mem_hl)	 	;# mem_hl := localMbottom + 4

	movl	localMpointer,Reax			;# Local pointer
	addl	CONST 4,Reax			
	movl	Reax,EXTNAME(mem_hp)		;# mem_hp := localMpointer + 4

 ;# If we are profiling store allocation we set mem_hl so that a trap
 ;# will be generated.
	cmpl	CONST 0,EXTNAME(store_profiling)	 
	je		MDstp2			;# if store_profiling==0	jump to MDstp2
	movl	Reax,EXTNAME(mem_hl)		;# mem_hl := mem_hp
;#	say 	EXTNAME(mem_hl),1
MDstp2:
	movl	EBX_OFF[Rebp],Rebx	;# Load the registers
	movl	ECX_OFF[Rebp],Recx
	movl	EDX_OFF[Rebp],Redx
	movl	ESI_OFF[Rebp],Resi
	movl	EDI_OFF[Rebp],Redi

 	movl	PC_OFF[Rebp],Reax	;# Look at pc
	cmpl	CONST ZERO,Reax
	jne		MDstp3			;# May be zero indicating a retry
;#	say     2,2
	movl	[Redx],Recx			;# so load from closure address
	movl	Recx,Reax			;# and set the code address for %Recx.
MDstp3:
	movl	SP_OFF[Rebp],Resp	;# sp
	pushl	Reax				;# push return address
;#	say     %Reax,3
	pushl	FLAGS_OFF[Rebp]		;# and flags field
	movl	EAX_OFF[Rebp],Reax	;# Now get %Reax (return value)
	leal	mem_regs,Rebp		;# reset %Rebp
;#	say %Rebp,4
	popfl				;# reset flags
	movl	CONST 0,EXTNAME(in_run_time_system)	;# in_run_time_system:=0 (stack now kosher)
 	ret

;#
;# MD_trap_handler - When a signal is caught we save the faulting pc and
;# then continue the signal from here.  This code saves the other
;# registers in the stack segment, switches back to the C stack and
;# then jumps to MD_trap_handler1 to process the signal.
;#

IFDEF WINDOWS
	PUBLIC	MD_trap_handler
MD_trap_handler	PROC
ELSE
.global EXTNAME(MD_trap_handler)
EXTNAME(MD_trap_handler):
ENDIF
	movl	EXTNAME(poly_stack),Rebp
	movl	Resp,SP_OFF[Rebp]		;# sp
	movl	Reax,EAX_OFF[Rebp]		;# general regs
	movl	Rebx,EBX_OFF[Rebp]
	movl	Recx,ECX_OFF[Rebp]
	movl	Redx,EDX_OFF[Rebp]
	movl	Resi,ESI_OFF[Rebp]
	movl	Redi,EDI_OFF[Rebp]
	movl	EXTNAME(mem_hr),Reax				;# hr
	movl	Reax,HR_OFF[Rebp]
	movl	EXTNAME(mem_hp),Reax			;# localMpointer := mem_hp - 4;
	subl	CONST 4,Reax
	movl	Reax,localMpointer
	movl	CONST 1,EXTNAME(in_run_time_system) ;# poly_stack is now up to date, Resp is about to be messed up.
	movl	saved_sp,Resp
	movl	saved_bp,Rebp			;# added 4/11/94 SPF
	jmp		EXTNAME(MD_trap_handler1)

IFDEF WINDOWS
MD_trap_handler	ENDP
ENDIF

;# Define standard call macros. They are of the form:
;#
;#  CALL_IOn name, amode1, ... , amode, res
;#
;# where n is the number of arguments.
;# For each argument there is an argument mode which is either REF if the
;# argument is passed by reference, and VAL if the argument is passed by value.
;# Note: the order of the modes is the same as Poly and the reverse of C.
;# The result mode is either IND if the result is by reference and NOIND if it
;# is not. The reason arguments or results may be passed by reference is that
;# the garbage-collector may move objects on the heap but will only update
;# values on the Poly stack. REF arguments are copied to the save_vec and the
;# address of the entry on it is returned.
;#
;# N.B. see comment at the start of the file about different Poly/ML and C
;# calling conventions. Note also that the above comment doesn`t explain
;# the difference between REF and REFa and between VAL and VALa. You
;# should use REF or VAL for the first two (register) arguments and
;# REFa or VALa for the remaining (stack) arguments.
;#
;# In the 3.3X run-time system, we use the REF mode for all arguments,
;# which simplifies things somewhat.
;# SPF 12/11/1998

IFDEF WINDOWS

IND		MACRO
		mov		Reax,[Reax]
ENDM

NOIND	MACRO
ENDM


CALL_IO0	MACRO	name,res
	LOCAL	Aname
	LOCAL	Cname
	Aname	TEXTEQU	<&name&a>
	Cname	TEXTEQU	<&name&c>
Cname	PROTO	C
	PUBLIC	Aname
Aname		PROC
	mov		in_run_time_system,1		        ; Must set this before we mess-up the ML state */
	lea		Recx,RTD0					; on return, pop 0 ML arguments from stack */
	call	zap2_and_set_for_retry			; save ML state, after zapping %eaxb and %Rebx */
	lea		Resi,save_vec
	mov		save_vec_addr,Resi
	call	Cname					; call C function */
	res								; remove any indirection from result */
	jmp	return_from_io				; restore ML state and return */
RTD0:	ret
Aname	ENDP
	RegMask	name,(M_Reax OR M_Rebx OR M_Recx)
ENDM

CALL_IO1	MACRO	name,res
	LOCAL	Aname
	LOCAL	Cname
	LOCAL	Mname
	Aname	TEXTEQU	<&name&a>
	Cname	TEXTEQU	<&name&c>
Cname	PROTO	C
	PUBLIC	Aname
Aname		PROC
	mov		in_run_time_system,1		        ; Must set this before we mess-up the ML state */
	lea		Recx,RTD0					; on return, pop 0 ML arguments from stack */
	call	zap1_and_set_for_retry		; save ML state, after zapping %Rebx */
	lea		Resi,save_vec
	mov		[Resi],Reax					; remove indirection from arg1 */
	push	Resi
	lea		Resi,4[Resi]
	mov		save_vec_addr,Resi
	call	Cname					; call C function */
	add		Resp,4					; pop 1 C argument from stack */
	res								; remove any indirection from result */
	jmp	return_from_io				; restore ML state and return */
RTD0:	ret
Aname	ENDP
	RegMask	name,(M_Reax OR M_Rebx OR M_Recx)
ENDM

CALL_IO2	MACRO	name,res
	LOCAL	Aname
	LOCAL	Cname
	Aname	TEXTEQU	<&name&a>
	Cname	TEXTEQU	<&name&c>
Cname	PROTO	C
	PUBLIC	Aname
Aname		PROC
	mov		in_run_time_system,1		        ; Must set this before we mess-up the ML state */
	lea		Recx,RTD0					; on return, pop 0 ML arguments from stack */
	call	set_registers_for_retry			; save ML state */
	lea		Resi,save_vec
	mov		[Resi],Reax					; remove indirection from arg1 */
	push	Resi
	lea		Resi,4[Resi]
	mov		[Resi],Rebx				; remove indirection from arg2 */
	push	Resi
	lea		Resi,4[Resi]
	mov		save_vec_addr,Resi
	call	Cname					; call C function */
	add		Resp,8					; pop 2 C arguments from stack */
	res								; remove any indirection from result */
	jmp	return_from_io				; restore ML state and return */
RTD0:	ret
Aname	ENDP
	RegMask	name,(M_Reax OR M_Rebx OR M_Recx)
ENDM

CALL_IO3	MACRO	name,res
	LOCAL	Aname
	LOCAL	Cname
	Aname	TEXTEQU	<&name&a>
	Cname	TEXTEQU	<&name&c>
Cname	PROTO	C
	PUBLIC	Aname
Aname		PROC
	mov		in_run_time_system,1		        ; Must set this before we mess-up the ML state */
	lea		Recx,RTD1				; on return, pop 1 ML argument from stack */
	call	set_registers_for_retry			; save ML state */
	lea		Resi,save_vec
	mov		[Resi],Reax					; remove indirection from arg1 */
	push	Resi
	lea		Resi,4[Resi]
	mov		[Resi],Rebx				; remove indirection from arg2 */
	push	Resi
	lea		Resi,4[Resi]
	mov		Reax,4[Redi]				; Get arg3 */
	mov		[Resi],Reax				; remove indirection from arg3 */
	push	Resi
	lea		Resi,4[Resi]
	mov		save_vec_addr,Resi
	call	Cname					; call C function */
	add		Resp,12				; pop 3 C arguments from stack */
	res						; remove any indirection from result */
	jmp	return_from_io				; restore ML state and return */
RTD1:	ret	4
Aname	ENDP
	RegMask	name,(M_Reax OR M_Rebx OR M_Recx)
ENDM

CALL_IO5	MACRO	name,res
	LOCAL	Aname
	LOCAL	Cname
	LOCAL	Mname
	Aname	TEXTEQU	<&name&a>
	Cname	TEXTEQU	<&name&c>
Cname	PROTO	C
	PUBLIC	Aname
Aname		PROC
	mov		in_run_time_system,1		        ; Must set this before we mess-up the ML state */
	lea		Recx,RTD3				; on return, pop 3 ML arguments from stack */
	call	set_registers_for_retry			; save ML state */
	lea		Resi,save_vec
	mov		[Resi],Reax
	push	Resi
	lea		Resi,4[Resi]
	mov		[Resi],Rebx
	push	Resi
	lea		Resi,4[Resi]
	mov		Reax,12[Redi]				; Get arg3 */
	mov		[Resi],Reax
	push	Resi
	lea		Resi,4[Resi]
	mov		Reax,8[Redi]				; Get arg4 */
	mov		[Resi],Reax
	push	Resi
	lea		Resi,4[Resi]
	mov		Reax,4[Redi]				; Get arg5 */
	mov		[Resi],Reax
	push	Resi
	lea		Resi,4[Resi]
	mov		save_vec_addr,Resi
	call	Cname					; call C function */
	add		Resp,20				; pop 5 C arguments from stack */
	res						; remove any indirection from result */
	jmp	return_from_io				; restore ML state and return */
RTD3:	ret	12
Aname	ENDP
	RegMask	name,(M_Reax OR M_Rebx OR M_Recx)
ENDM

ELSE
#define IND	movl	(Reax),Reax
#define	NOIND

#define CALL_IO0(name,res) \
.extern EXTNAME(name##c); \
.global EXTNAME(name##a); \
EXTNAME(name##a): movl	$1,EXTNAME(in_run_time_system); \
	leal	RTD0,Recx; \
	call	zap2_and_set_for_retry; \
	leal	EXTNAME(save_vec),Resi; \
	movl	Resi,EXTNAME(save_vec_addr); \
	call	EXTNAME(name##c); \
	res; \
	jmp	return_from_io; \
	RegMask(name,(M_Reax OR M_Rebx OR M_Recx))

#define CALL_IO1(name,res) \
.extern EXTNAME(name##c); \
.global EXTNAME(name##a); \
EXTNAME(name##a): 	movl	$1,EXTNAME(in_run_time_system); \
	leal	RTD0,Recx;\
	call	zap1_and_set_for_retry; \
	leal	EXTNAME(save_vec),Resi; \
	movl	Reax,(Resi); \
	pushl	Resi; \
	leal	4(Resi),Resi; \
	movl	Resi,EXTNAME(save_vec_addr); \
	call	EXTNAME(name##c); \
	add	$4,Resp; \
	res; \
	jmp	return_from_io; \
	RegMask(name,(M_Reax OR M_Rebx OR M_Recx))


#define CALL_IO2(name,res) \
.extern EXTNAME(name##c); \
.global EXTNAME(name##a); \
EXTNAME(name##a):	movl	$1,EXTNAME(in_run_time_system); \
	leal	RTD0,Recx; \
	call	set_registers_for_retry; \
	leal	EXTNAME(save_vec),Resi; \
	movl	Reax,(Resi); \
	pushl	Resi; \
	leal	4(Resi),Resi; \
	movl	Rebx,(Resi); \
	pushl	Resi; \
	leal	4(Resi),Resi; \
	movl	Resi,EXTNAME(save_vec_addr); \
	call	EXTNAME(name##c); \
	add	$8,Resp; \
	res; \
	jmp	return_from_io; \
	RegMask(name,(M_Reax OR M_Rebx OR M_Recx))


#define CALL_IO3(name,res) \
.extern EXTNAME(name##c); \
.global EXTNAME(name##a); \
EXTNAME(name##a):	movl	$1,EXTNAME(in_run_time_system); \
	leal	RTD1,Recx; \
	call	set_registers_for_retry; \
	leal	EXTNAME(save_vec),Resi; \
	movl	Reax,(Resi); \
	pushl	Resi; \
	leal	4(Resi),Resi; \
	movl	Rebx,(Resi); \
	pushl	Resi; \
	leal	4(Resi),Resi; \
	movl	4(Redi),Reax; \
	movl	Reax,(Resi); \
	pushl	Resi; \
	leal	4(Resi),Resi; \
	movl	Resi,EXTNAME(save_vec_addr); \
	call	EXTNAME(name##c); \
	add	$12,Resp; \
	res; \
	jmp	return_from_io; \
	RegMask(name,(M_Reax OR M_Rebx OR M_Recx))

#define CALL_IO5(name,res) \
.extern EXTNAME(name##c); \
.global EXTNAME(name##a); \
EXTNAME(name##a):	movl	$1,EXTNAME(in_run_time_system); \
	leal	RTD3,Recx; \
	call	set_registers_for_retry; \
	leal	EXTNAME(save_vec),Resi; \
	movl	Reax,(Resi); \
	pushl	Resi; \
	leal	4(Resi),Resi; \
	movl	Rebx,(Resi); \
	pushl	Resi; \
	leal	4(Resi),Resi; \
	movl	12(Redi),Reax; \
	movl	Reax,(Resi); \
	pushl	Resi; \
	leal	4(Resi),Resi; \
	movl	8(Redi),Reax; \
	movl	Reax,(Resi); \
	pushl	Resi; \
	leal	4(Resi),Resi; \
	movl	4(Redi),Reax; \
	movl	Reax,(Resi); \
	pushl	Resi; \
	leal	4(Resi),Resi; \
	movl	Resi,EXTNAME(save_vec_addr); \
	call	EXTNAME(name##c); \
	add	$20,Resp; \
	res; \
	jmp	return_from_io; \
	RegMask(name,(M_Reax OR M_Rebx OR M_Recx))

ENDIF


;#
;# Sets up the sp, pc and hr values.
;# Called with the arguments in %Reax, %Rebx and on the stack.  %Recx points to code
;# for a normal return which will pop any arguments more than 2 from the stack.
;# Returns with %Redi pointing to the previous top-of-stack so that arguments
;# can be loaded from it.
;#

IFDEF WINDOWS
	PUBLIC	zap2_and_set_for_retry
ELSE
.global	zap2_and_set_for_retry
ENDIF
zap2_and_set_for_retry:
	xorl	Reax,Reax
zap1_and_set_for_retry:
	xorl	Rebx,Rebx
set_registers_for_retry:
	movl	EXTNAME(poly_stack),Rebp
	movl	Recx,PC_OFF[Rebp]		;# pc for normal return
	movl	Reax,EAX_OFF[Rebp]
	movl	Rebx,EBX_OFF[Rebp]
IFDEF WINDOWS
	movl	CONST TAGGED(0),dword ptr ECX_OFF[Rebp]	;# Value for %Recx
ELSE
	movl	CONST TAGGED(0),ECX_OFF(Rebp)	;# Value for %ecx
ENDIF
	movl	Redx,EDX_OFF[Rebp]		;# Closure pointer
	movl	Resi,ESI_OFF[Rebp]
	movl	Redi,EDI_OFF[Rebp]
	
	movl	EXTNAME(mem_hr),Resi
	movl	Resi,HR_OFF[Rebp]		;# handler register
	movl	EXTNAME(mem_hp),Resi	;# localMpointer := mem_hp - 4
	leal	(-4)[Resi],Resi
	movl	Resi,localMpointer
	
	popl	Resi					;# return address (in CALL_IO macro)
	movl	Resp,SP_OFF[Rebp]		;# sp
	
	movl	Resp,Redi				;# REMOVED SPF 4/11/94; reinstated 11/11/94
	movl	saved_sp,Resp			;# Get back to C-stack
	pushl	Resi					;# Insert our return address there

	movl	saved_bp,Rebp		;# added 4/11/94 SPF (needed for WNT)
	ret

;# zap2_and_set_for_retry	ENDP

;#
;# RETURN FROM I/O
;#
;# Reload the registers saved above. Note - any of the registers may
;# have been modified by garbage collection so we must load all the registers
;# to ensure that we do not have registers pointing at invalid addresses.
;#
	
return_from_io:

	movl	EXTNAME(poly_stack),Resi		;# Get old stack base
	movl	Reax,EAX_OFF[Resi]	;# Set the result (passed back in %Reax)
;#	jmp		MD_switch_to_poly1
	call	EXTNAME(MD_switch_to_poly_X)	;#  Actually a jump because the return addr
							;# is popped.

IFDEF WINDOWS
;#
ELSE
RTD0:	ret				;# Pop return address
RTD1:	ret	$4
RTD3:	ret	$12
ENDIF

CALLMACRO	CALL_IO2	process_env_dispatch_,IND		;# DCJM 25/4/2000
CALLMACRO	CALL_IO1	shrink_stack_,NOIND			;# SPF 10/11/1998
CALLMACRO	CALL_IO2	set_flags_,NOIND			;# SPF 10/11/1998 
	
CALLMACRO	CALL_IO1	finish,NOIND 				;# SPF 11/11/94
CALLMACRO	CALL_IO1	install_root,NOIND
CALLMACRO	CALL_IO1	change_dir,NOIND
CALLMACRO	CALL_IO3	substring,IND	;# SPF 11/11/94
CALLMACRO	CALL_IO1	profiler,NOIND
CALLMACRO	CALL_IO0	commit,NOIND
CALLMACRO	CALL_IO3	createf,NOIND
CALLMACRO	CALL_IO3	Real_str,NOIND
CALLMACRO	CALL_IO2	Real_geq,NOIND
CALLMACRO	CALL_IO2	Real_leq,NOIND
CALLMACRO	CALL_IO2	Real_gtr,NOIND
CALLMACRO	CALL_IO2	Real_lss,NOIND
CALLMACRO	CALL_IO2	Real_eq,NOIND
CALLMACRO	CALL_IO2	Real_neq,NOIND
CALLMACRO	CALL_IO2	Real_dispatch,NOIND
CALLMACRO	CALL_IO2	Real_add,NOIND
CALLMACRO	CALL_IO2	Real_sub,NOIND
CALLMACRO	CALL_IO2	Real_mul,NOIND
CALLMACRO	CALL_IO2	Real_div,NOIND
CALLMACRO	CALL_IO1	Real_neg,NOIND
CALLMACRO	CALL_IO1	Real_int,NOIND
CALLMACRO	CALL_IO1	Real_float,NOIND
CALLMACRO	CALL_IO1	Real_sqrt,NOIND
CALLMACRO	CALL_IO1	Real_sin,NOIND
CALLMACRO	CALL_IO1	Real_cos,NOIND
CALLMACRO	CALL_IO1	Real_arctan,NOIND
CALLMACRO	CALL_IO1	Real_exp,NOIND
CALLMACRO	CALL_IO1	Real_ln,NOIND
CALLMACRO	CALL_IO1	Real_repr,NOIND
CALLMACRO	CALL_IO1	Real_conv,NOIND
CALLMACRO	CALL_IO2	fork_process,IND
CALLMACRO	CALL_IO2	choice_process,NOIND
CALLMACRO	CALL_IO1	int_process,NOIND
CALLMACRO	CALL_IO0	kill_self,NOIND
CALLMACRO	CALL_IO2	send_on_channel,NOIND
CALLMACRO	CALL_IO1	receive_on_channel,NOIND
CALLMACRO	CALL_IO1	objsize_,IND             ;# MJC
CALLMACRO	CALL_IO1	showsize_,IND             ;# MJC

CALLMACRO	CALL_IO2	timing_dispatch_,IND	;# DCJM 10/4/00
CALLMACRO	CALL_IO0	get_dbasetime_,IND			;# MJC 15/09/89
CALLMACRO	CALL_IO0	interrupt_console_processes_,NOIND	;# MJC 01/08/90

CALLMACRO	CALL_IO1	install_subshells_,NOIND		;# MJC 12/09/90

CALLMACRO	CALL_IO1	XWindows_,IND		;# MJC 27/09/90

CALLMACRO	CALL_IO0	full_gc_,NOIND			;# MJC 18/03/91 
CALLMACRO	CALL_IO0	stack_trace_,NOIND			;# MJC 18/03/91
    
CALLMACRO	CALL_IO2	foreign_dispatch_,IND  	;# NIC 22/04/94

CALLMACRO	CALL_IO3	IO_dispatch_,IND  	;# DCJM 8/5/00
CALLMACRO	CALL_IO2	Net_dispatch_,IND  	;# DCJM 22/5/00
CALLMACRO	CALL_IO2	OS_spec_dispatch_,IND  	;# DCJM 22/5/00
CALLMACRO	CALL_IO2	Sig_dispatch_,IND  	;# DCJM 18/7/00
CALLMACRO	CALL_IO2	foreign_result_,IND  	;# DCJM 7/6/01

;#
;# A number of functions implemented in Assembly for efficiency reasons
;#

CALLMACRO	INLINE_ROUTINE	int_to_word
 ;# Extract the low order 32 bits from a word.
	testl	CONST TAG,Reax
	jz		get_first_long_word_a
	ret					;# Return the argument
CALLMACRO	RegMask int_to_word,(M_Reax OR M_Recx)

 ;# This is now used in conjunction with isShort in Word.fromInt.
CALLMACRO	INLINE_ROUTINE	get_first_long_word_a
IFDEF WINDOWS
	test	byte ptr [Reax-1],CONST 16	;# 16 is the "negative" bit
ELSE
	testb	CONST 16,(-1)[Reax]		;# 16 is the "negative" bit
ENDIF
	movl	[Reax],Reax		;# Extract the word which is already little-endian
	jz		gfw1
	negl	Reax			;# We can ignore overflow
gfw1:
CALLMACRO	MAKETAGGED	Reax,Reax
	ret
CALLMACRO	RegMask get_first_long_word,(M_Reax OR M_Recx)



CALLMACRO    INLINE_ROUTINE	move_bytes
 ;# Move a segment of memory from one location to another.
 ;# Must deal with the case of overlapping segments correctly.
 ;# (source, sourc_offset, destination, dest_offset, length)

 ;# Assume that the offsets and length are all short integers.
	movl	12[Resp],Redi				;# Destination address
	cmpl	Redi,localMbottom		;# tests for non-local address
	ja      move_bytes_long        ;# jump if (unsigned) %Redi < localMbottom
	cmpl    Redi,localMtop
	jna     move_bytes_long        ;# jump if (unsigned) %Redi >= localMtop   
	movl	8[Resp],Recx				;# Destination offset, untagged
	shrl	CONST TAGSHIFT,Recx
	addl	Recx,Redi
	movl	Reax,Resi					;# Source address
	shrl	CONST TAGSHIFT,Rebx
	addl	Rebx,Resi
	movl	4[Resp],Recx				;# Get the length to move
	shrl	CONST TAGSHIFT,Recx
	cld								;# Default to increment Redi,Resi
	cmpl	Redi,Resi					;# Check for potential overlap
 ;# If dest > src then use decrementing moves else
 ;# use incrementing moves.
	ja		mvb1
	std								;# Decrement Redi,Resi
	leal	(-1)[Resi+Recx],Resi
	leal	(-1)[Redi+Recx],Redi
mvb1:
IFDEF WINDOWS
	rep movsb						;# Copy the bytes
ELSE
	rep
	movsb							;# Copy the bytes
ENDIF
	movl	CONST UNIT,Reax				;# The function returns unit
	movl	Reax,Rebx			    ;# Clobber bad value in %ebx
	movl	Reax,Recx				;# and %Recx
	movl	Reax,Redi
	movl	Reax,Resi
 ;# Visual Studio 5 C++ seems to assume that the direction flag
 ;# is cleared.  I think that`s a bug but we have to go along with it.
	cld
	ret		CONST 12

move_bytes_long:
CALLMACRO	CALL_IO5	move_bytes_long_,NOIND

CALLMACRO	RegMask move_bytes,Mask_all


CALLMACRO    INLINE_ROUTINE	move_words
 ;# Move a segment of memory from one location to another.
 ;# Must deal with the case of overlapping segments correctly.
 ;# (source, source_offset, destination, dest_offset, length)

 ;# Assume that the offsets and length are all short integers.
	movl	12[Resp],Redi				;# Destination address
	cmpl	Redi,localMbottom		;# tests for non-local address
	ja      move_words_long        ;# jump if (unsigned) %Resi < localMbottom
	cmpl    Redi,localMtop
	jna     move_words_long        ;# jump if (unsigned) %Resi >= localMtop   
	movl	8[Resp],Recx				;# Destination offset
	leal	(-2)[Redi+Recx*2],Redi		;# Destination address plus offset
	leal	(-2)[Reax+Rebx*2],Resi		;# Source address plus offset
	movl	4[Resp],Recx				;# Get the length to move (words)
	shrl	CONST TAGSHIFT,Recx
	cld								;# Default to increment Redi,Resi
	cmpl	Redi,Resi					;# Check for potential overlap
 ;# If dest > src then use decrementing moves else
 ;# use incrementing moves.
	ja		mvw1
	std								;# Decrement Redi,Resi
	leal	(-4)[Resi+Recx*4],Resi
	leal	(-4)[Redi+Recx*4],Redi
mvw1:
IFDEF WINDOWS
	rep movsd						;# Copy the words
ELSE
	rep
	movsl							;# Copy the words
ENDIF
	movl	Reax,Recx			    ;# Clobber bad values
	movl	CONST UNIT,Reax				;# The function returns unit
	movl	Reax,Redi
	movl	Reax,Resi
 ;# Visual Studio 5 C++ seems to assume that the direction flag
 ;# is cleared.  I think that`s a bug but we have to go along with it.
	cld
	ret		CONST 12

move_words_long:
CALLMACRO	CALL_IO5	move_words_long_,NOIND

CALLMACRO	RegMask move_words,Mask_all
;#

CALLMACRO    INLINE_ROUTINE	int_eq
	cmpl	Rebx,Reax
	je		RetTrue
RetFalse:
	movl	CONST FALSE,Reax
	ret
CALLMACRO	RegMask int_eq,(M_Reax OR M_Recx)

CALLMACRO	INLINE_ROUTINE	int_neq
	cmpl	Rebx,Reax
	je		RetFalse
RetTrue:
	movl	CONST TRUE,Reax
	ret
CALLMACRO	RegMask int_neq,(M_Reax OR M_Recx)

CALLMACRO	INLINE_ROUTINE	not_bool
	xorl	CONST (TRUE-TAG),Reax	;# Change the value but leave the tag
	ret
CALLMACRO	RegMask not_bool,(M_Reax OR M_Recx)

 ;# or, and, xor shift etc. assume the values are tagged integers
CALLMACRO	INLINE_ROUTINE	or_word
	orl		Rebx,Reax
	ret
CALLMACRO	RegMask or_word,(M_Reax OR M_Recx)

CALLMACRO	INLINE_ROUTINE	and_word
	andl	Rebx,Reax
	ret
CALLMACRO	RegMask and_word,(M_Reax OR M_Recx)

CALLMACRO	INLINE_ROUTINE	xor_word
	xorl	Rebx,Reax
	orl		CONST TAG,Reax	;# restore the tag
	ret
CALLMACRO	RegMask xor_word,(M_Reax OR M_Recx)

CALLMACRO	INLINE_ROUTINE	shift_left_word
 ;# Assume that both args are tagged integers
 ;# Word.<<(a,b) is defined to return 0 if b > Word.wordSize
	cmpl	CONST TAGGED(31),Rebx
	jb		slw1
	movl	CONST ZERO,Reax
	ret
slw1:
	movl	Rebx,Recx
	shrl	CONST TAGSHIFT,Recx	;# remove tag
	subl	CONST TAG,Reax
	shll	R_cl,Reax
	orl		CONST TAG,Reax	;# restore the tag
	movl	Reax,Recx		;# clobber %Recx
	ret
CALLMACRO	RegMask shift_left_word,(M_Reax OR M_Recx)

CALLMACRO	INLINE_ROUTINE	shift_right_word
 ;# Word.>>(a,b) is defined to return 0 if b > Word.wordSize
	cmpl	CONST TAGGED(31),Rebx
	jb		srw1
	movl	CONST ZERO,Reax
	ret
srw1:
	movl	Rebx,Recx
	shrl	CONST TAGSHIFT,Recx	;# remove tag
	shrl	R_cl,Reax
	orl		CONST TAG,Reax	;# restore the tag
	movl	Reax,Recx		;# clobber %Recx
	ret
CALLMACRO	RegMask shift_right_word,(M_Reax OR M_Recx)

CALLMACRO	INLINE_ROUTINE	shift_right_arith_word
 ;# Word.~>>(a,b) is defined to return 0 or ~1 if b > Word.wordSize
 ;# The easiest way to do that is to set the shift to 31.
	cmpl	CONST TAGGED(31),Rebx
	jb		sra1
	movl	CONST TAGGED(31),Rebx
sra1:
	movl	Rebx,Recx
	shrl	CONST TAGSHIFT,Recx	;# remove tag
	sarl	R_cl,Reax
	orl		CONST TAG,Reax	;# restore the tag
	movl	Reax,Recx		;# clobber %Recx
	ret
CALLMACRO	RegMask shift_right_arith_word,(M_Reax OR M_Recx)


CALLMACRO	INLINE_ROUTINE	get_dbentrya
 ;# Get the database specific entry.
	movl	EXTNAME(processes),Reax		;# Points to the root vector.
	movl	8[Reax],Reax
	ret
CALLMACRO	RegMask get_dbentry,(M_Reax OR M_Recx)

;# needs to call runtime system because of changing refs

CALLMACRO 	CALL_IO1	set_dbentry_,NOIND

CALLMACRO	CALL_IO1	BadOpCode_,NOIND

CALLMACRO	INLINE_ROUTINE	io_operation
 ;# Returns the address of an entry in io_vector
	shrl	CONST TAGSHIFT,Reax		;# Remove tag
IFDEF WINDOWS
	movl	EXTNAME(interface_map)[Reax*4],Reax
ELSE
	movl	EXTNAME(interface_map)(,Reax,4),Reax
ENDIF
	ret
CALLMACRO	RegMask io_operation,(M_Reax OR M_Recx)

CALLMACRO	INLINE_ROUTINE	locksega
 ;# Clears the "mutable" bit on a segment

IFDEF WINDOWS
	and		byte ptr	[Reax-1],CONST(0ffh-B_mutable)
ELSE
	andb	CONST(0xff-B_mutable),-1[Reax]
ENDIF
	ret
CALLMACRO	RegMask lockseg,M_Recx

;#INLINE_ROUTINE(get_flags)
;#	cmpl	data0,%Reax
;#	jb	vf1		; skip if < data0
;#	movzbl	(%Reax-1),%Reax	; if > data0 return flag
;#	shll	$TAGSHIFT,%Reax	; Tag it
;#	orl	$TAG,%Reax
;#	ret
;#
;#vf1:	mov	$TAGGED(256),%Reax ; if < data0 must be in io area, return 256
;#	ret

;# For backwards compatibility this needs to call the RTS.
;# In due course it should be possible to have it simply return
;# the top byte of the length word as a tagged integer. 
CALLMACRO	CALL_IO1	get_flags_,NOIND

;# CALLMACRO	INLINE_ROUTINE	get_flags_a
;#	movzx	Reax, byte ptr [Reax-1]
;#	leal	1[Reax*2],Reax
;#	ret
;# CALLMACRO	RegMask get_flags_,(M_Recx OR M_Reax)


CALLMACRO	INLINE_ROUTINE	get_length_a
	movl	(-4)[Reax],Reax
	shll	CONST 8,Reax			;# Clear top byte
	shrl	CONST(8-TAGSHIFT),Reax	;# Make it a tagged integer
	orl	CONST TAG,Reax
	ret
CALLMACRO	RegMask get_length,(M_Reax OR M_Recx)

;#
;# Translated from SPARC (Added SPF 7/7/94) MDF 27/9/94
;# Calls a procedure with a given argument vector. The argument vector must
;# be copied onto the stack. If the argument vector is nil then there are no
;# arguments. One difference between "callcode_tupled" and "callcode" is that
;# "callcode_tupled" expects a single parameter which should be an ML pair of
;# the function closure and the argument tuple, whereas "callcode" expects
;# the closure and the argument tuple to be already in registers. The
;# other difference is that "callcode" extracts the arguments from the
;# tuple *backwards*, so reasons that I don`t understand.
;#
	
CALLMACRO	INLINE_ROUTINE	callcode_tupled
	movl	[Reax],Redx		;# %Redx = closure address
	movl	4[Reax],Resi	;# %Resi = arg-vec address; "touches" pair

	cmpl	CONST NIL,Resi	;# Is arg address nil?
	je		clcdt9			;# Skip if it is

	movl	(-4)[Resi],Recx		;# get length word of arg-vec; "touches" arg-vec
	andl	CONST Max_Length,Recx	;# remove flags. Now %Recx = number of arguments (can`t be 0). */

	movl	[Resi],Reax		;# First arg; mov is safe since arg-vec is "touched"
	loop	clcdt1			;# If only 1 arg, go to end, else carry on
	jmp		clcdt9
clcdt1:
	movl	4[Resi],Rebx		;# Second arg
	loop	clcdt7			;# If only 2 args, go to end, else carry on
	jmp		clcdt9
clcdt7:
;# Other arguments are put onto the stack.
clcdt8:
	pushl	8[Resi]		;# Push next arg (+8 because 2 args already done)
	addl	CONST 4,Resi
	loop	clcdt8		;# last one?
clcdt9:	
 ;# All args loaded
	movl	CONST UNIT,Resi
	movl	[Redx],Recx
IFDEF	WINDOWS
	jmp		Recx		;# and jump to the procedure.
ELSE
	jmp		*Recx		;# and jump to the procedure.
ENDIF
CALLMACRO	RegMask callcode_tupled,Mask_all ;# Calls an unknown function.


CALLMACRO	INLINE_ROUTINE	is_shorta
;# Returns true if the argument is tagged
	andl	CONST TAG,Reax
	jz		RetFalse
	jmp		RetTrue
CALLMACRO	RegMask is_short,(M_Reax OR M_Recx)

CALLMACRO	INLINE_ROUTINE	string_length
	testl	CONST TAG,Reax	;# Single char strings are represented by the
	jnz		RetOne		;# character.
	movl	[Reax],Reax	;# Get length field
CALLMACRO	MAKETAGGED	Reax,Reax
	ret
RetOne:	movl	CONST TAGGED(1),Reax
	ret
CALLMACRO	RegMask string_length,(M_Reax OR M_Recx)

 ;# Store the length of a string in the first word.
CALLMACRO	INLINE_ROUTINE	set_string_length_a
	shrl	CONST TAGSHIFT,Rebx	;# Untag the length
	movl	Rebx,[Reax]
	movl	CONST UNIT,Reax		;# Return unit
	movl	Reax,Rebx			;# Clobber untagged value
	ret
CALLMACRO	RegMask set_string_length,(M_Reax OR M_Recx OR M_Rebx)


CALLMACRO	INLINE_ROUTINE	string_sub
 ;# String subscript:  1 <= index <= length
	movl	Rebx,Redi
	shrl	CONST TAGSHIFT,Redi		;# Check index
	jle		Raise_sub		;# Must be > 0
	testl	CONST TAG,Reax	;# Single char?
	jnz		ssub1
	cmpl	[Reax],Redi
	jg		Raise_sub		;# Check index against length
IFDEF WINDOWS
	movzx	Reax, byte ptr 3[Reax+Redi]	;# Get character
ELSE
	movzbl	3[Reax,Redi],Reax	;# Get character
ENDIF
CALLMACRO	MAKETAGGED	Reax,Reax
	movl	Reax,Redi			;# Clobber bad value in %Redi
	ret

 ;# Single character - index must be 1.  If it is return the string/character
ssub1:	cmpl	CONST TAGGED(1),Rebx		;# Check index is 1
	jne		Raise_sub
	ret
CALLMACRO	RegMask string_sub,(M_Reax OR M_Recx OR M_Redi)

raise_range:
	movl	CONST TAGGED(4),Reax
	jmp		EXTNAME(raise_exa)

Raise_divide:
	movl	CONST TAGGED(7),Reax
	jmp		EXTNAME(raise_exa)

Raise_sub:
	movl	Redi,Reax		;# Clobber bad value in %Redi
	movl	CONST TAGGED(11),Reax
 ;# raise_exa:
CALLMACRO	CALL_IO1	raise_ex,NOIND

;# raisex (formerly raisexn) is used by compiled code.
CALLMACRO	INLINE_ROUTINE	raisex
	movl	EXTNAME(mem_hr),Recx	;# Get next handler into %ecx
 ;# Loop to find the handler for this exception. Handlers consist of one or more
 ;# pairs of identifier and code address, followed by the address of the next
 ;# handler.
rsx1:
IFDEF WINDOWS
	cmp		dword ptr [Recx],TAGGED(0)
ELSE
	cmpl	CONST TAGGED(0),[Recx]
ENDIF
	je		rsx7		;# default handler if it is TAGGED(0)
	movl	[Recx],Rebx	;# Arg1 - the identifier for this handler
	cmpl	[Reax],Rebx	;# Compare with exception tag - Have we got the right handler?
	je		rsx7		;# Skip if we found a match.
	addl	CONST 8,Recx		;# Look at the next handler.
	movl	[Recx],Rebx
	cmpl	Recx,Rebx		;# Is it a pointer to the next handler i.e.
	jb		rsx1		;# does it point further up the stack or at itself.
						;# (The last handler on the stack points at itself).
	cmpl	EXTNAME(end_of_stack),Rebx
	ja		rsx1		;# If not it must be a new pair, so look at that.
	movl	Rebx,Recx		;# It is a pointer to a new handler.
	jmp		rsx1

rsx7:	;# We have found the right handler - %Recx points to the data
	addl	CONST 4,Recx		;# point it at the code

	movl	[Recx],Redx		;# Get the handler entry point

 ;# There may be some other identifier/entry point pairs in this group.
 ;# We have to remove them and find the pointer to the next handler in the
 ;# chain.  This becomes the new handler pointer.
rsx6:	addl	CONST 4,Recx
	movl	[Recx],Rebx
	cmpl	Recx,Rebx	;# Is it a pointer to the next handler i.e.
	jb		rsx6		;# does it point further up the stack or at itself?
	cmpl	EXTNAME(end_of_stack),Rebx
	ja		rsx6

;# We`re now pointing to the pointer to the next handler.
	cmpl	CONST TAGGED(0),Redx	;# See if it was set up by exception_trace
	je		rsx9
 ;# Ordinary exception
	movl	Recx,Resp		;# Move stack pointer to handler frame
	popl	EXTNAME(mem_hr)	;# Load previous handler
	movl	CONST UNIT,Rebx	;# The values in some regs are illegal.
	movl	CONST UNIT,Recx
	pushl	Redx
	movl	Rebx,Redx
	ret			;# Now enter the handler

rsx9:
 ;# Must give an exception trace - ex_tracec unwinds to the next handler.
	movl	Reax,Rebx
	movl	Recx,Reax
CALLMACRO	CALL_IO2	ex_trace,NOIND

CALLMACRO	INLINE_ROUTINE	exception_tracea
 ;# Calls a procedure with no arguments and, if it returns normally, returns
 ;# its result. If the procedure raises an exception it prints a trace of the
 ;# stack from the place where the exception was first raised.
	pushl	EXTNAME(mem_hr)			;# Set up handler - save old handler
	pushl	CONST TAGGED(0)		;# push handler address.
	pushl	CONST TAGGED(0)		;# and exception id.
	movl	Resp,EXTNAME(mem_hr)		;# hr now points here.
	movl	Reax,Redx			;# Get argument (the procedure)
	movl	[Redx],Recx
 ;# The values on the stack must not point directly into this assembly
 ;# code segment otherwise there would be problems if we wrote out the stack.
 ;# Instead we indirect through the interface map.
	pushl	EXTNAME(interface_map)+(13*4)	;# Return to ``return_code``
	movl	CONST UNIT,Reax		;# Give the procedure a unit argument
IFDEF	WINDOWS
	jmp		Recx		;# Enter the procedure.
ELSE
	jmp		*Recx		;# Enter the procedure.
ENDIF
CALLMACRO	RegMask exception_trace,Mask_all	;# Calls unknown function

IFDEF WINDOWS
	PUBLIC	return_code		;# It is NOT a function entry point, so do not use INLINE_ROUTINE macro
return_code:
ELSE
	.globl	EXTNAME(return_code)
EXTNAME(return_code):
ENDIF
 ;# If it all works we return here. Now remove the handler.
	addl	CONST 8,Resp			;# Remove handler
	popl	EXTNAME(mem_hr)			;# Restore old handler
	ret

CALLMACRO	INLINE_ROUTINE	load_byte
	movl	Rebx,Redi
	shrl	CONST TAGSHIFT,Redi
IFDEF WINDOWS
	movzx	Redi, byte ptr [Reax][Redi]
ELSE
	movzbl	(Reax,Redi,1),Redi
ENDIF
CALLMACRO	MAKETAGGED	Redi,Reax
	movl	Reax,Redi		;# Clobber bad value in %Redi
	ret
CALLMACRO	RegMask load_byte,(M_Reax OR M_Recx OR M_Redi)

CALLMACRO	INLINE_ROUTINE	load_word
	shrl	CONST TAGSHIFT,Rebx
	movl	[Reax+Rebx*4],Reax
	movl	Reax,Rebx
	ret
CALLMACRO	RegMask load_word,(M_Reax OR M_Rebx OR M_Recx)

CALLMACRO	INLINE_ROUTINE	assign_byte

;# We can assume that the data value will not overflow 30 bits (it is only 1 byte!)
	movl	4[Resp],Recx
	shrl	CONST TAGSHIFT,Recx       ;# Remove tags from data value

;# We can assume that the index will not overflow 30 bits i.e. it is a tagged short
	shrl	CONST TAGSHIFT,Rebx		;# Remove tags from offset
	movb 	R_cl,[Reax+Rebx]

	movl	CONST UNIT,Reax				;# The function returns unit
	movl	Reax,Rebx			        ;# Clobber bad value in %Rebx
	movl	Reax,Recx					;# and %Recx
	ret		CONST 4
CALLMACRO	RegMask assign_byte,(M_Reax OR M_Rebx OR M_Recx)


CALLMACRO	INLINE_ROUTINE	assign_word
	movl	4[Resp],Recx

;# It would be fancier to write
;#	movb	cl,(%eax+%ebx*2-2)        
;# instead of the next 2 instructions and this would
;# also free us from the need to clobber %Rebx. (SPF 11/5/95)
;# Done.  DCJM 10/10/99.     

	movl	Recx,(-2)[Reax+Rebx*2]
    movl	CONST UNIT,Reax           ;# The function returns unit
    ret		CONST 4
CALLMACRO	RegMask assign_word,(M_Reax OR M_Recx)


;# alloc(size, flags, initial).  Allocates a segment of a given size and
;# initialises it.
;#
;# This is primarily used for arrays and for strings.  Refs are
;# allocated using inline code.
CALLMACRO	INLINE_ROUTINE	alloc_store
 ;# alloc(size, flags, initial).  Allocates a segment of a given size and
 ;# initialises it.
 ;# First check that the length is acceptable
	testl	CONST TAG,Reax
	jz		raise_range
	movl	Reax,Redi
	shrl	CONST TAGSHIFT,Redi		;# Remove tag
	jnz		allst0					;# (test for 0) Make zero sized objects 1
	movl	CONST 1,Redi			;# because they mess up the g.c.
	movl	CONST TAGGED(1),Reax
allst0:
	cmpl	CONST Max_Length,Redi	;# Length field must fit in 24 bits
	ja		raise_range
	incl	Redi					;# Add 1 word
	shll	CONST 2,Redi			;# Get length in bytes
	movl	[Rebp],Redx
	subl	Redi,Redx				;# Allocate the space
	movl	Reax,Redi				;# Clobber bad value in Redi
	boundl	Redx,8[Rebp]		;# Check for free space.  May trap.
					;# N.B. MD_trap_handler1 assumes the
					;# bound instruction is 3 bytes.
	movl	Redx,[Rebp]				;# Put back in the heap ptr
	shrl	CONST TAGSHIFT,Reax
	movl	Reax,(-4)[Redx]			;# Put in length
	shrl	CONST TAGSHIFT,Rebx		;# remove tag from flag
	orl		CONST B_mutable,Rebx	;# set mutable bit
	movb	R_bl,(-1)[Redx]			;# and put it in.
 ;# Initialise the store.
	movl	Reax,Recx				;# Get back the no. of words.
	movl	4[Resp],Reax			;# Get initial value.
	cmpl	CONST B_mutablebytes,Rebx
	jne		allst2

 ;# If this is a byte seg
	shrl	CONST TAGSHIFT,Reax	;# untag the initialiser
	shll	CONST 2,Recx		;# Convert to bytes
	movl	Redx,Redi
IFDEF WINDOWS
	rep stosb
ELSE
	rep
	stosb
ENDIF
	jmp		allst3

 ;# If this is a word segment
allst2:
	movl	Redx,Redi
IFDEF WINDOWS
	rep stosd
ELSE
	rep
	stosl
ENDIF

allst3:
	movl	Redx,Reax

	movl	Reax,Recx		;# Clobber these
	movl	Reax,Redx
	movl	Reax,Rebx
	movl	Reax,Redi
	ret		CONST 4
CALLMACRO	RegMask alloc_store,(M_Reax OR M_Rebx OR M_Recx OR M_Redx OR M_Redi)


 ;#CALLMACRO	CALL_IO3	alloc_store_long_,NOIND

CALLMACRO	CALL_IO2	strconcat,IND

CALLMACRO	INLINE_ROUTINE	add_long
	movl	Reax,Redi
	andl	Rebx,Redi
	andl	CONST TAG,Redi
	jz		add_really_long
	leal	(-TAG)[Reax],Redi
	addl	Rebx,Redi
	jo		add_really_long
	movl	Redi,Reax
	ret
add_really_long:
	movl	Reax,Redi
CALLMACRO	CALL_IO2	add_long,IND
CALLMACRO	RegMask aplus,(M_Reax OR M_Recx OR M_Redi OR Mask_add_long)

CALLMACRO	INLINE_ROUTINE	sub_long
	movl	Reax,Redi
	andl	Rebx,Redi
	andl	CONST TAG,Redi
	jz		sub_really_long
	movl	Reax,Redi
	subl	Rebx,Redi
	jo		sub_really_long
	leal	TAG[Redi],Reax		;# Put back the tag
	movl	Reax,Redi
	ret
sub_really_long:
	movl	Reax,Redi
CALLMACRO	CALL_IO2	sub_long,IND
CALLMACRO	RegMask aminus,(M_Reax OR M_Recx OR M_Redi OR Mask_sub_long)

CALLMACRO	INLINE_ROUTINE	mult_long
	movl	Reax,Redi
	andl	Rebx,Redi
	andl	CONST TAG,Redi
	jz		mul_really_long
	movl	Rebx,Redi
	sarl	CONST TAGSHIFT,Redi	;# Shift multiplicand
	movl	Reax,Resi
	subl	CONST TAG,Resi			;# Just subtract off the tag off multiplier
	imull	Redi,Resi
	jo		mul_really_long
	addl	CONST TAG,Resi
	movl	Resi,Reax
	movl	Reax,Redi
	ret
mul_really_long:
	movl	Reax,Resi		;# Clobber this
	movl	Reax,Redi
CALLMACRO	CALL_IO2	mult_long,IND
CALLMACRO	RegMask amul,(M_Reax OR M_Recx OR M_Redi OR M_Resi OR Mask_mult_long)

CALLMACRO	INLINE_ROUTINE	div_long
	movl	Reax,Redi
	andl	Rebx,Redi
	andl	CONST TAG,Redi			;# %Redi now contains $0 or $1 (both legal!)
	jz		div_really_long
	cmpl	CONST TAGGED(0),Rebx	;# Could just handle the trap
	jz		Raise_divide		;# but it seems hardly worth it.
 ;# The only case of overflow is dividing the smallest negative number by -1
	cmpl	CONST TAGGED((-1)),Rebx
	jz		div_really_long
	sarl	CONST TAGSHIFT,Reax
	movl	Rebx,Redi
	sarl	CONST TAGSHIFT,Redi
	cdq
	idiv	Redi
CALLMACRO	MAKETAGGED	Reax,Reax
	movl	Reax,Redx
	movl	Reax,Redi
	ret
div_really_long:
	movl	Reax,Redi
CALLMACRO	CALL_IO2	div_long,IND
CALLMACRO	RegMask adiv,(M_Reax OR M_Recx OR M_Redi OR M_Redx OR Mask_div_long)

CALLMACRO	INLINE_ROUTINE	rem_long
	movl	Reax,Redi
	andl	Rebx,Redi
	andl	CONST TAG,Redi		;# %Redi now contains $0 or $1 (both legal!
	jz		rem_really_long
	cmpl	CONST TAGGED(0),Rebx	;# Could just handle the trap
	jz		Raise_divide		;# but it seems hardly worth it.
 ;# The only case of overflow is dividing the smallest negative number by -1
	cmpl	CONST TAGGED((-1)),Rebx
	jz		rem_really_long
	sarl	CONST TAGSHIFT,Reax
	movl	Rebx,Redi
	sarl	CONST TAGSHIFT,Redi
	cdq
	idiv	Redi
CALLMACRO	MAKETAGGED	Redx,Reax
	movl	Reax,Redx
	movl	Reax,Redi
	ret
rem_really_long:
	movl	Reax,Redi
CALLMACRO	CALL_IO2	rem_long,IND
CALLMACRO	RegMask amod,(M_Reax OR M_Recx OR M_Redi OR M_Redx OR Mask_rem_long)

CALLMACRO	INLINE_ROUTINE	equal_long
	cmpl	Reax,Rebx
	je		RetTrue
	movl	Reax,Recx	;# If either is short
	orl		Rebx,Reax	;# the result is false
	andl	CONST TAG,Reax
	jnz		RetFalse
	movl	Recx,Reax
CALLMACRO	CALL_IO2	equal_long,NOIND
CALLMACRO	RegMask equala,(M_Reax OR M_Recx OR Mask_equal_long)


CALLMACRO	INLINE_ROUTINE	or_long
IFDEF NOTATTHEMOMENT
	movl	Reax,Redi
	andl	Rebx,Redi
	andl	CONST TAG,Redi
	jz		or_really_long
	orl		Rebx,Reax
	movl	Reax,Redi
	ret
or_really_long:
ENDIF
CALLMACRO	CALL_IO2	or_long,IND
CALLMACRO	RegMask ora,(M_Reax OR M_Recx OR M_Redi OR Mask_or_long)

CALLMACRO	INLINE_ROUTINE	xor_long
IFDEF NOTATTHEMOMENT
	movl	Reax,Redi
	andl	Rebx,Redi
	andl	CONST TAG,Redi
	jz		xor_really_long
	xorl	Rebx,Reax
	orl		CONST TAG,Reax	;# restore the tag
	movl	Reax,Redi
	ret
xor_really_long:
ENDIF
CALLMACRO	CALL_IO2	xor_long,IND
CALLMACRO	RegMask xora,(M_Reax OR M_Recx OR M_Redi OR Mask_xor_long)

CALLMACRO	INLINE_ROUTINE	and_long
IFDEF NOTATTHEMOMENT
	movl	Reax,Redi
	andl	Rebx,Redi
	andl	CONST TAG,Redi
	jz		and_really_long
	andl	Rebx,Reax
	movl	Reax,Redi
	ret
and_really_long:
ENDIF
CALLMACRO	CALL_IO2	and_long,IND
CALLMACRO	RegMask anda,(M_Reax OR M_Recx OR M_Redi OR Mask_and_long)

CALLMACRO	INLINE_ROUTINE	neg_long
	testl	CONST TAG,Reax
	jz		neg_really_long
	movl	CONST (TAGGED(0)+TAG),Redi
	subl	Reax,Redi
	jo		neg_really_long
	movl	Redi,Reax
	ret
neg_really_long:
	movl	Reax,Redi
CALLMACRO	CALL_IO1	neg_long,IND
CALLMACRO	RegMask aneg,(M_Reax OR M_Recx OR M_Redi OR Mask_neg_long)

CALLMACRO	INLINE_ROUTINE	int_geq
	movl	Reax,Recx	;# Use long test if either is long
	andl	Rebx,Reax
	andl	CONST TAG,Reax
	jz		igeq1
	cmpl	Rebx,Recx
	jge		RetTrue
	jmp		RetFalse
igeq1:
	movl	Recx,Reax
CALLMACRO	CALL_IO2	ge_long,NOIND
CALLMACRO	RegMask int_geq,(M_Reax OR M_Recx OR Mask_ge_long)

CALLMACRO	INLINE_ROUTINE	int_leq
	movl	Reax,Recx
	andl	Rebx,Reax
	andl	CONST TAG,Reax
	jz		ileq1
	cmpl	Rebx,Recx
	jle		RetTrue
	jmp		RetFalse
ileq1:
	movl	Recx,Reax
CALLMACRO	CALL_IO2	le_long,NOIND
CALLMACRO	RegMask int_leq,(M_Reax OR M_Recx OR Mask_le_long)

CALLMACRO	INLINE_ROUTINE	int_gtr
	movl	Reax,Recx
	andl	Rebx,Reax
	andl	CONST TAG,Reax
	jz		igtr1
	cmpl	Rebx,Recx
	jg		RetTrue
	jmp		RetFalse
igtr1:
	movl	Recx,Reax
CALLMACRO	CALL_IO2	gt_long,NOIND
CALLMACRO	RegMask int_gtr,(M_Reax OR M_Recx OR Mask_gt_long)

CALLMACRO	INLINE_ROUTINE	int_lss
	movl	Reax,Recx
	andl	Rebx,Reax
	andl	CONST TAG,Reax
	jz		ilss1
	cmpl	Rebx,Recx
	jl		RetTrue
	jmp		RetFalse
ilss1:
	movl	Recx,Reax
CALLMACRO	CALL_IO2	ls_long,NOIND
CALLMACRO	RegMask int_lss,(M_Reax OR M_Recx OR Mask_ls_long)

CALLMACRO	INLINE_ROUTINE	offset_address
 ;# This is needed in the code generator, but is a very risky thing to do.
	shrl	CONST TAGSHIFT,Rebx		;# Untag
	addl	Rebx,Reax		;# and add in
	movl	Reax,Rebx
	ret
CALLMACRO	RegMask offset_address,(M_Reax OR M_Recx OR M_Rebx)

CALLMACRO	INLINE_ROUTINE	teststreq
	movl	Reax,Recx		;# Are either just single chars?
	orl		Rebx,Reax
	andl	CONST TAG,Reax
	jz		teststreq2
	cmpl	Rebx,Recx		;# Must be identical
	jz		RetTrue
	jmp		RetFalse
teststreq2:
	movl	Recx,Reax
	movl	Rebx,Redi		;# Move ready for cmpsb.
	movl	[Reax],Recx		;# Get length
	addl	CONST 4,Recx	;# add 4 for the length field.
	movl	Reax,Resi		;# Move to correct reg for cmpsb
	cld						;# Make sure we increment
	cmpl	Reax,Reax		;# Set the Zero bit
;# Compare the strings.  Because the length field is at the beginning
;# it does not matter if the value we loaded into %Recx is wrong
IFDEF WINDOWS
	repe	cmpsb
ELSE
	repe	
	cmpsb
ENDIF
	movl	Reax,Resi		;# Make these valid
	movl	Reax,Recx
	movl	Reax,Redi
	jz		RetTrue
	jmp		RetFalse
CALLMACRO	RegMask teststreq,(M_Reax OR M_Recx OR M_Redi OR M_Resi)

CALLMACRO	INLINE_ROUTINE	teststrneq
	movl	Reax,Recx		;# Are either just single chars?
	orl		Rebx,Reax
	andl	CONST TAG,Reax
	jz		teststrneq2
	cmpl	Rebx,Recx		;# Must be identical
	jz		RetFalse
	jmp		RetTrue
teststrneq2:
	movl	Recx,Reax
	movl	Rebx,Redi		;# Move ready for cmpsb.
	movl	[Reax],Recx		;# Get length
	addl	CONST 4,Recx	;# add 4 for the length field.
	movl	Reax,Resi		;# Move to correct reg for cmpsb
	cld						;# Make sure we increment
	cmpl	Reax,Reax		;# Set the Zero bit
;# Compare the strings.  Because the length field is at the beginning
;# it does not matter if the value we loaded into %Recx is wrong
IFDEF WINDOWS
	repe	cmpsb
ELSE
	repe
	cmpsb
ENDIF
	movl	Reax,Resi		;# Make these valid
	movl	Reax,Recx
	movl	Reax,Redi
	jz		RetFalse
	jmp		RetTrue
CALLMACRO	RegMask teststrneq,(M_Reax OR M_Recx OR M_Redi OR M_Resi)


 ;# Temporary store for single character strings.
IFDEF WINDOWS
.data
strbuff1	dd	1
		dd	0
strbuff2	dd	1
		dd	0

.code
ELSE
.data
strbuff1:	.long	1
		.long	0
strbuff2:	.long	1
		.long	0

.text
ENDIF

;# General test routine.  Returns with the condition codes set
;# appropriately.

teststr:
	testl	CONST TAG,Reax
	jz		tststr1
	shrl	CONST TAGSHIFT,Reax
IFDEF WINDOWS
	mov		byte ptr	strbuff1+4,al
ELSE
	movb	%al,strbuff1+4
ENDIF
	leal	strbuff1,Reax
tststr1:
	testl	CONST TAG,Rebx
	jz		tststr2
	shrl	CONST TAGSHIFT,Rebx
IFDEF WINDOWS
	mov		byte ptr	strbuff2+4,bl
ELSE
	movb	%bl,strbuff2+4
ENDIF
	leal	strbuff2,Rebx
tststr2:
	movl	[Reax],Redi		;# Get length.
	movl	[Rebx],Recx		;# 
	cmpl	Recx,Redi		;# Find shorter length
	jge		tststr3
	movl	Redi,Recx
tststr3:
	leal	4[Reax],Resi	;# Load ptrs for cmpsb
	leal	4[Rebx],Redi
	cld					;# Make sure we increment
	cmpl	Reax,Reax		;# Set the Zero bit
IFDEF WINDOWS
	repe cmpsb			;# Compare while equal and Recx > 0
ELSE
	repe	
	cmpsb			;# Compare while equal and %ecx > 0
ENDIF
	jnz		tststr4
 ;# Strings are equal as far as the shorter of the two.  Have to compare
 ;# the lengths.
	movl	[Reax],Redi
	cmpl	[Rebx],Redi
tststr4:
	movl	Reax,Recx		;# Clobber these
	movl	Reax,Resi
	movl	Reax,Redi
	ret

 ;# These functions compare strings for lexical ordering.  This version, at
 ;# any rate, assumes that they are UNSIGNED bytes.

CALLMACRO	INLINE_ROUTINE	str_compare
	call	teststr
	ja		RetTrue			;# Return TAGGED(1) if it's greater
	je		RetFalse		;# Return TAGGED(0) if it's equal
	movl	CONST MINUS1,Reax	;# Return TAGGED(-1) if it's less.
	ret


CALLMACRO	INLINE_ROUTINE	teststrgeq
	call	teststr
	jnb		RetTrue
	jmp		RetFalse
CALLMACRO	RegMask teststrgeq,(M_Reax OR M_Recx OR M_Redi OR M_Resi)

CALLMACRO	INLINE_ROUTINE	teststrleq
	call	teststr
	jna		RetTrue
	jmp		RetFalse
CALLMACRO	RegMask teststrleq,(M_Reax OR M_Recx OR M_Redi OR M_Resi)

CALLMACRO	INLINE_ROUTINE	teststrlss
	call	teststr
	jb		RetTrue
	jmp		RetFalse
CALLMACRO	RegMask teststrlss,(M_Reax OR M_Recx OR M_Redi OR M_Resi)

CALLMACRO	INLINE_ROUTINE	teststrgtr
	call	teststr
	ja		RetTrue
	jmp		RetFalse
CALLMACRO	RegMask teststrgtr,(M_Reax OR M_Recx OR M_Redi OR M_Resi)

CALLMACRO	INLINE_ROUTINE	is_big_endian
	jmp		RetFalse	;# I386/486 is little-endian
CALLMACRO	RegMask is_big_endian,(M_Reax OR M_Recx)

CALLMACRO	INLINE_ROUTINE	bytes_per_word
	movl	CONST TAGGED(4),Reax  ;# 4 bytes per word
	ret
CALLMACRO	RegMask bytes_per_word,(M_Reax OR M_Recx)

 ;# Word functions.  These are all unsigned and do not raise Overflow
 
CALLMACRO	INLINE_ROUTINE	mul_word
	shrl	CONST TAGSHIFT,Rebx	;# Untag the multiplier
	subl	CONST TAG,Reax		;# Remove the tag from the multiplicand
	mull	Rebx				;# unsigned multiplication
	addl	CONST TAG,Reax		;# Add back the tag, but don`t shift
	movl	Reax,Redx			;# clobber this which has the high-end result
	movl	Reax,Rebx			;# and the other bad result.
	ret
CALLMACRO	RegMask mul_word,(M_Reax OR M_Recx OR M_Rebx OR M_Redx)

CALLMACRO	INLINE_ROUTINE	plus_word
	leal	(-TAG)[Reax+Rebx],Reax	;# Add the values and subtract a tag
	ret
CALLMACRO	RegMask plus_word,(M_Reax OR M_Recx)

CALLMACRO	INLINE_ROUTINE	minus_word
	subl	Rebx,Reax
	addl	CONST TAG,Reax			;# Put back the tag
	ret
CALLMACRO	RegMask minus_word,(M_Reax OR M_Recx)

CALLMACRO	INLINE_ROUTINE	div_word
	movl	Reax,Redi
	shrl	CONST TAGSHIFT,Reax
	shrl	CONST TAGSHIFT,Rebx
	movl	CONST 0,Redx
	div		Rebx
CALLMACRO	MAKETAGGED	Reax,Reax
	movl	Reax,Redx
	movl	Reax,Rebx
	ret
CALLMACRO	RegMask div_word,(M_Reax OR M_Recx OR M_Rebx OR M_Redx)

CALLMACRO	INLINE_ROUTINE	mod_word
	movl	Reax,Redi
	shrl	CONST TAGSHIFT,Reax
	shrl	CONST TAGSHIFT,Rebx
	movl	CONST 0,Redx
	div		Rebx
CALLMACRO	MAKETAGGED	Redx,Reax
	movl	Reax,Redx
	movl	Reax,Rebx
	ret
CALLMACRO	RegMask mod_word,(M_Reax OR M_Recx OR M_Rebx OR M_Redx)

CALLMACRO	INLINE_ROUTINE	word_eq
	cmpl	Rebx,Reax
	jz		RetTrue			;# True if they are equal.
	jmp		RetFalse
CALLMACRO	RegMask word_eq,(M_Reax OR M_Recx)

CALLMACRO	INLINE_ROUTINE	word_neq
	cmpl	Rebx,Reax
	jz		RetFalse
	jmp		RetTrue
CALLMACRO	RegMask word_neq,(M_Reax OR M_Recx)

CALLMACRO	INLINE_ROUTINE	word_geq
	cmpl	Rebx,Reax
	jnb		RetTrue
	jmp		RetFalse
CALLMACRO	RegMask word_geq,(M_Reax OR M_Recx)

CALLMACRO	INLINE_ROUTINE	word_leq
	cmpl	Rebx,Reax
	jna		RetTrue
	jmp		RetFalse
CALLMACRO	RegMask word_leq,(M_Reax OR M_Recx)

CALLMACRO	INLINE_ROUTINE	word_gtr
	cmpl	Rebx,Reax
	ja		RetTrue
	jmp		RetFalse
CALLMACRO	RegMask word_gtr,(M_Reax OR M_Recx)

CALLMACRO	INLINE_ROUTINE	word_lss
	cmpl	Rebx,Reax
	jb		RetTrue
	jmp		RetFalse
CALLMACRO	RegMask word_lss,(M_Reax OR M_Recx)

CALLMACRO	INLINE_ROUTINE	set_code_constanta
;# Store a constant in the code of a function.  Exactly how this
;# is done depends on the architecture.  It is crucial that we
;# don't get a garbage collection part way through this operation
;# which is why it's done in the RTS.
;# On this architecture we just store the constant directly into
;# the code.  Note that the offset, which is a BYTE offset, will
;# generally not be a multiple of 4 so the address will be unaligned.
	movl	8[Resp],Recx				;# Value to store
	shrl	CONST TAGSHIFT,Rebx			;# Remove tag from offset
	addl	Reax,Rebx					;# Compute address
IFDEF WINDOWS
	cmpl	CONST TAGGED(1),dword ptr 4[Resp] ;# Is it a relative branch?
ELSE
	cmpl	CONST TAGGED(1),4[Resp]	;# Is it a relative branch?
ENDIF
	jne		scc1
	subl	Rebx,Recx					;# Yes - Subtract the address
	subl	CONST 4,Recx				;# and make it relative to the end
scc1:
	movl 	Recx,[Rebx]					;# Store value.
	movl	CONST UNIT,Reax				;# The function returns unit
	movl	Reax,Rebx			        ;# Clobber bad value in Rebx
	movl	Reax,Recx			        ;# and possible bad value in Recx.
	ret		CONST 8
CALLMACRO	RegMask set_code_constant,(M_Reax OR M_Rebx OR M_Recx)

;# Register mask vector. - extern int registerMaskVector[];
;# Each entry in this vector is a set of the registers modified
;# by the function.  It is an untagged bitmap with the registers
;# encoded in the same way as the 
IFDEF WINDOWS
	align	4
	PUBLIC	registerMaskVector
registerMaskVector	dd	Mask_all				;# 0 is unused
ELSE
		.global	EXTNAME(registerMaskVector)
EXTNAME(registerMaskVector):
#define	dd	.long
	dd	Mask_all				;# 0 is unused
ENDIF
	dd	Mask_finish				;# 1
	dd	Mask_install_root       ;# 2
	dd	Mask_all				;# 3 is unused
	dd	Mask_all				;# 4 is unused
	dd	Mask_all				;# 5 is unused
	dd	Mask_strconcat          ;# 6
	dd	Mask_all				;# 7 is unused
	dd	Mask_all				;# 8 is unused
	dd	Mask_change_dir         ;# 9
	dd	Mask_all				;# 10 is unused
	dd	Mask_alloc_store         ;# 11
	dd	Mask_substring           ;# 12
	dd	Mask_all				 ;# return = 13
	dd	Mask_all				 ;# raisex = 14
	dd	Mask_get_length          ;# 15
	dd	Mask_all				;# 16 is unused
	dd	Mask_get_flags_          ;# 17
	dd	Mask_all				;# 18 is no longer used
	dd	Mask_all	            ;# 19 is no longer used
	dd	Mask_all			    ;# 20 is no longer used
	dd	Mask_all				;# 21 is unused
	dd	Mask_all				;# 22 is unused
	dd	Mask_all				;# 23 is unused
	dd	Mask_teststreq           ;# 24
	dd	Mask_teststrneq          ;# 25
	dd	Mask_teststrgtr          ;# 26
	dd	Mask_teststrlss          ;# 27
	dd	Mask_teststrgeq          ;# 28
	dd	Mask_teststrleq          ;# 29
	dd	Mask_exception_trace     ;# 30
	dd	Mask_all		         ;# 31 is no longer used
	dd	Mask_all			     ;# 32 is no longer used
	dd	Mask_all		         ;# 33 is no longer used
	dd	Mask_all		         ;# 34 is no longer used
	dd	Mask_all		         ;# 35 is no longer used
	dd	Mask_all		         ;# 36 is no longer used
	dd	Mask_all				 ;# 37 is unused
	dd	Mask_all				 ;# 38 is unused
	dd	Mask_all				 ;# 39 is unused
	dd	Mask_commit              ;# 40
	dd	Mask_all				 ;# 41 is unused
	dd	Mask_set_dbentry_        ;# 42
	dd	Mask_get_dbentry         ;# 43
	dd	Mask_all		         ;# 44 is no longer used
	dd	Mask_all		         ;# 45 is no longer used
	dd	Mask_createf             ;# 46
	dd	Mask_lockseg             ;# 47
	dd	Mask_all				 ;# nullorzero = 48
	dd	Mask_all	             ;# 49 is no longer used
	dd	Mask_all	             ;# 50 is no longer used
	dd	Mask_Net_dispatch_		 ;# 51
	dd	Mask_OS_spec_dispatch_	 ;# 52
	dd	Mask_all				;# 53 is unused
	dd	Mask_all				;# 54 is unused
	dd	Mask_all				;# version_number = 55
	dd	Mask_all				;# 56 is unused
	dd	Mask_all				;# 57 is unused
	dd	Mask_all				;# 58 is unused
	dd	Mask_all				;# 59 is unused
	dd	Mask_all				;# 60 is unused
	dd	Mask_IO_dispatch_		 ;# 61
	dd	Mask_Sig_dispatch_		 ;# 62
	dd	Mask_all				;# 63 is unused
	dd	Mask_all				;# 64 is unused
	dd	Mask_all				;# 65 is unused
	dd	Mask_all				;# 66 is unused
	dd	Mask_all				;# 67 is unused
	dd	Mask_all				;# 68 is unused
	dd	Mask_all				;# 69 is unused
	dd	Mask_all				;# 70 is unused
	dd	Mask_all				;# 71 is unused
	dd	Mask_all				;# 72 is unused
	dd	Mask_all				;# 73 is unused
	dd	Mask_all				;# 74 is unused
	dd	Mask_all				;# 75 is unused
	dd	Mask_all				;# 76 is unused
	dd	Mask_all				;# 77 is unused
	dd	Mask_all				;# 78 is unused
	dd	Mask_all				;# 79 is unused
	dd	Mask_all				;# Mask_version_number_1 = 80
	dd	Mask_all		         ;# 81 is now unused
	dd	Mask_fork_process        ;# 82
	dd	Mask_choice_process      ;# 83
	dd	Mask_kill_self           ;# 84
	dd	Mask_int_process         ;# 85
	dd	Mask_send_on_channel     ;# 86
	dd	Mask_receive_on_channel  ;# 87
	dd	Mask_profiler            ;# 88
	dd	Mask_all				;# 89 is unused
	dd	Mask_all				;# 90 is unused
	dd	Mask_all				;# 91 is unused
	dd	Mask_full_gc_            ;# 92
	dd	Mask_stack_trace_        ;# 93
	dd	Mask_timing_dispatch_	 ;# 94
	dd	Mask_all				;# 95 is unused
	dd	Mask_all				;# 96 is unused
	dd	Mask_all				;# 97 is unused
	dd	Mask_get_dbasetime_      ;# 98
	dd	Mask_objsize_            ;# 99
	dd	Mask_showsize_           ;# 100
	dd	Mask_all				;# 101 is unused
	dd	Mask_all				;# 102 is unused
	dd	Mask_interrupt_console_processes_ ;# 103
	dd	Mask_all				;# 104 is unused
	dd	Mask_is_short            ;# 105
	dd	Mask_aplus               ;# 106
	dd	Mask_aminus              ;# 107
	dd	Mask_amul                ;# 108
	dd	Mask_adiv                ;# 109
	dd	Mask_amod                ;# 110
	dd	Mask_aneg                ;# 111
	dd	Mask_xora				 ;# 112
	dd	Mask_equala              ;# 113
	dd	Mask_ora				 ;# 114
	dd	Mask_anda				 ;# 115
	dd	Mask_all				 ;# version_number_3 = 116
	dd	Mask_Real_str			 ;# 117
	dd	Mask_Real_geq            ;# 118
	dd	Mask_Real_leq            ;# 119
	dd	Mask_Real_gtr            ;# 120
	dd	Mask_Real_lss            ;# 121
	dd	Mask_Real_eq             ;# 122
	dd	Mask_Real_neq            ;# 123
	dd	Mask_Real_dispatch		 ;# 124
	dd	Mask_Real_add            ;# 125
	dd	Mask_Real_sub            ;# 126
	dd	Mask_Real_mul            ;# 127
	dd	Mask_Real_div            ;# 128
	dd	Mask_all				 ;# 129 is unused
	dd	Mask_Real_neg            ;# 130
	dd	Mask_all				 ;# 131 is unused
	dd	Mask_Real_repr           ;# 132
	dd	Mask_Real_conv           ;# 133
	dd	Mask_Real_int            ;# 134
	dd	Mask_Real_float          ;# 135
	dd	Mask_Real_sqrt           ;# 136
	dd	Mask_Real_sin            ;# 137
	dd	Mask_Real_cos            ;# 138
	dd	Mask_Real_arctan         ;# 139
	dd	Mask_Real_exp            ;# 140
	dd	Mask_Real_ln             ;# 141
	dd	Mask_all		         ;# 142 is no longer used
	dd	Mask_all				 ;# 143 is unused
	dd	Mask_all				 ;# 144 is unused
	dd	Mask_all				 ;# 145 is unused
	dd	Mask_all				 ;# 146 is unused
	dd	Mask_all				 ;# 147 is unused
	dd	Mask_all				 ;# stdin = 148
	dd	Mask_all				 ;# stdout= 149
	dd	Mask_process_env_dispatch_	 ;# 150
	dd	Mask_set_string_length	 ;# 151
	dd	Mask_get_first_long_word ;# 152
	dd	Mask_all				 ;# 153 is unused
	dd	Mask_all				 ;# 154 is unused
	dd	Mask_all				 ;# 155 is unused
	dd	Mask_all				 ;# 156 is unused
	dd	Mask_all				 ;# 157 is unused
	dd	Mask_all				 ;# 158 is unused
	dd	Mask_all				 ;# 159 is unused
	dd	Mask_all				 ;# 160 is unused
	dd	Mask_all				 ;# 161 is unused
	dd	Mask_all				 ;# 162 is unused
	dd	Mask_all				 ;# 163 is unused
	dd	Mask_all				 ;# 164 is unused
	dd	Mask_all				 ;# 165 is unused
	dd	Mask_all				 ;# 166 is unused
	dd	Mask_all				 ;# 167 is unused
	dd	Mask_all				 ;# 168 is unused
	dd	Mask_all				 ;# 169 is unused
	dd	Mask_all				 ;# 170 is unused
	dd	Mask_all				 ;# 171 is unused
	dd	Mask_all				 ;# 172 is unused
	dd	Mask_all				 ;# 173 is unused
	dd	Mask_all				 ;# 174 is unused
	dd	Mask_all				 ;# 175 is unused
	dd	Mask_all				 ;# 176 is unused
	dd	Mask_all				 ;# 177 is unused
	dd	Mask_all				 ;# 178 is unused
	dd	Mask_all				 ;# 179 is unused
	dd	Mask_all				 ;# 180 is unused
	dd	Mask_all				 ;# 181 is unused
	dd	Mask_all				 ;# 182 is unused
	dd	Mask_all				 ;# 183 is unused
	dd	Mask_all				 ;# 184 is unused
	dd	Mask_all				 ;# 185 is unused
	dd	Mask_all				 ;# 186 is unused
	dd	Mask_all				 ;# 187 is unused
	dd	Mask_all				 ;# 188 is unused
	dd	Mask_io_operation        ;# 189
	dd	Mask_all				 ;# 190 is unused
	dd	Mask_all	             ;# 191 is no longer used
	dd	Mask_all				 ;# 192 is unused
	dd	Mask_all				 ;# 193 is unused
	dd	Mask_set_code_constant	 ;# 194
	dd	Mask_move_words			 ;# 195
	dd	Mask_shift_right_arith_word	 ;# 196
	dd	Mask_int_to_word		 ;# 197
	dd	Mask_move_bytes			 ;# 198
 	dd	Mask_all				 ;# 199 now unused
	dd	Mask_set_flags_          ;# 200
	dd	Mask_shrink_stack_       ;# 201
	dd	Mask_all				 ;# stderr = 202
 	dd	Mask_all				 ;# 203 now unused
	dd	Mask_callcode_tupled     ;# 204
	dd	Mask_foreign_dispatch_   ;# 205
	dd	Mask_install_subshells_  ;# 206
	dd	Mask_all				 ;# 207 is unused
	dd	Mask_all				 ;# 208 now unused
	dd	Mask_XWindows_           ;# 209
	dd	Mask_all				 ;# 210 is unused
	dd	Mask_all				 ;# 211 is unused
	dd	Mask_all				 ;# 212 is unused
	dd	Mask_is_big_endian       ;# 213
	dd	Mask_bytes_per_word      ;# 214
	dd	Mask_offset_address      ;# 215
	dd	Mask_shift_right_word    ;# 216
	dd	Mask_word_neq            ;# 217
	dd	Mask_not_bool            ;# 218
	dd	Mask_all				 ;# 219 is unused
	dd	Mask_all				 ;# 220 is unused
	dd	Mask_all				 ;# 221 is unused
	dd	Mask_all				 ;# 222 is unused
	dd	Mask_string_length       ;# 223
	dd	Mask_all				 ;# 224 is unused
	dd	Mask_all				 ;# 225 is unused
	dd	Mask_all				 ;# 226 is unused
	dd	Mask_all				 ;# 227 is unused
	dd	Mask_all				 ;# 228 is unused
	dd	Mask_int_eq              ;# 229
	dd	Mask_int_neq             ;# 230
	dd	Mask_int_geq             ;# 231
	dd	Mask_int_leq             ;# 232
	dd	Mask_int_gtr             ;# 233
	dd	Mask_int_lss             ;# 234
	dd	Mask_string_sub          ;# 235
	dd	Mask_all				 ;# 236 is unused
	dd	Mask_all				 ;# 237 is unused
	dd	Mask_mul_word            ;# 238
	dd	Mask_plus_word           ;# 239
	dd	Mask_minus_word          ;# 240
	dd	Mask_div_word            ;# 241
	dd	Mask_or_word             ;# 242
	dd	Mask_and_word            ;# 243
	dd	Mask_xor_word            ;# 244
	dd	Mask_shift_left_word     ;# 245
	dd	Mask_mod_word            ;# 246
	dd	Mask_word_geq            ;# 247
	dd	Mask_word_leq            ;# 248
	dd	Mask_word_gtr            ;# 249
	dd	Mask_word_lss            ;# 250
	dd	Mask_word_eq             ;# 251
	dd	Mask_load_byte           ;# 252
	dd	Mask_load_word           ;# 253
	dd	Mask_assign_byte         ;# 254
	dd	Mask_assign_word         ;# 255


END
