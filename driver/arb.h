/*
    Title: 	arb.h - exports signature for arb.c

	Copyright (c) 2000
		Cambridge University Technical Services Limited

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

*/

/* created 26/10/93 SPF */

extern Handle Make_arbitrary_precision(int);
extern Handle Make_unsigned(unsigned);
extern Handle Make_arb_from_pair(unsigned hi, unsigned lo);
extern Handle Make_arb_from_pair_scaled(unsigned hi, unsigned lo, unsigned scale);
extern void get_C_pair(word *number, unsigned int *pHi, unsigned int *pLo);

extern Handle add_longc(Handle,Handle);
extern Handle sub_longc(Handle,Handle);
extern Handle mult_longc(Handle,Handle);/* CALL_IO2(mult_long, REF, REF, IND) */
extern Handle div_longc(Handle,Handle); /* CALL_IO2(div_long, REF, REF, IND)  */
extern Handle rem_longc(Handle,Handle); /* CALL_IO2(rem_long, REF, REF, IND)  */
extern Handle or_longc(Handle,Handle); /* CALL_IO2(or_long, REF, REF, IND)  */
extern Handle and_longc(Handle,Handle); /* CALL_IO2(and_long, REF, REF, IND)  */
extern Handle xor_longc(Handle,Handle); /* CALL_IO2(xor_long, REF, REF, IND)  */
extern Handle neg_longc(Handle);


/* indirection removed 31/10/93 SPF */
extern unsigned get_C_ulong(word *);
extern unsigned short get_C_ushort(word *);
extern int      get_C_long(word *);
extern short    get_C_short(word *);
extern word     comp_longc(Handle,Handle);


extern Handle vec_length_c(Handle);

/* Not used in SPARC version */
#if !defined(SPARC)
extern word equal_longc(Handle,Handle);
extern word gt_longc(Handle,Handle);
extern word ls_longc(Handle,Handle);
extern word ge_longc(Handle,Handle);
extern word le_longc(Handle,Handle);

extern Handle string_length_c(Handle);
extern word   load_byte_long_c(Handle,Handle);
extern Handle load_word_long_c(Handle,Handle);
extern word int_to_word_c(Handle x);
#endif

extern word *alloc_store_long_c(Handle, Handle, Handle);
