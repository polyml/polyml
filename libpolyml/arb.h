/*
    Title:  arb.h - exports signature for arb.c

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

#ifndef ARB_H_INCLUDED
#define ARB_H_INCLUDED

class SaveVecEntry;
typedef SaveVecEntry *Handle;

extern Handle Make_arbitrary_precision(POLYSIGNED);
extern Handle Make_unsigned(POLYUNSIGNED);
extern Handle Make_arb_from_pair(unsigned hi, unsigned lo);
extern Handle Make_arb_from_pair_scaled(unsigned hi, unsigned lo, unsigned scale);
void get_C_pair(PolyWord number, unsigned long *pHi, unsigned long *pLo);

extern Handle add_longc(Handle,Handle);
extern Handle sub_longc(Handle,Handle);
extern Handle mult_longc(Handle,Handle);
extern Handle div_longc(Handle,Handle);
extern Handle rem_longc(Handle,Handle);
extern Handle or_longc(Handle,Handle);
extern Handle and_longc(Handle,Handle);
extern Handle xor_longc(Handle,Handle);
extern Handle neg_longc(Handle);

extern Handle equal_longc(Handle y, Handle x);
extern Handle not_equal_longc(Handle y, Handle x);
extern Handle gt_longc(Handle y, Handle x);
extern Handle ls_longc(Handle y, Handle x);
extern Handle ge_longc(Handle y, Handle x);
extern Handle le_longc(Handle y, Handle x);
extern Handle int_to_word_c(Handle x);



extern POLYUNSIGNED     get_C_ulong(PolyWord);
extern unsigned short   get_C_ushort(PolyWord);
extern POLYSIGNED       get_C_long(PolyWord);
extern short            get_C_short(PolyWord);
extern int              compareLong(Handle,Handle);


#endif
