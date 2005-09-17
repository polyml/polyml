/*
    Title: 	reals.h

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

#ifndef _REALS_H

#define _REALS_H
#include "globals.h"

/* Real numbers are represented by the address of the value. */
#define DBLE (sizeof(double)/sizeof(word))
union db { double dble; word wrd[DBLE]; };


extern double real_arg(Handle x);
extern word *real_result(double x);

extern word *Real_addc (Handle, Handle);
extern word *Real_subc (Handle, Handle);
extern word *Real_mulc (Handle, Handle);
extern word *Real_divc (Handle, Handle);
extern word *Real_negc (Handle);
extern int Real_compc (Handle, Handle);
extern word *Real_convc (Handle);
extern word Real_intc (Handle);
extern word *Real_floatc (Handle);
extern word *Real_sqrtc (Handle);
extern word *Real_sinc (Handle);
extern word *Real_cosc (Handle);
extern word *Real_arctanc (Handle);
extern word *Real_expc (Handle);
extern word *Real_lnc (Handle);
extern pstring Real_reprc (Handle);
extern word *Real_strc(Handle hDigits, Handle hMode, Handle arg);
extern int Real_geqc(Handle y, Handle x);
extern int Real_leqc(Handle y, Handle x);
extern int Real_gtrc(Handle y, Handle x);
extern int Real_lssc(Handle y, Handle x);
extern int Real_eqc(Handle y, Handle x);
extern int Real_neqc(Handle y, Handle x);

extern word *Real_dispatchc(Handle args, Handle code);

extern void uninit_reals(void);
extern void init_reals(void);
extern void re_init_reals(void);


#endif
