/*
    Title:  reals.h

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

class SaveVecEntry;
typedef SaveVecEntry *Handle;

extern double real_arg(Handle x);
extern Handle real_result(double x);

extern Handle Real_addc (Handle, Handle);
extern Handle Real_subc (Handle, Handle);
extern Handle Real_mulc (Handle, Handle);
extern Handle Real_divc (Handle, Handle);
extern Handle Real_negc (Handle);
extern Handle Real_convc (Handle);
extern Handle Real_intc (Handle);
extern Handle Real_floatc (Handle);
extern Handle Real_sqrtc (Handle);
extern Handle Real_sinc (Handle);
extern Handle Real_cosc (Handle);
extern Handle Real_arctanc (Handle);
extern Handle Real_expc (Handle);
extern Handle Real_lnc (Handle);
extern Handle Real_reprc (Handle);
extern Handle Real_strc(Handle hDigits, Handle hMode, Handle arg);
extern Handle Real_geqc(Handle y, Handle x);
extern Handle Real_leqc(Handle y, Handle x);
extern Handle Real_gtrc(Handle y, Handle x);
extern Handle Real_lssc(Handle y, Handle x);
extern Handle Real_eqc(Handle y, Handle x);
extern Handle Real_neqc(Handle y, Handle x);

extern Handle Real_dispatchc(Handle args, Handle code);

#endif
