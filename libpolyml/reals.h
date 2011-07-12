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
class TaskData;

extern double real_arg(Handle x); // Also used in "foreign.cpp"
extern Handle real_result(TaskData *mdTaskData, double x); // Also used in "foreign.cpp"

extern Handle Real_addc (TaskData *mdTaskData, Handle, Handle);
extern Handle Real_subc (TaskData *mdTaskData, Handle, Handle);
extern Handle Real_mulc (TaskData *mdTaskData, Handle, Handle);
extern Handle Real_divc (TaskData *mdTaskData, Handle, Handle);
extern Handle Real_negc (TaskData *mdTaskData, Handle);
extern Handle Real_convc (TaskData *mdTaskData, Handle);
extern Handle Real_intc (TaskData *mdTaskData, Handle);
extern Handle Real_floatc (TaskData *mdTaskData, Handle);
extern Handle Real_sqrtc (TaskData *mdTaskData, Handle);
extern Handle Real_sinc (TaskData *mdTaskData, Handle);
extern Handle Real_cosc (TaskData *mdTaskData, Handle);
extern Handle Real_arctanc (TaskData *mdTaskData, Handle);
extern Handle Real_expc (TaskData *mdTaskData, Handle);
extern Handle Real_lnc (TaskData *mdTaskData, Handle);
extern Handle Real_reprc (TaskData *mdTaskData, Handle);
extern Handle Real_strc(TaskData *mdTaskData, Handle hDigits, Handle hMode, Handle arg);
extern Handle Real_geqc(TaskData *mdTaskData, Handle y, Handle x);
extern Handle Real_leqc(TaskData *mdTaskData, Handle y, Handle x);
extern Handle Real_gtrc(TaskData *mdTaskData, Handle y, Handle x);
extern Handle Real_lssc(TaskData *mdTaskData, Handle y, Handle x);
extern Handle Real_eqc(TaskData *mdTaskData, Handle y, Handle x);
extern Handle Real_neqc(TaskData *mdTaskData, Handle y, Handle x);

extern Handle Real_dispatchc(TaskData *mdTaskData, Handle args, Handle code);

#endif
