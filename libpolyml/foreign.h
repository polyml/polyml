/*
    Title:  Header for foreign function interface

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

#ifndef _FOREIGN_H_DEFINED
#define _FOREIGN_H_DEFINED 1

class SaveVecEntry;
typedef SaveVecEntry *Handle;
class TaskData;

class ScanAddress;

extern Handle foreign_dispatch_c (TaskData *taskData, Handle args, Handle fcode_h);

extern void *CCallbackFunction(unsigned cbNo, void **args);

// This is needed when compiling a callback.
typedef enum {
  Cchar    = 1,
  Cdouble  = 2,
  Cfloat   = 3,
  Cint     = 4,
  Clong    = 5,
  Cpointer = 6,
  Cshort   = 7,
  Cstruct  = 8,  /* should never occur, since boxed value is untagged */
  Cuint    = 9
} Ctype;
  
#endif
