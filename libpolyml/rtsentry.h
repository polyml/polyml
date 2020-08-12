/*
    Title:  rtsentry.h - Entry points to the run-time system

    Copyright (c) 2016 David C. J. Matthews

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

*/

#ifndef RTSENTRY_H_INCLUDED
#define RTSENTRY_H_INCLUDED
class SaveVecEntry;
class TaskData;
class PolyObject;

typedef SaveVecEntry *Handle;

extern Handle creatEntryPointObject(TaskData *taskData, Handle entryH, bool isFuncPtr);
extern const char *getEntryPointName(PolyObject *p, bool *isFuncPtr);
extern bool setEntryPoint(PolyObject *p);

typedef void (*polyRTSFunction)();

typedef struct _entrypts {
    const char *name;
    polyRTSFunction entry;
} *entrypts;

// Ensure that the RTS calls can be found by the linker.
#ifndef POLYEXTERNALSYMBOL
#ifdef _MSC_VER
#define POLYEXTERNALSYMBOL __declspec(dllexport)
#else
#define POLYEXTERNALSYMBOL
#endif
#endif

#ifdef POLYML32IN64
// This is needed for legacy only.  RTS calls previously passed the
// real address of the thread ID.  They now pass a PolyWord containing
// the thread ID.
// Once we've fully bootstrapped FirstArgument can be replaced with PolyWord.
union firstArgFull
{
    operator PolyWord()
    { if (value >= 0x100000000) return (PolyWord)((PolyObject*)value); else return PolyWord::FromUnsigned((POLYUNSIGNED)value); }
    uintptr_t value;
};
typedef union firstArgFull FirstArgument;
#else
typedef PolyWord  FirstArgument;
#endif

#endif
