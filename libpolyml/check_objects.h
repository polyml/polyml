/*
    Title:  check_objects.h - Validate addresses in objects.

    Copyright (c) 2006, 2012
        David C. J. Matthews

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

#ifndef CHECK_OBJECTS_INCLUDED
#define CHECK_OBJECTS_INCLUDED

#include "globals.h"
#include "diagnostics.h" // For userOptions


extern void DoCheck (const PolyWord pt);
extern void DoCheckPointer (const PolyWord pt);
extern void DoCheckObject (const PolyObject *base, POLYUNSIGNED lengthWord);
extern void DoCheckMemory(void);

#define Check(pt)        {if (debugOptions & DEBUG_CHECK_OBJECTS) DoCheck(pt); }
#define CheckObject(pt)  {if (debugOptions & DEBUG_CHECK_OBJECTS) DoCheckObject(pt, (pt)->LengthWord()); }
#define CheckObjectL(pt, l)  {if (debugOptions & DEBUG_CHECK_OBJECTS) DoCheckObject(pt, l); }
#define CheckPointer(pt) {if (debugOptions & DEBUG_CHECK_OBJECTS) DoCheckPointer(pt); }
#define CheckMemory() { if (debugOptions & DEBUG_CHECK_OBJECTS) DoCheckMemory(); }

#endif
