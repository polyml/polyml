/*
    Title:  polystring.h - String functions and types

    Copyright (c) 2006 David C.J. Matthews

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

#ifndef POLYSTRING_H
#define POLYSTRING_H

#include "globals.h" // For PolyObject

class SaveVecEntry;
typedef SaveVecEntry *Handle;

// A string object.  N.B. Poly strings can be either a single tagged integer or
// a pointer to this.
class PolyStringObject: public PolyObject {

public:
    POLYUNSIGNED length;
    char chars[1];
};

//typedef PolyStringObject STRING, *pstring;

extern PolyWord EmptyString(void);

/* PolyStringObject functions */
extern PolyWord Buffer_to_Poly(const char *buffer, unsigned length);
extern PolyWord C_string_to_Poly(const char *buffer);
extern POLYUNSIGNED Poly_string_to_C(PolyWord ps, char *buff, POLYUNSIGNED bufflen);
extern char *Poly_string_to_C_alloc(PolyWord ps);

#ifdef UNICODE

#ifdef HAVE_TCHAR_H
#include <tchar.h>
#else
#define WCHAR short
#endif

extern PolyWord C_string_to_Poly(const WCHAR *buffer);
extern POLYUNSIGNED Poly_string_to_C(PolyWord ps, WCHAR *buff, POLYUNSIGNED bufflen);
extern WCHAR *Poly_string_to_U_alloc(PolyWord ps);

// Poly_string_to_T_alloc returns a Unicode string in Unicode and char string otherwise.
#define Poly_string_to_T_alloc	Poly_string_to_U_alloc
#else
#define Poly_string_to_T_alloc	Poly_string_to_C_alloc
#endif

Handle convert_string_list(int count, char **strings);
extern char **stringListToVector(Handle list);
extern void freeStringVector(char **vec);
extern void print_string(PolyWord s);

// These should no longer be used in the RTS except internally.
// They are currently used by the ML code during bootstrapping.
extern Handle strconcatc(Handle x, Handle y);
extern Handle string_subc(Handle x, Handle y);
Handle string_length_c(Handle string);

#define DEREFSTRINGHANDLE(_x)    ((PolyStringObject *)(_x)->WordP())

extern Handle compareStrings(Handle y, Handle x);
extern Handle testStringEqual(Handle y, Handle x);
extern Handle testStringNotEqual(Handle y, Handle x);
extern Handle testStringGreater(Handle y, Handle x);
extern Handle testStringLess(Handle y, Handle x);
extern Handle testStringGreaterOrEqual(Handle y, Handle x);
extern Handle testStringLessOrEqual(Handle y, Handle x);



#endif /* POLYSTRING_H */
