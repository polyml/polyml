/*
    Title:  polystring.cpp - String functions and types

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

#ifdef HAVE_CONFIG_H
#include "config.h"
#elif defined(_WIN32)
#include "winconfig.h"
#else
#error "No configuration file"
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "globals.h"
#include "polystring.h"
#include "run_time.h"
#include "mpoly.h"
#include "sys.h"
#include "arb.h"
#include "save_vec.h"
#include "processes.h"

#define SAVE(x) mdTaskData->saveVec.push(x)
#define SIZEOF(x) (sizeof(x)/sizeof(PolyWord))

// Return empty string.
PolyWord EmptyString(void)
{
    return (PolyObject*)IoEntry(POLY_SYS_emptystring);
}

PolyWord Buffer_to_Poly(TaskData *mdTaskData, const char *buffer, size_t length) 
/* Returns a string as a Poly string. */
{
    /* Return the null string if it's empty. */
    if (length == 0) return EmptyString();
    
    /* Return the character itself if the length is 1 */
    if (length == 1) return TAGGED(((unsigned char *)buffer)[0]);
    
    /* Get the number of words required, plus 1 for length word,
       plus flag bit. */
    PolyStringObject *result = (PolyStringObject *)(alloc(mdTaskData, WORDS(length) + 1, F_BYTE_OBJ));
    
    /* Set length of string, then copy the characters. */
    result->length = length;
    memcpy(result->chars,buffer,length);
    /* We are relying on alloc zeroing any unused bytes in the
       allocated store.  That's essential for structure equality to
       work properly since it compares ALL bytes in a byte segment.
       (c.f. test*/
    return result;
} /* Buffer_to_Poly */


PolyWord C_string_to_Poly(TaskData *mdTaskData, const char *buffer)
/* Returns a C string as a Poly string. */
{
    if (buffer == NULL) return EmptyString();
    
    return Buffer_to_Poly(mdTaskData, buffer, strlen(buffer));
} /* C_string_to_Poly */

POLYUNSIGNED Poly_string_to_C(PolyWord ps, char *buff, POLYUNSIGNED bufflen)
/* Copies the characters from the string into the destination buffer.
   Returns original length of string. */
{
    if (IS_INT(ps))
    {
        buff[0] = (char)(UNTAGGED(ps));
        buff[1] = '\0';
        return(1);
    }
    PolyStringObject *str = (PolyStringObject *)ps.AsObjPtr();
    POLYUNSIGNED chars = str->length >= bufflen ? bufflen-1 : str->length;
    if (chars != 0) strncpy(buff, str->chars, chars);
    buff[chars] = '\0';
    return chars;
} /* Poly_string_to_C */

char *Poly_string_to_C_alloc(PolyWord ps)
/* Similar to Poly_string_to_C except that the string is allocated using
   malloc and must be freed by the caller. */
{
    char    *res;

    if (IS_INT(ps))
    {
        res = (char*)malloc(2);
        res[0] = (char)(UNTAGGED(ps));
        res[1] = '\0';
    }
    else
    {
        PolyStringObject * str = (PolyStringObject *)ps.AsObjPtr();
        POLYUNSIGNED chars = str->length;
        res = (char*)malloc(chars+1);
        if (chars != 0) strncpy(res, str->chars, chars);
        res[chars] = '\0';
    }
    return res;
} /* Poly_string_to_C_alloc */

#ifdef UNICODE

/* We need Unicode versions of these. */
PolyWord C_string_to_Poly(const WCHAR *buffer)
/* Returns a Unicode string as a Poly string. */
{
    if (buffer == NULL) return EmptyString();
    
    unsigned long length = wcslen(buffer);

    /* Return the null string if it's empty. */
    if (length == 0) return EmptyString();
    
    /* Return the character itself if the length is 1 */
    if (length == 1) return TAGGED(((unsigned char *)buffer)[0]);
    
    /* Get the number of words required, plus 1 for length word,
       plus flag bit. */
    PolyStringObject *result = (PolyStringObject *)(alloc(WORDS(length) + 1, F_BYTE_OBJ));
    
    /* Set length of string, then copy the characters. */
    result->length = length;
    for (unsigned long i = 0; i < length; i++) result->chars[i] = (char)buffer[i];

    return result;
} /* C_string_to_Poly */

POLYUNSIGNED Poly_string_to_C(PolyWord ps, WCHAR *buff, POLYUNSIGNED bufflen)
{
    if (IS_INT(ps))
    {
        buff[0] = (WCHAR)(UNTAGGED(ps));
        buff[1] = 0;
        return(1);
    }

    PolyStringObject *str = (PolyStringObject *)ps.AsObjPtr();
    POLYUNSIGNED chars = str->length >= bufflen ? bufflen-1 : str->length;
    for (POLYUNSIGNED i = 0; i < chars; i++) buff[i] = str->chars[i];
    buff[chars] = 0;
    return chars;
} /* Poly_string_to_C */

WCHAR *Poly_string_to_U_alloc(PolyWord ps)
{
    if (IS_INT(ps))
    {
        WCHAR *res = (WCHAR*)malloc(2 * sizeof(WCHAR));
        res[0] = (WCHAR)(UNTAGGED(ps));
        res[1] = 0;
        return res;
    }
    else
    {
        PolyStringObject *str = (PolyStringObject *)ps.AsObjPtr();
        POLYUNSIGNED chars = str->length;
        WCHAR * res = (WCHAR*)malloc((chars+1) * sizeof(WCHAR));
        for (POLYUNSIGNED i = 0; i < chars; i++) res[i] = str->chars[i];
        res[chars] = 0;
        return res;
    }
} /* Poly_string_to_U_alloc */

#endif

/* convert_string_list return a list of strings. */
Handle convert_string_list(TaskData *mdTaskData, int count, char **strings)
{
    Handle saved = mdTaskData->saveVec.mark();
    Handle list  = SAVE(ListNull);
    
    /* It's simplest to process the strings in reverse order */
    for (int i = count - 1; 0 <= i; i--)
    {
        /* 
        The order of these declarations is important, becaue we don't
        want to have make to make the cons cell mutable. This is only
        safe if we promise to initialise it fully before the next
        ML heap allocation. SPF 29/11/96
        */
        Handle value = SAVE(C_string_to_Poly(mdTaskData, strings[i]));
        Handle next  = alloc_and_save(mdTaskData, SIZEOF(ML_Cons_Cell));
        
        DEREFLISTHANDLE(next)->h = DEREFWORDHANDLE(value); 
        DEREFLISTHANDLE(next)->t = DEREFLISTHANDLE(list);
        
        /* reset save vector to stop it overflowing */    
        mdTaskData->saveVec.reset(saved);
        list = SAVE(DEREFHANDLE(next));
    }
    
    return list;
}

/* Convert a string list to a vector of C strings. */
char **stringListToVector(Handle list)
{
    int len = 0;
    /* Find the length of the list */
    for (PolyWord p = DEREFHANDLE(list); p != ListNull; p = ((ML_Cons_Cell*)p.AsObjPtr())->t) len++;
    /* Allocate vector. */
    char **vec = (char**)calloc(len+1, sizeof(char*));
    /* Copy the strings and put them into the vector. */
    len = 0;

    PolyWord q = DEREFHANDLE(list);
    while (q != ListNull) {
        ML_Cons_Cell *cell = (ML_Cons_Cell*)q.AsObjPtr();
        vec[len++] = Poly_string_to_C_alloc(cell->h);
        q = cell->t;
    }
    return vec;
}

/* Free the memory used by the vector. */
void freeStringVector(char **vec)
{
    char **p;
    if (vec == 0) return;
    for (p = vec; *p != 0; p++) free(*p);
    free(vec);
}

// Concatenate two strings.  Now used only internally in the RTS.
Handle strconcatc(TaskData *mdTaskData, Handle y, Handle x)
/* Note: arguments are in the reverse order from Poly */
{
    Handle result;
    POLYUNSIGNED len, xlen, ylen;
    char *from_ptr, *to_ptr;
    
    if (IS_INT(DEREFWORD(x)))
        xlen = 1;
    else 
        xlen = DEREFSTRINGHANDLE(x)->length;
    
    /* Don't concatenate with null strings */
    if (xlen == 0) return y;
    
    if (IS_INT(DEREFWORD(y)))
        ylen = 1;
    else
        ylen = DEREFSTRINGHANDLE(y)->length;
    
    if (ylen == 0) return x;
    
    len = xlen + ylen;
    
    /* Get store for combined string. Include rounding up to next word and
    room for the length word and add in the flag. */
    result = alloc_and_save(mdTaskData, (len + sizeof(PolyWord)-1)/sizeof(PolyWord) + 1, F_BYTE_OBJ);
    
    DEREFSTRINGHANDLE(result)->length = len;
    
    /* Copy first string */
    to_ptr = DEREFSTRINGHANDLE(result)->chars;
    if (xlen == 1)
    {
        *to_ptr++ = (char)UNTAGGED(DEREFSTRINGHANDLE(x));
    }
    else
    {
        from_ptr = DEREFSTRINGHANDLE(x)->chars;
        while (xlen-- > 0) (*to_ptr++ = *from_ptr++);
    }
    
    
    /* Add on second */
    if (ylen == 1)
    {
        *to_ptr = (char)UNTAGGED(DEREFSTRINGHANDLE(y));
    }
    else
    {
        from_ptr = DEREFSTRINGHANDLE(y)->chars;
        while (ylen-- > 0) (*to_ptr++ = *from_ptr++);
    }
    
    return(result);
} /* strconcat */

void print_string(PolyWord s)
{
    if (IS_INT(s))
        putc((char)UNTAGGED(s), stdout);
    else {
        PolyStringObject * str = (PolyStringObject *)s.AsObjPtr();
        fwrite(str->chars, 1, str->length, stdout);
    }
}

Handle string_length_c(TaskData *mdTaskData, Handle string)    /* Length of a string */
{
    PolyWord str = string->Word();
    if (str.IsTagged()) // Short form
        return Make_arbitrary_precision(mdTaskData, 1);
    
    POLYUNSIGNED length = ((PolyStringObject *)str.AsObjPtr())->length;
    return Make_arbitrary_precision(mdTaskData, length);
}

static PolyStringObject s_test_x, s_test_y;

// These functions are used in the interpreter.  They are generally replaced by
// hand-coded versions in the assembly code section.
static int string_test(PolyWord x, PolyWord y) 
/* Returns -1, 0, +1 if the first string is less, equal to or greater than the
   second. These are addresses of the strings because calling
   fix_persistent_address could result in a garbage-collection which could
   move the other string. */
{
    POLYUNSIGNED i;
    PolyStringObject *xs, *ys;
    /* Deal with single characters. */
    if (IS_INT(x))
    {
        s_test_x.length = 1;
        s_test_x.chars[0] = (char)UNTAGGED(x);
        xs = &s_test_x;
    }
    else xs = (PolyStringObject*)x.AsObjPtr();

    if (IS_INT(y))
    {
        s_test_y.length = 1;
        s_test_y.chars[0] = (char)UNTAGGED(y);
        ys = &s_test_y;
    }
    else ys = (PolyStringObject*)y.AsObjPtr();
    /* Now do the comparison. */

    for(i = 0; i < xs->length && i < ys->length; i++)
    {
        if (xs->chars[i] != ys->chars[i])
            return xs->chars[i] < ys->chars[i] ? -1 : 1;
    }
    /* They must be equal or one must be a leading substring of the other. */
    if (i < xs->length)
        return 1; /* y must be the substring. */
    else if (i < ys->length)
        return -1; /* x must be the substring */
    else
        return 0; /* They must be equal. */
}

Handle compareStrings(TaskData *mdTaskData, Handle y, Handle x)
{
    return mdTaskData->saveVec.push(TAGGED(string_test(DEREFWORD(x), DEREFWORD(y))));
}

Handle testStringEqual(TaskData *mdTaskData, Handle y, Handle x)
{
    return mdTaskData->saveVec.push(string_test(DEREFWORD(x), DEREFWORD(y)) == 0 ? TAGGED(1) : TAGGED(0));
}

Handle testStringNotEqual(TaskData *mdTaskData, Handle y, Handle x)
{
    return mdTaskData->saveVec.push(string_test(DEREFWORD(x), DEREFWORD(y)) != 0 ? TAGGED(1) : TAGGED(0));
}

Handle testStringGreater(TaskData *mdTaskData, Handle y, Handle x)
{
    return mdTaskData->saveVec.push(string_test(DEREFWORD(x), DEREFWORD(y)) > 0 ? TAGGED(1) : TAGGED(0));
}

Handle testStringLess(TaskData *mdTaskData, Handle y, Handle x)
{
    return mdTaskData->saveVec.push(string_test(DEREFWORD(x), DEREFWORD(y)) < 0 ? TAGGED(1) : TAGGED(0));
}

Handle testStringGreaterOrEqual(TaskData *mdTaskData, Handle y, Handle x)
{
    return mdTaskData->saveVec.push(string_test(DEREFWORD(x), DEREFWORD(y)) >= 0 ? TAGGED(1) : TAGGED(0));
}

Handle testStringLessOrEqual(TaskData *mdTaskData, Handle y, Handle x)
{
    return mdTaskData->saveVec.push(string_test(DEREFWORD(x), DEREFWORD(y)) <= 0 ? TAGGED(1) : TAGGED(0));
}
