/*
    Title:      Arbitrary Precision Package.
    Author:     Dave Matthews, Cambridge University Computer Laboratory

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

/*
Arbitrary precision package in C.

Integers are held in two formats in this system, long-form and short-form.
The two are distinquished by the integer tag bit, short-form having the tag
bit set and pointers to long-form being untagged.
The long-form integers use the standard Poly format for multi-word
objects, with the length count and flags in the word just before the object
pointed to.  The sign of long-form integers is coded in one of the flag bits.
Short integers are signed quantities, and can be directly
manipulated by the relevant instructions, but if overflow occurs then the full
long versions of the operations will need to be called.
Long-form integers are held as vectors of bytes (i.e. unsigned char)
low-order byte first. It is assumed that a ``byte'' will hold an 8-bit
quantity and a ``long'' at least two ``bytes''. It is essential that unsigned
values are used.
Integers are always stored in the least possible number of words, and
will be shortened to the short-form when possible.

Thanks are due to D. Knuth for the long division algorithm.
*/

/*
DCJM 13/5/06 - Begin changes to use the top bit of the last byte as a sign bit.
Actually, it will probably be simpler to always use a whole byte for the sign.
Start by adding an extra zero byte.

N.B. This actually breaks the rule that all immutable values must have a
canonical representation.  That is required for structure equality to work.
*/ 

#ifdef WIN32
#include "winconfig.h"
#else
#include "config.h"
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x)   assert(x)
#else
#define ASSERT(x)
#endif

#include "globals.h"
#include "sys.h"
#include "run_time.h"
#include "arb.h"
#include "save_vec.h"
#include "processes.h"

/******************************************************************************/
/*                                                                            */
/*      get_length - utility function                                         */
/*                                                                            */
/******************************************************************************/
/* Returns the length of the argument with trailing zeros removed.
   Excludes the sign byte (when we've added it - not done yet). */

static POLYUNSIGNED get_length(PolyWord x)
{
    byte *u = (byte *)x.AsObjPtr();
    POLYUNSIGNED  lu  = OBJECT_LENGTH(x)*sizeof(PolyWord);
    
    for( ; (lu > 0) && (u[lu-1] == 0); lu--)
    {
        /* do nothing */
    }
    
    return lu;
} /* get_length */

/******************************************************************************/
/*                                                                            */
/*      get_C_ulong - called by runtime system                                */
/*                                                                            */
/******************************************************************************/
/* indirection removed 31/10/93 SPF */
POLYUNSIGNED get_C_ulong(TaskData *taskData, PolyWord number)
{
    if ( IS_INT(number) )
    {
        POLYSIGNED i = UNTAGGED(number);
        if ( i < 0 ) raise_exception0(taskData, EXC_size );
        return i;
    }
    else
    {
        POLYUNSIGNED length = get_length(number);
        POLYSIGNED c = 0;
        byte *ptr = number.AsCodePtr();
        if (OBJ_IS_NEGATIVE(GetLengthWord(number))) raise_exception0(taskData, EXC_size );
        if ( length > sizeof(PolyWord) ) raise_exception0(taskData, EXC_size );
        while ( length-- ) c = (c << 8) | ((byte *) ptr)[length];
        return c;
    }
}

/******************************************************************************/
/*                                                                            */
/*      get_C_long - called by runtime system                                 */
/*                                                                            */
/******************************************************************************/
/* indirection removed 31/10/93 SPF */
#define MAX_INT_PLUS1   ((POLYUNSIGNED)0x80 << ( (sizeof(PolyWord)-1) *8))
POLYSIGNED get_C_long(TaskData *taskData, PolyWord number)
{
    if ( IS_INT(number) )
    {
        return UNTAGGED(number);
    }
    else
    {
        int sign   = OBJ_IS_NEGATIVE(GetLengthWord(number)) ? -1 : 0;
        POLYUNSIGNED length = get_length(number);
        POLYUNSIGNED c = 0;
        byte *ptr = number.AsCodePtr();
        
        if ( length > sizeof(PolyWord) ) raise_exception0(taskData, EXC_size );
        
        while ( length-- )
        {
            c = (c << 8) | ptr[length];
        }
        
        if ( sign == 0 && c <  MAX_INT_PLUS1) return   (POLYSIGNED)c;
        if ( sign != 0 && c <= MAX_INT_PLUS1) return -((POLYSIGNED)c);
        
        raise_exception0(taskData, EXC_size );
        /*NOTREACHED*/
		return 0;
    }
}

/******************************************************************************/
/*                                                                            */
/*      get_C_short - called by runtime system                                */
/*                                                                            */
/******************************************************************************/
/* indirection removed 31/10/93 SPF */
short get_C_short(TaskData *taskData, PolyWord number)
{
    int i = (int)get_C_long(taskData, number);
    
    if ( i <= 32767 && i >= -32768 ) return i;
    
    raise_exception0(taskData, EXC_size );
    /*NOTREACHED*/
	return 0;
}

/******************************************************************************/
/*                                                                            */
/*      get_C_ushort - called by runtime system                               */
/*                                                                            */
/******************************************************************************/
/* indirection removed 31/10/93 SPF */
unsigned short get_C_ushort(TaskData *taskData, PolyWord number)
{
    POLYUNSIGNED u = get_C_ulong(taskData, number );
    
    if ( u <= 65535 ) return (short)u;
    
    raise_exception0(taskData, EXC_size );
    /*NOTREACHED*/
	return 0;
}

/* Get a arbitrary precision value as a pair of words.
   At present this is used to extract a 64-bit quantity as two
   words.  Only used in Windows code.  */
void get_C_pair(TaskData *taskData, PolyWord number, unsigned long *pHi, unsigned long *pLo)
{
    if ( IS_INT(number) )
    {
        /* It will fit in the low-order word. */
        *pLo = (unsigned long) UNTAGGED(number);
        *pHi = 0;
        return;
    }
    else
    {
        POLYUNSIGNED length = get_length(number);
        unsigned i;
        unsigned long c;
        POLYOBJPTR ptr = number.AsObjPtr();
        
        if (OBJ_IS_NEGATIVE(GetLengthWord(number))) raise_exception0(taskData, EXC_size);
        if ( length > 2*sizeof(unsigned long) ) raise_exception0(taskData, EXC_size);
        
        /* Low-order word. */
        if (length > sizeof(unsigned long)) i = sizeof(unsigned long); else i = length;
        c = 0;
        while (i--) c = (c << 8) | ((byte *) ptr)[i];
        *pLo = c;
        
        /* High-order word. */
        i = length;
        c = 0;
        while (i-- > sizeof(unsigned long)) c = (c << 8) | ((byte *) ptr)[i];
        *pHi = c;
        
        return;
    }
}

/******************************************************************************/
/*                                                                            */
/*      get_long - utility function                                           */
/*                                                                            */
/******************************************************************************/
/* Gets the arguments into the long form. */
/* HACK - must NOT be used for results.   */
static Handle get_long(TaskData *taskData, Handle x, PolyWord *extend, int *sign)
{
    if (IS_INT(DEREFWORD(x)))
    { /* Short form - put it in the temporary in the task-data  */
        POLYSIGNED x_v = UNTAGGED(DEREFWORD(x));
        PolyObject *pObj = (PolyObject*)(extend+1);
        pObj->SetLengthWord(1);
        byte *u = (byte*)pObj;
;
        
        if (x_v >= 0)
        {
            *sign = 0;
        }
        else /* Negative */ 
        {
            *sign = -1;
            x_v   = -x_v;
        }
        
        /* Put into extend buffer, low order byte first. */
        for (unsigned i = 0; i < sizeof(PolyWord); i++)
        {
            u[i] = x_v & 0xff;
            x_v = x_v >> 8;
        }
        return taskData->saveVec.push(pObj);
    }
    else
    { /* Long-form - x is an address. */
        *sign = OBJ_IS_NEGATIVE(GetLengthWord(DEREFWORD(x))) ? -1 : 0;
        return x;
    }
} /* get_long */

/******************************************************************************/
/*                                                                            */
/*      copy_long - utility function                                          */
/*                                                                            */
/******************************************************************************/
static Handle copy_long(TaskData *taskData, Handle x, POLYUNSIGNED lx)
{
    Handle y = alloc_and_save(taskData, WORDS(lx+1), F_BYTE_BIT|F_MUTABLE_BIT);
    
    // copy the bytes
    byte *u = DEREFBYTEHANDLE(x);  
    byte *v = DEREFBYTEHANDLE(y);  
    
    for (unsigned i = 0; i < lx; i++) v[i] = u[i];
    
    return y;
}


/******************************************************************************/
/*                                                                            */
/*      make_canonical - utility function                                     */
/*                                                                            */
/******************************************************************************/
/* make_canonical is used to force a result into its shortest form,
   in the style of get_length, but also may convert its argument
   from long to short integer */

static Handle make_canonical(TaskData *taskData, Handle x, int sign)
{   /* get length in BYTES */
    POLYUNSIGNED size = get_length(DEREFWORD(x));
    PolyObject *tempX = DEREFWORDHANDLE(x);
    
    ASSERT(size < (OBJ_OBJECT_LENGTH(tempX->LengthWord())*sizeof(PolyWord))); // Check we always have a sign byte.

    /* We can use the short representation if it will fit in 4 bytes. */
    if (size <= sizeof(PolyWord))
    {
        /* Convert the digits. */
        byte    *u = DEREFBYTEHANDLE(x);
        POLYUNSIGNED r = 0;
        for (unsigned i=0; i < sizeof(PolyWord); i++)
        {
            r |= ((POLYUNSIGNED)u[i]) << (8*i);
        }
        
        /* Check for MAXTAGGED+1 before subtraction
           in case MAXTAGGED is 0x7fffffff */
        
        if (r <= MAXTAGGED || (r == MAXTAGGED+1 && sign < 0))
        {
            if (sign < 0)
                return taskData->saveVec.push(TAGGED(-(POLYSIGNED)r));
            else
                return taskData->saveVec.push(TAGGED(r));
        }
    }
    
    /* The length word of the object is changed to reflect the new length.
       This is safe because any words thrown away must be zero. */
    DEREFWORDHANDLE(x)->SetLengthWord(WORDS(size+1), F_BYTE_BIT | (sign < 0 ? F_NEGATIVE_BIT: 0));
    
    return x;
} /* make_canonical */


/******************************************************************************/
/*                                                                            */
/*      Make_XXX functions (used in runtime system)                           */
/*                                                                            */
/******************************************************************************/
Handle Make_arbitrary_precision(TaskData *taskData, POLYSIGNED val)
/* Called from routines in the run-time system to generate an arbitrary
   precision integer from a word value. */
{
    if (val <= MAXTAGGED && val >= -MAXTAGGED-1) /* No overflow */
        return taskData->saveVec.push(TAGGED(val));
    
    POLYUNSIGNED uval;

    if (val < 0) uval = -val;
    else uval = val;

    int words = 1;
    // If the high order byte is non-zero add an extra word for the sign.
    if ((uval >> ((sizeof(long)-1)*8)) != 0) words++;

    Handle y = alloc_and_save(taskData, words, ((val < 0) ? F_NEGATIVE_BIT : 0)| F_BYTE_BIT);

    byte *v = DEREFBYTEHANDLE(y);
    for (POLYUNSIGNED i = 0; uval != 0; i++)
    {
        v[i] = (byte)(uval & 0xff);
        uval >>= 8;
    }
    
    return y;
}

Handle Make_unsigned(TaskData *taskData, POLYUNSIGNED uval)
/* Called from routines in the run-time system to generate an arbitrary
   precision integer from an unsigned value. */
{
    if (uval <= MAXTAGGED) return taskData->saveVec.push(TAGGED(uval));
    
    int words = 1;
    // If the high order byte is non-zero add an extra word for the sign.
    if ((uval >> ((sizeof(long)-1)*8)) != 0) words++;

    Handle y = alloc_and_save(taskData, words, F_BYTE_BIT);
    
    byte *v = DEREFBYTEHANDLE(y);
    for (POLYUNSIGNED i = 0; uval != 0; i++)
    {
        v[i] = (byte)(uval & 0xff);
        uval >>= 8;
    }
    
    return y;
}

/* Creates an arbitrary precision number from two words.
   At present this is used for 64-bit quantities. */
Handle Make_arb_from_pair(TaskData *taskData, unsigned hi, unsigned lo)
{
    /* If the high word is zero we can use either the tagged short
       form or a single word. */
    if (hi == 0) return Make_unsigned(taskData, lo);
    int words = 2;
    // If the high order byte is non-zero add an extra word for the sign.
    if ((hi >> ((sizeof(hi)-1)*8)) != 0) words++;
    
    Handle y = alloc_and_save(taskData, words, F_BYTE_BIT);
    
    byte *v = DEREFBYTEHANDLE(y);
    int i;
    for (i = 0; i < 4/* Get a 32 bit value. */; i++)
    {
        v[i] = lo & 0xff;
        lo >>= 8;
    }
    for (; hi != 0 && i < 8; i++)
    {
        v[i] = hi & 0xff;
        hi >>= 8;
    }
    
    return y;
}

/* Returns hi*scale+lo as an arbitrary precision number.  Currently used
   for Unix time values where the time is returned as two words, a number
   of seconds and a number of microseconds and we wish to return the
   result as a number of microseconds. */
Handle Make_arb_from_pair_scaled(TaskData *taskData, unsigned hi, unsigned lo, unsigned scale)
{
   /* We might be able to compute the number as a 64 bit quantity and
      then convert it but this is probably more portable. It does risk
      overflowing the save vector. */
    Handle hHi = Make_unsigned(taskData, hi);
    Handle hLo = Make_unsigned(taskData, lo);
    Handle hScale = Make_unsigned(taskData, scale);
    return add_longc(taskData, mult_longc(taskData, hHi, hScale), hLo);
}

/******************************************************************************/
/*                                                                            */
/*      neg_longc - called from assembly code                                  */
/*                                                                            */
/******************************************************************************/
Handle neg_longc(TaskData *taskData, Handle x)
{
    Handle long_x, long_y;
    int sign_x;
    
    if (IS_INT(DEREFWORD(x)))
    {
        POLYSIGNED s = UNTAGGED(DEREFWORD(x));
        if (s != -MAXTAGGED-1) // If it won't overflow
            return taskData->saveVec.push(TAGGED(-s));
    }
    
    /* Either overflow or long argument - convert to long form */
    long_x = get_long(taskData, x, taskData->x_extend, &sign_x);
    
    /* Get length of arg. */
    POLYUNSIGNED lx = get_length(DEREFWORD(long_x));
    
    /* copy the argument (so we can return it) */
    long_y = copy_long(taskData, long_x,lx);
    
    /* Return the value with the sign changed. */
    return make_canonical(taskData, long_y, sign_x ^ -1);
} /* neg_longc */


/******************************************************************************/
/*                                                                            */
/*      add_unsigned_long - utility function                                  */
/*            computes sign * (abs(x) + abs(y))                               */
/*                                                                            */
/******************************************************************************/
static Handle add_unsigned_long(TaskData *taskData, Handle x, Handle y, int sign)
{
    byte *u; /* byte-pointer for longer number  */
    byte *v; /* byte-pointer for shorter number */
    Handle z;
    
    int lu;   /* length of u in bytes */
    int lv;   /* length of v in bytes */
    
    /* find the longer number */
    POLYUNSIGNED lx = get_length(DEREFWORD(x));
    POLYUNSIGNED ly = get_length(DEREFWORD(y));
    
    /* Make ``u'' the longer. */
    if (lx < ly)
    {
        // Get result vector. It must be 1 byte longer than u
        // to have space for any carry, plus one byte for the sign.
        z = alloc_and_save(taskData, WORDS(ly+2), F_MUTABLE_BIT|F_BYTE_BIT);
        
        /* now safe to dereference pointers */
        u = DEREFBYTEHANDLE(y); lu = ly;
        v = DEREFBYTEHANDLE(x); lv = lx;
    }
    
    else
    {
        // Get result vector. It must be 1 byte longer than u
        // to have space for any carry, plus one byte for the sign.
        z = alloc_and_save(taskData, WORDS(lx+2), F_MUTABLE_BIT|F_BYTE_BIT);
        
        /* now safe to dereference pointers */
        u = DEREFBYTEHANDLE(x); lu = lx;
        v = DEREFBYTEHANDLE(y); lv = ly;
    }
    
    /* do the actual addition */
    byte  *w = DEREFBYTEHANDLE(z);
    int carry = 0;
    int i     = 0;
    
    /* Do the additions */
    for( ; i < lv; i++)
    {
        carry += u[i] + v[i];
        w[i] = carry & 0xff;
        carry >>= 8; 
    }
    
    /* Add the carry to the rest of ``u''. */
    for( ; i < lu; i++)
    {
        carry += u[i];
        w[i] = carry & 0xff;
        carry >>= 8;
    }
    
    /* Finally put the carry into the last byte */
    w[i] = (byte)carry;
    
    return make_canonical(taskData, z, sign);
} /* add_unsigned_long */


/******************************************************************************/
/*                                                                            */
/*      sub_unsigned_long - utility function                                  */
/*            computes sign * (abs(x) - abs(y))                               */
/*                                                                            */
/******************************************************************************/
static Handle sub_unsigned_long(TaskData *taskData, Handle x, Handle y, int sign)
{
    byte *u; /* byte-pointer alias for larger number  */
    byte *v; /* byte-pointer alias for smaller number */
    int lu;   /* length of u in bytes */
    int lv;   /* length of v in bytes */
    Handle z;
    
    /* get the larger argument into ``u'' */
    /* This is necessary so that we can discard */
    /* the borrow at the end of the subtraction */
    POLYUNSIGNED lx = get_length(DEREFWORD(x));
    POLYUNSIGNED ly = get_length(DEREFWORD(y));
    
    if (lx < ly)
    {
        sign ^= -1; /* swap sign of result SPF 21/1/94 */
        z = alloc_and_save(taskData, WORDS(ly+1), F_MUTABLE_BIT|F_BYTE_BIT);
        
        
        /* now safe to dereference pointers */
        u = DEREFBYTEHANDLE(y); lu = ly;
        v = DEREFBYTEHANDLE(x); lv = lx;
    }
    else if (ly < lx)
    {
        z = alloc_and_save(taskData, WORDS(lx+1), F_MUTABLE_BIT|F_BYTE_BIT);
        
        /* now safe to dereference pointers */
        u = DEREFBYTEHANDLE(x); lu = lx;
        v = DEREFBYTEHANDLE(y); lv = ly;
    }
    
    else /* lx == ly */
    { /* Must look at the numbers to decide which is bigger. */
        int i = lx - 1;
        for( ; i >= 0 && DEREFBYTEHANDLE(x)[i] == DEREFBYTEHANDLE(y)[i]; i--);
        
        if (i < 0) return taskData->saveVec.push(TAGGED(0)); /* They are equal */
        
        if (DEREFBYTEHANDLE(x)[i] < DEREFBYTEHANDLE(y)[i])
        {
            sign ^= -1; /* swap sign of result SPF 21/1/94 */
            z = alloc_and_save(taskData, WORDS(ly+1), F_MUTABLE_BIT|F_BYTE_BIT);
            
            /* now safe to dereference pointers */
            u = DEREFBYTEHANDLE(y); lu = ly;
            v = DEREFBYTEHANDLE(x); lv = lx;
        }
        else
        {
            z = alloc_and_save(taskData, WORDS(lx+1), F_MUTABLE_BIT|F_BYTE_BIT);
            
            /* now safe to dereference pointers */
            u = DEREFBYTEHANDLE(x); lu = lx;
            v = DEREFBYTEHANDLE(y); lv = ly;
        }
    }
    
    byte    *w = DEREFBYTEHANDLE(z);
    int borrow = 1; /* Becomes 0 if there is a borrow */
    int      i = 0;
    
    /* Do the subtractions */
    for( ; i < lv; i++)
    {
        borrow += 255 + u[i] - v[i];
        w[i] = borrow & 0xff;
        borrow >>= 8;
    }
    
    /* Add the borrow into the rest of ``u''. */
    for( ; i < lu; i++)
    {
        borrow += 255 + u[i];
        w[i] = borrow & 0xff;
        borrow >>= 8;
    }
    
    return make_canonical(taskData, z, sign);
} /* sub_unsigned_long */


/******************************************************************************/
/*                                                                            */
/*      add_longc - called from sparc_dep.c                                   */
/*                                                                            */
/******************************************************************************/
Handle add_longc(TaskData *taskData, Handle y, Handle x)
{
    Handle long_x, long_y;
    int sign_x, sign_y;
    
    if (IS_INT(DEREFWORD(x)) && IS_INT(DEREFWORD(y)))
    {  /* Both short */
       /* The easiest way to do the addition is simply *x-1+*y, but that
        makes it more difficult to check for overflow. */
        POLYSIGNED t = UNTAGGED(DEREFWORD(x)) + UNTAGGED(DEREFWORD(y));
        if (t <= MAXTAGGED && t >= -MAXTAGGED-1) /* No overflow */
        {
            return taskData->saveVec.push(TAGGED(t));
        }
    }
    
    /* Either overflow or long arguments - convert to long form */
    long_x = get_long(taskData, x, taskData->x_extend , &sign_x);
    long_y = get_long(taskData, y, taskData->y_extend , &sign_y);
    
    /* Work out whether to add or subtract */
    if ((sign_y ^ sign_x) >= 0) /* signs the same? */
    { /* sign(x) * (abs(x) + abs(y)) */
        return add_unsigned_long(taskData, long_x, long_y, sign_x);
    }
    else
    { /* sign(x) * (abs(x) - abs(y)) */
        return sub_unsigned_long(taskData, long_x, long_y, sign_x);
    }
} /* add_longc */


/******************************************************************************/
/*                                                                            */
/*      sub_longc - called from sparc_dep.c                                   */
/*                                                                            */
/******************************************************************************/
Handle sub_longc(TaskData *taskData, Handle y, Handle x)
{
    Handle long_x, long_y;
    int sign_x, sign_y;
    
    if (IS_INT(DEREFWORD(x)) && 
        IS_INT(DEREFWORD(y))) /* Both short */
    {
    /* The easiest way to do the subtraction is simply *x-*y+1, but that
        makes it more difficult to check for overflow. */
        POLYSIGNED t = UNTAGGED(DEREFWORD(x)) - UNTAGGED(DEREFWORD(y));
        if (t <= MAXTAGGED && t >= -MAXTAGGED-1) /* No overflow */
            return taskData->saveVec.push(TAGGED(t));
    }
    
    /* Either overflow or long arguments. */
    long_x = get_long(taskData, x, taskData->x_extend , &sign_x); /* Convert to long form */
    long_y = get_long(taskData, y, taskData->y_extend , &sign_y);
    
    /* If the signs are different add the two values. */
    if ((sign_y ^ sign_x) < 0) /* signs differ */
    { /* sign(x) * (abs(x) + abs(y)) */
        return add_unsigned_long(taskData, long_x, long_y, sign_x);
    }
    else
    { /* sign(x) * (abs(x) - abs(y)) */
        return sub_unsigned_long(taskData, long_x, long_y, sign_x);
    }
} /* sub_longc */


/******************************************************************************/
/*                                                                            */
/*      mult_longc - called from sparc_assembly.s                             */
/*                                                                            */
/******************************************************************************/
Handle mult_longc(TaskData *taskData, Handle y, Handle x)
{
    Handle long_x, long_y, long_z;
    int sign_x, sign_y;
    
    /* I'm not sure how to detect overflow here - so go through the full
    routine even if both arguments are short. */
    
    /* Convert to long form */
    long_x = get_long(taskData, x, taskData->x_extend , &sign_x);
    long_y = get_long(taskData, y, taskData->y_extend , &sign_y);
    
    /* Get lengths of args. */
    POLYUNSIGNED lx = get_length(DEREFWORD(long_x));
    POLYUNSIGNED ly = get_length(DEREFWORD(long_y));
    
    /* Check for zero args. */
    if (lx == 0 || ly == 0)
    {
        return taskData->saveVec.push(TAGGED(0));
    }
    
    /* Get space for result */
    long_z = alloc_and_save(taskData, WORDS(lx+ly+1), F_MUTABLE_BIT|F_BYTE_BIT);
    
    {
        /* Can now load the actual addresses because they will not change now. */
        byte *u = DEREFBYTEHANDLE(long_x);
        byte *v = DEREFBYTEHANDLE(long_y);
        byte *w = DEREFBYTEHANDLE(long_z);
        
        for(POLYUNSIGNED i = 0; i < lx; i++)
        {
            POLYUNSIGNED j;
            long r = 0; /* Set the carry to zero */
            for(j = 0; j < ly; j++)
            {
                /* Compute the product. */
                r += u[i] * v[j];
                /* Now add in to the result. */
                r += w[i+j];
                w[i+j] = r & 0xff;
                r >>= 8;
            }
            /* Put in any carry. */
            w[i+j] = (byte)r;
        }
        
        return make_canonical(taskData, long_z, sign_x ^ sign_y);
    }
} /* mult_long */


/******************************************************************************/
/*                                                                            */
/*      div_unsigned_long - utility function                                  */
/*                                                                            */
/******************************************************************************/
static void div_unsigned_long(byte *u, byte *v, byte *res, POLYUNSIGNED lu, POLYUNSIGNED lv)
/* Unsigned division. This is the main divide and remainder routine. The
result is packed into ``res'' and is separated out by div and rem */
{
    POLYUNSIGNED i,j;
    int bits;
    long r;
    
    /* Find out how far to shift v to get a 1 in the top bit. */
    bits = 0;
    for(r = v[lv-1]; r < 128; r <<= 1) bits++; /* 128 ??? */
    
    /* Shift u that amount into res+2. We have allowed enough room for
       overflow. */
    r = 0;
    for (i = 0; i < lu; i++)
    {
        r |= u[i] << bits; /*``Or in'' the new bits after shifting*/
        res[i+2] = r & 0xff;    /* Put into the destination. */
        r >>= 8;                /* and shift down the carry. */
    }
    res[i+2] = (byte)r; /* Put in the carry */
    
    /* And v that amount. It has already been copied. */
    if ( bits )
    {
        r = 0;
        for (i = 0; i < lv; i++)
        { r |= v[i] << bits; v[i] = r & 0xff; r >>= 8; }
        /* No carry */
    }
    
    for(j = lu+2; j >= lv+2; j--)
    {
    /* j iterates over the higher digits of the dividend until we are left
        with a number which is less than the divisor. This is the remainder. */
        long quotient, dividend, r;
        dividend = res[j]*256 + res[j-1];
        quotient = (res[j] == v[lv-1]) ? 255 : dividend/(long)v[lv-1];
        
        if (lv != 1)
        {
            while ((long)v[lv-2]*quotient >
                (dividend - quotient*(long)v[lv-1])*256 + (long)res[j-2])
            {
                quotient--;
            }
        }
        
        /* The quotient is at most 1 too large */
        /* Subtract the product of this with ``v'' from ``res''. */
        r = 1; /* Initial borrow */
        for(i = 0; i < lv; i++)
        {
            r += 255 + res[j-lv+i] - quotient * v[i];
            res[j-lv+i] = r & 0xff;
            r >>= 8;
        }
        
        r += res[j]; /* Borrow from leading digit. */
                     /* If we are left with a borrow when the subtraction is complete the
                     quotient must have been too big. We add ``v'' to the dividend and
        subtract 1 from the quotient. */
        if (r == 0 /* would be 1 if there were no borrow */)
        { 
            quotient --;
            r = 0;
            for (i = 0; i < lv; i++)
            {
                r += v[i] + res[j-lv+i];
                res[j-lv+i] = r & 0xff;
                r >>= 8;
            }
        }
        /* Place the next digit of quotient in result */
        res[j] = (byte)quotient;
    }
    
    /* Likewise the remainder. */
    if (bits)
    {
        r = 0;
        j = lv;
        j = lv;
        while (j > 0)
        {
            j--;
            r |= res[j+2];
            res[j+2] = (r >> bits) & 0xff;
            r = (r & 0xff) << 8;
        }
    }
} /* div_unsigned_long */

/******************************************************************************/
/*                                                                            */
/*      div_long_c - called from sparc_assembly.s                             */
/*                                                                            */
/******************************************************************************/
Handle div_longc(TaskData *taskData, Handle y, Handle x)
{
    Handle long_x, long_y;
    int sign_x, sign_y;
    
    if (IS_INT(DEREFWORD(x)) &&
        IS_INT(DEREFWORD(y))) /* Both short */
    {
        POLYSIGNED xs = UNTAGGED(DEREFWORD(x));
        POLYSIGNED ys = UNTAGGED(DEREFWORD(y));
        /* Raise exceptions if dividing by zero. */
        if (ys == 0)
            raise_exception0(taskData, EXC_divide);
        
        /* Only possible overflow is minint div -1 */
        if (xs != -MAXTAGGED-1 || ys != -1)
            return taskData->saveVec.push(TAGGED(xs / ys));
    }
    
    /* Long operands or overflow - convert to long form */
    long_x = get_long(taskData, x, taskData->x_extend , &sign_x);
    long_y = get_long(taskData, y, taskData->y_extend , &sign_y);
    
    /* Get lengths of args. */
    POLYUNSIGNED lx = get_length(DEREFWORD(long_x));
    POLYUNSIGNED ly = get_length(DEREFWORD(long_y));
    
    /* If length of v is zero raise divideerror */
    if (ly == 0) raise_exception0(taskData, EXC_divide);
    
    /* Get quotient */
    if (lx < ly)
    {
        /* When x < y quotient must be zero. */
        return taskData->saveVec.push(TAGGED(0));
    }
    else
    {
        Handle long_z;
        long_y = copy_long(taskData, long_y,ly); /* copy in case it needs shifting */
        
        /* vector for result - size of x + 3 */
        long_z = alloc_and_save(taskData, WORDS(lx+3+1), F_MUTABLE_BIT|F_BYTE_BIT);
        
        div_unsigned_long
            (DEREFBYTEHANDLE(long_x), 
            DEREFBYTEHANDLE(long_y),
            DEREFBYTEHANDLE(long_z),
            lx, ly);
        
        /* Copy it down */
        POLYUNSIGNED i;
        for(i = 0; i <= lx-ly; i++) 
        {
            DEREFBYTEHANDLE(long_z)[i] = DEREFBYTEHANDLE(long_z)[i+ly+2];
        }
        
        /* Clear the rest */
        for(; i < lx+3; i++)
        {
            DEREFBYTEHANDLE(long_z)[i] = 0; 
        }
        
        return make_canonical(taskData, long_z, sign_x ^ sign_y);
    }
} /* div_longc */

/******************************************************************************/
/*                                                                            */
/*      rem_long_c - called from sparc_assembly.s                             */
/*                                                                            */
/******************************************************************************/
Handle rem_longc(TaskData *taskData, Handle y, Handle x)
{
    Handle long_x, long_y;
    int sign_x, sign_y;
    
    if (IS_INT(DEREFWORD(x)) && 
        IS_INT(DEREFWORD(y))) /* Both short */
    {
        POLYSIGNED xs = UNTAGGED(DEREFWORD(x));
        POLYSIGNED ys = UNTAGGED(DEREFWORD(y));
        /* Raise exceptions if remaindering by zero. */
        if (ys == 0)
            raise_exception0(taskData, EXC_divide);
        
        /* Only possible overflow is minint mod -1 */
        if (xs != -MAXTAGGED-1 || ys != -1)
            return taskData->saveVec.push(TAGGED(xs % ys));
    }
    
    /* Long operands or overflow - convert any shorts to long form */
    long_x = get_long(taskData, x, taskData->x_extend , &sign_x);
    long_y = get_long(taskData, y, taskData->y_extend , &sign_y);
    
    /* Get lengths of args. */ 
    POLYUNSIGNED lx = get_length(DEREFWORD(long_x));
    POLYUNSIGNED ly = get_length(DEREFWORD(long_y));
    
    /* If length of y is zero raise divideerror */
    if (ly == 0) raise_exception0(taskData, EXC_divide);
    
    /* Get remainder */
    if (lx < ly)
    {
        /* When x < y remainder is x. */
        return x; /* NOT long_x, since x may be a short. SPF 1/11/95 */
    }
    else
    {
        POLYUNSIGNED i;
        Handle long_z;
        
        /* copy in case it needs shifting */
        long_y = copy_long(taskData, long_y,ly);
        
        /* vector for result - size of x + 3 */
        long_z = alloc_and_save(taskData, WORDS(lx+3+1), F_MUTABLE_BIT|F_BYTE_BIT);
        
        div_unsigned_long
            (DEREFBYTEHANDLE(long_x),
            DEREFBYTEHANDLE(long_y),
            DEREFBYTEHANDLE(long_z),
            lx, ly);
        
        /* Copy it down */
        for(i = 0; i < ly; i++)
        {
            DEREFBYTEHANDLE(long_z)[i] = DEREFBYTEHANDLE(long_z)[i+2];
        }
        
        /* Clear the rest */
        for(; i < lx+3; i++)
        {
            DEREFBYTEHANDLE(long_z)[i] = 0; 
        }
        
        return make_canonical(taskData, long_z, sign_x /* Same sign as dividend */ );
        /* ML says it should have same as divisor. */
    }
} /* rem_longc */



/******************************************************************************/
/*                                                                            */
/*      compare_unsigned - utility function                                   */
/*                                                                            */
/******************************************************************************/
/* compare_unsigned is passed LONG integers only */
static int compare_unsigned(Handle x, Handle y)
{
    /* First look at the lengths */
    POLYUNSIGNED lx = get_length(DEREFWORD(x));
    POLYUNSIGNED ly = get_length(DEREFWORD(y));
    
    if (lx != ly)  /* u > v if u longer than v */
    {
        return (lx > ly ? 1 : -1);
    }
    
    { /* Same length - look at the values. */
        byte *u = DEREFBYTEHANDLE(x);
        byte *v = DEREFBYTEHANDLE(y);

        POLYUNSIGNED i = lx;
        while (i > 0) 
        {
            i--;
            if (u[i] != v[i]) 
            {
                return u[i] > v[i] ? 1 : -1;
            }
        }
        /* Must be equal */
        return 0;
    } 
} /* compare_unsigned */


/******************************************************************************/
/*                                                                            */
/*      compareLong - called by run-time system                                */
/*                                                                            */
/******************************************************************************/
int compareLong(TaskData *taskData, Handle y, Handle x)
{
    Handle long_x, long_y;
    int sign_x, sign_y;
    
    if (IS_INT(DEREFWORD(x)) &&
        IS_INT(DEREFWORD(y))) /* Both short */
    {
        POLYSIGNED t = UNTAGGED(DEREFWORD(x)) - UNTAGGED(DEREFWORD(y));
        
        if (t == 0) return 0; /* equal */
        else if (t < 0) return -1; /* less */
        else return 1; /* greater */
    }
    
    /* Convert to long form */
    long_x = get_long(taskData, x, taskData->x_extend, &sign_x); 
    long_y = get_long(taskData, y, taskData->y_extend, &sign_y);
    
    if (sign_x >= 0) /* x is positive */
    {
        if (sign_y >= 0) /* y also positive */ 
        {
            return compare_unsigned(long_x, long_y);
        }
        else /* y negative so x > y */
        {
            return 1;
        }
    }
    else 
    { /* x is negative */
        if (sign_y < 0) /* y also negative */
        {
            return compare_unsigned(long_y, long_x);
        }
        else /* y positive so x < y */
        {
            return -1; 
        }
    }
} /* compareLong */


/* logical_long.  General purpose function for binary logical operations. */
static Handle logical_long(TaskData *taskData, Handle x, Handle y, int signX, int signY,
                           unsigned(*op)(unsigned, unsigned))
{
    byte *u; /* byte-pointer for longer number  */
    byte *v; /* byte-pointer for shorter number */
    Handle z;
    int sign, signU, signV;
    
    POLYUNSIGNED lu;   /* length of u in bytes */
    POLYUNSIGNED lv;   /* length of v in bytes */
    
    { /* find the longer number */
        POLYUNSIGNED lx = get_length(DEREFWORD(x));
        POLYUNSIGNED ly = get_length(DEREFWORD(y));
        
        /* Make ``u'' the longer. */
        if (lx < ly)
        {
            // Get result vector. There can't be any carry at the end so
            // we just need to make this as large as the larger number
            // plus sign byte
            z = alloc_and_save(taskData, WORDS(ly+1), F_MUTABLE_BIT|F_BYTE_BIT);
            
            /* now safe to dereference pointers */
            u = DEREFBYTEHANDLE(y); lu = ly;
            v = DEREFBYTEHANDLE(x); lv = lx;
            signU = signY; signV = signX;
        }
        
        else
        {
            /* Get result vector. */
            z = alloc_and_save(taskData, WORDS(lx+1), F_MUTABLE_BIT|F_BYTE_BIT);
            
            /* now safe to dereference pointers */
            u = DEREFBYTEHANDLE(x); lu = lx;
            v = DEREFBYTEHANDLE(y); lv = ly;
            signU = signX; signV = signY;
        }
    }
    
    sign = (*op)(signU, signV); /* -1 if negative, 0 if positive. */
    
    { /* do the actual operations */
        byte  *w = DEREFBYTEHANDLE(z);
        int borrowU = 1, borrowV = 1, borrowW = 1;
        POLYUNSIGNED i = 0;
        
        /* Do the operations. */
        for( ; i < lv; i++)
        {
            int wI;
            /* Have to convert negative values to twos complement. */
            if (signU) borrowU += 255 - u[i];
            else borrowU = u[i];
            if (signV) borrowV += 255 - v[i];
            else borrowV = v[i];
            wI = (*op)(borrowU, borrowV) & 255;
            if (sign)
            {
                /* Have to convert the result back to twos complement. */
                borrowW += 255 - wI;
                w[i] = borrowW & 255;
                borrowW >>= 8;
            }
            else w[i] = wI;
            borrowU >>= 8;
            borrowV >>= 8;
        }
        /* At this point the borrow of V should be zero. */
        ASSERT(signV == 0 || borrowV == 0);
        
        /* Continue with ``u''. */
        for( ; i < lu; i++)
        {
            int wI;
            if (signU) borrowU += 255 - u[i];
            else borrowU = u[i];
            if (signV) borrowV = 255; else borrowV = 0;
            wI = (*op)(borrowU, borrowV) & 255;
            if (sign)
            {
                /* Have to convert the result back to twos complement. */
                borrowW += 255 - wI;
                w[i] = borrowW & 255;
                borrowW >>= 8;
            }
            else w[i] = wI;
            borrowU >>= 8;
            borrowV >>= 8;
        }
        /* We should now no longer have any borrows. */
        ASSERT(signU == 0 || borrowU == 0);
        ASSERT(sign == 0 || borrowW == 0);
    }
    
    return make_canonical(taskData, z, sign);
} /* logical_long */

static unsigned doAnd(unsigned i, unsigned j)
{
    return i & j;
}

static unsigned doOr(unsigned i, unsigned j)
{
    return i | j;
}

static unsigned doXor(unsigned i, unsigned j)
{
    return i ^ j;
}

/******************************************************************************/
/*                                                                            */
/*      and_longc - called by run-time system                                 */
/*                                                                            */
/******************************************************************************/
Handle and_longc(TaskData *taskData, Handle y, Handle x)
{
    Handle long_x, long_y;
    int sign_x, sign_y;
    
    if (IS_INT(DEREFWORD(x)) && 
        IS_INT(DEREFWORD(y))) /* Both short */
    {
       /* There's no problem with overflow so we can just AND together
           the values. */
        POLYSIGNED t = UNTAGGED(DEREFWORD(x)) & UNTAGGED(DEREFWORD(y));
        return taskData->saveVec.push(TAGGED(t));
    }
    
    /* Long arguments. */
    long_x = get_long(taskData, x, taskData->x_extend , &sign_x); /* Convert to long form */
    long_y = get_long(taskData, y, taskData->y_extend , &sign_y);
    
    return logical_long(taskData, long_x, long_y, sign_x, sign_y, doAnd);
}

/******************************************************************************/
/*                                                                            */
/*      or_longc - called by run-time system                                 */
/*                                                                            */
/******************************************************************************/
Handle or_longc(TaskData *taskData, Handle y, Handle x)
{
    Handle long_x, long_y;
    int sign_x, sign_y;
    
    if (IS_INT(DEREFWORD(x)) && 
        IS_INT(DEREFWORD(y))) /* Both short */
    {
    /* There's no problem with overflow so we can just OR together
        the values. */
        POLYSIGNED t = UNTAGGED(DEREFWORD(x)) | UNTAGGED(DEREFWORD(y));
        return taskData->saveVec.push(TAGGED(t));
    }
    
    /* Long arguments. */
    long_x = get_long(taskData, x, taskData->x_extend , &sign_x); /* Convert to long form */
    long_y = get_long(taskData, y, taskData->y_extend , &sign_y);
    
    return logical_long(taskData, long_x, long_y, sign_x, sign_y, doOr);
}

/******************************************************************************/
/*                                                                            */
/*      xor_longc - called by run-time system                                 */
/*                                                                            */
/******************************************************************************/
Handle xor_longc(TaskData *taskData, Handle y, Handle x)
{
    Handle long_x, long_y;
    int sign_x, sign_y;
    
    if (IS_INT(DEREFWORD(x)) && 
        IS_INT(DEREFWORD(y))) /* Both short */
    {
    /* There's no problem with overflow so we can just XOR together
        the values. */
        POLYSIGNED t = UNTAGGED(DEREFWORD(x)) ^ UNTAGGED(DEREFWORD(y));
        return taskData->saveVec.push(TAGGED(t));
    }
    
    /* Long arguments. */
    long_x = get_long(taskData, x, taskData->x_extend , &sign_x); /* Convert to long form */
    long_y = get_long(taskData, y, taskData->y_extend , &sign_y);
    
    return logical_long(taskData, long_x, long_y, sign_x, sign_y, doXor);
}

/*
These functions are used primarily during porting.  They handle both
short and long forms of integers and are generally superseded by
hand coded assembly code versions and the code generator.
Some of them are retained in some code-generators to handle the
long forms of integers.
*/

Handle equal_longc(TaskData *taskData, Handle y, Handle x)
/* Returns 1 if the arguments are equal, otherwise 0. */
{
    bool c = compareLong(taskData, y, x) == 0;
    return taskData->saveVec.push(c ? TAGGED(1) : TAGGED(0));
}

Handle not_equal_longc(TaskData *taskData, Handle y, Handle x)
{
    bool c = compareLong(taskData, y, x) != 0;
    return taskData->saveVec.push(c ? TAGGED(1) : TAGGED(0));
}

Handle gt_longc(TaskData *taskData, Handle y, Handle x)
{
    bool c = (compareLong(taskData, y, x) == 1);
    
    return taskData->saveVec.push(c ? TAGGED(1) : TAGGED(0));
}

Handle ls_longc(TaskData *taskData, Handle y, Handle x)
{
    bool c = (compareLong(taskData, y, x) == -1);
    
    return taskData->saveVec.push(c ? TAGGED(1) : TAGGED(0));
}

Handle ge_longc(TaskData *taskData, Handle y, Handle x)
{
    bool c = compareLong(taskData, y, x) != -1;
    
    return taskData->saveVec.push(c ? TAGGED(1) : TAGGED(0));
}

Handle le_longc(TaskData *taskData, Handle y, Handle x)
{
    bool c = compareLong(taskData, y, x) != 1;
    
    return taskData->saveVec.push(c ? TAGGED(1) : TAGGED(0));
}

/* Return the low-order bits of an integer whether it is long or
   short form. */
Handle int_to_word_c(TaskData *taskData, Handle x)
{
    /* If it's already short it's easy. */
    if (IS_INT(DEREFWORD(x)))
        return x;
    
    byte    *u = DEREFBYTEHANDLE(x);
    POLYUNSIGNED r = 0;
    for (unsigned i=0; i < sizeof(PolyWord); i++)
    {
        r |= u[i] << (8*i);
    }
    if (OBJ_IS_NEGATIVE(x->Word().AsObjPtr()->LengthWord()))
        r = 0-r; // Use 0-r rather than -r since it's an unsigned value.
    return taskData->saveVec.push(TAGGED(r));
}


