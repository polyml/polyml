/*
    Title:      Arbitrary Precision Package.
    Author:     Dave Matthews, Cambridge University Computer Laboratory

    Further modification Copyright 2010 David C. J. Matthews

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

#ifdef HAVE_CONFIG_H
#include "config.h"
#elif defined(WIN32)
#include "winconfig.h"
#else
#error "No configuration file"
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x)   assert(x)
#else
#define ASSERT(x)
#endif

#ifdef HAVE_GMP_H
#include <gmp.h>
#endif

#include "globals.h"
#include "sys.h"
#include "run_time.h"
#include "arb.h"
#include "save_vec.h"
#include "processes.h"

#define BITS_PER_POLYWORD (SIZEOF_LONG*8)

#ifdef HAVE_GMP_H
#if (BITS_PER_POLYWORD > GMP_LIMB_BITS)
// We're assuming that every GMP limb occupies at least one word
#error "Size of GMP limb is less than a Poly word"
#endif
#endif

#ifdef HAVE_GMP_H
#define DEREFLIMBHANDLE(_x)      ((mp_limb_t *)DEREFHANDLE(_x))

// Returns the length of the argument with trailing zeros removed.
static mp_size_t numLimbs(PolyWord x)
{
    mp_size_t lu = OBJECT_LENGTH(x)*sizeof(PolyWord)/sizeof(mp_limb_t);
    mp_limb_t *u = (mp_limb_t *)x.AsObjPtr();
    while (lu > 0 && u[lu-1] == 0) lu--;
    return lu;
}

#else
// Returns the length of the argument with trailing zeros removed.

static POLYUNSIGNED get_length(PolyWord x)
{
    byte *u = (byte *)x.AsObjPtr();
    POLYUNSIGNED  lu  = OBJECT_LENGTH(x)*sizeof(PolyWord);

    for( ; (lu > 0) && (u[lu-1] == 0); lu--)
    {
        /* do nothing */
    }

    return lu;
}
#endif

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
        if (OBJ_IS_NEGATIVE(GetLengthWord(number))) raise_exception0(taskData, EXC_size );
#ifdef HAVE_GMP_H
        unsigned length = numLimbs(number);
        if (length > 1) raise_exception0(taskData, EXC_size);
        mp_limb_t first = *(mp_limb_t*)number.AsCodePtr();
#if (BITS_PER_POLYWORD < GMP_NUMB_BITS)
        if (first > (mp_limb_t)1 << BITS_PER_POLYWORD)
            raise_exception0(taskData, EXC_size);
#endif
        return first;
#else
        byte *ptr = number.AsCodePtr();
        POLYUNSIGNED length = get_length(number);
        if (length > sizeof(PolyWord) ) raise_exception0(taskData, EXC_size);
        POLYSIGNED c = 0;
        while ( length-- ) c = (c << 8) | ((byte *) ptr)[length];
        return c;
#endif
    }
}

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
#ifdef HAVE_GMP_H
        unsigned length = numLimbs(number);
        if (length > 1) raise_exception0(taskData, EXC_size);
        mp_limb_t c = *(mp_limb_t*)number.AsCodePtr();
#else
        POLYUNSIGNED length = get_length(number);
        POLYUNSIGNED c = 0;
        byte *ptr = number.AsCodePtr();

        if ( length > sizeof(PolyWord) ) raise_exception0(taskData, EXC_size );

        while ( length-- )
        {
            c = (c << 8) | ptr[length];
        }
#endif
        if ( sign == 0 && c <  MAX_INT_PLUS1) return   (POLYSIGNED)c;
        if ( sign != 0 && c <= MAX_INT_PLUS1) return -((POLYSIGNED)c);

        raise_exception0(taskData, EXC_size );
        /*NOTREACHED*/
		return 0;
    }
}

short get_C_short(TaskData *taskData, PolyWord number)
{
    int i = (int)get_C_long(taskData, number);

    if ( i <= 32767 && i >= -32768 ) return i;

    raise_exception0(taskData, EXC_size );
    /*NOTREACHED*/
	return 0;
}

unsigned short get_C_ushort(TaskData *taskData, PolyWord number)
{
    POLYUNSIGNED u = get_C_ulong(taskData, number );

    if ( u <= 65535 ) return (short)u;

    raise_exception0(taskData, EXC_size );
    /*NOTREACHED*/
	return 0;
}

#ifdef WINDOWS_PC
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
    if (OBJ_IS_NEGATIVE(GetLengthWord(number))) raise_exception0(taskData, EXC_size);

    POLYUNSIGNED length = get_length(number);
    unsigned i;
    unsigned long c;
    POLYOBJPTR ptr = number.AsObjPtr();

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
}
#endif

static Handle get_long(Handle x, Handle extend, int *sign)
{
    if (IS_INT(DEREFWORD(x)))
    {
        // Short form - put it in the temporary.
        POLYSIGNED x_v = UNTAGGED(DEREFWORD(x));
        if (x_v >= 0) *sign = 0;
        else /* Negative */
        {
            *sign = -1;
            x_v   = -x_v;
        }
 #ifdef HAVE_GMP_H
        mp_limb_t *u = DEREFLIMBHANDLE(extend);
        *u = x_v;
#else
        byte *u = DEREFBYTEHANDLE(extend);

        /* Put into extend buffer, low order byte first. */
        for (unsigned i = 0; i < sizeof(PolyWord); i++)
        {
            u[i] = x_v & 0xff;
            x_v = x_v >> 8;
        }
#endif
        return extend;
    }
    else
    { /* Long-form - x is an address. */
        *sign = OBJ_IS_NEGATIVE(GetLengthWord(DEREFWORD(x))) ? -1 : 0;
        return x;
    }
}

#ifndef HAVE_GMP_H
static Handle copy_long(TaskData *taskData, Handle x, POLYUNSIGNED lx)
{
    Handle y = alloc_and_save(taskData, WORDS(lx), F_BYTE_OBJ|F_MUTABLE_BIT);

    // copy the bytes
    byte *v = DEREFBYTEHANDLE(y);
    memcpy(v, DEREFBYTEHANDLE(x), lx);
    return y;
}
#endif

/* make_canonical is used to force a result into its shortest form,
   in the style of get_length, but also may convert its argument
   from long to short integer */
static Handle make_canonical(TaskData *taskData, Handle x, int sign)
{
#ifdef HAVE_GMP_H
    unsigned size = numLimbs(DEREFWORD(x));
    if (size == 1)
    {
        mp_limb_t r = *DEREFLIMBHANDLE(x);
        if (r <= MAXTAGGED || (r == MAXTAGGED+1 && sign < 0))
        {
            if (sign < 0)
                return taskData->saveVec.push(TAGGED(-(POLYSIGNED)r));
            else
                return taskData->saveVec.push(TAGGED(r));
        }
    }
    // Throw away any unused words.
    DEREFWORDHANDLE(x)->SetLengthWord(WORDS(size*sizeof(mp_limb_t)), F_BYTE_OBJ | (sign < 0 ? F_NEGATIVE_BIT: 0));
    return x;
#else
    /* get length in BYTES */
    POLYUNSIGNED size = get_length(DEREFWORD(x));

    // We can use the short representation if it will fit in a word.
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
    DEREFWORDHANDLE(x)->SetLengthWord(WORDS(size+1), F_BYTE_OBJ | (sign < 0 ? F_NEGATIVE_BIT: 0));

    return x;
#endif
}


Handle Make_arbitrary_precision(TaskData *taskData, POLYSIGNED val)
/* Called from routines in the run-time system to generate an arbitrary
   precision integer from a word value. */
{
    if (val <= MAXTAGGED && val >= -MAXTAGGED-1) /* No overflow */
        return taskData->saveVec.push(TAGGED(val));

    POLYUNSIGNED uval = val < 0 ? -val : val;
#ifdef HAVE_GMP_H
    Handle y = alloc_and_save(taskData, WORDS(sizeof(mp_limb_t)), ((val < 0) ? F_NEGATIVE_BIT : 0)| F_BYTE_OBJ);
    mp_limb_t *v = DEREFLIMBHANDLE(y);
    *v = uval;
#else
    Handle y = alloc_and_save(taskData, 1, ((val < 0) ? F_NEGATIVE_BIT : 0)| F_BYTE_OBJ);
    byte *v = DEREFBYTEHANDLE(y);
    for (POLYUNSIGNED i = 0; uval != 0; i++)
    {
        v[i] = (byte)(uval & 0xff);
        uval >>= 8;
    }
#endif
    return y;
}

Handle Make_unsigned(TaskData *taskData, POLYUNSIGNED uval)
/* Called from routines in the run-time system to generate an arbitrary
   precision integer from an unsigned value. */
{
    if (uval <= MAXTAGGED) return taskData->saveVec.push(TAGGED(uval));
 #ifdef HAVE_GMP_H
    Handle y = alloc_and_save(taskData, WORDS(sizeof(mp_limb_t)), F_BYTE_OBJ);
    mp_limb_t *v = DEREFLIMBHANDLE(y);
    *v = uval;
#else
    Handle y = alloc_and_save(taskData, 1, F_BYTE_OBJ);
    byte *v = DEREFBYTEHANDLE(y);
    for (POLYUNSIGNED i = 0; uval != 0; i++)
    {
        v[i] = (byte)(uval & 0xff);
        uval >>= 8;
    }
#endif
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

    Handle y = alloc_and_save(taskData, words, F_BYTE_OBJ);

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

Handle neg_longc(TaskData *taskData, Handle x)
{
    if (IS_INT(DEREFWORD(x)))
    {
        POLYSIGNED s = UNTAGGED(DEREFWORD(x));
        if (s != -MAXTAGGED-1) // If it won't overflow
            return taskData->saveVec.push(TAGGED(-s));
    }

    // Either overflow or long argument - convert to long form.
#if HAVE_GMP_H
    PolyWord    x_extend[1+WORDS(sizeof(mp_limb_t))];
#else
    PolyWord    x_extend[2];
#endif
    SaveVecEntry x_extend_addr = SaveVecEntry(PolyWord::FromStackAddr(&(x_extend[1])));
    Handle x_ehandle = &x_extend_addr;

    int sign_x;
    Handle long_x = get_long(x, x_ehandle, &sign_x);

#ifdef HAVE_GMP_H
    POLYUNSIGNED lx = numLimbs(DEREFWORD(long_x))*sizeof(mp_limb_t);
#else
    /* Get length of arg. */
    POLYUNSIGNED lx = get_length(DEREFWORD(long_x));
#endif
    Handle long_y = alloc_and_save(taskData, WORDS(lx), F_MUTABLE_BIT|F_BYTE_OBJ);
    byte *v = DEREFBYTEHANDLE(long_y);
    memcpy(v, DEREFBYTEHANDLE(long_x), lx);
#ifndef HAVE_GMP_H
    // Make sure the last word is zero.  We may have unused bytes there.
    memset(v+lx, 0, WORDS(lx)*sizeof(PolyWord)-lx);
#endif

    /* Return the value with the sign changed. */
    return make_canonical(taskData, long_y, sign_x ^ -1);
} /* neg_longc */

#ifdef HAVE_GMP_H
static Handle add_unsigned_long(TaskData *taskData, Handle x, Handle y, int sign)
{
    /* find the longer number */
    mp_size_t lx = numLimbs(DEREFWORD(x));
    mp_size_t ly = numLimbs(DEREFWORD(y));
    mp_limb_t *u; /* limb-pointer for longer number  */
    mp_limb_t *v; /* limb-pointer for shorter number */
    Handle z;
    mp_size_t lu;   /* length of u in limbs */
    mp_size_t lv;   /* length of v in limbs */

    if (lx < ly)
    {
        // Get result vector. It must be 1 limb longer than u
        // to have space for any carry.
        z = alloc_and_save(taskData, WORDS((ly+1)*sizeof(mp_limb_t)), F_MUTABLE_BIT|F_BYTE_OBJ);

        /* now safe to dereference pointers */
        u = DEREFLIMBHANDLE(y); lu = ly;
        v = DEREFLIMBHANDLE(x); lv = lx;
    }
    else
    {
        // Get result vector. It must be 1 limb longer than u
        // to have space for any carry.
        z = alloc_and_save(taskData, WORDS((lx+1)*sizeof(mp_limb_t)), F_MUTABLE_BIT|F_BYTE_OBJ);

        /* now safe to dereference pointers */
        u = DEREFLIMBHANDLE(x); lu = lx;
        v = DEREFLIMBHANDLE(y); lv = ly;
    }

    mp_limb_t *w = DEREFLIMBHANDLE(z);
    // Do the addition.
    mp_limb_t carry = 0;
    if (lv != 0) carry = mpn_add_n(w, u, v, lv);
    // Add the carry to the rest of the longer number.
    if (lu != lv) carry = mpn_add_1(w+lv, u+lv, lu-lv, carry);
    // Put the remaining carry in the final limb.
    w[lu] = carry;
    return make_canonical(taskData, z, sign);
}
#else
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
        // to have space for any carry.
        z = alloc_and_save(taskData, WORDS(ly+1), F_MUTABLE_BIT|F_BYTE_OBJ);

        /* now safe to dereference pointers */
        u = DEREFBYTEHANDLE(y); lu = ly;
        v = DEREFBYTEHANDLE(x); lv = lx;
    }

    else
    {
        // Get result vector. It must be 1 byte longer than u
        // to have space for any carry, plus one byte for the sign.
        z = alloc_and_save(taskData, WORDS(lx+2), F_MUTABLE_BIT|F_BYTE_OBJ);

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
#endif

#ifdef HAVE_GMP_H
static Handle sub_unsigned_long(TaskData *taskData, Handle x, Handle y, int sign)
{
    mp_limb_t *u; /* limb-pointer alias for larger number  */
    mp_limb_t *v; /* limb-pointer alias for smaller number */
    mp_size_t lu;   /* length of u in limbs */
    mp_size_t lv;   /* length of v in limbs */
    Handle z;

    /* get the larger argument into ``u'' */
    /* This is necessary so that we can discard */
    /* the borrow at the end of the subtraction */
    mp_size_t lx = numLimbs(DEREFWORD(x));
    mp_size_t ly = numLimbs(DEREFWORD(y));

    // Find the larger number.  Check the lengths first and if they're equal the values.
    int res;
    if (lx < ly) res = -1;
    else if (lx > ly) res = 1;
    else res = mpn_cmp(DEREFLIMBHANDLE(x), DEREFLIMBHANDLE(y), lx);

    // If they're equal the result is zero.
    if (res == 0) return taskData->saveVec.push(TAGGED(0)); /* They are equal */

    if (res < 0)
    {
        sign ^= -1; /* swap sign of result */
        z = alloc_and_save(taskData, WORDS(ly*sizeof(mp_limb_t)), F_MUTABLE_BIT|F_BYTE_OBJ);

        /* now safe to dereference pointers */
        u = DEREFLIMBHANDLE(y); lu = ly;
        v = DEREFLIMBHANDLE(x); lv = lx;
    }
    else
    {
        z = alloc_and_save(taskData, WORDS(lx*sizeof(mp_limb_t)), F_MUTABLE_BIT|F_BYTE_OBJ);

        /* now safe to dereference pointers */
        u = DEREFLIMBHANDLE(x); lu = lx;
        v = DEREFLIMBHANDLE(y); lv = ly;
    }

    mp_limb_t *w = DEREFLIMBHANDLE(z);
    // Do the subtraction.
    mp_limb_t borrow = 0;
    if (lv != 0) borrow = mpn_sub_n(w, u, v, lv);
    // Subtract the borrow from the rest of the longer number.
    if (lu != lv) borrow = mpn_sub_1(w+lv, u+lv, lu-lv, borrow);
    return make_canonical(taskData, z, sign);
}
#else
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
        z = alloc_and_save(taskData, WORDS(ly+1), F_MUTABLE_BIT|F_BYTE_OBJ);


        /* now safe to dereference pointers */
        u = DEREFBYTEHANDLE(y); lu = ly;
        v = DEREFBYTEHANDLE(x); lv = lx;
    }
    else if (ly < lx)
    {
        z = alloc_and_save(taskData, WORDS(lx+1), F_MUTABLE_BIT|F_BYTE_OBJ);

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
            z = alloc_and_save(taskData, WORDS(ly+1), F_MUTABLE_BIT|F_BYTE_OBJ);

            /* now safe to dereference pointers */
            u = DEREFBYTEHANDLE(y); lu = ly;
            v = DEREFBYTEHANDLE(x); lv = lx;
        }
        else
        {
            z = alloc_and_save(taskData, WORDS(lx+1), F_MUTABLE_BIT|F_BYTE_OBJ);

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
#endif

Handle add_longc(TaskData *taskData, Handle y, Handle x)
{
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

#if HAVE_GMP_H
    PolyWord    x_extend[1+WORDS(sizeof(mp_limb_t))];
    PolyWord    y_extend[1+WORDS(sizeof(mp_limb_t))];
#else
    PolyWord    x_extend[2], y_extend[2];
#endif
    SaveVecEntry x_extend_addr = SaveVecEntry(PolyWord::FromStackAddr(&(x_extend[1])));
    Handle x_ehandle = &x_extend_addr;
    SaveVecEntry y_extend_addr = SaveVecEntry(PolyWord::FromStackAddr(&(y_extend[1])));
    Handle y_ehandle = &y_extend_addr;

    /* Either overflow or long arguments - convert to long form */
    int sign_x, sign_y;
    Handle long_x = get_long(x, x_ehandle, &sign_x);
    Handle long_y = get_long(y, y_ehandle, &sign_y);

    /* Work out whether to add or subtract */
    if ((sign_y ^ sign_x) >= 0) /* signs the same? */
        /* sign(x) * (abs(x) + abs(y)) */
        return add_unsigned_long(taskData, long_x, long_y, sign_x);
    else
        /* sign(x) * (abs(x) - abs(y)) */
        return sub_unsigned_long(taskData, long_x, long_y, sign_x);
} /* add_longc */

Handle sub_longc(TaskData *taskData, Handle y, Handle x)
{
    if (IS_INT(DEREFWORD(x)) &&
        IS_INT(DEREFWORD(y))) /* Both short */
    {
    /* The easiest way to do the subtraction is simply *x-*y+1, but that
        makes it more difficult to check for overflow. */
        POLYSIGNED t = UNTAGGED(DEREFWORD(x)) - UNTAGGED(DEREFWORD(y));
        if (t <= MAXTAGGED && t >= -MAXTAGGED-1) /* No overflow */
            return taskData->saveVec.push(TAGGED(t));
    }

#if HAVE_GMP_H
    PolyWord    x_extend[1+WORDS(sizeof(mp_limb_t))];
    PolyWord    y_extend[1+WORDS(sizeof(mp_limb_t))];
#else
    PolyWord    x_extend[2], y_extend[2];
#endif
    SaveVecEntry x_extend_addr = SaveVecEntry(PolyWord::FromStackAddr(&(x_extend[1])));
    Handle x_ehandle = &x_extend_addr;
    SaveVecEntry y_extend_addr = SaveVecEntry(PolyWord::FromStackAddr(&(y_extend[1])));
    Handle y_ehandle = &y_extend_addr;

    /* Either overflow or long arguments. */
    int sign_x, sign_y;
    Handle long_x = get_long(x, x_ehandle, &sign_x); /* Convert to long form */
    Handle long_y = get_long(y, y_ehandle, &sign_y);

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


Handle mult_longc(TaskData *taskData, Handle y, Handle x)
{

#if HAVE_GMP_H
    PolyWord    x_extend[1+WORDS(sizeof(mp_limb_t))];
    PolyWord    y_extend[1+WORDS(sizeof(mp_limb_t))];
#else
    PolyWord    x_extend[2], y_extend[2];
#endif
    SaveVecEntry x_extend_addr = SaveVecEntry(PolyWord::FromStackAddr(&(x_extend[1])));
    Handle x_ehandle = &x_extend_addr;
    SaveVecEntry y_extend_addr = SaveVecEntry(PolyWord::FromStackAddr(&(y_extend[1])));
    Handle y_ehandle = &y_extend_addr;

    /* Either overflow or long arguments. */
    int sign_x, sign_y;
    Handle long_x = get_long(x, x_ehandle, &sign_x); /* Convert to long form */
    Handle long_y = get_long(y, y_ehandle, &sign_y);

#if HAVE_GMP_H
    mp_size_t lx = numLimbs(DEREFWORD(long_x));
    mp_size_t ly = numLimbs(DEREFWORD(long_y));

    // Check for zero args.
    if (lx == 0 || ly == 0) return taskData->saveVec.push(TAGGED(0));

    Handle z = alloc_and_save(taskData, WORDS((lx+ly)*sizeof(mp_limb_t)), F_MUTABLE_BIT|F_BYTE_OBJ);
    mp_limb_t *w = DEREFLIMBHANDLE(z);
    mp_limb_t *u = DEREFLIMBHANDLE(long_x), *v = DEREFLIMBHANDLE(long_y);

    // The first argument must be the longer.
    if (lx < ly) mpn_mul(w, v, ly, u, lx);
    else mpn_mul(w, u, lx, v, ly);

    return make_canonical(taskData, z, sign_x ^ sign_y);

#else
    /* Get lengths of args. */
    POLYUNSIGNED lx = get_length(DEREFWORD(long_x));
    POLYUNSIGNED ly = get_length(DEREFWORD(long_y));

    // Check for zero args.
    if (lx == 0 || ly == 0) return taskData->saveVec.push(TAGGED(0));

    /* Get space for result */
    Handle long_z = alloc_and_save(taskData, WORDS(lx+ly+1), F_MUTABLE_BIT|F_BYTE_OBJ);

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
#endif
} /* mult_long */

#ifndef HAVE_GMP_H
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
#endif

Handle div_longc(TaskData *taskData, Handle y, Handle x)
{
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

#if HAVE_GMP_H
    PolyWord    x_extend[1+WORDS(sizeof(mp_limb_t))];
    PolyWord    y_extend[1+WORDS(sizeof(mp_limb_t))];
#else
    PolyWord    x_extend[2], y_extend[2];
#endif
    SaveVecEntry x_extend_addr = SaveVecEntry(PolyWord::FromStackAddr(&(x_extend[1])));
    Handle x_ehandle = &x_extend_addr;
    SaveVecEntry y_extend_addr = SaveVecEntry(PolyWord::FromStackAddr(&(y_extend[1])));
    Handle y_ehandle = &y_extend_addr;

    int sign_x, sign_y;
    Handle long_x = get_long(x, x_ehandle, &sign_x);
    Handle long_y = get_long(y, y_ehandle, &sign_y);

#ifdef HAVE_GMP_H
    /* Get lengths of args. */
    mp_size_t lx = numLimbs(DEREFWORD(long_x));
    mp_size_t ly = numLimbs(DEREFWORD(long_y));

    // If length of v is zero raise divideerror.
    if (ly == 0) raise_exception0(taskData, EXC_divide);

    if (lx < ly) /* When x < y quotient must be zero. */
        return taskData->saveVec.push(TAGGED(0));

    Handle long_z = alloc_and_save(taskData, WORDS((lx-ly+1)*sizeof(mp_limb_t)), F_MUTABLE_BIT|F_BYTE_OBJ);
    mp_limb_t *w = DEREFLIMBHANDLE(long_z);
    mp_limb_t *u = DEREFLIMBHANDLE(long_x), *v = DEREFLIMBHANDLE(long_y);

    // Allocate temporary space for the remainder.
    mp_limb_t *remainder = (mp_limb_t*)calloc(ly, sizeof(mp_limb_t));

    // Do the division.
    mpn_tdiv_qr(w, remainder, 0, u, lx, v, ly);
    free(remainder);

    return make_canonical(taskData, long_z, sign_x ^ sign_y);

#else
    /* Get lengths of args. */
    POLYUNSIGNED lx = get_length(DEREFWORD(long_x));
    POLYUNSIGNED ly = get_length(DEREFWORD(long_y));

    /* If length of v is zero raise divideerror */
    if (ly == 0) raise_exception0(taskData, EXC_divide);

    /* Get quotient */
    if (lx < ly) /* When x < y quotient must be zero. */
        return taskData->saveVec.push(TAGGED(0));

    long_y = copy_long(taskData, long_y,ly); /* copy in case it needs shifting */

    /* vector for result - size of x + 3 */
    Handle long_z = alloc_and_save(taskData, WORDS(lx+3+1), F_MUTABLE_BIT|F_BYTE_OBJ);

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
#endif
} /* div_longc */

Handle rem_longc(TaskData *taskData, Handle y, Handle x)
{
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

#if HAVE_GMP_H
    PolyWord    x_extend[1+WORDS(sizeof(mp_limb_t))];
    PolyWord    y_extend[1+WORDS(sizeof(mp_limb_t))];
#else
    PolyWord    x_extend[2], y_extend[2];
#endif
    SaveVecEntry x_extend_addr = SaveVecEntry(PolyWord::FromStackAddr(&(x_extend[1])));
    Handle x_ehandle = &x_extend_addr;
    SaveVecEntry y_extend_addr = SaveVecEntry(PolyWord::FromStackAddr(&(y_extend[1])));
    Handle y_ehandle = &y_extend_addr;

    int sign_x, sign_y;
    Handle long_x = get_long(x, x_ehandle, &sign_x);
    Handle long_y = get_long(y, y_ehandle, &sign_y);

#ifdef HAVE_GMP_H
    /* Get lengths of args. */
    mp_size_t lx = numLimbs(DEREFWORD(long_x));
    mp_size_t ly = numLimbs(DEREFWORD(long_y));

    // If length of v is zero raise divideerror.
    if (ly == 0) raise_exception0(taskData, EXC_divide);
    if (lx < ly) return x; /* When x < y remainder is x. */

    Handle long_z = alloc_and_save(taskData, WORDS(ly*sizeof(mp_limb_t)), F_MUTABLE_BIT|F_BYTE_OBJ);
    mp_limb_t *w = DEREFLIMBHANDLE(long_z);
    mp_limb_t *u = DEREFLIMBHANDLE(long_x), *v = DEREFLIMBHANDLE(long_y);

    // Allocate temporary space for the quotient.
    mp_limb_t *quotient = (mp_limb_t*)calloc(lx-ly+1, sizeof(mp_limb_t));

    // Do the division.
    mpn_tdiv_qr(quotient, w, 0, u, lx, v, ly);
    free(quotient);

    return make_canonical(taskData, long_z, sign_x /* Same sign as dividend */);

#else

    /* Get lengths of args. */
    POLYUNSIGNED lx = get_length(DEREFWORD(long_x));
    POLYUNSIGNED ly = get_length(DEREFWORD(long_y));

    /* If length of y is zero raise divideerror */
    if (ly == 0) raise_exception0(taskData, EXC_divide);

    /* Get remainder */
    if (lx < ly) return x; /* When x < y remainder is x. */

    POLYUNSIGNED i;
    Handle long_z;

    /* copy in case it needs shifting */
    long_y = copy_long(taskData, long_y, ly);

    /* vector for result - size of x + 3 */
    long_z = alloc_and_save(taskData, WORDS(lx+3+1), F_MUTABLE_BIT|F_BYTE_OBJ);

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
#endif
} /* rem_longc */

/* compare_unsigned is passed LONG integers only */
static int compare_unsigned(Handle x, Handle y)
{
#ifdef HAVE_GMP_H
    mp_size_t lx = numLimbs(DEREFWORD(x));
    mp_size_t ly = numLimbs(DEREFWORD(y));

    if (lx != ly)  /* u > v if u longer than v */
    {
        return (lx > ly ? 1 : -1);
    }
    return mpn_cmp(DEREFLIMBHANDLE(x), DEREFLIMBHANDLE(y), lx);
#else
    /* First look at the lengths */
    POLYUNSIGNED lx = get_length(DEREFWORD(x));
    POLYUNSIGNED ly = get_length(DEREFWORD(y));

    if (lx != ly)  /* u > v if u longer than v */
    {
        return (lx > ly ? 1 : -1);
    }

    // Same length - look at the values. */
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
#endif
}

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
    long_x = get_long(x, taskData->x_ehandle, &sign_x);
    long_y = get_long(y, taskData->y_ehandle, &sign_y);

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
#ifdef HAVE_GMP_H
        POLYUNSIGNED lx = numLimbs(DEREFWORD(x)) * sizeof(mp_limb_t);
        POLYUNSIGNED ly = numLimbs(DEREFWORD(y)) * sizeof(mp_limb_t);
#else
        POLYUNSIGNED lx = get_length(DEREFWORD(x));
        POLYUNSIGNED ly = get_length(DEREFWORD(y));
#endif

        /* Make ``u'' the longer. */
        if (lx < ly)
        {
            // Get result vector. There can't be any carry at the end so
            // we just need to make this as large as the larger number.
            z = alloc_and_save(taskData, WORDS(ly), F_MUTABLE_BIT|F_BYTE_OBJ);

            /* now safe to dereference pointers */
            u = DEREFBYTEHANDLE(y); lu = ly;
            v = DEREFBYTEHANDLE(x); lv = lx;
            signU = signY; signV = signX;
        }

        else
        {
            /* Get result vector. */
            z = alloc_and_save(taskData, WORDS(lx+1), F_MUTABLE_BIT|F_BYTE_OBJ);

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

Handle and_longc(TaskData *taskData, Handle y, Handle x)
{
    if (IS_INT(DEREFWORD(x)) &&
        IS_INT(DEREFWORD(y))) /* Both short */
    {
       /* There's no problem with overflow so we can just AND together
           the values. */
        POLYSIGNED t = UNTAGGED(DEREFWORD(x)) & UNTAGGED(DEREFWORD(y));
        return taskData->saveVec.push(TAGGED(t));
    }

#if HAVE_GMP_H
    PolyWord    x_extend[1+WORDS(sizeof(mp_limb_t))];
    PolyWord    y_extend[1+WORDS(sizeof(mp_limb_t))];
#else
    PolyWord    x_extend[2], y_extend[2];
#endif
    SaveVecEntry x_extend_addr = SaveVecEntry(PolyWord::FromStackAddr(&(x_extend[1])));
    Handle x_ehandle = &x_extend_addr;
    SaveVecEntry y_extend_addr = SaveVecEntry(PolyWord::FromStackAddr(&(y_extend[1])));
    Handle y_ehandle = &y_extend_addr;

    // Convert to long form.
    int sign_x, sign_y;
    Handle long_x = get_long(x, x_ehandle, &sign_x);
    Handle long_y = get_long(y, y_ehandle, &sign_y);

    return logical_long(taskData, long_x, long_y, sign_x, sign_y, doAnd);
}

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
    long_x = get_long(x, taskData->x_ehandle , &sign_x); /* Convert to long form */
    long_y = get_long(y, taskData->y_ehandle , &sign_y);

    return logical_long(taskData, long_x, long_y, sign_x, sign_y, doOr);
}

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
    long_x = get_long(x, taskData->x_ehandle , &sign_x); /* Convert to long form */
    long_y = get_long(y, taskData->y_ehandle , &sign_y);

    return logical_long(taskData, long_x, long_y, sign_x, sign_y, doXor);
}

// Convert a long precision value to integer
double get_C_real(TaskData *, PolyWord x)
{
    if (IS_INT(x)) {
        POLYSIGNED t = UNTAGGED(x);
        return (double)t;
    }
    byte *u = (byte *)(x.AsObjPtr());
    POLYUNSIGNED lx = OBJECT_LENGTH(x)*sizeof(PolyWord);
    double acc = 0;
    for( ; lx > 0; lx--) {
        acc = acc * 256 + (double)u[lx-1];
    }
    if (OBJ_IS_NEGATIVE(GetLengthWord(x)))
        return -acc;
    else return acc;
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

#ifdef HAVE_GMP_H
    // It may be big- or little-endian.
    POLYUNSIGNED r = (POLYUNSIGNED)*DEREFLIMBHANDLE(x);
#else
    // Little-endian.
    byte    *u = DEREFBYTEHANDLE(x);
    POLYUNSIGNED r = 0;
    for (unsigned i=0; i < sizeof(PolyWord); i++)
    {
        r |= (POLYUNSIGNED)u[i] << (8*i);
    }
#endif
    if (OBJ_IS_NEGATIVE(x->Word().AsObjPtr()->LengthWord()))
        r = 0-r; // Use 0-r rather than -r since it's an unsigned value.
    return taskData->saveVec.push(TAGGED(r));
}


