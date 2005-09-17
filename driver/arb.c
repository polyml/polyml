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
Thanks to Mike Crawley at Brunel for some bug fixes.
*/

/* Tidied up 27/10/93 SPF */
/* Types straightened out - uses (word *) not (byte *) wherever possible */
/* 4/11/93 SPF Now uses Handles  */
/* 21/1/94 SPF bugfix to sub_unsigned_long (changes sign when x < y) */ 

#include <stdio.h>
#include <assert.h>

#include "globals.h"
#include "sys.h"

/* added 26/10/93 SPF */
#include "run_time.h"
#include "mmap.h"
#include "arb.h"

/* Buffers to extend short values - assumes 4 bytes per word. */

/* The "value in the Poly" Heap */
static word x_extend[2] = {1, 0}; /* length word, expanded value */
static word y_extend[2] = {1, 0};

/* "the save_vec cell" */
static word *x_extend_addr = &(x_extend[1]);
static word *y_extend_addr = &(y_extend[1]);
static word *zero_word     = (word *)TAGGED(0);

/* The "pointer to the save_vec cell" */
static Handle xHandle    = (Handle) &x_extend_addr;
static Handle yHandle    = (Handle) &y_extend_addr;
static Handle zeroHandle = (Handle) &zero_word;

#define MAX_LENGTH (1<<24)  /* only 24 bit length word */


/******************************************************************************/
/*                                                                            */
/*      get_length - utility function                                         */
/*                                                                            */
/******************************************************************************/
/* Returns the length of the argument with trailing zeros removed. */

static int get_length(word *x)
{
    byte *u = (byte *)x;
    int  lu  = OBJ_OBJECT_LENGTH(x[-1])*sizeof(word);
    
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
unsigned get_C_ulong(word *number)
{
  
  if ( IS_INT(number) )
  {
    word i = UNTAGGED(number);
    
    if ( i < 0 ) raise_exception0 ( EXC_size );

    return (unsigned) i;
  }
  else
  {
    int length = get_length(number);
    unsigned c = 0;
    
    if (OBJ_IS_NEGATIVE(number[-1])) raise_exception0 ( EXC_size );
    
    if ( length > sizeof(word) ) raise_exception0 ( EXC_size );
    
    while ( length-- ) c = (c << 8) | ((byte *) number)[length];
  
    return c;

  }
}

/******************************************************************************/
/*                                                                            */
/*      get_C_long - called by runtime system                                 */
/*                                                                            */
/******************************************************************************/
/* indirection removed 31/10/93 SPF */
int get_C_long(word *number)
{
  
  if ( IS_INT(number) )
  {
    return UNTAGGED(number);
  }
  else
  {
    int sign   = OBJ_IS_NEGATIVE(number[-1]) ? -1 : 0;
    int length = get_length(number);
    unsigned c = 0;

    if ( length > sizeof(word) ) raise_exception0 ( EXC_size );
    
    while ( length-- )
    {
       c = (c << 8) | ((byte *) number)[length];
    }
  
    if ( sign == 0 && c <  (unsigned) 0x80000000 ) return   (int)c;
    if ( sign != 0 && c <= (unsigned) 0x80000000 ) return -((int)c);
    
    raise_exception0 ( EXC_size );
    /*NOTREACHED*/
  }
}

/******************************************************************************/
/*                                                                            */
/*      get_C_short - called by runtime system                                */
/*                                                                            */
/******************************************************************************/
/* indirection removed 31/10/93 SPF */
short get_C_short(word *number)
{
  int i = get_C_long ( number );

  if ( i <= 32767 && i >= -32768 ) return i;
  
  raise_exception0 ( EXC_size );
  /*NOTREACHED*/
}

/******************************************************************************/
/*                                                                            */
/*      get_C_ushort - called by runtime system                               */
/*                                                                            */
/******************************************************************************/
/* indirection removed 31/10/93 SPF */
unsigned short get_C_ushort(word *number)
{
  unsigned u = get_C_ulong ( number );

  if ( u <= 65535 ) return u;
  
  raise_exception0 ( EXC_size );
  /*NOTREACHED*/
}

/* Get a arbitrary precision value as a pair of words.
   At present this is used to extra a 64-bit quantity as two
   words. */
void get_C_pair(word *number, unsigned int *pHi, unsigned int *pLo)
{
	if ( IS_INT(number) )
	{
		/* It will fit in the low-order word. */
		*pLo = UNTAGGED(number);
		*pHi = 0;
		return;
	}
	else
	{
		int length = get_length(number);
		int	i;
		unsigned c;

		if (OBJ_IS_NEGATIVE(number[-1])) raise_exception0(EXC_size);
		if ( length > 8 ) raise_exception0(EXC_size);

		/* Low-order word. */
		if (length > 4) i = 4; else i = length;
		c = 0;
		while (i--) c = (c << 8) | ((byte *) number)[i];
		*pLo = c;

		/* High-order word. */
		i = length;
		c = 0;
		while (i-- > 4) c = (c << 8) | ((byte *) number)[i];
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
static Handle get_long(Handle x, Handle extend, int *sign)
{
   if (IS_INT(DEREFWORDHANDLE(x)))
   { /* Short form - put it in the pre-allocated temporary. (HACK) */
	   int i;
     int x_v = UNTAGGED(DEREFHANDLE(x));
     byte *u = DEREFBYTEHANDLE(extend);
 
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
	 for (i = 0; i < sizeof(word); i++)
	 {
		u[i] = x_v & 0xff;
		x_v = x_v >> 8;
	 }
     return extend;
  }
  else
  { /* Long-form - x is an address. */
    *sign = OBJ_IS_NEGATIVE(DEREFWORDHANDLE(x)[-1]) ? -1 : 0;
    return x;
  }
} /* get_long */

/******************************************************************************/
/*                                                                            */
/*      copy_long - utility function                                          */
/*                                                                            */
/******************************************************************************/
static Handle copy_long(Handle x, int lx)
{
  Handle y = alloc_and_save(OBJ_MUTABLE_BIT+OBJ_BYTE_BIT+WORDS(lx));
  
  { /* copy the bytes */
    byte *u = DEREFBYTEHANDLE(x);  
    byte *v = DEREFBYTEHANDLE(y);  
    int i;
  
    for ( i = 0; i < lx; i++ ) 
    {
      v[i] = u[i];
    }
   }
   
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
   
static Handle make_canonical(Handle x, int sign)
{   /* get length in BYTES */
    int size = get_length(DEREFWORDHANDLE(x));
     
    /* We can use the short representation if it will fit in 4 bytes. */
    if (size <= sizeof(word))
     {
        /* Convert the digits. */
        byte    *u = DEREFBYTEHANDLE(x);
		unsigned r = 0;
		int i;
		for (i=0; i < sizeof(word); i++)
		{
			r |= u[i] << (8*i);
		}
              
        /* Check for MAXTAGGED+1 before subtraction
           in case MAXTAGGED is 0x7fffffff */
           
        if (r <= MAXTAGGED || (r == MAXTAGGED+1 && sign < 0))
          {
            if (sign < 0)
              {
                return push_to_save_vec(TAGGED(-(word)r));
              }
            else
              {
                return push_to_save_vec(TAGGED((word)r));
              }
         }
    }
    
    /* The length word of the object is changed to reflect the new length.
       This is safe because any words thrown away must be zero. */
       
    if (sign < 0) 
      {
        DEREFWORDHANDLE(x)[-1] = OBJ_NEGATIVE_BIT + OBJ_BYTE_BIT + WORDS(size);
      }
    else
      {
        DEREFWORDHANDLE(x)[-1] = OBJ_BYTE_BIT + WORDS(size);
      }
    
    return x;
} /* make_canonical */


/******************************************************************************/
/*                                                                            */
/*      Make_XXX functions (used in runtime system)                           */
/*                                                                            */
/******************************************************************************/
Handle Make_arbitrary_precision(int val)
/* Called from routines in the run-time system to generate an arbitrary
   precision integer from a word value. */
{
    Handle y;
    unsigned uval;

    if (val <= MAXTAGGED && val >= -MAXTAGGED-1) /* No overflow */
        return push_to_save_vec(TAGGED(val));

    if (val < 0)
    {
      y = alloc_and_save(OBJ_NEGATIVE_BIT+OBJ_BYTE_BIT+1);
      uval = -val;
    }
    else 
    {
      y = alloc_and_save(OBJ_BYTE_BIT+1);
      uval = val;
    }

    { byte *v = DEREFBYTEHANDLE(y);
      int i;
      for (i = 0; uval != 0; i++)
        {
          v[i] = uval & 0xff;
          uval >>= 8;
        }
    }
    
    return y;
}

Handle Make_unsigned(unsigned uval)
/* Called from routines in the run-time system to generate an arbitrary
   precision integer from an unsigned value. */
{
    Handle y;
    
    if (uval <= MAXTAGGED) return push_to_save_vec(TAGGED(uval));

    y = alloc_and_save(OBJ_BYTE_BIT+1);

    { byte *v = DEREFBYTEHANDLE(y);
      int i;
      for (i = 0; uval != 0; i++)
        {
          v[i] = uval & 0xff;
          uval >>= 8;
        }
    }
    
    return y;
}

/* Creates an arbitrary precision number from two words.
   At present this is used for 64-bit quantities. */
Handle Make_arb_from_pair(unsigned hi, unsigned lo)
{
    Handle y;
	byte	*v;
	int	i;
	/* If the high word is zero we can use either the tagged short
	   form or a single word. */
	if (hi == 0) return Make_unsigned(lo);

    y = alloc_and_save(OBJ_BYTE_BIT+(8/sizeof(word)));

    v = DEREFBYTEHANDLE(y);
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
Handle Make_arb_from_pair_scaled(unsigned hi, unsigned lo, unsigned scale)
{
	/* We might be able to compute the number as a 64 bit quantity and
	   then convert it but this is probably more portable. It does risk
	   overflowing the save vector. */
	Handle hHi = Make_unsigned(hi);
	Handle hLo = Make_unsigned(lo);
	Handle hScale = Make_unsigned(scale);
	return add_longc(mult_longc(hHi, hScale), hLo);
}

/******************************************************************************/
/*                                                                            */
/*      neg_longc - called from assembly code                                  */
/*                                                                            */
/******************************************************************************/
Handle neg_longc(Handle x)
{
  Handle long_x, long_y;
  int sign_x, lx;
  
  if (IS_INT(DEREFWORDHANDLE(x)) && 
      (word)(DEREFHANDLE(x)) != TAGGED(-MAXTAGGED-1) /* -maxint */)
      /* Short and no overflow. */
    {
      return push_to_save_vec(TAGGED(- UNTAGGED(DEREFWORDHANDLE(x))));
    }
    
  /* Either overflow or long argument - convert to long form */
  long_x = get_long(x, xHandle, &sign_x);
  
  /* Get length of arg. */
  lx = get_length(DEREFWORDHANDLE(long_x));
  
  /* copy the argument (so we can return it) */
  long_y = copy_long(long_x,lx);
  
  /* Return the value with the sign changed. */
  return make_canonical(long_y, sign_x ^ -1);
} /* neg_longc */


/******************************************************************************/
/*                                                                            */
/*      add_unsigned_long - utility function                                  */
/*            computes sign * (abs(x) + abs(y))                               */
/*                                                                            */
/******************************************************************************/
static Handle add_unsigned_long(Handle x, Handle y, int sign)
{
    byte *u; /* byte-pointer for longer number  */
    byte *v; /* byte-pointer for shorter number */
    Handle z;
    
    int lu;   /* length of u in bytes */
    int lv;   /* length of v in bytes */
    
    { /* find the longer number */
      int lx = get_length(DEREFWORDHANDLE(x));
      int ly = get_length(DEREFWORDHANDLE(y));
      
      /* Make ``u'' the longer. */
      if (lx < ly)
        {
          /* Get result vector. It must be 1 byte longer than u */
          /* to have space for any carry.                       */
          z = alloc_and_save(OBJ_MUTABLE_BIT+OBJ_BYTE_BIT+WORDS(ly+1));
          
          /* now safe to dereference pointers */
          u = DEREFBYTEHANDLE(y); lu = ly;
          v = DEREFBYTEHANDLE(x); lv = lx;
        }
          
      else
        {
         /* Get result vector. It must be 1 byte longer than u */
         /* to have space for any carry.                       */
         z = alloc_and_save(OBJ_MUTABLE_BIT+OBJ_BYTE_BIT+WORDS(lx+1));
         
         /* now safe to dereference pointers */
          u = DEREFBYTEHANDLE(x); lu = lx;
          v = DEREFBYTEHANDLE(y); lv = ly;
        }
    }
    
    { /* do the actual addition */
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
    }

    return make_canonical(z, sign);
} /* add_unsigned_long */


/******************************************************************************/
/*                                                                            */
/*      sub_unsigned_long - utility function                                  */
/*            computes sign * (abs(x) - abs(y))                               */
/*                                                                            */
/******************************************************************************/
static Handle sub_unsigned_long(Handle x, Handle y, int sign)
{
    byte *u; /* byte-pointer alias for larger number  */
    byte *v; /* byte-pointer alias for smaller number */
    int lu;   /* length of u in bytes */
    int lv;   /* length of v in bytes */
    Handle z;

    {/* get the larger argument into ``u'' */
     /* This is necessary so that we can discard */
     /* the borrow at the end of the subtraction */
      int lx = get_length(DEREFWORDHANDLE(x));
      int ly = get_length(DEREFWORDHANDLE(y));
      
      if (lx < ly)
        {
          sign ^= -1; /* swap sign of result SPF 21/1/94 */
          z = alloc_and_save(OBJ_MUTABLE_BIT+OBJ_BYTE_BIT+WORDS(ly));
          

        /* now safe to dereference pointers */
          u = DEREFBYTEHANDLE(y); lu = ly;
          v = DEREFBYTEHANDLE(x); lv = lx;
         }
      else if (ly < lx)
        {
          z = alloc_and_save(OBJ_MUTABLE_BIT+OBJ_BYTE_BIT+WORDS(lx));
          
         /* now safe to dereference pointers */
          u = DEREFBYTEHANDLE(x); lu = lx;
          v = DEREFBYTEHANDLE(y); lv = ly;
         }
         
      else /* lx == ly */
       { /* Must look at the numbers to decide which is bigger. */
         int i = lx - 1;
         for( ; i >= 0 && DEREFBYTEHANDLE(x)[i] == DEREFBYTEHANDLE(y)[i]; i--);
         
         if (i < 0) return push_to_save_vec(TAGGED(0)); /* They are equal */
         
         if (DEREFBYTEHANDLE(x)[i] < DEREFBYTEHANDLE(y)[i])
           {
             sign ^= -1; /* swap sign of result SPF 21/1/94 */
             z = alloc_and_save(OBJ_MUTABLE_BIT+OBJ_BYTE_BIT+WORDS(ly));
          
            /* now safe to dereference pointers */
             u = DEREFBYTEHANDLE(y); lu = ly;
             v = DEREFBYTEHANDLE(x); lv = lx;
            }
         else
           {
             z = alloc_and_save(OBJ_MUTABLE_BIT+OBJ_BYTE_BIT+WORDS(lx));
          
            /* now safe to dereference pointers */
             u = DEREFBYTEHANDLE(x); lu = lx;
             v = DEREFBYTEHANDLE(y); lv = ly;
            }
      }
    }
    
    
    {
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
        
      return make_canonical(z, sign);
    }
} /* sub_unsigned_long */


/******************************************************************************/
/*                                                                            */
/*      add_longc - called from sparc_dep.c                                   */
/*                                                                            */
/******************************************************************************/
Handle add_longc(Handle y, Handle x)
{
   Handle long_x, long_y;
   int sign_x, sign_y;
   
   if (IS_INT(DEREFWORDHANDLE(x)) && IS_INT(DEREFWORDHANDLE(y)))
     {  /* Both short */
       /* The easiest way to do the addition is simply *x-1+*y, but that
          makes it more difficult to check for overflow. */
       word t = UNTAGGED(DEREFWORDHANDLE(x)) + UNTAGGED(DEREFWORDHANDLE(y));
       if (t <= MAXTAGGED && t >= -MAXTAGGED-1) /* No overflow */
         {
           return push_to_save_vec(TAGGED(t));
         }
     }
     
   /* Either overflow or long arguments - convert to long form */
   long_x = get_long(x, xHandle , &sign_x);
   long_y = get_long(y, yHandle , &sign_y);
   
   /* Work out whether to add or subtract */
   if ((sign_y ^ sign_x) >= 0) /* signs the same? */
   { /* sign(x) * (abs(x) + abs(y)) */
     return add_unsigned_long(long_x, long_y, sign_x);
   }
   else
   { /* sign(x) * (abs(x) - abs(y)) */
     return sub_unsigned_long(long_x, long_y, sign_x);
   }
} /* add_longc */


/******************************************************************************/
/*                                                                            */
/*      sub_longc - called from sparc_dep.c                                   */
/*                                                                            */
/******************************************************************************/
Handle sub_longc(Handle y, Handle x)
{
   Handle long_x, long_y;
   int sign_x, sign_y;
   
   if (IS_INT(DEREFWORDHANDLE(x)) && 
       IS_INT(DEREFWORDHANDLE(y))) /* Both short */
     {
       /* The easiest way to do the subtraction is simply *x-*y+1, but that
          makes it more difficult to check for overflow. */
       word t = UNTAGGED(DEREFWORDHANDLE(x)) - UNTAGGED(DEREFWORDHANDLE(y));
       if (t <= MAXTAGGED && t >= -MAXTAGGED-1) /* No overflow */
           return push_to_save_vec(TAGGED(t));
     }
     
   /* Either overflow or long arguments. */
   long_x = get_long(x, xHandle , &sign_x); /* Convert to long form */
   long_y = get_long(y, yHandle , &sign_y);
   
   /* If the signs are different add the two values. */
   if ((sign_y ^ sign_x) < 0) /* signs differ */
   { /* sign(x) * (abs(x) + abs(y)) */
     return add_unsigned_long(long_x, long_y, sign_x);
   }
   else
   { /* sign(x) * (abs(x) - abs(y)) */
     return sub_unsigned_long(long_x, long_y, sign_x);
   }
} /* sub_longc */


/******************************************************************************/
/*                                                                            */
/*      mult_longc - called from sparc_assembly.s                             */
/*                                                                            */
/******************************************************************************/
Handle mult_longc(Handle y, Handle x)
{
  Handle long_x, long_y, long_z;
  int lx, ly, sign_x, sign_y;
  
  /* I'm not sure how to detect overflow here - so go through the full
     routine even if both arguments are short. */
     
  /* Convert to long form */
  long_x = get_long(x, xHandle , &sign_x);
  long_y = get_long(y, yHandle , &sign_y);
  
  /* Get lengths of args. */
  lx = get_length(DEREFWORDHANDLE(long_x));
  ly = get_length(DEREFWORDHANDLE(long_y));
  
  /* Check for zero args. */
  if (lx == 0 || ly == 0)
  {
    return zeroHandle;
  }
  
  /* Get space for result */
  long_z = alloc_and_save(OBJ_MUTABLE_BIT+OBJ_BYTE_BIT+WORDS(lx+ly));
  
  {
    /* Can now load the actual addresses because they will not change now. */
    byte *u = DEREFBYTEHANDLE(long_x);
    byte *v = DEREFBYTEHANDLE(long_y);
    byte *w = DEREFBYTEHANDLE(long_z);
    int i,j;
    long r;
    
    for(i = 0; i < lx; i++)
      {
        r = 0; /* Set the carry to zero */
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
    
   return make_canonical(long_z, sign_x ^ sign_y);
 }
} /* mult_long */


/******************************************************************************/
/*                                                                            */
/*      div_unsigned_long - utility function                                  */
/*                                                                            */
/******************************************************************************/
static void div_unsigned_long(byte *u, byte *v, byte *res, int lu, int lv)
/* Unsigned division. This is the main divide and remainder routine. The
   result is packed into ``res'' and is separated out by div and rem */
{
    int i,j;
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
      if ( bits )
      {
        r = 0;
        for(j = lv; --j >= 0; )
          {
            r |= res[j+2];
            res[j+2] = (r >> bits) & 0xff;
            r = (r & 0xff) << 8;
          }
     }
    return;
} /* div_unsigned_long */

/******************************************************************************/
/*                                                                            */
/*      div_long_c - called from sparc_assembly.s                             */
/*                                                                            */
/******************************************************************************/
Handle div_longc(Handle y, Handle x)
{
  Handle long_x, long_y;
  int lx, ly, sign_x, sign_y;
  
  if (IS_INT(DEREFWORDHANDLE(x)) &&
      IS_INT(DEREFWORDHANDLE(y))) /* Both short */
  {
    /* Raise exceptions if dividing by zero. */
    if ((word)DEREFHANDLE(y) == TAGGED(0))
    {
      raise_exception0(EXC_divide);
    }
    
    /* Only possible overflow is minint div -1 */
    if ((word)DEREFHANDLE(x) != TAGGED(-MAXTAGGED-1) ||
        (word)DEREFHANDLE(y) != TAGGED(-1))
    {
      return push_to_save_vec(TAGGED( UNTAGGED(DEREFWORDHANDLE(x)) /
                                       UNTAGGED(DEREFWORDHANDLE(y)) ));
    }
  }
  
  /* Long operands or overflow - convert to long form */
  long_x = get_long(x, xHandle , &sign_x);
  long_y = get_long(y, yHandle , &sign_y);
  
  /* Get lengths of args. */
  lx = get_length(DEREFWORDHANDLE(long_x));
  ly = get_length(DEREFWORDHANDLE(long_y));
  
  /* If length of v is zero raise divideerror */
  if (ly == 0) raise_exception0(EXC_divide);
  
  /* Get quotient */
  if (lx < ly)
  {
     /* When x < y quotient must be zero. */
     return zeroHandle;
  }
  else
  {
    int i;
    Handle long_z;
    long_y = copy_long(long_y,ly); /* copy in case it needs shifting */
    
    /* vector for result - size of x + 3 */
    long_z = alloc_and_save(OBJ_MUTABLE_BIT+OBJ_BYTE_BIT+WORDS(lx+3));
    
    div_unsigned_long
      (DEREFBYTEHANDLE(long_x), 
       DEREFBYTEHANDLE(long_y),
       DEREFBYTEHANDLE(long_z),
       lx, ly);
    
    /* Copy it down */
    for(i = 0; i <= lx-ly; i++) 
    {
      DEREFBYTEHANDLE(long_z)[i] = DEREFBYTEHANDLE(long_z)[i+ly+2];
    }
      
    /* Clear the rest */
    for(; i < lx+3; i++)
    {
      DEREFBYTEHANDLE(long_z)[i] = 0; 
    }
    
    return make_canonical(long_z, sign_x ^ sign_y);
  }
} /* div_longc */

/******************************************************************************/
/*                                                                            */
/*      rem_long_c - called from sparc_assembly.s                             */
/*                                                                            */
/******************************************************************************/
Handle rem_longc(Handle y, Handle x)
{
  Handle long_x, long_y;
  int lx, ly, sign_x, sign_y;
  
  if (IS_INT(DEREFWORDHANDLE(x)) && 
      IS_INT(DEREFWORDHANDLE(y))) /* Both short */
  {
    /* Raise exceptions if remaindering by zero. */
    if ((word)DEREFHANDLE(y) == TAGGED(0)) 
    {
      raise_exception0(EXC_divide);
    }
    
    /* Only possible overflow is minint mod -1 */
    if ((word)DEREFHANDLE(x) != TAGGED(-MAXTAGGED-1) || 
        (word)DEREFHANDLE(y) != TAGGED(-1))
    {
      return push_to_save_vec(TAGGED( UNTAGGED(DEREFWORDHANDLE(x)) %
                                      UNTAGGED(DEREFWORDHANDLE(y)) ));
    }
  }
    
  /* Long operands or overflow - convert any shorts to long form */
  long_x = get_long(x, xHandle , &sign_x);
  long_y = get_long(y, yHandle , &sign_y);

  /* Get lengths of args. */ 
  lx = get_length(DEREFWORDHANDLE(long_x));
  ly = get_length(DEREFWORDHANDLE(long_y));
  
  /* If length of y is zero raise divideerror */
  if (ly == 0) raise_exception0(EXC_divide);
  
  /* Get remainder */
  if (lx < ly)
  {
     /* When x < y remainder is x. */
     return x; /* NOT long_x, since x may be a short. SPF 1/11/95 */
  }
  else
  {
    int i;
    Handle long_z;
    
    /* copy in case it needs shifting */
    long_y = copy_long(long_y,ly);
    
    /* vector for result - size of x + 3 */
    long_z = alloc_and_save(OBJ_MUTABLE_BIT+OBJ_BYTE_BIT+WORDS(lx+3));
    
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
    
    return make_canonical(long_z, sign_x /* Same sign as dividend */ );
             /* ML says it should have same as divisor. */
  }
} /* rem_longc */


/******************************************************************************/
/*                                                                            */
/*      alloc_store_long_c - called from sparc_assembly.s                     */
/*                                                                            */
/******************************************************************************/
/* CALL_IO3(alloc_store_long_, REF, REF, REF, NOIND) */
word *alloc_store_long_c(Handle initial, Handle flags_handle, Handle size )
{
  word *vector;
  
  unsigned flags = get_C_ulong(DEREFWORDHANDLE(flags_handle));
  unsigned usize = get_C_ulong(DEREFWORDHANDLE(size));
  unsigned value; 
  
  if ( usize == 0 ) usize = 1;

  if ( usize >= MAX_LENGTH ) raise_exception0 ( EXC_size );

  vector = alloc ( usize | (flags << 24) | OBJ_MUTABLE_BIT );
  
  value = (unsigned)DEREFHANDLE(initial); /* passed by REF in case it is a pointer */
 
  if ( OBJ_IS_BYTE_OBJECT(vector[-1]) )
  {
    if ( value != TAGGED(0) ) crash ("non-zero byte segment\n");
  }
  else if ( value != 0 ) 
  {
    word *p = vector;
    
    while ( usize-- ) *p++ = value;
  }
  
  return vector;
}


/******************************************************************************/
/*                                                                            */
/*      compare_unsigned - utility function                                   */
/*                                                                            */
/******************************************************************************/
/* compare_unsigned is passed LONG integers only */
static word compare_unsigned(Handle x, Handle y)
{
    int lx, ly;

    /* First look at the lengths */
    lx = get_length(DEREFWORDHANDLE(x));
    ly = get_length(DEREFWORDHANDLE(y));
    
    if (lx != ly)  /* u > v if u longer than v */
    {
       return (lx > ly ? TAGGED(1) : TAGGED(-1));
    }
        
    { /* Same length - look at the values. */
      byte *u = DEREFBYTEHANDLE(x);
      byte *v = DEREFBYTEHANDLE(y);
      int i;
      
      for(i = lx - 1; i >= 0; i--)
      {
        if (u[i] != v[i]) 
        {
          return u[i] > v[i] ? TAGGED(1) : TAGGED(-1);
        }
      }
      /* Must be equal */
      return TAGGED(0);
    } 
} /* compare_unsigned */


/******************************************************************************/
/*                                                                            */
/*      comp_longc - called by run-time system                                */
/*                                                                            */
/******************************************************************************/
word comp_longc(Handle y, Handle x)
{
  Handle long_x, long_y;
  int sign_x, sign_y;

  if (IS_INT(DEREFWORDHANDLE(x)) &&
      IS_INT(DEREFWORDHANDLE(y))) /* Both short */
  {
    int t = UNTAGGED(DEREFHANDLE(x)) - UNTAGGED(DEREFHANDLE(y));
    
    if (t == 0) return TAGGED(0); /* equal */
    else if (t < 0) return TAGGED(-1); /* less */
    else return TAGGED(1); /* greater */
  }
   
  /* Convert to long form */
  long_x = get_long(x, xHandle, &sign_x); 
  long_y = get_long(y, yHandle, &sign_y);
  
  if (sign_x >= 0) /* x is positive */
  {
    if (sign_y >= 0) /* y also positive */ 
    {
      return compare_unsigned(long_x, long_y);
    }
    else /* y negative so x > y */
    {
      return TAGGED(1);
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
      return TAGGED(-1); 
    }
  }
} /* comp_longc */


/* logical_long.  General purpose function for binary logical operations. */
static Handle logical_long(Handle x, Handle y, int signX, int signY,
						   int(*op)(int, int))
{
    byte *u; /* byte-pointer for longer number  */
    byte *v; /* byte-pointer for shorter number */
    Handle z;
	int sign, signU, signV;
    
    int lu;   /* length of u in bytes */
    int lv;   /* length of v in bytes */
    
    { /* find the longer number */
      int lx = get_length(DEREFWORDHANDLE(x));
      int ly = get_length(DEREFWORDHANDLE(y));
      
      /* Make ``u'' the longer. */
      if (lx < ly)
        {
          /* Get result vector. There can't be any carry at the end so
		     we just need to make this as large as the larger number. */
          z = alloc_and_save(OBJ_MUTABLE_BIT+OBJ_BYTE_BIT+WORDS(ly));
          
          /* now safe to dereference pointers */
          u = DEREFBYTEHANDLE(y); lu = ly;
          v = DEREFBYTEHANDLE(x); lv = lx;
		  signU = signY; signV = signX;
        }
          
      else
        {
         /* Get result vector. */
         z = alloc_and_save(OBJ_MUTABLE_BIT+OBJ_BYTE_BIT+WORDS(lx));
         
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
      int i     = 0;
      
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
	  assert(signV == 0 || borrowV == 0);

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
	  assert(signU == 0 || borrowU == 0);
	  assert(sign == 0 || borrowW == 0);
    }

    return make_canonical(z, sign);
} /* logical_long */

static int doAnd(int i, int j)
{
	return i & j;
}

static int doOr(int i, int j)
{
	return i | j;
}

static int doXor(int i, int j)
{
	return i ^ j;
}

/******************************************************************************/
/*                                                                            */
/*      and_longc - called by run-time system                                 */
/*                                                                            */
/******************************************************************************/
Handle and_longc(Handle y, Handle x)
{
   Handle long_x, long_y;
   int sign_x, sign_y;

   if (IS_INT(DEREFWORDHANDLE(x)) && 
       IS_INT(DEREFWORDHANDLE(y))) /* Both short */
     {
       /* There's no problem with overflow so we can just AND together
	      the values. */
		word t = UNTAGGED(DEREFWORDHANDLE(x)) & UNTAGGED(DEREFWORDHANDLE(y));
		return push_to_save_vec(TAGGED(t));
     }

   /* Long arguments. */
   long_x = get_long(x, xHandle , &sign_x); /* Convert to long form */
   long_y = get_long(y, yHandle , &sign_y);

   return logical_long(long_x, long_y, sign_x, sign_y, doAnd);
}

/******************************************************************************/
/*                                                                            */
/*      or_longc - called by run-time system                                 */
/*                                                                            */
/******************************************************************************/
Handle or_longc(Handle y, Handle x)
{
   Handle long_x, long_y;
   int sign_x, sign_y;

   if (IS_INT(DEREFWORDHANDLE(x)) && 
       IS_INT(DEREFWORDHANDLE(y))) /* Both short */
     {
       /* There's no problem with overflow so we can just OR together
	      the values. */
		word t = UNTAGGED(DEREFWORDHANDLE(x)) | UNTAGGED(DEREFWORDHANDLE(y));
		return push_to_save_vec(TAGGED(t));
     }

   /* Long arguments. */
   long_x = get_long(x, xHandle , &sign_x); /* Convert to long form */
   long_y = get_long(y, yHandle , &sign_y);

   return logical_long(long_x, long_y, sign_x, sign_y, doOr);
}

/******************************************************************************/
/*                                                                            */
/*      xor_longc - called by run-time system                                 */
/*                                                                            */
/******************************************************************************/
Handle xor_longc(Handle y, Handle x)
{
   Handle long_x, long_y;
   int sign_x, sign_y;

   if (IS_INT(DEREFWORDHANDLE(x)) && 
       IS_INT(DEREFWORDHANDLE(y))) /* Both short */
     {
       /* There's no problem with overflow so we can just XOR together
	      the values. */
		word t = UNTAGGED(DEREFWORDHANDLE(x)) ^ UNTAGGED(DEREFWORDHANDLE(y));
		return push_to_save_vec(TAGGED(t));
     }

   /* Long arguments. */
   long_x = get_long(x, xHandle , &sign_x); /* Convert to long form */
   long_y = get_long(y, yHandle , &sign_y);

   return logical_long(long_x, long_y, sign_x, sign_y, doXor);
}

/*
These functions are used primarily during porting.  They handle both
short and long forms of integers and are generally superseded by
hand coded assembly code versions and the code generator.
Some of them are retained in some code-generators to handle the
long forms of integers.
*/

#if !defined(SPARC)
/*
The Sparc assembly code traps on long forms and so uses the
same trap handling code as for code-generated code.
*/

word equal_longc(Handle y, Handle x)
/* Returns 1 if the arguments are equal, otherwise 0. */
{
  Handle long_x, long_y;
  int sign_x, sign_y;
  
  /* Equal shorts or identical longs? */
  if (DEREFHANDLE(x) == DEREFHANDLE(y)) return TAGGED(1);
  
  /* If either is short they are only equal if they are the same value. */ 
  if (IS_INT(DEREFWORDHANDLE(x)) || 
      IS_INT(DEREFWORDHANDLE(y))) return TAGGED(0);
  
  /* Convert to long form */
  long_x = get_long(x, xHandle , &sign_x);
  long_y = get_long(y, yHandle , &sign_y);
  
  /* Different signs? */ 
  if (sign_x != sign_y) return TAGGED(0); /* must be different */
      
  return compare_unsigned(long_x,long_y) == TAGGED(0) ? TAGGED(1) : TAGGED(0);
} /* equal_longc */

word gt_longc(Handle y, Handle x)
{
  int c = (comp_longc(y, x) == TAGGED(1));
  
  return c ? TAGGED(1) : TAGGED(0);
}

word ls_longc(Handle y, Handle x)
{
  int c = (comp_longc(y, x) == TAGGED(-1));
  
  return c ? TAGGED(1) : TAGGED(0);
}

word ge_longc(Handle y, Handle x)
{
  int c = comp_longc(y, x) != TAGGED(-1);
  
  return c ? TAGGED(1) : TAGGED(0);
}

word le_longc(Handle y, Handle x)
{
  int c = comp_longc(y, x) != TAGGED(1);
  
  return c ? TAGGED(1) : TAGGED(0);
}
#endif

#if !defined(SPARC) && !defined(i386)

Handle vec_length_c(Handle vector)    /* Length of a vector */
{
  word length;
  
  length = OBJ_OBJECT_LENGTH(DEREFWORDHANDLE(vector)[-1]);
  
  return Make_arbitrary_precision ( length );
}

Handle string_length_c(Handle string)    /* Length of a string */
{
  word length;
  
  if (IS_INT(DEREFWORDHANDLE(string))) return Make_arbitrary_precision ( 1 );
  
  length = DEREFWORDHANDLE(string)[0];
  
  return Make_arbitrary_precision ( length );
}

word load_byte_long_c(Handle byte_no /* offset in BYTES */, Handle addr)
{
  unsigned offset = get_C_ulong(DEREFWORDHANDLE(byte_no));
  
  return TAGGED(DEREFBYTEHANDLE(addr)[offset]);
}

Handle load_word_long_c(Handle word_no /* offset in WORDS */, Handle addr)
{
  unsigned offset = get_C_ulong(DEREFWORDHANDLE(word_no));
  
  return push_to_save_vec(DEREFWORDHANDLE(addr)[offset] );
}

/* Return the low-order bits of an integer whether it is long or
   short form. */
word int_to_word_c(Handle x)
{
	byte	*u;
	unsigned r = 0;
	int i;
	/* If it's already short it's easy. */
	if (IS_INT(DEREFWORDHANDLE(x))) return (word)DEREFWORDHANDLE(x);

    u = DEREFBYTEHANDLE(x);
	for (i=0; i < sizeof(word); i++)
	{
		r |= u[i] << (8*i);
	}
	if (OBJ_IS_NEGATIVE(DEREFWORDHANDLE(x)[-1])) r = -r;
	return TAGGED(r);
}
#endif

