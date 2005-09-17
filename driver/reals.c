/*
    Title: 	Real number package.
    Author: 	Dave Matthews, Cambridge University Computer Laboratory

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

#if defined(SOLARIS2)
/* Other operating systems include "finite" in math.h, but Solaris doesn't? */
#include <ieeefp.h>
#elif defined(WINDOWS_PC)
#define finite	_finite
#define isnan	_isnan
#define copysign _copysign
#elif defined(LINUX)
#include <fpu_control.h>
#elif defined(MACOSX)
/* Not there. */
#elif defined(FREEBSD)
#include <ieeefp.h>
#endif

#include <float.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "globals.h"
#include "run_time.h"
#include "reals.h"
#include "proper_io.h"
#include "arb.h"
#include "sys.h"
#include "realconv.h"

/*
	The Standard Basis Library assumes IEEE representation for reals.  Among other
	things it does not permit equality on reals.  That simplifies things
	considerably since we don't have to worry about there being two different
	representations of zero as 0 and ~0.  We also don't need to check that the
	result is finite since NaN is allowed as a result.
	This code could do with being checked by someone who really understands
	IEEE floating point arithmetic.

    The original real number functions all had separate entries in the interface
	vector.  Newly added functions all go through a single dispatch function.
	DCJM: March 2000.
*/

double posInf, negInf, notANumber;

/* Assumes that there will be a separate handler for floating point overflow
   and underflow. */

/* tidied up 27/10/93 SPF */
/*  can't assume (double *) has same alignment as (word *), so all the */
/* parameters should be passed in as a Handle, not (double **).        */

double real_arg(Handle x)
{
    union db r_arg_x;
    int i;
    for(i = 0; i < DBLE; i++)
    {
      r_arg_x.wrd[i] = DEREFWORDHANDLE(x)[i];
    }
    return r_arg_x.dble;
}

word *real_result(double x)
{
   word *v;
   int i;
   union db argx;

   argx.dble = x;

   v = (word *)alloc(DBLE | OBJ_BYTE_BIT);
   /* Copy as words in case the alignment is wrong. */
   for(i = 0; i < DBLE; i++)
   {
      v[i] = argx.wrd[i];
   }
   return v;
}

/* CALL_IO2(Real_add, REF, REF, NOIND) */
word *Real_addc(Handle y, Handle x)
{
    return real_result(real_arg(x)+real_arg(y));
}

/* CALL_IO2(Real_sub, REF, REF, NOIND) */
word *Real_subc(Handle y, Handle x)
{
    return real_result(real_arg(x)-real_arg(y));
}

/* CALL_IO2(Real_mul, REF, REF, NOIND) */
word *Real_mulc(Handle y, Handle x)
{
    return real_result(real_arg(x)*real_arg(y));
}

/* CALL_IO2(Real_div, REF, REF, NOIND) */
word *Real_divc(Handle y, Handle x)
{
    double dx = real_arg(x);
    double dy = real_arg(y);
    return real_result(dx/dy);
}

/* CALL_IO1(Real_neg, REF, NOIND) */
word *Real_negc(Handle x)
{
    return real_result(-real_arg(x));
}

/* The old Real_comp function isn't right for IEEE arithmetic. These
   functions were added to implement these correctly.
   On Windows, at any rate, the comparison operations do not necessarily
   return false on unordered arguments so we have to explicitly test for NaN.
 */
int Real_geqc(Handle y, Handle x)
{
	double dx = real_arg(x), dy = real_arg(y);
	if (isnan(dx) || isnan(dy)) return TAGGED(0);
	return dx >= dy ? TAGGED(1) : TAGGED(0);
}

int Real_leqc(Handle y, Handle x)
{
	double dx = real_arg(x), dy = real_arg(y);
	if (isnan(dx) || isnan(dy)) return TAGGED(0);
	return dx <= dy ? TAGGED(1) : TAGGED(0);
}

int Real_gtrc(Handle y, Handle x)
{
	double dx = real_arg(x), dy = real_arg(y);
	if (isnan(dx) || isnan(dy)) return TAGGED(0);
	return dx > dy ? TAGGED(1) : TAGGED(0);
}

int Real_lssc(Handle y, Handle x)
{
	double dx = real_arg(x), dy = real_arg(y);
	if (isnan(dx) || isnan(dy)) return TAGGED(0);
	return dx < dy ? TAGGED(1) : TAGGED(0);
}

int Real_eqc(Handle y, Handle x)
{
	double dx = real_arg(x), dy = real_arg(y);
	if (isnan(dx) || isnan(dy)) return TAGGED(0);
	return dx == dy ? TAGGED(1) : TAGGED(0);
}

int Real_neqc(Handle y, Handle x)
{
	double dx = real_arg(x), dy = real_arg(y);
	if (isnan(dx) || isnan(dy)) return TAGGED(0);
	return dx != dy ? TAGGED(1) : TAGGED(0);
}

/* CALL_IO1(Real_float, REF, NOIND) */
word *Real_floatc(Handle x) /* SHORT int to real */
{
    int n = UNTAGGED(DEREFWORDHANDLE(x));
    return real_result((double)n);
}

/* CALL_IO1(Real_int, REF, NOIND) */
word Real_intc(Handle x) /* real to SHORT int */
{
    int i;
    double dx,di;
    dx = real_arg(x);
    di = floor(dx); /* Get the largest integer <= dx */
    i  = (int) di ; /* Convert (truncate) this to integer */
    /* Bug fix thanks to Mike Crawley at Brunel. */
    if (di > (double)MAXTAGGED || di < -(double)MAXTAGGED -1)
    {
       raise_exception0(EXC_size);
    }
    return TAGGED(i);
}

/* CALL_IO1(Real_sqrt, REF, NOIND) */
word *Real_sqrtc(Handle arg)
{
    double dx;
    dx = real_arg(arg);
    return real_result(sqrt(dx));
}

/* CALL_IO1(Real_sin, REF, NOIND) */
word *Real_sinc(Handle arg)
{
    return real_result(sin(real_arg(arg)));
}

/* CALL_IO1(Real_cos, REF, NOIND) */
word *Real_cosc(Handle arg)
{
    return real_result(cos(real_arg(arg)));
}

/* CALL_IO1(Real_arctan, REF, NOIND) */
word *Real_arctanc(Handle arg)
{
    return real_result(atan(real_arg(arg)));
}

/* CALL_IO1(Real_exp, REF, NOIND) */
word *Real_expc(Handle arg)
{
    return real_result(exp(real_arg(arg)));
}

/* CALL_IO1(Real_ln, REF, NOIND) */
word *Real_lnc(Handle arg)
{
	double x = real_arg(arg);
	/* Make sure the result conforms to the definition. */
	if (x < 0.0)
		return real_result(notANumber); /* Nan. */
	else if (x == 0.0) /* x may be +0.0 or -0.0 */
		return real_result(negInf); /* -infinity. */
    else return real_result(log(x));
}

/* Real_Rep and Real_reprc are redundant.  This is now dealt with by a function within the
   basis library.  DCJM June 2002.*/

static void Real_Rep(double val, char *string_buffer)
/* Puts the string representation into ``string_buffer'' and edits it into
   the Poly representation. i.e. replacing '-' by '~' and removing '+', and
   putting in a ".0" if the number does not have an E or a decimal point. */
{
    int dot_or_e = 0, digits = 0;
    char *sptr;
	/* First handle the special cases.  We can't rely on sprintf doing
	   them in the way we want. */
	if (isnan(val))
	{
		strcpy(string_buffer, "nan");
	}
	else if (! finite(val))
	{
		if (val > 0.0) strcpy(string_buffer, "inf");
		else strcpy(string_buffer, "~inf");
	}
	else if (val == 0.0) 
	{
		if (copysign(1.0, val) < 0.0)
			strcpy(string_buffer, "~0.0");
		else strcpy(string_buffer, "0.0");
	}
	else
	{
		sprintf(string_buffer, "%.10G", val);
    
		for(sptr = string_buffer; *sptr != '\0'; sptr++)
		{
			if (*sptr == '-') *sptr = '~';
			else if (*sptr == '+')
			{
				/* Shift the rest up to cover the '+' */
				strcpy(sptr, sptr+1);
				sptr--;
			}
			else if (*sptr == '.' || *sptr == 'E')
			{
				if (! digits)
				{
					/* Must have a digit before the decimal point
					   - shift down and put in a zero. */
					register char *p;
					for (p = sptr; *p != '\0'; p++);
					for (; p >= sptr; p--) p[1] = *p;
					*sptr = '0';
					digits = 1;
				}
				dot_or_e = 1;
			}
			else if (*sptr >= '0' && *sptr <= '9') digits = 1;
		}
		if (!dot_or_e) strcat(string_buffer, ".0");
	}
} /* Real_Rep */

/* CALL_IO1(Real_repr, REF, NOIND) */
pstring Real_reprc(Handle val) /* real to string */
{
    char string_buffer[30];
    Real_Rep(real_arg(val), string_buffer);
    return C_string_to_Poly(string_buffer);
} /* Real_reprc */

/* CALL_IO1(Real_conv, REF, NOIND) */
word *Real_convc(Handle str) /* string to real */
{
    double result;
    int i;
    char string_buffer[30], *finish;
    
    Poly_string_to_C((pstring)DEREFHANDLE(str), string_buffer, 30);
    
    /* Scan the string turning '~' into '-' */
    for(i = 0; string_buffer[i] != '\0'; i ++)
    {
        if (string_buffer[i] == '~')
        {
          string_buffer[i] = '-';
        }
    }
        
    /* Now convert it */
	errno = 0;
	result = strtod(string_buffer, &finish);
	if (*finish != '\0' || errno != 0)
    {
       raise_exception_string(EXC_conversion, "");
    }
	
    return real_result(result);
}/* Real_conv */

static double real_arg1(Handle x)
{
    union db r_arg_x;
    int i;
    for(i = 0; i < DBLE; i++)
    {
      r_arg_x.wrd[i] = DEREFHANDLE(x)[0][i];
    }
    return r_arg_x.dble;
}

static double real_arg2(Handle x)
{
    union db r_arg_x;
    int i;
    for(i = 0; i < DBLE; i++)
    {
      r_arg_x.wrd[i] = DEREFHANDLE(x)[1][i];
    }
    return r_arg_x.dble;
}

static word *powerOf(Handle args)
{
	double x = real_arg1(args), y = real_arg2(args);
	/* Some of the special cases are defined and don't seem to match
	   the C pow function (at least as implemented in MS C). */
	/* Maybe handle all this in ML? */
	if (isnan(x))
	{
		if (y == 0.0) return real_result(1.0);
		else return real_result(notANumber);
	}
	else if (isnan(y)) return real_result(y); /* i.e. nan. */
	else if (x == 0.0 && y < 0.0)
	{
		/* This case is not handled correctly in Solaris. It always
		   returns -infinity. */
		int iy = (int)floor(y);
		/* If x is -0.0 and y is an odd integer the result is -infinity. */
		if (copysign(1.0, x) < 0.0 && (double)iy == y && (iy & 1))
			return real_result(negInf); /* -infinity. */
		else return real_result(posInf); /* +infinity. */
	}
	return real_result(pow(x, y));
}

#ifdef WINDOWS_PC
static void setrounding(Handle args)
{
	switch (get_C_long(DEREFWORDHANDLE(args)))
	{
	case 0: _controlfp(_RC_NEAR, _MCW_RC); break; // Choose nearest
	case 1: _controlfp(_RC_DOWN, _MCW_RC); break; // Towards negative infinity
	case 2: _controlfp(_RC_UP, _MCW_RC); break; // Towards positive infinity
	case 3: _controlfp(_RC_CHOP, _MCW_RC); break; // Truncate towards zero
	}
}

static word getrounding(void)
{
	switch (_controlfp(0,0) & _MCW_RC)
	{
	case _RC_NEAR: return TAGGED(0);
	case _RC_DOWN: return TAGGED(1);
	case _RC_UP: return TAGGED(2);
	case _RC_CHOP: return TAGGED(3);
	}
}
#elif defined(LINUX)
static void setrounding(Handle args)
{
	fpu_control_t ctrl;
	_FPU_GETCW(ctrl);
	ctrl &= ~_FPU_RC_ZERO; /* Mask off any existing rounding. */
        switch (get_C_long(DEREFWORDHANDLE(args)))
        {
	case 0: ctrl |= _FPU_RC_NEAREST;
	case 1: ctrl |= _FPU_RC_DOWN;
	case 2: ctrl |= _FPU_RC_UP;
	case 3: ctrl |= _FPU_RC_ZERO;
	}
	_FPU_SETCW(ctrl);
}

static word getrounding(void)
{
	fpu_control_t ctrl;
	_FPU_GETCW(ctrl);
	switch (ctrl & _FPU_RC_ZERO)
	{
	case _FPU_RC_NEAREST: return TAGGED(0);
	case _FPU_RC_DOWN: return TAGGED(1);
	case _FPU_RC_UP: return TAGGED(2);
	case _FPU_RC_ZERO: return TAGGED(4);
	}
	return TAGGED(0); /* Never reached but this avoids warning message. */
}

#elif defined(MACOSX)
/* Mac OS X doesn't define any function to do this so we have to do it by hand. */
static void setrounding(Handle args)
{
	switch (get_C_long(DEREFWORDHANDLE(args)))
	{
	case 0: __asm__("mtfsfi 7,0"); break; /* Choose nearest */
	case 1: __asm__("mtfsfi 7,3"); break; /* Towards negative infinity */
	case 2: __asm__("mtfsfi 7,2"); break; /* Towards positive infinity */
	case 3: __asm__("mtfsfi 7,1"); break; /* Truncate towards zero */
	}
}

static void getround(union db *res)
{
    __asm__ ("mffs f0");
    __asm__ ("stfd f0,0(r3)");
}

static word getrounding(void)
{
        union db roundingRes;
        getround(&roundingRes);
        switch (roundingRes.wrd[1] & 3)
        {
        case 0: return TAGGED(0); /* Choose nearest */
        case 1: return TAGGED(3); /* Round towards zero */
        case 2: return TAGGED(2); /* Towards positive infinity */
        case 3: return TAGGED(1); /* Towards negative infinity */
        }
	return TAGGED(0); /* Never reached but this avoids warning message. */
}

#else
static void setrounding(Handle args)
{
	switch (get_C_long(DEREFWORDHANDLE(args)))
	{
	case 0: fpsetround(FP_RN); break; /* Choose nearest */
	case 1: fpsetround(FP_RM); break; /* Towards negative infinity */
	case 2: fpsetround(FP_RP); break; /* Towards positive infinity */
	case 3: fpsetround(FP_RZ); break; /* Truncate towards zero */
	}
}

static word getrounding(void)
{
	switch (fpgetround())
	{
	case FP_RN: return TAGGED(0);
	case FP_RM: return TAGGED(1);
	case FP_RP: return TAGGED(2);
	case FP_RZ: return TAGGED(3);
	default: return TAGGED(0); /* Shouldn't happen. */ 
	}
}
#endif

word *Real_strc(Handle hDigits, Handle hMode, Handle arg)
{
	double	dx = real_arg(arg);
	int		decpt, sign;
	int		mode = get_C_long(DEREFWORDHANDLE(hMode));
	int		digits = get_C_long(DEREFWORDHANDLE(hDigits));
	/* Compute the shortest string which gives the required value. */
	/* N.B. dtoa uses static buffers and is NOT thread-safe. */
	char *chars = dtoa(dx, mode, digits, &decpt, &sign, NULL);
	/* We have to be careful in case an allocation causes a
	   garbage collection. */
	pstring pStr = C_string_to_Poly(chars);
	Handle ppStr = push_to_save_vec((word)pStr);
	/* Allocate a triple for the results. */
	word *result = alloc(3);
	result[0] = (word)DEREFWORDHANDLE(ppStr);
	result[1] = TAGGED(decpt);
	result[2] = TAGGED(sign);
	return result;
}

/* Functions added for Standard Basis Library are all indirected through here. */
word *Real_dispatchc(Handle args, Handle code)
{
	int c = get_C_long(DEREFWORDHANDLE(code));
	switch (c)
	{
	case 0: /* tan */ return real_result(tan(real_arg(args)));
	case 1: /* asin */
		{
			double x = real_arg(args);
			if (x < -1.0 || x > 1.0)
				return real_result(notANumber);
			else return real_result(asin(x));
		}
	case 2: /* acos */
		{
			double x = real_arg(args);
			if (x < -1.0 || x > 1.0)
				return real_result(notANumber);
			else return real_result(acos(x));
		}
	case 3: /* atan2 */ return real_result(atan2(real_arg1(args), real_arg2(args)));
	case 4: /* pow */ return powerOf(args);
	case 5: /* log10 */
		{
			double x = real_arg(args);
			/* Make sure the result conforms to the definition. */
			if (x < 0.0)
				return real_result(notANumber); /* Nan. */
			else if (x == 0.0) /* x may be +0.0 or -0.0 */
				return real_result(negInf); /* -infinity. */
			else return real_result(log10(x));
		}
	case 6: /* sinh */ return real_result(sinh(real_arg(args)));
	case 7: /* cosh */ return real_result(cosh(real_arg(args)));
	case 8: /* tanh */ return real_result(tanh(real_arg(args)));
	case 9: /* setroundingmode */
		setrounding(args);
		return (word*) TAGGED(0); /* Unit */
	case 10: /* getroundingmode */
		return (word*)getrounding();
	/* Floating point representation queries. */
#ifdef _DBL_RADIX
	case 11: /* Value of radix */ return (word*)TAGGED(_DBL_RADIX);
#else
	case 11: /* Value of radix */ return (word*)TAGGED(FLT_RADIX);
#endif
	case 12: /* Value of precision */ return (word*)TAGGED(DBL_MANT_DIG);
	case 13: /* Maximum number */ return real_result(DBL_MAX);
	/* float.h describes DBL_MIN as the minimum positive number.
	   In fact this is the minimum NORMALISED number.  The smallest
	   number which can be represented is DBL_MIN*2**(-DBL_MANT_DIG) */
	case 14: /* Minimum normalised number. */
		return real_result(DBL_MIN);
	case 15: /* Is finite */
		return finite(real_arg(args)) ? (word*)TAGGED(1) : (word*)TAGGED(0);
	case 16: /* Is Nan */
		return isnan(real_arg(args)) ? (word*)TAGGED(1) : (word*)TAGGED(0);
	case 17: /* Get sign bit.  There may be better ways to find this. */
		return copysign(1.0, real_arg(args)) < 0.0 ? (word*)TAGGED(1) : (word*)TAGGED(0);
	case 18: /* Copy sign. */
		return real_result(copysign(real_arg1(args), real_arg2(args)));
	case 19: /* Return largest integral value (as a real) <= x. */
		return real_result(floor(real_arg(args)));
	case 20: /* Return smallest integral value (as a real) >= x  */
		return real_result(ceil(real_arg(args)));
	case 21:
		{ /* Truncate towards zero */
			double dx = real_arg(args);
			if (dx >= 0.0) return real_result(floor(dx));
			else return real_result(ceil(dx));
		}
	case 22: /* Round to nearest integral value. */
		{
			double dx = real_arg(args);
			double drem = fmod(dx, 2.0);
			if (drem == 0.5 || drem == -1.5)
				/* If the value was exactly positive even + 0.5 or
				   negative odd -0.5 round it down, otherwise round it up. */
				return real_result(ceil(dx-0.5));
			else return real_result(floor(dx+0.5));
		}
	case 23: /* Compute ldexp */
		{
			int exp = get_C_long(DEREFHANDLE(args)[1]);
			return real_result(ldexp(real_arg1(args), exp));
		}
	case 24: /* Get mantissa. */
		{
			int	exp;
			return real_result(frexp(real_arg(args), &exp));
		}
	case 25: /* Get exponent. */
		{
			int	exp;
			(void)frexp(real_arg(args), &exp);
			return (word*)TAGGED(exp);
		}
	case 26: /* Return the mantissa from a Nan as a real number. */
		{
			union db r_arg_x, r_arg_y;
			int i;
			/* We want to simply replace the exponent by the exponent
			   value for 0.5<=x<1.
			   I think there may be a more portable way of doing this. */
			r_arg_x.dble = posInf; /* Positive infinity. */
			r_arg_y.dble = 0.5;
			/* Use the infinity value as a mask, removing any bits set
			   and replace by the exponent from 0.5. */
			for(i = 0; i < DBLE; i++)
			{
				r_arg_x.wrd[i] =
					(DEREFWORDHANDLE(args)[i] & ~ r_arg_x.wrd[i]) | r_arg_y.wrd[i];
			}
			return real_result(r_arg_x.dble);
		}
	case 27: /* Construct a Nan from a given mantissa. */
		{
			union db r_arg;
			int i;
			r_arg.dble = posInf; /* Positive infinity. */
			/* OR in the exponent. */
			for(i = 0; i < DBLE; i++)
			{
				r_arg.wrd[i] |= DEREFWORDHANDLE(args)[i];
			}
			return real_result(r_arg.dble);
		}
	case 28: /* Return the number of bytes for a real.  */
		return (word*)TAGGED(sizeof(double));
	default: crash("Unknown real function: %d\n", c);
	}
}

void uninit_reals(void)
{
}

void re_init_reals(void)
{
}

void init_reals(void)
{
	/* Some compilers object to overflow in constants so
	   we compute the values here. */
	double zero = 0.0;
#if defined(FREEBSD)
	/* In FreeBSD 3.4 at least, we sometimes get floating point
	   exceptions if we don't clear the mask.  Maybe need to do
	   this on other platforms as well just to be sure. */
	fpsetmask(0);
#endif
	posInf = 1.0 / zero;
	negInf = -1.0 / zero;
	notANumber = zero / zero;
}
