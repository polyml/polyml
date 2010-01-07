/*
    Title:  Real number package.
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#elif defined(WIN32)
#include "winconfig.h"
#else
#error "No configuration file"
#endif

#ifdef HAVE_IEEEFP_H
/* Other operating systems include "finite" in math.h, but Solaris doesn't? */
#include <ieeefp.h>
#endif

#ifdef HAVE_FPU_CONTROL_H
#include <fpu_control.h>
#endif

#ifdef HAVE_FENV_H
#include <fenv.h>
#endif

#if (defined(WIN32))
#define finite  _finite
#define isnan   _isnan
#define copysign _copysign
#endif

#ifdef HAVE_FLOAT_H
#include <float.h>
#endif

#ifdef HAVE_MATH_H
#include <math.h>
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include "globals.h"
#include "run_time.h"
#include "reals.h"
#include "arb.h"
#include "sys.h"
#include "realconv.h"
#include "polystring.h"
#include "save_vec.h"
#include "rts_module.h"
#include "machine_dep.h"
#include "processes.h"

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

/* Real numbers are represented by the address of the value. */
#define DBLE sizeof(double)

union db { double dble; byte bytes[DBLE]; };

/* Assumes that there will be a separate handler for floating point overflow
   and underflow. */

/* tidied up 27/10/93 SPF */
/*  can't assume (double *) has same alignment as (PolyWord *), so all the */
/* parameters should be passed in as a Handle, not (double **).        */

double real_arg(Handle x)
{
    union db r_arg_x;
    for (unsigned i = 0; i < DBLE; i++)
    {
        r_arg_x.bytes[i] = DEREFBYTEHANDLE(x)[i];
    }
    return r_arg_x.dble;
}

Handle real_result(TaskData *mdTaskData, double x)
{
    union db argx;
    
    argx.dble = x;
    
    PolyObject *v = alloc(mdTaskData, DBLE/sizeof(PolyWord), F_BYTE_OBJ);
    /* Copy as words in case the alignment is wrong. */
    for(unsigned i = 0; i < DBLE; i++)
    {
        v->AsBytePtr()[i] = argx.bytes[i];
    }
    return mdTaskData->saveVec.push(v);
}

/* CALL_IO2(Real_add, REF, REF, NOIND) */
Handle Real_addc(TaskData *mdTaskData, Handle y, Handle x)
{
    return real_result(mdTaskData, real_arg(x)+real_arg(y));
}

/* CALL_IO2(Real_sub, REF, REF, NOIND) */
Handle Real_subc(TaskData *mdTaskData, Handle y, Handle x)
{
    return real_result(mdTaskData, real_arg(x)-real_arg(y));
}

/* CALL_IO2(Real_mul, REF, REF, NOIND) */
Handle Real_mulc(TaskData *mdTaskData, Handle y, Handle x)
{
    return real_result(mdTaskData, real_arg(x)*real_arg(y));
}

/* CALL_IO2(Real_div, REF, REF, NOIND) */
Handle Real_divc(TaskData *mdTaskData, Handle y, Handle x)
{
    double dx = real_arg(x);
    double dy = real_arg(y);
    return real_result(mdTaskData, dx/dy);
}

/* CALL_IO1(Real_neg, REF, NOIND) */
Handle Real_negc(TaskData *mdTaskData, Handle x)
{
    return real_result(mdTaskData, -real_arg(x));
}

/* The old Real_comp function isn't right for IEEE arithmetic. These
   functions were added to implement these correctly.
   On Windows, at any rate, the comparison operations do not necessarily
   return false on unordered arguments so we have to explicitly test for NaN.
 */
Handle Real_geqc(TaskData *mdTaskData, Handle y, Handle x)
{
    double dx = real_arg(x), dy = real_arg(y);
    if (isnan(dx) || isnan(dy)) return mdTaskData->saveVec.push(TAGGED(0));
    return mdTaskData->saveVec.push(dx >= dy ? TAGGED(1) : TAGGED(0));
}

Handle Real_leqc(TaskData *mdTaskData, Handle y, Handle x)
{
    double dx = real_arg(x), dy = real_arg(y);
    if (isnan(dx) || isnan(dy)) return mdTaskData->saveVec.push(TAGGED(0));
    return mdTaskData->saveVec.push(dx <= dy ? TAGGED(1) : TAGGED(0));
}

Handle Real_gtrc(TaskData *mdTaskData, Handle y, Handle x)
{
    double dx = real_arg(x), dy = real_arg(y);
    if (isnan(dx) || isnan(dy)) return mdTaskData->saveVec.push(TAGGED(0));
    return mdTaskData->saveVec.push(dx > dy ? TAGGED(1) : TAGGED(0));
}

Handle Real_lssc(TaskData *mdTaskData, Handle y, Handle x)
{
    double dx = real_arg(x), dy = real_arg(y);
    if (isnan(dx) || isnan(dy)) return mdTaskData->saveVec.push(TAGGED(0));
    return mdTaskData->saveVec.push(dx < dy ? TAGGED(1) : TAGGED(0));
}

Handle Real_eqc(TaskData *mdTaskData, Handle y, Handle x)
{
    double dx = real_arg(x), dy = real_arg(y);
    if (isnan(dx) || isnan(dy)) return mdTaskData->saveVec.push(TAGGED(0));
    return mdTaskData->saveVec.push(dx == dy ? TAGGED(1) : TAGGED(0));
}

Handle Real_neqc(TaskData *mdTaskData, Handle y, Handle x)
{
    double dx = real_arg(x), dy = real_arg(y);
    if (isnan(dx) || isnan(dy)) return mdTaskData->saveVec.push(TAGGED(1));
    return mdTaskData->saveVec.push(dx != dy ? TAGGED(1) : TAGGED(0));
}

/* CALL_IO1(Real_float, REF, NOIND) */
Handle Real_floatc(TaskData *mdTaskData, Handle x) /* SHORT int to real */
{
    double d = get_C_real(mdTaskData, DEREFWORDHANDLE(x));
    return real_result(mdTaskData, d);
}

/* CALL_IO1(Real_int, REF, NOIND) */
Handle Real_intc(TaskData *mdTaskData, Handle x) /* real to SHORT int */
{
    double dx = real_arg(x);
    double di = floor(dx); /* Get the largest integer <= dx */
    POLYSIGNED i  = (POLYSIGNED) di ; /* Convert (truncate) this to integer */
    /* Bug fix thanks to Mike Crawley at Brunel. */
    if (di > (double)MAXTAGGED || di < -(double)MAXTAGGED -1)
    {
       raise_exception0(mdTaskData, EXC_size);
    }
    return mdTaskData->saveVec.push(TAGGED(i));
}

/* CALL_IO1(Real_sqrt, REF, NOIND) */
Handle Real_sqrtc(TaskData *mdTaskData, Handle arg)
{
    double dx = real_arg(arg);
    return real_result(mdTaskData, sqrt(dx));
}

/* CALL_IO1(Real_sin, REF, NOIND) */
Handle Real_sinc(TaskData *mdTaskData, Handle arg)
{
    return real_result(mdTaskData, sin(real_arg(arg)));
}

/* CALL_IO1(Real_cos, REF, NOIND) */
Handle Real_cosc(TaskData *mdTaskData, Handle arg)
{
    return real_result(mdTaskData, cos(real_arg(arg)));
}

/* CALL_IO1(Real_arctan, REF, NOIND) */
Handle Real_arctanc(TaskData *mdTaskData, Handle arg)
{
    return real_result(mdTaskData, atan(real_arg(arg)));
}

/* CALL_IO1(Real_exp, REF, NOIND) */
Handle Real_expc(TaskData *mdTaskData, Handle arg)
{
    return real_result(mdTaskData, exp(real_arg(arg)));
}

/* CALL_IO1(Real_ln, REF, NOIND) */
Handle Real_lnc(TaskData *mdTaskData, Handle arg)
{
    double x = real_arg(arg);
    /* Make sure the result conforms to the definition. */
    if (x < 0.0)
        return real_result(mdTaskData, notANumber); /* Nan. */
    else if (x == 0.0) /* x may be +0.0 or -0.0 */
        return real_result(mdTaskData, negInf); /* -infinity. */
    else return real_result(mdTaskData, log(x));
}

/* Real_Rep and Real_reprc are redundant.  This is now dealt with by a function within the
   basis library.  DCJM June 2002.*/

static void Real_Rep(TaskData *mdTaskData, double val, char *string_buffer)
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
Handle Real_reprc(TaskData *mdTaskData, Handle val) /* real to string */
{
    char string_buffer[30];
    Real_Rep(mdTaskData, real_arg(val), string_buffer);
    return mdTaskData->saveVec.push(C_string_to_Poly(mdTaskData, string_buffer));
} /* Real_reprc */

/* CALL_IO1(Real_conv, REF, NOIND) */
Handle Real_convc(TaskData *mdTaskData, Handle str) /* string to real */
{
    double result;
    int i;
    char *finish;
    char *string_buffer = Poly_string_to_C_alloc(DEREFHANDLE(str));
    
    /* Scan the string turning '~' into '-' */
    for(i = 0; string_buffer[i] != '\0'; i ++)
    {
        if (string_buffer[i] == '~') string_buffer[i] = '-';
    }
        
    /* Now convert it */
    result = poly_strtod(string_buffer, &finish);
    bool isError = *finish != '\0'; // Test before deallocating
    free(string_buffer);
    // We no longer detect overflow and underflow and instead return
    // (signed) zeros for underflow and (signed) infinities for overflow.
    if (isError) raise_exception_string(mdTaskData, EXC_conversion, "");

    return real_result(mdTaskData, result);
}/* Real_conv */

static double real_arg1(Handle x)
{
    union db r_arg_x;
    for(unsigned i = 0; i < DBLE; i++)
    {
        r_arg_x.bytes[i] = DEREFHANDLE(x)->Get(0).AsObjPtr()->AsBytePtr()[i];
    }
    return r_arg_x.dble;
}

static double real_arg2(Handle x)
{
    union db r_arg_x;
    for(unsigned i = 0; i < DBLE; i++)
    {
        r_arg_x.bytes[i] = DEREFHANDLE(x)->Get(1).AsObjPtr()->AsBytePtr()[i];
    }
    return r_arg_x.dble;
}

static Handle powerOf(TaskData *mdTaskData, Handle args)
{
    double x = real_arg1(args), y = real_arg2(args);
    /* Some of the special cases are defined and don't seem to match
       the C pow function (at least as implemented in MS C). */
    /* Maybe handle all this in ML? */
    if (isnan(x))
    {
        if (y == 0.0) return real_result(mdTaskData, 1.0);
        else return real_result(mdTaskData, notANumber);
    }
    else if (isnan(y)) return real_result(mdTaskData, y); /* i.e. nan. */
    else if (x == 0.0 && y < 0.0)
    {
        /* This case is not handled correctly in Solaris. It always
           returns -infinity. */
        int iy = (int)floor(y);
        /* If x is -0.0 and y is an odd integer the result is -infinity. */
        if (copysign(1.0, x) < 0.0 && (double)iy == y && (iy & 1))
            return real_result(mdTaskData, negInf); /* -infinity. */
        else return real_result(mdTaskData, posInf); /* +infinity. */
    }
    return real_result(mdTaskData, pow(x, y));
}

// It would be nice to be able to use autoconf to test for these as functions
// but they are frequently inlined 
#if defined(HAVE_FENV_H)
// C99 version.  This is becoming the most common.
static PolyWord getrounding(TaskData *)
{
    switch (fegetround())
    {
    case FE_TONEAREST: return TAGGED(0);
    case FE_DOWNWARD: return TAGGED(1);
    case FE_UPWARD: return TAGGED(2);
    case FE_TOWARDZERO: return TAGGED(3);
    }
    return TAGGED(0); // Keep the compiler happy
}

static void setrounding(TaskData *taskData, Handle args)
{
    switch (get_C_long(taskData, DEREFWORDHANDLE(args)))
    {
    case 0: fesetround(FE_TONEAREST); break; // Choose nearest
    case 1: fesetround(FE_DOWNWARD); break; // Towards negative infinity
    case 2: fesetround(FE_UPWARD); break; // Towards positive infinity
    case 3: fesetround(FE_TOWARDZERO); break; // Truncate towards zero
    }
}

#elif (defined(HAVE_IEEEFP_H) && ! defined(__CYGWIN__))
// Older FreeBSD.  Cygwin has the ieeefp.h header but not the functions!
static PolyWord getrounding(TaskData *)
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

static void setrounding(TaskData *taskData, Handle args)
{
    switch (get_C_long(taskData, DEREFWORDHANDLE(args)))
    {
    case 0: fpsetround(FP_RN); break; /* Choose nearest */
    case 1: fpsetround(FP_RM); break; /* Towards negative infinity */
    case 2: fpsetround(FP_RP); break; /* Towards positive infinity */
    case 3: fpsetround(FP_RZ); break; /* Truncate towards zero */
    }
}

#elif defined(WINDOWS_PC)
// Windows version
static PolyWord getrounding(TaskData *)
{
    switch (_controlfp(0,0) & _MCW_RC)
    {
    case _RC_NEAR: return TAGGED(0);
    case _RC_DOWN: return TAGGED(1);
    case _RC_UP: return TAGGED(2);
    case _RC_CHOP: return TAGGED(3);
    }
    return TAGGED(0); // Keep the compiler happy
}

static void setrounding(TaskData *mdTaskData, Handle args)
{
    switch (get_C_long(mdTaskData, DEREFWORDHANDLE(args)))
    {
    case 0: _controlfp(_RC_NEAR, _MCW_RC); break; // Choose nearest
    case 1: _controlfp(_RC_DOWN, _MCW_RC); break; // Towards negative infinity
    case 2: _controlfp(_RC_UP, _MCW_RC); break; // Towards positive infinity
    case 3: _controlfp(_RC_CHOP, _MCW_RC); break; // Truncate towards zero
    }
}

#elif defined(_FPU_GETCW) && defined(_FPU_SETCW)
// Older Linux version
static PolyWord getrounding(TaskData *)
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

static void setrounding(TaskData *taskData, Handle args)
{
    fpu_control_t ctrl;
    _FPU_GETCW(ctrl);
    ctrl &= ~_FPU_RC_ZERO; /* Mask off any existing rounding. */
    switch (get_C_long(taskData, DEREFWORDHANDLE(args)))
    {
    case 0: ctrl |= _FPU_RC_NEAREST;
    case 1: ctrl |= _FPU_RC_DOWN;
    case 2: ctrl |= _FPU_RC_UP;
    case 3: ctrl |= _FPU_RC_ZERO;
    }
    _FPU_SETCW(ctrl);
}

#elif (defined(HOSTARCHITECTURE_PPC) && defined(MACOSX))
// Older versions of the Mac OS X didn't have a suitable function.

static void getround(union db *res)
{
    __asm__ ("mffs f0");
    __asm__ ("stfd f0,0(r3)");
}

static PolyWord getrounding(TaskData *)
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

static void setrounding(TaskData *taskData, Handle args)
{
    switch (get_C_long(taskData, DEREFWORDHANDLE(args)))
    {
    case 0: __asm__("mtfsfi 7,0"); break; /* Choose nearest */
    case 1: __asm__("mtfsfi 7,3"); break; /* Towards negative infinity */
    case 2: __asm__("mtfsfi 7,2"); break; /* Towards positive infinity */
    case 3: __asm__("mtfsfi 7,1"); break; /* Truncate towards zero */
    }
}
#else
// Give up.
static PolyWord getrounding(TaskData *mdTaskData)
{
    raise_exception_string(mdTaskData, EXC_Fail, "Unable to get flaoting point rounding control");
}

static void setrounding(TaskData *mdTaskData, Handle)
{
    raise_exception_string(mdTaskData, EXC_Fail, "Unable to set flaoting point rounding control");
}
#endif

Handle Real_strc(TaskData *mdTaskData, Handle hDigits, Handle hMode, Handle arg)
{
    double  dx = real_arg(arg);
    int     decpt, sign;
    int     mode = get_C_long(mdTaskData, DEREFWORDHANDLE(hMode));
    int     digits = get_C_long(mdTaskData, DEREFWORDHANDLE(hDigits));
    /* Compute the shortest string which gives the required value. */
    /*  */
    char *chars = poly_dtoa(dx, mode, digits, &decpt, &sign, NULL);
    /* We have to be careful in case an allocation causes a
       garbage collection. */
    PolyWord pStr = C_string_to_Poly(mdTaskData, chars);
    poly_freedtoa(chars);
    Handle ppStr = mdTaskData->saveVec.push(pStr);
    /* Allocate a triple for the results. */
    PolyObject *result = alloc(mdTaskData, 3);
    result->Set(0, DEREFWORDHANDLE(ppStr));
    result->Set(1, TAGGED(decpt));
    result->Set(2, TAGGED(sign));
    return mdTaskData->saveVec.push(result);
}

/* Functions added for Standard Basis Library are all indirected through here. */
Handle Real_dispatchc(TaskData *mdTaskData, Handle args, Handle code)
{
    int c = get_C_long(mdTaskData, DEREFWORDHANDLE(code));
    switch (c)
    {
    case 0: /* tan */ return real_result(mdTaskData, tan(real_arg(args)));
    case 1: /* asin */
        {
            double x = real_arg(args);
            if (x < -1.0 || x > 1.0)
                return real_result(mdTaskData, notANumber);
            else return real_result(mdTaskData, asin(x));
        }
    case 2: /* acos */
        {
            double x = real_arg(args);
            if (x < -1.0 || x > 1.0)
                return real_result(mdTaskData, notANumber);
            else return real_result(mdTaskData, acos(x));
        }
    case 3: /* atan2 */ return real_result(mdTaskData, atan2(real_arg1(args), real_arg2(args)));
    case 4: /* pow */ return powerOf(mdTaskData, args);
    case 5: /* log10 */
        {
            double x = real_arg(args);
            /* Make sure the result conforms to the definition. */
            if (x < 0.0)
                return real_result(mdTaskData, notANumber); /* Nan. */
            else if (x == 0.0) /* x may be +0.0 or -0.0 */
                return real_result(mdTaskData, negInf); /* -infinity. */
            else return real_result(mdTaskData, log10(x));
        }
    case 6: /* sinh */ return real_result(mdTaskData, sinh(real_arg(args)));
    case 7: /* cosh */ return real_result(mdTaskData, cosh(real_arg(args)));
    case 8: /* tanh */ return real_result(mdTaskData, tanh(real_arg(args)));
    case 9: /* setroundingmode */
        setrounding(mdTaskData, args);
        return mdTaskData->saveVec.push(TAGGED(0)); /* Unit */
    case 10: /* getroundingmode */
        return mdTaskData->saveVec.push(getrounding(mdTaskData));
    /* Floating point representation queries. */
#ifdef _DBL_RADIX
    case 11: /* Value of radix */ return mdTaskData->saveVec.push(TAGGED(_DBL_RADIX));
#else
    case 11: /* Value of radix */ return mdTaskData->saveVec.push(TAGGED(FLT_RADIX));
#endif
    case 12: /* Value of precision */ return mdTaskData->saveVec.push(TAGGED(DBL_MANT_DIG));
    case 13: /* Maximum number */ return real_result(mdTaskData, DBL_MAX);
    /* float.h describes DBL_MIN as the minimum positive number.
       In fact this is the minimum NORMALISED number.  The smallest
       number which can be represented is DBL_MIN*2**(-DBL_MANT_DIG) */
    case 14: /* Minimum normalised number. */
        return real_result(mdTaskData, DBL_MIN);
    case 15: /* Is finite */
        return mdTaskData->saveVec.push(finite(real_arg(args)) ? TAGGED(1) : TAGGED(0));
    case 16: /* Is Nan */
        return mdTaskData->saveVec.push(isnan(real_arg(args)) ? TAGGED(1) : TAGGED(0));
    case 17: /* Get sign bit.  There may be better ways to find this. */
        return mdTaskData->saveVec.push(copysign(1.0, real_arg(args)) < 0.0 ? TAGGED(1) : TAGGED(0));
    case 18: /* Copy sign. */
        return real_result(mdTaskData, copysign(real_arg1(args), real_arg2(args)));
    case 19: /* Return largest integral value (as a real) <= x. */
        return real_result(mdTaskData, floor(real_arg(args)));
    case 20: /* Return smallest integral value (as a real) >= x  */
        return real_result(mdTaskData, ceil(real_arg(args)));
    case 21:
        { /* Truncate towards zero */
            double dx = real_arg(args);
            if (dx >= 0.0) return real_result(mdTaskData, floor(dx));
            else return real_result(mdTaskData, ceil(dx));
        }
    case 22: /* Round to nearest integral value. */
        {
            double dx = real_arg(args);
            double drem = fmod(dx, 2.0);
            if (drem == 0.5 || drem == -1.5)
                /* If the value was exactly positive even + 0.5 or
                   negative odd -0.5 round it down, otherwise round it up. */
                return real_result(mdTaskData, ceil(dx-0.5));
            else return real_result(mdTaskData, floor(dx+0.5));
        }
    case 23: /* Compute ldexp */
        {
            int exp = get_C_long(mdTaskData, DEREFHANDLE(args)->Get(1));
            return real_result(mdTaskData, ldexp(real_arg1(args), exp));
        }
    case 24: /* Get mantissa. */
        {
            int exp;
            return real_result(mdTaskData, frexp(real_arg(args), &exp));
        }
    case 25: /* Get exponent. */
        {
            int exp;
            (void)frexp(real_arg(args), &exp);
            return mdTaskData->saveVec.push(TAGGED(exp));
        }
    case 26: /* Return the mantissa from a Nan as a real number. */
        // I think this is no longer used.
        {
            union db r_arg_x, r_arg_y;
            /* We want to simply replace the exponent by the exponent
               value for 0.5<=x<1.
               I think there may be a more portable way of doing this. */
            r_arg_x.dble = posInf; /* Positive infinity. */
            r_arg_y.dble = 0.5;
            /* Use the infinity value as a mask, removing any bits set
               and replace by the exponent from 0.5. */
            byte *barg = DEREFBYTEHANDLE(args);
            for(unsigned i = 0; i < DBLE; i++)
            {
                r_arg_x.bytes[i] = (barg[i] & ~r_arg_x.bytes[i]) | r_arg_y.bytes[i];
            }
            return real_result(mdTaskData, r_arg_x.dble);
        }
    case 27: /* Construct a Nan from a given mantissa. */
        // I think this is no longer used.
        {
            union db r_arg;
            r_arg.dble = posInf; /* Positive infinity. */
            /* OR in the exponent. */
            byte *barg = DEREFBYTEHANDLE(args);
            for(unsigned i = 0; i < DBLE; i++)
            {
                r_arg.bytes[i] = r_arg.bytes[i] | barg[i];
            }
            return real_result(mdTaskData, r_arg.dble);
        }
    case 28: /* Return the number of bytes for a real.  */
        return mdTaskData->saveVec.push(TAGGED(sizeof(double)));

    default:
        {
            char msg[100];
            sprintf(msg, "Unknown real arithmetic function: %d", c);
            raise_exception_string(mdTaskData, EXC_Fail, msg);
            return 0;
        }
    }
}

class RealArithmetic: public RtsModule
{
public:
    virtual void Init(void);
};

// Declare this.  It will be automatically added to the table.
static RealArithmetic realModule;

void RealArithmetic::Init(void)
{
    /* Some compilers object to overflow in constants so
       we compute the values here. */
    double zero = 0.0;
#if(defined(HAVE_IEEEFP_H) && ! defined(__CYGWIN__))
    /* In FreeBSD 3.4 at least, we sometimes get floating point
       exceptions if we don't clear the mask.  Maybe need to do
       this on other platforms as well just to be sure. */
    fpsetmask(0);
#endif
    posInf = 1.0 / zero;
    negInf = -1.0 / zero;
    notANumber = zero / zero;
}
