/*
    Title:  Real number package.
    Author:     Dave Matthews, Cambridge University Computer Laboratory

    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Further work copyright David C.J. Matthews 2011

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

#if (defined(_MSC_VER))
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

#ifdef HAVE_STDINT_H
#include <stdint.h>
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

// Positive and negative infinities and (positive) NaN.
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

/* CALL_IO1(Real_abs, REF, NOIND) */
// This handles +/- NaN correctly.
// TODO: That assumes that notANumber is positive which may not be true.
Handle Real_absc(TaskData *mdTaskData, Handle x)
{
    double dx = real_arg(x);
    if (isnan(dx)) return real_result(mdTaskData, notANumber);
    else return real_result(mdTaskData, fabs(real_arg(x)));
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
// This previously always converted to a SHORT integer.  Using
// int64_t here means we will capture all the significant bits of
// the mantissa.  The calling code checks for infinities and NaNs
// and reduces the exponent if it is too big to fit.
Handle Real_intc(TaskData *mdTaskData, Handle x)
{
    double dx = real_arg(x);
    int64_t i = (int64_t)dx;
    return Make_arbitrary_precision(mdTaskData, i);
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

#define POLY_ROUND_TONEAREST    0
#define POLY_ROUND_DOWNWARD     1
#define POLY_ROUND_UPWARD       2
#define POLY_ROUND_TOZERO       3

#if defined(__SOFTFP__)
// soft-float lacks proper rounding mode support
// While some systems will support fegetround/fesetround, it will have no
// effect on the actual rounding performed, as the software implementation only
// ever rounds to nearest.
static int getrounding(TaskData *)
{
    return POLY_ROUND_TONEAREST;
}

static void setrounding(TaskData *taskData, Handle args)
{
    switch (get_C_long(taskData, DEREFWORDHANDLE(args)))
    {
    case POLY_ROUND_TONEAREST: return; // The only mode supported
    }
    raise_exception_string(taskData, EXC_Fail, "Setting the floating point rounding control to a value other than TO_NEAREST is not supported for software-based floating point implementations");
}

// It would be nice to be able to use autoconf to test for these as functions
// but they are frequently inlined 
#elif defined(HAVE_FENV_H)
// C99 version.  This is becoming the most common.
static int getrounding(TaskData *)
{
    switch (fegetround())
    {
    case FE_TONEAREST: return POLY_ROUND_TONEAREST;
    case FE_DOWNWARD: return POLY_ROUND_DOWNWARD;
    case FE_UPWARD: return POLY_ROUND_UPWARD;
    case FE_TOWARDZERO: return POLY_ROUND_TOZERO;
    }
    return 0; // Keep the compiler happy
}

static void setrounding(TaskData *taskData, Handle args)
{
    switch (get_C_long(taskData, DEREFWORDHANDLE(args)))
    {
    case POLY_ROUND_TONEAREST: fesetround(FE_TONEAREST); break; // Choose nearest
    case POLY_ROUND_DOWNWARD: fesetround(FE_DOWNWARD); break; // Towards negative infinity
    case POLY_ROUND_UPWARD: fesetround(FE_UPWARD); break; // Towards positive infinity
    case POLY_ROUND_TOZERO: fesetround(FE_TOWARDZERO); break; // Truncate towards zero
    }
}

#elif (defined(HAVE_IEEEFP_H) && ! defined(__CYGWIN__))
// Older FreeBSD.  Cygwin has the ieeefp.h header but not the functions!
static int getrounding(TaskData *)
{
    switch (fpgetround())
    {
    case FP_RN: return POLY_ROUND_TONEAREST;
    case FP_RM: return POLY_ROUND_DOWNWARD;
    case FP_RP: return POLY_ROUND_UPWARD;
    case FP_RZ: return POLY_ROUND_TOZERO;
    default: return 0; /* Shouldn't happen. */ 
    }
}

static void setrounding(TaskData *taskData, Handle args)
{
    switch (get_C_long(taskData, DEREFWORDHANDLE(args)))
    {
    case POLY_ROUND_TONEAREST: fpsetround(FP_RN); break; /* Choose nearest */
    case POLY_ROUND_DOWNWARD: fpsetround(FP_RM); break; /* Towards negative infinity */
    case POLY_ROUND_UPWARD: fpsetround(FP_RP); break; /* Towards positive infinity */
    case POLY_ROUND_TOZERO: fpsetround(FP_RZ); break; /* Truncate towards zero */
    }
}

#elif defined(_WIN32)
// Windows version
static int getrounding(TaskData *)
{
    switch (_controlfp(0,0) & _MCW_RC)
    {
    case _RC_NEAR: return POLY_ROUND_TONEAREST;
    case _RC_DOWN: return POLY_ROUND_DOWNWARD;
    case _RC_UP: return POLY_ROUND_UPWARD;
    case _RC_CHOP: return POLY_ROUND_TOZERO;
    }
    return 0; // Keep the compiler happy
}

static void setrounding(TaskData *mdTaskData, Handle args)
{
    switch (get_C_long(mdTaskData, DEREFWORDHANDLE(args)))
    {
    case POLY_ROUND_TONEAREST: _controlfp(_RC_NEAR, _MCW_RC); break; // Choose nearest
    case POLY_ROUND_DOWNWARD: _controlfp(_RC_DOWN, _MCW_RC); break; // Towards negative infinity
    case POLY_ROUND_UPWARD: _controlfp(_RC_UP, _MCW_RC); break; // Towards positive infinity
    case POLY_ROUND_TOZERO: _controlfp(_RC_CHOP, _MCW_RC); break; // Truncate towards zero
    }
}

#elif defined(_FPU_GETCW) && defined(_FPU_SETCW)
// Older Linux version
static int getrounding(TaskData *)
{
    fpu_control_t ctrl;
    _FPU_GETCW(ctrl);
    switch (ctrl & _FPU_RC_ZERO)
    {
    case _FPU_RC_NEAREST: return POLY_ROUND_TONEAREST;
    case _FPU_RC_DOWN: return POLY_ROUND_DOWNWARD;
    case _FPU_RC_UP: return POLY_ROUND_UPWARD;
    case _FPU_RC_ZERO: return POLY_ROUND_TOZERO;
    }
    return 0; /* Never reached but this avoids warning message. */
}

static void setrounding(TaskData *taskData, Handle args)
{
    fpu_control_t ctrl;
    _FPU_GETCW(ctrl);
    ctrl &= ~_FPU_RC_ZERO; /* Mask off any existing rounding. */
    switch (get_C_long(taskData, DEREFWORDHANDLE(args)))
    {
    case POLY_ROUND_TONEAREST: ctrl |= _FPU_RC_NEAREST;
    case POLY_ROUND_DOWNWARD: ctrl |= _FPU_RC_DOWN;
    case POLY_ROUND_UPWARD: ctrl |= _FPU_RC_UP;
    case POLY_ROUND_TOZERO: ctrl |= _FPU_RC_ZERO;
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

static int getrounding(TaskData *)
{
    union db roundingRes;
    getround(&roundingRes);
    switch (roundingRes.wrd[1] & 3)
    {
    case 0: return POLY_ROUND_TONEAREST; /* Choose nearest */
    case 1: return POLY_ROUND_TOZERO; /* Round towards zero */
    case 2: return POLY_ROUND_UPWARD; /* Towards positive infinity */
    case 3: return POLY_ROUND_DOWNWARD; /* Towards negative infinity */
    }
    return 0; /* Never reached but this avoids warning message. */
}

static void setrounding(TaskData *taskData, Handle args)
{
    switch (get_C_long(taskData, DEREFWORDHANDLE(args)))
    {
    case POLY_ROUND_TONEAREST: __asm__("mtfsfi 7,0"); break; /* Choose nearest */
    case POLY_ROUND_DOWNWARD: __asm__("mtfsfi 7,3"); break; /* Towards negative infinity */
    case POLY_ROUND_UPWARD: __asm__("mtfsfi 7,2"); break; /* Towards positive infinity */
    case POLY_ROUND_TOZERO: __asm__("mtfsfi 7,1"); break; /* Truncate towards zero */
    }
}
#else
// Give up.
static int getrounding(TaskData *mdTaskData)
{
    raise_exception_string(mdTaskData, EXC_Fail, "Unable to get floating point rounding control");
}

static void setrounding(TaskData *mdTaskData, Handle)
{
    raise_exception_string(mdTaskData, EXC_Fail, "Unable to set floating point rounding control");
}
#endif

Handle Real_strc(TaskData *mdTaskData, Handle hDigits, Handle hMode, Handle arg)
{
    double  dx = real_arg(arg);
    int     decpt, sign;
    int     mode = get_C_int(mdTaskData, DEREFWORDHANDLE(hMode));
    int     digits = get_C_int(mdTaskData, DEREFWORDHANDLE(hDigits));
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
    unsigned c = get_C_unsigned(mdTaskData, DEREFWORDHANDLE(code));
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
        return mdTaskData->saveVec.push(TAGGED(getrounding(mdTaskData)));
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
#if (0)
    case 15: /* Is finite */ /* No longer used - implemented in ML. */
        return mdTaskData->saveVec.push(finite(real_arg(args)) ? TAGGED(1) : TAGGED(0));
    case 16: /* Is Nan */ /* No longer used - implemented in ML. */
        return mdTaskData->saveVec.push(isnan(real_arg(args)) ? TAGGED(1) : TAGGED(0));
#endif
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
            int exp = get_C_int(mdTaskData, DEREFHANDLE(args)->Get(1));
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
#if (HAVE_DECL_FPSETMASK && ! defined(__CYGWIN__))
    /* In FreeBSD 3.4 at least, we sometimes get floating point
       exceptions if we don't clear the mask.  Maybe need to do
       this on other platforms as well just to be sure. */
    // N.B.  fpsetmask is defined in the headers on Cygwin but there's no function!
    fpsetmask(0);
#endif
    // NAN and INFINITY are defined in GCC but not in Visual C++.
#if (defined(INFINITY))
    posInf = INFINITY;
    negInf = -(INFINITY);
#else
    {
        double zero = 0.0;
        posInf = 1.0 / zero;
        negInf = -1.0 / zero;
    }
#endif
#if (defined(NAN))
    notANumber = NAN;
#else
    {
        double zero = 0.0;
        notANumber = zero / zero;
    }
#endif
    // Make sure this is a positive NaN since we return it from "abs".
    // "Positive" in this context is copysign(1.0, x) > 0.0 because that's
    // how we test the sign so we test it first and then try to change the
    // sign if it's wrong.
    if (copysign(1.0, notANumber) < 0)
        notANumber = copysign(notANumber, 1.0);
}
