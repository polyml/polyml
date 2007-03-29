/*
    Title:  Real number conversion
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
#ifndef REALCONV_H
#define REALCONV_H

#ifdef WIN32
#include "winconfig.h"
#else
#include "config.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

#ifndef HAVE_STRTOD
extern double strtod(const char *s00, char **se);
#endif

#ifndef HAVE_DTOA
extern char *dtoa(double d, int mode, int ndigits,
            int *decpt, int *sign, char **rve);
#endif

#ifdef __cplusplus
};
#endif

#endif
