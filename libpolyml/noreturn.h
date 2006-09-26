/*
    Title:  No return header.
    Author: David C.J. Matthews

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

#ifndef _NORETURN_H
#define _NORETURN_H

/* The exception functions don't return but instead raise exceptions.  This macro
   tells the compiler about this and prevents spurious errors.  */
#if defined(__GNUC__) || defined(__attribute__)
#define NORETURNFN(x)   x __attribute__((noreturn))
#define NORET        0
#elif (defined(_MSC_EXTENSIONS) && (_MSC_VER >= 1200))
#define NORETURNFN(x) __declspec(noreturn) x
#define NORET        0
#else
#define NORETURNFN(x)
#define NORET        return 0
#endif


#endif

