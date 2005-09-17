/*
    Title: 	Header for diagnostics.c

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

#ifndef _DIAGNOSTICS_H_USED

#define _DIAGNOSTICS_H_USED

/* Give macro `__FUNCTION__' a defintion if we are not using GCC */
#ifndef __GNUC__
#define __FUNCTION__	    __FILE__ 
#endif


/*------------------------------------------------------------------------------
    Diagnostics macros
    - syntactically they're like statements, can be in unbraced if-branch.

    Intended usuage.

    WARN and WARN_ASSERT for anything that is recoverable.
    - turn off in main release driver?   
    FATAL_ASSERT - serious problem.
    	    	 - their is DEFINITELY no recovery possible.
		 - not turned of in release driver?
------------------------------------------------------------------------------*/

#define false 0
#define true  1

#define DEVELOPEMENT true

#define FATAL_ASSERT(x,args)                                                        \
        do{                                                                         \
            if(!(x)) {                                                              \
                proper_printf("** FATAL ASSERTION TRIPPED IN: %s **\n",__FUNCTION__);	    \
                proper_printf args;                                                        \
		proper_printf("\n");	    	    	    	    	    	    	    \
		exit(999);  	    	    	    	    	    	    	    \
    	    }	    	    	    	    	    	    	    	    	    \
        }while(0)
		    	
#define WARN_ASSERT(x,args)                                                         \
        do{                                                                         \
            if(DEVELOPEMENT && !(x)) {                                              \
                proper_printf("** WARNING TRIPPED IN: %s **\n",__FUNCTION__);      	    \
		proper_printf("** PLEASE REPORT ASAP **\n");	    	    	    	    \
                proper_printf args;                                                        \
		proper_printf("\n");	    	    	    	    	    	    	    \
		proper_printf("** NON FATAL: continuing... **\n");   	    	     	    \
    	    }	    	    	    	    	    	    	    	    	    \
     	}while(0)
	    	
#define WARN(args)  WARN_ASSERT(false,args)



#if !defined(WINDOWS_PC)
extern const char* syserror(int);
  /* return string corresponding to system error number passed in */
#endif

#ifndef __GNUC__
#ifndef __attribute__
#define __attribute__(attribute) /* attribute */
#endif
#endif

#if	(defined(_MSC_EXTENSIONS) && (_MSC_VER >= 1200))
#define NORETURN	__declspec(noreturn)
#else
#define NORETURN
#endif

NORETURN extern void Exit(const char *, ...) __attribute__((noreturn));
NORETURN extern void Crash(const char *, ...) __attribute__((noreturn));
NORETURN extern void SysError(const char *, ...) __attribute__((noreturn));
NORETURN extern void Usage(char *message) __attribute__((noreturn));

#endif
