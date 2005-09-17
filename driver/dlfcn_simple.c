/*
Copyright (c) 2002 Peter O'Gorman <ogorman@users.sourceforge.net>

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/


/* Just to prove that it isn't that hard to add Mac calls to your code :)
   This works with pretty much everything, including kde3 xemacs and the gimp,
   I'd guess that it'd work in at least 95% of cases, use this as your starting
   point, rather than the mess that is dlfcn.c, assuming that your code does not
   require ref counting or symbol lookups in dependent libraries
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdarg.h>
#include <limits.h>
#include <mach-o/dyld.h>
#include "dlfcn.h"

#define ERR_STR_LEN 256
static const char *error(int setget, const char *str, ...);



/* Set and get the error string for use by dlerror */
static const char *error(int setget, const char *str, ...)
{
	static char errstr[ERR_STR_LEN];
	static int err_filled = 0;
	const char *retval;
	NSLinkEditErrors ler;
	int lerno;
	const char *dylderrstr;
	const char *file;
	va_list arg;
	if (setget <= 0)
	{
		va_start(arg, str);
		strncpy(errstr, "dlsimple: ", ERR_STR_LEN);
		vsnprintf(errstr + 10, ERR_STR_LEN - 10, str, arg);
		va_end(arg);
	/* We prefer to use the dyld error string if setget is 0 */
		if (setget == 0) {
			NSLinkEditError(&ler, &lerno, &file, &dylderrstr);
			fprintf(stderr,"dyld: %s\n",dylderrstr);
			if (dylderrstr && strlen(dylderrstr))
				strncpy(errstr,dylderrstr,ERR_STR_LEN);
		}		
		err_filled = 1;
		retval = NULL;
	}
	else
	{
		if (!err_filled)
			retval = NULL;
		else
			retval = errstr;
		err_filled = 0;
	}
	return retval;
}

/* dlopen */
void *dlopen(const char *path, int mode)
{
	void *module = 0;
	NSObjectFileImage ofi = 0;
	NSObjectFileImageReturnCode ofirc;
	static int (*make_private_module_public) (NSModule module) = 0;
	unsigned int flags =  NSLINKMODULE_OPTION_RETURN_ON_ERROR | NSLINKMODULE_OPTION_PRIVATE;

	/* If we got no path, the app wants the global namespace, use -1 as the marker
	   in this case */
	if (!path)
		return (void *)-1;

	/* Create the object file image, works for things linked with the -bundle arg to ld */
	ofirc = NSCreateObjectFileImageFromFile(path, &ofi);
	switch (ofirc)
	{
		case NSObjectFileImageSuccess:
			/* It was okay, so use NSLinkModule to link in the image */
			if (!(mode & RTLD_LAZY)) flags += NSLINKMODULE_OPTION_BINDNOW;
			module = NSLinkModule(ofi, path,flags);
			/* Don't forget to destroy the object file image, unless you like leaks */
			NSDestroyObjectFileImage(ofi);
			/* If the mode was global, then change the module, this avoids
			   multiply defined symbol errors to first load private then make
			   global. Silly, isn't it. */
			if ((mode & RTLD_GLOBAL))
			{
			  if (!make_private_module_public)
			  {
			    _dyld_func_lookup("__dyld_NSMakePrivateModulePublic", 
				(unsigned long *)&make_private_module_public);
			  }
			  make_private_module_public(module);
			}
			break;
		case NSObjectFileImageInappropriateFile:
			/* It may have been a dynamic library rather than a bundle, try to load it */
			module = (void *)NSAddImage(path, NSADDIMAGE_OPTION_RETURN_ON_ERROR);
			break;
		case NSObjectFileImageFailure:
			error(0,"Object file setup failure :  \"%s\"", path);
			return 0;
		case NSObjectFileImageArch:
			error(0,"No object for this architecture :  \"%s\"", path);
			return 0;
		case NSObjectFileImageFormat:
			error(0,"Bad object file format :  \"%s\"", path);
			return 0;
		case NSObjectFileImageAccess:
			error(0,"Can't read object file :  \"%s\"", path);
			return 0;		
	}
	if (!module)
		error(0, "Can not open \"%s\"", path);
	return module;
}

const char *dlerror(void)
{
	return error(1, (char *)NULL);
}

int dlclose(void *handle)
{
	if ((((struct mach_header *)handle)->magic == MH_MAGIC) ||
		(((struct mach_header *)handle)->magic == MH_CIGAM))
	{
		error(-1, "Can't remove dynamic libraries on darwin");
		return 0;
	}
	if (!NSUnLinkModule(handle, 0))
	{
		error(0, "unable to unlink module %s", NSNameOfModule(handle));
		return 1;
	}
	return 0;
}



void *dlsym(void *handle, const char *symbol)
{
	int sym_len = strlen(symbol);
	void *value = NULL;
	char *malloc_sym = NULL;
	NSSymbol *nssym = 0;
	malloc_sym = malloc(sym_len + 2);
	if (malloc_sym)
	{
		sprintf(malloc_sym, "_%s", symbol);
		/* If the handle is -1, if is the app global context */
		if (handle == (void *)-1)
		{
			/* Global context, use NSLookupAndBindSymbol */
			if (NSIsSymbolNameDefined(malloc_sym))
			{
				nssym = NSLookupAndBindSymbol(malloc_sym);
			}
		}
		/* Now see if the handle is a struch mach_header* or not, use NSLookupSymbol in image
		   for libraries, and NSLookupSymbolInModule for bundles */
		else
		{
			/* Check for both possible magic numbers depending on x86/ppc byte order */
			if ((((struct mach_header *)handle)->magic == MH_MAGIC) ||
				(((struct mach_header *)handle)->magic == MH_CIGAM))
			{
				if (NSIsSymbolNameDefinedInImage((struct mach_header *)handle, malloc_sym))
				{
					nssym = NSLookupSymbolInImage((struct mach_header *)handle,
												  malloc_sym,
												  NSLOOKUPSYMBOLINIMAGE_OPTION_BIND
												  | NSLOOKUPSYMBOLINIMAGE_OPTION_RETURN_ON_ERROR);
				}
	
			}
			else
			{
				nssym = NSLookupSymbolInModule(handle, malloc_sym);
			}
		}
		if (!nssym)
		{
			error(0, "Symbol \"%s\" Not found", symbol);
		}
		value = NSAddressOfSymbol(nssym);
		free(malloc_sym);
	}
	else
	{
		error(-1, "Unable to allocate memory");
	}
	return value;
}
