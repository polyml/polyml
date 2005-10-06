/*
    Title:      Foreign function interface
    Author:     Nick Chapman

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

/* Supported operating systems */

#include <stdio.h>
#include <stdlib.h>

#include <string.h>

#if defined(SOLARIS2) || defined(LINUX) || defined(FREEBSD)
#include <unistd.h> /* sleep */
#include <dlfcn.h>
#endif

#if defined(WINDOWS_PC)
#include <time.h>
#include <windows.h>
#include "Console.h" /* For hApplicationInstance. */
#else
/* UNIX version */
#include <time.h>
#endif

#if defined(FREEBSD) || defined(MACOSX)
#include <stdlib.h>
#elif defined(WINDOWS_PC)
#include <malloc.h>
#else
#include <alloca.h>
#endif

#if defined(MACOSX)
#include "dlfcn.h"
#endif

#include <assert.h>

#include <stdarg.h>
#include <errno.h>
#include "globals.h"
#include "objects.h"
#include "arb.h"
#include "reals.h"
#include "foreign.h"
#include "diagnostics.h"
#include "run_time.h"
#include "proper_io.h"
#include "sys.h"
#include "machine_dep.h"
#include "mpoly.h"
#include "processes.h"


/**********************************************************************
 *
 *  Stuff...
 *   
 **********************************************************************/
    
#define DLOPENFLAGS 1

#define Head(p)	(p->h)
#define Tail(p)	(p->t)

extern word *nil_value;
#define IsNil(p)  ((word*)p == nil_value)
#define NonNil(p) ((word*)p != nil_value)


#define UNHANDLE(h) 	DEREFWORDHANDLE(h)
#define SAVE(x)	    	push_to_save_vec((word)(x))

#define BYTES(x)        (OBJ_BYTE_BIT    | (x))
#define MUTABLE(x)  	(OBJ_MUTABLE_BIT | (x))


int in_foreign_code = 0; /* Start off not in foreign code */


/**********************************************************************
 *
 *  Foreign Debug
 *   
 **********************************************************************/


static int foreign_debug = 0;

/* Debug levels:
   0 - (no-debug)
   1 - (debug) 	  Bug specific tracing.
   2 - (info)	  Important info, e.g. the actual calls to foreign functions
   3 - (trace)	  Trace every c-function
   4 - (mes)	  Any old rubbish
   */

/*
   Level 1 can be used to track different bugs when they appear.
   For example, used it to watch Volatile GC when
   tracking `Invalid volatile' bug.
*/   


/***
 In each of the following macros, ARGS must be a bracketed arg list
 suitable for printf. This means usage will look like the following:
 (Note the double open/close brackets.)
 
       info(("example message with a parameter <%d>\n",parameter));
***/       


#define mesN(N,ARGS) \
    { \
      if (foreign_debug>=(N)) \
      { proper_printf("%s:%4i (%s) ", __FILE__, __LINE__, __FUNCTION__); \
        proper_printf ARGS; \
      } \
    }

#define show(ARGS) mesN(0,ARGS)
#define mes1(ARGS) mesN(1,ARGS)
#define mes2(ARGS) mesN(2,ARGS)
#define mes3(ARGS) mesN(3,ARGS)
#define mes4(ARGS) mesN(4,ARGS)


#define debug	mes1
#define info	mes2
#define trace	mes3
#define TRACE	mes3(("\n"))
#define mes 	mes4


static Handle get_foreign_debug (Handle ignored)
{
  TRACE;
  return Make_arbitrary_precision(foreign_debug);
}

static Handle set_foreign_debug (Handle h)
{
  TRACE;
  foreign_debug = get_C_long((word *)UNHANDLE(h));
  return h;
}
 

#define RAISE_EXN(string) raise_exception_string(EXC_foreign,(string))


/**********************************************************************
 *
 * Ctype enum
 * 
 **********************************************************************/

/*
   datatype RawCtype =
     Cchar
   | Cdouble
   | Cfloat
   | Cint
   | Clong
   | Cpointer
   | Cshort
   | Cstruct of int
*/

typedef enum {
  Cchar    = 1,
  Cdouble  = 2,
  Cfloat   = 3,
  Cint     = 4,
  Clong    = 5,
  Cpointer = 6,
  Cshort   = 7,
  Cstruct  = 8,  /* should never occur, since boxed value is untagged */
  Cuint    = 9
} Ctype;

static char *stringOfCtype(Ctype c)
{
  switch (c) {
  case Cchar    : return "Cchar";
  case Cdouble  : return "Cdouble";
  case Cfloat   : return "Cfloat";
  case Cint     : return "Cint";
  case Clong    : return "Clong";
  case Cpointer : return "Cpointer";
  case Cshort   : return "Cshort";
  case Cstruct  : return "Cstruct";
  case Cuint    : return "Cuint";
  default       : {
    static char buf[100];
    sprintf(buf,"Bad Ctype <%d>", c);
    return buf;
  }
  }
}


/**********************************************************************
 *
 * Volatiles ...
 * 
 **********************************************************************/

#define INITIAL_NUM_VOLS 200

/*typedef enum {False,True} Bool;*/
typedef int Bool; /* record size for malloc/free wrappers */

typedef struct {
  word* ML_pointer;		/* Pointer to ML token object. */
  void* C_pointer;		/* Pointer to C storage. */
  Bool Own_C_space;		/* Size if this is the owner of storage. */
} Volatile;


static Volatile *vols;

#define FIRST_VOL 0

static int num_vols = 0;
static int next_vol = FIRST_VOL;


/**********************************************************************
 *
 *  Expand Volatile array
 *   
 **********************************************************************/

static void expand_vol_array(void)
{
  int new_num_vols = (num_vols==0) ? INITIAL_NUM_VOLS : num_vols*2;
  info(("<%d> ---> <%d>\n", num_vols, new_num_vols));
  {
    Volatile *new_vols = (Volatile*)malloc(sizeof(Volatile)*new_num_vols);
    if (new_vols == NULL) {
      RAISE_EXN("Can't Enlarge Volatile Array");
      /*NOTREACHED*/
    }
    memcpy(new_vols,vols,num_vols*sizeof(Volatile));
    free(vols);
    vols = new_vols;
    num_vols = new_num_vols;
  }
}


/* This table contains all the callback functions that have been created.  Once a callback
   has been set up it remains in existence for the rest of the session. */
/* TODO: Grow this table dynamically. */
struct _cbStructEntry {
	word		*mlFunction;		/* The corresponding ML function. */
	word		*argType;			/* The argument type information. */
	unsigned char *cFunction;		/* The C function "stub" code. */
} callbackTable[] =
{
	{ NULL },
	{ NULL },
	{ NULL },
	{ NULL },
	{ NULL }
};


/**********************************************************************
 *
 *  Malloc / Free Wrappers
 *   
 **********************************************************************/

static int malloc_count = 0;
#if 0
#define Vmalloc(where,size) {where = malloc(size); printf("malloc: %p,%d\n",where,size); fflush(stdout); malloc_count++;}
#else
#define Vmalloc(where,size) {where = malloc(size); malloc_count++;}
#endif
#define Vfree(p) { free(p);  malloc_count--;}


/**********************************************************************
 *
 *  Volatile Allocation
 *   
 **********************************************************************/

/* With magic numbers */
#define VOL_BOX_SIZE	    	2
#define V_INDEX(v)  	    	(*((v)+1))
#define V_MAGIC(v)  	    	(*(v))
#define VOL_MAGIC_NUMBER    	65169
#define MakeVolMagic(v)	    	V_MAGIC((v)) = VOL_MAGIC_NUMBER
#define IsVolMagic(v)	    	(V_MAGIC((v)) == VOL_MAGIC_NUMBER)

/* No Magic...
#define VOL_BOX_SIZE	    	1
#define V_INDEX(v)  	    	(*(v))
#define MakeVolMagic(v)	    	
#define IsVolMagic(v)	    	1
*/

#define ML_POINTER(v) 	    	(vols[V_INDEX(v)].ML_pointer)
#define C_POINTER(v) 	    	(vols[V_INDEX(v)].C_pointer)
#define OWN_C_SPACE(v) 	    	(vols[V_INDEX(v)].Own_C_space)


/* Allocate a new "vol" in the table and in ML space and returns a pointer to the ML "vol". */
static Handle vol_alloc (void)
{

  Handle result = SAVE(alloc(MUTABLE(BYTES(VOL_BOX_SIZE))));
  word* v = UNHANDLE(result);
  
  trace(("index=<%d>\n",next_vol));
  if (next_vol >= num_vols) expand_vol_array();
  V_INDEX(v) = next_vol++;
  MakeVolMagic(v);
  ML_POINTER(v) = v;
  C_POINTER(v) = NULL;
  OWN_C_SPACE(v) = /*False*/0;

  return result;
}

/* Allocate a new "vol" in the table which points to a C object of size "size". */
static Handle vol_alloc_with_c_space (int size)
{
  Handle res = vol_alloc();
  trace(("size= %d\n",size));
  Vmalloc( C_POINTER(UNHANDLE(res)), size );
  OWN_C_SPACE(UNHANDLE(res)) = /*True*/size;
  return res;
}


/**********************************************************************
 *
 *  Volatile Dereferencing --- A safe version of C_POINTER
 *   
 **********************************************************************/

/* Returns the C-pointer component corresponding to the "vol" argument. */
static void* DEREFVOL (word *v)
{ TRACE; {

  int index = V_INDEX(v);
  trace(("<%d>\n",index));

  if (!(IsVolMagic(v))) {
    info (("Invalid volatile -- bad magic number, index=<%d>\n", index));
    RAISE_EXN("Bad volatile magic number");
  }
			   
  if (index < num_vols) {
    if (vols[index].ML_pointer == v) {
      /* everything is okay */
      return vols[index].C_pointer;
      
    } else {
      info(("Invalid volatile -- backpointer is wrong <%d>: <%p> != <%p>\n",
	   index, (void *)v, (void *)vols[index].ML_pointer));
    }
  } else {
    info(("Invalid volatile -- no such vol index <%d>\n", index));
  }
  RAISE_EXN("Invalid volatile");
  /*NOTREACHED*/
}}

/**********************************************************************
 *
 *  Volatile Sanity Checking (debug only)
 *   
 **********************************************************************/
#if 0
static void sanity_check_vols(void)
{ TRACE; {
  int v, v2;
  for (v=FIRST_VOL; v < next_vol; v++) {
    if (vols[v].ML_pointer == NULL) {
      trace(("vol <%d> has nulled ML_pointer\n",v));
    } else {
      v2 = V_INDEX(vols[v].ML_pointer);
      if ( v2 != v ) {
	info(("vol <%d> has been corrupted to <%d>\n",v,v2));
      }
    }
  }
}}
#endif

/**********************************************************************
 *
 *  C Programming Primitives
 *   
 **********************************************************************/

/* This is roughly equivalent to calling malloc via the FFI, except that the vol it
   returns refers to the newly allocated store.  Typically we need a pointer to the
   store so we need to call address(alloc s). */
static Handle allocate (Handle h)
{ TRACE; {
  int size = get_C_long(UNHANDLE(h)); /* bytes */

  Handle space = vol_alloc_with_c_space(size);
  return space;
}}

/* Constructs a one-word C object whose value is the C-pointer of the argument. */
static Handle address (Handle h)
{ TRACE; {
  Handle res = vol_alloc_with_c_space(sizeof(void*));
  *(void**)C_POINTER(UNHANDLE(res)) = DEREFVOL(UNHANDLE(h));
  return res;
}}

/* Returns a vol containing the value at the address given in its argument. */
static Handle deref (Handle h)
{ TRACE; {
  Handle res = vol_alloc();
  C_POINTER(UNHANDLE(res))= *(void**)DEREFVOL(UNHANDLE(h));
  return res;
}}


/******
 * offset is intended for accessing structure components.
 * pointer_add can be written in terms offset,address & deref:
 *   pointer_add p n == address (offset (deref p) n)
 ******/
/* This seems odd to me (DCJM) but it makes sense when you realise that a pointer is
   a vol whose C-pointer value contains the address of the vol containing the storage.
   There's always one more level of indirection than you think.  DCJM 12/4/04. */
static Handle offset (Handle h)
{ TRACE; {
  Handle res = vol_alloc();
  word *structure  = (word*)UNHANDLE(h)[0];
  int num_bytes = get_C_long((word*)UNHANDLE(h)[1]);

  C_POINTER(UNHANDLE(res)) = (char*)DEREFVOL(structure) + num_bytes;
  return res;
}}
  

static Handle assign (Handle h)
{ TRACE; {

  word *left  = (word*)UNHANDLE(h)[0];
  word *right = (word*)UNHANDLE(h)[1];
  int size = get_C_long((word*)UNHANDLE(h)[2]); /* bytes */
  int i;

  for (i=0; i<size; i++) {
    ((char*)C_POINTER(left))[i] = ((char*)DEREFVOL(right))[i];
  }

  return h; /* to be ignored */
}}


static Handle c_sizeof (Handle h)
{ TRACE; {
  word* v = UNHANDLE(h);
  
    if (!(IS_INT(v))) {
      int size = get_C_long((word*)*v);
      trace(("Cstruct, size <%d>\n", size));
      return Make_arbitrary_precision(size);
    }
    else {
      Ctype ctype = UNTAGGED(v);
      trace(("<%s>\n", stringOfCtype(ctype)));
      switch (ctype) {
      case Cchar    : return Make_arbitrary_precision(sizeof(char));
      case Cdouble  : return Make_arbitrary_precision(sizeof(double));
      case Cfloat   : return Make_arbitrary_precision(sizeof(float));
      case Cint     : return Make_arbitrary_precision(sizeof(int));
      case Clong    : return Make_arbitrary_precision(sizeof(long));
      case Cpointer : return Make_arbitrary_precision(sizeof(void*));
      case Cshort   : return Make_arbitrary_precision(sizeof(short));
      case Cuint    : return Make_arbitrary_precision(sizeof(unsigned));
      default: {
		char buf[100];
		sprintf(buf, "Unknown ctype <%s>", stringOfCtype(ctype));
		RAISE_EXN(buf);
		/*NOTREACHED*/
		/* Keep -Wall happy */ return (Handle)0;
      }
      }
    }
}}


static Handle alignment (Handle h)
{ TRACE; {
  word* v = UNHANDLE(h);
  
    if (!(IS_INT(v))) {
      RAISE_EXN("alignment of structure"); 
    }
    else {
      Ctype ctype = UNTAGGED(v);
      trace(("<%s>\n", stringOfCtype(ctype)));
      switch (ctype) {
#ifdef __GNUC__	
      case Cchar    : return Make_arbitrary_precision(__alignof__(char));
      case Cdouble  : return Make_arbitrary_precision(__alignof__(double));
      case Cfloat   : return Make_arbitrary_precision(__alignof__(float));
      case Cint     : return Make_arbitrary_precision(__alignof__(int));
      case Clong    : return Make_arbitrary_precision(__alignof__(long));
      case Cpointer : return Make_arbitrary_precision(__alignof__(void*));
      case Cshort   : return Make_arbitrary_precision(__alignof__(short));
      case Cuint    : return Make_arbitrary_precision(__alignof__(unsigned));
#else
      /* Take a guess... */
	  /* Use "sizeof" here.  DCJM 19/4/01. */
      case Cchar    : return Make_arbitrary_precision(sizeof(char));
      case Cdouble  : return Make_arbitrary_precision(sizeof(double));
      case Cfloat   : return Make_arbitrary_precision(sizeof(float));
      case Cint     : return Make_arbitrary_precision(sizeof(int));
      case Clong    : return Make_arbitrary_precision(sizeof(long));
      case Cpointer : return Make_arbitrary_precision(sizeof(void*));
      case Cshort   : return Make_arbitrary_precision(sizeof(short));
      case Cuint    : return Make_arbitrary_precision(sizeof(unsigned));
#endif	
      default: {
	char buf[100];
	sprintf(buf, "Unknown ctype <%s>", stringOfCtype(ctype));
	RAISE_EXN(buf);
      }
      }
    }
  /*NOTREACHED*/
  /* Keep -Wall happy */ return (Handle)0;
}}


/**********************************************************************
 *
 *  Volatile Garbage Collection
 *   
 **********************************************************************

During GC this function is called three times, with "op" taking the values
    1 -- MarkRuntimeObject
    2 -- ReferenceRuntimeObject
    3 -- UpdateRuntimeObject

Pass 1 is irrelevant. We call "op" (MarkRuntimeObject) with the second
parameter equal to 1.  This indicates the ML pointers held by the
array "vols" are weak references and so "op" does nothing and returns
immediately.

During Pass 2, the condition "(vols[from].ML_pointer != NULL)" is
always true, so we call "op" (ReferenceRuntimeObject) with the address
of each ML pointer cell contained in the array "vols".  Any cell that
holds a heap pointer that is not marked is overwritten by "op" to be
NULL. After the call to "op" we test if the cell has been nulled, if
so we deallocate the malloced space pointed to by the corresponding C
pointer.

Also, because the condition "(vols[from].ML_pointer != NULL)" is true
on every iteration of the loop, the counter "to" always keeps in step
with the loop counter "from". This prevents us from entering the code
guarded by the condition "(from>to)" (intended for pass 3).

During the Pass 3, we only call "op" (UpdateRuntimeObject) with the
address of those ML pointers that were not nulled on the previous
stage. The ML pointers are overwritten by "op" with their new location
in the heap.

Also, on this pass we compact the array "vols". The "to" counter is
only incremented when we encounter an ML pointer that is not NULL,
whereas the "from" counter is incremented on every iteration.  If
"from" > "to" we shift the pair of pointers (ML and C) leftwards in the
array from index "from" to index "to" . This requires us to update the
index held by the ML heap cell to be "to".

Finally we set "next_vol" to be "to", the index of the next volatile
to be allocated.  Although this occurs on every pass, it only has
effect on Pass 3.

**********************************************************************/

void volatile_gc (GCOpFunc op)
{ TRACE; {
  int to,from, i;

  for (from=FIRST_VOL, to=FIRST_VOL; from < next_vol; from++) {
    mes(("to=<%d> from=<%d>\n",to,from));

    if (vols[from].ML_pointer != NULL) {
      op ( &(vols[from].ML_pointer), 1 /* weak */);

      if (vols[from].ML_pointer == NULL) { /* It's no longer reachable. */
		if (vols[from].Own_C_space) {

		  mes(("Trashing malloc space of <%d>\n",from));
		  {int i; for (i=0; i<vols[from].Own_C_space; i++) {
			((char*)vols[from].C_pointer)[i] = 0;
		  }}

		  trace(("Freeing malloc space of <%d>\n",from));
		  Vfree(vols[from].C_pointer);
		}
      }

      if (from>to) {
		trace(("Shifting volatile <%d> ---> <%d>\n",from,to));
		vols[to] = vols[from];
		V_INDEX(vols[to].ML_pointer) = to;
      }
      to++;
    }
  }
  next_vol = to;
  info(("unfreed mallocs=<%d> next_vol=<%d>\n", malloc_count, next_vol));

	/* Callback table.  Added DCJM 12/4/04.  We always process these as strong references.
	   For the time being at any rate we treat these as permanent entries so that once a
       callback is set up it cannot be garbage-collected. */
	for (i = 0; i < sizeof(callbackTable)/sizeof(callbackTable[0]); i++) {
		if (callbackTable[i].mlFunction != NULL) {
			op (&(callbackTable[i].mlFunction), 0 /* strong */);
			op (&(callbackTable[i].argType), 0 /* strong */);
		}
	}
}}


/**********************************************************************
 *
 *  Load a Dynamic Library.
 *   
 **********************************************************************/

static Handle load_lib (Handle string)
{
  char name[500];

  Poly_string_to_C(DEREFSTRINGHANDLE(string),name,sizeof(name));
  info(("<%s>\n", name));

#if defined(WINDOWS_PC)
  {
    HINSTANCE lib = LoadLibrary(name);
    if (lib == NULL) 
    {
      char buf[256];
      sprintf(buf, "load_lib <%s> : %i", name, GetLastError());
      RAISE_EXN(buf);
    }

    {
      Handle res = vol_alloc_with_c_space(sizeof(void*));
      *(void**)DEREFVOL(UNHANDLE(res)) = lib;
      return res;
    }
  }
#else  /* UNIX version */
  {
    void *lib = dlopen(name,DLOPENFLAGS);
    if (!lib)
    {
      char buf[256];
      sprintf(buf, "load_lib <%s> : %s", name, dlerror());
      RAISE_EXN(buf);
    }

    {
      Handle res = vol_alloc_with_c_space(sizeof(void*));
      *(void**)DEREFVOL(UNHANDLE(res)) = lib;
      return res;
    }
  }
#endif
}


/**********************************************************************
 *
 *  Load Symbol from a Dynamic Library
 *   
 **********************************************************************/

static Handle load_sym (Handle h)
{
  char name[500];
  
  Poly_string_to_C((pstring)DEREFHANDLE(h)[1],name,sizeof(name));
  info(("<%s>\n", name));

#if defined(WINDOWS_PC)
  {
    FARPROC sym = GetProcAddress( *(void**)DEREFVOL(DEREFHANDLE(h)[0]), name);
    
    if (sym == NULL) 
    {
      char buf[256];
      sprintf(buf, "load_sym <%s> : %i", name,GetLastError());
      RAISE_EXN(buf);
    }

    {
      Handle res = vol_alloc_with_c_space(sizeof(void*));
      *(void**)DEREFVOL(UNHANDLE(res)) = sym;
      return res;
    }
  }
  
#else /* UNIX version */
  {
    void *sym = dlsym( *(void**)DEREFVOL(DEREFHANDLE(h)[0]), name );
    
    if (!sym)
    {
      char buf[256];
      sprintf(buf, "load_sym <%s> : %s", name, dlerror());
      RAISE_EXN(buf);
    }

    {
      Handle res = vol_alloc_with_c_space(sizeof(void*));
      *(void**)DEREFVOL(UNHANDLE(res)) = sym;
      return res;
    }
  }
#endif
}


/**********************************************************************
 *
 *  Call a symbol with a list of conversion/argument pairs
 *   
 **********************************************************************/


typedef void*	(*ftype)();


#define STRUCT(n) struct {char xyz[n];}

#define MAX_STRUCT_SIZE 1024
typedef STRUCT(MAX_STRUCT_SIZE) STRUCT_MAX;

static void print_call
(
  void* a1,  void* a2,  void* a3,  void* a4,  void* a5,
  void* a6,  void* a7,  void* a8,  void* a9,  void* a10,
  void* a11, void* a12, void* a13, void* a14, void* a15,
  void* b1,  void* b2,  void* b3,  void* b4,  void* b5,
  void* b6,  void* b7,  void* b8,  void* b9,  void* b10,
  void* b11, void* b12, void* b13, void* b14, void* b15
)
{
  info (("<%p> <%p> <%p> <%p> <%p> <%p> <%p> <%p> <%p> <%p>"
         "<%p> <%p> <%p> <%p> <%p> <%p> <%p> <%p> <%p> <%p>"
	     "<%p> <%p> <%p> <%p> <%p> <%p> <%p> <%p> <%p> <%p>\n", 
	     a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,
	     b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15));
}

/* Macro to call a function and get a particular argument type.
   Note: We only allocate the result handle once the function has
   returned.  The Windows GUI code may make a call back into ML and
   that can disrupt the save vector.
   DCJM 6/6/01.  */
/*
Since we now support callbacks in Unix as well I've extended this to
Unix.  This will add a small overhead to every function call since the
ML code will always make a second call into the runtime-system to get
the function result.
DCJM 7/4/04
*/
#ifdef WINDOWS_PC
#define CALL_TYPED(TYPE)	\
	do { \
		Handle res;\
		TYPE result;\
		MD_set_for_retry(POLY_SYS_foreign_result);\
		in_foreign_code = 1;\
		result = ((TYPE(*)())fun)(a1,\
			a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,b1,b2,b3,b4,b5,b6,\
			b7,b8,b9,b10,b11,b12,b13,b14,b15);\
		in_foreign_code = 0;\
		res = vol_alloc_with_c_space(sizeof(TYPE));\
		*(TYPE*)DEREFVOL(UNHANDLE(res)) = result;\
		runtime_assign_word((word *) (&(DEREFPROCHANDLE(processes)->lastCallResult)), (word)UNHANDLE(res));\
		runtime_assign_word((word *)(&(DEREFPROCHANDLE(processes)->lastErrNo)),TAGGED(errno));\
		runtime_assign_word((word *)(&(DEREFPROCHANDLE(processes)->lastErrcode)),TAGGED(-(int)GetLastError()));\
		select_next_process();\
		RE_ENTER_POLY(100);\
	} while (0)
#else
#define CALL_TYPED(TYPE)	\
	do { \
		Handle res;\
		TYPE result;\
		MD_set_for_retry(POLY_SYS_foreign_result);\
		in_foreign_code = 1;\
		result = ((TYPE(*)())fun)(a1,\
			a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,b1,b2,b3,b4,b5,b6,\
			b7,b8,b9,b10,b11,b12,b13,b14,b15);\
		in_foreign_code = 0;\
		res = vol_alloc_with_c_space(sizeof(TYPE));\
		*(TYPE*)DEREFVOL(UNHANDLE(res)) = result;\
		runtime_assign_word((word *) (&(DEREFPROCHANDLE(processes)->lastCallResult)), (word)UNHANDLE(res));\
		runtime_assign_word((word *)(&(DEREFPROCHANDLE(processes)->lastErrNo)),TAGGED(errno));\
		select_next_process();\
		RE_ENTER_POLY(100);\
	} while (0)
#endif

/* current version - all platforms */
static Handle apply_rec (int iter,ftype fun,word** conv,word *ret_conv,word **args,
		          void *a1,void *a2,void *a3,void *a4,void *a5,
		          void *a6,void *a7,void *a8,void *a9,void *a10,
		          void *a11,void *a12,void *a13,void *a14,void *a15,
                  void *b1,void *b2,void *b3,void *b4,void *b5,
                  void *b6,void *b7,void *b8,void *b9,void *b10,
                  void *b11,void *b12,void *b13,void *b14,void *b15)
{
  mes(("iter = <%d> args = "
       "<%p> <%p> <%p> <%p> <%p> <%p> <%p> <%p> <%p> ,%p>"
	   "<%p> <%p> <%p> <%p> <%p> <%p> <%p> <%p> <%p> ,%p>"
	   "<%p> <%p> <%p> <%p> <%p> <%p> <%p> <%p> <%p> ,%p>\n",
       iter, 
       a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,
       b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15));
  
  if (iter < 0) {
    mes(("Calling foreign function\n"));
    print_call(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,
               b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15);

    if (!(IS_INT(ret_conv)))
    {
		/* Structs as results.  Now that I've removed structs as arguments
		   I'm strongly tempted to forbid structs as results as well.  I guess
		   there's more of a case for allowing structs as results because it's
		   not possible to implement them any other way.
		   DCJM 14/4/04. */
      typedef struct { int x0;  } small_struct1;
      typedef struct { int x0; int x1; } small_struct2;
      typedef struct { int x0; int x1; int x2; } small_struct3;
      typedef struct { int x0; int x1; int x2; int x3; } small_struct4;
    
      int size = get_C_long(*(word**)ret_conv);
      info(("Expecting return type Cstruct, size <%d>\n", size));
      
      /* We have to treat small structures specially, because GCC does.
         It would be nice to insist that the library we are calling
         should be compiled with "-fpcc-struct-return" but that's
         not practical at the moment for the Siemens BDD libraries,
         which we need to access. Hopefully, these special cases
         should give us a minor performance boost too.
         SPF 31/3/1998
      */
      /* It seems that two-word structures, at least, are implemented
	     using registers in Visual C++ V5 so some of these special
		 cases are needed in the Windows version.  There's no harm
	     in treating several other of the cases specially as well.
	     DCJM 8/10/1999
	  */

      if (size == sizeof(small_struct1)) CALL_TYPED(small_struct1);
      else if (size == sizeof(small_struct2)) CALL_TYPED(small_struct2);
      else if (size == sizeof(small_struct3)) CALL_TYPED(small_struct3);
      else if (size == sizeof(small_struct4)) CALL_TYPED(small_struct4);

      else 
		if (size > sizeof(STRUCT_MAX))
      {
		char buf[100];
		sprintf(buf, "Required size of return structure <%d> is too large", size);
		raise_exception_string(EXC_foreign, buf);
      }
      else
      {
        /* We call the function saying that it will return a STRUCT_MAX.
           It's OK that the function probably returns something smaller,
           because space is allocated by the caller, not the callee.
           Then we have to copy the result into a STRUCT_MAX.
           Finally, we have to extract the part we're really interested
           in and copy that into the volatile. 
           
           Note that
           
              (1) We copy the result twice - yeuch.
              (2) The whole scheme falls over completely if the
                  callee passes the result back in registers
                  rather than using the pre-allocated space.
              (3) Compiling the callee with -fpcc-struct-return
                  would solve this problem (for GCC) but wouldn't
                  work if the callee needs to call some other code
                  that is compiled use -freg-struct-return. This
                  is currently an issue for the HP version of the
                  makefsm libraries.
              (4) That's why we treat small structures specially!
                  
           SPF 31/3/1998
        */
      
	Handle res = vol_alloc_with_c_space(size);
	in_foreign_code = 1;

	{
	   STRUCT_MAX temp = 
	      ((STRUCT_MAX(*)())fun)
	        (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,
	         b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15);

	   memcpy(DEREFVOL(UNHANDLE(res)), &temp, size);
        }


		in_foreign_code = 0;
		mes(("Returning from foreign function\n"));
		return res;
      }
    }
    else {
      Ctype ctype = UNTAGGED(ret_conv);
    
      info(("Expecting return type <%s>\n", stringOfCtype(ctype)));
      switch (ctype) {
      case Cchar: CALL_TYPED(char);
      case Cdouble: CALL_TYPED(double);
      case Cfloat: CALL_TYPED(float);
      case Cint: CALL_TYPED(int);
      case Clong: CALL_TYPED(long);
      case Cpointer: CALL_TYPED(void*);
      case Cshort: CALL_TYPED(short);
      case Cuint: CALL_TYPED(unsigned);
      default: {
		char buf[100];
		sprintf(buf, "Unknown return convention <%s>", stringOfCtype(ctype));
		raise_exception_string(EXC_foreign, buf);
      }
      
      }
    }
    
  }
  else {
    word* arg = DEREFVOL(args[iter]);
    
    if (!(IS_INT(conv[iter]))) {
	  /* This code previously created a temporary struct, copied
	     the argument in there and then passed the address of this
	     as the argument.  Of the machines and systems I have here
	     the only one to do that is Sparc/Solaris.  GCC on i386
	     (Linux and FreeBSD), GCC on PPC (Mac OS X) and MS VS 6
	     all pass structs by copying the words and passing them
	     as actual arguments.  Because of this uncertainty I've
	     decided to forbid structs as arguments and raise an
	     exception here.  DCJM 14/4/04. */
		RAISE_EXN("Structs as function arguments are not supported.\n");
     }
    else {
      Ctype ctype = UNTAGGED(conv[iter]);
      info(("<%s>\n", stringOfCtype(ctype)));
      switch (ctype) {

      case Cchar:
		return apply_rec(iter-1, fun, conv, ret_conv, args, (void*)(long)(*(char*)arg), 
		 a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,b1,b2,b3,b4,b5,b6,
		 b7,b8,b9,b10,b11,b12,b13,b14);

      case Cdouble: /* Double is two words -- pass them separately */
		return apply_rec(iter-1, fun, conv, ret_conv, args, (void*)arg[0], (void*)arg[1] ,  
		 a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,b1,b2,b3,b4,b5,b6,
		 b7,b8,b9,b10,b11,b12,b13);

      case Cfloat:
		return apply_rec(iter-1, fun, conv, ret_conv, args, (void*)*arg,  
		 a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,b1,b2,b3,b4,b5,b6,
		 b7,b8,b9,b10,b11,b12,b13,b14);

      case Cint:
		return apply_rec(iter-1, fun, conv, ret_conv, args, (void*)*arg,  
		 a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,b1,b2,b3,b4,b5,b6,
		 b7,b8,b9,b10,b11,b12,b13,b14);

      case Clong:
		return apply_rec(iter-1, fun, conv, ret_conv, args, (void*)(*(long*)arg),  
		 a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,b1,b2,b3,b4,b5,b6,
		 b7,b8,b9,b10,b11,b12,b13,b14);

      case Cpointer:
		return apply_rec(iter-1, fun, conv, ret_conv, args, (void*)*arg,  
		 a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,b1,b2,b3,b4,b5,b6,
		 b7,b8,b9,b10,b11,b12,b13,b14);

      case Cshort:
		return apply_rec(iter-1, fun, conv, ret_conv, args, (void*)(long)(*(short*)arg),  
		 a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,b1,b2,b3,b4,b5,b6,
		 b7,b8,b9,b10,b11,b12,b13,b14);

      case Cuint:
		return apply_rec(iter-1, fun, conv, ret_conv, args, (void*)(*(unsigned*)arg),  
		 a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,b1,b2,b3,b4,b5,b6,
		 b7,b8,b9,b10,b11,b12,b13,b14);

      default: {
		char buf[100];
		sprintf(buf, "Unknown calling convention <%s>", stringOfCtype(ctype));
		raise_exception_string(EXC_foreign, buf);
      }
      }
    }
  }

  /*NOTREACHED*/
  /* Keep -Wall happy */ return (Handle)0;
}


static int length_list (ML_Cons_Cell *p)
{
  TRACE; {
    return IsNil(p) ? 0 : 1 + length_list (Tail(p));
  }
}

static Handle call_sym (Handle h)
{
  TRACE; {
    ftype sym               = *(ftype*)DEREFVOL(DEREFHANDLE(h)[0]);
    ML_Cons_Cell *arg_list  = ((ML_Cons_Cell**)DEREFHANDLE(h))[1];
    int num_args            = length_list(arg_list);
    word* ret_conv  	    = DEREFHANDLE(h)[2];

    
    if (num_args > 15) /* was 9, but increased for WNT calls. SPF 3/5/95 */
    {
      RAISE_EXN("Too many args\n");
    }

    {	
      word** arg_tuple = (word**)alloca(num_args * sizeof(word*));
      word** conv      = (word**)alloca(num_args * sizeof(word*));

      int i;
      ML_Cons_Cell *p;

      for (i=0, p=arg_list; i<num_args; i++,p=Tail(p)) {
        conv[i]      = (word*)Head(p)[0];
        arg_tuple[i] = (word*)Head(p)[1];
      }

      {
		Handle res = 
		  apply_rec (num_args-1, sym, conv, ret_conv, arg_tuple, 
					 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
					 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);

		return res;
      }
      
    }
  }
}


/**********************************************************************
 *
 *  Char Conversion
 *   
 **********************************************************************/

/* This is backwards compatible with the old code in which toCchar and fromCchar
   worked on "string" rather than "char".  Since single character strings and
   chars have the same representation there's no need to change this.
   DCJM 27/6/01. */
static Handle toCchar (Handle h)
{
  char s[2];
  Poly_string_to_C(DEREFSTRINGHANDLE(h),s,2);
  mes(("<%c>\n", s[0]));
  {
    Handle res = vol_alloc_with_c_space(sizeof(char));
    *(char*)DEREFVOL(UNHANDLE(res)) = s[0];
    return res;
  }
}

static Handle fromCchar (Handle h)
{
  char c;
  c = *(char*)DEREFVOL(UNHANDLE(h));
  mes(("<%c>\n", c));
  return SAVE(Buffer_to_Poly(&c,1));
}


/**********************************************************************
 *
 *  Double Conversion
 *   
 **********************************************************************/

static Handle toCdouble (Handle h)
{
  double d = real_arg(h);
  mes(("<%f>\n", d));
  {
    Handle res = vol_alloc_with_c_space(sizeof(double));
    *(double*)DEREFVOL(UNHANDLE(res)) = d;
    return res;
  }
}

static Handle fromCdouble (Handle h)
{
  double d = *(double*)DEREFVOL(UNHANDLE(h));
  mes(("<%f>\n", d));
  return SAVE(real_result(d));
}


/**********************************************************************
 *
 *  Float Conversion
 *   
 **********************************************************************/

static Handle toCfloat (Handle h)
{
  float f = (float)real_arg(h);
  mes(("<%f>\n", f));
  {
    Handle res = vol_alloc_with_c_space(sizeof(float));
    *(float*)DEREFVOL(UNHANDLE(res)) = f;
    return res;
  }
}

static Handle fromCfloat (Handle h)
{
  float f = *(float*)DEREFVOL(UNHANDLE(h));
  mes(("<%f>\n", f));
  return SAVE(real_result((double)f));
}


/**********************************************************************
 *
 *  Int Conversion
 *   
 **********************************************************************/

static Handle toCint (Handle h)
{
  int i = get_C_long((word*)UNHANDLE(h));
  mes(("value = %d\n", i));
  {
    Handle res = vol_alloc_with_c_space(sizeof(int));
    *(int*)DEREFVOL(UNHANDLE(res)) = i;
    return res;
  }
}

static Handle fromCint (Handle h)
{
  int i = *(int*)DEREFVOL(UNHANDLE(h));
  mes(("<%d>\n", i));
  return Make_arbitrary_precision(i);
}


/**********************************************************************
 *
 *  Long Conversion
 *   
 **********************************************************************/

static Handle toClong (Handle h)
{
  long i = get_C_long((word*)UNHANDLE(h));
  mes(("value = %d\n", (int)i));
  {
    Handle res = vol_alloc_with_c_space(sizeof(long));
    *(long*)DEREFVOL(UNHANDLE(res)) = i;
    return res;
  }
}

static Handle fromClong (Handle h)
{
  long i = *(long*)DEREFVOL(UNHANDLE(h));
  mes(("<%d>\n", (int)i));
  return Make_arbitrary_precision(i);
}


/**********************************************************************
 *
 *  Short Conversion
 *   
 **********************************************************************/

static Handle toCshort (Handle h)
{
  short i = get_C_long((word*)UNHANDLE(h));
  mes(("<%d>\n", (int)i));
  {
    Handle res = vol_alloc_with_c_space(sizeof(short));
    *(short*)DEREFVOL(UNHANDLE(res)) = i;
    return res;
  }
}

static Handle fromCshort (Handle h)
{
  short i = *(short*)DEREFVOL(UNHANDLE(h));
  mes(("<%d>\n", (int)i));
  return Make_arbitrary_precision(i);
}


/**********************************************************************
 *
 *  Unsigned int Conversion
 *   
 **********************************************************************/

static Handle toCuint (Handle h)
{
  unsigned i = get_C_ulong((word*)UNHANDLE(h));
  mes(("value = %d\n", (int)i));
  {
    Handle res = vol_alloc_with_c_space(sizeof(unsigned));
    *(unsigned*)DEREFVOL(UNHANDLE(res)) = i;
    return res;
  }
}

static Handle fromCuint (Handle h)
{
  unsigned i = *(unsigned*)DEREFVOL(UNHANDLE(h));
  mes(("<%d>\n", (int)i));
  return Make_unsigned(i);
}

/**********************************************************************
 *
 *  String Conversion
 *   
 **********************************************************************/

#define PSTRING_LENGTH(pstr) \
    (IS_INT((pstr)) ? 1 : (pstr)->length)

static Handle fillCstring (Handle h)
{ TRACE; {
  int size = PSTRING_LENGTH((pstring)DEREFHANDLE(h)[1]) + 1;
  Poly_string_to_C((pstring)DEREFHANDLE(h)[1],DEREFVOL(DEREFHANDLE(h)[0]),size);
  mes(("<%s>\n", (char*)C_POINTER(DEREFHANDLE(h)[0])));
  return h; /* to be ignored */
}}


static Handle toCstring (Handle h)
{ TRACE; {
  int size = PSTRING_LENGTH(DEREFSTRINGHANDLE(h)) + 1;

  /* Allocate c-space for both the string & a pointer to the string,
     which is owned by the same vol. */
  Handle res = vol_alloc_with_c_space(sizeof(char*)+size);

  /* Make the first word of the c-space point to the second word */
  *(void**)C_POINTER(UNHANDLE(res)) = 1 + (void**)C_POINTER(UNHANDLE(res));

  /* Copy the string into the c-space starting at the second word */
  Poly_string_to_C
    (DEREFSTRINGHANDLE(h),
     (char*)(1 + (void**)C_POINTER(UNHANDLE(res))),
     size);
  return res;
}}

static Handle fromCstring (Handle h)
{ TRACE; {
  char* str = *(char**)DEREFVOL(UNHANDLE(h));
  mes(("<%s>\n", str));
  return SAVE(C_string_to_Poly(str));
}}

/* Byte vector functions.  The representation is the same as a string but
   the values may include zero bytes.  For fromCbytes the length
   therefore has to be passed as an argument.   Added DCJM 29/6/01. */
static Handle toCbytes (Handle h)
{ TRACE; {
  int size = PSTRING_LENGTH(DEREFSTRINGHANDLE(h));

  /* Allocate c-space for both the string & a pointer to the string,
     which is owned by the same vol. */
  Handle res = vol_alloc_with_c_space(sizeof(char*)+size);
  char	**p = (char**)C_POINTER(UNHANDLE(res));

  /* Make the first word of the c-space point to the second word */
  *(char***)p = p + 1;

  /* Copy the string into the c-space starting at the second word */
  if (size == 1) **p = UNTAGGED(DEREFHANDLE(h));
  else memcpy(*p, DEREFSTRINGHANDLE(h)->chars, size);

  return res;
}}

#define EMPTYSTRING interface_map[POLY_SYS_nullorzero]

static Handle fromCbytes (Handle h)
{ TRACE; {
  char* str = *(char**)DEREFVOL(DEREFHANDLE(h)[0]);
  int size = get_C_long(DEREFHANDLE(h)[1]);
  if (str == NULL) return SAVE(EMPTYSTRING);
  else return SAVE(Buffer_to_Poly(str, size));
}}


/**********************************************************************
 *
 *  call_sym_and_convert
 *   
 **********************************************************************/

/* these #defines now all use the TAGGED macro for portability SPF 19/10/94 */
#define directedArg_In  TAGGED(1)
#define directedArg_Out TAGGED(2)

/*
    datatype 'a union =
        Char    of string
      | Double  of real
      | Float   of real
      | Int     of int
      | Long    of int
      | Short   of int
      | String  of string
      | Vol     of 'a
*/
#define union_Char  	TAGGED(1)
#define union_Double	TAGGED(2)
#define union_Float 	TAGGED(3)
#define union_Int   	TAGGED(4)
#define union_Long  	TAGGED(5)
#define union_Short 	TAGGED(6)
#define union_String	TAGGED(7)
#define union_Vol    	TAGGED(8)
#define union_Uint  	TAGGED(9)


static Handle UNION_MAKE(int tag, Handle contents)
{
  Handle res = SAVE(alloc(2));
  UNHANDLE(res)[1] = tag;
  DEREFHANDLE(res)[0] = UNHANDLE(contents);
  return res;
}

/*
    datatype 'ctype unionChoice =
        chooseChar
      | chooseDouble
      | chooseFloat
      | chooseInt
      | chooseLong
      | chooseShort
      | chooseString;
      | chooseVol of 'ctype
*/
#define choice_chooseChar  	TAGGED(1)
#define choice_chooseDouble	TAGGED(2)
#define choice_chooseFloat 	TAGGED(3)
#define choice_chooseInt   	TAGGED(4)
#define choice_chooseLong  	TAGGED(5)
#define choice_chooseShort 	TAGGED(6)
#define choice_chooseString	TAGGED(7)
#define choice_chooseVol    	TAGGED(8) /* not used, since boxed value is untagged */
#define choice_chooseUint  	TAGGED(9)


#define TAG(x)	    	((int)(UNHANDLE(x)[1]))
#define CONTENTS(x)  	(SAVE(UNHANDLE(x)[0]))


#define LIST_ISNULL(x)	(IsNil(UNHANDLE(x)))
#define LIST_HEAD(x)	(SAVE(Head(DEREFLISTHANDLE(x))))
#define LIST_TAIL(x)	(SAVE(Tail(DEREFLISTHANDLE(x))))
#define LIST_NULL   	(SAVE(nil_value))

static Handle LIST_CONS (Handle x,Handle xs)
{
  Handle res = SAVE(alloc(sizeof(ML_Cons_Cell)));
  Head(DEREFLISTHANDLE(res)) = UNHANDLE(x);
  Tail(DEREFLISTHANDLE(res)) = DEREFLISTHANDLE(xs);
  return res;
}

#define TUPLE_GET1(x)  (SAVE(DEREFHANDLE(x)[0]))
#define TUPLE_GET2(x)  (SAVE(DEREFHANDLE(x)[1]))
#define TUPLE_GET3(x)  (SAVE(DEREFHANDLE(x)[2]))

static Handle TUPLE_MAKE2 (Handle x,Handle y)
{
  Handle res = SAVE(alloc(2));
  DEREFHANDLE(res)[0] = UNHANDLE(x);
  DEREFHANDLE(res)[1] = UNHANDLE(y);
  return res;
}

static Handle TUPLE_MAKE3 (Handle x,Handle y,Handle z)
{
  Handle res = SAVE(alloc(3));
  DEREFHANDLE(res)[0] = UNHANDLE(x);
  DEREFHANDLE(res)[1] = UNHANDLE(y);
  DEREFHANDLE(res)[2] = UNHANDLE(z);
  return res;
}

/* For testing...
static Handle print_ctype_and_vol (Handle pair)
{

  word* ctype = UNHANDLE(TUPLE_GET1(pair));
  Handle vol  = TUPLE_GET2(pair);
  void* thing = DEREFVOL(UNHANDLE(vol));

  switch ((Ctype)UNTAGGED(ctype)) {
  case Cchar    : proper_printf("Cchar <%c>\n",         *(char*)thing);     break;
  case Cdouble  : proper_printf("Cdouble <%f>\n",       *(double*)thing);   break;
  case Cfloat   : proper_printf("Cfloat <%f>\n",        *(float*)thing);    break;
  case Cint     : proper_printf("Cint <%d>\n",          *(int*)thing);      break;
  case Clong    : proper_printf("Clong <%ld>\n",        *(long*)thing);     break;
  case Cpointer : proper_printf("Cpointer <%.3s>...\n", *(char**)thing);    break;
  case Cshort   : proper_printf("Cshort <%d>\n",        *(short*)thing);    break;
  default       : {
    show(("Must be a Cstruct <%d>\n", get_C_long(ctype)));
  }}
  return pair;
}
...*/


static Handle union2vol_and_ctype (Handle u)
{
  Handle contents = CONTENTS(u);
  switch (TAG(u)) {
  case union_Char:      return TUPLE_MAKE2(SAVE(TAGGED(Cchar)),    toCchar(contents));
  case union_Double:    return TUPLE_MAKE2(SAVE(TAGGED(Cdouble)),  toCdouble(contents));
  case union_Float:     return TUPLE_MAKE2(SAVE(TAGGED(Cfloat)),   toCfloat(contents));
  case union_Int:       return TUPLE_MAKE2(SAVE(TAGGED(Cint)),     toCint(contents));
  case union_Long:      return TUPLE_MAKE2(SAVE(TAGGED(Clong)),    toClong(contents));
  case union_Short:     return TUPLE_MAKE2(SAVE(TAGGED(Cshort)),   toCshort(contents));
  case union_String:    return TUPLE_MAKE2(SAVE(TAGGED(Cpointer)), toCstring(contents));
  case union_Uint:      return TUPLE_MAKE2(SAVE(TAGGED(Cuint)),    toCuint(contents));
  case union_Vol:   	return contents;
    
  default:  	    	RAISE_EXN ("Unknown union tag\n");
  }
  /*NOTREACHED*/
}


static Handle choice2ctype (Handle choice)
{
  word *either_tag_or_pointer = UNHANDLE(choice);
  switch ((word)either_tag_or_pointer) {
  case choice_chooseChar:       return SAVE(TAGGED(Cchar));
  case choice_chooseDouble:     return SAVE(TAGGED(Cdouble));
  case choice_chooseFloat:      return SAVE(TAGGED(Cfloat));
  case choice_chooseInt:        return SAVE(TAGGED(Cint));
  case choice_chooseLong:       return SAVE(TAGGED(Clong));
  case choice_chooseShort:      return SAVE(TAGGED(Cshort));
  case choice_chooseString:     return SAVE(TAGGED(Cpointer));
  case choice_chooseUint:       return SAVE(TAGGED(Cuint));
  default:  	    	    	return SAVE(*either_tag_or_pointer);
  }
}

  
static  Handle choice_and_vol2union (Handle pair)
{
  Handle choice = TUPLE_GET1(pair);
  Handle vol = TUPLE_GET2(pair);
  word *maybe_tag = UNHANDLE(choice);
  switch ((word)maybe_tag) {
  case choice_chooseChar:       return UNION_MAKE(union_Char,   fromCchar(vol));
  case choice_chooseDouble:     return UNION_MAKE(union_Double, fromCdouble(vol));
  case choice_chooseFloat:      return UNION_MAKE(union_Float,  fromCfloat(vol));
  case choice_chooseInt:        return UNION_MAKE(union_Int,    fromCint(vol));
  case choice_chooseLong:       return UNION_MAKE(union_Long,   fromClong(vol));
  case choice_chooseShort:      return UNION_MAKE(union_Short,  fromCshort(vol));
  case choice_chooseString:     return UNION_MAKE(union_String, fromCstring(vol));
  case choice_chooseUint:       return UNION_MAKE(union_Uint,   fromCuint(vol));
  default:  	    	    	return UNION_MAKE(union_Vol,	vol);
  }
}

  
/*
DCJM 7/4/04.  Based on reading the code, it seems as though the arguments to a function
can be either "in" or "out" parameters.  "in" parameters are passed as expected from ML
to C whereas "out" parameters have to be constructed from the type information supplied
and their values returned along with the result.
*/
static Handle mkArgs (Handle xs)
{ TRACE; {
  if (LIST_ISNULL(xs)) {
    return TUPLE_MAKE2 (LIST_NULL, LIST_NULL);
  }
  else {
    Handle x    = LIST_HEAD(xs);
    Handle rest = mkArgs(LIST_TAIL(xs));
    Handle args = TUPLE_GET1(rest);
    Handle rets = TUPLE_GET2(rest);
    
    switch (TAG(x)) {
    case directedArg_In: {
      Handle vol_and_ctype = union2vol_and_ctype(CONTENTS(x));
      return TUPLE_MAKE2(LIST_CONS(vol_and_ctype,args),rets);
    }
    case directedArg_Out: {
      Handle choice = CONTENTS(x);
      Handle ctype = choice2ctype(choice);
      Handle space = allocate(c_sizeof(ctype));
      Handle arg = TUPLE_MAKE2(SAVE(TAGGED(Cpointer)), address(space));
      Handle ret = TUPLE_MAKE2(choice,space);
      
      return TUPLE_MAKE2(LIST_CONS(arg,args),
			 LIST_CONS(ret,rets));
    }
      
    default: RAISE_EXN ("Unknown directedArg tag\n");
    }
  }
  /*NOTREACHED*/
}}

typedef Handle (*Handle2Handle)(Handle);

static Handle map (Handle2Handle f, Handle xs)
{
  return
    LIST_ISNULL(xs) ? LIST_NULL
      	    	    : LIST_CONS( f(LIST_HEAD(xs)), map(f,LIST_TAIL(xs)) );
}
/*
DCJM 7/4/04.  For reading the code it seems as though call_sym_and_convert takes three
arguments: sym, unionArgs and retChoice and returns a pair (x, y) as the result.
sym is simply the symbol to call.
unionArgs is a list of arguments to the function.  An argument can either be an "in" parameter
consisting of type info and a value or an "out" parameter in which case only the type info
is supplied.
retChoice is the type of the result.
The result of this function is a pair consisting of the value and a list of the "out" parameters.
*/
static Handle call_sym_and_convert (Handle triple)
{TRACE; {
  Handle sym       = TUPLE_GET1(triple);
  Handle unionArgs = TUPLE_GET2(triple);
  Handle retChoice = TUPLE_GET3(triple);

  Handle listpair  = mkArgs(unionArgs);
  Handle args      = TUPLE_GET1(listpair);
  Handle rets      = TUPLE_GET2(listpair);
  Handle retCtype  = choice2ctype(retChoice);

  /*map(print_ctype_and_vol,args);*/
  {
  Handle vol = call_sym(TUPLE_MAKE3(sym,args,retCtype));

  return TUPLE_MAKE2 (choice_and_vol2union(TUPLE_MAKE2(retChoice,vol)),
		      map(choice_and_vol2union,rets));
}
}}

/* Forward references to machine-dependent code. */
unsigned char *MD_Build_Callback(int cbEntryNo, Handle cResultType, int nArgsToRemove);
void MD_GetCallbackArg(void **args, void *argLoc, int nSize);

/*
DCJM 7/4/04.  This function creates a vol containing a pointer to a function that can
be used as a callback.  
*/
static Handle createCallbackFunction(Handle triple, int nArgSpace)
{TRACE; {
	int i;
	Handle cArgTypeList = TUPLE_GET1(triple);
	Handle cResultType = TUPLE_GET2(triple);
	Handle mlFunction = TUPLE_GET3(triple);
	Handle res;
	/* See if we have a free entry. */
	for (i = 0; i < sizeof(callbackTable)/sizeof(callbackTable[0]); i++) {
		if (callbackTable[i].mlFunction == NULL) break;
	}
	if (i >= sizeof(callbackTable)/sizeof(callbackTable[0])) {
		RAISE_EXN("Too many callback functions");
	}
	callbackTable[i].argType = UNHANDLE(cArgTypeList);
	callbackTable[i].mlFunction = UNHANDLE(mlFunction);
	callbackTable[i].cFunction = MD_Build_Callback(i, cResultType, nArgSpace);
	/* Construct a "vol" containing the pointer to the C function. */
	res = vol_alloc_with_c_space(sizeof(void*));
	*(unsigned char **)C_POINTER(UNHANDLE(res)) = callbackTable[i].cFunction;
	return res;
}}

/* Create a callback using C calling conventions.  The calling function removes the
   arguments from the stack. */
static Handle toCfunction (Handle triple)
{
	return createCallbackFunction(triple, 0);
}

/* When calling using the Pascal calling convention the called code has to pop the arguments. */
static int computeArgSpace(Handle argTypeList)
{
	ML_Cons_Cell *argPtr = DEREFLISTHANDLE(argTypeList);
	int sum = 0;
	while (! IsNil(argPtr)) {
		word *argType		= Head(argPtr);
		if (! IS_INT(argType)) {
			/* It's a structure.  Don't allow it at least at the moment. */
			RAISE_EXN ("Structure arguments to callbacks are not supported\n");
		}
		else {
			Ctype ctype = UNTAGGED(argType);
			int nSize = 0;
			/* char and short args are always converted to ints. */
			switch (ctype) {
			case Cchar: case Cshort: case Cint: nSize = sizeof(int); break;
			case Cdouble: nSize = sizeof(double); break;
			case Cfloat: nSize = sizeof(float); break;
			case Clong: nSize = sizeof(long); break;
			case Cpointer: nSize = sizeof(void*); break;
			case Cuint: nSize = sizeof(unsigned); break;
			case Cstruct: break; /* To avoid a warning */
			}
			sum += nSize;
			argPtr = Tail(argPtr);
		}
	}
	return sum;
}

/* Create a callback using Pascal/WINAPI/CALLBACK/__stdcall calling conventions.
   The CALLED function must remove the arguments from the stack before returning. */
static Handle toPascalfunction (Handle triple)
{TRACE; {
	Handle cArgTypeList = TUPLE_GET1(triple);
	return createCallbackFunction(triple, computeArgSpace(cArgTypeList));
}}

static JMP_BUF returnBuf;

/* This is called when a callback function returns.  We wrap the user's callback function
   in a call here to deliver the result.  This does a long jump to restore the C stack to the
   point where it is able to return to the foreign function which made the callback.
   DCJM 9/4/04. */
static Handle deliverResult(Handle res)
{
	LONGJMP(returnBuf, (int)res);
}

/* Create the ML argument list from the C arguments. */
static Handle buildArgList(Handle argTypeList, void ** argPtr)
{
	TRACE; {
		if (LIST_ISNULL(argTypeList)) return argTypeList; /* Handle pointing to NULL. */
		else {
			Handle argType		= LIST_HEAD(argTypeList);
			Handle argValue;
			if (! IS_INT(UNHANDLE(argType))) {
				/* It's a structure.  This is a mess.  GCC apparently passes the address of the
				   structure here whereas MS C passes the structure by value. */
				/* I don't think an exception will work here. */
				RAISE_EXN ("Structure arguments to callbacks are not supported\n");
			}
			else {
				Ctype ctype = UNTAGGED(UNHANDLE(argType));
				int nSize = 0;
				trace(("<%s>\n", stringOfCtype(ctype)));
				/* Extract the appropriately typed argument and create a vol with the value.
				   This means that the process of extracting the arguments actually involves two
				   steps: this step where we create the vol list and a second step which uses the
				   fromXX to extract the actual ML arguments. */
				switch (ctype) {
				case Cchar: nSize = sizeof(char); break;
				case Cdouble: nSize = sizeof(double); break;
				case Cfloat: nSize = sizeof(float); break;
				case Cint: nSize = sizeof(int); break;
				case Clong: nSize = sizeof(long); break;
				case Cpointer: nSize = sizeof(void*); break;
				case Cshort: nSize = sizeof(short); break;
				case Cuint: nSize = sizeof(unsigned); break;
				case Cstruct: break; /* To avoid a warning */
				}
				argValue = vol_alloc_with_c_space(nSize);
				MD_GetCallbackArg(argPtr, DEREFVOL(UNHANDLE(argValue)), nSize);
			}
			return LIST_CONS(argValue, buildArgList(LIST_TAIL(argTypeList), argPtr));
		}
	}
}

/*
We deal with callbacks by forking a new process (thread) and suspending the old one
until this one returns.  That's rather inefficient since it involves allocating a new
process base and stack.  The main advantage is that it simplifies the way we call between
ML and C.  Whenever we call from ML to C we save the registers in the base of the stack
and when we return from C to ML we restore them from there.  The garbage collector treats
the base of the stack specially and in particular the p_pc field is always regarded as a
code pointer (the other reason we can enter the C code is because of a trap to handle a
garbage collection call or arbitrary precision integer overflow and in this case p_pc may
not be word + 2-byte aligned).
It might be possible to change all this and have the interface to the RTS save the registers
at the top of the stack (we would have to ensure we always have space so it doesn't overflow).
Calling into the RTS from ML is used frequently in all programs so the real question is
whether it is worth modifying the call sequence used when calling into the RTS simply in
order to speed up callbacks from C to ML.
DCJM 9/4/04.
*/

static void *callback(int cbNo, void **args)
{
	JMP_BUF oldBuf1, oldBuf2;
	Handle resultHandle, originalProcess;
	Handle h;
	Handle mlArgs;
	ProcessHandle newProcess;
	process_base *oldProcess;
	assert(cbNo >= 0 && cbNo < sizeof(callbackTable)/sizeof(callbackTable[0]));
	if (callbackTable[cbNo].mlFunction == NULL) {
		/* The entry has never been set or more likely it's been GCed away. */
		crash("Attempt to a callback to an ML function that no longer exists.");
	}
	h = SAVE(callbackTable[cbNo].mlFunction);

	memcpy(&oldBuf1, &returnBuf, sizeof(JMP_BUF));
	memcpy(&oldBuf2, &re_enter_poly, sizeof(JMP_BUF));
	/* Set up the ML arguments from the C arguments. */
	/* We use the argType list to process the function arguments and build up
	   a list of the actual arguments. */
	mlArgs = buildArgList(SAVE(callbackTable[cbNo].argType), args);
	/* Forking a process to run the callback is expensive since it
	   will involve allocating a new process base and stack.  We might
	   be able to use the caller's stack by arranging for call_sym to
	   return a callback function and for the ML code to call the
	   callback. */
	newProcess = fork_function(h, mlArgs);
	/* oldProcess is the process which called the foreign function */
	oldProcess = DEREFPROCHANDLE(processes);
	runtime_assign_word((word*)&(DEREFPROCHANDLE(newProcess)->callbackCaller), (word)oldProcess);
	/*MD_set_for_retry(ioCall);*/
	remove_process(oldProcess);
	/* First time - enter the Poly code. */
	select_next_process();

	/* Set up a jump to receive the result.  The result is zero when we
	   set up the jump.  Non-zero result means deliverResult has been
	   call and we've returned from the callback function */
	resultHandle = (Handle)SETJMP(returnBuf);
	if (resultHandle == 0) /* Calling the function */
	{
		/* We need to use enter_poly_code here to set up a new longjmp
		   for RTS calls. */
		enter_poly_code();
		/*NOTREACHED*/
	}

	/* We've returned from the callback function.  Restore the state. */
	memcpy(&returnBuf, &oldBuf1, sizeof(JMP_BUF));
	memcpy(&re_enter_poly, &oldBuf2, sizeof(JMP_BUF));
	/* Restore the original process. */
	originalProcess = SAVE(DEREFPROCHANDLE(processes)->callbackCaller);
	add_process(originalProcess, PROCESS_RUNABLE);
	/* Kill the one which returned the result. */
	kill_process(DEREFPROCHANDLE(processes));
	/* We must make sure that "processes" points to the process we suspended.
	   That process is the one that will receive the result when the C
	   function eventually returns. */
	select_next_process();
	DEREFPROCHANDLE(processes) = DEREFPROCHANDLE(originalProcess);

	/* Return the address of the vol.  The stub function then has to extract the
	   appropriate result depending on how it was compiled. */
	return DEREFVOL(UNHANDLE(resultHandle));
}

/**********************************************************************
 *
 *  Foreign Dispatch
 *   
 **********************************************************************/

typedef Handle(* type_hh_fun)(Handle);

static type_hh_fun handlers[] =
{
  get_foreign_debug,
  set_foreign_debug,

  load_lib,
  load_sym,
  call_sym,
  call_sym_and_convert,

  allocate,
  address,
  deref,
  offset,
  assign,
  c_sizeof,
  alignment,
  
  toCchar,
  fromCchar,

  toCdouble,
  fromCdouble,

  toCfloat,
  fromCfloat,

  toCint,
  fromCint,

  toClong,
  fromClong,

  toCshort,
  fromCshort,

  fillCstring,
  toCstring,
  fromCstring,

  toCuint,		/* Added DCJM 17/5/01. */
  fromCuint,		/* Added DCJM 17/5/01. */

  toCbytes,		/* Added DCJM 29/6/01. */
  fromCbytes,

  toCfunction,		/* Added DCJM 7/4/04. */
  toPascalfunction,	/* Added DCJM 7/4/04. */
  deliverResult		/* Added DCJM 7/4/04. */
};
    
#define NUM_HANDLERS (sizeof(handlers)/sizeof(type_hh_fun))


Handle foreign_dispatch_c (Handle args, Handle fcode_h)
{
  int fcode = get_C_long((word*)UNHANDLE(fcode_h));

  if (fcode < 0 || fcode >= NUM_HANDLERS) {
    char buf[100];
    sprintf(buf, "Unknown foreign dispatch code <%d>", fcode);
    RAISE_EXN(buf);
  }

  mes(("dispatch code = %d\n", fcode));
  /* dispatch to desired function */
  return (handlers[fcode])(args);
}

/*
We handle callbacks (calls from C into ML) by creating a new ML process (thread)
to run it.  We assume that we will only ever get a callback as a side-effect of
calling a C function (i.e. that a C function does not set up a signal handler
and then generate an ML function callback asynchronously).
The effect of generating a new ML thread is that the thread that originally
called into C has to be suspended and subsequently "retry" in order to be put into
the state where it can collect the result of the C function.   
*/
Handle foreign_result_c(Handle args, Handle fcode_h)
{
	int fcode = get_C_long((word*)UNHANDLE(fcode_h));
	switch (fcode)
	{
	case 4: /* call_sym. */
		{
			return SAVE(DEREFPROCHANDLE(processes)->lastCallResult);
		}

	case 5: /* call_sym_and_convert */
		{
			/* We have to set up the arguments again. */
			Handle unionArgs = TUPLE_GET2(args);
			Handle retChoice = TUPLE_GET3(args);

			Handle listpair  = mkArgs(unionArgs);
			Handle rets      = TUPLE_GET2(listpair);

			Handle vol = SAVE(DEREFPROCHANDLE(processes)->lastCallResult);

			return TUPLE_MAKE2 (choice_and_vol2union(TUPLE_MAKE2(retChoice,vol)),
					  map(choice_and_vol2union,rets));
		}

	default:
		{
			char buf[100];
			sprintf(buf, "Unknown foreign dispatch code <%d>", fcode);
			RAISE_EXN(buf);
		}
	}
	/*NOTREACHED*/
}

#if defined(i386)
/* We have to compile the callback function dynamically.  This code mallocs the store for it.
   At least at the moment, the store is never freed.  If we decide to garbage collect it
   we could store it in a vol. */
unsigned char *MD_Build_Callback(int cbEntryNo, Handle cResultType, int nArgsToRemove)
{
	int max_callback_size = 36; /* Sufficient for the largest callback (actually 33 I think).*/
	unsigned char *result = (unsigned char*)malloc(max_callback_size);
	/* TODO: This does not allocate memory with execute permissions and that could cause problems
	   in newer operating systems such as Windows XP SP2. DCJM 19/8/04. */
	unsigned char *p = result;
	int cbAddr = (int)&callback;
	/* This code creates a variable on the stack which is initialised to point to the first arg,
	   then calls "callback" with the address of this variable and the callback reference number.
	   When "callback" returns with the ADDRESS of the result this code extracts the result in
	   the appropriate form and returns with it. */
	*p++ = 0xC8;	/* enter 4, 0 */ /* Need one word on the stack. */
	*p++ = 0x48;
	*p++ = 0x00;
	*p++ = 0x00;
	*p++ = 0x8D;	/* lea eax,[ebp+8] */ /* Address of first arg. */
	*p++ = 0x45;
	*p++ = 0x08;
	*p++ = 0x89;	/* mov dword ptr [ebp-4],eax */ /* Store it in the variable. */
	*p++ = 0x45;
	*p++ = 0xFC;
	*p++ = 0x8D;	/* lea ecx,[ebp-4] */ /* Get the address of the variable. */
	*p++ = 0x4D;
	*p++ = 0xFC;
	*p++ = 0x51;	/* push ecx */
	*p++ = 0x68;	/* push cbEntryNo */
	*p++ = cbEntryNo & 0xff;
	cbEntryNo = cbEntryNo >> 8;
	*p++ = cbEntryNo & 0xff;
	cbEntryNo = cbEntryNo >> 8;
	*p++ = cbEntryNo & 0xff;
	cbEntryNo = cbEntryNo >> 8;
	*p++ = cbEntryNo & 0xff;
	/* The call is PC relative so we have to subtract the address of the END of the call instruction. */
	cbAddr -= (int)p + 5; /* The instruction is 5 bytes long. */
	*p++ = 0xE8;	/* call cbAddr */
	*p++ = cbAddr & 0xff;
	cbAddr = cbAddr >> 8;
	*p++ = cbAddr & 0xff;
	cbAddr = cbAddr >> 8;
	*p++ = cbAddr & 0xff;
	cbAddr = cbAddr >> 8;
	*p++ = cbAddr & 0xff;
	*p++ = 0x83;	/* add esp,8 */ /* Probably not needed since we're about to leave. */
	*p++ = 0xC4;
	*p++ = 0x08;
	/* Put in the return sequence.  eax points to the C_pointer for the result. */
	if (! IS_INT(UNHANDLE(cResultType))) {
		/* We might be able to get this to work but it's probably too much effort. */
		RAISE_EXN ("Structure results from callbacks are not supported\n");
	}
	else {
		switch (UNTAGGED(UNHANDLE(cResultType))) {
		case Cchar: /* movsbl eax, [eax] */
			*p++ = 0x0f;
			*p++ = 0xbe;
			*p++ = 0x00;
			break;
		case Cshort: /* movswl eax, [eax] */
			*p++ = 0x0f;
			*p++ = 0xbf;
			*p++ = 0x00;
			break;
		case Cfloat: /* flds [eax] */
			*p++ = 0xD9;
			*p++ = 0x00;
			break;
		case Cdouble: /* fldl [eax] */
			*p++ = 0xDD;
			*p++ = 0x00;
			break;
		case Cint: case Cuint: case Clong: case Cpointer:
			*p++ = 0x8B;	/* mov eax,dword ptr [eax] */
			*p++ = 0x00;
			break;
		default: crash("Unknown C type"); /* This shouldn't happen */
		}
	}
	*p++ = 0xC9;	/* leave */
	if (nArgsToRemove == 0) *p++ = 0xC3; /* Usual case for C functions */
	else { /* Pascal calling convention - remove arguments. */
		*p++ = 0xC2;    /* ret n */
		*p++ = nArgsToRemove & 0xff;
		nArgsToRemove = nArgsToRemove >> 8;
		*p++ = nArgsToRemove & 0xff;
	}
	assert(p - result <= max_callback_size);
	return result;
}

/* This function retrieves the callback arguments.  How the arguments to the
   function are stored is likely to be machine-dependent since some arguments are
   likely to be passed in registers and others on the stack. On the i386 it's
   easy - all arguments are on the stack. */
void MD_GetCallbackArg(void **args, void *argLoc, int nSize)
{
	/* nSize is the size of the result when we copy it into the vol.  char and short
	   arguments are always expanded to int.  This code will work for little-endians
	   but NOT for big-endian machines. */
	memcpy(argLoc, *args, nSize);
	/* Go on to the next argument. */
	nSize += sizeof(int)-1;
	nSize &= -(int)sizeof(int);
	*args = (void*) (*(char**)args + nSize);
}

#else
/* Not currently supported on other architectures. */
unsigned char *MD_Build_Callback(int cbEntryNo, Handle cResultType, int nArgsToRemove)
{
}

void MD_GetCallbackArg(void **args, void *argLoc, int nSize)
{
	RAISE_EXN("Callback functions are currently only implemented for the i386");
}

#endif

