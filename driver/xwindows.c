/*
    Title:      X-Windows/Motif Interface.

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

/

/* xwindows.c */

/* Removed indirection from get_C_* functions SPF 31/10/93 */
/* Added Handle type 2/11/93 */
/* Fixed "GetString can only be used once" bug 17/11/93 */

/* Dealing with gcc warning messages SPF 6/1/94 */
/* Retrofit to old Sun cc SPF 7/1/94 */
/* 25/1/94 SPF Fixed bug in EmptyVisual (core-dump when v==NULL) */

/* Comment added 4/11/93 SPF

Global Invariants:

(1) Get functions promise not to allocate on the Poly/ML heap

(2) The Poly/ML heap contains pointers into the C heap!
    As these are only valid for one session, the run-time
    system records which Poly/ML objects have been created
    in the current session. Only these objects contain
    valid C pointers, and so may be dereferenced.
    
    The "bad" Poly/ML objects are:

Flags      Object                      Bad Field                    Access Function
-----      ------                      ---------                    ---------------
M      X_GC_Object             GC               *gc                 GetGC                       
       X_Font_Object           Font             *font               GetFont
       ditto                   XFontStruct     **fs                 GetFS
       X_Cursor_Object         Cursor           *cursor             GetCursor
BM     X_Window_Object         Drawable         *drawable           GetDrawable, GetPixmap
       X_Pixmap_Object         Pixmap           *pixmap             GetDrawable, GetPixmap
       X_Colormap_Object       Colormap         *cmap               GetColormap
       X_Visual_Object         Visual          **visual             GetVisual  (* FISHY *)
B      X_Display_Object        Display          *display            (?) GetDisplay (?)
       ditto                   XtAppContext      app_context        NONE(?)
M      X_Widget_Object         Widget           *widget             GetWidget, GetNWidget
B      X_Trans_Object          XtTranslations    table              GetTrans
B      X_Acc_Object            XtAccelerators    acc                GetAcc
       
   WARNING: the above list of unsafe fields was created by SPF
            and may be incomplete.

   The function CheckExists should be called on these objects
   before it is safe to use any of the above fields. That's
   because the object may have been created in a previous ML
   session, so the pointers that it contains may no longer be
   valid. Using the appropriate access function listed above
   guarantees that CheckExists is called.
   
   Exception: the fields can safely be tested against C's zero
   (None, Null) even if CheckExists hasn't been called. Note that
   this is only database-safe because this value is used for
   uninitialised fields, so it doesn't confuse the garbage-collector.
   
   For all the above fields EXCEPT display, app_context, table, acc
   the run-time system creates an indirection object in the Poly heap.
   These fields don't need an indirection object because the object
   which contains them is itself a BYTE object.
   
   This indirection is a byte-object. The indirection is necessary
   because the garbage collector would object to finding a C pointer
   in a standard ML labelled record. The alternative would be to store
   the C pointer as an ML integer, but then we would have to convert
   back to a C pointer befor we could dereference it.
   
   For similar reasons, eventMask is also stored as a boxed word.

  abstype Colormap = Colormap with end; (* X_Colormap_Object *)
  abstype Cursor   = Cursor   with end; (* X_Cursor_Object   *)
  abstype Drawable = Drawable with end; (* X_Window_Object, XPixmap_Object *)
  abstype Font     = Font     with end; (* X_Font_Object     *)
  abstype GC       = GC       with end; (* X_GC_Object       *)
  abstype Visual   = Visual   with end; (* X_Visual_Object   *)
  abstype Display  = Display  with end; (* X_Display_Object  *)

  abstype Widget         = Widget         of int with end;
  abstype XtAccelerators = XtAccelerators of int with end;
  abstype XtTranslations = XtTranslations of int with end;
*/



/* MLXPoint, MLXRectangle, MLXArc, MLPair, MLTriple added 31/10/93 SPF */

#if defined(__GNUC__)
#define __USE_FIXED_PROTOTYPES__
#endif

#if !defined(__GNUC__)
#if !defined(__attribute__)
#define __attribute__(attribute) /* attribute */
#endif
#endif


#include <sys/types.h>
#include <sys/time.h>

#include <time.h>

#include <signal.h>
#include <stdlib.h>
#include <fcntl.h>
#include <ctype.h>

#if defined(FREEBSD) || defined(MACOSX)
#include <stdlib.h>
#else
#include <alloca.h>
#endif

#include <assert.h>

/* what goes wrong? ... gid, fd, private15 inaccessible */
/* THIS NEEDS TO BE FIXED!!!! */
#define XLIB_ILLEGAL_ACCESS 1       /* We need access to some opaque structures */

/* use prototypes, but make sure we get Booleans, not ints */
#define NeedWidePrototypes 0

#include <X11/Xlib.h>

#include <X11/keysym.h> /* IsCursorKey, IsFunctionKey, et cetera */
#include <X11/Xproto.h> /* needed for protocol names such as X_CreateWindow */
#include <X11/Xatom.h>  /* XA_ATOM, et cetera */

#include <Xm/Xm.h>
#include <Xm/ArrowB.h>
#include <Xm/ArrowBG.h>
#include <Xm/BulletinB.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>
#include <Xm/Command.h>
#include <Xm/DialogS.h>
#include <Xm/DrawingA.h>
#include <Xm/DrawnB.h>
#include <Xm/FileSB.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/List.h>
#include <Xm/MainW.h>
#include <Xm/MenuShell.h>
#include <Xm/MessageB.h>
#include <Xm/PanedW.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#include <Xm/ScrollBar.h>
#include <Xm/ScrolledW.h>
#include <Xm/SelectioB.h>
#include <Xm/SeparatoG.h>
#include <Xm/Separator.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>

/* Motif 1.2 */
#include <Xm/VendorS.h>
/* for XmIsDesktopObject */
#include <Xm/DesktopP.h>
/* for XmIsExtObject */
#include <Xm/ExtObjectP.h>
/* for XmIsShellExt */
#include <Xm/ShellEP.h>
/* for XmIsVendorShellExt */
#include <Xm/VendorSEP.h>
#if(0)
/* for XmIsWorldObject */
/* This is not supported in FreeBSD or Solaris 8. */
#include <Xm/WorldP.h>
#endif


#if defined(SOLARIS2)
  /* This should be in the Solaris version of <sys/time.h> but isn't. */
  /* From solaris 2.6 onwards, it's defined as follows. */
  extern int gettimeofday(struct timeval *, void *);
#endif

/* The following are only forward so we can declare attributes */
static void RaiseXWindows(char *s) __attribute__((noreturn));


#define ButtonClickMask (Word((unsigned)1 << 31))

#define XMASK(m) ((m) &~ButtonClickMask)

#undef SIZEOF

#include "globals.h"
#include "sys.h"
#include "memory.h"

#define debug1(fmt,p1) { /*EMPTY*/ }
#undef  debug1
#define debug1(fmt,p1) {if (A.debug & DEBUG_X) proper_printf(fmt,p1);}
#define debug3(fmt,p1,p2,p3) {if (A.debug & DEBUG_X) proper_printf(fmt,p1,p2,p3);}

#define debugCreate(type,value)  debug1("%x " #type " created\n",(int)(value))
#define debugReclaim(type,value) debug1("%x " #type " reclaimed\n",(int)(value))
#define debugReclaimRef(type,value) debug1("%x " #type " reference reclaimed\n",(int)(value))
#define debugRefer(type,value) debug1("%x " #type " referenced\n",(int)(value))
#define debugCreateCallback(MLValue,CValue,CListCell)  debug3("%p Widget callback reference created (%p,%p)\n",CValue,CListCell,MLValue)
#define debugReclaimCallback(MLValue,CValue,CListCell) debug3("%p Widget callback reference removed (%p,%p)\n",CValue,CListCell,MLValue)

/* added 26/10/93 SPF */
#include "run_time.h"
#include "arb.h"
#include "mpoly.h"
#include "gc.h"
#include "xcall_numbers.h"
#include "diagnostics.h"

#include "proper_io.h"
#include "processes.h"


/* forward declarations */

static Atom WM_DELETE_WINDOW(Display *d); /* was int SPF 6/1/94 */

#define DEREFDISPLAYHANDLE(h) ((X_Display_Object *)DEREFHANDLE(h))
#define DEREFWINDOWHANDLE(h)  ((X_Window_Object *)DEREFHANDLE(h))
#define DEREFXOBJECTHANDLE(h) ((X_Object *)DEREFHANDLE(h))

#define SAVE(x) push_to_save_vec((word)(x))

#define Make_int(x) Make_arbitrary_precision(x)
#define Make_string(s) SAVE(C_string_to_Poly(s))
#define Make_bool(b) Make_unsigned((b) != 0)

#define SIZEOF(x) (sizeof(x)/sizeof(word))

#define min(a,b) (a < b ? a : b)
#define max(a,b) (a > b ? a : b)

#define MUTABLE(x) (OBJ_MUTABLE_BIT | (x))
#define BYTES(x)   (OBJ_BYTE_BIT    | (x))

#define ASSIGN(p,v) runtime_assign_word((word *)(&(p)),(word)(v))

#define ISNIL(p)  ((word *)(p) == nil_value)
#define NONNIL(p) ((word *)(p) != nil_value)
#define MLLIST(p) ((p) ? (p) : (ML_Cons_Cell *)nil_value)

/* The Solaris version of gettimeofday used to only need 1 argument,
   but now it takes 2, just like everything else. 
   SPF 17/2/1998
*/
#define GETTIMEOFDAY(arg) gettimeofday(arg,NULL)


/********************************************************************************/
/* Objects are created MUTABLE and are FINISHED when all their fields have been */
/* filled in (assuming they are immutable objects). This is so that we can      */
/* consider the possibility of storing immutable objects in read-only memory    */
/* segments (not currently implemented).    SPF 7/12/93                         */
/********************************************************************************/
static Handle FINISHED(Handle P)
{
  word *pt = DEREFWORDHANDLE(P);
  
  assert(isValidHandle(P));
  
  assert(OBJ_IS_MUTABLE_OBJECT(pt[-1]));
  
  pt[-1] &= ~OBJ_MUTABLE_BIT;
  
  return P;
}


static void RaiseXWindows(char *s)
{
  if (! A.garbage_collecting)
    {
      raise_exception_string(EXC_XWindows,s);
    }
  else
    {
     /* Crash added 7/7/94 SPF */
     Crash("Tried to raise exception (XWindows \"%s\") during garbage collection\n",s);
    }
  /*NOTREACHED*/
}


/* bugfixed 6/12/94 SPF */
#define RaiseXWindows2(varmessage,constmessage) \
{ \
   const char message[] = constmessage; \
   int n1 = strlen(varmessage); \
   int n2 = strlen(message); \
   char *mess = (char *)alloca(n1 + n2 + 1); \
   strcat(strncpy(mess,varmessage,n1),message); \
   RaiseXWindows(mess); \
   /*NOTREACHED*/ \
}

static void RaiseRange(void)
{
  raise_exception0(EXC_size);
}

typedef unsigned char uchar;

/* indirection removed SPF 31/10/93 */
static uchar get_C_uchar(word *a)
{
  unsigned u = get_C_ushort(a);
  
  if (u >= 256) RaiseRange();

  return u;
}


/******************************************************************************/
/*                                                                            */
/*      String                                                                */
/*                                                                            */
/******************************************************************************/

#define String STRING

#if 0
typedef struct
{
  word length;
  char chars[4];
} String;

/* fudge difference between (String *) and (pstring) */
#define Poly_String_To_C(x,y,z) Poly_string_to_C((pstring)(x),y,z)

#else
/* difference between (String *) and (pstring) removed */
#define Poly_String_To_C(x,y,z) Poly_string_to_C(x,y,z)
#endif


#define GetString(s) _GetString((word *)(s))
/* can only be called TABLESIZE times per X opcode */
static String *_GetString(word *s)
{
#define TABLESIZE 5
  static String string[TABLESIZE] = { {1,{0}}, {1,{0}}, {1,{0}}, {1,{0}}, {1,{0}} };
  static int index = 0;
  
  if (! OBJ_IS_AN_INTEGER(Word(s))) return (String *) s;

  index = (index + 1) % TABLESIZE;
  string[index].chars[0] = UNTAGGED(s);

  return &string[index];
#undef TABLESIZE
}


/******************************************************************************/
/*                                                                            */
/*      XObjects (Type definitions)                                           */
/*                                                                            */
/******************************************************************************/

/* We keep a list of all objects created by calls to X.         */
/* When an object is created we add an entry to the list and    */
/* return the entry. If the entry becomes inaccessible          */
/* by the garbage collector then we free the object.            */
/* The list is created by malloc so that it is not in the heap. */

typedef enum
{
  X_GC       = TAGGED(111),
  X_Font     = TAGGED(222),
  X_Cursor   = TAGGED(333),
  X_Window   = TAGGED(444),
  X_Pixmap   = TAGGED(555),
  X_Colormap = TAGGED(666),
  X_Visual   = TAGGED(777),
  X_Display  = TAGGED(888),
  X_Widget   = TAGGED(999),
  X_Trans    = TAGGED(1111),
  X_Acc      = TAGGED(2222)
} X_type;

typedef struct               /* BYTE object */
{
  X_type         type;
  XtTranslations table;      /* C value */
} X_Trans_Object;

typedef struct               /* BYTE object */
{
  X_type         type;
  XtAccelerators acc;        /* C value */
} X_Acc_Object;

typedef struct              /* BYTE object */
{
  X_type       type;
  Display     *display;     /* C value */
  unsigned     screen;      /* C value */
  XtAppContext app_context; /* C value */
} X_Display_Object;  

typedef struct
{
  X_type            type;
  Font             *font;  /* Token for C value */
  XFontStruct     **fs;    /* Token for C value */
  X_Display_Object *ds;    /* Token */
} X_Font_Object;

typedef struct
{
  X_type            type;
  Cursor           *cursor; /* Token for C value */
  X_Display_Object *ds;     /* Token */
} X_Cursor_Object;

typedef struct
{
  X_type            type;
  Pixmap           *pixmap;  /* Token for C value */
  X_Display_Object *ds;      /* Token */
} X_Pixmap_Object;

typedef struct
{
  X_type            type;
  Colormap         *cmap;  /* Token for C value */
  X_Display_Object *ds;    /* Token */
} X_Colormap_Object;

typedef struct                    /* MUTABLE */
{
  X_type            type;
  Widget           *widget;       /* Token for C value */
  ML_Cons_Cell     *callbackList; /* mutable */
  word             *state;        /* mutable */
  X_Display_Object *ds;           /* Token */
} X_Widget_Object;

typedef struct
{
  X_type            type;
  Visual          **visual;  /* Token for C value */
  X_Display_Object *ds;      /* Token */
} X_Visual_Object;

typedef struct                   /* MUTABLE */
{
  X_type            type;
  GC               *gc;          /* Token for C value */
  X_Font_Object    *font_object; /* mutable; may be 0 */
  X_Pixmap_Object  *tile;        /* mutable; may be 0 */
  X_Pixmap_Object  *stipple;     /* mutable; may be 0 */
  X_Pixmap_Object  *clipMask;    /* mutable; may be 0 */
  X_Display_Object *ds;          /* Token */
} X_GC_Object;

typedef struct X_Window_Struct X_Window_Object;

struct X_Window_Struct                 /* MUTABLE */
{
  X_type             type;
  Drawable          *drawable;         /* Token for C value */
  word              *handler;          /* mutable? */
  word              *state;            /* mutable? */
  word              *eventMask;        /* Token for C value; token itself is mutable */
  X_Colormap_Object *colormap_object;  /* mutable; may be 0 */
  X_Cursor_Object   *cursor_object;    /* mutable; may be 0 */
  X_Pixmap_Object   *backgroundPixmap; /* mutable; may be 0 */
  X_Pixmap_Object   *borderPixmap;     /* mutable; may be 0 */
  X_Window_Object   *parent;           /* may be 0 */
  X_Display_Object  *ds;               /* Token */
};

typedef union
{
  X_type            type;
  X_GC_Object       g;
  X_Font_Object     f;
  X_Cursor_Object   c;
  X_Window_Object   w;
  X_Pixmap_Object   p;
  X_Colormap_Object m;
  X_Visual_Object   v;
  X_Display_Object  d;
  X_Widget_Object   W;
  X_Trans_Object    t;
  X_Acc_Object      a;
} X_Object;  /* stored in Poly heap */



/******************************************************************************/
/*                                                                            */
/*      Forward declarations                                                  */
/*                                                                            */
/******************************************************************************/
static Font           GetFont(X_Object *P);
static Cursor         GetCursor(X_Object *P);
static Colormap       GetColormap(X_Object *P);
static Visual        *GetVisual(X_Object *P);
static XtTranslations GetTrans(X_Object *P);
static XtAccelerators GetAcc(X_Object *P);
static Pixmap         GetPixmap(X_Object *P);
static Widget         GetNWidget(X_Object *P);
static Window         GetWindow(X_Object *P);
static Display       *GetDisplay(X_Object *P);

static void DestroyWindow(X_Object *W);
static void DestroySubwindows(X_Object *W);

static X_GC_Object     *GCObject(X_Object *P);
static X_Pixmap_Object *PixmapObject(X_Object *P);
static X_Widget_Object *WidgetObject(X_Object *P);
static X_Window_Object *WindowObject(X_Object *P);

/******************************************************************************/
/*                                                                            */
/*      C lists (Type definitions)                                            */
/*                                                                            */
/******************************************************************************/

typedef struct X_List_struct X_List;

struct X_List_struct
{
  X_List   *next;           /* pointer into C heap */
  X_Object *object;         /* pointer into Poly heap; weak */
};

typedef struct timeval TimeVal;

/* In C heap */
typedef struct T_List_struct T_List;
struct T_List_struct
{
  T_List          *next;           /* pointer into C heap */
  TimeVal          timeout;       /* here */
  X_Window_Object *window_object; /* pointer into Poly heap, or 0; weak */
  X_Widget_Object *widget_object; /* pointer into Poly heap, or 0; strong */
  word            *alpha;         /* pointer into Poly heap; strong */
  word            *handler;       /* pointer into Poly heap; strong */
  int              expired;       /* here */
};
/* NB precisely one of window_object and widget_object should be non-zero */

/* In C heap */
typedef struct C_List_struct C_List;
struct C_List_struct
{
  word            *function; /* pointer into Poly heap; strong */
  X_Widget_Object *widget_object;   /* pointer into Poly heap; strong */
  C_List          *next;     /* pointer into C heap */
};

/* lists of X objects currently in Poly heap i.e. those created in this session */
#define XLISTSIZE 1001 /* must be coprime to 4 ('cos pointers are word-aligned) */
static X_List *XList[XLISTSIZE] = {0};

static T_List *TList       = 0; /* C pending messages list, ordered by arrival time */
static C_List *CList       = 0; /* Acts as root for objects "owned" by C callbacks */
static ML_Cons_Cell *FList = 0; /* ML Callback list - acts as a Root for the Heap */
static ML_Cons_Cell *GList = 0; /* ML Event list - acts as a Root for the Heap */

static Bool callbacks_enabled = False;



/******************************************************************************/
/*                                                                            */
/*      High-speed XList routines                                             */
/*                                                                            */
/******************************************************************************/

/* maps an (X_Object *) to an (unsigned); this mapping from must give the same   */
/* (unsigned) for each (X_Object) for an entire Poly/ML session, even though its */
/* address may change at every garbage collection.                               */
/* The way we achieve this is by returning the address of the corresponding C    */
/* object. Note that since the ML object doesn't necessarily correspond to a real*/
/* C object, this value may be neither valid nor sensible (but it WILL be a      */
/* constant).                                                                    */
/* Unfortunately, we can't do this for GCs or VISUALS, since the actual C object */
/* contains the id we want, and we can't access the id if we haven't got the     */
/* object. For these, we return a constant instead.                              */
static int hashId(X_Object *P)
{

#define HASH_GC     0
#define HASH_VISUAL 1

  switch(P->type)
  {
    case X_GC:       return HASH_GC;
    case X_Font:     return (unsigned)(*(P->f.font));
    case X_Cursor:   return (unsigned)(*(P->c.cursor));
    case X_Window:   return (unsigned)(*(P->w.drawable));
    case X_Pixmap:   return (unsigned)(*(P->p.pixmap));
    case X_Colormap: return (unsigned)(*(P->m.cmap));
    case X_Visual:   return HASH_VISUAL;
    case X_Display:  return (unsigned)(P->d.display);
    case X_Widget:   return (unsigned)(*(P->W.widget));
    case X_Trans:    return (unsigned)(P->t.table);
    case X_Acc:      return (unsigned)(P->a.acc);
    default:         Crash ("Bad X_Object type (%d) in hashId",P->type);
  }
  /*NOTREACHED*/
}

static void initXList(void)
{
  int i;
  
  for (i = 0; i < XLISTSIZE; i++)
    {
      XList[i] = NULL;
    }
}

static X_List **hashXList(X_Object *P)
{
  unsigned id = hashId(P);
  unsigned n  = (id % XLISTSIZE); /* a poor hash function, but good enough for now */
  return &(XList[n]);
}

static X_List *findXList(unsigned id)
{
  unsigned n  = (id % XLISTSIZE); /* a poor hash function, but good enough for now */
  return XList[n];
}

/******************************************************************************/
/*                                                                            */
/*      C lists (Polymorphic functions)                                       */
/*                                                                            */
/******************************************************************************/
#define CreateList4(n,p,bytes,f)    CreateList4_private(n,(char *)(p),bytes,f)
#define CreateList5(n,p,bytes,f,a1) CreateList5_private(n,(char *)(p),bytes,f,a1)

static Handle CreateList4_private(int n, char *p, word bytes, Handle (*f)())
{
  Handle list  = SAVE(nil_value);
  Handle tail  = SAVE(nil_value); /* just to get a save_vec cell */
  
  Handle saved = mark_save_vec(); /* remember how far the save vector has been extended */

  word   i;
  
  for (i = 0; i < n; i++, p += bytes)
  {
    Handle value = (* f)(p,i);
    Handle next  = alloc_and_save(MUTABLE(SIZEOF(ML_Cons_Cell)));
    
    if (i == 0)
      { /* initialise list header */
        DEREFHANDLE(list) = DEREFHANDLE(next);
      }
    else 
      { /* finish previous cell in list */
        DEREFLISTHANDLE(tail)->t = DEREFLISTHANDLE(next);
        FINISHED(tail);
      }
    
    DEREFHANDLE(tail) = DEREFHANDLE(next);
    
    DEREFLISTHANDLE(tail)->h = DEREFWORDHANDLE(value); 

    /* reset save vector to stop it overflowing */    
    reset_save_vec(saved); /* next, value now contain junk */
  }
  
  /* finish last cell in list */
  if (n != 0)
    {
      DEREFLISTHANDLE(tail)->t = (ML_Cons_Cell *)nil_value;
      FINISHED(tail);
    }
    
  return list;
}

static Handle CreateList5_private(int n, char *p, word bytes, Handle (*f)(), Handle a1)
{
  Handle list  = SAVE(nil_value);
  Handle tail  = SAVE(nil_value); /* just to get a save_vec cell */
  
  Handle saved = mark_save_vec(); /* remember how far the save vector has been extended */

  word   i;
  
  for (i = 0; i < n; i++, p += bytes)
  {
    Handle value = (* f)(p,i,a1);
    Handle next  = alloc_and_save(MUTABLE(SIZEOF(ML_Cons_Cell)));
    
    if (i == 0)
      { /* initialise list header */
        DEREFHANDLE(list) = DEREFHANDLE(next);
      }
    else 
      { /* finish previous cell in list */
        DEREFLISTHANDLE(tail)->t = DEREFLISTHANDLE(next);
        FINISHED(tail);
      }
    
    DEREFHANDLE(tail) = DEREFHANDLE(next);
    
    DEREFLISTHANDLE(tail)->h = DEREFWORDHANDLE(value); 

    /* reset save vector to stop it overflowing */    
    reset_save_vec(saved); /* next, value now contain junk */
  }
  
  /* finish last cell in list */
  if (n != 0)
    {
      DEREFLISTHANDLE(tail)->t = (ML_Cons_Cell *)nil_value;
      FINISHED(tail);
    }
    
  return list;
}

#define GetList4(list,s,bytes,f)          GetList4_private((ML_Cons_Cell *)list,(char *)(s),bytes,f)

static void GetList4_private(ML_Cons_Cell *list, char *s,word bytes,VoidFunc get)
{
  ML_Cons_Cell *p;
  int i = 0;
  
  for(p = list; NONNIL(p); p = p->t)
  {
    (* get)(p->h,s,i);
    s += bytes;
    i++;
  }
}

/* ListLength no longer requires indirection via handle SPF 4/11/93 */
static int ListLength(ML_Cons_Cell *list)
{
  ML_Cons_Cell *p;
  word  n = 0;

  for(p = list; NONNIL(p); p = p->t) n++;

  return n;
}

/******************************************************************************/
/*                                                                            */
/*      TList Purge Functions (SPF 29/11/93)                                  */
/*                                                                            */
/******************************************************************************/
static void PurgePendingWidgetMessages(X_Widget_Object *P)
{
  T_List **T = &TList;
  
  while(*T)
  {
    T_List *t = *T;
    
    if (t->widget_object == P)  /* clear out pending messages for this widget */
    {
      *T = t->next;
      free(t);
    }
    else T = &t->next;
  }
}  

static void PurgePendingWindowMessages(X_Window_Object *P)
{
  T_List **T = &TList;
  
  while(*T)
  {
    T_List *t = *T;
    
    if (t->window_object == P)  /* clear out pending messages for this window */
    {
      *T = t->next;
      free(t);
    }
    else T = &t->next;
  }
}  

/******************************************************************************/
/*                                                                            */
/*      CList Purge Functions (SPF 29/2/96)                                   */
/*                                                                            */
/******************************************************************************/
static void PurgeCCallbacks(X_Widget_Object *P, Widget w)
{
  C_List **C = &CList;
  
  while(*C)
  {
    C_List *c = *C;
    
    if (c->widget_object == P)  /* clear out callback info for this widget */
    {
      debugReclaimCallback(P,w,c);
      *C = c->next;
      free(c);
    }
    else C = &c->next;
  }
}  

/******************************************************************************/
/*                                                                            */
/*      XObjects (Polymorphic functions 1)                                    */
/*                                                                            */
/******************************************************************************/
static int ResourceExists(X_Object *P)
{
  X_List *L;

  for(L = *hashXList(P); L; L = L->next)
    {
      if (L->object == P) return 1;
    }

  return 0;
}

/* SafeResourceExists is like ResourceExists but doesn't assume that
   we actually have a valid X object, so it doesn't use hashing.
   SPF 6/4/95 */ 
static int SafeResourceExists(X_Object *P)
{
  unsigned n;
  
  for (n = 0; n < XLISTSIZE; n++)
  {
    X_List *L;
  
    for(L = XList[n]; L; L = L->next)
      {
	if (L->object == P) return 1;
      }
  }
  return 0;
}

static void DestroyXObject(X_Object *P)
{
  X_List **X = hashXList(P);
  
  switch(P->type)
  {
    case X_GC:
    {
      X_GC_Object *G = GCObject(P);
      
      GC       gc = *G->gc;
      Display *d  =  G->ds->display;

      if (gc == DefaultGC(d,G->ds->screen))
        {
          debugReclaimRef(GC,gc->gid);
        }
      else
        {
          debugReclaim(GC,gc->gid);
          XFreeGC(d,gc); /* SAFE(?) */
        }
      break;
    }

    case X_Font:
    {
      Font f = GetFont(P);

      if (f == None)
        {
          debugReclaimRef(Font,f);
        }
      else
        {
          debugReclaim(Font,f);

#if NEVER
          XUnloadFont(GetDisplay(P),f);
#endif
        }
      break;
    }

    case X_Cursor:
    {
      Cursor cursor = GetCursor(P);

      if (cursor == None)
        {
          debugReclaimRef(Cursor,cursor);
        }
      else
        {
          debugReclaim(Cursor,cursor);

#if NEVER
          XFreeCursor(GetDisplay(P),cursor);
#endif
        }

      break;
    }

    case X_Window:
    {
      /* added 29/11/93 SPF */
      PurgePendingWindowMessages(WindowObject(P));

      if (((X_Window_Object *)P)->parent != 0) /* this clients window */
        {
          debugReclaim(Window,GetWindow(P));
          DestroyWindow(P);
        }
      else /* None, ParentRelative, and other clients windows */
        {
          debugReclaimRef(Window,GetWindow(P));
        }
      break;
    }

    case X_Pixmap:
    {
      Pixmap pixmap = GetPixmap(P);

      if (pixmap == None)
        {
          debugReclaimRef(Pixmap,pixmap);
        }
      else
       {
          debugReclaim(Pixmap,pixmap);

#if NEVER
        XFreePixmap(GetDisplay(P),pixmap);
#endif
       }

      break;
    }

    case X_Colormap:
    {
      Colormap cmap = GetColormap(P);

      if (cmap == None)
        {
          debugReclaimRef(Colormap,cmap);
        }
      else
        {
          debugReclaim(Colormap,cmap);
#if NEVER
          XFreeColormap(GetDisplay(P),cmap);
#endif
        }
      break;
    }

    case X_Visual:
    {
      Visual *visual = GetVisual(P);

      debugReclaimRef(Visual,visual->visualid);
      break;
    }

    case X_Widget:
    {
      Widget widget = GetNWidget(P);

      PurgePendingWidgetMessages(WidgetObject(P));
      debugReclaimRef(Widget,widget);
      break;
    }
    
    case X_Trans:
    {
      XtTranslations table = GetTrans(P);

      debugReclaimRef(Trans,table);
      break;
    }
    
    case X_Acc:
    {
      XtAccelerators acc = GetAcc((X_Object *)P);

      debugReclaimRef(Acc,acc);
      break;
    }
    
    default: Crash ("Unknown X_Object type %d",P->type);
  }

  while(*X)
  {
    X_List *L = *X;

    if (L->object == P)
    {
      *X = L->next;
      free(L);
      return;
    }
    else X = &L->next;
  }
  proper_printf("DestroyXObject: destroy failed\n");
}

#define CheckExists(P,resource) \
{\
  if (! ResourceExists(P)) RaiseXWindows ("Non-existent " #resource); \
}

static X_Font_Object *FontObject(X_Object *P)
{
  assert(P->type == X_Font);

  return (X_Font_Object *)P;
}

static X_Object *FindResource
(
  Handle   dsHandle,   /* Handle to (X_Display_Object *) */
  X_type   type,
  unsigned id,
  unsigned hashid
)
{
  X_List *L;
  X_Display_Object *d = (type == X_Widget) ? NULL : DEREFDISPLAYHANDLE(dsHandle);

  for(L = findXList(hashid); L; L = L->next)
  {
    X_Object *P = L->object;
    
    if (P->type == type)
    {
      switch(type)
      {
        case X_GC:       if (P->g.ds == d && (*P->g.gc)->gid          == id) return P; break;
        case X_Font:     if (P->f.ds == d && (*P->f.font)             == id) return P; break;
        case X_Cursor:   if (P->c.ds == d && (*P->c.cursor)           == id) return P; break;
        case X_Window:   if (P->w.ds == d && (*P->w.drawable)         == id) return P; break;
        case X_Pixmap:   if (P->p.ds == d && (*P->p.pixmap)           == id) return P; break;
        case X_Colormap: if (P->m.ds == d && (*P->m.cmap)             == id) return P; break;
        case X_Visual:   if (P->v.ds == d && (*P->v.visual)->visualid == id) return P; break;
        
        case X_Widget:   if (*(P->W.widget) == (Widget) id) return P; break;
        
        case X_Display:  break;
        case X_Trans:    break;
        case X_Acc:      break;

        default: Crash ("Bad X_Object type (%d) in FindResource",type);
      }
    }
  }

  return 0;
}


#define FindWindow(d,id)   ((X_Window_Object *)   FindResource(d,X_Window,(unsigned)id,(unsigned)id))
#define FindPixmap(d,id)   ((X_Pixmap_Object *)   FindResource(d,X_Pixmap,(unsigned)id,(unsigned)id))
#define FindCursor(d,id)   ((X_Cursor_Object *)   FindResource(d,X_Cursor,(unsigned)id,(unsigned)id))
#define FindFont(d,id)     ((X_Font_Object *)     FindResource(d,X_Font,(unsigned)id,(unsigned)id))
#define FindColormap(d,id) ((X_Colormap_Object *) FindResource(d,X_Colormap,(unsigned)id,(unsigned)id))
#define FindWidget(id)     ((X_Widget_Object *)   FindResource((Handle)NULL,X_Widget,(unsigned)id,(unsigned)id))

/* can't use id for hashing in the following, so use arbitrary values instead */
#define FindGC(d,id)       ((X_GC_Object *)       FindResource(d,X_GC,(unsigned)id,HASH_GC))
#define FindVisual(d,id)   ((X_Visual_Object *)   FindResource(d,X_Visual,(unsigned)id,HASH_VISUAL))

static Handle AddXObject(Handle objectHandle)
{
  X_List **X = hashXList(DEREFXOBJECTHANDLE(objectHandle));
  X_List  *L = (X_List *) malloc(sizeof(X_List));

  L->next   = *X;
  L->object = (X_Object *)DEREFHANDLE(objectHandle);

  *X = L;

  return objectHandle;
}

/******************************************************************************/
/*                                                                            */
/*      MLXPoint - implements ML XPoint datatype                              */
/*                                                                            */
/******************************************************************************/
typedef struct /* depends on XPoint datatype + ML compiler hash function */
{
word    *x; /* ML int */
word    *y; /* ML int */
} MLXPoint;

#define Point(p) ((MLXPoint *) p)

/* shouldn't these be long values? */
#define GetPointX(p) get_C_short(Point(p)->x)
#define GetPointY(p) get_C_short(Point(p)->y)

#define GetOffsetX(p) get_C_ushort(Point(p)->x)
#define GetOffsetY(p) get_C_ushort(Point(p)->y)

static Handle CreatePoint(int x, int y)
{
  Handle pointHandle = alloc_and_save(MUTABLE(SIZEOF(MLXPoint)));
  
/* Still allocating, so must use explicit DEREF for each element */
#define point ((MLXPoint *)DEREFHANDLE(pointHandle))
  point->x = DEREFWORDHANDLE(Make_int(x));
  point->y = DEREFWORDHANDLE(Make_int(y));
#undef point

  return FINISHED(pointHandle);
}

static void GetPoints(MLXPoint *p, XPoint *A)
{
  A->x = GetPointX(p);
  A->y = GetPointY(p);
}

/******************************************************************************/
/*                                                                            */
/*      MLXRectangle - implements ML XRectangle datatype                      */
/*                                                                            */
/******************************************************************************/

typedef struct /* depends on XRectangle datatype + ML compiler hash function */
{
word    *top;     /* ML int */
word    *left;    /* ML int */
word    *right;   /* ML int */
word    *bottom;  /* ML int */
} MLXRectangle;

#define Rect(R) ((MLXRectangle *) R)

#define GetRectTop(R)    get_C_short(Rect(R)->top)
#define GetRectLeft(R)   get_C_short(Rect(R)->left)
#define GetRectRight(R)  get_C_short(Rect(R)->right)
#define GetRectBottom(R) get_C_short(Rect(R)->bottom)

#define GetRectX(R)  GetRectLeft(R)
#define GetRectY(R)  GetRectTop(R)

/* functions added 29/10/93 SPF */
#define GetRectW(R) _GetRectW(Rect(R))
static unsigned _GetRectW(MLXRectangle *R)
{
  long result = GetRectRight(R) - GetRectLeft(R);

  if (result < 0) RaiseRange();
  return (unsigned)result;
}

#define GetRectH(R) _GetRectH(Rect(R))
static unsigned _GetRectH(MLXRectangle *R)
{
  long result = GetRectBottom(R) - GetRectTop(R);

  if (result < 0) RaiseRange();
  return (unsigned)result;
}

/* static MLXRectangle **CreateRect(top,left,bottom,right) */
static Handle CreateRect(int top, int left, int bottom, int right)
{
  Handle rectHandle = alloc_and_save(MUTABLE(SIZEOF(MLXRectangle)));

/* Still allocating, so must use explicit DEREF for each element */
#define rect ((MLXRectangle *)DEREFHANDLE(rectHandle))
  rect->top    = DEREFWORDHANDLE(Make_int(top));
  rect->left   = DEREFWORDHANDLE(Make_int(left));
  rect->right  = DEREFWORDHANDLE(Make_int(right));
  rect->bottom = DEREFWORDHANDLE(Make_int(bottom));
#undef rect

  return FINISHED(rectHandle);
}

#define CreateArea(w,h) CreateRect(0,0,(int)h,(int)w)

static void GetRects(MLXRectangle *p, XRectangle *A)
{
  A->x      = GetRectX(p);
  A->y      = GetRectY(p);
  A->width  = GetRectW(p);
  A->height = GetRectH(p);
}

#define CheckZeroRect(R) check_zero_rect(Rect(R))
static void check_zero_rect(MLXRectangle *R)
{
  unsigned x = GetRectX(R);
  unsigned y = GetRectY(R);
  unsigned w = GetRectW(R);
  unsigned h = GetRectH(R);

  if (x != 0 || y != 0 || 
/*     w <= 0 || h <= 0 ||   w,h now unsigned SPF 29/10/93 */
       w == 0 || h == 0 || 
       w > 65535 || h > 65535) RaiseRange(); 
}


/******************************************************************************/
/*                                                                            */
/*      MLXArc - implements ML XArc datatype                                  */
/*                                                                            */
/******************************************************************************/

/* MLXArc added 31/10/93 SPF; depends on ML XArc datatype */
typedef struct
{
MLXRectangle *r;
word         *a1;   /* ML int */
word         *a2;   /* ML int */
} MLXArc;

#define Arc(A) ((MLXArc *) A)

#define GetArcR(A)  Arc(A)->r
#define GetArcA1(A) get_C_short(Arc(A)->a1)
#define GetArcA2(A) get_C_short(Arc(A)->a2)

static void GetArcs(MLXArc *p, XArc *A)
{
  A->x      = GetRectX(GetArcR(p));
  A->y      = GetRectY(GetArcR(p));
  A->width  = GetRectW(GetArcR(p));
  A->height = GetRectH(GetArcR(p));
  A->angle1 = GetArcA1(p);
  A->angle2 = GetArcA2(p);
}


/******************************************************************************/
/*                                                                            */
/*      Colormap                                                              */
/*                                                                            */
/******************************************************************************/

static X_Colormap_Object *ColormapObject(X_Object *P)
{
  assert(P->type == X_Colormap);

  return (X_Colormap_Object *)P;
}

static Colormap GetColormap(X_Object *P)
{
  assert(P->type == X_Colormap);

  /* val NoColormap = xcall (23,0) : Colormap; */
  /* special case for NoColormap - correct(?) */
  if ( *(((X_Colormap_Object *)P)->cmap) == None) return None;

  CheckExists(P,colormap);

  return *(((X_Colormap_Object *)P)->cmap);
}

static Handle EmptyColormap
(
  Handle   dsHandle /* Handle to (X_Display_Object *) */,
  Colormap id
)
{
  X_Colormap_Object *E = FindColormap(dsHandle,id);

  if (E)
  {
    return SAVE(E);
  }
  else
  {
    Handle objectHandle = alloc_and_save(MUTABLE(SIZEOF(X_Colormap_Object)));
    Handle cmapHandle   = alloc_and_save(MUTABLE(BYTES(1)));
    
    /* Must do all allocations before we do the first dereference */
    X_Colormap_Object *object = (X_Colormap_Object *)DEREFHANDLE(objectHandle);
    Colormap          *cmap   = (Colormap *)DEREFHANDLE(cmapHandle);
    
    *cmap = id; FINISHED(cmapHandle);

    object->type = X_Colormap;
    object->cmap = cmap;
    object->ds   = DEREFDISPLAYHANDLE(dsHandle);

    debugRefer(Colormap,id);

    return AddXObject(FINISHED(objectHandle));
  }
}


/******************************************************************************/
/*                                                                            */
/*      Visual                                                                */
/*                                                                            */
/******************************************************************************/
static Visual *GetVisual(X_Object *P)
{
  static Visual EMPTYVISUAL = { 0 };

  assert(P->type == X_Visual);

  /* val NoVisual = xcall (24,0) : Visual; */
  /* special case for NoVisual */
  if (*(((X_Visual_Object *)P)->visual) == None) return &EMPTYVISUAL; /* FISHY (?) */

  CheckExists(P,visual);

  return *(((X_Visual_Object *)P)->visual);
}

static Handle EmptyVisual
(
  Handle  dsHandle, /* Handle to (X_Display_Object *) */
  Visual *v
)
{
  if (v != None)
    {
      X_Visual_Object *E = FindVisual(dsHandle,v->visualid);

      if (E) return SAVE(E);
    }
  
  /* else */
  {
    Handle objectHandle = alloc_and_save(MUTABLE(SIZEOF(X_Visual_Object)));
    Handle visualHandle = alloc_and_save(MUTABLE(BYTES(1)));
    
    /* Must do all allocations before we do the first dereference */
    X_Visual_Object *object = (X_Visual_Object *)DEREFHANDLE(objectHandle);
    Visual         **visual = (Visual **)DEREFHANDLE(visualHandle);
    
    *visual = v; FINISHED(visualHandle);
  
    object->type   = X_Visual;
    object->visual = visual;
    object->ds     = DEREFDISPLAYHANDLE(dsHandle);
  
    debugRefer(Visual,(v == None) ? None : v->visualid);
      
    return AddXObject(FINISHED(objectHandle));
  }
}

/******************************************************************************/
/*                                                                            */
/*      GC                                                                    */
/*                                                                            */
/******************************************************************************/
static X_GC_Object *GCObject(X_Object *P)
{
  assert(P->type == X_GC);

  return (X_GC_Object *)P;
}

static GC GetGC(X_Object *P)
{
  assert(P->type == X_GC);

  CheckExists(P,gc);

  return *(((X_GC_Object *)P)->gc);
}


static Handle GetDefaultGC(Handle dsHandle /* Handle to (X_Display_Object *) */)
{
  GC defaultGC = 
    DefaultGC(DEREFDISPLAYHANDLE(dsHandle)->display,
              DEREFDISPLAYHANDLE(dsHandle)->screen);

  X_GC_Object *G = FindGC(dsHandle,defaultGC->gid);

  if (G)
  {
    return SAVE(G);
  }
  else
  {
    Handle objectHandle = alloc_and_save(MUTABLE(SIZEOF(X_GC_Object)));
    Handle GCHandle     = alloc_and_save(MUTABLE(BYTES(1)));
    
    /* Must do all allocations before we do the first dereference */
    X_GC_Object *object  = (X_GC_Object *)DEREFHANDLE(objectHandle);
    GC          *gc      = (GC *)DEREFHANDLE(GCHandle);

    *gc = defaultGC; FINISHED(GCHandle);

    debugRefer(GC,defaultGC->gid);

    object->type = X_GC;
    object->gc   = gc;
    object->ds   = DEREFDISPLAYHANDLE(dsHandle);
/*
    object->font_object = 0;
    object->tile        = 0;
    object->stipple     = 0;
    object->clipMask    = 0;
*/    

    return AddXObject(objectHandle); /* must stay MUTABLE */
  }
}

static void ChangeGC(X_GC_Object *G, unsigned n, word *P)
{
  XGCValues v;
  
  unsigned mask = 1 << n;
  
  switch(mask)
  {
    case GCFunction:          v.function           = get_C_ushort(P); break;
    case GCPlaneMask:         v.plane_mask         = get_C_ulong (P); break;
    case GCForeground:        v.foreground         = get_C_ulong (P); break;
    case GCBackground:        v.background         = get_C_ulong (P); break;
    case GCLineWidth:         v.line_width         = get_C_short (P); break;
    case GCLineStyle:         v.line_style         = get_C_ushort(P); break;
    case GCCapStyle:          v.cap_style          = get_C_ushort(P); break;
    case GCJoinStyle:         v.join_style         = get_C_ushort(P); break;
    case GCFillStyle:         v.fill_style         = get_C_ushort(P); break;
    case GCFillRule:          v.fill_rule          = get_C_ushort(P); break;
    case GCTileStipXOrigin:   v.ts_x_origin        = get_C_short (P); break;
    case GCTileStipYOrigin:   v.ts_y_origin        = get_C_short (P); break;
    case GCSubwindowMode:     v.subwindow_mode     = get_C_ushort(P); break;
    case GCGraphicsExposures: v.graphics_exposures = get_C_ushort(P); break;
    case GCClipXOrigin:       v.clip_x_origin      = get_C_short (P); break;
    case GCClipYOrigin:       v.clip_y_origin      = get_C_short (P); break;
    case GCDashOffset:        v.dash_offset        = get_C_ushort(P); break;
    case GCDashList:          v.dashes             = get_C_uchar (P); break;
    case GCArcMode:           v.arc_mode           = get_C_ushort(P); break;

    case GCFont:     v.font = GetFont((X_Object *)P);
                     ASSIGN(G->font_object,FontObject((X_Object *)P));
                     break;
                     
    case GCTile:     v.tile = GetPixmap((X_Object *)P);
                     ASSIGN(G->tile,PixmapObject((X_Object *)P));
                     break;
                     
    case GCStipple:  v.stipple = GetPixmap((X_Object *)P);
                     ASSIGN(G->stipple, PixmapObject((X_Object *)P));
                     break;
                     
    case GCClipMask: v.clip_mask = GetPixmap((X_Object *)P);
                     ASSIGN(G->clipMask,PixmapObject((X_Object *)P));
                     break;

    default: Crash ("Bad gc mask %u",mask);
  }
  
  XChangeGC(GetDisplay((X_Object *)G),GetGC((X_Object *)G),mask,&v);
}

static Handle CreateGC
(
  Handle   dsHandle /* Handle to (X_Display_Object *) */,
  Drawable w
)
{
  Handle objectHandle = alloc_and_save(MUTABLE(SIZEOF(X_GC_Object)));
  Handle GCHandle     = alloc_and_save(MUTABLE(BYTES(1)));
  
  /* Must do all allocations before we do the first dereference */
  X_GC_Object *object  = (X_GC_Object *)DEREFHANDLE(objectHandle);
  GC          *gc      = (GC *)DEREFHANDLE(GCHandle);

  *gc = XCreateGC(DEREFDISPLAYHANDLE(dsHandle)->display,w,0,0);
  FINISHED(GCHandle);

  debugCreate(GC,(*gc)->gid);

  object->type = X_GC;
  object->gc   = gc;
  object->ds   = DEREFDISPLAYHANDLE(dsHandle);
/*
  object->font_object = 0;
  object->tile        = 0;
  object->stipple     = 0;
  object->clipMask    = 0;
*/    
  return AddXObject(objectHandle); /* must remain MUTABLE */
}


/******************************************************************************/
/*                                                                            */
/*      Window                                                                */
/*                                                                            */
/******************************************************************************/
static X_Window_Object *WindowObject(X_Object *P)

{
  assert(P->type == X_Window);

  return (X_Window_Object *)P;
}

static Window GetWindow(X_Object *P)
{
  if (P->type == X_Pixmap)
  {
    if (*P->p.pixmap == None) return None;
  
    RaiseXWindows ("Not a window");
  }

  assert(P->type == X_Window);

  CheckExists(P,window);

  return *(P->w.drawable);
}

static Handle EmptyWindow
(
  Handle dsHandle, /* Handle to (X_Display_Object *) */
  Window w
)
{
  X_Window_Object *W = FindWindow(dsHandle,w);

  if (W)
  {
    return SAVE(W);
  }
  else
  {
  
    Handle objectHandle    = alloc_and_save(MUTABLE(SIZEOF(X_Window_Object)));
    Handle eventMaskHandle = alloc_and_save(MUTABLE(BYTES(1)));
    Handle drawableHandle  = alloc_and_save(MUTABLE(BYTES(1)));
  
    /* Must do all allocations before we do the first dereference */
    X_Window_Object *object    = DEREFWINDOWHANDLE(objectHandle);
    Drawable        *drawable  = (Drawable *)DEREFHANDLE(drawableHandle);
    word            *eventMask = DEREFWORDHANDLE(eventMaskHandle);
 
    *drawable  = w; FINISHED(drawableHandle);
    
#ifdef nodef
    /* DCJM: This gets in the way of trying to handle ButtonPress events -
       get rid of it. */
    /* so that Motif windows get ButtonClick XEvent structures */
    *eventMask = ButtonClickMask; /* eventMask must remain MUTABLE */
#else
    *eventMask = 0;
#endif

    object->type      = X_Window;
    object->drawable  = drawable;
    object->handler   = nil_value;
    object->state     = nil_value;
    object->eventMask = eventMask;
/*
    object->colormap_object  = 0;
    object->cursor_object    = 0;
    object->backgroundPixmap = 0;
    object->borderPixmap     = 0;
    object->parent           = 0;
*/    
    object->ds        = DEREFDISPLAYHANDLE(dsHandle);

    debugRefer(Window,w);

    return AddXObject(objectHandle); /* must remain MUTABLE */
  }
}


/******************************************************************************/
/*                                                                            */
/*      Pixmap                                                                */
/*                                                                            */
/******************************************************************************/

static X_Pixmap_Object *PixmapObject(X_Object *P)
{
  assert(P->type == X_Pixmap);
  
  return (X_Pixmap_Object *)P;
}

static Pixmap GetPixmap(X_Object *P)
{
  if (P->type == X_Window)
  {

    if (! ResourceExists(P)) 
      {
        debug1("Non-existent window %x\n",(int)P);
      }

    if (*(P->w.drawable) == None) return None;
  
    RaiseXWindows ("Not a pixmap");
  }

  assert(P->type == X_Pixmap);

  /* val NoDrawable     = xcall (20,0) : Drawable; */
  /* val ParentRelative = xcall (20,1) : Drawable; */

  /* special case for NoDrawable */
  if (*P->p.pixmap == 0) return None;

  /* special case for ParentRelative */
  if (*P->p.pixmap == 1) return None;

  CheckExists(P,pixmap);

  return *(P->p.pixmap);
}

static Handle EmptyPixmap
(
  Handle dsHandle, /* Handle to (X_Display_Object *) */
  Pixmap id
)
{
  X_Pixmap_Object *E = FindPixmap(dsHandle,id);

  if (E)
  {
    return SAVE(E);
  }
  else
  {
    Handle objectHandle = alloc_and_save(MUTABLE(SIZEOF(X_Pixmap_Object)));
    Handle pixmapHandle = alloc_and_save(MUTABLE(BYTES(1)));
  
    /* Must do all allocations before we do the first dereference */
    X_Pixmap_Object *object  = (X_Pixmap_Object *)DEREFHANDLE(objectHandle);
    Pixmap          *pixmap  = (Pixmap *)DEREFHANDLE(pixmapHandle);
    
    *pixmap = id; FINISHED(pixmapHandle);

    object->type   = X_Pixmap;
    object->pixmap = pixmap;
    object->ds     = DEREFDISPLAYHANDLE(dsHandle);

    debugCreate(Pixmap,id);

    return AddXObject(FINISHED(objectHandle));
  }
}


/******************************************************************************/
/*                                                                            */
/*      Drawable                                                              */
/*                                                                            */
/******************************************************************************/

static Drawable GetDrawable(X_Object *P)
{
  CheckExists(P,drawable);

  switch(P->type)
  {
    case X_Window: return *(P->w.drawable);
    case X_Pixmap: return *(P->p.pixmap);
    default: Crash ("Bad X_Object type (%d) in GetDrawable",P->type);
  }
  
  /*NOTREACHED*/
}

/******************************************************************************/
/*                                                                            */
/*      DS / Display                                                          */
/*                                                                            */
/******************************************************************************/
static Handle GetDS(X_Object *P)
{
  X_Display_Object *ds;
  
  CheckExists(P,resource);

  switch(P->type)
  {
    case X_GC:       ds = P->g.ds; break;
    case X_Font:     ds = P->f.ds; break;
    case X_Cursor:   ds = P->c.ds; break;
    case X_Window:   ds = P->w.ds; break;
    case X_Pixmap:   ds = P->p.ds; break;
    case X_Colormap: ds = P->m.ds; break;
    case X_Visual:   ds = P->v.ds; break;
    case X_Widget:   ds = P->W.ds; break;
    case X_Display:  ds = &(P->d); break; /* i.e. P cast to the right type */
    
    default: Crash ("Bad X_Object type (%d) in GetDS",P->type);
  }

  assert(ds != (X_Display_Object *)nil_value); /* SPF */

  return SAVE(ds);
}


static Display *GetDisplay(X_Object *P)
{
  CheckExists(P,resource);

  switch(P->type)
  {
    case X_GC:       return P->g.ds->display;
    case X_Font:     return P->f.ds->display;
    case X_Cursor:   return P->c.ds->display;
    case X_Window:   return P->w.ds->display;
    case X_Pixmap:   return P->p.ds->display;
    case X_Colormap: return P->m.ds->display;
    case X_Visual:   return P->v.ds->display;
    case X_Widget:   return P->W.ds->display;
    case X_Display:  return P->d.display;
    
    default:         Crash ("Bad X_Object type (%d) in GetDisplay",P->type);
  }

  /*NOTREACHED*/
}


/******************************************************************************/
/*                                                                            */
/*      FS / Font                                                             */
/*                                                                            */
/******************************************************************************/
static Font GetFont(X_Object *P)
{
  assert(P->type == X_Font);
  
  /* val NoFont = xcall (22,0) : Font; */
  /* special case for NoFont - valid(?) */
  if (*(((X_Font_Object *)P)->font) == None) return None;

  CheckExists(P,font);
  
  return *(((X_Font_Object *)P)->font);
}


static Handle EmptyFont
(
  Handle       dsHandle, /* Handle to (X_Display_Object *) */
  Font         id,
  XFontStruct *fs
)
{
  X_Font_Object *E = FindFont(dsHandle,id);

  if (E && (fs == NULL || *(E->fs) == fs))
  {
    return SAVE(E);
  }
  else
  {
  
    Handle objectHandle  = alloc_and_save(MUTABLE(SIZEOF(X_Font_Object)));
    Handle fontHandle    = alloc_and_save(MUTABLE(BYTES(1)));
    Handle FSHandle      = alloc_and_save(MUTABLE(BYTES(1)));
  
    /* Must do all allocations before we do the first dereference */
    X_Font_Object *object = (X_Font_Object *)DEREFHANDLE(objectHandle);
    Font          *font   = (Font *)DEREFHANDLE(fontHandle);
    XFontStruct  **FS     = (XFontStruct **)DEREFHANDLE(FSHandle);

    *font = id; FINISHED(fontHandle);
    *FS   = fs; FINISHED(FSHandle);

    object->type = X_Font;
    object->font = font;
    object->fs   = FS;
    object->ds   = DEREFDISPLAYHANDLE(dsHandle);

    debugCreate(Font,id);

    return AddXObject(FINISHED(objectHandle));
  }
}



/******************************************************************************/
/*                                                                            */
/*      Cursor                                                                */
/*                                                                            */
/******************************************************************************/
static X_Cursor_Object *CursorObject(X_Object *P)
{
  assert(P->type == X_Cursor);

  return (X_Cursor_Object *)P;
}

static Cursor GetCursor(X_Object *P)
{
  assert(P->type == X_Cursor);
  
  /* val NoCursor = xcall (21,0) : Cursor; */
  /* special case for NoCursor */
  if (*(((X_Cursor_Object *)P)->cursor) == None) return None;

  CheckExists(P,cursor);

  return *(((X_Cursor_Object *)P)->cursor);
}


static Handle EmptyCursor
(
  Handle dsHandle, /* Handle to (X_Display_Object *) */
  Cursor id
)
{
  X_Cursor_Object *E = FindCursor(dsHandle,id);

  if (E)
  {
    return SAVE(E);
  }
  else
  {
  
    Handle objectHandle  = alloc_and_save(MUTABLE(SIZEOF(X_Cursor_Object)));
    Handle cursorHandle  = alloc_and_save(MUTABLE(BYTES(1)));
  
    /* Must do all allocations before we do the first dereference */
    X_Cursor_Object *object = (X_Cursor_Object *)DEREFHANDLE(objectHandle);
    Cursor          *cursor = (Cursor *)DEREFHANDLE(cursorHandle);
    
    *cursor = id; FINISHED(cursorHandle);

    object->type   = X_Cursor;
    object->cursor = cursor;
    object->ds     = DEREFDISPLAYHANDLE(dsHandle);

    debugRefer(Cursor,id);

    return AddXObject(FINISHED(objectHandle));
  }
}

static Handle CreateFontCursor
(
  Handle   dsHandle, /* Handle to (X_Display_Object *) */
  unsigned shape
)
{
  return EmptyCursor(dsHandle,XCreateFontCursor(DEREFDISPLAYHANDLE(dsHandle)->display,shape));
}

static Handle CreateGlyphCursor
(
  Handle    dsHandle, /* Handle to (X_Display_Object *) */
  Font      sf,
  Font      mf,
  unsigned  sc,
  unsigned  mc,
  XColor   *foreground,
  XColor   *background
)
{
  return EmptyCursor(dsHandle,XCreateGlyphCursor(DEREFDISPLAYHANDLE(dsHandle)->display,sf,mf,sc,mc,foreground,background));
}

static Handle CreatePixmapCursor
(
  Handle    dsHandle, /* Handle to (X_Display_Object *) */
  Pixmap    source,
  Pixmap    mask,
  XColor   *foreground,
  XColor   *background,
  unsigned  x,
  unsigned  y
)
{
  return EmptyCursor(dsHandle,XCreatePixmapCursor(DEREFDISPLAYHANDLE(dsHandle)->display,source,mask,foreground,background,x,y));
}

/******************************************************************************/
/*                                                                            */
/*      Widget                                                                */
/*                                                                            */
/******************************************************************************/
static Widget GetNWidget(X_Object *P)
{
  assert(P->type == X_Widget);
  
  if (*(((X_Widget_Object *)P)->widget) == NULL) return NULL;
  
  CheckExists(P,widget);

  return *(((X_Widget_Object *)P)->widget);
}

static Widget GetWidget(X_Object *P)
{
  assert(P->type == X_Widget);
  
  if (*(((X_Widget_Object *)P)->widget) == NULL)
    {
      RaiseXWindows ("Not a real widget");
    }

  CheckExists(P,widget);

  return *(((X_Widget_Object *)P)->widget);
}

/* added 6/11/94 SPF */
static Widget GetRealizedWidget(char *where, X_Object *P)
{
  Widget w;
  
  assert(P->type == X_Widget);

  w = *(((X_Widget_Object *)P)->widget);

  if (w == NULL)
  {
     RaiseXWindows2(where,": not a real widget");
  }

  CheckExists(P,widget);

  if (XtIsRealized(w) == False)
  {
     RaiseXWindows2(where,": widget is not realized");
  }

  return w;
}

/* P is a pointer to an X_Widget_Object */
static X_Widget_Object *WidgetObjectToken(X_Object *P)
{
  assert(P->type == X_Widget);
  return (X_Widget_Object *)P;
}

/* P is a pointer to an X_Widget_Object, which is bound to a C widget */
static X_Widget_Object *WidgetObject(X_Object *P)
{
  assert(P->type == X_Widget);
  
  CheckExists(P,widget);

  return (X_Widget_Object *)P;
}


static Handle EmptyWidget
(
  Handle dsHandle, /* Handle to (X_Display_Object *) */
  Widget id
)
{
  X_Widget_Object *E = FindWidget(id);

  if (E)
  {
    return SAVE(E);
  }
  else
  {
  
    Handle objectHandle = alloc_and_save(MUTABLE(SIZEOF(X_Widget_Object)));
    Handle widgetHandle = alloc_and_save(MUTABLE(BYTES(1)));
  
    /* Must do all allocations before we do the first dereference */
    X_Widget_Object *object = (X_Widget_Object *)DEREFHANDLE(objectHandle);
    Widget          *widget = (Widget *)DEREFHANDLE(widgetHandle);

    *widget = id; FINISHED(widgetHandle);

    object->type         = X_Widget;
    object->widget       = widget;
    object->callbackList = (ML_Cons_Cell *)nil_value;
    object->state        = nil_value;
    object->ds           = DEREFDISPLAYHANDLE(dsHandle);

    debugRefer(Widget,id);
    
    return AddXObject(objectHandle); /* Must stay MUTABLE */
  }
}

static Handle NewWidget
(
  Handle dsHandle, /* Handle to (X_Display_Object *) */
  Widget id
)
{
  X_Widget_Object *E = FindWidget(id);

  if (E) DestroyXObject((X_Object *)E);
  
  return EmptyWidget(dsHandle,id);
}


/******************************************************************************/
/*                                                                            */
/*      Text Widgets                                                          */
/*                                                                            */
/******************************************************************************/
static Widget GetTextWidget(char *funcname, X_Object *P)
{
  Widget w = GetWidget(P);
  
  if (XmIsText(w)) return w;
  
  /* Text operations are also legal on TextField widgets */
  if (XmIsTextField(w)) return w;
  
  RaiseXWindows2(funcname,": not a Text or TextField widget");
  /*NOTREACHED*/
}


/******************************************************************************/
/*                                                                            */
/*      TextField Widgets                                                     */
/*                                                                            */
/******************************************************************************/
static Widget GetTextFieldWidget(char *funcname, X_Object *P)
{
  Widget w = GetWidget(P);
  
  if (XmIsTextField(w)) return w;
  
  RaiseXWindows2(funcname,": not a TextField widget");
  /*NOTREACHED*/
}

/******************************************************************************/
/*                                                                            */
/*      List Widgets                                                          */
/*                                                                            */
/******************************************************************************/
static Widget GetListWidget(char *funcname, X_Object *P)
{
  Widget w = GetWidget(P);
  
  if (XmIsList(w)) return w;
  
  RaiseXWindows2(funcname,": not a List widget");
  /*NOTREACHED*/
}


/******************************************************************************/
/*                                                                            */
/*      Window                                                                */
/*                                                                            */
/******************************************************************************/
static void RemoveWindowEvents(Display *d, Window w)
{
  XEvent event;

  XSync(d,False);

  while(XCheckWindowEvent(d,w,~0,&event))
  {
    /* do nothing */
  }
}


static Handle AddWindow
(
  Window W,
  Handle handlerHandle, /* Handle to (word *) (?)  */
  Handle stateHandle,   /* Handle to (word *) (?)  */      
  Handle parentHandle   /* Handle to (X_Window_Object *) */
)
{
  XWMHints hints;
  Atom deleteWindow; /* was int SPF 6/1/94 */
  Display *d = GetDisplay(DEREFXOBJECTHANDLE(parentHandle));

  Handle objectHandle    = alloc_and_save(MUTABLE(SIZEOF(X_Window_Object)));
  Handle eventMaskHandle = alloc_and_save(MUTABLE(BYTES(1)));
  Handle drawableHandle  = alloc_and_save(MUTABLE(BYTES(1)));

  /* Must do all allocations before we do the first dereference */
  X_Window_Object *object    = DEREFWINDOWHANDLE(objectHandle);
  Drawable        *drawable  = (Drawable *)DEREFHANDLE(drawableHandle);
  word            *eventMask = DEREFWORDHANDLE(eventMaskHandle);

  *eventMask = 0; /* eventMask must remain MUTABLE */
  *drawable  = W; FINISHED(drawableHandle);

  hints.flags = InputHint;
  hints.input = True;

  XSetWMHints(d,W,&hints);
  
  deleteWindow = WM_DELETE_WINDOW(d);
  
  if (deleteWindow != None) XSetWMProtocols(d,W,&deleteWindow,1);

  debugCreate(Window,W);

  object->type      = X_Window;
  object->drawable  = drawable;
  object->eventMask = eventMask;
  object->handler   = DEREFWORDHANDLE(handlerHandle);
  object->state     = DEREFWORDHANDLE(stateHandle);
  object->parent    = DEREFWINDOWHANDLE(parentHandle);
  object->ds        = DEREFWINDOWHANDLE(parentHandle)->ds; /* Tidy up (?) */
/*
  object->colormap_object  = 0;
  object->cursor_object    = 0;
  object->backgroundPixmap = 0;
  object->borderPixmap     = 0;
*/

  if (ISNIL(DEREFHANDLE(handlerHandle))) Crash ("No handler set");

  return AddXObject(objectHandle); /* object must remain MUTABLE */
}

static void DestroyWindow(X_Object *W /* Should be a Window Object! */)
{
  Window   w = GetWindow(W);
  Display *d = GetDisplay(W);
  
  debugReclaim(Window,w);

  XUnmapWindow(d,w);

  DestroySubwindows(W);
  
  XDestroyWindow(d,w);

  RemoveWindowEvents(d,w);
}

static Handle CreateSimpleWindow
(
  Handle   parent, /* Handle to (X_Window_Object *) */
  int      x,
  int      y,
  unsigned w,
  unsigned h,
  unsigned borderWidth,
  unsigned border,
  unsigned background,
  Handle   handler, /* Handle to (word *) (?) */
  Handle   state    /* Handle to (word *) (?) */
)
{
  Window W = XCreateSimpleWindow(GetDisplay(DEREFXOBJECTHANDLE(parent)),
                                   GetWindow(DEREFXOBJECTHANDLE(parent)),
                                   x,y,w,h,
                                   borderWidth,border,background);

  if (W == 0) RaiseXWindows ("XCreateSimpleWindow failed");

  return AddWindow(W,handler,state,parent);
}

static Handle CreateWindow
(
  Handle   parent, /* Handle to (X_Window_Object *) */
  int      x,
  int      y,
  unsigned w,
  unsigned h,
  unsigned borderWidth,
  unsigned depth,
  unsigned class,
  Visual   *visual,
  Handle   handler, /* Handle to (word *) (?) */
  Handle   state    /* Handle to (word *) (?) */
)
{
  Window W;

  W = XCreateWindow(GetDisplay(DEREFXOBJECTHANDLE(parent)),
                      GetWindow(DEREFXOBJECTHANDLE(parent)),
                      x,y,w,h,
                      borderWidth,depth,class,visual,0,0);

  if (W == 0) RaiseXWindows ("XCreateWindow failed");

  return AddWindow(W,handler,state,parent);
}

static void DestroySubwindows(X_Object *W /* should be a Window object! */)
{
  Window   root,parent,*children;
  unsigned n;
  int      s;

  Window   w = GetWindow(W);
  Display *d = GetDisplay(W);
  
  s = XQueryTree(d,w,&root,&parent,&children,&n);

  if (s == 0)
  {
    RaiseXWindows ("XDestroySubwindows failed");
    return;
  }
  
  XUnmapSubwindows(d,w);

  if (n)
  {
    Handle dsHandle = GetDS(W);

    while(n--)
    {
      X_Window_Object *child = FindWindow(dsHandle,children[n]);

      if (child) DestroyXObject((X_Object *)child);
    }

    XFree((char *)children);
  }

  XDestroySubwindows(d,w);
}


/******************************************************************************/
/*                                                                            */
/*      Translations / Accelerators                                           */
/*                                                                            */
/******************************************************************************/
static Handle EmptyTrans(XtTranslations table)
{
  Handle objectHandle = alloc_and_save(MUTABLE(BYTES(SIZEOF(X_Trans_Object))));

  /* Must do all allocations before we do the first dereference */
  X_Trans_Object *object = (X_Trans_Object *)DEREFHANDLE(objectHandle);

  /* OK to store C values because this is a byte object */
  object->type  = X_Trans;
  object->table = table;
  
  debugRefer(Trans,table);

  return AddXObject(FINISHED(objectHandle));
}

static XtTranslations GetTrans(X_Object *P)
{
  assert(P->type == X_Trans);
  
  CheckExists(P,trans);
  
  return ((X_Trans_Object *)P)->table;
}

static Handle EmptyAcc(XtTranslations acc)
{

  Handle objectHandle = alloc_and_save(MUTABLE(BYTES(SIZEOF(X_Acc_Object))));

  /* Must do all allocations before we do the first dereference */
  X_Acc_Object *object = (X_Acc_Object *)DEREFHANDLE(objectHandle);

  /* OK to store C values because this is a byte object */
  object->type = X_Acc;
  object->acc  = acc;

  debugRefer(Acc,acc);

  return AddXObject(FINISHED(objectHandle));
}

static XtAccelerators GetAcc(X_Object *P)
{
  assert(P->type == X_Acc);
  
  CheckExists(P,acc);
  
  return ((X_Acc_Object *)P)->acc;
}

/******************************************************************************/
/*                                                                            */
/*      Utility functions                                                     */
/*                                                                            */
/******************************************************************************/

static XtGrabKind GetXtGrabKind(word *P)
{
  int i = get_C_long(P);
  
  /* This encoding must be the same as that used in Motif/ml_bind.ML */
  switch (i)
  {
    case 0:  return XtGrabNone;
    case 1:  return XtGrabNonexclusive;
    case 2:  return XtGrabExclusive;
    
    default: Crash ("Bad XtGrabKind index (%d) in GetXtGrabKind",i);
  }

  return XtGrabNone; /* to keep lint/gcc happy */
}

/******************************************************************************/
/*                                                                            */
/*      MLXStandardColormap - implements ML XStandardColormap datatype        */
/*                                                                            */
/******************************************************************************/

typedef struct
{
X_Colormap_Object *Colormap;
word              *redMax;    /* ML int */
word              *redMult;   /* ML int */
word              *greenMax;  /* ML int */
word              *greenMult; /* ML int */
word              *blueMax;   /* ML int */
word              *blueMult;  /* ML int */
word              *basePixel; /* ML int */
X_Visual_Object   *visual;
}  MLXStandardColormap;


static void GetStandardColormap(MLXStandardColormap *P, XStandardColormap *s)
{
  s->colormap   = GetColormap((X_Object *)P->Colormap);
  
  s->red_max    = get_C_ulong(P->redMax);
  s->red_mult   = get_C_ulong(P->redMult);
  s->green_max  = get_C_ulong(P->greenMax);
  s->green_mult = get_C_ulong(P->greenMult);
  s->blue_max   = get_C_ulong(P->blueMax);
  s->blue_mult  = get_C_ulong(P->blueMult);
  s->base_pixel = get_C_ulong(P->basePixel);
  
  s->visualid   = GetVisual((X_Object *)P->visual)->visualid; /* UNSAFE(?) */
  s->killid     = None;
}

static Handle CreateStandardColormap
(
  XStandardColormap *s,
  word               i,
  Handle             dsHandle /* Handle to (X_Display_Object *) */
)
{
  XVisualInfo T;
  XVisualInfo *info;
  int count;
  
  Handle tupleHandle = alloc_and_save(MUTABLE(SIZEOF(MLXStandardColormap)));
  
  T.visualid = s->visualid;
  T.visual   = None;
  
  info = XGetVisualInfo(DEREFDISPLAYHANDLE(dsHandle)->display,VisualIDMask,&T,&count);
  
  if (info)
  {
    T.visual = info->visual;
    
    XFree((char *)info);
  }

/* Still allocating, so must use explicit DEREF for each element */
#define tuple /* hack */((MLXStandardColormap *)DEREFHANDLE(tupleHandle))
  tuple->Colormap  = (X_Colormap_Object *)DEREFHANDLE(EmptyColormap(dsHandle,s->colormap));
  tuple->redMax    = DEREFWORDHANDLE(Make_unsigned(s->red_max));
  tuple->redMult   = DEREFWORDHANDLE(Make_unsigned(s->red_mult));
  tuple->greenMax  = DEREFWORDHANDLE(Make_unsigned(s->green_max));
  tuple->greenMult = DEREFWORDHANDLE(Make_unsigned(s->green_mult));
  tuple->blueMax   = DEREFWORDHANDLE(Make_unsigned(s->blue_max));
  tuple->blueMult  = DEREFWORDHANDLE(Make_unsigned(s->blue_mult));
  tuple->basePixel = DEREFWORDHANDLE(Make_unsigned(s->base_pixel));
  tuple->visual    = (X_Visual_Object *)DEREFHANDLE(EmptyVisual(dsHandle,T.visual));
#undef tuple
  
  return FINISHED(tupleHandle);
}


/******************************************************************************/
/*                                                                            */
/*      Polymorphic pairs                                                     */
/*                                                                            */
/******************************************************************************/

typedef struct
{
word *x0; /* first value  */
word *x1; /* second value */
}
MLPair;


/* Polymorphic pair creation */
static Handle CreatePair(Handle p1, Handle p2)
{
  Handle pairHandle = alloc_and_save(MUTABLE(SIZEOF(MLPair)));

/* Still allocating, so must use explicit DEREF for each element */
#define pair ((MLPair *)DEREFHANDLE(pairHandle))
  pair->x0 = DEREFWORDHANDLE(p1);
  pair->x1 = DEREFWORDHANDLE(p2);
#undef pair

  return FINISHED(pairHandle);
}


/******************************************************************************/
/*                                                                            */
/*      Polymorphic triples                                                   */
/*                                                                            */
/******************************************************************************/

typedef struct
{
word *x0; /* first value  */
word *x1; /* second value */
word *x2; /* third value  */
}
MLTriple;

#define FST(P)   ((P)->x0)
#define SND(P)   ((P)->x1)
#define THIRD(P) ((P)->x2)

static Handle CreateTriple(Handle p1, Handle p2, Handle p3)
{
  Handle tripleHandle = alloc_and_save(MUTABLE(SIZEOF(MLTriple)));

/* Still allocating, so must use explicit DEREF for each element */
#define triple ((MLTriple *)DEREFHANDLE(tripleHandle))
  triple->x0 = DEREFWORDHANDLE(p1);
  triple->x1 = DEREFWORDHANDLE(p2);
  triple->x2 = DEREFWORDHANDLE(p3);
#undef triple

  return FINISHED(tripleHandle);
}


/******************************************************************************/
/*                                                                            */
/*      MLXImage - Implements ML XImage datatype                              */
/*                                                                            */
/******************************************************************************/
typedef struct
{
  String *data;            /* ML (abstype containing) string */
  MLXRectangle *size;
  word *depth;           /* ML int */
  word *format;          /* (short ML int) XYBitmap | XYPixmap | ZPixmap */
  word *xoffset;         /* ML int */
  word *bitmapPad;       /* ML int */
  word *byteOrder;       /* (short ML int) LSBFirst | MSBFirst */
  word *bitmapUnit;      /* ML int */
  word *bitsPerPixel;    /* ML int */
  word *bytesPerLine;    /* ML int */
  word *visualRedMask;   /* ML int */
  word *bitmapBitOrder;  /* (short ML int) LSBFirst | MSBFirst */
  word *visualBlueMask;  /* ML int */
  word *visualGreenMask; /* ML int */
} MLXImage;

#define MLImageFormat(n) (n+1)
#define MLImageOrder(n)  (n+1)
#define CImageFormat(n)  (n-1)
#define CImageOrder(n)   (n-1)

static int ImageBytes(XImage *image)
{
  int dsize = image->bytes_per_line * image->height;

  if (image->format == XYPixmap) dsize = dsize * image->depth;
  
  return dsize;
}

static XImage *GetXImage(Display *d, MLXImage *I)
/* can only be called once per X opcode */
{
  static XImage image = { 0 };
  
  String  *data           = GetString(I->data);
  unsigned width          = GetRectW(I->size);
  unsigned height         = GetRectH(I->size);
  unsigned depth          = get_C_ulong(I->depth);
  unsigned format         = get_C_ulong(I->format);
  int      xoffset        = get_C_short(I->xoffset);
  int      bitmapPad      = get_C_short(I->bitmapPad);
  int      bytesPerLine   = get_C_long (I->bytesPerLine);

  unsigned byteOrder      = get_C_ulong(I->byteOrder);
  unsigned bitmapUnit     = get_C_ulong(I->bitmapUnit);
  unsigned bitsPerPixel   = get_C_ulong(I->bitsPerPixel);
  unsigned bitmapBitOrder = get_C_ulong(I->bitmapBitOrder);

  format         = CImageFormat(format);
  byteOrder      = CImageOrder(byteOrder);
  bitmapBitOrder = CImageOrder(bitmapBitOrder);
  
  image.width            = width;
  image.height           = height;
  image.xoffset          = xoffset;
  image.format           = format;
  image.data             = data->chars;
  image.byte_order       = byteOrder;
  image.bitmap_unit      = bitmapUnit;
  image.bitmap_bit_order = bitmapBitOrder;
  image.bitmap_pad       = bitmapPad;
  image.depth            = depth;
  image.bytes_per_line   = bytesPerLine;
  image.bits_per_pixel   = bitsPerPixel;
  image.red_mask         = get_C_ulong(I->visualRedMask);
  image.green_mask       = get_C_ulong(I->visualGreenMask);
  image.blue_mask        = get_C_ulong(I->visualBlueMask);
  
  if (ImageBytes(&image) != data->length) RaiseXWindows ("Bad image string length");
  
  return &image;
}

static Handle CreateImage(XImage *image)
{
  Handle XHandle = alloc_and_save(MUTABLE(SIZEOF(MLXImage)));
  
  int dsize = ImageBytes(image);

/* Still allocating, so must use explicit DEREF for each element */
#define  X ((MLXImage *)DEREFHANDLE(XHandle))
  X->data            = (String *)Buffer_to_Poly(image->data,dsize);
  X->size            = (MLXRectangle *)DEREFHANDLE(CreateArea(image->width,image->height));
  X->depth           = DEREFWORDHANDLE(Make_unsigned(image->depth));
  X->format          = DEREFWORDHANDLE(Make_unsigned(MLImageFormat(image->format)));
  X->xoffset         = DEREFWORDHANDLE(Make_int(image->xoffset));
  X->bitmapPad       = DEREFWORDHANDLE(Make_int(image->bitmap_pad));
  X->byteOrder       = DEREFWORDHANDLE(Make_unsigned(MLImageOrder(image->byte_order)));
  X->bitmapUnit      = DEREFWORDHANDLE(Make_unsigned(image->bitmap_unit));
  X->bitsPerPixel    = DEREFWORDHANDLE(Make_unsigned(image->bits_per_pixel));
  X->bytesPerLine    = DEREFWORDHANDLE(Make_int(image->bytes_per_line));
  X->visualRedMask   = DEREFWORDHANDLE(Make_unsigned(image->red_mask));
  X->bitmapBitOrder  = DEREFWORDHANDLE(Make_unsigned(MLImageOrder(image->bitmap_bit_order)));
  X->visualBlueMask  = DEREFWORDHANDLE(Make_unsigned(image->blue_mask));
  X->visualGreenMask = DEREFWORDHANDLE(Make_unsigned(image->green_mask));
#undef X

  XDestroyImage(image);
  
  return FINISHED(XHandle);
}

static Handle GetImage
(
  Display *d,
  Drawable drawable,
  int      x,
  int      y,
  unsigned w,
  unsigned h,
  unsigned /* long */ mask,
  int format
)
{
  XImage *image = XGetImage(d,drawable,x,y,w,h,mask,CImageFormat(format));
  
  if (image == 0) RaiseXWindows ("XGetImage failed");
  
  return CreateImage(image);
}

static Handle SubImage
(
  XImage   *image,
  int      x,
  int      y,
  unsigned w,
  unsigned h
)
{
  XImage *subimage = XSubImage(image,x,y,w,h);
  
  if (subimage == 0) RaiseXWindows ("XSubImage failed");
  
  return CreateImage(subimage);
}


/******************************************************************************/
/*                                                                            */
/*      XImage                                                                */
/*                                                                            */
/******************************************************************************/
static void GetSubImage
(
  Display  *d,
  Drawable  drawable,
  int       sx,
  int       sy,
  unsigned  sw,
  unsigned  sh,
  unsigned /* long */ mask,
  int       format,
  XImage   *image,
  int       dx,
  int       dy
)
{
  XGetSubImage(d,drawable,sx,sy,sw,sh,mask,CImageFormat(format),image,dx,dy);
  
  /* XFree((char *)image); */
}

static void PutImage
(
  Display  *d,
  Drawable drawable,
  GC       gc,
  XImage  *image,
  int      sx,
  int      sy,
  int      dx,
  int      dy,
  unsigned dw,
  unsigned dh
)
{
  XPutImage(d,drawable,gc,image,sx,sy,dx,dy,dw,dh);
  
  /* XFree((char *)image); */
}

static Handle GetPixel(XImage *image, int x, int y)
{
  unsigned pixel = XGetPixel(image,x,y);
  
  /* XFree((char *)image); */
  
  return Make_unsigned(pixel);
}

static void PutPixel(XImage *image, int x, int y, unsigned pixel)
{
  XPutPixel(image,x,y,pixel);
  
  /* XFree((char *)image); */
}

static void AddPixel(XImage *image, unsigned value)
{
  XAddPixel(image,value);
  
  /* XFree((char *)image); */
}


/******************************************************************************/
/*                                                                            */
/*      TimeVal                                                               */
/*                                                                            */
/******************************************************************************/
static int DoubleClickTime = 250; /* Double click time in milliseconds       */
static int MouseDrift      = 5;   /* Mouse movement allowed in button events */

static void NormaliseTime(TimeVal *t)
{
  while(t->tv_usec >= 1000000) { t->tv_usec -= 1000000; t->tv_sec++; }
  while(t->tv_usec < 0)        { t->tv_usec += 1000000; t->tv_sec--; }
}

static void TimeAdd(TimeVal *a, TimeVal *b, TimeVal *t)
{
  t->tv_sec  = a->tv_sec  + b->tv_sec;
  t->tv_usec = a->tv_usec + b->tv_usec;

  NormaliseTime(t);
}

static int TimeLt(TimeVal *a, TimeVal *b)
{
  return ((a->tv_sec <  b->tv_sec) ||
         ((a->tv_sec == b->tv_sec) && (a->tv_usec <  b->tv_usec)));
}

static int TimeLeq(TimeVal *a, TimeVal *b)
{
  return ((a->tv_sec <  b->tv_sec) ||
         ((a->tv_sec == b->tv_sec) && (a->tv_usec <=  b->tv_usec)));
}

/******************************************************************************/
/*                                                                            */
/*      (?)                                                                   */
/*                                                                            */
/******************************************************************************/
typedef struct
{
  XButtonEvent *button;    /* initial button press event   */
  int           up,down;   /* count of button transitions  */
} PredicateArgs;

static Bool SameClickEvent(Display *dpy, XEvent *ev, XPointer arg)
{
  PredicateArgs *A = (PredicateArgs *)arg;
  
  switch(ev->type)
  {
    case MotionNotify:
    {
      int dx = ev->xmotion.x - A->button->x;
      int dy = ev->xmotion.y - A->button->y;

      if (ev->xmotion.window != A->button->window) return False;

      if (abs(dx) > MouseDrift) return False;
      if (abs(dy) > MouseDrift) return False;

      return True;
    }

    case ButtonPress:
    case ButtonRelease:
    {
      int dx = ev->xbutton.x - A->button->x;
      int dy = ev->xbutton.y - A->button->y;

      if (ev->xbutton.window != A->button->window) return False;

      if (ev->xbutton.button != A->button->button) return False;

      if (abs(dx) > MouseDrift) return False;
      if (abs(dy) > MouseDrift) return False;

      if (ev->type == ButtonPress) A->down++; else A->up++;

      return True;
    }
  }

  return False;
}

static void WaitDoubleClickTime(Handle dsHandle, PredicateArgs *A)
{
  XEvent N;
  TimeVal start_time,end_time,dt;
  Display *d = DEREFDISPLAYHANDLE(dsHandle)->display;

  /* 
    AIX doesn't document support for NULL pointers in the select call,
     so we have to initialise empty fd_sets instead. SPF 30/10/95
  */
  fd_set read_fds, write_fds, except_fds;
  FD_ZERO(&read_fds);
  FD_ZERO(&write_fds);
  FD_ZERO(&except_fds);

  {
    int fd     = d->fd;
    assert (0 <= fd && fd < FD_SETSIZE); 
    FD_SET(fd,&read_fds);
  }

  GETTIMEOFDAY(&start_time);

  dt.tv_sec  = 0;
  dt.tv_usec = DoubleClickTime * 1000;

  TimeAdd(&start_time,&dt,&end_time);

  for (;;)
  {
    int extended = 0;

    while(XCheckIfEvent(d,&N,SameClickEvent,(char *) A))
    {
      if (DEREFDISPLAYHANDLE(dsHandle)->app_context) XtDispatchEvent(&N);
      
      extended = 1;
    }

    if (QLength(d)) break;  /* some other event to be processed next */

    if (extended)           /* button event extended, so extend time period */
    {
      dt.tv_sec  = 0;
      dt.tv_usec = DoubleClickTime * 1000;

      TimeAdd(&end_time,&dt,&end_time);
    }

    if (TimeLeq(&end_time,&start_time)) break; /* the time period has elapsed */

    select(FD_SETSIZE,&read_fds,&write_fds,&except_fds,&dt);

    GETTIMEOFDAY(&start_time);
  }
}

static Handle GetKeyVector(uchar *keys, word i)
{
  word index = i / 8;
  word mask  = 1 << (i % 8);
  
  return Make_bool(keys[index] &mask);
}

static Handle QueryKeymap(Display *d)
{
  char keys[32];
  
  XQueryKeymap(d,keys);
  
  return CreateList4(256,keys,0,GetKeyVector);
}

/******************************************************************************/
/*                                                                            */
/*      EventName                                                             */
/*                                                                            */
/******************************************************************************/
typedef struct
{
  char *name;
  int   type;
} EventName;

static EventName EventNames[] =
{
  { "KeyPress",KeyPress },
  { "KeyRelease",KeyRelease },
  { "ButtonPress",ButtonPress },
  { "ButtonRelease",ButtonRelease },
  { "MotionNotify",MotionNotify },
  { "EnterNotify",EnterNotify },
  { "LeaveNotify",LeaveNotify },
  { "FocusIn",FocusIn },
  { "FocusOut",FocusOut },
  { "KeymapNotify",KeymapNotify },
  { "Expose",Expose },
  { "GraphicsExpose",GraphicsExpose },
  { "NoExpose",NoExpose },
  { "VisibilityNotify",VisibilityNotify },
  { "CreateNotify",CreateNotify },
  { "DestroyNotify",DestroyNotify },
  { "UnmapNotify",UnmapNotify },
  { "MapNotify",MapNotify },
  { "MapRequest",MapRequest },
  { "ReparentNotify",ReparentNotify },
  { "ConfigureNotify",ConfigureNotify },
  { "ConfigureRequest",ConfigureRequest },
  { "GravityNotify",GravityNotify },
  { "ResizeRequest",ResizeRequest },
  { "CirculateNotify",CirculateNotify },
  { "CirculateRequest",CirculateRequest },
  { "PropertyNotify",PropertyNotify },
  { "SelectionClear",SelectionClear },
  { "SelectionRequest",SelectionRequest },
  { "SelectionNotify",SelectionNotify },
  { "ColormapNotify",ColormapNotify },
  { "ClientMessage",ClientMessage },
  { "MappingNotify",MappingNotify },
};

#define NEVENTS (sizeof(EventNames)/sizeof(EventName))

static char *DebugEventName(int type)
{
  int i;

  for(i = 0; i < NEVENTS; i++)
  {
    if (EventNames[i].type == type) return EventNames[i].name;
  }
  
  return "** BAD EVENT **";
}

static int WM_PROTOCOLS(Display *d)
{
  static int protocols = None;
  
  if (protocols == None) protocols = XInternAtom(d,"WM_PROTOCOLS",True);
  
  return protocols;
}

static Atom WM_DELETE_WINDOW(Display *d)
{
  static Atom deleteWindow = None;
  
  if (deleteWindow == None) deleteWindow = XInternAtom(d,"WM_DELETE_WINDOW",True);
  
  return deleteWindow;
}

/******************************************************************************/
/*                                                                            */
/*      Structures used by CreateEvent function.                              */
/*                                                                            */
/* These typedefs should correspond with the tuples used by MakeXKeyEvent etc */
/*                                                                            */
/******************************************************************************/


typedef struct
{
X_Window_Object *root;
X_Window_Object *subwindow;
word            *time;       /* ML int */
MLXPoint        *pointer;
MLXPoint        *rootPointer;
word            *modifiers;  /* ML modifier (int) */
word            *keycode;    /* ML int */
} ML_KeyEvent_Data;

typedef struct
{
X_Window_Object *root;
X_Window_Object *subwindow;
word            *time;       /* ML int */
MLXPoint        *pointer;
MLXPoint        *rootPointer;
word            *modifiers;  /* ML modifier (int) */
word            *button;     /* ML int */
} ML_ButtonEvent_Data;

typedef struct
{
X_Window_Object *root;
X_Window_Object *subwindow;
word            *time;       /* ML int */
MLXPoint        *pointer;
MLXPoint        *rootPointer;
word            *modifiers;  /* ML modifier (int) */
word            *button;     /* ML int */
word            *up;         /* ML int */
word            *down;       /* ML int */
} ML_ButtonClick_Data;

typedef struct
{
X_Window_Object *root;
X_Window_Object *subwindow;
word            *time;       /* ML int */
MLXPoint        *pointer;
MLXPoint        *rootPointer;
word            *modifiers;  /* ML modifier (int) */
word            *isHint;     /* ML bool */
} ML_MotionEvent_Data;


typedef struct
{
X_Window_Object *root;
X_Window_Object *subwindow;
word            *time;       /* ML int */
MLXPoint        *pointer;
MLXPoint        *rootPointer;
word            *mode;        /* ?  */
word            *detail;      /* ? */
word            *focus;       /* ? */
word            *modifiers;   /* ML modifier (int) */
} ML_CrossingEvent_Data;


typedef struct
{
MLXRectangle *region;
word         *count;  /* ML int */
} ML_ExposeEvent_Data;

typedef struct
{
X_Window_Object *window;
MLXPoint        *position;
MLXRectangle    *size;
word            *borderWidth;      /* ML int */
X_Window_Object *above;
word            *overrideRedirect; /* ML bool */
} ML_ConfigureNotify_Data;

typedef struct
{
X_Window_Object *window;
MLXPoint        *position;
MLXRectangle    *size;
word            *borderWidth;
X_Window_Object *above;
word            *detail;      /* ? */
} ML_ConfigureRequest_Data;


typedef struct
{
MLXRectangle *region;
word         *count;  /* ML int */
word         *code;   /* ML int */
} ML_GraphicsExposeEvent_Data;

typedef struct
{
word *mode;   /* ML int ? */
word *detail; /* ML int ? */
} ML_FocusChangeEvent_Data;

typedef struct
{
X_Window_Object *window;
MLXPoint        *position;
MLXRectangle    *size;
word            *borderWidth;      /* ML int */
word            *overrideRedirect; /* ML bool */
} ML_CreateEvent_Data;

typedef struct
{
X_Window_Object *window;
word            *fromConfigure; /* ML bool */
} ML_UnmapEvent_Data;

typedef struct
{
X_Window_Object *window;
word            *overrideRedirect; /* ML bool */
} ML_MapEvent_Data;

typedef struct
{
X_Window_Object *window;
X_Window_Object *parent;
MLXPoint        *position;
word            *overrideRedirect; /* ML bool */
} ML_ReparentEvent_Data;

typedef struct
{
X_Window_Object *window;
MLXPoint        *position;
} ML_GravityEvent_Data;

typedef struct
{
X_Window_Object *window;
word            *place;
} ML_CirculateEvent_Data;

typedef struct
{
X_Colormap_Object *colormap_object;
word              *new;        /* ML bool */
word              *installed;  /* ML bool */
} ML_ColormapEvent_Data;

typedef struct
{
word *selection; /* ML int */
word *time;      /* ML int */
} ML_SelectionClear_Data;

typedef struct
{
X_Window_Object *requestor;
word            *selection; /* ML int */
word            *target;    /* ML int */
word            *property;  /* ML int */
word            *time;      /* ML int */
} ML_SelectionRequest_Data;


typedef struct
{
word *selection; /* ML int */
word *target;    /* ML int */
word *property;  /* ML int */
word *time;      /* ML int */
} ML_Selection_Data;


typedef struct
{
word            *type;       /* ML (?) */
word            *sendEvent;  /* ML bool */
X_Window_Object *window;
word            *data;       /* pointer to event-specific data, in ML_XXX_Data format */
ML_Cons_Cell    *callbacks;  /* ML list of something */
ML_Cons_Cell	*events;     /* ML list */
} ML_Event;


/******************************************************************************/
/*                                                                            */
/*      CreateEvent function                                                  */
/*                                                                            */
/******************************************************************************/

static Handle CreateEvent
(
  Handle  dsHandle, /* Handle to (X_Display_Object *) */
  XEvent *ev,
  Handle  W         /* Handle to (X_Window_Object *) */
)
{
  Handle eventHandle = alloc_and_save(MUTABLE(SIZEOF(ML_Event)));

  Display *d     = DEREFDISPLAYHANDLE(dsHandle)->display;
  int type       = ev->xany.type;
  int send_event = ev->xany.send_event;
  
  assert(d == ev->xany.display);
  
  if (A.debug & DEBUG_X)
  {
    proper_printf("CreateEvent called, type=%s,", DebugEventName(type));
    proper_printf(" window=%lx\n", ev->xany.window);
  }

#define event ((ML_Event *)DEREFHANDLE(eventHandle))
  event->type      = DEREFWORDHANDLE(Make_unsigned(type));
  event->sendEvent = DEREFWORDHANDLE(Make_bool(send_event));
  event->window    = DEREFWINDOWHANDLE(W);

  switch(type)
  {
    case KeyPress:
    case KeyRelease:
    {
      Handle dataHandle = alloc_and_save(MUTABLE(SIZEOF(ML_KeyEvent_Data)));

#define data ((ML_KeyEvent_Data *)DEREFHANDLE(dataHandle))
      data->root        = DEREFWINDOWHANDLE(EmptyWindow(dsHandle,ev->xkey.root));
      data->subwindow   = DEREFWINDOWHANDLE(EmptyWindow(dsHandle,ev->xkey.subwindow));
      data->time        = (word *)           DEREFHANDLE(Make_unsigned(ev->xkey.time));
      data->pointer     = (MLXPoint *)       DEREFHANDLE(CreatePoint(ev->xkey.x,ev->xkey.y));
      data->rootPointer = (MLXPoint *)       DEREFHANDLE(CreatePoint(ev->xkey.x_root,ev->xkey.y_root));
      data->modifiers   = (word *)           DEREFHANDLE(Make_unsigned(ev->xkey.state));
      data->keycode     = (word *)           DEREFHANDLE(Make_unsigned(ev->xkey.keycode));
#undef data

      event->data = DEREFWORDHANDLE(FINISHED(dataHandle));
      
      break;
    }

      
    case ButtonPress:
    case ButtonRelease:
    {
      
      if (*(DEREFWINDOWHANDLE(W))->eventMask &ButtonClickMask)
      {
        Handle dataHandle;
        PredicateArgs A;

        A.button = &ev->xbutton;
        A.up     = (ev->type == ButtonRelease);
        A.down   = (ev->type == ButtonPress);
      
        WaitDoubleClickTime(dsHandle,&A);

        dataHandle = alloc_and_save(MUTABLE(SIZEOF(ML_ButtonClick_Data)));
 
#define data ((ML_ButtonClick_Data *)DEREFHANDLE(dataHandle))
        data->root        = DEREFWINDOWHANDLE(EmptyWindow(dsHandle,ev->xbutton.root));
        data->subwindow   = DEREFWINDOWHANDLE(EmptyWindow(dsHandle,ev->xbutton.subwindow));
        data->time        = (word *)           DEREFHANDLE(Make_unsigned(ev->xbutton.time));
        data->pointer     = (MLXPoint *)       DEREFHANDLE(CreatePoint(ev->xbutton.x,ev->xbutton.y));
        data->rootPointer = (MLXPoint *)       DEREFHANDLE(CreatePoint(ev->xbutton.x_root,ev->xbutton.y_root));
        data->modifiers   = (word *)           DEREFHANDLE(Make_unsigned(ev->xbutton.state));
        data->button      = (word *)           DEREFHANDLE(Make_unsigned(ev->xbutton.button));
        data->up          = (word *)           DEREFHANDLE(Make_unsigned(A.up));
        data->down        = (word *)           DEREFHANDLE(Make_unsigned(A.down));
#undef data
  
        event->type = DEREFWORDHANDLE(Make_unsigned(42)); /* What's this for? */
        event->data = DEREFWORDHANDLE(FINISHED(dataHandle));

      }
      else
      {
        Handle dataHandle = alloc_and_save(MUTABLE(SIZEOF(ML_ButtonEvent_Data)));
  
#define data ((ML_ButtonEvent_Data *)DEREFHANDLE(dataHandle))
        data->root        = DEREFWINDOWHANDLE(EmptyWindow(dsHandle,ev->xbutton.root));
        data->subwindow   = DEREFWINDOWHANDLE(EmptyWindow(dsHandle,ev->xbutton.subwindow));
        data->time        = (word *)           DEREFHANDLE(Make_unsigned(ev->xbutton.time));
        data->pointer     = (MLXPoint *)       DEREFHANDLE(CreatePoint(ev->xbutton.x,ev->xbutton.y));
        data->rootPointer = (MLXPoint *)       DEREFHANDLE(CreatePoint(ev->xbutton.x_root,ev->xbutton.y_root));
        data->modifiers   = (word *)           DEREFHANDLE(Make_unsigned(ev->xbutton.state));
        data->button      = (word *)           DEREFHANDLE(Make_unsigned(ev->xbutton.button));
#undef data
  
        event->data = DEREFWORDHANDLE(FINISHED(dataHandle));
      
      }
      
      break;
    }


    case MotionNotify:
    {

      Handle dataHandle = alloc_and_save(MUTABLE(SIZEOF(ML_MotionEvent_Data)));

#define data ((ML_MotionEvent_Data *)DEREFHANDLE(dataHandle))
      data->root        = DEREFWINDOWHANDLE(EmptyWindow(dsHandle,ev->xmotion.root));
      data->subwindow   = DEREFWINDOWHANDLE(EmptyWindow(dsHandle,ev->xmotion.subwindow));
      data->time        = (word *)           DEREFHANDLE(Make_unsigned(ev->xmotion.time));
      data->pointer     = (MLXPoint *)       DEREFHANDLE(CreatePoint(ev->xmotion.x,ev->xmotion.y));
      data->rootPointer = (MLXPoint *)       DEREFHANDLE(CreatePoint(ev->xmotion.x_root,ev->xmotion.y_root));
      data->modifiers   = (word *)           DEREFHANDLE(Make_unsigned(ev->xmotion.state));
      data->isHint      = (word *)           DEREFHANDLE(Make_unsigned(ev->xmotion.is_hint));
#undef data

      event->data = DEREFWORDHANDLE(FINISHED(dataHandle));
    
      
      break;
    }

    case EnterNotify:
    case LeaveNotify:
    {
      Handle dataHandle = alloc_and_save(MUTABLE(SIZEOF(ML_CrossingEvent_Data)));

#define data ((ML_CrossingEvent_Data *)DEREFHANDLE(dataHandle))
      data->root        = DEREFWINDOWHANDLE(EmptyWindow(dsHandle,ev->xcrossing.root));
      data->subwindow   = DEREFWINDOWHANDLE(EmptyWindow(dsHandle,ev->xcrossing.subwindow));
      data->time        = (word *)           DEREFHANDLE(Make_unsigned(ev->xcrossing.time));
      data->pointer     = (MLXPoint *)       DEREFHANDLE(CreatePoint(ev->xcrossing.x,ev->xcrossing.y));
      data->rootPointer = (MLXPoint *)       DEREFHANDLE(CreatePoint(ev->xcrossing.x_root,ev->xcrossing.y_root));
      data->mode        = (word *)           DEREFHANDLE(Make_unsigned(ev->xcrossing.mode));
      data->detail      = (word *)           DEREFHANDLE(Make_unsigned(ev->xcrossing.detail));
      data->focus       = (word *)           DEREFHANDLE(Make_bool(ev->xcrossing.focus));
      data->modifiers   = (word *)           DEREFHANDLE(Make_unsigned(ev->xcrossing.state));
#undef data

      event->data = DEREFWORDHANDLE(FINISHED(dataHandle));
    
      break;
    }

    case Expose:
    {
      int left   = ev->xexpose.x;
      int top    = ev->xexpose.y;
      int right  = left + ev->xexpose.width;
      int bottom = top  + ev->xexpose.height;

      Handle dataHandle;

      while(XCheckTypedWindowEvent(d,ev->xexpose.window,Expose,ev))
      {
        int L = ev->xexpose.x;
        int T = ev->xexpose.y;
        int R = L + ev->xexpose.width;
        int B = T + ev->xexpose.height;
        
        assert(ev->type == Expose);

        left   = min(left,L);
        top    = min(top,T);
        right  = max(right,R);
        bottom = max(bottom,B);
      }
      
      dataHandle = alloc_and_save(MUTABLE(SIZEOF(ML_ExposeEvent_Data)));

#define data ((ML_ExposeEvent_Data *)DEREFHANDLE(dataHandle))
      data->region = (MLXRectangle *)DEREFHANDLE(CreateRect(top,left,bottom,right));
      data->count  = (word *)        DEREFHANDLE(Make_unsigned(0));
#undef data

      event->data = DEREFWORDHANDLE(FINISHED(dataHandle));
      
      break;
    }


    case GraphicsExpose:
    {
      int left   = ev->xgraphicsexpose.x;
      int top    = ev->xgraphicsexpose.y;
      int right  = left + ev->xgraphicsexpose.width;
      int bottom = top  + ev->xgraphicsexpose.height;

      Handle dataHandle = alloc_and_save(MUTABLE(SIZEOF(ML_GraphicsExposeEvent_Data)));

#define data ((ML_GraphicsExposeEvent_Data *)DEREFHANDLE(dataHandle))
      data->region = (MLXRectangle *)DEREFHANDLE(CreateRect(top,left,bottom,right));
      data->count  = (word *)        DEREFHANDLE(Make_unsigned(ev->xgraphicsexpose.count));
      data->code   = (word *)        DEREFHANDLE(Make_unsigned(ev->xgraphicsexpose.major_code));
#undef data

      event->data = DEREFWORDHANDLE(FINISHED(dataHandle));
      
      break;
    }

    case NoExpose:
    {
      event->data = DEREFWORDHANDLE(Make_unsigned(ev->xnoexpose.major_code));
      
      break;
    }

    case ConfigureNotify:
    {
      Handle dataHandle = alloc_and_save(MUTABLE(SIZEOF(ML_ConfigureNotify_Data)));

#define data ((ML_ConfigureNotify_Data *)DEREFHANDLE(dataHandle))
      data->window           = DEREFWINDOWHANDLE(EmptyWindow(dsHandle,ev->xconfigure.window));
      data->position         = (MLXPoint *)       DEREFHANDLE(CreatePoint(ev->xconfigure.x,ev->xconfigure.y));
      data->size             = (MLXRectangle *)   DEREFHANDLE(CreateArea(ev->xconfigure.width,ev->xconfigure.height));
      data->borderWidth      = (word *)           DEREFHANDLE(Make_int(ev->xconfigure.border_width));
      data->above            = DEREFWINDOWHANDLE(EmptyWindow(dsHandle,ev->xconfigure.above));
      data->overrideRedirect = (word *)           DEREFHANDLE(Make_bool(ev->xconfigure.override_redirect));
#undef data

      event->data = DEREFWORDHANDLE(FINISHED(dataHandle));
      
      break;
    }



    case FocusIn:
    case FocusOut:
    {
      Handle dataHandle = alloc_and_save(MUTABLE(SIZEOF(ML_FocusChangeEvent_Data)));

#define data ((ML_FocusChangeEvent_Data *)DEREFHANDLE(dataHandle))
      data->mode   = DEREFWORDHANDLE(Make_int(ev->xfocus.mode));
      data->detail = DEREFWORDHANDLE(Make_int(ev->xfocus.detail));
#undef data

      event->data = DEREFWORDHANDLE(FINISHED(dataHandle));

      break;
    }

    case VisibilityNotify:
    {
      event->data = DEREFWORDHANDLE(Make_int(ev->xvisibility.state));
      
      break;
    }


    case CreateNotify:
    {
      Handle dataHandle = alloc_and_save(MUTABLE(SIZEOF(ML_CreateEvent_Data)));

#define data ((ML_CreateEvent_Data *)DEREFHANDLE(dataHandle))
      data->window           = DEREFWINDOWHANDLE(EmptyWindow(dsHandle,ev->xcreatewindow.window));
      data->position         = (MLXPoint *)       DEREFHANDLE(CreatePoint(ev->xcreatewindow.x,ev->xcreatewindow.y));
      data->size             = (MLXRectangle *)   DEREFHANDLE(CreateArea(ev->xcreatewindow.width,ev->xcreatewindow.height));
      data->borderWidth      = (word *)           DEREFHANDLE(Make_int(ev->xcreatewindow.border_width));
      data->overrideRedirect = (word *)           DEREFHANDLE(Make_bool(ev->xcreatewindow.override_redirect));
#undef data

      event->data = DEREFWORDHANDLE(FINISHED(dataHandle));
      
      break;
    }

    case DestroyNotify:
    {
      debugReclaim(Window,ev->xdestroywindow.window);
      event->data = DEREFWORDHANDLE(EmptyWindow(dsHandle,ev->xdestroywindow.window));
      
      break;
    }
    
    case UnmapNotify:
    {
      Handle dataHandle = alloc_and_save(MUTABLE(SIZEOF(ML_UnmapEvent_Data)));

#define data ((ML_UnmapEvent_Data *)DEREFHANDLE(dataHandle))
      data->window        = DEREFWINDOWHANDLE(EmptyWindow(dsHandle,ev->xunmap.window));
      data->fromConfigure = (word *)           DEREFHANDLE(Make_bool(ev->xunmap.from_configure));
#undef data

      event->data = DEREFWORDHANDLE(FINISHED(dataHandle));

      break;
    }
    
    case MapNotify:
    {
      Handle dataHandle = alloc_and_save(MUTABLE(SIZEOF(ML_MapEvent_Data)));

#define data ((ML_MapEvent_Data *)DEREFHANDLE(dataHandle))
      data->window           = DEREFWINDOWHANDLE(EmptyWindow(dsHandle,ev->xmap.window));
      data->overrideRedirect = (word *)           DEREFHANDLE(Make_bool(ev->xmap.override_redirect));
#undef data

      event->data = DEREFWORDHANDLE(FINISHED(dataHandle));
      
      break;
    }
    
    case MapRequest:
    {
      event->data = DEREFWORDHANDLE(EmptyWindow(dsHandle,ev->xmaprequest.window));
      
      break;
    }


    case ReparentNotify:
    {
      Handle dataHandle = alloc_and_save(MUTABLE(SIZEOF(ML_ReparentEvent_Data)));

#define data ((ML_ReparentEvent_Data *)DEREFHANDLE(dataHandle))
      data->window           = DEREFWINDOWHANDLE(EmptyWindow(dsHandle,ev->xreparent.window));
      data->parent           = DEREFWINDOWHANDLE(EmptyWindow(dsHandle,ev->xreparent.parent));
      data->position         = (MLXPoint *)       DEREFHANDLE(CreatePoint(ev->xreparent.x,ev->xreparent.y));
      data->overrideRedirect = (word *)           DEREFHANDLE(Make_bool(ev->xreparent.override_redirect));
#undef data

      event->data = DEREFWORDHANDLE(FINISHED(dataHandle));
      
      break;
    }


    case ConfigureRequest:
    {
      Handle dataHandle = alloc_and_save(MUTABLE(SIZEOF(ML_ConfigureRequest_Data)));

#define data ((ML_ConfigureRequest_Data *)DEREFHANDLE(dataHandle))
      data->window      = DEREFWINDOWHANDLE(EmptyWindow(dsHandle,ev->xconfigurerequest.window));
      data->position    = (MLXPoint *)       DEREFHANDLE(CreatePoint(ev->xconfigurerequest.x,ev->xconfigurerequest.y));
      data->size        = (MLXRectangle *)   DEREFHANDLE(CreateArea(ev->xconfigurerequest.width,ev->xconfigurerequest.height));
      data->borderWidth = (word *)           DEREFHANDLE(Make_int(ev->xconfigurerequest.border_width));
      data->above       = DEREFWINDOWHANDLE(EmptyWindow(dsHandle,ev->xconfigurerequest.above));
      data->detail      = (word *)           DEREFHANDLE(Make_int(ev->xconfigurerequest.detail));
#undef data

      event->data = DEREFWORDHANDLE(FINISHED(dataHandle));

      break;
    }

    case GravityNotify:
    {
      Handle dataHandle = alloc_and_save(MUTABLE(SIZEOF(ML_GravityEvent_Data)));

#define data ((ML_GravityEvent_Data *)DEREFHANDLE(dataHandle))
      data->window   = DEREFWINDOWHANDLE(EmptyWindow(dsHandle,ev->xgravity.window));
      data->position = (MLXPoint *)       DEREFHANDLE(CreatePoint(ev->xgravity.x,ev->xgravity.y));
#undef data

      event->data = DEREFWORDHANDLE(FINISHED(dataHandle));

      break;
    }

    case ResizeRequest:
    {
      event->data = DEREFWORDHANDLE(CreateArea(ev->xresizerequest.width,ev->xresizerequest.height));
      
      break;
    }


    case CirculateNotify:
    {
      Handle dataHandle = alloc_and_save(MUTABLE(SIZEOF(ML_CirculateEvent_Data)));

#define data ((ML_CirculateEvent_Data *)DEREFHANDLE(dataHandle))
      data->window = DEREFWINDOWHANDLE(EmptyWindow(dsHandle,ev->xcirculate.window));
      data->place  = (word *)           DEREFHANDLE(Make_int(ev->xcirculate.place));
#undef data

      event->data = DEREFWORDHANDLE(FINISHED(dataHandle));
      
      break;
    }

    case CirculateRequest:
    {
      Handle dataHandle = alloc_and_save(MUTABLE(SIZEOF(ML_CirculateEvent_Data)));

#define data ((ML_CirculateEvent_Data *)DEREFHANDLE(dataHandle))
      data->window = DEREFWINDOWHANDLE(EmptyWindow(dsHandle,ev->xcirculaterequest.window));
      data->place  = (word *)           DEREFHANDLE(Make_int(ev->xcirculaterequest.place));
#undef data

      event->data = DEREFWORDHANDLE(FINISHED(dataHandle));

      break;
    }

    case ColormapNotify:
    {
      Handle dataHandle = alloc_and_save(MUTABLE(SIZEOF(ML_ColormapEvent_Data)));

#define data ((ML_ColormapEvent_Data *)DEREFHANDLE(dataHandle))
      data->colormap_object = (X_Colormap_Object *)DEREFHANDLE(EmptyColormap(dsHandle,ev->xcolormap.colormap));
      data->new             = (word *)             DEREFHANDLE(Make_bool(ev->xcolormap.new));
      data->installed       = (word *)             DEREFHANDLE(Make_bool(ev->xcolormap.state == ColormapInstalled));
#undef data

      event->data = DEREFWORDHANDLE(FINISHED(dataHandle));
      
      break;
    }
    
    case MappingNotify:
    {
      XRefreshKeyboardMapping((XMappingEvent *)ev); /* cast added SPF 6/1/94 */
      return 0; /* HACK !!!! */
    }

    case SelectionClear:
    {
      Handle dataHandle = alloc_and_save(MUTABLE(SIZEOF(ML_SelectionClear_Data)));

#define data ((ML_SelectionClear_Data *)DEREFHANDLE(dataHandle))
      data->selection = DEREFWORDHANDLE(Make_unsigned(ev->xselectionclear.selection));
      data->time      = DEREFWORDHANDLE(Make_unsigned(ev->xselectionclear.time));
#undef data

      event->data = DEREFWORDHANDLE(FINISHED(dataHandle));

      break;
    }
    
    case SelectionNotify:
    {
      Handle dataHandle = alloc_and_save(MUTABLE(SIZEOF(ML_Selection_Data)));

#define data ((ML_Selection_Data *)DEREFHANDLE(dataHandle))
      data->selection = DEREFWORDHANDLE(Make_unsigned(ev->xselection.selection));
      data->target    = DEREFWORDHANDLE(Make_unsigned(ev->xselection.target));
      data->property  = DEREFWORDHANDLE(Make_unsigned(ev->xselection.property));
      data->time      = DEREFWORDHANDLE(Make_unsigned(ev->xselection.time));
#undef data

      event->data = DEREFWORDHANDLE(FINISHED(dataHandle));
      
      break;
    }
    
    case SelectionRequest:
    {
      Handle dataHandle = alloc_and_save(MUTABLE(SIZEOF(ML_SelectionRequest_Data)));

#define data ((ML_SelectionRequest_Data *)DEREFHANDLE(dataHandle))
      data->requestor = DEREFWINDOWHANDLE(EmptyWindow(dsHandle,ev->xselectionrequest.requestor));
      data->selection = DEREFWORDHANDLE(Make_unsigned(ev->xselectionrequest.selection));
      data->target    = DEREFWORDHANDLE(Make_unsigned(ev->xselectionrequest.target));
      data->property  = DEREFWORDHANDLE(Make_unsigned(ev->xselectionrequest.property));
      data->time      = DEREFWORDHANDLE(Make_unsigned(ev->xselectionrequest.time));
#undef data

      event->data = DEREFWORDHANDLE(FINISHED(dataHandle));
      
      break;
    }

    case ClientMessage:
    {
      int protocols    = WM_PROTOCOLS(d);
      int deleteWindow = WM_DELETE_WINDOW(d);
      
      if (protocols                != None      && 
           deleteWindow             != None      && 
           ev->xclient.message_type == protocols && 
           ev->xclient.format       == 32        && 
           ev->xclient.data.l[0]    == deleteWindow)
      {
        event->type = DEREFWORDHANDLE(Make_unsigned(43)); /* (?) */
      
        break;
      }
      else return 0;
    }

    case PropertyNotify: return 0;
    
    case KeymapNotify: return 0;   /* Broken: the window field does not tell me the window requesting this event */

    default: Crash ("Bad event type %x",ev->type);
  }
  
  event->callbacks = MLLIST(FList); /* Safe, since FList is a Root */
  FList = 0;

  event->events = MLLIST(GList); /* Safe, since GList is a Root */
  GList = 0;

  return FINISHED(eventHandle);
#undef event
}

/******************************************************************************/
/*                                                                            */
/*      HERE                                                                  */
/*                                                                            */
/******************************************************************************/
static Handle LookupString(Display *d, unsigned keycode, unsigned modifiers)
{
  XKeyEvent ev;
  int n;
  KeySym keysym; /* was int SPF 6/1/94 */
  char buffer[500];
  
  ev.display = d;
  ev.keycode = keycode;
  ev.state   = modifiers;
  
  n = XLookupString(&ev,buffer,sizeof(buffer)-1,&keysym,NULL);
  
  buffer[n] = '\0';
  
  return CreatePair(Make_string(buffer),Make_unsigned(keysym));
}

static Handle GetScreenSaver(Display *d)
{
  int timeout,interval,blanking,exposures;
  Handle tuple;
  
  XGetScreenSaver(d,&timeout,&interval,&blanking,&exposures);
  
  tuple = alloc_and_save(MUTABLE(4));

#define data DEREFHANDLE(tuple)
  data[0] = DEREFWORDHANDLE(Make_int(timeout));
  data[1] = DEREFWORDHANDLE(Make_int(interval));
  data[2] = DEREFWORDHANDLE(Make_unsigned(blanking));
  data[3] = DEREFWORDHANDLE(Make_unsigned(exposures));
#undef data
  
  return FINISHED(tuple);
}

static Handle TranslateCoordinates
(
  Handle dsHandle, /* Handle to (X_Display_Object *) */
  Window src,
  Window dst,
  int    x,
  int    y
)
{
  Window child;
  int    dx,dy,s;
  
  s = XTranslateCoordinates(DEREFDISPLAYHANDLE(dsHandle)->display,src,dst,x,y,&dx,&dy,&child);
  
  if (s == 0) RaiseXWindows ("XTranslateCoordinates failed");
  
  return CreatePair(CreatePoint(dx,dy),EmptyWindow(dsHandle,child));
}
  

static Handle QueryBest
(
  int    (*f)(),
  Display *d,
  Drawable drawable,
  unsigned width,
  unsigned height
)
{
  int W,H;
  
  int s = (* f)(d,drawable,width,height,&W,&H);
  
  if (s == 0) RaiseXWindows ("XQueryBest failed");
  
  return CreateArea(W,H);
}

static Handle QueryPointer
(
  Handle dsHandle, /* Handle to (X_Display_Object *) */
  Window  w
)
{
  Window   root,child;
  int      rootX,rootY;
  int      winX,winY;
  unsigned mask;
  int      s;
  Handle tuple;
  
  s = XQueryPointer(DEREFDISPLAYHANDLE(dsHandle)->display,w,&root,&child,&rootX,&rootY,&winX,&winY,&mask);
  
  tuple = alloc_and_save(MUTABLE(6));

#define data DEREFHANDLE(tuple)  
  data[0] = DEREFWORDHANDLE(Make_unsigned(s));
  data[1] = DEREFWORDHANDLE(EmptyWindow(dsHandle,root));
  data[2] = DEREFWORDHANDLE(EmptyWindow(dsHandle,child));
  data[3] = DEREFWORDHANDLE(CreatePoint(rootX,rootY));
  data[4] = DEREFWORDHANDLE(CreatePoint(winX,winY));
  data[5] = DEREFWORDHANDLE(Make_unsigned(mask));
#undef data
  
  return FINISHED(tuple);
}

static Handle ReadBitmap
(
  Handle   dsHandle, /* handle to (X_Display_Object *) */
  Drawable w,
  String  *string
)
{
  unsigned width,height;
  char name[500];
  int s,xhot,yhot;
  Pixmap pixmap;
  Handle tuple;

  Poly_String_To_C(string,name,sizeof(name));

  s = XReadBitmapFile(DEREFDISPLAYHANDLE(dsHandle)->display,w,name,&width,&height,&pixmap,&xhot,&yhot);
  
  tuple = alloc_and_save(MUTABLE(4));

#define data DEREFHANDLE(tuple)

  data[0] = DEREFWORDHANDLE(Make_unsigned(s));

  if (s == BitmapSuccess)
  {
    data[1] = DEREFWORDHANDLE(EmptyPixmap(dsHandle,pixmap));
    data[2] = DEREFWORDHANDLE(CreateArea(width,height));
    data[3] = DEREFWORDHANDLE(CreatePoint(xhot,yhot));
  }
  
  /******************** What if we don't succeed? Badly-formed tuple !!!! */

#undef data
  
  return FINISHED(tuple);
}

static Handle WriteBitmapFile
(
  String  *string,
  Display *d,
  Pixmap   bitmap,
  unsigned w,
  unsigned h,
  int      x,
  int      y
)
{
  char name[500]; int s;

  Poly_String_To_C(string,name,sizeof(name));

  s = XWriteBitmapFile(d,name,bitmap,w,h,x,y);
  
  return Make_unsigned(s);
}

static Handle GetDefault(Display *d, String *s1, String *s2)
{
  char program[500]; char option[500]; char *s;

  Poly_String_To_C(s1,program,sizeof(program));
  Poly_String_To_C(s2,option ,sizeof(option));

  s = XGetDefault(d,program,option);
  
  if (s == NULL) RaiseXWindows ("XGetDefault failed");
  
  return Make_string(s);
}


static void GetWindows(word *p, Window *w)
{
  *w = GetWindow((X_Object *)p);
}


static void GetSegments(word ***p, XSegment *A)
{
  A->x1 = GetPointX(p[0]);
  A->y1 = GetPointY(p[0]);
  A->x2 = GetPointX(p[1]);
  A->y2 = GetPointY(p[1]);
}

static void GetChar2(word *p, XChar2b *A)
{
  unsigned u = get_C_ushort(p);
  
  A->byte1 = u >> 8;
  A->byte2 = u &0xFF;
}

static void CopyString(word *w, char **p)
{
  String *s = GetString(w);
  word    n = s->length+1;
  
  *p = malloc(n);
  
  Poly_String_To_C(s,*p,n);
}

static void GetText(word **p, XTextItem *A)
{
  CopyString(p[0],&A->chars);
  
  A->nchars = strlen(A->chars);
  A->delta  = get_C_short(p[1]);
  A->font   = GetFont((X_Object *)p[2]);
}

static void FreeText(XTextItem *A)
{
  free(A->chars);
}

static void GetText16(word **p, XTextItem16 *A)
{
  word     N = ListLength((ML_Cons_Cell *)(p[0]));
  XChar2b *L = (XChar2b *) malloc(N * sizeof(XChar2b));
  
  GetList4(p[0],L,sizeof(XChar2b),GetChar2);
  
  A->chars  = L;
  A->nchars = N;
  A->delta  = get_C_short(p[1]);
  A->font   = GetFont((X_Object *)p[2]);
}

static void FreeText16(XTextItem16 *A)
{
  free(A->chars);
}

static void DrawList
(
  IntFunc    draw,
  VoidFunc   get,
  word       bytes,
  Display   *d,
  Drawable   drawable,
  GC         gc,
  Handle     list, /* a list of something */
  unsigned   u1,
  unsigned   u2
)
{
  if (NONNIL(DEREFHANDLE(list)))
  {
    word  N = ListLength(DEREFLISTHANDLE(list));
    char *L = (char *)alloca(N * bytes);
  
    GetList4(DEREFHANDLE(list),L,bytes,get);
    
    (* draw)(d,drawable,gc,L,N,u1,u2);
  }
}

static void DrawText
(
  IntFunc    draw,
  VoidFunc   get,
  VoidFunc   dispose,
  word       bytes,
  Display   *d,
  Drawable   drawable,
  GC         gc,
  int        x,
  int        y,
  Handle     list
)
{
  if (NONNIL(DEREFHANDLE(list)))
  {
    word  N = ListLength(DEREFLISTHANDLE(list));
    char *L = (char *)alloca(N * bytes);
  
    GetList4(DEREFHANDLE(list),L,bytes,get);
    
    (* draw)(d,drawable,gc,x,y,L,N);
    
    if (dispose)
      {
        while (N--)
          {
            (* dispose)(L);
            L += bytes;
          }
      }
       
  }
}

static void SetClipRectangles
(
  Display *d,
  GC       gc,
  int      x,
  int      y,
  Handle   list,
  unsigned order
)
{
  if (ISNIL(DEREFHANDLE(list)))
  {
    XSetClipRectangles(d,gc,x,y,NULL,0,order);
  }
  else
  {
    word        N = ListLength(DEREFLISTHANDLE(list));
    XRectangle *L = (XRectangle *) alloca(N * sizeof(XRectangle));
    
    GetList4(DEREFHANDLE(list),L,sizeof(XRectangle),GetRects);
    
    XSetClipRectangles(d,gc,x,y,L,N,order);
  }
}

static void GetUChars(word *p, uchar *u)
{
  *u = get_C_uchar(p);
}

static void SetDashes
(
  Display *d,
  GC       gc,
  unsigned offset,
  Handle   list
)
{
  if (NONNIL(DEREFHANDLE(list)))
  {
    word   N = ListLength(DEREFLISTHANDLE(list));
    char *D  = (char *) alloca(N);
  
    GetList4(DEREFHANDLE(list),D,sizeof(uchar),GetUChars);
    
    XSetDashes(d,gc,offset,D,N);
  }
}

static Handle CreateDrawable
(
  Window  *p,
  word     i,
  Handle   dsHandle /* Handle to (X_Display_Object *) */
)
{
  return EmptyWindow(dsHandle,*p);
}

static Handle QueryTree
(
  Handle   dsHandle, /* Handle to (X_Display_Object *) */
  Window   w
)
{
  Window root,parent,*children; 
  unsigned n;
  Handle data;

  int s = XQueryTree(DEREFDISPLAYHANDLE(dsHandle)->display,w,&root,&parent,&children,&n);

  if (s == 0) RaiseXWindows ("XQueryTree failed");

  data = CreateTriple(EmptyWindow(dsHandle,root),
                      EmptyWindow(dsHandle,parent),
                      CreateList5(n,children,sizeof(Window),CreateDrawable,dsHandle));

  if (n) XFree((char *)children);

  return data;
}

static void RestackWindows(Handle list /* handle to list of X_Window_Objects (?) */)
{
  if (NONNIL(DEREFHANDLE(list)))
  {
    unsigned N = ListLength(DEREFLISTHANDLE(list));
    Window  *W = (Window *) alloca(N * sizeof(Window));
    Display *d = GetDisplay((X_Object *)DEREFLISTHANDLE(list)->h);
  
    GetList4(DEREFHANDLE(list),W,sizeof(Window),GetWindows);
  
    XRestackWindows(d,W,N);
  }
}

static Handle GetGeometry
(
  Handle   dsHandle, /* Handle to (X_Display_Object *) */
  Drawable w
)
{
  int x,y;
  unsigned width,height,borderWidth,depth;
  Window root;
  Handle dataHandle;

  int s = XGetGeometry(DEREFDISPLAYHANDLE(dsHandle)->display,w,&root,&x,&y,&width,&height,&borderWidth,&depth);
  
  if (s == 0) RaiseXWindows ("XGetGeometry failed");
  
  dataHandle = alloc_and_save(MUTABLE(5));

#define data DEREFHANDLE(dataHandle) 
  data[0] = DEREFWORDHANDLE(EmptyWindow(dsHandle,root));
  data[1] = DEREFWORDHANDLE(CreatePoint(x,y));
  data[2] = DEREFWORDHANDLE(CreateArea(width,height));
  data[3] = DEREFWORDHANDLE(Make_unsigned(borderWidth));
  data[4] = DEREFWORDHANDLE(Make_unsigned(depth));
#undef data

  return FINISHED(dataHandle);
}

static Handle GetWindowAttributes
(
  Handle   dsHandle, /* Handle to (X_Display_Object *) */
  Drawable w
)
{
  XWindowAttributes wa; 
  Handle dataHandle;
  
  int s = XGetWindowAttributes(DEREFDISPLAYHANDLE(dsHandle)->display,w,&wa);
  
  if (s == 0) RaiseXWindows ("XGetWindowAttributes failed");
  
  dataHandle = alloc_and_save(MUTABLE(20));

/* HACKY - should define struct? */
#define data DEREFHANDLE(dataHandle) 
  data[ 0] = DEREFWORDHANDLE(CreatePoint(wa.x,wa.y));
  data[ 1] = DEREFWORDHANDLE(CreateArea(wa.width,wa.height));
  data[ 2] = DEREFWORDHANDLE(Make_int(wa.border_width));
  data[ 3] = DEREFWORDHANDLE(Make_unsigned(wa.depth));
  data[ 4] = DEREFWORDHANDLE(EmptyVisual(dsHandle,wa.visual));
  data[ 5] = DEREFWORDHANDLE(EmptyWindow(dsHandle,wa.root));
  data[ 6] = DEREFWORDHANDLE(Make_unsigned(wa.class));
  data[ 7] = DEREFWORDHANDLE(Make_unsigned(wa.bit_gravity));
  data[ 8] = DEREFWORDHANDLE(Make_unsigned(wa.win_gravity));
  data[ 9] = DEREFWORDHANDLE(Make_unsigned(wa.backing_store));
  data[10] = DEREFWORDHANDLE(Make_unsigned(wa.backing_planes));
  data[11] = DEREFWORDHANDLE(Make_unsigned(wa.backing_pixel));
  data[12] = DEREFWORDHANDLE(Make_bool(wa.save_under));
  data[13] = DEREFWORDHANDLE(EmptyColormap(dsHandle,wa.colormap));
  data[14] = DEREFWORDHANDLE(Make_bool(wa.map_installed));
  data[15] = DEREFWORDHANDLE(Make_unsigned(wa.map_state));
  data[16] = DEREFWORDHANDLE(Make_unsigned(wa.all_event_masks));
  data[17] = DEREFWORDHANDLE(Make_unsigned(wa.your_event_mask));
  data[18] = DEREFWORDHANDLE(Make_unsigned(wa.do_not_propagate_mask));
  data[19] = DEREFWORDHANDLE(Make_bool(wa.override_redirect));
#undef data
  
  return FINISHED(dataHandle);
}

static void ChangeWindowAttributes
(
  X_Window_Object *W,
  unsigned         n,
  word            *P
)
{
  XSetWindowAttributes a;
  
  unsigned mask = 1 << n;
  
  switch(mask)
  {
    case CWBitGravity:       a.bit_gravity           = get_C_ulong(P); break;
    case CWWinGravity:       a.win_gravity           = get_C_ulong(P); break;
    case CWBackingStore:     a.backing_store         = get_C_ulong(P); break;
    case CWBackingPlanes:    a.backing_planes        = get_C_ulong(P); break;
    case CWBackingPixel:     a.backing_pixel         = get_C_ulong(P); break;
    case CWOverrideRedirect: a.override_redirect     = get_C_ulong(P); break;
    case CWSaveUnder:        a.save_under            = get_C_ulong(P); break;
    case CWEventMask:        a.event_mask            = get_C_ulong(P); break;
    case CWDontPropagate:    a.do_not_propagate_mask = get_C_ulong(P); break;

    case CWBackPixel:    a.background_pixel = get_C_ulong(P);
                         ASSIGN(W->backgroundPixmap,0);
                         break;
                         
    case CWBackPixmap:   a.background_pixmap = GetPixmap((X_Object *)P);
                         ASSIGN(W->backgroundPixmap,PixmapObject((X_Object *)P));
                         break;
                         
    case CWBorderPixel:  a.border_pixel = get_C_ulong(P);
                         ASSIGN(W->borderPixmap,0);
                         break;
                         
    case CWBorderPixmap: a.border_pixmap = GetPixmap((X_Object *)P);
                         ASSIGN(W->borderPixmap,PixmapObject((X_Object *)P));
                         break;
                         
    case CWColormap:     a.colormap = GetColormap((X_Object *)P);
                         ASSIGN(W->colormap_object,ColormapObject((X_Object *)P));
                         break;
                         
    case CWCursor:       a.cursor = GetCursor((X_Object *)P);
                         ASSIGN(W->cursor_object,CursorObject((X_Object *)P));
                         break;

    default: Crash ("Bad window mask %u",mask);
  }
  
  XChangeWindowAttributes(GetDisplay((X_Object *)W),GetWindow((X_Object *)W),mask,&a);
}


static void ConfigureWindow
(
  Display *d,
  Window   w,
  word   **tuple /* (P,S,w,d,s,flags) */
)
{
  XWindowChanges wc;
  
  unsigned mask = get_C_ulong(tuple[5]);
  
  CheckZeroRect(tuple[1]);
  
  wc.x            = GetPointX  (tuple[0]);
  wc.y            = GetPointY  (tuple[0]);
  wc.width        = GetRectW   (tuple[1]); 
  wc.height       = GetRectH   (tuple[1]);
  wc.border_width = get_C_ulong(tuple[2]);
  wc.sibling      = GetWindow  ((X_Object *)tuple[3]);
  wc.stack_mode   = get_C_ulong(tuple[4]);
  
  XConfigureWindow(d,w,mask,&wc);
}



/* The order of these depends on the XColor datatype */

typedef struct
{
  word *red;     /* ML bool */
  word *blue;    /* ML bool */
  word *doRed;   /* ML bool */
  word *green;   /* ML int */
  word *pixel;   /* ML int */
  word *doBlue;  /* ML int */
  word *doGreen; /* ML int */
} MLXColor;      /* in Poly heap */

static void ClearXColor(XColor *x)
{
  x->red = x->green = x->blue = x->pixel = x->flags = 0;
}

static Handle CreateXColor(XColor *x)
{
  Handle XHandle = alloc_and_save(MUTABLE(SIZEOF(MLXColor)));

#define X ((MLXColor *)DEREFHANDLE(XHandle))
  X->red     = DEREFWORDHANDLE(Make_unsigned(x->red));
  X->green   = DEREFWORDHANDLE(Make_unsigned(x->green));
  X->blue    = DEREFWORDHANDLE(Make_unsigned(x->blue));
  X->pixel   = DEREFWORDHANDLE(Make_unsigned(x->pixel));
  X->doRed   = DEREFWORDHANDLE(Make_bool(x->flags &DoRed));
  X->doGreen = DEREFWORDHANDLE(Make_bool(x->flags &DoGreen));
  X->doBlue  = DEREFWORDHANDLE(Make_bool(x->flags &DoBlue));
#undef X

  return FINISHED(XHandle);
}

static XColor xcolor1 = { 0 };
static XColor xcolor2 = { 0 };

static XColor *GetXColor(MLXColor *P, XColor *x)
{
  x->red   = get_C_ushort(P->red);
  x->green = get_C_ushort(P->green);
  x->blue  = get_C_ushort(P->blue);
  x->pixel = get_C_ulong (P->pixel);
  
  x->flags = (DoRed   * get_C_ulong(P->doRed))
           | (DoGreen * get_C_ulong(P->doGreen))
           | (DoBlue  * get_C_ulong(P->doBlue));
  
  return x;
}

#define GetXColor1(P) GetXColor(P,&xcolor1)
#define GetXColor2(P) GetXColor(P,&xcolor2)

static Handle AllocColor(Display *d, Colormap cmap, XColor *x)
{
  int s = XAllocColor(d,cmap,x);

  if (s == 0) RaiseXWindows ("XAllocColor failed");
  
  return CreateXColor(x);
}

static Handle CreateUnsigned(unsigned *p)
{
  return Make_unsigned(*p);
}

static Handle CreateUnsignedLong(unsigned long *p)
{
  return Make_unsigned(*p);
}

static Handle AllocColorCells
(
  Display *d,
  Colormap cmap,
  unsigned contig,
  unsigned nplanes,
  unsigned ncolors
)
{
  unsigned long *masks;  /* was unsigned SPF 6/1/94 */
  unsigned long *pixels; /* was unsigned SPF 6/1/94 */
  int s;
  
  if (ncolors < 1) RaiseRange();
  
  masks  = (unsigned long *) alloca(nplanes * sizeof(unsigned long));
  pixels = (unsigned long *) alloca(ncolors * sizeof(unsigned long));

  s = XAllocColorCells(d,cmap,contig,masks,nplanes,pixels,ncolors);

  if (s == 0) RaiseXWindows ("XAllocColorCells failed");
  
  return CreatePair(CreateList4(nplanes,masks ,sizeof(unsigned long),CreateUnsignedLong),
                    CreateList4(ncolors,pixels,sizeof(unsigned long),CreateUnsignedLong));
}

static Handle AllocColorPlanes
(
  Display *d,
  Colormap cmap,
  unsigned contig,
  unsigned ncolors,
  unsigned nreds,
  unsigned ngreens,
  unsigned nblues
)
{
  unsigned long rmask;   /* was unsigned SPF 6/1/94 */ 
  unsigned long gmask;   /* was unsigned SPF 6/1/94 */
  unsigned long bmask;   /* was unsigned SPF 6/1/94 */
  unsigned long *pixels; /* was unsigned SPF 6/1/94 */
  Handle tuple;
  int s;
  
  if (ncolors < 1) RaiseRange();
  
  pixels = (unsigned long *) alloca(ncolors * sizeof(unsigned long));

  s = XAllocColorPlanes(d,cmap,contig,pixels,ncolors,nreds,ngreens,nblues,&rmask,&gmask,&bmask);

  if (s == 0) RaiseXWindows ("XAllocColorPlanes failed");
  
  tuple = alloc_and_save(MUTABLE(4));

#define data DEREFHANDLE(tuple)
  data[0] = DEREFWORDHANDLE(CreateList4(ncolors,pixels,sizeof(unsigned long),CreateUnsignedLong));
  data[1] = DEREFWORDHANDLE(Make_unsigned(rmask));
  data[2] = DEREFWORDHANDLE(Make_unsigned(gmask));
  data[3] = DEREFWORDHANDLE(Make_unsigned(bmask));
#undef data

  return FINISHED(tuple);
}

static Handle AllocNamedColor(Display *d, Colormap cmap, String *string)
{
  char   name[500];
  int    s;
  XColor hardware;
  XColor database;
  
  ClearXColor(&hardware);
  ClearXColor(&database);
  
  Poly_String_To_C(string,name,sizeof(name));

  s = XAllocNamedColor(d,cmap,name,&hardware,&database);

  if (s == 0) RaiseXWindows ("XAllocNamedColor failed");
  
  return CreatePair(CreateXColor(&hardware),CreateXColor(&database));
}

static Handle LookupColor(Display *d, Colormap cmap, String *string)
{
  char   name[500];
  int    s;
  XColor hardware;
  XColor database;
  
  ClearXColor(&hardware);
  ClearXColor(&database);
  
  Poly_String_To_C(string,name,sizeof(name));

  s = XLookupColor(d,cmap,name,&database,&hardware);

  if (s == 0) RaiseXWindows ("XLookupColor failed");
  
  return CreatePair(CreateXColor(&database),CreateXColor(&hardware));
}

static Handle ParseColor(Display *d, Colormap cmap, String *string)
{
  char   name[500];
  int    s;
  XColor x;
  
  ClearXColor(&x);
  
  Poly_String_To_C(string,name,sizeof(name));

  s = XParseColor(d,cmap,name,&x);

  if (s == 0) RaiseXWindows ("XParseColor failed");
  
  return CreateXColor(&x);
}

static Handle QueryColor(Display *d, Colormap cmap, unsigned pixel)
{
  XColor x;
  
  ClearXColor(&x);

  x.pixel = pixel;
  
  XQueryColor(d,cmap,&x);

  return CreateXColor(&x);
}

static void GetXPixel(word *p, XColor *X)
{
  ClearXColor(X);
  
  X->pixel = get_C_ulong(p);
}

static Handle QueryColors(Display *d, Colormap cmap, Handle list)
{
  unsigned N = ListLength(DEREFLISTHANDLE(list));
  XColor  *P = (XColor *) alloca(N * sizeof(XColor));
  
  GetList4(DEREFHANDLE(list),P,sizeof(XColor),GetXPixel);
  
  XQueryColors(d,cmap,P,N);

  return CreateList4(N,P,sizeof(XColor),CreateXColor);
}

static void StoreNamedColor
(
  Display *d,
  Colormap cmap,
  String  *string,
  unsigned pixel,
  unsigned doRed,
  unsigned doGreen,
  unsigned doBlue
)
{
  unsigned flags = (DoRed * doRed) | (DoGreen * doGreen) | (DoBlue * doBlue);

  char name[500];
  
  Poly_String_To_C(string,name,sizeof(name));

  XStoreNamedColor(d,cmap,name,pixel,flags);
}

static void StoreColors(Display *d, Colormap cmap, Handle list)
{
  unsigned N = ListLength(DEREFLISTHANDLE(list));
  XColor  *P = (XColor *) alloca(N * sizeof(XColor));
  
  GetList4(DEREFHANDLE(list),P,sizeof(XColor),(VoidFunc)(GetXColor));
  
  XStoreColors(d,cmap,P,N);
}

static void GetUnsigned(word *p, unsigned *u)
{
  *u = get_C_ulong(p);
}

static void GetUnsignedLong(word *p, unsigned long *u)
{
  *u = get_C_ulong(p);
}


static void FreeColors
(
  Display *d,
  Colormap cmap,
  Handle   list,
  unsigned planes
)
{
  unsigned  N = ListLength(DEREFLISTHANDLE(list));
  unsigned long *P = (unsigned long *) alloca(N * sizeof(unsigned long));
  
  GetList4(DEREFHANDLE(list),P,sizeof(unsigned long),GetUnsignedLong);
  
  XFreeColors(d,cmap,P,N,planes);
}

static Handle CreateColormap
(
  Colormap *p,
  word      i,
  Handle   dsHandle /* handle to (X_Display_Object *) */
)
{
  return EmptyColormap(dsHandle,*p);
}

static Handle ListInstalledColormaps
(
  Handle   dsHandle, /* handle to (X_Display_Object *) */
  Drawable drawable
)
{
  int  count;
  Colormap *cmaps;
  Handle list;
  
  cmaps = XListInstalledColormaps(DEREFDISPLAYHANDLE(dsHandle)->display,drawable,&count);
  
  if (cmaps == 0) RaiseXWindows ("XListInstalledColormaps failed");
  
  list = CreateList5(count,cmaps,sizeof(Colormap),CreateColormap,dsHandle);
  
  XFree((char *)cmaps);
  
  return list;
}


static Handle GetTimeOfDay(void)
{
  TimeVal now;
  
  GETTIMEOFDAY(&now);
  
  return CreatePair(Make_unsigned(now.tv_sec),Make_unsigned(now.tv_usec));
}

static Handle GetState(X_Window_Object *P)
{
  assert(P->type == X_Window);

  CheckExists((X_Object *)P,window);

  if (ISNIL(P->handler)) Crash ("No handler set");

  return CreatePair(SAVE(P->handler),SAVE(P->state));
}

static void SetState(X_Window_Object *W, word *handler, word *state)
{
  if (! ResourceExists((X_Object *)W)) return;

  assert(W->type == X_Window);

  if (NONNIL(handler))
  {
    /* we are setting the handler and initial state    */
    /* so we need to remove all pending messages for   */
    /* this window since they will have the wrong type */
    
    PurgePendingWindowMessages(W);
  
    ASSIGN(W->handler,handler);
    ASSIGN(W->state  ,state);

  }
  else ASSIGN(W->state,state);   /* just update state */
}

/* Check if the first timer event has already expired. */
static void CheckTimerQueue(void)
{
  if (TList)
  {
    TimeVal now;
    GETTIMEOFDAY(&now);
    TList->expired = TimeLeq(&TList->timeout,&now);
  }
}

static void InsertTimeout
(
  X_Window_Object *window_object,
  unsigned         ms,
  word            *alpha,
  word            *handler
)
{
  T_List **tail;
  T_List *new;
  TimeVal now;
  
  assert(window_object->type == X_Window);
  CheckExists((X_Object *)window_object,window);
  
  if (ISNIL(window_object->handler)) Crash ("No handler set");
  
  if (window_object->handler != handler) RaiseXWindows ("Handler mismatch");
  
  { /* find insertion point in list */
    TimeVal dt;
    
    GETTIMEOFDAY(&now);
    dt.tv_sec  = ms / 1000;
    dt.tv_usec = 1000 * (ms % 1000);
    
    new = (T_List *) malloc(sizeof(T_List));
    TimeAdd(&now,&dt,&new->timeout);
    
    /* We use TimeLt here, not TimeLeq, because we
       want to add new messages AFTER existing ones.
       SPF 21/3/97
    */
    for(tail = &TList; *tail; tail = &(*tail)->next)
    {
      if (TimeLt(&new->timeout,&(*tail)->timeout)) break;
    }
  }
  
  new->next          = *tail;
  new->window_object = window_object;
  new->widget_object = (X_Widget_Object *)0;
  new->alpha         = alpha;
  new->handler       = handler;
  new->expired       = 0;

  *tail = new;
}

/* called when a widget is destroyed by Xt/Motif */
static void DestroyWidgetCallback
(
  Widget    widget,
  XtPointer client_data,
  XtPointer call_data
)
{
  /* find the ML widget (if any) associated with the C widget */
  X_Widget_Object *widget_object = FindWidget(widget);
  
  if (widget_object != NULL)
    {
      /* Destroy the ML widget representations */
      DestroyXObject((X_Object *)widget_object);
      /* Assume we can't get a C callback from a destroyed widget */
      PurgeCCallbacks(widget_object,widget);
    }

  debugReclaim(Widget,widget);
}

#if 0
#define CheckRealized(Widget,Where)\
{ \
  if (XtIsRealized(Widget) == False) \
    RaiseXWindows(#Where ": widget is not realized"); \
}

static Window WindowOfWidget(Widget widget)
{
  CheckRealized(widget,WindowOfWidget);
  return XtWindowOfObject(widget);
}
#endif

/* Now returns NULL (None) for unrealized widgets SPF 1/2/94 */
static Window WindowOfWidget(Widget widget)
{
  return XtIsRealized(widget) ? XtWindowOfObject(widget) : None;
}


static void InsertWidgetTimeout
(
  X_Widget_Object *widget_object,
  unsigned         ms,
  word            *alpha,
  word            *handler
)
{
  T_List **tail;
  T_List *new;
  TimeVal now;
  
  assert(widget_object->type == X_Widget);
  CheckExists((X_Object *)widget_object,widget);
#if NEVER
  CheckRealized(GetWidget((X_Object *)widget_object),InsertWidgetTimeout);
#endif

  /* check that handler occurs in widget's callback list */
  {
    ML_Cons_Cell *p;
    for(p = widget_object->callbackList; NONNIL(p); p = p->t)
    {
      MLPair *q = (MLPair *)p->h;
      if (SND(q) == handler) break;
    }
    if (ISNIL(p)) RaiseXWindows ("Handler mismatch");
  }
  
  
  {
    TimeVal dt;
    
    GETTIMEOFDAY(&now);
    
    dt.tv_sec = ms / 1000;
    dt.tv_usec = 1000 * (ms % 1000);
    
    new = (T_List *) malloc(sizeof(T_List));
    
    TimeAdd(&now,&dt,&new->timeout);
    
    /* We use TimeNegative here, not TimeExpired, because we
       want to add new messages AFTER existing ones.
       SPF 21/3/97
    */
    for(tail = &TList; *tail; tail = &(*tail)->next)
    {
      if (TimeLt(&new->timeout,&(*tail)->timeout)) break;
    }
  }
  
  new->next          = *tail;
  new->window_object = (X_Window_Object *)0;
  new->widget_object = widget_object;
  new->alpha         = alpha;
  new->handler       = handler;
  new->expired       = 0;

  *tail = new;
}
  
static Handle NextEvent(Handle dsHandle /* handle to (X_Display_Object *) */)
{
  for (;;)
  {
    /* Added here SPF 23/2/95 - check whether a timer event has expired */
    CheckTimerQueue();
    
    if (TList && TList->expired)
    {
      T_List *next = TList->next;
      
      EventHandle E = alloc_and_save(MUTABLE(SIZEOF(ML_Event)));
      
#define event ((ML_Event *)DEREFHANDLE(E))
      event->type       = DEREFWORDHANDLE(Make_unsigned(99));
      event->sendEvent  = DEREFWORDHANDLE(Make_bool(True));
      event->data       = TList->alpha;

      if (TList->window_object != 0)
      {
        assert(TList->widget_object == 0);
        
        event->window     = TList->window_object;
        event->callbacks  = (ML_Cons_Cell *)nil_value;
        event->events     = (ML_Cons_Cell *)nil_value;

        assert(TList->window_object->handler == TList->handler);
      }
      else /* it is a Widget message */
      {
        /* TList->widget_object etc. act like Roots */
        assert(TList->widget_object != 0);

        {
          Window w        = WindowOfWidget(GetWidget((X_Object *)TList->widget_object));
          event->window   = DEREFWINDOWHANDLE(EmptyWindow(GetDS((X_Object *)TList->widget_object),w));
        }

        { /* create callback list - allocates storage */
          Handle tailHandle    = SAVE(nil_value);
          Handle widgetHandle  = SAVE(TList->widget_object);
          Handle handlerHandle = SAVE(TList->handler);
          Handle pairHandle    = CreatePair(widgetHandle,handlerHandle);
  
          event->callbacks     = DEREFLISTHANDLE(CreatePair(pairHandle,tailHandle));
          event->events        = (ML_Cons_Cell *)nil_value;
        }
      }
#undef event
      
      free(TList);
      
      TList = next;
      
      return FINISHED(E);
    }
    else /* ! (TList && TList->expired) */
    if (DEREFDISPLAYHANDLE(dsHandle)->app_context == 0)
      /* use XNextEvent to get next event */
    {
      Display *display = DEREFDISPLAYHANDLE(dsHandle)->display;
      int      pending = XPending(display);
  
      if (pending == 0)
      {
        process_may_block(display->fd, POLY_SYS_XWindows);
      }
      else /* X Event arrived */
      {
        XEvent ev;
        X_Window_Object *W;
      
        XNextEvent(display,&ev);
        W = FindWindow(dsHandle,ev.xany.window);
        
        if (W && NONNIL(W->handler))
        {
          EventHandle E = CreateEvent(dsHandle,&ev,SAVE(W));
          if (E) return E;
        }
      }
    }
    else /* use XtAppNextEvent to get next event */
    {
      /* should use Xt to do time events as well */
      int pending = XtAppPending(DEREFDISPLAYHANDLE(dsHandle)->app_context);
  
      if (pending == 0)
      {
        process_may_block(DEREFDISPLAYHANDLE(dsHandle)->display->fd,
			POLY_SYS_XWindows);
      }
      else
      {
        if ((pending & XtIMXEvent) == 0)   /* not an X Event, probably an Xt timer event */
        {
          assert(FList == 0);
                    
          callbacks_enabled = True;
          XtAppProcessEvent(DEREFDISPLAYHANDLE(dsHandle)->app_context,pending);
          callbacks_enabled = False;

          if (FList)
          {
            EventHandle E = alloc_and_save(MUTABLE(SIZEOF(ML_Event)));

#define event ((ML_Event *)DEREFHANDLE(E))
            event->type      = DEREFWORDHANDLE(Make_unsigned(100));
            event->sendEvent = DEREFWORDHANDLE(Make_bool(True));
            event->window    = (X_Window_Object *)nil_value;
            event->data      = nil_value;
            event->callbacks = FList; /* FList != 0 */
            event->events    = GList;
#undef event
            FList = 0;
            GList = 0;
            return FINISHED(E);
          }
        }
        else /* Xt Event arrived */
        {
          XEvent ev;
          int dispatched;
        
          assert(FList == 0);  
                
          XtAppNextEvent(DEREFDISPLAYHANDLE(dsHandle)->app_context,&ev);
          
          callbacks_enabled = True;
          dispatched = XtDispatchEvent(&ev);
          callbacks_enabled = False;
          
          if (!dispatched)
          {
            X_Window_Object *W = FindWindow(dsHandle,ev.xany.window);
        
            assert(FList == 0 && GList == 0); 
                       
            if (W && NONNIL(W->handler))
            {
              EventHandle E = CreateEvent(dsHandle,&ev,SAVE(W));
              if (E) return E;
            }
          }
          else if (FList || GList)
          {
            EventHandle E = CreateEvent(dsHandle,&ev,EmptyWindow(dsHandle,ev.xany.window));
            if (E) return E;
          }
        }
      }
    }
  }
}

static Handle GetInputFocus(Handle dsHandle /* handle to (X_Display_Object *) */)
{
  Window focus;
  int revertTo;
  
  XGetInputFocus(DEREFDISPLAYHANDLE(dsHandle)->display,&focus,&revertTo);
  
  return CreatePair(EmptyWindow(dsHandle,focus),Make_unsigned(revertTo));
}

static void SetSelectionOwner
(
  Handle   dsHandle, /* handle to (X_Display_Object *) */
  unsigned selection,
  Window   owner,
  unsigned time
)
{
  Window old = XGetSelectionOwner(DEREFDISPLAYHANDLE(dsHandle)->display,selection);
  
  if (old != owner)
  {
    /* SelectionClear is only sent by the server when the ownership of a */
    /* selection passes from one client to another.  We want every ML    */
    /* window to behave like a separate client, so when the ownership of */
    /* a selection passes from one ML window to another we have to send  */
    /* the SelectionClear ourselves.                                     */
  
    X_Window_Object *W = FindWindow(dsHandle,old);
    
    if (W && NONNIL(W->handler))   /* this clients window */
    {
      XEvent event; /* was XSelectionClearEvent SPF 6/1/94 */
      
      event.xselectionclear.type       = SelectionClear;
      event.xselectionclear.serial     = 0;
      event.xselectionclear.send_event = True;
      event.xselectionclear.display    = DEREFDISPLAYHANDLE(dsHandle)->display;
      event.xselectionclear.window     = old;
      event.xselectionclear.selection  = selection;
      event.xselectionclear.time       = time;
      
      XSendEvent(DEREFDISPLAYHANDLE(dsHandle)->display,old,True,0,&event);
    }
  }
  
  XSetSelectionOwner(DEREFDISPLAYHANDLE(dsHandle)->display,selection,owner,time);
}

static void SendSelectionNotify
(
  Display *d,
  unsigned selection,
  unsigned target,
  unsigned property,
  Window   requestor,
  unsigned time
)
{
  XEvent event; /* was XSelectionEvent SPF 6/1/94 */
  
  event.xselection.type       = SelectionNotify;
  event.xselection.serial     = 0;
  event.xselection.send_event = True;
  event.xselection.display    = d;
  event.xselection.requestor  = requestor;
  event.xselection.selection  = selection;
  event.xselection.target     = target;
  event.xselection.property   = property;
  event.xselection.time       = time;
  
  XSendEvent(d,requestor,True,0,&event);
}

static Handle InternAtom
(
  Display *d,
  String  *string,
  Bool     only_if_exists
)
{
  char name[500];

  Poly_String_To_C(string,name,sizeof(name));

  return Make_unsigned(XInternAtom(d,name,only_if_exists));
}

static Handle GetAtomName(Display *d, unsigned atom)
{
  Handle s;
  
  char *name = XGetAtomName(d,atom);
  
  if (name == NULL) RaiseXWindows ("XGetAtomName failed");
  
  s = Make_string(name);
  
  XFree((char *)name);
  
  return s;
}

/* The order of these depends on the XCharStruct datatype */
typedef struct
{
word *width;      /* ML int */
word *ascent;     /* ML int */
word *descent;    /* ML int */
word *lbearing;   /* ML int */
word *rbearing;   /* ML int */
word *attributes; /* ML int */
} MLXCharStruct;

static Handle CreateCharStruct(XCharStruct *cs)
{
  Handle dataHandle = alloc_and_save(MUTABLE(SIZEOF(MLXCharStruct)));
  
#define data ((MLXCharStruct *)DEREFHANDLE(dataHandle))
  data->width      = DEREFWORDHANDLE(Make_int(cs->width));
  data->ascent     = DEREFWORDHANDLE(Make_int(cs->ascent));
  data->descent    = DEREFWORDHANDLE(Make_int(cs->descent));
  data->lbearing   = DEREFWORDHANDLE(Make_int(cs->lbearing));
  data->rbearing   = DEREFWORDHANDLE(Make_int(cs->rbearing));
  data->attributes = DEREFWORDHANDLE(Make_unsigned(cs->attributes));
#undef data
  
  return FINISHED(dataHandle);
}

/* The order of these depends on the XFontStruct datatype */
typedef struct
{
X_Font_Object  *font_object;
word           *ascent;        /* ML int */
word           *descent;       /* ML int */
word           *maxChar;       /* ML int */
word           *minChar;       /* ML int */
word           *perChar;       /* ML XCharStruct list */
word           *maxByte1;      /* ML int */
word           *minByte1;      /* ML int */
word           *direction;     /* (short ML int) FontLeftToRight | FontRightToLeft */
MLXCharStruct  *maxBounds;
MLXCharStruct  *minBounds;
word           *defaultChar;   /* ML int */
word           *allCharsExist; /* ML bool */
} MLXFontStruct;

static Handle CreateFontStruct
(
  XFontStruct *fs,
  word         i,
  Handle       dsHandle /* Handle to (X_Display_Object *) */
)
{
  Handle dataHandle = alloc_and_save(MUTABLE(SIZEOF(MLXFontStruct)));

  int n = fs->max_char_or_byte2 - fs->min_char_or_byte2 + 1;
  
  if (fs->per_char == 0) n = 0;

#define data ((MLXFontStruct *)DEREFHANDLE(dataHandle))
  data->font_object   = (X_Font_Object *)DEREFHANDLE(EmptyFont(dsHandle,fs->fid,fs));
  data->ascent        = DEREFWORDHANDLE(Make_int(fs->ascent));
  data->descent       = DEREFWORDHANDLE(Make_int(fs->descent));
  data->maxChar       = DEREFWORDHANDLE(Make_unsigned(fs->max_char_or_byte2));
  data->minChar       = DEREFWORDHANDLE(Make_unsigned(fs->min_char_or_byte2));
  data->perChar       = DEREFWORDHANDLE(CreateList4(n,fs->per_char,sizeof(XCharStruct),CreateCharStruct));
  data->maxByte1      = DEREFWORDHANDLE(Make_unsigned(fs->max_byte1));
  data->minByte1      = DEREFWORDHANDLE(Make_unsigned(fs->min_byte1));
  data->direction     = DEREFWORDHANDLE(Make_unsigned((fs->direction == FontLeftToRight) ? 1 : 2));
  data->maxBounds     = (MLXCharStruct *)DEREFHANDLE(CreateCharStruct(&fs->max_bounds));
  data->minBounds     = (MLXCharStruct *)DEREFHANDLE(CreateCharStruct(&fs->min_bounds));
  data->defaultChar   = DEREFWORDHANDLE(Make_unsigned(fs->default_char));
  data->allCharsExist = DEREFWORDHANDLE(Make_bool(fs->all_chars_exist));
#undef data
  
  return FINISHED(dataHandle);
}

static XFontStruct *GetFS(X_Font_Object *P)
{
  
  assert(P->type == X_Font);

  if (*(P->fs) == NULL) RaiseXWindows ("Not a real XFontStruct");

  CheckExists((X_Object *)P,font);

  return *(P->fs);
}

static XFontStruct *GetFontStruct(MLXFontStruct *P)
{
  return GetFS(P->font_object);
}

static Handle CreateString(char **s)
{
  return Make_string(*s);
}

static Handle GetFontPath(Display *d)
{
  Handle list;
  char **names;
  int count;

  names = XGetFontPath(d,&count);

  if (names == 0) RaiseXWindows ("XGetFontPath failed");

  list = CreateList4(count,names,sizeof(char *),CreateString);
  
  XFreeFontNames(names);
  
  return list;
}

static void FreeStrings(char **s, int n)
{
  while(n--) free(*s++);
  return;
}

static void SetFontPath(Display *d, Handle list)
{
  if (NONNIL(DEREFHANDLE(list)))
  {
    word   N = ListLength(DEREFLISTHANDLE(list));
    char **D = (char **) alloca(N * sizeof(char *));
  
    GetList4(DEREFHANDLE(list),D,sizeof(char *),CopyString);
    
    XSetFontPath(d,D,N);
    
    FreeStrings(D,N);
  }
  return;
}

static Handle ListFonts(Display *d, String *string, unsigned maxnames)
{
  char name[500];
  Handle list; 
  char **names;
  int count;

  Poly_String_To_C(string,name,sizeof(name));

  names = XListFonts(d,name,maxnames,&count);

  if (names == 0) RaiseXWindows ("XListFonts failed");

  list = CreateList4(count,names,sizeof(char *),CreateString);
  
  XFreeFontNames(names);
  
  return list;
}

static Handle ListFontsWithInfo
(
  Handle   dsHandle, /* Handle to (X_Display_Object *) */
  String  *string,
  unsigned maxnames
)
{
  char name[500];
  char **names;
  int count;
  XFontStruct *info;
  Handle pair;

  Poly_String_To_C(string,name,sizeof(name));

  names = XListFontsWithInfo(DEREFDISPLAYHANDLE(dsHandle)->display,name,maxnames,&count,&info);

  if (names == 0) RaiseXWindows ("XListFontsWithInfo failed");

  pair = CreatePair(CreateList4(count,names,sizeof(char *),CreateString),
                    CreateList5(count,info,sizeof(XFontStruct),CreateFontStruct,dsHandle));
  
  XFree((char *)info);
  XFreeFontNames(names);
  
  return pair;
}

static Handle LoadFont
(
  Handle  dsHandle, /* Handle to (X_Display_Object *) */
  String *string
)
{
  char name[500]; Font font;

  Poly_String_To_C(string,name,sizeof(name));

  font = XLoadFont(DEREFDISPLAYHANDLE(dsHandle)->display,name);

  if (font == 0) RaiseXWindows("XLoadFont failed");

  return EmptyFont(dsHandle,font,(XFontStruct *)NULL);
}

static Handle LoadQueryFont
(
  Handle  dsHandle, /* Handle to (X_Display_Object *) */
  String *string
)
{
  char name[500]; XFontStruct *fs;

  Poly_String_To_C(string,name,sizeof(name));

  fs = XLoadQueryFont(DEREFDISPLAYHANDLE(dsHandle)->display,name);

  if (fs == 0) RaiseXWindows ("XLoadQueryFont failed");
  
  return CreateFontStruct(fs,0,dsHandle);
}

static Handle QueryFont
(
  Handle dsHandle, /* Handle to (X_Display_Object *) */
  Font   font
)
{
  XFontStruct *fs;

  fs = XQueryFont(DEREFDISPLAYHANDLE(dsHandle)->display,font);

  if (fs == 0) RaiseXWindows ("XQueryFont failed");
  
  return CreateFontStruct(fs,0,dsHandle);
}

static Handle TextExtents(XFontStruct *fs, String *s)
{
  Handle dataHandle = alloc_and_save(MUTABLE(4));
  
  int direction,ascent,descent; XCharStruct overall;

  XTextExtents(fs,s->chars,s->length,&direction,&ascent,&descent,&overall);

#define data DEREFHANDLE(dataHandle)  
  data[0] = DEREFWORDHANDLE(Make_unsigned((direction == FontLeftToRight) ? 1 : 2));
  data[1] = DEREFWORDHANDLE(Make_int(ascent));
  data[2] = DEREFWORDHANDLE(Make_int(descent));
  data[3] = DEREFWORDHANDLE(CreateCharStruct(&overall));
#undef data
  
  return FINISHED(dataHandle);
}

static Handle TextExtents16(XFontStruct *fs, Handle list)
{
  Handle dataHandle = alloc_and_save(MUTABLE(4));
  
  int direction,ascent,descent; XCharStruct overall;
  
  word     N = ListLength(DEREFLISTHANDLE(list));
  XChar2b *L = (XChar2b *) alloca(N * sizeof(XChar2b));

  GetList4(DEREFHANDLE(list),L,sizeof(XChar2b),GetChar2);

  XTextExtents16(fs,L,N,&direction,&ascent,&descent,&overall);
  
#define data DEREFHANDLE(dataHandle)  
  data[0] = DEREFWORDHANDLE(Make_unsigned((direction == FontLeftToRight) ? 1 : 2));
  data[1] = DEREFWORDHANDLE(Make_int(ascent));
  data[2] = DEREFWORDHANDLE(Make_int(descent));
  data[3] = DEREFWORDHANDLE(CreateCharStruct(&overall));
#undef data
  
  return FINISHED(dataHandle);
}

static Handle TextWidth(XFontStruct *fs, String *s)
{
  if (fs->per_char == 0) return Make_int(s->length * fs->max_bounds.width);
  
  return Make_int(XTextWidth(fs,s->chars,s->length));
}

static Handle TextWidth16(XFontStruct *fs, Handle list)
{
  word     N = ListLength(DEREFLISTHANDLE(list));
  XChar2b *L = (XChar2b *) alloca(N * sizeof(XChar2b));

  GetList4(DEREFHANDLE(list),L,sizeof(XChar2b),GetChar2);

  return Make_int(XTextWidth16(fs,L,N));
}

static Handle GetTextProperty(Display *d, Window w, unsigned property)
{
  XTextProperty T;
  Handle tuple;
  
  int s = XGetTextProperty(d,w,&T,property);
  
  if (s == 0) RaiseXWindows ("XGetTextProperty failed");
  
  tuple = alloc_and_save(MUTABLE(4));

#define data DEREFHANDLE(tuple)
  data[0] = (word *)Buffer_to_Poly((char *)T.value,T.nitems * T.format / 8);
  data[1] = DEREFWORDHANDLE(Make_unsigned(T.encoding));
  data[2] = DEREFWORDHANDLE(Make_int(T.format));
  data[3] = DEREFWORDHANDLE(Make_unsigned(T.nitems));
#undef data
  
  return FINISHED(tuple);
}

static void GetXWMHints(word **P, XWMHints *H)
{
  H->input         = get_C_ulong(P[0]);
  H->initial_state = get_C_ulong(P[1]);
  H->icon_pixmap   = GetPixmap((X_Object *)P[2]);
  H->icon_window   = GetWindow((X_Object *)P[3]);
  H->icon_x        = GetPointX(P[4]);
  H->icon_y        = GetPointY(P[4]);
  H->icon_mask     = GetPixmap((X_Object *)P[5]);
  H->flags         = get_C_ulong(P[6]);
  H->window_group  = 0;
}



typedef struct
{
MLXPoint     *x0;
MLXRectangle *x1;
MLXRectangle *x2;
MLXRectangle *x3;
MLXRectangle *x4;
MLPair       *x5;  /* pair of points */
MLXRectangle *x6;
word         *x7;
word         *x8;
} MLXWMSizeHintsTuple;

static void GetXWMSizeHints(MLXWMSizeHintsTuple *P, XSizeHints *H)
{
  CheckZeroRect(P->x1);
  CheckZeroRect(P->x2);
  CheckZeroRect(P->x3);
  CheckZeroRect(P->x4);
  CheckZeroRect(P->x6);

  H->x            = GetPointX(P->x0);
  H->y            = GetPointY(P->x0);
  H->width        = GetRectW(P->x1);
  H->height       = GetRectH(P->x1);
  H->min_width    = GetRectW(P->x2);
  H->min_height   = GetRectH(P->x2);
  H->max_width    = GetRectW(P->x3);
  H->max_height   = GetRectH(P->x3);
  H->width_inc    = GetRectW(P->x4);
  H->height_inc   = GetRectH(P->x4);
  H->min_aspect.x = GetPointX(FST(P->x5));
  H->min_aspect.y = GetPointY(FST(P->x5));
  H->max_aspect.x = GetPointX(SND(P->x5));
  H->max_aspect.y = GetPointY(SND(P->x5));
  H->base_width   = GetRectW(P->x6);
  H->base_height  = GetRectH(P->x6);
  H->win_gravity  = get_C_ulong(P -> x7);
  H->flags        = get_C_ulong(P -> x8);
}

static void GetIconSize(MLTriple *P, XIconSize *s)
{
  CheckZeroRect((MLXRectangle *)FST(P));
  CheckZeroRect((MLXRectangle *)SND(P));
  CheckZeroRect((MLXRectangle *)THIRD(P));

  s->min_width = GetRectW(FST(P)); s->min_height = GetRectH(FST(P));
  s->max_width = GetRectW(SND(P)); s->max_height = GetRectH(SND(P));
  s->width_inc = GetRectW(THIRD(P)); s->height_inc = GetRectH(THIRD(P));
}

static void GetSigned(word *p,int  *i)
{
  *i = get_C_long(p);
}

static void GetPixmaps(X_Object *p, Pixmap *m)
{
  *m = GetPixmap(p);
}

static void GetColormaps(X_Object *p, Colormap *c)
{
  *c = GetColormap(p);
}

static void GetCursors(X_Object *p, Cursor *c)
{
  *c = GetCursor(p);
}

static void GetDrawables(X_Object *p, Drawable *d)
{
  *d = GetDrawable(p);
}

static void GetFonts(X_Object *p, Font *f)
{
  *f = GetFont(p);
}

static void GetVisualIds(X_Object *p, unsigned *u)
{
  *u = GetVisual(p)->visualid;
}

static void SetProperty
(
  Display *d,
  Window   w,
  unsigned property,
  unsigned target,
  Handle   list,
  unsigned encoding
)
{
  unsigned format; 
  unsigned bytes;
  uchar *value;
    
   /* SPF 7/7/94 - XA_STRING pulled out as special case; this enables */
   /* gcc to understand the previously data-dependant control flow.   */
  if (encoding == XA_STRING) 
    {
       String *s = GetString (DEREFHANDLE(list));
       
       format = 8;
       bytes  = s->length;
       value  = (uchar *) s->chars;
    }
    
  else
    {
      unsigned length = ListLength(DEREFLISTHANDLE(list));
      unsigned size;
      VoidFunc get;
     
      switch(encoding)
      {
	case XA_ATOM:          size = sizeof(unsigned);          get = GetUnsigned;         format = 32; break;
	case XA_BITMAP:        size = sizeof(Pixmap);            get = GetPixmaps;          format = 32; break;
	case XA_COLORMAP:      size = sizeof(Colormap);          get = GetColormaps;        format = 32; break;
	case XA_CURSOR:        size = sizeof(Cursor);            get = GetCursors;          format = 32; break;
	case XA_DRAWABLE:      size = sizeof(Drawable);          get = GetDrawables;        format = 32; break;
	case XA_FONT:          size = sizeof(Font);              get = GetFonts;            format = 32; break;
	case XA_PIXMAP:        size = sizeof(Pixmap);            get = GetPixmaps;          format = 32; break;
	case XA_VISUALID:      size = sizeof(unsigned);          get = GetVisualIds;        format = 32; break;
	case XA_CARDINAL:      size = sizeof(unsigned);          get = GetUnsigned;         format = 32; break;
	case XA_INTEGER:       size = sizeof(int);               get = GetSigned;           format = 32; break;
	case XA_WINDOW:        size = sizeof(Window);            get = GetWindows;          format = 32; break;
	case XA_ARC:           size = sizeof(XArc);              get = GetArcs;             format = 16; break;
	case XA_POINT:         size = sizeof(XPoint);            get = GetPoints;           format = 16; break;
	case XA_RECTANGLE:     size = sizeof(XRectangle);        get = GetRects;            format = 16; break;
	case XA_RGB_COLOR_MAP: size = sizeof(XStandardColormap); get = GetStandardColormap; format = 32; break;
	case XA_WM_HINTS:      size = sizeof(XWMHints);          get = GetXWMHints;         format = 32; break;
	case XA_WM_SIZE_HINTS: size = sizeof(XSizeHints);        get = GetXWMSizeHints;     format = 32; break;
	case XA_WM_ICON_SIZE:  size = sizeof(XIconSize);         get = GetIconSize;         format = 32; break;
	default: Crash ("Bad property type %x",encoding); /*NOTREACHED*/
      }
  
      bytes  = length * size;
      value  = (uchar *) alloca(bytes);
      GetList4(DEREFHANDLE(list),value,(int)size,get);
    }
  
  {
    XTextProperty T;
    
    T.value    = value;
    T.encoding = target;
    T.format   = format;
    T.nitems   = (bytes * 8) / format;
    
    XSetTextProperty(d,w,&T,property);
  }
}

static Handle GetWMHints
(
  Handle dsHandle, /* Handle to (X_Display_Object *) */
  Window w
)
{
  Handle tuple = alloc_and_save(MUTABLE(7));
  
  XWMHints *H = XGetWMHints(DEREFDISPLAYHANDLE(dsHandle)->display,w);
  
  if (H)
  {
  
#define data DEREFHANDLE(tuple)  
    data[0] = DEREFWORDHANDLE(Make_unsigned(H->input));
    data[1] = DEREFWORDHANDLE(Make_unsigned(H->initial_state));
    data[2] = DEREFWORDHANDLE(EmptyPixmap(dsHandle,H->icon_pixmap));
    data[3] = DEREFWORDHANDLE(EmptyWindow(dsHandle,H->icon_window));
    data[4] = DEREFWORDHANDLE(CreatePoint(H->icon_x,H->icon_y));
    data[5] = DEREFWORDHANDLE(EmptyPixmap(dsHandle,H->icon_mask));
    data[6] = DEREFWORDHANDLE(Make_unsigned(H->flags));
#undef data

    XFree((char *)H);
  }
  
  /* else what (?) */
  
  return FINISHED(tuple);
}

static Handle GetWMSizeHints
(
  Display *d,
  Window   w,
  unsigned property
)
{
  XSizeHints H;
  long supplied; /* was unsigned SPF 6/1/94 */
  
  Handle tuple = alloc_and_save(MUTABLE(9));
  
  int s = XGetWMSizeHints(d,w,&H,&supplied,property);
  
  if (s)
  {
    Handle p1 = CreatePoint(H.min_aspect.x,H.min_aspect.y);
    Handle p2 = CreatePoint(H.max_aspect.x,H.max_aspect.y);

#define data DEREFHANDLE(tuple)   
    data[0] = DEREFWORDHANDLE(CreatePoint(H.x,H.y));
    data[1] = DEREFWORDHANDLE(CreateArea(H.width,H.height));
    data[2] = DEREFWORDHANDLE(CreateArea(H.min_width,H.min_height));
    data[3] = DEREFWORDHANDLE(CreateArea(H.max_width,H.max_height));
    data[4] = DEREFWORDHANDLE(CreateArea(H.width_inc,H.height_inc));
    data[5] = DEREFWORDHANDLE(CreatePair(p1,p2));
    data[6] = DEREFWORDHANDLE(CreateArea(H.base_width,H.base_height));
    data[7] = DEREFWORDHANDLE(Make_unsigned(H.win_gravity));
    data[8] = DEREFWORDHANDLE(Make_unsigned(H.flags));
#undef data
  }
  
  /* else (?) */
  
  return FINISHED(tuple);
}

#if 0
typedef struct
{
MLPair       *x0; /* pair of points */
MLXRectangle *x1;
word         *x2; /* ML int */
} MLWMGeometryTriple;
#endif

static Handle WMGeometry
(
  Handle               dsHandle, /* Handle to (X_Display_Object *) */
  String              *user,
  String              *def,
  unsigned             borderWidth,
  MLXWMSizeHintsTuple *P
)
{
  XSizeHints H; int x,y,width,height,gravity,mask;
  
  char userGeometry[500],defaultGeometry[500];
  
  GetXWMSizeHints(P,&H);
  
  Poly_String_To_C(user,userGeometry   ,sizeof(userGeometry));
  Poly_String_To_C(def ,defaultGeometry,sizeof(defaultGeometry));

  mask = XWMGeometry(DEREFDISPLAYHANDLE(dsHandle)->display,
                       DEREFDISPLAYHANDLE(dsHandle)->screen,
                       userGeometry,
                       defaultGeometry,
                       borderWidth,
                       &H,&x,&y,&width,&height,&gravity);
  
  return CreateTriple(CreatePoint(x,y),CreateArea(width,height),Make_unsigned(gravity));
}

static Handle CreateIconSize(XIconSize *s)
{
  return CreateTriple(CreateArea(s->min_width,s->min_height),
                        CreateArea(s->max_width,s->max_height),
                        CreateArea(s->width_inc,s->height_inc));
}

static Handle GetIconSizes(Display *d, Window w)
{
  XIconSize *sizes; 
  int count;
  
  int s = XGetIconSizes(d,w,&sizes,&count);
  
  if (s)
  {
    Handle list = CreateList4(count,sizes,sizeof(XIconSize),CreateIconSize);
  
    XFree((char *)sizes);
    
    return list;
  }
  
  return SAVE(nil_value);
}

static Handle GetTransientForHint
(
  Handle dsHandle, /* Handle to (X_Display_Object *) */
  Window w
)
{
  Window p;
  
  int s = XGetTransientForHint(DEREFDISPLAYHANDLE(dsHandle)->display,w,&p);
  
  if (s == 0) RaiseXWindows ("XGetTransientForHint failed");
  
  return EmptyWindow(dsHandle,p);
}

static Handle GetWMColormapWindows
(
  Handle dsHandle, /* Handle to (X_Display_Object *) */
  Window parent
)
{
  Window *windows;
  int count;
  
  int s = XGetWMColormapWindows(DEREFDISPLAYHANDLE(dsHandle)->display,parent,&windows,&count);
  
  if (s)
  {
    Handle list = CreateList5(count,windows,sizeof(Window),CreateDrawable,dsHandle);

    XFree((char *)windows);
    
    return list;
  }
  
  return SAVE(nil_value);
}


static Handle GetRGBColormaps
(
  Handle   dsHandle, /* Handle to (X_Display_Object *) */
  Window   w,
  unsigned property
)
{
  XStandardColormap *maps;
  int count;
  
  int s = XGetRGBColormaps(DEREFDISPLAYHANDLE(dsHandle)->display,w,&maps,&count,property);
  
  if (s)
  {
    Handle list = CreateList5(count,maps,sizeof(XStandardColormap),CreateStandardColormap,dsHandle);

    XFree((char *)maps);
    
    return list;
  }
  
  return SAVE(nil_value);
}

static Handle GetID(X_Object *P)
{
  switch(P->type)
  {
    case X_GC:       return Make_unsigned(GetGC(P)->gid);           /* GCID       */
    case X_Font:     return Make_unsigned(GetFont(P));              /* FontID     */
    case X_Cursor:   return Make_unsigned(GetCursor(P));            /* CursorId   */
    case X_Window:   return Make_unsigned(GetWindow(P));            /* DrawableID */
    case X_Pixmap:   return Make_unsigned(GetPixmap(P));            /* DrawableID */
    case X_Colormap: return Make_unsigned(GetColormap(P));          /* ColormapID */
    case X_Visual:   return Make_unsigned(GetVisual(P)->visualid);  /* VisualID   */
    case X_Widget:   return Make_unsigned((unsigned)GetNWidget(P)); /* Widget -- SAFE(?) */
    default:         Crash ("Bad X_Object type (%d) in GetID",P->type) /*NOTREACHED*/;
  }
}

static Handle OpenDisplay(String *string)
{
  char               name[500];
  Display           *display;
  Handle dsHandle /* Handle to (X_Display_Object *) */;

  Poly_String_To_C(string,name,sizeof(name));

  if (A.noDisplay) RaiseXWindows ("XOpenDisplay failed");

  display = XOpenDisplay(name);

  if (display == 0) RaiseXWindows ("XOpenDisplay failed");

  /* I don't think this is needed.  DCJM 26/5/2000. */
  /* add_file_descr(display->fd); */
  
  dsHandle = alloc_and_save(MUTABLE(BYTES(SIZEOF(X_Display_Object))));

  debug1 ("%s display opened\n",DisplayString(display));

  debug1 ("%x display fd\n",display->fd);

#define ds DEREFDISPLAYHANDLE(dsHandle)
  /* Ok to store C values because this is a byte object */
  ds->type        = X_Display;
  ds->display     = display;
  ds->screen      = DefaultScreen(display);
  ds->app_context = 0;
#undef ds

  return AddXObject(FINISHED(dsHandle));
}

/* indirection removed SPF 11/11/93 */
static XmFontList GetXmFontList(ML_Cons_Cell *p /* NOT a handle */)
{
  if (NONNIL(p))
  {
    char       charset[500];
    XmFontList L;
    MLPair    *q = (MLPair *)(p->h);
    
    Poly_String_To_C((String *)SND(q),charset,sizeof(charset));
    L = XmFontListCreate((XFontStruct *)FST(q),charset); /* cast added SPF 6/1/94 */
    
    p = p->t;
    
    while(NONNIL(p))
    {
      q = (MLPair *)(p->h);
      
      Poly_String_To_C((String *)SND(q),charset,sizeof(charset));
      L = XmFontListAdd(L,(XFontStruct *)FST(q),charset); /* cast added SPF 6/1/94 */
      
      p = p->t;
    }
    
    return L;
  }
  
  return 0;
}

/*
      datatype CType = CAccelerators  of XtAccelerators
                     | CBool          of bool
                     | CColormap      of Colormap
                     | CCursor        of Cursor
                     | CDimension     of int
                     | CFontList      of (XFontStruct * string) list
                     | CInt           of int
                     | CIntTable      of int list
                     | CKeySym        of int
                     | CPixmap        of Drawable
                     | CPosition      of int
                     | CString        of string
                     | CStringTable   of string list
                     | CTrans         of XtTranslations
                     | CUnsignedChar  of int
                     | CUnsignedTable of int list
                     | CVisual        of Visual
                     | CWidget        of Widget
                     | CWidgetList    of Widget list
                     | CXmString      of XmString
                     | CXmStringTable of XmString list;
*/

#define CAccelerators  1
#define CBool          2
#define CColormap      3
#define CCursor        4
#define CDimension     5
#define CFontList      6
#define CInt           7
#define CIntTable      8
#define CKeySym        9
#define CPixmap        10
#define CPosition      11
#define CString        12
#define CStringTable   13
#define CTrans         14
#define CUnsignedChar  15
#define CUnsignedTable 16
#define CVisual        17
#define CWidget        18
#define CWidgetList    19
#define CXmString      20
#define CXmStringTable 21

typedef struct
{
  word  tag;
  word  N;
  char *name;
  union
  {
    XtAccelerators acc;
    Boolean        bool;
    Colormap       cmap;
    Cursor         cursor;
    Dimension      dim;
    XmFontList     F;
    int            i;
    int           *I;
    KeySym         keysym;
    Pixmap         pixmap;
    Position       posn;
    char          *string;
    char         **S;
    XtTranslations trans;
    uchar          u;
    uchar         *U;
    Visual        *visual;
    Widget         widget;
    WidgetList     W;
    XmString       xmString;
    XmString      *X;
  } u;
} ArgType;


static void GetXmString(word *w, XmString *p)
{
  char *s;
  
  CopyString(w,&s);
  
  *p = XmStringCreateLtoR(s,XmSTRING_DEFAULT_CHARSET);
  
  free(s);
}

static void GetXmStrings(ML_Cons_Cell *list, ArgType *T)
{
  T->N   = 0;
  T->u.X = 0;

  if (NONNIL(list))
  {
    T->N   = ListLength(list);
    T->u.X = (XmString *) malloc(T->N * sizeof(XmString));
  
    GetList4(list,T->u.X,sizeof(XmString),GetXmString);
  }
}

static void GetStrings(ML_Cons_Cell *list, ArgType *T)
{
  T->N   = 0;
  T->u.S = 0;

  if (NONNIL(list))
  {
    T->N   = ListLength(list);
    T->u.S = (char **) malloc(T->N * sizeof(char *));
  
    GetList4(list,T->u.S,sizeof(char *),CopyString);
  }
}

static void FreeXmStrings(ArgType *T)
{
  word i;
  
  for(i = 0; i < T->N; i++) XmStringFree (T->u.X[i]);
  
  free(T->u.X);
}

static void GetITable(ML_Cons_Cell *list, ArgType *T)
{
  T->N   = 0;
  T->u.I = 0;

  if (NONNIL(list))
  {
    T->N   = ListLength(list);
    T->u.I = (int *) malloc(T->N * sizeof(int));
  
    GetList4(list,T->u.I,sizeof(int),GetUnsigned);
  }
}

static void GetUTable(ML_Cons_Cell *list, ArgType *T)
{
  T->N   = 0;
  T->u.U = 0;

  if (NONNIL(list))
  {
    T->N   = ListLength(list);
    T->u.U = (uchar *)malloc(T->N * sizeof(uchar));
  
    GetList4(list,T->u.U,sizeof(uchar),GetUChars);
  }
}

/*
    case CIntTable:      GetITable   ((ML_Cons_Cell *)v,T); break;
    case CUnsignedTable: GetUTable   ((ML_Cons_Cell *)v,T); break;
    case CString:        CopyString  (v,&T->u.string); break;
    case CStringTable:   GetStrings  ((ML_Cons_Cell *)v,T); break;
    case CXmString:      GetXmString (v,&T->u.xmString); break;
    case CXmStringTable: GetXmStrings((ML_Cons_Cell *)v,T); break;
*/

static void FreeArgs(ArgType *T, word N)
{
  while(N--)
  {
    free(T->name);
    
    switch(T->tag)
    {
      case CAccelerators:  break;
      case CBool:          break;
      case CColormap:      break;
      case CCursor:        break;
      case CDimension:     break;
      case CFontList:      XmFontListFree(T->u.F); break;
      case CInt:           break;
      case CIntTable:      break;
      case CKeySym:        break;
      case CPixmap:        break;
      case CPosition:      break;
      case CString:        XtFree(T->u.string); break;
      case CStringTable:   FreeStrings(T->u.S,T->N); free(T->u.S); break;
      case CTrans:         break;
      case CUnsignedChar:  break;
      case CUnsignedTable: break;
      case CVisual:        break;
      case CWidget:        break;
      case CWidgetList:    break;
      case CXmString:      XmStringFree (T->u.xmString); break;
      case CXmStringTable: FreeXmStrings(T); break;
      
      default: Crash ("Bad arg type %x",T->tag);
    }
    
    T++;
  }
}

/* 
type Arg sharing type Arg = exn;
val Exn: Arg -> Exn = Cast;
val Arg: Exn -> Arg = Cast;
datatype Exn = EXN of unit ref * string * unit;
*/

/* (string,(v,tag)) */
static void SetArgType(MLPair *p, ArgType *T)
{
  word *v = FST((MLPair *)SND(p));
  
  T->tag = UNTAGGED(SND((MLPair *)SND(p)));
  T->N   = 0;
  T->u.i = 0;

  CopyString(FST(p),&T->name);
  
  switch(T->tag)
  {
    case CAccelerators:  T->u.acc    = GetAcc       ((X_Object *)v); break;
    case CBool:          T->u.bool   = get_C_ulong  (v); break;
    case CColormap:      T->u.cmap   = GetColormap  ((X_Object *)v); break;
    case CCursor:        T->u.cursor = GetCursor    ((X_Object *)v); break;
    case CDimension:     T->u.dim    = get_C_ushort (v); break;
    case CFontList:      T->u.F      = GetXmFontList((ML_Cons_Cell *)v); break;
    case CInt:           T->u.i      = get_C_long   (v); break;
    case CKeySym:        T->u.keysym = get_C_ulong  (v); break;
    case CPixmap:        T->u.pixmap = GetPixmap    ((X_Object *)v); break;
    case CPosition:      T->u.posn   = get_C_short  (v); break;
    case CTrans:         T->u.trans  = GetTrans     ((X_Object *)v); break;
    case CUnsignedChar:  T->u.u      = get_C_uchar  (v); break;
    case CVisual:        T->u.visual = GetVisual    ((X_Object *)v); break;
    case CWidget:        T->u.widget = GetNWidget   ((X_Object *)v); break;

    /* The following types allocate memory, but only in the C heap */
    
    case CIntTable:      GetITable   ((ML_Cons_Cell *)v,T); break;
    case CUnsignedTable: GetUTable   ((ML_Cons_Cell *)v,T); break;
    case CString:        CopyString  (v,&T->u.string); break;
    case CStringTable:   GetStrings  ((ML_Cons_Cell *)v,T); break;
    case CXmString:      GetXmString (v,&T->u.xmString); break;
    case CXmStringTable: GetXmStrings((ML_Cons_Cell *)v,T); break;
    
    default: Crash ("Bad arg type %x",T->tag);
  }
}

static void SetArgs(Arg *A, ArgType *T, word N)
{
  while(N--)
  {
    A->name = T->name;
  
    switch(T->tag)
    {
      case CAccelerators:  A->value = (XtArgVal) T->u.acc; break;
      case CBool:          A->value = (XtArgVal) T->u.bool; break;
      case CColormap:      A->value = (XtArgVal) T->u.cmap; break;
      case CCursor:        A->value = (XtArgVal) T->u.cursor; break;
      case CDimension:     A->value = (XtArgVal) T->u.dim; break;
      case CFontList:      A->value = (XtArgVal) T->u.F; break;
      case CInt:           A->value = (XtArgVal) T->u.i; break;
      case CIntTable:      A->value = (XtArgVal) T->u.I; break;
      case CKeySym:        A->value = (XtArgVal) T->u.keysym; break;
      case CPixmap:        A->value = (XtArgVal) T->u.pixmap; break;
      case CPosition:      A->value = (XtArgVal) T->u.posn; break;
      case CString:        A->value = (XtArgVal) T->u.string; break;
      case CStringTable:   A->value = (XtArgVal) T->u.S; break;
      case CTrans:         A->value = (XtArgVal) T->u.trans; break;
      case CUnsignedChar:  A->value = (XtArgVal) T->u.u; break;
      case CUnsignedTable: A->value = (XtArgVal) T->u.U; break;
      case CVisual:        A->value = (XtArgVal) T->u.visual; break;
      case CWidget:        A->value = (XtArgVal) T->u.widget; break;
      case CXmString:      A->value = (XtArgVal) T->u.xmString; break;
      case CXmStringTable: A->value = (XtArgVal) T->u.X; break;
      
      default: Crash ("Bad arg type %x",T->tag);
    }
    
    A++;
    T++;
  }
}

/* add current callback to (pending?) FList */
static void RunWidgetCallback(Widget w, XtPointer closure, XtPointer call_data)
{
  C_List *C = (C_List *)closure;

  if (callbacks_enabled)
  {
    Handle tailHandle     = SAVE(MLLIST(FList));
    Handle widgetHandle   = SAVE(C->widget_object);
    Handle functionHandle = SAVE(C->function);
    Handle pairHandle     = CreatePair(widgetHandle,functionHandle);
    
    FList = (ML_Cons_Cell *)DEREFHANDLE(CreatePair(pairHandle,tailHandle));
  }
#if 0
  else proper_printf("Ignoring event for widget %p\n",C->widget_object);
#endif
}

static void SetCallbacks(X_Widget_Object *W, ML_Cons_Cell *list, word *initial)
{
  char name[100];
  ML_Cons_Cell *p;
  Widget w = GetWidget((X_Object *)W);
  
  assert(w != NULL); /* SPF */
  assert(w != (Widget)nil_value); /* SPF */
  
  for(p = W->callbackList; NONNIL(p); p = p->t)
  {
    MLPair *q = (MLPair *)p->h;
      
    Poly_String_To_C((String *)FST(q),name,sizeof(name));
    
    if (strcmp(name,"messageCallback")  != 0
     && strcmp(name,XtNdestroyCallback) != 0)
    {
      XtRemoveAllCallbacks(w,name);
    }
  }
  
#if 0
  /* We no longer need the old callback data for this widget,
    assuming we've replaced all the callbacks. But what if
    we've only replaced some of them? It's probably better
    to allow this space leak that to delete vital callback data.
    I'll have to think about this hard sometime. (Of course, the
    user isn't supposed to call XtSetCallbacks more than once, in which
    case the problem doesn't even arise.) SPF 29/2/96 */
  PurgeCCallbacks(W,w);
#endif
  
  for(p = list; NONNIL(p); p = p->t)
  {
    C_List *C = (C_List *)malloc(sizeof(C_List));
    MLPair *q = (MLPair *)p->h;
      
    C->function      = SND(q);
    C->widget_object = W;
    C->next          = CList;
    
    debugCreateCallback(W,w,C);
      
    CList = C;
    
    Poly_String_To_C((String *)FST(q),name,sizeof(name));

    if (strcmp(name,"messageCallback")  != 0
     && strcmp(name,XtNdestroyCallback) != 0)
    {
      XtAddCallback(w,name,RunWidgetCallback,C);
    }
  }
  
  ASSIGN(W->state       ,initial);
  ASSIGN(W->callbackList,list);
}

static void RunWidgetEventhandler (Widget w, XtPointer p, XEvent *ev, Boolean *c)
{
  C_List *C = (C_List *)p;
  if ( callbacks_enabled )
  {
    Handle tailHandle     = SAVE(MLLIST(GList));
    Handle widgetHandle   = SAVE(C->widget_object);
    Handle functionHandle = SAVE(C->function);
    Handle pairHandle     = CreatePair(widgetHandle,functionHandle);
    
    GList = (ML_Cons_Cell *)DEREFHANDLE(CreatePair(pairHandle,tailHandle));
  }
}

static void AddEventhandler (
   X_Widget_Object *W, EventMask EventM, Boolean nonmask, Handle p)
{
  Widget w = GetWidget((X_Object *)W) ;
  C_List *C = (C_List *) malloc ( sizeof(C_List) ) ;
  /* Add the function to the callback list, so that it
     will not be G.C'ed away. */
  C->function = DEREFWORDHANDLE(p);
  C->widget_object   = W ;
  C->next     = CList ;
      
  CList = C ;

  XtAddEventHandler (w, EventM, nonmask, RunWidgetEventhandler, C);
}

static Handle AppInitialise
(
  String *s1,
  String *s2,
  String *s3,
  Handle fallbackHead,
  Handle argHead
)
{
  char               displayName[500];
  char               appName[500];
  char               appClass[500];
  XtAppContext       app_context;
  Display           *display;
  Widget             shell;
  Handle dsHandle /* Handle to (X_Display_Object *) */;
  int                argc = 0; /* an "int" for Solaris, but should be "unsigned" for SunOS */
  word               F = ListLength(DEREFLISTHANDLE(fallbackHead)) + 1;
  word               N = ListLength(DEREFLISTHANDLE(argHead));
  char             **S = (char   **) alloca(F * sizeof(char *));
  Arg               *R = (Arg     *) alloca(N * sizeof(Arg));
  ArgType           *T = (ArgType *) alloca(N * sizeof(ArgType));

  Poly_String_To_C(s1,displayName ,sizeof(displayName));
  Poly_String_To_C(s2,appName     ,sizeof(appName));
  Poly_String_To_C(s3,appClass    ,sizeof(appClass));

  if (A.noDisplay) RaiseXWindows ("XtAppInitialise failed (-noDisplay specified)");

  app_context = XtCreateApplicationContext();
  
  GetList4(DEREFHANDLE(fallbackHead),S,sizeof(char *),CopyString);
  S[F-1] = NULL;   /* list must be NULL terminated */
  XtAppSetFallbackResources(app_context,S);

  display = XtOpenDisplay(app_context,displayName,appName,appClass,NULL,0,&argc,0);
  if (display == 0) RaiseXWindows ("XtAppInitialise failed (can't open display)");

  /* I don't think this is needed.  DCJM 26/5/2000 */
  /* add_file_descr(display->fd); */
  
  debug1 ("%s display opened\n",DisplayString(display));
  debug1 ("%x display fd\n",display->fd);

  /* ok to store C values because this is a BYTE object */
  dsHandle = alloc_and_save(MUTABLE(BYTES(SIZEOF(X_Display_Object))));
  DEREFDISPLAYHANDLE(dsHandle)->type        = X_Display;
  DEREFDISPLAYHANDLE(dsHandle)->display     = display;
  DEREFDISPLAYHANDLE(dsHandle)->screen      = DefaultScreen(display);
  DEREFDISPLAYHANDLE(dsHandle)->app_context = app_context; 
  AddXObject(FINISHED(dsHandle));
  
  GetList4(DEREFHANDLE(argHead),T,sizeof(ArgType),SetArgType);
  SetArgs(R,T,N);
  shell = XtAppCreateShell(appName,appClass,applicationShellWidgetClass,display,R,N);
  FreeArgs(T,N);
  
  if (shell == 0) RaiseXWindows ("XtAppInitialise failed  (can't create application shell)");

  /* added 7/12/94 SPF */
  XtAddCallback(shell,XtNdestroyCallback,DestroyWidgetCallback,NULL);
  
  return NewWidget(dsHandle,shell);
}

static Handle CreatePopupShell
(
  String  *s,
  Handle  dsHandle, /* Handle to (X_Display_Object *) */
  Widget  parent,
  Handle  list
)
{
  char name[100]; Widget shell;
  
  word     N = ListLength(DEREFLISTHANDLE(list));
  Arg     *A = (Arg     *) alloca(N * sizeof(Arg));
  ArgType *T = (ArgType *) alloca(N * sizeof(ArgType));

  GetList4(DEREFHANDLE(list),T,sizeof(ArgType),SetArgType);
  SetArgs(A,T,N);
  
  Poly_String_To_C(s,name,sizeof(name));
  
  shell = XtCreatePopupShell(name,applicationShellWidgetClass,parent,A,N);
  
  FreeArgs(T,N);

  if (shell == 0) RaiseXWindows ("XtCreatePopupShell failed");
  
  /* added 7/12/94 SPF */
  XtAddCallback(shell,XtNdestroyCallback,DestroyWidgetCallback,NULL);

  return NewWidget(dsHandle,shell);
}

static Handle CreateXm
(
  Widget (*create)(),
  char   *failed,
  Handle  dsHandle, /* Handle to (X_Display_Object *) */
  Widget  parent,
  String *s,
  Handle  list      /* Handle to (ML_Cons_Cell *) */
)
{
  char name[100]; Widget w;
  
  
  word     N = ListLength(DEREFLISTHANDLE(list));
  Arg     *A = (Arg     *) alloca(N * sizeof(Arg));
  ArgType *T = (ArgType *) alloca(N * sizeof(ArgType));

  GetList4(DEREFHANDLE(list),T,sizeof(ArgType),SetArgType);
  SetArgs(A,T,N);

  Poly_String_To_C(s,name,sizeof(name));
  
  w = (* create)(parent,name,A,N);
  
  FreeArgs(T,N);

  if (w == 0) RaiseXWindows(failed);

  XtAddCallback(w,XtNdestroyCallback,DestroyWidgetCallback,NULL);

  return NewWidget(dsHandle,w);
}

static void SetValues(Widget w, Handle list)
{
  word     N = ListLength(DEREFLISTHANDLE(list));
  Arg     *A = (Arg     *) alloca(N * sizeof(Arg));
  ArgType *T = (ArgType *) alloca(N * sizeof(ArgType));

  GetList4(DEREFHANDLE(list),T,sizeof(ArgType),SetArgType);
  SetArgs(A,T,N);
  
  XtSetValues(w,A,N);

  FreeArgs(T,N);
}

typedef struct
{
  char *listName;
  char *intName;
} StringPair;

static StringPair listTypes[] =
{
  {"argv"                  ,"argc"},
  {"buttonAccelerators"    ,"buttonCount"},
  {"buttonAcceleratorText" ,"buttonCount"},
  {"buttonMnemonicCharSets","buttonCount"},
  {"buttonMnemonics"       ,"buttonCount"},
  {"buttons"               ,"buttonCount"},
  {"buttonType"            ,"buttonCount"},
  {"children"              ,"numChildren"},
  {"dirListItems"          ,"dirListItemCount"},
  {"fileListItems"         ,"fileListItemCount"},
  {"historyItems"          ,"historyItemCount"},
  {"items"                 ,"itemCount"},
  {"listItems"             ,"listItemCount"},
  {"selectedItems"         ,"selectedItemCount"},
  {"selectionArray"        ,"selectionArrayCount"},
};

#define MAXListTYPES (sizeof(listTypes)/sizeof(listTypes[0]))

/* (string,(v,tag)) - ML (string*Ctype) */
static void GetArgType
(
  MLPair  *p,
  ArgType *T,
  int      i, /* not used; needed to keep function type right */
  Widget   w
)
{
  T->tag = UNTAGGED(SND((MLPair *)SND(p)));
  T->N   = 0;
  T->u.i = 0;

  CopyString(FST(p),&T->name);

  if (T->tag == CIntTable      ||
      T->tag == CUnsignedTable ||
      T->tag == CWidgetList    ||
      T->tag == CStringTable   ||
      T->tag == CXmStringTable)      /* if it is a list type we need to get the length from another resource */
  {
    Arg arg; word i; int result;
      
    for(i = 0; i < MAXListTYPES; i++)
    {
      if (strcmp(listTypes[i].listName,T->name) == 0) break;
    }
    
    if (i == MAXListTYPES) Crash ("Bad list resource name %s",T->name);

    arg.name  = listTypes[i].intName;
    arg.value = (XtArgVal) &result;
    /* Bug fix here which only appeared in OpenMotif and LessTif.  We need
       to pass the address of an integer here to receive the result.
       DCJM 17/5/02. */
    
    XtGetValues(w,&arg,1);
      
    T->N = result;
  }
}

static Handle CreateWidget
(
  Widget *p,
  int    i, /* not used */
  Handle dsHandle /* Handle to (X_Display_Object *) */
)
{
  return EmptyWidget(dsHandle,*p);
}

static Handle CreateXmString(XmString *t)
{
  char  *s;
  Handle S;
  
  XmStringGetLtoR(*t,XmSTRING_DEFAULT_CHARSET,&s);
  
  S = Make_string(s);
  
  XtFree(s);
  
  return S;
}

static Handle CreateFontList
(
  Handle     dsHandle, /* Handle to (X_Display_Object *) */
  XmFontList F
)
{
  XmFontContext   C;
  XmStringCharSet charset;
  XFontStruct    *fs;

  Handle list  = SAVE(nil_value);
  Handle tail  = SAVE(nil_value);

  Handle saved = mark_save_vec(); /* remember how far the save vector has been extended */

  
  if (XmFontListInitFontContext(&C,F) == False) return list;
  
  while(XmFontListGetNextFont(C,&charset,&fs))
  {
    ML_Cons_Cell *L = (ML_Cons_Cell *)alloc(MUTABLE(SIZEOF(ML_Cons_Cell)));
    
    if (ISNIL(DEREFHANDLE(list))) DEREFLISTHANDLE(list) = L;
    
    if (NONNIL(DEREFHANDLE(tail)))
       { 
         DEREFLISTHANDLE(tail)->t = L;
         FINISHED(tail);
       }
    
    DEREFLISTHANDLE(tail) = L;
    
    /* the new list element is joined on, but not filled in */
    
    DEREFLISTHANDLE(tail)->h = DEREFWORDHANDLE(CreatePair(CreateFontStruct(fs,0,dsHandle),Make_string(charset))); 
    DEREFLISTHANDLE(tail)->t = (ML_Cons_Cell *)nil_value;
    
    reset_save_vec(saved);   /* reset save vector to stop it overflowing */
  }
  
  XmFontListFreeFontContext(C);
  
  if (NONNIL(DEREFHANDLE(tail))) FINISHED(tail);
  
  return list;
}

static Handle CreateUChar(uchar *p)
{
  return Make_unsigned(*p);
}

static Handle CreateArg
(
  ArgType *T,
  int      i,
  Handle   dsHandle /* Handle to (X_Display_Object *) */
)
{
  Handle value;

  switch(T->tag)
  {
    case CAccelerators:  value = EmptyAcc      (T->u.acc);       break;
    case CBool:          value = Make_bool     (T->u.bool);      break;
    case CColormap:      value = EmptyColormap (dsHandle,T->u.cmap);   break;
    case CCursor:        value = EmptyCursor   (dsHandle,T->u.cursor); break;
    case CDimension:     value = Make_int      (T->u.dim);       break;
    case CFontList:      value = CreateFontList(dsHandle,T->u.F);      break;
    case CInt:           value = Make_int      (T->u.i);         break;
    case CKeySym:        value = Make_unsigned (T->u.keysym);    break;
    case CPixmap:        value = EmptyPixmap   (dsHandle,T->u.pixmap); break;
    case CPosition:      value = Make_int      (T->u.posn);      break;
    case CString:        value = Make_string   (T->u.string);    break;
    case CTrans:         value = EmptyTrans    (T->u.trans);     break;
    case CUnsignedChar:  value = Make_unsigned (T->u.u);         break;
    case CVisual:        value = EmptyVisual   (dsHandle,T->u.visual); break;
    case CWidget:        value = EmptyWidget   (dsHandle,T->u.widget); break;

    case CXmString:      value = CreateXmString(&T->u.xmString); break;
    
    case CIntTable:      value = CreateList4(T->N,T->u.I,sizeof(int),     CreateUnsigned);        break;
    case CUnsignedTable: value = CreateList4(T->N,T->u.U,sizeof(uchar),   CreateUChar);           break;
    case CStringTable:   value = CreateList4(T->N,T->u.S,sizeof(char *),  CreateString);          break;
    case CWidgetList:    value = CreateList5(T->N,T->u.W,sizeof(Widget),  CreateWidget,dsHandle); break;
    case CXmStringTable: value = CreateList4(T->N,T->u.X,sizeof(XmString),CreateXmString);        break;
    
    default: Crash ("Bad arg type %x",T->tag); /*NOTREACHED*/
  }
  
  return value;
}

static Handle GetValue
(
  Handle  dsHandle, /* Handle to (X_Display_Object *) */
  Widget  w,
  MLPair *pair /* ML (string*Ctype) */
)
{
  Arg       A;
  ArgType   T;
  XmString *X = (XmString *) 0x55555555;
  XmString *Y = (XmString *) 0xAAAAAAAA;

  GetArgType(pair,&T,0,w);
  
  A.name  = T.name;
  A.value = (XtArgVal) &T.u;
  T.u.X   = X;
  
  /* The value is set to X. If it is left set to X      */
  /* then this may be a value this widget doesn't have. */
  
  XtGetValues(w,&A,1);
  
  if (T.u.X == X)
  {
    T.u.X = Y;

    XtGetValues(w,&A,1);

    if (T.u.X == Y)
    {
      char buffer[500];
      
      sprintf(buffer,"XtGetValues (%s) failed",T.name);
    
      RaiseXWindows(buffer);
    }
  }
  
  return CreateArg(&T,0,dsHandle);
}

/* What is the real ML type of p? (string*Ctype*string*string*string*Ctype) */
static void GetResource
(
  word      **p,
  XtResource *R,
  int         i,
  ArgType    *T,
  ArgType    *D,
  Widget      w
)
{
  GetArgType((MLPair *)p,&T[i],0,w); /* HACK !!! */
  
  CopyString(p[0],&R->resource_name);
  CopyString(p[2],&R->resource_class);
  CopyString(p[3],&R->resource_type);
  
  R->resource_size   = 4;
  R->resource_offset = Bytes(&T[i].u) - Bytes(T);
  
  SetArgType((MLPair *)&p[4],&D[i]); /* HACK !!! */
  
  R->default_type = D[i].name;
  
  if (UNTAGGED(p[5][1]) == CString)
    R->default_addr = (XtPointer) D[i].u.string;
  else
    R->default_addr = (XtPointer) &D[i].u;
}

static Handle GetSubresources
(
  Handle  dsHandle, /* Handle to (X_Display_Object *) */
  Widget  w,
  String *s1,
  String *s2,
  Handle  list
)
{
  char name [100];
  char class[100];
  
  word        N = ListLength(DEREFLISTHANDLE(list));
  ArgType    *T = (ArgType    *) alloca(N * sizeof(ArgType));
  ArgType    *D = (ArgType    *) alloca(N * sizeof(ArgType));
  XtResource *R = (XtResource *) alloca(N * sizeof(XtResource));

  {
    ML_Cons_Cell *p;
    int i = 0;
    
    for(p = (ML_Cons_Cell *)DEREFHANDLE(list); NONNIL(p); p = (ML_Cons_Cell *)(p->t))
    {
      GetResource((word **)(p->h),&R[i],i,T,D,w);
      i++;
    }
  }

  Poly_String_To_C(s1,name ,sizeof(name));
  Poly_String_To_C(s2,class,sizeof(class));
  
  XtGetSubresources(w,T,name,class,R,N,NULL,0);
  
  return CreateList5(N,T,sizeof(ArgType),CreateArg,dsHandle);
}

static Handle GetApplicationResources (
  Handle  dsHandle, /* Handle to (X_Display_Object *) */
  Widget  w,
  Handle  list
)
{
  word        N = ListLength (DEREFLISTHANDLE(list)) ;
  ArgType    *T = (ArgType    *) alloca ( N * sizeof(ArgType) ) ;
  ArgType    *D = (ArgType    *) alloca ( N * sizeof(ArgType) ) ;
  XtResource *R = (XtResource *) alloca ( N * sizeof(XtResource) ) ;

  {
    ML_Cons_Cell *p;
    int i = 0;
    
    for(p = (ML_Cons_Cell *)DEREFHANDLE(list); NONNIL(p); p = (ML_Cons_Cell *)(p->t))
    {
      GetResource((word **)(p->h),&R[i],i,T,D,w);
      i++;
    }
  }
  
  XtGetApplicationResources ( w,T,R,N,NULL,0 ) ;
  
  return CreateList5 ( N,T,sizeof(ArgType),CreateArg,dsHandle ) ;
}

static void GetChild(X_Object *p, Widget *w)
{
  *w = GetWidget(p);
  
  if (XtParent(*w) == NULL) RaiseXWindows ("not a child"); 
}

static void ManageChildren(Handle list)
{
  word    N = ListLength(DEREFLISTHANDLE(list));
  Widget *W = (Widget *) alloca(N * sizeof(Widget));

  GetList4(DEREFHANDLE(list),W,sizeof(Widget),GetChild);
  
  XtManageChildren(W,N);
}

static void UnmanageChildren(Handle list)
{
  word    N = ListLength(DEREFLISTHANDLE(list));
  Widget *W = (Widget *) alloca(N * sizeof(Widget));

  GetList4(DEREFHANDLE(list),W,sizeof(Widget),GetChild);
  
  XtUnmanageChildren(W,N);
}

static Handle ParseTranslationTable(String *s)
{
  XtTranslations table;
  
  int   size   = s->length + 1;
  char *buffer = (char *)alloca(size);

  Poly_String_To_C(s,buffer,size);
  table = XtParseTranslationTable(buffer);
  
  return EmptyTrans(table);
}

static void CommandError(Widget w, word *s)
{
  XmString p;
  
  GetXmString(s,&p);
  
  XmCommandError(w,p);

  XmStringFree (p);
}

static void FileSelectionDoSearch(Widget w, word *s)
{
  XmString p;
  
  GetXmString(s,&p);
  
  XmFileSelectionDoSearch(w,p);

  XmStringFree (p);
}

static void MenuPosition (Widget w, int x, int y)
{
   XButtonPressedEvent ev;
   memset (&ev, 0, sizeof(ev));
   ev.type = 4; /* Must be button. */
   ev.x_root = x;
   ev.y_root = y;
   ev.button = 3; /* Is this required? */
   ev.same_screen = 1; /* Assume this. */
   XmMenuPosition (w, &ev);
}

static Handle XmIsSomething(unsigned is_code, Widget widget)
{
  unsigned i;
  
  switch(is_code)
  {
    case  1: i = XmIsArrowButton        (widget); break;
    case  2: i = XmIsArrowButtonGadget  (widget); break;
    case  3: i = XmIsBulletinBoard      (widget); break;
    case  4: i = XmIsCascadeButton      (widget); break;
    case  5: i = XmIsCascadeButtonGadget(widget); break;
    case  6: i = XmIsCommand            (widget); break;
    case  7: i = XmIsDesktopObject      (widget); break; /* ok - SPF 9/8/94 */
    case  8: i = XmIsDialogShell        (widget); break;
/* Unsupported in Motif 1.2
    case  9: i = XmIsDisplayObject      (widget); break;
*/
    case 10: i = XmIsDrawingArea        (widget); break;
    case 11: i = XmIsDrawnButton        (widget); break;
    case 12: i = XmIsExtObject          (widget); break; /* ok - SPF 9/8/94 */
    case 13: i = XmIsFileSelectionBox   (widget); break;
    case 14: i = XmIsForm               (widget); break;
    case 15: i = XmIsFrame              (widget); break;
    case 16: i = XmIsGadget             (widget); break;
    case 17: i = XmIsLabel              (widget); break;
    case 18: i = XmIsLabelGadget        (widget); break;
    case 19: i = XmIsList               (widget); break;
    case 20: i = XmIsMainWindow         (widget); break;
    case 21: i = XmIsManager            (widget); break;
    case 22: i = XmIsMenuShell          (widget); break;
    case 23: i = XmIsMessageBox         (widget); break;
    case 24: i = XmIsMotifWMRunning     (widget); break;
    case 25: i = XmIsPanedWindow        (widget); break;
    case 26: i = XmIsPrimitive          (widget); break;
    case 27: i = XmIsPushButton         (widget); break;
    case 28: i = XmIsPushButtonGadget   (widget); break;
    case 29: i = XmIsRowColumn          (widget); break;
    case 30: i = XmIsScale              (widget); break;
/* Unsupported in Motif 1.2
    case 31: i = XmIsScreenObject       (widget); break;
*/
    case 32: i = XmIsScrollBar          (widget); break;
    case 33: i = XmIsScrolledWindow     (widget); break;
    case 34: i = XmIsSelectionBox       (widget); break;
    case 35: i = XmIsSeparator          (widget); break;
    case 36: i = XmIsSeparatorGadget    (widget); break;
#ifdef LESSTIF_VERSION
/* This is not supported in LessTif, at least not 0.89. */
    case 37: RaiseXWindows("XmIsShellExt: not implemented");
#else
    case 37: i = XmIsShellExt           (widget); break; /* ok - SPF 9/8/94 */
#endif
    case 38: i = XmIsText               (widget); break;
    case 39: i = XmIsTextField          (widget); break;
    case 40: i = XmIsToggleButton       (widget); break;
    case 41: i = XmIsToggleButtonGadget (widget); break;
    case 42: i = XmIsVendorShell        (widget); break;
    case 43: i = XmIsVendorShellExt     (widget); break; /* ok - SPF 9/8/94 */
/* Unsupported in Motif 1.2
    case 44: i = XmIsWorldObject        (widget); break;
*/
    
    default: Crash ("Bad code (%d) in XmIsSomething",is_code);
            /* NOTREACHED*/
  }

  return Make_bool(i);
}


/******************************************************************************/
/*                                                                            */
/*      Wrappers for standard widget operations                               */
/*                                                                            */
/******************************************************************************/

/************************* 0 parameters, no result ****************************/

/* widget -> unit */
static void WidgetAction
(
  char *func_name,
  Widget getWidget(char *, X_Object *),
  void applyFunc(Widget),
  X_Object *arg1
)
{
  Widget w = getWidget(func_name,arg1);
  applyFunc(w); 
}

/************************* 1 parameter, no result *****************************/

/* widget -> bool -> unit */
static void WidgetBoolAction
(
  char *func_name,
  Widget getWidget(char *, X_Object *),
  void applyFunc(Widget, Boolean),
  X_Object *arg1,
  word *arg2
)
{
  Widget w  = getWidget(func_name,arg1);
  Boolean b = (get_C_short(arg2) != 0);
  applyFunc(w,b);
}

/* widget -> int -> unit */
static void WidgetIntAction
(
  char *func_name,
  Widget getWidget(char *, X_Object *),
  void applyFunc(Widget, int),
  X_Object *arg1,
  word *arg2
)
{
  Widget w = getWidget(func_name,arg1);
  int i    = get_C_long(arg2);
  applyFunc(w,i);
}

/* widget -> int -> unit */
static void WidgetLongAction
(
  char *func_name,
  Widget getWidget(char *, X_Object *),
  void applyFunc(Widget, long),
  X_Object *arg1,
  word *arg2
)
{
  Widget w = getWidget(func_name,arg1);
  long i   = get_C_long(arg2);
  applyFunc(w,i);
}

/* widget -> string -> unit */
static void WidgetXmstringAction
(
  char *func_name,
  Widget getWidget(char *, X_Object *),
  void applyFunc(Widget, XmString),
  X_Object *arg1,
  word *arg2
)
{
  Widget w     = getWidget(func_name,arg1);
  XmString s;
  GetXmString(arg2,&s);
  applyFunc(w,s);
  XmStringFree(s);
}


/* widget -> string list -> unit */
static void WidgetXmstringlistAction
(
  char *func_name,
  Widget getWidget(char *, X_Object *),
  void applyFunc(Widget, XmString *, int),
  X_Object *arg1,
  ML_Cons_Cell *arg2
)
{
  Widget w          = getWidget(func_name,arg1);
  int n             = ListLength(arg2);
  XmString *strings = (XmString *)alloca(n * sizeof(XmString));
  GetList4(arg2,strings,sizeof(XmString),GetXmString);
  applyFunc(w,strings,n);
  {int i; for (i = 0; i < n; i ++) XmStringFree(strings[i]);}
}

/************************* 2 parameters, no result ****************************/

/* widget -> int -> bool -> unit */
static void WidgetIntBoolAction
(
  char *func_name,
  Widget getWidget(char *, X_Object *),
  void applyFunc(Widget, int, Boolean),
  X_Object *arg1,
  word *arg2,
  word *arg3
)
{ 
  Widget w  = getWidget(func_name,arg1);
  int i     = get_C_long(arg2);
  Boolean b = (get_C_ushort(arg3) != 0);
  applyFunc(w,i,b);
}

/* widget -> int -> int -> unit */
static void WidgetIntIntAction
(
  char *func_name,
  Widget getWidget(char *, X_Object *),
  void applyFunc(Widget, int, int),
  X_Object *arg1,
  word *arg2,
  word *arg3
)
{ 
  Widget w  = getWidget(func_name,arg1);
  int x     = get_C_long(arg2);
  int y     = get_C_long(arg3);
  applyFunc(w,x,y);
}

/* widget -> string -> bool -> unit */
static void WidgetXmstringBoolAction
(
  char *func_name,
  Widget getWidget(char *, X_Object *),
  void applyFunc(Widget, XmString, Boolean),
  X_Object *arg1,
  word *arg2,
  word *arg3
)
{
  Widget w     = getWidget(func_name,arg1);
  XmString s;
  Boolean b    = (get_C_ushort(arg3) != 0);
  
  GetXmString(arg2,&s);
  applyFunc(w,s,b);
  XmStringFree(s);
}


/* widget -> string -> int -> unit */
static void WidgetXmstringIntAction
(
  char *func_name,
  Widget getWidget(char *, X_Object *),
  void applyFunc(Widget, XmString, int),
  X_Object *arg1,
  word *arg2,
  word *arg3
)
{
  Widget w     = getWidget(func_name,arg1);
  XmString s;
  int i        = get_C_long(arg3);
  GetXmString(arg2,&s);
  applyFunc(w,s,i);
  XmStringFree(s);
}

/* widget -> string list -> int -> unit */
static void WidgetXmstringlistIntAction
(
  char *func_name,
  Widget getWidget(char *, X_Object *),
  void applyFunc(Widget, XmString *, int, int),
  X_Object *arg1,
  ML_Cons_Cell *arg2,
  word *arg3
)
{
  Widget w          = getWidget(func_name,arg1);
  int n             = ListLength(arg2);
  int i             = get_C_long(arg3);
  XmString *strings = (XmString *)alloca(n * sizeof(XmString));

  GetList4(arg2,strings,sizeof(XmString),GetXmString);
  applyFunc(w,strings,n,i);
  {int i; for (i = 0; i < n; i ++) XmStringFree(strings[i]);}
}

/************************* n parameters, some result **************************/
static Handle int_ptr_to_arb(int *p)
{
  return (Make_arbitrary_precision(*p));
}

/* widget -> int */
static Handle WidgetToInt
(
  char *func_name,
  Widget getWidget(char *, X_Object *),
  int applyFunc(Widget),
  X_Object *arg1
)
{
  Widget w = getWidget(func_name,arg1);
  int res  = applyFunc(w);
  return(Make_arbitrary_precision(res));
}

/* widget -> int */
static Handle WidgetToLong
(
  char *func_name,
  Widget getWidget(char *, X_Object *),
  long applyFunc(Widget),
  X_Object *arg1
)
{
  Widget w = getWidget(func_name,arg1);
  long res  = applyFunc(w);
  return(Make_unsigned(res));
}

#if 0
/* widget -> int */
static Handle WidgetToUnsigned
(
  char *func_name,
  Widget getWidget(char *, X_Object *),
  unsigned applyFunc(Widget),
  X_Object *arg1
)
{
  Widget w = getWidget(func_name,arg1);
  unsigned res  = applyFunc(w);
  return(Make_unsigned(res));
}
#endif

/* widget -> bool */
static Handle WidgetToBool
(
  char *func_name,
  Widget getWidget(char *, X_Object *),
  Boolean applyFunc(Widget),
  X_Object *arg1
)
{
  Widget w = getWidget(func_name,arg1);
  Boolean res  = applyFunc(w);
  return(Make_bool(res));
}

/* widget -> string */
static Handle WidgetToString
(
  char *func_name,
  Widget getWidget(char *, X_Object *),
  char *applyFunc(Widget),
  X_Object *arg1
)
{
  Widget w   = getWidget(func_name,arg1);
  char *s    = applyFunc(w);
  Handle res = Make_string(s); /* safe, even if C pointer is NULL */
  XtFree(s);
  return(res);
}

/* widget -> int list */
static Handle WidgetToIntlist
(
  char *func_name,
  Widget getWidget(char *, X_Object *),
  Boolean applyFunc(Widget, int**, int *),
  X_Object *arg1
)
{
  int item_count, *items;
  Boolean non_empty;
  Widget w  = getWidget(func_name,arg1);

  non_empty = applyFunc(w, &items, &item_count);
  
  if (non_empty != TRUE)
    /* nothing found, and Motif hasn't allocated any space */
    /* so just retun nil */
    {
       return (SAVE(nil_value));
    }
  else
    /* copy the list into the ML heap, then free it */
    {
      Handle res = CreateList4(item_count,items,sizeof(int),int_ptr_to_arb);
      XtFree((char *)items);
      return res;
    }
}

/* widget -> string -> int list */
static Handle WidgetXmstringToIntlist
(
  char *func_name,
  Widget getWidget(char *, X_Object *),
  Boolean applyFunc(Widget, XmString, int**, int *),
  X_Object *arg1,
  word *arg2
)
{
  int item_count, *items;
  Boolean non_empty;
  Widget w     = getWidget(func_name,arg1);
  XmString s;
  
  GetXmString(arg2,&s);
  non_empty = applyFunc(w, s, &items, &item_count);
  XmStringFree(s);
  
  if (non_empty != TRUE)
    /* nothing found, so just retun nil */
    {
       return (SAVE(nil_value));
    }
  else
    /* copy the list into the ML heap, then free it */
    {
      Handle res = CreateList4(item_count,items,sizeof(int),int_ptr_to_arb);
      XtFree((char *)items);
      return res;
    }
}

/* widget -> string -> int */
static Handle WidgetXmstringToInt
(
  char *func_name,
  Widget getWidget(char *, X_Object *),
  int applyFunc(Widget, XmString),
  X_Object *arg1,
  word *arg2
)
{
  Widget w     = getWidget(func_name,arg1);
  XmString s;
  int res;
  
  GetXmString(arg2,&s);
  res = applyFunc(w, s);
  XmStringFree(s);
  
  return (Make_int(res));
}

/* widget -> string -> bool */
static Handle WidgetXmstringToBool
(
  char *func_name,
  Widget getWidget(char *, X_Object *),
  Boolean applyFunc(Widget, XmString),
  X_Object *arg1,
  word *arg2
)
{
  Widget w     = getWidget(func_name,arg1);
  XmString s;
  Boolean res;

  GetXmString(arg2,&s);
  res = applyFunc(w, s);
  XmStringFree(s);

  return (Make_bool(res));
}


/******************************************************************************/

/* code added SPF 25/2/95 */
static int isPossibleString(word *P)
{
  word L;
  int n;
  int m;
  
  if (!OBJ_IS_DATAPTR(P)) return 0;
  
  L = P[-1];
  
  if (! OBJ_IS_BYTE_OBJECT(L)) return 0;
  
  /* get object word count */
  n = OBJ_OBJECT_LENGTH(L);
  
  if (n < 1) return 0;

  /* get string byte count */
  m = P[0];
  
  if (m < 0) return 0;
  /* Since if m == -1, we got confused! */

  /* number of words to hold the bytes */
  m = (m + 3) / 4;
  
  /* number of words to hold the bytes, plus the byte count */
  m = m + 1;

  /* If that's the same as the object word count, 
     we've probably got a genuine string! */
  return (m == n);
}

/* Prints out the contents of a word in the X interface tuple */
static void DebugPrintWord(word *P /* was X_Object *P */)
{
  if (IS_INT(Word(P))) 
  {
    proper_printf("Short %d", UNTAGGED(Word(P)));
    return;
  }
  
  if (IsIOPointer(H,P))
  {
    proper_printf("IO %p",P);
    return;
  }

  if (isPossibleString(P))
  {
    if (((pstring)P)->length <= 40)
    {
      proper_printf("String: \"");
      print_string((pstring) P);
      proper_printf("\"");
      return;
    }
    else
    {
      proper_printf("Long String: %p", P);    
      return;
    }
  }

  /* The problem with the following code was that we can't be sure
     that the object we have is really an X_Object - it might just
     look like one. If this is the case, when we try to validate
     the object using ResourceExists we may get a core dump because
     ResourceExists assumes it has a valid X_Object and calls
     hashId which dereferences fields within the so-called X_object.
     That's why we redefine ResourceExists to be SafeResourceExists
     which doesn't make any assumptions about the contents of the
     so-called X_object. SPF 6/4/95 */

#define XP ((X_Object *)P)
#define ResourceExists SafeResourceExists
  {
    switch(XP->type)
    {
      case X_GC:       (ResourceExists(XP)
		       ? proper_printf("GC %lx", GetGC(XP)->gid)
		       : proper_printf("Old GC <%x>",(int)P));
		       return;
						  
      case X_Font:     (ResourceExists(XP)
		       ? proper_printf("Font %lx",GetFont(XP))
		       : proper_printf("Old Font <%x>",(int)P));
		       return;
						  
      case X_Cursor:   (ResourceExists(XP)
		       ? proper_printf("Cursor %lx",GetCursor(XP))
		       : proper_printf("Old Cursor <%x>",(int)P));
		       return;
			       
      case X_Window:   (ResourceExists(XP)
		       ? proper_printf("Window %lx",GetWindow(XP))
		       : proper_printf("Old Window <%p>",P));
		       return;
			       
      case X_Pixmap:   (ResourceExists(XP)
		       ? proper_printf("Pixmap %lx",GetPixmap(XP))
		       : proper_printf("Old Pixmap <%p>",P));
		       return;
      
      case X_Colormap: (ResourceExists(XP)
			? proper_printf("Colormap %lx",GetColormap(XP))
			: proper_printf("Old Colormap <%p>",P));
		       return;
      
      case X_Visual:   (ResourceExists(XP)
		       ? proper_printf("Visual %lx",GetVisual(XP)->visualid)
		       : proper_printf("Old Visual <%p>",P));
		       return;
      
      case X_Widget:   (ResourceExists(XP)
		       ? proper_printf("Widget %p",GetNWidget(XP))
		       : proper_printf("Old Widget <%p>",P));
		       return;
      
      case X_Trans:    (ResourceExists(XP)
		       ? proper_printf("Trans %p",GetTrans(XP))
		       : proper_printf("Old Trans <%p>",P));
		       return;
     
      case X_Acc:      (ResourceExists(XP)
		       ? proper_printf("Acc %p",GetAcc(XP))
		       : proper_printf("Old Acc <%p>",P));
		       return;
      
      case X_Display:  (ResourceExists(XP)
		       ? proper_printf("Display %s", DisplayString(GetDisplay(XP)))
		       + proper_printf(":%x", GetDisplay(XP)->fd)
		       : proper_printf("Old Display <%p>",P));
		       return;
      
      default:         proper_printf("Pointer "ZERO_X"%p",P);
		       return;
    }
   }
#undef ResourceExists
#undef XP
}

/* Prints out the contents of the X interface tuple */
static void DebugPrintCode(word **pt /* was word *pt - is NOT a Handle */)
{
  word N = OBJ_OBJECT_LENGTH((word)pt[-1]);
  word i = 1;
  
  assert(IS_INT(pt[0]));
  
  proper_printf("%d:(", UNTAGGED(pt[0]));
  
  while(i < N)
  {
    DebugPrintWord(pt[i++]);
    
    if (i < N)
    {
      proper_printf(",");
    }
  }
  
  proper_printf(")\n");
}

#define P0  DEREFHANDLE(params)[0]
#define P1  DEREFHANDLE(params)[1]
#define P2  DEREFHANDLE(params)[2]
#define P3  DEREFHANDLE(params)[3]
#define P4  DEREFHANDLE(params)[4]
#define P5  DEREFHANDLE(params)[5]
#define P6  DEREFHANDLE(params)[6]
#define P7  DEREFHANDLE(params)[7]
#define P8  DEREFHANDLE(params)[8]
#define P9  DEREFHANDLE(params)[9]
#define P10 DEREFHANDLE(params)[10]
#define P11 DEREFHANDLE(params)[11]
#define P12 DEREFHANDLE(params)[12]

#define XP1 ((X_Object *)P1)
#define XP2 ((X_Object *)P2)
#define XP3 ((X_Object *)P3)
#define XP4 ((X_Object *)P4)
#define XP5 ((X_Object *)P5)
#define XP6 ((X_Object *)P6)
#define XP7 ((X_Object *)P7)

/* Xwindows_c gets passed the address of an object in save_vec, */
/* which is itself a pointer to a tuple in the Poly heap.       */

Handle XWindows_c(params)
Handle params;

{
  word code = get_C_short(P0);
  
  if ((A.debug & DEBUG_X)) DebugPrintCode(DEREFHANDLE(params));

  switch(code)
  {
    case XCALL_Not:
      return Make_unsigned(~ get_C_ulong(P1));
    
    case XCALL_And:
      return Make_unsigned(get_C_ulong(P1) & get_C_ulong(P2));
      
    case XCALL_Or:
      return Make_unsigned(get_C_ulong(P1) | get_C_ulong(P2));
      
    case XCALL_Xor:
      return Make_unsigned(get_C_ulong(P1) ^ get_C_ulong(P2));
    
    case XCALL_DownShift:
      return Make_unsigned(get_C_ulong(P1) >> get_C_ulong(P2));
      
    case XCALL_UpShift:
      return Make_unsigned(get_C_ulong(P1) << get_C_ulong(P2));

    case XCALL_NoDrawable:
      return EmptyPixmap(SAVE(nil_value),(Pixmap)get_C_ulong(P1));
      
    case XCALL_NoCursor:
      return EmptyCursor(SAVE(nil_value),(Cursor)None);
      
    case XCALL_NoFont:
      return EmptyFont(SAVE(nil_value),(Font)None,(XFontStruct *)NULL);
      
    case XCALL_NoColormap:
      return EmptyColormap(SAVE(nil_value),(Colormap) None);
      
    case XCALL_NoVisual:
      return EmptyVisual(SAVE(nil_value),(Visual *)None);
    
    case XCALL_GetTimeOfDay:
      return GetTimeOfDay();

/* Colorcells 100 */
    case XCALL_XAllocColor:
      return AllocColor(GetDisplay(XP1),GetColormap(XP1),GetXColor1((MLXColor *)P2));
    
    case XCALL_XAllocColorCells:
      return AllocColorCells(GetDisplay(XP1),
                             GetColormap(XP1),
                             get_C_ulong(P2),
                             get_C_ulong(P3),
                             get_C_ulong(P4));
    
    case XCALL_XAllocColorPlanes:
      return AllocColorPlanes(GetDisplay(XP1),
                              GetColormap(XP1),
                              get_C_ulong(P2),
                              get_C_ulong(P3),
                              get_C_ulong(P4),
                              get_C_ulong(P5),
                              get_C_ulong(P6));
    
    case XCALL_XAllocNamedColor:
      return AllocNamedColor(GetDisplay(XP1),GetColormap(XP1),GetString(P2));
    
    case XCALL_XFreeColors:
      FreeColors(GetDisplay(XP1),GetColormap(XP1),SAVE(P2),get_C_ulong(P3));
      break;
    
    case XCALL_XLookupColor:
      return LookupColor(GetDisplay(XP1),GetColormap(XP1),GetString(P2));
    
    case XCALL_XParseColor:
      return ParseColor(GetDisplay(XP1),GetColormap(XP1),GetString(P2));
    
    case XCALL_XQueryColor:
      return QueryColor(GetDisplay(XP1),GetColormap(XP1),get_C_ulong(P2));
    
    case XCALL_XQueryColors:
      return QueryColors(GetDisplay(XP1),GetColormap(XP1),SAVE(P2));
    
    case XCALL_XStoreColor:
      XStoreColor(GetDisplay(XP1),GetColormap(XP1),GetXColor1((MLXColor *)P2));
      break;

    case XCALL_XStoreColors:
      StoreColors(GetDisplay(XP1),GetColormap(XP1),SAVE(P2));
      break;

    case XCALL_XStoreNamedColor:
      StoreNamedColor(GetDisplay(XP1),
                      GetColormap(XP1),
                      GetString(P2),
                      get_C_ulong(P3),
                      get_C_ulong(P4),
                      get_C_ulong(P5),
                      get_C_ulong(P6));
      break;

    case XCALL_BlackPixel:
       { Handle dsHandle /* Handle to (X_Display_Object *) */ = GetDS(XP1); 
         return Make_unsigned(BlackPixel(DEREFDISPLAYHANDLE(dsHandle)->display,
                                         DEREFDISPLAYHANDLE(dsHandle)->screen)); }
               
    case XCALL_WhitePixel:
       { Handle dsHandle /* Handle to (X_Display_Object *) */ = GetDS(XP1);
         return Make_unsigned(WhitePixel(DEREFDISPLAYHANDLE(dsHandle)->display,
                                         DEREFDISPLAYHANDLE(dsHandle)->screen)); }

/* Colormaps 150 */
    case XCALL_XCopyColormapAndFree:
      return EmptyColormap(GetDS(XP1),XCopyColormapAndFree(GetDisplay(XP1),GetColormap(XP1)));
    
    case XCALL_XCreateColormap:
      return EmptyColormap(GetDS(XP1),XCreateColormap(GetDisplay(XP1),GetDrawable(XP1),GetVisual(XP2),get_C_ulong(P3)));

    case XCALL_XInstallColormap:
      XInstallColormap(GetDisplay(XP1),GetColormap(XP1)); break;
    
    case XCALL_XListInstalledColormaps:
      return ListInstalledColormaps(GetDS(XP1),GetDrawable(XP1));
    
    case XCALL_XUninstallColormap:
      XUninstallColormap(GetDisplay(XP1),GetColormap(XP1)); break;

    case XCALL_DefaultColormap:
      { Handle dsHandle /* Handle to (X_Display_Object *) */ = GetDS(XP1);
        return EmptyColormap(dsHandle,
                             DefaultColormap(DEREFDISPLAYHANDLE(dsHandle)->display,
                                             DEREFDISPLAYHANDLE(dsHandle)->screen));
      }
                                                               
    case XCALL_DefaultVisual:
      { Handle dsHandle /* Handle to (X_Display_Object *) */ = GetDS(XP1);
        return EmptyVisual(dsHandle,
                           DefaultVisual(DEREFDISPLAYHANDLE(dsHandle)->display,
                                         DEREFDISPLAYHANDLE(dsHandle)->screen));
      }

    case XCALL_DisplayCells:
      { Handle dsHandle /* Handle to (X_Display_Object *) */ = GetDS(XP1); 
        return Make_unsigned(DisplayCells(DEREFDISPLAYHANDLE(dsHandle)->display,
                                          DEREFDISPLAYHANDLE(dsHandle)->screen));
      }
    
    case XCALL_VisualClass:
      return Make_unsigned(GetVisual(XP1)->class);
      
    case XCALL_VisualRedMask:
      return Make_unsigned(GetVisual(XP1)->red_mask);
      
    case XCALL_VisualGreenMask:
      return Make_unsigned(GetVisual(XP1)->green_mask);
      
    case XCALL_VisualBlueMask:
      return Make_unsigned(GetVisual(XP1)->blue_mask);

/* Cursors 200 */
    case XCALL_XCreateFontCursor:
      return CreateFontCursor(GetDS(XP1),get_C_ulong(P2));
    
    case XCALL_XCreateGlyphCursor:
      return CreateGlyphCursor(GetDS(XP1),
                               GetFont(XP1),
                               GetFont(XP2),
                               get_C_ulong(P3),
                               get_C_ulong(P4),
                               GetXColor1((MLXColor *)P5),
                               GetXColor2((MLXColor *)P6));

    case XCALL_XCreatePixmapCursor:
      return CreatePixmapCursor(GetDS(XP1),
                                GetPixmap(XP1),  /* source     */
                                GetPixmap(XP2),  /* mask       */
                                GetXColor1((MLXColor *)P3), /* foreground */
                                GetXColor2((MLXColor *)P4), /* background */
                                GetOffsetX(P5), /* x          */
                                GetOffsetY(P5)  /* y          */);

    case XCALL_XDefineCursor:
      XDefineCursor(GetDisplay(XP1),GetWindow(XP1),GetCursor(XP2));
      ASSIGN(WindowObject(XP1)->cursor_object,CursorObject(XP2));
      break;

    case XCALL_XQueryBestCursor:
      CheckZeroRect(Rect(P2));
      return QueryBest(XQueryBestCursor,
		       GetDisplay(XP1),
		       GetDrawable(XP1),
		       GetRectW(Rect(P2)),
		       GetRectH(Rect(P2)));

    case XCALL_XRecolorCursor:
      XRecolorCursor(GetDisplay(XP1),
                     GetCursor(XP1),
                     GetXColor1((MLXColor *)P2),
                     GetXColor2((MLXColor *)P3));
      break;

    case XCALL_XUndefineCursor:
      XUndefineCursor(GetDisplay(XP1),GetWindow(XP1));
      ASSIGN(WindowObject(XP1)->cursor_object,0);
      break;

/* Display Specifications 250 */

    case XCALL_XOpenDisplay:
      return OpenDisplay(GetString(XP1)); 

#define DODISPLAYOP(op) \
{\
   Handle dsHandle /* Handle to (X_Display_Object *) */ = GetDS(XP1);\
   return Make_unsigned(op(DEREFDISPLAYHANDLE(dsHandle)->display,\
                           DEREFDISPLAYHANDLE(dsHandle)->screen));\
}

    case XCALL_CellsOfScreen:
      { Handle dsHandle /* Handle to (X_Display_Object *) */ = GetDS(XP1);
        return Make_unsigned(CellsOfScreen(ScreenOfDisplay(DEREFDISPLAYHANDLE(dsHandle)->display,
                                                           DEREFDISPLAYHANDLE(dsHandle)->screen)));
      }
              
    case XCALL_DefaultDepth:
      DODISPLAYOP(DefaultDepth)
      
    case XCALL_DisplayHeight:
      DODISPLAYOP(DisplayHeight)
      
    case XCALL_DisplayHeightMM:
      DODISPLAYOP(DisplayHeightMM)
      
    case XCALL_DisplayPlanes:
      DODISPLAYOP(DisplayPlanes)

    case XCALL_DisplayString:
      { Handle dsHandle /* Handle to (X_Display_Object *) */ = GetDS(XP1);
        return Make_string(DisplayString(DEREFDISPLAYHANDLE(dsHandle)->display));
      }

    case XCALL_DisplayWidth:
      DODISPLAYOP(DisplayWidth)
      
    case XCALL_DisplayWidthMM:
      DODISPLAYOP(DisplayWidthMM)
#undef DODISPLAYOP

    
#define DODISPLAYSCREENOP(op) \
{\
   Handle dsHandle /* Handle to (X_Display_Object *) */ = GetDS(XP1);\
   return Make_unsigned(op(ScreenOfDisplay(DEREFDISPLAYHANDLE(dsHandle)->display,\
                                           DEREFDISPLAYHANDLE(dsHandle)->screen)));\
}

    case XCALL_DoesBackingStore:
      DODISPLAYSCREENOP(DoesBackingStore)
      
    case XCALL_DoesSaveUnders:
      DODISPLAYSCREENOP(DoesSaveUnders)
      
    case XCALL_EventMaskOfScreen:
      DODISPLAYSCREENOP(EventMaskOfScreen)
      
    case XCALL_MaxCmapsOfScreen:
      DODISPLAYSCREENOP(MaxCmapsOfScreen)
      
    case XCALL_MinCmapsOfScreen:
      DODISPLAYSCREENOP(MinCmapsOfScreen)
#undef DODISPLAYSCREENOP

    case XCALL_ProtocolRevision:
      { Handle dsHandle /* Handle to (X_Display_Object *) */ = GetDS(XP1);
        return Make_unsigned(ProtocolRevision(DEREFDISPLAYHANDLE(dsHandle)->display));
      }
      
    case XCALL_ProtocolVersion:
      { Handle dsHandle /* Handle to (X_Display_Object *) */ = GetDS(XP1);
        return Make_unsigned(ProtocolVersion(DEREFDISPLAYHANDLE(dsHandle)->display));
      }
      
    case XCALL_ServerVendor:
      { Handle dsHandle /* Handle to (X_Display_Object *) */ = GetDS(XP1);
        return Make_string  (ServerVendor(DEREFDISPLAYHANDLE(dsHandle)->display));
      }
      
    case XCALL_VendorRelease:
     { Handle dsHandle /* Handle to (X_Display_Object *) */ = GetDS(XP1);
      return Make_unsigned(VendorRelease(DEREFDISPLAYHANDLE(dsHandle)->display));
     }

/* Drawing Primitives 300 */
    case XCALL_XClearArea:
      XClearArea(GetDisplay(XP1),
		 GetWindow(XP1),
		 GetRectX(P2),
		 GetRectY(P2),
		 GetRectW(P2),
		 GetRectH(P2),
		 get_C_ulong(P3));
      break;

    case XCALL_XClearWindow:
      XClearWindow(GetDisplay(XP1),GetWindow(XP1));
      break;

    case XCALL_XCopyArea:
      XCopyArea(GetDisplay(XP1),
		GetDrawable(XP1),
		GetDrawable(XP2),
		GetGC(XP3),
		GetPointX(P4),
		GetPointY(P4),
		GetRectW(P5),
		GetRectH(P5),
		GetRectX(P5),
		GetRectY(P5));
      break;

    case XCALL_XCopyPlane:
      XCopyPlane(GetDisplay(XP1),
		 GetDrawable(XP1),
		 GetDrawable(XP2),
		 GetGC(XP3),
		 GetPointX(P4),
		 GetPointY(P4),
		 GetRectW(P5),
		 GetRectH(P5),
		 GetRectX(P5),
		 GetRectY(P5),
		 get_C_ulong(P6));
      break;

    case XCALL_XDrawArc:
      XDrawArc(GetDisplay(XP1),
	       GetDrawable(XP1),
	       GetGC(XP2),
	       GetRectX(GetArcR(P3)),
	       GetRectY(GetArcR(P3)),
	       GetRectW(GetArcR(P3)),
	       GetRectH(GetArcR(P3)),
	       GetArcA1(P3),
	       GetArcA2(P3));
      break;

    case XCALL_XDrawArcs:
      DrawList(XDrawArcs,
	       GetArcs,
	       sizeof(XArc),
	       GetDisplay(XP1),
	       GetDrawable(XP1),
	       GetGC(XP2),
	       SAVE(P3),
	       0,
	       0);
      break;

    case XCALL_XDrawImageString:
      XDrawImageString(GetDisplay(XP1),
		       GetDrawable(XP1),
		       GetGC(XP2),
		       GetPointX(P3),
		       GetPointY(P3),
		       GetString(P4)->chars,
		       GetString(P4)->length);
      break;

    case XCALL_XDrawImageString16:
      DrawText(XDrawImageString16,
	       GetChar2,
	       (VoidFunc)NULL,
	       sizeof(XChar2b),
	       GetDisplay(XP1),
	       GetDrawable(XP1),
	       GetGC(XP2),
	       GetPointX(P3),
	       GetPointY(P3),
	       SAVE(P4));
      break;

    case XCALL_XDrawLine:
      XDrawLine(GetDisplay(XP1),
		GetDrawable(XP1),
		GetGC(XP2),
		GetPointX(P3),
		GetPointY(P3),
		GetPointX(P4),
		GetPointY(P4));
      break;

    case XCALL_XDrawLines:
      DrawList(XDrawLines,
	       GetPoints,
	       sizeof(XPoint),
	       GetDisplay(XP1),
	       GetDrawable(XP1),
	       GetGC(XP2),
	       SAVE(P3),
	       get_C_ulong(P4),0);
      break;

    case XCALL_XDrawPoint:
      XDrawPoint(GetDisplay(XP1),
		 GetDrawable(XP1),
		 GetGC(XP2),
		 GetPointX(P3),
		 GetPointY(P3));
      break;

    case XCALL_XDrawPoints:
      DrawList(XDrawPoints,
	       GetPoints,
	       sizeof(XPoint),
	       GetDisplay(XP1),
	       GetDrawable(XP1),
	       GetGC(XP2),
	       SAVE(P3),
	       get_C_ulong(P4),0);
      break;

    case XCALL_XDrawRectangle:
      XDrawRectangle(GetDisplay(XP1),
		     GetDrawable(XP1),
		     GetGC(XP2),
		     GetRectX(P3),
		     GetRectY(P3),
		     GetRectW(P3),
		     GetRectH(P3));
      break;

    case XCALL_XDrawRectangles:
      DrawList(XDrawRectangles,
	       GetRects,
	       sizeof(XRectangle),
	       GetDisplay(XP1),
	       GetDrawable(XP1),
	       GetGC(XP2),
	       SAVE(P3),0,0);
      break;

    case XCALL_XDrawSegments:
      DrawList(XDrawSegments,
	       GetSegments,
	       sizeof(XSegment),
	       GetDisplay(XP1),
	       GetDrawable(XP1),
	       GetGC(XP2),
	       SAVE(P3),0,0);
      break;

    case XCALL_XDrawString:
      XDrawString(GetDisplay(XP1),
		  GetDrawable(XP1),
		  GetGC(XP2),
		  GetPointX(P3),
		  GetPointY(P3),
		  GetString(P4)->chars,
		  GetString(P4)->length);
      break;

    case XCALL_XDrawString16:
      DrawText(XDrawString16,
	       GetChar2,
	       (VoidFunc)NULL,
	       sizeof(XChar2b),
	       GetDisplay(XP1),
	       GetDrawable(XP1),
	       GetGC(XP2),
	       GetPointX(P3),
	       GetPointY(P3),
	       SAVE(P4));
      break;

    case XCALL_XDrawText:
      DrawText(XDrawText,
	       GetText,
	       FreeText,
	       sizeof(XTextItem),
	       GetDisplay(XP1),
	       GetDrawable(XP1),
	       GetGC(XP2),
	       GetPointX(P3),
	       GetPointY(P3),
	       SAVE(P4));
      break;

    case XCALL_XDrawText16:
      DrawText(XDrawText16,
	       GetText16,
	       FreeText16,
	       sizeof(XTextItem16),
	       GetDisplay(XP1),
	       GetDrawable(XP1),
	       GetGC(XP2),
	       GetPointX(P3),
	       GetPointY(P3),
	       SAVE(P4));
      break;

    case XCALL_XFillArc:
      XFillArc(GetDisplay(XP1),
	       GetDrawable(XP1),
	       GetGC(XP2),
	       GetRectX(GetArcR(P3)),
	       GetRectY(GetArcR(P3)),
	       GetRectW(GetArcR(P3)),
	       GetRectH(GetArcR(P3)),
	       GetArcA1(P3),
	       GetArcA2(P3));
      break;

    case XCALL_XFillArcs:
      DrawList(XFillArcs,
	       GetArcs,
	       sizeof(XArc),
	       GetDisplay(XP1),
	       GetDrawable(XP1),
	       GetGC(XP2),
	       SAVE(P3),0,0);
      break;

    case XCALL_XFillPolygon:
      DrawList(XFillPolygon,
	       GetPoints,
	       sizeof(XPoint),
	       GetDisplay(XP1),
	       GetDrawable(XP1),
	       GetGC(XP2),
	       SAVE(P3),
	       get_C_ulong(P4),
	       get_C_ulong(P5));
      break;

    case XCALL_XFillRectangle:
      XFillRectangle(GetDisplay(XP1),
		     GetDrawable(XP1),
		     GetGC(XP2),
		     GetRectX(P3),
		     GetRectY(P3),
		     GetRectW(P3),
		     GetRectH(P3));
      break;

    case XCALL_XFillRectangles:
      DrawList(XFillRectangles,
	       GetRects,
	       sizeof(XRectangle),
	       GetDisplay(XP1),
	       GetDrawable(XP1),
	       GetGC(XP2),
	       SAVE(P3),0,0);
      break;

/* Events 350 */

    case XCALL_XSelectInput:
      ASSIGN(*WindowObject(XP1)->eventMask,get_C_ulong(P2));
      XSelectInput(GetDisplay(XP1),GetWindow(XP1),XMASK(*WindowObject(XP1)->eventMask));
      break;

    case XCALL_XSynchronize:
      XSynchronize(GetDisplay(XP1),get_C_ulong(P2));
      break;

    case XCALL_GetState:
      return GetState(WindowObject(XP1)); /* WindowObject added SPF */

    case XCALL_SetState:
      SetState(WindowObject(XP1),P2,P3); /* WindowObject added SPF */
      break;
      
    case XCALL_NextEvent:
      return NextEvent(GetDS(XP1));

    case XCALL_InsertTimeout:
      InsertTimeout(WindowObject(XP1),get_C_ulong(P2),P3,P4); /* WindowObject added SPF */
      break;
    
    case XCALL_XSetInputFocus:
      XSetInputFocus(GetDisplay(XP1),GetWindow(XP2),get_C_ulong(P3),get_C_ulong(P4));
      break;
    
    case XCALL_XGetInputFocus:
      return GetInputFocus(GetDS(XP1));
    
    case XCALL_XSetSelectionOwner:
      SetSelectionOwner(GetDS(XP1),get_C_ulong(P2),GetWindow(XP3),get_C_ulong(P4));
      break;
  
    case XCALL_XGetSelectionOwner:
      { Handle dsHandle /* Handle to (X_Display_Object *) */ = GetDS(XP1);
        return EmptyWindow(dsHandle,XGetSelectionOwner(DEREFDISPLAYHANDLE(dsHandle)->display,
                                                       get_C_ulong(P2)));
      }
  
    case XCALL_XConvertSelection:
      XConvertSelection(GetDisplay(XP4),
			get_C_ulong(P1),
			get_C_ulong(P2),
			get_C_ulong(P3),
			GetWindow(XP4),
			get_C_ulong(P5));
      break;
  
    case XCALL_XSendSelectionNotify:
      SendSelectionNotify(GetDisplay(XP4),
			  get_C_ulong(P1),
			  get_C_ulong(P2),
			  get_C_ulong(P3),
			  GetWindow(XP4),
			  get_C_ulong(P5));
      break;

    case XCALL_XDeleteProperty:
      XDeleteProperty(GetDisplay(XP1),GetWindow(XP1),get_C_ulong(P2));
      break;
    
    case XCALL_XInternAtom:
      return InternAtom(GetDisplay(XP1),GetString(P2),get_C_long(P3));
    
    case XCALL_XGetAtomName:
      return GetAtomName(GetDisplay(XP1),get_C_ulong(P2));

/* Fonts 400 */

    case XCALL_XGetFontPath:
      return GetFontPath(GetDisplay(XP1));
    
    case XCALL_XListFonts:
      return ListFonts(GetDisplay(XP1),GetString(P2),get_C_ulong(P3));
    
    case XCALL_XListFontsWithInfo:
      return ListFontsWithInfo(GetDS(XP1),GetString(P2),get_C_ulong(P3));
    
    case XCALL_XLoadFont:
      return LoadFont(GetDS(XP1),GetString(P2));
    
    case XCALL_XLoadQueryFont:
      return LoadQueryFont(GetDS(XP1),GetString(P2));

    case XCALL_XQueryFont:
      return QueryFont(GetDS(XP1),GetFont(XP1));
    
    case XCALL_XSetFontPath:
      SetFontPath(GetDisplay(XP1),SAVE(P2));
      break;

/* Grabbing 450 */

/* Graphics Context 500 */

    case XCALL_DefaultGC:
      return GetDefaultGC(GetDS(XP1));
    
    case XCALL_UpdateGC:
      ChangeGC(GCObject(XP1),get_C_ulong(P2),P3);
      break;

    case XCALL_XCreateGC:
      return CreateGC(GetDS(XP1),GetDrawable(XP1));

    case XCALL_XSetClipRectangles:
      SetClipRectangles(GetDisplay(XP1),
			GetGC(XP1),
			GetPointX(P2),
			GetPointY(P2),
			SAVE(P3),
			get_C_ulong(P4));
      break;

    case XCALL_XSetDashes:
      SetDashes(GetDisplay(XP1),
		GetGC(XP1),
		get_C_ulong(P2),
		SAVE(P3));
      break;

/* Images 550 */

    case XCALL_XAddPixel:
      AddPixel(GetXImage(GetDisplay(XP1),(MLXImage *)P2),get_C_ulong(P3));
      break;
    
    case XCALL_XGetImage:
      return GetImage(GetDisplay(XP1),
		      GetDrawable(XP1),
		      GetRectX(P2),
		      GetRectY(P2),
		      GetRectW(P2),
		      GetRectH(P2),
		      get_C_ulong(P3),
		      get_C_long(P4)); 

    case XCALL_XGetPixel:
      return GetPixel(GetXImage(GetDisplay(XP1),(MLXImage *)P2),
                      GetPointX(P3),
                      GetPointY(P3));
    
    case XCALL_XGetSubImage:
      GetSubImage(GetDisplay(XP1),
		  GetDrawable(XP1),
		  GetRectX(P2),
		  GetRectY(P2),
		  GetRectW(P2),
		  GetRectH(P2),
		  get_C_ulong(P3),
		  get_C_long(P4),
		  GetXImage(GetDisplay(XP1),(MLXImage *)P5),
		  GetPointX(P6),
		  GetPointY(P6));
      break;

    case XCALL_XPutImage:
      PutImage(GetDisplay(XP1),
	       GetDrawable(XP1),
	       GetGC(XP2),
	       GetXImage(GetDisplay(XP1),(MLXImage *)P3),
	       GetPointX(P4),
	       GetPointY(P4),
	       GetRectX(P5),
	       GetRectY(P5),
	       GetRectW(P5),
	       GetRectH(P5));
      break;

    case XCALL_XPutPixel:
      PutPixel(GetXImage(GetDisplay(XP1),(MLXImage *)P2),
               GetPointX(P3),
               GetPointY(P3),
               get_C_ulong(P4));
      break;
    
    case XCALL_XSubImage:
      return SubImage(GetXImage(GetDisplay(XP1),(MLXImage *)P2),
		      GetRectX(P3),
		      GetRectY(P3),
		      GetRectW(P3),
		      GetRectH(P3));

    case XCALL_BitmapBitOrder:
      return Make_unsigned(MLImageOrder(BitmapBitOrder(GetDisplay(XP1))));
      
    case XCALL_BitmapPad:
      return Make_unsigned(BitmapPad(GetDisplay(XP1)));
      
    case XCALL_BitmapUnit:
      return Make_unsigned(BitmapUnit(GetDisplay(XP1)));
      
    case XCALL_ByteOrder:
      return Make_unsigned(MLImageOrder(ImageByteOrder(GetDisplay(XP1))));

/* Keyboard 600 */
    case XCALL_XLookupString:
      return LookupString(GetDisplay(XP1),get_C_ulong(P2),get_C_ulong(P3));
    
    case XCALL_XQueryKeymap:
      return QueryKeymap(GetDisplay(XP1));
    
    case XCALL_IsCursorKey:
      return Make_bool(IsCursorKey(get_C_ulong(P1)));
      
    case XCALL_IsFunctionKey:
      return Make_bool(IsFunctionKey(get_C_ulong(P1)));
      
    case XCALL_IsKeypadKey:
      return Make_bool(IsKeypadKey(get_C_ulong(P1)));
      
    case XCALL_IsMiscFunctionKey:
      return Make_bool(IsMiscFunctionKey(get_C_ulong(P1)));
      
    case XCALL_IsModifierKey:
      return Make_bool(IsModifierKey(get_C_ulong(P1)));
      
    case XCALL_IsPFKey:
      return Make_bool(IsPFKey(get_C_ulong(P1)));

/* Output Buffer 650 */
    case XCALL_XFlush:
      XFlush(GetDisplay(XP1));
      break;
    
    case XCALL_XSync:
      XSync(GetDisplay(XP1),get_C_ulong(P2));
      break;
    
/* Pointers 700 */
    case XCALL_XQueryPointer:
      return QueryPointer(GetDS(XP1),GetWindow(XP1));

/* Regions 750*/

/* SAVE Set 800 */

/* Screen Saver 850 */
    case XCALL_XActivateScreenSaver:
      XActivateScreenSaver(GetDisplay(XP1));
      break;
    
    case XCALL_XForceScreenSaver:
      XForceScreenSaver(GetDisplay(XP1),get_C_ulong(P2));
      break;
    
    case XCALL_XGetScreenSaver:
      return GetScreenSaver(GetDisplay(XP1));
    
    case XCALL_XResetScreenSaver:
      XResetScreenSaver(GetDisplay(XP1));
      break;
    
    case XCALL_XSetScreenSaver:
      XSetScreenSaver(GetDisplay(XP1),
		      get_C_long(P2),
		      get_C_long(P3),
		      get_C_ulong(P4),
		      get_C_ulong(P5));
      break;

/* Standard Geometry 900 */
    case XCALL_XTranslateCoordinates:
      return TranslateCoordinates(GetDS(XP1),
                                  GetWindow(XP1),
                                  GetWindow(XP2),
                                  GetPointX(P3),
                                  GetPointY(P3));

/* Text 950 */
    case XCALL_XTextExtents:
      return TextExtents(GetFontStruct((MLXFontStruct *)P1),GetString(P2));
      
    case XCALL_XTextExtents16:
      return TextExtents16(GetFontStruct((MLXFontStruct *)P1),SAVE(P2));
      
    case XCALL_XTextWidth:
      return TextWidth(GetFontStruct((MLXFontStruct *)P1),GetString(P2));
    
    case XCALL_XTextWidth16:
      return TextWidth16(GetFontStruct((MLXFontStruct *)P1),SAVE(P2));

/* Tiles, Pixmaps, Stipples and Bitmaps 1000 */
    case XCALL_XCreateBitmapFromData:
    {
      Handle dsHandle /* Handle to (X_Display_Object *) */ = GetDS(XP1);
      CheckZeroRect(P3);
      return EmptyPixmap(dsHandle,
                XCreateBitmapFromData(
                  DEREFDISPLAYHANDLE(dsHandle)->display,
                  GetDrawable(XP1),     /* drawable */
		  GetString(P2)->chars, /* data     */
		  GetRectW(P3),         /* width    */
		  GetRectH(P3)));       /* height   */
    }

    case XCALL_XCreatePixmap:
    {
      Handle dsHandle /* Handle to (X_Display_Object *) */ = GetDS(XP1);
      CheckZeroRect(P2);
      return EmptyPixmap(dsHandle,
              XCreatePixmap(
                DEREFDISPLAYHANDLE(dsHandle)->display,
		GetDrawable(XP1),  /* drawable */
		GetRectW(P2),      /* width    */
		GetRectH(P2),      /* height   */
		get_C_ulong(P3))); /* depth    */
    }

    case XCALL_XCreatePixmapFromBitmapData:
    {
      Handle dsHandle /* Handle to (X_Display_Object *) */ = GetDS(XP1);
      CheckZeroRect(P3);
      
      return EmptyPixmap(dsHandle,
               XCreatePixmapFromBitmapData(
                 DEREFDISPLAYHANDLE(dsHandle)->display,
                 GetDrawable(XP1),     /* drawable */
                 GetString(P2)->chars, /* data     */
                 GetRectW(P3),         /* width    */
                 GetRectH(P3),         /* height   */
                 get_C_ulong(P4),      /* foreground */
                 get_C_ulong(P5),      /* background */
                 get_C_ulong(P6)));    /* depth    */
    }

    case XCALL_XQueryBestStipple:
      CheckZeroRect(P2);
      return QueryBest(XQueryBestStipple,
		       GetDisplay(XP1),
		       GetDrawable(XP1),
		       GetRectW(P2),
		       GetRectH(P2));

    case XCALL_XQueryBestTile:
      CheckZeroRect(P2);
      return QueryBest(XQueryBestTile,
		       GetDisplay(XP1),
		       GetDrawable(XP1),
		       GetRectW(P2),
		       GetRectH(P2));
    
    case XCALL_XReadBitmapFile:
      return ReadBitmap(GetDS(XP1),GetDrawable(XP1),GetString(P2));
    
    case XCALL_XWriteBitmapFile:
      CheckZeroRect(P3);
      return WriteBitmapFile(GetString(XP1),
			     GetDisplay(XP2),
			     GetPixmap(XP2),
			     GetRectW(P3),
			     GetRectH(P3),
			     GetPointX(P4),
			     GetPointY(P4));

/* User Preferences 1050 */
    case XCALL_XAutoRepeatOff:
      XAutoRepeatOff(GetDisplay(XP1));
      break;
      
    case XCALL_XAutoRepeatOn:
      XAutoRepeatOn (GetDisplay(XP1));
      break;
    
    case XCALL_XBell:
      XBell(GetDisplay(XP1),get_C_short(P2));
      break;
    
    case XCALL_XGetDefault:
      return GetDefault(GetDisplay(XP1),GetString(P2),GetString(P3));

/* Window Attributes 1100 */
    case XCALL_ChangeWindow:
      ChangeWindowAttributes(WindowObject(XP1),get_C_ulong(P2),P3);
      break;
    
    case XCALL_XGetGeometry:
      return GetGeometry(GetDS(XP1),GetDrawable(XP1));

    case XCALL_XGetWindowAttributes:
      return GetWindowAttributes(GetDS(XP1),GetDrawable(XP1));
    
    case XCALL_XSetWindowBorderWidth:
      XSetWindowBorderWidth(GetDisplay(XP1),GetWindow(XP1),get_C_ulong(P2));
      break;
    
/* Window Configuration 1150 */
    case XCALL_XCirculateSubwindows:
      XCirculateSubwindows(GetDisplay(XP1),GetWindow(XP1),get_C_ulong(P2));
      break;
    
    case XCALL_XConfigureWindow:
      ConfigureWindow(GetDisplay(XP1),GetWindow(XP1),(word **)P2);
      break;
    
    case XCALL_XLowerWindow:
      XLowerWindow(GetDisplay(XP1),GetWindow(XP1));
      break;
    
    case XCALL_XMapRaised:
      XMapRaised(GetDisplay(XP1),GetWindow(XP1));
      break;

    case XCALL_XMapSubwindows:
      XMapSubwindows(GetDisplay(XP1),GetWindow(XP1));
      break;

    case XCALL_XMapWindow:
      XMapWindow(GetDisplay(XP1),GetWindow(XP1));
      break;

    case XCALL_XMoveResizeWindow:
      CheckZeroRect(P3);
      XMoveResizeWindow(GetDisplay(XP1),
		        GetWindow(XP1),
		        GetPointX(P2),
		        GetPointY(P2), 
		        GetRectW(P3),
		        GetRectH(P3));
      break;

    case XCALL_XMoveWindow:
      XMoveWindow(GetDisplay(XP1),
                  GetWindow(XP1),
                  GetPointX(P2),
                  GetPointY(P2));
      break;

    case XCALL_XQueryTree:
      return QueryTree(GetDS(XP1),GetWindow(XP1));
    
    case XCALL_XRaiseWindow:
      XRaiseWindow(GetDisplay(XP1),GetWindow(XP1));
      break;
    
    case XCALL_XReparentWindow:
      XReparentWindow(GetDisplay(XP1),
                      GetWindow(XP1),
                      GetWindow(XP2),
                      GetPointX(P3),
                      GetPointY(P3));
      break;
    
    case XCALL_XResizeWindow:
      CheckZeroRect(P2);
      XResizeWindow(GetDisplay(XP1),
                    GetWindow(XP1),
                    GetRectW(P2),
                    GetRectH(P2));
      break;

    case XCALL_XRestackWindows:
      RestackWindows(SAVE(P1));
      break;
    
    case XCALL_XUnmapSubwindows:
      XUnmapSubwindows(GetDisplay(XP1),GetWindow(XP1));
      break;

    case XCALL_XUnmapWindow:
      XUnmapWindow(GetDisplay(XP1),GetWindow(XP1));
      break;

/* Window Existence 1200 */
    case XCALL_RootWindow:
      { Handle dsHandle /* Handle to (X_Display_Object *) */ = GetDS(XP1);
        return EmptyWindow(dsHandle,
                 RootWindow(DEREFDISPLAYHANDLE(dsHandle)->display,
                            DEREFDISPLAYHANDLE(dsHandle)->screen));
      }

    case XCALL_DestroyXObject:
      DestroyXObject(XP1);
      break;
    
    case XCALL_XDestroySubwindows:
      DestroySubwindows(XP1);
      break;

    case XCALL_XCreateSimpleWindow:
      CheckZeroRect(P3);
      return CreateSimpleWindow(SAVE(XP1),       /* parent      */
				GetPointX(P2),   /* x           */
				GetPointY(P2),   /* y           */
				GetRectW(P3),    /* w           */
				GetRectH(P3),    /* h           */
				get_C_ulong(P4), /* borderWidth */
				get_C_ulong(P5), /* border      */
				get_C_ulong(P6), /* background  */
				SAVE(P7),        /* handler     */
				SAVE(P8));       /* state       */

    case XCALL_XCreateWindow:
      CheckZeroRect(P3);
      return CreateWindow(SAVE(XP1),       /* parent      */
			  GetPointX(P2),   /* x           */
			  GetPointY(P2),   /* y           */
			  GetRectW(P3),    /* w           */
			  GetRectH(P3),    /* h           */
			  get_C_ulong(P4), /* borderWidth */
			  get_C_ulong(P5), /* depth       */
			  get_C_ulong(P6), /* class       */
			  GetVisual(XP7),  /* visual      */
			  SAVE(P8),        /* handler     */
			  SAVE(P9));       /* state       */

/* Window Manager 1250 */
    case XCALL_XSetProperty:
      SetProperty(GetDisplay(XP1),
                  GetWindow(XP1),
                  get_C_ulong(P2),
                  get_C_ulong(P3),
                  SAVE(P4),
                  get_C_ulong(P5));
      break;

    case XCALL_XGetTextProperty:
      return GetTextProperty(GetDisplay(XP1),GetWindow(XP1),get_C_ulong(P2));
    
    case XCALL_XGetWMHints:
      return GetWMHints(GetDS(XP1),GetWindow(XP1));

    case XCALL_XGetWMSizeHints:
      return GetWMSizeHints(GetDisplay(XP1),GetWindow(XP1),get_C_ulong(P2));
    
    case XCALL_XGetIconSizes:
      return GetIconSizes(GetDisplay(XP1),GetWindow(XP1));
    
    case XCALL_XGetTransientForHint:
      return GetTransientForHint(GetDS(XP1),GetWindow(XP1));
    
    case XCALL_XGetWMColormapWindows:
      return GetWMColormapWindows(GetDS(XP1),GetWindow(XP1));
    
    case XCALL_XGetRGBColormaps:
      return GetRGBColormaps(GetDS(XP1),GetWindow(XP1),get_C_ulong(P2));
    
    case XCALL_XWMGeometry:
      return WMGeometry(GetDS(XP1),
                        GetString(P2),
                        GetString(P3),
                        get_C_ulong(P4),
                        (MLXWMSizeHintsTuple *)P5);

/* Miscellaneous 1300 */
    case XCALL_GetID:
      return GetID(XP1);
      
    case XCALL_ResourceExists:
      return Make_bool(ResourceExists(XP1));
      
    case XCALL_GetDisplay:
      return GetDS(XP1);

/******************************************************************************/
/*                                                                            */
/*      Xt Calls                                                              */
/*                                                                            */
/******************************************************************************/
    case XCALL_NoWidget:
      return EmptyWidget(SAVE(nil_value), (Widget)NULL);

    case XCALL_AppInitialise:
      return AppInitialise((String *)P1, /* display name      */
			   (String *)P2, /* application name  */
			   (String *)P3, /* application class */
			   SAVE(P4),     /* Fallback list     */
			   SAVE(P5)      /* Arg list          */);
    
    case XCALL_XtRealizeWidget:
      XtRealizeWidget(GetWidget(XP1));
      break;

    case XCALL_XtManageChildren:
      ManageChildren(SAVE(P1));
      break;
    
    case XCALL_XtUnmanageChildren:
      UnmanageChildren(SAVE(P1));
      break;
    
    case XCALL_XtDestroyWidget:
      {
        Widget w = GetWidget(XP1);
	XtDestroyWidget(w);
       /* The following test seems necessary - sometimes the callback from  */
       /* the above call destroys the widget, sometimes it doesn't. I think */
       /* it always should, and I can't work out why this strange behaviour */
       /* occurs.                                               SPF 9/12/93 */
       if (ResourceExists(XP1)) 
       {
	 DestroyXObject(XP1);
	 PurgeCCallbacks((X_Widget_Object *)XP1,w);
       }
       break;
      }
    
    case XCALL_SetCallbacks:
      SetCallbacks (WidgetObject(XP1),(ML_Cons_Cell *)P2,P3);
      break; /* WidgetObject added SPF */
    
    case XCALL_XtSetValues:
      SetValues(GetWidget(XP1),SAVE(P2));
      break;
    
    case XCALL_GetValue:
      return GetValue(GetDS(XP1),GetWidget(XP1),(MLPair *)P2);
    
    case XCALL_XtParent:
      return EmptyWidget(GetDS(XP1),XtParent(GetWidget(XP1)));
    
    case XCALL_XtWindow:
      return EmptyWindow(GetDS(XP1),WindowOfWidget(GetWidget(XP1)));
    
    case XCALL_XtDisplay:
      return GetDS(XP1);
    
    case XCALL_XtUnrealizeWidget:
      XtUnrealizeWidget(GetWidget(XP1)); break;
    
    case XCALL_XtName:
      return Make_string(XtName(GetWidget(XP1)));
    
    case XCALL_XtParseTranslationTable:
      return ParseTranslationTable(GetString(XP1));
    
    case XCALL_XtOverrideTranslations:
      XtOverrideTranslations(GetWidget(XP1),GetTrans(XP2));
      break;
    
    case XCALL_XtAugmentTranslations:
      XtAugmentTranslations(GetWidget(XP1),GetTrans(XP2));
      break;
    
    case XCALL_XtUninstallTranslations: XtUninstallTranslations(GetWidget(XP1)); break;
    
/*
    case XCALL_XtTranslateTablePrint: _XtTranslateTablePrint(GetTrans(XP1)); break;
*/
    
    case XCALL_XtCreatePopupShell:
      return CreatePopupShell(GetString(XP1),GetDS(XP2),GetWidget(XP2),SAVE(P3));
    
    case XCALL_InsertWidgetTimeout:
      InsertWidgetTimeout(WidgetObject(XP1),get_C_ulong(P2),P3,P4);
      break; /* WidgetObject added SPF */
    
    case XCALL_GetWidgetState:
      return SAVE(WidgetObjectToken(XP1)->state); /* was WidgetObject(XP1) (SPF) */
    
    case XCALL_SetWidgetState:
      ASSIGN(WidgetObjectToken(XP1)->state,P2);
      break;  /* was WidgetObject(XP1) (SPF) */
    
    case XCALL_XtSetSensitive:
      XtSetSensitive(GetWidget(XP1),get_C_ulong(P2));
      break;
    
    case XCALL_XtIsSensitive:
      return Make_bool(XtIsSensitive(GetWidget(XP1)));
    
    case XCALL_GetSubresources:
      return GetSubresources(GetDS(XP1),
                             GetWidget(XP1),
                             GetString(P2),
                             GetString(P3),
                             SAVE(P4));
    
    case XCALL_Cast:
      return SAVE(P1);

    case XCALL_XtPopup:
      XtPopup(GetWidget(XP1),GetXtGrabKind(P2));
      break;
      
    case XCALL_XtPopdown:
      XtPopdown(GetWidget(XP1));
      break;
      
    case XCALL_XtMapWidget:
      XtMapWidget(GetRealizedWidget("XtMapWidget",XP1));
      break;
      
    case XCALL_XtUnmapWidget:
      XtUnmapWidget(GetRealizedWidget("XtUnmapWidget",XP1));
      break;
      
    case XCALL_XtIsManaged:
      return Make_bool(XtIsManaged(GetWidget(XP1)));
    
    case XCALL_XtIsRealized:
      return Make_bool(XtIsRealized(GetWidget(XP1)));
    
    /* Added DCJM. */
    case XCALL_XtGetApplicationResources:
		return GetApplicationResources ( GetDS(XP1),GetWidget(XP1),SAVE(P2) ) ;

    case XCALL_XtAddEventHandler:
		AddEventhandler (WidgetObject(XP1), get_C_ulong(P2),
				 get_C_ulong(P3), SAVE(P4)); break;


/******************************************************************************/
/*                                                                            */
/*      Motif Calls - widget creation                                         */
/*                                                                            */
/******************************************************************************/
/* Motif 4000 */

#define XMCREATE(number,name) \
    case number: return CreateXm(name, \
                                  #name " failed", \
                                   GetDS(XP1), \
                                   GetWidget(XP1), \
                                   GetString(P2), \
                                   SAVE(P3))

    XMCREATE(XCALL_XmCreateArrowButton,XmCreateArrowButton);
    XMCREATE(XCALL_XmCreateArrowButtonGadget,XmCreateArrowButtonGadget);
    XMCREATE(XCALL_XmCreateBulletinBoard,XmCreateBulletinBoard);
    XMCREATE(XCALL_XmCreateBulletinBoardDialog,XmCreateBulletinBoardDialog);
    XMCREATE(XCALL_XmCreateCascadeButton,XmCreateCascadeButton);
    XMCREATE(XCALL_XmCreateCascadeButtonGadget,XmCreateCascadeButtonGadget);
    XMCREATE(XCALL_XmCreateCommand,XmCreateCommand);
    XMCREATE(XCALL_XmCreateDialogShell,XmCreateDialogShell);
    XMCREATE(XCALL_XmCreateDrawingArea,XmCreateDrawingArea);
    XMCREATE(XCALL_XmCreateDrawnButton,XmCreateDrawnButton);
    XMCREATE(XCALL_XmCreateErrorDialog,XmCreateErrorDialog);
    XMCREATE(XCALL_XmCreateFileSelectionBox,XmCreateFileSelectionBox);
    XMCREATE(XCALL_XmCreateFileSelectionDialog,XmCreateFileSelectionDialog);
    XMCREATE(XCALL_XmCreateForm,XmCreateForm);
    XMCREATE(XCALL_XmCreateFormDialog,XmCreateFormDialog);
    XMCREATE(XCALL_XmCreateFrame,XmCreateFrame);
    XMCREATE(XCALL_XmCreateInformationDialog,XmCreateInformationDialog);
    XMCREATE(XCALL_XmCreateLabel,XmCreateLabel);
    XMCREATE(XCALL_XmCreateLabelGadget,XmCreateLabelGadget);
    XMCREATE(XCALL_XmCreateList,XmCreateList);
    XMCREATE(XCALL_XmCreateMainWindow,XmCreateMainWindow);
    XMCREATE(XCALL_XmCreateMenuBar,XmCreateMenuBar);
    XMCREATE(XCALL_XmCreateMenuShell,XmCreateMenuShell);
    XMCREATE(XCALL_XmCreateMessageBox,XmCreateMessageBox);
    XMCREATE(XCALL_XmCreateMessageDialog,XmCreateMessageDialog);
    XMCREATE(XCALL_XmCreateOptionMenu,XmCreateOptionMenu);
    XMCREATE(XCALL_XmCreatePanedWindow,XmCreatePanedWindow);
    XMCREATE(XCALL_XmCreatePopupMenu,XmCreatePopupMenu);
    XMCREATE(XCALL_XmCreatePromptDialog,XmCreatePromptDialog);
    XMCREATE(XCALL_XmCreatePulldownMenu,XmCreatePulldownMenu);
    XMCREATE(XCALL_XmCreatePushButton,XmCreatePushButton);
    XMCREATE(XCALL_XmCreatePushButtonGadget,XmCreatePushButtonGadget);
    XMCREATE(XCALL_XmCreateQuestionDialog,XmCreateQuestionDialog);
    XMCREATE(XCALL_XmCreateRadioBox,XmCreateRadioBox);
    XMCREATE(XCALL_XmCreateRowColumn,XmCreateRowColumn);
    XMCREATE(XCALL_XmCreateScale,XmCreateScale);
    XMCREATE(XCALL_XmCreateScrollBar,XmCreateScrollBar);
    XMCREATE(XCALL_XmCreateScrolledList,XmCreateScrolledList);
    XMCREATE(XCALL_XmCreateScrolledText,XmCreateScrolledText);
    XMCREATE(XCALL_XmCreateScrolledWindow,XmCreateScrolledWindow);
    XMCREATE(XCALL_XmCreateSelectionBox,XmCreateSelectionBox);
    XMCREATE(XCALL_XmCreateSelectionDialog,XmCreateSelectionDialog);
    XMCREATE(XCALL_XmCreateSeparator,XmCreateSeparator);
    XMCREATE(XCALL_XmCreateSeparatorGadget,XmCreateSeparatorGadget);
    XMCREATE(XCALL_XmCreateSimpleCheckBox,XmCreateSimpleCheckBox);
    XMCREATE(XCALL_XmCreateSimpleMenuBar,XmCreateSimpleMenuBar);
    XMCREATE(XCALL_XmCreateSimpleOptionMenu,XmCreateSimpleOptionMenu);
    XMCREATE(XCALL_XmCreateSimplePopupMenu,XmCreateSimplePopupMenu);
    XMCREATE(XCALL_XmCreateSimplePulldownMenu,XmCreateSimplePulldownMenu);
    XMCREATE(XCALL_XmCreateSimpleRadioBox,XmCreateSimpleRadioBox);
    XMCREATE(XCALL_XmCreateText,XmCreateText);
    XMCREATE(XCALL_XmCreateTextField,XmCreateTextField);
    XMCREATE(XCALL_XmCreateToggleButton,XmCreateToggleButton);
    XMCREATE(XCALL_XmCreateToggleButtonGadget,XmCreateToggleButtonGadget);
    XMCREATE(XCALL_XmCreateWarningDialog,XmCreateWarningDialog);
    XMCREATE(XCALL_XmCreateWorkArea,XmCreateWorkArea);
    XMCREATE(XCALL_XmCreateWorkingDialog,XmCreateWorkingDialog);
    
#undef XMCREATE

/******************************************************************************/
/*                                                                            */
/*      Motif Calls - miscellaneous                                           */
/*                                                                            */
/******************************************************************************/
    case XCALL_XmCascadeButtonHighlight:
      XmCascadeButtonHighlight(GetWidget(XP1),get_C_ulong(P2));
      break;
    
    case XCALL_XmCommandError:
      CommandError(GetWidget(XP1),P2);
      break;
    
    case XCALL_XmCommandGetChild:
      return EmptyWidget(GetDS(XP1),
               XmCommandGetChild(GetWidget(XP1),get_C_ulong(P2)));

    case XCALL_XmFileSelectionBoxGetChild:
      return EmptyWidget(GetDS(XP1),
               XmFileSelectionBoxGetChild(GetWidget(XP1),get_C_ulong(P2)));

    case XCALL_XmFileSelectionDoSearch:
      FileSelectionDoSearch(GetWidget(XP1),P2);
      break;

    case XCALL_XmIsSomething:
      return XmIsSomething(get_C_ulong(P1),GetWidget(XP2));

    case XCALL_XmMainWindowSetAreas:
      XmMainWindowSetAreas(GetWidget(XP1),
			   GetNWidget(XP2),
			   GetNWidget(XP3),
			   GetNWidget(XP4),
			   GetNWidget(XP5),
			   GetNWidget(XP6));
      break;

    case XCALL_XmMainWindowSepX:
      switch(get_C_ulong(P2))
        {
         case 1:
           return EmptyWidget(GetDS(XP1),XmMainWindowSep1(GetWidget(XP1)));
           
         case 2:
           return EmptyWidget(GetDS(XP1),XmMainWindowSep2(GetWidget(XP1)));
           
         default:
           return EmptyWidget(GetDS(XP1),XmMainWindowSep3(GetWidget(XP1)));
         }

    case XCALL_XmMessageBoxGetChild:
      return EmptyWidget(GetDS(XP1),
               XmMessageBoxGetChild(GetWidget(XP1),get_C_ulong(P2)));

    case XCALL_XmOptionButtonGadget:
      return EmptyWidget(GetDS(XP1),XmOptionButtonGadget(GetWidget(XP1)));
      
    case XCALL_XmOptionLabelGadget:
      return EmptyWidget(GetDS(XP1),XmOptionLabelGadget (GetWidget(XP1)));

    case XCALL_XmSelectionBoxGetChild:
      return EmptyWidget(GetDS(XP1),
               XmSelectionBoxGetChild(GetWidget(XP1),get_C_ulong(P2)));
    
    case XCALL_XmSetMenuCursor:
      XmSetMenuCursor(GetDisplay(XP1),GetCursor(XP2)); break;

    case XCALL_XmScrolledWindowSetAreas:
      XmScrolledWindowSetAreas(GetWidget(XP1),
                               GetNWidget(XP2),
                               GetNWidget(XP3),
                               GetNWidget(XP4));
      break;


/******************************************************************************/
/*                                                                            */
/*      Operations on XmText widgets                                          */
/*                                                                            */
/******************************************************************************/

#define TextWidgetToLong(func) \
case XCALL_ ## func : \
  return(WidgetToLong(#func,GetTextWidget,func,XP1))
  
#define TextWidgetToInt(func) \
case XCALL_ ## func : \
  return(WidgetToInt(#func,GetTextWidget,func,XP1))
  
#define TextWidgetToBool(func) \
case XCALL_ ## func : \
  return(WidgetToBool(#func,GetTextWidget,func,XP1))
  
#define TextWidgetToString(func) \
case XCALL_ ## func : \
  return(WidgetToString(#func,GetTextWidget,func,XP1))
  
#define TextWidgetIntAction(func) \
case XCALL_ ## func : \
  WidgetIntAction(#func,GetTextWidget,func,XP1,P2); \
  break

#define TextWidgetLongAction(func) \
case XCALL_ ## func : \
  WidgetLongAction(#func,GetTextWidget,func,XP1,P2); \
  break

#define TextWidgetBoolAction(func) \
case XCALL_ ## func : \
  WidgetBoolAction(#func,GetTextWidget,func,XP1,P2); \
  break


/* XmTextClearSelection not supported */
/* XmTextCopy not supported */
/* XmTextCut not supported */
#ifdef LESSTIF_VERSION
/* This is not supported in LessTif, at least not 0.89. */
    case XCALL_XmTextGetAddMode:
	RaiseXWindows("XmTextGetAddMode: not implemented");
#else
    TextWidgetToBool(XmTextGetAddMode);
#endif
    TextWidgetToLong(XmTextGetCursorPosition);
    TextWidgetToInt(XmTextGetBaseline);
    TextWidgetToBool(XmTextGetEditable);
    TextWidgetToLong(XmTextGetInsertionPosition);
    TextWidgetToLong(XmTextGetLastPosition);
    TextWidgetToInt(XmTextGetMaxLength);
    TextWidgetToString(XmTextGetSelection);
/* XmTextGetSelectionPosition not supported */
    TextWidgetToString(XmTextGetString);
/* XmTextGetSource not supported */
    TextWidgetToLong(XmTextGetTopCharacter);

    case XCALL_XmTextInsert:
      {
        Widget w = GetTextWidget("XmTextInsert",XP1);
	{
	  unsigned pos = get_C_ulong(P2);
          String *s    = GetString(P3);
	  int   size   = s->length + 1;
	  char *buffer = (char *)alloca(size);
	
	  Poly_String_To_C(s,buffer,size);
	  XmTextInsert(w,pos,buffer);
          break;
	}
      }
      
    TextWidgetToBool(XmTextPaste); /* with side effect! */
/* XmTextPosToXY not supported */
    TextWidgetToBool(XmTextRemove); /* with side effect! */

    case XCALL_XmTextReplace:
      {
        Widget w = GetTextWidget("XmTextReplace",XP1);
	{
          unsigned from_pos = get_C_ulong(P2);
          unsigned to_pos   = get_C_ulong(P3);
          String *s    = GetString(P4);
	  int   size   = s->length + 1;
	  char *buffer = (char *)alloca(size);
	
	  Poly_String_To_C(s,buffer,size);
	  XmTextReplace(w,from_pos,to_pos,buffer);
          break;
	}
      }
      
    TextWidgetIntAction(XmTextScroll); /* for side effect! */
    TextWidgetBoolAction(XmTextSetAddMode);
    TextWidgetLongAction(XmTextSetCursorPosition);
    TextWidgetBoolAction(XmTextSetEditable);
/* XmTextSetHighlight not supported */
    TextWidgetLongAction(XmTextSetInsertionPosition);
    TextWidgetIntAction(XmTextSetMaxLength);
/* XmTextSetSelection not supported */
/* XmTextSetSource not supported */


/* inlined SPF 15/2/94 */
    case XCALL_XmTextSetString:
      {
        Widget w = GetTextWidget("XmTextSetString",XP1);
	{
          String *s    = GetString(P2);
	  int   size   = s->length + 1;
	  char *buffer = (char *)alloca(size);
	
	  Poly_String_To_C(s,buffer,size);
	  XmTextSetString(w,buffer);
          break;
	}
      }

    TextWidgetLongAction(XmTextSetTopCharacter);
    TextWidgetLongAction(XmTextShowPosition);

    case XCALL_XmTextXYToPos:
      {
        Widget w = GetTextWidget("XmTextXYToPos",XP1);
	{
          int x = get_C_long(P2);
          int y = get_C_long(P3);
	  return Make_int(XmTextXYToPos(w,x,y));
	}
      }

#undef TextWidgetToLong
#undef TextWidgetToInt
#undef TextWidgetToBool
#undef TextWidgetToString
#undef TextWidgetIntAction
#undef TextWidgetBoolAction

/******************************************************************************/
/*                                                                            */
/*      Operations on XmTextField widgets                                     */
/*                                                                            */
/******************************************************************************/

#define TextFieldWidgetToLong(func) \
case XCALL_ ## func : \
  return(WidgetToLong(#func,GetTextFieldWidget,func,XP1))

  
#define TextFieldWidgetToInt(func) \
case XCALL_ ## func : \
  return(WidgetToInt(#func,GetTextFieldWidget,func,XP1))

#define TextFieldWidgetToBool(func) \
case XCALL_ ## func : \
  return(WidgetToBool(#func,GetTextFieldWidget,func,XP1))
  
#define TextFieldWidgetToString(func) \
case XCALL_ ## func : \
  return(WidgetToString(#func,GetTextFieldWidget,func,XP1))
  
#define TextFieldWidgetIntAction(func) \
case XCALL_ ## func : \
  WidgetIntAction(#func,GetTextFieldWidget,func,XP1,P2); \
  break

#define TextFieldWidgetLongAction(func) \
case XCALL_ ## func : \
  WidgetLongAction(#func,GetTextFieldWidget,func,XP1,P2); \
  break

#define TextFieldWidgetBoolAction(func) \
case XCALL_ ## func : \
  WidgetBoolAction(#func,GetTextFieldWidget,func,XP1,P2); \
  break


/* XmTextFieldClearSelection not supported */
/* XmTextFieldCopy not supported */
/* XmTextFieldCut not supported */
#ifdef LESSTIF_VERSION
	/* This is not supported in LessTif, at least not 0.89. */
    case XCALL_XmTextFieldGetAddMode:
	RaiseXWindows("XmTextFieldGetAddMode: not implemented");
#else
    TextFieldWidgetToBool(XmTextFieldGetAddMode);
#endif
    TextFieldWidgetToInt(XmTextFieldGetBaseline);
    TextFieldWidgetToLong(XmTextFieldGetCursorPosition);
    TextFieldWidgetToBool(XmTextFieldGetEditable);
    TextFieldWidgetToLong(XmTextFieldGetInsertionPosition);
    TextFieldWidgetToLong(XmTextFieldGetLastPosition);
    TextFieldWidgetToInt(XmTextFieldGetMaxLength);
    TextFieldWidgetToString(XmTextFieldGetSelection);
/* XmTextFieldGetSelectionPosition not supported */
    TextFieldWidgetToString(XmTextFieldGetString);
/* XmTextFieldGetSource not supported */

    case XCALL_XmTextFieldInsert:
      {
        Widget w = GetTextFieldWidget("XmTextFieldInsert",XP1);
	{
	  unsigned pos = get_C_ulong(P2);
          String *s    = GetString(P3);
	  int   size   = s->length + 1;
	  char *buffer = (char *)alloca(size);
	
	  Poly_String_To_C(s,buffer,size);
	  XmTextFieldInsert(w,pos,buffer);
          break;
	}
      }
      
    TextFieldWidgetToBool(XmTextFieldPaste); /* for side effect! */
/* XmTextFieldPosToXY not supported */
    TextFieldWidgetToBool(XmTextFieldRemove); /* for side effect! */

    case XCALL_XmTextFieldReplace:
      {
        Widget w = GetTextFieldWidget("XmTextFieldReplace",XP1);
	{
          unsigned from_pos = get_C_ulong(P2);
          unsigned to_pos   = get_C_ulong(P3);
          String *s    = GetString(P4);
	  int   size   = s->length + 1;
	  char *buffer = (char *)alloca(size);
	
	  Poly_String_To_C(s,buffer,size);
	  XmTextFieldReplace(w,from_pos,to_pos,buffer);
          break;
	}
      }
      
    TextFieldWidgetBoolAction(XmTextFieldSetAddMode);
    TextFieldWidgetLongAction(XmTextFieldSetCursorPosition);
    TextFieldWidgetBoolAction(XmTextFieldSetEditable);
/* XmTextFieldSetHighlight not supported */
    TextFieldWidgetLongAction(XmTextFieldSetInsertionPosition);
    TextFieldWidgetIntAction(XmTextFieldSetMaxLength);
/* XmTextFieldSetSelection not supported */


/* inlined SPF 15/2/94 */
    case XCALL_XmTextFieldSetString:
      {
        Widget w = GetTextFieldWidget("XmTextFieldSetString",XP1);
	{
          String *s    = GetString(P2);
	  int   size   = s->length + 1;
	  char *buffer = (char *)alloca(size);
	
	  Poly_String_To_C(s,buffer,size);
	  XmTextFieldSetString(w,buffer);
          break;
	}
      }

    TextFieldWidgetLongAction(XmTextFieldShowPosition);  /* for side effect! */

    case XCALL_XmTextFieldXYToPos:
      {
        Widget w = GetTextFieldWidget("XmTextFieldXYToPos",XP1);
	{
          int x = get_C_long(P2);
          int y = get_C_long(P3);
	  return Make_int(XmTextFieldXYToPos(w,x,y));
	}
      }

    case XCALL_XmTrackingLocate:
      return EmptyWidget(GetDS(XP1),
               XmTrackingLocate(GetWidget(XP1),GetCursor(XP2),get_C_ulong(P3)));
    
    case XCALL_XmUpdateDisplay:
      XmUpdateDisplay(GetWidget(XP1));
      break;
   
#undef TextFieldWidgetToLong
#undef TextFieldWidgetToInt
#undef TextFieldWidgetToBool
#undef TextFieldWidgetToString
#undef TextFieldWidgetIntAction
#undef TextFieldWidgetLongAction
#undef TextFieldWidgetBoolAction

/******************************************************************************/
/*                                                                            */
/*      Operations on XmList widgets                                          */
/*                                                                            */
/******************************************************************************/

#define ListWidgetAction(func) \
case XCALL_ ## func : \
  WidgetAction(#func,GetListWidget,func,XP1); \
  break

#define ListWidgetBoolAction(func) \
case XCALL_ ## func : \
  WidgetBoolAction(#func,GetListWidget,func,XP1,P2); \
  break

#define ListWidgetXmstringAction(func) \
case XCALL_ ## func : \
  WidgetXmstringAction(#func,GetListWidget,func,XP1,P2); \
  break

#define ListWidgetXmstringlistAction(func) \
case XCALL_ ## func : \
  WidgetXmstringlistAction(#func,GetListWidget,func,XP1,(ML_Cons_Cell *)P2); \
  break

#define ListWidgetIntAction(func) \
case XCALL_ ## func : \
  WidgetIntAction(#func,GetListWidget,func,XP1,P2); \
  break

#define ListWidgetIntIntAction(func) \
case XCALL_ ## func : \
  WidgetIntIntAction(#func,GetListWidget,func,XP1,P2,P3); \
  break

#define ListWidgetXmstringIntAction(func) \
case XCALL_ ## func : \
  WidgetXmstringIntAction(#func,GetListWidget,func,XP1,P2,P3); \
  break

#define ListWidgetIntBoolAction(func) \
case XCALL_ ## func : \
  WidgetIntBoolAction(#func,GetListWidget,func,XP1,P2,P3); \
  break

#define ListWidgetXmstringBoolAction(func) \
case XCALL_ ## func : \
  WidgetXmstringBoolAction(#func,GetListWidget,func,XP1,P2,P3); \
  break

#define ListWidgetXmstringlistIntAction(func) \
case XCALL_ ## func : \
  WidgetXmstringlistIntAction(#func,GetListWidget,func,XP1,(ML_Cons_Cell *)P2,P3); \
  break

#define ListWidgetXmstringToIntlist(func) \
case XCALL_ ## func : \
  return(WidgetXmstringToIntlist(#func,GetListWidget,func,XP1,P2))

#define ListWidgetToIntlist(func) \
case XCALL_ ## func : \
  return(WidgetToIntlist(#func,GetListWidget,func,XP1))

#define ListWidgetXmstringToBool(func) \
case XCALL_ ## func : \
  return(WidgetXmstringToBool(#func,GetListWidget,func,XP1,P2))

#define ListWidgetXmstringToInt(func) \
case XCALL_ ## func : \
  return(WidgetXmstringToInt(#func,GetListWidget,func,XP1,P2))

/************************* Adding Items to List *******************************/
    ListWidgetXmstringIntAction(XmListAddItem);
    ListWidgetXmstringIntAction(XmListAddItemUnselected);
    ListWidgetXmstringlistIntAction(XmListAddItems); 

/************************* Deleting Items from List ***************************/
    ListWidgetAction(XmListDeleteAllItems);
    ListWidgetXmstringAction(XmListDeleteItem);
    ListWidgetXmstringlistAction(XmListDeleteItems);
    ListWidgetIntAction(XmListDeletePos);
    ListWidgetIntIntAction(XmListDeleteItemsPos);

/************************* Deselecting Items **********************************/
    ListWidgetAction(XmListDeselectAllItems);
    ListWidgetXmstringAction(XmListDeselectItem);
    ListWidgetIntAction(XmListDeselectPos);


/************************* Query Functions ************************************/
    ListWidgetXmstringToIntlist(XmListGetMatchPos);
    ListWidgetToIntlist(XmListGetSelectedPos);
    ListWidgetXmstringToBool(XmListItemExists);
    ListWidgetXmstringToInt(XmListItemPos);

/************************* Replacing Items in the List ************************/
    case XCALL_XmListReplaceItems: 
      /* Unpairing the strings is done in the ML, because it's easier there. */
      {
	Widget w = GetListWidget("XmListReplaceItems",XP1);
	int n    = ListLength((ML_Cons_Cell *)P2);
	int n2   = ListLength((ML_Cons_Cell *)P3);
	
	if (n != n2) 
	  {
	    RaiseXWindows("XmListReplaceItems: strings lists are different lengths");
	  }
	else
	  {
	    XmString *oldstrings = (XmString *)alloca(n * sizeof(XmString));
	    XmString *newstrings = (XmString *)alloca(n * sizeof(XmString));
	    
	    GetList4(P2,oldstrings,sizeof(XmString),GetXmString);
	    GetList4(P3,newstrings,sizeof(XmString),GetXmString);
	    XmListReplaceItems(w,oldstrings,n,newstrings);
            {int i; for (i = 0; i < n; i ++) XmStringFree(oldstrings[i]);}
            {int i; for (i = 0; i < n; i ++) XmStringFree(newstrings[i]);}
          }
	 break;
      }

    ListWidgetXmstringlistIntAction(XmListReplaceItemsPos);

/************************* Selecting Items in the List ************************/
    ListWidgetXmstringBoolAction(XmListSelectItem);
    ListWidgetIntBoolAction(XmListSelectPos);

/************************* Set Add Mode ***************************************/
    ListWidgetBoolAction(XmListSetAddMode);

/************************* Set Appearance *************************************/
    ListWidgetXmstringAction(XmListSetBottomItem); 
    ListWidgetIntAction(XmListSetBottomPos);
    ListWidgetIntAction(XmListSetHorizPos);
    ListWidgetXmstringAction(XmListSetItem); 
    ListWidgetIntAction(XmListSetPos);
  
#undef ListWidgetAction
#undef ListWidgetBoolAction
#undef ListWidgetXmstringAction
#undef ListWidgetXmstringlistAction
#undef ListWidgetIntAction
#undef ListWidgetIntIntAction
#undef ListWidgetXmstringIntAction
#undef ListWidgetXmstringBoolAction
#undef ListWidgetXmstringlistIntAction
#undef ListWidgetXmstringToIntlist
#undef ListWidgetToIntlist
#undef ListWidgetXmstringToBool
#undef ListWidgetXmstringToInt


     /* Calls added by DCJM. */
    case XCALL_XmMenuPosition:
		MenuPosition( GetWidget(XP1), get_C_ulong(P2), get_C_ulong(P3)); break; 
/******************************************************************************/
/*                                                                            */
/*      Default case                                                          */
/*                                                                            */
/******************************************************************************/

    default: Crash ("Unimplemented X Windows call %d",code);
  }

  return Make_bool(False);
}

typedef struct
{
  int   code;
  char *name;
} CodeName;

static CodeName ProtocolNames[] =
{
  { X_CreateWindow,"XCreateWindow"},
  { X_ChangeWindowAttributes,"XChangeWindowAttributes"},
  { X_GetWindowAttributes,"XGetWindowAttributes"},
  { X_DestroyWindow,"XDestroyWindow"},
  { X_DestroySubwindows,"XDestroySubwindows"},
  { X_ChangeSaveSet,"XChangeSAVESet"},
  { X_ReparentWindow,"XReparentWindow"},
  { X_MapWindow,"XMapWindow"},
  { X_MapSubwindows,"XMapSubwindows"},
  { X_UnmapWindow,"XUnmapWindow"},
  { X_UnmapSubwindows,"XUnmapSubwindows"},
  { X_ConfigureWindow,"XConfigureWindow"},
  { X_CirculateWindow,"XCirculateWindow"},
  { X_GetGeometry,"XGetGeometry"},
  { X_QueryTree,"XQueryTree"},
  { X_InternAtom,"XInternAtom"},
  { X_GetAtomName,"XGetAtomName"},
  { X_ChangeProperty,"XChangeProperty"},
  { X_DeleteProperty,"XDeleteProperty"},
  { X_GetProperty,"XGetProperty"},
  { X_ListProperties,"XListProperties"},
  { X_SetSelectionOwner,"XSetSelectionOwner"},
  { X_GetSelectionOwner,"XGetSelectionOwner"},
  { X_ConvertSelection,"XConvertSelection"},
  { X_SendEvent,"XSendEvent"},
  { X_GrabPointer,"XGrabPointer"},
  { X_UngrabPointer,"XUngrabPointer"},
  { X_GrabButton,"XGrabButton"},
  { X_UngrabButton,"XUngrabButton"},
  { X_ChangeActivePointerGrab,"XChangeActivePointerGrab"},
  { X_GrabKeyboard,"XGrabKeyboard"},
  { X_UngrabKeyboard,"XUngrabKeyboard"},
  { X_GrabKey,"XGrabKey"},
  { X_UngrabKey,"XUngrabKey"},
  { X_AllowEvents,"XAllowEvents"},
  { X_GrabServer,"XGrabServer"},
  { X_UngrabServer,"XUngrabServer"},
  { X_QueryPointer,"XQueryPointer"},
  { X_GetMotionEvents,"XGetMotionEvents"},
  { X_TranslateCoords,"XTranslateCoords"},
  { X_WarpPointer,"XWarpPointer"},
  { X_SetInputFocus,"XSetInputFocus"},
  { X_GetInputFocus,"XGetInputFocus"},
  { X_QueryKeymap,"XQueryKeymap"},
  { X_OpenFont,"XOpenFont"},
  { X_CloseFont,"XCloseFont"},
  { X_QueryFont,"XQueryFont"},
  { X_QueryTextExtents,"XQueryTextExtents"},
  { X_ListFonts,"XListFonts"},
  { X_ListFontsWithInfo,"XListFontsWithInfo"},
  { X_SetFontPath,"XSetFontPath"},
  { X_GetFontPath,"XGetFontPath"},
  { X_CreatePixmap,"XCreatePixmap"},
  { X_FreePixmap,"XFreePixmap"},
  { X_CreateGC,"XCreateGC"},
  { X_ChangeGC,"XChangeGC"},
  { X_CopyGC,"XCopyGC"},
  { X_SetDashes,"XSetDashes"},
  { X_SetClipRectangles,"XSetClipRectangles"},
  { X_FreeGC,"XFreeGC"},
  { X_ClearArea,"XClearArea"},
  { X_CopyArea,"XCopyArea"},
  { X_CopyPlane,"XCopyPlane"},
  { X_PolyPoint,"XPolyPoint"},
  { X_PolyLine,"XPolyLine"},
  { X_PolySegment,"XPolySegment"},
  { X_PolyRectangle,"XPolyRectangle"},
  { X_PolyArc,"XPolyArc"},
  { X_FillPoly,"XFillPoly"},
  { X_PolyFillRectangle,"XPolyFillRectangle"},
  { X_PolyFillArc,"XPolyFillArc"},
  { X_PutImage,"XPutImage"},
  { X_GetImage,"XGetImage"},
  { X_PolyText8,"XPolyText8"},
  { X_PolyText16,"XPolyText16"},
  { X_ImageText8,"XImageText8"},
  { X_ImageText16,"XImageText16"},
  { X_CreateColormap,"XCreateColormap"},
  { X_FreeColormap,"XFreeColormap"},
  { X_CopyColormapAndFree,"XCopyColormapAndFree"},
  { X_InstallColormap,"XInstallColormap"},
  { X_UninstallColormap,"XUninstallColormap"},
  { X_ListInstalledColormaps,"XListInstalledColormaps"},
  { X_AllocColor,"XAllocColor"},
  { X_AllocNamedColor,"XAllocNamedColor"},
  { X_AllocColorCells,"XAllocColorCells"},
  { X_AllocColorPlanes,"XAllocColorPlanes"},
  { X_FreeColors,"XFreeColors"},
  { X_StoreColors,"XStoreColors"},
  { X_StoreNamedColor,"XStoreNamedColor"},
  { X_QueryColors,"XQueryColors"},
  { X_LookupColor,"XLookupColor"},
  { X_CreateCursor,"XCreateCursor"},
  { X_CreateGlyphCursor,"XCreateGlyphCursor"},
  { X_FreeCursor,"XFreeCursor"},
  { X_RecolorCursor,"XRecolorCursor"},
  { X_QueryBestSize,"XQueryBestSize"},
  { X_QueryExtension,"XQueryExtension"},
  { X_ListExtensions,"XListExtensions"},
  { X_ChangeKeyboardMapping,"XChangeKeyboardMapping"},
  { X_GetKeyboardMapping,"XGetKeyboardMapping"},
  { X_ChangeKeyboardControl,"XChangeKeyboardControl"},
  { X_GetKeyboardControl,"XGetKeyboardControl"},
  { X_Bell,"XBell"},
  { X_ChangePointerControl,"XChangePointerControl"},
  { X_GetPointerControl,"XGetPointerControl"},
  { X_SetScreenSaver,"XSetScreenSaver"},
  { X_GetScreenSaver,"XGetScreenSaver"},
  { X_ChangeHosts,"XChangeHosts"},
  { X_ListHosts,"XListHosts"},
  { X_SetAccessControl,"XSetAccessControl"},
  { X_SetCloseDownMode,"XSetCloseDownMode"},
  { X_KillClient,"XKillClient"},
  { X_RotateProperties,"XRotateProperties"},
  { X_ForceScreenSaver,"XForceScreenSaver"},
  { X_SetPointerMapping,"XSetPointerMapping"},
  { X_GetPointerMapping,"XGetPointerMapping"},
  { X_SetModifierMapping,"XSetModifierMapping"},
  { X_GetModifierMapping,"XGetModifierMapping"},
  { X_NoOperation,"XNoOperation"}
};

static CodeName ProtocolErrors[] =
{
  { Success,"Success"},
  { BadRequest,"BadRequest"},
  { BadValue,"BadValue"},
  { BadWindow,"BadWindow"},
  { BadPixmap,"BadPixmap"},
  { BadAtom,"BadAtom"},
  { BadCursor,"BadCursor"},
  { BadFont,"BadFont"},
  { BadMatch,"BadMatch"},
  { BadDrawable,"BadDrawable"},
  { BadAccess,"BadAccess"},
  { BadAlloc,"BadAlloc"},
  { BadColor,"BadColor"},
  { BadGC,"BadGC"},
  { BadIDChoice,"BadIDChoice"},
  { BadName,"BadName"},
  { BadLength,"BadLength"},
  { BadImplementation,"BadImplementation"}
};

static int XWindowsError(Display *display, XErrorEvent *error)
{
  char *errorName   = "unknown";
  char *requestName = "unknown";
  int   i,n;
  char  buffer[500];
  
  n = sizeof(ProtocolErrors) / sizeof(ProtocolErrors[0]);
  
  for(i = 0; i < n; i++) 
    {
     if (ProtocolErrors[i].code == error->error_code)
       {
        errorName = ProtocolErrors[i].name;
       }
    }

  n = sizeof(ProtocolNames) / sizeof(ProtocolNames[0]);
  
  for(i = 0; i < n; i++) 
    {
     if (ProtocolNames[i].code == error->request_code)
       {
         requestName = ProtocolNames[i].name;
       }
    }

  sprintf(buffer,"%s in %s",errorName,requestName);

  proper_printf("\nX Error %s\n\n", buffer);

#if NEVER
  /* Raise exception if we are running in synchronous mode */
  if (display->private15) RaiseXWindows(buffer);
#endif

  return 0; /* DUMMY value - SPF 6/1/94 */
}

static void XWindowsGC(GCOpFunc op)
{
  /* Process all the objects in the list. If an object */
  /* is not found from outside then it is removed.     */

  T_List **T = &TList;
  C_List **C = &CList;

  int i;
  
  /* process all XList headers */
  for (i = 0; i < XLISTSIZE; i++)
    {
      X_List *L = XList[i];
    
      while(L)
      {
	X_Object *P = L->object;  /* copy object pointer */
	X_List   *N = L->next;    /* copy next   pointer */
    
	(* op)((word **)&P,1 /* weak */);
    
	/* P may have been moved, or overwritten with a 0 if not accessible */
	
	if (P == 0)
	  DestroyXObject(L->object);
	else
	  L->object = P;
    
	L = N;
      }
    }
    
  /* Process the timeout/message list */
  
  while(*T)
  {
    T_List *t = *T;
    
    (* op)(&t->alpha  ,0 /* strong */);
    (* op)(&t->handler,0 /* strong */);
    (* op)((word **)&t->window_object ,1 /* weak   */);
    (* op)((word **)&t->widget_object ,0 /* strong */);
    
    if (t->window_object == 0 && t->widget_object == 0)
    {
      *T = t->next;
      
      free(t);
    }
    else T = &t->next;
  }

  /* Process the callback list */
  
  while(*C)
  {
    C_List *c = *C;
    
    (* op)(&c->function,0 /* strong */);
    (* op)((word **)&c->widget_object,0 /* strong */);
    
    /* DCJM: This doesn't make sense.  The widget entry will only
       go to zero if the G.C. operation was weak, not strong as in
       the line above. */
    if (c->widget_object == 0)
    {
      *C = c->next;
      
      free(c);
    }
    else C = &c->next;
  }
  
  /* Process the callback waiting list */
  
  (* op)((word **)&FList,0 /* strong */);

  /* and the Xt event waiting list. */
  (* op) ((word **)&GList,0 /* strong */ ) ;
}

void init_xwindow_system(void)
{
  initXList(); /* added 9/12/93 SPF */

  XtToolkitInitialize();

  XSetErrorHandler(XWindowsError);

  RegisterGCProc(XWindowsGC);
  
/* no need to check timer event queue as an interrupt! SPF 23/2/95 */
#if 0
  register_interrupt_proc(XWindowsInterrupted);
#endif
}

void re_init_xwindow_system(void) /* added 18/11/93 SPF */
{
}

