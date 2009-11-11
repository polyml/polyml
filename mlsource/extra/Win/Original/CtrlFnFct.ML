(*
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
*)


signature importCInterfaceSig = 
sig
  type sym
  type 'a Conversion
  type Ctype
  type vol
 
  val get_sym  : string -> string -> sym
  val call_sym     : sym -> (Ctype * vol) list -> Ctype -> vol

  val alloc        : int -> Ctype -> vol
  val address      : vol -> vol
  val deref        : vol -> vol
  val offset       : int -> Ctype -> vol -> vol
  val assign       : Ctype -> vol -> vol -> unit

  val Cchar       : Ctype
  val Cfloat      : Ctype
  val Cint        : Ctype
  val Clong       : Ctype
  val Cshort      : Ctype
  val Cpointer    : Ctype -> Ctype
  val Cstruct     : Ctype list -> Ctype
  val Cfunction   : Ctype list * Ctype -> Ctype
  val Cvoid       : Ctype

  val voidStar : Ctype

  val toCstring    : string -> vol
  val toCchar      : string -> vol
  val toCfloat     : real -> vol
  val toCint       : int -> vol
  val toClong      : int -> vol
  val toCshort     : int -> vol

  val fromCstring  : vol -> string
  val fromCchar    : vol -> string
  val fromCfloat   : vol -> real
  val fromCint     : vol -> int
  val fromClong    : vol -> int
  val fromCshort   : vol -> int

  val break_struct  : Ctype list -> vol -> vol list
  val make_struct   : (Ctype * vol) list -> vol

  val CHAR  : string Conversion
  val DOUBLE  : real Conversion
  val FLOAT   : real Conversion
  val INT     : int Conversion
  val LONG    : int Conversion
  val SHORT   : int Conversion
  val STRING  : string Conversion
  val VOID    : unit Conversion
  val BOOL    : bool Conversion
  val POINTER : vol Conversion

    val call4 : sym ->
      'a Conversion
    * 'b Conversion
    * 'c Conversion
    * 'd Conversion -> 'e Conversion
    -> 'a * 'b * 'c * 'd -> 'e
end;

signature importMacroSig = 
sig
   val LOWORD : int -> int
   val HIWORD : int -> int
   val MAKELPARAM : int * int -> int  
end;

signature importConstTypeSig = 
sig
    type 'a encoding
    type ButtonMessageEnquiry
    type ScrollBarAction
    type ComboBoxAction
    type EditCtlBehaviour
    type ListBoxAction

    type ButtonStyle
    val ButtonStyleE : ButtonStyle encoding
     
    type EnableScrollBarFlag
    val EnableScrollBarFlagE : EnableScrollBarFlag encoding

    type DialogBoxFlag
    val DialogBoxFlagE : DialogBoxFlag encoding

    val repE : 'a encoding -> 'a -> int
    val absE : 'a encoding -> int -> 'a

    val ButtonMessageEnquiryE : ButtonMessageEnquiry encoding
    val BM_GETCHECK : ButtonMessageEnquiry
    val BM_SETCHECK : ButtonMessageEnquiry
    val BM_GETSTATE : ButtonMessageEnquiry
    val BM_SETSTATE : ButtonMessageEnquiry
    val BM_SETSTYLE : ButtonMessageEnquiry

    val ScrollBarActionE : ScrollBarAction encoding
    val SBM_SETPOS         : ScrollBarAction
    val SBM_GETPOS         : ScrollBarAction
    val SBM_SETRANGE       : ScrollBarAction
    val SBM_SETRANGEREDRAW : ScrollBarAction
    val SBM_GETRANGE       : ScrollBarAction
    val SBM_ENABLE_ARROWS  : ScrollBarAction
             
    val ComboBoxActionE : ComboBoxAction encoding
    val CB_GETEDITSEL             : ComboBoxAction
    val CB_LIMITTEXT              : ComboBoxAction
    val CB_SETEDITSEL             : ComboBoxAction
    val CB_ADDSTRING              : ComboBoxAction
    val CB_DELETESTRING           : ComboBoxAction
    val CB_DIR                    : ComboBoxAction
    val CB_GETCOUNT               : ComboBoxAction
    val CB_GETCURSEL              : ComboBoxAction
    val CB_GETLBTEXT              : ComboBoxAction
    val CB_GETLBTEXTLEN           : ComboBoxAction
    val CB_INSERTSTRING           : ComboBoxAction
    val CB_RESETCONTENT           : ComboBoxAction
    val CB_FINDSTRING             : ComboBoxAction
    val CB_SELECTSTRING           : ComboBoxAction
    val CB_SETCURSEL              : ComboBoxAction
    val CB_SHOWDROPDOWN           : ComboBoxAction
    val CB_GETITEMDATA            : ComboBoxAction
    val CB_SETITEMDATA            : ComboBoxAction
    val CB_GETDROPPEDCONTROLRECT  : ComboBoxAction
    val CB_SETITEMHEIGHT          : ComboBoxAction
    val CB_GETITEMHEIGHT          : ComboBoxAction
    val CB_SETEXTENDEDUI          : ComboBoxAction
    val CB_GETEXTENDEDUI          : ComboBoxAction
    val CB_GETDROPPEDSTATE        : ComboBoxAction
    val CB_FINDSTRINGEXACT        : ComboBoxAction
    val CB_SETLOCALE              : ComboBoxAction
    val CB_GETLOCALE              : ComboBoxAction
    val CB_MSGMAX                 : ComboBoxAction

    val EditCtlBehaviourE : EditCtlBehaviour encoding
    val EM_GETSEL                 : EditCtlBehaviour
    val EM_SETSEL                 : EditCtlBehaviour
    val EM_GETRECT                : EditCtlBehaviour
    val EM_SETRECT                : EditCtlBehaviour
    val EM_SETRECTNP              : EditCtlBehaviour
    val EM_SCROLL                 : EditCtlBehaviour
    val EM_LINESCROLL             : EditCtlBehaviour
    val EM_SCROLLCARET            : EditCtlBehaviour
    val EM_GETMODIFY              : EditCtlBehaviour
    val EM_SETMODIFY              : EditCtlBehaviour
    val EM_GETLINECOUNT           : EditCtlBehaviour
    val EM_LINEINDEX              : EditCtlBehaviour
    val EM_SETHANDLE              : EditCtlBehaviour
    val EM_GETHANDLE              : EditCtlBehaviour
    val EM_GETTHUMB               : EditCtlBehaviour
    val EM_LINELENGTH             : EditCtlBehaviour
    val EM_REPLACESEL             : EditCtlBehaviour
    val EM_GETLINE                : EditCtlBehaviour
    val EM_LIMITTEXT              : EditCtlBehaviour
    val EM_CANUNDO                : EditCtlBehaviour
    val EM_UNDO                   : EditCtlBehaviour
    val EM_FMTLINES               : EditCtlBehaviour
    val EM_LINEFROMCHAR           : EditCtlBehaviour
    val EM_SETTABSTOPS            : EditCtlBehaviour
    val EM_SETPASSWORDCHAR        : EditCtlBehaviour
    val EM_EMPTYUNDOBUFFER        : EditCtlBehaviour
    val EM_GETFIRSTVISIBLELINE    : EditCtlBehaviour
    val EM_SETREADONLY            : EditCtlBehaviour
    val EM_SETWORDBREAKPROC       : EditCtlBehaviour
    val EM_GETWORDBREAKPROC       : EditCtlBehaviour
    val EM_GETPASSWORDCHAR        : EditCtlBehaviour

    val ListBoxActionE : ListBoxAction encoding 
    val LB_ADDSTRING             : ListBoxAction
    val LB_INSERTSTRING          : ListBoxAction
    val LB_DELETESTRING          : ListBoxAction
    val LB_SELITEMRANGEEX        : ListBoxAction
    val LB_RESETCONTENT          : ListBoxAction
    val LB_SETSEL                : ListBoxAction
    val LB_SETCURSEL             : ListBoxAction
    val LB_GETSEL                : ListBoxAction
    val LB_GETCURSEL             : ListBoxAction
    val LB_GETTEXT               : ListBoxAction
    val LB_GETTEXTLEN            : ListBoxAction
    val LB_GETCOUNT              : ListBoxAction
    val LB_SELECTSTRING          : ListBoxAction
    val LB_DIR                   : ListBoxAction
    val LB_GETTOPINDEX           : ListBoxAction
    val LB_FINDSTRING            : ListBoxAction
    val LB_GETSELCOUNT           : ListBoxAction
    val LB_GETSELITEMS           : ListBoxAction
    val LB_SETTABSTOPS           : ListBoxAction
    val LB_GETHORIZONTALEXTENT   : ListBoxAction
    val LB_SETHORIZONTALEXTENT   : ListBoxAction
    val LB_SETCOLUMNWIDTH        : ListBoxAction
    val LB_ADDFILE               : ListBoxAction
    val LB_SETTOPINDEX           : ListBoxAction
    val LB_GETITEMRECT           : ListBoxAction
    val LB_GETITEMDATA           : ListBoxAction
    val LB_SETITEMDATA           : ListBoxAction
    val LB_SELITEMRANGE          : ListBoxAction
    val LB_SETANCHORINDEX        : ListBoxAction
    val LB_GETANCHORINDEX        : ListBoxAction
    val LB_SETCARETINDEX         : ListBoxAction
    val LB_GETCARETINDEX         : ListBoxAction
    val LB_SETITEMHEIGHT         : ListBoxAction
    val LB_GETITEMHEIGHT         : ListBoxAction
    val LB_FINDSTRINGEXACT       : ListBoxAction
    val LB_SETLOCALE             : ListBoxAction
    val LB_GETLOCALE             : ListBoxAction
    val LB_SETCOUNT              : ListBoxAction
    val LB_MSGMAX                : ListBoxAction
end;

signature importTypeSig = 
sig
  type 'a Conversion
  type vol
  type Point
  type Rectangle

  val mkPoint : {x:int,y:int} -> Point
  val breakPoint : Point -> {x:int,y:int}
  val mkRect : {left:int,top:int,right:int,bottom:int} -> Rectangle
  val breakRect : Rectangle -> {left:int,top:int,right:int,bottom:int}

  datatype MLHWND = MLHWND of vol
  val HWND : MLHWND Conversion

  datatype MLHLOCAL= MLHLOCAL of vol
  val HLOCAL : MLHLOCAL Conversion
end;

signature importDirSig =
sig
   val wpolyname   : string 
   val gdilibname  : string
   val userlibname : string
end;

functor CtrlFnFct ( structure Dir : importDirSig
                    structure CInterface : importCInterfaceSig
                    structure Macro : importMacroSig
                    structure ConstType : importConstTypeSig
                    structure Type : importTypeSig

                    sharing type CInterface.vol =
                                 Type.vol
                    sharing type CInterface.Conversion =
                                 Type.Conversion
                  ) =
struct

local
open CInterface
open Macro
open ConstType 
open Type
open Dir

val get = get_sym userlibname

val SendMessage = call4 (get "SendMessageA") (HWND,LONG,LONG,LONG) (LONG)
fun SendMessageV (MLHWND h,m,p1,p2) = fromClong ( call_sym 
                                                    (get "SendMessageA")
                                                    [(Cvoid,h),
                                                     (Clong,toClong m),
                                                     (Clong,toClong p1),
                                                     (Clong,p2) ] Clong )

fun SendMessageVV (MLHWND h,m,p1,p2) = fromClong ( call_sym 
                                                    (get "SendMessageA")
                                                    [(Cvoid,h),
                                                     (Clong,toClong m),
                                                     (Clong,p1),
                                                     (Clong,p2) ] Clong )

fun btoi b = if b then 1 else 0

exception WrongNumberOfVolatiles

fun break_struct4 types v =
let val vols = break_struct types v
in case vols of
   [v1,v2,v3,v4] => (v1,v2,v3,v4)
 | _       =>  raise WrongNumberOfVolatiles   
end;

in

(* Buttons *)

fun button__GETCHECK (w)     = SendMessage(w,repE ButtonMessageEnquiryE BM_GETCHECK,0,0)

fun button__SETCHECK (w,f)   = SendMessage(w,repE ButtonMessageEnquiryE BM_SETCHECK,f,0)

fun button__GETSTATE (w)     = SendMessage(w,repE ButtonMessageEnquiryE BM_GETSTATE,0,0)

fun button__SETSTATE (w,f)   = SendMessage(w,repE ButtonMessageEnquiryE BM_SETSTATE,f,0)

fun button__SETSTYLE (w,s,f) = SendMessage(w,repE ButtonMessageEnquiryE BM_SETSTYLE,repE ButtonStyleE s,btoi f)

(* Scroll Bars *)

fun scrollbar_SETPOS (w,pos,f)           = SendMessage(w,repE ScrollBarActionE SBM_SETPOS,pos,btoi f)

fun scrollbar_GETPOS  (w)                = SendMessage(w,repE ScrollBarActionE SBM_GETPOS,0,0)

fun scrollbar_SETRANGE (w,min,max)       = SendMessage(w,repE ScrollBarActionE SBM_SETRANGE,min,max)

fun scrollbar_SETRANGEREDRAW (w,min,max) = SendMessage(w,repE ScrollBarActionE SBM_SETRANGEREDRAW,min,max)

fun scrollbar_GETRANGE (w,mp,Mp)         = SendMessage(w,repE ScrollBarActionE SBM_GETRANGE,mp,Mp)

fun scrollbar_ENABLE_ARROWS (w,f)        = SendMessage(w,repE ScrollBarActionE SBM_ENABLE_ARROWS,repE EnableScrollBarFlagE f,0)

(* Combo Boxes *)

fun combobox_GETEDITSEL (w) = 
let val r = SendMessage(w,repE ComboBoxActionE CB_GETEDITSEL,0,0)
in (LOWORD r,HIWORD r)
end

fun combobox_LIMITTEXT    (w,limit) = SendMessage(w,repE ComboBoxActionE CB_LIMITTEXT,limit,0)

fun combobox_SETEDITSEL   (w,start,finish) = 
                        SendMessage(w,repE ComboBoxActionE CB_SETEDITSEL,0,MAKELPARAM(start,finish))

fun combobox_ADDSTRING    (w,s) = SendMessageV(w,repE ComboBoxActionE CB_ADDSTRING,0,toCstring s)

fun combobox_DELETESTRING (w,i) = SendMessage(w,repE ComboBoxActionE CB_DELETESTRING,i,0)

fun combobox_DIR          (w,f,s) = SendMessageV(w,repE ComboBoxActionE CB_DIR,repE DialogBoxFlagE f,toCstring s)

fun combobox_GETCOUNT     (w) = SendMessage(w,repE ComboBoxActionE CB_GETCOUNT,0,0)

fun combobox_GETCURSEL    (w) = SendMessage(w,repE ComboBoxActionE CB_GETCURSEL,0,0)

fun combobox_GETLBTEXT    (w,i) = 
let val s = alloc 100 Cchar
    val r = SendMessageV(w,repE ComboBoxActionE CB_GETLBTEXT,i,s)
in fromCstring s
end

fun combobox_GETLBTEXTLEN (w,i) = SendMessage(w,repE ComboBoxActionE CB_GETLBTEXTLEN,i,0)

fun combobox_INSERTSTRING (w,i,s) = SendMessageV(w,repE ComboBoxActionE CB_INSERTSTRING,i,toCstring s)

fun combobox_RESETCONTENT (w) = SendMessage(w,repE ComboBoxActionE CB_RESETCONTENT,0,0)

fun combobox_FINDSTRING   (w,i,s) = SendMessageV(w,repE ComboBoxActionE CB_FINDSTRING,i,toCstring s)

fun combobox_SELECTSTRING (w,i,s) = SendMessageV(w,repE ComboBoxActionE CB_SELECTSTRING,i,toCstring s)

fun combobox_SETCURSEL    (w,i) = SendMessage(w,repE ComboBoxActionE CB_SETCURSEL,i,0)

fun combobox_SHOWDROPDOWN  (w,f) = SendMessage(w,repE ComboBoxActionE CB_SHOWDROPDOWN,btoi f,0)

fun combobox_GETITEMDATA   (w,i) = SendMessage(w,repE ComboBoxActionE CB_GETITEMDATA,i,0)

fun combobox_SETITEMDATA   (w,i,data) = SendMessage(w,repE ComboBoxActionE CB_SETITEMDATA,i,data)

fun combobox_GETDROPPEDCONTROLRECT (w) =
let val rect = alloc 4 Clong 
    val r = SendMessageV(w,repE ComboBoxActionE CB_GETDROPPEDCONTROLRECT,0,address rect)
    val (t,r,b,l) = break_struct4 ([Clong,Clong,Clong,Clong]) rect
in
  mkRect {left=fromClong t,top=fromClong r,right=fromClong b,bottom=fromClong l}
end

fun combobox_SETITEMHEIGHT  (w,i,h) = SendMessage(w,repE ComboBoxActionE CB_SETITEMHEIGHT,i,h)

fun combobox_GETITEMHEIGHT  (w,i) = SendMessage(w,repE ComboBoxActionE CB_GETITEMHEIGHT,i,0)

fun combobox_SETEXTENDEDUI  (w,f) = SendMessage(w,repE ComboBoxActionE CB_SETEXTENDEDUI,btoi f,0)

fun combobox_GETEXTENDEDUI  (w) = SendMessage(w,repE ComboBoxActionE CB_GETEXTENDEDUI,0,0)

fun combobox_GETDROPPEDSTATE (w) = SendMessage(w,repE ComboBoxActionE CB_GETDROPPEDSTATE,0,0)

fun combobox_FINDSTRINGEXACT (w,i,s) = SendMessageV(w,repE ComboBoxActionE CB_FINDSTRINGEXACT,i,toCstring s)

fun combobox_SETLOCALE       (w,i) = SendMessage(w,repE ComboBoxActionE CB_SETLOCALE,i,0)

fun combobox_GETLOCALE       (w) = SendMessage(w,repE ComboBoxActionE CB_GETLOCALE,0,0)

(* Edit Controls *)

fun editcontrol_GETSEL  (w) = 
let val r = SendMessage(w,repE EditCtlBehaviourE EM_GETSEL,0,0)
in (LOWORD r, HIWORD r)
end

fun editcontrol_SETSEL  (w,start,finish) = SendMessage(w,repE EditCtlBehaviourE EM_SETSEL,start,finish)

fun editcontrol_GETRECT (w) = 
let val rect = alloc 4 Clong
    val r = SendMessageV(w,repE EditCtlBehaviourE EM_GETRECT,0,address rect)
    val (t,r,b,l) = break_struct4 ([Clong,Clong,Clong,Clong]) rect
in mkRect {top=fromClong t,left=fromClong r,bottom=fromClong b,right=fromClong l}
end

fun editcontrol_SETRECT (w,r) = 
let val {top,right,bottom,left} = breakRect r  
    val rect = make_struct 
             [(Clong,toClong left),
              (Clong,toClong top),
              (Clong,toClong right),
              (Clong,toClong bottom) ]

in SendMessageV(w,repE EditCtlBehaviourE EM_SETRECT,0,address rect)
end

fun editcontrol_SETRECTNP (w,r) =
let val {top,right,bottom,left} = breakRect r  
    val rect = make_struct 
             [(Clong,toClong left),
              (Clong,toClong top),
              (Clong,toClong right),
              (Clong,toClong bottom) ]
in SendMessageV(w,repE EditCtlBehaviourE EM_SETRECTNP,0,address rect)
end

fun editcontrol_SCROLL (w,i) = SendMessage(w,repE EditCtlBehaviourE EM_SCROLL,i,0)

fun editcontrol_LINESCROLL (w,x,y) = SendMessage(w,repE EditCtlBehaviourE EM_LINESCROLL,x,y)

fun editcontrol_SCROLLCARET (w) = SendMessage(w,repE EditCtlBehaviourE EM_SCROLLCARET,0,0)

fun editcontrol_GETMODIFY (w) = SendMessage(w,repE EditCtlBehaviourE EM_GETMODIFY,0,0)

fun editcontrol_SETMODIFY (w,f) = SendMessage(w,repE EditCtlBehaviourE EM_SETMODIFY,btoi f,0)

fun editcontrol_GETLINECOUNT (w) = SendMessage(w,repE EditCtlBehaviourE EM_GETLINECOUNT,0,0)

fun editcontrol_LINEINDEX (w,lno) = SendMessage(w,repE EditCtlBehaviourE EM_LINEINDEX,lno,0)

fun editcontrol_SETHANDLE (w,MLHLOCAL h) = SendMessageVV(w,repE EditCtlBehaviourE EM_SETHANDLE,h,toClong 0)

fun editcontrol_GETHANDLE (w) = MLHLOCAL (toClong (SendMessage(w,repE EditCtlBehaviourE EM_GETHANDLE,0,0)) )

fun editcontrol_LINELENGTH (w,i) = SendMessage(w,repE EditCtlBehaviourE EM_LINELENGTH,i,0)

fun editcontrol_REPLACESEL (w,s) = SendMessageV(w,repE EditCtlBehaviourE EM_REPLACESEL,0,toCstring s)

fun editcontrol_GETLINE (w,lno) = 
let val l = alloc 100 Cchar
    val r = SendMessageV(w,repE EditCtlBehaviourE EM_GETLINE,lno,address l)
in fromCstring l
end

fun editcontrol_LIMITTEXT (w,l) = SendMessage(w,repE EditCtlBehaviourE EM_LIMITTEXT,l,0)

fun editcontrol_CANUNDO (w) = SendMessage(w,repE EditCtlBehaviourE EM_CANUNDO,0,0)

fun editcontrol_UNDO (w) = SendMessage(w,repE EditCtlBehaviourE EM_UNDO,0,0)

fun editcontrol_FMTLINES (w,f) = SendMessage(w,repE EditCtlBehaviourE EM_FMTLINES,btoi f,0)

fun editcontrol_LINEFROMCHAR (w,i) = SendMessage(w,repE EditCtlBehaviourE EM_LINEFROMCHAR,i,0)

fun editcontrol_SETTABSTOPS (w,ilist) = 
let val l = List.length ilist
    val arr = alloc l Cshort
in
  SendMessageV(w,repE EditCtlBehaviourE EM_SETTABSTOPS,l,address arr)
end

fun editcontrol_SETPASSWORDCHAR (w,ch) = SendMessage(w,repE EditCtlBehaviourE EM_SETPASSWORDCHAR, ord ch,0)

fun editcontrol_EMPTYUNDOBUFFER (w) = SendMessage(w,repE EditCtlBehaviourE EM_EMPTYUNDOBUFFER,0,0)

fun editcontrol_GETFIRSTVISIBLELINE (w) = SendMessage(w,repE EditCtlBehaviourE EM_GETFIRSTVISIBLELINE,0,0)

fun editcontrol_SETREADONLY (w,f) = SendMessage(w,repE EditCtlBehaviourE EM_SETREADONLY,btoi f,0)

fun editcontrol_GETPASSWORDCHAR (w) = chr (SendMessage (w,repE EditCtlBehaviourE EM_GETPASSWORDCHAR,0,0)    )


(* List Box *)


fun listbox_ADDSTRING             (w,s) = SendMessageV(w,repE ListBoxActionE LB_ADDSTRING,0,toCstring s)

fun listbox_INSERTSTRING          (w,i,s) = SendMessageV(w,repE ListBoxActionE LB_INSERTSTRING,i,toCstring s)

fun listbox_DELETESTRING          (w,i) = SendMessage(w,repE ListBoxActionE LB_DELETESTRING,i,0)

fun listbox_SELITEMRANGEEX        (w,fst,lst) = SendMessage(w,repE ListBoxActionE LB_SELITEMRANGEEX,fst,lst)

fun listbox_RESETCONTENT          (w) = SendMessage(w,repE ListBoxActionE LB_RESETCONTENT,0,0)

fun listbox_SETSEL                (w,f,i) = SendMessage(w,repE ListBoxActionE LB_SETSEL,btoi f,i) 

fun listbox_GETSEL                (w,i) = SendMessage(w,repE ListBoxActionE LB_GETSEL,i,0)

fun listbox_GETCURSEL             (w) = SendMessage(w,repE ListBoxActionE LB_GETCURSEL,0,0)

fun listbox_GETTEXT               (w,i) = 
let val s = alloc 100 Cchar
    val r = SendMessageV(w,repE ListBoxActionE LB_GETTEXT,i,address s)
in fromCstring s
end

fun listbox_GETTEXTLEN            (w,i) = SendMessage(w,repE ListBoxActionE LB_GETTEXTLEN,i,0)

fun listbox_GETCOUNT              (w) = SendMessage(w,repE ListBoxActionE LB_GETCOUNT,0,0)

fun listbox_SELECTSTRING          (w,i,s) = SendMessageV(w,repE ListBoxActionE LB_SELECTSTRING,i,toCstring s)

fun listbox_DIR (w,attr,s) = 
           SendMessageV(w,repE ListBoxActionE LB_DIR,repE DialogBoxFlagE attr,toCstring s)

fun listbox_GETTOPINDEX           (w) = SendMessage(w,repE ListBoxActionE LB_GETTOPINDEX,0,0)

fun listbox_FINDSTRING            (w,i,s) = SendMessageV(w,repE ListBoxActionE LB_FINDSTRING,i,toCstring s)

fun listbox_GETSELCOUNT           (w) = SendMessage(w,repE ListBoxActionE LB_GETSELCOUNT,0,0)

fun listbox_GETSELITEMS           (w,i) = 
let val buf = alloc i Cint
    val r = SendMessageV(w,repE ListBoxActionE LB_GETSELITEMS,i,address buf)
    fun v2l v 0 = []
    |   v2l v i = (v2l v (i-1)) @ [(fromCint (offset (i-1) Cint v))]   
in
  v2l buf
end

fun listbox_SETTABSTOPS (w,ilist) = 
let val l = List.length ilist
    val arr = alloc l Cshort
in
  SendMessageV(w,repE ListBoxActionE LB_SETTABSTOPS,l,address arr)
end

fun listbox_GETHORIZONTALEXTENT   (w) = SendMessage(w,repE ListBoxActionE LB_GETHORIZONTALEXTENT,0,0)

fun listbox_SETHORIZONTALEXTENT   (w,x) = SendMessage(w,repE ListBoxActionE LB_SETHORIZONTALEXTENT,x,0)

fun listbox_SETCOLUMNWIDTH        (w,x) = SendMessage(w,repE ListBoxActionE LB_SETCOLUMNWIDTH,x,0)

fun listbox_ADDFILE               (w,s) = SendMessageV(w,repE ListBoxActionE LB_ADDFILE,0,toCstring s)

fun listbox_SETTOPINDEX           (w,i) = SendMessage(w,repE ListBoxActionE LB_SETTOPINDEX,i,0)

fun listbox_GETITEMRECT           (w,i) = 
let val rect = alloc 4 Clong
    val r = SendMessageV(w,repE ListBoxActionE LB_GETITEMRECT,i,address rect)
    val (t,r,b,l) = break_struct4 ([Clong,Clong,Clong,Clong]) rect
in mkRect {top=fromClong t,left=fromClong r, bottom=fromClong b,right=fromClong l}
end

fun listbox_GETITEMDATA           (w,i) = SendMessage(w,repE ListBoxActionE LB_GETITEMDATA,i,0)

fun listbox_SETITEMDATA           (w,i,data) = SendMessage(w,repE ListBoxActionE LB_SETITEMDATA,i,data)

fun listbox_SELITEMRANGE (w,f,fst,lst) = 
                    SendMessage(w,repE ListBoxActionE LB_SELITEMRANGE,btoi f,MAKELPARAM (fst,lst) )

fun listbox_SETANCHORINDEX        (w,i) = SendMessage(w,repE ListBoxActionE LB_SETANCHORINDEX,i,0)

fun listbox_GETANCHORINDEX        (w) = SendMessage(w,repE ListBoxActionE LB_GETANCHORINDEX,0,0)

fun listbox_SETCARETINDEX (w,i,f) = 
                    SendMessage(w,repE ListBoxActionE LB_SETCARETINDEX,i,MAKELPARAM(f,0))

fun listbox_GETCARETINDEX         (w) = SendMessage(w,repE ListBoxActionE LB_GETCARETINDEX,0,0)

fun listbox_SETITEMHEIGHT (w,i,y) = 
                    SendMessage(w,repE ListBoxActionE LB_SETITEMHEIGHT,i,MAKELPARAM(y,0))

fun listbox_GETITEMHEIGHT         (w,i) = SendMessage(w,repE ListBoxActionE LB_GETITEMHEIGHT,i,0)

fun listbox_FINDSTRINGEXACT       (w,i,s) = SendMessageV(w,repE ListBoxActionE LB_FINDSTRINGEXACT,i,toCstring s)

fun listbox_SETLOCALE             (w,i) = SendMessage(w,repE ListBoxActionE LB_SETLOCALE,i,0)

fun listbox_GETLOCALE             (w) = SendMessage(w,repE ListBoxActionE LB_GETLOCALE,0,0)

fun listbox_SETCOUNT              (w,i) = SendMessage(w,repE ListBoxActionE LB_SETCOUNT,i,0)

end

end;
