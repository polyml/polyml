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
 type vol
 val toCint : int -> vol
end



signature importConstTypeSig =
sig
    type 'a encoding
    val orLE : 'a encoding -> 'a list -> 'a

    type MenuFlag 
    val MF_CHECKED : MenuFlag
    val MF_GRAYED  :MenuFlag
    val MF_MENUBREAK :MenuFlag
    val MF_MENUBARBREAK  : MenuFlag
    val MF_SEPARATOR : MenuFlag
    val MF_POPUP : MenuFlag

    type ClassStyle 
    val CS_HREDRAW : ClassStyle
    val CS_VREDRAW : ClassStyle
    type WindowStyle
    val WS_OVERLAPPEDWINDOW : WindowStyle
    val WS_POPUPWINDOW : WindowStyle
    type WindowStyleExtended
    type ButtonStyle
    type ScrollBarStyle
    type ScrollBarType
    type StaticStyle
    type EditStyle
    type ListBoxStyle
    type ComboBoxStyle
    val CW_USEDEFAULT : int 
    type IconIdentifier
    val IDI_APPLICATION : IconIdentifier 
    type CursorIdentifier 
    val IDC_ARROW : CursorIdentifier    
    val WindowStyleExtendedE : WindowStyleExtended encoding
    val MenuFlagE : MenuFlag encoding
    val ClassStyleE : ClassStyle encoding
    val WindowStyleE : WindowStyle encoding
    val ButtonStyleE : ButtonStyle encoding
    val StaticStyleE : StaticStyle encoding
    val ScrollBarStyleE : ScrollBarStyle encoding
    val EditStyleE : EditStyle encoding
    val ListBoxStyleE : ListBoxStyle encoding
    val ComboBoxStyleE : ComboBoxStyle encoding

    val SBS_HORZ : ScrollBarStyle
    val SBS_CHILD : ScrollBarStyle
    val SBS_VERT : ScrollBarStyle
    val SB_CTL : ScrollBarType
    val CBS_SIMPLE : ComboBoxStyle
    val CBS_CHILD : ComboBoxStyle
    val ES_LEFT : EditStyle
    val ES_AUTOHSCROLL : EditStyle
    val ES_BORDER : EditStyle
    val ES_CHILD : EditStyle 
    val LBS_STANDARD : ListBoxStyle
    val LBS_CHILD : ListBoxStyle
    val SS_SIMPLE : StaticStyle
    val SS_CHILD : StaticStyle
    val BS_CHILD : ButtonStyle
    val BS_PUSHBUTTON : ButtonStyle    
end;

signature importTypeSig =
sig
   type vol
   eqtype Id
   val newId : unit -> Id
   val intOfId : Id -> int
   val sameId : Id * Id -> bool

   datatype 'a option = Some of 'a | None

   datatype MLHWND = MLHWND of vol
end;

signature importHandlerSig =
sig
   type window
   type MLHMENU
   type MLHDC
   type System
   type ProcessID

   exception Windows of string

   val newProcessID : unit -> ProcessID

   val RunProcess : ProcessID -> unit

   val AddCallbackML : window * (System -> unit) -> unit
   val AddWindowRTS : window * ProcessID * bool -> bool    

   val SetProcessRunStateRTS : int * int -> unit

   val EnableProcessWindowsRTS : int * bool -> unit

   val delegate : ProcessID -> (unit -> unit) -> bool

   val nullWindow : unit -> window
   val nullMenu : unit -> MLHMENU

   val killprocess: ProcessID -> unit
   val closeDownWin: window -> unit

   val GetpidRTS : window -> int

   val GetDeviceContextRTS : window -> MLHDC

   val HasValidDeviceContextRTS : window -> bool
  
end;

signature importMsgFnSig =
sig
   eqtype Id 
   type System
   type MouseMessage
   type ControlMessage
   type KeyMessage
   type ScrollMessage
   type RefreshMessage
   type DestroyMessage
   type OtherMessage
   val mouseMessageOf : System -> MouseMessage
   val controlMessageOf : System -> ControlMessage
   val keyMessageOf : System -> KeyMessage
   val refreshMessageOf : System -> RefreshMessage
   val scrollMessageOf : System -> ScrollMessage
   val destroyMessageOf : System -> DestroyMessage
   val otherMessageOf : System -> OtherMessage

 datatype callbacktype = MouseCB 
                       | ControlCB of Id
                       | TimerCB of Id
                       | KeyCB
                       | ScrollCB 
                       | RefreshCB
                       | DestroyCB
                       | OtherCB
                       
   val getcallbacktype : System -> callbacktype
end;

signature importUserFnSig =
sig
    type MLHMENU
    type MLHWND
    type MenuFlag
    type ScrollBarType
        type Rectangle

    val CreatePopupMenu : unit -> MLHMENU
    val CreateMenu : unit -> MLHMENU
    val SetMenu : MLHWND * MLHMENU -> bool
    val AppendMenu : MLHMENU * MenuFlag * int * string -> bool
    val AppendSubMenu : MLHMENU * MenuFlag * MLHMENU * string -> bool
    val UpdateWindow : MLHWND -> bool
    val SetScrollRange : MLHWND * ScrollBarType * int * int * bool -> bool 
    val SetScrollPos : MLHWND * ScrollBarType * int * bool -> int
    val EnableWindow : MLHWND * bool -> bool
    val GetParent : MLHWND -> MLHWND
    val GetWindowRect : MLHWND -> bool * Rectangle
end;

signature importUserFn2Sig =
sig
   type vol
   type MLHWND
   datatype Color = Rgb of int * int * int
   type MLHBRUSH
   type WindowStyle
   type WindowStyleExtended
   type ClassStyle
   type ButtonStyle
   type ComboBoxStyle
   type EditStyle
   type ListBoxStyle
   type ScrollBarStyle
   type StaticStyle
   type IconIdentifier
   type CursorIdentifier
   datatype MLHICON = MLHICON of vol
   datatype MLHCURSOR = MLHCURSOR of vol
   type MLHMENU

   val CreateSolidBrush : Color -> MLHBRUSH

   val mkWindow : WindowStyleExtended * ClassStyle * MLHICON * MLHCURSOR
                  * MLHBRUSH * string * WindowStyle * int * int * int 
                  * int * MLHWND * MLHMENU -> MLHWND 
   val mkButton : string * ButtonStyle * int * int * int * int * int * MLHWND -> MLHWND 
   val mkComboBox : string * ComboBoxStyle * int * int * int * int * int * MLHWND -> MLHWND
   val mkEdit : string * EditStyle * int * int * int * int * int * MLHWND -> MLHWND
   val mkListBox : string * ListBoxStyle * int * int * int * int * int * MLHWND -> MLHWND
   val mkScrollBar : string * ScrollBarStyle * int * int * int * int * int * MLHWND -> MLHWND
   val mkStatic : string * StaticStyle * int * int * int * int * MLHWND -> MLHWND

   val SetTimer : MLHWND * int * int -> bool
   val KillTimer : MLHWND * int -> bool
   val ShowWindow : MLHWND -> bool

   val LoadIcon : IconIdentifier -> MLHICON 
   val LoadCursor : CursorIdentifier -> MLHCURSOR

   type Rectangle
   datatype Section = Nothing | Section of Rectangle

   val Union: Rectangle -> Rectangle -> Rectangle
   val RightR: Rectangle -> int
   val BottomR: Rectangle -> int


end;

signature importGdiFnSig =
sig
end;

functor GadgetFct ( structure CInterface : importCInterfaceSig
                    structure ConstType : importConstTypeSig
                    structure Type : importTypeSig
                    structure Handler : importHandlerSig
                    structure MsgFn : importMsgFnSig
                    structure UserFn : importUserFnSig
                    structure UserFn2 : importUserFn2Sig
                    structure GdiFn : importGdiFnSig 
                  
                    sharing type MsgFn.Id = Type.Id
                    sharing type ConstType.MenuFlag =
                                 UserFn.MenuFlag
                    sharing type ConstType.ScrollBarType =
                                 UserFn.ScrollBarType
                    sharing type ConstType.ScrollBarStyle =
                                 UserFn2.ScrollBarStyle
                    sharing type ConstType.ClassStyle =
                                 UserFn2.ClassStyle
                    sharing type ConstType.ComboBoxStyle =
                                 UserFn2.ComboBoxStyle
                    sharing type ConstType.ListBoxStyle =
                                 UserFn2.ListBoxStyle
                    sharing type ConstType.StaticStyle =
                                 UserFn2.StaticStyle
                    sharing type ConstType.EditStyle =
                                 UserFn2.EditStyle
                    sharing type ConstType.WindowStyleExtended =
                                 UserFn2.WindowStyleExtended
                    sharing type CInterface.vol =
                                 Type.vol =
                                 UserFn2.vol
                    sharing type ConstType.WindowStyle =
                                 UserFn2.WindowStyle
                    sharing type Handler.MLHMENU =
                                 UserFn.MLHMENU =
                                 UserFn2.MLHMENU
                    sharing type ConstType.ButtonStyle =
                                 UserFn2.ButtonStyle
                    sharing type Handler.window = 
                                 UserFn.MLHWND = 
                                 UserFn2.MLHWND =
                                 Type.MLHWND
                    sharing type Handler.System =
                                 MsgFn.System
                    sharing type ConstType.IconIdentifier =
                                 UserFn2.IconIdentifier
                    sharing type ConstType.CursorIdentifier =
                                 UserFn2.CursorIdentifier
                  ) =
struct

local
  open CInterface
  open ConstType
  open Type
  open Handler
  open MsgFn
  open UserFn
  open UserFn2
  open GdiFn

in
  
  fun concat [] = [] | concat (a::x) = a@concat x

  type position = int*int

  type 'a handler = 'a -> unit

  type 'a port = 'a handler list ref ;

  datatype 'a inport  = In  of 'a port
       and 'a outport = Out of 'a port;

  val inport = In
  and outport = Out;

  fun connectIn (In (p as ref fs)) f = (p := f::fs);


abstype 'a command = Command of unit -> 'a
    and 'a drawcommand = DrawCommand of MLHDC -> 'a
with

  fun mkCommand f = Command f;
  fun mkDrawCommand f = DrawCommand f;

  (* change name of next function to drawInWin *)

  fun mkWindowCommand (DrawCommand f) w = 
                Command ( fn () => (if HasValidDeviceContextRTS(w) 
                                         then f (GetDeviceContextRTS w)
                                         else raise (Windows "Attempt to draw to no Drawable window") )
                        )

  fun execute (Command f) = f()
  infixr thenC seqC thenDC seqDC;
  fun (Command f) thenC g = Command (fn() => execute(g(f()))) ;
  fun (Command f) seqC (Command g) = Command (fn() => (f();g())) ;


  fun (DrawCommand f) thenDC G = DrawCommand( fn dc => let val fres = f dc
                                                         val DrawCommand g = G fres
                                                         in  g dc
                                                       end)
                                           
  fun (DrawCommand f) seqDC (DrawCommand g) = DrawCommand (fn dc => (f dc ; g dc))  

  fun returnC a = Command(fn() => a)
  fun returnDC a = DrawCommand (fn _ => a)

  type action = unit command;
  val noAction = Command(fn() => ()) : action
  fun mkAction f = mkCommand f : action
  fun actions [] = noAction
  |   actions [a] = a
  |   actions (a :: more) = a seqC (actions more)
end;

   
fun send (Out fs) a 
         = mkAction (fn () => (map (fn f => f a) (!fs); ()))
 
fun newport() = ref [] : '_a port;


fun sharedstate (stateref: 'a ref) (nextstatefun: 'b * 'a -> 'a * action) : 'b -> unit
       = let fun handlefun input
                    = let val (nextstate,action) = nextstatefun(input, !stateref)
                      in (stateref:= nextstate;execute action) end;
         in handlefun end;


type callbackcase = callbacktype * (System->unit)

fun makeScrollCB f win sref = let fun next(msg,s) = f(scrollMessageOf msg, win, s)
                                 val cb = sharedstate sref next
                             in (ScrollCB, cb) end;

fun makeMouseCB f win sref = let fun next(msg,s) = f(mouseMessageOf msg, win, s)
                                 val cb = sharedstate sref next
                             in (MouseCB, cb) end;

fun makeKeyCB f win sref = let fun next(msg,s) = f(keyMessageOf msg, win, s)
                               val cb = sharedstate sref next
                           in (KeyCB, cb) end;

fun makeRefreshCB f win sref = let fun next(msg,s) = f(refreshMessageOf msg, win, s)
                               val cb = sharedstate sref next
                           in (RefreshCB, cb) end;

fun makeControlCB (id,f) win sref = let fun next(msg,s) = f(controlMessageOf msg, win, s)
                                        val cb = sharedstate sref next
                                    in (ControlCB id, cb) end;

fun makeDestroyCB f win sref = let fun next(msg,s) = f(destroyMessageOf msg, win, s)
                               val cb = sharedstate sref next
                           in (DestroyCB, cb) end;

fun makeMenuCB (id,f) win sref = let fun next(msg,s) = f(win, s)
                                        val cb = sharedstate sref next
                                    in (ControlCB id, cb) end;

fun makeOtherCB f win sref = let fun next(msg,s) = f(otherMessageOf msg, win, s)
                               val cb = sharedstate sref next
                           in (OtherCB, cb) end;

fun makeTimerCB id action = (TimerCB id, fn s => execute action)

fun assoc [] f a = f a
|   assoc ((b,c)::more) f a = if a=b then c else assoc more f a

fun callbackwith cbs system
    = assoc cbs (fn _ => fn _ => ()) (getcallbacktype system) system

    (*  callbackwith : callbackcase list -> System -> unit
    Explanation:-
    assoc cbs default   converts the callbackcase list cbs to a mapping
                callbacktype -> (System->unit)
                using default on unmatched callbacktypes
    this mapping is applied to (messagetype system) to get the appropriate
    (System->unit) function which is finally applied to system
    *)


(* MENUS *)

(* datatype label = IconLabel of Icon   |   Label of string *)
type label = string

datatype 's menuItem = MenuItem of label * ('s menuAttrib list)
                     | Popup of label * 's menuItem list
                     | Separator

and      's menuAttrib = MenuFunction of window * 's -> 's * action
                     (*  | ItemAccelerator of string *) 
               | ItemId of Id
                       | ItemChecked
               | ItemDisabled
               | ItemBreak
               | ItemBarBreak

fun getMenuFun [] = None
|   getMenuFun (MenuFunction f::_) = Some f
|   getMenuFun (_::more) = getMenuFun more;

fun getItemId [] = newId()        (* generates a fresh id if not supplied by user *)
|   getItemId (ItemId i::_) = i
|   getItemId (_::more) = getItemId more;

fun getItemFlags [] = []
|   getItemFlags (ItemChecked::more) = MF_CHECKED::getItemFlags more
|   getItemFlags (ItemDisabled::more) = MF_GRAYED::getItemFlags more
|   getItemFlags (ItemBreak::more) = MF_MENUBREAK::getItemFlags more
|   getItemFlags (ItemBarBreak::more) = MF_MENUBARBREAK::getItemFlags more
|   getItemFlags (_::more) = getItemFlags more

fun makeItemFlag atts = orLE MenuFlagE (getItemFlags atts)


fun addMenuItems hmenu [] = []
|   addMenuItems hmenu (Separator::more) 
    = let val u = AppendMenu(hmenu, MF_SEPARATOR, 0, "")
      in addMenuItems hmenu more end
|   addMenuItems hmenu (MenuItem(s,atts)::more)
    = let val id = getItemId atts
              val state = makeItemFlag atts 
          val u = AppendMenu(hmenu, state, intOfId id, s)
              val menufun = getMenuFun atts
          in
          case menufun of None => addMenuItems hmenu more
                            | Some f => (id,f)::addMenuItems hmenu more
          end
|   addMenuItems hmenu (Popup(s,items)::more)
    = let val submenu = CreatePopupMenu ()
              val label = s
              val u = AppendSubMenu(hmenu, MF_POPUP, submenu, label)
              val funs = addMenuItems submenu items 
          in
          funs @ addMenuItems hmenu more
          end

fun createMenu(items)
         = let val hmenu = CreateMenu ()
           in
        (hmenu, addMenuItems hmenu items)
           end

(* WINDOWS *)

datatype 's winAttrib
          = WinMouse of MouseMessage * window * 's -> 's * action
          | WinKey of KeyMessage * window * 's -> 's * action
          | WinRefresh of RefreshMessage * window * 's -> 's * action
          | WinMenuBar of 's menuItem list
          | WinScroll of ScrollMessage * window * 's -> 's * action
          | WinIcon of IconIdentifier
          | WinBackground of Color
          | WinDrawable
          | WinSize of int * int
          | WinName of string
          | WinStyle of WindowStyle list
          | WinExStyle of WindowStyleExtended list
          | WinClassStyle of ClassStyle list
          | WinCursor of CursorIdentifier
          | WinDestroy of DestroyMessage * window * 's -> 's * action
          | WinMessage of OtherMessage * window * 's -> 's * action
    
fun getWinDestroy [] = None
|   getWinDestroy (WinDestroy b :: more) = Some b
|   getWinDestroy (_ :: more) = getWinDestroy more

fun getWinDrawable [] = false
|   getWinDrawable (WinDrawable :: more) = true
|   getWinDrawable (_ :: more) = getWinDrawable more

fun getWinScroll [] = None
|   getWinScroll (WinScroll b :: more) = Some b
|   getWinScroll (_ :: more) = getWinScroll more

fun getWinName [] = ""
|   getWinName (WinName s::more) = s
|   getWinName (_::more) = getWinName more

fun getWinSize [] = (CW_USEDEFAULT,CW_USEDEFAULT)
|   getWinSize (WinSize s::more) = s
|   getWinSize (_::more) = getWinSize more

fun getWinMenu [] = None
|   getWinMenu (WinMenuBar items :: more) = Some items
|   getWinMenu (_ :: more) = getWinMenu more

fun getWinMouse [] = None
|   getWinMouse (WinMouse b :: more) = Some b
|   getWinMouse (_ :: more) = getWinMouse more

fun getWinKey [] = None
|   getWinKey (WinKey b :: more) = Some b
|   getWinKey (_ :: more) = getWinKey more

fun getWinRefresh [] = None
|   getWinRefresh (WinRefresh b :: more) = Some b
|   getWinRefresh (_ :: more) = getWinRefresh more

fun getWinOther [] = None
|   getWinOther (WinMessage b :: more) = Some b
|   getWinOther (_ :: more) = getWinOther more 

fun getWinStyle [] = [WS_OVERLAPPEDWINDOW]
|   getWinStyle (WinStyle b :: more) = b
|   getWinStyle (_ :: more) = getWinStyle more

fun getWinExStyle [] = []
|   getWinExStyle (WinExStyle b :: more) = b
|   getWinExStyle (_ :: more) = getWinExStyle more

fun getWinIcon [] = IDI_APPLICATION
|   getWinIcon (WinIcon b :: more) = b
|   getWinIcon (_ :: more) = getWinIcon more

fun getWinClassStyle [] = [CS_HREDRAW, CS_VREDRAW]
|   getWinClassStyle (WinClassStyle b :: more) = b
|   getWinClassStyle (_ :: more) = getWinClassStyle more

fun getWinBackground [] = Rgb(255,255,255)
|   getWinBackground (WinBackground b :: more) = b
|   getWinBackground (_ :: more) = getWinBackground more

fun getWinCursor [] = IDC_ARROW
|   getWinCursor (WinCursor b :: more) = b
|   getWinCursor (_ :: more) = getWinCursor more

fun winMake parent (x,y) attribs (* doesn't deal with menu or scroll bars *)
       = let
         val exstyle = orLE WindowStyleExtendedE (getWinExStyle attribs)
         val cstyle = orLE ClassStyleE (getWinClassStyle attribs)
         val icon = LoadIcon (getWinIcon attribs)
         val cursor = LoadCursor (getWinCursor attribs)
         val background = CreateSolidBrush(getWinBackground attribs)
         val label = getWinName attribs
         val style = orLE WindowStyleE (getWinStyle attribs)
         val (w,h) = getWinSize attribs
     in
        mkWindow(exstyle, cstyle, icon, cursor, background,
                          label, style, x,y,w,h,parent, nullMenu() )
     end

(* BUTTONS *)

datatype 's buttonAttrib
        = ButtonLabel of string
        | ButtonSize of int * int
        | ButtonId of Id
        | ButtonFunction of ControlMessage * window * 's -> 's * action
        | ButtonStyle of ButtonStyle list

fun getButtonId [] = newId()
|   getButtonId (ButtonId id::more) = id
|   getButtonId (_::more) = getButtonId more

fun getButtonLabel [] = ""
|   getButtonLabel (ButtonLabel s::more) = s
|   getButtonLabel (_::more) = getButtonLabel more

fun getButtonFunction [] = None
|   getButtonFunction (ButtonFunction b::more) = Some b
|   getButtonFunction (_::more) = getButtonFunction more

fun getButtonStyle [] = [BS_CHILD]
|   getButtonStyle (ButtonStyle b::more) = b
|   getButtonStyle (_::more) = getButtonStyle more

fun getButtonSize [] = (CW_USEDEFAULT, CW_USEDEFAULT)
|   getButtonSize (ButtonSize b::more) = b
|   getButtonSize (_::more) = getButtonSize more

fun buttonMake parent (x,y) attribs
       = let val (w,h) = getButtonSize attribs
         val label = getButtonLabel attribs
         val style = orLE  ButtonStyleE (getButtonStyle attribs)
         val buttonId = getButtonId attribs
     in
         (mkButton( label, style, x,y,w,h,intOfId buttonId,parent),buttonId)
     end

(* SCROLLBARS *)

datatype Orientation = OVertical | OHorizontal

datatype 's scrollBarAttrib
        = ScrollOrientation of Orientation
        | ScrollSize of int * int
        | ScrollId of Id
        | ScrollFunction of window * 's -> 's * action
        | ScrollStyle of ScrollBarStyle list
        | ScrollRange of int * int
        | ScrollInitVal of int

fun getScrollOrientation [] = OHorizontal
|   getScrollOrientation (ScrollOrientation b::more) = b
|   getScrollOrientation (_::more) = getScrollOrientation more

fun getScrollSize [] = (CW_USEDEFAULT, CW_USEDEFAULT)
|   getScrollSize (ScrollSize b::more) = b
|   getScrollSize (_::more) = getScrollSize more

fun getScrollId [] = newId ()
|   getScrollId (ScrollId b::more) = b
|   getScrollId (_::more) = getScrollId more

fun getScrollFunction [] = None
|   getScrollFunction (ScrollFunction b::more) = Some b
|   getScrollFunction (_::more) = getScrollFunction more

fun getScrollStyle [] = [SBS_HORZ,SBS_CHILD]
|   getScrollStyle (ScrollStyle b::more) = b
|   getScrollStyle (_::more) = getScrollStyle more

fun getScrollRange [] = (1,100)
|   getScrollRange (ScrollRange b::more) = b
|   getScrollRange (_::more) = getScrollRange more

fun getScrollInitVal [] = 1
|   getScrollInitVal (ScrollInitVal b::more) = b
|   getScrollInitVal (_::more) = getScrollInitVal more

fun scrollbarMake parent (x,y) attribs 
       = let val (w,h) = getScrollSize attribs
             val label = ""
             val style = orLE ScrollBarStyleE (getScrollStyle attribs)

             val (v1,v2) = getScrollRange attribs
             val v = getScrollInitVal attribs
              
             val orient = (case (getScrollOrientation attribs) of
                              OHorizontal => SBS_HORZ
                            | OVertical   => SBS_VERT )

             val scrollId = getScrollId attribs

             val w = mkScrollBar(label,orLE ScrollBarStyleE [style,orient], 
                                 x,y,w,h,intOfId scrollId,parent)
             val u = SetScrollRange (w, SB_CTL, v1, v2, false)
             val u = SetScrollPos   (w, SB_CTL, v, false) 
     in
        (w,scrollId)
     end

(* COMBOBOXES *)

datatype 's comboBoxAttrib
        = ComboBoxStyle of ComboBoxStyle list
        | ComboBoxLabel of string
        | ComboBoxSize of int * int
        | ComboBoxId of Id
        | ComboBoxFunction of ControlMessage * window * 's -> 's * action

fun getComboBoxStyle [] = [CBS_SIMPLE,CBS_CHILD]
|   getComboBoxStyle (ComboBoxStyle b::more) = b
|   getComboBoxStyle (_::more) = getComboBoxStyle more

fun getComboBoxLabel [] = ""
|   getComboBoxLabel (ComboBoxLabel b::more) = b
|   getComboBoxLabel (_::more) = getComboBoxLabel more

fun getComboBoxSize [] = (CW_USEDEFAULT,CW_USEDEFAULT)
|   getComboBoxSize (ComboBoxSize b::more) = b
|   getComboBoxSize (_::more) = getComboBoxSize more

fun getComboBoxId [] = newId()
|   getComboBoxId (ComboBoxId b::more) = b
|   getComboBoxId (_::more) = getComboBoxId more

fun getComboBoxFunction [] = None
|   getComboBoxFunction (ComboBoxFunction b::more) = Some b
|   getComboBoxFunction (_::more) = getComboBoxFunction more

fun comboBoxMake parent (x,y) attribs
       = let val (w,h) = getComboBoxSize attribs
         val label = getComboBoxLabel attribs
         val style = orLE  ComboBoxStyleE (getComboBoxStyle attribs)
         val comboboxId = getComboBoxId attribs
     in
        (mkComboBox( label, style, x,y,w,h,intOfId comboboxId,parent),comboboxId)
     end

(* EDITS *)

datatype 's editAttrib
        = EditStyle of EditStyle list
        | EditLabel of string
        | EditSize of int * int
        | EditId of Id
        | EditFunction of ControlMessage * window * 's -> 's * action

fun getEditStyle [] = [ES_AUTOHSCROLL,ES_BORDER,ES_CHILD]
|   getEditStyle (EditStyle b::more) = b
|   getEditStyle (_::more) = getEditStyle more

fun getEditLabel [] = ""
|   getEditLabel (EditLabel b::more) = b
|   getEditLabel (_::more) = getEditLabel more

fun getEditSize [] = (CW_USEDEFAULT,CW_USEDEFAULT)
|   getEditSize (EditSize b::more) = b
|   getEditSize (_::more) = getEditSize more

fun getEditId [] = newId()
|   getEditId (EditId b::more) = b
|   getEditId (_::more) = getEditId more

fun getEditFunction [] = None
|   getEditFunction (EditFunction b::more) = Some b
|   getEditFunction (_::more) = getEditFunction more

fun editMake parent (x,y) attribs
       = let val (w,h) = getEditSize attribs
         val label = getEditLabel attribs
         val style = orLE  EditStyleE (getEditStyle attribs)
         val editId = getEditId attribs
     in
         (mkEdit(label,style,x,y,w,h,intOfId editId,parent),editId)
     end

(* LISTBOXES *)

datatype 's listBoxAttrib
        = ListBoxStyle of ListBoxStyle list
        | ListBoxLabel of string
        | ListBoxSize of int * int
        | ListBoxId of Id
        | ListBoxFunction of ControlMessage * window * 's -> 's * action

fun getListBoxStyle [] = [LBS_STANDARD,LBS_CHILD]
|   getListBoxStyle (ListBoxStyle b::more) = b
|   getListBoxStyle (_::more) = getListBoxStyle more

fun getListBoxLabel [] = ""
|   getListBoxLabel (ListBoxLabel b::more) = b
|   getListBoxLabel (_::more) = getListBoxLabel more

fun getListBoxSize [] = (CW_USEDEFAULT,CW_USEDEFAULT)
|   getListBoxSize (ListBoxSize b::more) = b
|   getListBoxSize (_::more) = getListBoxSize more

fun getListBoxId [] = newId()
|   getListBoxId (ListBoxId b::more) = b
|   getListBoxId (_::more) = getListBoxId more

fun getListBoxFunction [] = None
|   getListBoxFunction (ListBoxFunction b::more) = Some b
|   getListBoxFunction (_::more) = getListBoxFunction more

fun listBoxMake parent (x,y) attribs
       = let val (w,h) = getListBoxSize attribs
         val label = getListBoxLabel attribs
         val style = orLE ListBoxStyleE (getListBoxStyle attribs)
         val listboxId = getListBoxId attribs  
     in
        (mkListBox (label,style,x,y,w,h,intOfId listboxId,parent),listboxId)
     end

(* STATICS *)

datatype 's staticAttrib
        = StaticStyle of StaticStyle list
        | StaticLabel of string
        | StaticSize of int * int

fun getStaticStyle [] = [SS_SIMPLE,SS_CHILD]
|   getStaticStyle (StaticStyle b::more) = b
|   getStaticStyle (_::more) = getStaticStyle more

fun getStaticLabel [] = ""
|   getStaticLabel (StaticLabel b::more) = b
|   getStaticLabel (_::more) = getStaticLabel more

fun getStaticSize [] = (CW_USEDEFAULT,CW_USEDEFAULT)
|   getStaticSize (StaticSize b::more) = b
|   getStaticSize (_::more) = getStaticSize more

fun staticMake parent (x,y) attribs
       = let val (w,h) = getStaticSize attribs
         val label = getStaticLabel attribs
         val style = orLE StaticStyleE (getStaticStyle attribs)
     in
         mkStatic (label,style,x,y,w,h,parent)
     end


(**********************************************************
  The following type is returned when a gadget is started,
  and contains some gadget control 
 **********************************************************)

datatype gadgetresult 
     = GR of {
              wins: window list,            (* [] for Abstract gadgets, more than one for groups *)
              show: action,         (* shows window and any children windows *)
              kill: action,         (* Only used to terminate top level, closes then killsprocess *)
              close: action,            (* Closesdown window and children, bottom up *)
              callbacks: callbackcase list, (* Any callbacks which need to be handled by parent *)
              children: gadgetresult list   (* Results of starting children gadgets *)
             }

fun getwinsGR(GR{wins = wins,...}) = wins
fun getshowGR(GR{show = show,...}) = show
fun getkillGR(GR{kill=kill,...}) = kill
fun getcloseGR(GR{close=close,...}) = close
fun getcbsGR(GR{callbacks=callbacks,...}) = callbacks
fun getchildrenGR(GR{children=children,...}) = children


(*******************************************************************************************
No longer needed:    fun killgroupprocess grlist = noAction
 *******************************************************************************************)

fun groupGR pid grlist  (* Extra PID argument needed for kill, forked processes dealt with by forkedGadget *)
                   = GR{wins      = concat(map  getwinsGR  grlist),
                        show      = actions(map getshowGR grlist),
                        kill      = actions(map getcloseGR grlist @ [mkAction(fn()=>killprocess pid)]),
                        close     = actions(map getcloseGR grlist),
                        callbacks = concat(map getcbsGR grlist),
                        children  = grlist
                       }

type gadgetargs = ProcessID * window * position
datatype gadget = Gadget of (gadgetargs -> gadgetresult)

fun startGadget (Gadget g) args   (** should be made local -- only runGadget exported **)
       = g args;

fun forkedGadget G 
       = Gadget(fn (pid0,parent,position) =>
        let val pid = newProcessID()
                    val u = RunProcess pid
            val GR{wins,show,kill,close,callbacks,children} = startGadget G (pid,parent,position)
                    val killboth = actions[mkAction(fn () => killprocess pid0), kill]
                in
                    (* closing a forked gadget requires the forked process to be killed as well.
                       killing now requires 2 processes to be killed
                    *)
                   GR{wins=wins,
                      show=show,
                      kill=killboth,
                      close=kill,
                      callbacks = callbacks,
                      children = children
                     }
        end);

(*
   runGadget : gadget -> (int*int) -> gadgetresult
   This is a primitive function to start a gadget (generating a new process and null parent).
   It returns a gadgetresult which contains a kill action which can be executed to terminate the
   running gadget cleanly.
*)

fun runGadget g position
      = let val pid = newProcessID()
            val shell = MLHWND (toCint 0)
        val gr = startGadget g (pid,shell,position)
        in 
        (execute(getshowGR gr) ; RunProcess pid; gr ) 
        end;

(*
  runApplication : (action -> gadget) -> (int*int) -> gadgetresult
  This function is the main one intended for users to run application gadgets.
  runApplication requires an argument (makeGusing: action -> gadget) rather than just a gadget.
  The argument for makeGusing will be supplied internally and will be a killer action to
  cleanly terminate the running gadget with all children, etc. Thus the argument
  'makeGusing' can build any gadget using this killer action wherever the user wants (e.g. attached to
  a child button).
  If the killer action is executed when the gadget is running, it sends () on a private
  internal channel c. When the made gadget (makeGusing killer) is started with the more primitive
  runGadget, it returns a gadget result (gr) which includes its own clean kill action (kill). This
  kill action deals with children, windows and the process on which the gadget is running.
  It does not exist before the gadget is run so could not be used directly to make the gadget.
  A separate process is forked to receive the () on channel c. It merely executes this resulting
  kill returned in the gadget result gr when it receives () on c.
  Finally, a new gadget result is returned. This is virtually the same as gr except that the kill
  has to be modified to do the old kill and deal with the waiting forked process as well.
  In fact the new killer action is exactly what is required here, because it sends () on c
  which causes the forked process both to do the kill for us and to finish!
 *)

local
 open Process
in

fun runApplication (makeGusing : action -> gadget) position =
let val c = channel () : unit channel
    val killall = mkAction (fn () => send (c,()))
    val GR{kill,wins,close,show,callbacks,children} = runGadget (makeGusing killall) position
    val u = fork (fn () => (receive c ; execute kill))
in  GR{kill=killall,
       wins=wins,
       close=close,
       show=show,
       callbacks=callbacks,
       children=children}
end

end;

fun offset(a:int, b:int) (x,y) = (a+x, b+y);

fun simpleGroupGadget children (* gadgets paired with an offset *)
    = Gadget(fn (pid,parent,pos) =>
         let fun startchild (g,shift) = startGadget g (pid,parent,offset pos shift)
         in groupGR pid (map startchild children)
         end) ;

(***************************************************************

(* For Gadget Group Layout Functions:  *)

(* Retrieve from a gadgetresult the overall rectangle occupied by the gadget
   A section is (already) defined as Section of rectangle | Nothing
   and we need Nothing for abstract gadgets

   imports needed:  GetWindowRect
                    type Rectangle
                    datatype Section =  Nothing | Section of Rectangle
                    val Union: Rectangle -> Rectangle -> Rectangle
                    val RightR: Rectangle -> int
                    val BottomR: Rectangle -> int
*)
fun bigUnion [] = error "union of no rectangles"
|   bigUnion [r] = r
|   bigUnion (r1::more) = Union r1 (bigUnion more)

fun rectofGR gr
     = (case getwinsGR gr
        of [] => Nothing
        |  somewins => Section(bigUnion(map GetWindowRect somewins))
       )

(* Simple Layout:  (rows with tops aligned & columns left aligned)
   does not require adjustment after creation of windows (other alignments do)
*)

datatype 'a layout = PositionAt of ((int*int) * 'a layout)
                   | Row of int * 'a list                   (* int for spacing *)
                   | Column of int * 'a list



fun hmove n (x,y) Nothing = (x,y)
|   hmove n (x,y) (Section r) = (RightR r + n,y)
fun vmove n (x,y) Nothing = (x,y)
|   vmove n (x,y) (Section r) = (x,BottomR r + n)

fun layoutGadget gs =
    = Gadget(fn (pid,parent,pos) =>
         let fun startPos (PositionAt (shift,gl)) pos = startPos gl (offset pos shift)
             |   startPos (Row (n,gs)) pos = startAll (hmove n) gs pos
             |   startPos (Column(n,gs)) pos = startAll (vmove n) gs pos
             and startAll f [] pos = []
             |   startAll f (g::gs) pos = let val gr = startGadget g (pid,parent,pos)
                                              val newpos = f pos (rectofGR gr)
                                              val grs = startAll f gs newpos
                                          in gr::grs end
         in groupGR pid (startPos gs pos)
         end) ;

******************************************************************)

val noChild = Gadget(fn (pid,parent,pos) => groupGR pid [])

fun nobehaveGadget a = 
let fun b w (m,s) = (s,noAction)
in a b (inport (newport ()))
end;

(*
    winGadget : 's winAttrib list               attributes
        -> 's                       start state
        -> child list                   positions and gadgets
        -> (window -> ('a * 's) -> ('s * action))   high level behav'r
        -> 'a inport                    for high level messages
        -> gadget

    winGadget winAttribs startstate children b inport

   This creates a parent window gadget with high level behavior determined
   by b and startstate (and connected to inport) along with options/attributes
   described in winattribs some of which share the state.
   Children are gadgets (plus a  position relative to parent area)
   which do not share the state (they have their own state when made and
   communicate via message sending). Internally, however, this is acheived
   by children and options creating callbacks which are collected together
   in the parent
 *)

fun winGadget winAttribs startstate child b inport
      = Gadget(fn (pid, parent, position) =>
            let
    (* create win using parent, position and other info from winDef
       null menu handle for main win.  Register window on RTS *)
        val window = winMake parent position winAttribs
        val u = AddWindowRTS(window,pid,getWinDrawable winAttribs)

    (* set up shared state *)
        val winstate = ref startstate

    (* set up abstract behaviour: DELETE NEXT 3 LINES for winGadget0 *)
        val handlefun = sharedstate winstate (b window)
        fun high a = ( delegate pid (fn() => handlefun a) ; () )
        val u = connectIn inport high

    (* Create menu if defined in windef and return callbackcases *)
        val menuCB : callbackcase list
             = case getWinMenu winAttribs
             of None =>  []
             |  Some m =>   let val (hmenu, behaviours) = createMenu m
                                 (* behaviours :  (Id * (window * 's -> 's * action)) list *)
                            fun makeCB itm = makeMenuCB itm window winstate
                        in (SetMenu(window, hmenu); map makeCB behaviours) end

    (* ScrollBar callbackcase *)
        val scrollCB = case getWinScroll winAttribs
                of None =>  []
                |  Some b => [makeScrollCB b window winstate]

    (* Mouse callbackcase *)
        val mouseCB = case getWinMouse winAttribs
                of None =>  []
                |  Some b => [makeMouseCB b window winstate]
    (* Key callbackcase *)
        val keyCB = case getWinKey winAttribs
                of None =>  []
                |  Some b => [makeKeyCB b window winstate]

    (* Refresh callbackcase *)
        val refreshCB = case getWinRefresh winAttribs
                of None =>  []
                |  Some b => [makeRefreshCB b window winstate]

    (* Destroy callbackcase *)
        val destroyCB = case getWinDestroy winAttribs
                of None =>  []
                |  Some b => [makeDestroyCB b window winstate]

    (* Other callbackcase *)
        val otherCB = case getWinOther winAttribs
                of None =>  []
                |  Some b => [makeOtherCB b window winstate]

    (* Create children and get result callbacks *)
        val childGR = startGadget child (pid,window,(0,0))
                val childcallbacks = getcbsGR childGR
    (* Collect All callbacks and deal with them *)
                val callbacks = concat[menuCB,mouseCB,keyCB,refreshCB,
                                       otherCB,scrollCB,destroyCB,childcallbacks]

                val u = AddCallbackML(window,callbackwith callbacks)
        val parentcallbacks = [] (* None for Parent *)
                val show = actions[mkAction (fn() => (ShowWindow window; UpdateWindow window;())),
                   getshowGR childGR
                                  ]
                val close = actions[getcloseGR childGR, mkAction(fn() => closeDownWin window)]
                val kill = actions[mkAction(fn() => killprocess pid),close]
                val wins = [window]
            in
                GR{wins = wins,
                   show = show, 
                   kill = kill, 
                   close = close,
                   callbacks = parentcallbacks, 
                   children = [childGR]
                  }
            end);

fun winGadgetBasic winAttribs startstate children = 
    nobehaveGadget (winGadget winAttribs startstate children)

fun abstractGadget startstate b inport (* No window *)
   = Gadget(fn (pid, parent, _) =>
       let val stateref = ref startstate
       val handlefun = sharedstate stateref b
           fun high a = ( delegate pid (fn () => handlefun a) ; ())
           val u = connectIn inport high
           val dummywin = nullWindow()
           val u = AddWindowRTS(dummywin,pid,false)
           val close = mkAction(fn() => closeDownWin dummywin)
           val kill = actions[mkAction(fn() => killprocess pid),close]
       in
            GR{wins = [],
               show = noAction,
               kill = kill,
               close = close , 
               callbacks = [], 
               children = []
              }
       end);

datatype repeaterControl = RepeaterStart
                         | RepeaterStop
                         | RepeaterInterval of int


fun repeaterGadget action startspeed inport(* No window *)
   = Gadget(fn (pid, parent, _) =>
       let datatype runningState = running of int
                                 | stopped of int
       
           (* timers should not be called with negative delays
              the function v (validate) forces speed to 0 
              if a negative value is specified *)
 
           fun mkValid speed = if speed >= 0 then speed else 0

           val timerId = newId()
           val stateref = ref (stopped (mkValid startspeed)) 


           fun stopgo (RepeaterStart,stopped speed) = 
                    (running speed,
                     mkAction(fn () => 
                               ( SetTimer(parent,intOfId timerId, speed);
                                 () ) ))

           |   stopgo (RepeaterStop,stopped speed)  = 
                    (stopped speed,noAction)

           |   stopgo (RepeaterInterval newspeed,stopped speed) = 
                    (stopped (mkValid newspeed),noAction)
           
           |   stopgo (RepeaterStart,running speed) = 
                    (running speed,noAction)

           |   stopgo (RepeaterStop,running speed)  = 
                    (stopped speed,
                     mkAction(fn () => 
                               ( KillTimer(parent, intOfId timerId);
                                 () ) ))

           |   stopgo (RepeaterInterval newspeed,running speed) = 
                    (running (mkValid newspeed),
                     mkAction(fn () => 
                               ( KillTimer(parent, intOfId timerId);
                                 SetTimer(parent,intOfId timerId, (mkValid newspeed));
                                 () ) ))

           val handlefun = sharedstate stateref stopgo
           fun high a = ( delegate pid (fn () => handlefun a) ; () )
           val u = connectIn inport high

           fun stopgoresult m =
           let val (newstate,act) = stopgo (m,running 0)
           in mkAction(fn () => ())
           end

       val cbs = [makeTimerCB timerId action]

       in
            GR{wins = [],
               show  = noAction,
               kill  = stopgoresult RepeaterStop,  (* makes sure it has stopped *)
               close = stopgoresult RepeaterStop, 
               callbacks = cbs, 
               children = []
              }
       end);


fun buttonGadget buttonAttribs startstate b inport
      = Gadget(fn (pid, parent, position) =>
            let
    (* create button using parent, position and other info from buttonDef *)
        val (window,buttonId) = buttonMake parent position buttonAttribs
        val u = AddWindowRTS(window,pid,false)

    (* set up shared state *)
        val winstate = ref startstate
    (* set up abstract behaviour *)
                val handlefun = sharedstate winstate (b window)
                fun high a = ( delegate pid (fn() => handlefun a) ; () )
                val u = connectIn inport high

    (* Click callbackcase *)
        val buttonCB = case getButtonFunction buttonAttribs
                of None =>  []
                |  Some b => [makeControlCB (buttonId,b) window winstate]

            val show = mkAction (fn() => (ShowWindow window; UpdateWindow window;()))
                val close = mkAction(fn() => closeDownWin window)
                val kill = actions[mkAction(fn() => killprocess pid),close]
                val wins = [window]
            in
                GR{wins = wins,
                   show = show, 
                   kill = kill, 
                   close = close,
                   callbacks = buttonCB,     (* pass callback to parent *)
                   children = []
                  }
            end);


fun buttonGadgetBasic buttonAttribs startstate = 
        nobehaveGadget (buttonGadget buttonAttribs startstate);


fun scrollGadget scrollAttribs startstate b inport
      = Gadget(fn (pid, parent, position) =>
            let
    (* create scrollbar using parent, position and other info from scrollAttribs *)
        val (window,scrollId) = scrollbarMake parent position scrollAttribs
        val u = AddWindowRTS(window,pid,false)

    (* set up shared state *)
        val winstate = ref startstate
    (* set up abstract behaviour *)
        val handlefun = sharedstate winstate (b window)
        fun high a = ( delegate pid (fn() => handlefun a) ; () )
        val u = connectIn inport high

    (* Click callbackcase *)
        val scrollCB = case getScrollFunction scrollAttribs
                of None =>  []
                |  Some b => [makeMenuCB (scrollId,b) window winstate]

            val show = mkAction (fn() => (ShowWindow window; UpdateWindow window;()))
                val close = mkAction(fn() => closeDownWin window)
                val kill = actions[mkAction(fn() => killprocess pid),close]
                val wins = [window]
            in
                GR{wins = wins,
                   show = show, 
                   kill = kill, 
                   close = close,
                   callbacks = scrollCB,     (* pass callback to parent *)
                   children = []
                  }
            end);

fun scrollGadgetBasic scrollAttribs startstate = 
        nobehaveGadget (scrollGadget scrollAttribs startstate);

fun comboboxGadget comboboxAttribs startstate b inport
      = Gadget(fn (pid, parent, position) =>
            let
    (* create combobox using parent, position and other info from comboboxAttribs *)
        val (window,comboboxId) = comboBoxMake parent position comboboxAttribs
        val u = AddWindowRTS(window,pid,false)

    (* set up shared state *)
        val winstate = ref startstate
    (* set up abstract behaviour *)
        val handlefun = sharedstate winstate (b window)
        fun high a = ( delegate pid (fn() => handlefun a) ; () )
        val u = connectIn inport high

    (* Click callbackcase *)
        val comboboxCB = case getComboBoxFunction comboboxAttribs
                of None =>  []
                |  Some b => [makeControlCB (comboboxId,b) window winstate]

            val show = mkAction (fn() => (ShowWindow window; UpdateWindow window;()))
                val close = mkAction(fn() => closeDownWin window)
                val kill = actions[mkAction(fn() => killprocess pid),close]
                val wins = [window]
            in
                GR{wins = wins,
                   show = show, 
                   kill = kill, 
                   close = close,
                   callbacks = comboboxCB, (* pass callback to parent *)
                   children = []
                  }
            end);

fun comboboxGadgetBasic comboboxAttribs startstate = 
        nobehaveGadget (comboboxGadget comboboxAttribs startstate);

fun editGadget editAttribs startstate b inport
      = Gadget(fn (pid, parent, position) =>
            let
    (* create edit using parent, position and other info from editAttribs *)
        val (window,editId) = editMake parent position editAttribs
        val u = AddWindowRTS(window,pid,false)

    (* set up shared state *)
        val winstate = ref startstate
    (* set up abstract behaviour *)
        val handlefun = sharedstate winstate (b window)
        fun high a = ( delegate pid (fn() => handlefun a) ; () )
        val u = connectIn inport high

    (* Click callbackcase *)
        val editCB = case getEditFunction editAttribs
                of None =>  []
                |  Some b => [makeControlCB (editId,b) window winstate]

            val show = mkAction (fn() => (ShowWindow window; UpdateWindow window;()))
                val close = mkAction(fn() => closeDownWin window)
                val kill = actions[mkAction(fn() => killprocess pid),close]
                val wins = [window]
            in
                GR{wins = wins,
                   show = show, 
                   kill = kill, 
                   close = close,
                   callbacks = editCB, (* pass callback to parent *)
                   children = []
                  }
            end);

fun editGadgetBasic editAttribs startstate = 
        nobehaveGadget (editGadget editAttribs startstate);

fun listboxGadget listboxAttribs startstate b inport
      = Gadget(fn (pid, parent, position) =>
            let
    (* create listbox using parent, position and other info from listboxAttribs *)
        val (window,listboxId) = listBoxMake parent position listboxAttribs
        val u = AddWindowRTS(window,pid,false)

    (* set up shared state *)
        val winstate = ref startstate
    (* set up abstract behaviour *)
        val handlefun = sharedstate winstate (b window)
        fun high a = ( delegate pid (fn() => handlefun a) ; () )
        val u = connectIn inport high

    (* Click callbackcase *)
        val listboxCB = case getListBoxFunction listboxAttribs
                of None =>  []
                |  Some b => [makeControlCB (listboxId,b) window winstate]

            val show = mkAction (fn() => (ShowWindow window; UpdateWindow window;()))
                val close = mkAction(fn() => closeDownWin window)
                val kill = actions[mkAction(fn() => killprocess pid),close]
                val wins = [window]
            in
                GR{wins = wins,
                   show = show, 
                   kill = kill, 
                   close = close,
                   callbacks = listboxCB,     (* pass callback to parent *)
                   children = []
                  }
            end);

fun listboxGadgetBasic listboxAttribs startstate = 
        nobehaveGadget (listboxGadget listboxAttribs startstate);

fun staticGadget staticAttribs
      = Gadget(fn (pid, parent, position) =>
            let
    (* create static using parent, position and other info from staticAttribs *)
        val window = staticMake parent position staticAttribs
        val u = AddWindowRTS(window,pid,false)

            val show = mkAction (fn() => (ShowWindow window; UpdateWindow window;()))
                val close = noAction
                val kill = actions[mkAction(fn() => killprocess pid),close]
                val wins = [window]
            in
                GR{wins = wins,
                   show = show, 
                   kill = kill, 
                   close = close,
                   callbacks = [],     (* pass callback to parent *)
                   children = []
                  }
            end);


(**** Running Dialogs ****)
                    
local
  open Process

  fun runGadgetDialog parentpid g position
      = let val pid = newProcessID()
            val shell = MLHWND (toCint 0)
            val gr = startGadget g (pid,shell,position)
        in 
        (execute(getshowGR gr) ; 
         RunProcess pid; 
         SetProcessRunStateRTS (parentpid,1);
         gr ) 
        end

  val ConsoleWindow = MLHWND (toCint 0)
in

fun modalDialogCmd w ( d : ('_a -> action) -> gadget ) position =
  mkCommand(fn () => 
   let val c = channel()
     fun result a = mkAction (fn () => send (c,a))

     val parentpid = GetpidRTS w

     val b = EnableProcessWindowsRTS (parentpid,false)

     val GR{kill,...} = runGadgetDialog parentpid (d result) position
     val r = receive c 

   in (execute kill ; 
       SetProcessRunStateRTS (parentpid,0) ;
       EnableProcessWindowsRTS (parentpid,true) ; 
       r)
   end )

(* The following function is used to spawn a modal dialog
   from the ML console. The console is blocked until the
   dialog has closed *) 

fun runModalDialog dlg position =
     execute (modalDialogCmd ConsoleWindow dlg position)

end

val runModelessDialog = runApplication


(**** Creating Dialogs ****)


fun modaldialogGadget winAttribs startstate children b inport = 
 winGadget ((WinStyle [WS_POPUPWINDOW]):: winAttribs)
           startstate
           children
           b
           inport
           
fun modaldialogGadgetBasic winAttribs startstate children = 
    nobehaveGadget (modaldialogGadget winAttribs startstate children)


val modelessdialogGadget = winGadget
val modelessdialogGadgetBasic = winGadgetBasic


fun localPort (f: '_a port -> gadget) :gadget =
       Gadget(fn (pid, parent, position) =>
         let val p = newport()
         in startGadget (f p) (pid,parent,position)
         end);


fun localId (f: Id -> gadget) :gadget =
      Gadget(fn (pid, parent, position) =>
         let val id = newId()
         in startGadget (f id) (pid,parent,position)
         end);


(* NB loop f p takes an existing port(p) and links
   f: '_a inport -> '_a outport -> gadget  in a loop. It connects up the inport
   so ONLY outport p can be used outside.  This allows external use of the 
   outport which would not be possible if the port was created internally
*)
fun loop f p = f (inport p) (outport p)  


end (* of local *)

end (* of functor GadgetFct *);



