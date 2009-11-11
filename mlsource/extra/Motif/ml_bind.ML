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

structure Motif: MOTIF_SIG =
struct

open MotifTypes;
open XmTypes;
open MotifCore;

local
  open XWindows;
  open XCall;
  open InitialiseCore;
in
    
(******************************************************************************)
(*                                                                            *)
(*      A few Xt operations                                                   *)
(*                                                                            *)
(******************************************************************************)
  val NoWidget:Widget = xcall (XCALL_NoWidget,0);
  
  fun WidgetID     (w:Widget):int  = xcall (XCALL_GetID,w);
  fun WidgetExists (w:Widget):bool = xcall (XCALL_ResourceExists,w);

  fun XtParseTranslationTable (s:string):XtTranslations =
    xcall (XCALL_XtParseTranslationTable,s);
  
  fun XtOverrideTranslations (w:Widget) (t:XtTranslations):unit =
    xcall (XCALL_XtOverrideTranslations,w,t);
  
  fun XtAugmentTranslations (w:Widget) (t:XtTranslations):unit =
    xcall (XCALL_XtAugmentTranslations,w,t);
  
  fun XtUninstallTranslations (w:Widget):unit =
    xcall (XCALL_XtUninstallTranslations,w);

(* SPF 14/2/94 not currently supported by run-time ...    
  fun XtTranslateTablePrint (t:XtTranslations):unit =
    xcall (XCALL_XtTranslateTablePrint,t);
... *)
  fun XtTranslateTablePrint (t:XtTranslations):unit =
    raise XWindows "XtTranslateTablePrint: not supported";
    
  fun XtParent  (w:Widget):Widget   = xcall (XCALL_XtParent,w);
  fun XtWindow  (w:Widget):Drawable = xcall (XCALL_XtWindow,w);
  fun XtDisplay (w:Widget):Display  = xcall (XCALL_XtDisplay,w);
  fun XtName    (w:Widget):string   = xcall (XCALL_XtName,w);
    
  fun XtAppInitialise  (D:string)
                       (A:string)
                       (C:string)
                       (F:string list)
                       (L:Arg list):Widget =
  let
    val shell   = xcall (XCALL_AppInitialise,D,A,C,F,XtSetExnVals L);
    val display = XtDisplay shell;
  in
    XStartDisplayHandler display;
    shell
  end;
  
  fun XtCreatePopupShell (name:string)
                         (parent:Widget)
                         (L:Arg list):Widget =
    xcall (XCALL_XtCreatePopupShell,name,parent,XtSetExnVals L);
  
  fun XtRealizeWidget   (w:Widget):unit = xcall (XCALL_XtRealizeWidget,w);
  fun XtUnrealizeWidget (w:Widget):unit = xcall (XCALL_XtUnrealizeWidget,w);
  fun XtDestroyWidget   (w:Widget):unit = xcall (XCALL_XtDestroyWidget,w);
  
  fun XtManageChildren   (L:Widget list):unit =
    xcall (XCALL_XtManageChildren,L);
    
  fun XtUnmanageChildren (L:Widget list):unit =
    xcall (XCALL_XtUnmanageChildren,L);
    
  fun XtManageChild   w = XtManageChildren   [w];
  fun XtUnmanageChild w = XtUnmanageChildren [w];
    
  fun XtSetValues (w:Widget) (L:Arg list):unit =
    xcall (XCALL_XtSetValues,w,XtSetExnVals L);
    
  fun XtSetValue w a = XtSetValues w [a];
    
  fun XtGetValues w = map (XtGetValue w);
      
(******************************************************************************)
(*                                                                            *)
(*      A few more Xt operations (SPF 6/12/94, 19/1/95)                       *)
(*                                                                            *)
(******************************************************************************)

  datatype XtGrabKind =
    XtGrabNone
  | XtGrabNonexclusive
  | XtGrabExclusive;
  
  fun XtPopup (w: Widget) XtGrabNone         : unit = xcall (XCALL_XtPopup,w,0)
    | XtPopup (w: Widget) XtGrabNonexclusive : unit = xcall (XCALL_XtPopup,w,1)
    | XtPopup (w: Widget) XtGrabExclusive    : unit = xcall (XCALL_XtPopup,w,2);

  fun XtPopdown (w: Widget) : unit = 
    xcall (XCALL_XtPopdown,w);

  fun XtMapWidget (w: Widget) : unit =
    xcall (XCALL_XtMapWidget,w);

  fun XtUnmapWidget (w: Widget) : unit =
    xcall (XCALL_XtUnmapWidget,w);

  fun XtIsManaged (w:Widget):bool =
    xcall (XCALL_XtIsManaged,w);
    
  fun XtIsRealized (w:Widget):bool =
    xcall (XCALL_XtIsRealized,w);

(******************************************************************************)
(*                                                                            *)
(*      Motif Callbacks                                                       *)
(*                                                                            *)
(******************************************************************************)
  datatype XtCallback =
    XmNactivateCallback
  | XmNapplyCallback
  | XmNarmCallback
  | XmNbrowseSelectionCallback
  | XmNcancelCallback
  | XmNcascadingCallback
  | XmNcommandChangedCallback
  | XmNcommandEnteredCallback
  | XmNdecrementCallback
  | XmNdefaultActionCallback
  | XmNdestroyCallback
  | XmNdisarmCallback
  | XmNdragCallback
  | XmNentryCallback
  | XmNexposeCallback
  | XmNextendedSelectionCallback
  | XmNfocusCallback
  | XmNgainPrimaryCallback
  | XmNhelpCallback
  | XmNincrementCallback
  | XmNinputCallback
  | XmNlosePrimaryCallback
  | XmNlosingFocusCallback
  | XmNmapCallback
  | XmNmessageCallback            (* this callback gets Message XEvents only *)
  | XmNmodifyVerifyCallback
  | XmNmotionVerifyCallback
  | XmNmultipleSelectionCallback
  | XmNnoMatchCallback
  | XmNokCallback
  | XmNpageDecrementCallback
  | XmNpageIncrementCallback
  | XmNpopdownCallback
  | XmNpopupCallback
  | XmNresizeCallback
  | XmNsimpleCallback
  | XmNsingleSelectionCallback
  | XmNtoBottomCallback
  | XmNtoTopCallback
  | XmNunmapCallback
  | XmNvalueChangedCallback;
  
  local
    fun Name XmNactivateCallback           = "activateCallback"
    |   Name XmNapplyCallback              = "applyCallback"
    |   Name XmNarmCallback                = "armCallback"
    |   Name XmNbrowseSelectionCallback    = "browseSelectionCallback"
    |   Name XmNcancelCallback             = "cancelCallback"
    |   Name XmNcascadingCallback          = "cascadingCallback"
    |   Name XmNcommandChangedCallback     = "commandChangedCallback"
    |   Name XmNcommandEnteredCallback     = "commandEnteredCallback"
    |   Name XmNdecrementCallback          = "decrementCallback"
    |   Name XmNdefaultActionCallback      = "defaultActionCallback"
    |   Name XmNdestroyCallback            = "destroyCallback"
    |   Name XmNdisarmCallback             = "disarmCallback"
    |   Name XmNdragCallback               = "dragCallback"
    |   Name XmNentryCallback              = "entryCallback"
    |   Name XmNexposeCallback             = "exposeCallback"
    |   Name XmNextendedSelectionCallback  = "extendedSelectionCallback"
    |   Name XmNfocusCallback              = "focusCallback"
    |   Name XmNgainPrimaryCallback        = "gainPrimaryCallback"
    |   Name XmNhelpCallback               = "helpCallback"
    |   Name XmNincrementCallback          = "incrementCallback"
    |   Name XmNinputCallback              = "inputCallback"
    |   Name XmNlosePrimaryCallback        = "losePrimaryCallback"
    |   Name XmNlosingFocusCallback        = "losingFocusCallback"
    |   Name XmNmapCallback                = "mapCallback"
    |   Name XmNmessageCallback            = "messageCallback"
    |   Name XmNmodifyVerifyCallback       = "modifyVerifyCallback"
    |   Name XmNmotionVerifyCallback       = "motionVerifyCallback"
    |   Name XmNmultipleSelectionCallback  = "multipleSelectionCallback"
    |   Name XmNnoMatchCallback            = "noMatchCallback"
    |   Name XmNokCallback                 = "okCallback"
    |   Name XmNpageDecrementCallback      = "pageDecrementCallback"
    |   Name XmNpageIncrementCallback      = "pageIncrementCallback"
    |   Name XmNpopdownCallback            = "popdownCallback"
    |   Name XmNpopupCallback              = "popupCallback"
    |   Name XmNresizeCallback             = "resizeCallback"
    |   Name XmNsimpleCallback             = "simpleCallback"
    |   Name XmNsingleSelectionCallback    = "singleSelectionCallback"
    |   Name XmNtoBottomCallback           = "toBottomCallback"
    |   Name XmNtoTopCallback              = "toTopCallback"
    |   Name XmNunmapCallback              = "unmapCallback"
    |   Name XmNvalueChangedCallback       = "valueChangedCallback";
  in  
    fun XtSetCallbacks (w:Widget) (L:(XtCallback * (Widget * 'a XEvent * '_b -> '_b)) list) (initialState:'_b) =
    let
      fun NameCallbacks []         = [] 
      |   NameCallbacks ((c,f)::T) = (Name c,f) :: NameCallbacks T;
      
      datatype 'a option = None | Some of 'a;
      
      fun FindMessageCallback []                          = None
      |   FindMessageCallback ((XmNmessageCallback,f)::_) = Some f 
      |   FindMessageCallback (_::T)                      = FindMessageCallback T;
      
      val M = FindMessageCallback L;
    in
    xcall (XCALL_SetCallbacks,w,NameCallbacks L,initialState);
      
      fn (ms:int) => fn (alpha:'a) =>
        case M of
          Some f => xcall (XCALL_InsertWidgetTimeout,w,ms,alpha,f)
        | None   => ()
    end;
  end;
    
  fun XtSetSensitive (w:Widget) (s:bool):unit =
    xcall (XCALL_XtSetSensitive,w,s);
    
  fun XtIsSensitive (w:Widget):bool =
    xcall (XCALL_XtIsSensitive,w);
  
(******************************************************************************)
(*                                                                            *)
(*      Motif Widget Creation Routines                                        *)
(*                                                                            *)
(******************************************************************************)
  local
    fun CreateXm (n:int)
                 (p:Widget)
                 (s:string)
                 (L:Arg list):Widget =
      xcall (n,p,s,XtSetExnVals L);
  in
    val XmCreateArrowButton         = CreateXm XCALL_XmCreateArrowButton;
    val XmCreateArrowButtonGadget   = CreateXm XCALL_XmCreateArrowButtonGadget;
    val XmCreateBulletinBoard       = CreateXm XCALL_XmCreateBulletinBoard;
    val XmCreateBulletinBoardDialog = CreateXm XCALL_XmCreateBulletinBoardDialog;
    val XmCreateCascadeButton       = CreateXm XCALL_XmCreateCascadeButton;
    val XmCreateCascadeButtonGadget = CreateXm XCALL_XmCreateCascadeButtonGadget;
    val XmCreateCommand             = CreateXm XCALL_XmCreateCommand;
    val XmCreateDialogShell         = CreateXm XCALL_XmCreateDialogShell;
    val XmCreateDrawingArea         = CreateXm XCALL_XmCreateDrawingArea;
    val XmCreateDrawnButton         = CreateXm XCALL_XmCreateDrawnButton;
    val XmCreateErrorDialog         = CreateXm XCALL_XmCreateErrorDialog;
    val XmCreateFileSelectionBox    = CreateXm XCALL_XmCreateFileSelectionBox;
    val XmCreateFileSelectionDialog = CreateXm XCALL_XmCreateFileSelectionDialog;
    val XmCreateForm                = CreateXm XCALL_XmCreateForm;
    val XmCreateFormDialog          = CreateXm XCALL_XmCreateFormDialog;
    val XmCreateFrame               = CreateXm XCALL_XmCreateFrame;
    val XmCreateInformationDialog   = CreateXm XCALL_XmCreateInformationDialog;
    val XmCreateLabel               = CreateXm XCALL_XmCreateLabel;
    val XmCreateLabelGadget         = CreateXm XCALL_XmCreateLabelGadget;
    val XmCreateList                = CreateXm XCALL_XmCreateList;
    val XmCreateMainWindow          = CreateXm XCALL_XmCreateMainWindow;
    val XmCreateMenuBar             = CreateXm XCALL_XmCreateMenuBar;
    val XmCreateMenuShell           = CreateXm XCALL_XmCreateMenuShell;
    val XmCreateMessageBox          = CreateXm XCALL_XmCreateMessageBox;
    val XmCreateMessageDialog       = CreateXm XCALL_XmCreateMessageDialog;
    val XmCreateOptionMenu          = CreateXm XCALL_XmCreateOptionMenu;
    val XmCreatePanedWindow         = CreateXm XCALL_XmCreatePanedWindow;
    val XmCreatePopupMenu           = CreateXm XCALL_XmCreatePopupMenu;
    val XmCreatePromptDialog        = CreateXm XCALL_XmCreatePromptDialog;
    val XmCreatePulldownMenu        = CreateXm XCALL_XmCreatePulldownMenu;
    val XmCreatePushButton          = CreateXm XCALL_XmCreatePushButton;
    val XmCreatePushButtonGadget    = CreateXm XCALL_XmCreatePushButtonGadget;
    val XmCreateQuestionDialog      = CreateXm XCALL_XmCreateQuestionDialog;
    val XmCreateRadioBox            = CreateXm XCALL_XmCreateRadioBox;
    val XmCreateRowColumn           = CreateXm XCALL_XmCreateRowColumn;
    val XmCreateScale               = CreateXm XCALL_XmCreateScale;
    val XmCreateScrollBar           = CreateXm XCALL_XmCreateScrollBar;
    val XmCreateScrolledList        = CreateXm XCALL_XmCreateScrolledList;
    val XmCreateScrolledText        = CreateXm XCALL_XmCreateScrolledText;
    val XmCreateScrolledWindow      = CreateXm XCALL_XmCreateScrolledWindow;
    val XmCreateSelectionBox        = CreateXm XCALL_XmCreateSelectionBox;
    val XmCreateSelectionDialog     = CreateXm XCALL_XmCreateSelectionDialog;
    val XmCreateSeparator           = CreateXm XCALL_XmCreateSeparator;
    val XmCreateSeparatorGadget     = CreateXm XCALL_XmCreateSeparatorGadget;
    val XmCreateSimpleCheckBox      = CreateXm XCALL_XmCreateSimpleCheckBox;
    val XmCreateSimpleMenuBar       = CreateXm XCALL_XmCreateSimpleMenuBar;
    val XmCreateSimpleOptionMenu    = CreateXm XCALL_XmCreateSimpleOptionMenu;
    val XmCreateSimplePopupMenu     = CreateXm XCALL_XmCreateSimplePopupMenu;
    val XmCreateSimplePulldownMenu  = CreateXm XCALL_XmCreateSimplePulldownMenu;
    val XmCreateSimpleRadioBox      = CreateXm XCALL_XmCreateSimpleRadioBox;
    val XmCreateText                = CreateXm XCALL_XmCreateText;
    val XmCreateTextField           = CreateXm XCALL_XmCreateTextField;
    val XmCreateToggleButton        = CreateXm XCALL_XmCreateToggleButton;
    val XmCreateToggleButtonGadget  = CreateXm XCALL_XmCreateToggleButtonGadget;
    val XmCreateWarningDialog       = CreateXm XCALL_XmCreateWarningDialog;
    val XmCreateWorkArea            = CreateXm XCALL_XmCreateWorkArea;
    val XmCreateWorkingDialog       = CreateXm XCALL_XmCreateWorkingDialog;
  end;
  
(******************************************************************************)
(*                                                                            *)
(*      Motif Widget Query Routines                                           *)
(*                                                                            *)
(******************************************************************************)
  fun XmIsArrowButton         (w:Widget):bool = xcall(XCALL_XmIsSomething, 1,w);
  fun XmIsArrowButtonGadget   (w:Widget):bool = xcall(XCALL_XmIsSomething, 2,w);
  fun XmIsBulletinBoard       (w:Widget):bool = xcall(XCALL_XmIsSomething, 3,w);
  fun XmIsCascadeButton       (w:Widget):bool = xcall(XCALL_XmIsSomething, 4,w);
  fun XmIsCascadeButtonGadget (w:Widget):bool = xcall(XCALL_XmIsSomething, 5,w);
  fun XmIsCommand             (w:Widget):bool = xcall(XCALL_XmIsSomething, 6,w);
  fun XmIsDesktopObject       (w:Widget):bool = xcall(XCALL_XmIsSomething, 7,w);
  fun XmIsDialogShell         (w:Widget):bool = xcall(XCALL_XmIsSomething, 8,w);
(* unsupported by solaris version of toolkit ...
  fun XmIsDisplayObject       (w:Widget):bool = xcall(XCALL_XmIsSomething, 9,w);
... *)
  fun XmIsDrawingArea         (w:Widget):bool = xcall(XCALL_XmIsSomething,10,w);
  fun XmIsDrawnButton         (w:Widget):bool = xcall(XCALL_XmIsSomething,11,w);
  fun XmIsExtObject           (w:Widget):bool = xcall(XCALL_XmIsSomething,12,w);
  fun XmIsFileSelectionBox    (w:Widget):bool = xcall(XCALL_XmIsSomething,13,w);
  fun XmIsForm                (w:Widget):bool = xcall(XCALL_XmIsSomething,14,w);
  fun XmIsFrame               (w:Widget):bool = xcall(XCALL_XmIsSomething,15,w);
  fun XmIsGadget              (w:Widget):bool = xcall(XCALL_XmIsSomething,16,w);
  fun XmIsLabel               (w:Widget):bool = xcall(XCALL_XmIsSomething,17,w);
  fun XmIsLabelGadget         (w:Widget):bool = xcall(XCALL_XmIsSomething,18,w);
  fun XmIsList                (w:Widget):bool = xcall(XCALL_XmIsSomething,19,w);
  fun XmIsMainWindow          (w:Widget):bool = xcall(XCALL_XmIsSomething,20,w);
  fun XmIsManager             (w:Widget):bool = xcall(XCALL_XmIsSomething,21,w);
  fun XmIsMenuShell           (w:Widget):bool = xcall(XCALL_XmIsSomething,22,w);
  fun XmIsMessageBox          (w:Widget):bool = xcall(XCALL_XmIsSomething,23,w);
  fun XmIsMotifWMRunning      (w:Widget):bool = xcall(XCALL_XmIsSomething,24,w);
  fun XmIsPanedWindow         (w:Widget):bool = xcall(XCALL_XmIsSomething,25,w);
  fun XmIsPrimitive           (w:Widget):bool = xcall(XCALL_XmIsSomething,26,w);
  fun XmIsPushButton          (w:Widget):bool = xcall(XCALL_XmIsSomething,27,w);
  fun XmIsPushButtonGadget    (w:Widget):bool = xcall(XCALL_XmIsSomething,28,w);
  fun XmIsRowColumn           (w:Widget):bool = xcall(XCALL_XmIsSomething,29,w);
  fun XmIsScale               (w:Widget):bool = xcall(XCALL_XmIsSomething,30,w);
(* unsupported by solaris version of toolkit ...
  fun XmIsScreenObject        (w:Widget):bool = xcall(XCALL_XmIsSomething,31,w);
... *)
  fun XmIsScrollBar           (w:Widget):bool = xcall(XCALL_XmIsSomething,32,w);
  fun XmIsScrolledWindow      (w:Widget):bool = xcall(XCALL_XmIsSomething,33,w);
  fun XmIsSelectionBox        (w:Widget):bool = xcall(XCALL_XmIsSomething,34,w);
  fun XmIsSeparator           (w:Widget):bool = xcall(XCALL_XmIsSomething,35,w);
  fun XmIsSeparatorGadget     (w:Widget):bool = xcall(XCALL_XmIsSomething,36,w);
  fun XmIsShellExt            (w:Widget):bool = xcall(XCALL_XmIsSomething,37,w);
  fun XmIsText                (w:Widget):bool = xcall(XCALL_XmIsSomething,38,w);
  fun XmIsTextField           (w:Widget):bool = xcall(XCALL_XmIsSomething,39,w);
  fun XmIsToggleButton        (w:Widget):bool = xcall(XCALL_XmIsSomething,40,w);
  fun XmIsToggleButtonGadget  (w:Widget):bool = xcall(XCALL_XmIsSomething,41,w);
  fun XmIsVendorShell         (w:Widget):bool = xcall(XCALL_XmIsSomething,42,w);
  fun XmIsVendorShellExt      (w:Widget):bool = xcall(XCALL_XmIsSomething,43,w);
(* Not supported in Motif 1.2
  fun XmIsWorldObject         (w:Widget):bool = xcall(XCALL_XmIsSomething,44,w);
*)


(******************************************************************************)
(*                                                                            *)
(*      Operations valid for specific Widget classes only                     *)
(*                                                                            *)
(******************************************************************************)  fun XmCascadeButtonHighlight (w:Widget) (b:bool):unit =
    xcall (XCALL_XmCascadeButtonHighlight,w,b);
  
  fun XmCommandError (w:Widget) (s:XmString):unit =
    xcall (XCALL_XmCommandError,w,s);

  fun XmCommandGetChild (w:Widget) (b:XmDefaultButtonType):Widget =
    xcall (XCALL_XmCommandGetChild,w,fromDefaultButtonType b);

  fun XmFileSelectionBoxGetChild (w:Widget) (b:XmDefaultButtonType):Widget =
    xcall (XCALL_XmFileSelectionBoxGetChild,w,fromDefaultButtonType b);
  
  fun XmFileSelectionDoSearch (w:Widget) (s:XmString):unit =
    xcall (XCALL_XmFileSelectionDoSearch,w,s);


  fun XmMainWindowSetAreas (a:Widget)
                           (b:Widget)
                           (c:Widget)
                           (d:Widget)
                           (e:Widget)
                           (f:Widget):unit =
    xcall (XCALL_XmMainWindowSetAreas,a,b,c,d,e,f);
  
  fun XmMainWindowSep1 (w:Widget):Widget = xcall (XCALL_XmMainWindowSepX,w,1);
  fun XmMainWindowSep2 (w:Widget):Widget = xcall (XCALL_XmMainWindowSepX,w,2);
  fun XmMainWindowSep3 (w:Widget):Widget = xcall (XCALL_XmMainWindowSepX,w,3);

  fun XmMessageBoxGetChild (w:Widget) (b:XmDefaultButtonType):Widget =
    xcall (XCALL_XmMessageBoxGetChild,w,fromDefaultButtonType b);
  
  fun XmOptionButtonGadget (w:Widget):Widget =
    xcall (XCALL_XmOptionButtonGadget,w);
    
  fun XmOptionLabelGadget  (w:Widget):Widget =
    xcall (XCALL_XmOptionLabelGadget,w);

  fun XmScrolledWindowSetAreas (a:Widget)
                               (b:Widget)
                               (c:Widget)
                               (d:Widget):unit =
    xcall (XCALL_XmScrolledWindowSetAreas,a,b,c,d);
  
  fun XmSelectionBoxGetChild (w:Widget) (b:XmDefaultButtonType):Widget =
    xcall (XCALL_XmSelectionBoxGetChild,w,fromDefaultButtonType b);
  
  fun XmSetMenuCursor (d:Display) (c:Cursor):unit =
    xcall (XCALL_XmSetMenuCursor,d,c);

  fun XmTrackingLocate (w:Widget) (c:Cursor) (b:bool):Widget =
    xcall (XCALL_XmTrackingLocate,w,c,b);
  
  fun XmUpdateDisplay (w:Widget):unit =
    xcall (XCALL_XmUpdateDisplay,w);
    
local
  type code = int;
  
  fun get (opn : code) : Widget -> '_a =
    fn (w:Widget) => xcall (opn,w);

  fun set (opn : code) : Widget -> '_a -> unit =
    fn (w:Widget) => fn(x:'_a) => xcall (opn,w,x);

  val getInt    : code -> Widget -> int = get;
  val getBool   : code -> Widget -> bool = get;
  val getString : code -> Widget -> string = get;

  val setInt    : code -> Widget -> int    -> unit = set;
  val setBool   : code -> Widget -> bool   -> unit = set;
  val setString : code -> Widget -> string -> unit = set;
in
(******************************************************************************)
(*                                                                            *)
(*      Operations on XmText Widgets                                          *)
(*                                                                            *)
(******************************************************************************)
 val XmTextGetAddMode =
    getBool XCALL_XmTextGetAddMode;

  val XmTextGetBaseline =
    getInt XCALL_XmTextGetBaseline;

  val XmTextGetCursorPosition =
    getInt XCALL_XmTextGetCursorPosition;

  val XmTextGetEditable =
    getBool XCALL_XmTextGetEditable;

  val XmTextGetInsertionPosition =
    getInt XCALL_XmTextGetInsertionPosition;

  val XmTextGetLastPosition =
    getInt XCALL_XmTextGetLastPosition;

  val XmTextGetMaxLength =
    getInt XCALL_XmTextGetMaxLength;

  val XmTextGetSelection =
    getInt XCALL_XmTextGetSelection;

  val XmTextGetString=
    getString XCALL_XmTextGetString;

  val XmTextGetTopCharacter =
    getInt XCALL_XmTextGetTopCharacter;

  fun XmTextInsert (w:Widget) (n:int) (s:string) : unit =
    xcall (XCALL_XmTextInsert,w,n,s);

  val XmTextPaste =
    getBool XCALL_XmTextPaste;

  val XmTextRemove =
    getBool XCALL_XmTextRemove;

  fun XmTextReplace (w:Widget) (from:int) (to:int) (s:string) : unit =
    xcall (XCALL_XmTextReplace,w,from,to,s);

  val XmTextScroll =
    setInt XCALL_XmTextScroll;

  val XmTextSetAddMode =
    setBool XCALL_XmTextSetAddMode;

  val XmTextSetCursorPosition =
    setInt XCALL_XmTextSetCursorPosition;

  val XmTextSetEditable =
    setBool XCALL_XmTextSetEditable;

  val XmTextSetInsertionPosition  =
    setInt XCALL_XmTextSetInsertionPosition;

  val XmTextSetMaxLength =
    setInt XCALL_XmTextSetMaxLength;

  val XmTextSetString =
    setString XCALL_XmTextSetString;

  val XmTextSetTopCharacter =
    setInt XCALL_XmTextSetTopCharacter;

  val XmTextShowPosition =
    setInt XCALL_XmTextShowPosition;

  fun XmTextXYToPos (w:Widget) (x:int) (y:int) : int =
    xcall (XCALL_XmTextXYToPos,w,x,y);
      
(******************************************************************************)
(*                                                                            *)
(*      Operations on XmTextField Widgets                                     *)
(*                                                                            *)
(******************************************************************************)
  val XmTextFieldGetAddMode =
    getBool XCALL_XmTextFieldGetAddMode;

  val XmTextFieldGetBaseline =
    getInt XCALL_XmTextFieldGetBaseline;

  val XmTextFieldGetCursorPosition =
    getInt XCALL_XmTextFieldGetCursorPosition;

  val XmTextFieldGetEditable =
    getBool XCALL_XmTextFieldGetEditable;

  val XmTextFieldGetInsertionPosition =
    getInt XCALL_XmTextFieldGetInsertionPosition;

  val XmTextFieldGetLastPosition =
    getInt XCALL_XmTextFieldGetLastPosition;

  val XmTextFieldGetMaxLength =
    getInt XCALL_XmTextFieldGetMaxLength;

  val XmTextFieldGetSelection =
    getInt XCALL_XmTextFieldGetSelection;

  val XmTextFieldGetString=
    getString XCALL_XmTextFieldGetString;

  fun XmTextFieldInsert (w:Widget) (n:int) (s:string) : unit =
    xcall (XCALL_XmTextFieldInsert,w,n,s);

  val XmTextFieldPaste =
    getBool XCALL_XmTextFieldPaste;

  val XmTextFieldRemove =
    getBool XCALL_XmTextFieldRemove;

  fun XmTextFieldReplace (w:Widget) (from:int) (to:int) (s:string) : unit =
    xcall (XCALL_XmTextFieldReplace,w,from,to,s);

  val XmTextFieldSetAddMode =
    setBool XCALL_XmTextFieldSetAddMode;

  val XmTextFieldSetCursorPosition =
    setInt XCALL_XmTextFieldSetCursorPosition;

  val XmTextFieldSetEditable =
    setBool XCALL_XmTextFieldSetEditable;

  val XmTextFieldSetInsertionPosition  =
    setInt XCALL_XmTextFieldSetInsertionPosition;

  val XmTextFieldSetMaxLength =
    setInt XCALL_XmTextFieldSetMaxLength;

  val XmTextFieldSetString =
    setString XCALL_XmTextFieldSetString;

  val XmTextFieldShowPosition =
    setInt XCALL_XmTextFieldShowPosition;

  fun XmTextFieldXYToPos (w:Widget) (x:int) (y:int) : int =
    xcall (XCALL_XmTextFieldXYToPos,w,x,y);
end; (* local *)
      
(******************************************************************************)
(*                                                                            *)
(*      Operations on XmList Widgets                                          *)
(*                                                                            *)
(******************************************************************************)

  fun XmListAddItem (w: Widget) (s:string) (pos:int) : unit =
    xcall (XCALL_XmListAddItem,w,s,pos);
   
  fun XmListAddItemUnselected (w: Widget) (s: string) (pos: int) : unit =
    xcall (XCALL_XmListAddItemUnselected,w,s,pos);
   
  fun XmListAddItems (w: Widget) (sl: string list) (pos: int) : unit =
    xcall (XCALL_XmListAddItems,w,sl,pos);
   
  fun XmListDeleteAllItems (w: Widget) : unit =
    xcall (XCALL_XmListDeleteAllItems,w);

  fun XmListDeleteItem (w: Widget) (s: string) : unit =
    xcall (XCALL_XmListDeleteItem,w,s);

  fun XmListDeleteItems (w: Widget) (sl: string list) : unit =
    xcall (XCALL_XmListDeleteItems,w,sl);

  fun XmListDeletePos (w: Widget) (pos: int) : unit =
    xcall (XCALL_XmListDeletePos,w,pos);

  fun XmListDeleteItemsPos (w: Widget) (num: int) (pos: int) : unit =
    xcall (XCALL_XmListDeleteItemsPos,w,num,pos);

  fun XmListDeselectAllItems (w: Widget) : unit =
    xcall (XCALL_XmListDeselectAllItems,w);

  fun XmListDeselectItem (w: Widget) (s: string) : unit =
    xcall (XCALL_XmListDeselectItem,w,s);

  fun XmListDeselectPos (w: Widget) (pos: int) : unit =
    xcall (XCALL_XmListDeselectPos,w,pos);

  fun XmListGetMatchPos (w: Widget) (s: string) : int list =
    xcall (XCALL_XmListGetMatchPos,w,s);

  fun XmListGetSelectedPos (w: Widget) : int list =
    xcall (XCALL_XmListGetSelectedPos,w);

  fun XmListItemExists (w: Widget) (s: string) : bool =
    xcall (XCALL_XmListItemExists,w,s);

  fun XmListItemPos (w: Widget) (s: string) : int =
    xcall (XCALL_XmListItemPos,w,s);

  fun XmListReplaceItems (w: Widget) (oldNewPairs : (string * string) list) : unit =
  let
    val oldStrings = map #1 oldNewPairs;
    val newStrings = map #2 oldNewPairs;
  in
    xcall (XCALL_XmListReplaceItems,w,oldStrings,newStrings)
  end;
    
  fun XmListReplaceItemsPos (w: Widget) (sl: string list) (pos: int) : unit =
    xcall (XCALL_XmListReplaceItemsPos,w,sl,pos);
  
  fun XmListSelectItem (w: Widget) (s: string) (notify: bool) : unit =
    xcall (XCALL_XmListSelectItem,w,s,notify);
  
  fun XmListSelectPos (w: Widget) (pos: int) (notify: bool) : unit =
    xcall (XCALL_XmListSelectPos,w,pos,notify);
  
  fun XmListSetAddMode (w: Widget) (on: bool) : unit =
    xcall (XCALL_XmListSetAddMode,w,on);
  
  fun XmListSetBottomItem (w: Widget) (s:string) : unit =
    xcall (XCALL_XmListSetBottomItem,w,s);
  
  fun XmListSetBottomPos (w: Widget) (pos:int) : unit =
    xcall (XCALL_XmListSetBottomPos,w,pos);
  
  fun XmListSetHorizPos (w: Widget) (pos:int) : unit =
    xcall (XCALL_XmListSetHorizPos,w,pos);

  fun XmListSetItem (w: Widget) (s:string) : unit =
    xcall (XCALL_XmListSetItem,w,s);
  
  fun XmListSetPos (w: Widget) (pos:int) : unit =
    xcall (XCALL_XmListSetPos,w,pos);
  
  fun XmMenuPosition (w: Widget) (ev: 'a XEvent): unit =
    case ev of
       ButtonPress { rootPointer=XPoint{x,y}, ...} =>
        xcall(XCALL_XmMenuPosition, w, x, y)
     | ButtonClick { rootPointer=XPoint{x,y}, ...} =>
        xcall(XCALL_XmMenuPosition, w, x, y)
     | _ => raise XWindows("Bad event type in XmMenuPosition")

end; (* local *)
end; (* struct *)
