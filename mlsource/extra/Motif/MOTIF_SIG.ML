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

signature MOTIF_SIG =
sig
  
  include XM_TYPES
  
    val XtParseTranslationTable : string -> XtTranslations
    
    val XtOverrideTranslations : Widget -> XtTranslations -> unit
    val XtAugmentTranslations :  Widget -> XtTranslations -> unit
    
    val XtUninstallTranslations : Widget -> unit
    
    val XtTranslateTablePrint : XtTranslations -> unit
  
    val NoWidget :     Widget
    val WidgetID :     Widget -> int
    val WidgetExists : Widget -> bool
  
    val XtAppInitialise : string -> 
                         string -> 
                         string -> string list -> Arg list -> Widget
  
    val XtCreatePopupShell : string -> Widget -> Arg list -> Widget
  
    val XtRealizeWidget :   Widget -> unit
    val XtUnrealizeWidget : Widget -> unit
    val XtDestroyWidget :   Widget -> unit
    val XtManageChild :     Widget -> unit
    val XtUnmanageChild :   Widget -> unit
    
    val XtManageChildren :   Widget list -> unit
    val XtUnmanageChildren : Widget list -> unit
    
    val XtSetExceptionType : ('a -> Arg) -> ('a -> XmRType) -> unit
    
    val XtSetValue :  Widget -> Arg -> unit
    val XtSetValues : Widget -> Arg list -> unit
    
    val XtGetValue :  Widget -> string -> Arg
    val XtGetValues : Widget -> string list -> Arg list
    
    val XtGetSubresources : Widget -> string -> string -> (string * string * XmRType) list -> Arg list

    val XtParent :  Widget -> Widget
    val XtWindow :  Widget -> XWindows.Drawable
    val XtDisplay : Widget -> XWindows.Display
    val XtName :    Widget -> string
    
    val XtSetCallbacks : Widget ->
                        (XtCallback * (Widget * 'a XWindows.XEvent * '_b -> '_b)) list ->
                        '_b ->
                        int -> 'a -> unit
    
    val XtSetSensitive : Widget -> bool -> unit
    
    val XtIsSensitive : Widget -> bool
  
    (* added 6/12/94 SPF *)
    val XtPopup       : Widget -> XtGrabKind -> unit;
    val XtPopdown     : Widget -> unit;
    val XtMapWidget   : Widget -> unit;
    val XtUnmapWidget : Widget -> unit;
    
    (* added 19/1/95 SPF *)
    val XtIsManaged  : Widget -> bool
    val XtIsRealized : Widget -> bool

    (* Added by DCJM. 23/3/01. *)
    val XtGetApplicationResources: Widget -> (string * string * XmRType) list -> Arg list ;
    val XtAddEventHandler:
       Widget -> XWindows.EventMask list -> bool -> (Widget * 'a XWindows.XEvent -> unit) -> unit

    val XmCreateArrowButton :         Widget -> string -> Arg list -> Widget
    val XmCreateArrowButtonGadget :   Widget -> string -> Arg list -> Widget
    val XmCreateBulletinBoard :       Widget -> string -> Arg list -> Widget
    val XmCreateBulletinBoardDialog : Widget -> string -> Arg list -> Widget
    val XmCreateCascadeButton :       Widget -> string -> Arg list -> Widget
    val XmCreateCascadeButtonGadget : Widget -> string -> Arg list -> Widget
    val XmCreateCommand :             Widget -> string -> Arg list -> Widget
    val XmCreateDialogShell :         Widget -> string -> Arg list -> Widget
    val XmCreateDrawingArea :         Widget -> string -> Arg list -> Widget
    val XmCreateDrawnButton :         Widget -> string -> Arg list -> Widget
    val XmCreateErrorDialog :         Widget -> string -> Arg list -> Widget
    val XmCreateFileSelectionBox :    Widget -> string -> Arg list -> Widget
    val XmCreateFileSelectionDialog : Widget -> string -> Arg list -> Widget
    val XmCreateForm :                Widget -> string -> Arg list -> Widget
    val XmCreateFormDialog :          Widget -> string -> Arg list -> Widget
    val XmCreateFrame :               Widget -> string -> Arg list -> Widget
    val XmCreateInformationDialog :   Widget -> string -> Arg list -> Widget
    val XmCreateLabel :               Widget -> string -> Arg list -> Widget
    val XmCreateLabelGadget :         Widget -> string -> Arg list -> Widget
    val XmCreateList :                Widget -> string -> Arg list -> Widget
    val XmCreateMainWindow :          Widget -> string -> Arg list -> Widget
    val XmCreateMenuBar :             Widget -> string -> Arg list -> Widget
    val XmCreateMenuShell :           Widget -> string -> Arg list -> Widget
    val XmCreateMessageBox :          Widget -> string -> Arg list -> Widget
    val XmCreateMessageDialog :       Widget -> string -> Arg list -> Widget
    val XmCreateOptionMenu :          Widget -> string -> Arg list -> Widget
    val XmCreatePanedWindow :         Widget -> string -> Arg list -> Widget
    val XmCreatePopupMenu :           Widget -> string -> Arg list -> Widget
    val XmCreatePromptDialog :        Widget -> string -> Arg list -> Widget
    val XmCreatePulldownMenu :        Widget -> string -> Arg list -> Widget
    val XmCreatePushButton :          Widget -> string -> Arg list -> Widget
    val XmCreatePushButtonGadget :    Widget -> string -> Arg list -> Widget
    val XmCreateQuestionDialog :      Widget -> string -> Arg list -> Widget
    val XmCreateRadioBox :            Widget -> string -> Arg list -> Widget
    val XmCreateRowColumn :           Widget -> string -> Arg list -> Widget
    val XmCreateScale :               Widget -> string -> Arg list -> Widget
    val XmCreateScrollBar :           Widget -> string -> Arg list -> Widget
    val XmCreateScrolledList :        Widget -> string -> Arg list -> Widget
    val XmCreateScrolledText :        Widget -> string -> Arg list -> Widget
    val XmCreateScrolledWindow :      Widget -> string -> Arg list -> Widget
    val XmCreateSelectionBox :        Widget -> string -> Arg list -> Widget
    val XmCreateSelectionDialog :     Widget -> string -> Arg list -> Widget
    val XmCreateSeparator :           Widget -> string -> Arg list -> Widget
    val XmCreateSeparatorGadget :     Widget -> string -> Arg list -> Widget
    val XmCreateSimpleCheckBox :      Widget -> string -> Arg list -> Widget
    val XmCreateSimpleMenuBar :       Widget -> string -> Arg list -> Widget
    val XmCreateSimpleOptionMenu :    Widget -> string -> Arg list -> Widget
    val XmCreateSimplePopupMenu :     Widget -> string -> Arg list -> Widget
    val XmCreateSimplePulldownMenu :  Widget -> string -> Arg list -> Widget
    val XmCreateSimpleRadioBox :      Widget -> string -> Arg list -> Widget
    val XmCreateText :                Widget -> string -> Arg list -> Widget
    val XmCreateTextField :           Widget -> string -> Arg list -> Widget
    val XmCreateToggleButton :        Widget -> string -> Arg list -> Widget
    val XmCreateToggleButtonGadget :  Widget -> string -> Arg list -> Widget
    val XmCreateWarningDialog :       Widget -> string -> Arg list -> Widget
    val XmCreateWorkArea :            Widget -> string -> Arg list -> Widget
    val XmCreateWorkingDialog :       Widget -> string -> Arg list -> Widget

    val XmCascadeButtonHighlight : Widget -> bool -> unit
    
    val XmCommandError : Widget -> XmString -> unit
    
    val XmCommandGetChild : Widget -> XmDefaultButtonType -> Widget

    val XmFileSelectionBoxGetChild : Widget -> XmDefaultButtonType -> Widget
    
    val XmFileSelectionDoSearch : Widget -> XmString -> unit

    val XmIsArrowButton :         Widget -> bool
    val XmIsArrowButtonGadget :   Widget -> bool
    val XmIsBulletinBoard :       Widget -> bool
    val XmIsCascadeButton :       Widget -> bool
    val XmIsCascadeButtonGadget : Widget -> bool
    val XmIsCommand :             Widget -> bool
    val XmIsDesktopObject :       Widget -> bool
    val XmIsDialogShell :         Widget -> bool
(* unsupported by solaris version of toolkit ...
    val XmIsDisplayObject :       Widget -> bool
... *)
    val XmIsDrawingArea :         Widget -> bool
    val XmIsDrawnButton :         Widget -> bool
    val XmIsExtObject :           Widget -> bool
    val XmIsFileSelectionBox :    Widget -> bool
    val XmIsForm :                Widget -> bool
    val XmIsFrame :               Widget -> bool
    val XmIsGadget :              Widget -> bool
    val XmIsLabel :               Widget -> bool
    val XmIsLabelGadget :         Widget -> bool
    val XmIsList :                Widget -> bool
    val XmIsMainWindow :          Widget -> bool
    val XmIsManager :             Widget -> bool
    val XmIsMenuShell :           Widget -> bool
    val XmIsMessageBox :          Widget -> bool
    val XmIsMotifWMRunning :      Widget -> bool
    val XmIsPanedWindow :         Widget -> bool
    val XmIsPrimitive :           Widget -> bool
    val XmIsPushButton :          Widget -> bool
    val XmIsPushButtonGadget :    Widget -> bool
    val XmIsRowColumn :           Widget -> bool
    val XmIsScale :               Widget -> bool
(* unsupported by solaris version of toolkit ...
    val XmIsScreenObject :        Widget -> bool
... *)
    val XmIsScrollBar :           Widget -> bool
    val XmIsScrolledWindow :      Widget -> bool
    val XmIsSelectionBox :        Widget -> bool
    val XmIsSeparator :           Widget -> bool
    val XmIsSeparatorGadget :     Widget -> bool
    val XmIsShellExt :            Widget -> bool
    val XmIsText :                Widget -> bool
    val XmIsTextField :           Widget -> bool
    val XmIsToggleButton :        Widget -> bool
    val XmIsToggleButtonGadget :  Widget -> bool
    val XmIsVendorShell :         Widget -> bool
    val XmIsVendorShellExt :      Widget -> bool
(* unsupported in Motif 1.2
    val XmIsWorldObject :         Widget -> bool
*)

    val XmMainWindowSetAreas : Widget -> Widget -> Widget -> Widget -> Widget -> Widget -> unit
    
    val XmMainWindowSep1 : Widget -> Widget
    val XmMainWindowSep2 : Widget -> Widget
    val XmMainWindowSep3 : Widget -> Widget
  
    val XmMessageBoxGetChild : Widget -> XmDefaultButtonType -> Widget
  
    val XmOptionButtonGadget : Widget -> Widget
    val XmOptionLabelGadget :  Widget -> Widget
  
    val XmScrolledWindowSetAreas : Widget -> Widget -> Widget -> Widget -> unit
  
    val XmSelectionBoxGetChild : Widget -> XmDefaultButtonType -> Widget
    
    val XmSetMenuCursor : XWindows.Display -> XWindows.Cursor -> unit
  
    val XmTrackingLocate : Widget -> XWindows.Cursor -> bool -> Widget
    
    val XmUpdateDisplay : Widget -> unit

(******************************************************************************)
(*                                                                            *)
(*      Operations on XmText Widgets                                          *)
(*                                                                            *)
(******************************************************************************)

    val XmTextGetAddMode : Widget -> bool
    val XmTextGetBaseline : Widget -> int
    val XmTextGetCursorPosition : Widget -> int
    val XmTextGetEditable : Widget -> bool
    val XmTextGetInsertionPosition : Widget -> int
    val XmTextGetLastPosition : Widget -> int
    val XmTextGetMaxLength : Widget -> int
    val XmTextGetSelection : Widget -> int
    val XmTextGetString : Widget -> string
    val XmTextGetTopCharacter : Widget -> int
    val XmTextInsert : Widget -> int -> string -> unit
    val XmTextPaste : Widget -> bool
    val XmTextRemove : Widget -> bool
    val XmTextReplace : Widget -> int -> int -> string -> unit
    val XmTextScroll : Widget -> int -> unit
    val XmTextSetAddMode : Widget -> bool -> unit
    val XmTextSetCursorPosition : Widget -> int -> unit
    val XmTextSetEditable : Widget -> bool -> unit
    val XmTextSetInsertionPosition : Widget -> int -> unit
    val XmTextSetMaxLength : Widget -> int -> unit
    val XmTextSetString : Widget -> string -> unit
    val XmTextSetTopCharacter : Widget -> int -> unit
    val XmTextShowPosition : Widget -> int -> unit
    val XmTextXYToPos : Widget -> int -> int -> int
      
(******************************************************************************)
(*                                                                            *)
(*      Operations on XmTextField Widgets                                     *)
(*                                                                            *)
(******************************************************************************)

    val XmTextFieldGetAddMode : Widget -> bool
    val XmTextFieldGetBaseline : Widget -> int
    val XmTextFieldGetCursorPosition : Widget -> int
    val XmTextFieldGetEditable : Widget -> bool
    val XmTextFieldGetInsertionPosition : Widget -> int
    val XmTextFieldGetLastPosition : Widget -> int
    val XmTextFieldGetMaxLength : Widget -> int
    val XmTextFieldGetSelection : Widget -> int
    val XmTextFieldGetString : Widget -> string
    val XmTextFieldInsert : Widget -> int -> string -> unit
    val XmTextFieldPaste : Widget -> bool
    val XmTextFieldRemove : Widget -> bool
    val XmTextFieldReplace : Widget -> int -> int -> string -> unit
    val XmTextFieldSetAddMode : Widget -> bool -> unit
    val XmTextFieldSetCursorPosition : Widget -> int -> unit
    val XmTextFieldSetEditable : Widget -> bool -> unit
    val XmTextFieldSetInsertionPosition : Widget -> int -> unit
    val XmTextFieldSetMaxLength : Widget -> int -> unit
    val XmTextFieldSetString : Widget -> string -> unit
    val XmTextFieldShowPosition : Widget -> int -> unit
    val XmTextFieldXYToPos : Widget -> int -> int -> int


(******************************************************************************)
(*                                                                            *)
(*      Operations on XmList Widgets                                          *)
(*                                                                            *)
(******************************************************************************)

(* 
   This widget maintains an ordered list of strings. The user can select
   or deselect strings in the list. The programmer can refer to the
   strings either by name (passing in the string itself) or by position
   (passing in an integer; the first string in the list has position 1).
*)


(************************* Adding Items to List *******************************)

  (* Add one item (the string) at the specified postion (the int). *)
  (* If the string is in the Widget's current selected items list,
     it will be pre-selected. *)
  val XmListAddItem            : Widget -> string -> int -> unit;
  
  (* As XmListAddItems, except that the new item is not pre-selected. *)
  val XmListAddItemUnselected  : Widget -> string -> int -> unit;
  
  (* Add a number of items, starting at the specifed position. *)
  val XmListAddItems           : Widget -> string list -> int -> unit;
  
(************************* Deleting Items from List ***************************)

  (* Delete all items in the list *)
  val XmListDeleteAllItems     : Widget -> unit;
  
  (* Delete the item (specified as a string) from the list *)
  val XmListDeleteItem         : Widget -> string -> unit;
  
  (* Delete several item (specified as strings) from the list *)
  val XmListDeleteItems        :  Widget -> string list -> unit;
  
  (* Delete the item (specified by its position) from the widget *)
  val XmListDeletePos:       Widget -> int -> unit;
  
  (* Delete several items (specified by their positions) from the widget *)
  val XmListDeleteItemsPos:  Widget -> int -> int -> unit;
  
(************************* Deselecting Items **********************************)
  (* Deselect all items in the list *)
  val XmListDeselectAllItems:  Widget -> unit;

  (* Deselect the item (specified as a string). *)
  val XmListDeselectItem:      Widget -> string -> unit;

  (* Deselect the item (specified by its position). *)
  val XmListDeselectPos:       Widget -> int -> unit;

(************************* Query Functions ************************************)

  (* Return the list of positions at which the string appears in the list *)
  val XmListGetMatchPos:    Widget -> string -> int list 
  
  (* Return the list of positions of the currently selected items *)
  val XmListGetSelectedPos: Widget -> int list
  
  (* Does the item occur in the list? *)
  val XmListItemExists:     Widget -> string -> bool;
  
  (* What is the first occurrence of the item in the list? (0 if absent). *)
  val XmListItemPos:        Widget -> string -> int; 

(************************* Replacing Items in the List ************************)

  (* For each (old, new) pair, replace each occurrence of "old" with "new". *)
  val XmListReplaceItems:              Widget -> (string * string) list -> unit;
  
  (* Replace items in the list, starting at the specified position *)
  val XmListReplaceItemsPos:           Widget -> string list -> int -> unit;
  
(************************* Selecting Items in the List ************************)

  (* Highlight the specified item and add it to the list of selected items.
     If the bool is true, also invoke the selection callback. *)
  val XmListSelectItem: Widget -> string -> bool -> unit;
  val XmListSelectPos:  Widget -> int    -> bool -> unit;

(************************* Set Add Mode ***************************************)
  (* Activate (bool is "true") or deactivate (bool is "false") Add Mode. *)
  val XmListSetAddMode:    Widget -> bool -> unit;

(************************* Set Appearance *************************************)

  (* Make an existing item the last visible item in the list. *)
  val XmListSetBottomItem: Widget -> string -> unit; 
  val XmListSetBottomPos:  Widget -> int    -> unit;
  
  (* Moves the horizontal scroolbar to the speciied position *)
  val XmListSetHorizPos:   Widget -> int -> unit;
  
  (* Make an existing item the first visible item in the list. *)
  val XmListSetItem:       Widget -> string -> unit; 
  val XmListSetPos:        Widget -> int -> unit;
  
(************************* Miscellaneous *************************************)

(* Not supported by the SUNOS version of the tool-kit ...
  val XmListAddItemsUnselected : Widget -> string list -> int -> unit;
  val XmListDeletePositions: Widget -> int list -> unit;
  val XmListGetKbdItemPos:  Widget -> int
  val XmListPosSelected:    Widget -> int -> bool;
  val XmListReplaceItemsPosUnselected: Widget -> string list -> int -> unit;
  val XmListReplaceItemsPositions:     Widget -> (int * string) list -> unit;
  val XmListSetKbdItemPos:      Widget -> int -> bool; 
  val XmListUpdateSelectedList: Widget -> unit; 
  val XmListYToPos:             Widget -> int -> int 
  val XmListPosToBounds:        Widget -> ??? 
... *)

    (* Added by DCJM. 23/3/01. *)
  val XmMenuPosition: Widget -> 'a XWindows.XEvent -> unit

 (* local *)

end (* signature *)
