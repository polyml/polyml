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

signature MOTIF_TYPES =
sig
  structure XWINDOWS:
  sig
    type Colormap;
    type Cursor;
    type Display;
    type Drawable;
    type Visual;
    type XFontStruct;
    type XWMStateHint;
    
    type 'a XEvent;
  end;
    
    (* should we have a separate Xt level? Probaly. SPF 6/12/94 *)
    type Widget;
    type XtAccelerators;
    type XtTranslations;
    
    (* Added 6/12/94 SPF *)
    datatype XtGrabKind =
      XtGrabNone
    | XtGrabNonexclusive
    | XtGrabExclusive;
  
    datatype XmAlignment =
       XmALIGNMENT_BEGINNING
     | XmALIGNMENT_CENTER
     | XmALIGNMENT_END;
    
    datatype XmArrowDirection =
      XmARROW_UP
    | XmARROW_DOWN
    | XmARROW_LEFT
    | XmARROW_RIGHT;

    datatype XmAttachment = 
      XmATTACH_NONE
    | XmATTACH_FORM
    | XmATTACH_OPPOSITE_FORM
    | XmATTACH_WIDGET
    | XmATTACH_OPPOSITE_WIDGET
    | XmATTACH_POSITION
    | XmATTACH_SELF;
  
    datatype XmButtonType = 
      XmPUSHBUTTON
    | XmCHECKBUTTON
    | XmTOGGLEBUTTON
    | XmRADIOBUTTON
    | XmCASCADEBUTTON
    | XmSEPARATOR
    | XmDOUBLE_SEPARATOR
    | XmTITLE;

    datatype XmCommandWindowLocation = 
      XmCOMMAND_ABOVE_WORKSPACE
    | XmCOMMAND_BELOW_WORKSPACE;

    datatype XmDefaultButtonType = 
      XmDIALOG_NONE
    | XmDIALOG_APPLY_BUTTON
    | XmDIALOG_CANCEL_BUTTON
    | XmDIALOG_DEFAULT_BUTTON
    | XmDIALOG_OK_BUTTON
    | XmDIALOG_FILTER_LABEL
    | XmDIALOG_FILTER_TEXT
    | XmDIALOG_HELP_BUTTON
    | XmDIALOG_LIST
    | XmDIALOG_LIST_LABEL
    | XmDIALOG_MESSAGE_LABEL
    | XmDIALOG_SELECTION_LABEL
    | XmDIALOG_SYMBOL_LABEL
    | XmDIALOG_TEXT
    | XmDIALOG_SEPARATOR
    | XmDIALOG_DIR_LIST
    | XmDIALOG_DIR_LIST_LABEL
 (* | XmDIALOG_COMMAND_TEXT    = XmDIALOG_TEXT
    | XmDIALOG_VALUE_TEXT      = XmDIALOG_TEXT
    | XmDIALOG_PROMPT_LABEL    = XmDIALOG_SELECTION_LABEL
    | XmDIALOG_HISTORY_LIST    = XmDIALOG_LIST
    | XmDIALOG_FILE_LIST       = XmDIALOG_LIST
    | XmDIALOG_FILE_LIST_LABEL = XmDIALOG_LIST_LABEL *);

    val XmDIALOG_COMMAND_TEXT:    XmDefaultButtonType;
    val XmDIALOG_VALUE_TEXT:      XmDefaultButtonType;
    val XmDIALOG_PROMPT_LABEL:    XmDefaultButtonType;
    val XmDIALOG_HISTORY_LIST:    XmDefaultButtonType;
    val XmDIALOG_FILE_LIST:       XmDefaultButtonType;
    val XmDIALOG_FILE_LIST_LABEL: XmDefaultButtonType;
    
    datatype XmDeleteResponse =
      XmDESTROY
    | XmUNMAP
    | XmDO_NOTHING;

    datatype XmDialogStyle = 
      XmDIALOG_MODELESS
    | XmDIALOG_PRIMARY_APPLICATION_MODAL
    | XmDIALOG_FULL_APPLICATION_MODAL
    | XmDIALOG_SYSTEM_MODAL
 (* | XmDIALOG_APPLICATION_MODAL *);

    val XmDIALOG_APPLICATION_MODAL: XmDialogStyle;
    
    datatype XmDialogType =
      XmDIALOG_ERROR
    | XmDIALOG_INFORMATION
    | XmDIALOG_MESSAGE
    | XmDIALOG_QUESTION
    | XmDIALOG_WARNING
    | XmDIALOG_WORKING
    | XmDIALOG_WORK_AREA
    | XmDIALOG_PROMPT
    | XmDIALOG_SELECTION
    | XmDIALOG_COMMAND
    | XmDIALOG_FILE_SELECTION;
    
    datatype XmEditMode = 
      XmMULTI_LINE_EDIT
    | XmSINGLE_LINE_EDIT;
    
    datatype XmFileTypeMask = 
      XmFILE_REGULAR
    | XmFILE_DIRECTORY
    | XmFILE_ANY_TYPE;

    datatype XmIndicatorType = 
      XmN_OF_MANY
    | XmONE_OF_MANY;

    datatype XmKeyboardFocusPolicy = 
      XmEXPLICIT
    | XmPOINTER;

    datatype XmLabelType = 
      XmPIXMAP
    | XmSTRING;
    
    datatype XmMultiClickType = 
      XmMULTICLICK_DISCARD
    | XmMULTICLICK_KEEP;

    datatype XmNavigationType = 
      XmNONE
    | XmTAB_GROUP
    | XmSTICKY_TAB_GROUP
    | XmEXCLUSIVE_TAB_GROUP;

    datatype XmOrientation = 
      XmVERTICAL
    | XmHORIZONTAL;
    
    datatype XmPacking = 
      XmPACK_TIGHT
    | XmPACK_COLUMN
    | XmPACK_NONE;
    
    datatype XmProcessingDirection =
      XmMAX_ON_TOP
    | XmMAX_ON_BOTTOM
    | XmMAX_ON_LEFT
    | XmMAX_ON_RIGHT;

    datatype XmResizePolicy =
      XmRESIZE_NONE
    | XmRESIZE_ANY
    | XmRESIZE_GROW;

    datatype XmRowColumnType = 
      XmWORK_AREA
    | XmMENU_BAR
    | XmMENU_PULLDOWN
    | XmMENU_POPUP
    | XmMENU_OPTION;

    datatype XmScrollBarDisplayPolicy =
      XmSTATIC
    | XmAS_NEEDED;

    datatype XmScrollBarPlacement =
      XmBOTTOM_RIGHT
    | XmTOP_RIGHT
    | XmBOTTOM_LEFT
    | XmTOP_LEFT;

    datatype XmScrollingPolicy =
      XmAUTOMATIC
    | XmAPPLICATION_DEFINED;
    
    datatype XmSelectionPolicy =
      XmSINGLE_SELECT
    | XmMULTIPLE_SELECT
    | XmEXTENDED_SELECT
    | XmBROWSE_SELECT;
    
    datatype XmShadowType =
      XmNO_LINE
    | XmSINGLE_LINE
    | XmDOUBLE_LINE
    | XmSINGLE_DASHED_LINE
    | XmDOUBLE_DASHED_LINE
    | XmSHADOW_ETCHED_IN
    | XmSHADOW_ETCHED_OUT
    | XmSHADOW_IN
    | XmSHADOW_OUT;

    datatype XmStringDirection = 
      XmSTRING_DIRECTION_L_TO_R
    | XmSTRING_DIRECTION_R_TO_L
    | XmSTRING_DIRECTION_DEFAULT;

    datatype XmTextScanType = 
      XmSELECT_POSITION
    | XmSELECT_WHITESPACE
    | XmSELECT_WORD
    | XmSELECT_LINE
    | XmSELECT_ALL
    | XmSELECT_PARAGRAPH;

    datatype XmUnitType = 
      XmPIXELS
    | Xm100TH_MILLIMETERS
    | Xm1000TH_INCHES
    | Xm100TH_POINTS
    | Xm100TH_FONT_UNITS;

    datatype XmVisualPolicy =
      XmVARIABLE
    | XmCONSTANT
    | XmRESIZE_IF_POSSIBLE;
    
    type XmString = string;

    type Arg = exn;
    
    datatype XmRType = 
      XmRAcceleratorTable       of XtAccelerators
    | XmRAlignment              of XmAlignment
    | XmRArrowDirection         of XmArrowDirection
    | XmRAtom                   of int
    | XmRAttachment             of XmAttachment
    | XmRBool                   of bool
    | XmRButtonTypeTable        of XmButtonType list
    | XmRCardinal               of int
    | XmRColormap               of XWINDOWS.Colormap
    | XmRCommandWindowLocation  of XmCommandWindowLocation
    | XmRCursor                 of XWINDOWS.Cursor
    | XmRDefaultButtonType      of XmDefaultButtonType
    | XmRDeleteResponse         of XmDeleteResponse
    | XmRDialogStyle            of XmDialogStyle
    | XmRDialogType             of XmDialogType
    | XmRDimension              of int
    | XmREditMode               of XmEditMode
    | XmRFileTypeMask           of XmFileTypeMask
    | XmRFontList               of (XWINDOWS.XFontStruct * string) list
    | XmRIndicatorType          of XmIndicatorType
    | XmRInitialState           of XWINDOWS.XWMStateHint
    | XmRInt                    of int
    | XmRKeySym                 of int
    | XmRKeySymTable            of int list
    | XmRKeyboardFocusPolicy    of XmKeyboardFocusPolicy
    | XmRLabelType              of XmLabelType
    | XmRMultiClick             of XmMultiClickType
    | XmRNavigationType         of XmNavigationType
    | XmROrientation            of XmOrientation
    | XmRPacking                of XmPacking
    | XmRProcessingDirection    of XmProcessingDirection
    | XmRResizePolicy           of XmResizePolicy
    | XmRRowColumnType          of XmRowColumnType
    | XmRPixel                  of int
    | XmRPixmap                 of XWINDOWS.Drawable
    | XmRPosition               of int
    | XmRScrollBarDisplayPolicy of XmScrollBarDisplayPolicy
    | XmRScrollBarPlacement     of XmScrollBarPlacement
    | XmRScrollingPolicy        of XmScrollingPolicy
    | XmRSelectionPolicy        of XmSelectionPolicy
    | XmRShadowType             of XmShadowType
    | XmRShort                  of int
    | XmRStringDirection        of XmStringDirection
    | XmRString                 of string
    | XmRStringTable            of string list
    | XmRTextScanTypeTable      of XmTextScanType list
    | XmRTranslationTable       of XtTranslations
    | XmRUnitType               of XmUnitType
    | XmRVisual                 of XWINDOWS.Visual
    | XmRVisualPolicy           of XmVisualPolicy
    | XmRWidget                 of Widget
    | XmRWidgetList             of Widget list
    | XmRXmString               of XmString
    | XmRXmStringTable          of XmString list;
    
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
    | XmNmessageCallback   (* this callback gets Message XEvents only *)
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
end; (* signature *)
