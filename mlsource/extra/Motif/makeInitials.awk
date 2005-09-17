/NOT SUPPORTED/ { exit }

{ if ( NF >= 2 && $3 != "(callback)" )
  {
    type = "unknown" ;
    
    if ( $3 =="Atom" ) type = "XmRAtom" ;
    if ( $3 =="Cardinal" ) type = "XmRCardinal" ;
    if ( $3 =="Colormap" ) type = "XmRColormap" ;
    if ( $3 =="Cursor" ) type = "XmRCursor" ;
    if ( $3 =="Dimension" ) type = "XmRDimension" ;
    if ( $3 =="Drawable" ) type = "XmRPixmap" ;
    if ( $3 =="KeySym" ) type = "XmRKeySym" ;
    if ( $3 =="KeySym" && $4 == "list" ) type = "XmRKeySymTable" ;
    if ( $3 =="Pixel" ) type = "XmRPixel" ;
    if ( $3 =="Position" ) type = "XmRPosition" ;
    if ( $3 =="Visual" ) type = "XmRVisual" ;
    if ( $3 =="Widget" ) type = "XmRWidget" ;
    if ( $3 =="Widget" && $4 == "list" ) type = "XmRWidgetList" ;
    if ( $3 =="XFontStruct" && $4 == "list" ) type = "XmRFontList" ;
    if ( $3 =="XWMStateHint" ) type = "XmRInitialState" ;
    if ( $3 =="XtAccelerators" ) type = "XmRAcceleratorTable" ;
    if ( $3 =="XmAlignment" ) type = "XmRAlignment" ;
    if ( $3 =="XmArrowDirection" ) type = "XmRArrowDirection" ;
    if ( $3 =="XmAttachment" ) type = "XmRAttachment" ;
    if ( $3 =="XmButtonType" && $4 == "list" ) type = "XmRButtonTypeTable" ;
    if ( $3 =="XmCommandWindowLocation" ) type = "XmRCommandWindowLocation" ;
    if ( $3 =="XmDefaultButtonType" ) type = "XmRDefaultButtonType" ;
    if ( $3 =="XmDeleteResponse" ) type = "XmRDeleteResponse" ;
    if ( $3 =="XmDialogStyle" ) type = "XmRDialogStyle" ;
    if ( $3 =="XmDialogType" ) type = "XmRDialogType" ;
    if ( $3 =="XmEditMode" ) type = "XmREditMode" ;
    if ( $3 =="XmFileTypeMask" ) type = "XmRFileTypeMask" ;
    if ( $3 =="XmIndicatorType" ) type = "XmRIndicatorType" ;
    if ( $3 =="XmKeyboardFocusPolicy" ) type = "XmRKeyboardFocusPolicy" ;
    if ( $3 =="XmLabelType" ) type = "XmRLabelType" ;
    if ( $3 =="XmMultiClickType" ) type = "XmRMultiClick" ;
    if ( $3 =="XmNavigationType" ) type = "XmRNavigationType" ;
    if ( $3 =="XmOrientation" ) type = "XmROrientation" ;
    if ( $3 =="XmPacking" ) type = "XmRPacking" ;
    if ( $3 =="XmProcessingDirection" ) type = "XmRProcessingDirection" ;
    if ( $3 =="XmResizePolicy" ) type = "XmRResizePolicy" ;
    if ( $3 =="XmRowColumnType" ) type = "XmRRowColumnType" ;
    if ( $3 =="XmScrollBarDisplayPolicy" ) type = "XmRScrollBarDisplayPolicy" ;
    if ( $3 =="XmScrollBarPlacement" ) type = "XmRScrollBarPlacement" ;
    if ( $3 =="XmScrollingPolicy" ) type = "XmRScrollingPolicy" ;
    if ( $3 =="XmSelectionPolicy" ) type = "XmRSelectionPolicy" ;
    if ( $3 =="XmShadowType" ) type = "XmRShadowType" ;
    if ( $3 =="XmString" ) type = "XmRXmString" ;
    if ( $3 =="XmString" && $4 == "list" ) type = "XmRXmStringTable" ;
    if ( $3 =="XmStringDirection" ) type = "XmRStringDirection" ;
    if ( $3 =="XmTextPosition" ) type = "XmRInt" ;
    if ( $3 =="XmTextScanType" && $4 == "list" ) type = "XmRTextScanTypeTable" ;
    if ( $3 =="XmUnitType" ) type = "XmRUnitType" ;
    if ( $3 =="XmVisualPolicy" ) type = "XmRVisualPolicy" ;
    if ( $3 =="XtTranslations" ) type = "XmRTranslationTable" ;
    if ( $3 =="bool" ) type = "XmRBool" ;
    if ( $3 =="int" ) type = "XmRInt" ;
    if ( $3 =="short" ) type = "XmRShort" ;
    if ( $3 =="string" ) type = "XmRString" ;
    if ( $3 =="string" && $4 == "list" ) type = "XmRStringTable" ;
    
    printf ("val U = XtSetExceptionType %s %s ;\n",$1,type)
  }
}
