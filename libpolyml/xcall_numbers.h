/*
    Title:      X-Windows/Motif calls.

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

#define XCALL_Not                               10
#define XCALL_And                               11
#define XCALL_Or                                12
#define XCALL_Xor                               13
#define XCALL_DownShift                         14
#define XCALL_UpShift                           15
  
#define XCALL_NoDrawable                        20
#define XCALL_NoCursor                          21
#define XCALL_NoFont                            22
#define XCALL_NoColormap                        23
#define XCALL_NoVisual                          24

#define XCALL_GetTimeOfDay                      30

/* Colorcells 100 */
#define XCALL_XAllocColor                       103
#define XCALL_XAllocColorCells                  104
#define XCALL_XAllocColorPlanes                 105
#define XCALL_XAllocNamedColor                  106
#define XCALL_XFreeColors                       107
#define XCALL_XLookupColor                      108
#define XCALL_XParseColor                       109
#define XCALL_XQueryColor                       110
#define XCALL_XQueryColors                      111
#define XCALL_XStoreColor                       112
#define XCALL_XStoreColors                      113
#define XCALL_XStoreNamedColor                  114
#define XCALL_BlackPixel                        115
#define XCALL_WhitePixel                        116

/* Colormaps 150 */
#define XCALL_XCopyColormapAndFree              150
#define XCALL_XCreateColormap                   151
#define XCALL_XInstallColormap                  154
#define XCALL_XListInstalledColormaps           155
#define XCALL_XUninstallColormap                158
#define XCALL_DefaultColormap                   159
#define XCALL_DefaultVisual                     160
#define XCALL_DisplayCells                      161
#define XCALL_VisualClass                       162
#define XCALL_VisualRedMask                     163
#define XCALL_VisualGreenMask                   164
#define XCALL_VisualBlueMask                    165
  
/* Cursors 200 */
#define XCALL_XCreateFontCursor                 200
#define XCALL_XCreateGlyphCursor                201
#define XCALL_XCreatePixmapCursor               202
#define XCALL_XDefineCursor                     203
#define XCALL_XQueryBestCursor                  205
#define XCALL_XRecolorCursor                    206
#define XCALL_XUndefineCursor                   207

#define XCALL_XOpenDisplay                      222  
  
/* Display Specifications 250 */
#define XCALL_CellsOfScreen                     250
#define XCALL_DefaultDepth                      251
#define XCALL_DisplayHeight                     252
#define XCALL_DisplayHeightMM                   253
#define XCALL_DisplayPlanes                     254
#define XCALL_DisplayString                     255
#define XCALL_DisplayWidth                      256
#define XCALL_DisplayWidthMM                    257
#define XCALL_DoesBackingStore                  258
#define XCALL_DoesSaveUnders                    259
#define XCALL_EventMaskOfScreen                 260
#define XCALL_MaxCmapsOfScreen                  261
#define XCALL_MinCmapsOfScreen                  262
#define XCALL_ProtocolRevision                  263
#define XCALL_ProtocolVersion                   264
#define XCALL_ServerVendor                      265
#define XCALL_VendorRelease                     266

/* Drawing Primitives 300 */
#define XCALL_XClearArea                        300
#define XCALL_XClearWindow                      301
#define XCALL_XCopyArea                         302
#define XCALL_XCopyPlane                        303
#define XCALL_XDrawArc                          304
#define XCALL_XDrawArcs                         305
#define XCALL_XDrawImageString                  306
#define XCALL_XDrawImageString16                307
#define XCALL_XDrawLine                         308
#define XCALL_XDrawLines                        309
#define XCALL_XDrawPoint                        310
#define XCALL_XDrawPoints                       311
#define XCALL_XDrawRectangle                    312
#define XCALL_XDrawRectangles                   313
#define XCALL_XDrawSegments                     314
#define XCALL_XDrawString                       315
#define XCALL_XDrawString16                     316
#define XCALL_XDrawText                         317
#define XCALL_XDrawText16                       318
#define XCALL_XFillArc                          319
#define XCALL_XFillArcs                         320
#define XCALL_XFillPolygon                      321
#define XCALL_XFillRectangle                    322
#define XCALL_XFillRectangles                   323

/* Events 350 */
#define XCALL_XSelectInput                      350
#define XCALL_XSynchronize                      351

#define XCALL_GetState                          361
#define XCALL_SetState                          362 
#define XCALL_NextEvent                         365
#define XCALL_InsertTimeout                     366  

    
#define XCALL_XSetInputFocus                    370
#define XCALL_XGetInputFocus                    371
#define XCALL_XSetSelectionOwner                372
#define XCALL_XGetSelectionOwner                373
#define XCALL_XConvertSelection                 374
#define XCALL_XSendSelectionNotify              375
#define XCALL_XDeleteProperty                   376
#define XCALL_XInternAtom                       377
#define XCALL_XGetAtomName                      378
  
/* Fonts 400 */
#define XCALL_XGetFontPath                      401  
#define XCALL_XListFonts                        402
#define XCALL_XListFontsWithInfo                403
#define XCALL_XLoadFont                         404
#define XCALL_XLoadQueryFont                    405
#define XCALL_XQueryFont                        406
#define XCALL_XSetFontPath                      407

/* Grabbing 450 */
  
/* Graphics Context 500 */
#define XCALL_DefaultGC                         500
#define XCALL_UpdateGC                          501
#define XCALL_XCreateGC                         503

#define XCALL_XSetClipRectangles                509  
#define XCALL_XSetDashes                        510
  
/* Images 550 */
#define XCALL_XAddPixel                         550
#define XCALL_XGetImage                         552
#define XCALL_XGetPixel                         553
#define XCALL_XGetSubImage                      554
#define XCALL_XPutImage                         555
#define XCALL_XPutPixel                         556
#define XCALL_XSubImage                         557
#define XCALL_BitmapBitOrder                    558
#define XCALL_BitmapPad                         559
#define XCALL_BitmapUnit                        560
#define XCALL_ByteOrder                         561

/* Keyboard 600 */
#define XCALL_XLookupString                     611
#define XCALL_XQueryKeymap                      612
#define XCALL_IsCursorKey                       618
#define XCALL_IsFunctionKey                     619
#define XCALL_IsKeypadKey                       620
#define XCALL_IsMiscFunctionKey                 621
#define XCALL_IsModifierKey                     622
#define XCALL_IsPFKey                           623
  
/* Output Buffer 650 */
#define XCALL_XFlush                            650  
#define XCALL_XSync                             651

/* Pointers 700 */
#define XCALL_XQueryPointer                     703

/* Regions 750 */
  
/* Save Set 800 */
  
/* Screen Saver 850 */
#define XCALL_XActivateScreenSaver              850
#define XCALL_XForceScreenSaver                 851
#define XCALL_XGetScreenSaver                   852
#define XCALL_XResetScreenSaver                 853
#define XCALL_XSetScreenSaver                   854

/* Standard Geometry 900 */
#define XCALL_XTranslateCoordinates             902
  
/* Text 950 */
#define XCALL_XTextExtents                      950
#define XCALL_XTextExtents16                    951
#define XCALL_XTextWidth                        952
#define XCALL_XTextWidth16                      953

/* Tiles, Pixmaps, Stipples and Bitmaps 1000 */
#define XCALL_XCreateBitmapFromData             1000
#define XCALL_XCreatePixmap                     1001
#define XCALL_XCreatePixmapFromBitmapData       1002

#define XCALL_XQueryBestStipple                 1004
#define XCALL_XQueryBestTile                    1005
#define XCALL_XReadBitmapFile                   1006
#define XCALL_XWriteBitmapFile                  1007

/* User Preferences 1050 */
#define XCALL_XAutoRepeatOff                    1050
#define XCALL_XAutoRepeatOn                     1051
#define XCALL_XBell                             1052
#define XCALL_XGetDefault                       1053

/* Window Attributes 1100 */
#define XCALL_ChangeWindow                      1100
#define XCALL_XGetGeometry                      1101
#define XCALL_XGetWindowAttributes              1102
#define XCALL_XSetWindowBorderWidth             1107
  
/* Window Configuration 1150 */
#define XCALL_XCirculateSubwindows              1150
#define XCALL_XConfigureWindow                  1153
#define XCALL_XLowerWindow                      1154
#define XCALL_XMapRaised                        1155
#define XCALL_XMapSubwindows                    1156
#define XCALL_XMapWindow                        1157
#define XCALL_XMoveResizeWindow                 1158
#define XCALL_XMoveWindow                       1159
#define XCALL_XQueryTree                        1160
#define XCALL_XRaiseWindow                      1161
#define XCALL_XReparentWindow                   1162
#define XCALL_XResizeWindow                     1163
#define XCALL_XRestackWindows                   1164
#define XCALL_XUnmapSubwindows                  1165
#define XCALL_XUnmapWindow                      1166

/* Window Existence 1200 */
#define XCALL_RootWindow                        1200
#define XCALL_DestroyXObject                    1201
#define XCALL_XDestroySubwindows                1202
#define XCALL_XCreateSimpleWindow               1203  
#define XCALL_XCreateWindow                     1204

/* Window Manager 1250 */
#define XCALL_XSetProperty                      1299
#define XCALL_XGetTextProperty                  1250
#define XCALL_XGetWMHints                       1253
#define XCALL_XGetWMSizeHints                   1255
#define XCALL_XGetIconSizes                     1257
#define XCALL_XGetTransientForHint              1259
#define XCALL_XGetWMColormapWindows             1261
#define XCALL_XGetRGBColormaps                  1263
#define XCALL_XWMGeometry                       1264

/* Miscellaneous and Convenience functions 1300 */
#define XCALL_GetID                             1300
#define XCALL_ResourceExists                    1301
#define XCALL_GetDisplay                        1303

/* X Toolkit 3000 */
#define XCALL_NoWidget                          3000

#define XCALL_AppInitialise                     3001
#define XCALL_XtRealizeWidget                   3002
#define XCALL_XtManageChildren                  3003
#define XCALL_XtUnmanageChildren                3004
#define XCALL_XtDestroyWidget                   3005
#define XCALL_SetCallbacks                      3006
#define XCALL_XtSetValues                       3007
#define XCALL_GetValue                          3008
#define XCALL_XtParent                          3009
#define XCALL_XtWindow                          3010
#define XCALL_XtDisplay                         3011
#define XCALL_XtUnrealizeWidget                 3012
#define XCALL_XtName                            3013
#define XCALL_XtParseTranslationTable           3014
#define XCALL_XtOverrideTranslations            3015
#define XCALL_XtAugmentTranslations             3016
#define XCALL_XtUninstallTranslations           3017
#define XCALL_XtTranslateTablePrint             3018
#define XCALL_XtCreatePopupShell                3019
#define XCALL_InsertWidgetTimeout               3020
#define XCALL_GetWidgetState                    3021
#define XCALL_SetWidgetState                    3022
#define XCALL_XtSetSensitive                    3023
#define XCALL_XtIsSensitive                     3024
#define XCALL_GetSubresources                   3025
#define XCALL_Cast                              3026

/* added 6/12/94 SPF */
#define XCALL_XtPopup                           3027
#define XCALL_XtPopdown                         3028
#define XCALL_XtMapWidget                       3029
#define XCALL_XtUnmapWidget                     3030

/* added 19/1/95 SPF */
#define XCALL_XtIsManaged                       3031
#define XCALL_XtIsRealized                      3032

/* added 23/3/01 DCJM */
#define XCALL_XtGetApplicationResources         3033
#define XCALL_XtAddEventHandler                 3034


#define XCALL_XmCreateArrowButton               4000
#define XCALL_XmCreateArrowButtonGadget         4001
#define XCALL_XmCreateBulletinBoard             4002
#define XCALL_XmCreateBulletinBoardDialog       4003
#define XCALL_XmCreateCascadeButton             4004
#define XCALL_XmCreateCascadeButtonGadget       4005
#define XCALL_XmCreateCommand                   4006
#define XCALL_XmCreateDialogShell               4007
#define XCALL_XmCreateDrawingArea               4008
#define XCALL_XmCreateDrawnButton               4009
#define XCALL_XmCreateErrorDialog               4010
#define XCALL_XmCreateFileSelectionBox          4011
#define XCALL_XmCreateFileSelectionDialog       4012
#define XCALL_XmCreateForm                      4013
#define XCALL_XmCreateFormDialog                4014
#define XCALL_XmCreateFrame                     4015
#define XCALL_XmCreateInformationDialog         4016
#define XCALL_XmCreateLabel                     4017
#define XCALL_XmCreateLabelGadget               4018
#define XCALL_XmCreateList                      4019
#define XCALL_XmCreateMainWindow                4020
#define XCALL_XmCreateMenuBar                   4021
#define XCALL_XmCreateMenuShell                 4022
#define XCALL_XmCreateMessageBox                4023
#define XCALL_XmCreateMessageDialog             4024
#define XCALL_XmCreateOptionMenu                4025
#define XCALL_XmCreatePanedWindow               4026
#define XCALL_XmCreatePopupMenu                 4027
#define XCALL_XmCreatePromptDialog              4028
#define XCALL_XmCreatePulldownMenu              4029
#define XCALL_XmCreatePushButton                4030
#define XCALL_XmCreatePushButtonGadget          4031
#define XCALL_XmCreateQuestionDialog            4032
#define XCALL_XmCreateRadioBox                  4033
#define XCALL_XmCreateRowColumn                 4034
#define XCALL_XmCreateScale                     4035
#define XCALL_XmCreateScrollBar                 4036
#define XCALL_XmCreateScrolledList              4037
#define XCALL_XmCreateScrolledText              4038
#define XCALL_XmCreateScrolledWindow            4039
#define XCALL_XmCreateSelectionBox              4040
#define XCALL_XmCreateSelectionDialog           4041
#define XCALL_XmCreateSeparator                 4042
#define XCALL_XmCreateSeparatorGadget           4043
#define XCALL_XmCreateSimpleCheckBox            4044
#define XCALL_XmCreateSimpleMenuBar             4045
#define XCALL_XmCreateSimpleOptionMenu          4046
#define XCALL_XmCreateSimplePopupMenu           4047
#define XCALL_XmCreateSimplePulldownMenu        4048
#define XCALL_XmCreateSimpleRadioBox            4049
#define XCALL_XmCreateText                      4050
#define XCALL_XmCreateTextField                 4051
#define XCALL_XmCreateToggleButton              4052
#define XCALL_XmCreateToggleButtonGadget        4053
#define XCALL_XmCreateWarningDialog             4054
#define XCALL_XmCreateWorkArea                  4055
#define XCALL_XmCreateWorkingDialog             4056
    
#define XCALL_XmCascadeButtonHighlight          4060
#define XCALL_XmCommandError                    4061
#define XCALL_XmCommandGetChild                 4062
#define XCALL_XmFileSelectionBoxGetChild        4063
#define XCALL_XmFileSelectionDoSearch           4064

#define XCALL_XmIsSomething                     4080

#define XCALL_XmMainWindowSetAreas              4100
#define XCALL_XmMainWindowSepX                  4101
#define XCALL_XmMessageBoxGetChild              4105
#define XCALL_XmOptionButtonGadget              4106
#define XCALL_XmOptionLabelGadget               4107
#define XCALL_XmSelectionBoxGetChild            4108
    
#define XCALL_XmSetMenuCursor                   4200
#define XCALL_XmScrolledWindowSetAreas          4201

#define XCALL_XmTextGetString                   4202
#define XCALL_XmTextSetString                   4203
#define XCALL_XmTextSetInsertionPosition        4204


#define XCALL_XmTrackingLocate                  4210
#define XCALL_XmUpdateDisplay                   4211

/* XmText widget 4300 */

/* XmTextClearSelection                         4300 */
/* XmTextCopy                                   4301 */
/* XmTextCut                                    4302 */
#define XCALL_XmTextGetAddMode                  4303
#define XCALL_XmTextGetBaseline                 4304
#define XCALL_XmTextGetCursorPosition           4305   
#define XCALL_XmTextGetEditable                 4306
#define XCALL_XmTextGetInsertionPosition        4307
#define XCALL_XmTextGetLastPosition             4308
#define XCALL_XmTextGetMaxLength                4309
#define XCALL_XmTextGetSelection                4310
/* XmTextGetSelectionPosition                   4311 */
/* XCALL_XmTextGetString defined above */
/* XmTextGetSource                              4313 */
#define XCALL_XmTextGetTopCharacter             4314
#define XCALL_XmTextInsert                      4315
#define XCALL_XmTextPaste                       4316
/* XmTextPosToXY                                4317 */
#define XCALL_XmTextRemove                      4318
#define XCALL_XmTextReplace                     4319
#define XCALL_XmTextScroll                      4320
#define XCALL_XmTextSetAddMode                  4321
#define XCALL_XmTextSetCursorPosition           4322
#define XCALL_XmTextSetEditable                 4323
/* XmTextSetHighlight                           4324 */
/* XCALL_XmTextSetInsertionPosition defined above */
#define XCALL_XmTextSetMaxLength                4326
/* XmTextSetSelection                           4327 */
/* XmTextSetSource                              4328 */
/* XCALL_XmTextSetString defined above */
#define XCALL_XmTextSetTopCharacter             4330
#define XCALL_XmTextShowPosition                4331
#define XCALL_XmTextXYToPos                     4332


#if 0
/* should think about adding the following sometime, but */
/* they are not supported by the (SUNOS) 1.1.4a toolkit  */
extern int XmTextGetSubstring() 
extern int XmTextGetSubstringWcs() 
extern wchar_t * XmTextGetStringWcs() 
extern void XmTextSetStringWcs() 
extern void XmTextReplaceWcs() 
extern void XmTextInsertWcs() 
extern wchar_t * XmTextGetSelectionWcs() 
extern void XmTextDisableRedisplay() 
extern void XmTextEnableRedisplay() 
extern Boolean XmTextFindString() 
extern Boolean XmTextFindStringWcs() 
#endif


/* XmTextField widget 4350 */

/* XmTextFieldClearSelection                    4350 */
/* XmTextFieldCopy                              4351 */
/* XmTextFieldCut                               4352 */
#define XCALL_XmTextFieldGetAddMode             4353
#define XCALL_XmTextFieldGetBaseline            4354            
#define XCALL_XmTextFieldGetCursorPosition      4355            
#define XCALL_XmTextFieldGetEditable            4356
#define XCALL_XmTextFieldGetInsertionPosition   4357
#define XCALL_XmTextFieldGetLastPosition        4358
#define XCALL_XmTextFieldGetMaxLength           4359
#define XCALL_XmTextFieldGetSelection           4360
/* XmTextFieldGetSelectionPosition              4361 */
#define XCALL_XmTextFieldGetString              4362
#define XCALL_XmTextFieldInsert                 4363
#define XCALL_XmTextFieldPaste                  4364
/* XmTextFieldPosToXY                           4365 */
#define XCALL_XmTextFieldRemove                 4366
#define XCALL_XmTextFieldReplace                4367
#define XCALL_XmTextFieldSetAddMode             4368
#define XCALL_XmTextFieldSetCursorPosition      4369
#define XCALL_XmTextFieldSetEditable            4370
/* XmTextFieldSetHighlight                      4371 */
#define XCALL_XmTextFieldSetInsertionPosition   4372
#define XCALL_XmTextFieldSetMaxLength           4373
/* XmTextFieldSetSelection                      4374 */
#define XCALL_XmTextFieldSetString              4375
#define XCALL_XmTextFieldShowPosition           4376
#define XCALL_XmTextFieldXYToPos                4377


#if 0
/* should think about adding the following sometime, but */
/* they are not supported by the (SUNOS) 1.1.4a toolkit  */
extern int XmTextFieldGetSubstring() 
extern wchar_t * XmTextFieldGetStringWcs() 
extern int XmTextFieldGetSubstringWcs() 
extern void XmTextFieldSetStringWcs() 
extern void XmTextFieldReplaceWcs() 
extern void XmTextFieldInsertWcs() 
extern wchar_t * XmTextFieldGetSelectionWcs() 
#endif

/* XmList widget 4400 */

#define XCALL_XmListAddItem             4400
#define XCALL_XmListAddItemUnselected   4401
#define XCALL_XmListAddItems            4402
#define XCALL_XmListDeleteAllItems      4403
#define XCALL_XmListDeleteItem          4404
#define XCALL_XmListDeleteItems         4405
#define XCALL_XmListDeletePos           4406
#define XCALL_XmListDeleteItemsPos      4407
#define XCALL_XmListDeselectAllItems    4408
#define XCALL_XmListDeselectItem        4409
#define XCALL_XmListDeselectPos         4410
#define XCALL_XmListGetMatchPos         4411
#define XCALL_XmListGetSelectedPos      4412
#define XCALL_XmListItemExists          4413
#define XCALL_XmListItemPos             4414 
#define XCALL_XmListReplaceItems        4415 
#define XCALL_XmListReplaceItemsPos     4416
#define XCALL_XmListSelectItem          4417
#define XCALL_XmListSelectPos           4418
#define XCALL_XmListSetAddMode          4419
#define XCALL_XmListSetBottomItem       4420 
#define XCALL_XmListSetBottomPos        4421
#define XCALL_XmListSetHorizPos         4422
#define XCALL_XmListSetItem             4423 
#define XCALL_XmListSetPos              4424
  
    /* Added by DCJM. 23/3/01. */
#define XCALL_XmMenuPosition            4500

#if 0
/* Not supported by the SUNOS version of the tool-kit */
#define XCALL_XmListAddItemsUnselected
#define XCALL_XmListDeletePositions
#define XCALL_XmListGetKbdItemPos
#define XCALL_XmListPosSelected
#define XCALL_XmListReplaceItemsPosUnselected
#define XCALL_XmListReplaceItemsPositions 
#define XCALL_XmListSetKbdItemPos 
#define XCALL_XmListUpdateSelectedList 
#define XCALL_XmListYToPos
#define XCALL_XmListPosToBounds 
#endif                        

