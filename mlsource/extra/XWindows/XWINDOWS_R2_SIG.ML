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


(* Grabbing *)
  
  eqtype GrabMode ;
  
  val GrabModeSync:  GrabMode ;
  val GrabModeAsync: GrabMode ;
  
  eqtype GrabStatus ;
  
  val GrabSuccess:     GrabStatus ;
  val AlreadyGrabbed:  GrabStatus ;
  val GrabInvalidTime: GrabStatus ;
  val GrabNotViewable: GrabStatus ;
  val GrabFrozen:      GrabStatus ;
  
(*
  val XChangeActivePointerGrab: EventMask list -> Cursor -> int -> unit ;
*)
(*
  val XGrabButton:              ButtonName -> Modifier list -> Drawable -> bool -> EventMask list -> GrabMode -> GrabMode -> Drawable -> Cursor -> unit ;
*)
(*
  val XGrabKey:                 int        -> Modifier list -> Drawable -> bool -> GrabMode -> GrabMode -> unit ;
*)
(*
  val XGrabKeyboard:            Drawable -> bool -> GrabMode -> GrabMode -> int -> GrabStatus ;
*)
(*
  val XGrabPointer:             Drawable -> bool -> EventMask list -> GrabMode -> GrabMode -> Drawable -> Cursor -> int -> GrabStatus ;
*)
(*
  val XGrabServer:              unit -> unit ;
*)
(*
  val XUngrabButton:            ButtonName -> Modifier list -> Drawable -> unit ;
*)
(*
  val XUngrabKey:               int        -> Modifier list -> Drawable -> unit ;
*)
(*
  val XUngrabKeyboard:          int  -> unit ;
*)
(*
  val XUngrabPointer:           int  -> unit ;
*)
(*
  val XUngrabServer:            unit -> unit ;
*)
  
  val CurrentTime: int ;
  
  (* Keyboard *)
  
  eqtype AutoRepeatMode ;
  
  val AutoRepeatModeOff:     AutoRepeatMode ;
  val AutoRepeatModeOn:      AutoRepeatMode ;
  val AutoRepeatModeDefault: AutoRepeatMode ;
  
  datatype XKeyboardControl = KBKeyClickPercent of int
                            | KBBellPercent     of int
                            | KBBellPitch       of int
                            | KBBellDuration    of int
                            | KBLed             of int
                            | KBLedMode         of bool
                            | KBKey             of int
                            | KBAutoRepeatMode  of AutoRepeatMode ;
                            
  datatype XKeyboardState = XKeyboardState of { keyClickPercent:  int,
                                                bellPercent:      int,
                                                bellPitch:        int,
                                                bellDuration:     int,
                                                leds:             bool list,  (* 32  bools *)
                                                autoRepeats:      bool list,  (* 256 bools *)
                                                globalAutoRepeat: AutoRepeatMode } ;
                                                
  
  datatype XModifierKeymap = XModifierKeymap of int             (* maxKeysPerModifier               *)
                                              * int list list ; (* 8 by maxKeysPerModifier keycodes *)
  
  eqtype MapIndex ;

  val ShiftMapIndex:   MapIndex ;
  val LockMapIndex:    MapIndex ;
  val ControlMapIndex: MapIndex ;
  val Mod1MapIndex:    MapIndex ;
  val Mod2MapIndex:    MapIndex ;
  val Mod3MapIndex:    MapIndex ;
  val Mod4MapIndex:    MapIndex ;
  val Mod5MapIndex:    MapIndex ;
  
  eqtype MapStatus ;
  
  val MappingSuccess: MapStatus ;
  val MappingBusy:    MapStatus ;
  val MappingFailed:  MapStatus ;

(*
  val XChangeKeyboardControl:  XKeyboardControl list -> unit ;
*)
(*
  val XChangeKeyboardMapping:  int -> int -> int list -> unit ;
*)
(*
  val XDeleteModifiermapEntry: XModifierKeymap -> int -> MapIndex -> unit ;
*)
(*
  val XGetKeyboardControl:     unit -> XKeyboardState ;
*)
(*
  val XGetKeyboardMapping:     int -> int -> int list ;
*)
(*
  val XGetModifierMapping:     unit -> XModifierKeymap ;
*)
(*
  val XInsertModifiermapEntry: XModifierKeymap -> int -> MapIndex -> XModifierKeymap ;
*)
(*
  val XKeycodeToKeysym:        int -> int -> int ;
*)
(*
  val XKeysymToKeycode:        int -> int ;
*)
(*
  val XKeysymToString:         int -> string ;
*)
(*
  val XLookupKeysym:           int -> MapIndex -> int ;
*)
(*
  val XNewModifiermap:         int -> XModifierKeymap ;
*)
(*
  val XQueryKeymap:            unit -> bool list ; (* 256 bools *)
*)
(*
  val XRebindKeysym:           int -> int list -> string -> unit ;
*)
(*
  val XRefreshKeyboardMapping: XMappingEvent -> unit ;
*)
(*
  val XSetModifierMapping:     XModifierKeymap -> MapStatus ;
*)
(*
  val XStringToKeysym:         string -> int ;
*)
  
  (* Pointers *)
  
(*
  val XChangePointerControl: bool -> bool -> int -> int -> int -> unit ;
*)
(*
  val XGetPointerControl:    unit -> (int * int * int) ;
*)
(*
  val XGetPointerMapping:    int -> int list ;
*)
(*
  val XSetPointerMapping:    int list -> MapStatus ;
*)
(*
  val XWarpPointer:          Drawable -> Drawable -> XRectangle -> XPoint -> unit ;
*)
  
  (* Save Set *)
  
  eqtype SetMode ;
  
  val SetModeInsert: SetMode ;
  val SetModeDelete: SetMode ;
  
(*
  val XAddToSaveSet:      Drawable -> unit ;
*)
(*
  val XChangeSaveSet:     Drawable -> SetMode -> unit ;
*)
(*
  val XRemoveFromSaveSet: Drawable -> unit ;
*)
  
