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

signature XEVENT_SIG = sig

  include XTYPES_SIG ;

  datatype EventMask = KeyPressMask             | KeyReleaseMask      | ButtonPressMask     | ButtonReleaseMask
                     | EnterWindowMask          | LeaveWindowMask     | PointerMotionMask   | PointerMotionHintMask
                     | Button1MotionMask        | Button2MotionMask   | Button3MotionMask   | Button4MotionMask        
                     | Button5MotionMask        | ButtonMotionMask    | KeymapStateMask     | ExposureMask
                     | VisibilityChangeMask     | StructureNotifyMask | ResizeRedirectMask  | SubstructureNotifyMask
                     | SubstructureRedirectMask | FocusChangeMask     | PropertyChangeMask  | ColormapChangeMask
                     | OwnerGrabButtonMask
                     | ButtonClickMask      (* extra event mask *) ;
  
  datatype Modifier = ShiftMask   | LockMask    | ControlMask
                    | Mod1Mask    | Mod2Mask    | Mod3Mask    | Mod4Mask    | Mod5Mask
                    | Button1Mask | Button2Mask | Button3Mask | Button4Mask | Button5Mask
                    | AnyModifier ;

  datatype ButtonName = Button1 | Button2 | Button3 | Button4 | Button5 | AnyButton ;
  
  datatype Placement = PlaceOnTop | PlaceOnBottom ;
  
  datatype StackMode = Above | Below | TopIf | BottomIf | Opposite ;
  
  datatype NotifyMode = NotifyNormal | NotifyGrab | NotifyUngrab | NotifyWhileGrabbed ;
  
  datatype NotifyDetail = NotifyAncestor         | NotifyVirtual | NotifyInferior    | NotifyNonLinear
                        | NotifyNonLinearVirtual | NotifyPointer | NotifyPointerRoot | NotifyDetailNone ;
  
  datatype GraphicsCode = CopyArea | CopyPlane ;
  
  datatype Visibility = VisibilityUnobscured | VisibilityPartiallyObscured | VisibilityFullyObscured ;
  
  datatype 'a XEvent = ButtonPress      of { sendEvent:   bool,
                                             window:      Drawable,
                                             root:        Drawable,
                                             subwindow:   Drawable,
                                             time:        int,
                                             pointer:     XPoint,
                                             rootPointer: XPoint,
                                             modifiers:   Modifier list,
                                             button:      ButtonName }

                     | ButtonRelease    of { sendEvent:   bool,
                                             window:      Drawable,
                                             root:        Drawable,
                                             subwindow:   Drawable,
                                             time:        int,
                                             pointer:     XPoint,
                                             rootPointer: XPoint,
                                             modifiers:   Modifier list,
                                             button:      ButtonName }

                     | ButtonClick      of { sendEvent:   bool,                (* extra event type *)
                                             window:      Drawable,
                                             root:        Drawable,
                                             subwindow:   Drawable,
                                             time:        int,
                                             pointer:     XPoint,
                                             rootPointer: XPoint,
                                             modifiers:   Modifier list,
                                             button:      ButtonName,
                                             up:          int,                 (* number of up   transitions *)
                                             down:        int }                (* number of down transitions *)

                     | CirculateNotify  of { sendEvent: bool,
                                             event:     Drawable,
                                             window:    Drawable,
                                             place:     Placement }

                     | CirculateRequest of { sendEvent: bool,
                                             parent:    Drawable,
                                             window:    Drawable,
                                             place:     Placement }

                     | ColormapNotify   of { sendEvent: bool,
                                             window:    Drawable,
                                             colormap:  Colormap,
                                             new:       bool,
                                             installed: bool }

                     | ConfigureNotify  of { sendEvent:        bool,
                                             event:            Drawable,
                                             window:           Drawable,
                                             position:         XPoint,
                                             size:             XRectangle,
                                             borderWidth:      int,
                                             above:            Drawable,
                                             overrideRedirect: bool }

                     | ConfigureRequest of { sendEvent:   bool,
                                             parent:      Drawable,
                                             window:      Drawable,
                                             position:    XPoint,
                                             size:        XRectangle,
                                             borderWidth: int,
                                             above:       Drawable,
                                             detail:      StackMode }

                     | CreateNotify     of { sendEvent:        bool,
                                             parent:           Drawable,
                                             window:           Drawable,
                                             position:         XPoint,
                                             size:             XRectangle,
                                             borderWidth:      int,
                                             overrideRedirect: bool }

                     | DestroyNotify    of { sendEvent: bool,
                                             event:     Drawable,
                                             window:    Drawable }

                     | EnterNotify      of { sendEvent:   bool,
                                             window:      Drawable,
                                             root:        Drawable,
                                             subwindow:   Drawable,
                                             time:        int,
                                             pointer:     XPoint,
                                             rootPointer: XPoint,
                                             mode:        NotifyMode,
                                             detail:      NotifyDetail,
                                             focus:       bool,
                                             modifiers:   Modifier list }

                     | LeaveNotify      of { sendEvent:   bool,
                                             window:      Drawable,
                                             root:        Drawable,
                                             subwindow:   Drawable,
                                             time:        int,
                                             pointer:     XPoint,
                                             rootPointer: XPoint,
                                             mode:        NotifyMode,
                                             detail:      NotifyDetail,
                                             focus:       bool,
                                             modifiers:   Modifier list }

                     | Expose           of { sendEvent: bool,
                                             window:    Drawable,
                                             region:    XRectangle,
                                             count:     int }

                     | FocusIn          of { sendEvent: bool,
                                             window:    Drawable,
                                             mode:      NotifyMode,
                                             detail:    NotifyDetail }

                     | FocusOut         of { sendEvent: bool,
                                             window:    Drawable,
                                             mode:      NotifyMode,
                                             detail:    NotifyDetail }

                     | GraphicsExpose   of { sendEvent: bool,
                                             window:    Drawable,
                                             region:    XRectangle,
                                             count:     int,
                                             code:      GraphicsCode }

                     | NoExpose         of { sendEvent: bool,
                                             window:    Drawable,
                                             code:      GraphicsCode }

                     | GravityNotify    of { sendEvent: bool,
                                             event:     Drawable,
                                             window:    Drawable,
                                             position:  XPoint }

                     | KeymapNotify     of { sendEvent: bool,
                                             window:    Drawable,
                                             keyVector: bool list (* 256 bools *) }

                     | KeyPress         of { sendEvent:   bool,
                                             window:      Drawable,
                                             root:        Drawable,
                                             subwindow:   Drawable,
                                             time:        int,
                                             pointer:     XPoint,
                                             rootPointer: XPoint,
                                             modifiers:   Modifier list,
                                             keycode:     int }

                     | KeyRelease       of { sendEvent:   bool,
                                             window:      Drawable,
                                             root:        Drawable,
                                             subwindow:   Drawable,
                                             time:        int,
                                             pointer:     XPoint,
                                             rootPointer: XPoint,
                                             modifiers:   Modifier list,
                                             keycode:     int }

                     | MapNotify        of { sendEvent:        bool,
                                             event:            Drawable,
                                             window:           Drawable,
                                             overrideRedirect: bool }

                     | UnmapNotify      of { sendEvent:     bool,
                                             event:         Drawable,
                                             window:        Drawable,
                                             fromConfigure: bool }

                     | MapRequest       of { sendEvent: bool,
                                             parent:    Drawable,
                                             window:    Drawable }

                     | MotionNotify     of { sendEvent:   bool,
                                             window:      Drawable,
                                             root:        Drawable,
                                             subwindow:   Drawable,
                                             time:        int,
                                             pointer:     XPoint,
                                             rootPointer: XPoint,
                                             modifiers:   Modifier list,
                                             isHint:      bool }

                     | ReparentNotify   of { sendEvent:        bool,
                                             event:            Drawable,
                                             window:           Drawable,
                                             parent:           Drawable,
                                             position:         XPoint,
                                             overrideRedirect: bool }

                     | ResizeRequest    of { sendEvent: bool,
                                             window:    Drawable,
                                             size:      XRectangle }

                     | SelectionClear   of { sendEvent: bool,
                                             window:    Drawable,
                                             selection: int,
                                             time:      int }
                     
                     | SelectionNotify  of { sendEvent: bool,
                                             requestor: Drawable,
                                             selection: int,
                                             target:    int,
                                             property:  int,
                                             time:      int }
                     
                     | SelectionRequest of { sendEvent: bool,
                                             owner:     Drawable,
                                             requestor: Drawable,
                                             selection: int,
                                             target:    int,
                                             property:  int,
                                             time:      int }
                     
                     | VisibilityNotify of { sendEvent:  bool,
                                             window:     Drawable,
                                             visibility: Visibility }

                     | DeleteRequest    of { window: Drawable }              (* extra event type *)

                     | Message          of { window: Drawable, message: 'a } (* extra event type *)
                     
                     | NoEvent ;

end (* XEVENT_SIG *) ;
