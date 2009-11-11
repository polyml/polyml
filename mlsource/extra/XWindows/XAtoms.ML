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

structure XAtoms: XATOMS_SIG = struct

  val XA_PRIMARY              = 1 ;
  val XA_SECONDARY            = 2 ;
  val XA_ARC                  = 3 ;
  val XA_ATOM                 = 4 ;
  val XA_BITMAP               = 5 ;
  val XA_CARDINAL             = 6 ;
  val XA_COLORMAP             = 7 ;
  val XA_CURSOR               = 8 ;
  val XA_CUT_BUFFER0          = 9 ;
  val XA_CUT_BUFFER1          = 10 ;
  val XA_CUT_BUFFER2          = 11 ;
  val XA_CUT_BUFFER3          = 12 ;
  val XA_CUT_BUFFER4          = 13 ;
  val XA_CUT_BUFFER5          = 14 ;
  val XA_CUT_BUFFER6          = 15 ;
  val XA_CUT_BUFFER7          = 16 ;
  val XA_DRAWABLE             = 17 ;
  val XA_FONT                 = 18 ;
  val XA_INTEGER              = 19 ;
  val XA_PIXMAP               = 20 ;
  val XA_POINT                = 21 ;
  val XA_RECTANGLE            = 22 ;
  val XA_RESOURCE_MANAGER     = 23 ;
  val XA_RGB_COLOR_MAP        = 24 ;
  val XA_RGB_BEST_MAP         = 25 ;
  val XA_RGB_BLUE_MAP         = 26 ;
  val XA_RGB_DEFAULT_MAP      = 27 ;
  val XA_RGB_GRAY_MAP         = 28 ;
  val XA_RGB_GREEN_MAP        = 29 ;
  val XA_RGB_RED_MAP          = 30 ;
  val XA_STRING               = 31 ;
  val XA_VISUALID             = 32 ;
  val XA_WINDOW               = 33 ;
  val XA_WM_COMMAND           = 34 ;
  val XA_WM_HINTS             = 35 ;
  val XA_WM_CLIENT_MACHINE    = 36 ;
  val XA_WM_ICON_NAME         = 37 ;
  val XA_WM_ICON_SIZE         = 38 ;
  val XA_WM_NAME              = 39 ;
  val XA_WM_NORMAL_HINTS      = 40 ;
  val XA_WM_SIZE_HINTS        = 41 ;
  val XA_MIN_SPACE            = 43 ;
  val XA_NORM_SPACE           = 44 ;
  val XA_MAX_SPACE            = 45 ;
  val XA_END_SPACE            = 46 ;
  val XA_SUPERSCRIPT_X        = 47 ;
  val XA_SUPERSCRIPT_Y        = 48 ;
  val XA_SUBSCRIPT_X          = 49 ;
  val XA_SUBSCRIPT_Y          = 50 ;
  val XA_UNDERLINE_POSITION   = 51 ;
  val XA_UNDERLINE_THICKNESS  = 52 ;
  val XA_STRIKEOUT_ASCENT     = 53 ;
  val XA_STRIKEOUT_DESCENT    = 54 ;
  val XA_ITALIC_ANGLE         = 55 ;
  val XA_X_HEIGHT             = 56 ;
  val XA_QUAD_WIDTH           = 57 ;
  val XA_WEIGHT               = 58 ;
  val XA_POINT_SIZE           = 59 ;
  val XA_RESOLUTION           = 60 ;
  val XA_COPYRIGHT            = 61 ;
  val XA_NOTICE               = 62 ;
  val XA_FONT_NAME            = 63 ;
  val XA_FAMILY_NAME          = 64 ;
  val XA_FULL_NAME            = 65 ;
  val XA_CAP_HEIGHT           = 66 ;
  val XA_WM_CLASS             = 67 ;
  val XA_WM_TRANSIENT_FOR     = 68 ;

end (* XAtoms *) ;
