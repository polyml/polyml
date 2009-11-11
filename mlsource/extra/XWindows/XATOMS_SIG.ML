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

signature XATOMS_SIG = sig

  val XA_PRIMARY:              int ;
  val XA_SECONDARY:            int ;
  val XA_ARC:                  int ;
  val XA_ATOM:                 int ;
  val XA_BITMAP:               int ;
  val XA_CARDINAL:             int ;
  val XA_COLORMAP:             int ;
  val XA_CURSOR:               int ;
  val XA_CUT_BUFFER0:          int ;
  val XA_CUT_BUFFER1:          int ;
  val XA_CUT_BUFFER2:          int ;
  val XA_CUT_BUFFER3:          int ;
  val XA_CUT_BUFFER4:          int ;
  val XA_CUT_BUFFER5:          int ;
  val XA_CUT_BUFFER6:          int ;
  val XA_CUT_BUFFER7:          int ;
  val XA_DRAWABLE:             int ;
  val XA_FONT:                 int ;
  val XA_INTEGER:              int ;
  val XA_PIXMAP:               int ;
  val XA_POINT:                int ;
  val XA_RECTANGLE:            int ;
  val XA_RESOURCE_MANAGER:     int ;
  val XA_RGB_COLOR_MAP:        int ;
  val XA_RGB_BEST_MAP:         int ;
  val XA_RGB_BLUE_MAP:         int ;
  val XA_RGB_DEFAULT_MAP:      int ;
  val XA_RGB_GRAY_MAP:         int ;
  val XA_RGB_GREEN_MAP:        int ;
  val XA_RGB_RED_MAP:          int ;
  val XA_STRING:               int ;
  val XA_VISUALID:             int ;
  val XA_WINDOW:               int ;
  val XA_WM_COMMAND:           int ;
  val XA_WM_HINTS:             int ;
  val XA_WM_CLIENT_MACHINE:    int ;
  val XA_WM_ICON_NAME:         int ;
  val XA_WM_ICON_SIZE:         int ;
  val XA_WM_NAME:              int ;
  val XA_WM_NORMAL_HINTS:      int ;
  val XA_WM_SIZE_HINTS:        int ;
  val XA_MIN_SPACE:            int ;
  val XA_NORM_SPACE:           int ;
  val XA_MAX_SPACE:            int ;
  val XA_END_SPACE:            int ;
  val XA_SUPERSCRIPT_X:        int ;
  val XA_SUPERSCRIPT_Y:        int ;
  val XA_SUBSCRIPT_X:          int ;
  val XA_SUBSCRIPT_Y:          int ;
  val XA_UNDERLINE_POSITION:   int ;
  val XA_UNDERLINE_THICKNESS:  int ;
  val XA_STRIKEOUT_ASCENT:     int ;
  val XA_STRIKEOUT_DESCENT:    int ;
  val XA_ITALIC_ANGLE:         int ;
  val XA_X_HEIGHT:             int ;
  val XA_QUAD_WIDTH:           int ;
  val XA_WEIGHT:               int ;
  val XA_POINT_SIZE:           int ;
  val XA_RESOLUTION:           int ;
  val XA_COPYRIGHT:            int ;
  val XA_NOTICE:               int ;
  val XA_FONT_NAME:            int ;
  val XA_FAMILY_NAME:          int ;
  val XA_FULL_NAME:            int ;
  val XA_CAP_HEIGHT:           int ;
  val XA_WM_CLASS:             int ;
  val XA_WM_TRANSIENT_FOR:     int ;

end (* XATOMS_SIG *) ;
