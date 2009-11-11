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
*)

(**************************************************************************

  How do draw commands work ?

  The following functions use a device context to draw to the 
  client area of a window.

  Such a DC can be obtained by GetDC (MLHWND -> MLHDC)
  and released by ReleaseDC ( MLHWWD * MLHDC -> int )
  
  Getting a DC is an expensive operation and in order to 
  make things more efficient we add HDC entries to the window
  table in the runtime system. A winGadget which is a 
  DrawableWindow has a valid HDC (obtained with GetDC) when
  the window ia created. If it is not DrawableWindow it has
  a null HDC.
 
  DrawableWindow is a 'winAttrib.

 **************************************************************************)

functor DrawComFct() =
struct

local
    open UserFn
    open UserFn2
    open GdiFn
    val mkDrawCommand = Gadget.mkDrawCommand

    fun dc1 gf       = mkDrawCommand (fn dc => gf dc)
    fun dc2 gf a     = mkDrawCommand (fn dc => gf (dc,a))
    fun dc3 gf (a,b) = mkDrawCommand (fn dc => gf (dc,a,b))
    fun dc4 gf (a,b,c) = mkDrawCommand (fn dc => gf (dc,a,b,c))
    fun dc5 gf (a,b,c,d) = mkDrawCommand (fn dc => gf (dc,a,b,c,d))
    fun dc6 gf (a,b,c,d,e) = mkDrawCommand (fn dc => gf (dc,a,b,c,d,e))
    fun dc9 gf (a,b,c,d,e,f,g,h) = 
            mkDrawCommand (fn dc => gf (dc,a,b,c,d,e,f,g,h))
    fun dc11 gf (a,b,c,d,e,f,g,h,i,j) = 
            mkDrawCommand (fn dc => gf (dc,a,b,c,d,e,f,g,h,i,j))
    fun dc12 gf (a,b,c,d,e,f,g,h,i,j,k) = 
            mkDrawCommand (fn dc => gf (dc,a,b,c,d,e,f,g,h,i,j,k))
in
    val GetClipBox = dc1 GetClipBox
    val OffsetWindowOrgEx = dc3 OffsetWindowOrgEx
    val InvertRect = dc2 InvertRect
    val SetArcDirection = dc2 SetArcDirection
    val CreateCompatibleBitmap = dc3 CreateCompatibleBitmap
    val GetMetaRgn = dc2 GetMetaRgn
    val SetPolyFillMode = dc2 SetPolyFillMode
    val StretchBlt = dc11 StretchBlt
    val SetMetaRgn = dc1 SetMetaRgn
    val GetCurrentPositionEx = dc1 GetCurrentPositionEx
    val StrokePath = dc1 StrokePath
    val GetWorldTransform = dc1 GetWorldTransform
    val SetMapperFlags = dc2 SetMapperFlags
    val FrameRgn = dc5 FrameRgn
    val SetWorldTransform = dc2 SetWorldTransform
    val FlattenPath = dc1 FlattenPath 
    val SelectClipRgn = dc2 SelectClipRgn
    val GetDeviceCaps = dc2 GetDeviceCaps
    val GetDCOrgEx = dc1 GetDCOrgEx
    val PtVisible = dc4 PtVisible
    val WidenPath = dc1 WidenPath
    val CreateHalftonePalette = dc1 CreateHalftonePalette
    val GetMapMode = dc1 GetMapMode
    val SelectObject = dc2 SelectObject
    val SetMapMode = dc3 SetMapMode
    val GetStretchBltMode = dc1 GetStretchBltMode
    val RectVisible = dc2 RectVisible
    val OffsetClipRgn = dc3 OffsetClipRgn
    val SetStretchBltMode = dc2 SetStretchBltMode
    val GetMiterLimit = dc1 GetMiterLimit
    val GetSystemPaletteUse = dc1 GetSystemPaletteUse
    val GetWindowExtEx = dc1 GetWindowExtEx
    val SetMiterLimit = dc2 SetMiterLimit
    val SetSystemPaletteUse = dc2 SetSystemPaletteUse
    val SetWindowExtEx = dc3 SetWindowExtEx
    val GetAspectRatioFilterEx = dc1 GetAspectRatioFilterEx
    val GetGraphicsMode = dc1 GetGraphicsMode
    val ExtSelectClipRgn = dc3 ExtSelectClipRgn
    val PathToRegion = dc1 PathToRegion
    val GetCurrentObject = dc2 GetCurrentObject
    val InvertRgn = dc2 InvertRgn
    val AbortPath = dc1 AbortPath
    val FillRgn = dc3 FillRgn
    val PaintRgn = dc2 PaintRgn
    val GetTextCharacterExtra = dc1 GetTextCharacterExtra
    val ArcTo = dc6 ArcTo
    val PatBlt = dc6 PatBlt
    val SelectClipPath = dc2 SelectClipPath
    val GetViewportOrgEx = dc1 GetViewportOrgEx
    val GetWindowOrgEx = dc1 GetWindowOrgEx
    val GetBrushOrgEx = dc1 GetBrushOrgEx
    val FillRect = dc3 FillRect
    val SetTextCharacterExtra = dc2 SetTextCharacterExtra
    val SelectPalette = dc3 SelectPalette
    val UpdateColors = dc1 UpdateColors
    val SetViewportOrgEx = dc3 SetViewportOrgEx
    val SetWindowOrgEx = dc3 SetWindowOrgEx
    val SetTextJustification = dc3 SetTextJustification
    val DrawText = dc4 DrawText
    val SetBrushOrgEx = dc2 SetBrushOrgEx
    val RealizePalette = dc1 RealizePalette
    val GetViewportExtEx = dc1 GetViewportExtEx
    val Arc = dc6 Arc
    val BeginPath = dc1 BeginPath
    val OffsetViewportOrgEx = dc3 OffsetViewportOrgEx
    val MoveToEx = dc3 MoveToEx
    val BitBlt = dc9 BitBlt
    val SetViewportExtEx = dc3 SetViewportExtEx
    val GetClipRgn = dc2 GetClipRgn
    val GetTextAlign = dc1 GetTextAlign
    val FillPath = dc1 FillPath
    val StrokeAndFillPath = dc1 StrokeAndFillPath
    val EndPath = dc1 EndPath
    val AngleArc = dc6 AngleArc
    val FrameRect = dc3 FrameRect
    val SetTextAlign = dc2 SetTextAlign
    val GetPolyFillMode = dc1 GetPolyFillMode
    val ModifyWorldTransform = dc3 ModifyWorldTransform
    val CloseFigure = dc1 CloseFigure
    val MaskBlt = dc12 MaskBlt
    val SetBoundsRect = dc3 SetBoundsRect
    val GetROP2 = dc1 GetROP2
    val SetROP2 = dc2 SetROP2
    val ScrollDC = dc6 ScrollDC
    val WindowFromDC = dc1 WindowFromDC
    val ExcludeUpdateRgn = dc2 ExcludeUpdateRgn
    val DrawIcon = dc4 DrawIcon
    val DrawFocusRect = dc2 DrawFocusRect
    val GetBkMode = dc1 GetBkMode
    val GetBoundsRect = dc2 GetBoundsRect
    val SetBkMode = dc2 SetBkMode
    val ExcludeClipRect = dc2 ExcludeClipRect
    val Pie = dc4 Pie
    val PolyDraw = dc2 PolyDraw
    val DrawRectangle = dc2 DrawRectangle
    val PolyBezierTo = dc2 PolyBezierTo
    val GetPath = dc2 GetPath
    val GetNearestColor = dc2 GetNearestColor
    val PolylineTo = dc2 PolylineTo
    val ExtFloodFill = dc5 ExtFloodFill
    val GetCharABCWidthsFloat = dc3 GetCharABCWidthsFloat
    val Polyline = dc2 Polyline
    val DrawRoundRect = dc4 DrawRoundRect
    val GetCharABCWidths = dc3 GetCharABCWidths
    val GetCharWidth32 = dc3 GetCharWidth32
    val DPtoLP = dc2 DPtoLP
    val LPtoDP = dc2 LPtoDP
    val TextOut = dc3 TextOut
    val GetCharWidthFloat = dc3 GetCharWidthFloat
    val ExtTextOut = dc6 ExtTextOut
    val Ellipse = dc2 Ellipse
    val GetPixel = dc3 GetPixel
    val GetTextExtentPoint32 = dc2 GetTextExtentPoint32
    val SetPixel = dc4 SetPixel
    val PolyBezier = dc2 PolyBezier
    val Polygon = dc2 Polygon
    val IntersectClipRect = dc2 IntersectClipRect
    val GetTextColor = dc1 GetTextColor
    val SetTextColor = dc2 SetTextColor
    val LineTo = dc2 LineTo
    val GetBkColor = dc1 GetBkColor
    val SetBkColor = dc2 SetBkColor
    val Chord = dc4 Chord
end
end;        
