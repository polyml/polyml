(*
    Copyright (c) 2001
        David C.J. Matthews

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
structure WinSystem :
  sig   
    type SystemMetrics
    val SM_ARRANGE : SystemMetrics
    val SM_CLEANBOOT : SystemMetrics
    val SM_CMOUSEBUTTONS : SystemMetrics
    val SM_CXBORDER : SystemMetrics
    val SM_CXCURSOR : SystemMetrics
    val SM_CXDLGFRAME : SystemMetrics
    val SM_CXDOUBLECLK : SystemMetrics
    val SM_CXDRAG : SystemMetrics
    val SM_CXEDGE : SystemMetrics
    val SM_CXFIXEDFRAME : SystemMetrics
    val SM_CXFRAME : SystemMetrics
    val SM_CXFULLSCREEN : SystemMetrics
    val SM_CXHSCROLL : SystemMetrics
    val SM_CXHTHUMB : SystemMetrics
    val SM_CXICON : SystemMetrics
    val SM_CXICONSPACING : SystemMetrics
    val SM_CXMAXIMIZED : SystemMetrics
    val SM_CXMAXTRACK : SystemMetrics
    val SM_CXMENUCHECK : SystemMetrics
    val SM_CXMENUSIZE : SystemMetrics
    val SM_CXMIN : SystemMetrics
    val SM_CXMINIMIZED : SystemMetrics
    val SM_CXMINSPACING : SystemMetrics
    val SM_CXMINTRACK : SystemMetrics
    val SM_CXSCREEN : SystemMetrics
    val SM_CXSIZE : SystemMetrics
    val SM_CXSIZEFRAME : SystemMetrics
    val SM_CXSMICON : SystemMetrics
    val SM_CXSMSIZE : SystemMetrics
    val SM_CXVSCROLL : SystemMetrics
    val SM_CYBORDER : SystemMetrics
    val SM_CYCAPTION : SystemMetrics
    val SM_CYCURSOR : SystemMetrics
    val SM_CYDLGFRAME : SystemMetrics
    val SM_CYDOUBLECLK : SystemMetrics
    val SM_CYDRAG : SystemMetrics
    val SM_CYEDGE : SystemMetrics
    val SM_CYFIXEDFRAME : SystemMetrics
    val SM_CYFRAME : SystemMetrics
    val SM_CYFULLSCREEN : SystemMetrics
    val SM_CYHSCROLL : SystemMetrics
    val SM_CYICON : SystemMetrics
    val SM_CYICONSPACING : SystemMetrics
    val SM_CYKANJIWINDOW : SystemMetrics
    val SM_CYMAXIMIZED : SystemMetrics
    val SM_CYMAXTRACK : SystemMetrics
    val SM_CYMENU : SystemMetrics
    val SM_CYMENUCHECK : SystemMetrics
    val SM_CYMENUSIZE : SystemMetrics
    val SM_CYMIN : SystemMetrics
    val SM_CYMINIMIZED : SystemMetrics
    val SM_CYMINSPACING : SystemMetrics
    val SM_CYMINTRACK : SystemMetrics
    val SM_CYSCREEN : SystemMetrics
    val SM_CYSIZE : SystemMetrics
    val SM_CYSIZEFRAME : SystemMetrics
    val SM_CYSMCAPTION : SystemMetrics
    val SM_CYSMICON : SystemMetrics
    val SM_CYSMSIZE : SystemMetrics
    val SM_CYVSCROLL : SystemMetrics
    val SM_CYVTHUMB : SystemMetrics
    val SM_DBCSENABLED : SystemMetrics
    val SM_DEBUG : SystemMetrics
    val SM_MENUDROPALIGNMENT : SystemMetrics
    val SM_MIDEASTENABLED : SystemMetrics
    val SM_MOUSEPRESENT : SystemMetrics
    val SM_MOUSEWHEELPRESENT : SystemMetrics
    val SM_NETWORK : SystemMetrics
    val SM_PENWINDOWS : SystemMetrics
    val SM_RESERVED1 : SystemMetrics
    val SM_RESERVED2 : SystemMetrics
    val SM_RESERVED3 : SystemMetrics
    val SM_RESERVED4 : SystemMetrics
    val SM_SECURE : SystemMetrics
    val SM_SHOWSOUNDS : SystemMetrics
    val SM_SLOWMACHINE : SystemMetrics
    val SM_SWAPBUTTON : SystemMetrics
    val GetSystemMetrics : SystemMetrics -> int
  end
 =
struct
local
    open CInterface
    open Base
in
    abstype SystemMetrics = ABS of int
    with
        val SM_CXSCREEN             = ABS 0
        val SM_CYSCREEN             = ABS 1
        val SM_CXVSCROLL            = ABS 2
        val SM_CYHSCROLL            = ABS 3
        val SM_CYCAPTION            = ABS 4
        val SM_CXBORDER             = ABS 5
        val SM_CYBORDER             = ABS 6
        val SM_CXDLGFRAME           = ABS 7
        val SM_CYDLGFRAME           = ABS 8
        val SM_CYVTHUMB             = ABS 9
        val SM_CXHTHUMB             = ABS 10
        val SM_CXICON               = ABS 11
        val SM_CYICON               = ABS 12
        val SM_CXCURSOR             = ABS 13
        val SM_CYCURSOR             = ABS 14
        val SM_CYMENU               = ABS 15
        val SM_CXFULLSCREEN         = ABS 16
        val SM_CYFULLSCREEN         = ABS 17
        val SM_CYKANJIWINDOW        = ABS 18
        val SM_MOUSEPRESENT         = ABS 19
        val SM_CYVSCROLL            = ABS 20
        val SM_CXHSCROLL            = ABS 21
        val SM_DEBUG                = ABS 22
        val SM_SWAPBUTTON           = ABS 23
        val SM_RESERVED1            = ABS 24
        val SM_RESERVED2            = ABS 25
        val SM_RESERVED3            = ABS 26
        val SM_RESERVED4            = ABS 27
        val SM_CXMIN                = ABS 28
        val SM_CYMIN                = ABS 29
        val SM_CXSIZE               = ABS 30
        val SM_CYSIZE               = ABS 31
        val SM_CXFRAME              = ABS 32
        val SM_CYFRAME              = ABS 33
        val SM_CXMINTRACK           = ABS 34
        val SM_CYMINTRACK           = ABS 35
        val SM_CXDOUBLECLK          = ABS 36
        val SM_CYDOUBLECLK          = ABS 37
        val SM_CXICONSPACING        = ABS 38
        val SM_CYICONSPACING        = ABS 39
        val SM_MENUDROPALIGNMENT    = ABS 40
        val SM_PENWINDOWS           = ABS 41
        val SM_DBCSENABLED          = ABS 42
        val SM_CMOUSEBUTTONS        = ABS 43
        val SM_SECURE               = ABS 44
        val SM_CXEDGE               = ABS 45
        val SM_CYEDGE               = ABS 46
        val SM_CXMINSPACING         = ABS 47
        val SM_CYMINSPACING         = ABS 48
        val SM_CXSMICON             = ABS 49
        val SM_CYSMICON             = ABS 50
        val SM_CYSMCAPTION          = ABS 51
        val SM_CXSMSIZE             = ABS 52
        val SM_CYSMSIZE             = ABS 53
        val SM_CXMENUSIZE           = ABS 54
        val SM_CYMENUSIZE           = ABS 55
        val SM_ARRANGE              = ABS 56
        val SM_CXMINIMIZED          = ABS 57
        val SM_CYMINIMIZED          = ABS 58
        val SM_CXMAXTRACK           = ABS 59
        val SM_CYMAXTRACK           = ABS 60
        val SM_CXMAXIMIZED          = ABS 61
        val SM_CYMAXIMIZED          = ABS 62
        val SM_NETWORK              = ABS 63
        val SM_CLEANBOOT            = ABS 67
        val SM_CXDRAG               = ABS 68
        val SM_CYDRAG               = ABS 69
        val SM_SHOWSOUNDS           = ABS 70
        val SM_CXMENUCHECK          = ABS 71
        val SM_CYMENUCHECK          = ABS 72
        val SM_SLOWMACHINE          = ABS 73
        val SM_MIDEASTENABLED       = ABS 74
        val SM_MOUSEWHEELPRESENT    = ABS 75
        
        val SM_CXFIXEDFRAME         = SM_CXDLGFRAME
        val SM_CYFIXEDFRAME         = SM_CYDLGFRAME
        val SM_CXSIZEFRAME          = SM_CXFRAME
        val SM_CYSIZEFRAME          = SM_CYFRAME

        local
            val getSystemMetrics = call1 (user "GetSystemMetrics") (INT) INT
        in
            fun GetSystemMetrics(ABS i) = getSystemMetrics i
        end 
    end
end
end;
