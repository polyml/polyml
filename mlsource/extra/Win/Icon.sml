(*
    Copyright (c) 2001, 2015
        David C.J. Matthews

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

structure Icon:
  sig
    type HICON and HINSTANCE and HDC
    val CopyIcon : HICON -> HICON
    val DestroyIcon : HICON -> unit
    val DrawIcon : HDC * int * int * HICON -> unit
    val ExtractIcon : HINSTANCE * string * int -> HICON
    val IDI_APPLICATION : Resource.RESID
    val IDI_ASTERISK : Resource.RESID
    val IDI_ERROR : Resource.RESID
    val IDI_EXCLAMATION : Resource.RESID
    val IDI_HAND : Resource.RESID
    val IDI_INFORMATION : Resource.RESID
    val IDI_QUESTION : Resource.RESID
    val IDI_WARNING : Resource.RESID
    val IDI_WINLOGO : Resource.RESID
    val LoadIcon : HINSTANCE * Resource.RESID -> HICON
  end =
struct
    local
        open Foreign
        open Base
        open Resource
    in
        type HICON = HICON and HINSTANCE = HINSTANCE and HDC = HDC
        val isHiconNull = isHgdiObjNull

        fun checkIcon c = (checkResult(not(isHiconNull c)); c)

        val CopyIcon =
            checkIcon o winCall1 (user "CopyIcon") (cHICON) cHICON

        val DestroyIcon =
            checkResult o winCall1 (user "DestroyIcon") (cHICON) cBool
            
        val DrawIcon =
            checkResult o winCall4 (user "DrawIcon") (cHDC, cInt, cInt, cHICON) cBool

        val ExtractIcon = winCall3 (user "ExtractIcon") (cHINSTANCE, cString, cUint) cHICON

        val LoadIcon =
            checkIcon o winCall2 (user "LoadIconA") (cHINSTANCE, cRESID) cHICON

        (* Built-in icons. *)
        val IDI_APPLICATION     = Resource.IdAsInt 32512
        val IDI_ASTERISK        = Resource.IdAsInt 32516
        val IDI_EXCLAMATION     = Resource.IdAsInt 32515
        val IDI_HAND            = Resource.IdAsInt 32513
        val IDI_ERROR           = IDI_HAND
        val IDI_INFORMATION     = IDI_ASTERISK
        val IDI_QUESTION        = Resource.IdAsInt 32514
        val IDI_WARNING         = IDI_EXCLAMATION
        val IDI_WINLOGO         = Resource.IdAsInt 32517
        
(*
TODO:
CreateIcon  - complicated
CreateIconFromResource   - complicated
CreateIconFromResourceEx  
CreateIconIndirect  
DrawIconEx  
ExtractAssociatedIcon  
ExtractIconEx  
GetIconInfo  
LookupIconIdFromDirectory  
LookupIconIdFromDirectoryEx  
*)
    end
end;
