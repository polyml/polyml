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
        open CInterface
        open Base
        open Resource
    in
        type HICON = HICON and HINSTANCE = HINSTANCE and HDC = HDC
        val hiconNull = hgdiObjNull
        and isHiconNull = isHgdiObjNull

        fun checkIcon c = (checkResult(not(isHiconNull c)); c)

        val CopyIcon =
            checkIcon o call1 (user "CopyIcon") (HICON) HICON

        val DestroyIcon =
            checkResult o call1 (user "DestroyIcon") (HICON) BOOL
            
        val DrawIcon =
            checkResult o call4 (user "DrawIcon") (HDC, INT, INT, HICON) BOOL

        val ExtractIcon = call3 (user "ExtractIcon") (HINSTANCE, STRING, INT) HICON

        val LoadIcon =
            checkIcon o call2 (user "LoadIconA") (HINSTANCE, RESID) HICON

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
