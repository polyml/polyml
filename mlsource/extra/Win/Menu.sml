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

structure Menu:
  sig
    type HMENU and HBITMAP and HWND and HINSTANCE
    type RECT =  { left: int, top: int, right: int, bottom: int }
    
    datatype MenuFlag =
        MF_BYCOMMAND | MF_BYPOSITION | MF_SEPARATOR | MF_ENABLED | MF_GRAYED |
        MF_DISABLED | MF_UNCHECKED | MF_CHECKED | MF_USECHECKBITMAPS | MF_STRING |
        MF_BITMAP | MF_OWNERDRAW | MF_POPUP | MF_MENUBARBREAK | MF_MENUBREAK |
        MF_UNHILITE | MF_HILITE | MF_DEFAULT | MF_SYSMENU | MF_HELP |
        MF_RIGHTJUSTIFY | MF_MOUSESELECT

    datatype MenuIdOrHandle = MenuHandle of HMENU | MenuId of int

    datatype MenuItemOptions =
          MFT_MENUBARBREAK
        | MFT_MENUBREAK
        | MFT_RADIOCHECK
        | MFT_RIGHTJUSTIFY
        | MFT_RIGHTORDER

    datatype MenuItemType =
          MFT_BITMAP of HBITMAP
        | MFT_OWNERDRAW of int
        | MFT_SEPARATOR
        | MFT_STRING of string

    datatype MenuState =
          MFS_CHECKED
        | MFS_DEFAULT
        | MFS_DISABLED
        | MFS_ENABLED
        | MFS_GRAYED
        | MFS_HILITE
        | MFS_UNCHECKED
        | MFS_UNHILITE

    type MenuItemInfo =
        {
            menuType: MenuItemType,
            menuOptions: MenuItemOptions list,
            state: MenuState list,
            wID: int,
            hSubMenu: HMENU option,
            hbmpChecked: HBITMAP option,
            hbmpUnchecked: HBITMAP option,
            itemData: int
        }

    val AppendMenu : HMENU * MenuFlag list * MenuIdOrHandle * MenuItemType -> unit
    val CheckMenuRadioItem : HMENU * int * int * int * MenuFlag -> unit
    val CreateMenu : unit -> HMENU
    val CreatePopupMenu : unit -> HMENU
    val DeleteMenu : HMENU * int * MenuFlag -> unit
    val DestroyMenu : HMENU -> unit
    val DrawMenuBar : HWND -> unit
    val EnableMenuItem : HMENU * int * MenuFlag -> MenuFlag list
    val GetMenu : HWND -> HMENU

    datatype GMDIFlags = GMDI_GOINTOPOPUPS | GMDI_USEDISABLED
    val GetMenuDefaultItem : HMENU * bool * GMDIFlags list -> int

    val GetMenuItemCount : HMENU -> int
    val GetMenuItemID : HMENU -> int
    val GetMenuItemInfo : HMENU * int * bool -> MenuItemInfo
    val GetMenuItemRect : HWND * HMENU * int -> RECT
    val GetMenuState : HMENU * int * MenuFlag -> MenuFlag list * int
    val GetMenuString : HMENU * int * MenuFlag -> string
    val GetSubMenu : HMENU * int -> HMENU
    val GetSystemMenu : HWND * bool -> HMENU
    val HiliteMenuItem : HWND * HMENU * int * MenuFlag list -> unit
    val InsertMenu : HMENU * int * MenuFlag list * MenuIdOrHandle * MenuItemType -> unit
    val IsMenu : HMENU -> bool
    val LoadMenu : HINSTANCE * Resource.RESID -> HMENU
    val ModifyMenu : HMENU * int * MenuFlag list * MenuIdOrHandle * MenuItemType -> unit
    val RemoveMenu : HMENU * int * MenuFlag -> unit
    val SetMenu : HWND * HMENU option -> unit
    val SetMenuItemInfo : HMENU * int * bool * MenuItemInfo -> unit
    val InsertMenuItem : HMENU * int * bool * MenuItemInfo -> unit

    datatype TrackPopupMenuOptions =
        TPM_LEFTBUTTON | TPM_RIGHTBUTTON | TPM_LEFTALIGN | TPM_CENTERALIGN | TPM_RIGHTALIGN |
        TPM_TOPALIGN | TPM_VCENTERALIGN | TPM_BOTTOMALIGN | (*TPM_HORIZONTAL | TPM_VERTICAL |*)
        TPM_NONOTIFY | TPM_RETURNCMD
    val TrackPopupMenu : HMENU * TrackPopupMenuOptions list * int * int * HWND -> int

    val SetMenuContextHelpId: HMENU * int -> unit
    val GetMenuContextHelpId: HMENU -> int
  end =
struct
    local
        open CInterface
        open Base

        (* Flags used in GetItemInfo and SetItemInfo. *)
        val MIIM_STATE       = 0x00000001
        val MIIM_ID          = 0x00000002
        val MIIM_SUBMENU     = 0x00000004
        val MIIM_CHECKMARKS  = 0x00000008
        val MIIM_TYPE        = 0x00000010
        val MIIM_DATA        = 0x00000020

        val allInfo = 0x3f

    in
        open MenuBase

        type HMENU = HMENU and HBITMAP = HBITMAP and RECT = RECT and HWND = HWND
        and HINSTANCE = HINSTANCE

        val hmenuNull = hmenuNull
        and isHmenuNull = isHmenuNull

        fun checkMenu c = (checkResult(not(isHmenuNull c)); c)

        (* Check here means "make active", the opposite of uncheck *)
        val CheckMenuRadioItem =
            checkResult o
            call5 (user "CheckMenuRadioItem") (HMENU, UINT, UINT, UINT, MENUFLAG) BOOL

        val CreateMenu =
            checkMenu o call0 (user "CreateMenu") () HMENU

        val CreatePopupMenu =
            checkMenu o call0 (user "CreatePopupMenu") () HMENU

        val DeleteMenu = 
            checkResult o
            call3 (user "DeleteMenu") (HMENU, UINT, MENUFLAG) BOOL

        val DestroyMenu = 
            checkResult o call1 (user "DestroyMenu") (HMENU) BOOL

        val DrawMenuBar = 
            checkResult o call1 (user "DrawMenuBar") (HWND) BOOL

        local
            val enableCall = call3(user "EnableMenuItem") (HMENU, INT, MENUFLAG) INT
        in
            fun EnableMenuItem(hMenu: HMENU, id: int, flags: MenuFlag): MenuFlag list =
            let
                val res = enableCall(hMenu, id, flags)
            in
                checkResult(res <> ~1);
                toMenuFlagSet res
            end
        end

        val GetMenu = call1 (user "GetMenu") (HWND) HMENU

        datatype GMDIFlags = GMDI_GOINTOPOPUPS | GMDI_USEDISABLED
        local
            val tab = [
                (GMDI_USEDISABLED, 0x0001),
                (GMDI_GOINTOPOPUPS, 0x0002) ]
        in
            val GMDIFLAGS = tableSetConversion(tab, NONE)
        end

        local
            val callGMDI = call3 (user "GetMenuDefaultItem") (HMENU, BOOL, GMDIFLAGS) UINT
        in
            fun GetMenuDefaultItem(hMenu: HMENU, m: bool, opts: GMDIFlags list): int =
            let
                
                val res = callGMDI(hMenu, m, opts)
            in
                checkResult(res <> ~1);
                res
            end
        end

        fun GetMenuItemCount hMenu =
        case call1 (user "GetMenuItemCount") (HMENU) INT (hMenu) of
            ~1 => raiseSysErr()
        |   n => n

        val GetMenuItemID = call1 (user "GetMenuItemID") (HMENU) INT
 
        local
            val getMenuString = call5 (user "GetMenuStringA")
                          (HMENU,INT,POINTER,INT,MENUFLAG) (POSINT "GetMenuString")
        in
            (* Loop until we have read the whole string. *)
            fun GetMenuString(h,i,f): string =
                getStringCall(fn (buff, n) => getMenuString(h,i,buff,n,f))
        end


        datatype MenuItemType =
            MFT_BITMAP of HBITMAP
        |   MFT_SEPARATOR
        |   MFT_STRING of string
        |   MFT_OWNERDRAW of int

        val mft_STRING          = 0wx00000000
        val mft_BITMAP          = 0wx00000004
        val mft_OWNERDRAW       = 0wx00000100
        val mft_SEPARATOR       = 0wx00000800
        val mft_POPUP           = 0wx00000010
        val typeBits = 0wx914

        datatype MenuItemOptions =
            MFT_MENUBARBREAK
        |   MFT_MENUBREAK
        |   MFT_RADIOCHECK
        |   MFT_RIGHTJUSTIFY
        |   MFT_RIGHTORDER

        local
            val tab = [
                (MFT_MENUBARBREAK, 0x00000020),
                (MFT_MENUBREAK,    0x00000040),
                (MFT_RADIOCHECK,   0x00000200),
                (MFT_RIGHTORDER,   0x00002000),
                (MFT_RIGHTJUSTIFY, 0x00004000)]
        in
            val (fromMFT, toMFT) = tableSetLookup(tab, NONE)
        end

        datatype MenuState =
            MFS_GRAYED
        |   MFS_DISABLED
        |   MFS_CHECKED
        |   MFS_DEFAULT
        |   MFS_HILITE
        |   MFS_ENABLED
        |   MFS_UNCHECKED
        |   MFS_UNHILITE

        local
            val tab = [
                (MFS_DISABLED,  0x00000002),
                (MFS_ENABLED,   0x00000000),
                (MFS_GRAYED,    0x00000003),
                (MFS_CHECKED,   0x00000008),
                (MFS_UNCHECKED, 0x00000000),
                (MFS_HILITE,    0x00000080),
                (MFS_UNHILITE,  0x00000000),
                (MFS_DEFAULT,   0x00001000)]
        in
            val MENUSTATE = tableSetConversion(tab, NONE)
        end
        
        type MenuItemInfo =
            {
                (*mask: int,*) (* Datatype? *)
                menuType: MenuItemType,
                menuOptions: MenuItemOptions list,
                state: MenuState list,
                wID: int,
                hSubMenu: HMENU option,
                hbmpChecked: HBITMAP option,
                hbmpUnchecked: HBITMAP option,
                itemData: int
            }

        (* Although we can selectively return information it's probably simpler to
           return the lot.  It's only in SetMenuItemInfo where we might want to
           update only some of the information.
           To find out if we've got all the string we will have to loop until
           the value of cch we get back is less than the buffer we passed. *)
        local
            val MENUITEMINFO =
                STRUCT11(INT,INT,WORD,MENUSTATE,INT,HMENUOPT,HGDIOBJOPT,HGDIOBJOPT,INT,POINTER,INT)
            val (fromCmenuiteminfo, toCmenuiteminfo, menuItemStruct) = breakConversion MENUITEMINFO
            val getMenuItemInfo =
                call4 (user "GetMenuItemInfoA") (HMENU, UINT, BOOL, POINTER)
                    (SUCCESSSTATE "GetMenuItemInfo")
            val setMenuItemInfo =
                call4 (user "SetMenuItemInfoA") (HMENU, UINT, BOOL, POINTER)
                    (SUCCESSSTATE "SetMenuItemInfo")
            val insertMenuItem =
                call4 (user "InsertMenuItemA") (HMENU, UINT, BOOL, POINTER)
                    (SUCCESSSTATE "InsertMenuItem")
        in
            fun GetMenuItemInfo(hMenu: HMENU, uItem: int, fByPosition): MenuItemInfo =
            let
                (* We need to set the size and the bits for the information we want.
                   In order to be able to retrieve the "Type" information we have to
                   be prepared to accept a string.  To do that the dwTypeData must
                   point to a small vector.  To simplify matters we don't try to get
                   the whole string because we can't guess the size.  Instead we use
                   GetMenuString. Everything else can be zero. *)
                val v = alloc 4 Cchar
                val r = toCmenuiteminfo(sizeof menuItemStruct, allInfo, 0w0, [], 0, NONE, NONE,
                            NONE, 0, address v, 4);
                val res = getMenuItemInfo(hMenu, uItem, fByPosition, address r)
                val (_, _, mtype, state, wID, hSubMenu, hbmpChecked, hbmpUnchecked,
                    itemData, typeData, cch) = fromCmenuiteminfo r
                val menuType =
                    if LargeWord.andb(mtype, mft_BITMAP) <> 0w0
                    then MFT_BITMAP(handleOfInt(LOWORD(fromCuint typeData)))
                    else if LargeWord.andb(mtype, mft_OWNERDRAW) <> 0w0
                    then MFT_OWNERDRAW(fromCint typeData)
                    else if LargeWord.andb(mtype, mft_SEPARATOR) <> 0w0
                    then MFT_SEPARATOR
                    else (* String *) MFT_STRING(GetMenuString(hMenu, uItem,
                                        if fByPosition then MF_BYPOSITION else MF_BYCOMMAND))
                (* The options are the other bits in the type field. *)
                val menuOptions =
                    toMFT(LargeWord.toInt(LargeWord.andb(LargeWord.notb typeBits, mtype)))
            in
                { menuType = menuType, menuOptions = menuOptions, wID = wID,
                  hSubMenu = hSubMenu, hbmpChecked = hbmpChecked,
                  hbmpUnchecked = hbmpUnchecked, itemData = itemData,
                  state = state }
            end

            (* It's simplest to set everything. *)
            fun SetMenuItemInfo(hMenu: HMENU, uItem: int, fByPosition,
                    ({menuType, menuOptions, wID, hSubMenu, hbmpChecked, hbmpUnchecked,
                      itemData, state }: MenuItemInfo)) =
            let
                val (bits, typeData, cch) =
                    case menuType of
                        MFT_BITMAP b => (mft_BITMAP, toCint(intOfHandle b), 0)
                    |   MFT_OWNERDRAW i => (mft_OWNERDRAW, toCint i, 0)
                    |   MFT_SEPARATOR => (mft_SEPARATOR, toCint 0, 0)
                    |   MFT_STRING s => (mft_STRING, toCstring s, size s + 1)
                        
                val mtype = LargeWord.orb(LargeWord.fromInt (fromMFT menuOptions), bits)
                val r = toCmenuiteminfo(sizeof menuItemStruct, allInfo, mtype, state, wID,
                            hSubMenu, hbmpChecked, hbmpUnchecked, itemData, typeData, cch);
            in
                setMenuItemInfo(hMenu, uItem, fByPosition, address r)
            end

            fun InsertMenuItem(hMenu: HMENU, uItem: int, fByPosition,
                    ({menuType, menuOptions, wID, hSubMenu, hbmpChecked, hbmpUnchecked,
                      itemData, state }: MenuItemInfo)) =
            let
                val (bits, typeData, cch) =
                    case menuType of
                        MFT_BITMAP b => (mft_BITMAP, toCint(intOfHandle b), 0)
                    |   MFT_OWNERDRAW i => (mft_OWNERDRAW, toCint i, 0)
                    |   MFT_SEPARATOR => (mft_SEPARATOR, toCint 0, 0)
                    |   MFT_STRING s => (mft_STRING, toCstring s, size s + 1)
                        
                val mtype = LargeWord.orb(LargeWord.fromInt (fromMFT menuOptions), bits)
                val r = toCmenuiteminfo(sizeof menuItemStruct, allInfo, mtype, state, wID,
                            hSubMenu, hbmpChecked, hbmpUnchecked, itemData, typeData, cch);
            in
                insertMenuItem(hMenu, uItem, fByPosition, address r)
            end
        end

        local
            val getMenuState = call3 (user "GetMenuState") (HMENU,INT,MENUFLAG) UINT
        in
            (* If the menu opens a submenu the high order word is the number of
               items.  The low order word is the state. *)
            fun GetMenuState (hm, i, mf): MenuFlag list * int =
            let
                val res = getMenuState(hm, i, mf)
            in
                checkResult(res >= 0);
                (toMenuFlagSet(LOWORD res), HIWORD res)
            end
        end

        val GetSubMenu             = call2 (user "GetSubMenu") (HMENU,INT) HMENU

        val GetSystemMenu          = call2 (user "GetSystemMenu") (HWND,BOOL) HMENU

        val HiliteMenuItem = 
            call4 (user "HiliteMenuItem") (HWND,HMENU,INT,MENUFLAGSET) (SUCCESSSTATE "HiliteMenuItem")

        val IsMenu                 = call1 (user "IsMenu") (HMENU) BOOL

        (* InsertMenu can insert a string item or a submenu. *)
        datatype MenuIdOrHandle =
            MenuId of int
        |   MenuHandle of HMENU

        local
            fun getDisplay (MFT_BITMAP hb)           = (mft_BITMAP, toCint(hgdiAsInt hb))
             |  getDisplay MFT_SEPARATOR             = (mft_SEPARATOR, toCint 0)
             |  getDisplay (MFT_STRING (s: string))  = (mft_STRING, toCstring s)
             |  getDisplay (MFT_OWNERDRAW (i: int))  = (mft_OWNERDRAW, toCint i)

            fun InsertOrModifyMenu (functionName: string)
                (hMenu: HMENU, pos: int, flags: MenuFlag list,
                           new: MenuIdOrHandle, disp: MenuItemType) =
            let
                (* Flags - mask out the ones we set by other means. *)
                val f1 = LargeWord.andb(LargeWord.fromInt(fromMenuFlagSet flags),
                            LargeWord.notb typeBits)
                (* The C call incorporates various options within the flags.  It's better,
                   in ML, to pull these out and treat them as part of the datatype. *)
                (* The "new" argument indicates whether the item is a sub-menu or
                   should send a message containing the id when the menu item is
                   selected. *)
                val (f2, id) =
                    case new of
                        MenuId i => (0w0, i)
                    |   MenuHandle m => (mft_POPUP, intOfHandle m)
                (* The "disp" argument describes how the item is displayed. *)
                val (f3, str) = getDisplay disp
                val flags = List.foldl LargeWord.orb 0w0 [f1,f2,f3]
            in
                call5 (user functionName) (HMENU,INT,WORD,INT,POINTER) (SUCCESSSTATE functionName)
                        (hMenu, pos, flags, id, str)
            end
        in
            val InsertMenu = InsertOrModifyMenu "InsertMenuA"
            and ModifyMenu = InsertOrModifyMenu "ModifyMenuA"

            fun AppendMenu(hMenu: HMENU, flags: MenuFlag list, new: MenuIdOrHandle, disp: MenuItemType) =
            let
                val f1 = LargeWord.andb(LargeWord.fromInt(fromMenuFlagSet flags),
                            LargeWord.notb typeBits)
                val (f2, id) =
                    case new of
                        MenuId i => (0wx00000000, i)
                    |   MenuHandle m => (mft_POPUP, intOfHandle m)
                val (f3, str) = getDisplay disp
                val flags = List.foldl LargeWord.orb 0w0 [f1,f2,f3]
            in
                call4 (user "AppendMenuA") (HMENU,WORD,INT,POINTER) (SUCCESSSTATE "AppendMenuA")
                        (hMenu, flags, id, str)
            end
        end

        val RemoveMenu = call3(user "RemoveMenu") (HMENU, INT, MENUFLAG) (SUCCESSSTATE "RemoveMenu")

        datatype TrackPopupMenuOptions =
            TPM_LEFTBUTTON | TPM_RIGHTBUTTON | TPM_LEFTALIGN | TPM_CENTERALIGN | TPM_RIGHTALIGN |
            TPM_TOPALIGN | TPM_VCENTERALIGN | TPM_BOTTOMALIGN | (*TPM_HORIZONTAL | TPM_VERTICAL |*)
            TPM_NONOTIFY | TPM_RETURNCMD

        local
            val tab = [
                (TPM_LEFTBUTTON, 0x0000),
                (TPM_RIGHTBUTTON, 0x0002),
                (TPM_LEFTALIGN, 0x0000),
                (TPM_CENTERALIGN, 0x0004),
                (TPM_RIGHTALIGN, 0x0008),
                (TPM_TOPALIGN, 0x0000),
                (TPM_VCENTERALIGN, 0x0010),
                (TPM_BOTTOMALIGN, 0x0020),
                (*(TPM_HORIZONTAL, 0x0000),
                (TPM_VERTICAL, 0x0040),*)
                (TPM_NONOTIFY, 0x0080),
                (TPM_RETURNCMD, 0x0100)
                ]
        in
            val TRACKPOPUPOPTIONS = tableSetConversion(tab, NONE)
        end

        local
            val trackPopupMenu =
                call7 (user "TrackPopupMenu")
                    (HMENU, TRACKPOPUPOPTIONS, INT, INT, INT, HWND, POINTER) INT
        in
            fun TrackPopupMenu(menu, flags, x, y, owner) =
                trackPopupMenu(menu, flags, x, y, 0, owner, toCint 0)
        end

        local
            val (fromR,toR,ctypeR) = breakConversion RECT
        in
            fun GetMenuItemRect(hWnd, hMenu, item): RECT =
            let
                val (fromR,toR,ctypeR) = breakConversion RECT
                val r = alloc 1 ctypeR
                val res = call4 (user "GetMenuItemRect") (HWND, HMENU, UINT, POINTER)
                            (SUCCESSSTATE "GetMenuItemRect") (hWnd, hMenu, item, address r)
            in
                fromR r
            end
        end

        val LoadMenu = call2 (user "LoadMenuA") (HINSTANCE, RESID) HMENU
        val SetMenu = call2 (user "SetMenu") (HWND, HMENUOPT) (SUCCESSSTATE "SetMenu")

        val SetMenuContextHelpId =
            call2 (user "SetMenuContextHelpId") (HMENU, INT)
                    (SUCCESSSTATE "SetWindowContextHelpId")
    
        val GetMenuContextHelpId = call1 (user "GetMenuContextHelpId") (HMENU) INT

        (* *)
        (*fun LoadMenuIndirect (mlist: (MenuFlag list * int * string) list list) =
        let
            val count = List.length mlist
            val menu = *)
(*
TODO: 
GetMenuCheckMarkDimensions  - use GetSystemMetrics   
LoadMenuIndirect  
MenuItemFromPoint   
SetMenuDefaultItem   
SetMenuItemBitmaps   
TrackPopupMenuEx   

Obsolete Functions
CheckMenuItem  
GetMenuCheckMarkDimensions  
ModifyMenu  
*)
    end
end;
(*
struct

    datatype MenuItemData = MID of {option: Style.flag, id:MenuItem, display:string}

    (* I don't think this will work.  The strings have to be Unicode. *)
    fun LoadMenuIndirect (mlist) =
    let val count = List.length mlist
    
        val menu = alloc count (Cstruct [Cshort,Cshort,Cpointer Cchar])
    
        fun pl2a v n [] = () 
        |   pl2a v n (MID {option=flag,
                           id= MenuID id,
                           display=s} :: rest) = 
        let
           val item = make_struct [(Cshort,toCshort (repE MenuFlagE flag)),
                                   (Cshort,toCshort id ),
                                   (Cpointer Cchar,toCstring s) ] 
        in
          ( assign  (Cstruct [Cshort,Cshort,Cpointer Cchar]) 
          (offset n (Cstruct [Cshort,Cshort,Cpointer Cchar]) v) item ;
            pl2a v (n+1) rest ) 
        end
    
        val u = pl2a menu 0 mlist
    
    in
      call1 (getuser "LoadMenuIndirectA")
            (POINTER) (HMENU)
            (address menu)
    end 
end;
*)
