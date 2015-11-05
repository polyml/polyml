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
        | MFT_OWNERDRAW of SysWord.word
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
        open Foreign
        open Base
    in
        open MenuBase

        type HMENU = HMENU and HBITMAP = HBITMAP and RECT = RECT and HWND = HWND
        and HINSTANCE = HINSTANCE

        val isHmenuNull = isHmenuNull

        fun checkMenu c = (checkResult(not(isHmenuNull c)); c)

        (* Check here means "make active", the opposite of uncheck *)
        val CheckMenuRadioItem =
            checkResult o
            winCall5 (user "CheckMenuRadioItem") (cHMENU, cUint, cUint, cUint, cMENUFLAG) cBool

        val CreateMenu =
            checkMenu o winCall0 (user "CreateMenu") () cHMENU

        val CreatePopupMenu =
            checkMenu o winCall0 (user "CreatePopupMenu") () cHMENU

        val DeleteMenu = 
            checkResult o
            winCall3 (user "DeleteMenu") (cHMENU, cUint, cMENUFLAG) cBool

        val DestroyMenu = 
            checkResult o winCall1 (user "DestroyMenu") (cHMENU) cBool

        val DrawMenuBar = 
            checkResult o winCall1 (user "DrawMenuBar") (cHWND) cBool

        local
            val enableCall = winCall3(user "EnableMenuItem") (cHMENU, cUint, cMENUFLAG) cInt
        in
            fun EnableMenuItem(hMenu: HMENU, id: int, flags: MenuFlag): MenuFlag list =
            let
                val res = enableCall(hMenu, id, flags)
            in
                checkResult(res <> ~1);
                toMenuFlagSet res
            end
        end

        val GetMenu = winCall1 (user "GetMenu") (cHWND) cHMENU

        datatype GMDIFlags = GMDI_GOINTOPOPUPS | GMDI_USEDISABLED
        local
            val tab = [
                (GMDI_USEDISABLED, 0x0001),
                (GMDI_GOINTOPOPUPS, 0x0002) ]
        in
            val GMDIFLAGS = tableSetConversion(tab, NONE) cUint
        end

        local
            val callGMDI = winCall3 (user "GetMenuDefaultItem") (cHMENU, cBool, GMDIFLAGS) cUint
        in
            fun GetMenuDefaultItem(hMenu: HMENU, m: bool, opts: GMDIFlags list): int =
            let
                
                val res = callGMDI(hMenu, m, opts)
            in
                checkResult(res <> ~1);
                res
            end
        end

        local
            val getMenuItemCount = winCall1 (user "GetMenuItemCount") (cHMENU) cInt
        in
            fun GetMenuItemCount hMenu =
            case getMenuItemCount hMenu of
                ~1 => raiseSysErr()
            |   n => n
        end

        val GetMenuItemID = winCall1 (user "GetMenuItemID") (cHMENU) cUint
 
        local
            val getMenuString = winCall5 (user "GetMenuStringA")
                          (cHMENU,cUint,cPointer,cInt,cMENUFLAG) (cPOSINT "GetMenuString")
        in
            (* Loop until we have read the whole string. *)
            fun GetMenuString(h,i,f): string =
                getStringCall(fn (buff, n) => getMenuString(h,i,buff,n,f))
        end


        datatype MenuItemType =
            MFT_BITMAP of HBITMAP
        |   MFT_SEPARATOR
        |   MFT_STRING of string
        |   MFT_OWNERDRAW of SysWord.word

        val mft_STRING          = 0wx00000000 (* Replaced by MIIM_STRING *)
        val mft_BITMAP          = 0wx00000004 (* Replaced by MIIM_BITMAP and hbmpItem *)
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
            val cMENUSTATE = tableSetConversion(tab, NONE) cUint
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
            (* Flags used in GetItemInfo and SetItemInfo. *)
            (*val MIIM_STATE       = 0x00000001
            val MIIM_ID          = 0x00000002
            val MIIM_SUBMENU     = 0x00000004
            val MIIM_CHECKMARKS  = 0x00000008
            (*val MIIM_TYPE        = 0x00000010 *) (* Replaced by new fields. *)
            val MIIM_DATA        = 0x00000020
            val MIIM_STRING      = 0x00000040 (* Added *)
            val MIIM_BITMAP      = 0x00000080 (* Added *)
            val MIIM_FTYPE       = 0x00000100*)
            val allInfo = 0x1ef
        
            val cMENUITEMINFO =
                cStruct12(cUint,cUint,cUint,cMENUSTATE,cUint,cHMENUOPT,cHGDIOBJOPT,
                          cHGDIOBJOPT,cULONG_PTR,cPointer,cUint, cHGDIOBJ)
            val {ctype={size=sizeMenuItemStruct, ...}, ...} = breakConversion cMENUITEMINFO
            (*val (fromCmenuiteminfo, toCmenuiteminfo, menuItemStruct) = breakConversion MENUITEMINFO*)
            val getMenuItemInfo =
                winCall4 (user "GetMenuItemInfoA") (cHMENU, cUint, cBool, cStar cMENUITEMINFO)
                    (successState "GetMenuItemInfo")
            val setMenuItemInfo =
                winCall4 (user "SetMenuItemInfoA") (cHMENU, cUint, cBool, cConstStar cMENUITEMINFO)
                    (successState "SetMenuItemInfo")
            val insertMenuItem =
                winCall4 (user "InsertMenuItemA") (cHMENU, cUint, cBool, cConstStar cMENUITEMINFO)
                    (successState "InsertMenuItem")
        in
            fun GetMenuItemInfo(hMenu: HMENU, uItem: int, fByPosition): MenuItemInfo =
            let
                (* First request allInfo.  Look at the returned type and cch.  If cch is
                   non-zero allocate memory of cch+1 and pass memory pointer and cch+1 to
                   get the string. *)
                val r = ref (Word.toInt sizeMenuItemStruct, allInfo, 0, [], 0, NONE, NONE, NONE, 0, Memory.null, 0, hNull)
                val () = getMenuItemInfo(hMenu, uItem, fByPosition, r)
                val cch = #11(!r)
                val str =
                    if cch = 0 then ""
                    else
                    let
                        open Memory
                        val v = malloc (Word.fromInt cch + 0w1)
                        val () =
                            r := (Word.toInt sizeMenuItemStruct, allInfo, 0, [], 0, NONE, NONE, NONE, 0, v, cch+1, hNull)
                    in
                        (* Get the string.  Updates r *)
                        getMenuItemInfo(hMenu, uItem, fByPosition, r)
                            handle ex => (free v; raise ex);
                        fromCstring v before free v
                    end
                val (_, _, mtype, state, wID, hSubMenu, hbmpChecked, hbmpUnchecked,
                    itemData, typeData, _, hbmp) = ! r
                val mtype = LargeWord.fromInt mtype
                val menuType =
                    if LargeWord.andb(mtype, mft_BITMAP) <> 0w0
                    then MFT_BITMAP hbmp
                    else if LargeWord.andb(mtype, mft_OWNERDRAW) <> 0w0
                    then MFT_OWNERDRAW(Memory.voidStar2Sysword typeData)
                    else if LargeWord.andb(mtype, mft_SEPARATOR) <> 0w0
                    then MFT_SEPARATOR
                    else (* String *) MFT_STRING str
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
                open Memory
                val (bits, typeData, cch, bmp) =
                    case menuType of
                        MFT_BITMAP b => (mft_BITMAP, null, 0, b)
                    |   MFT_OWNERDRAW i => (mft_OWNERDRAW, sysWord2VoidStar i, 0, hNull)
                    |   MFT_SEPARATOR => (mft_SEPARATOR, null, 0, hNull)
                    |   MFT_STRING s => (mft_STRING, toCstring s, size s + 1, hNull)
                        
                val mtype = LargeWord.orb(LargeWord.fromInt (fromMFT menuOptions), bits)
                val r = (Word.toInt sizeMenuItemStruct, allInfo, LargeWord.toInt mtype, state, wID,
                            hSubMenu, hbmpChecked, hbmpUnchecked, itemData, typeData, cch, bmp)
            in
                setMenuItemInfo(hMenu, uItem, fByPosition, r)
                    handle ex => (free typeData; raise ex);
                free typeData
            end

            fun InsertMenuItem(hMenu: HMENU, uItem: int, fByPosition,
                    ({menuType, menuOptions, wID, hSubMenu, hbmpChecked, hbmpUnchecked,
                      itemData, state }: MenuItemInfo)) =
            let
                open Memory
                val (bits, typeData, cch, bmp) =
                    case menuType of
                        MFT_BITMAP b => (mft_BITMAP, null, 0, b)
                    |   MFT_OWNERDRAW i => (mft_OWNERDRAW, sysWord2VoidStar i, 0, hNull)
                    |   MFT_SEPARATOR => (mft_SEPARATOR, null, 0, hNull)
                    |   MFT_STRING s => (mft_STRING, toCstring s, size s + 1, hNull)
                        
                val mtype = LargeWord.orb(LargeWord.fromInt (fromMFT menuOptions), bits)
                val r = (Word.toInt sizeMenuItemStruct, allInfo, LargeWord.toInt mtype, state, wID,
                            hSubMenu, hbmpChecked, hbmpUnchecked, itemData, typeData, cch, bmp)
            in
                insertMenuItem(hMenu, uItem, fByPosition, r)
                    handle ex => (free typeData; raise ex);
                free typeData
            end
        end

        local
            val getMenuState = winCall3 (user "GetMenuState") (cHMENU,cUint,cMENUFLAG) cUint
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

        val GetSubMenu             = winCall2 (user "GetSubMenu") (cHMENU,cInt) cHMENU

        val GetSystemMenu          = winCall2 (user "GetSystemMenu") (cHWND,cBool) cHMENU

        val HiliteMenuItem = 
            winCall4 (user "HiliteMenuItem") (cHWND,cHMENU,cUint,cMENUFLAGSET) (successState "HiliteMenuItem")

        val IsMenu                 = winCall1 (user "IsMenu") (cHMENU) cBool

        (* InsertMenu can insert a string item or a submenu. *)
        datatype MenuIdOrHandle =
            MenuId of int
        |   MenuHandle of HMENU

        local
            open Memory
            (* Get the menu item.  If this is a string we have to free the memory afterwards. *)
            fun getDisplay (MFT_BITMAP hb)           = (mft_BITMAP, voidStarOfHandle hb, null)
             |  getDisplay MFT_SEPARATOR             = (mft_SEPARATOR, null, null)
             |  getDisplay (MFT_STRING (s: string))  = let val v = toCstring s in (mft_STRING, v, v) end
             |  getDisplay (MFT_OWNERDRAW i)  = (mft_OWNERDRAW, sysWord2VoidStar i, null)
             
            fun InsertOrModifyMenu (functionName: string) =
            let
                val docall =
                    winCall5 (user functionName) (cHMENU,cUint,cUint,cPointer,cPointer) (successState functionName)
            in
                fn(hMenu: HMENU, pos: int, flags: MenuFlag list,
                               new: MenuIdOrHandle, disp: MenuItemType) =>
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
                            MenuId i => (0w0, sysWord2VoidStar (SysWord.fromInt i))
                        |   MenuHandle m => (mft_POPUP, voidStarOfHandle m)
                    (* The "disp" argument describes how the item is displayed. *)
                    val (f3, str, toFree) = getDisplay disp
                    val flags = List.foldl LargeWord.orb 0w0 [f1,f2,f3]
                in
                    docall(hMenu, pos, LargeWord.toInt flags, id, str)
                        handle ex => (free toFree; raise ex);
                    free toFree
                end
            end
            
            val appendMenu =
                winCall4 (user "AppendMenuA") (cHMENU,cUint,cPointer,cPointer) (successState "AppendMenuA")
        in
            val InsertMenu = InsertOrModifyMenu "InsertMenuA"
            and ModifyMenu = InsertOrModifyMenu "ModifyMenuA"

            fun AppendMenu(hMenu: HMENU, flags: MenuFlag list, new: MenuIdOrHandle, disp: MenuItemType) =
            let
                val f1 = LargeWord.andb(LargeWord.fromInt(fromMenuFlagSet flags),
                            LargeWord.notb typeBits)
                val (f2, id) =
                    case new of
                        MenuId i => (0w0, sysWord2VoidStar (SysWord.fromInt i))
                    |   MenuHandle m => (mft_POPUP, voidStarOfHandle m)
                val (f3, str, toFree) = getDisplay disp
                val flags = List.foldl LargeWord.orb 0w0 [f1,f2,f3]
            in
               appendMenu (hMenu, LargeWord.toInt flags, id, str)
                        handle ex => (free toFree; raise ex);
               free toFree
            end
        end

        val RemoveMenu = winCall3(user "RemoveMenu") (cHMENU, cUint, cMENUFLAG) (successState "RemoveMenu")

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
            val TRACKPOPUPOPTIONS = tableSetConversion(tab, NONE) cUint
        end

        local
            val trackPopupMenu =
                winCall7 (user "TrackPopupMenu")
                    (cHMENU, TRACKPOPUPOPTIONS, cInt, cInt, cInt, cHWND, cPointer) cInt
        in
            fun TrackPopupMenu(menu, flags, x, y, owner) =
                trackPopupMenu(menu, flags, x, y, 0, owner, Memory.null)
        end

        local
            val getMenuItemRect =
                winCall4 (user "GetMenuItemRect") (cHWND, cHMENU, cUint, cStar cRect) (successState "GetMenuItemRect")
        in
            fun GetMenuItemRect(hWnd, hMenu, item): RECT =
            let
                val r = ref { top = 0, bottom=0, left=0, right=0}
                val () = getMenuItemRect(hWnd, hMenu, item, r)
            in
                ! r
            end
        end

        val LoadMenu = winCall2 (user "LoadMenuA") (cHINSTANCE, cRESID) cHMENU
        val SetMenu = winCall2 (user "SetMenu") (cHWND, cHMENUOPT) (successState "SetMenu")

        val SetMenuContextHelpId =
            winCall2 (user "SetMenuContextHelpId") (cHMENU, cDWORD)
                    (successState "SetWindowContextHelpId")
    
        val GetMenuContextHelpId = winCall1 (user "GetMenuContextHelpId") (cHMENU) cDWORD

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
      winCall1 (getuser "LoadMenuIndirectA")
            (POINTER) (cHMENU)
            (address menu)
    end 
end;
*)
