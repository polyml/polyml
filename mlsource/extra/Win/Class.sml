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
structure Class:
sig
    type HWND (* = Win.HWND *) and Message (* = Message.Message *)
    and HINSTANCE (* = Globals.HINSTANCE *)
    and HBRUSH (* = Brush.HBRUSH *)
    and HICON (* = Icon.HICON *)
    and HCURSOR (* = Cursor.HCURSOR *)
    and HGDIOBJ

    datatype LRESULT = LRESINT of int | LRESHANDLE of HGDIOBJ

    datatype 'a ATOM =
        Registered of
            {proc: HWND * Message * 'a -> LRESULT * 'a, className: string}
      | SystemClass of string

    val Button : unit ATOM
    val ComboBox : unit ATOM
    val ComboLBox : unit ATOM
    val DDEMLEvent : unit ATOM
    val Edit : unit ATOM
    val ListBox : unit ATOM
    val MDIClient : unit ATOM
    val ScrollBar : unit ATOM
    val Static : unit ATOM

    structure Style :
      sig
        include BIT_FLAGS

        val CS_BYTEALIGNCLIENT : flags
        val CS_BYTEALIGNWINDOW : flags
        val CS_CLASSDC : flags
        val CS_DBLCLKS : flags
        val CS_GLOBALCLASS : flags
        val CS_HREDRAW : flags
        val CS_KEYCVTWINDOW : flags
        val CS_NOCLOSE : flags
        val CS_NOKEYCVT : flags
        val CS_OWNDC : flags
        val CS_PARENTDC : flags
        val CS_SAVEBITS : flags
        val CS_VREDRAW : flags
      end

    type 'a WNDCLASSEX =
        {style: Style.flags, 
         wndProc: HWND * Message * 'a -> LRESULT * 'a,
         hInstance: HINSTANCE,
         hIcon: HICON option,
         hCursor: HCURSOR option,
         hbrBackGround: HBRUSH option,
         menuName: Resource.RESID option,
         className: string,
         hIconSm: HICON option}

    val RegisterClassEx : 'a WNDCLASSEX -> 'a ATOM

    val UnregisterClass : string * HINSTANCE -> unit
    val GetClassName : HWND -> string
    val GetClassInfoEx: HINSTANCE * string -> 'a WNDCLASSEX
  end
 =
struct
    local
        open CInterface
        open Base
        open Resource
    in
        type Message = MessageBase.Message
        type HWND = HWND and HINSTANCE = HINSTANCE and HICON = HICON
        and HBRUSH = HBRUSH and HCURSOR = HCURSOR and HGDIOBJ = HGDIOBJ
        datatype LRESULT = datatype MessageBase.LRESULT

        structure Style =
        struct
            type flags = SysWord.word
            fun toWord f = f
            fun fromWord f = f
            val flags = List.foldl (fn (a, b) => SysWord.orb(a,b)) 0w0
            fun allSet (fl1, fl2) = SysWord.andb(fl1, fl2) = fl1
            fun anySet (fl1, fl2) = SysWord.andb(fl1, fl2) <> 0w0
            fun clear (fl1, fl2) = SysWord.andb(SysWord.notb fl1, fl2)
    
            val CS_VREDRAW: flags          = 0wx0001
            val CS_HREDRAW: flags          = 0wx0002
            val CS_KEYCVTWINDOW: flags     = 0wx0004
            val CS_DBLCLKS: flags          = 0wx0008
            val CS_OWNDC: flags            = 0wx0020
            val CS_CLASSDC: flags          = 0wx0040
            val CS_PARENTDC: flags         = 0wx0080
            val CS_NOKEYCVT: flags         = 0wx0100
            val CS_NOCLOSE: flags          = 0wx0200
            val CS_SAVEBITS: flags         = 0wx0800
            val CS_BYTEALIGNCLIENT: flags  = 0wx1000
            val CS_BYTEALIGNWINDOW: flags  = 0wx2000
            val CS_GLOBALCLASS: flags      = 0wx4000
    
            val all = flags[CS_VREDRAW, CS_HREDRAW, CS_KEYCVTWINDOW, CS_DBLCLKS, CS_OWNDC,
                            CS_CLASSDC, CS_NOKEYCVT, CS_NOCLOSE, CS_SAVEBITS,
                            CS_BYTEALIGNCLIENT, CS_BYTEALIGNWINDOW, CS_GLOBALCLASS]
    
            val intersect = List.foldl (fn (a, b) => SysWord.andb(a,b)) all
        end
    
        (* Classes are either registered by the user, in which case they have
           ML callback functions, or they are built-in, such as Edit. *)
        datatype 'a ATOM =
            Registered of { proc: HWND * Message * 'a -> LRESULT * 'a, className: string }
        |   SystemClass of string

        val Button: unit ATOM = SystemClass "Button"
        and ComboBox: unit ATOM = SystemClass "ComboBox"
        and ComboLBox: unit ATOM = SystemClass "ComboLBox"
        and DDEMLEvent: unit ATOM = SystemClass "DDEMLEvent"
        and Edit: unit ATOM = SystemClass "Edit"
        and ListBox: unit ATOM = SystemClass "ListBox"
        and MDIClient: unit ATOM = SystemClass "MDIClient" (* Maybe treat this specially. *)
        and ScrollBar: unit ATOM = SystemClass "ScrollBar"
        and Static: unit ATOM = SystemClass "Static"

        type 'a WNDCLASSEX =
            {style: Style.flags, 
             wndProc: HWND * Message * 'a -> LRESULT * 'a,
             hInstance: HINSTANCE,
             hIcon: HICON option,
             hCursor: HCURSOR option,
             hbrBackGround: HBRUSH option,
             menuName: RESID option,
             className: string,
             hIconSm: HICON option}

        local
            val WNDPROC = PASCALFUNCTION4 (HWND, INT, POINTER, POINTER) POINTER
            val WNDCLASSEX = STRUCT12(INT,WORD,WNDPROC,INT,INT,HINSTANCE,HGDIOBJOPT,
                                      HGDIOBJOPT,HGDIOBJOPT,RESID,STRING,HGDIOBJOPT)
            val (fromCwndclassex, toCwndclassex, wndclassEx) = breakConversion WNDCLASSEX
            val CallWindowProc =
                call5 (user "CallWindowProcA") (WNDPROC, HWND, INT, POINTER, POINTER) POINTER
        in
            fun RegisterClassEx({style: Style.flags, 
                                wndProc: HWND * Message * 'a -> LRESULT * 'a,
                                hInstance: HINSTANCE,
                                hIcon: HICON option,
                                hCursor: HCURSOR option,
                                hbrBackGround: HBRUSH option,
                                menuName: RESID option,
                                className: string,
                                hIconSm: HICON option}: 'a WNDCLASSEX): 'a ATOM =
            let
                (* The window procedure we pass to the C call is our dispatch function
                   in the RTS. *)
                val windowProc = Message.mainCallbackFunction
                val cWndClass =
                    toCwndclassex(sizeof wndclassEx,
                        Style.toWord style,
                        windowProc,
                        0, (* Class extra *)
                        0, (* Window extra *)
                        hInstance,
                        hIcon,
                        hCursor,
                        hbrBackGround,
                        getOpt(menuName, IdAsInt 0),
                        className,
                        hIconSm)
    
                val res = call1 (user "RegisterClassExA") (POINTER) INT (address cWndClass)
                (* The result is supposed to be an atom but it doesn't always work to
                   pass this directly to CreateWindow. *)
            in
                checkResult(res <> 0);
                Registered{proc = wndProc, className = className}
            end

            fun GetClassInfoEx(hInst, class): 'a WNDCLASSEX =
            let
                val v =
                    toCwndclassex(sizeof wndclassEx, 0w0, fn _ => toCint 0, 0, 0, hNull, 
                                  NONE, NONE, NONE, IdAsInt 0, "", NONE)
                val res = call3(user "GetClassInfoExA") (HINSTANCE, STRING, POINTER)
                            (SUCCESSSTATE "GetClassInfoEx") (hInst, class, address v)
                val (_, style, wproc, _, _, hInstance, hIcon, hCursor, hbrBackGround,
                     menuName, className, hIconSm) = fromCwndclassex v
                val mName =
                    case menuName of
                        IdAsInt 0 => NONE
                    |   IdAsString "" => NONE
                    |   m => SOME m
                fun wndProc(hwnd, msg, state) =
                let
                    val (msgId: int, wParam: vol, lParam: vol) = Message.compileMessage msg
                    val res: vol = CallWindowProc(wproc, hwnd, msgId, wParam, lParam)
                in
                    (Message.messageReturnFromParams(msg, wParam, lParam, res), state)
                end
            in
                {style = Style.fromWord style, wndProc = wndProc, hInstance = hInstance,
                 hIcon = hIcon, hCursor = hCursor, hbrBackGround = hbrBackGround,
                 menuName = mName, className = className, hIconSm = hIconSm }
            end
        end

        (* The underlying call can take either a string or an atom.  I really don't
           know which is better here. *)
        val UnregisterClass =
            call2 (user "UnregisterClassA") (STRING, HINSTANCE) (SUCCESSSTATE "UnregisterClass")
    
        local
            val getClassName = call3 (user "GetClassNameA") (HWND, POINTER, INT) INT
        in
            fun GetClassName hwnd =
                getStringCall(fn (v, i) => getClassName(hwnd, v, i))
        end
(*
The following functions are used with window classes. 
GetClassInfoEx  
GetClassLong  
GetWindowLong  
SetClassLong  
SetWindowLong  

Obsolete Functions
  
GetClassInfo  
GetClassWord  
GetWindowWord  
RegisterClass  
SetClassWord  
SetWindowWord 
*)
    end
end;
