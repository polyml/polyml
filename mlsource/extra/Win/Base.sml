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
(* This contains various types and other values which are needed in various
   modules.  All the exported types are contained in other structures. *)
structure Base =
struct
local
    open CInterface
    val System_isShort : vol -> bool =
        RunCall.run_call1 RuntimeCalls.POLY_SYS_is_short
in

    fun absConversion {abs,rep} C = 
    let val (fromC,toC,Ctype) = breakConversion C   
    in mkConversion (abs o fromC) (toC o rep) (Ctype)
    end

    (* In many cases we can pass a set of options as a bit set. *)
    fun bitsetConversion {abs, rep} =
    let
        val (fromC, toC, Ctype) = breakConversion INT
        val fromList = List.foldl (fn(i, n) => IntInf.orb(rep i, n)) 0
        fun toList n = [abs n] (* This is a bit of a mess. *)
    in
        mkConversion (toList o fromCuint) (toCuint o fromList) Cuint
    end

    (* Whenever we call a foreign function we remember the value of GetLastError.
       We have to do that because we may have changed to a different ML process in
       the meantime and made another system call. *)
    fun GetLastError(): OS.syserror =
        RunCall.run_call2 RuntimeCalls.POLY_SYS_os_specific (1100, ())

    (* The string argument of the SysErr exception is supposed to match the result of OS.errMsg. *)
    fun raiseSysErr () = let val err = GetLastError() in raise OS.SysErr(OS.errorMsg err, SOME err) end

    (* Many system calls return bool.  If the result is false we raise an exception. *)
    fun checkResult true = () | checkResult false = raiseSysErr ()

    (* SUCCESSSTATE returns a conversion which checks the result is true.
       It cannot be used for an argument, only a result. *)
    local
        val (fromCbool, _, bool) = breakConversion BOOL
    in
    fun SUCCESSSTATE name =
        mkConversion (checkResult o fromCbool)
            (fn () => raise OS.SysErr("SUCCESSSTATE", NONE)) bool
    end

    type POINT = { x: int, y: int }

    local
        fun breakPoint ({x,y}: POINT) = (x,y)
        fun mkPoint (x,y): POINT = {x=x, y=y}
    in
        val POINT = absConversion {abs=mkPoint, rep=breakPoint} (STRUCT2 (LONG,LONG))
    end

    type RECT =  { left: int, top: int, right: int, bottom: int }

    local
        fun breakRect ({left,top,right,bottom}: RECT) = (left,top,right,bottom)
        fun mkRect (left,top,right,bottom): RECT =
            {left=left,top=top,right=right,bottom=bottom}
    in
        val RECT = absConversion {abs=mkRect, rep=breakRect} (STRUCT4 (LONG,LONG,LONG,LONG))
    end

    type SIZE = { cx: int, cy: int }
    local
        fun breakSize ({cx,cy}: SIZE) = (cx,cy)
        fun mkSize (cx,cy): SIZE = {cx=cx, cy=cy}
    in
        val SIZE = absConversion {abs=mkSize, rep=breakSize} (STRUCT2 (LONG,LONG))
    end

    (* Handles are generally opaque values. *)
    abstype 'a HANDLE = Hand of int
    with
        val hNull = Hand 0
        fun isHNull(Hand 0) = true | isHNull _ = false
        (* We sometimes need the next two functions internally.
           They're needed externally unless we change the result type
           of SendMessage to allow us to return a handle for certain
           messages. *)
        val handleOfInt = Hand
        fun intOfHandle(Hand n) = n
    end

    (* We just need these as placeholders. We never create values of
       these types.  They are used simply as a way of creating different
       handle types. *)
    abstype GdiObj = GdiObj
    and Instance = Instance
    and Drop = Drop
    and DeviceContext = DeviceContext
    and Menu = Menu
    and Window = Window
    with
    end


    (* HINSTANCE is used as an instance of a module. *)
    type HINSTANCE = Instance HANDLE
    and  HDROP = Drop HANDLE
    and  HGDIOBJ = GdiObj HANDLE
    and  HDC = DeviceContext HANDLE
    and  HMENU = Menu HANDLE
    and  HWND = Window HANDLE

    local
        (* We need to use INT here rather than UINT to maintain
           compatibility with ApplicationInstance. *)
        fun HANDLE() =
            absConversion {abs=handleOfInt, rep=intOfHandle} INT
        fun hoptOfInt 0 = NONE
         |  hoptOfInt i = SOME(handleOfInt i)
        
        fun HANDLEOPT() =
            absConversion {abs=hoptOfInt, rep=fn v => intOfHandle(getOpt(v, hNull)) } INT
    in
        val HGDIOBJ:   HGDIOBJ Conversion = HANDLE()
        and HDROP:     HDROP Conversion = HANDLE()
        and HMENU:     HMENU Conversion = HANDLE()
        and HINSTANCE: HINSTANCE Conversion = HANDLE()
        and HDC:       HDC Conversion = HANDLE()
        and HWND:      HWND Conversion = HANDLE()

        val HMENUOPT:  HMENU option Conversion = HANDLEOPT()
        and HGDIOBJOPT: HGDIOBJ option Conversion = HANDLEOPT()
        and HWNDOPT: HWND option Conversion = HANDLEOPT()
    end

    (* Temporary declarations. *)
    val hgdiObjNull:HGDIOBJ  = hNull
    and isHgdiObjNull: HGDIOBJ -> bool = isHNull
    and hgdiAsInt: HGDIOBJ -> int = intOfHandle
    and intAsHgdi: int -> HGDIOBJ = handleOfInt
    and hdcNull: HDC = hNull
    and isHdcNull: HDC -> bool = isHNull
    and hmenuNull: HMENU = hNull
    and isHmenuNull: HMENU -> bool = isHNull
    and hinstanceNull: HINSTANCE = hNull
    and isHinstanceNull: HINSTANCE -> bool = isHNull
    and hwndNull: HWND = hNull

    (* All these are various kinds of HGDIOBJ.  It's too complicated to try
       to use different types for them. *)
    type HPALETTE = HGDIOBJ and HFONT = HGDIOBJ and HPEN = HGDIOBJ
    and HBITMAP = HGDIOBJ and HRGN = HGDIOBJ and HBRUSH = HGDIOBJ
    and HENHMETAFILE = HGDIOBJ and HMETAFILE = HGDIOBJ

    val HPALETTE: HPALETTE Conversion = HGDIOBJ
    and HFONT: HFONT Conversion = HGDIOBJ
    and HPEN: HPEN Conversion = HGDIOBJ
    and HBITMAP: HBITMAP Conversion = HGDIOBJ
    and HRGN: HRGN Conversion = HGDIOBJ
    and HBRUSH: HBRUSH Conversion = HGDIOBJ
    and HENHMETAFILE: HENHMETAFILE Conversion = HGDIOBJ
    and HMETAFILE: HMETAFILE Conversion = HGDIOBJ

    (* I'm not so happy about treating these as HGDIOBJ but it makes the
       types of messages such as BM_SETIMAGE simpler. *)
    type HICON = HGDIOBJ and HCURSOR = HGDIOBJ
    val HICON = HGDIOBJ and HCURSOR = HGDIOBJ

    (* The easiest way to deal with datatypes is often by way of a table. *)
    fun tableLookup (table: (''a * int) list, default) =
    let
        fun toInt [] x =
            (case default of NONE => raise Fail "tableLookup: not found" | SOME (_, d) => d x)
         |  toInt ((y, i) :: tl) x = if x = y then i else toInt tl x

        fun fromInt [] x =
            (case default of
                NONE => raise Fail ("tableLookup: not found" ^ Int.toString x)
             |  SOME (d, _) => d x)
         |  fromInt ((y, i) :: tl) x = if x = i then y else fromInt tl x
    in
        (toInt table, fromInt table)
    end

    fun tableConversion (table: (''a * int) list, default): ''a Conversion  =
    let
        val (toInt, fromInt) = tableLookup(table, default)
    in
        absConversion {abs = fromInt, rep = toInt} INT
    end

    (* In other cases we have sets of options.  We represent them by a list.
       The order of the elements in the table is significant if we are to be
       able to handle multiple bits.  Patterns with more than one bit set
       MUST be placed later than those with a subset of those bits. *)
    fun tableSetLookup (table: (''a * int) list, default) =
    let
        (* Conversion to integer - just fold the values. *)
        fun toInt' [] x =
            (case default of NONE => raise Fail "tableLookup: not found" | SOME (_, d) => d x)
         |  toInt' ((y, i) :: tl) x = if x = y then i else toInt' tl x

        val toInt = List.foldl (fn (a, b) => IntInf.orb(toInt' table a, b)) 0

        (* It would speed up the searches if we ordered the list so that multiple
           bit entries preceded those with fewer bits but it's much easier to lay
           out the tables if we do it this way. *)
        fun fromInt _ _ 0 = [] (* Zero is an empty list. *)

         |  fromInt [] NONE x = (* Not found *)
                (case default of
                    NONE => raise Fail ("tableLookup: not found" ^ Int.toString x)
                  | SOME (d, _) => [d x])

         |  fromInt [] (SOME(res, bits)) x = (* Found something - remove it from the set. *)
                (res :: fromInt table NONE (IntInf.andb(x, IntInf.notb bits)))

         |  fromInt ((res, bits)::tl) sofar x =
                if bits <> 0 andalso IntInf.andb(x, bits) = bits
                then (* Matches *) fromInt tl (SOME(res, bits)) x
                else (* Doesn't match *) fromInt tl sofar x
    in
        (toInt, fromInt table NONE)
    end

    fun tableSetConversion (table: (''a * int) list, default): ''a list Conversion  =
    let
        val (toInt, fromInt) = tableSetLookup(table, default)
    in
        absConversion {abs = fromInt, rep = toInt} INT
    end

    
    structure FindReplaceFlags:>
      sig
        include BIT_FLAGS
        val FR_DIALOGTERM : flags
        val FR_DOWN : flags
        val FR_FINDNEXT : flags
        val FR_HIDEMATCHCASE : flags
        val FR_HIDEUPDOWN : flags
        val FR_HIDEWHOLEWORD : flags
        val FR_MATCHCASE : flags
        val FR_NOMATCHCASE : flags
        val FR_NOUPDOWN : flags
        val FR_NOWHOLEWORD : flags
        val FR_REPLACE : flags
        val FR_REPLACEALL : flags
        val FR_SHOWHELP : flags
        val FR_WHOLEWORD : flags
      end =
    struct
        type flags = SysWord.word
        fun toWord f = f
        fun fromWord f = f
        val flags = List.foldl (fn (a, b) => SysWord.orb(a,b)) 0w0
        fun allSet (fl1, fl2) = SysWord.andb(fl1, fl2) = fl1
        fun anySet (fl1, fl2) = SysWord.andb(fl1, fl2) <> 0w0
        fun clear (fl1, fl2) = SysWord.andb(SysWord.notb fl1, fl2)

        val FR_DOWN                       = 0wx00000001
        val FR_WHOLEWORD                  = 0wx00000002
        val FR_MATCHCASE                  = 0wx00000004
        val FR_FINDNEXT                   = 0wx00000008
        val FR_REPLACE                    = 0wx00000010
        val FR_REPLACEALL                 = 0wx00000020
        val FR_DIALOGTERM                 = 0wx00000040
        val FR_SHOWHELP                   = 0wx00000080
        val FR_NOUPDOWN                   = 0wx00000400
        val FR_NOMATCHCASE                = 0wx00000800
        val FR_NOWHOLEWORD                = 0wx00001000
        val FR_HIDEUPDOWN                 = 0wx00004000
        val FR_HIDEMATCHCASE              = 0wx00008000
        val FR_HIDEWHOLEWORD              = 0wx00010000

        val all = flags[FR_DOWN, FR_WHOLEWORD, FR_MATCHCASE, FR_FINDNEXT, FR_REPLACE,
                        FR_REPLACEALL, FR_DIALOGTERM, FR_NOUPDOWN, FR_NOMATCHCASE,
                        FR_NOWHOLEWORD, FR_HIDEUPDOWN, FR_HIDEMATCHCASE, FR_HIDEWHOLEWORD]

        val intersect = List.foldl (fn (a, b) => SysWord.andb(a,b)) all
    end

    (* The class "string" may be a name or an atom. *)
    datatype ClassType = NamedClass of string | ClassAtom of int

    local
        fun class2Vol (ClassAtom i) =
                if i >= 0 andalso i < 0xC000 then toCint i
                else raise Fail "atom out of range"
        |   class2Vol (NamedClass s) = toCstring s

        fun vol2Class v =
        let
            val v' = fromCint v
        in
            if System_isShort v then ClassAtom v' else NamedClass(fromCstring v)
        end
    in
        val CLASS = mkConversion vol2Class class2Vol voidStar
    end

    (* Clipboard formats.  I've added CF_NONE, CF_PRIVATE, CF_GDIOBJ and CF_REGISTERED *)
    datatype ClipboardFormat =
        CF_NONE | CF_TEXT | CF_BITMAP | CF_METAFILEPICT | CF_SYLK | CF_DIF | CF_TIFF |
        CF_OEMTEXT | CF_DIB | CF_PALETTE | CF_PENDATA | CF_RIFF | CF_WAVE | CF_UNICODETEXT |
        CF_ENHMETAFILE | CF_OWNERDISPLAY | CF_DSPTEXT | CF_DSPBITMAP | CF_DSPMETAFILEPICT |
        CF_DSPENHMETAFILE | CF_PRIVATE of int | CF_GDIOBJ of int | CF_REGISTERED of int |
        CF_HDROP | CF_LOCALE

        local
            val tab = [
                (CF_NONE,                  0),
                (CF_TEXT,                  1),
                (CF_BITMAP,                2),
                (CF_METAFILEPICT,          3),
                (CF_SYLK,                  4),
                (CF_DIF,                   5),
                (CF_TIFF,                  6),
                (CF_OEMTEXT,               7),
                (CF_DIB,                   8),
                (CF_PALETTE,               9),
                (CF_PENDATA,               10),
                (CF_RIFF,                  11),
                (CF_WAVE,                  12),
                (CF_UNICODETEXT,           13),
                (CF_ENHMETAFILE,           14),
                (CF_HDROP,                 15),
                (CF_LOCALE,                16),
                (CF_OWNERDISPLAY,          0x0080),
                (CF_DSPTEXT,               0x0081),
                (CF_DSPBITMAP,             0x0082),
                (CF_DSPMETAFILEPICT,       0x0083),
                (CF_DSPENHMETAFILE,        0x008E)
                ]
            fun toInt (CF_PRIVATE i) =
                    if i >= 0 andalso i < 0xff then 0x0200 + i else raise Size
            |   toInt (CF_GDIOBJ i) =
                    if i >= 0 andalso i < 0xff then 0x0300 + i else raise Size
            |   toInt (CF_REGISTERED i) = i
            |   toInt _ = raise Match

            fun fromInt i =
                if i >= 0x0200 andalso i <= 0x02ff then CF_PRIVATE(i-0x0200)
                else if i >= 0x0300 andalso i <= 0x03ff then CF_GDIOBJ(i-0x0300)
                else if i >= 0xC000 andalso i < 0xFFFF then CF_REGISTERED i
                else raise Match
        in
            val CLIPFORMAT = tableConversion(tab, SOME(fromInt, toInt))
        end

    (* Resources may be specified by strings or by ints.  Should this be an abstype? *)
    datatype RESID = IdAsInt of int | IdAsString of string

    local
        fun resid2Vol (IdAsInt i) =
                if i >= 0 andalso i < 65536 then toCint i
                else raise Fail "resource id out of range"
        |   resid2Vol (IdAsString s) = toCstring s

        fun vol2Resid v =
        let
            val v' = fromCint v
        in
            if System_isShort v then IdAsInt v' else IdAsString(fromCstring v)
        end
    in
        val RESID = mkConversion vol2Resid resid2Vol voidStar;
    end

    (*datatype HelpContext =
        HelpInfo_MenuItem of
    |   HelpInfo_Window of

    type HELPINFO = {
    }*)


    (* Useful conversions. *)

    (* It would be nice if these were included in CInterface but that only goes as
       far as STRUCT9. *)
    fun STRUCT10 (c1,c2,c3,c4,c5,c6,c7,c8,c9,c10) =
        let
        fun break10 ts v =
            case break_struct ts v of
                [a,b,c,d,e,f,g,h,i,j] => (a,b,c,d,e,f,g,h,i,j)
            | _ => raise Fail "break"
        val (from1,to1,ctype1) = breakConversion c1
        val (from2,to2,ctype2) = breakConversion c2
        val (from3,to3,ctype3) = breakConversion c3
        val (from4,to4,ctype4) = breakConversion c4
        val (from5,to5,ctype5) = breakConversion c5
        val (from6,to6,ctype6) = breakConversion c6
        val (from7,to7,ctype7) = breakConversion c7
        val (from8,to8,ctype8) = breakConversion c8
        val (from9,to9,ctype9) = breakConversion c9
        val (from10,to10,ctype10) = breakConversion c10
    
        val ctypes = [ctype1,ctype2,ctype3,ctype4,ctype5,ctype6,ctype7,ctype8,ctype9,ctype10]
            
        fun from v =
            let val (v1,v2,v3,v4,v5,v6,v7,v8,v9,v10) = break10 ctypes v
            in (from1 v1,from2 v2,from3 v3,from4 v4,from5 v5,from6 v6,
                from7 v7,from8 v8,from9 v9,from10 v10)
            end
    
        fun to (a,b,c,d,e,f,g,h,i,j) =
            make_struct (ListPair.zip (ctypes, [to1 a,to2 b,to3 c,to4 d,to5 e,
                                                to6 f,to7 g,to8 h,to9 i,to10 j]))
        in
        mkConversion from to (Cstruct ctypes)
        end

    fun STRUCT11 (c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11) =
        let
        fun break11 ts v =
            case break_struct ts v of
                [a,b,c,d,e,f,g,h,i,j,k] => (a,b,c,d,e,f,g,h,i,j,k)
            | _ => raise Fail "break"
        val (from1,to1,ctype1) = breakConversion c1
        val (from2,to2,ctype2) = breakConversion c2
        val (from3,to3,ctype3) = breakConversion c3
        val (from4,to4,ctype4) = breakConversion c4
        val (from5,to5,ctype5) = breakConversion c5
        val (from6,to6,ctype6) = breakConversion c6
        val (from7,to7,ctype7) = breakConversion c7
        val (from8,to8,ctype8) = breakConversion c8
        val (from9,to9,ctype9) = breakConversion c9
        val (from10,to10,ctype10) = breakConversion c10
        val (from11,to11,ctype11) = breakConversion c11
    
        val ctypes = [ctype1,ctype2,ctype3,ctype4,ctype5,ctype6,
                      ctype7,ctype8,ctype9,ctype10,ctype11]
            
        fun from v =
            let val (v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11) = break11 ctypes v
            in (from1 v1,from2 v2,from3 v3,from4 v4,from5 v5,from6 v6,
                from7 v7,from8 v8,from9 v9,from10 v10,from11 v11)
            end
    
        fun to (a,b,c,d,e,f,g,h,i,j,k) =
            make_struct (ListPair.zip (ctypes, [to1 a,to2 b,to3 c,to4 d,to5 e, to6 f,
                                                to7 g,to8 h,to9 i,to10 j,to11 k]))
        in
        mkConversion from to (Cstruct ctypes)
        end


    fun STRUCT12 (c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12) =
        let
        fun break12 ts v =
            case break_struct ts v of
                [a,b,c,d,e,f,g,h,i,j,k,l] => (a,b,c,d,e,f,g,h,i,j,k,l)
            | _ => raise Fail "break"
        val (from1,to1,ctype1) = breakConversion c1
        val (from2,to2,ctype2) = breakConversion c2
        val (from3,to3,ctype3) = breakConversion c3
        val (from4,to4,ctype4) = breakConversion c4
        val (from5,to5,ctype5) = breakConversion c5
        val (from6,to6,ctype6) = breakConversion c6
        val (from7,to7,ctype7) = breakConversion c7
        val (from8,to8,ctype8) = breakConversion c8
        val (from9,to9,ctype9) = breakConversion c9
        val (from10,to10,ctype10) = breakConversion c10
        val (from11,to11,ctype11) = breakConversion c11
        val (from12,to12,ctype12) = breakConversion c12
    
        val ctypes = [ctype1,ctype2,ctype3,ctype4,ctype5,ctype6,
                      ctype7,ctype8,ctype9,ctype10,ctype11,ctype12]
            
        fun from v =
            let val (v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12) = break12 ctypes v
            in (from1 v1,from2 v2,from3 v3,from4 v4,from5 v5,from6 v6,
                from7 v7,from8 v8,from9 v9,from10 v10,from11 v11,from12 v12)
            end
    
        fun to (a,b,c,d,e,f,g,h,i,j,k,l) =
            make_struct (ListPair.zip (ctypes, [to1 a,to2 b,to3 c,to4 d,to5 e, to6 f,
                                                to7 g,to8 h,to9 i,to10 j,to11 k, to12 l]))
        in
        mkConversion from to (Cstruct ctypes)
        end

    fun STRUCT14 (c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14) =
        let
        fun break14 ts v =
            case break_struct ts v of
                [a,b,c,d,e,f,g,h,i,j,k,l,m,n] => (a,b,c,d,e,f,g,h,i,j,k,l,m,n)
            | _ => raise Fail "break"
        val (from1,to1,ctype1) = breakConversion c1
        val (from2,to2,ctype2) = breakConversion c2
        val (from3,to3,ctype3) = breakConversion c3
        val (from4,to4,ctype4) = breakConversion c4
        val (from5,to5,ctype5) = breakConversion c5
        val (from6,to6,ctype6) = breakConversion c6
        val (from7,to7,ctype7) = breakConversion c7
        val (from8,to8,ctype8) = breakConversion c8
        val (from9,to9,ctype9) = breakConversion c9
        val (from10,to10,ctype10) = breakConversion c10
        val (from11,to11,ctype11) = breakConversion c11
        val (from12,to12,ctype12) = breakConversion c12
        val (from13,to13,ctype13) = breakConversion c13
        val (from14,to14,ctype14) = breakConversion c14
    
        val ctypes = [ctype1,ctype2,ctype3,ctype4,ctype5,ctype6,
                      ctype7,ctype8,ctype9,ctype10,ctype11,ctype12,
                      ctype13,ctype14]
            
        fun from v =
            let val (v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14) = break14 ctypes v
            in (from1 v1,from2 v2,from3 v3,from4 v4,from5 v5,from6 v6,
                from7 v7,from8 v8,from9 v9,from10 v10,from11 v11,from12 v12,
                from13 v13,from14 v14)
            end
    
        fun to (a,b,c,d,e,f,g,h,i,j,k,l,m,n) =
            make_struct (ListPair.zip (ctypes, [to1 a,to2 b,to3 c,to4 d,to5 e, to6 f,
                                                to7 g,to8 h,to9 i,to10 j,to11 k, to12 l,
                                                to13 m, to14 n]))
        in
        mkConversion from to (Cstruct ctypes)
        end

    fun STRUCT16 (c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,c16) =
        let
        nonfix o
        fun break16 ts v =
            case break_struct ts v of
                [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p] => (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)
            | _ => raise Fail "break"
        val (from1,to1,ctype1) = breakConversion c1
        val (from2,to2,ctype2) = breakConversion c2
        val (from3,to3,ctype3) = breakConversion c3
        val (from4,to4,ctype4) = breakConversion c4
        val (from5,to5,ctype5) = breakConversion c5
        val (from6,to6,ctype6) = breakConversion c6
        val (from7,to7,ctype7) = breakConversion c7
        val (from8,to8,ctype8) = breakConversion c8
        val (from9,to9,ctype9) = breakConversion c9
        val (from10,to10,ctype10) = breakConversion c10
        val (from11,to11,ctype11) = breakConversion c11
        val (from12,to12,ctype12) = breakConversion c12
        val (from13,to13,ctype13) = breakConversion c13
        val (from14,to14,ctype14) = breakConversion c14
        val (from15,to15,ctype15) = breakConversion c15
        val (from16,to16,ctype16) = breakConversion c16
    
        val ctypes = [ctype1,ctype2,ctype3,ctype4,ctype5,ctype6,
                      ctype7,ctype8,ctype9,ctype10,ctype11,ctype12,
                      ctype13,ctype14,ctype15,ctype16]
            
        fun from v =
            let val (v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16) = break16 ctypes v
            in (from1 v1,from2 v2,from3 v3,from4 v4,from5 v5,from6 v6,
                from7 v7,from8 v8,from9 v9,from10 v10,from11 v11,from12 v12,
                from13 v13,from14 v14,from15 v15,from16 v16)
            end
    
        fun to (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) =
            make_struct (ListPair.zip (ctypes, [to1 a,to2 b,to3 c,to4 d,to5 e, to6 f,
                                                to7 g,to8 h,to9 i,to10 j,to11 k, to12 l,
                                                to13 m, to14 n, to15 o, to16 p]))
        in
        mkConversion from to (Cstruct ctypes)
        end

    fun STRUCT18 (c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,c16,c17,c18) =
        let
        nonfix o
        fun break18 ts v =
            case break_struct ts v of
                [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r] =>
                    (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r)
            | _ => raise Fail "break"
        val (from1,to1,ctype1) = breakConversion c1
        val (from2,to2,ctype2) = breakConversion c2
        val (from3,to3,ctype3) = breakConversion c3
        val (from4,to4,ctype4) = breakConversion c4
        val (from5,to5,ctype5) = breakConversion c5
        val (from6,to6,ctype6) = breakConversion c6
        val (from7,to7,ctype7) = breakConversion c7
        val (from8,to8,ctype8) = breakConversion c8
        val (from9,to9,ctype9) = breakConversion c9
        val (from10,to10,ctype10) = breakConversion c10
        val (from11,to11,ctype11) = breakConversion c11
        val (from12,to12,ctype12) = breakConversion c12
        val (from13,to13,ctype13) = breakConversion c13
        val (from14,to14,ctype14) = breakConversion c14
        val (from15,to15,ctype15) = breakConversion c15
        val (from16,to16,ctype16) = breakConversion c16
        val (from17,to17,ctype17) = breakConversion c17
        val (from18,to18,ctype18) = breakConversion c18
    
        val ctypes = [ctype1,ctype2,ctype3,ctype4,ctype5,ctype6,
                      ctype7,ctype8,ctype9,ctype10,ctype11,ctype12,
                      ctype13,ctype14,ctype15,ctype16,ctype17,ctype18]
            
        fun from v =
            let val (v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,
                     v11,v12,v13,v14,v15,v16,v17,v18) = break18 ctypes v
            in (from1 v1,from2 v2,from3 v3,from4 v4,from5 v5,from6 v6,
                from7 v7,from8 v8,from9 v9,from10 v10,from11 v11,from12 v12,
                from13 v13,from14 v14,from15 v15,from16 v16,
                from17 v17,from18 v18)
            end
    
        fun to (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) =
            make_struct (ListPair.zip (ctypes, [to1 a,to2 b,to3 c,to4 d,to5 e, to6 f,
                                                to7 g,to8 h,to9 i,to10 j,to11 k, to12 l,
                                                to13 m,to14 n,to15 o, to16 p,
                                                to17 q,to18 r]))
        in
        mkConversion from to (Cstruct ctypes)
        end

    fun STRUCT19 (c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,c16,c17,c18,c19) =
        let
        nonfix o
        fun break19 ts v =
            case break_struct ts v of
                [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s] =>
                    (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s)
            | _ => raise Fail "break"
        val (from1,to1,ctype1) = breakConversion c1
        val (from2,to2,ctype2) = breakConversion c2
        val (from3,to3,ctype3) = breakConversion c3
        val (from4,to4,ctype4) = breakConversion c4
        val (from5,to5,ctype5) = breakConversion c5
        val (from6,to6,ctype6) = breakConversion c6
        val (from7,to7,ctype7) = breakConversion c7
        val (from8,to8,ctype8) = breakConversion c8
        val (from9,to9,ctype9) = breakConversion c9
        val (from10,to10,ctype10) = breakConversion c10
        val (from11,to11,ctype11) = breakConversion c11
        val (from12,to12,ctype12) = breakConversion c12
        val (from13,to13,ctype13) = breakConversion c13
        val (from14,to14,ctype14) = breakConversion c14
        val (from15,to15,ctype15) = breakConversion c15
        val (from16,to16,ctype16) = breakConversion c16
        val (from17,to17,ctype17) = breakConversion c17
        val (from18,to18,ctype18) = breakConversion c18
        val (from19,to19,ctype19) = breakConversion c19
    
        val ctypes = [ctype1,ctype2,ctype3,ctype4,ctype5,ctype6,
                      ctype7,ctype8,ctype9,ctype10,ctype11,ctype12,
                      ctype13,ctype14,ctype15,ctype16,ctype17,ctype18,
                      ctype19]
            
        fun from v =
            let val (v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,
                     v11,v12,v13,v14,v15,v16,v17,v18,v19) = break19 ctypes v
            in (from1 v1,from2 v2,from3 v3,from4 v4,from5 v5,from6 v6,
                from7 v7,from8 v8,from9 v9,from10 v10,from11 v11,from12 v12,
                from13 v13,from14 v14,from15 v15,from16 v16,
                from17 v17,from18 v18,from19 v19)
            end
    
        fun to (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s) =
            make_struct (ListPair.zip (ctypes, [to1 a,to2 b,to3 c,to4 d,to5 e, to6 f,
                                                to7 g,to8 h,to9 i,to10 j,to11 k, to12 l,
                                                to13 m,to14 n,to15 o, to16 p,
                                                to17 q,to18 r,to19 s]))
        in
        mkConversion from to (Cstruct ctypes)
        end

    fun STRUCT20 (c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,c16,c17,c18,c19,c20) =
        let
        nonfix o
        fun break20 ts v =
            case break_struct ts v of
                [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t] =>
                    (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)
            | _ => raise Fail "break"
        val (from1,to1,ctype1) = breakConversion c1
        val (from2,to2,ctype2) = breakConversion c2
        val (from3,to3,ctype3) = breakConversion c3
        val (from4,to4,ctype4) = breakConversion c4
        val (from5,to5,ctype5) = breakConversion c5
        val (from6,to6,ctype6) = breakConversion c6
        val (from7,to7,ctype7) = breakConversion c7
        val (from8,to8,ctype8) = breakConversion c8
        val (from9,to9,ctype9) = breakConversion c9
        val (from10,to10,ctype10) = breakConversion c10
        val (from11,to11,ctype11) = breakConversion c11
        val (from12,to12,ctype12) = breakConversion c12
        val (from13,to13,ctype13) = breakConversion c13
        val (from14,to14,ctype14) = breakConversion c14
        val (from15,to15,ctype15) = breakConversion c15
        val (from16,to16,ctype16) = breakConversion c16
        val (from17,to17,ctype17) = breakConversion c17
        val (from18,to18,ctype18) = breakConversion c18
        val (from19,to19,ctype19) = breakConversion c19
        val (from20,to20,ctype20) = breakConversion c20
    
        val ctypes = [ctype1,ctype2,ctype3,ctype4,ctype5,ctype6,
                      ctype7,ctype8,ctype9,ctype10,ctype11,ctype12,
                      ctype13,ctype14,ctype15,ctype16,ctype17,ctype18,
                      ctype19,ctype20]
            
        fun from v =
            let val (v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,
                     v11,v12,v13,v14,v15,v16,v17,v18,v19,v20) = break20 ctypes v
            in (from1 v1,from2 v2,from3 v3,from4 v4,from5 v5,from6 v6,
                from7 v7,from8 v8,from9 v9,from10 v10,from11 v11,from12 v12,
                from13 v13,from14 v14,from15 v15,from16 v16,
                from17 v17,from18 v18,from19 v19,from20 v20)
            end
    
        fun to (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) =
            make_struct (ListPair.zip (ctypes, [to1 a,to2 b,to3 c,to4 d,to5 e, to6 f,
                                                to7 g,to8 h,to9 i,to10 j,to11 k, to12 l,
                                                to13 m,to14 n,to15 o, to16 p,
                                                to17 q,to18 r,to19 s,to20 t]))
        in
        mkConversion from to (Cstruct ctypes)
        end

    (* Conversion for a fixed size character array. *)
    fun CHARARRAY n =
        let
            val base = Cstruct (List.tabulate (n, fn _ => Cchar))
            fun from (v: vol) : string = fromCstring(address v)
            fun to (s:string): vol =
                deref(toCstring(if size s > n-1 then String.substring(s, 0, n-1) else s))
        in
            mkConversion from to base
        end


    fun kernel name = get_sym "kernel32.dll" name
    and user sym = get_sym "user32.DLL" sym
    and commdlg sym = get_sym "comdlg32.DLL" sym
    and gdi sym = get_sym "gdi32.DLL" sym
    and shell sym = get_sym "shell32.DLL" sym
    and comctl sym = get_sym "comctl32.DLL" sym

    (* Conversion to and from LargeWord.word.  This is used for 32-bit flags. *)
    val WORD = absConversion {abs = LargeWord.fromInt, rep = LargeWord.toInt} UINT

    (* Various functions return zero if error.  This conversion checks for that. *)
    fun POSINT name =
        absConversion {abs = fn 0 => raiseSysErr() | n => n, rep = fn i => i} UINT

    (* Conversion between string option and C strings.  NONE is converted to NULL. *)
    local
        fun toCstropt NONE = toCint 0
         |  toCstropt (SOME s) = toCstring s

        fun fromCstropt v =
            if fromCint v = 0
            then NONE
            else SOME(fromCstring v)
    in
        val STRINGOPT = mkConversion fromCstropt toCstropt (Cpointer Cchar)
    end

    (* In several cases when extracting a string it is not possible in advance
       to know how big to make the buffer.  This function loops until all the
       string has been extracted. *)
    fun getStringCall(f: vol*int -> int): string =
    let
        fun doCall initialSize =
        let
            (* Allocate a buffer to receive the result.  For safety we make it
               one character longer than we actually say because it's not always
               clear whether the length we pass is the size including the NULL.
               Equally we are only certain we have read the whole string if
               the return value is less than initialSize-1 because the return
               value could be the number of real characters copied to the buffer. *)
            val buff = alloc (initialSize+1) Cchar
            val resultSize = f(address buff, initialSize)
        in
            if resultSize < initialSize-1
            then (* We've got it all. *) fromCstring(address buff)
            else doCall(initialSize + initialSize div 2)
        end
    in
        doCall (*1024*) 3 (* Use a small size initially for testing. *)
    end

    (* Some C functions take a vector of values to allow a variable number of
       elements to be passed.  We use a list for this in ML. *)
    fun list2Vector (conv: 'a Conversion) (l:'a list): vol * int =
    let
        val count = List.length l
        val (_, to, element) = breakConversion conv
        val vec =alloc count element
        fun setItem(item, n) =
            (assign element (offset n element vec) (to item); n+1)
        val _: int = List.foldl setItem 0 l 
    in
        (address vec, count)
    end

    (* We have to allocate some of the values as global handles. *)
    abstype HGLOBAL = HG of int
    with
        val hglobalNull = HG 0
        fun isHglobalNull(HG 0) = true | isHglobalNull _ = false
        val HGLOBAL = absConversion {abs=HG, rep=fn (HG i) => i} UINT
    end
    
    val GlobalAlloc = call2 (kernel "GlobalAlloc") (INT, INT) HGLOBAL
    val GlobalLock = call1 (kernel "GlobalLock") (HGLOBAL) POINTER
    val GlobalFree = call1 (kernel "GlobalFree") (HGLOBAL) HGLOBAL
    val GlobalSize = call1 (kernel "GlobalSize") (HGLOBAL) INT
    val GlobalUnlock = call1 (kernel "GlobalUnlock") (HGLOBAL) BOOL

    (* Conversion for Word8Vector.  We can't do this as a general conversion because
       we can't find out how big the C vector is. *)
    val fromWord8vec = toCbytes
    and toWord8vec = fromCbytes

    (* Conversion for a fixed size byte array. *)
    fun BYTEARRAY n =
    let
        val base = Cstruct (List.tabulate (n, fn _ => Cchar))
        fun from v = toWord8vec(address v, n)
        fun to w =
            if Word8Vector.length w <> n then raise Size else deref(fromWord8vec w)
    in
        mkConversion from to base
    end

    (* Conversion for a fixed size char array. *)
    fun CHARARRAY n =
    let
        val base = Cstruct (List.tabulate (n, fn _ => Cchar))
        fun from (v: vol): string =
        let
            open Word8Vector
            infix sub
            val vector = toWord8vec(address v, n)
            val len = length vector
            (* This will be null-terminated - trim it at the first null. *)
            fun term i =
                if i = len orelse (vector sub i) = 0w0 then i else term (i+1)
        in
            Byte.unpackStringVec(Word8VectorSlice.slice(vector, 0, SOME(term 0)))
        end
        fun to (s: string): vol =
        let
            open Word8Array
            val substr = if size s < n then s else String.substring(s, 0, n-1)
            val arr = array(n, 0w0)
            val _ = copyVec{src=Byte.stringToBytes substr, dst=arr, di=0}
        in
            deref(fromWord8vec(vector arr))
        end
    in
        mkConversion from to base
    end

    (* These should always be UNSIGNED values. *)
    local
        open LargeWord
        infix << >> orb andb
    in
        fun LOWORD(l) = toInt(fromInt l andb 0wxFFFF)
        fun HIWORD(l) = toInt((fromInt l >> 0w16) andb 0wxFFFF)
    
        fun MAKELONG(a, b) =
            toInt ((fromInt b << 0w16) orb (fromInt a andb 0wxFFFF))
            
        fun HIBYTE(w) = toInt((fromInt w >> 0w8) andb 0wxFF)
        fun LOBYTE(w) = toInt(fromInt w andb 0wxFF)
    end

    (* Convert between strings and vectors containing Unicode characters.
       N.B.  These are not null terminated. *)
    fun unicodeToString(w: Word8Vector.vector): string =
    let
        val inputLength = Word8Vector.length w
        val WideCharToMultiByte = call8 (kernel "WideCharToMultiByte")
            (INT, INT, POINTER, INT, POINTER, INT, INT, INT) INT;
        val outputBuf = alloc inputLength Cchar (* Assume big enough for the moment. *)

        val conv = WideCharToMultiByte(0 (* CP_ACP *),
            0 (* Default *),
            fromWord8vec w,
            inputLength div 2, (* Number of Unicode characters. *)
            address outputBuf,
            inputLength,
            0,
            0);
    in
        (* We can't use fromCstring here because it's not necessarily null terminated. *)
        CharVector.tabulate(conv, fn i => fromCchar(offset i Cchar outputBuf))
    end

    fun stringToUnicode(s: string): Word8Vector.vector =
    let
        val inputLength = size s
        val outputLength = inputLength * 2 (* Should be enough. *)
        val MultiByteToWideChar =
            call6 (kernel "MultiByteToWideChar") (INT, INT, STRING, INT, POINTER, INT) INT;
        val outputBuf = alloc outputLength Cchar

        val conv = MultiByteToWideChar(0 (* CP_ACP *),
            0 (* Default *),
            s,
            inputLength,
            address outputBuf,
            outputLength+1);
    in
        toWord8vec (address outputBuf, conv*2)
    end

end
end;
