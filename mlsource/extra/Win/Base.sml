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
(* This contains various types and other values which are needed in various
   modules.  All the exported types are contained in other structures. *)
structure Base:
sig
    val winCall0: Foreign.symbol -> unit -> 'a Foreign.conversion -> unit -> 'a
    val winCall1: Foreign.symbol -> 'a Foreign.conversion -> 'b Foreign.conversion -> 'a -> 'b
    val winCall2: Foreign.symbol -> 'a Foreign.conversion * 'b Foreign.conversion -> 'c Foreign.conversion -> 'a * 'b -> 'c
    val winCall3: Foreign.symbol -> 'a Foreign.conversion * 'b Foreign.conversion * 'c Foreign.conversion -> 'd Foreign.conversion -> 'a * 'b * 'c -> 'd
    val winCall4: Foreign.symbol -> 'a Foreign.conversion * 'b Foreign.conversion * 'c Foreign.conversion * 'd Foreign.conversion -> 'e Foreign.conversion ->
            'a * 'b * 'c * 'd -> 'e
    val winCall5:
        Foreign.symbol -> 'a Foreign.conversion * 'b Foreign.conversion * 'c Foreign.conversion * 'd Foreign.conversion *  'e Foreign.conversion ->
            'f Foreign.conversion -> 'a * 'b * 'c * 'd * 'e -> 'f
    val winCall6:
        Foreign.symbol -> 'a Foreign.conversion * 'b Foreign.conversion * 'c Foreign.conversion * 'd Foreign.conversion * 'e Foreign.conversion *
             'f Foreign.conversion -> 'g Foreign.conversion -> 'a * 'b * 'c * 'd * 'e * 'f -> 'g
    val winCall7:
        Foreign.symbol -> 'a Foreign.conversion * 'b Foreign.conversion * 'c Foreign.conversion * 'd Foreign.conversion * 'e Foreign.conversion *
             'f Foreign.conversion * 'g Foreign.conversion -> 'h Foreign.conversion ->
             'a * 'b * 'c * 'd * 'e * 'f * 'g -> 'h
    val winCall8:
        Foreign.symbol -> 'a Foreign.conversion * 'b Foreign.conversion * 'c Foreign.conversion * 'd Foreign.conversion * 'e Foreign.conversion *
             'f Foreign.conversion * 'g Foreign.conversion * 'h Foreign.conversion -> 'i Foreign.conversion ->
             'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h -> 'i
    val winCall9:
        Foreign.symbol -> 'a Foreign.conversion * 'b Foreign.conversion * 'c Foreign.conversion * 'd Foreign.conversion * 'e Foreign.conversion *
             'f Foreign.conversion * 'g Foreign.conversion * 'h Foreign.conversion * 'i Foreign.conversion ->
             'j Foreign.conversion -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i -> 'j
    val winCall10:
        Foreign.symbol -> 'a Foreign.conversion * 'b Foreign.conversion * 'c Foreign.conversion * 'd Foreign.conversion * 'e Foreign.conversion *
             'f Foreign.conversion * 'g Foreign.conversion * 'h Foreign.conversion * 'i Foreign.conversion * 'j Foreign.conversion ->
             'k Foreign.conversion -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j -> 'k
    val winCall11:
        Foreign.symbol -> 'a Foreign.conversion * 'b Foreign.conversion * 'c Foreign.conversion * 'd Foreign.conversion * 'e Foreign.conversion *
             'f Foreign.conversion * 'g Foreign.conversion * 'h Foreign.conversion * 'i Foreign.conversion * 'j Foreign.conversion * 'k Foreign.conversion ->
             'l Foreign.conversion -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k -> 'l
    val winCall12:
        Foreign.symbol -> 'a Foreign.conversion * 'b Foreign.conversion * 'c Foreign.conversion * 'd Foreign.conversion * 'e Foreign.conversion *
             'f Foreign.conversion * 'g Foreign.conversion * 'h Foreign.conversion * 'i Foreign.conversion * 'j Foreign.conversion * 'k Foreign.conversion *
             'l Foreign.conversion -> 'm Foreign.conversion ->
             'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l -> 'm
    val winCall13:
        Foreign.symbol -> 'a Foreign.conversion * 'b Foreign.conversion * 'c Foreign.conversion * 'd Foreign.conversion * 'e Foreign.conversion *
             'f Foreign.conversion * 'g Foreign.conversion * 'h Foreign.conversion * 'i Foreign.conversion * 'j Foreign.conversion * 'k Foreign.conversion *
             'l Foreign.conversion * 'm Foreign.conversion -> 'n Foreign.conversion ->
             'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm -> 'n
    val winCall14:
        Foreign.symbol -> 'a Foreign.conversion * 'b Foreign.conversion * 'c Foreign.conversion * 'd Foreign.conversion * 'e Foreign.conversion *
             'f Foreign.conversion * 'g Foreign.conversion * 'h Foreign.conversion * 'i Foreign.conversion * 'j Foreign.conversion * 'k Foreign.conversion *
             'l Foreign.conversion * 'm Foreign.conversion * 'n Foreign.conversion ->
            'o Foreign.conversion -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm * 'n -> 'o

    (* We probably don't need all of these. *)
    val winFun0: unit -> 'a Foreign.conversion -> (unit -> 'a) Foreign.conversion
    val winFun1: 'a Foreign.conversion -> 'b Foreign.conversion -> ('a -> 'b) Foreign.conversion
    val winFun2: 'a Foreign.conversion * 'b Foreign.conversion -> 'c Foreign.conversion -> ('a * 'b -> 'c) Foreign.conversion
    val winFun3: 'a Foreign.conversion * 'b Foreign.conversion *  'c Foreign.conversion -> 'd Foreign.conversion -> ('a * 'b * 'c -> 'd) Foreign.conversion
    val winFun4: 'a Foreign.conversion * 'b Foreign.conversion * 'c Foreign.conversion * 'd Foreign.conversion -> 'e Foreign.conversion ->
            ('a * 'b * 'c * 'd -> 'e) Foreign.conversion
    val winFun5: 'a Foreign.conversion * 'b Foreign.conversion * 'c Foreign.conversion * 'd Foreign.conversion * 'e Foreign.conversion -> 'f Foreign.conversion ->
            ('a * 'b * 'c * 'd * 'e -> 'f) Foreign.conversion
    val winFun6:
        'a Foreign.conversion * 'b Foreign.conversion * 'c Foreign.conversion * 'd Foreign.conversion * 'e Foreign.conversion * 'f Foreign.conversion ->
            'g Foreign.conversion -> ('a * 'b * 'c * 'd * 'e * 'f -> 'g) Foreign.conversion

    val kernel: string -> Foreign.symbol
    and user: string -> Foreign.symbol
    and commdlg: string -> Foreign.symbol
    and gdi: string -> Foreign.symbol
    and shell: string -> Foreign.symbol
    and comctl: string -> Foreign.symbol
    
    val cSIZE_T: int Foreign.conversion
    and cLPARAM: int Foreign.conversion
    and cLONG_PTR: int Foreign.conversion
    and cULONG_PTR: int Foreign.conversion
    and cINT_PTR: int Foreign.conversion
    and cUINT_PTR: int Foreign.conversion
    and cDWORD: int Foreign.conversion
   
    val cBool: bool Foreign.conversion
    
    val successState: string -> unit Foreign.conversion
    val cPOSINT: string -> int Foreign.conversion
    
    type POINT = { x: int, y: int }
    val cPoint: POINT Foreign.conversion
    type RECT =  { left: int, top: int, right: int, bottom: int }
    val cRect: RECT Foreign.conversion
    type SIZE = { cx: int, cy: int }
    val cSize: SIZE Foreign.conversion

    eqtype 'a HANDLE
    val hNull: 'a HANDLE
    val isHNull: 'a HANDLE -> bool
    val handleOfVoidStar: Foreign.Memory.voidStar -> 'a HANDLE
    and voidStarOfHandle: 'a HANDLE -> Foreign.Memory.voidStar

    eqtype HMENU and HDC and HWND and HINSTANCE and HGDIOBJ
    and HDROP and HRSRC and HUPDATE

    val cHGDIOBJ:   HGDIOBJ Foreign.conversion
    and cHDROP:     HDROP Foreign.conversion
    and cHMENU:     HMENU Foreign.conversion
    and cHINSTANCE: HINSTANCE Foreign.conversion
    and cHDC:       HDC Foreign.conversion
    and cHWND:      HWND Foreign.conversion
    val cHMENUOPT:  HMENU option Foreign.conversion
    and cHGDIOBJOPT: HGDIOBJ option Foreign.conversion
    and cHWNDOPT: HWND option Foreign.conversion
    and cHRSRC: HRSRC Foreign.conversion
    and cHUPDATE: HUPDATE Foreign.conversion

    val hgdiObjNull:HGDIOBJ 
    and isHgdiObjNull: HGDIOBJ -> bool
    and hdcNull: HDC
    and isHdcNull: HDC -> bool
    and hmenuNull: HMENU
    and isHmenuNull: HMENU -> bool
    and hinstanceNull: HINSTANCE
    and isHinstanceNull: HINSTANCE -> bool
    and hwndNull: HWND

    type HPALETTE = HGDIOBJ and HFONT = HGDIOBJ and HPEN = HGDIOBJ
    and HBITMAP = HGDIOBJ and HRGN = HGDIOBJ and HBRUSH = HGDIOBJ
    and HENHMETAFILE = HGDIOBJ and HMETAFILE = HGDIOBJ

    val cHPALETTE: HPALETTE Foreign.conversion
    and cHFONT: HFONT Foreign.conversion
    and cHPEN: HPEN Foreign.conversion
    and cHBITMAP: HBITMAP Foreign.conversion
    and cHRGN: HRGN Foreign.conversion
    and cHBRUSH: HBRUSH Foreign.conversion
    and cHENHMETAFILE: HENHMETAFILE Foreign.conversion
    and cHMETAFILE: HMETAFILE Foreign.conversion

    
    type HICON = HGDIOBJ and HCURSOR = HGDIOBJ
    val cHICON: HICON Foreign.conversion
    and cHCURSOR: HCURSOR Foreign.conversion
    
    val absConversion:
        {abs: 'a -> 'b, rep: 'b -> 'a} -> 'a Foreign.conversion -> 'b Foreign.conversion

    val tableLookup:
        (''a * int) list * ((int -> ''a) * (''a -> int)) option -> (''a -> int) * (int -> ''a)
    and tableSetLookup:
        (''a * int) list * ((int -> ''a) * (''a -> int)) option -> (''a list -> int) * (int -> ''a list)

    val tableConversion:
        (''a * int) list * ((int -> ''a) * (''a -> int)) option ->
            int Foreign.conversion -> ''a Foreign.conversion
    and tableSetConversion:
        (''a * int) list * ((int -> ''a) * (''a -> int)) option ->
            int Foreign.conversion -> ''a list Foreign.conversion
    
    datatype ClassType = NamedClass of string | ClassAtom of int
    val cCLASS: ClassType Foreign.conversion

    datatype ClipboardFormat =
        CF_NONE | CF_TEXT | CF_BITMAP | CF_METAFILEPICT | CF_SYLK | CF_DIF | CF_TIFF |
        CF_OEMTEXT | CF_DIB | CF_PALETTE | CF_PENDATA | CF_RIFF | CF_WAVE | CF_UNICODETEXT |
        CF_ENHMETAFILE | CF_OWNERDISPLAY | CF_DSPTEXT | CF_DSPBITMAP | CF_DSPMETAFILEPICT |
        CF_DSPENHMETAFILE | CF_PRIVATE of int | CF_GDIOBJ of int | CF_REGISTERED of int |
        CF_HDROP | CF_LOCALE
    val cCLIPFORMAT: ClipboardFormat Foreign.conversion
    
    datatype RESID = IdAsInt of int | IdAsString of string
    val cRESID: RESID Foreign.conversion
    
    val cWORD: LargeWord.word Foreign.conversion 
    val STRINGOPT: string option Foreign.conversion
    val cCHARARRAY: int -> string Foreign.conversion
    val fromCstring: Foreign.Memory.voidStar -> string
    val toCstring: string -> Foreign.Memory.voidStar (* Memory must be freed *)
    
    val getStringCall: (Foreign.Memory.voidStar*int -> int) -> string

    eqtype HGLOBAL
    val cHGLOBAL: HGLOBAL Foreign.conversion
    val GlobalAlloc: int * int -> HGLOBAL
    val GlobalLock: HGLOBAL -> Foreign.Memory.voidStar
    val GlobalFree: HGLOBAL -> HGLOBAL
    val GlobalSize: HGLOBAL -> int
    val GlobalUnlock: HGLOBAL -> bool

    
    val HIWORD: int -> int
    val LOWORD: int -> int
    val MAKELONG: int * int -> int
    val HIBYTE: int -> int
    val LOBYTE: int -> int
    
    val unicodeToString: Word8Vector.vector -> string
    val stringToUnicode: string -> Word8Vector.vector
    
    val GetLastError: unit -> OS.syserror
    
    val checkResult: bool -> unit
    val raiseSysErr: unit -> 'a
    
    structure FindReplaceFlags:
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
    end

end =
struct
    open Foreign
(*    val System_isShort : vol -> bool =
        RunCall.run_call1 RuntimeCalls.POLY_SYS_is_short*)

    fun absConversion {abs: 'a -> 'b, rep: 'b -> 'a} (c: 'a conversion) : 'b conversion =
    let
        val { load=loadI, store=storeI, ctype } = breakConversion c
        fun load m = abs(loadI m)
        fun store(m, v) = storeI(m, rep v)
    in
        makeConversion { load = load, store = store, ctype = ctype }
    end

    (* In many cases we can pass a set of options as a bit set. *)
    (*
    fun bitsetConversion {abs, rep} =
    let
        val (fromC, toC, Ctype) = breakConversion INT
        val fromList = List.foldl (fn(i, n) => IntInf.orb(rep i, n)) 0
        fun toList n = [abs n] (* This is a bit of a mess. *)
    in
        mkConversion (toList o fromCuint) (toCuint o fromList) Cuint
    end*)
   
    val cDWORD = cUint32 (* Defined to be 32-bit unsigned *)
    
    (* For some reason Windows has both INT_PTR and LONG_PTR and they
       are slightly different. *)
    val cLONG_PTR =
        if #size LowLevel.cTypePointer = 0w4
        then cLong
        else cInt64
    
    val cINT_PTR =
        if #size LowLevel.cTypePointer = 0w4
        then cInt
        else cInt64

    val cULONG_PTR =
        if #size LowLevel.cTypePointer = 0w4
        then cUlong
        else cUint64

    val cUINT_PTR =
        if #size LowLevel.cTypePointer = 0w4
        then cUint
        else cUint64

    val cLPARAM = cLONG_PTR
    val cSIZE_T = cULONG_PTR (* Probably. *)

    (* These are called XXX32.DLL on both 32-bit and 64-bit. *)
    fun kernel name = getSymbol(loadLibrary "kernel32.dll") name
    and user sym = getSymbol(loadLibrary "user32.DLL") sym
    and commdlg sym = getSymbol(loadLibrary "comdlg32.DLL") sym
    and gdi sym = getSymbol(loadLibrary "gdi32.DLL") sym
    and shell sym = getSymbol(loadLibrary "shell32.DLL") sym
    and comctl sym = getSymbol(loadLibrary "comctl32.DLL") sym

    (* We need to use the Pascal calling convention on 32-bit Windows. *)
    val winAbi =
        case List.find (fn ("stdcall", _) => true | _ => false) LibFFI.abiList of
            SOME(_, abi) => abi
        |   NONE => LibFFI.abiDefault

    (* As well as setting the abi we can also use the old argument order. *)
    fun winCall0 s f t = call0withAbi winAbi f t s
    and winCall1 s f t = call1withAbi winAbi f t s
    and winCall2 s f t = call2withAbi winAbi f t s
    and winCall3 s f t = call3withAbi winAbi f t s
    and winCall4 s f t = call4withAbi winAbi f t s
    and winCall5 s f t = call5withAbi winAbi f t s
    and winCall6 s f t = call6withAbi winAbi f t s
    and winCall7 s f t = call7withAbi winAbi f t s
    and winCall8 s f t = call8withAbi winAbi f t s
    and winCall9 s f t = call9withAbi winAbi f t s
    and winCall10 s f t = call10withAbi winAbi f t s
    and winCall11 s f t = call11withAbi winAbi f t s
    and winCall12 s f t = call12withAbi winAbi f t s
    and winCall13 s f t = call13withAbi winAbi f t s
    and winCall14 s f t = call14withAbi winAbi f t s
    
    fun winFun0 args = cFunction0withAbi winAbi args
    and winFun1 args = cFunction1withAbi winAbi args
    and winFun2 args = cFunction2withAbi winAbi args
    and winFun3 args = cFunction3withAbi winAbi args
    and winFun4 args = cFunction4withAbi winAbi args
    and winFun5 args = cFunction5withAbi winAbi args
    and winFun6 args = cFunction6withAbi winAbi args

    (* Previously we had a specific call to do this.  The error state is
       no longer set by the new FFI. *)
(*
    fun GetLastError(): OS.syserror =
        RunCall.run_call2 RuntimeCalls.POLY_SYS_os_specific (1100, ())
*)
    local
        val getLastError = winCall0 (kernel "GetLastError") () cDWORD
    in
        fun GetLastError(): OS.syserror =
            (* Windows error codes are negative values in OS.syserror. *)
            RunCall.unsafeCast (~ (getLastError()))
    end

    (* The string argument of the SysErr exception is supposed to match the result of OS.errMsg. *)
    fun raiseSysErr () = let val err = GetLastError() in raise OS.SysErr(OS.errorMsg err, SOME err) end

    (* Many system calls return bool.  If the result is false we raise an exception. *)
    fun checkResult true = () | checkResult false = raiseSysErr ()
    
    val cBool: bool conversion =
        absConversion{abs = fn 0 => false | _ => true, rep = fn false => 0 | true => 1} cInt

    fun successState name: unit conversion =
         absConversion { abs = checkResult, rep = fn _ => raise Fail ("successState:" ^ name) } cBool


    type POINT = { x: int, y: int }

    local
        fun breakPoint ({x,y}: POINT) = (x,y)
        fun mkPoint (x,y): POINT = {x=x, y=y}
    in
        val cPoint = absConversion {abs=mkPoint, rep=breakPoint} (cStruct2 (cLong, cLong))
    end

    type RECT =  { left: int, top: int, right: int, bottom: int }

    local
        fun breakRect ({left,top,right,bottom}: RECT) = (left,top,right,bottom)
        fun mkRect (left,top,right,bottom): RECT =
            {left=left,top=top,right=right,bottom=bottom}
    in
        val cRect = absConversion {abs=mkRect, rep=breakRect} (cStruct4 (cLong,cLong,cLong,cLong))
    end

    type SIZE = { cx: int, cy: int }
    local
        fun breakSize ({cx,cy}: SIZE) = (cx,cy)
        fun mkSize (cx,cy): SIZE = {cx=cx, cy=cy}
    in
        val cSize = absConversion {abs=mkSize, rep=breakSize} (cStruct2 (cLong,cLong))
    end

    (* Handles are generally opaque values.  We want them to be eqtypes, though. *)
    local
        structure HandStruct :>
            sig
                eqtype 'a HANDLE
                val hNull: 'a HANDLE
                val isHNull: 'a HANDLE -> bool
                val handleOfVoidStar: Memory.voidStar -> 'a HANDLE
                and voidStarOfHandle: 'a HANDLE -> Memory.voidStar
            end =
        struct
            type 'a HANDLE = Memory.voidStar
            val hNull = Memory.null
            fun isHNull h = h = hNull
        
            (* We sometimes need the next two functions internally.
               They're needed externally unless we change the result type
               of SendMessage to allow us to return a handle for certain
               messages. *)
            fun handleOfVoidStar h = h
            and voidStarOfHandle h = h
        end
    in
        open HandStruct
    end

    (* We just need these as placeholders. We never create values of
       these types.  They are used simply as a way of creating different
       handle types. *)
    (* Don't use abstype - we want them to eqtypes *)
    datatype GdiObj = GdiObj
    and Instance = Instance
    and Drop = Drop
    and DeviceContext = DeviceContext
    and Menu = Menu
    and Window = Window
    and Global = Global
    and Src = Src
    and Update = Update

    (* HINSTANCE is used as an instance of a module. *)
    type HINSTANCE = Instance HANDLE
    and  HDROP = Drop HANDLE
    and  HGDIOBJ = GdiObj HANDLE
    and  HDC = DeviceContext HANDLE
    and  HMENU = Menu HANDLE
    and  HWND = Window HANDLE
    and  HGLOBAL = Global HANDLE
    and  HRSRC = Src HANDLE
    and  HUPDATE = Update HANDLE

    local
        fun cHANDLE() =
            absConversion {abs=handleOfVoidStar, rep=voidStarOfHandle} cPointer
        fun hoptOfvs n =
            if Memory.voidStar2Sysword n = 0w0 then NONE else SOME(handleOfVoidStar n)
        
        fun cHANDLEOPT() =
            absConversion {abs=hoptOfvs, rep=fn v => voidStarOfHandle(getOpt(v, hNull)) } cPointer
    in
        val cHGDIOBJ:   HGDIOBJ conversion = cHANDLE()
        and cHDROP:     HDROP conversion = cHANDLE()
        and cHMENU:     HMENU conversion = cHANDLE()
        and cHINSTANCE: HINSTANCE conversion = cHANDLE()
        and cHDC:       HDC conversion = cHANDLE()
        and cHWND:      HWND conversion = cHANDLE()

        val cHMENUOPT:  HMENU option conversion = cHANDLEOPT()
        and cHGDIOBJOPT: HGDIOBJ option conversion = cHANDLEOPT()
        and cHWNDOPT: HWND option conversion = cHANDLEOPT()
        
        val cHGLOBAL: HGLOBAL conversion = cHANDLE()
        and cHRSRC: HRSRC conversion = cHANDLE()
        and cHUPDATE: HUPDATE conversion = cHANDLE()
    end

    (* Temporary declarations. *)
    val hgdiObjNull:HGDIOBJ  = hNull
    and isHgdiObjNull: HGDIOBJ -> bool = isHNull
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

    val cHPALETTE: HPALETTE conversion = cHGDIOBJ
    and cHFONT: HFONT conversion = cHGDIOBJ
    and cHPEN: HPEN conversion = cHGDIOBJ
    and cHBITMAP: HBITMAP conversion = cHGDIOBJ
    and cHRGN: HRGN conversion = cHGDIOBJ
    and cHBRUSH: HBRUSH conversion = cHGDIOBJ
    and cHENHMETAFILE: HENHMETAFILE conversion = cHGDIOBJ
    and cHMETAFILE: HMETAFILE conversion = cHGDIOBJ

    (* I'm not so happy about treating these as HGDIOBJ but it makes the
       types of messages such as BM_SETIMAGE simpler. *)
    type HICON = HGDIOBJ and HCURSOR = HGDIOBJ
    val cHICON = cHGDIOBJ and cHCURSOR = cHGDIOBJ

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

    fun tableConversion (table: (''a * int) list, default) (conv: int conversion): ''a conversion  =
    let
        val (toInt, fromInt) = tableLookup(table, default)
    in
        absConversion {abs = fromInt, rep = toInt} conv
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

    fun tableSetConversion (table: (''a * int) list, default) (conv: int conversion): ''a list conversion  =
    let
        val (toInt, fromInt) = tableSetLookup(table, default)
    in
        absConversion {abs = fromInt, rep = toInt} conv
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
        open Memory
        val {store=storeS, load=loadS, ctype} = breakConversion cString

        fun storeClass(m, ClassAtom i) =
            if i >= 0 andalso i < 0xC000
            then (setAddress(m, 0w0, sysWord2VoidStar(SysWord.fromInt i)); fn () => ())
            else raise Fail "atom out of range"
        |   storeClass(m, NamedClass s) = storeS(m, s)

        fun loadClass m =
        let
            val v = getAddress(m, 0w0)
        in
            if voidStar2Sysword v < 0wxC000
            then ClassAtom(SysWord.toInt(voidStar2Sysword v))
            else NamedClass(loadS m)
        end

    in
        val cCLASS = makeConversion { load = loadClass, store = storeClass, ctype = ctype }
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
            (* Assume this is cUint *)
            val cCLIPFORMAT = tableConversion(tab, SOME(fromInt, toInt)) cUint
        end

    (* Resources may be specified by strings or by ints. *)
    datatype RESID = IdAsInt of int | IdAsString of string

    local
        open Memory
        val {store=storeS, load=loadS, ctype} = breakConversion cString

        fun storeResid(m, IdAsInt i) =
            if i >= 0 andalso i < 65536
            then (setAddress(m, 0w0, sysWord2VoidStar(SysWord.fromInt i)); fn () => ())
            else raise Fail "resource id out of range"
        |   storeResid(m, IdAsString s) = storeS(m, s)

        fun loadResid m =
        let
            val v = getAddress(m, 0w0)
        in
            if voidStar2Sysword v < 0w65536
            then IdAsInt(SysWord.toInt(voidStar2Sysword v))
            else IdAsString(loadS m)
        end
    in
        val cRESID =
            makeConversion { load = loadResid, store = storeResid, ctype = ctype }
    end

    (*datatype HelpContext =
        HelpInfo_MenuItem of
    |   HelpInfo_Window of

    type HELPINFO = {
    }*)


    (* Useful conversions. *)
    (* Conversion to and from LargeWord.word.  This is used for 32-bit flags. *)
    val cWORD = absConversion {abs = LargeWord.fromInt, rep = LargeWord.toInt} cUlong

    (* Various functions return zero if error.  This conversion checks for that. *)
    fun cPOSINT _ =
        absConversion {abs = fn 0 => raiseSysErr() | n => n, rep = fn i => i} cInt

    (* Conversion between string option and C strings.  NONE is converted to NULL. *)
    val STRINGOPT = cOptionPtr cString

    (* Convert a C string to ML.  We can't use #load cString because the argument is the address of
       the address of the string.  N.B. We normally have to free the memory after use. *)
    fun fromCstring buff =
    let
        open Memory
        (* We can't use #load cString because the argument is the address of
           the address of the string. *)
        fun sLen i = if get8(buff, i) = 0w0 then i else sLen(i+0w1)
        val length = sLen 0w0
        fun loadChar i =
            Char.chr(Word8.toInt(get8(buff, Word.fromInt i)))
    in
        CharVector.tabulate(Word.toInt length, loadChar)
    end

    fun toCstring s =
    let
        open Memory
        val sLen = Word.fromInt(String.size s)
        val sMem = malloc(sLen + 0w1)
        val () = CharVector.appi(fn(i, ch) => set8(sMem, Word.fromInt i, Word8.fromInt(Char.ord ch))) s
        val () = set8(sMem, sLen, 0w0)
    in
        sMem
    end

    (* In several cases when extracting a string it is not possible in advance
       to know how big to make the buffer.  This function loops until all the
       string has been extracted. *)
    fun getStringCall(f: Memory.voidStar*int -> int): string =
    let
        open Memory
        
        fun doCall initialSize =
        let
            (* Allocate a buffer to receive the result.  For safety we make it
               one character longer than we actually say because it's not always
               clear whether the length we pass is the size including the NULL.
               Equally we are only certain we have read the whole string if
               the return value is less than initialSize-1 because the return
               value could be the number of real characters copied to the buffer. *)
            val buff = malloc (Word.fromInt(initialSize+1))
            val resultSize = f(buff, initialSize)
        in
            if resultSize < initialSize-1
            then (* We've got it all. *)
                fromCstring buff before free buff
            else ( free buff; doCall(initialSize + initialSize div 2) )
        end
    in
        doCall (*1024*) 3 (* Use a small size initially for testing. *)
    end

    (* Some C functions take a vector of values to allow a variable number of
       elements to be passed.  We use a list for this in ML. *)
(*    fun list2Vector (conv: 'a conversion) (l:'a list): Memory.voidStar * int =
    let
        val count = List.length l
        val {load=loada, store=storea, release=releasea, ctype={size=sizea, ...}} = conv
        open Memory
        val vec = malloc(Word.fromInt count * sizea)
        fun setItem(item, n) = storea(vec + n, 0w0, 
            (assign element (offset n element vec) (to item); n+sizea)
        val _: int = List.foldl setItem 0w0 l 
    in
        (address vec, count)
    end*)

    (* We have to allocate some of the values as global handles. *)
(*    abstype HGLOBAL = HG of int
    with
        val hglobalNull = HG 0
        fun isHglobalNull(HG 0) = true | isHglobalNull _ = false
        val cHGLOBAL = absConversion {abs=HG, rep=fn (HG i) => i} UINT
    end*)

    val GlobalAlloc = winCall2 (kernel "GlobalAlloc") (cInt, cSIZE_T) cHGLOBAL
    val GlobalLock = winCall1 (kernel "GlobalLock") (cHGLOBAL) cPointer
    val GlobalFree = winCall1 (kernel "GlobalFree") (cHGLOBAL) cHGLOBAL
    val GlobalSize = winCall1 (kernel "GlobalSize") (cHGLOBAL) cSIZE_T
    val GlobalUnlock = winCall1 (kernel "GlobalUnlock") (cHGLOBAL) cBool
(*
    (* Conversion for Word8Vector.  We can't do this as a general conversion because
       we can't find out how big the C vector is. *)
    val fromWord8vec = toCbytes
    and toWord8vec = fromCbytes
*)
(*
    (* Conversion for a fixed size byte array. *)
    fun BYTEARRAY n =
    let
        val base = Cstruct (List.tabulate (n, fn _ => Cchar))
        fun from v = toWord8vec(address v, n)
        fun to w =
            if Word8Vector.length w <> n then raise Size else deref(fromWord8vec w)
    in
        mkConversion from to base
    end *)

    (* Conversion for a fixed size char array. *)
    fun cCHARARRAY n : string conversion =
    let
        (* Make it a struct of chars *)
        val { size=sizeC, align=alignC, ffiType=ffiTypeC } = LowLevel.cTypeChar
        val arraySize = sizeC * Word.fromInt n
        fun ffiType () =
            LibFFI.createFFItype {
                size = arraySize, align = alignC, typeCode=LibFFI.ffiTypeCodeStruct,
                elements = List.tabulate (n, fn _ => ffiTypeC()) }
        val arrayType: LowLevel.ctype =
            { size = arraySize, align = alignC, ffiType = ffiType }

        open Memory

        fun load(v: voidStar): string =
        let
            (* It should be null-terminated but just in case... *)
            fun sLen i = if i = Word.fromInt n orelse get8(v, i) = 0w0 then i else sLen(i+0w1)
            val length = sLen 0w0
            fun loadChar i =
                Char.chr(Word8.toInt(get8(v, Word.fromInt i)))
        in
            CharVector.tabulate(Word.toInt length, loadChar)
        end

        fun store(v: voidStar, s: string) =
        let
            (* The length must be less than the size to allow for the null *)
            val sLen = size s
            val _ = sLen < n orelse raise Fail "string too long"
        in
            CharVector.appi(fn(i, ch) => set8(v, Word.fromInt i, Word8.fromInt(Char.ord ch))) s;
            set8(v, Word.fromInt sLen, 0w0);
            fn () => ()
        end
    in
        makeConversion { load = load, store = store, ctype = arrayType }
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
    local
        val CP_ACP = 0 (* Default *)
        val WideCharToMultiByte = winCall8 (kernel "WideCharToMultiByte")
            (cUint, cDWORD, cByteArray, cInt, cPointer, cInt, cPointer, cPointer) cInt
        val MultiByteToWideChar =
            winCall6 (kernel "MultiByteToWideChar") (cUint, cDWORD, cString, cInt, cPointer, cInt) cInt
    in
        fun unicodeToString(w: Word8Vector.vector): string =
        let
            open Memory
            val inputLength = Word8Vector.length w  div 2 (* Number of unicode chars *)
            val outputLength =
                WideCharToMultiByte(CP_ACP, 0, w, inputLength, null, 0, null, null)
            val outputBuf = malloc(Word.fromInt outputLength)

            val conv = WideCharToMultiByte(CP_ACP, 0, w, inputLength, outputBuf, outputLength, null, null)

            fun loadChar i =
                Char.chr(Word8.toInt(get8(outputBuf, Word.fromInt i)))
        in
            (* We can't use fromCstring here because it's not necessarily null terminated. *)
            CharVector.tabulate(conv, loadChar) before free outputBuf
        end

        fun stringToUnicode(s: string): Word8Vector.vector =
        let
            open Memory
            val inputLength = size s + 1 (* Include terminating null *)
            (* Convert the whole string including the terminating null. *)
            val outputLength = MultiByteToWideChar(CP_ACP, 0, s, inputLength, null, 0)
            val outputBuf = malloc(Word.fromInt outputLength)
            val conv = MultiByteToWideChar(CP_ACP, 0, s, inputLength, outputBuf, outputLength)
            fun loadByte i = get8(outputBuf, Word.fromInt i)
        in
            Word8Vector.tabulate(conv, loadByte) before free outputBuf
        end
    end

end;
