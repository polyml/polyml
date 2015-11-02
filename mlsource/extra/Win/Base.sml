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
structure Base =
struct
local
    open Foreign
(*    val System_isShort : vol -> bool =
        RunCall.run_call1 RuntimeCalls.POLY_SYS_is_short*)
in
(*
    fun absConversion {abs,rep} C = 
    let val (fromC,toC,Ctype) = breakConversion C   
    in mkConversion (abs o fromC) (toC o rep) (Ctype)
    end*)

    fun absConversion {abs: 'a -> 'b, rep: 'b -> 'a} (c: 'a conversion) : 'b conversion =
    let
        val { load=loadI, store=storeI, release = releaseI, ctype } = c
        fun load m = abs(loadI m)
        fun store(m, v) = storeI(m, rep v)
        fun release(m, v) = releaseI(m, rep v)
    in
        { load = load, store = store, release = release, ctype = ctype }
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
   
    val cSIZE_T = cLong (* Not necessarily so. *)
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

    fun winCall0 args = call0withAbi winAbi args
    and winCall1 args = call1withAbi winAbi args
    and winCall2 args = call2withAbi winAbi args
    and winCall3 args = call3withAbi winAbi args
    and winCall4 args = call4withAbi winAbi args
    and winCall5 args = call5withAbi winAbi args
    and winCall6 args = call6withAbi winAbi args
    and winCall7 args = call7withAbi winAbi args
    and winCall8 args = call8withAbi winAbi args
    and winCall9 args = call9withAbi winAbi args
    and winCall10 args = call10withAbi winAbi args
    and winCall11 args = call11withAbi winAbi args
    and winCall12 args = call12withAbi winAbi args
    and winCall13 args = call13withAbi winAbi args
    and winCall14 args = call14withAbi winAbi args
    
    fun winFun0 args = cFunction0withAbi winAbi args
    and winFun1 args = cFunction1withAbi winAbi args
    and winFun2 args = cFunction2withAbi winAbi args
    and winFun3 args = cFunction3withAbi winAbi args
    and winFun4 args = cFunction4withAbi winAbi args
    and winFun5 args = cFunction5withAbi winAbi args
    and winFun6 args = cFunction6withAbi winAbi args

    (* Converter for a pointer to an item without update.  This should be in Foreign. *)
    local
        open Memory LowLevel
    in
        fun cConstStar ({load=loada, store=storea, release=releasea, ctype=ctypea}: 'a conversion): 'a conversion =
        let
            fun load s = loada(getAddress(s, 0w0))
            
            fun store(m, s) =
            let
                (* When we pass a ref X into a cStar cX function we need to
                   allocate a memory cell big enough for a cX value.  Then
                   we copy the current value of the ML into this.  We set
                   the argument, a pointer, to the address of the cell. *)
                val mem = malloc(#size ctypea)
                val () = setAddress(m, 0w0, mem)
            in
                storea(mem, s)
            end
            
            fun release(m, s) =
            let
                val mem = getAddress(m, 0w0) (* The address of our cell. *)
            in
                releasea(mem, s);
                free mem
            end
        in
            {load=load, store=store, release=release, ctype = cTypePointer}
        end
    end

    (* Previously we had a specific call to do this.  The error state is
       no longer set by the new FFI. *)
(*
    fun GetLastError(): OS.syserror =
        RunCall.run_call2 RuntimeCalls.POLY_SYS_os_specific (1100, ())
*)
    fun GetLastError(): OS.syserror =
    let
        val errorCode = winCall0 (kernel "GetLastError") () cDWORD ()
    in
        (* Windows error codes are negative values in OS.syserror. *)
        RunCall.unsafeCast (~ errorCode)
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
        val SIZE = absConversion {abs=mkSize, rep=breakSize} (cStruct2 (cLong,cLong))
    end

    (* Handles are generally opaque values. *)
    abstype 'a HANDLE = Hand of Memory.voidStar
    with
        val hNull = Hand Memory.null
        fun isHNull(Hand h) = Memory.voidStar2Sysword h = 0w0
        (* We sometimes need the next two functions internally.
           They're needed externally unless we change the result type
           of SendMessage to allow us to return a handle for certain
           messages. *)
        val handleOfVoidStar = Hand
        fun voidStarOfHandle(Hand n) = n
        fun handleOfInt n = Hand(Memory.sysWord2VoidStar(SysWord.fromInt n))
        fun intOfHandle(Hand n) = SysWord.toInt(Memory.voidStar2Sysword n)
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
    and Global = Global
    and Src = Src
    and Update = Update
    with
    end


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
        (* We need to use INT here rather than UINT to maintain
           compatibility with ApplicationInstance. *)
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
        val {store=storeS, load=loadS, release=releaseS, ctype} = cString

        fun storeClass(m, ClassAtom i) =
            if i >= 0 andalso i < 0xC000
            then setAddress(m, 0w0, sysWord2VoidStar(SysWord.fromInt i))
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
        
        fun releaseClass(_, ClassAtom _) = ()
        |   releaseClass(m, NamedClass s) = releaseS(m, s)

    in
        val cCLASS = { load = loadClass, store = storeClass, release = releaseClass, ctype = ctype }
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
            val cCLIPFORMAT = tableConversion(tab, SOME(fromInt, toInt))
        end

    (* Resources may be specified by strings or by ints.  Should this be an abstype? *)
    datatype RESID = IdAsInt of int | IdAsString of string

    local
        open Memory
        val {store=storeS, load=loadS, release=releaseS, ctype} = cString

        fun storeResid(m, IdAsInt i) =
            if i >= 0 andalso i < 65536
            then setAddress(m, 0w0, sysWord2VoidStar(SysWord.fromInt i))
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
        
        fun releaseResid(_, IdAsInt _) = ()
        |   releaseResid(m, IdAsString s) = releaseS(m, s)
    in
        val cRESID =
            { load = loadResid, store = storeResid, release = releaseResid, ctype = ctype }
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
            set8(v, Word.fromInt sLen, 0w0)
        end
    in
        { load = load, store = store, release = fn _ => (), ctype = arrayType }
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
(*
    (* Convert between strings and vectors containing Unicode characters.
       N.B.  These are not null terminated. *)
    fun unicodeToString(w: Word8Vector.vector): string =
    let
        val inputLength = Word8Vector.length w
        val WideCharToMultiByte = winCall8 (kernel "WideCharToMultiByte")
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
            winCall6 (kernel "MultiByteToWideChar") (INT, INT, STRING, INT, POINTER, INT) INT;
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
*)
end
end;
