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
(* Common dialogues. *)
structure CommonDialog :
  sig
    type HWND and HDC and COLORREF = Color.COLORREF and HINSTANCE
    type POINT = { x: int, y: int }
    type RECT =  { left: int, top: int, right: int, bottom: int }

    (* Error codes *)
    datatype CDERR =
            DIALOGFAILURE
        |   GENERALCODES
        |   STRUCTSIZE
        |   INITIALIZATION
        |   NOTEMPLATE
        |   NOHINSTANCE
        |   LOADSTRFAILURE
        |   FINDRESFAILURE
        |   LOADRESFAILURE
        |   LOCKRESFAILURE
        |   MEMALLOCFAILURE
        |   MEMLOCKFAILURE
        |   NOHOOK
        |   REGISTERMSGFAIL

        |   PRINTERCODES
        |   SETUPFAILURE
        |   PARSEFAILURE
        |   RETDEFFAILURE
        |   LOADDRVFAILURE
        |   GETDEVMODEFAIL
        |   INITFAILURE
        |   NODEVICES
        |   NODEFAULTPRN
        |   DNDMMISMATCH
        |   CREATEICFAILURE
        |   PRINTERNOTFOUND
        |   DEFAULTDIFFERENT

        |   CHOOSEFONTCODES
        |   NOFONTS
        |   MAXLESSTHANMIN

        |   FILENAMECODES
        |   SUBCLASSFAILURE
        |   INVALIDFILENAME
        |   BUFFERTOOSMALL

        |   FINDREPLACECODES
        |   BUFFERLENGTHZERO

        |   CHOOSECOLORCODES

    val CommDlgExtendedError : unit -> CDERR

    (* ChooseColor *)

    structure ChooseColorFlags :
      sig
        include BIT_FLAGS
        val CC_ANYCOLOR : flags
        val CC_FULLOPEN : flags
        val CC_PREVENTFULLOPEN : flags
        val CC_RGBINIT : flags
        val CC_SHOWHELP : flags
        val CC_SOLIDCOLOR : flags
      end

    type CHOOSECOLOR =
    {
        owner: HWND option,
        result: COLORREF,
        customColors: COLORREF list,
        flags: ChooseColorFlags.flags
    }

    val ChooseColor : CHOOSECOLOR -> CHOOSECOLOR option


    (* ChooseFont *)

    structure ChooseFontFlags :
      sig
        include BIT_FLAGS
        val CF_ANSIONLY : flags
        val CF_APPLY : flags
        val CF_BOTH : flags
        val CF_EFFECTS : flags
        val CF_FIXEDPITCHONLY : flags
        val CF_FORCEFONTEXIST : flags
        val CF_NOFACESEL : flags
        val CF_NOOEMFONTS : flags
        val CF_NOSCRIPTSEL : flags
        val CF_NOSIMULATIONS : flags
        val CF_NOSIZESEL : flags
        val CF_NOSTYLESEL : flags
        val CF_NOVECTORFONTS : flags
        val CF_NOVERTFONTS : flags
        val CF_PRINTERFONTS : flags
        val CF_SCALABLEONLY : flags
        val CF_SCREENFONTS : flags
        val CF_SCRIPTSONLY : flags
        val CF_SELECTSCRIPT : flags
        val CF_SHOWHELP : flags
        val CF_TTONLY : flags
        val CF_WYSIWYG : flags
      end

    structure ChooseFontTypes :
      sig
        include BIT_FLAGS
        val BOLD_FONTTYPE : flags
        val ITALIC_FONTTYPE : flags
        val PRINTER_FONTTYPE : flags
        val REGULAR_FONTTYPE : flags
        val SCREEN_FONTTYPE : flags
        val SIMULATED_FONTTYPE : flags
      end

    type CHOOSEFONT =
    {
        owner: HWND option,
        context: HDC option,
        logFont: Font.LOGFONT option,
        pointSize: int,
        flags: ChooseFontFlags.flags,
        colors: COLORREF,
        style: string option,
        fontType: ChooseFontTypes.flags,
        size: {min: int, max: int} option
    }

    val ChooseFont : CHOOSEFONT -> CHOOSEFONT option

    (* FindText and ReplaceText *)
    structure FindReplaceFlags :
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

    datatype
      TemplateType =
          TemplateDefault
        | TemplateHandle of Dialog.DLGTEMPLATE
        | TemplateResource of HINSTANCE * Resource.RESID

    type FINDREPLACE =
    {
        owner : HWND,
        template: TemplateType,
        flags: FindReplaceFlags.flags,
        findWhat: string,
        replaceWith: string,
        bufferSize: int
    }

    val FindText : FINDREPLACE -> HWND
    val ReplaceText : FINDREPLACE -> HWND
 

    (* GetOpenFileName and GetSaveFileName *)

    structure OpenFileFlags :
      sig
        include BIT_FLAGS
        val OFN_ALLOWMULTISELECT : flags
        val OFN_CREATEPROMPT : flags
        val OFN_EXPLORER : flags
        val OFN_EXTENSIONDIFFERENT : flags
        val OFN_FILEMUSTEXIST : flags
        val OFN_HIDEREADONLY : flags
        val OFN_LONGNAMES : flags
        val OFN_NOCHANGEDIR : flags
        val OFN_NODEREFERENCELINKS : flags
        val OFN_NOLONGNAMES : flags
        val OFN_NONETWORKBUTTON : flags
        val OFN_NOREADONLYRETURN : flags
        val OFN_NOTESTFILECREATE : flags
        val OFN_NOVALIDATE : flags
        val OFN_OVERWRITEPROMPT : flags
        val OFN_PATHMUSTEXIST : flags
        val OFN_READONLY : flags
        val OFN_SHAREAWARE : flags
        val OFN_SHOWHELP : flags
      end

    type OPENFILENAME =
    {
        owner: HWND option,
        template: TemplateType,
        filter: (string * string) list,
        customFilter: (string * string) option,
        filterIndex: int,
        file: string,   (* Initial value of file and returned result. *)
        maxFile: int,   (* Max size of expected file name. *)
        fileTitle : string,
        initialDir: string option,
        title: string option, (* Optional title - default is Save or Open. *)
        flags: OpenFileFlags.flags,
        defExt: string option
    }

    val GetFileTitle : string -> string
    val GetOpenFileName : OPENFILENAME -> OPENFILENAME option
    val GetSaveFileName : OPENFILENAME -> OPENFILENAME option

    (* PageSetupDlg *)
    structure PageSetupFlags :
      sig
        include BIT_FLAGS
        val PSD_DEFAULTMINMARGINS : flags
        val PSD_DISABLEMARGINS : flags
        val PSD_DISABLEORIENTATION : flags
        val PSD_DISABLEPAGEPAINTING : flags
        val PSD_DISABLEPAPER : flags
        val PSD_DISABLEPRINTER : flags
        val PSD_INHUNDREDTHSOFMILLIMETERS : flags
        val PSD_INTHOUSANDTHSOFINCHES : flags
        val PSD_MARGINS : flags
        val PSD_MINMARGINS : flags
        val PSD_NONETWORKBUTTON : flags
        val PSD_NOWARNING : flags
        val PSD_RETURNDEFAULT : flags
        val PSD_SHOWHELP : flags
      end

    type PAGESETUPDLG =
    {
        owner: HWND option,
        devMode: DeviceContext.DEVMODE option,
        devNames: DeviceContext.DEVNAMES option,
        flags: PageSetupFlags.flags,
        paperSize: POINT,
        minMargin: RECT,
        margin: RECT
        (* For the moment we ignore the other options. *)
    }

    val PageSetupDlg : PAGESETUPDLG -> PAGESETUPDLG option

    (* PrintDlg *)
    structure PrintDlgFlags :
      sig
        include BIT_FLAGS
        val PD_ALLPAGES : flags
        val PD_COLLATE : flags
        val PD_DISABLEPRINTTOFILE : flags
        val PD_HIDEPRINTTOFILE : flags
        val PD_NONETWORKBUTTON : flags
        val PD_NOPAGENUMS : flags
        val PD_NOSELECTION : flags
        val PD_NOWARNING : flags
        val PD_PAGENUMS : flags
        val PD_PRINTSETUP : flags
        val PD_PRINTTOFILE : flags
        val PD_RETURNDC : flags
        val PD_RETURNDEFAULT : flags
        val PD_RETURNIC : flags
        val PD_SELECTION : flags
        val PD_SHOWHELP : flags
        val PD_USEDEVMODECOPIES : flags
        val PD_USEDEVMODECOPIESANDCOLLATE : flags
     end

    type PRINTDLG =
    {
        owner: HWND option,
        devMode: DeviceContext.DEVMODE option,
        devNames: DeviceContext.DEVNAMES option,
        context: HDC option,
        flags: PrintDlgFlags.flags,
        fromPage: int,
        toPage: int,
        minPage: int,
        maxPage: int,
        copies: int
        (* For the moment we ignore the other options. *)
    }

    val PrintDlg : PRINTDLG -> PRINTDLG option
  end
 =
struct
    local
        open CInterface
        open Globals
        open Base
        open DeviceContext Color Font GdiBase
        

        (* Copy a string to a particular offset in a buffer and
           add a null terminator. *)
        fun stringToBuf (buf, n, s) =
        let
            fun copyToBuf (buf, n) (i, v): unit =
                assign Cchar (offset (n+i) Cchar buf) (toCchar v)
        in
            CharVector.appi (copyToBuf (buf, n)) s;
            assign Cchar (offset (n + size s) Cchar buf) (toCint 0)
        end

        fun allocAndInitialise(space: int, str: string): vol =
        let
            val space = Int.max(space, size str) + 1
            val buf = alloc space Cchar
        in
            stringToBuf(buf, 0, str);
            address buf
        end

        val (toHWND,  fromHWND, _)  = breakConversion HWND
        and (toRESID, fromRESID, _) = breakConversion RESID
        and (toHINST, fromHINST, _) = breakConversion HINSTANCE
        and (fromCDevMode, toCDevMode, _) = breakConversion DeviceBase.LPDEVMODE

    in
        type HWND = HWND and HDC = HDC and COLORREF = COLORREF and HINSTANCE = HINSTANCE
        type RECT = RECT and POINT = POINT

        datatype CDERR =
            DIALOGFAILURE    (* 0xffff *)
        |   GENERALCODES     (* 0x0000 *)
        |   STRUCTSIZE       (* 0x0001 *)
        |   INITIALIZATION   (* 0x0002 *)
        |   NOTEMPLATE       (* 0x0003 *)
        |   NOHINSTANCE      (* 0x0004 *)
        |   LOADSTRFAILURE   (* 0x0005 *)
        |   FINDRESFAILURE   (* 0x0006 *)
        |   LOADRESFAILURE   (* 0x0007 *)
        |   LOCKRESFAILURE   (* 0x0008 *)
        |   MEMALLOCFAILURE  (* 0x0009 *)
        |   MEMLOCKFAILURE   (* 0x000A *)
        |   NOHOOK           (* 0x000B *)
        |   REGISTERMSGFAIL  (* 0x000C *)

        |   PRINTERCODES     (* 0x1000 *)
        |   SETUPFAILURE     (* 0x1001 *)
        |   PARSEFAILURE     (* 0x1002 *)
        |   RETDEFFAILURE    (* 0x1003 *)
        |   LOADDRVFAILURE   (* 0x1004 *)
        |   GETDEVMODEFAIL   (* 0x1005 *)
        |   INITFAILURE      (* 0x1006 *)
        |   NODEVICES        (* 0x1007 *)
        |   NODEFAULTPRN     (* 0x1008 *)
        |   DNDMMISMATCH     (* 0x1009 *)
        |   CREATEICFAILURE  (* 0x100A *)
        |   PRINTERNOTFOUND  (* 0x100B *)
        |   DEFAULTDIFFERENT (* 0x100C *)

        |   CHOOSEFONTCODES  (* 0x2000 *)
        |   NOFONTS          (* 0x2001 *)
        |   MAXLESSTHANMIN   (* 0x2002 *)

        |   FILENAMECODES    (* 0x3000 *)
        |   SUBCLASSFAILURE  (* 0x3001 *)
        |   INVALIDFILENAME  (* 0x3002 *)
        |   BUFFERTOOSMALL   (* 0x3003 *)

        |   FINDREPLACECODES (* 0x4000 *)
        |   BUFFERLENGTHZERO (* 0x4001 *)

        |   CHOOSECOLORCODES (* 0x5000 *)


        fun CommDlgExtendedError () =
            case call0 (commdlg "CommDlgExtendedError") () INT () of
                0x0000  => GENERALCODES
            |   0x0001  => STRUCTSIZE
            
            |   0x0002  => INITIALIZATION
            |   0x0003  => NOTEMPLATE
            |   0x0004  => NOHINSTANCE
            |   0x0005  => LOADSTRFAILURE
            |   0x0006  => FINDRESFAILURE
            |   0x0007  => LOADRESFAILURE
            |   0x0008  => LOCKRESFAILURE
            |   0x0009  => MEMALLOCFAILURE
            |   0x000A  => MEMLOCKFAILURE
            |   0x000B  => NOHOOK
            |   0x000C  => REGISTERMSGFAIL
            
            |   0x1000  => PRINTERCODES
            |   0x1001  => SETUPFAILURE
            |   0x1002  => PARSEFAILURE
            |   0x1003  => RETDEFFAILURE
            |   0x1004  => LOADDRVFAILURE
            |   0x1005  => GETDEVMODEFAIL
            |   0x1006  => INITFAILURE
            |   0x1007  => NODEVICES
            |   0x1008  => NODEFAULTPRN
            |   0x1009  => DNDMMISMATCH
            |   0x100A  => CREATEICFAILURE
            |   0x100B  => PRINTERNOTFOUND
            |   0x100C  => DEFAULTDIFFERENT
            
            |   0x2000  => CHOOSEFONTCODES
            |   0x2001  => NOFONTS
            |   0x2002  => MAXLESSTHANMIN
            
            |   0x3000  => FILENAMECODES
            |   0x3001  => SUBCLASSFAILURE
            |   0x3002  => INVALIDFILENAME
            |   0x3003  => BUFFERTOOSMALL
            
            |   0x4000  => FINDREPLACECODES
            |   0x4001  => BUFFERLENGTHZERO
            |   _       => DIALOGFAILURE;

        (* As always there are a number of ways of matching the C types to
           ML.  Since functions such as GetOpenFileName update their
           parameters, probably the easiest way to deal with them is
           as functions which return an updated parameter set. *)
        datatype TemplateType =
            TemplateHandle of Dialog.DLGTEMPLATE
        |   TemplateResource of HINSTANCE * Resource.RESID
        |   TemplateDefault
    
        structure OpenFileFlags:>
          sig
            include BIT_FLAGS
            val OFN_ALLOWMULTISELECT : flags
            val OFN_CREATEPROMPT : flags
            val OFN_EXPLORER : flags
            val OFN_EXTENSIONDIFFERENT : flags
            val OFN_FILEMUSTEXIST : flags
            val OFN_HIDEREADONLY : flags
            val OFN_LONGNAMES : flags
            val OFN_NOCHANGEDIR : flags
            val OFN_NODEREFERENCELINKS : flags
            val OFN_NOLONGNAMES : flags
            val OFN_NONETWORKBUTTON : flags
            val OFN_NOREADONLYRETURN : flags
            val OFN_NOTESTFILECREATE : flags
            val OFN_NOVALIDATE : flags
            val OFN_OVERWRITEPROMPT : flags
            val OFN_PATHMUSTEXIST : flags
            val OFN_READONLY : flags
            val OFN_SHAREAWARE : flags
            val OFN_SHOWHELP : flags
          end
        =
        struct
            type flags = SysWord.word
            fun toWord f = f
            fun fromWord f = f
            val flags = List.foldl (fn (a, b) => SysWord.orb(a,b)) 0w0
            fun allSet (fl1, fl2) = SysWord.andb(fl1, fl2) = fl1
            fun anySet (fl1, fl2) = SysWord.andb(fl1, fl2) <> 0w0
            fun clear (fl1, fl2) = SysWord.andb(SysWord.notb fl1, fl2)
    
            val OFN_READONLY                 = 0wx00000001
            val OFN_OVERWRITEPROMPT          = 0wx00000002
            val OFN_HIDEREADONLY             = 0wx00000004
            val OFN_NOCHANGEDIR              = 0wx00000008
            val OFN_SHOWHELP                 = 0wx00000010
            val OFN_NOVALIDATE               = 0wx00000100
            val OFN_ALLOWMULTISELECT         = 0wx00000200
            val OFN_EXTENSIONDIFFERENT       = 0wx00000400
            val OFN_PATHMUSTEXIST            = 0wx00000800
            val OFN_FILEMUSTEXIST            = 0wx00001000
            val OFN_CREATEPROMPT             = 0wx00002000
            val OFN_SHAREAWARE               = 0wx00004000
            val OFN_NOREADONLYRETURN         = 0wx00008000
            val OFN_NOTESTFILECREATE         = 0wx00010000
            val OFN_NONETWORKBUTTON          = 0wx00020000
            val OFN_NOLONGNAMES              = 0wx00040000 (* force no long names for 4.x modules*)
            val OFN_EXPLORER                 = 0wx00080000 (* new look commdlg*)
            val OFN_NODEREFERENCELINKS       = 0wx00100000
            val OFN_LONGNAMES                = 0wx00200000 (* force long names for 3.x modules*)
    
            val all = flags[OFN_READONLY, OFN_OVERWRITEPROMPT, OFN_HIDEREADONLY,
                            OFN_NOCHANGEDIR, OFN_SHOWHELP,
                            OFN_NOVALIDATE, OFN_ALLOWMULTISELECT, OFN_EXTENSIONDIFFERENT,
                            OFN_PATHMUSTEXIST, OFN_FILEMUSTEXIST, OFN_CREATEPROMPT,
                            OFN_SHAREAWARE, OFN_NOREADONLYRETURN, OFN_NOTESTFILECREATE,
                            OFN_NONETWORKBUTTON, OFN_NOLONGNAMES, OFN_EXPLORER,
                            OFN_NODEREFERENCELINKS, OFN_LONGNAMES]
    
            val intersect = List.foldl (fn (a, b) => SysWord.andb(a,b)) all
        end

        (* These flags are local only. *)
        val OFN_ENABLEHOOK               = 0wx00000020 (* Never used. *)
        val OFN_ENABLETEMPLATE           = 0wx00000040
        val OFN_ENABLETEMPLATEHANDLE     = 0wx00000080

        type OPENFILENAME =
        {
            owner: HWND option,
            template: TemplateType,
            filter: (string * string) list,
            customFilter: (string * string) option,
            filterIndex: int,
            file: string,   (* Initial value of file and returned result. *)
            maxFile: int,   (* Max size of expected file name. *)
            fileTitle : string,
            initialDir: string option,
            title: string option, (* Optional title - default is Save or Open. *)
            flags: OpenFileFlags.flags,
            defExt: string option
        }

        local
            val OPENFILENAME = STRUCT20(UINT, HWNDOPT, POINTER, POINTER, POINTER, INT, INT,
                POINTER, INT, POINTER, INT, STRINGOPT, STRINGOPT, WORD, SHORT, SHORT,
                STRINGOPT, INT, INT, POINTER)
            val (toOFN, fromOFN, ofnStruct) = breakConversion OPENFILENAME

            fun toCOpenFileName(
                {
                    owner: HWND option,
                    template: TemplateType,
                    filter: (string * string) list,
                    customFilter: (string * string) option,
                    filterIndex: int,
                    file: string,
                    maxFile: int,
                    fileTitle : string,
                    initialDir: string option,
                    title: string option,
                    flags: OpenFileFlags.flags,
                    defExt: string option
                }:OPENFILENAME): vol =
            let
                val hw: vol = fromHWND(getOpt(owner, hwndNull))

                val (f1: LargeWord.word, inst: vol, templ: vol) =
                    case template of
                        TemplateHandle dlgTemp =>
                            (OFN_ENABLETEMPLATEHANDLE,
                             (* This is supposed to be a handle. *)
                             fromWord8vec(Dialog.compileTemplate dlgTemp),
                             toCint 0)
                    |   TemplateResource(hInst, resId) =>
                            (
                            OFN_ENABLETEMPLATE,
                            fromHINST hInst,
                            fromRESID resId
                            )
                    |   TemplateDefault => (0w0, toCint 0, toCint 0)

                local
                    (* The filter strings are pairs of strings with a final
                       terminating null.  That implies that the strings cannot be empty.
                       Should we check that?
                       Get the store needed for the strings, including the null
                       terminations and the final null. *)
                    val filterSize =
                        List.foldl (fn((s1,s2),n) => size s1 + size s2 + n + 2) 1 filter
                    val buf = alloc filterSize Cchar

                    fun copyToBuf((s1,s2), n) =
                    let
                        val ss1 = size s1 and ss2 = size s2
                    in
                        stringToBuf(buf, n, s1);
                        stringToBuf(buf, n+ss1+1, s2);
                        n+ss1+ss2+2 (* Result is the next offset. *)
                    end

                    val lastAddr = List.foldl copyToBuf 0 filter
                    val _ = assign Cchar (offset lastAddr Cchar buf) (toCint 0);
                in
                    val lpstrFilter =
                        case filter of
                            nil => toCint 0 (* Set it to null. *)
                        |   _ => address buf
                end

                val (lpstrCustomFilter, nMaxCustFilter) =
                    case customFilter of
                        NONE => (toCint 0, 0)
                    |   SOME (dispString, pattern) =>
                        let
                            (* Make sure we have enough space. 100 is probably big enough. *)
                            val space = Int.max(size dispString + size pattern + 2, 100)
                            val buf = alloc space Cchar
                        in
                            stringToBuf(buf, 0, dispString);
                            stringToBuf(buf, size dispString + 1, pattern);
                            (address buf, space)
                        end

                val lpstrFile = (* Full name of file including path. *)
                    allocAndInitialise(maxFile, file)
                val lpstrFileTitle = (* Name excluding the path. *)
                    allocAndInitialise(maxFile, fileTitle)
            in
                address(
                fromOFN(sizeof ofnStruct, (* lStructSize *)
                      owner, (* hwndOwner *)
                      inst, (* hInstance *)
                      lpstrFilter,
                      lpstrCustomFilter,
                      nMaxCustFilter,
                      filterIndex,
                      lpstrFile,
                      maxFile+1, (* nMaxFile *)
                      lpstrFileTitle,
                      maxFile+1, (* nMaxFileTitle *)
                      initialDir,
                      title,
                      LargeWord.orb(f1, OpenFileFlags.toWord flags), (* Flags *)
                      0, (* nFileOffset *)
                      0, (* nFileExtension *)
                      defExt,
                      0, (* lCustData *)
                      0, (* lpfnHook *)
                      templ)) (* lpTemplateName *)
            end

            (* Most of the fields are unchanged so we're better off extracting
               them from the original.  If we've passed in a template we have
               to get it from the original because we can only convert a
               memory object to a Word8Vector.vector if we know its length. *)
            fun fromCOpenFileName v
                ({ owner, template, filter, maxFile, initialDir,
                   title, defExt, ...}:OPENFILENAME): OPENFILENAME =
            let
                val (_, _, _, _, lpstrCustomFilter, _, nFilterIndex, lpstrFile,
                     _, lpstrFileTitle, _, _, _, flags, _, _, _, _, _, _) = toOFN(deref v)

                val customFilter =
                    if fromCint lpstrCustomFilter = 0
                    then NONE
                    else
                    let
                        val s1 = fromCstring lpstrCustomFilter
                        val s2 = fromCstring
                            (address(offset (size s1 +1) Cchar (deref lpstrCustomFilter)))
                    in
                        SOME(s1, s2)
                    end
            in
                {
                    owner = owner,
                    template = template,
                    filter = filter,
                    customFilter = customFilter,
                    filterIndex = nFilterIndex,
                    file = fromCstring lpstrFile,
                    maxFile = maxFile,
                    fileTitle = fromCstring lpstrFileTitle,
                    initialDir = initialDir,
                    title = title,
                        (* Mask off the template flags. *)
                    flags = OpenFileFlags.fromWord(LargeWord.andb(LargeWord.notb 0wxE0, flags)),
                    defExt = defExt
                }
            end
        in
    
            fun GetOpenFileName (arg: OPENFILENAME): OPENFILENAME option =
            let
                val converted = toCOpenFileName arg
                val result =
                    call1 (commdlg "GetOpenFileNameA") POINTER BOOL converted
            in
                if result
                then SOME(fromCOpenFileName converted arg)
                else NONE
            end
            and GetSaveFileName (arg: OPENFILENAME): OPENFILENAME option =
            let
                val converted = toCOpenFileName arg
                val result =
                    call1 (commdlg "GetSaveFileNameA") POINTER BOOL converted
            in
                if result
                then SOME(fromCOpenFileName converted arg)
                else NONE
            end
        end (* local *)

        fun GetFileTitle(file: string): string =
        let
            val gft = call3(commdlg "GetFileTitleA") (STRING, POINTER, SHORT) SHORT
            val buffsize = gft(file, toCint 0, 0)
            val _ = checkResult(buffsize >= 0)
            val buf = alloc buffsize Cchar
            val result = gft(file, address buf, buffsize)
        in
            checkResult(result >= 0);
            fromCstring(address buf)
        end


        (* This is a bit messy.  It creates a modeless dialogue box
           and sends messages to the parent window.  The only problem is that
           the message identifier is not a constant.  It has to be obtained
           by a call to RegisterWindowMessage. *)
        (* We also have to ensure that the "vol" containing the FINDREPLACE
           structure is not freed until the dialogue window is destroyed. *)

        structure FindReplaceFlags = FindReplaceFlags

        (* These flags are local only. *)
        val FR_ENABLEHOOK                 = 0wx00000100
        val FR_ENABLETEMPLATE             = 0wx00000200
        val FR_ENABLETEMPLATEHANDLE       = 0wx00002000

        (* The address of this structure is passed in messages.  That all looks
           extremely messy. *)
        type FINDREPLACE =
        {
            owner : HWND, (* NOT an option. *)
            template: TemplateType,
            flags: FindReplaceFlags.flags,
            findWhat: string,
            replaceWith: string,
            bufferSize: int
        }

        local
            val FINDREPLACE = STRUCT11(UINT, HWND, POINTER, WORD, POINTER, POINTER,
                SHORT, SHORT, INT, INT, POINTER)
            val (toOFR, fromOFR, ofrStruct) = breakConversion FINDREPLACE

            fun toCFindReplace(
                    {
                        owner : HWND, (* NOT an option. *)
                        template: TemplateType,
                        flags: FindReplaceFlags.flags,
                        findWhat: string,
                        replaceWith: string,
                        bufferSize: int
                    }: FINDREPLACE): vol =
            let
                val (f1: LargeWord.word, inst: vol, templ: vol) =
                    case template of
                        TemplateHandle dlgTemp =>
                            (FR_ENABLETEMPLATEHANDLE,
                             (* This is supposed to be a handle. *)
                             fromWord8vec(Dialog.compileTemplate dlgTemp),
                             toCint 0)
                    |   TemplateResource(hInst, resId) =>
                            (
                            FR_ENABLETEMPLATE,
                            fromHINST hInst,
                            fromRESID resId
                            )
                    |   TemplateDefault => (0w0, toCint 0, toCint 0)
                val lpstrFindWhat = allocAndInitialise(bufferSize, findWhat)
                val lpstrReplaceWith = allocAndInitialise(bufferSize, replaceWith)
            in
                address(
                fromOFR(sizeof ofrStruct, (* lStructSize *)
                      owner, (* hwndOwner *)
                      inst, (* hInstance *)
                      LargeWord.orb(f1, FindReplaceFlags.toWord flags), (* Flags *)
                      lpstrFindWhat,
                      lpstrReplaceWith,
                      bufferSize,
                      bufferSize,
                      0, (* lCustData *)
                      0, (* lpfnHook *)
                      templ)) (* lpTemplateName *)
            end

            fun findReplace name (arg: FINDREPLACE): HWND =
            let
                val converted = toCFindReplace arg
                val result =
                    call1 (commdlg (name ^"A")) POINTER HWND converted
            in
                checkResult(not(isHNull result));
                (* We need to keep hold of the vol corresponding to the
                    FINDREPLACE structure otherwise it may be garbage-
                    collected away. Also, since this is a modeless dialogue
                    we have to add it to the modeless dialogue list so
                    that keyboard functions work. *)
                (Message.addModelessDialogue(result, converted); result)
            end
        in
            val FindText = findReplace "FindText"
            and ReplaceText = findReplace "ReplaceText"
        end

        structure PageSetupFlags :>
          sig
            include BIT_FLAGS
            val PSD_DEFAULTMINMARGINS : flags
            val PSD_DISABLEMARGINS : flags
            val PSD_DISABLEORIENTATION : flags
            val PSD_DISABLEPAGEPAINTING : flags
            val PSD_DISABLEPAPER : flags
            val PSD_DISABLEPRINTER : flags
            val PSD_INHUNDREDTHSOFMILLIMETERS : flags
            val PSD_INTHOUSANDTHSOFINCHES : flags
            val PSD_MARGINS : flags
            val PSD_MINMARGINS : flags
            val PSD_NONETWORKBUTTON : flags
            val PSD_NOWARNING : flags
            val PSD_RETURNDEFAULT : flags
            val PSD_SHOWHELP : flags
          end
         =
        struct
            type flags = SysWord.word
            fun toWord f = f
            fun fromWord f = f
            val flags = List.foldl (fn (a, b) => SysWord.orb(a,b)) 0w0
            fun allSet (fl1, fl2) = SysWord.andb(fl1, fl2) = fl1
            fun anySet (fl1, fl2) = SysWord.andb(fl1, fl2) <> 0w0
            fun clear (fl1, fl2) = SysWord.andb(SysWord.notb fl1, fl2)
    
            val PSD_DEFAULTMINMARGINS             = 0wx00000000 (* default (printer's) *)
            (*val PSD_INWININIINTLMEASURE           = 0wx00000000 *)(* 1st of 4 possible *)
            
            val PSD_MINMARGINS                    = 0wx00000001 (* use caller's *)
            val PSD_MARGINS                       = 0wx00000002 (* use caller's *)
            val PSD_INTHOUSANDTHSOFINCHES         = 0wx00000004 (* 2nd of 4 possible *)
            val PSD_INHUNDREDTHSOFMILLIMETERS     = 0wx00000008 (* 3rd of 4 possible *)
            val PSD_DISABLEMARGINS                = 0wx00000010
            val PSD_DISABLEPRINTER                = 0wx00000020
            val PSD_NOWARNING                     = 0wx00000080
            val PSD_DISABLEORIENTATION            = 0wx00000100
            val PSD_RETURNDEFAULT                 = 0wx00000400
            val PSD_DISABLEPAPER                  = 0wx00000200
            val PSD_SHOWHELP                      = 0wx00000800
            (*
            val PSD_ENABLEPAGESETUPHOOK           = 0wx00002000
            val PSD_ENABLEPAGESETUPTEMPLATE       = 0wx00008000
            val PSD_ENABLEPAGESETUPTEMPLATEHANDLE = 0wx00020000
            val PSD_ENABLEPAGEPAINTHOOK           = 0wx00040000 *)

            val PSD_DISABLEPAGEPAINTING           = 0wx00080000
            val PSD_NONETWORKBUTTON               = 0wx00200000
    
            val all = flags[PSD_DEFAULTMINMARGINS, PSD_MINMARGINS, PSD_MARGINS,
                            PSD_INTHOUSANDTHSOFINCHES, PSD_INHUNDREDTHSOFMILLIMETERS,
                            PSD_DISABLEMARGINS, PSD_DISABLEPRINTER, PSD_NOWARNING,
                            PSD_DISABLEORIENTATION, PSD_RETURNDEFAULT, PSD_DISABLEPAPER,
                            PSD_SHOWHELP, PSD_DISABLEPAGEPAINTING, PSD_NONETWORKBUTTON]
    
            val intersect = List.foldl (fn (a, b) => SysWord.andb(a,b)) all
        end

        structure PrintDlgFlags :>
          sig
            include BIT_FLAGS
            val PD_ALLPAGES : flags
            val PD_COLLATE : flags
            val PD_DISABLEPRINTTOFILE : flags
            val PD_HIDEPRINTTOFILE : flags
            val PD_NONETWORKBUTTON : flags
            val PD_NOPAGENUMS : flags
            val PD_NOSELECTION : flags
            val PD_NOWARNING : flags
            val PD_PAGENUMS : flags
            val PD_PRINTSETUP : flags
            val PD_PRINTTOFILE : flags
            val PD_RETURNDC : flags
            val PD_RETURNDEFAULT : flags
            val PD_RETURNIC : flags
            val PD_SELECTION : flags
            val PD_SHOWHELP : flags
            val PD_USEDEVMODECOPIES : flags
            val PD_USEDEVMODECOPIESANDCOLLATE : flags
          end
     =
        struct
            type flags = SysWord.word
            fun toWord f = f
            fun fromWord f = f
            val flags = List.foldl (fn (a, b) => SysWord.orb(a,b)) 0w0
            fun allSet (fl1, fl2) = SysWord.andb(fl1, fl2) = fl1
            fun anySet (fl1, fl2) = SysWord.andb(fl1, fl2) <> 0w0
            fun clear (fl1, fl2) = SysWord.andb(SysWord.notb fl1, fl2)
    
            val PD_ALLPAGES                  = 0wx00000000
            val PD_SELECTION                 = 0wx00000001
            val PD_PAGENUMS                  = 0wx00000002
            val PD_NOSELECTION               = 0wx00000004
            val PD_NOPAGENUMS                = 0wx00000008
            val PD_COLLATE                   = 0wx00000010
            val PD_PRINTTOFILE               = 0wx00000020
            val PD_PRINTSETUP                = 0wx00000040
            val PD_NOWARNING                 = 0wx00000080
            val PD_RETURNDC                  = 0wx00000100
            val PD_RETURNIC                  = 0wx00000200
            val PD_RETURNDEFAULT             = 0wx00000400
            val PD_SHOWHELP                  = 0wx00000800
            (*val PD_ENABLEPRINTHOOK           = 0wx00001000
            val PD_ENABLESETUPHOOK           = 0wx00002000
            val PD_ENABLEPRINTTEMPLATE       = 0wx00004000
            val PD_ENABLESETUPTEMPLATE       = 0wx00008000
            val PD_ENABLEPRINTTEMPLATEHANDLE = 0wx00010000
            val PD_ENABLESETUPTEMPLATEHANDLE = 0wx00020000 *)
            val PD_USEDEVMODECOPIES          = 0wx00040000
            val PD_USEDEVMODECOPIESANDCOLLATE = 0wx00040000
            val PD_DISABLEPRINTTOFILE        = 0wx00080000
            val PD_HIDEPRINTTOFILE           = 0wx00100000
            val PD_NONETWORKBUTTON           = 0wx00200000

    
            val all = flags[PD_ALLPAGES, PD_SELECTION, PD_PAGENUMS, PD_NOSELECTION, PD_NOPAGENUMS,
                            PD_COLLATE, PD_PRINTTOFILE, PD_PRINTSETUP, PD_NOWARNING, PD_RETURNDC,
                            PD_RETURNIC, PD_RETURNDEFAULT, PD_SHOWHELP, PD_USEDEVMODECOPIES,
                            PD_USEDEVMODECOPIESANDCOLLATE, PD_DISABLEPRINTTOFILE,
                            PD_HIDEPRINTTOFILE, PD_NONETWORKBUTTON]
    
            val intersect = List.foldl (fn (a, b) => SysWord.andb(a,b)) all
        end

        type PAGESETUPDLG =
        {
            owner: HWND option,
            devMode: DEVMODE option,
            devNames: DEVNAMES option,
            flags: PageSetupFlags.flags,
            paperSize: POINT,
            minMargin: RECT,
            margin: RECT
            (* For the moment we ignore the other options. *)
        }

        type PRINTDLG =
        {
            owner: HWND option,
            devMode: DEVMODE option,
            devNames: DEVNAMES option,
            context: HDC option,
            flags: PrintDlgFlags.flags,
            fromPage: int,
            toPage: int,
            minPage: int,
            maxPage: int,
            copies: int
            (* For the moment we ignore the other options. *)
        }

        local

            val PAGESETUPDLG = STRUCT14(UINT, HWNDOPT, HGLOBAL, HGLOBAL, WORD, POINT,
                                    RECT, RECT, HINSTANCE, INT, INT, INT, INT, INT)
            val (toPSD, fromPSD, psdStruct) = breakConversion PAGESETUPDLG

            (* A DEVNAMES structure is a structure containing offsets followed by
               the actual strings. *)
            val DEVNAMES = STRUCT4(SHORT, SHORT, SHORT, SHORT)
            val (toDN, fromDN, dnStruct) = breakConversion DEVNAMES
            val DN_DEFAULTPRN      = 0x0001

            fun toDevNames NONE = hglobalNull
            |   toDevNames (SOME{driver, device, output, default}) =
                let
                    (* We need memory for the DEVNAMES structure plus the strings plus
                       their terminating nulls. *)
                    val devnameSize = sizeof dnStruct
                    val sizeDriver = size driver
                    and sizeDevice = size device
                    and sizeOutput = size output
                    val space = devnameSize + sizeDriver + sizeDevice + sizeOutput + 3
                    val mHandle = GlobalAlloc(0, space)
                    val buff = deref(GlobalLock mHandle)
                    (* Copy in the strings and calculate the next offset. *)
                    fun copyString b str =
                    (
                        fillCstring b str;
                        offset (size str+1) Cchar b
                    );
                    val off1 = copyString (offset 1 dnStruct buff) driver;
                    val off2 = copyString off1 device
                    val _ = copyString off2 output
                in
                    assign dnStruct buff
                        (fromDN(devnameSize, devnameSize+sizeDriver+1,
                                devnameSize+sizeDriver+sizeDevice+2,
                                if default then DN_DEFAULTPRN else 0));
                    GlobalUnlock mHandle;
                    mHandle
                end

            (* Convert a DevNames structure.  Also frees the handle if it's not null. *)
            fun fromDevNames v =
                if isHglobalNull v then NONE
                else
                let
                    val buff = deref(GlobalLock v)
                    val (off0, off1, off2, def) = toDN buff
                    val driver = fromCstring(address(offset off0 Cchar buff))
                    val device = fromCstring(address(offset off1 Cchar buff))
                    val output = fromCstring(address(offset off2 Cchar buff))
                    val default = IntInf.andb(def, DN_DEFAULTPRN) <> 0
                in
                    GlobalUnlock v;
                    GlobalFree v;
                    SOME {driver=driver, device=device, output=output, default=default}
                end

            fun toCPageSetupDlg({
                owner: HWND option,
                devMode: DEVMODE option,
                devNames: {driver: string, device: string, output: string, default: bool} option,
                flags: PageSetupFlags.flags,
                paperSize: POINT,
                minMargin: RECT,
                margin: RECT}: PAGESETUPDLG) : vol =
            let
                val devnames = toDevNames devNames
                val devmode =
                    case devMode of
                        NONE => hglobalNull
                    |   SOME dv =>
                        let
                            val dev = deref(toCDevMode dv)
                            (* toCDevMode constructs the structure in local memory.
                               We have to copy it to store allocated with GlobalAlloc. *)
                            val size = fromCshort(offset 36 Cchar dev) +
                                       fromCshort(offset 38 Cchar dev)
                            val hGlob = GlobalAlloc(0, size)
                            val mem = deref(GlobalLock hGlob)
                            fun doCopy t f 0 = ()
                             |  doCopy t f i =
                                (
                                 assign Cchar t f;
                                 doCopy(offset 1 Cchar t) (offset 1 Cchar f) (i-1)
                                )
                        in
                            doCopy mem dev size;
                            GlobalUnlock hGlob;
                            hGlob
                        end
            in
                address(
                    fromPSD (sizeof psdStruct, owner, devmode, devnames, PageSetupFlags.toWord flags,
                        paperSize, minMargin, margin, hinstanceNull, 0, 0, 0, 0, 0 ) )
            end

            fun fromCPageSetupDlg v : PAGESETUPDLG =
            let
                val (_, owner, hgDevMode, hgDevNames, flags, paperSize, minMargin, margin,
                     _, _, _, _, _, _) = toPSD(deref v)
                val devMode =
                    if isHglobalNull hgDevMode
                    then NONE
                    else let
                        
                        val r = SOME(fromCDevMode(GlobalLock hgDevMode))
                    in
                        GlobalUnlock hgDevMode;
                        GlobalFree hgDevMode;
                        r
                    end;
                val devNames = fromDevNames hgDevNames
            in
                { owner = owner, devMode = devMode, devNames = devNames,
                  flags = PageSetupFlags.fromWord flags,
                  paperSize = paperSize, minMargin = minMargin, margin = margin }
            end


            (* This is a bit of a mess.  It turns out that the fields after the five
               shorts are not aligned onto 4-byte boundaries. Since we don't currently use
               them this doesn't matter except that we have to set the size explicitly. *)
            val PRINTDLG = STRUCT19(UINT, HWNDOPT, HGLOBAL, HGLOBAL, HDC, WORD, SHORT,
                                    SHORT, SHORT, SHORT, SHORT, INT, INT, INT,
                                    INT, INT, INT, INT, INT)
            val (toPRD, fromPRD, _) = breakConversion PRINTDLG
            val printDlgSize = 66

            fun toCPrintDlg({
                owner: HWND option,
                devMode: DEVMODE option,
                devNames: {driver: string, device: string, output: string, default: bool} option,
                context: HDC option,
                flags: PrintDlgFlags.flags,
                fromPage: int,
                toPage: int,
                minPage: int,
                maxPage: int,
                copies: int}: PRINTDLG) : vol =
            let
                val devnames = toDevNames devNames
                val devmode =
                    case devMode of
                        NONE => hglobalNull
                    |   SOME dv =>
                        let
                            val dev = deref(toCDevMode dv)
                            (* toCDevMode constructs the structure in local memory.
                               We have to copy it to store allocated with GlobalAlloc. *)
                            val size = fromCshort(offset 36 Cchar dev) +
                                       fromCshort(offset 38 Cchar dev)
                            val hGlob = GlobalAlloc(0, size)
                            val mem = deref(GlobalLock hGlob)
                            fun doCopy t f 0 = ()
                             |  doCopy t f i =
                                (
                                 assign Cchar t f;
                                 doCopy(offset 1 Cchar t) (offset 1 Cchar f) (i-1)
                                )
                        in
                            doCopy mem dev size;
                            GlobalUnlock hGlob;
                            hGlob
                        end
            in
                address(
                    fromPRD (printDlgSize, owner, devmode, devnames, getOpt(context, hdcNull),
                        PrintDlgFlags.toWord flags, fromPage, toPage, minPage, maxPage, copies,
                        0, 0, 0, 0, 0, 0, 0, 0 ) )
            end

            fun fromCPrintDlg v : PRINTDLG =
            let
                val (_, owner, hgDevMode, hgDevNames, hdc, flags, fromPage, toPage, minPage,
                     maxPage, copies, _, _, _, _, _, _, _, _) = toPRD(deref v)
                val devMode =
                    if isHglobalNull hgDevMode
                    then NONE
                    else let
                        
                        val r = SOME(fromCDevMode(GlobalLock hgDevMode))
                    in
                        GlobalUnlock hgDevMode;
                        GlobalFree hgDevMode;
                        r
                    end;
                val devNames = fromDevNames hgDevNames
            in
                { owner = owner, devMode = devMode, devNames = devNames,
                  context = if isHdcNull hdc then NONE else SOME hdc,
                  flags = PrintDlgFlags.fromWord flags, fromPage = fromPage, toPage = toPage,
                  minPage = minPage, maxPage = maxPage, copies = copies }
            end

        in
            fun PageSetupDlg (arg: PAGESETUPDLG): PAGESETUPDLG option =
            let
                val converted = toCPageSetupDlg arg
                val result =
                    call1 (commdlg "PageSetupDlgA") POINTER BOOL converted
                (* Convert the result.  We have to do this even if the result is
                   false to make sure we call GlobalFree on any global handles. *)
                val newArg = fromCPageSetupDlg converted
            in
                if result
                then SOME newArg
                else NONE
            end

            and PrintDlg (arg: PRINTDLG): PRINTDLG option =
            let
                val converted = toCPrintDlg arg
                val result =
                    call1 (commdlg "PrintDlgA") POINTER BOOL converted
                (* Convert the result.  We have to do this even if the result is
                   false to make sure we call GlobalFree on any global handles. *)
                val newArg = fromCPrintDlg converted
            in
                if result
                then SOME newArg
                else NONE
            end
        end

        structure ChooseFontFlags :>
          sig
            include BIT_FLAGS
            val CF_ANSIONLY : flags
            val CF_APPLY : flags
            val CF_BOTH : flags
            val CF_EFFECTS : flags
            val CF_FIXEDPITCHONLY : flags
            val CF_FORCEFONTEXIST : flags
            val CF_NOFACESEL : flags
            val CF_NOOEMFONTS : flags
            val CF_NOSCRIPTSEL : flags
            val CF_NOSIMULATIONS : flags
            val CF_NOSIZESEL : flags
            val CF_NOSTYLESEL : flags
            val CF_NOVECTORFONTS : flags
            val CF_NOVERTFONTS : flags
            val CF_PRINTERFONTS : flags
            val CF_SCALABLEONLY : flags
            val CF_SCREENFONTS : flags
            val CF_SCRIPTSONLY : flags
            val CF_SELECTSCRIPT : flags
            val CF_SHOWHELP : flags
            val CF_TTONLY : flags
            val CF_WYSIWYG : flags
          end
     =
        struct
            type flags = SysWord.word
            fun toWord f = f
            fun fromWord f = f
            val flags = List.foldl (fn (a, b) => SysWord.orb(a,b)) 0w0
            fun allSet (fl1, fl2) = SysWord.andb(fl1, fl2) = fl1
            fun anySet (fl1, fl2) = SysWord.andb(fl1, fl2) <> 0w0
            fun clear (fl1, fl2) = SysWord.andb(SysWord.notb fl1, fl2)
    
            val CF_SCREENFONTS             = 0wx00000001
            val CF_PRINTERFONTS            = 0wx00000002
            val CF_BOTH                    = 0wx00000003: flags
            val CF_SHOWHELP                = 0wx00000004
            (*
            val CF_ENABLEHOOK              = 0wx00000008
            val CF_ENABLETEMPLATE          = 0wx00000010
            val CF_ENABLETEMPLATEHANDLE    = 0wx00000020
            *)
            (*val CF_INITTOLOGFONTSTRUCT     = 0wx00000040*)
            (*val CF_USESTYLE                = 0wx00000080*)
            val CF_EFFECTS                 = 0wx00000100
            val CF_APPLY                   = 0wx00000200
            val CF_ANSIONLY                = 0wx00000400
            val CF_SCRIPTSONLY             = CF_ANSIONLY
            val CF_NOVECTORFONTS           = 0wx00000800
            val CF_NOOEMFONTS              = CF_NOVECTORFONTS
            val CF_NOSIMULATIONS           = 0wx00001000
            (*val CF_LIMITSIZE               = 0wx00002000*)
            val CF_FIXEDPITCHONLY          = 0wx00004000
            val CF_WYSIWYG                 = 0wx00008000
            val CF_FORCEFONTEXIST          = 0wx00010000
            val CF_SCALABLEONLY            = 0wx00020000
            val CF_TTONLY                  = 0wx00040000
            val CF_NOFACESEL               = 0wx00080000
            val CF_NOSTYLESEL              = 0wx00100000
            val CF_NOSIZESEL               = 0wx00200000
            val CF_SELECTSCRIPT            = 0wx00400000
            val CF_NOSCRIPTSEL             = 0wx00800000
            val CF_NOVERTFONTS             = 0wx01000000
    
            val all = flags[CF_SCREENFONTS, CF_PRINTERFONTS, CF_SHOWHELP,
                            CF_EFFECTS, CF_APPLY, CF_ANSIONLY, CF_NOVECTORFONTS,
                            CF_NOSIMULATIONS, CF_FIXEDPITCHONLY, CF_WYSIWYG, CF_FORCEFONTEXIST,
                            CF_SCALABLEONLY, CF_TTONLY, CF_NOFACESEL, CF_NOSTYLESEL, CF_NOSIZESEL,
                            CF_SELECTSCRIPT, CF_NOSCRIPTSEL, CF_NOVERTFONTS]
    
            val intersect = List.foldl (fn (a, b) => SysWord.andb(a,b)) all
        end

        structure ChooseFontTypes :>
          sig
            include BIT_FLAGS
            val BOLD_FONTTYPE : flags
            val ITALIC_FONTTYPE : flags
            val PRINTER_FONTTYPE : flags
            val REGULAR_FONTTYPE : flags
            val SCREEN_FONTTYPE : flags
            val SIMULATED_FONTTYPE : flags
          end
     =
        struct
            type flags = SysWord.word
            fun toWord f = f
            fun fromWord f = f
            val flags = List.foldl (fn (a, b) => SysWord.orb(a,b)) 0w0
            fun allSet (fl1, fl2) = SysWord.andb(fl1, fl2) = fl1
            fun anySet (fl1, fl2) = SysWord.andb(fl1, fl2) <> 0w0
            fun clear (fl1, fl2) = SysWord.andb(SysWord.notb fl1, fl2)
    
            val SIMULATED_FONTTYPE    = 0wx8000
            val PRINTER_FONTTYPE      = 0wx4000
            val SCREEN_FONTTYPE       = 0wx2000
            val BOLD_FONTTYPE         = 0wx0100
            val ITALIC_FONTTYPE       = 0wx0200
            val REGULAR_FONTTYPE      = 0wx0400
    
            val all = flags[SIMULATED_FONTTYPE, PRINTER_FONTTYPE, SCREEN_FONTTYPE,
                            BOLD_FONTTYPE, ITALIC_FONTTYPE, REGULAR_FONTTYPE]
    
            val intersect = List.foldl (fn (a, b) => SysWord.andb(a,b)) all
        end

        type CHOOSEFONT = {
            owner: HWND option,
            context: HDC option,
            logFont: LOGFONT option,
            pointSize: int,
            flags: ChooseFontFlags.flags,
            colors: COLORREF,
            style: string option,
            fontType: ChooseFontTypes.flags,
            size: {min: int, max: int} option
            }

        local
            val CHOOSEFONT = STRUCT16(UINT, HWNDOPT, HDC, POINTER, INT, WORD, COLORREF,
                                INT, INT, INT, INT, POINTER, SHORT, SHORT, INT, INT)
            val (toCF, fromCF, cfStruct) = breakConversion CHOOSEFONT
            val (toLF, fromLF, lfStruct) = breakConversion FontBase.LOGFONT
            val CF_LIMITSIZE               = 0wx00002000
            val CF_INITTOLOGFONTSTRUCT     = 0wx00000040
            val CF_USESTYLE                = 0wx00000080

            fun toCChooseFont({
                owner: HWND option,
                context: HDC option,
                logFont: LOGFONT option,
                pointSize: int,
                flags: ChooseFontFlags.flags,
                colors: COLORREF,
                style: string option,
                fontType: ChooseFontTypes.flags,
                size: {min: int, max: int} option
                }) =
            let
                (* Use the supplied logFont otherwise allocate store for a new one. *)
                val logf =
                    case logFont of
                        SOME logf => address(fromLF logf)
                    |   NONE => address(alloc 1 lfStruct)
                (* Copy any style to the buffer - I don't know why this is 64. *)
                val lpszStyle = allocAndInitialise(64, getOpt(style, ""))
                val (min, max) = case size of SOME {min, max} => (min, max) | NONE => (0,0)
                val f1 = case size of SOME _ => CF_LIMITSIZE | _ => 0w0
                val f2 = case logFont of SOME _ => CF_INITTOLOGFONTSTRUCT | _ => 0w0
                val f3 = case style of SOME _ => CF_USESTYLE | _ => 0w0
                val flags = List.foldl LargeWord.orb 0w0 [ChooseFontFlags.toWord flags, f1, f2, f3]
            in
                address(
                    fromCF(sizeof cfStruct, owner, getOpt(context, hdcNull), logf, pointSize,
                        flags, colors, 0, 0, 0, 0, lpszStyle,
                        LargeWord.toInt (ChooseFontTypes.toWord fontType), 0, min, max))
            end

            fun fromCChooseFont v : CHOOSEFONT =
            let
                val (_, owner, hdc, logf, pointSize, flags, colors, _, _, _, _, style,
                     types, _, min, max) = toCF(deref v)
                val minMax =
                    if LargeWord.andb(flags, CF_LIMITSIZE) = 0w0
                    then NONE
                    else SOME{min=min, max=max}
                val style =
                    if LargeWord.andb(flags, CF_USESTYLE) = 0w0
                    then NONE
                    else SOME(fromCstring style)
            in
                { owner = owner, context = if isHdcNull hdc then NONE else SOME hdc,
                  logFont = SOME(toLF(deref logf)), pointSize = pointSize,
                  (* Remove CF_LIMITSIZE and/or CF_INITTOLOGFONTSTRUCT *)
                  flags = ChooseFontFlags.intersect[ChooseFontFlags.fromWord flags],
                  colors = colors, style = style,
                  fontType =
                     ChooseFontTypes.fromWord(LargeWord.andb(LargeWord.fromInt types, 0wxffff)),
                  size = minMax}
            end
        in
            fun ChooseFont (arg: CHOOSEFONT): CHOOSEFONT option =
            let
                val converted = toCChooseFont arg
                val result =
                    call1 (commdlg "ChooseFontA") POINTER BOOL converted
            in
                if result
                then SOME(fromCChooseFont converted)
                else NONE
            end

        end

        structure ChooseColorFlags :>
          sig
            include BIT_FLAGS
            val CC_ANYCOLOR : flags
            val CC_FULLOPEN : flags
            val CC_PREVENTFULLOPEN : flags
            val CC_RGBINIT : flags
            val CC_SHOWHELP : flags
            val CC_SOLIDCOLOR : flags
          end
     =
        struct
            type flags = SysWord.word
            fun toWord f = f
            fun fromWord f = f
            val flags = List.foldl (fn (a, b) => SysWord.orb(a,b)) 0w0
            fun allSet (fl1, fl2) = SysWord.andb(fl1, fl2) = fl1
            fun anySet (fl1, fl2) = SysWord.andb(fl1, fl2) <> 0w0
            fun clear (fl1, fl2) = SysWord.andb(SysWord.notb fl1, fl2)
    
            val CC_RGBINIT               = 0wx00000001
            val CC_FULLOPEN              = 0wx00000002
            val CC_PREVENTFULLOPEN       = 0wx00000004
            val CC_SHOWHELP              = 0wx00000008
            (*val CC_ENABLEHOOK            = 0wx00000010
            val CC_ENABLETEMPLATE        = 0wx00000020
            val CC_ENABLETEMPLATEHANDLE  = 0wx00000040*)
            val CC_SOLIDCOLOR            = 0wx00000080
            val CC_ANYCOLOR              = 0wx00000100
    
            val all = flags[CC_RGBINIT, CC_FULLOPEN, CC_PREVENTFULLOPEN,
                            CC_SHOWHELP, CC_SOLIDCOLOR, CC_ANYCOLOR]
    
            val intersect = List.foldl (fn (a, b) => SysWord.andb(a,b)) all
        end

        type CHOOSECOLOR =
        {
            owner: HWND option,
            result: COLORREF,
            customColors: COLORREF list,
            flags: ChooseColorFlags.flags
        }

        local
            val CHOOSECOLOR = STRUCT9(UINT, HWNDOPT, INT, COLORREF, POINTER, WORD,
                                      INT, INT, INT)
            (* The custom colours are held in an array of 16 elements. *)
            val CUSTOM = STRUCT16(COLORREF, COLORREF, COLORREF, COLORREF,
                                  COLORREF, COLORREF, COLORREF, COLORREF, 
                                  COLORREF, COLORREF, COLORREF, COLORREF, 
                                  COLORREF, COLORREF, COLORREF, COLORREF)
            val (toCC, fromCC, ccStruct) = breakConversion CHOOSECOLOR
            val (toM, fromM, mStruct) = breakConversion CUSTOM
            val (toCR, fromCR, cref) = breakConversion COLORREF

            fun toCChooseColor {
                owner: HWND option,
                result: COLORREF,
                customColors: COLORREF list,
                flags: ChooseColorFlags.flags
            } =
            let
                val custom = alloc 1 mStruct
                val black = fromCR(RGB{red=0, green=0, blue=0})
                fun fillCustom(_, 16) = ()
                 |  fillCustom([], i) =
                        (assign cref (offset i cref custom) black; fillCustom([], i+1))
                 |  fillCustom(hd::tl, i) =
                        (assign cref (offset i cref custom) (fromCR hd); fillCustom(tl, i+1))
            in
                fillCustom(customColors, 0);
                address(
                    fromCC(sizeof ccStruct, owner, 0, result, address custom,
                        ChooseColorFlags.toWord flags, 0, 0, 0))
            end

            fun fromCChooseColor v : CHOOSECOLOR =
            let
                val (_, owner, _, result, custom, flags, _, _, _) = toCC(deref v)
                val custom =
                    List.tabulate(16, fn i => toCR(offset i cref(deref custom)))
            in
                { owner = owner, flags = ChooseColorFlags.fromWord flags,
                  customColors = custom, result = result}
            end
        in
            fun ChooseColor (arg: CHOOSECOLOR): CHOOSECOLOR option =
            let
                val converted = toCChooseColor arg
                val result =
                    call1 (commdlg "ChooseColorA") POINTER BOOL converted
            in
                if result
                then SOME(fromCChooseColor converted)
                else NONE
            end
        end

(*
typedef struct tagCHOOSECOLORA {
   DWORD        lStructSize;
   HWND         hwndOwner;
   HWND         hInstance;
   COLORREF     rgbResult;
   COLORREF*    lpCustColors;
   DWORD        Flags;
   LPARAM       lCustData;
   LPCCHOOKPROC lpfnHook;
   LPCSTR       lpTemplateName;
} CHOOSECOLORA, *LPCHOOSECOLORA;

*)
(*
ChooseColor  
PrintDlgEx  - NT 5.0 and later only

The following application-defined hook procedures are used with common dialog boxes. 

CCHookProc   
CFHookProc   
FRHookProc   
OFNHookProc   
OFNHookProcOldStyle   
PagePaintHook   
PageSetupHook   
PrintHookProc   
SetupHookProc  
*)
    end
end;
