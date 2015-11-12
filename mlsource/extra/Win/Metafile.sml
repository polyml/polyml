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

structure Metafile :
  sig
    type HENHMETAFILE
    type HMETAFILE
    type HDC (*= Base.HDC*)
    type RECT = { top: int, left: int, bottom: int, right: int }
    type SIZE = { cx: int, cy: int }
    datatype MapMode = datatype Transform.MapMode
    type METAFILEPICT = {mm: MapMode, size: SIZE, hMF: HMETAFILE}

    type ENHMETAHEADER =
        {
            bounds: RECT, frame: RECT, fileSize: int, records: int,
            handles: int, palEntries: int, resolutionPixels: SIZE,
            resolutionMM: SIZE, openGL: bool
        }

    val CloseEnhMetaFile : HDC -> HENHMETAFILE
    val CloseMetaFile : HDC -> HMETAFILE
    val CopyEnhMetaFile : HENHMETAFILE * string -> HENHMETAFILE
    val CopyMetaFile : HMETAFILE * string -> HMETAFILE
    val CreateEnhMetaFile :
       HDC * string option * RECT *
       {pictureName: string, applicationName: string} option -> HDC
    val CreateMetaFile : string option -> HDC
    val DeleteEnhMetaFile : HENHMETAFILE -> unit
    val DeleteMetaFile : HMETAFILE -> unit
    val GdiComment : HDC * Word8Vector.vector -> unit
    val GetEnhMetaFile : string -> HENHMETAFILE
    val GetEnhMetaFileBits : HENHMETAFILE -> Word8Vector.vector
    val GetEnhMetaFileDescription :
       HENHMETAFILE -> {pictureName: string, applicationName: string} option
    val GetEnhMetaFileHeader : HENHMETAFILE -> ENHMETAHEADER
    val GetMetaFile : string -> HMETAFILE
    val GetMetaFileBitsEx : HMETAFILE -> Word8Vector.vector
    val GetWinMetaFileBits :
       HENHMETAFILE * Transform.MapMode * HDC -> Word8Vector.vector
    val PlayEnhMetaFile : HDC * HENHMETAFILE * RECT -> unit
    val PlayMetaFile : HDC * HMETAFILE -> unit
    val SetEnhMetaFileBits : Word8Vector.vector -> HENHMETAFILE
    val SetWinMetaFileBits :
       Word8Vector.vector * HDC * {size: SIZE, mapMode: MapMode} option -> HENHMETAFILE

  end =
struct
    local
        open Foreign Base GdiBase
    in
        datatype MapMode = datatype Transform.MapMode
        type HENHMETAFILE = HENHMETAFILE and HMETAFILE = HMETAFILE
        type HDC = Base.HDC
        type SIZE = SIZE and RECT = RECT
        type METAFILEPICT = METAFILEPICT

        (* TODO: Many of these should check for NULL as a result indicating an error. *)
        val CloseEnhMetaFile = call1 (gdi "CloseEnhMetaFile") (cHDC) cHENHMETAFILE
        and CloseMetaFile = call1 (gdi "CloseMetaFile") (cHDC) cHMETAFILE
        and CopyEnhMetaFile = call2 (gdi "CopyEnhMetaFileA") (cHENHMETAFILE, cString) cHENHMETAFILE
        and CopyMetaFile = call2 (gdi "CopyMetaFileA") (cHMETAFILE, cString) cHMETAFILE
        and CreateMetaFile = call1 (gdi "CreateMetaFileA") (STRINGOPT) cHDC
        and DeleteEnhMetaFile =
            call1 (gdi "DeleteEnhMetaFile") (cHENHMETAFILE) (successState "DeleteEnhMetaFile")
        and DeleteMetaFile = call1 (gdi "DeleteMetaFile") (cHMETAFILE) (successState "DeleteMetaFile")
        and GetEnhMetaFile = call1 (gdi "GetEnhMetaFileA") (cString) cHENHMETAFILE
        and GetMetaFile = call1 (gdi "GetMetaFileA") (cString) cHMETAFILE
        and PlayEnhMetaFile = call3(gdi "PlayEnhMetaFile") (cHDC, cHENHMETAFILE, cConstStar cRect)
                (successState "PlayEnhMetaFile")
        and PlayMetaFile = call2(gdi "PlayMetaFile") (cHDC, cHMETAFILE) (successState "PlayMetaFile")
    
        local
            val cemf = call4 (gdi "CreateEnhMetaFileA") (cHDC, STRINGOPT, cConstStar cRect, cPointer) cHDC
        in
            fun CreateEnhMetaFile(hdc, name, r, NONE) = cemf(hdc, name, r, Memory.null)
             |  CreateEnhMetaFile(hdc, name, r, SOME{applicationName, pictureName}) =
                let
                    val appSize = size applicationName and pictSize = size pictureName
                    open Memory
                    val buff = malloc (Word.fromInt(appSize + pictSize + 3))
                in
                    (* The two strings are copied to the buffer with a null between and two
                       nulls at the end. *)
                    copyStringToMem(buff, 0, applicationName);
                    copyStringToMem(buff, appSize+1, pictureName);
                    set8(buff, Word.fromInt(appSize + pictSize + 2), 0w0);
                    (cemf(hdc, name, r, buff)
                        handle ex => (free buff; raise ex)) before free buff
                end
        end

        local
            val gdiComment = call3 (gdi "GdiComment") (cHDC, cUint, cPointer) (successState "GdiComment")
        in
            fun GdiComment(hdc, v) =
            let
                val vecsize = Word8Vector.length v
                val buff = toCWord8vec v
            in
                gdiComment (hdc, vecsize, buff) handle ex => (Memory.free buff; raise ex);
                Memory.free buff
            end
        end
    
        local
            val gemfb = call3 (gdi "GetEnhMetaFileBits") (cHENHMETAFILE, cUint, cPointer) 
                            (cPOSINT "GetEnhMetaFileBits")
        in
            fun GetEnhMetaFileBits(hemf: HENHMETAFILE): Word8Vector.vector =
            let
                (* Call with a NULL buffer to find out how big it is. *)
                open Memory
                val size = gemfb(hemf, 0, Memory.null)
                val buff = malloc(Word.fromInt size)
                val res = gemfb(hemf, size, buff) handle ex => (free buff; raise ex)
            in
                fromCWord8vec(buff, size) before free buff
            end
        end
    
        local
            val gemfb = call3 (gdi "GetMetaFileBitsEx") (cHMETAFILE, cUint, cPointer) 
                            (cPOSINT "GetMetaFileBitsEx")
        in
            fun GetMetaFileBitsEx(hemf: HMETAFILE): Word8Vector.vector =
            let
                (* Call with a NULL buffer to find out how big it is. *)
                open Memory
                val size = gemfb(hemf, 0, Memory.null)
                val buff = malloc(Word.fromInt size)
                val res = gemfb(hemf, size, buff) handle ex => (free buff; raise ex)
            in
                fromCWord8vec(buff, size) before free buff
            end
        end
    
    
        local
            val gemfd = call3 (gdi "GetEnhMetaFileDescriptionA") (cHENHMETAFILE, cUint, cPointer) cInt
            (* It's supposed to return a uint but GDI_ERROR is -1 *)
        in
            fun GetEnhMetaFileDescription(hemf: HENHMETAFILE) =
                (* Call with a NULL buffer to find out how big it is. *)
                case gemfd(hemf, 0, Memory.null) of
                    0 => NONE (* No error - simply no description. *)
                |   len =>
                        if len < 0 then raiseSysErr()
                        else
                        let
                            (* The application and picture names are encoded as a pair. *)
                            open Memory
                            infix 6 ++
                            val buff = malloc (Word.fromInt len)
                            val res = gemfd(hemf, len, buff)
                            val str1 = fromCstring buff
                            val str2 = fromCstring(buff ++ Word.fromInt (size str1 +1))
                        in
                            SOME {applicationName=str1, pictureName=str2}
                        end
        end

        local
            val setEnhMetaFileBits = call2 (gdi "SetEnhMetaFileBits") (cUint, cPointer) cHENHMETAFILE
        in
            fun SetEnhMetaFileBits(v: Word8Vector.vector): HENHMETAFILE =
            let
                val mem = toCWord8vec v
            in
                (setEnhMetaFileBits (Word8Vector.length v, mem)
                    handle ex => (Memory.free mem; raise ex)) before Memory.free mem
            end
        end
    
        local
            val gwmfb = call5 (gdi "GetWinMetaFileBits") (cHENHMETAFILE, cUint, cPointer, cMAPMODE, cHDC)
                            (cPOSINT "GetWinMetaFileBits")
        in
            fun GetWinMetaFileBits(hemf, mapMode, hdc) =
            let
                (* Call with a null pointer to get the size. *)
                open Memory
                val size = gwmfb(hemf, 0, null, mapMode, hdc)
                val buff = malloc (Word.fromInt size)
                val _ = gwmfb(hemf, size, buff, mapMode, hdc)
                            handle ex => (free buff; raise ex)
            in
                fromCWord8vec(buff, size) before free buff
            end
        end

        local
            val swmfb = call4 (gdi "SetWinMetaFileBits") (cUint, cPointer, cHDC, cOptionPtr(cConstStar cMETAFILEPICT)) cHENHMETAFILE
        in
            fun SetWinMetaFileBits(v, hdc, opts) =
            let
                val optmfp =
                    case opts of
                        NONE => NONE
                    |   SOME {size, mapMode} => SOME {mm=mapMode, size=size, hMF=hgdiObjNull}
                val mem = toCWord8vec v
            in
                (swmfb(Word8Vector.length v, mem, hdc, optmfp)
                    handle ex => (Memory.free mem; raise ex)) before Memory.free mem
            end
        end

        type ENHMETAHEADER =
            {
                bounds: RECT, frame: RECT, fileSize: int, records: int,
                handles: int, palEntries: int, resolutionPixels: SIZE,
                resolutionMM: SIZE, openGL: bool
            }

        local
            val ENHMETAHEADER = cStruct18(cDWORD, cDWORD, cRect, cRect, cDWORD, cDWORD, cDWORD, cDWORD,
                cWORD, cWORD, cDWORD, cDWORD, cDWORD, cSize, cSize, cDWORD, cDWORD, cDWORD)
            val {load=toEMH, ...} = breakConversion ENHMETAHEADER
            val gemf = call3 (gdi "GetEnhMetaFileHeader") (cHENHMETAFILE, cUint, cPointer)
                    (cPOSINT "GetEnhMetaFileHeader")
        in
            fun GetEnhMetaFileHeader(h: HENHMETAFILE): ENHMETAHEADER =
            let
                (* Initial call with a NULL buffer to get size and check the handle. *)
                open Memory
                val size = gemf(h, 0, null)
                val buff = malloc(Word.fromInt size)
                val _ = gemf(h, size, buff) handle ex => (free buff; raise ex)
                val (_, _, bounds, frame, _, _, fileSize, records, handles,
                    _, _, _, palEntries, resolutionPixels, resolutionMM,
                    _, _, openGL) = toEMH buff
                val () = free buff
                (* Ignore the description and the pixelFormat structure.
                   We can get the description using GetEnhMetaFileDescription. *)
            in
                { bounds = bounds, frame = frame, fileSize = fileSize,
                  records = records, handles = handles, palEntries = palEntries,
                  resolutionPixels = resolutionPixels, resolutionMM = resolutionMM,
                  openGL = openGL <> 0 }
            end
        end

    (*
    Other metafile Functions
        EnhMetaFileProc  
        EnumEnhMetaFile  
        GetEnhMetaFilePaletteEntries  
        PlayEnhMetaFileRecord  
        
        Obsolete Functions
        EnumMetaFile  
        EnumMetaFileProc  
        PlayMetaFileRecord  
        SetMetaFileBitsEx   
    *)
    end
end;
