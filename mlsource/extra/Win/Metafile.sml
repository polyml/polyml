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
    val SetMetaFileBits : Word8Vector.vector -> HMETAFILE
    val SetWinMetaFileBits :
       Word8Vector.vector * HDC * {size: SIZE, mapMode: MapMode} option -> HENHMETAFILE

  end =
struct
    local
        open CInterface Base GdiBase
    in
        datatype MapMode = datatype Transform.MapMode
        type HENHMETAFILE = HENHMETAFILE and HMETAFILE = HMETAFILE
        type HDC = Base.HDC
        type SIZE = SIZE and RECT = RECT
        type METAFILEPICT = METAFILEPICT

        (* TODO: Many of these should check for NULL as a result indicating an error. *)
        val CloseEnhMetaFile = call1 (gdi "CloseEnhMetaFile") (HDC) HENHMETAFILE
        and CloseMetaFile = call1 (gdi "CloseMetaFile") (HDC) HMETAFILE
        and CopyEnhMetaFile = call2 (gdi "CopyEnhMetaFileA") (HENHMETAFILE, STRING) HENHMETAFILE
        and CopyMetaFile = call2 (gdi "CopyMetaFileA") (HMETAFILE, STRING) HMETAFILE
        and CreateMetaFile = call1 (gdi "CreateMetaFileA") (STRINGOPT) HDC
        and DeleteEnhMetaFile =
            call1 (gdi "DeleteEnhMetaFile") (HENHMETAFILE) (SUCCESSSTATE "DeleteEnhMetaFile")
        and DeleteMetaFile = call1 (gdi "DeleteMetaFile") (HMETAFILE) (SUCCESSSTATE "DeleteMetaFile")
        and GetEnhMetaFile = call1 (gdi "GetEnhMetaFileA") (STRING) HENHMETAFILE
        and GetMetaFile = call1 (gdi "GetMetaFileA") (STRING) HMETAFILE
        and PlayEnhMetaFile = call3(gdi "PlayEnhMetaFile") (HDC, HENHMETAFILE, POINTERTO RECT)
                (SUCCESSSTATE "PlayEnhMetaFile")
        and PlayMetaFile = call2(gdi "PlayMetaFile") (HDC, HMETAFILE) (SUCCESSSTATE "PlayMetaFile")
    
        local
            val cemf = call4 (gdi "CreateEnhMetaFileA") (HDC, STRINGOPT, POINTERTO RECT, POINTER) HDC
        in
            fun CreateEnhMetaFile(hdc, name, r, NONE) = cemf(hdc, name, r, toCint 0)
             |  CreateEnhMetaFile(hdc, name, r, SOME{applicationName, pictureName}) =
                let
                    val appSize = size applicationName and pictSize = size pictureName
                    val buff = alloc (appSize + pictSize + 3) Cchar
                in
                    (* The two strings are copied to the buffer with a null between and two
                       nulls at the end. *)
                    fillCstring buff applicationName;
                    fillCstring (offset (appSize+1) Cchar buff) pictureName;
                    assign Cchar (offset (appSize+pictSize+2) Cchar buff) (toCchar #"\000");
                    cemf(hdc, name, r, address buff)
                end
        end
    
        fun GdiComment(hdc, v) =
        let
            val vecsize = Word8Vector.length v
            val buff = fromWord8vec v
        in
            call3 (gdi "GdiComment") (HDC, INT, POINTER) (SUCCESSSTATE "GdiComment")
                (hdc, vecsize, buff)
        end
    
        local
            val gemfb = call3 (gdi "GetEnhMetaFileBits") (HENHMETAFILE, INT, POINTER) 
                            (POSINT "GetEnhMetaFileBits")
        in
            fun GetEnhMetaFileBits(hemf: HENHMETAFILE): Word8Vector.vector =
            let
                (* Call with a NULL buffer to find out how big it is. *)
                val size = gemfb(hemf, 0, toCint 0)
                val buff = alloc size Cchar
                val res = gemfb(hemf, size, address buff)
            in
                toWord8vec(address buff, size)
            end
        end
    
        local
            val gemfb = call3 (gdi "GetMetaFileBitsEx") (HMETAFILE, INT, POINTER) 
                            (POSINT "GetMetaFileBitsEx")
        in
            fun GetMetaFileBitsEx(hemf: HMETAFILE): Word8Vector.vector =
            let
                (* Call with a NULL buffer to find out how big it is. *)
                val size = gemfb(hemf, 0, toCint 0)
                val buff = alloc size Cchar
                val res = gemfb(hemf, size, address buff)
            in
                toWord8vec(address buff, size)
            end
        end
    
    
        local
            val gemfd = call3 (gdi "GetEnhMetaFileDescriptionA") (HENHMETAFILE, INT, POINTER) INT
        in
            fun GetEnhMetaFileDescription(hemf: HENHMETAFILE) =
                (* Call with a NULL buffer to find out how big it is. *)
                case gemfd(hemf, 0, toCint 0) of
                    0 => NONE (* No error - simply no description. *)
                |   len =>
                        if len < 0 then raiseSysErr()
                        else
                        let
                            (* The application and picture names are encoded as a pair. *)
                            val buff = alloc len Cchar
                            val res = gemfd(hemf, len, address buff)
                            val str1 = fromCstring(address buff)
                            val str2 = fromCstring(address(offset (size str1 +1) Cchar buff))
                        in
                            SOME {applicationName=str1, pictureName=str2}
                        end
        end
    
        fun SetEnhMetaFileBits(v: Word8Vector.vector): HENHMETAFILE =
                call2 (gdi "SetEnhMetaFileBits") (INT, POINTER) HENHMETAFILE
                    (Word8Vector.length v, fromWord8vec v)
    
        fun SetMetaFileBits(v: Word8Vector.vector): HMETAFILE =
                call2 (gdi "SetMetaFileBits") (INT, POINTER) HMETAFILE
                    (Word8Vector.length v, fromWord8vec v)
    
        local
            val gwmfb = call5 (gdi "GetWinMetaFileBits") (HENHMETAFILE, INT, POINTER, MAPMODE, HDC)
                            (POSINT "GetWinMetaFileBits")
        in
            fun GetWinMetaFileBits(hemf, mapMode, hdc) =
            let
                (* Call with a null pointer to get the size. *)
                val size = gwmfb(hemf, 0, toCint 0, mapMode, hdc)
                val buff = alloc size Cchar
                val res = gwmfb(hemf, size, address buff, mapMode, hdc)
            in
                toWord8vec(address buff, size)
            end
        end

        local
            val swmfb = call4 (gdi "SetWinMetaFileBits") (INT, POINTER, HDC, POINTER) HENHMETAFILE
            val (_, fromMfp, _) = breakConversion METAFILEPICT
        in
            fun SetWinMetaFileBits(v, hdc, opts) =
            let
                val optBuff =
                    case opts of
                        NONE => toCint 0
                    |   SOME {size, mapMode} =>
                        address(fromMfp{mm=mapMode, size=size, hMF=hgdiObjNull})
            in
                swmfb(Word8Vector.length v, fromWord8vec v, hdc, optBuff)
            end
        end

        type ENHMETAHEADER =
            {
                bounds: RECT, frame: RECT, fileSize: int, records: int,
                handles: int, palEntries: int, resolutionPixels: SIZE,
                resolutionMM: SIZE, openGL: bool
            }

        local
            val ENHMETAHEADER = STRUCT18(INT, INT, RECT, RECT, INT, INT, INT, INT,
                SHORT, SHORT, INT, INT, INT, SIZE, SIZE, INT, INT, BOOL)
            val (toEMH, _, _) = breakConversion ENHMETAHEADER
            val gemf = call3 (gdi "GetEnhMetaFileHeader") (HENHMETAFILE, INT, POINTER)
                    (POSINT "GetEnhMetaFileHeader")
        in
            fun GetEnhMetaFileHeader(h: HENHMETAFILE): ENHMETAHEADER =
            let
                (* Initial call with a NULL buffer to get size and check the handle. *)
                val size = gemf(h, 0, toCint 0)
                val buff = alloc size Cchar
                val res = gemf(h, size, address buff)
                val (_, _, bounds, frame, _, _, fileSize, records, handles,
                    _, descSize, offDesc, palEntries, resolutionPixels, resolutionMM,
                    pixelFormatSize, pixelFormatOff, openGL) = toEMH buff
                (* Ignore the description and the pixelFormat structure.
                   We can get the description using GetEnhMetaFileDescription. *)
            in
                { bounds = bounds, frame = frame, fileSize = fileSize,
                  records = records, handles = handles, palEntries = palEntries,
                  resolutionPixels = resolutionPixels, resolutionMM = resolutionMM,
                  openGL = openGL }
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
