(*
    Title:      Foreign Function Interface
    Author:     David Matthews
    Copyright   David Matthews 2015

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

(* This is defined separately so that the values are computed and
   available as constants for the Foreign structure. *)
structure ForeignPrivate =
struct
    local
        fun getSizeAndAlign (n: int) =
        let
            val ffiType = RunCall.run_call2 RuntimeCalls.POLY_SYS_ffi (52, n)
            val (size: word, align: word, _, _) = (* Just get the first two fields. *)
                RunCall.run_call2 RuntimeCalls.POLY_SYS_ffi (53, ffiType)
        in
            {size=size, align=align}
        end
    in
        val saVoid      = getSizeAndAlign 0
        and saUint8     = getSizeAndAlign 1
        and saSint8     = getSizeAndAlign 2
        and saUint16    = getSizeAndAlign 3
        and saSint16    = getSizeAndAlign 4
        and saUint32    = getSizeAndAlign 5
        and saSint32    = getSizeAndAlign 6
        and saUint64    = getSizeAndAlign 7
        and saSint64    = getSizeAndAlign 8
        and saFloat     = getSizeAndAlign 9
        and saDouble    = getSizeAndAlign 10
        and saPointer   = getSizeAndAlign 11
        and saUChar     = getSizeAndAlign 12
        and saSChar     = getSizeAndAlign 13
        and saUShort    = getSizeAndAlign 14
        and saSShort    = getSizeAndAlign 15
        and saUint      = getSizeAndAlign 16
        and saSint      = getSizeAndAlign 17
        and saUlong     = getSizeAndAlign 18
        and saSlong     = getSizeAndAlign 19
    end
    
    val bigEndian : bool = RunCall.run_call0 RuntimeCalls.POLY_SYS_is_big_endian ()
    val wordSize : word = RunCall.run_call0 RuntimeCalls.POLY_SYS_bytes_per_word ()
    
    (* Minimum argument size. *)
    val ffiMinArgSize: Word.word = RunCall.run_call2 RuntimeCalls.POLY_SYS_ffi (51, 15)
end;

signature FOREIGN =
sig
    exception Foreign of string

    structure Memory:
    sig
        eqtype volatileRef
        val volatileRef: SysWord.word -> volatileRef
        val setVolatileRef: volatileRef * SysWord.word -> unit
        val getVolatileRef: volatileRef -> SysWord.word
        
        eqtype voidStar
        val voidStar2Sysword: voidStar -> SysWord.word
        val sysWord2VoidStar: SysWord.word -> voidStar
        val null: voidStar
        
        (* Remember an address except across loads. *)
        val memoise: ('a -> voidStar) ->'a -> unit -> voidStar
        
        (* malloc - allocate memory.  N.B. argument is the number of bytes. *)
        val malloc: word -> voidStar
        (* free - free allocated memory. *)
        val free: voidStar -> unit

        val get8:  voidStar * Word.word -> Word8.word
        val get16: voidStar * Word.word -> Word.word
        val get32: voidStar * Word.word -> Word32.word
        val get64: voidStar * Word.word -> SysWord.word
        val set8:  voidStar * Word.word * Word8.word -> unit
        val set16: voidStar * Word.word * Word.word -> unit
        val set32: voidStar * Word.word * Word32.word -> unit
        val set64: voidStar * Word.word * SysWord.word -> unit

        val getFloat:   voidStar * Word.word -> real
        val getDouble:  voidStar * Word.word -> real
        val setFloat:   voidStar * Word.word * real -> unit
        val setDouble:  voidStar * Word.word * real -> unit

        val getAddress: voidStar * Word.word -> voidStar
        val setAddress: voidStar * Word.word * voidStar -> unit
    end
    
    structure System:
    sig
        type voidStar = Memory.voidStar
        val loadLibrary: string -> voidStar
        and loadExecutable: unit -> voidStar
        and freeLibrary: voidStar -> unit
        and getSymbol: voidStar * string -> voidStar
    end
    
    structure LibFFI:
    sig
        eqtype abi
        (* List of ABIs defined in libffi for this platform. *)
        val abiList: (string * abi) list
        (* The default Abi. *)
        val abiDefault:         abi

        (* Type codes. *)
        val ffiTypeCodeVoid:    Word.word
        and ffiTypeCodeInt:     Word.word
        and ffiTypeCodeFloat:   Word.word
        and ffiTypeCodeDouble:  Word.word
        and ffiTypeCodeUInt8:   Word.word
        and ffiTypeCodeSInt8:   Word.word
        and ffiTypeCodeUInt16:  Word.word
        and ffiTypeCodeSInt16:  Word.word
        and ffiTypeCodeUInt32:  Word.word
        and ffiTypeCodeSInt32:  Word.word
        and ffiTypeCodeUInt64:  Word.word
        and ffiTypeCodeSInt64:  Word.word
        and ffiTypeCodeStruct:  Word.word
        and ffiTypeCodePointer: Word.word

        (* Predefined types.  These are addresses so have to be reloaded
           in each session. *)
        eqtype ffiType
        val ffiType2voidStar: ffiType -> Memory.voidStar
        val voidStar2ffiType: Memory.voidStar -> ffiType

        val getFFItypeVoid: unit -> ffiType
        and getFFItypeUint8: unit -> ffiType
        and getFFItypeSint8: unit -> ffiType
        and getFFItypeUint16: unit -> ffiType
        and getFFItypeSint16: unit -> ffiType
        and getFFItypeUint32: unit -> ffiType
        and getFFItypeSint32: unit -> ffiType
        and getFFItypeUint64: unit -> ffiType
        and getFFItypeSint64: unit -> ffiType
        and getFFItypeFloat: unit -> ffiType
        and getFFItypeDouble: unit -> ffiType
        and getFFItypePointer: unit -> ffiType
        and getFFItypeUChar: unit -> ffiType
        and getFFItypeSChar: unit -> ffiType
        and getFFItypeUShort: unit -> ffiType
        and getFFItypeSShort: unit -> ffiType
        and getFFItypeUint: unit -> ffiType
        and getFFItypeSint: unit -> ffiType
        and getFFItypeUlong: unit -> ffiType
        and getFFItypeSlong: unit -> ffiType
        
        val extractFFItype:
            ffiType -> { size: word, align: word, typeCode: word, elements: ffiType list }
        val createFFItype:
            { size: word, align: word, typeCode: word, elements: ffiType list } -> ffiType

        eqtype cif
        val cif2voidStar: cif -> Memory.voidStar
        val voidStar2cif: Memory.voidStar -> cif
        val createCIF: abi * ffiType * ffiType list -> cif
        val callFunction:
            { cif: cif, function: Memory.voidStar, result: Memory.voidStar, arguments: Memory.voidStar } -> unit

        val createCallback:
            (Memory.voidStar * Memory.voidStar -> unit) * cif -> Memory.voidStar
        val freeCallback: Memory.voidStar -> unit
    end
    
    type library
    type symbol
    val loadLibrary: string -> library
    val loadExecutable: unit -> library
    val getSymbol: library * string -> symbol
    val symbolAsAddress: symbol -> Memory.voidStar

    structure LowLevel:
    sig
        type ctype =
        {
            size: Word.word, (* Size in bytes *)
            align: Word.word, (* Alignment *)
            ffiType: unit -> LibFFI.ffiType
        }
        
        val cTypeVoid: ctype
        and cTypePointer: ctype
        and cTypeInt8: ctype
        and cTypeChar: ctype
        and cTypeUint8: ctype
        and cTypeUchar: ctype
        and cTypeInt16: ctype
        and cTypeUint16: ctype
        and cTypeInt32: ctype
        and cTypeUint32: ctype
        and cTypeInt64: ctype
        and cTypeUint64: ctype
        and cTypeInt: ctype
        and cTypeUint: ctype
        and cTypeLong: ctype
        and cTypeUlong: ctype
        and cTypeFloat: ctype
        and cTypeDouble: ctype
        
        val cStruct: ctype list -> ctype

        val callwithAbi: LibFFI.abi -> symbol -> ctype list -> ctype -> Memory.voidStar list * Memory.voidStar -> unit
        val call: symbol -> ctype list -> ctype -> Memory.voidStar list * Memory.voidStar -> unit
        
        val cFunctionWithAbi:
            LibFFI.abi -> ctype list -> ctype -> (Memory.voidStar * Memory.voidStar -> unit) -> Memory.voidStar
        val cFunction:
            ctype list -> ctype -> (Memory.voidStar * Memory.voidStar -> unit) -> Memory.voidStar
    end

    type 'a conversion =
    {
        load: Memory.voidStar -> 'a, (* Load a value from C memory *)
        store: Memory.voidStar * 'a -> unit, (* Store a value in C memory *)
        release: Memory.voidStar * 'a -> unit, (* Copy back any result and free any memory allocated in "store" *)
        ctype: LowLevel.ctype
    }

    val cVoid: unit conversion
    val cPointer: Memory.voidStar conversion
    val cInt8: int conversion
    val cUint8: int conversion
    val cChar: char conversion
    val cUchar: Word8.word conversion
    val cInt16: int conversion
    val cUint16: int conversion
    val cInt32: int conversion
    val cUint32: int conversion
    val cInt64: int conversion
    val cUint64: int conversion
    val cShort: int conversion
    val cUshort: int conversion
    val cInt: int conversion
    val cUint: int conversion
    val cLong: int conversion
    val cUlong: int conversion
    val cString: string conversion
    val cFloat: real conversion
    val cDouble: real conversion

    val cFunction0withAbi: LibFFI.abi -> unit -> 'a conversion -> (unit -> 'a) conversion
    val cFunction0: unit -> 'a conversion -> (unit -> 'a) conversion
    val cFunction1withAbi: LibFFI.abi -> 'a conversion -> 'b conversion -> ('a -> 'b) conversion
    val cFunction1: 'a conversion -> 'b conversion -> ('a -> 'b) conversion
    val cFunction2withAbi:
        LibFFI.abi -> 'a conversion * 'b conversion -> 'c conversion -> ('a * 'b -> 'c) conversion
    val cFunction2: 'a conversion * 'b conversion -> 'c conversion -> ('a * 'b -> 'c) conversion
    val cFunction3withAbi:
        LibFFI.abi -> 'a conversion * 'b conversion * 'c conversion -> 'd conversion -> ('a * 'b *'c -> 'd) conversion
    val cFunction3: 'a conversion * 'b conversion *  'c conversion -> 'd conversion -> ('a * 'b * 'c -> 'd) conversion
    val cFunction4withAbi:
        LibFFI.abi -> 'a conversion * 'b conversion * 'c conversion * 'd conversion -> 'e conversion ->
            ('a * 'b * 'c * 'd -> 'e) conversion
    val cFunction4: 'a conversion * 'b conversion * 'c conversion * 'd conversion -> 'e conversion ->
            ('a * 'b * 'c * 'd -> 'e) conversion
    val cFunction5withAbi:
        LibFFI.abi -> 'a conversion * 'b conversion * 'c conversion * 'd conversion * 'e conversion -> 'f conversion ->
            ('a * 'b * 'c * 'd * 'e -> 'f) conversion
    val cFunction5: 'a conversion * 'b conversion * 'c conversion * 'd conversion * 'e conversion -> 'f conversion ->
            ('a * 'b * 'c * 'd * 'e -> 'f) conversion
    val cFunction6withAbi:
        LibFFI.abi -> 'a conversion * 'b conversion * 'c conversion * 'd conversion * 'e conversion * 'f conversion ->
            'g conversion -> ('a * 'b * 'c * 'd * 'e * 'f -> 'g) conversion
    val cFunction6:
        'a conversion * 'b conversion * 'c conversion * 'd conversion * 'e conversion * 'f conversion ->
            'g conversion -> ('a * 'b * 'c * 'd * 'e * 'f -> 'g) conversion

    (* Remove the "release" from a conversion.  Used if extra memory allocated
       by the argument must not be freed when the function returns.  *)
    val permanent: 'a conversion -> 'a conversion

    (* Call by reference.  *)
    val cStar: 'a conversion -> 'a ref conversion
    
    val cStruct2: 'a conversion * 'b conversion -> ('a * 'b) conversion
    val cStruct3: 'a conversion * 'b conversion * 'c conversion -> ('a*'b*'c)conversion
    
    val call0withAbi: LibFFI.abi -> symbol -> unit -> 'a conversion -> unit -> 'a
    val call0: symbol -> unit -> 'a conversion -> unit -> 'a
    val call1withAbi: LibFFI.abi -> symbol -> 'a conversion -> 'b conversion -> 'a -> 'b
    val call1: symbol -> 'a conversion -> 'b conversion -> 'a -> 'b
    val call2withAbi: LibFFI.abi -> symbol -> 'a conversion * 'b conversion -> 'c conversion -> 'a * 'b -> 'c
    val call2: symbol -> 'a conversion * 'b conversion -> 'c conversion -> 'a * 'b -> 'c
end;

structure Foreign:> FOREIGN =
struct
    fun id x = x
    exception Foreign = RunCall.Foreign

    open ForeignPrivate

    structure Memory =
    struct
        (* Both volatileRef and SysWord.word are the ADDRESSes of the actual value. *)
        type volatileRef = word ref

        val memMove: SysWord.word * word * SysWord.word * word* word -> unit =
            RunCall.run_call5 RuntimeCalls.POLY_SYS_move_bytes
       
        fun volatileRef init =
        let
            (* Allocate a single word marked as mutable, weak, no-overwrite, byte. *)
            (* A weak byte cell is cleared to zero when it is read in either from the
               executable or from a saved state.  Using the no-overwrite bit ensures
               that if it is contained in the executable it won't be changed by loading
               a saved state but there's a problem if it is contained in a parent state.
               Then loading a child state will clear it because we reload all the parents
               when we load a child. *)
            val v = RunCall.run_call3 RuntimeCalls.POLY_SYS_alloc_store(0w1, 0wx69, 0w0)
            (* Copy the SysWord into it. *)
            val () = memMove(init, 0w0, RunCall.unsafeCast v, 0w0, wordSize)
        in
            v
        end

        fun setVolatileRef(v, i) =
            memMove(i, 0w0, RunCall.unsafeCast v, 0w0, wordSize)

        fun getVolatileRef var =
        let
            (* Allocate a single word marked as mutable, byte. *)
            val v = RunCall.run_call3 RuntimeCalls.POLY_SYS_alloc_store(0w1, 0wx41, 0w0)
            val () = memMove(RunCall.unsafeCast var, 0w0, v, 0w0, wordSize)
            val () = RunCall.run_call1 RuntimeCalls.POLY_SYS_lockseg v
        in
            v
        end

        type voidStar = SysWord.word
        val voidStar2Sysword = id and sysWord2VoidStar = id (* Exported conversions *)
        val null: voidStar = 0w0
        
        fun 'a memoise(f: 'a -> voidStar) (a: 'a) : unit -> voidStar =
        let
            (* Initialise to zero.  That means the function won't be
               executed until we actually want the result. *)
            val v = volatileRef 0w0
        in
            (* If we've reloaded the volatile ref it will have been reset to zero.
               We need to execute the function and set it. *)
            fn () => (case getVolatileRef v of 0w0 => let val r = f a in setVolatileRef(v, r); r end | r => r)
        end

        fun malloc (s: word): voidStar = RunCall.run_call2 RuntimeCalls.POLY_SYS_ffi (0, s)
        
        fun free (s: voidStar): unit = RunCall.run_call2 RuntimeCalls.POLY_SYS_ffi (1, s)

        fun get8 (s: voidStar, i: Word.word): Word8.word =
            RunCall.run_call3 RuntimeCalls.POLY_SYS_cmem_load_8 (s, 0w0, i)
        and get16(s: voidStar, i: Word.word): Word.word =
            RunCall.run_call3 RuntimeCalls.POLY_SYS_cmem_load_16 (s, 0w0, i)
        and get32(s: voidStar, i: Word.word): Word32.word =
            RunCall.run_call3 RuntimeCalls.POLY_SYS_cmem_load_32 (s, 0w0, i)
        and get64(s: voidStar, i: Word.word): SysWord.word =
            RunCall.run_call3 RuntimeCalls.POLY_SYS_cmem_load_64 (s, 0w0, i)
        and getFloat(s: voidStar, i: Word.word): real =
            RunCall.run_call3 RuntimeCalls.POLY_SYS_cmem_load_float (s, 0w0, i)
        and getDouble(s: voidStar, i: Word.word): real =
            RunCall.run_call3 RuntimeCalls.POLY_SYS_cmem_load_double (s, 0w0, i)

        fun set8 (s: voidStar, i: Word.word, v: Word8.word): unit =
            RunCall.run_call4 RuntimeCalls.POLY_SYS_cmem_store_8 (s, 0w0, i, v)
        and set16 (s: voidStar, i: Word.word, v: Word.word): unit =
            RunCall.run_call4 RuntimeCalls.POLY_SYS_cmem_store_16 (s, 0w0, i, v)
        and set32 (s: voidStar, i: Word.word, v: Word32.word): unit =
            RunCall.run_call4 RuntimeCalls.POLY_SYS_cmem_store_32 (s, 0w0, i, v)
        and set64 (s: voidStar, i: Word.word, v: SysWord.word): unit =
            RunCall.run_call4 RuntimeCalls.POLY_SYS_cmem_store_64 (s, 0w0, i, v)
        and setFloat (s: voidStar, i: Word.word, v: real): unit =
            RunCall.run_call4 RuntimeCalls.POLY_SYS_cmem_store_float (s, 0w0, i, v)
        and setDouble (s: voidStar, i: Word.word, v: real): unit =
            RunCall.run_call4 RuntimeCalls.POLY_SYS_cmem_store_double (s, 0w0, i, v)

        (* Get and set addresses.  This is a bit messy because it has to compile on 64-bits as well as 32-bits. *)
        val getAddress: voidStar * Word.word -> voidStar =
            if wordSize = 0w4 then Word32.toLargeWord o get32 else get64
        val setAddress: voidStar * Word.word * voidStar -> unit =
            if wordSize = 0w4 then fn (s, i, v) => set32(s, i, Word32.fromLargeWord v) else set64
    end
    
    structure System =
    struct
        type voidStar = Memory.voidStar
        fun loadLibrary(s: string): voidStar = RunCall.run_call2 RuntimeCalls.POLY_SYS_ffi (2, s)
        and loadExecutable(): voidStar = RunCall.run_call2 RuntimeCalls.POLY_SYS_ffi (3, ())
        and freeLibrary(s: voidStar): unit = RunCall.run_call2 RuntimeCalls.POLY_SYS_ffi (4, s)
        and getSymbol(lib: voidStar, s: string): voidStar = RunCall.run_call2 RuntimeCalls.POLY_SYS_ffi (5, (lib, s))
    end

    structure LibFFI =
    struct
        type abi = Word.word
        val abiList: (string * abi) list = RunCall.run_call2 RuntimeCalls.POLY_SYS_ffi (50, ())

        local
            fun getConstant (n: int) : Word.word = RunCall.run_call2 RuntimeCalls.POLY_SYS_ffi (51, n)
        in
            val abiDefault          = getConstant 0
            
            and ffiTypeCodeVoid     = getConstant 1
            and ffiTypeCodeInt      = getConstant 2
            and ffiTypeCodeFloat    = getConstant 3
            and ffiTypeCodeDouble   = getConstant 4
            and ffiTypeCodeUInt8    = getConstant 5
            and ffiTypeCodeSInt8    = getConstant 6
            and ffiTypeCodeUInt16   = getConstant 7
            and ffiTypeCodeSInt16   = getConstant 8
            and ffiTypeCodeUInt32   = getConstant 9
            and ffiTypeCodeSInt32   = getConstant 10
            and ffiTypeCodeUInt64   = getConstant 11
            and ffiTypeCodeSInt64   = getConstant 12
            and ffiTypeCodeStruct   = getConstant 13
            and ffiTypeCodePointer  = getConstant 14
        end

        type ffiType = Memory.voidStar
        val ffiType2voidStar = id
        and voidStar2ffiType = id

        local
            fun getFFItype (n: int) (): ffiType = RunCall.run_call2 RuntimeCalls.POLY_SYS_ffi (52, n)
        in
            val getFFItypeVoid      = getFFItype 0
            and getFFItypeUint8     = getFFItype 1
            and getFFItypeSint8     = getFFItype 2
            and getFFItypeUint16    = getFFItype 3
            and getFFItypeSint16    = getFFItype 4
            and getFFItypeUint32    = getFFItype 5
            and getFFItypeSint32    = getFFItype 6
            and getFFItypeUint64    = getFFItype 7
            and getFFItypeSint64    = getFFItype 8
            and getFFItypeFloat     = getFFItype 9
            and getFFItypeDouble    = getFFItype 10
            and getFFItypePointer   = getFFItype 11
            and getFFItypeUChar     = getFFItype 12
            and getFFItypeSChar     = getFFItype 13
            and getFFItypeUShort    = getFFItype 14
            and getFFItypeSShort    = getFFItype 15
            and getFFItypeUint      = getFFItype 16
            and getFFItypeSint      = getFFItype 17
            and getFFItypeUlong     = getFFItype 18
            and getFFItypeSlong     = getFFItype 19
        end

        fun extractFFItype (s: ffiType) =
        let
            val (size: word, align: word, typ: word, elem: Memory.voidStar) =
                RunCall.run_call2 RuntimeCalls.POLY_SYS_ffi (53, s)
            (* Unpack the "elements". *)
            open Memory
            fun loadElements i =
            let
                val a = getAddress(elem, i)
            in
                if a = 0w0
                then []
                else a :: loadElements(i+0w1)
            end
            val elements =
                if elem = sysWord2VoidStar 0w0
                then []
                else loadElements 0w0
        in
            { size=size, align=align, typeCode = typ, elements = elements }
        end
        
        (* Construct a new FFItype in allocated memory. *)
        fun createFFItype { size: word, align: word, typeCode: word, elements: ffiType list }: ffiType =
            RunCall.run_call2 RuntimeCalls.POLY_SYS_ffi (54, (size, align, typeCode, elements))

        type cif = Memory.voidStar
        val cif2voidStar = id
        and voidStar2cif = id

        (* Construct and prepare a CIF in allocated memory. *)
        fun createCIF (abi: abi, resultType: ffiType, argTypes: ffiType list): cif =
            RunCall.run_call2 RuntimeCalls.POLY_SYS_ffi (55, (abi, resultType, argTypes))

        (* Call a function. We have to pass some space for the result *)
        fun callFunction
            { cif: cif, function: Memory.voidStar, result: Memory.voidStar, arguments: Memory.voidStar }: unit =
            RunCall.run_call2 RuntimeCalls.POLY_SYS_ffi (56, (cif, function, result, arguments))

        (* Create a callback.  Returns the C function. *)
        fun createCallback(f: Memory.voidStar * Memory.voidStar -> unit, cif: cif): Memory.voidStar =
            RunCall.run_call2 RuntimeCalls.POLY_SYS_ffi (57, (f, cif))
        
        (* Free a callback.  This takes the C function address returned by createCallback *)
        fun freeCallback(cb: Memory.voidStar): unit =
            RunCall.run_call2 RuntimeCalls.POLY_SYS_ffi (58, cb)
    end

    type library = unit -> Memory.voidStar
    type symbol = unit -> Memory.voidStar

    (* Load the library but memoise it so if we reference the library in another
       session we will reload it.  We load the library immediately so that if
       there is an error we get the error immediately. *)
    fun loadLibrary (name: string): library = Memory.memoise System.loadLibrary name
    and loadExecutable (): library  = Memory.memoise System.loadExecutable ()
    
    (* To get a symbol we memoise a function that forces a library load if necessary
       and then gets the symbol. *)
    fun getSymbol(lib: library, name: string): symbol =
        Memory.memoise (fn s => System.getSymbol(lib(), s)) name

    (* This forces the symbol to be loaded.  The result is NOT memoised. *)
    fun symbolAsAddress(s: symbol): Memory.voidStar = s()

    (* Internal. *)
    local
        open Memory
    in
        fun checkedMalloc w =
            let val m = malloc w in if m = null then raise Foreign "Insufficient memory" else m end

        (* Utility function. *)
        fun alignUp(s, align) = Word.andb(s + align-0w1, ~ align)
    end

    structure LowLevel =
    struct
        type ctype =
        {
            size: Word.word, (* Size in bytes *)
            align: Word.word, (* Alignment *)
            ffiType: unit -> LibFFI.ffiType
        }
         
        local
            open LibFFI Memory

            val getffArg =
                if ffiMinArgSize = 0w4 then Word32.toLargeWord o get32
                else if ffiMinArgSize = 0w8 then get64
                else raise Foreign ("Unable to load ffi_arg size=" ^ Word.toString ffiMinArgSize)
            
        in
            val cTypeVoid = 
                { size= #size saVoid, align= #align saVoid, ffiType = memoise getFFItypeVoid () }
            val cTypePointer = 
                { size= #size saPointer, align= #align saPointer, ffiType = memoise getFFItypePointer () }
            val cTypeInt8 =
                { size= #size saSint8, align= #align saSint8, ffiType = memoise getFFItypeSint8 () }
            val cTypeChar = cTypeInt8
            val cTypeUint8 = 
                { size= #size saUint8, align= #align saUint8, ffiType = memoise getFFItypeUint8 () }
            val cTypeUchar = cTypeUint8
            val cTypeInt16 =
                { size= #size saSint16, align= #align saSint16, ffiType = memoise getFFItypeSint16 () }
            val cTypeUint16 =
                { size= #size saUint16, align= #align saUint16, ffiType = memoise getFFItypeUint16 () }
            val cTypeInt32 =
                { size= #size saSint32, align= #align saSint32, ffiType = memoise getFFItypeSint32 () }
            val cTypeUint32 =
                { size= #size saUint32, align= #align saUint32, ffiType = memoise getFFItypeUint32 () }
            val cTypeInt64 =
                { size= #size saSint64, align= #align saSint64, ffiType = memoise getFFItypeSint64 () }
            val cTypeUint64 =
                { size= #size saUint64, align= #align saUint64, ffiType = memoise getFFItypeUint64 () }
            val cTypeInt =
                { size= #size saSint, align= #align saSint, ffiType = memoise getFFItypeSint () }
            val cTypeUint =
                { size= #size saUint, align= #align saUint, ffiType = memoise getFFItypeUint () }
            val cTypeLong =
                { size= #size saSlong, align= #align saSlong, ffiType = memoise getFFItypeSlong () }
            val cTypeUlong =
                { size= #size saUlong, align= #align saUlong, ffiType = memoise getFFItypeUlong () }
            val cTypeFloat =
                { size= #size saFloat, align= #align saFloat, ffiType = memoise getFFItypeFloat () }
            val cTypeDouble =
                { size= #size saDouble, align= #align saDouble, ffiType = memoise getFFItypeDouble () }

            fun cStruct(fields: ctype list): ctype =
            let
                (* The total alignment is the maximum alignment of the fields. *)
                val align = foldl(fn ({align, ...}, a) => Word.max(align, a)) 0w1 fields
                (* Each field needs to be on its alignment.  Finally we round up the size
                   to the total alignment. *)
                val size =
                    alignUp(foldl(fn ({align, size, ...}, s) => alignUp(s, align) + size) 0w0 fields, align)

                val types = map #ffiType fields

                (* Make the type but only when it's used. *)
                fun ffiType () =
                    LibFFI.createFFItype {
                        size = size, align = align, typeCode=LibFFI.ffiTypeCodeStruct,
                        elements = map (fn t => t()) types }
            in
                {align=align, size=size, ffiType=memoise ffiType ()}
            end

            fun callwithAbi (abi: abi) (fnAddr: unit->voidStar) (argTypes: ctype list) (resType: ctype): voidStar list * voidStar -> unit =
            let
                (* Preparation when we create the function. *)
                fun buildCif () = createCIF (abi, #ffiType resType (), map (fn {ffiType, ...} => ffiType ()) argTypes)
                val cif: unit->cif = memoise buildCif ()
                val nArgs = List.length argTypes
                val resSize = #size resType
                
                (* If the result size is smaller than ffiMinArgSize we have to
                   first store the result in a value of size ffiMinArgSize then copy
                   the result.  This is a restriction of libffi. *)
                fun smallSpace (args, resMem) =
                let
                    val _ = List.length args = nArgs orelse raise Foreign "Incorrect number of arguments"
                    val resultSize = alignUp(ffiMinArgSize, #align saPointer)
                    val argResVec = checkedMalloc(resultSize + #size saPointer * Word.fromInt nArgs)
                    val argLocn = argResVec + SysWord.fromLargeWord(Word.toLargeWord resultSize)
                    val _ = List.foldl(fn (arg, n) => (setAddress(argLocn, n, arg); n+0w1)) 0w0 args
                in
                    let
                        val () = callFunction { cif=cif(), function=fnAddr(), result = argResVec, arguments = argLocn}
                        val result: SysWord.word = getffArg(argResVec, 0w0)
                    in
                        (* Copy to the final location.  Currently "void" has size 1 so if
                           the function has a void result we still copy one byte. *)
                        if #size resType = 0w1
                        then set8(resMem, 0w0, Word8.fromLargeWord result)
                        else if #size resType = 0w2
                        then set16(resMem, 0w0, Word.fromLargeWord result)
                        else if #size resType = 0w4
                        then set32(resMem, 0w0, Word32.fromLargeWord result)
                        else raise Foreign "Unable to set result: wrong size";
                        free argResVec
                    end handle exn => (free argResVec; raise exn)
                end
                
                (* If we have enough space. *)
                fun largeSpace (args, resMem) =
                let
                    val _ = List.length args = nArgs orelse raise Foreign "Incorrect number of arguments"
                    val argVec =
                        if nArgs = 0 then null else checkedMalloc(#size saPointer * Word.fromInt nArgs)
                    val _ = List.foldl(fn (arg, n) => (setAddress(argVec, n, arg); n+0w1)) 0w0 args
                in
                    let
                        val () = callFunction { cif=cif(), function=fnAddr(), result = resMem, arguments = argVec}
                    in
                        free argVec
                    end handle exn => (free argVec; raise exn)
                end
            in
                if resSize < ffiMinArgSize
                then smallSpace
                else largeSpace
            end

            fun call x = callwithAbi abiDefault x (* Have to make it a fun to avoid value restriction *)

            (* Build a call-back function.  Returns a function to take the actual ML function,
               create a callback and then return the address. *)
            fun cFunctionWithAbi (abi: abi) (argTypes: ctype list) (resType: ctype):
                    (voidStar * voidStar -> unit) -> voidStar =
            let
                fun buildCif () = createCIF (abi, #ffiType resType (), map (fn {ffiType, ...} => ffiType ()) argTypes)
                val cif: unit->cif = memoise buildCif ()
            in
                fn cbFun => createCallback(cbFun, cif())
            end
            
            fun cFunction x = cFunctionWithAbi abiDefault x
        end

    end

    type 'a conversion =
    {
        load: Memory.voidStar -> 'a, (* Load a value from C memory *)
        store: Memory.voidStar * 'a -> unit, (* Store a value in C memory *)
        release: Memory.voidStar * 'a -> unit, (* Free any memory allocated in "store" *)
        ctype: LowLevel.ctype
    }

    (* Conversions *)
    local
        open LibFFI Memory LowLevel
        fun checkRange(i, min, max) = if i < min orelse i > max then raise Overflow else i
        fun noFree _ = () (* None of these allocate extra memory or need to update. *)
    in
        val cVoid: unit conversion =
            { load=fn _ => (), store=fn _ => (), release = noFree, ctype = cTypeVoid }

        (* cPointer should only be used to base other conversions on. *)
        val cPointer: voidStar conversion =
            { load=fn a => getAddress(a, 0w0), store=fn(a, v) => setAddress(a, 0w0, v),
              release = noFree, ctype = cTypePointer }

        local
            fun load(m: voidStar): int = Word8.toIntX(get8(m, 0w0))
            fun store(m: voidStar, i: int) =
                set8(m, 0w0, Word8.fromInt(checkRange(i, ~128, 127)))
        in
            val cInt8: int conversion =
                { load=load, store=store, release = noFree, ctype = cTypeInt8 }
        end

        local
            (* Char is signed in C but unsigned in ML. *)
            fun load(m: voidStar): char = Char.chr(Word8.toInt(get8(m, 0w0)))
            fun store(m: voidStar, i: char) =
                set8(m, 0w0, Word8.fromInt(Char.ord i))
        in
            val cChar: char conversion =
                { load=load, store=store, release = noFree, ctype = cTypeChar }
        end

        local
            (* Uchar - convert as Word8.word. *)
            fun load(m: voidStar): Word8.word = get8(m, 0w0)
            fun store(m: voidStar, i: Word8.word) = set8(m, 0w0, i)
        in
            val cUchar: Word8.word conversion =
                { load=load, store=store, release = noFree, ctype = cTypeUchar }
        end

        local
            fun load(m: voidStar): int = Word8.toInt(get8(m, 0w0))
            fun store(m: voidStar, i: int) =
                set8(m, 0w0, Word8.fromInt(checkRange(i, 0, 255)))
        in
            val cUint8: int conversion =
                { load=load, store=store, release = noFree, ctype = cTypeUint8 }
        end

        local
            fun load(m: voidStar): int = Word.toIntX(get16(m, 0w0))
            fun store(m: voidStar, i: int) =
                set16(m, 0w0, Word.fromInt(checkRange(i, ~32768, 32767)))
        in
            val cInt16: int conversion =
                { load=load, store=store, release = noFree, ctype = cTypeInt16 }
        end

        local
            fun load(m: voidStar): int = Word.toInt(get16(m, 0w0))
            fun store(m: voidStar, i: int) =
                set16(m, 0w0, Word.fromInt(checkRange(i, 0, 65535)))
        in
            val cUint16: int conversion =
                { load=load, store=store, release = noFree, ctype = cTypeUint16 }
        end

        local
            fun load(m: voidStar): int = Word32.toIntX(get32(m, 0w0))
            fun store(m: voidStar, i: int) =
                set32(m, 0w0, Word32.fromInt(checkRange(i, ~2147483648, 2147483647)))
        in
            val cInt32: int conversion =
                { load=load, store=store, release = noFree, ctype = cTypeInt32 }
        end

        local
            fun load(m: voidStar): int = Word32.toInt(get32(m, 0w0))
            fun store(m: voidStar, i: int) =
                set32(m, 0w0, Word32.fromInt(checkRange(i, 0, 4294967295)))
        in
            val cUint32: int conversion =
                { load=load, store=store, release = noFree, ctype = cTypeUint32 }
        end

        local
            fun load(m: voidStar): int = SysWord.toIntX(get64(m, 0w0))
            val max = IntInf.<<(1, 0w63) - 1 and min = ~ (IntInf.<<(1, 0w63))
            fun store(m: voidStar, i: int) =
                set64(m, 0w0, SysWord.fromInt(checkRange(i, min, max)))
        in
            val cInt64: int conversion =
                { load=load, store=store, release = noFree, ctype = cTypeInt64 }
        end

        local
            fun load(m: voidStar): int = SysWord.toInt(get64(m, 0w0))
            val max = IntInf.<<(1, 0w64) - 1
            fun store(m: voidStar, i: int) =
                set64(m, 0w0, SysWord.fromInt(checkRange(i, 0, max)))
        in
            val cUint64: int conversion =
                { load=load, store=store, release = noFree, ctype = cTypeUint64 }
        end

        local
            fun load(m: voidStar): real = getFloat(m, 0w0)
            fun store(m: voidStar, v: real) = setFloat(m, 0w0, v)
        in
            val cFloat: real conversion =
                { load=load, store=store, release = noFree, ctype = cTypeFloat }
        end

        local
            fun load(m: voidStar): real = getDouble(m, 0w0)
            fun store(m: voidStar, v: real) = setDouble(m, 0w0, v)
        in
            val cDouble: real conversion =
                { load=load, store=store, release = noFree, ctype = cTypeDouble }
        end

        val cShort =
            if #size saSShort = #size saSint16 then cInt16
            else if #size saSShort = #size saSint32 then cInt32
            else raise Foreign "Unable to find type for short"

        val cUshort = 
            if #size saUShort = #size saUint16 then cUint16
            else if #size saUShort = #size saUint32 then cUint32
            else raise Foreign "Unable to find type for unsigned"

        val cInt =
            if #size saSint = #size saSint16 then cInt16
            else if #size saSint = #size saSint32 then cInt32
            else if #size saSint = #size saSint64 then cInt64
            else raise Foreign "Unable to find type for int"

        val cUint = 
            if #size saUint = #size saUint16 then cUint16
            else if #size saUint = #size saUint32 then cUint32
            else if #size saUint = #size saUint64 then cUint64
            else raise Foreign "Unable to find type for unsigned"

        val cLong =
            if #size saSlong = #size saSint16 then cInt16
            else if #size saSlong = #size saSint32 then cInt32
            else if #size saSlong = #size saSint64 then cInt64
            else raise Foreign "Unable to find type for long"

        val cUlong = 
            if #size saUlong = #size saUint16 then cUint16
            else if #size saUlong = #size saUint32 then cUint32
            else if #size saUlong = #size saUint64 then cUint64
            else raise Foreign "Unable to find type for unsigned long"

        local
            fun load(s: voidStar): string =
            let
                (* The location contains the address of the string. *)
                val sAddr = getAddress(s, 0w0)
                fun loadChar i =
                let
                    val ch = get8(sAddr, i)
                in
                    if ch = 0w0 then []
                    else Char.chr(Word8.toInt ch) :: loadChar(i+0w1)
                end
            in
                String.implode(loadChar 0w0)
            end
            
            fun store(v: voidStar, s: string) =
            let
                val sLen = Word.fromInt(String.size s)
                val sMem = checkedMalloc(sLen + 0w1)
                val () = CharVector.appi(fn(i, ch) => set8(sMem, Word.fromInt i, Word8.fromInt(Char.ord ch))) s
                val () = set8(sMem, sLen, 0w0)
            in
                setAddress(v, 0w0, sMem)
            end
            
            fun release(s: voidStar, _) = Memory.free(getAddress(s, 0w0))
        in
            val cString: string conversion =
                { load=load, store=store, release = release, ctype = cTypePointer }
        end

    end

    (* Replace the "release" function by a null function.  This is intended for situations
       where an argument should not be deleted once the function completes.
       This also prevents copying of the result if necessary. *)
    fun permanent({load, store, ctype, ...}: 'a conversion): 'a conversion =
        { release=fn _ => (), load=load, store=store, ctype=ctype }
    
    val toSysWord = SysWord.fromLarge o Word.toLarge

    fun cStruct2(a: 'a conversion, b: 'b conversion): ('a*'b)conversion =
    let
        val {load=loada, store=storea, release=releasea, ctype = ctypea as {size=sizea, ... }} = a
        and {load=loadb, store=storeb, release=releaseb, ctype = ctypeb as {align=alignb, ... }} = b
        
        val offsetb = alignUp(sizea, alignb)
        fun load s = (loada s, loadb(s + toSysWord offsetb))
        and store (s, (a, b)) = (storea(s, a); storeb(s + toSysWord offsetb, b))
        and release(s, (a, b)) = (releasea(s, a); releaseb(s + toSysWord offsetb, b))

    in
        {load=load, store=store, release = release, ctype = LowLevel.cStruct[ctypea, ctypeb]}
    end

    fun cStruct3(a: 'a conversion, b: 'b conversion, c: 'c conversion): ('a*'b*'c)conversion =
    let
        val {load=loada, store=storea, release=releasea, ctype = ctypea as {size=sizea, ...} } = a
        and {load=loadb, store=storeb, release=releaseb, ctype = ctypeb as {size=sizeb, align=alignb, ...} } = b
        and {load=loadc, store=storec, release=releasec, ctype = ctypec as {align=alignc, ...} } = c
       
        val offsetb = alignUp(sizea, alignb)
        val offsetc = alignUp(offsetb + sizeb, alignc)

        fun load s = (loada s, loadb(s + toSysWord offsetb), loadc(s + toSysWord offsetc))
        and store (s, (a, b, c)) =
            (storea(s, a); storeb(s + toSysWord offsetb, b); storec(s + toSysWord offsetc, c))
        and release(s, (a, b, c)) = (releasea(s, a); releaseb(s + toSysWord offsetb, b); releasec(s + toSysWord offsetc, c))
    in
        {load=load, store=store, release=release, ctype = LowLevel.cStruct[ctypea, ctypeb, ctypec]}
    end

    (* Conversion for call-by-reference. *)
    local
        open Memory LowLevel
    in
        fun cStar({load=loada, store=storea, release=releasea, ctype=ctypea}: 'a conversion): 'a ref conversion =
        let
            (* It's not clear if this is useful. *)
            fun load s = ref(loada(getAddress(s, 0w0)))
            
            fun store(m, ref s) =
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
                val olds = !s
            in
                s := loada mem; (* Update the ref from the value in the cell. *)
                (* It's not clear what release should do here. *)
                releasea(mem, olds);
                free mem
            end
        in
            {load=load, store=store, release=release, ctype = cTypePointer}
        end
    end

    (* Calls with conversion. *)
    local
        open LibFFI Memory LowLevel
    in
        fun call0withAbi (abi: abi) (fnAddr: unit->voidStar) () (resConv: 'a conversion): unit->'a =
        let
            val resType = #ctype resConv
            val callF = callwithAbi abi fnAddr [] resType
        in
            fn () =>
            let
                val rMem = checkedMalloc(#size resType)
            in
                let
                    val () = callF([], rMem)
                    val result = #load resConv rMem
                in
                    free rMem;
                    result
                end handle exn => (free rMem; raise exn)
            end
        end

        fun call0 x = call0withAbi abiDefault x (* Have to make it a fun to avoid value restriction *)

        fun call1withAbi (abi: abi) (fnAddr: unit->voidStar) (argConv: 'a conversion) (resConv: 'b conversion): 'a ->'b =
        let
            val resType = #ctype resConv
            val callF = callwithAbi abi fnAddr [#ctype argConv] resType
        in
            fn x =>
            let
                (* Allocate space for argument(s) and result. *)
                (* TODO: Use a single allocation here. *)
                val rMem = checkedMalloc(#size resType)
                val argMem = checkedMalloc(#size(#ctype argConv))
                val () = #store argConv (argMem, x)
                fun freeAll () =
                    (#release argConv (argMem, x); free rMem; free argMem)
            in
                let
                    val () = callF([argMem], rMem)
                    val result = #load resConv rMem
                in
                    freeAll ();
                    result
                end handle exn => (freeAll (); raise exn)
            end
        end

        fun call1 x = call1withAbi abiDefault x (* Have to make it a fun to avoid value restriction *)

        fun call2withAbi (abi: abi) (fnAddr: unit->voidStar) (arg1Conv: 'a conversion, arg2Conv: 'b conversion)
                          (resConv: 'c conversion): 'a * 'b -> 'c =
        let
            val resType = #ctype resConv
            val callF = callwithAbi abi fnAddr [#ctype arg1Conv, #ctype arg2Conv] resType
        in
            fn (x, y) =>
            let
                val rMem = checkedMalloc(#size resType)
                val arg1Mem = checkedMalloc(#size(#ctype arg1Conv))
                val () = #store arg1Conv (arg1Mem, x)
                val arg2Mem = checkedMalloc(#size(#ctype arg2Conv))
                val () = #store arg2Conv (arg2Mem, y)
                fun freeAll() =
                    (#release arg1Conv(arg1Mem, x); #release arg2Conv (arg2Mem, y); free rMem; free arg1Mem; free arg2Mem)
            in
                let
                    val () = callF([arg1Mem, arg2Mem], rMem)
                    val result = #load resConv rMem
                in
                    freeAll();
                    result
                end handle exn => (freeAll(); raise exn)
            end
        end

        fun call2 x = call2withAbi abiDefault x (* Have to make it a fun to avoid value restriction *)
    end

    local
        open LibFFI Memory LowLevel

        fun makeCallbackConv(
            abi: abi, argTypes: ctype list, resType: ctype,
            callback: ('a -> 'b) -> voidStar * voidStar -> unit): ('a -> 'b) conversion =
        let
            val makeCallback = cFunctionWithAbi abi argTypes resType
            
            (* Create the callback *)
            fun store(v: voidStar, f: 'a -> 'b) =
                setAddress(v, 0w0, makeCallback(callback f))
 
            (* Release the callback *)
            fun release(v, _) = freeCallback(getAddress(v, 0w0))

            (* load should never be called. *)
            fun load _ = raise Foreign "Callbacks cannot be returned as results"
       in
            { load=load, store=store, release=release, ctype = cTypePointer }
        end
    in
        (* Callback conversion *)
        fun cFunction0withAbi (abi: abi) () (resConv: 'a conversion) : (unit -> 'a) conversion =
        let
            fun callback (f: unit -> 'a) (_: voidStar, res: voidStar): unit =
                (* f has no arguments so just store away the result. *)
                #store resConv (res, f())
        in
            makeCallbackConv(abi, [], #ctype resConv, callback)
        end
        
        fun cFunction0 x = cFunction0withAbi abiDefault x

        fun cFunction1withAbi (abi: abi) (argConv: 'a conversion) (resConv: 'b conversion) : ('a -> 'b) conversion =
        let
            fun callback (f: 'a -> 'b) (args: voidStar, res: voidStar): unit =
                (* args is the address of a vector containing just one argument *)
            let
                val result = f (#load argConv (getAddress(args, 0w0)))
            in
                #store resConv (res, result)
            end
        in
            makeCallbackConv(abi, [#ctype argConv], #ctype resConv, callback)
        end
        
        fun cFunction1 x = cFunction1withAbi abiDefault x

        fun cFunction2withAbi (abi: abi) (arg1Conv: 'a conversion, arg2Conv: 'b conversion)
                             (resConv: 'c conversion) : ('a *'b -> 'c) conversion =
        let
            fun callback (f: 'a *'b -> 'c) (args: voidStar, res: voidStar): unit =
            let
                val result =
                    f (
                        #load arg1Conv (getAddress(args, 0w0)),
                        #load arg2Conv (getAddress(args, 0w1)))
            in
                #store resConv (res, result)
            end
        in
            makeCallbackConv(abi, [#ctype arg1Conv, #ctype arg2Conv], #ctype resConv, callback)
        end
        
        fun cFunction2 x = cFunction2withAbi abiDefault x

        fun cFunction3withAbi (abi: abi) (arg1Conv: 'a conversion, arg2Conv: 'b conversion, arg3Conv: 'c conversion)
                             (resConv: 'd conversion) : ('a *'b * 'c -> 'd) conversion =
        let
            fun callback (f: 'a *'b * 'c -> 'd) (args: voidStar, res: voidStar): unit =
            let
                val result =
                    f (
                        #load arg1Conv (getAddress(args, 0w0)),
                        #load arg2Conv (getAddress(args, 0w1)),
                        #load arg3Conv (getAddress(args, 0w2)))
            in
                #store resConv (res, result)
            end
        in
            makeCallbackConv(abi,
                [#ctype arg1Conv, #ctype arg2Conv, #ctype arg3Conv], #ctype resConv, callback)
        end
        
        fun cFunction3 x = cFunction3withAbi abiDefault x

        fun cFunction4withAbi (abi: abi)
                (arg1Conv: 'a conversion, arg2Conv: 'b conversion, arg3Conv: 'c conversion, arg4Conv: 'd conversion)
                (resConv: 'e conversion) : ('a *'b * 'c * 'd -> 'e) conversion =
        let
            fun callback (f: 'a *'b * 'c * 'd -> 'e) (args: voidStar, res: voidStar): unit =
            let
                val result =
                    f (
                        #load arg1Conv (getAddress(args, 0w0)),
                        #load arg2Conv (getAddress(args, 0w1)),
                        #load arg3Conv (getAddress(args, 0w2)),
                        #load arg4Conv (getAddress(args, 0w3)))
            in
                #store resConv (res, result)
            end
        in
            makeCallbackConv(abi,
                [#ctype arg1Conv, #ctype arg2Conv, #ctype arg3Conv, #ctype arg4Conv], #ctype resConv, callback)
        end
        
        fun cFunction4 x = cFunction4withAbi abiDefault x

        fun cFunction5withAbi (abi: abi)
                (arg1Conv: 'a conversion, arg2Conv: 'b conversion, arg3Conv: 'c conversion,
                 arg4Conv: 'd conversion, arg5Conv: 'e conversion)
                (resConv: 'f conversion) : ('a *'b * 'c * 'd * 'e -> 'f) conversion =
        let
            fun callback (f: 'a *'b * 'c * 'd * 'e -> 'f) (args: voidStar, res: voidStar): unit =
            let
                val result =
                    f (
                        #load arg1Conv (getAddress(args, 0w0)),
                        #load arg2Conv (getAddress(args, 0w1)),
                        #load arg3Conv (getAddress(args, 0w2)),
                        #load arg4Conv (getAddress(args, 0w3)),
                        #load arg5Conv (getAddress(args, 0w4)))
            in
                #store resConv (res, result)
            end
        in
            makeCallbackConv(abi,
                [#ctype arg1Conv, #ctype arg2Conv, #ctype arg3Conv, #ctype arg4Conv, #ctype arg5Conv],
                #ctype resConv, callback)
        end
        
        fun cFunction5 x = cFunction5withAbi abiDefault x

        fun cFunction6withAbi (abi: abi)
                (arg1Conv: 'a conversion, arg2Conv: 'b conversion, arg3Conv: 'c conversion,
                 arg4Conv: 'd conversion, arg5Conv: 'e conversion, arg6Conv: 'f conversion)
                (resConv: 'g conversion) : ('a *'b * 'c * 'd * 'e * 'f -> 'g) conversion =
        let
            fun callback (f: 'a *'b * 'c * 'd * 'e * 'f -> 'g) (args: voidStar, res: voidStar): unit =
            let
                val result =
                    f (
                        #load arg1Conv (getAddress(args, 0w0)),
                        #load arg2Conv (getAddress(args, 0w1)),
                        #load arg3Conv (getAddress(args, 0w2)),
                        #load arg4Conv (getAddress(args, 0w3)),
                        #load arg5Conv (getAddress(args, 0w4)),
                        #load arg6Conv (getAddress(args, 0w5)))
            in
                #store resConv (res, result)
            end
        in
            makeCallbackConv(abi,
                [#ctype arg1Conv, #ctype arg2Conv, #ctype arg3Conv,
                 #ctype arg4Conv, #ctype arg5Conv, #ctype arg6Conv],
                #ctype resConv, callback)
        end
        
        fun cFunction6 x = cFunction6withAbi abiDefault x

    end
end;
