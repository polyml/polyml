(*
    Title:      Foreign Function Interface: main part
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

        val ++ : voidStar * word -> voidStar
        val -- : voidStar * word -> voidStar
        
        (* Remember an address except across loads. *)
        val memoise: ('a -> voidStar) ->'a -> unit -> voidStar
        
        exception Memory

        (* malloc - allocate memory.  N.B. argument is the number of bytes.
           Raises Memory exception if it cannot allocate. *)
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
    val getSymbol: library -> string  -> symbol
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
    val cByteArray: Word8Vector.vector conversion
    val cFloat: real conversion
    val cDouble: real conversion
    
    (* When a pointer e.g. a string may be null. *)
    val cOptionPtr: 'a conversion -> 'a option conversion

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

    (* structs. *)
    val cStruct2: 'a conversion * 'b conversion -> ('a * 'b) conversion
    val cStruct3: 'a conversion * 'b conversion * 'c conversion -> ('a*'b*'c)conversion
    val cStruct4: 'a conversion * 'b conversion * 'c conversion * 'd conversion -> ('a*'b*'c*'d)conversion
    val cStruct5: 'a conversion * 'b conversion * 'c conversion * 'd conversion *
                      'e conversion -> ('a*'b*'c*'d*'e)conversion
    val cStruct6: 'a conversion * 'b conversion * 'c conversion * 'd conversion *
                      'e conversion * 'f conversion -> ('a*'b*'c*'d*'e*'f)conversion
    val cStruct7: 'a conversion * 'b conversion * 'c conversion * 'd conversion *
                      'e conversion * 'f conversion * 'g conversion -> ('a*'b*'c*'d*'e*'f*'g)conversion
    val cStruct8: 'a conversion * 'b conversion * 'c conversion * 'd conversion *
                      'e conversion * 'f conversion * 'g conversion * 'h conversion -> ('a*'b*'c*'d*'e*'f*'g*'h)conversion
    val cStruct9: 'a conversion * 'b conversion * 'c conversion * 'd conversion *
                       'e conversion * 'f conversion * 'g conversion * 'h conversion * 'i conversion ->
                       ('a*'b*'c*'d*'e*'f*'g*'h*'i)conversion
    val cStruct10: 'a conversion * 'b conversion * 'c conversion * 'd conversion * 'e conversion * 'f conversion * 'g conversion *
                   'h conversion * 'i conversion * 'j conversion -> ('a*'b*'c*'d*'e*'f*'g*'h*'i*'j)conversion
    val cStruct11: 'a conversion * 'b conversion * 'c conversion * 'd conversion * 'e conversion * 'f conversion * 'g conversion *
                   'h conversion * 'i conversion * 'j conversion * 'k conversion -> ('a*'b*'c*'d*'e*'f*'g*'h*'i*'j*'k)conversion
    val cStruct12: 'a conversion * 'b conversion * 'c conversion * 'd conversion * 'e conversion * 'f conversion * 'g conversion *
                   'h conversion * 'i conversion * 'j conversion * 'k conversion * 'l conversion ->
                   ('a*'b*'c*'d*'e*'f*'g*'h*'i*'j*'k*'l)conversion
    val cStruct13: 'a conversion * 'b conversion * 'c conversion * 'd conversion * 'e conversion * 'f conversion * 'g conversion *
                   'h conversion * 'i conversion * 'j conversion * 'k conversion * 'l conversion * 'm conversion ->
                   ('a*'b*'c*'d*'e*'f*'g*'h*'i*'j*'k*'l*'m)conversion
    val cStruct14: 'a conversion * 'b conversion * 'c conversion * 'd conversion * 'e conversion * 'f conversion * 'g conversion *
                   'h conversion * 'i conversion * 'j conversion * 'k conversion * 'l conversion * 'm conversion * 'n conversion ->
                   ('a*'b*'c*'d*'e*'f*'g*'h*'i*'j*'k*'l*'m*'n)conversion
    val cStruct15: 'a conversion * 'b conversion * 'c conversion * 'd conversion * 'e conversion * 'f conversion * 'g conversion *
                   'h conversion * 'i conversion * 'j conversion * 'k conversion * 'l conversion * 'm conversion * 'n conversion *
                   'o conversion -> ('a*'b*'c*'d*'e*'f*'g*'h*'i*'j*'k*'l*'m*'n*'o)conversion
    val cStruct16: 'a conversion * 'b conversion * 'c conversion * 'd conversion * 'e conversion * 'f conversion * 'g conversion *
                   'h conversion * 'i conversion * 'j conversion * 'k conversion * 'l conversion * 'm conversion * 'n conversion *
                   'o conversion * 'p conversion -> ('a*'b*'c*'d*'e*'f*'g*'h*'i*'j*'k*'l*'m*'n*'o*'p)conversion
    val cStruct17: 'a conversion * 'b conversion * 'c conversion * 'd conversion * 'e conversion * 'f conversion * 'g conversion *
                   'h conversion * 'i conversion * 'j conversion * 'k conversion * 'l conversion * 'm conversion * 'n conversion *
                   'o conversion * 'p conversion * 'q conversion ->
                    ('a*'b*'c*'d*'e*'f*'g*'h*'i*'j*'k*'l*'m*'n*'o*'p*'q)conversion
    val cStruct18: 'a conversion * 'b conversion * 'c conversion * 'd conversion * 'e conversion * 'f conversion * 'g conversion *
                   'h conversion * 'i conversion * 'j conversion * 'k conversion * 'l conversion * 'm conversion * 'n conversion *
                   'o conversion * 'p conversion * 'q conversion * 'r conversion ->
                    ('a*'b*'c*'d*'e*'f*'g*'h*'i*'j*'k*'l*'m*'n*'o*'p*'q*'r)conversion
    val cStruct19: 'a conversion * 'b conversion * 'c conversion * 'd conversion * 'e conversion * 'f conversion * 'g conversion *
                   'h conversion * 'i conversion * 'j conversion * 'k conversion * 'l conversion * 'm conversion * 'n conversion *
                   'o conversion * 'p conversion * 'q conversion * 'r conversion * 's conversion ->
                    ('a*'b*'c*'d*'e*'f*'g*'h*'i*'j*'k*'l*'m*'n*'o*'p*'q*'r*'s)conversion
    val cStruct20: 'a conversion * 'b conversion * 'c conversion * 'd conversion * 'e conversion * 'f conversion * 'g conversion *
                   'h conversion * 'i conversion * 'j conversion * 'k conversion * 'l conversion * 'm conversion * 'n conversion *
                   'o conversion * 'p conversion * 'q conversion * 'r conversion * 's conversion * 't conversion ->
                    ('a*'b*'c*'d*'e*'f*'g*'h*'i*'j*'k*'l*'m*'n*'o*'p*'q*'r*'s*'t)conversion

    val call0withAbi: LibFFI.abi -> symbol -> unit -> 'a conversion -> unit -> 'a
    val call0: symbol -> unit -> 'a conversion -> unit -> 'a
    val call1withAbi: LibFFI.abi -> symbol -> 'a conversion -> 'b conversion -> 'a -> 'b
    val call1: symbol -> 'a conversion -> 'b conversion -> 'a -> 'b
    val call2withAbi: LibFFI.abi -> symbol -> 'a conversion * 'b conversion -> 'c conversion -> 'a * 'b -> 'c
    val call2: symbol -> 'a conversion * 'b conversion -> 'c conversion -> 'a * 'b -> 'c
    val call3withAbi:
        LibFFI.abi -> symbol -> 'a conversion * 'b conversion * 'c conversion -> 'd conversion -> 'a * 'b * 'c -> 'd
    val call3: symbol -> 'a conversion * 'b conversion * 'c conversion -> 'd conversion -> 'a * 'b * 'c -> 'd
    val call4withAbi:
        LibFFI.abi -> symbol -> 'a conversion * 'b conversion * 'c conversion * 'd conversion -> 'e conversion ->
            'a * 'b * 'c * 'd -> 'e
    val call4: symbol -> 'a conversion * 'b conversion * 'c conversion * 'd conversion -> 'e conversion ->
            'a * 'b * 'c * 'd -> 'e
    val call5withAbi:
        LibFFI.abi -> symbol -> 'a conversion * 'b conversion * 'c conversion * 'd conversion * 'e conversion ->
            'f conversion -> 'a * 'b * 'c * 'd * 'e -> 'f
    val call5:
        symbol -> 'a conversion * 'b conversion * 'c conversion * 'd conversion *  'e conversion ->
            'f conversion -> 'a * 'b * 'c * 'd * 'e -> 'f
    val call6withAbi:
        LibFFI.abi -> symbol -> 'a conversion * 'b conversion * 'c conversion * 'd conversion * 'e conversion *
             'f conversion -> 'g conversion -> 'a * 'b * 'c * 'd * 'e * 'f -> 'g
    val call6:
        symbol -> 'a conversion * 'b conversion * 'c conversion * 'd conversion * 'e conversion *
             'f conversion -> 'g conversion -> 'a * 'b * 'c * 'd * 'e * 'f -> 'g
    val call7withAbi:
        LibFFI.abi -> symbol -> 'a conversion * 'b conversion * 'c conversion * 'd conversion * 'e conversion *
             'f conversion * 'g conversion -> 'h conversion ->
             'a * 'b * 'c * 'd * 'e * 'f * 'g -> 'h
    val call7:
        symbol -> 'a conversion * 'b conversion * 'c conversion * 'd conversion * 'e conversion *
             'f conversion * 'g conversion -> 'h conversion ->
             'a * 'b * 'c * 'd * 'e * 'f * 'g -> 'h
    val call8withAbi:
        LibFFI.abi -> symbol -> 'a conversion * 'b conversion * 'c conversion * 'd conversion * 'e conversion *
             'f conversion * 'g conversion * 'h conversion -> 'i conversion ->
             'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h -> 'i
    val call8:
        symbol -> 'a conversion * 'b conversion * 'c conversion * 'd conversion * 'e conversion *
             'f conversion * 'g conversion * 'h conversion -> 'i conversion ->
             'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h -> 'i
    val call9withAbi:
        LibFFI.abi -> symbol -> 'a conversion * 'b conversion * 'c conversion * 'd conversion * 'e conversion *
             'f conversion * 'g conversion * 'h conversion * 'i conversion ->
             'j conversion -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i -> 'j
    val call9:
        symbol -> 'a conversion * 'b conversion * 'c conversion * 'd conversion * 'e conversion *
             'f conversion * 'g conversion * 'h conversion * 'i conversion ->
             'j conversion -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i -> 'j
    val call10withAbi:
        LibFFI.abi -> symbol -> 'a conversion * 'b conversion * 'c conversion * 'd conversion * 'e conversion *
             'f conversion * 'g conversion * 'h conversion * 'i conversion * 'j conversion ->
             'k conversion -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j -> 'k
    val call10:
        symbol -> 'a conversion * 'b conversion * 'c conversion * 'd conversion * 'e conversion *
             'f conversion * 'g conversion * 'h conversion * 'i conversion * 'j conversion ->
             'k conversion -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j -> 'k
    val call11withAbi:
        LibFFI.abi -> symbol -> 'a conversion * 'b conversion * 'c conversion * 'd conversion * 'e conversion *
             'f conversion * 'g conversion * 'h conversion * 'i conversion * 'j conversion * 'k conversion ->
             'l conversion -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k -> 'l
    val call11:
        symbol -> 'a conversion * 'b conversion * 'c conversion * 'd conversion * 'e conversion *
             'f conversion * 'g conversion * 'h conversion * 'i conversion * 'j conversion * 'k conversion ->
             'l conversion -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k -> 'l
    val call12withAbi:
        LibFFI.abi -> symbol -> 'a conversion * 'b conversion * 'c conversion * 'd conversion * 'e conversion *
             'f conversion * 'g conversion * 'h conversion * 'i conversion * 'j conversion * 'k conversion *
             'l conversion -> 'm conversion ->
             'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l -> 'm
    val call12:
        symbol -> 'a conversion * 'b conversion * 'c conversion * 'd conversion * 'e conversion *
             'f conversion * 'g conversion * 'h conversion * 'i conversion * 'j conversion * 'k conversion *
             'l conversion -> 'm conversion ->
             'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l -> 'm
    val call13withAbi:
        LibFFI.abi -> symbol -> 'a conversion * 'b conversion * 'c conversion * 'd conversion * 'e conversion *
             'f conversion * 'g conversion * 'h conversion * 'i conversion * 'j conversion * 'k conversion *
             'l conversion * 'm conversion -> 'n conversion ->
             'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm -> 'n
    val call13:
        symbol -> 'a conversion * 'b conversion * 'c conversion * 'd conversion * 'e conversion *
             'f conversion * 'g conversion * 'h conversion * 'i conversion * 'j conversion * 'k conversion *
             'l conversion * 'm conversion -> 'n conversion ->
             'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm -> 'n
    val call14withAbi:
        LibFFI.abi -> symbol -> 'a conversion * 'b conversion * 'c conversion * 'd conversion * 'e conversion *
             'f conversion * 'g conversion * 'h conversion * 'i conversion * 'j conversion * 'k conversion *
             'l conversion * 'm conversion * 'n conversion ->
            'o conversion -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm * 'n -> 'o
    val call14:
        symbol -> 'a conversion * 'b conversion * 'c conversion * 'd conversion * 'e conversion *
             'f conversion * 'g conversion * 'h conversion * 'i conversion * 'j conversion * 'k conversion *
             'l conversion * 'm conversion * 'n conversion ->
            'o conversion -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm * 'n -> 'o
end;

structure Foreign:> FOREIGN =
struct
    fun id x = x
    exception Foreign = RunCall.Foreign

    open ForeignConstants
    
    structure Memory = ForeignMemory
    infix 6 ++ --

    (* Internal utility function. *)
    fun alignUp(s, align) = Word.andb(s + align-0w1, ~ align)
    
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
                if a = null
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
    fun getSymbol(lib: library) (name: string): symbol =
        Memory.memoise (fn s => System.getSymbol(lib(), s)) name

    (* This forces the symbol to be loaded.  The result is NOT memoised. *)
    fun symbolAsAddress(s: symbol): Memory.voidStar = s()

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
                    val argResVec = malloc(resultSize + #size saPointer * Word.fromInt nArgs)
                    val argLocn = argResVec ++ resultSize
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
                        if nArgs = 0 then null else malloc(#size saPointer * Word.fromInt nArgs)
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
            fun load(m: voidStar): int =
                if wordSize = 0w4
                then
                let
                    val v1 = get32(m, 0w0) and v2 = get32(m, 0w1)
                in
                    if bigEndian
                    then IntInf.<<(Word32.toIntX v1, 0w32) + Word32.toInt v2
                    else IntInf.<<(Word32.toIntX v2, 0w32) + Word32.toInt v1
                end
                else SysWord.toIntX(get64(m, 0w0))

            val max = IntInf.<<(1, 0w63) - 1 and min = ~ (IntInf.<<(1, 0w63))

            fun store(m: voidStar, i: int) =
                if wordSize = 0w4
                then
                let
                    val _ = checkRange(i, min, max)
                    val lo = Word32.fromInt i and hi = Word32.fromInt (IntInf.~>>(i, 0w32))
                in
                    if bigEndian
                    then (set32(m, 0w0, hi); set32(m, 0w1, lo))
                    else (set32(m, 0w0, lo); set32(m, 0w1, hi))
                end
                else set64(m, 0w0, SysWord.fromInt(checkRange(i, min, max)))
        in
            val cInt64: int conversion =
                { load=load, store=store, release = noFree, ctype = cTypeInt64 }
        end

        local
            fun load(m: voidStar): int =
                if wordSize = 0w4
                then
                let
                    val v1 = get32(m, 0w0) and v2 = get32(m, 0w1)
                in
                    if bigEndian
                    then IntInf.<<(Word32.toInt v1, 0w32) + Word32.toInt v2
                    else IntInf.<<(Word32.toInt v2, 0w32) + Word32.toInt v1
                end
                else SysWord.toInt(get64(m, 0w0))

            val max = IntInf.<<(1, 0w64) - 1

            fun store(m: voidStar, i: int) =
                if wordSize = 0w4
                then
                let
                    val _ = checkRange(i, 0, max)
                    val lo = Word32.fromInt i and hi = Word32.fromInt (IntInf.~>>(i, 0w32))
                in
                    if bigEndian
                    then (set32(m, 0w0, hi); set32(m, 0w1, lo))
                    else (set32(m, 0w0, lo); set32(m, 0w1, hi))
                end
                else set64(m, 0w0, SysWord.fromInt(checkRange(i, 0, max)))
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
                fun sLen i = if get8(sAddr, i) = 0w0 then i else sLen(i+0w1)
                val length = sLen 0w0
                fun loadChar i =
                    Char.chr(Word8.toInt(get8(sAddr, Word.fromInt i)))
            in
                CharVector.tabulate(Word.toInt length, loadChar)
            end
            fun store(v: voidStar, s: string) =
            let
                val sLen = Word.fromInt(String.size s)
                val sMem = malloc(sLen + 0w1)
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

        (* This is used if we want to pass NULL rather than a pointer in some cases. *)
        fun cOptionPtr({load, store, release, ctype}:'a conversion): 'a option conversion =
            if #typeCode(extractFFItype(#ffiType ctype ())) <> ffiTypeCodePointer
            then raise Foreign "cOptionPtr must be applied to a pointer type"
            else
            let
                fun loadOpt(s: voidStar) =
                    if getAddress(s, 0w0) = null then NONE else SOME(load s)

                fun storeOpt(v: voidStar, NONE) = setAddress(v, 0w0, null)
                |   storeOpt(v: voidStar, SOME s) = store(v, s)
                
                fun releaseOpt(_, NONE) = ()
                |   releaseOpt(v: voidStar, SOME s) = release(v, s)
            in
                { load=loadOpt, store=storeOpt, release = releaseOpt, ctype = cTypePointer }
            end

        local
            (* Word8Vector.vector to C array of bytes.  It is only possible to
               do this one way because conversion from a C array requires
               us to know the size. *)
            fun load _ = raise Foreign "cByteArray cannot convert from C to ML"

            fun store(v: voidStar, s: Word8Vector.vector) =
            let
                open Word8Vector
                val sLen = Word.fromInt(length s)
                val sMem = malloc sLen
                val () = appi(fn(i, b) => set8(sMem, Word.fromInt i, b)) s
            in
                setAddress(v, 0w0, sMem)
            end
            
            fun release(s: voidStar, _) = Memory.free(getAddress(s, 0w0))
        in
            val cByteArray: Word8Vector.vector conversion =
                { load=load, store=store, release = release, ctype = cTypePointer }
        end
    end

    (* Replace the "release" function by a null function.  This is intended for situations
       where an argument should not be deleted once the function completes.
       This also prevents copying of the result if necessary. *)
    fun permanent({load, store, ctype, ...}: 'a conversion): 'a conversion =
        { release=fn _ => (), load=load, store=store, ctype=ctype }
 
    val op ++ = Memory.++

    fun cStruct2(a: 'a conversion, b: 'b conversion): ('a*'b)conversion =
    let
        val {load=loada, store=storea, release=releasea, ctype = ctypea as {size=sizea, ... }} = a
        and {load=loadb, store=storeb, release=releaseb, ctype = ctypeb as {align=alignb, ... }} = b
        
        val offsetb = alignUp(sizea, alignb)
        fun load s = (loada s, loadb(s ++ offsetb))
        and store (s, (a, b)) = (storea(s, a); storeb(s ++ offsetb, b))
        and release(s, (a, b)) = (releasea(s, a); releaseb(s ++ offsetb, b))

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

        fun load s = (loada s, loadb(s ++ offsetb), loadc(s ++ offsetc))
        and store (s, (a, b, c)) =
            (storea(s, a); storeb(s ++ offsetb, b); storec(s ++ offsetc, c))
        and release(s, (a, b, c)) = (releasea(s, a); releaseb(s ++ offsetb, b); releasec(s ++ offsetc, c))
    in
        {load=load, store=store, release=release, ctype = LowLevel.cStruct[ctypea, ctypeb, ctypec]}
    end

    fun cStruct4(a: 'a conversion, b: 'b conversion, c: 'c conversion, d: 'd conversion): ('a*'b*'c*'d)conversion =
    let
        val {load=loada, store=storea, release=releasea, ctype = ctypea as {size=sizea, ...} } = a
        and {load=loadb, store=storeb, release=releaseb, ctype = ctypeb as {size=sizeb, align=alignb, ...} } = b
        and {load=loadc, store=storec, release=releasec, ctype = ctypec as {size=sizec, align=alignc, ...} } = c
        and {load=loadd, store=stored, release=released, ctype = ctyped as {align=alignd, ...} } = d
 
        val offsetb = alignUp(sizea, alignb)
        val offsetc = alignUp(offsetb + sizeb, alignc)
        val offsetd = alignUp(offsetc + sizec, alignd)

        fun load s = (loada s, loadb(s ++ offsetb), loadc(s ++ offsetc), loadd(s ++ offsetd))
        and store (s, (a, b, c, d)) =
            (storea(s, a); storeb(s ++ offsetb, b); storec(s ++ offsetc, c); stored(s ++ offsetd, d))
        and release(s, (a, b, c, d)) =
            (releasea(s, a); releaseb(s ++ offsetb, b); releasec(s ++ offsetc, c); released(s ++ offsetd, d))
    in
        {load=load, store=store, release=release, ctype = LowLevel.cStruct[ctypea, ctypeb, ctypec, ctyped]}
    end

    fun cStruct5(a: 'a conversion, b: 'b conversion, c: 'c conversion, d: 'd conversion,
                 e: 'e conversion): ('a*'b*'c*'d*'e)conversion =
    let
        val {load=loada, store=storea, release=releasea, ctype = ctypea as {size=sizea, ...} } = a
        and {load=loadb, store=storeb, release=releaseb, ctype = ctypeb as {size=sizeb, align=alignb, ...} } = b
        and {load=loadc, store=storec, release=releasec, ctype = ctypec as {size=sizec, align=alignc, ...} } = c
        and {load=loadd, store=stored, release=released, ctype = ctyped as {size=sized, align=alignd, ...} } = d
        and {load=loade, store=storee, release=releasee, ctype = ctypee as {align=aligne, ...} } = e

        val offsetb = alignUp(sizea, alignb)
        val offsetc = alignUp(offsetb + sizeb, alignc)
        val offsetd = alignUp(offsetc + sizec, alignd)
        val offsete = alignUp(offsetd + sized, aligne)

        fun load s =
            (loada s, loadb(s ++ offsetb), loadc(s ++ offsetc), loadd(s ++ offsetd), loade(s ++ offsete))
        and store (s, (a, b, c, d, e)) =
            (storea(s, a); storeb(s ++ offsetb, b); storec(s ++ offsetc, c); stored(s ++ offsetd, d); storee(s ++ offsete, e))
        and release(s, (a, b, c, d, e)) =
            (releasea(s, a); releaseb(s ++ offsetb, b); releasec(s ++ offsetc, c);
             released(s ++ offsetd, d); releasee(s ++ offsete, e))
    in
        {load=load, store=store, release=release, ctype = LowLevel.cStruct[ctypea, ctypeb, ctypec, ctyped, ctypee]}
    end

    fun cStruct6(a: 'a conversion, b: 'b conversion, c: 'c conversion, d: 'd conversion,
                 e: 'e conversion, f: 'f conversion): ('a*'b*'c*'d*'e*'f)conversion =
    let
        val {load=loada, store=storea, release=releasea, ctype = ctypea as {size=sizea, ...} } = a
        and {load=loadb, store=storeb, release=releaseb, ctype = ctypeb as {size=sizeb, align=alignb, ...} } = b
        and {load=loadc, store=storec, release=releasec, ctype = ctypec as {size=sizec, align=alignc, ...} } = c
        and {load=loadd, store=stored, release=released, ctype = ctyped as {size=sized, align=alignd, ...} } = d
        and {load=loade, store=storee, release=releasee, ctype = ctypee as {size=sizee, align=aligne, ...} } = e
        and {load=loadf, store=storef, release=releasef, ctype = ctypef as {align=alignf, ...} } = f

        val offsetb = alignUp(sizea, alignb)
        val offsetc = alignUp(offsetb + sizeb, alignc)
        val offsetd = alignUp(offsetc + sizec, alignd)
        val offsete = alignUp(offsetd + sized, aligne)
        val offsetf = alignUp(offsete + sizee, alignf)

        fun load s =
            (loada s, loadb(s ++ offsetb), loadc(s ++ offsetc), loadd(s ++ offsetd),
             loade(s ++ offsete), loadf(s ++ offsetf))
        and store (s, (a, b, c, d, e, f)) =
            (storea(s, a); storeb(s ++ offsetb, b); storec(s ++ offsetc, c); stored(s ++ offsetd, d);
             storee(s ++ offsete, e); storef(s ++ offsetf, f))
        and release(s, (a, b, c, d, e, f)) =
            (releasea(s, a); releaseb(s ++ offsetb, b); releasec(s ++ offsetc, c); released(s ++ offsetd, d);
             releasee(s ++ offsete, e); releasef(s ++ offsetf, f))
    in
        {load=load, store=store, release=release, ctype = LowLevel.cStruct[ctypea, ctypeb, ctypec, ctyped, ctypee, ctypef]}
    end

    fun cStruct7(a: 'a conversion, b: 'b conversion, c: 'c conversion, d: 'd conversion,
                 e: 'e conversion, f: 'f conversion, g: 'g conversion): ('a*'b*'c*'d*'e*'f*'g)conversion =
    let
        val {load=loada, store=storea, release=releasea, ctype = ctypea as {size=sizea, ...} } = a
        and {load=loadb, store=storeb, release=releaseb, ctype = ctypeb as {size=sizeb, align=alignb, ...} } = b
        and {load=loadc, store=storec, release=releasec, ctype = ctypec as {size=sizec, align=alignc, ...} } = c
        and {load=loadd, store=stored, release=released, ctype = ctyped as {size=sized, align=alignd, ...} } = d
        and {load=loade, store=storee, release=releasee, ctype = ctypee as {size=sizee, align=aligne, ...} } = e
        and {load=loadf, store=storef, release=releasef, ctype = ctypef as {size=sizef, align=alignf, ...} } = f
        and {load=loadg, store=storeg, release=releaseg, ctype = ctypeg as {align=aligng, ...} } = g

        val offsetb = alignUp(sizea, alignb)
        val offsetc = alignUp(offsetb + sizeb, alignc)
        val offsetd = alignUp(offsetc + sizec, alignd)
        val offsete = alignUp(offsetd + sized, aligne)
        val offsetf = alignUp(offsete + sizee, alignf)
        val offsetg = alignUp(offsetf + sizef, aligng)

        fun load s =
            (loada s, loadb(s ++ offsetb), loadc(s ++ offsetc), loadd(s ++ offsetd),
             loade(s ++ offsete), loadf(s ++ offsetf), loadg(s ++ offsetg))
        and store (s, (a, b, c, d, e, f, g)) =
            (storea(s, a); storeb(s ++ offsetb, b); storec(s ++ offsetc, c); stored(s ++ offsetd, d);
             storee(s ++ offsete, e); storef(s ++ offsetf, f); storeg(s ++ offsetg, g))
        and release(s, (a, b, c, d, e, f, g)) =
            (releasea(s, a); releaseb(s ++ offsetb, b); releasec(s ++ offsetc, c); released(s ++ offsetd, d);
             releasee(s ++ offsete, e); releasef(s ++ offsetf, f); releaseg(s ++ offsetg, g))
    in
        {load=load, store=store, release=release, ctype = LowLevel.cStruct[ctypea, ctypeb, ctypec, ctyped, ctypee, ctypef, ctypeg]}
    end

    fun cStruct8(a: 'a conversion, b: 'b conversion, c: 'c conversion, d: 'd conversion,
                 e: 'e conversion, f: 'f conversion, g: 'g conversion, h: 'h conversion):
                    ('a*'b*'c*'d*'e*'f*'g*'h)conversion =
    let
        val {load=loada, store=storea, release=releasea, ctype = ctypea as {size=sizea, ...} } = a
        and {load=loadb, store=storeb, release=releaseb, ctype = ctypeb as {size=sizeb, align=alignb, ...} } = b
        and {load=loadc, store=storec, release=releasec, ctype = ctypec as {size=sizec, align=alignc, ...} } = c
        and {load=loadd, store=stored, release=released, ctype = ctyped as {size=sized, align=alignd, ...} } = d
        and {load=loade, store=storee, release=releasee, ctype = ctypee as {size=sizee, align=aligne, ...} } = e
        and {load=loadf, store=storef, release=releasef, ctype = ctypef as {size=sizef, align=alignf, ...} } = f
        and {load=loadg, store=storeg, release=releaseg, ctype = ctypeg as {size=sizeg, align=aligng, ...} } = g
        and {load=loadh, store=storeh, release=releaseh, ctype = ctypeh as {align=alignh, ...} } = h

        val offsetb = alignUp(sizea, alignb)
        val offsetc = alignUp(offsetb + sizeb, alignc)
        val offsetd = alignUp(offsetc + sizec, alignd)
        val offsete = alignUp(offsetd + sized, aligne)
        val offsetf = alignUp(offsete + sizee, alignf)
        val offsetg = alignUp(offsetf + sizef, aligng)
        val offseth = alignUp(offsetg + sizeg, alignh)

        fun load s =
            (loada s, loadb(s ++ offsetb), loadc(s ++ offsetc), loadd(s ++ offsetd),
             loade(s ++ offsete), loadf(s ++ offsetf), loadg(s ++ offsetg), loadh(s ++ offseth))
        and store (s, (a, b, c, d, e, f, g, h)) =
            (storea(s, a); storeb(s ++ offsetb, b); storec(s ++ offsetc, c); stored(s ++ offsetd, d);
             storee(s ++ offsete, e); storef(s ++ offsetf, f); storeg(s ++ offsetg, g); storeh(s ++ offseth, h))
        and release(s, (a, b, c, d, e, f, g, h)) =
            (releasea(s, a); releaseb(s ++ offsetb, b); releasec(s ++ offsetc, c); released(s ++ offsetd, d);
             releasee(s ++ offsete, e); releasef(s ++ offsetf, f); releaseg(s ++ offsetg, g);
             releaseh(s ++ offseth, h))
    in
        {load=load, store=store, release=release,
         ctype = LowLevel.cStruct[ctypea, ctypeb, ctypec, ctyped, ctypee, ctypef, ctypeg, ctypeh]}
    end

    fun cStruct9(a: 'a conversion, b: 'b conversion, c: 'c conversion, d: 'd conversion,
                 e: 'e conversion, f: 'f conversion, g: 'g conversion, h: 'h conversion,
                 i: 'i conversion): ('a*'b*'c*'d*'e*'f*'g*'h*'i)conversion =
    let
        val {load=loada, store=storea, release=releasea, ctype = ctypea as {size=sizea, ...} } = a
        and {load=loadb, store=storeb, release=releaseb, ctype = ctypeb as {size=sizeb, align=alignb, ...} } = b
        and {load=loadc, store=storec, release=releasec, ctype = ctypec as {size=sizec, align=alignc, ...} } = c
        and {load=loadd, store=stored, release=released, ctype = ctyped as {size=sized, align=alignd, ...} } = d
        and {load=loade, store=storee, release=releasee, ctype = ctypee as {size=sizee, align=aligne, ...} } = e
        and {load=loadf, store=storef, release=releasef, ctype = ctypef as {size=sizef, align=alignf, ...} } = f
        and {load=loadg, store=storeg, release=releaseg, ctype = ctypeg as {size=sizeg, align=aligng, ...} } = g
        and {load=loadh, store=storeh, release=releaseh, ctype = ctypeh as {size=sizeh, align=alignh, ...} } = h
        and {load=loadi, store=storei, release=releasei, ctype = ctypei as {align=aligni, ...} } = i

        val offsetb = alignUp(sizea, alignb)
        val offsetc = alignUp(offsetb + sizeb, alignc)
        val offsetd = alignUp(offsetc + sizec, alignd)
        val offsete = alignUp(offsetd + sized, aligne)
        val offsetf = alignUp(offsete + sizee, alignf)
        val offsetg = alignUp(offsetf + sizef, aligng)
        val offseth = alignUp(offsetg + sizeg, alignh)
        val offseti = alignUp(offseth + sizeh, aligni)

        fun load s =
            (loada s, loadb(s ++ offsetb), loadc(s ++ offsetc), loadd(s ++ offsetd),
             loade(s ++ offsete), loadf(s ++ offsetf), loadg(s ++ offsetg),
             loadh(s ++ offseth), loadi(s ++ offseti))
        and store (s, (a, b, c, d, e, f, g, h, i)) =
            (storea(s, a); storeb(s ++ offsetb, b); storec(s ++ offsetc, c); stored(s ++ offsetd, d);
             storee(s ++ offsete, e); storef(s ++ offsetf, f); storeg(s ++ offsetg, g);
             storeh(s ++ offseth, h); storei(s ++ offseti, i))
        and release(s, (a, b, c, d, e, f, g, h, i)) =
            (releasea(s, a); releaseb(s ++ offsetb, b); releasec(s ++ offsetc, c); released(s ++ offsetd, d);
             releasee(s ++ offsete, e); releasef(s ++ offsetf, f); releaseg(s ++ offsetg, g);
             releaseh(s ++ offseth, h); releasei(s ++ offseti, i))
    in
        {load=load, store=store, release=release,
         ctype = LowLevel.cStruct[ctypea, ctypeb, ctypec, ctyped, ctypee, ctypef, ctypeg, ctypeh, ctypei]}
    end

    fun cStruct10(a: 'a conversion, b: 'b conversion, c: 'c conversion, d: 'd conversion,
                  e: 'e conversion, f: 'f conversion, g: 'g conversion, h: 'h conversion,
                  i: 'i conversion, j: 'j conversion):
                  ('a*'b*'c*'d*'e*'f*'g*'h*'i*'j)conversion =
    let
        val {load=loada, store=storea, release=releasea, ctype = ctypea as {size=sizea, ...} } = a
        and {load=loadb, store=storeb, release=releaseb, ctype = ctypeb as {size=sizeb, align=alignb, ...} } = b
        and {load=loadc, store=storec, release=releasec, ctype = ctypec as {size=sizec, align=alignc, ...} } = c
        and {load=loadd, store=stored, release=released, ctype = ctyped as {size=sized, align=alignd, ...} } = d
        and {load=loade, store=storee, release=releasee, ctype = ctypee as {size=sizee, align=aligne, ...} } = e
        and {load=loadf, store=storef, release=releasef, ctype = ctypef as {size=sizef, align=alignf, ...} } = f
        and {load=loadg, store=storeg, release=releaseg, ctype = ctypeg as {size=sizeg, align=aligng, ...} } = g
        and {load=loadh, store=storeh, release=releaseh, ctype = ctypeh as {size=sizeh, align=alignh, ...} } = h
        and {load=loadi, store=storei, release=releasei, ctype = ctypei as {size=sizei, align=aligni, ...} } = i
        and {load=loadj, store=storej, release=releasej, ctype = ctypej as {align=alignj, ...} } = j

        val offsetb = alignUp(sizea, alignb)
        val offsetc = alignUp(offsetb + sizeb, alignc)
        val offsetd = alignUp(offsetc + sizec, alignd)
        val offsete = alignUp(offsetd + sized, aligne)
        val offsetf = alignUp(offsete + sizee, alignf)
        val offsetg = alignUp(offsetf + sizef, aligng)
        val offseth = alignUp(offsetg + sizeg, alignh)
        val offseti = alignUp(offseth + sizeh, aligni)
        val offsetj = alignUp(offseti + sizei, alignj)

        fun load s =
            (loada s, loadb(s ++ offsetb), loadc(s ++ offsetc), loadd(s ++ offsetd),
             loade(s ++ offsete), loadf(s ++ offsetf), loadg(s ++ offsetg),
             loadh(s ++ offseth), loadi(s ++ offseti), loadj(s ++ offsetj))
        and store (x, (a, b, c, d, e, f, g, h, i, j)) =
            (storea(x, a); storeb(x ++ offsetb, b); storec(x ++ offsetc, c); stored(x ++ offsetd, d);
             storee(x ++ offsete, e); storef(x ++ offsetf, f); storeg(x ++ offsetg, g);
             storeh(x ++ offseth, h); storei(x ++ offseti, i); storej(x ++ offsetj, j))
        and release(x, (a, b, c, d, e, f, g, h, i, j)) =
            (releasea(x, a); releaseb(x ++ offsetb, b); releasec(x ++ offsetc, c); released(x ++ offsetd, d);
             releasee(x ++ offsete, e); releasef(x ++ offsetf, f); releaseg(x ++ offsetg, g);
             releaseh(x ++ offseth, h); releasei(x ++ offseti, i); releasej(x ++ offsetj, j))
    in
        {load=load, store=store, release=release,
         ctype = LowLevel.cStruct[ctypea, ctypeb, ctypec, ctyped, ctypee, ctypef, ctypeg, ctypeh, ctypei, ctypej]}
    end

    fun cStruct11(a: 'a conversion, b: 'b conversion, c: 'c conversion, d: 'd conversion,
                  e: 'e conversion, f: 'f conversion, g: 'g conversion, h: 'h conversion,
                  i: 'i conversion, j: 'j conversion, k: 'k conversion):
                  ('a*'b*'c*'d*'e*'f*'g*'h*'i*'j*'k)conversion =
    let
        val {load=loada, store=storea, release=releasea, ctype = ctypea as {size=sizea, ...} } = a
        and {load=loadb, store=storeb, release=releaseb, ctype = ctypeb as {size=sizeb, align=alignb, ...} } = b
        and {load=loadc, store=storec, release=releasec, ctype = ctypec as {size=sizec, align=alignc, ...} } = c
        and {load=loadd, store=stored, release=released, ctype = ctyped as {size=sized, align=alignd, ...} } = d
        and {load=loade, store=storee, release=releasee, ctype = ctypee as {size=sizee, align=aligne, ...} } = e
        and {load=loadf, store=storef, release=releasef, ctype = ctypef as {size=sizef, align=alignf, ...} } = f
        and {load=loadg, store=storeg, release=releaseg, ctype = ctypeg as {size=sizeg, align=aligng, ...} } = g
        and {load=loadh, store=storeh, release=releaseh, ctype = ctypeh as {size=sizeh, align=alignh, ...} } = h
        and {load=loadi, store=storei, release=releasei, ctype = ctypei as {size=sizei, align=aligni, ...} } = i
        and {load=loadj, store=storej, release=releasej, ctype = ctypej as {size=sizej, align=alignj, ...} } = j
        and {load=loadk, store=storek, release=releasek, ctype = ctypek as {align=alignk, ...} } = k

        val offsetb = alignUp(sizea, alignb)
        val offsetc = alignUp(offsetb + sizeb, alignc)
        val offsetd = alignUp(offsetc + sizec, alignd)
        val offsete = alignUp(offsetd + sized, aligne)
        val offsetf = alignUp(offsete + sizee, alignf)
        val offsetg = alignUp(offsetf + sizef, aligng)
        val offseth = alignUp(offsetg + sizeg, alignh)
        val offseti = alignUp(offseth + sizeh, aligni)
        val offsetj = alignUp(offseti + sizei, alignj)
        val offsetk = alignUp(offsetj + sizej, alignk)

        fun load s =
            (loada s, loadb(s ++ offsetb), loadc(s ++ offsetc), loadd(s ++ offsetd),
             loade(s ++ offsete), loadf(s ++ offsetf), loadg(s ++ offsetg),
             loadh(s ++ offseth), loadi(s ++ offseti), loadj(s ++ offsetj),
             loadk(s ++ offsetk))
        and store (x, (a, b, c, d, e, f, g, h, i, j, k)) =
            (storea(x, a); storeb(x ++ offsetb, b); storec(x ++ offsetc, c); stored(x ++ offsetd, d);
             storee(x ++ offsete, e); storef(x ++ offsetf, f); storeg(x ++ offsetg, g);
             storeh(x ++ offseth, h); storei(x ++ offseti, i); storej(x ++ offsetj, j);
             storek(x ++ offsetk, k))
        and release(x, (a, b, c, d, e, f, g, h, i, j, k)) =
            (releasea(x, a); releaseb(x ++ offsetb, b); releasec(x ++ offsetc, c); released(x ++ offsetd, d);
             releasee(x ++ offsete, e); releasef(x ++ offsetf, f); releaseg(x ++ offsetg, g);
             releaseh(x ++ offseth, h); releasei(x ++ offseti, i); releasej(x ++ offsetj, j);
             releasek(x ++ offsetk, k))
    in
        {load=load, store=store, release=release,
         ctype = LowLevel.cStruct[ctypea, ctypeb, ctypec, ctyped, ctypee, ctypef, ctypeg, ctypeh, ctypei, ctypej,
                                  ctypek]}
    end
    
    fun cStruct12(a: 'a conversion, b: 'b conversion, c: 'c conversion, d: 'd conversion,
                  e: 'e conversion, f: 'f conversion, g: 'g conversion, h: 'h conversion,
                  i: 'i conversion, j: 'j conversion, k: 'k conversion, l: 'l conversion):
                  ('a*'b*'c*'d*'e*'f*'g*'h*'i*'j*'k*'l)conversion =
    let
        val {load=loada, store=storea, release=releasea, ctype = ctypea as {size=sizea, ...} } = a
        and {load=loadb, store=storeb, release=releaseb, ctype = ctypeb as {size=sizeb, align=alignb, ...} } = b
        and {load=loadc, store=storec, release=releasec, ctype = ctypec as {size=sizec, align=alignc, ...} } = c
        and {load=loadd, store=stored, release=released, ctype = ctyped as {size=sized, align=alignd, ...} } = d
        and {load=loade, store=storee, release=releasee, ctype = ctypee as {size=sizee, align=aligne, ...} } = e
        and {load=loadf, store=storef, release=releasef, ctype = ctypef as {size=sizef, align=alignf, ...} } = f
        and {load=loadg, store=storeg, release=releaseg, ctype = ctypeg as {size=sizeg, align=aligng, ...} } = g
        and {load=loadh, store=storeh, release=releaseh, ctype = ctypeh as {size=sizeh, align=alignh, ...} } = h
        and {load=loadi, store=storei, release=releasei, ctype = ctypei as {size=sizei, align=aligni, ...} } = i
        and {load=loadj, store=storej, release=releasej, ctype = ctypej as {size=sizej, align=alignj, ...} } = j
        and {load=loadk, store=storek, release=releasek, ctype = ctypek as {size=sizek, align=alignk, ...} } = k
        and {load=loadl, store=storel, release=releasel, ctype = ctypel as {align=alignl, ...} } = l

        val offsetb = alignUp(sizea, alignb)
        val offsetc = alignUp(offsetb + sizeb, alignc)
        val offsetd = alignUp(offsetc + sizec, alignd)
        val offsete = alignUp(offsetd + sized, aligne)
        val offsetf = alignUp(offsete + sizee, alignf)
        val offsetg = alignUp(offsetf + sizef, aligng)
        val offseth = alignUp(offsetg + sizeg, alignh)
        val offseti = alignUp(offseth + sizeh, aligni)
        val offsetj = alignUp(offseti + sizei, alignj)
        val offsetk = alignUp(offsetj + sizej, alignk)
        val offsetl = alignUp(offsetk + sizek, alignl)

        fun load s =
            (loada s, loadb(s ++ offsetb), loadc(s ++ offsetc), loadd(s ++ offsetd),
             loade(s ++ offsete), loadf(s ++ offsetf), loadg(s ++ offsetg),
             loadh(s ++ offseth), loadi(s ++ offseti), loadj(s ++ offsetj),
             loadk(s ++ offsetk), loadl(s ++ offsetl))
        and store (x, (a, b, c, d, e, f, g, h, i, j, k, l)) =
            (storea(x, a); storeb(x ++ offsetb, b); storec(x ++ offsetc, c); stored(x ++ offsetd, d);
             storee(x ++ offsete, e); storef(x ++ offsetf, f); storeg(x ++ offsetg, g);
             storeh(x ++ offseth, h); storei(x ++ offseti, i); storej(x ++ offsetj, j);
             storek(x ++ offsetk, k); storel(x ++ offsetl, l))
        and release(x, (a, b, c, d, e, f, g, h, i, j, k, l)) =
            (releasea(x, a); releaseb(x ++ offsetb, b); releasec(x ++ offsetc, c); released(x ++ offsetd, d);
             releasee(x ++ offsete, e); releasef(x ++ offsetf, f); releaseg(x ++ offsetg, g);
             releaseh(x ++ offseth, h); releasei(x ++ offseti, i); releasej(x ++ offsetj, j);
             releasek(x ++ offsetk, k); releasel(x ++ offsetl, l))
    in
        {load=load, store=store, release=release,
         ctype = LowLevel.cStruct[ctypea, ctypeb, ctypec, ctyped, ctypee, ctypef, ctypeg, ctypeh, ctypei, ctypej,
                                  ctypek, ctypel]}
    end
    
    fun cStruct13(a: 'a conversion, b: 'b conversion, c: 'c conversion, d: 'd conversion,
                  e: 'e conversion, f: 'f conversion, g: 'g conversion, h: 'h conversion,
                  i: 'i conversion, j: 'j conversion, k: 'k conversion, l: 'l conversion,
                  m: 'm conversion):
                  ('a*'b*'c*'d*'e*'f*'g*'h*'i*'j*'k*'l*'m)conversion =
    let
        val {load=loada, store=storea, release=releasea, ctype = ctypea as {size=sizea, ...} } = a
        and {load=loadb, store=storeb, release=releaseb, ctype = ctypeb as {size=sizeb, align=alignb, ...} } = b
        and {load=loadc, store=storec, release=releasec, ctype = ctypec as {size=sizec, align=alignc, ...} } = c
        and {load=loadd, store=stored, release=released, ctype = ctyped as {size=sized, align=alignd, ...} } = d
        and {load=loade, store=storee, release=releasee, ctype = ctypee as {size=sizee, align=aligne, ...} } = e
        and {load=loadf, store=storef, release=releasef, ctype = ctypef as {size=sizef, align=alignf, ...} } = f
        and {load=loadg, store=storeg, release=releaseg, ctype = ctypeg as {size=sizeg, align=aligng, ...} } = g
        and {load=loadh, store=storeh, release=releaseh, ctype = ctypeh as {size=sizeh, align=alignh, ...} } = h
        and {load=loadi, store=storei, release=releasei, ctype = ctypei as {size=sizei, align=aligni, ...} } = i
        and {load=loadj, store=storej, release=releasej, ctype = ctypej as {size=sizej, align=alignj, ...} } = j
        and {load=loadk, store=storek, release=releasek, ctype = ctypek as {size=sizek, align=alignk, ...} } = k
        and {load=loadl, store=storel, release=releasel, ctype = ctypel as {size=sizel, align=alignl, ...} } = l
        and {load=loadm, store=storem, release=releasem, ctype = ctypem as {align=alignm, ...} } = m

        val offsetb = alignUp(sizea, alignb)
        val offsetc = alignUp(offsetb + sizeb, alignc)
        val offsetd = alignUp(offsetc + sizec, alignd)
        val offsete = alignUp(offsetd + sized, aligne)
        val offsetf = alignUp(offsete + sizee, alignf)
        val offsetg = alignUp(offsetf + sizef, aligng)
        val offseth = alignUp(offsetg + sizeg, alignh)
        val offseti = alignUp(offseth + sizeh, aligni)
        val offsetj = alignUp(offseti + sizei, alignj)
        val offsetk = alignUp(offsetj + sizej, alignk)
        val offsetl = alignUp(offsetk + sizek, alignl)
        val offsetm = alignUp(offsetl + sizel, alignm)

        fun load s =
            (loada s, loadb(s ++ offsetb), loadc(s ++ offsetc), loadd(s ++ offsetd),
             loade(s ++ offsete), loadf(s ++ offsetf), loadg(s ++ offsetg),
             loadh(s ++ offseth), loadi(s ++ offseti), loadj(s ++ offsetj),
             loadk(s ++ offsetk), loadl(s ++ offsetl), loadm(s ++ offsetm))
        and store (x, (a, b, c, d, e, f, g, h, i, j, k, l, m)) =
            (storea(x, a); storeb(x ++ offsetb, b); storec(x ++ offsetc, c); stored(x ++ offsetd, d);
             storee(x ++ offsete, e); storef(x ++ offsetf, f); storeg(x ++ offsetg, g);
             storeh(x ++ offseth, h); storei(x ++ offseti, i); storej(x ++ offsetj, j);
             storek(x ++ offsetk, k); storel(x ++ offsetl, l); storem(x ++ offsetm, m))
        and release(x, (a, b, c, d, e, f, g, h, i, j, k, l, m)) =
            (releasea(x, a); releaseb(x ++ offsetb, b); releasec(x ++ offsetc, c); released(x ++ offsetd, d);
             releasee(x ++ offsete, e); releasef(x ++ offsetf, f); releaseg(x ++ offsetg, g);
             releaseh(x ++ offseth, h); releasei(x ++ offseti, i); releasej(x ++ offsetj, j);
             releasek(x ++ offsetk, k); releasel(x ++ offsetl, l); releasem(x ++ offsetm, m))
    in
        {load=load, store=store, release=release,
         ctype = LowLevel.cStruct[ctypea, ctypeb, ctypec, ctyped, ctypee, ctypef, ctypeg, ctypeh, ctypei, ctypej,
                                  ctypek, ctypel, ctypem]}
    end
    
    nonfix o

    fun cStruct14(a: 'a conversion, b: 'b conversion, c: 'c conversion, d: 'd conversion,
                  e: 'e conversion, f: 'f conversion, g: 'g conversion, h: 'h conversion,
                  i: 'i conversion, j: 'j conversion, k: 'k conversion, l: 'l conversion,
                  m: 'm conversion, n: 'n conversion):
                  ('a*'b*'c*'d*'e*'f*'g*'h*'i*'j*'k*'l*'m*'n)conversion =
    let
        val {load=loada, store=storea, release=releasea, ctype = ctypea as {size=sizea, ...} } = a
        and {load=loadb, store=storeb, release=releaseb, ctype = ctypeb as {size=sizeb, align=alignb, ...} } = b
        and {load=loadc, store=storec, release=releasec, ctype = ctypec as {size=sizec, align=alignc, ...} } = c
        and {load=loadd, store=stored, release=released, ctype = ctyped as {size=sized, align=alignd, ...} } = d
        and {load=loade, store=storee, release=releasee, ctype = ctypee as {size=sizee, align=aligne, ...} } = e
        and {load=loadf, store=storef, release=releasef, ctype = ctypef as {size=sizef, align=alignf, ...} } = f
        and {load=loadg, store=storeg, release=releaseg, ctype = ctypeg as {size=sizeg, align=aligng, ...} } = g
        and {load=loadh, store=storeh, release=releaseh, ctype = ctypeh as {size=sizeh, align=alignh, ...} } = h
        and {load=loadi, store=storei, release=releasei, ctype = ctypei as {size=sizei, align=aligni, ...} } = i
        and {load=loadj, store=storej, release=releasej, ctype = ctypej as {size=sizej, align=alignj, ...} } = j
        and {load=loadk, store=storek, release=releasek, ctype = ctypek as {size=sizek, align=alignk, ...} } = k
        and {load=loadl, store=storel, release=releasel, ctype = ctypel as {size=sizel, align=alignl, ...} } = l
        and {load=loadm, store=storem, release=releasem, ctype = ctypem as {size=sizem, align=alignm, ...} } = m
        and {load=loadn, store=storen, release=releasen, ctype = ctypen as {align=alignn, ...} } = n

        val offsetb = alignUp(sizea, alignb)
        val offsetc = alignUp(offsetb + sizeb, alignc)
        val offsetd = alignUp(offsetc + sizec, alignd)
        val offsete = alignUp(offsetd + sized, aligne)
        val offsetf = alignUp(offsete + sizee, alignf)
        val offsetg = alignUp(offsetf + sizef, aligng)
        val offseth = alignUp(offsetg + sizeg, alignh)
        val offseti = alignUp(offseth + sizeh, aligni)
        val offsetj = alignUp(offseti + sizei, alignj)
        val offsetk = alignUp(offsetj + sizej, alignk)
        val offsetl = alignUp(offsetk + sizek, alignl)
        val offsetm = alignUp(offsetl + sizel, alignm)
        val offsetn = alignUp(offsetm + sizem, alignn)

        fun load s =
            (loada s, loadb(s ++ offsetb), loadc(s ++ offsetc), loadd(s ++ offsetd),
             loade(s ++ offsete), loadf(s ++ offsetf), loadg(s ++ offsetg),
             loadh(s ++ offseth), loadi(s ++ offseti), loadj(s ++ offsetj),
             loadk(s ++ offsetk), loadl(s ++ offsetl), loadm(s ++ offsetm),
             loadn(s ++ offsetn))
        and store (x, (a, b, c, d, e, f, g, h, i, j, k, l, m, n)) =
            (storea(x, a); storeb(x ++ offsetb, b); storec(x ++ offsetc, c); stored(x ++ offsetd, d);
             storee(x ++ offsete, e); storef(x ++ offsetf, f); storeg(x ++ offsetg, g);
             storeh(x ++ offseth, h); storei(x ++ offseti, i); storej(x ++ offsetj, j);
             storek(x ++ offsetk, k); storel(x ++ offsetl, l); storem(x ++ offsetm, m);
             storen(x ++ offsetn, n))
        and release(x, (a, b, c, d, e, f, g, h, i, j, k, l, m, n)) =
            (releasea(x, a); releaseb(x ++ offsetb, b); releasec(x ++ offsetc, c); released(x ++ offsetd, d);
             releasee(x ++ offsete, e); releasef(x ++ offsetf, f); releaseg(x ++ offsetg, g);
             releaseh(x ++ offseth, h); releasei(x ++ offseti, i); releasej(x ++ offsetj, j);
             releasek(x ++ offsetk, k); releasel(x ++ offsetl, l); releasem(x ++ offsetm, m);
             releasen(x ++ offsetn, n))
    in
        {load=load, store=store, release=release,
         ctype = LowLevel.cStruct[ctypea, ctypeb, ctypec, ctyped, ctypee, ctypef, ctypeg, ctypeh, ctypei, ctypej,
                                  ctypek, ctypel, ctypem, ctypen]}
    end

    fun cStruct15(a: 'a conversion, b: 'b conversion, c: 'c conversion, d: 'd conversion,
                  e: 'e conversion, f: 'f conversion, g: 'g conversion, h: 'h conversion,
                  i: 'i conversion, j: 'j conversion, k: 'k conversion, l: 'l conversion,
                  m: 'm conversion, n: 'n conversion, o: 'o conversion):
                  ('a*'b*'c*'d*'e*'f*'g*'h*'i*'j*'k*'l*'m*'n*'o)conversion =
    let
        val {load=loada, store=storea, release=releasea, ctype = ctypea as {size=sizea, ...} } = a
        and {load=loadb, store=storeb, release=releaseb, ctype = ctypeb as {size=sizeb, align=alignb, ...} } = b
        and {load=loadc, store=storec, release=releasec, ctype = ctypec as {size=sizec, align=alignc, ...} } = c
        and {load=loadd, store=stored, release=released, ctype = ctyped as {size=sized, align=alignd, ...} } = d
        and {load=loade, store=storee, release=releasee, ctype = ctypee as {size=sizee, align=aligne, ...} } = e
        and {load=loadf, store=storef, release=releasef, ctype = ctypef as {size=sizef, align=alignf, ...} } = f
        and {load=loadg, store=storeg, release=releaseg, ctype = ctypeg as {size=sizeg, align=aligng, ...} } = g
        and {load=loadh, store=storeh, release=releaseh, ctype = ctypeh as {size=sizeh, align=alignh, ...} } = h
        and {load=loadi, store=storei, release=releasei, ctype = ctypei as {size=sizei, align=aligni, ...} } = i
        and {load=loadj, store=storej, release=releasej, ctype = ctypej as {size=sizej, align=alignj, ...} } = j
        and {load=loadk, store=storek, release=releasek, ctype = ctypek as {size=sizek, align=alignk, ...} } = k
        and {load=loadl, store=storel, release=releasel, ctype = ctypel as {size=sizel, align=alignl, ...} } = l
        and {load=loadm, store=storem, release=releasem, ctype = ctypem as {size=sizem, align=alignm, ...} } = m
        and {load=loadn, store=storen, release=releasen, ctype = ctypen as {size=sizen, align=alignn, ...} } = n
        and {load=loado, store=storeo, release=releaseo, ctype = ctypeo as {align=aligno, ...} } = o

        val offsetb = alignUp(sizea, alignb)
        val offsetc = alignUp(offsetb + sizeb, alignc)
        val offsetd = alignUp(offsetc + sizec, alignd)
        val offsete = alignUp(offsetd + sized, aligne)
        val offsetf = alignUp(offsete + sizee, alignf)
        val offsetg = alignUp(offsetf + sizef, aligng)
        val offseth = alignUp(offsetg + sizeg, alignh)
        val offseti = alignUp(offseth + sizeh, aligni)
        val offsetj = alignUp(offseti + sizei, alignj)
        val offsetk = alignUp(offsetj + sizej, alignk)
        val offsetl = alignUp(offsetk + sizek, alignl)
        val offsetm = alignUp(offsetl + sizel, alignm)
        val offsetn = alignUp(offsetm + sizem, alignn)
        val offseto = alignUp(offsetn + sizen, aligno)

        fun load s =
            (loada s, loadb(s ++ offsetb), loadc(s ++ offsetc), loadd(s ++ offsetd),
             loade(s ++ offsete), loadf(s ++ offsetf), loadg(s ++ offsetg),
             loadh(s ++ offseth), loadi(s ++ offseti), loadj(s ++ offsetj),
             loadk(s ++ offsetk), loadl(s ++ offsetl), loadm(s ++ offsetm),
             loadn(s ++ offsetn), loado(s ++ offseto))
        and store (x, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)) =
            (storea(x, a); storeb(x ++ offsetb, b); storec(x ++ offsetc, c); stored(x ++ offsetd, d);
             storee(x ++ offsete, e); storef(x ++ offsetf, f); storeg(x ++ offsetg, g);
             storeh(x ++ offseth, h); storei(x ++ offseti, i); storej(x ++ offsetj, j);
             storek(x ++ offsetk, k); storel(x ++ offsetl, l); storem(x ++ offsetm, m);
             storen(x ++ offsetn, n); storeo(x ++ offseto, o))
        and release(x, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)) =
            (releasea(x, a); releaseb(x ++ offsetb, b); releasec(x ++ offsetc, c); released(x ++ offsetd, d);
             releasee(x ++ offsete, e); releasef(x ++ offsetf, f); releaseg(x ++ offsetg, g);
             releaseh(x ++ offseth, h); releasei(x ++ offseti, i); releasej(x ++ offsetj, j);
             releasek(x ++ offsetk, k); releasel(x ++ offsetl, l); releasem(x ++ offsetm, m);
             releasen(x ++ offsetn, n); releaseo(x ++ offseto, o))
    in
        {load=load, store=store, release=release,
         ctype = LowLevel.cStruct[ctypea, ctypeb, ctypec, ctyped, ctypee, ctypef, ctypeg, ctypeh, ctypei, ctypej,
                                  ctypek, ctypel, ctypem, ctypen, ctypeo]}
    end

    fun cStruct16(a: 'a conversion, b: 'b conversion, c: 'c conversion, d: 'd conversion,
                  e: 'e conversion, f: 'f conversion, g: 'g conversion, h: 'h conversion,
                  i: 'i conversion, j: 'j conversion, k: 'k conversion, l: 'l conversion,
                  m: 'm conversion, n: 'n conversion, o: 'o conversion, p: 'p conversion):
                  ('a*'b*'c*'d*'e*'f*'g*'h*'i*'j*'k*'l*'m*'n*'o*'p)conversion =
    let
        val {load=loada, store=storea, release=releasea, ctype = ctypea as {size=sizea, ...} } = a
        and {load=loadb, store=storeb, release=releaseb, ctype = ctypeb as {size=sizeb, align=alignb, ...} } = b
        and {load=loadc, store=storec, release=releasec, ctype = ctypec as {size=sizec, align=alignc, ...} } = c
        and {load=loadd, store=stored, release=released, ctype = ctyped as {size=sized, align=alignd, ...} } = d
        and {load=loade, store=storee, release=releasee, ctype = ctypee as {size=sizee, align=aligne, ...} } = e
        and {load=loadf, store=storef, release=releasef, ctype = ctypef as {size=sizef, align=alignf, ...} } = f
        and {load=loadg, store=storeg, release=releaseg, ctype = ctypeg as {size=sizeg, align=aligng, ...} } = g
        and {load=loadh, store=storeh, release=releaseh, ctype = ctypeh as {size=sizeh, align=alignh, ...} } = h
        and {load=loadi, store=storei, release=releasei, ctype = ctypei as {size=sizei, align=aligni, ...} } = i
        and {load=loadj, store=storej, release=releasej, ctype = ctypej as {size=sizej, align=alignj, ...} } = j
        and {load=loadk, store=storek, release=releasek, ctype = ctypek as {size=sizek, align=alignk, ...} } = k
        and {load=loadl, store=storel, release=releasel, ctype = ctypel as {size=sizel, align=alignl, ...} } = l
        and {load=loadm, store=storem, release=releasem, ctype = ctypem as {size=sizem, align=alignm, ...} } = m
        and {load=loadn, store=storen, release=releasen, ctype = ctypen as {size=sizen, align=alignn, ...} } = n
        and {load=loado, store=storeo, release=releaseo, ctype = ctypeo as {size=sizeo, align=aligno, ...} } = o
        and {load=loadp, store=storep, release=releasep, ctype = ctypep as {align=alignp, ...} } = p

        val offsetb = alignUp(sizea, alignb)
        val offsetc = alignUp(offsetb + sizeb, alignc)
        val offsetd = alignUp(offsetc + sizec, alignd)
        val offsete = alignUp(offsetd + sized, aligne)
        val offsetf = alignUp(offsete + sizee, alignf)
        val offsetg = alignUp(offsetf + sizef, aligng)
        val offseth = alignUp(offsetg + sizeg, alignh)
        val offseti = alignUp(offseth + sizeh, aligni)
        val offsetj = alignUp(offseti + sizei, alignj)
        val offsetk = alignUp(offsetj + sizej, alignk)
        val offsetl = alignUp(offsetk + sizek, alignl)
        val offsetm = alignUp(offsetl + sizel, alignm)
        val offsetn = alignUp(offsetm + sizem, alignn)
        val offseto = alignUp(offsetn + sizen, aligno)
        val offsetp = alignUp(offseto + sizeo, alignp)

        fun load s =
            (loada s, loadb(s ++ offsetb), loadc(s ++ offsetc), loadd(s ++ offsetd),
             loade(s ++ offsete), loadf(s ++ offsetf), loadg(s ++ offsetg),
             loadh(s ++ offseth), loadi(s ++ offseti), loadj(s ++ offsetj),
             loadk(s ++ offsetk), loadl(s ++ offsetl), loadm(s ++ offsetm),
             loadn(s ++ offsetn), loado(s ++ offseto), loadp(s ++ offsetp))
        and store (x, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)) =
            (storea(x, a); storeb(x ++ offsetb, b); storec(x ++ offsetc, c); stored(x ++ offsetd, d);
             storee(x ++ offsete, e); storef(x ++ offsetf, f); storeg(x ++ offsetg, g);
             storeh(x ++ offseth, h); storei(x ++ offseti, i); storej(x ++ offsetj, j);
             storek(x ++ offsetk, k); storel(x ++ offsetl, l); storem(x ++ offsetm, m);
             storen(x ++ offsetn, n); storeo(x ++ offseto, o); storep(x ++ offsetp, p))
        and release(x, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)) =
            (releasea(x, a); releaseb(x ++ offsetb, b); releasec(x ++ offsetc, c); released(x ++ offsetd, d);
             releasee(x ++ offsete, e); releasef(x ++ offsetf, f); releaseg(x ++ offsetg, g);
             releaseh(x ++ offseth, h); releasei(x ++ offseti, i); releasej(x ++ offsetj, j);
             releasek(x ++ offsetk, k); releasel(x ++ offsetl, l); releasem(x ++ offsetm, m);
             releasen(x ++ offsetn, n); releaseo(x ++ offseto, o); releasep(x ++ offsetp, p))
    in
        {load=load, store=store, release=release,
         ctype = LowLevel.cStruct[ctypea, ctypeb, ctypec, ctyped, ctypee, ctypef, ctypeg, ctypeh, ctypei, ctypej,
                                  ctypek, ctypel, ctypem, ctypen, ctypeo, ctypep]}
    end

    fun cStruct17(a: 'a conversion, b: 'b conversion, c: 'c conversion, d: 'd conversion,
                  e: 'e conversion, f: 'f conversion, g: 'g conversion, h: 'h conversion,
                  i: 'i conversion, j: 'j conversion, k: 'k conversion, l: 'l conversion,
                  m: 'm conversion, n: 'n conversion, o: 'o conversion, p: 'p conversion,
                  q: 'q conversion):
                  ('a*'b*'c*'d*'e*'f*'g*'h*'i*'j*'k*'l*'m*'n*'o*'p*'q)conversion =
    let
        val {load=loada, store=storea, release=releasea, ctype = ctypea as {size=sizea, ...} } = a
        and {load=loadb, store=storeb, release=releaseb, ctype = ctypeb as {size=sizeb, align=alignb, ...} } = b
        and {load=loadc, store=storec, release=releasec, ctype = ctypec as {size=sizec, align=alignc, ...} } = c
        and {load=loadd, store=stored, release=released, ctype = ctyped as {size=sized, align=alignd, ...} } = d
        and {load=loade, store=storee, release=releasee, ctype = ctypee as {size=sizee, align=aligne, ...} } = e
        and {load=loadf, store=storef, release=releasef, ctype = ctypef as {size=sizef, align=alignf, ...} } = f
        and {load=loadg, store=storeg, release=releaseg, ctype = ctypeg as {size=sizeg, align=aligng, ...} } = g
        and {load=loadh, store=storeh, release=releaseh, ctype = ctypeh as {size=sizeh, align=alignh, ...} } = h
        and {load=loadi, store=storei, release=releasei, ctype = ctypei as {size=sizei, align=aligni, ...} } = i
        and {load=loadj, store=storej, release=releasej, ctype = ctypej as {size=sizej, align=alignj, ...} } = j
        and {load=loadk, store=storek, release=releasek, ctype = ctypek as {size=sizek, align=alignk, ...} } = k
        and {load=loadl, store=storel, release=releasel, ctype = ctypel as {size=sizel, align=alignl, ...} } = l
        and {load=loadm, store=storem, release=releasem, ctype = ctypem as {size=sizem, align=alignm, ...} } = m
        and {load=loadn, store=storen, release=releasen, ctype = ctypen as {size=sizen, align=alignn, ...} } = n
        and {load=loado, store=storeo, release=releaseo, ctype = ctypeo as {size=sizeo, align=aligno, ...} } = o
        and {load=loadp, store=storep, release=releasep, ctype = ctypep as {size=sizep, align=alignp, ...} } = p
        and {load=loadq, store=storeq, release=releaseq, ctype = ctypeq as {align=alignq, ...} } = q

        val offsetb = alignUp(sizea, alignb)
        val offsetc = alignUp(offsetb + sizeb, alignc)
        val offsetd = alignUp(offsetc + sizec, alignd)
        val offsete = alignUp(offsetd + sized, aligne)
        val offsetf = alignUp(offsete + sizee, alignf)
        val offsetg = alignUp(offsetf + sizef, aligng)
        val offseth = alignUp(offsetg + sizeg, alignh)
        val offseti = alignUp(offseth + sizeh, aligni)
        val offsetj = alignUp(offseti + sizei, alignj)
        val offsetk = alignUp(offsetj + sizej, alignk)
        val offsetl = alignUp(offsetk + sizek, alignl)
        val offsetm = alignUp(offsetl + sizel, alignm)
        val offsetn = alignUp(offsetm + sizem, alignn)
        val offseto = alignUp(offsetn + sizen, aligno)
        val offsetp = alignUp(offseto + sizeo, alignp)
        val offsetq = alignUp(offsetp + sizep, alignq)

        fun load s =
            (loada s, loadb(s ++ offsetb), loadc(s ++ offsetc), loadd(s ++ offsetd),
             loade(s ++ offsete), loadf(s ++ offsetf), loadg(s ++ offsetg),
             loadh(s ++ offseth), loadi(s ++ offseti), loadj(s ++ offsetj),
             loadk(s ++ offsetk), loadl(s ++ offsetl), loadm(s ++ offsetm),
             loadn(s ++ offsetn), loado(s ++ offseto), loadp(s ++ offsetp),
             loadq(s ++ offsetq))
        and store (x, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)) =
            (storea(x, a); storeb(x ++ offsetb, b); storec(x ++ offsetc, c); stored(x ++ offsetd, d);
             storee(x ++ offsete, e); storef(x ++ offsetf, f); storeg(x ++ offsetg, g);
             storeh(x ++ offseth, h); storei(x ++ offseti, i); storej(x ++ offsetj, j);
             storek(x ++ offsetk, k); storel(x ++ offsetl, l); storem(x ++ offsetm, m);
             storen(x ++ offsetn, n); storeo(x ++ offseto, o); storep(x ++ offsetp, p);
             storeq(x ++ offsetq, q))
        and release(x, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)) =
            (releasea(x, a); releaseb(x ++ offsetb, b); releasec(x ++ offsetc, c); released(x ++ offsetd, d);
             releasee(x ++ offsete, e); releasef(x ++ offsetf, f); releaseg(x ++ offsetg, g);
             releaseh(x ++ offseth, h); releasei(x ++ offseti, i); releasej(x ++ offsetj, j);
             releasek(x ++ offsetk, k); releasel(x ++ offsetl, l); releasem(x ++ offsetm, m);
             releasen(x ++ offsetn, n); releaseo(x ++ offseto, o); releasep(x ++ offsetp, p);
             releaseq(x ++ offsetq, q))
    in
        {load=load, store=store, release=release,
         ctype = LowLevel.cStruct[ctypea, ctypeb, ctypec, ctyped, ctypee, ctypef, ctypeg, ctypeh, ctypei, ctypej,
                                  ctypek, ctypel, ctypem, ctypen, ctypeo, ctypep, ctypeq]}
    end

    fun cStruct18(a: 'a conversion, b: 'b conversion, c: 'c conversion, d: 'd conversion,
                  e: 'e conversion, f: 'f conversion, g: 'g conversion, h: 'h conversion,
                  i: 'i conversion, j: 'j conversion, k: 'k conversion, l: 'l conversion,
                  m: 'm conversion, n: 'n conversion, o: 'o conversion, p: 'p conversion,
                  q: 'q conversion, r: 'r conversion):
                  ('a*'b*'c*'d*'e*'f*'g*'h*'i*'j*'k*'l*'m*'n*'o*'p*'q*'r)conversion =
    let
        val {load=loada, store=storea, release=releasea, ctype = ctypea as {size=sizea, ...} } = a
        and {load=loadb, store=storeb, release=releaseb, ctype = ctypeb as {size=sizeb, align=alignb, ...} } = b
        and {load=loadc, store=storec, release=releasec, ctype = ctypec as {size=sizec, align=alignc, ...} } = c
        and {load=loadd, store=stored, release=released, ctype = ctyped as {size=sized, align=alignd, ...} } = d
        and {load=loade, store=storee, release=releasee, ctype = ctypee as {size=sizee, align=aligne, ...} } = e
        and {load=loadf, store=storef, release=releasef, ctype = ctypef as {size=sizef, align=alignf, ...} } = f
        and {load=loadg, store=storeg, release=releaseg, ctype = ctypeg as {size=sizeg, align=aligng, ...} } = g
        and {load=loadh, store=storeh, release=releaseh, ctype = ctypeh as {size=sizeh, align=alignh, ...} } = h
        and {load=loadi, store=storei, release=releasei, ctype = ctypei as {size=sizei, align=aligni, ...} } = i
        and {load=loadj, store=storej, release=releasej, ctype = ctypej as {size=sizej, align=alignj, ...} } = j
        and {load=loadk, store=storek, release=releasek, ctype = ctypek as {size=sizek, align=alignk, ...} } = k
        and {load=loadl, store=storel, release=releasel, ctype = ctypel as {size=sizel, align=alignl, ...} } = l
        and {load=loadm, store=storem, release=releasem, ctype = ctypem as {size=sizem, align=alignm, ...} } = m
        and {load=loadn, store=storen, release=releasen, ctype = ctypen as {size=sizen, align=alignn, ...} } = n
        and {load=loado, store=storeo, release=releaseo, ctype = ctypeo as {size=sizeo, align=aligno, ...} } = o
        and {load=loadp, store=storep, release=releasep, ctype = ctypep as {size=sizep, align=alignp, ...} } = p
        and {load=loadq, store=storeq, release=releaseq, ctype = ctypeq as {size=sizeq, align=alignq, ...} } = q
        and {load=loadr, store=storer, release=releaser, ctype = ctyper as {align=alignr, ...} } = r

        val offsetb = alignUp(sizea, alignb)
        val offsetc = alignUp(offsetb + sizeb, alignc)
        val offsetd = alignUp(offsetc + sizec, alignd)
        val offsete = alignUp(offsetd + sized, aligne)
        val offsetf = alignUp(offsete + sizee, alignf)
        val offsetg = alignUp(offsetf + sizef, aligng)
        val offseth = alignUp(offsetg + sizeg, alignh)
        val offseti = alignUp(offseth + sizeh, aligni)
        val offsetj = alignUp(offseti + sizei, alignj)
        val offsetk = alignUp(offsetj + sizej, alignk)
        val offsetl = alignUp(offsetk + sizek, alignl)
        val offsetm = alignUp(offsetl + sizel, alignm)
        val offsetn = alignUp(offsetm + sizem, alignn)
        val offseto = alignUp(offsetn + sizen, aligno)
        val offsetp = alignUp(offseto + sizeo, alignp)
        val offsetq = alignUp(offsetp + sizep, alignq)
        val offsetr = alignUp(offsetq + sizeq, alignr)

        fun load s =
            (loada s, loadb(s ++ offsetb), loadc(s ++ offsetc), loadd(s ++ offsetd),
             loade(s ++ offsete), loadf(s ++ offsetf), loadg(s ++ offsetg),
             loadh(s ++ offseth), loadi(s ++ offseti), loadj(s ++ offsetj),
             loadk(s ++ offsetk), loadl(s ++ offsetl), loadm(s ++ offsetm),
             loadn(s ++ offsetn), loado(s ++ offseto), loadp(s ++ offsetp),
             loadq(s ++ offsetq), loadr(s ++ offsetr))
        and store (x, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)) =
            (storea(x, a); storeb(x ++ offsetb, b); storec(x ++ offsetc, c); stored(x ++ offsetd, d);
             storee(x ++ offsete, e); storef(x ++ offsetf, f); storeg(x ++ offsetg, g);
             storeh(x ++ offseth, h); storei(x ++ offseti, i); storej(x ++ offsetj, j);
             storek(x ++ offsetk, k); storel(x ++ offsetl, l); storem(x ++ offsetm, m);
             storen(x ++ offsetn, n); storeo(x ++ offseto, o); storep(x ++ offsetp, p);
             storeq(x ++ offsetq, q); storer(x ++ offsetr, r))
        and release(x, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)) =
            (releasea(x, a); releaseb(x ++ offsetb, b); releasec(x ++ offsetc, c); released(x ++ offsetd, d);
             releasee(x ++ offsete, e); releasef(x ++ offsetf, f); releaseg(x ++ offsetg, g);
             releaseh(x ++ offseth, h); releasei(x ++ offseti, i); releasej(x ++ offsetj, j);
             releasek(x ++ offsetk, k); releasel(x ++ offsetl, l); releasem(x ++ offsetm, m);
             releasen(x ++ offsetn, n); releaseo(x ++ offseto, o); releasep(x ++ offsetp, p);
             releaseq(x ++ offsetq, q); releaser(x ++ offsetr, r))
    in
        {load=load, store=store, release=release,
         ctype = LowLevel.cStruct[ctypea, ctypeb, ctypec, ctyped, ctypee, ctypef, ctypeg, ctypeh, ctypei, ctypej,
                                  ctypek, ctypel, ctypem, ctypen, ctypeo, ctypep, ctypeq, ctyper]}
    end

    fun cStruct19(a: 'a conversion, b: 'b conversion, c: 'c conversion, d: 'd conversion,
                  e: 'e conversion, f: 'f conversion, g: 'g conversion, h: 'h conversion,
                  i: 'i conversion, j: 'j conversion, k: 'k conversion, l: 'l conversion,
                  m: 'm conversion, n: 'n conversion, o: 'o conversion, p: 'p conversion,
                  q: 'q conversion, r: 'r conversion, s: 's conversion):
                  ('a*'b*'c*'d*'e*'f*'g*'h*'i*'j*'k*'l*'m*'n*'o*'p*'q*'r*'s)conversion =
    let
        val {load=loada, store=storea, release=releasea, ctype = ctypea as {size=sizea, ...} } = a
        and {load=loadb, store=storeb, release=releaseb, ctype = ctypeb as {size=sizeb, align=alignb, ...} } = b
        and {load=loadc, store=storec, release=releasec, ctype = ctypec as {size=sizec, align=alignc, ...} } = c
        and {load=loadd, store=stored, release=released, ctype = ctyped as {size=sized, align=alignd, ...} } = d
        and {load=loade, store=storee, release=releasee, ctype = ctypee as {size=sizee, align=aligne, ...} } = e
        and {load=loadf, store=storef, release=releasef, ctype = ctypef as {size=sizef, align=alignf, ...} } = f
        and {load=loadg, store=storeg, release=releaseg, ctype = ctypeg as {size=sizeg, align=aligng, ...} } = g
        and {load=loadh, store=storeh, release=releaseh, ctype = ctypeh as {size=sizeh, align=alignh, ...} } = h
        and {load=loadi, store=storei, release=releasei, ctype = ctypei as {size=sizei, align=aligni, ...} } = i
        and {load=loadj, store=storej, release=releasej, ctype = ctypej as {size=sizej, align=alignj, ...} } = j
        and {load=loadk, store=storek, release=releasek, ctype = ctypek as {size=sizek, align=alignk, ...} } = k
        and {load=loadl, store=storel, release=releasel, ctype = ctypel as {size=sizel, align=alignl, ...} } = l
        and {load=loadm, store=storem, release=releasem, ctype = ctypem as {size=sizem, align=alignm, ...} } = m
        and {load=loadn, store=storen, release=releasen, ctype = ctypen as {size=sizen, align=alignn, ...} } = n
        and {load=loado, store=storeo, release=releaseo, ctype = ctypeo as {size=sizeo, align=aligno, ...} } = o
        and {load=loadp, store=storep, release=releasep, ctype = ctypep as {size=sizep, align=alignp, ...} } = p
        and {load=loadq, store=storeq, release=releaseq, ctype = ctypeq as {size=sizeq, align=alignq, ...} } = q
        and {load=loadr, store=storer, release=releaser, ctype = ctyper as {size=sizer, align=alignr, ...} } = r
        and {load=loads, store=stores, release=releases, ctype = ctypes as {align=aligns, ...} } = s

        val offsetb = alignUp(sizea, alignb)
        val offsetc = alignUp(offsetb + sizeb, alignc)
        val offsetd = alignUp(offsetc + sizec, alignd)
        val offsete = alignUp(offsetd + sized, aligne)
        val offsetf = alignUp(offsete + sizee, alignf)
        val offsetg = alignUp(offsetf + sizef, aligng)
        val offseth = alignUp(offsetg + sizeg, alignh)
        val offseti = alignUp(offseth + sizeh, aligni)
        val offsetj = alignUp(offseti + sizei, alignj)
        val offsetk = alignUp(offsetj + sizej, alignk)
        val offsetl = alignUp(offsetk + sizek, alignl)
        val offsetm = alignUp(offsetl + sizel, alignm)
        val offsetn = alignUp(offsetm + sizem, alignn)
        val offseto = alignUp(offsetn + sizen, aligno)
        val offsetp = alignUp(offseto + sizeo, alignp)
        val offsetq = alignUp(offsetp + sizep, alignq)
        val offsetr = alignUp(offsetq + sizeq, alignr)
        val offsets = alignUp(offsetr + sizer, aligns)

        fun load s =
            (loada s, loadb(s ++ offsetb), loadc(s ++ offsetc), loadd(s ++ offsetd),
             loade(s ++ offsete), loadf(s ++ offsetf), loadg(s ++ offsetg),
             loadh(s ++ offseth), loadi(s ++ offseti), loadj(s ++ offsetj),
             loadk(s ++ offsetk), loadl(s ++ offsetl), loadm(s ++ offsetm),
             loadn(s ++ offsetn), loado(s ++ offseto), loadp(s ++ offsetp),
             loadq(s ++ offsetq), loadr(s ++ offsetr), loads(s ++ offsets))
        and store (x, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)) =
            (storea(x, a); storeb(x ++ offsetb, b); storec(x ++ offsetc, c); stored(x ++ offsetd, d);
             storee(x ++ offsete, e); storef(x ++ offsetf, f); storeg(x ++ offsetg, g);
             storeh(x ++ offseth, h); storei(x ++ offseti, i); storej(x ++ offsetj, j);
             storek(x ++ offsetk, k); storel(x ++ offsetl, l); storem(x ++ offsetm, m);
             storen(x ++ offsetn, n); storeo(x ++ offseto, o); storep(x ++ offsetp, p);
             storeq(x ++ offsetq, q); storer(x ++ offsetr, r); stores(x ++ offsets, s))
        and release(x, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)) =
            (releasea(x, a); releaseb(x ++ offsetb, b); releasec(x ++ offsetc, c); released(x ++ offsetd, d);
             releasee(x ++ offsete, e); releasef(x ++ offsetf, f); releaseg(x ++ offsetg, g);
             releaseh(x ++ offseth, h); releasei(x ++ offseti, i); releasej(x ++ offsetj, j);
             releasek(x ++ offsetk, k); releasel(x ++ offsetl, l); releasem(x ++ offsetm, m);
             releasen(x ++ offsetn, n); releaseo(x ++ offseto, o); releasep(x ++ offsetp, p);
             releaseq(x ++ offsetq, q); releaser(x ++ offsetr, r); releases(x ++ offsets, s))
    in
        {load=load, store=store, release=release,
         ctype = LowLevel.cStruct[ctypea, ctypeb, ctypec, ctyped, ctypee, ctypef, ctypeg, ctypeh, ctypei, ctypej,
                                  ctypek, ctypel, ctypem, ctypen, ctypeo, ctypep, ctypeq, ctyper, ctypes]}
    end

    fun cStruct20(a: 'a conversion, b: 'b conversion, c: 'c conversion, d: 'd conversion,
                  e: 'e conversion, f: 'f conversion, g: 'g conversion, h: 'h conversion,
                  i: 'i conversion, j: 'j conversion, k: 'k conversion, l: 'l conversion,
                  m: 'm conversion, n: 'n conversion, o: 'o conversion, p: 'p conversion,
                  q: 'q conversion, r: 'r conversion, s: 's conversion, t: 't conversion):
                  ('a*'b*'c*'d*'e*'f*'g*'h*'i*'j*'k*'l*'m*'n*'o*'p*'q*'r*'s*'t)conversion =
    let
        val {load=loada, store=storea, release=releasea, ctype = ctypea as {size=sizea, ...} } = a
        and {load=loadb, store=storeb, release=releaseb, ctype = ctypeb as {size=sizeb, align=alignb, ...} } = b
        and {load=loadc, store=storec, release=releasec, ctype = ctypec as {size=sizec, align=alignc, ...} } = c
        and {load=loadd, store=stored, release=released, ctype = ctyped as {size=sized, align=alignd, ...} } = d
        and {load=loade, store=storee, release=releasee, ctype = ctypee as {size=sizee, align=aligne, ...} } = e
        and {load=loadf, store=storef, release=releasef, ctype = ctypef as {size=sizef, align=alignf, ...} } = f
        and {load=loadg, store=storeg, release=releaseg, ctype = ctypeg as {size=sizeg, align=aligng, ...} } = g
        and {load=loadh, store=storeh, release=releaseh, ctype = ctypeh as {size=sizeh, align=alignh, ...} } = h
        and {load=loadi, store=storei, release=releasei, ctype = ctypei as {size=sizei, align=aligni, ...} } = i
        and {load=loadj, store=storej, release=releasej, ctype = ctypej as {size=sizej, align=alignj, ...} } = j
        and {load=loadk, store=storek, release=releasek, ctype = ctypek as {size=sizek, align=alignk, ...} } = k
        and {load=loadl, store=storel, release=releasel, ctype = ctypel as {size=sizel, align=alignl, ...} } = l
        and {load=loadm, store=storem, release=releasem, ctype = ctypem as {size=sizem, align=alignm, ...} } = m
        and {load=loadn, store=storen, release=releasen, ctype = ctypen as {size=sizen, align=alignn, ...} } = n
        and {load=loado, store=storeo, release=releaseo, ctype = ctypeo as {size=sizeo, align=aligno, ...} } = o
        and {load=loadp, store=storep, release=releasep, ctype = ctypep as {size=sizep, align=alignp, ...} } = p
        and {load=loadq, store=storeq, release=releaseq, ctype = ctypeq as {size=sizeq, align=alignq, ...} } = q
        and {load=loadr, store=storer, release=releaser, ctype = ctyper as {size=sizer, align=alignr, ...} } = r
        and {load=loads, store=stores, release=releases, ctype = ctypes as {size=sizes, align=aligns, ...} } = s
        and {load=loadt, store=storet, release=releaset, ctype = ctypet as {align=alignt, ...} } = t

        val offsetb = alignUp(sizea, alignb)
        val offsetc = alignUp(offsetb + sizeb, alignc)
        val offsetd = alignUp(offsetc + sizec, alignd)
        val offsete = alignUp(offsetd + sized, aligne)
        val offsetf = alignUp(offsete + sizee, alignf)
        val offsetg = alignUp(offsetf + sizef, aligng)
        val offseth = alignUp(offsetg + sizeg, alignh)
        val offseti = alignUp(offseth + sizeh, aligni)
        val offsetj = alignUp(offseti + sizei, alignj)
        val offsetk = alignUp(offsetj + sizej, alignk)
        val offsetl = alignUp(offsetk + sizek, alignl)
        val offsetm = alignUp(offsetl + sizel, alignm)
        val offsetn = alignUp(offsetm + sizem, alignn)
        val offseto = alignUp(offsetn + sizen, aligno)
        val offsetp = alignUp(offseto + sizeo, alignp)
        val offsetq = alignUp(offsetp + sizep, alignq)
        val offsetr = alignUp(offsetq + sizeq, alignr)
        val offsets = alignUp(offsetr + sizer, aligns)
        val offsett = alignUp(offsets + sizes, alignt)

        fun load s =
            (loada s, loadb(s ++ offsetb), loadc(s ++ offsetc), loadd(s ++ offsetd),
             loade(s ++ offsete), loadf(s ++ offsetf), loadg(s ++ offsetg),
             loadh(s ++ offseth), loadi(s ++ offseti), loadj(s ++ offsetj),
             loadk(s ++ offsetk), loadl(s ++ offsetl), loadm(s ++ offsetm),
             loadn(s ++ offsetn), loado(s ++ offseto), loadp(s ++ offsetp),
             loadq(s ++ offsetq), loadr(s ++ offsetr), loads(s ++ offsets), loadt(s ++ offsett))
        and store (x, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)) =
            (storea(x, a); storeb(x ++ offsetb, b); storec(x ++ offsetc, c); stored(x ++ offsetd, d);
             storee(x ++ offsete, e); storef(x ++ offsetf, f); storeg(x ++ offsetg, g);
             storeh(x ++ offseth, h); storei(x ++ offseti, i); storej(x ++ offsetj, j);
             storek(x ++ offsetk, k); storel(x ++ offsetl, l); storem(x ++ offsetm, m);
             storen(x ++ offsetn, n); storeo(x ++ offseto, o); storep(x ++ offsetp, p);
             storeq(x ++ offsetq, q); storer(x ++ offsetr, r); stores(x ++ offsets, s); storet(x ++ offsett, t))
        and release(x, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)) =
            (releasea(x, a); releaseb(x ++ offsetb, b); releasec(x ++ offsetc, c); released(x ++ offsetd, d);
             releasee(x ++ offsete, e); releasef(x ++ offsetf, f); releaseg(x ++ offsetg, g);
             releaseh(x ++ offseth, h); releasei(x ++ offseti, i); releasej(x ++ offsetj, j);
             releasek(x ++ offsetk, k); releasel(x ++ offsetl, l); releasem(x ++ offsetm, m);
             releasen(x ++ offsetn, n); releaseo(x ++ offseto, o); releasep(x ++ offsetp, p);
             releaseq(x ++ offsetq, q); releaser(x ++ offsetr, r); releases(x ++ offsets, s); releaset(x ++ offsett, t))
    in
        {load=load, store=store, release=release,
         ctype = LowLevel.cStruct[ctypea, ctypeb, ctypec, ctyped, ctypee, ctypef, ctypeg, ctypeh, ctypei, ctypej,
                                  ctypek, ctypel, ctypem, ctypen, ctypeo, ctypep, ctypeq, ctyper, ctypes, ctypet]}
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
    (* Note: it may be possible to have general functions to compute offsets
       but we don't do that because this way the compiler can compute the offsets
       as constants during inline expansion. *)
    local
        open LibFFI Memory LowLevel
    in
        fun call0withAbi (abi: abi) (fnAddr: unit->voidStar) ()
            ({ctype = resType, load= resLoad, ...} : 'a conversion): unit->'a =
        let
            val callF = callwithAbi abi fnAddr [] resType
        in
            fn () =>
            let
                val rMem = malloc(#size resType)
            in
                let
                    val () = callF([], rMem)
                    val result = resLoad rMem
                in
                    free rMem;
                    result
                end handle exn => (free rMem; raise exn)
            end
        end

        fun call0 x = call0withAbi abiDefault x (* Have to make it a fun to avoid value restriction *)

        fun call1withAbi (abi: abi) (fnAddr: unit->voidStar)
            ({ ctype = argType, store = argStore, release = argRelease, ...}: 'a conversion)
            ({ ctype = resType, load= resLoad, ...}: 'b conversion): 'a ->'b =
        let
            val callF = callwithAbi abi fnAddr [argType] resType
        in
            fn x =>
            let
                (* Allocate space for argument(s) and result.
                   We can't use cStruct here because we only store the
                   argument before the call and load the result after. *)
                val argOffset = alignUp(#size resType, #align argType)
                val rMem = malloc(argOffset + #size argType)
                val argAddr = rMem ++ argOffset
                val () = argStore (argAddr, x)
                fun freeAll () = (argRelease (argAddr, x); free rMem)
            in
                let
                    val () = callF([argAddr], rMem)
                    val result = resLoad rMem
                in
                    freeAll ();
                    result
                end handle exn => (freeAll (); raise exn)
            end
        end

        fun call1 x = call1withAbi abiDefault x (* Have to make it a fun to avoid value restriction *)

        fun call2withAbi (abi: abi) (fnAddr: unit->voidStar)
            ({ ctype = arg1Type, store = arg1Store, release = arg1Release, ...}: 'a conversion,
             { ctype = arg2Type, store = arg2Store, release = arg2Release, ...}: 'b conversion)
            ({ ctype = resType, load= resLoad, ...}: 'c conversion): 'a * 'b -> 'c =
        let
            val callF = callwithAbi abi fnAddr [arg1Type, arg2Type] resType
        in
            fn (x, y) =>
            let
                val arg1Offset = alignUp(#size resType, #align arg1Type)
                val arg2Offset = alignUp(arg1Offset + #size arg1Type, #align arg2Type)
                val rMem = malloc(arg2Offset + #size arg2Type)
                val arg1Addr = rMem ++ arg1Offset
                val arg2Addr = rMem ++ arg2Offset
                val () = arg1Store (arg1Addr, x)
                val () = arg2Store (arg2Addr, y)
                fun freeAll() =
                    (arg1Release(arg1Addr, x); arg2Release (arg2Addr, y); free rMem)
            in
                let
                    val () = callF([arg1Addr, arg2Addr], rMem)
                    val result = resLoad rMem
                in
                    freeAll();
                    result
                end handle exn => (freeAll(); raise exn)
            end
        end

        fun call2 x = call2withAbi abiDefault x (* Have to make it a fun to avoid value restriction *)

        fun call3withAbi (abi: abi) (fnAddr: unit->voidStar)
            ({ ctype = arg1Type, store = arg1Store, release = arg1Release, ...}: 'a conversion,
             { ctype = arg2Type, store = arg2Store, release = arg2Release, ...}: 'b conversion,
             { ctype = arg3Type, store = arg3Store, release = arg3Release, ...}: 'c conversion)
            ({ ctype = resType, load= resLoad, ...}: 'd conversion): 'a * 'b *'c -> 'd =
        let
            val callF = callwithAbi abi fnAddr [arg1Type, arg2Type, arg3Type] resType
        in
            fn (x, y, z) =>
            let
                val arg1Offset = alignUp(#size resType, #align arg1Type)
                val arg2Offset = alignUp(arg1Offset + #size arg1Type, #align arg2Type)
                val arg3Offset = alignUp(arg2Offset + #size arg2Type, #align arg3Type)
                val rMem = malloc(arg3Offset + #size arg3Type)
                val arg1Addr = rMem ++ arg1Offset
                val arg2Addr = rMem ++ arg2Offset
                val arg3Addr = rMem ++ arg3Offset
                val () = arg1Store (arg1Addr, x)
                val () = arg2Store (arg2Addr, y)
                val () = arg3Store (arg3Addr, z)
                fun freeAll() =
                    (arg1Release(arg1Addr, x); arg2Release (arg2Addr, y); arg3Release (arg3Addr, z); free rMem)
            in
                let
                    val () = callF([arg1Addr, arg2Addr, arg3Addr], rMem)
                    val result = resLoad rMem
                in
                    freeAll();
                    result
                end handle exn => (freeAll(); raise exn)
            end
        end

        fun call3 x = call3withAbi abiDefault x (* Have to make it a fun to avoid value restriction *)

        fun call4withAbi (abi: abi) (fnAddr: unit->voidStar)
            ({ ctype = arg1Type, store = arg1Store, release = arg1Release, ...}: 'a conversion,
             { ctype = arg2Type, store = arg2Store, release = arg2Release, ...}: 'b conversion,
             { ctype = arg3Type, store = arg3Store, release = arg3Release, ...}: 'c conversion,
             { ctype = arg4Type, store = arg4Store, release = arg4Release, ...}: 'd conversion)             
            ({ ctype = resType, load= resLoad, ...}: 'e conversion): 'a * 'b *'c * 'd -> 'e =
        let
            val callF = callwithAbi abi fnAddr [arg1Type, arg2Type, arg3Type, arg4Type] resType
        in
            fn (a, b, c, d) =>
            let
                val arg1Offset = alignUp(#size resType, #align arg1Type)
                val arg2Offset = alignUp(arg1Offset + #size arg1Type, #align arg2Type)
                val arg3Offset = alignUp(arg2Offset + #size arg2Type, #align arg3Type)
                val arg4Offset = alignUp(arg3Offset + #size arg3Type, #align arg4Type)
                val rMem = malloc(arg4Offset + #size arg4Type)
                val arg1Addr = rMem ++ arg1Offset
                val arg2Addr = rMem ++ arg2Offset
                val arg3Addr = rMem ++ arg3Offset
                val arg4Addr = rMem ++ arg4Offset
                val () = arg1Store (arg1Addr, a)
                val () = arg2Store (arg2Addr, b)
                val () = arg3Store (arg3Addr, c)
                val () = arg4Store (arg4Addr, d)
                fun freeAll() =
                    (arg1Release(arg1Addr, a); arg2Release (arg2Addr, b); arg3Release (arg3Addr, c); arg4Release (arg4Addr, d); free rMem)
            in
                let
                    val () = callF([arg1Addr, arg2Addr, arg3Addr, arg4Addr], rMem)
                    val result = resLoad rMem
                in
                    freeAll();
                    result
                end handle exn => (freeAll(); raise exn)
            end
        end

        fun call4 x = call4withAbi abiDefault x

        fun call5withAbi (abi: abi) (fnAddr: unit->voidStar)
            ({ ctype = arg1Type, store = arg1Store, release = arg1Release, ...}: 'a conversion,
             { ctype = arg2Type, store = arg2Store, release = arg2Release, ...}: 'b conversion,
             { ctype = arg3Type, store = arg3Store, release = arg3Release, ...}: 'c conversion,
             { ctype = arg4Type, store = arg4Store, release = arg4Release, ...}: 'd conversion,
             { ctype = arg5Type, store = arg5Store, release = arg5Release, ...}: 'e conversion)             
            ({ ctype = resType, load= resLoad, ...}: 'f conversion): 'a * 'b *'c * 'd * 'e -> 'f =
        let
            val callF =
                callwithAbi abi fnAddr [arg1Type, arg2Type, arg3Type, arg4Type, arg5Type] resType
        in
            fn (a, b, c, d, e) =>
            let
                val arg1Offset = alignUp(#size resType, #align arg1Type)
                val arg2Offset = alignUp(arg1Offset + #size arg1Type, #align arg2Type)
                val arg3Offset = alignUp(arg2Offset + #size arg2Type, #align arg3Type)
                val arg4Offset = alignUp(arg3Offset + #size arg3Type, #align arg4Type)
                val arg5Offset = alignUp(arg4Offset + #size arg4Type, #align arg5Type)
                val rMem = malloc(arg5Offset + #size arg5Type)
                val arg1Addr = rMem ++ arg1Offset
                val arg2Addr = rMem ++ arg2Offset
                val arg3Addr = rMem ++ arg3Offset
                val arg4Addr = rMem ++ arg4Offset
                val arg5Addr = rMem ++ arg5Offset
                val () = arg1Store (arg1Addr, a)
                val () = arg2Store (arg2Addr, b)
                val () = arg3Store (arg3Addr, c)
                val () = arg4Store (arg4Addr, d)
                val () = arg5Store (arg5Addr, e)
                fun freeAll() =
                    (arg1Release(arg1Addr, a); arg2Release (arg2Addr, b); arg3Release (arg3Addr, c);
                     arg4Release (arg4Addr, d); arg5Release (arg5Addr, e); free rMem)
            in
                let
                    val () = callF([arg1Addr, arg2Addr, arg3Addr, arg4Addr, arg5Addr], rMem)
                    val result = resLoad rMem
                in
                    freeAll();
                    result
                end handle exn => (freeAll(); raise exn)
            end
        end

        fun call5 x = call5withAbi abiDefault x

        fun call6withAbi (abi: abi) (fnAddr: unit->voidStar)
            ({ ctype = arg1Type, store = arg1Store, release = arg1Release, ...}: 'a conversion,
             { ctype = arg2Type, store = arg2Store, release = arg2Release, ...}: 'b conversion,
             { ctype = arg3Type, store = arg3Store, release = arg3Release, ...}: 'c conversion,
             { ctype = arg4Type, store = arg4Store, release = arg4Release, ...}: 'd conversion,
             { ctype = arg5Type, store = arg5Store, release = arg5Release, ...}: 'e conversion,             
             { ctype = arg6Type, store = arg6Store, release = arg6Release, ...}: 'f conversion)             
            ({ ctype = resType, load= resLoad, ...}: 'g conversion): 'a * 'b *'c * 'd * 'e * 'f -> 'g =
        let
            val callF =
                callwithAbi abi fnAddr [arg1Type, arg2Type, arg3Type, arg4Type, arg5Type, arg6Type] resType
        in
            fn (a, b, c, d, e, f) =>
            let
                val arg1Offset = alignUp(#size resType, #align arg1Type)
                val arg2Offset = alignUp(arg1Offset + #size arg1Type, #align arg2Type)
                val arg3Offset = alignUp(arg2Offset + #size arg2Type, #align arg3Type)
                val arg4Offset = alignUp(arg3Offset + #size arg3Type, #align arg4Type)
                val arg5Offset = alignUp(arg4Offset + #size arg4Type, #align arg5Type)
                val arg6Offset = alignUp(arg5Offset + #size arg5Type, #align arg6Type)
                val rMem = malloc(arg6Offset + #size arg6Type)
                val arg1Addr = rMem ++ arg1Offset
                val arg2Addr = rMem ++ arg2Offset
                val arg3Addr = rMem ++ arg3Offset
                val arg4Addr = rMem ++ arg4Offset
                val arg5Addr = rMem ++ arg5Offset
                val arg6Addr = rMem ++ arg6Offset
                val () = arg1Store (arg1Addr, a)
                val () = arg2Store (arg2Addr, b)
                val () = arg3Store (arg3Addr, c)
                val () = arg4Store (arg4Addr, d)
                val () = arg5Store (arg5Addr, e)
                val () = arg6Store (arg6Addr, f)
                fun freeAll() =
                    (arg1Release(arg1Addr, a); arg2Release (arg2Addr, b); arg3Release (arg3Addr, c);
                     arg4Release (arg4Addr, d); arg5Release (arg5Addr, e); arg6Release (arg6Addr, f); free rMem)
            in
                let
                    val () = callF([arg1Addr, arg2Addr, arg3Addr, arg4Addr, arg5Addr , arg6Addr], rMem)
                    val result = resLoad rMem
                in
                    freeAll();
                    result
                end handle exn => (freeAll(); raise exn)
            end
        end

        fun call6 x = call6withAbi abiDefault x

        fun call7withAbi (abi: abi) (fnAddr: unit->voidStar)
            ({ ctype = arg1Type, store = arg1Store, release = arg1Release, ...}: 'a conversion,
             { ctype = arg2Type, store = arg2Store, release = arg2Release, ...}: 'b conversion,
             { ctype = arg3Type, store = arg3Store, release = arg3Release, ...}: 'c conversion,
             { ctype = arg4Type, store = arg4Store, release = arg4Release, ...}: 'd conversion,
             { ctype = arg5Type, store = arg5Store, release = arg5Release, ...}: 'e conversion,             
             { ctype = arg6Type, store = arg6Store, release = arg6Release, ...}: 'f conversion,             
             { ctype = arg7Type, store = arg7Store, release = arg7Release, ...}: 'g conversion)             
            ({ ctype = resType, load= resLoad, ...}: 'h conversion): 'a * 'b *'c * 'd * 'e * 'f * 'g -> 'h =
        let
            val callF =
                callwithAbi abi fnAddr [arg1Type, arg2Type, arg3Type, arg4Type, arg5Type, arg6Type, arg7Type] resType
        in
            fn (a, b, c, d, e, f, g) =>
            let
                val arg1Offset = alignUp(#size resType, #align arg1Type)
                val arg2Offset = alignUp(arg1Offset + #size arg1Type, #align arg2Type)
                val arg3Offset = alignUp(arg2Offset + #size arg2Type, #align arg3Type)
                val arg4Offset = alignUp(arg3Offset + #size arg3Type, #align arg4Type)
                val arg5Offset = alignUp(arg4Offset + #size arg4Type, #align arg5Type)
                val arg6Offset = alignUp(arg5Offset + #size arg5Type, #align arg6Type)
                val arg7Offset = alignUp(arg6Offset + #size arg6Type, #align arg7Type)
                val rMem = malloc(arg7Offset + #size arg7Type)
                val arg1Addr = rMem ++ arg1Offset
                val arg2Addr = rMem ++ arg2Offset
                val arg3Addr = rMem ++ arg3Offset
                val arg4Addr = rMem ++ arg4Offset
                val arg5Addr = rMem ++ arg5Offset
                val arg6Addr = rMem ++ arg6Offset
                val arg7Addr = rMem ++ arg7Offset
                val () = arg1Store (arg1Addr, a)
                val () = arg2Store (arg2Addr, b)
                val () = arg3Store (arg3Addr, c)
                val () = arg4Store (arg4Addr, d)
                val () = arg5Store (arg5Addr, e)
                val () = arg6Store (arg6Addr, f)
                val () = arg7Store (arg7Addr, g)
                fun freeAll() =
                    (arg1Release(arg1Addr, a); arg2Release (arg2Addr, b); arg3Release (arg3Addr, c);
                     arg4Release (arg4Addr, d); arg5Release (arg5Addr, e); arg6Release (arg6Addr, f);
                     arg7Release (arg7Addr, g);free rMem)
            in
                let
                    val () = callF([arg1Addr, arg2Addr, arg3Addr, arg4Addr, arg5Addr, arg6Addr, arg7Addr], rMem)
                    val result = resLoad rMem
                in
                    freeAll();
                    result
                end handle exn => (freeAll(); raise exn)
            end
        end

        fun call7 x = call7withAbi abiDefault x

        fun call8withAbi (abi: abi) (fnAddr: unit->voidStar)
            ({ ctype = arg1Type, store = arg1Store, release = arg1Release, ...}: 'a conversion,
             { ctype = arg2Type, store = arg2Store, release = arg2Release, ...}: 'b conversion,
             { ctype = arg3Type, store = arg3Store, release = arg3Release, ...}: 'c conversion,
             { ctype = arg4Type, store = arg4Store, release = arg4Release, ...}: 'd conversion,
             { ctype = arg5Type, store = arg5Store, release = arg5Release, ...}: 'e conversion,             
             { ctype = arg6Type, store = arg6Store, release = arg6Release, ...}: 'f conversion,             
             { ctype = arg7Type, store = arg7Store, release = arg7Release, ...}: 'g conversion,             
             { ctype = arg8Type, store = arg8Store, release = arg8Release, ...}: 'h conversion)             
            ({ ctype = resType, load= resLoad, ...}: 'i conversion):
                'a * 'b *'c * 'd * 'e * 'f * 'g * 'h -> 'i =
        let
            val callF =
                callwithAbi abi fnAddr
                    [arg1Type, arg2Type, arg3Type, arg4Type, arg5Type, arg6Type, arg7Type, arg8Type] resType
        in
            fn (a, b, c, d, e, f, g, h) =>
            let
                val arg1Offset = alignUp(#size resType, #align arg1Type)
                val arg2Offset = alignUp(arg1Offset + #size arg1Type, #align arg2Type)
                val arg3Offset = alignUp(arg2Offset + #size arg2Type, #align arg3Type)
                val arg4Offset = alignUp(arg3Offset + #size arg3Type, #align arg4Type)
                val arg5Offset = alignUp(arg4Offset + #size arg4Type, #align arg5Type)
                val arg6Offset = alignUp(arg5Offset + #size arg5Type, #align arg6Type)
                val arg7Offset = alignUp(arg6Offset + #size arg6Type, #align arg7Type)
                val arg8Offset = alignUp(arg7Offset + #size arg7Type, #align arg8Type)
                val rMem = malloc(arg8Offset + #size arg8Type)
                val arg1Addr = rMem ++ arg1Offset
                val arg2Addr = rMem ++ arg2Offset
                val arg3Addr = rMem ++ arg3Offset
                val arg4Addr = rMem ++ arg4Offset
                val arg5Addr = rMem ++ arg5Offset
                val arg6Addr = rMem ++ arg6Offset
                val arg7Addr = rMem ++ arg7Offset
                val arg8Addr = rMem ++ arg8Offset
                val () = arg1Store (arg1Addr, a)
                val () = arg2Store (arg2Addr, b)
                val () = arg3Store (arg3Addr, c)
                val () = arg4Store (arg4Addr, d)
                val () = arg5Store (arg5Addr, e)
                val () = arg6Store (arg6Addr, f)
                val () = arg7Store (arg7Addr, g)
                val () = arg8Store (arg8Addr, h)
                fun freeAll() =
                    (arg1Release(arg1Addr, a); arg2Release (arg2Addr, b); arg3Release (arg3Addr, c);
                     arg4Release (arg4Addr, d); arg5Release (arg5Addr, e); arg6Release (arg6Addr, f);
                     arg7Release (arg7Addr, g); arg8Release (arg8Addr, h); free rMem)
            in
                let
                    val () = callF([arg1Addr, arg2Addr, arg3Addr, arg4Addr, arg5Addr, arg6Addr, arg7Addr, arg8Addr], rMem)
                    val result = resLoad rMem
                in
                    freeAll();
                    result
                end handle exn => (freeAll(); raise exn)
            end
        end

        fun call8 x = call8withAbi abiDefault x

        fun call9withAbi (abi: abi) (fnAddr: unit->voidStar)
            ({ ctype = arg1Type, store = arg1Store, release = arg1Release, ...}: 'a conversion,
             { ctype = arg2Type, store = arg2Store, release = arg2Release, ...}: 'b conversion,
             { ctype = arg3Type, store = arg3Store, release = arg3Release, ...}: 'c conversion,
             { ctype = arg4Type, store = arg4Store, release = arg4Release, ...}: 'd conversion,
             { ctype = arg5Type, store = arg5Store, release = arg5Release, ...}: 'e conversion,             
             { ctype = arg6Type, store = arg6Store, release = arg6Release, ...}: 'f conversion,             
             { ctype = arg7Type, store = arg7Store, release = arg7Release, ...}: 'g conversion,             
             { ctype = arg8Type, store = arg8Store, release = arg8Release, ...}: 'h conversion,             
             { ctype = arg9Type, store = arg9Store, release = arg9Release, ...}: 'i conversion)             
            ({ ctype = resType, load= resLoad, ...}: 'j conversion):
                'a * 'b *'c * 'd * 'e * 'f * 'g * 'h * 'i -> 'j =
        let
            val callF =
                callwithAbi abi fnAddr
                    [arg1Type, arg2Type, arg3Type, arg4Type, arg5Type, arg6Type, arg7Type, arg8Type, arg9Type] resType
        in
            fn (a, b, c, d, e, f, g, h, i) =>
            let
                val arg1Offset = alignUp(#size resType, #align arg1Type)
                val arg2Offset = alignUp(arg1Offset + #size arg1Type, #align arg2Type)
                val arg3Offset = alignUp(arg2Offset + #size arg2Type, #align arg3Type)
                val arg4Offset = alignUp(arg3Offset + #size arg3Type, #align arg4Type)
                val arg5Offset = alignUp(arg4Offset + #size arg4Type, #align arg5Type)
                val arg6Offset = alignUp(arg5Offset + #size arg5Type, #align arg6Type)
                val arg7Offset = alignUp(arg6Offset + #size arg6Type, #align arg7Type)
                val arg8Offset = alignUp(arg7Offset + #size arg7Type, #align arg8Type)
                val arg9Offset = alignUp(arg8Offset + #size arg8Type, #align arg9Type)
                val rMem = malloc(arg9Offset + #size arg9Type)
                val arg1Addr = rMem ++ arg1Offset
                val arg2Addr = rMem ++ arg2Offset
                val arg3Addr = rMem ++ arg3Offset
                val arg4Addr = rMem ++ arg4Offset
                val arg5Addr = rMem ++ arg5Offset
                val arg6Addr = rMem ++ arg6Offset
                val arg7Addr = rMem ++ arg7Offset
                val arg8Addr = rMem ++ arg8Offset
                val arg9Addr = rMem ++ arg9Offset
                val () = arg1Store (arg1Addr, a)
                val () = arg2Store (arg2Addr, b)
                val () = arg3Store (arg3Addr, c)
                val () = arg4Store (arg4Addr, d)
                val () = arg5Store (arg5Addr, e)
                val () = arg6Store (arg6Addr, f)
                val () = arg7Store (arg7Addr, g)
                val () = arg8Store (arg8Addr, h)
                val () = arg9Store (arg9Addr, i)
                fun freeAll() =
                    (arg1Release(arg1Addr, a); arg2Release (arg2Addr, b); arg3Release (arg3Addr, c);
                     arg4Release (arg4Addr, d); arg5Release (arg5Addr, e); arg6Release (arg6Addr, f);
                     arg7Release (arg7Addr, g); arg8Release (arg8Addr, h); arg9Release (arg9Addr, i); free rMem)
            in
                let
                    val () =
                        callF([arg1Addr, arg2Addr, arg3Addr, arg4Addr, arg5Addr, arg6Addr, arg7Addr, arg8Addr, arg9Addr], rMem)
                    val result = resLoad rMem
                in
                    freeAll();
                    result
                end handle exn => (freeAll(); raise exn)
            end
        end

        fun call9 x = call9withAbi abiDefault x

        fun call10withAbi (abi: abi) (fnAddr: unit->voidStar)
            ({ ctype = arg1Type, store = arg1Store, release = arg1Release, ...}: 'a conversion,
             { ctype = arg2Type, store = arg2Store, release = arg2Release, ...}: 'b conversion,
             { ctype = arg3Type, store = arg3Store, release = arg3Release, ...}: 'c conversion,
             { ctype = arg4Type, store = arg4Store, release = arg4Release, ...}: 'd conversion,
             { ctype = arg5Type, store = arg5Store, release = arg5Release, ...}: 'e conversion,             
             { ctype = arg6Type, store = arg6Store, release = arg6Release, ...}: 'f conversion,             
             { ctype = arg7Type, store = arg7Store, release = arg7Release, ...}: 'g conversion,             
             { ctype = arg8Type, store = arg8Store, release = arg8Release, ...}: 'h conversion,             
             { ctype = arg9Type, store = arg9Store, release = arg9Release, ...}: 'i conversion,             
             { ctype = arg10Type, store = arg10Store, release = arg10Release, ...}: 'j conversion)             
            ({ ctype = resType, load= resLoad, ...}: 'k conversion):
                'a * 'b *'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j -> 'k =
        let
            val callF =
                callwithAbi abi fnAddr
                    [arg1Type, arg2Type, arg3Type, arg4Type, arg5Type, arg6Type, arg7Type,
                     arg8Type, arg9Type, arg10Type] resType
        in
            fn (a, b, c, d, e, f, g, h, i, j) =>
            let
                val arg1Offset = alignUp(#size resType, #align arg1Type)
                val arg2Offset = alignUp(arg1Offset + #size arg1Type, #align arg2Type)
                val arg3Offset = alignUp(arg2Offset + #size arg2Type, #align arg3Type)
                val arg4Offset = alignUp(arg3Offset + #size arg3Type, #align arg4Type)
                val arg5Offset = alignUp(arg4Offset + #size arg4Type, #align arg5Type)
                val arg6Offset = alignUp(arg5Offset + #size arg5Type, #align arg6Type)
                val arg7Offset = alignUp(arg6Offset + #size arg6Type, #align arg7Type)
                val arg8Offset = alignUp(arg7Offset + #size arg7Type, #align arg8Type)
                val arg9Offset = alignUp(arg8Offset + #size arg8Type, #align arg9Type)
                val arg10Offset = alignUp(arg9Offset + #size arg9Type, #align arg10Type)
                val rMem = malloc(arg10Offset + #size arg10Type)
                val arg1Addr = rMem ++ arg1Offset
                val arg2Addr = rMem ++ arg2Offset
                val arg3Addr = rMem ++ arg3Offset
                val arg4Addr = rMem ++ arg4Offset
                val arg5Addr = rMem ++ arg5Offset
                val arg6Addr = rMem ++ arg6Offset
                val arg7Addr = rMem ++ arg7Offset
                val arg8Addr = rMem ++ arg8Offset
                val arg9Addr = rMem ++ arg9Offset
                val arg10Addr = rMem ++ arg10Offset
                val () = arg1Store (arg1Addr, a)
                val () = arg2Store (arg2Addr, b)
                val () = arg3Store (arg3Addr, c)
                val () = arg4Store (arg4Addr, d)
                val () = arg5Store (arg5Addr, e)
                val () = arg6Store (arg6Addr, f)
                val () = arg7Store (arg7Addr, g)
                val () = arg8Store (arg8Addr, h)
                val () = arg9Store (arg9Addr, i)
                val () = arg10Store (arg10Addr, j)
                fun freeAll() =
                    (arg1Release(arg1Addr, a); arg2Release (arg2Addr, b); arg3Release (arg3Addr, c);
                     arg4Release (arg4Addr, d); arg5Release (arg5Addr, e); arg6Release (arg6Addr, f);
                     arg7Release (arg7Addr, g); arg8Release (arg8Addr, h); arg9Release (arg9Addr, i);
                     arg10Release (arg10Addr, j); free rMem)
            in
                let
                    val () =
                        callF([arg1Addr, arg2Addr, arg3Addr, arg4Addr, arg5Addr, arg6Addr, arg7Addr,
                               arg8Addr, arg9Addr, arg10Addr], rMem)
                    val result = resLoad rMem
                in
                    freeAll();
                    result
                end handle exn => (freeAll(); raise exn)
            end
        end

        fun call10 x = call10withAbi abiDefault x

        fun call11withAbi (abi: abi) (fnAddr: unit->voidStar)
            ({ ctype = arg1Type, store = arg1Store, release = arg1Release, ...}: 'a conversion,
             { ctype = arg2Type, store = arg2Store, release = arg2Release, ...}: 'b conversion,
             { ctype = arg3Type, store = arg3Store, release = arg3Release, ...}: 'c conversion,
             { ctype = arg4Type, store = arg4Store, release = arg4Release, ...}: 'd conversion,
             { ctype = arg5Type, store = arg5Store, release = arg5Release, ...}: 'e conversion,             
             { ctype = arg6Type, store = arg6Store, release = arg6Release, ...}: 'f conversion,             
             { ctype = arg7Type, store = arg7Store, release = arg7Release, ...}: 'g conversion,             
             { ctype = arg8Type, store = arg8Store, release = arg8Release, ...}: 'h conversion,             
             { ctype = arg9Type, store = arg9Store, release = arg9Release, ...}: 'i conversion,             
             { ctype = arg10Type, store = arg10Store, release = arg10Release, ...}: 'j conversion,             
             { ctype = arg11Type, store = arg11Store, release = arg11Release, ...}: 'k conversion)             
            ({ ctype = resType, load= resLoad, ...}: 'l conversion):
                'a * 'b *'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k -> 'l =
        let
            val callF =
                callwithAbi abi fnAddr
                    [arg1Type, arg2Type, arg3Type, arg4Type, arg5Type, arg6Type, arg7Type,
                     arg8Type, arg9Type, arg10Type, arg11Type] resType
        in
            fn (a, b, c, d, e, f, g, h, i, j, k) =>
            let
                val arg1Offset = alignUp(#size resType, #align arg1Type)
                val arg2Offset = alignUp(arg1Offset + #size arg1Type, #align arg2Type)
                val arg3Offset = alignUp(arg2Offset + #size arg2Type, #align arg3Type)
                val arg4Offset = alignUp(arg3Offset + #size arg3Type, #align arg4Type)
                val arg5Offset = alignUp(arg4Offset + #size arg4Type, #align arg5Type)
                val arg6Offset = alignUp(arg5Offset + #size arg5Type, #align arg6Type)
                val arg7Offset = alignUp(arg6Offset + #size arg6Type, #align arg7Type)
                val arg8Offset = alignUp(arg7Offset + #size arg7Type, #align arg8Type)
                val arg9Offset = alignUp(arg8Offset + #size arg8Type, #align arg9Type)
                val arg10Offset = alignUp(arg9Offset + #size arg9Type, #align arg10Type)
                val arg11Offset = alignUp(arg10Offset + #size arg10Type, #align arg11Type)
                val rMem = malloc(arg11Offset + #size arg11Type)
                val arg1Addr = rMem ++ arg1Offset
                val arg2Addr = rMem ++ arg2Offset
                val arg3Addr = rMem ++ arg3Offset
                val arg4Addr = rMem ++ arg4Offset
                val arg5Addr = rMem ++ arg5Offset
                val arg6Addr = rMem ++ arg6Offset
                val arg7Addr = rMem ++ arg7Offset
                val arg8Addr = rMem ++ arg8Offset
                val arg9Addr = rMem ++ arg9Offset
                val arg10Addr = rMem ++ arg10Offset
                val arg11Addr = rMem ++ arg11Offset
                val () = arg1Store (arg1Addr, a)
                val () = arg2Store (arg2Addr, b)
                val () = arg3Store (arg3Addr, c)
                val () = arg4Store (arg4Addr, d)
                val () = arg5Store (arg5Addr, e)
                val () = arg6Store (arg6Addr, f)
                val () = arg7Store (arg7Addr, g)
                val () = arg8Store (arg8Addr, h)
                val () = arg9Store (arg9Addr, i)
                val () = arg10Store (arg10Addr, j)
                val () = arg11Store (arg11Addr, k)
                fun freeAll() =
                    (arg1Release(arg1Addr, a); arg2Release (arg2Addr, b); arg3Release (arg3Addr, c);
                     arg4Release (arg4Addr, d); arg5Release (arg5Addr, e); arg6Release (arg6Addr, f);
                     arg7Release (arg7Addr, g); arg8Release (arg8Addr, h); arg9Release (arg9Addr, i);
                     arg10Release (arg10Addr, j); arg11Release (arg11Addr, k); free rMem)
            in
                let
                    val () =
                        callF([arg1Addr, arg2Addr, arg3Addr, arg4Addr, arg5Addr, arg6Addr, arg7Addr,
                               arg8Addr, arg9Addr, arg10Addr, arg11Addr], rMem)
                    val result = resLoad rMem
                in
                    freeAll();
                    result
                end handle exn => (freeAll(); raise exn)
            end
        end

        fun call11 x = call11withAbi abiDefault x

        fun call12withAbi (abi: abi) (fnAddr: unit->voidStar)
            ({ ctype = arg1Type, store = arg1Store, release = arg1Release, ...}: 'a conversion,
             { ctype = arg2Type, store = arg2Store, release = arg2Release, ...}: 'b conversion,
             { ctype = arg3Type, store = arg3Store, release = arg3Release, ...}: 'c conversion,
             { ctype = arg4Type, store = arg4Store, release = arg4Release, ...}: 'd conversion,
             { ctype = arg5Type, store = arg5Store, release = arg5Release, ...}: 'e conversion,             
             { ctype = arg6Type, store = arg6Store, release = arg6Release, ...}: 'f conversion,             
             { ctype = arg7Type, store = arg7Store, release = arg7Release, ...}: 'g conversion,             
             { ctype = arg8Type, store = arg8Store, release = arg8Release, ...}: 'h conversion,             
             { ctype = arg9Type, store = arg9Store, release = arg9Release, ...}: 'i conversion,             
             { ctype = arg10Type, store = arg10Store, release = arg10Release, ...}: 'j conversion,             
             { ctype = arg11Type, store = arg11Store, release = arg11Release, ...}: 'k conversion,             
             { ctype = arg12Type, store = arg12Store, release = arg12Release, ...}: 'l conversion)             
            ({ ctype = resType, load= resLoad, ...}: 'm conversion):
                'a * 'b *'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l -> 'm =
        let
            val callF =
                callwithAbi abi fnAddr
                    [arg1Type, arg2Type, arg3Type, arg4Type, arg5Type, arg6Type, arg7Type,
                     arg8Type, arg9Type, arg10Type, arg11Type, arg12Type] resType
        in
            fn (a, b, c, d, e, f, g, h, i, j, k, l) =>
            let
                val arg1Offset = alignUp(#size resType, #align arg1Type)
                val arg2Offset = alignUp(arg1Offset + #size arg1Type, #align arg2Type)
                val arg3Offset = alignUp(arg2Offset + #size arg2Type, #align arg3Type)
                val arg4Offset = alignUp(arg3Offset + #size arg3Type, #align arg4Type)
                val arg5Offset = alignUp(arg4Offset + #size arg4Type, #align arg5Type)
                val arg6Offset = alignUp(arg5Offset + #size arg5Type, #align arg6Type)
                val arg7Offset = alignUp(arg6Offset + #size arg6Type, #align arg7Type)
                val arg8Offset = alignUp(arg7Offset + #size arg7Type, #align arg8Type)
                val arg9Offset = alignUp(arg8Offset + #size arg8Type, #align arg9Type)
                val arg10Offset = alignUp(arg9Offset + #size arg9Type, #align arg10Type)
                val arg11Offset = alignUp(arg10Offset + #size arg10Type, #align arg11Type)
                val arg12Offset = alignUp(arg11Offset + #size arg11Type, #align arg12Type)
                val rMem = malloc(arg12Offset + #size arg12Type)
                val arg1Addr = rMem ++ arg1Offset
                val arg2Addr = rMem ++ arg2Offset
                val arg3Addr = rMem ++ arg3Offset
                val arg4Addr = rMem ++ arg4Offset
                val arg5Addr = rMem ++ arg5Offset
                val arg6Addr = rMem ++ arg6Offset
                val arg7Addr = rMem ++ arg7Offset
                val arg8Addr = rMem ++ arg8Offset
                val arg9Addr = rMem ++ arg9Offset
                val arg10Addr = rMem ++ arg10Offset
                val arg11Addr = rMem ++ arg11Offset
                val arg12Addr = rMem ++ arg12Offset
                val () = arg1Store (arg1Addr, a)
                val () = arg2Store (arg2Addr, b)
                val () = arg3Store (arg3Addr, c)
                val () = arg4Store (arg4Addr, d)
                val () = arg5Store (arg5Addr, e)
                val () = arg6Store (arg6Addr, f)
                val () = arg7Store (arg7Addr, g)
                val () = arg8Store (arg8Addr, h)
                val () = arg9Store (arg9Addr, i)
                val () = arg10Store (arg10Addr, j)
                val () = arg11Store (arg11Addr, k)
                val () = arg12Store (arg12Addr, l)
                fun freeAll() =
                    (arg1Release(arg1Addr, a); arg2Release (arg2Addr, b); arg3Release (arg3Addr, c);
                     arg4Release (arg4Addr, d); arg5Release (arg5Addr, e); arg6Release (arg6Addr, f);
                     arg7Release (arg7Addr, g); arg8Release (arg8Addr, h); arg9Release (arg9Addr, i);
                     arg10Release (arg10Addr, j); arg11Release (arg11Addr, k); arg12Release (arg12Addr, l); free rMem)
            in
                let
                    val () =
                        callF([arg1Addr, arg2Addr, arg3Addr, arg4Addr, arg5Addr, arg6Addr, arg7Addr,
                               arg8Addr, arg9Addr, arg10Addr, arg11Addr, arg12Addr], rMem)
                    val result = resLoad rMem
                in
                    freeAll();
                    result
                end handle exn => (freeAll(); raise exn)
            end
        end

        fun call12 x = call12withAbi abiDefault x

        fun call13withAbi (abi: abi) (fnAddr: unit->voidStar)
            ({ ctype = arg1Type, store = arg1Store, release = arg1Release, ...}: 'a conversion,
             { ctype = arg2Type, store = arg2Store, release = arg2Release, ...}: 'b conversion,
             { ctype = arg3Type, store = arg3Store, release = arg3Release, ...}: 'c conversion,
             { ctype = arg4Type, store = arg4Store, release = arg4Release, ...}: 'd conversion,
             { ctype = arg5Type, store = arg5Store, release = arg5Release, ...}: 'e conversion,             
             { ctype = arg6Type, store = arg6Store, release = arg6Release, ...}: 'f conversion,             
             { ctype = arg7Type, store = arg7Store, release = arg7Release, ...}: 'g conversion,             
             { ctype = arg8Type, store = arg8Store, release = arg8Release, ...}: 'h conversion,             
             { ctype = arg9Type, store = arg9Store, release = arg9Release, ...}: 'i conversion,             
             { ctype = arg10Type, store = arg10Store, release = arg10Release, ...}: 'j conversion,             
             { ctype = arg11Type, store = arg11Store, release = arg11Release, ...}: 'k conversion,             
             { ctype = arg12Type, store = arg12Store, release = arg12Release, ...}: 'l conversion,             
             { ctype = arg13Type, store = arg13Store, release = arg13Release, ...}: 'm conversion)             
            ({ ctype = resType, load= resLoad, ...}: 'n conversion):
                'a * 'b *'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm -> 'n =
        let
            val callF =
                callwithAbi abi fnAddr
                    [arg1Type, arg2Type, arg3Type, arg4Type, arg5Type, arg6Type, arg7Type,
                     arg8Type, arg9Type, arg10Type, arg11Type, arg12Type, arg13Type] resType
        in
            fn (a, b, c, d, e, f, g, h, i, j, k, l, m) =>
            let
                val arg1Offset = alignUp(#size resType, #align arg1Type)
                val arg2Offset = alignUp(arg1Offset + #size arg1Type, #align arg2Type)
                val arg3Offset = alignUp(arg2Offset + #size arg2Type, #align arg3Type)
                val arg4Offset = alignUp(arg3Offset + #size arg3Type, #align arg4Type)
                val arg5Offset = alignUp(arg4Offset + #size arg4Type, #align arg5Type)
                val arg6Offset = alignUp(arg5Offset + #size arg5Type, #align arg6Type)
                val arg7Offset = alignUp(arg6Offset + #size arg6Type, #align arg7Type)
                val arg8Offset = alignUp(arg7Offset + #size arg7Type, #align arg8Type)
                val arg9Offset = alignUp(arg8Offset + #size arg8Type, #align arg9Type)
                val arg10Offset = alignUp(arg9Offset + #size arg9Type, #align arg10Type)
                val arg11Offset = alignUp(arg10Offset + #size arg10Type, #align arg11Type)
                val arg12Offset = alignUp(arg11Offset + #size arg11Type, #align arg12Type)
                val arg13Offset = alignUp(arg12Offset + #size arg12Type, #align arg13Type)
                val rMem = malloc(arg13Offset + #size arg13Type)
                val arg1Addr = rMem ++ arg1Offset
                val arg2Addr = rMem ++ arg2Offset
                val arg3Addr = rMem ++ arg3Offset
                val arg4Addr = rMem ++ arg4Offset
                val arg5Addr = rMem ++ arg5Offset
                val arg6Addr = rMem ++ arg6Offset
                val arg7Addr = rMem ++ arg7Offset
                val arg8Addr = rMem ++ arg8Offset
                val arg9Addr = rMem ++ arg9Offset
                val arg10Addr = rMem ++ arg10Offset
                val arg11Addr = rMem ++ arg11Offset
                val arg12Addr = rMem ++ arg12Offset
                val arg13Addr = rMem ++ arg13Offset
                val () = arg1Store (arg1Addr, a)
                val () = arg2Store (arg2Addr, b)
                val () = arg3Store (arg3Addr, c)
                val () = arg4Store (arg4Addr, d)
                val () = arg5Store (arg5Addr, e)
                val () = arg6Store (arg6Addr, f)
                val () = arg7Store (arg7Addr, g)
                val () = arg8Store (arg8Addr, h)
                val () = arg9Store (arg9Addr, i)
                val () = arg10Store (arg10Addr, j)
                val () = arg11Store (arg11Addr, k)
                val () = arg12Store (arg12Addr, l)
                val () = arg13Store (arg13Addr, m)
                fun freeAll() =
                    (arg1Release(arg1Addr, a); arg2Release (arg2Addr, b); arg3Release (arg3Addr, c);
                     arg4Release (arg4Addr, d); arg5Release (arg5Addr, e); arg6Release (arg6Addr, f);
                     arg7Release (arg7Addr, g); arg8Release (arg8Addr, h); arg9Release (arg9Addr, i);
                     arg10Release (arg10Addr, j); arg11Release (arg11Addr, k); arg12Release (arg12Addr, l);
                     arg13Release (arg13Addr, m); free rMem)
            in
                let
                    val () =
                        callF([arg1Addr, arg2Addr, arg3Addr, arg4Addr, arg5Addr, arg6Addr, arg7Addr,
                               arg8Addr, arg9Addr, arg10Addr, arg11Addr, arg12Addr, arg13Addr], rMem)
                    val result = resLoad rMem
                in
                    freeAll();
                    result
                end handle exn => (freeAll(); raise exn)
            end
        end

        fun call13 x = call13withAbi abiDefault x

        fun call14withAbi (abi: abi) (fnAddr: unit->voidStar)
            ({ ctype = arg1Type, store = arg1Store, release = arg1Release, ...}: 'a conversion,
             { ctype = arg2Type, store = arg2Store, release = arg2Release, ...}: 'b conversion,
             { ctype = arg3Type, store = arg3Store, release = arg3Release, ...}: 'c conversion,
             { ctype = arg4Type, store = arg4Store, release = arg4Release, ...}: 'd conversion,
             { ctype = arg5Type, store = arg5Store, release = arg5Release, ...}: 'e conversion,
             { ctype = arg6Type, store = arg6Store, release = arg6Release, ...}: 'f conversion,
             { ctype = arg7Type, store = arg7Store, release = arg7Release, ...}: 'g conversion,
             { ctype = arg8Type, store = arg8Store, release = arg8Release, ...}: 'h conversion,
             { ctype = arg9Type, store = arg9Store, release = arg9Release, ...}: 'i conversion,
             { ctype = arg10Type, store = arg10Store, release = arg10Release, ...}: 'j conversion,
             { ctype = arg11Type, store = arg11Store, release = arg11Release, ...}: 'k conversion,
             { ctype = arg12Type, store = arg12Store, release = arg12Release, ...}: 'l conversion,
             { ctype = arg13Type, store = arg13Store, release = arg13Release, ...}: 'm conversion,           
             { ctype = arg14Type, store = arg14Store, release = arg14Release, ...}: 'n conversion)             
            ({ ctype = resType, load= resLoad, ...}: 'o conversion):
                'a * 'b *'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm * 'n -> 'o =
        let
            val callF =
                callwithAbi abi fnAddr
                    [arg1Type, arg2Type, arg3Type, arg4Type, arg5Type, arg6Type, arg7Type,
                     arg8Type, arg9Type, arg10Type, arg11Type, arg12Type, arg13Type,
                     arg14Type] resType
        in
            fn (a, b, c, d, e, f, g, h, i, j, k, l, m, n) =>
            let
                val arg1Offset = alignUp(#size resType, #align arg1Type)
                val arg2Offset = alignUp(arg1Offset + #size arg1Type, #align arg2Type)
                val arg3Offset = alignUp(arg2Offset + #size arg2Type, #align arg3Type)
                val arg4Offset = alignUp(arg3Offset + #size arg3Type, #align arg4Type)
                val arg5Offset = alignUp(arg4Offset + #size arg4Type, #align arg5Type)
                val arg6Offset = alignUp(arg5Offset + #size arg5Type, #align arg6Type)
                val arg7Offset = alignUp(arg6Offset + #size arg6Type, #align arg7Type)
                val arg8Offset = alignUp(arg7Offset + #size arg7Type, #align arg8Type)
                val arg9Offset = alignUp(arg8Offset + #size arg8Type, #align arg9Type)
                val arg10Offset = alignUp(arg9Offset + #size arg9Type, #align arg10Type)
                val arg11Offset = alignUp(arg10Offset + #size arg10Type, #align arg11Type)
                val arg12Offset = alignUp(arg11Offset + #size arg11Type, #align arg12Type)
                val arg13Offset = alignUp(arg12Offset + #size arg12Type, #align arg13Type)
                val arg14Offset = alignUp(arg13Offset + #size arg13Type, #align arg14Type)
                val rMem = malloc(arg14Offset + #size arg14Type)
                val arg1Addr = rMem ++ arg1Offset
                val arg2Addr = rMem ++ arg2Offset
                val arg3Addr = rMem ++ arg3Offset
                val arg4Addr = rMem ++ arg4Offset
                val arg5Addr = rMem ++ arg5Offset
                val arg6Addr = rMem ++ arg6Offset
                val arg7Addr = rMem ++ arg7Offset
                val arg8Addr = rMem ++ arg8Offset
                val arg9Addr = rMem ++ arg9Offset
                val arg10Addr = rMem ++ arg10Offset
                val arg11Addr = rMem ++ arg11Offset
                val arg12Addr = rMem ++ arg12Offset
                val arg13Addr = rMem ++ arg13Offset
                val arg14Addr = rMem ++ arg14Offset
                val () = arg1Store (arg1Addr, a)
                val () = arg2Store (arg2Addr, b)
                val () = arg3Store (arg3Addr, c)
                val () = arg4Store (arg4Addr, d)
                val () = arg5Store (arg5Addr, e)
                val () = arg6Store (arg6Addr, f)
                val () = arg7Store (arg7Addr, g)
                val () = arg8Store (arg8Addr, h)
                val () = arg9Store (arg9Addr, i)
                val () = arg10Store (arg10Addr, j)
                val () = arg11Store (arg11Addr, k)
                val () = arg12Store (arg12Addr, l)
                val () = arg13Store (arg13Addr, m)
                val () = arg14Store (arg14Addr, n)
                fun freeAll() =
                    (arg1Release(arg1Addr, a); arg2Release (arg2Addr, b); arg3Release (arg3Addr, c);
                     arg4Release (arg4Addr, d); arg5Release (arg5Addr, e); arg6Release (arg6Addr, f);
                     arg7Release (arg7Addr, g); arg8Release (arg8Addr, h); arg9Release (arg9Addr, i);
                     arg10Release (arg10Addr, j); arg11Release (arg11Addr, k); arg12Release (arg12Addr, l);
                     arg13Release (arg13Addr, m); arg14Release (arg14Addr, n); free rMem)
            in
                let
                    val () =
                        callF([arg1Addr, arg2Addr, arg3Addr, arg4Addr, arg5Addr, arg6Addr, arg7Addr,
                               arg8Addr, arg9Addr, arg10Addr, arg11Addr, arg12Addr, arg13Addr, arg14Addr], rMem)
                    val result = resLoad rMem
                in
                    freeAll();
                    result
                end handle exn => (freeAll(); raise exn)
            end
        end

        fun call14 x = call14withAbi abiDefault x

    end

    local
        open LibFFI Memory LowLevel
       
        (* Release the callback *)
        fun release(v, _) = freeCallback(getAddress(v, 0w0))

        (* load should never be called. *)
        fun load _ = raise Foreign "Callbacks cannot be returned as results" 
    in
        (* Callback conversion *)
        fun cFunction0withAbi (abi: abi) () (resConv: 'a conversion) : (unit->'a) conversion =
        let
            fun callback (f: unit -> 'a) (_: voidStar, res: voidStar): unit =
                #store resConv (res, f ())

            val makeCallback = cFunctionWithAbi abi [] (#ctype resConv)

            (* Really make the callback when we store the actual function. *)
            fun store (v: voidStar, f) = setAddress(v, 0w0, makeCallback(callback f))
        in
            { load=load, store=store, release=release, ctype=cTypePointer }
        end
   
        fun cFunction0 x = cFunction0withAbi abiDefault x

        fun cFunction1withAbi (abi: abi)
                (argConv: 'a conversion) (resConv: 'b conversion) : ('a -> 'b) conversion =
        let
            fun callback (f: 'a -> 'b) (args: voidStar, res: voidStar): unit =
            let
                val result = f (#load argConv (getAddress(args, 0w0)))
            in
                #store resConv (res, result)
            end

            val makeCallback = cFunctionWithAbi abi [#ctype argConv] (#ctype resConv)

            (* Really make the callback when we store the actual function. *)
            fun store (v: voidStar, f) = setAddress(v, 0w0, makeCallback(callback f))
        in
            { load=load, store=store, release=release, ctype=cTypePointer }
        end
   
        fun cFunction1 x = cFunction1withAbi abiDefault x

        (* For the cases with more arguments we take out the conversion code.
           The reason has to do with inlining.  All callback functions have
           ctype=cTypePointer whatever the function arguments and we REALLY
           want that to be inlined even if the construction of the arguments
           isn't.  Having the result record inlined enables the calling
           function to compute the size of the argument record at compile
           time. *)
        local
            fun makeConvCallback(abi, arg1Conv: 'a conversion, arg2Conv: 'b conversion, resConv: 'c conversion) =
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
            
                val argTypes = [#ctype arg1Conv, #ctype arg2Conv]
                and resType = #ctype resConv

                val makeCallback = cFunctionWithAbi abi argTypes resType
            in
                (* Really make the callback when we store the actual function. *)
                fn f => makeCallback(callback f)
            end
        in
            fun cFunction2withAbi (abi: abi)
                    (arg1Conv: 'a conversion, arg2Conv: 'b conversion) (resConv: 'c conversion) : ('a *'b -> 'c) conversion =
            let
                val cb = makeConvCallback(abi, arg1Conv, arg2Conv, resConv)
                fun store (v: voidStar, f) = setAddress(v, 0w0, cb f)
            in
                { load=load, store=store, release=release, ctype=cTypePointer }
            end
       
            fun cFunction2 x = cFunction2withAbi abiDefault x
        end


        local
            fun makeConvCallback(abi, arg1Conv: 'a conversion, arg2Conv: 'b conversion, arg3Conv: 'c conversion, resConv: 'd conversion) =
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
            
                val argTypes =
                    [#ctype arg1Conv, #ctype arg2Conv, #ctype arg3Conv]
                and resType = #ctype resConv

                val makeCallback = cFunctionWithAbi abi argTypes resType
            in
                (* Really make the callback when we store the actual function. *)
                fn f => makeCallback(callback f)
            end
        in
            fun cFunction3withAbi (abi: abi)
                    (arg1Conv: 'a conversion, arg2Conv: 'b conversion, arg3Conv: 'c conversion)
                    (resConv: 'd conversion) : ('a *'b * 'c -> 'd) conversion =
            let
                val cb = makeConvCallback(abi, arg1Conv, arg2Conv, arg3Conv, resConv)
                fun store (v: voidStar, f) = setAddress(v, 0w0, cb f)
            in
                { load=load, store=store, release=release, ctype=cTypePointer }
            end
       
            fun cFunction3 x = cFunction3withAbi abiDefault x
        end

        local
            fun makeConvCallback(abi, arg1Conv: 'a conversion, arg2Conv: 'b conversion, arg3Conv: 'c conversion,
                 arg4Conv: 'd conversion, resConv: 'e conversion) =
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
            
                val argTypes =
                    [#ctype arg1Conv, #ctype arg2Conv, #ctype arg3Conv, #ctype arg4Conv]
                and resType = #ctype resConv

                val makeCallback = cFunctionWithAbi abi argTypes resType
            in
                (* Really make the callback when we store the actual function. *)
                fn f => makeCallback(callback f)
            end
        in
            fun cFunction4withAbi (abi: abi)
                    (arg1Conv: 'a conversion, arg2Conv: 'b conversion, arg3Conv: 'c conversion,
                     arg4Conv: 'd conversion) (resConv: 'e conversion) : ('a *'b * 'c * 'd -> 'e) conversion =
            let
                val cb = makeConvCallback(abi, arg1Conv, arg2Conv, arg3Conv, arg4Conv, resConv)
                fun store (v: voidStar, f) = setAddress(v, 0w0, cb f)
            in
                { load=load, store=store, release=release, ctype=cTypePointer }
            end
       
            fun cFunction4 x = cFunction4withAbi abiDefault x
        end

        local
            fun makeConvCallback(abi, arg1Conv: 'a conversion, arg2Conv: 'b conversion, arg3Conv: 'c conversion,
                 arg4Conv: 'd conversion, arg5Conv: 'e conversion, resConv: 'f conversion) =
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
            
                val argTypes =
                    [#ctype arg1Conv, #ctype arg2Conv, #ctype arg3Conv,
                         #ctype arg4Conv, #ctype arg5Conv]
                and resType = #ctype resConv

                val makeCallback = cFunctionWithAbi abi argTypes resType
            in
                (* Really make the callback when we store the actual function. *)
                fn f => makeCallback(callback f)
            end
        in
            fun cFunction5withAbi (abi: abi)
                    (arg1Conv: 'a conversion, arg2Conv: 'b conversion, arg3Conv: 'c conversion,
                     arg4Conv: 'd conversion, arg5Conv: 'e conversion)
                    (resConv: 'f conversion) : ('a *'b * 'c * 'd * 'e -> 'f) conversion =
            let
                val cb = makeConvCallback(abi, arg1Conv, arg2Conv, arg3Conv,
                                      arg4Conv, arg5Conv, resConv)
                fun store (v: voidStar, f) = setAddress(v, 0w0, cb f)
            in
                { load=load, store=store, release=release, ctype=cTypePointer }
            end
       
            fun cFunction5 x = cFunction5withAbi abiDefault x
        end

        local
            fun makeConvCallback(abi, arg1Conv: 'a conversion, arg2Conv: 'b conversion, arg3Conv: 'c conversion,
                 arg4Conv: 'd conversion, arg5Conv: 'e conversion, arg6Conv: 'f conversion,
                 resConv: 'g conversion) =
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
            
                val argTypes =
                    [#ctype arg1Conv, #ctype arg2Conv, #ctype arg3Conv,
                         #ctype arg4Conv, #ctype arg5Conv, #ctype arg6Conv]
                and resType = #ctype resConv

                val makeCallback = cFunctionWithAbi abi argTypes resType
            in
                (* Really make the callback when we store the actual function. *)
                fn f => makeCallback(callback f)
            end
        in
            fun cFunction6withAbi (abi: abi)
                    (arg1Conv: 'a conversion, arg2Conv: 'b conversion, arg3Conv: 'c conversion,
                     arg4Conv: 'd conversion, arg5Conv: 'e conversion, arg6Conv: 'f conversion)
                    (resConv: 'g conversion) : ('a *'b * 'c * 'd * 'e * 'f -> 'g) conversion =
            let
                val cb = makeConvCallback(abi, arg1Conv, arg2Conv, arg3Conv,
                                      arg4Conv, arg5Conv, arg6Conv, resConv)
                fun store (v: voidStar, f) = setAddress(v, 0w0, cb f)
            in
                { load=load, store=store, release=release, ctype=cTypePointer }
            end
       
            fun cFunction6 x = cFunction6withAbi abiDefault x
        end

    end
end;
