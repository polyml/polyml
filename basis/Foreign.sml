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

        val callwithAbi: LibFFI.abi -> ctype list -> ctype -> symbol -> Memory.voidStar list * Memory.voidStar -> unit
        val call: ctype list -> ctype -> symbol -> Memory.voidStar list * Memory.voidStar -> unit
        
        val cFunctionWithAbi:
            LibFFI.abi -> ctype list -> ctype -> (Memory.voidStar * Memory.voidStar -> unit) -> Memory.voidStar
        val cFunction:
            ctype list -> ctype -> (Memory.voidStar * Memory.voidStar -> unit) -> Memory.voidStar
    end

    type 'a conversion

    val makeConversion:
        {
            load: Memory.voidStar -> 'a, (* Load a value from C memory *)
            store: Memory.voidStar * 'a -> unit -> unit, (* Store value and return free function. *)
            ctype: LowLevel.ctype
        } -> 'a conversion

    val breakConversion:
        'a conversion ->
        {
            load: Memory.voidStar -> 'a, (* Load a value from C memory *)
            store: Memory.voidStar * 'a -> unit -> unit, (* Store value and return free function. *)
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

    (* Remove the "free" from a conversion.  Used if extra memory allocated
       by the argument must not be freed when the function returns.  *)
    val permanent: 'a conversion -> 'a conversion

    (* Call by reference.  *)
    val cStar: 'a conversion -> 'a ref conversion
    (* Pass a const pointer *)
    val cConstStar: 'a conversion -> 'a conversion

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

            fun callwithAbi (abi: abi) (argTypes: ctype list) (resType: ctype): symbol -> voidStar list * voidStar -> unit =
            let
                (* Preparation when we create the function. *)
                fun buildCif () = createCIF (abi, #ffiType resType (), map (fn {ffiType, ...} => ffiType ()) argTypes)
                val cif: unit->cif = memoise buildCif ()
                val nArgs = List.length argTypes
                val resSize = #size resType
                
                (* If the result size is smaller than ffiMinArgSize we have to
                   first store the result in a value of size ffiMinArgSize then copy
                   the result.  This is a restriction of libffi. *)
                fun smallSpace (fnAddr: unit->voidStar) (args, resMem) =
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
                fun largeSpace (fnAddr: unit->voidStar) (args, resMem) =
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
        store: Memory.voidStar * 'a -> unit -> unit, (* Store a value in C memory *)
        update: Memory.voidStar * 'a -> unit, (* Update - only used in cStar. *)
        ctype: LowLevel.ctype
    }
    
    fun makeConversion { load, store, ctype } =
        { load = load, store = store, ctype = ctype, update = fn _ => () }

    fun breakConversion({load, store, ctype, ... }: 'a conversion) =
        { load = load, store = store, ctype = ctype }

    (* Conversions *)
    local
        open LibFFI Memory LowLevel
        fun checkRange(i, min, max) = if i < min orelse i > max then raise Overflow else i
        fun noFree _ = () (* None of these allocate extra memory or need to update. *)
    in
        val cVoid: unit conversion =
            makeConversion{ load=fn _ => (), store=fn _ => noFree, ctype = cTypeVoid }

        (* cPointer should only be used to base other conversions on. *)
        val cPointer: voidStar conversion =
            makeConversion { load=fn a => getAddress(a, 0w0), store=fn(a, v) => (setAddress(a, 0w0, v); noFree),
                             ctype = cTypePointer }

        local
            fun load(m: voidStar): int = Word8.toIntX(get8(m, 0w0))
            fun store(m: voidStar, i: int) =
                (set8(m, 0w0, Word8.fromInt(checkRange(i, ~128, 127))); noFree)
        in
            val cInt8: int conversion =
                makeConversion { load=load, store=store, ctype = cTypeInt8 }
        end

        local
            (* Char is signed in C but unsigned in ML. *)
            fun load(m: voidStar): char = Char.chr(Word8.toInt(get8(m, 0w0)))
            fun store(m: voidStar, i: char) =
                (set8(m, 0w0, Word8.fromInt(Char.ord i)); noFree)
        in
            val cChar: char conversion =
                makeConversion{ load=load, store=store, ctype = cTypeChar }
        end

        local
            (* Uchar - convert as Word8.word. *)
            fun load(m: voidStar): Word8.word = get8(m, 0w0)
            fun store(m: voidStar, i: Word8.word) = (set8(m, 0w0, i); noFree)
        in
            val cUchar: Word8.word conversion =
                makeConversion{ load=load, store=store, ctype = cTypeUchar }
        end

        local
            fun load(m: voidStar): int = Word8.toInt(get8(m, 0w0))
            fun store(m: voidStar, i: int) =
                (set8(m, 0w0, Word8.fromInt(checkRange(i, 0, 255))); noFree)
        in
            val cUint8: int conversion =
                makeConversion{ load=load, store=store, ctype = cTypeUint8 }
        end

        local
            fun load(m: voidStar): int = Word.toIntX(get16(m, 0w0))
            fun store(m: voidStar, i: int) =
                (set16(m, 0w0, Word.fromInt(checkRange(i, ~32768, 32767))); noFree)
        in
            val cInt16: int conversion =
                makeConversion{ load=load, store=store, ctype = cTypeInt16 }
        end

        local
            fun load(m: voidStar): int = Word.toInt(get16(m, 0w0))
            fun store(m: voidStar, i: int) =
                (set16(m, 0w0, Word.fromInt(checkRange(i, 0, 65535))); noFree)
        in
            val cUint16: int conversion =
                makeConversion{ load=load, store=store, ctype = cTypeUint16 }
        end

        local
            fun load(m: voidStar): int = Word32.toIntX(get32(m, 0w0))
            fun store(m: voidStar, i: int) =
                (set32(m, 0w0, Word32.fromInt(checkRange(i, ~2147483648, 2147483647))); noFree)
        in
            val cInt32: int conversion =
                makeConversion{ load=load, store=store, ctype = cTypeInt32 }
        end

        local
            fun load(m: voidStar): int = Word32.toInt(get32(m, 0w0))
            fun store(m: voidStar, i: int) =
                (set32(m, 0w0, Word32.fromInt(checkRange(i, 0, 4294967295))); noFree)
        in
            val cUint32: int conversion =
                makeConversion{ load=load, store=store, ctype = cTypeUint32 }
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
                    else (set32(m, 0w0, lo); set32(m, 0w1, hi));
                    noFree
                end
                else (set64(m, 0w0, SysWord.fromInt(checkRange(i, min, max))); noFree)
        in
            val cInt64: int conversion =
                makeConversion{ load=load, store=store, ctype = cTypeInt64 }
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
                    else (set32(m, 0w0, lo); set32(m, 0w1, hi));
                    noFree
                end
                else (set64(m, 0w0, SysWord.fromInt(checkRange(i, 0, max))); noFree)
        in
            val cUint64: int conversion =
                makeConversion{ load=load, store=store, ctype = cTypeUint64 }
        end

        local
            fun load(m: voidStar): real = getFloat(m, 0w0)
            fun store(m: voidStar, v: real) = (setFloat(m, 0w0, v); noFree)
        in
            val cFloat: real conversion =
                makeConversion{ load=load, store=store, ctype = cTypeFloat }
        end

        local
            fun load(m: voidStar): real = getDouble(m, 0w0)
            fun store(m: voidStar, v: real) = (setDouble(m, 0w0, v); noFree)
        in
            val cDouble: real conversion =
                makeConversion{ load=load, store=store, ctype = cTypeDouble }
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
                setAddress(v, 0w0, sMem);
                fn () => Memory.free sMem
            end

        in
            val cString: string conversion =
                makeConversion { load=load, store=store, ctype = cTypePointer }
        end

        (* This is used if we want to pass NULL rather than a pointer in some cases. *)
        fun cOptionPtr({load, store, update, ctype}:'a conversion): 'a option conversion =
            if #typeCode(extractFFItype(#ffiType ctype ())) <> ffiTypeCodePointer
            then raise Foreign "cOptionPtr must be applied to a pointer type"
            else
            let
                fun loadOpt(s: voidStar) =
                    if getAddress(s, 0w0) = null then NONE else SOME(load s)

                fun storeOpt(v: voidStar, NONE) = (setAddress(v, 0w0, null); fn _ => ())
                |   storeOpt(v: voidStar, SOME s) = store(v, s)
                
                (* Do we have update here? *)
                fun updateOpt(_, NONE) = ()
                |   updateOpt(v: voidStar, SOME s) = update(v, s)
            in
                { load=loadOpt, store=storeOpt, update = updateOpt, ctype = cTypePointer }
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
                setAddress(v, 0w0, sMem);
                fn () => Memory.free sMem
            end

        in
            val cByteArray: Word8Vector.vector conversion =
                makeConversion{ load=load, store=store, ctype = cTypePointer }
        end
    end

    (* Remove the free part from the store fn.  This is intended for situations
       where an argument should not be deleted once the function completes. *)
    fun permanent({load, store, ctype, update}: 'a conversion): 'a conversion =
    let
        fun storeP args = (ignore (store args); fn () => ())
    in
        { load=load, store=storeP, update = update, ctype=ctype }
    end
 
    val op ++ = Memory.++

    fun cStruct2(a: 'a conversion, b: 'b conversion): ('a*'b)conversion =
    let
        val {load=loada, store=storea, update=updatea, ctype = ctypea as {size=sizea, ... }} = a
        and {load=loadb, store=storeb, update=updateb, ctype = ctypeb as {align=alignb, ... }} = b
        
        val offsetb = alignUp(sizea, alignb)
        fun load s = (loada s, loadb(s ++ offsetb))
        and store (x, (a, b)) =
        let
            val freea = storea(x, a) and freeb = storeb(x ++ offsetb, b)
        in
            fn () => ( freea(); freeb() )
        end
        and update(s, (a, b)) = (updatea(s, a); updateb(s ++ offsetb, b))

    in
        {load=load, store=store, update = update, ctype = LowLevel.cStruct[ctypea, ctypeb]}
    end

    fun cStruct3(a: 'a conversion, b: 'b conversion, c: 'c conversion): ('a*'b*'c)conversion =
    let
        val {load=loada, store=storea, update=updatea, ctype = ctypea as {size=sizea, ...} } = a
        and {load=loadb, store=storeb, update=updateb, ctype = ctypeb as {size=sizeb, align=alignb, ...} } = b
        and {load=loadc, store=storec, update=updatec, ctype = ctypec as {align=alignc, ...} } = c
       
        val offsetb = alignUp(sizea, alignb)
        val offsetc = alignUp(offsetb + sizeb, alignc)

        fun load s = (loada s, loadb(s ++ offsetb), loadc(s ++ offsetc))
        and store (x, (a, b, c)) =
        let
            val freea = storea(x, a) and freeb = storeb(x ++ offsetb, b) and freec = storec(x ++ offsetc, c)
        in
            fn () => ( freea(); freeb(); freec() )
        end
        and update(s, (a, b, c)) = (updatea(s, a); updateb(s ++ offsetb, b); updatec(s ++ offsetc, c))
    in
        {load=load, store=store, update=update, ctype = LowLevel.cStruct[ctypea, ctypeb, ctypec]}
    end

    fun cStruct4(a: 'a conversion, b: 'b conversion, c: 'c conversion, d: 'd conversion): ('a*'b*'c*'d)conversion =
    let
        val {load=loada, store=storea, update=updatea, ctype = ctypea as {size=sizea, ...} } = a
        and {load=loadb, store=storeb, update=updateb, ctype = ctypeb as {size=sizeb, align=alignb, ...} } = b
        and {load=loadc, store=storec, update=updatec, ctype = ctypec as {size=sizec, align=alignc, ...} } = c
        and {load=loadd, store=stored, update=updated, ctype = ctyped as {align=alignd, ...} } = d
 
        val offsetb = alignUp(sizea, alignb)
        val offsetc = alignUp(offsetb + sizeb, alignc)
        val offsetd = alignUp(offsetc + sizec, alignd)

        fun load s = (loada s, loadb(s ++ offsetb), loadc(s ++ offsetc), loadd(s ++ offsetd))
        and store (x, (a, b, c, d)) =
        let
            val freea = storea(x, a) and freeb = storeb(x ++ offsetb, b) and freec = storec(x ++ offsetc, c)
            and freed = stored(x ++ offsetd, d)
        in
            fn () => ( freea(); freeb(); freec(); freed() )
        end
        and update(s, (a, b, c, d)) =
            (updatea(s, a); updateb(s ++ offsetb, b); updatec(s ++ offsetc, c); updated(s ++ offsetd, d))
    in
        {load=load, store=store, update=update, ctype = LowLevel.cStruct[ctypea, ctypeb, ctypec, ctyped]}
    end

    fun cStruct5(a: 'a conversion, b: 'b conversion, c: 'c conversion, d: 'd conversion,
                 e: 'e conversion): ('a*'b*'c*'d*'e)conversion =
    let
        val {load=loada, store=storea, update=updatea, ctype = ctypea as {size=sizea, ...} } = a
        and {load=loadb, store=storeb, update=updateb, ctype = ctypeb as {size=sizeb, align=alignb, ...} } = b
        and {load=loadc, store=storec, update=updatec, ctype = ctypec as {size=sizec, align=alignc, ...} } = c
        and {load=loadd, store=stored, update=updated, ctype = ctyped as {size=sized, align=alignd, ...} } = d
        and {load=loade, store=storee, update=updatee, ctype = ctypee as {align=aligne, ...} } = e

        val offsetb = alignUp(sizea, alignb)
        val offsetc = alignUp(offsetb + sizeb, alignc)
        val offsetd = alignUp(offsetc + sizec, alignd)
        val offsete = alignUp(offsetd + sized, aligne)

        fun load s =
            (loada s, loadb(s ++ offsetb), loadc(s ++ offsetc), loadd(s ++ offsetd), loade(s ++ offsete))
        and store (x, (a, b, c, d, e)) =
        let
            val freea = storea(x, a) and freeb = storeb(x ++ offsetb, b) and freec = storec(x ++ offsetc, c)
            and freed = stored(x ++ offsetd, d) and freee = storee(x ++ offsete, e)
        in
            fn () => ( freea(); freeb(); freec(); freed(); freee() )
        end
        and update(s, (a, b, c, d, e)) =
            (updatea(s, a); updateb(s ++ offsetb, b); updatec(s ++ offsetc, c);
             updated(s ++ offsetd, d); updatee(s ++ offsete, e))
    in
        {load=load, store=store, update=update, ctype = LowLevel.cStruct[ctypea, ctypeb, ctypec, ctyped, ctypee]}
    end

    fun cStruct6(a: 'a conversion, b: 'b conversion, c: 'c conversion, d: 'd conversion,
                 e: 'e conversion, f: 'f conversion): ('a*'b*'c*'d*'e*'f)conversion =
    let
        val {load=loada, store=storea, update=updatea, ctype = ctypea as {size=sizea, ...} } = a
        and {load=loadb, store=storeb, update=updateb, ctype = ctypeb as {size=sizeb, align=alignb, ...} } = b
        and {load=loadc, store=storec, update=updatec, ctype = ctypec as {size=sizec, align=alignc, ...} } = c
        and {load=loadd, store=stored, update=updated, ctype = ctyped as {size=sized, align=alignd, ...} } = d
        and {load=loade, store=storee, update=updatee, ctype = ctypee as {size=sizee, align=aligne, ...} } = e
        and {load=loadf, store=storef, update=updatef, ctype = ctypef as {align=alignf, ...} } = f

        val offsetb = alignUp(sizea, alignb)
        val offsetc = alignUp(offsetb + sizeb, alignc)
        val offsetd = alignUp(offsetc + sizec, alignd)
        val offsete = alignUp(offsetd + sized, aligne)
        val offsetf = alignUp(offsete + sizee, alignf)

        fun load s =
            (loada s, loadb(s ++ offsetb), loadc(s ++ offsetc), loadd(s ++ offsetd),
             loade(s ++ offsete), loadf(s ++ offsetf))
        and store (x, (a, b, c, d, e, f)) =
        let
            val freea = storea(x, a) and freeb = storeb(x ++ offsetb, b) and freec = storec(x ++ offsetc, c)
            and freed = stored(x ++ offsetd, d) and freee = storee(x ++ offsete, e) and freef = storef(x ++ offsetf, f)
        in
            fn () => ( freea(); freeb(); freec(); freed(); freee(); freef() )
        end
        and update(s, (a, b, c, d, e, f)) =
            (updatea(s, a); updateb(s ++ offsetb, b); updatec(s ++ offsetc, c); updated(s ++ offsetd, d);
             updatee(s ++ offsete, e); updatef(s ++ offsetf, f))
    in
        {load=load, store=store, update=update, ctype = LowLevel.cStruct[ctypea, ctypeb, ctypec, ctyped, ctypee, ctypef]}
    end

    fun cStruct7(a: 'a conversion, b: 'b conversion, c: 'c conversion, d: 'd conversion,
                 e: 'e conversion, f: 'f conversion, g: 'g conversion): ('a*'b*'c*'d*'e*'f*'g)conversion =
    let
        val {load=loada, store=storea, update=updatea, ctype = ctypea as {size=sizea, ...} } = a
        and {load=loadb, store=storeb, update=updateb, ctype = ctypeb as {size=sizeb, align=alignb, ...} } = b
        and {load=loadc, store=storec, update=updatec, ctype = ctypec as {size=sizec, align=alignc, ...} } = c
        and {load=loadd, store=stored, update=updated, ctype = ctyped as {size=sized, align=alignd, ...} } = d
        and {load=loade, store=storee, update=updatee, ctype = ctypee as {size=sizee, align=aligne, ...} } = e
        and {load=loadf, store=storef, update=updatef, ctype = ctypef as {size=sizef, align=alignf, ...} } = f
        and {load=loadg, store=storeg, update=updateg, ctype = ctypeg as {align=aligng, ...} } = g

        val offsetb = alignUp(sizea, alignb)
        val offsetc = alignUp(offsetb + sizeb, alignc)
        val offsetd = alignUp(offsetc + sizec, alignd)
        val offsete = alignUp(offsetd + sized, aligne)
        val offsetf = alignUp(offsete + sizee, alignf)
        val offsetg = alignUp(offsetf + sizef, aligng)

        fun load s =
            (loada s, loadb(s ++ offsetb), loadc(s ++ offsetc), loadd(s ++ offsetd),
             loade(s ++ offsete), loadf(s ++ offsetf), loadg(s ++ offsetg))
        and store (x, (a, b, c, d, e, f, g)) =
        let
            val freea = storea(x, a) and freeb = storeb(x ++ offsetb, b) and freec = storec(x ++ offsetc, c)
            and freed = stored(x ++ offsetd, d) and freee = storee(x ++ offsete, e) and freef = storef(x ++ offsetf, f)
            and freeg = storeg(x ++ offsetg, g)
        in
            fn () => ( freea(); freeb(); freec(); freed(); freee(); freef(); freeg() )
        end
        and update(s, (a, b, c, d, e, f, g)) =
            (updatea(s, a); updateb(s ++ offsetb, b); updatec(s ++ offsetc, c); updated(s ++ offsetd, d);
             updatee(s ++ offsete, e); updatef(s ++ offsetf, f); updateg(s ++ offsetg, g))
    in
        {load=load, store=store, update=update, ctype = LowLevel.cStruct[ctypea, ctypeb, ctypec, ctyped, ctypee, ctypef, ctypeg]}
    end

    fun cStruct8(a: 'a conversion, b: 'b conversion, c: 'c conversion, d: 'd conversion,
                 e: 'e conversion, f: 'f conversion, g: 'g conversion, h: 'h conversion):
                    ('a*'b*'c*'d*'e*'f*'g*'h)conversion =
    let
        val {load=loada, store=storea, update=updatea, ctype = ctypea as {size=sizea, ...} } = a
        and {load=loadb, store=storeb, update=updateb, ctype = ctypeb as {size=sizeb, align=alignb, ...} } = b
        and {load=loadc, store=storec, update=updatec, ctype = ctypec as {size=sizec, align=alignc, ...} } = c
        and {load=loadd, store=stored, update=updated, ctype = ctyped as {size=sized, align=alignd, ...} } = d
        and {load=loade, store=storee, update=updatee, ctype = ctypee as {size=sizee, align=aligne, ...} } = e
        and {load=loadf, store=storef, update=updatef, ctype = ctypef as {size=sizef, align=alignf, ...} } = f
        and {load=loadg, store=storeg, update=updateg, ctype = ctypeg as {size=sizeg, align=aligng, ...} } = g
        and {load=loadh, store=storeh, update=updateh, ctype = ctypeh as {align=alignh, ...} } = h

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
        and store (x, (a, b, c, d, e, f, g, h)) =
        let
            val freea = storea(x, a) and freeb = storeb(x ++ offsetb, b) and freec = storec(x ++ offsetc, c)
            and freed = stored(x ++ offsetd, d) and freee = storee(x ++ offsete, e) and freef = storef(x ++ offsetf, f)
            and freeg = storeg(x ++ offsetg, g) and freeh = storeh(x ++ offseth, h)
        in
            fn () => ( freea(); freeb(); freec(); freed(); freee(); freef(); freeg(); freeh() )
        end
        and update(s, (a, b, c, d, e, f, g, h)) =
            (updatea(s, a); updateb(s ++ offsetb, b); updatec(s ++ offsetc, c); updated(s ++ offsetd, d);
             updatee(s ++ offsete, e); updatef(s ++ offsetf, f); updateg(s ++ offsetg, g);
             updateh(s ++ offseth, h))
    in
        {load=load, store=store, update=update,
         ctype = LowLevel.cStruct[ctypea, ctypeb, ctypec, ctyped, ctypee, ctypef, ctypeg, ctypeh]}
    end

    fun cStruct9(a: 'a conversion, b: 'b conversion, c: 'c conversion, d: 'd conversion,
                 e: 'e conversion, f: 'f conversion, g: 'g conversion, h: 'h conversion,
                 i: 'i conversion): ('a*'b*'c*'d*'e*'f*'g*'h*'i)conversion =
    let
        val {load=loada, store=storea, update=updatea, ctype = ctypea as {size=sizea, ...} } = a
        and {load=loadb, store=storeb, update=updateb, ctype = ctypeb as {size=sizeb, align=alignb, ...} } = b
        and {load=loadc, store=storec, update=updatec, ctype = ctypec as {size=sizec, align=alignc, ...} } = c
        and {load=loadd, store=stored, update=updated, ctype = ctyped as {size=sized, align=alignd, ...} } = d
        and {load=loade, store=storee, update=updatee, ctype = ctypee as {size=sizee, align=aligne, ...} } = e
        and {load=loadf, store=storef, update=updatef, ctype = ctypef as {size=sizef, align=alignf, ...} } = f
        and {load=loadg, store=storeg, update=updateg, ctype = ctypeg as {size=sizeg, align=aligng, ...} } = g
        and {load=loadh, store=storeh, update=updateh, ctype = ctypeh as {size=sizeh, align=alignh, ...} } = h
        and {load=loadi, store=storei, update=updatei, ctype = ctypei as {align=aligni, ...} } = i

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
        and store (x, (a, b, c, d, e, f, g, h, i)) =
        let
            val freea = storea(x, a) and freeb = storeb(x ++ offsetb, b) and freec = storec(x ++ offsetc, c)
            and freed = stored(x ++ offsetd, d) and freee = storee(x ++ offsete, e) and freef = storef(x ++ offsetf, f)
            and freeg = storeg(x ++ offsetg, g) and freeh = storeh(x ++ offseth, h) and freei = storei(x ++ offseti, i)
        in
            fn () => ( freea(); freeb(); freec(); freed(); freee(); freef(); freeg(); freeh(); freei() )
        end
        and update(s, (a, b, c, d, e, f, g, h, i)) =
            (updatea(s, a); updateb(s ++ offsetb, b); updatec(s ++ offsetc, c); updated(s ++ offsetd, d);
             updatee(s ++ offsete, e); updatef(s ++ offsetf, f); updateg(s ++ offsetg, g);
             updateh(s ++ offseth, h); updatei(s ++ offseti, i))
    in
        {load=load, store=store, update=update,
         ctype = LowLevel.cStruct[ctypea, ctypeb, ctypec, ctyped, ctypee, ctypef, ctypeg, ctypeh, ctypei]}
    end

    fun cStruct10(a: 'a conversion, b: 'b conversion, c: 'c conversion, d: 'd conversion,
                  e: 'e conversion, f: 'f conversion, g: 'g conversion, h: 'h conversion,
                  i: 'i conversion, j: 'j conversion):
                  ('a*'b*'c*'d*'e*'f*'g*'h*'i*'j)conversion =
    let
        val {load=loada, store=storea, update=updatea, ctype = ctypea as {size=sizea, ...} } = a
        and {load=loadb, store=storeb, update=updateb, ctype = ctypeb as {size=sizeb, align=alignb, ...} } = b
        and {load=loadc, store=storec, update=updatec, ctype = ctypec as {size=sizec, align=alignc, ...} } = c
        and {load=loadd, store=stored, update=updated, ctype = ctyped as {size=sized, align=alignd, ...} } = d
        and {load=loade, store=storee, update=updatee, ctype = ctypee as {size=sizee, align=aligne, ...} } = e
        and {load=loadf, store=storef, update=updatef, ctype = ctypef as {size=sizef, align=alignf, ...} } = f
        and {load=loadg, store=storeg, update=updateg, ctype = ctypeg as {size=sizeg, align=aligng, ...} } = g
        and {load=loadh, store=storeh, update=updateh, ctype = ctypeh as {size=sizeh, align=alignh, ...} } = h
        and {load=loadi, store=storei, update=updatei, ctype = ctypei as {size=sizei, align=aligni, ...} } = i
        and {load=loadj, store=storej, update=updatej, ctype = ctypej as {align=alignj, ...} } = j

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
        let
            val freea = storea(x, a) and freeb = storeb(x ++ offsetb, b) and freec = storec(x ++ offsetc, c)
            and freed = stored(x ++ offsetd, d) and freee = storee(x ++ offsete, e) and freef = storef(x ++ offsetf, f)
            and freeg = storeg(x ++ offsetg, g) and freeh = storeh(x ++ offseth, h) and freei = storei(x ++ offseti, i)
            and freej = storej(x ++ offsetj, j)
        in
            fn () =>
                (
                    freea(); freeb(); freec(); freed(); freee(); freef(); freeg();
                    freeh(); freei(); freej()
                )
        end
        and update(x, (a, b, c, d, e, f, g, h, i, j)) =
            (updatea(x, a); updateb(x ++ offsetb, b); updatec(x ++ offsetc, c); updated(x ++ offsetd, d);
             updatee(x ++ offsete, e); updatef(x ++ offsetf, f); updateg(x ++ offsetg, g);
             updateh(x ++ offseth, h); updatei(x ++ offseti, i); updatej(x ++ offsetj, j))
    in
        {load=load, store=store, update=update,
         ctype = LowLevel.cStruct[ctypea, ctypeb, ctypec, ctyped, ctypee, ctypef, ctypeg, ctypeh, ctypei, ctypej]}
    end

    fun cStruct11(a: 'a conversion, b: 'b conversion, c: 'c conversion, d: 'd conversion,
                  e: 'e conversion, f: 'f conversion, g: 'g conversion, h: 'h conversion,
                  i: 'i conversion, j: 'j conversion, k: 'k conversion):
                  ('a*'b*'c*'d*'e*'f*'g*'h*'i*'j*'k)conversion =
    let
        val {load=loada, store=storea, update=updatea, ctype = ctypea as {size=sizea, ...} } = a
        and {load=loadb, store=storeb, update=updateb, ctype = ctypeb as {size=sizeb, align=alignb, ...} } = b
        and {load=loadc, store=storec, update=updatec, ctype = ctypec as {size=sizec, align=alignc, ...} } = c
        and {load=loadd, store=stored, update=updated, ctype = ctyped as {size=sized, align=alignd, ...} } = d
        and {load=loade, store=storee, update=updatee, ctype = ctypee as {size=sizee, align=aligne, ...} } = e
        and {load=loadf, store=storef, update=updatef, ctype = ctypef as {size=sizef, align=alignf, ...} } = f
        and {load=loadg, store=storeg, update=updateg, ctype = ctypeg as {size=sizeg, align=aligng, ...} } = g
        and {load=loadh, store=storeh, update=updateh, ctype = ctypeh as {size=sizeh, align=alignh, ...} } = h
        and {load=loadi, store=storei, update=updatei, ctype = ctypei as {size=sizei, align=aligni, ...} } = i
        and {load=loadj, store=storej, update=updatej, ctype = ctypej as {size=sizej, align=alignj, ...} } = j
        and {load=loadk, store=storek, update=updatek, ctype = ctypek as {align=alignk, ...} } = k

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
        let
            val freea = storea(x, a) and freeb = storeb(x ++ offsetb, b) and freec = storec(x ++ offsetc, c)
            and freed = stored(x ++ offsetd, d) and freee = storee(x ++ offsete, e) and freef = storef(x ++ offsetf, f)
            and freeg = storeg(x ++ offsetg, g) and freeh = storeh(x ++ offseth, h) and freei = storei(x ++ offseti, i)
            and freej = storej(x ++ offsetj, j) and freek = storek(x ++ offsetk, k)
        in
            fn () =>
                (
                    freea(); freeb(); freec(); freed(); freee(); freef(); freeg();
                    freeh(); freei(); freej(); freek()
                )
        end
        and update(x, (a, b, c, d, e, f, g, h, i, j, k)) =
            (updatea(x, a); updateb(x ++ offsetb, b); updatec(x ++ offsetc, c); updated(x ++ offsetd, d);
             updatee(x ++ offsete, e); updatef(x ++ offsetf, f); updateg(x ++ offsetg, g);
             updateh(x ++ offseth, h); updatei(x ++ offseti, i); updatej(x ++ offsetj, j);
             updatek(x ++ offsetk, k))
    in
        {load=load, store=store, update=update,
         ctype = LowLevel.cStruct[ctypea, ctypeb, ctypec, ctyped, ctypee, ctypef, ctypeg, ctypeh, ctypei, ctypej,
                                  ctypek]}
    end
    
    fun cStruct12(a: 'a conversion, b: 'b conversion, c: 'c conversion, d: 'd conversion,
                  e: 'e conversion, f: 'f conversion, g: 'g conversion, h: 'h conversion,
                  i: 'i conversion, j: 'j conversion, k: 'k conversion, l: 'l conversion):
                  ('a*'b*'c*'d*'e*'f*'g*'h*'i*'j*'k*'l)conversion =
    let
        val {load=loada, store=storea, update=updatea, ctype = ctypea as {size=sizea, ...} } = a
        and {load=loadb, store=storeb, update=updateb, ctype = ctypeb as {size=sizeb, align=alignb, ...} } = b
        and {load=loadc, store=storec, update=updatec, ctype = ctypec as {size=sizec, align=alignc, ...} } = c
        and {load=loadd, store=stored, update=updated, ctype = ctyped as {size=sized, align=alignd, ...} } = d
        and {load=loade, store=storee, update=updatee, ctype = ctypee as {size=sizee, align=aligne, ...} } = e
        and {load=loadf, store=storef, update=updatef, ctype = ctypef as {size=sizef, align=alignf, ...} } = f
        and {load=loadg, store=storeg, update=updateg, ctype = ctypeg as {size=sizeg, align=aligng, ...} } = g
        and {load=loadh, store=storeh, update=updateh, ctype = ctypeh as {size=sizeh, align=alignh, ...} } = h
        and {load=loadi, store=storei, update=updatei, ctype = ctypei as {size=sizei, align=aligni, ...} } = i
        and {load=loadj, store=storej, update=updatej, ctype = ctypej as {size=sizej, align=alignj, ...} } = j
        and {load=loadk, store=storek, update=updatek, ctype = ctypek as {size=sizek, align=alignk, ...} } = k
        and {load=loadl, store=storel, update=updatel, ctype = ctypel as {align=alignl, ...} } = l

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
        let
            val freea = storea(x, a) and freeb = storeb(x ++ offsetb, b) and freec = storec(x ++ offsetc, c)
            and freed = stored(x ++ offsetd, d) and freee = storee(x ++ offsete, e) and freef = storef(x ++ offsetf, f)
            and freeg = storeg(x ++ offsetg, g) and freeh = storeh(x ++ offseth, h) and freei = storei(x ++ offseti, i)
            and freej = storej(x ++ offsetj, j) and freek = storek(x ++ offsetk, k) and freel = storel(x ++ offsetl, l)
        in
            fn () =>
                (
                    freea(); freeb(); freec(); freed(); freee(); freef(); freeg();
                    freeh(); freei(); freej(); freek(); freel()
                )
        end
        and update(x, (a, b, c, d, e, f, g, h, i, j, k, l)) =
            (updatea(x, a); updateb(x ++ offsetb, b); updatec(x ++ offsetc, c); updated(x ++ offsetd, d);
             updatee(x ++ offsete, e); updatef(x ++ offsetf, f); updateg(x ++ offsetg, g);
             updateh(x ++ offseth, h); updatei(x ++ offseti, i); updatej(x ++ offsetj, j);
             updatek(x ++ offsetk, k); updatel(x ++ offsetl, l))
    in
        {load=load, store=store, update=update,
         ctype = LowLevel.cStruct[ctypea, ctypeb, ctypec, ctyped, ctypee, ctypef, ctypeg, ctypeh, ctypei, ctypej,
                                  ctypek, ctypel]}
    end
    
    fun cStruct13(a: 'a conversion, b: 'b conversion, c: 'c conversion, d: 'd conversion,
                  e: 'e conversion, f: 'f conversion, g: 'g conversion, h: 'h conversion,
                  i: 'i conversion, j: 'j conversion, k: 'k conversion, l: 'l conversion,
                  m: 'm conversion):
                  ('a*'b*'c*'d*'e*'f*'g*'h*'i*'j*'k*'l*'m)conversion =
    let
        val {load=loada, store=storea, update=updatea, ctype = ctypea as {size=sizea, ...} } = a
        and {load=loadb, store=storeb, update=updateb, ctype = ctypeb as {size=sizeb, align=alignb, ...} } = b
        and {load=loadc, store=storec, update=updatec, ctype = ctypec as {size=sizec, align=alignc, ...} } = c
        and {load=loadd, store=stored, update=updated, ctype = ctyped as {size=sized, align=alignd, ...} } = d
        and {load=loade, store=storee, update=updatee, ctype = ctypee as {size=sizee, align=aligne, ...} } = e
        and {load=loadf, store=storef, update=updatef, ctype = ctypef as {size=sizef, align=alignf, ...} } = f
        and {load=loadg, store=storeg, update=updateg, ctype = ctypeg as {size=sizeg, align=aligng, ...} } = g
        and {load=loadh, store=storeh, update=updateh, ctype = ctypeh as {size=sizeh, align=alignh, ...} } = h
        and {load=loadi, store=storei, update=updatei, ctype = ctypei as {size=sizei, align=aligni, ...} } = i
        and {load=loadj, store=storej, update=updatej, ctype = ctypej as {size=sizej, align=alignj, ...} } = j
        and {load=loadk, store=storek, update=updatek, ctype = ctypek as {size=sizek, align=alignk, ...} } = k
        and {load=loadl, store=storel, update=updatel, ctype = ctypel as {size=sizel, align=alignl, ...} } = l
        and {load=loadm, store=storem, update=updatem, ctype = ctypem as {align=alignm, ...} } = m

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
        let
            val freea = storea(x, a) and freeb = storeb(x ++ offsetb, b) and freec = storec(x ++ offsetc, c)
            and freed = stored(x ++ offsetd, d) and freee = storee(x ++ offsete, e) and freef = storef(x ++ offsetf, f)
            and freeg = storeg(x ++ offsetg, g) and freeh = storeh(x ++ offseth, h) and freei = storei(x ++ offseti, i)
            and freej = storej(x ++ offsetj, j) and freek = storek(x ++ offsetk, k) and freel = storel(x ++ offsetl, l)
            and freem = storem(x ++ offsetm, m)
        in
            fn () =>
                (
                    freea(); freeb(); freec(); freed(); freee(); freef(); freeg();
                    freeh(); freei(); freej(); freek(); freel(); freem()
                )
        end
        and update(x, (a, b, c, d, e, f, g, h, i, j, k, l, m)) =
            (updatea(x, a); updateb(x ++ offsetb, b); updatec(x ++ offsetc, c); updated(x ++ offsetd, d);
             updatee(x ++ offsete, e); updatef(x ++ offsetf, f); updateg(x ++ offsetg, g);
             updateh(x ++ offseth, h); updatei(x ++ offseti, i); updatej(x ++ offsetj, j);
             updatek(x ++ offsetk, k); updatel(x ++ offsetl, l); updatem(x ++ offsetm, m))
    in
        {load=load, store=store, update=update,
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
        val {load=loada, store=storea, update=updatea, ctype = ctypea as {size=sizea, ...} } = a
        and {load=loadb, store=storeb, update=updateb, ctype = ctypeb as {size=sizeb, align=alignb, ...} } = b
        and {load=loadc, store=storec, update=updatec, ctype = ctypec as {size=sizec, align=alignc, ...} } = c
        and {load=loadd, store=stored, update=updated, ctype = ctyped as {size=sized, align=alignd, ...} } = d
        and {load=loade, store=storee, update=updatee, ctype = ctypee as {size=sizee, align=aligne, ...} } = e
        and {load=loadf, store=storef, update=updatef, ctype = ctypef as {size=sizef, align=alignf, ...} } = f
        and {load=loadg, store=storeg, update=updateg, ctype = ctypeg as {size=sizeg, align=aligng, ...} } = g
        and {load=loadh, store=storeh, update=updateh, ctype = ctypeh as {size=sizeh, align=alignh, ...} } = h
        and {load=loadi, store=storei, update=updatei, ctype = ctypei as {size=sizei, align=aligni, ...} } = i
        and {load=loadj, store=storej, update=updatej, ctype = ctypej as {size=sizej, align=alignj, ...} } = j
        and {load=loadk, store=storek, update=updatek, ctype = ctypek as {size=sizek, align=alignk, ...} } = k
        and {load=loadl, store=storel, update=updatel, ctype = ctypel as {size=sizel, align=alignl, ...} } = l
        and {load=loadm, store=storem, update=updatem, ctype = ctypem as {size=sizem, align=alignm, ...} } = m
        and {load=loadn, store=storen, update=updaten, ctype = ctypen as {align=alignn, ...} } = n

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
        let
            val freea = storea(x, a) and freeb = storeb(x ++ offsetb, b) and freec = storec(x ++ offsetc, c)
            and freed = stored(x ++ offsetd, d) and freee = storee(x ++ offsete, e) and freef = storef(x ++ offsetf, f)
            and freeg = storeg(x ++ offsetg, g) and freeh = storeh(x ++ offseth, h) and freei = storei(x ++ offseti, i)
            and freej = storej(x ++ offsetj, j) and freek = storek(x ++ offsetk, k) and freel = storel(x ++ offsetl, l)
            and freem = storem(x ++ offsetm, m) and freen = storen(x ++ offsetn, n)
        in
            fn () =>
                (
                    freea(); freeb(); freec(); freed(); freee(); freef(); freeg();
                    freeh(); freei(); freej(); freek(); freel(); freem();
                    freen()
                )
        end
        and update(x, (a, b, c, d, e, f, g, h, i, j, k, l, m, n)) =
            (updatea(x, a); updateb(x ++ offsetb, b); updatec(x ++ offsetc, c); updated(x ++ offsetd, d);
             updatee(x ++ offsete, e); updatef(x ++ offsetf, f); updateg(x ++ offsetg, g);
             updateh(x ++ offseth, h); updatei(x ++ offseti, i); updatej(x ++ offsetj, j);
             updatek(x ++ offsetk, k); updatel(x ++ offsetl, l); updatem(x ++ offsetm, m);
             updaten(x ++ offsetn, n))
    in
        {load=load, store=store, update=update,
         ctype = LowLevel.cStruct[ctypea, ctypeb, ctypec, ctyped, ctypee, ctypef, ctypeg, ctypeh, ctypei, ctypej,
                                  ctypek, ctypel, ctypem, ctypen]}
    end

    fun cStruct15(a: 'a conversion, b: 'b conversion, c: 'c conversion, d: 'd conversion,
                  e: 'e conversion, f: 'f conversion, g: 'g conversion, h: 'h conversion,
                  i: 'i conversion, j: 'j conversion, k: 'k conversion, l: 'l conversion,
                  m: 'm conversion, n: 'n conversion, o: 'o conversion):
                  ('a*'b*'c*'d*'e*'f*'g*'h*'i*'j*'k*'l*'m*'n*'o)conversion =
    let
        val {load=loada, store=storea, update=updatea, ctype = ctypea as {size=sizea, ...} } = a
        and {load=loadb, store=storeb, update=updateb, ctype = ctypeb as {size=sizeb, align=alignb, ...} } = b
        and {load=loadc, store=storec, update=updatec, ctype = ctypec as {size=sizec, align=alignc, ...} } = c
        and {load=loadd, store=stored, update=updated, ctype = ctyped as {size=sized, align=alignd, ...} } = d
        and {load=loade, store=storee, update=updatee, ctype = ctypee as {size=sizee, align=aligne, ...} } = e
        and {load=loadf, store=storef, update=updatef, ctype = ctypef as {size=sizef, align=alignf, ...} } = f
        and {load=loadg, store=storeg, update=updateg, ctype = ctypeg as {size=sizeg, align=aligng, ...} } = g
        and {load=loadh, store=storeh, update=updateh, ctype = ctypeh as {size=sizeh, align=alignh, ...} } = h
        and {load=loadi, store=storei, update=updatei, ctype = ctypei as {size=sizei, align=aligni, ...} } = i
        and {load=loadj, store=storej, update=updatej, ctype = ctypej as {size=sizej, align=alignj, ...} } = j
        and {load=loadk, store=storek, update=updatek, ctype = ctypek as {size=sizek, align=alignk, ...} } = k
        and {load=loadl, store=storel, update=updatel, ctype = ctypel as {size=sizel, align=alignl, ...} } = l
        and {load=loadm, store=storem, update=updatem, ctype = ctypem as {size=sizem, align=alignm, ...} } = m
        and {load=loadn, store=storen, update=updaten, ctype = ctypen as {size=sizen, align=alignn, ...} } = n
        and {load=loado, store=storeo, update=updateo, ctype = ctypeo as {align=aligno, ...} } = o

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
        let
            val freea = storea(x, a) and freeb = storeb(x ++ offsetb, b) and freec = storec(x ++ offsetc, c)
            and freed = stored(x ++ offsetd, d) and freee = storee(x ++ offsete, e) and freef = storef(x ++ offsetf, f)
            and freeg = storeg(x ++ offsetg, g) and freeh = storeh(x ++ offseth, h) and freei = storei(x ++ offseti, i)
            and freej = storej(x ++ offsetj, j) and freek = storek(x ++ offsetk, k) and freel = storel(x ++ offsetl, l)
            and freem = storem(x ++ offsetm, m) and freen = storen(x ++ offsetn, n) and freeo = storeo(x ++ offseto, o)
        in
            fn () =>
                (
                    freea(); freeb(); freec(); freed(); freee(); freef(); freeg();
                    freeh(); freei(); freej(); freek(); freel(); freem();
                    freen(); freeo()
                )
        end
        and update(x, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)) =
            (updatea(x, a); updateb(x ++ offsetb, b); updatec(x ++ offsetc, c); updated(x ++ offsetd, d);
             updatee(x ++ offsete, e); updatef(x ++ offsetf, f); updateg(x ++ offsetg, g);
             updateh(x ++ offseth, h); updatei(x ++ offseti, i); updatej(x ++ offsetj, j);
             updatek(x ++ offsetk, k); updatel(x ++ offsetl, l); updatem(x ++ offsetm, m);
             updaten(x ++ offsetn, n); updateo(x ++ offseto, o))
    in
        {load=load, store=store, update=update,
         ctype = LowLevel.cStruct[ctypea, ctypeb, ctypec, ctyped, ctypee, ctypef, ctypeg, ctypeh, ctypei, ctypej,
                                  ctypek, ctypel, ctypem, ctypen, ctypeo]}
    end

    fun cStruct16(a: 'a conversion, b: 'b conversion, c: 'c conversion, d: 'd conversion,
                  e: 'e conversion, f: 'f conversion, g: 'g conversion, h: 'h conversion,
                  i: 'i conversion, j: 'j conversion, k: 'k conversion, l: 'l conversion,
                  m: 'm conversion, n: 'n conversion, o: 'o conversion, p: 'p conversion):
                  ('a*'b*'c*'d*'e*'f*'g*'h*'i*'j*'k*'l*'m*'n*'o*'p)conversion =
    let
        val {load=loada, store=storea, update=updatea, ctype = ctypea as {size=sizea, ...} } = a
        and {load=loadb, store=storeb, update=updateb, ctype = ctypeb as {size=sizeb, align=alignb, ...} } = b
        and {load=loadc, store=storec, update=updatec, ctype = ctypec as {size=sizec, align=alignc, ...} } = c
        and {load=loadd, store=stored, update=updated, ctype = ctyped as {size=sized, align=alignd, ...} } = d
        and {load=loade, store=storee, update=updatee, ctype = ctypee as {size=sizee, align=aligne, ...} } = e
        and {load=loadf, store=storef, update=updatef, ctype = ctypef as {size=sizef, align=alignf, ...} } = f
        and {load=loadg, store=storeg, update=updateg, ctype = ctypeg as {size=sizeg, align=aligng, ...} } = g
        and {load=loadh, store=storeh, update=updateh, ctype = ctypeh as {size=sizeh, align=alignh, ...} } = h
        and {load=loadi, store=storei, update=updatei, ctype = ctypei as {size=sizei, align=aligni, ...} } = i
        and {load=loadj, store=storej, update=updatej, ctype = ctypej as {size=sizej, align=alignj, ...} } = j
        and {load=loadk, store=storek, update=updatek, ctype = ctypek as {size=sizek, align=alignk, ...} } = k
        and {load=loadl, store=storel, update=updatel, ctype = ctypel as {size=sizel, align=alignl, ...} } = l
        and {load=loadm, store=storem, update=updatem, ctype = ctypem as {size=sizem, align=alignm, ...} } = m
        and {load=loadn, store=storen, update=updaten, ctype = ctypen as {size=sizen, align=alignn, ...} } = n
        and {load=loado, store=storeo, update=updateo, ctype = ctypeo as {size=sizeo, align=aligno, ...} } = o
        and {load=loadp, store=storep, update=updatep, ctype = ctypep as {align=alignp, ...} } = p

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
        let
            val freea = storea(x, a) and freeb = storeb(x ++ offsetb, b) and freec = storec(x ++ offsetc, c)
            and freed = stored(x ++ offsetd, d) and freee = storee(x ++ offsete, e) and freef = storef(x ++ offsetf, f)
            and freeg = storeg(x ++ offsetg, g) and freeh = storeh(x ++ offseth, h) and freei = storei(x ++ offseti, i)
            and freej = storej(x ++ offsetj, j) and freek = storek(x ++ offsetk, k) and freel = storel(x ++ offsetl, l)
            and freem = storem(x ++ offsetm, m) and freen = storen(x ++ offsetn, n) and freeo = storeo(x ++ offseto, o)
            and freep = storep(x ++ offsetp, p)
        in
            fn () =>
                (
                    freea(); freeb(); freec(); freed(); freee(); freef();
                    freeg(); freeh(); freei(); freej(); freek(); freel();
                    freem(); freen(); freeo(); freep()
                )
        end
        and update(x, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)) =
            (updatea(x, a); updateb(x ++ offsetb, b); updatec(x ++ offsetc, c); updated(x ++ offsetd, d);
             updatee(x ++ offsete, e); updatef(x ++ offsetf, f); updateg(x ++ offsetg, g);
             updateh(x ++ offseth, h); updatei(x ++ offseti, i); updatej(x ++ offsetj, j);
             updatek(x ++ offsetk, k); updatel(x ++ offsetl, l); updatem(x ++ offsetm, m);
             updaten(x ++ offsetn, n); updateo(x ++ offseto, o); updatep(x ++ offsetp, p))
    in
        {load=load, store=store, update=update,
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
        val {load=loada, store=storea, update=updatea, ctype = ctypea as {size=sizea, ...} } = a
        and {load=loadb, store=storeb, update=updateb, ctype = ctypeb as {size=sizeb, align=alignb, ...} } = b
        and {load=loadc, store=storec, update=updatec, ctype = ctypec as {size=sizec, align=alignc, ...} } = c
        and {load=loadd, store=stored, update=updated, ctype = ctyped as {size=sized, align=alignd, ...} } = d
        and {load=loade, store=storee, update=updatee, ctype = ctypee as {size=sizee, align=aligne, ...} } = e
        and {load=loadf, store=storef, update=updatef, ctype = ctypef as {size=sizef, align=alignf, ...} } = f
        and {load=loadg, store=storeg, update=updateg, ctype = ctypeg as {size=sizeg, align=aligng, ...} } = g
        and {load=loadh, store=storeh, update=updateh, ctype = ctypeh as {size=sizeh, align=alignh, ...} } = h
        and {load=loadi, store=storei, update=updatei, ctype = ctypei as {size=sizei, align=aligni, ...} } = i
        and {load=loadj, store=storej, update=updatej, ctype = ctypej as {size=sizej, align=alignj, ...} } = j
        and {load=loadk, store=storek, update=updatek, ctype = ctypek as {size=sizek, align=alignk, ...} } = k
        and {load=loadl, store=storel, update=updatel, ctype = ctypel as {size=sizel, align=alignl, ...} } = l
        and {load=loadm, store=storem, update=updatem, ctype = ctypem as {size=sizem, align=alignm, ...} } = m
        and {load=loadn, store=storen, update=updaten, ctype = ctypen as {size=sizen, align=alignn, ...} } = n
        and {load=loado, store=storeo, update=updateo, ctype = ctypeo as {size=sizeo, align=aligno, ...} } = o
        and {load=loadp, store=storep, update=updatep, ctype = ctypep as {size=sizep, align=alignp, ...} } = p
        and {load=loadq, store=storeq, update=updateq, ctype = ctypeq as {align=alignq, ...} } = q

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
        let
            val freea = storea(x, a) and freeb = storeb(x ++ offsetb, b) and freec = storec(x ++ offsetc, c)
            and freed = stored(x ++ offsetd, d) and freee = storee(x ++ offsete, e) and freef = storef(x ++ offsetf, f)
            and freeg = storeg(x ++ offsetg, g) and freeh = storeh(x ++ offseth, h) and freei = storei(x ++ offseti, i)
            and freej = storej(x ++ offsetj, j) and freek = storek(x ++ offsetk, k) and freel = storel(x ++ offsetl, l)
            and freem = storem(x ++ offsetm, m) and freen = storen(x ++ offsetn, n) and freeo = storeo(x ++ offseto, o)
            and freep = storep(x ++ offsetp, p) and freeq = storeq(x ++ offsetq, q)
        in
            fn () =>
                (
                    freea(); freeb(); freec(); freed(); freee(); freef(); freeg();
                    freeh(); freei(); freej(); freek(); freel(); freem();
                    freen(); freeo(); freep(); freeq()
                )
        end
        and update(x, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)) =
            (updatea(x, a); updateb(x ++ offsetb, b); updatec(x ++ offsetc, c); updated(x ++ offsetd, d);
             updatee(x ++ offsete, e); updatef(x ++ offsetf, f); updateg(x ++ offsetg, g);
             updateh(x ++ offseth, h); updatei(x ++ offseti, i); updatej(x ++ offsetj, j);
             updatek(x ++ offsetk, k); updatel(x ++ offsetl, l); updatem(x ++ offsetm, m);
             updaten(x ++ offsetn, n); updateo(x ++ offseto, o); updatep(x ++ offsetp, p);
             updateq(x ++ offsetq, q))
    in
        {load=load, store=store, update=update,
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
        val {load=loada, store=storea, update=updatea, ctype = ctypea as {size=sizea, ...} } = a
        and {load=loadb, store=storeb, update=updateb, ctype = ctypeb as {size=sizeb, align=alignb, ...} } = b
        and {load=loadc, store=storec, update=updatec, ctype = ctypec as {size=sizec, align=alignc, ...} } = c
        and {load=loadd, store=stored, update=updated, ctype = ctyped as {size=sized, align=alignd, ...} } = d
        and {load=loade, store=storee, update=updatee, ctype = ctypee as {size=sizee, align=aligne, ...} } = e
        and {load=loadf, store=storef, update=updatef, ctype = ctypef as {size=sizef, align=alignf, ...} } = f
        and {load=loadg, store=storeg, update=updateg, ctype = ctypeg as {size=sizeg, align=aligng, ...} } = g
        and {load=loadh, store=storeh, update=updateh, ctype = ctypeh as {size=sizeh, align=alignh, ...} } = h
        and {load=loadi, store=storei, update=updatei, ctype = ctypei as {size=sizei, align=aligni, ...} } = i
        and {load=loadj, store=storej, update=updatej, ctype = ctypej as {size=sizej, align=alignj, ...} } = j
        and {load=loadk, store=storek, update=updatek, ctype = ctypek as {size=sizek, align=alignk, ...} } = k
        and {load=loadl, store=storel, update=updatel, ctype = ctypel as {size=sizel, align=alignl, ...} } = l
        and {load=loadm, store=storem, update=updatem, ctype = ctypem as {size=sizem, align=alignm, ...} } = m
        and {load=loadn, store=storen, update=updaten, ctype = ctypen as {size=sizen, align=alignn, ...} } = n
        and {load=loado, store=storeo, update=updateo, ctype = ctypeo as {size=sizeo, align=aligno, ...} } = o
        and {load=loadp, store=storep, update=updatep, ctype = ctypep as {size=sizep, align=alignp, ...} } = p
        and {load=loadq, store=storeq, update=updateq, ctype = ctypeq as {size=sizeq, align=alignq, ...} } = q
        and {load=loadr, store=storer, update=updater, ctype = ctyper as {align=alignr, ...} } = r

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
        let
            val freea = storea(x, a) and freeb = storeb(x ++ offsetb, b) and freec = storec(x ++ offsetc, c)
            and freed = stored(x ++ offsetd, d) and freee = storee(x ++ offsete, e) and freef = storef(x ++ offsetf, f)
            and freeg = storeg(x ++ offsetg, g) and freeh = storeh(x ++ offseth, h) and freei = storei(x ++ offseti, i)
            and freej = storej(x ++ offsetj, j) and freek = storek(x ++ offsetk, k) and freel = storel(x ++ offsetl, l)
            and freem = storem(x ++ offsetm, m) and freen = storen(x ++ offsetn, n) and freeo = storeo(x ++ offseto, o)
            and freep = storep(x ++ offsetp, p) and freeq = storeq(x ++ offsetq, q) and freer = storer(x ++ offsetr, r)
        in
            fn () =>
                (
                    freea(); freeb(); freec(); freed(); freee(); freef(); freeg();
                    freeh(); freei(); freej(); freek(); freel(); freem();
                    freen(); freeo(); freep(); freeq(); freer()
                )
        end
        and update(x, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)) =
            (updatea(x, a); updateb(x ++ offsetb, b); updatec(x ++ offsetc, c); updated(x ++ offsetd, d);
             updatee(x ++ offsete, e); updatef(x ++ offsetf, f); updateg(x ++ offsetg, g);
             updateh(x ++ offseth, h); updatei(x ++ offseti, i); updatej(x ++ offsetj, j);
             updatek(x ++ offsetk, k); updatel(x ++ offsetl, l); updatem(x ++ offsetm, m);
             updaten(x ++ offsetn, n); updateo(x ++ offseto, o); updatep(x ++ offsetp, p);
             updateq(x ++ offsetq, q); updater(x ++ offsetr, r))
    in
        {load=load, store=store, update=update,
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
        val {load=loada, store=storea, update=updatea, ctype = ctypea as {size=sizea, ...} } = a
        and {load=loadb, store=storeb, update=updateb, ctype = ctypeb as {size=sizeb, align=alignb, ...} } = b
        and {load=loadc, store=storec, update=updatec, ctype = ctypec as {size=sizec, align=alignc, ...} } = c
        and {load=loadd, store=stored, update=updated, ctype = ctyped as {size=sized, align=alignd, ...} } = d
        and {load=loade, store=storee, update=updatee, ctype = ctypee as {size=sizee, align=aligne, ...} } = e
        and {load=loadf, store=storef, update=updatef, ctype = ctypef as {size=sizef, align=alignf, ...} } = f
        and {load=loadg, store=storeg, update=updateg, ctype = ctypeg as {size=sizeg, align=aligng, ...} } = g
        and {load=loadh, store=storeh, update=updateh, ctype = ctypeh as {size=sizeh, align=alignh, ...} } = h
        and {load=loadi, store=storei, update=updatei, ctype = ctypei as {size=sizei, align=aligni, ...} } = i
        and {load=loadj, store=storej, update=updatej, ctype = ctypej as {size=sizej, align=alignj, ...} } = j
        and {load=loadk, store=storek, update=updatek, ctype = ctypek as {size=sizek, align=alignk, ...} } = k
        and {load=loadl, store=storel, update=updatel, ctype = ctypel as {size=sizel, align=alignl, ...} } = l
        and {load=loadm, store=storem, update=updatem, ctype = ctypem as {size=sizem, align=alignm, ...} } = m
        and {load=loadn, store=storen, update=updaten, ctype = ctypen as {size=sizen, align=alignn, ...} } = n
        and {load=loado, store=storeo, update=updateo, ctype = ctypeo as {size=sizeo, align=aligno, ...} } = o
        and {load=loadp, store=storep, update=updatep, ctype = ctypep as {size=sizep, align=alignp, ...} } = p
        and {load=loadq, store=storeq, update=updateq, ctype = ctypeq as {size=sizeq, align=alignq, ...} } = q
        and {load=loadr, store=storer, update=updater, ctype = ctyper as {size=sizer, align=alignr, ...} } = r
        and {load=loads, store=stores, update=updates, ctype = ctypes as {align=aligns, ...} } = s

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
        let
            val freea = storea(x, a) and freeb = storeb(x ++ offsetb, b) and freec = storec(x ++ offsetc, c)
            and freed = stored(x ++ offsetd, d) and freee = storee(x ++ offsete, e) and freef = storef(x ++ offsetf, f)
            and freeg = storeg(x ++ offsetg, g) and freeh = storeh(x ++ offseth, h) and freei = storei(x ++ offseti, i)
            and freej = storej(x ++ offsetj, j) and freek = storek(x ++ offsetk, k) and freel = storel(x ++ offsetl, l)
            and freem = storem(x ++ offsetm, m) and freen = storen(x ++ offsetn, n) and freeo = storeo(x ++ offseto, o)
            and freep = storep(x ++ offsetp, p) and freeq = storeq(x ++ offsetq, q) and freer = storer(x ++ offsetr, r)
            and frees = stores(x ++ offsets, s)
        in
            fn () =>
                (
                    freea(); freeb(); freec(); freed(); freee(); freef(); freeg();
                    freeh(); freei(); freej(); freek(); freel(); freem();
                    freen(); freeo(); freep(); freeq(); freer(); frees()
                )
        end
        and update(x, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)) =
            (updatea(x, a); updateb(x ++ offsetb, b); updatec(x ++ offsetc, c); updated(x ++ offsetd, d);
             updatee(x ++ offsete, e); updatef(x ++ offsetf, f); updateg(x ++ offsetg, g);
             updateh(x ++ offseth, h); updatei(x ++ offseti, i); updatej(x ++ offsetj, j);
             updatek(x ++ offsetk, k); updatel(x ++ offsetl, l); updatem(x ++ offsetm, m);
             updaten(x ++ offsetn, n); updateo(x ++ offseto, o); updatep(x ++ offsetp, p);
             updateq(x ++ offsetq, q); updater(x ++ offsetr, r); updates(x ++ offsets, s))
    in
        {load=load, store=store, update=update,
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
        val {load=loada, store=storea, update=updatea, ctype = ctypea as {size=sizea, ...} } = a
        and {load=loadb, store=storeb, update=updateb, ctype = ctypeb as {size=sizeb, align=alignb, ...} } = b
        and {load=loadc, store=storec, update=updatec, ctype = ctypec as {size=sizec, align=alignc, ...} } = c
        and {load=loadd, store=stored, update=updated, ctype = ctyped as {size=sized, align=alignd, ...} } = d
        and {load=loade, store=storee, update=updatee, ctype = ctypee as {size=sizee, align=aligne, ...} } = e
        and {load=loadf, store=storef, update=updatef, ctype = ctypef as {size=sizef, align=alignf, ...} } = f
        and {load=loadg, store=storeg, update=updateg, ctype = ctypeg as {size=sizeg, align=aligng, ...} } = g
        and {load=loadh, store=storeh, update=updateh, ctype = ctypeh as {size=sizeh, align=alignh, ...} } = h
        and {load=loadi, store=storei, update=updatei, ctype = ctypei as {size=sizei, align=aligni, ...} } = i
        and {load=loadj, store=storej, update=updatej, ctype = ctypej as {size=sizej, align=alignj, ...} } = j
        and {load=loadk, store=storek, update=updatek, ctype = ctypek as {size=sizek, align=alignk, ...} } = k
        and {load=loadl, store=storel, update=updatel, ctype = ctypel as {size=sizel, align=alignl, ...} } = l
        and {load=loadm, store=storem, update=updatem, ctype = ctypem as {size=sizem, align=alignm, ...} } = m
        and {load=loadn, store=storen, update=updaten, ctype = ctypen as {size=sizen, align=alignn, ...} } = n
        and {load=loado, store=storeo, update=updateo, ctype = ctypeo as {size=sizeo, align=aligno, ...} } = o
        and {load=loadp, store=storep, update=updatep, ctype = ctypep as {size=sizep, align=alignp, ...} } = p
        and {load=loadq, store=storeq, update=updateq, ctype = ctypeq as {size=sizeq, align=alignq, ...} } = q
        and {load=loadr, store=storer, update=updater, ctype = ctyper as {size=sizer, align=alignr, ...} } = r
        and {load=loads, store=stores, update=updates, ctype = ctypes as {size=sizes, align=aligns, ...} } = s
        and {load=loadt, store=storet, update=updatet, ctype = ctypet as {align=alignt, ...} } = t

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
        let
            val freea = storea(x, a) and freeb = storeb(x ++ offsetb, b) and freec = storec(x ++ offsetc, c)
            and freed = stored(x ++ offsetd, d) and freee = storee(x ++ offsete, e) and freef = storef(x ++ offsetf, f)
            and freeg = storeg(x ++ offsetg, g) and freeh = storeh(x ++ offseth, h) and freei = storei(x ++ offseti, i)
            and freej = storej(x ++ offsetj, j) and freek = storek(x ++ offsetk, k) and freel = storel(x ++ offsetl, l)
            and freem = storem(x ++ offsetm, m) and freen = storen(x ++ offsetn, n) and freeo = storeo(x ++ offseto, o)
            and freep = storep(x ++ offsetp, p) and freeq = storeq(x ++ offsetq, q) and freer = storer(x ++ offsetr, r)
            and frees = stores(x ++ offsets, s) and freet = storet(x ++ offsett, t)
        in
            fn () =>
                (
                    freea(); freeb(); freec(); freed(); freee(); freef(); freeg();
                    freeh(); freei(); freej(); freek(); freel(); freem();
                    freen(); freeo(); freep(); freeq(); freer(); frees(); freet()
                )
        end
        and update(x, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)) =
            (updatea(x, a); updateb(x ++ offsetb, b); updatec(x ++ offsetc, c); updated(x ++ offsetd, d);
             updatee(x ++ offsete, e); updatef(x ++ offsetf, f); updateg(x ++ offsetg, g);
             updateh(x ++ offseth, h); updatei(x ++ offseti, i); updatej(x ++ offsetj, j);
             updatek(x ++ offsetk, k); updatel(x ++ offsetl, l); updatem(x ++ offsetm, m);
             updaten(x ++ offsetn, n); updateo(x ++ offseto, o); updatep(x ++ offsetp, p);
             updateq(x ++ offsetq, q); updater(x ++ offsetr, r); updates(x ++ offsets, s); updatet(x ++ offsett, t))
    in
        {load=load, store=store, update=update,
         ctype = LowLevel.cStruct[ctypea, ctypeb, ctypec, ctyped, ctypee, ctypef, ctypeg, ctypeh, ctypei, ctypej,
                                  ctypek, ctypel, ctypem, ctypen, ctypeo, ctypep, ctypeq, ctyper, ctypes, ctypet]}
    end

    (* Conversion for call-by-reference. *)
    local
        open Memory LowLevel
    in
        fun cStar({load=loada, store=storea, update=updatea, ctype=ctypea}: 'a conversion): 'a ref conversion =
        let
            (* This would only be used if we returned a cStar. *)
            fun load s = ref(loada(getAddress(s, 0w0)))
            
            fun store(m, ref s) =
            let
                (* When we pass a ref X into a cStar cX function we need to
                   allocate a memory cell big enough for a cX value.  Then
                   we copy the current value of the ML into this.  We set
                   the argument, a pointer, to the address of the cell. *)
                val mem = malloc(#size ctypea)
                val () = setAddress(m, 0w0, mem)
                val freea = storea(mem, s)
            in
                fn () => (free mem; freea())
            end
            
            (* I'm not sure about whether we should do anything with the
               inherited update here. *)
            fun update(m, s) =
            let
                val mem = getAddress(m, 0w0) (* The address of our cell. *)
                val olds = !s
            in
                s := loada mem; (* Update the ref from the value in the cell. *)
                updatea(mem, olds)
            end
        in
            {load=load, store=store, update=update, ctype = cTypePointer}
        end

        (* Similar to cStar but without the need to update the result. *)
        fun cConstStar({load=loada, store=storea, update=updatea, ctype=ctypea}: 'a conversion): 'a conversion =
        let
            fun load s = loada(getAddress(s, 0w0))
            
            fun store(m, s) =
            let
                val mem = malloc(#size ctypea)
                val () = setAddress(m, 0w0, mem)
                val freea = storea(mem, s)
            in
                fn () => (free mem; freea())
            end
            
            (* Do we have to do anything here?  Could we pass a const pointer
               to a structure with variable fields? *)
            fun update(m, s) =
            let
                val mem = getAddress(m, 0w0) (* The address of our cell. *)
            in
                updatea(mem, s)
            end
        in
            {load=load, store=store, update=update, ctype = cTypePointer}
        end
    end

    (* Calls with conversion. *)
    (* Note: it may be possible to have general functions to compute offsets
       but we don't do that because this way the compiler can compute the offsets
       as constants during inline expansion. *)
    local
        open LibFFI Memory LowLevel
    in
        fun callInternal0withAbi (abi: abi) ()
            ({ctype = resType, load= resLoad, ...} : 'a conversion): symbol -> unit->'a =
        let
            val callF = callwithAbi abi [] resType
        in
            fn fnAddr =>
            let
                val cf = callF fnAddr
            in
                fn () =>
                let
                    val rMem = malloc(#size resType)
                in
                    let
                        val () = cf([], rMem)
                        val result = resLoad rMem
                    in
                        free rMem;
                        result
                    end handle exn => (free rMem; raise exn)
                end
            end
        end

        fun call0withAbi abi symbol argTypes resType = callInternal0withAbi abi argTypes resType symbol
        fun call0 x = call0withAbi abiDefault x (* Have to make it a fun to avoid value restriction *)

        fun callInternal1withAbi (abi: abi)
            ({ ctype = argType, store = argStore, update = argUpdate, ...}: 'a conversion)
            ({ ctype = resType, load= resLoad, ...}: 'b conversion): symbol -> 'a ->'b =
        let
            val callF = callwithAbi abi [argType] resType
        in
            fn fnAddr =>
            let
                val cf = callF fnAddr
            in
                fn x =>
                let
                    (* Allocate space for argument(s) and result.
                       We can't use cStruct here because we only store the
                       argument before the call and load the result after. *)
                    val argOffset = alignUp(#size resType, #align argType)
                    val rMem = malloc(argOffset + #size argType)
                    val argAddr = rMem ++ argOffset
                    val freea = argStore (argAddr, x)
                    fun freeAll () = (freea(); free rMem)
                in
                    let
                        val () = cf([argAddr], rMem)
                        val result = resLoad rMem
                    in
                        argUpdate (argAddr, x);
                        freeAll ();
                        result
                    end handle exn => (freeAll (); raise exn)
                end
            end
        end

        fun call1withAbi abi symbol argTypes resType = callInternal1withAbi abi argTypes resType symbol
        fun call1 x = call1withAbi abiDefault x (* Have to make it a fun to avoid value restriction *)

        fun callInternal2withAbi (abi: abi)
            ({ ctype = arg1Type, store = arg1Store, update = arg1Update, ...}: 'a conversion,
             { ctype = arg2Type, store = arg2Store, update = arg2Update, ...}: 'b conversion)
            ({ ctype = resType, load= resLoad, ...}: 'c conversion): symbol -> 'a * 'b -> 'c =
        let
            val callF = callwithAbi abi [arg1Type, arg2Type] resType
        in
            fn fnAddr =>
            let
                val cf = callF fnAddr
            in
                fn (a, b) =>
                let
                    val arg1Offset = alignUp(#size resType, #align arg1Type)
                    val arg2Offset = alignUp(arg1Offset + #size arg1Type, #align arg2Type)
                    val rMem = malloc(arg2Offset + #size arg2Type)
                    val arg1Addr = rMem ++ arg1Offset
                    val arg2Addr = rMem ++ arg2Offset
                    val freea = arg1Store (arg1Addr, a)
                    val freeb = arg2Store (arg2Addr, b)
                    fun freeAll() = (freea(); freeb(); free rMem)
                in
                    let
                        val () = cf([arg1Addr, arg2Addr], rMem)
                        val result = resLoad rMem
                    in
                        arg1Update(arg1Addr, a); arg2Update (arg2Addr, b);
                        freeAll();
                        result
                    end handle exn => (freeAll(); raise exn)
                end
            end
        end

        fun call2withAbi abi symbol argTypes resType = callInternal2withAbi abi argTypes resType symbol
        fun call2 x = call2withAbi abiDefault x (* Have to make it a fun to avoid value restriction *)

        fun callInternal3withAbi (abi: abi)
            ({ ctype = arg1Type, store = arg1Store, update = arg1Update, ...}: 'a conversion,
             { ctype = arg2Type, store = arg2Store, update = arg2Update, ...}: 'b conversion,
             { ctype = arg3Type, store = arg3Store, update = arg3Update, ...}: 'c conversion)
            ({ ctype = resType, load= resLoad, ...}: 'd conversion): symbol -> 'a * 'b *'c -> 'd =
        let
            val callF = callwithAbi abi [arg1Type, arg2Type, arg3Type] resType
        in
            fn fnAddr =>
            let
                val cf = callF fnAddr
            in
                fn (a, b, c) =>
                let
                    val arg1Offset = alignUp(#size resType, #align arg1Type)
                    val arg2Offset = alignUp(arg1Offset + #size arg1Type, #align arg2Type)
                    val arg3Offset = alignUp(arg2Offset + #size arg2Type, #align arg3Type)
                    val rMem = malloc(arg3Offset + #size arg3Type)
                    val arg1Addr = rMem ++ arg1Offset
                    val arg2Addr = rMem ++ arg2Offset
                    val arg3Addr = rMem ++ arg3Offset
                    val freea = arg1Store (arg1Addr, a)
                    val freeb = arg2Store (arg2Addr, b)
                    val freec = arg3Store (arg3Addr, c)
                    fun freeAll() = (freea(); freeb(); freec(); free rMem)
                in
                    let
                        val () = cf([arg1Addr, arg2Addr, arg3Addr], rMem)
                        val result = resLoad rMem
                    in
                        arg1Update(arg1Addr, a); arg2Update (arg2Addr, b); arg3Update (arg3Addr, c);
                        freeAll();
                        result
                    end handle exn => (freeAll(); raise exn)
                end
            end
        end

        fun call3withAbi abi symbol argTypes resType = callInternal3withAbi abi argTypes resType symbol
        fun call3 x = call3withAbi abiDefault x (* Have to make it a fun to avoid value restriction *)

        fun callInternal4withAbi (abi: abi)
            ({ ctype = arg1Type, store = arg1Store, update = arg1Update, ...}: 'a conversion,
             { ctype = arg2Type, store = arg2Store, update = arg2Update, ...}: 'b conversion,
             { ctype = arg3Type, store = arg3Store, update = arg3Update, ...}: 'c conversion,
             { ctype = arg4Type, store = arg4Store, update = arg4Update, ...}: 'd conversion)             
            ({ ctype = resType, load= resLoad, ...}: 'e conversion): symbol -> 'a * 'b *'c * 'd -> 'e =
        let
            val callF = callwithAbi abi [arg1Type, arg2Type, arg3Type, arg4Type] resType
        in
            fn fnAddr =>
            let
                val cf = callF fnAddr
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
                    val freea = arg1Store (arg1Addr, a)
                    val freeb = arg2Store (arg2Addr, b)
                    val freec = arg3Store (arg3Addr, c)
                    val freed = arg4Store (arg4Addr, d)
                    fun freeAll() = (freea(); freeb(); freec(); freed(); free rMem)
                in
                    let
                        val () = cf([arg1Addr, arg2Addr, arg3Addr, arg4Addr], rMem)
                        val result = resLoad rMem
                    in
                        arg1Update(arg1Addr, a); arg2Update (arg2Addr, b); arg3Update (arg3Addr, c);
                        arg4Update (arg4Addr, d);
                        freeAll();
                        result
                    end handle exn => (freeAll(); raise exn)
                end
            end
        end

        fun call4withAbi abi symbol argTypes resType = callInternal4withAbi abi argTypes resType symbol
        fun call4 x = call4withAbi abiDefault x

        fun callInternal5withAbi (abi: abi)
            ({ ctype = arg1Type, store = arg1Store, update = arg1Update, ...}: 'a conversion,
             { ctype = arg2Type, store = arg2Store, update = arg2Update, ...}: 'b conversion,
             { ctype = arg3Type, store = arg3Store, update = arg3Update, ...}: 'c conversion,
             { ctype = arg4Type, store = arg4Store, update = arg4Update, ...}: 'd conversion,
             { ctype = arg5Type, store = arg5Store, update = arg5Update, ...}: 'e conversion)             
            ({ ctype = resType, load= resLoad, ...}: 'f conversion): symbol -> 'a * 'b *'c * 'd * 'e -> 'f =
        let
            val callF =
                callwithAbi abi [arg1Type, arg2Type, arg3Type, arg4Type, arg5Type] resType
        in
            fn fnAddr =>
            let
                val cf = callF fnAddr
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
                    val freea = arg1Store (arg1Addr, a)
                    val freeb = arg2Store (arg2Addr, b)
                    val freec = arg3Store (arg3Addr, c)
                    val freed = arg4Store (arg4Addr, d)
                    val freee = arg5Store (arg5Addr, e)
                    fun freeAll() =
                        (freea(); freeb(); freec(); freed(); freee(); free rMem)
                in
                    let
                        val () = cf([arg1Addr, arg2Addr, arg3Addr, arg4Addr, arg5Addr], rMem)
                        val result = resLoad rMem
                    in
                        arg1Update(arg1Addr, a); arg2Update (arg2Addr, b); arg3Update (arg3Addr, c);
                        arg4Update (arg4Addr, d); arg5Update (arg5Addr, e);
                        freeAll();
                        result
                    end handle exn => (freeAll(); raise exn)
                end
            end
        end

        fun call5withAbi abi symbol argTypes resType = callInternal5withAbi abi argTypes resType symbol
        fun call5 x = call5withAbi abiDefault x

        fun callInternal6withAbi (abi: abi)
            ({ ctype = arg1Type, store = arg1Store, update = arg1Update, ...}: 'a conversion,
             { ctype = arg2Type, store = arg2Store, update = arg2Update, ...}: 'b conversion,
             { ctype = arg3Type, store = arg3Store, update = arg3Update, ...}: 'c conversion,
             { ctype = arg4Type, store = arg4Store, update = arg4Update, ...}: 'd conversion,
             { ctype = arg5Type, store = arg5Store, update = arg5Update, ...}: 'e conversion,             
             { ctype = arg6Type, store = arg6Store, update = arg6Update, ...}: 'f conversion)             
            ({ ctype = resType, load= resLoad, ...}: 'g conversion): symbol -> 'a * 'b *'c * 'd * 'e * 'f -> 'g =
        let
            val callF =
                callwithAbi abi [arg1Type, arg2Type, arg3Type, arg4Type, arg5Type, arg6Type] resType
        in
            fn fnAddr =>
            let
                val cf = callF fnAddr
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
                    val freea = arg1Store (arg1Addr, a)
                    val freeb = arg2Store (arg2Addr, b)
                    val freec = arg3Store (arg3Addr, c)
                    val freed = arg4Store (arg4Addr, d)
                    val freee = arg5Store (arg5Addr, e)
                    val freef = arg6Store (arg6Addr, f)
                    fun freeAll() =
                        (freea(); freeb(); freec(); freed(); freee(); freef(); free rMem)
                in
                    let
                        val () = cf([arg1Addr, arg2Addr, arg3Addr, arg4Addr, arg5Addr , arg6Addr], rMem)
                        val result = resLoad rMem
                    in
                        arg1Update(arg1Addr, a); arg2Update (arg2Addr, b); arg3Update (arg3Addr, c);
                        arg4Update (arg4Addr, d); arg5Update (arg5Addr, e); arg6Update (arg6Addr, f);
                        freeAll();
                        result
                    end handle exn => (freeAll(); raise exn)
                end
            end
        end

        fun call6withAbi abi symbol argTypes resType = callInternal6withAbi abi argTypes resType symbol
        fun call6 x = call6withAbi abiDefault x

        fun callInternal7withAbi (abi: abi)
            ({ ctype = arg1Type, store = arg1Store, update = arg1Update, ...}: 'a conversion,
             { ctype = arg2Type, store = arg2Store, update = arg2Update, ...}: 'b conversion,
             { ctype = arg3Type, store = arg3Store, update = arg3Update, ...}: 'c conversion,
             { ctype = arg4Type, store = arg4Store, update = arg4Update, ...}: 'd conversion,
             { ctype = arg5Type, store = arg5Store, update = arg5Update, ...}: 'e conversion,             
             { ctype = arg6Type, store = arg6Store, update = arg6Update, ...}: 'f conversion,             
             { ctype = arg7Type, store = arg7Store, update = arg7Update, ...}: 'g conversion)             
            ({ ctype = resType, load= resLoad, ...}: 'h conversion):
                symbol -> 'a * 'b *'c * 'd * 'e * 'f * 'g -> 'h =
        let
            val callF =
                callwithAbi abi [arg1Type, arg2Type, arg3Type, arg4Type, arg5Type, arg6Type, arg7Type] resType
        in
            fn fnAddr =>
            let
                val cf = callF fnAddr
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
                    val freea = arg1Store (arg1Addr, a)
                    val freeb = arg2Store (arg2Addr, b)
                    val freec = arg3Store (arg3Addr, c)
                    val freed = arg4Store (arg4Addr, d)
                    val freee = arg5Store (arg5Addr, e)
                    val freef = arg6Store (arg6Addr, f)
                    val freeg = arg7Store (arg7Addr, g)
                    fun freeAll() =
                        (freea(); freeb(); freec(); freed(); freee(); freef(); freeg(); free rMem)
                in
                    let
                        val () = cf([arg1Addr, arg2Addr, arg3Addr, arg4Addr, arg5Addr, arg6Addr, arg7Addr], rMem)
                        val result = resLoad rMem
                    in
                        arg1Update(arg1Addr, a); arg2Update (arg2Addr, b); arg3Update (arg3Addr, c);
                        arg4Update (arg4Addr, d); arg5Update (arg5Addr, e); arg6Update (arg6Addr, f);
                        arg7Update (arg7Addr, g);
                        freeAll();
                        result
                    end handle exn => (freeAll(); raise exn)
                end
            end
        end

        fun call7withAbi abi symbol argTypes resType = callInternal7withAbi abi argTypes resType symbol
        fun call7 x = call7withAbi abiDefault x

        fun callInternal8withAbi (abi: abi)
            ({ ctype = arg1Type, store = arg1Store, update = arg1Update, ...}: 'a conversion,
             { ctype = arg2Type, store = arg2Store, update = arg2Update, ...}: 'b conversion,
             { ctype = arg3Type, store = arg3Store, update = arg3Update, ...}: 'c conversion,
             { ctype = arg4Type, store = arg4Store, update = arg4Update, ...}: 'd conversion,
             { ctype = arg5Type, store = arg5Store, update = arg5Update, ...}: 'e conversion,             
             { ctype = arg6Type, store = arg6Store, update = arg6Update, ...}: 'f conversion,             
             { ctype = arg7Type, store = arg7Store, update = arg7Update, ...}: 'g conversion,             
             { ctype = arg8Type, store = arg8Store, update = arg8Update, ...}: 'h conversion)             
            ({ ctype = resType, load= resLoad, ...}: 'i conversion):
                symbol -> 'a * 'b *'c * 'd * 'e * 'f * 'g * 'h -> 'i =
        let
            val callF =
                callwithAbi abi
                    [arg1Type, arg2Type, arg3Type, arg4Type, arg5Type, arg6Type, arg7Type, arg8Type] resType
        in
            fn fnAddr =>
            let
                val cf = callF fnAddr
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
                    val freea = arg1Store (arg1Addr, a)
                    val freeb = arg2Store (arg2Addr, b)
                    val freec = arg3Store (arg3Addr, c)
                    val freed = arg4Store (arg4Addr, d)
                    val freee = arg5Store (arg5Addr, e)
                    val freef = arg6Store (arg6Addr, f)
                    val freeg = arg7Store (arg7Addr, g)
                    val freeh = arg8Store (arg8Addr, h)
                    fun freeAll() =
                        (freea(); freeb(); freec(); freed(); freee(); freef(); freeg();
                         freeh(); free rMem)
                in
                    let
                        val () = cf([arg1Addr, arg2Addr, arg3Addr, arg4Addr, arg5Addr, arg6Addr, arg7Addr, arg8Addr], rMem)
                        val result = resLoad rMem
                    in
                        arg1Update(arg1Addr, a); arg2Update (arg2Addr, b); arg3Update (arg3Addr, c);
                        arg4Update (arg4Addr, d); arg5Update (arg5Addr, e); arg6Update (arg6Addr, f);
                        arg7Update (arg7Addr, g); arg8Update (arg8Addr, h);
                        freeAll();
                        result
                    end handle exn => (freeAll(); raise exn)
                end
            end
        end

        fun call8withAbi abi symbol argTypes resType = callInternal8withAbi abi argTypes resType symbol
        fun call8 x = call8withAbi abiDefault x

        fun callInternal9withAbi (abi: abi)
            ({ ctype = arg1Type, store = arg1Store, update = arg1Update, ...}: 'a conversion,
             { ctype = arg2Type, store = arg2Store, update = arg2Update, ...}: 'b conversion,
             { ctype = arg3Type, store = arg3Store, update = arg3Update, ...}: 'c conversion,
             { ctype = arg4Type, store = arg4Store, update = arg4Update, ...}: 'd conversion,
             { ctype = arg5Type, store = arg5Store, update = arg5Update, ...}: 'e conversion,             
             { ctype = arg6Type, store = arg6Store, update = arg6Update, ...}: 'f conversion,             
             { ctype = arg7Type, store = arg7Store, update = arg7Update, ...}: 'g conversion,             
             { ctype = arg8Type, store = arg8Store, update = arg8Update, ...}: 'h conversion,             
             { ctype = arg9Type, store = arg9Store, update = arg9Update, ...}: 'i conversion)             
            ({ ctype = resType, load= resLoad, ...}: 'j conversion):
                symbol -> 'a * 'b *'c * 'd * 'e * 'f * 'g * 'h * 'i -> 'j =
        let
            val callF =
                callwithAbi abi
                    [arg1Type, arg2Type, arg3Type, arg4Type, arg5Type, arg6Type, arg7Type, arg8Type, arg9Type] resType
        in
            fn fnAddr =>
            let
                val cf = callF fnAddr
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
                    val freea = arg1Store (arg1Addr, a)
                    val freeb = arg2Store (arg2Addr, b)
                    val freec = arg3Store (arg3Addr, c)
                    val freed = arg4Store (arg4Addr, d)
                    val freee = arg5Store (arg5Addr, e)
                    val freef = arg6Store (arg6Addr, f)
                    val freeg = arg7Store (arg7Addr, g)
                    val freeh = arg8Store (arg8Addr, h)
                    val freei = arg9Store (arg9Addr, i)
                    fun freeAll() =
                        (freea(); freeb(); freec(); freed(); freee(); freef(); freeg();
                         freeh(); freei(); free rMem)
                in
                    let
                        val () =
                            cf([arg1Addr, arg2Addr, arg3Addr, arg4Addr, arg5Addr, arg6Addr, arg7Addr, arg8Addr, arg9Addr], rMem)
                        val result = resLoad rMem
                    in
                        arg1Update(arg1Addr, a); arg2Update (arg2Addr, b); arg3Update (arg3Addr, c);
                        arg4Update (arg4Addr, d); arg5Update (arg5Addr, e); arg6Update (arg6Addr, f);
                        arg7Update (arg7Addr, g); arg8Update (arg8Addr, h); arg9Update (arg9Addr, i);
                        freeAll();
                        result
                    end handle exn => (freeAll(); raise exn)
                end
            end
        end

        fun call9withAbi abi symbol argTypes resType = callInternal9withAbi abi argTypes resType symbol
        fun call9 x = call9withAbi abiDefault x

        fun callInternal10withAbi (abi: abi)
            ({ ctype = arg1Type, store = arg1Store, update = arg1Update, ...}: 'a conversion,
             { ctype = arg2Type, store = arg2Store, update = arg2Update, ...}: 'b conversion,
             { ctype = arg3Type, store = arg3Store, update = arg3Update, ...}: 'c conversion,
             { ctype = arg4Type, store = arg4Store, update = arg4Update, ...}: 'd conversion,
             { ctype = arg5Type, store = arg5Store, update = arg5Update, ...}: 'e conversion,             
             { ctype = arg6Type, store = arg6Store, update = arg6Update, ...}: 'f conversion,             
             { ctype = arg7Type, store = arg7Store, update = arg7Update, ...}: 'g conversion,             
             { ctype = arg8Type, store = arg8Store, update = arg8Update, ...}: 'h conversion,             
             { ctype = arg9Type, store = arg9Store, update = arg9Update, ...}: 'i conversion,             
             { ctype = arg10Type, store = arg10Store, update = arg10Update, ...}: 'j conversion)             
            ({ ctype = resType, load= resLoad, ...}: 'k conversion):
                symbol -> 'a * 'b *'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j -> 'k =
        let
            val callF =
                callwithAbi abi
                    [arg1Type, arg2Type, arg3Type, arg4Type, arg5Type, arg6Type, arg7Type,
                     arg8Type, arg9Type, arg10Type] resType
        in
            fn fnAddr =>
            let
                val cf = callF fnAddr
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
                    val freea = arg1Store (arg1Addr, a)
                    val freeb = arg2Store (arg2Addr, b)
                    val freec = arg3Store (arg3Addr, c)
                    val freed = arg4Store (arg4Addr, d)
                    val freee = arg5Store (arg5Addr, e)
                    val freef = arg6Store (arg6Addr, f)
                    val freeg = arg7Store (arg7Addr, g)
                    val freeh = arg8Store (arg8Addr, h)
                    val freei = arg9Store (arg9Addr, i)
                    val freej = arg10Store (arg10Addr, j)
                    fun freeAll() =
                        (freea(); freeb(); freec(); freed(); freee(); freef(); freeg();
                         freeh(); freei(); freej(); free rMem)
                in
                    let
                        val () =
                            cf([arg1Addr, arg2Addr, arg3Addr, arg4Addr, arg5Addr, arg6Addr, arg7Addr,
                                   arg8Addr, arg9Addr, arg10Addr], rMem)
                        val result = resLoad rMem
                    in
                        arg1Update(arg1Addr, a); arg2Update (arg2Addr, b); arg3Update (arg3Addr, c);
                        arg4Update (arg4Addr, d); arg5Update (arg5Addr, e); arg6Update (arg6Addr, f);
                        arg7Update (arg7Addr, g); arg8Update (arg8Addr, h); arg9Update (arg9Addr, i);
                        arg10Update (arg10Addr, j);
                        freeAll();
                        result
                    end handle exn => (freeAll(); raise exn)
                end
            end
        end

        fun call10withAbi abi symbol argTypes resType = callInternal10withAbi abi argTypes resType symbol
        fun call10 x = call10withAbi abiDefault x

        fun callInternal11withAbi (abi: abi)
            ({ ctype = arg1Type, store = arg1Store, update = arg1Update, ...}: 'a conversion,
             { ctype = arg2Type, store = arg2Store, update = arg2Update, ...}: 'b conversion,
             { ctype = arg3Type, store = arg3Store, update = arg3Update, ...}: 'c conversion,
             { ctype = arg4Type, store = arg4Store, update = arg4Update, ...}: 'd conversion,
             { ctype = arg5Type, store = arg5Store, update = arg5Update, ...}: 'e conversion,             
             { ctype = arg6Type, store = arg6Store, update = arg6Update, ...}: 'f conversion,             
             { ctype = arg7Type, store = arg7Store, update = arg7Update, ...}: 'g conversion,             
             { ctype = arg8Type, store = arg8Store, update = arg8Update, ...}: 'h conversion,             
             { ctype = arg9Type, store = arg9Store, update = arg9Update, ...}: 'i conversion,             
             { ctype = arg10Type, store = arg10Store, update = arg10Update, ...}: 'j conversion,             
             { ctype = arg11Type, store = arg11Store, update = arg11Update, ...}: 'k conversion)             
            ({ ctype = resType, load= resLoad, ...}: 'l conversion):
                symbol -> 'a * 'b *'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k -> 'l =
        let
            val callF =
                callwithAbi abi
                    [arg1Type, arg2Type, arg3Type, arg4Type, arg5Type, arg6Type, arg7Type,
                     arg8Type, arg9Type, arg10Type, arg11Type] resType
        in
            fn fnAddr =>
            let
                val cf = callF fnAddr
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
                    val freea = arg1Store (arg1Addr, a)
                    val freeb = arg2Store (arg2Addr, b)
                    val freec = arg3Store (arg3Addr, c)
                    val freed = arg4Store (arg4Addr, d)
                    val freee = arg5Store (arg5Addr, e)
                    val freef = arg6Store (arg6Addr, f)
                    val freeg = arg7Store (arg7Addr, g)
                    val freeh = arg8Store (arg8Addr, h)
                    val freei = arg9Store (arg9Addr, i)
                    val freej = arg10Store (arg10Addr, j)
                    val freek = arg11Store (arg11Addr, k)
                    fun freeAll() =
                        (freea(); freeb(); freec(); freed(); freee(); freef(); freeg();
                         freeh(); freei(); freej(); freek(); free rMem)
                in
                    let
                        val () =
                            cf([arg1Addr, arg2Addr, arg3Addr, arg4Addr, arg5Addr, arg6Addr, arg7Addr,
                                   arg8Addr, arg9Addr, arg10Addr, arg11Addr], rMem)
                        val result = resLoad rMem
                    in
                        arg1Update(arg1Addr, a); arg2Update (arg2Addr, b); arg3Update (arg3Addr, c);
                        arg4Update (arg4Addr, d); arg5Update (arg5Addr, e); arg6Update (arg6Addr, f);
                        arg7Update (arg7Addr, g); arg8Update (arg8Addr, h); arg9Update (arg9Addr, i);
                        arg10Update (arg10Addr, j); arg11Update (arg11Addr, k);
                        freeAll();
                        result
                    end handle exn => (freeAll(); raise exn)
                end
            end
        end

        fun call11withAbi abi symbol argTypes resType = callInternal11withAbi abi argTypes resType symbol
        fun call11 x = call11withAbi abiDefault x

        fun callInternal12withAbi (abi: abi)
            ({ ctype = arg1Type, store = arg1Store, update = arg1Update, ...}: 'a conversion,
             { ctype = arg2Type, store = arg2Store, update = arg2Update, ...}: 'b conversion,
             { ctype = arg3Type, store = arg3Store, update = arg3Update, ...}: 'c conversion,
             { ctype = arg4Type, store = arg4Store, update = arg4Update, ...}: 'd conversion,
             { ctype = arg5Type, store = arg5Store, update = arg5Update, ...}: 'e conversion,             
             { ctype = arg6Type, store = arg6Store, update = arg6Update, ...}: 'f conversion,             
             { ctype = arg7Type, store = arg7Store, update = arg7Update, ...}: 'g conversion,             
             { ctype = arg8Type, store = arg8Store, update = arg8Update, ...}: 'h conversion,             
             { ctype = arg9Type, store = arg9Store, update = arg9Update, ...}: 'i conversion,             
             { ctype = arg10Type, store = arg10Store, update = arg10Update, ...}: 'j conversion,             
             { ctype = arg11Type, store = arg11Store, update = arg11Update, ...}: 'k conversion,             
             { ctype = arg12Type, store = arg12Store, update = arg12Update, ...}: 'l conversion)             
            ({ ctype = resType, load= resLoad, ...}: 'm conversion):
                symbol ->
                    'a * 'b *'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l -> 'm =
        let
            val callF =
                callwithAbi abi
                    [arg1Type, arg2Type, arg3Type, arg4Type, arg5Type, arg6Type, arg7Type,
                     arg8Type, arg9Type, arg10Type, arg11Type, arg12Type] resType
        in
            fn fnAddr =>
            let
                val cf = callF fnAddr
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
                    val freea = arg1Store (arg1Addr, a)
                    val freeb = arg2Store (arg2Addr, b)
                    val freec = arg3Store (arg3Addr, c)
                    val freed = arg4Store (arg4Addr, d)
                    val freee = arg5Store (arg5Addr, e)
                    val freef = arg6Store (arg6Addr, f)
                    val freeg = arg7Store (arg7Addr, g)
                    val freeh = arg8Store (arg8Addr, h)
                    val freei = arg9Store (arg9Addr, i)
                    val freej = arg10Store (arg10Addr, j)
                    val freek = arg11Store (arg11Addr, k)
                    val freel = arg12Store (arg12Addr, l)
                    fun freeAll() =
                        (freea(); freeb(); freec(); freed(); freee(); freef(); freeg();
                         freeh(); freei(); freej(); freek(); freel(); free rMem)
                in
                    let
                        val () =
                            cf([arg1Addr, arg2Addr, arg3Addr, arg4Addr, arg5Addr, arg6Addr, arg7Addr,
                                   arg8Addr, arg9Addr, arg10Addr, arg11Addr, arg12Addr], rMem)
                        val result = resLoad rMem
                    in
                        arg1Update(arg1Addr, a); arg2Update (arg2Addr, b); arg3Update (arg3Addr, c);
                        arg4Update (arg4Addr, d); arg5Update (arg5Addr, e); arg6Update (arg6Addr, f);
                        arg7Update (arg7Addr, g); arg8Update (arg8Addr, h); arg9Update (arg9Addr, i);
                        arg10Update (arg10Addr, j); arg11Update (arg11Addr, k); arg12Update (arg12Addr, l);
                        freeAll();
                        result
                    end handle exn => (freeAll(); raise exn)
                end
            end
        end

        fun call12withAbi abi symbol argTypes resType = callInternal12withAbi abi argTypes resType symbol
        fun call12 x = call12withAbi abiDefault x

        fun callInternal13withAbi (abi: abi)
            ({ ctype = arg1Type, store = arg1Store, update = arg1Update, ...}: 'a conversion,
             { ctype = arg2Type, store = arg2Store, update = arg2Update, ...}: 'b conversion,
             { ctype = arg3Type, store = arg3Store, update = arg3Update, ...}: 'c conversion,
             { ctype = arg4Type, store = arg4Store, update = arg4Update, ...}: 'd conversion,
             { ctype = arg5Type, store = arg5Store, update = arg5Update, ...}: 'e conversion,             
             { ctype = arg6Type, store = arg6Store, update = arg6Update, ...}: 'f conversion,             
             { ctype = arg7Type, store = arg7Store, update = arg7Update, ...}: 'g conversion,             
             { ctype = arg8Type, store = arg8Store, update = arg8Update, ...}: 'h conversion,             
             { ctype = arg9Type, store = arg9Store, update = arg9Update, ...}: 'i conversion,             
             { ctype = arg10Type, store = arg10Store, update = arg10Update, ...}: 'j conversion,             
             { ctype = arg11Type, store = arg11Store, update = arg11Update, ...}: 'k conversion,             
             { ctype = arg12Type, store = arg12Store, update = arg12Update, ...}: 'l conversion,             
             { ctype = arg13Type, store = arg13Store, update = arg13Update, ...}: 'm conversion)             
            ({ ctype = resType, load= resLoad, ...}: 'n conversion):
                symbol ->
                    'a * 'b *'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm -> 'n =
        let
            val callF =
                callwithAbi abi
                    [arg1Type, arg2Type, arg3Type, arg4Type, arg5Type, arg6Type, arg7Type,
                     arg8Type, arg9Type, arg10Type, arg11Type, arg12Type, arg13Type] resType
        in
            fn fnAddr =>
            let
                val cf = callF fnAddr
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
                    val freea = arg1Store (arg1Addr, a)
                    val freeb = arg2Store (arg2Addr, b)
                    val freec = arg3Store (arg3Addr, c)
                    val freed = arg4Store (arg4Addr, d)
                    val freee = arg5Store (arg5Addr, e)
                    val freef = arg6Store (arg6Addr, f)
                    val freeg = arg7Store (arg7Addr, g)
                    val freeh = arg8Store (arg8Addr, h)
                    val freei = arg9Store (arg9Addr, i)
                    val freej = arg10Store (arg10Addr, j)
                    val freek = arg11Store (arg11Addr, k)
                    val freel = arg12Store (arg12Addr, l)
                    val freem = arg13Store (arg13Addr, m)
                    fun freeAll() =
                        (freea(); freeb(); freec(); freed(); freee(); freef(); freeg();
                         freeh(); freei(); freej(); freek(); freel(); freem(); free rMem)
                in
                    let
                        val () =
                            cf([arg1Addr, arg2Addr, arg3Addr, arg4Addr, arg5Addr, arg6Addr, arg7Addr,
                                   arg8Addr, arg9Addr, arg10Addr, arg11Addr, arg12Addr, arg13Addr], rMem)
                        val result = resLoad rMem
                    in
                        arg1Update(arg1Addr, a); arg2Update (arg2Addr, b); arg3Update (arg3Addr, c);
                        arg4Update (arg4Addr, d); arg5Update (arg5Addr, e); arg6Update (arg6Addr, f);
                        arg7Update (arg7Addr, g); arg8Update (arg8Addr, h); arg9Update (arg9Addr, i);
                        arg10Update (arg10Addr, j); arg11Update (arg11Addr, k); arg12Update (arg12Addr, l);
                        arg13Update (arg13Addr, m);
                        freeAll();
                        result
                    end handle exn => (freeAll(); raise exn)
                end
            end
        end

        fun call13withAbi abi symbol argTypes resType = callInternal13withAbi abi argTypes resType symbol
        fun call13 x = call13withAbi abiDefault x

        fun callInternal14withAbi (abi: abi)
            ({ ctype = arg1Type, store = arg1Store, update = arg1Update, ...}: 'a conversion,
             { ctype = arg2Type, store = arg2Store, update = arg2Update, ...}: 'b conversion,
             { ctype = arg3Type, store = arg3Store, update = arg3Update, ...}: 'c conversion,
             { ctype = arg4Type, store = arg4Store, update = arg4Update, ...}: 'd conversion,
             { ctype = arg5Type, store = arg5Store, update = arg5Update, ...}: 'e conversion,
             { ctype = arg6Type, store = arg6Store, update = arg6Update, ...}: 'f conversion,
             { ctype = arg7Type, store = arg7Store, update = arg7Update, ...}: 'g conversion,
             { ctype = arg8Type, store = arg8Store, update = arg8Update, ...}: 'h conversion,
             { ctype = arg9Type, store = arg9Store, update = arg9Update, ...}: 'i conversion,
             { ctype = arg10Type, store = arg10Store, update = arg10Update, ...}: 'j conversion,
             { ctype = arg11Type, store = arg11Store, update = arg11Update, ...}: 'k conversion,
             { ctype = arg12Type, store = arg12Store, update = arg12Update, ...}: 'l conversion,
             { ctype = arg13Type, store = arg13Store, update = arg13Update, ...}: 'm conversion,           
             { ctype = arg14Type, store = arg14Store, update = arg14Update, ...}: 'n conversion)             
            ({ ctype = resType, load= resLoad, ...}: 'o conversion):
                symbol ->
                    'a * 'b *'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm * 'n -> 'o =
        let
            val callF =
                callwithAbi abi
                    [arg1Type, arg2Type, arg3Type, arg4Type, arg5Type, arg6Type, arg7Type,
                     arg8Type, arg9Type, arg10Type, arg11Type, arg12Type, arg13Type,
                     arg14Type] resType
        in
            fn fnAddr =>
            let
                val cf = callF fnAddr
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
                    val freea = arg1Store (arg1Addr, a)
                    val freeb = arg2Store (arg2Addr, b)
                    val freec = arg3Store (arg3Addr, c)
                    val freed = arg4Store (arg4Addr, d)
                    val freee = arg5Store (arg5Addr, e)
                    val freef = arg6Store (arg6Addr, f)
                    val freeg = arg7Store (arg7Addr, g)
                    val freeh = arg8Store (arg8Addr, h)
                    val freei = arg9Store (arg9Addr, i)
                    val freej = arg10Store (arg10Addr, j)
                    val freek = arg11Store (arg11Addr, k)
                    val freel = arg12Store (arg12Addr, l)
                    val freem = arg13Store (arg13Addr, m)
                    val freen = arg14Store (arg14Addr, n)
                    fun freeAll() =
                        (freea(); freeb(); freec(); freed(); freee(); freef(); freeg();
                         freeh(); freei(); freej(); freek(); freel(); freem(); freen(); free rMem)
                in
                    let
                        val () =
                            cf([arg1Addr, arg2Addr, arg3Addr, arg4Addr, arg5Addr, arg6Addr, arg7Addr,
                                   arg8Addr, arg9Addr, arg10Addr, arg11Addr, arg12Addr, arg13Addr, arg14Addr], rMem)
                        val result = resLoad rMem
                    in
                        arg1Update(arg1Addr, a); arg2Update (arg2Addr, b); arg3Update (arg3Addr, c);
                        arg4Update (arg4Addr, d); arg5Update (arg5Addr, e); arg6Update (arg6Addr, f);
                        arg7Update (arg7Addr, g); arg8Update (arg8Addr, h); arg9Update (arg9Addr, i);
                        arg10Update (arg10Addr, j); arg11Update (arg11Addr, k); arg12Update (arg12Addr, l);
                        arg13Update (arg13Addr, m); arg14Update (arg14Addr, n); 
                        freeAll();
                        result
                    end handle exn => (freeAll(); raise exn)
                end
            end
        end

        fun call14withAbi abi symbol argTypes resType = callInternal14withAbi abi argTypes resType symbol
        fun call14 x = call14withAbi abiDefault x

    end

    local
        open LibFFI Memory LowLevel
    in
        (* Callback conversion *)
        fun cFunction0withAbi (abi: abi) () (resConv: 'a conversion) : (unit->'a) conversion =
        let
            fun callback (f: unit -> 'a) (_: voidStar, res: voidStar): unit =
                ignore(#store resConv (res, f ()))
            (* Ignore the result of #store resConv.  What this means is if the
               callback returns something, e.g. a string, that requires
               dynamic allocation there will be a memory leak. *)

            val makeCallback = cFunctionWithAbi abi [] (#ctype resConv)

            (* Really make the callback when we store the actual function. *)
            fun store (v: voidStar, f) =
            let
                val cb = makeCallback(callback f)
            in
                setAddress(v, 0w0, cb);
                fn () => freeCallback cb
            end
            
            (* If we return a function as a result we need to wrap it as an ML call. *)
            val call = callInternal0withAbi abi () resConv
            
            fun load(v: voidStar): unit->'a =
                let val f = getAddress(v, 0w0) in call (fn () => f) end
        in
            makeConversion { load=load, store=store, ctype=cTypePointer }
        end
   
        fun cFunction0 x = cFunction0withAbi abiDefault x

        fun cFunction1withAbi (abi: abi)
                (argConv: 'a conversion) (resConv: 'b conversion) : ('a -> 'b) conversion =
        let
            fun callback (f: 'a -> 'b) (args: voidStar, res: voidStar): unit =
            let
                val result = f (#load argConv (getAddress(args, 0w0)))
            in
                ignore(#store resConv (res, result))
            end

            val makeCallback = cFunctionWithAbi abi [#ctype argConv] (#ctype resConv)

            (* Really make the callback when we store the actual function. *)
            fun store (v: voidStar, f) =
            let
                val cb = makeCallback(callback f)
            in
                setAddress(v, 0w0, cb);
                fn () => freeCallback cb
            end

            val call = callInternal1withAbi abi argConv resConv
            
            fun load(v: voidStar): 'a -> 'b =
                let val f = getAddress(v, 0w0) in call (fn () => f) end
        in
            makeConversion { load=load, store=store, ctype=cTypePointer }
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
                    ignore(#store resConv (res, result))
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
                fun store (v: voidStar, f) =
                    let val c = cb f in setAddress(v, 0w0, c); fn () => freeCallback c end

                val call = callInternal2withAbi abi (arg1Conv, arg2Conv) resConv
            
                fun load(v: voidStar): 'a *'b -> 'c =
                    let val f = getAddress(v, 0w0) in call (fn () => f) end
            in
                makeConversion{ load=load, store=store, ctype=cTypePointer }
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
                    ignore(#store resConv (res, result))
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
                fun store (v: voidStar, f) =
                    let val c = cb f in setAddress(v, 0w0, c); fn () => freeCallback c end

                val call = callInternal3withAbi abi (arg1Conv, arg2Conv, arg3Conv) resConv
            
                fun load(v: voidStar): 'a *'b * 'c -> 'd =
                    let val f = getAddress(v, 0w0) in call (fn () => f) end
            in
                makeConversion { load=load, store=store, ctype=cTypePointer }
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
                    ignore(#store resConv (res, result))
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
                fun store (v: voidStar, f) =
                    let val c = cb f in setAddress(v, 0w0, c); fn () => freeCallback c end

                val call = callInternal4withAbi abi (arg1Conv, arg2Conv, arg3Conv, arg4Conv) resConv
            
                fun load(v: voidStar): 'a *'b * 'c * 'd -> 'e =
                    let val f = getAddress(v, 0w0) in call (fn () => f) end
            in
                makeConversion { load=load, store=store, ctype=cTypePointer }
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
                    ignore(#store resConv (res, result))
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
                fun store (v: voidStar, f) =
                    let val c = cb f in setAddress(v, 0w0, c); fn () => freeCallback c end

                val call =
                    callInternal5withAbi abi (arg1Conv, arg2Conv, arg3Conv, arg4Conv, arg5Conv) resConv
            
                fun load(v: voidStar): 'a *'b * 'c * 'd * 'e -> 'f =
                    let val f = getAddress(v, 0w0) in call (fn () => f) end
            in
                makeConversion { load=load, store=store, ctype=cTypePointer }
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
                    ignore(#store resConv (res, result))
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
                fun store (v: voidStar, f) =
                    let val c = cb f in setAddress(v, 0w0, c); fn () => freeCallback c end

                val call =
                    callInternal6withAbi abi (arg1Conv, arg2Conv, arg3Conv, arg4Conv, arg5Conv, arg6Conv) resConv
            
                fun load(v: voidStar): 'a *'b * 'c * 'd * 'e * 'f -> 'g =
                    let val f = getAddress(v, 0w0) in call (fn () => f) end
            in
                makeConversion { load=load, store=store, ctype=cTypePointer }
            end
       
            fun cFunction6 x = cFunction6withAbi abiDefault x
        end

    end
end;
