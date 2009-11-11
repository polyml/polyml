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

structure LocaleBase =
struct
    local
        open CInterface Base
    in
        datatype PrimaryLanguage =
            LANG_AFRIKAANS | LANG_ICELANDIC | LANG_ALBANIAN | LANG_INDONESIAN 
          | LANG_ARABIC | LANG_ITALIAN  | LANG_BASQUE | LANG_JAPANESE 
          | LANG_BELARUSIAN | LANG_KOREAN | LANG_BULGARIAN | LANG_LATVIAN 
          | LANG_CATALAN | LANG_LITHUANIAN | LANG_CHINESE | LANG_NEUTRAL 
          | LANG_CROATIAN | LANG_NORWEGIAN | LANG_CZECH | LANG_POLISH 
          | LANG_DANISH | LANG_PORTUGUESE  | LANG_DUTCH | LANG_ROMANIAN 
          | LANG_ENGLISH | LANG_RUSSIAN | LANG_ESTONIAN | LANG_SERBIAN 
          | LANG_FAEROESE | LANG_SLOVAK | LANG_FARSI | LANG_SLOVENIAN 
          | LANG_FINNISH | LANG_SPANISH | LANG_FRENCH | LANG_SWEDISH 
          | LANG_GERMAN | LANG_THAI | LANG_GREEK | LANG_TURKISH 
          | LANG_HEBREW | LANG_UKRANIAN | LANG_HUNGARIAN | LANG_VIETNAMESE

        datatype SubLanguage =
            SUBLANG_ARABIC_SAUDI_ARABIA | SUBLANG_GERMAN 
         | SUBLANG_ARABIC_IRAQ | SUBLANG_GERMAN_SWISS 
         | SUBLANG_ARABIC_EGYPT | SUBLANG_GERMAN_AUSTRIAN 
         | SUBLANG_ARABIC_LIBYA | SUBLANG_GERMAN_LUXEMBOURG 
         | SUBLANG_ARABIC_ALGERIA | SUBLANG_GERMAN_LIECHTENSTEIN 
         | SUBLANG_ARABIC_MOROCCO | SUBLANG_ITALIAN 
         | SUBLANG_ARABIC_TUNISIA | SUBLANG_ITALIAN_SWISS 
         | SUBLANG_ARABIC_OMAN | SUBLANG_KOREAN 
         | SUBLANG_ARABIC_YEMEN | SUBLANG_KOREAN_JOHAB 
         | SUBLANG_ARABIC_SYRIA | SUBLANG_NEUTRAL 
         | SUBLANG_ARABIC_JORDAN | SUBLANG_NORWEGIAN_BOKMAL 
         | SUBLANG_ARABIC_LEBANON | SUBLANG_NORWEGIAN_NYNORSK 
         | SUBLANG_ARABIC_KUWAIT | SUBLANG_PORTUGUESE 
         | SUBLANG_ARABIC_UAE | SUBLANG_PORTUGUESE_BRAZILIAN 
         | SUBLANG_ARABIC_BAHRAIN | SUBLANG_SERBIAN_LATIN 
         | SUBLANG_ARABIC_QATAR | SUBLANG_SERBIAN_CYRILLIC 
         | SUBLANG_CHINESE_TRADITIONAL | SUBLANG_SPANISH 
         | SUBLANG_CHINESE_SIMPLIFIED | SUBLANG_SPANISH_MEXICAN 
         | SUBLANG_CHINESE_HONGKONG | SUBLANG_SPANISH_MODERN 
         | SUBLANG_CHINESE_SINGAPORE | SUBLANG_SPANISH_GUATEMALA 
         | SUBLANG_DEFAULT | SUBLANG_SPANISH_COSTA_RICA 
         | SUBLANG_DUTCH | SUBLANG_SPANISH_PANAMA 
         | SUBLANG_DUTCH_BELGIAN | SUBLANG_SPANISH_DOMINICAN_REPUBLIC 
         | SUBLANG_ENGLISH_US | SUBLANG_SPANISH_VENEZUELA 
         | SUBLANG_ENGLISH_UK | SUBLANG_SPANISH_COLOMBIA 
         | SUBLANG_ENGLISH_AUS | SUBLANG_SPANISH_PERU 
         | SUBLANG_ENGLISH_CAN | SUBLANG_SPANISH_ARGENTINA 
         | SUBLANG_ENGLISH_NZ | SUBLANG_SPANISH_ECUADOR 
         | SUBLANG_ENGLISH_EIRE | SUBLANG_SPANISH_CHILE 
         | SUBLANG_ENGLISH_SOUTH_AFRICA | SUBLANG_SPANISH_URUGUAY 
         | SUBLANG_ENGLISH_JAMAICA | SUBLANG_SPANISH_PARAGUAY 
         | SUBLANG_ENGLISH_CARIBBEAN | SUBLANG_SPANISH_BOLIVIA 
         | SUBLANG_ENGLISH_BELIZE | SUBLANG_SPANISH_EL_SALVADOR 
         | SUBLANG_ENGLISH_TRINIDAD | SUBLANG_SPANISH_HONDURAS 
         | SUBLANG_FRENCH | SUBLANG_SPANISH_NICARAGUA 
         | SUBLANG_FRENCH_BELGIAN | SUBLANG_SPANISH_PUERTO_RICO 
         | SUBLANG_FRENCH_CANADIAN | SUBLANG_SWEDISH 
         | SUBLANG_FRENCH_SWISS | SUBLANG_SWEDISH_FINLAND 
         | SUBLANG_FRENCH_LUXEMBOURG | SUBLANG_SYS_DEFAULT 

        local
            val tab = [
            (LANG_NEUTRAL, 0x00),
            (LANG_AFRIKAANS, 0x36),
            (LANG_ALBANIAN, 0x1c),
            (LANG_ARABIC, 0x01),
            (LANG_BASQUE, 0x2d),
            (LANG_BELARUSIAN, 0x23),
            (LANG_BULGARIAN, 0x02),
            (LANG_CATALAN, 0x03),
            (LANG_CHINESE, 0x04),
            (LANG_CROATIAN, 0x1a),
            (LANG_CZECH, 0x05),
            (LANG_DANISH, 0x06),
            (LANG_DUTCH, 0x13),
            (LANG_ENGLISH, 0x09),
            (LANG_ESTONIAN, 0x25),
            (LANG_FAEROESE, 0x38),
            (LANG_FARSI, 0x29),
            (LANG_FINNISH, 0x0b),
            (LANG_FRENCH, 0x0c),
            (LANG_GERMAN, 0x07),
            (LANG_GREEK, 0x08),
            (LANG_HEBREW, 0x0d),
            (LANG_HUNGARIAN, 0x0e),
            (LANG_ICELANDIC, 0x0f),
            (LANG_INDONESIAN, 0x21),
            (LANG_ITALIAN, 0x10),
            (LANG_JAPANESE, 0x11),
            (LANG_KOREAN, 0x12),
            (LANG_LATVIAN, 0x26),
            (LANG_LITHUANIAN, 0x27),
            (LANG_NORWEGIAN, 0x14),
            (LANG_POLISH, 0x15),
            (LANG_PORTUGUESE, 0x16),
            (LANG_ROMANIAN, 0x18),
            (LANG_RUSSIAN, 0x19),
            (LANG_SERBIAN, 0x1a),
            (LANG_SLOVAK, 0x1b),
            (LANG_SLOVENIAN, 0x24),
            (LANG_SPANISH, 0x0a),
            (LANG_SWEDISH, 0x1d),
            (LANG_THAI, 0x1e),
            (LANG_TURKISH, 0x1f),
            (LANG_UKRANIAN, 0x22),
            (LANG_VIETNAMESE, 0x2a)]
        in
            val (fromPrim, toPrim) = tableLookup(tab, NONE)
        end

        local
            val tab = [
            (SUBLANG_NEUTRAL, 0x00),
            (SUBLANG_DEFAULT, 0x01),
            (SUBLANG_SYS_DEFAULT, 0x02),
            (SUBLANG_ARABIC_SAUDI_ARABIA, 0x01),
            (SUBLANG_ARABIC_IRAQ, 0x02),
            (SUBLANG_ARABIC_EGYPT, 0x03),
            (SUBLANG_ARABIC_LIBYA, 0x04),
            (SUBLANG_ARABIC_ALGERIA, 0x05),
            (SUBLANG_ARABIC_MOROCCO, 0x06),
            (SUBLANG_ARABIC_TUNISIA, 0x07),
            (SUBLANG_ARABIC_OMAN, 0x08),
            (SUBLANG_ARABIC_YEMEN, 0x09),
            (SUBLANG_ARABIC_SYRIA, 0x0a),
            (SUBLANG_ARABIC_JORDAN, 0x0b),
            (SUBLANG_ARABIC_LEBANON, 0x0c),
            (SUBLANG_ARABIC_KUWAIT, 0x0d),
            (SUBLANG_ARABIC_UAE, 0x0e),
            (SUBLANG_ARABIC_BAHRAIN, 0x0f),
            (SUBLANG_ARABIC_QATAR, 0x10),
            (SUBLANG_CHINESE_TRADITIONAL, 0x01),
            (SUBLANG_CHINESE_SIMPLIFIED, 0x02),
            (SUBLANG_CHINESE_HONGKONG, 0x03),
            (SUBLANG_CHINESE_SINGAPORE, 0x04),
            (SUBLANG_DUTCH, 0x01),
            (SUBLANG_DUTCH_BELGIAN, 0x02),
            (SUBLANG_ENGLISH_US, 0x01),
            (SUBLANG_ENGLISH_UK, 0x02),
            (SUBLANG_ENGLISH_AUS, 0x03),
            (SUBLANG_ENGLISH_CAN, 0x04),
            (SUBLANG_ENGLISH_NZ, 0x05),
            (SUBLANG_ENGLISH_EIRE, 0x06),
            (SUBLANG_ENGLISH_SOUTH_AFRICA, 0x07),
            (SUBLANG_ENGLISH_JAMAICA, 0x08),
            (SUBLANG_ENGLISH_CARIBBEAN, 0x09),
            (SUBLANG_ENGLISH_BELIZE, 0x0a),
            (SUBLANG_ENGLISH_TRINIDAD, 0x0b),
            (SUBLANG_FRENCH, 0x01),
            (SUBLANG_FRENCH_BELGIAN, 0x02),
            (SUBLANG_FRENCH_CANADIAN, 0x03),
            (SUBLANG_FRENCH_SWISS, 0x04),
            (SUBLANG_FRENCH_LUXEMBOURG, 0x05),
            (SUBLANG_GERMAN, 0x01),
            (SUBLANG_GERMAN_SWISS, 0x02),
            (SUBLANG_GERMAN_AUSTRIAN, 0x03),
            (SUBLANG_GERMAN_LUXEMBOURG, 0x04),
            (SUBLANG_GERMAN_LIECHTENSTEIN, 0x05),
            (SUBLANG_ITALIAN, 0x01),
            (SUBLANG_ITALIAN_SWISS, 0x02),
            (SUBLANG_KOREAN, 0x01),
            (SUBLANG_KOREAN_JOHAB, 0x02),
            (SUBLANG_NORWEGIAN_BOKMAL, 0x01),
            (SUBLANG_NORWEGIAN_NYNORSK, 0x02),
            (SUBLANG_PORTUGUESE, 0x02),
            (SUBLANG_PORTUGUESE_BRAZILIAN, 0x01),
            (SUBLANG_SERBIAN_LATIN, 0x02),
            (SUBLANG_SERBIAN_CYRILLIC, 0x03),
            (SUBLANG_SPANISH, 0x01),
            (SUBLANG_SPANISH_MEXICAN, 0x02),
            (SUBLANG_SPANISH_MODERN, 0x03),
            (SUBLANG_SPANISH_GUATEMALA, 0x04),
            (SUBLANG_SPANISH_COSTA_RICA, 0x05),
            (SUBLANG_SPANISH_PANAMA, 0x06),
            (SUBLANG_SPANISH_DOMINICAN_REPUBLIC, 0x07),
            (SUBLANG_SPANISH_VENEZUELA, 0x08),
            (SUBLANG_SPANISH_COLOMBIA, 0x09),
            (SUBLANG_SPANISH_PERU, 0x0a),
            (SUBLANG_SPANISH_ARGENTINA, 0x0b),
            (SUBLANG_SPANISH_ECUADOR, 0x0c),
            (SUBLANG_SPANISH_CHILE, 0x0d),
            (SUBLANG_SPANISH_URUGUAY, 0x0e),
            (SUBLANG_SPANISH_PARAGUAY, 0x0f),
            (SUBLANG_SPANISH_BOLIVIA, 0x10),
            (SUBLANG_SPANISH_EL_SALVADOR, 0x11),
            (SUBLANG_SPANISH_HONDURAS, 0x12),
            (SUBLANG_SPANISH_NICARAGUA, 0x13),
            (SUBLANG_SPANISH_PUERTO_RICO, 0x14),
            (SUBLANG_SWEDISH, 0x01),
            (SUBLANG_SWEDISH_FINLAND, 0x02)]
        in
            val (fromSub, toSub) = tableLookup(tab, NONE)
        end

        datatype LANGID = MAKELANGID of PrimaryLanguage * SubLanguage

        local
            fun fromLANGID(MAKELANGID(prim, sub)) =
                IntInf.orb(fromPrim prim, IntInf.<<(fromSub sub, 0w10))
            (* It seems that GetUserDefaultLangID at least sets the top word
               to something odd so we mask both parts. *)
            fun toLANGID l =
                MAKELANGID(
                    toPrim(IntInf.andb(l, 0x3ff)),
                    toSub(IntInf.andb(IntInf.~>>(l, 0w10), 0x3f))
                )
                
        in
            val LANGID = absConversion {abs = toLANGID, rep = fromLANGID} INT
        end 
    end
end;
