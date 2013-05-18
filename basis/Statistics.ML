(*
    Title:      Pretty Printer.
    Author:     David Matthews
    Copyright   David Matthews 2013

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

(*
    This is an interface to the statistics provided by the run-time system.
    For machine-independence and backwards compatibility they are encoded
    using ASN1 binary encodeing
*)

local
    datatype statistic =
        UnknownStat
    |   CounterStat of { identifier: int, name: string, count: int }
    |   SizeStat of { identifier: int, name: string, size: int }
    |   TimeStat of { identifier: int, name: string, time: Time.time }
    |   UserStat of { identifier: int, name: string, count: int }

    datatype component =
        CounterValue of int
    |   ByteCount of int
    |   Time of Time.time
    |   UnknownComponent

    fun convStats(v: Word8Vector.vector) =
    let
        open Word8VectorSlice
        fun getNext n =
            if length n = 0 then NONE
            else SOME(sub(n, 0), subslice(n, 1, NONE))

        (* Convert the length data *)
        fun getLength p =
            case getNext p of
                SOME (n, t) =>
                if n < 0wx80 then (Word8.toInt n, t)
                else
                let
                    fun getL(0w0, m, l) = (m, l)
                    |   getL(n, m, t) =
                            case getNext t of
                                SOME (hd, tl) => getL(n-0w1, m * 256 + Word8.toInt hd, tl)
                            |   NONE => raise Fail "Format error" 
                in
                    getL(Word8.andb(n, 0wx7f), 0, t)
                end
            |   NONE => raise Fail "Format error"


        fun parseInt p =
            case getNext p of
                NONE => 0
            |   SOME(h, tl) =>
                let
                    fun parseRest(n, p) =
                        case getNext p of
                            NONE => n
                        |   SOME (hd, tl) => parseRest(n * 256 + Word8.toInt hd, tl)
                in
                    parseRest(Word8.toIntX h, tl)
                end

        and parseString t = Byte.bytesToString(vector t)

        and splitTagLength p =
            case getNext p of
                SOME (code, t) =>
                let
                    (* TODO: For long-term forwards compatibility we could consider the possibility of
                       a tag value >= 31 which will be encoded in subsequent bytes before the
                       length. *)
                    val (len, tail) = getLength t
                    val thisData = Word8VectorSlice.subslice(tail, 0, SOME len)
                    val remainder = Word8VectorSlice.subslice(tail, len, NONE)
                in
                    (code, thisData, remainder)
                end
            |   NONE => raise Fail "Format error"

        and parseStatistic p =
            case splitTagLength p of
                (0wx61, thisData, remainder) =>
                    (
                        case parseComponents({identifier=0, name="", value=UnknownComponent}, thisData) of
                            {identifier, name, value=CounterValue v} =>
                                (CounterStat{identifier=identifier, name=name, count=v}, remainder)
                        |   _ => (UnknownStat, remainder)
                    )

            |   (0wx62, thisData, remainder) =>
                    (
                        case parseComponents({identifier=0, name="", value=UnknownComponent}, thisData) of
                            {identifier, name, value=ByteCount s} =>
                                (SizeStat{identifier=identifier, name=name, size=s}, remainder)
                        |   _ => (UnknownStat, remainder)
                    )

            |   (0wx63, thisData, remainder) =>
                    (
                        case parseComponents({identifier=0, name="", value=UnknownComponent}, thisData) of
                            {identifier, name, value=Time t} =>
                                (TimeStat{identifier=identifier, name=name, time=t}, remainder)
                        |   _ => (UnknownStat, remainder)
                    )

            |   (0wx6B, thisData, remainder) =>
                    (
                        case parseComponents({identifier=0, name="", value=UnknownComponent}, thisData) of
                            {identifier, name, value=CounterValue c} =>
                                (UserStat{identifier=identifier, name=name, count=c}, remainder)
                        |   _ => (UnknownStat, remainder)
                    )

            |  (_, _, remainder) => (UnknownStat, remainder) 


        and parseComponents(result as {identifier, name, value }, p) =
            if length p = 0
            then result
            else
            (
                case splitTagLength p of
                    (0wx44, thisData, remainder) =>
                        parseComponents({identifier=parseInt thisData, name=name, value=value}, remainder)
                |   (0wx45, thisData, remainder) =>
                        parseComponents({name=parseString thisData, identifier=identifier, value=value}, remainder)
                |   (0wx46, thisData, remainder) =>
                        parseComponents({identifier=identifier, name=name, value=CounterValue(parseInt thisData)}, remainder)
                |   (0wx47, thisData, remainder) =>
                        parseComponents({identifier=identifier, name=name, value=ByteCount(parseInt thisData)}, remainder)
                |   (0wx68, thisData, remainder) =>
                        let
                            fun parseTime (t, p) =
                                if length p = 0
                                then t
                                else
                                (
                                    case splitTagLength p of
                                        (0wx49, thisData, remainder) =>
                                            parseTime(t + Time.fromSeconds(parseInt thisData), remainder)
                                    |   (0wx4A, thisData, remainder) =>
                                            parseTime(t + Time.fromMicroseconds(parseInt thisData), remainder)
                                    |   (_, _, remainder) => parseTime(t, remainder) (* Unknown *)
                                )
                        in
                            parseComponents({identifier=identifier, name=name,
                                    value=Time(parseTime(Time.zeroTime, thisData))}, remainder)
                        end
                |  (_, _, remainder) => parseComponents(result, remainder)
            )

        fun parseStatistics l =
            if length l = 0
            then []
            else
            let
                val (item, rest) = parseStatistic l
                val items = parseStatistics rest
            in
                item :: items
            end

        val stats =
            case splitTagLength (full v) of
                (0wx60, thisData, _) => parseStatistics thisData
            |   _ => raise Fail "Statistics not available"
        
        fun extractCounter(n, l) =
            case List.find (fn CounterStat{identifier, ...} => identifier = n | _ => false) l of
                SOME(CounterStat{ count, ...}) => count
            |   _ => 0
        and extractSize(n, l) =
            case List.find (fn SizeStat{identifier, ...} => identifier = n | _ => false) l of
                SOME(SizeStat{ size, ...}) => size
            |   _ => 0
        and extractTime(n, l) =
            case List.find (fn TimeStat{identifier, ...} => identifier = n | _ => false) l of
                SOME(TimeStat{ time, ...}) => time
            |   _ => Time.zeroTime
        and extractUser(n, l) =
            case List.find (fn UserStat{identifier, ...} => identifier = n | _ => false) l of
                SOME(UserStat{ count, ...}) => count
            |   _ => 0
    in
        {
            threadsTotal = extractCounter(1, stats),
            threadsInML = extractCounter(2, stats),
            threadsWaitIO = extractCounter(3, stats),
            threadsWaitMutex = extractCounter(4, stats),
            threadsWaitCondVar = extractCounter(5, stats),
            threadsWaitSignal = extractCounter(6, stats),
            gcFullGCs = extractCounter(7, stats),
            gcPartialGCs = extractCounter(8, stats),
            sizeHeap = extractSize(9, stats),
            sizeHeapFreeLastGC = extractSize(10, stats),
            sizeHeapFreeLastFullGC = extractSize(11, stats),
            sizeAllocation = extractSize(12, stats),
            sizeAllocationFree = extractSize(13, stats),
            timeNonGCUser = extractTime(14, stats),
            timeNonGCSystem = extractTime(15, stats),
            timeGCUser = extractTime(16, stats),
            timeGCSystem = extractTime(17, stats),
            userCounters = Vector.tabulate(8, fn n => extractUser(n+18, stats))
        }
    end

    open RunCall RuntimeCalls

in
    structure PolyML =
    struct
        open PolyML
        structure Statistics =
        struct
            fun getLocalStats() =
                convStats(RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (29, ()))
            and getRemoteStats(pid: int) =
                convStats(RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (30, pid))
            and numUserCounters(): int =
                RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (27, ())
            and setUserCounter(which: int, value: int): unit =
                RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (28, (which, value))
        end
    end
end;
