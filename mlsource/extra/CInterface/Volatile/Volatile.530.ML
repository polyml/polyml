(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited

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

(***
Implementaion of a wrapper for the raw C programming primitives,
which provides the means for holding onto ML objects to avoid GC.

Also Convert between Ctype & RawCtype where necessary.
***)



signature VOLATILE_SIG =
sig
    type vol
    type Ctype
    
    val load_lib     : string -> vol
    val load_sym     : vol -> string -> vol
    val call_sym     : vol -> (Ctype * vol) list -> Ctype -> vol
    
    val alloc        : int -> Ctype -> vol
    val address      : vol -> vol
    val deref        : vol -> vol
    val offset       : int -> Ctype -> vol -> vol
    val assign       : Ctype -> vol -> vol -> unit

    val toCchar      : string -> vol
    val toCdouble    : real -> vol
    val toCfloat     : real -> vol
    val toCint       : int -> vol
    val toClong      : int -> vol
    val toCshort     : int -> vol
    val toCuint      : int -> vol

    val fromCchar    : vol -> string
    val fromCdouble  : vol -> real
    val fromCfloat   : vol -> real
    val fromCint     : vol -> int
    val fromClong    : vol -> int
    val fromCshort   : vol -> int
    val fromCuint    : vol -> int

end    


signature DISPATCH_SIG =
sig
    type rawvol
    type RawCtype
    
    val load_lib     : string -> rawvol
    val load_sym     : rawvol -> string -> rawvol
    val call_sym     : rawvol -> (RawCtype * rawvol) list -> RawCtype -> rawvol
    
    val alloc        : int -> rawvol
    val address      : rawvol -> rawvol
    val deref        : rawvol -> rawvol
    val offset       : rawvol -> int -> rawvol
    val assign       : rawvol -> rawvol -> int -> unit

    val toCchar      : string -> rawvol
    val toCdouble    : real -> rawvol
    val toCfloat     : real -> rawvol
    val toCint       : int -> rawvol
    val toClong      : int -> rawvol
    val toCshort     : int -> rawvol

    val fromCchar    : rawvol -> string
    val fromCdouble  : rawvol -> real
    val fromCfloat   : rawvol -> real
    val fromCint     : rawvol -> int
    val fromClong    : rawvol -> int
    val fromCshort   : rawvol -> int

end;

signature RAW_CTYPE_SIG =
sig
    datatype RawCtype =
    Cchar | Cdouble | Cfloat | Cint | Clong | Cpointer | Cshort | Cuint
      | Cstruct of int
end;    

signature CTYPE_SIG =
sig
    datatype Ctype =
    Cchar
      | Cdouble
      | Cfloat
      | Cint
      | Clong
      | Cshort
      | Cuint
      | Cpointer of Ctype
      | Cstruct of Ctype list
    (*| Cfunction of Ctype list * Ctype *)
      | Cvoid

    val sizeof : Ctype -> int
end;

signature FOREIGN_EXCEPTION_SIG =
sig
    exception Foreign of string
end;


(**********************************************************************
 *
 *  Functor Application
 *
 **********************************************************************)

functor VOLATILE
    (structure Dispatch : DISPATCH_SIG
     structure RawCtype : RAW_CTYPE_SIG
     structure Ctype : CTYPE_SIG
     structure ForeignException : FOREIGN_EXCEPTION_SIG
     sharing type Dispatch.RawCtype = RawCtype.RawCtype
) : VOLATILE_SIG  =
struct    

    
(***  Make just type rawvol visible ***)    
local structure Rawvol :
    sig
    type rawvol
    end = Dispatch
in open Rawvol
end
open Ctype
open ForeignException

(*.....
datatype owner  = Owner of rawvol * dependancy ref list
and vol = Vol of rawvol * dependancy
where type dependancy = (int * owner) option
.....*)

(* Local version of option *)
datatype 'a option = Some of 'a | None;


datatype owner  = Owner of rawvol * (int * owner) option ref list
abstype vol = Vol of rawvol * (int * owner) option
with
    fun thevol  (Vol (v,_)) = v
    fun depends (Vol (_,x)) = x
    val vol                 = Vol    
end

    local
        fun prettyVol _ _ (m: vol) = PolyML.PrettyString "?"
    in
        val () = PolyML.addPrettyPrinter prettyVol
    end


val PointerSize = 4
(* DCJM: That should be obtained from bytes_per_word. *)


fun copy n x =
    (******
     * Make a list containing n elements all equal to x.
     ******)
    let fun copy' 0 acc = acc
      | copy' n acc = copy' (n-1) (x::acc)
    in
    if n <= 0 then [] else copy' n []
    end

    
fun drop 0 xs = xs
  | drop _ [] = []
  | drop n (_::xs) = drop (n-1) xs;

   
fun dependencies g =
    case depends g of
    None => []
      | Some(i,Owner(_,refs)) =>
        if i mod PointerSize = 0
        then drop (i div PointerSize) refs
        else []


fun selfOwner v deps =
    (******
     * Create a vol that `owns' itself, with depndancies "deps".
     ******)
    vol (v,Some(0,Owner(v,deps)))
    

(***********************************************************************
a = alloc(n)
***********************************************************************)

fun alloc n ctype =
    let val m = n * sizeof ctype
    val v = Dispatch.alloc m
    in
    selfOwner v (map ref (copy (m div PointerSize) None))
    end

(***********************************************************************
a = &b
***********************************************************************)

fun address g =
    let val v = Dispatch.address (thevol g)
    in selfOwner v [ref (depends g)]
    end

(***********************************************************************
a = *b
***********************************************************************)

fun deref g =
    vol (Dispatch.deref (thevol g),
     case (dependencies g) of
         [] => None
       | (r::_) => !r)
         

(***********************************************************************
a = b.X (offset n objects of type ctype)    [*a = ( char* )&b + n]
***********************************************************************)

fun offset n ctype g =
    let val m = n * sizeof ctype
    in
    vol (Dispatch.offset (thevol g) m,
         case depends g of
         None => None
           | Some (i,x) => Some (i+m, x))
    end

(***********************************************************************
a := b (n bytes)
***********************************************************************)

fun copyRefs 0 _ _              = ()
  | copyRefs n (g::gs) (h::hs)  = (g := !h; copyRefs (n-1) gs hs)
  | copyRefs _ _ _              = ()

    
fun assign ctype g h =
    let val n = sizeof ctype
    in
    (copyRefs (n div 4) (dependencies g) (dependencies h);
     Dispatch.assign (thevol g) (thevol h) n)
    end



(**********************************************************************
 From / To C values
 **********************************************************************)


fun makeVol v   =  selfOwner v []

    
fun load_lib s =
    makeVol (Dispatch.load_lib s)
    handle Foreign mes => raise (Foreign ("load_lib "^s^": "^mes))

    
fun load_sym g s =
    makeVol (Dispatch.load_sym (thevol g) s)
    handle Foreign mes => raise (Foreign ("load_sym "^s^": "^mes))

    
    
fun convertRawCtype Cchar             = RawCtype.Cchar 
  | convertRawCtype Cdouble           = RawCtype.Cdouble
  | convertRawCtype Cfloat            = RawCtype.Cfloat
  | convertRawCtype Cint              = RawCtype.Cint
  | convertRawCtype Clong             = RawCtype.Clong
  | convertRawCtype Cshort            = RawCtype.Cshort
  | convertRawCtype Cuint             = RawCtype.Cuint
  | convertRawCtype (Cpointer _)      = RawCtype.Cpointer
(*| convertRawCtype (Cfunction _)     = RawCtype.Cpointer*) (*hmm?*)
  | convertRawCtype (Cstruct ts)      = RawCtype.Cstruct (sizeof (Cstruct ts))
  | convertRawCtype Cvoid             = RawCtype.Cint (*hack*)


fun call_sym g args rt =
    makeVol (Dispatch.call_sym
         (thevol g)
         (map (fn (t,g) => (convertRawCtype t,thevol g)) args)
         (convertRawCtype rt))
    

val toCchar     = makeVol o Dispatch.toCchar  
val toCdouble   = makeVol o Dispatch.toCdouble
val toCfloat    = makeVol o Dispatch.toCfloat
val toCint      = makeVol o Dispatch.toCint   
val toClong     = makeVol o Dispatch.toClong  
val toCshort    = makeVol o Dispatch.toCshort 
val toCuint     = makeVol o Dispatch.toCuint   


val fromCchar   = Dispatch.fromCchar      o thevol
val fromCdouble = Dispatch.fromCdouble    o thevol
val fromCfloat  = Dispatch.fromCfloat     o thevol
val fromCint    = Dispatch.fromCint         o thevol
val fromClong   = Dispatch.fromClong        o thevol
val fromCshort  = Dispatch.fromCshort     o thevol
val fromCuint   = Dispatch.fromCuint        o thevol


end (* struct *)
