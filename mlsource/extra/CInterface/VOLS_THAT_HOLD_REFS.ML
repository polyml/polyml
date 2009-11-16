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
***)

(*
I think the idea here is to allow C data structures to be built up.
If we allocate a C object using the low-level "alloc" function it may be
freed by the GC if the corresponding ML object is no longer referenced.
We don't want that to happen if we have stored a pointer to this C object
in another C object.  Only if the root of the whole structure is no longer
reachable do we want the objects within the structure to be freed.
DCJM 30/4/01.

I've modified this to make it more efficient, particularly for large
arrays.  DCJM 27/6/01.
*)

(**********************************************************************
 *  Functor Definition
 **********************************************************************)


functor VOLS_THAT_HOLD_REFS
    (structure Underlying : VolatileSig
     structure ForeignException : ForeignExceptionSig)
     :> VolatileSig =
struct


structure Ctype = Underlying.Ctype
structure BehaviourRefs = Underlying.BehaviourRefs
structure Union = Underlying.Union
    
val sizeof = Ctype.sizeof;
open ForeignException
open Union


(*.....
datatype owner  = Owner of rawvol * dependancy ref list
and vol = Vol of rawvol * dependancy
where type dependancy = (int * owner) option
.....*)

(* The int records information for offset *)
    
datatype owner = Owner of Underlying.vol * ownerOpt Array.array ref
and ownerOpt = None | Some of int * owner (* This is more storage efficient than using "option". *)

datatype vol = Vol of {thevol: Underlying.vol, depends: ownerOpt}

fun vol(thevol, depends) = Vol{thevol = thevol, depends = depends}
fun thevol(Vol{thevol, ...}) = thevol
and depends(Vol{depends, ...}) = depends
 

val PointerSize = sizeof (Ctype.Cpointer Ctype.Cvoid)

val MaxVectorSize = 20 (* Maximum initial size of the dependency array. *)

fun selfOwner v deps =
    (******
     * Create a vol that `owns' itself, with dependencies "deps".
     ******)
    vol (v,Some(0,Owner(v, ref deps)))
    

(***********************************************************************
a = alloc(n)
***********************************************************************)

(* "alloc" allocates a new region of memory.  The dependencies are
   initialised to None.  If the region is large we don't allocate
   a dependency array that big at this stage.  Instead we grow the
   array later if necessary. Typically large arrays are byte arrays
   and they don't actually have dependencies. *)
fun alloc n ctype =
    let val m = n * sizeof ctype
    val v = Underlying.alloc n ctype
    in
    selfOwner v (Array.array(Int.min(m div PointerSize, MaxVectorSize), None))
    end

(***********************************************************************
a = &b
***********************************************************************)

(* "address" creates a new vol containing the address of an existing one.
   We create a single element array to hold the dependencies.  *)
fun address (Vol{thevol, depends}) =
    let val v = Underlying.address thevol
    in selfOwner v (Array.array (1, depends))
    end

(***********************************************************************
a = *b
***********************************************************************)

(* "deref" returns the value at the appropriate offset.  The dependency
   is the dependency at the corresponding (word) offset. (We need to do
   this if, for example, we have created a list in the C space.  If
   we extract the tail of a cell we need to hang on to the rest of
   the list.) *)
fun deref(Vol{thevol, depends}) =
    vol (Underlying.deref thevol,
        case depends of
            None => None
        |   Some(i, Owner(_, ref refs)) =>
                if i mod PointerSize <> 0
                then None
                else 
                let
                    val j = i div PointerSize
                in
                    if j < 0 orelse j >= Array.length refs
                    then None
                    else Array.sub(refs, j)
                end)
         

(***********************************************************************
a = b.X (offset n objects of type ctype)    [*a = ( char* )&b + n]
***********************************************************************)

(* "offset" gives us a new address based on the old one.  We shift the
   origin of the dependency index by the appropriate amount.  Because
   offset can be applied to this vol to give a new address we can't
   select the dependency at this offset yet.
   This is where the self-owner comes in.  Although we're creating a
   new vol here we need to retain the original vol in the event of a GC. *)
fun offset n ctype (Vol{thevol, depends}) =
    let val m = n * sizeof ctype
    in
    vol (Underlying.offset n ctype thevol,
         case depends of
         None => None
           | Some (i,x) => Some (i+m, x))
    end

(***********************************************************************
a := b (n bytes)
***********************************************************************)

(* When we make an assignment of h into g we update the dependencies of
   g to point to the dependencies of h.  We may have to grow the
   destination array.  *)
fun assign ctype g h =
    let val n = sizeof ctype
    in
        case (depends g, depends h) of
            (Some(i, Owner(_, drefs as ref destRefs)), Some(j, Owner(_, ref sourceRefs))) =>
                (* We only copy the references if we're moving at least a word and the
                   source and destinations are word aligned.  We also skip this if the
                   offsets are negative.  It's just possible that there might be a C
                   function which would return an area of memory with a pointer offset
                   within it.  In that case we don't want to raise a Subscript exception
                   but I don't know how to deal with the dependencies. *)
                if n < PointerSize orelse i mod PointerSize <> 0 orelse j mod PointerSize <> 0
                   orelse i < 0 orelse j < 0
                then ()
                else
                let
                    (* Neither the source array nor the destination array may be big enough. *)
                    val di = i div PointerSize (* Starting offset in dest. *)
                    val si = j div PointerSize (* Starting offset in source. *)
                    val maxLen = n div PointerSize (* Words to copy. *)
                    val sLen = Array.length sourceRefs
                    val toCopy = if si + maxLen <= sLen then maxLen else sLen - si
                in
                    if toCopy <= 0
                    then () (* Nothing in the source to copy. *)
                    else
                    let
                        val diMax = di + toCopy (* Maximum offset. *)
                        val dLen = Array.length destRefs
                        val dVec =
                            if dLen < diMax
                            then (* Need to grow array. *)
                            let
                                val newSize = Int.max(diMax, dLen + dLen div 2)
                                val newArray = Array.array(newSize, None)
                            in
                                (* Copy the old dependencies *)
                                Array.copy{dst = newArray, src = destRefs, di = 0};
                                drefs := newArray; (* Remember this new array. *)
                                newArray
                            end
                            else destRefs
                    in
                        (* Copy the dependencies being updated by the assignment. *)
                        ArraySlice.copy{dst = dVec, src = ArraySlice.slice(sourceRefs, si, SOME toCopy), di = di}
                    end
                end
        |   _ => ();
     Underlying.assign ctype (thevol g) (thevol h)
    end



(**********************************************************************
 From / To C values
 **********************************************************************)

fun makeVol v   =  selfOwner v (Array.array(1, None))

    
fun load_lib s =
    makeVol (Underlying.load_lib s)

    
fun load_sym (Vol{thevol, ...}) s =
    makeVol (Underlying.load_sym thevol s)


fun ID x = x;    
fun call_sym_and_convert g args rt =
    let val (u,us) =
    Underlying.call_sym_and_convert
        (thevol g)
        (map (mapDirectedArg ID thevol) args)
        rt
    in
    (mapUnion makeVol u, map (mapUnion makeVol) us)
    end;

    
val toCchar     = makeVol o Underlying.toCchar  
val toCdouble   = makeVol o Underlying.toCdouble
val toCfloat    = makeVol o Underlying.toCfloat
val toCint      = makeVol o Underlying.toCint   
val toClong     = makeVol o Underlying.toClong  
val toCshort    = makeVol o Underlying.toCshort 
val toCstring   = makeVol o Underlying.toCstring  
val toCuint     = makeVol o Underlying.toCuint   
val toCbytes    = makeVol o Underlying.toCbytes


val fromCchar   = Underlying.fromCchar      o thevol
val fromCdouble = Underlying.fromCdouble    o thevol
val fromCfloat  = Underlying.fromCfloat     o thevol
val fromCint    = Underlying.fromCint       o thevol
val fromClong   = Underlying.fromClong      o thevol
val fromCshort  = Underlying.fromCshort     o thevol
val fromCstring = Underlying.fromCstring    o thevol
val fromCuint   = Underlying.fromCuint      o thevol

val fillCstring = Underlying.fillCstring    o thevol

fun fromCbytes(v, i) = Underlying.fromCbytes(thevol v, i)

(*
DCJM 7/4/04.  I've added these although I suspect that they need to do more than
they're currently doing.
*)
fun toCfunction argType resType (f: vol list -> vol) =
    let
        fun appF (args: Underlying.vol list) : Underlying.vol =
            let
            in
               thevol (f (map makeVol args))
            end
    in
        makeVol (Underlying.toCfunction argType resType appF)
    end


fun toPascalfunction argType resType (f: vol list -> vol) =
    let
        fun appF (args: Underlying.vol list) : Underlying.vol =
            let
            in
               thevol (f (map makeVol args))
            end
    in
        makeVol (Underlying.toPascalfunction argType resType appF)
    end


fun setFinal f v = Underlying.setFinal (thevol f) (thevol v)


    local
        fun prettyVol _ _ (_: vol) = PolyML.PrettyString "?"
    in
        val () = PolyML.addPrettyPrinter prettyVol
    end


end (* struct *)
