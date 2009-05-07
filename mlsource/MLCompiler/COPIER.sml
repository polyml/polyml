(*
    Copyright (c) 2009 David C.J. Matthews

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
    Derived from the STRUCTURES module:
    Copyright (c) 2000-9
        Cambridge University Technical Services Limited
    Title:      Module Structure and Operations.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985

*)

functor COPIER(
    structure STRUCTVALS : STRUCTVALSIG;
    structure TYPETREE : TYPETREESIG

    structure UNIVERSALTABLE:
    sig
        type universal = Universal.universal
        type univTable
        type 'a tag = 'a Universal.tag

        val univEnter:  univTable * 'a tag * string * 'a -> unit;
        val univLookup: univTable * 'a tag * string -> 'a option;
        val univFold:   univTable * (string * universal * 'a -> 'a) * 'a -> 'a;
    end;

sharing type
  STRUCTVALS.types
= TYPETREE.types

sharing type
  STRUCTVALS.values
= TYPETREE.values

sharing type
  STRUCTVALS.typeId
= TYPETREE.typeId

sharing type
  STRUCTVALS.structVals
= TYPETREE.structVals

sharing type
  STRUCTVALS.typeConstrs
= TYPETREE.typeConstrs

sharing type
    UNIVERSALTABLE.univTable
=   STRUCTVALS.univTable
)
:COPIERSIG =
struct
    open STRUCTVALS TYPETREE UNIVERSALTABLE
    open Universal; (* for tag record selectors *)

    type tsvEnv = { enterType:   string * typeConstrs -> unit,
                  enterStruct: string * structVals  -> unit,
                  enterVal   : string * values      -> unit };

    (* Generate new entries for all the elements of the signature.
       As well as copying the signature it also keeps track of addresses used in
       the signature for values.  This is needed because when we're constructing a signature
       we need to know the maximum address used.
       This is used to two cases only: when we have a named signature with possible sharing or
       "where types" or when including a signature.  Really these cases should renumber the
       value entries. *)
    fun fullCopySig 
        (source        : signatures,
         resEnv        : tsvEnv,
         copyId        : typeId -> typeId,
         strName       : string): unit =
    let
        fun copyTypeCons (tcon : typeConstrs) : typeConstrs =
            copyTypeConstr (tcon, copyId, fn x => x, strName);

        fun copyTyp (t : types) : types =
            copyType (t, fn x => x, (* Don't bother with type variables. *) copyTypeCons);

        (* First copy the type constructors in this signature and any substructures.
          It's inefficient but harmless to do this again for substructures.
          TODO: Tidy this up. *)
        val () = copyTypeConstructors(source, copyId, strName)
    in
    univFold
     (sigTab source,
      (fn (dName: string, dVal: universal, num) =>
        (if tagIs structVar dVal
         then let
           val oldStruct = tagProject structVar dVal;
           val oldSig     = structSignat oldStruct;
           
           (* Make a new sub-structure. *)
            val tab = makeSignatureTable ();
            (* Copy everything into the new signature. *)
            val () =
              fullCopySig 
                (oldSig,
                {
                  enterType   = fn (s,v) => univEnter (tab, typeConstrVar, s, v),
                  enterStruct = fn (s,v) => univEnter (tab, structVar,     s, v),
                  enterVal    = fn (s,v) => univEnter (tab, valueVar,      s, v)
                },
                copyId, strName ^ dName ^ ".");
            (* TODO: What, if anything is the typeID map for the result. *)
            val newSig =
                makeSignature(sigName source, tab, sigMinTypes oldSig,
                            sigMaxTypes oldSig, sigDeclaredAt oldSig, sigTypeIdMap oldSig)
           val newStruct =
                Struct { name = structName oldStruct, signat = newSig,
                         access = structAccess oldStruct, locations = structLocations oldStruct}
         in
           #enterStruct resEnv (dName, newStruct)
         end (* structures *)
                 
         else if tagIs typeConstrVar dVal
         then
         let
            val newConstr =
                fullCopyDatatype(tagProject typeConstrVar dVal, copyId, strName)
         in
            #enterType resEnv (dName, newConstr)
         end

          (* Finally the values and exceptions. *)
          else if tagIs valueVar dVal
            then let
              val v = tagProject valueVar dVal;
            in
              case v of Value {typeOf, class, name, access, locations, ...} =>
                    let
                      val newType = copyTyp typeOf;
                      (* Can save creating a new object if the address and type
                         are the same as they were. *)
                      val res =
                        if not (identical (newType, typeOf))
                        then Value {typeOf=newType, class=class, name=name,
                                    access=access,locations=locations}
                        else v
                    in
                      #enterVal resEnv (name, res)
                    end
            end 
          else ()
        ) 
      ),
      ()
     )
  end (* fullCopySig *)

  (* Make entries for all the type constructors.  The only reason for
     doing this separately from fullCopySig is to try to ensure that the
     names we give the types are appropriate.  If we do this as part of
     fullCopySig we could get the wrong name in cases such as
     sig structure S: sig type t end structure T : sig val x: S.t end end.
     If fullCopySig happens to process "x" before "S" it will copy "t"
     and give it the name "T.t" rather than "S.t". *)
    and copyTypeConstructors(source: signatures, copyId: typeId -> typeId, strName: string): unit =
    let
        fun copyTypeCons (tcon : typeConstrs) : typeConstrs =
            copyTypeConstr (tcon, copyId, fn x => x, strName);
    in
    univFold
     (sigTab source,
      (fn (dName: string, dVal: universal, ()) =>
        (if tagIs structVar dVal
         then let
           val oldStruct = tagProject structVar dVal;
           val oldSig     = structSignat oldStruct;
         in
           copyTypeConstructors(oldSig, copyId, strName ^ dName ^ ".")
         end (* structures *)
                 
         else if tagIs typeConstrVar dVal
         then let (* Types *)
          (* Make a new constructor.  It will be entered in the match table
             and picked up when we copy the signature. *)
           val oldConstr = tagProject typeConstrVar dVal;
           val newConstr = copyTypeCons oldConstr
           fun idNumber tc =
            case tcIdentifier tc of
                Bound { offset, ...} => "(" ^ Int.toString offset ^ ")"
            |   _ => "(Not bound)"
           val _ = if tcEquality oldConstr andalso (isBoundId(tcIdentifier newConstr) orelse isFreeId(tcIdentifier newConstr))
                        andalso not (tcEquality newConstr)
           then TextIO.print (concat["Equality mismatch ", tcName oldConstr, idNumber oldConstr, 
                                " ", tcName newConstr, idNumber newConstr, "\n"])
           else ();
          in
           ()
          end
            
        else ()
        ) 
      ),
      ()
     )
     end

    and fullCopyDatatype(oldConstr, copyId, strName) =
    let
        fun copyTypeCons (tcon : typeConstrs) : typeConstrs =
            copyTypeConstr (tcon, copyId, fn x => x, strName);

        fun copyTyp (t : types) : types =
            copyType (t, fn x => x, (* Don't bother with type variables. *) copyTypeCons);

        (* Make a new constructor. *)
           
           (* 
              The new type constructor will use the NEW polymorphic
              type variables. This is because copyTypeCons uses the
              table built by matchSigs which maps OLD constructors to
              NEW ones, and the NEW constructors contain NEW type variables.
           *)
        val newConstr = copyTypeCons oldConstr;
           
        (* We must copy the datatype if any of the value
              constructors have to be copied. The datatype may
              be rigid but some of the value constructors may
              refer to flexible type names. *)
        val mustCopy = ref (not (identicalConstr (newConstr, oldConstr)));
           
        local
            val oldTypeVars : types list = List.map TypeVar (tcTypeVars oldConstr);
            val newTypeVars : types list = List.map TypeVar (tcTypeVars newConstr);
(* 
   We CAN legitimately get different numbers of type variables here,
   it we're trying to recover from a user error that we've already
   diagnosed. We'll just ignore the extra variables. SPF 26/6/96
*)
            fun zipTypeVars (x::xs) (y::ys) = (x, y) :: zipTypeVars xs ys
               | zipTypeVars _  _   = []
                 
            val typeVarTable : (types * types) list = 
               zipTypeVars oldTypeVars newTypeVars;
             
            fun copyTypeVar (t : types) : types =
            let
               fun search [] = t
                 | search ((oldTypeVar, newTypeVar) :: rest) =
                    if sameTypeVar (t, oldTypeVar) then newTypeVar else search rest
             in
               search typeVarTable
             end;
        in
             (* 
                 Dave was wrong - we DO need to copy the polymorphic type variables -
                  at least, we do here! This version hides the old version of
                  copyTyp, which is in the enclosing environment. The entire
                  type/signature matching code needs a thorough overhaul.
                  SPF 16/4/96
             *)
             (* TODO: If SPF is right we also need to redefine
                 copyTypeCons. DCJM 17/2/00.  *)
            fun copyTyp (t : types) : types = copyType (t, copyTypeVar, copyTypeCons);
        end;
           
           (* 
              Now copy the value constructors. The equality status
              and any equivalence (i.e. type t = ...) will have been
              processed when the constructor was copied.
              
              What's going on here? Copying the type constructor will
              use the NEW polymorphic variables, but copying the rest of
              the type will use the OLD ones, since copyTyp doesn't copy
              individual type variables - what a MESS! I think this means
              that we end up with OLD variables throughout.
              SPF 15/4/96
           *)
        val copiedConstrs =
             map 
              (fn (v as Value{name, typeOf, class, access, locations}) =>
               let
                 (* Copy its type and make a new constructor if the type
                    has changed. *)
                 val newType = copyTyp typeOf;
                 val typeChanged  = not (identical (newType, typeOf));
                 (* If this datatype shares with another one we will already have
                    constructors available.  This can happen, in particular, if
                    we have a signature constraining the result of a structure.
                    There will be sharing between the datatype in the implementing
                    structure and the result signature. *)
                 val copy =
                   if typeChanged
                   then let
                     val v' =
                        Value{name=name, typeOf=newType, class=class,
                              access=access, locations = locations}
                     (* See if the constructor already exists. *)
                   in
                     let
                         val original = findValueConstructor v'
                     in
                         (* We try to use the original if it is global since that
                           allows us to print values of the datatype.  If it is
                           not global we MUSTN'T use the copy.  It may be local
                           and so may not exist later on. *)
                        case original of
                            Value{access=Global _, ...} => original
                        |    _ => v'
                     end
                   end
                   else v;
               in
                 if typeChanged then mustCopy := true else ();
                 copy (* Return the copy. *)
               end)
              (tcConstructors oldConstr);
    in
        if not (null copiedConstrs)
        then tcSetConstructors (newConstr, copiedConstrs)
        else ();
        newConstr
    end

    (* Copy the result signature of a structure. *)
    fun copySig 
        (source       : signatures,
         wantCopy     : int -> bool,
         mapTypeId    : int -> typeId,
         strName      : string)
        : signatures = 
    let
        (* Make a new signature. *)
        val tab = makeSignatureTable ();
        fun copyId(id as Bound{ offset, ...}) =
            if wantCopy offset then mapTypeId offset else id
        |   copyId id = id

        val tsvEnv =
        {
            enterType   = fn (s,v) => univEnter (tab, typeConstrVar, s, v),
            enterStruct = fn (s,v) => univEnter (tab, structVar, s, v),
            enterVal = fn (s, v) => univEnter (tab, valueVar, s, v)
        }
        (* Copy everything into the new signature. *)
        val () = fullCopySig (source, tsvEnv, copyId, strName);
    in
        makeSignature(sigName source, tab, sigMinTypes source, sigMaxTypes source, sigDeclaredAt source, mapTypeId)
    end (* copySig *)

    (* Copy the result signature of a structure and renumber values and structures.  The type IDs
       will have numbers in range below startValues. *)
    fun copySigAndRenumber
        (source       : signatures,
         wantCopy     : int -> bool,
         mapTypeId    : int -> typeId,
         startValues  : int,
         strName      : string)
        : signatures = 
    let
        (* Make a new signature. *)
        val tab = makeSignatureTable ();
        fun copyId(id as Bound{ offset, ...}) =
            if wantCopy offset then mapTypeId offset else id
        |   copyId id = id
        
        val address = ref startValues

        (* Adjust the addresses of values and structures if necessary.
           The only time we do this is when producing the result signature of
           a functor and then we include space for the type IDs explicitly. *)
        fun newAccess(Formal _) =
            let val addr = !address in address := addr+1; Formal addr end
        |   newAccess access = access (* Global? *)

        val tsvEnv =
        {
            enterType   = fn (s,v) => univEnter (tab, typeConstrVar, s, v),
            enterStruct =
                fn (name, str) =>
                    univEnter (tab, structVar, name,
                        Struct{ name = structName str, signat = structSignat str,
                                access = newAccess(structAccess str),
                                locations = structLocations str}),
            enterVal =
                fn (dName, Value { name, typeOf, access, class, locations }) =>
                    univEnter (tab, valueVar, dName,
                        Value{name=name, typeOf = typeOf, access=newAccess access,
                              class=class, locations=locations})
        }
        (* Copy everything into the new signature. *)
        val () = fullCopySig (source, tsvEnv, copyId, strName);
    in
        makeSignature(sigName source, tab, sigMinTypes source, sigMaxTypes source, sigDeclaredAt source, mapTypeId)
    end (* copySig *)

    structure Sharing =
    struct
        type signatures     = signatures
        type typeConstrs    = typeConstrs
        type structVals     = structVals
        type values         = values
        type typeId         = typeId
        type valAccess      = valAccess
    end

end;
