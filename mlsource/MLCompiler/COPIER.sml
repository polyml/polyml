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
    
    structure UTILITIES:
    sig
        val splitString: string -> { first:string,second:string }
    end

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
    open STRUCTVALS TYPETREE UNIVERSALTABLE UTILITIES
    open Universal; (* for tag record selectors *)

    type tsvEnv = { enterType:   string * typeConstrs -> unit,
                  enterStruct: string * structVals  -> unit,
                  enterVal   : string * values      -> unit };

    (* Type constructor cache.  This maps typeIDs in the copied signature to
       type constructors.  More importantly, it identifies a type constructor
       that carries that type ID so that when we copy the values the string
       name is appropriate. *)

    (* Copy a type constructor if it is Bound and in the required range.  If this refers to a type
       function copies that as well. Does not copy value constructors. *)
    fun localCopyTypeConstr (tcon, typeMap, copyTypeVar, mungeName, cache) =
        case tcIdentifier tcon of
            TypeFunction(args, equiv) =>
            let
                val copiedEquiv =
                    copyType(equiv, fn x => x,
		                fn tcon =>
                            localCopyTypeConstr (tcon, typeMap, fn x => x, mungeName, cache))
            in
                if identical (equiv, copiedEquiv)
                then tcon (* Type is identical and we don't want to change the name. *)
                else (* How do we find a type function? *)
                    if null (tcConstructors tcon)
                then makeTypeAbbreviation(tcName tcon, args, copiedEquiv, tcLocations tcon)
                else raise Misc.InternalError "localCopyTypeConstr: Well-formedness broken"
            end

        |   id =>
            (
                case typeMap id of
                    NONE =>
                    (
                        (*print(concat[tcName tcon, " not copied\n"]);*)
                        tcon (* No change *)
                    )
                |   SOME newId =>
                    let
                        val name = #second(splitString (tcName tcon))
                        fun cacheMatch tc =
                            sameTypeId(tcIdentifier tc, newId) andalso #second(splitString(tcName tc)) = name
                    in
                        case List.find cacheMatch cache of
                            SOME tc =>
                            (
                                (*print(concat[tcName tcon, " copied as ", tcName tc, "\n"]);*)
                                tc (* Use the entry from the cache. *)
                            )
                        |   NONE =>
                            (* Either a hidden identifier or alternatively this can happen as part of
                               the matching process.
                               When matching a structure to a signature we first match up the type
                               constructors then copy the type of each value replacing bound type IDs
                               with the actual IDs as part of the checking process.
                               We will return SOME newId but we don't have a
                               cache so return NONE for List.find. *)
                            let
                                val oldName = tcName tcon
                                val newName = mungeName(tcName tcon)
                            in
                                (*print(concat[tcName tcon, " not cached\n"]);*)
                                makeDatatypeConstr(newName,
                                    tcTypeVars tcon, newId, 0 (* Always global. *), tcLocations tcon)
                            end
                    end
            )

    (* Exported version. *)
    fun copyTypeConstr (tcon, typeMap, copyTypeVar, mungeName) =
        localCopyTypeConstr(tcon, typeMap, copyTypeVar, mungeName, [])

    (* Generate new entries for all the elements of the signature.
       As well as copying the signature it also keeps track of addresses used in
       the signature for values.  This is needed because when we're constructing a signature
       we need to know the maximum address used.
       This is used to two cases only: when we have a named signature with possible sharing or
       "where types" or when including a signature.  Really these cases should renumber the
       value entries. *)
    fun localFullCopySig(sourceTab, resEnv, copyId, strName, cacheTail): unit =
    let

        fun buildTypeCache(sourceTab, strName, buildDatatypes, initialCache, cacheTail) =
        let
            (* Process sub-directories first.  That way they will be further down the list. *)
            fun foldSubStructs(dName, dVal, rest) =
            if tagIs structVar dVal
            then
            let
                val oldStruct = tagProject structVar dVal
                val oldSig    = structSignat oldStruct
            in
                buildTypeCache(sigTab oldSig, strName ^ dName ^ "." (* Add structure names. *),
                        buildDatatypes, initialCache, rest)
            end
            else rest

            (* Then the types within this structure. *)
            fun foldTypes(dName, dVal, rest) =
            if tagIs typeConstrVar dVal
            then
            let
                val tcon = tagProject typeConstrVar dVal
                fun makeName s = strName ^ (#second(splitString s))
            in
                (* On the first pass we build datatypes, on the second type abbreviations
                   using the copied datatypes. *)
                case tcIdentifier tcon of
                    TypeFunction(args, equiv) =>
                    if buildDatatypes then rest (* Not on this pass. *)
                    else (* Build a new entry whether the typeID has changed or not. *)
                    let
                        val copiedEquiv =
                            copyType(equiv, fn x => x,
        		                fn tcon =>
                                    localCopyTypeConstr(tcon, copyId, fn x => x, makeName, initialCache))
                    in
                        makeTypeAbbreviation(makeName(tcName tcon), args, copiedEquiv, tcLocations tcon) :: rest
                    end
                |   id =>
                    if not buildDatatypes then rest (* Not on this pass. *)
                    else
                    (
                        case copyId id of
                            NONE => rest (* Skip (or add to cache?) *)
                        |   SOME newId =>
                            (if null (tcConstructors tcon) then makeFrozenTypeConstrs else makeDatatypeConstr)
                                (makeName(tcName tcon),
                                    tcTypeVars tcon, newId, 0 (* Always global. *), tcLocations tcon) :: rest
                    )
            end
            else rest
        in
             univFold(sourceTab, foldTypes,
                univFold(sourceTab, foldSubStructs, cacheTail))
        end

        (* Process datatypes.  While processing these we make new entries for every
           datatype even if they are already in the cache.  That way we end up with
           the last entry in the list being the most local and that's the one we want
           to use for type abbreviations and values. *)
        val datatypeCache =
            buildTypeCache(sourceTab, strName, true, (* Datatypes *) [], cacheTail)
        (* Now add any type abbreviations.  These can refer to datatypes we added in the
           previous pass but don't reuse type abbreviations we add elsewhere. *)
        val typeCache =
            buildTypeCache(sourceTab, strName, false, (* Type abbreviations. *)datatypeCache, datatypeCache)

        fun copyTypeCons (tcon : typeConstrs) : typeConstrs =
            localCopyTypeConstr (tcon, copyId, fn x => x, fn s => strName ^ s, typeCache)

        fun copyTyp (t : types) : types =
            copyType (t, fn x => x, (* Don't bother with type variables. *) copyTypeCons)
 
        fun copyDatatypeAndConstructors(oldConstr, copyId, strName) =
        let
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
   in
    univFold
     (sourceTab,
      (fn (dName: string, dVal: universal, num) =>
        (if tagIs structVar dVal
         then let
           val oldStruct = tagProject structVar dVal;
           val oldSig     = structSignat oldStruct;
           
           (* Make a new sub-structure. *)
            val tab = makeSignatureTable ();
            (* Copy everything into the new signature. *)
            val () =
              localFullCopySig 
                (sigTab oldSig,
                {
                  enterType   = fn (s,v) => univEnter (tab, typeConstrVar, s, v),
                  enterStruct = fn (s,v) => univEnter (tab, structVar,     s, v),
                  enterVal    = fn (s,v) => univEnter (tab, valueVar,      s, v)
                },
                copyId, strName ^ dName ^ ".", typeCache);
            (* TODO: What, if anything is the typeID map for the result. *)
            val newSig =
                makeSignature(sigName oldSig, tab, sigMinTypes oldSig,
                            sigMaxTypes oldSig, sigDeclaredAt oldSig, sigTypeIdMap oldSig, [])
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
                copyDatatypeAndConstructors(tagProject typeConstrVar dVal, copyId, strName)
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

    (* Exported versions of these. *)
    fun fullCopySig(source, resEnv, copyId, strName) =
        localFullCopySig(sigTab source, resEnv, copyId, strName, [])

    and fullCopyDatatype(oldConstr, copyId, strName) =
    let
        val sigSpace = makeSignatureTable()
        val Env { enterType, ...} = makeEnv sigSpace
        val () = enterType(tcName oldConstr, oldConstr)
        val resType = ref NONE
        val resEnv =
            {
                enterType = fn (_, tc) => resType := SOME tc,
                enterStruct = fn (s, _) => raise Misc.InternalError ("enterStruct "^s),
                enterVal = fn (s, _) => raise Misc.InternalError ("enterVal "^s)
            }
        val () = localFullCopySig(sigSpace, resEnv, copyId, strName, [])
    in
        valOf(! resType)
    end

    (* Copy the result signature of a structure. *)
    fun copySig 
        (source       : signatures,
         wantCopy     : int -> bool,
         mapTypeId    : int -> typeId)
        : signatures = 
    let
        (* Make a new signature. *)
        val tab = makeSignatureTable ();
        fun copyId(id as Bound{ offset, ...}) =
            if wantCopy offset then SOME(mapTypeId offset) else NONE
        |   copyId id = NONE

        val tsvEnv =
        {
            enterType   = fn (s,v) => univEnter (tab, typeConstrVar, s, v),
            enterStruct = fn (s,v) => univEnter (tab, structVar, s, v),
            enterVal = fn (s, v) => univEnter (tab, valueVar, s, v)
        }
        (* Copy everything into the new signature. *)
        val () = fullCopySig (source, tsvEnv, copyId, "");
    in
        makeSignature(sigName source, tab, sigMinTypes source, sigMaxTypes source, sigDeclaredAt source, mapTypeId, [])
    end (* copySig *)

    (* Copy the result signature of a structure and renumber values and structures.  The type IDs
       will have numbers in range below startValues. *)
    fun copySigAndRenumber
        (source       : signatures,
         wantCopy     : int -> bool,
         mapTypeId    : int -> typeId,
         startValues  : int)
        : signatures = 
    let
        (* Make a new signature. *)
        val tab = makeSignatureTable ();
        fun copyId(id as Bound{ offset, ...}) =
            if wantCopy offset then SOME(mapTypeId offset) else NONE
        |   copyId id = NONE
        
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
        val () = fullCopySig (source, tsvEnv, copyId, "");
    in
        makeSignature(sigName source, tab, sigMinTypes source, sigMaxTypes source, sigDeclaredAt source, mapTypeId, [])
    end (* copySig *)

    structure Sharing =
    struct
        type signatures     = signatures
        type typeConstrs    = typeConstrs
        type structVals     = structVals
        type values         = values
        type typeId         = typeId
        type valAccess      = valAccess
        type types          = types
    end

end;
