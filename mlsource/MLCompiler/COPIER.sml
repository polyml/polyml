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
    fun localCopyTypeConstr (tcon, typeMap, _, mungeName, cache) =
        case tcIdentifier tcon of
            id as TypeId{typeFn=(_, EmptyType), ...} =>
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
                                val newName = mungeName(tcName tcon)
                            in
                                (*print(concat[tcName tcon, " not cached\n"]);*)
                                makeDatatypeConstr(newName,
                                    tcTypeVars tcon, newId, 0 (* Always global. *), tcLocations tcon)
                            end
                    end
            )

       |    TypeId{typeFn=(args, equiv), description, access, idKind, ...} =>
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
                then (*makeTypeAbbreviation(tcName tcon, args, copiedEquiv, tcLocations tcon)*)
                    makeFrozenTypeConstrs (tcName tcon, args,
                        TypeId {
                            access = access, description = description, idKind = idKind,
                            typeFn=(args, copiedEquiv)},
		                0, tcLocations tcon)

                else raise Misc.InternalError "localCopyTypeConstr: Well-formedness broken"
            end


    (* Exported version. *)
    fun copyTypeConstr (tcon, typeMap, copyTypeVar, mungeName) =
        localCopyTypeConstr(tcon, typeMap, copyTypeVar, mungeName, [])

    (* Compose typeID maps.  If the first map returns a Bound id we apply the second otherwise
       just return the result of the first. *)
    fun composeMaps(m1, m2) n =
    let
        fun map2 (TypeId{idKind=Bound{ offset, ...}, typeFn=(_, EmptyType), ...}) = m2 offset

        |   map2 (id as TypeId{idKind=Free _, typeFn=(_, EmptyType), ...}) = id

        |   map2 (TypeId{typeFn=(args, equiv), access, description, idKind, ...}) =
            let
                fun copyId(TypeId{idKind=Free _, ...}) = NONE
                |   copyId id = SOME(map2 id)
                (* If it's a type function e.g. this was a "where type" we have to apply the
                   map to any type identifiers in the type. *)
                val copiedEquiv =
                    copyType(equiv, fn x => x,
		                fn tcon => copyTypeConstr (tcon, copyId, fn x => x, fn y => y))
            in
                TypeId{typeFn=(args, copiedEquiv), access=access, description=description, idKind=idKind}
            end
    in
        map2(m1 n)
    end

    (* Generate new entries for all the elements of the signature.
       As well as copying the signature it also keeps track of addresses used in
       the signature for values.  This is needed because when we're constructing a signature
       we need to know the maximum address used.
       This is used to two cases only: when we have a named signature with possible sharing or
       "where types" or when including a signature.  Really these cases should renumber the
       value entries. *)
    fun localCopySig(sourceTab, resEnv, mapTypeId, singleLevel, strName, newMap, cacheTail): unit =
    let

        fun buildTypeCache(sourceTab, strName, mapTypeId, buildDatatypes, initialCache, cacheTail) =
        let
            (* Process sub-directories first.  That way they will be further down the list. *)
            fun foldSubStructs(dName, dVal, rest) =
            if tagIs structVar dVal
            then
            let
                val oldStruct = tagProject structVar dVal
                val Signatures { tab, typeIdMap, ...} = structSignat oldStruct
            in
                buildTypeCache(tab, strName ^ dName ^ "." (* Add structure names. *),
                    composeMaps(typeIdMap, mapTypeId), buildDatatypes, initialCache, rest)
            end
            else rest

            (* Then the types within this structure. *)
            fun foldTypes(_, dVal, rest) =
            if tagIs typeConstrVar dVal
            then
            let
                val tcon = tagProject typeConstrVar dVal
                fun makeName s = strName ^ (#second(splitString s))
                fun copyId(TypeId{idKind=Bound{ offset, ...}, ...}) = SOME(mapTypeId offset)
                |   copyId _ = NONE
            in
                (* On the first pass we build datatypes, on the second type abbreviations
                   using the copied datatypes. *)
                case tcIdentifier tcon of
                    id as TypeId{typeFn=(_, EmptyType), ...} =>
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
                 |  TypeId{typeFn=(args, equiv), access, description, idKind, ...} =>
                    if buildDatatypes then rest (* Not on this pass. *)
                    else (* Build a new entry whether the typeID has changed or not. *)
                    let
                        val copiedEquiv =
                            copyType(equiv, fn x => x,
        		                fn tcon =>
                                    localCopyTypeConstr(tcon, copyId, fn x => x, makeName, initialCache))
                        val copiedId =
                            TypeId{typeFn=(args, copiedEquiv), access=access, description=description, idKind=idKind}
                    in
                        makeFrozenTypeConstrs(makeName(tcName tcon), args, copiedId, 0, tcLocations tcon) :: rest
                    end
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
            buildTypeCache(sourceTab, strName, mapTypeId, true, (* Datatypes *) [], cacheTail)
        (* Now add any type abbreviations.  These can refer to datatypes we added in the
           previous pass but don't reuse type abbreviations we add elsewhere. *)
        val typeCache =
            buildTypeCache(sourceTab, strName, mapTypeId, false, (* Type abbreviations. *)datatypeCache, datatypeCache)

        fun copyTypeCons (tcon : typeConstrs) : typeConstrs =
        let
            fun copyId(TypeId{idKind=Bound{ offset, ...}, ...}) = SOME(mapTypeId offset)
            |   copyId _ = NONE
        in
            localCopyTypeConstr (tcon, copyId, fn x => x, fn s => strName ^ s, typeCache)
        end

        fun copyTyp (t : types) : types =
            copyType (t, fn x => x, (* Don't bother with type variables. *) copyTypeCons)
 
    in
        univFold
            (sourceTab,
            fn (dName: string, dVal: universal, _) =>
            if tagIs structVar dVal
            then 
            let
                val oldStruct = tagProject structVar dVal;
                val Signatures { name, tab, typeIdMap, minTypes, maxTypes, declaredAt, ...} = structSignat oldStruct;

                val newSig =
                    if singleLevel
                    then (* Just compose the maps. *)
                        makeSignature(name, tab, minTypes, maxTypes, declaredAt, composeMaps(typeIdMap, mapTypeId), [])
                    else (* Recursive copy. *)
                    let
                        (* Make a new sub-structure. *)
                        val newTab = makeSignatureTable ();
                        (* Copy everything into the new signature. *)
                        val () =
                            localCopySig 
                                (tab,
                                {
                                    enterType   = fn (s,v) => univEnter (newTab, typeConstrVar, s, v),
                                    enterStruct = fn (s,v) => univEnter (newTab, structVar,     s, v),
                                    enterVal    = fn (s,v) => univEnter (newTab, valueVar,      s, v)
                                },
                                composeMaps(typeIdMap, mapTypeId), false, strName ^ dName ^ ".", newMap, typeCache)
                    in
                        (* If we're copying it all set the resulting map to the new map. *)
                        makeSignature(name, newTab, minTypes, maxTypes, declaredAt, newMap, [])
                    end
                val newStruct =
                    Struct { name = structName oldStruct, signat = newSig,
                             access = structAccess oldStruct, locations = structLocations oldStruct}
            in
                #enterStruct resEnv (dName, newStruct)
            end (* structures *)
                 
            else if tagIs typeConstrVar dVal
            then
            let
                val oldConstr = tagProject typeConstrVar dVal
                val newConstr = copyTypeCons oldConstr;
                (* Copy the value constructors for a datatype. *)
       
                fun copyValueConstr(v as Value{name, typeOf, class, access, locations, references, ...}) =
                let
                    (* Copy its type and make a new constructor if the type has changed. *)
                    val newType = copyTyp typeOf;
                in
                    if not (identical (newType, typeOf))
                    then Value{name=name, typeOf=newType, class=class,
                               access=access, locations = locations, references = references}
                    else v
                end;

                val copiedConstrs = map copyValueConstr(tcConstructors oldConstr);
                val () =
                    if not (null copiedConstrs)
                    then tcSetConstructors (newConstr, copiedConstrs)
                    else ()
            in
                #enterType resEnv (dName, newConstr)
            end

            (* Finally the values and exceptions. *)
            else if tagIs valueVar dVal
            then
            let
                val v as Value {typeOf, class, name, access, locations, references, ...} =
                    tagProject valueVar dVal;
                val newType = copyTyp typeOf;
                (* Can save creating a new object if the address and type
                   are the same as they were. *)
                val res =
                    if not (identical (newType, typeOf))
                    then Value {typeOf=newType, class=class, name=name,
                                    access=access,locations=locations, references = references}
                    else v
            in
                #enterVal resEnv (name, res)
            end 
            else (),
            ()
            )
    end (* fullCopySig *)

    (* Exported versions of these. *)

    (* Open a structure or include a signature. *)
    fun openSignature(Signatures{ tab, typeIdMap, ...}, resEnv, strName) =
        localCopySig(tab, resEnv, typeIdMap, true (* One level. *), strName, typeIdMap, [])

    and fullCopyDatatype(oldConstr, mapTypeId, strName) =
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
        val () = localCopySig(sigSpace, resEnv, mapTypeId, true, strName, fn _ => raise Subscript, [])
    in
        valOf(! resType)
    end

    fun replaceMap(source: signatures, mapTypeId: int -> typeId, min, boundIds, newMap): signatures =
    let
        (* Make a new signature. *)
        val tab = makeSignatureTable ();

        val tsvEnv =
        {
            enterType   = fn (s,v) => univEnter (tab, typeConstrVar, s, v),
            enterStruct = fn (s,v) => univEnter (tab, structVar, s, v),
            enterVal = fn (s, v) => univEnter (tab, valueVar, s, v)
        }
        (* Copy everything into the new signature. *)
        val () = localCopySig(sigTab source, tsvEnv, mapTypeId, false, "", newMap, [])
    in
        makeSignature(sigName source, tab, min, min + List.length boundIds, sigDeclaredAt source, newMap, boundIds)
    end (* replaceMap *)

    (* Find the maximum run-time offset used for a value or structure in a signature.
       This excludes type IDs. *)
    fun getNextRuntimeOffset(Signatures{tab, ...}): int =
    let
        fun getOffset(_, dVal, m) =
        if tagIs valueVar dVal
        then case tagProject valueVar dVal of
            Value { access = Formal addr, ...} => Int.max(addr+1, m)
        |   _ => m
        else if tagIs structVar dVal
        then case structAccess(tagProject structVar dVal) of
            Formal addr => Int.max(addr+1, m)
        |   _ => m
        else m
    in
        univFold(tab, getOffset, 0)
    end

    structure Sharing =
    struct
        type signatures     = signatures
        type typeConstrs    = typeConstrs
        type structVals     = structVals
        type values         = values
        type typeId         = typeId
        type valAccess      = valAccess
        type types          = types
        type univTable      = univTable
    end

end;
