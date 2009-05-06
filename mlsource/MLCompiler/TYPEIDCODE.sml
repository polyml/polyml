(*
    Copyright (c) 2009 David C. J. Matthews

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

functor TYPEIDCODE (
    structure LEX : LEXSIG;
    structure CODETREE : CODETREESIG

    structure STRUCTVALS : STRUCTVALSIG;

    structure DEBUG :
    sig
        val printDepthFunTag : (unit->int) Universal.tag
        val errorDepthTag: int Universal.tag
        val getParameter :
           'a Universal.tag -> Universal.universal list -> 'a
    end;

    structure PRETTY : PRETTYSIG;

    structure ADDRESS :
    sig
        type machineWord;
        val toMachineWord : 'a    -> machineWord;
        val F_words     : Word8.word;
        val F_bytes     : Word8.word;
        val F_mutable   : Word8.word;
    end;
    
    sharing LEX.Sharing = STRUCTVALS.Sharing = PRETTY.Sharing = CODETREE.Sharing = ADDRESS
) : TYPEIDCODESIG =
struct
    open CODETREE STRUCTVALS PRETTY ADDRESS
    open Misc RuntimeCalls

    val ioOp : int -> machineWord = RunCall.run_call1 POLY_SYS_io_operation;

    val andb = Word8.andb and orb = Word8.orb
    infix 6 andb;
    infix 7 orb;
    val mutableFlags = F_words orb F_mutable;

    (* codeStruct and codeAccess are copied from ValueOps. *)
    fun codeStruct (str, level) =
        if isUndefinedStruct str
        then CodeNil
        else codeAccess (structAccess str, level)

    and codeAccess (Global code, _) = code
      
    |   codeAccess (Local{addr=ref locAddr, level=ref locLevel}, level) =
            mkLoad (locAddr, level - locLevel) (* No need for the recursive case. *)
     
    |   codeAccess (Selected{addr, base}, level) =
            mkInd (addr, codeStruct (base, level))
     
    |   codeAccess (acc, level) = raise InternalError "No access"

    (* Load an identifier. *)
    fun codeId(typeId, level) =
        if isFreeId typeId orelse isBoundId typeId
        then codeAccess(idAccess typeId, level)
        else (* Type function. *)
        (
            vaGlobal(defaultEqAndPrintCode())
        )

    (* Opaque matching and functor application create new type IDs using an existing
       type as implementation.  The equality function is inherited whether the type
       was specified as an eqtype or not.  The print function is inherited but a new
       ref is created so that if a pretty printer is installed for the new type it
       does not affect the old type. *)
    fun codeGenerativeId(sourceCode, level) =
        mkTuple
        [
            mkInd(0, sourceCode),
            mkEval
                (mkConst (ioOp POLY_SYS_alloc_store),
                [mkConst (toMachineWord 1), mkConst (toMachineWord mutableFlags),
                 mkEval(mkConst(ioOp POLY_SYS_load_word),
                    [mkInd(1, sourceCode), CodeZero], false)
                  ],
            false)  
        ]

    (* A generative ID may also be derived from a type function.  *)
    fun codeGenerativeTypeFunction(typeArgs, typeResult, level) =
        vaGlobal(defaultEqAndPrintCode())

    (* Create the equality and type functions for a set of mutually recursive datatypes. *)
    fun createDatatypeFunctions(typelist, mkAddr, level) =
    let
        (* Each entry has an equality function and a ref to a print function.
           The print functions for each type needs to indirect through the refs
           when printing other types so that if a pretty printer is later
           installed for one of the types the others will use the new pretty
           printer.  That means that the code has to be produced in stages. *)
        (* Create the equality functions.  Because mutual decs can only be functions we
           can't create the typeIDs themselves as mutual declarations. *)
        val eqAddresses = List.map(fn _ => mkAddr()) typelist (* Make addresses for the equalities. *)
        local
            fun createEquality(tc, addr) =
            let
                val code =
                if tcEquality tc
                then
                let
                    (* The structure equality function takes an argument pair.  We need a
                       function that takes two Poly-style arguments. *)
                    val defaultEqCode =
                        mkInlproc(
                            mkInlproc(
                                mkEval(mkConst(toMachineWord structureEq),
                                    [mkTuple[mkLoad(~1, 0), mkLoad(~2, 0)]], true), 1, 2, "eq-helper"),
                            0, 0, "eq-helper()")
                in
                    defaultEqCode
                end
                else (* The type system should ensure that this is never called. *)
                    mkConst(toMachineWord(fn _ => fn _ => raise Fail "Not an equality type"))
            in
                mkDec(addr, code)
            end
        in
            val equalityFunctions =
                mkMutualDecs(ListPair.map createEquality (typelist, eqAddresses))
        end

        (* Create the typeId values and set their addresses.  The print function is
           initially set as zero. *)
        local
            fun makeTypeId(tc, eqAddr) =
            let
                val var = vaLocal(idAccess(tcIdentifier tc))
                val newAddr = mkAddr()
                val idCode =
                mkTuple
                [
                    mkLoad(eqAddr, 0),
                    mkEval
                        (mkConst (ioOp POLY_SYS_alloc_store),
                        [mkConst (toMachineWord 1), mkConst (toMachineWord mutableFlags),
                         CodeZero (* Temporary value. *)],
                    false)  
                ]
            in
                #addr var := newAddr;
                #level var:= level;
                mkDec(newAddr, idCode)
            end
        in
            val typeIdCode = ListPair.map makeTypeId (typelist, eqAddresses)
        end

        (* Create the print functions and set the printer code for each typeId. *)
        local
            fun setPrinter tc =
            let
                fun printer depth typeArgs value = PrettyString ("(" ^ tcName tc ^ ")")
            in
                mkEval(
                    mkConst (ioOp POLY_SYS_assign_word),
                    [mkInd(1, codeId(tcIdentifier tc, level)), CodeZero, mkConst (toMachineWord printer)],
                    false)
            end
        in
            val printerCode = List.map setPrinter typelist
        end
    in
        equalityFunctions :: typeIdCode @ printerCode
    end

    structure Sharing =
    struct
        type typeId     = typeId
        type codetree   = codetree
        type types      = types
        type typeConstrs= typeConstrs
    end
end;
