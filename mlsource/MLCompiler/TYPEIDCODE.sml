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
    structure CODETREE :
    sig
        type machineWord
        type codetree

        val CodeNil:          codetree;
        val CodeTrue:         codetree;
        val CodeZero:         codetree;
        val isCodeNil:        codetree -> bool;
        val mkTuple:          codetree list -> codetree;
        val mkRecLoad:        int -> codetree;
        val mkLoad:           int * int -> codetree;
        val mkInd:            int * codetree -> codetree;
        val mkConst:          machineWord -> codetree;
        val mkEnv:            codetree list -> codetree;
        val mkProc:           codetree * int * int * string -> codetree;
        val mkInlproc:        codetree * int * int * string -> codetree;
        val mkEval:           codetree * codetree list * bool -> codetree;
        val mkStr:            string   -> codetree;
        val mkRaise:          codetree -> codetree;
        val mkNot:            codetree -> codetree;
        val mkTestnull:       codetree -> codetree;
        val mkTestnotnull:    codetree -> codetree;
        val mkTestinteq:      codetree * codetree -> codetree;
        val mkTestptreq:      codetree * codetree -> codetree;
        val mkCand:           codetree * codetree -> codetree;
        val mkCor:              codetree * codetree -> codetree;
        val mkMutualDecs:       codetree list -> codetree;
        val mkIf:               codetree * codetree * codetree -> codetree;
        val mkDec:              int * codetree -> codetree;
        val evalue:           codetree -> machineWord;

        val structureEq:      machineWord * machineWord -> bool

        val genCode:          codetree * Universal.universal list -> unit -> codetree
    end;

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
    
    sharing LEX.Sharing = STRUCTVALS.Sharing = PRETTY.Sharing = CODETREE = ADDRESS
) : TYPEIDCODESIG =
struct
    open CODETREE STRUCTVALS PRETTY ADDRESS
    open Misc RuntimeCalls

    val ioOp : int -> machineWord = RunCall.run_call1 POLY_SYS_io_operation;

    val andb = Word8.andb and orb = Word8.orb
    infix 6 andb;
    infix 7 orb;

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
        then case idAccess typeId of
            Local { addr = ref 0, level = ref 0 } =>
            (
                print(concat["Local value unset\n"]);
                vaGlobal(defaultEqAndPrintCode())
            )
        |   _ => codeAccess(idAccess typeId, level)
        else (* Type function. *)
        (
            print(concat["codeTypeId: Value is a type function\n"]);
            vaGlobal(defaultEqAndPrintCode())
        )

    (* Opaque matching and functor application create new type IDs using an existing
       type as implementation.  The equality function is inherited whether the type
       was specified as an eqtype or not.  The print function is inherited but a new
       ref is created so that if a pretty printer is installed for the new type it
       does not affect the old type. *)
    fun codeGenerativeId(sourceCode, level) =
    let
        val mutableFlags = F_words orb F_mutable;
    in
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
    end

    (* A generative ID may also be derived from a type function.  *)
    fun codeGenerativeTypeFunction(typeArgs, typeResult, level) =
    let
    in
        print(concat["Value is a type function\n"]);
        vaGlobal(defaultEqAndPrintCode())
    end    

    structure Sharing =
    struct
        type typeId     = typeId
        type codetree   = codetree
        type types      = types
    end
end;
