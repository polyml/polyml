(*
    Copyright (c) 2013 David C.J. Matthews

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
If a function has an empty closure it can be code-generated immediately.  That may allow
other functions or tuples to be generated immediately as well.  As well as avoiding
run-time allocations this also allows the code-generator to use calls/jumps to constant
addresses.
*)
functor CODETREE_CODEGEN_CONSTANT_FUNCTIONS (
    structure BASECODETREE: BaseCodeTreeSig
    structure CODETREE_FUNCTIONS: CodetreeFunctionsSig

    structure BACKEND:
    sig
        type codetree
        type machineWord = Address.machineWord
        val codeGenerate:
            codetree * int * Universal.universal list -> (unit -> machineWord) * Universal.universal list
        structure Sharing : sig type codetree = codetree end
    end

    structure DEBUG :
    sig
        val codetreeAfterOptTag: bool Universal.tag (* If true then print the original code. *)
        val getParameter : 'a Universal.tag -> Universal.universal list -> 'a
    end

    structure PRETTY : PRETTYSIG

    sharing
        BASECODETREE.Sharing
    =   CODETREE_FUNCTIONS.Sharing
    =   BACKEND.Sharing
    =   PRETTY.Sharing
):
sig
    type codetree
    type machineWord = Address.machineWord
    val codeGenerate: codetree * int * Universal.universal list -> (unit -> machineWord) * Universal.universal list
    structure Sharing: sig type codetree = codetree end
end =
struct
    open BASECODETREE
    open CODETREE_FUNCTIONS
    open Address

    exception InternalError = Misc.InternalError

    datatype lookupVal = EnvGenLoad of loadForm | EnvGenConst of machineWord * Universal.universal list

    type cgContext =
    {
        lookupAddr: loadForm -> lookupVal,
        enterConstant: int * (machineWord * Universal.universal list) -> unit,
        debugArgs: Universal.universal list
    }

    (* Code-generate a function or set of mutually recursive functions that contain no free variables
       and run the code to return the address.  This allows us to further fold the address as
       a constant if, for example, it is used in a tuple. *)
    local
        exception Interrupt = Thread.Thread.Interrupt
    in
        fun codeGenerateToConstant debugSwitches (pt, localCount) =
        let
            val () =
                if DEBUG.getParameter DEBUG.codetreeAfterOptTag debugSwitches
                then PRETTY.getCompilerOutput debugSwitches (BASECODETREE.pretty pt) else ()

            val (code, props) = BACKEND.codeGenerate(pt, localCount, debugSwitches)
        in
            Constnt (code(), props) (* Evaluate it and convert any exceptions into Raise instrs. *)
                handle Interrupt => raise Interrupt (* Must not handle this *)
                | exn => Raise (Constnt(toMachineWord exn, []))
        end
    end

    fun cgFuns ({ lookupAddr, ...}: cgContext) (Extract ext) =
        (
            (* Look up the entry.  It may now be a constant.  If it isn't it may still
               have changed if it is a closure entry and other closure entries have
               been replaced by constants. *)
            case lookupAddr ext of
                EnvGenLoad load => SOME(Extract load)
            |   EnvGenConst w => SOME(Constnt w)
        )

    |   cgFuns ({lookupAddr, debugArgs, ...}) 
            (Lambda { body, isInline, name, closure, argTypes, resultType, localCount, recUse}) =
        let
            val cArray = Array.array(localCount, NONE)
            val newClosure = makeClosure()

            fun lookupLocal(load as LoadLocal n) =
                (
                    case Array.sub(cArray, n) of
                        NONE => EnvGenLoad load
                    |   SOME w => EnvGenConst w
                )
            |   lookupLocal(LoadClosure n) =
                (
                    case lookupAddr(List.nth (closure, n)) of
                        EnvGenLoad load => EnvGenLoad(addToClosure newClosure load)
                    |   c as EnvGenConst _ => c
                )
            |   lookupLocal load = EnvGenLoad load (* Argument or Recursive *)
            
            val context =
            {
                lookupAddr = lookupLocal,
                enterConstant = fn (n, w) => Array.update(cArray, n, SOME w),
                debugArgs = debugArgs
            }

            (* Process the body to deal with any sub-functions and also to bind
               in any constants from free variables. *)
            val newBody = mapCodetree (cgFuns context) body
            (* Build the resulting lambda. *)
            val resultClosure = extractClosure newClosure
            val resultLambda =
                Lambda { 
                    body = newBody, isInline = isInline, name = name, closure = resultClosure,
                    argTypes = argTypes, resultType = resultType, localCount = localCount,
                    recUse = recUse
                }
        in
            (* If the closure is (now) empty we can code-generate it. *)
            case resultClosure of
                [] => SOME(codeGenerateToConstant debugArgs (resultLambda, localCount))
            |   _ =>  SOME resultLambda
        end

    |   cgFuns (context as { enterConstant, debugArgs, ...}) (Newenv(envBindings, envExp)) =
        let
            (* First expand out any mutually-recursive bindings.  This ensures that if
               we have any RecDecs left *)
            val expandedBindings =
                List.foldr (fn (d, l) => partitionMutableBindings d @ l) [] envBindings

            fun processBindings(Declar{value, addr, use} :: tail) =
                (
                    (* If this is a constant put it in the table otherwise create a binding. *)
                    case mapCodetree (cgFuns context) value of
                        Constnt w => (enterConstant(addr, w); processBindings tail)
                    |   code => Declar{value=code, addr=addr, use=use} :: processBindings tail
                )                    

            |   processBindings(NullBinding c :: tail) =
                    NullBinding(mapCodetree (cgFuns context) c) :: processBindings tail

            |   processBindings(RecDecs[{addr, lambda, use}] :: tail) =
                    (* Single recursive bindings - treat as simple binding *)
                    processBindings(Declar{addr=addr, value=Lambda lambda, use = use} :: tail)               

            |   processBindings(RecDecs recdecs :: tail) =
                let
                    (* We know that this forms a strongly connected component so it is only
                       possible to code-generate the group if no function has a free-variable
                       outside the group.  Each function must have at least one free
                       variable which is part of the group.  *)
                    fun processEntry {addr, lambda, use} =
                        case mapCodetree (cgFuns context) (Lambda lambda) of
                            Lambda result => { addr=addr, lambda=result, use=use}
                        |   _ => raise InternalError "processEntry: not lambda"

                    val processedGroup = map processEntry recdecs

                    (* If every free variable is another member of the group we can
                       code-generate the group. *)
                    local
                        fun closureItemInGroup(LoadLocal n) =
                                List.exists(fn{addr, ...} => n = addr) processedGroup
                        |   closureItemInGroup _ = false

                        fun onlyInGroup{lambda={closure, ...}, ...} = List.all closureItemInGroup closure
                    in
                        val canCodeGen = List.all onlyInGroup processedGroup
                    end
                in
                    if canCodeGen
                    then
                    let
                        val extracts =
                            List.map(fn {addr, ...} => Extract(LoadLocal addr)) processedGroup

                        val code = Newenv([RecDecs processedGroup], mkTuple extracts)
                        val maxAddr = List.foldl(fn ({addr, ...}, n) => Int.max(addr, n)) 0 processedGroup
                        (* Code generate it. *)
                        val results = codeGenerateToConstant debugArgs (code, maxAddr+1)
                        (* Enter the constants in the table. *)
                        fun enterDec ({addr, ...}, n) =
                        (
                            case findEntryInBlock(results,  n, false) of
                                Constnt w => enterConstant(addr, w)
                            |   _ => raise InternalError "Not a constant";
                            n+1
                        )
                        val _ = List.foldl enterDec 0 processedGroup
                    in
                        processBindings tail
                    end

                    else RecDecs processedGroup :: processBindings tail
                end

            |   processBindings(Container{addr, use, size, setter} :: tail) =
                    Container{addr=addr, use=use, size=size,
                              setter = mapCodetree (cgFuns context) setter} :: processBindings tail
                
            |   processBindings [] = []

            val bindings = processBindings expandedBindings
            val body = mapCodetree (cgFuns context) envExp
        in
            case bindings of
                [] => SOME body
            |   _ => SOME(Newenv(bindings, body))
        end

    |   cgFuns context (Tuple{ fields, isVariant }) =
            (* Create any constant tuples that have arisen because they contain
               constant functions. *)
            SOME((if isVariant then mkDatatype else mkTuple)(map (mapCodetree (cgFuns context)) fields))

    |   cgFuns _ _ = NONE
    
    fun codeGenerate(original, nLocals, debugArgs) =
    let
        val cArray = Array.array(nLocals, NONE)
        fun lookupAddr(load as LoadLocal n) =
            (
                case Array.sub(cArray, n) of
                    NONE => EnvGenLoad load
                |   SOME w => EnvGenConst w
            )
        |   lookupAddr _ = raise InternalError "lookupConstant: top-level reached"
            
        val context = 
        {
            lookupAddr = lookupAddr,
            enterConstant = fn (n, w) => Array.update(cArray, n, SOME w),
            debugArgs = debugArgs
        }
        
        val resultCode = mapCodetree (cgFuns context) original
        val (code, props) = BACKEND.codeGenerate(resultCode, nLocals, debugArgs)

        (* The code may consist of tuples (i.e. compiled ML structures) containing
           a mixture of Loads, where the values are yet to be compiled, and
           Constants, where the code has now been compiled.  We need to extract
           any properties from the constants and return the whole lot as
           tuple properties. *)
        fun extractProps(Constnt(_, p)) = p
        |   extractProps(Extract ext) =
            (
                case lookupAddr ext of
                    EnvGenLoad _ => []
                |   EnvGenConst(_, p) => p
            )
        |   extractProps(Tuple{fields, ...}) =
            let
                val fieldProps = map extractProps fields
            in
                if List.all null fieldProps
                then []
                else [Universal.tagInject CodeTags.tupleTag fieldProps]
            end
        |   extractProps(Newenv(_, exp)) = extractProps exp
        |   extractProps _ = []

        val newProps = extractProps original

    in
        (code, CodeTags.mergeTupleProps(newProps, props))
    end

    structure Sharing = struct type codetree = codetree end
end;
