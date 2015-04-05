(* Example code for
   let
    fun addOne x = x+1
   in
    addOne 2
   end
*)
local
    open PolyML.CodeTree RuntimeCalls
    val addFn = rtsFunction POLY_SYS_aplus
    val one = mkConstant (RunCall.unsafeCast 1)
    val addOne =
        mkFunction(mkCall(addFn, [mkLoad(~1, 0), one]), 1, "addOne")
    val two = mkConstant (RunCall.unsafeCast 2)
    val block =
        mkEnv([mkDec(1, addOne)], mkCall(mkLoad(1, 0), [two]))
    val run = genCode block ()
    val result = valOf (evalue run)
in
    val r: int = RunCall.unsafeCast result
end;

(* Another example showing how ML values can be incorporated in the code.
The code is roughly equivalent to the ML code but the function being defined takes
two arguments rather than a tuple.
let
    fun concatWithSpace(a,b) = a ^ " " ^ b
in
    TextIO.print(concatWithSpace("Hello", "World\n"
end
*)
let
    open PolyML.NameSpace PolyML.CodeTree
    (* Simple case: just cast this as a constant. *)
    val printCode = mkConstant (RunCall.unsafeCast TextIO.print)
    (* More complicated case.  Using this gets access to any inlined code for "^". *)
    val concat = codeForValue(valOf (#lookupVal PolyML.globalNameSpace "^"))
    (* fun concatWithSpace(a,b) = a ^ " " ^ b 
       The function we're defining takes two arguments but ML functions such as "^" always
       take a single argument which may be a tuple. *)
    val space = mkConstant(RunCall.unsafeCast " ")
    val concatWithSpace =
        mkFunction(
            mkCall(concat,
            [mkTuple[
                mkCall(concat, [mkTuple[mkLoad(~2, 0), space]]),
                mkLoad(~1, 0)
                ]
            ]),
            2, "concatWithSpace")
    val hello = mkConstant(RunCall.unsafeCast "Hello")
    val world = mkConstant(RunCall.unsafeCast "World\n")
    val block =
        mkEnv(
            [mkDec(1, concatWithSpace)],
            mkCall(printCode,
                [mkCall(mkLoad(1, 0), [hello, world])]
            )
        )
    val code = genCode block
in
    code();
    ()
end;

(* Example of building a loop. *)
let
    open RuntimeCalls;
    open PolyML.CodeTree;

    fun printInt n = TextIO.print(Int.toString n ^ "\n");

    val code =
    mkBeginLoop(
    mkIf(
            mkCall(rtsFunction POLY_SYS_equala, [mkLoad(1,0), mkConstant(RunCall.unsafeCast 10)]),
            mkConstant(RunCall.unsafeCast 0),
            mkEnv(
                [mkNullDec(mkCall(mkConstant(RunCall.unsafeCast printInt), [mkLoad(1,0)]))],
                mkLoop[
                    mkCall(rtsFunction POLY_SYS_aplus, [mkLoad(1,0), mkConstant(RunCall.unsafeCast 1)])
                ]
            )
        )
        ,
        [(1, mkConstant(RunCall.unsafeCast 0))]
    )
in
    genCode code ()
end;

