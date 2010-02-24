(* Bug found.  Final references for the inner loop were being processed in the wrong order. *)

PolyML.Compiler.maxInlineSize := 40;

type location = { startPosition: int, endPosition: int };
datatype ptProperties =
    PTprint of string
|   PTparent of unit -> exportTree
|   PTpreviousSibling of unit -> exportTree
|   PTnextSibling of unit -> exportTree
|   PTfirstChild of unit -> exportTree
withtype exportTree = location * ptProperties list

datatype direction = Up | Down | Left | Right;

fun find([], _) = NONE (* No change *)
|   find(PTparent p :: _, Up) = SOME p
|   find(PTpreviousSibling p :: _, Left) = SOME p
|   find(PTnextSibling p :: _, Right) = SOME p
|   find(PTfirstChild p :: _, Down) = SOME p
|   find(_ :: tl, dir) = find (tl, dir);

fun navigateTo(searchLocation as {startOffset, endOffset}, lastParsetree as ({ startPosition, endPosition, ... }, tree)) =
    if startOffset = startPosition andalso endOffset = endPosition
    then (* We're there already. *) lastParsetree
    else if startOffset >= startPosition andalso endOffset <= endPosition
    then (* It's this node or a child. *)
        let
            val child = find(tree, Down)
        in
            (* See if the element we want is actually a child. *)
            case child of
                SOME child =>
                let
                    (* See which child it is. *)
                    fun findChild(location as {startPosition, endPosition, ...}, child) =
                        if startOffset >= startPosition andalso endOffset <= endPosition
                        then SOME (location, child)
                        else
                        case find(child, Right) of
                            NONE => NONE
                        |   SOME next => findChild(next())
                in
                    case findChild(child()) of
                        NONE => lastParsetree (* In this *)
                    |   SOME child => navigateTo(searchLocation, child)
                end
            |   NONE => lastParsetree (* No children. *)
        end
    else raise Fail "bad";

fun pos(s, e) = {startPosition=s, endPosition=e}: location;
val n1 = (pos(1, 2), [PTprint("a")]);
val n2 = (pos(3, 4), [PTprint("b"), PTnextSibling(fn () => n1)]);
val n3 = (pos(1, 4), [PTprint("c"), PTfirstChild(fn()=>n2)]);

case navigateTo({startOffset=1, endOffset=1}, n3) of
    ({endPosition = 2, startPosition = 1}, [PTprint "a"]) => ()
|   _ => raise Fail "bad";
