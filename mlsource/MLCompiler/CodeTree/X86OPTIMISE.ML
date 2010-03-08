(*
    Copyright David C. J. Matthews 2010

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

functor X86OPTIMISE(
    structure X86CODE: X86CODESIG
) :
sig
    type operation
    type code
    type operations = operation list

    val optimise: code * operations -> operations

    structure Sharing:
    sig
        type operation = operation
        type code = code
    end
end =
struct
    open X86CODE
    exception InternalError = Misc.InternalError

    fun optimise(code, ops) =
    let
        (* All these instructions can't be eliminated after a jump. *)
        fun labelInstruction(ForwardJumpLabel(Labels{uses=ref uses, ...})) = uses > 0
        |   labelInstruction(BackJumpLabel(BackLabels{uses=ref uses, ...})) = uses > 0
        |   labelInstruction(StartHandler _) = true
        |   labelInstruction(FillJumpTable _) = true
        |   labelInstruction _ = false

        (* If we remove a branch the use counts are reduced. *)
        fun eliminate(JumpBack(BackLabels{uses, ...})) =
            (!uses > 0 orelse raise InternalError "eliminate"; uses := !uses-1)
        |   eliminate(UncondBranch(Labels{uses, ...})) =
            (!uses > 0 orelse raise InternalError "eliminate"; uses := !uses-1)
        |   eliminate(ConditionalBranch{label=Labels{uses, ...}, ...}) =
            (!uses > 0 orelse raise InternalError "eliminate"; uses := !uses-1)
        |   eliminate _ = ()
    
        (* Optimise the code list by repeated scans up and down the list.
           Scan forward through the list reversing it as we go.  Then scan the
           reversed list and turn it back into the original order. *)
        fun forward([], list, rep) = reverse(list, [], rep)

            (* Eliminate dead instructions after RaiseException up to the next label. *)
        |   forward((u as RaiseException) :: next :: tl, list, rep) =
            if labelInstruction next
            then forward(next :: tl, u :: list, rep)
            else (eliminate next; forward(u :: tl, list, true))

            (* Eliminate Unconditional branches to the next instruction. *)
        |   forward((u as UncondBranch(Labels{refs=source, ...})) ::
                    (next as ForwardJumpLabel(Labels{refs=dest, ...})) :: tl, list, rep) =
            if source = dest
            then (eliminate u; forward(next :: tl, list, true))
            else forward(next :: tl, u :: list, rep)
            
            (* Eliminate dead instructions after UnconditionalBranch up to the next label. *)
        |   forward((u as UncondBranch _) :: next :: tl, list, rep) =
            if labelInstruction next
            then forward(next :: tl, u :: list, rep)
            else (eliminate next; forward(u :: tl, list, true))
 
            (* Eliminate dead instructions after JumpBack up to the next label. *)
        |   forward((u as JumpBack _) :: next :: tl, list, rep) =
            if labelInstruction next
            then forward(next :: tl, u :: list, rep)
            else (eliminate next; forward(u :: tl, list, true))

            (* Eliminate unreferenced labels. *)
        |   forward(ForwardJumpLabel(Labels{uses=ref 0, ...}) :: tl, list, _) =
                forward(tl, list, true)

        |   forward(BackJumpLabel(BackLabels{uses=ref 0, ...}) :: tl, list, _) =
                forward(tl, list, true)

            (* Reorder conditional and unconditional branches.  If we have a conditional branch
               followed by an unconditional branch followed by the destination of the conditional
               branch we can turn the test round. *)
        |   forward((c as ConditionalBranch{label=Labels{refs=condRef, ...}, test, predict, ...}) ::
                    (u as UncondBranch uncondLab) ::
                    (d as ForwardJumpLabel(Labels{refs=dest, ...})) :: tl, list, rep) =
            if condRef = dest
            then
            let
                fun reverseTest JE  = JNE
                |   reverseTest JNE = JE
                |   reverseTest JA  = JNA
                |   reverseTest JB  = JNB
                |   reverseTest JNA = JA
                |   reverseTest JNB = JB
                |   reverseTest JL  = JGE
                |   reverseTest JG  = JLE
                |   reverseTest JLE = JG
                |   reverseTest JGE = JL
                |   reverseTest JO  = JNO
                |   reverseTest JNO = JO

                fun reversePrediction PredictNeutral = PredictNeutral
                |   reversePrediction PredictTaken = PredictNotTaken
                |   reversePrediction PredictNotTaken = PredictTaken
            in
                eliminate c;
                forward(d :: tl,
                    ConditionalBranch{label=uncondLab, test=reverseTest test, predict=reversePrediction predict} ::
                        list, true)
            end
            else forward(u :: d :: tl, c :: list, rep)


        |   forward(hd :: tl, list, rep) = forward(tl, hd :: list, rep)
        
        and reverse([], list, rep) = (list, rep)
        |   reverse(hd :: tl, list, rep) = reverse(tl, hd :: list, rep)

        (* Repeat scans through the code until there are no further changes. *)
        fun repeat ops =
            case forward(ops, [], false) of
                (list, false) => list
            |   (list, true) => repeat list

    in
        if lowLevelOptimise code
        then repeat ops
        else ops
    end

    structure Sharing =
    struct
        type operation = operation
        type code = code
    end
end;
