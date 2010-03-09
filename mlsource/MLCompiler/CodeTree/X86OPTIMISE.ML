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
        fun labelInstruction(JumpLabel(Labels{uses=ref uses, ...})) = uses > 0
        |   labelInstruction(StartHandler _) = true
        |   labelInstruction(FillJumpTable _) = true
        |   labelInstruction _ = false

        (* If we remove a branch the use counts are reduced. *)
        fun eliminate(UncondBranch(Labels{uses, ...})) =
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
        |   forward((u as UncondBranch(Labels{forward=source, ...})) ::
                    (next as JumpLabel(Labels{forward=dest, ...})) :: tl, list, rep) =
            if source = dest
            then (eliminate u; forward(next :: tl, list, true))
            else forward(next :: tl, u :: list, rep)
           
            (* Eliminate dead instructions after UnconditionalBranch up to the next label. *)
        |   forward((u as UncondBranch _) :: next :: tl, list, rep) =
            if labelInstruction next
            then forward(next :: tl, u :: list, rep)
            else (eliminate next; forward(u :: tl, list, true))

            (* Eliminate unreferenced labels.  *)
        |   forward(JumpLabel(Labels{uses=ref 0, ...}) :: tl, list, _) =
                forward(tl, list, true)

            (* Branch chaining. If we have a label followed by an unconditional branch set the
               chain entry of the label to destination of the branch. *)
        |   forward((l as JumpLabel(Labels{chain as ref NONE, ...})) ::
                    (u as UncondBranch(uncondLab as Labels{chain=ref chainL, ...})) :: tl, list, _) =
            (
                if isSome chainL then chain := chainL else chain := SOME uncondLab;
                (* It's essential to include the branch in the output at this point.
                   Otherwise we could eliminate the branch if it happened to be followed
                   by its destination, set the use count of the destination label to
                   zero, eliminate that; all before we had updated the incoming
                   branches. *)
                forward(tl, u :: l :: list, true)
            )

            (* Reorder conditional and unconditional branches.  If we have a conditional branch
               followed by an unconditional branch followed by the destination of the conditional
               branch we can turn the test round. *)
        |   forward((c as ConditionalBranch{label=Labels{forward=condRef, ...}, test, predict, ...}) ::
                    (u as UncondBranch uncondLab) ::
                    (d as JumpLabel(Labels{forward=dest, ...})) :: tl, list, rep) =
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

            (* Eliminate unreferenced labels.  *)
        |   reverse(JumpLabel(Labels{uses=ref 0, ...}) :: tl, list, _) =
                reverse(tl, list, true)

            (* Combine multiple labels by setting the first to point to the second.  This may subsequently
               eliminate the first label.  This simplifies branch chaining.  This can only be
               done after we've eliminated any unreferenced labels. *)
        |   reverse((a as JumpLabel(Labels{chain as ref NONE, ...})) ::
                    (b as JumpLabel(bL as Labels{chain=ref chainB, ...})) :: tl,
                    list, _) =
            (
                if isSome chainB then chain := chainB else chain := SOME bL;
                reverse(b :: tl, a :: list, true)
            )

            (* Branch chaining.  If we have a branch to a destination that is chained
               we replace this by a branch to the ultimate destination.  This is done on
               the reverse pass because generally branches are forward and the extensions will
               have been added after the branches were seen on the forward pass. *)
        |   reverse(UncondBranch(Labels{chain=ref(SOME dest), uses=oldUses, ...}) :: tl, list, _) =
            let
                fun follow(Labels{chain=ref(SOME c), ...}) = follow c
                |   follow c = c
                val dest as Labels{uses=newUses, ...} = follow dest
            in
                oldUses := !oldUses - 1;
                ! newUses >= 1 orelse raise InternalError "UncondBranch1";
                newUses := !newUses + 1;
                ! oldUses >= 0 orelse raise InternalError "UncondBranch2";
                reverse(UncondBranch dest :: tl, list, true)
            end

        |   reverse(
                ConditionalBranch{
                    label=Labels{chain=ref(SOME dest), uses=oldUses, ...}, test, predict}
                    :: tl, list, _) =
            let
                fun follow(Labels{chain=ref(SOME c), ...}) = follow c
                |   follow c = c
                val dest as Labels{uses=newUses, ...} = follow dest
            in
                oldUses := !oldUses - 1;
                ! newUses >= 1 orelse raise InternalError "ConditionalBranch1";
                newUses := !newUses + 1;
                ! oldUses >= 0 orelse raise InternalError "ConditionalBranch2";
                reverse(ConditionalBranch{label=dest, test=test, predict=predict} :: tl, list, true)
            end

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
