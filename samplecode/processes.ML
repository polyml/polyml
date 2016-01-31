(*
    Title:      Process package for ML.
    Author:     David C. J. Matthews
    Copyright (c) 2007

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

(* This is provided for backwards compatibility.  New programs should use
   the Thread structure directly. *)

structure Process:
sig
   type 'a channel 
   val channel: unit -> '_a channel
   val send:    'a channel * 'a -> unit
   val receive: 'a channel -> 'a
   val fork:    (unit->unit)->unit
   val console: (unit->unit)->(unit->unit)
   val choice:  (unit->unit)*(unit->unit)->unit
   val interruptConsoleProcesses: unit->unit
end =
struct
    open Thread.Thread Thread.Mutex Thread.ConditionVar
    
    val debug = ref false and identifiers = ref 0 and ids = ref 0;

    (* Each process created by fork, console or choice has this information
       as thread-local data.  *)
    datatype processData = PROC of {
        synchro: (synchroniser * direction) list ref,   (* The synchroniser chain. *)
        blocker: conditionVar, (* Condition var to block this process. *)
        processNo: int          (* An identifier for debugging. *)
    }
    
    and synchroniser =
        SYNCH of {
        state: choicestate ref, (* The state of this choice. *)
        synchLock: mutex      (* A mutex to protect the state variable. *)
    }
    
    and choicestate = ChoiceUntaken | ChoiceTaken of direction
    
    and direction = DirLeft | DirRight;
    
    val procTag = Universal.tag(): processData Universal.tag

    (* Get the process data for this thread.  If it was created by a Thread call
       directly it may not yet have any process data so we need to make it now.  *)
    fun get_process_data(): processData =
        case getLocal procTag of
            SOME p => p
        |   NONE =>
            let
                val pnum = (identifiers := !identifiers+1; !identifiers);
                val pData = PROC { synchro = ref [], blocker = conditionVar(), processNo = pnum }
            in
                setLocal(procTag, pData);
                pData
            end

    datatype 'a channel =
        CHAN of {senders: 'a procVal list ref,
                 receivers: 'a option ref procVal list ref,
                 chanLock: mutex,
                 Id: int}

    (* This represents a suspended process.  The 'a is either a value to be
       sent or a "basket", a reference to hold the result. *)
    withtype 'a procVal = conditionVar * processData * 'a

    fun channel () : 'a channel =
        CHAN {senders = ref [], receivers = ref [],
              chanLock = mutex(), Id = (ids := !ids+1; !ids) }

    datatype 'a synchResult = NoMatch | FoundMatch of 'a procVal

    (* Prunes the synchroniser list to remove committed choices.
       Returns the first non-committed synchroniser or the first committed
       synchroniser with a choice that is taken in the "wrong" direction
       (i.e. which ndicates that this process must not be allowed to communicate). *)
    fun getActiveSynchroniser(PROC{synchro, ...}, unlockAfter) =
    let
        fun getSynch [] = []
        |   getSynch(l as (SYNCH{state, synchLock}, dir) :: t) =
            (
                lock synchLock;
                case ! state of
                    ChoiceUntaken => (* This is untaken.  Stop here. *)
                    (
                        if unlockAfter
                        then unlock synchLock
                        else (getSynch t; ()); (* We need to lock any others. *)
                        l
                    )
                |   ChoiceTaken d =>
                        (* This is taken.  Stop here if it is taken in a different way. *)
                    (
                        unlock synchLock;
                        if d = dir
                        then getSynch t
                        else l
                    )
            )
        val newSynchList = getSynch(! synchro)
    in
        (* We can update the list for this process.  We don't need to lock the
           synchro variable since it is only updated either by the process itself or
           when this process is waiting on a channel, which is locked before access. *)
        synchro := newSynchList;
        newSynchList
    end

    (* Try to find a matching process.  toSearch is the list of corresponding
       processes i.e. receivers if we are trying to send, senders if we are
       trying to receive.  The result is a pair of the updated version of the
       toSearch, with the matching process removed if a match has been found
       and the matching process's data. *)
    fun synchronise (toSearch: 'a procVal list, thisProcess) :
            'a procVal list * 'a synchResult =
    let     
        (* Release the lock on the synchroniser for the process that is looking for
           a partner.  This is only called if no matching process can be found.  *)
        fun releaseLock(PROC{synchro = ref synchro, ...}) =
            List.app
                (fn (SYNCH{synchLock, state=ref ChoiceUntaken}, _) => unlock synchLock
                  |  _ => ()) synchro

        (* Commit the choices and release the locks.  If some entries are shared
           between the two processes then we may find some entries already set. *)
        fun commitChoices(PROC{synchro=synchro as ref synch, ...}) =
        (
            List.app
                (fn (SYNCH{synchLock, state=state as ref ChoiceUntaken}, dir) =>
                     (state := ChoiceTaken dir; unlock synchLock)
                  |  _ => ()) synch;
            synchro := [] (* Since all are taken we can set this to the empty list. *)
        )

        (* Get the first synchroniser and lock it unless it is already committed. *)
        val mySynch = getActiveSynchroniser(thisProcess, false (* Leave locked. *))

        (* Get the list of synchronisers for a potential matching process.  Generally
           any process on the sender list will match a receiver and vice versa.  The
           exception is if the two processes are alternative choices.  We have to be
           careful with the synchroniser lists.  We've already locked the list for our
           process so we mustn't lock any synchronisers that are shared.  *)
        datatype matchResult =
            MrTaken | MrAlternatives | MrOK of (synchroniser * direction) list

        fun getMatchingSynchs(PROC{synchro, ...}) =
        let
            fun getSynch([], _) = MrOK []
            |   getSynch(l as (SYNCH{state, ...}, dir) :: t,
                         myL as (SYNCH{state=s, ...}, myDir) :: myT) =
                if s <> state 
                then (* Different references - safe to lock.  *)
                    lockSynch(l, myL)
                else (* Same reference - already locked. *)
                    if dir <> myDir (* These are different choices. *)
                then MrAlternatives (* Not allowed to communicate. *)
                else (* OK, same choice: test the rest*)
                    getSynch(t, myT)
            |   getSynch(l, []) =
                    (* The list of synchronisers for the original process is empty or
                       has run out before this.  *)
                    lockSynch(l, [])
    
            and lockSynch(l as (SYNCH{state, synchLock}, dir) :: t, myL) =
                (
                    lock synchLock;
                    case ! state of
                        ChoiceUntaken => (* This is untaken.  Stop here. *)
                        (
                            getSynch(t, myL); (* We need to lock any others. *)
                            MrOK l
                        )
                    |   ChoiceTaken d =>
                            (* This is taken.  Stop here if it is taken in a different way. *)
                        (
                            unlock synchLock;
                            if d = dir
                            then getSynch(t, myL)
                            else MrTaken
                        )
                )
            |   lockSynch _ = raise Match (* Suppress warning *)
        
        in
            getSynch(! synchro, mySynch) 
        end

        fun findAProcess [] = 
            (* Find a process that matches and return the new list of partners
              and the new list of runnable processes. *)
           (* No match *) ([], NoMatch)
        |   findAProcess((entry as (_,d,_)) :: t) =
            case getMatchingSynchs d of
                MrTaken =>
                    (* This process is a committed choice in a different direction.  Drop
                       it from the list since it can never communicate.  *)
                    findAProcess t
            |   MrAlternatives =>
                    (* This process is an alternative choice with our process.  It can
                       still communicate, just not with us.  Skip this and try the next. *)
                let
                    val (clist, result) = findAProcess t
                in
                    (entry :: clist, result)
                end
            |   MrOK _ =>
                    (t, FoundMatch entry) (* Return the new list. *)
        
    in
        case mySynch of
            (SYNCH{state = ref (ChoiceTaken _), ...}, _) :: _ =>
           (* This choice is already taken - kill this process.
              Actually all we do at this stage is pretend that the process
              cannot communicate, and suspend it.  Later it may be removed
              from the channel. *)
            (toSearch, NoMatch)
        |   _ => (* No synch or uncommitted choice. *)
            case findAProcess toSearch of
                t as (_, NoMatch) => (releaseLock thisProcess; t)
            |   t as (_, FoundMatch(_,p,_)) =>
                    (commitChoices thisProcess; commitChoices p; t)
    end

    (* We need to ensure that interrupts are delivered synchronously when
       synchronising rather than risk receiving an interrupt while holding a lock. *)
    fun blockInterrupt (f: unit->'a) =
    let
        open Thread
        val oldState = getAttributes()
    in
        case List.find (fn InterruptState _ => true | _ => false) oldState of
            SOME(InterruptState InterruptDefer) => f() (* Continue to defer. *)
        |   SOME(InterruptState InterruptSynch) => f() (* No need to change. *)
        |   _ => (* Unset(?) or asynchronous.  Have to make synchronous. *)
            let
                val () = setAttributes[InterruptState InterruptSynch]
                (* Call the function.  It may raise an Interrupt exception if it has to
                   wait.  In that case we still need to restore the old state. *)
                val result = 
                    f() handle exn => (setAttributes oldState; raise exn)
                val () = setAttributes oldState;
            in
                result
            end
    end

    fun send (CHAN {senders, receivers, chanLock, ...}, v:'a) =
        blockInterrupt(fn () =>
            let
                val () = lock chanLock;
                val myProcessData as PROC { blocker, ...} = get_process_data()
            in
                case synchronise(!receivers, myProcessData)
                of  (newlist, FoundMatch (p,_,basket)) (* Success *) =>
                    (
                        basket := SOME v; (* Put the sent value into the receiver's basket. *)
                        receivers := newlist;
                        signal p; (* Wake up the new thread. *)
                        unlock chanLock
                    )
                |   (newlist, NoMatch) (* Failure *) =>
                    (* Set the new receiver/sender list to include this process,
                       and suspend ourselves, releasing the lock. *)
                    (
                        senders := (blocker, myProcessData, v) :: !senders;
                        receivers := newlist;
                        (* Wait until we're woken up and release the lock.
                           This may result in an exception but if the exception is
                           raised the lock will be reacquired so we must unlock it in
                           the handler. *)
                        wait(blocker, chanLock)
                            handle exn => (unlock chanLock; raise exn);
                        (* We don't need the lock any longer. *)
                        unlock chanLock
                    )
            end
        )
    
    fun receive (CHAN {senders, receivers, chanLock, ...}): 'a =
        blockInterrupt(fn () =>
            let
                val () = lock chanLock;
                val myProcessData as PROC { blocker, ...} = get_process_data()
            in
                case synchronise(!senders, myProcessData)
                of  (newlist, FoundMatch (p,_,v)) (* Success *) =>
                    (
                        senders := newlist;
                        signal p; (* Wake up the sending thread. *)
                        unlock chanLock;
                        v (* This is our result *)
                    )
                |   (newlist, NoMatch) (* Failure *) =>
                    (* Set the new receiver/sender list to include this process,
                       and suspend ourselves, releasing the lock. *)
                    let
                        val basket = ref NONE; (* Create a basket to receive the result. *)
                    in
                        receivers := (blocker, myProcessData, basket) :: !receivers;
                        senders := newlist;
                        (* Wait until we're woken up and release the lock.
                           This may result in an exception but if the exception is
                           raised the lock will be reacquired so we must unlock it in
                           the handler. *)
                        wait(blocker, chanLock)
                            handle exn => (unlock chanLock; raise exn);
                        (* We don't need the lock any longer. *)
                        unlock chanLock;
                        valOf(!basket) (* This should have been set to SOME v by the sender. *)
                    end
            end
        )


    fun new_process f synch attrs =
    (* Make a new process. *)
    let
        val pnum = (identifiers := !identifiers+1; !identifiers);
        val data =
            PROC { synchro = ref synch,  processNo = pnum, blocker = conditionVar() }
        fun fun_to_fork () =
            (
                setLocal(procTag, data);
                (f () handle _ => ())
            )
        val newproc = fork(fun_to_fork, attrs)
    in
        if !debug then (PolyML.print("new_process:", data); ()) else ();
        newproc
    end
    
    fun fork f =
    let
        (* Get the parent's synchroniser and remove any redundant entries. *)
        val synch = getActiveSynchroniser(get_process_data(), true)
        val _ =
            new_process f synch (* Share the parent's synchroniser. *)
                [EnableBroadcastInterrupt false] (* Does not accept broadcasts. *)
    in
        ()
    end
    
    and console f =
    let
        (* Get the parent's synchroniser and remove any redundant entries. *)
        val synch = getActiveSynchroniser(get_process_data(), true)
        val threadId =
            new_process f synch (* Share the parent's synchroniser. *)
                [EnableBroadcastInterrupt true] (* Accepts broadcasts. *)
    in
        (* Return a function that will interrupt the process. *)
        fn () => interrupt threadId
    end

    and choice (f, g) =(* Fork a pair of "choice" processes. *)
    let
        (* Get the parent's synchroniser and remove any redundant entries. *)
        val synch = getActiveSynchroniser(get_process_data(), true)
        (* If the parent is already a Choice (whether Taken or not), we
           run the new processes in Parallel with it. The reason for this
           is that if we have choice( (choice(a,b); c), d)  we allow both
           a and c (say) to communicate (N.B.  "choice" creates two new
           processes and returns immediately so c runs in parallel with
           a and b).  It is actually equivalent to a.c + b.c + d .  *)
        val newSynch = SYNCH{state = ref ChoiceUntaken, synchLock = mutex()}
    in
        new_process g (synch @ [(newSynch, DirLeft)])
            [EnableBroadcastInterrupt false];
        new_process f (synch @ [(newSynch, DirRight)])
            [EnableBroadcastInterrupt false];
        ()
    end

    val interruptConsoleProcesses = broadcastInterrupt
end;
