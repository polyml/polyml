(* Check that ThreadLib.protect releases its mutex, restores interrupt attributes,
   and preserves exception diagnostics on normal and exceptional exits. *)
local
    datatype interruptState = datatype Thread.Thread.interruptState
    datatype threadAttribute = datatype Thread.Thread.threadAttribute
    exception Payload of int * string

    val blockingTimeout = Time.fromSeconds 10

    (* Allow time for the blocking test to time out and the worker to report its result. *)
    val workerTimeout = Time.fromNanoseconds (3 * Time.toNanoseconds blockingTimeout)

    fun check b : unit = if b then () else raise Fail "Thread protection"

    fun checkUnlocked mutex : unit =
        (* There is no direct lock-state query. Acquire without blocking to check that the mutex is
           unlocked, then undo this acquisition to leave it unlocked. *)
        (check (Thread.Mutex.trylock mutex); Thread.Mutex.unlock mutex)

    fun mode () =
        case List.find (fn InterruptState _ => true | _ => false)
            (Thread.Thread.getAttributes ()) of
            SOME (InterruptState s) => s
          | _ => raise Fail "Missing interrupt mode"

    (* Check temporary interrupt masking and restoration on normal and exceptional exits:
       callers must retain their settings, and exceptions must retain their diagnostics. *)
    fun attributes mutex (state, broadcast) : unit =
        let
            (* Check the broadcast flag: changing delivery mode must not change
               whether the thread accepts broadcast interrupts. *)
            fun checkAttributes state =
                (check (mode () = state);
                 check (List.exists (fn EnableBroadcastInterrupt b => b = broadcast | _ => false)
                    (Thread.Thread.getAttributes ())))

            (* Deferred interrupts must stay deferred inside protection; other modes
               become synchronous so delivery cannot interrupt locking or unlocking. *)
            val () = Thread.Thread.setAttributes
                [InterruptState state, EnableBroadcastInterrupt broadcast]
            val expected = if state = InterruptDefer then state else InterruptSynch

            (* A successful callback must run with the temporary attributes, then leave
               the caller's attributes restored and the mutex available for reuse. *)
            val () = ThreadLib.protect mutex checkAttributes expected
            val () = checkAttributes state
            val () = checkUnlocked mutex

            (* Repeat with a failing callback: cleanup must preserve the exception's
               payload and source location rather than replace its diagnostics. *)
            val () =
                let
                    (* Arbitrary payload and location chosen to be recognizable. *)
                    val payload = (42, "payload")
                    val location = {file = "protection-origin", startLine = 7, startPosition = 2,
                         endLine = 8, endPosition = 3}
                in
                (ThreadLib.protect mutex (fn () => (checkAttributes expected;
                    PolyML.Exception.raiseWithLocation (Payload payload, location))) ();
                 raise Fail "Exception lost")
                handle e as Payload actual =>
                    (check (actual = payload);
                     check (PolyML.Exception.exceptionLocation e = SOME location))
                end
        in
            (* Exceptional cleanup must restore the same state as normal cleanup. *)
            checkAttributes state;
            checkUnlocked mutex
        end

    fun expectInterrupt f : unit =
        (f (); raise Fail "Interrupt lost") handle Thread.Thread.Interrupt => ()

    (* Request an interrupt from inside a protected callback and check when it is
       delivered. With fails = false, test all four initial modes. With fails = true,
       test only asynchronous modes: the callback raises Payload, but the pending
       interrupt must be delivered during cleanup before Payload can be reraised. *)
    fun pending mutex state fails : unit =
        let
            (* This is the mode protect must save and restore after the callback. *)
            val () = Thread.Thread.setAttributes [InterruptState state]

            (* Set only if protect returns normally, before any explicit delivery below. *)
            val returned = ref false

            (* interrupt (self ()) requests an interrupt of this test thread. Protection
               keeps deferred mode or switches to synchronous mode. Since this callback
               has no interruption point, the request stays pending when it returns or
               raises Payload; it must not interrupt the callback asynchronously. *)
            fun operation () =
                (Thread.Thread.interrupt (Thread.Thread.self ());
                 if fails then raise Payload (99, "interrupted cleanup") else ())
        in
            (* Accept only Interrupt. A missing interrupt or an escaping Payload fails
               the test, including when interrupt delivery replaces Payload in cleanup. *)
            expectInterrupt (fn () =>
                (ThreadLib.protect mutex operation ();
                 returned := true;
                 (* Synchronous mode leaves the request pending until testInterrupt.
                    Deferred mode first needs synchronous handling enabled; that change
                    may itself deliver the request. Asynchronous modes must deliver
                    inside protect's cleanup and must never reach this code. *)
                 if state = InterruptDefer
                 then Thread.Thread.setAttributes [InterruptState InterruptSynch] else ();
                 Thread.Thread.testInterrupt ()));

            (* Thus returned must be true only for a successful callback whose original
               mode was deferred or synchronous. In both asynchronous modes it must be
               false, whether the callback returned or raised Payload. *)
            check (!returned =
                (not fails andalso (state = InterruptSynch orelse state = InterruptDefer)));

            (* One-shot delivery must consume the asynchronous permission so a second
               interrupt cannot arrive asynchronously during the caller's recovery. *)
            if state = InterruptAsynchOnce then check (mode () = InterruptSynch) else ();

            (* Leave a known mode for later checks and verify that delivery did not
               strand the mutex in the locked state. *)
            Thread.Thread.setAttributes [InterruptState InterruptSynch];
            checkUnlocked mutex
        end

    (* Interrupt a protected callback at a condition-variable wait: blocking operations
       must remain interruptible, and unwinding must release all protection mutexes. *)
    fun blockingInterrupt mutex : unit =
        let
            (* The gate coordinates delivery; parent is this test thread, not the runner. *)
            val gate = Thread.Mutex.mutex ()
            val parent = Thread.Thread.self ()

            (* Called with both mutex and gate held by the nested protection below. *)
            fun wait () =
                let
                    (* The worker acquires gate only after waitUntil releases it,
                       ensuring the interrupt arrives at the blocking point. *)
                    val _ = Thread.Thread.fork (fn () => ThreadLib.protect gate
                        (fn () => Thread.Thread.interrupt parent) (), [])
                    (* A lost interrupt must fail within a bounded time instead of hanging. *)
                    val deadline = Time.+ (Time.now (), blockingTimeout)
                in
                    (* No thread signals this condition variable, so success requires an
                       Interrupt exception. The wait reacquires gate before raising it. *)
                    Thread.ConditionVar.waitUntil
                        (Thread.ConditionVar.conditionVar (), gate, deadline);
                    raise Fail "Blocking interrupt lost"
                end
        in
            (* The interrupt must unwind both protection levels after the wait releases
               and reacquires gate; neither mutex may remain locked afterward. *)
            expectInterrupt (fn () => ThreadLib.protect mutex
                (ThreadLib.protect gate wait) ());
            checkUnlocked gate;
            checkUnlocked mutex
        end

    (* Run tests in a separate thread so their interrupt settings and pending requests
       cannot leak into the runner. Report failures back, and bound the wait by
       workerTimeout so a deadlocked test cannot stall the runner indefinitely. *)
    fun inWorker (test : Thread.Mutex.mutex -> unit) : unit =
        let
            val gate = Thread.Mutex.mutex ()
            val changed = Thread.ConditionVar.conditionVar ()
            (* NONE means unfinished; SOME NONE means success; SOME (SOME exn) means failure.
               Access under gate keeps publishing and waiting synchronized. *)
            val result = ref (NONE : exn option option)
            fun worker () =
                let
                    (* Give the tests their own mutex, independent of the reporting gate.
                       Capture failures so an exception here also fails the runner's test. *)
                    val outcome = (test (Thread.Mutex.mutex ()); NONE) handle exn => SOME exn
                in
                    (* Prevent another interrupt from disrupting result publication,
                       regardless of the settings left behind by a failed test. *)
                    Thread.Thread.setAttributes
                        [InterruptState InterruptDefer, EnableBroadcastInterrupt false];
                    Thread.Mutex.lock gate;
                    (* Publish before signalling so the runner can observe completion
                       even if it has not started waiting yet. *)
                    result := SOME outcome;
                    Thread.ConditionVar.signal changed;
                    Thread.Mutex.unlock gate
                end
            (* Start with explicit attributes rather than depending on runner settings. *)
            val _ = Thread.Thread.fork
                (worker, [InterruptState InterruptSynch, EnableBroadcastInterrupt false])
            (* workerTimeout is derived from blockingTimeout, allowing the inner wait
               to time out and report its failure before the runner gives up waiting. *)
            val deadline = Time.+ (Time.now (), workerTimeout)
            (* The result is the completion condition; always recheck it after waking. *)
            fun await () =
                case !result of
                    SOME outcome => outcome
                  | NONE =>
                        if Time.>= (Time.now (), deadline)
                        then raise Fail "Thread protection worker timeout"
                        else (Thread.ConditionVar.waitUntil (changed, gate, deadline); await ())
            (* Waiting releases gate so the worker can publish. Release it on failure
               too, so a timeout or runner interrupt cannot strand the worker on gate. *)
            val () = Thread.Mutex.lock gate
            val outcome = await ()
                handle exn => (Thread.Mutex.unlock gate; PolyML.Exception.reraise exn)
            val () = Thread.Mutex.unlock gate
        in
            (* Preserve the worker's exception location when reporting it to the runner. *)
            case outcome of NONE => () | SOME exn => PolyML.Exception.reraise exn
        end
in
    val () = inWorker (fn mutex =>
        let
            (* Protection must pass arguments unchanged and return the callback's result;
               a leaked mutex would prevent subsequent operations from completing. *)
            val () =
                (check (ThreadLib.protect mutex (fn (s, n) => (n + 1, s ^ "!"))
                    ("value", 5) = (6, "value!")); checkUnlocked mutex)

            (* The polymorphic interface must also accept functions as arguments. *)
            val () = check (ThreadLib.protect mutex (fn f => f true) Bool.not = false)

            (* Masking must be temporary and reraising must preserve diagnostics on both
               exit paths, for every interrupt mode and broadcast setting. *)
            val () = List.app (fn state =>
                (attributes mutex (state, false); attributes mutex (state, true)))
                [InterruptDefer, InterruptSynch, InterruptAsynch, InterruptAsynchOnce]

            (* Pending interrupts must be delivered at the appropriate point; one-shot
               asynchronous delivery must leave the thread handling interrupts synchronously. *)
            val () = List.app (fn state => pending mutex state false)
                [InterruptDefer, InterruptSynch, InterruptAsynch, InterruptAsynchOnce]

            (* Callbacks may acquire other library locks, so protection must support nesting. *)
            val () =
                (check (ThreadLib.protect mutex
                    (ThreadLib.protect (Thread.Mutex.mutex ()) (fn n => n + 1)) 8 = 9);
                 checkUnlocked mutex)

            (* Restoring asynchronous handling can interrupt cleanup of an earlier exception;
               the mutex must be released exactly once even when that happens. *)
            val () = List.app (fn state => pending mutex state true)
                [InterruptAsynch, InterruptAsynchOnce]

            (* A callback interrupted at a blocking wait must release its locks so later users
               can proceed. A bounded wait ensures a lost interrupt fails rather than hangs. *)
            val () = blockingInterrupt mutex
        in
            ()
        end)
end;
