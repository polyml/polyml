(* Finalisers.  This is based on the MLton code and was
   originally contributed by Phil Clayton (phil DOT clayton AT lineone DOT net *)

(* The signature FINALIZABLE is the same as MLTON_FINALIZABLE.  See
 * https://github.com/MLton/mlton/blob/master/basis-library/mlton/finalizable.sig
 *
 * The structure Finalizable is based on MLtonFinalizable.  See
 * https://github.com/MLton/mlton/blob/master/basis-library/mlton/finalizable.sml
 *
 *)

signature FINALIZABLE =
  sig
    type 'a t
    val new : 'a -> 'a t
    val addFinalizer : 'a t * ('a -> unit) -> unit
    val finalizeBefore : 'a t * 'b t -> unit
    val touch : 'a t -> unit
    val withValue : 'a t * ('a -> 'b) -> 'b
  end

structure Finalizable :> FINALIZABLE =
  struct
    datatype 'a t =
      T of {
        value : 'a ref,
        finalizers : ('a -> unit) list ref,
        afters : (unit -> unit) list ref
      }

    (* `touch (T {value, ...})` is an operation that requires `value` but
     * does nothing. *)
    fun touch (T {value, ...}) = RunCall.run_call1 RuntimeCalls.POLY_SYS_touch_final value: unit

    fun withValue (t as T {value, ...}, f) =
      f (!value) handle e => (touch t; raise e)

    fun addFinalizer (T {finalizers, ...}, f) =
      finalizers := f :: !finalizers

    fun finalizeBefore (T {afters, ...}, t) =
      afters := (fn () => touch t) :: !afters


    type pending = {isAlive : unit -> bool, runFinalizers : unit -> unit}

    local
      (* global state for finalizables that have not been finalized. *)
      (* Use a no-overwrite reference here so that it is not reset when
         SaveState.loadState is called. *)
      val pendingList : pending list ref = 
        LibrarySupport.noOverwriteRef []

      fun update f x =
        let
          val (y, pendingList') = f (x, !pendingList)
        in
          pendingList := pendingList';
          y
        end

      val mutex = Thread.Mutex.mutex ()
    in
      fun updatePendingList f = ThreadLib.protect mutex (update f)
    end


    fun add (p, ps) = ((), p :: ps)

    fun clean ((), ps) =
      foldl
        (
          fn (p as {isAlive, runFinalizers}, (runNowFns, ps)) =>
            if isAlive ()
            then (runNowFns, p :: ps)
            else (runFinalizers :: runNowFns, ps)
        )
        ([], [])
        ps

    fun swap (a, b) = (b, a)


    local
      fun threadFn () = (
        Thread.Mutex.lock Weak.weakLock;
        while true do (
          app (fn f => f ()) (updatePendingList clean ());
          Thread.ConditionVar.wait (Weak.weakSignal, Weak.weakLock)
        )
      )


        fun cleanAtExit () =
        let
            fun loop ps =
              let
                val () = PolyML.fullGC ()  (* PolyML.fullGC is synchronous *)
                val (runNowFns, ps') = clean ((), ps)
              in
                app (fn f => f ()) runNowFns;
                if not (null runNowFns)
                then loop ps'
                else ()
              end

            (* Empty the pending list so that the cleaning thread does not
             * start running finalizers too. *)
            val ps = updatePendingList swap []
        in
            loop ps
        end

        fun startUp () = (Thread.Thread.fork (threadFn, []); OS.Process.atExit cleanAtExit)
    in
        val () = PolyML.onEntry startUp; (* For future sessions *)
        val () = startUp() (* For this session *)
    end

    fun new (v : 'a) : 'a t =
      let
        val afters = ref []
        val finalizers = ref []
        val value = ref v
        val t =
          T {
            afters = afters,
            finalizers = finalizers,
            value = value
          }
        val weak = Weak.weak (SOME value)
        fun isAlive () = isSome (!weak)
        fun runFinalizers () = (
          List.app (fn f => f v) (!finalizers);
          List.app (fn f => f ()) (!afters)
        )
        val pending = {isAlive = isAlive, runFinalizers = runFinalizers}

        val () = updatePendingList add pending
      in
        t
      end
  end;
