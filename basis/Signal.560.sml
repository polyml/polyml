(*
    Title:      Signal structure and signature.
    Author:     David Matthews
    Copyright   David Matthews 2000, 2008


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
signature SIGNAL =
sig
    datatype sig_handle = SIG_DFL | SIG_IGN | SIG_HANDLE of int->unit
    val signal: int * sig_handle -> sig_handle
end;

structure Signal: SIGNAL =
struct
    datatype sig_handle = SIG_DFL | SIG_IGN | SIG_HANDLE of int->unit
    local
        val doSig = RunCall.run_call2 RuntimeCalls.POLY_SYS_signal_handler
    in
        fun signal(s, cmd) =
        let
            val c =
                case cmd of
                    SIG_DFL => 0
                |   SIG_IGN => 1
                |   SIG_HANDLE f => RunCall.unsafeCast f
        in
            case doSig(0, (s, c)) of
                0 => SIG_DFL
            |   1 => SIG_IGN
            |   f => SIG_HANDLE(RunCall.unsafeCast f)
        end
    end
    
    local
        datatype sigHandle = SigHandle of (int->unit) * int | WeakMarker
        val doSig = RunCall.run_call2 RuntimeCalls.POLY_SYS_signal_handler
        open Thread

        fun sigThread(): unit =
        let
            (* This call to the RTS returns either a pair of a signal
               and a handler or a flag indicating that some wek reference
               has been set to NONE.  These aren't logically related but
               it's convenient to use a single thread for both. *)
            val nextSig: sigHandle = doSig(1, ())

            (* When we get a WeakMarker message we need to broadcast
               on this condition variable. *)
            fun broadCastWeak haveLock () =
            (
                if haveLock then () else Mutex.lock Weak.weakLock;
                ConditionVar.broadcast Weak.weakSignal;
                Mutex.unlock Weak.weakLock
            )
                
        in
            case nextSig of
                SigHandle (handler, signal) => (handler signal handle _ => ())
            |   WeakMarker =>
                    (* If the lock is free we can do the broadcast now but
                       to avoid waiting and being unable to handle any signals
                       we fork off a thread if we can't. *)
                    if Mutex.trylock Weak.weakLock
                    then broadCastWeak true ()
                    else (Thread.fork(broadCastWeak false, []); ());
            sigThread() (* Forever. *)
        end
        
        fun forkThread() =
            (Thread.fork(sigThread, []); ()) handle Thread _ => print "Unable to create signal thread\n"

    in
        (* Run this thread now and also start one each time we start up. *)
        val _ = forkThread()
        val _ = PolyML.onEntry forkThread
    end
end;
