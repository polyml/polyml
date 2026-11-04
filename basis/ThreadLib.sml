(*
    Title:      Thread library
    Author:     David C. J. Matthews
    Copyright (c) 2007-2014, 2018, 2020

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

structure ThreadLibSupport :>
sig
    type state
    val preprotect : Thread.Mutex.mutex -> state
    val postprotect : Thread.Mutex.mutex -> state -> unit
    val failed : Thread.Mutex.mutex -> state -> exn -> 'a
end =
struct
    type state = Word.word

    (* Consume the saved state on the same thread, with the same mutex.
       Cleanup must execute exactly once. *)
    fun preprotect m =
    let
        open Word
        (* Set this to handle interrupts synchronously except if we are blocking
           them.  We don't want to get an asynchronous interrupt while we are
           actually locking or unlocking the mutex but if we have to block to do
           IO then we should allow an interrupt at that point. *)
        val oldAttrs : Word.word = RunCall.loadWord (Thread.Thread.self (), 0w1)
        val () =
            if andb (oldAttrs, 0w6) = 0w0 (* Already deferred? *)
            then ()
            else RunCall.storeWord (Thread.Thread.self (), 0w1,
                    orb (andb (notb 0w6, oldAttrs), 0w2))
        val () = Thread.Mutex.lock m
    in
        oldAttrs
    end

    fun postprotect m oldAttrs =
    (
        Thread.Mutex.unlock m;
        (* Restore the original interrupt mode and deliver pending asynchronous
           interrupts only after releasing the mutex. *)
        RunCall.storeWord (Thread.Thread.self (), 0w1, oldAttrs);
        if Word.andb (oldAttrs, 0w4) = 0w4 then Thread.Thread.testInterrupt () else ()
    )

    fun failed m saved exn =
    (
        postprotect m saved;
        (* Reraise the exception preserving the location information. *)
        PolyML.Exception.reraise exn
    )
end;

structure ThreadLib:
sig
    val protect: Thread.Mutex.mutex -> ('a -> 'b) -> 'a -> 'b
end =
struct
    (* This applies a function while a mutex is being held. 
       Although this can be defined in terms of Thread.Thread.getAttributes it's
       defined here using the underlying calls.  The original version with
       getAttributes appeared as a major allocation hot-spot when building the
       compiler because "protect" is called round every access to the global
       name-space. *)
    fun protect m f a =
    let
        val saved = ThreadLibSupport.preprotect m
        val result = f a
            handle exn => ThreadLibSupport.failed m saved exn
    in
        ThreadLibSupport.postprotect m saved;
        result
    end
end;
