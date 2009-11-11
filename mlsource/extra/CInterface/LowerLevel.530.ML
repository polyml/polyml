(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited

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

structure LowerLevel =
struct
    open Volatile;
    open Volatile.BehaviourRefs;
    open ForeignException;
    
    (**********************************************************************
    An 'a promise represents a suspended computation of type 'a.  A promise
    is created by passing an 'a thunk (a function from unit to 'a) to
    makePromise.
    
    A promise is activated using forcePromise.  When a promise is forced,
    the result is remembered for subsequent force.  This ensures that the
    thunk passed to makePromise will be evaluated at most once.
    
    For example, evaluation of the following would cause the message,
    "computing..." to be echoed just once:
    
        fun echo s = output (std_out,s);
        let val P = makePromise (fn () => (echo "computing...\n"; 2 + 2))
        in forcePromise P + forcePromise P
        end;
    **********************************************************************)    
    
        datatype ('a,'b) either = This of 'a | That of 'b
        datatype 'a promise = Promise of (unit -> 'a, 'a) either ref
    
        fun makePromise thunk = Promise (ref (This thunk));
    
        fun forcePromise (Promise thingref) =
        case (!thingref) of
            This thunk => let val res = thunk()
                  in (thingref := That res; res)
                  end
          | That res => res;
    
    (**********************************************************************
    An 'a inert represents a re-creatable value of type 'a.  Recreation is
    performed when a given exception is raised during the subsequent use
    of the inert value.  This is useful for objects which do not survive
    from one PolyML session to the next, e.g. values of type "vol".
    
    > makeInert CATCH BUILD
    
    Create an inert value by forcing the thunk BUILD. Use CATCH as a
    predicate on exception values.  It must return true for any exception
    which indicates recreation of this inert value is necessary.
    Recreation will be achieved by re-forcing BUILD.
    
    > withInert INERT F
    
    Apply the function F to the value encapsulated within INERT.
    If during the course of this application recreation is necessary
    then perform that recreation.
    **********************************************************************)
    
        datatype 'a inert =
        Inert of 'a ref * (unit -> 'a) * (exn -> bool)
    
        fun makeInert catch build =
        Inert(ref(build()),
              build,
              catch);
    
        fun withInert (Inert(value,build,catch)) f =
        f (!value)
        handle e => if catch e then (value := build(); f (!value))
                       else raise e;
    
    (**********************************************************************
     * Debugging...
     **********************************************************************)
    
    fun inform str =
        (******
         * Output a message if "inform_about_symbol_reload" ref is set.
         ******)
        if !inform_about_symbol_reload then TextIO.print str else ();
    
    (**********************************************************************
     * Combination of Inert & Promises...
     **********************************************************************)
       
    fun makeIP catch build = makePromise (fn () => makeInert catch build);
    fun withIP ip f        = withInert (forcePromise ip) f
    
    (**********************************************************************
     * Implementation of dylib & sym.
     **********************************************************************)
        
    datatype dylib = Dylib of string ref * vol inert promise;
    datatype sym = Sym of vol inert promise;
        
    fun withDylib (Dylib(_,ip)) f   = withIP ip f;
    fun withSym (Sym ip) f          = withIP ip f;
    
    fun get_libPath (Dylib(pathRef,_))      = !pathRef;
    fun set_libPath (Dylib(pathRef,_)) path =  pathRef := path;
    
        
    val catchIV =
        (******
         * Returns "true" when given an `invalid volatile' exception.
         * Note: this string must *exactly* match the corresponding
         * string found in the runtime system file: .../Driver/foreign.c
         ******)
        fn Foreign "Invalid volatile" => true | _ => false;
    
      
    fun load_lib path =
        (******
         * Load a dynamic library.
         * This loading is done lazily, i.e. not until the library is needed.
         * The library will automatically reload in subsequent PolyML sessions
         * using the latest pathname set by set_libPath above.
         ******)
        let val pathRef = ref path
        in Dylib(pathRef,
             makeIP catchIV (fn () =>
             (inform ("Loading library: "^(!pathRef)^"\n");
              Volatile.load_lib (!pathRef))))
        end;
    
       
    fun load_sym dylib symName =
        (******
         * Load a symbol from a dynamic library.
         * This loading is done lazily. Neither the library or the symbol
         * will be loaded until the symbol is actually needed.
         * The symbol will be reloaded in subsequent PolyML sessions.
         ******)
        Sym(makeIP catchIV (fn () =>
            withDylib dylib (fn dylibVol =>
        (inform ("Loading symbol: "^symName^"\n");
         Volatile.load_sym dylibVol symName))));
    
    
    fun call_sym_and_convert sym args retType =
        (******
         * Call a symbol as a C-function.
         * May load/reload either the symbol, or both the symbol and the library.
         ******)
        withSym sym (fn symVol =>
        Volatile.call_sym_and_convert symVol args retType);

    (* Set a finalizer for a vol. *)
    fun setFinal sym vol =
        withSym sym (fn symVol => Volatile.setFinal symVol vol)
    
    
    fun volOfSym sym =
        (******
         * Extract a *valid* volatile from a symbol
         * May load/reload either the symbol, or both the symbol and the library.
         *
         * The reload is caused because we attempt to take the address
         * of the vol which will raise an exception if the vol is invalid.
         ******)
        withSym sym (fn symVol => (Volatile.address symVol;
                       symVol));
        
    
    fun mapSym f sym =
        (******
         * Change order of params for export...
         ******)
        withSym sym f;
    
        
    fun get_sym libPath (*symName*) =
        (******
         * Common combination of load_lib & load_sym.
         * Partial application will cause the library to be shared, and
         * hence only loaded/reloaded once per session.
         ******)
        load_sym (load_lib libPath) (*symName*) ;
    

    open Union; 
        
    (* now imported from Union ...
       exception Never of string;
    ... *)
    
    fun never string = raise Never string;
    
    (* Define call_sym in terms of call_sym_and_convert *)

    fun call_sym sym args retCtype =
    let
      val result = 
        call_sym_and_convert
          sym 
          (map (fn x => In (Vol x)) args)
          (chooseVol retCtype)
    in
      case result of
        (v,[]) => if isVol v then deVol v else never "call_sym"
      | _      => never "call_sym"
    end


    local
        fun prettyDylib _ _ (_: dylib) = PolyML.PrettyString "?"
        fun prettySym _ _ (_: sym) = PolyML.PrettyString "?"
    in
        val () = PolyML.addPrettyPrinter prettyDylib
        and () = PolyML.addPrettyPrinter prettySym
    end


end;
