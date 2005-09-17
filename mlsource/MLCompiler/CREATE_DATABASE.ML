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


functor CREATE_DATABASE

(

(*****************************************************************************)
(*                  UNIVERSAL                                                *)
(*****************************************************************************)
structure UNIVERSAL :
sig
  type universal
  type 'a tagMethods
  
  val makeTag : unit -> '_a tagMethods
  
  val tagIs      : 'a tagMethods -> universal -> bool
  val tagInject  : 'a tagMethods -> 'a -> universal
  val tagProject : 'a tagMethods -> universal -> 'a
end 

(*****************************************************************************)
(*                  RUNCALL                                                  *)
(*****************************************************************************)
structure RUNCALL :
sig
  val run_call0 : int -> unit -> 'a
  val run_call1 : int -> 'a -> 'b
  val run_call3 : int -> ('a * 'b * 'c) -> 'd
end 

) :

(*****************************************************************************)
(*                  CREATEDATABASE export signature                          *)
(*****************************************************************************)
sig
  type 'a dbRef
  val dbRefCreate : '_a -> '_a dbRef
  val dbRefPut    : 'a dbRef * 'a -> unit
  val dbRefGet    : 'a dbRef -> 'a

  type 'a dbList
  val dbListCreate : (unit -> '_a) * '_a list -> '_a dbList
  val dbListToList :  'a dbList -> 'a list

  val createDatabase : string * (unit -> unit) * (unit -> bool) -> unit
  
  val resetSpecifics : unit -> unit
end =

(*****************************************************************************)
(*                  CREATEDATABASE functor body                              *)
(*****************************************************************************)
struct
  open RuntimeCalls; (* for POLY_SYS numbers *)
  (* 
     We need to make the database entry a list of universals,
     because Poly expects this, rather than a list of
     (universal * (universal -> universal)) pairs. This is why we
     need the extra "specificTag" overhead. Eventually, I'll
     get rid of this. SPF 6/10/94
     
     This code it tricky because:
     
       (1) We want to apply the inherit function  at the time
           we create the child database. This means that the
           database list must contain both the db-specific value
           and the inherit function.
           
       (2) What would happen if we created a "specific" AFTER
           we created the child database, then referenced it in the
           child? We have to create an entry for it on-the-fly.
           This means that the specific itself must contain some
           initialisation information.
           
           This doesn't really work properly for multiple levels
           of inheritance, unfortunately, because we only have
           ONE initial value.
   *)

   local
     open UNIVERSAL;
     val setDbentry : universal list -> unit =
       RUNCALL.run_call1 POLY_SYS_set_dbentry;
        
     val getDbentry : unit -> universal list =
       RUNCALL.run_call0 POLY_SYS_get_dbentry;
     
     type 'a foo = 'a * ('a -> 'a)
   in  
     abstype 'a specific = Specific of ('a tagMethods * 'a foo)
     with
       val specificTag : universal foo tagMethods = makeTag ();
     
       fun resetSpecifics () = setDbentry [];
     
       (* storeTaggedPair is only called with tags that are known
          to be not in the db-specific list. *)
       fun storeTaggedPair (tag: 'a tagMethods, (x: 'a, f: 'a -> 'a)) : unit = 
       let
         val init     = tagInject tag x;
         fun reinit u = tagInject tag (f (tagProject tag u));
         val entry    = tagInject specificTag (init, reinit)
       in
         setDbentry (entry :: getDbentry ())
       end;
     
       fun newSpecific (p : '_a foo) : '_a specific =
       let
         val tag : '_a tagMethods = makeTag ();
         val U : unit = storeTaggedPair (tag, p)
       in
         Specific (tag,p)
       end;
       
       fun getSpecific (Specific (tag, p) : 'a specific) : 'a =
       let
         fun findEntry (h :: t) =
           (tagProject tag (#1 (tagProject specificTag h))
             handle Match => findEntry t)
             
         (* Make a new one and add it to the table for this database *)
         | findEntry [] = 
         let
           val U : unit = storeTaggedPair (tag, p);
         in
           #1 p
         end;
       in
         findEntry (getDbentry ())
       end;
       
       fun createDatabase
            (name : string,
             rootShell : unit -> unit, 
             systemCommit : unit -> bool) : unit =
       let
         val parentDbentry = getDbentry ();
         
         fun newRoot () =
         let
           (* First call commit - this is only done on first entry to the child
              database. It does not actually save the database but instead
              sets a flag (in the run-time system) saying that commit has been
              called. The run-time system ignores the first call to "commit"
              because a Poly/ML session normally starts with a process
              in the database calling "commit" (with a bit of luck, it's
              the same process that saved the database image by calling
              "commit" in the previous session). We have to fake this here
              with a dummy call to "commit" instead. *)
           val dummy : bool = systemCommit ();

           (* "inherit" must be executed in child database *)
	   fun inherit n [] =
	     let
	       val s = "Initialised " ^ Int.toString n ^ " db-specific values\n"
	       val U : unit = TextIO.print s; 
             in
               []
             end

	   | inherit n (h :: t) =
	     if tagIs specificTag h
	     then let
	       val (x,f) = tagProject specificTag h
	     in
	       tagInject specificTag (f x, f) :: inherit (n + 1) t
	     end
	     else inherit n t;
	     

           (* next initialise the db-specific values *)
           val childDbentry = inherit 0 parentDbentry;
           val U : unit     = setDbentry childDbentry;
         in
           rootShell ()
         end;

       in
         (* We have to commit here to get everything out of the
            heap, so creating the child database doesn't "steal"
            all our useful data. There ought to be a better way
            to do this (a change to the RTS?) but I can't think of
            it now. SPF 31/5/95 *)
         (* The RTS has now been bugfixed, so we can get rid of the
            unpleasant commit into the parent database. SPF 15/6/95 *)
         RUNCALL.run_call3 POLY_SYS_createf (name, newRoot, parentDbentry)
       end;
     end (* abstype *)
   end;  (* local *)
     
(*****************************************************************************)
(*                  dbRef type (ref initialised to inherited value)          *)
(*****************************************************************************)
   abstype 'a dbRef = DbRef of 'a ref specific
   with
     fun dbRefCreate v         = DbRef (newSpecific (ref v, fn r => ref (!r)))
     fun dbRefPut (DbRef r, v) = getSpecific r := v;
     fun dbRefGet (DbRef r)    = ! (getSpecific r)
   end;

(*****************************************************************************)
(*                  dbList type (one entry for each database level)          *)
(*****************************************************************************)
   abstype 'a dbList = DbList of 'a list specific
   with
     fun dbListToList (DbList l)    = getSpecific l;
     fun dbListCreate (new,initial) = 
       DbList (newSpecific (initial, fn t => new () :: t))
   end;
end;


