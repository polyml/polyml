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

structure UniversalTable :
sig
  type universal;
  type 'a tag;
  type univTable;
  type 'a iter;
  
  val makeUnivTable:    unit -> univTable;
  
  val univEnter:        univTable * 'a tag * string * 'a -> unit;
  val univLookup:       univTable * 'a tag * string -> 'a option;
  val univDelete:       univTable * 'a tag * string -> unit;
  
  val univOver:         univTable -> (string * universal) iter;
  val univFold:         univTable * (string * universal * 'a -> 'a) * 'a -> 'a;
  
  val univOverSpecific: univTable * 'a tag -> (string * 'a) iter;

  (* Freeze a mutable table so it is immutable. *)
  val univFreeze:       univTable -> univTable

end =


struct
  local
    open Misc;
    open HashTable;
    open Universal;
  in
    type universal = universal
    type 'a tag = 'a tag;
    type 'a iter = 'a iter;
  
    abstype univTable = Table of universal list hash
    with
      fun makeUnivTable() = Table (hashMake 10); (* Choose a number. *)
      
      fun univLookup (Table tab, t:'a tag, name:string) : 'a option =
      let
        fun search []     = NONE
        |   search (H::T) = if tagIs t H then SOME (tagProject t H) else search T;
      in
        case hashSub (tab, name) of
            NONE => NONE
        |   SOME t => search t
      end;
      
      fun univEnter (Table tab, t:'a tag, name:string, v:'a) : unit =
      let
        val u = tagInject t v;
        
        (* If there is already an entry we add it to the list,
           otherwise we start a new list. *)
  
        fun search []     = [u]
        |   search (H::T) = if tagIs t H then u :: T else H :: search T;
        
        val newList =
            case hashSub (tab, name) of
                SOME v => search v
            |   NONE => [u]
      in
        hashSet (tab, name, newList)
      end;
      
      fun univDelete (Table tab, t:'a tag, name:string) : unit  =
      let
        fun search []     = []
        |   search (H::T) = if tagIs t H then T else H :: search T;
      in
         case hashSub (tab, name) of
             SOME l => hashSet (tab, name, search l)
         |   NONE => ()
      end;
      
      local
        fun fst (a,_) = a;
        fun snd (_,b) = b;
      in  
        fun univOver (Table tab) = (* Iterator over all the entries. *)
        let
          fun continue (_,[]) = false
            | continue _      = true;
    
          fun value (_,[]) = 
                raise InternalError "UniversalTable.univOver.value"
            | value (i,H::_) = (fst (Value i),H);
    
          (* Get the next table entry which has something in it. *)
          fun nextList i = 
          let
            val nt = Next i;
          in
            if Continue nt (* Have we reached the end ? *)
            then
              (* No, but the entry may be empty if it has been deleted. *)
              case snd (Value nt) of
                [] => nextList nt
              | nl => (nt,nl)
            else
              (nt,[]) (* At the end *)
          end;
            
          (* Return the next FULL entry. *)
          
          fun next (_ ,[]) =     
                raise InternalError "UniversalTable.univOver.next"
            | next (ti,[_])  = makeIter (nextList ti)
                           (* Finished one list - get the next. *)
            | next (ti,_::T) = makeIter (ti,T)
          
          and makeIter i = 
            Iter
              { continue = continue i,
                next     = fn () => next i,
                value    = fn () => value i
              };
  
          val t1 = hashOver tab;
        in
          makeIter 
           (if Continue t1
            then
              case snd (Value t1) of
                [] => nextList t1
              | l1 => (t1,l1)
            else
              (t1,[]))
        end;
        
        (* Iterator over entries with a specific tag. *)
        fun univOverSpecific (tab : univTable, t:'a tag) = 
        let
          fun tagMatches i = tagIs t (snd (Value i));
          
          (* Return the next entry with the specific tag *)
          
          fun continue (n : (string * universal) iter) : bool =
            Continue n andalso
              (tagMatches n orelse continue (Next n));
          
          fun try (n : (string * universal) iter) : (string * 'a) iter =
            if Continue n
            then if tagMatches n
                 then makeIter n
                 else try (Next n)
            else makeIter n (* ??? SPF/18/8/94 *)
          
          and makeIter (i : (string * universal) iter) : (string * 'a) iter =
            Iter
              {
                continue = continue i, (* was "Continue i" (SPF 18/8/94) *)
                next     = fn () => try (Next i),
                value    = fn () => 
                  let
                    val (s,u) = Value i
                  in
                    (s, tagProject t u)
                  end
              };
        in
          try (univOver tab)
        end;
        
        fun univFold (table, f, acc) =
        let
          fun foldF i acc =
            if not (Continue i)
            then acc
            else let
              val (name,u) = Value i;
            in
              foldF (Next i) (f (name, u, acc))
            end;
        in
          foldF (univOver table) acc
        end;
      end (* local *);

    fun univFreeze (Table h) = Table(hashFreeze h)

    end (* with *);
  end (* local *);
end (* UniversalTable *);
