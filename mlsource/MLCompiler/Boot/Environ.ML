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

structure Environ =
struct

  local
    open Misc ;
    open Universal ;
  in
    abstype environ = 
      Environ of
        {
          enter:  string -> universal -> unit,
          delete: string -> tag -> unit,
          lookup: string -> tag -> universal,
          over:   (string * universal) iter
        }
    with
      val environIn = Environ ;
      
      fun environOut (Environ E) = E ;
    
    end (* environ *) ;
  
  end (* local *) ;
  
end (* Environ *) ;


(*
LET environ:
{  Acts as a container for the set of operations
   which actually do the entry and looking up. Uses a sorting routine to sort
   the names when printing out an environment. }
   TYPE (environ)
   name_val_pair:
      TYPE (nvp)
      name: PROC(nvp)string;
      val: PROC(nvp)universal;
      constr: PROC(string; universal)nvp
      END;
   in: PROC(
   	TYPE
        enter: PROC(string; universal);
        lookup: PROC(string; universal$utag)universal;
	delete: PROC(string; universal$utag);
	over: TYPE (iter)
	   init: PROC()iter;
	   continue: PROC(iter)boolean;
	   value: PROC(iter)name_val_pair;
	   next: PROC(iter)iter
	   END
	END)environ;
   out: PROC(environ)
          TYPE
          enter: PROC(string; universal);
          lookup: PROC(string; universal$utag)universal;
	  delete: PROC(string; universal$utag);
	  over: TYPE (iter)
	     init: PROC()iter;
	     continue: PROC(iter)boolean;
	     value: PROC(iter)name_val_pair;
	     next: PROC(iter)iter
	     END
	  END;
   enter: PROC(environ; string; universal);
   lookup: PROC(environ; string; universal$utag)universal;
   delete: PROC(environ; string; universal$utag);
   print: PROC(environ)
   END
==
   TYPE (environ)

   LET name_val_pair == RECORD(name: string; val: universal);

   EXTENDS
      RECORD(out:
	TYPE
        enter: PROC(string; universal);
        lookup: PROC(string; universal$utag)universal;
	delete: PROC(string; universal$utag);
	over: TYPE (iter)
	   init: PROC()iter;
	   continue: PROC(iter)boolean;
	   value: PROC(iter)name_val_pair;
	   next: PROC(iter)iter
	   END
	END);

    LET in == environ$constr;

    LET enter ==
    PROC(env: environ; name: string; dec: universal) . 
       env.out$enter(name, dec);

    LET lookup ==
    PROC(env: environ; name: string; u: universal$utag)universal .
       env.out$lookup(name, u);

    LET delete ==
    PROC(env: environ; name: string; u: universal$utag) .
       env.out$delete(name, u);

    LET print == { Print any declaration values only. }
    PROC(x: environ)
       BEGIN
       LET word_list == list string;
       LET longest == new 1;
       LET Head == new word_list$nil;
       for((environ$out x)$over,
	 PROC(u: environ$name_val_pair) .
	    IF decl$is u.val
	    THEN
	       BEGIN
	       LET name == u.name;
	       IF name.length > @longest THEN longest := name.length;
	       Head := name :: @Head
	       END);
       Head := quick_sort(@Head);
       { Work out number of columns so that there is room for the longest word
         and at least one space between. }
       LET cols == succ 72 { Width of screen } div succ(@longest);
       LET colno == new 1;
       for(over_list(@Head),
	 PROC(name: string)
	    BEGIN
	    print(name);
	    colno +:= 1;
	    IF @colno > cols
	    THEN ( print('\n'); colno := 1 )
	    ELSE for(upto(name.length, @longest), PROC(integer) (print(' ')));
	    END
          );
       IF @colno <> 1 THEN print('\n')
       END { print }
    END { environ };

*)
