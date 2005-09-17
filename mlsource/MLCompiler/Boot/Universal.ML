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

structure Universal :

sig
  type universal
  type 'a tagMethods
  
  val makeTag : unit -> '_a tagMethods
  
  val tagIs      : 'a tagMethods -> universal -> bool
  val tagInject  : 'a tagMethods -> 'a -> universal
  val tagProject : 'a tagMethods -> universal -> 'a
  
end =


struct

  abstype universal = Universal of exn
  with
    type 'a tagMethods =
      { 
        is:      universal -> bool,
        inject: 'a -> universal,
        project: universal -> 'a
      };
   
    fun makeTag () : '_a tagMethods =
    let
      exception E of '_a ;
    in
      { 
        inject  = fn x => Universal (E x),
        project = fn (Universal (E x)) => x    | _ => raise Match,
        is      = fn (Universal (E x)) => true | _ => false
      }
    end ;

    val tagIs      : 'a tagMethods -> universal -> bool  = #is
    val tagInject  : 'a tagMethods -> 'a -> universal    = #inject
    val tagProject : 'a tagMethods -> universal -> 'a    = #project
  end ;
  
end;

(*
This code will test the above structure


datatype t = T of int ;
datatype x = X of string ;

val {is=ist,inject=injectT:t->universal,project=projectT} = makeTag();
val {is=isx,inject=injectX:x->universal,project=projectX} = makeTag();

val a = injectT (T 42) ;
val b = injectT (T 24) ;
val c = injectX (X "hello") ;
val d = injectX (X "mike") ;

map ist [a,b,c,d] ;
map isx [a,b,c,d] ;

projectT a ;
projectT b ;
projectT c ;
projectT d ;

projectX a ;
projectX b ;
projectX c ;
projectX d ;
*)

