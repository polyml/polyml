(*
    Title:      Standard Basis Library: General Structure
    Author:     David Matthews
    Copyright   David Matthews 1999

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

(* G&R 2004 status: checked, no change. *)

signature GENERAL =
  sig
    eqtype  unit
    type  exn
    exception Bind
    exception Chr
    exception Div
    exception Domain
    exception Fail of string
    exception Match
    exception Overflow
    exception Size
    exception Span
    exception Subscript
    val exnName : exn -> string
    val exnMessage : exn -> string
    datatype order = LESS | EQUAL | GREATER
    val ! : 'a ref -> 'a
    val := : ('a ref * 'a) -> unit
    val o : (('b -> 'c) * ('a -> 'b)) -> 'a -> 'c
    val before : ('a * unit) -> 'a
    val ignore : 'a -> unit
  end;


(* We declare the values in the top-level environment and construct
   the structure afterwards rather than opening the structure.  The
   reason for this is that we would prefer that types unit and exn
   did not capture the General structure name. *)
local
	open RuntimeCalls (* for POLY_SYS and EXC numbers *)
	val System_loadw: exn*int->string  = RunCall.run_call2 POLY_SYS_load_word
in
    local
		structure Bind = RunCall.Run_exception0( val ex_iden  = EXC_Bind )
	in
		exception Bind = Bind.ex
	end
    local
		structure Div = RunCall.Run_exception0( val ex_iden  = EXC_divide )
	in
		exception Div = Div.ex
	end
    local
		structure Match = RunCall.Run_exception0( val ex_iden  = EXC_Match )
	in
		exception Match = Match.ex
	end
    local
		structure Overflow = RunCall.Run_exception0( val ex_iden  = EXC_overflow )
	in
		exception Overflow = Overflow.ex
	end
    local
		structure Subscript = RunCall.Run_exception0( val ex_iden  = EXC_subscript )
	in
		exception Subscript = Subscript.ex
	end
    local
		structure Size = RunCall.Run_exception0( val ex_iden  = EXC_size )
	in
		exception Size = Size.ex
	end

	exception Domain and Span and Chr

	(* Exception packets.  The first word is the code, a unique id; the second is
	   the exception name and the third is the exception argument. *)
	fun exnName (ex: exn) = System_loadw(ex, 1)
	
	(* exnMessage is contained in the Bootstrap.ExnMessage structure and is
       actually just the same as PolyML.makeString.  However we have to make
       sure that we don't capture the namespace that is being used to compile
       the code at this point.
       Since exception packets don't contain any description of the type of the
       exception argument if we want to be able to print it we have to find the
       type by using the unique id.  We can search all the declared exceptions
       and all the exceptions in structures to find the unique id and if we find
       it we can use the type information there.  That requires a name space and
       the best choice of name space is the one used to compile the code that
       contains the CALL to exnMessage, not the one used to declare it here. *)
    open Bootstrap.ExnMessage
    
	(* fun exnMessage (ex: exn) = PolyML.makestring ex *)
	
    datatype order = LESS | EQUAL | GREATER
	
	fun op before (a, b) = a
	fun ignore a = ()
	
	structure General (*: GENERAL *) (* Don't use a signature here. *) =
		struct
		type unit = unit (* This has to be primitive because its value is given by () *)
		type exn = exn
		exception Bind = Bind and Div = Div and Match = Match and Chr = Chr
		exception Overflow = Overflow and Domain= Domain and Fail = Fail
		exception Span = Span and Subscript = Subscript and Size = Size

		val exnName = exnName
		and op := = op := and ! = ! and op o = op o
		and op before = op before and ignore = ignore

        open Bootstrap.ExnMessage
		
	    datatype order = datatype order
	end
end

(* Although these are available unqualified we always use them
   qualified within this library so that dependencies are
   maintained. *)
