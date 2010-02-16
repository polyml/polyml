(* Bug in handling of polymorphic values.  This produced an internal exception. *)

local
    val q = op ::
    
    datatype s = S 
in
    fun p s = q (S, s)
end;


functor F() = struct datatype s = S end;

local
    val q = op ::
    structure R = F()
    open R
in
    fun p s = q (S, s)
end;
