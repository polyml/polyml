(* Cut down version of bug reported by Phil Clayton in SVN 1180. *)

structure RedBlackDict =
struct
    type 'a maplet = string * 'a

    datatype colour = Red | Black

    datatype 'a tree =
    	Node of 'a node
    |	Tip
    withtype 'a node = 'a tree * colour * 'a maplet * 'a tree;

    datatype 'a map = Map of {t : 'a tree, bh : int};

    fun min (Map {t, ...} : 'a map) : 'a maplet = (
    	let	fun leftmost (l, _, A, _) = case l of
    			Node n	=> leftmost n
    		|	Tip	=> A

    	in	case t of
    			Node n	=> leftmost n
    		|	Tip	=> raise Empty
    	end
    )
end;
