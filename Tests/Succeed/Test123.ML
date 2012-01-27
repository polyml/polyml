(* The first part gives an internal error. *)
local
    fun f {data, ...} = ()

    fun g {base = {rows, cols, ...}, row:'a} = ()

    fun h (reg as {base, row:int, ...}) =
        let 
            val _ = f base (* Eventually, fixes base as {rows, cols, data} *)
            val _ = g reg (* Fixes reg as {base, row} *)
        in
            ()
        end
    
    val tt : { rows: int, cols: int, data: string }  -> unit = f
in
end;

(* This part reports an Empty exception. *)

structure Vector2 :> sig
    type 'a vector
    val sub: 'a vector * int * int -> 'a
end =
struct
(*    open IterateX*)

   fun badArg _ = raise Fail "bad"
   fun repeat f cnt init = let 
          fun iter (n,v) = if n = cnt then v else iter(n+1,f(n,v))
          in
            if cnt < 0 
              then badArg ("repeat","count < 0")
              else iter (0,init)
          end
 

    type 'a vector =
         { rows: int
         , cols: int
         , data: 'a Vector.vector
         }
    datatype traversal = RowMajor | ColMajor

    fun tabulate tr (r, c, f) = raise Size
 
    fun sub (v as {cols=c, data=d,...}, i, j) = raise Size (*d // (i * c + j)*)

   fun dimensions ({rows=r, cols=c,...}) = (r, c)

     fun getRegion {base, row, col, nrows, ncols} =
    let
        val (lRows, lCols) = dimensions base
    in
        raise Subscript
    end

    fun mapi tr f (reg as {base, row, col, ...}) =
        let val (nrows, ncols) = getRegion reg
            fun select (i, j) = 
                let val (i', j') = (i + row, j + col)
                in (i', j', sub (base, i', j')) end
        in tabulate tr (nrows, ncols, f o select) end

    fun foldi tr f x (reg as {base, row, col, ...}) =
        let val (nrows, ncols) = getRegion reg
            fun foldval (i, z) =
                let val (j, k) =
                        case tr
                         of RowMajor => (row + i div ncols, col + i mod ncols)
                          | ColMajor => (row + i mod nrows, col + i div nrows)
                in f (j, k, sub (base, j, k), z) end
        in repeat foldval (nrows * ncols) x end

end;
