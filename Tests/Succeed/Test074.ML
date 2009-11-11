(* this structure compiles OK *)
structure HashTable =
struct
  type hashedstring = string
  fun hash s = s
  fun unhash s = s
end;

(* This structure reports
  Exception- InternalError: incrUseCount: no entry 2 raised while compiling
*)

structure Prettyprinter =
struct

  datatype printmode = compressed | uncompressed | verbose

  fun print mode = 
  let 
     fun printid s = (case mode of 
                compressed => HashTable.hash s
              | uncompressed => HashTable.unhash s
              | verbose => HashTable.unhash s)
   in printid
  end

end;
