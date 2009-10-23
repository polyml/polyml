(* Three different bugs to do with where-type constraints. *)


signature EXPORTTREESIG =
sig
    (* Export tree. *)
    type ptProperties 
    include sig type exportTree end where type exportTree = int * ptProperties list
end;

functor INITIALISE_ (

    structure VALUEOPS : sig
        type location
        end where type location = int;

    structure EXPORTTREE: EXPORTTREESIG

    structure MAKE :
    sig
        type exportTree
    end where type exportTree = EXPORTTREE.exportTree;

) =
struct end;

(* I think I've identified these. *)

functor INITIALISE_ (

structure EXPORTTREE: sig
    type exportTree
end where type exportTree = int

structure MAKE :
sig
    type exportTree
end where type exportTree = EXPORTTREE.exportTree;


) = struct end;

(* I think I've identified this one. *)

signature EXPORTTREESIG =
sig
    type ptProperties
    include sig type location end where     type location =
        { file: string, startLine: int, startPosition: int, endLine: int, endPosition: int }
    
    include sig type exportTree end where type exportTree = location * ptProperties list
    
    include sig type navigation end where type navigation =
        {parent: (unit -> exportTree) option,
         next: (unit -> exportTree) option,
         previous: (unit -> exportTree) option}

end;
functor F(structure E: EXPORTTREESIG) = struct end;


