(* This violates an assumption that all structures produced by a
   core-language expression are global.  structure "T" is still
   local when "open S" is processed. *)
structure S =
struct
    structure T =
        struct
            val x = 1
        end
end
(* N.B. No semicolon in here. *)
open S;
T.x;
