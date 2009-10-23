(* This caused a segmentation fault. *)
datatype t =
                S
        |       B of (int * t) list;
B [(1, S)] = B [(1, S)];
