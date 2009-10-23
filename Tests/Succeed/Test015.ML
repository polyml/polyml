(* Bug report from Sebastian Skalberg.  There was a problem with the code
   added in version 4.1.2 to handle flexible labelled records. *)

structure VHI =
  struct

  fun update_typs   {typs,thy} typs'   = {typs=typs',thy=thy }

  fun translate_type args = #thy args

  fun parse_usertype pos = translate_type

  fun parse_internaltype args = #thy args

  fun parse_expr inner pos args = translate_type args
    | parse_expr inner pos args = parse_internaltype args

  fun parse_formal args =
      update_typs args (parse_usertype "var" args)

  fun parse_index args =
      update_typs args (parse_expr false "index" args)

  end;
