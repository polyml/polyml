(* This gives a Fixed Record error but should not give an
   internal error. *)
(* Test fom Lars Hupel - Bug ticket 16. *)

type tracer = {
  enabled: bool,
  parent: int
}

type trace_data = {
  ctxt: bool,
  tracer: tracer
}

fun with_parent {enabled, parent} =
  {enabled = enabled, parent = 0}

fun disable_trace {parent = parent, ...} =
  {enabled = false, parent = 0}

fun generate_result {ctxt, tracer = {enabled, parent}} =
  0 > ctxt

fun ask data =
  let
    val {ctxt, ...} = data
  in
    case generate_result data of
      true =>
        with_parent ctxt
  end;
