(* On X86/64 these resulted in InternalError exceptions because the
   offset was too large to fit into a 32-bit integer. *)

val a = fn
    ~1 => "A"
  | ~2 => "B"
  | ~3 => "C"
  | ~4 => "D"
  | ~5 => "E"
  | ~6 => "F"
  | ~7 => "G"
  | ~8 => "H"
  | _  => ""
;

if a ~2 = "B" andalso a 0 = ""
then ()
else raise Fail "error";

val e = fn
	0wx7FFFFFFF => "A"
  | 0wx7FFFFFFE => "B"
  | 0wx7FFFFFFD => "C"
  | 0wx7FFFFFFC => "D"
  | 0wx7FFFFFFB => "E"
  | 0wx7FFFFFFA => "F"
  | 0wx7FFFFFF9 => "G"
  | 0wx7FFFFFF8 => "H"
  | _  => "";


if e 0wx7FFFFFFC = "D" andalso e 0w0 = ""
then ()
else raise Fail "error";

