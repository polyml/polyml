(* This caused the intermediate code optimiser to blow up. *)

fun I x = x

infixr &
datatype ('a, 'b) pair = op & of 'a * 'b

infixr 1 &&&>
fun (f &&&> g) h (a & b) = f (fn a' => g (fn b' => h (a' & b')) b) a
;

      (
        I
         &&&> I
         &&&> I
         &&&> I
         &&&> I
         &&&> I
         &&&> I
         &&&> I
         &&&> I
         &&&> I
         &&&> I
         &&&> I
         &&&> I
         &&&> I
         &&&> I
         &&&> I
         &&&> I
      )
;
