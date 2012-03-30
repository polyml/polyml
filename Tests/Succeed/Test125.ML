(* The Standard ML syntax allows "op" in an exception binding.
   Poly/ML wasn't allowing it. *)
infix & %;
exception op &;
exception op % = op &;
