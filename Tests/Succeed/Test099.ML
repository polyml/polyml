(* Tests for functions added to the CharArray and CharArraySlice structures. *)
fun check f = if f() then () else raise Fail "Wrong";

val a = CharArray.tabulate (10, fn i => Char.chr(i+ Char.ord #"0"));
check(fn () => CharArray.vector a = "0123456789");
check(fn () => CharArray.findi (fn (_, #"3") => true | _ => false) a = SOME(3, #"3"));
check(fn () => not(isSome(CharArray.findi (fn (_, #"A") => true | _ => false) a)));
check(fn () => CharArray.find (fn #"3" => true | _ => false) a = SOME #"3");
check(fn () => not(isSome(CharArray.find (fn #"A" => true | _ => false) a)));
check(fn () => CharArray.exists (fn #"3" => true | _ => false) a);
check(fn () => not(CharArray.exists (fn #"A" => true | _ => false) a));
check(fn () => not(CharArray.all (fn #"3" => true | _ => false) a));
check(fn () => CharArray.all (fn x => x < #"A") a);
check(fn () => not(CharArray.all (fn x => x < #"9") a));
check(fn () => CharArray.collate Char.compare (a, a) = EQUAL);
val b = CharArray.array(10, #"0");
check(fn () => CharArray.vector b = CharVector.fromList(List.tabulate(10, fn _ => #"0")));
CharArray.copy{src = a, dst=b, di=0};
check(fn () => CharArray.vector b = "0123456789");
check(fn () => (CharArray.copy{src = a, dst=b, di=1}; false) handle Subscript => true);
check(fn () => CharArray.collate Char.compare (a, b) = EQUAL);
CharArray.update(a, 9, #"A");
check(fn () => CharArray.collate Char.compare (a, b) = GREATER);
CharArray.update(b, 2, #"B");
check(fn () => CharArray.collate Char.compare (a, b) = LESS);
check(fn () => CharArray.collate Char.compare (CharArray.array(0, #"0"), CharArray.array(0, #"0")) = EQUAL);
check(fn () => CharArray.collate Char.compare (CharArray.array(0, #"0"), a) = LESS);
check(fn () => CharArray.collate Char.compare (a, CharArray.array(0, #"0")) = GREATER);
check(fn () => CharArray.collate Char.compare (CharArray.array(1, #"0"), a) = LESS);
check(fn () => CharArray.collate Char.compare (CharArray.array(1, #"1"), a) = GREATER);

val sa = CharArraySlice.slice(a, 2, NONE);
val sb = CharArraySlice.slice(a, 6, SOME 3);
check(fn () => CharArraySlice.vector sa = "2345678A");
check(fn () => CharArraySlice.vector sb = "678");
check(fn () => (CharArraySlice.slice(a, 6, SOME 5); false) handle Subscript => true);

check(fn () => CharArraySlice.findi(fn (_, #"3") => true | _ => false) sa = SOME(1, #"3"));
check(fn () => not(isSome(CharArraySlice.findi(fn (_, #"3") => true | _ => false) sb)));
check(fn () => not(CharArraySlice.all (fn x => x < #"9") sa));
check(fn () => CharArraySlice.all (fn x => x < #"9") sb);
CharArraySlice.copy{src=sb, dst=a, di=1};
check(fn () => CharArray.vector a = "067845678A");
(* Check for overlapping areas. *)
CharArraySlice.copy{src=sb, dst=a, di=7};
check(fn () => CharArray.vector a = "0678456678");
check(fn () => CharArraySlice.vector sb = "667");
val sc = CharArraySlice.slice(a, 4, SOME 3);
check(fn () => CharArraySlice.vector sc = "456");
CharArraySlice.copy{src=sc, dst=a, di=3};
check(fn () => CharArray.vector a = "0674566678");
