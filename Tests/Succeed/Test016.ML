(* Bug in the equality compiler. This caused a seg fault. *)

datatype ('a, 'v) trie = Trie of 'v option * ('a * ('a, 'v) trie) list;

val x = Trie (NONE, [(0, Trie (SOME ~2, []))]);

val y = Trie
      (NONE,
         [(~2, Trie (NONE, [(0, Trie (SOME 1, [])), (0, Trie (NONE, []))])),
            (0,
               Trie
                  (SOME ~2,
                     [(0, Trie (SOME 2, [])), (1, Trie (SOME ~1, []))]))]);


x = y;
