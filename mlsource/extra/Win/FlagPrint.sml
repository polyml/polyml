(*
    Copyright (c) 2001, 2015
        David C.J. Matthews

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)
functor FlagPrint(structure BITS: BIT_FLAGS) =
struct
    (* Auxiliary function to create a function to print out bit flags.
       The function must actually be installed by the caller because
       it has to be called with the type itself. *)
    fun createFlagPrinter (flagTable: (BITS.flags * string) list) =
    let
        fun accumulateFlags _ [] = []
         |  accumulateFlags f ((w, s)::t) =
            if BITS.allSet(w, f) then s :: accumulateFlags(BITS.clear(w, f)) t
            else accumulateFlags f t
    
        fun printFlags depth _ x =
            (* This is just the code to print a list. *)
            let
                open PolyML
              val stringFlags = accumulateFlags x flagTable
              fun plist [] _ = []
               |  plist _ 0 = [PrettyString "..."]
               |  plist [h]    _ = [PrettyString h]
               |  plist (h::t) depth =
                        PrettyString(h ^ ",") ::
                        PrettyBreak (1, 0) ::
                        plist t (depth - 1)
            in
              PrettyBlock (3, false, [],
                PrettyString "[" ::
                    ((if depth <= 0 then [PrettyString "..."]
                          else plist stringFlags depth) @
                    [PrettyString "]"]
                    )
                )
            end
    in
        printFlags
    end;
end;
