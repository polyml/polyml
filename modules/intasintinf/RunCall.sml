(*
    Title:      Rebuild the basis library: Runcall
    Copyright   David C.J. Matthews 2016

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

(* This won't generally be useful but  including this allows the compiler to build. *)
structure RunCall =
struct
    open RunCall
    val run_call0 = fn n => run_call0 (FixedInt.fromLarge n)
    and run_call1 = fn n => run_call1 (FixedInt.fromLarge n)
    and run_call2 = fn n => run_call2 (FixedInt.fromLarge n)
    and run_call3 = fn n => run_call3 (FixedInt.fromLarge n)
    and run_call4 = fn n => run_call4 (FixedInt.fromLarge n)
    and run_call5 = fn n => run_call5 (FixedInt.fromLarge n)
    and run_call2C2 = fn n => run_call2C2 (FixedInt.fromLarge n)
end;

useBasis "RuntimeCalls";
