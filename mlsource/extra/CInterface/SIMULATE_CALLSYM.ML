(*
	Copyright (c) 2000
		Cambridge University Technical Services Limited

	This library is free software; you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public
	License as published by the Free Software Foundation; either
	version 2.1 of the License, or (at your option) any later version.
	
	This library is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
	Lesser General Public License for more details.
	
	You should have received a copy of the GNU Lesser General Public
	License along with this library; if not, write to the Free Software
	Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

(**********************************************************************
 * Redefine call_sym in terms of call_sym_and_convert
 * to demonstrate that no information is lost.
 * and to maintain same top level interface.
 **********************************************************************)

functor SIMULATE_CALLSYM
    (LowerLevel : LowerLevelSig) : OrigLowerLevelSig =
struct

    open LowerLevel;
    open LowerLevel.Union; 
        
    (* now imported from Union ...
       exception Never of string;
    ... *)
    
    fun never string = raise Never string;

    fun call_sym sym args retCtype =
    let
      val result = 
        call_sym_and_convert
          sym 
          (map (fn x => In (Vol x)) args)
          (chooseVol retCtype)
    in
      case result of
        (v,[]) => if isVol v then deVol v else never "call_sym"
      | _      => never "call_sym"
    end

end;
