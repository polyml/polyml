(*
    Title:      Standard Basis Library: Array2 signature.
    Author:     David Matthews
    Copyright   David Matthews 2000, 2005, 2016

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

signature ARRAY2 =
sig
    eqtype 'a array
    type 'a region =
    {
        base : 'a array,
        row : int,
        col : int,
        nrows : int option,
        ncols : int option
    }
    datatype traversal = RowMajor | ColMajor
    val array: int * int * 'a -> 'a array
    val fromList: 'a list list -> 'a array
    val tabulate: traversal -> int * int * (int * int -> 'a) -> 'a array
    val sub: 'a array * int * int -> 'a
    val update: 'a array * int * int * 'a -> unit
    val dimensions: 'a array -> int * int
    val nCols: 'a array -> int
    val nRows: 'a array -> int
    val row: 'a array * int -> 'a Vector.vector
    val column: 'a array * int -> 'a Vector.vector
    val copy:
     {src : 'a region, dst : 'a array, dst_row : int, dst_col : int} -> unit
    val appi: traversal -> (int * int * 'a -> unit) -> 'a region -> unit
    val app: traversal -> ('a -> unit) -> 'a array -> unit
    val modifyi: traversal -> (int * int * 'a -> 'a) -> 'a region -> unit
    val modify: traversal -> ('a -> 'a) -> 'a array -> unit
    val foldi:
        traversal -> (int * int * 'a * 'b -> 'b) -> 'b -> 'a region -> 'b
    val fold: traversal -> ('a * 'b -> 'b) -> 'b -> 'a array -> 'b
end;
