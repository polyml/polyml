(* Example code for a C-library accessible from ML
   using the CInterface structure.

   Copyright David C.J. Matthews 1999-2009

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

open CInterface;

val mylib = CInterface.load_lib "Foreign";

(* Example of creating a conversion for a datatype. *)
datatype intTree = NullTree | Node of {left: intTree, right: intTree, valu: int};

(* The corresponding C structure is
typedef struct _tree {
    struct _tree *left, *right;
    int nValue;
} *tree;
*)

local
    (* Start with the C structure. *)
    val TREENODE = STRUCT3(POINTER, POINTER, INT);
    val (unMakeStruct, mkStruct, structType) = breakConversion TREENODE;
in
(* The following function builds a C data structure from an ML datatype. *)
    fun tree2vol NullTree: vol = toCint 0
     |  tree2vol (Node{left, right, valu}) =
         let
             (* Construct a struct from the values. *)
             val str = mkStruct(tree2vol left, tree2vol right, valu)
             (* Allocate a struct on the heap. *)
             val v = alloc 1 structType
         in
             (* Copy it over *)
             assign structType v str;
             (* Return the address of the heap area. *)
             address v
         end;
             
(* The inverse of tree2vol. We don't actually use this in this example. *)
    fun vol2tree v =
        if fromCint v = 0
        then NullTree
        else
        let
            val struc = deref v (* Get the structure itself from the pointer. *)
            val (left, right, valu) = unMakeStruct struc
        in
            Node{left=vol2tree left, right=vol2tree right, valu = valu }
        end
end;

(* Build a conversion out of this. *)
val TREE = mkConversion vol2tree tree2vol voidStar;

val sumTree = CInterface.call1 ( CInterface.load_sym mylib "SumTree") TREE INT;

val aTree = Node{left=Node{left=NullTree, right=NullTree, valu=4},
               right=Node{
                    left= Node{left=NullTree, right=NullTree, valu=3},
                    right=NullTree, valu=5},
               valu = 7};
sumTree aTree;


(* Example of returning a structure. *)
val returnR2 = CInterface.call2 ( CInterface.load_sym mylib "ReturnR2")
    (CInterface.INT, CInterface.INT) (STRUCT2(CInterface.INT, CInterface.INT));
(* This function crashes the old version.  Now fixed. *)
returnR2(5,6);

(* Example of passing and returning strings. *)
val dupNString = CInterface.call2 (CInterface.load_sym mylib "DupNString") (CInterface.INT, CInterface.STRING)
    CInterface.STRING;

dupNString (4, "hi");

(* Example of a callback function. *)

fun f (i, j) = (PolyML.print(i, j); i+j);
val doAdd = call2 (load_sym mylib "MakeCallback") (INT, FUNCTION2 (INT, INT) INT) INT;
doAdd(4, f);
(* Check that an exception is properly propagated. *)
(* This doesn't work if the library has been compiled with GCC.  Foreign.c is C code
   not C++ and the C++ exception mechanism won't propagate a C++ exception through
   C code.  *)
(* doAdd(4, fn _ => raise Fail "failed"); *)

(* int a, char b, double c, float d, short e, int *f *)
fun myCallback [v1, v2, v3, v4, v5, v6] =
    let
        val a = fromCint v1;
        val b = fromCchar v2;
        val c = fromCdouble v3;
        val d = fromCfloat v4;
        val e = fromCshort v5
        (* f ? *)
    in
        PolyML.print(a, b, c, d, e);
        toCdouble 99.0
    end
 |  myCallback _ = toCint 1234;


val myCbVol = toCfunction [Cint, Cchar, Cdouble, Cfloat, Cshort, Cpointer Cint] Cdouble myCallback;

val returnR3 = call_sym (load_sym mylib "MakeCallback2") [(Cpointer Cint, myCbVol) ] Cdouble;
fromCdouble returnR3;

val doit = call2(load_sym mylib "MakeCallback3") (FUNCTION1 INT VOID, INT) VOID;
doit(fn i => print(Int.toString i), 2);

(* Test for finalisation. *)
val allocateIt = call0 (load_sym mylib "AllocateIt") () POINTER;
val v1 = allocateIt();
val v2 = allocateIt ();
val final = load_sym mylib "FreeIt";
setFinal final v1;
setFinal final v2;

(* Activating the finalisers requires a full GC. *)
val v1 = 0; (* The v1 object is no longer reachable. *)
PolyML.fullGC();
val v2 = 0; (* The v2 object is no longer reachable. *)
PolyML.fullGC();

