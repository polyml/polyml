(*
    Title:      Source level debugger for Poly/ML
    Author:     David Matthews
    Copyright  (c)   David Matthews 2000

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

functor DEBUGGER_ (

(*****************************************************************************)
(*                  STRUCTVALS                                               *)
(*****************************************************************************)
structure STRUCTVALS : STRUCTVALSIG;

(*****************************************************************************)
(*                  VALUEOPS                                                 *)
(*****************************************************************************)
structure VALUEOPS : VALUEOPSSIG;

structure CODETREE :
sig
  type machineWord
  type codetree
  val mkConst:          machineWord -> codetree;
  val CodeZero:         codetree;
end

structure TYPETREE:
sig
    type types
    val unitType:   types;
    val exnType:    types;
end

structure ADDRESS :
sig
  type machineWord;
  val toMachineWord: 'a -> machineWord
end;

sharing type
  CODETREE.machineWord
= VALUEOPS.machineWord
= ADDRESS.machineWord

sharing type
  CODETREE.codetree
= VALUEOPS.codetree
= STRUCTVALS.codetree

sharing type
  STRUCTVALS.values 
= VALUEOPS.values

sharing type
  STRUCTVALS.types 
= VALUEOPS.types
= TYPETREE.types

sharing type
  STRUCTVALS.typeConstrs 
= VALUEOPS.typeConstrs

sharing type
  STRUCTVALS.fixStatus 
= VALUEOPS.fixStatus

sharing type
  STRUCTVALS.structVals 
= VALUEOPS.structVals

sharing type
  STRUCTVALS.signatures 
= VALUEOPS.signatures

sharing type
  STRUCTVALS.functors 
= VALUEOPS.functors

sharing type
  STRUCTVALS.locationProp 
= VALUEOPS.locationProp
)
:
sig
    type types
	type values
	type machineWord
    type fixStatus
    type structVals
    type typeConstrs
    type signatures
    type functors
    type locationProp
    type location

	datatype environEntry =
		EnvValue of string * types * locationProp list
	|	EnvException of string * types * locationProp list
	|	EnvVConstr of string * types * bool * int * locationProp list
	|	EnvStaticLevel

    type nameSpace =
      { 
        lookupVal:    string -> values option,
        lookupType:   string -> typeConstrs option,
        lookupFix:    string -> fixStatus option,
        lookupStruct: string -> structVals option,
        lookupSig:    string -> signatures option,
        lookupFunct:  string -> functors option,

        enterVal:     string * values      -> unit,
        enterType:    string * typeConstrs -> unit,
        enterFix:     string * fixStatus   -> unit,
        enterStruct:  string * structVals  -> unit,
        enterSig:     string * signatures  -> unit,
        enterFunct:   string * functors    -> unit,

        allVal:       unit -> (string*values) list,
        allType:      unit -> (string*typeConstrs) list,
        allFix:       unit -> (string*fixStatus) list,
        allStruct:    unit -> (string*structVals) list,
        allSig:       unit -> (string*signatures) list,
        allFunct:     unit -> (string*functors) list
      };

    (* The debugger function supplied to the compiler. *)
    type debugger = int * values * int * string * string * nameSpace -> unit
    val nullDebug: debugger

    val debuggerFunTag : debugger Universal.tag
    
    datatype debugReason =
        DebugEnter of machineWord * types
    |   DebugLeave of machineWord * types
    |   DebugException of exn
    |   DebugStep

    (* Functions inserted into the compiled code. *)
	val debugFunction:
		debugger * debugReason * string * location -> environEntry list -> machineWord list -> unit
end
=
struct
    open STRUCTVALS VALUEOPS CODETREE

	(* The static environment contains these kinds of entries. *)
	datatype environEntry =
		EnvValue of string * types * locationProp list
	|	EnvException of string * types * locationProp list
	|	EnvVConstr of string * types * bool * int * locationProp list
	|	EnvStaticLevel

    datatype debugReason =
        DebugEnter of machineWord * types
    |   DebugLeave of machineWord * types
    |   DebugException of exn
    |   DebugStep

    (* We pass an integer code plus a value as arguments to the debugger function
       rather than a datatype because it's simpler when passing arguments through
       the bootstrap. *)
    type debugger = int * values * int * string * string * nameSpace -> unit
    (* Create a tag so that the debugger can be included in the parameters. *)
    val debuggerFunTag : debugger Universal.tag = Universal.tag()
    fun nullDebug _ = ()

    (* Reason codes passed to the debugger function. *)
    val debugEnterFun = 1
    and debugLeaveFun = 2
    and debugExceptFun = 3
    and debugLineChange = 4
    
    val dummyValue = mkGvar("", TYPETREE.unitType, CodeZero, [])
    fun makeSpace ctEnv rtEnv =
    let
        (* Create the environment. *)
		fun lookupValues (EnvValue(name, ty, location) :: ntl, valu :: vl) s =
		  		if name = s
				then SOME(mkGvar(name, ty, mkConst valu, location))
				else lookupValues(ntl, vl) s

		  |  lookupValues (EnvException(name, ty, location) :: ntl, valu :: vl) s =
		  		if name = s
				then SOME(mkGex(name, ty, mkConst valu, location))
				else lookupValues(ntl, vl) s

		  |  lookupValues (EnvVConstr(name, ty, nullary, count, location) :: ntl, valu :: vl) s =
		  		if name = s
				then SOME(makeValueConstr(name, ty, nullary, count, Global(mkConst valu), location))
				else lookupValues(ntl, vl) s

		  |  lookupValues (EnvStaticLevel :: ntl, vl) s =
		  		(* Static level markers have no effect here. *)
		  		lookupValues(ntl, vl) s

		  | lookupValues _ _ =
		  	 (* The name we are looking for isn't in
			    the environment.
				The lists should be the same length. *)
			 NONE

 		fun allValues (EnvValue(name, ty, location) :: ntl, valu :: vl) =
		  		(name, mkGvar(name, ty, mkConst valu, location)) :: allValues(ntl, vl)

		 |  allValues (EnvException(name, ty, location) :: ntl, valu :: vl) =
		  		(name, mkGex(name, ty, mkConst valu, location)) :: allValues(ntl, vl)

		 |  allValues (EnvVConstr(name, ty, nullary, count, location) :: ntl, valu :: vl) =
		  		(name, makeValueConstr(name, ty, nullary, count, Global(mkConst valu), location)) ::
				    allValues(ntl, vl)

		 |  allValues (EnvStaticLevel :: ntl, vl) = allValues(ntl, vl)

		 |  allValues _ = []
         
        (* We have a full environment here for future expansion but at
           the moment only the value environment is used. *)
        fun noLook _ = NONE
        and noEnter _ = raise Fail "Cannot update this name space"
        and allEmpty _ = []
   in
       {
            lookupVal = lookupValues(ctEnv, rtEnv),
            lookupType = noLook, lookupFix = noLook, lookupStruct = noLook,
            lookupSig = noLook, lookupFunct = noLook, enterVal = noEnter,
            enterType = noEnter, enterFix = noEnter, enterStruct = noEnter,
            enterSig = noEnter, enterFunct = noEnter,
            allVal = fn () => allValues(ctEnv, rtEnv),
            allType = allEmpty,
            allFix = allEmpty,
            allStruct = allEmpty,
            allSig = allEmpty,
            allFunct = allEmpty }
    end;

	(* A pointer to this function is inserted in the code for each line. *)
	(* Although the nameTypeList and valueList are the same
	   length we build them separately.  This allows the
	   nameTypeList to be built at compile time and reduces
	   the run-time costs. *)
	fun debugFunction (debugger, reason, functionName, location) staticEnv valueList =
	let
		(* The function name supplied is made up to be suitable for output
		   when profiling.  We need to clean it up a bit for use here. The
		   general form is F().S.v-f(2)g where F is a functor, S a structure,
		   v a val declaration, f a curried function and g the function itself.
           If we're within a function we may also have '()' at the end.
		   For the moment just strip out the argument numbers. *)
		fun checkChar (#")", (_, l)) = (true,  #")" :: l) (* Start of parens *)
		  | checkChar (#"(", (_, l)) = (false, #"(" :: l) (* End of parens *)
		  | checkChar (_, (true, l)) = (true, l) (* Remove the character *)
		  | checkChar (c, (false, l)) = (false, c :: l)
		val (_, chars) = List.foldr checkChar (false, []) (explode functionName)
        val name1 = String.implode chars
        (* Remove final '()'.  This makes the name within the function consistent
           with the name on entry and exit to the function. *)
        fun removeSuffix s1 s2 =
            if String.isSuffix s1 s2
            then String.substring(s2, 0, String.size s2-String.size s1)
            else s2
        (* Remove the trailing '()' which appears inside the function so that
           we get the same name for the entry and exit code as we do inside
           the function.
           If this function is defined by a val declaration rather than a
           fun declaration remove the trailing '-' *)
        val processedName = removeSuffix "-" (removeSuffix "()" name1)

        val (code, value) =
            case reason of
                DebugEnter (argValue, argType) =>
                    (debugEnterFun, mkGvar("", argType, mkConst argValue, [DeclaredAt location]))
            |   DebugLeave (fnResult, resType) =>
                    (debugLeaveFun, mkGvar("", resType, mkConst fnResult, [DeclaredAt location]))
            |   DebugException exn =>
				let
                    val exnVal = ADDRESS.toMachineWord exn
                    (* The exception is always a value of type exn. *)
                    val resVal = mkGvar("", TYPETREE.exnType, mkConst exnVal, [DeclaredAt location])
                in
                    (debugExceptFun, resVal)
                end
            |   DebugStep =>
                    (debugLineChange, dummyValue)
	in
        debugger(code, value, #startLine location, #file location, processedName, makeSpace staticEnv valueList)
	end
end;
