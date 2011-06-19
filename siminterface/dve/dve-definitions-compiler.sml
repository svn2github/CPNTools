(************************************************************************)
(* CPN Tools Simulator (Simulator/CPN)                                  *)
(* Copyright 2010-2011 AIS Group, Eindhoven University of Technology    *)
(* All rights reserved.                                                 *)
(*                                                                      *)
(* This file is part of the CPN Tools Simulator (Simulator/CPN).        *)
(*                                                                      *)
(* You can choose among two licenses for this code, either the GNU      *)
(* General Public License version 2 or the below 4-clause BSD License.  *)
(*                                                                      *)
(************************************************************************)
(* GNU General Public License for CPN Tools Simulator (Simulator/CPN)   *)
(*                                                                      *)
(* CPN Tools is free software: you can redistribute it and/or modify    *)
(* it under the terms of the GNU General Public License as published by *)
(* the Free Software Foundation, either version 2 of the License, or    *)
(* (at your option) any later version.                                  *)
(*                                                                      *)
(* CPN Tools is distributed in the hope that it will be useful,         *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of       *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *)
(* GNU General Public License for more details.                         *)
(*                                                                      *)
(* You should have received a copy of the GNU General Public License    *)
(* along with CPN Tools.  If not, see <http://www.gnu.org/licenses/>.   *)
(************************************************************************)
(* 4-clause BSD License for CPN Tools Simulator (Simulator/CPN)         *)
(*                                                                      *)
(* Redistribution and use in source and binary forms, with or without   *)
(* modification, are permitted provided that the following conditions   *)
(* are met:                                                             *)
(*                                                                      *)
(* 1. Redistributions of source code must retain the above copyright    *)
(* notice, this list of conditions and the following disclaimer.        *)
(* 2. Redistributions in binary form must reproduce the above copyright *)
(* notice, this list of conditions and the following disclaimer in the  *)
(* documentation and/or other materials provided with the distribution. *)
(* 3. All advertising materials mentioning features or use of this      *)
(* software must display the following acknowledgement: “This product   *)
(* includes software developed by the AIS Group, Eindhoven University   *)
(* of Technology and the CPN Group, Aarhus University.”                 *)
(* 4. Neither the name of the AIS Group, Eindhoven University of        *)
(* Technology, the name of Eindhoven University of Technology, nor the  *)
(* names of its contributors may be used to endorse or promote products *)
(* derived from this software without specific prior written permission.*)
(*                                                                      *)
(* THIS SOFTWARE IS PROVIDED BY THE AIS GROUP, EINDHOVEN UNIVERSITY OF  *)
(* TECHNOLOGY AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,   *)
(* BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND    *)
(* FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL   *)
(* THE AIS GROUP, EINDHOVEN UNIVERSITY OF TECHNOLOGY BE LIABLE FOR ANY  *)
(* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL   *)
(* DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE    *)
(* GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS        *)
(* INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER *)
(* IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR      *)
(* OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN  *)
(*IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                         *)
(************************************************************************)
(*
 *  File:
 *     dve-definitions-compiler.sml
 *
 *  Created:
 *     Nov. 14, 2008
 *
 *  Generate:
 *     structure DveDefinitions = struct
 *        type state = ...
 *        type event = ...
 *        ...
 *     end
 *)


structure DveDefinitionsCompiler: sig

    val gen: System.system -> string list

end = struct

open DveCompilerUtils

fun compileStateType s = let
    fun compileProcessStateTypes () = let	
	fun compileProcessState p = let
	    val num     = ref 0
	    val pName   = Process.getName p
	    val pInit   = Process.getInit p
	    val pStates = Process.getStates p
	    fun compileState s = getLocalStateName (pName, State.getName s)
	    fun compileStateToString s =
		String.concat [
		getLocalStateName (pName, State.getName s), " => \"",
		State.getName s, "\"" ]
	    fun compileStateToSMLString s =
		String.concat [
		getLocalStateName (pName, State.getName s), " => \"",
		getLocalStateName (pName, State.getName s), "\"" ]
	    fun compileStateToInt s =
		String.concat [
		getLocalStateName (pName, State.getName s), " => ",
		Int.toString (!num) ] before (num := (!num) + 1)
	    fun compileIntToState s =
		String.concat [
		Int.toString (!num), " => ",
		getLocalStateName (pName, State.getName s) ]
		before num := (!num) + 1
	    val typeDef =
		String.concat [
		"datatype ", getLocalStateType (pName), " =\n",
		listFormat {init = "   ", sep = "\n | ", final = "\n",
			    fmt = compileState} pStates,
		"val ", getInitStateName pName, " = ",
		getLocalStateName (pName, State.getName pInit), "\n" ]
	    val stateToString =
		String.concat [
		"fun ", getLocalStateToString pName, " s =\n   case s\n",
		listFormat {init = " of ", sep = "\n  | ", final = "\n",
			    fmt = compileStateToString}
			   pStates ]
	    val stateToSMLString =
		String.concat [
		"fun ", getLocalStateToSMLString pName, " s =\ncase s\n",
		listFormat {init = " of ", sep = "\n  | ", final = "\n",
			    fmt = compileStateToSMLString}
			   pStates ]
	    val stateToInt = (
		num := 0;
		String.concat [
		"\nfun ", getLocalStateToInt pName, " s =\ncase s\n",
		listFormat { init = " of ", sep = "\n | ", final = "\n",
			     fmt = compileStateToInt } pStates ])
	    val intToState = (
		num := 0;
		String.concat [
		"\nfun ", getIntToLocalState pName, " i =\n",
		"   case i\n",
		listFormat {init = " of ", sep = "\n | ",
			    final = "\n | _ => raise Impossible " ^
				    "(\"invalid state id\")\n",
			    fmt = compileIntToState} pStates, "\n" ])
	in
	    String.concat [
	    typeDef, stateToString, stateToSMLString, stateToInt, intToState ]
	end
    in
	String.concat (List.map compileProcessState (System.getProcs s))
    end

    fun compileComp comp =
	SOME (String.concat [ getCompName comp, ": ", getCompTypeName comp ])

    fun compileConst comp = let
	val v = valOf (getCompVar comp)
	val t = Var.getTyp v
    in
	SOME ("val " ^ (getCompName comp) ^ ": " ^
	      (typeName t) ^ " ref = ref " ^ (compileInitVal t))
    end

    fun compileArrayType (t, bt, n) = let
	val init = Utils.constructList ("base_type", n)
	val indexes = List.tabulate (n, fn i => i)
	val typeDef = String.concat [
		      "type ", typeName t, " =\n   ",
		      listFormat { init  = "",
				   final = "\n",
				   sep   = " *\n   ",
				   fmt   = (fn s => s) } init ]
	fun fmt i = let
	    fun fmt' j = if i = j
			 then "item"
			 else "_"
	    val def = listFormat { init  = "(",
				   final = ")",
				   sep   = ", ",
				   fmt   = fmt' } indexes
	in
	    String.concat [
	    arrayGetFunc t, " (", def, ", ", Int.toString i, ") = item" ]
	end
    	val getFunc = String.concat [
		      "fun ", listFormat { init  = "",
					   final = "\n  | ",
					   sep   = "\n  | ",
					   fmt   = fmt } indexes,
		      arrayGetFunc t, " _ = raise IndexError\n" ]
	fun fmt i = let
	    fun fmt' j = if i = j
			 then "item"
			 else "a" ^ (Int.toString j)
	    fun fmt'' j = if i = j
			  then "_"
			  else "a" ^ (Int.toString j)
	    val def = listFormat { init  = "(",
				   final = ")",
				   sep   = ", ",
				   fmt   = fmt'' } indexes
	    val res = listFormat { init  = "(",
				   final = ")",
				   sep   = ", ",
				   fmt   = fmt' } indexes
	in
	    String.concat [
	    arraySetFunc t,
	    " (", def, ", ", Int.toString i, ", item) = ", res ]
	end			
	val setFunc = String.concat [
		      "fun ", listFormat { init  = "",
					   final = "\n  | ",
					   sep   = "\n  | ",
					   fmt   = fmt } indexes,
		      arraySetFunc t, " _ = raise IndexError\n" ]
	fun fmt j = "a" ^ (Int.toString j)
	val def = listFormat { init  = "(",
			       final = ")",
			       sep   = ", ",
			       fmt   = fmt } indexes
	fun fmt i = "(baseToString (a" ^ (Int.toString i) ^ "))"
    	val toStringFunc = String.concat [
			   "fun ", arrayToStringFunc t, " ", def,
			   " = String.concat [ \"[\", ",
			   listFormat { init  = "",
					final = "",
					sep   = ", \", \", ",
					fmt   = fmt } indexes,
			   ", \"]\" ]\n" ]
    	val toSMLStringFunc = String.concat [
			      "fun ", arrayToSMLStringFunc t, " ", def,
			      " = String.concat [ \"(\", ",
			      listFormat { init  = "",
					   final = "",
					   sep   = ", \", \", ",
					   fmt   = fmt } indexes,
			      ", \")\" ]\n" ]
    in
	String.concat [
	typeDef, getFunc, setFunc, toStringFunc, toSMLStringFunc ]
    end

    val dimensions = ref []
    fun compileArrayTypes () = let
	fun compileVar v =
	    case Var.getTyp v of
		t as Typ.ARRAY_TYPE (bt, dim) =>
		if isSome (List.find (fn i => i = dim) (!dimensions))
		then ""
		else (dimensions := dim :: !dimensions;
		      compileArrayType (t, bt, dim))
	      | _ => ""
	fun compileProc p =
	    String.concat (List.map compileVar (Process.getVars p))
    in
	String.concat [
	List.foldl (fn (v, str) => str ^ (compileVar v))
		   "" (System.getVars s),
	List.foldl (fn (p, str) => str ^ (compileProc p))
		   "" (System.getProcs s)
	]
    end
	
    val arrayTypeDefs = compileArrayTypes ()
    val processStateTypeDefs = compileProcessStateTypes ()
    val comps = buildStateComps s
    val (consts, comps) = List.partition isCompConst comps
in
    [
     arrayTypeDefs,
     processStateTypeDefs,
     "type state = {\n",
     Utils.fmt {init  = "   ",
		sep   = ",\n   ",
		final = "\n}\n",
		fmt   = compileComp} comps,
     Utils.fmt {init  = "",
		sep   = "\n",
		final = "\n",
		fmt   = compileConst} consts
    ]
end

fun compileEventType (s as { prop, ... }: System.system) = let
    fun isProcEvent (name, LOCAL (_, p, _)) = p = name
      | isProcEvent (name, SYNC (_, _, p, _, q, _)) = p = name orelse q = name
    fun getProcEvents (events, name) =
	List.filter (fn e => isProcEvent (name, e)) events
    val systemEvents = buildEvents s
    val propertyEvents = buildPropertyEvents s
    val eventTypeDef =
	String.concat [
	"datatype event =\n",
	 (listFormat {init  = "   DUMMY_EVENT",
		      sep   = "",
		      final = "\n",
		      fmt   = fn e => ("\n | " ^ (getEventName e)) }
		     systemEvents), "\n" ]
    val propertyEventTypeDef =
	String.concat [
	"datatype property_event =\n",
	 (listFormat {init  = "   ",
		      sep   = "\n | ",
		      final = "\n",
		      fmt   = getEventName} propertyEvents), "\n" ]
    val (n, eventToInt) =
	List.foldl
	    (fn (e, (i, str)) =>
		(i + 1,
		 String.concat [
		 str, "  | eventToInt ", getEventName e, " = ",
		 Int.toString i, "\n" ]))
	    (1, "fun eventToInt DUMMY_EVENT = 0\n") systemEvents
in
    [
     eventTypeDef,
     case propertyEvents of [] => "" | _ => propertyEventTypeDef,
     eventToInt
     ]
end

fun gen s = let
    val eventsDef = compileEventType s
    val stateDef = compileStateType s
in
    [ "structure DveDefinitions = struct\n",
      "exception EventNotEnabled\n",
      "exception IndexError\n",
      "exception ModelError of int * string\n",
      "exception Impossible of string\n",
      "type base_type = int\n",
      "fun baseToString v = Int.toString v\n" ] @
    [ "\n(***\n *  event definition\n ***)\n" ] @
    eventsDef @
    [ "\n(***\n *  state definition\n ***)\n" ] @
    stateDef @
    [ "\nend\n" ]
end

end
