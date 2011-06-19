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
 *     dve-event-execution-compiler.sml
 *
 *  Created:
 *     Nov. 14, 2008
 *
 *  Generate:
 *     structure DveEventExecutionCompiler: sig
 *        val execEvent: state * event -> state * event list
 *        val execPropertyEvent: state * event -> state * event list
 *     end
 *
 *  The second function is only generated if the model contains a property
 *  process.
 *)


structure DveEventExecutionCompiler: sig

    val gen: System.system * bool -> string list

end = struct

open DveCompilerUtils

fun compileExecEvent getEvents funcName withDummy (s, checks) = let

    val events = getEvents s

    val comps = buildStateComps s

    val map: mapping ref = ref (buildMapping comps)

    fun compileExecEvent' e = let
	
	val _ = map := buildMapping comps
	val tmpVarCounter = ref 0
	val currentProc = ref ""
			    
	fun newTmpVal () =
	    (tmpVarCounter := (!tmpVarCounter) + 1;
	     "tmp" ^ Int.toString (!tmpVarCounter))

	(*  return the string corresponding to the expression var which is
	 *  assigned expression value.  So it is simply value if var is a
	 *  simple variable, and it is setFunc (v, i, value) if var is
	 *  the expression v[i]
	 *)
	fun compileExprAssign (proc : string,
			       var  : Expr.var_ref,
			       value: string,
			       pos  : Pos.pos) =
	    case var
	     of Expr.SIMPLE_VAR v => value
	      | Expr.ARRAY_ITEM (v, index) => let
		    val p = Int.toString pos
		    val current = SOME proc
		    val arrayExpr = compileExpr
					(Expr.VAR_REF (pos, Expr.SIMPLE_VAR v))
					(current, !map, comps, checks)
		    val indexExpr = compileExpr
					index
					(current, !map, comps, checks)
		    val comp = getVarComp (comps, SOME proc, v)
		in
		    String.concat [
		    arraySetFunc (Var.getTyp (valOf (getCompVar comp))),
		    " (", arrayExpr, ", ", baseToInt indexExpr, ", ", value,
		    ")", checkIndex checks comps (SOME proc) pos var index ]
		end		

	fun compileStat (Stat.ASSIGN (pos, var, value)) = let
	    val someProc = SOME (!currentProc)
	    val comp = getVarComp (comps, someProc, Expr.getVarName var)
	    val (newVal, result) =
		case (var, value)
		 of (Expr.SIMPLE_VAR _, Expr.INT (_, num)) =>
		    (intToBase (LargeInt.toString num), "")
		  | (Expr.SIMPLE_VAR _, Expr.BOOL_CONST (_, true)) =>
		    ("1", "")
		  | (Expr.SIMPLE_VAR _, Expr.BOOL_CONST (_, false)) =>
		    ("1", "")
		  | _ => let val tmpVal = newTmpVal ()
			     val valueExpr =
				 compileExpr
				     value (someProc, !map, comps, checks)
			 in
			     (tmpVal,
			      String.concat [
			      "   val ", tmpVal, " = ",
			      compileExprAssign
				  (!currentProc, var, valueExpr, pos), "\n" ])
			 end
	in
	    map := updateMapping (!map, comp, newVal);
	    result
	end
							 
	fun compileStatList stats =
	    String.concat (List.map compileStat stats)
			      
	fun compileExecLocalEvent (proc, trans) = let
	    val sl     = Trans.getEffect trans
	    val dest   = getLocalStateName(proc, Trans.getDest trans)
	    val _      = currentProc := proc
	    val result = compileStatList sl
	in
	    map := updateMapping (!map, PROCESS_STATE proc, dest);
	    result
	end
						  
	fun compileExecSyncEvent (proc1, trans1, proc2, trans2) = let
	    (*  NB: proc1 is the sender and proc2 the receiver  *)
	    val dest1  = getLocalStateName(proc1, Trans.getDest trans1)
	    val dest2  = getLocalStateName(proc2, Trans.getDest trans2)
	    val sl1    = Trans.getEffect trans1
	    val sl2    = Trans.getEffect trans2
	    val sent   = Sync.getData (valOf (Trans.getSync trans1))
	    val recv   = Sync.getData (valOf (Trans.getSync trans2))
	    val result =
		(*
		 *  in case of a synchronization event, the state change is
		 *  done as follows:
		 *  1 - reception of the data sent if any
		 *  2 - sending process executes its effect
		 *  3 - receiving process executes its effect
		 *  4 - changement of states of both processes
		 *)
		String.concat [
		case (sent, recv) of
		    (SOME dataSent, SOME (Expr.VAR_REF (pos, var))) => let
			val someProc = SOME proc2
			val tmpVal   = newTmpVal ()
			val comp     = getVarComp (comps, someProc,
						   Expr.getVarName var)
			(*
			 *  we have to take care here that the data sent and
			 *  the receiving variable do not belong to the same
			 *  process
			 *)
			val result =
			    String.concat [
			    "   val dataSent = ",
			    compileExpr dataSent
					(SOME proc1, !map, comps, checks), 
			    "\n   val ", tmpVal, " = ",
			    compileExprAssign (proc2, var, "dataSent", pos),
			    "\n" ]
		    in
			map := updateMapping (!map, comp, tmpVal);
			result
		    end
		  | _ => "",
		(currentProc := proc1; compileStatList sl1),
		(currentProc := proc2; compileStatList sl2) ]
	in
	    map := updateMapping (!map, PROCESS_STATE proc1, dest1);
	    map := updateMapping (!map, PROCESS_STATE proc2, dest2);
	    result
	end
    in
	case e
	 of LOCAL (_, proc, trans) =>
	    compileExecLocalEvent (proc, trans)
	  | SYNC (_, _, proc1, trans1, proc2, trans2) =>
	    compileExecSyncEvent (proc1, trans1, proc2, trans2)
    end
			      
    val eventsPerFunc = 32

    fun compileSlice n [] = []
      | compileSlice n l = let
	    val evts = List.take (l, Int.min (eventsPerFunc, List.length l))
	    fun compile (e, (str, first)) =
		(String.concat [
		 str,
		 if first then "" else "\n | ",
		 String.concat [
		 getEventName e, " => let\n",
		 compileExecEvent' e, "in\n",
		 "   ", mappingToState (!map),
		 "\nend" ]
		 ],
		 false)
	    val exec = #1 (List.foldl compile ("", true) evts)
	in
	    [ "fun ", funcName, Int.toString n,
	      " (", genComps comps, ", e) =\n",
	      "   case e of\n   ", exec,
	      if n = 0 andalso (List.length l <= eventsPerFunc)
	      then "\n"
	      else "\n | _ => raise Impossible \"\"\n" ] @
	    (compileSlice
		 (n + 1)
		 (List.drop (l, Int.min (eventsPerFunc, List.length l))))
	end
    val funcs = compileSlice 0 events
in
    funcs @
    [ if events = []
      then "fun " ^ funcName ^ " (st, _) = st\n"
      else let fun compile (e, (n, str)) =
		   (n + 1,
		    String.concat [
		    str, if str <> "" then "\n  | " else "",
		    funcName, " (st, ", getEventName e, ") = ", funcName,
		    Int.toString (n div eventsPerFunc),
		    " (st, ", getEventName e, ")" ])
	       val (_, f) = List.foldl
				compile
				(0,
				 if withDummy
				 then funcName ^ " (st, DUMMY_EVENT) = st"
				 else "") events
	   in
	       String.concat [
	       "fun ", f, "\n" ]
	   end,
      "\n" ]
end

fun gen (s as { prop, ... }: System.system, checks) =
    [ "structure DveEventExecution = struct\n",
      "\nopen DveDefinitions\n" ] @
    (compileExecEvent buildEvents "execEvent" true (s, checks)) @
    (case prop of NONE => []
		| SOME _ => compileExecEvent
				buildPropertyEvents "execPropertyEvent"
				false (s, checks)) @
    [ "end\n" ]

end
