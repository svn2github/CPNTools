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
 *     dve-model-compiler.sml
 *
 *  Created:
 *     Nov. 17, 2008
 *
 *  Generate:
 *     structure DveModel: MODEL = struct
 *        ...
 *     end
 *  If there is in the model a process describing an ltl formula then DveModel
 *  has signature LTL_MODEL.
 *)


structure DveModelCompiler: sig

    val gen: System.system * bool -> string list

end = struct

open DveCompilerUtils

fun compileGetInitialStates (s, checks, comps, events) = let
    val map: mapping ref = ref []
    val tmpVarCounter = ref 0
    fun newTmpVal () = (tmpVarCounter := (!tmpVarCounter) + 1;
			"tmp" ^ Int.toString (!tmpVarCounter))
    (*  We initialize the initial state as follows:
     *  1 - set the initial states of processes
     *  2 - compute the initial values of global variables
     *  3 - compute the initial values of local variables
     *)
    fun generateLocalStateInit proc = let
	val procName = Process.getName proc
	val procInit = Process.getInit proc				       
    in
	map := (PROCESS_STATE procName, getInitStateName procName) :: (!map)
    end

    fun generateVarInit (var : Var.var,
			 proc: string option) = let
	val tmpVar = newTmpVal ()
	val init = Var.getInit var
    in
	(tmpVar,
	 String.concat [
	 "      val ", tmpVar, " = ",
	 case init of
	     NONE   => compileInitVal (Var.getTyp var)
	   | SOME e => compileExpr e (proc, !map, comps, checks),
	"\n" ])
    end

    fun generateGlobalVarInit var = let
	val (newVar, eval) = generateVarInit (var, NONE)
    in
	map := (GLOBAL_VAR var, newVar) :: (!map);
	eval
    end

    fun generateLocalVarsInit proc = let
	val procName = Process.getName proc
	fun generateLocalVarInit var = let
	    val (newVar, eval) = generateVarInit (var, SOME procName)
	in
	    map := (LOCAL_VAR (procName, var), newVar) :: (!map);
	    eval
	end
    in
	String.concat (List.map generateLocalVarInit (Process.getVars proc))
    end
				     
    fun generateConstsInit comp =
	if not (isCompConst comp)
	then ""
	else String.concat [
	     "      val _ = ", getCompName comp, " := ",
	     getImage (!map, comp), "\n" ]


    val _ = List.app generateLocalStateInit (System.getProcs s)
    val evalGlob =
	String.concat (List.map generateGlobalVarInit (System.getVars s))
    val evalLocal =
	String.concat (List.map generateLocalVarsInit (System.getProcs s))
    val evalConst =
	String.concat (List.map generateConstsInit comps)
in
    String.concat [
    "fun getInitialStates () = let\n",
    "   fun generateInitialState () = let\n",
    evalGlob,
    evalLocal,
    evalConst,
    "   in\n",
    "      ", mappingToState (!map), "\n",
    "   end\n",
    "   val init = generateInitialState ()\n",
    "in\n",
    "   [ (init, getEnabledEvents (init)) ]\n",
    "end\n" ]
end

fun compileStateToString (s, comps) = let
    val tab = ref "   "
    val globalVars = getGlobalVars comps
    val processes = System.getProcs s
    fun varToString comp = let
	val var = valOf (getCompVar comp)
	val t = Var.getTyp var
	val value = getCompToStringFuncName comp ^ getComp (comp, "s")
    in
	"\"\\n" ^ (!tab) ^ Var.getName (var) ^ " = \" ^ " ^ value ^ " ^ \"\""
    end
    fun processToString proc = let
	val procName = Process.getName proc
	val vars = getLocalVars (comps, procName)
	val _ = tab := "      "
    in
	String.concat [
	"\"\\n   ", procName, " @ \" ^ ", getLocalStateToString procName,
	" ", getComp (PROCESS_STATE procName, "s"), " ^ \" {\" ^\n",
	if vars = []
	then "   \"\\n   }\""
	else listFormat {
	     init  = "   ",
	     sep   = " ^\n   ",
	     final = " ^\n   \"\\n   }\"",
	     fmt   = varToString} vars ]
    end
in
    String.concat [
     "fun stateToString (s: state) = let\n",
     "in\n",
     "   \"{\" ^\n",
     if globalVars = []
     then ""
     else listFormat { init  = "   ",
		       sep   = " ^\n   ",
		       final = " ^\n",
		       fmt   = varToString } globalVars,
     if processes = []
     then ""
     else listFormat { init  = "   ",
		       sep   = " ^\n   ",
		       final = " ^\n",
		       fmt   = processToString } processes,
     "\"\\n}\\n\"\n",
     "end\n"
    ]
end

fun compileEventToString (s, events) = let
    fun transToString (proc, trans) = let
	val src  = Trans.getSrc  trans
	val dest = Trans.getDest trans
    in
	proc ^ ": " ^ src ^ " -> " ^ dest
    end
    fun eventToString e =
	String.concat [
	"\n    | ", getEventName e, " => \"",
	case e
	 of LOCAL (_, proc, trans) =>
	    String.concat [ "(", transToString (proc, trans), ")" ]
	  | SYNC  (_, _, proc1, trans1, proc2, trans2) =>
	    String.concat [ 
	    "(", transToString (proc1, trans1),
	    ", ", transToString (proc2, trans2), ")" ],
	"\"" ]
in
    String.concat [
    "fun eventToString (e: event) =\n",
    case events
     of [] => "\"\"\n"
      | _  => String.concat [
	      "   case e\n",
	      "    of DUMMY_EVENT => \"\"",
	      listFormat { init  = "",
			   sep   = "",
			   final = "\n",
			   fmt   = eventToString } events ] ]
end

fun compileNextStates ({ prop = NONE, ... }: System.system) =
    String.concat [
    "fun nextStates (s, DUMMY_EVENT) = raise Impossible \"\"\n",
    "  | nextStates (s, e) = let\n",
    "   val next = execEvent (s, e)\n",
    "in\n",
    "   [ (next, getEnabledEvents (next)) ]\n",
    "end\n" ]
  | compileNextStates ({ prop = SOME _, ... }: System.system) =
    String.concat [
    "fun nextStates (s, e) = let\n",
    "   val propEvts = getPropertyEnabledEvents s\n",
    "   val s = execEvent (s, e)\n",
    "   val evts = getEnabledEvents s\n",
    "in\n",
    "   List.map (fn e => (execPropertyEvent (s, e), evts)) propEvts\n",
    "end\n" ]

fun compileExecuteSequence s =
    String.concat [
    "fun executeSequence (s: state, seq: event list) = let\n",
    "   fun loop ((s, evts), seq) =\n",
    "      (s, evts) :: \n",
    "      (case seq of [] => []\n",
    "                 | evt :: seq' => let\n",
    "                    val next     = execEvent (s, evt)\n",
    "                    val nextEvts = getEnabledEvents (next)\n",
    "                 in\n",
    "                    loop ((next, nextEvts), seq')\n",
    "                 end)\n",
    "in\n",
    "   loop ((s, getEnabledEvents s), seq)\n",
    "end\n" ]

fun compileAccepting (s as { prop = SOME (_, proc), ... }: System.system,
		      comps) = let
    val proc = System.getProc (s, proc)
    val procName = Process.getName proc
    val accept = Process.getAccept proc
    val states = Process.getStates proc
    val comp = getCompName (PROCESS_STATE (Process.getName proc))
    fun oneState state =
	String.concat [
	"accepting ({ ", comp, " = ",
	getLocalStateName (procName, State.getName state),
	", ... }: state) = true" ]
    val final = if List.length accept = List.length states
		then "\n"
		else String.concat [ case accept of [] => "" | _ => "\n  | ",
				     "accepting _ = false\n" ]
in
    listFormat { init  = "fun ",
		 sep   = "\n  | ",
		 final = final,
		 fmt   = oneState } accept
end    
  | compileAccepting (s as { prop = NONE, ... }: System.system, _) = ""
    

fun gen (s as { prop, ... }, checks) = let
    val comps = buildStateComps s
    val events = buildEvents s
in
    [
     "structure DveModel: ",
     case prop of NONE => "MODEL" | SOME _ => "LTL_MODEL",
     " = struct\n",
     "open DveDefinitions\n",
     "open DveEnablingTest\n",
     "open DveEventExecution\n",
     compileGetInitialStates (s, checks, comps, events),
     compileStateToString (s, comps),
     compileEventToString (s, events),
     compileNextStates s,
     compileExecuteSequence s,
     compileAccepting (s, comps),
     "end\n"
    ]
end

end
