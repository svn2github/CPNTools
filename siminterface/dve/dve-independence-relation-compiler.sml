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
 *     dve-independence-relation-compiler.sml
 *
 *  Created:
 *     Nov. 17, 2008
 *
 *  Generate:
 *     structure DveIndependenceRelation: INDEPENDENCE_RELATION = struct
 *        structure Model = DveModel
 *        ...
 *     end
 *)


structure DveIndependenceRelationCompiler: sig

    val gen: System.system -> string list

end = struct

open DveCompilerUtils

fun compileAreIndependent (s as { prop, ... }: System.system) = let

    val procs = List.filter (fn p => case prop
				      of NONE => true
				       | SOME (_, q) => Process.getName p <> q)
			    (System.getProcs s)
    val events = List.filter (fn e =>
				 case prop
				  of NONE => true
				   | SOME (_, q) =>
				     (case e
				       of LOCAL (_, p, _) => p <> q
					| SYNC (_, _, p, _, p', _) =>
					  p <> q andalso p' <> q))
			     (buildEvents s)
		 
    fun areIndependent
	(e1 as (LOCAL (_, p1, t1)),
	 e2 as (LOCAL (_, p2, t2))) =
	 System.areIndependent ((Process.getProcess (procs, p1), t1),
				(Process.getProcess (procs, p2), t2))
      | areIndependent
	(e1 as (LOCAL (_, p1, t1)),
	 e2 as (SYNC (_, _, p2, t2, p3, t3))) =
	 System.areIndependent ((Process.getProcess (procs, p1), t1),
				(Process.getProcess (procs, p2), t2)) andalso
	 System.areIndependent ((Process.getProcess (procs, p1), t1),
				(Process.getProcess (procs, p3), t3))
      | areIndependent
	(e1 as (SYNC (_, _, p1, t1, p2, t2)),
	 e2 as (SYNC (_, _, p3, t3, p4, t4))) =
	 System.areIndependent ((Process.getProcess (procs, p1), t1),
				(Process.getProcess (procs, p3), t3)) andalso
	 System.areIndependent ((Process.getProcess (procs, p1), t1),
				(Process.getProcess (procs, p4), t4)) andalso
	 System.areIndependent ((Process.getProcess (procs, p2), t2),
				(Process.getProcess (procs, p3), t3)) andalso
	 System.areIndependent ((Process.getProcess (procs, p2), t2),
				(Process.getProcess (procs, p4), t4))
      | areIndependent (e1, e2) = areIndependent (e2, e1)

    fun twoEvents (e1, e2) =
	if areIndependent (e1, e2)
	then String.concat [
	     " (", getEventName e1, ", ", getEventName e2, ") => true\n  |" ]
	else ""
in
    [ "val areIndependent = fn\n   " ] @
    ListXProd.mapX twoEvents (events, events) @
    [ " _ => false\n" ]
end

fun compilePersistentSet (s: System.system) = let
    val ls = System.getCoIndependentStates s
    fun oneCase l = let
	val t = List.concat (List.map (fn (_, _, t) => t) l)
	val test = 
	    listFormat {
	    init  = "(fn e => false",
	    sep   = "",
	    final = ") ",
	    fmt   = (fn t => (" orelse " ^ (case getEventName' t
					     of	NONE   => "false"
					      | SOME e => e ^ " = e")))
	    } t
    in
	String.concat [
	"if ",
	listFormat {
	init  = "",
	sep   = " andalso ",
	final = " ",
	fmt   = (fn (p, s, _) =>
		    (getComp (PROCESS_STATE (Process.getName p), "s") ^ " = " ^
		     getLocalStateName (Process.getName p, State.getName s)))
	} l,
	"andalso (List.exists ", test, " e) then\n",
	"      List.filter ", test, "e"
	]
    end
in
    "fun persistentSet (s, e) = " ^
    (if ls = []
     then "e\n"
     else "\n" ^ (listFormat {
		  init  = "   ",
		  sep   = "\n   else ",
		  final = "\n   else e\n",
		  fmt   = oneCase } ls))
end

fun gen s =
    [ "structure DveIndependenceRelation: INDEPENDENCE_RELATION = ",
      "struct\n",
      "open DveDefinitions\n" ] @
    compileAreIndependent s @
    [ compilePersistentSet s,
      "end\n" ]
end
