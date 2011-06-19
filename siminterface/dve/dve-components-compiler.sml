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
 *     dve-components-compiler.sml
 *
 *  Created:
 *     Jan. 13, 2009
 *
 *  Generate:
 *     structure DveComponents: MODEL_COMPONENTS = struct
 *        type event = ...
 *        ...
 *     end
 *)


structure DveComponentsCompiler: sig

    val gen: System.system -> string list

end = struct

open DveCompilerUtils

fun updated (sys, comps) e = let
    fun updates (LOCAL (_, proc, tr), GLOBAL_VAR var) =
	List.exists (fn v => v = Var.getName var) (Trans.modifiedVars tr)
	andalso
	not (Process.hasLocalVariable (System.getProc (sys, proc))
				      (Var.getName var))
      (*****)
      | updates (LOCAL (_, proc, tr), PROCESS_STATE proc') =
	(proc = proc') andalso
	(Trans.getSrc tr) <> (Trans.getDest tr)
      (*****)
      | updates (LOCAL (_, proc, tr), LOCAL_VAR (proc', var)) =
	(proc = proc') andalso
	List.exists (fn v => v = Var.getName var) (Trans.modifiedVars tr)
      (*****)
      | updates (SYNC (i1, i2, p1, t1, p2, t2), c) =
	updates (LOCAL (i1, p1, t1), c) orelse
	updates (LOCAL (i2, p2, t2), c)
in	
    #2 (List.foldl (fn (c, (n, l)) => (n + 1, if updates (e, c)
					      then (n, c) :: l
					      else l))
		   (0, []) comps)
end

fun compileComponents (s: System.system) = let
    val comps  = List.filter (fn c => not (isCompConst c)) (buildStateComps s)
    val events = buildEvents s
    fun componentsUpdated e =
	String.concat [
	"\n  | componentsUpdated ", getEventName e, " = ",
	listFormat { init  = "[ ",
		     sep   = ", ",
		     final = " ]",
		     fmt   = Int.toString }
		   (List.map #1 (updated (s, comps) e)) ]
    val i = ref 0
    fun componentDef c = 
	String.concat [
	"COMP", Int.toString (!i), " of ",
	getCompTypeName c ] before i := !i + 1
    val i = ref 0
    fun getComponent c =
	String.concat [
	"getComponent ({", getCompName c, ", ...}: state, ",
	Int.toString (!i), ") = COMP", Int.toString (!i),
	" ", getCompName c, "\n  | " ] before i := !i + 1
    val i = ref 0
    fun componentName c =
	String.concat [
	"componentName ", Int.toString (!i), " = \"",
	getCompDescription c, "\"\n  | " ] before i := !i + 1
    val i = ref 0
    fun componentToString c =
	String.concat [
	"componentToString (COMP", Int.toString (!i), " c) = ",
	getCompToStringFuncName c, " c" ] before i := !i + 1
    val i = ref 0
    fun componentToSML c =
	String.concat [
	"componentToSMLString (COMP", Int.toString (!i), " c) = ",
	getCompToSMLStringFuncName c, " c" ] before i := !i + 1
	
in
    [ "val numComponents = ", Int.toString (List.length comps), "\n\n",
      "datatype component = \n",
      listFormat { init  = "    ",
		   sep   = "\n  | ",
		   final = "\n",
		   fmt   = componentDef } comps, "\n",
      listFormat { init  = "fun ",
		   sep   = "",
		   final = ("getComponent _ = raise Impossible \"" ^
			    "invalid component_id\"\n"),
		   fmt   = getComponent } comps, "\n",
      listFormat { init  = "fun ",
		   sep   = "",
		   final = ("componentName _ = raise Impossible \"" ^
			    "invalid component_id\"\n"),
		   fmt   = componentName } comps, "\n",
      "fun componentsUpdated DUMMY_EVENT = []",
      listFormat { init  = "",
		   sep   = "",
		   final = "\n",
		   fmt   = componentsUpdated } events, "\n",
      listFormat { init  = "fun ",
		   sep   = "\n  | ",
		   final = "",
		   fmt   = componentToString } comps, "\n",
      listFormat { init  = "fun ",
		   sep   = "\n  | ",
		   final = "",
		   fmt   = componentToSML } comps, "\n" ]
end

fun compileLargeComponents (s: System.system) = let
    val comps = List.filter (fn c => not (isCompConst c)) (buildStateComps s)
    val largeComps = buildLargeStateComps s
    val events = buildEvents s
    fun updated' e = let
	val lc = List.map #2 (updated (s, comps) e)
	fun intersect lc' =
	    List.exists (fn c => List.exists (fn c' => c = c') lc) lc'
    in
	#2 (List.foldl (fn (c, (n, l)) => (n + 1,
					   if intersect (getSubComps c)
					   then (n, c) :: l
					   else l))
		       (0, []) largeComps)
    end
in
    [
      "val numComponents = ", Int.toString (List.length largeComps), "\n",
      "fun "
    ] @
    (List.foldr (fn (e, rest) =>
		    String.concat [
		    "componentsUpdated ", getEventName e, " = ",
		    listFormat { init  = "[ ",
				 sep   = ", ",
				 final = " ]",
				 fmt   = Int.toString }
			       (List.map #1 (updated' e))  ]
		    :: "\n  | " :: rest)
		["componentsUpdated DUMMY_EVENT = []" ] events)
end

fun gen s = let
    val componentsDef = compileComponents s
    val componentsLargeDef = compileLargeComponents s
in
    [ "structure DveComponents(*: MODEL_COMPONENTS*) = struct\n",
      "open DveDefinitions\n",
      "type event = event\n",
      "type state = state\n",
      "type component_id = int\n" ] @ componentsDef @
    [ "\n",
      "val hashComponents = DveComponentsHashFunction.hash\n",
      "\nend\n\n",
      
      "structure DveLargeComponents(*: MODEL_COMPONENTS*) = struct\n",
      "open DveDefinitions\n",
      "type event = event\n",
      "type state = state\n",
      "type component_id = int\n" ] @ componentsLargeDef @
    [ "\n",
      "val hashComponents = DveLargeComponentsHashFunction.hash\n",
      "\nend\n" ]
end

end
