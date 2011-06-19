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
 *     dve-hash-function-compiler.sml
 *
 *  Created:
 *     Nov. 17, 2008
 *
 *  Generate:
 *     structure DveHashFunction: HASH_FUNCTION = struct
 *        type state = DveModel.state
 *        ...
 *     end
 *)


structure DveHashFunctionCompiler: sig

    val gen: System.system -> string list

    val genHashComponents: System.system -> string list

    val genHashLargeComponents: System.system -> string list

end = struct

open DveCompilerUtils

fun hashIntList [] = "   0w37"
  | hashIntList (int :: ints) =
    String.concat [ "   (hashInt ", int, "\n", hashIntList ints, ")" ]

fun gen s = let
    val comps = buildStateComps s
    val comps = List.filter (fn c => not (isCompConst c)) comps
    val (ints, compsStr) = genAllComps (comps, NONE)
    val map = buildMapping comps
in
    [
     "structure DveHashFunction: HASH_FUNCTION = struct\n",
     "open DveDefinitions\n",
     "type state = DveModel.state\n",
     "fun hashInt i h = Word.<<(h, 0w5) + h + (Word.fromInt i) + 0w720\n",
     "fun hash (", compsStr, ": state) = (\n", hashIntList ints, ")\n",
     "end\n\n"
    ]
end

fun modifyingEvents sys events comp = let
    fun filter pred events = let
	fun getProcAndTrans (LOCAL (_, p, t)) = [ (p, t) ]
	  | getProcAndTrans (SYNC (_, _, p, t, q, t')) = [ (p, t), (q, t') ]
    in
	List.filter (fn e => List.exists pred (getProcAndTrans e)) events
    end
in
    case comp
     of PROCESS_STATE p =>
	filter (fn (q, t) =>
		   q = p andalso (Trans.getSrc t) <> (Trans.getDest t))
	       events
      | LOCAL_VAR (p, var) => let
	    fun modify t = isSome (List.find (fn v' => Var.getName var = v')
					     (Trans.modifiedVars t))
	in
	    filter (fn (q, t) => q = p andalso modify t) events
	end
      | GLOBAL_VAR var => let
	    val v = Var.getName var
	    fun modify (p, t) =
		isSome (List.find (fn v' => v = v') (Trans.modifiedVars t))
		andalso
		(not (Process.hasLocalVariable (System.getProc (sys, p)) v))
	in
	    filter (fn (p, t) => modify (p, t)) events
	end
end

fun genHashComponents s =  let
    val events = buildEvents s
    val comps = List.filter (fn c => not (isCompConst c)) (buildStateComps s)
    val comps = #2 (List.foldl (fn (c, (id, l)) => (id + 1, (id, c) :: l))
			       (0, []) comps)
    val comps = List.rev comps
    fun hashComp (_, comp) = let
	val (ints, def) = genOneComp comp
    in
	[
	 "(*****)\n",
	 "fun hash", getCompName comp,
	 " ({ ", def, ",... }: DveModel.state) = (\n",
	 hashIntList ints, ")\n",
	 "(*****)\n\n"
	]
    end
    val compsSorted = List.map
			  (fn (i, c) =>
			      (i, List.length (modifyingEvents s events c), c))
			  comps
    val compsSorted = List.filter (fn (_, n, _) => n > 0) compsSorted
    val compsSorted = ListMergeSort.sort
			  (fn ((_, n1, _), (_, n2, _)) => n1 > n2)
			  compsSorted
in
    [
     "structure DveComponentsHashFunction = struct\n",
     "open DveDefinitions\n",
     "fun hashInt i h = Word.<<(h, 0w5) + h + (Word.fromInt i) + 0w720\n"
    ]
    @
    (List.concat (List.map hashComp comps))
    @
    [
     "val hash = [\n",
     List.foldl (fn ((n, c), str) =>
		    String.concat [ if str = "" then "" else str ^ ",\n",
				    "   (", Int.toString n,
				    ", hash", getCompName c, ")" ])
		"" comps,
     "\n]\n\n",
     "val hashHeuristic = [\n",
     List.foldl (fn ((id, n, c), str) =>
		    String.concat [ if str = "" then "" else str ^ ",\n",
				    "   (",
				    Int.toString id, ", ",
				    Int.toString n, ", ",
				    "hash", getCompName c, ")" ])
		"" compsSorted,
     "\n]\n",
     "end\n\n"
    ]
end

fun genHashLargeComponents s =  let
    val events = buildEvents s
    val comps = List.filter (fn c => not (isCompConst c)) (buildStateComps s)
    val comps = buildLargeStateComps s
    val comps = #2 (List.foldl (fn (c, (id, l)) => (id + 1, (id, c) :: l))
			       (0, []) comps)
    val comps = List.rev comps
    fun hashComp (_, comp) = let
	val l = List.map genOneComp (case comp of GLOBAL c => c
						| PROCESS (_, c) => c)
	val ints = List.concat (List.map #1 l)
	val defs = case l
		    of [] => ""
		     | l  => ListFormat.fmt { init  = "",
					      final = ", ",
					      sep   = ", ",
					      fmt   = #2 } l
    in
	[
	 "(*****)\n",
	 "fun hash", getLargeCompName comp,
	 " ({ ", defs, "... }: DveModel.state) = (\n",
	 hashIntList ints, ")\n",
	 "(*****)\n\n"
	]
    end
    fun count comp = let
	fun cmp (LOCAL _, SYNC _) = GREATER
	  | cmp (SYNC _, LOCAL _) = LESS
	  | cmp (LOCAL (i, _, _), LOCAL (j, _, _)) =
	    if i > j then GREATER else if i < j then LESS else EQUAL
	  | cmp (SYNC (i, j, _, _, _, _), SYNC (i', j', _, _, _, _)) =
	    if i > i' then GREATER else if i < i' then LESS else
	    if j > j' then GREATER else if j < j' then LESS else EQUAL
	    
	val c = case comp of GLOBAL c => c
			   | PROCESS (_, c) => c
	val l = List.concat (List.map (fn c => modifyingEvents s events c) c)
	val l = ListMergeSort.uniqueSort cmp l
    in
	List.length l
    end		     
    val compsSorted = List.map (fn (id, c) => (id, count c, c)) comps
    val compsSorted = List.filter (fn (_, n, _) => n > 0) compsSorted
    val compsSorted = ListMergeSort.sort
			  (fn ((_, n1, _), (_, n2, _)) => n1 > n2)
			  compsSorted
in
    [
     "structure DveLargeComponentsHashFunction = struct\n",
     "open DveDefinitions\n",
     "fun hashInt i h = Word.<<(h, 0w5) + h + (Word.fromInt i) + 0w720\n"
    ]
    @
    (List.concat (List.map hashComp comps))
    @
    [
     "val hash = [\n",
     List.foldl (fn ((n, c), str) =>
		    String.concat [ if str = "" then "" else str ^ ",\n",
				    "   (", Int.toString n,
				    ", hash", getLargeCompName c, ")" ])
		"" comps,
     "\n]\n\n",
     "val hashHeuristic = [\n",
     List.foldl (fn ((id, n, c), str) =>
		    String.concat [ if str = "" then "" else str ^ ",\n",
				    "   (",
				    Int.toString id, ", ",
				    Int.toString n, ", ",
				    "hash", getLargeCompName c, ")" ])
		"" compsSorted,
     "\n]\n",
     "end\n\n"
    ]
end

end
