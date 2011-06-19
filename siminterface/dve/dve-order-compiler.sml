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
 *     dve-order-compiler.sml
 *
 *  Created:
 *     Nov. 17, 2008
 *
 *  Generate:
 *     structure DveStateOrder: ORD_KEY = struct
 *        type ord_key = DveModel.state
 *        ...
 *     end
 *     structure DveEventOrder: ORD_KEY = struct
 *        type ord_key = DveModel.event
 *        ...
 *     end
 *)


structure DveOrderCompiler: sig

    val genState: System.system -> string list

    val genEvent: System.system -> string list

end = struct

open DveCompilerUtils

fun compileStateOrder (s: System.system) = let
    val comps = buildStateComps s
    fun cmpBasicItem (comp1, comp2) =
	String.concat [
	"if ", comp1, " < ", comp2, "\n",
	"   then LESS\n",
	"   else if ", comp1, " > ", comp2, "\n",
	"   then GREATER\n"
	]
    val (l1, c1) = genAllComps (comps, SOME "s1")
    val (l2, c2) = genAllComps (comps, SOME "s2")
in
    String.concat [
    "fun compareState (", c1, ": state, ", c2, ": state) =\n   ",
    case comps
     of [] => "EQUAL\n"
      | _  => listFormat { init  = "",
			   final = "   else EQUAL\n",
			   sep   = "   else ",
			   fmt   = cmpBasicItem }
			 (ListPair.zip (l1, l2)) ]
end

fun compileEventOrder (s: System.system) =
    String.concat [
    "fun compareEvent (e1: event, e2: event) = let\n",
    "   val e1 = eventToInt e1\n",
    "   val e2 = eventToInt e2\n",
    "in if e1>e2 then GREATER else if e1=e2 then EQUAL else LESS end\n" ]

fun genState s =
    [
     "structure DveStateOrder: ORD_KEY = struct\n",
     "open DveDefinitions\n",
     "type ord_key = DveModel.state\n",
     compileStateOrder s,
     "val compare = compareState\n",
     "end\n"
    ]

fun genEvent s =
    [
     "structure DveEventOrder: ORD_KEY = struct\n",
     "open DveDefinitions\n",
     "type ord_key = DveModel.event\n",
     compileEventOrder s,
     "val compare = compareEvent\n",
     "end\n"
    ]

end
