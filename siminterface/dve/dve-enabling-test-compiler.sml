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
 *     dve-enabling-test-compiler.sml
 *
 *  Created:
 *     Nov. 14, 2008
 *
 *  Generate:
 *     structure DveEnablingTest: sig
 *        val getEnabledEvents: state -> event list
 *        val getPropertyEnabledEvents: state -> property_event list
 *     end
 *
 *  The second function is only generated if the model contains a property
 *  process.
 *)


structure DveEnablingTestCompiler: sig

    val gen: System.system * bool -> string list

end = struct

open DveCompilerUtils

fun compileGetEnabledEvents getEvents funcName
			    (s as { prop, ... } : System.system, checks) = let

    val events = getEvents s

    val comps = buildStateComps s

    fun compileIsEventEnabled (e, checks) = let
	val mapping = buildMapping comps
	fun compileStateTest (procName, trans) =
	    getCompName (PROCESS_STATE procName) ^ " = " ^
	    getLocalStateName (procName, Trans.getSrc trans)		  
	fun compileStateTest2 (procName1, trans1, procName2, trans2) =
	    String.concat [
	    "(", getCompName (PROCESS_STATE procName1),
	    ",", getCompName (PROCESS_STATE procName2), ") = ",
	    "(", getLocalStateName (procName1, Trans.getSrc trans1),
	    ",", getLocalStateName (procName2, Trans.getSrc trans2), ")" ]
	fun compileGuardTest (procName, trans) =
	    case Trans.getGuard trans
	     of NONE   => ""
	      | SOME e => " andalso 0 <> " ^
			  compileExpr e (SOME procName, mapping, comps, checks)
    in
	case e of
	    LOCAL (_, p, t) =>
	    compileStateTest (p, t) ^ compileGuardTest (p, t)
	  | SYNC  (_, _, p1, t1, p2, t2) =>
	    compileStateTest2 (p1, t1, p2, t2) ^
	    compileGuardTest (p1, t1) ^ compileGuardTest (p2, t2)
    end

    fun testName e = "isEnabled" ^ (getEventName e)

    fun compileIsEnabledTest e =
	[ "\nfun ", testName e, " (", genComps comps, ": state) =\n",
	  "   (", compileIsEventEnabled (e, checks), ")" ]
	
    fun compileTest e =
	[ "   if ", testName e, " st",
	  " then l := ", getEventName e, " :: !l",
	  " else ();\n" ]

    val funcs = List.concat (List.map compileIsEnabledTest events)
    val tests = List.concat (List.map compileTest (List.rev events))
in
    funcs @
    [ "\nfun ", funcName, " (st: state) = let\n",
      "   val l = ref []\n",
      "in\n" ] @
    tests @
    [ "   !l\nend\n" ]
end

fun gen (s as { prop, ... }, checks) =
    [ "structure DveEnablingTest = struct\n",
      "\nopen DveDefinitions\n" ] @
    (compileGetEnabledEvents buildEvents "getEnabledEvents" (s, checks)) @
    (case prop
      of NONE => []
       | SOME _ => compileGetEnabledEvents
		       buildPropertyEvents
		       "getPropertyEnabledEvents" (s, checks)) @
    (case prop
      of NONE => []
       | SOME _ =>
	 [ "local\n",
	   "   fun getEnabled s =\n",
	   "      case (getEnabledEvents s, getPropertyEnabledEvents s)\n",
	   "       of (_, []) => [ ]\n",
           "        | ([], _) => [ DUMMY_EVENT ]\n",
           "        | (evts, _) => evts\n",
	   "in\n",
	   "   val getEnabledEvents = getEnabled\n",
	   "end\n" ]) @
    [ "end\n" ]

end
