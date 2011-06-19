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
signature BUCHI_MODEL =
sig
    include GAME_MODEL

    type subjectState

    val getPredicates : state -> (subjectState -> bool) PLTLSyntax.formula list
    val getLabels : state -> string list
end


functor BuchiSimulator(structure Expression : BUCHI_EXPRESSION) : BUCHI_MODEL =
struct
    type event = unit
    type state = int
    type subjectState = Expression.state

    exception EventNotEnabled
    exception Error of string

    fun getLabels s = Vector.sub (Expression.labels, s)

    fun getPredicates s = #2 (Vector.sub (Expression.transitionTable,s))

    (* event -> bool *)
    fun controllable (e : event) = false
    (* (state * event list) -> bool *)
    fun winning (s, _) = List.exists(fn i => i=s) Expression.accepting

    fun transitions s = case Vector.sub (Expression.transitionTable, s) 
			 of ([],_) => (s,[]) (* No transitions -> disabled *)
			  | _ => (s,[()])    (* Some transitions -> enabled *)

    (* unit -> (state * event list) list *)
    fun getInitialStates () = List.map transitions Expression.initials

    (* state * event -> (state * event list) list *)
    fun nextStates (s,_) = 
	let
	  val t = Vector.sub (Expression.transitionTable, s)
	in
	    List.map transitions (#1 t)
	end

    (* unit -> state *)
    fun getCurrentState () = List.hd Expression.initials

    (* unit -> event list *)
    fun getEvents s = #2(transitions s)

    (* state * event list -> (state * event list) list *)
    fun executeSequence l = raise Error "Not implemented" (* TODO *)
			    
    (* state -> string *)
    fun stateToString (s : state) =
	let
	    fun labelsToString (l::[]) = l
	      | labelsToString (l::ls) = l^", "^(labelsToString ls)
	      | labelsToString [] = ""
	    val s' = if (winning (s,[()]))
		     then "Accept "
		     else ""
	    val s'' = if List.exists (fn ss => ss = s) (List.map (fn (ss',_) => ss') (getInitialStates ()))
		      then "Initial: "^s'
		      else s'
	in 
	    s''^"["^(labelsToString (getLabels s))^"]"
	end				    

    (* event -> string *)
    fun eventToString (e : event) = "" 

end


functor ProductHashFunction(structure Hash: HASH_FUNCTION) : HASH_FUNCTION =
struct
    type state = int * Hash.state
    fun hash (s, t) = (Hash.hash t) + (Word31.fromInt s)
end

structure BuchiHashFunction : HASH_FUNCTION =
struct
    type state = int
    fun hash s = Word31.fromInt s
end
