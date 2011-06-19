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
functor BuchiProductModel(structure Model: MODEL
structure BuchiModel: BUCHI_MODEL
where type subjectState = Model.state * Model.event list) : GAME_MODEL =
struct
    type event = Model.event
    type state = BuchiModel.state * Model.state

    exception EventNotEnabled = Model.EventNotEnabled
    exception Error of string

    fun combine (ltls, cpns) = 
    let
        open PLTLSyntax
        fun app (ATOMIC ap) s = ap s
          | app (NOT(ATOMIC f)) s = Bool.not (f s)
          | app TRUE _ = true
          | app FALSE _ = false
          | app _ _= raise Error "Unexpected construct"
        fun combine' ((ltl, events)::ltls, cpns) acc =
          let
              val preds = List.map app (BuchiModel.getPredicates ltl)
              fun test state [] = true
                | test state (pred::preds) =
                if pred state
                then test state preds
                else false
              fun combine'' (cpn::cpns) acc =
                  if test cpn preds
                  then
                      let
                          val evts = if List.null events
                                     then []
                                     else #2 cpn
                      in
                          combine'' cpns (((ltl, #1 cpn), evts)::acc)
                      end
                  else combine'' cpns acc
                | combine'' [] acc = acc
          in 
              combine' (ltls, cpns) (combine'' cpns acc)
          end
         | combine' ([],_) acc = acc
    in
        combine' (ltls, cpns) []
    end

    (* val controllable : event -> bool *)
    fun controllable e = false

    (* val winning : (state * event list) -> bool *)
    fun winning ((s, _), _) = BuchiModel.winning (s,[])

    (* val getInitialStates : unit -> (state * event list) list *)
    fun getInitialStates () = 
	let
	    val ltlInitials = BuchiModel.getInitialStates ()
	    val cpnInitials = Model.getInitialStates ()
	in
	    combine (ltlInitials,cpnInitials)
	end

    (* unit -> state *)
    fun getCurrentState () = #1 (List.hd(getInitialStates ()))
    (* unit -> event list *)
    fun getEvents s = [] (* TODO ?? *)
		   

    (* val nextStates : state * event -> (state * event list) list *)
    fun nextStates ((ls,cs), e : event) =
        combine (BuchiModel.nextStates (ls, List.hd (BuchiModel.getEvents ls)),
                 Model.nextStates (cs, e))

    (* val executeSequence : state * event list -> (state * event list) list *)
    (* HACK! Used by SimpleLTLChecker to get complete trace *)
    fun executeSequence (_,el) = 
	let 
	    val cpns = Model.executeSequence (#1(List.hd(Model.getInitialStates ())), el) 
	    val initBuchi = #1(List.hd(BuchiModel.getInitialStates ()))
	in 
	    List.map (fn (cs,cel) => ((initBuchi,cs),cel)) cpns
	end

    (* val stateToString : state -> string *)
    fun stateToString (ls,cs) = (BuchiModel.stateToString ls)^" : "^(Model.stateToString cs)

    (* val eventToString : event -> string *)
    fun eventToString e = Model.eventToString e

end

