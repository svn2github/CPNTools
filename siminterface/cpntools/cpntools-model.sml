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
* Module:       Model interface for CPN models
*
* Description:  Main functor for generating the MODEL interface for 
*               CPN model. 
*
* Todo:         This implementation does not exploit locality
*)

functor CPNToolsModel (structure CPNToolsState : CPN'STATE
structure CPNToolsEvent : CPN'EVENT) :
sig
    include GAME_MODEL
    
    val markDirty : unit -> unit
end =
struct
    type state = CPNToolsState.state
    type event = CPNToolsEvent.event
    type internal_state = CPNToolsState.internal_state
    type internal_event = event

    exception EventNotEnabled

    (* --- initial state is the state of the simulator when functor is instantiated --- *)
    val initial = CPNToolsState.getState()

    (* --- current state is used to keep track of the current state of the simulator. 
           This allows current state to be obtained and set in many cases without copying 
           to/from the real simulator --- *)
    val current_state = ref initial

    (* --- flag used to signal whether current_state is valid, 
           i.e., equals state of the underlying simulator  --- *)
    val dirty = ref false

    fun markDirty() = dirty := true

    fun setState state =
        if (not (!dirty)) andalso state = (!current_state)
        then ()
        else 
            let
                val _ = CPNToolsState.setState(state)
                val _ = current_state := state
                val _ = dirty := false
            in
                ()
            end

    fun getState' () =
        if !dirty
        then
            let
                val _ = current_state := (CPNToolsState.getState ())
                val _ = dirty := false
            in
                [(!current_state, CPNToolsEvent.getEnabled())]
            end
        else [(!current_state, CPNToolsEvent.getEnabled())]

    fun getCurrentState () =
        if !dirty
        then
            let
                val _ = current_state := (CPNToolsState.getState ())
                val _ = dirty := false
             in
                 !current_state
            end
        else !current_state

    fun getEvents state =
    let
        val _ = setState state
    in
        CPNToolsEvent.getEnabled()
    end

    fun getInitialStates () =
        (setState initial; getState' ())

    fun execute event =
    let
        val _ = CPNToolsEvent.execute event
        handle _ => (dirty := true; raise EventNotEnabled)
    in
        dirty := true
    end


    fun nextStates (state, event) =
        (setState state; execute event; getState'())

    fun executeSequence (state, events) =
    let
        val _ = setState state
        val _ = List.map execute events
    in
        getState' ()
    end

    val stateToString = CPNToolsState.toString
    val eventToString = CPNToolsEvent.toString
    val controllable = CPNToolsEvent.controllable
    fun winning (state, _) = CPNToolsState.winning state

    fun internalizeEvent e = e
    fun externalizeEvent e = e
    
    fun internalizeState s = CPNToolsState.internalize s
    fun externalizeState s = CPNToolsState.externalize s

end
