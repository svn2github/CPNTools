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
functor WaitingSetExploration (
structure Model : MODEL
structure Storage : STORAGE
structure WaitingSet : WAITINGSET
sharing type Model.state = Storage.item
) : TRACE_EXPLORATION =
struct
    type state = Model.state
    type event = Model.event
    type id = Storage.id
    type 'a storage = 'a Storage.storage
		      
    fun explore transform_arc transform_state
        {state_hook, s_initial,
        arc_hook, a_initial,
        pre_trace_hook, post_trace_hook, t_initial} 
        storage initial_states =
        let
            fun walk' ((state, events), id, t_value) (event, (waiting, storage, s_value, a_value)) =
            let
                val s = (state, events)
                val successors = Model.nextStates (state, event)
                fun walk'' ((state, events), (waiting, storage, s_value, a_value)) =
                let
                    val s' = transform_state (state, events)
                    val (state', _) = s'
                    val s'' = (state', transform_arc s')
                    val arc = (s, event, s'')
                    val a_value' = arc_hook (arc, t_value, a_value)
                    val (id', seen, storage') = Storage.add (storage, state')
                in
                    if seen
                    then (waiting, storage', s_value, a_value')
                    else
                        let
                            val (t_value', storage'') =
                                pre_trace_hook (arc, id, id', t_value, storage')
                            val s_value' = state_hook (s'', t_value', s_value)
                            val waiting' =
                                WaitingSet.enqueue (waiting, (s'', id', t_value'))
                        in
                            (waiting', storage'', s_value', a_value')
                        end
                end
            in
                List.foldl walk'' (waiting, storage, s_value, a_value) successors
            end
            fun walk (waiting, storage, s_value, a_value) =
                if WaitingSet.isEmpty waiting
                then (storage, s_value, a_value)
                else
                    let
                        val (waiting', ((state, events), id, t_value)) = WaitingSet.dequeue waiting
                        val result =
                            List.foldl (walk' ((state, events), id, t_value))
                            (waiting', storage, s_value, a_value) events
                    in
                        walk result
                    end
            fun initial ((state, events), (waiting, storage, s_value)) =
            let
                val s' = transform_state (state, events)
                val (state', _) = s'
                val (id, seen, storage') = Storage.add (storage, state')
            in
                if seen
                then (waiting, storage', s_value)
                else
                    let
                        val s_value' = state_hook (s', t_initial, s_value)
                        val events' = transform_arc s'
                        val waiting' =
                            WaitingSet.enqueue (waiting, ((state', events'), id, t_initial))
                    in
                        (waiting', storage', s_value')
                    end
            end
            val waiting = WaitingSet.empty
            val (waiting', storage', s_value) =
                List.foldl initial (waiting, storage, s_initial) initial_states
        in
            walk (waiting', storage', s_value, a_initial)
        end
end
