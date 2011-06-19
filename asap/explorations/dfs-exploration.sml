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
functor DFSExploration(
structure Model : MODEL
structure Storage : STORAGE
sharing type Model.state = Storage.item
) : TRACE_EXPLORATION = struct

    type id = Storage.id
    type state = Model.state
    type event = Model.event
    type 'a storage = 'a Storage.storage

    fun explore transform_arc transform_state
        {state_hook, s_initial,
        arc_hook, a_initial,
        pre_trace_hook, post_trace_hook, t_initial} 
        storage initial_states =
        let
            fun dfs trace ((state, events), id, (storage, s_value, a_value)) =
            let
                fun loop (event, (storage, s_value, a_value)) =
                let
                    val successors = Model.nextStates (state, event)
                    fun explore_one ((state', events'), (storage, s_value, a_value)) =
                    let
                        val (state'', events'') = transform_state (state', events')
                        val s'' = (state'', transform_arc (state'', events''))
                        val arc = ((state, events), event, s'')
                        val a_value' = arc_hook (arc, trace, a_value)
                        val (id', seen, storage') = Storage.add (storage, state'')
                    in
                        if seen
                        then (storage', s_value, a_value)
                        else
                            let
                                val (trace', storage'') = pre_trace_hook (arc, id, id', trace, storage')
                                val s_value' = state_hook (s'', trace', s_value)
                                val (storage'', s_value'', a_value'') =
                                    dfs trace' (s'', id', (storage'', s_value', a_value'))
                                val (trace'', storage''') = post_trace_hook (arc, id, id', trace', storage'')
                            in
                                (storage''', s_value'', a_value'')
                            end
                    end
                in
                    List.foldl explore_one (storage, s_value, a_value) successors
                end
            in
                List.foldl loop (storage, s_value, a_value) events
            end
            fun initial ((state, events), (storage, s_value, a_value)) =
            let
                val (state', events') = transform_state (state, events)
                val (id, seen, storage') = Storage.add (storage, state')
            in
                if seen
                then (storage', s_value, a_value)
                else
                    let
                        val s_value' = state_hook ((state', events'), t_initial, s_value)
                        val events'' = transform_arc (state', events')
                    in
                        dfs t_initial ((state', events''), id, (storage', s_value', a_value))
                    end
            end
        in
            List.foldl initial (storage, s_initial, a_initial) initial_states
        end
end
