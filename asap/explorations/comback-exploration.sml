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
 * TODO:             If #initial_states storage != initial_states
 *                   unexpected things may happen
 *)
functor ComBackExploration(
structure Exploration : TRACE_EXPLORATION where type id = int
structure Storage : COMBACK_STORAGE where type state = Exploration.state *
Exploration.event list and type id = Exploration.id
sharing type Storage.storage = Exploration.storage
sharing type Exploration.state = Storage.item
sharing type Exploration.event = Storage.event
) : TRACE_EXPLORATION = struct
    open Exploration
	 
    fun explore transform_arc transform_state
		{state_hook, s_initial, arc_hook, a_initial, pre_trace_hook,
		 post_trace_hook, t_initial} 
		storage initial_states =
        let
            fun pre_trace_hook' ((CPN's, CPN'ev, CPN's'), id, id', t_value, storage) =
		let
		    (*
		     val _ = print "Adding back-edge from "
                     val _ = print (Int.toString number)
                     val _ = print " to "
                     val _ = print (Int.toString from)
                     val _ = print "\n"
		     *)
                    (*val number = Storage.numItems storage
		    val _ = print "Adding back-edge from "
                    val _ = print (Int.toString id')
                    val _ = print " to "
                    val _ = print (Int.toString number)
                    val _ = print "\n"*)
		    val storage' = Storage.addBackedge storage (id', CPN's', (CPN'ev, id))
		in
		    pre_trace_hook ((CPN's, CPN'ev, CPN's'), id, id', t_value, storage')
		end		
        in
            Exploration.explore transform_arc transform_state
	    {state_hook = state_hook, s_initial = s_initial, arc_hook = arc_hook,
            a_initial = a_initial, pre_trace_hook = pre_trace_hook',
            post_trace_hook = post_trace_hook, t_initial = t_initial }
            storage initial_states
        end
end

functor ComBackExplorationNoArcs(
structure Exploration : TRACE_EXPLORATION where type id = int
structure Storage : COMBACK_STORAGE where type state = Exploration.state *
Exploration.event list and type event = int and type id = Exploration.id
sharing type Storage.storage = Exploration.storage
sharing type Exploration.state = Storage.item
			      
) : TRACE_EXPLORATION =
struct


    open Exploration

    fun explore transform_arc transform_state
        {state_hook, s_initial, arc_hook, a_initial, pre_trace_hook,
        post_trace_hook, t_initial} 
        storage initial_states =
        let
            fun pre_trace_hook' (((CPN's, CPN'evs), CPN'ev, CPN's'), id, id', (trace, t_value), storage) =
            let
                val from = hd trace handle _ => 1
                local
                    exception Found of int
                in
                val edge = 
                    (List.foldl 
                         (fn (event, no) => if CPN'ev = event
                                            then raise Found no
                                            else no + 1
                         ) 0 CPN'evs) handle Found no => no
                end
                val storage' = Storage.addBackedge storage (id', CPN's', (edge, from))
                val (t_value', storage'') = pre_trace_hook (((CPN's, CPN'evs),
                CPN'ev, CPN's'), id, id', t_value, storage')
            in
                ((id'::trace, t_value'), storage'')
            end

            fun post_trace_hook' (CPN's, id, id', (trace, t_value), storage) =
            let
                val (t_value', storage') = post_trace_hook (CPN's, id, id', t_value, storage)
            in
                ((trace, t_value'), storage')
            end

            fun arc_hook' (CPN's, (_, trace), initial) =
                arc_hook (CPN's, trace, initial)

            fun state_hook' (CPN's, (_, trace), CPN'v) =
              state_hook (CPN's, trace, CPN'v)

        in
            Exploration.explore transform_arc transform_state
            { state_hook = state_hook', s_initial = s_initial, arc_hook = arc_hook',
            a_initial = a_initial, pre_trace_hook = pre_trace_hook',
            post_trace_hook = post_trace_hook', t_initial = ([], t_initial) }
            storage initial_states
        end
end
