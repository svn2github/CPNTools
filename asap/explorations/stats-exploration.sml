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
functor IntermediateStatsExploration(
structure JavaExecute : JAVA_EXECUTE
structure Exploration : TRACE_EXPLORATION
) : TRACE_EXPLORATION =
struct
    open JavaExecute
    open Exploration

    val arcs = ref 0
    val states = ref 0
    val varcs = ref 0
    val lastTime = ref ~1
    val start = ref (Time.now ())

    fun send name spent =
        execute name [vINT (!arcs), vINT (!states), vINT (!varcs), vINT (!arcs), vINT spent]

    fun transmit () =
    let
        val spent = Real.floor (Time.toReal (Time.-(Time.now(), !start)))
        val time = if !lastTime <> spent
                   then (lastTime := spent; true)
                   else false
    in
        if time
        then send "intermediate" spent
        else ()
    end

    fun transform_arc old arc =
    let
        val _ = varcs := (!varcs + 1)
        val _ = transmit ()
    in
        old arc
    end

    fun arc_hook old arc =
    let
        val _ = arcs := (!arcs + 1)
        val _ = transmit ()
    in
        old arc 
    end

    fun state_hook old state =
    let
        val _ = states := (!states + 1)
        val _ = transmit ()
    in
        old state
    end

    fun explore transform_arc_old transform_state
        {state_hook = state_hook_old, s_initial,
        arc_hook = arc_hook_old, a_initial,
        pre_trace_hook, post_trace_hook, t_initial} 
        storage initial_states =
        let
            val _ = start := (Time.now())
            val _ = transmit()
            fun done () =
            let
                val spent = Real.floor (Time.toReal (Time.-(Time.now(), !start)))
            in
                send "statistics" spent
            end
            val result = 
                Exploration.explore (transform_arc transform_arc_old) transform_state
                { state_hook = state_hook state_hook_old, s_initial = s_initial,
                arc_hook = arc_hook arc_hook_old, a_initial = a_initial, pre_trace_hook
                = pre_trace_hook, post_trace_hook = post_trace_hook, t_initial = t_initial} storage initial_states
                handle ex => (done(); raise ex)
            val _ = done ()
        in
            result
        end
end
