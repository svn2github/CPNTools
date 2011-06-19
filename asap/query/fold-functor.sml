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

functor StateFold (
structure Exploration: SIMPLE_EXPLORATION
structure StateFold : STATE_FOLD_FUNCTION
where type state = Exploration.state and type event = Exploration.event
) = struct
    local
        open Exploration
    in
        fun fold storage initial_states =
        let
            val (_, result, _) =
                Exploration.explore { a_initial = (), arc_hook = fn _ => (),
                s_initial = StateFold.initial,
                state_hook = StateFold.fold}
                storage initial_states
        in
            result
        end
        handle StateFold.Done result => result
    end
end

functor FilterStates(
structure StateFold : STATE_FOLD_FUNCTION
val filter : (StateFold.state * StateFold.event list) * StateFold.value -> bool
) : STATE_FOLD_FUNCTION =
struct
    open StateFold

    fun fold (state, value) =
        if filter (state, value)
        then StateFold.fold (state, value)
        else value
end

functor LimitStates(
structure StateFold : STATE_FOLD_FUNCTION
val limit : int
) : sig
    include STATE_FOLD_FUNCTION

    val reset : unit -> unit
    end =
struct
    val count = ref 0

    fun reset () = (count := 0)

    fun filter (state, value) =
        (count := (!count) + 1;
        if (!count) >= limit
        then raise StateFold.Done (StateFold.fold (state, value))
        else true)

    structure StateFold' = FilterStates(
    structure StateFold = StateFold
    val filter = filter)

    open StateFold'
end


functor EventFold (
structure Exploration: SIMPLE_EXPLORATION
structure EventFold : EVENT_FOLD_FUNCTION
where type state = Exploration.state and type event = Exploration.event
) = struct
    local
        open Exploration
    in
        fun fold storage initial_states =
        let
            val (_, _, result) =
                Exploration.explore { s_initial = (), state_hook = fn _ => (),
                a_initial = EventFold.initial,
                arc_hook = EventFold.fold}
                storage initial_states
        in
            result
        end
        handle EventFold.Done result => result
    end
end

