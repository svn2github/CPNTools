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
functor SafetyChecker(
structure Exploration : SIMPLE_TRACE_EXPLORATION
) = struct
    open Exploration

    local
        exception Done of
                 (state * (state * event) list) list

        fun pre_trace_hook_gather (((s, _), ev, _), _, _, rest, storage) = 
            ((s, ev)::rest, storage)
        fun pre_trace_hook_ignore (_, _, _, _, storage) = 
            ([], storage)

        fun state_hook max process property ((state, events), trace,
					       (rest, errors)) =
            if property (state, events)
            then (rest, errors)
	    else let val rest = process ((state, trace), rest)
		 in
		     if errors = max
		     then raise Done rest
		     else (rest, errors + 1)
		 end
        
    in
        fun explore build_trace max_errors init_options initial_states
            process property = let
	    val (result, _) =
            #2 (Exploration.explore
            {a_initial = (), arc_hook = fn _ => (),
             s_initial = ([], 1),
	     state_hook = state_hook max_errors process property,
            t_initial = [],
            post_trace_hook = fn (_, _, _, rest, storage) => (List.tl rest, storage),
            pre_trace_hook = if build_trace
                             then pre_trace_hook_gather
                             else pre_trace_hook_ignore}
            init_options
            initial_states)
	in
	    result
	end
            handle Done result => result
    end
end

functor SimpleSafetyChecker(
structure Exploration: SIMPLE_EXPLORATION) = struct
local
    open Exploration
    exception Done of
              (state * (state * event) list) list

    fun state_hook property ((state, events), errors) =
        if property (state, events)
        then errors
        else state::errors

    fun state_hook maxErrors process property ((state, events),
					 (rest, errors)) =
        if property (state, events)
        then (rest, errors)
        else let val rest = process ((state, []), rest)
             in
                 if errors = maxErrors
                 then raise Done rest
                 else (rest, errors + 1)
             end
in

fun explore maxErrors init_options initial_states process property = let
    val (result, _) =
        #2 (Exploration.explore
		{a_initial = (),
		 arc_hook = fn _ => (),
		 s_initial = ([], 1),
		 state_hook = state_hook maxErrors process property }
                init_options
		initial_states)
in
    result
end
    handle Done result => result
end

end
