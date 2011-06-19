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
 * Module:       PT-net simulator
 *
 * Description:  Code able to simulate a PT-net
 *)
functor PTNetUtilities(
structure PTNet : PT_NET
) =
struct
    exception IllegalSpecification

    val initial_state =
        let
            val places = Vector.fromList PTNet.P
        in
            Vector.map PTNet.M0 places
        end

    fun item_to_number [] n item2 =
        raise IllegalSpecification
      | item_to_number (item1::rest) n item2 =
        if item1 = item2
        then n
        else item_to_number rest (n + 1) item2
    fun position_of lst = item_to_number lst 0

    val transitions =
        let
            val place_number = position_of PTNet.P

            fun pre_compute_transition transition =
                (ListMergeSort.sort Int.> (List.map place_number (PTNet.Pre transition)),
                ListMergeSort.sort Int.> (List.map place_number (PTNet.Post transition)))
        in
            List.map pre_compute_transition PTNet.T
        end
end

functor PTNetModelCommon(
      structure PTNet : PT_NET
) =
struct
    type state = Word.word vector

    exception EventNotEnabled

    structure Utillities = PTNetUtilities(structure PTNet = PTNet)
    open Utillities

    fun enabled state (pre, _) =
    let
        fun has_tokens [] = true
          | has_tokens (n::rest) =
            Word.>=(Vector.sub (state, n), 0w1)
            andalso has_tokens rest
    in
        has_tokens pre
    end

    fun increment amount state positions =
    let
        fun update_one (position, state) =
            Vector.update (state, position, Word.+ (Vector.sub (state,
            position), amount))
    in
        List.foldl update_one state positions
    end

    val minus_one = Word.- (0w0, 0w1)

    fun execute state (pre, post) =
        increment 0w1 (increment minus_one state pre) post
    fun execute' (event, state) = execute state event

    fun stateToString state =
    let
        fun place_to_buffer [] _ = []
          | place_to_buffer (place::rest) number =
            let
                val buffer = place_to_buffer rest (number + 1)
                val tokens = Vector.sub (state, number)
                val name = PTNet.placeToString place
            in
                if tokens > 0w0 andalso not (name = "")
                then
                    name
                    ::(": ")
                    ::(Word.toString tokens)
                    ::("\n")
                    ::buffer
                else buffer
            end
    in
        String.concat (place_to_buffer PTNet.P 0)
    end
end

functor PTNetModel(
      structure PTNet : PT_NET
) : MODEL =
struct
    structure ModelCommon = PTNetModelCommon(structure PTNet = PTNet)
    open ModelCommon
    type event = int list * int list

    fun successors state =
        List.filter (enabled state) transitions

    local
        val initial_enabled = successors initial_state
    in
        fun getInitialStates () = [(initial_state, initial_enabled)]
    end

    fun getCurrentState () = initial_state

    fun getEvents state = successors state

    fun nextStates (state, event) =
    let
        val successor = execute state event
    in
        [(successor, successors successor)]
    end

    fun executeSequence (state, events) =
    let
        val successor = List.foldl execute' state events
    in
        [(successor, successors successor)]
    end

    fun eventToString event =
    let
        val number = position_of transitions event
        val transition = List.nth (PTNet.T, number)
    in
        PTNet.transitionToString transition
    end
end

functor SmartPTNetModel(
      structure PTNet : PT_NET
) : MODEL =
struct
    structure ModelCommon = PTNetModelCommon(structure PTNet = PTNet)
    open ModelCommon
    type event = int

    structure IntOrder : ORD_KEY =
    struct
        type ord_key = int
        val compare = Int.compare
    end

    structure TransitionSet = SplaySetFn(IntOrder)

    val transitions = Vector.fromList ModelCommon.transitions

    fun post_set place =
    let
        fun has elm lst =
            Option.isSome (List.find (fn elm' => elm = elm') lst)
        fun contains_place (number, (pre, post), rest) =
            if has place pre
            then number::rest
            else rest
    in
        Vector.foldli contains_place [] transitions
    end

    fun post_set' [] = TransitionSet.empty
      | post_set' (place::places) =
        TransitionSet.addList ((post_set' places), post_set place)

    val (maybeEnabled, maybeDisabled) =
        List.foldr (fn ((pre, post), (me, md)) =>
        ((post_set' post)::me, (post_set' pre)::md)) ([], []) ModelCommon.transitions

    val maybeEnabled = Vector.fromList maybeEnabled
    val maybeDisabled = Vector.fromList maybeDisabled

    val enabled = ref TransitionSet.empty
    val disabled = ref TransitionSet.empty
    val current_state = ref initial_state

    fun recalculate () =
    let
        val state = !current_state
        fun update_one (number, transition, (en, dis)) =
            if ModelCommon.enabled state transition
            then (number::en, dis)
            else (en, number::dis)
        val (en, dis) = Vector.foldri update_one ([], []) transitions
        val _ = enabled := TransitionSet.addList (TransitionSet.empty, en)
        val _ = disabled := TransitionSet.addList (TransitionSet.empty, dis)
    in
        ()
    end

    val _ = recalculate ()

    fun update state =
        if state = !current_state
        then ()
        else (current_state := state; recalculate ())

    fun getInitialStates () =
    let
        val _ = update initial_state
    in
        [(initial_state, TransitionSet.listItems (!enabled))]
    end


    fun getCurrentState () = !current_state

    fun getEvents state = raise EventNotEnabled

    fun updateEnabled state event =
    let
        val _ = current_state := state
        val recalculate =
            TransitionSet.union (
            TransitionSet.intersection (!enabled, Vector.sub (maybeDisabled, event)),
            TransitionSet.intersection (!disabled, Vector.sub (maybeEnabled, event)))
        val en = TransitionSet.difference (!enabled, Vector.sub (maybeDisabled, event))
        val dis = TransitionSet.difference (!disabled, Vector.sub (maybeEnabled, event))
(*        val _ = print "Needs to reconsider "
        val _ = print (Int.toString (TransitionSet.numItems recalculate))
        val _ = print " items\n"*)
        fun check_one (event, (en, dis)) =
            if ModelCommon.enabled state (Vector.sub (transitions, event))
            then (TransitionSet.add (en, event), dis)
            else (en, TransitionSet.add (dis, event))
        val (en, dis) = TransitionSet.foldl check_one (en, dis) recalculate
        val _ = enabled := en
        val _ = disabled := dis
    in
        ()
    end

    fun nextStates (state, event) =
    let
        val transition = Vector.sub (transitions, event)
        val state' = execute state transition
        val _ = if state = !current_state
                then updateEnabled state' event
                else update state'
    in
        [(state', TransitionSet.listItems (!enabled))]
    end

    fun execute'' (event, state) =
    let
        val transition = Vector.sub (transitions, event)
    in
        execute' (transition, state)
    end

    fun executeSequence (state, [event]) =
        nextStates (state, event)
      | executeSequence (state, events) =
        let
            val state' = List.foldl execute'' state events
            val _ = update state'
        in
            [(state', TransitionSet.listItems (!enabled))]
        end

    fun eventToString event =
    let
        val transition = List.nth (PTNet.T, event)
    in
        PTNet.transitionToString transition
    end
end
