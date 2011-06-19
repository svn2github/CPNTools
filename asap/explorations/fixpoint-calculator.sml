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
signature PROPERTY =
sig
    type state
    type event
    type value

    val initial : state * event list -> value
    val update : state * event * state -> value -> value
    val evaluate : value -> bool
    val satisfies : state * event list -> bool
end

functor False (
structure Model : MODEL
): PROPERTY =
struct
    type state = Model.state
    type event = Model.event
    type value = unit

    fun initial _ = ()
    fun update _ _ = ()
    fun evaluate _ = false
    fun satisfies _ = false
end

functor True (
structure Model : MODEL
): PROPERTY =
struct
    type state = Model.state
    type event = Model.event
    type value = unit

    fun initial _ = ()
    fun update _ _ = ()
    fun evaluate _ = true
    fun satisfies _ = true
end

functor Safety (
structure Model : MODEL
val property : Model.state -> bool
): PROPERTY =
struct
    type state = Model.state
    type event = Model.event
    type value = bool

    fun initial (state, events) = property state

    fun update _ _ = true

    fun evaluate value = value

    val satisfies = initial
end

functor DeadExists (
structure Model : MODEL
): PROPERTY =
struct
    type state = Model.state
    type event = Model.event
    type value = bool

    fun initial (_, []) = true
      | initial _ = false

    fun update _ _ = true

    fun evaluate value = value

    val satisfies = initial
end

functor DeadInevitable (
structure Model : MODEL
): PROPERTY =
struct
    type state = Model.state
    type event = Model.event
    type value = int

    fun initial (_, events) = List.length events

    fun update _ n = n - 1
    fun evaluate 0 = true
      | evaluate _ = false
    fun satisfies (_, []) = true
      | satisfies _ = false
end

functor SafetyGame (
structure Model : GAME_MODEL
val property : Model.state -> bool
) =
struct
    type state = Model.state
    type event = Model.event
    type value = int * bool

    fun initial (_, events) =
    let
        fun count [] = 0
          | count (evt::evts) =
          if Model.controllable evt
          then 1 + (count evts)
          else count evts
    in
        (count events, false)
    end

    fun update (_, event, _) (controllable, uncontrollable) =
        if Model.controllable event
        then (controllable - 1, uncontrollable)
        else (controllable, uncontrollable)

    fun evaluate (0, true) = true
      | evaluate _ = false

    fun satisfies (state, events) = property state
end


functor GameWinning (
structure Model : GAME_MODEL
) =
struct
    type state = Model.state
    type event = Model.event
    type value = int * bool

    fun initial (_, events) =
    let
        fun count [] = 0
          | count (evt::evts) =
          if Model.controllable evt
          then count evts
          else 1 + (count evts)
    in
        (count events, false)
    end

    fun update (_, event, _) (uncontrollable, controllable) =
        if Model.controllable event
        then (uncontrollable, true)
        else (uncontrollable - 1, controllable)

    fun evaluate (0, true) = true
      | evaluate _ = false

    val satisfies = Model.winning
end

functor FixPointCalculator (
structure Model : MODEL
structure Storage : STORAGE
structure True : STORAGE
structure WaitingSet : WAITINGSET
structure Property : PROPERTY
structure HashFunction : HASH_FUNCTION
sharing type Model.state = Storage.item
sharing type Model.state = True.item
sharing type Property.state = Model.state
sharing type Property.event = Model.event
sharing type HashFunction.state = Model.state
) =
struct
    local
    type hash_table = (Model.state, (((Model.state * Model.event list) * Model.event) list) * Property.value) HashTable.hash_table
    exception TerminateEarly of unit Storage.storage * unit True.storage
    in
    fun calculate early init_options1 init_options2 initial_states =
    let
        fun addAll ((a, bs), waiting) =
            List.foldl
            (fn (b, waiting) =>  WaitingSet.enqueue (waiting, ((a, bs), b)))
            waiting
            bs

        val addList =
            List.foldl (fn (elm, waiting) => WaitingSet.enqueue (waiting, elm))

        fun addDependency dependency ((state, events), dependencies) =
        let
            val (previous, value) = 
                case HashTable.find dependencies state
                  of SOME value => value
                   | NONE => ([], Property.initial (state, events))
            val _ = HashTable.insert dependencies (state, (List.@(dependency, previous), value))
        in
            dependencies
        end

        fun getValue dependencies state =
        let
            val (_, result) = HashTable.lookup dependencies state
        in
            result
        end

        fun setValue dependencies (state, value) =
        let
            val (previous, _) = HashTable.lookup dependencies state
            val _ = HashTable.insert dependencies (state, (previous, value))
        in
            dependencies
        end

        fun getDependencies dependencies state =
        let
            val (result, _) = HashTable.lookup dependencies state
        in
            result
        end

        fun equals (a, b) = a = b

        val storage = Storage.emptyStorage init_options1 ()
        val storage' = #2 (Storage.addList (storage, List.map (fn (a, b) => a)
        initial_states))
        val satisfied = True.emptyStorage init_options2 ()
        val dependencies = HashTable.mkTable (HashFunction.hash, equals) (1000, LibBase.NotFound) : hash_table
        val dependencies' = List.foldl (addDependency []) dependencies initial_states
        val waiting = WaitingSet.empty
        val waiting' = List.foldl addAll waiting initial_states

        fun allSatisfied satisfied =
            List.all
            (fn (state, events) => Property.satisfies (state, events) orelse 
                                   True.contains (satisfied, state))
            initial_states

      val testEarly =
        if early then (fn storage => fn satisfied =>
                       if allSatisfied satisfied
                       then raise TerminateEarly (storage, satisfied)
                       else ())
                 else (fn _ => fn _ => ())


        fun satisfies satisfied (state, events) =
            Property.satisfies (state, events) orelse
            True.contains (satisfied, state)

        fun walk storage satisfied dependencies waiting =
            if WaitingSet.isEmpty waiting
            then (storage, satisfied)
            else
                let
                    val (waiting', ((state, events), event)) = WaitingSet.dequeue waiting
                    val next = Model.nextStates (state, event)
                    fun walk' ((state', events'), (storage, satisfied, dependencies, waiting)) =
                        if Storage.contains (storage, state')
                        then (* re-evaluate state *)
                            let
                                val value = getValue dependencies state
                                val satisfy = satisfies satisfied (state, events)
                                val satisfy' = satisfies satisfied (state', events')
                                val (dependencies', value') = 
                                    if satisfy' andalso (not satisfy)
                                    then 
                                        let
                                            val value' = Property.update (state, event, state') value
                                            val dependencies' = setValue dependencies (state, value')
                                        in 
                                            (dependencies', value')
                                        end
                                    else (dependencies, value)
                                val (waiting', satisfied') =
                                    if (not satisfy) andalso Property.evaluate value'
                                    then
                                        let
                                            val previous = getDependencies dependencies state
                                            val waiting' = addList waiting previous
                                            val satisfied' = #3 (True.add
                                            (satisfied, state))
                                            val _ = testEarly storage satisfied'
                                        in
                                            (waiting', satisfied')
                                        end
                                    else (waiting, satisfied)
                                val dependencies'' =
                                    if satisfy'
                                    then dependencies'
                                    else addDependency [((state, events), event)] ((state', events'), dependencies')
                            in
                                (storage, satisfied', dependencies'', waiting')
                            end
                        else (* state is new *)
                            let
                                val storage' = #3 (Storage.add (storage,
                                state'))
                                val dependencies' =
                                    addDependency [((state, events), event)] ((state', events'), dependencies)
                                val waiting' = addAll ((state', events'), waiting)
                                val waiting'' =
                                    if Property.satisfies (state', events')
                                    then WaitingSet.enqueue (waiting', ((state, events), event))
                                    else waiting'
                            in
                                (storage', satisfied, dependencies', waiting'')
                            end
                    val (storage', satisfied', dependencies', waiting'') =
                        List.foldl walk' (storage, satisfied, dependencies, waiting') next
                in
                    walk storage' satisfied' dependencies' waiting''
                end

        val (storage'', satisfied') = walk storage' satisfied dependencies' waiting'
                                      handle TerminateEarly result => result
    in
        (storage', satisfied', allSatisfied satisfied')
    end
    end
end
