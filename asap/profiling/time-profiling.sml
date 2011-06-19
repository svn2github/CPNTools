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
functor ProfiledHash (
structure Subject : HASH_FUNCTION
val name: string
) : sig
    include HASH_FUNCTION
    include PROFILING
end =
struct
    structure Profiling = ProfilingHelp(val name = name)
    open Profiling
    open Subject

    val hashPhase = makePhase "Hash function"
    val hashStat = makeStat "Hash calls"

    val hash = phaseAndStat hashPhase hashStat hash
end

functor ProfiledStorage (
structure Subject : STORAGE
val name: string
) : sig
    include STORAGE
    include PROFILING

    val storageStat : stat
    val storagePhase : phase
end =
struct
    structure Profiling = ProfilingHelp(val name = name)
    open Profiling
    open Subject

    val storageStat = makeStat "Storage"
    val addStat = makeStat' storageStat "Storage.add"
    val addListStat = makeStat' addStat "Storage.addList"
    val containsStat = makeStat' storageStat "Storage.contains"
    val tagStat = makeStat' storageStat "Storage.tag"
    val getTagStat = makeStat' tagStat "Storage.getTag"
    val setTagStat = makeStat' tagStat "Storage.setTag"

    val storagePhase = makePhase "Storage"

    fun emptyStorage a b = phaseAndStat storagePhase storageStat Subject.emptyStorage a b
    fun add a = phaseAndStat storagePhase addStat Subject.add a
    fun addList a = phaseAndStat storagePhase addListStat Subject.addList a
    fun contains a = phaseAndStat storagePhase containsStat Subject.contains a
    fun contains' a = phaseAndStat storagePhase containsStat Subject.contains' a
    fun isEmpty a = phaseAndStat storagePhase storageStat Subject.isEmpty a
    fun numItems a = phaseAndStat storagePhase storageStat Subject.numItems a
    fun getTag a = phaseAndStat storagePhase getTagStat Subject.getTag a
    fun getTag' a = phaseAndStat storagePhase getTagStat Subject.getTag' a
    fun setTag a = phaseAndStat storagePhase setTagStat Subject.setTag a
    fun setTag' a = phaseAndStat storagePhase setTagStat Subject.setTag' a
end

functor ProfiledExplicitStorage(
structure Subject: EXPLICIT_STORAGE
val name : string
) : sig
    include EXPLICIT_STORAGE
    include PROFILING

    val storageStat : stat
    val storagePhase : phase
end =
struct
    structure Profiling = ProfilingHelp(val name = name)
    open Profiling
    open Subject
    structure P = ProfiledStorage(structure Subject = Subject val name = name)
    open P

    val lookupStat = makeStat' storageStat "Storage.lookup"

    fun lookup a = phaseAndStat storagePhase lookupStat Subject.lookup a
    fun map a b = phaseAndStat storagePhase lookupStat Subject.map a b
    fun app a b = phaseAndStat storagePhase lookupStat Subject.app a b
    fun foldl a b c = phaseAndStat storagePhase lookupStat Subject.foldl a b c
    fun partition a b = phaseAndStat storagePhase lookupStat Subject.partition a b
    fun filter a b = phaseAndStat storagePhase lookupStat Subject.filter a b
    fun exists a b = phaseAndStat storagePhase lookupStat Subject.exists a b
    fun find a b = phaseAndStat storagePhase lookupStat Subject.find a b
end

functor ProfiledRemoveStorage(
structure Subject: REMOVE_STORAGE
val name: string
) : sig
    include REMOVE_STORAGE
    include PROFILING
end = 
struct
    structure Profiling = ProfilingHelp(val name = name)
    open Profiling
    open Subject
    structure P = ProfiledStorage(structure Subject = Subject val name = name)
    open P

    val deleteStat = makeStat' storageStat "Storage.delete"

    fun delete a = phaseAndStat storagePhase deleteStat Subject.delete a
    fun delete' a = phaseAndStat storagePhase deleteStat Subject.delete' a
end

functor ProfiledExplicitRemoveStorage(
structure Subject : EXPLICIT_REMOVE_STORAGE
val name: string
) : sig
    include EXPLICIT_REMOVE_STORAGE
    include PROFILING
end = 
struct
    structure Profiling = ProfilingHelp(val name = name)
    open Profiling
    open Subject
    structure P = ProfiledExplicitStorage(structure Subject = Subject val name = name)
    open P

    val deleteStat = makeStat' storageStat "Storage.delete"

    fun delete a = phaseAndStat storagePhase deleteStat Subject.delete a
    fun delete' a = phaseAndStat storagePhase deleteStat Subject.delete' a
end

functor ProfiledWaitingSet (
structure Subject : WAITINGSET
val name : string
) : sig
    include WAITINGSET
    include PROFILING
end = 
struct
    structure Profiling = ProfilingHelp(val name = name)
    open Profiling
    open Subject

    val waitingsetPhase = makePhase "WaitingSet "
    val waitingsetStat = makeStat "WaitingSet "

    fun isEmpty a = phaseAndStat waitingsetPhase waitingsetStat Subject.isEmpty a
    fun enqueue a = phaseAndStat waitingsetPhase waitingsetStat Subject.enqueue a
    fun dequeue a = phaseAndStat waitingsetPhase waitingsetStat Subject.dequeue a
    fun head a = phaseAndStat waitingsetPhase waitingsetStat Subject.head a
    fun peek a = phaseAndStat waitingsetPhase waitingsetStat Subject.peek a
    fun length a = phaseAndStat waitingsetPhase waitingsetStat Subject.length a
end

functor ProfiledTraceExploration(
structure Subject : TRACE_EXPLORATION
val name : string
): sig
    include TRACE_EXPLORATION
    include PROFILING
end = struct
    structure Profiling = ProfilingHelp(val name = name)
    open Profiling
    open Subject 

    val transformationStat = makeStat "Exploration.transform"
    val arcTransformationPhase = makePhase "Exploration.transformArc"
    val arcTransformationStat = makeStat' transformationStat  "Exploration.transformArc"
    val stateTransformationPhase = makePhase "Exploration.transformState"
    val stateTransformationStat = makeStat' transformationStat "Exploration.transformState"

    val hookStat = makeStat "Exploration.hook"
    val arcHookPhase = makePhase "Exploration.arcHook"
    val arcHookStat = makeStat' hookStat "Exploration.arcHook"
    val stateHookPhase = makePhase "Exploration.stateHook"
    val stateHookStat = makeStat' hookStat "Exploration.stateHook"
    val traceHookStat = makeStat' hookStat "Exploration.traceHook"
    val preTraceHookPhase = makePhase "Exploration.preTraceHook"
    val preTraceHookStat = makeStat' traceHookStat "Exploration.preTraceHook"
    val postTraceHookPhase = makePhase "Exploration.postTraceHook"
    val postTraceHookStat = makeStat' traceHookStat "Exploration.postTraceHook"

    fun explore arcTrans stateTrans { a_initial, arc_hook, s_initial,
        state_hook, pre_trace_hook, post_trace_hook, t_initial } storage init =
        Subject.explore
        (phaseAndStat arcTransformationPhase arcTransformationStat arcTrans)
        (phaseAndStat stateTransformationPhase stateTransformationStat stateTrans)
        { a_initial = a_initial, s_initial = s_initial, t_initial = t_initial,
        state_hook = phaseAndStat stateHookPhase stateHookStat state_hook,
        arc_hook = phaseAndStat arcHookPhase arcHookStat arc_hook,
        pre_trace_hook = phaseAndStat preTraceHookPhase preTraceHookStat pre_trace_hook,
        post_trace_hook = phaseAndStat postTraceHookPhase postTraceHookStat post_trace_hook }
        storage init
end

functor ProfiledExploration(
structure Subject : EXPLORATION
val name : string
): sig
    include EXPLORATION
    include PROFILING
end = struct
    structure Profiling = ProfilingHelp(val name = name)
    open Profiling
    open Subject

    val transformationStat = makeStat "Exploration.transform"
    val arcTransformationPhase = makePhase "Exploration.transformArc"
    val arcTransformationStat = makeStat' transformationStat  "Exploration.transformArc"
    val stateTransformationPhase = makePhase "Exploration.transformState"
    val stateTransformationStat = makeStat' transformationStat "Exploration.transformState"

    val hookStat = makeStat "Exploration.hook"
    val arcHookPhase = makePhase "Exploration.arcHook"
    val arcHookStat = makeStat' hookStat "Exploration.arcHook"
    val stateHookPhase = makePhase "Exploration.stateHook"
    val stateHookStat = makeStat' hookStat "Exploration.stateHook"

    fun explore arcTrans stateTrans { a_initial, arc_hook, s_initial,
        state_hook } storage init =
        Subject.explore
        (phaseAndStat arcTransformationPhase arcTransformationStat arcTrans)
        (phaseAndStat stateTransformationPhase stateTransformationStat stateTrans)
        { a_initial = a_initial, s_initial = s_initial, 
        state_hook = phaseAndStat stateHookPhase stateHookStat state_hook,
        arc_hook = phaseAndStat arcHookPhase arcHookStat arc_hook }
        storage init
end

functor ProfiledSimpleExploration(
structure Subject : SIMPLE_EXPLORATION
val name : string
): sig
    include SIMPLE_EXPLORATION
    include PROFILING
end = struct
    structure Profiling = ProfilingHelp(val name = name)
    open Profiling
    open Subject

    val hookStat = makeStat "Exploration.hook"
    val arcHookPhase = makePhase "Exploration.arcHook"
    val arcHookStat = makeStat' hookStat "Exploration.arcHook"
    val stateHookPhase = makePhase "Exploration.stateHook"
    val stateHookStat = makeStat' hookStat "Exploration.stateHook"

    fun explore { a_initial, arc_hook, s_initial, state_hook } storage init =
        Subject.explore
        { a_initial = a_initial, s_initial = s_initial, 
        state_hook = phaseAndStat stateHookPhase stateHookStat state_hook,
        arc_hook = phaseAndStat arcHookPhase arcHookStat arc_hook }
        storage init
end

functor ProfiledModel(
structure Subject : MODEL
val name : string
) : sig
    include MODEL
    include PROFILING

    val stat: stat
    val phase: phase
end = struct
    structure Profiling = ProfilingHelp(val name = name)
    open Profiling
    open Subject

    val stat = makeStat "Model"
    val phase = makePhase "Model"
    val nextStatesStat = makeStat "Model.nextStates"
    val nextStatesPhase = makePhase "Model.nextStates"
    val executeSequenceStat = makeStat "Model.executeSequence"
    val executeSequencePhase = makePhase "Model.executeSequence"
    val toStringStat = makeStat "Model.toString"
    val stateToStringStat = makeStat' toStringStat "Model.stateToString"
    val stateToStringPhase = makePhase "Model.stateToString"
    val eventToStringStat = makeStat' toStringStat "Model.eventToString"
    val eventToStringPhase = makePhase "Model.eventToString"

    val getInitialStates = phaseAndStat phase stat getInitialStates
    val getCurrentState = phaseAndStat phase stat getCurrentState
    val getEvents = phaseAndStat phase stat getEvents
    val nextStates = phaseAndStat nextStatesPhase nextStatesStat nextStates
    val executeSequence = phaseAndStat executeSequencePhase executeSequenceStat executeSequence
    val stateToString = phaseAndStat stateToStringPhase stateToStringStat stateToString
    val eventToString = phaseAndStat eventToStringPhase eventToStringStat eventToString
      end

functor ProfiledGameModel(
structure Subject : GAME_MODEL
val name: string
) : sig
    include GAME_MODEL
    include PROFILING
end = struct
    structure Profiling = ProfilingHelp(val name = name)
    open Profiling
    open Subject
    structure P = ProfiledModel(structure Subject = Subject val name = name)
    open P

    val controllable = phaseAndStat phase stat controllable
    val winning = phaseAndStat phase stat winning
end

functor ProfiledSafetyProperty(
structure Subject : SAFETY_PROPERTY
val name : string
) : sig
    include SAFETY_PROPERTY
    include PROFILING
end = struct
    structure Profiling = ProfilingHelp(val name = name)
    open Profiling
    open Subject

    val stat = makeStat "SafetyProperty"
    val phase = makePhase "SafetyProperty"

    val evaluate = phaseAndStat phase stat evaluate
end

functor ProfiledCache(
structure Subject : CACHE
val name: string
): sig
    include CACHE
    include PROFILING

    val stat: stat
    val phase: phase
end = struct
    structure Profiling = ProfilingHelp(val name = name)
    open Profiling
    open Subject

    val stat = makeStat "Cache"
    val phase = makePhase "Cache"
    val insertStat = makeStat' stat "Cache.insert"
    val inDomainStat = makeStat' stat "Cache.inDomain"
    val lookupStat = makeStat' stat "Cache.lookup"
    val findStat = makeStat' stat "Cache.find"
    val removeStat = makeStat' stat "Cache.remove"
    val numItemsStat = makeStat' stat "Cache.numItems"
    val sizeStat = makeStat' stat "Cache.size"

    fun p s f x = phaseAndStat phase s f x

    fun emptyCache a = p stat Subject.emptyCache a
    fun insert a b = p insertStat Subject.insert a b
    fun inDomain a b = p inDomainStat Subject.inDomain a b
    fun lookup a b = p lookupStat Subject.lookup a b
    fun find a b = p findStat Subject.find a b
    fun remove a b = p removeStat Subject.remove a b
    fun numItems a = p numItemsStat Subject.numItems a
    fun size a = p sizeStat Subject.size a
end

functor ProfiledPersistenceCache (
structure Subject : PERSISTENCE_CACHE
val name : string
) : sig
    include PERSISTENCE_CACHE
    include PROFILING
end = struct
    structure Profiling = ProfilingHelp(val name = name)
    open Profiling
    open Subject
    structure P = ProfiledCache(structure Subject = Subject val name = name)
    open P

    fun emptyCache' a b = phaseAndStat phase stat Subject.emptyCache' a b
end

functor ProfiledMapping(
structure Subject : MAPPING
val name: string
) : sig
    include MAPPING
    include PROFILING

    val stat: stat
end = struct
    structure Profiling = ProfilingHelp(val name = name)
    open Profiling
    open Subject

    val stat = makeStat "Mapping"
    val mapStat = makeStat' stat "Mapping.map"
    val mapPhase = makePhase "Mapping.map"

    val map = phaseAndStat mapPhase mapStat map
end

functor ProfiledReversibleMapping (
structure Subject : REVERSIBLE_MAPPING
val name : string
) : sig
    include REVERSIBLE_MAPPING
    include PROFILING
end = struct
    structure Profiling = ProfilingHelp(val name = name)
    open Profiling
    open Subject
    structure P = ProfiledMapping(structure Subject = Subject val name = name)
    open P

    val unmapStat = makeStat' stat "Mapping.unmap"
    val unmapPhase = makePhase "Mapping.unmap"

    val unmap = phaseAndStat unmapPhase unmapStat unmap
end

functor ProfiledPacker(
structure Subject : PACKER
val name : string
) : sig
    include PACKER
    include PROFILING
end = struct
    open Profiling
    open Subject 

    structure StatePacker = ProfiledMapping(structure Subject = StatePacker
        val name = String.concat ["StatePacker ", name])
    structure EventPacker = ProfiledMapping(structure Subject = EventPacker
        val name = String.concat ["EventPacker ", name])

    fun getStats () = List.@ (StatePacker.getStats(), EventPacker.getStats())
    fun getPhases () = List.@ (StatePacker.getPhases(), EventPacker.getPhases())
    fun reset() = (StatePacker.reset(); EventPacker.reset())
end

functor ProfiledSerializer(
structure Subject : SERIALIZER
val name: string
) : sig
    include SERIALIZER
    include PROFILING
end = struct
    open Subject
    structure P = ProfiledMapping(structure Subject = Subject
    val name = String.concat ["Serializer ", name])
    open P
end

functor ProfiledModelSerializer(
structure Subject : MODEL_SERIALIZER
val name : string
): sig
    include MODEL_SERIALIZER
    include PROFILING
end = struct
    open Profiling
    open Subject

    structure StateSerializer = ProfiledSerializer(structure Subject = StateSerializer
        val name = String.concat ["StateSerializer ", name])
    structure EventsSerializer = ProfiledSerializer(structure Subject = EventsSerializer
        val name = String.concat ["StateSerializer ", name])

    fun getStats () = List.@ (StateSerializer.getStats(), EventsSerializer.getStats())
    fun getPhases () = List.@ (StateSerializer.getPhases(), EventsSerializer.getPhases())
    fun reset() = (StateSerializer.reset(); EventsSerializer.reset())
end
