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
signature GENERIC_EXTERNAL_EXPLORATION = sig
    
include TRACE_EXPLORATION

type report = { ioQueue   : LargeInt.int,
		ioVisited : LargeInt.int,
		crossTrans: int,
		partSwap  : int,
		expanded  : int,
		events    : int,
		reorgTime : real }

val makeReport:
    'a storage
    -> (state * event list) list
    -> 'a storage * report

end



functor GenericExternalExploration(
structure Model        : MODEL
structure Storage      : PART_EXTERNAL_STORAGE
structure WaitingSet   : WAITINGSET
structure StateEncoder : SERIALIZER where type src = Model.state
structure EventsEncoder: SERIALIZER where type src = Model.event list
structure PartFunction : PART_FUNCTION where type state = Model.state
                                         and type event = Model.event
val queueBufferSize    : int
val maxPartSize        : int option
val localCheck         : bool
val selectPart         : (Storage.part_id -> int)
			 -> (Storage.part_id *
			     Storage.part_id list *
			     Storage.part_id list) option
) = struct

type report = { ioQueue   : LargeInt.int,
                ioVisited : LargeInt.int,
                crossTrans: int,
                partSwap  : int,
                expanded  : int,
                events    : int,
                reorgTime : real }

type event = Model.event

type state = Model.state

type id = Storage.id

type 'a storage = 'a Storage.storage

val maxPartSize = case maxPartSize
		   of NONE => 0
		    | SOME s => s

structure Queue = PartExternalStorage(
structure Encoder = StateEncoder)

fun merge (s, evts) = let
    val lg = Word8Vector.length s
    val pref = Word8Vector.tabulate (2, fn 0 => Word8.fromInt (lg div 256)
                                         | 1 => Word8.fromInt (lg mod 256)
                                         | _ => raise LibBase.Impossible "")
in
    Word8Vector.concat [ pref, s, evts ]
end

fun split v = let
    val lg = Word8Vector.length v
    val sLg = (Word8.toInt (Word8Vector.sub (v, 0)) * 256) +
              (Word8.toInt (Word8Vector.sub (v, 1)))
    val s = Word8Vector.tabulate
                (sLg, fn i => Word8Vector.sub (v, i + 2))
    val evts = Word8Vector.tabulate
                   (lg - sLg - 2, fn i => Word8Vector.sub (v, i + 2 + sLg))
in
    (s, evts)
end

fun explore transformEvents
	    transformState
	    { a_initial,
	      s_initial,
	      t_initial,
	      arc_hook,
	      state_hook,
	      pre_trace_hook,
	      post_trace_hook }
	    initStorage
	    initStates = let

    val defaultTag = (false, 0, ExternalPartition.nullId)

    val emptyVector = Word8Vector.fromList []

    val H = ExternalPartition.init (ref defaultTag, emptyVector)
    val V = initStorage
    val Q = Queue.emptyStorage { path       = Storage.path V,
				 prefix     = "QUEUE",
				 bufferSize = queueBufferSize,
				 initParts  = Storage.numPartitions V } ()

    val expanded   = ref 0
    val events     = ref 0
    val swaps      = ref 0
    val crossTrans = ref 0

    fun reorganize p =
        PartFunction.dynamic
	andalso PartFunction.canReorganize p
	andalso ExternalPartition.numItems H >= maxPartSize

    fun execEvent (p, (s, evts), id) (ev, (U, succs, sVal, aVal)) = let
	fun enqueueSucc ((s', evts'), (U, succs, sVal, aVal)) = let
	    val (s', evts') = transformState (s', evts')
	    val evts' = transformEvents (s', evts')
	    val aVal = arc_hook (((s, evts), ev, (s', evts')),
				 t_initial, aVal)
	    val (s', p') = PartFunction.part s'
            val evts' = EventsEncoder.map evts'
	    val cross = p <> p'
	in
	    events := !events + 1;
	    if cross
	    then crossTrans := !crossTrans + 1
	    else ();
	    if localCheck andalso not cross
	    then let val (inserted, id', _) =
			 ExternalPartition.insert
			     (H, s', (ref (false, 1, id), evts'))
		 in
		     if inserted
		     then (WaitingSet.enqueue (U, id'), succs + 1, sVal, aVal)
		     else (U, succs, sVal, aVal)
		 end
	    else (Queue.insert (Q, p', merge (s', evts'));
		  (U, succs + 1, sVal, aVal))
	end
    in
	PartFunction.eventHook ev;
	List.foldl enqueueSucc
		   (U, succs, sVal, aVal)
		   (Model.nextStates (s, ev))
    end
	
    fun expandState (id, p, (U, sVal, aVal)) =
	case ExternalPartition.getItem (H, id)
	 of NONE => (U, sVal, aVal)
	  | SOME (sV, (data, evtsV)) => let
		val s = StateEncoder.unmap sV
		val evts = EventsEncoder.unmap evtsV
		val sVal = state_hook ((s, evts), t_initial, sVal)
		val (store, succs, pred) = !data
		val (U, succs, sVal, aVal) =
		    List.foldl (execEvent (p, (s, evts), id))
			       (U, 0, sVal, aVal) evts
	    in
		data := (true, succs, pred);
		expanded := !expanded + 1;
		(U, sVal, aVal)
	    end

    fun expandPartition (p, (U, sVal, aVal)) =
	if WaitingSet.isEmpty U
	then (false, U, (sVal, aVal))
	else let val (U, id) = WaitingSet.dequeue U
		 val (U, sVal, aVal) = expandState
					      (id, p, (U, sVal, aVal))
	     in
		 if reorganize p
		 then (true, U, (sVal, aVal))
		 else expandPartition (p, (U, sVal, aVal))
	     end

    fun loadPart (p, toLoad, toRemove) = let
	val _ = List.app (fn partId =>
			     Storage.loadPart
				 (fn v => (v, (ref defaultTag, emptyVector)))
				 (fn _ => ()) () (V, partId, H))
			 toLoad
	val U = Queue.loadPart (fn v => let val (s, evts) = split v
					in (s, (ref defaultTag, evts)) end)
			       (op ::) [] (Q, p, H)
    in
	List.app (fn partId => Storage.removeDuplicates
				   (fn _ => ()) () (V, partId, H))
		 toRemove;
	Queue.emptyPart (Q, p);
	List.foldl (fn (id, U) => WaitingSet.enqueue (U, id))
		   WaitingSet.empty U
    end

    fun unloadPart p =
	(Storage.unloadPart
	     (fn (v, (ref (true, _, _), _)) => SOME v | _ => NONE) (V, p, H);
	 ExternalPartition.empty H)
    
    fun reorganizePart (U, old, new) = let
	fun unloadQueue U =
	    if WaitingSet.isEmpty U
	    then ()
	    else let val (U, id) = WaitingSet.dequeue U
		 in
		     case ExternalPartition.deleteId (H, id)
		      of NONE => ()
		       | SOME (sV, (_, evtsV)) =>
			 Queue.insert
			     (Q, PartFunction.part' sV, merge (sV, evtsV));
		     unloadQueue U
		 end
    in
	Queue.reorganizeAndEmptyPart (Q, old, new);
	unloadQueue U;
	Storage.unloadPart
	    (fn (v, (ref (true, _, _), _)) => SOME v | _ => NONE) (V, old, H);
	Storage.reorganizePart PartFunction.part' (V, old, new);
	ExternalPartition.empty H
    end

    fun iterate (sVal, aVal) =
	case selectPart (fn partId => Queue.partNumItems (Q, partId))
	 of NONE => (sVal, aVal)
	  | SOME (p, toLoad, toRemove) => let
		val U = loadPart (p, toLoad, toRemove)
		val (reorganize, U, (sVal, aVal)) =
		    expandPartition (p, (U, sVal, aVal))
	    in
		swaps := !swaps + 1;
		if reorganize
		then case PartFunction.reorganize (H, p)
                      of [] => ExternalPartition.empty H
		       | l => reorganizePart (U, p, l)
		else unloadPart p;
		iterate (sVal, aVal)
	    end

    fun enqueueInitialState ((s, evts), (sVal, aVal)) = let
	val sVal = state_hook ((s, evts), t_initial, sVal)
	val (s, p) = PartFunction.part s
        val evts = EventsEncoder.map evts
    in
	Queue.insert (Q, p, merge (s, evts));
	(sVal, aVal)
    end
		    
    val (sVal, aVal) = List.foldl enqueueInitialState (s_initial, a_initial)
				  initStates
    val (sVal, aVal) = iterate (sVal, aVal)
    val report = {
        ioQueue    = LargeInt.+ (Queue.readAccesses Q,
				 Queue.writeAccesses Q),
        ioVisited  = LargeInt.+ (Storage.readAccesses V,
				 Storage.writeAccesses V),
        crossTrans = !crossTrans,
        expanded   = !expanded,
        events     = !events,
        partSwap   = !swaps,
        reorgTime  = 0.0
    }
in
    (V, sVal, aVal, report)
end

fun makeReport initStorage initStates = let
    val (V, _, _, report) =
        (explore
            (fn (_, evts) => evts)
            (fn s => s)
            {
             a_initial = (),
             s_initial = (),
             t_initial = (),
             arc_hook = fn _ => (),
             state_hook = fn _ => (),
             pre_trace_hook = fn (_, _, _, _, storage) => ((), storage),
             post_trace_hook = fn (_, _, _, _, storage) => ((), storage)
            }
            initStorage
            initStates)
in
    (V, report)
end


local
    fun exp transformEvents transformState hooks initStorage initStates = let
        val (storage, sVal, aVal, _) =
            explore transformEvents transformState hooks initStorage initStates
    in
        (storage, sVal, aVal)
    end
in
val explore = exp
end

end



(*
 *  BFS-HDDD algorithm
 *)
functor BFSHDDDExploration(
structure Model        : MODEL
structure Storage      : PART_EXTERNAL_STORAGE
structure StateEncoder : SERIALIZER where type src = Model.state
structure EventsEncoder: SERIALIZER where type src = Model.event list
structure PartFunction : PART_FUNCTION
			     where type state = Model.state
                               and type event = Model.event
val queueBufferSize    : int
val maxPartSize        : int option
) = struct

val selected = ref (~ 1)
val depth = ref (~ 1)
val levelEmpty = ref false

fun selectPart sizeOfPart = let
    val next = (!selected + 1) mod (PartFunction.numPartitions ())
in
    if next = 0 andalso !levelEmpty
    then NONE
    else (if next = 0
	  then (depth := !depth + 1;
		print (Int.toString (!depth) ^ "\n");
		levelEmpty := true)		
	  else ();
	  selected := next;
	  if sizeOfPart next = 0
	  then selectPart sizeOfPart
	  else (levelEmpty := false;
		SOME (next, [], [ next ] )))
    
end

structure Exploration = GenericExternalExploration(
structure Model = Model
structure Storage = Storage
structure WaitingSet = FifoQueue
structure StateEncoder = StateEncoder
structure EventsEncoder = EventsEncoder
structure PartFunction = PartFunction
val queueBufferSize = queueBufferSize
val maxPartSize = maxPartSize
val selectPart = selectPart
val localCheck = false)

open Exploration

end



(*
 *  PART algorithm
 *)
functor PartExploration (
structure WaitingSet   : WAITINGSET
structure Model        : MODEL
structure StateEncoder : SERIALIZER where type src = Model.state
structure EventsEncoder: SERIALIZER where type src = Model.event list
structure Storage      : PART_EXTERNAL_STORAGE where type item = Model.state
structure PartFunction : PART_FUNCTION where type state = Model.state
                                         and type event = Model.event
val queueBufferSize    : int
val maxPartSize        : int option): GENERIC_EXTERNAL_EXPLORATION = struct

fun selectPart sizeOfPart = let
    val parts = List.tabulate (PartFunction.numPartitions (),
			       (fn i => (i, sizeOfPart i)))
    val max = List.foldl (fn ((i, n), NONE) => SOME (i, n)
			   | ((i, n), SOME (j, m)) =>(
			     if n > m
			     then SOME (i, n)
			     else SOME (j, m))) NONE parts
in
    case max
     of NONE => NONE
      | SOME (_, 0) => NONE
      | SOME (i, _) => SOME (i, [ i ], [])
end

structure Exploration = GenericExternalExploration(
structure Model = Model
structure Storage = Storage
structure WaitingSet = WaitingSet
structure StateEncoder = StateEncoder
structure EventsEncoder = EventsEncoder
structure PartFunction = PartFunction
val queueBufferSize = queueBufferSize
val maxPartSize = maxPartSize
val selectPart = selectPart
val localCheck = true)

open Exploration

end
