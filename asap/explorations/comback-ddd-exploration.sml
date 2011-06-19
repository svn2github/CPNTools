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
 *  File:
 *     comback-ddd-storage.sml
 *
 *  Created:
 *     May 05, 2008
 *
 *  Description:
 *     Provide the exploration algorithm based on the comback method extended
 *  with delayed duplicate detection, taken from:
 *  The ComBack Method Revisited: Caching Strategies and Extension with
 *  Delayed Duplicate Detection. In ToPNoC'2009.
 *  Sami Evangelista, Michael Westergaard and Lars Michael Kristensen.
 *
 *  To do:
 *  - undeterministic systems are not supported
 *
 *  Parameters of functor ComBackDDDExploration:
 *  - reconstruct: should the queue contain only state identifiers (instead of
 *    full state vectors) and reconstruct states from these
 *  - groupReconstructions: if reconstruct = true, indicates if we group the
 *    reconstruction of queued states
 *  - reconstructions: if reconstruct = true and groupReconstructions = true,
 *    indicates the size of the groups, i.e., how many states we reconstrcut in
 *    a single step
 *)


functor ComBackDDDExploration (
val reconstruct         : bool
val groupReconstructions: bool
val reconstructions     : int
structure Model         : MODEL
structure Storage       : COMBACK_DDD_STORAGE
where type id = int
sharing type Storage.item = Model.state
sharing type Storage.event = Model.event
): TRACE_EXPLORATION = struct

val errorUndeterministic =
    "ComBackDDDExploration: undeterministic systems not supported"
val errorSeveralInitStates =
    "ComBackDDDExploration: several initial states not supported"
val (reconstruct, groupReconstructions, reconstructions) =
    if reconstruct andalso groupReconstructions andalso reconstructions <= 0
    then (print ("ComBackDDDExploration: groupReconstructions = true " ^
		 "and reconstructions = 0 => " ^
		 "reconstructions grouping turned off\n");
	  (true, false, 0))
    else (reconstruct, groupReconstructions, reconstructions)


type state = Model.state
type event = Model.event
type 'a storage = 'a Storage.storage
type id = Storage.id


val statesCurrentLevel = ref 1
val statesNextLevel = ref (~ 1)
val currentLevel = ref 0
val levelWidth = ref 1


structure Queue: sig

type item = Model.state * Model.event list

type 'a queue

val isEmpty: 'a queue -> bool

val empty: 'a queue

val enqueue: 'a queue * (item * Storage.id * 'a) -> 'a queue

val dequeue: 'a queue * 'b Storage.storage ->
	     'a queue * (item * Storage.id * 'a)

val map: ((Storage.id -> Storage.id) -> 'a -> 'a) ->
	 (Storage.id -> Storage.id) -> 'a queue -> 'a queue

end = struct

type item = Model.state * Model.event list

type 'a queue =
     (item * Storage.id * 'a) list *
     (Storage.id * 'a) Fifo.fifo *
     (item * Storage.id * 'a) Fifo.fifo

fun isEmpty ([], idFifo, fullFifo) = Fifo.isEmpty idFifo andalso
				     Fifo.isEmpty fullFifo
  | isEmpty _ = false

val empty = ([], Fifo.empty, Fifo.empty)

fun enqueue ((l, idFifo, fullFifo), (s, id, tag)) =
    (statesNextLevel := !statesNextLevel + 1;
     if reconstruct
     then (l, Fifo.enqueue (idFifo, (id, tag)), fullFifo)
     else (l, idFifo,  Fifo.enqueue (fullFifo, (s, id, tag))))

fun dequeue ((item :: l, idFifo, fullFifo), V) =
    ((l, idFifo, fullFifo), item)
  | dequeue (([], idFifo, fullFifo), V) =
    if not (Fifo.isEmpty fullFifo)
    then let val (fullFifo, item) = Fifo.dequeue fullFifo
	 in (([], idFifo, fullFifo), item) end
    else
	if not groupReconstructions
	then let val (idFifo, (id, tag)) = Fifo.dequeue idFifo
	     in 
		 (([], idFifo, fullFifo),
		  (Storage.reconstruct (V, id), id, tag))
	     end
	else let fun f 0 fifo = (fifo, [])
		   | f N fifo =
		     if Fifo.isEmpty fifo
		     then (fifo, [])
		     else let val (fifo, item) = Fifo.dequeue fifo
			      val (fifo, items) = f (N - 1) fifo
			  in (fifo, item :: items) end
		 val (idFifo, ids) = f reconstructions idFifo
		 val states = Storage.reconstructList (V, ids)
	     in
		 dequeue ((states, idFifo, fullFifo), V)
	     end

fun map mapTag mapId (l, idFifo, fullFifo) =
    (List.map (fn (s, id, tag) => (s, mapId id, mapTag mapId tag)) l,
     Fifo.map (fn (id, tag) => (mapId id, mapTag mapId tag)) idFifo,
     Fifo.map (fn (s, id, tag) => (s, mapId id, mapTag mapId tag)) fullFifo)

end


fun explore arcTransformer
	    stateTransformer
	    {
	     a_initial,
	     s_initial,
	     t_initial,
	     state_hook,
	     arc_hook,
	     pre_trace_hook,
	     post_trace_hook
	    }
	    initStorage
	    initStates = let

    fun mapQueueTag mapId (SOME (id, ev, eid), tVal) =
	(SOME (mapId id, ev, eid), tVal)
      | mapQueueTag _ tag = tag

    fun candidateNew ((V, Q), id, (s', id', pred)) = let
	val (tVal, V) =
	    case pred
	     of NONE => (t_initial, V)
	      | SOME (s, e, id, tVal) =>
		pre_trace_hook ((s, e, s'), id, id', tVal, V)
	val Q = Queue.enqueue (Q, (s', id', (NONE, tVal)))
    in
	(V, Q)
    end

    val duplicateDetection = Storage.duplicateDetection candidateNew 

    val s0 = case initStates
	      of [ s0 ] => s0
	       | _ => raise LibBase.Unimplemented errorUndeterministic

    fun handleState (((s, evts), id, (ancestor, tVal)),
		     (V, C, Q, sVal, aVal)) = let
	val sVal = state_hook ((s, evts), tVal, sVal)
	fun handleEvent (ev, (V, Q, sVal, aVal, eid, refs)) = let
	    val s' = case Model.nextStates (s, ev)
		      of [ s' ] => s'
		       | _ => raise LibBase.Unimplemented errorUndeterministic
	    val (s', evts') = stateTransformer s'
	    val s' = (s', arcTransformer (s', evts'))
	    val isNew = Storage.insert V C s'
				       (SOME ((s, evts), ev, id, tVal))
				       (SOME (id, ev, Storage.EVENT, eid))
	    val aVal = arc_hook (((s, evts), ev, s'), tVal, aVal)
	    val (Q, refs) =
		case isNew
		 of Storage.NEW id' => let
			val ancestor' = case (evts', ancestor)
					 of ([ _ ], NONE) => SOME (id, ev, eid)
					  | ([ _ ], some) => some
					  | _ => NONE
			val (tVal, V) =
			    pre_trace_hook
				(((s, evts), ev, s'), id, id', tVal, V)
			val Q = Queue.enqueue (Q, (s', id', (ancestor', tVal)))
			val refs =
			    case (ancestor, ancestor')
			     of (SOME (id, ev, eid), NONE) =>
				(Storage.setPredecessor
				     V id' (id, ev, Storage.SEQUENCE, eid);
				 0)
			      | _ => refs + 1
		    in
			(Q, refs)
		    end
		  | Storage.OLD id' => (Q, refs)
		  | Storage.MAYBE => (Q, refs)
	in
	    (V, Q, sVal, aVal, eid + 1, refs)
	end
	val (V, Q, sVal, aVal, _, refs) =
	    List.foldl handleEvent (V, Q, sVal, aVal, 0, 0) evts
	val h = (Math.pow (Real.fromInt (refs * (!currentLevel)), 2.0)) /
		(Real.fromInt (!levelWidth))
    in
	statesCurrentLevel := !statesCurrentLevel - 1;
	Storage.cacheState (V, (s, evts), id, h);
	Storage.unrefState V id;
	if !statesCurrentLevel <> 0
	then ()
	else (currentLevel := !currentLevel + 1;
	      levelWidth := !statesNextLevel;
	      statesCurrentLevel := !statesNextLevel;
	      statesNextLevel := 0);
	(V, C, Q, sVal, aVal)
    end

    fun loop (V, C, Q, sVal, aVal) =
	if Queue.isEmpty Q
	then (V, C, sVal, aVal)
	else let val (Q, (s, id, (ancestor, tVal))) = Queue.dequeue (Q, V)
		 val (V, C, Q, sVal, aVal) =
		     handleState ((s, id, (ancestor, tVal)),
				  (V, C, Q, sVal, aVal))
		 val (V, Q) = duplicateDetection V C (V, Q) false
		 val Q = Storage.garbageCollection
			     V C (Queue.map mapQueueTag) Q
	     in
		 loop (V, C, Q, sVal, aVal)
	     end
	     
    fun iter (V, C, Q, sVal, aVal) = let
	val (V, C, sVal, aVal) = loop (V, C, Q, sVal, aVal)
	val (V, Q) = duplicateDetection V C (V, Queue.empty) true
    in
	if Queue.isEmpty Q
	then (V, sVal, aVal)
	else iter (V, C, Q, sVal, aVal)
    end
				    
    val V = initStorage
    val C = Storage.emptyCandidateSet V
    val D = []
    val Q = let
	val s0 = stateTransformer s0
	val s0 = (#1 s0, arcTransformer s0)
	val res = Storage.insert V C s0 NONE NONE
    in
	case res of
	    Storage.NEW id => Queue.enqueue (Queue.empty,
					     (s0, id, (NONE, t_initial)))
	  | _ => raise LibBase.Unimplemented errorSeveralInitStates
    end
    val (V, sVal, aVal) = iter (V, C, Q, s_initial, a_initial)
in
    (V, sVal, aVal)
end

end
