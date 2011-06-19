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
 *     sweep-line-exploration.sml
 *
 *  Created:
 *     Nov. 13, 2007
 *
 *  Description:
 *     Sweep-line state space exploration.
 *)


signature PROGRESS_MEASURE = sig
    
structure Progress: ORD_KEY
    
type state

val getProgress:
    state
    -> Progress.ord_key

end



signature SWEEP_LINE_EXPLORATION = sig

include TRACE_EXPLORATION

type progress
type state_events = state * event list
type arc = state_events * event * state_events

val sweepExplore :
    (state_events -> progress)
    -> { sweepHook: state_events list * 'd storage -> unit,
	 gcHook   : state_events list * 'd storage * progress * 'b * 'a ->
		    'b * 'a}
    -> (state_events -> event list)
    -> (state_events -> state_events)
    -> { a_initial      : 'a,
	 s_initial      : 'b,
	 t_initial      : 'c,
	 arc_hook       : arc * 'c * 'a -> 'a, 
	 state_hook     : state_events * 'c * 'b -> 'b,
	 pre_trace_hook : arc * id * id * 'c * 'd storage -> 'c * 'd storage,
	 post_trace_hook: arc * id * id * 'c * 'd storage -> 'c * 'd storage }
    -> 'd storage
    -> state_events list
    -> 'd storage * 'b * 'a

end



signature PRIORITY_QUEUE = sig

type 'a queue

structure Key: ORD_KEY

val mkQueue:
    ('a -> Key.ord_key)
    -> 'a queue

val isEmpty:
    'a queue
    -> bool

val numItems:
    'a queue
    -> int

val insert:
    'a * 'a queue
    -> 'a queue

val remove:
    'a queue
    -> 'a * 'a queue

val next:
    'a queue
    -> 'a option

val fold:
    ('a * 'a queue * 'b -> 'a queue * 'b)
    -> 'a queue
    -> 'b
    -> 'b

end



functor PriorityQueue(structure Key: ORD_KEY): PRIORITY_QUEUE = struct

structure Key = Key

datatype 'a heap = EMPTY | ND of (int * 'a * 'a heap * 'a heap)
datatype 'a queue = Q of (int * ('a -> Key.ord_key) * 'a heap)

fun mkQueue priority = Q (0, priority, EMPTY)

fun numItems (Q (n, _, _)) = n

fun singletonHeap x = ND (1, x, EMPTY, EMPTY)
fun singleton (x, priority) = Q (1, priority, singletonHeap x)

fun rank EMPTY = 0
  | rank (ND (r, _, _, _)) = r

fun mkNode (x, a, b) = if (rank a >= rank b)
		       then ND(rank b + 1, x, a, b)
		       else ND(rank a + 1, x, b, a)

fun merge priority nodes = let
    fun mergeHeap (h, EMPTY) = h
      | mergeHeap (EMPTY, h) = h
      | mergeHeap (h1 as ND (_, x, h11, h12), h2 as ND (_, y, h21, h22)) =
	case Key.compare (priority x, priority y)
	 of GREATER => mkNode (x, h11, mergeHeap (h12, h2))
	  | _ => mkNode (y, h21, mergeHeap (h1, h22))
in mergeHeap nodes end

fun insert (x, Q (n, priority, h)) =
    Q(n + 1, priority, merge priority (singletonHeap x, h))

fun remove (Q (_, _, EMPTY)) = raise List.Empty
  | remove (Q (n, priority, ND (_, x, h1, h2))) =
    (x, Q (n - 1, priority, merge priority (h1, h2)))

fun next (Q (_, _, EMPTY)) = NONE
  | next (Q (_, _, ND (_, x, _, _))) = SOME x

fun numItems (Q (n, _, _)) = n

fun isEmpty (Q (_, _, EMPTY)) = true
  | isEmpty _ = false

fun fold f queue data =
    if isEmpty queue
    then data
    else let val (item, queue) = remove queue
	     val (queue, data) = f (item, queue, data)
	 in
	     fold f queue data
	 end

end



(*
 *  sweep line exploration.  only for internal use. use functors below instead
 *)
functor InternalSweepLineExploration (
val markInitStatesAsPersistent : bool
structure Storage              : REMOVE_STORAGE
structure Model                : MODEL
structure Progress             : ORD_KEY
sharing type Model.state = Storage.item
) = struct
    
type id = Storage.id
type state = Model.state
type event = Model.event
type 'a storage = 'a Storage.storage
type progress = Progress.ord_key
type state_events = state * event list
type arc = state_events * event * state_events

structure PQ = PriorityQueue(structure Key = struct
type ord_key = Progress.ord_key
fun compare items = case Progress.compare items of GREATER => LESS
						 | LESS => GREATER
						 | EQUAL => EQUAL
end)

val numQueue = ref 0
val numStored = ref 0
val peakNumQueue = ref 0
val peakNumStored = ref 0

fun sweepExplore
	getProgress
	{ sweepHook,
	  gcHook }
	arcTransformer
	stateTransformer
	{ state_hook,
	  s_initial,
          arc_hook,
	  a_initial,
          pre_trace_hook,
	  post_trace_hook,
	  t_initial }
	initStorage
	initStates = let
    val _ = (numQueue := 0;
	     peakNumQueue := 0;
	     peakNumStored := 0)	     
    fun incrNums () =
	(numStored := !numStored + 1;
	 numQueue := !numQueue + 1;
	 if !numQueue > !peakNumQueue
	 then peakNumQueue := !numQueue
	 else ();
	 if !numStored > !peakNumStored
	 then peakNumStored := !numStored
	 else ())
    fun gc (storage, states, prog, sVal, aVal) = let
	val (sVal, aVal) = gcHook (states, storage, prog, sVal, aVal)
	val storage =
	    List.foldl (fn ((s, _), storage) => (numStored := !numStored - 1;
						 Storage.delete (storage, s)))
		       storage states
    in
	(storage, sVal, aVal)
    end
    fun handleState
	    (((s1, s1Evts), s1Id, s1Prog, s1Del, s1Trace),
	     unproc,
	     (toDel, roots, storage, sVal, aVal)) = let	
	fun handleEvent
		(s1Evt,
		 (unproc, toDel, roots, storage, sVal, aVal)) = let	    
	    fun handleSucc
		    ((s2, s2Evts),
		     (unproc, toDel, roots, storage, sVal, aVal)) = let
		val (s2, s2Evts) = stateTransformer (s2, s2Evts)
		val s2Evts = arcTransformer (s2, s2Evts)
		val aVal = arc_hook (((s1, s1Evts), s1Evt, (s2, s2Evts)),
				     s1Trace, aVal)
		val (s2Id, seen, storage) = Storage.add (storage, s2)
	    in
		if seen
		then (unproc, toDel, roots, storage, sVal, aVal)
		else let val s2Prog = getProgress (s2, s2Evts)
                         val (s2Trace, storage) =
                             pre_trace_hook (((s1, s1Evts), s1Evt,
					      (s2, s2Evts)),
					     s1Id, s2Id, s1Trace, storage)
                         val sVal = state_hook ((s2, s2Evts), s2Trace, sVal)
		     in
			 incrNums ();
			 case Progress.compare (s1Prog, s2Prog)
			  (*
			   *  regress edge =>
			   *     put s2 in the root state queue
			   *)
			  of GREATER =>
			     (unproc, toDel,
			      ((s2, s2Evts), s2Id, s2Trace) :: roots,
			      storage, sVal, aVal)		    
			   (*
			    *  non regress edge =>
			    *     put s2 in the unprocessed state queue
			    *)
			   | _ =>
			     (PQ.insert
				  (((s2, s2Evts), s2Id,
				    s2Prog, true, s2Trace), unproc),
			      toDel, roots, storage, sVal, aVal)
		     end
	    end						    
        in
	    List.foldl handleSucc
		       (unproc, toDel, roots, storage, sVal, aVal)
		       (Model.nextStates (s1, s1Evt))
	end								     
	val (unproc, toDel, roots, storage, sVal, aVal) =
	    List.foldl handleEvent
		       (unproc, toDel, roots, storage, sVal, aVal) s1Evts
		       
	(*
	 *  garbage collection.  the delete flag s1Del of state s1 must be true
	 *  otherwise s1 is marked as persistent and must not be deleted
	 *)
	val (toDel, storage, sVal, aVal) =
	    if not s1Del
	    then (toDel, storage, sVal, aVal)
	    else case toDel
		  of NONE => (SOME (s1Prog, [ (s1, s1Evts) ]),
			      storage, sVal, aVal)
		   | SOME (value, states) =>
		     case Progress.compare (s1Prog, value)
		      of GREATER => let
			     val (storage, sVal, aVal) =
				 gc (storage, states, value, sVal, aVal)
			 in
			     (SOME (s1Prog, [ (s1, s1Evts) ]),
			      storage, sVal, aVal)
			 end
		       | EQUAL => (SOME (value, (s1, s1Evts) :: states),
				   storage, sVal, aVal)
		       | LESS => raise LibBase.Impossible
					   "error in SweepLineExploration"
    in
	numQueue := !numQueue - 1;
	(unproc, (toDel, roots, storage, sVal, aVal))
    end

    (*
     *  a sweep of the algorithm
     *)
    fun sweep ([], storage, sVal, aVal) = (storage, sVal, aVal)
      | sweep (roots, storage, sVal, aVal) = let
	    val _ = sweepHook (List.map #1 roots, storage)
	    (*
	     *  put root states into the queue (the toDel bit is set to false)
	     *)
	    val queue =
		List.foldl
		    (fn ((s, id, trace), q) =>
			PQ.insert ((s, id, getProgress s, false, trace), q))
		    (PQ.mkQueue (fn (_, _, prog, _, _) => prog)) roots
	    val (toDel, roots, storage, sVal, aVal) =
		PQ.fold handleState queue (NONE, [], storage, sVal, aVal)
	    val (storage, sVal, aVal) =
		case toDel
		 of SOME (value, ids) => gc (storage, ids, value, sVal, aVal)
		  | NONE => (storage, sVal, aVal)
	in
	    sweep (roots, storage, sVal, aVal)
	end

    (*
     *  create the initial queue of root states and the storage containing the
     *  initial states
     *)
    fun handleInitState ((st, stEvts), (roots, storage, sVal)) = let
        val (st, stEvts) = stateTransformer (st, stEvts)
        val (stId, seen, storage) = Storage.add (storage, st)
    in
	if seen
	then (roots, storage, sVal)
	else let val sVal   = state_hook ((st, stEvts), t_initial, sVal)
                 val stEvts = arcTransformer (st, stEvts)
	     in
		 incrNums ();			 
                 (((st, stEvts), stId, t_initial) :: roots, storage, sVal)
             end
    end

    val (initRoots, initStorage, initSVal) =
	List.foldl
	    handleInitState ([], initStorage, s_initial) initStates

    val (storage, sVal, aVal) =
	sweep (initRoots, initStorage, initSVal, a_initial)
in
    (storage, sVal, aVal)
end

end



(*
 *  default sweep line exploration
 *)
functor SweepLineExploration(
val markInitStatesAsPersistent : bool
structure Storage              : REMOVE_STORAGE
structure Model                : MODEL
structure Measure              : PROGRESS_MEASURE
   where type state = Model.state * Model.event list
sharing type Model.state = Storage.item
): SWEEP_LINE_EXPLORATION = struct

structure Exploration = InternalSweepLineExploration (
val markInitStatesAsPersistent = markInitStatesAsPersistent
structure Storage  = Storage
structure Model    = Model
structure Progress = Measure.Progress)

open Exploration

fun explore arcTransformer
	    stateTransformer
	    hooks
	    initStorage
	    initStates =
    Exploration.sweepExplore
	Measure.getProgress
	{ sweepHook = fn _ => (),
	  gcHook    = fn (_, _, _, sVal, aVal) => (sVal, aVal) }
	arcTransformer
	stateTransformer
	hooks
	initStorage
	initStates

end



(*
 *  sweep line exploration with debug hooks
 *)
functor SweepLineDebugExploration(
structure Storage    : REMOVE_STORAGE
structure Exploration: SWEEP_LINE_EXPLORATION
   where type 'a storage = 'a Storage.storage
structure Measure    : PROGRESS_MEASURE
   where type Progress.ord_key = Exploration.progress
     and type state = Exploration.state * Exploration.event list
): SWEEP_LINE_EXPLORATION = struct

open Exploration

fun intToString i = StringCvt.padLeft #" " 10 (Int.toString i)

fun debugSweepHook oldHook (params as (roots, storage)) =
    (print (String.concat [
	    "Sweep-line exploration: new sweep begins with ",
		intToString (List.length roots),
	    " root state(s) and ",
	    intToString (Storage.numItems storage),
	    " persistent state(s)\n" ]);
     oldHook params)

fun debugGcHook oldHook (params as (ids, storage, prog, sVal, aVal)) =
    (print (String.concat [
	    "Sweep-line exploration: GC deletes ",
	    intToString (List.length ids),
	    " state(s), ",
	    "storage now contains ",
	    intToString ((Storage.numItems storage) - (List.length ids)),
	    " state(s)\n" ]);
     oldHook params)

fun sweepExplore getProgress
		 { sweepHook,
		   gcHook }
		 arcTransformer
		 stateTransformer
		 hooks
		 initStorage
		 initStates = let
in
    Exploration.sweepExplore
	getProgress
	{ sweepHook = debugSweepHook sweepHook,
	  gcHook    = debugGcHook gcHook }
	arcTransformer
	stateTransformer
	hooks
	initStorage
	initStates
end

fun explore arcTransformer
	    stateTransformer
	    hooks
	    initStorage
	    initStates =
    Exploration.sweepExplore
	Measure.getProgress
	{ sweepHook = debugSweepHook (fn _ => ()),
	  gcHook    = debugGcHook (fn (_, _, _, sVal, aVal) => (sVal, aVal)) }
	arcTransformer
	stateTransformer
	hooks
	initStorage
	initStates

end



(*
 *  sweep line exploration to collect statistics
 *)
functor SweepLineStatisticsExploration(
val markInitStatesAsPersistent : bool
structure Storage : REMOVE_STORAGE
structure Model : MODEL
structure Measure : PROGRESS_MEASURE where
   type state = Model.state * Model.event list
sharing type Model.state = Storage.item
) = struct

type SweepLineExplorationReport = {
     visitedStates        : int,
     visitedArcs          : int,
     persistentStates     : int list,
     finalPersistentStates: int,
     peakStored           : int,
     peakQueue            : int,
     regressEdges         : int,
     forwardEdges         : int,
     nullEdges            : int,
     sweeps               : int,
     gcStates             : int list
}

structure Exploration = InternalSweepLineExploration (
val markInitStatesAsPersistent = false
structure Storage = Storage
structure Model = Model
structure Progress = Measure.Progress)

fun explore { a_initial,
	      s_initial,
	      arc_hook,
	      state_hook }
	    storage
	    initStates = let

    val sweeps = ref 0
    val persistentStates : int list ref = ref []
    val gcStates : int list ref = ref []
    val visitedStates = ref 0
    val visitedArcs = ref 0
    val regressEdges = ref 0
    val forwardEdges = ref 0
    val nullEdges = ref 0
    fun sweepHook (roots, storage) = let
	val n = Storage.numItems storage
    in
	sweeps := (!sweeps) + 1;
	persistentStates := (List.length roots) :: (!persistentStates)
    end
    fun gcHook (toDelete, storage, prog, sVal, aVal) =
	(gcStates := (List.length toDelete) :: (!gcStates);
	 (sVal, aVal))
    fun arcHook (arc as (s1, _, s2), _, aVal) = let
	val s1Prog = Measure.getProgress s1
	val s2Prog = Measure.getProgress s2
	val var = case Measure.Progress.compare (s1Prog, s2Prog)
		   of GREATER => regressEdges
		    | LESS    => forwardEdges
		    | EQUAL   => nullEdges
    in
	visitedArcs := !visitedArcs + 1;
	var := !var + 1;
	arc_hook (arc, aVal)
    end
    fun stateHook (s, _, sVal) =
	(visitedStates := !visitedStates + 1;
	 state_hook (s, sVal))
    val (storage, sVal, aVal) =
	Exploration.sweepExplore
	    Measure.getProgress
	    { sweepHook = sweepHook,
	      gcHook    = gcHook }
	    (fn (_, evts) => evts)
	    (fn s => s)
	    { state_hook      = stateHook,
	      s_initial       = s_initial,
              arc_hook        = arcHook,
	      a_initial       = a_initial,
              pre_trace_hook  = fn (_, _, _, _, storage) => ((), storage),
	      post_trace_hook = fn (_, _, _, _, storage) => ((), storage),
	      t_initial       = () }
	    storage
	    initStates
in
    ({ visitedStates         = !visitedStates,
       visitedArcs           = !visitedArcs,
       persistentStates      = List.rev (!persistentStates),
       finalPersistentStates = Storage.numItems storage,
       peakStored            = !Exploration.peakNumStored,
       peakQueue             = !Exploration.peakNumQueue,
       regressEdges          = !regressEdges,
       forwardEdges          = !forwardEdges,
       nullEdges             = !nullEdges,
       sweeps                = !sweeps,
       gcStates              = List.rev (!gcStates) },
     sVal, aVal)
end

end
