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
 *     ts-sweep-ltl-checker.sml
 *
 *  Created:
 *     Jul. 17, 2009
 *
 *  Description:
 *     Implements the 2S-SWEEP ltl model checking algorithm based on the
 *  sweep-line method.
 *     
 *
 *)

functor NDFSSweep(
structure Model  : LTL_MODEL
structure Storage: EXPLICIT_REMOVE_STORAGE
   where type item = Model.state
structure Measure: PROGRESS_MEASURE
   where type state = Model.state * Model.event list
structure PQ     : PRIORITY_QUEUE
   where type Key.ord_key = Measure.Progress.ord_key) = struct

exception AcceptingCycle

fun eqProg progs = Measure.Progress.compare progs = EQUAL
				
fun sweepNDFS (stateHook1, stateHook2, arcHook1, arcHook2, insert) (V, R) = let
    fun dfs1 ((s, evts), (V, R, Q, D)) = let
	val prog = Measure.getProgress (s, evts)
	val _ = stateHook1 ((s, evts), prog)
	fun dfs2 ((s, evts), V) = let
	    val prog = Measure.getProgress (s, evts)
	    val _ = stateHook2 ((s, evts), prog)
            fun execEvent (evt, V) = let
		fun visitSucc ((s', evts'), V) = let
                    val ((_, onStack', flagged'), _) = Storage.getTag (V, s')
		    val prog' = Measure.getProgress (s', evts')
		in
		    arcHook2 ((s, evts), prog, (s', evts'), prog');
		    if not (eqProg (prog, prog'))
		    then V
		    else if onStack'
		    then raise AcceptingCycle
		    else if not flagged'
		    then dfs2 ((s', evts'), V)
		    else V
		end
            in
		List.foldl visitSucc V (Model.nextStates (s, evt))
            end
	    val ((true, onStack, false), mapTag) = Storage.getTag (V, s)
	    val V = Storage.setTag (V, s, ((true, onStack, true), mapTag))
	    val V = List.foldl execEvent V evts
	in
            V
	end
	fun execEvent (evt, (V, R, Q, D)) = let
            fun visitSucc ((s', evts'), (V, R, Q, D)) = let
		val prog' = Measure.getProgress (s', evts')
		val cmp = Measure.Progress.compare (prog, prog')
		val (_, seen, V) = insert (V, s')
            in
		arcHook1 ((s, evts), prog, (s', evts'), prog');
		if seen
		then let val ((visited', _, _), _) = Storage.getTag (V, s')
		     in
			 case (cmp, visited')
			  of (EQUAL, false) =>
			     dfs1 ((s', evts'), (V, R, Q, D))
			   | _ => (V, R, Q, D)
		     end
		else case cmp
		      of EQUAL   =>
			 dfs1 ((s', evts'), (V, R, Q, s' :: D))
		       | GREATER =>
			 (V, (prog', (s', evts')) :: R, Q, D)
		       | LESS    =>
			 (V, R, PQ.insert ((prog', false, (s', evts')), Q), D)
            end
	in
            List.foldl visitSucc (V, R, Q, D) (Model.nextStates (s, evt))
	end
	val V = Storage.setTag (V, s, ((true, true, false), NONE))
	val (V, R, Q, D) = List.foldl execEvent (V, R, Q, D) evts
	val V = if Model.accepting s
		then dfs2 ((s, evts), V)
		else V
	val V = Storage.setTag (V, s, ((true, false, true), NONE))
    in
	(V, R, Q, D)
    end
    fun loop (V, R, Q) =
	case PQ.next Q
	 of NONE => (V, R)
	  | SOME (progRef, _, _) => let
		fun fillQueue Q =
		    case PQ.next Q
		     of NONE => ([], Q)
		      | SOME (prog, persistent, s) =>
			if not (eqProg (prog, progRef))
			then ([], Q)
			else let val (_, Q) = PQ.remove Q
				 val (l, Q) = fillQueue Q
			     in
				 ((s, persistent) :: l, Q)
			     end
		fun explore ((se as (s, _), pers), data as (V, R, Q, D)) = let
		    val (V, R, Q, D) = if #1 (#1 (Storage.getTag (V, s)))
				       then (V, R, Q, D)
				       else dfs1 (se, data)
		in
		    (V, R, Q, if pers then D else s :: D)
		end
		val (l, Q) = fillQueue Q
		val (V, R, Q, D) = List.foldl explore (V, R, Q, []) l
		val V = List.foldl (fn (s, V) => Storage.delete (V, s)) V D
	    in
		loop (V, R, Q)
	    end
    val Q = List.foldl (fn ((p, s), Q) => PQ.insert ((p, true, s), Q))
		       (PQ.mkQueue #1) R
in
    loop (V, [], Q)
end

end


		  
functor TSSweepLTLChecker(
structure Model  : LTL_MODEL
structure Storage: EXPLICIT_REMOVE_STORAGE
   where type item = Model.state
structure Measure: PROGRESS_MEASURE
   where type state = Model.state * Model.event list
) = struct

structure Prog = Measure.Progress

structure PMMap = RedBlackMapFn (structure K = Measure.Progress)

datatype ltl_result =
	 NO_ACCEPTING_CYCLE
       | ACCEPTING_CYCLE of Model.event list 

exception Cycle of Model.event list

structure SweepExploration = SweepLineExploration (
val markInitStatesAsPersistent = false
structure Storage = Storage
structure Model = Model
structure Measure = Measure)

structure NDFSChecker = NDFSLTLChecker(
structure Storage = Storage
structure Model = Model)

fun check transformEvents
	  transformState
	  initOptions
	  initStates = let
	     
    fun recCheck pg progMap toVisit
		 { stored, visited, iterations, pgSize } = let
	val _ = print (String.concat [
		       "***  new iteration starts - ",
		       "progress graph size: ",
		       case pg
			of NONE => "n.a."
			 | SOME pg => Int.toString (Graph.numNodes pg),
		       "  ***\n" ])
	val stored = ref stored
	val visited = ref visited

	fun getProgress s = let
	    val prog = Measure.getProgress s
	in
	    case PMMap.find (progMap, prog)
	     of SOME prog => prog
	      | NONE => prog
	end

	fun visit s =
	    case toVisit
	     of NONE => true
	      | SOME toVisit =>
		isSome (PMMap.find (toVisit, getProgress s))
		
	(*
	 *  sweep-line hook for the garbage collection phasis:
	 *   - when a group of states is garbage collected we launch a nested
	 *     depth-first search (if this group contains some accepting state)
	 *     on all deleted states to find an accepting cycle.
	 *   - when a state with a different progress measure than the one of
	 *     garbage collected states is reached we delete all its enabled
	 *     events to not visit this state (this sucks but i did not find
	 *     any other way)
	 *)
	fun gcHook (deleted, storage, prog, _, _) = let
	    val NDFSStorage = NDFSChecker.emptyStorage initOptions
	    fun transformEvents' (s as (_, evts)) =
		if Prog.compare (getProgress s, prog) = EQUAL
		then transformEvents s
		else []
	    fun check (se as (s, _)) =
		if Model.accepting s
		then ignore (
		     NDFSChecker.check transformEvents'
				       transformState
				       { a_initial  = (),
					 s_initial  = (),
					 state_hook = fn _ => (),
					 arc_hook   = fn _ => () }
				       NDFSStorage
				       [ se ])
		else ()
	    val numItems = Storage.numItems storage
	in
	    if numItems > !stored
	    then stored := numItems
	    else ();
	    List.app check deleted;
	    ((), ())
	end

	(*
	 *  all necessary stuffs for the construction of the progress graph
	 *)
	val g = Graph.empty ()
	val progresses = ref PMMap.empty
	fun newNode increment (p, accepting) = 
	    case PMMap.find (!progresses, p)
	     of SOME n => let
		    val (p, s, a) = Graph.getNodeTag g n
		    val s = if increment then s + 1 else s
		    val a = a orelse accepting
		in
		    Graph.setNodeTag g (n, (p, s, a));
		    n
		end
	      | NONE => let
		    val n = Graph.newNode g (p, 1, accepting)
		in
		    progresses := PMMap.insert (!progresses, p, n);
		    n
		end
	fun stateHook modifyGraph (se as (s, _), _, _) =
	    (if modifyGraph
	     then ignore (newNode true (getProgress se, Model.accepting s))
	     else ();
	     visited := !visited + 1;
	     if !visited mod 100000 = 0
	     then print (Int.toString (!visited) ^ "\n")
	     else ())
	fun arcHook ((arc as (se as (s, _), _, se' as (s', _)), _, _)) = let
	    val n = newNode false (getProgress se, Model.accepting s)
	    val n' = newNode false (getProgress se', Model.accepting s')
	in
	    if List.exists (fn n'' => n' = n'') (Graph.getSucc g n)
	    then ()
	    else ignore (Graph.newEdge g (n, n', ()))
	end
	val (stateHook, arcHook) =
	    if isSome pg
	    then (stateHook false, fn _ => ())
	    else (stateHook true, arcHook)
	val result =
	    (SweepExploration.sweepExplore
		 getProgress
		 { startSweepHook = fn _ => (),
		   endSweepHook   = fn _ => (),
		   gcHook         = gcHook,
		   visit          = visit }
		 transformEvents
		 transformState
		 { a_initial       = (),
		   s_initial       = (),
		   t_initial       = (),
		   pre_trace_hook  = fn (_, _, _, _, storage) => ((), storage),
		   post_trace_hook = fn (_, _, _, _, storage) => ((), storage),
		   state_hook      = stateHook,
		   arc_hook        = arcHook }
		 (Storage.emptyStorage initOptions ())
		 initStates;
	     NONE)
	    handle NDFSChecker.AcceptingCycle trace =>
		   SOME ({ stored     = !stored,
			   visited    = !visited,
			   iterations = iterations,
			   pgSize     = if pgSize = 0
					then Graph.numNodes g
					else pgSize },
			 ACCEPTING_CYCLE trace)
		   
	(*
	 *  functions below are used to redefine the progress mapping after a
	 *  sweep-line exploration
	 *)
	fun mergeSCC (pg, progMap, toVisit, scc) = let
	    fun mergeNodes l = let
		val p = List.hd (ListMergeSort.uniqueSort Prog.compare
							  (List.map #1 l))
		val s = List.foldl (fn ((_, s, _), sum) => sum + s) 0 l
	    in
		(p, s, true)
	    end
	    val (pg, new) =
		Graph.mergeSCCs mergeNodes List.hd pg [ List.map #1 scc ]
	    val (n, tag as (min, _, _)) = List.hd new
	    val ancestors = Graph.listAncestors pg n
	    val toVisit = List.foldl (fn ((_, (p, _, _)), map) =>
					 PMMap.insert (map, p, ()))
				     toVisit ((n, tag) :: ancestors)
	    val progMap =
		List.foldl
		    (fn ((_, (p, _, _)), map) => let
			    val map = PMMap.mapi (fn (p', min') =>
						     if Prog.compare
							    (min', p) = EQUAL
						     then min
						     else min') map
			in
			    PMMap.insert (map, p, min)
			end)
		    progMap scc
	in
	    (pg, progMap, toVisit)
	end
	fun estimateSCC [ ] = raise LibBase.Impossible ""
	  | estimateSCC [ _ ] = NONE
	  | estimateSCC l =
	    if List.all (fn (_, (_, _, accepting)) => not accepting) l
	    then NONE
	    else SOME (List.foldl (fn ((_, (_, s, _)), sum) => sum + s) 0 l)
    	fun computeMin pg =
	    Graph.foldSCCs pg (fn (scc, NONE) => estimateSCC scc
				| (scc, SOME min) =>
				  case estimateSCC scc
				   of NONE => SOME min
				    | SOME e => SOME (Int.min (e, min)))
			   NONE
    	fun chooseAndMergeSCC (pg, progMap, toVisit, min) =
	    case Graph.foldSCCs
		     pg (fn (scc, NONE) =>
			    (case estimateSCC scc
			      of NONE => NONE
			       | SOME e => if e <= (min * 150) div 100
					   then SOME scc
					   else NONE)
			  | (_, some) => some) NONE
	     of NONE => NONE
	      | SOME scc => SOME (mergeSCC (pg, progMap, toVisit, scc))
	fun modifyProgressMapping (pg, progMap) =
	    case computeMin pg
	     of NONE => NONE
	      | SOME min => let
		    fun loop (pg, progMap, toVisit) =
			case chooseAndMergeSCC (pg, progMap, toVisit, min)
			 of NONE => SOME (pg, progMap, toVisit)
			  | SOME (pg, progMap, toVisit) =>
			    loop (pg, progMap, toVisit)
		in
		    loop (pg, progMap, PMMap.empty)
		end
    in
	case result
	 of SOME result => result
	  | NONE => let
		val pgSize = case pg of NONE => Graph.numNodes g | _ => pgSize
		val pg = case pg of NONE => g | SOME pg => pg
	    in
		case modifyProgressMapping (pg, progMap)
		 of NONE => ({ stored     = !stored,
			       visited    = !visited,
			       iterations = iterations,
			       pgSize     = pgSize }, NO_ACCEPTING_CYCLE)
		  | SOME (pg, progMap, toVisit) =>
		    recCheck
			(SOME pg) progMap (SOME toVisit)
			{ stored     = !stored,
			  visited    = !visited,
			  iterations = iterations + 1,
			  pgSize     = pgSize }
	    end
    end
    val t0 = Time.now ()
    val ({ stored, visited, iterations, pgSize }, result) =
	recCheck NONE PMMap.empty NONE
		 { stored     = 0,
		   visited    = 0, 
		   iterations = 1,
		   pgSize     = 0 }
in
    ({stored     = stored,
      visited    = visited,
      iterations = iterations,
      time       = Time.- (Time.now (), t0),
      pgSize     = pgSize },
     result)
end
				
end
(*****************************************************************************)






(*****************************************************************************)
functor NSweepLTLChecker(
structure Model  : LTL_MODEL
structure Storage: EXPLICIT_REMOVE_STORAGE
   where type item = Model.state
structure Measure: PROGRESS_MEASURE
   where type state = Model.state * Model.event list
) = struct

structure Prog = Measure.Progress

structure PMMap = RedBlackMapFn (structure K = Measure.Progress)

datatype ltl_result =
	 NO_ACCEPTING_CYCLE
       | ACCEPTING_CYCLE of Model.event list 

exception Cycle of Model.event list

structure SweepExploration = SweepLineExploration (
val markInitStatesAsPersistent = false
structure Storage = Storage
structure Model = Model
structure Measure = Measure)

structure NDFSChecker = NDFSLTLChecker(
structure Storage = Storage
structure Model = Model)

fun check transformEvents
	  transformState
	  initOptions
	  initStates = let

    val preTraceHook  = fn (_, _, _, _, storage) => ((), storage)
    val postTraceHook = fn (_, _, _, _, storage) => ((), storage)
    
    (*
     *  for statistics
     *)
    val stored = ref 0
    val visited = ref 0
    val iterations = ref 0

    (*
     *  exploration hooks for the construction of the progress graph
     *)
    val progGraph = Graph.empty ()
    val progresses = ref PMMap.empty
    fun newNode increment (p, accepting) = 
	case PMMap.find (!progresses, p)
	 of SOME n => let
		val (p, s, a) = Graph.getNodeTag progGraph n
		val s = if increment then s + 1 else s
		val a = a orelse accepting
	    in
		Graph.setNodeTag progGraph (n, (p, s, a));
		n
	    end
	  | NONE => let
		val n = Graph.newNode progGraph (p, 1, accepting)
	    in
		progresses := PMMap.insert (!progresses, p, n);
		n
	    end
    fun stateHook _ =
	(visited := !visited + 1;
	 if !visited mod 100000 = 0
	 then print (Int.toString (!visited) ^ "\n")
	 else ())
    fun stateHookChangePG (se as (s, _), _, _) =
	(newNode true (Measure.getProgress se, Model.accepting s);
	 stateHook se)
    fun arcHookChangePG (((se as (s, _), _, se' as (s', _)), _, _)) = let
	val n = newNode false (Measure.getProgress se, Model.accepting s)
	val n' = newNode false (Measure.getProgress se', Model.accepting s')
    in
	if List.exists (fn n'' => n' = n'') (Graph.getSucc progGraph n)
	then ()
	else ignore (Graph.newEdge progGraph (n, n', ()))
    end
		
    (*
     *  when a class of states is garbage collected (for both levels):
     *  - perform nested depth first searchs from accepting states to detect
     *    cycles
     *  - do not visit states with a different progress measures than the one
     *    of garbage collected states
     *)
    fun gcHook getProgress (deleted, storage, prog, _, _) = let
	val NDFSStorage = NDFSChecker.emptyStorage initOptions
	fun transformEvents' (s as (_, evts)) =
	    if Prog.compare (getProgress s, prog) = EQUAL
	    then transformEvents s
	    else []
	fun check (se as (s, _)) =
	    if Model.accepting s
	    then ignore (NDFSChecker.check transformEvents'
					   transformState
					   { a_initial  = (),
					     s_initial  = (),
					     state_hook = fn _ => (),
					     arc_hook   = fn _ => () }
					   NDFSStorage
					   [ se ])
	    else ()
	val numItems = Storage.numItems storage
    in
	if numItems > !stored
	then stored := numItems
	else ();
	List.app check deleted;
	((), ())
    end

    val first = ref true

    (*
     *  when a sweep of the first level terminates
     *)
    fun endSweepHook (oldRoots, newRoots, storage) = let
	val progsRoots = List.map Measure.getProgress oldRoots
	val progsRoots = ListMergeSort.uniqueSort Prog.compare progsRoots
	val nodesRoots = List.map (fn r => valOf (PMMap.find (!progresses, r)))
				  progsRoots
	fun mergeSCC (g, progMap, toVisit, scc) = let
	    fun mergeNodes l = let
		val p = List.hd (ListMergeSort.uniqueSort Prog.compare
							  (List.map #1 l))
		val s = List.foldl (fn ((_, s, _), sum) => sum + s) 0 l
	    in
		(p, s, true)
	    end
	    val (g, new) =
		Graph.mergeSCCs mergeNodes List.hd g [ List.map #1 scc ]
	    val (n, tag as (min, _, _)) = List.hd new
	    val ancestors = Graph.listAncestors g n
	    val toVisit = List.foldl (fn ((_, (p, _, _)), map) =>
					 PMMap.insert (map, p, ()))
				     toVisit ((n, tag) :: ancestors)
	    val progMap =
		List.foldl
		    (fn ((_, (p, _, _)), map) => let
			    val map = PMMap.mapi (fn (p', min') =>
						     if Prog.compare
							    (min', p) = EQUAL
						     then min
						     else min') map
			in
			    PMMap.insert (map, p, min)
			end)
		    progMap scc
	in
	    (g, progMap, toVisit)
	end
	fun estimateSCC [ ] = raise LibBase.Impossible ""
	  | estimateSCC [ _ ] = NONE
	  | estimateSCC l =
	    if List.all (fn (_, (_, _, accepting)) => not accepting) l
	    then NONE
	    else SOME (List.foldl (fn ((_, (_, s, _)), sum) => sum + s) 0 l)
	fun chooseAndMergeSCC (g, progMap, toVisit, min) =
	    case Graph.foldSCCs
		     g (fn (scc, NONE) =>
			   (case estimateSCC scc
			     of NONE => NONE
			      | SOME e => if e <= (min * 150) div 100
					  then SOME scc
					  else NONE)
			 | (_, some) => some) NONE
	     of NONE => NONE
	      | SOME scc => SOME (mergeSCC (g, progMap, toVisit, scc))
	fun mergeSCCS (g, progMap) = let
	    val min =
		Graph.foldSCCs g (fn (scc, NONE) => estimateSCC scc
				    | (scc, SOME min) =>
				      case estimateSCC scc
				       of NONE => SOME min
					| SOME e => SOME (Int.min (e, min)))
			       NONE
	in
	    case min
	     of NONE => NONE
	      | SOME min => let
		    fun loop (g, progMap, toVisit) =
			case chooseAndMergeSCC (g, progMap, toVisit, min)
			 of NONE => SOME (g, progMap, toVisit)
			  | SOME (g, progMap, toVisit) =>
			    loop (g, progMap, toVisit)
		in
		    loop (g, progMap, PMMap.empty)
		end
	end
	fun loop (g, progMap) =
	    case mergeSCCS (g, progMap)
	     of NONE => ()
	      | SOME (g, progMap, toVisit) => let
		    fun getProgress s = let
			val prog = Measure.getProgress s
		    in
			case PMMap.find (progMap, prog)
			 of SOME prog => prog
			  | NONE => prog
		    end
		    fun visit s = isSome (PMMap.find (toVisit, getProgress s))
		    exception Done
		in
		    ignore (
		    SweepExploration.sweepExplore
			getProgress
			{ startSweepHook = fn _ => (),
			  endSweepHook   = fn _ => raise Done,
			  gcHook         = gcHook getProgress,
			  visit          = visit }
			transformEvents
			transformState
			{ a_initial       = (),
			  s_initial       = (),
			  t_initial       = (),
			  pre_trace_hook  = preTraceHook,
			  post_trace_hook = postTraceHook,
			  state_hook      = stateHook,
			  arc_hook        = fn _ => () }
			(Storage.emptyStorage initOptions ())
			oldRoots)
		    handle Done => ();
		    loop (g, progMap)
		end
    in
	if !first
	then first := false
	else loop (Graph.subGraph progGraph nodesRoots, PMMap.empty)
    end

    val t0 = Time.now ()
    val result =
	(SweepExploration.sweepExplore
	     Measure.getProgress
	     { startSweepHook = fn _ => iterations := !iterations + 1,
	       endSweepHook   = endSweepHook,
	       gcHook         = gcHook Measure.getProgress,
	       visit          = fn _ => true }
	     transformEvents
	     transformState
	     { a_initial       = (),
	       s_initial       = (),
	       t_initial       = (),
	       pre_trace_hook  = preTraceHook,
	       post_trace_hook = postTraceHook,
	       state_hook      = stateHookChangePG,
	       arc_hook        = arcHookChangePG }
	     (Storage.emptyStorage initOptions ())
	     initStates;
	 NO_ACCEPTING_CYCLE)
	handle NDFSChecker.AcceptingCycle _ => ACCEPTING_CYCLE []
in
    ({stored     = !stored,
      visited    = !visited,
      iterations = !iterations,
      time       = Time.- (Time.now (), t0),
      pgSize     = Graph.numNodes progGraph },
     result)
end
				
end
(*****************************************************************************)






(*****************************************************************************)
functor NSweepLTLChecker(
structure Model  : LTL_MODEL
structure Storage: EXPLICIT_REMOVE_STORAGE
   where type item = Model.state
structure Measure: PROGRESS_MEASURE
   where type state = Model.state * Model.event list
) = struct

structure Prog = Measure.Progress

structure PMMap = RedBlackMapFn (structure K = Measure.Progress)

datatype ltl_result =
	 NO_ACCEPTING_CYCLE
       | ACCEPTING_CYCLE of Model.event list 

exception Cycle of Model.event list

structure SweepExploration = SweepLineExploration (
val markInitStatesAsPersistent = false
structure Storage = Storage
structure Model = Model
structure Measure = Measure)

structure NDFSChecker = NDFSLTLChecker(
structure Storage = Storage
structure Model = Model)

fun check transformEvents
	  transformState
	  initOptions
	  initStates = let

    val preTraceHook  = fn (_, _, _, _, storage) => ((), storage)
    val postTraceHook = fn (_, _, _, _, storage) => ((), storage)
						    
    (*
     *  for statistics
     *)
    val stored = ref 0
    val visited = ref 0
    val iterations = ref 0

    (*
     *  exploration hooks for the construction of the progress graph
     *)
    val progGraph = Graph.empty ()
    val progresses = ref PMMap.empty
    fun newNode increment (p, accepting) = 
	case PMMap.find (!progresses, p)
	 of SOME n => let
		val (p, s, a) = Graph.getNodeTag progGraph n
		val s = if increment then s + 1 else s
		val a = a orelse accepting
	    in
		Graph.setNodeTag progGraph (n, (p, s, a));
		n
	    end
	  | NONE => let
		val n = Graph.newNode progGraph (p, 1, accepting)
	    in
		progresses := PMMap.insert (!progresses, p, n);
		n
	    end
    fun stateHook _ =
	(visited := !visited + 1;
	 if !visited mod 100000 = 0
	 then print (Int.toString (!visited) ^ "\n")
	 else ())
    fun stateHookChangePG (se as (s, _), _, _) =
	(newNode true (Measure.getProgress se, Model.accepting s);
	 stateHook se)
    fun arcHookChangePG (((se as (s, _), _, se' as (s', _)), _, _)) = let
	val n = newNode false (Measure.getProgress se, Model.accepting s)
	val n' = newNode false (Measure.getProgress se', Model.accepting s')
    in
	if List.exists (fn n'' => n' = n'') (Graph.getSucc progGraph n)
	then ()
	else ignore (Graph.newEdge progGraph (n, n', ()))
    end
		
    (*
     *  when a class of states is garbage collected (for both levels):
     *  - perform nested depth first searchs from accepting states to detect
     *    cycles
     *  - do not visit states with a different progress measures than the one
     *    of garbage collected states
     *)
    fun gcHook (deleted, storage, prog, _, _) = let
	val NDFSStorage = NDFSChecker.emptyStorage initOptions
	fun transformEvents' (s as (_, evts)) =
	    if Prog.compare (Measure.getProgress s, prog) = EQUAL
	    then transformEvents s
	    else []
	fun check (se as (s, _)) =
	    if Model.accepting s
	    then ignore (NDFSChecker.check transformEvents'
					   transformState
					   { a_initial  = (),
					     s_initial  = (),
					     state_hook = fn _ => (),
					     arc_hook   = fn _ => () }
					   NDFSStorage
					   [ se ])
	    else ()
	val numItems = Storage.numItems storage
    in
	if numItems > !stored
	then stored := numItems
	else ();
	List.app check deleted;
	((), ())
    end

    val first = ref true

    (*
     *  when a sweep of the first level terminates
     *)
    fun endSweepHook (oldRoots, newRoots, sweepStorage) = let
	exception Done
	val listToMap = List.foldl
			    (fn (prog, map) => PMMap.insert (map, prog, ()))
			    PMMap.empty
	val sccProgGraph = Graph.computeSCCGraph (List.map (#1 o #2)) progGraph
	val accepting = Graph.foldNodes progGraph
					(fn (_, (p, _, true), l) => p :: l
					  | (_, _, l) => l) []
	val accepting = listToMap accepting
	fun gcHook (deleted, storage, prog, _, _) =
	    if not (PMMap.inDomain (accepting, prog))
	    then ((), ())
	    else let fun isIn (l, p) =
			 isSome (
			 List.find (fn p' => Prog.compare (p, p') = EQUAL) l)
		     val progsSCC = Graph.foldNodes sccProgGraph
						    (fn (_, [ _ ], l) => l
						      | (_, l, l') =>
							if isIn (l, prog)
							then l
							else l') []
		     val progsSCC = listToMap progsSCC
		     val NDFSStorage = NDFSChecker.emptyStorage initOptions
		     fun transformEvents' (s as (_, evts)) =
			 if PMMap.inDomain (progsSCC, Measure.getProgress s)
			 then transformEvents s
			 else []
		     fun check s =
			 NDFSChecker.check transformEvents'
					   transformState
					   { a_initial  = (),
					     s_initial  = (),
					     state_hook = fn _ => (),
					     arc_hook   = fn _ => () }
					   NDFSStorage
					   [ s ]
		     val accepting = List.filter (Model.accepting o #1) deleted
		 in
		     if PMMap.isEmpty progsSCC
		     then ()
		     else List.app (ignore o check) accepting;
		     let val numItems = Storage.numItems storage +
					Storage.numItems sweepStorage +
					Storage.numItems NDFSStorage
		     in
			 if numItems > !stored
			 then stored := numItems
			 else ()
		     end;
		     ((), ())
		 end
	val nodes = Graph.listNodes progGraph
	val nodesSCC = Graph.listNodes sccProgGraph
	fun getSCC _ (_, [ _ ]) = false
	  | getSCC p (_, l) =
	    List.exists (fn p' => Prog.compare (p, p') = EQUAL) l
	val nodes = List.mapPartial
			(fn (_, (_, _, false)) => NONE
			  | (n, (p, _, true)) =>
			    if List.exists (getSCC p) nodesSCC
			    then SOME (n, p)
			    else NONE)
			nodes
	val toVisit = List.foldl
			  (fn ((n, p), map) =>
			      List.foldl (fn ((n, (p, _, _)), map) =>
					     PMMap.insert (map, p, ()))
					 map (Graph.listAncestors progGraph n))
			  PMMap.empty nodes
	fun visit s = isSome (PMMap.find (toVisit, Measure.getProgress s))
    in
	if !first
	then first := false
	else (ignore (SweepExploration.sweepExplore
			  Measure.getProgress
			  { startSweepHook = fn _ => (),
			    endSweepHook   = fn _ => raise Done,
			    gcHook         = gcHook,
			    visit          = visit }
			  transformEvents
			  transformState
			  { a_initial       = (),
			    s_initial       = (),
			    t_initial       = (),
			    pre_trace_hook  = preTraceHook,
			    post_trace_hook = postTraceHook,
			    state_hook      = stateHook,
			    arc_hook        = fn _ => () }
			  (Storage.emptyStorage initOptions ())
			  oldRoots)) handle Done => ()
    end

    val t0 = Time.now ()
    val result =
	(SweepExploration.sweepExplore
	     Measure.getProgress
	     { startSweepHook = fn _ => iterations := !iterations + 1,
	       endSweepHook   = endSweepHook,
	       gcHook         = gcHook,
	       visit          = fn _ => true }
	     transformEvents
	     transformState
	     { a_initial       = (),
	       s_initial       = (),
	       t_initial       = (),
	       pre_trace_hook  = preTraceHook,
	       post_trace_hook = postTraceHook,
	       state_hook      = stateHookChangePG,
	       arc_hook        = arcHookChangePG }
	     (Storage.emptyStorage initOptions ())
	     initStates;
	 NO_ACCEPTING_CYCLE)
	handle NDFSChecker.AcceptingCycle _ => ACCEPTING_CYCLE []
in
    ({stored     = !stored,
      visited    = !visited,
      iterations = !iterations,
      time       = Time.- (Time.now (), t0),
      pgSize     = Graph.numNodes progGraph },
     result)
end
				
end
(*****************************************************************************)






(*****************************************************************************)
functor MapSweepLTLChecker(
structure Model  : LTL_MODEL
structure Storage: EXPLICIT_REMOVE_STORAGE
   where type item = Model.state
structure Measure: PROGRESS_MEASURE
   where type state = Model.state * Model.event list
structure Order  : ORD_KEY
   where type ord_key = Model.state
) = struct

structure Prog = Measure.Progress

structure PMMap = RedBlackMapFn (structure K = Measure.Progress)
structure PQ = PriorityQueue (
structure Key = struct
type ord_key = Prog.ord_key
fun compare keys = case Prog.compare keys of EQUAL => EQUAL
					   | LESS => GREATER
					   | GREATER => LESS
end)

fun cmpIntProg ((i1, k1), (i2, k2)) = case Int.compare (i1, i2)
				       of EQUAL => Prog.compare (k1, k2)
					| result => result
structure IntProgPQ = PriorityQueue (
structure Key = struct
type ord_key = int * Prog.ord_key
fun compare keys = case cmpIntProg keys of EQUAL => EQUAL
					 | LESS => GREATER
					 | GREATER => LESS
end)

datatype ltl_result =
	 NO_ACCEPTING_CYCLE
       | ACCEPTING_CYCLE of Model.event list 

exception AcceptingCycle

structure NDFSSweep = NDFSSweep(
structure Model   = Model
structure Storage = Storage
structure Measure = Measure
structure PQ      = PQ)

fun check endCheck
	  transformEvents
	  transformState
	  initOptions
	  initStates = let
    
    val cmp = Prog.compare
    fun eqProg progs = cmp progs = EQUAL
    val preTraceHook  = fn (_, _, _, _, storage) => ((), storage)
    val postTraceHook = fn (_, _, _, _, storage) => ((), storage)
    val listToMap = List.foldl (fn (prog, map) => PMMap.insert (map, prog, ()))
			       PMMap.empty
						    
    (*
     *  for statistics
     *)
    val stored = ref 0
    val visited = ref 0
    val iterations = ref 0
    val t0 = Time.now ()
    fun printStates k name int =
	if !int mod k = 0
	then print (Int.toString (!int) ^ " states " ^ name ^ "\n")
	else ()
    fun oneVisit () = (visited := !visited + 1;
		       printStates 10000 "visited" visited)
    fun insert (V, s) = let
	val result as (_, _, V) = Storage.add (V, s)
	val n = Storage.numItems V
    in
	if n > !stored
	then stored := n
	else ();
	result
    end

    (*
     *  exploration hooks for the construction of the progress graph
     *)
    val progGraph = Graph.empty ()
    val progresses = ref PMMap.empty
    fun newNode (p, accepting) = 
	case PMMap.find (!progresses, p)
	 of SOME n => let
		val (p, a) = Graph.getNodeTag progGraph n
		val a = a orelse accepting
	    in
		Graph.setNodeTag progGraph (n, (p, a));
		n
	    end
	  | NONE => let
		val n = Graph.newNode progGraph (p, accepting)
	    in
		progresses := PMMap.insert (!progresses, p, n);
		n
	    end

    (**************************************************************************
     *  nested depth-first search
     ************************************************************************
    fun dfs1 (prog, n) ((s, evts), (V, R, Q, D)) = let
	val _ = oneVisit ()
	fun dfs2 ((s, evts), V) = let
	    val _ = oneVisit ()
            fun execEvent (evt, V) = let
		fun visitSucc ((s', evts'), V) = let
                    val ((_, onStack', flagged'), _) = Storage.getTag (V, s')
		in
		    if eqProg (prog, Measure.getProgress (s', evts'))
		    then if onStack'
			 then raise AcceptingCycle
			 else if not flagged'
			 then dfs2 ((s', evts'), V)
			 else V
		    else V
		end
            in
		List.foldl visitSucc V (Model.nextStates (s, evt))
            end
	    val ((true, onStack, false), mapTag) = Storage.getTag (V, s)
	    val V = Storage.setTag (V, s, ((true, onStack, true), mapTag))
	    val V = List.foldl execEvent V evts
	in
            V
	end
        fun execEvent (evt, (V, R, Q, D)) = let
            fun visitSucc ((s', evts'), (V, R, Q, D)) = let
		val prog' = Measure.getProgress (s', evts')
		val cmp = cmp (prog, prog')
                val (_, seen, V) = insert (V, s')
		val n' = newNode (prog', Model.accepting s', not seen)
            in
		if List.exists (fn n'' => n' = n'') (Graph.getSucc progGraph n)
		then ()
		else ignore (Graph.newEdge progGraph (n, n', ()));
                if seen
                then let val ((visited', _, _), _) = Storage.getTag (V, s')
		     in
			 case (cmp, visited')
			  of (EQUAL, false) =>
			     dfs1 (prog, n) ((s', evts'), (V, R, Q, D))
			   | _ => (V, R, Q, D)
		     end
                else case cmp
		      of EQUAL   =>
			 dfs1 (prog, n) ((s', evts'), (V, R, Q, s' :: D))
		       | GREATER =>
			 (V, (prog', (s', evts')) :: R, Q, D)
		       | LESS    =>
			 (V, R, PQ.insert ((prog', false, (s', evts')), Q), D)
            end
        in
            List.foldl visitSucc (V, R, Q, D) (Model.nextStates (s, evt))
        end
	val V = Storage.setTag (V, s, ((true, true, false), NONE))
	val (V, R, Q, D) = List.foldl execEvent (V, R, Q, D) evts
	val V = if Model.accepting s
		then dfs2 ((s, evts), V)
		else V
	val V = Storage.setTag (V, s, ((true, false, true), NONE))
    in
        (V, R, Q, D)
    end

    fun sweepNDFS (V, R, Q) =
	case PQ.next Q
	 of NONE => (V, R)
	  | SOME (progRef, _, _) => let
		val n = PMMap.lookup (!progresses, progRef)
		fun loop Q =
		    case PQ.next Q
		     of NONE => ([], Q)
		      | SOME (prog, persistent, s) =>
			if not (eqProg (prog, progRef))
			then ([], Q)
			else let val (_, Q) = PQ.remove Q
				 val (l, Q) = loop Q
			     in
				 ((s, persistent) :: l, Q)
			     end
		fun explore ((se as (s, _), pers), data as (V, R, Q, D)) = let
		    val (V, R, Q, D) = if #1 (#1 (Storage.getTag (V, s)))
				       then (V, R, Q, D)
				       else dfs1 (progRef, n) (se, data)
		in
		    (V, R, Q, if pers then D else s :: D)
		end
		val (l, Q) = loop Q
		val (V, R, Q, D) = List.foldl explore (V, R, Q, []) l
		val V = List.foldl (fn (s, V) => Storage.delete (V, s)) V D
	    in
		sweepNDFS (V, R, Q)
	    end*)

    (**************************************************************************
     *  mrp search
     *************************************************************************)
    fun mrp (Q, V, R, toVisit) = let
	val S = Storage.map #1 R
	fun visit (s, evts) (iter, prog) (Q, V, S, G) = let
	    val _ = oneVisit ()
	    val (r, acc) = valOf (#2 (Storage.getTag (V, s)))
	    val (S, propagate) =
		if Storage.contains (R, s)
		then case Order.compare (r, s)
		      of LESS => (#3 (Storage.add (S, s)),
				  (s, Model.accepting s))
		       | GREATER => (if Storage.contains (S, s)
				     then Storage.delete (S, s)
				     else S,
				     (r, acc))
		       | EQUAL => (S, (r, acc))
		else if Model.accepting s
		then (S, (r, true))
		else (S, (r, acc))
	    fun execEvent (evt, (Q, V, S, G)) = let
		fun visitSucc (((s', evts'), prog'), (Q, V, S, G)) = let
		    fun checkPropagation ((r, acc), NONE) = SOME (r, acc)
		      | checkPropagation ((r, acc), SOME (r', acc')) =
			case Order.compare (r, r')
			 of GREATER => SOME (r, acc)
			  | EQUAL => if acc andalso not acc'
				     then SOME (r, true)
				     else NONE
			  | LESS => NONE
		    val prog' = (case cmp (prog, prog')
				  of GREATER => iter + 1
				   | _ => iter,
				 prog')
		    val (_, seen', V) = insert (V, s')
		    val _ = case propagate
			     of (_, false) => ()
			      | (r, true) => if r = s'
					     then raise AcceptingCycle
					     else ()
		    val G = if seen'
			    then G
			    else IntProgPQ.insert ((prog', s'), G)
		    val (dfsTag', mrpTag') = Storage.getTag (V, s')
		    val (V, Q) =
			case checkPropagation (propagate, mrpTag')
			 of NONE => (V, Q)
			  | some =>
			    (Storage.setTag (V, s', (dfsTag', some)),
			     IntProgPQ.insert ((prog', (s', evts')), Q))
		in
		    (Q, V, S, G)
		end
		fun mapSucc s' = let
		    val prog' = Measure.getProgress s'
		in
		    if PMMap.inDomain (toVisit, prog')
		       andalso (not (cmp (prog, prog') = GREATER)
				orelse Storage.contains (V, #1 s'))
		    then SOME (s', prog')
		    else NONE
		end		    
		val succs = List.mapPartial mapSucc (Model.nextStates (s, evt))
	    in
		List.foldl visitSucc (Q, V, S, G) succs
	    end
	in
	    List.foldl execEvent (Q, V, S, G) evts
	end
	fun loop (Q, V, S, G) = let
	    fun gc (V, G, prog) =
		case IntProgPQ.next G
		 of NONE => (V, G)
		  | SOME (prog', s) =>
		    if isSome prog
		       andalso cmpIntProg (prog', valOf prog) <> LESS
		    then (V, G)
		    else let val V = Storage.delete (V, s)
			     val (_, G) = IntProgPQ.remove G
			 in
			     gc (V, G, prog)
			 end
	in
	    if IntProgPQ.isEmpty Q
	    then (#1 (gc (V, G, NONE)), S)
	    else let val ((p, s), Q) = IntProgPQ.remove Q
		     val (V, G) = gc (V, G, SOME p)
		     val (Q, V, S, G) = visit s p (Q, V, S, G)
		 in
		     loop (Q, V, S, G)
		 end
	end
    in
	loop (Q, V, S, IntProgPQ.mkQueue #1)
    end

    fun sweepMRP (V, R) = let
	fun loop (V, R, toVisit) =
	    if Storage.isEmpty R
	    then ()
	    else let fun enqueue (s, evts, (V, Q)) = let
			 val se = (s, evts)
			 val p = Measure.getProgress se
			 val (dfsTag, _) = Storage.getTag (V, s)
			 val V = Storage.setTag
				     (V, s,
				      (dfsTag, SOME (s, Model.accepting s)))
			 val Q = IntProgPQ.insert (((0, p), se), Q)
		     in
			 (V, Q)
		     end
		     val (V, Q) = Storage.foldl
				      enqueue (V, IntProgPQ.mkQueue #1) R
		     val (V, S) = mrp (Q, V, R, toVisit)
		     val V = Storage.foldl
				 (fn (s, _, V) => let
					 val (dfsTag, _) =
					     Storage.getTag (V, s)
				     in
					 Storage.setTag (V, s, (dfsTag, NONE))
				     end) V R
		     val R = Storage.foldl
				 (fn (s, _, R) => Storage.delete (R, s)) R S
		 in
		     loop (V, R, toVisit)
		 end
	val sccProgGraph = Graph.computeSCCGraph (List.map #2) progGraph
	val sccs = List.map #2 (Graph.listNodes sccProgGraph)
	val sccs = List.mapPartial (fn scc => if List.exists #2 scc
					      then SOME (List.map #1 scc)
					      else NONE) sccs
	val progRoots = List.map #1 R
	val progRoots = ListMergeSort.uniqueSort cmp progRoots
	fun exploreSCC [ _ ] = ()
	  | exploreSCC scc = let
		fun find l p = List.exists (fn p' => eqProg (p, p')) l
		val progRoots = List.filter (find scc) progRoots
		val R = List.filter (fn (p, _) => find progRoots p) R
		val R = List.foldl (fn ((_, (s, evts)), R) => let
					   val R = #3 (Storage.add (R, s))
				       in
					   Storage.setTag (R, s, evts)
				       end)				     
				   (Storage.emptyStorage initOptions []) R
	    in
		loop (V, R, listToMap scc)
	    end
    in
	List.app exploreSCC sccs
    end

    val first = ref false

    fun stateHook1 ((s, _), prog) =
	(newNode (prog, Model.accepting s);
	 oneVisit ())

    fun stateHook2 _ = oneVisit ()

    fun arcHook1 ((s, _), prog, (s', _), prog') = let
	val n = newNode (prog, Model.accepting s)
	val n' = newNode (prog', Model.accepting s')
    in
	if List.exists (fn n'' => n' = n'') (Graph.getSucc progGraph n)
	then ()
	else ignore (Graph.newEdge progGraph (n, n', ()))
    end

    fun arcHook2 _ = ()

    val ndfsHooks = (stateHook1, stateHook2, arcHook1, arcHook2, insert)

    fun sweepStepByStep (V, []) = ()
      | sweepStepByStep (V, R) = let
	    val (V, R') = NDFSSweep.sweepNDFS ndfsHooks (V, R)
	in
	    if !first
	    then first := false
	    else sweepMRP (V, R);
	    sweepStepByStep (V, R')
	end

    fun sweepEnd (V, R) = let
	fun loop (V, []) = (V, [])
	  | loop (V, R) = let
		val result = NDFSSweep.sweepNDFS ndfsHooks (V, R)
		val (V, R') = loop result
	    in
		(V, R :: R')
	    end
	val (V, R) = loop (V, R)
    in
	sweepMRP (V, List.concat R)
    end

    val sweep = if endCheck
		then sweepEnd
		else sweepStepByStep

    val V = Storage.emptyStorage initOptions ((false, false, false), NONE)

    val (V, R) = List.foldl
		     (fn (se as (s, _), (V, R)) => let
			     val p = Measure.getProgress se
			     val (_, seen, V) = insert (V, s)
			     val n = newNode (p, Model.accepting s)
			 in
			     (V, (Measure.getProgress se, se) :: R)
			 end)
		     (V, []) initStates

    val result = (sweep (V, R);
		  NO_ACCEPTING_CYCLE)
	handle AcceptingCycle => ACCEPTING_CYCLE []
in
    ({stored     = !stored,
      visited    = !visited,
      iterations = !iterations,
      time       = Time.- (Time.now (), t0),
      pgSize     = Graph.numNodes progGraph },
     result)
end
				
end
