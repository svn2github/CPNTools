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
 *     ndfs-ltl-checker.sml
 *
 *  Created:
 *     Jul. 12, 2009
 *
 *  Description:
 *     Provides the nested-depth first search algorithm for ltl model checking:
 *  Memory Efficient Algorithms for the Verification of Temporal Properties.
 *  In CAV'1990. C. Courcoubetis and M.Y. Vardi and P. Wolper and M. Yannakakis
 *
 *  To do:
 *     - the algorithm seems ok. comparisons were made with divine on all
 *  beem instances (with < 1000000 states) and the results were the same but
 *  we should check that error traces provided are correct
 *
 *)


functor NDFSLTLChecker(
structure Model  : GAME_MODEL
structure Storage: REMOVE_STORAGE
   where type item = Model.state) = struct

exception AcceptingCycle of (Model.state * Model.event * Model.state) list

structure Exploration = DFSExploration(
structure Model = Model
structure Storage = Storage) : TRACE_EXPLORATION where type state = Storage.item


fun emptyStorage initOptions = Storage.emptyStorage initOptions false

fun check transformEvents
	  transformState
	  { a_initial,
	    s_initial,
	    state_hook,
	    arc_hook }
	  initStorage
	  initStates = let

    (*
     *  parameters and hooks for the 2nd DFS
     *  -  when an arc reaches a state on the 1st DFS stack (i.e., its tag is
     *     set to true) we have found an accepting cycle
     *  -  special case for the root (accepting) state since it is inserted to
     *     the storage during the 2nd exploration and hence its tag is set to
     *     false 
     *)
    fun arcHook2 (((s, _), e, (s', _)), (root, trace, storage), _) =
	if Storage.getTag (storage, s') orelse s' = root
	then raise AcceptingCycle (List.rev ((s, e, s') :: trace))
	else ()
    fun dfs2Parameters (root, trace, storage) =
	{ a_initial       = (),
	  s_initial       = (),
	  t_initial       = (root, trace, storage),
	  pre_trace_hook  = fn (((s, _), e, (s', _)), _, _, (root, trace, _), storage) =>
			       ((root, (s, e, s') :: trace, storage), storage),
	  post_trace_hook = fn ((_, e, _), _, _, (root, trace, _), storage) =>
			       ((root, List.tl trace, storage), storage),
	  state_hook      = fn (_, _, _) => (),
	  arc_hook        = arcHook2 }

    (*
     *  parameters and hooks for the 1st DFS
     *  -  we keep track of states that are backtracked from so that we can
     *     delete them from the storage when the 2nd DFS starts.
     *  -  when reaching a new state, we set its tag to true to specify it is
     *     on the 1st DFS stack.
     *  -  when a state s' is backtracked from, if it is accepting we first
     *     delete all the states that were reachable from s' from the storage
     *     and then launch the 2nd DFS.  in any case we then set its tag to
     *     false to specify it has left the 1st DFS stack.
     *)
    fun preTraceHook1 (((s, _), e, (s', _)), id, id',
		       (trace, toDelete), storage) =
	(((s, e, s') :: trace, toDelete), Storage.setTag' (storage, id', true))
    fun postTraceHook1 ((_, _, (s', es')), id, id',
			(trace, toDelete), storage) =
	if not (Model.winning (s', es'))
	then ((List.tl trace, id' :: toDelete),
	      Storage.setTag' (storage, id', false))
	else let val storage = List.foldl (fn (id, storage) =>
					      Storage.delete' (storage, id))
					  storage (id' :: toDelete)
		 val (storage, _, _) =
		     Exploration.explore transformEvents
					 transformState
					 (dfs2Parameters (s', trace, storage))
					 storage
					 [ (s', es') ]
	     in
		 ((List.tl trace, []), Storage.setTag' (storage, id', false))
	     end
    val dfs1Parameters =
	{ a_initial       = a_initial,
	  s_initial       = s_initial,
	  t_initial       = ([], []),
	  pre_trace_hook  = preTraceHook1,
	  post_trace_hook = postTraceHook1,
	  state_hook      = fn (s, _, sVal) => state_hook (s, sVal),
	  arc_hook        = fn (arc, _, aVal) => arc_hook (arc, aVal) }
    val (_, sVal, aVal) =
	Exploration.explore transformEvents
			    transformState
			    dfs1Parameters
			    initStorage
			    initStates
in
    (sVal, aVal)
end

end

