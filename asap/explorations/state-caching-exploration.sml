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
 *     state-caching-exploration.sml
 *
 *  Description:
 *     State caching based state space exploration.
 *
 *  Created:
 *    Nov. 13, 2007
 *
 *)


(*
 *  signature for mutable cache
 *)
signature STATE_CACHING_CACHE = sig
    
    type 'a cache
	 
    val emptyCache: int -> 'a cache

    (* 
     *  insert cache item => insert item in cache.  returns NONE if there were
     *  some room for item or SOME replaced if item replaced had to be removed
     *  to make room for item
     *)
    val insert: 'a cache -> 'a -> 'a option

    val doInsert: 'a cache -> int -> bool
				      
end


(*
 *  random caching strategy
 *)
functor RandomCache
(val replacementRate: int * int): STATE_CACHING_CACHE = struct

(*  array of items * max size * current size * random numbers generator  *)
type 'a cache = 'a option Array.array * int * int ref * Random.rand

val (hi, low) = replacementRate
		
fun emptyCache max =
    (Array.array (max, NONE), max, ref 0, Random.rand (17, 83))

fun insert (_, 0, _, _) _ = NONE
  | insert (items, max, current, rand) item =
    if !current < max
    then (Array.update (items, !current, SOME item);
	  current := !current + 1;
	  NONE)
    else let val replacedIndex = Random.randRange (0, max - 1) rand
	     val replaced = Array.sub (items, replacedIndex)
	 in
	     Array.update (items, replacedIndex, SOME item);
	     replaced
	 end

fun doInsert (_, 0, _, _) _ = false
  | doInsert (_, max, current, rand) _ =
    (!current < max) orelse (Random.randRange (1, low) rand) <= hi
			       
end


(*
 *  stratified caching strategy
 *)
functor StratifiedCache
(val k: int): STATE_CACHING_CACHE = struct

type 'a cache = unit
		
fun emptyCache max = ()

fun insert _ _ = NONE

fun doInsert _ depth = depth mod k = 0
			       
end



functor StateCachingExploration(
structure Storage : REMOVE_STORAGE
structure Exploration : TRACE_EXPLORATION
structure Cache : STATE_CACHING_CACHE
val cacheSize : int
sharing type Storage.storage = Exploration.storage
sharing type Exploration.state = Storage.item
sharing type Exploration.id = Storage.id
) : TRACE_EXPLORATION =
struct

type state      = Exploration.state
type id         = Exploration.id
type event      = Exploration.event
type 'a storage = 'a Exploration.storage

fun explore transform_arc transform_state
            {
	     state_hook,
	     s_initial,
	     arc_hook,
	     a_initial,
	     pre_trace_hook,
             post_trace_hook,
	     t_initial
	    } 
            storage initial_states = let

    val cache = Cache.emptyCache cacheSize
    val depth = ref 0

    fun pre_trace_hook' (arc, id, id', t_value, storage) =
	(depth := !depth + 1;
	 pre_trace_hook (arc, id, id', t_value, storage))

    fun post_trace_hook' (arc, id, id', t_value, storage) = let
	val _ = depth := !depth - 1
	val storage =
	    if Cache.doInsert cache (!depth)
	    then let val replaced = Cache.insert cache id'
		 in
		     case replaced
		      of NONE => storage
		       | SOME replaced => Storage.delete' (storage, replaced)
		 end
	    else Storage.delete' (storage, id')
    in
	post_trace_hook (arc, id, id', t_value, storage)
    end
		 
in
    Exploration.explore transform_arc transform_state
			{
			 state_hook = state_hook,
			 s_initial = s_initial,
			 arc_hook = arc_hook,
			 a_initial = a_initial,
			 pre_trace_hook = pre_trace_hook',
			 post_trace_hook = post_trace_hook',
			 t_initial = t_initial
			}
			storage initial_states
end
					  
end
