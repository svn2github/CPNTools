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
 *     Provide a comback storage extended with a delayed duplicate detection
 *  mechanism taken from:
 *  The ComBack Method Revisited: Caching Strategies and Extension with
 *  Delayed Duplicate Detection. In ToPNoC'2009.
 *  Sami Evangelista, Michael Westergaard and Lars Michael Kristensen.
 *
 *  To do:
 *  - undeterministic systems are not supported
 *  - some operations are not supported (e.g., add. use insert instead)
 *)


signature COMBACK_DDD_STORAGE = sig

    include STORAGE

    exception OutOfMemory

    type event
	 
    type 'a candidate_set

    datatype reconstructing_item = EVENT
				 | SEQUENCE
		     
    datatype insert_answer = NEW of id
			   | OLD of id
			   | MAYBE
	 
    type pred_info = id * event * reconstructing_item * int

    val hashId:
	id
	-> word

    val emptyCandidateSet:
	'a storage
	-> 'b candidate_set
					 
    val insert:
	'a storage
	-> 'b candidate_set
	-> item * event list
	-> 'b
	-> pred_info option
	-> insert_answer

    val setPredecessor:
	'a storage
	-> id
	-> pred_info
	-> unit
		
    val duplicateDetection:
	('c * id option * ((item * event list) * id * 'b) -> 'c)
	-> 'a storage
	-> 'b candidate_set
	-> 'c
	-> bool
	-> 'c
				      
    val cacheState:
	'a storage * (item * event list) * id * real
	-> unit
								    
    val reconstruct:
	'a storage * id
	-> item * event list
					
    val reconstructList:
	'a storage * (id * 'b) list
	-> ((item * event list) * id * 'b) list
			 
    val unrefState:
	'a storage
	-> id
	-> unit
					
    val garbageCollection:
	'a storage
	-> 'b candidate_set
	-> ((id -> id) -> 'c -> 'c)
	-> 'c
	-> 'c

    val appTag:
	('a -> unit)
	-> 'a storage
	-> unit

    val appId:
	(id -> unit)
	-> 'a storage
	-> unit

    val foldTag:
	('a * 'b -> 'b)
	-> 'b
	-> 'a storage
	-> 'b

    val getReconstitutionExecutions:
	'a storage
	-> LargeInt.int
						   
    val getSuccessorsDistribution:
	'a storage
	-> (int * int) list
						  
    val peakNumItems:
	'a storage
	-> int

    val printStorage:
	'a storage
	-> 'b candidate_set
	-> unit

    val toDotty:
	'a storage
	-> string
	-> unit

    val getId:
	'a storage
	-> item
	-> id
	    
end



signature COMBACK_DDD_STORAGE_CACHE = sig

    type 'a cache

    val empty:
	int
	-> 'a cache

    val insert:
	'a cache
	-> int * 'a
	-> bool * int option

    val insert':
	'a cache
	-> (int * 'a) * real
	-> (int -> int)
	-> bool * int option

    val find:
	'a cache
	-> int
	-> 'a option

    val delete:
	'a cache
	-> int
	-> unit

    val map:
	(int -> int)
	-> 'a cache
	-> unit

end

signature COMBACK_DDD_STORAGE_L1_CACHE = sig
    include COMBACK_DDD_STORAGE_CACHE
end
 
signature COMBACK_DDD_STORAGE_L2_CACHE = sig
    include COMBACK_DDD_STORAGE_CACHE
end



structure IntMap: sig
    type 'a map

    val init:
	int
	-> 'a map

    val insert:
	'a map
	-> int
	-> 'a
	-> ('a -> 'a)
	-> bool

    val remove:
	'a map
	-> int
	-> 'a option

    val find:
	'a map
	-> int
	-> 'a option

    val fold:
	'a map
	-> (int * 'a * 'b -> 'b)
	-> 'b
	-> 'b

    val app:
	'a map
	-> (int * 'a -> unit)
	-> unit

    val numItems:
	'a map
	-> int

    val modifyKey:
	(int -> int)
	-> 'a map
	-> unit

end = struct

type 'a map = (int * 'a) list Array.array * int ref

fun init size =
    (Array.tabulate (if size = 0 then 1 else size, fn i => []), ref 0)

fun numItems (_, n) = !n

fun insert (map, numItems) i value update = let
    fun loop [] = (true, [(i, value)])
      | loop ((i', value') :: l) =
	if i = i'
	then (false, (i, update value') :: l)
	else let val (inserted, l) = loop l
	     in (inserted, (i', value') :: l) end
    val k = i mod Array.length map
    val (inserted, l) = loop (Array.sub (map, k))	    
in
    if inserted
    then numItems := !numItems + 1
    else ();
    Array.update (map, k, l);
    inserted
end

fun remove (map, numItems) i = let
    fun loop [] = (NONE, [])
      | loop ((i', value') :: l) =
	if i = i'
	then (SOME value', l)
	else let val (removed, l) = loop l
	     in (removed, (i', value') :: l) end
    val k = i mod Array.length map
    val (removed, l) = loop (Array.sub (map, k))
in
    if isSome removed
    then numItems := !numItems - 1
    else ();
    Array.update (map, k, l);
    removed
end

fun find (map, _) i = let
    fun isI (i', _) = i = i'
in
    case List.find isI (Array.sub (map, i mod Array.length map))
     of NONE => NONE
      | SOME (_, v) => SOME v 
end

fun fold (map, _) f value =
    Array.foldl
	(fn (l, value) =>
	    List.foldl (fn ((i, item), value) => f (i, item, value)) value l)
	value map

fun app (map, _) f = Array.app (fn l => List.app f l) map

fun modifyKey mapKey (map as (a, _)) = let
    val map' as (a', _) = init (Array.length a)
in
    app map (fn (i, v) => (insert map' (mapKey i) v (fn i => i); ()));
    Array.modifyi (fn (i, _) => Array.sub (a', i)) a
end

end



(*
 *  ComBack-DDD storage.  only for internal use,  use functors below instead
 *)
functor InternalComBackDDDStorage(
type encoded_event
eqtype state
eqtype event
val nullEvent: encoded_event
val getEncodedEvent: event * int -> encoded_event
val getEvent: (state * event list) * encoded_event -> event
val s0: state * event list
val nextState: (state * event list) * event -> state * event list
structure Hash: HASH_FUNCTION
structure Cache: COMBACK_DDD_STORAGE_CACHE
sharing type Hash.state = state
) = struct

exception OutOfMemory

val fnName = "InternalComBackDDDStorage"

val gcRatio = 0.1

type encoded_event = encoded_event

type event = event

type item = state

type key = word

type id = int

datatype reconstructing_item = EVENT
			     | SEQUENCE

datatype insert_answer = NEW of id
		       | OLD of id
		       | MAYBE

type pred_info = id * event * reconstructing_item * int

type init_options = {
     tableSize       : int,
     candidateSetSize: int,
     combackCacheSize: int,
     stateCacheSize  : int option
}

datatype 'a candidate_holder =
         ONE of (state * event list * id * reconstructing_item *
		 encoded_event * 'a)
       | NO
       | DELETED of id

type bucket = (word * int) array

type 'a state_info = {
     flags: Word8.word,
     refs : Word8.word,
     pred : int,
     evt  : encoded_event,
     tag  : 'a
}

type 'a storage = {
     default         : 'a,
     tableSize       : int,
     candidateSetSize: int,
     combackCacheSize: int,
     stateCacheSize  : int option,
     next            : int ref,
     numItems        : int ref,
     peakNumItems    : int ref,
     executions      : LargeInt.int ref,
     backTable       : ('a state_info) array ref,
     stateTable      : bucket array,
     combackCache    : (state * event list) Cache.cache,
     toReconstruct   : id list ref,
     unrefed         : id list ref
}

type 'a candidate_set = {
     size         : int,
     candidates   : 'a candidate_holder array,
     numCandidates: int ref
}

val hashId = Word.fromInt

fun emptyStorage { tableSize, stateCacheSize,
		   candidateSetSize, combackCacheSize } default = let
    val stateTable = Array.tabulate (tableSize, fn i => Array.fromList [])
in
    {
     tableSize         = tableSize,
     candidateSetSize  = candidateSetSize,
     combackCacheSize  = combackCacheSize,
     next              = ref 0,
     numItems          = ref 0,
     peakNumItems      = ref 0,
     stateCacheSize    = stateCacheSize,
     executions        = ref (LargeInt.fromInt 0),
     default           = default,
     backTable         = ref (Array.fromList []),
     stateTable        = stateTable,
     combackCache      = Cache.empty combackCacheSize,
     toReconstruct     = ref [],
     unrefed           = ref []
    }
end

fun emptyCandidateSet ({ candidateSetSize, ... }: 'a storage) = {
    size          = candidateSetSize,
    numCandidates = ref 0,
    candidates    = Array.tabulate (if candidateSetSize <= 10
				    then 50
				    else candidateSetSize * 2, (fn i => NO))
}


(*
 *  flags associated with each state in the backedge table
 *  - del: the state is deleted and should be removed during the next garbage
 *     collection
 *  - check: the state must be checked against the candidate set during the
 *     duplicate detection next
 *  - sequence: to reconstruct the state we must execute from its predecessor
 *     its reconstructing event followed by a sequence of events until we reach
 *     a state with several executable events
 *  - cached: the state is in the comback cache
 *)
val emptyFlag = Word8.fromInt 0

fun setDel        flags = Word8.orb (flags, 0w1)
fun setCheck      flags = Word8.orb (flags, 0w2)
fun setSequence   flags = Word8.orb (flags, 0w4)
fun setCached     flags = Word8.orb (flags, 0w8)
fun unsetDel      flags = Word8.andb (flags, 0wxFE)
fun unsetCheck    flags = Word8.andb (flags, 0wxFD)
fun unsetSequence flags = Word8.andb (flags, 0wxFA)
fun unsetCached   flags = Word8.andb (flags, 0wxF7)
fun isSetDel      flags = Word8.andb (flags, 0w1) <> 0w0
fun isSetCheck    flags = Word8.andb (flags, 0w2) <> 0w0
fun isSetSequence flags = Word8.andb (flags, 0w4) <> 0w0
fun isSetCached   flags = Word8.andb (flags, 0w8) <> 0w0

fun changeFlags ({ backTable, ... }: 'a storage) f id = let
    val { flags, pred, evt, tag, refs } = Array.sub (!backTable, id)
in
    Array.update (!backTable, id, { flags = f flags,
				    pred  = pred,
				    evt   = evt,
				    tag   = tag,
				    refs  = refs })
end

fun printStorage ({ numItems, toReconstruct, stateTable, next,
		    backTable, ... }: 'a storage)
		 ({ candidates, ... } : 'b candidate_set) =
    (print "\n******************************\n";
     print "state table = [\n";
     Array.app (fn b => (Array.app (fn (k, id) =>
				       print ("   (" ^
					      (Word.toString k) ^ ", " ^
					      (Int.toString id) ^
					      ")\n")) b))
	       stateTable;
     print "]\n\nbackedge table = [\n";
     Array.appi (fn (i, { flags, pred, refs, ... }) =>
		    if i >= (!next)
		    then ()
		    else print (
			 (Int.toString i) ^ " =>\t(" ^
			 "pred = " ^ (Int.toString pred) ^ "\t" ^
			 "refs = " ^ (Word8.toString refs) ^ "\t" ^
			 "del = " ^ (Bool.toString (isSetDel flags))^ "\t" ^
			 "check = " ^ (Bool.toString (isSetCheck flags)) ^
			 ")\n"))
		(!backTable);
     print "]\n\nto reconstruct = [\n";
     print (List.foldl (fn (i, s) => (s ^ "   " ^ (Int.toString i) ^ "\n"))
		       "" (!toReconstruct));
     print "]\n\ncandidates = [\n";
     Array.app (fn ONE (_, _, id, _, _, _) =>
		   print ("   pred = " ^ (Int.toString id) ^ "\n")
		 | _ => ())
	       candidates;
     print "]\n******************************\n\n")

fun checkStorage (storage as { numItems, toReconstruct, stateTable, next,
			       backTable, ... }: 'a storage) = let
    val succs = Array.array (Array.length (!backTable), 0)
    fun checkPredecessor (_, { pred = ~1, ... }) = ()
      | checkPredecessor (id, { refs, pred, flags, ... }: 'a state_info) =
	if isSetDel flags orelse id >= !next
	then ()
	else (let val n = Array.sub (succs, pred)
	      in Array.update (succs, pred, n + 1) end;
	      let val { flags, ... } = Array.sub (!backTable, pred)
	      in
		  if isSetDel flags
		  then raise LibBase.Impossible
				 (fnName ^ ".checkStorage: non deleted " ^
				  "state with a deleted predecessor: " ^
				  (Int.toString id))
		  else if pred >= id
		  then raise LibBase.Impossible
				 (fnName ^ ".checkStorage: state " ^
				  (Int.toString id) ^
				  " has state " ^ (Int.toString pred) ^
				  " as predecessor")
		  else ()
	      end)
in
    Array.appi checkPredecessor (!backTable);
    Array.appi (fn (id, n) => let
		       val { refs, ... } = Array.sub (!backTable, id)
		   in
		       if id >= !next orelse Word8.toInt refs >= n
		       then ()
		       else raise LibBase.Impossible
				      (fnName ^ ".checkStorage: " ^
				       "invalid number of refs for state " ^
				       (Int.toString id) ^ ": " ^
				       (Int.toString n) ^ " > " ^
				       (Int.toString (Word8.toInt refs)))
		   end)
	       succs
end

fun nextCandidate ({ candidates, ... }: 'a candidate_set) 0 =
    Array.length candidates - 1
  | nextCandidate _ bid =
    bid - 1

fun expand a v = let
    val lg = Array.length a
in
    Array.tabulate (lg + 1, fn i => if i = lg then v else Array.sub (a, i))
end

fun bucket ({ tableSize, stateTable, ... }: 'a storage) k = let
    val bid = Word.toInt (Word.mod (k, Word.fromInt tableSize))
in
    (bid, Array.sub (stateTable, bid))
end

fun execStep (s, flags, evt) = let
    val evt = getEvent (s, evt)
    fun execChain s = let
	fun loop ((s, evts as [ evt ]), n) =
	    loop (nextState ((s, evts), evt), n + 1)
	  | loop res = res
    in
	loop (nextState (s, evt), 1)
    end
in
    if isSetSequence flags
    then execChain s
    else (nextState (s, evt), 1)
end

fun recons (storage as { combackCache, executions,
			 backTable, ... }: 'a storage) id = let
    fun backtrack 0 = s0
      | backtrack id = let
	    val { flags, pred, evt, ... } = Array.sub (!backTable, id)
	in
	    (*
	     * if isSetDel flags
	     * then raise LibBase.Impossible (fnName ^ ".recons")
	     * else ();
	     *)
	    if isSetCached flags
	    then valOf (Cache.find combackCache id)
	    else let val s = backtrack pred
		     val (s, n) = execStep (s, flags, evt)
		 in
		     executions := LargeInt.+ (!executions,
					       LargeInt.fromInt n);
		     s
		 end
	end
in    
    backtrack id
end

fun gc (storage as { next, numItems, stateTable, unrefed,
		     backTable, combackCache, toReconstruct, ... }: 'a storage)
       ({ candidates, ... }: 'b candidate_set) mapData data = let
    val _ = checkStorage storage
    val idMap = Array.tabulate (!next, fn _ => ~ 1)
    fun mapId ~1 = ~1
      | mapId id = case Array.sub (idMap, id)
		    of ~1 => raise LibBase.Impossible
				       (fnName ^ ".garbageCollection")
		     | res => res
in
    Array.modify (fn bucket =>
		     Array.fromList
			 (Array.foldl
			      (fn ((k, i), l) =>
				  if isSetDel (#flags
						   (Array.sub (!backTable, i)))
				  then l
				  else (k, i) :: l) [] bucket))
		 stateTable;
    Array.foldli (fn (i, { flags, pred, evt, tag, refs }, free) => 
		     if i >= !next orelse isSetDel flags
		     then free
		     else (Array.update (!backTable, free,
					 { flags = flags,
					   pred  = mapId pred,
					   evt   = evt,
					   tag   = tag,
					   refs  = refs });
			   Array.update (idMap, i, free);
			   free + 1))
		 0 (!backTable);
    Array.modifyi (fn (_, ONE (s, evts, id, t, ev, tag)) =>
		      ONE (s, evts, mapId id, t, ev, tag)
		    | _ => NO)
		  candidates;
    Array.app (fn b => Array.modify (fn (k, id) => (k, mapId id)) b)
	      stateTable;
    toReconstruct := List.map mapId (!toReconstruct);
    unrefed := List.map mapId (!unrefed);
    Cache.map mapId combackCache;
    next := !numItems;
    mapData mapId data before checkStorage storage
end

fun garbageCollection (storage as { next, numItems, ... }: 'a storage)
		      candSet mapData data =
    if Real.fromInt ((!next) - (!numItems)) < gcRatio * Real.fromInt (!next)
    then data
    else gc storage candSet mapData data

fun cacheNewState (storage as { combackCache, ... }: 'a storage,
		   (s, evts), id) = let
    val (inserted, removed) = Cache.insert combackCache (id, (s, evts))
in
    if not inserted
    then ()
    else (changeFlags storage setCached id;
	  case removed
	   of NONE => ()
	    | SOME id => changeFlags storage unsetCached id)
end

fun cacheState (storage as { backTable, combackCache, ... }: 'a storage,
		(s, evts), id, h) = let
    val (inserted, removed) =
	Cache.insert' combackCache ((id, (s, evts)), h)
		      (fn i => #pred (Array.sub (!backTable, i)))
in
    if not inserted
    then ()
    else (changeFlags storage setCached id;
	  case removed
	   of NONE => ()
	    | SOME id => changeFlags storage unsetCached id)
end
    
fun generateTree (storage as { backTable, combackCache,
			       candidateSetSize, ... }: 'a storage) ids = let
    val tree = IntMap.init (candidateSetSize * 2)
    fun backtrack id data succ = let
	val new = IntMap.insert
		      tree id
		      (case succ of NONE => (data, true, [])
				  | SOME succ => (data, false, [succ]))
		      (fn item as (data', visit, succs) =>
			  let val data = if isSome data'
					 then data'
					 else data
			  in
			      case succ of
				  NONE => (data, true, succs)
				| SOME succ => (data, visit, succ :: succs)
			  end)		  
    in
	if not new
	then NONE
	else let val { flags, pred, ... } = Array.sub (!backTable, id)
	     in
		 if pred = ~1
		 then if id <> 0
		      then raise LibBase.Impossible
				     (fnName ^ ".generateTree: " ^
				      "generation of a state with no pred\n")
		      else SOME (id, s0)
		 else if isSetCached flags
		 then SOME (id, valOf (Cache.find combackCache id))
		 else backtrack pred NONE (SOME id) 
	     end
    end
    val roots = List.foldl (fn ((id, data), l) =>
			       case backtrack id (SOME data) NONE
				of NONE => l
				 | SOME s => s :: l)
    			   [] ids
in
    (tree, roots)
end

fun foldTree (storage as { backTable, executions, ... }: 'a storage)
	     f init (tree, roots) = let
    fun dfs id (s, evts) (n, v) = let
	val (data, visit, succs) = valOf (IntMap.find tree id)
	fun visitSucc (succ, (n, v)) = let
	    val s = (s, evts)
	    val { evt, flags, ... } = Array.sub (!backTable, succ)
	    val (s, m) = execStep (s, flags, evt)
	in
	    dfs succ s (n + m, v)
	end
	val v = if visit
		then f (id, valOf data, (s, evts), v)
		else v
    in
	List.foldl visitSucc (n, v) succs
    end
    val (n, v) = List.foldl (fn ((id, s), (n, v)) => dfs id s (n, v))
			    (0, init) roots
in
    executions := LargeInt.+ (!executions, LargeInt.fromInt n);
    v
end

fun reconstruct (storage, id) = recons storage id

fun reconstructList (storage, []) = []
  | reconstructList (storage, ids) = let
        val tree = generateTree storage ids
    in
	foldTree storage (fn (id, data, s, l) => (s, id, data) :: l) [] tree
    end

fun refState ({ backTable, ... }: 'a storage) id = let
    val { flags, pred, evt, tag, refs } = Array.sub (!backTable, id)
in
    Array.update (!backTable, id, { flags = flags,
				    pred  = pred,
				    evt   = evt,
				    tag   = tag,
				    refs  = Word8.+ (refs, 0w1) })
end

fun unrefState ({ unrefed, backTable, ... }: 'a storage) id = let
    val { flags, pred, evt, tag, refs } = Array.sub (!backTable, id)
in
    Array.update (!backTable, id, { flags = flags,
				    pred  = pred,
				    evt   = evt,
				    tag   = tag,
				    refs  = Word8.- (refs, 0w1) });
    if refs = 0w1
    then unrefed := id :: !unrefed
    else ()
end
  
fun newEntry (storage as { numItems, unrefed, peakNumItems, combackCache,
			   default, next, stateCacheSize,
			   backTable, ... }: 'a storage)
	     (s, evts) (pred, evt, evtType) updatePredRefs = let
    fun double a v = let
	val lg  = Array.length a
	val nlg = if lg = 0 then 1 else 2 * lg
    in
	Array.tabulate (nlg, fn i => if i < lg then Array.sub (a, i) else v)
    end
    fun expand s =
	if (!next) < (Array.length (!backTable))
	then Array.update (!backTable, !next, s)
	else backTable := double (!backTable) s
    val flags = case evtType
		 of SEQUENCE => setSequence emptyFlag
		  | EVENT => emptyFlag
in
    expand { flags = flags,
	     pred  = pred,
	     evt   = evt,
	     tag   = default,
	     refs  = 0w1 };
    if pred <> ~1 andalso updatePredRefs
    then refState storage pred
    else ();
    numItems := !numItems + 1;
    if !peakNumItems >= !numItems
    then ()
    else peakNumItems := !numItems;
    case stateCacheSize
     of NONE => ()
      | SOME size =>
	if size >= !numItems
	then ()
	else let fun getFirst [] = raise OutOfMemory
		   | getFirst (id :: l) = let
			 val { flags, ... } = Array.sub (!backTable, id)
		     in
			 if not (isSetCheck flags)
			 then (id, l)
			 else let val (u, l) = getFirst l
			      in
				  (u, id :: l)
			      end
		     end
		 val (id, unrefed') = getFirst (!unrefed)
	     	 val { flags, pred, evt, tag, refs } =
		     Array.sub (!backTable, id)
	     in
		 Array.update (!backTable, id, { flags = setDel flags,
						 pred  = ~1,
						 evt   = evt,
						 tag   = tag,
						 refs  = 0w0 });
		 unrefed := unrefed';
		 numItems := !numItems - 1;
		 Cache.delete combackCache id;
		 if pred <> ~1
		 then unrefState storage pred
		 else ()
	     end;
    !next before next := !next + 1
end

fun insert (storage as { backTable, stateTable, combackCache,
			 toReconstruct, ... }: 'a storage)
	   (candSet as { numCandidates, candidates, size }: 'b candidate_set)
	   (s, evts) tag predInfo = let
    val bt = !backTable
    val k = Hash.hash s
    fun isS (k', id) = let
	val { flags, ... } = Array.sub (!backTable, id)
    in
	k = k'
	andalso not (isSetDel flags)
	andalso isSetCached flags
	andalso #1 (valOf (Cache.find combackCache id)) = s
    end
    val inCache = Array.find isS (#2 (bucket storage k))
in
    case inCache
     of SOME (_, id) => OLD id
      | NONE => let
	    val (bid, b) = bucket storage k
	    fun new () = let
		val predInfo =
		    case predInfo
		     of NONE => (~1, nullEvent, EVENT)
		      | SOME (id, evt, evtType, evtNum) =>
			(id, getEncodedEvent (evt, evtNum), evtType)
		val id = newEntry storage (s, evts) predInfo true
	    in
		Array.update (stateTable, bid, expand b (k, id));
		cacheNewState (storage, (s, evts), id);
		NEW id
	    end
	in
	    if size = 0
	    then case Array.find
			  (fn (k', id) => let
				  val { flags, ... } =
				      Array.sub (!backTable, id)
			      in
				  k = k'
				  andalso (not (isSetDel flags))
				  andalso (#1 (recons storage id)) = s
			      end) b
		  of SOME (_, id) => OLD id
		   | NONE => new ()
            else let val (conflicts, toReconstruct') =
			 Array.foldl
			     (fn ((k', id), (c, l)) =>
				 if k' <> k
				 then (c, l)
				 else let val { flags, pred, evt, tag, refs } =
					      Array.sub (!backTable, id)
				      in
					  if isSetDel flags
					  then (c, l)
					  else if isSetCheck flags
					  then (true, l)
					  else (Array.update
						    (!backTable, id,
						     { flags = setCheck flags,
						       pred  = pred,
						       evt   = evt,
						       tag   = tag,
						       refs  = refs });
						(true, id :: l))
				      end)
			     (false, []) b
		 in
		     toReconstruct := List.@ (toReconstruct', !toReconstruct);
		     if not conflicts
		     then new ()
		     else let val (pred, evt, evtType, evtNum) =
				  valOf predInfo
			      fun f bid =
				  case Array.sub (candidates, bid) of
				      NO => SOME bid
				    | ONE (s', _, _, _, _, _) =>
				      if s = s'
				      then NONE
				      else f (nextCandidate candSet bid)
				    | DELETED _ => raise LibBase.Impossible ""
			      val l = Word.fromInt (Array.length candidates)
			  in
			      case f (Word.toInt (Word.mod (k, l)))
			       of NONE => ()
				| SOME bid => let
				      val evt = getEncodedEvent (evt, evtNum)
				  in
				      numCandidates := !numCandidates + 1;
				      refState storage pred;
				      Array.update
					  (candidates, bid,
					   ONE (s, evts, pred,
						evtType, evt, tag))
				  end;
			      MAYBE
			  end
		 end
	end
end

fun setPredecessor (storage as { backTable, combackCache,
				 candidateSetSize, ... }: 'a storage) id
		   (pred, evt, evtType, evtNum) = let
    val evt = getEncodedEvent (evt, evtNum)
    val { flags, pred = old, tag, refs, ... } = Array.sub (!backTable, id)
    val flags = (case evtType of SEQUENCE => setSequence
			       | EVENT    => unsetSequence) flags
in
    Array.update (!backTable, id, { flags = flags,
				    pred  = pred,
				    evt   = evt,
				    tag   = tag,
				    refs  = refs });
    refState storage pred;
    unrefState storage old
end

fun duplicateDetection'
	newState
	(storage as { backTable, stateTable, toReconstruct, ... }: 'a storage)
	(candSet as { candidates, numCandidates, ... }: 'b candidate_set)
	data = let
    val tree = generateTree storage
			    (List.map (fn id => (id, ())) (!toReconstruct))
    fun visit (id, _, (s, _), _) = let
	fun delete bid =
	    case Array.sub (candidates, bid) of
		NO => ()
	      | DELETED _ => delete (nextCandidate candSet bid)
	      | ONE (s', _, pred, _, _, _) =>
		if s' = s
		then Array.update (candidates, bid, DELETED pred)
		else delete (nextCandidate candSet bid)
    in
	delete (Word.toInt (Word.mod (Hash.hash s,
				      Word.fromInt
					  (Array.length candidates))));
	changeFlags storage unsetCheck id
    end
    val data =
	(foldTree storage visit () tree;
         Array.foldl
             (fn (NO, data) => data
               | (DELETED _, data) => data
               | (ONE (s, evts, pred, evtType, evt, tag), data) => let
		     val k = Hash.hash s
		     val (bid, b) = bucket storage k
		     val id = newEntry
				  storage (s, evts) (pred, evt, evtType) false
		 in
		     Array.update (stateTable, bid, expand b (k, id));
                     newState (data, SOME pred, ((s, evts), id, tag))
		 end) data candidates)
in
    toReconstruct := [];
    numCandidates := 0;
    Array.modify (fn slot => 
		     (case slot of DELETED pred => unrefState storage pred
				 | _ => ();
		      NO))
		 candidates;
    data
end

fun duplicateDetection _ _ ({ numCandidates = ref 0, ... }: 'b candidate_set)
		       data _ = data
  | duplicateDetection newState storage candSet data true =
    duplicateDetection' newState storage candSet data
  | duplicateDetection
	newState storage
	(candSet as {size, numCandidates, ... }: 'b candidate_set) data false =
    if !numCandidates < size
    then data
    else duplicateDetection' newState storage candSet data

fun findId storage s = let
    val k = Hash.hash s
    val (_, b) = bucket storage k
in
    Array.find (fn (k', id) => k' = k andalso s = #1 (recons storage id)) b
end

fun getId storage s = case findId storage s of NONE => raise LibBase.NotFound
					     | SOME (_, id) => id

fun peakNumItems ({ peakNumItems, ... }: 'a storage) = !peakNumItems

fun getReconstitutionExecutions ({ executions, ... }: 'a storage) =
    !executions

fun toDotty ({ backTable, numItems, ...}: 'a storage) f = let
    val f = TextIO.openOut f
    val arcs = Array.foldli (fn (i, {pred, ...}, arcs) =>
				if pred = ~1 orelse i >= (!numItems)
				then arcs
				else (pred, i) :: arcs)
			    [] (!backTable)
in
    TextIO.output (f, "digraph G {\n");
    List.app (fn (src, dest) => (TextIO.output (f, Int.toString src);
				 TextIO.output (f, " -> ");
				 TextIO.output (f, Int.toString dest);
				 TextIO.output (f, "\n")))
	     (List.rev arcs);
    TextIO.output (f, "}\n") ;
    TextIO.closeOut f
end

fun getSuccessorsDistribution ({ backTable, numItems,
				 next, ...}: 'a storage) = let
    val refs = Array.tabulate (!next, fn _ => 0)
    val max = ref 0
    fun update (i, {pred, ...}: 'a state_info) =
	if pred = ~1 orelse i >= (!next)
	then ()
	else let val n = Array.sub (refs, pred) + 1
	     in
		 if !max >= n then () else max := n;
		 Array.update (refs, pred, n)
	     end
in
    Array.appi update (!backTable);
    let
	val a = Array.tabulate (!max + 1, fn _ => 0)
    in
	Array.app (fn i => Array.update (a, i, Array.sub (a, i) + 1)) refs;
	Array.foldri (fn (i, n, l) => (i, n) :: l) [] a
    end
end

(*  functions of STORAGE  *)
fun add (storage as {stateTable, ...}: 'a storage, s) = let
    val k = Hash.hash s
    val (bid, b) = bucket storage k
    val id = case Array.find (fn (k', id) => k' = k andalso
					     s = #1 (recons storage id)) b
	      of NONE => NONE
	       | SOME (_, id) => SOME id

in
    case id
     of SOME id => (id, true, storage)
      | NONE => raise LibBase.Unimplemented (fnName ^ ".add")
end

fun addList (storage, ss) = let
    fun addOne (s, (other, storage)) = let
        val (id, contained, storage') = add (storage, s)
    in
        ((id, contained) :: other, storage')
    end
in
    List.foldl addOne ([], storage) ss
end

fun contains (storage, s) =  isSome (findId storage s)

fun contains' ({ backTable, next, ... }: 'a storage, id) =
    id < (!next) andalso
    not (isSetDel (#flags (Array.sub (!backTable, id))))

fun isEmpty (storage as { numItems, ... }: 'a storage) = !numItems = 0
		    
fun numItems ({ numItems, ... }: 'a storage) = !numItems

fun lookup (storage, id) = #1 (recons storage id)

fun getTag (storage as { backTable, ... }: 'a storage, s) =
    #tag (Array.sub (!backTable, getId storage s))

fun getTag' (storage as { backTable, ... }: 'a storage, id) =
    #tag (Array.sub (!backTable, id))

fun setTag (storage as { backTable, ... }: 'a storage, s, newTag) = let
    val id = getId storage s
    val { flags, pred, evt, tag, refs } = Array.sub (!backTable, id)
in
    Array.update (!backTable, id, { flags = flags,
				    pred  = pred,
				    evt   = evt,
				    tag   = newTag,
				    refs  = refs });
    storage
end

fun setTag' (storage as { backTable, ... }: 'a storage, id, newTag) = let
    val { flags, pred, evt, tag, refs } = Array.sub (!backTable, id)
in
    Array.update (!backTable, id, { flags = flags,
				    pred  = pred,
				    evt   = evt,
				    tag   = newTag,
				    refs  = refs });
    storage
end

fun appTag f (storage as { next, backTable, ... }: 'a storage) =
    Array.appi (fn (i, { flags, tag, ... }) =>
		   if i >= (!next) orelse isSetDel flags
		   then ()
		   else f tag) (!backTable)
    
fun foldTag f v (storage as { next, backTable, ... }: 'a storage) = 
    Array.foldli (fn (i, { flags, tag, ... }, v) =>
		     if i >= (!next) orelse isSetDel flags
		     then v
		     else f (tag, v)) v (!backTable)

fun appId f (storage as { next, backTable, ... }: 'a storage) =
    Array.appi (fn (i, { flags, ... }) =>
		   if i >= (!next) orelse isSetDel flags
		   then ()
		   else f i) (!backTable)

end



(*****************************************************************************
 *  different types of storage
 *****************************************************************************)

(*
 *  events are stored as such in the backedge table
 *)
functor ComBackDDDStorage(
structure Model: MODEL
structure Hash : HASH_FUNCTION
structure Cache: COMBACK_DDD_STORAGE_CACHE
val init_states: (Model.state * Model.event list) list
sharing type Hash.state = Model.state): COMBACK_DDD_STORAGE = struct

val s0 = List.hd (init_states)

val nullEvent = List.hd (#2 s0)

type encoded_event = Model.event

fun getEncodedEvent (e, _) = e
fun getEvent (_, e) = e
fun nextState ((s, evts), e) = List.hd (Model.nextStates (s, e))

structure Storage = InternalComBackDDDStorage(
type encoded_event = encoded_event
type state = Model.state
type event = Model.event
val s0 = s0
val nextState = nextState
val nullEvent = nullEvent
val getEncodedEvent = getEncodedEvent
val getEvent = getEvent
structure Hash = Hash
structure Cache = Cache)

open Storage

end



(*
 *  events are stored as numbers in the backedge table (number of the event in
 *  the enabled list of the state)
 *)
functor ComBackDDDNumStorage(
structure Model: MODEL
structure Hash : HASH_FUNCTION
structure Cache: COMBACK_DDD_STORAGE_CACHE
sharing type Hash.state = Model.state): COMBACK_DDD_STORAGE = struct

val s0 = List.hd (Model.getInitialStates ())

val nullEvent = Word8.fromInt 0

type encoded_event = Word8.word

fun getEncodedEvent (_, num) = Word8.fromInt num
fun getEvent ((s, evts), n) = List.nth (evts, Word8.toInt n)
fun nextState ((s, evts), e) = List.hd (Model.nextStates (s, e))

structure Storage = InternalComBackDDDStorage(
type encoded_event = encoded_event
type state = Model.state
type event = Model.event
val s0 = s0
val nextState = nextState
val nullEvent = nullEvent
val getEncodedEvent = getEncodedEvent
val getEvent = getEvent
structure Hash = Hash
structure Cache = Cache)

open Storage

end



(*
 *  events are stored as pointers in the backedge table
 *)
functor ComBackDDDPointerStorage(
structure Model    : MODEL
structure HashState: HASH_FUNCTION
structure HashEvent: HASH_FUNCTION
structure Cache    : COMBACK_DDD_STORAGE_CACHE
sharing type HashState.state = Model.state
sharing type HashEvent.state = Model.event): COMBACK_DDD_STORAGE = struct

structure Dictionary = HashDictionary(
type item = Model.event
val hash = HashEvent.hash)

val s0 = List.hd (Model.getInitialStates ())

val nullEvent = Dictionary.nullRef

type encoded_event = Dictionary.item_ref
		     
fun getEncodedEvent (e, _) = Dictionary.insert e
fun getEvent (_, e) = Dictionary.retrieve e
fun nextState ((s, evts), e) = List.hd (Model.nextStates (s, e))

structure Storage = InternalComBackDDDStorage(
type encoded_event = encoded_event
type state = Model.state
type event = Model.event
val s0 = s0
val nextState = nextState
val nullEvent = nullEvent
val getEncodedEvent = getEncodedEvent
val getEvent = getEvent
structure Hash = HashState
structure Cache = Cache)

open Storage

end




(*****************************************************************************
 *  different types of cache
 *****************************************************************************)

structure RealPriority: PRIORITY = struct
type priority = real
type item = real * int
fun compare (r1, r2) = if Real.> (r1, r2)
		       then LESS
		       else if Real.< (r1, r2)
		       then GREATER
		       else EQUAL
fun priority (p, _) = p
end



structure ComBackDDDStorageFifoCache: COMBACK_DDD_STORAGE_L1_CACHE = struct

type 'a cache = {
     max  : int,
     next : int ref,
     items: int option Array.array,
     table: 'a IntMap.map
}

fun empty max = {
    max   = max,
    next  = ref 0,
    items = Array.tabulate (max, fn _ => NONE),
    table = IntMap.init (2 * max)
}

fun insert { max = 0, ... } _ = (false, NONE)
  | insert { max, next, items, table } (i, s) = let
	val removed = Array.sub (items, !next)
    in
	case removed of NONE => ()
		      | SOME j => ignore (IntMap.remove table j);
	IntMap.insert table i s (fn v => s);
	Array.update (items, !next, SOME i);
	next := (!next + 1) mod max;
	(true, removed)
    end
    
fun insert' _ _ _ = (false, NONE)

fun find { max, next, items, table } i = IntMap.find table i

fun delete { max, next, items, table } i = ignore (IntMap.remove table i)

fun map mapKey { max, next, items, table } = (
    Array.modify (fn NONE => NONE |
		     SOME i => if isSome (IntMap.find table i)
			       then SOME (mapKey i)
			       else NONE) items;
    IntMap.modifyKey mapKey table)

end



structure ComBackDDDStorageRandomCache: COMBACK_DDD_STORAGE_L1_CACHE = struct

type 'a cache = {
     rand : Random.rand,
     max  : int,
     items: int option Array.array,
     table: 'a IntMap.map
}

fun empty max = {
    rand  = Random.rand (1007, 65789),
    max   = max,
    items = Array.tabulate (max, fn _ => NONE),
    table = IntMap.init (2 * max)
}

fun insert { max = 0, ... } _ = (false, NONE)
  | insert { max, rand, items, table } (i, s) = let
	val idx = Random.randRange (0, max - 1) rand
    in
	case Array.sub (items, idx)
	 of NONE => (IntMap.insert table i s (fn v => s);
		     Array.update (items, idx, SOME i);
		     (true, NONE))
	  | SOME j => if Random.randRange (0, 1) rand = 0
		      then (false, NONE)
		      else (IntMap.remove table j;
			    IntMap.insert table i s (fn v => s);
			    Array.update (items, idx, SOME i);
			    (true, SOME j))
    end

fun insert' _ _ _ = (false, NONE)

fun find { max, rand, items, table } i = IntMap.find table i

fun delete { max, rand, items, table } i = (IntMap.remove table i; ())

fun map mapKey { max, rand, items, table } = (
    Array.modify (fn NONE => NONE |
		     SOME i => if isSome (IntMap.find table i)
			       then SOME (mapKey i)
			       else NONE) items;
    IntMap.modifyKey mapKey table)

end



structure ComBackDDDStorageHeuristicCache = struct

structure PQ = LeftPriorityQFn(RealPriority)

type 'a cache = {
     max  : int,
     num  : int ref,
     items: PQ.queue ref,
     table: 'a IntMap.map
}

fun empty max = {
    max   = max,
    num   = ref 0,
    items = ref PQ.empty,
    table = IntMap.init (2 * max)
}

fun insert _ _ = (false, NONE)

fun insert' { max = 0, ... } _ _ = (false, NONE)
  | insert' { max, num, items, table } ((i, s), h) _ =
    if max > !num
    then (num := !num + 1;
	  IntMap.insert table i s (fn i => i);
	  items := PQ.insert ((h, i), !items);
	  (true, NONE))
    else let fun findNext q = let
		 val ((h, i), q) = PQ.remove q
	     in
		 if isSome (IntMap.find table i)
		 then ((h, i), q)
		 else findNext q
	     end
	     val ((h', i'), items') = findNext (!items)
	 in
	     items := items';
	     if Real.>= (h', h)
	     then (items := PQ.insert ((h', i'), !items);
		   (false, NONE))
	     else (IntMap.remove table i';
		   IntMap.insert table i s (fn i => i);
		   items := PQ.insert ((h, i), !items);
		   (true, SOME i'))
	 end
	 
fun find { max, num, items, table } i = IntMap.find table i

fun cleanQueue queue table mapKey =
    if PQ.isEmpty queue
    then PQ.empty
    else let val ((h, i), queue) = PQ.remove queue
	     val queue = cleanQueue queue table mapKey
	 in
	     if isSome (IntMap.find table i)
	     then PQ.insert ((h, mapKey i), queue)
	     else queue
	 end

fun delete { max, num, items, table } i =
    if isSome (IntMap.remove table i)
    then num := !num - 1
    else ()

fun map mapKey { max, num, items, table } = (
    items := cleanQueue (!items) table mapKey;
    IntMap.modifyKey mapKey table)

end



functor ComBackDDDStorageDistanceCache(
val k: int): COMBACK_DDD_STORAGE_L2_CACHE = struct

open ComBackDDDStorageHeuristicCache

fun insert' { max = 0, ... } _ _ = (false, NONE)
  | insert' { max, num, items, table } ((i, s), h) getPred =
    if max > !num
    then (num := !num + 1;
	  IntMap.insert table i s (fn i => i);
	  items := PQ.insert ((h, i), !items);
	  (true, NONE))
    else let fun findNext q = let
		 val ((h, i), q) = PQ.remove q
	     in
		 if isSome (IntMap.find table i)
		 then ((h, i), q)
		 else findNext q
	     end
	     val ((h', i'), items') = findNext (!items)
	 in
	     items := items';
	     if Real.>= (h', h)
	     then (items := PQ.insert ((h', i'), !items);
		   (false, NONE))
	     else let fun backtrack 0 _ = true
			| backtrack n id = let
			      val pred = getPred id
			  in
			      not (isSome (IntMap.find table pred))
			      andalso backtrack (n - 1) pred 
			  end
		  in
		      if backtrack (k - 1) i
		      then (IntMap.remove table i';
			    IntMap.insert table i s (fn i => i);
			    items := PQ.insert ((h, i), !items);
			    (true, SOME i'))
		      else (items := PQ.insert ((h', i'), !items);
			    (false, NONE))
		  end
	 end

end



functor ComBackDDDStorageCombinationCache(
structure Cache1: COMBACK_DDD_STORAGE_L1_CACHE
structure Cache2: COMBACK_DDD_STORAGE_L2_CACHE
val prop: int * int): COMBACK_DDD_STORAGE_CACHE =
struct

type 'a cache = ('a Cache1.cache * 'a Cache2.cache)

fun empty max = let
    val size = max * (#1 prop) div (#2 prop)
in
    (Cache1.empty size, Cache2.empty (max - size))
end

fun insert (c1, c2) item = let
    val result as (inserted, removed) = Cache1.insert c1 item
in
    if inserted
    then result
    else Cache2.insert c2 item
end

fun insert' (c1, c2) item f = let
    val result as (inserted, removed) = Cache1.insert' c1 item f
in
    if inserted
    then result
    else Cache2.insert' c2 item f
end

fun find (c1, c2) i =
    case Cache1.find c1 i of
        SOME s => SOME s
      | NONE => Cache2.find c2 i

fun map mapKey (c1, c2) = (
    Cache1.map mapKey c1;
    Cache2.map mapKey c2)

fun delete (c1, c2) i = (
    Cache1.delete c1 i;
    Cache2.delete c2 i)

end



structure ComBackDDDStorageHeuristicCache: COMBACK_DDD_STORAGE_L2_CACHE =
ComBackDDDStorageHeuristicCache
