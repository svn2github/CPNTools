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
 *     bfs-disk-exploration.sml
 *
 *  Created:
 *     Jan. 10, 2008
 *
 *  Description:
 *     Provides the disk based algorithms BFS-HDDD and BFS-DDDD from
 *  Dynamic Delayed Duplicate Detection for External Memory Model Checking.
 *  In SPIN'2008.
 *  Sami Evangelista. LNCS # 5156.
 *
 *  To do:
 *  - since there are some problems to set/get position in files partial
 *  duplicate detection is turned off
 *  
 *)


signature BFS_DISK_EXPLORATION = sig

    include TRACE_EXPLORATION

    type report = {
	 errorRate   : Real.real,
	 detections  : int list,
	 layers      : int list,
	 layersApprox: int list,
	 expanded    : int
    }

    val makeReport: 'a storage -> (state * event list) list ->
		    'a storage * report

end


functor BFSDiskExploration (
val cacheSize          : int
val previousLayers     : int
val maxPartSize        : int
val dynamic            : bool
structure Model        : MODEL
structure Storage      : LAYERED_PART_EXT_STORAGE
structure StateEncoder : SERIALIZER where type src = Model.state
structure EventsEncoder: SERIALIZER where type src = Model.event list
): BFS_DISK_EXPLORATION = struct

val powSup        = 0.7
val powInf        = 0.2
val prevAcc       = 1
val readCost      = 1.0
val writeCost     = 2.0
val expansionCost = 1.0
val errorRate     = 0.0

val toInt   = Real.toInt IEEEReal.TO_NEAREST
val fromInt = Real.fromInt

val hashVector = HashWord8Vector.hash

type state = Model.state
type event = Model.event
type 'a storage = 'a Storage.storage
type id = unit
type report = {
     errorRate   : Real.real,
     detections  : int list,
     layers      : int list,
     layersApprox: int list,
     expanded    : int
}

type bit_vector = Word8Vector.vector


(*****
 *  queue used by the algorithm
 *****)
structure Queue: sig

    type queue

    type depth = int

    type part = int

    val init: string * string * int * int -> queue

    val enqueue: queue * (bit_vector * bit_vector) -> unit

    val incrDepth: queue -> unit

    val writeAndEmpty: queue -> unit

    val cacheTooSmall: queue -> bool

    val size: queue -> int

    val foldPart: ((bit_vector * bit_vector) * 'a -> 'a) -> 'a ->
		  (queue * depth * part) -> 'a

    val appPart: (bit_vector * bit_vector -> unit) ->
		 queue * depth * part -> unit
								     
    val appPartStates: (bit_vector -> unit) -> queue * depth * part -> unit

    val deleteFile: queue * depth * part -> unit

    val reorganizePartitions: queue -> unit

end = struct

structure Stream = Stream(structure Encoder = struct
type src  = bit_vector
type dest = bit_vector
fun map s = s
val unmap = map
end)

structure StreamPair = StreamPair(
structure Stream1 = Stream
structure Stream2 = Stream)

type depth = int
type part = int
type queue = {
     path         : string,
     prefix       : string,
     cacheSize    : int,
     maxPerSlot   : int,
     slots        : int,
     slotsW       : Word.word,
     partitions   : int ref,
     depth        : int ref,
     size         : int ref,
     toStore      : int ref,
     cacheTooSmall: bool ref,
     cache        : ((bool ref * bit_vector * bit_vector) list) array
}

fun statesFileName (path, prefix, depth, partId) = let
    val file = String.concat [ prefix, "-",
			       Int.toString depth, "-",
			       Int.toString partId ]
in
    OS.Path.joinDirFile {dir = path, file = file }
end

fun eventsFileName (path, prefix, depth, partId) =
    statesFileName (path, prefix, depth, partId) ^ "-EVTS"

fun init (path, prefix, partitions, cacheSize) = let
    fun look 2097152 = 2097152
      | look n = if n >= cacheSize div 4
		 then n
		 else look (n * 2)
    val slots = look 65536
in
    {
     path          = path,
     prefix        = prefix,
     partitions    = ref partitions,
     cacheSize     = cacheSize,
     slots         = slots,
     size          = ref 0,
     depth         = ref 0,
     slotsW        = Word.fromInt slots,
     cache         = Array.array (slots, []),
     toStore       = ref 0,
     maxPerSlot    = cacheSize div slots,
     cacheTooSmall = ref false
    }
end

fun incrDepth ({ depth, size, cacheTooSmall, ... }: queue) = (
    size := 0;
    depth := !depth + 1;
    cacheTooSmall := false)

fun writeAll' empty ({ path, prefix, partitions, slots, maxPerSlot, cache,
		       depth, toStore, size, cacheTooSmall, ... }: queue) = let
    fun writeOne empty partId = let
	val sf = statesFileName (path, prefix, !depth, partId)
	val ef = eventsFileName (path, prefix, !depth, partId)
	val sf = Stream.openAppend sf
	val ef = Stream.openAppend ef
	fun writeSlot slotId = let
	    fun writeState (stored as (ref false), s, evts) = (
		Stream.writeOne (sf, s);
		Stream.writeOne (ef, evts);
		stored := true)
	      | writeState _ = ()
	    val l = Array.sub (cache, slotId)
	in
	    List.app writeState l;
	    if empty
	    then Array.update (cache, slotId, [])
	    else if List.length l <= maxPerSlot
	    then ()
	    else Array.update (cache, slotId, List.take (l, maxPerSlot))
	end
	fun loopSlot id = if id > (slots - 1)
			  then ()
			  else (writeSlot id;
				loopSlot (id + (!partitions)))
    in
	loopSlot partId;
	BinIO.closeOut sf;
	BinIO.closeOut ef
    end
    fun loopPart id = if id > (!partitions - 1)
		      then ()
		      else (writeOne empty id;
			    loopPart (id + 1))
in
    loopPart 0;
    size := (!size) + (!toStore);
    toStore := 0
end

val writeAll = writeAll' false

val writeAndEmpty = writeAll' true

fun enqueue (queue as {
	     slots, maxPerSlot, cache, slotsW, cacheSize,
	     toStore, cacheTooSmall, ... }: queue,
	     (s, evts)) = let
    val slotId = Word.toInt (Word.mod (hashVector s, slotsW))
    val slot   = Array.sub (cache, slotId)
in
    if isSome (List.find (fn (_, s', _) => s = s') slot)
    then ()
    else let fun replaceFirst [] =
		 ([], false)
	       | replaceFirst ((ref true, _, _) :: l) =
		 ((ref false, s, evts) :: l, true)
	       | replaceFirst (item :: l) = let
		     val (l', found) = replaceFirst l
		 in
		     (item :: l', found)
		 end
	     val (newSlot, found) = replaceFirst slot
	     val newSlot = if found
			   then (cacheTooSmall := true; newSlot)
			   else (ref false, s, evts) :: slot
	 in
	     toStore := !toStore + 1;
	     Array.update (cache, slotId, newSlot);
	     if !toStore <= cacheSize
	     then ()
	     else writeAll queue
	 end
end

fun cacheTooSmall ({ cacheTooSmall, ... }: queue) = !cacheTooSmall

fun size (queue as { size, ... }: queue) = !size

fun foldPart f value ({path, prefix, ...}: queue, depth, partId) = let
    val sf = statesFileName (path, prefix, depth, partId)
    val ef = eventsFileName (path, prefix, depth, partId)
in
    StreamPair.fold' f value (sf, ef, NONE)
end

fun appPart f (queue, depth, partId) =
    foldPart (fn (s, _) => f s) () (queue, depth, partId)

fun appPartStates f ({path, prefix, ...}: queue, depth, partId) =
    Stream.app' f (statesFileName (path, prefix, depth, partId), NONE)

fun deleteFile ({path, prefix, ...}: queue, depth, partId) = (
    OS.FileSys.remove (statesFileName (path, prefix, depth, partId))
    handle _ => ();
    OS.FileSys.remove (eventsFileName (path, prefix, depth, partId))
    handle _ => ())


fun reorganizePartitions (queue as {
			  path, prefix, partitions, depth, ... }: queue) = let
    val tmpPrefix = prefix ^ "-TMP"
    val newPartitionsW = Word.fromInt (!partitions * 2)
    val allPartitions = List.tabulate (!partitions, fn i => i)
    val allNewPartitions = List.tabulate (!partitions * 2, fn i => i)
    val mv = OS.FileSys.rename
    fun mapAll partId = let
	val sf  = statesFileName (path, prefix, !depth, partId)
	val ef  = eventsFileName (path, prefix, !depth, partId)
	val p1  = partId
	val p2  = !partitions + partId
	val sf1 = statesFileName (path, prefix ^ "-TMP", !depth, p1)
	val ef1 = eventsFileName (path, prefix ^ "-TMP", !depth, p1)
	val sf1 = Stream.openAppend sf1
	val ef1 = Stream.openAppend ef1
	val sf2 = statesFileName (path, prefix ^ "-TMP", !depth, p2)
	val ef2 = eventsFileName (path, prefix ^ "-TMP", !depth, p2)
	val sf2 = Stream.openAppend sf2
	val ef2 = Stream.openAppend ef2
	fun mapOne (s, evts) = let
	    val h = hashVector s
	    val p = Word.toInt (Word.mod (h, newPartitionsW))
	in
	    if p = p1
	    then (Stream.writeOne (sf1, s);
		  Stream.writeOne (ef1, evts))
	    else if p = p2
	    then (Stream.writeOne (sf2, s);
		  Stream.writeOne (ef2, evts))
	    else raise LibBase.Impossible "reorganizePartitions"
	end
    in
	StreamPair.fold' (fn (s, _) => mapOne s) () (sf, ef, NONE);
	BinIO.closeOut sf1;
	BinIO.closeOut ef1;
	BinIO.closeOut sf2;
	BinIO.closeOut ef2
    end
in
    writeAndEmpty queue;
    List.app mapAll allPartitions;
    List.app (fn i => (
		 mv { new = statesFileName (path, prefix, !depth, i),
		      old = statesFileName (path, tmpPrefix, !depth, i) };
		 mv { new = eventsFileName (path, prefix, !depth, i),
		      old = eventsFileName (path, tmpPrefix, !depth, i) }))
	     allNewPartitions;
    partitions := !partitions * 2
end

end



(*****
 *  exploration procedure
 *****)
fun explore transformEvents
	    transformState
	    {
	     a_initial,
	     s_initial,
	     t_initial,
	     arc_hook,
	     state_hook,
	     pre_trace_hook,
	     post_trace_hook
	    }
	    initStorage
	    initStates = let
    
    val _ = print "Warning: error-traces not supported\n"
    (*  to turn off partial duplicate detection  *)
    val previousLayers = (
	if previousLayers = 0
	then ()
	else print "Warning: partial duplicate detection disabled\n";
	0)
						    
    val visited    = initStorage
    val candidates = Storage.initCandidateSet visited
    val partitions = ref (Storage.numPartitions visited)
    val path       = Storage.getPath visited
    val queue      = Queue.init (path, "QUEUE", !partitions, cacheSize)
    val part       = ExtPartition.init (0, NONE)
    val reorganize = ref false

    (*************************************************************************)
    (*  for statistics and to decide when to perform duplicate detection     *)
    val visitedStates     = ref 0
    val visitedArcs       = ref 0
    val depth             = ref 0
    val depthLastDD       = ref (~ 1)
    val detections        : (int list) ref = ref []
    val lengthsSelected   : (int list) ref = ref []
    val doDetection       = ref false
    val backTrans         = ref (Array.array (1, 0.0))
    val noDDCost          = ref 0
    val approx            = ref (Array.array (2, 1.0))
    val acc               = ref (Array.array (2, 1.0))
    val duplicatesApprox  = ref 0

    fun layerSize l = Storage.layerSize (candidates, l) +
		      Storage.layerSize (visited, l)
    fun incrDepth () = let
	fun expandArray a value = let
	    val lg = Array.length (!a)
	    val a' = Array.tabulate (lg + 1, (fn i => if i = lg
						      then value
						      else Array.sub (!a, i)))
	in a := a' end
    in
	depth := !depth + 1;
	expandArray approx 0.0;
	expandArray acc 0.0;
	expandArray backTrans 0.0
    end
    fun incrBackTrans (d, i) = Array.update (!backTrans, d,
					     Array.sub (!backTrans, d) + i)
    fun isSelected l = let
	fun isSelected' [] = false
	  | isSelected' (l' :: list) = l = l' orelse isSelected' list
    in isSelected' (!lengthsSelected) end
    fun acceleration i = if i = (~ 1) then 1.0 else Array.sub (!acc, i)
    fun next acc = let
	val result = if acc > 1.0
		     then acc / Math.pow (acc + 0.001, powSup)
		     else acc * Math.pow (acc - 0.001, powInf);
	val result = if result < 0.01 then 0.01 else result
    in result end
    (*************************************************************************)

    (*****
     *  decide if duplicate detection should be performed or delayed
     *****)
    fun decideDD () =
	(not dynamic) orelse
	let
	    fun DD1Cost () = readCost
	    fun selectedSize () =
		List.foldl (fn (l, sum) => sum + layerSize (!depth - (l + 1)))
			   0 (!lengthsSelected)
	    fun degree () = if (!visitedStates) = 0
			    then 1.0
			    else (fromInt (!visitedArcs)) /
				 (fromInt (!visitedStates))
	    fun duplicateCost () =
		readCost + expansionCost + writeCost * (2.0 + degree ())
	    fun multiplicity () =
		if not (Queue.cacheTooSmall queue)
		then 1.0
		else let val read = ref 0
			 val size = ref 0
			 val merged = ref 0
			 val part = ExtPartition.init 0
			 fun merge partId = let
			     fun insert s = (
				 read := !read + 1;
				 ExtPartition.insert (fn _ => ()) (part, s, 0))
			 in
			     ExtPartition.empty part;
			     Queue.appPartStates
				 insert (queue, !depth, partId);
			     if ExtPartition.numItems part = 0
			     then ()
			     else (size   := !size +
					     (ExtPartition.numItems part);
				   merged := !merged + 1);
			     !merged =
			     Int.min (1, Int.max (1, !partitions div 100))
			 end
			 fun loopPart id = if id > (!partitions - 1)
					   then ()
					   else if merge id
					   then ()
					   else loopPart (id + 1)
		     in
			 loopPart 0;
			 (fromInt (!read)) / (fromInt (!size))
		     end
	    val expected   = toInt (Array.sub (!approx, !depth))
	    val reach      = toInt (fromInt (Queue.size queue) /
				    multiplicity ())
	    val duplicates = reach - expected
	    val err        = Math.pow (1.0 + errorRate,
				       fromInt (!depth) -
				       fromInt (!depthLastDD + 1))
	    val duplicates = toInt (fromInt duplicates * err)
	    val DDCost     = toInt (readCost *
				    (fromInt (Storage.numItems visited) +
				     fromInt (Storage.numItems candidates) -
				     fromInt (selectedSize ())))
	in
	    noDDCost := !noDDCost + (toInt (duplicateCost ()) * duplicates);
	    duplicatesApprox := !duplicatesApprox + duplicates;
	    (!noDDCost >= DDCost)
	end
    
    (***** 
     *  expand a state and enqueue its successors
     *****)
    fun handleState ((s, evts), (sVal, aVal)) = let
	val s    = StateEncoder.unmap s
	val evts = EventsEncoder.unmap evts
	val sVal = state_hook ((s, evts), t_initial, sVal)
        fun handleEvent (ev, (sVal, aVal)) = let
	    fun handleSucc ((s', evts'), (sVal, aVal)) = let
		val (s', evts') = transformState (s', evts')
		val evts' = transformEvents (s', evts')
		val aVal = arc_hook (((s, evts), ev, (s', evts')),
				     t_initial, aVal)
	    in
		visitedArcs := !visitedArcs + 1;
		Queue.enqueue (queue, (StateEncoder.map s',
				       EventsEncoder.map evts'));
		(sVal, aVal)
	    end
	in
	    List.foldl handleSucc (sVal, aVal) (Model.nextStates (s, ev))
        end
    in
	visitedStates := !visitedStates + 1;
	List.foldl handleEvent (sVal, aVal) evts
    end

    (***** 
     *  process a partititon. duplicate detection is perfomed
     *****)
    fun handlePartitionDetection partId = let
	
	(*  load candidate states and delete the candidate file  *)
	val d = ref (~ 1)
	fun onDuplicate (d', _) = incrBackTrans ((!d) - (d' + 1), 1.0)
	fun insert (s, sd) = (
	    d := sd;
	    ExtPartition.insert onDuplicate (part, s, (!d, NONE)))
	val _ = (Storage.appPartLayer insert (candidates, partId);
		 Storage.empty (candidates, partId))

	(*  load states of the queue and delete the queue file  *)
	fun onDuplicate (d', _) = let
	    val lg = (!depth) - (d' + 1)
	in
	    if lg >= 0
	    then incrBackTrans (lg, 1.0)
	    else ()
	end
	fun insert (s, evts) =
	    ExtPartition.insert onDuplicate (part, s, (!depth, SOME evts))
	val _ = (Queue.appPart insert (queue, !depth, partId);
		 Queue.deleteFile (queue, !depth, partId);
		 reorganize := (!reorganize orelse
				ExtPartition.numItems part > maxPartSize))

	(*  delete the states of the visited file from the partition  *)
	val d = ref (~ 1)
	fun onDuplicate (d', _) = incrBackTrans (d' - (!d + 1), 10.0)
	fun delete (s, sd) = (
	    d := sd;
	    ExtPartition.delete onDuplicate (part, s))
	val _ = Storage.appPartLayer delete (visited, partId)

	(*  append remaining states (depth by depth) to the visited file  *)
	val d = ref (~ 1)	
	fun append (st, (stDepth, _)) =
	    if stDepth <> !d
	    then ()
	    else Storage.storeState (visited, st)
	fun loop cur = if cur > (!depth)
		       then ()
		       else (d := cur;
			     Storage.openAppend (visited, partId);
			     Storage.startLayer (visited, cur);
			     ExtPartition.app append part;
			     Storage.close visited;
			     loop (cur + 1))
	val _ = loop (!depthLastDD + 1)
    in
	()
    end


    (*****
     *  process a partititon. duplicate detection is not perfomed
     *****)
    fun handlePartitionNoDetection partId = let

	(*  load states in the queue and delete the queue file  *)
	fun insert (s, evts) =
	    ExtPartition.insert (fn _ => ()) (part, s, (!depth, SOME evts))
	val _ = (
	    Queue.appPart insert (queue, !depth, partId);		 
	    Queue.deleteFile (queue, !depth, partId);
	    reorganize := (!reorganize orelse
			   ExtPartition.numItems part > maxPartSize))
		
	(*  look at selected depths  *)
	fun delete l = let
	    val d = (!depth) - (l + 1)
	    fun onDuplicate _ =
		incrBackTrans ((!depth) - (d + 1),
			       if !depth = !depthLastDD then 10.0 else 1.0)
	    val storage = if d <= (!depthLastDD)
			  then ref visited
			  else ref candidates
	    fun delete s = ExtPartition.delete onDuplicate (part, s)
	in
	    Storage.openRead (!storage, partId);
	    Storage.appLayer delete (!storage, d);
	    Storage.close (!storage)
	end
	val _ = List.app delete (!lengthsSelected)

	(*  append remaining states to the candidate file  *)
	fun append (st, d) = Storage.storeState (candidates, st)
	val _ = (
	    Storage.openAppend (candidates, partId);
	    Storage.startLayer (candidates, !depth);
	    ExtPartition.app append part;
	    Storage.close candidates)
    in
	()
    end

    (*****
     *  process all partitions one by one using procedure proc
     *****)
    fun handleLayer (proc, (sVal, aVal)) = let
	fun toString n f i = StringCvt.padLeft #" " n (f i)
	val longToString  = toString 10 Int.toString
	val smallToString = toString 5  Int.toString
	val _ = print (String.concat [
		       "Depth = ",
		       smallToString (!depth), "     ",
		       "Visited = ",
			longToString (Storage.numItems visited), "     ",
		       "Candidates = ",
		       longToString (Storage.numItems candidates), "\n" ])
	fun handlePartition (partId, (cont, sVal, aVal)) = (
	    ExtPartition.empty part;
	    proc partId;
	    let
		(*  expand the states remaining in the partition  *)
		fun expand ((_, (_, NONE)), value) = value
		  | expand ((s, (d, SOME evts)), (cont, sVal, aVal)) = let
			val (sVal, aVal) =
			    handleState ((s, evts), (sVal, aVal))
		    in
			(true, sVal, aVal)
		    end
	    in
		ExtPartition.fold expand (cont, sVal, aVal) part
	    end)
	val _ = (
	    Storage.newLayer visited;
	    Storage.newLayer candidates)
	val result = List.foldl handlePartition
				(false, sVal, aVal)
				(List.tabulate (!partitions, fn i => i))
	fun approximate () = (
	    if not (!doDetection)
	    then ()
	    else Array.modifyi (fn (i, a) => if i > (!depth - 1)
					     then a
					     else fromInt (layerSize (i + 1)) /
						  fromInt (layerSize (i)))
			       (!acc);
	    let
		fun averagePrevious () = let
		    val N = prevAcc
		    val i = ref 1
		    val res = ref 0.0
		    val d = ref 0
		in
		    while (!depth) - (!i) >= ~ 1 andalso (!i <= N) do
			(res := !res + acceleration (!depth - (!i)) *
				       fromInt (N - (!i) + 1);
			 d   := !d + N - (!i) + 1;
			 i   := !i + 1);
		    (!res) / (fromInt (!d))
		end
		val newAcc = next (averagePrevious ())
		val prev   = if !doDetection
			     then fromInt (layerSize (!depth))
			     else Array.sub (!approx, !depth)
		val a      = Real.* (newAcc, prev)
		val queue  = fromInt (Queue.size queue)
		val a      = if a > queue then queue else a
	    in
		Array.update (!acc, !depth, newAcc);
		Array.update (!approx, !depth + 1, a)
	    end)
    in
	Queue.writeAndEmpty queue;
	approximate ();
	if not (!doDetection)
	then ()
	else (noDDCost := 0;
	      depthLastDD := !depth;
	      detections  := !depth :: (!detections));
	if not (!reorganize)
	then ()
	else let val allParts = List.tabulate (!partitions, fn i => i)
		 fun hash s =
		     Word.toInt (Word.mod (hashVector s,
					   Word.fromInt (!partitions * 2)))
		 fun reorganizePart set partId = (
		     Storage.reorganizePart
			 hash (set, partId, [partId, partId + (!partitions)]))
	     in
		 Queue.reorganizePartitions queue;
		 List.app (reorganizePart visited) allParts;
		 List.app (reorganizePart candidates) allParts;
		 partitions := !partitions * 2;
		 reorganize := false
	     end;
	result
    end

    fun main () = let 
	fun handleInitState (s, evts) = let
	    val (s, evts) = transformState (s, evts)
	    val evts = transformEvents (s, evts)
	in
	    Queue.enqueue (queue, (StateEncoder.map s,
				   EventsEncoder.map evts))
	end
	fun loopLayer (sVal, aVal) = (
	    doDetection := decideDD ();
	    Queue.incrDepth queue;
	    let
		val proc = if !doDetection
			   then handlePartitionDetection
			   else handlePartitionNoDetection
		val (cont, sVal, aVal) = handleLayer (proc, (sVal, aVal))

		(*  select lengths for partial duplicate detection  *)
		fun selectBackTransLengths () = let
		    val selected = ref []
		    val total = ref 0.0
		    fun insert (l, num) = let
			fun walk (l, num) 0 list = list
			  | walk (l, num) n []   = [(l, num)]
			  | walk (l, num) n ((l', num') :: list) =
			    if num > num'
			    then (l, num) :: (walk (l', num') (n - 1) list)
			    else (l', num') :: (walk (l, num) (n - 1) list)
		    in
			selected := walk (l, num) previousLayers (!selected)
		    end
		    fun handleLength l = let
			val num = Array.sub (!backTrans, l)
		    in
			total := Real.+ (!total, num);
			if Real.== (num, 0.0)
			then ()
			else insert (l, num)
		    end
		    fun handleLengths l =
			if l = (!depth)
			then ()
			else (handleLength l;
			      handleLengths (l + 1))
		    fun cut [] sum = []
		      | cut ((l , num) :: list) sum =
			if Real.> (num + sum, Real.* (0.95, !total))
			then [(l , num)]
			else (l , num) :: (cut list (num + sum))
		in
		    handleLengths 0;
		    selected := cut (!selected) 0.0;
		    lengthsSelected := ListMergeSort.sort
					   (fn (l, l') => l' > l)
					   (List.map (fn (l, _) => l)
						     (!selected))
		end
	    in
		if cont
		then (if not (!doDetection)
		      then ()
		      else selectBackTransLengths ();
		      incrDepth ();
		      loopLayer (sVal, aVal))
		else
		    if Storage.numItems candidates = 0
		    then (sVal, aVal)
		    else (doDetection := true;
			  incrDepth ();
			  Queue.incrDepth queue;
			  let
			      val (_, sVal, aVal) =
				  handleLayer
				      (handlePartitionDetection, (sVal, aVal))
			  in
			      (sVal, aVal)
			  end)
	    end)
    in
	List.app handleInitState initStates;
	Queue.writeAndEmpty queue;
	loopLayer (s_initial, a_initial)
    end

    val (sVal, aVal) = main ()
    val layers = Array.tabulate
		     (!depth + 1, fn i => Storage.layerSize (visited, i))
in
    (visited, sVal, aVal,
     (layers, !approx, acc, detections, !visitedStates))
end



(*****
 *  report creation
 *****)
fun makeReport initStorage initStates = let
    val (visited, _, _, (layers, approx, acc, detections, expanded)) =
	explore
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
	    initStates
    fun oneSlice ((lo, hi), sum) = let
	val all      = List.tabulate (hi - lo + 1, (fn i => lo + i))
	fun estimI i = (Real.toInt IEEEReal.TO_NEAREST
				   (Array.sub (approx, i)))
	val estim    = List.foldl (fn (i, sum) => sum + estimI i) 0 all
	fun realI i  = Array.sub (layers, i)
	val real     = List.foldl (fn (i, sum) => sum + realI i) 0 all
	val abs      = real - estim
	val abs      = if abs >= 0 then abs else ~ abs
    in
	sum + abs
    end
    val n = (detections := List.rev (!detections); List.length (!detections))
    val l = List.tabulate
		(n, (fn i => if i = 0
			     then 0
			     else (List.nth (!detections, i - 1)) + 1))
    val slices = ListPair.zip (l, !detections)
    val error = (fromInt (List.foldl oneSlice 0 slices)) /
		(fromInt (Storage.numItems visited))
in
    (visited,
     {
      errorRate    = error,
      detections   = !detections,
      layers       = Array.foldr (fn (s, l) => if s > 0 then s :: l else l)
				 [] layers,
      layersApprox = Array.foldr
			 (fn (s, l) =>
			     (if Real.isNan s then 0 else toInt s) :: l) []
			 approx,
      expanded     = expanded
     })
end



fun explore' eventsTransform stateTransformer
	     hooks initStorage initStates = let
    val (storage, sVal, aVal, _) =
	explore eventsTransform stateTransformer
		hooks initStorage initStates
in
    (storage, sVal, aVal)
end
val explore = explore'

end
