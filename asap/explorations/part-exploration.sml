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
 *     part-exploration.sml
 *
 *  Created:
 *     Jan. 10, 2008
 *
 *  Description:
 *     Provides the PART external algorithm from:
 *    "Time-Efficient Model Checking with Magnetic Disk. In TACAS'2005.
 *    Tonglaga Bao and Michael Jones. LNCS # 3440.
 *
 *  To do:
 *  - trace is not handled.  how to do that?
 *  
 *)


signature PART_EXPLORATION = sig
    
    include TRACE_EXPLORATION

    type report = { ioQueue   : LargeInt.int,
		    ioVisited : LargeInt.int,
		    crossTrans: int,
		    partSwap  : int,
		    expanded  : int,
		    events    : int,
		    reorgTime : real }

    val makeReport: 'a storage -> (state * event list) list ->
		    'a storage * report

end


functor PartExploration (
structure Model        : MODEL
structure StateEncoder : SERIALIZER where type src = Model.state
structure EventsEncoder: SERIALIZER where type src = Model.event list
structure Storage      : PART_EXT_STORAGE where type item = Model.state
structure PartFunction : PART_FUNCTION where type state = Model.state
                                         and type event = Model.event
val queueBufferSize    : int
val maxPartSize        : int option
val dynamicDelay       : bool
val prop               : (int * int) option
) : PART_EXPLORATION = struct

val maxPartSize = case maxPartSize of NONE => 0 | SOME n => n

val dynamicPartitioning = PartFunction.dynamic

val prop = if dynamicDelay
	   then valOf prop
	   else (0, 0)

val (propM, propD) = prop

type id = Storage.id
type state = Model.state
type event = Model.event
type 'a storage = 'a Storage.storage

type report = { ioQueue   : LargeInt.int,
		ioVisited : LargeInt.int,
		crossTrans: int,
		partSwap  : int,
		expanded  : int,
		events    : int,
		reorgTime : real }


(*****
 *  partitioned queue used by the algorithm
 *****)
structure Queue: sig

    type bit_vector = Word8Vector.vector

    type part_queue

    type part_id = int

    val init:
	string * string * int * int
	-> part_queue

    val isEmpty:
	part_queue * part_id
	-> bool

    val numItems:
	part_queue
	-> int

    val partNumItems:
	part_queue * part_id
	-> int

    val readAccesses:
	part_queue
	-> LargeInt.int

    val writeAccesses:
	part_queue
	-> LargeInt.int

    val check:
	part_queue
	-> unit
    
    val selectLongestQueue:
	part_queue
	-> int option

    val enqueue:
	part_queue * (bit_vector * bit_vector * bool) * part_id
	-> unit

    val fold:
	((bit_vector * bit_vector * bool) * 'a -> bool * 'a)
	-> 'a
	-> part_queue * part_id
	-> 'a

    val reorganizePart:
	(Word8Vector.vector -> part_id)
	-> part_queue * part_id * part_id list
	-> unit

end = struct

structure NullEncoder : REVERSIBLE_MAPPING = struct
type src = Word8Vector.vector
type dest = Word8Vector.vector
fun map str = str
val unmap = map
end

structure Stream = Stream(structure Encoder = NullEncoder)

exception Check

type bit_vector = Word8Vector.vector

type part_id = int

type part_queue = {
     path      : string,
     prefix    : string,
     space     : int ref,
     parts     : int ref,
     bufferSize: int,
     queues    : ((bit_vector * bit_vector * bool) list * int * int)
		     Array.array ref,
     readAcc   : LargeInt.int ref,
     writeAcc  : LargeInt.int ref
}

fun merge (s, evts, cross) = let
    val lg = Word8Vector.length s
    val pref = Word8Vector.tabulate (3, fn 0 => if cross
						then 0w1
						else 0w0
					 | 1 => Word8.fromInt (lg div 256)
					 | 2 => Word8.fromInt (lg mod 256)
					 | _ => raise LibBase.Impossible "")
in
    Word8Vector.concat [ pref, s, evts ]
end

fun split v = let
    val lg = Word8Vector.length v
    val cross = case Word8Vector.sub (v, 0)
		 of 0w1 => true
		  | 0w0 => false
		  | _ => raise LibBase.Impossible ""
    val sLg = (Word8.toInt (Word8Vector.sub (v, 1)) * 256) +
	      (Word8.toInt (Word8Vector.sub (v, 2)))
    val s = Word8Vector.tabulate
		(sLg, fn i => Word8Vector.sub (v, i + 3))
    val evts = Word8Vector.tabulate
		   (lg - sLg - 3, fn i => Word8Vector.sub (v, i + 3 + sLg))
in
    (s, evts, cross)
end
		   
fun fileName (path, prefix, partId) =
    OS.Path.joinDirFile { dir  = path,
			  file = prefix ^ "-" ^ Int.toString partId }
     
fun memQueueSize (partitions, bufferSize) = bufferSize div partitions

val emptyQueue = ([], 0, 0)

fun init (path, prefix, initParts, bufferSize) = {
    path       = path,
    prefix     = prefix,
    parts      = ref initParts,
    space      = ref bufferSize,
    bufferSize = bufferSize,
    queues     = ref (Array.array (initParts, emptyQueue)),
    readAcc    = ref (LargeInt.fromInt 0),
    writeAcc   = ref (LargeInt.fromInt 0)
}

fun isEmpty ({ queues, ... }: part_queue, partId) =
    case Array.sub (!queues, partId) of ([], 0, 0) => true | _ => false

fun numItems ({ queues, ... }: part_queue) =
    Array.foldl (fn ((_, ms, ds), s) => s + ms + ds) 0 (!queues)

fun partNumItems ({ queues, ... }: part_queue, partId) =
    case Array.sub (!queues, partId) of (_, ms, ds) => ms + ds

fun readAccesses ({ readAcc, ... }: part_queue) = !readAcc

fun writeAccesses ({ writeAcc, ... }: part_queue) = !writeAcc
    
fun selectLongestQueue ({ parts, queues, ... }: part_queue) = let
    fun loop i = if i >= !parts
		 then (0, 0)
		 else let val (_, ms, ds) = Array.sub (!queues, i)
			  val size = ms + ds
			  val (iMax, max) = loop (i + 1)
		      in
			  if size > max
			  then (i, size)
			  else (iMax, max)
		      end
    val (iMax, max) = loop 0
in
    if max > 0
    then SOME iMax
    else NONE
end

fun enqueue ({ path, prefix, parts, bufferSize,
	       queues, writeAcc, space, ... }: part_queue,
	     (s, evts, cross), partId) = let
    fun unload (partId, []) = ()
      | unload (partId, q) = let
	    val diskQueue = Stream.openAppend (fileName (path, prefix, partId))
	in
	    List.app (fn s => Stream.writeOne (diskQueue, merge s)) q;
	    BinIO.closeOut diskQueue
	end
    val (q, ms, ds) = Array.sub (!queues, partId)
in
    if !space > 0
    then Array.update (!queues, partId, ((s, evts, cross) :: q, ms + 1, ds))
    else (space := bufferSize;
	  writeAcc := LargeInt.+ (!writeAcc,
				  LargeInt.fromInt bufferSize);
	  Array.appi (fn (i, (q, _, _)) => unload (i, q)) (!queues);
	  Array.modifyi (fn (i, (_, ms, ds)) =>
			    if i <> partId
			    then ([], 0, ms + ds)
			    else ([ (s, evts, cross) ], 1, ms + ds))
			(!queues));
    space := !space - 1
end
			    
fun fold f value (queue as { path, prefix, readAcc, queues, ... }: part_queue,
		  partId) = let
    fun listFold ([], value) = ([], value)
      | listFold (item :: l, value) = let
	    val (continue, value) = f (item, value)
	in
	    if continue
	    then listFold (l, value)
	    else (l, value)
	end
in
    case Array.sub (!queues, partId)
     of ([], 0, 0) => value
      | (_ :: _, 0, _) => raise LibBase.Impossible ""
      | ([], 0, ds) => let
	    val old = fileName (path, prefix, partId)
	    val new = old ^ "-TMP"
	    val stream = (OS.FileSys.rename { new = new, old = old };
			  Stream.openIn new)
	    val _ = (Array.update (!queues, partId, emptyQueue);
		     readAcc := LargeInt.+ (!readAcc, LargeInt.fromInt ds))
	    fun loop (continue, value) =
		case Stream.readOne stream
		 of NONE => (continue, value)
		  | SOME s =>
		    if continue
		    then loop (f (split s, value))
		    else (enqueue (queue, split s, partId);
			  loop (continue, value))
	    val (continue, value) = loop (true, value)
	in
	    BinIO.closeIn stream;
	    OS.FileSys.remove new;
	    if continue
	    then fold f value (queue, partId)
	    else value
	end
      | (q, _, ds) => (
	Array.update (!queues, partId, ([], 0, ds));
	case listFold (q, value) of
	    ([], value) => fold f value (queue, partId)
	  | (q, value) => let
		val (q', ms, ds) = Array.sub (!queues, partId)
	    in
		Array.update (!queues, partId,
			      (q @ q', List.length q + ms, ds));
		value
	    end)
end

fun check ({ queues, ... }: part_queue) = let
    fun checkOne (i, (q, _, _)) =
	List.app (fn (s, _, _) => if PartFunction.part' s <> i
				  then raise Check
				  else ()) q
in
    Array.appi checkOne (!queues)
end

fun reorganizePart hash
		   (queue as { path, readAcc, prefix,
			       parts, queues, space, ... }: part_queue,
		    partId, newPartIds) = let
    fun reenqueue (item as (s, _, _))= enqueue (queue, item, hash s)
    val (l, ms, ds)  = Array.sub (!queues, partId)
    val newParts = !parts + (List.length newPartIds) - 1
    val newQueues = Array.tabulate (newParts,
				 fn i => if i <> partId andalso i < (!parts)
					 then Array.sub (!queues, i)
					 else emptyQueue)
    val old = fileName (path, prefix, partId)
    val tmp = old ^ "-TMP"
in
    space := !space + ms;
    parts := newParts;
    queues := newQueues;
    readAcc := LargeInt.+ (!readAcc, LargeInt.fromInt ds);
    (OS.FileSys.rename { new = tmp, old = old }) handle _ => ();
    Stream.fold' (fn (s, _) => reenqueue (split s)) () (tmp, NONE);
    List.app reenqueue l;
    (OS.FileSys.remove tmp) handle _ => ()
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

    val V          = initStorage
    val C          = Storage.initCandidateSet V
    val initParts  = (PartFunction.init ();
		      PartFunction.numPartitions ())
    val _          = (Storage.init (V, initParts);
		      Storage.init (C, initParts))
    val path       = Storage.getPath V
    val Q          = Queue.init (path, "QUEUE", initParts, queueBufferSize)
    val P          = ExtPartition.init true
    val swap       = ref 0
    val crossTrans = ref 0
    val expanded   = ref 0
    val events     = ref 0
    val reorgTime  = ref 0.0

    fun reorganize partId =
	dynamicPartitioning
	andalso PartFunction.canReorganize partId
	andalso	ExtPartition.numItems P >= maxPartSize

    fun unloadPart (storage, partId) = let
	fun store (_, s, true) = Storage.storeState (storage, s)
	  | store _ = ()
    in
	Storage.openAppend (storage, partId);
	ExtPartition.app store P;
	ExtPartition.empty P;
	Storage.close storage
    end

    fun cleanPart partId =
	ExtPartition.mapPartial
	    (fn (s, _) => if PartFunction.part' s = partId
			  then SOME (s, false)
			  else NONE)
	    P

    fun loadPart storages partId = let
	fun loadStorage (storage, writeAfter) = let
	    fun insert s = ignore (ExtPartition.insert (P, s, writeAfter))
	in
	    Storage.appPart insert (storage, partId)
	end
    in
	List.app loadStorage storages
    end

    fun handleState ((sV, evtsV, cross), (partId, (sVal, aVal))) = let
	val isIn = ref false
	fun handleEvent (s, evts) (ev, (sVal, aVal)) = let
	    fun handleSucc ((s', evts'), (sVal, aVal)) = let
		val (s', evts') = transformState (s', evts')
		val evts' = transformEvents (s', evts')
		val aVal = arc_hook (((s, evts), ev, (s', evts')),
				     t_initial, aVal)
		val (s', partId') = PartFunction.part s'
		val evts' = EventsEncoder.map evts'
		val cross = partId <> partId'
		val enqueue = cross
			      orelse (#1 (ExtPartition.insert (P, s', true)))
	    in
		if cross
		then crossTrans := !crossTrans + 1
		else ();
		if enqueue
		then Queue.enqueue (Q, (s', evts', cross), partId')
		else ();
		(sVal, aVal)
	    end
	in
	    PartFunction.eventHook ev;
	    List.foldl handleSucc (sVal, aVal) (Model.nextStates (s, ev))
	end
	val expand = (not cross)
		     orelse (#1 (ExtPartition.insert (P, sV, true)))
    in
	if not expand
	then (true, (partId, (sVal, aVal)))
	else let val se as (_, evts) =
		     (StateEncoder.unmap sV, EventsEncoder.unmap evtsV)
		 val sVal = state_hook (se, t_initial, sVal)
		 val result = List.foldl (handleEvent se) (sVal, aVal) evts
	     in
		 events := !events + (List.length evts);
		 expanded := !expanded + 1;
		 (not (reorganize partId), (partId, result))
	     end
    end

    fun printMsg () = let
	fun toString n f i = StringCvt.padLeft #" " n (f i)
	val longToString  = toString 11 Int.toString
	val l = if dynamicDelay
		then [ longToString (Storage.numItems V), " visited",
		       longToString (Storage.numItems C), " candidates",
		       longToString (Queue.numItems Q), " unprocessed\n" ]
		else [ longToString (Storage.numItems V), " visited",
		       longToString (Queue.numItems Q), " unprocessed\n" ]
    in
	print (String.concat l)
    end

    fun expandQueue delayedDetection storage (partId, data) = let
	val (_, data) = Queue.fold handleState (partId, data) (Q, partId)
    in
    	if not (reorganize partId)
	then (unloadPart (storage, partId);
	      data)
	else let val t = Timer.startRealTimer ()
	     in
		 if delayedDetection
		 then loadPart [ (V, false), (C, true) ] partId
		 else ();
		 Storage.empty (C, partId);
		 (case PartFunction.reorganize (P, partId)
		   of [] => (ExtPartition.empty P;
			     data)
		    | l => let
			  val kept = Storage.reorganizeMemPart
					 PartFunction.part' (V, P, partId, l)
		      in
			  Storage.reorganizePart
			      PartFunction.part' (C, partId, l);
			  Queue.reorganizePart
			      PartFunction.part' (Q, partId, l);
			  cleanPart kept;
			  printMsg ();
			  expandQueue false V (kept, data)
		      end)
		 before reorgTime := !reorgTime +
				     Time.toReal (Timer.checkRealTimer t)
	     end;
	data
    end

    fun loop data =
	case Queue.selectLongestQueue Q
	 of NONE => data
	  | SOME partId => let
		val qs = Queue.partNumItems (Q, partId)
		val cs = Storage.partNumItems (C, partId)
		val vs = Storage.partNumItems (V, partId)
		val delay = dynamicDelay
			    andalso qs + cs < (propM * vs) div propD
		val (storageRead, storageWritten) =
		    if delay
		    then ([ ], C)
		    else ([ (V, false), (C, true) ], V)
	    in
		printMsg ();
		loadPart storageRead partId;
		swap := !swap + 1;
		loop (expandQueue delay storageWritten (partId, data))
	    end

    fun handleInitState (s, evts) = let
	val (s, evts) = transformState (s, evts)
	val evts = transformEvents (s, evts)
	val (s, partId) = PartFunction.part s
	val evts = EventsEncoder.map evts
    in
	Queue.enqueue (Q, (s, evts, true), partId)
    end
    val (sVal, aVal) = (List.app handleInitState initStates;
			loop (s_initial, a_initial) before
			printMsg ())
    val ioVisited =
	List.foldl (fn (a, b) => LargeInt.+ (a, b))
		   (LargeInt.fromInt 0)
		   [ Storage.readAccesses C,
		     Storage.writeAccesses C,
		     Storage.readAccesses V,
		     Storage.writeAccesses V ]
    val report = {
	ioQueue    = LargeInt.+ (Queue.readAccesses Q, Queue.writeAccesses Q),
	ioVisited  = ioVisited,
	crossTrans = !crossTrans,
	expanded   = !expanded,
	events     = !events,
	partSwap   = !swap,
	reorgTime  = !reorgTime
    }
in
    (V, sVal, aVal, report)
end


fun makeReport initStorage initStates = let
    val (V, _, _, report) =
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
in
    (V, report)
end


local
    fun exp eventsTransform stateTransformer hooks initStorage initStates = let
	val (storage, sVal, aVal, _) =
	    explore eventsTransform stateTransformer
		    hooks initStorage initStates
    in
	(storage, sVal, aVal)
    end
in

val explore = exp

end
	  
end
