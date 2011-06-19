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
 *     external-storage.sml
 *
 *  Created:
 *     Jan. 10, 2008
 *
 *  Description:
 *     External storages.  Storage may be partitioned onto multiple files using
 *  some hash function and organized in layers, e.g., BFS layers.
 *
 *  To do:
 *  - most operations of signature STORAGE are not implemented
 *
 *)

signature EXTERNAL_PARTITION = sig

exception NotFound

eqtype id

type item = Word8Vector.vector
     
type 'a part

val nullId:
    id

val init:
    'a
    -> 'a part

val empty:
    'a part
    -> unit
				   
val isEmpty:
    'a part
    -> bool

val numItems:
    'a part
    -> int

val insert:
    'a part * item * 'a
    -> bool * id * 'a
								  
val delete:
    'a part * item
    -> (id * 'a) option
								  
val deleteId:
    'a part * id
    -> (item * 'a) option
							 
val app:
    (id * item * 'a -> unit)
    -> 'a part
    -> unit
						     
val fold:
    ((id * item * 'a) * 'b -> 'b)
    -> 'b
    -> 'a part
    -> 'b
								 
val member:
    'a part * item
    -> bool

val getItem:
    'a part * id
    -> (item * 'a) option

end



structure ExternalPartition: EXTERNAL_PARTITION = struct

exception NotFound

type item = Word8Vector.vector

type id = word * int

type key = Word8Vector.vector

type 'a part = (int ref * int ref * (id * key * 'a) list array)

val slots  = 100807
val slotsW = Word.fromInt slots
val nullId = (0w0, ~1)

fun init _ = (ref 0, ref 0, Array.array (slots, []))

fun isEmpty (items, _, _) = !items <> 0

fun numItems (items, _, _) = !items

fun getSlot (v, part) = let
    val h = HashWord8Vector.hash v
    val slotId = Word.toInt (h mod slotsW)
    val slot = Array.sub (part, slotId)
in
    (h, slotId, slot)
end

fun empty (items, next, part) = (
    items := 0;
    next := 0;
    Array.modify (fn _ => []) part)

fun getItem ((_, _, part), (h, num)) = let
    val slotId = Word.toInt (h mod slotsW)
    val slot = Array.sub (part, slotId)
in
    case List.find (fn ((h', num'), _, _) => h = h' andalso num = num') slot
     of NONE => NONE
      | SOME (_, v, tag) => SOME (v, tag)
end    

fun insert ((items, next, part), v, tag) = let
    val (h, slotId, slot) = getSlot (v, part)
in
    case List.find (fn (_, v', _) => v = v') slot
     of SOME (id, _, tag) => (false, id, tag)
      | NONE => let val id = (h, !next)
		in
		    items := !items + 1;
		    next := !next + 1;
		    Array.update (part, slotId, (id, v, tag) :: slot);
		    (true, id, tag)
		end
end

fun delete ((items, _, part), v) = let
    val (_, slotId, slot) = getSlot (v, part)
    val info = ref NONE
    val slot = List.filter (fn item as (id', v', tag') =>
			       if v <> v'
			       then true
			       else (info := SOME (id', tag');
				     false))
			   slot
in
    case !info of NONE => () | _ => items := !items - 1;
    Array.update (part, slotId, slot);
    !info
end

fun deleteId ((items, _, part), id as (h, num)) = let
    val slotId = Word.toInt (h mod slotsW)
    val slot = Array.sub (part, slotId)
    val info = ref NONE
    val slot = List.filter (fn item as (id', v', tag') =>
			       if id <> id'
			       then true
			       else (info := SOME (v', tag');
				     false))
			   slot
in
    case !info of NONE => () | SOME _ => items := !items - 1;
    Array.update (part, slotId, slot);
    !info
end

fun app f (_, _, part) =
    Array.app (fn slot => List.app f slot) part

fun fold f value (_, _, part) =
    Array.foldl (fn (slot, value) => List.foldl f value slot) value part
    
fun member ((_, _, part), v) =
    List.exists (fn (_, v', _) => v = v') (#3 (getSlot (v, part)))

end



signature EXTERNAL_STORAGE = sig

include STORAGE

val diskUsage:
    'a storage
    -> LargeInt.int

val readAccesses:
    'a storage
    -> LargeInt.int

val writeAccesses:
    'a storage
    -> LargeInt.int

end



signature PART_EXTERNAL_STORAGE = sig

include EXTERNAL_STORAGE

type part_id = int

type bit_vector = Word8Vector.vector

val path:
    'a storage
    -> string

val prefix:
    'a storage
    -> string

val partNumItems:
    'a storage * part_id
    -> int

val loadPart:
    (bit_vector -> ExternalPartition.item * 'b)
    -> (ExternalPartition.id * 'c -> 'c)
    -> 'c
    -> 'a storage * part_id * 'b ExternalPartition.part
    -> 'c

val unloadPart:
    (ExternalPartition.item * 'b -> bit_vector option)
    -> 'a storage * part_id * 'b ExternalPartition.part
    -> unit

val flushPart:
    'a storage * part_id
    -> unit

val emptyPart:
    'a storage * part_id
    -> unit

val removeDuplicates:
    (ExternalPartition.id * 'c -> 'c)
    -> 'c
    -> 'a storage * part_id * 'b ExternalPartition.part
    -> 'c

val insert:
    'a storage * part_id * bit_vector
    -> unit

val appPart:
    (bit_vector -> unit)
    -> 'a storage * part_id
    -> unit

val foldPart:
    (bit_vector * 'b -> 'b)
    -> 'b
    -> 'a storage * part_id
    -> 'b

val numPartitions:
    'a storage
    -> int
    
val stateDistribution:
    'a storage
    -> (part_id * int) list

val reorganizePart:
    (bit_vector -> part_id)
    -> 'a storage * part_id * part_id list
    -> unit

val reorganizeAndEmptyPart:
    'a storage * part_id * part_id list
    -> unit

val reorganizeMemPart:
    (bit_vector -> part_id)
    -> 'a storage * 'b ExternalPartition.part * part_id * part_id list
    -> unit

end




functor PartExternalStorage(
structure Encoder: SERIALIZER): PART_EXTERNAL_STORAGE = struct

exception PartExtStorageIOError

structure ExternalPartition = ExternalPartition

val fnName = "PartExtStorage"

val lgWidth = 2

val partIdToString = Int.toString

type part_id = int

type bit_vector = Word8Vector.vector

type id = Word32.word * Word32.word

type item = Encoder.src

type init_options = {
     path      : string,
     prefix    : string,
     bufferSize: int,
     initParts : int
}

type 'a storage = {
     path       : string,
     prefix     : string,
     parts      : int ref,
     items      : int ref,
     distrib    : int Array.array ref,
     readAcc    : LargeInt.int ref,
     writeAcc   : LargeInt.int ref,
     buffer     : bit_vector list Array.array ref,
     bufferSize : int,
     bufferSpace: int ref
}

fun path ({ path, ... }: 'a storage) = path

fun prefix ({ prefix, ... }: 'a storage) = prefix

fun incr (a, partId, n) =
    Array.update (a, partId, Array.sub (a, partId) + n)

fun rmFile fileName =
    (OS.FileSys.remove fileName) handle _ => ()

fun partName (path, prefix, partId) =
    OS.Path.joinDirFile { dir  = path,
			  file = prefix ^ "-" ^ (partIdToString partId) }

fun emptyStorage { path, prefix, bufferSize, initParts } _ = let
    val path = if OS.Path.isAbsolute path
	       then path
	       else OS.Path.mkAbsolute { path       = path,
					 relativeTo = OS.FileSys.getDir () }
in
    {
     path        = path,
     prefix      = prefix,
     parts       = ref initParts,
     items       = ref 0,
     bufferSize  = bufferSize,
     bufferSpace = ref bufferSize,
     distrib     = ref (Array.array (initParts, 0)),
     readAcc     = ref (LargeInt.fromInt 0),
     writeAcc    = ref (LargeInt.fromInt 0),
     buffer      = ref (Array.array (initParts, []))
    }
end

fun numItems ({ items, ... }: 'a storage) = !items

fun partNumItems ({ distrib, ... }: 'a storage, partId) =
    Array.sub (!distrib, partId)

fun isEmpty ({ items, ... }: 'a storage) = !items = 0

fun numItems ({ items, ... }: 'a storage) = !items

fun numPartitions ({ parts, ... }: 'a storage) = !parts

fun readAccesses ({ readAcc, ... }: 'a storage) = !readAcc

fun writeAccesses ({ writeAcc, ... }: 'a storage) = !writeAcc

fun stateDistribution ({ distrib, ... }: 'a storage) =
    Array.foldri (fn (i, n, l) => (i, n) :: l) [] (!distrib)

fun openRead ({path, prefix, ...}: 'a storage, partId) =
    (SOME (BinIO.openIn (partName (path, prefix, partId)))) handle _ => NONE

fun openAppend ({path, prefix, ...}: 'a storage, partId) = let
    val stream = BinIO.openAppend (partName (path, prefix, partId))
in
    BinIO.StreamIO.setBufferMode (BinIO.getOutstream stream, IO.BLOCK_BUF);
    stream
end

fun writeState (stream, v) = let
    val lg = Word8Vector.length v
    val written = lgWidth + lg
in
    BinIO.output1 (stream, Word8.fromInt (lg div 256));
    BinIO.output1 (stream, Word8.fromInt (lg mod 256));    
    BinIO.output (stream, v)
end

fun readState stream =
    if BinIO.endOfStream stream
    then NONE
    else let val w1 = Word8.toInt (valOf (BinIO.input1 stream))
	     val w2 = Word8.toInt (valOf (BinIO.input1 stream))
	     val lg = w1 * 256 + w2
	     val v = BinIO.inputN (stream, lg)
         in
	     SOME v
         end

fun emptyPart (S as {path, items, prefix, distrib,
		     bufferSpace, buffer, ...}: 'a storage,
               partId) = let
    val file = partName (path, prefix, partId)
in
    rmFile file;
    items := (!items) - Array.sub (!distrib, partId);
    bufferSpace := !bufferSpace + (List.length (Array.sub (!buffer, partId)));
    Array.update (!distrib, partId, 0);
    Array.update (!buffer, partId, [])
end

fun foldStream f value S stream =
    case readState stream
     of NONE => (value, 0)
      | SOME item => let
	    val (value, n) = foldStream f (f (item, value)) S stream
	in
	    (value, n + 1)
	end

fun foldPart f value (S as { buffer, readAcc, ... }: 'a storage, partId) = let
    val value = List.foldl f value (Array.sub (!buffer, partId))
in
    case openRead (S, partId)
     of NONE => value
      | SOME stream => let val (value, n) = foldStream f value S stream
		       in
			   readAcc := LargeInt.+ (!readAcc,
						  LargeInt.fromInt n);
			   value
		       end before BinIO.closeIn stream
end

fun appPart f (S, partId) =
    foldPart (fn (s, ()) => f s) () (S, partId)

fun loadPart map f value (S, partId, P) =
    foldPart (fn (item, value) =>
		 let val (item, tag) = map item
		     val (inserted, id, _) =
			 ExternalPartition.insert (P, item, tag)
		 in
		     if inserted
		     then f (id, value)
		     else value
		 end) value (S, partId)

fun unloadPart map (S as { items, writeAcc, distrib, ...}: 'a storage,
		    partId, P) = let
    val stream = openAppend (S, partId)
    val n = ExternalPartition.fold
		(fn ((_, item, tag), n) =>
		    case map (item, tag)
		     of SOME v => (writeState (stream, v);
				   n + 1)
		      | NONE => n) 0 P
in
    incr (!distrib, partId, n);
    items := !items + n;
    writeAcc := LargeInt.+ (!writeAcc, LargeInt.fromInt n);
    BinIO.closeOut stream
end

fun removeDuplicates f value (S, partId, P) =
    foldPart (fn (item, value) => case ExternalPartition.delete (P, item)
				   of SOME info => f (#1 info, value)
				    | NONE => value)
	     value (S, partId)

fun reorganizePart hash
		   (S as { path, prefix, parts, items, distrib,
			   readAcc, writeAcc, buffer, ... }: 'a storage,
		    partId, newIds) = let
    val new = List.map (fn p => (p, BinIO.openOut
					(partName (path, prefix, p) ^ ".tmp")))
		       newIds
    fun loop stream =
	case readState stream
	 of NONE => 0
	  | SOME s => let
                val p' = hash s
                val new = #2 (valOf (List.find (fn (p, _) => p = p') new))
            in
                incr (!distrib, p', 1);
                writeState (new, s);
                1 + (loop stream)
            end
    fun getI array default i = if i <> partId andalso i < (!parts)
			       then Array.sub (array, i)
			       else default
    val n = List.length newIds
    val buf = Array.sub (!buffer, partId)
in
    buffer := Array.tabulate (!parts - 1 + n, getI (!buffer) []);
    distrib := Array.tabulate (!parts - 1 + n, getI (!distrib) 0);
    parts := !parts - 1 + n;
    case openRead (S, partId)
     of NONE => ()
      | SOME old => let val n = loop old
		    in
			readAcc := LargeInt.+ (!readAcc, n);
			writeAcc := LargeInt.+ (!writeAcc, n);
			BinIO.closeIn old;
			OS.FileSys.remove (partName (path, prefix, partId))
		    end;
    List.app (fn (partId, new) =>
		 let val f = partName (path, prefix, partId)
		 in
                     BinIO.closeOut new;
                     if 0 = Array.sub (!distrib, partId)
                     then (rmFile f;
			   rmFile (f ^ ".tmp"))
                     else (OS.FileSys.rename { new = f,
                                               old = f ^ ".tmp" })
		 end) new;
    List.app (fn s =>
		 let val p = hash s
		 in
                     incr (!distrib, p, 1);
                     Array.update (!buffer, p, s :: Array.sub (!buffer, p))
		 end) buf
end
				      
fun reorganizeAndEmptyPart
	(S as { path, prefix, parts, items, buffer, distrib, ... }: 'a storage,
         partId, newIds) = let
    fun getI array default i = if i <> partId andalso i < (!parts)
                               then Array.sub (array, i)
                               else default
    val newParts = !parts - 1 + (List.length newIds)
in
    items := !items - Array.sub (!distrib, partId);
    distrib := Array.tabulate (newParts, getI (!distrib) 0);
    buffer := Array.tabulate (newParts, getI (!buffer) []);
    parts := newParts;
    rmFile (partName (path, prefix, partId))
end
				      
fun reorganizeMemPart
	hash
	(S as { path, prefix, parts, items,
		writeAcc, buffer, distrib, ... }: 'a storage,
         part, partId, newIds) = let
    fun openNew id = BinIO.openOut (partName (path, prefix, id))
    val new = List.map (fn partId => (partId, openNew partId)) newIds
    fun write (_, s, _) = let
        val p' = hash s
	val new = #2 (valOf (List.find (fn (p, _) => p = p') new))
    in
        items := !items + 1;
	incr (!distrib, p', 1);
	writeState (new, s)
    end
    val n = LargeInt.fromInt (ExternalPartition.numItems part)
in
    reorganizeAndEmptyPart (S, partId, newIds);
    ExternalPartition.app write part;
    writeAcc := LargeInt.+ (!writeAcc, n);
    List.app (fn (partId, new) => (
                 BinIO.closeOut new;
                 if 0 = Array.sub (!distrib, partId)
                 then rmFile (partName (path, prefix, partId))
                 else ())) new
end

fun flushPart (S as { writeAcc, bufferSpace, buffer, ... }: 'a storage,
	       partId) = let
    val stream = openAppend (S, partId)
    val b = Array.sub (!buffer, partId)
    val n = List.foldl (fn (s, n) => (writeState (stream, s); n + 1)) 0 b
in
    bufferSpace := !bufferSpace + n;
    writeAcc := LargeInt.+ (!writeAcc, LargeInt.fromInt n);
    Array.update (!buffer, partId, []);
    BinIO.closeOut stream
end

fun insert (S as { parts, distrib, items, bufferSize,
		   writeAcc, bufferSpace, buffer, ... }: 'a storage,
	    partId, item) =
    (if bufferSize = 0
     then let val stream = openAppend (S, partId)
	  in
	      writeState (stream, item);
	      writeAcc := LargeInt.+ (!writeAcc, LargeInt.fromInt 1);
	      BinIO.closeOut stream
	  end
     else (if !bufferSpace = 0
	   then List.app (fn p => flushPart (S, p))
			 (List.tabulate (!parts, fn i => i))
	   else ();
	   Array.update (!buffer, partId,
			 item :: (Array.sub (!buffer, partId)));
	   bufferSpace := !bufferSpace - 1);
     items := !items + 1;
     incr (!distrib, partId, 1))

fun add _ =
    raise LibBase.Unimplemented (fnName ^ ".add")

fun addList _ =
    raise LibBase.Unimplemented (fnName ^ ".addList")

fun contains _ =
    raise LibBase.Unimplemented (fnName ^ ".contains")

fun contains' _ =
    raise LibBase.Unimplemented (fnName ^ ".contains'")

fun lookup _ =
    raise LibBase.Unimplemented (fnName ^ ".lookup")

fun getTag _ =
    raise LibBase.Unimplemented (fnName ^ ".getTag")

fun getTag' _ =
    raise LibBase.Unimplemented (fnName ^ ".getTag'")

fun setTag _ =
    raise LibBase.Unimplemented (fnName ^ ".setTag")

fun setTag' _ =
    raise LibBase.Unimplemented (fnName ^ ".setTag'")

fun diskUsage _ =
    raise LibBase.Unimplemented (fnName ^ ".diskUsage")

end
