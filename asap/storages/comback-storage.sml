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
signature COMBACK_STATS =
sig
  type 'a storage

  val countCollisionSizes : 'a storage -> string
  val countBacktrackLengths : 'a storage -> string
  val getMaxId : 'a storage -> string
  val allStats : 'a storage -> string
end

signature COMBACK_STORAGE =
sig
  include REMOVE_STORAGE
    type event
    type state

  val addBackedge: 'a storage -> int * state * (event * int) -> 'a storage

  structure Stats : COMBACK_STATS where type 'a storage = 'a storage
end

functor ComBackCommon (
structure Model : MODEL
type event
eqtype hash
structure Hash : HASH_FUNCTION where type state = hash
structure Cache : CACHE where type key = int
val hash : Model.state -> hash
val trace : (Model.state * Model.event list) -> event list -> (Model.state * Model.event list) list
val default_event : event
val default_hash : hash
) =
struct
    structure Key : HASH_KEY =
    struct
        type hash_key = Hash.state

        val hashVal = Hash.hash

        fun sameKey (a, b) = a = b
    end

    structure HashTable = LargeHashTableFn(Key)

    type item = Model.state
    type event = event
    type state = Model.state * Model.event list
    type id = int
    type init_options = { hash_size : int, array_size : int,
                          cache_found : bool, cache_size : int,
                          initial_states : (Model.state * Model.event list) list}
    
    open HashTable
    open DynamicArray

    fun equals (a, b) = a = b

    type state_mapping = (int list) hash_table
    type 'a edge_array = (event * int * Hash.state * 'a) DynamicArray.array
    (* State mapping, max used id, current size, edge array, should we cache when a state is
     * discovered (vs caching when we re-discover a state), the cache, initial
     * states *)
    type 'a storage = state_mapping * int * int * 'a edge_array * bool * (Model.state *
    Model.event list) Cache.cache * (Model.state * Model.event list) list

    fun emptyStorage { hash_size, array_size, cache_found, cache_size,
        initial_states } default =
        (mkTable (hash_size, LibBase.NotFound),
        0,
        0,
        array (array_size, (default_event, 0, default_hash, default)),
        cache_found,
        Cache.emptyCache cache_size,
        initial_states) : 'a storage

    fun numItems (_, _, size, _, _, _, _) = size
    fun isEmpty (_, _, 0, _, _, _, _) = true
      | isEmpty _ = false

    fun backtrack_to edge_array cache_found cache initial_states id =
    let
        fun gather 1 acc = acc
          | gather id acc =
          let
              val (event, predecessor, _, _) = sub (edge_array, id)
          in
              gather predecessor (event::acc)
          end

        fun gather_cache 1 acc = (initial_states, acc)
          | gather_cache id acc =
          case Cache.find cache id
            of NONE =>
            let
                val (event, predecessor, _, _) = sub (edge_array, id)
            in
                gather_cache predecessor (event::acc)
            end
             | SOME state => ([state], acc)

        fun get_states (initial_states, gathered) =
            List.concat (
            List.map
            (fn state => trace state gathered)
            initial_states)
    in
        if Cache.size cache = 0
        then get_states (initial_states, gather id [])
        else 
            let
                val states = get_states (gather_cache id [])
            in
                if cache_found
                then states
                else case states
                       of [state] => (Cache.insert cache (id, state); states)
                        | _ => states
            end
    end

  fun addBackedge (state_mapping, max_id, size, edge_array, cache_found,
        cache, initial_states) (id, state, (event, predecessor)) =
        let
            val (_, _, hash, value) = sub (edge_array, id)
            val _ = update (edge_array, id, (event, predecessor, hash, value))
        in
            if cache_found then (Cache.insert cache (id, state); ()) else ();
            (state_mapping, max_id, size, edge_array, cache_found, cache, initial_states)
        end

    fun member' ((state_mapping, max_id, size, edge_array, cache_found, cache, initial_states), element) =
    let
        val hash = hash element
        val ids = lookup state_mapping hash handle LibBase.NotFound => []
        val states = List.map (fn id => (backtrack_to edge_array cache_found
        cache initial_states id, id)) ids
        val states' = List.foldl (fn ((lst, id), rest) => List.foldl (fn (elm,
        rest) => (elm, id)::rest) rest lst) [] states 
        val found =
            List.find (fn ((state', events), id) => equals (element, state')) states'
    in
        case found
          of NONE => (hash, ids, 0, false)
           | SOME (_, id) => (hash, ids, id, true)
    end

    fun contains (storage, element) =
        #4 (member' (storage, element))

    fun contains' ((state_mapping, max_id, size, edge_array, cache_found, cache, initial_states), id) =
      id <= max_id andalso
      let
        val (_, _, hash, _) = sub (edge_array, id)
        val ids = lookup state_mapping hash
      in
        List.exists (fn x => x = id) ids
      end

    fun delete' ((state_mapping, max_id, size, edge_array, cache_found, cache, initial_states), id) =
      if id <= max_id
      then
        let
          val (_, _, hash, _) = sub (edge_array, id)
          val ids = lookup state_mapping hash
        in
          if ids = [id]
          then
            let
              val _ = remove state_mapping hash
            in
              (state_mapping, max_id, size - 1, edge_array, cache_found, cache, initial_states)
            end
          else
            let
              val ids' = List.filter (fn x => x <> id) ids
            in
              if ids = ids'
              then (state_mapping, max_id, size, edge_array, cache_found, cache, initial_states)
              else
                let
                  val _ = insert state_mapping (hash, ids')
                in
                  (state_mapping, max_id, size - 1, edge_array, cache_found, cache, initial_states)
                end
            end
        end
      else (state_mapping, max_id, size, edge_array, cache_found, cache, initial_states)

    fun delete (storage, element) =
    let
      val (_, _, id, exists) = member' (storage, element)
    in
      if exists
      then delete' (storage, id)
      else storage
    end

    fun add ((state_mapping, max_id, size, edge_array, cache_found, cache, initial_states), element) =
    let
        val (hash, ids, id, hasElement) = member' ((state_mapping, max_id, size, edge_array,
        cache_found, cache, initial_states), element)
    in
        if hasElement
        then (id, true, (state_mapping, max_id, size, edge_array, cache_found, cache, initial_states))
        else
            let
              val size' = size + 1
              val max_id' = max_id + 1
(*                val _ = print "Inserting state with number "
                val _ = print (Int.toString size')
                val _ = print "\n"*)
                val _ = insert state_mapping (hash, max_id'::ids)
            in
              (size', false, (state_mapping, max_id', size', edge_array, cache_found, cache,
                initial_states))
            end
    end

    fun addList (storage, elems) =
    let
        fun addOne (elem, (other, storage)) =
        let
            val (id, contained, storage') =
                add (storage, elem)
        in
            ((id, contained)::other, storage')
        end
    in
        List.foldl addOne ([], storage) elems
    end

  fun lookup ((state_mapping, max_id, size, edge_array, cache_found, cache,
        initial_states), id) =
        case backtrack_to edge_array cache_found cache initial_states id
            of [(elm, _)] => elm
           | _ => raise LibBase.NotFound

  fun getTag' ((state_mapping, max_id, size, edge_array, cache_found, cache,
        initial_states), id) =
        let
            val (_, _, _, value) = sub (edge_array, id)
        in
            value
        end

    fun getTag (storage, element) =
    let
        val (_, _, id, found) = member' (storage, element)
    in
        if (found)
        then getTag' (storage, id)
        else raise LibBase.NotFound
    end

  fun setTag' ((state_mapping, max_id, size, edge_array, cache_found, cache,
        initial_states), id, value) =
        let
            val (event, number, hash, _) = sub (edge_array, id)
            val _ = update (edge_array, id, (event, number, hash, value))
        in
          (state_mapping, max_id, size, edge_array, cache_found, cache,
            initial_states)
        end

    fun setTag (storage, element, value) =
    let
        val (_, _, id, found) = member' (storage, element)
    in
        if (found)
        then setTag' (storage, id, value)
        else raise LibBase.NotFound
    end
structure Stats =
struct
  type 'a storage = 'a storage
    fun countCollisionSizes (state_mapping, _, _, _, _, _, _) =
    let
        fun count (lst, counter) =
            Counter.add counter (List.length lst)
        val counter = HashTable.fold count (Counter.new()) state_mapping
    in
        Counter.toString counter
    end

    fun countBacktrackLengths (_, _, size, edge_array, _, _, _) =
    let
        fun gather 1 = 0
          | gather id =
          let
              val (_, predecessor, _, _) = DynamicArray.sub (edge_array, id)
          in
              1 + (gather predecessor)
          end
        fun count 0 counter = counter
          | count n counter =
          count (n - 1) (Counter.add counter (gather n))
        val counter = count size (Counter.new ())
    in
        Counter.toString counter
    end

    fun getMaxId (_, max_id, _, _, _, _, _) =
      Int.toString max_id

    fun allStats storage =
        String.concat ["\nCollision sizes: ", countCollisionSizes storage,
        "\nBackTrack lengths: ", countBacktrackLengths storage,
        "\nMaximal used id: ", getMaxId storage]
end

end

functor ComBackCommonNoArcs(
structure Model : MODEL
eqtype hash
structure Hash : HASH_FUNCTION where type state = hash
structure Cache : CACHE where type key = int
val hash : Model.state -> hash
val default_hash : hash
) =
struct
    fun trace (state, events) [] = [(state, events)]
      | trace (state, events) (trans::rest) =
        let
            val next = Model.nextStates (state, List.nth (events, trans))
                       handle _ => []
        in
            List.concat (List.map (fn next => trace next rest) next)
        end

    structure Common = ComBackCommon (
    structure Model = Model
    type event = int
    type hash = hash
    structure Hash = Hash
    structure Cache = Cache
    val hash = hash
    val trace = trace
    val default_event = 0
    val default_hash = default_hash
    )

    open Common
end

functor ComBackCommonArcs(
structure Model : MODEL
eqtype hash
structure Hash : HASH_FUNCTION where type state = hash
structure Cache : CACHE where type key = int
val hash : Model.state -> hash
val default_hash : hash
) =
struct
    fun trace (state, events) track =
    let
        val _ = print "Tracing from state:\n###########################################################\n"
        val _ = print (Model.stateToString state)
        val _ = print
        "-----------------------------------------------------------\n"
        val _ = print 
        "Via events:\n===========================================================\n"
        fun pe [] = ()
          | pe (event::rest) =
          let
              val _ = print (Model.eventToString (Option.valOf event))
              val _ = print
              "-----------------------------------------------------------\n"
          in
              pe rest
          end
        val _ = pe track
        val _ = print "###########################################################\n"
    in
        (Model.executeSequence (state, List.map Option.valOf track)) handle _ => []
    end

    fun trace (state, events) track =
        (Model.executeSequence (state, List.map Option.valOf track)) handle _ => []

    structure Common = ComBackCommon(
    structure Model = Model
    type event = Model.event option
    type hash = hash
    structure Hash = Hash
    structure Cache = Cache
    val hash = hash
    val trace = trace
    val default_event = NONE
    val default_hash = default_hash
    )

    open Common

    type event = Model.event

    fun addBackedge store (id, state, (trans, pred)) =
        Common.addBackedge store (id, state, (SOME trans, pred))
end

functor ComBackStorage64NoArcs(
structure Model : MODEL
structure Hash1 : HASH_FUNCTION
structure Hash2 : HASH_FUNCTION
structure Cache : CACHE where type key = int
sharing type Model.state = Hash1.state = Hash2.state
) : COMBACK_STORAGE =
struct
    fun hash elm = (Word32.fromLargeWord (Word.toLargeWord (Hash1.hash elm)),
		    Word32.fromLargeWord (Word.toLargeWord (Hash2.hash elm)))

    structure Common = ComBackCommonNoArcs(
    structure Model = Model
    structure Hash = Hash64ToHash31
    structure Cache = Cache
    type hash = Word32.word * Word32.word
    val hash = hash
    val default_hash = (Word32.fromInt 0, Word32.fromInt 0)
    )

    open Common
end

functor ComBackStorage64(
structure Model : MODEL
structure Hash1 : HASH_FUNCTION
structure Hash2 : HASH_FUNCTION
structure Cache : CACHE where type key = int
sharing type Model.state = Hash1.state = Hash2.state
) : COMBACK_STORAGE =
struct
    fun hash elm = (Word32.fromLargeWord (Word.toLargeWord (Hash1.hash elm)),
		    Word32.fromLargeWord (Word.toLargeWord (Hash2.hash elm)))

    structure Common = ComBackCommonArcs(
    structure Model = Model
    structure Hash = Hash64ToHash31
    structure Cache = Cache
    type hash = Word32.word * Word32.word
    val hash = hash
    val default_hash = (Word32.fromInt 0, Word32.fromInt 0)
    )

    open Common
end

functor ComBackStorage62NoArcs(
structure Model : MODEL
structure Hash1 : HASH_FUNCTION
structure Hash2 : HASH_FUNCTION
structure Cache : CACHE where type key = int
sharing type Model.state = Hash1.state = Hash2.state
) : COMBACK_STORAGE =
struct
    fun hash elm = (Hash1.hash elm, Hash2.hash elm)

    structure Common = ComBackCommonNoArcs(
    structure Model = Model
    structure Hash = Hash62ToHash31
    structure Cache = Cache
    type hash = Word.word * Word.word
    val hash = hash
    val default_hash = (0w0, 0w0)
    )

    open Common
end

functor ComBackStorage62(
structure Model : MODEL
structure Hash1 : HASH_FUNCTION
structure Hash2 : HASH_FUNCTION
structure Cache : CACHE where type key = int
sharing type Model.state = Hash1.state = Hash2.state
) : COMBACK_STORAGE =
struct
    fun hash elm = (Hash1.hash elm, Hash2.hash elm)

    structure Common = ComBackCommonArcs(
    structure Model = Model
    structure Hash = Hash62ToHash31
    structure Cache = Cache
    type hash = Word.word * Word.word
    val hash = hash
    val default_hash = (0w0, 0w0)
    )

    open Common
end

functor ComBackStorage32NoArcs(
structure Model : MODEL
structure Hash : HASH_FUNCTION
structure Cache : CACHE where type key = int
sharing type Model.state = Hash.state
) : COMBACK_STORAGE =
struct
    fun hash elm = Word32.fromLargeWord (Word.toLargeWord (Hash.hash elm))

    structure Common = ComBackCommonNoArcs(
    structure Model = Model
    structure Hash = Hash32ToHash31
    structure Cache = Cache
    type hash = Word32.word
    val hash = hash
    val default_hash = Word32.fromInt 0
    )

    open Common
end


functor ComBackStorage32(
structure Model : MODEL
structure Hash : HASH_FUNCTION
structure Cache : CACHE where type key = int
sharing type Model.state = Hash.state
) : COMBACK_STORAGE =
struct
    fun hash elm = Word32.fromLargeWord (Word.toLargeWord (Hash.hash elm))

    structure Common = ComBackCommonArcs(
    structure Model = Model
    structure Hash = Hash32ToHash31
    structure Cache = Cache
    type hash = Word32.word
    val hash = hash
    val default_hash = Word32.fromInt 0
    )

    open Common
end

functor ComBackStorage31NoArcs(
structure Model : MODEL
structure Hash : HASH_FUNCTION
structure Cache : CACHE where type key = int
sharing type Model.state = Hash.state
) : COMBACK_STORAGE =
struct
    val hash = Hash.hash

    structure Common = ComBackCommonNoArcs(
    structure Model = Model
    structure Hash = Hash31ToHash31
    structure Cache = Cache
    type hash = Word.word
    val hash = hash
    val default_hash = 0w0
    )

    open Common
end

functor ComBackStorage31(
structure Model : MODEL
structure Hash : HASH_FUNCTION
structure Cache : CACHE where type key = int
sharing type Model.state = Hash.state
) : COMBACK_STORAGE =
struct
    val hash = Hash.hash

    structure Common = ComBackCommonArcs(
    structure Model = Model
    structure Hash = Hash31ToHash31
    structure Cache = Cache
    type hash = Word.word
    val hash = hash
    val default_hash = 0w0
    )

    open Common
end
