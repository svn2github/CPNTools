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
functor HashCache(
structure Hash : HASH_FUNCTION
val remove_after_each : bool
) : PERSISTENCE_CACHE =
struct
    type key = Hash.state
    type 'a cache = (key, 'a) HashTable.hash_table * int * (key * 'a -> bool)

    fun equals (a, b) = a = b

    fun emptyCache' persistence size =
        (HashTable.mkTable (Hash.hash, equals) (size div 4, LibBase.NotFound), size,
        persistence)

    fun emptyCache size =
        emptyCache' (fn _ => false) size

    fun cleanup (cache, max_size, persistence) =
        if HashTable.numItems cache > max_size
        then
            let
                local
                    val itemsToRemove = ref (if remove_after_each
                                             then HashTable.numItems cache - max_size
                                             else HashTable.numItems cache - (max_size div 2))
                in
                    fun predicate elm =
                        if persistence elm orelse (!itemsToRemove <= 0)
                        then true
                        else (itemsToRemove := !itemsToRemove - 1; false)
                end
                val _ = 
                    HashTable.filteri predicate cache
            in
                (cache, max_size, persistence)
            end
        else (cache, max_size, persistence)

    fun insert (cache, max_size, persistence) (key, value) =
        (HashTable.insert cache (key, value);
         cleanup (cache, max_size, persistence))

    fun inDomain (cache, _, _) key =
        HashTable.inDomain cache key

    fun lookup (cache, _, _) key =
        HashTable.lookup cache key

    fun find (cache, _, _) key =
        HashTable.find cache key

    fun remove (cache, max_size, persistence) key =
        (HashTable.remove cache key; (cache, max_size, persistence))

    fun numItems (cache, _, _) =
        HashTable.numItems cache

    fun size (_, max_size, _) = max_size
end
