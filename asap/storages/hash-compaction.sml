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
functor HashCompactionStorage(
structure HashState : HASH_FUNCTION
structure HashWord  : HASH_FUNCTION
   where type state = Word.word) : REMOVE_STORAGE =
struct
	
type item = HashState.state
type 'a storage = (HashWord.state, 'a) LargeHashTable.hash_table * 'a
type init_options = { init_size : int }
type id = item
	  
fun equals (a, b) = a = b

fun emptyStorage { init_size } default =
    (LargeHashTable.mkTable (HashWord.hash, equals)
			    (init_size, LibBase.NotFound), default)
    
fun add ((storage, default), element) = let
    val hashValue = HashState.hash element
in
    if LargeHashTable.inDomain storage hashValue
    then (element, true, (storage, default))
    else (element, false, 
          (LargeHashTable.insert storage (hashValue, default);
	   (storage, default)))
end

fun addList (storage, elems) = let
    fun addOne (elem, (other, storage)) = let
        val (id, contained, storage') = add (storage, elem)
    in
        ((id, contained)::other, storage')
    end
in
    List.foldl addOne ([], storage) elems
end

fun contains ((storage, _), element) =
    LargeHashTable.inDomain storage (HashState.hash element)

val contains' = contains

fun numItems (storage, _) = LargeHashTable.numItems storage

fun isEmpty storage =
    numItems storage = 0
    
fun delete ((storage, default), element) =
    (LargeHashTable.remove storage (HashState.hash element);
     (storage, default))

fun getTag ((storage, default), element) =
    LargeHashTable.lookup storage (HashState.hash element)

val getTag' = getTag

fun setTag ((storage, default), element, value) = let
    val hashValue = HashState.hash element
in
    if LargeHashTable.inDomain storage hashValue
    then (LargeHashTable.insert storage (hashValue, value);
	  (storage, default))
    else raise LibBase.NotFound
end

val setTag' = setTag

fun lookup (_, item) = item

val delete' = delete

end



functor DefaultHashCompactionStorage(
structure Hash: HASH_FUNCTION) : REMOVE_STORAGE = struct

structure Storage = HashCompactionStorage(
structure HashState = Hash
structure HashWord  = Hash31ToHash31)

open Storage

end
