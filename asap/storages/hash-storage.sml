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
functor HashStorage(
structure Model : MODEL
structure Hash : HASH_FUNCTION
sharing type Model.state = Hash.state
) : EXPLICIT_REMOVE_STORAGE =
struct
    type item = Hash.state
    type 'a storage = (Hash.state, 'a) LargeHashTable.hash_table * 'a
    type init_options = { init_size : int }
    type id = item

    fun equals (a, b) = a = b

    fun emptyStorage { init_size } default =
        (LargeHashTable.mkTable (Hash.hash, equals) (init_size, LibBase.NotFound), default)

    fun add ((storage, default), element) =
        if (LargeHashTable.inDomain storage element)
        then (element, true, (storage, default))
        else (element, false, (LargeHashTable.insert storage (element, default);
        (storage, default)))

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

    fun add' (element, storage) =
        #3 (add (storage, element))

    fun contains ((storage, default), element) =
        LargeHashTable.inDomain storage element
    val contains' = contains

    fun numItems (storage, default) = LargeHashTable.numItems storage

    fun isEmpty storage =
        numItems storage = 0

    fun delete ((storage, default), element) =
        (LargeHashTable.remove storage element; (storage, default))
    val delete' = delete

    fun foldl f init (storage, default) =
        LargeHashTable.foldi (fn (element, value, rest) => f (element, value,
        rest)) init storage

    fun app f storage =
        foldl (fn (element, value, _) => f (element, value)) () storage

    fun map f (storage, default) =
        foldl (fn (element, value, storage) => add' (f (element, value), storage))
        (emptyStorage { init_size = (2 * numItems (storage, default)) } default)
        (storage, default)

    local
        exception Found of item
    in
        fun find f storage =
            (foldl (fn (element, value, _) => if f (element, value)
                                              then raise (Found element)
                                              else ()) () storage; NONE)
            handle (Found element) => SOME element
    end

    fun exists f storage =
        Option.isSome (find f storage)

    fun partition f (storage, default) =
        foldl (fn (element, value, (yes, no)) => if f (element, value)
                                              then (add' (element, yes), no)
                                              else (yes, add' (element, no)))
                                              (emptyStorage { init_size = 1000 } default,
                                              emptyStorage { init_size = 1000}
                                              default)
        (storage, default)

    fun filter f (storage, default) =
        foldl (fn (element, value, storage) => if f (element, value)
                                                 then add' (element, storage)
                                                 else storage)
        (emptyStorage { init_size = 1000 } default)
        (storage, default)

    fun getTag ((storage, default), element) =
        LargeHashTable.lookup storage element
    val getTag' = getTag
    fun setTag ((storage, default), element, value) =
        if LargeHashTable.inDomain storage element
        then (LargeHashTable.insert storage (element, value); (storage, default))
        else raise LibBase.NotFound
    val setTag' = setTag

    fun lookup (_, item) = item
end
