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
functor BitStateStorage (
structure Hash : HASH_FUNCTION
) : REMOVE_STORAGE =
struct
    type item = Hash.state
    type 'a storage = BigBitArray.array * int * 'a
    type init_options = { init_size : word }
    type id = item

    fun emptyStorage { init_size } default =
        (BigBitArray.init init_size, 0, default)

    fun add ((array, size, default), element) = 
    let
        val h = Hash.hash element
    in
        if (BigBitArray.is_set h array)
        then (element, true, (array, size, default))
        else (element, false, (BigBitArray.set h array, size + 1, default))
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

    fun contains ((array, _, _), element) =
        BigBitArray.is_set (Hash.hash element) array
    val contains' = contains

    fun isEmpty (_, 0, _) = true
      | isEmpty _ = false

    fun numItems (_, items, _) = items

    fun getTag _ = raise LibBase.Unimplemented "SetStorage does not support tagging"
    val getTag' = getTag
    fun setTag _ = raise LibBase.Unimplemented "SetStorage does not support tagging"
    val setTag' = setTag

    fun delete ((array, size, default), element) =
    let
        val h = Hash.hash element
    in
        if (BigBitArray.is_set h array)
        then (BigBitArray.reset h array, size - 1, default)
        else raise LibBase.NotFound
    end
    val delete' = delete

    fun lookup (_, element) = element
end

functor BloomFilterStorage (
structure Hash1 : HASH_FUNCTION
structure Hash2 : HASH_FUNCTION
sharing type Hash1.state = Hash2.state
) : STORAGE =
struct
    type item = Hash1.state
    type 'a storage = BigBitArray.array * int * 'a
    type init_options = { init_size : word }
    type id = item

    fun emptyStorage { init_size } default =
        (BigBitArray.init init_size, 0, default)

    fun add ((array, size, default), element) = 
    let
        val h1 = Hash1.hash element
        val h2 = Hash2.hash element
    in
        if (BigBitArray.is_set h1 array) andalso (BigBitArray.is_set h2 array)
        then (element, true, (array, size, default))
        else (element, false, (BigBitArray.set h1 (BigBitArray.set h2 array),
        size + 1, default))
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

    fun getTag _ = raise LibBase.Unimplemented "SetStorage does not support tagging"
    val getTag' = getTag
    fun setTag _ = raise LibBase.Unimplemented "SetStorage does not support tagging"
    val setTag' = setTag

    fun contains ((array, _, _), element) =
        (BigBitArray.is_set (Hash1.hash element) array) andalso
        (BigBitArray.is_set (Hash2.hash element) array)
    val contains' = contains

    fun isEmpty (_, 0, _) = true
      | isEmpty _ = false

    fun numItems (_, items, _) = items

    fun lookup (_, element) = element
end

functor DoubleHashingStorage (
structure Hash1 : HASH_FUNCTION
structure Hash2 : HASH_FUNCTION
sharing type Hash1.state = Hash2.state
) : STORAGE =
struct
    type item = Hash1.state
    type 'a storage = BigBitArray.array * int * word * 'a
    type init_options = { init_size : word, combinations : word }
    type id = item

    fun emptyStorage { init_size, combinations } default =
        (BigBitArray.init init_size, 0, combinations, default)

    fun isset h1 h2 0w0 array = BigBitArray.is_set h1 array
      | isset h1 h2 n array = BigBitArray.is_set (Word.+(h1, Word.*(n, h2))) array
      andalso isset h1 h2 (n - 0w1) array

    fun set h1 h2 0w0 array = BigBitArray.set h1 array
      | set h1 h2 n array = BigBitArray.set (Word.+(h1, Word.*(n, h2))) (set h1 h2 (n-0w1) array)


    fun add ((array, size, combinations, default), element) = 
    let
        val h1 = Hash1.hash element
        val h2 = Hash2.hash element
    in
        if (isset h1 h2 combinations array)
        then (element, true, (array, size, combinations, default))
        else (element, false, (set h1 h2 combinations array, size + 1, combinations, default))
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

    fun getTag _ = raise LibBase.Unimplemented "SetStorage does not support tagging"
    val getTag' = getTag
    fun setTag _ = raise LibBase.Unimplemented "SetStorage does not support tagging"
    val setTag' = setTag

    fun lookup (_, element) = element

    fun contains ((array, _, combinations, _), element) =
    let
        val h1 = Hash1.hash element
        val h2 = Hash2.hash element
    in
        isset h1 h2 combinations array
    end
    val contains' = contains

    fun isEmpty (_, 0, _, _) = true
      | isEmpty _ = false

    fun numItems (_, items, _, _) = items
end
