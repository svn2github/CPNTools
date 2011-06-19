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
functor HashMapStorage(
structure Mapping: REVERSIBLE_MAPPING
structure Hash   : HASH_FUNCTION
sharing type Mapping.dest = Hash.state
) : EXPLICIT_REMOVE_STORAGE = struct

type id = word * int

type ('a, 'b) bucket = (int * 'a * 'b) vector

type item = Mapping.src

type 'a storage = {
     table  : (Mapping.dest, 'a) bucket array,
     next   : int ref,
     default: 'a,
     size   : int,
     items  : int ref
}

type init_options = {
     init_size : int
}

fun equals (a, b) = a = b

fun emptyStorage { init_size } default =
    {
     table   = Array.array (init_size, Vector.fromList []),
     next    = ref 0,
     items   = ref 0,
     size    = init_size,
     default = default
    }

fun index (h, size) =
    Word.toInt (Word.mod (h, Word.fromInt size))

fun add (storage as { table, next, items, size, default }, state) = let
    val item = Mapping.map state
    val h = Hash.hash item
    val i = index (h, size)
    val v = Array.sub (table, i)
    val lg = Vector.length v
    val num = Vector.foldl (fn (_, res as (SOME _)) => res
			     | ((num, item', _), NONE) => if item' = item
							  then SOME num
							  else NONE) NONE v
in
    case num
     of SOME num => ((h, num), true, storage)
      | NONE => (
	Array.update (table, i,
		      Vector.tabulate (lg + 1,
				       (fn i => if i = lg
						then (!next, item, default)
						else Vector.sub (v, i))));
	((h, !next), false, storage)
	before (items := !items + 1;
		next := !next + 1))
end

fun addList (storage, states) = let
    fun addOne (state, (other, storage)) = let
        val (id, contained, storage') = add (storage, state)
    in
        ((id, contained)::other, storage')
    end
in
    List.foldl addOne ([], storage) states
end

fun add' (state, storage) = #3 (add (storage, state))

fun contains ({ table, size, ... }: 'a storage, state) = let
    val item = Mapping.map state
    val h = Hash.hash item
    val i = index (h, size)
    val v = Array.sub (table, i)
in
    Vector.exists (fn (_, item', _) => item = item') v
end

fun contains' ({ table, size, ... }: 'a storage, (h, num)) = let
    val i = index (h, size)
    val v = Array.sub (table, i)
in
    Vector.exists (fn (num', _, _) => num = num') v
end

fun numItems ({ items, ... }: 'a storage) = !items

fun isEmpty ({ items, ... }: 'a storage) = !items = 0

fun lookup ({ table, size, ... }: 'a storage, (h, num)) = let
    val i = index (h, size)
    val v = Array.sub (table, i)
    val item = #2 (valOf (Vector.find (fn (num', _, _) => num' = num) v))
in
    Mapping.unmap item
end

fun getTag ({ table, size, ... }: 'a storage, state) = let
    val item = Mapping.map state
    val h = Hash.hash item
    val i = index (h, size)
    val v = Array.sub (table, i)
in
    #3 (valOf (Vector.find (fn (_, item', _) => item' = item) v))
end

fun getTag' ({ table, size, ... }: 'a storage, (h, num)) = let
    val i = index (h, size)
    val v = Array.sub (table, i)
in
    #3 (valOf (Vector.find (fn (num', _, _) => num' = num) v))
end

fun setTag (storage as { table, size, ... }: 'a storage, state, tag) = let
    val item = Mapping.map state
    val h = Hash.hash item
    val i = index (h, size)
    val v = Array.sub (table, i)
    val (j, (num, item, _)) =
	valOf (Vector.findi (fn (_, (_, item', _)) => item' = item) v)
in
    Array.update (table, i, Vector.update (v, j, (num, item, tag)));
    storage
end

fun setTag' (storage as { table, size, ... }: 'a storage, (h, num), tag) = let
    val i = index (h, size)
    val v = Array.sub (table, i)
    val (j, (num, item, _)) =
	valOf (Vector.findi (fn (_, (num', _, _)) => num' = num) v)
in
    Array.update (table, i, Vector.update (v, j, (num, item, tag)));
    storage
end

fun foldl f value ({ table, ... }: 'a storage) =
    Array.foldl (fn (v, value) =>
		    Vector.foldl (fn ((_, item, tag), value) =>
				     f (Mapping.unmap item, tag, value))
				 value v)
		value table

fun exists pred ({ table, ... }: 'a storage) =
    Array.exists
	(fn v => Vector.exists (fn (_, item, tag) =>
				   pred (Mapping.unmap item, tag)) v) table

fun find pred ({ table, size, ... }: 'a storage) = let
    fun loop i =
	if i = size
	then NONE
	else let val v = Array.sub (table, i)
		 val state =
		     Vector.foldl (fn ((_, item, tag), NONE) => let
					  val state = Mapping.unmap item
				      in
					  if pred (state, tag)
					  then SOME state
					  else NONE
				      end     
				    | (_, some) => some) NONE v
	     in
		 case state
		  of NONE => loop (i + 1)
		   | _ => state
	     end
in
    loop 0
end

fun delete (storage as { table, next, items, size, default }, state) = let
    val item = Mapping.map state
    val h = Hash.hash item
    val i = index (h, size)
    val v = Array.sub (table, i)
    val item = Vector.findi (fn (_, (_, item', _)) => item = item') v
in
    case item
     of NONE => raise LibBase.NotFound
      | SOME (j, _) => let
	    val lg = Vector.length v
	    val v = Vector.tabulate (Vector.length v - 1,
				     (fn j' => if j' < j
					       then Vector.sub (v, j')
					       else Vector.sub (v, j' + 1)))
	in
	    items := !items - 1;
	    Array.update (table, i, v);
	    storage
	end
end

fun delete' (storage as { table, next, items, size, default }, (h, num)) = let
    val i = index (h, size)
    val v = Array.sub (table, i)
    val item = Vector.findi (fn (_, (num', _, _)) => num = num') v
in
    case item
     of NONE => raise LibBase.NotFound
      | SOME (j, _) => let
	    val lg = Vector.length v
	    val v = Vector.tabulate (Vector.length v - 1,
				     (fn j' => if j' < j
					       then Vector.sub (v, j')
					       else Vector.sub (v, j' + 1)))
	in
	    items := !items - 1;
	    Array.update (table, i, v);
	    storage
	end
end

fun app f ({ table, ... }: 'a storage) =
    Array.app (fn v => Vector.app (fn (_, item, tag) =>
				      f (Mapping.unmap item, tag)) v)
	      table

fun filter f (storage as { default, size, ... }: 'a storage) =
    foldl (fn (item, tag, storage) => if f (item, tag)
				      then add' (item, storage)
				      else storage)
          (emptyStorage { init_size = size } default)
          storage

fun map f (storage as { default, size, ... }: 'a storage) =
    foldl (fn (item, tag, storage) => add' (f (item, tag), storage))
          (emptyStorage { init_size = size } default)
          storage

fun partition f (storage as { default, size, ... }: 'a storage) =
    foldl (fn (item, tag, (yes, no)) => if f (item, tag)
					then (add' (item, yes), no)
					else (yes, add' (item, no)))
          (emptyStorage { init_size = size } default,
           emptyStorage { init_size = size } default)
          storage

end
