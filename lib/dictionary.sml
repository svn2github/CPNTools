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
 *     dictionary.sml
 *
 *  Created:
 *     Nov. 12, 2008
 *
 *  Description:
 *     A simple dictionary used e.g., by the CPN Tools serializer to store
 *  pointers, i.e., 32-bit words, instead of full items.
 *
 *)


signature DICTIONARY = sig

eqtype item

type item_ref = Word32.word

exception NotFound

exception Full

val empty: unit -> unit

val nullRef: item_ref

val numItems: unit -> int

val insert: item -> item_ref  (*  may raise Full  *)

val retrieve: item_ref -> item  (*  may raise NotFound  *)

val app: (item -> unit) -> unit

val fold: (item * 'a -> 'a) -> 'a -> 'a

end


functor HashDictionary(
eqtype item
val hash: item -> word): DICTIONARY = struct

type item = item

type item_ref = Word32.word

exception NotFound

exception Full

structure Dictionary = HashTableFn (
type hash_key = word
val hashVal = fn i => i
val sameKey = (op =))

val dict: (item list) Dictionary.hash_table =
    Dictionary.mkTable (100000, LibBase.NotFound)

val items = ref 0

val nullRef = Word32.fromInt 0

local
    fun h item = case Word.andb (hash item, 0wxFFFFFF)
		  of 0wx0 => 0wxFFFFFF
		   | h => h
in val hash = h end

fun empty () = (
    Dictionary.clear dict;
    items := 0)

fun numItems () = !items

fun insert item = let
    val k = hash item
    val l = Dictionary.find dict k
    val l = case l of NONE => [] | SOME l => l
    fun loop [] num = (false, num)
      | loop (item' :: l) num =
        if item = item'
        then (true, num)
        else loop l (num + 1)
    val (found, num) = loop l 0
in
    case num of 256 => raise Full | _ => ();
    if found
    then ()
    else (items := !items + 1;
          Dictionary.insert dict (k, List.@ (l, [item])));
    Word32.orb (Word32.fromLargeWord (Word.toLargeWord k),
		Word32.<< (Word32.fromInt num, 0wx18))
end

fun retrieve itemRef = let
    val k = Word.andb (Word.fromLargeWord (Word32.toLargeWord itemRef), 0wxFFFFFF)
    val num = Word32.toInt (Word32.>> (itemRef, 0wx18))
in
    List.nth (Dictionary.lookup dict k, num)
    handle _ => raise NotFound
end

fun app f = Dictionary.app (fn items => List.app f items) dict

fun fold f value = Dictionary.fold
		       (fn (items, value) => List.foldl f value items)
		       value dict

end
