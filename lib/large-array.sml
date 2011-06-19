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
structure LibBase = struct
exception Unimplemented of string
exception Impossible of string
exception NotFound
end


structure LargeArray: ARRAY =
struct
    local
        open Array

        fun log2 0 = 0
          | log2 n = 1 + (log2 (n div 2))
  val shift = 0w2
  val shift = Word.-(Word.fromInt (log2 maxLen), 0w1)
  val modulo = Word.-(Word.<<(0w1, shift), 0w1)
  val sliceSize = Word.toInt(Word.+(modulo, 0w1))
        fun slicePart n = Word.toInt(Word.>>(Word.fromInt n, shift))
        fun slicePos n = Word.toInt(Word.andb(Word.fromInt n, modulo))
    in
    type 'a array = int * ('a Array.array) Vector.vector
        type 'a vector = 'a vector

        val maxLen = Option.valOf Int.maxInt

        fun array (size, default) =
        let
            val slices = slicePart size
            val rest = slicePos size
            val lst = if rest = 0
                      then []
                      else [Array.array (rest, default)]
            fun build 0 = lst
              | build n =
              (Array.array(sliceSize, default))::(build (n - 1))
        in
            (size, Vector.fromList (build slices))
        end

  fun fromList _ = raise LibBase.Unimplemented "LargeArray.fromList"

  fun tabulate (count, f) =
  let
    val slices = slicePart count
    val rest = slicePos count
    fun f' start n = f (start + n)
    fun tabulate' 0 = []
      | tabulate' n =
      (Array.tabulate (sliceSize, f' ((slices - n) * sliceSize)))::
      (tabulate' (n-1))
    val lst = tabulate' slices
    val lst' = if rest = 0
              then lst
              else List.@(lst, [Array.tabulate (rest, f' (slices * sliceSize))])
  in
    (count, Vector.fromList lst')
  end


        fun length (lngth, _) = lngth

        fun sub ((_, array), pos) =
        let
            val slice = slicePart pos
            val pos' = slicePos pos
            val array' = Vector.sub(array, slice)
        in
            Array.sub(array', pos')
        end

        fun update ((_, array), pos, elm) =
        let
            val slice = slicePart pos
            val pos' = slicePos pos
            val array' = Vector.sub(array, slice)
        in
            Array.update(array', pos', elm)
        end

        fun vector _ = raise LibBase.Unimplemented "LargeArray.vector"
        fun copy _ = raise LibBase.Unimplemented "LargeArray.copy"
        fun copyVec _ = raise LibBase.Unimplemented "LargeArray.copyVec"
        fun appi _ = raise LibBase.Unimplemented "LargeArray.appi"
  fun app f (_, array) =
  let
    fun f' array' =
      Array.app f array'
  in
    Vector.app f' array
  end
        fun modifyi _ = raise LibBase.Unimplemented "LargeArray.modifyi"

  fun modify f (_, array) =
    Vector.app (fn array' => ignore (Array.modify f array')) array;

  fun foldli _ = raise LibBase.Unimplemented "LargeArray.foldli"
  fun foldri _ = raise LibBase.Unimplemented "LargeArray.foldri"

  fun foldl f start (_, array) =
    Vector.foldl (fn (array, start) => Array.foldl f start array) start array
  fun foldr f start (_, array) =
    Vector.foldr (fn (array, start) => Array.foldr f start array) start array

  fun findi _ = raise LibBase.Unimplemented "LargeArray.findi"
        fun find _ = raise LibBase.Unimplemented "LargeArray.find"
        fun exists _ = raise LibBase.Unimplemented "LargeArray.exists"
        fun all _ = raise LibBase.Unimplemented "LargeArray.all"
        fun collate _ = raise LibBase.Unimplemented "LargeArray.collate"
    end
end

structure StandardArray = Array
structure Array = LargeArray

type 'a array = 'a LargeArray.array
