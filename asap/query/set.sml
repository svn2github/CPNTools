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
structure Set :
sig
    type 'a set

    val has : ('a * 'a -> bool) -> 'a -> ('a set) -> bool
    val subtract : ('a * 'a -> bool) -> ('a set) -> ('a set) -> ('a set)
    val union : ('a * 'a -> bool) -> ('a set) -> ('a set) -> ('a set)
    val intersect : ('a * 'a -> bool) -> ('a set) -> ('a set) -> ('a set)
    val subset : ('a * 'a -> bool) -> ('a set) -> ('a set) -> bool
    val equals : ('a * 'a -> bool) -> ('a set) -> ('a set) -> bool
    val powerset : ('a set) -> 'a set set
    val sort : ('a * 'a -> bool) -> ('a set) -> ('a set)

    val fromList : ('a list) -> ('a set)
    val toList : ('a set) -> ('a list)

    val toString : ('a -> string) -> ('a set) -> string
end =
struct
    type 'a set = 'a list

    fun has equals elm lst = List.exists (fn elm' => equals(elm, elm')) lst
    fun subtract equals lst1 lst2 = List.filter (fn elm => not (has equals elm lst2)) lst1
    fun union equals lst1 lst2 = List.@(lst1, subtract equals lst2 lst1)
    fun intersect equals lst1 lst2 =
        List.filter (fn elm => has equals elm lst2) lst1
    fun subset eql lst1 lst2 =
        List.null (subtract eql lst1 lst2)
    fun equals eql lst1 lst2 =
        if (List.length lst1) = (List.length lst2)
        then ((subset eql lst1 lst2) andalso (subset eql lst2 lst1))
        else false
    fun powerset [] = [[]]
      | powerset (hd::tl) =
      let
          val s = powerset tl
          val hds = List.map (fn tl => hd::tl) s
      in
          List.@(s, hds)
      end
    val sort = ListMergeSort.sort

    fun fromList set = set
    fun toList set = set

    fun toString printer [] = "{}"
      | toString printer (s::ss) =
      let
          fun walk [] = ["}"]
            | walk (s::ss) = ", "::(printer s)::(walk ss)
      in
          String.concat ("{"::(printer s)::(walk ss))
      end
  end

structure EqSet :
sig
    type 'a set

    val has : ''a -> (''a set) -> bool
    val subtract : (''a set) -> (''a set) -> (''a set)
    val union : (''a set) -> (''a set) -> (''a set)
    val intersect : (''a set) -> (''a set) -> (''a set)
    val subset : (''a set) -> (''a set) -> bool
    val equals : (''a set) -> (''a set) -> bool
    val powerset : ('a set) -> 'a set set
    val sort : ('a * 'a -> bool) -> ('a set) -> ('a set)

    val fromList : ('a list) -> ('a set)
    val toList : ('a set) -> ('a list)

    val toString : ('a -> string) -> ('a set) -> string
end =
struct
    open Set

    fun eql (a, b) = (a = b)

    fun has elm lst = Set.has eql elm lst
    fun subtract lst1 lst2 = Set.subtract eql lst1 lst2
    fun union lst1 lst2 = Set.union eql lst1 lst2
    fun subset lst1 lst2 = Set.subset eql lst1 lst2
    fun equals lst1 lst2 = Set.equals eql lst1 lst2
    fun intersect lst1 lst2 = Set.intersect eql lst1 lst2
end
