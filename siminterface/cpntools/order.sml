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
(* TODO:             Does not work with timed types
 *)
structure CPN'OrderCommon =
struct
    fun CPN'OrderPlace order ([], []) = EQUAL
      | CPN'OrderPlace order ([], _) = LESS
      | CPN'OrderPlace order (_, []) = GREATER
      | CPN'OrderPlace order (hd1::tl1, hd2::tl2) =
      let
        val result = order (hd1, hd2)
      in
        if result = EQUAL
        then CPN'OrderPlace order (tl1, tl2)
        else result
      end
end

functor CPN'Order (structure CPN'NetCapture : CPN'NETCAPTURE ) :
sig
(*  val genEventOrder : CPN'NetCapture.Net -> string list*)
  val genStateOrder : CPN'NetCapture.Net -> string list
end =
struct
fun genStateOrder (net, sorts) =
let
      val rest = ["in\nCPN'OrderTop (CPN'net1, CPN'net2)\nend\nend"]
      fun generate_top rest =
      let
        fun generate_header no rest =
          List.foldl
          (fn ({name, prime = 1, ...} : CPN'NetCapture.Page, rest) => ", "::name::" = CPN'"::name::"'"::no::rest | (_, rest) => rest)
          (""::rest) net
        fun generate_contents rest =
          List.@(List.foldl
          (fn ({name, prime = 1, ...} : CPN'NetCapture.Page, rest) =>
          List.@("let\n val CPN'result = CPN'OrderPage'"::name::" (CPN'"::name::"'1, CPN'"::
          name::"'2)\nin\nif CPN'result = EQUAL\nthen\n"::rest, ["else CPN'result\nend\n"])
          | (_, rest) => rest)
          ["EQUAL\n"] net,
          rest)
      in
        "fun CPN'OrderTop ({"::
        (List.tl (generate_header "1" ("}, {"::(List.tl (generate_header "2" ("}) =\n"::
        (generate_contents rest)))))))
      end
      fun generate_page ({name, places, subpages, ...} : CPN'NetCapture.Page, rest) =
      let
        fun generate_places_header no rest = 
          List.foldl (fn ({name, port, ...}, rest) =>
          if port
          then rest
          else ", "::name::" = CPN'var'"::name::"'"::no::rest) rest places
        fun generate_subpages_header no ({transition_name, ...} : CPN'NetCapture.Subpage, rest) =
          ", "::transition_name::" = CPN'var'"::transition_name::"'"::no::rest
        fun generate_places_contents rest =
          List.foldl (fn ({name, port, sort, ... }, rest) =>
          if port
          then rest
          else List.@("let\nval CPN'result = CPN'OrderPlace "::sort::".cmp (CPN'var'"::
          name::"'1 , CPN'var'"::name::"'2)\nin if CPN'result = EQUAL\nthen\n"::rest,
          ["else CPN'result\nend\n"]))
          rest places
        fun generate_subpages_contents ({transition_name, page_name, ... } : CPN'NetCapture.Subpage, rest) =
          List.@("let\nval CPN'result = CPN'OrderPage'"::page_name::" (CPN'var'"::transition_name::
          "'1, CPN'var'"::transition_name::"'2)\nin if CPN'result = EQUAL\nthen\n"::rest,
          ["else CPN'result\nend\n"])
      in
        List.@("fun CPN'OrderPage'"::name::" ({"::
        (List.tl (generate_places_header "1"
        (List.foldl (generate_subpages_header "1") (""::"} : "::name::", {"::(List.tl (generate_places_header "2"
        (List.foldl (generate_subpages_header "2") (""::"} : "::name::") =\n"::
        (generate_places_contents (List.foldl generate_subpages_contents
        ["EQUAL\n"] subpages)))
        subpages)))) subpages))), rest)
      end
    in
      "local\nopen CPN'OrderCommon Mark\nin\nfun CPN'StateOrder (CPN'net1 : CPNToolsState.state, CPN'net2 : CPNToolsState.state) =\nlet\n"::
      (List.foldr generate_page (generate_top (""::rest))
      net)
    end
end;
