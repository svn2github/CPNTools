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
structure CPN'HashCommon = struct
    fun CPN'HashPlace hashFunction place (start : Word.word) (CPN'combine :
        Word.word * Word.word -> Word.word) =
        List.foldl (fn (CPN'p, CPN'h) => hashFunction CPN'p CPN'h CPN'combine) (CPN'combine (0w65823,
        start)) place

    fun CPN'HashColorUnit _ (rest : Word.word) (CPN'combine : Word.word *
        Word.word -> Word.word) = CPN'combine (0w27, rest)

    fun CPN'HashColorBool true (rest : Word.word) (CPN'combine : Word.word *
        Word.word -> Word.word) = CPN'combine (0w29, rest)
      | CPN'HashColorBool false (rest : Word.word) (CPN'combine : Word.word *
      Word.word -> Word.word) = CPN'combine (0w31, rest)

    fun CPN'HashColorInt CPN'i (rest : Word.word) (CPN'combine : Word.word *
        Word.word -> Word.word) =
        CPN'combine (Word.fromInt CPN'i, rest)

    fun CPN'HashColorChar CPN'c = CPN'HashColorInt (Char.ord CPN'c)

    fun CPN'HashColorString CPN's (rest : Word.word) (CPN'combine : Word.word *
        Word.word -> Word.word) =
        let
            fun combiner (CPN'c, CPN'h) = CPN'HashColorChar CPN'c CPN'h CPN'combine
        in
            CharVector.foldl combiner (CPN'combine(0w23, rest)) CPN's
        end

    fun CPN'HashColorIntInf CPN'i = CPN'HashColorString (IntInf.toString CPN'i)

    fun CPN'HashColorReal CPN'r = CPN'HashColorString (Real.toString CPN'r)

    fun CPN'HashColorList hashFunction [] rest (CPN'combine : Word.word *
        Word.word -> Word.word) = CPN'combine(0w37, rest)
      | CPN'HashColorList hashFunction (CPN'l::ll) rest CPN'combine
      = hashFunction CPN'l (CPN'HashColorList hashFunction ll rest CPN'combine)
      CPN'combine

  val CPN'HashColorTime = CPN'HashColorIntInf
end

functor CPN'HashFunction (structure CPN'NetCapture : CPN'NETCAPTURE ) :
sig
  val genHashFunction : CPN'NetCapture.Net -> string list
end=
struct
fun genHashFunction (net, sorts) =
let
    fun gen_cs ({ name = CPN'name, timed = CPN'timed, sort = CPN'sort }, CPN'rest) =
    let
        fun CPN'head ((CPN'name, _), CPN'rest) =
          ", "::CPN'name::" = CPN'"::CPN'name::CPN'rest
        fun CPN'body ((CPN'name, CPN'cs), CPN'rest) =
          List.@("CPN'HashColor'"::CPN'cs::" CPN'"::CPN'name::" ("::CPN'rest, [") CPN'combine"])
      fun gen_one (CPN'NetCapture.unit_cs, CPN'rest) =
        "CPN'HashColorUnit CPN'v CPN'rest CPN'combine"::CPN'rest
        | gen_one (CPN'NetCapture.bool_cs, CPN'rest) =
        "CPN'HashColorBool CPN'v CPN'rest CPN'combine"::CPN'rest
        | gen_one (CPN'NetCapture.int_cs, CPN'rest) =
        "CPN'HashColorInt CPN'v CPN'rest CPN'combine"::CPN'rest
        | gen_one (CPN'NetCapture.intinf_cs, CPN'rest) =
        "CPN'HashColorIntInf CPN'v CPN'rest CPN'combine"::CPN'rest
        | gen_one (CPN'NetCapture.real_cs, CPN'rest) =
        "CPN'HashColorReal CPN'v CPN'rest CPN'combine"::CPN'rest
        | gen_one (CPN'NetCapture.char_cs, CPN'rest) =
        "CPN'HashColorChar CPN'v CPN'rest CPN'combine"::CPN'rest
        | gen_one (CPN'NetCapture.string_cs, CPN'rest) =
        "CPN'HashColorString CPN'v CPN'rest CPN'combine"::CPN'rest
        | gen_one (CPN'NetCapture.time_cs, CPN'rest) =
        "CPN'HashColorTime CPN'v CPN'rest CPN'combine"::CPN'rest
        | gen_one (CPN'NetCapture.enum_cs CPN'values, CPN'rest) =
        "case CPN'v of"::
        (List.tl (#1 (List.foldl (fn (CPN'n, (CPN'rest, CPN'i)) =>
        ("| "::"\n("::CPN'n::") => CPN'combine(0w"::(Int.toString CPN'i)::", CPN'rest)"::CPN'rest,
        CPN'i + 1)) (CPN'rest, 1) CPN'values)))
        | gen_one (CPN'NetCapture.index_cs CPN'idx, CPN'rest) =
        "case CPN'v of\n ("::CPN'idx::"(CPN'i)) => CPN'HashColorInt CPN'i CPN'rest CPN'combine"::CPN'rest
        | gen_one (CPN'NetCapture.list_cs CPN'sort, CPN'rest) =
        "CPN'HashColorList CPN'HashColor'"::CPN'sort::" CPN'v CPN'rest CPN'combine"::CPN'rest
        | gen_one (CPN'NetCapture.product_cs CPN'members, CPN'rest) =
        let
          val CPN'body = List.@ (List.foldl CPN'body ["CPN'rest"] CPN'members,
          CPN'rest)
        in
          "case CPN'v of\n{"::(List.tl (List.foldl CPN'head (""::"} => "::CPN'body) CPN'members))
        end
        | gen_one (CPN'NetCapture.record_cs CPN'members, CPN'rest) =
        let
          val CPN'body = List.@ (List.foldl CPN'body ["CPN'rest"] CPN'members, CPN'rest)
        in
          "case CPN'v of\n{"::(List.tl (List.foldl CPN'head (""::"} => "::CPN'body) CPN'members))
        end
        | gen_one (CPN'NetCapture.funsubset_cs CPN'sort, CPN'rest) =
        "CPN'HashColor'"::CPN'sort::" CPN'v CPN'rest CPN'combine"::CPN'rest
        | gen_one (CPN'NetCapture.listsubset_cs CPN'sort, CPN'rest) =
        "CPN'HashColor'"::CPN'sort::" CPN'v CPN'rest CPN'combine"::CPN'rest
        | gen_one (CPN'NetCapture.alias_cs CPN'sort, CPN'rest) =
        "CPN'HashColor'"::CPN'sort::" CPN'v CPN'rest CPN'combine"::CPN'rest
        | gen_one (CPN'NetCapture.union_cs CPN'name, CPN'rest) =
          let
              fun CPN'entry ((CPN'name, CPN'cs), CPN'rest) =
                  if CPN'cs = ""
                  then "("::CPN'name::") => CPN'HashColorUnit () CPN'rest CPN'combine\n| "::CPN'rest
                  else "("::CPN'name::"(CPN'i)) => CPN'HashColor'"::CPN'cs::" CPN'i CPN'rest CPN'combine\n| "::CPN'rest
          in
            "case CPN'v\nof "::
            (List.foldl CPN'entry ("_ => 0w0"::CPN'rest) CPN'name)
          end
    in
      "fun CPN'HashColor'"::CPN'name::" CPN'v CPN'rest CPN'combine =\n   "::
      gen_one (CPN'sort, "\n"::CPN'rest)
    end

      val rest = ["\nin\nCPN'HashTop (CPN'net, 0w53)\nend\nend"]
      fun generate_top rest =
      let
        fun generate_header rest =
          List.foldl
          (fn ({name, prime = 1, ...} : CPN'NetCapture.Page, rest) => ", "::name::rest | (_, rest) => rest)
          (""::rest) net
        fun generate_contents rest =
          List.@(List.foldl
          (fn ({name, prime = 1, ...} : CPN'NetCapture.Page, rest) =>
          List.concat["CPN'HashPage'"::name::" ("::name::",\n"::[], rest, [")"]]
          | (_, rest) => rest)
          ["CPN'rest"] net,
          rest)
      in
        "fun CPN'HashTop ({"::
        (List.tl (generate_header ("}, CPN'rest) =\n"::
        (generate_contents rest))))
      end
      fun generate_page ({name, places, subpages, ...} : CPN'NetCapture.Page, rest) =
      let
        fun generate_places_header rest = 
          List.foldl (fn ({name, port, ...}, rest) =>
          if port
          then rest
          else ", "::name::" = CPN'var'"::name::rest) rest places
        fun generate_subpages_header ({transition_name, ...} : CPN'NetCapture.Subpage, rest) =
          ", "::transition_name::" = CPN'var'"::transition_name::rest
        fun generate_places_contents rest =
          List.foldl (fn ({name, port, sort, ... }, rest) =>
          if port
          then rest
          else List.@("CPN'HashPlace CPN'HashColor'"::sort::" CPN'var'"::name::"\n("::rest, [") CPN'combine"]))
          rest places
        fun generate_subpages_contents ({transition_name, page_name, ... } : CPN'NetCapture.Subpage, rest) =
          List.@("CPN'HashPage'"::page_name::" (CPN'var'"::transition_name::",\n"::rest, [")"])
      in
        List.@("fun CPN'HashPage'"::name::" ({"::
        (List.tl (generate_places_header
        (List.foldl generate_subpages_header (""::"} : "::name::", CPN'rest) =\n"::
        (generate_places_contents (List.foldl generate_subpages_contents
        ["CPN'rest"] subpages)))
        subpages))), "\n"::rest)
      end
in
    "local\nopen CPN'HashCommon Mark\nin\n\n"::
    "(*****  hash functions for color sets  *****)\n" ::
    List.foldr gen_cs ("(*******************************************)\n\n" ::
		       "\nfun CPNToolsHashFunction CPN'combine"::
		       " (CPN'net : CPNToolsState.state) =\nlet\n"::
		       List.foldr generate_page (generate_top (""::rest)) net)
	       sorts
end
end
