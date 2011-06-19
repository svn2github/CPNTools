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
(* Serializes states and events for sending to Java
 *
 * The serialization packs things into a lot of lists so the protocol can be
 * extended, e.g., with time without making many changes to existing code and/or
 * breaking things.
 *
 * TODO:             Does not work with timed types
 *)

signature CPN'PackCommon =
sig
    type value
    exception IllegalMultiSet
    val packMS : (''a -> string) -> ''a list -> value
    val packPlace : string list -> (''a -> value) -> ''a -> value list -> value list
    val unpackMS : (TextIO.instream -> 'a) -> value -> 'a list
end;

functor CPN'PackCommon (structure JavaExecute : JAVA_EXECUTE) :
CPN'PackCommon where type value = JavaExecute.value =
struct
    open JavaExecute
    exception IllegalMultiSet

    fun packMS packFunction [] = vLIST []
      | packMS packFunction (first::rest) =
    let
        fun localPack previous previousCount [] =
            [vLIST [vINT previousCount, vSTRING (packFunction previous)]]
          | localPack previous previousCount (head::tail) =
            if (previous = head)
            then localPack previous (previousCount + 1) tail
            else (vLIST [vINT previousCount, vSTRING (packFunction previous)])::
                 localPack head 1 tail 
    in
        vLIST (localPack first 1 rest)
    end

    fun packPlace ids function ms rest =
        (vLIST [vLIST (List.map (fn a => vSTRING a) ids), function ms])::rest

    fun unpackMS unpackFunction (vLIST lst) =
    let
        val unpacker = unpackFunction o TextIO.openString
        fun build (number, value, rest) = List.foldl (fn _ => value::rest) rest
          (List.tabulate (number, fn _ => ()))
        fun localUnpack [] = []
          | localUnpack ((vLIST [vINT coefficient, vSTRING value])::rest) =
            build (coefficient, (unpacker value), (localUnpack rest))
          | localUnpack _ = raise IllegalMultiSet
    in
        localUnpack lst
    end
      | unpackMS _ _ = raise IllegalMultiSet
end

functor CPN'PackFunction (structure CPN'NetCapture : CPN'NETCAPTURE ) :
sig
  val genPackStateFunction : CPN'NetCapture.Net -> string list
  val genPackEventFunction : CPN'NetCapture.Net -> string list
  val genPackerFunction : CPN'NetCapture.Net -> string list
end=
struct
fun genPackStateFunction (net, sorts) =
let
    fun gen_cs ({ name = CPN'name, timed = CPN'timed, sort = CPN'sort }, CPN'rest) =
        "val CPN'PackColor'"::CPN'name::" = CPN'PackCommon.packMS "::CPN'name::".mkstr\n"::CPN'rest

      val rest = ["\nin\nJavaExecute.vLIST (CPN'PackTop (CPN'net, []))\nend\nend"]
      fun generate_top rest =
      let
        fun generate_header rest =
          List.foldl
          (fn ({name, prime = 1, ...} : CPN'NetCapture.Page, rest) => ", "::name::rest | (_, rest) => rest)
          (""::rest) net
        fun generate_contents rest =
          List.@(List.foldl
          (fn ({name, prime = 1, ...} : CPN'NetCapture.Page, rest) =>
          List.concat["CPN'PackPage'"::name::" [] ("::name::",\n"::[], rest, [")"]]
          | (_, rest) => rest)
          ["CPN'rest"] net,
          rest)
      in
        "fun CPN'PackTop ({"::
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
          List.foldl (fn ({name, port, sort, id, ... }, rest) =>
          if port
          then rest
          else List.@("CPN'PackCommon.packPlace (\""::id::"\"::CPN'ids) CPN'PackColor'"::sort::" CPN'var'"::name::"\n("::rest, [")"]))
          rest places
        fun generate_subpages_contents ({transition_name, page_name, transition_id, ... } : CPN'NetCapture.Subpage, rest) =
            List.@("CPN'PackPage'"::page_name::" (\""::transition_id::"\"::CPN'ids) (CPN'var'"::transition_name::",\n"::rest, [")"])
      in
        List.@("fun CPN'PackPage'"::name::" CPN'ids ({"::
        (List.tl (generate_places_header
        (List.foldl generate_subpages_header (""::"} : "::name::", CPN'rest) =\n"::
        (generate_places_contents (List.foldl generate_subpages_contents
        ["CPN'rest"] subpages)))
        subpages))), "\n"::rest)
      end
in
    "local\n"::
    "structure CPN'PackCommon = CPN'PackCommon(structure JavaExecute = JavaExecute)\n"::
    "(*****  packingfunctions for color sets  *****)\n" ::
    List.foldr gen_cs ("(*******************************************)\n\n" ::
		       "\nin\nfun map "::
		       " (CPN'net : CPNToolsState.state) =\nlet\n"::
		       List.foldr generate_page (generate_top (""::rest)) net)
	       sorts
end

fun genPackEventFunction (net, sorts) =
let
    fun getTransitionList instance ({ prime = 1, ... } : CPN'NetCapture.Page) = []
      | getTransitionList instance ({ id, ... } : CPN'NetCapture.Page) =
      let
          fun matches ({ instances, page_id, ... } : CPN'NetCapture.Subpage) =
              page_id = id andalso List.exists (fn (a, b) => b = instance) instances
          val page = Option.valOf (List.find (fn (pg : CPN'NetCapture.Page) =>
          List.exists matches (#subpages pg)) net)
          val subpage = Option.valOf (List.find matches (#subpages page))
          val superpageInstance =
              #1 (Option.valOf (List.find (fn (a, b) => b = instance) (#instances subpage)))
      in
          (#transition_id subpage)::
          (getTransitionList superpageInstance page)
      end

    fun generatePage (page as { id, instances, transitions, name = page_name, ... }: CPN'NetCapture.Page, rest) =
    let
        fun generateInstance (number, rest) =
        let
            val trace = getTransitionList number page
            val trace' = List.foldr (fn (id, rest) => "(vSTRING \""::id::"\")::"::rest) ["[]"] trace
            val trace'' = String.concat trace'
            fun generateTransition ({ id, name, variables, free_variables, ...} : CPN'NetCapture.Transition, rest) =
            let
                val variables = List.concat (free_variables::variables)
                fun generateHead rest =
                    List.foldr (fn ({name, sort}, rest) => ", "::name::" = CPN'var'"::name::rest) rest variables
                fun generateBody rest =
                let
                    fun generateVariable ({name, sort}, rest) =
                        ", "::"vLIST [vSTRING \""::name::"\", vSTRING ("::sort::".mkstr CPN'var'"::name::")]"::rest
                in
                    "[vLIST ((vSTRING \""::id::"\")::"::trace''::"), vLIST ["::
                    (List.tl (List.foldl generateVariable (""::"]]"::rest) variables))
                end
            in
                "  | localMap ("::page_name::"'"::name::" ("::(Int.toString number)::", {"::
                (List.tl (generateHead (""::"})) = "::(generateBody ("\n"::rest)))))
            end
        in
            List.foldl generateTransition rest transitions
        end
    in
        List.foldl generateInstance rest (List.tabulate (instances, fn n => n + 1))
    end
in
    "local open Bind JavaExecute in\n"::
    "fun map event =\n"::
    "let\n"::
    "fun localMap CPN'FAKE = []\n"::
    (List.foldl generatePage (
    "  | localMap _ = []\n"::
    "in\n"::
    "JavaExecute.vLIST (localMap event)\n"::
    "end\n"::
    "end\n"::
    []) net)
end

    fun genPackerFunction net =
        List.concat ["structure CPNToolsPacker : PACKER = struct\n"::
        "open Mark Bind JavaExecute\n"::
        "structure StatePacker = struct\n"::
        "type src = state\n"::
        "type dest = value\n"::
        genPackStateFunction net,
        "\n"::
        "end\n"::
        "structure EventPacker = struct\n"::
        "type src = event\n"::
        "type dest = value\n"::
        genPackEventFunction net,
        "end\n"::
        "end\n"::
        []]
end
