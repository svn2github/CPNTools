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
* Module:       CPN state type
*
* Description:  Code generation function for the CPN state type
*
* TODO:         Pages with no places are shown
*               "Fake" (=port) places are not
*               generated in Mark
*               Fusion places are shown "everywhere"
*               Maybe rename to CPN'StateGen
*
* Generate
* structure Pages = struct
*    val Top'RecNo1 = 1
*    val Top'RecNo2 = 2
* end
*
* Generate
* structure Mark' = struct
*    val Receiver'Receive (instance) =
*        case instance
*          of Top'RecNo1 => Mark.Top'RecNo1'Receive
*           | Top'RecNo2 => Mark.Top'RecNo2'Receive
* end
*
* Generate
* structure SetMark? = struct
*    val Top.RecNo1.Receive (cs ms, state) -> state
*    val Top.RecNo2.Receive (cs ms, state) -> state
* end
*)

functor CPN'State (structure CPN'NetCapture : CPN'NETCAPTURE) : 
	sig
	    val genMark  : CPN'NetCapture.Net -> string list
	    val genState : CPN'NetCapture.Net -> string list
	end =
struct
    exception StateGenerationError of string

  fun gen_type internal (net : CPN'NetCapture.Page list) =
    let
        fun generate ({id, places, subpages, name, ...} : CPN'NetCapture.Page, rest) =
        let
            fun gen_subpage ({transition_name, transition_id, page_id, page_name, instances}, rest) =
                transition_name::" : "::(if internal
					 then "CPN'" ^ page_name
					 else page_name)::",\n"::rest
            fun gen_places () =
                List.foldl (fn ({name, id, port, ...}, rest) =>
                if port
                then rest
                else name::" : CPN'place"::id::
		     (if internal
		      then String.concat[".ims.cs ", "CPN'place", id, ".ims.ims"]
		      else ".ims.cs ms")::",\n"::rest)
                [] places
            val contents = List.foldl gen_subpage (gen_places ()) subpages
            val contents' = List.rev contents
            val contents'' = List.tl contents' handle _ => []
            val contents''' = List.rev contents''
        in
            (List.concat["type "::(if internal
				   then "CPN'" ^ name
				   else name)::" = {\n"::[],
            contents''',
            "\n}\n"::[]])::rest
        end
        fun generate_top net =
        let
            fun generate_one ({name, prime = 0, ...} : CPN'NetCapture.Page, rest) =
                rest
              | generate_one ({name, prime = 1, ...}, rest) =
                ",\n"::(if internal
			then "CPN'" ^ name
			else name)::" : "::name::rest
              | generate_one _ =
                raise StateGenerationError "prime multiplicity not 0 or 1"
            val contents = "\n}\n"::(List.tl (List.foldl generate_one ["\n"] net))
            val contents' = List.rev contents
        in
	    
            "type "::(if internal
		      then "internal_state"
		      else "state")::" = {"::contents'
        end
    in
        List.concat (
        List.foldr generate [generate_top net] net)
    end

    fun get_subpage inst_info instance =
        #2 (Option.valOf (List.find (fn (a, b) => a = instance) inst_info))

    fun gen_getter ((net, _) : CPN'NetCapture.Net) =
    let
        fun generate ({id, places, subpages, name, instances, ...} : CPN'NetCapture.Page, rest) =
        let
            fun gen_page instance =
            let
                fun gen_subpage ({transition_name, transition_id, page_id, page_name, instances}, rest) =
                    transition_name::" = "::page_name::"'"::(Int.toString
                    (get_subpage instances instance))::",\n"::rest
                fun gen_places () =
                    List.foldl (fn ({name, id, port, ...}, rest) =>
                    if port
                    then rest
                    else name::" = CPN'place"::id::".get "::(Int.toString
                    instance)::",\n"::rest)
                    [] places
                val contents = List.foldl gen_subpage (gen_places ()) subpages
                val contents' = List.rev contents
                val contents'' = List.tl contents' handle _ => []
                val contents''' = List.rev contents''
            in
                (List.concat["val "::name::"'"::(Int.toString instance)::" = {\n"::[],
                contents''',
                "\n}\n"::[]])
            end
        in
            List.concat (List.tabulate (instances, fn instance => gen_page
            (instance + 1)))::rest
        end
        fun generate_top net =
        let
            fun generate_one ({name, prime = 0, ...} : CPN'NetCapture.Page, rest) =
                rest
              | generate_one ({name, prime = 1, ...}, rest) =
                ",\n"::"'1"::name::" = "::name::rest
              | generate_one _ =
                raise StateGenerationError "prime multiplicity not 0 or 1"
            val contents = "\n}\n"::(List.tl (List.foldl generate_one ["\n"] net))
            val contents' = List.rev contents
        in
            "{"::contents'
        end
    in
        List.concat [
        "fun getState () : state =\n"::
        "let\n"::
        "val _ = ()\n"::[],
        List.concat (List.foldr generate [] net),
        "in\n"::[],
        generate_top net,
        "end\n"::[]
        ]
    end

    fun gen_setter ((net, _) : CPN'NetCapture.Net) =
    let
        fun generate ({id, places, subpages, name, instances, ...} : CPN'NetCapture.Page, rest) =
        let
            val subpage_head =
                List.foldl
                (fn ({transition_name, ...}, rest) => ", "::transition_name::rest)
                [""] subpages
            val all_head =
                List.foldl
                (fn ({name, port, ...}, rest) =>
                if port
                then rest
                else ", "::name::rest)
                subpage_head places
            fun gen_page instance =
            let
                fun gen_subpage ({transition_name, transition_id, page_id, page_name, instances}, rest) =
                    "val _ = set'"::page_name::"'"::(Int.toString
                    (get_subpage instances instance))::" "::transition_name::"\n"::rest
                fun gen_places () =
                    List.foldl (fn ({name, id, port, ...}, rest) =>
                    if port
                    then rest
                    else "val _ = CPN'place"::id::".set "::(Int.toString
                    instance)::" "::name::"\n"::rest)
                    [] places
                val contents = List.foldl gen_subpage (gen_places ()) subpages
            in
                (List.concat["fun set'"::name::"'"::(Int.toString instance)::" {"::[],
                List.tl all_head,
                "} =\n"::
                "let\n"::
                "val _ = ()\n"::[],
                contents,
                "in\n"::
                "()\n"::
                "end\n"::[]])
            end
        in
            List.concat (List.tabulate (instances, fn instance => gen_page
            (instance + 1)))::rest
        end
        fun generate_top net =
        let
            val header =
                List.foldl
                (fn ({name, prime = 1, ...}, rest) => ", "::name::rest | (_, rest) => rest)
                [""] net
            fun generate_one ({name, prime = 0, ...} : CPN'NetCapture.Page, rest) =
                rest
              | generate_one ({name, prime = 1, ...}, rest) =
                "val _ = set'"::name::"'1 "::name::"\n"::rest
              | generate_one _ =
                raise StateGenerationError "prime multiplicity not 0 or 1"
            val contents = List.foldl generate_one [] net
        in
            List.concat [
            "fun set {"::(List.tl header),
            "} =\n"::
            "let\n"::
            "val _ = ()\n"::[],
            contents,
            "in\n"::
            "()\n"::
            "end\n"::[]]
        end
    in
        List.concat [
        "fun setState (state : state) =\n"::
        "let\n"::
        "val _ = ()\n"::[],
        List.concat (List.foldr generate [] net),
        generate_top net,
        "in\n"::
        "set (state)\n"::
        "end\n"::[]
        ]
    end

    fun gen_winning ((net, _) : CPN'NetCapture.Net) =
    let
        fun generate ({id, places, subpages, name, instances, ...} : CPN'NetCapture.Page, rest) =
        let
            val subpage_head =
                List.foldl
                (fn ({transition_name, ...}, rest) => ", "::transition_name::rest)
                [""] subpages
            val all_head =
                List.foldl
                (fn ({name, port, ...}, rest) =>
                if port
                then rest
                else ", "::name::rest)
                subpage_head places
            fun gen_page instance =
            let
                fun gen_subpage ({transition_name, transition_id, page_id, page_name, instances}, rest) =
                    "(winning'"::page_name::"'"::(Int.toString
                    (get_subpage instances instance))::" "::transition_name::") orelse\n"::rest
                fun gen_places () =
                    List.foldl (fn ({name, id, port, ...}, rest) =>
                    if port
                    then rest
                    else if (String.isPrefix "win" name)
                    then "("::name::" <> []) orelse\n"::rest
                    else rest)
                    [] places
                val contents = List.foldl gen_subpage (gen_places ()) subpages
            in
                (List.concat["fun winning'"::name::"'"::(Int.toString instance)::" {"::[],
                List.tl all_head,
                "} =\n"::
                contents,
                "false\n"::[]])
            end
        in
            List.concat (List.tabulate (instances, fn instance => gen_page
            (instance + 1)))::rest
        end
        fun generate_top net =
        let
            val header =
                List.foldl
                (fn ({name, prime = 1, ...}, rest) => ", "::name::rest | (_, rest) => rest)
                [""] net
            fun generate_one ({name, prime = 0, ...} : CPN'NetCapture.Page, rest) =
                rest
              | generate_one ({name, prime = 1, ...}, rest) =
                "(winning'"::name::"'1 "::name::") orelse\n"::rest
              | generate_one _ =
                raise StateGenerationError "prime multiplicity not 0 or 1"
            val contents = List.foldl generate_one [] net
        in
            List.concat [
            "fun winning' {"::(List.tl header),
            "} =\n"::
            contents,
            "false\n"::[]]
        end
    in
        List.concat [
        "fun winning (state : state) =\n"::
        "let\n"::
        "val _ = ()\n"::[],
        List.concat (List.foldr generate [] net),
        generate_top net,
        "in\n"::
        "winning' state\n"::
        "end\n"::[]
        ]
    end

    fun genMark ((net, _) : CPN'NetCapture.Net) =
    let
        val prime = List.filter (fn {prime = 1, ...} => true | _ => false) net
        fun generate ({places, name, subpages, ...} : CPN'NetCapture.Page, path) =
        let
            val path_to_name =
                String.concat (
                List.tl (
                List.foldl (fn (name, rest) => "'"::name::rest) [] path)
                handle _ => [])
            fun build [] = "state"
              | build [value] = String.concat ["#", value, "(state)"]
              | build (head::tail) = String.concat ["#", head, "(", build tail, ")"]
            fun generate_self () =
                "fun "::path_to_name::" (state : state) = "::
                (build path)::"\n"::[]
            fun generate_place () =
                List.foldl (
                fn ({name = place_name, port, ...}, rest) =>
                    if port
                    then rest
                    else "fun "::path_to_name::"'"::place_name::
                    " (state : state) = "::
                    (build (place_name::path))::"\n"::rest)
                    [] places
            fun equals (a, b) = a = b
            val page_table : (string, CPN'NetCapture.Page) HashTable.hash_table =
                HashTable.mkTable (HashString.hashString, equals) (40,
                LibBase.Impossible "page_table.find")
            val _ = List.foldl (fn (page as {id, ...}, _) =>
                HashTable.insert page_table (id, page)) () net
            fun generate_subpage ({transition_name, transition_id, page_id, page_name,
                instances}, rest) =
                (generate (HashTable.lookup page_table page_id, 
                transition_name::path))::rest
        in
            List.concat [
            generate_self (),
            generate_place (),
            List.concat (List.foldl generate_subpage [] subpages)
            ]
        end
    in
        List.concat [
        "structure Mark = struct\n"::[],
        gen_type true net,
        gen_type false net,
        List.concat (List.map (fn (page as {name, ...}) => generate (page, [name])) prime),
        "end\n"::[]
        ]
    end

    fun gen_tostring ((net, _) : CPN'NetCapture.Net) =
    let
        val prime = List.filter (fn {prime = 1, ...} => true | _ => false) net
        fun generate ({places, name, subpages, ...} : CPN'NetCapture.Page, path) =
        let
            val path_to_name =
                String.concat (
                List.tl (
                List.foldl (fn (name, rest) => "'"::name::rest) [] path)
                handle _ => [])
            fun build [] = "state"
              | build [value] = String.concat ["#", value, "(state)"]
              | build (head::tail) = String.concat ["#", head, "(", build tail, ")"]
            fun generate_place () =
                List.foldl (
                fn ({name = place_name, port, id, ...}, rest) =>
                    if port
                    then rest
                    else "val result = \"\\n\"::(CPN'place"::id::
                    ".ims.cs.mkstr_ms ("::(build (place_name::path))::
                    "))::\""::path_to_name::"'"::place_name::
                    ": \"::result\n"::rest) [] places
            fun equals (a, b) = a = b
            val page_table : (string, CPN'NetCapture.Page) HashTable.hash_table =
                HashTable.mkTable (HashString.hashString, equals) (40,
                LibBase.Impossible "page_table.find")
            val _ = List.foldl (fn (page as {id, ...}, _) =>
                HashTable.insert page_table (id, page)) () net
            fun generate_subpage ({transition_name, transition_id, page_id, page_name,
                instances}, rest) =
                (generate (HashTable.lookup page_table page_id, 
                transition_name::path))::rest
        in
            List.concat [
            generate_place (),
            List.concat (List.foldl generate_subpage [] subpages)
            ]
        end
    in
        List.concat [
        "fun toString (state : state) =\n"::
        "let\n"::
        "val result = []\n"::[],
        List.concat ((List.map (fn (page as {name, ...}) => generate (page,
        [name])) prime)),
        "in\n"::
        "String.concat (List.rev result)\n"::
        "end\n"::[]
        ]
    end

    fun genState net =
        List.concat [
        "local\n"::
        "open Mark\n"::
        "in\n"::
        "structure CPNToolsState : CPN'STATE =\n"::
        "struct\n"::
        "open Mark\n"::[],
        gen_getter net,
        gen_setter net,
        gen_winning net,
        gen_tostring net,
        ["fun internalize _ = raise LibBase.Unimplemented \"internalize not implemented\"\n"],
        ["fun externalize _ = raise LibBase.Unimplemented \"externalize not implemented\"\n"],
        "end\n"::
        "end\n"::[]
        ]
end
