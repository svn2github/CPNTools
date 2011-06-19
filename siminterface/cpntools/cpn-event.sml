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
  Module:       CPN event type

  Description:  Code generating function for the CPN event type

  TODO:         toString ?
                Is execute really ok? Time?
                Perhaps rename functor to CPN'EventGen?
*)

functor CPN'Event (structure CPN'NetCapture : CPN'NETCAPTURE) : 
	sig
	    val genBind     : CPN'NetCapture.Net -> string list
	    val genEvent    : CPN'NetCapture.Net -> string list
	    val gentoString : CPN'NetCapture.Net -> string list
	end =
struct

    fun fold_transitions f =
        List.foldl
        (fn (page as {transitions, ...} : CPN'NetCapture.Page, rest) =>
        List.foldl (f page) rest transitions)

    fun genBind ((net, _) : CPN'NetCapture.Net) =
    let
        fun gen_transition
            ({name = page_name, ...} : CPN'NetCapture.Page)
            ({free_variables, variables, name, ...} : CPN'NetCapture.Transition, rest) =
        let
            fun gen_var ({name, sort}, rest) =
                ", "::name::": "::sort::".cs"::rest
            val variables' = 
                List.foldl (fn (vars, rest) => List.foldl gen_var rest vars)
                (""::"}\n"::rest) variables
            val variables'' = List.foldl gen_var variables' free_variables
            val variables''' = List.tl variables''
        in
            "| "::page_name::"'"::name::" of int * {"::variables'''
        end
    in
        "structure Bind =\n"::
        "struct\n"::
        "datatype event = CPN'FAKE\n"::
        (fold_transitions gen_transition ["end\n"] net)
    end

    fun gen_enabled ((net, _) : CPN'NetCapture.Net) =
    let
        fun gen_transition
            ({name = page_name, instances, ...} : CPN'NetCapture.Page)
            ({free_variables, variables, name, id, ...} : CPN'NetCapture.Transition, rest) =
        let
            fun gen_name ({name, sort}, rest) = ", "::name::rest
            val free = List.foldr gen_name [""] free_variables
            fun gen_vars ([], rest) = rest
              | gen_vars (vars, rest) =
              ", "::"{"::
              (List.tl (List.foldr gen_name (""::"}"::rest) vars))
            val vars = List.foldr gen_vars free variables
            fun gen_value ({name, sort}, rest) = ", "::name::" = "::name::rest
            val free_values = List.foldr gen_value [""] free_variables
            val values =
                List.foldr (fn (vars, rest) => List.foldr gen_value rest vars)
                free_values variables
        in
            List.concat [
            "fun CPN'"::page_name::"'"::name::" () =\n"::
            "let\n"::
            "fun flatten CPN'inst (("::
            List.tl vars,
            "), rest) =\n"::
            "("::page_name::"'"::name::" (CPN'inst, {"::(List.tl values),
            "}))::rest\n"::
            "fun get (CPN'inst, rest) =\n"::
            "List.foldr (flatten CPN'inst) rest (CPN'transition"::id::".CPN'bindings CPN'inst)\n"::
            "in\n"::
            "List.foldl get [] (List.tabulate ("::
            (Int.toString instances)::", fn CPN'n => CPN'n + 1))\n"::
            "end\n"::[],
            rest
            ]
        end

        fun gen_transition_invocation
            ({name = page_name, ...} : CPN'NetCapture.Page)
            ({name, ...} : CPN'NetCapture.Transition, rest) =
            ", "::"CPN'"::page_name::"'"::name::" ()"::rest
    in
        List.concat [
        "fun getEnabled () =\n"::
        "let\n"::
        (fold_transitions gen_transition [] net),
        "in\n"::
        "List.concat [\n"::
        (List.tl (fold_transitions gen_transition_invocation [""] net)),
        "]\n"::
        "end\n"::[]
        ]
    end

    fun gen_execute ((net, _) : CPN'NetCapture.Net) =
    let
        fun gen_name ({name, sort}, rest) = ", "::name::rest
        fun gen_transition
            ({name = page_name, instances, ...} : CPN'NetCapture.Page)
            ({free_variables, variables, name, id, ...} : CPN'NetCapture.Transition, rest) =
            let
                val free_values = List.foldr gen_name [""] free_variables
                val values =
                    List.foldr (fn (vars, rest) => List.foldr gen_name rest vars)
                    free_values variables
                val free = List.foldr gen_name [""] free_variables
                fun gen_value ({name, sort}, rest) = ", "::name::" = "::name::rest
                fun gen_vars ([], rest) = rest
                  | gen_vars (vars, rest) =
                    ", "::"{"::
                    (List.tl (List.foldr gen_value (""::"}"::rest) vars))
                val vars = List.foldr gen_vars free variables
            in
                List.concat [
                "| CPN'exec ("::page_name::"'"::name::" (CPN'inst, {"::
                (List.tl values),
                "})) =\n"::
                "CPN'transition"::id::".CPN'occfun (CPN'inst, ("::[],
                List.tl vars,
                "), false)\n"::rest
                ]
            end
    in
        List.concat[
        "fun execute event =\n"::
        "let\n"::
        "fun CPN'exec CPN'FAKE = (CPN'Sim.is_disabled, [])\n"::
        (fold_transitions gen_transition [] net),
        "in\n"::
        "(CPN'exec event; ())\n"::
        "end\n"::[]
        ]
    end

    fun gen_controllable ((net, _) : CPN'NetCapture.Net) =
    let
        fun gen_transition
            ({name = page_name, instances, ...} : CPN'NetCapture.Page)
            ({name, id, ...} : CPN'NetCapture.Transition, rest) =
                "| CPN'controllable ("::page_name::"'"::name::" _) =\n"::
                "CPN'transition"::id::".CPN'controllable\n"::rest
    in
        List.concat[
        "fun controllable event =\n"::
        "let\n"::
        "fun CPN'controllable CPN'FAKE = true\n"::
        (fold_transitions gen_transition [] net),
        "in\n"::
        "CPN'controllable event\n"::
        "end\n"::[]
        ]
    end

    fun gentoString ((net, _) : CPN'NetCapture.Net) =
        ["fun toString event = \"\"\n"]

    fun genEvent (net : CPN'NetCapture.Net) =
        List.concat [
        "structure CPNToolsEvent : CPN'EVENT =\n"::
        "struct\n"::
        "open Bind\n"::[],
        gen_enabled net,
        gen_execute net,
        gen_controllable net,
        gentoString net,
        "end\n"::[]
        ]
end
