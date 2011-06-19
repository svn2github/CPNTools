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
functor CPN'AlphaState (structure CPN'NetCapture : CPN'NETCAPTURE): sig
    val genAlphaState : CPN'NetCapture.Net -> string list
end = struct

exception StateGenerationError of string

fun gen_type (net : CPN'NetCapture.Page list) = let
    fun generate ({id, places, subpages, name, ...}: CPN'NetCapture.Page,
		  rest) = let
        fun gen_subpage ({transition_name, transition_id,
			  page_id, page_name, instances}, rest) =
            transition_name::" : 'a "::page_name::",\n"::rest
        fun gen_places () =
            List.foldl (fn ({name, id, port, ...}, rest) =>
			   if port
			   then rest
			   else name::": 'a"::",\n"::rest)
                       [] places
        val contents = List.foldl gen_subpage (gen_places ()) subpages
        val contents = List.rev contents
        val contents = List.rev (List.tl contents handle _ => [])
    in
        (List.concat ["type 'a "::name::" = {\n"::[],
		      contents,
		      "\n}\n"::[]])::rest
    end
    fun generate_top net = let
        fun generate_one ({name, prime = 0, ...} : CPN'NetCapture.Page, rest) =
            rest
          | generate_one ({name, prime = 1, ...}, rest) =
            ",\n"::name::" : 'a "::name::rest
          | generate_one _ =
            raise StateGenerationError "prime multiplicity not 0 or 1"
        val contents = "\n}\n"::(List.tl (List.foldl generate_one ["\n"] net))
        val contents = List.rev contents
    in
	
        "type 'a state = {"::contents
    end
in
    List.concat (
    List.foldr generate [generate_top net] net)
end

fun genAlphaState (net, _) =
    List.concat [
    "structure CPNToolsAlphaState =\n"::
    "struct\n"::[],
    gen_type net,
    ["end\n"]
    ]
    
end
