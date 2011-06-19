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
(* File: xml.sml
 *
 * XML encoding and parsing
 *)

(* FIXME: Validating XML parser in SML: 
 * http://www.informatik.uni-trier.de/~aberlea/Fxp/ *)
(*
signature CPN'XML_Encode = 
    sig
	type element

	val encode: command * parameter list
    end;
*)
structure CPN'XML_Encode = 
struct
    type name = string
    type value = string

    type attribute = name * value
    type attributes = attribute list

    datatype node = Element of { tag:name, attrs:attributes, chlds: node list }
	          | EmptyEl of { tag:name, attrs:attributes }
    type xml = node

    (* local fn *)
    fun attlst2string nil = ""
      | attlst2string ((n,a)::attlst) = 
	" "^n^"=\""^a^"\""^(attlst2string attlst)
	
    fun toString (EmptyEl {tag=name,attrs=attlst}) =
	("<"^name^(attlst2string attlst)^"/>")
      | toString (Element {tag=name,attrs=attlst,chlds=children}) =
	let
	    val res= String.concat
		(map toString children)
	in
	    ("<"^name^(attlst2string attlst)^">"^res^"</"^name^">")
	end

end;

signature CPN'SERIALISE =
sig
    type xml

    val toString : xml -> string
end;

signature CPN'COMMANDS =
sig
    type style

    val aux_create_node {x:int,y:int,s:style} -> id
end;



functor CPN'CreateCommands(structure CPN'Serialise:CPN'SERIALISE) : CPN'COMMANDS =
struct
    structure S = CPN'Serialise

    (* local fn *)
    datatype style =
	POS of {x:int, y:int}
      | SIZE of {w:int, h:int}
      | LINE of {c:rgb_name, w:int, s:lstyle}
      | FILL of {c:rgb_name, s:texture}
    val default_style = [];
    fun execute cmd =
	send(S.toString cmd)

    fun aux_create_node {x,y,s} = execute
	(
	 let
	     val styles= ser_style s
	 in
	     S.EmptyEl {tag="aux_create_node",
			attrs=("x",Int.toString x)::
		              ("y",Int.toString x)::
			      styles}
	 end
	 )
end;

structure CPN'Commands =
    CPN'CreateCommands(structure CPN'Serialise = CPN'XML_Encode);
