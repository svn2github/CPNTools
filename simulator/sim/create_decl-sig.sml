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
(* File: create_decl-sig.sml
 *
 * Interface description for check and creation of declarations.
 *)

signature CPN'DECL = sig

    type parameters

    datatype global_decl =
	unit_cs       of string 
      | bool_cs       of (string * string)
      | int_cs        of (string * string)
      | intinf_cs     of (string * string)
      | real_cs       of (string * string)
      | char_cs       of (string * string)
      | string_cs     of {char: string * string, length: string * string}
      | enum_cs       of string list
      | index_cs      of {idx: string, over: string * string}
      | list_cs       of {cs: string, length: string * string}
      | product_cs    of string list
      | record_cs     of (string * string) list
      | union_cs      of (string * string) list
      | funsubset_cs  of {cs: string, subset: string}
      | listsubset_cs of {cs: string, subset: string list}
      | time_cs
      | alias_cs      of string
      | globref       of {name: string, exp: string}
      | usefile       of string
      | sml_code      of string
      | append_var    of string * string list
      | append_msvar  of string * string list
      | append_alias  of string * string list
      | append_decl   of string * string list
      | channel       of (string * string)

    datatype local_decl =
	pageref       of {name: string, exp: string, page: CPN'Id.id}
      | instref       of {name: string, exp: string, page: CPN'Id.id}

    val out_of_range : string

    val compile_decl : CPN'Id.id * string list * bool  ->
	CPN'Id.id * string * (string list * string list)

    val find : string * (string * string) list -> string

    val create_global : (CPN'Id.id * global_decl * 
			 {name   : string,
			  timed  : bool,
			  var    : string list,
			  msvar  : string list,
			  alias  : string list,
			  declare: string list} option) list
	-> (CPN'Id.id * string * (string list * string list)) list

    val create_local : (CPN'Id.id * local_decl) list
	-> (CPN'Id.id * string * (string list * string list)) list

    val create_vars : string * string * string list -> unit

    val create_msvars : string * string * string list -> unit

    val append_all: string -> unit;

    val reserved_words : string list;
end
