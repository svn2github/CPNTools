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
(* File: tables-sig.sml
 *
 * Interface description for instance related tables.
 *)

signature CPN'REPTABLE = sig

    structure Time: CPN'TIME

    type order_item;
    type offset_item;
    type dep_item;

    val order_table : (CPN'Id.id,order_item) HashTable.hash_table;
    val offset_table : (CPN'Id.id,offset_item) HashTable.hash_table;
    val dep_table : (CPN'Id.id,dep_item) HashTable.hash_table;

    val append_order : CPN'Id.id * CPN'Id.id * int list -> unit
    val append_offset : CPN'Id.id * CPN'Id.id * Time.time option -> unit
    val append_dep : CPN'Id.id * CPN'Id.id -> unit

    val find_order : CPN'Id.id -> order_item;
    val find_offset : CPN'Id.id -> offset_item;
    val find_dep : CPN'Id.id -> dep_item;

    val peek_order : CPN'Id.id -> order_item option;
    val peek_offset : CPN'Id.id -> offset_item option;
    val peek_dep : CPN'Id.id -> CPN'Id.id list option;

    val get_order :  CPN'Id.id -> int list list
    val get_offset :  CPN'Id.id -> Time.time option list;
    val get_dep :  CPN'Id.id -> CPN'Id.id list;

    val rm_order : CPN'Id.id -> unit;
    val rm_offset : CPN'Id.id -> unit;
    val rm_dep : CPN'Id.id -> unit;

    val remove : CPN'Id.id -> unit;
    val rm_trans : CPN'Id.id * CPN'Id.id -> unit;
end


signature CPN'INSTTABLE = sig

    structure RepTable: CPN'REPTABLE

    datatype offset_type =
	one_offset of string
      | max_offset of string
      | var_offset
      | un_timed

    type transition_item 
    type substitution_item 
    type page_item 
    type place_item 

    val transition_table: (CPN'Id.id,transition_item) HashTable.hash_table
    val substitution_table: (CPN'Id.id,substitution_item) HashTable.hash_table
    val page_table: (CPN'Id.id,page_item) HashTable.hash_table
    val place_table: (CPN'Id.id,place_item) HashTable.hash_table

    (* Create the instance tables from scratch. *)
    val init: unit -> unit
    val clean: unit -> unit

    val no_of_tis: int ref

    val get_no_of_inst: CPN'Id.id -> int

    val get_socket_inst_list: CPN'Id.id -> (CPN'Id.id * int) list
					   
    val get_inst_cons: ((CPN'Id.id*int)*(CPN'Id.id*int) list) -> (CPN'Id.id*int) list

    val get_t_index: CPN'Id.id -> int * int

    val get_ti_index: CPN'Id.id * int -> int

    val get_ti_indices: unit -> (CPN'Id.id * (int * int)) list

    val get_order: CPN'Id.id -> int list

    val get_offset: CPN'Id.id -> offset_type

    val get_dep_trans: (CPN'Id.id * int) -> int list

    val get_dep_list: (CPN'Id.id * int) -> int list

    val get_input_place_ids: CPN'Id.id -> CPN'Id.id list
    val get_inhibitor_place_ids: CPN'Id.id -> CPN'Id.id list

    val get_output_place_instances: CPN'Id.id * int -> (CPN'Id.id * int) list

    val get_sur_places: CPN'Id.id * int -> (CPN'Id.id * int) list

    val get_page_structure: unit -> (CPN'Id.id * (CPN'Id.id * CPN'Id.id * int) option list) list

end
