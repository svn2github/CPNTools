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
(* File: ims-sig.sml
 *
 * Interface description for internal multi-set data structures.
 * The primary IMS is used for untimed places or the ready tokens on
 * timed places.
 * The secondary IMS is used for making a priority queue sorted by
 * the token time stamps. This is used for waiting tokens on timed places.
 *)

signature CPN'PRIMARY_IMS = sig

    structure cs: COLORSET;

    type cs;
    sharing type cs = cs.cs;
	
    type 'a ims;
	    
    val cmp     : cs * cs -> order;
	
    val init    : cs ims ref * cs CPN'MS.ms -> unit;
    val empty   : unit -> cs ims;
    
    (* Return true if the ims (including reserved elements) is empty *)
    val is_empty: cs ims -> bool;

    val insert  : cs ims ref * cs -> unit;
    val addto   : cs ims ref * cs CPN'MS.ms -> unit;
	
    (* Delete exactly the given element. *)
    val delete  : cs ims ref * cs -> unit;
    (* Delete exactly those elements given as argument. *)
    val subfrom : cs ims ref * cs CPN'MS.ms -> unit;
	
    (* Return number of elements in the ims including reserved elements *)
    val size    : cs ims -> int;
    (* Return pair: (size of the ims) and  all elements in ims including 
     * reserved elements *)
    val extract : cs ims -> (int * cs CPN'MS.ms);
	
    (* Number of instances of color in ims (including reserved elements) *)
    val cf      : cs ims * cs -> int;
    (* Return true if cs is in ims (including reserved elements) *)
    val member  : cs ims * cs -> bool;
    (* Return true if ms is a subset of ims (including reserved elements) *)
    val subset  : cs ims * cs CPN'MS.ms -> bool;
	
    (* Apply function to all elements in the ims including reserved elements *)
    val fold    : (cs * 'b -> 'b) -> cs ims -> 'b -> 'b;

    (* Apply function f to all elements x (incl. reserved elements) in ims, 
     * return list containing elements for which f x evaluated to true *)
    val filter  : (cs -> bool) -> cs ims -> cs CPN'MS.ms;

    (* Returns those elements (including reserved elements) which 
     * are EQUAL as specified by the
     * supplied compare function. Traversal of ims is guided by the 
     * compare function. The ims is not modified during traversal. *)
    val collect : (cs -> order) -> cs ims -> cs CPN'MS.ms; 
	
    (* Initialize the reservation handling, update the ims *)
    val init_res   : cs ims ref -> int;

    (* Pick a random color from the ims, and return the color and the ims 
     * without all instances of that element. Ingore all reservations, 
     * and reserve no elements.  Raise exn if empty or all reserved. *)
    val next : exn -> cs ims -> cs option -> cs * cs ims;

    (* Return a random color and reserve it, raise exn if empty or 
     * all reserved. *)
    val random_res : exn -> cs ims ref -> cs;

    (* Return a random color for a ms-exp and reserve it. *)
    val ms_random_res : (cs list ref * int ref * int) -> exn -> cs ims ref -> cs;
    (* Keep reserving until a random colour fulfilling the function is found, 
     * raise exn if empty or all reserved. *)
    val check_res  : exn -> (cs -> bool) -> cs ims ref -> cs;

    (* Reserve all of the unreserved instances of the color in ims *)
    val res_col : (cs ims ref * cs) -> unit
	
end;

signature CPN'SECONDARY_IMS = sig

    structure pims: CPN'PRIMARY_IMS;
    structure Time: CPN'TIME

    type cs;
    sharing type cs = pims.cs;
	
    type 'a ims;
		
    val time      : cs -> Time.time;
	
    val init      : cs ims ref * cs CPN'MS.ms -> unit;
    val empty     : unit -> cs ims;
    val is_empty  : cs ims -> bool;
	
    val insert    : cs ims ref * cs -> unit;
    val addto     : cs ims ref * cs CPN'MS.ms -> unit;
	
    val size      : cs ims -> int;
    val extract   : cs ims -> (int * cs CPN'MS.ms);
    val fold      : (cs * 'b -> 'b) -> cs ims -> 'b -> 'b;
	
    val min       : cs ims -> Time.time option;
    val deleteto  : cs ims ref * Time.time -> cs CPN'MS.ms;
    val deleteall : cs ims ref -> cs CPN'MS.ms;
end;
