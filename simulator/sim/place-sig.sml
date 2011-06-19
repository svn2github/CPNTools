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
(* File: place-sig.sml
 *
 * Interface description for place data structures.
 *)

signature CPN'PLACE = sig

    structure ims: CPN'PRIMARY_IMS
	
    val init_mark: ims.cs CPN'MS.ms ref

    val mark	: int -> ims.cs ims.ims ref
			 
    val set_init_mark: unit -> unit
    val change_no_of_inst: int -> unit
    val change_assignments: ims.cs ims.ims ref list -> unit
    val init	: int -> int
    val size	: int -> int
    val print	: int -> string
    val addto	: int -> ims.cs CPN'MS.ms -> unit
    val subfrom	: int -> ims.cs CPN'MS.ms -> unit
    val get	: int -> ims.cs CPN'MS.ms
    val set	: int -> ims.cs CPN'MS.ms -> unit
end

signature CPN'TIMEDPLACE = sig
    
    structure sims: CPN'SECONDARY_IMS;

    val init_mark: sims.pims.cs CPN'MS.ms ref

    (* Function mark returns the tokens that are READY. 
     * Returns all tokens with time stamps less than or
     * equal to model time. May also return tokens 
     * with time stamps greater than model time 
     * if an arc from the place to a transition
     * has a timed arc inscription, e.g. x@+ts *)
    val mark	: int -> sims.pims.cs sims.pims.ims ref
    val wait	: int -> sims.cs sims.ims ref
	
    val set_init_mark: unit -> unit
    val change_no_of_inst: int -> unit
    val change_assignments: (sims.pims.cs sims.pims.ims ref list)
                          * (sims.cs sims.ims ref list)
                          * (((int -> sims.Time.time option) * int) list)
                          * (((int -> int) * int) list) -> unit

    (* returns the smallest time stamp that is greater than model time
     *)
    val next_time: int -> sims.Time.time option
    val init	: int -> int
    val size	: int -> int
    val print	: int -> string
    val addto	: int -> sims.cs CPN'MS.ms -> unit
    val subfrom	: int -> sims.cs CPN'MS.ms -> unit
    val get	: int -> sims.cs CPN'MS.ms
    val set	: int -> sims.cs CPN'MS.ms -> unit
end

signature CPN'PLACES = sig

    functor MakePlace(structure ims: CPN'PRIMARY_IMS; 
		      val no_of_inst: int): CPN'PLACE

    functor MakeFusion(structure ims: CPN'PRIMARY_IMS): CPN'PLACE

    functor MakePort(structure ims: CPN'PRIMARY_IMS;
                     val markings: ims.cs ims.ims ref list): CPN'PLACE

    functor MakeTimedPlace(structure sims: CPN'SECONDARY_IMS;
			   val no_of_inst: int
			   and offset: sims.Time.time): CPN'TIMEDPLACE

    functor MakeTimedFusion(structure sims: CPN'SECONDARY_IMS;
			    val offset: sims.Time.time): CPN'TIMEDPLACE

    functor MakeTimedPort(structure sims: CPN'SECONDARY_IMS;
			  val marks: sims.pims.cs sims.pims.ims ref list
			  and waits: sims.cs sims.ims ref list
			  and next_times: ((int -> sims.Time.time option) * int) list
			  and inits: ((int -> int) * int) list): CPN'TIMEDPLACE

    functor MakeTimedPlaceM(structure sims: CPN'SECONDARY_IMS;
			    val no_of_inst: int
			    and max_offset: sims.Time.time): CPN'TIMEDPLACE 

    functor MakeTimedFusionM(structure sims: CPN'SECONDARY_IMS;
			     val max_offset: sims.Time.time): CPN'TIMEDPLACE

    functor MakeTimedPlaceN(structure sims: CPN'SECONDARY_IMS;
			    val no_of_inst: int): CPN'TIMEDPLACE

    functor MakeTimedFusionN(structure sims: CPN'SECONDARY_IMS): CPN'TIMEDPLACE

    val print_mark: CPN'Id.id * int -> string
	    
    val size_mark: CPN'Id.id * int -> int

    val init_mark_funs: (unit -> unit) list ref

    val set_init_mark: (unit -> unit)

    val change_mark: CPN'Id.id * int * bool -> unit
					       
    val instances: (CPN'Id.id,{print: int -> string, size: int -> int}) HashTable.hash_table
end
