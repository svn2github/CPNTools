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
(* File: unit_pims.sml
 *
 * Unit primary internal multi-set. This representation is specially 
 * optimised for unit colour sets.
 *)

(* WARNING: Cannot be used with timed cs since col is not defined 
 * in this case. *)
functor CPN'UnitPIMS(structure cs: COLORSET): CPN'PRIMARY_IMS = struct

    structure cs = cs;

    type cs = cs.cs;
	
    type 'a ims = int * int

    local
        val unit = cs.base 
	fun all n = Misc.I.fold (fn (_,tail) => unit::tail) (0,n) nil
    in

	fun cmp (_,_) = EQUAL;

	fun init (ims, ems) = 
	    ims:= (CPN'MS.size ems, 0)

        fun empty () = (0,0);

	fun size ((n,r): 'a ims) = n+r

        fun is_empty (0,0) = true
          | is_empty _ = false

        fun insert (ims as ref((n,r): 'a ims), _) = 
	    ims:= (n+1,r)

        fun addto (_, nil) = ()
          | addto (ims, item::rms) = insert(ims,item) before addto (ims,rms)

        fun delete (ims as ref ((n,r): 'a ims), _) =
            if size(n,r)>0 then ims:=(n+r-1,0) else raise Subtract

        fun subfrom (_, nil) = ()
          | subfrom (ims,item::rms) = delete(ims,item) before subfrom(ims,rms)

        fun cf ((n,r): 'a ims, _) = n+r

	fun member ((n,r): 'a ims, _) = n+r>0;

        fun subset ((n,r): 'a ims, ms) = length ms <= n+r

        fun extract (n,r) = (n+r, all(n+r))

	fun fold _ (0,0) z = z
	  | fold f (n,r) z = List.foldl f z (all(n+r))

        fun filter f (n,r) = if (f unit) then all(n+r)  else nil

        fun collect f (0,0) = nil
          | collect f (n,r) = 
	    case (f unit) of 
		EQUAL => all(n+r)
	      | _ => nil

	fun init_res (ims as ref(n,r)) = (ims:= (n+r,0); n+r)

	fun next exn ((0,_): cs ims) _ = raise exn
	  | next _ (ims as (n,_)) _ = (unit,(0,0))

        fun random_res exn (ref ((0,_): cs ims)) = raise exn
	  | random_res _ (ims as ref(n,r)) = (ims:=(n-1,r+1); unit) 

	fun ms_random_res _ exn ims = random_res exn ims

	fun check_res exn _ (ref(0,_)) = raise exn
	  | check_res exn f (ims as ref(n,r)) = 
	    if (f unit) then (ims:=(0,n+r); unit) else raise exn

	fun res_col (ims as ref(n,r), _ ) = ims := (0,r+n)
    end
end;
