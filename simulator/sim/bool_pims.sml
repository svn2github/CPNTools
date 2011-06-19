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
(* File: bool_pims.sml
 *
 * Boolean primary internal multi-set. This representation is specially 
 * optimised for boolean colour sets.
 *)

(* WARNING: Cannot be used with timed cs since col is not defined 
 * in this case. *)
functor CPN'BoolPIMS(structure cs: COLORSET): CPN'PRIMARY_IMS = struct

    structure cs = cs;

    type cs = cs.cs;
	
    local
	val col0 = cs.col 0
	val col1 = cs.col 1

	fun all (n0,n1) = 
	    Misc.I.fold (fn (_,tail) => col0::tail) (0,n0) 
	    (Misc.I.fold (fn (_,tail) => col1::tail) (0,n1) nil)
    in
        type 'a ims = int * int * int * int

	val cmp = cs.cmp

	fun init (ims, ems) = let
	    fun cf (col,(n0,n1)) =
		if cs.lt(col,col1) then (n0+1,n1) else (n0,n1+1)
	    val (n0,n1) = foldr cf (0,0) ems
	in
	    ims:= (n0,n1,0,0)
	end

        fun empty () = (0,0,0,0);

        fun is_empty (0,0,0,0) = true
          | is_empty _ = false

        fun insert (ims as ref((n0,n1,r0,r1): 'a ims), col) = 
	    if cs.lt(col,col1) then ims:= (n0+1,n1,r0,r1) else ims:= (n0,n1+1,r0,r1)

        fun addto (_, nil: cs.cs ms) = ()
          | addto (ims, item::rms) = insert(ims,item) before addto (ims,rms)

        fun delete (ims as ref ((n0,n1,r0,r1): 'a ims), col) =
	    if cs.lt(col,col1) then
		if n0+r0>0 then ims:=(n0+r0-1,n1+r1,0,0) else raise Subtract
	    else
		if n1+r1>0 then ims:=(n0+r0,n1+r1-1,0,0) else raise Subtract

        fun subfrom (_, nil: cs.cs ms) = ()
          | subfrom (ims,item::rms) = delete(ims,item) before subfrom(ims,rms)

        fun cf ((n0,n1,r0,r1): 'a ims, col) = if cs.lt(col,col1) then n0+r0 else n1+r1

	fun member ((n0,n1,r0,r1): 'a ims, col) = if cs.lt(col,col1) then n0+r0>0 else n1+r1>0

        fun subset ((n0,n1,r0,r1): 'a ims, ems) = let
	    fun count (x::xs,(m0,m1)) = 
		if cs.lt(x,col1) then count(xs,(m0+1,m1))
		else count(xs,(m0,m1+1)) 
	      | count (nil,m) = m
	    val (m0,m1) = count (ems,(0,0))
	in
	    m0<=n0+r0 andalso m1<=n1+r1
	end

	fun size ((n0,n1,r0,r1): 'a ims) = n0+n1+r0+r1

        fun extract (n0,n1,r0,r1) = (n0+n1+r0+r1, all(n0+r0,n1+r1))

	fun fold _ (0,0,0,0) z = z
	  | fold f (n0,n1,r0,r1) z = List.foldl f z (all(n0+r0,n1+r1))

        fun filter f (n0,n1,r0,r1) = 
	    all (if f col0 then n0+r0 else 0, if f col1 then n1+r1 else 0)
		    
        fun collect cmp (n0,n1,r0,r1) =
	    all (case cmp col0 of EQUAL => n0+r0 | _ => 0,
		 case cmp col1 of EQUAL => n1+r1 | _ => 0)
 
	fun init_res (ims as ref((n0,n1,r0,r1): 'a ims)) = 
	    (ims:= (n0+r0,n1+r1,0,0); n0+r0+n1+r1)

        fun next exn ((0,0,_,_)) _ = raise exn
	  | next _ (ims as (n0,0,_,_)) _ =
	    (col0,(0,0,0,0))
	  | next _ (ims as (0,n1,_,_)) _ =
	    (col1,(0,0,0,0))
	  | next _ (ims as (n0,n1,_,_)) _ =
	    if cs.lt(cs.ran(),col1) then (col0,(0,n1,0,0))
	    else (col1,(n0,0,0,0))

        fun random_res exn (ref(0,0,_,_)) = raise exn
	  | random_res exn (ims as ref(n0,0,r0,r1)) =
	    (ims:= (n0-1,0,r0+1,r1); col0)
	  | random_res exn (ims as ref(0,n1,r0,r1)) =
	    (ims:= (0,n1-1,r0,r1+1); col1)
	  | random_res _ (ims as ref(n0,n1,r0,r1)) =
	    if cs.lt(cs.ran(),col1) then (ims:= (n0-1,n1,r0+1,r1); col0)
	    else (ims:= (n0,n1-1,r0,r1+1); col1)

	fun ms_random_res _ exn ims = random_res exn ims

	fun check_res exn f ims = let
	    val col = random_res exn ims
	in
	    if f col then col else check_res exn f ims
	end

	fun res_col (ims as ref(n0,n1,r0,r1),col) = 
	    if cs.lt(col,col1)
	    then ims:= (0,n1,r0+n0,r1)
	    else ims:= (n0,0,r0,r1+n1)
    end
end;
