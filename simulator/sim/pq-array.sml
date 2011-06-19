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
(* File: pq-array
 *
 * Array implementation of a priority queue. 
 * Used to hold 'maybe_readies' during simulation.
 *
 * Invariant in heap: sons are always >= their father
 *)

functor MakeArrayPQ (Time: CPN'TIME) = struct

local
    open Array;
	 
    fun lt ((a,_),(b,_)) = Time.lt(a,b);
	    
    fun father i   = (i-1) div 2
    fun lson i     = 2*i + 1
    fun rson i     = 2*i + 2
	
    fun swap (heap,index,i,j) =
	let
	    val itemi as (_,ki) = sub(heap,i);
	    val itemj as (_,kj) = sub(heap,j);
	in
	    (update(heap,i,itemj); 
	     update(heap,j,itemi);
	     update(index,ki,j);
	     update(index,kj,i))
	end;
	
    fun down (heap,index,last) node =
	if (lson node) < last then 
	   (* there is two sons *)
	   if lt(sub(heap,lson node),sub(heap,rson node)) then
	       (* the left son is smallest, check him *)
	       if lt(sub(heap,lson node),sub(heap,node)) then
		   (* the son is less than the father, swap and continue *)
		   (swap(heap,index,node,lson node);
		    down (heap,index,last) (lson node))
	       else ()
	   else
	       (* the right son is smallest, check him *)
	       if lt(sub(heap,rson node),sub(heap,node)) then
		   (* the son is less than the father, swap and continue *)
		   (swap(heap,index,node,rson node);
		    down (heap,index,last) (rson node))
	       else ()
	else if (lson node) = last then
	    (* there is only one son, the left *)
	    if lt(sub(heap,lson node),sub(heap,node)) then
		(* the son is less than the father, swap *)
		swap(heap,index,node,lson node)
	    else ()
	else
	    (* there is no sons *)
	    ()

    fun up _ 0 = ()
      | up (heap,index,last) node = 
	if lt(sub(heap,node),sub(heap,father node)) then
	    (* the son is less than father, swap *)
	    (swap (heap,index,node,father node); 
	     (* push the new father down *)
	     down (heap,index,last) (father node);
	     (* push the new father up *)
	     up (heap,index,last) (father node))
	else ()
	    
	    
in
    type pq = {heap: (Time.time * int) array,
	       last: int,
	       index: int array};

    exception EmptyPQ;

    fun create n = 
	{heap= tabulate(n,fn _ => (Time.null,~1)), 
	 last= ~1,
	 index= tabulate(n,fn _ => ~1)}
	
    fun insert (pq as ref({heap,last,index}: pq), time, i) = let
	val last = last+1
		   
	val _ = CPN'debug ("PQ inserting: "^(Time.toString time))

    in
	(update (heap,last,(time,i));
	 update (index,i,last);
	 up (heap,index,last) last;
	 pq:={heap=heap,index=index,last=last})
    end;

    fun min ({last= ~1,...}: pq) = raise EmptyPQ
      | min ({heap,...}) = #1(sub(heap,0));

    fun deleteto (ref({last= ~1,...}: pq),_) = nil
      | deleteto (pq as ref({heap,last,index}), until) = let

	    val _ = CPN'debug ("PQ deleteto: "^(Time.toString until))

	val (time,i) = sub(heap,0);
	val item as (_,j) = sub(heap,last);
    in
	if Time.leq(time,until) then
	    (update (heap,0,item);
	     update (index,j,0);
	     down (heap,index,last-1) 0;
	     pq:= {heap=heap,last=last-1,index=index};
	     i::deleteto(pq,until))
	else
	    nil
    end

    fun delete (pq as ref({heap,last,index}), i) = let

	val _ = CPN'debug ("PQ delete: "^(Int.toString i))

	val j = sub(index,i);
	val item as (_,k) = sub(heap,last);
    in
	(update (heap,j,item);
	 update (index,k,j);
	 down (heap,index,last-1) j;
	 up (heap,index,last-1) j;
	 pq:= {heap=heap,last=last-1,index=index})
    end
end (* local *)
end (* structure PQ *)
