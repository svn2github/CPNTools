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
(* File: place.sml
 *
 * Place data structures.
 *)

(*FIXME: Remove nesting of functors within structure*)
structure CPN'Places: CPN'PLACES = struct
    open Array

functor MakePlace(structure ims: CPN'PRIMARY_IMS; 
		  val no_of_inst: int): CPN'PLACE =  struct

    structure ims = ims;

    fun gen_markings noi = tabulate(noi,fn _ => ref(ims.empty()))
    val cur_no_of_inst= ref(no_of_inst)

    val marking = ref(gen_markings no_of_inst)

    val change_assignments = fn _ => raise InternalError "Cannot change port assignments on a place"

    fun change_no_of_inst noi =
	(marking:= (gen_markings noi);
	 cur_no_of_inst:= noi
	 )

    val init_mark = ref(nil: ims.cs CPN'MS.ms)
		    
    fun mark i = sub(!marking,i-1)

    fun set_init_mark _ =
	(CPN'inst:= 1;  (*FIXME:remove CPN'inst?*)
	 while !CPN'inst <= !cur_no_of_inst do
	     (ims.init(mark(!CPN'inst), !init_mark);
	      inc CPN'inst))

    fun init i = ims.init_res(mark i)

    fun size i = ims.size(!(mark i))

    fun print i = let
	val (size,ems) = ims.extract(!(mark i))
    in
	 ims.cs.mkstr_ms ems
    end

    fun addto i ems = ims.addto(mark i, ems)
	 
    fun subfrom i ems = ims.subfrom(mark i, ems)
	 
    fun get i = ims.fold (op ::) (!(mark i)) nil
	 
    fun set i ems = ims.init(mark i, ems) 
end


functor MakeFusion(structure ims: CPN'PRIMARY_IMS): CPN'PLACE = struct

    structure ims = ims;

    val cur_no_of_inst= ref(1)

    val marking = ref(ims.empty())

    val init_mark = ref(nil: ims.cs CPN'MS.ms)

    val change_no_of_inst = fn (noi:int) => raise InternalError "Cannot change no of inst on a fusion place"
    val change_assignments = fn _ => raise InternalError "Cannot change port assignments on a fusion place"

    fun mark _ = marking

    fun set_init_mark _ =
	(* Fusion place is initialize as instance 1 *)
	(CPN'inst:= 1;  (*FIXME:remove CPN'inst?*)
	 ims.init(mark 1, !init_mark))

    fun init i = ims.init_res (mark i)

    fun size i = ims.size (!(mark i))

    fun print i = let
	val (size,ems) = ims.extract(!(mark i))
    in
	 ims.cs.mkstr_ms ems
    end
	 
    fun addto i ems = ims.addto(mark i, ems)

    fun subfrom i ems = ims.subfrom(mark i, ems)

    fun get i = ims.fold (op ::) (!(mark i)) nil

    fun set i ems = ims.init(mark i, ems) 
end


functor MakePort(structure ims: CPN'PRIMARY_IMS;
		 val markings: ims.cs ims.ims ref list): CPN'PLACE = struct

    structure ims = ims;

    val cur_no_of_inst= ref(List.length markings)

    val marking = ref(fromList markings)

    val change_no_of_inst = fn (noi:int) => raise InternalError "Cannot change no of inst on a port place"
    fun change_assignments assignments =
	(marking:= (fromList assignments);
	 cur_no_of_inst:= (List.length assignments)
	 )

    val init_mark = ref(nil: ims.cs CPN'MS.ms)
 
    fun mark i = sub(!marking,i-1)
	
    fun set_init_mark _ = ()

    fun init i = ims.init_res(mark i)

    fun size i = ims.size(!(mark i))
	
    fun print i = let
	val (size,ems) = ims.extract(!(mark i))
    in
	ims.cs.mkstr_ms ems
    end

    fun addto i ems = ims.addto(mark i, ems)
	
    fun subfrom i ems = ims.subfrom(mark i, ems)
	
    fun get i = ims.fold (op ::) (!(mark i)) nil
	
    fun set i ems = ims.init(mark i, ems) 
end


    (*************** Timed places with only one known offset ***************)

(*FIXME: Introduce runtime change number of instances as above*)
functor MakeTimedPlace(structure sims: CPN'SECONDARY_IMS;
		       val no_of_inst: int
		       and offset: sims.Time.time): CPN'TIMEDPLACE = struct

    structure sims = sims;
    structure Time = sims.Time;

    (* locals *)
    fun gen_ready noi = tabulate(noi,fn _ => ref(sims.pims.empty()))
    fun gen_waiting noi = tabulate(noi,fn _ => ref(sims.empty()))
    val cur_no_of_inst= ref(no_of_inst)

    val ready= ref(gen_ready no_of_inst)
    val waiting= ref(gen_waiting no_of_inst)

    val change_assignments = fn _ => raise InternalError "Cannot change port assignments on a timed place"
    fun change_no_of_inst noi =
	(ready:= (gen_ready noi);
	 waiting:= (gen_waiting noi);
	 cur_no_of_inst:= noi
	 )

    val init_mark = ref(nil: sims.pims.cs CPN'MS.ms)

    fun mark i = sub(!ready,i-1)
    fun wait i = sub(!waiting,i-1)
	    
    fun set_init_mark _ =
	(CPN'inst:= 1;   (*FIXME:remove CPN'inst*)
	 while !CPN'inst <= no_of_inst do
	     (sims.pims.init(mark (!CPN'inst), nil);
	      sims.init(wait (!CPN'inst), !init_mark); 
	      inc CPN'inst))
    
    (* Suggests next time there could be ready tokens. Takes into account
     * the offset from input arc.
     * We only look at sims because we only have exactly one arc. *)
    fun next_time i = 
	let
	    val min_waiting_time= sims.min (!(wait i))
	in
	    case min_waiting_time of
		NONE => NONE
	      | SOME t => SOME(Time.sub(t,offset))
	end
	    
    fun init i =  
	(sims.pims.addto(mark i,
			 sims.deleteto(wait i,
				       Time.add(Time.time(),offset)));
	 sims.pims.init_res (mark i))
	
    fun size i = 
	 sims.size(!(wait i)) +  sims.pims.size(!(mark i))

    fun print i = let
	val (rsize,rmark) = sims.pims.extract (!(mark i))
	val (wsize,wmark) = sims.extract (!(wait i))
    in
	 sims.pims.cs.mkstr_ms (List.@(rmark,wmark))
    end

    fun addto i ems = sims.addto(wait i, ems)
	 
    fun subfrom i ems = sims.pims.subfrom(mark i, ems)

    fun get i = 
	sims.pims.fold (op ::) (!(mark i)) (sims.fold (op ::) (!(wait i)) nil)

    fun set i ems = (sims.pims.init(mark i, nil); sims.init(wait i, ems))
end


functor MakeTimedFusion(structure sims: CPN'SECONDARY_IMS;
			val offset: sims.Time.time): CPN'TIMEDPLACE = struct

    structure sims = sims;
    structure Time = sims.Time;
	    
    val ready= ref(sims.pims.empty())
    val waiting= ref(sims.empty())
	
    val change_assignments = fn _ => raise InternalError "Cannot change port assignments on a timed fusion place"
    val change_no_of_inst = fn _ => raise InternalError "Cannot change no of inst on a timed fusion place"

    val init_mark = ref(nil: sims.pims.cs CPN'MS.ms)

    fun mark _ = ready
    fun wait _ = waiting
	
    fun set_init_mark _ =
	(* Fusion place is initialize as instance 1 *)
	(CPN'inst:= 1;   (*FIXME:remove CPN'inst*)
	 sims.pims.init(mark 1, nil);
	 sims.init(wait 1, !init_mark)) 

    fun next_time i =
	let
	    val min_waiting_time= sims.min (!(wait i))
	in
	    case min_waiting_time of
		NONE => NONE
	      | SOME t => SOME(Time.sub(t,offset))
	end

    fun init i =  
	(sims.pims.addto(mark i,
			 sims.deleteto(wait i,
				       Time.add(Time.time(),offset)));
	 sims.pims.init_res (mark i))

    fun size i = sims.size(!(wait i)) + sims.pims.size(!(mark i))

    fun print i = let
	val (rsize,rmark) = sims.pims.extract (!(mark i))
	val (wsize,wmark) = sims.extract (!(wait i))
    in
	sims.pims.cs.mkstr_ms (List.@(rmark,wmark))
    end

    fun addto i ems = sims.addto(wait i, ems)
	
    fun subfrom i ems = sims.pims.subfrom(mark i, ems)
	
    fun get i = 
	sims.pims.fold (op ::) (!(mark i)) (sims.fold (op ::) (!(wait i)) nil)

    fun set i ems = (sims.pims.init(mark i, nil); sims.init(wait i, ems))
end


functor MakeTimedPort(structure sims: CPN'SECONDARY_IMS;
		      val marks: sims.pims.cs sims.pims.ims ref list
		      and waits: sims.cs sims.ims ref list
		      and next_times: ((int -> sims.Time.time option) * int) list
		      and inits: ((int -> int) * int) list): CPN'TIMEDPLACE = struct

    structure sims = sims
    structure Time = sims.Time

    val cur_no_of_inst= ref(List.length marks)
    val ready = ref(fromList marks)
    val waiting = ref(fromList waits)
    val next_times_store = ref(next_times)
    val inits_store = ref(inits)
	
    val init_mark = ref(nil: sims.pims.cs CPN'MS.ms)
	
    fun mark i = sub(!ready,i-1)
    fun wait i = sub(!waiting,i-1)
	
    val change_no_of_inst = fn (noi:int) => raise InternalError "Cannot change no of inst on a timed port place"
    fun change_assignments (m_assigns,w_assigns,nt,i) =
	(ready:= (fromList m_assigns);
	 waiting:= (fromList w_assigns);
	 next_times_store:= nt;
	 inits_store:= i;
	 cur_no_of_inst:= (List.length m_assigns)
	 )

    fun set_init_mark _ = ()
	
    fun next_time i = let
	val (next_time',i') = List.nth(!next_times_store,i-1)
	    handle _ => raise InternalError ("Wrong instance number "^
					       (Int.toString i)^
					       " used at a port")
    in
	next_time' i'
    end

    fun init i = let
	val (init',i') = List.nth(!inits_store,i-1)
	    handle _ => raise InternalError ("Wrong instance number "^
					       (Int.toString i)^
						   " used at a port")
    in
	init' i'
    end

    fun size i = 
	sims.size(!(wait i)) + sims.pims.size(!(mark i))

    fun print i = let
	val (rsize,rmark) = sims.pims.extract (!(mark i))
	val (wsize,wmark) = sims.extract (!(wait i))
    in
	sims.pims.cs.mkstr_ms (List.@(rmark,wmark))
    end

    fun addto i ems = sims.addto(wait i, ems)
	
    fun subfrom i ems = sims.pims.subfrom(mark i, ems)
	
    fun get i = 
	sims.pims.fold (op ::) (!(mark i)) (sims.fold (op ::) (!(wait i)) nil)

    fun set i ems = (sims.pims.init(mark i, nil); sims.init(wait i, ems))
end


(************ Timed places with multiple knowns offsets ************)

functor MakeTimedPlaceM(structure sims: CPN'SECONDARY_IMS;
			val no_of_inst: int
			and max_offset: sims.Time.time): CPN'TIMEDPLACE = struct

    structure sims = sims;
    structure Time= sims.Time;

    (* locals *)
    fun gen_ready noi = tabulate(noi,fn _ => ref(sims.pims.empty()))
    fun gen_waiting noi = tabulate(noi,fn _ => ref(sims.empty()))
    val cur_no_of_inst= ref(no_of_inst)

    val ready= ref(gen_ready no_of_inst)
    val waiting= ref(gen_waiting no_of_inst)

    val change_assignments = fn _ => raise InternalError "Cannot change port assignments on a timed place M"
    fun change_no_of_inst noi =
	(ready:= (gen_ready noi);
	 waiting:= (gen_waiting noi);
	 cur_no_of_inst:= noi
	 )

    val init_mark = ref(nil: sims.pims.cs CPN'MS.ms)

    fun mark i = sub(!ready,i-1)
    fun wait i = sub(!waiting,i-1)

    fun set_init_mark _ =
	(CPN'inst:= 1;   (*FIXME:remove CPN'inst*)
	 while !CPN'inst <= no_of_inst do
	     (sims.pims.init(mark (!CPN'inst), nil);
	      sims.init(wait (!CPN'inst), !init_mark); 
	      inc CPN'inst))

    (* This version is more complicated than for MakeTimedPlace because FIXME*)
    fun next_time i = let
	fun find_min (col, res) = let
	    val time = sims.time col
	in
	    if Time.ready time then
		res
	    else 
		case res of
		    NONE => SOME time
		  | SOME time' => 
			if Time.lt(time',time) then res else SOME time
	end
	val min_wait= 
	    case sims.min (!(wait i)) of
		NONE   => NONE
	      | SOME t => SOME(Time.sub(t,max_offset))
    in
	case sims.pims.fold find_min (!(mark i)) NONE of
	    NONE => min_wait
	  | SOME t => 
	    case min_wait of
		NONE    => SOME t
	      | SOME t' => SOME(if Time.lt(t',t) then t' else t)
    end
    
    fun init i =  
	(sims.pims.addto(mark i,
			 sims.deleteto(wait i,
				       Time.add(Time.time(),max_offset)));
	 sims.pims.init_res (mark i))

    fun size i = 
	sims.size(!(wait i)) + sims.pims.size(!(mark i))
	
    fun print i = let
	val (rsize,rmark) = sims.pims.extract (!(mark i))
	val (wsize,wmark) = sims.extract (!(wait i))
    in
	sims.pims.cs.mkstr_ms (List.@(rmark,wmark))
    end

    fun addto i ems = sims.addto(wait i, ems)
	
    fun subfrom i ems = sims.pims.subfrom(mark i, ems)
	
    fun get i = 
	sims.pims.fold (op ::) (!(mark i)) (sims.fold (op ::) (!(wait i)) nil)

    fun set i ems = (sims.pims.init(mark i, nil); sims.init(wait i, ems))
end


functor MakeTimedFusionM(structure sims: CPN'SECONDARY_IMS;
			 val max_offset: sims.Time.time): CPN'TIMEDPLACE = struct

    structure sims = sims;
    structure Time = sims.Time;

    val ready= ref(sims.pims.empty())
    val waiting= ref(sims.empty())
	
    val change_assignments = fn _ => raise InternalError "Cannot change port assignments on a timed fusion place M"
    val change_no_of_inst = fn _ => raise InternalError "Cannot change no of inst on a timed fusion place M"

    val init_mark = ref(nil: sims.pims.cs CPN'MS.ms)
	
    fun mark _ = ready
    fun wait _ = waiting
	
    fun set_init_mark _ =
	(* Fusion place is initialize as instance 1 *)
	(CPN'inst:=1;   (*FIXME:remove CPN'inst*)
	 sims.pims.init(ready,nil);
	 sims.init(waiting, !init_mark)) 

    fun next_time i = let
	fun find_min (col, res) = let
	    val time = sims.time col
	in
	    if Time.ready time then
		res
	    else 
		case res of
		    NONE => SOME time
		  | SOME time' => 
			if Time.lt(time',time) then res else SOME time
	end
	val min_wait= 
	    case sims.min (!(wait i)) of
		NONE   => NONE
	      | SOME t => SOME(Time.sub(t,max_offset))
    in
	case sims.pims.fold find_min (!(mark i)) NONE of
	    NONE => min_wait
	  | SOME t => 
	    case min_wait of
		NONE    => SOME t
	      | SOME t' => SOME(if Time.lt(t',t) then t' else t)
    end
    
    fun init i =  
	(sims.pims.addto(mark i,
			 sims.deleteto(wait i,
				       Time.add(Time.time(),max_offset)));
	 sims.pims.init_res (mark i))
	
    fun size i = sims.size(!(wait i)) +  sims.pims.size(!(mark i))

    fun print i = let
	val (rsize,rmark) = sims.pims.extract (!(mark i))
	val (wsize,wmark) = sims.extract (!(wait i))
    in
	sims.pims.cs.mkstr_ms (List.@(rmark,wmark))
    end

    fun addto i ems = sims.addto(wait i, ems)
	
    fun subfrom i ems = sims.pims.subfrom(mark i, ems)
	
    fun get i = 
	sims.pims.fold (op ::) (!(mark i)) (sims.fold (op ::) (!(wait i)) nil)

    fun set i ems = (sims.pims.init(mark i, nil); sims.init(wait i, ems))
end


(************ Timed places with an unknown offset ************)

functor MakeTimedPlaceN(structure sims: CPN'SECONDARY_IMS;
			val no_of_inst: int): CPN'TIMEDPLACE = struct

    structure sims = sims;
    structure Time = sims.Time;

    (* locals *)
    fun gen_ready noi = tabulate(noi,fn _ => ref(sims.pims.empty()))
    fun gen_waiting noi = tabulate(noi,fn _ => ref(sims.empty()))
    val cur_no_of_inst= ref(no_of_inst)

    val ready= ref(gen_ready no_of_inst)
    val waiting= ref(gen_waiting no_of_inst)
	
    val init_mark = ref(nil: sims.pims.cs CPN'MS.ms)
	
    fun mark i = sub(!ready,i-1)
    fun wait i = sub(!waiting,i-1)
	    
    val change_assignments = fn _ => raise InternalError "Cannot change port assignments on a timed place N"
    fun change_no_of_inst noi =
	(ready:= (gen_ready noi);
	 waiting:= (gen_waiting noi);
	 cur_no_of_inst:= noi
	 )

    fun set_init_mark _ =
	(CPN'inst:= 1;   (*FIXME:remove CPN'inst*)
	 while !CPN'inst <= no_of_inst do
	     (sims.pims.init(mark (!CPN'inst), nil);
	      sims.init(wait (!CPN'inst), !init_mark); 
	      inc CPN'inst))

    (* FIXME: BROKEN: 
     * This should take into accound the variable offsets
     * See MakeTimedPlaceM.next_time for hints *)
    fun next_time i = let
	fun find_min (col, res) = let
	    val time = sims.time col
	in
	    if Time.ready time then 
		res
	    else 
		case res of
		    NONE => SOME time
		  | SOME time' => 
			if Time.lt(time',time) then res else SOME time
	end
    in
	sims.pims.fold find_min (!(mark i)) NONE
    end

    fun init i = 
	(sims.pims.addto(mark i,
			 sims.deleteall(wait i));
	 sims.pims.init_res (mark i))
	
    fun size i = 
	sims.size(!(wait i)) + sims.pims.size(!(mark i))
	
    fun print i = let
	val (rsize,rmark) = sims.pims.extract (!(mark i))
	val (wsize,wmark) = sims.extract (!(wait i))
    in
	sims.pims.cs.mkstr_ms (List.@(rmark,wmark))
    end

    fun addto i ems = sims.addto(wait i, ems)
	
    fun subfrom i ems = sims.pims.subfrom(mark i, ems)
	
    fun get i = 
	sims.pims.fold (op ::) (!(mark i)) (sims.fold (op ::) (!(wait i)) nil)

    fun set i ems = (sims.pims.init(mark i, nil); sims.init(wait i, ems))
end


functor MakeTimedFusionN(structure sims: CPN'SECONDARY_IMS): CPN'TIMEDPLACE = struct

    structure sims = sims;
    structure Time = sims.Time;

    val ready= ref(sims.pims.empty())
    val waiting= ref(sims.empty())
	
    val change_assignments = fn _ => raise InternalError "Cannot change port assignments on a timed fusion place N"
    val change_no_of_inst = fn _ => raise InternalError "Cannot change no of inst on a timed fusion place N"

    val init_mark = ref(nil: sims.pims.cs CPN'MS.ms)
	
    fun mark _ = ready
    fun wait _ = waiting

    fun set_init_mark _ =
	(* Fusion place is initialize as instance 1 *)
	(CPN'inst:= 1;   (*FIXME:remove CPN'inst*)
	 sims.pims.init(mark 1, !init_mark))
	
    fun next_time i = let
	fun find_min (col, res) = let
	    val time = sims.time col
	in
	    if Time.ready time then 
		res
	    else 
		case res of
		    NONE => SOME time
		  | SOME time' => 
			    if Time.lt(time',time) then res else SOME time
	end
    in
	sims.pims.fold find_min (!(mark i)) NONE
    end

    fun init i = 
	(sims.pims.addto(mark i, sims.deleteall(wait i));
	 sims.pims.init_res (mark i))

    fun size i = sims.size(!(wait i)) + sims.pims.size(!(mark i))
	
    fun print i = let
	val (rsize,rmark) = sims.pims.extract (!(mark i))
	val (wsize,wmark) = sims.extract (!(wait i))
    in
	sims.pims.cs.mkstr_ms (List.@(rmark,wmark))
    end

    fun addto i ems = sims.addto(wait i, ems)
	
    fun subfrom i ems = sims.pims.subfrom(mark i, ems)
	
    fun get i = 
	sims.pims.fold (op ::) (!(mark i)) (sims.fold (op ::) (!(wait i)) nil)
    
    fun set i ems = (sims.pims.init(mark i, nil); sims.init(wait i, ems))
end

(* -- Main of CPN'Places ---------------------------------------------- *)

    type item = {print: int -> string, size: int -> int}
    val instances: (CPN'Id.id,item) HashTable.hash_table =
	  HashTable.mkTable hashId (64, InternalError "Place.instances")

    fun print_mark (p: CPN'Id.id , i: int) =
	(#print(HashTable.lookup instances p)) i

    fun size_mark (p: CPN'Id.id , i: int) =
	(#size(HashTable.lookup instances p)) i

    val init_mark_funs = ref (nil: (unit -> unit) list)

    fun set_init_mark () = List.app (fn f => f()) (!init_mark_funs) 

    (* The change marking is implemented as close to the previous
     * implementation as possible. *)

    fun change_mark (p,i: int,toinit)= let
	
	val (name,ims,cs,im) = case CPN'PlaceTable.peek p of
	    SOME{int=SOME{name,ims,...},ext={cs,im,...},...} => 
		(name,ims,cs,im)
	  | _ => raise InternalError "change_mark"
  
	val mark = if toinit then "!"^name^".init_mark" else "CPN'marking" 

	val inst = Int.toString i
    in
	if CPN'CSTable.is_timed cs then
	    CPN'Env.exec
	    ["val _ = (",ims,"_sims.init(",name,".wait ",inst,",",mark,"); ",
	     ims,"_sims.pims.init(",name,".mark ",inst,",nil));"]
	else
	    CPN'Env.exec
	    ["val _ = ",ims,".init(",name,".mark ",inst,",",mark,");"]
    end 
end
