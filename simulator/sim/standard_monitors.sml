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
(* File: standard_monitors.sml
 *
 * Creation of standard monitors.
 *)

functor CPN'StandardMonitors 
	    (structure Time : CPN'TIME
	     and InstTable : CPN'INSTTABLE
	     and CreateMonitor: CPN'CREATEMONITOR) : CPN'STANDARDMONITORS = 
struct

fun get_sur_transitions (pid,i) = 
    let
	val tids = 
	    (List.filter CPN'TransitionTable.is_transition 
			 (List.map (fn (tid,i) => tid)
				   (HashTable.listItemsi CPN'TransitionTable.table)))
	fun get_sur_trans (pid,i) = 
	    map (fn t => (t,i))
		(List.filter 
		     (fn tid => 
			 List.exists (fn p => p=pid)
				     (CPN'TransitionTable.get_places tid))
		     tids)
	val surrounding_transitions = 
	    flatten (map get_sur_trans 
			 (InstTable.get_inst_cons ((pid,i),[])))
    in
	Misc.unique_sort 
	    ((fn ((tid1,i1),(tid2,i2)) => String.<(tid1,tid2) orelse 
					((tid1=tid2) andalso i1<i2))) 
	    (remdupl surrounding_transitions)
    end

(* Data collectors *)

fun markingSize (id,name,pid,i,update_logfile) = 
    (CreateMonitor.remove id;
    case CPN'PlaceTable.peek pid of
	NONE => ((id,[(pid,"Place not found!")],([],[])),[])
      | SOME _ => 
	let
	(* FIXME what happens if the model becomes "timed" after 
	 * the monitor is created? *)
	val timed = CPN'MonitorTable.model_is_timed()

	val pid_insts = [(pid,i)]
	val tid_insts = if timed 
			then get_sur_transitions (pid,i)
			else [] (* activate after every step *)

	val predfun = CPN'MonitorTemplate.gen_fun(tid_insts, pid_insts, 
						  "pred","true","false",true)
	val obsfun = 
	    CPN'MonitorTemplate.gen_fun
		(tid_insts, pid_insts, 
		 (* could be optimized to use place.size rather than 
		  * length (place.get), but this would require
		  * changes in create_monitors *)
		 "obs","size("^
		       CPN'MonitorTemplate.gen_mark_arg_name(pid,i)^
		       ")","~1",true)

	val initfun = 
	    CPN'MonitorTemplate.gen_fun
		(tid_insts, pid_insts, 
		 "init","SOME (size("^
			CPN'MonitorTemplate.gen_mark_arg_name(pid,i)^"))",
		 "",false)
	val stopfun = 
	    if timed
	    then CPN'MonitorTemplate.gen_fun
		     (tid_insts, pid_insts, 
		      "stop","SOME (size("^
			CPN'MonitorTemplate.gen_mark_arg_name(pid,i)^"))",
		      "",false)
	    else CPN'MonitorTemplate.gen_fun
		     (tid_insts, pid_insts, "stop","NONE","",false)
		     
	val createres = 
	    CreateMonitor.create_some 
		[(id,{name=name,
		      montype = CPN'MonitorTable.step_monitor,
		      places=pid_insts,
		      transitions=tid_insts,
		      kind=CPN'MonitorTable.datacoll 
			       {timed=timed,
				logfile=update_logfile,
				(* FIXME need better way to handle ids? *)
				init=(id^"_1",initfun),
				stop=(id^"_4",stopfun),
				pred=(id^"_2",predfun),
				obs=(id^"_3",obsfun)}})]
    in
	(hd createres, tid_insts)
    end)

fun listLength (id,name,pid,i,update_logfile) = 
    (CreateMonitor.remove id;
    case CPN'PlaceTable.peek pid of 
	NONE => ((id,[(pid,"Place not found!")],([],[])),[])
      | SOME _ => 
	let
	(* FIXME what happens if the model becomes "timed" after 
	 * the monitor is created? *)
	val timed = CPN'MonitorTable.model_is_timed()

	val cs = (#cs (#ext (CPN'PlaceTable.find pid)))

	val isListCS = 
	    case CPN'CSTable.get_prime_kind (#kind(CPN'CSTable.find cs)) of
		CPN'CSTable.list_cs _ => true
	      | _ => false

	val isTimed = (CPN'CSTable.is_timed cs
		       handle InternalError _ => false)

	val pid_insts = [(pid,i)]
	val tid_insts = if timed 
			then get_sur_transitions (pid,i)
			else [] (* activate after every step *)

	val predfun = CPN'MonitorTemplate.gen_fun(tid_insts, pid_insts, 
						  "pred","true","false",true)
	val obsfun = 
	    CPN'MonitorTemplate.gen_fun
		(tid_insts, pid_insts, 
		 "obs","length"^(if isTimed 
				 then "(ModelTime.col"
				 else "")^"(ms_to_col("^
		       CPN'MonitorTemplate.gen_mark_arg_name(pid,i)^
		       "))"^(if isTimed then ")" else ""),"~1",true)

	val initfun = 
	    CPN'MonitorTemplate.gen_fun
		(tid_insts, pid_insts, 
		 "init","SOME (length"^(if isTimed 
					then "(ModelTime.col"
					else "")^"(ms_to_col("^
			CPN'MonitorTemplate.gen_mark_arg_name(pid,i)^
			")))"^(if isTimed then ")" else ""),"",false)
	val stopfun = 
	    if timed
	    then CPN'MonitorTemplate.gen_fun
		     (tid_insts, pid_insts, 
		      "stop","SOME (length"^(if isTimed 
					     then "(ModelTime.col"
					     else "")^"(ms_to_col("^
			     CPN'MonitorTemplate.gen_mark_arg_name(pid,i)^
			     ")))"^(if isTimed then ")" else ""),"",false)
	    else CPN'MonitorTemplate.gen_fun
		     (tid_insts, pid_insts, "stop","NONE","",false)

	val res = 
	    if isListCS
	    then hd(CreateMonitor.create_some 
		     [(id,{name=name,
			   montype = CPN'MonitorTable.step_monitor,
			   places=pid_insts,
			   transitions=tid_insts,
			   kind=CPN'MonitorTable.datacoll 
				    {timed=timed,
				     logfile=update_logfile,
				     (* FIXME need better way to handle ids? *)
				     init=(id^"_1",initfun),
				     stop=(id^"_4",stopfun),
				     pred=(id^"_2",predfun),
				     obs=(id^"_3",obsfun)}})])
	    else (id,[(id,"Error: color set "^cs^" is not a list colour set.")],([],[]))
    in
	(res, if isListCS then tid_insts else [])
    end)

fun countOccurrences (id,name,tid,i,update_logfile) = 
    (CreateMonitor.remove id;
    case CPN'TransitionTable.peek tid of
	NONE => (id, [(tid,"Transition not found!")],([],[]))
      | SOME (CPN'TransitionTable.substitution _) => 
	(id,[(tid,"Cannot create a monitor to\ncount occurrences of substitution transitions!")],([],[]))
      | SOME _ => 
	let
	val tid_insts = [(tid,i)]
	val pid_insts = []
	val predfun = CPN'MonitorTemplate.gen_fun(tid_insts,pid_insts,
						  "pred","true","false",true)
	val obsfun = 
	    CPN'MonitorTemplate.gen_fun	(tid_insts,pid_insts,
					 "obs","1","~1",true)

	val initfun = 
	    CPN'MonitorTemplate.gen_fun
		(tid_insts,pid_insts,"init","NONE","",false)
	val stopfun = 
	    CPN'MonitorTemplate.gen_fun
		(tid_insts,pid_insts,"stop","NONE","",false)

	val createres = 
	    CreateMonitor.create_some 
		[(id,{name=name,
		      montype = CPN'MonitorTable.step_monitor,
		      places=pid_insts,
		      transitions=tid_insts,
		      kind=CPN'MonitorTable.datacoll 
			       {timed=false,
				logfile=update_logfile,
				(* FIXME need better way to handle ids *)
				init=(id^"_1",initfun),
				stop=(id^"_4",stopfun),
				pred=(id^"_2",predfun),
				obs=(id^"_3",obsfun)}})]
    in
	hd createres
    end)

(* Breakpoints *)

fun placeIsEmpty (id,name,pid,i,isempty) = 
    (CreateMonitor.remove id;
    case CPN'PlaceTable.peek pid of 
	NONE => (id,[(pid,"Place not found!")],([],[]))
      | SOME _  => 
	let
	    val tid_insts = []
	    val pid_insts = [(pid,i)]
	    val truestr = CPN'MonitorTemplate.gen_mark_arg_name(pid,i)^
			  (if isempty then " = " else " <> ")^"empty"
	    val predfun = CPN'MonitorTemplate.gen_fun
			      (tid_insts,pid_insts,
			       "pred",truestr,"false",true)
	    val createres = 
		CreateMonitor.create_some 
		    [(id,{name=name,
			  montype = CPN'MonitorTable.step_monitor,
			  places=pid_insts,
			  transitions=tid_insts,
			  kind=CPN'MonitorTable.breakpoint 
				   (id^"_1",predfun)})]
	in
	    hd createres
	end)

fun transitionIsEnabled (id,name,tid,i,isenabled) = 
    (CreateMonitor.remove id;
    case CPN'TransitionTable.peek tid of
	NONE => (id,[(tid,"Transition not found!")],([],[]))
      | SOME (CPN'TransitionTable.substitution _) => 
	(id,[(tid,"Cannot create a monitor to\ncheck enabling of substitution transitions!")],([],[]))
      | SOME _  => 
	let
	    val tid_insts = []
	    val pid_insts = []
	    val truestr = "CPN'Sim.check_enab(\""^tid^"\","^
			  Int.toString(i)^")="^Bool.toString(isenabled)
	    val predfun = CPN'MonitorTemplate.gen_fun
			      (tid_insts,pid_insts,
			       "pred",truestr,"false",true)
	    val errs = CPN'MonitorAux.check_subnet ([],[(tid,i)])
		       
	    val createres = 
		if errs = []
		then 
		hd (CreateMonitor.create_some 
		    [(id,{name=name,
			  montype = CPN'MonitorTable.step_monitor,
			  places=pid_insts,
			  transitions=tid_insts,
			  kind=CPN'MonitorTable.breakpoint 
				   (id^"_1",predfun)})])
		else (id,errs,([],[]))

	    (* Necessary for checking if a transition becomes
	     * enabled when time is increased *)
	    val _ = 	
		if (CPN'MonitorTable.model_is_timed()) andalso
		   (#2 createres)=[]
		then CPN'Env.use_string 
			 ["val _ = CPN'Monitors.insert_fun(\"",
			  id,"\",\"",name,".monitor\",",
			  name,".monitor,",
			  " CPN'Monitors.sim_stop_crit_fun_list)\n"] 
		else ()
	in
	    createres
	end)
end
