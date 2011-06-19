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
(* File: monitor.sml
 *
 * Creation of monitors.
 *)

functor CPN'Monitors (structure Sim : CPN'SIM 
		      and Replications : CPN'REPLICATIONS) : CPN'MONITORS =
struct

structure Sim = Sim

fun set_is_active mid_bool_list = 
    let
	fun set (mid,b) = 
	    CPN'Env.exec [CPN'MonitorTable.get_name mid,".is_active:=",
			  Bool.toString(b)]
    in
	map set mid_bool_list
    end

fun insert_fun (mid,funname,mfun,funlistref) = 
    let
	val index = CPN'MonitorTable.get_order_index mid
	val predecessors = List.take (!CPN'MonitorTable.order,index)
	val successors = List.drop (!CPN'MonitorTable.order,index+1)

	val withoutmid = List.filter (fn (m,n,f) => m <> mid) (!funlistref)

	fun insert_in_order [] = [(mid,funname,mfun)]
	  | insert_in_order ((m,n,f)::funlist) = 
	    if List.exists (fn m1 => m=m1) successors
	    then (mid,funname,mfun)::(m,n,f)::funlist
	    else (m,n,f)::(insert_in_order funlist)
    in
	funlistref := insert_in_order(withoutmid)
    end

fun rm_fun (mid,funlistref) = 
    let
	(* assume that the mid is found at most once in the list*)
	fun rm [] = []
	  | rm ((m,n,f)::rest) = 
	    if m=mid
	    then rest
	    else (m,n,f)::(rm rest)
		
	fun rm_from_list funlistref = 
	    funlistref := rm (!funlistref)
    in
	rm_from_list funlistref
    end

(* Monitoring functions for individual simulations *)

val sim_init_fun_list = ref []: (CPN'Id.id * string * (unit -> unit)) list ref
val step_monitor_fun_list = ref []: (CPN'Id.id * string * (unit -> unit)) list ref
val sim_stop_fun_list = ref []: (CPN'Id.id * string * (unit -> unit)) list ref
val sim_stop_crit_fun_list = ref []: (CPN'Id.id * string * (unit -> unit)) list ref

fun init_sim_monitors () = 
    app (fn (mid,n,f) => 
	    (f()
	     handle exn => (Sim.monitor_exn_source:=n;
			    raise exn))) (!sim_init_fun_list)

fun monitor_step () = 
    app (fn (mid,n,f) => 
	    (f()
	     handle exn => (Sim.monitor_exn_source:=n;
			    raise exn))) (!step_monitor_fun_list)

fun stop_sim_monitors () = 
    (app (fn (mid,n,f) => 
	     (f()
	      handle exn => (Sim.monitor_exn_source:=n;
			     raise exn))) (!sim_stop_fun_list);
     CPN'PerfReport.save_sim_report(NONE))

fun sim_stop_crit_monitors() = 
    app (fn (mid,n,f) => 
	    (f()
	     handle exn => (Sim.monitor_exn_source:=n;
			    raise exn))) (!sim_stop_crit_fun_list)

(* Insert the monitoring functions in the simulator *)
val _ = Sim.insert_init_state_fun
	    ("CPN'Monitors.init_sim_monitors",init_sim_monitors)
val _ = Sim.insert_stop_fun
	    ("CPN'Monitors.stop_sim_monitors",stop_sim_monitors)
val _ = Sim.insert_step_inc_fun
	    ("CPN'Monitors.monitor_step",monitor_step)
val _ = Sim.insert_stop_crit_fun
	    ("CPN'Monitors.sim_stop_crit_monitors",sim_stop_crit_monitors)


(* Monitoring functions for replicated simulations *)
val rep_init_fun_list = ref []: (CPN'Id.id * string * (unit -> unit)) list ref
val post_rep_monitor_fun_list = ref []: (CPN'Id.id * string * (unit -> unit)) list ref
val pre_rep_monitor_fun_list = ref []: (CPN'Id.id * string * (unit -> unit)) list ref
val rep_stop_fun_list = ref []: (CPN'Id.id * string * (unit -> unit)) list ref

fun init_rep_monitors () = app (fn (mid,n,f) => f()) (!rep_init_fun_list)

fun monitor_pre_rep () = app (fn (mid,n,f) => f()) (!pre_rep_monitor_fun_list)

fun monitor_post_rep (stepstr,timestr,reasonstr) = 
    (* FIXME: should it be possible to do something
     * with the strings that are returned at the end of a simulation? *)
    app (fn (mid,n,f) => f()) (!post_rep_monitor_fun_list)

fun stop_rep_monitors (numrep:int) = 
    (app (fn (mid,n,f) => f()) (!rep_stop_fun_list);
     CPN'PerfReport.save_iid_report(NONE,SOME numrep);
     (* FIXME: this will cause an error if replication 
      * output dir is not specified. *)
     app (fn percent => 
	     CPN'PerfReport.saveConfidenceIntervalReport(OS.Path.concat(Output.getRepOutputDir(),"confidenceintervals"^Int.toString percent^".txt"),percent)) (CPN'PerfOptions.get_ci_percentages());
     if !CPN'GnuplotScript.saveScripts
     then CPN'GnuplotScript.saveRepScripts() 
     else ())
    handle CPN'StreamIO.IOError s => raise CPN'Cancel s
	 | CPN'Error s => raise CPN'Error ("Error in rep_stop_monitors: "^s)
	 | exn => raise CPN'Error("Error in rep_stop_monitors: "^(exnMessage exn))

(* Insert the monitoring functions in the sim replicator *)

val _ = Replications.insert_init_rep_fun
	    ("CPN'Monitors.init_rep_monitors",init_rep_monitors)
val _ = Replications.insert_stop_fun
	    ("CPN'Monitors.stop_rep_monitors",stop_rep_monitors)
val _ = Replications.insert_pre_rep_fun
	    ("CPN'Monitors.monitor_pre_rep",monitor_pre_rep)
val _ = Replications.insert_post_rep_fun
	    ("CPN'Monitors.monitor_post_rep",monitor_post_rep)

(*
val _ = CPN'Env.use_string 
	    ["val _ = CPN'Replications.insert_init_rep_fun(\"",
	     "CPN'Monitors.init_rep_monitors\",",
	     "CPN'Monitors.init_rep_monitors)\n"]
val _ = CPN'Env.use_string 
	    ["val _ = CPN'Replications.insert_stop_fun(\"",
	     "CPN'Monitors.stop_rep_monitors\",",
	     "CPN'Monitors.stop_rep_monitors)\n"]
val _ = CPN'Env.use_string 
	    ["val _ = CPN'Replications.insert_pre_rep_fun(\"",
	     "CPN'Monitors.monitor_pre_rep\",",
	     "CPN'Monitors.monitor_pre_rep)\n"]
val _ = CPN'Env.use_string 
	    ["val _ = CPN'Replications.insert_post_rep_fun(\"",
	     "CPN'Monitors.monitor_post_rep\",",
	     "CPN'Monitors.monitor_post_rep)\n"]
*)

fun flush_out(stream) = 
    if !Sim.Options.flush_files
       then TextIO.flushOut(stream)
    else ()

functor CreateUntimedDC (type bindelem 
			 and subnet
			 and markings
			 structure SV: CPN'UNTIMEDSTATVAR
			 val pred: subnet -> bool
			 and obs : subnet -> SV.data
			 and init : markings -> SV.data option
			 and stop : markings -> SV.data option
			 and get_subnet: bindelem -> subnet
			 and get_markings: unit -> markings
			 and name: string
			 and montype: CPN'MonitorTable.montype
			 and updatelogfile: bool) : UNTIMEDDCMONITOR = struct

type bindelem = bindelem
type subnet = subnet
type markings = markings

val get_subnet = get_subnet
val get_markings = get_markings
val pred = pred
val obs = obs
val init = init
val stop = stop

val is_active = ref true

structure SV = SV
type data = SV.data

val statvar = SV.create()
fun count() = SV.count(statvar)
fun min() =  SV.min(statvar)
fun max() =  SV.max(statvar)
fun first() =  SV.first(statvar)
fun last() =  SV.last(statvar)
fun sum() =  SV.sum(statvar)
fun avrg() =  SV.avrg(statvar)
fun ss() =  SV.ss(statvar)
fun ssd() =  SV.ssd(statvar)
fun vari() =  SV.vari(statvar)
fun std() =  SV.std(statvar)
fun ci (level:int) = SV.ci(statvar,level)
fun get_stat_strings() = (name,SV.toStrings(statvar))

(* logfile stuff *)
val logfile_name = (name^".log")
val update_logfile = ref updatelogfile;
val stream = ref (NONE: TextIO.outstream option)

fun close_logfile() = case (!stream) of
			  NONE => ()
			| SOME st => (TextIO.closeOut(st);
				     stream := NONE)
fun init_logfile() =
    if (!update_logfile)
    then 
	let
	    val filepath = 
		case montype of 
		    CPN'MonitorTable.step_monitor => 
		    (Output.initSimLogfileDir();
		     OS.Path.concat(Output.getSimLogfileDir(),logfile_name))
		  | CPN'MonitorTable.sim_monitor => 
		    (Output.initRepLogfileDir();
		     OS.Path.concat(Output.getRepLogfileDir(),logfile_name))
	in
	    case (!stream) of
		NONE => (stream := (if (Sim.step()=0)
				    then SOME(TextIO.openOut(filepath))
				    else (SOME(TextIO.openAppend(filepath))
					  handle IO => SOME(TextIO.openOut(filepath))));
			 if OS.FileSys.fileSize(filepath)=0 
			 then case montype of 
				  CPN'MonitorTable.step_monitor => 
				  (TextIO.output(valOf(!stream),"#data counter step time\n") (* time should not be included for untimed models *);
				   flush_out(valOf(!stream)))
				| CPN'MonitorTable.sim_monitor => 
				  (TextIO.output(valOf(!stream),"#data counter sim\n");
				   flush_out(valOf(!stream)))
			 else ())
	      | _ => (close_logfile();
		      init_logfile())
	end
    else ()

fun set_update_logfile (b:bool) = update_logfile := b

fun addtologfile(x:data) = 
    if (!update_logfile)
    then case (!stream) of
	     NONE => (init_logfile();
		      addtologfile(x))
	   | SOME stream => 
	     case montype of 
		 CPN'MonitorTable.step_monitor =>
		 (TextIO.output(stream, 
			       (SV.dataToString x)^" "^
			       (Int.toString(count()))^" "^
			       (IntInf.toString(Sim.step()))^" "^
			       (Sim.Time.toString(Sim.Time.time()))^ (* "" if unit time *)
			       "\n");
		  flush_out(stream))
	       | CPN'MonitorTable.sim_monitor => 
		 (TextIO.output(stream, 
			       (SV.dataToString x)^" "^
			       (Int.toString(count()))^" "^
			       (Int.toString(Replications.get_rep_num()))^"\n");
		  flush_out(stream))
    else ()

(* for independent, identically distributed values *)
val count_iid = CPN'IUSV.create()
val min_iid = SV.create()
val max_iid = SV.create()
val sum_iid = SV.create()
val avrg_iid = CPN'RUSV.create()
val ssd_iid = CPN'RUSV.create()
val std_iid = CPN'RUSV.create()
val vari_iid = CPN'RUSV.create()

fun iid_filename(filesuffix) = 
    name^"_"^filesuffix^".log"

fun create_iid_logfile(filesuffix) = 
    if not (!Replications.running_reps)
    then () 
    else let
	    val filename = iid_filename(filesuffix)
	    val filepath = 
		(Output.initRepLogfileDir();
		 OS.Path.concat(Output.getRepLogfileDir(),filename))
	    val fid = TextIO.openOut(filepath)
	in
	    TextIO.output(fid,"#data counter\n");
	    TextIO.closeOut(fid)
	end
(* let
	    val filename = iid_filename(filesuffix)
	    val filepath = 
		if !Replications.running_reps
		then (Output.initRepLogfileDir();
		      OS.Path.concat(Output.getRepLogfileDir(),filename))
		else ((* Output.initSimLogfileDir();
		      OS.Path.concat(Output.getSimLogfileDir(),filename) *))
	    val fid = TextIO.openOut(filepath)
	in
	    TextIO.output(fid,"#data counter\n");
	    TextIO.closeOut(fid)
	end
*)

fun addto_iid_logfile(filesuffix,datastr,countstr) = 
    if not (!Replications.running_reps)
    then ()
    else let
	    val filename = iid_filename(filesuffix)
	    val filepath = OS.Path.concat(Output.getRepLogfileDir(),filename)
	    val fid = (TextIO.openAppend(filepath)
		       handle IO => TextIO.openOut filepath)
	in
	    TextIO.output(fid,datastr^" "^countstr^"\n");
	    TextIO.closeOut(fid)
	end
(* let
	    val filename = iid_filename(filesuffix)
	    val filepath = 
		if !Replications.running_reps
		then OS.Path.concat(Output.getRepLogfileDir(),filename)
		else OS.Path.concat(Output.getSimLogfileDir(),filename)
	    val fid = TextIO.openAppend(filepath)
	in
	    TextIO.output(fid,datastr^" "^countstr^"\n");
	    TextIO.closeOut(fid)
	end
*)

fun init_iid() = 
    (CPN'IUSV.init(count_iid);
     SV.init(min_iid);
     SV.init(max_iid);
     SV.init(sum_iid); 
     CPN'RUSV.init(avrg_iid);
     CPN'RUSV.init(ssd_iid);
     CPN'RUSV.init(std_iid);
     CPN'RUSV.init(vari_iid);
     app create_iid_logfile ["count_iid","min_iid","max_iid","sum_iid",
			     "avrg_iid","ssd_iid","std_iid","vari_iid"])

fun upd_iid() = 
    (CPN'IUSV.upd(count_iid,IntInf.fromInt(count()));
     SV.upd(min_iid,min());
     SV.upd(max_iid,max());
     SV.upd(sum_iid,sum()); 
     CPN'RUSV.upd(avrg_iid,avrg());
     CPN'RUSV.upd(ssd_iid,ssd());
     CPN'RUSV.upd(std_iid,std());
     CPN'RUSV.upd(vari_iid,vari());
     addto_iid_logfile("count_iid",
		       CPN'IUSV.dataToString(CPN'IUSV.last(count_iid)),
		       Int.toString(CPN'IUSV.count(count_iid)));
     addto_iid_logfile("min_iid",SV.dataToString(SV.last(min_iid)),
		       Int.toString(SV.count(min_iid)));
     addto_iid_logfile("max_iid",SV.dataToString(SV.last(max_iid)),
		       Int.toString(SV.count(max_iid)));
     addto_iid_logfile("sum_iid",SV.dataToString(SV.last(sum_iid)),
		       Int.toString(SV.count(sum_iid)));
     addto_iid_logfile("avrg_iid",
		       CPN'RUSV.dataToString(CPN'RUSV.last(avrg_iid)),
		       Int.toString(CPN'RUSV.count(avrg_iid)));
     addto_iid_logfile("ssd_iid",
		       CPN'RUSV.dataToString(CPN'RUSV.last(ssd_iid)),
		       Int.toString(CPN'RUSV.count(ssd_iid)));
     addto_iid_logfile("std_iid",
		       CPN'RUSV.dataToString(CPN'RUSV.last(std_iid)),
		       Int.toString(CPN'RUSV.count(std_iid)));
     addto_iid_logfile("vari_iid",
		       CPN'RUSV.dataToString(CPN'RUSV.last(vari_iid)),
		       Int.toString(CPN'RUSV.count(vari_iid))))

fun action obsval = (SV.upd(statvar,obsval);
		     addtologfile(obsval))

fun stop_monitor() = 
    (if !is_active
     then let
	     val stopobs = stop(get_markings())
	     val _ = CPN'debug (name^".stop obsval="^
				(case stopobs of
				     NONE =>  "NONE"
				   | SOME obsval => "SOME "^SV.dataToString(obsval)))
	 in 
	     case stopobs of 
		 NONE => ()
	       | SOME obsval => action(obsval);
	     upd_iid()
	 end
     else ();
     close_logfile())

fun init_monitor()
  = (SV.init(statvar);
     init_logfile();
     (if !is_active
      then let
	      val currentmarkings = get_markings()
	      val initobs = init(currentmarkings)
	  in
	      case initobs of 
		  NONE => ()
		| SOME obsval => action(obsval)
	  end
      else());
     if (!Replications.running_reps andalso 
	 Replications.get_rep_num()=0) 
	orelse
	((not(!Replications.running_reps)) andalso 
	 Sim.subrun()=0) 
     then init_iid()
     else ())
	   
fun monitor (bindelem:bindelem) =
    if !is_active
    then let
	    val _ = CPN'debug (name^".monitor (is active)")
	    val currentsubnet = get_subnet bindelem
	in
	    if pred (currentsubnet)
	    then action(obs(currentsubnet))
	    else ()
	end
	    handle exn => (Sim.monitor_exn_source:=(name^".monitor");
			   raise exn)
    else ()
	      
end (* CreateUntimedDC *)

functor CreateTimedDC (type bindelem 
		       and subnet
		       and markings
		       structure SV: CPN'TIMEDSTATVAR
		       structure MSV: CPN'UNTIMEDSTATVAR
		       sharing type SV.data = MSV.data
		       val pred: subnet -> bool
		       and obs : subnet -> SV.data
		       and init : markings -> SV.data option
		       and stop : markings -> SV.data option
		       and get_subnet: bindelem -> subnet
		       and get_markings: unit -> markings
		       and name: string
		       and updatelogfile: bool) : TIMEDDCMONITOR = struct

type bindelem = bindelem
type subnet = subnet
type markings = markings
type time = SV.time

val get_subnet = get_subnet
val get_markings = get_markings
val pred = pred
val obs = obs
val init = init
val stop = stop

val is_active = ref true

structure SV = SV
type data = SV.data
type sumtype = SV.sumtype
structure MSV = MSV

val statvar = SV.create()
fun count() = SV.count(statvar)
fun min() =  SV.min(statvar)
fun max() =  SV.max(statvar)
fun first() =  SV.first(statvar)
fun last() =  SV.last(statvar)
fun sum() =  SV.sum(statvar)
fun avrg() =  SV.avrg(statvar)
fun ss() =  SV.ss(statvar)
fun ssd() =  SV.ssd(statvar)
fun vari() =  SV.vari(statvar)
fun std() =  SV.std(statvar)
fun ci (level:int) = SV.ci(statvar,level)
fun get_stat_strings() = (name,SV.toStrings(statvar))

fun starttime() = SV.starttime(statvar)
fun lasttime() = SV.lasttime(statvar)
fun interval() = SV.interval(statvar)

(* logfile stuff *)
val logfile_name = (name^".log")
val update_logfile = ref updatelogfile;
val stream = ref (NONE: TextIO.outstream option)

fun close_logfile() = case (!stream) of
			  NONE => ()
			| SOME st => (TextIO.closeOut(st);
				     stream := NONE)
fun init_logfile() =
    if (!update_logfile)
    then 
	let
	    val filepath = 
		(Output.initSimLogfileDir();
		 OS.Path.concat(Output.getSimLogfileDir(),logfile_name))
	in
	    case (!stream) of
		NONE => (stream := (if (Sim.step()=(IntInf.fromInt 0))
				    then SOME(TextIO.openOut(filepath))
				    else (SOME(TextIO.openAppend(filepath))
					  handle IO => SOME(TextIO.openOut(filepath))));
			 if OS.FileSys.fileSize(filepath)=0 
			 then (TextIO.output(valOf(!stream),"#data counter step time\n") (* time should not be included for untimed models *);
			       flush_out(valOf(!stream)))
			 else ())
	      | _ => (close_logfile();
		      init_logfile())
	end
    else ()

fun set_update_logfile (b:bool) = update_logfile := b

fun addtologfile(x:data) = 
    if (!update_logfile)
    then case (!stream) of
	     NONE => (init_logfile();
		      addtologfile(x))
	   | SOME stream => 
	     (TextIO.output(stream, 
			   (SV.dataToString x)^" "^
			   (Int.toString(count()))^" "^
			   (IntInf.toString(Sim.step()))^" "^
			   (Sim.Time.toString(Sim.Time.time()))^ (* "" if unit time *)
			   "\n");
	      flush_out(stream)) 
    else ()

fun action obsval = (SV.upd(statvar,obsval);
		     addtologfile(obsval))

(* for independent, identically distributed values *)
val count_iid = CPN'IUSV.create()
val min_iid = MSV.create()
val max_iid = MSV.create()
(* val sum_iid = CPN'I/RV.create() *)
val avrg_iid = CPN'RUSV.create()
val ssd_iid = CPN'RUSV.create()
val std_iid = CPN'RUSV.create()
val vari_iid = CPN'RUSV.create()

fun iid_filename(filesuffix) = 
    name^"_"^filesuffix^".log"

fun create_iid_logfile(filesuffix) = 
    if not (!Replications.running_reps)
    then () 
    else let
	    val filename = iid_filename(filesuffix)
	    val filepath = 
		(Output.initRepLogfileDir();
		 OS.Path.concat(Output.getRepLogfileDir(),filename))
	    val fid = TextIO.openOut(filepath)
	in
	    TextIO.output(fid,"#data counter\n");
	    TextIO.closeOut(fid)
	end
(* let
	    val filename = iid_filename(filesuffix)
	    val filepath = 
		if !Replications.running_reps
		then (Output.initRepLogfileDir();
		      OS.Path.concat(Output.getRepLogfileDir(),filename))
		else ((* Output.initSimLogfileDir();
		      OS.Path.concat(Output.getSimLogfileDir(),filename) *))
	    val fid = TextIO.openOut(filepath)
	in
	    TextIO.output(fid,"#data counter\n");
	    TextIO.closeOut(fid)
	end
*)

fun addto_iid_logfile(filesuffix,datastr,countstr) = 
    if not (!Replications.running_reps)
    then ()
    else let
	    val filename = iid_filename(filesuffix)
	    val filepath = OS.Path.concat(Output.getRepLogfileDir(),filename)
	    val fid = (TextIO.openAppend(filepath)
		       handle IO => TextIO.openOut filepath)
	in
	    TextIO.output(fid,datastr^" "^countstr^"\n");
	    TextIO.closeOut(fid)
	end
(* let
     val filename = iid_filename(filesuffix)
	    val filepath = 
		if !Replications.running_reps
		then OS.Path.concat(Output.getRepLogfileDir(),filename)
		else OS.Path.concat(Output.getSimLogfileDir(),filename)
	    val fid = TextIO.openAppend(filepath)
	in
	    TextIO.output(fid,datastr^" "^countstr^"\n");
	    TextIO.closeOut(fid)
	end
*)

fun init_iid() = 
    (CPN'IUSV.init(count_iid);
     MSV.init(min_iid);
     MSV.init(max_iid);
     (* SV.init(sum_iid,sum()); *)
     CPN'RUSV.init(avrg_iid);
     CPN'RUSV.init(ssd_iid);
     CPN'RUSV.init(std_iid);
     CPN'RUSV.init(vari_iid);
     app create_iid_logfile ["count_iid","min_iid","max_iid",
			     (* "sum_iid",*)
			     "avrg_iid","ssd_iid","std_iid","vari_iid"])

fun upd_iid()=
    (CPN'IUSV.upd(count_iid,IntInf.fromInt(count()));
     MSV.upd(min_iid,min());
     MSV.upd(max_iid,max());
     (* SV.upd(sum_iid,sum()); *)
     CPN'RUSV.upd(avrg_iid,avrg());
     CPN'RUSV.upd(ssd_iid,ssd());
     CPN'RUSV.upd(std_iid,std());
     CPN'RUSV.upd(vari_iid,vari());
     addto_iid_logfile("count_iid",
		       CPN'IUSV.dataToString(CPN'IUSV.last(count_iid)),
		       Int.toString(CPN'IUSV.count(count_iid)));
     addto_iid_logfile("min_iid",MSV.dataToString(MSV.last(min_iid)),
		       Int.toString(MSV.count(min_iid)));
     addto_iid_logfile("max_iid",MSV.dataToString(MSV.last(max_iid)),
		       Int.toString(MSV.count(max_iid)));
(*     addto_iid_logfile("sum_iid",SV.dataToString(SV.last(sum_iid)), 
		       Int.toString(SV.count(sum_iid))); *)
     addto_iid_logfile("avrg_iid",
		       CPN'RUSV.dataToString(CPN'RUSV.last(avrg_iid)),
		       Int.toString(CPN'RUSV.count(avrg_iid)));
     addto_iid_logfile("ssd_iid",
		       CPN'RUSV.dataToString(CPN'RUSV.last(ssd_iid)),
		       Int.toString(CPN'RUSV.count(ssd_iid)));
     addto_iid_logfile("std_iid",
		       CPN'RUSV.dataToString(CPN'RUSV.last(std_iid)),
		       Int.toString(CPN'RUSV.count(std_iid)));
     addto_iid_logfile("vari_iid",
		       CPN'RUSV.dataToString(CPN'RUSV.last(vari_iid)),
		       Int.toString(CPN'RUSV.count(vari_iid))))

fun init_monitor()
  = (SV.init(statvar);
     init_logfile();
     (if !is_active
      then let
	      val currentmarkings = get_markings()
	      val initobs = init(currentmarkings)
	  in
	      case initobs of 
		  NONE => ()
		| SOME obsval => action(obsval)
	  end
      else());
     if (!Replications.running_reps andalso 
	 Replications.get_rep_num()=0) 
	orelse
	((not(!Replications.running_reps)) andalso 
	 Sim.subrun()=0) 
     then init_iid()
     else ())

fun stop_monitor() = 
    (if !is_active
     then let
	     val stopobs = stop(get_markings())
	 in 
	     case stopobs of 
		 NONE => ()
	       | SOME obsval => action(obsval);
	     upd_iid()
	 end
     else ();
     close_logfile())

fun monitor (bindelem:bindelem) =
    if !is_active
    then let
	    val _ = CPN'debug (name^".monitor (is active)")
	    val currentsubnet = get_subnet bindelem
	in
	    if pred (currentsubnet)
	    then action(obs(currentsubnet))
	    else ()
	end
	    handle exn => (Sim.monitor_exn_source:=(name^".monitor");
			   raise exn)
    else ()

end (* CreateTimedDC *)


functor CreateBreakpoint(type bindelem 
			 and subnet
			 and markings
			 val pred: subnet -> bool
			 and get_subnet: bindelem -> subnet
			 and get_markings: unit -> markings
			 and name: string): MONITOR = struct

type bindelem = bindelem
type subnet = subnet
type markings = markings
val get_subnet = get_subnet
val get_markings = get_markings
val pred = pred

val name = name 

val is_active = ref true

fun monitor (bindelem:bindelem) = 
    if !is_active
    then let
	    val _ = CPN'debug (name^".monitor (is active)")
	    val currentsubnet = get_subnet bindelem
	in
	    if pred (currentsubnet)
	    then Sim.stop_simulator("Breakpoint: "^name)
	    else ()
	end
	    handle exn => (Sim.monitor_exn_source:=(name^".monitor");
			   raise exn)
    else ()

end (* end CreateBreakpoint *)


functor CreateUserDef(type bindelem 
		      and subnet
		      and markings
		      val get_subnet: bindelem -> subnet
		      val get_markings: unit -> markings
		      and pred: subnet -> bool
		      and init: markings -> unit
		      and stop: markings -> unit
		      and action_obs: subnet -> unit
		      and name: string): INITSTOPMONITOR = struct 

type bindelem = bindelem
type subnet = subnet
type markings = markings
val get_subnet = get_subnet
val get_markings = get_markings
val init = init
val stop = stop
val pred = pred
val action_obs = action_obs

val is_active = ref true

fun init_monitor () = 
    if !is_active
    then init(get_markings())
    else ()
    
fun stop_monitor () = 
    if !is_active
    then stop(get_markings())
    else ()

fun monitor (bindelem:bindelem) = 
    if !is_active
    then let
	    val _ = CPN'debug (name^".monitor (is active)")
	    val currentsubnet = get_subnet bindelem
	in
	    if pred (currentsubnet)
	    then action_obs currentsubnet
	    else ()
	end
	    handle exn => (Sim.monitor_exn_source:=(name^".monitor");
			   raise exn)
    else ()

end (* end CreateUserDef *)

functor CreateWriteFile (type bindelem 
			 and subnet
			 and markings
			 val get_subnet: bindelem -> subnet
			 val get_markings: unit -> markings
			 and pred: subnet -> bool
			 and obs: subnet -> string
			 and init: markings -> string 
			 and stop: markings -> string 
			 and fileext: string
			 and montype: CPN'MonitorTable.montype
			 and name: string): INITSTOPMONITOR = struct 

type bindelem = bindelem
type subnet = subnet
type markings = markings
val get_subnet = get_subnet
val get_markings = get_markings
val init = init
val stop = stop
val pred = pred
val obs = obs

val is_active = ref true

val file_name = (name^(if fileext=""
		       then ""
		       else "."^fileext))
val stream = ref (NONE: TextIO.outstream option)

fun close_file() = case (!stream) of
			  NONE => ()
			| SOME st => (TextIO.closeOut(st);
				     stream := NONE)
fun init_file() =
    if (!is_active)
    then 
	let
	    val filepath = 
		case montype of 
		    CPN'MonitorTable.step_monitor => 
		    (OS.Path.concat(Output.getSimOutputDir(),file_name))
		  | CPN'MonitorTable.sim_monitor => 
		    (OS.Path.concat(Output.getRepOutputDir(),file_name))
	in
	    case montype of 
		CPN'MonitorTable.step_monitor => Output.initSimOutputDir()
	      | CPN'MonitorTable.sim_monitor => Output.initRepOutputDir();
	    case (!stream) of
		NONE => (stream := (if (Sim.step()=(IntInf.fromInt 0))
				    then SOME(TextIO.openOut(filepath))
				    else (SOME(TextIO.openAppend(filepath))
					  handle IO => SOME(TextIO.openOut(filepath)))))
	      | _ => (close_file();
		      init_file())
	end
    else ()



fun addtofile(s:string) = 
    case (!stream) of
	NONE => (init_file();
		 addtofile(s))
      | SOME stream => (TextIO.output(stream, s);
			flush_out(stream))

fun action obsval = addtofile(obsval)

fun stop_monitor() = (if !is_active 
		      then let
			      val currentmarkings = get_markings()
			      val stopobs = stop(currentmarkings)
			  in
			      action(stopobs);
			      case (!stream) of
				  NONE => ()
				| SOME s => TextIO.flushOut(s)
			  end
		      else();
		      close_file())

fun init_monitor() = 
    (init_file();
     (if !is_active
      then let
	      val currentmarkings = get_markings()
	      val initobs = init(currentmarkings)
	  in
	      action(initobs);
	      case (!stream) of
		  NONE => ()
		| SOME s => TextIO.flushOut(s)
	  end
      else()))
	   
fun monitor (bindelem:bindelem) =
    if !is_active
    then let
	    val _ = CPN'debug (name^".monitor (is active)")
	    val currentsubnet = get_subnet bindelem
	in
	    if pred (currentsubnet)
	    then action(obs(currentsubnet))
	    else ()
	end
	    handle exn => (Sim.monitor_exn_source:=(name^".monitor");
			   raise exn)
    else ()

end (* end CreateWriteFile *)

end (* CPN'Monitor*)
