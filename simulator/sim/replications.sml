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
(* File: replications.sml
 *
 * For running multiple simulation replications
 *)

functor CPN'MakeReplications(structure Sim: CPN'SIM) : CPN'REPLICATIONS = struct

(* FIXME: any other obvious stop criteria?*)
datatype stop_crits = 
    until_rep of int
  | conf_int_precision of int (* confidence interval precision *) 

val stop_crits = ref (nil: stop_crits list)

(* Replications can be stopped via user request *)
local
    val stopReq = ref false
    val stopRequestReason = ref ""
in
    fun stop_replications(reason_str) = (stopRequestReason:=reason_str;
					 stopReq:=true)
    fun is_stop_requested() = !stopReq
    fun clear_stop_req() = (stopRequestReason:="";
			    stopReq:=false)
end

val running_reps = ref false
val cur_rep = ref 0
fun get_rep_num() = !cur_rep

val error_msg = "Replication run stopped due to error during simulation!"
val stop_crit_msg = ref ([] : string list)

fun check_stop_criteria () = 
    let
	fun check_crit (until_rep n) = 
	    (get_rep_num()) = n 
	  | check_crit _ = false (* FIXME *)
    in
	not (List.exists (fn b => b ) (map check_crit (!stop_crits)))
    end


fun insert_fun (name:string,f,funlistref) = 
    let
	fun insert elm [] = [elm]
	  | insert (name,f) ((n,f')::tail) = 
	    if (name=n)
	    then (n,f)::tail
	    else (n,f')::(insert (name,f) tail)
    in
	funlistref := (insert (name,f) (!funlistref))
    end

(* Functions called before first replication is run *)
val init_rep_funs = ref ([]: (string * (unit -> unit)) list)
fun insert_init_rep_fun (name:string,f:(unit->unit)) = 
    insert_fun(name,f,init_rep_funs)

(* Functions called after stop criteria is met, int is # of reps run *)
val stop_funs = ref ([]: (string * (int -> unit)) list)
fun insert_stop_fun (name:string,f:(int->unit)) = 
    insert_fun(name,f,stop_funs)

(* Functions called before each replication is run *)
val pre_rep_funs = ref ([]: (string * (unit -> unit)) list)
fun insert_pre_rep_fun (name:string,f:(unit->unit)) = 
    insert_fun(name,f,pre_rep_funs)

(* Functions called after each replication is run *)
val post_rep_funs = ref ([]: (string * 
			      (string * string * string  -> unit)) list)
fun insert_post_rep_fun (name:string,f:(string*string*string->unit)) = 
    insert_fun(name,f,post_rep_funs)

(* EXAMPLES OF USE:
 * Open batch status file monitor, 
 * initialise batch data collector monitors,
 * reset random seeds 
 *)
fun invoke_init_funs() = 
    (CPN'debug("Invoke replication init funs");
     app (fn (s,f) => f()) (!init_rep_funs))


(* EXAMPLES OF USE:
 * Save batch performance report (with confidence intervals),
 * close batch status file monitor,  
 * save gnuplot scripts
 *)
fun invoke_stop_funs(n) = 
    (CPN'debug("Invoke replication stop funs after "^Int.toString(n)^"reps.");
     app (fn (s,f) => f(n)) (!stop_funs))


(* EXAMPLES OF USE:
 * Initialise step data collectors,
 * set parameter values
 *)
fun invoke_pre_funs() = 
    (CPN'debug("Invoke pre-replication funs");
     app (fn (s,f) => f()) (!pre_rep_funs))


(* EXAMPLES OF USE:
 * Update batch status file,
 * save simulation performance report,
 * update batch data collector monitors, 
 *)
fun invoke_post_funs(sim_msg) = 
    (CPN'debug("Invoke post-replication funs");
     app (fn (s,f) => f(sim_msg)) (!post_rep_funs))

(* Timer for running sims during replications *)
val rep_real_timer = ref (NONE: Timer.real_timer option)

(* Replication reports *)
val update_report = ref true;
val report_file_name = ref "replication_report.txt"
val report_full_path = ref ""
val report_file = (ref NONE): (TextIO.outstream option ref);

val user_init_report_fun = (ref NONE): ((unit -> string) option) ref
val user_update_report_fun = (ref NONE): ((unit -> string) option) ref
val user_close_report_fun = (ref NONE): ((unit -> string) option) ref

fun open_report fname =
    let
	val absfilename = Output.myConcat(Output.getRepOutputDir(),fname)
	val _ = Output.initRepOutputDir()
	val _ = report_full_path := absfilename
    in
	report_file:=
	((SOME(TextIO.openAppend absfilename)) 
	 (* openAppend doesn't work on Windows if file does not exist *)
	 handle _ => SOME(TextIO.openOut absfilename))
    end

fun close_report () = 
    case (!report_file) of
	NONE => ()
      | SOME fd => (TextIO.closeOut (fd);
		    report_file:=NONE)

fun write_report_entry str = 
    let
	fun write_entry str =
	    (TextIO.output(valOf(!report_file),str);
	     TextIO.flushOut(valOf(!report_file)))
    in
	if (!update_report) then
	    case (!report_file) of
		NONE => 
		    (open_report (!report_file_name);
		     case (!report_file) of
			 NONE => ()
		       | SOME file => write_entry str)
	      | SOME file => write_entry str
	else 
	    case (!report_file) of
		NONE => ()
	      | SOME file => close_report()
    end

fun init_report() = 
    (write_report_entry ("CPN Tools report for simulation replications\n"^
			 "Net: "^
			 Output.myConcat(Output.getModelDir(),
					Output.getModelName())^
			 "\nOutput directory: "^
			 Output.getRepOutputDir()^"\n\n");
     (case !user_init_report_fun of
	  NONE => ()
	| (SOME initfun) => write_report_entry((initfun())^"\n\n")))

fun update_report (stepstr,timestr,reasonstr) = 
    write_report_entry (
	 "Simulation no.: "^(Int.toString (get_rep_num()))^"\n"
        ^"Steps.........: "^stepstr^"\n"
        ^(if timestr<>"" then	 
	   "Model time....: "^(timestr)^"\n" 
	  else "")
	^"Stop reason...: "^reasonstr^"\n"^
	(case (!rep_real_timer) of 
	     NONE=> ""
	   | SOME rt => "Time to run simulation: "^
			(SMLTime.fmt 0 (Timer.checkRealTimer rt))^
			" seconds")^"\n"
	^(case !user_update_report_fun of
	      NONE => ""
	    | SOME updatefun => updatefun())^
	"\n\n")

fun updateAndclose_report() = 
    (case !user_close_report_fun of
	 NONE => ()
       | SOME closefun => write_report_entry(closefun());
     close_report())

fun init() = (cur_rep:= 0;
	      running_reps := true;
	      init_report())

(* FIXME: Should Sim.init_all be called before invoking init funs?
 * Is it a problem if Output.setSimOutputDir is called after
 * invoking the pre funs? It is called via Sim.init_all *)
fun run() = 
    (CPN'debug("Starting replication runs...");
     Output.enableRepDir();
     init();
     invoke_init_funs(); 
     while check_stop_criteria() do
	 (invoke_pre_funs();
	  (* FIXME: Currently assumed that terminating sims are run
	   * i.e. no good support for steady state *)
	  Sim.init_all();  
	  rep_real_timer := (SOME (Timer.startRealTimer()));
	  let
	      (* returns 3 strings: step, stop time, & reason *)
	      val (stepstr, timestr,reasonstr) =  Sim.run()
	      val _ = inc cur_rep;
	      val _ = update_report(stepstr,timestr,reasonstr)
	      val _ = if String.isPrefix "Error:" reasonstr
		      then (write_report_entry (error_msg);
			    raise CPN'Stop (error_msg^"\n\t"^reasonstr))
		      else ()
	      val _ = CPN'debug ("Replication "^Int.toString(get_rep_num())^
				 " finished\n"^reasonstr)
	  in
	      rep_real_timer := NONE;
	      invoke_post_funs(stepstr,timestr,reasonstr)
	  end
	  );
     invoke_stop_funs(get_rep_num());
     updateAndclose_report();
     Output.disableRepDir();
     running_reps := false;
     CPN'debug("Stopping replication runs")
     )
    handle exn => (CPN'debug("Exception during CPN'Replications.run "^
			     (exnName exn));
		   write_report_entry("\nUnhandled exception while running replications: "
				      ^(exnName exn)^
				      (if (exnName exn)<>(exnMessage exn)
				       then ", with message: "^exnMessage exn
				       else "")^"\n")
		   handle e => () (* ignore exceptions, usually I/O *);
		   close_report();
		   Output.disableRepDir();
		   raise exn)

fun nreplications n = 
    if n < 2 
    then raise CPN'Error ("Error: number of replications requested ("^
			      (Int.toString n)^") must be more than 1.")
    else (stop_crits := [until_rep n];
	  run())

end (* CPN'Replication *)
