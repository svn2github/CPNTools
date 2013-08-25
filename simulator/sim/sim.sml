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
(* File: sim.sml
 *
 * Simulation facilities.
 *)
functor CPN'MakeSim(structure Options: CPN'OPTIONS
		    and InstTable: CPN'INSTTABLE
		    and Places: CPN'PLACES
		    and References: CPN'REFERENCES) : CPN'SIM = struct

structure Time = Options.Time;
structure Options = Options;


(*****************************  Priority queue *****************************)
structure PQ = MakeArrayPQ (Time);


(************************** Types and exceptions ***************************)

datatype status = unknown | disabled | maybe_ready_at of Time.time
datatype result = is_executed | is_disabled | is_maybe_ready_at of Time.time

datatype mode = test | fast | bind of bool | all_enabled 
	      | pick of (string * string) list
    
exception Exit

(***************** Tables with bind and execute functions ******************)

structure BETable = struct

    type item = (mode * int -> result * string list) * (* be *)
                (mode * int -> int * (result * string list) * (unit -> result * string list)) * (* bf *)
                int (* priority *)

    val table: (CPN'Id.id,item) HashTable.hash_table = 
	HashTable.mkTable hashId (19,InternalError("BETable.find"))

    val insert = HashTable.insert table
    val remove = HashTable.remove table
    val find = HashTable.lookup table
    val peek = HashTable.find table
    fun list () = HashTable.listItemsi table
end

type inst_dump = {name: string,
		  index: int * int,
		  dep_list: int list list,
              priority: int}

(* FIXME: Cleanup usage of DumpTable in this file *)
structure DumpTable = struct

    val table: (CPN'Id.id,inst_dump) HashTable.hash_table = 
	HashTable.mkTable hashId (19,InternalError("DumpTable.find"))

    val no_of_tis = ref 0
	
    val insert = HashTable.insert table
    val remove = HashTable.remove table
    val find = HashTable.lookup table
    val peek = HashTable.find table
    fun list () = HashTable.listItemsi table
end

(* FIXME: remove following ref when code dump has been impr*)
val generate_instances = ref true

fun bind_fair bindings occfun picker bind_exe extract (m, inst) =
    case bind_exe (test, inst)
      of (is_executed, msg) =>
      let
          val b = bindings inst
          val (count, b') = extract b
      in
          if m = test
          then
              (count, (is_executed, msg), fn () => occfun (inst, picker b', true))
          else 
              let
                  val result = occfun (inst, picker b', true)
              in
                  (count, result, fn () => result)
              end
      end
       | v => (0, v, fn () => v)

    val filters = ref []: (string * {
           check : CPN'Id.id * int -> bool,
           execute : CPN'Id.id * int -> unit,
           reset: unit -> unit }) list ref

      fun check_filters (t, i) [] = true
        | check_filters (t, i) ((_, { check, execute, reset })::rest) =
        if (check (t, i))
        then check_filters (t, i) rest
        else false

        fun execute_filters (t, i) [] = ()
        | execute_filters (t, i) ((_, { check, execute, reset })::rest) =
        (execute (t, i); execute_filters (t, i) rest)

        fun reset_filters [] = ()
        | reset_filters ((_, { check, execute, reset })::rest) =
        (reset (); reset_filters rest)

        fun wrap_filter id f (mode, inst) =
            if check_filters (id, inst) (!filters)
            then case f (mode, inst)
                   of (is_executed, errors) =>
                   let
                       val _ = execute_filters (id, inst) (!filters)
                   in
                       (is_executed, errors)
                   end
                    | (result, errors) => (result, errors)
            else (is_disabled, [])

        fun wrap_filter_fair id f (mode, inst) =
            if check_filters (id, inst) (!filters)
            then case f (mode, inst)
                   of (count, (is_executed, errors), bind_exe) =>
                   let
                       val _ = execute_filters (id, inst) (!filters)
                   in
                       (count, (is_executed, errors), bind_exe)
                   end
                    | result => result
            else (0, (is_disabled, []), fn () => (is_disabled, []))

fun add_be (t,bf, bfair, priority) = 
    (if !generate_instances then let
	open InstTable
	    
	val (m,n) = get_t_index t
	    
	val dep_list = 
	    CPN'Misc.I.fold (fn (a,b) => get_dep_list(t,a)::b) (1,n-m+2) nil
    in
	 I.+= (DumpTable.no_of_tis,n-m+1);
	case CPN'TransitionTable.peek t of
	    SOME (CPN'TransitionTable.transition{name,output,...}) =>
            DumpTable.insert (t,{name=name,index=(m,n),dep_list=dep_list,priority=priority})
	  | _ => raise InternalError "add_be"
    end else ();
    BETable.insert (t, (wrap_filter t bf, wrap_filter_fair t bfair, priority)))

fun dump_inst () = let

    fun dump ((t,{name,index=(m,n),dep_list,priority}),tail) =
	","::"("::CPN'Id.toString t::",{name=\""::name::"\",\
	 \ index=("::Int.toString m::","::Int.toString n::"),\
       \ priority="::Int.toString(priority)::",\
	 \ dep_list="::
	(CPN'Misc.makelist (CPN'Misc.makelist Int.toString) dep_list)::
	"})"::tail
in
    (app (fn str => CPN'CodeGen.dump_source str) 
     ("\n val _ = CPN'Sim.load_inst ("::Int.toString(!DumpTable.no_of_tis)::
      ",["::tl(foldr dump ["","]);"] (DumpTable.list())));
     CPN'CodeGen.term_dumping();
     generate_instances:= false)
end
	
fun load_inst (n,list) = 
    (DumpTable.no_of_tis:= n; app DumpTable.insert list)

(****************************** The Top Loop *******************************)

val dummy_ti = {id= CPN'Id.base,
name= "", 
status= ref unknown, 
dep_list= [0], 
inst= 0, 
priority= ~1,
bind_exe= fn (_: mode * int) => (is_disabled,[""]),
bind_fair = fn (_: mode * int) => (0, (is_disabled, [""]), fn () =>
(is_disabled, [""]))
}

val transitions = ref(Array.array(0,dummy_ti))
val prioritizedtransitions : ((int * int * CPN'Id.id * int) list ref) = ref []

val priofun = fn n => (#priority (Array.sub(!transitions, n))) 

val unknowns = ref(PQRS.create 0 priofun)
val maybe_readies = ref(PQ.create 0)

val model_time = Time.model_time

val cur_subrun = ref 0
val warmup_length = ref Time.null 
val warmup_elapsed = ref false

val cur_step = ref (IntInf.fromInt 0)
val stop_step = ref (IntInf.fromInt 0)
val stop_time = case Time.start_time of
    NONE => ref Time.null
  | SOME t => ref t

(* Simulator can be stopped via software request *)
local
    val softwareStopReq = ref false
    val stop_request_reason = ref ""
in
    fun stop_simulator(reason_str) = (stop_request_reason:=reason_str;
				      softwareStopReq:=true)
    fun is_software_stop_req() = !softwareStopReq
    fun clear_software_stop_req() = (stop_request_reason:="";
				     softwareStopReq:=false)
    fun get_software_stop_reason() = !stop_request_reason
end

    (* Handling stop criteria messages *)

    val stop_crit_msg = ref(nil: string list)   

    val stop_crit_header = "The following stop criteria are fulfilled:"
    val disabled_msg = "The transition instance is disabled!"
    val be_disabled_msg = "The (partially) specified binding element is disabled!"
    val executed_msg = "The transition instance has occured!"
    val be_executed_msg = "The transition instance has occured with a (partially) specified binding!"
    val no_enabled_msg = ref "No more enabled transitions!"
	
    fun no_enabled() = stop_crit_msg::= !no_enabled_msg 

    fun error_exn_msg (exn,name) = 
	stop_crit_msg::= concat 
	 ["Error: The exception ",exn," is raised while transition ",
	  name," occured!"];

    fun critical_error_exn_msg (exn,name) = 
	stop_crit_msg::= concat 
	 ["Error: The exception ",exn," is raised outside the code region while\
	  \ transition ",name," occured, and the state might be incorrect!"];

    fun internal_error_msg (msg,name) =
	stop_crit_msg::= concat
	 ["Error: An Internal Error occured while transition ",name," occured!\n",
	  "Internal Error: ",msg];

    fun cancel_exn_msg (msg,name) = 
	stop_crit_msg::= concat 
	 ["Error: The simulation stopped while ",name," occured because of:\n", msg]; 

    fun cancel_man_bind_msg (name) = 
	stop_crit_msg::= concat
	["Manual binding of transition ",name," has been cancelled."]

    fun stop_exn_msg msg = stop_crit_msg::= msg;

    val monitor_exn_source = ref ""
    fun get_monitor_exn_source() = 
	(!monitor_exn_source^"\n") before monitor_exn_source:=""

val report_ref = ref (nil: string list)
val report_file_prefix = ref "simrep-"
val report_file_name = ref "" (* ("/tmp/simrep-"^(Word32.toString(OSDep.getpid()))^".txt")*)
val report_file = ref (NONE: TextIO.outstream option)

fun find_max_report_num (dirname) = 
    if not(OS.FileSys.access (dirname,[])) 
    then 0 (* dir does not yet exist, return 0 *)
    else let
	    val theDir = (OS.FileSys.openDir dirname)
	    val firstEntry = (OS.FileSys.readDir theDir)
			     
	    fun maxReportNum (SOME "", dir,strprefix) = 0
	    fun maxReportNum (NONE, dir,strprefix) = 0
	      | maxReportNum (SOME theEntry, dir,strprefix) = 
		if not (String.isPrefix strprefix theEntry)
		then maxReportNum(OS.FileSys.readDir dir, dir, strprefix)
		else
		    let
			val isfile = not (OS.FileSys.isDir 
					      (Output.myConcat(dirname,theEntry)))
			val rest = 
			    if isfile
			    then String.extract (theEntry,String.size(strprefix),
						 NONE)
			    else "" (* a dummy string *)
			val noextension = 
			    case rev(String.explode rest) of
				(#"t")::(#"x")::(#"t")::(#".")::charlist => 
				rev charlist
			      | _ => []
			val areDigits = (length noextension > 0) andalso 
					List.all Char.isDigit (noextension)
					
			val curnum =  if areDigits 
				      then Option.getOpt(Int.fromString (implode noextension),0)
				      else 0
			val nextEntry = (OS.FileSys.readDir dir)
		    in
			Int.max (curnum, maxReportNum (nextEntry, dir,strprefix))
		    end
	    val max = maxReportNum (firstEntry,theDir, !report_file_prefix)
	    val _ = OS.FileSys.closeDir theDir
	in
	    max
	end

    fun open_report() =
	if (!report_file_name)<>""
	then (* The report has been opened previously in this sim *) 
	    report_file:=SOME(TextIO.openAppend (!report_file_name))
	else 
	    let
		val dirname = Output.getSimOutputDir()
		val _ = Output.initSimOutputDir()
		val n = (find_max_report_num (dirname))+1
		val filename = Output.myConcat (dirname, (!report_file_prefix)^
							Int.toString(n)^".txt")
	    in 
		report_file_name := filename;
		report_file:= (SOME(TextIO.openOut filename) 
			       handle _ => NONE);
		case (!report_file) of
		    NONE =>  ()
		  | SOME _ =>  
		    (write_report_entry 
			 ("CPN Tools simulation report for:\n"^
			  Output.myConcat(Output.getModelDir(),
					 Output.getModelName())^
			  "\nReport generated: "^
			  (Date.toString(Date.fromTimeLocal(SMLTime.now())))^
			  "\n");
			 ())
	    end
		handle NotValidDirExn s => 
		       raise NotValidDirExn ("Cannot open file for simulation report because:\n"^s)

    and close_report () 
      = case (!report_file) of
	    NONE => ()
	  | SOME fd => (TextIO.closeOut (fd);
			report_file:=NONE)
		       
    and write_report_entry str =
	let
	    fun write_entry str =
		(TextIO.output(valOf(!report_file),str);
		 TextIO.flushOut(valOf(!report_file));
		 str)
		
	in
	    if (!Options.report_transitions) then
		case (!report_file) of
		    NONE => 
		    (open_report();
		     case (!report_file) of
			 NONE => str
		       | SOME file => write_entry str)
		  | SOME file => write_entry str
	    else 
		case (!report_file) of
		    NONE => str
		  | SOME file => (close_report (); str)
	end

val time_inc_funs = ref (nil: (Time.time * Time.time -> unit) list)

(* add element to list if name is new, otherwise update element *)
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

val step_inc_funs = ref (nil: (string *(unit -> unit)) list)
fun insert_step_inc_fun (name:string,f:(unit->unit)) = 
    insert_fun(name,f,step_inc_funs)

val init_state_funs = ref (nil : (string * (unit -> unit)) list)
fun insert_init_state_fun (name:string,f:(unit->unit)) = 
    insert_fun(name,f,init_state_funs)

(* step and time at which stop funs were last called 
val stop_funs_called_null = (0, case Time.start_time of 
				    NONE => Time.null
				  | SOME t => t )*)
val stop_funs_called = ref (IntInf.fromInt 0)
val stop_funs = ref (nil : (string * (unit -> unit)) list)
fun insert_stop_fun (name:string,f:(unit->unit)) = 
    insert_fun(name,f,stop_funs)

fun call_stop_funs() = 
    if !cur_step = !stop_funs_called
    then (CPN'debug("Stop funs already called after current step."))
    else (CPN'debug("Calling sim stop funs, step="^
		    IntInf.toString(!cur_step)^", time="^
		    Time.mkstr(Time.time()));
	  app (fn (fstr,f) => f()) (!stop_funs)
	  handle NotValidDirExn s => stop_exn_msg("\nError: NotValidDirExn in simulation stop funs\n"^(if !monitor_exn_source="" then "" else get_monitor_exn_source())^s)
	       | exn => stop_exn_msg("Error: exception raised in simulation stop funs: \n"^exnName exn^(if !monitor_exn_source="" then "" else "\n"^get_monitor_exn_source()));
	  stop_funs_called := !cur_step)

val stop_crit_funs = ref (nil : (string * (unit -> unit)) list)
fun insert_stop_crit_fun (name:string,f:(unit->unit)) = 
    insert_fun(name,f,stop_crit_funs)

fun CPN'debug_dump_intlist nil msg = (CPN'debug ((" ## ")^msg))
  | CPN'debug_dump_intlist (elm::tail) msg =
    (CPN'debug ("::"^(Int.toString elm));
     CPN'debug_dump_intlist tail msg);

(* FIXME: remove following ref when code dump has been impr*)
val instances_changed = ref true

(* Print contents of transitions array. For debugging. *)
fun print_scheduler_status() =
    Array.appi 
    (fn (idx,{id=id,name=n,status=ref s,dep_list=dl,inst=i,priority,bind_exe=_,
    bind_fair})=> print ("["^(Int.toString idx)^"]: id="^id^"("^(Int.toString i)^") status="^(case s of unknown=>"unknwn" | disabled=>"disabl" | maybe_ready_at t =>("maybe("^(Time.toString t)^")"))^" dep=["^(if null(dl) then "" else (CPN'concat(tl(CPN'Misc.flatten(map (fn el=> ","::[Int.toString el]) dl)))))^"]"^" name="^n^"\n"))
    (!transitions);

(* Build the internal structures of the scheduler of simulation engine.
 * Should be called if the instance table has changed.
 * It is similar to  create_scheduler except that this function is use in 
 * stand-alone mode only (cf. demo/bootstrap_simulator.sml). *)
fun create_scheduler_standalone() =
    let
	fun create() =
	    let
		val _ = 
		    if Array.length(!transitions) <> !DumpTable.no_of_tis then
			transitions:= Array.array(!DumpTable.no_of_tis, dummy_ti)
		    else ()

              fun create_ti (t,(be, bf, priority)) = 
                let
                  val {name,index=(m,n),dep_list,priority} = DumpTable.find t

			fun create_i (nil,i) = 
			    if i>n-m+2 then raise InternalError "Sim.create_ti" else ()
			  | create_i (d::ds,i) =
				(Array.update(!transitions, m+i-1,
					      {id= t,
					       name= name,
					       status= ref unknown,
					       dep_list= d,
					       inst= i,
                                     priority= priority,
                                     bind_exe= be,
                                     bind_fair = bf});
				 create_i (ds,i+1))
		    in
			create_i (dep_list,1)
		    end
	    in
		app create_ti (BETable.list())
	    end
    in
	CPN'debug "create_scheduler_standalone";
	CPN'report_timing(concat["create_scheduler_standalone @ "]);
	if !instances_changed then (create(); instances_changed:= false) else ()
    end;

(* Build the internal structures of the scheduler of simulation engine.
 * Should be called if the instance table has changed. *)
fun create_scheduler() =
    let
	(* FIXME: is this too much overhead? *)
	val _ = InstTable.init()

	val _ = CPN'debug "create_scheduler";
	val _ = CPN'report_timing(concat["create_scheduler @ "]);

	val ti_indices= InstTable.get_ti_indices()
	val no_of_tis= foldr (fn ((_,(m,n)),sum)=> sum+(n-m+1)) 0 ti_indices
	val _ = transitions:= Array.array(no_of_tis, dummy_ti)
      val prioritysorted = ref []

	fun insert_ti t = 
	    let
		val name= 
		    case CPN'TransitionTable.peek t of
			SOME (CPN'TransitionTable.transition{name,...}) => name
		      | _ => raise InternalError ("Sim.insert_ti peek "^t)
            val (be, bf, priority)= BETable.find t
		    handle (InternalError "BETable.find") => 
			   (CPN'debug("BETable.find in create_scheduler t="^t);
			    raise InternalError "BETable.find")
		val (m,n) = InstTable.get_t_index t
		val dep_list = CPN'Misc.I.fold 
		    (fn (a,b) => InstTable.get_dep_list(t,a)::b) (1,n-m+2) nil

		fun create_i (nil,i) = 
		    if i>n-m+2 then raise InternalError "Sim.insert_ti" else ()
		  | create_i (d::ds,i) =
			(Array.update(!transitions, m+i-1,
				     {id= t,
				      name= name,
				      status= ref unknown,
				      dep_list= d,
				      inst= i,
                              priority= priority,
                              bind_exe= be,
                              bind_fair = bf
                              });
                              prioritysorted := ((priority, m + i - 1, t, i)::(!prioritysorted));
			create_i (ds,i+1))
	    in
		create_i (dep_list,1)
	    end
    in
	app insert_ti (map (fn (t,_)=>t) ti_indices);
      prioritizedtransitions := (ListMergeSort.sort (fn ((a, _, _, _), (b, _, _, _)) => a > b) (!prioritysorted))
    end

(* Reset the transition scheduler internal structures*)
fun reset_scheduler () =
    let
	val _ = CPN'debug "reset_scheduler";
	val _ = CPN'report_timing(concat["reset_scheduler @ "]);

	val n = Array.length (!transitions)
	
	fun set_unknown 0 = ()
	  | set_unknown i = 
	    ((#status(Array.sub(!transitions,i-1))):= unknown; 
	     set_unknown(i-1))
    in
	set_unknown n;
      unknowns:= PQRS.create n priofun;
	(case Time.start_time of 
	     NONE => () 
	   | SOME t => maybe_readies:=PQ.create n)
    end;

(* Reset simulation engine *)
fun reset_sim () =
    (
     CPN'debug "reset_sim";
     CPN'report_timing(concat["reset_sim @ "]);

     (* Call the stop functions if they haven't already been called *)
     call_stop_funs();
     stop_funs_called := 0;

     (* Reset step and time *)
     stop_step:= (IntInf.fromInt 0);
     cur_step:= (IntInf.fromInt 0);
     cur_subrun:=0;
     case Time.start_time of 
	 NONE => () 
       | SOME t => (stop_time:= t;
		    model_time:= t;
		    warmup_elapsed := false);

     (* Init random generator *)
     if !Options.reset_ran_gen then 
	 (CPN'Random.init (!Options.seed))
     else ();
     clear_software_stop_req();
     (* Init sim report *)
     close_report();
     report_file_name := ""
    );

fun init_state () =
    (
     CPN'debug "init_state";
     CPN'report_timing(concat["init_state @ "]);
     CPN'stop_timing ();

     (* Init references *)
     if !Options.reset_ref_vars then 
	 (!References.init())
     else ();

     (* Init markings *)
     Places.set_init_mark();
     
     Output.setSimOutputDir();

     reset_filters (!filters);

     CPN'debug "Calling init_state_funs...";
     app (fn (fname,f) => f()) (!init_state_funs)
     handle exn => 
	    raise CPN'Error((exnName exn)^
			    "raised when calling init state funs"^
			    (if !monitor_exn_source="" 
			     then "" 
			     else " in \n"^
				  get_monitor_exn_source()))
     )

fun init_all() = (create_scheduler();
		  reset_scheduler();
		  reset_sim();
		  init_state())

(* The main scheduler runtime system follows *)
local
    fun mark_dependents deps =
    let
    fun mark_dependents'  nil = ()
      | mark_dependents' (x::xs) = let
	val {status,...} = Array.sub(!transitions,x) handle exn => (CPN'debug (" mark_dependents raise exn "^(exnName exn)^":"^(Int.toString (Array.length (!transitions)))^"<="^(Int.toString x)); dummy_ti)
    in
	case !status of
	    unknown => mark_dependents xs
	  | disabled =>
		(status:= unknown;
		 PQRS.insert (unknowns,x);
		 mark_dependents xs)
	  | maybe_ready_at(time) => 
		(status:= unknown;
		 PQ.delete (maybe_readies,x);
		 PQRS.insert (unknowns,x);
		 mark_dependents xs)
       end
    in
        if (List.null (!filters))
        then mark_dependents' deps
        else (reset_scheduler (); ())
    end

    local
	(* FIXME: This apparently does not work in the toploop.
	 * It does however work when on the top-level. *)

	val userRequest1 = ref 0
	val userRequest1Handler = (fn n => userRequest1 := 1);

	fun installUserRequest1Handler h = (Signals.setHandler(valOf(Signals.fromString("USR1")), 
							       Signals.HANDLER(fn (_,n,c) => (h n; c))); ());

    in
	fun enable_remote_stop() = installUserRequest1Handler userRequest1Handler
	fun is_stop_request() = !userRequest1>0 
	fun accept_stop_request() = userRequest1:= 0

    end

    (* handling of stop criteria *)

    val make_trans_report =
	case Time.start_time of 
	    NONE => (fn (name,inst: int,pgname,tail) =>
		     if !Options.report_transitions then
			 "\n"::IntInf.toString(!cur_step)::"\t"::name::" @ ("::(Int.toString inst)::":"::pgname::")"::tail
		     else tail)
	  | SOME _ => (fn (name,inst: int,pgname,tail) =>
		       if !Options.report_transitions then
			   "\n"::IntInf.toString(!cur_step)::"\t"::
			   Time.mkstr(!model_time)::"\t"::name::" @ ("::(Int.toString inst)::":"::pgname::")"::tail
		       else tail)


    fun make_func_report (id,inst) =
	case (!Options.report_function) of
	    NONE => ()
	  | SOME func => let
			     val str = func(id,inst)
			 in 
			     (write_report_entry str;())
			 end

    fun make_report (id,name,inst,report) =
	(if !Options.report_transitions then
	     let
		 val pgname = (#name (CPN'PageTable.get_page(CPN'TransitionTable.get_page id)))
		 val str = concat(make_trans_report(name,inst,pgname,report))
	     in 
		 (write_report_entry str;())
	     end
	 else ();
	 make_func_report(id,inst))

    (* FIXME: not finished *)
    fun check_warmup() = 
	if !warmup_elapsed
	then ()
	else () 


in
    fun check_stop_crit () = let

	fun add msg =
	    case !stop_crit_msg of 
		 nil => stop_crit_msg:= [msg, stop_crit_header]
	       | _ => stop_crit_msg::= msg

	fun check (Options.until_step n) =
	    if n = !cur_step then
		add ("\n - Until Step Number is "^(IntInf.toString n))
	    else ()
	  | check (Options.additional_steps n) =
	    if n + !stop_step = !cur_step then
		add ("\n - Additional Steps "^(IntInf.toString n))
	    else ()
	  | check (Options.until_time t) =
	    if Time.ready t then
		(model_time:= t;
		 add ("\n - Until Time is "^(Time.mkstr t)))
	    else ()
	  | check (Options.additional_time t) =
	    if Time.ready(Time.add(!stop_time,t)) then    
		 (model_time:= Time.add(!stop_time,t);
		  add ("\n - Additional Time "^(Time.mkstr t)))
	    else ()
		     
    in
(* THIS DOES NOT CURRENTLY WORK
        if is_stop_request() then 
	   (CPN'debug " --- SIGNAL USR1 WAS DETECTED ---";
	    accept_stop_request();
	    add "\n - Simulation stopped by user (signal USR1)")
	else ();
*)
	    
	app check (!Options.stop_crits);
	app (fn (fstr,f) => f()) (!stop_crit_funs);
        if is_software_stop_req() then 
	   (add ("\n - "^get_software_stop_reason());
	    clear_software_stop_req())
	else ();
	if (!stop_crit_msg)=nil
	then true
	else (call_stop_funs();
	      false)
    end

    fun increase_time() = 
	let
	    fun move j =
		(#status(Array.sub(!transitions,j)):= unknown; 
	         PQRS.insert (unknowns,j))
		
	    val time = PQ.min (!maybe_readies);
	in
	    (* Hook for calling functions when time is increased *)
	    app (fn f => f(!model_time, time)) (!time_inc_funs);
	    (if Time.lt(time,!model_time) then 
	         raise InternalError "increase_time: model time decreased"
	     else
	         model_time:= time);
	    app move (PQ.deleteto(maybe_readies, time));
	    if check_stop_crit() then time else raise Exit
        end

    fun make_response() =
        (stop_step:= !cur_step;
	 stop_time:= !model_time;
	 (IntInf.toString(!cur_step), 
	  if isSome(Time.start_time) then Time.mkstr(!model_time) else "",
	  concat(rev(!stop_crit_msg))) before stop_crit_msg:= nil)

    fun inst() = !CPN'inst

    fun inc_step() = (cur_step := (!cur_step) + 1)
    fun step() = !cur_step

    fun subrun() = !cur_subrun

    fun set_warmup_length (warmup:Time.time) = 
	case Time.start_time of
	    SOME st => if Time.lt (warmup,Time.null)
		       then raise CPN'Error "Error: warmup length cannot be negative!"
		       else warmup_length := warmup
	  | NONE => raise InternalError "Error: cannot set warmup length when start time is NONE!"

    (* temporary hook for setting sim report options *)
    fun set_report_options(reptrans,repbinds) =
	(Options.report_transitions := reptrans;
	 Options.report_bindings := (repbinds andalso reptrans))

    fun save_report (filename: string) =
	(close_report();
	 OS.FileSys.rename {new=filename,old=(!report_file_name)})

    fun clear_report () = 
	(report_ref:= nil;
	 close_report ();
	 TextIO.closeOut(TextIO.openOut(!report_file_name))
	     handle _ => (CPN'debug ("sim.clear_report: close -- did not ("^(!report_file_name)^")")))

    (* Check enabling without using scheduler structures. Useful when
     * a transition instance is available but not the scheduler. *)
    fun check_enab_no_scheduler (t,i) = 
	let
          val (bind_exe, bind_fair, priority)= BETable.find t
     in
         if check_filters (t, i) (!filters)
         then let
          val index = InstTable.get_ti_index (t,i)
     in
	    case bind_exe (test,i) of
		(is_executed,_) => true
	      | _ => false
               end else false end

    fun remove_filter name =
    let
        fun rm [] = []
          | rm ((n, f)::r) =
            if n = name
            then rm r
            else (n, f)::(rm r)
    in
        filters := (rm (!filters))
    end

    fun add_filter (name, filter) =
    let
        val _ = remove_filter name
        val _ = filters := ((name, filter)::(!filters))
    in
        ()
    end

    fun get_lowest_enabled_priority bound_priority =
    let
      fun get_priority [] = bound_priority
        | get_priority ((priority, _, id, inst)::rest) =
        if bound_priority <= priority
        then bound_priority
        else if check_enab (id, inst)
             then priority
             else get_priority rest
    in
      get_priority (!prioritizedtransitions)
    end

    and check_enab (t,i) = let
	val _ = CPN'debug ("check_enab "^t^" "^(Int.toString i))
	val index = InstTable.get_ti_index (t,i)
      val {bind_exe,bind_fair, inst,dep_list,status,name,priority,id} = 
	    Array.sub(!transitions,index)
      val lowest_priority = get_lowest_enabled_priority priority
    in
	CPN'inst:= inst;
      if lowest_priority >= priority
      then
        case !status of
            unknown =>
              (case bind_exe (test,inst) of
                    (is_executed,_) => true
                  | (is_disabled,_) => 
                      (status:= disabled;
                       PQRS.delete(unknowns,index);
                       false)
                  | (is_maybe_ready_at(time),_) =>
                      (status:= maybe_ready_at(time);
                       PQRS.delete (unknowns,index);
                       PQ.insert (maybe_readies,time,index);
                       false))
           | _ => false
      else false
    end

    fun run_fast() = 
	(while check_stop_crit() do let
	    (* There was problems getting the loop to be tail recursive
	     * (the image grows if it is not tail recursive). 
	     * This construct ensures that it is tail recursive 
	     *)
	    val _ = if check_stop_crit() then () else raise Exit; 

	    val index = PQRS.ran(unknowns) handle PQRS.EmptyRanSet =>
		(increase_time();
		 PQRS.ran (unknowns))
  
	    val {bind_exe,bind_fair,inst,dep_list,status,name,priority,id} = 
		Array.sub(!transitions,index)
	in
          CPN'inst:= inst;
	    (case (if !Options.fair_be then #2 (bind_fair (fast, inst)) else bind_exe (fast,inst)) of
		(is_disabled,_) => 
		    (status:= disabled;
		     PQRS.delete(unknowns,index))
	      | (is_executed,report) => 
		    (mark_dependents dep_list;
		     app (fn (fstr,f) => f()) (!step_inc_funs);
		     make_report(id,name,inst,report))
	      | (is_maybe_ready_at(time),_) =>
		    (status:= maybe_ready_at(time);
		     PQRS.delete (unknowns,index);
		     PQ.insert (maybe_readies,time,index)))
		 handle CPN'Error str => error_exn_msg(str,name)
		      | CPN'Cancel str => cancel_exn_msg(str,name)
		      | CPN'Stop str => stop_exn_msg(str)
		      | NotValidDirExn s => cancel_exn_msg(s,name)
		      | InternalError str => internal_error_msg(str,name)
		      | exn => critical_error_exn_msg(exnName exn,name)
	end handle PQ.EmptyPQ => (no_enabled();
				  call_stop_funs())
                 | Exit => ();
(*	 app (fn (fstr,f) => f()) (!stop_funs)
	 handle NotValidDirExn s => stop_exn_msg("\nError: NotValidDirExn in simulation stop funs\n"^s)
	      | exn => stop_exn_msg("Error: in simulation stop funs: "^exnName exn) ;*)
            inc cur_subrun ; (* FIXME: needs to depend on length of subrun *)
            make_response())

    fun build_all_enabled() =
    let
        fun find_first() =
        let
            val index = PQRS.ran(unknowns) handle PQRS.EmptyRanSet =>
            (increase_time();
		 PQRS.ran (unknowns))
 
	    val {bind_exe,bind_fair,inst,dep_list,status,name,priority,id} = 
              Array.sub(!transitions,index)
          val (result, _) = bind_exe (test, inst)
        in
            case result
              of is_executed => (index, inst)
               | is_disabled =>
                let
                    val _ = status := disabled
                    val _ = PQRS.delete(unknowns,index)
                in
                    find_first()
                end
               | is_maybe_ready_at(time) =>
                let
                    val _ = status := maybe_ready_at(time)
                    val _ = PQRS.delete(unknowns,index)
                    val _ = PQ.insert (maybe_readies,time,index)
                in
                    find_first()
                end
        end

        val first = find_first()
     
        val head = !(#head (!unknowns))

        val indices = List.tabulate(RS.size head, fn n => DynamicArray.sub (#set head, n)) 
        fun check_index index =
        let
	    val {bind_exe,bind_fair,inst,dep_list,status,name,priority,id} = 
              Array.sub(!transitions,index)
            val (count, (result, _), binder) = bind_fair (test, inst)
        in
            case result
              of is_executed => (index, count, inst, binder)
               | is_disabled =>
                let
                    val _ = status := disabled
                    val _ = PQRS.delete(unknowns,index)
                in
                    (index, count, inst, binder)
                end
               | is_maybe_ready_at(time) =>
                let
                    val _ = status := maybe_ready_at(time)
                    val _ = PQRS.delete(unknowns,index)
                    val _ = PQ.insert (maybe_readies,time,index)
                in
                    (index, count, inst, binder)
                end
        end

        val bindings = List.map check_index indices
        val enabled = List.filter (fn (_, 0, _, _) => false | _ => true) bindings
    in
        (first, indices, bindings, enabled)
    end

    fun run_fair () =
    let
        fun execute [] _ = raise InternalError "Trying to execute a non-existing transdition; this should never happen"
          | execute ((index, count, inst, binder)::rest) remainder =
            if remainder >= count
            then execute rest (remainder - count)
            else (CPN'inst := inst; (index, binder ()))
        fun runner () =
        let
            val _ = if check_stop_crit() then () else raise Exit
            val (_, _, enabled, _) = build_all_enabled ()
            val count = List.foldl (fn ((_, n, _, _), m) => n + m) 0 enabled
            val selected = CPN'Random.int count
            val name = "Unknown Transition"
            val _ =
                let
                    val (index, (status, msg)) = execute enabled selected
                    val {bind_exe,bind_fair,inst,dep_list,status,name,priority,id} = 
                        Array.sub(!transitions,index)
                    val _ = mark_dependents dep_list
                    val _ = app (fn (fstr,f) => f()) (!step_inc_funs)

                    val _ = make_report(id,name,inst,msg)
                in ()
                end
                     handle CPN'Error str => error_exn_msg(str,name)
                          | CPN'Cancel str => cancel_exn_msg(str,name)
                          | CPN'Stop str => stop_exn_msg(str)
                          | NotValidDirExn s => cancel_exn_msg(s,name)
                          | InternalError str => internal_error_msg(str,name)
                          | exn => critical_error_exn_msg(exnName exn,name)
        in
            runner()
        end
            
    in
        (runner() handle PQ.EmptyPQ => (no_enabled(); call_stop_funs()) | Exit => ());
        inc cur_subrun;
        make_response()
    end


    fun run() =
        if !Options.global_fair
        then run_fair()
        else run_fast()

    (* FIXME: some of the error messages are not appropriate, e.g. because
     * they indicate that a transition occurred, when in fact, the exception
     * was raised when determining that the transition was disabled *)
    fun random_step_fast () =
	let
	    val index = PQRS.ran(unknowns) 
		handle PQRS.EmptyRanSet => (increase_time();
					  PQRS.ran (unknowns))

	    val {bind_exe,bind_fair, inst,dep_list,status,name,priority,id} = 
		Array.sub(!transitions,index)

	    val occurred = ref false
	    val ok = ref false

	    fun compute_bindings() =
		(CPN'inst:=inst;
		 (case (if !Options.fair_be then #2 (bind_fair (fast, inst)) else bind_exe (fast,inst)) of
		      (is_executed,report) => 
		      (mark_dependents dep_list;
		       app (fn (fstr,f) => f()) (!step_inc_funs);
		       make_report(id,name,inst,report);
		       stop_crit_msg::= executed_msg;
		       CPN'debug("random_step executed");
		       ok := true;
		       occurred := true)
		    | (is_disabled,_) => 
		      (status:= disabled;
		       PQRS.delete(unknowns,index);
		       ok := true)
		    | (is_maybe_ready_at time,_) =>
		      (status:= maybe_ready_at time;
		       PQRS.delete (unknowns,index);
		       PQ.insert (maybe_readies,time,index);
		       ok := true))
		 )
	in
	    (if not (check_stop_crit()) then 
		 raise Exit
	     else (compute_bindings()
		   handle CPN'Error str => error_exn_msg(str,name)
			| CPN'Cancel str => cancel_exn_msg(str,name)
			| CPN'Stop str => stop_exn_msg(str)
			| NotValidDirExn s => cancel_exn_msg("NotValidDirExn for directory\n"^s,name)
			| InternalError str => internal_error_msg(str,name)
			| exn => critical_error_exn_msg(exnName exn,name)));
	    case (!occurred,!ok) of 
		(true,true) => (make_response(), SOME (id,inst), true)
        | (false,true) =>  random_step_fast()
	      | (_,false) => (make_response(),NONE,false)
	end
            handle PQ.EmptyPQ => ((* cannot increase time *)
				  no_enabled();
				  call_stop_funs();
				  (make_response(),NONE,true))
		 | Exit => ((* raised in increase_time or if stop crit met *)
			    (make_response(),NONE,true))
		 | exn => (stop_exn_msg("Error: a random step could not be taken because the following exception was raised:"^(exnName exn));
             (make_response(),NONE,false))

    fun random_step_fair() = ()

    fun random_step() =
        if !Options.global_fair
        then random_step_fast()
        else random_step_fast()

    fun man_bind (t,i, interactive: bool) = let

	val _ = CPN'debug("man_bind ("^t^","^(Int.toString i)^
			  ", interactive = "^(Bool.toString interactive)^
			  ")")

	val _ = 
	    if !CPN'Settings.use_manbind then () 
	    else raise InternalError "No code generated for manual bindings"

	val index = InstTable.get_ti_index (t,i)
	val {bind_exe,bind_fair,inst,dep_list,status,name,priority,id} = 
	    Array.sub(!transitions,index)

	fun compute_bindings() =
	    (CPN'inst:=inst;
	     case !status of
		unknown => 
		    (case bind_exe (bind interactive,inst) of
			 (is_executed,report) => 
			     (mark_dependents dep_list;
			      app (fn (fstr,f) => f()) (!step_inc_funs);
			      make_report(id,name,inst,report);
			      stop_crit_msg::= executed_msg)
		       | (is_disabled,_) => 
			     (status:= disabled;
			      PQRS.delete(unknowns,i);
			      stop_crit_msg::= disabled_msg)
		       | (is_maybe_ready_at time,_) =>
			     (status:= maybe_ready_at time;
			      PQRS.delete (unknowns,i);
			      PQ.insert (maybe_readies,time,i);
			      handle_maybe_ready_at()))
	      | disabled => (stop_crit_msg::= disabled_msg)
	      | maybe_ready_at time => (handle_maybe_ready_at())
			 )

	and handle_maybe_ready_at() = let
	    val cur_time = !model_time
	in
	    while check_all_unknowns() = 0 do
		increase_time();
	    if Time.lt(cur_time,!model_time) then
		compute_bindings()
	    else
		stop_crit_msg::= disabled_msg
	end

	and check_all_unknowns() = let
	    
	    fun check i = 
		if i<=0 then 0
		else let
                val {bind_exe,bind_fair,inst,dep_list,status,name,priority,id} = 
			Array.sub(!transitions,i)
		in
		    CPN'inst:= inst;
		    case bind_exe (test,inst) of
			(is_executed,_) => 
			    1+check(i-1)
		      | (is_disabled,_) => 
			    (status:= disabled;
			     PQRS.delete(unknowns,i);
			     check(i-1))
		      | (is_maybe_ready_at(time),_) =>
			    (status:= maybe_ready_at(time);
			     PQRS.delete (unknowns,i);
			     PQ.insert (maybe_readies,time,i);
			     check(i-1))
		end
	in
	    check(PQRS.size(unknowns))
	end

    in
	(if not (check_stop_crit()) then 
	     ()
	 else compute_bindings()
	     handle CPN'Error str => error_exn_msg(str,name)
		  | CPN'Cancel str => cancel_exn_msg(str,name)
		  | CPN'Stop str => stop_exn_msg(str)
		  | NotValidDirExn s => cancel_exn_msg("NotValidDirExn for directory\n"^s,name)
		  | InternalError str => internal_error_msg(str,name)
		  | CPN'CancelManBind => 
		    (* interactive manual binding cancelled *)
		    (cancel_man_bind_msg(name);
		     raise Exit)
		  | exn => critical_error_exn_msg(exnName exn,name))
	handle Exit => (); 
	make_response()
    end 

    (* pick_bind: function for firing a partially specified binding of 
     * a transition. Parameter varbinds specifies the desired bindings
     * for a subset of the variables of the transition. Both the 
     * variable names and the desired values are given as string. 
     * For any variable that is not specified, the simulator randomly
     * selects among the enabled bindings that are possible.
     * 
     * t: transition id
     * i: instance
     * varbinds: list of pairs: (varstr,valstr) *)

    fun pick_bind (t,i, varbinds: (string * string) list) = let

	val _ = CPN'debug("pick_bind ("^t^","^(Int.toString i)^", length varbinds="^Int.toString(length varbinds)^")")

	(* FIXME: check for duplicate vars *) 

	val index = InstTable.get_ti_index (t,i)
	val {bind_exe,bind_fair,inst,dep_list,status,name,priority,id} = 
	    Array.sub(!transitions,index)

	fun compute_bindings() =
	    (CPN'inst:=inst;
	     case !status of
		unknown => 
		    (case bind_exe (pick varbinds,inst) of
			 (is_executed,report) => 
			     (mark_dependents dep_list;
			      app (fn (fstr,f) => f()) (!step_inc_funs);
			      make_report(id,name,inst,report);
			      if varbinds=[]
			      then stop_crit_msg::= executed_msg
			      else stop_crit_msg::= be_executed_msg)
		       | (is_disabled,_) => 
			     (status:= disabled;
			      PQRS.delete(unknowns,i);
			      stop_crit_msg::= disabled_msg)
		       | (is_maybe_ready_at time,_) =>
			     (status:= maybe_ready_at time;
			      PQRS.delete (unknowns,i);
			      PQ.insert (maybe_readies,time,i);
			      handle_maybe_ready_at()))
	      | disabled => (stop_crit_msg::= disabled_msg)
	      | maybe_ready_at time => (handle_maybe_ready_at())
			 )

	and handle_maybe_ready_at() = let
	    val cur_time = !model_time
	in
	    while check_all_unknowns() = 0 do
		increase_time();
	    if Time.lt(cur_time,!model_time) then
		compute_bindings()
	    else
		stop_crit_msg::= disabled_msg
	end

	and check_all_unknowns() = let
	    
	    fun check i = 
		if i<=0 then 0
		else let
		    val {bind_exe,bind_fair,inst,dep_list,status,name,priority,id} = 
			Array.sub(!transitions,i)
		in
		    CPN'inst:= inst;
		    case bind_exe (test,inst) of
			(is_executed,_) => 
			    1+check(i-1)
		      | (is_disabled,_) => 
			    (status:= disabled;
			     PQRS.delete(unknowns,i);
			     check(i-1))
		      | (is_maybe_ready_at(time),_) =>
			    (status:= maybe_ready_at(time);
			     PQRS.delete (unknowns,i);
			     PQ.insert (maybe_readies,time,i);
			     check(i-1))
		end
	in
	    check(PQRS.size(unknowns))
	end

    in
	(if not (check_stop_crit()) then 
	     ()
	 else compute_bindings()
	     handle CPN'Error str => error_exn_msg(str,name)
		  | CPN'Cancel str => cancel_exn_msg(str,name)
		  | CPN'Stop str => stop_exn_msg(str)
		  | NotValidDirExn s => cancel_exn_msg("NotValidDirExn for directory\n"^s,name)
		  | InternalError str => internal_error_msg(str,name)
		  | CPN'CancelPickBind => stop_crit_msg::= be_disabled_msg
		  | exn => critical_error_exn_msg(exnName exn,name))
	handle Exit => (); 
	make_response()
    end 

    val print_mark = map Places.print_mark

    fun print_page_mark nil = nil
      | print_page_mark ((page,i)::pages) = 
	foldr (fn (p,tail) => ((p,i),Places.print_mark(p,i))::tail)
	(print_page_mark pages)
	(CPN'PageTable.get_places page)
	
    val print_size = map Places.size_mark

    fun print_page_size nil = nil
      | print_page_size ((page,i)::pages) = 
	foldr (fn (p,tail) => ((p,i),Places.size_mark(p,i))::tail)
	(print_page_size pages)
	(CPN'PageTable.get_places page)

    val print_enab = map check_enab
    val print_enab_no_scheduler = map check_enab_no_scheduler
	
    fun print_page_enab nil = nil
      | print_page_enab ((page,i)::pages) = let
	fun f (t,tail) =
	    if CPN'TransitionTable.is_transition t then 
		((t,i),check_enab(t,i))::tail 
	    else tail
    in
	foldr f (print_page_enab pages) (CPN'PageTable.get_transitions page)
    end

    fun change_mark (nil,_) = () (* should perhaps remove the symbols *)
      | change_mark ((pid,inst)::places,toinit) = 
	(CPN'inst:= inst;   (*FIXME:remove CPN'inst?*)
	 Places.change_mark (pid,inst,toinit);
	 mark_dependents (InstTable.get_dep_trans(pid,inst));
	 change_mark (places,toinit))

    fun change_model_time time = let
	fun move j =
	    (#status(Array.sub(!transitions,j)):= unknown; 
	     PQRS.insert (unknowns,j))
    in
	(model_time:= Time.maketime time;
	 app move (PQ.deleteto(maybe_readies, !model_time)))
    end	 

    fun increase_model_time() = let
	val next_time_opt = SOME(PQ.min(!maybe_readies)) 
	    handle PQ.EmptyPQ => (call_stop_funs();
				  no_enabled();
				  NONE)
    in
	case next_time_opt of
	    NONE => (false, #3(make_response()))
	  | SOME t => ((increase_time(); 
			CPN'debug ("increase time to "^(Time.toString t));
			(true,""))
		       handle Exit => (false,#3(make_response())))
    end

    fun get_internal_structure () =
    let
        fun pq_to_list pq =
        if PQRS.Q.isEmpty pq
        then []
        else 
            let
                val (head, rest) = PQRS.Q.remove pq
            in
                head::(pq_to_list rest)
            end
        val unknowns' = pq_to_list (#queue (!unknowns))
        val unknowns'' = (#headprio (!unknowns), #head (!unknowns))::unknowns'
        val unknowns''' = List.map(fn (key, ref ranset) => (key, List.tabulate(RS.size(ranset), fn n => DynamicArray.sub (#set ranset, n)))) unknowns''
        val maybe_readies' = List.tabulate (#last (!maybe_readies) + 1, fn n => Array.sub (#heap (!maybe_readies), n))
        val transitions' = List.tabulate (Array.length (!transitions), fn n => (n, #name (Array.sub (!transitions, n)), #dep_list (Array.sub (!transitions, n))))
        val vector' = CPN'dynArrayToList (#vector (!unknowns))
        val vector'' = List.map(fn (_, NONE) => NONE | (n, SOME (ref ranset)) => SOME (n, List.tabulate(RS.size(ranset), fn n => DynamicArray.sub (#set ranset, n)))) vector'
    in 
    { headpriority = #headprio (!unknowns), unknowns = unknowns''',
    maybe_readies = maybe_readies', prioritized = !prioritizedtransitions, transitions = transitions', time = (Time.toString (!model_time)), vector = vector'' }
    end
    
    (*************************** internal stuff **************************)

    local
	open Misc;

	(* Selects a timed token from a place with marking, mark, 
	 * based on the given arc exp of the place. Selection is
	 * determined by supplied comparison function where it
	 * returns EQUAL. (Note that exp_tv is the relative timestamp
	 * of the arc exp.)
	 * Typically called during binding and occurrence of a transition.
	 * Note that it does not strictly follow the subtraction algebra for
	 * timed ms of K.Jensen. Instead it selects fairly among ready tokens
	 * which have timestamp less than the arc expression.
	 * This is safe since simulation is using interleaved semantics and 
	 * hence the diamond rule is not violated. *)
        fun select_one_token(mark,collect,cmp,exp_col,exp_tv,exn) =
	    let
		fun cf (Time.@(mark_col,mark_tv)) =
		    case cmp(Time.@(mark_col,Time.null),Time.@(exp_col,Time.null)) of
			EQUAL =>
			(* if Time.ready(mark_tv) then EQUAL *)
 			(* if Time.ready(Time.sub(mark_tv,exp_tv)) *)
			if Time.leq(Time.null, Time.sub(exp_tv, mark_tv))
			then EQUAL 
			else LESS
		      | rel => rel
	    in
		case (collect cf mark) of
		    nil => raise exn
		  | tms => CPN'MS.random tms
	    end;

    in
	(* Fetch coef number of colour col from tms, and 
	 * raise exn in case there isn't enough tokens. 
	 * Assumes that coef >=0 *) 
	fun fetch_tms _ _ (_,0,_) = nil
	  | fetch_tms exn _ (nil,_,_) = raise exn
	  | fetch_tms exn lt (tms,coef,col) = 
	    let
		val (item,tms') = CPN'MS.get_ran exn tms
		val col' = Time.col item
	    in
		if lt(col,col') orelse lt(col',col) then
		    fetch_tms exn lt (tms',coef,col) 
		else
		    item::(fetch_tms exn lt (tms',coef-1,col)) 
	    end

        (* See select_one_token *)
	fun collect_token exn (mark,collect,cmp,exp,tv) =
	    select_one_token(!mark,collect,cmp,exp,tv,exn)

	(* Same as collect_token except that the arc expression is of
	 * kind tms_exp. *)
	fun collect_tms exn (mark,collect,cmp,delete,exp) = let

	    (* make a copy which can be safely modified *)
	    val cur_mark= ref (!mark)

	    fun iter (nil, res) = res
	      | iter ((Time.@(col,time))::rtms, res) =
		let
		    val sel_token = select_one_token(!cur_mark,collect,cmp,col,time,exn)
		    val _ = delete(cur_mark,sel_token)
		in
		    iter(rtms,sel_token::res)
		end
	in
	    iter (exp, nil)
	end

    end (* local *)

    (* Make a quick guess at whether or not a transition may be enabled.
     * We also need to take into account if it may be enabled at a
     * later time. *)
    (* FIXME: perhaps introduce enough_tokens,maybe_enough,not_enough 
     * instead of boolean (which is not optimal) *)
    fun each_place (enough1, (enough2,state as (is_maybe_ready_at _))) =
	(enough1 andalso enough2,state)
      | each_place (enough_tokens, answer) =
	if enough_tokens then answer else (false, is_disabled)

    (* Calculate best guess for time at which a tokens are ready for 
     * places around a transition. Note that we need to take into account 
     * that an arc can evaluate to an empty ms, and hence use min instead
     * of max. *)
    fun each_timed_place (enough1, NONE, (enough2, state)) =
	(enough1 andalso enough2, state)
      | each_timed_place (enough1, SOME time, (enough2, is_disabled)) =
	(enough1 andalso enough2, is_maybe_ready_at time)
      | each_timed_place (enough1, SOME time1, 
			  (enough2, is_maybe_ready_at time2)) =
	(enough1 andalso enough2, 
	 is_maybe_ready_at (if Time.lt(time1,time2) then time1 else time2))
      | each_timed_place _ = raise InternalError "each_timed_place"

    local
	fun show_place i (p,true)  = 
	    (Places.size_mark (p,i), Places.print_mark (p,i))
	  | show_place i (p,false) = (Places.size_mark (p,i),"")
    in
	fun pause_before show_arc (t,i) =
	    if !Options.pause_before_step then 
		let
		    val (places,arcs) = CPN'Response.pause_before (t,i)
		in
		    (if !Options.show_marking then
			 CPN'Response.show_markings (map (show_place i) places)
		     else ();
		     if !Options.pause_show_tokens then
			 CPN'Response.show_tokens (map show_arc arcs)
		     else ();
		     CPN'Response.end_pause())
		end
	    else ()

	fun pause_after show_arc (t,i) =
	    if !Options.pause_after_step then 
		let
		    val (places,arcs) = CPN'Response.pause_after (t,i)
		in
		    (if !Options.show_marking then
			CPN'Response.show_markings (map (show_place i) places)
		     else ();
		     if !Options.pause_show_tokens then
			 CPN'Response.show_tokens (map show_arc arcs)
		     else ();
		     CPN'Response.end_pause())
		end
	    else ()
    end

    fun tst_ill_marks (ref nil) = ()
      | tst_ill_marks (ref arcxreasons) =
	raise CPN'Cancel(concat
	       ("The following arc(s) resulted in illegal color(s):\n"::
		(map (fn (arc,res) => "("^(CPN'Id.toString arc)^", "^res^")\n") arcxreasons)))

    fun code_action action inst input =
	(action inst input
	 handle CPN'Cancel s => raise CPN'Cancel ("Error: in code segment\n"^s)
	      | CPN'Stop s => raise CPN'Stop ("Error: in code segment:\n"^s)
	      | CPN'StreamIO.IOError s => raise CPN'Cancel ("Error: CPN'Stream.IOError in code segment\n"^s)
	      | NotValidDirExn s => raise CPN'Cancel("Error: in code segment\n"^s)
	      | InternalError s => raise CPN'Cancel("Error: InternalError in code segment\n"^s)
	      | exn => raise CPN'Error((exnName exn)^" (raised in code segment)"))

    fun monitor monfun inst be =
	(monfun inst be
	 handle CPN'Cancel s => 
		raise CPN'Cancel ("Error: CPN'Cancel raised when monitoring\n"^
				  get_monitor_exn_source()^s)
	      | CPN'Stop s => 
		raise CPN'Stop ("Error: CPN'Stop raised when monitoring\n"^s)
	      | CPN'StreamIO.IOError s => 
		raise CPN'Cancel ("Error: CPN'Stream.IOError monitoring\n"^
						  get_monitor_exn_source()^s)
	      | NotValidDirExn s => 
		raise CPN'Cancel("Error: monitoring\n"^
				 get_monitor_exn_source()^s)
	      | InternalError s => 
		raise CPN'Cancel("Error: InternalError monitoring\n"^
				 get_monitor_exn_source()^s)
	      | exn => 
		raise CPN'Error((exnName exn)^" raised when monitoring "^
				get_monitor_exn_source()))

end (* local *)
end
