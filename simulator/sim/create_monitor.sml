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
(* File: create_monitor.sml
 *
 * Creation of monitors.
 *)

functor CPN'CreateMonitor
	    (structure Sim : CPN'SIM 
	     and Decl: CPN'DECL
	     and Monitors: CPN'MONITORS) : CPN'CREATEMONITOR  =
struct


val datatype_name = "BindElem";

(* Generates
 *  | <pgname>'<transname> of int * {v1: v1cs, v2: v2cs, ..., vn: vncs}
 * as a list of strings
 *)
fun gen_be_constructor (tid, tail) =  
    let
	val vars = CPN'TransitionTable.get_all_vars tid 
    in
    ("  | "::(CPN'TransitionTable.get_name tid)::" of int * {"::
    (tl (foldr (fn (a,b) => ", "::a::": "::
			(#cs(CPN'VarTable.find a))::b) ["","}  \n"] vars)))
    ::tail
    end

(* Generate binding element datatype for given transition ids *)
fun gen_be_datatype tid_insts = 
    if tid_insts=[] then "type "^datatype_name^" = unit\n"
    else 
	let
	   val tids =  map (fn (tid,i) => tid) tid_insts
	   val unique_tids = unique_sort CPN'Id.lt tids
	in
	    "datatype "^datatype_name^" = \n"^
	    (concat(tl(List.concat(foldr gen_be_constructor [[""]] unique_tids))))
	end

fun gen_place_ms_type (pid,inst:int) = 
    (#cs (#ext (CPN'PlaceTable.find pid)))^
    (if (CPN'CSTable.is_timed(#cs (#ext (CPN'PlaceTable.find pid))))
     then " t"
     else " ")^"ms"

fun gen_markings pid_insts =
      (concat(foldr (fn ((pid,inst),b) => 
		     "\n val "::
		     (CPN'MonitorTemplate.gen_mark_arg_name (pid,inst))::
		     " = CPN'place"::pid::".get("::Int.toString(inst)::")"::b) 
	      [""] pid_insts))

(* generates unit or 
   cs1 (t)ms * cs2 (t)ms * ... * csn (t)ms 
*)
fun gen_markings_type [] = "unit\n" 
  | gen_markings_type (pid_insts) = 
    (concat(tl(foldr (fn ((pid,inst),b) => 
		      " * "::gen_place_ms_type (pid,inst)::b) 
	       ["\n"] pid_insts)))

fun gen_subnet_type (tid_insts,[]) = datatype_name
  | gen_subnet_type (tid_insts,pid_insts) = 
    (if tid_insts=[] then "" else datatype_name^" * ")^
    gen_markings_type(pid_insts)

fun gen_get_subnet (tid_insts,[]) = 
    "fun get_subnet (CPN'be:"^datatype_name^") = CPN'be\n"
  | gen_get_subnet (tid_insts,pid_insts) = 
    "fun get_subnet (CPN'be:"^datatype_name^"):subnet = \nlet"^
    (gen_markings pid_insts)^"\nin\n"^"("^
    (if tid_insts=[] then "" else "CPN'be")^	
    (if pid_insts<>[] andalso tid_insts<>[] then "," else "")^
    (concat(tl(foldr (fn ((pid,inst),b) =>
			 ", "::(CPN'MonitorTemplate.gen_mark_arg_name (pid,inst))::b)
		     [""] pid_insts)))^")\n end \n"

fun gen_get_markings [] = 
    "fun get_markings() = ()\n"
  | gen_get_markings (pid_insts) = 
    "fun get_markings():markings = \nlet"^
    (gen_markings pid_insts)^"\nin\n"^"("^
    (concat(tl(foldr (fn ((pid,inst),b) =>
			 ", "::(CPN'MonitorTemplate.gen_mark_arg_name (pid,inst))::b)
		     [""] pid_insts)))^")\n end \n"

fun gen_cpn_monitor_struct (item:CPN'MonitorTable.item) = 
    "structure CPN'"::(#name item)::" = struct\n"::
    gen_be_datatype(#transitions item)::
    "type markings = "::gen_markings_type(#places item)::"\n"::
    "type subnet = "::gen_subnet_type(#transitions item,#places item)::"\n"::
    gen_get_subnet(#transitions item,#places item)::
    gen_get_markings(#places item)::
    (case (#kind item) of
	 CPN'MonitorTable.datacoll {init,pred,obs,stop,...} => 
	 (#2 init)^"\n"^(#2 pred)^"\n"^(#2 obs)^"\n"^(#2 stop)
       | CPN'MonitorTable.breakpoint(_,pred) => pred^"\n"
       | CPN'MonitorTable.user_def{aux,init,pred,obs,act,stop} => 
	 (#2 aux)^"\n"^(#2 init)^"\n"^(#2 pred)^"\n"^
	 (#2 obs)^"\n"^(#2 stop)^"\n"^(#2 act)
       | CPN'MonitorTable.write_file{init,pred,obs,stop,fileext} => 
	 (#2 init)^"\n"^(#2 pred)^"\n"^
	 (#2 obs)^"\n"^(#2 stop)
     )::
    "\nend (* CPN'"::(#name item)::["*)\n" ]

(* Assume that syntax errors messages from ML 
 * have one of following formats:
 * :x.y Error ...
 * :x.y-w.z Error ... 
 * where w,x,y,z are integers *)
fun get_err_pos errstr = 
    let
	val stream = TextIO.openString errstr
	(* remove leading #":" *)
	val _ = case CPN'StreamIO.get_one stream of 
		    SOME #":" => ()
		  | _ => raise CPN'Error "No : in get_err_pos"
				  
	(* Assume that next chars in stream have following format
	 * x.y *)
	fun get_line_col s = 
	    let
		val linechrlist = 
		    #2 (CPN'StreamIO.get_until(stream, [#"."]))
		val (stopchar,colchrlist) = 
		    CPN'StreamIO.get_until(stream, [#"-",#" "])
	    in
		(stopchar,linechrlist,colchrlist)
	    end
	val (stopchr1,line1chrs,col1chrs) = get_line_col stream
	val (stopchr2,line2chrs,col2chrs) = 
	    if stopchr1=(#"-")
	    then get_line_col stream
	    else (stopchr1,[],[])
	val _ = TextIO.closeIn(stream)
	val line1 = Int.fromString(implode line1chrs)
	val col1 = Int.fromString(implode col1chrs)
	val line2 = Int.fromString(implode line2chrs)
	val col2 = Int.fromString(implode col2chrs)
    in
	{line1=line1,col1=col1,line2=line2,col2=col2}
    end
	
(* auxstr = contains auxiliary code necessary for checking the decl 
 * assume that there are no errors in auxstr which we generate
 * declstr = the decl to be checked *)
fun get_decl_errors (auxstr,declstr) = 
    case CPN'Env.is_decl("\n"^auxstr^"\n"^declstr) of
	NONE => NONE
      | SOME declerr => 
	let
	    fun countnewline s = 
		length(List.filter (fn c => c=(#"\n")) (explode s))
	    (* number of lines of text preceding the declstr *)
	    val auxlines = (countnewline auxstr)+1 
	    val {line1,col1,line2,col2} = get_err_pos declerr
	    val posstr = 
		case (line1,col1,line2,col2) of
		    (SOME l1, SOME c1, SOME l2, SOME c2) => 
		    Int.toString(l1-auxlines)^"."^Int.toString(c1)^"-"^
		    Int.toString(l2-auxlines)^"."^Int.toString(c2)^"\n"
		  | (SOME l1, SOME c1, NONE,NONE ) =>
		    Int.toString(l1-auxlines)^"."^Int.toString(c1)^"\n"
		  |  _ => ""
	    val correctedpos = if posstr = ""
			       then ""
			       else "Error found. Position of (first) error is probably "^posstr
	in
	    SOME (correctedpos^declerr) 
	end

(* CPN'AstLib.syntax_type_check adds "; end;" to the end of its argument *)
(* FIXME: positioning of error messages when testing fun type is wrong *)
fun check_fun_type (funstr,funname,domainstr,rangestr,be_datatype_str) = 
    case get_decl_errors (concat 
	 ["local ",be_datatype_str," \n",(* make the temp binding elements visible *)
	  funstr,"\nin\n","val CPN'y ="],
	 concat [funname," : ",domainstr," -> ",rangestr," end"]) of 
	NONE => ""
      | SOME errstr => ("Error: type of "^funname^" function is wrong.\n"^errstr)

fun get_dc_obs_type (obsfun,subnettypestr,be_datatype_str) = 
    case check_fun_type(obsfun,"obs",subnettypestr,"int",be_datatype_str) of
	"" => {obstypeerr = NONE, obstype= SOME "int"}
      | errstr => 
	case check_fun_type(obsfun,"obs",subnettypestr,"real",
			    be_datatype_str) of 
	    "" => {obstypeerr = NONE, obstype= SOME "real"}
	  | errstr => 
	    case check_fun_type(obsfun,"obs",subnettypestr,"IntInf.int",
				be_datatype_str) of 
		"" => {obstypeerr=NONE, obstype=SOME "IntInf.int"}
	      | errstr => {obstypeerr=SOME errstr, obstype=NONE}

fun check_datacoll (mid,CPN'MonitorTable.datacoll{init,stop,pred,obs,
					      timed,logfile},
		    subnettypestr: string,
		    markingstypestr: string,
		    be_datatype_str: string,
		    warn: bool) = 
    let
	val timed_err_str = 
	    if timed andalso (not (CPN'MonitorTable.model_is_timed()))
	    then "Cannot create a timed data collector until\n at least one place in the net has a timed colour set."
	    else ""

	val pred_errstr = 
	    case get_decl_errors((be_datatype_str),(#2 pred)) of 
		NONE => check_fun_type((#2 pred),"pred",subnettypestr,
				       "bool",be_datatype_str)
	      | SOME errstr => errstr;

	val obs_isdecl_err = get_decl_errors((be_datatype_str),(#2 obs))
	val {obstypeerr,obstype} = 
	    case obs_isdecl_err of
		SOME _ => {obstypeerr= obs_isdecl_err, obstype=NONE}
	      | NONE => get_dc_obs_type(#2 obs,subnettypestr,be_datatype_str)

	val obs_errstr =
	    case obstypeerr of
		SOME err => 
		(err^"\n"^"This obs function must be of type:\n  "^
		 subnettypestr^
		 " -> <number>, where <number> is int, real, or IntInf.int")
	      | _ =>  ""

	val init_errstr = 
	    if obs_errstr = "" 
	    then 
		case get_decl_errors("",#2 init) of
		    NONE => 
		    (case check_fun_type(#2 init,"init",markingstypestr,
					 Option.valOf(obstype)^" option",
					 be_datatype_str) of
			 "" => ""
		       | errstr => 
			 "Error: the obs function returns \
			 \values of type "^Option.valOf(obstype)^
			 "\ntherefore the init function must\
			 \ return values of type "^
			 Option.valOf(obstype)^" option.\n"^errstr)
		  | SOME errstr => errstr
	    else "Initialization function not checked because observation function has errors"
	val stop_errstr = 
	    if obs_errstr = "" 
	    then 
		case get_decl_errors("",#2 stop) of
		    NONE => 
		    (case check_fun_type(#2 stop,"stop",markingstypestr,
					 Option.valOf(obstype)^" option",
					 be_datatype_str) of
			 "" => ""
		       | errstr => 
			 "Error: the obs function returns \
			 \values of type "^Option.valOf(obstype)^
			 "\ntherefore the stop function must\
			 \ return values of type "^
			 Option.valOf(obstype)^" option.\n"^errstr)
		  | SOME errstr => errstr
	    else "Stop function not checked because observation function has errors"
    in
	[(#1 init,init_errstr),(#1 pred, pred_errstr),(#1 obs, obs_errstr),(#1 stop, stop_errstr),(mid,timed_err_str)]
    end
  | check_datacoll _ = raise InternalError ("check_datacoll")


fun check_user_def (CPN'MonitorTable.user_def{aux,init,pred,obs,act,stop},
		    subnettypestr:string,
		    markingstypestr:string,
		    be_datatype_str:string,
		    warn: bool) = 
    let
	val pred_errstr = 
	    case get_decl_errors (be_datatype_str,#2 pred) of 
		NONE => check_fun_type((#2 pred),"pred",subnettypestr,
				       "bool",be_datatype_str)
	      | SOME errstr => errstr;

	val obs_errstr = 
	    case get_decl_errors ((be_datatype_str),(#2 obs)) of 
		NONE => "" 
	      | SOME errstr => errstr;

	val init_errstr = 
	    case get_decl_errors ("",#2 init) of
		NONE => check_fun_type(#2 init,"init",markingstypestr,
				       "unit",be_datatype_str)
	      | SOME errstr => errstr
			  
	val stop_errstr = 
	    case get_decl_errors ("",#2 stop) of
		NONE => check_fun_type(#2 stop,"stop",markingstypestr,
				       "unit",be_datatype_str)
	      | SOME errstr => errstr

	val act_errstr = 
	    if obs_errstr = "" 
	    then (* check type of action function *) 
	    case get_decl_errors ("",#2 act) of
		NONE => check_fun_type((#2 obs)^"\n"^(#2 act),
				       "action o obs",subnettypestr,
				       "unit",be_datatype_str)
	      | SOME errstr => errstr
	    else "Action function not checked because observation function has errors"
    in
	[(#1 init,init_errstr),(#1 pred, pred_errstr),(#1 obs, obs_errstr),
	 (#1 stop,stop_errstr),(#1 act, act_errstr)]
    end
  | check_user_def _ = raise InternalError ("check_user_def ")
		
	      
fun check_breakpoint (CPN'MonitorTable.breakpoint(id,pred),
		      subnettypestr:string,
		      be_datatype_str:string,
		      warn: bool) =
    let
	val pred_errstr = 
	    case get_decl_errors ((be_datatype_str),pred) of 
		NONE => check_fun_type(pred,"pred",subnettypestr,
				       "bool",be_datatype_str)
	      | SOME errstr => errstr;
    in
	[(id, pred_errstr)]
    end
  | check_breakpoint _ = raise InternalError ("check_breakpoint")
    
fun check_write_file (CPN'MonitorTable.write_file{init,stop,pred,obs,
						  fileext},
		      subnettypestr: string,
		      markingstypestr: string,
		      be_datatype_str:string,
		      warn: bool) = 
    let
	val pred_errstr = 
	    case get_decl_errors((be_datatype_str),(#2 pred)) of 
		NONE => check_fun_type((#2 pred),"pred",subnettypestr,
				       "bool",be_datatype_str)
	      | SOME errstr => errstr;

	val obs_errstr = 
	    case get_decl_errors((be_datatype_str),(#2 obs)) of 
		NONE => check_fun_type((#2 obs),"obs",subnettypestr,
				       "string",be_datatype_str)
	      | SOME errstr => errstr;

	val init_errstr = 
	    case get_decl_errors ("",#2 init) of
		NONE => check_fun_type(#2 init,"init",markingstypestr,
				       "string",be_datatype_str)
	      | SOME errstr => errstr
		 
	val stop_errstr = 
	    case get_decl_errors ("",#2 stop) of
		NONE => check_fun_type(#2 stop,"stop",markingstypestr,
				       "string",be_datatype_str)
	      | SOME errstr => errstr
    in
	[(#1 init,init_errstr),(#1 pred, pred_errstr),(#1 obs, obs_errstr),
	 (#1 stop, stop_errstr)]
    end
  | check_write_file _ = raise InternalError ("check_write_file")

fun syntax_check (mid,item:CPN'MonitorTable.item,be_datatype_str:string) =
    let
	val subnettypestr = gen_subnet_type(#transitions item,
					    #places item)
	val markingstypestr = gen_markings_type (#places item)
			
	val warn = !CPN'Settings.ignore_warnings
		   
	val errstr_list = 
	    case (#kind item) of
		CPN'MonitorTable.datacoll _ => 
		check_datacoll(mid,#kind item,subnettypestr,markingstypestr,
			       be_datatype_str,warn)
	      | CPN'MonitorTable.breakpoint (id,pred) => 
		check_breakpoint (#kind item,subnettypestr,
				  be_datatype_str,warn)
	      | CPN'MonitorTable.user_def _ => 
		check_user_def(#kind item,subnettypestr,markingstypestr,
			       be_datatype_str,warn)
	      | CPN'MonitorTable.write_file _ => 
		check_write_file(#kind item,subnettypestr,markingstypestr,
				 be_datatype_str,warn)
    in
	errstr_list
    end

fun insert_funs (mid,item:CPN'MonitorTable.item,be_datatype_str:string) = 
    let
	(* FIXME: need to deal with rep monitors *)
	fun insert (CPN'MonitorTable.user_def _,
		    CPN'MonitorTable.step_monitor) = 
	    (CPN'Env.use_string ["val _ = CPN'Monitors.insert_fun(\"",
				 mid,"\",\"",(#name item),".init_monitor\",",
				 (#name item),".init_monitor,",
				 " CPN'Monitors.sim_init_fun_list)\n"];
	     CPN'Env.use_string ["val _ = if step()=IntInf.fromInt 0 then ",(#name item),".init_monitor() else () \n"]; 
	     CPN'Env.use_string ["val _ = CPN'Monitors.insert_fun(\"",
				 mid,"\",\"",(#name item),".stop_monitor\",",
				 (#name item),".stop_monitor,",
				 " CPN'Monitors.sim_stop_fun_list)\n"])
	  | insert (CPN'MonitorTable.write_file _,
		    CPN'MonitorTable.step_monitor) = 
	    (CPN'Env.use_string ["val _ = CPN'Monitors.insert_fun(\"",
				 mid,"\",\"",(#name item),".init_monitor\",",
				 (#name item),".init_monitor,",
				 " CPN'Monitors.sim_init_fun_list)\n"];
	     CPN'Env.use_string ["val _ = if step()=IntInf.fromInt 0 then ",(#name item),".init_monitor() else () \n"]; 
	     CPN'Env.use_string ["val _ = CPN'Monitors.insert_fun(\"",
				 mid,"\",\"",(#name item),".stop_monitor\",",
				 (#name item),".stop_monitor,",
				 " CPN'Monitors.sim_stop_fun_list)\n"])
	  | insert (CPN'MonitorTable.datacoll dc,
		    montype) = 
	    let
		val montypestr = case montype of 
				     CPN'MonitorTable.step_monitor => "sim"
				   | CPN'MonitorTable.sim_monitor => "rep"
		val subnettypestr = gen_subnet_type(#transitions item,
						     #places item)
		val obstype = 
		    case get_dc_obs_type(#2(#obs(dc)),subnettypestr,
					 be_datatype_str) of
			{obstypeerr=SOME str,obstype} => raise InternalError "unknown obstype in CreateMonitor.insert_funs "
		      | {obstype=SOME "IntInf.int",...} => "int"
		      | {obstype=SOME tstr,...} => tstr
		      | _ => raise InternalError "Match error in CreateMonitor.insert_funs"
	    in
		CPN'Env.use_string 
		    ["val _ = CPN'Monitors.insert_fun(\"",
		     mid,"\",\"",(#name item),".init_monitor\",",
		     (#name item),".init_monitor,",
		     " CPN'Monitors.",montypestr,"_init_fun_list)\n"];
	     CPN'Env.use_string ["val _ = if step()=IntInf.fromInt 0 then ",(#name item),".init_monitor() else ()\n"];
		CPN'Env.use_string 
		    ["val _ = CPN'Monitors.insert_fun(\"",
		     mid,"\",\"",(#name item),".stop_monitor\",",
		     (#name item),".stop_monitor,",
		     " CPN'Monitors.",montypestr,"_stop_fun_list)\n"];
		(case montype of 
		     CPN'MonitorTable.step_monitor => 
		     app (fn (s,obskind) => 
			     CPN'Env.use_string 
				 ["val _=CPN'PerfReport.insert_in_iid_list(\"",
				  mid,"\",\"",(#name item),"\",\"",s,"\",",
				  (#name item),".",s,") ",
				  "CPN'PerfReport.",
				  obskind,
				  "untimed_statvars_iidobs\n"]) 
			 (List.@([("count_iid","int"),("avrg_iid","real"),
				 ("min_iid",obstype),("max_iid",obstype)],
			  (if (#timed dc)
			   then []
			   else [("sum_iid",obstype)])))
		   | CPN'MonitorTable.sim_monitor => 
		     (* FIXME: remove? *)
		     (CPN'Env.use_string 
			 ["val _ = CPN'Monitors.insert_fun(\"",
			  mid,"\",\"",(#name item),".monitor\",",
			  (#name item),".monitor,",
			  " CPN'Monitors.post_rep_monitor_fun_list)\n"];
			 CPN'Env.use_string 
			     ["val _ = CPN'PerfReport.insert_in_list (\"",
			      mid,"\",\"",(#name item),"\",",
			      (#name item),".statvar) ",
			      "CPN'PerfReport.rep_",
			      obstype,
			      "untimed_statvars\n"]));
		CPN'Env.use_string 
		    ["val _ = CPN'PerfReport.insert_in_list (\"",
		     mid,"\",\"",(#name item),"\",",
		     (#name item),".get_stat_strings) ",
		     "CPN'PerfReport.",
		     if (#timed dc) then "timed" else "untimed",
		     "_",montypestr,"_get_stats_funs\n"]
	    end
	  | insert (CPN'MonitorTable.breakpoint _,
		    CPN'MonitorTable.step_monitor) = ()
	  | insert _ = (* FIXME *)
	    (CPN'debug "CPN'CreateMonitor.insert_funs not fully implemented yet";
	    raise InternalError "CPN'CreateMonitor.insert_funs not fully implemented yet")
    in
	if (#transitions item) = [] andalso 
	   (#montype item) = CPN'MonitorTable.step_monitor
	then 
	    CPN'Env.use_string ["val _ = CPN'Monitors.insert_fun(\"",
				mid,"\",\"",(#name item),".monitor\",",
				(#name item),".monitor,",
				" CPN'Monitors.step_monitor_fun_list)\n"] 
	else ();
	insert (#kind item,#montype item)
    end

fun create_datacoll (id, item as {name,places,transitions,montype,
				  kind=CPN'MonitorTable.datacoll
					   {timed,logfile,
					    init,stop,pred,obs}},
		     be_datatype_str:string) = 
    let
	val auxstructstr = gen_cpn_monitor_struct (item)
	val _ = CPN'Env.use_string(auxstructstr)
	val subnettypestr = gen_subnet_type(transitions,places)
	val obstype = 
	    case get_dc_obs_type(#2 obs,subnettypestr,be_datatype_str) of
		{obstypeerr=SOME str,...} => 
		(CPN'debug ("obstypeerr in create_datacoll: "^str);
		 raise InternalError "Unknown obstype in CreateMonitor.create_datacoll")
	      | {obstype=SOME "IntInf.int",...} => "II"
	      | {obstype=SOME "int",...} => "I"
	      | {obstype=SOME "real",...} => "R"
	      | _ => raise InternalError "Match error in CreateMonitor.create_datacoll"

	val monstruct = 
	    ["\nstructure ",name," = CPN'Monitors.Create",
	     if timed then "T" else "Unt","imedDC (",
	     "type bindelem = CPN'",name,".BindElem ",
	     "and subnet = CPN'",name,".subnet ",
	     "and markings = CPN'",name,".markings ",
	     "structure SV = CPN'",
	     if obstype="R" then "R" else "I",
	     if timed then "T" else "U","SV ",
	     if timed 
	     then "structure MSV = CPN'"^
		  (if obstype="R" then "R" else "I")^
		  "USV "
	     else "",
	     "val pred = CPN'",name,".pred ",
	     "and obs =(",
	     if obstype = "I" (* obs fun returns int *)
	     then " IntInf.fromInt o"
	     else "",
	     " CPN'",name,".obs):subnet->SV.data ",
	     "and init =(",
	     if obstype = "I" (* init fun returns int *)
	     then " (fn CPN'iopt => case CPN'iopt of NONE => NONE | SOME CPN'i => SOME (IntInf.fromInt CPN'i)) o"
	     else "",
	     " CPN'",name,".init):markings->SV.data option ",
	     "and stop =(",
	     if obstype = "I" (* stop fun returns int *)
	     then " (fn CPN'iopt => case CPN'iopt of NONE => NONE | SOME CPN'i => SOME (IntInf.fromInt CPN'i)) o"
	     else "", 
	     " CPN'",name,".stop):markings->SV.data option ",
	     "and get_subnet = CPN'",name,".get_subnet ",
	     "and get_markings = CPN'",name,".get_markings ",
	     "and name = \"",name,"\"",
	     if timed then ""
	     else ("and montype = "^
		   (case montype of
			CPN'MonitorTable.step_monitor => "CPN'MonitorTable.step_monitor "
		      | CPN'MonitorTable.sim_monitor => "CPN'MonitorTable.sim_monitor ")),
	     " and updatelogfile = ",Bool.toString(logfile),
	     ")\n"]
	val _ = CPN'Env.use_string(monstruct)
    in
	 CPN'Dep.find_dependencies 
	     (id,concat (auxstructstr^^monstruct))
    end

  | create_datacoll _ = raise InternalError "create_datacoll"

fun create_breakpoint (id, item as {name,places,transitions,montype, 
				    kind=CPN'MonitorTable.breakpoint _}) = 
    let
	val auxstructstr = gen_cpn_monitor_struct (item)
	val _ = CPN'Env.use_string(auxstructstr)
	val monstruct = 
	    ["\nstructure ",name," = CPN'Monitors.CreateBreakpoint (",
	     "type bindelem = CPN'",name,".BindElem ",
	     "and subnet = CPN'",name,".subnet ",
	     "and markings = CPN'",name,".markings ",
	     "val pred = CPN'",name,".pred ",
	     "and get_subnet = CPN'",name,".get_subnet ",
	     "and get_markings = CPN'",name,".get_markings ",
	     "and name = \"",name,"\"",
	     ")\n"]
	val _ = CPN'Env.use_string(monstruct)
    in
	 CPN'Dep.find_dependencies 
	     (id,concat (auxstructstr^^monstruct))
    end
  | create_breakpoint _ = raise InternalError "create_breakpoint"
				

fun create_user_def (id, item as {name,places,transitions,montype, 
			  kind=CPN'MonitorTable.user_def{aux,init,pred,
							 obs,act,stop}}) = 
    let
	val auxstructstr = gen_cpn_monitor_struct (item)
	val _ = CPN'Env.use_string(auxstructstr)
	val monstruct = 
	    ["\nstructure ",name," = CPN'Monitors.CreateUserDef (",
	     "type bindelem = CPN'",name,".BindElem ",
	     "and subnet = CPN'",name,".subnet ",
	     "and markings = CPN'",name,".markings ",
	     "val pred = CPN'",name,".pred ",
	     "and init = CPN'",name,".init ",
	     "and stop = CPN'",name,".stop ",
	     "and action_obs = (CPN'",name,".action o CPN'",name,".obs): ",
	     "CPN'",name,".subnet -> unit ",
	     "and get_subnet = CPN'",name,".get_subnet ",
	     "and get_markings = CPN'",name,".get_markings ",
	     "and name = \"",name,"\"",")\n"]
	val _ = CPN'Env.use_string(monstruct)
    in
	 CPN'Dep.find_dependencies 
	     (id,concat (auxstructstr^^monstruct))
    end
  | create_user_def _ = raise InternalError "create_user_def"

fun create_write_file (id, item as {name,places,transitions,montype,
				    kind=CPN'MonitorTable.write_file
					   {init,stop,pred,obs,fileext}}) = 
    let
	val auxstructstr = gen_cpn_monitor_struct(item)
	val _ = CPN'Env.use_string(auxstructstr)
	val monstruct = 
	    ["\nstructure ",name," = CPN'Monitors.CreateWriteFile(",
	     "type bindelem = CPN'",name,".BindElem ",
	     "and subnet = CPN'",name,".subnet ",
	     "and markings = CPN'",name,".markings ",
	     "val pred = CPN'",name,".pred ",
	     "and obs = CPN'",name,".obs ",
	     "and init = CPN'",name,".init ",
	     "and stop = CPN'",name,".stop ",
	     "and fileext = \"",fileext,"\"",
	     "and get_subnet = CPN'",name,".get_subnet ",
	     "and get_markings = CPN'",name,".get_markings ",
	     "and montype = ",
	     case montype of
		 CPN'MonitorTable.step_monitor => 
		 "CPN'MonitorTable.step_monitor "
	       | CPN'MonitorTable.sim_monitor => 
		 "CPN'MonitorTable.sim_monitor ",
	     "and name = \"",name,"\"",")\n"]
	val _ = CPN'Env.use_string(monstruct)
    in
	 CPN'Dep.find_dependencies 
	     (id,concat (auxstructstr^^monstruct))
    end

  | create_write_file _ = raise InternalError "create_write_file"

fun remove_funs (mid,kind,transitions,montype) = 
    let 
	fun remove (CPN'MonitorTable.user_def _,
		    CPN'MonitorTable.step_monitor) = 
	    (Monitors.rm_fun (mid,Monitors.sim_init_fun_list);
	     Monitors.rm_fun (mid,Monitors.sim_stop_fun_list))
	  | remove (CPN'MonitorTable.write_file _,
		    CPN'MonitorTable.step_monitor) = 
	    (Monitors.rm_fun (mid,Monitors.sim_init_fun_list);
	     Monitors.rm_fun (mid,Monitors.sim_stop_fun_list))
	  | remove (CPN'MonitorTable.datacoll dc,
		    CPN'MonitorTable.step_monitor) = 
	    (Monitors.rm_fun (mid,Monitors.sim_init_fun_list);
	     Monitors.rm_fun (mid,Monitors.sim_stop_fun_list);
	     CPN'PerfReport.rm_from_lists mid)
	  | remove (CPN'MonitorTable.datacoll dc,
		    CPN'MonitorTable.sim_monitor) = 
	    (Monitors.rm_fun (mid,Monitors.rep_init_fun_list);
	     Monitors.rm_fun (mid,Monitors.rep_stop_fun_list);
	     Monitors.rm_fun (mid,Monitors.post_rep_monitor_fun_list);
	     CPN'PerfReport.rm_from_lists mid)
	  | remove (CPN'MonitorTable.breakpoint _,
		    CPN'MonitorTable.step_monitor) = 
	    (Monitors.rm_fun (mid, Monitors.sim_stop_crit_fun_list))
	  | remove _ = 
	    (CPN'debug "CPN'CreateMonitors.remove_funs not fully implemented yet";
	    raise InternalError "CPN'CreateMonitors.remove_funs not fully implemented yet")
    in
	if transitions = [] andalso montype=CPN'MonitorTable.step_monitor
	then Monitors.rm_fun (mid,Monitors.step_monitor_fun_list)
	else ();
	remove (kind,montype)
    end

fun remove mid = 
    case CPN'MonitorTable.peek mid of 
	NONE => ()
      | SOME item  => (CPN'Env.remove_decl mid;
		       CPN'MonitorTable.remove mid;
		       remove_funs (mid,#kind item, #transitions item, 
				    #montype item))

fun errors_exist errstr_list = 
    (List.exists (fn (id,str) => str<>"") errstr_list)

fun create_monitor (id,item as {kind,places,transitions,name,montype}:CPN'MonitorTable.item) = 
    let
	val _ = 
	    CPN'debug ("create_monitor ("^name^",trans=["^
		       concat(tl(foldr (fn ((tid,inst),b) => ","::tid::"("::Int.toString inst::")"::b) [""] 
				       transitions))^
		       "], places=["^
		       concat(tl(foldr (fn ((pid,inst),b) => ","::pid::"("::Int.toString inst::")"::b) [""] 
				       places))^"], kind= "^
		       (case kind of 
			    CPN'MonitorTable.datacoll _ => "datacoll" 
			  | CPN'MonitorTable.breakpoint _ => "breakpoint" 
			  | CPN'MonitorTable.user_def _ => "user_def"
			  | CPN'MonitorTable.write_file _ => "write_file")^")")

	val _ = remove id

	val mlname = CPN'getMLidentifierPrefix name
	val nameerr = 
	    if (mlname<>name) orelse (mlname="")
	    then [(id,"Error: The name of the monitor ("^name^
		       ") must start with a letter, and it can contain \
		       \only letters, numbers, underscores (_) and \
		       \apostrophes (').")]
	    else []

	val subneterrs = CPN'MonitorAux.check_subnet(places,transitions)

	val err_strs = 	nameerr^^subneterrs

	val be_datatype_str = if err_strs = []
			      then gen_be_datatype(#transitions item)
			      else ""
			      
	val syntax_err_strs = 
	    if err_strs = []
	    then syntax_check (id,item,be_datatype_str)
	    else err_strs

    in
	if errors_exist syntax_err_strs
	then (remove id;
	      (id,
	      List.filter (fn (id,err_str)=> err_str<>"") syntax_err_strs, 
	      ([],[])))
	else 
	    let
		val dep_overwrite = 
		    case kind of 
			CPN'MonitorTable.datacoll _ => 
			create_datacoll (id,item,be_datatype_str)
		      | CPN'MonitorTable.breakpoint _ => 
			create_breakpoint (id,item)
		      | CPN'MonitorTable.user_def _ => 
			create_user_def (id,item)
		      | CPN'MonitorTable.write_file _ => 
			create_write_file (id,item)
	    in
		(CPN'MonitorTable.insert (id,item);
		 insert_funs (id,item,be_datatype_str);
		 (id,[],dep_overwrite))
	    end
		handle NotValidDirExn s => 
		       (remove id;
			(id,[(id,"Output error when creating monitor:\n"^s)],
			 ([],[])))
		     | InternalError s => 
		       (remove id;
			(id,[(id,"Internal Error when creating monitor:\n"^s)],
			 ([],[])))
		     | exn => 
		       (remove id;
			(id,[(id,"Exception creating monitor:\n"^exnName exn)],
			 ([],[])))
    end 
	
fun create_all () = 
    let
	val monitorlist = map (fn mid => (mid,CPN'MonitorTable.find mid)) 
			      (CPN'MonitorTable.get_order())
    in
        CPN'report_timing (concat["create monitor structs @ "]);
	map create_monitor monitorlist
    end

fun create_some monitors = 
    (CPN'report_timing (concat["create some monitor structs @ "]);
     map create_monitor monitors)

end (* CPN'CreateMonitors *)
