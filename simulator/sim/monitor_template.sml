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
(* File: templates.sml
 *
 * Template generation for monitor functions. 
 *)

structure CPN'MonitorTemplate = struct

(* FIXME Many similar or same functions found in create_monitor.sml? *)
fun gen_filter_pattern fun_name valstr ((tid,i), tail) = 
    let
	val maxLen = 65
	val recLen = 15
	val vars = Misc.sort String.< (CPN'TransitionTable.get_all_vars tid)
	val sl1 = "\n      | "::fun_name::"BindElem ("::
		  (CPN'TransitionTable.get_name tid)::
		  " ("::(Int.toString i)::[", "]
	val sl2 = 
	    "{"::(tl (foldr (fn (a,b) => ","::a::b) 
			    ["","})) = ",valstr] vars))
	val filterfunlist = sl1^^sl2
	(* indentation that works with current font in GUI *)
	val spaces = "                               "
	
	fun break [] strlist = concat (rev strlist)
	  | break (var::vars) (prefix::strlist) =
	    if prefix=spaces orelse
	       prefix=(spaces^"{")
	    then break vars ((prefix^var^(if vars<>[]
					  then ","
					  else "")::strlist))
	    else (* prefix <> spaces *)
		if String.size (prefix^var) > maxLen
		then break (var::vars) (spaces::(prefix^"\n")::strlist)
		else break vars ((prefix^var^(if vars<>[]
					  then ","
					  else "")::strlist))
    in
	if String.size (concat filterfunlist) > maxLen andalso 
	   String.size (concat sl2) > recLen
	then (sl1^^["\n"^(break vars [spaces^"{"]),"})) = ",valstr])::tail
	else filterfunlist::tail
    end

fun gen_filter_fun ([],fun_name,valstr1,valstr2) = "  "^valstr1
  | gen_filter_fun (tid_insts,fun_name,valstr1,valstr2) = 
	"let\n  fun "^
	(concat (tl (List.concat 
		     (foldr (gen_filter_pattern fun_name valstr1) 
			    [[]] (tid_insts)))))^
	"\n      | "^fun_name^"BindElem _ = "^valstr2^
	"\nin\n  "^fun_name^"BindElem bindelem  \nend"

(* Generates:
 * <pgname>'<placename>_<inst>_mark as a string
 *)
fun gen_mark_arg_name (pid:CPN'Id.id,inst:int) = 
    (CPN'PlaceTable.get_name pid)^"_"^Int.toString(inst)^"_mark"

fun gen_fun (tid_insts,pid_insts, fun_name,valstr1,valstr2,withBE) =  
    let
	val indent = ",\n"^"              "
    in
	"fun "^fun_name^" ("^
	(if (tid_insts<>[] andalso withBE) then "bindelem" else "")^
	    (if (pid_insts<>[]) andalso (tid_insts<>[]) andalso withBE
	     then indent else "")^ 
	(concat(tl(foldr (fn ((pid,inst),b) =>
			   indent::
			   gen_mark_arg_name(pid,inst)::" : "::
			   (#cs (#ext (CPN'PlaceTable.find pid)))::
			   (if (CPN'CSTable.is_timed(#cs (#ext (CPN'PlaceTable.find pid))))
			    then " t"
			    else " ")::"ms"::b)
		    [""] pid_insts)))^") = "^
	(if withBE 
	 then "\n"^gen_filter_fun(tid_insts,fun_name,valstr1,valstr2) 
	 else "\n  "^valstr1)^"\n"
    end

fun user_def_templates(tid_insts: (CPN'Id.id * int) list, 
		       pid_insts: (CPN'Id.id * int) list) = 
    case CPN'MonitorAux.check_subnet (pid_insts,tid_insts) of 
	[] => (true,
	       [gen_fun(tid_insts, pid_insts, "init","","",false),
		gen_fun(tid_insts, pid_insts, "pred","true","false",true),
		gen_fun(tid_insts, pid_insts, "obs","","",true),
		"fun action (observedval) = \n", (* action function *)
		gen_fun(tid_insts, pid_insts, "stop","","",false)])
      | errlist => (false, 
		    List.foldr (fn ((id,errstr),errs) => 
				   id::errstr::errs) [] errlist)

fun breakpoint_templates(tid_insts:(CPN'Id.id * int) list, 
			pid_insts: (CPN'Id.id * int) list) = 
    case CPN'MonitorAux.check_subnet (pid_insts,tid_insts) of 
	[] => (true,
	       [gen_fun(tid_insts, pid_insts, "pred","true","false",true)])
      | errlist => (false, 
		    List.foldr (fn ((id,errstr),errs) => 
				   id::errstr::errs) [] errlist)

fun datacoll_templates(timed: bool,
		       tid_insts: (CPN'Id.id * int) list, 
		       pid_insts: (CPN'Id.id * int) list) = 
    case CPN'MonitorAux.check_subnet (pid_insts,tid_insts) of 
	[] => (true,
	       [gen_fun(tid_insts, pid_insts, "pred","true","false",true),
		gen_fun(tid_insts, pid_insts, "obs","0","~1",true),
		gen_fun(tid_insts, pid_insts, "init",if timed 
						     then "SOME 0"
						     else "NONE","",false),
		gen_fun(tid_insts, pid_insts, "stop",if timed 
						     then "SOME 0"
						     else "NONE","",false)])
      | errlist => (false, 
		    List.foldr (fn ((id,errstr),errs) => 
				   id::errstr::errs) [] errlist)

fun write_file_templates(tid_insts: (CPN'Id.id * int) list, 
			 pid_insts: (CPN'Id.id * int) list) = 
    case CPN'MonitorAux.check_subnet (pid_insts,tid_insts) of 
	[] => (true,
	       [gen_fun(tid_insts, pid_insts, "init","\"\"","\"\"",false),
		gen_fun(tid_insts, pid_insts, "pred","true","false",true),
		gen_fun(tid_insts, pid_insts, "obs","\"\"","\"\"",true),
		gen_fun(tid_insts, pid_insts, "stop","\"\"","\"\"",false)])
      | errlist => (false, 
		    List.foldr (fn ((id,errstr),errs) => 
				   id::errstr::errs) [] errlist)

end (* structure CPN'MonitorTemplate*)

