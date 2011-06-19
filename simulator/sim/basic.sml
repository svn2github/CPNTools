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
(* File: basic.sml
 *
 * Basic function, values, and renames.
 *)

structure CPN'Id = struct
    type id = string

    val base = ""

    fun eq (a,b:id) = a=b

    fun lt (a,b:id) = a < b

    val hash = HashString.hashString

    fun toString s = concat ["\"",s,"\""]

    fun makeid s = s
end

exception Subtract
exception BoundGroup

(* exception BindFailure as described in Master's Thesis
 * One attempt at binding has failed, a new attempt can be made 
 * Only raised in functions that are not the first for 
 * an expression group. 
*)
exception BindFailure

exception BindFailureGenAll

(* exception BindFatalFailure as described in Master's Thesis
 * Binding of a transition has failed completely. 
 * Only raised by first function in expression group.
*)
exception BindFatalFailure

exception CPN'Error = CpnMLSys.EvalProcess.CPN'Error
exception CPN'Cancel of string
exception CPN'Stop of string
exception CPN'CancelManBind
exception CPN'CancelPickBind

(* Used in state space tool *)
exception IllegalName of string 

(************************* renames *************************)

structure CPN'String = String
structure CPN'Real = Real
structure CPN'Int = Int
structure CPN'List = List
structure CPN'Array = Array
structure CPN'IO = TextIO

val CPN'use = use
val CPN'hd = hd
val CPN'tl = tl
val CPN'nth = List.nth
val CPN'app = app
val CPN'map = map
val CPN'concat = concat
val tod = fn () =>  Int32.fromLarge (Time.toSeconds(Time.now()))
fun inc r = r:= (!r)+1
val CPN'inc = inc
fun dec r = r:= (!r)-1
val CPN'dec = dec

val CPN'build = CpnML.Toploop.build; (* Used by AENewSave.c:AESaveFam *)

val CPN'inst = ref 1;

(************************* defaults *************************)

structure CPN'Settings = struct

    (* data structures used for internal multi-set representations *)
    val pims_name = ref "CPN'MakeTreeListPIMS"
    val sims_name = ref "CPN'MakeTreeSIMS"

    (* Should code be generated for handling pause in simulation *)
    val use_pause = ref false

    (* Should code be generated for handling manual bindings *)
    val use_manbind = ref false

    (* Should code be generated for handling check of legal bindings *)
    val use_legal_check = ref true

    (* Should code be generated for reporting bindings *)
    val use_report_binds = ref true

    (* Should the declared symbols be returned to C *)
    val use_record_symbols = ref true

    (* Should System.Unsafe.cast be used for enum ord fun *)
    val use_cast = ref false

    (* Should enum function be generated fast or short *)    
    val small_enum_size = ref 4 

    (* Size of color-sets from which it is possible to bind *)
    val bindable_cs_size = ref 100

    (* Should oa functions ask through the dmo *)
    val use_dmo = ref true

    (* Should code generation be made? *)
    val use_codegen = ref true

    (* Should performance for syntax check and code gen be reported? *)
    val use_timing = ref false

    (* Ignore warnings in declarations *)
    val ignore_warnings = ref false

end

(************************** debugging **************************)

exception InternalError = CpnMLSys.SimProcess.InternalError;

val CPN'debug = CpnMLSys.GramError.debug;
val CPN'idebug = CpnMLSys.GramError.idebug;

val CPN'save_debug_info  = CpnMLSys.GramError.save_debug_info;
val CPN'term_debug  = CpnMLSys.GramError.term_debug;

fun CPN'debug_exn exn msg =
    ((raise exn)
	 handle InternalError str => 
	     CPN'debug("Exception InternalError \""^str^"\" is raised "^msg)
	      | _ => CPN'debug("Exception "^exnName exn^" is raised "^msg);
      raise exn)

signature CPN'CODEGEN = sig 
    val init_dumping       : unit -> unit
    val term_dumping       : unit -> unit
    val dump_source        : string -> unit
    val set_dumping_filename : string -> unit
end;

structure CPN'CodeGen:CPN'CODEGEN = struct

    val dumping_installed = CPN'Settings.use_codegen; (* Master trigger *)
    val dumping_initialised = ref false;
    val dumping_stream = ref (NONE: TextIO.outstream option);
    val dumping_filename = ref "";

    fun set_dumping_filename name = 
	if (name="") then
	    (CPN'debug "set_dumping_filename: code gen dump OFF";
	    dumping_installed := false)
(*	    dumping_filename := "/tmp/CPNML-"^(Word32.toString(Posix.Process.pidToWord(Posix.ProcEnv.getpid())))^".sml"*)
	else
	    (CPN'debug ("set_dumping_filename: code gen dump ON: "^name);
	    dumping_filename := name);

    fun init_dumping() = 
	let
	    val _ = CPN'debug "fun init_dumping";
	in
	    if !dumping_installed then
		(dumping_stream := (SOME(TextIO.openOut(!dumping_filename)) 
				    handle exn => 
					(CPN'debug_exn exn ("INIT_DUMPING FAILED!!!: "^(!dumping_filename));
					 dumping_installed := false;
					 NONE));
		 dumping_initialised := true)
	    else (CPN'debug "fun init_dumping: NOT INIT")
	end

    fun term_dumping() = 
	(case !dumping_stream of
	     SOME(file) => (TextIO.flushOut file;
			    TextIO.closeOut file;
			    dumping_stream:=NONE)
	   | NONE       => ();
	 dumping_initialised := false
	 );
	
(*    fun turn_dumping_onoff new_state = 
	(if !dumping_installed andalso !dumping_initialised then
	     dumping_mode:=new_state
	 else
	     raise InternalError "CPN'CodeGen not initialised");
*)
    fun dump_source str = 
	if !dumping_installed andalso !dumping_initialised then
	    (TextIO.output (valOf(!dumping_stream),str);
	     TextIO.flushOut (valOf(!dumping_stream)))
	else ((*CPN'debug ("dump_source: NOT!!!"^(Bool.toString(!dumping_installed))^"-"^(Bool.toString(!dumping_initialised)))*));

end;

local
    open Timer
    val timer = ref (NONE: cpu_timer option)
    val stream = ref (NONE: TextIO.outstream option)
in
    fun CPN'start_timing name = 
	case (!timer,!CPN'Settings.use_timing) of
	    (NONE,true) =>
		(timer:= SOME(startCPUTimer ());
		 stream:= SOME(TextIO.openOut("/tmp/"^name)))  (* Set this to "c:/"^name if
								* debugging on windows.
								*)
	  | _ => ()

    fun CPN'report_timing msg =
	case (!timer,!CPN'Settings.use_timing) of
	    (SOME t,true) =>
		let
		    val {usr,sys} = checkCPUTimer t
		    val total = Time.+ (usr,sys)
		    val text = concat[msg,"\t",
				      Time.toString total,"=",
				      Time.toString usr,"+",
				      Time.toString sys,"\n"]
		in
		    case !stream of
			NONE => ()
		      | SOME s => (TextIO.output(s,text); TextIO.flushOut s);
		    text
		end
	  | _ => "Timing Disabled"

    fun CPN'stop_timing () = 
	(case (!stream,!CPN'Settings.use_timing) of
	     (SOME s,true)  => (TextIO.closeOut s; stream:= NONE)
	   | _ => ();
	 timer:= NONE)
end

(* Returns the prefix of the string s that consists of
 * alphanumeric characters, underscores (_) and primes ('). 
 * Ensures that the first char in the string is a letter. 
 * If there is no legal ML identifier prefix, return "" *)
fun CPN'getMLidentifierPrefix (s:string) =
    let
	fun check' nil = nil
	  | check' (x::xs) =if (Char.isAlphaNum x) orelse 
			       (x=(#"_")) orelse 
			       (x=(#"'"))
			    then x::check' xs
			    else nil
	fun check nil = nil
	  | check (x::xs) = if (Char.isAlpha x)
			    then check' (x::xs)
			    else []
    in
	String.implode(check (String.explode s))
    end
