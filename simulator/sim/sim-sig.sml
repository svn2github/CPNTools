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
(* File: sim-sig.sml
 *
 * Interface description for simulation facilities.
 *)

signature CPN'SIM = sig
    
    structure Time: CPN'TIME
    structure Options: CPN'OPTIONS

    datatype mode = bind of bool | fast | test | all_enabled
		  | pick of (string * string) list
    datatype result = is_disabled | is_executed | is_maybe_ready_at of Time.time 
    datatype status = unknown | disabled | maybe_ready_at of Time.time

    val transitions : {id: CPN'Id.id,
		       name: string,
		       status: status ref,
		       dep_list: int list,
		       inst: int,
                   priority: int,
		       bind_exe: mode * int -> result * string list, 
		       bind_fair: mode * int -> int * (result * string list) * (unit -> (result * string list))} 
	Array.array ref

    val bind_fair : 
      ('a -> 'b)
          -> ('a * 'c * bool -> result * 'd)
             -> ('e -> 'c)
                -> (mode * 'a -> result * 'd)
                   -> ('b -> int * 'e) -> mode * 'a -> int * (result * 'd) * (unit -> (result * 'd))
    val instances_changed: bool ref
    val generate_instances: bool ref

    val add_be : CPN'Id.id * (mode * int -> result * string list) * (mode * int
    -> int * (result * string list) * (unit -> result * string list)) * int -> unit
    type inst_dump
    val dump_inst : unit -> unit
    val print_scheduler_status : unit -> unit
    val load_inst : int * (CPN'Id.id * inst_dump) list -> unit

    (* ref to list of (func name * (unit -> unit)) *)
    val init_state_funs:  (string * (unit -> unit)) list ref
    val insert_init_state_fun: string * (unit -> unit) -> unit
    val time_inc_funs: (Time.time * Time.time -> unit) list ref
    (* ref to list of (func name * (unit -> unit)) *)
    val step_inc_funs: (string * (unit -> unit)) list ref
    val insert_step_inc_fun: string * (unit -> unit) -> unit
    (* ref to list of (func name * (unit -> unit)) *)
    val stop_funs:  (string * (unit -> unit)) list ref
    val insert_stop_fun: string * (unit -> unit) -> unit
    (* ref to list of (func name * (unit -> unit)) *)
    val stop_crit_funs:  (string * (unit -> unit)) list ref
    val insert_stop_crit_fun: string * (unit -> unit) -> unit

    val no_enabled_msg: string ref

    val stop_simulator: string -> unit
    val clear_software_stop_req: unit -> unit

    val inc_step : unit -> unit
    val step : unit -> IntInf.int
    val inst : unit -> int
    val subrun : unit -> int

    val set_report_options : bool * bool -> unit (* temporary hook for setting sim report options *)
    val set_warmup_length: Time.time -> unit
    val save_report : string -> unit
    val clear_report : unit -> unit

    val create_scheduler_standalone : unit -> unit
    val create_scheduler : unit -> unit
    val reset_scheduler : unit -> unit
    val reset_sim : unit -> unit
    val init_state : unit -> unit
    val init_all : unit -> unit

    val check_enab : CPN'Id.id * int -> bool
    val check_enab_no_scheduler : CPN'Id.id * int -> bool
    val man_bind : CPN'Id.id * int * bool -> string * string * string
    val pick_bind : CPN'Id.id * int * (string * string) list -> 
		    string * string * string
    val run : unit -> string * string * string
    val build_all_enabled: unit -> (int * int) * (int list) * ((int * int * int * (unit -> (result * string
    list))) list) * ((int * int * int * (unit -> (result * string
    list))) list)
    val random_step : unit -> ((string * string * string) * 
			      (* The transition instance, if any,
			       * that occurred *)
			      (CPN'Id.id * int) option * 
			      (* hack to indicate absence of unexpected problems 
			       * true: transition occurred, or stop criterium was met, or no enabled transition
			       * false: an unexpected exception was raised *)
			      bool)

    val print_enab : (CPN'Id.id * int) list -> bool list
    val print_enab_no_scheduler : (CPN'Id.id * int) list -> bool list
    val print_size : (CPN'Id.id * int) list -> int list
    val print_mark : (CPN'Id.id * int) list -> string list
    val print_page_enab : 
	(CPN'Id.id * int) list -> ((CPN'Id.id * int) * bool) list
    val print_page_size : 
	(CPN'Id.id * int) list -> ((CPN'Id.id * int) * int) list
    val print_page_mark : 
	(CPN'Id.id * int) list -> ((CPN'Id.id * int) * string) list

    val change_mark : (CPN'Id.id * int) list * bool -> unit
    val change_model_time : string -> unit
    val increase_model_time : unit -> bool * string

    (* internal stuff *)

    val each_place : bool * (bool * result) -> bool * result
    val each_timed_place : bool * Time.time option * (bool * result) 
	                   -> bool * result

    val collect_token : exn -> 
	'a ref
	* (('b Time.timed -> order) -> 'a -> 'c CPN'MS.ms)
	* ('b Time.timed * 'd Time.timed -> order) * 'd
	* Time.time
	-> 'c

    val collect_tms : exn -> 
	'a ref * (('b Time.timed -> order) -> 'a -> 'c CPN'MS.ms)
	* ('b Time.timed * 'd Time.timed -> order)
	* ('a ref * 'c -> unit)
	* 'd Time.timed CPN'MS.ms
	-> 'c CPN'MS.ms

    val fetch_tms : exn -> ('a * 'a -> bool)
	                -> 'a Time.timed CPN'MS.ms * int * 'a
                        -> 'a Time.timed CPN'MS.ms

    val pause_before : ((CPN'Id.id * bool) -> (int * string)) -> CPN'Id.id * int -> unit

    val pause_after : ((CPN'Id.id * bool) -> (int * string)) -> CPN'Id.id * int -> unit

    val tst_ill_marks : (CPN'Id.id * string) list ref -> unit

    val code_action : (int -> 'a -> 'b) -> int -> 'a -> 'b

    val monitor : (int -> 'a -> 'b) -> int -> 'a -> 'b
    val monitor_exn_source : string ref

    val get_internal_structure : unit ->
    {
    headpriority : int,
    unknowns : (int * int list) list,
    maybe_readies : (Time.time * int) list,
    transitions : (int * string * int list) list,
    prioritized : (int * int * CPN'Id.id * int) list,
    time : string,
    vector : (int * int list) option list
    }

    val call_stop_funs : unit -> unit
    val make_response : unit -> string * string * string
    val check_stop_crit : unit -> bool

end
