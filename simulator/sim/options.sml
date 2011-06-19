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
(* File: options.sml
 *
 * Simulation and initialization options.
 *)

functor CPN'CreateOptions(structure Time: CPN'TIME): CPN'OPTIONS = struct

    structure Time=Time

    datatype stop_crits =
	until_step of IntInf.int
      | additional_steps of IntInf.int
      | until_time of Time.time
      | additional_time of Time.time

    val stop_crits = ref (nil: stop_crits list)

    val pause_before_step = ref false
    val pause_after_step = ref false
    val pause_show_tokens = ref false
    val pause_cont_after = ref (NONE: int option)

    val report_transitions = ref false
    val report_bindings = ref false (* bindings only reported with transitions *)
    val report_function = ref (NONE: (CPN'Id.id * int -> string) option)

    val show_marking = ref true
    val show_enabling = ref true

    val fair_be = ref false
    val global_fair = ref false

    val seed = ref (SOME 87)
    val _ =  CPN'Random.init (!seed);

    val reset_ran_gen = ref false
    val reset_ref_vars = ref false

    val flush_files = ref false

local
    open Misc;
in
    fun set_sim {stop_crit=(untilstep, addsteps, untiltime, addtime),
		 pause=(beforestep,afterstep,showtok,contafter),
		 report=(reptrans,repbinds,repfun),
		 show=(showmark,showenab),
		 fairness=(be, global)
             } =
       (stop_crits:= nil;
	if untilstep="" then ()
	else stop_crits::= until_step (valOf(IntInf.fromString untilstep));
	if addsteps="" then ()
	else stop_crits::= additional_steps (valOf(IntInf.fromString addsteps));
        if untiltime="" orelse not(isSome Time.start_time)then ()
	else stop_crits::= until_time (Time.maketime untiltime);
	if addtime="" orelse not(isSome Time.start_time) then ()
	else stop_crits::= additional_time (Time.maketime addtime);
	pause_before_step:= beforestep;
	pause_after_step:= afterstep;
	pause_show_tokens:= showtok;
	pause_cont_after:= Int.fromString contafter;
        report_transitions:= reptrans;
	report_bindings:= (repbinds andalso reptrans);
	if repfun="" then report_function:= NONE
	else Compiler.Interact.useStream(TextIO.openString
		       ("CPN'Options.report_function:= SOME("^repfun^")"));
	show_marking:= showmark;
	show_enabling:= showenab;
      fair_be := be;
      global_fair := global
      )

    fun set_init {seed=no,reset=(ran_gen,ref_vars)} =
	(seed:= (if no="" then NONE else Int.fromString no);
	 CPN'Random.init (!seed);
	 reset_ran_gen:= ran_gen;
	 reset_ref_vars:= ref_vars)
end
end
