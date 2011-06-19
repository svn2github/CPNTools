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
(* File: bootstrap_simulator.sml
 *
 * Demonstration of how to boot and run the simulator stand-alone.
 *)

(* We do not have access to GUI *)
val _ = CPN'Settings.use_dmo:= false;

(* Bootstrap the simulator with the time type used *)
val _ = CPN'bootstrap("real","0.0","");
val _ = CPN'bootstrap("intinf","valOf(IntInf.fromString\"0\")","");
val _ = CPN'bootstrap("int","0","");
val _ = CPN'bootstrap("","","");
	 
(* Use the code dumped by a switch to simulator, insts. already generated *)
val _ = CPN'Sim.generate_instances:= false;
val _ = use "codedump.sml";

(* Create and init the simulation scheduler *)
val _ = CPN'Sim.instances_changed:= true;
val _ = CPN'Sim.create_scheduler_standalone(); (*Do not use create_scheduler*)
val _ = CPN'Sim.reset_scheduler();
(* What should reset when calling reset_sim?
 * reset= (reset-ran-gen?,reset-ref-vars?)
 * seed= if empty string use discrete(0,valOf(Int.maxInt)-1), else set seed for more reproducable sims *)
val _ = CPN'Options.set_init {reset=(false,true),seed="123"};
val _ = CPN'Sim.reset_sim();

(* Get ready to simulate *)
val _ = CPN'Sim.init_state();

(* Setup stop criterial -- can be combined *)
val _ = CPN'Options.stop_crits:= []; (* no stop criteria *)
val _ = CPN'Options.stop_crits:= [CPN'Options.until_step 1000];
val _ = CPN'Options.stop_crits:= [CPN'Options.additional_steps 100];
val _ = CPN'Options.stop_crits:= [CPN'Options.until_time 1000];
val _ = CPN'Options.stop_crits:= [CPN'Options.additional_time 100];

(* Set reporting options *)
val _ = CPN'Sim.Options.report_transitions := true;
val _ = CPN'Sim.Options.report_bindings := true;

(* Run the simulator *)
val (end_step,end_time,stop_message) = CPN'Sim.run();

(* See demo/simtimer.sml and demo/transtiming.sml for timing simulation runs *)
