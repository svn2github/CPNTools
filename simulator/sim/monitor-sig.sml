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
(* File: monitor-sig.sml
 *
 * 
 *)

signature MONITORSUBNET = sig
    type bindelem
    type markings
    type subnet
    val get_subnet: bindelem -> subnet
    val get_markings: unit -> markings
end

signature MONITOR = sig
    include MONITORSUBNET

    val monitor: bindelem -> unit
    val is_active: bool ref
end

signature INITSTOPMONITOR = sig
    include MONITOR

    val init_monitor: unit -> unit
    val stop_monitor: unit -> unit
end

signature GENERICDCMONITOR = sig
    include INITSTOPMONITOR

    type data
 
    val update_logfile: bool ref

    val count: unit -> int
    val min: unit -> data
    val max: unit -> data
    val first: unit -> data
    val last: unit -> data

    val avrg: unit -> real
    val ci: int -> {percentage: int,
		    avrg : real,
		    half_length : real option,
		    upper_endpoint : real option,
		    lower_endpoint : real option} 
    val ssd: unit -> real
    val vari: unit -> real
    val std: unit -> real

    val get_stat_strings: unit -> string * CPN'StatStrings
end

signature UNTIMEDDCMONITOR = sig
    include GENERICDCMONITOR

    structure SV : CPN'UNTIMEDSTATVAR
    val statvar : SV.Statvar

    sharing type data = SV.data

    val sum: unit -> data
    val ss: unit -> data

    val count_iid : CPN'IUSV.Statvar
    val min_iid : SV.Statvar 
    val max_iid : SV.Statvar
    val sum_iid : SV.Statvar
    val avrg_iid : CPN'RUSV.Statvar
    val ssd_iid : CPN'RUSV.Statvar 
    val std_iid : CPN'RUSV.Statvar 
    val vari_iid : CPN'RUSV.Statvar
 
end

signature TIMEDDCMONITOR = sig
    include GENERICDCMONITOR

    structure SV : CPN'TIMEDSTATVAR
    val statvar : SV.Statvar
    structure MSV : CPN'UNTIMEDSTATVAR

    sharing type data = SV.data
    type sumtype
    sharing type sumtype = SV.sumtype
    type time
    sharing type time = SV.time

    val sum: unit -> sumtype
    val ss: unit -> sumtype

    val starttime: unit -> time
    val lasttime: unit -> time
    val interval: unit -> time

    val count_iid : CPN'IUSV.Statvar
    val min_iid : MSV.Statvar 
    val max_iid : MSV.Statvar
    (* sum_iid intentionally left out as there are currently
     * no examples of when it would be useful 
     * sum_iid can be intinf or real *)
    val avrg_iid : CPN'RUSV.Statvar
    val ssd_iid : CPN'RUSV.Statvar 
    val std_iid : CPN'RUSV.Statvar 
    val vari_iid : CPN'RUSV.Statvar
 end

signature CPN'MONITORS = sig

    structure Sim : CPN'SIM

    val set_is_active : (CPN'Id.id * bool) list -> unit list

    val insert_fun : CPN'Id.id * string * 'a * (CPN'Id.id * string * 'a) list ref
                     -> unit
    val rm_fun : CPN'Id.id * (CPN'Id.id * 'b * 'c) list ref -> unit

    val sim_init_fun_list : (CPN'Id.id * string * (unit -> unit)) list ref
    val init_sim_monitors : unit -> unit
    val sim_stop_fun_list : (CPN'Id.id * string * (unit -> unit)) list ref
    val stop_sim_monitors : unit -> unit
    val step_monitor_fun_list : (CPN'Id.id * string * (unit -> unit)) list ref
    val sim_stop_crit_fun_list:(CPN'Id.id * string * (unit -> unit)) list ref  
    val monitor_step : unit -> unit

    val rep_init_fun_list : (CPN'Id.id * string * (unit -> unit)) list ref
    val init_rep_monitors : unit -> unit
    val rep_stop_fun_list : (CPN'Id.id * string * (unit -> unit)) list ref
    val stop_rep_monitors : int -> unit

    val post_rep_monitor_fun_list : (CPN'Id.id * string * (unit -> unit)) list ref
    val monitor_pre_rep : unit -> unit
    val pre_rep_monitor_fun_list : (CPN'Id.id * string * (unit -> unit)) list ref
    val monitor_post_rep : string * string * string -> unit

    functor CreateBreakpoint (type bindelem 
			      and subnet
			      and markings
			      val pred: subnet -> bool
			      and get_subnet: bindelem -> subnet
			      and get_markings: unit -> markings
			      and name: string): MONITOR
						 
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
			     and montype : CPN'MonitorTable.montype
			     and updatelogfile: bool) : UNTIMEDDCMONITOR

    functor CreateTimedDC (type bindelem 
			   and subnet
			   and markings
			   structure SV: CPN'TIMEDSTATVAR
			   structure MSV: CPN'UNTIMEDSTATVAR (* for min, max iid values*)
			   sharing type SV.data = MSV.data
			   val pred: subnet -> bool
			   and obs : subnet -> SV.data
			   and init : markings -> SV.data option
			   and stop : markings -> SV.data option
			   and get_subnet: bindelem -> subnet
			   and get_markings: unit -> markings
			   and name: string
			   and updatelogfile: bool) : TIMEDDCMONITOR

    functor CreateUserDef(type bindelem 
			  and subnet
			  and markings
			  val get_subnet: bindelem -> subnet
			  val get_markings: unit -> markings
			  and pred: subnet -> bool
			  and init: markings -> unit
			  and stop: markings -> unit
			  (* compose action and observation functions
			   * to avoid problems of not knowing return 
			   * type of obs function *)
			  and action_obs: subnet -> unit
			  and name: string): INITSTOPMONITOR

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
			     and name: string): INITSTOPMONITOR
end
