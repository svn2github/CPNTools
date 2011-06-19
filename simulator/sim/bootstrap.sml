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
(* File: bootstrap.sml
 *
 * Bootstraps the simulator with type of time.
 *)

(* The third argument is intended for code generation dump filename *)

fun CPN'bootstrap (timetype, starttime, filename) =
    (case timetype of
	 "" => CPN'Env.use_string ["\n structure CPN'Time = CPN'UnitTime;"]
       | "intinf" => 
	     CPN'Env.use_string ["\n structure CPN'Time = CPN'IntInfTime\
	      \ (val start = ",starttime,");\n\
	      \ structure CPN'TMS = CPN'CreateTMS\
	      \ (structure Time = CPN'Time);\n\
	      \ structure TMS = CPN'TMS\
	      \ open CPN'Time TMS;"] (* ModelTime structure becomes visible
				      * when TMS is opened. If TMS is not
				      * opened then ModelTime structure
				      * must be created in another way *)
       | "int" => 
	     CPN'Env.use_string ["\n structure CPN'Time = CPN'IntTime\
	      \ (val start = ",starttime,");\n\
	      \ structure CPN'TMS = CPN'CreateTMS\
	      \ (structure Time = CPN'Time);\n\
	      \ structure TMS = CPN'TMS\
	      \ open CPN'Time TMS;"]
       | "real" => 
	     CPN'Env.use_string ["\n structure CPN'Time = CPN'RealTime\
	      \ (val start = ",starttime,");\n\
	      \ structure CPN'TMS = CPN'CreateTMS\
	      \ (structure Time = CPN'Time);\n\
	      \ structure TMS = CPN'TMS\n\
	      \ open CPN'Time TMS;"]
       | _ => raise InternalError("illegal time");
      CPN'Env.use_string["\n structure CPN'RepTable = CPN'CreateRepTable\
       \ (structure Time = CPN'Time);\n\
       \ structure CPN'ITSV = CPN'CreateITSV(CPN'Time);\n\
       \ structure CPN'RTSV = CPN'CreateRTSV(CPN'Time);\n\
       \ structure CPN'InstTable = CPN'CreateInstTable\
       \ (structure RepTable = CPN'RepTable);\n\
       \ structure CPN'SyntaxCheck = CPN'MakeSyntaxCheck\
       \ (structure InstTable = CPN'InstTable);\n\
       \ structure CPN'Options = CPN'CreateOptions\
       \ (structure Time = CPN'Time);\n\
       \ structure CPN'ColorSets = CPN'CreateColorSets\
       \ (structure Time = CPN'Time);\n\
       \ structure CPN'Decl = CPN'CreateDecl\
       \ (structure CS = CPN'ColorSets);\n\
       \ structure CPN'Place = CPN'Places;\n\
       \ structure CPN'PlaceSim = CPN'CreatePlace\
       \ (structure InstTable = CPN'InstTable);\n\
       \ structure CPN'Reference = CPN'References;\n\
       \ structure CPN'ReferenceSim = CPN'CreateReference\
       \ (structure InstTable = CPN'InstTable);\n\
       \ structure CPN'Transition = CPN'CreateTransition\
       \ (structure InstTable = CPN'InstTable\
       \ and Decl = CPN'Decl);\n\
       \ structure CPN'Sim = CPN'MakeSim\
       \ (structure Options = CPN'Options\
       \ and InstTable = CPN'InstTable\
       \ and Places = CPN'Place\
       \ and References = CPN'Reference);\n\
       \ val step = CPN'Sim.step;\n\
       \ val inst = CPN'Sim.inst;\n\
       \ structure CPN'Replications = CPN'MakeReplications\
       \(structure Sim = CPN'Sim);\n\
       \ structure CPN'Monitors = CPN'Monitors\
       \  (structure Sim = CPN'Sim\
       \   and Replications = CPN'Replications);\n\
       \ structure Replications = CPN'Replications;\
       \ structure CPN'CreateMonitor = CPN'CreateMonitor\
       \  (structure Sim = CPN'Sim\
       \   and Monitors = CPN'Monitors\
       \   and Decl = CPN'Decl);\n \
       \ structure CPN'StandardMonitors = CPN'StandardMonitors\
       \  (structure Time = CPN'Time\
       \   and CreateMonitor = CPN'CreateMonitor \
       \   and InstTable = CPN'InstTable);\n \
       \ structure CPN'SimGlue = CPN'CreateSimGlue\
       \ (structure Options = CPN'Options\
       \ and Decl = CPN'Decl\
       \ and SyntaxCheck = CPN'SyntaxCheck\
       \ and CreateMonitor = CPN'CreateMonitor\
       \ and Monitors = CPN'Monitors\
       \ and StandardMonitors = CPN'StandardMonitors\
       \ and InstTable = CPN'InstTable\
       \ and Sim = CPN'Sim\
       \ and Place = CPN'PlaceSim\
       \ and Reference = CPN'ReferenceSim\
       \ and Transition = CPN'Transition);\n\
       \ val _ = CPN'CodeGen.set_dumping_filename(\"",filename,"\");\n\
       \ val _ = CPN'MonitorAux.get_port_socket_chain:=CPN'InstTable.get_inst_cons;\n\
       \ val _ = CPN'PerfReport.step:=CPN'Sim.step;\n\
       \ val _ = CPN'PerfReport.timestr:=(fn () => CPN'Time.mkstr(time()));\n\
       \ val _ = CPN'Env.init();"];


      (nil,nil,nil));

val _ = CpnMLSys.SimProcess.NSBootstrap := CPN'bootstrap;



    (* When debugging on windows, use the following line instead of the 
     * corresponding line above.
     *
     * Also go to basic.sml and change the CPN'start_timing prefix.
     *
     * \ val _ = CPN'CodeGen.set_dumping_filename(\"c:/codedump.sml\");\n\ 
     *)
