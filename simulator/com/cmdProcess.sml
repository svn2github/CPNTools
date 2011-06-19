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
(*
  Module:	    Command Process (CMD)

  Description:	    The DMO process command processor. Contains functions
  		    to establish connections with the GRAM (as master
		    or slave). Provides streams for making function
		    call requests with or w/o automatic glue. All
		    requests to the GRAM should originate through this module. 
		    Provides a 'wait' function with should be used when waiting
		    for data from the GRAM. This function can process any
		    requests received from the GRAM during the wait.

  CPN Tools
*)


(* $Source: /users/cpntools/repository/cpn2000/sml/com/cmdProcess.sml,v $ *)

val rcsid = "$Header: /users/cpntools/repository/cpn2000/sml/com/cmdProcess.sml,v 1.4.2.2 2006/06/29 06:57:20 mw Exp $";


import "stream.sig";
import "simProcess.sig";
import "evalProc.sig";
import "error.sig";
import "miscUtils.sig";
import "majorOpcodes.sig";
import "cmdProcess.sig";
import "osdep.sig";


functor CmdProcess (structure Str : STREAM
		    structure Eval : EVALPROCESS
		    structure Sim : SIMPROCESS
		    structure Err : GRAMERROR
		    structure Utils : MISCUTILS
		    structure Ops : MAJOROPCODES
		    structure OSDep : OSDEP

		    sharing Str = Eval.Stream = Sim.Stream
			) : CMDPROCESS = struct
    
    val rcsid = "$Header: /users/cpntools/repository/cpn2000/sml/com/cmdProcess.sml,v 1.4.2.2 2006/06/29 06:57:20 mw Exp $";

    val version = ref "Portions Copyright (c) 2010-2011 AIS Group, Eindhoven University of Technology\n";

    fun get id =
        case (List.find (fn (name, _) => name = id) (Posix.ProcEnv.uname ()))
          of (SOME (_, value)) => value
           | NONE => "<unknown>"

    val copyright = ref (String.concat
                      ["Simulator/CPN v3.1.9 [built: ", 
                      Date.toString (Date.fromTimeLocal (Time.now())), " by ", 
                      Posix.ProcEnv.getlogin (), " on ", get "nodename", "]"])

    structure Str = Str;

    val std_out = TextIO.stdOut;
    val output = TextIO.output;
    val output1 = TextIO.output1;
    val flush_out = TextIO.flushOut;

    type gramAppl = {name:string, 
		     outs:Str.outstream,
		     ins:Str.instream};
    datatype connMode = Master of string | Slave;
    datatype eventType = Result | GfcResult | ChartResult | MimicResult | Any | None;

    exception cmdProcessFail of string;
    exception ReqFail of string;	
    exception gfcReqFail of string;
    exception chartReqFail of string;
    exception mimicReqFail of string;
    exception stopLoop of unit;

    val startupMsgTermChar = chr 1;

    val theGram = ref {name="",outs = Str.makeOut (OSDep.getGramOutIOD ()), 
		       ins = Str.makeIn (OSDep.getGramInIOD()) };

    fun estabConnection (Master gramName) =
	(Err.log  "SYSTEM ERROR: Master mode not supported yet.";
	 raise cmdProcessFail "SYSTEM ERROR: Master mode not supported yet.")
      | estabConnection Slave = 
	{name = "",
	 outs = Str.makeOut (OSDep.getGramOutIOD ()),
	 ins = Str.makeIn (OSDep.getGramInIOD ())};

    fun breakConnection ({outs,ins,...}:gramAppl) =
	(Str.destroyIn ins;
	 Str.destroyOut outs);

    fun interrupt ({outs,ins,...}:gramAppl) = 
	(Err.log "SYSTEM ERROR: Master mode not supported yet; no interrupts.";
	 raise cmdProcessFail "SYSTEM ERROR: Master mode not supported yet; no interrupts.");


    fun genericWait evalProcess ({outs,ins,...}:gramAppl,event) =
	let
	    fun process () =
		case (Str.getInteger ins) of

		    1 => (evalProcess(ins);
			  process())

		  | 2 => if (event = Result) 
			    orelse (event = Any) then Result
			 else (Err.log "SYSTEM ERROR: CmdProcess.genericWait: Unexpected receipt of result.";
			       process())

		  | 4 => if (event = Result) 
			     orelse (event = Any) then 
			     raise ReqFail "Call failed in GRAM."
			 else (Err.log "SYSTEM ERROR: CmdProcess.genericWait: Unexpected receipt of result error.";
			       process())

		  | 5 => if (event = GfcResult) 
			     orelse (event = Any) then GfcResult
			 else (Err.log "SYSTEM ERROR: CmdProcess.genericWait: Unexpected receipt of GFC result.";
			       process())

		  | 6 => if (event = GfcResult) 
			     orelse (event = Any) then 
			     raise gfcReqFail "Call Failed in GRAM"
			 else (Err.log "SYSTEM ERROR: CmdProcess.genericWait: Unexpected receipt of GFC result error.";
			      process())

		  | 7 => if (event = ChartResult) 
			     orelse (event = Any) then ChartResult
			 else (Err.log "SYSTEM ERROR: CmdProcess.genericWait: Unexpected receipt of Chart result.";
			       process())

		  | 8 => if (event = ChartResult) 
			     orelse (event = Any) then 
			     raise chartReqFail "Call Failed in GRAM"
			 else (Err.log "SYSTEM ERROR: CmdProcess.genericWait: Unexpected receipt of chart result error.";
			      process())

		  | 9 => (Sim.process(ins,outs);
			     process())

		  | 10 => if (event = MimicResult) 
			     orelse (event = Any) then MimicResult
			 else (Err.log "SYSTEM ERROR: CmdProcess.genericWait: Unexpected receipt of Mimic result.";
			       process())

		  | 11 => if (event = MimicResult) 
			     orelse (event = Any) then 
			     raise mimicReqFail "Call Failed in GRAM"
			 else (Err.log "SYSTEM ERROR: CmdProcess.genericWait: Unexpected receipt of Mimic result error.";
			      process())

		  | opcode => (Err.log ("SYSTEM ERROR: CmdProcess.genericWait: Unexpected opcode received.("^(Int.toString opcode)^")");
			  process());
	in
	    process() 
	end;
	
			
    (* wait with eval i.e. "use" *)
    val waitWithEval = genericWait Eval.process;
    (* wait w/o eval *)
    val waitWoutEval = genericWait Eval.processStub;
	
    fun genericBgLoop waitFn theGram = 
	let
	    fun loop () : unit = 
		(* Call 'waitFn' with our interrupt handler installed.
		 * Our handler is empty. Should an  interrupt occur
		 * while waitFn is running, the handler will
		 * be invoked and execution will proceed at the
		 * continuation of the 'Utils.handleInterrupt' exp.
		 * i.e. at 'loop()'
		 *)
		(Utils.handleInterrupt (fn () => waitFn (theGram,None))
		                       (fn () => ())
		     handle Str.connFail s => raise stopLoop ()
			  | _  => ();
		 loop())
	in
	    output(std_out,Compiler.banner);
	    output(std_out,("\n"^(!copyright)^"\n"));
	    output(std_out,("\n"^(!version)));
	    output1(std_out,startupMsgTermChar);

	    flush_out std_out;
	    loop()
	end;

    val bgLoopWithEval = genericBgLoop waitWithEval;
    val bgLoopWoutEval = genericBgLoop waitWoutEval;
	

    fun getStreams ({ins,outs,...}:gramAppl) = 
	(Str.putInteger outs Ops.Req;
	 (outs,ins));
	

    fun getGfcStreams ({ins,outs,...}:gramAppl) =
	(Str.putInteger outs Ops.gfcReq;
	 (outs,ins));

    fun getChartStreams ({ins,outs,...}:gramAppl) =
	(Str.putInteger outs Ops.chartReq;
	 (outs,ins));
    
    fun getMimicStreams ({ins,outs,...}:gramAppl) =
	(Str.putInteger outs Ops.mimicReq;
	 (outs,ins));
    
    fun emlProcessOneReq ({outs,ins,...}:gramAppl) = 
	case (Str.getInteger ins) of

	    1 => Eval.process(ins)

	  | 2 => Err.log "SYSTEM ERROR: CmdProcess.emlProcessOneReq: Unexpected receipt of result."

	  | 4 => Err.log "SYSTEM ERROR: CmdProcess.emlProcessOneReq: Unexpected receipt of result error."

	  | 5 => Err.log "SYSTEM ERROR: CmdProcess.emlProcessOneReq: Unexpected receipt of GFC result."

	  | 6 => Err.log "SYSTEM ERROR: CmdProcess.emlProcessOneReq: Unexpected receipt of GFC result error."

	  | 9 => Sim.process(ins,outs)

	  | _ => Err.log "SYSTEM ERROR: CmdProcess.emlProcessOneReq: Unexpected opcode received.";
 


end; (* CMDPROCESS *) 

    
