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
                structure Extension : EXTENSION

		    sharing Str = Eval.Stream = Extension.Stream
			) : CMDPROCESS = struct
    
    val rcsid = "$Header: /users/cpntools/repository/cpn2000/sml/com/cmdProcess.sml,v 1.4.2.2 2006/06/29 06:57:20 mw Exp $";

    val version = ref "Portions Copyright (c) 2010-2012 AIS Group, Eindhoven University of Technology\n";

    fun get id =
        case (List.find (fn (name, _) => name = id) (Posix.ProcEnv.uname ()))
          of (SOME (_, value)) => value
           | NONE => "<unknown>"

    val copyright = ref (String.concat
                      ["Simulator/CPN v3.5.3 [built: ", 
                      Date.toString (Date.fromTimeLocal (Time.now())), " by ", 
                      (Posix.ProcEnv.getlogin () handle _ => "<unknown>"), " on ", get "nodename", "]"])

    structure Str = Str;

    val std_out = TextIO.stdOut;
    val output = TextIO.output;
    val output1 = TextIO.output1;
    val flush_out = TextIO.flushOut;

    type gramAppl = {name:string, 
		     outs:Str.outstream,
		     ins:Str.instream};
    datatype connMode = Master of string | Slave;
    datatype eventType = Result | GfcResult | CBResult | ExtSimResult | Any | None;

    exception cmdProcessFail of string;
    exception ReqFail of string;	
    exception gfcReqFail of string;
    exception stopLoop of unit;

    structure BIS = BIS(structure Stream = Str structure Err = Err)
    open BIS

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

    fun genericWait evalProcess (stream as {outs,ins,...}:gramAppl,event) =
	let
          fun evtToString Result = "Result"
            | evtToString GfcResult = "GfcResult"
            | evtToString CBResult = "CBResult"
            | evtToString ExtSimResult = "ExtSimResult"
            | evtToString Any = "Any"
            | evtToString None = "None"
          exception Finished of eventType
          fun process () =
          let
              val streams = ins::(Extension.getStream ())
(*              val _ = Err.log ("Waiting on select for " ^ (evtToString
event))*)
              val streams = Str.select streams
              val _ =
                  case (streams)
                    of ((SOME gram)::_) =>
                    let
                        val opcode = Str.getInteger gram
(*                        val _ = Err.log ("Received on GRAM "
                                    ^ (Int.toString opcode) ^ 
                                    " - " ^ (evtToString event))*)
                    in
                         case (opcode, event)
                           of (9, _) =>
                           let
                               val msg = read_message ins
                               val result = Sim.process msg
                               val result' = Extension.watched waitAndRead
                               (stream, ExtSimResult) (msg, result)
                           in
                               send_result Ops.simRes outs result'
                           end
                            | (1, _) => evalProcess ins
                            | (2, Result) => raise Finished Result
                            | (2, Any) => raise Finished Result
                            | (3, CBResult) => raise Finished CBResult
                            | (3, Any) => raise Finished CBResult
                            | (4, Result) => raise ReqFail "Call failed in GRAM"
                            | (4, Any) => raise ReqFail "Call failed in GRAM"
                            | (5, GfcResult) => raise Finished GfcResult
                            | (5, Any) => raise Finished GfcResult
                            | (6, GfcResult) => raise ReqFail "Call failed in GRAM"
                            | (6, Any) => raise ReqFail "Call failed in GRAM"
                            | _ => 
                                    Err.log ("ERROR: Received unexpected result with opcode "
                                    ^ (Int.toString opcode) ^ 
                                    " - " ^ (evtToString event))
                    end
                      | _ => ()
              val _ = 
                  case (streams)
                    of [_, SOME ext] =>
                    let
                        val (extin, extout) = Extension.getStreams ()
                        val opcode = Str.getInteger ext
(*                        val _ = Err.log ("Received on EXT "
                                    ^ (Int.toString opcode) ^ 
                                    " - " ^ (evtToString event))*)
                    in
                         case (opcode, event)
                           of (9, _) =>
                           let
                               val msg = read_message extin
                               val result = Sim.process msg
                           in
                               send_result Ops.simRes extout result
                           end
                            | (1, _) => evalProcess ins
                            | (3, _) =>
                           let
                               val msg = read_message extin
                               val _ = send_result Ops.cbReq outs msg
                               val _ = waitWithEval (stream, CBResult)
                               val result = read_message ins
                           in
                               send_result Ops.cbRes extout result
                           end
                            | (7, ExtSimResult) =>
                                        raise Finished ExtSimResult
                            | (7, Any) => raise Finished ExtSimResult
                            | _ =>
                                    Err.log ("ERROR: Received unexpected extension result with opcode "
                                    ^ (Int.toString opcode) ^ 
                                    " - " ^ (evtToString event))
                    end
                  | _ => ()
          in
              process ()
          end 
	in
	    (process(); event) handle (Finished v) => v
	end
    (* wait with eval i.e. "use" *)
    and waitWithEval p = genericWait Eval.process p
    (* wait w/o eval *)
    and waitWoutEval p = genericWait Eval.processStub p
    and waitAndRead (stream, event) ins =
      let
          val _ = waitWithEval (stream, event)
      in
          read_message ins
      end

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

    fun getGfcStreams ({ins,outs,...}:gramAppl) =
	(Str.putInteger outs Ops.gfcReq;
	 (outs,ins));

	val currentStream = ref (NONE: gramAppl option)

	(* main function for processing a simulator request *)
(*	fun process stream =
	    (currentStream:= SOME stream;
	     send_result Ops.simRes (#outs stream) (Sim.process
           (read_message (#ins stream)));
	     currentStream:= NONE)*)

	fun response (blist,ilist,slist) =
	    case !theGram of
		{ins,outs, ...} => 
		    (send_result Ops.simRes outs (blist, Sim.RESPTAG::ilist, slist);
		     case Str.getInteger ins of
			 (* remove opcode before getting lists *)
			 9 => read_message ins 
		       | _ => raise Sim.InternalError "Expected opcode not received in response")
end; (* CMDPROCESS *) 

    
