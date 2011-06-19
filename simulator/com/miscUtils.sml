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
  Module:	    Misc Utilities (MUT)

  Description:	    All shareable utilities that do not belong to any
  		    specific module should be placed here.

  CPN Tools
*)



(* $Source: /users/cpntools/repository/cpn2000/sml/com/miscUtils.sml,v $ *)

val rcsid = "$Header: /users/cpntools/repository/cpn2000/sml/com/miscUtils.sml,v 1.1.1.1 2001/10/02 11:34:22 cpn Exp $";


import "miscUtils.sig";
import "osdep.sig";


functor MiscUtils (structure OSDep : OSDEP) : MISCUTILS = struct

    local
       infix +=;
       fun r += (i: int) = r:= (!r)+i;

       val userRequest1 = ref 0 (* The number of userRequest1 signals *)
       val userRequest2 = ref 0 (* The number of userRequest2 signals *)

       val userRequest1Handler = (fn n => userRequest1 += n);
       val userRequest2Handler = (fn n => userRequest2 += n);

       (* Install handlers of user requests *)
       val _ = OSDep.installUserRequest1Handler userRequest1Handler;
       val _ = OSDep.installUserRequest2Handler userRequest2Handler;
    in
       fun clearUserRequest1 () = userRequest1:= 0
       fun clearUserRequest2 () = userRequest2:= 0
           
       fun getUserRequest1 () = !userRequest1
       fun getUserRequest2 () = !userRequest2
    end

      
    (*
     * Interrupt handler function
     *
     * executes given function (code) with an interrupt handler installed.
     * handles the interrupt with the provided handler (handler).
     *)

    fun handleInterrupt code handler = 
	let
	    val oldSigintHandler = Signals.inqHandler 
		                   Signals.sigINT;
	    fun restoreHandler () = Signals.setHandler
		                    (Signals.sigINT, oldSigintHandler);
	in
	    (* Execute 'code()' with our 'handler' installed;
	     * Our handler invokes the users' provided handler
	     * and returns a continuation 'k'. Throwing
	     * to will cause execution to proceed with the line
	     * which reinstalls the old signal handler
	     *)

	    (* 
	     * What happens when an interrupt occurs?
	     *
	     * So, if during the execution of 'code()', an interrupt signal
	     * is received by the process. ML's runtime will invoke our
	     * handler which will invoke the user's handler. Our handler will
	     * return with 'k'. ML's runtime will continue execution
	     * at 'k'.
	     *)

	    (SMLofNJ.Cont.callcc (fn k => (Signals.setHandler 
			      (Signals.sigINT, 
			       Signals.HANDLER(fn _ => (handler (); k))); 
			      code ();())))
	         handle any => (restoreHandler();
				raise any);
	    restoreHandler();
            ()
	end;


    fun makeInterruptableFn theFn theHandler theFnArgs= 
	let
	    val oldSigintHandler = Signals.inqHandler 
		                   Signals.sigINT;

	    fun setIntHandler aHandler = 
		Signals.setHandler (Signals.sigINT,
					   aHandler);

	    fun restoreHandler () = setIntHandler oldSigintHandler;

	    fun installHandlerWithCont cont = 
		setIntHandler (Signals.HANDLER (fn _ => (theHandler();
					        cont)));

	    val theResult = SMLofNJ.Cont.callcc (fn k => (installHandlerWithCont k;
					     theFn theFnArgs;
					     ())) 
		                handle any => (restoreHandler ();
					       raise any);
	in
	    restoreHandler ();
	    theResult
	end;

end;  (* Functor MiscUtils *)
