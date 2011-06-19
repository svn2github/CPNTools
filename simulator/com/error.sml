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
  Module:	    Error (ERR)

  Description:	    Error message and debugging support module. Contains
  		    functions which should be used to print debugging 
		    and error messages from regular code.

  CPN Tools
*)

import "error.sig";


functor GramError () : GRAMERROR = struct

    val debugState = ref false;
    val debugfilename = ref (NONE : (string option));
    val logstream = ref TextIO.stdErr;

    fun log s = (TextIO.output (!logstream, s^"\n");
		 TextIO.flushOut (!logstream))


    val ident = ref 0;
    val spaces = "                                                           ";
    val maxident= String.size spaces

    fun debug s =
     if not (!debugState) then ()
     else
	 let
           val tmpident= (!ident)
           val safeident= if (tmpident<0) then 0
                          else if (tmpident>maxident) then maxident
                               else tmpident
           val s'= (String.substring (spaces,0,safeident))^s
       in
           log s'
       end

    fun idebug (d,s) =
     if not (!debugState) then ()
     else
	 let
           val tmpident= (!ident)+(if (d<0) then d else 0)
           val safeident= if (tmpident<0) then 0
                          else if (tmpident>maxident) then maxident
                               else tmpident
           val s'= (String.substring (spaces,0,safeident))^s
           val _ = ident:= (!ident)+d
       in
           log s'
       end

    fun dumpstring s = TextIO.output(!logstream, s) 
		       
    (* Function used to dump boolean, integer, and lists 
     * received from and sent to GRAM *)
    fun dumpBISlists (initstr, blist, ilist, slist) = let
	
	fun dumplist mkstr list = let
	    
	    fun dump nil = dumpstring "]\n"
	      | dump (x::nil) = dumpstring ((mkstr x)^"]\n")
	      | dump (x::xs) = (dumpstring ((mkstr x)^","); dump xs)
	in
	    (dumpstring "["; dump list)
	    end
    in
	if !debugState then
	    (dumpstring initstr;
	     dumplist Bool.toString blist;
	     dumplist Int.toString ilist;
	     dumplist (fn s => "\""^s^"\"") slist;
	     TextIO.flushOut(!logstream))
	else ()
    end

    fun save_debug_info filename = 
	let
	    val fid = TextIO.openOut(filename)
	    val _ = (debugfilename := (SOME filename))
	    val _ = (logstream := fid)
	in
	    debugState := true
	end

    fun term_debug() = 
	((case (!debugfilename) of
	      SOME _ => (TextIO.closeOut(!logstream);
			 debugfilename := NONE)
	    | NONE => ());
	  logstream := TextIO.stdErr;
	  debugState := false)

end;

