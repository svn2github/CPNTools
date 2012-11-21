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
(* OS dependent functions *)

val rcsid = "$Header: /users/cpntools/repository/cpn2000/sml/com/posix.sml,v 1.2.2.1 2005/02/17 17:38:59 mw Exp $";

import "osdep.sig";

structure OSDep : OSDEP =
struct
    local
	fun getWordEnv name =  case OS.Process.getEnv name of
	                          SOME s => valOf(Word32.fromString s)
                           | NONE => Word32.fromInt 3
    in
      fun getGramOutIOD () = Posix.FileSys.fdToIOD( Posix.FileSys.wordToFD (Word32.fromInt 3));
      fun getGramInIOD () = Posix.FileSys.fdToIOD( Posix.FileSys.wordToFD (Word32.fromInt 3));
        fun getGramOutIOD () = Posix.FileSys.fdToIOD(Posix.FileSys.wordToFD
            (getWordEnv "EVALHANDLE"))
        fun getGramInIOD () = Posix.FileSys.fdToIOD(Posix.FileSys.wordToFD
            (getWordEnv "EVALHANDLE"))
    end;
	
    exception IOFail of string;
    local
	fun in_wait(iodesc:OS.IO.iodesc):unit
	    = let
		  val polldesc = valOf(OS.IO.pollDesc iodesc)
		  val polldesc_in = OS.IO.pollIn polldesc
		  val pinf = hd(OS.IO.poll([polldesc_in],NONE))
	      in
		  if (OS.IO.isIn pinf) then
		      ()
		  else
		      raise IOFail "in_wait"  (* alternatively  in_wait(fd) *)
	      end
	  
	fun out_wait(iodesc:OS.IO.iodesc):unit
	    = let
		  val polldesc = valOf(OS.IO.pollDesc iodesc)
		  val polldesc_out = OS.IO.pollOut polldesc
		  val pinf = hd(OS.IO.poll([polldesc_out],NONE))
	      in
		  if (OS.IO.isOut pinf) then
		      ()
		  else
		      raise IOFail "out_wait"  (* alternatively out_wait(fd) *)
	      end
	  
	(* protect a function call against signals *)
	fun protect f x 
	    = let
		  val save_mask = Signals.masked()
		  val _ = Signals.maskSignals Signals.MASKALL
		  val y = (f x) handle ex => (Signals.maskSignals save_mask;
					      raise ex)
	      in
		  Signals.maskSignals save_mask; y
       end

  fun protect f x = f x
    in

	fun write (iodesc:OS.IO.iodesc, ba:ByteArray.bytearray, l:int, pos) : int = 
	    let
		fun write' (iodesc:OS.IO.iodesc, ba:ByteArray.bytearray, l:int, pos) : int = 
		    let
			val len =  Posix.IO.writeArr(valOf(Posix.FileSys.iodToFD iodesc),
						     Word8ArraySlice.slice (ba, pos, SOME l))
		    in
			if len = 0 then
			    raise IOFail "write"
			else len
		    end
	    in
		(if (l > 0) then
		     (out_wait(iodesc);
		      protect (fn _ => write'(iodesc,ba,l,pos)) ())
		 else 0)
		     handle _ => raise IOFail "write"
	    end;

	fun read (iodesc:OS.IO.iodesc, ba:ByteArray.bytearray, l:int,pos) : int = 
	    let
		fun read' (iodesc:OS.IO.iodesc, ba:ByteArray.bytearray, l:int, pos) : int = 
		    let
			val len = Posix.IO.readArr(valOf(Posix.FileSys.iodToFD iodesc),
						   Word8ArraySlice.slice (ba, pos, SOME l))
		    in
			if len = 0 then
			    raise IOFail "read"
			else len
		    end
	    in
		(if (l > 0) then
		     (in_wait(iodesc);
		     protect (fn _ => read'(iodesc,ba,l,pos)) ())
		 else 0)
		     handle _ => raise IOFail "read"
	    end;
    end;

    val getpid = Posix.Process.pidToWord o Posix.ProcEnv.getpid;

    fun installUserRequest1Handler h = (Signals.setHandler(valOf(Signals.fromString("USR1")), 
							   Signals.HANDLER(fn (_,n,c) => (h n; c))); ());
    fun installUserRequest2Handler h = (Signals.setHandler(valOf(Signals.fromString("USR2")), 
							   Signals.HANDLER(fn (_,n,c) => (h n; c))); ());
    fun installUserRequest1Handler h = ();
    fun installUserRequest2Handler h = ();
end;
