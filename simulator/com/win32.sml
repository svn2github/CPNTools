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

val rcsid = "$Header: /users/cpntools/repository/cpn2000/sml/com/win32.sml,v 1.2.2.1 2006/06/29 06:57:20 mw Exp $";

import "osdep.sig";

structure OSDep : OSDEP =
struct
    local
	fun getWordEnv name =  case OS.Process.getEnv name of
	                          SOME s => valOf(Word32.fromString s)
	                        | NONE => Word32.fromInt 0;
    in
	fun getGramOutIOD () = Win32.FileSys.hndlToIOD(getWordEnv "EVALWHANDLE");
	fun getGramInIOD () = Win32.FileSys.hndlToIOD(getWordEnv "EVALRHANDLE");
    end;
    
    exception IOFail of string;

    fun write (iodesc, ba, l, pos) : int = 
	if(l > 0) then
	    let
		val len = Win32.IO.writeArr(Win32.FileSys.IODToHndl iodesc, Word8ArraySlice.slice (ba, pos, SOME l ))
		    handle _ => raise IOFail "write"
	    in
		if len = 0 then
		    raise IOFail "write"
		else
		    len
	    end
	else
	    0;
	    
    fun read (iodesc:OS.IO.iodesc, ba:ByteArray.bytearray, l:int, pos) : int = 
	if(l > 0) then
	    let
		val len = Win32.IO.readArr(Win32.FileSys.IODToHndl iodesc, Word8ArraySlice.slice (ba, pos, SOME l))
		    handle _ => raise IOFail "write"
	    in
		if len = 0 then
		    raise IOFail "write"
		else
		    len
	    end
	else
	    0;

    (* Not access to pids on WIN32, so use a 'random' number *)
    fun getpid () = Word32.fromLargeInt(Time.toSeconds(Time.now()));

    (* Not yet supported on WIN32 *)
    fun installUserRequest1Handler h = ();
    fun installUserRequest2Handler h = ();
end;
