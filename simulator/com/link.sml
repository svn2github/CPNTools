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
  Module:	    Linker

  Description:	    "use" this file to build the DMO.

  CPN Tools
*)


(* $Source: /users/cpntools/repository/cpn2000/sml/com/link.sml,v $ *)


CM.stabilize true "../image.cm";
(* FIXME 74+ Control.Elab.showTypeErrorCulprits := true;*)

structure Compiler = struct
	open Backend

	val version = Compiler.version
	val architecture = Compiler.architecture
	local
	  val {system, version_id, date} = version
	  fun to_string [] = ""
	  | to_string [minor] = Int.toString minor
	  | to_string (major::minors) = (Int.toString major) ^ "." ^ (to_string minors)
	in
	  val banner = system ^ " v" ^ (to_string version_id) ^ " [built: " ^ date ^ "]"
	end
	  
      structure Environment = Environment
	structure Control = Control
	structure Symbol = Symbol
	structure Ast = Ast
	structure Source = Source
	structure EnvRef = EnvRef
      structure MLParser = MLParser
      structure CompInfo = CompInfo
      structure ErrorMsg = ErrorMsg
end;

Compiler.Control.MC.matchRedundantError := false;
Compiler.Control.polyEqWarn := false;

use "importE.sml";
use "nxsml97.sml"; 

exception UnsupportedArch;
case SMLofNJ.SysInfo.getOSKind() of
    SMLofNJ.SysInfo.UNIX => use "posix.sml"
  | SMLofNJ.SysInfo.WIN32 => use "win32.sml"
  | _ => raise UnsupportedArch;

val tod = fn () =>  Time.toSeconds(Time.now());

import "unsafe";
import "miscUtils";
import "error";
import "debugmodule";
import "communicate";
import "byteArray";
import "stream";
import "majorOpcodes";
import "evalProc";
import "bis";
import "extension";
import "simProcess";
import "cmdProcess";
import "glue";
import "gramControl";
import "cpnml.sig";
import "toploop";

(*
 *
 * CpnMLSys: SYSTEM STRUCTURES
 *
 *)

structure CpnMLSys = struct

    structure GramUnsafe = GramUnsafe();

    structure MiscUtils = MiscUtils (structure OSDep = OSDep);

    structure GramError = GramError ();

    structure GramCommunicate = Communicate(structure Unsafe = GramUnsafe
					    structure OSDep = OSDep);

    structure ByteArrayExt = ByteArrayExt(structure Unsafe = GramUnsafe);

    structure GramStream = Stream(structure Comm = GramCommunicate
				  structure ByteArrayExt = ByteArrayExt
				  structure Unsafe = GramUnsafe);


    structure MajorOpcodes = MajorOpcodes ();

    structure EvalProcess = EvalProcess(structure Str = GramStream
					structure Err = GramError
					structure Utils = MiscUtils
					structure BAExt = ByteArrayExt);

    structure Extension = Extension(structure Stream = GramStream structure Err = GramError);

    structure SimProcess = SimProcess(structure Str = GramStream
                                      structure Extension = Extension
                                      structure Err = GramError
                                      structure BAExt = ByteArrayExt
                                      structure Opcodes = MajorOpcodes);

    structure CmdProcess = CmdProcess(structure Str = GramStream
				      structure Eval = EvalProcess
                              structure Sim = SimProcess
                              structure Extension = Extension
				      structure Err = GramError
				      structure Utils = MiscUtils
				      structure Ops = MajorOpcodes
				      structure OSDep = OSDep);

    structure Glue = Glue(structure Cmd = CmdProcess);

end; (* structure CpnMLSys *)

import "java-execute";

(*
 *
 * CpnML: USER STRUCTURES
 *
 *)

structure CpnML : CPNML = struct
    
    structure S = CpnMLSys;

    structure Toploop = Toploop(structure Cmd = S.CmdProcess);

    structure Control = GramControl(structure Cmd = S.CmdProcess
				    structure Err = S.GramError);

end; (* structure CpnML *)

