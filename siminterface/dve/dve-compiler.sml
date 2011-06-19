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
 *  File:
 *     dve-compiler.sml
 *
 *  Created:
 *     Nov. 13, 2007
 *
 *  Description:
 *     Generate all the sml code for a dve model.  Creates all the necessary
 *  sml and cm files.
 *)


structure DveCompiler: sig

val compile:
    string * string * bool * TextIO.outstream option
    -> unit

val compileDir:
    string * string * bool * TextIO.outstream option
    -> unit

end = struct

fun compile (inFile, path, checks, errStream) = let
    fun compile sys = let
	val exported: string list ref = ref []
	val files: string list ref = ref []
	val dateStr = Date.toString (Date.fromTimeUniv (Time.now ()))
	fun out file str = TextIO.output (file, str)
	fun createFile fileName export f params = let
	    val data = List.concat (List.map (fn f => f params) f)
	    val file = TextIO.openOut (OS.Path.concat (path, fileName))
	    val out = out file
	in
	    files := !files @ [ fileName ];
	    exported := !exported @ export;
	    out ("(*\n");
	    out (" * File:            " ^ fileName ^ "\n");
	    out (" * Generation date: " ^ dateStr ^ "\n");
	    out (" *\n");
	    out (" * Run-time checks: " ^ (Bool.toString checks) ^ "\n");
	    out (" *)\n\n");
	    List.app (fn str => out str) data;
	    TextIO.closeOut file
	end
	fun createCM fileName = let
	    val file = TextIO.openOut (OS.Path.concat (path, fileName))
	    val out = out file
	in
	    out ("(*\n");
	    out (" * File:            " ^ fileName ^ "\n");
	    out (" * Generation date: " ^ dateStr ^ "\n");
	    out (" *)\n\n");
	    out "library\n";
	    List.app (fn e => out ("\tstructure " ^ e ^ "\n")) (!exported);
	    out "is\n\n";
	    out "#if (defined(SMLNJ_VERSION))\n";
	    out "\t$/basis.cm\n";
	    out "\t$/smlnj-lib.cm\n";
	    out "#endif\n";
	    List.app (fn f => out ("\t" ^ f ^ "\n")) (!files);
	    TextIO.closeOut file
	end
	val sys = DveSimplifier.simplify sys
    in
	createFile "definitions.sml"
		   [ "DveDefinitions" ]
		   [ DveDefinitionsCompiler.gen ] sys;
	createFile "enabling-test.sml"
		   [ "DveEnablingTest" ]
		   [ DveEnablingTestCompiler.gen ] (sys, checks);
	createFile "event-execution.sml"
		   [ "DveEventExecution" ]
		   [ DveEventExecutionCompiler.gen ] (sys, checks);
	createFile "model.sml"
		   [ "DveModel" ]
		   [ DveModelCompiler.gen ] (sys, checks);
	createFile "serializer.sml"
		   [ "DveStateSerializer",
		     "DveEventSerializer",
		     "DveEventListSerializer" ]
		   [ DveSerializerCompiler.genState,
		     DveSerializerCompiler.genEvent ] sys;
	createFile "order.sml"
		   [ "DveStateOrder",
		     "DveEventOrder" ]
		   [ DveOrderCompiler.genState,
		     DveOrderCompiler.genEvent ] sys;
	createFile "hash-function.sml"
		   [ "DveHashFunction",
		     "DveComponentsHashFunction",
		     "DveLargeComponentsHashFunction" ]
		   [ DveHashFunctionCompiler.gen,
		     DveHashFunctionCompiler.genHashComponents,
		     DveHashFunctionCompiler.genHashLargeComponents ] sys;
	createFile "components.sml"
		   [ "DveComponents",
		     "DveLargeComponents" ]
		   [ DveComponentsCompiler.gen ] sys;
	case System.getProg sys
	 of NONE => ()
	  | SOME _ => createFile "progress.sml"
				 [ "DveProgress" ]
				 [ DveProgressCompiler.gen ] (sys, checks);
	createCM "all-no-ind.cm";
	createFile "independence-relation.sml"
		   [ "DveIndependenceRelation" ]
		   [ DveIndependenceRelationCompiler.gen ] sys;
	createCM "all.cm"
    end
in
    case DveParser.parse (inFile, errStream)
     of NONE => ()
      | SOME sys => compile sys
end

fun compileDir (inDir, outDir, checks, errStream) = let
    val stream = OS.FileSys.openDir inDir
    fun loop NONE = ()
      | loop (SOME f) = let
            val full   = OS.Path.concat (inDir, f)
            val split  = OS.Path.splitBaseExt (f)
            val model  = #base split
            val ext    = #ext split
            val outDir = OS.Path.concat (outDir, model)
        in
            if not (isSome ext) orelse valOf ext <> "dve"
	    then ()
            else (TextIO.print ("(*  model " ^ model ^ "  *)\n");
                  (Posix.FileSys.mkdir (outDir, Posix.FileSys.S.irwxu)
                   handle SysErr => ());
                  compile (full, outDir, checks, errStream))
                 handle Errors.CompilerError (lineNo, msg) =>
			Errors.outputErrorMsg
                            (errStream, full, SOME lineNo, msg);
	    loop (OS.FileSys.readDir stream)
        end
in
    loop (OS.FileSys.readDir stream)
end

end
