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
 *     dve-parser.sml
 *
 *  Created:
 *     Nov. 13, 2007
 *
 *  Description:
 *     Parser for dve specifications.
 *)


structure DveParser: sig

    val parse: string * TextIO.outstream option -> System.system option

end = struct

structure DveLrVals = DveLrValsFun(
structure Token = LrParser.Token)

structure Lex = DveLexFun(
structure Tokens = DveLrVals.Tokens)

structure Parser = Join(
structure ParserData = DveLrVals.ParserData
structure Lex        = Lex
structure LrParser   = LrParser)

exception SyntaxError of int
			 
fun invoke lexstream = let
    fun print_error (s,i:int,_) = raise SyntaxError i
in
    Parser.parse(0, lexstream, print_error, ())
end

fun parse (fileName, errStream) = let
    val _ = Errors.initErrors ()
    val _ = Lex.UserDeclarations.initLexer ()
    val f = TextIO.openIn fileName
    fun read n = if TextIO.endOfStream f then "" else TextIO.inputN (f, n)
    val lexer = Parser.makeLexer read		 
    val (result, lexer) = invoke lexer
    val (nextToken, lexer) = Parser.Stream.get lexer
    val _ = DveSemAnalyzer.checkSystem result
    val _ = Errors.raiseSemErrorsIfAny ()
    val _ = TextIO.closeIn f
in
    SOME result
end
    handle SyntaxError lineNo =>
	   (Errors.outputErrorMsg
		(errStream, fileName, SOME lineNo, "syntax error");
	    NONE)
	 | Errors.LexError (lineNo, msg) =>
	   (Errors.outputErrorMsg
		(errStream, fileName, SOME lineNo, msg);
	    NONE)
	 | Errors.ParseError (lineNo, msg) =>
	   (Errors.outputErrorMsg
		(errStream, fileName, SOME lineNo, msg);
	    NONE)
	 | Errors.SemError l =>
	   let
	       fun printError (pos, msg) =
		   Errors.outputErrorMsg
		       (errStream, fileName, SOME pos, msg)
	   in
	       (List.app printError l;
		NONE)
	   end
	 | IO.Io _ =>
	   (Errors.outputErrorMsg
		(errStream, fileName, NONE, "could not open file");
	    NONE)
	   

fun parse' fileName = parse (fileName, SOME TextIO.stdOut)	       
	       
fun parseTests dir = let
    val stream = OS.FileSys.openDir dir
    val file = ref (SOME "")
in
    while (file := OS.FileSys.readDir stream; !file <> NONE) do
	case !file
	 of NONE => ()
	  | SOME f => let val full = OS.Path.concat (dir, f)
		      in
			  TextIO.print ("(*" ^ full ^ "*)\n");
			  parse (full, SOME TextIO.stdOut);
			  ()
		      end
end

end
