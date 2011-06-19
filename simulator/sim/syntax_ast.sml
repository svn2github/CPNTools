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
(* File: syntax_ast.sml
 *
 * Abstract syntax tree handling.
 * Depends on SML/NJ internal compiler structures.
 *)

structure CPN'AstLib:
    sig
        type exp = Compiler.Ast.exp;
        type dec = Compiler.Ast.dec;
        type pat = Compiler.Ast.pat;

	exception SyntaxError of string;		

	val unpack_exp : dec -> exp
	val unpack_pat : dec -> pat
	val unpack_channel : exp -> exp;
	val syntax_type_check : string list -> dec;
	val syntax_check : string -> unit;
    end =
    struct

	open Compiler.Ast;

	exception SyntaxError = CPN'SyntaxDatatypes.SyntaxError;
      exception Compile of string

	val err = ref "";

	fun exp_handler (Compile s) =
	    raise SyntaxError (concat[!err," ",s,"\n"])
	  | exp_handler (CPN'Error s) =
	    raise SyntaxError s
	  | exp_handler (SyntaxError s) =
	    raise SyntaxError s
	  | exp_handler (InternalError s) =
	    raise SyntaxError
		(concat["Error: exception InternalError is raised with: ",s])
	  | exp_handler exn = 
	    raise SyntaxError 
		(concat["Error: exception ",exnName exn," is raised"]);

	fun unpack_exp 
			(MarkDec 
			 (StrDec 
			  [MarkStrb 
			   (Strb 
			    {constraint= NoSig,
			     def= MarkStr 
			     (BaseStr
			      (SeqDec 
			       [MarkDec 
				((ValDec 
				  ([MarkVb 
				   (Vb {exp = 
					MarkExp 
					(FnExp 
					 [Rule 
					  {exp = 
					   MarkExp 
					   (ConstraintExp 
					    {constraint,expr=e}, (_,_) ),
					   pat=_}], (_,_) ) ,
					pat=_, lazyp = _}, (_,_) )],_)) 
                              , (_,_) )]) , (_,_) ),name}, (_,_) )], (_,_))) =  e
        | unpack_exp (SeqDec [exp]) = unpack_exp exp
	  | unpack_exp _ = raise InternalError "unpackexp\n";

	fun unpack_pat
			(MarkDec 
			 (StrDec 
			  [MarkStrb 
			   (Strb 
			    {constraint= NoSig,
			     def= MarkStr 
			     (BaseStr
			      (SeqDec 
			       [MarkDec 
				((ValDec (
				  [MarkVb 
				   (Vb {exp = 
					MarkExp 
					(FnExp 
					 [Rule 
					  {exp = _,
					   pat=p}], (_,_) ) ,
					pat=_, lazyp = _}, (_,_) )],_)) 
				 , (_,_) )]) , (_,_) ),...}, (_,_) )], (_,_) )) = p
        | unpack_pat (SeqDec [exp]) = unpack_pat exp
	  | unpack_pat _ = raise InternalError "unpackpat\n";

	fun unpack_channel (FlatAppExp
		[{fixity=NONE,
		  item=MarkExp
		  	(SeqExp
				[FlatAppExp
					[{fixity=NONE,
					  item=MarkExp
					  	(VarExp _, _),...},
					 {fixity = f,
					  item = i, ...}
					]], _), ...}]) = i
(*	 | unpack_channel (FlatAppExp
		[{fixity=NONE,
		  item=MarkExp
		  	(SeqExp
				[FlatAppExp
					[{fixity=NONE,
					  item=MarkExp
					  	(VarExp [SYMBOL _, SYMBOL (_, "!")], _),...},
					 {fixity = f,
					  item = i, ...}
					]], _), ...}]) = i*)

	fun syntax_type_check str_list =
	    let
		val _ = err := "";

		val code_str = concat (str_list^^["; end;"]);
		val src = Compiler.Source.newSource
		    ("",0,
		     TextIO.openString 
		     code_str,false,
		     {consumer = fn s => (err := (!err)^s), 
		      flush = fn () => (), 
		      linewidth = fn () => 80})

		val theenvsc = Compiler.Environment.staticPart(Compiler.EnvRef.combined());

		(* syntax check: *)
	      val ast = Compiler.MLParser.parse src ()
            val ast = (case ast of
                           (Compiler.MLParser.PARSE result) => result
                         | Compiler.MLParser.ABORT => raise Compile "Aborted parsing"
                         | Compiler.MLParser.EOF => raise Compile "EOF while parsing"
                         | Compiler.MLParser.ERROR => raise Compile "Error parsing")
  		    handle e => exp_handler e;
			    
		(* type check: *)
            val compInfo = Compiler.Compile.mkCompInfo {source = src,
            transform = fn any => any}

              val baseEnvRefunSC = Compiler.EnvRef.pervasive;
		fun checkErrors s = 
		    if Compiler.CompInfo.anyErrors compInfo then raise Compile s else ()

		val _ = ((Compiler.TopCompile.elaborate
			 {ast= ast,
			  statenv = theenvsc,
			  compInfo= compInfo, guid = ()}) before checkErrors "Elaborate failure")
			 handle e => exp_handler e

                val _ = Compiler.Source.closeSource src;
	    in
		ast
	    end; (* syntax_type_check *)

	fun syntax_check str =
	    let (* perform syntax check (parse) only *)

		val _ = err := "";
		val code_str = str^"; end;";

		val src = Compiler.Source.newSource
		    ("",0,
		     TextIO.openString 
		     code_str,false,
		     {consumer = fn s => (err := (!err)^s), 
		      flush = fn () => (), 
		      linewidth = fn () => 80})

	      val ast = Compiler.MLParser.parse src ()
            val _ = case ast of
                           (Compiler.MLParser.PARSE result) => result
                         | Compiler.MLParser.ABORT => raise Compile "Aborted parsing"
                         | Compiler.MLParser.EOF => raise Compile "EOF while parsing"
                         | Compiler.MLParser.ERROR => raise Compile "Error parsing"
  		    handle e => exp_handler e;

                val _ = Compiler.Source.closeSource src;
	    in
		()
	    end;
    end; (* CPN'astLib *)
