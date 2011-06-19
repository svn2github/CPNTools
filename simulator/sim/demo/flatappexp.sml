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
(* File: flatappexp.sml
 *
 * Demonstration of how to expand the FlatAppExp AST token by means of
 * the Precedence.parse facility of the SML/NJ compiler. Used for transforming
 * functions/constructors in inline notation to prefix.
 *)

infix 1 @+;
fun (ms:int) @+ (time:real) = [(ms,time)];
val ignore = 0.0;
type Int = int;
type Bool = bool;
type Prod = Int * Bool;
(* type No = int;
val Server = 1;
type Mess = string;
datatype Id = App of No | Server;
*)
val myval = (2,{AA=4,BB=4});
type MYTYPE = int * {AA:int,BB:int};

local (* Compile.elaborate example *)
  exception OOPS of string;
  val str_old = "structure CPN'x = struct val _ = fn (adr: Id,id: Id,id2: Id,disp: Mess,server: Id,mess: Mess,location: Id) => ([id=id2]): bool list; end;";
  val str_1 = "structure CPN'x = struct val (x,x) = myval end;";
  val str_2 = "structure CPN'x = struct val _ = fn a1 as (_,{AA=aa,...}) => myval:MYTYPE; end;";
  val str ="structure CPN'x = struct val _ = fn (pp: Prod,bb: Bool,x: Int,y: Int,z: Int,ii: Int) => (1`pp@+4): (Prod) CPN'TMS.tms; end;";

  val source
    = Compiler.Source.newSource
        ("", 1, TextIO.openString str, 
         false, {consumer = print, flush = fn () => (), linewidth = 80});

  val _ = print "---PARSING---\n";
  val ast = Compiler.TopCompile.parse source;

  val compInfo
    = Compiler.TopCompile.mkCompInfo
        (source, 
         #get Compiler.EnvRef.core (),
         fn any => any);
  val baseEnvRefunSC = Compiler.EnvRef.unSC (Compiler.EnvRef.pervasive);
  fun checkErrors s 
    = if Compiler.TopCompile.anyErrors compInfo then 
        raise OOPS s 
      else ();
  val {static=statenv, ...}
    = Compiler.BareEnvironment.layerEnv
         (#get (Compiler.EnvRef.topLevel) (), #get baseEnvRefunSC ());
(*
  val _ = print "---ELABORATING---\n";
  val _ = Compiler.TopCompile.elaborate{ast=ast,compenv=statenv,compInfo=compInfo}
          before checkErrors "Elaborate failure";
*)
in
  val _ = Compiler.Source.closeSource source;
  val myast = ast
end;

(*
structure CPN'x = struct val _ = fn (resstat: int) => ((resstat@ignore)): int; end;
*)
exception InternalError of string;
local
  open Compiler.Ast;
	fun unpack_exp (SeqDec 
			[MarkDec 
			 (StrDec 
			  [MarkStrb 
			   (Strb 
			    {constraint= NoSig,
			     def= MarkStr 
			     (BaseStr    (* OBS:KHM was StructStr *)
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
					pat=_}, (_,_) )],_)) 
				, (_,_) )]) , (_,_) ),name}, (_,_) )], (_,_) )]) =  e
	  | unpack_exp _ = raise InternalError "unpackexp\n";
in
  val unpack = unpack_exp
end;

fun flatappexp (Compiler.Ast.FlatAppExp items) =
    let
	val env = Compiler.BareEnvironment.staticPart(Compiler.EnvRef.combined())
	val error = fn region =>
	    (Compiler.ErrorMsg.errorNoFile
	     ({ consumer= fn s => (print "synchk:parse_exp:FATAL: Error consumer was called"), 
	       flush= fn () => (),
	       linewidth = 80},
	      ref false)
	     region);
    in
	Precedence.parse 
	{apply=fn(f,a) => Compiler.Ast.AppExp{function=f,argument=a},
	 pair =fn(a,b) => Compiler.Ast.TupleExp[a,b]}
	(items,env,error)
    end
  | flatappexp _ = raise InternalError "error";

(*
val exp1 = flatappexp(unpack myast);
val exp2 = case exp1 of (Compiler.Ast.MarkExp (exp,_)) => exp;
val exp3 = flatappexp(case exp2 of (Compiler.Ast.SeqExp [exp]) => exp);
val exp4 = case exp3 of (Compiler.Ast.MarkExp (exp,_)) => exp;
val exp5 = flatappexp(case exp4 of (Compiler.Ast.ListExp [exp]) => exp);
val {argument=exp6,function=exp7} = case exp5 of (Compiler.Ast.AppExp exp) => exp;
val exp8 = case exp7 of (Compiler.Ast.MarkExp(Compiler.Ast.VarExp exp,_)) => exp;
*)
