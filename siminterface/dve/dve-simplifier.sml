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
 *     dve-simplifier.sml
 *
 *  Created:
 *     Nov. 17, 2008
 *
 *  Description:
 *     Some simple static analysis procedures to symplify dve systems.
 *)


structure DveSimplifier: sig

    val simplify: System.system -> System.system

end = struct

open Expr
open Stat

fun evalExpr (INT (_, n)) = SOME (LargeInt.toInt n)
  | evalExpr (BOOL_CONST (_, false)) = SOME 0
  | evalExpr (BOOL_CONST (_, true)) = SOME 1
  | evalExpr (ARRAY_INIT _) = NONE
  | evalExpr (PROCESS_STATE _) = NONE
  | evalExpr (VAR_REF _) = NONE
  | evalExpr (PROCESS_VAR_REF _) = NONE
  | evalExpr (BIN_OP (_, left, binOp, right)) =
    (case (evalExpr left, evalExpr right)
      of (SOME l, SOME r) =>		    
	 ((case binOp of
	       PLUS    => SOME (l + r)
	     | MINUS   => SOME (l - r)
	     | TIMES   => SOME (l * r)
	     | DIV     => SOME (l div r)
	     | MOD     => SOME (l mod r)
	     | EQ      => if l = r  then SOME 1 else SOME 0
	     | NEQ     => if l <> r then SOME 1 else SOME 0
	     | SUP     => if l > r  then SOME 1 else SOME 0
	     | SUP_EQ  => if l >= r then SOME 1 else SOME 0
	     | INF     => if l < r  then SOME 1 else SOME 0
	     | INF_EQ  => if l <= r then SOME 1 else SOME 0
	     | AND     => if l <> 0 andalso r <> 0 then SOME 1 else SOME 0
	     | OR      => if l <> 0 orelse r <> 0 then SOME 1 else SOME 0
	     | IMPLY   => if l = 0 orelse r <> 0 then SOME 1 else SOME 0
	     | LSHIFT  => NONE
	     | RSHIFT  => NONE
	     | AND_BIT => NONE
	     | OR_BIT  => NONE
	     | XOR     => NONE)
	  handle _ => NONE)
       | _ => NONE)
  | evalExpr (UN_OP (_, unOp, right)) =
    case evalExpr right
     of SOME r =>
	((case unOp
	   of NOT    => if r = 0 then SOME 1 else SOME 0
	    | UMINUS => SOME (~ r)
	    | NEG    => NONE)
	 handle _ => NONE)
      | _ => NONE

fun simplifyExpr (e as (INT _)) = e
  | simplifyExpr (e as (BOOL_CONST _)) = e
  | simplifyExpr (e as (PROCESS_STATE _)) = e
  | simplifyExpr (e as (ARRAY_INIT _)) = e
  | simplifyExpr (e as (BIN_OP (pos, left, binOp, right))) =
    (case evalExpr e
      of SOME i => INT (pos, LargeInt.fromInt i)
       | NONE   => let
	     val ls = simplifyExpr left
	     val rs = simplifyExpr right
	 in
	     case (ls, binOp, rs)
	      of (BOOL_CONST (_, false), AND, _) => ls
	       | (BOOL_CONST (_, true), AND, _) => rs
	       | (INT (_, 0), AND, _) => ls
	       | (INT (_, _), AND, _) => rs
	       | (BOOL_CONST (_, true), OR, _) => ls
	       | (BOOL_CONST (_, false), OR, _) => rs
	       | (INT (_, 0), OR, _) => rs
	       | (INT (_, _), OR, _) => ls
	       | (_, AND, BOOL_CONST (_, false)) => rs
	       | (_, AND, BOOL_CONST (_, true)) => ls
	       | (_, AND, INT (_, 0)) => rs
	       | (_, AND, INT (_, _)) => ls
	       | (_, OR, BOOL_CONST (_, true)) => rs
	       | (_, OR, BOOL_CONST (_, false)) => ls
	       | (_, OR, INT (_, 0)) => ls
	       | (_, OR, INT (_, _)) => rs
	       | _ => BIN_OP (pos, ls, binOp, rs)
	 end)
  | simplifyExpr (e as (UN_OP (pos, unOp, right))) =
    (case evalExpr e of
	 SOME i => INT (pos, LargeInt.fromInt i)
       | NONE   => UN_OP (pos, unOp, simplifyExpr right))
  | simplifyExpr (e as (VAR_REF (pos, v))) = VAR_REF (pos, simplifyVarRef v)
  | simplifyExpr (e as (PROCESS_VAR_REF (pos, proc, v))) =
    PROCESS_VAR_REF (pos, proc, simplifyVarRef v)
and simplifyVarRef (v as (SIMPLE_VAR _)) = v
  | simplifyVarRef (v as (ARRAY_ITEM (var, index))) =
    ARRAY_ITEM (var, simplifyExpr index)
    
fun simplifyStat (ASSIGN (p, var, assign)) =
    ASSIGN (p, var, simplifyExpr assign)

fun simplifyTrans { pos, id, src, dest, guard, sync, effect } = let
    val guardEval = case guard of NONE => SOME 1
				| SOME guard => evalExpr guard
    val (newGuard, keep) =
	case guardEval
	 of NONE   => (SOME (simplifyExpr (valOf guard)), true)
	  | SOME 0 => (NONE, false)
	  | SOME _ => (NONE, true)
in
    if keep
    then SOME { pos    = pos,
		id     = id,
		src    = src,
		dest   = dest,
		guard  = newGuard,
		sync   = sync,
		effect = List.map simplifyStat effect }
    else NONE
end

fun simplifyProcess { pos, name, vars, states, init, accept, trans } =
    { pos    = pos,
      name   = name,
      vars   = vars,
      states = states,
      init   = init,
      trans  = List.mapPartial simplifyTrans trans,
      accept = accept }
    
fun simplify ({ t, glob, chans, procs, prop, progs, prog }: System.system) =
    { t     = t,
      glob  = glob,
      chans = chans,
      procs = List.map simplifyProcess procs,
      prop  = prop,
      progs = progs,
      prog  = prog }
		 
end
