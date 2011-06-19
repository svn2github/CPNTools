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
 *     dve-sem-analyzer.sml
 *
 *  Created:
 *     Nov. 13, 2007
 *
 *  Description:
 *     Semantical analyzer for a dve system.
 *)


structure DveSemAnalyzer: sig

val checkSystem:
    System.system
    -> unit
       
end = struct

fun checkNoRedefinition getName getPos kind l = let
    fun checkNoRedefinition' (prev, []) = ()
      | checkNoRedefinition' (prev, item :: next) = let
	    fun isItem item' = ((getName item') = (getName item))
	in
	    if List.exists isItem prev
	    then Errors.addError
		     (getPos item,
		      "redefinition of " ^ kind ^ " " ^ getName item)
	    else ();
	    checkNoRedefinition' (prev @ [item], next)
	end
in
    checkNoRedefinition' ([], l)
end

val checkNoVarRedefinition =
    checkNoRedefinition	Var.getName Var.getPos "variable"

val checkNoProcessRedefinition =
    checkNoRedefinition	Process.getName Process.getPos "process"

val checkNoStateRedefinition =
    checkNoRedefinition	State.getName State.getPos "state"

val checkNoProgressRedefinition =
    checkNoRedefinition	Progress.getName Progress.getPos "progress"

fun checkExpr (e, visible as (vars, chans, procs)) = let
    fun checkVar (Expr.PROCESS_STATE (pos, proc, state)) =
	(case Process.isProcess (procs, proc)
	  of NONE =>
	     Errors.addError (pos, "undefined process: " ^ proc)
	   | SOME procDef =>
	     case State.getState (Process.getStates procDef, state) of
		 NONE =>
		 Errors.addError (pos, "process " ^ proc ^
				       " has no state " ^ state)
	       | SOME _ => ())
      | checkVar (Expr.VAR_REF (pos, var)) = let
	    val name = Expr.getVarName var
	in
	    case Var.getVar (vars, name)
	     of NONE =>	Errors.addError (pos, "undefined variable: " ^ name)
	      | SOME varDef =>
		case (Var.getTyp varDef, var)
		 of (Typ.ARRAY_TYPE _, Expr.SIMPLE_VAR _) =>
		    Errors.addError
			(pos, "index expected for array " ^ name)
		  | _ => ()
	end
      | checkVar (Expr.PROCESS_VAR_REF (pos, proc, var)) =
	(case Process.isProcess (procs, proc)
	  of NONE =>
	     Errors.addError (pos, "undefined process: " ^ proc)
	   | SOME _ => let
		 val v = Expr.getVarName var
		 val p = Process.getProcess (procs, proc)
		 val vars = Process.getVars p
	     in
		 case Var.getVar (vars, v)
		  of NONE => Errors.addError (pos, "undefined variable: " ^ v)
		   | SOME varDef =>
		     case (Var.getTyp varDef, var)
		      of (Typ.ARRAY_TYPE _, Expr.SIMPLE_VAR _) =>
			 Errors.addError
			     (pos, "index expected for array " ^ v)
		       | _ => ()
	     end)
      | checkVar _ = ()
in
    Expr.app checkVar e
end

fun checkVar (var, visible) =
    if (Var.getConst var) andalso (Var.getInit var = NONE)
    then Errors.addError (Var.getPos var,
			  "initial value expected for constant " ^
			  Var.getName var)
    else case Var.getInit var
	  of NONE => ()
	   | SOME e => checkExpr (e, visible)

fun checkVarList (vars, visible) = let
    fun checkVarList ([], _) = ()
      | checkVarList (var :: vars, visible as (visibleVars, p, c)) =
	(checkVar (var, visible);
	 checkVarList (vars, (visibleVars @ [var], p, c)))
in
    checkNoVarRedefinition vars;
    checkVarList (vars, visible)
end

fun checkStat (s, visible) =
    case s
     of Stat.ASSIGN (pos, var, assigned) =>
	(checkExpr (Expr.VAR_REF (pos, var), visible);
	 checkExpr (assigned, visible))

fun checkStatList (l, visible) =
    List.app (fn s => checkStat (s, visible)) l

fun checkSync (s, visible as (vars, chans, procs)) = let
    val pos = Sync.getPos s
    val chanName = Sync.getChan s
    val data = Sync.getData s
    val typ = Sync.getTyp s
    val chanDef = Channel.getChannel (chans, chanName)
in
    case chanDef
     of NONE => Errors.addError (pos, "undefined channel: " ^ chanName)
      | SOME d => 
	case data
	 of NONE => ()
	  | SOME e =>
	    (checkExpr (e, visible);
	     if typ = Sync.SEND
	     then ()
	     else case e
		   of Expr.VAR_REF _ => ()
		    | _ =>
		      Errors.addError
			  (pos,
			   "invalid lvalue in receive on channel " ^ chanName))
end

fun checkTrans (trans, proc, visible) = let
    val pos  = Trans.getPos trans
    val src  = State.getState (Process.getStates proc, Trans.getSrc  trans)
    val dest = State.getState (Process.getStates proc, Trans.getDest trans)
    fun checkState s name =
	case s
	 of SOME _ => ()
	  | NONE   => Errors.addError (pos, "process " ^ Process.getName proc ^
					    " has no state " ^ name)
in
    checkState src (Trans.getSrc trans);
    checkState dest (Trans.getDest trans);
    case Trans.getGuard trans
     of NONE   => ()
      | SOME e => checkExpr (e, visible);
    case Trans.getSync trans
     of NONE   => ()
      | SOME s => checkSync (s, visible);
     checkStatList (Trans.getEffect trans, visible)
end

fun checkTransList (l, proc, visible) =
    List.app (fn t => checkTrans (t, proc, visible)) l

fun checkProcess (proc, visible as (vars, chans, procs)) = let
    val states = Process.getStates proc
    val init = Process.getInit proc
    val procPos =  Process.getPos proc
    val accept = Process.getAccept proc
    val initName = State.getName init
    val initDef = State.getState (states, initName)
    val acceptDefs =
	List.map (fn s => (s, State.getState (states, State.getName s))) accept
in
    checkVarList (Process.getVars proc, visible);
    checkNoStateRedefinition (Process.getStates proc);
    case initDef
     of NONE => Errors.addError (State.getPos init,
				 initName ^ " is not a state of process " ^
				 Process.getName proc)
      | SOME _ => ();
    List.app (fn (s, SOME _) => ()
	       | (s, NONE) => Errors.addError
				  (State.getPos s,
				   State.getName s ^
				   " is not a state of process " ^
				   Process.getName proc)) acceptDefs;
    checkTransList (Process.getTrans proc, proc, 
		       (vars @ Process.getVars proc, chans, procs))
end

fun checkProcessList (procs, visible) =
    (checkNoProcessRedefinition procs;
     List.app (fn proc => checkProcess (proc, visible)) procs)

fun checkProperty (procs, NONE) = ()
  | checkProperty (procs, SOME (pos, proc)) =
    if not (List.exists (fn p => Process.getName p = proc) procs)
    then Errors.addError (pos, proc ^ " is not a process")
    else ()

fun checkProgressList (progs, visible) =
    (checkNoProgressRedefinition progs;
     List.app (fn prog => checkExpr (Progress.getMap prog, visible))
	      progs)

fun checkProgress (progs, NONE) = ()
  | checkProgress (progs, SOME (pos, prog)) =
    if not (List.exists (fn p => Progress.getName p = prog) progs)
    then Errors.addError (pos,
			  prog ^ " is not a progress measure of the system")
    else ()

fun checkSystem ({ t, glob, chans, procs,
		   prop, progs, prog, ... }: System.system) =
    (checkVarList (glob, ([], chans, procs));
     checkProcessList (procs, (glob, chans, procs));
     checkProperty (procs, prop);
     checkProgressList (progs, (glob, chans, procs));
     checkProgress (progs, prog))

end
