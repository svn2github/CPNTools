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
 *     dve-compiler-utils.sml
 *
 *  Created:
 *     Nov. 17, 2008
 *
 *  Description:
 *    Some useful functions used everywhere by the compiler.
 *)


structure DveCompilerUtils = struct

(*
 * SYNC  = event that corresponds to a synchronous exchange between 2 proc
 * LOCAL = event that corresponds to the execution of single process
 *)
datatype event =
	 LOCAL of
	 int *         (*  id of the transition  *)
	 string *      (*  process name  *)
	 Trans.trans   (*  transition  *)
       | SYNC of
	 int *          (*  id of the sending process transition  *)
	 int *          (*  id of the receiving process transition  *)
	 string *       (*  sending process name  *)
	 Trans.trans *  (*  sending process transition  *)
	 string *       (*  receiving process name  *)
	 Trans.trans    (*  receiving process transition  *)

(*  component of the state type  *)
datatype state_comp =
	 GLOBAL_VAR of Var.var
       | LOCAL_VAR of string * Var.var
       | PROCESS_STATE of string

datatype large_state_comp =
	 PROCESS of string * state_comp list
       | GLOBAL of state_comp list

type mapping = (state_comp * string) list

val baseType = "int"

val listFormat = ListFormat.fmt

fun baseToInt ex = ex

fun intToBase ex = ex

fun typeName (Typ.BASIC_TYPE _) = "base_type"
  | typeName (Typ.ARRAY_TYPE (_, n)) = "base_type_array" ^ (Int.toString n)

fun arrayGetFunc t = typeName t ^ "_get"

fun arraySetFunc t = typeName t ^ "_set"

fun arrayToStringFunc t = typeName t ^ "_toString"

fun arrayToSMLStringFunc t = typeName t ^ "_toSMLString"

fun getLocalStateType (proc) =
    "P_" ^ (Utils.toLower proc) ^ "_STATE_TYPE"

fun getLocalStateToInt (proc) =
    getLocalStateType proc ^ "_ToInt"

fun getIntToLocalState (proc) =
    "intTo_" ^ getLocalStateType proc

fun getLocalStateName (proc, state) =
    "P_" ^ (Utils.toLower proc) ^ "_STATE_" ^ (Utils.toLower state)

fun getLocalStateToString (proc) = getLocalStateType proc ^ "_ToString"

fun getLocalStateToSMLString (proc) = getLocalStateType proc ^ "_ToSMLString"

fun getCompVar (GLOBAL_VAR v) = SOME v
  | getCompVar (LOCAL_VAR (_, v)) = SOME v	
  | getCompVar (PROCESS_STATE _) = NONE

fun getCompName (GLOBAL_VAR v) = "GV_" ^ (Utils.toLower (Var.getName v))
  | getCompName (LOCAL_VAR (p, v)) = "LV_" ^ (Utils.toLower (p)) ^ "_V_" ^
				     (Utils.toLower (Var.getName v))
  | getCompName (PROCESS_STATE p) = "P_" ^ (Utils.toLower p) ^ "_S"

fun getCompTypeName (GLOBAL_VAR def) = typeName (Var.getTyp def)
  | getCompTypeName (LOCAL_VAR (_, def)) = typeName (Var.getTyp def)
  | getCompTypeName (PROCESS_STATE proc) = getLocalStateType proc

fun getCompDescription (GLOBAL_VAR v) = Var.getName v
  | getCompDescription (LOCAL_VAR (p, v)) = p ^ "->" ^ (Var.getName v)
  | getCompDescription (PROCESS_STATE p) = "process " ^ (Utils.toLower p)

fun getCompToStringFuncName (PROCESS_STATE p) = getLocalStateToString p
  | getCompToStringFuncName comp = let
	val var = valOf (getCompVar comp)
	val t = Var.getTyp var
    in
	case t of Typ.ARRAY_TYPE _ => arrayToStringFunc t
		| Typ.BASIC_TYPE _ => "baseToString"
    end

fun getCompToSMLStringFuncName (PROCESS_STATE p) = getLocalStateToSMLString p
  | getCompToSMLStringFuncName comp = let
	val var = valOf (getCompVar comp)
	val t = Var.getTyp var
    in
	case t of Typ.ARRAY_TYPE _ => arrayToSMLStringFunc t
		| Typ.BASIC_TYPE _ => "baseToString"
    end

fun getLargeCompName (GLOBAL _) = "GLOBAL"
  | getLargeCompName (PROCESS (p, _)) = "PROCESS_" ^ p

fun isCompConst comp =
    case getCompVar comp of NONE => false | SOME var => Var.getConst var

fun getComp (comp, st) =
    if isCompConst comp
    then "(!" ^ (getCompName comp) ^ ")"
    else "(#" ^ (getCompName comp) ^ " " ^ st ^ ")"

fun getEventName (LOCAL (id, proc, tr)) =
    String.concat [
    "LOC_", Int.toString id,
    "_P_", Utils.toLower proc,
    "_T_", Utils.toLower (Trans.getSrc tr),
    "_TO_", Utils.toLower (Trans.getDest tr) ]
  | getEventName (SYNC (id1, id2, proc1, tr1, proc2, tr2)) =
    String.concat [
    "SYN_", Int.toString id1, "_", Int.toString id2,
    "_P_", Utils.toLower proc1,
    "_T_", Utils.toLower (Trans.getSrc tr1),
    "_TO_", Utils.toLower (Trans.getDest tr1),
    "_P_", Utils.toLower proc2,
    "_T_", Utils.toLower (Trans.getSrc tr2),
    "_TO_", Utils.toLower (Trans.getDest tr2) ]

fun getEventName' (events, t) =
    case List.find (fn LOCAL (id', _, _) => Trans.getId t = id'
		     | SYNC  (id', id'', _, _, _, _) =>
		       Trans.getId t = id' orelse
		       Trans.getId t = id'')
		   events of
	NONE   => NONE
      | SOME e => SOME (getEventName e)

fun getInitStateName (proc) =
    "INIT_P_" ^ (Utils.toLower proc)

fun getImage (mapping: mapping,
	      comp   : state_comp) =
    case List.find (fn (c, _) => comp = c) mapping of
	NONE          => raise Errors.InternalError
      | SOME (_, img) => img

fun getVarImage (mapping: mapping,
		 proc   : string option,
		 varName: string) = let
    fun isLocalVar (LOCAL_VAR (p, var), _) =
	(Var.getName var) = varName andalso SOME p = proc
      | isLocalVar _ = false
    fun isGlobalVar (GLOBAL_VAR var, _) = (Var.getName var) = varName
      | isGlobalVar _ = false
in
    (*  first look in local variables that hide global ones  *)
    case List.find isLocalVar mapping
     of NONE => (case List.find isGlobalVar mapping
		  of NONE => raise Errors.InternalError
		   | SOME (_, img) => img)
      | SOME (_, img) => img
end

fun getProcessStateImage (mapping: mapping,
			  proc   : string) = let
    fun isProcessState (PROCESS_STATE p, _) = proc = p
      | isProcessState _                    = false
in
    case List.find isProcessState mapping of
	NONE          => raise Errors.InternalError
      | SOME (_, img) => img
end

fun buildStateComps (s: System.system) = let
    fun mapProcessState proc = PROCESS_STATE (Process.getName proc)
    fun mapGlobalVar v = GLOBAL_VAR v
    fun getProcessVarList proc = let
	fun mapVar var = LOCAL_VAR(Process.getName proc, var)
    in
	List.map mapVar (Process.getVars proc)
    end
in
    List.map mapGlobalVar (System.getVars s) @
    List.concat (List.map getProcessVarList (System.getProcs s)) @
    List.map mapProcessState (System.getProcs s)
end

fun buildLargeStateComps sys = let
    val comps = List.filter (fn c => not (isCompConst c)) (buildStateComps sys)
in
    (GLOBAL (List.mapPartial
		 (fn GLOBAL_VAR v => SOME (GLOBAL_VAR v) | _ => NONE) comps))
    ::
    List.map (fn p => PROCESS (p, List.filter (fn LOCAL_VAR (q, _) => p = q
						| PROCESS_STATE q => p = q
						| _ => false)
					      comps))
	     (List.map Process.getName (System.getProcs sys))
end

fun getSubComps (GLOBAL c) = c
  | getSubComps (PROCESS (_, c)) = c

local
fun build procs (s as { prop, ... }: System.system) = let
    fun getLocalEventsProcess proc = let
	fun map tr =
	    if isSome (Trans.getSync tr)
	       andalso Sync.getMode (valOf (Trans.getSync tr)) = Sync.SYNC
	    then NONE
	    else SOME (LOCAL (Trans.getId tr, Process.getName proc, tr))
    in
	List.mapPartial map (Process.getTrans proc)
    end
    val localEvents = List.concat (List.map getLocalEventsProcess procs)
    fun getSyncEventsProcess proc = let
	fun map tr =
	    case Trans.getSync tr
	     of	NONE => NONE
	      | SOME s =>
		if (Sync.getMode s) <> Sync.SYNC
		then NONE
		else SOME (Sync.getTyp s, Sync.getChan s,
			   Process.getName proc, tr)
    in
	List.mapPartial map (Process.getTrans proc)
    end
    fun match ((stype, schan, sproc, _), (rtype, rchan, rproc, _)) =
	(stype, rtype) = (Sync.SEND, Sync.RECV)
	andalso (schan = rchan)
	andalso (sproc <> rproc)
    fun map ((_, _, sproc, str), (_, _, rproc, rtr)) =
	SYNC (Trans.getId str, Trans.getId rtr, sproc, str, rproc, rtr)
    val syncTrans = List.concat (List.map getSyncEventsProcess procs)
    val syncEvents = List.map map (Utils.zipPartial match syncTrans)
in
    localEvents @ syncEvents
end
in
fun buildEvents (s as { prop, ... }: System.system) = let
    val procs = case prop
		 of NONE => System.getProcs s
		  | SOME (_, p) => List.filter
				       (fn proc => Process.getName proc <> p)
				       (System.getProcs s)
in
    build procs s
end
fun buildPropertyEvents (s as { prop, ... }: System.system) = let
    val procs = case prop
		 of NONE => []
		  | SOME (_, p) => List.filter
				       (fn proc => Process.getName proc = p)
				       (System.getProcs s)
in
    build procs s
end
end

fun getGlobalVars comps = let
    fun isGlobalVar c = case c of GLOBAL_VAR _ => true | _ => false
in
    List.filter isGlobalVar comps
end

fun getLocalVars (comps, proc) = let
    fun isLocalVar c = case c of LOCAL_VAR (p, _) => p = proc | _ => false
in
    List.filter isLocalVar comps
end

fun getVarComp (comps: state_comp list,
		proc : string option,
		var  : string) = let
    fun isLocalVar (LOCAL_VAR (p, v)) = (Var.getName v, valOf proc) = (var, p)
      | isLocalVar _                  = false
    fun isGlobalVar (GLOBAL_VAR v)    = Var.getName v = var
      | isGlobalVar _                 = false
in
    if isSome proc
    then case List.find isLocalVar comps of
	     NONE   => (case List.find isGlobalVar comps
			 of NONE => raise Errors.InternalError |
			    SOME v => v)
	   | SOME v => v
    else case List.find isGlobalVar comps
	  of NONE => raise Errors.InternalError |
	     SOME v => v
end

fun buildMapping comps =
    List.map (fn comp => (comp,
			  if not (isCompConst comp)
			  then getCompName comp
			  else "(!" ^ (getCompName comp) ^ ")")) comps

fun genComps comps =
    Utils.fmt { init  = "{ ",
		final = " }",
		fmt   = (fn c => if isCompConst c
				 then NONE
				 else SOME (getCompName c)),
		sep   = ", " } comps

fun genOneComp c = let
    fun oneVar c v =
	case Var.getTyp v of
	    Typ.ARRAY_TYPE (_, size) => let
		fun getIth i = getCompName c ^ "_ITEM_" ^ (Int.toString i)
		val indexes = List.tabulate (size, (fn i => i))
	    in
		(List.map getIth indexes,
		 Utils.fmt { init  = (getCompName c) ^ " = (",
			     final = ")",
			     fmt   = fn i => SOME (getIth i),
			     sep   = ", " } indexes)
	    end	    
	  | _ => ([ getCompName c ], getCompName c)
in
    case c of GLOBAL_VAR v => oneVar c v
	    | LOCAL_VAR (_, v) => oneVar c v
	    | PROCESS_STATE p =>
	      ([ "(" ^ getLocalStateToInt p ^ " " ^ getCompName c ^ ")"],
	       getCompName c)
end

fun genAllComps (comps, prefix) = let
    val l = ref []
    fun name c = case prefix of NONE => getCompName c
			      | SOME pref => pref ^ "_" ^ (getCompName c)
    fun oneVar c v =
	case Var.getTyp v of
	    Typ.ARRAY_TYPE (_, size) =>
	    SOME (Utils.fmt {
		  init  = (getCompName c) ^ " = (",
		  final = ")",
		  fmt   = fn i =>
			     let
				 val c = name c ^ "_ITEM_" ^ (Int.toString i)
			     in
				 l := c :: (!l);
				 SOME c
			     end,
		  sep   = ", " }
			    (List.tabulate (size, (fn i => i))))
	    
	  | _ => (l := (name c) :: (!l);
		  SOME ((getCompName c) ^ " = " ^ (name c)))
    fun oneComp c =
	if isCompConst c
	then NONE
	else case c of GLOBAL_VAR v => oneVar c v
		     | LOCAL_VAR (_, v) => oneVar c v
		     | PROCESS_STATE p => (
		       l := ("(" ^ (getLocalStateToInt p) ^ " " ^
			     (name c) ^ ")") ::
			    (!l);
		       SOME ((getCompName c) ^ " = " ^ (name c)))
    val comps = Utils.fmt { init  = "{ ",
			    final = " }",
			    fmt   = oneComp,
			    sep   = ", " } comps
in
    (List.rev (!l), comps)
end

fun mappingToState mapping = let
    fun mapComp (comp, value) =
	if isCompConst comp
	then NONE
	else SOME ((getCompName comp) ^ " =\n" ^ value)
in
    Utils.fmt {init  = "{\n",
	       sep   = ",\n",
	       final = "\n}",
	       fmt   = mapComp} mapping
end

fun updateMapping (mapping, comp, newValue) = let
    fun sameComp (c1, c2) =
	case (c1, c2)
	 of (GLOBAL_VAR var1, GLOBAL_VAR var2) =>
	    Var.getName var1 = Var.getName var2
	  | (LOCAL_VAR (proc1, var1), LOCAL_VAR (proc2, var2)) =>
	    proc1 = proc2 andalso Var.getName var1 = Var.getName var2
	  | (PROCESS_STATE (proc1), PROCESS_STATE (proc2)) =>
	    proc1 = proc2
	  | _ =>
	    false
in
    case mapping of
	[] => [(comp, newValue)]
      | (map as (comp', _)) :: mapping' =>
	if sameComp (comp, comp')
	then (comp, newValue) :: mapping'
	else map :: updateMapping (mapping', comp, newValue)
end

fun checkIndex checks comps proc pos var index = let
    val p = Int.toString pos
    val checkStr =
	" handle indexError => raise ModelError(" ^ p ^ ", \"index overflow\")"
in
    if not checks
    then ""
    else case index of
	     Expr.INT (_, num) => let
		 val comp = getVarComp (comps, proc, Expr.getVarName var)
		 val varDef = valOf (getCompVar comp)
		 val noCheck = case Var.getTyp varDef of
				   Typ.ARRAY_TYPE (_, size) =>
				   num < LargeInt.fromInt size
				 | _ => false
	     in
		 if noCheck
		 then ""
		 else checkStr
	     end
	   | _ => checkStr
end

fun compileInitVal (Typ.BASIC_TYPE _) = intToBase "0"
  | compileInitVal (Typ.ARRAY_TYPE (_, n)) =
    listFormat { init  = "(",
		 final = ")",
		 sep   = ", ",
		 fmt   = (fn s => s) } (Utils.constructList (intToBase "0", n))

fun compileExpr (e: Expr.expr)
		(context as (proc   : string option,
			     mapping: mapping,
			     comps  : state_comp list,
			     checks : bool)) = let
    fun compileVarRef (pos, proc, var) = let
	val v = getVarImage (mapping, proc, Expr.getVarName var)
	val p = Int.toString pos
    in
	case var
	 of Expr.ARRAY_ITEM (array, index) => let
		val comp = getVarComp (comps, proc, array)
		val t    = Var.getTyp (valOf (getCompVar comp))
	    in
		String.concat [
		arrayGetFunc t, " (", v, ", ",
		baseToInt compileExpr index context, ")",
		checkIndex checks comps proc pos var index ]
	    end
	  | Expr.SIMPLE_VAR (var) => v
    end
in
    "(" ^ (
    case e
      (*  int  *)
     of Expr.INT (_, num) => intToBase (LargeInt.toString num)
	
      (*  true  *)
      | Expr.BOOL_CONST (_, true) => "1"

      (*  false  *)
      | Expr.BOOL_CONST (_, false) => "0"
	      
      (*  array initializer  *)
      | Expr.ARRAY_INIT (_, exprs) => let
	    fun comp e = compileExpr e context
	in
	    listFormat {init  = "(",
			sep   = ", ",
			final = ")",
			fmt   = comp} exprs
	end
			      
      (*  state of a process  *)
      | Expr.PROCESS_STATE (_, proc, state) =>
	"if " ^ getProcessStateImage (mapping, proc) ^ " " ^
	" = " ^ getLocalStateName (proc, state) ^ " then 1 else 0"
			      
      (*  local variable of another process  *)
      | Expr.PROCESS_VAR_REF (pos, proc, var) =>
	compileVarRef (pos, SOME proc, var)
	
      (*  binary operations  *)
      | Expr.BIN_OP (pos, left, binOp, right) => let
	    val l = compileExpr left context
	    val r = compileExpr right context
	    val p = Int.toString pos

	    fun compileNumOp opStr =
		l ^ " " ^ opStr ^ " " ^ r ^
		(if checks
		 then " handle Overflow => raise ModelError (" ^ p ^
		      ", \"overflow\")"
		 else "")

	    fun compileDivOp opStr =
		l ^ " " ^ opStr ^ " " ^ r ^ 
		(if checks
		 then " handle " ^
		      "Div => raise ModelError (" ^ p ^
		      ", \"division by 0\") | " ^
		      "Overflow => raise ModelError (" ^ p ^
		      ", \"overflow\")"
		 else "")

	    fun compileCompOp opStr =
		"if " ^ l ^ " " ^ opStr ^ " " ^ r ^ " then 1 else 0"

	    fun compileBoolOp opStr =
		"if (" ^ l ^ " <> 0) " ^ opStr ^
		" (" ^ r ^ " <> 0) then 1 else 0"

	    fun compileBitOp opStr =
		"Word.toInt (Word." ^ opStr ^ " (" ^
		"Word.fromInt " ^ (baseToInt l) ^ ", " ^
		"Word.fromInt " ^ (baseToInt r) ^ "))"
	in
	    case binOp
	     of	Expr.PLUS    => compileNumOp  "+"
	      | Expr.MINUS   => compileNumOp  "-"
	      | Expr.TIMES   => compileNumOp  "*"
	      | Expr.DIV     => compileDivOp  "div"
	      | Expr.MOD     => compileDivOp  "mod"
	      | Expr.EQ      => compileCompOp "="
	      | Expr.NEQ     => compileCompOp "<>"
	      | Expr.SUP     => compileCompOp ">"
	      | Expr.SUP_EQ  => compileCompOp ">="
	      | Expr.INF     => compileCompOp "<"
	      | Expr.INF_EQ  => compileCompOp "<="
	      | Expr.AND     => compileBoolOp "andalso"
	      | Expr.OR      => compileBoolOp "orelse"
	      | Expr.LSHIFT  => compileBitOp  "<<"
	      | Expr.RSHIFT  => compileBitOp  ">>"
	      | Expr.AND_BIT => compileBitOp  "andb"
	      | Expr.OR_BIT  => compileBitOp  "orb"
	      | Expr.XOR     => compileBitOp  "xorb"
	      | any          =>
		raise Errors.CompilerError
			  (pos,
			   "unimplemented feature in compiler: operator " ^
			   Expr.binOpToString any)
	end
						 
      (*  unary operations  *)
      | Expr.UN_OP (pos, unOp, right) => let
	    val r = compileExpr right context
	    val p = Int.toString pos
	in
	    case unOp
	     of	Expr.NOT    =>
		"if " ^ r ^ " <> 0 then 0 else 1"
	      | Expr.UMINUS =>
		"~ " ^ r ^ 
		(if checks
		 then " handle " ^
		      "Overflow => raise ModelError (" ^ p ^ ", \"overflow\")"
		 else "")
	      | Expr.NEG    =>
		raise Errors.CompilerError
			  (pos,
			   "unimplemented feature in compiler: operator " ^
			   Expr.unOpToString Expr.NEG)
	end

      (*  variable reference  *)
      | Expr.VAR_REF (pos, var) => compileVarRef (pos, proc, var)) ^
    ")"
end

end
