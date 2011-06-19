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
(* File: syntax_tables.sml
 *
 * Tables used in the syntax checker.
 *)

structure CPN'SyntaxTables :
    sig
	exception HashNotFound;

	type var_item;
	type grp_item;

	datatype var_type = 
	    cpn  
	  | pseudo of string; (* the name of the cpn var *)

	val VarTable   : (string,var_item) CPN'StringTable.hash_table;
	val GroupTable : grp_item CPN'IntTable.hash_table;

	val init_tables : unit -> unit;
	val init_exp    : unit -> unit;

	(* insert funs: *)
	val insert_exp    : CPN'SyntaxDatatypes.exp -> unit;
	val insert_var    : string -> unit;
	val insert_grpvar : string * int * int -> string;

	(* pseudo-var funs: *)
	val list_pseudo_vars   : unit -> (string * int * int) list;
	val save_pseudo_vars   : unit -> unit;
	val delete_pseudo_vars : unit -> unit;
	val is_pseudo          : string -> bool;

	(* group funs: *)
	val no_of_groups         : unit -> int;
	val valid_group          : int -> bool;
        val list_groups          : unit ->  grp_item list;
	val list_unbound_grpvars : int -> string list;
	val list_group_vars      : int -> string list

	(* list all unbound vars: *)
	val list_unbound_vars : unit -> string list;


	(* set the given variable as bindable *)
	val set_bindable : string -> unit;

	    
	(* list information about all input places currently
	 * held in the tables: *)
	val list_input : unit -> {place: CPN'Id.id,
				  arcs: (CPN'Id.id * 
					 CPN'PlaceTable.exp_type) list,
				  no_of_tokens: int option} list;

	(* funs operating on the 'checked' table, holding both 
	 * place and transition ids: *)

	val init_checked   : CPN'Id.id list -> unit;
	val insert_checked : CPN'Id.id -> unit;
	val rm_checked     : CPN'Id.id -> unit;
	val list_checked   : unit -> CPN'Id.id list;
	val is_checked     : CPN'Id.id -> bool;
    end =  
struct

open CPN'SyntaxDatatypes;

    structure CPN'StringTable = HashTable
    structure CPN'IdTable = CPN'StringTable

    local
	fun eq (x: string, y) = (x = y)
    in
	val hashString = (HashString.hashString, eq)
    end

exception HashNotFound;
val InitHashLength = CPN'IntKey.hash_val;

datatype var_type = 
    cpn  
  | pseudo of string; (* the name of the cpn var *)

type var_item =
    {var        : string,   (* the variable name *)
     grpid      : int,      (* which group is this var in? *)
     varid      : int,      (* the number of the var _in the group_ *)
     bindable   : bool ref, (* is this var bindable? Changed during OGA *)
     var_type   : var_type, (* holds name of cpn-var in case of pseudo-var *)
     no_of_exps : int ref   (* can only use var for key-lookup if this is >1 *)
     };

(* a variable which isn't  in a group yet: *)

type grp_item =
    {grpid : int,           
     no_of_vars  : int ref,              (* how many vars in the group? *)
     arc_exps    : {place: CPN'Id.id,
		    arcs: (CPN'Id.id * CPN'PlaceTable.exp_type) list,
		    parsed_aexps: aexp,
		    no_of_tokens: int option} list,
     guard_items : {item_str: string, 
		    parsed_item: gitem} list,
     vars        : string list ref       (* to be able to effectively *)
     }; 				 (* look up all vars of a grp *)

(* table holding the variables placed in groups: *)

val VarTable: ((string,var_item) CPN'StringTable.hash_table) = 
    CPN'StringTable.mkTable hashString (InitHashLength,HashNotFound);

(* table holding variables not yet placed in a group: *)

val WaitingVarTable: ((string,var_type) CPN'StringTable.hash_table) = 
    CPN'StringTable.mkTable hashString (InitHashLength,HashNotFound);

(* a table holding variables of a 'sub-expression' (an exp which isn't 
 * dividable). Used for the management which is necessary because we allow
 * the use of multiple appearences of a variable in a pattern *)

val SubexpVarTable: ((string,string) CPN'StringTable.hash_table) = 
    CPN'StringTable.mkTable hashString (InitHashLength,HashNotFound);

(* table holding the expression groups: *)

val GroupTable:(grp_item CPN'IntTable.hash_table) =
    CPN'IntTable.mkTable(InitHashLength,HashNotFound);

val InputPlaceTable: ((string,int) CPN'IdTable.hash_table) = 
    CPN'IdTable.mkTable hashString (InitHashLength, HashNotFound);

(* the 'pseudo-vars' of a sub-expression. 
 * The pseudo-var name, the cpn-var name and a position of the cpn-var
 * in a corresponding expression-string.
 *)

val SubexpPseudovarTable : ((string,(string*int*int)) CPN'StringTable.hash_table) = 
    CPN'StringTable.mkTable hashString (InitHashLength,
			    InternalError "key not found in pseudo-var table");

val next_grp_id = ref 0; 
val cur_pseudo_no = ref 0; (* a number used when generating pseudo-var names *)

val cur_grp_id = ref NONE: int option ref; 

(* cur_grp_id is NONE until an identifier has been found for the group we're
 * working on. This happens when a previously seen variable is met. When the
 * treatment of an expression is finished and cur_grp_id is still NONE, we 
 * insert the exp in a new group of its own.
 *)

fun init_tables () =
    (next_grp_id := 0;
     cur_grp_id := NONE;
     cur_pseudo_no := 0;
     CPN'StringTable.filter (fn _ => false) SubexpPseudovarTable;
     CPN'StringTable.filter (fn _ => false) VarTable;
     CPN'StringTable.filter (fn _ => false) WaitingVarTable;
     CPN'StringTable.filter (fn _ => false) SubexpVarTable;
     CPN'IdTable.filter (fn _ => false) InputPlaceTable;
     CPN'IntTable.filter (fn _ => false) GroupTable
     );

fun init_exp () =
     (CPN'StringTable.filter (fn _ => false) WaitingVarTable;
      CPN'StringTable.filter (fn _ => false) SubexpVarTable;
      CPN'StringTable.filter (fn _ => false) SubexpPseudovarTable;
      cur_grp_id := NONE);

fun insert_waiting (v, var_type) =
    CPN'StringTable.insert WaitingVarTable (v, var_type);

fun insert_subexpvar v =
    CPN'StringTable.insert 
    SubexpVarTable (v, v);

local
    (* Merge expressions of double arcs into one expression *)
    fun compress_parsed (NonDiv (a  as {exp=e , coef=(c ,_), timed= t ,...}),
			 NonDiv (a' as {exp=e', coef=(c',_), timed= t',...}))=
	(case t of
	     NONE  =>  (* if t is NONE so is t' 
			(because the corresponding place is un-timed) *)
		 Div (NonDiv a, NonDiv a', 
		      concat [c,if c="" 
				then ""
				else "`",e," ++ ",c',if c'=""
						     then ""
						     else "`",e'],
		      NONE)
	   | SOME (s,_) => 
		 case t' of
		     SOME (s',_) =>
			 Div (NonDiv a, NonDiv a',
			      concat["(",c,if c="" 
					   then ""
					   else "`",e,if s=""
						      then ""
						      else "@+",s,")+++(",c',
				     if c'="" 
				     then ""
				     else "`",e',if s'=""
						 then ""
						 else "@+",s',")"],
			      SOME "")
		   | _ => raise InternalError "fun compress_parsed")
      | compress_parsed (NonDiv (a as {exp= e, coef= (c,_), timed= t,...}),
			 Div (a' as (a1, a2, expstr, time))) =
	(case t of 
	     NONE =>
		 Div (NonDiv a, Div a', 
		      concat [c,if c=""
				then ""
				else "`",e," ++ ",expstr],
		      NONE)
	   | SOME (s,_) => 
		 (case time of 
		      SOME "" =>
			  Div (NonDiv a, Div a',
			       concat [c,if c=""
					 then ""
					 else "`",
				       e,if s=""
					 then ""
					 else "@+",s," +++ ",expstr],
			       SOME "")
		    | SOME s' =>
			  Div (NonDiv a, Div a',
			       concat [c,if c=""
					 then ""
					 else "`",e,if s=""
						    then ""
						    else "@+",s,
				       " +++(",expstr,")",
				       if s'=""
				       then ""
				       else "@+",s'],
			       SOME "")
		    | _ => raise InternalError "fun compress_parsed" ))
      | compress_parsed (Div (a' as (a1, a2, expstr, time)),
			 NonDiv (a as {exp= e, coef= (c,_), timed= t,...})) =
	compress_parsed (NonDiv a, Div a')
      | compress_parsed (Div (a as (a1, a2, expstr, time)),
			 Div (a' as (a'1, a'2, expstr', time'))) =
	case time of
	    NONE => Div (Div a, Div a',
			 concat [expstr," ++ ",expstr'],
			 NONE)
	  | SOME "" => (* a holds a tms exp *)
		(case time' of
		     SOME "" => (* a' holds a tms exp *)
			 Div (Div a, Div a',
			      concat [expstr," +++ ",expstr'],
			      SOME "")
		   | SOME str => (* a' holds an ms exp *)
			 Div (Div a, Div a',
			      concat [expstr," +++(",expstr',")@+",str],
			      SOME "")
		   | _ => raise InternalError "fun compress_parsed" )
	  | SOME str => (* a holds an ms exp *)
		(case time' of
		     SOME "" => (* a' holds a tms exp *)
			 Div (Div a, Div a',
			      concat [expstr," +++ ",expstr'],
			      SOME "")
		   | SOME str =>  (* a' holds an ms exp *)
			 Div (Div a, Div a',
			     concat [expstr," ++ ",expstr'],
			      SOME str)
		   | _ => raise InternalError "fun compress_parsed" );
	
    fun option_add (NONE,NONE) = NONE
      | option_add (SOME n:int option, NONE) = SOME n
      | option_add (NONE, SOME n) = SOME n
      | option_add (SOME n, SOME n') = SOME (n+n')

    fun ins (aexp,[],res) = aexp::res
      | ins (arc  as {place=p , arcs=a , parsed_aexps=pa , no_of_tokens=t },
	    (arc' as {place=p', arcs=a', parsed_aexps=pa', no_of_tokens=t'})::xs,res) =
	if p = p' then
	    CPN'Misc.flatten [xs,
			      {place = p, 
			       arcs = a^^a',
			       parsed_aexps = compress_parsed (pa, pa'),
			       no_of_tokens = option_add (t,t')}::res]
	else
	    ins (arc, xs, arc'::res);
in
    fun ins_compressed (aexp, id) = let

	val {no_of_vars, arc_exps, 
	     guard_items, vars,...} = (CPN'IntTable.lookup GroupTable id)
	    handle HashNotFound => raise InternalError "ins_compressed"
    in
	CPN'IntTable.insert GroupTable 
	(id,{grpid = id, no_of_vars = no_of_vars, 
	     arc_exps = ins(aexp,arc_exps,[]),
	     guard_items = guard_items, vars = vars})
    end; (* ins_compressed *)
end; (* local *)

fun insert_grp (exp, id) = let

    val {no_of_vars, arc_exps, guard_items, vars,...} = 
	(CPN'IntTable.lookup GroupTable id) 
	handle HashNotFound => 
	    {grpid=id, no_of_vars = ref 0,
	     arc_exps = [], guard_items = [], vars = ref []};
    val arc_exps' = 
	case exp of (Arc a) => a::arc_exps | _ => arc_exps;
(*    val guard_items' = 
	case exp of (Guard g) => g::guard_items | _ => guard_items; *)
    val guard_items' = 
	case exp of 
	    (Guard (g as {parsed_item=(NonBindable {vars=vars,...}),...})) => 
		(g::guard_items)
	  | (Guard (g as {parsed_item=(Bindable ({vars=vars1,...},{vars=vars2,...})),...})) => 
		(g::guard_items)
	  | _ => guard_items;
in
    CPN'IntTable.insert GroupTable 
    (id,{grpid = id, no_of_vars = no_of_vars, arc_exps = arc_exps',
	 guard_items = guard_items', vars = vars})
end; (* insert_grp *)

fun insert_var v =
    case CPN'StringTable.find VarTable v of
	NONE =>
	    (CPN'StringTable.insert 
	    VarTable (v, {var = v, grpid = ~1, varid = ~1,
			  bindable = ref false, var_type = cpn,
			  no_of_exps = ref 1})          )
      | _ => ();

fun insert_transvar (v, id, var_type) = let
    val {no_of_vars, vars,...}  = CPN'IntTable.lookup GroupTable id
	handle HashNotFound => raise InternalError ("insert_transvar "^v)
    val var_item_option = CPN'StringTable.find VarTable v
    val new_item = {var = v, grpid = id, varid = !no_of_vars, 
		   bindable = ref false, var_type = var_type,
		   no_of_exps = ref 1}
    val item = if not (Option.isSome(var_item_option))
	       then new_item
	       else if (#grpid(Option.valOf var_item_option)) <> ~1
	       then raise InternalError "insert_transvar: inserting known item with proper group"
	       else {var = v, grpid = id, varid = !no_of_vars, 
		     bindable = #bindable(Option.valOf(var_item_option)), 
		   var_type = var_type,
		   no_of_exps = ref(!(#no_of_exps(Option.valOf(var_item_option)))+1)}
in
    (CPN'StringTable.insert 
     VarTable (v, item);
     inc no_of_vars;
     vars := v::(!vars);
     case var_type of
	 cpn => ()
       | (pseudo v') =>  (* insert a pseudo-guard *)
	     insert_grp 
	     (Guard {item_str = concat[v,"=",v'],
		     parsed_item = Bindable 
		     ({exp = v, vars = [v], is_pat = true},
		      {exp = v', vars = [v'], is_pat = true})},id))
end (* insert_transvar *)

fun move_waiting_vars id =
    (CPN'StringTable.appi (fn (v, var_type) => 
			   (insert_transvar (v, id, var_type))) WaitingVarTable)

local
    fun insert_exp' exp =
	((case (!cur_grp_id) of 
	     NONE => (* exp's in a group of its own so far; new id: *)
		 (insert_grp (exp, !next_grp_id);
		  move_waiting_vars (!next_grp_id);
		  inc next_grp_id)
	   | (SOME id) => (* exp already got an id; no waiting vars *)
		 (insert_grp (exp, id)));
	 init_exp ())

in
    fun insert_exp (Arc (exp as {place,...})) =
	(case CPN'IdTable.find InputPlaceTable place of
	     NONE =>
		 ((case (!cur_grp_id) of
		       NONE =>
			   CPN'IdTable.insert InputPlaceTable (place,!next_grp_id)
		     | SOME id => 			 
			   CPN'IdTable.insert InputPlaceTable (place,id));
                  insert_exp' (Arc exp))
	   | SOME grp_id => 
		 (move_waiting_vars grp_id;
		  ins_compressed (exp, grp_id);
		  init_exp()))
      | insert_exp exp = insert_exp' exp
end 

(* merge two disjunct groups: *)

fun merge_groups (src, dest) = 
    if (src = dest) orelse (src < 0) orelse (dest < 0) then () else
	let
	    val {grpid = _, 
		 no_of_vars = ref nov_src, 
		 arc_exps = aexps_src, 
		 guard_items = gitems_src, 
		 vars = ref v_src} = CPN'IntTable.remove GroupTable src;

	    val {grpid = _, 
		 no_of_vars = ref nov_dest, 
		 arc_exps = aexps_dest, 
		 guard_items = gitems_dest, 
		 vars = ref v_dest} = CPN'IntTable.remove GroupTable dest;

	    (* update information for each variable in src group: *)

	    fun upd_vars ([],_) = ()
	      | upd_vars (v::xs,n) =
		let
		    val {var_type, no_of_exps,bindable,varid,...} = 
			CPN'StringTable.remove VarTable v;
		in
		    (CPN'StringTable.insert 
		     VarTable (v,{var = v, grpid = dest,
				  varid = n, bindable = ref (!bindable),
				  var_type = var_type, no_of_exps = no_of_exps});
		     upd_vars (xs,n+1))
		end;
		     
	in
	    (upd_vars (v_src, nov_dest);
	     CPN'IntTable.insert GroupTable 
	     (dest, {grpid = src, no_of_vars = ref (nov_src + nov_dest),
		     arc_exps = CPN'Misc.flatten [aexps_src,aexps_dest],
		     guard_items = CPN'Misc.flatten [gitems_src, gitems_dest],
		     vars = ref (CPN'Misc.flatten [v_src, v_dest]) }))
	end;

local
    fun get_new_name v = let

	val cpn_no = !cur_pseudo_no;
	val _ = inc cur_pseudo_no;
    in
	concat ["CPN'VAR", Int.toString cpn_no, v]
    end

    fun insert_new_grpvar v = 
	(case (!cur_grp_id) of
	     NONE => (* haven't found a grp-id yet *)
	     (insert_waiting (v, cpn);
	      insert_subexpvar v)
	   | (SOME id) =>
	     (insert_transvar (v, id, cpn);
	      insert_subexpvar v))
in
    fun insert_grpvar (v, min, max) =
	case (CPN'StringTable.find SubexpVarTable v) of
	    SOME(_) => (* we have seen this one before in this sub-exp *)
	    let
		val v' = get_new_name v;
	    in
		v' before 
		CPN'StringTable.insert SubexpPseudovarTable (v',
							     (v, min, max))
	    end
	  | NONE => (* haven't seen this one before in _this_ subexp *)
	    v before 
	    (case (CPN'StringTable.find VarTable v) of
		 NONE => (* v hasn't appeared in any exp yet *)
		 insert_new_grpvar v
	       | SOME {no_of_exps, grpid=(~1),...} => 
		 (* v was inserted because either it is an output var 
		  * from a code segment or it is an unbound input var
		  * from a code segment *)
		 insert_new_grpvar v
	       | SOME {no_of_exps, grpid,...} => 
		 (* v is already in a group! *)
		 (case !cur_grp_id of
		      NONE => 
		      (* haven't found a group yet *)
		      (cur_grp_id := (SOME grpid);
		       move_waiting_vars grpid)
		    | SOME grpid' => 
		      (* have already found a group; merge the two
		       * with the current group as the source *)
		      (merge_groups (grpid',grpid);
		       cur_grp_id := SOME grpid);
		      insert_subexpvar v;
		      inc no_of_exps))
end; (* local to 'insert_grpvar' *)

fun list_pseudo_vars () =
    foldr (fn ((a,(_,b,c)),l) 
	  => (a,b,c)::l) [] (CPN'StringTable.listItemsi SubexpPseudovarTable);

fun delete_pseudo_vars () =
    (CPN'StringTable.filter (fn _ => false) SubexpPseudovarTable;
     CPN'StringTable.filter (fn _ => false) SubexpVarTable)

fun save_pseudo_vars () =
     (case (!cur_grp_id) of
	  NONE => 
	      (app (fn (v,(v',_,_)) => insert_waiting (v, pseudo v')) 
	       (CPN'StringTable.listItemsi SubexpPseudovarTable)) 
       | (SOME id) => 
	     (app (fn (v,(v',_,_)) => insert_transvar (v, id, pseudo v'))
	      (CPN'StringTable.listItemsi SubexpPseudovarTable));
      delete_pseudo_vars())


fun is_pseudo v =
    case CPN'StringTable.find SubexpPseudovarTable v of
	NONE => false
      | _ => true

fun valid_group id =
    (CPN'IntTable.lookup GroupTable id; true)
    handle HashNotFound => false;

fun list_groups () =
    map (fn (a,b) => b) (CPN'IntTable.listItemsi GroupTable);

fun grpvar_filter f id = 
    CPN'Misc.filter f (!(#vars (CPN'IntTable.lookup GroupTable id)))
    handle HashNotFound => raise InternalError "grpvar_filter"

fun set_bindable v =
    case CPN'StringTable.find VarTable v of
	SOME{bindable,...} => bindable:= true
      | NONE => raise InternalError "set_bindable"

fun list_unbound_grpvars id =
    grpvar_filter 
    (fn v => not (!(#bindable (CPN'StringTable.lookup VarTable v)))) id
    handle HashNotFound => raise InternalError "list_unbound_grpvars"

fun list_unbound_vars () =
    foldr (fn ((v,{bindable,...}:var_item),l) => 
	  if (!bindable) then l else v::l) 
    [] (CPN'StringTable.listItemsi VarTable)

fun list_group_vars id =
    (grpvar_filter (fn v => (#var_type (CPN'StringTable.lookup VarTable v)) = cpn) id)
    handle HashNotFound => raise InternalError "list_group_vars"


fun list_input () =
    foldr (fn ((grp_id,{arc_exps,...}),l) =>
	      foldr (fn ({place,arcs,no_of_tokens,...},l) =>
	       {place = place, arcs = arcs, no_of_tokens = no_of_tokens}::l)
	      l arc_exps)
    [] (CPN'IntTable.listItemsi GroupTable);

(* NOTICE: it is not all groups in the range [0,..,next_grp_id-1] that exists
 * some of these may be merged into other groups! *)

fun no_of_groups () = !next_grp_id;

(********** CheckedTable *************************)

val CheckedTable : ((string,unit) CPN'IdTable.hash_table) = 
    CPN'IdTable.mkTable hashString (InitHashLength, HashNotFound);

fun init_checked checked =
    CPN'IdTable.filter (fn _ => false) CheckedTable
    before app (fn id => CPN'IdTable.insert CheckedTable (id,())) checked;

fun insert_checked id =
    CPN'IdTable.insert CheckedTable (id,());

fun list_checked () = 
    map (fn (a,b) => a) (CPN'IdTable.listItemsi CheckedTable);

fun rm_checked id =
    () before CPN'IdTable.remove CheckedTable id
    handle HashNotFound => ();

fun is_checked id =
    case CPN'IdTable.find CheckedTable id of
	NONE => false
      | _ => true;

end; (* CPN'SyntaxTables *)

(* A table holding symbols living in a local scope in an arc-exp
 * (let ... in ... end) 
 *)

structure CPN'SymTable = struct

    exception HashNotFound;
    
    val table: ((string,int) CPN'StringTable.hash_table) = 
	CPN'StringTable.mkTable hashString (17,HashNotFound);

    val peek = CPN'StringTable.find table;
    val insert = CPN'StringTable.insert table;
    val remove = CPN'StringTable.remove table;
    val list = fn () => CPN'StringTable.listItems table;

    fun add sym = 
	case peek sym of
	    NONE => insert (sym,1)
	  | SOME n => insert (sym,n+1);

    fun rm [] = ()
      | rm (s::xs) =
	case peek s of
	    NONE => ()
	  | SOME 1 => (remove s;rm xs)
	  | SOME n => (insert (s,n-1);rm xs)
end;
