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
(* File: syntax_check.sml
 *
 * Syntax checker facilities.
 *)

val all_expr = ref [] : (string * bool) list ref

functor CPN'MakeSyntaxCheck (structure InstTable: CPN'INSTTABLE) : CPN'SYNTAXCHECK = struct

    structure Time = InstTable.RepTable.Time;

    exception IllegalMsExp of string;	

    val max = Int.max
    and min = Int.min;

    open  CPN'TransitionTable Compiler.Ast
	CPN'SyntaxErrors CPN'SyntaxDatatypes CPN'AstLib CPN'SyntaxTables;

    (* values and functions dealing with the statusbar message: *)

    val NoOfCheckedPages = ref 0;
    val NoOfCheckedPlaces = ref 0;
    val NoOfCheckedTransitions = ref 0;

    fun display_status statref =
	let
          val displ = (fn _ => ())
	    val _ = inc statref;
	in
	    displ [Int.toString (!NoOfCheckedPages)," Pages, ",
		   Int.toString (!NoOfCheckedPlaces)," Places, ",
		   Int.toString (!NoOfCheckedTransitions)," Transitions Checked"]
	end

    fun init_check () = 
	(CPN'start_timing ("timing-"^(Word32.toString(OSDep.getpid()))^".txt");
	 NoOfCheckedPages := 0;
	 NoOfCheckedPlaces := 0;
	 NoOfCheckedTransitions := 0;
	 CPN'CodeGen.init_dumping())

(*    fun codegen_init onoff =
	(CPN'Settings.use_codegen := onoff;
	 CPN'CodeGen.init_dumping());
*)
    val flatten = CPN'Misc.flatten;
    val filter = CPN'Misc.filter;
 
    val null_time_str = InstTable.RepTable.Time.null_str;
    val null_time_val = InstTable.RepTable.Time.null;

    (* the number of tokens to be removed from a place;
     * if a non-constant coef. exists for an arc-exp, this value
     * is NONE, otherwise SOME n, where n is the number of tokens *)
    val no_of_tokens = (ref (SOME 0)) : int option ref
	
    (* the time-value (if constant) for a timed place. If  p(a) 
     * for the current arc a is un-timed, this value is not used   *)
    val time_constant = ref (SOME nil): Time.time list option ref

    val expstr_head = ref "";
    val expstr_head_withrefs = ref "";
    val patstr_head = ref "";
    val cpn_vars = ref "";
    val page_refs = ref "";

    val expstr_headlength = ref 0;
    val patstr_headlength = ref 0;

    val errors = ref []: (CPN'Id.id * string) list ref;

    val uses = ref []: (CPN'Id.id * string list) list ref;
	
    val asts = ref []: (CPN'Id.id * string) list ref;
	
    fun ml_resp s = concat["",s];

    fun error (id, str_list) = 
	(errors ::= (id, concat str_list);
	 raise SyntaxError "");

    fun has_error id = 
	List.exists (fn (i,str) => i=id) (!errors)

    fun use (id,str)
	= uses := (id,CPN'Dep.find_use str)::(!uses);

    fun store_ast (id,str)
(*    = asts := (id, str)::(!asts);*)
	= asts := (id,CPN'Dep.find_ast str)::(!asts)
      handle _ => ();

    fun insert_use (id,use_lst)
	= let
	      fun trav ((id',use_lst')::rest)
		  = if id=id' then (id',use_lst^^use_lst')::rest
		    else ((id',use_lst')::(trav rest))
		| trav [] = [(id,use_lst)]
	  in
	      uses := trav (!uses)
	  end;

(*
 input    : {arcs: (id * CPN'PlaceTable.exp_type) list, place: id, no_of_tokens: int option} list,
 groups   : {vars: string list, bops: bop list} list,
 time_reg : string,
 code_reg : {input : string list, output: string list, action: string} option,
 free_vars: string list,
 output   : {arcs : (id * CPN'PlaceTable.exp_type) list, place: id} list
*)

    fun find_vars ({vars, bops}::rest) res = find_vars rest (res^^vars)
      | find_vars [] res = res;

    fun construct_arc_exp ({id, place, exp}::rest) res
	= construct_arc_exp rest ("("^exp^");\n"::res)
      | construct_arc_exp [] res = res;
	
    fun construct_trans_dep_fun  (groups, free_vars, arcs, guard, time, code, chan, prio)
	= let
	      val all_vars = find_vars groups free_vars;
	      val pattern = concat("("::tl(foldr (fn (v,b) => ","::v::": "::(#cs (CPN'VarTable.find v))::b) 
					   ["",")"] all_vars));
	      val exp = concat (construct_arc_exp arcs []);
	  in
	      concat ["fun CPN'x ",pattern," = (",exp,"(",code,");(",time,");(",guard,");(",chan,");(",prio,"));"]
	  end;
    fun construct_arc_dep_fun (groups, free_vars, arc)
	= let
	      val all_vars = find_vars groups free_vars;
	      val pattern = concat("("::tl(foldr (fn (v,b) => ","::v::": "::(#cs (CPN'VarTable.find v))::b) 
					   ["",")"] all_vars));
	      val exp = concat (construct_arc_exp [arc] []);
	  in
	      concat ["fun CPN'x ",pattern," = (",exp,"());"]
	  end;
    fun construct_guard_dep_fun  (groups, free_vars, guard)
	= let
	      val all_vars = find_vars groups free_vars;
	      val pattern = concat("("::tl(foldr (fn (v,b) => ","::v::": "::(#cs (CPN'VarTable.find v))::b) 
					   ["",")"] all_vars));
	  in
	      concat ["fun CPN'x ",pattern," = (",guard,");"]
	  end;
    fun get_var_decls (v::rest)
	= (#decl_id (CPN'VarTable.find v))::(get_var_decls rest)
      | get_var_decls [] = [];
		  	
    fun find_trans_dep (id, groups, free_vars, input, output, inout, inhibitor,
        reset, guard, time, code, chan, priority)
	= (use (id, construct_trans_dep_fun (groups, free_vars, 
					     input^^output^^inout,
					     guard, time, code, chan, priority));
	   (* FIXME: does this find dependencies for declarations of 
	    variables in output pattern of code segment? *)
	   insert_use (id, get_var_decls (find_vars groups free_vars));
	   List.map (fn arc => store_ast (#id arc, construct_arc_dep_fun (groups,
         free_vars, arc))) (input^^output^^inout);
	   store_ast (id, construct_guard_dep_fun (groups, free_vars, guard)));
			
    fun ms_wrap st = concat ["1`",st];
    fun wrap_parenthesis exp = concat ["(",exp,")"];

    fun initialise_check (page,checked) = let

	val _ = cpn_vars := CPN'VarTable.get_var_string ();
	val _ = page_refs := 
	    CPN'RefTable.get_var_string (CPN'PageTable.get_ref_vars page);
    in
	(expstr_head := 
	 concat ["structure CPN'x = struct val _ = fn ", !cpn_vars," => ("];
	 expstr_head_withrefs := 
	 concat ["structure CPN'x = struct ",
		  !page_refs," val _ = fn ", !cpn_vars," => ("];
	 patstr_head := "structure CPN'x = struct val _ = fn (";
	 expstr_headlength := String.size (!expstr_head) + 2;
	 patstr_headlength := String.size (!patstr_head) + 2;
	 init_checked checked;
	 errors := [];
       asts := [];
	 uses := [])
    end;

    fun exp_pos n = n - (!expstr_headlength)
	    
    fun exp_syntaxtree (aexp,cs) =
	unpack_exp (syntax_type_check [!expstr_head,aexp,"): ",cs])
	    
    fun pat_syntaxtree (aexp,cs) =
	unpack_pat (syntax_type_check [!patstr_head,aexp,") => ()"])

    fun guard_syntaxtree guard =
	unpack_exp (syntax_type_check [!expstr_head,guard,"): bool list"])

    fun chan_syntaxtree chan =
	unpack_exp (syntax_type_check [!expstr_head,chan,"): unit"])

    fun priority_syntaxtree priority =
      unpack_exp (syntax_type_check ["structure CPN'x = struct val _ = fn () => (",priority,"): int"])

    fun check_only (aexp,cs) = 
	syntax_type_check [!expstr_head, aexp,"): ",cs];

    fun check_with_pagerefs (exp, constraint) =
	syntax_type_check [!expstr_head_withrefs, exp,"): ",constraint];


    (************** functions for checking transitions  ******************)
	
    (* 'filter_entries'
     * delete 'pseudo-vars' from entries, i.e., from the lists of 
     * variables in each entry of a record or a tuple.
     * This is only done when we discover that a tuple or a record
     * is _not_ a pattern even with the pseudo-vars inserted.
     * Most often a (top-level) tuple or a record will be a pattern.
     * 
     *)

    fun filter_entries entry =  let

	fun filter_vlist ([],res) = res
	  | filter_vlist (v::xs,res) =
	    if (is_pseudo v) then filter_vlist (xs, res)
	    else filter_vlist (xs, v::res)
		
	fun filter (NonRec l) = NonRec (filter_vlist (l,[]))
	  | filter (Rec l) =
	    let
		fun filter' ([],res) = res
		  | filter' ({exp,label,no,vars}::xs,res) =
		    filter' (xs, {exp = exp, label = label, no = no,
				  vars = filter_vlist (vars,[])}::res)
	    in
		Rec (filter'(l,[]))
	    end
    in
	filter entry
    end (* filter_entries *)
	
    (* insert 'pseudo-vars' into string 'exp': *)

    fun insert_pseudo_vars(exp, min, max) = let

	fun ins e [] = [e]
	  | ins (e as (_,vmin,_)) ((e' as (_,vmin',_))::xs) =
	    if vmin<vmin' then
		e'::(ins e xs)
	    else
		e::e'::xs
				 
	fun sort res [] = res
	  | sort res (e::xs) = 
	    sort (ins e res) xs

	fun ins_pseudo exp [] = exp
	  | ins_pseudo exp ((var,vmin,max)::xs) =
	    let
		val vmin' = vmin-min;
		val max' = max-min;
		val exp' = 
		    if vmin'<0 then exp
		    else 
			(concat [String.substring(exp,0,vmin'), var,
				 String.substring (exp, max',
						   (String.size exp)-max')])
	    in
		ins_pseudo exp' xs
                           handle Subscript => (CPN'debug("ins_pseudo:SUBSCRIPT"); raise Subscript)
	    end

	val exp = String.substring(exp, min, max-min)
                           handle Subscript => (CPN'debug("insert_pseudo_vars:SUBSCRIPT exp="^exp^" min="^(Int.toString min)^" max="^(Int.toString max)); raise Subscript)
	val pseudo_vars = list_pseudo_vars ()
    in
	(* FIXME: change to Misc.sort *)
	ins_pseudo exp (sort [] pseudo_vars)
    end (* insert_pseudo_vars *)

    (* Returns sub-exp of exp with inserted pseudo vars if exp is a pattern.
     * Otherwise return sub-exp of exp unexpanded. *)
    fun is_pattern (exp, min, max) = let
	val exp' = insert_pseudo_vars (exp, min, max)
	val exp'' = String.substring(exp, min, max-min)
	    handle Subscript => raise Subscript
    in
	((true, exp')
	 before (pat_syntaxtree (exp', ""); ()))
	handle _ => (false, exp'')
    end
		
    fun pattern_type(aexp, entries, min, max, coef, atexp, wldcd) = let

	val (is_pattern, exp') = is_pattern(aexp, min, max);
        val _ = (all_expr := ((aexp, is_pattern)::(!all_expr)))
	val entries' = if is_pattern then entries 
		       else (filter_entries entries);

	val _ = if is_pattern then () else no_of_tokens := NONE;
	    
	val _ = 
	    case (!no_of_tokens) of 
		NONE => ()
	      | SOME n => 
		    case coef of
			("1",_) => 
			    no_of_tokens := SOME (n+1)
		      | (s,_) => 
			    (no_of_tokens := SOME (n+(valOf(Int.fromString s))))
			    handle Option => 
				no_of_tokens := NONE

	val _ = case atexp of
	    NONE => ()
	  | SOME (s,_)  => 
		case (!time_constant) of
		    NONE => ()
		  | _ => (CPN'debug ("pattern_type: case time_constant: s="^s);
			  CPN'Env.exec ["val _ = CPN'SyntaxCheck.time_constant:= SOME(maketime \""^s^"\"::(valOf(!CPN'SyntaxCheck.time_constant)))"];
			  CPN'debug ("pattern_type: case time_constant: time_constant="^(foldr (fn (elm,res)=> res^(Time.toString elm)^",") "" (valOf(!time_constant))))
			  )
			handle Error => (time_constant:= NONE)

    in 
	({exp = exp', entries = entries', pattern = is_pattern,
	  wldcd = wldcd, coef = coef, timed = atexp} : exptype)
	before
	(if is_pattern then save_pseudo_vars ()		
	 else  delete_pseudo_vars ())
    end 

    fun parse_wildcard _ = 
	raise SyntaxError "cannot check for wild-cards yet"

    and parse_symbols (_, res) [] = res
      | parse_symbols ((vmin,vmax), (vlist, min, max)) (s::xs) =
	let
	    val name = Compiler.Symbol.name s
	    val vlist' = 
		case (CPN'VarTable.peek name) of 
		    SOME _ => 
			(case CPN'SymTable.peek name of
			     NONE => (insert_grpvar(name,vmin,vmax))::vlist
			   | SOME _ => vlist)
		  | NONE => vlist
	in
	    parse_symbols ((vmin,vmax), (vlist',min, max)) xs
	end

	(* A number of functions for handling local declarations...
	 * parse_declsym keeps track of the symbols which appear on the left
	 * side of '=' in 'let val .. = ..' 	   *)

    and parse_declsym local_sym [] = local_sym
      | parse_declsym local_sym (s::xs) =
	let
	    fun ins (s,[],res) = (s::res,false)
	      | ins (s,s'::xs,res) = 
		if s = s' then (s::(xs^^res),true)
		else ins (s,xs,s'::res)
		    
	    val s = Compiler.Symbol.name s;
	    val (local_sym',found) = ins (s,local_sym,[])
	in
	    (* if the symbol in question hasn't been seen before in _this_
	     * 'let .. in ..end' then add it to the set of locally defined
	     * symbols *)
	    local_sym' before (if found then () else CPN'SymTable.add s)
	end

	(* parse definitions in RecordPat *)

    and parse_def (_,l) [] = l
      | parse_def (f,l) ((s,p)::xs) =
	parse_def (f, f (parse_pat (f,l) p) [s]) xs

	(* parse pattern *)
    and	parse_pat (f,l) (VarPat slist) = 
	f l slist
      | parse_pat (f,l) (RecordPat {def,...}) = 
	parse_def (f,l) def
      | parse_pat (f,l) (ListPat plist) =
	foldr (fn (p,l) => parse_pat (f,l) p) l plist
      | parse_pat (f,l) (TuplePat plist) =
	foldr (fn (p,l) => parse_pat (f,l) p) l plist
      | parse_pat (f,l) (FlatAppPat pats) =
	let
	    val env = Compiler.Environment.staticPart(Compiler.EnvRef.combined())
	    val error = fn region =>
		(Compiler.ErrorMsg.errorNoFile
		 ({ consumer= fn s => (CPN'debug "synchk:parse_pat:FATAL: Error consumer was called"), 
		   flush= fn () => (),
		   linewidth = fn () => 80},
		  ref false)
		 region);

            (* The following two functions are stolen from 
               110/src/sml-nj/elaborate/elabcore.sml *)
	    fun apply_pat (MarkPat(c,(l1,r1)),MarkPat(p,(l2,r2))) = 
		MarkPat(AppPat{constr=c, argument=p},(Int.min(l1,l2),Int.max(r1,r2)))
	      | apply_pat (c ,p) = AppPat{constr=c, argument=p}
	    fun tuple_pat (MarkPat(a,(l,_)),MarkPat(b,(_,r))) =
		MarkPat(TuplePat[a,b],(l,r))
	      | tuple_pat (a,b) = TuplePat[a,b]
	in
	    parse_pat (f,l)
	    (Precedence.parse {apply=apply_pat, pair=tuple_pat}
	                      (pats,env,error))
	end
      | parse_pat (f,l) (AppPat {argument,...}) =
	parse_pat (f,l) argument 

      (* don't care about the constraint in the above. 
       * E.g., in 'val (A x) = ', 
       * 'A' cannot be a CPN variable but x can...*)
      | parse_pat (f,l) (ConstraintPat {pattern,...}) =
	parse_pat (f,l) pattern
      | parse_pat (f,l) (LayeredPat {varPat, expPat}) =
	parse_pat (f,(parse_pat (f,l) expPat)) varPat
      | parse_pat (f,l) (MarkPat (p, (_,_) )) =
	parse_pat (f,l) p
      | parse_pat (f,l) (VectorPat plist) =
	foldr (fn (p,l) => parse_pat (f,l) p) l plist
      | parse_pat (f,l) (OrPat plist) =
	foldr (fn (p,l) => parse_pat (f,l) p) l plist
      | parse_pat (f,l) WildPat = l
      | parse_pat (f,l) (IntPat _) = l
      | parse_pat (f,l) (WordPat _) = l
      | parse_pat (f,l) (StringPat _) = l
      | parse_pat (f,l) (CharPat _) = l

	(* parse variable binding; local_sym holds current locally 
	 * defined symbols *)

    and	parse_vb (res,local_sym) [] = (res,local_sym)
      | parse_vb (res,local_sym) ((Vb {pat,exp,lazyp})::xs) = 
	 parse_vb (parse_exp res exp, 
		   parse_pat (parse_declsym,local_sym) pat) xs
      | parse_vb ((v,a,b),local_sym) ((MarkVb (vb, (a',b') )::xs)) = 
	 parse_vb ((v,min(a,exp_pos a'),max(b,exp_pos b')),local_sym) (vb::xs)

	(* parse recursive variable binding *)
	
    and	parse_rvb (res,local_sym) (Rvb {var,exp,...}) =
	(parse_exp res exp, parse_declsym local_sym  [var])
      | parse_rvb res (MarkRvb (rvb, (_,_) )) =
	(parse_rvb res rvb)

	(* parse declaration *)
	
    and	parse_dec res (ValDec (vlist,_) ) = 
	(parse_vb res vlist)
      | parse_dec res (ValrecDec (rvblist,_) ) =
	(foldr (fn (p,l) => parse_rvb l p) res rvblist)
      | parse_dec res (MarkDec (dec, (_,_) )) = (*FIXME:why is region ignored?*)
	(parse_dec res dec)
      | parse_dec res (SeqDec dlist) = 
	(foldr (fn (d,l) => parse_dec l d) res dlist)
      | parse_dec _ (FunDec _) = raise InternalError "Illegal FunDec"
      | parse_dec _ (TypeDec _) = raise InternalError "Illegal TypeDec"
      | parse_dec _ (LocalDec _) = raise InternalError "Illegal LocalDec"
      | parse_dec _ _ = raise InternalError "Illegal ?Dec"

	(* the following functions all deal with expressions *)
	    
    and parse_tup res [] = res
      | parse_tup res (e::xs) = 
	parse_tup (parse_exp res e) xs

    and parse_rec res [] = res
      | parse_rec res ((_,e)::xs) = 
	parse_rec (parse_exp res e) xs

    and parse_seq res [] = res
      | parse_seq res (e::xs) = 
	parse_seq (parse_exp res e) xs

    and parse_rules res [] = res
      | parse_rules res ((Rule {pat = p, exp = e})::xs) =
	parse_rules (parse_exp res e) xs

    and parse_exp (res as (_,min,max)) (VarExp slist) = 
	parse_symbols ((min,max), res) slist
      | parse_exp res (FnExp r) = 
	parse_rules res r
      | parse_exp res (FlatAppExp items) =           (* NEW *)
	    let
		val env = Compiler.Environment.staticPart(Compiler.EnvRef.combined())
		val error = fn region =>
                               (Compiler.ErrorMsg.errorNoFile
                                 ({ consumer= fn s => (CPN'debug "synchk:parse_exp:FATAL: Error consumer was called"), 
                                    flush= fn () => (),
                                    linewidth = fn () => 80},
                                  ref false)
                                 region);
	    in
		parse_exp res 
                          (Precedence.parse 
                            {apply=fn(f,a) => AppExp{function=f,argument=a},
                             pair =fn(a,b) => TupleExp[a,b]}
                             (items,env,error))
	    end
      | parse_exp res (AppExp {function = exp1, argument = exp2}) = 
	parse_exp (parse_exp res exp1) exp2
      | parse_exp res (CaseExp {expr = e, rules = r}) =
	parse_rules (parse_exp res e) r
      | parse_exp res (LetExp {dec = d, expr = e}) =
	let
	    val (res,local_syms) = parse_dec (res,[]) d
	in
	    parse_exp res e before (CPN'SymTable.rm local_syms)
	end
      | parse_exp res (SeqExp elist) = parse_seq res elist
      | parse_exp res (IntExp e) = res
      | parse_exp res (WordExp e) = res  (* NEW *)
      | parse_exp res (RealExp e) = res
      | parse_exp res (StringExp e) = res
      | parse_exp res (CharExp e) = res  (* NEW *)
      | parse_exp res (RecordExp elist) = parse_rec res elist
      | parse_exp res (ListExp elist) = parse_seq res elist  (* NEW *)
      | parse_exp res (TupleExp elist) = parse_tup res elist
      | parse_exp res (SelectorExp e) = res
      | parse_exp res (ConstraintExp {expr = e,...}) = parse_exp res e
      | parse_exp res (HandleExp {expr = e, rules = r}) =
	parse_rules (parse_exp res e) r
      | parse_exp res (RaiseExp e) = parse_exp res e
      | parse_exp res (IfExp {test=exp1, thenCase=exp2, elseCase=exp3}) =
	parse_exp (parse_exp (parse_exp res exp1) exp2) exp3
      | parse_exp res (AndalsoExp (exp1,exp2)) = 
	parse_exp (parse_exp res exp1) exp2
      | parse_exp res (OrelseExp (exp1,exp2)) = 
	parse_exp (parse_exp res exp1) exp2
      | parse_exp res (VectorExp elist) =
	parse_tup res elist
      | parse_exp res (WhileExp {expr = exp1, test = exp2}) = 
	parse_exp (parse_exp res exp1) exp2
      | parse_exp (v,a,b) (MarkExp (VarExp elist, (a',b') )) =
	parse_symbols ((exp_pos a',exp_pos b'),
		       (v, min(a,exp_pos a'), max(b,exp_pos b'))) elist
      | parse_exp (v,a,b) (MarkExp (e, (a',b') )) = 
	parse_exp (v, min(a,exp_pos a'), max(b,exp_pos b')) e
	
    (* parse fun for an input arc: *)
	
    and parse_input (ast, aexp, ms, timed, wldcd) = let

	val max_pos = String.size aexp;

	(* functions for parsing 'toplevel' arc-inscriptions *)
	fun parse_top ms res (FlatAppExp items) =
	    let
		val env = Compiler.Environment.staticPart(Compiler.EnvRef.combined())
		val error = fn region =>
                               (Compiler.ErrorMsg.errorNoFile
                                 ({ consumer= fn s => (CPN'debug "synchk:parse_top:FATAL: Error consumer was called"), 
                                    flush= fn () => (),
                                    linewidth = fn () => 80},
                                  ref false)
                                 region);
	    in
		parse_top ms res 
                          (Precedence.parse 
                            {apply=fn(f,a) => AppExp{function=f,argument=a},
                             pair =fn(a,b) => TupleExp[a,b]}
                             (items,env,error))
	    end
          | parse_top ms (res as (min,max,coef,atexp))
	    (AppExp {function = MarkExp (VarExp [opr], (_,_) ),
		     argument = TupleExp [exp1,exp2]}) =
	    (case (Compiler.Symbol.name opr) of
		 "`" => parse_pling (exp1, exp2, coef,atexp)
	       | "@+" => parse_atplus (exp1,exp2,coef)
	       | "@" => raise SyntaxError "parse_top: @ case" (*FIXME: change*)
	       | "++" =>
		     (if ms then 
			  Div (parse_top ms (max_pos, 0, 
					     ("",[]), atexp) exp1,
			       parse_top ms (max_pos, 0, 
					     ("",[]), atexp) exp2,
			       aexp, timed)
		      else let (* non-ms '+' operator: *)
			  
			  val (v',min',max') =
			      parse_exp (parse_exp 
					 ([],max_pos,0)
					 exp1) exp2
		      in
			  NonDiv (pattern_type (aexp, NonRec v', min', max', 
						coef, atexp, wldcd))
		      end)
	       | "+++" =>
			  Div (parse_top ms (max_pos, 0, 
					     ("",[]), atexp) exp1,
			       parse_top ms (max_pos, 0, 
					     ("",[]), atexp) exp2,
			       aexp, timed)
	       | _ => (* unknown operator *)
		     let
			 val (v, min', max') =
			     parse_exp (parse_exp ([],min,max) exp1) exp2;
		     in
			 NonDiv (pattern_type (aexp, NonRec v, min', max',
					       coef, atexp, wldcd))
		     end)
	  | parse_top ms (_,_,coef,atexp) (MarkExp (e, (min',max') )) = 
	    parse_top ms (exp_pos min',exp_pos max',coef,atexp) e
	  | parse_top ms res (SeqExp [e]) = (* "(exp)" *)
	    parse_top ms res e
	  | parse_top ms res (TupleExp elist) =
	    parse_toptuple res elist
	  | parse_top ms res (RecordExp elist) =
	    parse_toprec res elist
	  | parse_top _ (min, max, coef, atexp) e = 
	    let   (* non-toplevel exp-type *)
		val (v, min', max') = 
		    parse_exp ([], min, max) e
	    in
		NonDiv (pattern_type(aexp,NonRec v, min',max', 
				     coef, atexp,wldcd))
	    end (* parse_top *)

	and parse_pling (exp1, exp2, _, atexp) =
	    let
		val (vlist, min, max) = parse_exp ([],max_pos,0) exp1
		val coef = (String.substring(aexp,min,max-min), vlist)
                           handle Subscript => (CPN'debug("parse_pling:SUBSCRIPT");
						raise Subscript)
	    in
		parse_top false (max_pos, 0, coef, atexp) exp2
	    end (* parse_pling *)

	and parse_atplus (exp, time_exp, coef) =
	    let
		val (vlist, min, max) =  parse_exp ([],max_pos,0) time_exp
		val entry = filter_entries(NonRec vlist) 
		val time_exp_str = String.substring (aexp, min, max-min)
                           handle Subscript => (CPN'debug("parse_atplus:SUBSCRIPT");
						raise Subscript)
	    in
		case entry of
		    (NonRec vlist') =>
			parse_top true (max_pos, 0, coef,
					SOME(time_exp_str, vlist')) exp
		  | _ => raise InternalError "fun parse_atplus"
	    end (* parse_atplus *)

	and parse_toptuple (min, max, coef, atexp) l =
	    let
		fun parse_toptuple' _ [] = []
		  | parse_toptuple' n (e::xs) =
		    let
			val (v', min', max') = parse_exp ([],max_pos,0) e
		    in
			{exp = String.substring(aexp,min',max'-min'),
			 label = Int.toString n, no = n,
			 vars = v'}::(parse_toptuple' (n+1) xs)
                           handle Subscript => (CPN'debug("parse_toptuple:SUBSCRIPT");
						raise Subscript)
		    end
	    in
		NonDiv (pattern_type(aexp, Rec (parse_toptuple' 1 l), 
				     min, max, coef, atexp, wldcd))
	    end (* parse_toptuple *)

	and parse_toprec (min, max, coef, atexp) l =
	    let
		fun parse_toprec' _ [] = []
		  | parse_toprec' n ((label,e)::xs) =
		    let
			val (v', min',max') = 
			    parse_exp ([],max_pos,0) e
		    in
			{exp = String.substring(aexp,min',max'-min'),
			 label = Compiler.Symbol.name label, no = n,
			 vars = v'}::(parse_toprec' (n+1) xs)
                           handle Subscript => (CPN'debug("parse_toprec:SUBSCRIPT"); 
						raise Subscript)
		    end
	    in
		NonDiv (pattern_type(aexp, Rec (parse_toprec' 1 l), 
				     min, max, coef, atexp,wldcd))
	    end (* parse_toprec *)
	
	val time_parm = 
	    case timed of NONE => NONE
	  | SOME s => SOME (s,[]);
		
	val coef =  if ms then "" else "1";
    in
	(parse_top ms (max_pos, 0, (coef, []), time_parm) ast)
	handle Match => (CPN'debug "parse_top raised Match"; raise Match)
	     | Subscript => (CPN'debug "parse_top raised Subscript"; raise Subscript)
    end (* parse_input *)

    (* parse fun for an output arc *)

    fun parse_output (ast, aexp, _, _, _) = let
	
	(* for output arcs we only want to find the variables that
	 * appear in the inscriptions *)
	val max_pos = String.size aexp;

	val (vlist, _, _) = parse_exp ([],max_pos,0) ast

	(* delete 'pseudo-vars' from the var-list. On output arcs
	 * '(x,x)'  expressions can be handled directly *)
	    
	val _ = app (fn v => CPN'SyntaxTables.insert_var v) 
	    (case filter_entries (NonRec vlist) of
		 NonRec vlist => vlist
	       | _ => raise InternalError "fun parse_output")

    in    (* dummy div: *)
	NonDiv {exp = aexp, entries = NonRec vlist, pattern = false,
		wldcd = NONE, coef = ("",[]), timed = NONE }
    end;

    (* parse fun for 'x as pat' construct: *)

    fun parse_as (LayeredPat {varPat = VarPat slist,...},aexp,cs) = 
	let
	    fun skip_empty ((#" ")::xs) = skip_empty xs
	      | skip_empty l = l
	    
	    fun skip_name' (_,[]) = []
	      | skip_name' (ps,(#" ")::l) = ps^^l
	      | skip_name' (ps,(#"(")::l) = skip_name' ((#"(")::ps,l)
	      | skip_name' ((#"(")::ps,(#")")::l) = ps^^l
	      | skip_name' (ps,_::l) = skip_name' (ps,l)
	    
	    fun skip_name l = skip_name' ([],l);
		
	    fun skip l = (* given "x as pat" returns "pat" *)
		implode(skip_empty 
			(skip_name 
			 (skip_empty 
			  (skip_name
			   (skip_empty (explode l))))))
		
	    val aexp' = skip aexp;
	    val var = hd (#1 (parse_symbols ((0,0),([],0,0)) slist))
	in
	    (CPN'PlaceTable.token_exp var, 
	     parse_input(exp_syntaxtree(aexp',cs), aexp',false,NONE, SOME var))
	end 
      | parse_as (MarkPat (p, (_,_) ),aexp,timed) =
	parse_as (p,aexp,timed)
      | parse_as _ = raise SyntaxError ""
    
    (* Make sure that parentheses aren't
     * missing around product token inscriptions *)
    fun check_token_exp_parens (id,exp,errstr) = 
	let
	    val (stopchar,_) = 
		CPN'StreamIO.get_until_eos (TextIO.openString exp,[#","],true)
	in
	    if stopchar=(#",")
	    then error(id,[errstr,"Expression must be enclosed in parentheses"])
	    else ()
	end

    fun check_aexp parse_fun (id, aexp, cs, timed_place) = let 
	
	val _ = if aexp="" 
		    then error (id, [ArcError,EmptyError])
		else ()
    
	fun check_with_refs [] = raise SyntaxError ""
	  | check_with_refs ((aexp,constraint)::xs) =
	    () before (check_with_pagerefs(aexp,constraint); ())
	    handle SyntaxError _ => check_with_refs xs;


	fun mkms cs = concat["(",cs,") CPN'MS.ms"]
	fun mktms cs = concat["(",cs,") CPN'TMS.tms"]

	val aexp' = wrap_parenthesis aexp
	(* this is a hack! a closer look at the ast-parse
	 * funs should reveal why the ast position marks
	 * aren't ok for "f(x)" but are for "(f(x))"! *)

	open CPN'PlaceTable
    in
	if timed_place then
	    (token_exp aexp,
	     parse_fun                         		(* token-exp? *)
	     (exp_syntaxtree (aexp',cs), 
	      aexp', false, SOME ("0"), NONE))  (* Used to be  SOME ("(maketime \""^null_time_str^"\")") 
						 * instead of SOME("0"). 
						 * Changed to make @+ operator take an integer argument.
						 * This will break unit- and real-time.
						 * 
						 * mads
						 *)
	    handle (SyntaxError s) =>
		(ms_exp aexp,
		 parse_fun                     		(* ms-exp *)
		 (exp_syntaxtree (aexp', mkms cs), 
		  aexp', true, SOME ("0"), NONE))  (* Used to be  SOME ("(maketime \""^null_time_str^"\")") 
						    * instead of SOME("0"). 
						    * Changed to make @+ operator take an integer argument.
						    * This will break unit- and real-time.
						    * 
						    * mads
						    *)
		handle (SyntaxError _) =>
		    (tms_exp (ms_wrap aexp),
		     parse_fun                 		(* token-exp@+... *)
		     (exp_syntaxtree (wrap_parenthesis(ms_wrap aexp), mktms cs),
		      wrap_parenthesis(ms_wrap aexp), true, SOME "", NONE))
		    handle (SyntaxError _) =>
			(tms_exp aexp,
			 parse_fun              	(* ms-exp@+...*)
			 (exp_syntaxtree (aexp', mktms cs),
			  aexp', true, SOME "", NONE))
			handle (SyntaxError _) => 
			    (* try with the ref-vars to give 
			     * better error mes: *)
			    (check_with_refs 
			     [(aexp',cs),
			      (aexp', mkms cs),
			      (ms_wrap aexp', mktms cs),
			      (aexp, mktms cs)];
			     error (id,[ArcError,RefVarError]))
			    handle (SyntaxError _) => 
				error (id, [ArcError,ml_resp s])
	else		    
	    (token_exp aexp,
	     parse_fun(exp_syntaxtree (aexp', cs), aexp', false, NONE, NONE))
	    handle (SyntaxError _) =>
		   parse_as (pat_syntaxtree(aexp',cs), aexp', cs)
	    handle (SyntaxError _) =>
		   (ms_exp aexp,
		    parse_fun
			(exp_syntaxtree (aexp', mkms cs), 
			 aexp', true, NONE, NONE))
		   handle (SyntaxError s) =>
			  (check_only(ms_wrap aexp, mktms cs);
			   error (id,[ArcError,TimedExpError,"\n",CSnotTimedError,cs]))
			  handle (SyntaxError "") => 
				 (* SyntaxError raised by error fun in previous handle statement*)
				 raise SyntaxError ""
			       | (SyntaxError _) => 
				 (check_only (aexp', mktms cs);
				  error (id,[ArcError,TimedExpError,"\n",CSnotTimedError,cs]))
				 handle (SyntaxError "") =>
					(* SyntaxError raised by error fun in previous handle statement*)
					raise SyntaxError ""
				      | (SyntaxError _) => 
					(* try with the ref-vars to
					 * give better error mes: *)
					(check_with_refs [(aexp',cs), (aexp',mkms cs)];
					 error (id,[ArcError,RefVarError]))
					handle (SyntaxError _) => 
					       if (has_error id)
					       then raise SyntaxError ""
					       else error (id,[ArcError,ml_resp s])
    end (* end of check_aexp *)

    local
	fun ins (e,[],res) = e::res
	  | ins (outarc as {place = p, arcs = a},
		 (outarc' as {place = p',arcs = a'})::xs, res) =
	    if p = p' then
		flatten[{place=p, arcs=flatten[a,a']}::xs,res]
	    else
		ins (outarc, xs, outarc'::res);
    in			
	fun compress_out_arcs [] = []
	  | compress_out_arcs [e] = [e]
	  | compress_out_arcs arcs =
	    foldr (fn (x,l) => ins (x,l,[])) [] arcs
    end

    (* initialise rep. and dep. tables for a place, according to 
     * what kind of place it is :                                *)

    fun init_repdep (place_id, place_kind, trans_id, timed) = let

	val place_id = case place_kind of
	    CPN'PlaceTable.fusion grp => grp
	  | _ => place_id
    in
	(if timed then 
	     case !time_constant of
		 NONE => InstTable.RepTable.append_offset (place_id, trans_id, NONE)
	       | SOME lst => app (fn elm=> InstTable.RepTable.append_offset (place_id, trans_id, SOME elm)) lst
	 else ();
	 InstTable.RepTable.append_dep (place_id, trans_id);
	 InstTable.RepTable.append_order (place_id, trans_id, []))
    end

    fun check_input (_, [], _, true) = raise SyntaxError ""
      | check_input (res,[],_, false) = res
(*    Do not allow empty arc inscriptions
      | check_input (res, {exp = "", ...}::xs, trans_id, check_failure) =
	check_input (res, xs, trans_id, check_failure) *)
      | check_input (res, {id, place, exp}::xs, trans_id, check_failure) =
	let
	    val {kind,ext={cs,...},...} = CPN'PlaceTable.find place

	    val timed = CPN'CSTable.is_timed cs;

	    val (exp_type_exp, parsed_exp) =
		check_aexp parse_input (id, exp, cs, timed)
		handle SyntaxError _ => 
		    (check_input(res, xs, trans_id, true); 
		     raise SyntaxError "")

	    val _ = case exp_type_exp of 
			(CPN'PlaceTable.token_exp e) => 
			check_token_exp_parens (id,exp,ArcError)
		      | _ => ()

	    (* a check is made on whether a syntax error has been seen 
	     * - if so we do not want to update tables etc. *)
	    (* FIXME: Apparently obsolete comment - see bugfix below *)
	in
(*	    if check_failure then
		(CPN'debug "   (check failure)?";
		check_input(res, xs, trans_id, true) )
	    else *)  (*-APPARENTLY-BUGFIX-*)
	    let
		val _ = CPN'debug ("check_input: (ALWAYS FALSE?) check_failure="^(Bool.toString check_failure));
		val no_of_tokens' = 
		    (!no_of_tokens) before (no_of_tokens := SOME 0);
	    in
		(init_repdep (place, kind, trans_id, timed);
		 time_constant := (SOME nil);
		 insert_exp (Arc {place =place, 
				  arcs =[(id,exp_type_exp)], 
				  parsed_aexps = parsed_exp, 
				  no_of_tokens = no_of_tokens'});
		 check_input ({place=place, arcs=[(id,exp_type_exp)]}::res,
			      xs, trans_id, check_failure))
	    end
	end (* check_input *)

    fun check_empty (res, [], true) = raise SyntaxError ""
      | check_empty (res, [], false) = res
      | check_empty (res, { id, exp = "", place }::xs, check_failure) = 
        check_empty ({ arcs = [(id, NONE)], place = place }::res, xs, check_failure)
      | check_empty (res, {id, place, exp}::xs, check_failure) =
        error (id, [ArcError, NonEmptyError])

    fun check_output (res, [], true) = raise SyntaxError ""
      | check_output (res, [], false) = res
(*    Do not allow empty arc inscriptions
      | check_output (output,{exp = "",...}::xs, check_failure) =
	check_output (output,xs, check_failure) *)
      | check_output (output,{id, place, exp}::xs, check_failure) =
	let
	    (* start fix for: if ... then ... else ... @+ ... *)
	    fun fix exp =
		let
		    fun fix' (nil,_) = exp
		      | fix' ((#"@")::(#"+")::xs, ys) =
			concat(("(")::implode(rev ys)::")@+"::[(implode xs)])
		      | fix' (x::xs, ys) = fix' (xs, x::ys)
		in
		    fix'(explode exp,nil)
		end
	    val exp = fix exp
	    (* end fix *)

	    val {cs,...} = CPN'PlaceTable.get_ext place;

	    val {timed,...} = CPN'CSTable.find cs;
		
	    val (exp_type_exp, _) =
		check_aexp parse_output (id, exp, cs, timed)
		handle SyntaxError _ =>
		    (check_output (output, xs, true); raise SyntaxError "")

	    val _ = case exp_type_exp of 
			(CPN'PlaceTable.token_exp e) => 
			check_token_exp_parens(id,exp,ArcError)
		      | _ => ()
	in
	    (init_exp();
	     if check_failure then
		 check_output (output, xs, true)
	     else
		 check_output ({place=place, arcs=[(id,exp_type_exp)]}::output,
			       xs, check_failure))
	end (* check_output *)
    
    (* guard check funs: *)

    fun save_and_filter (is_pattern, vlist) =
	if is_pattern then 
	    vlist before save_pseudo_vars()
	else 
	    let 
		val entry = filter_entries (NonRec vlist) (* NonRec is a hack *)
	    in
		case entry of
		    (NonRec vlist') => 
		    vlist' before delete_pseudo_vars()
		  | _ => raise InternalError "fun parse_equal"
	    end

    fun parse_guard (ast, guard) = let

	val max_pos = String.size guard - 1 ;
	val min_pos = 1; (* because of initial "[" *)
	    
	fun parse_top (min,max) (FlatAppExp items) =           (* NEW *)
	    let
		val env = Compiler.Environment.staticPart(Compiler.EnvRef.combined())
		val error = fn region =>
                               (Compiler.ErrorMsg.errorNoFile
                                 ({ consumer= fn s => (CPN'debug "synchk:parse_guard:FATAL: Error consumer was called"), 
                                    flush= fn () => (),
                                    linewidth = fn () => 80},
                                  ref false)
                                 region);
	    in
		parse_top (min,max) 
                          (Precedence.parse 
                            {apply=fn(f,a) => AppExp{function=f,argument=a},
                             pair =fn(a,b) => TupleExp[a,b]}
                             (items,env,error))
	    end
	  | parse_top (min,max)
	    (AppExp {function = MarkExp (VarExp [opr], (_,_) ),
		     argument = TupleExp [exp1,exp2]}) = 
	    (case (Compiler.Symbol.name opr) of
		 "=" => parse_equal (exp1,exp2, min, max)
	       | _ => (* unknown operator *)
		     let
			 val (vlist,min',max') = 
			     parse_exp (parse_exp ([],max_pos,min_pos) exp1) 
			     exp2;

			 val item_string = String.substring(guard,min',
							    max'-min')
                           handle Subscript => (CPN'debug("parse_guard:SUBSCRIPT"); raise Subscript)

			 val vlist = save_and_filter(false,vlist);
		     in
			 {item_str = item_string,
			  parsed_item = NonBindable {exp = item_string,
						     vars = vlist, 
						     is_pat = false}}
		     end)
	  | parse_top (_,_) (MarkExp (e, (min',max') )) = 
		 parse_top (exp_pos min',exp_pos max') e
	  | parse_top (min,max) (SeqExp [e]) = (* "(exp)" *)
		 parse_top (min,max) e
	  | parse_top (min,max) e = 
		 let
		     val (vlist,min,max) = parse_exp ([],min, max) e;
			 
		     val item_str = String.substring(guard, min, max-min)
                           handle Subscript => (CPN'debug("parse_top:SUBSCRIPT"); raise Subscript)
			    
		     val vlist = save_and_filter(false, vlist);
			 
		     (* top-level 'ifs' doesn't get position marks: *)
		     (* FIXME: the following hack must be changed *)
		     fun insert_if (#"("::xs) = insert_if xs
		       | insert_if (#"i"::(#"f")::xs) = false
		       | insert_if _ = true

		     val item_str = case e of
			 (IfExp _) => 
			     if insert_if (explode item_str) then
				 concat["if ",item_str]
			     else
				 item_str
		       | _ => item_str;
		 in
		     {item_str = item_str,
		      parsed_item = 
		      NonBindable {exp = item_str, 
				   vars = vlist, 
				   is_pat = false}}
		 end

	and parse_equal (exp1, exp2, min, max) =
	    let
		val (v1,min,max) = parse_exp ([],max_pos, min_pos) exp1;
		val str1 = String.substring(guard, min, max-min)
                           handle Subscript => (CPN'debug("parse_equal1:SUBSCRIPT"); raise Subscript)
(*		val (is_pat1, str1') = is_pattern (str1, 0, max-min );  *)
		val (is_pat1, str1') = is_pattern (guard, min, max );
		val some_vars1= not(List.null v1);
		val v1' = save_and_filter (is_pat1, v1);
		    
		val (v2,min,max) = parse_exp ([],max_pos, min_pos) exp2
		val str2 = String.substring(guard,min,max-min)
                           handle Subscript => (CPN'debug("parse_equal2:SUBSCRIPT"); raise Subscript)
(*		val (is_pat2, str2') = is_pattern(str2, 0, max-min);*)
		val (is_pat2, str2') = is_pattern(guard, min, max);
		val some_vars2= not(List.null v2);
		val v2' = save_and_filter (is_pat2, v2);
		val item_str = concat [str1," = ", str2];
	    in
		if (not ((is_pat1 andalso some_vars1) orelse (is_pat2 andalso some_vars2))) then
		    {item_str =item_str,
		     parsed_item = NonBindable {exp = item_str, 
						vars = v1^^v2, 
						is_pat = false}}
		else
		    {item_str = item_str,
		     parsed_item = Bindable ({exp = str1', 
					      vars = v1',
					      is_pat = is_pat1},
					     {exp = str2', 
					      vars = v2',
					      is_pat = is_pat2})}
	    end
	
	fun parse_guarditems res (FlatAppExp items) =
	    let
		val env = Compiler.Environment.staticPart(Compiler.EnvRef.combined())
		val error = fn region =>
                               (Compiler.ErrorMsg.errorNoFile
                                 ({ consumer= fn s => (CPN'debug "synchk:parse_guarditems:FATAL: Error consumer was called"), 
                                    flush= fn () => (),
                                    linewidth = fn () => 80},
                                  ref false)
                                 region);
	    in
		parse_guarditems res 
                          (Precedence.parse 
                            {apply=fn(f,a) => AppExp{function=f,argument=a},
                             pair =fn(a,b) => TupleExp[a,b]}
                             (items,env,error))
	    end
	  | parse_guarditems res (ListExp (exp::exps)) = 
		    (insert_exp (Guard (parse_top res exp)))
			 before parse_guarditems res (ListExp exps)
	  | parse_guarditems res (ListExp nil) = ()
	  | parse_guarditems res (AppExp {function = MarkExp(VarExp [opr],_),
					  argument = TupleExp [exp1,exp2]}) = 
	    (case (Compiler.Symbol.name opr) of
		 "::" =>
		     (insert_exp 
		      (Guard (parse_top res exp1)))
		     before (parse_guarditems res exp2)
	       | s => raise InternalError ("fun parse_guarditems: symbol was "^s))
	  | parse_guarditems res (AppExp _) = (CPN'debug "GUARD:parse_guarditems:case AppExp(length<>2)")
	  | parse_guarditems res (MarkExp (e, (_,_) )) = 
	    parse_guarditems res e
	  | parse_guarditems res (SeqExp [e]) = (* "(exp)" *)
	    parse_guarditems res e
	  | parse_guarditems res (SeqExp _) = (CPN'debug "GUARD:parse_guarditems:case SeqExp(length<>1)")
	  | parse_guarditems _ (VarExp _) = (CPN'debug "GUARD:parse_guarditems:case VarExp")
	  | parse_guarditems _ (FnExp _) = (CPN'debug "GUARD:parse_guarditems:case FnExp")
	  | parse_guarditems _ (CaseExp _) = (CPN'debug "GUARD:parse_guarditems:case CaseExp")
	  | parse_guarditems _ (LetExp _) = (CPN'debug "GUARD:parse_guarditems:case LetExp")
	  | parse_guarditems _ (RecordExp _) = (CPN'debug "GUARD:parse_guarditems:case RecordExp")
	  | parse_guarditems _ (TupleExp _) = (CPN'debug "GUARD:parse_guarditems:case TupleExp")
	  | parse_guarditems _ (SelectorExp _) = (CPN'debug "GUARD:parse_guarditems:case SelectorExp")
	  | parse_guarditems _ (ConstraintExp _) = (CPN'debug "GUARD:parse_guarditems:case ConstraintExp")
	  | parse_guarditems _ (IfExp _) = (CPN'debug "GUARD:parse_guarditems:case IfExp")
	  | parse_guarditems _ (AndalsoExp _) = (CPN'debug "GUARD:parse_guarditems:case AndalsoExp")
	  | parse_guarditems _ (OrelseExp _) = (CPN'debug "GUARD:parse_guarditems:case OrelseExp")
	  | parse_guarditems _ (VectorExp _) = (CPN'debug "GUARD:parse_guarditems:case VectorExp")
	  | parse_guarditems _ (WhileExp _) = (CPN'debug "GUARD:parse_guarditems:case WhileExp")
	  | parse_guarditems _ _ = (CPN'debug "GUARD:parse_guarditems: last case"

				    ) (* e.g., nil *)
    in
	parse_guarditems (max_pos, min_pos) ast
    end; (* parse_guard *)

    fun check_guard (_,"") = ()
      | check_guard (id,guard) =
	parse_guard (guard_syntaxtree guard, guard)
	handle (SyntaxError s) => 
	    (let
		 val guard' = concat["[",guard,"]"]
	     in
		  parse_guard (guard_syntaxtree guard', guard')
	     end 
        handle (SyntaxError _) => 
	     ((check_with_pagerefs(guard,"bool list")
	       handle (SyntaxError _) => 
		   check_with_pagerefs(concat ["[",guard,"]"],"bool list"));
	      error (id,[ArcError,RefVarError]))
	handle (SyntaxError _) => 
	     error (id, [GuardError,ml_resp s]))
		 
    fun find_bops trans_id = let

	fun sort_ins (w,v,b,[]) = [(w,v,b)]
	  | sort_ins (w,v,b,(w',v',b')::res) =
	    if w' < w then (w',v',b')::(sort_ins (w,v,b,res))
	    else (w,v,b)::(w',v',b')::res

	fun find_bops' (res, ~1) = 
	    ((map (fn (w,v,b) => {vars = v, bops = b}) res)
	     handle _ => (raise InternalError "final map in find_bops"))
	  | find_bops' (res, n) =
	    (CPN'debug ("  find_bops':"^(Int.toString n)^":T="^trans_id);
	    if (not (valid_group n)) then find_bops' (res, n-1)
	    else let
		val _ = CPN'debug ("  PTnet.sim:"^(Int.toString n));
		val (weight,bop_seq) = (CPN'PTnet.sim (CPN'PTnet.con_ptnet n))
		    handle SyntaxError s => error(trans_id,[s])

		fun add_bp_info bseq = let
		    
		    fun upd_place (p,[]) = [(p,1)]
		      | upd_place (p,(p',n)::xs) = 
			if p = p' then (p,n+1)::xs
			else (p',n)::(upd_place (p,xs));
			    
		    fun find_bp_places ([], places) = places
		      | find_bp_places ((B_p{isdiv=SOME _,place,...})::xs,
					places) =
			find_bp_places (xs, upd_place (place,places))
		      | find_bp_places (_::xs, places) = 
			find_bp_places (xs, places)
			
		    fun add_no ([],_,_,_) = []
		      | add_no (l,_,_,~1) = l
		      | add_no (seq::xs,p,n,i) =
			(case seq of
			     B_p {isdiv = SOME _,place,pat,coef,vars,time} =>
				 if place = p then
				     (B_p {isdiv=SOME (n-i,n),
					   place=place,pat=pat,
					   coef=coef,
					   vars=vars,
					   time=time})::
				     (add_no (xs,p,n,i-1))
				 else
				     seq::(add_no (xs,p,n,i))
			   | _ => seq::(add_no (xs,p,n,i)));
			 
		    fun add_places (seq,[]) = seq
		      | add_places (seq,(p,1)::xs) = 
			add_places (seq,xs) 
		      | add_places (seq,(p,n)::xs) =
			add_places (add_no (seq,p,n,n-1),xs)
		in
		    add_places (bseq, find_bp_places (bseq,[]))
		end

		fun upd_single (place_id, order) =
		    InstTable.RepTable.append_order (place_id, trans_id, order);

		fun upd_rep_table [] = ()
		  | upd_rep_table (B_k {place, keys,...}::xs) =
		    (upd_single (place, map #no keys))
		    before
		    upd_rep_table xs
		  | upd_rep_table (_::xs) = upd_rep_table xs
	    in
		find_bops' (sort_ins (weight, list_group_vars n, 
				      add_bp_info bop_seq, res), n-1)
		before 
		upd_rep_table bop_seq
	    end)
    in
	find_bops' ([], no_of_groups()-1)
    end (* find_bops' *)

    (* code region check fun: *)

    fun check_code_reg (_,"") = (NONE,[])
      | check_code_reg (id,str) =
	(case CPN'parse_coderegion str of
	     NONE => (NONE,[])
	   | SOME ((inp_vlist, inp_vstr),
		   (outp_vlist, outp_vstr),
		   action, unbound_inputvars) => 
		 (case CPN'Env.is_decl 
		      (concat 
		       [!page_refs,
			"val _ = fn ",inp_vstr," => let val ",
			if outp_vlist=[] then "code_reg_output : unit" else outp_vstr,
			    " = ",action," in () end;"]) of
		      NONE => 
			  (app set_bindable outp_vlist;
			   (SOME {input = inp_vlist,
				  output = outp_vlist,
				  action = action},
			    unbound_inputvars))
		    | SOME s => 
			  raise SyntaxError (ml_resp s)))
	     handle SyntaxError s => error (id, [CodeError,s]);
						
    (* channel region check fun: *)

    fun check_chan_reg (_,"") = NONE
      | check_chan_reg (id,str) =
      let val ast = chan_syntaxtree str
	    val (vlist, _, _) = parse_exp ([], String.size str, 0) ast

	in SOME str
	end
	handle SyntaxError s => error (id, [ChannelError, s]);
(*	     error (id, [ChannelError,"Channels are not supported yet"]);*)
						

    fun check_priority_reg (_,"") = NONE
      | check_priority_reg (id,str) =
      let val ast = priority_syntaxtree str
        val (vlist, _, _) = parse_exp ([], String.size str, 0) ast

    in SOME str
    end
    handle SyntaxError s => error (id, [PriorityError, s]);
     (* check time region: *)
		 
    fun check_time_reg (_,"") = ("CPN'Time.fromInt(0)")  
      | check_time_reg (id,time_reg) =
	(check_with_pagerefs (concat ["(1`1)",time_reg],"int CPN'TMS.tms")
	 handle SyntaxError s => error (id,[TimeRegError,ml_resp s]);
	 case explode time_reg of
           ((#"@")::(#"+")::(#"+")::xs) => implode xs
         | ((#"@")::(#"+")::xs) => String.concat["CPN'Time.fromInt(", implode xs, ")"]
	   | _ => error (id, [TimeRegError,"Time region must start with @+"]))

    (* check if un-bound vars on output arcs are bindable: *)

    fun check_colorsets (id, vlist) =
	foldr (fn (v,l) =>
	      if (CPN'Env.is_bindable_cs (#cs(CPN'VarTable.find v))) then 
		  v::l
	      else 
		  error (id,[VarBindingError,v]))
	[] vlist

    (* Add a 'B_c' group for each of a list of vars *)

    fun add_bc_bop (res,[]) = res
      | add_bc_bop (res,v::xs) = 
	(insert_var v;
	 #bindable(CPN'StringTable.lookup VarTable v):= true;
	 add_bc_bop({vars=[v],
		     bops=[ B_c {var= v, cs= #cs(CPN'VarTable.find v)}]}::res, xs)) 
	handle HashNotFound => raise InternalError "add_bc_bop"
	    
    fun check_transition ({id, name, input, output, inout, inhibitor, reset,  guard, 
			   time_reg, code_reg, chan_reg, priority_reg, controllable}, page)  = let

	val _ = CPN'debug("check_transition "^id^"(name="^name^")")

	val is_checked = 
	    fn l => not (List.exists (fn {id, place, exp} =>
				 (not (is_checked place))) l);
    in
	if (is_checked input) andalso 
	    (is_checked output) andalso 
	    (is_checked inout) andalso 
	    (is_checked inhibitor) andalso 
	    (is_checked reset) then let

	    val _ = init_tables ();

	    val errors = ref false;
	       
	    val _ = check_input ([],input, id, !errors)
		handle SyntaxError _ => [] before errors := true;
			       
	    val inout' = check_input ([], inout, id, !errors)
		handle SyntaxError _ => [] before errors := true;

	    val _ = check_guard (id, guard)
		handle SyntaxError _ => errors := true;
		   
	    (* Find binding operators *)
	    val groups = 
		(if not (!errors) then find_bops id else [])
		handle SyntaxError _ => [] before errors := true

	    val time_reg' = check_time_reg (id,time_reg)
		handle SyntaxError _ => "" before errors := true;
		    
	    val (code_reg', unbound_inputvars) = check_code_reg (id, code_reg)
		handle SyntaxError _ => (NONE,[]) before errors := true;

	    val chan_reg' = check_chan_reg (id, chan_reg)
		handle SyntaxError _ => NONE before errors := true;

          val priority_reg' = check_priority_reg (id, priority_reg)
            handle SyntaxError _ => NONE before errors := true;

	   (* Add 'B_c' operators for unbound variables
	    * in the input part of the code-region: *)
	    val groups = 
		(if not (!errors) then
		    add_bc_bop (groups, unbound_inputvars) 
		else [])
		handle SyntaxError _ => [] before errors := true;

	    val output' = check_output ([], output, !errors)
		handle SyntaxError _ => [] before  errors := true;

	    val output'' =  
		if not (!errors) then
		   compress_out_arcs (inout'^^output')
		else [];

	    (*val inhibitor' = check_output ([], inhibitor, !errors)
		handle SyntaxError _ => [] before  errors := true;
	    val reset' = check_output ([], reset, !errors)
		handle SyntaxError _ => [] before  errors := true;*)
	    val inhibitor' = check_empty ([], inhibitor, !errors)
		handle SyntaxError _ => [] before  errors := true;
	    val reset' = check_empty ([], reset, !errors)
		handle SyntaxError _ => [] before  errors := true;
		    
	    val free_vars = 
		(if not (!errors) then
		    check_colorsets(id, list_unbound_vars())
		else [])
		handle SyntaxError _ => [] before  errors := true;
		    
	    val no_errors = 
		if not (!errors) 
		then () 
		else (* If there are input places with legal input arcs, then
		      * they were added to InstTable.RepTable in check_input
		      * Remove dependency between input places with 
		      * legal input arcs and this transition with errors *)
		    let
			val inputpids = map #place (list_input())
			val fusion_grps = 
			    foldr (fn (pid,tail) => 
				      case CPN'PlaceTable.peek pid of
					  (SOME {kind=CPN'PlaceTable.fusion grp ,...}) => grp::tail
					| _ => tail) [] inputpids
		    in
		    (app (fn pid => InstTable.RepTable.rm_trans (pid,id)) 
		     (inputpids^^fusion_grps);
			 raise SyntaxError "")
		    end

	    in
		(CPN'TransitionTable.insert 
		 (id,transition{page = page, 
				name = name,
				input = list_input(),
				groups = groups,
				monitors=[], 
				time_reg = time_reg',
				code_reg = code_reg',
				chan_reg = chan_reg',
				priority_reg = priority_reg',
				free_vars = free_vars, 
				output = output'',
                        reset = reset',
                        inhibitor = inhibitor',
                        controllable=controllable});
		 find_trans_dep (id, groups, free_vars, input, output, inout,
             inhibitor, reset, guard, time_reg, (case code_reg'
						    of SOME elm => #action elm
                                         | _ => ""), (case chan_reg' of SOME elm => elm | _ => ""), (case priority_reg' of SOME elm => elm | _ => ""));
		 insert_checked id)
	    end
	else 
	    error (id,["Transition not checked because a neighbour place has an error."]) (* Marks this trans. as un-checked *)
    end;

    fun check_transitions (transition_list,page_id) = 
	app (fn t => (check_transition (t, page_id)
		      handle SyntaxError str => 
			  (CPN'PageTable.remove_diff (page_id,{places'=nil,transitions'=[#id t]});
			   raise SyntaxError str);
		      display_status NoOfCheckedTransitions)
	     handle SyntaxError s => 
		 display_status NoOfCheckedTransitions) transition_list;

    (************** functions for checking places ******************)
	    
    local
	open CPN'PlaceTable

	fun cmp_markings (im, token_exp im') = 
	    CPN'Env.str_cmp "=" (im, im')
	  | cmp_markings (im, ms_exp im') = 
	    CPN'Env.str_cmp "==" (im, im')
	  | cmp_markings (im, tms_exp im') = 
	    CPN'Env.str_cmp "==" (im, im')

	fun extract_marking (token_exp im) = im
	  | extract_marking (ms_exp im) = im
	  | extract_marking (tms_exp im) = im;
		
	fun ins_place (id, kind, page, name, cs, im) = 
	    (insert(id, kind,
		    {page = page, 
		     name = name,
		     cs = cs, 
		     im = im});
	     insert_checked id);

	fun check_im (id,token_exp im, cs) =
	    (
             syntax_type_check ["structure CPN'x = struct val _ = fn () => (",
			       im,"): ",cs];

	     use (id, "val _ = fn () => ("^im^"): "^cs^".cs");
	     store_ast (id, "fun CPN'x () = (1`("^im^")): "^cs^".cs CPN'MS.ms");

	     if (CPN'Env.is_legal_ms cs ("1`("^im^")"))
		 then ()
	     else (raise SyntaxError (IllegalColIM)))
	  | check_im (id,ms_exp im, cs) =
	    (syntax_type_check ["structure CPN'x = struct val _ = fn () => (",
			       im,"): ",cs," CPN'MS.ms"];
	
	     use (id, "val _ = fn () => ("^im^"): "^cs^".cs CPN'MS.ms");
	     store_ast (id, "fun CPN'x () = ("^im^"): "^cs^".cs CPN'MS.ms");

	     if (CPN'Env.is_legal_ms cs im)
		 then () 
	    else (raise SyntaxError (IllegalColIM)))
	  | check_im (id,tms_exp im, cs) =
	    (syntax_type_check ["structure CPN'x = struct val _ = fn () => (",
			       im,"): ",cs," CPN'TMS.tms"];
	
	     use (id, "val _ = fn () => ("^im^"): "^cs^".cs CPN'TMS.tms");
(*	     store_ast (id, "fun CPN'x () = ("^im^"): "^cs^".cs CPN'TMS.tms");  Unlikely that we can exchange timed MSes /mw *)

	    if (CPN'Env.is_legal_ms (cs^"'timed") im)
		then () 
	    else (raise SyntaxError (IllegalColIM)))

	fun check_place (id, kind, page, name, cs, im) = 
	    (CPN'debug("check_place "^id^" (kind="^
		       (case kind of
			    place => "place"
			  | port _ => "port" 
			  | fusion _ => "fusion" 
			  | group => "group")^", name="^name^")");
	     case (CPN'CSTable.peek cs) of 
		 SOME {timed,...} =>
		     (if im = "" then                          (* empty IM? *)
                          (use (id, "val _ = fn () => empty: "^cs^" CPN'MS.ms");
                           store_ast (id, "fun CPN'x () = empty: "^cs^" CPN'MS.ms");
			  ins_place(id, kind, page, name, cs, ms_exp "[]"))
		      else                                 (* token-exp IM? *)
			  (check_im (id,token_exp im, cs);
			   ins_place(id, kind, page, name, cs, token_exp im))
			  handle SyntaxError _ =>                 (* ms IM? *)
			      (check_im (id,ms_exp im, cs);  
			       ins_place(id, kind, page, name, cs, ms_exp im))
			     handle (SyntaxError s) =>
				 (if timed then                   (* tms IM? *)
				     (check_im (id,tms_exp im, cs);
				      ins_place (id, kind, page,
						 name, cs, tms_exp im))
				     handle SyntaxError _ =>
					 let             (* timed token IM? *)
					     val im' = ms_wrap im
					 in
					     (check_im (id, tms_exp im', cs);
					      ins_place (id, kind, page, 
							 name, cs, 
							 tms_exp im'))
					     handle SyntaxError _ =>
						 error (id, [IllegalMS,
							     ml_resp s])
					 end
				 else
				     (check_im (id, tms_exp im, cs);
				      error (id,[IllegalMS,TimedExpError]))
				     handle SyntaxError _ =>
					 let            (* timed token IM? *)
					     val im' = ms_wrap im
					 in
					     (check_im (id, tms_exp im', cs);
					      error (id,[IllegalMS,
							 TimedExpError]))
					 end)
				     handle SyntaxError _ =>
					 error(id,[IllegalMS,ml_resp s]))
	       | NONE => error (id,[IllegalCS]))
	(* end of check_place *)
    in
    fun clean_up_place p =
	case CPN'PlaceTable.peek p of
	    NONE => 
	    raise InternalError ("clean_up_place: "^p)
	  | SOME _ => 
	    (CPN'debug ("clean_up_place "^p);
	     CPN'PlaceTable.remove p;
	     InstTable.RepTable.remove p)
					    
	fun check_places (places,page) = 
	    app (fn {id, name, cs,im} =>
		 ((check_place (id, place, page, name, cs, im);
		  case CPN'PlaceTable.peek id of
		      (SOME {ext={im=CPN'PlaceTable.token_exp exp,...},...})=>
		      (check_token_exp_parens (id,exp,IllegalMS)
		       handle SyntaxError str =>
			      (clean_up_place(id);
			       rm_checked id;
			       raise SyntaxError str))
		    | _ => ())
		  handle SyntaxError str =>
			 (CPN'PageTable.remove_diff (page,{places'=[id],transitions'=nil});
			  raise SyntaxError str);
			 (display_status NoOfCheckedPlaces)
			 )
		 handle SyntaxError _ =>
		     display_status NoOfCheckedPlaces) places;

	fun check_fusion (_,[],_) = ()
	  | check_fusion (checked, {id, grp, name, cs, im}::xs, page) =
	    (let
		val _ = CPN'debug("check_fusion "^id^" (grp="^grp^")")
		val {ext={page=page', name=name', cs=cs', im=im'},...} = 
		    case peek grp of
			SOME p => p
		      | NONE => 
			    (check_place (grp, group, page, name, cs, im);
			     CPN'PageTable.append_group(page, grp);
			     find grp);
			    
		val im = case im of "" => "[]" | _ => im;
	    in
		(check_place (id, fusion grp, page, name, cs, im);
		 if CPN'CSTable.is_equiv(cs,cs') then
		     (if cmp_markings (im,im') then 
			 ()
		     else
			 (app rm_checked (id::checked);
			  clean_up_place id;
			  error (id,[FusionMatchErrorIM])))
		 else
		     (app rm_checked (id::checked);
		      clean_up_place id;
		      error (id, [FusionMatchErrorCS]));
		 check_fusion (id::checked, xs,page))
	    end) (* check_fusion *) 
		 handle SyntaxError str => 
		     (app rm_checked checked;
		      CPN'PageTable.remove_diff (page,{places'=[id],transitions'=nil});
		      (* check the rest of the list to make sure that
		       * all fusion places with errors are removed from
		       * CPN'PageTable *)
		      check_fusion (id::checked, xs,page))

	fun check_fusions (fusions,page) =
	    let
		(* assemble list of groups into lists of lists of groups,
		 * each list has elements with same grp *)
		fun ins (e,[]) = [[e]]
		  | ins (e as {grp = g,name,id,im,cs},
			 (grp as ({grp = g',...}::_))::xs) =
		    if g = g' then
			(e::grp)::xs
		    else
			grp::(ins(e,xs))
		  | ins _ = raise InternalError "fun check_fusions"
	    in
		app  
		(fn l => (check_fusion ([],l,page);
			  display_status NoOfCheckedPlaces)
		 handle SyntaxError _ => 
		     display_status NoOfCheckedPlaces)
		(foldr (fn (g,l) => ins (g,l)) [] fusions)
	    end;

	fun check_substitutions ([],_) = ()
	  | check_substitutions ({id, name, subpage, border}::xs, page) =
	    let
		val _ = CPN'debug("check_substitution "^id^" (name="^name^")")

		exception BadBorder;

		exception PortSocketExn of CPN'Id.id * string
		
		fun check_border (false, []) = raise BadBorder
		  | check_border (true, []) = ()
		  | check_border (all_ok, {port = the_port, socket}::xs) = 
		    if (is_checked socket) then	let

			val {ext = {cs = cs, im = im,
				    name=socket_name,...},...} =
			    CPN'PlaceTable.find socket 
			    handle InternalError s => 
				raise InternalError (s^" fun check_border socket");

			val {ext = {cs = cs', im = im',...},...} =
			    CPN'PlaceTable.find the_port
			    handle InternalError s => 
				   raise PortSocketExn (socket,"Cannot find port on page "^CPN'PageTable.get_name subpage)

		    in
			(if CPN'CSTable.is_equiv(cs,cs') then ()
			 else error (socket, [SocketMatchErrorCS]);
			 check_border (all_ok,xs))
			 before
			 append_socket (the_port, (socket, id))
			 handle SyntaxError s => check_border(false,xs)
		    end
		    else check_border (false,xs)
	    in
		(check_border (true, border)
		 (* accept the substitution transition even though 
		  * there are problems with surrounding places, otherwise
		  * there may be problems syntax checking other super
		  * and subpages *)
		 handle BadBorder => ()
		      | PortSocketExn (socketid,errmsg) => 
			(error (socketid,[errmsg])
			 handle SyntaxError _ => 
				(display_status NoOfCheckedTransitions;
				 CPN'PageTable.remove_diff (page,{places'=[socketid],transitions'=nil})));
		 CPN'TransitionTable.insert 
		 (id,substitution{page = page,
				  name = name, 
				  subpage = subpage,
				  border = border});
		 CPN'PageTable.append_sub_node(id,page,subpage);
		 insert_checked id;
		 display_status NoOfCheckedTransitions;
		 check_substitutions (xs, page))
	    end;
	    
    end (* local to place-related check funs *)

    fun clean_up_page (id, {name, prime, included, 
			    checked_places,places_to_check, 
			    fusion_places_to_check,fusion_groups_to_check,
			    checked_transitions,
			    transitions_to_check,substitutions_to_check}) =
	case CPN'PageTable.peek id of
	    NONE => (* haven't seen this page before; insert it *)
		CPN'PageTable.insert 
		    (id,{page = {name=name,
				 prime=prime,
				 included=included,
				 places= checked_places^^places_to_check^^fusion_places_to_check,
				 transitions=checked_transitions^^transitions_to_check^^substitutions_to_check},
			 decl = NONE,
			 sub_pages = [],
			 super_pages = []})
          | SOME {page = {places = places', transitions = transitions',
			  prime = prime', included = included', name = name'},
		  decl,super_pages,sub_pages} => 
		let
		    (* Remove the fusion groups that are associated
		     * with other pages *)
		    val filtered_checked_places = 
			List.filter 
			    (fn pid => case CPN'PlaceTable.peek pid of
					   (SOME {kind=CPN'PlaceTable.group,int,
						  ext={page=grpid,...}})=> grpid=id
					 | _ => true)
			    checked_places

		    (* Fusion groups associated with this page that
		     * do not need to be rechecked *)
		    val checked_fusion_groups_this_page = 
			List.filter 
			    (fn pid => case CPN'PlaceTable.peek pid of
					   (SOME {kind=CPN'PlaceTable.group,int,
						  ext={page=grpid,...}})=> grpid=id
					 | _ => false)
			    checked_places

		    (* Fusion groups that are associated with this 
		     * page that need to be rechecked *)
		    val fg_to_recheck_this_page = 
			List.filter 
			    (fn pid => case CPN'PlaceTable.peek pid of
					   (SOME {kind=CPN'PlaceTable.group,int,
						  ext={page=grpid,...}})=> grpid=id
					 | _ => false)
			    fusion_groups_to_check
			    
		    val places_to_add = filtered_checked_places^^
                                        places_to_check^^
				        fusion_places_to_check

		    val known_places = places_to_add^^fg_to_recheck_this_page

		    val known_transitions = checked_transitions^^
				            transitions_to_check^^
					    substitutions_to_check

		    (* There are no more fusion places on this 
		     * page that are in fusion group with id p *)
		    fun clean_up_fusion_grp p = 
			case CPN'PlaceTable.peek p of
			    NONE => 
			    raise InternalError ("clean_up_fusion_grp: "^p)
			  | (SOME {kind,ext={page,name,cs,im},int}) => 
			    let
				val _ = CPN'debug ("clean_up_fusion_grp: "^p)
				val memids = CPN'PlaceTable.get_fusion_group_members p
				val pgids = 
				    List.filter 
					(fn pgid => pgid<>id)
					(map CPN'PlaceTable.get_page memids)
			    in
				if pgids = []
				then (* no other members of this fusion grp *)
				    clean_up_place p
				else (* move this grp to another page*)
				    (CPN'PlaceTable.insert(p,kind,{page=hd pgids,name=name,cs=cs,im=im});
				     CPN'PageTable.remove_diff (id,{places'=[p],transitions'=[]});
				     CPN'PageTable.append_group (hd pgids,p))
			    end
			
		    fun clean_up_transition t =
			 case peek t of
			    NONE => raise InternalError ("clean_up_transition: "^t)
			  | SOME (substitution {subpage,...}) => 
				(CPN'debug ("clean_up_transition (subst) "^t);
				 CPN'PageTable.remove_super_page (subpage,id,t);
				 CPN'PageTable.remove_sub_page (id,subpage,t);
				 remove t;
				 ())
			  | SOME (transition {input,...}) => 
				(CPN'debug ("clean_up_transition "^t);
				 app (fn pid => InstTable.RepTable.rm_trans (pid,t)) 
				 (InstTable.get_input_place_ids t);
				 remove t;
				 ())

		    (* Clean up substitution transitions that were previously
		     set in as transitions *)
		    fun clean_up_subst t = 
			case peek t of
			  SOME (transition {input,...}) =>
				(CPN'debug ("clean_up_subst "^t);
				 (app (fn p => InstTable.RepTable.rm_trans (#place p,t)) 
				 input;
				 remove t;
				 ()))
			  | _ => ()
				
		    (* Find the objects to be removed from the various tables:
		     * Return all elements from Y not existing in X.
		     *)
		    fun assym_diff (X,Y) = let
			fun f (y,ys) =
			    if List.exists (fn x => x=y) X then ys else y::ys
		    in
			foldr f nil Y
		    end
			
		    val deleted_pids = 
			assym_diff(known_places,places')

		    val (deleted_fusion_grps,deleted_places) = 
			List.partition 
			    (fn pid => CPN'PlaceTable.is_kind_group pid) 
			    deleted_pids
		in
		    ((* clean up places that have been deleted *)
		     app clean_up_place (deleted_places);
                     (* clean up fusion groups that have been deleted *)
	             app clean_up_fusion_grp (deleted_fusion_grps);
		     (* clean up places that are being checked again *)
		     app clean_up_place 
			 (List.filter 
			      (fn pid => case CPN'PlaceTable.peek pid of
					     SOME _ => true
					   | _ => false) (places_to_check^^
							  fusion_places_to_check^^
							  fg_to_recheck_this_page));
		     (* clean up transitions that have been deleted *)
		     app clean_up_transition (assym_diff(known_transitions,transitions'));
		     (* clean up transitions that are being checked again  *)
		     app clean_up_transition 
			 (List.filter 
			      (fn tid => case CPN'TransitionTable.peek tid of
					     SOME _ => true
					   | _ => false) transitions_to_check);
		     app clean_up_subst substitutions_to_check;
		     CPN'IdTable.insert CPN'PageTable.table 
		     (id, {page = {name = name, prime = prime, 
				   included = included,
				   places = places_to_add, 
				   transitions = known_transitions},
			   decl = decl,
			   super_pages = super_pages, 
			   (* sub_pages may have been changed after cleaning up a substitution transition*)
			   sub_pages = (CPN'PageTable.get_sub_pages id)}))
		end; (* clean_up_page *)
	    
    (************* function for checking a page *****************)

    fun check_page {id, name, prime, included,
		    checked:{places: CPN'Id.id list,
			     transitions: CPN'Id.id list},
		    places, fusions, substitutions, transitions} = let
	val _ = CPN'report_timing (concat["check_page ",name," @ "])
	val checked_p = #places checked;
	val checked_t = #transitions checked;
    in
	(I.+= (NoOfCheckedPlaces, (List.length checked_p));
	 I.+= (NoOfCheckedTransitions, (List.length checked_t));
	 (* FIXME: the clean up does not work, 
	  * it cleans things that should not be cleaned 
	  * 
	  * It would have been nice to know what it cleans, that
	  * it is not supposed to clean?!? [mads]
	  *)
	 clean_up_page (id,{name = name,
			    prime = prime,
			    included = included,
			    checked_places = checked_p,
			    places_to_check = map #id places,
			    fusion_places_to_check = map #id fusions,
			    fusion_groups_to_check = ListUtils.remdupl(map #grp fusions),
			    checked_transitions = checked_t,
			    transitions_to_check = map #id transitions,
			    substitutions_to_check = map #id substitutions});
	 initialise_check (id,List.@(checked_p,checked_t));
	 CPN'debug "-----CHECKING--PLACES--------";
	 check_places (places, id);
	 CPN'debug "-----CHECKING--FUSION-PLACES--------";
	 check_fusions (fusions, id);
	 CPN'debug "-----CHECKING--SUBSTITUTIONS--------";
	 check_substitutions(substitutions, id);
	 CPN'debug "-----CHECKING--TRANSITIONS--------";
	 check_transitions (transitions, id);
	 display_status NoOfCheckedPages;
	 CPN'PlaceTable.list();  (* FIXME: What does this do? Remove? *)
	 ((case !errors of
	       nil => nil
	     | list => (id,"")::list),
	   !uses, !asts))
    end handle SyntaxError str => 
	raise InternalError ("exception SyntaxError: \""^str^"\"")

    (* Partial fix to make it possible to change timed markings
     * Does not handle +++ in strings well. Improved input and input_ms
     * functions for colour sets would help, e.g. because cs.input functions
     * expect that there is whitespace between a colour and ++ or +++ *)
    fun fix_timed_markstr (c::(#"+")::(#"+")::(#"+")::rest) = 
	if Char.isSpace c
	   then c::(#"+")::(#"+")::(#"+")::(fix_timed_markstr rest)
	else c::(#" ")::(#"+")::(#"+")::(#"+")::(fix_timed_markstr rest)
      | fix_timed_markstr ((#"\n")::rest) = 
	(#"\\")::(#"n")::(fix_timed_markstr rest)
      | fix_timed_markstr ((#"\"")::rest) = 
	(#"\\")::(#"\"")::(fix_timed_markstr rest)
      | fix_timed_markstr (c::rest) = c::(fix_timed_markstr rest)
      | fix_timed_markstr [] = []

    fun check_mark (mark, cs) = let
	val is_timed_cs = CPN'CSTable.is_timed cs
	val ms = 
	    if is_timed_cs then " CPN'TMS.tms" else " CPN'MS.ms"
	val new_mark = 
	    case (mark,is_timed_cs) of 
		("",_) => "nil" 
	      | (_,false) => mark
	      | (_,true) => 
		cs^"'timed.input_ms (TextIO.openString \""^
		implode(fix_timed_markstr (explode mark))^"\")"
	val col_mark = 
	    case (mark,is_timed_cs) of 
		("",_) => "nil" 
	      | (_,false) => "1`"^mark
	      | (_,true) => 
		cs^"'timed.input_ms (TextIO.openString \""^
		implode(fix_timed_markstr (explode ("1`"^mark)))^"\")"

	val timed_col_mark = 
	    if is_timed_cs 
	    then cs^"'timed.input_ms (TextIO.openString \""^
		 implode(fix_timed_markstr (explode ("1`"^mark^"@"^Time.mkstr (Time.time()))))^"\")"
	    else ""

	val mark_type_ok = 	
	    ((* Remember this marking for the later change. Hack, but
	      * this is the way it was done before, and doing it this
	      * way implies less changes on the C-side *)
	     CPN'Env.use_string["val CPN'marking = ",mark," : ",cs,ms];
	     true)
	    handle _ => 
		   ((CPN'Env.use_string["val CPN'marking = ",new_mark," : ",cs,ms];
		     true)
		    handle _ => 
			   ((CPN'Env.use_string["val CPN'marking = ",col_mark," : ",cs,ms];
			     true)
			    handle _ => if is_timed_cs
					then ((CPN'Env.use_string["val CPN'marking = ",timed_col_mark," : ",cs,ms];
					       true)
					      handle _ => (CPN'Env.use_string["val CPN'marking = (",mark,")@+ ",Time.null_str,": ",cs,ms];
							   true)
					      handle _ => false)
					else false))
    in
	if mark_type_ok
	then ((CPN'Env.use_string["app (fn CPN'x => if ",cs,
				  if is_timed_cs then "'timed" else "",
				  ".legal CPN'x then () else raise Match) CPN'marking;"];
	      true)
	      handle _ => false)
	else false
    end

    fun equal_cs (cs1,cs2) =
	(CPN'CSTable.get_prime_cs cs1) = (CPN'CSTable.get_prime_cs cs2) 
	andalso
	(CPN'CSTable.is_timed cs1) = (CPN'CSTable.is_timed cs2) 

end; (* functor CPN'MakeSyntaxCheck *)
