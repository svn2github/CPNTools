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
(* File: dependency.sml
 *
 * Dependency analysis facilities. Used for incremental syntax check.
 *)

structure CPN'Dep : CPN'Dep'sig =
struct
    open Compiler.Symbol; 
	
    val VALspace_symtable : ((string,string) CPN'StringTable.hash_table) = 
	CPN'StringTable.mkTable hashString 
	(97,InternalError "Key not found in VALspace_symtable");

    val TYCspace_symtable : ((string,string) CPN'StringTable.hash_table) = 
	CPN'StringTable.mkTable hashString 
	(97,InternalError "Key not found in TYCspace_symtable");

    val SIGspace_symtable: ((string,string) CPN'StringTable.hash_table) = 
	CPN'StringTable.mkTable hashString 
	(97,InternalError "Key not found in SIGspace_symtable");

    val STRspace_symtable: ((string,string) CPN'StringTable.hash_table) = 
	CPN'StringTable.mkTable hashString 
	(97,InternalError "Key not found in STRspace_symtable");

    val FCTspace_symtable: ((string,string) CPN'StringTable.hash_table) = 
	CPN'StringTable.mkTable hashString 
	(97,InternalError "Key not found in FCTspace_symtable");

    val FIXspace_symtable: ((string,string) CPN'StringTable.hash_table) = 
	CPN'StringTable.mkTable hashString 
	(97,InternalError "Key not found in FIXspace");

    val LABspace_symtable: ((string,string) CPN'StringTable.hash_table) = 
	CPN'StringTable.mkTable hashString 
	(97,InternalError "Key not found in LABspace");

    val TYVspace_symtable: ((string,string) CPN'StringTable.hash_table) = 
	CPN'StringTable.mkTable hashString 
	(97,InternalError "Key not found in TYVspace");

    val FSIGspace_symtable: ((string,string) CPN'StringTable.hash_table) = 
	CPN'StringTable.mkTable hashString 
	(97,InternalError "Key not found in FSIGspace");

    fun symboltable sym =
	case nameSpace sym of
	    VALspace => VALspace_symtable
	  | TYCspace => TYCspace_symtable
	  | SIGspace => SIGspace_symtable
	  | STRspace => STRspace_symtable
	  | FCTspace => FCTspace_symtable
	  | FIXspace => FIXspace_symtable
	  | LABspace => LABspace_symtable
	  | TYVspace => TYVspace_symtable
	  | FSIGspace => FSIGspace_symtable;

    fun sym_tab_insert def (sym, is_constructor)
	= (CPN'StringTable.insert (symboltable sym) (name sym, def);
         if is_constructor
         then CPN'StringTable.insert TYCspace_symtable (name sym, def)
         else ());

    fun sym_tab_remove (sym, is_constructor)
	= (CPN'StringTable.remove (symboltable sym) (name sym);
         if is_constructor
         then CPN'StringTable.remove TYCspace_symtable (name sym)
         else "")
	handle InternalError str => "";

    fun sym_tab_clear ()
	= (map CPN'IdTable.clear
	   [VALspace_symtable,
	    TYCspace_symtable,
	    SIGspace_symtable,
	    STRspace_symtable,
	    FCTspace_symtable,
	    FIXspace_symtable,
	    LABspace_symtable,
	    TYVspace_symtable,
	    FSIGspace_symtable];
	   ());
	
    fun is_defined sym
	= CPN'StringTable.find (symboltable sym) (name sym);


    fun is_decl str =
      let
	  val err = ref (nil: string list);
      in
	  let
	      val src = Compiler.Source.newSource
			    ("",
			     TextIO.openString ("structure CPN'X = struct "^str^" end"),
			     false,
			     {consumer= fn s => (err:= (!err)^^[s]), 
                       flush= fn () => (), linewidth = fn () => 80})
	      val ast = Compiler.MLParser.parse src ()
            val ast = case ast of
                           (Compiler.MLParser.PARSE result) => result
                         | Compiler.MLParser.ABORT => raise Compile "Aborted parsing"
                         | Compiler.MLParser.EOF => raise Compile "EOF while parsing"
                         | Compiler.MLParser.ERROR => raise Compile "Error parsing"
            val compInfo = Compiler.Compile.mkCompInfo {source = src,
            transform = fn any => any}
            val baseEnvRefunSC = Compiler.EnvRef.pervasive;
		fun checkErrors s = 
		    if Compiler.CompInfo.anyErrors compInfo then raise Compile s else ()
		val statenv = Compiler.Environment.staticPart(Compiler.EnvRef.combined());
(*	      val {static=statenv,...} = Compiler.BareEnvironment.layerEnv((#get (Compiler.EnvRef.topLevel))(),
									   (#get
                                                         baseEnvRefunSC)())*)
	  in
		(Compiler.TopCompile.elaborate
			 {ast= ast,
			  statenv = statenv,
			  compInfo= compInfo, guid = ()}; NONE)
	       before checkErrors "Elaborate failure (is_decl)"
	  end
	      handle Compile _ => SOME(concat (!err))
                | ErrorMsg.Error => SOME(concat (!err))
      end

(*
    fun is_constructor sym
	= case CPN'StringTable.find TYCspace_symtable (name sym)
	of NONE => false 
	    | _ => true;
*)
    fun is_constructor sym
      = case (is_decl ("fun CPN'xx ("^(name sym)^" _) = ();"))
	 of NONE => true
	  | _ => false;

    val decltable : ((string, (symbol * bool) list)
			      CPN'IdTable.hash_table)
	= CPN'IdTable.mkTable hashString 
	(31,InternalError "Key not found in decltable");

    val decl_insert = CPN'IdTable.insert decltable;

    fun remove_col_var id
	= let
	      val (var,msvar) = CPN'CSTable.removeId id;
	  in
	      (app (fn v => (CPN'VarTable.remove v;
			     ())) var;
	       app (fn v => (CPN'VarTable.remove v;
			     ())) msvar;
	       ())
	  end;
	
    fun decl_remove id 
	= ((CPN'RefTable.remove id;
	    ()) handle InternalError _ => ();
	   remove_col_var id;
	   CPN'VarTable.remove_decl id;
	   CPN'IdTable.remove decltable id)
	handle InternalError _ => []; 
        
    fun decl_list ()
	= map (fn (st,lst) => (st,map (fn (sym,_) => name sym) lst)) 
	(CPN'IdTable.listItemsi decltable);
	
    fun decl_clear () = CPN'IdTable.filter (fn _ => false) decltable;

    fun only_one_of_each [] = []
      | only_one_of_each ((ee,be)::es) = 
	let
	    fun remove [] = []
	      | remove ((f,bf)::fs) = 
		if (name ee)=(name f) andalso (nameSpace ee)=(nameSpace f) 
		    then remove fs
		else (f,bf)::(remove fs)
	in
	    (ee,be)::(only_one_of_each (remove es))
	end;
	

    fun insert_defs (id, []) = ()
      | insert_defs (id, defined)
	= let
	      val unique_defined = only_one_of_each defined
	  in 
	      (decl_insert (id, unique_defined);
	       map (sym_tab_insert id) unique_defined;
	       ())
	  end;

    fun remove_defs id
	= let
	      val symbs = decl_remove id
	  in
	      map sym_tab_remove symbs;
	      map #1 symbs
	  end;

    fun clear_defs ()
	= (decl_clear ();
	   sym_tab_clear ());
		
    fun insert str (st::rest)
	= if str=st then (st::rest) else st::(insert str rest)
      | insert str [] = [str];
	
    fun to_use (sym::rest) res
	= to_use rest 
	    (case is_defined sym
	       of (SOME str) => insert str res
	        | NONE => res)
      | to_use [] res = res;
		   
    fun sym_tab_list ()
	= ((CPN'StringTable.listItemsi VALspace_symtable)^^
	   (CPN'StringTable.listItemsi TYCspace_symtable)^^
	   (CPN'StringTable.listItemsi SIGspace_symtable)^^
	   (CPN'StringTable.listItemsi STRspace_symtable)^^
	   (CPN'StringTable.listItemsi FCTspace_symtable)^^
	   (CPN'StringTable.listItemsi FIXspace_symtable)^^
	   (CPN'StringTable.listItemsi LABspace_symtable)^^
	   (CPN'StringTable.listItemsi TYVspace_symtable));

    fun makeSymbol (str::_) = str;
	

	  open Ast;

	  fun is_new known elm
	      = not (List.exists (fn x => x=(makeSymbol elm)) known);
	      
	  fun fixitem_exp_trav ({fixity, item, region}: exp fixitem) known
	      = exp_trav item known

	  and fixitem_pat_trav ({fixity, item, region}: pat fixitem) known
	      = pat_trav item known

	  and fixitem_pat_trav_constraint ({fixity, item, region}:
					   pat fixitem) known
	      = pat_trav_constraint item known

	  and fixitem_pats_trav (item::rest) known
	      = (fixitem_pat_trav item known)^^(fixitem_pats_trav rest known)
	    | fixitem_pats_trav [] _ = []

	  and fixitem_pats_trav_constraint (item::rest) known
	      = (fixitem_pat_trav_constraint item known)^^
	      (fixitem_pats_trav_constraint rest known)
	    | fixitem_pats_trav_constraint [] _ = []

    
	  and rules_trav ((Rule {exp, pat})::rules) known
	      = (exp_trav exp (pat_trav pat known))^^
              (pat_trav_constraint pat known)^^
              (rules_trav rules known)
	    | rules_trav [] _ = []
	      
	  and exp_trav (AndalsoExp (exp1, exp2)) known
	      = (exp_trav exp1 known)^^(exp_trav exp2 known)
	    | exp_trav (AppExp  {argument, function}) known
	      = (exp_trav argument known)^^(exp_trav function known)
	    | exp_trav (CaseExp {expr, rules}) known
	      = (exp_trav expr known)^^(rules_trav rules known)
	    | exp_trav (ConstraintExp {constraint, expr}) known
	      = (exp_trav expr known)^^
	      (ty_trav constraint known)
	    | exp_trav (FlatAppExp (exp::exp_list)) known
	      = (fixitem_exp_trav exp known)^^
	      (exp_trav (FlatAppExp exp_list) known)
	    | exp_trav (FlatAppExp []) _ = []
	    | exp_trav (FnExp rules) known = rules_trav rules known
	    | exp_trav (HandleExp {expr, rules}) known
	      = (exp_trav expr known)^^(rules_trav rules known)
	    | exp_trav (IfExp {elseCase, test, thenCase}) known
	      = (exp_trav elseCase known)^^(exp_trav test known)
	      ^^(exp_trav thenCase known)
	    | exp_trav (LetExp {dec, expr}) known
	      = (dec_trav dec known)^^
	        (exp_trav expr (known^^(map #1 (top_def dec))))
	    | exp_trav (ListExp (exp::exps)) known
	      = (exp_trav exp known)^^(exp_trav (ListExp exps) known)
	    | exp_trav (ListExp []) _ = []
	    | exp_trav (MarkExp (exp, region)) known = exp_trav exp known
	    | exp_trav (OrelseExp (exp1, exp2)) known
	      = (exp_trav exp1 known)^^(exp_trav exp2 known)
	    | exp_trav (RaiseExp exp) known = exp_trav exp known
	    | exp_trav (RecordExp ((symbol, exp)::rest)) known
	      = (*[makeSymbol [symbol]]^^ This is the lable, so it should not be recorded *)
	        (exp_trav exp known)^^(exp_trav (RecordExp rest) known)
	    | exp_trav (RecordExp []) _ = []
	    | exp_trav (SelectorExp symbol) known
	      = [makeSymbol [symbol]]
	    | exp_trav (SeqExp (exp::exps)) known
	      = (exp_trav exp known)^^(exp_trav (SeqExp exps) known)
	    | exp_trav (SeqExp []) _ = []
	    | exp_trav (TupleExp (exp::exps)) known
	      = (exp_trav exp known)^^(exp_trav (TupleExp exps) known)
	    | exp_trav (TupleExp []) _ = []
	    | exp_trav (VarExp path) known
	      = if is_new known path (* SC *)
		    then [makeSymbol path]
		else []
	    | exp_trav (VectorExp (exp::exps)) known
	      = (exp_trav exp known)^^(exp_trav (VectorExp exps) known)
	    | exp_trav (VectorExp []) _ = []
	    | exp_trav (WhileExp {expr, test}) known
	      = (exp_trav expr known)^^(exp_trav test known)
	    | exp_trav (CharExp _) _ = []
	    | exp_trav (IntExp _) _ = []
	    | exp_trav (RealExp _) _ = []
	    | exp_trav (StringExp _) _ = []
	    | exp_trav (WordExp _) _ = []

	  and exps_trav (exp::rest) known
	      = (exp_trav exp known)^^(exps_trav rest known)
	    | exps_trav [] _ = []

          and pat_trav_record ((_,pat)::rest) known
              =  pat_trav_record rest (pat_trav pat known)
            | pat_trav_record [] known = known

	  and pat_trav (AppPat {argument, constr}) known
	      = (pat_trav argument known)^^(pat_trav constr known)
	    | pat_trav (CharPat string) known = known
	    | pat_trav (ConstraintPat {constraint, pattern}) known
	      = pat_trav pattern known
	    | pat_trav (FlatAppPat pat_fixitem_list) known
	      = fixitem_pats_trav pat_fixitem_list known
	    | pat_trav (IntPat literal) known = known
	    | pat_trav (LayeredPat {expPat, varPat}) known
	      = (pat_trav expPat known)^^(pat_trav varPat known)
	    | pat_trav (ListPat (pat::pat_list)) known
	      = (pat_trav pat known)^^(pat_trav (ListPat pat_list) known)
	    | pat_trav (ListPat []) known = known
	    | pat_trav (MarkPat (pat, region)) known = pat_trav pat known
	    | pat_trav (OrPat (pat::pat_list)) known
	      = (pat_trav pat known)^^(pat_trav (OrPat pat_list) known)
	    | pat_trav (OrPat []) known = known
	    | pat_trav (RecordPat {def, flexibility}) known
              = pat_trav_record def known 
	    | pat_trav (StringPat _) known = known
	    | pat_trav (TuplePat (pat::pat_list)) known
	      = (pat_trav pat known)^^(pat_trav (TuplePat pat_list) known)
	    | pat_trav (TuplePat []) known  = known
	    | pat_trav (VarPat path) known = (makeSymbol path)::known
	    | pat_trav (VectorPat (pat::pat_list)) known
	      = (pat_trav pat known)^^(pat_trav (VectorPat pat_list) known)
	    | pat_trav (VectorPat []) known = known
	    | pat_trav WildPat known = known
	    | pat_trav (WordPat literal) known = known

	  and pat_trav_constraint (AppPat {argument, constr}) known
	      = (pat_trav_constraint argument known)^^
	      (pat_trav_constraint constr known)
	    | pat_trav_constraint (CharPat string) known = []
	    | pat_trav_constraint (ConstraintPat {constraint, pattern}) known
	      = (pat_trav_constraint pattern known)^^
	        (ty_trav constraint known)
	    | pat_trav_constraint (FlatAppPat pat_fixitem_list) known
	      = fixitem_pats_trav_constraint pat_fixitem_list known
	    | pat_trav_constraint (IntPat literal) known = []
	    | pat_trav_constraint (LayeredPat {expPat, varPat}) known
	      = (pat_trav_constraint expPat known)^^
	      (pat_trav_constraint varPat known)
	    | pat_trav_constraint (ListPat (pat::pat_list)) known
	      = (pat_trav_constraint pat known)^^
	      (pat_trav_constraint (ListPat pat_list) known)
	    | pat_trav_constraint (ListPat []) known = []
	    | pat_trav_constraint (MarkPat (pat, region)) known
	      = pat_trav_constraint pat known
	    | pat_trav_constraint (OrPat (pat::pat_list)) known
	      = (pat_trav_constraint pat known)^^
	      (pat_trav_constraint (OrPat pat_list) known)
	    | pat_trav_constraint (OrPat []) known = []
	    | pat_trav_constraint (RecordPat {def, flexibility}) known
	      = []
	    | pat_trav_constraint (StringPat _) known = []
	    | pat_trav_constraint (TuplePat (pat::pat_list)) known
	      = (pat_trav_constraint pat known)^^
	      (pat_trav_constraint (TuplePat pat_list) known)
	    | pat_trav_constraint (TuplePat []) known  = []
	    | pat_trav_constraint (VarPat path) known  =
		  if (is_constructor (makeSymbol path))
		      then [makeSymbol path]
		  else [] 
	    | pat_trav_constraint (VectorPat (pat::pat_list)) known
	      = (pat_trav_constraint pat known)^^
	      (pat_trav_constraint (VectorPat pat_list) known)
	    | pat_trav_constraint (VectorPat []) known = []
	    | pat_trav_constraint WildPat known = []
	    | pat_trav_constraint (WordPat literal) known = []

	  and(* vb_trav (LVb {exp, pat}) known
	      = exp_trav exp (pat_trav pat known)
	    |*) vb_trav (MarkVb (vb, region)) known = vb_trav vb known
	    | vb_trav (Vb {exp, pat, lazyp}) known = exp_trav exp (pat_trav pat known)

	  and vbs_trav [] known = []
	    | vbs_trav (vb::rest) known
	      = (vb_trav vb known)^^(vbs_trav rest known)

	  and (*rvb_trav (LRvb {exp, fixity, resultty, var}) known
	      = exp_trav exp known
	    |*) rvb_trav (MarkRvb (rvb, region)) known = rvb_trav rvb known
	    | rvb_trav (Rvb {exp, fixity, resultty, var, lazyp}) known
	      = exp_trav exp known

	  and rvbs_trav [] known = []
	    | rvbs_trav (rvb::rest) known
	      = (rvb_trav rvb known)^^(rvbs_trav rest known)

	  and strb_trav (MarkStrb (strb,region)) known = strb_trav strb known
	    | strb_trav (Strb {constraint, def, name}) known
	      = (strexp_trav def ((makeSymbol [name])::known))^^
                (sig_constraint_trav constraint ((makeSymbol [name])::known))

	  and strexp_bool_trav [] _ = []
	    | strexp_bool_trav ((strexp,_)::strexp_bool) known
	      = (strexp_trav strexp known)^^
	        (strexp_bool_trav strexp_bool known)
          
          and sig_constraint_trav (Transparent constrain) known
              = sigexp_trav constrain known
            | sig_constraint_trav _ _ = []
	      
	  and strexp_trav (MarkStr (strexp, region)) known
	      = strexp_trav strexp known
	    | strexp_trav (VarStr path) known = [makeSymbol path]
	    | strexp_trav (BaseStr dec) known = dec_trav dec known
	    | strexp_trav (ConstrainedStr (strexp, constrain)) known
	      = (strexp_trav strexp known)^^(sig_constraint_trav constrain known)
	    | strexp_trav (LetStr (dec, strexp)) known
	      = (dec_trav dec known)^^(strexp_trav strexp known)
	    | strexp_trav (AppStr (path, strexp_bool)) known
	      = (makeSymbol path)::(strexp_bool_trav strexp_bool known)
	    | strexp_trav (AppStrI (path, _)) known = [makeSymbol path]

	  and tyvar_trav (MarkTyv (tyvar, region)) known
	      = tyvar_trav tyvar known
	    | tyvar_trav (Tyv symbol) known = [makeSymbol [symbol]]

	  and tyvars_trav (tyv::rest) known
	      = (tyvar_trav tyv known)^^(tyvars_trav rest known)
	    | tyvars_trav [] _ = []

	  and clause_trav (Clause {exp, pats, resultty = SOME resultty}) known
	      = let
		    val known' = fixitem_pats_trav pats known
		in
		    (exp_trav exp known')^^(ty_trav resultty known')^^
		    (fixitem_pats_trav_constraint pats known)
		end
	    | clause_trav (Clause {exp, pats, resultty = NONE}) known
	      = let
		    val known' = fixitem_pats_trav pats known
		in
		    (exp_trav exp known')^^
		    (fixitem_pats_trav_constraint pats known)
		end

	  and clauses_trav (cl::rest) known
	      = (clause_trav cl known)^^(clauses_trav rest known)
	    | clauses_trav [] _ = []

	  and fb_trav (Fb (clause_list, _)) known = clauses_trav clause_list known
(*	    | fb_trav (LFb clause_list) known = clauses_trav clause_list known*)
	    | fb_trav (MarkFb (fb, _)) known = fb_trav fb known

	  and fbs_trav (fb::rest) known
	      = (fb_trav fb known)^^(fbs_trav rest known)
	    | fbs_trav [] _ = []

	  and ty_trav (ConTy (path,ty_list)) known
	      = (if is_new known path
		     then [makeSymbol path]
		 else [])^^
	      (tys_trav ty_list known)
	    | ty_trav (MarkTy (ty, region)) known = ty_trav ty known
	    | ty_trav (RecordTy ((symbol, ty)::rest)) known
	      = (ty_trav ty known)^^(ty_trav (RecordTy rest) known)
	    | ty_trav (RecordTy []) known = []
	    | ty_trav (TupleTy (ty::rest)) known
	      = (ty_trav ty known)^^(ty_trav (TupleTy rest) known)
	    | ty_trav (TupleTy []) known = []
	    | ty_trav (VarTy tyvar) known = tyvar_trav tyvar known

	  and tys_trav (ty::rest) known
	      = (ty_trav ty known)^^(tys_trav rest known)
	    | tys_trav [] _ = []

	  and dbrhs_trav ((symbol, NONE)::rest) known
	      = dbrhs_trav rest known
	    | dbrhs_trav ((symbol, SOME ty)::rest) known
	      = (ty_trav ty known)^^(dbrhs_trav rest known)
	    | dbrhs_trav [] known = []
	    (*| dbrhs_trav path known
	      = if is_new known path
		    then [makeSymbol path]
		else []*)

	  and db_trav (Db {rhs, tyc, tyvars, lazyp}) known = dbrhs_trav rhs known
(*	    | db_trav (LDb {rhs, tyc, tyvars}) known = dbrhs_trav rhs known*)
	    | db_trav (MarkDb (db, region)) known = db_trav db known

	  and dbs_trav (db::rest) known
	      = (db_trav db known)^^(dbs_trav rest known)
	    | dbs_trav [] _ = []

	  and eb_trav (EbDef {edef=path, exn}) known
	      = if is_new known path
		    then [makeSymbol path]
		else []
	    | eb_trav (EbGen {etype=NONE, exn}) known = []
	    | eb_trav (EbGen {etype=(SOME ty), exn}) known = ty_trav ty known 
	    | eb_trav (MarkEb (eb, region)) known = eb_trav eb known

	  and ebs_trav (db::rest) known
	      = (eb_trav db known)^^(ebs_trav rest known)
	    | ebs_trav [] _ = []

	  and tb_trav (MarkTb (tb, region)) known = tb_trav tb known
	    | tb_trav (Tb {def, tyc, tyvars}) known = ty_trav def known

	  and tbs_trav (tb::rest) known
	      = (tb_trav tb known)^^(tbs_trav rest known)
	    | tbs_trav [] _ = []

          and basesig_trav (ValSpec ((_,tyl)::rest)) known
              = (ty_trav tyl known)^^(basesig_trav (ValSpec rest) known)
            | basesig_trav _ _ = []

	  and basesigs_trav (tb::rest) known
	      = (basesig_trav tb known)^^(basesigs_trav rest known)
	    | basesigs_trav [] _ = []

	  and sigexp_trav (AugSig (sigexp, wherespec_list)) known = []
	    | sigexp_trav (BaseSig spec_list) known
              = basesigs_trav spec_list known
	    | sigexp_trav (MarkSig (sigexp, region)) known
	      = sigexp_trav sigexp known
	    | sigexp_trav (VarSig symbol) known
              = if is_new known [symbol]
		    then [makeSymbol [symbol]]
		else []

	  and fctexp_trav (AppFct (path, (strexp, bool)::rest, fsigexp)) known
	      = (strexp_trav strexp known)^^
	      (fctexp_trav (AppFct (path, rest, fsigexp)) known)
	    | fctexp_trav (AppFct (path, [], fsigexp)) known = []
	    | fctexp_trav (BaseFct {body, constraint, 
				    params=((symbol_option, sigexp)::list)})
	      known
	      = (strexp_trav body known)^^(sigexp_trav sigexp known)
	    | fctexp_trav (BaseFct {body, constraint, params=[]}) known = []
	    | fctexp_trav (LetFct (dec, fctexp)) known
	      = (dec_trav dec known)^^(fctexp_trav fctexp known)
	    | fctexp_trav (MarkFct (fctexp, region)) known 
	      = fctexp_trav fctexp known
	    | fctexp_trav (VarFct (path, fsigexp)) known = []

	  and fctb_trav (Fctb {def, name}) known = fctexp_trav def known
	    | fctb_trav (MarkFctb (fctb, region)) known = fctb_trav fctb known

	  and sigb_trav (MarkSigb (sigb, region)) known = sigb_trav sigb known
	    | sigb_trav (Sigb {def, name}) known = sigexp_trav def known

	  and fsigb_trav (Fsigb {def, name}) known = []
	    | fsigb_trav (MarkFsigb (fsig, region)) known
	      = fsigb_trav fsig known

	  and dec_trav (AbsDec (strb::strb_list)) known
	      = (strb_trav strb known)^^(dec_trav (AbsDec strb_list) known)
	    | dec_trav (AbsDec []) _ = []
	    | dec_trav (StrDec (strb::strb_list)) known
	      = (strb_trav strb known)^^(dec_trav (StrDec strb_list) known)
	    | dec_trav (StrDec []) _ = []
	    | dec_trav (SeqDec (dec::dec_list)) known

	      = (dec_trav dec known)^^
                (dec_trav (SeqDec dec_list) ((map #1 (top_def dec))^^known))

	    | dec_trav (SeqDec []) _ = []
	    | dec_trav (ValDec (vb_list, tyvar_list)) known
	      = (vbs_trav vb_list known)^^(tyvars_trav tyvar_list known)
	    | dec_trav (ValrecDec (rvb_list, tyvar_list)) known
	      = (rvbs_trav rvb_list known)^^(tyvars_trav tyvar_list known)
	    | dec_trav (MarkDec (dec, region)) known = dec_trav dec known
	    | dec_trav (FunDec (fb_list,tyvar_list)) known
	      = (fbs_trav fb_list known)^^(tyvars_trav tyvar_list known)

(*	    | dec_trav (OpenDec (path::rest)) known
	      = (if is_new known path 
		     then [makeSymbol path]
		 else [])^^
	      (dec_trav (OpenDec rest) known)
	    | dec_trav (OpenDec []) known = []                                   *)

            | dec_trav (OpenDec _) known = raise Compile "'open' not allowed in declarations"

	    | dec_trav (DatatypeDec {datatycs, withtycs}) known
	      = dbs_trav datatycs known
	    | dec_trav (AbstypeDec {abstycs, body, withtycs}) known
	      = (dbs_trav abstycs known)^^(dec_trav body known)
	    | dec_trav (TypeDec tb_list) known = tbs_trav tb_list known
	    | dec_trav (ExceptionDec eb_list) known = ebs_trav eb_list known
(*	    | dec_trav (ImportDec string_list) known = []*)
	    | dec_trav (OvldDec (symbol, ty, exp_list)) known
	      = (ty_trav ty known)^^(exps_trav exp_list known)
	    | dec_trav (FctDec (fctb::rest)) known
	      = (fctb_trav fctb known)^^(dec_trav (FctDec rest) known)
	    | dec_trav (FctDec []) known = []
	    | dec_trav (FixDec {fixity, ops}) known = []
	    | dec_trav (SigDec (sigb::list)) known
	      = (sigb_trav sigb known)^^(dec_trav (SigDec list) known)
	    | dec_trav (SigDec []) known = []
	    | dec_trav (FsigDec (fsigb::list)) known
	      = (fsigb_trav fsigb known)^^(dec_trav (FsigDec list) known)
	    | dec_trav (FsigDec []) known = []
	    | dec_trav (LocalDec (ldec, dec)) known
	      =(let 
		    val known = (map #1 (top_def ldec))^^known
		in
		    (dec_trav ldec known)^^(dec_trav dec known)
		end)

	  and top_fixitem_pat_val ({fixity = NONE, item, region}: pat fixitem)
	      = top_pat item
	    | top_fixitem_pat_val ({fixity = SOME symb, item, region}: pat fixitem)
	      = if is_constructor symb
	        then []
		else top_pat item

	  and top_fixitem_pat_fun ({fixity, item, region}: pat fixitem)
	      = top_pat item

	  and top_fixitem_pats_val (item::rest)
	      = (top_fixitem_pat_val item)^^(top_fixitem_pats_val rest)
	    | top_fixitem_pats_val [] = []

	  and top_fixitem_pats_fun (item::rest)
	      = top_fixitem_pat_fun item
	    | top_fixitem_pats_fun [] = []

	  and top_fixitem_pats_cons (item::rest)
	      = top_fixitem_pats_val (item::rest)
	    | top_fixitem_pats_cons [] = []
				    
	  and top_pat (AppPat {argument, constr})
	      = (top_pat (argument))^^(top_pat (constr))
	    | top_pat (CharPat string)
	      = []
	    | top_pat (ConstraintPat {constraint, pattern})
	      = top_pat (pattern)
	    | top_pat (FlatAppPat pat_fixitem_list)
	      = top_fixitem_pats_cons pat_fixitem_list
	    | top_pat (IntPat literal)
	      = []
	    | top_pat (LayeredPat {expPat, varPat})
	      = (top_pat (expPat))^^(top_pat (varPat))
	    | top_pat (ListPat (pat::pat_list))
	      = (top_pat (pat))^^(top_pat (ListPat pat_list))
	    | top_pat (ListPat [])
	      = []
	    | top_pat (MarkPat (pat, region))
	      = top_pat (pat)
	    | top_pat (OrPat (pat::pat_list))
	      = (top_pat (pat))^^(top_pat (OrPat pat_list))
	    | top_pat (OrPat [])
	      = []
	    | top_pat (RecordPat {def=((_,pat)::rest), flexibility})
	      = (top_pat (pat))^^(top_pat (RecordPat {def=rest, flexibility=flexibility}))
	    | top_pat (RecordPat {def=[], flexibility})
	      = []
	    | top_pat (StringPat _)
	      = []
	    | top_pat (TuplePat (pat::pat_list))
	      = (top_pat (pat))^^(top_pat (TuplePat pat_list))
	    | top_pat (TuplePat [])
	      = []
	    | top_pat (VarPat path)
	      = [(makeSymbol path, false)]
	    | top_pat (VectorPat (pat::pat_list))
	      = (top_pat (pat))^^(top_pat (VectorPat pat_list))
	    | top_pat (VectorPat [])
	      = []
	    | top_pat (WildPat)
	      = []
	    | top_pat (WordPat literal)
	      = []

	  and (*top_vb (LVb {exp, pat}) = top_pat pat
	    |*) top_vb (MarkVb (vb, region)) = top_vb vb
	    | top_vb (Vb {exp, pat, lazyp}) = top_pat pat

	  and top_vbs [] = []
	    | top_vbs (vb::rest) = (top_vb vb)^^(top_vbs rest)

	  and (*top_rvb (LRvb {exp, fixity, resultty, var}) = [(makeSymbol [var], false)]
	    |*) top_rvb (MarkRvb (rvb, region)) = top_rvb rvb 
	    | top_rvb (Rvb {exp, fixity, resultty, var, lazyp}) = [(makeSymbol [var], false)]

	  and top_rvbs [] = []
	    | top_rvbs (rvb::rest) = (top_rvb rvb)^^(top_rvbs rest)

	  and top_clause (Clause {exp, pats, resultty}) = top_fixitem_pats_fun pats

	  and top_clauses (cl::rest) = (top_clause cl)^^(top_clauses rest)
	    | top_clauses [] = []

	  and top_fb (Fb (clause_list, _)) = top_clauses clause_list
(*	    | top_fb (LFb clause_list) = top_clauses clause_list*)
	    | top_fb (MarkFb (fb, _)) = top_fb fb

	  and top_fbs (fb::rest) = (top_fb fb)^^(top_fbs rest)
	    | top_fbs [] = []

	  and top_dbrhs ((symbol, _)::rest)
	      = (makeSymbol [symbol], true)::(top_dbrhs rest)
	    | top_dbrhs [] = []
	    (*| top_dbrhs (Repl path) = [(makeSymbol path, false)]*)

	  and top_db (Db {rhs, tyc, tyvars, lazyp}) = 
	      (makeSymbol [tyc], false)::(top_dbrhs rhs)
(*	    | top_db (LDb {rhs, tyc, tyvars}) = 
	      (makeSymbol [tyc], false)::(top_dbrhs rhs)*)
	    | top_db (MarkDb (db, region)) = top_db db

	  and top_dbs (db::rest) = (top_db db)^^(top_dbs rest)
	    | top_dbs [] = []

	  and top_tb (MarkTb (tb, region)) = top_tb tb
	    | top_tb (Tb {def, tyc, tyvars}) = [(makeSymbol [tyc], false)]

	  and top_tbs (tb::rest) = (top_tb tb)^^(top_tbs rest)
	    | top_tbs [] = []

	  and top_eb (EbDef {edef=path, exn}) = [(makeSymbol [exn], false)]
	    | top_eb (EbGen {etype, exn}) = [(makeSymbol [exn], false)]
	    | top_eb (MarkEb (eb, region)) = top_eb eb

	  and top_ebs (db::rest) = (top_eb db)^^(top_ebs rest)
	    | top_ebs [] = []

	  and top_fctb (Fctb {def, name}) = [(makeSymbol [name], false)]
	    | top_fctb (MarkFctb (fctb, region)) = top_fctb fctb

	  and top_sigb (MarkSigb (sigb, region)) = top_sigb sigb
	    | top_sigb (Sigb {def, name}) = [(makeSymbol [name], false)]

	  and top_fsigb (Fsigb {def, name}) = [(makeSymbol [name], false)]
	    | top_fsigb (MarkFsigb (fsig, region)) = top_fsigb fsig

	  and top_strb (MarkStrb (strb,region)) = top_strb strb 
	    | top_strb (Strb {constraint, def, name}) 
	      = (makeSymbol [name], false)

	  and top_def (AbsDec ((Strb {constraint, def, name})::strb_list))
	      = (makeSymbol [name], false)::(top_def (AbsDec strb_list))
	    | top_def (AbsDec []) = []
	    | top_def (StrDec (strb::strb_list))
	      = (top_strb strb)::(top_def (StrDec strb_list))
	    | top_def (StrDec []) = []
	    | top_def (SeqDec (dec::dec_list))
	      = (top_def dec)^^(top_def (SeqDec dec_list))
	    | top_def (SeqDec []) = []
	    | top_def (ValDec (vb_list, tyvar_list)) = top_vbs vb_list
	    | top_def (MarkDec (dec, region))  = top_def dec 
	    | top_def (ValrecDec (rvb_list, tyvar_list)) = top_rvbs rvb_list
	    | top_def (FunDec (fb_list,tyvar_list)) = top_fbs fb_list
	    | top_def (OpenDec path_list) = [] 
	    (* not handled !!, i.e., the dependency might
	     be a bit too strong *)
	    | top_def (DatatypeDec {datatycs, withtycs}) = top_dbs datatycs
	    | top_def (AbstypeDec {abstycs, body, withtycs}) = []
	    (* not handled !! *)
	    | top_def (TypeDec tb_list) = top_tbs tb_list
	    | top_def (ExceptionDec eb_list) = top_ebs eb_list
(*	    | top_def (ImportDec string_list) = []*)
	    | top_def (OvldDec (symbol, ty, exp_list)) = [(makeSymbol [symbol], false)]
	    | top_def (FctDec (fctb::rest))
	      = (top_fctb fctb)^^(top_def (FctDec rest))
	    | top_def (FctDec []) = []
	    | top_def (FixDec {fixity, ops}) = []
	    | top_def (SigDec (sigb::list))
	      = (top_sigb sigb)^^(top_def (SigDec list))
	    | top_def (SigDec []) = []
	    | top_def (FsigDec (fsigb::list))
	      = (top_fsigb fsigb)^^(top_def (FsigDec list))
	    | top_def (FsigDec []) = []
	    | top_def (LocalDec (ldec, dec)) = top_def dec
	    | top_def _ = []

	  fun exp_ast (ConstraintExp {constraint, expr})
	      = exp_ast expr
	    | exp_ast (MarkExp (exp, region)) = exp_ast exp
          | exp_ast (AndalsoExp (exp1, exp2))
	      = concat ["<and>", (exp_ast exp1), (exp_ast exp2), "</and>"]
	    | exp_ast (OrelseExp (exp1, exp2))
	      = concat ["<or>", (exp_ast exp1), (exp_ast exp2), "</or>"]
	    | exp_ast (IfExp {elseCase, test, thenCase})
	      = concat ["<if><else>",(exp_ast elseCase), "</else><test>",
                        (exp_ast test), "</test><then>", (exp_ast thenCase), "</then></if>" ]
	    | exp_ast (FlatAppExp ([{fixity = NONE, item, region}]))
	      = exp_ast item
	    | exp_ast (FlatAppExp ([{fixity = SOME (symbol1),
					    item = MarkExp (VarExp [symbol2], _),
					    region = _}]))
	      = (if (name symbol1) = (name symbol2) then
		     (case (name symbol1) of
		       "empty" => "<empty/>"
		     | _ => concat ["<var>", (name symbol1), "</var>"])
		  else
		     "<unknown and=\"error\"/>")
	    | exp_ast (FlatAppExp ([{fixity = NONE, item = item1, region = _},
	                            {fixity = SOME (symbol1),
					    item = MarkExp (VarExp [symbol2], _),
					    region = _},
					    {fixity = NONE, item = item3, region = _}]))
	      = (if (name symbol1) = (name symbol2) then
		     (case (name symbol1) of
		       "`" => concat ["<multiset><cardinality>", (exp_ast item1),
				      "</cardinality><value>", (exp_ast item3), "</value></multiset>"]
		     | "++" => concat [(exp_ast item1), (exp_ast item3)]
		     | "=" => concat ["<equals>", (exp_ast item1), (exp_ast item3), "</equals>"]
		     | "+" => concat ["<add>", (exp_ast item1), (exp_ast item3), "</add>"]
		     | _ => "<unknown/>")
		  else
		     "<unknown and=\"error\"/>")
	    | exp_ast (SeqExp (exp::exps))
	      = concat [(exp_ast exp), (exp_ast (SeqExp exps))]
	    | exp_ast (SeqExp []) = ""
	    | exp_ast (IntExp value) = concat ["<int>", (IntInf.toString value), "</int>"]
	    | exp_ast (ListExp (exp::exps))
	      = concat [(exp_ast exp), (exp_ast (ListExp exps))]
	    | exp_ast (ListExp [])  = ""
	    | exp_ast _ = "<unknown/>"
(*
	    | exp_ast (AppExp  {argument, function})
	      = (exp_ast argument)^^(exp_ast function)
	    | exp_ast (CaseExp {expr, rules})
	      = (exp_ast expr)^^(rules_trav rules)
	    | exp_ast (FnExp rules) = rules_trav rules
	    | exp_ast (HandleExp {expr, rules})
	      = (exp_ast expr)^^(rules_trav rules)
	    | exp_ast (LetExp {dec, expr})
	      = (dec_trav dec)^^
	        (exp_ast expr (known^^(map #1 (top_def dec))))
	    | exp_ast (OrelseExp (exp1, exp2))
	      = (exp_ast exp1)^^(exp_ast exp2)
	    | exp_ast (RaiseExp exp) = exp_ast exp
	    | exp_ast (RecordExp ((symbol, exp)::rest))
	      = [makeSymbol [symbol]]^^(exp_ast exp)^^
	      (exp_ast (RecordExp rest))
	    | exp_ast (RecordExp []) _ = []
	    | exp_ast (SelectorExp symbol)
	      = [makeSymbol [symbol]]
	    | exp_ast (SeqExp (exp::exps))
	      = (exp_ast exp)^^(exp_ast (SeqExp exps))
	    | exp_ast (SeqExp []) _ = []
	    | exp_ast (TupleExp (exp::exps))
	      = (exp_ast exp)^^(exp_ast (TupleExp exps))
	    | exp_ast (TupleExp []) _ = []
	    | exp_ast (VarExp path)
	      = if is_new path (* SC *)
		    then [makeSymbol path]
		else []
	    | exp_ast (VectorExp (exp::exps))
	      = (exp_ast exp)^^(exp_ast (VectorExp exps))
	    | exp_ast (VectorExp []) _ = []
	    | exp_ast (WhileExp {expr, test})
	      = (exp_ast expr)^^(exp_ast test)
	    | exp_ast (CharExp _) = []
	    | exp_ast (RealExp _) = []
	    | exp_ast (StringExp _) = []
	    | exp_ast (WordExp _) = []
*)

	  fun remove_outer_struct 
	      (SeqDec [MarkDec
		       (StrDec [MarkStrb
				(Strb
				 {constraint=NoSig,
				  name=name,
				  def=MarkStr (BaseStr inner, region)},_)],_)])
	      = inner
	    | remove_outer_struct 
	      (MarkDec
		       (StrDec [MarkStrb
				(Strb
				 {constraint=NoSig,
				  name=name,
				  def=MarkStr (BaseStr inner, region)},_)],_))
	      = inner
	    | remove_outer_struct str = str

	  fun remove_outer_function 
	      (MarkDec
		       (FunDec ([MarkFb
				(Fb
                         ([Clause
				    { exp = MarkExp (FlatAppExp [
				      {fixity=NONE, item=MarkExp (
					  TupleExp[ inner,
					  FlatAppExp [ {
					    fixity=NONE,
					    item=MarkExp (RecordExp [], _), ... }]], _), ...}], _), ... }
				 ], _), _)], _), _))
	      = inner
	    | remove_outer_function
	      (MarkDec
		       (FunDec ([MarkFb
				(Fb
                         ([Clause
				    { exp = MarkExp (
 FlatAppExp
   [{fixity=NONE,
     item=MarkExp
            (SeqExp
               [FlatAppExp
                  [{fixity=NONE,
                    item=MarkExp
                           (SeqExp
                              [inner],
                            _),...}],
                FlatAppExp
                  [{fixity=NONE,item=MarkExp (RecordExp [],_),
                    ...}]],_),...}]
				    , _), ... }
				 ], _), _)], _), _))
	      = inner
	    | remove_outer_function
	      (MarkDec
		       (FunDec ([MarkFb
				(Fb
                         ([Clause
				    { exp = MarkExp (inner, _), ... }
				 ], _), _)], _), _))
                         = inner
          | remove_outer_function (SeqDec [dec]) = remove_outer_function dec
	      
fun find_dependencies (id,str)
    = let

	  val err = ref (nil: string list);
	      
	  val source = Compiler.Source.newSource
	      ("",

	       TextIO.openString (concat["structure CPN'X = struct ",
					 str," end"]), false,
	       {consumer = fn s => (err:= s::(!err)), 
		flush = fn () => (), 
            linewidth = fn () => 80});
	  
	val ast = Compiler.MLParser.parse source ()

        val ast = case ast of
                       (Compiler.MLParser.PARSE result) => result
                     | Compiler.MLParser.ABORT => raise Compile "Aborted parsing"
                     | Compiler.MLParser.EOF => raise Compile "EOF while parsing"
                     | Compiler.MLParser.ERROR => raise Compile "Error parsing"

	val ast = remove_outer_struct ast

	  val defined = top_def ast

	  val used = to_use (dec_trav ast []) []
	      	      
	  val overwrite = to_use (map #1 defined) []

      in
	  insert_defs (id,defined);
	  (used,overwrite)
      end

fun find_use str
    = let

	  val err = ref (nil: string list);
	      
	  val source = Compiler.Source.newSource
	      ("",

	       TextIO.openString (concat["structure CPN'X = struct ",
					 str," end"]),
               false,
	       {consumer = fn s => (err:= s::(!err)), 
		flush = fn () => (), 
            linewidth = fn () => 80});
	  
	val ast = Compiler.MLParser.parse source ()

        val ast = case ast of
                       (Compiler.MLParser.PARSE result) => result
                     | Compiler.MLParser.ABORT => raise Compile "Aborted parsing"
                     | Compiler.MLParser.EOF => raise Compile "EOF while parsing"
                     | Compiler.MLParser.ERROR => raise Compile "Error parsing"

	val ast = remove_outer_struct ast
      in
	  to_use (dec_trav ast []) []
      end;

fun find_ast str
    = let

	  val err = ref (nil: string list);
	      
	  val source = Compiler.Source.newSource
	      ("",
	       TextIO.openString (concat["structure CPN'X = struct ",
					 str," end"]), 
	       false,
	       {consumer = fn s => (err:= s::(!err)), 
		flush = fn () => (), 
            linewidth = fn () => 80});
	  
        val ast = Compiler.MLParser.parse source ()
        val ast = case ast of
                       (Compiler.MLParser.PARSE result) => result
                     | Compiler.MLParser.ABORT => raise Compile "Aborted parsing"
                     | Compiler.MLParser.EOF => raise Compile "EOF while parsing"
                     | Compiler.MLParser.ERROR => raise Compile "Error parsing"
	  val ast = remove_outer_function (remove_outer_struct ast);

      in
(*	  to_use (dec_trav ast []) [];*)
(*	  ast_trav ast*)
(*      str*)
        exp_ast ast
      end;

 fun get_inf str
    = let

	  val err = ref (nil: string list);
	      
	  val source = Compiler.Source.newSource
	      ("",
	       TextIO.openString (concat["structure CPN'X = struct ",
					 str," end"]),
	       false,
	       {consumer = fn s => (err:= s::(!err)), 
		flush = fn () => (), 
            linewidth = fn () => 80});
	  
	val ast = Compiler.MLParser.parse source ()

        val ast = case ast of
                       (Compiler.MLParser.PARSE result) => result
                     | Compiler.MLParser.ABORT => raise Compile "Aborted parsing"
                     | Compiler.MLParser.EOF => raise Compile "EOF while parsing"
                     | Compiler.MLParser.ERROR => raise Compile "Error parsing"

	val ast = remove_outer_struct ast

	  val used = to_use (dec_trav ast []) [];
	      	      
      in
	 (ast, dec_trav ast [], (map #1 (top_def ast)), used)
      end;

fun find_defines (str) = 
    ListUtils.remdupl(map Compiler.Symbol.name (#3 (get_inf str)))

  end;
