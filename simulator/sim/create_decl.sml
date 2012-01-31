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
(* File: create_decl.sml
 *
 * Check and creation of declarations.
 *)

functor CPN'CreateDecl(structure CS: CPN'COLORSETS): CPN'DECL = struct

    open CS;
    structure Time = CS.Time

    type parameters = {name   : string,
		       timed  : bool,
		       var    : string list,
		       msvar  : string list,
		       alias  : string list,
		       declare: string list} option

    datatype global_decl =
	unit_cs       of string
      | bool_cs       of (string * string)
      | int_cs        of (string * string)
      | intinf_cs     of (string * string)
      | real_cs       of (string * string)
      | char_cs       of (string * string)
      | string_cs     of {char: string * string, length: string * string}
      | enum_cs       of string list
      | index_cs      of {idx: string, over: string * string}
      | list_cs       of {cs: string, length: string * string}
      | product_cs    of string list
      | record_cs     of (string * string) list
      | union_cs      of (string * string) list
      | funsubset_cs  of {cs: string, subset: string}
      | listsubset_cs of {cs: string, subset: string list}
      | time_cs 
      | alias_cs      of string
      | globref       of {name: string, exp: string}
      | usefile       of string
      | sml_code      of string
      | append_var    of string * string list
      | append_msvar  of string * string list
      | append_alias  of string * string list
      | append_decl   of string * string list
      | channel       of (string * string)

    datatype local_decl =
	pageref       of {name: string, exp: string, page: CPN'Id.id}
      | instref       of {name: string, exp: string, page: CPN'Id.id}


    (******************** reserved words to be avoided ********************)
    
    (* When words are added to or removed from the lists of reserved words, 
     * the documentation for users should also be updated, e.g
     * http://wiki.daimi.au.dk/cpntools-help/reserved_identifiers.wiki *)
    val ml_reserved_words =
	["let", "abstype", "local", "struct", "sig", "in", "with", "if", 
	 "then", "else", "case", "of", "fn", "fun", "val", "and", "datatype", 
	 "type", "exception", "open", "infix", "infixr", "nonfix", "handle", 
	 "raise", "signature", "structure", "functor", "andalso", "orelse", 
	 "not", "as", "do", "end", "op", "rec", "while", "eqtype", "include", 
	 "sharing", "o", "ref", "print", "int", "bool", "real", 
	 "unit", "string", "true", "false", "NONE", "SOME"]

    val cpn_reserved_words = 
	["empty", "color", "colset", "action", "all", "colorset", 
	 "declare", "globref", "index", "input", "list", "output", 
	 "product", "record", "subset", "by", "var", "time", "step",
	 "ms", "tms","Node","Arc","Scc","size",
	 "Impartial","Fair","Just","No_Fairness","EntireGraph","NoLimit"]
	
    val reserved_words = 
	Misc.sort String.< (ml_reserved_words^^cpn_reserved_words)

    fun is_reserved w = 
	let
	    fun is_res w [] = false
	      | is_res w (x::xs) = 
		if x=w
		then true
		else (* reserved_words is sorted *)
		    if String.<(x,w)
		    then is_res w xs
		    else false
	in
	    is_res w reserved_words
	end


    (****************************** errors ******************************)

    fun make_error errs = let
	fun make (NONE, tail) = tail
	  | make (SOME err, tail) = err::tail
    in
	concat (foldr make nil errs)
    end

    fun cs_not_declared_msg CPN'cs = "Color-set not defined: "^CPN'cs

    fun reserved_err w = "Reserved word cannot be used as identifier: "^w^"\n"
			 
    fun filter_reserved strlist = 
	map reserved_err 
	    (List.filter (fn w => is_reserved w) strlist)

    fun is_declared decls decl = List.exists (fn (x: string) => x=decl) decls

    (****************************** compile *****************************)

    fun compile_decl (id, decls, warn) =
	(CPN'Env.use_string_no_exn_filter ("\n"::decls, warn);
	 (id,"",
	  if !CPN'Settings.use_record_symbols
	      then CPN'Dep.find_dependencies (id,concat decls)
	  else ([],[])))
	handle Compile s => (id,s,([],[]))
	     | IO.Io {name=s,...} => (id,s,([],[]))
	     | ErrorLowHigh => raise ErrorLowHigh
	     | ErrorMinMax => raise ErrorMinMax
	     | ErrorNotChar => raise ErrorNotChar
	     | exn => (id,"\nUncaught exception: "^(exnName exn),([],[]))

    (************* function to be used for each color-set *************)
    fun merge ((id1, err1, (l11,l12)), (id2, err2, (l21,l22)))
	= let
	      fun insert elm (h::r)
		  = if elm = h then (h::r) else h::(insert elm r)
		| insert elm [] = [elm];
		  
	      fun insert_all (h::r) res = insert_all r (insert h res)
		| insert_all [] res = res
	  in
	      (id1,err1^err2, (insert_all l11 l21,insert_all l12 l22))
	  end;
       
    fun create_append_alias (id,name,[]) = (id,"",([],[]))
      | create_append_alias (id,name,n::ns) =
	(CPN'CSTable.append_alias_cs(n,name);
	 merge (compile_decl (id,["\n structure ",n," = ",name,";\n\
				      \ type ",n," = ",n,".cs;"], false),
				      create_append_alias(id,name,ns)));
				      
    fun each_cs (name, kind, id, timed, var, msvar, alias, declare, code, identifiers) = 
	let 
	    val reserved = filter_reserved (name::identifiers)
	in
	    if reserved <> []
	    then (id, concat reserved, ([],[]))	    
	    else 
	let 

	fun create_timed false = (id,"",([],[]))
	  | create_timed true = 
	    (case Time.start_time of
		 NONE => 
		     (id,"Error: timed color-sets can only be used in\
		      \ simulations with time",([],[]))
	       | SOME _ =>
		      compile_decl (id,
			["\n structure ",name,"'timed = CPN'ColorSets.TimedCS\
			 \ (structure cs = ",name,
			 " and Time = CPN'Time);\n\
			 \ type ",name,"'timed = ",name,"'timed.cs;"], false))


	val res1 = compile_decl (id,code, false);

	val _ = CPN'CSTable.insert (name,
				    {kind=kind,id=id,timed=timed,
				     var=var,msvar=msvar,
				     alias=alias,declare=declare})

	val res2 = create_timed timed;
	    
	val res3 = create_append_alias (id,name,alias);
	    
	val res = merge (res1,merge(res2,res3))

	(* Remove name from IMS table *)
        (* FIXME: We remove too many entries in IMS table with filter_prefix *)
	val _ = CPN'IMSTable.filter_prefix (CPN'CSTable.gen_prime_name_t name)

    in
       case (#2 res) of 
           "" =>
               (app (fn v => CPN'VarTable.insert(v,{cs=name,
						    decl_id=id})) var;
		app (fn v => CPN'VarTable.insert(v,{cs=name^" CPN'MS.ms",
						    decl_id = id})) msvar;
		res)
	 | err =>
	       res
    end    (* else-part *)
	end (* each_cs *)

(******************** unit color-set declaration ********************)

fun create_unit (id, "", {name,timed,var,msvar,alias,declare}) =
    each_cs(name, CPN'CSTable.unit_cs NONE, 
	    id,timed,var,msvar,alias,declare,
	    ["\n structure ",name," = CPN'ColorSets.UnitCS\
	     \(val CPN'str = NONE);\n\
	     \ type ",name," = ",name,".cs;\n"],[])
  | create_unit (id, str, {name,timed,var,msvar,alias,declare}) =
    each_cs(name, CPN'CSTable.unit_cs (SOME str),
	     id,timed,var,msvar,alias,declare,
	    ["\n structure ",name," = CPN'ColorSets.UnitCS\
	     \(val CPN'str = SOME(\"",str,"\"));\n\
	     \ type ",name," = ",name,".cs;\n\
	     \ val "^str^" = ();"],[str])

(******************** bool color-set declaration ********************)

fun create_bool (id, ("",""), {name,timed,var,msvar,alias,declare}) =
    each_cs(name, CPN'CSTable.bool_cs NONE,
	    id,timed,var,msvar,alias,declare,
	    ["\n structure ",name," = CPN'ColorSets.BoolCS (val CPN'arg= NONE);\n\
	     \ type ",name," = ",name,".cs;"],[])
  | create_bool (id, (low,high), {name,timed,var,msvar,alias,declare}) =
    each_cs(name, CPN'CSTable.bool_cs (SOME(low,high)),
	    id,timed,var,msvar,alias,declare,
	    ["\n structure ",name," = CPN'ColorSets.BoolCS\
	     \ (val CPN'arg = SOME(\"",low,"\",\"",high,"\"));\n\
	     \ type ",name," = ",name,".cs;\n\
	     \ val "^low^" = false;\n\
	     \ val "^high^" = true;"],[low,high])

(******************** int color-set declaration ********************)

fun create_int (id, ("",""), {name,timed,var,msvar,alias,declare}) =
    each_cs(name, CPN'CSTable.int_cs NONE,
	     id,timed,var,msvar,alias,declare,
	    ["\n structure ",name," = CPN'ColorSets.IntCS;\n\
	     \ type ",name," = ",name,".cs;"],[])
  | create_int (id, (low,high), {name,timed,var,msvar,alias,declare}) =
    (case (CPN'Env.is_decl ("val _ = "^low^" : int"),
	   CPN'Env.is_decl ("val _ = "^high^" : int")) of
	 (NONE,NONE) => 
	     (each_cs(name, CPN'CSTable.int_cs (SOME(low,high)),
		      id,timed,var,msvar,alias,declare,
		     ["\n structure ",name," = CPN'ColorSets.IntWithCS\
		      \ (val CPN'low= ",low," and CPN'high= ",high,");\n\
		      \ type ",name," = ",name,".cs;"],[])
	      handle ErrorLowHigh => (id,error_low_high "int",([],[])))
       | (low_err, high_err) => (id,make_error[low_err,high_err],([],[])))


(************************ channel declaration ************************)

fun create_channel (id, (typ,name)) = 
    compile_decl (id, ["\n structure ", name, "= struct\n",
		       "  fun ! (n: ", typ, ") = ()\n",
		       "  fun ? (n: ", typ, ") = ()\n",
		       " end;"], false)

(******************** intinf color-set declaration ********************)

fun create_intinf (id, ("",""), {name,timed,var,msvar,alias,declare}) =
    each_cs(name, CPN'CSTable.intinf_cs NONE,
	     id,timed,var,msvar,alias,declare,
	    ["\n structure ",name," = CPN'ColorSets.IntInfCS;\n\
	     \ type ",name," = ",name,".cs;"],[])
  | create_intinf (id, (low,high), {name,timed,var,msvar,alias,declare}) =
    (case (CPN'Env.is_decl ("val _ = "^low^" : IntInf.int"),
	   CPN'Env.is_decl ("val _ = "^high^" : IntInf.int")) of
	 (NONE,NONE) => 
	     (each_cs(name, CPN'CSTable.intinf_cs (SOME(low,high)),
		      id,timed,var,msvar,alias,declare,
		     ["\n structure ",name," = CPN'ColorSets.IntInfWithCS\
		      \ (val CPN'low= ",low," and CPN'high= ",high,");\n\
		      \ type ",name," = ",name,".cs;"],[])
	      handle ErrorLowHigh => (id,error_low_high "intinf",([],[])))
       | (low_err, high_err) => (id,make_error[low_err,high_err],([],[])))

(******************** real color-set declaration ********************)

fun create_real (id, ("",""), {name,timed,var,msvar,alias,declare}) =
(* Disable declarations of real color-sets
    each_cs(name, CPN'CSTable.real_cs NONE,
	     id,timed,var,msvar,alias,declare,
	    ["\n structure ",name," = CPN'ColorSets.RealCS;\n\
	     \ type ",name," = ",name,".cs;"]) *)
    raise CPN'Error "Error: real color sets are not supported!"
  | create_real (id, (low,high), {name,timed,var,msvar,alias,declare}) =
(* Disable declarations of real color-sets
    (case (CPN'Env.is_decl ("val _ = "^low^" : real"),
	   CPN'Env.is_decl ("val _ = "^high^" : real")) of
	 (NONE,NONE) => 
	     (each_cs(name, CPN'CSTable.real_cs (SOME(low,high)),
		      id,timed,var,msvar,alias,declare,
		     ["\n structure ",name," = CPN'ColorSets.RealWithCS\
		      \ (val CPN'low= ",low," and CPN'high= ",high,");\n\
		      \ type ",name," = ",name,".cs;"])
	      handle ErrorLowHigh => (id,error_low_high "real",([],[])))
       | (low_err, high_err) => (id,make_error[low_err,high_err],([],[])))
*)
    raise CPN'Error "Error: real color sets are not supported!"

(******************** string color-set declaration ********************)

fun create_string (id, {char=("",""),length=("","")}, 
		   {name,timed,var,msvar,alias,declare}) =
    each_cs(name, CPN'CSTable.string_cs {char=NONE,length=NONE},
	     id,timed,var,msvar,alias,declare,
	    ["\n structure ",name," = CPN'ColorSets.StringCS;\n\
	     \ type ",name," = ",name,".cs;"],[])
  | create_string (id, {char=(low,high),length=("","")}, 
		   {name,timed,var,msvar,alias,declare}) =
    (case (CPN'Env.is_decl ("val _ = "^low^" : string"),
	   CPN'Env.is_decl ("val _ = "^high^" : string")) of
	 (NONE,NONE) => 
	     (each_cs(name, 
		      CPN'CSTable.string_cs {char=SOME(low,high), length=NONE},
		      id,timed,var,msvar,alias,declare,
		     ["\n structure ",name," = CPN'ColorSets.StringWithCS\
		      \ (val CPN'low= ",low," and CPN'high= ",high,");\n\
		      \ type ",name," = ",name,".cs;"],[])
	      handle ErrorLowHigh => (id,error_low_high "string",([],[]))
		   | ErrorNotChar => (id,error_not_char,([],[])))
       | (low_err, high_err) => (id,make_error[low_err,high_err],([],[])))
  | create_string (id, {char=("",""),length=(min,max)}, 
		   {name,timed,var,msvar,alias,declare}) =
    (case (CPN'Env.is_decl ("val _ = "^min^" : int"),
	   CPN'Env.is_decl ("val _ = "^max^" : int")) of
	 (NONE,NONE) => 
	     (each_cs(name,
		      CPN'CSTable.string_cs {char=NONE, length=SOME(min,max)},
		      id,timed,var,msvar,alias,declare,
		     ["\n structure ",name," = CPN'ColorSets.StringWithLengthCS\
		      \ (val CPN'low= \"\000\" and CPN'high= \"\255\"\
		      \ and CPN'min= ",min," and CPN'max= ",max,");\n\
		      \ type ",name," = ",name,".cs;"],[])
	      handle ErrorMinMax => (id,error_min_max "string",([],[])))
       | (min_err, max_err) => (id,make_error[min_err,max_err],([],[])))
  | create_string (id, {char=(low,high),length=(min,max)}, 
		   {name,timed,var,msvar,alias,declare}) =
    (case (CPN'Env.is_decl ("val _ = "^low^" : string"),
	   CPN'Env.is_decl ("val _ = "^high^" : string"),
	   CPN'Env.is_decl ("val _ = "^min^" : int"),
	   CPN'Env.is_decl ("val _ = "^max^" : int")) of
	 (NONE,NONE,NONE,NONE) => 
	     (each_cs(name, CPN'CSTable.string_cs {char=SOME(low,high),
						   length=SOME(min,max)},
		      id,timed,var,msvar,alias,declare,
		     ["\n structure ",name," = CPN'ColorSets.StringWithLengthCS\
		      \ (val CPN'low= ",low," and CPN'high= ",high,"\
		      \ and CPN'min= ",min," and CPN'max= ",max,");\n\
		      \ type ",name," = ",name,".cs;"],[])
	      handle ErrorMinMax => (id,error_min_max "string",([],[]))
		   | ErrorLowHigh => (id,error_low_high "string",([],[]))
		   | ErrorNotChar => (id,error_not_char,([],[])))
       | (low_err,high_err,min_err,max_err) => 
	     (id,make_error[low_err,high_err,min_err,max_err],([],[])))


(******************** list color-set declaration ********************)

fun create_list (id, {cs,length=("","")}, 
		 {name,timed,var,msvar,alias,declare}) =
    (case (CPN'CSTable.peek cs) of 
	 SOME _ => each_cs(name, CPN'CSTable.list_cs {cs=cs,length=NONE},
			   id,timed,var,msvar,alias,declare,
			   ["\n structure ",name," = CPN'ColorSets.ListCS\
			    \ (structure cs= ",cs,");\n\
			    \ type ",name," = ",name,".cs;"],[])
       | _ => (id, cs_not_declared_msg cs,([],[])))
  | create_list (id, {cs,length=(min,max)}, 
		 {name,timed,var,msvar,alias,declare}) =
    (case (CPN'CSTable.peek cs) of 
	 SOME _ => 
	     (case (CPN'Env.is_decl ("val _ = "^min^" : int"),
		    CPN'Env.is_decl ("val _ = "^max^" : int")) of
		  (NONE,NONE) => 
		      (each_cs(name, CPN'CSTable.list_cs {cs=cs,length=SOME(min,max)},
			       id,timed,var,msvar,alias,declare,
			       ["\n structure ",name," = CPN'ColorSets.ListWithCS\
				\ (structure cs= ",cs,";\
				\ val CPN'min= ",min," and CPN'max= ",max,");\n\
				\ type ",name," = ",name,".cs;"],[])
				handle ErrorMinMax => (id,error_min_max "list",([],[])))
		| (min_err,max_err) => (id,make_error[min_err,max_err],([],[])))
       | _ => (id, cs_not_declared_msg cs,([],[])))
				
(******************** index color-set declaration ********************)

fun create_index (id, {idx,over=("","")}, 
		  {name,timed,var,msvar,alias,declare}) =
    each_cs(name, CPN'CSTable.index_cs {idx=idx,over=NONE},
	    id,timed,var,msvar,alias,declare,
	    ["datatype ",name," = ",idx," of int;\n\
	     \ structure ",name," = CPN'ColorSets.IndexCS\
	     \ (type cs = ",name,";\
	     \ val CPN'idx = \"",idx,"\"\
	     \ and CPN'con = ",idx,
	     " and CPN'clr = fn (",idx," CPN'i) => CPN'i);"],[idx])
  | create_index (id, {idx,over=(low,high)}, 
		  {name,timed,var,msvar,alias,declare}) =
    (case (CPN'Env.is_decl ("val _ = "^low^" : int"),
	  CPN'Env.is_decl ("val _ = "^high^" : int")) of
	 (NONE,NONE) => 
	     (each_cs(name, CPN'CSTable.index_cs {idx=idx,over=SOME(low,high)},
		      id,timed,var,msvar,alias,declare,
		     ["datatype ",name," = ",idx," of int;\n\
		      \ structure ",name," = CPN'ColorSets.IndexWithCS\
		      \ (type cs = ",name,";\
		      \ val CPN'idx = \"",idx,"\"\
		      \ and CPN'low = ",low,
		      " and CPN'high = ",high,
		      " and CPN'con = ",idx,
		      " and CPN'clr = fn (",idx," CPN'i) => CPN'i);\n"],[idx])
	      handle ErrorLowHigh => (id,error_low_high "index",([],[])))
       | (low_err, high_err) => (id,make_error[low_err,high_err],([],[])))

(******************** func subset color-set declaration ********************)

fun create_funsubset (id, arg as {cs,subset}, 
		      {name,timed,var,msvar,alias,declare}) =
    (case (CPN'CSTable.peek cs) of 
	 SOME _ => (case CPN'Env.is_decl (concat["val _ = ",subset," : ",cs,".cs -> bool;"]) of
			NONE => 
			    each_cs(name, CPN'CSTable.funsubset_cs arg,
				    id,timed,var,msvar,alias,declare,
				    ["\nstructure ",name," = CPN'ColorSets.FunSubsetCS\
				     \ (structure cs = ",cs,";\
				     \ val CPN'subset = ",subset,");\n\
				     \ type ",name," = ",name,".cs;"],[])
		      | err => (id,make_error[err],([],[])))
       | _ => (id, cs_not_declared_msg cs,([],[])))

(******************** list subset color-set declaration ********************)

fun create_listsubset (id, arg as {cs,subset}, 
		       {name,timed,var,msvar,alias,declare}) = let
    val list =
	concat ("["::tl(foldr (fn (a,b) => ","::a::b) ["","]"] subset))
in
    (case (CPN'CSTable.peek cs) of 
	 SOME _ => 
	     (case CPN'Env.is_decl (concat["val _ = ",list," : ",cs,".cs list;"]) of
		  NONE => 
		      each_cs(name, CPN'CSTable.listsubset_cs arg,
			      id,timed,var,msvar,alias,declare,
			      ["\n structure ",name," = CPN'ColorSets.ListSubsetCS\
			       \ (structure cs = ",cs,";\
			       \ val CPN'subset = ",list,");\n\
			       \ type ",name," = ",name,".cs;"],[])
		| err => (id,make_error[err],([],[])))
       | _ => (id, cs_not_declared_msg cs,([],[])))
end

(********************** time color-set declaration ***********************)

fun create_time (id, {name,timed,var,msvar,alias,declare}) =
    each_cs(name, CPN'CSTable.time_cs,
	    id,timed,var,msvar,alias,declare,
	    ["\n structure ",name," = ",
	     case Time.name of
		 "int" => "CPN'ColorSets.IntCS;"
	       | "intinf" => "CPN'ColorSets.IntInfCS;"
	       | "real" => "CPN'ColorSets.RealCS;"
	       | _ => raise InternalError("create_time"),  
	     "\n type ",name," = CPN'Time.time"],[])

(******************** duplicate color-set declaration ********************)

fun create_alias (id, cs, {name,timed,var,msvar,alias,declare}) =
    case (CPN'CSTable.peek cs) of 
    SOME _ => each_cs(name, CPN'CSTable.alias_cs cs,
		      id,timed,var,msvar,alias,declare,
		      ["\n structure ",name," = ",cs,";\
		       \\n type ",name," = ",name,".cs;"],[])
  | _ => (id, cs_not_declared_msg cs,([],[]))

(******************** enumerate color-set declaration ********************)

fun create_enum (id, enm, {name,timed,var,msvar,alias,declare}) = let

    (* The gen_* functions are used when CPN'Settings.use_cast is false *)

    val N = length enm

    fun gen_input (enm,tail) =
	     "\n fun input CPN's =\
	      \\n case String.implode(CPN'StreamIO.get_next CPN's) of\n"::
	     tl(foldr (fn (a,b) => " | "::"\""::a::"\" => "::a::b)
		("\n | CPN'tok => raise CPN'StreamIO.IOError\
		 \(\"Illegal enumeration token '\"^CPN'tok^\"'\")"::tail) enm)

    fun gen_ord (enm,tail) = let
	fun gen (i,x::xs) = 
	    " | "::x::" => "::(Int.toString i)::gen(i+1,xs)
	  | gen (_,nil) = tail
    in
	"\n val ord = fn "::tl(gen(0,enm))
    end

    fun gen_col (enm,tail) = let
	fun gen (i,x::xs) = 
	    " | "::(Int.toString i)::" => "::x::gen(i+1,xs)
	   | gen (_,nil) = " | _ => raise CPN'Error CPN'ColorSets.out_of_range"::tail
    in
	"\n val col = fn "::tl(gen(0,enm))
    end

    fun gen_lt (enm,tail) = let
	fun gen (_::nil) =
	    "\n | "::"(_,_) => false"::tail
	  | gen (x::xs) =
	    "\n | "::"(_,"::x::") => false\
	     \\n | ("::x::",_) => true"::(gen xs)
	  | gen _ = raise Match
    in
	if N < !CPN'Settings.small_enum_size then
	     "\n val lt = fn "::tl(gen enm)
	else
	     "\n fun lt(CPN'x,CPN'y) = (ord CPN'x)<(ord CPN'y)"::tail
    end

    fun gen_cmp (name,tail) =
       ("\n val cmp:"::name::" * "::name::"-> order = (CPN'Misc.a_cmp lt)"::tail)
in
    each_cs(name, CPN'CSTable.enum_cs enm,
	id,timed,var,msvar,alias,declare,
	"datatype "::name::" = "::
	tl(foldr 
	    (fn (a,b) => " | "::a::b)
	    (if !CPN'Settings.use_cast then
		 ("\n structure "::name::" = CPN'ColorSets.EnumCS(type cs ="::name::
		  "; val CPN'enm = ["::tl(foldr (fn (a,b) => ","::"\""::a::"\""::b)
		  ("]);"::nil) enm))
	     else
		 ("\n structure "::name::" : COLORSET = struct "::
		  "\n type cs = "::name::
		  "\n val base = "::(hd enm)::
		  "\n fun all () = "::

		  "["::
		  tl(foldr
		      (fn (a,b) => ","::a::b) 
		      (""::"]"::
		       "\n fun legal (_: cs) = true\
			\\n fun illegal_msg (_: cs) = \"\" \
			\\n val mkstr = fn "::
		       tl(foldr
			   (fn (a,b) => " | "::a::" => \""::a::"\""::b)
			   (gen_input(enm,
			    gen_col(enm,
			    gen_ord(enm,
			    gen_lt(enm,
			    gen_cmp(name,
			     "\n fun output(CPN's,CPN'c) =\
			      \ CPN'IO.output(CPN's,(mkstr CPN'c)^\" \")\
			      \\n val input_ms : TextIO.instream -> cs CPN'MS.ms =\
			      \ CPN'MS.input_ms false input\
			      \\n val output_ms : TextIO.outstream * cs CPN'MS.ms -> unit\
			      \ = CPN'MS.output_ms false (output,lt)\
			      \\n fun size () = "::Int.toString(length enm)::
			     "\n val mkstr_ms : cs CPN'MS.ms -> string =\
			      \ CPN'MS.mkstr_ms (mkstr,lt)\
			      \\n fun ran () = col (CPN'Random.int(size()))\
			      \\n end;"::nil)))))
			   ) enm
                         )
		      ) enm
                    )
		 )
	    ) enm
	   ),enm)
end


local (* in product and record color-set declarations *)

    fun gen_head (comp,append) = let
	fun gen ((label,_)::rcs) =
	    ","::label::"=CPN'"::label::append::(gen rcs)
	  | gen nil = "}"::nil
    in
	concat ("{"::tl(gen comp))
    end

    fun gen_cs (name,comp,tail) = let
	fun gen ((label,cs)::rcs) = 
	    ","::label::":"::cs::(gen rcs)
	  | gen nil = "}"::tail
    in
	"\n type "::name::" = {"::tl(gen comp)
    end

    fun gen_base (comp,tail) = let
	fun gen ((label,cs)::rcs) = 
	    ","::label::"="::cs::".base"::(gen rcs)
	  | gen nil = "}"::tail
    in
        "\n val base = {"::tl(gen comp)
    end
    
    fun gen_illegal(head,comp,tail) = 
	"\n fun illegal_msg "::head::" = \n if legal "::head::
	" then \"\"\n else \"Illegal component in color: \""::
	"^(mkstr "::head::")"::tail

    fun gen_legal (head,comp,tail) = let
	fun gen ((label,cs)::rcs) =
	    " andalso "::cs::".legal (CPN'"::label::")"::(gen rcs)
	  | gen nil = gen_illegal(head,comp,tail)
    in
	"\n fun legal "::head::" = "::tl(gen comp)
    end
    
    fun gen_all (false,false,_,_,tail) = tail
      | gen_all (true,false,name,_,tail) = 
	"\n fun all () =\
	 \ CPN'ColorSets.error_not_decl(\"all\",\""::name::"\")"::tail
      | gen_all (_,true,name,comp,tail) = let

	fun gen ((_,cs)::rcs,tail) = ","::cs::".all()"::gen(rcs,tail)
	  | gen (_,tail) = tail
     in
	  "\n local\n\
	   \ val all_ref = ref(NONE: "::name::" CPN'MS.ms option)\n\
	   \ in\n\
	   \ fun all () = case !all_ref of SOME(all') => all'\n\
	   \ | NONE => let\
	   \\n val all' = mult ("::tl(gen(comp,")\
	   \\n in (all_ref:= SOME(all'); all') end\
	   \\n end"::tail))
    end
 
    fun gen_lt (head,head',comp,tail) = let
	fun gen ((label,cs),tail) =
	    "if "::cs::".lt(CPN'"::label::",CPN'"::label::"') then true\
	     \\n else if "::cs::".lt(CPN'"::label::"',CPN'"::label::") then false\
	     \\n else "::tail
    in
       "\n fun lt ("::head::","::head'::") = "::(foldr gen ("false"::tail) comp)
    end

    fun gen_cmp (name,tail) =
       ("\n val cmp:"::name::" * "::name::"-> order = (CPN'Misc.a_cmp lt)"::tail)

    fun gen_size (comp,tail) = let
	fun gen ((_,cs)::rcs) =
	    "*"::cs::".size()"::gen(rcs)
	  | gen nil = tail
    in
	"\n fun size () = "::tl(gen comp)
    end
    
    fun gen_col (false,false,_,_,tail) = tail
      | gen_col (true,false,name,_,tail) =
	"\n fun col _ =\
	 \ CPN'ColorSets.error_not_decl(\"col\",\""::name::"\")"::tail 
      | gen_col (_,true,name,comp,tail) = let
	fun gen ("",(label,cs)::rcs) =
	    "{"::label::"="::cs::".col(CPN'i mod "::cs::".size())"::
	    gen(cs^".size()",rcs)
	  | gen (divs,(label,cs)::rcs) =
	    ",\n "::label::"="::cs::".col((CPN'i div ("::divs::")) mod\
	     \ "::cs::".size())"::gen(cs^".size()*"^divs,rcs)
	  | gen _ = "}"::"\n else raise CPN'Error CPN'ColorSets.out_of_range"::tail
    in
	 "\n fun col CPN'i =\n\
	  \ if 0<=CPN'i andalso CPN'i<size() then\n "::
	 gen("", rev comp)
    end

    fun gen_ord (false,false,_,_,_,tail) = tail
      | gen_ord (true,false,name,_,_,tail) =
	"\n fun ord _ =\
	 \ CPN'ColorSets.error_not_decl(\"ord\",\""::name::"\")"::tail
      | gen_ord (_,true,name,head,comp,tail) = let
	fun gen ("", (label,cs)::rcs) =
	    cs::".ord(CPN'"::label::")"::gen(cs^".size()*",rcs)
	  | gen (muls, (label,cs)::rcs) =
	    "+"::muls::cs::".ord(CPN'"::label::")"::
	    gen(cs^".size()*"^muls,rcs)
	  | gen (_,nil) = tail
    in
	"\n fun ord "::head::" =\n"::gen("",rev comp)
    end
	
    fun gen_ran (false,false,_,_,tail) = tail
      | gen_ran (true,false,name,_,tail) =
	"\n fun ran _ =\
	 \ CPN'ColorSets.error_not_decl(\"ran\",\""::name::"\")"::tail
      | gen_ran (_,true,name,comp,tail) =	let
	fun gen ((label,cs)::rcs) =
	    ","::label::"="::cs::".ran()"::gen(rcs)
	  | gen nil = "}"::tail
    in
	 "\n fun ran () = {"::tl(gen comp)
    end
	
    fun gen_mult (genmult,genall,name,comp,tail) =
	if (not genmult) andalso (not genall) then tail else
	(* Genrerates with CPN' prepended on all variables: 
	 * (n length of tuple, m = n-1, and l = m-1)
	 * fun mult'cs (Y1: cs1 ms,...,Yn: csn ms): cs ms = let
	 *     fun mult (X1 as (c1,x1)::_,...,Xm as (cm,xm)::_,(cn,xn)::xns) =
	 *         (c1*...*cm*cn,(x1,...,xm,xn))::mult(X1,...XM,xns)
	 *       | mult (X1,...,Xl,_::xms,nil) = mult(X1,...,Xl,xms,Yn)
	 *       | mult (X1,...,_::xls,nil,_) = mult(X1,...,xls,Ym,Yn)
	 *       | ...
	 *       | mult (_::x1s,nil,_,...,_) = mult(x1s,Y2,...,Yn)
	 *       | mult (nil,_,...,_) = nil
	 *   in
	 *     mult(Y1,...,Yn)
	 *   end
	 *)
	let
	    val n = List.length comp
		
	    fun gen_head (i,(_,cs)::rcs,tail) =
		","::"CPN'Y"::(Int.toString i)::": "::cs::" CPN'MS.ms"::
		gen_head(i+1,rcs,tail) 
	      | gen_head (_,nil,tail) = tail

	    fun gen_arg (i,_::rcs,tail) =
		","::"CPN'Y"::(Int.toString i)::gen_arg(i+1,rcs,tail)
	      | gen_arg (_,nil,tail) = tail

	    fun gen_first (i,(label,_)::nil) = let
		val no = Int.toString i
	    in
		("(CPN'x"::no::")::CPN'xs"::no::nil, (* head_i *)
		 label::"=CPN'x"::no::nil,                         (* col_i *)
		 "CPN'xs"::no::nil)                                (* arg_i *) 
	    end
	      | gen_first (i,(label,_)::rcs) = let
		val no = Int.toString i
		val (head,col,arg) = gen_first (i+1,rcs)
	     in
		 ("CPN'X"::no::" as (CPN'x"::no::")::_,"::head,
		  label::"=CPN'x"::no::","::col,
		  "CPN'X"::no::","::arg)
	    end
	      | gen_first _ = raise Match
		
	    fun gen_rem (j,tail) = let
		fun gen i = let
		    val no = Int.toString i
		    val (head,arg) = if i<n then gen(i+1) else (nil,nil)
		in
		    if i<j then
			(","::"CPN'X"::no::head, ","::"CPN'X"::no::arg)
		    else if j<i then
			if i=j+1 then
			    (","::"nil"::head, ","::"CPN'Y"::no::arg)
			else
			    (","::"_"::head, ","::"CPN'Y"::no::arg)
		    else (* j=i *)
			(","::"_::CPN'xs"::no::head, ","::"CPN'xs"::no::arg)
		end
		    
		val (head,arg) = gen 1
	    in
		if j>0 then
		    " | CPN'mult("::concat(tl head)::") =\
		     \ CPN'mult("::concat(tl arg)::")\n"::
		    gen_rem(j-1,tail)
		else
		    " | CPN'mult("::concat(tl head)::") = nil\n"::tail
	    end
	
	    val (head,col,arg) = gen_first (1,comp)
	in
	    "\n fun mult ("::tl(gen_head(1,comp,"): "::name::" CPN'MS.ms = \nlet\n\
	     \ fun CPN'mult("::(concat head)::") =\n\
	     \ ({"::(concat col)::"})::\
	     \CPN'mult("::(concat arg)::")\n"::
             gen_rem(n-1," in CPN'mult("::tl(gen_arg(1,comp,") end"::tail)))))
	end

    fun gen_set (false,_,_,tail) = tail
      | gen_set (true,head,comp,tail) = let

	fun gen ((label,cs),tail) = let

	    fun f ((lbl,cs),tail) = 
		if lbl=label then ","::lbl::"=CPN'new_"::label::tail
		else ","::lbl::"=CPN'"::lbl::tail
	in
	    "\n fun set_"::label::" "::head::
	    " (CPN'new_"::label::":"::cs::") = \n if "::cs::".legal CPN'new_"::label::"\n then {"::
	    tl(foldr f ("} \n else raise CPN'Error ("::cs::".illegal_msg("::
			"CPN'new_"::label::"))"::tail) comp)
	end
    in
	foldr gen tail comp
    end
in

(******************** product color-set declaration ********************)

local

    fun gen_input (false,false,_,_,tail) = tail
      | gen_input (true,false,name,_,tail) = 
	"\n fun input _ =\
	 \ CPN'ColorSets.error_not_decl(\"input\",\""::name::"\")"::tail 
      | gen_input (_,true,name,comp,tail) = let
	fun gen_vals ((label,cs)::nil,tail) =
	    "\n val CPN'"::label::" =\
	     \ String.implode(#2(CPN'StreamIO.get_until(CPN's,[#\")\"])))"::tail
          | gen_vals ((label,cs)::rcs,tail) =
	    "\n val CPN'"::label::" =\
	     \ String.implode(#2(CPN'StreamIO.get_until(CPN's,[#\",\"])))"::
             gen_vals(rcs,tail)
          | gen_vals (nil,tail) = tail

        fun gen_prod ((label,cs)::rcs,tail) =
	    ","::cs::".input (TextIO.openString(CPN'"::label::"))"::
	    gen_prod(rcs,tail)
	  | gen_prod (nil,tail) = ""::tail
    in
        "\n fun input CPN's = let\
	 \\n val _ = CPN'StreamIO.skip_white_spaces CPN's\
	 \\n val _ = case CPN'StreamIO.get_one CPN's of\
	 \\n (SOME #\"(\") => ()\
	 \\n | NONE => raise CPN'StreamIO.IOError\
	 \(\"Can not find '(' when reading a product\")"::
	 gen_vals(comp,"\n in\
	  \\n ("::tl(gen_prod(comp,")\n end"::tail)))
    end

    fun gen_mkstr (head,comp,tail) = let
	fun gen ((label,cs)::rcs) =
	    ",\",\","::cs::".mkstr CPN'"::label::(gen rcs)
	  | gen _ = ",\")\"]"::tail
    in
	"\n fun mkstr "::head::" = CPN'concat [\"(\","::tl(gen comp)
    end
in

fun create_product (id, comp, {name,timed,var,msvar,alias,declare}) = let
    val cs_not_decl = List.filter (fn CPN'cs => (CPN'CSTable.peek CPN'cs)=NONE) comp

    val err_msg =  foldr (fn (CPN'cs,CPN'tail) => 
			  ("\n"::
			   (cs_not_declared_msg CPN'cs)::
			   CPN'tail)) [] cs_not_decl

    val _ = if cs_not_decl=[] then () 
	    else raise CPN'Error (concat (tl err_msg))

    fun extend (i,cs::rcs) = (Int.toString i,cs)::extend(i+1,rcs)
      | extend (_,nil) = nil

    val xcomp = extend(1,comp)
    val head = gen_head (xcomp,"");
    val head' = gen_head (xcomp,"'");
    val isdecl = is_declared declare
    val few_comps = (List.length comp)<10
in
    each_cs(name, CPN'CSTable.product_cs xcomp,
	    id,timed,var,msvar,alias,declare,
	    gen_cs(name,xcomp,
	    "\n structure "::name::" = struct\
	    \\n type cs = "::name::
	    gen_base(xcomp,
	    gen_lt(head,head',xcomp,
	    gen_cmp(name,
	    gen_mkstr(head,xcomp,
	    "\n val mkstr_ms : cs CPN'MS.ms -> string\
	     \ = CPN'MS.mkstr_ms (mkstr,lt)"::
	    gen_legal(head,xcomp,
	    gen_size(xcomp,
	    gen_mult((few_comps orelse (isdecl "mult")),
		     (few_comps orelse (isdecl "all")),name,xcomp,
	    gen_input(true,true,name,xcomp,
	    "\n fun output(CPN's,CPN'c) = \
	    \\n if legal CPN'c then CPN'IO.output(CPN's,(mkstr CPN'c)^\" \")\
	    \\n else raise CPN'Error (illegal_msg(CPN'c))\
	    \\n val input_ms : TextIO.instream -> cs CPN'MS.ms = CPN'MS.input_ms false input\
	    \\n val output_ms : TextIO.outstream * cs CPN'MS.ms -> unit\
	    \ = CPN'MS.output_ms false (output,lt)"::
	    gen_all(true,(few_comps orelse (isdecl "all")),name,xcomp,
	    gen_ord(true,(few_comps orelse (isdecl "ord")),name,head,xcomp,
	    gen_col(true,(few_comps orelse (isdecl "col")),name,xcomp,
	    gen_ran(true,(few_comps orelse (isdecl "ran")),name,xcomp,	    
	    gen_set((few_comps orelse (isdecl "set")),head,xcomp,"\n end;"::nil)))))))))))))),[])
end handle CPN'Error str => (id,str,([],[]))

fun append_product_decl(id,name,xcomp,decls) = let
    val isdecl = is_declared decls
    val head = gen_head (xcomp,"")
    val _ = CPN'CSTable.append_declare(name,decls)
    val few_comps = (List.length xcomp)<10
in
    compile_decl(id,"structure "::name::" = struct\n\
     \ open "::name::";"::
    gen_input(false,true,name,xcomp,
    gen_mult((few_comps orelse (isdecl "mult")),(few_comps orelse (isdecl "all")),name,xcomp,
    gen_all(false,(few_comps orelse (isdecl "all")),name,xcomp,
    gen_ord(false,(few_comps orelse (isdecl "ord")),name,head,xcomp,
    gen_col(false,(few_comps orelse (isdecl "col")),name,xcomp,
    gen_ran(false,(few_comps orelse (isdecl "ran")),name,xcomp,	    
    gen_set((few_comps orelse (isdecl "set")),head,xcomp,"\n end;"::nil))))))), false)
end

end (* local product cs *)

(******************** record color-set declaration ********************)

fun find (l,((a,b)::xs)) = if a=l then b else find(l,xs)
  | find (l, nil) = 
    raise CPN'StreamIO.IOError("Illegal identifier '"^l^"' when reading a record")

local
    fun gen_input (false,false,_,_,tail) = tail
      | gen_input (true,false,name,_,tail) = 
	"\n fun input _ =\
	 \ CPN'ColorSets.error_not_decl(\"input\",\""::name::"\")"::tail
      | gen_input (_,true,name,comp,tail) = let
	fun gen_vals (_::nil,tail) =
	    "\n val CPN'label =\
	     \ String.implode(#2(CPN'StreamIO.get_until(CPN's,[#\"=\"])))\
	     \\n val CPN'value =\
	     \ String.implode(#2(CPN'StreamIO.get_until(CPN's,[#\"}\"])))\
	     \\n val _ = CPN'list::= (CPN'label,CPN'value)"::tail
          | gen_vals (_::rcs,tail) =
	    "\n val CPN'label =\
	     \ String.implode(#2(CPN'StreamIO.get_until(CPN's,[#\"=\"])))\
	     \\n val CPN'value =\
	     \ String.implode(#2(CPN'StreamIO.get_until(CPN's,[#\",\"])))\
	     \\n val _ = CPN'list::= (CPN'label,CPN'value)"::gen_vals(rcs,tail)
          | gen_vals (nil,tail) = tail

        fun gen_rec ((label,cs)::rcs,tail) =
	    ",\n "::label::"="::cs::".input(TextIO.openString(CPN'Decl.find\
	     \(\""::label::"\", !CPN'list)))"::
            gen_rec(rcs,tail)
	  | gen_rec (nil,tail) = ""::tail
    in
        "\n fun input CPN's = let\
	 \\n val CPN'list = ref (nil: (string*string) list)\
	 \\n val _ = CPN'StreamIO.skip_white_spaces CPN's\
	 \\n val _ = case CPN'StreamIO.get_one CPN's of\
	 \\n (SOME #\"{\") => ()\
	 \\n | NONE => raise CPN'StreamIO.IOError\
	 \(\"Can not find '{' when reading a record\")"::
	 gen_vals(comp,"\n in\n {"::tl(gen_rec(comp,"}\n end"::tail)))
    end
in

fun create_record (id, comp, {name,timed,var,msvar,alias,declare}) = let
    val cs_not_decl = List.filter (fn (CPN'id,CPN'cs) => (CPN'CSTable.peek CPN'cs)=NONE) comp

    val err_msg =  foldr (fn ((CPN'id,CPN'cs),CPN'tail) => 
			  ("\n"::
			   (cs_not_declared_msg CPN'cs)::
			   CPN'tail)) [] cs_not_decl

    val _ = if cs_not_decl=[] then () 
	    else raise CPN'Error (concat (tl err_msg))

    fun gen_mkstr (head,comp,tail) = let
	fun gen ((label,cs)::rcs) =
	    ",\","::label::"=\","::cs::".mkstr CPN'"::label::(gen rcs)
	  | gen _ = ",\"}\"]"::tail
    in
	"\n fun mkstr "::head::" = CPN'concat [\"{"::tl(gen comp)
    end
    
    val head = gen_head (comp,"");
    val head' = gen_head (comp,"'");
    val isdecl = is_declared declare
    val few_comps = (List.length comp)<10
in
    each_cs(name, CPN'CSTable.record_cs comp,
	    id,timed,var,msvar,alias,declare,
	    gen_cs(name,comp,
	    "\n structure "::name::" = struct\
	    \\n type cs = "::name::
	    gen_base(comp,
	    gen_lt(head,head',comp,
	    gen_cmp(name,
	    gen_mkstr(head,comp,
	    "\n val mkstr_ms : cs CPN'MS.ms -> string\
	     \ = CPN'MS.mkstr_ms (mkstr,lt)"::
	    gen_legal(head,comp,
	    gen_size(comp,
	    gen_mult((few_comps orelse (isdecl "mult")),
		     (few_comps orelse (isdecl "all")),name,comp,
	    gen_input(true,true,name,comp,
	    "\n fun output(CPN's,CPN'c) = \
	    \\n if legal CPN'c then CPN'IO.output(CPN's,(mkstr CPN'c)^\" \")\
	    \\n else raise CPN'Error (illegal_msg(CPN'c))\
	    \\n val input_ms : TextIO.instream -> cs CPN'MS.ms = CPN'MS.input_ms false input\
	    \\n val output_ms : TextIO.outstream * cs CPN'MS.ms -> unit\
	    \ = CPN'MS.output_ms false (output,lt)"::
	    gen_all(true,(few_comps orelse (isdecl "all")),name,comp,    
	    gen_ord(true,(few_comps orelse (isdecl "ord")),name,head,comp,
	    gen_col(true,(few_comps orelse (isdecl "col")),name,comp,
	    gen_ran(true,(few_comps orelse (isdecl "ran")),name,comp,
	    gen_set((few_comps orelse (isdecl "set")),head,comp,"\n end;"::nil)))))))))))))),(map #1 comp))
end handle CPN'Error str => (id,str,([],[]))

fun append_record_decl(id,name,comp,decls) = let
    val isdecl = is_declared decls
    val head = gen_head (comp,"")
    val _ = CPN'CSTable.append_declare(name,decls)
    val few_comps = (List.length comp)<10
in
    compile_decl(id,"structure "::name::" = struct\n\
     \ open "::name::";"::
    gen_input(false,true,name,comp,
    gen_mult((few_comps orelse (isdecl "mult")),
	     (few_comps orelse (isdecl "all")),name,comp,
    gen_all(false,(few_comps orelse (isdecl "all")),name,comp,
    gen_ord(false,(few_comps orelse (isdecl "ord")),name,head,comp,
    gen_col(false,(few_comps orelse (isdecl "col")),name,comp,
    gen_ran(false,(few_comps orelse (isdecl "ran")),name,comp,	    
    gen_set((few_comps orelse (isdecl "set")),head,comp,"\n end;"::nil))))))), false)
end

end (* local record cs *)

end (* local product and record cs *)

(******************** union color-set declaration ********************)

local
    fun gen_all (false,false,_,_,tail) = tail
      | gen_all (true,false,name,_,tail) = 
	"\n fun all () =\
	 \ CPN'ColorSets.error_not_decl(\"all\",\""::name::"\")"::tail 
      | gen_all (_,true,name,comp,tail) = let
	fun gen ((label,"")::rcs,tail) =
	     "("::label::")::("::gen(rcs,")"::tail)
	  | gen ((label,cs)::rcs,tail) =
	    "foldr (fn (CPN'col,CPN'tail) =>\
	     \ ("::label::" CPN'col)::CPN'tail) ("::
	    gen(rcs,")"::"("::cs::".all())"::tail)
	  | gen (_,tail) = "nil"::tail 
    in
	 "\n local\n\
	 \ val all_ref = ref(NONE: "::name::" CPN'MS.ms option)\n\
	 \ in\n\
	 \ fun all () = case !all_ref of\n\
	 \ SOME(all') => all'\n\
	 \ | NONE =>\n\
	 \ let\n\
	 \ val all' = "::gen(comp,
	 "\n val _ = all_ref:= SOME(all')\n in all' end\n end"::tail)
    end

    fun gen_input (false,false,_,_,tail) = tail
      | gen_input (true,false,name,_,tail) = 
	"\n fun input _ =\
	 \ CPN'ColorSets.error_not_decl(\"input\",\""::name::"\")"::tail 
      | gen_input (_,true,name,comp,tail) = let
	fun gen (nil,tail) = "\n | _ => raise CPN'StreamIO.IOError\
	 \(\"Illegal identifier '\"^CPN'label^\"' in union\")"::tail
	  | gen ((label,"")::rcs,tail) =
	    "\n | "::"\""::label::"\" => "::
            label::gen(rcs,tail)
	  | gen ((label,cs)::rcs,tail) = 
	    "\n | "::"\""::label::"\" => "::
            label::"("::cs::".input(TextIO.openString CPN'value))"::gen(rcs,tail)
    in
	(* FIXME - stop char in get_until doesn't work for 
	 * identifiers without type *)
	"\n fun input CPN's = \n let\
	 \\n val (CPN'stop_ch,CPN'list) =\
	 \ CPN'StreamIO.get_until(CPN's,[#\"(\",#\" \"])\
	 \\n val (CPN'label,CPN'value) = case CPN'stop_ch of\
	 \\n #\" \" => (String.implode CPN'list,\"\")\
	 \\n | _ => (String.implode CPN'list,\
	 \ String.implode(#2(CPN'StreamIO.get_until(CPN's,[#\")\"]))))\
	 \\n in\n case CPN'label of \n "::tl(gen(comp,"\n end"::tail))
    end

    fun gen_ord (false,false,_,_,tail) = tail
      | gen_ord (true,false,name,_,tail) = 
	"\n fun ord _ =\
	 \ CPN'ColorSets.error_not_decl(\"ord\",\""::name::"\")"::tail 
      | gen_ord (_,true,name,comp,tail) = let
	fun gen (adds,(label,"")::rcs) = 
	    " | "::label::" => "::adds::gen("1+"^adds,rcs)
	  | gen (adds,(label,cs)::rcs) = 
	    " | "::"("::label::" CPN'a) => "::adds::"+("::cs::".ord CPN'a)"::
	    gen(cs^".size()+"^adds,rcs)
	  | gen _ = tail
    in
	 "\n val ord = fn "::tl(gen ("0",comp))
    end

    fun gen_col (false,false,_,_,tail) = tail
      | gen_col (true,false,name,_,tail) =
	"\n fun col _ =\
	 \ CPN'ColorSets.error_not_decl(\"col\",\""::name::"\")"::tail 
      | gen_col (_,true,name,comp,tail) = let
	fun gen (adds,(label,"")::rcs) = 
	    "if"::" CPN'i<1+"::adds::" then "::label::
	    " else "::gen("1+"^adds,rcs)
	  | gen (adds,(label,cs)::rcs) = 
	    "if"::" CPN'i<"::cs::".size()+"::adds::" then\
	     \ "::label::"("::cs::".col (CPN'i-("::adds::")))\n\
	     \ else "::gen(cs^".size()+"^adds,rcs)
	  | gen _ = "raise CPN'Error CPN'ColorSets.out_of_range"::tail
    in
	"\n fun col CPN'i =\n\
	 \ if 0<=CPN'i andalso"::tl(gen("0",comp))
    end

    fun gen_ran (false,false,_,_,tail) = tail
      | gen_ran (true,false,name,_,tail) =
	"\n fun ran _ =\
	 \ CPN'ColorSets.error_not_decl(\"ran\",\""::name::"\")"::tail 
      | gen_ran (_,true,name,comp,tail) = let
	fun gen (i,(label,"")::rcs) =
	    "\n | "::(Int.toString i)::" => "::label::gen(i+1,rcs)
	  | gen (i,(label,cs)::rcs) =
	    "\n | "::(Int.toString i)::" => "::label::"("::cs::".ran())"::
	    gen(i+1,rcs)
	  | gen _ = 
	    " | _ => raise Match\n\ 
	     \ in\n\
	     \ get (CPN'Random.int "::(Int.toString (length comp))::")\n\
	     \ end"::tail
    in
	"\n fun ran () = let\n\
	 \ val get = fn "::tl(gen(0,comp))
    end

    fun gen_of (false,_,_,tail) = tail
      | gen_of (true,name,comp,tail) = let
	fun gen ((label,"")::rcs) =
	    "\n val of_"::label::" = fn "::label::" => true\
	     \ | _ => false"::(gen rcs)
	  | gen ((label,_)::rcs) =
	    "\n val of_"::label::" = \
	     \ fn CPN'x as ("::label::" _) => legal CPN'x\
	     \ | _ => false"::(gen rcs)
	  | gen _ = tail
    in
	gen comp
    end
in

fun create_union (id, comp, {name,timed,var,msvar,alias,declare}) = let
    val cs_not_decl = List.filter (fn (CPN'id,CPN'cs) => 
				   (CPN'cs<>"") andalso
				   ((CPN'CSTable.peek CPN'cs)=NONE)) comp

    val err_msg =  foldr (fn ((CPN'id,CPN'cs),CPN'tail) => 
			  ("\n"::
			   (cs_not_declared_msg CPN'cs)::
			   CPN'tail)) [] cs_not_decl

    val _ = if cs_not_decl=[] then () 
	    else raise CPN'Error (concat (tl err_msg))

    fun gen_cs (comp,tail) = let
	fun gen ((label,"")::rcs) = " | "::label::(gen rcs)
	  | gen ((label,cs)::rcs) =
	    " | "::label::" of "::cs::(gen rcs)
	  | gen _ = tail
    in
	"\n datatype "::name::" = "::tl(gen comp)
    end

    fun gen_base ((label,"")::_,tail) =
	"\n val base = "::label::tail
      | gen_base ((label,cs)::_,tail) = 
	"\n val base = "::label::"("::cs::".base)"::tail
      | gen_base _ = raise Match

    fun gen_lt (comp,tail) = let
	fun gen ((label,"")::nil) =
	    "\n | "::"(_,_) => false"::tail
	  | gen ((label,"")::rcs) =
	    "\n | "::"(_,"::label::") => false\n\
	    \ | ("::label::",_) => true"::(gen rcs)
	  | gen ((label,cs)::nil) =
	    "\n | "::"("::label::" CPN'a,"::label::" CPN'b) =>\
	     \ "::cs::".lt(CPN'a,CPN'b)"::tail
	  | gen ((label,cs)::rcs) =
	    "\n | "::"("::label::" CPN'a,"::label::" CPN'b) =>\
	     \ "::cs::".lt(CPN'a,CPN'b)\n\
	     \ | "::"(_,"::label::" _) => false\n\
	     \ | ("::label::" _,_) => true"::(gen rcs)
	  | gen _ = raise Match
    in
	"\n val lt = fn "::tl(gen comp)
    end

    fun gen_cmp (name,tail) =
       ("\n val cmp:"::name::" * "::name::"-> order = (CPN'Misc.a_cmp lt)"::tail)

    fun gen_illegal(comp,tail) = 
	"\n fun illegal_msg CPN'u = \
	 \\n if legal CPN'u then \"\"\
	 \\n else \"Illegal union color: \"^mkstr CPN'u^\".\""::tail

    fun gen_legal (comp,tail) = let
	fun gen ((label,"")::rcs) =
	    "\n | "::label::" => true"::(gen rcs)
	  | gen ((label,cs)::rcs) =
	    "\n | "::"("::label::" CPN'a) =>\
	     \ "::cs::".legal CPN'a"::(gen rcs)
	  | gen _ = gen_illegal(comp,tail)
    in
	"\n val legal = fn "::tl(gen comp)
    end

    fun gen_mkstr (comp,tail) = let
	fun gen ((label,"")::rcs) =
	    "\n | "::label::" => \""::label::"\""::(gen rcs)

	  | gen ((label,cs)::rcs) = 
	    "\n | "::"("::label::" CPN'a) =>\
	     \ \""::label::"(\"^("::cs::".mkstr CPN'a)^\")\""::(gen rcs)
	  | gen _ = tail
    in
        "\n val mkstr = fn "::tl(gen comp)
    end

    fun gen_size (comp,tail) = let
	fun gen ((_,"")::rcs) = "+"::"1"::(gen rcs)
	  | gen ((_,cs)::rcs) = "+"::cs::".size()"::(gen rcs)
	  | gen _ = tail
    in
	"\n fun size () = "::tl(gen comp)
    end

    val isdecl = is_declared declare

in
    each_cs(name, CPN'CSTable.union_cs comp,
	    id,timed,var,msvar,alias,declare,
	    gen_cs(comp,
	    "\n structure "::name::" = struct\
	     \\n type cs = "::name::
	    gen_base(comp,
	    gen_lt(comp,
	    gen_cmp(name,
	    gen_mkstr(comp,
            "\n val mkstr_ms : cs CPN'MS.ms -> string\
	     \ = CPN'MS.mkstr_ms (mkstr,lt)"::
	    gen_legal(comp,
	    gen_size(comp,
	    gen_input(true,true,name,comp,
	    "\n fun output(CPN's,CPN'c) = \
	    \\n if legal CPN'c then CPN'IO.output(CPN's,(mkstr CPN'c)^\" \")\
	    \\n else raise CPN'Error (illegal_msg(CPN'c))\
	    \\n val input_ms : TextIO.instream -> cs CPN'MS.ms = CPN'MS.input_ms false input\
	    \\n val output_ms : TextIO.outstream * cs CPN'MS.ms -> unit\
	    \ = CPN'MS.output_ms false (output,lt)"::
            gen_all(true,true,name,comp,
	    gen_ord(true,true,name,comp,
	    gen_col(true,true,name,comp,
	    gen_ran(true,true,name,comp,
	    gen_of(true,name,comp,"\n end;"::nil))))))))))))),(map #1 comp))
end handle CPN'Error str => (id,str,([],[]))

fun append_union_decl(id,name,comp,decls) = let
    val isdecl = is_declared decls
    val _ = CPN'CSTable.append_declare(name,decls)
in
    compile_decl(id,"structure "::name::" = struct\n\
     \ open "::name::";"::
    gen_input(false,true,name,comp,
    gen_all(false,true,name,comp,
    gen_ord(false,true,name,comp,
    gen_col(false,true,name,comp,
    gen_ran(false,true,name,comp,	    
    gen_of(true,name,comp,"\n end;"::nil)))))), false)
end
end (* local for union cs *)

(***************************************************************************)

fun create_globref (id, arg as {name,exp}) =
    (CPN'RefTable.insert(id,CPN'RefTable.globref arg);
     compile_decl(id,["val ",name," = ref(",exp,")"], false))

fun create_pageref (id, arg as {name,exp,...}) =
    (CPN'RefTable.insert(id,CPN'RefTable.pageref arg);
     (* the pageref is declared to get the correct symbol-handling *)
     compile_decl(id,["val ",name," = ref(",exp,")"], false))

fun create_instref (id, arg as {name,exp,...}) =
    (CPN'RefTable.insert(id,CPN'RefTable.instref arg);
     (* the instref is declared to get the correct symbol-handling *)
     compile_decl(id,["val ",name," = ref(",exp,")"], false))

(***************************************************************************)

fun create_use (id,exp) =
    case CPN'Env.is_decl ("val _ = "^exp^" : string") of
	NONE => CPN'Env.use_file(id,exp)
      | err => (id,make_error[err],([],[])) 

(***************************************************************************)

fun create_sml (id, decl) =
    case CPN'Env.is_decl ("val _ = ("^decl^");") of (* this check is to catch simple expressions *)
       (SOME _) => 
       let
	   val reserved = filter_reserved (CPN'Dep.find_defines decl)
			(*  handle Compiler.Compile.Compile str => []*)
	in
	   if reserved <> []
	    then (id, concat reserved, ([],[]))	    
	    else compile_decl(id, [decl], true)
       end
      | NONE => (id, "Not a declaration",([],[])) 

(***************************************************************************)

fun create_global decls = let

    fun create(id, sml_code decl, NONE) = create_sml (id, decl)
      | create(id, usefile exp, NONE) = create_use(id,exp)
      | create(id, globref arg, NONE) = create_globref(id,arg)
      | create(id, unit_cs arg, SOME par) = create_unit(id,arg,par)
      | create(id, bool_cs arg, SOME par) = create_bool(id,arg,par)
      | create(id, int_cs arg, SOME par) = create_int(id,arg,par)
      | create(id, intinf_cs arg, SOME par) = create_intinf(id,arg,par)
      | create(id, real_cs arg, SOME par) = create_real(id,arg,par)
      | create(id, string_cs arg, SOME par) = create_string(id,arg,par)
      | create(id, list_cs arg, SOME par) = create_list(id,arg,par)
      | create(id, index_cs arg, SOME par) = create_index(id,arg,par)
      | create(id, funsubset_cs arg, SOME par) = create_funsubset(id,arg,par)
      | create(id, listsubset_cs arg,SOME par) = create_listsubset(id,arg,par)
      | create(id, enum_cs arg, SOME par) = create_enum(id,arg,par)
      | create(id, product_cs arg, SOME par) = create_product(id,arg,par)
      | create(id, record_cs arg, SOME par) = create_record(id,arg,par)
      | create(id, union_cs arg, SOME par) = create_union(id,arg,par)
      | create(id, time_cs, SOME par) = create_time(id,par)
      | create(id, alias_cs arg, SOME par) = create_alias(id,arg,par)
      | create(id, append_var (name,var), NONE) = 
	(case (CPN'CSTable.peek name)
	  of (SOME _) =>
	     let
		 fun cr_var_val_check (v::rest)
		   = "val "^v^" = "^name^".base;\n"^(cr_var_val_check rest)
		   | cr_var_val_check [] = "";
		     
		 (* check whether v is a constructor that is a 
		  * function, e.g. v = fn: int -> an index cs *)
                 fun var_constructor_fn_check [] = ""
		   | var_constructor_fn_check (v::rest) = 
		     "val _ = "^v^";\n"^(var_constructor_fn_check rest)

		 val reserved = filter_reserved var
	in
	    if reserved <> []
	    then (id, concat reserved, ([],[]))	    
	    else 
		 (id,"",
		  case (CPN'Dep.find_dependencies (id, var_constructor_fn_check var))
		   of ([], []) => 
		      (case (CPN'Dep.find_dependencies (id, cr_var_val_check var)) of
			   (uses,[]) => (CPN'CSTable.append_var(name,var);
					 app (fn v => CPN'VarTable.insert(v,{cs = name,
	 	 							     decl_id = id})) var;
					 
					 (uses, []))
			 | res => res)
		    | (uses,[]) => ([],uses)
		    | _ => raise InternalError "create_global append_var")
	     end
	   | none =>
	     (id, "Color set \""^name^"\" not defined", ([],[])))

(*	(CPN'CSTable.append_var(name,var);
	 app (fn v => CPN'VarTable.insert(v,{cs = name,
					     decl_id = id})) var;
	 (id,"",([#id (CPN'CSTable.find name)],[])))   *)
      | create(id, append_msvar (name,msvar), NONE) = 
	(* Temporarily disable declarations of msvars
	(CPN'CSTable.append_msvar(name,msvar);
	 app (fn v => CPN'VarTable.insert(v,{cs = name^" CPN'MS.ms",
					     decl_id = id})) msvar;
	 (id,"",([#id (CPN'CSTable.find name)],[]))) *)
	raise CPN'Error "Error: Multi-set variables (msvar) are not currently supported!"
      | create(id, append_alias (name,alias), NONE) = 
	(CPN'CSTable.append_alias (name,alias);
	 create_append_alias (id,name,alias))
      | create(id, append_decl (name,decl), NONE) = 
	let
	    val (_,err,syms) = case #kind(CPN'CSTable.find name) of
		CPN'CSTable.product_cs comp => 
		    append_product_decl (id,name,comp,decl)
	      | CPN'CSTable.record_cs comp => 
		    append_record_decl (id,name,comp,decl)
	      | CPN'CSTable.union_cs comp => 
		    append_union_decl (id,name,comp,decl)
	      | _ => raise InternalError("Decl.create append_decl");
	in
	    (CPN'CSTable.append_declare (name,decl);
	     (* the aliases must be compiled again *)
	     create_append_alias(id,name,#alias(CPN'CSTable.find name)))
	end
      | create(id, channel arg, NONE) = create_channel(id,arg)
      | create _ = raise InternalError("Decl.create")

    fun create_handle_error (id, arg, par)
	= (CPN'Env.remove_decl id;
	   create (id, arg, par))
	handle CPN'Error str => (id, str, ([],[]))
	     | (InternalError str) =>
		   (id,
		    concat["Error: exception InternalError is raised with ",
			   str],
		    ([],[]))
	     | (Compile str) =>
		   (id, concat["Error: exception Compile is raised with ",
			       str],
		    ([],[]))
	     | exn =>
		   (id, concat["Error: exception ",
			       exnName exn," is raised"],
		    ([],[]))
in
    CPN'report_timing "Creating declaration @";
    map create_handle_error decls before (CPN'report_timing " - done @"; ())
end

fun create_vars (id, cs, vars) =
    (CPN'CSTable.append_var (cs, vars);
     app (fn v => CPN'VarTable.insert(v,{cs=cs,decl_id=id})) vars);

fun create_msvars (id, cs, msvars) = let
    val cs_ms = cs^" CPN'MS.ms"
in
    (CPN'CSTable.append_msvar (cs, msvars);
     app (fn v => CPN'VarTable.insert(v,{cs=cs_ms,decl_id=id})) msvars)
end

fun create_local decls = let

    fun create(id, pageref arg) = create_pageref(id,arg)
      | create(id, instref arg) = create_instref(id,arg)
in
    map create decls
end

fun append_all cs = let

    val {kind,id,declare,...} = CPN'CSTable.find cs

    fun append_all'(append,comp) =
	if List.exists (fn x => x="all") declare then ()
	else (append(id,cs,comp,["all"]); ())
in
    (app append_all (CPN'CSTable.get_parent_cs cs);
     case kind of
	 CPN'CSTable.product_cs comp => append_all'(append_product_decl, comp)
       | CPN'CSTable.record_cs comp => append_all'(append_record_decl, comp)
       | CPN'CSTable.union_cs comp => append_all'(append_union_decl, comp)
       | _ => ())
end   
end (* CPN'Decl *)
