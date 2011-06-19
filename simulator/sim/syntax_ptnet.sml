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
(* File: syntax_ptnet.sml
 *
 * PT-net constructor and simulator.
 *)

structure CPN'Bitstring = struct

    open Bits;
    exception IllegalBit;

    fun pow2 i = lshift(1,i);
	
    (* compute word length by generating an overflow: *)
    fun word_length n w = let
	val w' = lshift (w,1)
    in
	(word_length (n+1) w') handle overflow => n
    end;
	    
    val WL = (word_length 2 1) - 1; (* subtr. 1 => MAX will compute below *)
	    
    val BSL = ref 0; (* Bit String Length *)

    val MAX = pow2 WL - 1;

    val tstBit = fn n => 
	if (n < 0 orelse n > (!BSL-1)) then 
	    raise IllegalBit 
	else ()
	    
    fun init n = BSL := n;

    fun toString bs =
	let
	    fun segToString bsword 0 _ = ""
	      | segToString bsword _ 0 = ""
	      | segToString bsword bswordlength bslength =
		((segToString (rshift(bsword,1)) (bswordlength-1) (bslength-1))^Int.toString(andb(bsword,1)))
	    fun iter nil _ _ = ""
	      | iter (head::tail) bswordlength bslength = 
		((iter tail bswordlength (bslength-bswordlength))^(segToString head bswordlength bslength))
	in
	    iter bs WL (!BSL)
	end;

    (************* bit string operations *************)
	
    (* Given a non-negative integer n and a bit-string
     * 'set_single' returns the bit-string with the n'th bit
     * set. 
     *)
	    
    fun set_single n l = let

	fun set_single' n [] =
	    if n >= WL then
		0::(set_single' (n-WL) [])
	    else
		[pow2 n]
	  | set_single' n (e::xs) =
		if n >= WL then
		    e::(set_single' (n-WL) xs)
		else
		    (orb(e,(pow2 n))::xs)
    in
	(tstBit n; set_single' n l)
    end;
		    
    (* Given a positive integer n and a bit-string
     * 'extend' returns a bit-string of length 
     * ((n-1) div WL)+1. *)
		    
    fun extend (n,[]) =
	if n>0 then (0::extend(n-WL,[])) else []
      | extend (n,e::xs) =
	if n>WL then (e::extend(n-WL,xs)) else e::xs
		    
    (* given a list of integers l, 'set_bits' returns a bit-string
     * where bit n is set iff n is in l. The bit-string returned 
     * is assured to be long enough to hold all places (= BSL)
     * by calling 'extend' *)
		    
    fun set_bits l = let

	fun set_all [] res = res
	  | set_all (e::xs) res =
	    set_all xs (set_single e res)
    in
	extend(!BSL,set_all l [])
    end

    fun get_bits bs =
    let
	fun segToNums bsword 0 _ _ = nil
	  | segToNums bsword _ 0 _ = nil
	  | segToNums bsword bswordlength bslength curbitnum =
	    (segToNums (rshift(bsword,1)) (bswordlength-1) (bslength-1) (curbitnum+1))@
	    (if (andb(bsword,1)=1) then 
		[curbitnum]
	    else nil)
	fun iter nil _ _ _ = nil
	  | iter (head::tail) bswordlength bslength curbitnum = 
	    ((iter tail bswordlength (bslength-bswordlength) (curbitnum+bswordlength))@(segToNums head bswordlength bslength curbitnum))
    in
	iter bs WL (!BSL) 0
    end

    fun and_bits (bs1,bs2) = let
	fun and' (nil,nil) = nil
	  | and' (el'::bs',el''::bs'') = (andb(el',el''))::and'(bs',bs'')
	  | and' (nil,el''::bs'') = (0::and'(nil,bs''))
	  | and' (el'::bs',nil) = (0::and'(bs',nil))
    in
	and'(bs1,bs2)
    end
	
    fun or_bits (bs1,bs2) = 
	let
	    fun or' (nil,nil) = nil
	      | or' (e1'::bs',e1''::bs'') = (orb(e1',e1''))::or'(bs',bs'')
	      | or' (nil,e1'::bs') = (e1'::or'(nil,bs'))
	      | or' (e1'::bs',nil) = (e1'::or'(bs',nil))
	in
	    or' (bs1,bs2)
	end
	    
    (* fun set_low n
     * set bits 0-(n-1) of the bit-string *)

    fun set_low 0 = extend (!BSL,[])
      | set_low n = 
	let
	    fun set_all n =
		if n <= WL then
		    [pow2 n - 1]
		else
		    MAX::(set_all (n-WL));
	in
	    (tstBit (n-1); extend(!BSL,set_all n))
	end;

    (* fun set_high n
     * set the higher n bits of the bit-string *)

    fun set_high 0 = extend (!BSL, [])
      | set_high n =
	let
	    fun set_high' n =
		if n <= 0 then []
		else if n <= WL then
		    [pow2 n -1]
		else
		    MAX::(set_high' (n-WL));
		
	    (* spool across the zero bits. This also sets the necessary
	     * bits in the entry that has both zeros and ones *)
			
	    fun spool zerobits =
		if zerobits < WL then
		    (* part of this word should be set*)
		    if (n+zerobits) <= WL then
			[xorb (pow2 (n + zerobits) - 1,
			       (pow2 zerobits - 1))]
		    else 
			(xorb(MAX,pow2 zerobits -1))::
			(set_high' (n - (WL-zerobits)))
		else
		    0::(spool (zerobits - WL))
	in
	    (tstBit (n-1); extend(!BSL,spool (!BSL - n)))
	end
end;

signature CPN'PTNET = sig
    type bop
	
    val con_ptnet : 
	int -> int * int * (bop * int list * int list) list
    val sim : 
	int * int * (bop * int list * int list) list -> int * bop list
end 

structure CPN'PTnet = struct

    open CPN'TransitionTable Bits 
	CPN'SyntaxDatatypes CPN'SyntaxTables;
	     
    structure IntPQ = MakeTreePQ (type key = int; val priority = Int.<);

    exception NotBindable;

    (* Low weight => higher priority *)
    fun weight (B_p _) = 100
      | weight (B_g _) = 1
      | weight (B_c _) = 1000
      | weight (B_k _) = 20
      | weight (T_a _) = 10
      | weight (T_g _) = 2
      | weight (T_v _) = 100; (* was 5 *)

    val flatten = CPN'Misc.flatten

    (* makestring funs for debugging. *)

    fun mkstr_vars [] = ""
      | mkstr_vars l = 
	concat (tl(foldr (fn (v,l) => ","::v::l) [""] l));
		
    fun mkstr_keys [] = ""
      | mkstr_keys l = 
	concat (tl(foldr (fn ({exp,label=_,no=_},l) => 
			  ","::"{exp = "::exp::",...}"::l) [""] l))

    fun mkstr_aexp (NonDiv{exp,...}) = exp
      | mkstr_aexp (Div(_,_,exp,_)) = exp

    fun mkstr_gitem (NonBindable{exp,...}) = exp
      | mkstr_gitem (Bindable(exp1,exp2)) = concat [#exp exp1," = ",#exp exp2]

    fun mkstr_bop (B_p {pat = p, vars = l,...}) = 
	concat ["B_p {pat = ",p,", vars = [",mkstr_vars l,"],...}"]
      | mkstr_bop (B_g {pat = p, vars = l, rebind_vars = l', exp}) = 
	concat ["B_g {pat = ",p,", vars = [",mkstr_vars l,"], rebind_vars = [",mkstr_vars l',"], exp = ",exp,"}"]
      | mkstr_bop (B_c {var = v,...}) = 
	concat ["B_c {var = ",v,",...}"]
      | mkstr_bop (B_k {pat = p, vars = l,keys = klist,...}) = 
	concat ["B_k {pat = ",p,", vars = [",mkstr_vars l,"],\
	         \ keys = [", mkstr_keys klist, "],...}"]
      | mkstr_bop (T_a {exp = e,...}) = 
	concat ["T_a {exp = ",e,",...}"]
      | mkstr_bop (T_g {exp = e,...}) = 
	concat ["T_g {exp = ",e,",...}"]
      | mkstr_bop (T_v {var1 = v1, var2 = v2}) = 
	concat ["T_v {var1 = ",v1,", var2 = ",v2,"}"];

    fun mkstr_seq res = 
	concat(foldr (fn (b,l) => "\n\t"::(mkstr_bop b)::"; "::l) [] res);

    fun list_mkstr l =
	concat ("["::tl(foldr (fn (n:int,l) => 
			       ","::(Int.toString n)::l) ["","]"] l))

    fun list_bit_mkstr l = let

	fun make (i,x,tail) =
	    if i>0 then
		make(Bits.lshift(i,1),x,(if Bits.andb(i,x) <> 0 then "1" else "0")::tail)
	    else tail
    in
	concat (foldr (fn (a,b) => make(1,a,b)) nil l)
    end

    fun debug_mark (m1: int list, m2: int list) = let
	val str1 = list_mkstr m1;
	val str2 = list_mkstr m2;
    in
	CPN'debug (concat ["m1: ", str1, "\nm2: ",str2])
    end;

    fun debug_current (msg, cur_weight: int, cur_mark, cur_seq) = 
	CPN'debug (concat [msg,"\n",
			   "current weight: ", Int.toString cur_weight, "\n",
			   "current mark  : ", list_mkstr cur_mark, " = ",list_bit_mkstr cur_mark,"\n",
			   "current seq   : ", mkstr_seq cur_seq, "\n"])

    fun debug_net [] = CPN'debug "empty net"
      | debug_net net = let
	
	fun debug_trans (trans,input,output) =
	    (CPN'debug (concat [" - Trans: ",mkstr_bop trans]);
	     CPN'debug (concat [" - Input: ",list_mkstr input]);
	     CPN'debug (concat [" - Output: ",list_mkstr output]))
    in
	(CPN'debug "PTNET: ";  app debug_trans net)
    end
		 
    (* Given a var id, return the variable name from VarTable *)
    fun get_var_from_vid vid =
	let
	    val found= List.find 
		(fn ({var,varid,...}:CPN'SyntaxTables.var_item) =>
		   vid=varid)
		(CPN'StringTable.listItems VarTable)
	in
	    case found of
		SOME elm => #var elm
		  | _ => raise InternalError ("get_var_from_vid: vid not found in VarTable: "^(Int.toString vid))
	end

    fun get_vid_from_var the_var =
	let
	    val found = List.find
			    (fn ({var,varid,...}:CPN'SyntaxTables.var_item) =>
				the_var = var)
			    (CPN'StringTable.listItems VarTable)
	in
	    case found of 
		SOME elm => #varid elm
	      | _ => raise InternalError ("get_vid_from_var: var not found in VarTable: "^the_var)
	end

    val t : unit CPN'IntListTable.hash_table = 
		 CPN'IntListTable.mkTable(29,InternalError "PT-net Hash Table")

    (* fun find_min_occur_seq (net, pq, done)
     * net: (trans, input, output) list
     *       trans : the binding operator
     *       input : int list denoting the pt-net marking before this 
     *               transition occours
     *       output: the pt-net marking after this transition occours 
     * pq : a priority queue holding the markings reached so far (together
     *      with the occurrence sequences leading to these markings)
     * done : int list denoting the wanted marking 
     *
     * description:
     * 
     * find_min_occour_seq develops parts of the O-graph for net until
     * the marking 'done' is found. In each iteration it chooses the
     * marking with minimum weight in pq and for each of 
     * *)

    val debug_steps = ref 0; 
		 
    fun find_min_occur_seq (net, pq, done_mark) = let

	val (cur_weight,(cur_mark,cur_seq)) = IntPQ.deletemin(pq)
	    handle _ => (CPN'debug "InternalError: empty PT PQ (did not find complete BOPS sequence where all variables are bound)";
			 raise SyntaxError "No complete binding sequence found for this transition.\nA typical cause to this is a variable which cannot be bound.");

	(* traverse all transition in the current marking, and make
	 * all enabled transitions occur *)
	fun traverse_trans (nil) = ()
	  | traverse_trans ((trans,input,output)::tios) = 
	    let
		
		(* NOTE: The following statement is no longer valid. As a
		 * special case we allow double binding of vars via the guard.
		 * Hence not all places has capacity one.
		 *)
		(* This version of bind_trans assumes that double binding of 
		 * variables is not allowed, i.e., all places has a max capacity 
		 * of one. This is safe as long as the correct B_k operation is 
		 * generated.
		 * The function is recursive in the length of the Bitstring
		 * which is represented as a list of integers, see structure
		 * above. *)
		fun bind_trans (_,nil,nil,nil) = (nil,nil)
		  | bind_trans (i,tin::tins,tout::touts,mark::marks) =
		    if orb(notb tin,mark) = ~1 then (* the transition is enabled *)
			case trans of
			    B_g _ =>
			    (* only case where vars can be rebound *)
			    let 
				val (marks',seq') = bind_trans(i+1,tins,touts,marks)
			    in
				(* make the transition occur *)
				(orb(xorb(mark,tin),tout)::marks',seq')
			    end
			  | _ =>    (* normal case: vars cannot be rebound *)
			    if andb(xorb(mark,tin),tout) = 0 then (* the capacity is ok *)
				let 
				    val (marks',seq') = bind_trans(i+1,tins,touts,marks);
				in
				    (* make the transition occur *)
				    (orb(xorb(mark,tin),tout)::marks',seq')
				end
			    else raise NotBindable
		    else raise NotBindable
		  | bind_trans _ = raise InternalError "bind_trans"
					 
		(* If trans is B_g then assign rebinding vars if needed *)
		fun insert_rebind_vars (bop,out,mark) =
		    case bop of
			B_g {pat,vars,rebind_vars,exp} => 		    
			let
			    val vids = map get_vid_from_var vars
			    val varbits = foldr (fn (x,ac) => CPN'Bitstring.or_bits((CPN'Bitstring.set_bits [x]),ac)) nil vids
			    val rbvars = map get_var_from_vid (CPN'Bitstring.get_bits(CPN'Bitstring.and_bits(varbits,mark)))
			in
			    B_g {pat=pat,vars=vars,rebind_vars=rbvars,exp=exp}
			end
		      | _ => bop
				     
			
		fun step () = let
		    val (new_mark,this_seq) = bind_trans(1, input, output, cur_mark)
		    val new_weight = foldr (fn (a,b) => weight a + b) cur_weight (trans::this_seq)
		    val new_seq = List.@(this_seq,(insert_rebind_vars(trans,output,cur_mark))::cur_seq)
		in
		    inc debug_steps;
		    IntPQ.insert(pq, new_weight, (new_mark,new_seq))
		end
		    
		val _ = step () handle NotBindable => ()
	    in
		traverse_trans (tios)
	    end
		
        fun fetch_all_done nil = nil
	  | fetch_all_done ((w,(m,s))::wmss) =
	    (if m = done_mark then
		 (w, List.rev s)::fetch_all_done(wmss)
	     else
		 fetch_all_done(wmss))
	    handle _ => raise InternalError "fetch_all_done"
	val _ = CPN'debug ("find_min_occur_seq:cur_mark= "^(CPN'Bitstring.toString cur_mark))
    in
	if cur_mark = done_mark then
	    (cur_weight,List.rev cur_seq)
	    
        (* (cur_weight,List.rev cur_seq)::
	 * fetch_all_done(deleteto(pq,cur_weight)) *)
	else 
	    (* a marking is only looked upon once. 
	     * If it is met again it is thrown away: *)
	    ((case CPN'IntListTable.find t cur_mark of 
		  NONE => 
		  (CPN'IntListTable.insert t (cur_mark,());
		   traverse_trans net)
		| SOME _ => (); 
              find_min_occur_seq(net,pq,done_mark))
	     handle (InternalError s) => raise (InternalError s)
		  | (SyntaxError s) => raise (SyntaxError s)
		  | exn => raise InternalError ("sim:find_min_occur_seq: "^(exnName exn)^" : "^(exnMessage exn)))
	    
    end; (* find_min_occur_seq *)
	
    fun sim (n_var, n_exp, tios) = let

	(* Print out PT net: Input/Output numbers are written as integers
	 * but should be interpreted as binary numbers, where each binary
         * digit represents a place. *)
	val _ = debug_net tios;
	val _ = debug_steps:= 0;
	val _ = CPN'debug (concat["PT sim n_var=",Int.toString n_var,", n_exp=",Int.toString n_exp])
	val _ = CPN'report_timing "PT sim start @"

	val _ = CPN'IntListTable.filter (fn _ => false) t;

	val init_mark = CPN'Bitstring.set_high n_exp;
	val done_mark = CPN'Bitstring.set_low n_var;
	val _ = CPN'debug ("sim:init_mark= "^(CPN'Bitstring.toString init_mark))
	val _ = CPN'debug ("sim:done_mark= "^(CPN'Bitstring.toString done_mark))
	    
	val pq: (int list * bop list) IntPQ.pq = IntPQ.create()
	val _ = IntPQ.insert(pq, 0, (init_mark, nil))

	val (w,b) = find_min_occur_seq(tios, pq, done_mark)

	val _ =  CPN'debug (concat["BOPS: (",Int.toString(!debug_steps),
				    " sim steps at weight ",Int.toString w,") ",mkstr_seq b]);
	val _ = CPN'report_timing (concat[" - done ",Int.toString(!debug_steps)," steps @"]);
    in
	(w,b)
    end handle (InternalError s) => raise (InternalError s)
             | (SyntaxError s) => raise (SyntaxError s)
             | _ => raise (InternalError "sim")

    (**** end of oga funs ****)
	    
    fun div_2 n = rshift (n, 1)
    fun mult_2 n = lshift (n, 1)
	    
    (* divide a tuple/record into constant/non-constant parts: *)
    fun const_nonconst (Rec entries) = 
	let
	    fun divide (res, []) = res
	      | divide ((consts,non_consts), e::xs) =
		case e of
		    {vars = [],exp, no, label} =>
			divide ((e::consts, non_consts),xs)
		  | _ => divide ((consts, e::non_consts),xs)
	in
	    divide (([],[]), entries)
	end
      | const_nonconst _ = 
	raise (InternalError "Match error in const_nonconst")

    (* collect the variables of a non-dividable expression: *)
    fun collect_vars (NonRec v_list) = v_list
      | collect_vars (Rec rec_list) = flatten(map #vars rec_list)

    (* collect the variables of an expression: *)
    fun collect_allvars (NonDiv {entries,...}) = collect_vars entries
      | collect_allvars (Div (e1, e2,_,_)) =
	(collect_allvars e1)^^(collect_allvars e2)

    (***** funs operation on the VarTable table *****)
    fun get_vids vlist =
	map (#varid o (CPN'StringTable.lookup VarTable)) vlist
	handle HashNotFound => 
	    raise InternalError (concat("fun get_vids ["::
				 (tl(foldr (fn(a,b)=>","::a::b) ["","]"] vlist)
				 ^^(" <? ["::tl(CPN'StringTable.foldi (fn(a,_,b)=>","::a::b) (["]"]) (VarTable))))))

    fun getset_cpn vlist = let

	fun getset_cpn' (v,(l1,l2)) =
	    case CPN'StringTable.find VarTable v of
		SOME {bindable, var_type, varid, ...} => 
		    (bindable:= true;
		     case var_type of
			 cpn => (v::l1, varid::l2)
		       | _ => (l1, varid::l2))
	      | NONE => raise InternalError "fun getset_cpn";

    in
	foldr getset_cpn' ([],[]) vlist
    end; (* getset_cpn *)

    fun no_of_exps v = !(#no_of_exps (CPN'StringTable.lookup VarTable v))
	handle HashNotFound => raise InternalError "fun no_of_exps";
	    
    (* find candidate variables for keys in key-lookups: *)
    fun key_vars [] = true
      | key_vars (v::xs) = ((no_of_exps v) > 1) andalso key_vars xs
	
    fun con_ptnet (grp_id) = let

	val _ = CPN'report_timing "PT con start @"

	val grp as {no_of_vars, arc_exps, guard_items,...} = 
	    CPN'IntTable.lookup GroupTable grp_id
	    handle HashNotFound => 
		raise InternalError ("fun con_ptnet "^(Int.toString grp_id));

	val ae_length = length arc_exps;
	val gi_length = length guard_items;
		    
	val _ = CPN'Bitstring.init (!no_of_vars + ae_length + gi_length);

	(********* make transitions for arc-expressions *********)

	(******** B_p *********)
	    
	(* In this version we demand that 
	 * 1. there's no variables in the coefficient-exp
	 * 2. all vars in the @+ exp is known.
         * 3. there is a coefficient (i.e., not a ms-like variable).
         *)

	fun ins_pat (_, {pattern = false,...}:exptype, res, _) = res
	  | ins_pat (_, {coef = ("", _),...}, res, _) = res
	  | ins_pat (_, {coef = (_,e::xs),...}, res, _) = res
	  | ins_pat (place_id, 
		     {exp, entries, wldcd, coef = (s,_), timed, pattern},
		     (cur_ptplace, analyse_list), isdiv) =
	    let
		val (time_vars, time) = 
		    case timed of 
			NONE => ([],NONE)
		      | SOME (t,vars) => (get_vids vars, SOME t);
			    
		val in_bits = (cur_ptplace-1)::time_vars;
			    
		val (cpn_vars, all_ids) = getset_cpn (collect_vars entries);

		val out_bits = 
		    if isdiv then 
			(cur_ptplace-1)::flatten[all_ids, time_vars]
		    else 
			flatten [all_ids, time_vars];
	    in
		(cur_ptplace,
		 (B_p {pat = exp, coef = s, 
		       vars = cpn_vars, 
		       isdiv = if isdiv then SOME(1,1) else NONE, 
		       (* SOME (1,1) is just a dummy value, which
			* is later changed to hold the total number
			* of B_p's in the dividable and the index of 
			* this B_p in the dividables
			*)
		       time = time, place = place_id},
		  CPN'Bitstring.set_bits in_bits,      (* in *)
		  CPN'Bitstring.set_bits out_bits)    (* out *)
		 ::analyse_list)
	    end;
                    
	(***** B_k *****)
    
	(* functions for making powerset: *)
    
	fun insert (e, []) = []
	  | insert (e, a::xs) = (e::a)::(insert(e,xs))
	    
	fun union (e, []) = [e]
	  | union (e, a::xs) = (e^^a)::(union(e,xs))
	    
	fun subsets [] = []
	  | subsets (e::xs: rectype list) =
	    let
		val s = subsets xs
	    in
		if (key_vars (#vars e)) then
		    [[e]]^^(s^^(insert(e,s)))
		else
		    s
	    end;
	    
	fun ins_one (place_id, 
		     {exp, entries, coef = (cf,_), timed,...}:exptype,
		     rest_entries, (cur_ptplace, analyse_list), isdiv) =
	    let
		val (time_vars, time) = 
		    case timed of 
			NONE => ([], NONE)
		      | SOME (t,vars) => (get_vids vars, SOME t);
				    
		val (cpn_vars, all_ids) = getset_cpn (collect_vars entries);

		val out_bits = 
		    (* compute the new marking. If the expression is
		     * dividable, don't remove a token from the 
		     * expression place (cur_ptplace-1) *)
		    if isdiv then
			flatten [[cur_ptplace-1], time_vars, all_ids]
		    else
			flatten [time_vars, all_ids];
				
		(* find the cpn-vars _not_ used as keys: *)
			
		fun cpnvars_compl l =
		    CPN'Misc.filter
		    (fn x => not (List.exists (fn y => y = x) l)) cpn_vars
		    
		(* given a set of entries to be used as keys, 
		 * extract information about these entries and 
		 * the variables in them : *)

		fun get_labels (res, []) = res
		  | get_labels ((labels_exp,entryvars),
				{exp,label,no,vars}::xs) =
		    get_labels (({exp=exp,no=no,label=label}::labels_exp,
				 vars^^entryvars), xs)
			    
		(* insert each of the found combinations of key-lookups:  *)

		fun ins ([], res) = (cur_ptplace, res)
		  | ins ([]::xs, res) = ins (xs,res)
		  | ins (entry_list::xs, analyse_list) =
		    let
			(* entry_list is a combination of entries
			 * upon which a key-lookup can be made *)
			
			val (labels_exp, entryvars) = 
			    get_labels (([],[]),entry_list)
			    
			val in_bits = 
			    flatten [[cur_ptplace-1],
				     time_vars,
				     get_vids entryvars];
		    in
			ins (xs,
			     (B_k {pat = exp, coef = cf,
				   vars = cpnvars_compl entryvars,
				   keys = labels_exp,
				   time = time,
				   place = place_id},
			      CPN'Bitstring.set_bits in_bits,
			      CPN'Bitstring.set_bits out_bits)::
			     analyse_list)
		    end (* ins *)
	    in
		ins (rest_entries, analyse_list)
	    end; 
	(* end ins_one *)

	fun ins_key (_, {coef = (_,_::_),...}, res, _) = res
	  | ins_key (_, {entries = NonRec _,...}, res, _) = res
	  | ins_key (_, {entries = Rec [e],...}, res, _) = res
	  | ins_key (place_id, exp as {entries,...}, res, isdiv) =
	    (case const_nonconst entries of
		 (_,nil) => res
	       | (consts, non_consts) => 
		     ins_one (place_id, exp, 
			      union (consts, subsets non_consts), res, isdiv))
	
	(***** T_a *****)
    
	fun ins_tst (place_id, 
		     {exp,entries,coef=(cf,cf_vars),wldcd,timed,...}: exptype,
		     (cur_ptplace, analyse_list)) =
	    let
		val (time, time_vars) = 
		    case timed of 
			NONE => (NONE, [])
		      | SOME (t,vars) => (SOME t, vars);
			    
		val vars = 
		    get_vids(flatten[time_vars,cf_vars,collect_vars entries])

	    in
		(cur_ptplace+1,
		 (T_a {exp = exp, coef = cf, time = time, place = place_id}, 
		  CPN'Bitstring.set_bits (cur_ptplace::vars), (* in *) 
		  CPN'Bitstring.set_bits vars)                (* out *)
		 ::analyse_list)
	    end
		
	(****** T_a for dividable expressions ******)
    
	fun ins_divtst (place_id, aexp, time_inf, vars,
			(cur_ptplace, analyse_list)) =
	    let
		val vars = get_vids vars;
	    in
		(cur_ptplace+1,
		 (T_a {exp = aexp, coef = "", time = time_inf, 
		       place = place_id}, 
		  CPN'Bitstring.set_bits (cur_ptplace::vars), (* in *) 
		  CPN'Bitstring.set_bits vars)            (* out *)
		 ::analyse_list)
	    end;

	(****** ******)
	    
	fun ins_div (place_id, e, res) =
	    case e of
		Div (e1, e2, _, _) => 
		    ins_div (place_id, e1, 
			     ins_div (place_id, e2,res))
	      | NonDiv e =>
		    ins_pat (place_id, e, ins_key(place_id,e,res,true), true)

	(****** insert arc expressions ******)
			    
        fun ins_aexps ([], res) = res
	  | ins_aexps ({place,parsed_aexps,arcs,no_of_tokens}::xs, res) =
	    ins_aexps
	    (xs, 
	     case parsed_aexps of 
		 Div (e1, e2, aexp, time_inf) => 
		     ins_div(place, e1, 
			     ins_div (place, e2,
				      ins_divtst(place,
						 aexp, 
						 time_inf,
						 collect_allvars parsed_aexps,
						 res)))
	       | NonDiv e' =>
		     ins_pat(place, e', 
			     ins_key(place, e', ins_tst(place,e',res), false),
			     false))

	(********* make transitions for guard items ************)
		    
	fun ins_gbind (({is_pat = false,...}:guard_exp,_), res) = res
	  | ins_gbind (({exp = e1, vars = vlist1,...},
			{exp = e2, vars = vlist2,...}:guard_exp), 
		       (cur_ptplace, analyse_list)) =
	    let
		(* get ids of vlist2: *)
		val in_bits = get_vids vlist2;
		    
		(* get list of cpn-vars, extract ids of vlist1     *)
		(* and, as a side-effect, mark vlist1 as bindable: *) 
		    
		val (cpn_vars, out_bits) = getset_cpn vlist1;
		val in_ptplaces= CPN'Bitstring.set_bits ((cur_ptplace-1)::in_bits)
		val out_ptplaces= CPN'Bitstring.set_bits(Misc.flatten[in_bits, out_bits])
	    in
		(cur_ptplace,
		 (B_g {pat = e1, vars = cpn_vars, 
		       rebind_vars = [], (*rebound vars are set during PT sim*)
		       exp = e2},
		  in_ptplaces,
		  out_ptplaces)
		 ::analyse_list)
	    end
		    
	fun ins_gtst ({exp, vars,...}:guard_exp,
		      (cur_ptplace, analyse_list)) =
	    let
		val var_bits = get_vids vars;
	    in
		(cur_ptplace+1,
		 (T_g {exp = exp},
		  CPN'Bitstring.set_bits(cur_ptplace::var_bits),
		  CPN'Bitstring.set_bits var_bits)
		 ::analyse_list)
	    end;
	    
	fun ins_gitems ([],res) = res
	  | ins_gitems ({parsed_item = NonBindable exp,item_str}::xs, res) = 
	    ins_gitems (xs,ins_gtst ( exp, res))
	  | ins_gitems ({parsed_item = Bindable
			 (g1 as {exp = e1, vars = vlist1,...},
			  g2 as {exp = e2, vars = vlist2,...}),item_str}::xs,
			res) =
	    ins_gitems 
	    (xs, 
	     ins_gbind ((g1,g2),
			ins_gbind ((g2,g1),
				   ins_gtst ({exp = concat [e1," = ",e2],
					      vars = vlist1^^vlist2,
					      is_pat = false},
					     res))))
	    
	(* insert 'bind color' transitions: *) 
	fun ins_bcs (nil, analyse_list) = analyse_list
	  | ins_bcs (v::vs, analyse_list) =
	    let
		val cs = #cs(CPN'VarTable.find v)
		    handle HashNotFound => raise InternalError "fun bind_color"
		val (_, out_bits) = getset_cpn [v];
	    in
		if (CPN'Env.is_bindable_cs cs) then
		     ins_bcs (vs,
			      (B_c {var = v, cs = cs},
			       CPN'Bitstring.set_bits [],
			       CPN'Bitstring.set_bits out_bits)
			      ::analyse_list)
		else raise SyntaxError ("Cannot bind "^v^
					" from large color-set") 
	    end
	
	val (_,analyse_list) = 
	    ins_gitems (guard_items, ins_aexps (arc_exps,(!no_of_vars,[])))

	(* insert B_c operators for all variables not bound above *)
	val analyse_list = 
	    ins_bcs (list_unbound_grpvars grp_id, analyse_list)

	val _ = CPN'report_timing " - done @"
    in
	(!no_of_vars,               (* number of elements in P_var *)
	 ae_length+gi_length,      (* number of elements in P_exp *)
	 analyse_list)
	handle SyntaxError s => raise SyntaxError s
	     | InternalError s => raise InternalError s
	     | _ => raise (InternalError "con_ptnet")
    end (* con_ptnet *)
end (* CPN'PTnet *)
