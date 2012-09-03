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
(* File: tables.sml
 *
 * Basic tables and instance related tables.
 *)

(*********************** Sizes of the Hash Tables ***********************)

local
    val cs_hash_size = 11
    val ref_hash_size = 5
    val var_hash_size = 17
    val node_hash_size = 29
    val page_hash_size = 17

    type id = CPN'Id.id
in

    local
	fun eq (x: string, y) = (x = y)
    in
	val hashString = (HashString.hashString, eq)
    end

    local
	fun eq (x: int, y) = (x = y)
    in
	val hashInt = (fn x => x, eq)
    end

    local
	fun eq (x::xs: int list, y::ys) = (x = y)
	  | eq (nil,nil) = true
	  | eq _ = false
    in
	val hashIntList = (foldl Int.+ 0, eq)
    end

    val hashId = (CPN'Id.hash, CPN'Id.eq)

(**************************** Color-Set Table ****************************)

structure CPN'CSTable = struct

    datatype kind =
	unit_cs       of string option
      | bool_cs       of (string * string) option
      | int_cs        of (string * string) option
      | intinf_cs     of (string * string) option
      | real_cs       of (string * string) option
      | char_cs       of (string * string) option
      | string_cs     of {char: (string * string) option,
			  length: (string * string) option}
      | enum_cs       of string list
      | index_cs      of {idx: string,
			  over: (string * string) option}
      | list_cs       of {cs: string,
			  length: (string * string) option}
      | product_cs    of (string * string) list
      | record_cs     of (string * string) list
      | union_cs      of (string * string) list
      | funsubset_cs  of {cs: string, subset: string}
      | listsubset_cs of {cs: string, subset: string list}
      | time_cs
      | alias_cs      of string

    type item = {kind   : kind,
		 id     : id,
		 timed  : bool,
		 var    : string list,
		 msvar  : string list,
		 declare: string list,
		 alias  : string list}

    val table : (string, item) HashTable.hash_table = 
	HashTable.mkTable hashString (cs_hash_size,InternalError("CStable.find"))

    val insert = HashTable.insert table 

    val remove = HashTable.remove table
		 
    val listItemsi = fn () =>  HashTable.listItemsi table
		 
    fun removeId id 
	= let
	      fun search [] = NONE
		| search ((name,h:item)::r)
		  = if (#id h) = id
			then SOME (name, #var h, #msvar h)
		    else search r
	  in
	      case search (HashTable.listItemsi table)
		of NONE => ([],[])
		 | SOME (name,var,msvar) =>
		       (remove name;
			(var,msvar))
	  end

    val find  = HashTable.lookup table

    val peek = HashTable.find table

    fun append_alias_cs (n,name) = 
	case peek name of
	    SOME{id,timed,var,msvar,...} =>
		insert (n, {kind= alias_cs name,
			    id=id,timed=timed,var=var,msvar=msvar,
			    declare=nil,alias=nil})
	  | NONE => raise InternalError("append_alias_cs")

    fun append_declare (name,declare') = 
	case peek name of
	    SOME{kind,id,timed,var,msvar,alias,declare} =>
		insert (name, {kind=kind,id=id,timed=timed,
			       var=var,msvar=msvar,
			       declare=List.@(declare',declare),alias=alias})
	  | NONE => raise InternalError("append_declare")

    fun append_alias (name,alias') = 
	case peek name of
	    SOME{kind,id,timed,var,msvar,alias,declare} =>
		insert (name, {kind=kind,
			       id=id,timed=timed,var=var,msvar=msvar,
			       declare=declare,alias=List.@(alias',alias)})
	  | NONE => raise InternalError("append_alias")

    fun append_var (name,var') = 
	case peek name of
	    SOME{kind,id,timed,var,msvar,alias,declare} =>
		insert (name, {kind=kind,id=id,timed=timed,
			       var=List.@(var',var),msvar=msvar,
			       declare=declare,alias=alias})
	  | NONE => raise InternalError("append_var")

    fun append_msvar (name,msvar') = 
	case peek name of
	    SOME{kind,id,timed,var,msvar,alias,declare} =>
		insert (name, {kind=kind,id=id,timed=timed,
			       var=var,msvar=List.@(msvar',msvar),
			       declare=declare,alias=alias})
	  | NONE => raise InternalError("append_msvar")


    fun list () = 
	let
	    fun convert_cs (unit_cs NONE, tail) = 
		"unit"::tail
	      | convert_cs (unit_cs(SOME str), tail) = 
		"unit with "::str::tail
	      | convert_cs (bool_cs NONE, tail) =
		"bool"::tail
	      | convert_cs (bool_cs(SOME(low,high)), tail) =
		"bool with ("::low::","::high::")"::tail
	      | convert_cs (int_cs NONE, tail) =
		"int"::tail
	      | convert_cs (int_cs(SOME(low,high)), tail) =
		"int with "::low::".."::high::tail
	      | convert_cs (intinf_cs NONE, tail) =
		"intinf"::tail
	      | convert_cs (intinf_cs(SOME(low,high)), tail) =
		"intinf with "::low::".."::high::tail
	      | convert_cs (real_cs NONE, tail) =
		"real"::tail
	      | convert_cs (real_cs(SOME(low,high)), tail) =
		"real with "::low::".."::high::tail
	      | convert_cs (char_cs NONE, tail) =
		"char"::tail
	      | convert_cs (char_cs(SOME(low,high)), tail) =
		"char with "::low::".."::high::tail
	      | convert_cs (string_cs{char=NONE,length=NONE}, tail) =
		"string"::tail
	      | convert_cs (string_cs{char=SOME(low,high),length=NONE},tail)=
		"string with "::low::".."::high::tail
	      | convert_cs (string_cs{char=SOME(low,high),
				      length=SOME(min,max)}, tail) =
		"string with "::low::".."::high::
		" length "::min::".."::max::tail
	      | convert_cs (enum_cs enm, tail) =
		"enumerate "::tl(foldr(fn(a,b)=>" | "::a::b) tail enm)
              | convert_cs (index_cs{idx,over=NONE}, tail) =
		"index "::idx::tail
	      | convert_cs (index_cs{idx,over=SOME(low,high)}, tail) =
		"index "::idx::" with "::low::".."::high::tail
              | convert_cs (list_cs{cs,length=NONE}, tail) =
		"list "::cs::tail
	      | convert_cs (list_cs{cs,length=SOME(min,max)}, tail) =
		"list "::cs::" with "::min::".."::max::tail
	      | convert_cs (product_cs comp, tail) =
		"product "::tl(foldr(fn((_,cs),b)=>" * "::cs::b) tail comp)
	      | convert_cs (record_cs comp, tail) =
		"record "::
		tl(foldr(fn((label,cs),b)=>" * "::label::": "::cs::b) tail comp)
	      | convert_cs (union_cs comp, tail) =
		"union "::
		tl(foldr(fn((label,cs),b)=>" + "::label::": "::cs::b) tail comp)
	      | convert_cs (funsubset_cs {cs,subset}, tail) = 
		"subset "::cs::" by "::subset::tail
	      | convert_cs (listsubset_cs {cs,subset}, tail) = 
		"subset "::cs::" with ["::
		tl(foldr (fn (a,b) => ","::a::b) (""::"]"::tail) subset)
	      | convert_cs (time_cs, tail) = "time"::tail
	      | convert_cs (alias_cs cs, tail) = cs::tail
	      | convert_cs _ = raise InternalError("Must be cs in CSTable")

	    fun convert_timed (timed, tail) =
		if timed then " timed "::tail else tail

	    fun convert_var (nil, tail) = tail
	      | convert_var (var, tail) =
		"\nvar "::tl(foldr (fn(a,b)=>", "::a::b) tail var)

	    fun convert_msvar (nil, tail) = tail
	      | convert_msvar (var, tail) =
		"\nmsvar "::tl(foldr (fn(a,b)=>", "::a::b) tail var)

	    fun convert_declare (nil, tail) = tail
	      | convert_declare (declare, tail) =
		"\ndeclare "::tl(foldr (fn(a,b)=>" "::a::b) tail declare)

	    fun convert_alias (nil, tail) = tail
	      | convert_alias (alias, tail) =
		"\nalias "::tl(foldr (fn(a,b)=>" "::a::b) tail alias)
		
	    fun convert (name,{kind,id,timed,var,msvar,declare,alias}) =
		concat("\ncolorset "::name::" = "::
			convert_cs(kind,
			convert_timed(timed,
			convert_var(var,
			convert_msvar(msvar,
			convert_declare(declare,
			convert_alias(alias,nil)))))))
	in
	    map convert (HashTable.listItemsi table)
	end

    local
	exception SimpleCS
    in
	fun get_components cs =
	    case peek cs of
		SOME{kind,...} =>
		    (case kind of
			 product_cs comp => comp
		       | record_cs comp => comp
		       | union_cs comp => comp
		       | alias_cs cs => get_components cs
		       | funsubset_cs {cs,...} => get_components cs
		       | listsubset_cs {cs,...} => get_components cs
		       | _ => raise SimpleCS)
	      | NONE => raise InternalError("get_components")
			 
	fun get_cs_order cs = let
	    fun get (nil,_) = nil
	      | get (_::xs,i) = i::get(xs,i+1)
	in
	    get (get_components cs,1) handle SimpleCS => [1]
	end 
    end

    fun get_prime_kind(alias_cs cs) = get_prime_kind (#kind(find cs))
      | get_prime_kind(funsubset_cs {cs,...}) = get_prime_kind(#kind(find cs))
      | get_prime_kind(listsubset_cs {cs,...}) = get_prime_kind(#kind(find cs))
      | get_prime_kind cs = cs

    fun get_prime_cs cs =
	case peek cs of
	    SOME{kind= alias_cs cs,...} => get_prime_cs cs
	  | SOME _ => cs
	  | NONE => raise InternalError ("get_prime_cs "^cs)

    fun get_parent_cs cs =
	 case peek cs of
	    SOME{kind,...} =>
		(case kind of
		     list_cs {cs,...} => [cs]
		   | product_cs comp => map #2 comp
		   | record_cs comp => map #2 comp 
		   | union_cs comp => 
		     (* remove any empty strings which indicate
		      * untyped constructors *)
		     List.filter (fn csi => csi<>"") (map #2 comp)
		   | funsubset_cs {cs,...} => [cs]
		   | listsubset_cs {cs,...} => [cs]
		   | alias_cs cs => [cs]
		   | _ => nil)
	  | NONE => raise InternalError("get_parent_cs "^cs)

    fun is_subtype cs =
	(* should maybe add boolean in item instead *)
	 case peek cs of
	    SOME{kind,...} =>
		(case kind of
		     unit_cs _ => false
		   | bool_cs _ => false
		   | int_cs NONE => false
		   | intinf_cs NONE => false
		   | real_cs NONE => false
		   | string_cs {char=NONE,length=NONE} => false
		   | index_cs {over=NONE,...} => false
		   | list_cs {length=NONE,...} => false
		   | product_cs comp => 
		     foldr (fn ((_,cs),b)=>(is_subtype cs) orelse b) false comp
		   | record_cs comp => 
		     foldr (fn ((_,cs),b)=>(is_subtype cs) orelse b) false comp
		   | union_cs comp => 
		     foldr (fn ((_,""),b)=>false orelse b
		            | ((_,cs),b)=>(is_subtype cs) orelse b) false comp
		   | alias_cs cs => is_subtype cs
		   | time_cs => false
		   | _ => true)
	  | NONE => raise InternalError("is_subtype "^cs)

    fun is_timed cs = 
	case peek cs of
	    SOME{timed,...} => timed
	  | NONE => raise InternalError("is_timed "^cs)

    fun is_equiv (cs,cs') = 
	(get_prime_cs cs) = (get_prime_cs cs') andalso
	(is_timed cs) = (is_timed cs')

    fun gen_prime_name_t cs =
	let
	    val prime_cs = get_prime_cs cs
	in
	    case (is_timed cs, is_timed prime_cs) of
		(true,true) => prime_cs^"'timed"
	      | (false,false) => prime_cs
	      | (true,_) => cs^"'timed"
	      | (false,_) => cs
	end

end (* CPN'CSTable *)


(**************************** Variable Table ****************************)

structure CPN'VarTable = struct

    type item = {cs : string,       (* the color-set of the variable *)
		 decl_id : string}  (* reference to place of definition of the variable *)
   

    val table: (string, item) HashTable.hash_table = 
	HashTable.mkTable hashString (var_hash_size,
				      InternalError("VarTable.find"))

    val insert = HashTable.insert table

    val remove = HashTable.remove table

    val find = HashTable.lookup table

    val peek = HashTable.find table

    fun list () = HashTable.listItemsi table

    fun remove_decl decl
	= let
	    fun del [] = ()
	      | del ((v,{cs=cs,decl_id=decl_id})::rest)
	        = if decl_id = decl
		      then (remove v;
			    del rest)
		  else del rest	      
	  in
	      del (list())
	  end

    fun get_var_string () =
	concat("("::tl(foldr (fn ((v,{cs=cs,decl_id=decl_id}),b) =>
			     ","::v::": "::cs::b) 
		       ["",")"] (list())))

end (* CPN'VarTable *)


(**************************** Reference Table ****************************)

structure CPN'RefTable = struct
    datatype item =
	globref of {name: string, exp: string}
      | pageref of {name: string, exp: string, page: id}
      | instref of {name: string, exp: string, page: id}

    val table: (string,item) HashTable.hash_table = 
	HashTable.mkTable hashString (ref_hash_size,
				      InternalError("RefTable.find"))

    val insert = HashTable.insert table

    val remove = HashTable.remove table

    val find = HashTable.lookup table

    val peek = HashTable.find table

    fun list () = HashTable.listItemsi table

    fun make_rep_name (globref{name,...}) = (name)
      | make_rep_name (pageref{name,page,...}) =
	concat ["CPN'",name,CPN'Id.makeid page]
      | make_rep_name (instref{name,page,...}) = 
	concat ["CPN'",name,CPN'Id.makeid page]

    fun get_rep_name rid = 
	case peek rid of
	    SOME(globref{name,...}) => (name,name)
	  | SOME(r as pageref{name,...}) => (name, make_rep_name r)
	  | SOME(r as instref{name,...}) => (name, make_rep_name r)
	  | _ => raise InternalError("get_rep_name")

    fun get_var_string var_list =
	let
	    fun varstr (pageref {page,name,exp},l) = 
		"val "::name::" = ref "::exp::";"::l
	      | varstr (instref {page,name,exp},l) = 
		"val "::name::" = "::"ref "::exp::";"::l
	      | varstr (globref {name,exp},l) =
		"val "::name::" = "::"ref "::exp::";"::l
	in
	    concat (foldr varstr [] (map find var_list))
	end
end


(****************************** Page Table ******************************)

structure CPN'PageTable = struct

    type item = {decl: {instrefs: id list,
			pagerefs: id list} option,
		 page: {name: string,
			prime: int,
			included: bool,
			places: id list,
			transitions: id list},
		 super_pages: (id * id) list,  (*subst trans id, super pg id*)
		 sub_pages: (id * id) list}    (*subst trans id, sub pg id*)


    val table: (id,item) HashTable.hash_table = 
	HashTable.mkTable hashId (page_hash_size,
				  InternalError("PageTable.find"))

    val peek = HashTable.find table

    val find = HashTable.lookup table

    val insert = HashTable.insert table;

    fun get_ref_vars id =
	case peek id of
	    SOME {decl= NONE,...} => nil
	  | SOME {decl= SOME{instrefs,pagerefs},...} =>
		List.@(instrefs,pagerefs)
	  | NONE => raise InternalError("get_ref_vars")

    fun get_sub_pages id = 
	case peek id of
	    SOME{sub_pages,...} => sub_pages
	  | NONE => raise InternalError("get_sub_pages")

    fun get_page id =
	case peek id of
	    SOME{page,...} => page
	  | NONE => raise InternalError("get_page")

    fun get_name id = (#name (get_page id))

    fun append (id,page) = 
    (* FIXME: extend such that removed nodes are notified *)
	case peek id of 
	    NONE =>
		insert (id,{page=page,
			    decl=NONE,
			    super_pages=nil,
			    sub_pages=nil})
	  | SOME{super_pages,sub_pages,decl,...} =>	
		insert (id,{page=page,
			    decl=decl,
			    super_pages=super_pages,
			    sub_pages=sub_pages})

    fun append_sub_node (subst_trans,super_page,sub_page) = 
	(case find super_page of
	     {decl,page,super_pages,sub_pages} =>
		 insert (super_page,
			 {page=page,
			  decl=decl,
			  super_pages=super_pages,
			  sub_pages=(if List.exists (fn (subst_trans',sub_page') => (subst_trans',sub_page')=(subst_trans,sub_page)) sub_pages then sub_pages else (subst_trans,sub_page)::sub_pages)});
		 case find sub_page of
		     {decl,page,super_pages,sub_pages} =>
			 insert (sub_page,
				 {page=page,
				  decl=decl,
				  super_pages=(if List.exists (fn (subst_trans',super_page') => (subst_trans',super_page')=(subst_trans,super_page)) super_pages then super_pages else (subst_trans,super_page)::super_pages),
				  sub_pages=sub_pages}))

    fun append_group (id, grp) = 
	case peek id of
	    SOME{page= {name,prime,included,places,transitions},
		 decl, super_pages, sub_pages} =>
	    insert (id, {page = {name=name,
				 prime=prime,
				 included=included,
				 places=grp::places,
				 transitions = transitions},
			 decl=decl,
			 super_pages=super_pages, 
			 sub_pages=sub_pages})
	  | NONE => raise InternalError ("append_group") 

    val remove = HashTable.remove table

    fun remove_super_page (id,s,st) = let
	val {page,decl,super_pages,sub_pages} = find id

	fun rm ((subtr,x)::xs) =
	    if CPN'Id.eq(s,x) andalso CPN'Id.eq(st,subtr) then xs else (subtr,x)::rm(xs)
	  | rm nil = raise InternalError("Not a super page")
    in
	HashTable.insert table (id,{page=page,
				      decl=decl,
				      super_pages=rm super_pages,
				      sub_pages=sub_pages})
    end

    fun remove_sub_page (id,s,st) = let
	val {page,decl,super_pages,sub_pages} = find id

	fun rm ((subtr,x)::xs) =
	    if CPN'Id.eq(s,x) andalso CPN'Id.eq(st,subtr) then xs else (subtr,x)::rm(xs)
	  | rm nil = raise InternalError("Not a sub page")
    in
	HashTable.insert table (id,{page=page,
				      decl=decl,
				      super_pages=super_pages,
				      sub_pages=rm sub_pages})
    end

    local
	fun rm (id,[]) = raise InternalError ("rm in PageTable: "^id)
	  | rm (id,id'::xs) = 
	    if CPN'Id.eq(id,id') then xs 
	    else id'::(rm (id,xs))

	val rm_all = fn (l1,l2) => foldr (fn (x,l) => rm (x,l)) l1 l2;
    in
	fun remove_diff (page,{places',transitions'}) = 
	    let
		val {page={name, prime, included, places, transitions},
		     decl, super_pages, sub_pages} = find page;
	    in
		HashTable.insert table 
		(page,{page = {name = name,
			       prime = prime, 
			       included = included,
			       places = rm_all(places,places'),
			       transitions = rm_all(transitions,transitions')},
		       decl=decl,
		       super_pages = super_pages,
		       sub_pages = sub_pages})
	    end
    end; (* local *)

    fun list () = HashTable.listItemsi table

    fun get_no_of_inst id = let
	val {page={prime,...},super_pages,...} = find id
    in
	foldr (fn (a,b) => (get_no_of_inst (#2 a))+b) prime super_pages
    end

    fun get_all_no_of_inst () =
	map (fn (id,_) => (id,get_no_of_inst id)) (HashTable.listItemsi table)

    fun get_places id = 
	case peek id of
	    SOME{page={places,...},...} => places
	  | NONE => raise InternalError("get_places")

    fun get_transitions id = 
	case peek id of
	    SOME{page={transitions,...},...} =>	transitions
	  | NONE => raise InternalError("get_transitions")

    fun list_all_places_bottom_up () = let

	fun find_bottom_pages ((id,{sub_pages=nil,...}: item)::pages, tail) =
	    find_bottom_pages(pages, id::tail)
	  | find_bottom_pages (_::pages, tail)= 
	    find_bottom_pages (pages, tail)
	  | find_bottom_pages (nil,tail) = tail

	fun get_super_pages pages = 
	    CPN'Misc.flatten 
	    (foldr (fn (id,tail) => (map #2 (#super_pages(find id)))::tail) nil pages)

	fun list_all (_, nil, nil) = nil
	  | list_all (listed, listing, nil) =
	    list_all (listed, nil, get_super_pages listing)
	  | list_all (listed, listing, id::pages) =
	    if List.exists (fn id' => CPN'Id.eq(id,id')) listed then
		list_all (listed, listing, pages)
	    else
		List.@(get_places id,list_all(id::listed, id::listing, pages))
    in
	list_all (nil, nil, find_bottom_pages (list(),nil))
    end

local
    fun find_top_pages ((id,{super_pages=nil,...}: item)::pages, tail) =
	find_top_pages(pages, id::tail)
      | find_top_pages (_::pages, tail)= 
	find_top_pages (pages, tail)
      | find_top_pages (nil,tail) = tail

    fun get_sub_pages_recursively pages = 
	CPN'Misc.flatten 
	(foldr (fn (id,tail) => (map #2 (#sub_pages(find id)))::tail) nil pages)
in
    fun list_all_places_top_down () = let

	fun list_all (_, nil, nil) = nil
	  | list_all (listed, listing, nil) =
	    list_all (listed, nil, get_sub_pages_recursively listing)
	  | list_all (listed, listing, id::pages) =
	    if List.exists (fn id' => CPN'Id.eq(id,id')) listed then
		list_all (listed, listing, pages)
	    else
		List.@(get_places id,list_all(id::listed, id::listing, pages))
    in
	list_all (nil, nil, find_top_pages (list(),nil))
    end

    fun list_all_pages_top_down () = let

	fun list_all (_, nil, nil) = nil
	  | list_all (listed, listing, nil) =
	    list_all (listed, nil, get_sub_pages_recursively listing)
	  | list_all (listed, listing, id::pages) =
	    if List.exists (fn id' => CPN'Id.eq(id,id')) listed then
		list_all (listed, listing, pages)
	    else
		id::list_all(id::listed, id::listing, pages)
    in
	list_all (nil, nil, find_top_pages (list(),nil))
    end

end (* local *)
end (* CPN'PageTable *)


(***************************** Place Table ******************************)

structure CPN'PlaceTable = struct
    datatype exp_type = 
	token_exp of string
      | ms_exp of string
      | tms_exp of string

    datatype kind =
	place 
      | port of (id*id) list (* list of the connected sockets and subst-nodes*)
      | fusion of id         (* member of a global or page fusion group *)
      | group                (* fusion group *)

    type item = {kind: kind,
		 ext : {page    : id,
			name    : string,
			cs      : string,
			im      : exp_type}, 
		 int : {name    : string,
			ims     : string, 
			order   : int list,
			offset  : string option} option}
    
    val table: (id,item) HashTable.hash_table = 
	HashTable.mkTable hashId (node_hash_size,
				  InternalError("PlaceTable.find"))

    val find = HashTable.lookup table

    val peek = HashTable.find table

    fun insert (p,kind,ext) =
	HashTable.insert table (p,{kind=kind,ext=ext,int=NONE})

    val remove = HashTable.remove table

    fun list () = HashTable.listItemsi table

    fun is_kind_place place_id =
	case find place_id of 
	    {kind=place,...} => true
	  | _ => false

    fun is_kind_port place_id =
	case find place_id of 
	    {kind=port _,...} => true
	  | _ => false

    fun is_kind_fusion place_id =
	case find place_id of 
	    {kind=fusion _,...} => true
	  | _ => false

    fun is_kind_group place_id =
	case find place_id of 
	    {kind=group,...} => true
	  | _ => false

    fun get_fusion_group_members grp =
	HashTable.foldi
	(fn (pid,el,res)=> 
	  case el of
	      {kind=fusion pid',...} => if pid'=grp then pid::res else res
	    | _ => res)
	nil table

    fun append_socket (p,s) =
	case peek p of 
	    SOME{kind=port sockets,ext,int} =>
		(case (List.exists (fn e => e=s) sockets) of
		     true => ()
		   | false => 
			 HashTable.insert table (p,{kind=port(s::sockets),
					     ext=ext,
					     int=int}))
          | SOME{kind=place,ext,int} =>
		HashTable.insert table (p,{kind=port[s],
					     ext=ext,
					     int=int})
	  | _ => raise InternalError("append_socket")

    fun append_rep (p,rep) = 
	case peek p of
	    SOME{kind,ext,...} =>
		HashTable.insert table (p,{kind=kind,ext=ext,int=SOME rep})
	  | _ => raise InternalError("append_rep")

    fun get_ext p = 
	case peek p of
	    SOME{ext,...} => ext
	  | _ => raise InternalError "get_ext"

    fun get_rep p = 
	case peek p of
	    SOME{int=SOME{name,ims,order,offset},ext={cs,...},...} => 
		{cs=cs,name=name,ims=ims,order=order,offset=offset}
	  | _ => raise InternalError("get_rep "^p)

    (* Input:  Id of a place
       Output: Id of page where the place is located
       Exception: InternalError in case the place id is non-existing
     *)
    fun get_page p = 
	case peek p of
	    SOME{ext={page,...},...} => page
	  | _ => raise InternalError "get_page"

    (* returns <pagename>'<placename>*)
    fun get_name id = 
	(CPN'PageTable.get_name(get_page id))^"'"^
	(#name (#ext (find id)))

    fun in_order (p,keys: {label: string, exp: string} list) = let
	fun in_order' ((lbl,_)::rcomp, 
		       ({label,...}: {label: string, exp: string})::rkeys) =
	    if label=lbl then in_order'(rcomp,rkeys) else false
	  | in_order' (_,nil) = true
	  | in_order' (nil,_) = true

	val {ext={cs,...},...} = find p
    in
	in_order'(CPN'CSTable.get_components cs,keys)
    end

    fun is_timed p =
	case peek p of
	    SOME{ext={cs,...},...} => CPN'CSTable.is_timed cs
	  | NONE => raise InternalError("is_timed")

end (* CPN'PlaceTable *)


(*************************** Transition Table ***************************)

structure CPN'TransitionTable = struct

(* DESCRIPTION OF BOP DATATYPE:
 *
 * Bind Pattern, B_p
 * 	pat: 	The pattern.
 *	coef: 	The coefficient, "1" in case of token expression.
 *	vars: 	A list of the variables in the pattern.
 *	isdiv: 	NONE if not a dividable expression, SOME(i,t) otherwise
 *		where t is the total number of B_p's in the dividable
 *		expression and i is the index.
 *	time: 	NONE if untimed, SOME(time) otherwise, SOME("0") if timed place
 * 		but expression is without time (i.e., without @+).
 * 	place:	The name of the attached place.
 *
 * Bind with a Key, B_k  
 * 	pat: 	The pattern.
 *	coef: 	The coefficient, "1" in case of token expression.
 *	vars: 	A list of the variables in the pattern not used as keys.
 *	keys: 	A list of the components {label,exp,no} used as key,
 * 		where label is the label (number in case of products), exp
 *		is the expression and no is the number of the component in the
 *		color-set order.
 *	time: 	NONE if untimed, SOME(time) otherwise, SOME("0") if timed place
 * 		but expression is without time (i.e., without @+).
 * 	place:	The name of the attached place.
 *
 * Bind from Guard, B_g
 * 	pat: 	The pattern.
 *	vars: 	A list of the variables in the pattern not used as keys.
 *	exp:	The expressions.
 *
 * Bind from Color-set, B_c
 *	var:	The variable.
 *	cs:	The color-set.
 * 
 * Test Arc, T_a
 *	exp:	The expression.
 *	coef:	The coefficient, "1" in case of a token expression, and
 *		"" in case of a dividable multi-set expression.
 *	time: 	NONE if untimed, SOME(time) otherwise, SOME("0") if timed place
 * 		but expression is without time (i.e., without @+).
 * 	place:	The name of the attached place.
 *
 * Test Guard, T_g
 *	exp: 	The expression.
 *
 * Test Variable bindings, T_v
 *	var1:	First variable.
 *	var2:	Second variable.
 *)

    datatype bop = 
	B_p of {pat  : string,
		coef : string,
		vars : string list,
		isdiv: (int * int) option,
		time : string option,
		place: id}
      | B_k of {pat  : string,
		coef : string, 
		vars : string list,
		keys : {label: string, 
			exp: string, 
			no: int} list, 
		time : string option,
		place: id}
      | B_g of {pat  : string,
		vars : string list,
		rebind_vars: string list, (* vars rebound during binding seq *)
		exp  : string}
      | B_c of {var  : string,
		cs   : string}
      | T_a of {exp  : string,
		coef : string,
		time : string option,
		place: id}
      | T_g of {exp  : string}
      | T_v of {var1 : string,
		var2 : string}

    datatype item =
	substitution of {page   : id,
			 name   : string,
			 subpage: id,
			 border : {port   : id, 
				   socket : id} list}
      | transition of {page     : id,
		       name     : string,
		       input    : {arcs: (id * CPN'PlaceTable.exp_type) list, 
				   place: id,
				   no_of_tokens: int option} list,
                   inhibitor : {arcs: (id * CPN'PlaceTable.exp_type option) list, 
				   place: id} list,
		       reset    : {arcs: (id * CPN'PlaceTable.exp_type option) list, 
				   place: id} list,
		       groups   : {vars: string list, bops: bop list} list,
		       time_reg : string,
		       code_reg : {input : string list,
				   output: string list,
				   action: string} option,
                   chan_reg : string option,
                   priority_reg : string option,
                   controllable : bool,
		       monitors : id list,
		       free_vars: string list,
		       output   : {arcs : (id * CPN'PlaceTable.exp_type) list, 
				   place: id} list}

    val table: (id,item) HashTable.hash_table = 
	HashTable.mkTable hashId (node_hash_size,
				  InternalError("TransitionTable.find"))

    val insert = HashTable.insert table 

    val remove = HashTable.remove table

    fun find t = HashTable.lookup table t

    fun peek t = HashTable.find table t

    fun list () = HashTable.listItemsi table

    fun list_sorted_by_name () = let

	fun f (_,transition{name,...}: item) = name
	  | f (_,substitution{name,...}) = name
    in
	CPN'Misc.sort (fn (a,b) => (f a)<(f b)) (list())
    end

    fun get_vars t = 
	case peek t of
	    SOME(transition{groups,...}) => 
		CPN'Misc.flatten (map (fn {vars,bops=_} => vars) groups)
	  | _ => raise InternalError("get_vars")

    fun get_free_vars t = 
	case peek t of
	    SOME(transition{free_vars,...}) => free_vars
	  | _ => raise InternalError("get_free_vars")

    fun get_codeseg_output_vars t = 
	case peek t of
	    SOME(transition{code_reg,...}) => 
		case code_reg of
		    SOME{output,...} => output
		  | NONE => []
	  | _ => raise InternalError("get_codeseg_output_vars")

    fun get_all_vars t =
	case peek t of
	    SOME(transition _) => 
		(get_vars t)^^
		(get_free_vars t)^^
		(get_codeseg_output_vars t)
	  | _ => raise InternalError("get_all_vars")
		
    fun get_page t =
	case peek t of
	    SOME(transition{page,...}) => page
	  | SOME(substitution{page,...}) => page
	  | NONE => raise InternalError("get_page")

    (* returns <pagename>'<transname> *)
    fun get_name t =
	case peek t of
	    SOME(transition{name,page,...}) => 
		concat[#name(CPN'PageTable.get_page page),"'",
			name]
	  | SOME(substitution{name,page,...}) => 
		concat[#name(CPN'PageTable.get_page page),"'",
			name]
	  | NONE => raise InternalError("CPN'TransitionTable.get_name")

    fun get_input t = 
	case peek t of
	    SOME(transition{input,...}) => map #place input
	  | _ => raise InternalError("get_input")

    fun get_output t = 
	case peek t of
	    SOME(transition{output,...}) => map #place output
	  | _ => raise InternalError("get_output")

    fun get_controllable t = 
	case peek t of
	    SOME(transition{controllable,...}) => controllable
	  | _ => raise InternalError("get_controllable")

    fun get_places t =
	case peek t of
	    SOME(transition{input,output,...}) => 
		foldr (fn ({place,...},tail) => place::tail)
		(map #place output)
		input
	  | _ => raise InternalError("get_places")

    fun is_transition t =
	case peek t of
	    SOME(transition _) => true
	  | SOME _ => false
	  | NONE => raise InternalError("is_transition "^t)

end (* CPN'TransitionTable *)


(****************************** IMS Table ******************************)

structure CPN'IMSTable = struct

    type item = string

    val table: (string,item) HashTable.hash_table = 
	HashTable.mkTable hashString (cs_hash_size,
				      InternalError("IMSTable.find"))

    val insert = HashTable.insert table 

    val remove = HashTable.remove table

    val find = HashTable.lookup table

    val peek = HashTable.find table

    (* Remove all entries of which pfix is a textual prefix *)
    fun filter_prefix pfix =
	let
	    val pfix'= "CPN'"^pfix       (* PIMS names have prefix CPN' *)
	in
	(CPN'debug ("IMSTable:filter_prefix: "^pfix');
	HashTable.filteri
         (fn (name,_)=> 
	                if not(String.isPrefix pfix' name) then
			    (
			     CPN'debug (" NOT PREFIX pfix'="^pfix'^" name="^name);
			     true
			     )
                        else
			    (
			     CPN'debug ("     PREFIX pfix'="^pfix'^" name="^name);
			     false
			     )
	                )
         table;
	 CPN'debug ("IMSTable="^(CPN'concat(map (fn (aa,bb)=> aa^"=>"^bb^"\n") (HashTable.listItemsi table))))
	 )
	end

end (* CPN'IMSTable *)


(****************************** Rep Table ******************************)

functor CPN'CreateRepTable(structure Time: CPN'TIME): CPN'REPTABLE = struct

    structure Time=Time

    (* id: the transition id
     * int list: list of orders of keys in record and tuple patterns *)
    type order_item = (id * int list) list;

    (* id: the transition id
     * time option: SOME(time) or NONE depending of the presence of @+ *)
    type offset_item = (id * Time.time option) list;

    (* list of transition ids. *)
    type dep_item = id list;

    (* Table of orders of keys in records and tuples.
     * id: the place id
     * order_item: see above
     *)
    val order_table: (id,order_item) HashTable.hash_table = 
	HashTable.mkTable hashId (node_hash_size,
				  InternalError("RepTable.find order_table"))

    (* For timed places where an outgoing arc has a tms expression with
     * a constant time offset. The offset may be a function but then the
     * item is NONE. See also documentation of B_k above.
     * id: the place id
     * offset_item: see above
     *)
    val offset_table: (id,offset_item) HashTable.hash_table = 
	HashTable.mkTable hashId (node_hash_size,
				  InternalError("RepTable.find offset_table"))

    (* Dependence table from a place to a number of transitions. Each arc
     * going from a place to a transition results in a dependency entry.
     * For fusion places we only represent the group, which can work
     * across pages.
     * id: the place id
     * dep_item: see above
     *)
    val dep_table: (id,dep_item) HashTable.hash_table = 
	HashTable.mkTable hashId (node_hash_size,
				  InternalError("RepTable.find dep_table"))

    local
	fun ins (t,x,nil) = [(t,x)]
	  | ins (t,x,(t',x')::xs) =
	    if CPN'Id.eq (t,t') then 
		(t,x)::xs
	    else 
		(t',x')::(ins(t,x,xs))
    in
	fun append_order (p,t,rep) = let
	    fun append id =
		case HashTable.find order_table id of
		    NONE => 
			HashTable.insert order_table (id, [(t,rep)])
		  | SOME order => 
			HashTable.insert order_table (id,ins(t,rep,order))
	in
	    case CPN'PlaceTable.peek p of
		SOME{kind=(CPN'PlaceTable.fusion grp),...} => append grp
	      | SOME _ => append p
	      | _ => raise InternalError("append_order")
	end

	fun append_offset (p,t,tv) = let
	    fun append id =
		case HashTable.find offset_table id of
		    NONE => 
			HashTable.insert offset_table (id,[(t,tv)])
		  | SOME offset => 
			HashTable.insert offset_table (id,(t,tv)::offset)
	in
	    case CPN'PlaceTable.peek p of
		SOME{kind=(CPN'PlaceTable.fusion grp),...} => append grp
	      | SOME _ => append p
	      | _ => raise InternalError("append_offset")
	end

    end (* local *)

    fun append_dep (p,t) = let
        fun ins nil = [t]
	  | ins (t'::ts) = 
	    if CPN'Id.eq (t,t') then 
		t::ts
	    else
		t'::(ins ts);

	fun append id =
	    case HashTable.find dep_table id of
		NONE => 
		    HashTable.insert dep_table (id,[t])
	      | SOME dep => 
		    HashTable.insert dep_table (id,ins dep)
    in
	case CPN'PlaceTable.peek p of
	      SOME{kind=CPN'PlaceTable.fusion grp,...} => append_dep(grp,t)
	    | SOME _ => append p
	| _ => raise InternalError("append_dep")
    end

    val rm_order = fn id => (HashTable.remove order_table id; ());
    val rm_offset = fn id => (HashTable.remove offset_table id; ());
    val rm_dep = fn id => (HashTable.remove dep_table id; ());

    val find_order = HashTable.lookup order_table;
    val find_offset = HashTable.lookup offset_table;
    val find_dep = HashTable.lookup dep_table;

    val peek_order = HashTable.find order_table
    val peek_offset = HashTable.find offset_table
    val peek_dep = HashTable.find dep_table

    fun remove id = 
	(case peek_order id of SOME _ => rm_order id | _ => ();
	 case peek_offset id of SOME _ => rm_offset id | _ => ();
	 case peek_dep id of SOME _ => rm_dep id | _ => ());

    fun rm_trans (p,t) =
	let
	    fun rm _ [] = []
	      | rm f (t'::xs) =
		if CPN'Id.eq(t,f t') then xs else t'::(rm f xs)
	in
	    (case peek_order p of 
		 SOME l => 
		     HashTable.insert order_table (p,rm #1 l)
	       | _ => ();
	     case peek_offset p of
		 SOME l =>
		     HashTable.insert offset_table (p,rm #1 l)
	       | _ => ();
	     case peek_dep p of
		 SOME l => 
		     HashTable.insert dep_table (p,rm (fn x => x) l)
	       | _ => ())
	end;
	 
    fun get_order p = let
	val p = case CPN'PlaceTable.peek p of
	    SOME{kind=CPN'PlaceTable.fusion grp,...} => grp
	  | _ => p
    in
	case peek_order p of
	    SOME item => map #2 item
	  | NONE => nil
    end

    fun get_offset p = let
	val p = case CPN'PlaceTable.peek p of
	    SOME{kind=CPN'PlaceTable.fusion grp,...} => grp
	  | _ => p
    in
	case peek_offset p of
	    SOME item => map #2 item
	  | NONE => nil
    end

    fun get_dep p =  let
	val p = case CPN'PlaceTable.peek p of
	    SOME{kind=CPN'PlaceTable.fusion grp,...} => grp
	  | _ => p
    in
	case peek_dep p of
	    SOME item => item
	  | NONE => nil
    end

end (* CPN'RepTable *)


(**************************** Inst Table ****************************)

functor CPN'CreateInstTable (structure RepTable: CPN'REPTABLE) : CPN'INSTTABLE = struct

    (* The instance tables are used for determining the different
     * instances.  
     *
     * The page_table table holds as first component the number of
     * instances of each page, such that it does not has to be
     * computed each time it is used. The second component is an array
     * with an entry for each instance. Each entry holds a list of
     * connections to superpage, each entry is either a triple
     * SOME(superpage, supernode, superinst), and NONE if it is not
     * connected to a superpage.
     *  
     * The transition_table holds the index of the first and last
     * instance of the given transition in the top loop, and the output 
     * places as third argument (used when dumping the table).
     *
     * The substitution_table holds the first and last instance of the
     * subpage that the given substitution node is determining.  
     *
     * The place_table holds a list of instance dependent connections.
     * the first integer is the instance of the place that is the key,
     * the second integer is the instance of the place given as second
     * component.
     * Note: The relation is not transitive. Only immediate neighbours 
     * are included.
     * FIXME: should this table continue to ignore fusion relations?
     *)

    structure RepTable = RepTable

    datatype offset_type =
	one_offset of string
      | max_offset of string
      | var_offset
      | un_timed

    type transition_item = int * int
    type substitution_item = int * int 
    type page_item = int * (id * id * int) option Array.array
    type place_item = (int * id * int) list

    val transition_table: (id,transition_item) HashTable.hash_table =
        HashTable.mkTable hashId (node_hash_size,
				  InternalError("TransitionInstTable.find"))
    val substitution_table: (id,substitution_item) HashTable.hash_table =
        HashTable.mkTable hashId (page_hash_size,
				  InternalError("SubstitutionInstTable.find"))
    val page_table: (id,page_item) HashTable.hash_table =
        HashTable.mkTable hashId (page_hash_size,
				  InternalError("PageInstTable.find"))
    val place_table: (id,place_item) HashTable.hash_table =
        HashTable.mkTable hashId (node_hash_size,
				  InternalError("PlaceInstTable.find"))

    val no_of_tis = ref 0;

    fun clean () = 
	(no_of_tis:= 0;
	 HashTable.filter (fn _ => false) transition_table;
	 HashTable.filter (fn _ => false) substitution_table;
	 HashTable.filter (fn _ => false) page_table;
	 HashTable.filter (fn _ => false) place_table)

    fun get_no_of_inst id = #1(HashTable.lookup page_table id)

    fun get_t_index t = 
	case HashTable.find transition_table t of
	    SOME (m,n) => (m,n)
	  | NONE => raise InternalError(concat["get_t_index ",
						CPN'Id.toString t,")"])
    fun get_ti_index (t,i) = 
	case HashTable.find transition_table t of
	    SOME (n,_) => n+i-1
	  | NONE => raise InternalError(concat["get_ti_index(",
						CPN'Id.toString t,",",
						Int.toString i,")"])

    fun get_ti_indices () = HashTable.listItemsi transition_table

    (* Create the instance tables from scratch. *)
    fun init () = let

	val _ = CPN'debug "InstTable.init";
	val _ = CPN'report_timing(concat["InstTable.init @ "]);

	fun init_s nil = ()
	  | init_s ((sid,n)::pages) =
	    (HashTable.insert page_table (sid,(n,Array.array(n,NONE)));
	     init_s pages)

       (* This temporary table holds the number of instances that is
	* not already used of the given page. Should maybe be a copy 
	* of page_table instead. *) 
        val subpage_table: (id,int) HashTable.hash_table =
	    HashTable.mkTable hashId (page_hash_size, 
				      InternalError("SubpageInstTable.find"))

	fun init_t nil = ()
	  | init_t ((tid,CPN'TransitionTable.transition{page,...})::tail) = let
	    val no = !no_of_tis
	    val no' = no + get_no_of_inst page
	in
	    (no_of_tis:= no';
	     HashTable.insert transition_table (tid, (no,no'-1)); 
	     init_t tail)
        end
	  | init_t ((tid,CPN'TransitionTable.substitution{page,subpage,...})::tail) = let
	    val n = get_no_of_inst page
	    val m = case HashTable.find subpage_table subpage of
		NONE => 0
	      | SOME m => m 
	    val connections = #2(HashTable.lookup page_table subpage)

	    fun update_super_cons 0 = ()
	      | update_super_cons i =
		(Array.update(connections,m+i-1,SOME(page,tid,i));
		 update_super_cons (i-1))
	 in
	     (HashTable.insert subpage_table (subpage,n+m);
	      HashTable.insert substitution_table (tid,(m+1,m+n));
	      update_super_cons n;
	      init_t tail)
	 end

	fun init_p nil = ()
	  | init_p (pid::pids) = let

	    fun find_socket_inst (nil,_) = NONE
	      | find_socket_inst ((s,t)::sockets,i) = let
		val (a,b) = HashTable.lookup substitution_table t 
	    in
		if a<=i andalso i<=b then SOME(s,i-a+1) 
		else find_socket_inst(sockets,i)
	    end

	    fun find_inst_cons (_,0,tail) = tail
	      | find_inst_cons (nil,_,tail) = tail
	      | find_inst_cons (sockets,i,tail) = 
		(case find_socket_inst (sockets,i) of 
		     NONE => 
			 find_inst_cons(sockets,i-1,tail)
		   | SOME(s,j) =>
			 (case HashTable.find place_table s of
			     NONE =>
				 HashTable.insert place_table (s,[(j,pid,i)])
			   | SOME list =>
				 HashTable.insert place_table 
				 (s,(j,pid,i)::list);
			  find_inst_cons(sockets,i-1,(i,s,j)::tail)))
	in
	    case CPN'PlaceTable.peek pid of
		SOME{kind=CPN'PlaceTable.port sockets, ext={page,...},...} =>
		    (case HashTable.find place_table pid of
			NONE =>
			    HashTable.insert place_table
			    (pid, find_inst_cons(sockets,
						 get_no_of_inst page,nil))
		      | SOME list =>
			    HashTable.insert place_table
			    (pid, find_inst_cons(sockets,
						 get_no_of_inst page,list));
		    init_p pids)
	      |	SOME{kind=CPN'PlaceTable.group, ext={page,...},...} =>
		   let
		       val fusion_members= CPN'PlaceTable.get_fusion_group_members pid
		       val fusion_ninsts= 
			    map
			    (fn fid=> (fid,get_no_of_inst(CPN'PlaceTable.get_page fid)))
			    fusion_members
		   in
		    app
		     (fn (fid,ninst) =>
		      (case HashTable.find place_table pid of
			   NONE =>
			       HashTable.insert place_table
			       (pid, List.tabulate(ninst,fn i=>(1,fid,i+1)))
			 | SOME list =>
			       HashTable.insert place_table
			       (pid, (List.tabulate(ninst,fn i=>(1,fid,i+1))^^list))))
		     fusion_ninsts;
		    init_p pids
		   end
	      |	SOME{kind=CPN'PlaceTable.fusion grp, ext={page,...},...} =>
		    (case HashTable.find place_table pid of
			NONE =>
			    HashTable.insert place_table
			    (pid, List.tabulate(get_no_of_inst page,fn i=>(i+1,grp,1)))
		      | SOME list =>
			    HashTable.insert place_table
			    (pid, List.tabulate(get_no_of_inst page,fn i=>(i+1,grp,1))^^list);
		    init_p pids)
	      | SOME _ => init_p pids
	      | _ => raise InternalError("init_p pid="^pid)
	end
    in
	(clean();
	 (* init pages *)
	 init_s (CPN'PageTable.get_all_no_of_inst());
	 (* init transitions, sorted by name *)
	 init_t (CPN'TransitionTable.list_sorted_by_name());
	 (* init places *)
	 init_p (CPN'PageTable.list_all_places_bottom_up()))
    end

    fun get_socket_inst_list p = let

	val sockets = case CPN'PlaceTable.peek p of
	    SOME{kind=CPN'PlaceTable.port sockets,...} => sockets
	  | _ => raise InternalError("get_socket_inst_list")

	val inst_cons =  CPN'Misc.sort (fn ((a:int,_,_),(b,_,_)) => a<b) 
	    (HashTable.lookup place_table p)

	fun get (inst,(i,s,j)::cons) =
	    if inst=i andalso 
		(List.exists (fn (s',_) => CPN'Id.eq(s',s)) sockets) then
		(s,j)::get(inst+1,cons)
	    else
		get(inst,cons)
	  | get (_,nil) = nil
    in
	(CPN'debug ("length sockets = "^(Int.toString (length sockets)));
	 CPN'debug ("length inst_cons = "^(Int.toString (length inst_cons)));
	if length sockets = length inst_cons then
	    map (fn (i,s,j) => (s,j)) inst_cons
	else
	    get (1, inst_cons))
    end

    (* Calculate the transitive closure of the instance relations in
     * place_table, starting from a given place instance. *)
    fun get_inst_cons ((place,inst),tail) = let

	val peek = HashTable.find place_table

	fun get ((place,inst), SOME((i,s,j)::cons), tail) = let

	    fun treated (p,i: int) =
		List.exists (fn (p',i') => i=i' andalso CPN'Id.eq(p,p')) tail
	in
	    if inst=i andalso not (treated(s,j)) then
		get ((place,inst), SOME cons, get((s,j), peek s, (s,j)::tail))
	    else
		get ((place,inst), SOME cons, tail)
	end
	  | get (_,_,tail) = tail
    in
	get ((place,inst), peek place, ((place,inst)::tail))
    end

    (* Same as get_inst_cons, but disregarding direct instance dependencies *)
    fun get_all_cons (place,tail) = let

	val peek = HashTable.find place_table

	fun get (place, SOME((_,s,_)::cons), tail) = let

	    fun treated p =
		List.exists (fn p' => CPN'Id.eq(p,p')) tail
	in
	    if not (treated s) then
		get (place, SOME cons, get(s, peek s, s::tail))
	    else
		get (place, SOME cons, tail)
	end
	  | get (_,_,tail) = tail
    in
	get (place, peek place, (place::tail))
    end

    fun get_order p = let

	open Array;

	val cs_order = 
	    CPN'CSTable.get_cs_order (#cs(CPN'PlaceTable.get_ext p))

	(* ids for all of the places that can be reached 
	 * from p (including p) through instance connections *)
	val all_cons = get_all_cons (p,nil)

	(* The ordering of all_cons is not important, but it can
	 * affect the calculation of the order of p. 
	 * By ensuring that the ordering of all_cons is the same 
	 * for all places that share the same instance connections,
	 * this will ensure that all places with the same
	 * instance connections will have the same order. *)
	val all_cons_sorted = Misc.sort CPN'Id.lt all_cons

	(* an array holding the orders for the different instances *)
	val orders = 
	    fromList(List.foldr (fn (s,tail) => List.@(RepTable.get_order s,tail))
		     nil (all_cons_sorted))

	val n = length orders

	(* candidates_i holds all the orders which orders_i is a subset of *)
	val candidates = array(n, [cs_order])

	fun subset (nil, _) = true
	  | subset (x::xs, ys) =
	    if List.exists (fn y => x=y) ys then subset(xs,ys) else false

	(* make the array of candidates *)
	fun make (~1,_) = ()
	  | make (i,~1) = make (i-1,n-1)
	  | make (i,j) = 
	    if subset(sub(orders,i),sub(orders,j)) then 
		(* orders_i is a subset of orders_j, insert it *)
		(update(candidates,i,sub(orders,j)::sub(candidates,i));
		 make(i,j-1))
	    else
		make(i,j-1)

	(* find the candidate with the max number of demands *)
	fun find_max (~1,_,j) = 
	    Misc.sort (fn (a,b) => List.length a < List.length b) 
	              (sub(candidates,j))
	  | find_max (i,m,j) =
	    if List.length(sub(candidates,i))>m then 
		find_max(i-1,List.length(sub(candidates,i)),i)
	    else
		find_max(i-1,m,j)

	(* extract the order from the candidates *)
	fun extract_order (order, nil) = order
	  | extract_order (order, demand::demands) = let
	    fun add_unused ds = 
		let
		    (* the keys from ds that are not already in order *)
		    val unused = 
			List.filter 
			    (fn d => not (List.exists (fn i => i=d) order))
			    ds
		in
		    order^^unused
		end
	in
	    extract_order (add_unused demand, demands)
	end
    in
	if n>0 then       
	    (make(n-1,n-1);
	     extract_order(nil, (find_max(n-1,0,0))))
	else
	    cs_order
    end

    fun get_offset p = let

	fun get_offset' (nil, _, res) = res
	  | get_offset' (NONE::_, _, _) = var_offset
	  | get_offset' ((SOME t')::offs, t, max_offset s) =
	    if RepTable.Time.lt(t,t') then
		get_offset'(offs, t', max_offset(RepTable.Time.mkstr t'))
	    else
		get_offset'(offs, t, max_offset s)
	  | get_offset' ((SOME t')::offs, t, one_offset s) =
	    if RepTable.Time.lt(t,t') then
		get_offset'(offs, t', max_offset(RepTable.Time.mkstr t'))
	    else if RepTable.Time.lt(t',t) then
		get_offset'(offs, t, max_offset s)
	    else
		get_offset'(offs, t, one_offset s)
	  | get_offset' _ = raise Match

    in
	case foldr (fn (s,tail) => List.@(RepTable.get_offset s,tail))
		   nil (get_all_cons (p,nil)) of
	    nil =>
		if CPN'PlaceTable.is_timed p then 
		    one_offset(RepTable.Time.null_str)
		else 
		    un_timed
	  | NONE::_ => var_offset
	  | (SOME t)::offsets =>
	    get_offset'(offsets, t, one_offset(RepTable.Time.mkstr t))
    end

    fun get_input_place_ids t =
	foldr (fn (p,tail) => get_all_cons(p,tail)) 
	nil (CPN'TransitionTable.get_input t)

    fun get_output_place_instances (t,i) =
	foldr (fn (p,tail) => get_inst_cons((p,i),tail)) 
	nil (CPN'TransitionTable.get_output t)

    fun get_sur_places (t,i) =
	foldr (fn (p,tail) => get_inst_cons((p,i),tail)) 
	nil (CPN'TransitionTable.get_places t)

    local
	fun get ((p,i), tail) =
	    foldr (fn (t,ts) => get_ti_index(t,i)::ts) tail (RepTable.get_dep p)

	open CPN'Misc
    in
	fun get_dep_trans (p,i) =
	    unique_sort (Int.<) (foldr get nil (get_inst_cons((p,i),nil)))

	fun get_dep_list (t,i) = 
	    unique_sort (Int.<) (foldr get nil (get_output_place_instances(t,i)))
    end

    fun get_page_structure () = let

	val pages = CPN'PageTable.list_all_pages_top_down()

	fun make nil = nil
	  | make (page::pages) = let

	    val (n,array) = HashTable.lookup page_table page

	    fun listofarray i =
		if i<n then 
		    Array.sub(array,i)::listofarray(i+1)
		else nil
	in
	    (page,listofarray 0)::make(pages)
	end
    in
	make pages
    end

end (* CPN'InstTable *)

(************************* Monitor Table **************************)
structure CPN'MonitorTable = struct

datatype obstype = intobs | realobs
			    
(* Activate monitor after sim steps or entire simulation *)
datatype montype = step_monitor | sim_monitor
				  
datatype kind = 
	 datacoll of {timed: bool,
		      logfile: bool,
		      init: id * string, (* initial observation before first step,
					  * cannot observe transitions 
					  * a SOME/NONE x option where x is the
					  * return type of the obs fun *)
		      stop: id * string, (* final observation after last step,
					  * cannot observe transitions 
					  * a SOME/NONE x option where x is the
					  * return type of the obs fun *)
		      pred: id * string, (* predicate function *)
		      obs : id * string} (* observation function *)
       | breakpoint of id * string (* predicate function *)
       | write_file of {init: id * string, (* initialization function *)
			pred: id * string, (* predicate function *)
			obs : id * string, (* observation function *)
			stop : id * string, (* stop function *)
			fileext: string}  (* file extension *)
       | user_def of {aux: id * string,  (* auxiliary values and functions *)
		      init: id * string, (* initialization function *)
		      pred: id * string, (* predicate function *)
		      obs : id * string, (* observation function *)
		      act : id * string, (* action function *)
		      stop : id * string} (* stop function *)
		     
type item = {kind : kind,
	     name   : string,
	     places : (CPN'Id.id * int) list, (* pid * instance *)
	     transitions : (CPN'Id.id * int) list, (* tid * instance *)
	     montype : montype}
	    
val table : (id, item) HashTable.hash_table = 
    HashTable.mkTable hashString (node_hash_size,InternalError("MonitorTable.find"))
    
(* The order in which monitors should be called 
 * The order may contain ids that do not correspond to 
 * monitor items that are found in the table. This will
 * be the case if, e.g. there is a syntax error in a 
 * monitor. 
 *)
val order = ref []: CPN'Id.id list ref
		    
fun set_order midlist = order:= midlist
fun get_order () = (!order)
		   
fun get_order_index mid =
    let
	fun get_index n [] = raise InternalError "CPN'MonitorTable.order_index"
	  | get_index n (m::mids) = 
	    if m=mid then n else get_index (n+1) mids
    in
	get_index 0 (!order)
    end
	
(* Useful since CPN Tools GUI always bootstraps for timed models *)
fun model_is_timed() =
    List.exists 
	(fn (pid, {ext={cs,...},...}) => CPN'CSTable.is_timed cs) 
	(CPN'PlaceTable.list())
	
fun find mid = HashTable.lookup table mid
	       
fun peek mid = HashTable.find table mid
	       
fun rm_from_transitions mid = 
    case peek mid of 
	NONE => ()
      | SOME item  => 
	let 
	    val {transitions,...} = find mid
		fun rm [] = []
		  | rm (m::mids) = 
		    if m=mid 
		    then mids
		    else m::(rm mids)
			 
		fun rm_from_trans (tid,i) = 
		    case CPN'TransitionTable.peek tid of 
			SOME (CPN'TransitionTable.transition
                  {page,name,input,inhibitor,reset,groups,time_reg,code_reg,chan_reg,priority_reg,monitors,free_vars,output,controllable}) => 
			CPN'TransitionTable.insert (tid,
                  CPN'TransitionTable.transition
                  {page=page,name=name,input=input,inhibitor=inhibitor,reset=reset,groups=groups,time_reg=time_reg,code_reg=code_reg,chan_reg=chan_reg,priority_reg=priority_reg,monitors=
                  rm
                  monitors,free_vars=free_vars,output=output,controllable=controllable})
		      | _ => () (* not found, or subst trans*)
	in 
	    app rm_from_trans transitions
	end
	    
fun remove mid = (rm_from_transitions mid;
		  HashTable.remove table mid)
		 
fun list () = HashTable.listItemsi table
	      
fun add_to_transitions mid = 
    let
	val {transitions,...} = find mid
				
	val index = get_order_index mid
	(* all mids before mid in order *)
	val predecessors = List.take (!order,index)
	(* all mids after mid in order *)
	val successors = List.drop (!order,index+1)
			 
	fun update_trans (tid,i) = 
	    case CPN'TransitionTable.find tid of
		CPN'TransitionTable.transition
            {page,name,input,inhibitor,reset,groups,time_reg,code_reg,chan_reg,priority_reg,monitors,free_vars,output,controllable} => 
		    let
			(* assume notmid list is in same order as order *)
			val withoutmid = List.filter (fn m => m<>mid) monitors
			fun insert [] = [mid]
			  | insert (m::mids) = 
			    if List.exists (fn m1 => m=m1) successors
			    then mid::m::mids
			    else m::(insert mids)
			val mlist = insert withoutmid
			in
			    CPN'TransitionTable.insert (tid,
                      CPN'TransitionTable.transition
                      {page=page,name=name,input=input,inhibitor = inhibitor,
                      reset = reset, groups=groups,time_reg=time_reg,code_reg=code_reg,chan_reg=chan_reg,priority_reg=priority_reg,monitors=mlist,free_vars=free_vars,output=output,controllable=controllable})
			end
		  | _ => raise InternalError "CPN'MonitorTable.add_to_transitions" 
	in
	    app update_trans transitions
	end

    fun insert (mid,i) = 
	(HashTable.insert table (mid,i);
	 add_to_transitions mid)

    fun get_name mid = 
	case peek mid of 
	    SOME {name,...} => name
	  | NONE => raise InternalError("CPN'MonitorTable.get_name "^mid)

    fun list_names () = map (fn (id,{name,...}) => name) (list())

    fun list_bykind CPN'kind = List.filter (fn (id,{kind,...}) => (kind=CPN'kind)) (list())

    fun get_kind mid = 
	case peek mid of
	    SOME {kind,...} => kind
	  | NONE => raise InternalError("CPN'MonitorTable.get_kind "^mid)

    fun filter (f: CPN'Id.id * item -> bool) = List.filter f (list())

    (* List the names of the step monitors that are NOT
     * associated with any transition, i.e. the monitors
     * that need to be evaluated after every step. *)
    fun list_step_monitors_no_trans() = 
	List.filter (fn (mid,{transitions,montype,...}) => (transitions=[]) andalso montype=step_monitor) (list())

end  (* CPN'MonitorTable *)

(**************************** Misc Tables ****************************)

structure CPN'StringTable = HashTable
structure CPN'IdTable = CPN'StringTable

local
  fun eq (x: string, y) = (x = y)
in
  val hashString = (HashString.hashString, eq)
end

(************************ Basic Int Hash Table ************************)

structure CPN'IntKey = struct 

    structure W = Word

    val hash_val = 31 (* should be 2^n-1 *)
        
    type hash_key = int;
    fun hashVal s = W.andb(W.fromInt s,W.fromInt hash_val);
    fun sameKey (s,s') = s = s'
end

structure CPN'IntTable = HashTableFn(CPN'IntKey);

(*********************** Basic Int List Hash Table *************************)

structure CPN'IntListKey = struct 

    structure W = Word

    val hash_val = 31  (* should be 2^n-1 *)
        
    type hash_key = int list;
    fun hashVal ([]:hash_key) = W.fromInt 0
      | hashVal (x::xs) = W.andb(W.fromInt(x*(W.toInt(hashVal xs))),W.fromInt hash_val);
    fun sameKey (s,s') = s = s'
end

structure CPN'IntListTable = HashTableFn(CPN'IntListKey);

end (* local *)
