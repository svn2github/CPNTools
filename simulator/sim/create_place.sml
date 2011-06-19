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
(* File: create_place.sml
 *
 * Creation of place instances.
 *)

functor CPN'CreatePlace(structure InstTable: CPN'INSTTABLE): CPN'CREATEPLACE = struct
local 
    open CPN'PlaceTable
in
    val no_of_places = ref 0
    val total_places = ref "0"

    fun display_status_bar () = 
	(inc no_of_places;())

    fun gen_place_name pid = "CPN'place"^(CPN'Id.makeid pid)

    fun gen_pims_name (cs_name,order: int list) =
	concat("CPN'"::cs_name::"_pims"::(map Int.toString order))

    fun gen_sims_name pims_name = pims_name^"_sims"

    fun gen_cmp (cs, comp, order, tail) = let

	fun cs_order (i:int,x::xs) = (i=x) andalso cs_order(i+1,xs)
	  | cs_order _ = true
				   
	fun gen_head (nil,append,tail) = tail
	  | gen_head (x::xs,append,tail) = let
	    val (label,_) = List.nth(comp,x-1)
	in
	    ","::label::"=CPN'"::label::append::gen_head(xs,append,tail)
	end
	   
	fun gen_body (nil,tail) = 
	    if CPN'CSTable.is_timed cs then
		"\n else if CPN'Time.lt(CPN''time,CPN''time') then GREATER\n\
		 \ else if CPN'Time.lt(CPN''time',CPN''time) then LESS\n\
		 \ else EQUAL"::tail
	    else
		"\n else EQUAL"::tail
	  | gen_body (x::xs,tail) = let
	    val (label,csi) = List.nth(comp,x-1)
	in
	    "\n else"::
	    " if "::csi::".lt(CPN'"::label::",CPN'"::label::"') then\
	     \ LESS\n\
	     \ else if "::csi::".lt(CPN'"::label::"',CPN'"::label::") then\
	     \ GREATER"::gen_body(xs,tail)
	end
    in
	if cs_order(1,order) then
	    if CPN'CSTable.is_timed cs then
		"CPN'Misc.a_cmp "::cs::"'timed.lt"::tail
	    else
		"CPN'Misc.a_cmp "::cs::".lt"::tail
	else 
	    if CPN'CSTable.is_timed cs then
		"\n fn (CPN'Time.@({"::
		tl(gen_head(order,"","},CPN''time),CPN'Time.@({"::
		tl(gen_head(order,"'","},CPN''time')) =>\n "::
		tl(gen_body(order,tail))))))
	    else
		"\n fn ({"::tl(gen_head(order,"","},{"::
		tl(gen_head(order,"'","}) =>\n "::tl(gen_body(order,tail))))))
    end 

    fun get_gen_pims (cs,kind,order) = let
	val cs_name = CPN'CSTable.gen_prime_name_t cs
	val name = gen_pims_name (cs_name,order)
    in
	case CPN'IMSTable.peek name of 
	    SOME _ => name
	  | NONE => 
		(case (CPN'CSTable.get_prime_kind kind,CPN'CSTable.is_timed cs) of
		     (CPN'CSTable.unit_cs _,false) => 
			 (CPN'IMSTable.insert(name,"CPN'UnitPIMS");
			  CPN'Env.use_string ["\n structure ", name," =\
			   \ CPN'UnitPIMS(structure cs = ",cs_name,");"])
		   | (CPN'CSTable.bool_cs _,false) => 
			 (CPN'IMSTable.insert(name,"CPN'BoolPIMS");
			  CPN'Env.use_string ["\n structure ", name," =\
			   \ CPN'BoolPIMS(structure cs = ",cs_name,");"])
		   | (CPN'CSTable.product_cs comp,_) => 
			 (CPN'IMSTable.insert(name,!CPN'Settings.pims_name);
			  CPN'Env.use_string ("\n structure "::name::
			   " = "::(!CPN'Settings.pims_name)::
			   "(structure cs = "::cs_name::";\
			   \ val cmp:"::cs_name::" * "::cs_name::" -> order = "::gen_cmp(cs,comp,order,[");"])))
		   | (CPN'CSTable.record_cs comp,_) => 
		         (CPN'IMSTable.insert(name,!CPN'Settings.pims_name);
			  CPN'Env.use_string ("\n structure "::name::
			   " = "::(!CPN'Settings.pims_name)::
			   "(structure cs = "::cs_name::";\
			   \ val cmp:"::cs_name::" * "::cs_name::" -> order = "::gen_cmp(cs,comp,order,[");"])))
		   | (_,_) =>
			(CPN'IMSTable.insert(name,"Tree");
			 CPN'Env.use_string ["\n structure ",name,
			  " = ",(!CPN'Settings.pims_name),
			  "(structure cs = ",cs_name,";\
			  \ val cmp:",cs_name," * ",cs_name," -> order = CPN'Misc.a_cmp ",cs_name,".lt);"]);
	         name)
    end

    fun get_gen_sims (pims_name) = let
	val name = gen_sims_name pims_name
    in
	case CPN'IMSTable.peek name of 
	    SOME _ => name
	  | NONE => (CPN'IMSTable.insert (name,!CPN'Settings.sims_name);
		     CPN'Env.use_string ["\n structure ",name,
		     " = ",(!CPN'Settings.sims_name),
		     "(structure pims = ",pims_name,
		     " and Time = CPN'Time;\
		      \ val time = fn (CPN'Time.@(_,CPN't)) => CPN't);"];
		     name)
    end

    fun gen_im (timed, exp_type, name, tail) =
	"\n val _ = ("::name::".init_mark:= "::				 
	(if timed then
 	    case exp_type of
	       	token_exp exp => "[(("^exp^")@(Option.valOf(CPN'Time.start_time)))]"
	      | ms_exp exp => "(map (fn CPN'col => CPN'col@(Option.valOf(CPN'Time.start_time)))("^exp^"))"
	      | tms_exp exp => "("^exp^")"
	 else
 	    case exp_type of
	       	token_exp exp => "["^exp^"]"
	      | ms_exp exp => "("^exp^")"
	      | tms_exp exp => 
		    raise InternalError("Error: Timed im for untimed place"))::
	"; CPN'Place.init_mark_funs::= "::name::".set_init_mark;"::
	name::".set_init_mark());"::tail
    (*FIXME: init_mark_funs: memory leak*)

    fun gen_insert (p,name) =
	"\n val _ = CPN'IdTable.insert CPN'Place.instances ("::(CPN'Id.toString p)::
	",{print="::name::".print,size="::name::".size});"::nil

    (* Generate a place from scratch if not upd_insts_only otherwise
     * call change_no_of_inst instead to quicky modify number of instances. *)
    fun create_place (p,cs,im,page,upd_insts_only) = let

	val no_of_inst = Int.toString(InstTable.get_no_of_inst page)
	val _ = CPN'debug(concat["create_place \
	                          \ (",p,",",cs,",im,",page,"), ",Bool.toString upd_insts_only,", no_of_inst=",no_of_inst])

	val {timed,kind,...} = CPN'CSTable.find cs
	val order = InstTable.get_order p
        val name = gen_place_name p
	val _ = display_status_bar()
    in
        if timed then let
	    (* FIXME: Hack to avoid errors when compiling 
	     * profiled code for timed places *)
	    val wasprofiling = Compiler.Profile.getProfMode()
	    val _ = if wasprofiling 
		    then Compiler.Profile.setProfMode false
		    else ()
	    val pims_name = get_gen_pims (cs,kind,order)
	    val sims_name = get_gen_sims (pims_name)
	in
          if upd_insts_only then
	    (CPN'Env.use_string ("\n val _ = "::name::".change_no_of_inst("::no_of_inst::");"::nil))
	  else
	    (case InstTable.get_offset p of
		 InstTable.one_offset off =>
		     (CPN'Env.use_string 
		      ("\n structure "::name::" = CPN'Place.MakeTimedPlace\
		       \ (structure sims= "::sims_name::
		       " and Time = CPN'Time;\
		       \ val no_of_inst= "::no_of_inst::
		       " and offset= (maketime \""::off::"\"));"::
		       gen_im(timed,im,name,gen_insert(p,name)));
		       append_rep(p,{name=name,ims=pims_name,
				     order=order,offset=SOME off}))
	       | InstTable.max_offset off => 
		     (CPN'Env.use_string 
		      ("\n structure "::name::" = CPN'Place.MakeTimedPlaceM\
		       \ (structure sims = "::sims_name::
		       " and Time = CPN'Time;\
		       \ val no_of_inst= "::no_of_inst::
		       " and max_offset= (maketime\""::off::"\"));"::
                      gen_im(timed,im,name,gen_insert(p,name)));
		      append_rep(p,{name=name,ims=pims_name,
				    order=order,offset=SOME off}))
	       | InstTable.var_offset => 
		     (CPN'Env.use_string
		      ("\n structure "::name::" = CPN'Place.MakeTimedPlaceN\
		       \ (structure sims= "::sims_name::";\
		       \ val no_of_inst = "::no_of_inst::");"::
		      gen_im(timed,im,name,gen_insert(p,name)));
		      append_rep(p,{name=name,ims=pims_name,
				    order=order,offset=NONE}))
	       | _ => raise InternalError ("create_place")) 
	    before (if wasprofiling 
		   then Compiler.Profile.setProfMode true
		   else ())
	end
	else
	let
	    val ims_name = get_gen_pims (cs,kind,order)
	in
	    append_rep(p,{name=name,ims=ims_name,order=order,offset=NONE});
	    if upd_insts_only then
	      (*FIXME: Should we call append_rep in this case?*)
	      (CPN'Env.use_string 
	       ("\n val _ = "::name::".change_no_of_inst("::no_of_inst::");"::nil))
	    else
	      (CPN'Env.use_string 
	       ("\n structure "::name::" = CPN'Place.MakePlace\
		\ (structure ims = "::ims_name::
		"; val no_of_inst = "::no_of_inst::");"::
		gen_im(timed,im,name,gen_insert(p,name))))
	end
    end

    (* Generate a fusion group from scratch if not upd_insts_only otherwise
     * call change_no_of_inst instead to quicky modify number of instances. *)
    fun create_fusion_group (p,cs,im,page,upd_insts_only) = let

	val _ = CPN'debug(concat["create_fusion_group \
				  \(",p,",",cs,",im,",page,")"])

	val {timed,kind,...} = CPN'CSTable.find cs
	val order = InstTable.get_order p
	val no_of_inst = Int.toString(InstTable.get_no_of_inst page)
        val name = gen_place_name p
	val _ = display_status_bar()
    in
        if timed then let
	    (* FIXME: Hack to avoid errors when compiling profiled
	     * code for timed places *)
	    val wasprofiling = Compiler.Profile.getProfMode()
	    val _ = if wasprofiling 
		    then Compiler.Profile.setProfMode false
		    else ()
	    val pims_name = get_gen_pims (cs,kind,order)
	    val sims_name = get_gen_sims (pims_name)
	in
          if upd_insts_only then
            () (* Fusion has only one instance ie no need to change insts *)
          else
	    (case InstTable.get_offset p of
		 InstTable.one_offset off =>
		     (CPN'Env.use_string 
		      ("\n structure "::name::" = CPN'Place.MakeTimedFusion\
		       \ (structure sims= "::sims_name::" and Time = CPN'Time;\
		       \ val offset = (maketime \""::off::"\"));"::
                      gen_im(timed,im,name,gen_insert(p,name)));
		      append_rep(p,{name=name,ims=pims_name,
				    order=order,offset=SOME off}))
	       | InstTable.max_offset off => 
		     (CPN'Env.use_string 
		      ("\n structure "::name::" = CPN'Place.MakeTimedFusionM\
		       \ (structure sims= "::sims_name::" and Time = CPN'Time;\
		       \ val max_offset = (maketime \""::off::"\"));"::
                       gen_im(timed,im,name,gen_insert(p,name)));
		       append_rep(p,{name=name,ims=pims_name,
				     order=order,offset=SOME off}))
	       | InstTable.var_offset => 
		     (CPN'Env.use_string 
		      ("\n structure "::name::" = CPN'Place.MakeTimedFusionN\
		       \ (structure sims= "::sims_name::
		       " and Time = CPN'Time);"::
		       gen_im(timed,im,name,gen_insert(p,name)));
		       append_rep(p,{name=name,ims=pims_name,
		     order=order,offset=NONE}))
	       | _ => raise InternalError ("create_fusion_group"))
	    before (if wasprofiling 
		    then Compiler.Profile.setProfMode true
		    else ())
	end
	else 
	let
	    val ims_name = get_gen_pims (cs,kind,order)
	in
	  append_rep(p,{name=name,ims=ims_name,order=order,offset=NONE});
          if upd_insts_only then
            () (* Fusion has only one instance *)
          else
	    (CPN'Env.use_string 
	     ("\n structure "::name::" = CPN'Place.MakeFusion\
	     \ (structure ims = "::ims_name::");"::
	     gen_im(timed,im,name,gen_insert(p,name))))
	end
    end

    fun create_fusion_member (p,grp,upd_insts_only) = let
	val {name,ims,order,offset,...} = get_rep grp
	val _ = display_status_bar()
    in
	CPN'debug "create_fusion_member";
	append_rep(p,{name=name,ims=ims,order=order,offset=offset}); 
	if upd_insts_only then
          () (* Fusion has only one instance *)
	else
	  CPN'Env.use_string ("\n structure "::(gen_place_name p)::" = "::
			      name::";"::gen_insert(p,name))
    end

    fun create_port (p, sockets: (CPN'Id.id * CPN'Id.id) list,upd_insts_only) = let
	
	val _ = CPN'debug(concat["create_port (",p,",sockets) ",Bool.toString upd_insts_only])
	
	val page_id = (case (CPN'PlaceTable.peek p) of
			   SOME {kind,ext,...} => (#page ext)
			 | NONE => "unknownID")
	val page_instances = CPN'PageTable.get_no_of_inst page_id
	val _ = CPN'debug("number of page instances = "^(Int.toString(page_instances)))
        val inst_sockets = InstTable.get_socket_inst_list p
        val name = gen_place_name p
	val {ims,cs,order,offset,...} = get_rep (#1(hd sockets))
	val _ = display_status_bar()
    in

	if CPN'CSTable.is_timed cs then let

	    fun gen (nil,(marks,waits,next_times,inits)) = 
		("["::tl(marks),
		 "["::tl(waits),
		 "["::tl(next_times),
		 "["::tl(inits))
	      | gen ((s,j)::sjs,(marks,waits,next_times,inits)) = let
		val {name,...} = get_rep s
		val inst = Int.toString j
	    in 
		gen(sjs,
		    (","::name::".mark "::inst::marks,
		     ","::name::".wait "::inst::waits,
		     ","::"("::name::".next_time,"::inst::")"::next_times,
		     ","::"("::name::".init,"::inst::")"::inits))
	    end
	
	    val (marks,waits,next_times,inits) = 
		gen (rev inst_sockets, (["","]"],["","]"],["","]"],["","]"]))

	    val cs_name = CPN'CSTable.gen_prime_name_t cs
	    val sims_name = gen_sims_name(gen_pims_name (cs_name, order))
	in
          if upd_insts_only then
	     (CPN'Env.use_string ("\n val _ = "::name::".change_assignments("::
		  concat(marks)::","::
                  concat(waits)::","::
                  concat(next_times)::","::
                  concat(inits)::")"::nil))
	  else
	    (CPN'Env.use_string ("\n structure "::name::" =\
	     \ CPN'Place.MakeTimedPort (structure sims = "::
	     sims_name::" and Time = CPN'Time;\
	     \\n val marks= "::concat(marks)::
	     "\n and waits= "::concat(waits)::
	     "\n and next_times= "::concat(next_times)::
	     "\n and inits= "::concat(inits)::")"::
	     gen_insert(p,name));
	     append_rep(p,{ims=ims,order=order,name=name,offset=offset}))
	end
	else let
	    fun gen (nil,tail) = ""::"]);"::tail
	      | gen ((s,j:int)::ss,tail) =
		","::(#name(get_rep s))::".mark "::(Int.toString j)::
		gen(ss,tail)
	in
	append_rep(p,{ims=ims,order=order,name=name,offset=offset});
	     if upd_insts_only then
		 (CPN'Env.use_string ("\n val _ = "::name::".change_assignments(["::
		  tl(gen(inst_sockets,gen_insert(p,name)))))
		  (*FIXME: append_rep not necessary in this case?*)
	     else
		 (CPN'debug (concat ("\n structure "::name::" =\
		  \ CPN'Place.MakePort (structure ims = "::ims::";\
		  \ val markings = ["::
		  tl(gen(inst_sockets,gen_insert(p,name)))));
		 (CPN'Env.use_string ("\n structure "::name::" =\
		  \ CPN'Place.MakePort (structure ims = "::ims::";\
		  \ val markings = ["::
		  tl(gen(inst_sockets,gen_insert(p,name)))))    )
	end
    end

    fun create (nil,_) = nil
      | create ((p as (_,{kind=fusion _,...}:item))::places,upd_insts_only) = 
        (* A fusion member, remember it as the group must be create first *)
        p::create(places,upd_insts_only)
      | create ((id,{kind=port nil,ext={cs,im,page,...},...})::places,upd_insts_only) =
        (* The port is not assigned to a socket, create a ordinary place *)
	(create_place(id,cs,im,page,upd_insts_only); 
	 create (places,upd_insts_only))
      | create ((p as (_,{kind=port _,...}))::places,upd_insts_only) = 
        (* A assigned port, remeber it as the socket must be create first *)
	p::create(places,upd_insts_only)
      | create ((id,{kind=group,ext={cs,im,page,...},...})::places,upd_insts_only) =
	(* A fusion group, create it *) 
	(create_fusion_group(id,cs,im,page,upd_insts_only); 
	 create (places,upd_insts_only))
      | create ((id,{kind=place,ext={cs,im,page,...},...})::places,upd_insts_only) =
	(* A ordinary place, create it *) 
	(create_place(id,cs,im,page,upd_insts_only); 
	 create (places,upd_insts_only))

    fun is_all_created (nil,id) = true
      | is_all_created (((s: CPN'Id.id, _)::ss),id) = 
	case peek s of
	    SOME{int=SOME _,...} => is_all_created (ss,id)
	  | _ => 
		(CPN'debug (concat("Socket: "::s::" is not generated,\
		 \ tailing port: "::id::nil));
				   false)

    fun create_tail (nil,nil,_) = ()
      | create_tail ((p as (id,{kind=port sockets,...}))::places,tail,upd_insts_only) = if is_all_created (sockets,id) then
		(create_port (id,sockets,upd_insts_only); 
		 create_tail (places,tail,upd_insts_only))
	    else
		create_tail (places,p::tail,upd_insts_only)
      | create_tail ((p,{kind=fusion grp,...}: item)::places,tail,upd_insts_only) =
	(create_fusion_member (p,grp,upd_insts_only);
	 create_tail (places,tail,upd_insts_only))
      | create_tail (nil,tail,upd_insts_only) = 
	   let
	       val _ = CPN'debug (concat ("creating tailing places: ["::
					  tl(foldr (fn ((a,_),b) => ","::a::b) ["","]"] tail)))
	       fun is_port (id,{kind=port sockets,...}:item) = true
		 | is_port _ = false
	       val (ports,others) = List.partition is_port tail
	       val (ports_with_sockets, ports_missing_sockets) = 
			    List.partition (fn (id,{kind=port sockets,...}:item) => is_all_created (sockets,id)) ports
	       val places = others^^ports_with_sockets
	   in
	       if places<>nil
		   then create_tail (places,ports_missing_sockets,upd_insts_only)
	       else raise InternalError "Trying to create port(s) with missing sockets."
	   end
      | create_tail _ = raise InternalError "create_tail"

    fun create_all () = let
	val _ = CPN'report_timing(concat["create all places"])
	val places = map (fn a => (a,find a))
	                 (CPN'PageTable.list_all_places_top_down())
    in
	total_places:= Int.toString(length places);
	create_tail(create (places,false),nil,false)
    end

    fun create_some (place_ids,upd_insts_only) = let
	val _ = CPN'report_timing(concat["create some places"])
	val places = map (fn a => (a,find a)) place_ids
    in
	total_places:= Int.toString(length places);
        create_tail(create (places,upd_insts_only),nil,upd_insts_only)
    end

end (* local *)
end (* functor *)
