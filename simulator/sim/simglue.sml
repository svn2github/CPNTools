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
(* File: simglue.sml
 *
 * Marshalling facilities to controlling process. Means for an external
 * process to control the simulation process.
 *)

functor CPN'CreateSimGlue(structure Options : CPN'OPTIONS
			  and Decl : CPN'DECL
			  and SyntaxCheck : CPN'SYNTAXCHECK
			  and InstTable: CPN'INSTTABLE
			  and Sim: CPN'SIM
			  and Place : CPN'CREATEPLACE
			  and Reference : CPN'CREATEREFERENCE
			  and Transition: CPN'CREATETRANSITION
			  and CreateMonitor: CPN'CREATEMONITOR
			  and Monitors: CPN'MONITORS
			  and StandardMonitors: CPN'STANDARDMONITORS) = struct

local
    open CpnMLSys.GramError;

    (* split (l,n) = ([l_0,...,l_{n-1}],[l_n,...,l_{length l}]) *)
    fun split (xs, 0) = (nil, xs)
      | split (x::xs, i) =
	let
	    val (ls, rs) = split (xs, i-1)
	in
	    (x::ls, rs)
	end
      | split _ = raise InternalError "split"

    (* pairsplit (l,n) = ([(l_0,l_1),...,(l_{2n-2},l_{2n-1}],
     *                    [l_2n,...,l_{length l}]) *)
    fun pairsplit (xs, 0) = (nil, xs)
      | pairsplit (x1::x2::xs, i) =
	let
	    val (ls, rs) = pairsplit (xs,i-1)
	in
	    ((x1,x2)::ls, rs)
	end
      | pairsplit _ = raise InternalError "pairsplit"
    
    fun unwrap_decl ress = let
	
	fun unwrap_res nil = (nil,nil,nil)
	  | unwrap_res ((id,err,(l1,l2))::ress) =
	    let
		val (blist,ilist,slist) = unwrap_res ress;
		val l = l1^^l2;
	    in
		(blist, (length l1)::((length l2)::ilist), id::err::(l^^slist))
	    end
    
	val (blist,ilist,slist) = unwrap_res ress
    in
	(blist, (length ress)::ilist, slist)
    end


    (* Reduces a code string to make it a more managble size in an
     * error message. *)
    fun clip_code_str str = 
	let
	    val transprefix = "\n structure CPN'transition"
	    fun getTransPrefix transcode = 
		let
		    val (pref,suff) = Substring.position transprefix (Substring.full transcode)
		    val (l,r) = Substring.splitl (fn c => c<>(#"=")) (suff)
		    val fullpref = Substring.concat [pref,l]
		    val commentstr = "= struct ... end (* see simulator debug info for full code *)\n"
		in
		    fullpref^commentstr
		end
	in
	    if isSubstring transprefix str 
		then (getTransPrefix str)
	    else str
	end

in    
    fun misc ([], [8, port], [host]) = 
        ((CpnMLSys.Extension.configure (host, port); ([true],[],[]))
        handle exn => ([false], [], ["Exception relocating extension server: " ^
        exnName(exn)]))
      | misc ([],[9],[model_name, model_dir, output_dir]) = 
	(* set model name and model and output directories *)
	((Output.setModelNameAndDirs 
	      (model_name, model_dir, 
	       if output_dir = "" 
	       then (* default output dir *) NONE
	       else SOME output_dir);
	  ([true],[],[]))
	 handle NotValidDirExn s => ([false],[],[s])
	      | CPN'Error s => ([false],[],["Error setting paths:\n"^s])
	      | exn => ([false],[],["Exception raised while setting paths: "^exnName(exn)]))
      | misc ([pause_before, pause_after, pause_show,
	       report_trans, report_binds, 
	       show_marking, show_enabling, fair_be, global_fair],
	      [10],
	      [until_step, add_step, until_time, add_time, 
	       pause_cont, report_func]) =
	(* set sim options *)
	((Options.set_sim{stop_crit= (until_step,add_step,until_time,add_time),
			  pause= (pause_before,pause_after,pause_show,pause_cont),
			  report= (report_trans,report_binds,report_func),
                    show= (show_marking,show_enabling),
                    fairness = (fair_be, global_fair)};
	  ([true],nil,nil))
	 handle NotValidDirExn s => ([false],nil,[s])
	      | exn => ([false],[],["Error setting simulation options: "^
				    exnName(exn)]))
      | misc ([reset_ran, reset_ref],[11],[seed_no])=
	(* set init options *)
	(Options.set_init{seed= (seed_no),
			  reset= (reset_ran, reset_ref)};
	 (nil,nil,nil))
      | misc ([timerep,piecerep,withsize,withsizeminor,excelrep],
	      [12],
	      [timeunits,costunits,
	       starttime,stoptime,timeinterval,
	       startpiece,stoppiece,pieceinteval]) =
	(* set WFA attributes *)
	raise InternalError "Not implemented"
      | misc (blist,[13],[]) = 
	(* set options for simulation performance report *)
	if (length blist <> 27)
	then raise InternalError ("Expecting 27 boolean values for selecting which statistics to include in simulation performance report.\nReceived "^(Int.toString (length blist))^" values instead.")
	else 
	    let
		val timedstats = {avrg=List.nth(blist,0),
				  ci=List.nth(blist,1),
				  count=List.nth(blist,2),
				  first=List.nth(blist,3),
				  last=List.nth(blist,4),
				  max=List.nth(blist,5),
				  min=List.nth(blist,6),
				  ss=List.nth(blist,7),
				  ssd=List.nth(blist,8),
				  std=List.nth(blist,9),
				  sum=List.nth(blist,10),
				  vari=List.nth(blist,11),
				  starttime=List.nth(blist,12),
				  interval=List.nth(blist,13),
				  lasttime=List.nth(blist,14)}

		val untimedstats = {avrg=List.nth(blist,15),
				    ci=List.nth(blist,16),
				    count=List.nth(blist,17),
				    first=List.nth(blist,18),
				    last=List.nth(blist,19),
				    max=List.nth(blist,20),
				    min=List.nth(blist,21),
				    ss=List.nth(blist,22),
				    ssd=List.nth(blist,23),
				    std=List.nth(blist,24),
				    sum=List.nth(blist,25),
				    vari=List.nth(blist,26)}
	    in 
		(CPN'PerfReport.selectUntimedStats(untimedstats);
		 CPN'PerfReport.selectTimedStats(timedstats);
		 ([],[],[]))
	    end
      | misc (blist,[14],[]) = 
	(* set options for replication/iid performance report *)
	if (length blist <> 12)
	then raise InternalError ("Expecting 12 boolean values for selecting which statistics to include in replication/iid performance report.\nReceived "^(Int.toString (length blist))^" values instead.")
	else 
	    let
		val stats = {avrg=List.nth(blist,0),
			     ci=List.nth(blist,1),
			     count=List.nth(blist,2),
			     first=List.nth(blist,3),
			     last=List.nth(blist,4),
			     max=List.nth(blist,5),
			     min=List.nth(blist,6),
			     ss=List.nth(blist,7),
			     ssd=List.nth(blist,8),
			     std=List.nth(blist,9),
			     sum=List.nth(blist,10),
			     vari=List.nth(blist,11)}
	    in
		(CPN'PerfReport.selectIIDStats(stats);
		 ([],[],[]))
	    end
      | misc (blist,15::ilist,[]) = 
	(* select confidence levels for confidence intervals
	 * in performance reports *)
	((CPN'PerfOptions.set_ci_percentages ilist;
	  ([true],nil,nil))
	 handle CPN'Error errmsg => ([false],nil,[errmsg]))

      | misc (nil, [20], [filename]) = 
	    ((([true],nil,nil) before (CpnML.Toploop.build filename))
	 handle IO.Io {name,...} => ([false],nil,["Error cannot save "^name]))

      | misc (nil,[30],[ogpath]) = 
	(* enter state space tool *)
	((CPN'Env.use_string ["val ogpath=\""^ogpath^"\"\n"];
	  use (ogpath^"switch1.sml");
	  use (ogpath^"switch2.sml");
	  use (ogpath^"switch3.sml");
	  use (ogpath^"switch4.sml");
	  use (ogpath^"switch5.sml");
	  use (ogpath^"switch6.sml");
	  use (ogpath^"switch7.sml");
	  use (ogpath^"switch8.sml");
	  ([true],nil,nil))
	 handle CPN'Error s => ([false],nil,["CPN'Error entering state space tool:\n"^s])
	      | InternalError s => ([false],nil,["InternalError entering state space tool:\n"^s])
	      | IllegalName s => ([false],nil,["IllegalName entering state space tool:\n"^s])
	      | exn => ([false],nil,["Exception raised entering state space tool:\n"^exnName(exn)]))

      | misc _ = (CPN'debug "Match error in SimGlue.misc"; raise Match);
	
    fun create_decl (blist, 1::ilist, slist) = let
	
	(* In the following are nv, nms, na, and nd the number of variables,
	 * multi-set variables, aliases, and declares, respectively.
	 *)

        fun pick_param (timed::blist, nv::nms::na::nd::ilist, name::slist)= let

	    val (var,slist) = split(slist,nv)
	    val (msvar,slist) = split(slist,nms)
	    val (alias,slist) = split(slist,na)
	    val (declare,slist) = split(slist,nd)
	in
	    (blist,ilist,slist,
	     SOME {name= name, 
		   timed= timed, 
		   var= var, 
		   msvar= msvar, 
		   alias= alias,
		   declare= declare})
	end
	  | pick_param _ = raise InternalError "pick_param"

	fun wrap (nil: bool list, nil: int list, nil: string list) = nil
	  | wrap (blist, 1::ilist, id::str::slist) = let
	    (* 1 => unit color-set declaration 
	     * bool list is   [timed,blist]
	     * int list is    [1,nv,nms,na,nd,ilist]
	     * string list is [id,str,name,
	     *	               var...,msvar...,alias...,declare...,slist]
	     *)
	    val (blist,ilist,slist,param) = pick_param (blist,ilist,slist)
	in
	    (id, Decl.unit_cs str, param)::wrap(blist,ilist,slist)
	end
	  | wrap (blist, 2::ilist, id::low::high::slist) = let 
	    (* 2 => bool color-set declaration 
	     * bool list is   [timed,blist]
	     * int list is    [2,nv,nms,na,nd,ilist]
	     * string list is [id,low,high,name,
	     *                 var...,msvar...,alias...,declare...,slist]
	     *)
	    val (blist,ilist,slist,param) = pick_param (blist,ilist,slist)
	in
	    (id, Decl.bool_cs (low,high), param)::wrap(blist,ilist,slist)
	end
	  | wrap (blist, 3::ilist, id::low::high::slist) = let 
	    (* 3 => int color-set declaration 
	     * bool list is   [timed,blist]
	     * int list is    [3,nv,nms,na,nd,ilist]
	     * string list is [id,low,high,name,
	     *                 var...,msvar...,alias...,declare...,slist]
	     *)
	    val (blist,ilist,slist,param) = pick_param (blist,ilist,slist)
	in
	    (id, Decl.int_cs (low,high), param)::wrap(blist,ilist,slist)
	end
	  | wrap (blist, 4::ilist, id::low::high::slist) = let
	    (* 4 => real color-set declaration 
	     * bool list is   [timed,blist]
	     * int list is    [4,nv,nms,na,nd,ilist]
	     * string list is [id,low,high,name,
	     *                 var...,msvar...,alias...,declare...,slist]
	     *)
	    val (blist,ilist,slist,param) = pick_param (blist,ilist,slist)
	in
	    (id, Decl.real_cs (low,high), param)::wrap(blist,ilist,slist)
	end
	  | wrap (blist, 5::ilist, id::low::high::min::max::slist) = let 
	    (* 5 => string color-set declaration 
	     * bool list is   [timed,blist]
	     * int list is    [5,nv,nms,na,nd,ilist]
	     * string list is [id,low,high,min,max,name,
	     *                 var...,msvar...,alias...,declare...,slist]
	     *)
	    val (blist,ilist,slist,param) = pick_param (blist,ilist,slist)
	in
	    (id, Decl.string_cs {char= (low,high), length= (min,max)}, param)::
	    wrap(blist,ilist,slist)
	end
	  | wrap (blist, 6::ne::ilist, id::slist) = let 
	    (* 6 => enumerated color-set declaration 
	     * bool list is   [timed,blist]
	     * int list is    [6,ne,nv,nms,na,nd,ilist]
	     * string list is [id,enms...,name,
	     *                 var...,msvar...,alias...,declare...,slist]
	     *)
	    val (enms,slist) = split(slist,ne)
	    val (blist,ilist,slist,param) = pick_param (blist,ilist,slist)
	in
	    (id, Decl.enum_cs enms, param)::wrap(blist,ilist,slist)
	end
 	  | wrap (blist, 7::ilist, id::idx::low::high::slist) = let
	    (* 7 => index color-set declaration 
	     * bool list is   [timed,blist]
	     * int list is    [7,nv,nms,na,nd,ilist]
	     * string list is [id,idx,low,high,name,
	     *                 var...,msvar...,alias...,declare...,slist]
	     *)
	    val (blist,ilist,slist,param) = pick_param (blist,ilist,slist)
	in
	    (id, Decl.index_cs {idx=idx, over=(low,high)}, param)::
	    wrap(blist,ilist,slist)
	end
	  | wrap (blist, 8::ilist, id::cs::min::max::slist) = let
	    (* 8 => list color-set declaration 
	     * bool list is   [timed,blist]
	     * int list is    [8,nv,nms,na,nd,ilist]
	     * string list is [id,cs,min,max,name,
	     *                 var...,msvar...,alias...,declare...,slist]
	     *)
	    val (blist,ilist,slist,param) = pick_param (blist,ilist,slist)
	in
	    (id, Decl.list_cs {cs=cs, length=(min,max)}, param)::
	    wrap(blist,ilist,slist)
	end
	  | wrap (blist, 9::nc::ilist, id::slist) = let
	    (* 9 => product color-set declaration 
	     * bool list is   [timed,blist]
	     * int list is    [9,nc,nv,nms,na,nd,ilist]
	     * string list is [id,comps...,name,
	     *                 var...,msvar...,alias...,declare...,slist]
	     *)
	    val (comps, slist) = split(slist,nc)
	    val (blist,ilist,slist,param) = pick_param (blist,ilist,slist)
	in
	    (id, Decl.product_cs comps, param)::wrap(blist,ilist,slist)
	end
	  | wrap (blist, 10::nc::ilist, id::slist) = let
	    (* 10 => record color-set declaration 
	     * bool list is   [timed,blist]
	     * int list is    [10,nc,nv,nms,na,nd,ilist]
	     * string list is [id,comps...,name,
	     *                 var...,msvar...,alias...,declare...,slist]
	     *)
	    val (comps,slist) = pairsplit(slist,nc)
	    val (blist,ilist,slist,param) = pick_param (blist,ilist,slist)
	in
	    (id, Decl.record_cs comps, param)::wrap(blist,ilist,slist)
	end
	  | wrap (blist, 11::nc::ilist, id::slist) = let 
	    (* 11 => union color-set declaration 
	     * bool list is   [timed,blist]
	     * int list is    [11,nc,nv,nms,na,nd,ilist]
	     * string list is [id,comps...,name,
	     *                 var...,msvar...,alias...,declare...,slist]
	     *)
	    val (comps,slist) = pairsplit(slist,nc)
	    val (blist,ilist,slist,param) = pick_param (blist,ilist,slist)
	in
	    (id, Decl.union_cs comps, param)::wrap(blist,ilist,slist)
	end
	  | wrap (blist, 12::ilist, id::cs::subset::slist) = let
	    (* 12 => function subset color-set declaration 
	     * bool list is   [timed,blist]
	     * int list is    [12,nv,nms,na,nd,ilist]
	     * string list is [id,cs,subset,name,
	     *                 var...,msvar...,alias...,declare...,slist]
	     *)
	    val (blist,ilist,slist,param) = pick_param (blist,ilist,slist)
	in
	    (id, Decl.funsubset_cs {cs=cs, subset=subset}, param)::
	    wrap(blist,ilist,slist)
	end
	  | wrap (blist, 13::ns::ilist, id::cs::slist) = let
	    (* 13 => list subset color-set declaration 
	     * bool list is   [timed,blist]
	     * int list is    [13,ns,nv,nms,na,nd,ilist]
	     * string list is [id,cs,subset,name,
	     *                 var...,msvar...,alias...,declare...,slist]
	     *)

	    val (subset,slist) = split(slist,ns)
	    val (blist,ilist,slist,param) = pick_param (blist,ilist,slist)
	in
	    (id, Decl.listsubset_cs {cs=cs, subset=subset}, param)::
	    wrap(blist,ilist,slist)
	end
	  | wrap (blist, 14::ilist, id::slist) = let
	    (* 14 => time color-set declaration 
	     * bool list is   [timed,blist]
	     * int list is    [14,nv,nms,na,nd,ilist]
	     * string list is [id,name,
	     *                 var...,msvar...,alias...,declare...,slist]
	     *)
	    val (blist,ilist,slist,param) = pick_param (blist,ilist,slist)
	in
	    (id, Decl.time_cs, param)::wrap(blist,ilist,slist)
	end
	  | wrap (blist, 15::ilist, id::cs::slist) = let
	    (* 15 => alias color-set declaration 
	     * bool list is   [timed,blist]
	     * int list is    [14,nv,nms,na,nd,ilist]
	     * string list is [id,cs,name,
	     *                 var...,msvar...,alias...,declare...,slist]
	     *)
	    val (blist,ilist,slist,param) = pick_param (blist,ilist,slist)
	in
	    (id, Decl.alias_cs cs, param)::wrap(blist,ilist,slist)
	end
	  | wrap (blist, 16::ilist, id::name::exp::slist) = 
	    (* 16 => globref declaration 
	     * bool list is   [blist]
	     * int list is    [16,ilist]
	     * string list is [id,name,exp,slist]
	     *)
	    (id, Decl.globref{name=name, exp=exp}, NONE)::
	    wrap(blist,ilist,slist)
	  | wrap (blist, 17::ilist, id::exp::slist) =
	    (* 17 => use file declaration 
	     * bool list is   [blist]
	     * int list is    [17,ilist]
	     * string list is [id,exp,slist]
	     *)
	    (id, Decl.usefile exp, NONE)::wrap(blist,ilist,slist)
	  | wrap (blist, 18::ilist, id::exp::slist) =
	    (* 18 => sml code declaration 
	     * bool list is   [blist]
	     * int list is    [18,ilist]
	     * string list is [id,exp,slist]
	     *)
	    (id, Decl.sml_code exp, NONE)::wrap(blist,ilist,slist)
	  | wrap (blist, 20::n::ilist, id::name::slist) = let
	    (* 20 => append variable
	     * bool list is   [blist]
	     * int list is    [20,n,ilist]
	     * string list is [id,name,var_1...var_n,slist]
	     *)
	    val (vars,slist) = split(slist,n)
	 in
	    (id, Decl.append_var (name,vars), NONE)::
	    wrap(blist,ilist,slist)
	 end
	  | wrap (blist, 21::n::ilist, id::name::slist) = let
	    (* 21 => append ms variable
	     * bool list is   [blist]
	     * int list is    [21,n,ilist]
	     * string list is [id,name,msvar_1...msvar_n,slist]
	     *)
	    val (msvars,slist) = split(slist,n)
	 in
	    (id, Decl.append_msvar (name,msvars), NONE)::
	    wrap(blist,ilist,slist)
	 end
	  | wrap (blist, 22::n::ilist, id::name::slist) = let
	    (* 22 => append alias color-set
	     * bool list is   [blist]
	     * int list is    [22,n,ilist]
	     * string list is [id,name,alias_1...alias_n,slist]
	     *)
	    val (aliases,slist) = split(slist,n)
	 in
	    (id, Decl.append_alias (name,aliases), NONE)::
	    wrap(blist,ilist,slist)
	 end
	  | wrap (blist, 23::n::ilist, id::name::slist) = let
	    (* 23 => append declare functions
	     * bool list is   [blist]
	     * int list is    [23,n,ilist]
	     * string list is [id,name,decl_1...decl_n,slist]
	     *)
	    val (decls,slist) = split(slist,n)
	 in
	    (id, Decl.append_decl (name,decls), NONE)::
	    wrap(blist,ilist,slist)
	 end
	  | wrap (blist, 24::ilist, id::low::high::slist) = let 
	    (* 24 => intinf color-set declaration 
	     * bool list is   [timed,blist]
	     * int list is    [24,nv,nms,na,nd,ilist]
	     * string list is [id,low,high,name,
	     *                 var...,msvar...,alias...,declare...,slist]
	     *)
	    val (blist,ilist,slist,param) = pick_param (blist,ilist,slist)
	in
	    (id, Decl.intinf_cs (low,high), param)::wrap(blist,ilist,slist)
	end
	  | wrap (blist, 25::ilist, id::typ::name::slist) = let 
	    (* 25 => channel declaration
	     * bool list is   [blist]
	     * int list is    [25,ilist]
	     * string list is [id,type,name,slist]
	     *)
	in
	    (id, Decl.channel (typ, name), NONE)::wrap(blist,ilist,slist)
	end
	  | wrap (blist, 26::ilist, id::low::high::slist) = let 
	    (* 24 => invariant declaration
	     * bool list is   [timed,blist]
	     * int list is    [24,nv,nms,na,nd,ilist]
	     * string list is [id,low,high,name,
	     *                 var...,msvar...,alias...,declare...,slist]
	     *)
	    val (blist,ilist,slist,param) = pick_param (blist,ilist,slist)
	in
	    (raise Match; (id, Decl.intinf_cs (low,high), param)::wrap(blist,ilist,slist))
	end
	  | wrap _ = 
	    (CPN'debug "Match error in create_decl wrap"; raise Match);

    in
	unwrap_decl(Decl.create_global (wrap(blist,ilist,slist)))
    end
      | create_decl (blist, 2::ilist, slist) = let

        fun wrap (blist, 1::ilist, id::name::exp::page::slist) = 
	    (* 1 => pageref declaration 
	     * bool list is   [blist]
	     * int list is    [1,ilist]
	     * string list is [id,name,exp,page,slist]
	     *)
	    (id, Decl.pageref{name=name, exp=exp, page=page})::
	    wrap(blist,ilist,slist)
	  | wrap (blist, 2::ilist, id::name::exp::page::slist) = 
	    (* 2 => instref declaration 
	     * bool list is   [blist]
	     * int list is    [2,ilist]
	     * string list is [id,name,exp,page,slist]
	     *)
	    (id, Decl.instref{name=name, exp=exp, page=page})::
	    wrap(blist,ilist,slist)
	  | wrap _ = (CPN'debug "Match error in create_decl wrap"; raise Match)
    in
	unwrap_decl(Decl.create_local (wrap(blist,ilist,slist)))
    end
      | create_decl _ = (CPN'debug "Match error in create_decl"; raise Match)

    fun syntax_check (nil, 1::nil, nil) = 
	((SyntaxCheck.init_check(); ([],nil,[]))
	handle IO.Io {name,...} => (nil,nil,["Could not open dump file "^name]))

      | syntax_check (blist, 2::ilist, slist) = let
	(* 
	 * [],
	 * [prime,nchkr,nchkp,nchkt,ninstrefs,npagerefs,nplaces,nfusions,
	 *  nsubs,nborder_1,..,nborder_nsubs,
	 *  ntrans,nin_1,nout_1,ninout_1,ninhibitor_1,nreset_1...,nin_ntrans,...],
	 * slist
	 *)

	fun wrap (included::blist, 
		  prime::nchkp::nchkt::nplaces::nfusions::ilist,
		  id::name::slist) = let

	    val (places, slist) = split(slist,nchkp) 
	    val (transitions, slist) = split(slist,nchkt)

	    val checked = {places=places,
			   transitions=transitions}

	    fun wrap_places (0,places,slist) = (places,slist)
	      | wrap_places (n,ps,id::name::cs::im::slist) =
		wrap_places (n-1,{id=id,name=name,cs=cs,im=im}::ps,slist)
	      | wrap_places _ = 
		(CPN'debug "Match error in wrap_places";raise Match);
		
	    val (places,slist) = wrap_places (nplaces,nil,slist)
		
	    fun wrap_fusions (0,fusions,slist) = (fusions,slist)
	      | wrap_fusions (n,fs,id::grp::name::cs::im::slist) =
		wrap_fusions (n-1,
			      {id=id,grp=grp,name=name,cs=cs,im=im}::fs,
			      slist)
	      | wrap_fusions _ = 
		(CPN'debug "Match error in wrap_fusions";raise Match);
		
	    val (fusions,slist) = wrap_fusions (nfusions,nil,slist)
		
	    fun wrap_substitutions (0,subs,ilist,slist) = (subs,ilist,slist)
	      | wrap_substitutions (n, subs, nborder::ilist,
				    id::name::subpage::slist) = let
		
		fun wrap_border (0,borders,slist) = (borders,slist)
		  | wrap_border (n,bs,port::socket::slist) =
		    wrap_border (n-1,{port=port,socket=socket}::bs,slist)
		  | wrap_border _ = 
		(CPN'debug "Match error in wrap_border";raise Match);		    
			
		val (border,slist) = wrap_border (nborder,nil,slist)
	    in
		wrap_substitutions (n-1,
				    {id=id, name=name,
				     subpage=subpage, border=border}::subs,
				    ilist, 
				    slist)
	    end
	      | wrap_substitutions _ = 
		(CPN'debug "Match error in wrap_substitutions";raise Match);		    		
	    val (substitutions,ilist,slist) = 
		wrap_substitutions (hd(ilist),nil,tl(ilist),slist)
		    
                  fun wrap_transitions (0,trans,blist,ilist,slist) =
                      (trans,blist,ilist,slist)
                    | wrap_transitions (n,trans,[],ilist,slist) =
                    wrap_transitions (n, trans, [true], ilist, slist)
	      | wrap_transitions
            (n,trans,controllable::blist,nin::nout::ninout::ninhibitor::nreset::ilist,
				  id::name::guard::treg::creg::chan::priority::slist) = let
		    
	        fun wrap_arcs (0,arcs,slist) = (arcs,slist)
		  | wrap_arcs (n,arcs,id::place::exp::slist) =
		    wrap_arcs (n-1,{id=id,place=place,exp=exp}::arcs,slist)
		  | wrap_arcs _ = 
		    (CPN'debug "Match error in wrap_arcs"; raise Match)
			
		val (input,slist) = wrap_arcs (nin,nil,slist)
		val (output,slist) = wrap_arcs (nout,nil,slist)
		val (inout,slist) = wrap_arcs (ninout,nil,slist)
		val (inhibitor,slist) = wrap_arcs (ninhibitor,nil,slist)
		val (reset,slist) = wrap_arcs (nreset,nil,slist)
	    in
		wrap_transitions(n-1,
				 {id=id, name=name, guard=guard,
				  time_reg=treg, code_reg=creg, chan_reg=chan, priority_reg=priority,
				  input=input, output=output, inout=inout, inhibitor =
                          inhibitor, reset=reset,controllable
                          = controllable}::
                          trans,
                          blist,
				 ilist,
				 slist)
	    end
	      | wrap_transitions _ = 
		    (CPN'debug "Match error in wrap_transitions"; raise Match)
		    
	    val (transitions,blist,ilist,slist) = 
		wrap_transitions (hd(ilist),nil,blist,tl(ilist),slist)
	in
	    {id=id, 
	     name=name, 
	     prime=prime, 
	     included=included,
	     checked=checked,
	     places=places, 
	     fusions=fusions,
	     substitutions=substitutions, 
	     transitions=transitions}
	end
	  | wrap _ = 
	    (CPN'debug "Match error in syntax_check wrap"; raise Match)
		
	fun unwrap'err ress = let
	    fun unwrap' nil = (nil,nil,nil)
	      | unwrap' ((id,err)::ress) = let
		val (blist,ilist,slist) = unwrap' ress
	    in
		(blist, ilist, id::err::slist)
	    end
	
	    val (blist,ilist,slist) = unwrap' ress
	in
	    (blist, (length ress)::ilist, slist)
	end

	fun unwrap'use use (bl1, il1, sl1)
	    = let
		  fun unwrap' nil = (nil,nil,nil)
		    | unwrap' ((id,use_lst)::ress)
		      = let
			    val (blist,ilist,slist) = unwrap' ress
			in
			    (blist, 
			     (length use_lst)::ilist, 
			     (id::use_lst)^^slist)
			end;
			
		  val (blist,ilist,slist) = unwrap' use
	      in
		  (bl1^^blist,il1^^((length use)::ilist),sl1^^slist)
	      end

	fun unwrap'ast asts (bl1, il1, sl1) = let
	    fun unwrap' nil = (nil,nil,nil)
	      | unwrap' ((id,ast)::asts) = let
		val (blist,ilist,slist) = unwrap' asts
	    in
		(blist, ilist, id::ast::slist)
	    end
	
	    val (blist,ilist,slist) = unwrap' asts
	in
         (bl1^^blist,il1^^((length asts)::ilist),sl1^^slist)
	end

    in
	let
	    val (err,use,ast) = SyntaxCheck.check_page (wrap (blist,ilist,slist))
	in
	    unwrap'ast ast (unwrap'use use (unwrap'err err))
	end
	handle SyntaxCheck.SyntaxError s =>
	    (CPN'debug ("SyntaxError "^s); (nil,[~1],nil))
	      | InternalError s =>
	    (CPN'debug ("InternalError "^s); raise InternalError s)
    end
      | syntax_check (nil, 3::nil, mark::cs::nil) = let
	val valid_mark = SyntaxCheck.check_mark(mark,cs)
    in
	([valid_mark],nil,[])
    end
      | syntax_check (nil, 4::nil, cs1::cs2::nil) = let
	val equal_cs = SyntaxCheck.equal_cs(cs1,cs2)
    in
	([equal_cs],nil,[])
    end
      | syntax_check _ = raise InternalError("syntax_check")


    (* start the simulator, i.e., create instances and initial state 
     * and return the page structure *)
    fun simulate (nil, 1::nil, nil) = let

	fun unwrap nil = (nil,nil,nil)
	  | unwrap ((page, connections)::pages) = let

	    fun unwrap' nil = unwrap pages
	      | unwrap' (NONE::cons) = 
		let
		    val (blist, ilist, slist) = unwrap' cons
		in 
		    (false::blist,ilist,slist)
		end
	      | unwrap' (SOME(superpage,supernode,superinst)::cons) =
		let
		    val (blist, ilist, slist) = unwrap' cons
		in 
		    (true::blist,superinst::ilist,superpage::supernode::slist)
		end

	    val (blist, ilist, slist) = unwrap' connections
        in
	    (blist, (length connections)::ilist, page::slist)
	end

	val _ = InstTable.init()
	val _ = Place.create_all()
	val _ = Reference.create_all()
	val _ = Transition.create_all()
	val _ = Sim.dump_inst()
	val _ = Sim.create_scheduler()
	val _ = Sim.reset_scheduler()
	val pages = InstTable.get_page_structure()
	val (blist,ilist,slist) = unwrap pages
    in
	(blist,(length pages)::ilist,slist)
    end

      (* Return time and step information *)
      | simulate (nil, 2::nil, nil) = 
        (nil,nil,[Sim.Time.mkstr (Sim.Time.time()),IntInf.toString(Sim.step())])

      (* Incrementally update instances *)
      | simulate (upd_insts_only::nil, 3::n_places::n_refs::n_trans::nil, inst_ids) =
	((let
	    val place_ids= List.take(inst_ids,n_places)
	    val inst_ids= List.drop(inst_ids,n_places)

	    val ref_ids= List.take(inst_ids,n_refs)
	    val inst_ids= List.drop(inst_ids,n_refs)

	    val trans_ids= List.take(inst_ids,n_trans)

	    val _ = InstTable.init()
	    (* FIXME: remove following two refs when code dump has been impr*)
	    val _ = CPN'Env.use_string ["\nval _ = CPN'Sim.instances_changed:=true;"];
	    val _ = CPN'Env.use_string ["\nval _ = CPN'Sim.generate_instances:=true;"];
	    val _ = Place.create_some(place_ids,upd_insts_only)
	    val _ = Reference.create_some(ref_ids)
	    val _ = Transition.create_some(trans_ids)

	    fun unwrap nil = (nil,nil,nil)
	      | unwrap ((page, connections)::pages) = let
		
		 fun unwrap' nil = unwrap pages
	           | unwrap' (NONE::cons) = 
		   let
		      val (blist, ilist, slist) = unwrap' cons
		   in 
		      (false::blist,ilist,slist)
		   end
	           | unwrap' (SOME(superpage,supernode,superinst)::cons) =
		   let
		      val (blist, ilist, slist) = unwrap' cons
		   in 
		    (true::blist,superinst::ilist,superpage::supernode::slist)
		   end

		 val (blist, ilist, slist) = unwrap' connections
	       in
		   (blist, (length connections)::ilist, page::slist)
	       end

	    val pages = InstTable.get_page_structure()
	    val (blist,ilist,slist) = unwrap pages
	in
	    (blist,(length pages)::ilist,slist)
	end)
	      handle InternalError s =>
		  (CPN'debug ("InternalError "^s); 
		   raise InternalError (clip_code_str s)))

      (* Update number of instances for a number of pages. *)
      | simulate (nil, 4::n_pages::nil, page_ids) =
	let

	    val _ = InstTable.init()
	    val page_ids= List.take(page_ids,n_pages)
	    val places_each_page= map CPN'PageTable.get_places page_ids

	    (* It is not necessary to update transitions because they are
	     * independent of instances. *)
	    val _ = map (fn pgplaces=> Place.create_some(pgplaces,true)) 
	            places_each_page
	in
	     (nil,nil,nil)
	end

      (* Build and initialise simulation scheduler. *)
      | simulate (nil, 5::nil, nil) =
	let
	    val _ = Sim.create_scheduler()
	    val _ = Sim.reset_scheduler()
	in
	     (nil,nil,nil)
	end

      (* Start a run. *)
      | simulate (nil, 11::nil, nil) = 
        let
	    val (step,time,res) = Sim.run()
	in
	     (nil,nil,[step,time,res])
	end 

      (* Fire the given transition instance, no manual binding. *)
      | simulate (nil, 12::inst::nil, tid::nil) = 
	let
	    val (step,time,res) = Sim.man_bind(tid,inst,false) 
	in
	    (nil,nil,[step,time,res])
	end

      (* Check enabling assuming that the scheduler has been created. *)
      | simulate (nil, 13::inst::nil, tid::nil) = 
	([Sim.check_enab(tid,inst)],nil,nil)

      (* Check enabling before the scheduler has been created *)
      | simulate (nil, 14::inst::nil, tid::nil) = 
      ([false],nil,nil)
	(* ([Sim.check_enab_no_scheduler(tid,inst)],nil,nil) *)

      (* Do manual binding. Requires interaction with user. *)
      | simulate (nil, 15::inst::nil, tid::nil) = 
	let
	    val (step,time,res) = Sim.man_bind(tid,inst,true)
	in
	    (nil,nil,[step,time,res])
	end

      (* Reset step, time, and random generator of simulator *)
      | simulate (nil, 19::nil, nil) = 
	(Sim.reset_sim();
	 (nil,nil,nil))

      (* Initialise state of simulator *)
      | simulate (nil, 20::nil, nil) = 
	(Sim.init_state();
	 (nil,nil,nil))

      (* Create+reset scheduler, reset simulator, and initialise state 
       * combination of 5, 19 and 20 from above *)
      | simulate (nil, 21::nil, nil) = 
	(Sim.init_all();
	 (nil,nil,nil))

      (* Change marking of a list of places. *)
      | simulate (toinit::nil, 22::np::ilist, slist) = let
	fun wrap (nil, nil, places) = places
	  | wrap (inst::ilist, pid::slist, places) = 
	    wrap (ilist,slist, (pid,inst)::places)
	  | wrap _ =
	    (CPN'debug "Match error in simulate wrap"; raise Match)
	in
	    (Sim.change_mark (wrap(ilist,slist,nil),toinit);
	     (nil,nil,nil))
	end

      (* Change model time. *)
      | simulate (nil, 23::nil, time::nil) = 
        (Sim.change_model_time time;
	 (nil,nil,nil))

      (* Increase model time. *)
      | simulate (nil,24::nil,nil) =
	let
	    val (success,msg) = Sim.increase_model_time()
	in
	    ([success],nil,[msg])
	end

      (* Print marking of a list of place instances. *)
      | simulate (nil, 31::np::ilist, slist) = let

	fun wrap (nil,nil,nil) = nil
	      | wrap (nil,i::ilist,pid::slist) = (pid,i)::wrap(nil,ilist,slist)
	      | wrap _ = 
		(CPN'debug "Match error in simulate wrap"; raise Match)

	val insts = wrap(nil,ilist,slist)
	val marks = map (fn (pid,i) => 
			 CPN'Places.print_mark (pid,i)
			 handle InternalError s => "Error! InternalError "^s^" raised when checking marking"
			      | exn => "Error! Exception "^exnName(exn)^" raised when checking marking") insts
	val sizes = map (fn (pid,i) => 
			 CPN'Places.size_mark (pid,i)
			 handle exn => ~1) insts

    in
	    (nil,(length marks)::sizes,marks)
    end

      (* Print marking size of a list of place instances. *)
      | simulate (nil, 32::np::ilist, slist) = let

	fun wrap (nil,nil,nil) = nil
	      | wrap (nil,i::ilist,pid::slist) = (pid,i)::wrap(nil,ilist,slist)
	      | wrap _ = raise InternalError("SimGlue.print_size")

	fun unwrap sizes = let
	    fun unwrap' nil = (nil,nil,nil)
	      | unwrap' (n::sizes) = let
		val (blist,ilist,slist) = unwrap' sizes
	    in
		(blist, n::ilist, slist)
	    end
	
	    val (blist,ilist,slist) = unwrap' sizes
	in
	    (blist, (length sizes)::ilist, slist)
	end
    in
	unwrap(Sim.print_size(wrap(nil,ilist,slist)))
    end

      (* Print the enabling of a list of transition instances. *)
      | simulate (nil, 35::nt::ilist, slist) = let

	 fun wrap (nil,nil,nil) = nil
	   | wrap (nil,i::ilist,tid::slist) = (tid,i)::wrap(nil,ilist,slist)
	   | wrap _ = 
		(CPN'debug "Match error in simulate wrap";
		 raise Match);

	 fun unwrap states = let
	     fun unwrap' nil = (nil,nil,nil)
	       | unwrap' ((state,msg)::states) = let
		 val (blist,ilist,slist) = unwrap' states
	     in
		 (state::blist, ilist, msg::slist)
	     end

	     val (blist,ilist,slist) = unwrap' states
	 in
	     (blist, (length states)::ilist, slist)
	 end

	 val tilist = wrap(nil,ilist,slist)

	 val enab_states = map (fn (tid,i) =>
				(Sim.check_enab (tid,i), "")
				handle InternalError s => (false, "Error! InternalError "^s^" raised when checking enabling")
				     | exn => (false, "Error! Exception "^exnName(exn)^" raised when checking enabling")) tilist

    in
	unwrap(enab_states)
    end

      (* Print the enabling of a list of transition instances
       * without assuming the existence of the scheduler. *)
      | simulate (nil, 36::nt::ilist, slist) = let

	 fun wrap (nil,nil,nil) = nil
	   | wrap (nil,i::ilist,tid::slist) = (tid,i)::wrap(nil,ilist,slist)
	   | wrap _ = 
		(CPN'debug "Match error in simulate wrap";
		 raise Match);

	 fun unwrap states = let
	     fun unwrap' nil = (nil,nil,nil)
	       | unwrap' ((state,msg)::states) = let
		 val (blist,ilist,slist) = unwrap' states
	     in
		 (state::blist, ilist, msg::slist)
	     end

	     val (blist,ilist,slist) = unwrap' states
	 in
	     (blist, (length states)::ilist, slist)
	 end

	 val tilist = wrap(nil,ilist,slist)

	 val enab_states = map (fn (tid,i) =>
				(Sim.check_enab_no_scheduler (tid,i), "")
				handle InternalError s => (false, "Error! InternalError "^s^" raised when checking enabling")
				     | exn => (false, "Error! Exception "^exnName(exn)^" raised when checking enabling")) tilist
    in
	unwrap(enab_states)
    end

      (* Save simulation report to file. *)
      | simulate (nil,41::nil,filename::nil) =
        (Sim.save_report(filename); (nil,nil,nil))

      (* Clear simulation report. *)
      | simulate (nil,42::nil,nil) =
        (Sim.clear_report(); (nil,nil,nil))

      (* Reset transition scheduler *)
      | simulate (nil,43::nil,nil) =
	(Sim.reset_scheduler(); (nil,nil,nil))

      (* Unknown opcode. *)
      | simulate _ = (CPN'debug "Match error in simulate"; raise Match);


local
    fun pick_node_ids (blist, np::nt::ilist, slist)= let
	val (placeids,slist) = split(slist,np)
	val (placeinsts,ilist)= split(ilist,np)
	val (transids,slist) = split(slist,nt)
	val (transinsts,ilist) = split(ilist,nt)
    in
	    (blist,ilist,slist,
	     ListPair.zip (placeids, placeinsts),
	     ListPair.zip (transids, transinsts))
    end
      | pick_node_ids _ = raise InternalError "pick_node_ids"
in
    (* Set the order of the monitors *)
    fun monitor ([]:bool list,[1,nm],slist) = 
	(CPN'MonitorTable.set_order(#1(split(slist,nm)));
	 ([],[],[]))

      (* Generate template functions *)
      | monitor (blist,2::ilist,slist) = 
	let 
	    fun unwrap_templates ress = let
		fun unwrap_res nil = (nil,nil,nil)
		  | unwrap_res ((id,(true,templatefuns))::ress) =
		    let
			val (blist,ilist,slist) = unwrap_res ress
		    in
			(true::blist, (length templatefuns)::ilist, 
			 id::(templatefuns^^slist))
		    end
		  | unwrap_res ((id,(false,errmsgs))::ress) =
		    let
			val (blist,ilist,slist) = unwrap_res ress
		    in
			(false::blist, 
			 (* two strings per errmsg: id + errstr *)
			 ((length errmsgs) div 2)::ilist, 
			 id::(errmsgs^^slist))
		    end
		val (blist,ilist,slist) = unwrap_res ress
	    in
		(blist, (length ress)::ilist, slist)
	    end
		
	    fun wrap (nil: bool list, nil: int list, nil: string list) = nil
	      | wrap (blist, 1::ilist, id::slist) = let
		    (* 1 => breakpoint template functions
		     * bool list is   [blist]
		     * int list is    [1,np,nt,pinst...,tinst...,ilist]
		     * string list is [id,pid...,tid...,slist]
		     *)
		    val (blist,ilist,slist,places,transitions) = 
			pick_node_ids (blist,ilist,slist)

		    (* FIXME: The following probably ought to be in 
		     * CPN'MonitorTemplate, but this way new functors 
		     * don't have to be created *)
		    val _ = CreateMonitor.remove id
		in
		    (id,CPN'MonitorTemplate.breakpoint_templates(transitions,places))::wrap(blist,ilist,slist)
		end
	      | wrap (blist, 2::ilist, id::slist) = let
		    (* 2 => user_def template functions
		     * bool list is   [blist]
		     * int list is    [2,np,nt,pinst...,tinst...,ilist]
		     * string list is [id,pid...,tid...,slist]
		     *)
		    val (blist,ilist,slist,places,transitions) = 
			pick_node_ids (blist,ilist,slist)

		    (* FIXME: The following probably ought to be in 
		     * CPN'MonitorTemplate, but this way new functors 
		     * don't have to be created *)
		    val _ = CreateMonitor.remove id
		in
		    (id,CPN'MonitorTemplate.user_def_templates(transitions,places))::wrap(blist,ilist,slist)
		end
	      | wrap (timed::blist, 3::ilist, id::slist) = let
		    (* 3 => data collector template functions
		     * bool list is   [timed::blist]
		     * int list is    [3,np,nt,pinst...,tinst...,ilist]
		     * string list is [id,pid...,tid...,slist]
		     *)
		    val (blist,ilist,slist,places,transitions) = 
			pick_node_ids (blist,ilist,slist)

		    (* FIXME: The following probably ought to be in 
		     * CPN'MonitorTemplate, but this way new functors 
		     * don't have to be created *)
		    val _ = CreateMonitor.remove id
		in
		    (id,CPN'MonitorTemplate.datacoll_templates(timed,transitions,places))::wrap(blist,ilist,slist)
		end
	      | wrap (blist, 4::ilist, id::slist) = let
		    (* 4 => write file template functions
		     * bool list is   blist
		     * int list is    [4,np,nt,pinst...,tinst...,ilist]
		     * string list is [id,pid...,tid...,slist]
		     *)
		    val (blist,ilist,slist,places,transitions) = 
			pick_node_ids (blist,ilist,slist)

		    (* FIXME: The following probably ought to be in 
		     * CPN'MonitorTemplate, but this way new functors 
		     * don't have to be created *)
		    val _ = CreateMonitor.remove id
		in
		    (id,CPN'MonitorTemplate.write_file_templates(transitions,places))::wrap(blist,ilist,slist)
		end

	      | wrap _ = 
		(CPN'debug "Match error in monitor 2 wrap"; raise Match);
	in
	    unwrap_templates(wrap(blist,ilist,slist))
	end

      (* Syntax check and create monitors *)
      | monitor (blist,3::ilist,slist) = let

	fun unwrap_monitor ress = let
	    fun unwrap_res nil = (nil,nil,nil)
	      | unwrap_res ((id,errs,(l1,l2))::ress) =
		let
		    val (blist,ilist,slist) = unwrap_res ress
		    val errlist = 
			List.foldr (fn ((i,errstr),tail) => i::errstr::tail) [] errs
		    val l = errlist^^l1^^l2
		in
		    (blist, (length errs)::(length l1)::((length l2)::ilist), 
		     id::(l^^slist))
		end
		    
	    val (blist,ilist,slist) = unwrap_res ress
	in
	    (blist, (length ress)::ilist, slist)
	end
	    
	fun wrap (nil: bool list, nil: int list, nil: string list) = nil
	  | wrap (blist, 1::ilist, id::name::predid::predfun::slist) = let
		(* 1 => breakpoint monitor
		 * bool list is   [blist]
		 * int list is    [1,np,nt,pinst...,tinst...,ilist]
		 * string list is [id,name,predfunid,predfun,
				   pid...,tid...,slist]
		 *)
		val (blist,ilist,slist,places,transitions) = 
		    pick_node_ids (blist,ilist,slist)
	    in
		(id, {kind=CPN'MonitorTable.breakpoint(predid,predfun),
		      (* FIXME: differentiate between step and sim monitors *)
		      montype = CPN'MonitorTable.step_monitor,
		      name=name,
		      places=places,
		      transitions=transitions})::wrap(blist,ilist,slist)
	    end
	  | wrap (blist, 2::ilist, id::name::initid::initfun::
				   predid::predfun::obsid::obsfun::
				   actionid::actionfun::stopid::stopfun
				   ::slist) = let
		(* 2 => user_def monitor
		 * bool list is   [blist]
		 * int list is    [1,np,nt,pinst...,tinst...,ilist]
		 * string list is [id,name,initid,initfun,
				   predid,predfun,obsid,obsfun,
				   actionid,actionfun,stopid,stopfun,
				   pid...,tid...,slist]
		 *)
		val (blist,ilist,slist,places,transitions) = 
		    pick_node_ids (blist,ilist,slist)
	    in
		(id, {kind=CPN'MonitorTable.user_def
			       (* FIXME should there be an aux value ?*)
			       {aux=(id^"_1",""),
				init=(initid,initfun),
				stop=(stopid,stopfun),
				pred=(predid,predfun),
				obs=(obsid,obsfun),
				act=(actionid,actionfun)},
		      (* FIXME: differentiate between step and sim monitors *)
		      montype = CPN'MonitorTable.step_monitor,
		      name=name,
		      places=places,
		      transitions=transitions})::wrap(blist,ilist,slist)
	    end
	  | wrap (timed::updatelogfile::blist, 3::ilist, 
		  id::name::predid::predfun::obsid::obsfun::
		  initid::initfun::stopid::stopfun::slist) = let
		(* 3 => data collection monitor
		 * bool list is   [timed,blist]
		 * int list is    [1,np,nt,pinst...,tinst...,ilist]
		 * string list is [id,name,predid,predfun,
				   obsid,obsfun,initid,initfun,
				   stopid,stopfun,pid...,tid...,slist]
		 *)
		val (blist,ilist,slist,places,transitions) = 
		    pick_node_ids (blist,ilist,slist)
	    in
		(id, {kind=CPN'MonitorTable.datacoll
			       {init=(initid,initfun),
				stop=(stopid,stopfun), 
				pred=(predid,predfun),
				obs=(obsid,obsfun),
				timed = timed,
				logfile = updatelogfile}, 
			       (* FIXME: differentiate between step and sim monitors *)
		      montype = CPN'MonitorTable.step_monitor,
		      name=name,
		      places=places,
		      transitions=transitions})::wrap(blist,ilist,slist)
	    end

	  | wrap (blist, 4::ilist, id::name::fileext::initid::initfun::
				   predid::predfun::obsid::obsfun::
				   stopid::stopfun::slist) = let
		(* 4 => write_file monitor
		 * bool list is   [blist]
		 * int list is    [1,np,nt,pinst...,tinst...,ilist]
		 * string list is [id,name,fileext,initid,initfun,
				   predid,predfun,obsid,obsfun,
				   stopid,stopfun,pid...,tid...,slist]
		 *)
		val (blist,ilist,slist,places,transitions) = 
		    pick_node_ids (blist,ilist,slist)
	    in
		(id, {kind=CPN'MonitorTable.write_file
			       {init=(initid,initfun),
				stop=(stopid,stopfun),
				pred=(predid,predfun),
				obs=(obsid,obsfun),
				fileext = fileext},
		      (* FIXME: differentiate between step and sim monitors *)
		      montype = CPN'MonitorTable.step_monitor,
		      name=name,
		      places=places,
		      transitions=transitions})::wrap(blist,ilist,slist)
	    end
	  | wrap _ = 
	    (CPN'debug "Match error in monitor 3 wrap"; raise Match);
	    
    in
	unwrap_monitor(CreateMonitor.create_some (wrap(blist,ilist,slist)))
    end
      (* Remove monitors *)
      | monitor ([],[4],mids) = 
	((map CreateMonitor.remove mids;
	  ([true],[],[]))
	 handle exn => ([false],[],["Error removing monitor(s)"]))

      (* Disable and enable monitors *)
      | monitor (blist,[5],mids) = 
	if (length blist) = (length mids)
	then (Monitors.set_is_active (ListPair.zip (mids,blist));
	      ([],[],[]))
	else raise InternalError "CPN'SimGlue.monitor 5"

      (* Create marking size data collector*)
      | monitor ([update_logfile],[20,pinst],[id,name,pid]) = let
	    val ((id,errs,(dep,overwrite)),tid_insts) =
		StandardMonitors.markingSize 
		    (id,name,pid,pinst,update_logfile)
		    
	    val (tids,insts) = ListPair.unzip tid_insts
	in
	    ([], 
	     ((length dep)::(length overwrite)::(length insts)::insts)^^
	     [length errs], 
	     id::(dep^^overwrite^^tids^^
		  (List.foldr (fn ((id,err),tail)=> id::err::tail)  [] errs)))
	end

      (* Create list length data collector*)
      | monitor ([update_logfile],[21,pinst],[id,name,pid]) = let
	    val ((id,errs,(dep,overwrite)),tid_insts) =
		StandardMonitors.listLength 
		    (id,name,pid,pinst,update_logfile)
		    
	    val (tids,insts) = ListPair.unzip tid_insts
	in
	    ([], 
	     ((length dep)::(length overwrite)::(length insts)::insts)^^
	     [(length errs)], 
	     id::(dep^^overwrite^^tids^^
	         (List.foldr (fn ((id,err),tail)=> id::err::tail)  [] errs)))
	end

      (* Create count occurrences data collector*)
      | monitor ([update_logfile],[22,tinst],[id,name,tid]) = let
	    val (id,errs,(dep,overwrite)) = 
		StandardMonitors.countOccurrences
		    (id,name,tid,tinst,update_logfile)
	in
	    ([],[length dep,length overwrite,length errs], 
	     id::(dep^^overwrite^^
		  (List.foldr (fn ((id,err),tail)=> id::err::tail)  [] errs)))
	end

      (* Create place is empty breakpoint*)
      | monitor ([isempty],[40,pinst],[id,name,pid]) = let
	    val (id,errs,(dep,overwrite)) = 
		StandardMonitors.placeIsEmpty
		    (id,name,pid,pinst,isempty)
	in
	    ([],[length dep,length overwrite,length errs], 
	     id::(dep^^overwrite^^
	     (List.foldr (fn ((id,err),tail)=> id::err::tail)  [] errs)))
	end

      (* Create transition is enabled breakpoint*)
      | monitor ([isenabled],[41,tinst],[id,name,tid]) = let
	    val (id,errs,(dep,overwrite)) = 
		StandardMonitors.transitionIsEnabled
		    (id,name,tid,tinst,isenabled)
	in
	    ([],[length dep,length overwrite,length errs], 
	     id::(dep^^overwrite^^
    		 (List.foldr (fn ((id,err),tail)=> id::err::tail)  [] errs)))
	end

      | monitor _ = (CPN'debug "Match error in monitor"; raise Match)

end (* local *)

    fun chart (nil, 1::ilist, nil) =
	(* create charts *)
	((* CPN'sacgen.gen_charts ilist;*) (nil,nil,nil))
      | chart (nil, 2::nbar::nline::nil, slist) = let
	(* init charts *)
	fun init_chart _ (0, slist) = slist
	  | init_chart init (n, chart::slist) = 
	    (init chart; init_chart init (n-1, slist))
	  | init_chart _ _ = (CPN'debug "Match error in init_chart"; raise Match);
(*
	val slist = init_chart BC_init_chart (nbar,slist)
	val slist = init_chart LC_init_chart (nline, slist)
*)      val slist = nil
    in
	case slist of
	    nil => (nil,nil,nil)
	  | _ =>  raise InternalError("SimGlue.charts")
     end
    | chart _ = (CPN'debug "Match error in chart"; raise Match);

    fun extension (b, i, s) =
        CpnMLSys.Extension.forward CpnMLSys.CmdProcess.waitAndRead
        (!CpnMLSys.CmdProcess.theGram, CpnMLSys.CmdProcess.ExtSimResult) 9
        (b, 10000::i, s)

    fun SimGlueInit () =
	(CpnMLSys.SimProcess.NSMisc:= misc;
	 CpnMLSys.SimProcess.NSCompileDecl:= create_decl;
	 CpnMLSys.SimProcess.NSSyntaxCheck:= syntax_check;
	 CpnMLSys.SimProcess.NSSimulate:= simulate;
	 CpnMLSys.SimProcess.NSMonitor:= monitor;
       CpnMLSys.SimProcess.NSChart:= chart;
       CpnMLSys.SimProcess.NSExtension := extension
       )

    val _ = SimGlueInit ()

end (* local *)

end; (* functor SimGlue *)
