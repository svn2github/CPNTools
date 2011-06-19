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
(* File: monitor_aux.sml
 *
 * Auxiliary functions for monitors
 *)

structure CPN'MonitorAux = struct

(* Hack to get access to CPN'InstTable.get_inst_cons without
 * having to convert CPN'MonitorAux to a functor *)
val get_port_socket_chain = 
    ref (fn ((pid:CPN'Id.id,i:int),([]:(CPN'Id.id*int) list)) => ([]:(CPN'Id.id * int) list))

fun nonUniqueNodeName [] = false 
  | nonUniqueNodeName [(nodeid,nodename,pageid)] = false
  | nonUniqueNodeName ((nodeid,nodename,pageid)::rest) = 
    List.exists (fn (t,n,p) => (n=nodename) andalso 
			       (p=pageid)) rest
    
fun nonUniqueNodeNames ([],errs) = errs
  | nonUniqueNodeNames (unchecked,errs) =
    if nonUniqueNodeName unchecked 
    then 
	let
	    val (nodeid,nodename,pageid) = hd unchecked
	    val (sameName,diffNames) = 
		List.partition (fn (nid,n,p) => (n=nodename) andalso 
						(p=pageid) andalso 
						(nid<>nodeid)) 
			       (tl unchecked)
	    val newerrs = 
		if sameName = []
		then errs
		else List.foldr 
			 (fn ((nid,n,p),errlist) => 
			     (nid,"Cannot monitor nodes with non-unique name ("^n^") on same page!")::errlist) 
			 errs ((nodeid,nodename,pageid)::sameName)
	in
	    nonUniqueNodeNames(diffNames,newerrs)
	end
    else nonUniqueNodeNames (tl unchecked,errs)
	 
fun problemPageNames ([],errs) = errs
  | problemPageNames ((pgid,"")::rest,errs) = 
    problemPageNames (rest,(pgid,"Page names must be non-empty")::errs)
  | problemPageNames ((pgid,name)::rest,errs) = 
    let
	val (sameName,unchecked) = 
	    List.partition (fn (id,n) => name=n andalso pgid<>id) rest
	val mlname = CPN'getMLidentifierPrefix name
	val newerrs = 
	    if sameName <> []
	    then List.foldl (fn ((id,n),e)=> 
				(id,"Page names ("^n^") must be unique")::e)
			    errs ((pgid,name)::sameName)
	    else 
		if mlname<>name
		then (pgid,"Error: The name of the page ("^name^
		       ") must start with a letter, and it can contain \
		       \only letters, numbers, underscores (_) and \
		       \apostrophes (').")::errs
		else errs
    in
	problemPageNames(unchecked,newerrs)
    end

fun nonUniquePlaces ([],checked,errs) = (checked,errs)
  | nonUniquePlaces ((pid,i,name,pgid)::rest,checked,errs) = 
    case CPN'PlaceTable.peek pid of 
	NONE => nonUniquePlaces(rest,checked,(pid,"Place not found!")::errs)
      | (SOME {kind=CPN'PlaceTable.fusion fgid,...}) => 
	let
	    val (samegroup,unchecked) = 
		List.partition 
		(fn (p,_,_,_) => 
		    case CPN'PlaceTable.peek p of
			(SOME{kind=CPN'PlaceTable.fusion n,...})=> fgid=n
		      | _ => false ) rest

	    val newerrs = if samegroup=[]
			  then []
			  else map (fn (p,_,_,_)=> (p,"Cannot monitor multiple places from same fusion group!")) ((pid,i,name,pgid)::samegroup)
	    val newchecked = if samegroup=[]
			     then (pid,name,pgid)::checked
			     else checked
	in
	    nonUniquePlaces (unchecked,newchecked,newerrs^^errs)
	end
      | (SOME {kind=CPN'PlaceTable.port _,...}) => 
	let
	    val pschain = (!get_port_socket_chain)((pid,i),[])
	    val (inchain,unchecked) = 
		List.partition 
		(fn (p,j,n,_) => List.exists 
				     (fn (plid,k) => p=plid andalso j=k) 
				     pschain)
		rest
	    val newerrs = if inchain=[]
			  then []
			  else map (fn (p,_,_,_)=> (p,"Cannot monitor multiple places in same port/socket chain!")) ((pid,i,name,pgid)::inchain)
	    val newchecked = if inchain=[]
			     then (pid,name,pgid)::checked
			     else checked
	in
	    nonUniquePlaces (unchecked,newchecked,newerrs^^errs)
	end
      | _ => nonUniquePlaces (rest,(pid,name,pgid)::checked,errs)


fun check_subnet (plinsts,trinsts) = 
    let
	val (trlist1,err1) = 
	    List.foldr 
		(fn ((tid,i),(ok,errs)) => 
		    case CPN'TransitionTable.peek tid of 
			NONE => (ok,(tid,"Transition not found!")::errs)
		      | SOME (CPN'TransitionTable.substitution _) => 
			(ok,(tid,"Cannot monitor substitution transitions!")::errs) 
		      | SOME (CPN'TransitionTable.transition {name="",...}) => 
			(ok,(tid,"Cannot monitor transition with empty ML name!")::errs) 
		      | SOME (CPN'TransitionTable.transition {name,page,...}) => 
			((tid,name,page)::ok,errs))
		([],[]) trinsts

	val err2 = nonUniqueNodeNames(trlist1,err1)

	val (pllist1,err3) = 	    
	    List.foldr 
		(fn ((pid,i),(ok,errs)) => 
		    case CPN'PlaceTable.peek pid of 
			NONE => (ok,(pid,"Place not found!")::errs)
		      | (SOME {ext={name="",...},...}) => 
			(ok,(pid,"Cannot monitor place with empty ML name!")::errs)
		      | (SOME {kind=CPN'PlaceTable.group,...}) => 
			(ok,(pid,"Cannot monitor fusion group!")::errs)
		      | (SOME {ext={name,page,...},...}) => 
			((pid,i,name,page)::ok,errs))  
		([],err2) plinsts

	val (pllist2,err3_5) = nonUniquePlaces(pllist1,[],err3)

	val err4 = nonUniqueNodeNames(pllist2,err3_5)

	val (okplpgs,err5) = 
	    foldr (fn ((plid,_,pgid),(okpgs,errs)) => 
		      case CPN'PageTable.peek pgid of
			  NONE => 
			  (okpgs,(CPN'PlaceTable.get_page plid, 
				  "Cannot find page for place!")::errs)
			| (SOME {page={name,...},...}) => 
			  ((pgid,name)::okpgs,errs)) ([],err4) pllist2


	val (okpgs,err6) = 
	    foldr (fn ((tid,_,pgid),(okpgs,errs)) => 
		      case CPN'PageTable.peek pgid of
			  NONE => 
			  (okpgs,(CPN'PlaceTable.get_page tid, 
				  "Cannot find page for transition!")::errs)
			| (SOME {page={name,...},...}) => 
			  ((pgid,name)::okpgs,errs)) (okplpgs,err5) trlist1

	val sortedpgs = 
	    Misc.unique_sort 
		(fn ((id1,name1),(id2,name2)) => CPN'Id.lt(id1,id2)) okpgs
	
	val err7 = problemPageNames(sortedpgs, err6)

    in
	err7
    end
		
end

