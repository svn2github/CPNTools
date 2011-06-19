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
(* File: treelist_pims.sml
 *
 * Primary Internal Multi-Set Implementation: This is a BB-tree implementation
 * of internal multi-sets. Together with the tree there is a list holding 
 * the reserved elements.
 *)

functor CPN'MakeTreeListPIMS (structure cs: COLORSET; 
		      val cmp: cs.cs * cs.cs -> order): CPN'PRIMARY_IMS = 
struct
    structure cs = cs;

    open Misc;

    type cs = cs.cs;

    val cmp = cmp;

    open CPN'Tree;
	
    val tsize = CPN'Tree.size
    val tfold = CPN'Tree.fold

    fun tinsert (TreeNil, col) = singleton col
      | tinsert (TreeNode{value,left,right,...}, col) =
	case cmp(value,col) of
	    LESS => balance(value, left, tinsert(right,col))
	  | GREATER => balance(value, tinsert(left,col), right)
	  | EQUAL => 
		if tsize left < tsize right then
		    balance(value, tinsert(left,col), right)
		else
		    balance(value, left, tinsert(right,col));

    fun taddto (tree, nil) = tree
      | taddto (tree, col::rms) = taddto (tinsert (tree,col), rms) 

    fun tdelete (TreeNil, col) = raise Subtract
      | tdelete (tree as TreeNode{value,left,right,...}, col) =
	(case cmp(value,col) of
	     LESS => balance(value, left, tdelete(right,col))
	   | GREATER => balance(value, tdelete(left,col), right)
	   | EQUAL => join(left,right))

    fun tdelete_all (TreeNil,col) = TreeNil
      | tdelete_all (tree,col) = let
	    fun delete (TreeNil, col) = TreeNil
	      | delete (tree as TreeNode{value,left,right,...}, col) =
		(case cmp(value,col) of
		     LESS => balance(value, left, delete(right,col))
		   | GREATER => balance(value, delete(left,col), right)
		   | EQUAL => join(left,right))
	    val tree' = delete (tree,col)
	in
	    if tsize tree = tsize tree' then tree
	    else tdelete_all (tree',col)
	end
	

    fun tsubfrom (tree, nil) = tree
      | tsubfrom (tree, col::rms) = tsubfrom (tdelete (tree,col), rms);

    (* Public functions *)

    type 'a ims = {tree: 'a tree, list: 'a list}

    fun empty () = {tree=TreeNil, list= nil}

    fun is_empty {tree=TreeNil,list=nil} = true
      | is_empty _ = false

    fun fold f {tree,list} base = tfold f (taddto(tree,list)) base

    fun size {tree,list} = tsize tree + length list

    fun insert (ims as ref({tree,list}: cs ims), col) =
	ims:= {tree=tinsert(tree,col),list=list};

    fun addto (ims, ems) = List.app (fn x => insert(ims,x)) ems

    fun init(ims,ems) = (ims:= empty(); addto(ims,ems))

    fun delete (ims as ref({tree,list=nil}: cs ims), col) =
	ims:={tree=tdelete(tree,col),list=nil}
      | delete (ims as ref({tree,list as value::res}: cs ims), col) =
	(case cmp(col,value) of
	     EQUAL => ims:= {tree=tree,list=res}
	   | _ => ims:= {tree=tdelete(taddto(tree,list),col),list=nil});

    fun subfrom (ims, ems) = List.app (fn x => delete(ims,x)) ems

    local
	fun tmember (TreeNil,_) = false
	  | tmember (TreeNode{value,left,right,...},col) = 
	    case cmp(value,col) of
		LESS => tmember (right,col)
	      | GREATER => tmember (left,col)
	      | EQUAL => true

	fun lmember (nil,_) = false
	  | lmember (value::list,col) =
	    case cmp(value,col) of
		EQUAL => true
	      | _ => lmember (list,col)
    in
	fun member({tree,list},col) =
	    tmember(tree,col) orelse lmember(list,col)
    end

    local
	fun tcf (TreeNil,_) = 0
	  | tcf (TreeNode{value,left,right,...},col) = 
	    case cmp(value,col) of
		LESS => tcf (right,col)
	      | GREATER => tcf (left,col)
	      | EQUAL => 1 + tcf(left,col) + tcf(right,col)

	fun lcf (nil,_) = 0
	  | lcf (value::list,col) =
	    case cmp(value,col) of
		EQUAL => 1 + lcf(list,col)
	      | _ => lcf (list,col)
    in
	fun cf({tree,list},col) = tcf(tree,col) + lcf(list,col)
    end

    fun subset (ims: cs ims, ems: cs CPN'MS.ms) =
	if CPN'MS.size ems <= size ims then
	    let
		fun subset' (coef,col,x::xs) =
		    if cs.lt (col,x) then
			coef<=cf(ims,col) andalso subset'(1,x,xs)
		    else 
			subset'(coef+1,col,xs)
		  | subset' (coef,col,nil) = coef<=cf(ims,col)
	    in
		case Misc.sort cs.lt ems of
		    (x::xs) => subset'(1,x,xs)
		  | nil => true
	    end
	else false

    fun filter func ({tree,list}: cs ims) = let
	fun filter' (TreeNil, list') = list'
	  | filter' (TreeNode{value,left,right,...}, list') =
	    if func value then
		filter' (left, value::(filter' (right, list')))
	    else
		filter' (left, (filter' (right, list')));
    in
	filter'(taddto(tree,list), nil)
    end

    fun collect cmp' ({tree,list}: cs ims) = let
	fun collect' (TreeNil, l) = l
	  | collect' (TreeNode{value,left,right,...}, l) =
	    case cmp' value of
		LESS => collect' (right, l)
	      | GREATER => collect' (left, l)
	      | EQUAL => collect' (left, value::(collect' (right,l)));
    in
	collect'(taddto(tree,list), nil)
    end

    fun extract ({tree,list}: cs ims) = let 
	val tree' = taddto(tree,list)
    in
	(tsize tree', tfold (op ::) tree' nil)
    end

    fun init_res (ims as ref {tree,list}) = let
	val tree' = taddto(tree,list)
    in
	(ims:= {tree=tree',list=nil}; tsize tree')
    end

(*
    fun random exn ({tree=TreeNil,...}: cs ims) = raise exn
      | random exn {tree,...} = let
	fun get (TreeNode{value,left,right,...}, i) = 
	    (case Int.compare(i,tsize left) of
		 LESS => get (left,i)
	       | EQUAL => value
	       | GREATER => get (right, i-(tsize left)-1))
	  | get _ = raise exn
    in
	get (tree, CPN'Random.int (tsize tree))
    end  
*)

    (* next is optimized. When prev=NONE, only one instance of a color
     * is removed from ims. If prev=(SOME col) then col, was returned 
     * previously and all instances of col must be removed from ims 
     * before removing a new, random color from ims *)
    fun next exn ({tree=TreeNil,...}) _ = raise exn
      | next exn (ims as ({tree,...}: cs ims)) prev = 
	case prev of SOME col => next exn {tree=(tdelete_all(tree,col)),list=nil} NONE
		   | NONE => 
		     let
			 fun get (TreeNode{value,left,right,...}, i) = 
			     (case Int.compare(i,tsize left) of
				  LESS => 
				  let
				      val (left',col') = get(left,i) 
				  in
				      (balance(value, left', right), col')
				  end
				| GREATER => 
				  let
				      val (right',col') = get(right,i-(tsize left)-1) 
				  in
				      (balance(value, left, right'), col')
				  end
				| EQUAL => (join(left,right), value))
			   | get _ = raise InternalError "next"
					   
			 val draw = CPN'Random.int(tsize tree) 
			 val (tree',col) = get (tree,draw)
		     in
			 (col,{tree=tree',list=nil})
		     end
			 
    fun random_res exn (ref {tree=TreeNil,...}) = raise exn
      | random_res _ (ims as ref({tree,list}: cs ims)) = let
	fun get (TreeNode{value,left,right,...}, i) = 
	    (case Int.compare(i,tsize left) of
		 LESS => 
		     let
			 val (left',col') = get(left,i) 
		     in
			 (balance(value, left', right), col')
		     end
	       | GREATER => 
		     let
			 val (right',col') = get(right,i-(tsize left)-1) 
		     in
			 (balance(value, left, right'), col')
		     end
	       | EQUAL => (join(left,right), value))
	  | get _ = raise InternalError "random_res"

	val draw = CPN'Random.int(tsize tree) 
	val (tree',col) = get (tree,draw)
    in
	(ims:= {tree=tree',list=col::list}; col)
    end

    fun ms_random_res _ exn (ref({tree=TreeNil,list=nil}: cs ims)) =
	raise exn
      | ms_random_res (reslist, cur, no) exn (ims as ref{tree,list}) = let
        (* we do not need to use the reslist, because this implementation 
	 * already uses a list *)
	val reserved = length list
	val notreserved = tsize tree
	val index = (!cur + no - 1) mod (reserved+notreserved)
    in
	if !cur = reserved + notreserved then
	    raise exn
	else if index >= reserved then
	    random_res exn ims before inc cur
	else
	    List.nth (list, index) before inc cur
    end

    fun check_res exn f ims = let
	val col = random_res exn ims
    in
	if f col then col else check_res exn f ims
    end

    fun res_col (ims as ref({tree,list}: cs ims), col) = 
	let
	    val unreserved = {tree=tree,list=nil}
	    val col_ms = (filter (fn c => cmp(c,col)=EQUAL) unreserved)
	    val remaining = tsubfrom (tree,col_ms)
	in
	    ims := {tree=remaining,list=col_ms^^list}
	end
	
end (* functor *)
