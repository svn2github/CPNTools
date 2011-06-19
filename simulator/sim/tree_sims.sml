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
(* File: tree_sims.sml
 *
 * Secondary internal multi-set implementation: This is a tree implementation
 * of internal multi-sets.
 *)

functor CPN'MakeTreeSIMS(structure Time: CPN'TIME
			 and pims: CPN'PRIMARY_IMS;
			 val time: pims.cs -> Time.time): CPN'SECONDARY_IMS = struct
    structure Time = Time;
    structure pims = pims;

    type 'a ems = (int * 'a) list;
	
    type cs = pims.cs
	
    val time = time

    local
	open CPN'Tree CPN'Misc

	fun cmp (a,b) = let
	    val ta = time a
	    val tb = time b
	in
	    if Time.lt(ta,tb) then LESS
	    else if Time.lt(tb,ta) then GREATER
	    else pims.cmp(a,b) 
	end

	val tsize = CPN'Tree.size

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
			
	fun tdelete (TreeNil, col) = raise Subtract
	  | tdelete (tree as TreeNode{value,left,right,...}, col) =
	    (case cmp(value,col) of
		 LESS => balance(value, left, tdelete(right,col))
	       | GREATER => balance(value, tdelete(left,col), right)
	       | EQUAL => join(left,right))
    in
	type 'a ims = 'a tree

	fun empty () = (TreeNil: cs ims)

	fun is_empty (TreeNil: cs ims) = true
	  | is_empty _ = false

	val size = CPN'Tree.size

	val fold = CPN'Tree.fold

	fun insert (ims as ref(tree: cs ims), col) = 
	    ims:= tinsert(tree,col);

	fun addto (ims, ems) = List.app (fn x => insert(ims,x)) ems
                    
	fun init (ims,ems) = (ims:= empty(); addto (ims,ems))
	    
	fun delete (ims as ref(tree: cs ims), col) =
	    ims:= tdelete(tree,col);

	fun subfrom (ims, ems) = List.app (fn x => delete(ims,x)) ems

	fun extract (tree: cs ims) = let 
	    
	    fun extract' (TreeNil,l) = l
	      | extract' (TreeNode{value,left,right,...},l) =
		extract' (left,value::(extract' (right,l)));
	in
	    (tsize tree, extract'(tree,nil))
	end

	fun deletemin (ims as ref(tree: cs ims)) = let
	    val (value, tree') = delmin tree
	in
	    (ims:= tree'; value)
	end handle EmptyTree => raise Empty

	fun deleteto (ref(TreeNil),_) = nil
	  | deleteto (ims as ref(tree: cs ims), until) =
	    if Time.leq(time(min tree), until) then
		deletemin(ims)::deleteto(ims,until)
	    else nil

	fun deleteall (ims: cs ims ref) =
	    (fold (op ::) (!ims) nil) before (ims:= empty())

	fun min (TreeNil) = NONE
	  | min (tree: cs ims) = SOME(time(CPN'Tree.min tree)) 
    end
end
