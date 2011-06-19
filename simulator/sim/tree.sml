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
(* File: tree.sml
 *
 * Basic BB-Tree functionallity to be used in various implementations.
 *)

(* The following invariants are observed:
 *
 * I1: size(TreeNode{size,l,r,...}) = 1 + size l + size r = n
 *
 * I2: 1/ratio <= size l / size r <= ratio
 *)

structure CPN'Tree = struct 

    datatype 'a tree = 
	TreeNil
      | TreeNode of {value: 'a, 
		     size: int, 
		     left: 'a tree, 
		     right: 'a tree}
	
    exception EmptyTree;

    fun size (TreeNil) = 0
      | size (TreeNode{size,...}) = size

    fun lson (TreeNode{left,...}) = left
      | lson (TreeNil) = raise EmptyTree;

    fun rson (TreeNode{right,...}) = right
      | rson (TreeNil) = raise EmptyTree;

    fun sons (TreeNode{left,right,...}) = (left,right)
      | sons _ = raise EmptyTree;
    
    (* The function new is added to be used as a smart TreeNode 
     * constructor, to ensure that the invariant I1 is not violated
     *)
    fun new(v,l,r) = 
	TreeNode{value=v,
		 size= 1 + (size l) + (size r),
		 left= l,
		 right= r}

    (* The following two functions makes a single counter clock wise 
     * rotation from t1 to t2 or a clock wise from t2 to t1
     * t1.  TreeNode(x,_,s1,TreeNode(y,_,s2,s3))
     * t2.  TreeNode(y,_,TreeNode(x,_,s1,s2),s3)
     *)
    fun single_rotate_ccw (x, s1, TreeNode{value=y,left=s2,right=s3,...}) =
	new(y, new(x,s1,s2), s3)
      | single_rotate_ccw _ = raise Match;
	    
    fun single_rotate_cw (y, TreeNode{value=x,left=s1,right=s2,...}, s3) =
	new(x, s1, new(y,s2,s3))
      | single_rotate_cw _ = raise Match;

    (* The following two functions makes a double counter clock wise 
     * rotation from t1 to t2 or a clock wise from t2 to t1
     * t1.  TreeNode(x,_,s1,TreeNode(z,_,TreeNode(y,_,s2,s3),s4))
     * t2.  TreeNode(y,_,TreeNode(x,_,s1,s2),TreeNode(z,_,s3,s4))
     *)
    fun double_rotate_ccw (x, s1, TreeNode{value=z,left=TreeNode{value=y,left=s2,right=s3,...},right=s4,...}) =
	new(y, new(x,s1,s2), new(z,s3,s4))
      | double_rotate_ccw _ = raise Match;
	
    fun double_rotate_cw (z, TreeNode{value=x,left=s1,right=TreeNode{value=y,left=s2,right=s3,...},...},s4) =
	new(y, new(x,s1,s2), new(z,s3,s4))
      | double_rotate_cw _ = raise Match;
	
    (* The value ratio is the weight ration, which tells that one subtree
     * should not have more than ratio times the number of elements in
     * the other subtree
     *)
    val ratio = 5;

    fun singleton item = TreeNode{value=item, 
				  size=1, 
				  left=TreeNil, 
				  right=TreeNil}
	    
    (* The function balance is added to be used as a smart TreeNode 
     * constructor, to ensure that the invariant I2 is not violated
     *)
    fun balance (p as (v,l,r)) =
	let
	    val ln = size l;
	    val rn = size r;
	in
	    if ln+rn < 2 then
		new p
	    else if rn > ratio * ln then
		let
		    val (rl,rr) = sons r;
		in
		    if size rl < size rr then 
			single_rotate_ccw p
		    else
			double_rotate_ccw p
		end
	    else if ln > ratio * rn then
		let
		    val (ll,lr) = sons l;
		in
		    if size lr < size ll then 
			single_rotate_cw p
		    else
			double_rotate_cw p
		end
	    else
		new p
	end;

    fun min (TreeNode{value,left=TreeNil,...}) = value
      | min (TreeNode{left,...}) = min left
      | min (TreeNil) = raise EmptyTree;
	
    fun max (TreeNode{value,right=TreeNil,...}) = value
      | max (TreeNode{right,...}) = max right
      | max (TreeNil) = raise EmptyTree;
	
    fun delmin (TreeNode{value,left=TreeNil,right,...}) = (value,right)
      | delmin (TreeNode{value,left,right,...}) = 
	let
	    val (value',left') = delmin left
	in
	    (value',balance(value,left',right))
	end
      | delmin _ = raise EmptyTree;
	
    fun delmax (TreeNode{value,left,right=TreeNil,...}) = (value,left)
      | delmax (TreeNode{value,left,right,...}) =
	let
	    val (value',right') = delmax right
	in
	    (value',balance(value,left,right'))
	end
      | delmax _ = raise EmptyTree;
	    
    fun join (TreeNil,right) = right
      | join (left,TreeNil) = left
      | join (left,right) = 
	if size left < size right then
	    let val (v,r) = delmin right in balance(v,left,r) end 
	else
	    let val (v,l) = delmax left in balance(v,l,right) end

    fun fold _ TreeNil base = base
      | fold f (TreeNode{value,left,right,...}) base =
	fold f left (f (value, fold f right base));

    fun revfold _ TreeNil base = base
      | revfold f (TreeNode{value,left,right,...}) base =
	revfold f right (f (value, revfold f left base));

    fun app _ TreeNil = ()
      | app f (TreeNode{value,left,right,...}) =
	(app f left; f value; app f right)

    fun revapp _ TreeNil = ()
      | revapp f (TreeNode{value,left,right,...}) =
	(revapp f right; f value; revapp f left)

end;
