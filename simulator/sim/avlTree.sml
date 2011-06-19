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
(*
 * Module:	    AVL Tree (AVLT)
 *
 * Description:	    Polymorphic AVL Tree module. Provides storage
 *     		    for efficient lookup of equality types. 
 *
 * CPN Tools
 *)

structure CPN'AvlTree = 
 struct 
  local
   exception avl;
   exception avl_left;
   exception avl_right;
   exception avl_find;
   exception taller;
   exception shorter;

   datatype balancefactor = LH | EH | RH;
   datatype 'a node = node of {key:string,value:'a, 
                               left:('a node)ref,
                               right:('a node)ref,
                               bf:balancefactor ref}
                         | emptynode;
  in
   abstype 'a avltree = tree of ('a node) ref
   with

   exception ExcAvlInsert;
   exception ExcAvlLookup;
   exception ExcAvlDelete;


   fun AvlNew () = tree(ref emptynode);

   fun AvlIsEmpty (tree(ref emptynode)) = true
     | AvlIsEmpty _ = false;

   fun AvlReset (tree (root)) = root := emptynode;

   fun AvlSet (tree (root1),tree (root2)) = root1 := (!root2);

   local

    fun rotleft (root as ref(node{right=root_right as 
                                  ref(node{left=root_right_left,...}),...})) =
     let
      val temp = (ref emptynode);
     in
      (temp := !root_right; 
       root_right := !root_right_left;
       root_right_left := !root; 
       root := !temp)
     end
      | rotleft _ = ();

    fun rotright (root as ref(node{left=root_left as 
                                   ref(node{right=root_left_right,...}),...})) =
     let
      val temp = (ref emptynode);
     in
      (temp := !root_left; 
       root_left := !root_left_right;
       root_left_right := !root; 
       root := !temp)
     end
      | rotright _ = ();

    fun rightbalance (root as ref(node{bf=root_bf, right=root_right as
                                       ref(node{bf=root_right_bf,...}),...})) =
      (case (!root_right_bf) of
         RH => (root_bf := EH; root_right_bf := EH; rotleft root; true) |
         EH => (root_bf := RH; root_right_bf := LH; rotleft root; false) | 
         LH => (case root_right
                of ref(node{left=ref(node{bf=root_right_left_bf,...}),...}) =>
                  (case (!root_right_left_bf) of
                     EH => (root_bf := EH; root_right_bf := EH) |
                     RH => (root_bf := LH; root_right_bf := EH) |
                     LH => (root_bf := EH; root_right_bf := RH);
                  root_right_left_bf := EH;
                  rotright root_right;
                  rotleft root;
                  true)
                | _ => raise avl))
      |  rightbalance _ = false;

    fun leftbalance (root as ref(node{bf=root_bf, left=root_left
                                    as ref(node{bf=root_left_bf,...}),...})) =
      (case (!root_left_bf) of
         LH => (root_bf := EH; root_left_bf := EH; rotright root; true) |
         EH => (root_bf := LH; root_left_bf := RH; rotright root; false) | 
         RH => (case root_left
                of ref(node{right=ref(node{bf=root_left_right_bf,...}),...}) =>
                  (case (!root_left_right_bf) of
                     EH => (root_bf := EH; root_left_bf := EH) |
                     RH => (root_bf := EH; root_left_bf := LH) |
                     LH => (root_bf := RH; root_left_bf := EH);
                  root_left_right_bf := EH;
                  rotleft root_left;
                  rotright root;
                  true)
               | _ => raise avl))
      |  leftbalance _ = false;
   in
    fun AvlInsert (tree(noderef)) (CPN'k,CPN'v) = 
    let 
     fun insert (root as (ref emptynode))
        = (root := node {key=CPN'k, value=CPN'v, right = ref emptynode, 
                            left = ref emptynode, bf = ref EH};
           raise taller)
       | insert (root as (ref (node {key, left, right, bf,...})))
        = if (CPN'k = key) 
            then raise ExcAvlInsert
            else 
              if (CPN'k < key)
                then (insert(left) handle taller =>
                        (case !bf of
                           EH => (bf := LH; raise taller) |
                           LH => (leftbalance root;()) |
                           RH => bf := EH))
                else (insert(right) handle taller =>
                        (case !bf of 
                           EH => (bf := RH; raise taller) |
                           LH => bf := EH |
                           RH => (rightbalance root;()) ));
    in 
     insert noderef handle taller => ()
    end;


(* This functions inserts the element (k,v) into tree if there is no
   element with key "k". If there is such an element (k,w) it is
   exchanged by (k,v).
   If the tree gets taller the function returns true, else false.
 

    fun AvlWriteIn (tree(noderef)) (k,v) = 
    let 
     fun exchange (root as (ref (node{left, right ,bf,...}))) 
        = (root := node {key=k, value=v, right=right, left=left, bf=bf};
           false)
      |  exchange _ = 
            raise CPN'userexcp ("Severe error in internal table")

     fun insert (root as (ref emptynode))
        = (root := node {key=k, value=v, right = ref emptynode, 
                            left = ref emptynode, bf = ref EH};
           raise taller)
       | insert (root as (ref (node {key, left, right, bf,...})))
        = if (k = key) 
            then                 (* we have to exchange *)
              exchange (root)
            else                 (* we have to insert   *)
              if (k < key)
                then (insert(left) handle taller =>
                        (case !bf of
                           EH => (bf := LH; raise taller) |
                           LH => (leftbalance root;true) |
                           RH => (bf := EH;true)))
                else (insert(right) handle taller =>
                        (case !bf of 
                           EH => (bf := RH; raise taller) |
                           LH => (bf := EH;true) |
                           RH => (rightbalance root;true) ));
    in 
     insert noderef handle taller => true
    end; *)

	       fun AvlFold (func,start,tree(noderef)) =
		   let 
		       fun fold (ref emptynode, result)
			   = result 
			 | fold (ref (node {left, right, value,...}),result)
			   = fold(left,func (value, fold(right,result)))
		   in 
		       fold(noderef,start)
		   end;
		   
		   
    fun AvlApp (func,tree(noderef)) =
    let 
     fun app (ref emptynode)
        = () |
      app (ref (node {left, right, value,...}))
        = ((app left); (func value); (app right))
    in 
     app noderef
    end;

    fun AvlAppKey (func,tree(noderef)) =
    let 
     fun app (ref emptynode)
        = () |
      app (ref (node {key, left, right, value,...}))
        = ((app left); (func (key,value)); (app right))
    in 
     app noderef
    end;

    fun AvlLookup (tree(noderef),CPN'k) =
    let 
     fun lookup (ref emptynode)
        = raise ExcAvlLookup
       | lookup (ref (node {key, left, right, value,...}))
        = if (CPN'k = key) 
            then value
            else if (CPN'k < key) then lookup(left) else lookup(right);
    in 
     lookup noderef
    end;

local
fun rightmost (root as (ref (node{left,right as (ref emptynode),...})))
  = let val tnode = ref (!root)
    in
	(root := !left;
	 (true,tnode))
    end
  | rightmost (root as (ref (node{right,bf,...})))
    = let val (short,tnode) = rightmost right
      in
	  if short
	  then
    	      case !bf of
		  RH => (bf := EH; (true,tnode))
		| EH => (bf := LH; (false,tnode))
		| LH => (leftbalance root,tnode)
	  else
	      (false,tnode)
      end
  | rightmost _ = raise avl_right;
    

fun leftmost (root as (ref (node{right,left as (ref emptynode),...})))
   = let val tnode = ref (!root)
     in
      (root := !right;
       (true,tnode))
      end
  | leftmost (root as (ref (node{left,bf,...})))
   = let val (short,tnode) = leftmost left
     in
      if short
      then 
       case !bf of
	  LH => (bf := EH; (true,tnode))
        | EH => (bf := RH; (false,tnode))
        | RH => (rightbalance root,tnode)
      else
       (false,tnode)
     end
  | leftmost _ = raise avl_left;

fun find (root as (ref emptynode)) = raise avl_find
 |  find (root as (ref (node{left as (ref emptynode),right as (ref emptynode),...})))
        = (root := emptynode; raise shorter)
 | find (root as (ref (node{key=root_key,left=root_left,right=root_right,bf=root_bf,...}))) 
       = case !root_bf of
            LH => (case rightmost root_left of 
                     (short,(tnode as (ref (node{key,left,right,bf,...})))) =>
			 (root := !tnode;
 			  left := !root_left;
			  right := !root_right;
   			  if short
                          then       
                 (bf := EH; raise shorter)
                else
                 bf := LH)
              | _ => raise avl)
                  
     | RH  => (case leftmost root_right
               of (short,(tnode as (ref (node{key,left,right,bf,...})))) =>
                (root := !tnode;
                 left := !root_left;
                 right := !root_right;
                 if short
                 then
                  (bf := EH; raise shorter)
                 else
                  bf := RH)
               | _ => raise avl)
                 
     | EH  => (case leftmost root_right
               of (short,(tnode as (ref (node{key,left,right,bf,...})))) =>
                (root := !tnode;
                 left := !root_left;
                 right := !root_right;
                 if short
                 then
                  bf := LH
                 else
                  bf := EH)
               | _ => raise avl)
                 
in

    fun AvlDelete (tree(noderef)) CPN'k =
    let 
     fun delete (ref emptynode)
        = raise ExcAvlDelete
       | delete (root as (ref (node {key,left,right,bf,...})))
        = if (CPN'k = key) 
            then (find root)
            else
              if (CPN'k < key)
                then (delete(left) handle shorter =>
                        (case !bf of
                           EH =>  bf := RH |
                           LH => (bf := EH; raise shorter) |
                           RH => if rightbalance root
                                 then raise shorter else () ))
                else (delete(right) handle shorter =>
                        (case !bf of 
                           EH =>  bf := LH |
                           LH =>  if leftbalance root
                                  then raise shorter else () |
                           RH => (bf := EH; raise shorter)));
    in 
     delete noderef handle shorter => ()
    end

end;

    fun AvlNo tabel
      = let val no = ref 0
        in
         (AvlApp (fn _ => (no:= !no + 1),tabel); (!no))
        end;

    fun AvlCopy tabel
      = let val newtab = AvlNew()
        in
         (AvlAppKey(AvlInsert newtab,tabel); newtab)
        end;

    fun AvlAdd (tab1,tab2)
      = let val newtab = AvlCopy tab1
        in
         (AvlAppKey
           (fn elm => AvlInsert newtab elm handle ExcAvlInsert => (),
           tab2);
          newtab)
        end;

    fun AvlAnd (tab1,tab2)
      = let val newtab = AvlNew()
        in
         (AvlAppKey
           (fn (elm as (key,_)) =>
            (AvlLookup (tab2,key); AvlInsert newtab elm)
              handle ExcAvlLookup => (),
           tab1);
          newtab)
        end;

    fun AvlSub (tab1,tab2)
      = let val newtab = AvlNew()
        in
         (AvlAppKey
          (fn elm as (key,_) =>
            (AvlLookup(tab2,key);())
              handle ExcAvlLookup =>
               AvlInsert newtab elm,
          tab1);
         newtab)
        end;
   end; 
  end; 
 end; 
end; 

