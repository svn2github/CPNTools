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
 * File: list_utils.sml
 *
 * Basic list utilities
 *)

structure ListUtils =
struct

fun mem [] a = false
  | mem (hd::tl) a = a=hd orelse mem tl a

fun remdupl [] = []
  | remdupl [hd] = [hd]
  | remdupl (hd::tl) = if mem tl hd
		       then remdupl tl
		       else hd::(remdupl tl)

fun rm a [] = []
  | rm a (hd::tl) = if a=hd
		    then tl
		    else hd::(rm a tl)
    
fun rmall a [] = []
  | rmall a (hd::tl) = if a=hd
		       then rmall a tl
		       else hd::(rmall a tl)

fun pos a [] = raise Match
  | pos a l = 
    let
	fun get_pos (n,[],a) = raise Match
	  | get_pos (n,x::xs,a) = 
	    if a=x
	    then n
	    else get_pos(n+1,xs,a)
    in
	get_pos(0,l,a)
    end

(* returns true if each element in the 
 * second list is contianed in the first list
 * ignores multiplicity of elements in second list *)
fun contains _ [] = true
  | contains [] (x::xs) = false
  | contains ys (x::xs) = (mem ys x) andalso (contains ys xs)

(* similar to contains, but does not ignore multiplicity
 * of elements in second list *)
fun contains_all _ [] = true
  | contains_all [] (x::xs) = false
  | contains_all ys (x::xs) = 
    (mem ys x) andalso (contains_all (rm x ys) xs)

fun intersect [] ys = []
  | intersect xs [] = []
  | intersect (x::xs) ys = 
    if mem ys x
    then x::(intersect xs (rm x ys))
    else intersect xs ys

fun union l1 l2 = List.@(l1,l2)

fun listsub xs [] = xs
  | listsub [] _  = raise Subtract
  | listsub xs (y::ys) = 
    if mem xs y
    then listsub (rm y xs) ys
    else raise Subtract

fun ins l e =  List.@(l,[e])

fun ins_new l e = 
    if mem l e
    then l 
    else List.@(l,[e])

fun fmap f l = List.concat(map f l)
 
fun mapfilter (_,nil) = nil
   | mapfilter(predprocfun,hd::tl) = 
     let
	 val (pred,proc) = !predprocfun hd
     in
	 if pred
	 then proc::(mapfilter(predprocfun,tl))
	 else mapfilter(predprocfun,tl)
     end
     
fun predlist _ nil = false
  | predlist predfun (hd::tl) = if predfun hd
				then true
				else predlist predfun tl
      
fun split (0,rest,front) = (rest, rev front)
  | split (n,hd::tl,front) = split(n-1, tl, hd::front)
      
local
    fun merge ([],ls) = ls
      | merge (ls,[]) = ls
      | merge (ls1 as ((node1:int)::rest1),ls2 as (node2::rest2)) = 
	if node1 <= node2
	then node1::merge(rest1,ls2)
	else node2::merge(ls1,rest2)

    fun mergepairs ([ls],_) = [ls]
      | mergepairs (lst as (l1::l2::ls),k) =
	if k mod 2 = 1
	then lst
	else mergepairs (merge (l1,l2)::ls, k div 2)

    fun nextrun (run,[]) = (rev run,[])
      | nextrun (run, ls as ((node:int)::rest)) =
	if node < hd run
	then (rev run, ls)
	else nextrun (node::run, rest)

    fun samsorting ([], ls, _) = hd (mergepairs (ls, 0))
      | samsorting (node::rest, ls, k) =
	let
	    val (run,tail) = nextrun ([node],rest)
	in
	    samsorting (tail, mergepairs (run::ls,k+1), k+1)
	end
in
fun sort_int_list (ls:int list) = samsorting (ls, [], 0)
end

end (*struct*);

open ListUtils;
