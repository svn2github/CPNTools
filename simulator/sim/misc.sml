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
(* File: misc.sml
 *
 * Miscellaneous functions to be used both internaly and externaly.
 *)

infix 0 +=;
infix 0 -=;
infix 0 *=;
infix 0 ::=;

(* FIXME: Hack for not shadowing SML Time struct *)
structure SMLTime= Time;

(*@m@
 This module contains miscellaneous functions and structures useful for 
 various general purposes.
 *)
structure Misc = struct

    (*@o@
     This is a generic comparison function, given a specific comparison
     function and a tuple by the caller, it returns the order.
     @type[('a * 'a -> bool) -> 'a * 'a -> order]
     @exam[- a_cmp (fn (a,b) => a>b) (4,2);
           val it = LESS : order]
     @retn[@xref[order]]
     *)
    fun a_cmp lt (a,b) =
	if lt(a,b) then LESS
	else if lt(b,a) then GREATER
	else EQUAL

    (*@m@
     This module contains various general functions for integer related
     operations.
     *)
    structure I = struct

	(*@o@
	 Increment a referenced integer by something.
	 @infx[+=]
	 *)
	fun r += (x: int) = r:= !r + x
	(*@o@
	 Decrement a referenced integer by something.
	 @infx[-=]
	 *)
	fun r -= (x: int) = r:= !r - x
	(*@o@
	 Multiply a referenced integer by something.
	 @infx[*=]
	 *)
	fun r *= (x: int) = r:= !r * x

	(*@o@
	 Calculates f(m,f(m+1,...f(n-1,init)...)), where [m,n-1] is an
	 integer interval and init is a start value.
	 @exam[- fold (fn (x,y) => x::y) (3,10) nil;
               val it = [3,4,5,6,7,8,9] : int list]
	 @type[(int * 'a -> 'a) -> int * int -> 'a -> 'a]
	 *)
	fun fold f (m,n) tail = let
	    fun fold'(i,tail) = 
		if i<n then f(i,fold'(i+1,tail)) else tail
	in
	    fold'(m,tail)
	end

	(*@o@
	 Calculates f(n-1,f(n-2,...f(m,init)...)), where [m,n-1] is an
	 integer interval and init is a start value.
	 @exam[- revfold (fn (x,y) => x::y) (3,10) nil;
               val it = [9,8,7,6,5,4,3] : int list]
	 @type[(int * 'a -> 'a) -> int * int -> 'a -> 'a]
	 *)
	fun revfold f (m,n) tail = let
	    fun revfold'(i,tail) = 
		if m<=i then f(i,revfold'(i-1,tail)) else tail
	in
	    revfold'(n-1,tail)
	end

	(*@o@
	 Applies a given function to each integer in the interval [m,n-1]
	 in sequence.
	 @exam[- app (fn x=> print(Int.toString x)) (4,10);
               456789val it = () : unit]
	 @type[(int -> 'a) -> int * int -> unit]
	 *)
	fun app f (m,n) = let
	    fun app' i = if i<n then (f i; app'(i+1)) else ()
	in
	    app' m
	end

	(*@o@
	 Applies a given function to each integer in the interval [m,n-1]
	 in reverse sequence.
	 @exam[- revapp (fn x=> print(Int.toString x)) (4,10);
               987654val it = () : unit]
	 @type[(int -> 'a) -> int * int -> unit]
	 *)
	fun revapp f (m,n) = let
	    fun app' i = if m<=i then (f i; app'(i-1)) else ()
	in
	    app'(n-1)
	end
    end

    (*@m@
     This module contains various general functions for real (floating point)
     related operations.
     *)
    structure R = struct

	(*@o@
	 Increment a referenced real by something.
	 @infx[+=]
	 *)
	fun r += (x: real) = r:= !r + x
	(*@o@
	 Decrement a referenced real by something.
	 @infx[-=]
	 *)
	fun r -= (x: real) = r:= !r - x
	(*@o@
	 Multiply a referenced real by something.
	 @infx[*=]
	 *)
	fun r *= (x: real) = r:= !r * x
    end 

    (*@m@
     This module contains various general functions for string
     related operations.
     *)
    structure S = struct

	(*@o@
	 Returns the sub-string of the n leftmost elements in a string.
	 @type[string * int -> string]
	 @excn[@xref[substring]]
	 *)
	fun left (s,i) = substring(s,0,i)

	(*@o@
	 Returns the sub-string of the n rightmost elements in a string.
	 @type[string * int -> string]
	 @excn[@xref[substring]]
	 *)
	fun right (s,i) = let
	    val n = String.size s
	in
	    substring(s,n-i,i)
	end
    end

    (*@o@
     Returns the string representation of list where each element is
     calculated by a given function which is applied to each integer
     in a given interval.
     @exam[- makelist (fn x => Int.toString x) (3,9);
           val it = "[3,4,5,6,7,8]" : string]
     @type[(int -> string) -> int * int -> string]
     *)
    fun makelist mkstr list =
	concat ("["::tl(foldr (fn (a,b) => ","::mkstr a::b) ["","]"] list))

    (*@m@
     This module contains various general functions for array
     related operations.
     *)
    structure A = struct
	local
	    open Array
	in

	    (*@o@
	     Applies given function to each element in given array,
	     replacing each element with a new value.
	     @type[('a -> 'a) -> 'a array -> unit]
	     *)
	    fun map f a = let
		val n = length a
		fun map' i = 
		    if i<n then (update(a,i,f(sub(a,i))); map'(i+1)) else ()
	    in
		map' 0
	    end		

	    (*@o@
	     Calculates f(A[0],f(A[1],...f(A[length-1])...)).
	     @type[('a * 'b -> 'b) -> 'a array -> 'b -> 'b]
	     @exam[- val myarr = tabulate (10,fn x=>x);
                   val it = [|0,1,2,3,4,5,6,7,8,9|] : int array
                   - A.fold (fn (x,y) => x::y) myarr nil;
                   val it = [0,1,2,3,4,5,6,7,8,9] : int list]
	     *)
	    fun fold f a tail = let
		val n = length a
		fun fold'(i,tail) = 
		    if i<n then f(sub(a,i),fold'(i+1,tail)) else tail
	    in
		fold'(0,tail)
	    end		

	    (*@o@
	     Calculates f(A[length-1],f(A[length-2],...f(A[0])...)).
	     @type[('a * 'b -> 'b) -> 'a array -> 'b -> 'b]
	     @exam[- val myarr = tabulate (10,fn x=>x);
                   val it = [|0,1,2,3,4,5,6,7,8,9|] : int array
                   - A.fold (fn (x,y) => x::y) myarr nil;
                   val it = [9,8,7,6,5,4,3,2,1,0] : int list]
	     *)
	    fun revfold f a tail = let
		val n = length a
		fun revfold'(i,tail) = 
		    if 0<=i then f(sub(a,i),revfold'(i-1,tail)) else tail
	    in
		revfold'(n-1,tail)
	    end		

	    (*@o@
	     Applies given function to each element in given array,
	     but without modifying the array.
	     @exam[- val myarr = tabulate (10,fn x=>x);
                   val it = [|0,1,2,3,4,5,6,7,8,9|] : int array
                   - A.app (fn x => print(Int.toString x)) myarr;
                   0123456789val it = () : unit]
	     @type[('a -> 'b) -> 'a array -> unit]
	     *)
	    fun app f a = let
		val n = length a
		fun app' i = if i<n then (f(sub(a,i)); app'(i+1)) else ()
	    in
		app' 0
	    end		

	    (*@o@
	     Applies given function to each element in given array in reverse
	     order, but without modifying the array.
	     @exam[- val myarr = tabulate (10,fn x=>x);
                   val it = [|0,1,2,3,4,5,6,7,8,9|] : int array
                   - A.revapp (fn x => print(Int.toString x)) myarr;
                   9876543210val it = () : unit]
	     @type[('a -> 'b) -> 'a array -> unit]
	     *)
	    fun revapp f a = let
		fun revapp' 0 = ()
		  | revapp' i = (f(sub(a,i-1)); revapp'(i-1))
	    in
		revapp' (length a)
	    end		
	end
    end

    (*@o@
     Filters integers in a given interval according to a given filter function.
     @type[(int -> bool) -> int * int -> int list]
     @exam[- filter (fn x => x>10) (5,20);
           val it = [11,12,13,14,15,16,17,18,19] : int list]
     *)
    fun filter f l = foldr (fn (x,xs) => if (f x) then x::xs else xs) [] l

    (*@o@
     A list of list is unfolded to a list of all the elements.
     @type['a list list -> 'a list]
     @exam[- flatten [[1,2,3],[3],[7,4,3,1,4]];
           val it = [1,2,3,3,7,4,3,1,4] : int list]
     *)
    fun flatten l = List.concat l

    (*@o@
     Prepend an element to a referenced list.
     @infx[::=]
     *)
    fun r ::= x = r:= x::(!r)

(*FIXME: sort+unique_sort: use smlnj-lib/Util/list-mergesort.sml instead
 * but remember that lt must be changed to (not o lt) *)
    (*@o@
     Smooth sorting algorithm from [Pau91] page 100. It has linear
     execution time (n) if the input is nearly sorted, degenerating
     to the best possible in the worst case (n log n).
     @type[('a * 'a -> bool) -> 'a list -> 'a list]
     @exam[- sort (fn (x,y) => x>y) [1,7,43,2,7];
           val it = [43,7,7,2,1] : int list]
     *)
    fun sort lt list = let
	fun merge (x::xs,y::ys) =
	    if lt(x,y) then x::merge(xs,y::ys) else y::merge(x::xs,ys)
	  | merge (nil,ys) = ys
	  | merge (xs,nil) = xs

	fun mergepairs (l1::l2::ls, k) = 
	    if Int.mod(k,2) = 1 then l1::l2::ls
	    else mergepairs(merge(l1,l2)::ls, Int.div(k,2))
	  | mergepairs (ls,_) = ls

	fun nextrun (run, x::xs) =
	    if lt(hd run,x) then nextrun(x::run,xs) else (rev run,x::xs)
	  | nextrun (run, nil) = (rev run, nil)

	fun samsorting (x::xs, ls, k) = 
	    let
		val (run,tail) = nextrun (x::nil, xs)
	    in
		samsorting (tail, mergepairs (run::ls,k+1), k+1)
	    end
          | samsorting (nil, ls, _) = hd(mergepairs(ls,0)) handle _ => nil
    in
	samsorting (list, nil, 0)
    end

    (*@o@
     Sort a list of elements according to a given order function, and
     remove duplicates.
     @type[('a * 'a -> bool) -> 'a list -> 'a list]
     @exam[- unique_sort (fn (x,y) => x>y) [1,7,43,2,7];
           val it = [43,7,2,1] : int list]
     *)
    fun unique_sort lt list = let
	fun merge (x::xs,y::ys) =
	    if lt(x,y) then x::merge(xs,y::ys) 
	    else if lt(y,x) then y::merge(x::xs,ys)
	    else x::merge(xs,ys)
	  | merge (nil,ys) = ys
	  | merge (xs,nil) = xs

	fun mergepairs (l1::l2::ls, k) = 
	    if Int.mod(k,2) = 1 then l1::l2::ls
	    else mergepairs(merge(l1,l2)::ls, Int.div(k,2))
	  | mergepairs (ls,_) = ls

	fun nextrun (run, x::xs) =
	    if lt(hd run,x) then nextrun(x::run,xs) else (rev run,x::xs)
	  | nextrun (run, nil) = (rev run, nil)

	fun samsorting (x::xs, ls, k) = 
	    let
		val (run,tail) = nextrun (x::nil, xs)
	    in
		samsorting (tail, mergepairs (run::ls,k+1), k+1)
	    end
	  | samsorting (nil, ls, _) = hd(mergepairs(ls,0)) handle _ => nil
    in
	samsorting (list, nil, 0)
    end

    (*@o@
     Return seconds and milliseconds since Jan. 1st 1970.
     @type[unit -> real]
     @exam[- time_of_day();
           val it = 947782183.068 : real]
     *)
    fun time_of_day () = Time.toReal (Time.now())

    (*@o@
     Corresponds to UNIX basename(1) command.
     @type[string -> string]
     @exam[- basename "/var/tmp/test";
           val it = "test" : string]
     *)
    fun basename xs = let
	fun get (#"/"::nil,ys) = rev ys
	  | get (nil,ys) = rev ys
	  | get (#"/"::xs,_) = get(xs,nil)
	  | get (x::xs,ys) = get(xs,x::ys)
    in
	implode(get(explode xs,nil))
    end

    fun isSubstring "" "" = true
      | isSubstring sstr "" = false
      | isSubstring sstr str = 
	(String.isPrefix sstr str) orelse 
	(isSubstring sstr (String.extract (str,1,NONE)))
end

(*@m@
 This module contains miscellaneous functions and structures useful for 
 various general purposes. It contains the @xref[Misc] module.
 *)
structure CPN'Misc = struct
    open Misc

    local
	val no = ref 0;
    in
	(*@o@
	 This function is used for generating a sequence of integers
	 starting from zero. The next "available" integer is returned
	 upon each call.
	 *)
	fun next() = !no before inc no;
    end

    exception CPN'UnknownVariable of string (* varname *)

fun filter_var_binding ((varstr,valstr),[])  = 
    raise CPN'UnknownVariable varstr
  | filter_var_binding ((varstr,valstr),
			 ((groupvars,groupbindingslist)::rest))
    = 
    if not (ListUtils.mem groupvars varstr)
    then (groupvars,groupbindingslist)::
	 (filter_var_binding ((varstr,valstr),rest))
    else
	let
	    val n = ListUtils.pos varstr groupvars
		    handle Match => raise InternalError "error getting position in filter_var_binding"
	    val filteredbindings = 
		List.filter 
		    (fn groupbinding => 
			List.nth(groupbinding,n)=valstr) groupbindingslist
	in
	    if filteredbindings = []
	    then raise BindFatalFailure
	    else (groupvars,filteredbindings)::rest
	end

fun get_binding_pos (desiredbindings,enabledbindings) = 
    let
	val _ = if length desiredbindings <>
		   length enabledbindings
		then raise InternalError "length mismatch in get_binding_pos"
		else ()

	fun pick_one ((gvs,desiredgroupbindings),
		     (groupvars,enabledgroupbindings)) = 
	    if gvs<>groupvars
	    then raise InternalError "group var mismatch in get_binding_pos"
	    else let
		    (* pick a random binding among the desired
		     * bindings for the variables in groupvars *)
		    val n = CPN'Random.int(List.length desiredgroupbindings)
		    val pickedbinding = List.nth(desiredgroupbindings,n)
		in
		    ListUtils.pos pickedbinding enabledgroupbindings
		    handle Match => raise InternalError "error getting position in get_binding_pos"

		end
    in
	map pick_one (ListPair.zip (desiredbindings,enabledbindings))
    end

end

(*@[Misc]@
 @note[This module is opened in the top-level environment.]
 *)
open Misc;

(* _overload += : ('a ref * 'a -> unit) as Misc.I.+= and Misc.R.+=; *)
(* _overload -= : ('a ref * 'a -> unit) as Misc.I.-= and Misc.R.-=; *)
(* _overload *= : ('a ref * 'a -> unit) as Misc.I.*= and Misc.R.*=; *)
