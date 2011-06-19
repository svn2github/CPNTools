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
 *  File:
 *     part-function.sml
 *
 *  Created:
 *     Jan. 10, 2009
 *
 *  Description:
 *     Partition functions used by the part external algorithm.  Also
 *  implements compositionaal hash function and the heuristics presented in
 *  paper:
 *     "Dynamic State Space Partitioning for External and Distributed Model
 *     Checking.
 *     In FMICS'2009.  Sami Evangelista and Lars Michael Kristensen."
 *  
 *)


structure CompositeHashFunction: sig

exception Path

exception Hash
	      
type ('a, 'b) comp_hash_function
	 
type id = int
	      
type path
	 
val empty:
    'b
    -> ('a, 'b) comp_hash_function

val hash:
    ('a, 'b) comp_hash_function
    -> 'a
    -> id
					     
val expand:
    ('a, 'b) comp_hash_function * path * ('a -> int) * 'b * (int * 'b) list
    -> ('a, 'b) comp_hash_function * (id * path) list
		     
val getPath:
    ('a, 'b) comp_hash_function * id
    -> path * 'b list * 'b
   
val print:
    ('b -> string)
    -> ('b -> string)
    -> ('a, 'b) comp_hash_function
    -> unit

val leaves:
    ('a, 'b) comp_hash_function
    -> int

end = struct

exception Hash

exception Path

type id = int

type path = int list

datatype ('a, 'b) node =
	 LEAF of int * 'b
       | FUNCTION of ('a -> int) * 'b * (int * ('a, 'b) node) list

type ('a, 'b) comp_hash_function = int * ('a, 'b) node

fun empty label = (1, LEAF (0, label))

fun hash (_, tree) value = let
    fun traverse (LEAF (i, _)) _ = i
      | traverse (FUNCTION (f, _, children)) value = let
	    val i = f value
	    val child = List.find (fn (j, _) => j = i) children
	in
	    case child
	     of NONE => raise Hash
	      | SOME (_, child) => traverse child value
	end
in
    traverse tree value
end

fun expand ((leaves, tree), path, f, label, children)  = let
    val n = ref 0
    val new = ref []
    fun traverse (LEAF (i, _), []) = let
	fun createChild (c, l) =
	    (c, if c = #1 (List.hd children)
		then (new := (i, path @ [ c ]) :: (!new);
		      LEAF (i, l))
		else (new := (!n + leaves, path @ [ c ]) :: (!new);
		      LEAF (!n + leaves, l))
		     before n := !n + 1)
	val children = List.map createChild children
    in	
	FUNCTION (f, label, children)
    end
      | traverse ((FUNCTION (g, label, children)), (i :: l)) = let
	    fun traverseChild (j, child) =
		(j, if i = j
		    then traverse (child, l)
		    else child)
	    val children = List.map traverseChild children
	in
	    FUNCTION (g, label, children)
	end
      | traverse _ = raise Path
    val tree = traverse (tree, path)
    val result = (!n + leaves, tree)
in
    (result, !new)
end

fun getPath ((_, tree), i) = let
    fun traverse (LEAF (j, l), (path, labels)) = if i = j
						 then SOME (path, labels, l)
						 else NONE
      | traverse (FUNCTION (f, label, children), (path, labels)) =
	List.foldl (fn ((j, c), NONE) =>
		       traverse (c, (j :: path, label :: labels))
		     | (_, result) => result)
		   NONE children
in
    case traverse (tree, ([], []))
     of NONE => raise Path
      | SOME (path, labels, last) => (List.rev path, List.rev labels, last)
end
				  
fun print funcLabelToString leafLabelToString (leaves, tree) = let
	fun printTabs tabs = let
	    fun loop 0 = ()
	      | loop n = (TextIO.print "\t"; loop (n - 1))
	in loop tabs end
	fun traverse (LEAF (i, label)) tabs =
	    TextIO.print ("PART " ^ leafLabelToString label ^ " (ID = " ^ 
			  Int.toString i ^ ")\n")
	  | traverse (FUNCTION (_, label, children)) tabs = (
	    TextIO.print ("FUNCTION " ^ funcLabelToString label ^ "\n");
	    List.app (fn (i, c) => (
			 printTabs (tabs + 1);
			 TextIO.print (Int.toString i);
			 TextIO.print " => ";
			 traverse c (tabs + 1))) children)
    in
	TextIO.print (Int.toString leaves ^ " leaves\n");
	traverse tree 0
    end

fun leaves (leaves, _) = leaves
	 
end


functor FunctionCommon(structure Model: MODEL) = struct

exception Reorganization

type state = Model.state

type event = Model.event

type part_id = int

type bit_vector = Word8Vector.vector

val parts = ref 1

fun numPartitions () = !parts

fun eventHook _ = ()

end


functor CompFunctionCommon(
structure Model       : MODEL
structure StateEncoder: SERIALIZER
where type src = Model.state) = struct

structure FunctionCommon = FunctionCommon(structure Model = Model)
open FunctionCommon

val dynamic = true

val func: (Model.state, int) CompositeHashFunction.comp_hash_function ref =
    ref (CompositeHashFunction.empty 0)

val dontCare = ~ 1

val degree = 10

val degreeW = Word.fromInt degree

val children = List.tabulate (degree, fn i => (i, i))

fun init () = (parts := 1;
	       func := CompositeHashFunction.empty 0)

fun part s = let
    val v = StateEncoder.map s
in
    (v, CompositeHashFunction.hash (!func) s)
end

fun part' v = CompositeHashFunction.hash (!func) (StateEncoder.unmap v)

fun updateFunc (path, l) = let
    fun loop (func, [], _, SOME id, ids) = (func, id :: ids)
      | loop (_, [], _, NONE, _) = raise LibBase.Impossible ""
      | loop (func, (h, label) :: tl, path, _, ids) = let
	    fun hash s = Word.toInt (Word.mod (h s, degreeW))
	    val (func, new) = CompositeHashFunction.expand
				  (func, path, hash, label, children)
	in
	    List.foldl (fn ((id, path), (func, ids)) =>
			   loop (func, tl, path, SOME id, ids))
		       (func, ids) new
	end
    val (func', new) = loop (!func, l, path, NONE, [])
in
    parts := !parts + (List.length new - 1);
    func := func';
    new
end

fun canReorganize _ = true

end


(*
 *  static/dynamic hash function based on the whole state vector
 *)
functor HashFunction(
val dynamic           : bool
val initParts         : int option
structure Model       : MODEL
structure StateEncoder: SERIALIZER
where type src = Model.state): PART_FUNCTION = struct

structure FunctionCommon = FunctionCommon(structure Model = Model)
open FunctionCommon

val dynamic = dynamic

val func:
    (Word8Vector.vector, int) CompositeHashFunction.comp_hash_function ref =
    ref (CompositeHashFunction.empty 0)

val hashVector = HashWord8Vector.hash

val hVal = ref 0w0

fun init () =
    if dynamic
    then (parts := 1;
	  func := CompositeHashFunction.empty 0)
    else let val initParts = valOf initParts
	     val w = Word.fromInt initParts
	     fun hash s = Word.toInt (Word.mod (hashVector s, w))
	 in
	     parts := initParts;
	     func := #1 (CompositeHashFunction.expand
			     (CompositeHashFunction.empty 0, [],
			      hash, 0, List.tabulate (initParts,
						      (fn i => (i, i)))))
	 end

fun part' v = (hVal := hashVector v;
	       CompositeHashFunction.hash (!func) v)
	      
fun part s = let
    val v = StateEncoder.map s
in
    (v, part' v)
end

fun modW w s = Word.toInt (Word.mod (!hVal, w))

fun reorganize (part, id) = let
    val (path, labels, partLabel) = CompositeHashFunction.getPath (!func, id)
    val (hash, label, children) = let
	fun pow 0 = 1 | pow n = pow (n - 1) * 2
	val m = pow (List.length labels + 1)
	val n = pow (List.length labels)
    in
	(modW (Word.fromInt m), m, [ (partLabel, partLabel),
				     (partLabel + n, partLabel + n) ])
    end
    val (func', new) =
	CompositeHashFunction.expand (!func, path, hash, label, children)
in
    parts := !parts + 1;
    func := func';
    List.map #1 new
end

fun canReorganize _ = dynamic

val _ = init ()

end


(*
 *  static hash function based on a component of the state vector
 *)
functor StaticLHCFunction(
val parts             : int
structure Model       : MODEL
structure StateEncoder: SERIALIZER where type src = Model.state
structure StateHash   : HASH_FUNCTION where type state = Model.state):
PART_FUNCTION = struct

val initParts = parts

structure FunctionCommon = FunctionCommon(structure Model = Model)
open FunctionCommon

val dynamic = false

val hash = StateHash.hash

fun init () = parts := initParts

fun hashState s = Word.toInt (Word.mod (hash s, Word.fromInt (!parts)))

fun part' v = hashState (StateEncoder.unmap v)

fun part s = (StateEncoder.map s, hashState s)

fun reorganize data = raise Reorganization

fun canReorganize _ = false

val _ = init ()
    
end


(*
 *  dynamic hash function based on a component of the state vector
 *)
functor DynamicLHCFunction(
val classes           : int
structure Model       : MODEL
structure StateEncoder: SERIALIZER where type src = Model.state
structure StateHash   : HASH_FUNCTION where type state = Model.state):
PART_FUNCTION = struct

structure FunctionCommon = FunctionCommon(structure Model = Model)
open FunctionCommon

val dynamic = true

val hash = StateHash.hash

val classMap = Array.array (classes, 0)

val canBeReorganized = ref (Array.array (!parts, true))

fun init () = parts := 1

fun hashState s =
    Array.sub (classMap,
	       Word.toInt (Word.mod (hash s, Word.fromInt (classes))))

fun part' v = hashState (StateEncoder.unmap v)

fun part s = (StateEncoder.map s, hashState s)

fun reorganize (part, id) = let
    val classes = Array.foldri (fn (i, id', l) => if id = id'
						  then i :: l
						  else l) [] classMap
in
    case List.take (classes, (List.length classes) div 2)
     of [] => (Array.update (!canBeReorganized, id, false);
	       [])
      | classes => (
	List.app (fn c => Array.update (classMap, c, !parts)) classes;
	canBeReorganized :=
	Array.tabulate (!parts + 1,
			(fn id =>
			    if id = !parts
			    then true
			    else Array.sub (!canBeReorganized, id)));
	[id, !parts] before parts := !parts + 1)
end

fun canReorganize id = Array.sub (!canBeReorganized, id)

val _ = init ()

end


(*
 *  all static heuristics
 *)
functor DynamicCompStaticFunction(
structure Model       : MODEL
val hashComponents    : (Model.state -> word) list
structure StateHash   : HASH_FUNCTION where type state = Model.state
structure StateEncoder: SERIALIZER where type src = Model.state
) : PART_FUNCTION = struct

structure FunctionCommon = CompFunctionCommon(
structure Model = Model
structure StateEncoder = StateEncoder)
open FunctionCommon

val components = 1

val lgHash = List.length hashComponents

fun reorganize (part, id) = let
    val (path, _, _) = CompositeHashFunction.getPath (!func, id)
    val lg = List.length path
    val h = if lg < lgHash
	    then let val l = List.drop (hashComponents, lg)
		     val l = if List.length l >= components
			     then List.take (l, components)
			     else l
		 in
		     List.map (fn f => (f, dontCare)) l
		 end
	    else [ (StateHash.hash, dontCare) ]
in
    updateFunc (path, h)
end

val _ = init ()

end


(*
 *  dynamic heuristic DR
 *)
functor DynamicCompDRFunction(
structure Model       : MODEL
structure StateHash   : HASH_FUNCTION where type state = Model.state
structure StateEncoder: SERIALIZER where type src = Model.state
structure Components  : MODEL_COMPONENTS where type event = Model.event
                                           and type state = Model.state
) : PART_FUNCTION = struct

structure FunctionCommon = CompFunctionCommon(
structure Model = Model
structure StateEncoder = StateEncoder)
open FunctionCommon

val hashComponents = Components.hashComponents

val rnd = Random.rand (17, 97)

fun reorganize (part, id) = let
    val (path, labels, _) = CompositeHashFunction.getPath (!func, id)
    val candidates =
	List.filter (fn (id, _) => not (List.exists (fn id' => id = id')
						    labels))
		    hashComponents
    val (h, label) =
	case candidates
	 of [] => (StateHash.hash, dontCare)
	  | candidates => let val lg = List.length candidates
			      val r = Random.randRange (0, lg - 1) rnd
			      val (id, h) = List.nth (candidates, r)
			  in (h, id) end
in
    updateFunc (path, [ (h, label) ])
end

val _ = init ()

end


(*
 *  dynamic heuristic DE
 *)
functor DynamicCompDEFunction (
structure Model       : MODEL
structure StateHash   : HASH_FUNCTION where type state = Model.state
structure StateEncoder: SERIALIZER where type src = Model.state
structure Components  : MODEL_COMPONENTS where type event = Model.event
                                           and type state = Model.state
) : PART_FUNCTION = struct

structure FunctionCommon = CompFunctionCommon(
structure Model = Model
structure StateEncoder = StateEncoder)
open FunctionCommon

val hashComponents = Components.hashComponents

val updates = Array.array (Components.numComponents, 0)

fun eventHook e =
    List.app (fn i => Array.update (updates, i, Array.sub (updates, i) + 1))
	     (Components.componentsUpdated e)

fun reorganize (part, id) = let
    val (path, labels, _) = CompositeHashFunction.getPath (!func, id)
    fun compHeuristic (id, h) = if List.exists (fn id' => id = id') labels
				then NONE
				else case Array.sub (updates, id)
				      of 0 => NONE
				       | u => SOME (id, h, u)
    val h = List.mapPartial compHeuristic hashComponents
    val h = ListMergeSort.sort (fn ((_, _, h), (_, _, h')) => h > h') h
    val (h, label) = case h of [] => (StateHash.hash, dontCare)
			     | (id, h, _) :: _ => (h, id)
in
    updateFunc (path, [ (h, label) ])
end

val _ = init ()

end


(*
 *  dynamic heuristic DD
 *)
functor DynamicCompDDFunction (
structure Model       : MODEL
structure StateHash   : HASH_FUNCTION where type state = Model.state
structure StateEncoder: SERIALIZER where type src = Model.state
structure Components  : MODEL_COMPONENTS where type event = Model.event
                                           and type state = Model.state
) : PART_FUNCTION = struct

structure FunctionCommon = CompFunctionCommon(
structure Model = Model
structure StateEncoder = StateEncoder)
open FunctionCommon

val hashComponents = Components.hashComponents

fun reorganize (part, id) = let
    val (path, labels, _) = CompositeHashFunction.getPath (!func, id)
    val h = List.mapPartial
		(fn (id, h) => if List.exists (fn id' => id = id') labels
			       then NONE
			       else SOME (id, h, Array.array (degree, 0)))
		hashComponents
    fun oneState (_, s, _) = let
        val s = StateEncoder.unmap s
    in
        List.app
            (fn (_, h, a) => let val h = Word.toInt (Word.mod (h s, degreeW))
                             in Array.update (a, h, Array.sub (a, h) + 1) end)
            h
    end
    val _ = ExternalPartition.app oneState part
    fun compHeuristic (id, h, a) = let
        val (sum, n) = Array.foldl (fn (0, data) => data
                                     | (s, (sum, n)) => (sum + s, n + 1))
                                   (0, 0) a
        val avg = (Real.fromInt sum) / (Real.fromInt n)
        val dev = Array.foldl
		      (fn (n, s) =>
			  Real.+ (s, Math.pow (Real.fromInt n - avg, 2.0)))
                      0.0 a
        val dev = Math.sqrt (dev / (Real.fromInt n))
    in
        if n <> 1
        then SOME (id, h, dev)
        else NONE
    end
    val h = List.mapPartial compHeuristic h
    val h = ListMergeSort.sort (fn ((_, _, h), (_, _, h')) => h > h') h
    val (h, label) = case h of [] => (StateHash.hash, dontCare)
			     | (id, h, _) :: _ => (h, id)
in
    updateFunc (path, [ (h, label) ])
end

val _ = init ()

end


(*
 *  dynamic heuristic DDE
 *)
functor DynamicCompDDEFunction (
structure Model       : MODEL
structure StateHash   : HASH_FUNCTION where type state = Model.state
structure StateEncoder: SERIALIZER where type src = Model.state
structure Components  : MODEL_COMPONENTS where type event = Model.event
                                           and type state = Model.state
) : PART_FUNCTION = struct

structure FunctionCommon = CompFunctionCommon(
structure Model = Model
structure StateEncoder = StateEncoder)
open FunctionCommon

val hashComponents = Components.hashComponents

val updates = Array.array (Components.numComponents, 0)

fun eventHook e =
    List.app (fn i => Array.update (updates, i, Array.sub (updates, i) + 1))
	     (Components.componentsUpdated e)

fun reorganize (part, id) = let
    val (path, labels, _) = CompositeHashFunction.getPath (!func, id)
    val h = List.mapPartial
		(fn (id, h) => if List.exists (fn id' => id = id') labels
				  orelse
				  Array.sub (updates, id) = 0
			       then NONE
			       else SOME (id, h, Array.array (degree, 0)))
		hashComponents
    fun oneState (_, s, _) = let
        val s = StateEncoder.unmap s
    in
        List.app (fn (_, h, a) =>
		     let val h = Word.toInt (Word.mod (h s, degreeW))
                     in Array.update (a, h, Array.sub (a, h) + 1) end) h
    end
    val _ = ExternalPartition.app oneState part
    fun compHeuristic (id, h, a) = let
        val (sum, n) = Array.foldl (fn (0, data) => data
                                     | (s, (sum, n)) => (sum + s, n + 1))
                                   (0, 0) a
        val avg = (Real.fromInt sum) / (Real.fromInt n)
        val dev = Array.foldl
		      (fn (n, s) =>
			  Real.+ (s, Math.pow (Real.fromInt n - avg, 2.0)))
                      0.0 a
        val dev = Math.sqrt (dev / (Real.fromInt n))
    in
        if n <> 1
        then SOME (id, h, dev, Array.sub (updates, id))
        else NONE
    end
    val h = List.mapPartial compHeuristic h
    val pos = Array.array (Components.numComponents, 0)
    fun sort cmp = let
	val h' = ListMergeSort.sort cmp h
    in
	List.foldl (fn ((id, _, _, _), i) =>
		       (Array.update (pos, id, Array.sub (pos, id) + i);
			i + 1)) 1 h'
    end
    val _ = (sort (fn ((_, _, h, _), (_, _, h', _)) => h > h');
	     sort (fn ((_, _, _, h), (_, _, _, h')) => h > h'))
    val min = Array.foldli (fn (i, 0, min) => min
			     | (i, iPos, NONE) => SOME i
			     | (i, iPos, SOME min) =>
			       if iPos < Array.sub (pos, min)
			       then SOME i
			       else SOME min) NONE pos
    val (h, label) =
	case min of NONE => (StateHash.hash, dontCare)
		  | SOME min =>
		    (#2 (valOf (List.find (fn (id', _) => id' = min)
					  hashComponents)), min)
in
    updateFunc (path, [ (h, label) ])
end

val _ = init ()

end
