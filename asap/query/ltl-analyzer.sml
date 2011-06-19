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
functor LTLOptimizerHelper(
structure Automaton : BUCHI_EXPRESSION
) =
struct
    type state = Automaton.state
    structure Automaton = Automaton
    structure Model = BuchiSimulator(structure Expression = Automaton)
    structure Hash = BuchiHashFunction
    structure Storage = HashStorage(structure Model = Model structure Hash = Hash)
    structure Exploration = DFSExploration(structure Model = Model
                                           structure Storage = Storage)
    structure Exploration = NoTraceExploration(structure Exploration = Exploration)
    structure Exploration = SimpleExploration(structure Exploration = Exploration)
    structure ReachabilityGraph =
        ReachabilityGraphGenerator(
            structure Graph = Graph
            structure Exploration = Exploration
            structure StateMapping = IdentityStateMapping (structure Model = Model)
            structure Dictionary = Storage 
            structure Storage = Storage)
    val (_, graph) = ReachabilityGraph.genGraph
        (Model.getInitialStates())
        (Storage.emptyStorage { init_size = 100 } ())
        { init_size = 100 }
        (fn (CPN'elm, _) => CPN'elm)  (fn (_, CPN'elm, _) => CPN'elm)
    val scc = Graph.computeSCCGraph graph

    val terminals = 
        Graph.foldNodes scc (fn (node, _, rest) => if Graph.getSucc scc node = []
                                                   then node::rest
                                                   else rest) []

    fun sccToBuchi nodes =
        List.map (Graph.getNodeTag graph) (List.concat (List.map (Graph.getNodeTag scc) nodes))

    fun contains lst elm = List.exists (fn e => e = elm) lst

    fun filter reachable [] = []
      | filter reachable (s::ss) =
      if contains reachable s
      then s::(filter reachable ss)
      else filter reachable ss

    fun renumber reachable (state, _, (numbering, next)) =
      if contains reachable state
      then ((fn s => if s = state then next else numbering s), next + 1)
      else (numbering, next)

    fun numbering reachable =
        Vector.foldli (renumber reachable) (fn _ => ~1, 0) Automaton.transitionTable

    fun map numbering reachable states = ListMergeSort.uniqueSort Int.compare
        (List.map numbering (filter reachable states))

    fun transitionTable numbering reachable =
        Vector.fromList
        (Vector.foldri (fn (state, (succ, formulas), result) =>
        if numbering state > ~1
        then (map numbering reachable succ, formulas)::result
        else result) [] Automaton.transitionTable)

    fun labels numbering =
        Vector.fromList
        (Vector.foldri (fn (state, labels, result) =>
        if numbering state > ~1
        then (labels)::result
        else result) [] Automaton.labels)

    fun labelsOf elm = ListMergeSort.uniqueSort String.compare (Vector.sub
        (Automaton.labels, Graph.getNodeTag graph elm))

    val reverseMap = DynamicArray.array (Vector.length Automaton.transitionTable, ~1)
    val _ = Graph.foldNodes graph (fn (node, id, ()) => DynamicArray.update
    (reverseMap, id, node)) ()

    val mapReverse = List.foldl (fn (id, rest) => case (DynamicArray.sub(reverseMap, id))
                                                    of ~1 => rest
                                                     | n => n::rest) []

    fun successorsOf elm = mapReverse (ListMergeSort.uniqueSort Int.compare (#1 (Vector.sub
        (Automaton.transitionTable, Graph.getNodeTag graph elm))))
end

functor LTLCompressor(
structure Automaton : BUCHI_EXPRESSION
) : BUCHI_EXPRESSION =
struct
    structure Helper = LTLOptimizerHelper(structure Automaton = Automaton)
    open Helper

    val initials = Graph.getRoots graph

    val accepting = mapReverse Automaton.accepting

    val emptyVector = Vector.tabulate(Graph.numNodes graph, fn n => ([~1], [] :
    (state -> bool) PLTLSyntax.formula list))
    val emptyVector' = Vector.tabulate(Graph.numNodes graph, fn n => ["FALSE"])

    val (transitionTable, labels) =
        Graph.foldNodes graph
        (fn (node, id, (transitionTable, labels)) =>
        let
            val (succ, formulas) = Vector.sub (Automaton.transitionTable, id)
            val succ' = mapReverse succ
            val labels' = Vector.sub (Automaton.labels, id)
        in
            (Vector.update (transitionTable, node, (succ', formulas)),
             Vector.update (labels, node, labels'))
        end) (emptyVector, emptyVector')
end

functor RemoveNonAcceptingSCC(
structure Automaton : BUCHI_EXPRESSION
) :BUCHI_EXPRESSION =
struct
    structure Helper = LTLOptimizerHelper(structure Automaton = Automaton)
    open Helper

    fun accepting [] = false
      | accepting [elm] =
        Model.winning (Graph.getNodeTag graph elm, []) andalso
        (List.exists (fn n => n = elm) (Graph.getSucc graph elm))
      | accepting elms =
        List.exists (fn state => Model.winning (Graph.getNodeTag graph state, [])) elms
    val accepting = Graph.foldNodes scc (fn (node, nodes, rest) => if accepting nodes
                                                                   then node::rest
                                                                   else rest) []
    val accepting = ListMergeSort.uniqueSort Int.compare accepting
    fun addPredecessors nodes pred =
    let
        val pred = List.concat (List.map (Graph.getPred scc) pred)
    in
        (ListMergeSort.uniqueSort Int.compare (List.@ (pred, nodes)), pred)
    end

    fun iterate nodes pred =
    let
        val (result, pred) = addPredecessors nodes pred
    in
        if pred = []
        then result
        else iterate result pred
    end

    val reachable = iterate accepting accepting

    val accepting' = sccToBuchi accepting
    val reachable' = sccToBuchi reachable

    val (numbering, max) = numbering reachable'

    val initials = map numbering reachable' Automaton.initials
    val accepting = map numbering accepting' Automaton.accepting

    val transitionTable = transitionTable numbering reachable'
    val labels = labels numbering
end

functor FinalSCCOptimizer(
structure Automaton : BUCHI_EXPRESSION
) :BUCHI_EXPRESSION =
struct
    structure Helper = LTLOptimizerHelper(structure Automaton = Automaton)
    open Helper

    fun sameLabels [] = false
      | sameLabels [elm] = false (* no need to do optimizarion in this case *)
      | sameLabels [elm1, elm2] = labelsOf elm1 = labelsOf elm2
      | sameLabels (elm1::elm2::rest) =
        sameLabels [elm1, elm2] andalso sameLabels (elm2::rest)

    val winning = List.exists (fn elm => Model.winning (Graph.getNodeTag graph elm, []))

    fun reducable node =
    let
        val elms = Graph.getNodeTag scc node
    in
        sameLabels elms andalso winning elms
    end

    val reducable = List.filter reducable terminals
    val reducable' = List.map (fn node => List.map (Graph.getNodeTag graph)
    (Graph.getNodeTag scc node)) reducable

    val reachable = List.tabulate (Vector.length Automaton.transitionTable, fn n => n)
    val numbering =
        List.foldl (fn (class, f) => fn n => if contains class n then List.hd
        class else f n) (fn n => n) reducable'

    val initials = map numbering reachable Automaton.initials
    val accepting = map numbering reachable Automaton.accepting

    val transitionTable =
        Vector.map (fn (succ, formulas) => (ListMergeSort.uniqueSort Int.compare
        (map numbering reachable succ), formulas)) Automaton.transitionTable
    val labels = Automaton.labels
end

functor RemoveRedundantStates(
structure Automaton : BUCHI_EXPRESSION
) =
struct
    structure Helper = LTLOptimizerHelper(structure Automaton = Automaton)
    open Helper

    val nodes = Graph.foldNodes graph
    (fn (node, elm, rest) =>
    (node,
    ListMergeSort.uniqueSort Int.compare (Graph.getSucc graph node),
    ListMergeSort.uniqueSort Int.compare (Graph.getPred graph node),
    ListMergeSort.uniqueSort String.compare (Vector.sub (Automaton.labels, elm)),
    #2 (Vector.sub (Automaton.transitionTable, elm))
    )::rest) []

    fun accept n = contains Automaton.accepting (Graph.getNodeTag graph n)
    fun init n = contains (Graph.getRoots graph) n

    val initial = Graph.getRoots graph

    val accepting = Graph.foldNodes graph
    (fn (node, elm, rest) => if contains Automaton.accepting elm
                             then node::rest
                             else rest) []

    fun merge s1 s2 = 
        ListMergeSort.uniqueSort Int.compare (List.@(s1, s2))

    fun subset s1 s2 =
        merge s1 s2 = s2
    fun subset' s1 s2 =
        ListMergeSort.uniqueSort String.compare (List.@(s1, s2)) = s2

    fun node (n, s, p, l, f) = n
    fun succ (n, s, p, l, f) = s
    fun pred (n, s, p, l, f) = p
    fun labels (n, s, p, l, f) = l
    fun formulas (n, s, p, l, f) = f

    fun defaultMap n = n

    fun directReplacable n1 n2 =
        succ n1 = succ n2 andalso
        labels n1 = labels n2

    fun subsetReplacable n1 n2 =
        succ n1 = succ n2 andalso
        subset' (labels n2) (labels n1) andalso
        subset (pred n1) (pred n2)

    fun toSelfLoopReplacable n1 n2 =
        labels n1 = labels n2 andalso
        (succ n1) = [node n2] andalso
        contains (succ n2) (node n2) andalso
        ((not (accept (node n1))) orelse (accept (node n2)))

    fun find pred lst =
        case List.find pred lst
          of SOME (elm) =>
             SOME (elm, List.filter (fn e => node e <> node elm) lst)
           | NONE => NONE

    fun iterate p next [] renumbering changed = (next, renumbering, changed)
      | iterate p next (hd::tl) renumbering changed =
      case find (p hd) tl
          of SOME (n2, tl) =>
          iterate p ((node n2, merge (succ n2) (succ hd), merge (pred n2) (pred
          hd), labels n2, formulas n2)::next) tl (fn n => if n = node hd
                                                          then node n2
                                                          else renumbering n) true
           | NONE => iterate p (hd::next) tl renumbering changed

    fun map renumbering (n, s, p, l, f) =
        (renumbering n, ListMergeSort.uniqueSort Int.compare (List.map
        renumbering s), ListMergeSort.uniqueSort Int.compare (List.map
        renumbering p), l, f)

    fun update (initial, accepting, nodes) (next, renumbering, changed) =
            if changed
            then (ListMergeSort.uniqueSort Int.compare (List.map renumbering initial),
                  ListMergeSort.uniqueSort Int.compare (List.map renumbering accepting),
                  List.map (map renumbering) next)
            else (initial, accepting, nodes)

    fun removeTrivialSCC ((initial, accepting, nodes), changed) =
    let
        val graph = Graph.empty ()
        val map = List.foldl (fn ((n, _, _, l, _), map) =>
                let
                    val node = Graph.newNode graph (n, l)
                in
                    (fn nn => if n = nn then node else map nn)
                end) (fn _ => ~1) nodes
        val _ = List.foldl (fn (n, _) => Graph.setRoot graph (map n)) () accepting
        val _ = List.foldl (fn ((n, s, _, _, _), _) =>
        List.foldl (fn (nn, _) => Graph.newEdge graph (map n, map nn, ())) 0 s) 0 nodes
        val scc = Graph.computeSCCGraph graph
        val sccs = Graph.foldNodes scc (fn (node, tag, rest) => tag::rest) []
        fun sameLabels [] = false
          | sameLabels [elm] = false
          | sameLabels [elm1, elm2] = (#2 (Graph.getNodeTag graph elm1)) = (#2
          (Graph.getNodeTag graph elm2))
          | sameLabels (elm1::elm2::rest) =
          sameLabels [elm1, elm2] andalso sameLabels (elm2::rest)
        val sccs' = List.filter sameLabels sccs
        fun filterInternal scc = List.filter (fn elm => not (contains scc elm))
        fun filterScc scc =
        let
            val events = ListMergeSort.uniqueSort Int.compare (List.concat
            (List.map (Graph.getSucc graph) scc))
            val events' = filterInternal scc events
        in
            not (List.exists (fn n => not (subset events' (ListMergeSort.uniqueSort
            Int.compare (Graph.getSucc graph n)))) scc)
        end
        fun mapSCC scc =
        let
            val events = ListMergeSort.uniqueSort Int.compare (List.concat
            (List.map (Graph.getSucc graph) scc))
            val events' = filterInternal scc events
        in
            (scc, events, events', List.map (fn n => ListMergeSort.uniqueSort
            Int.compare (Graph.getSucc graph n)) scc)
        end
        val sccs'' = List.filter filterScc sccs'
        val sccs''' = List.map (List.map (fn n => #1 (Graph.getNodeTag graph n))) sccs''
        val numbering =
            List.foldl (fn (scc, f) => fn n => if contains scc n then List.hd
            scc else f n) (fn n => n) sccs'''
        val next = List.foldl (fn (n, rest) => if numbering (node n) = node n then
            n::rest else rest) [] nodes
        val changed = if List.null sccs''' then changed else true
    in
        ((scc, sccs, sccs', List.map mapSCC sccs',sccs'', sccs''', next, numbering), (update (initial, accepting, nodes) (next, numbering,
        not (List.null sccs''')), changed))
    end

    fun step predicate ((initial, accepting, nodes), changed) =
    let
        val (next, renumbering, changed') = iterate predicate [] nodes defaultMap false
        val (initial, accepting, nodes) = update (initial, accepting, nodes)
        (next, renumbering, changed')
    in
        ((initial, accepting, nodes), changed orelse changed')
    end

    fun simpleReduce ((initial, accepting, nodes), changed) =
    let
        val ((initial, accepting, nodes), changed') = step directReplacable ((initial, accepting, nodes), false)
    in
        if changed'
        then simpleReduce ((initial, accepting, nodes), true)
        else  ((initial, accepting, nodes), changed)
    end

    fun reduce ((initial, accepting, nodes), changed) =
    let
        val ((initial, accepting, nodes), changed') = step toSelfLoopReplacable
        ((initial, accepting, List.rev nodes), false)
        val ((initial, accepting, nodes), changed') = step toSelfLoopReplacable
        ((initial, accepting, nodes), changed')
        val ((initial, accepting, nodes), changed') = step subsetReplacable
        ((initial, accepting, nodes), changed')
        val ((initial, accepting, nodes), changed') = step subsetReplacable
        ((initial, accepting, List.rev nodes), changed')
    in
        if changed'
        then reduce ((initial, accepting, nodes), true)
        else ((initial, accepting, nodes), changed)
    end

    fun reduceAll ((initial, accepting, nodes), changed) =
    let
        val ((initial, accepting, nodes), changed') = simpleReduce ((initial, accepting, nodes), false)
        val ((initial, accepting, nodes), changed') = reduce ((initial, accepting,
        nodes), changed')
        val (_, ((initial, accepting, nodes), changed')) = removeTrivialSCC ((initial,
        accepting, nodes), changed')
    in
        if changed'
        then reduceAll ((initial, accepting, nodes), true)
        else ((initial, accepting, nodes), changed)
    end

    val ((initials, accepting, nodes), changed) = reduceAll ((initial, accepting, nodes), false)

    val transitionTable = List.foldl
    (fn ((n, s, _, _, f), vector) => Vector.update (vector, n, (s, f)))
    Automaton.transitionTable nodes

    val labels = List.foldl
    (fn ((n, _, _, l, _), vector) => Vector.update (vector, n, l))
    Automaton.labels nodes
end
