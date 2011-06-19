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
 *     graph-gen.sml
 *
 *  Created:
 *     Feb. 15, 2008
 *
 *  Description:
 *     Reachability graph generator.  Provides the possiblity to compute
 *  strongly connected component of a state space.
 *)


structure Graph = struct

type node = int

type edge = int

type ('a, 'b) graph = {
     roots   : node list ref,
     nodes   : ('a * edge list) Array.array ref,
     edges   : ('b * node * node) Array.array ref,
     nextNode: int ref,
     nextEdge: int ref,
     numNodes: int ref,
     numEdges: int ref
}

exception NotFound

fun empty () = {
    roots    = ref [],
    nodes    = ref (Array.fromList []),
    edges    = ref (Array.fromList []),
    nextNode = ref 0,
    nextEdge = ref 0,
    numNodes = ref 0,
    numEdges = ref 0
}

val nullNode = ~ 1

val hashNode = Word.fromInt

val hashEdge = Word.fromInt

fun expand (array, size, newValue) =
    if size <> Array.length (!array)
    then Array.update (!array, size, newValue)
    else let val lg    = size
             val newLg = if lg = 0 then 1 else 2 * lg
         in
             array := Array.tabulate
                          (newLg, (fn i => if i >= lg
                                           then newValue
                                           else Array.sub (!array, i)))
         end

fun newNode ({ nodes, nextNode, numNodes, ... }: ('a, 'b) graph) tag = let
    val result = !nextNode
in
    expand (nodes, !nextNode, (tag, []));
    nextNode := !nextNode + 1;
    numNodes := !numNodes + 1;
    result
end

fun newEdge ({ edges, nextEdge, numEdges, nodes, ... }: ('a, 'b) graph)
            (from, to, tag) = let
    val result = !nextEdge
    val (tagFrom, edgesFrom) = Array.sub (!nodes, from)
    val (tagTo,   edgesTo)   = Array.sub (!nodes, to)
in
    expand (edges, !nextEdge, (tag, from, to));
    Array.update (!nodes, from, (tagFrom, result :: edgesFrom));
    Array.update (!nodes, to,   (tagTo,   result :: edgesTo));
    nextEdge := !nextEdge + 1;
    numEdges := !numEdges + 1;
    result
end

fun setRoot ({ roots, ... }: ('a, 'b) graph) r =
    roots := r :: (!roots)

fun getSucc ({ roots, nodes, edges, ... }: ('a, 'b) graph) node = let
    val (_, nodeEdges) = Array.sub (!nodes, node)
in
    List.mapPartial (fn e => let val (_, from, to) = Array.sub (!edges, e)
                             in
                                 if from = node
                                 then SOME to
                                 else NONE
                             end) nodeEdges
end

fun getPred ({ roots, nodes, edges, ... }: ('a, 'b) graph) node = let
    val (_, nodeEdges) = Array.sub (!nodes, node)
in
    List.mapPartial (fn e => let val (_, from, to) = Array.sub (!edges, e)
                             in
                                 if to = node
                                 then SOME from
                                 else NONE
                             end) nodeEdges
end

fun getNodeTag ({ nodes, ... }: ('a, 'b) graph) node = let
    val (tag, _) = Array.sub (!nodes, node)
in
    tag
end

fun setNodeTag ({ nodes, ... }: ('a, 'b) graph) (node, tag) = let
    val (_, nodeEdges) = Array.sub (!nodes, node)
in
    Array.update (!nodes, node, (tag, nodeEdges))
end

fun getEdgeTag ({ edges, ... }: ('a, 'b) graph) edge = let
    val (tag, _, _) = Array.sub (!edges, edge)
in
    tag
end

fun setEdgeTag ({ edges, ... }: ('a, 'b) graph) (edge, tag) = let
    val (_, from, to) = Array.sub (!edges, edge)
in
    Array.update (!edges, edge, (tag, from, to))
end

fun getSrc ({ edges, ... }: ('a, 'b) graph) edge = let
    val (_, from, _) = Array.sub (!edges, edge)
in
    from
end

fun getDest ({ edges, ... }: ('a, 'b) graph) edge = let
    val (_, _, to) = Array.sub (!edges, edge)
in
    to
end

fun getIncomingEdges (graph as { nodes, ... }: ('a, 'b) graph) node = let
    val (_, edges) = Array.sub (!nodes, node)
in
    List.filter (fn edge => getDest graph edge = node) edges
end

fun getOutcomingEdges (graph as { nodes, ... }: ('a, 'b) graph) node = let
    val (_, edges) = Array.sub (!nodes, node)
in
    List.filter (fn edge => getSrc graph edge = node) edges
end

fun getRoots ({ roots, ... }: ('a, 'b) graph) = !roots

fun numNodes ({ numNodes, ... }: ('a, 'b) graph) = !numNodes

fun numEdges ({ numEdges, ... }: ('a, 'b) graph) = !numEdges

fun computeSCCGraph (graph as { roots, numNodes,
                                nodes, edges, ... }: ('a, 'b) graph) = let
    val result  = empty ()
    val stack   = ref []
    val onStack = Array.array (!numNodes, false)
    val map     = Array.array (!numNodes, ~ 1)
    val index   = Array.array (!numNodes, ~ 1)
    val lowlink = Array.array (!numNodes, !numNodes + 1)
    val n       = ref (~ 1)
    val error1  = "computeSCCGraph: unable to find SCC of state"
    val error2  = "computeSCCGraph: empty stack when popping an SCC"
    val error3  = "computeSCCGraph: non empty stack after SCCs computation"
    fun getSCCOfState s =
        case Array.sub (map, s)
         of ~1 => raise LibBase.Impossible error1
          | n => n
    fun setSCCOfState (s, scc) =
        Array.update (map, s, scc)
    fun dfs node = let
        fun visitSucc succ = let
            val newLowlink =
                if Array.sub (index, succ) = ~ 1
                then (dfs succ;
                      SOME (Int.min (Array.sub (lowlink, node),
                                     Array.sub (lowlink, succ))))
                else if not (Array.sub (onStack, succ))
                then NONE
                else SOME (Int.min (Array.sub (lowlink, node),
                                    Array.sub (index, succ)))
        in
            case newLowlink
             of NONE => ()
              | SOME newLowlink => Array.update (lowlink, node, newLowlink)
        end
        fun popSCC () = let
            fun loop () =
                case !stack
                 of [] => raise LibBase.Impossible error2
                  | node :: tl => (
                    stack := tl;
                    Array.update (onStack, node, false);
                    if Array.sub (lowlink, node) = Array.sub (index, node)
                    then [ node ]
                    else node :: loop ())
            val scc = List.rev (loop ())
            val new = newNode result scc
        in
            List.app (fn node => setSCCOfState (node, new)) scc;
            let        fun getSuccSCC node =
                    List.map getSCCOfState (getSucc graph node)
                val succSCC = List.concat (List.map getSuccSCC scc)
                val succSCC = ListMergeSort.uniqueSort Int.compare succSCC
                val succSCC = List.filter (fn scc => scc <> new) succSCC
            in
                List.app (fn node => ignore (newEdge result (new, node, ())))
                         succSCC
            end
        end
    in
        n := !n + 1;
        Array.update (index, node, !n);
        Array.update (lowlink, node, !n);
        Array.update (onStack, node, true);
        stack := node :: (!stack);
        List.app visitSucc (getSucc graph node);
        if Array.sub (lowlink, node) = Array.sub (index, node)
        then popSCC ()
        else ()
    end
    val newRoots = List.map (fn root => (if Array.sub (index, root) <> ~1
                          then ()
                          else dfs root;
                          if !stack = []
                          then SOME (getSCCOfState root)
                          else raise LibBase.Impossible error3))
                    (!roots)
    val newRoots' = List.filter Option.isSome newRoots
    val newRoots'' = List.map Option.valOf newRoots'
    val newRoots''' = ListMergeSort.uniqueSort Int.compare newRoots''
    val _ = List.map (fn node => setRoot result node) newRoots'''
in
    result
end

fun foldNodes ({ nodes, nextNode, ... }: ('a, 'b) graph) f value =
    Array.foldli (fn (i, (tag, _), value) =>
                     if i >= !nextNode
                     then value
                     else f (i, tag, value))
                 value (!nodes)

fun appNodes graph f = foldNodes graph (fn (i, tag, _) => f (i, tag)) ()

fun foldEdges ({ edges, nextEdge, ... }: ('a, 'b) graph) f value =
    Array.foldli (fn (i, (tag, _, _), value) =>
                     if i >= !nextEdge
                     then value
                     else f (i, tag, value))
                 value (!edges)

fun appEdges graph f = foldEdges graph (fn (i, tag, _) => f (i, tag)) ()

fun toDot nodeToString edgeToString
          (graph as { nodes, edges, ... }: ('a, 'b) graph)  fileName = let
    val f = TextIO.openOut fileName
    fun putNode (i, tag) =
        TextIO.output (f, String.concat [
                          Int.toString i,
                          case nodeToString
                           of NONE => ""
                            | SOME f => (" [label=\"" ^ (f tag) ^ "\"]"),
                          ";\n" ])
    fun putEdge (i, tag) = let
        val src  = getSrc  graph i
        val dest = getDest graph i
    in
        TextIO.output (f, String.concat [
                          Int.toString src,
                          " -> ",
                          Int.toString dest,
                          case edgeToString
                           of NONE => ""
                            | SOME f => (" [label=\"" ^ (f tag) ^ "\"]"),
                          ";\n" ])
    end
in
    TextIO.output (f, "digraph G {\n");
    appNodes graph putNode;
    appEdges graph putEdge;
    TextIO.output (f, "}\n") ;
    TextIO.closeOut f
end

end



functor GraphExplorer(
structure WaitingSet: WAITINGSET
structure Graph     : GRAPH): GRAPH_EXPLORER = struct

datatype continue =
         STOP
       | VISIT
       | DONT_VISIT

type 'a waitingset = 'a WaitingSet.waitingset

structure Graph = Graph

type ('a, 'b, 'c) explorer_hooks = {
     nodeHook   : Graph.node * 'a * 'c -> 'c * continue,
     edgeHook   : Graph.edge * 'b * 'c -> 'c * continue,
     enterWSHook: Graph.node * 'a * 'c -> 'c,
     leaveWSHook: Graph.node * 'a * 'c -> 'c
}

fun explore graph { nodeHook, edgeHook, enterWSHook, leaveWSHook }
            value roots = let
    exception NotFound
    exception Done of 'a
    val visited = HashTable.mkTable (Graph.hashNode, op =) (100000, NotFound)
    fun enqueue (node, WS, value) = let
        val out = List.rev (Graph.getOutcomingEdges graph node)
        val nodeTag = Graph.getNodeTag graph node
        val value = enterWSHook (node, nodeTag, value)
        val WS = WaitingSet.enqueue (WS, (node, ref (true, out)))
    in (WS, value) end
    fun dequeue (node, WS, value) = let
        val (WS, _) = WaitingSet.dequeue WS
        val nodeTag = Graph.getNodeTag graph node
        val value = leaveWSHook        (node, nodeTag, value)
    in (WS, value) end
    fun visitEdge (node, edge, (WS, value)) = let
        val succ = Graph.getDest graph edge
        val edgeTag = Graph.getEdgeTag graph edge
        val (value, continue) = edgeHook (edge, edgeTag, value)
    in
        case continue
         of STOP => raise Done value
          | DONT_VISIT => (WS, value)
          | VISIT => if HashTable.inDomain visited succ
                     then (WS, value)
                     else (HashTable.insert visited (succ, ());
                           enqueue (succ, WS, value))
    end
    fun loop (WS, value) =
        case WaitingSet.peek WS
         of NONE => value
          | SOME (node, data) => let
                val (new, edges) = !data
                val (value, continue) =
                    if new
                    then nodeHook (node, Graph.getNodeTag graph node, value)
                    else (value, VISIT)
                val (WS, value) =
                    case (edges, continue)
                     of (_, STOP) => raise Done value
                      | (_, DONT_VISIT) => dequeue (node, WS, value)
                      | ([], VISIT) => dequeue (node, WS, value)
                      | (edge :: tl, VISIT) =>
                        (data := (false, tl);
                        visitEdge (node, edge, (WS, value)))
            in
                loop (WS, value)
            end
    val (WS, value) =
        List.foldl (fn (r, (WS, value)) =>
                       (HashTable.insert visited (r, ());
                        enqueue (r, WS, value)))
                   (WaitingSet.empty, value) roots
in
    (loop (WS, value)) handle Done value => value
end

end

(*
functor ComBackDDDReachabilityGraphGenerator(
structure Graph: GRAPH
structure Hash : HASH_FUNCTION
structure Model: MODEL
sharing type Model.state = Hash.state) = struct

structure Storage = ComBackDDDStorage(
structure Model = Model
structure Hash  = Hash
structure Cache = ComBackDDDStorageHeuristicCache)

structure Exploration = ComBackDDDExploration(
val reconstruct = true
val groupReconstructions = true
val reconstructions = 5000
structure Model = Model
structure Storage = Storage)

type state = Model.state
type event = Model.event
type 'a storage = 'a Storage.storage

structure Graph = Graph

fun genGraph initStates nodeTag edgeTag = let
    exception NotFound
    val storage = Storage.emptyStorage { tableSize         = 1000000,
                                         candidateSetSize  = 15000,
                                         combackCacheSize  = 30000,
                                         stateCacheSize    = NONE }
                                       ()
    val map = HashTable.mkTable (Storage.hashId, op =) (100000, NotFound)
    val graph = Graph.empty ()
    val (storage, _, _) =
        Exploration.explore
            (fn (_, evts) => evts)
            (fn s => s)
            {
             a_initial       = (),
             s_initial       = (),
             t_initial       = (),
             arc_hook        = fn _ => (),
             state_hook      = fn _ => (),
             pre_trace_hook  = fn (_, _, _, _, storage) => ((), storage),
             post_trace_hook = fn (_, _, _, _, storage) => ((), storage)
            } storage initStates
in
    Storage.appId
        (fn id => HashTable.insert map (id, Graph.newNode graph nodeTag))
        storage;
    Storage.appId
        (fn id => let val (s, evts) = Storage.reconstruct (storage, id)
                      val n = valOf (HashTable.find map id)
                      fun exploreSucc (s', _) = let
                          val id' = Storage.getId storage s'
                          val n' = valOf (HashTable.find map id')
                      in
                          ignore (Graph.newEdge graph (n, n', edgeTag))
                      end
                  in
                      List.app (fn e => List.app exploreSucc
                                                 (Model.nextStates (s, e)))
                               evts
                  end)
        storage;
    List.app
        (fn (s, _) => let val id = Storage.getId storage s
                          val r = valOf (HashTable.find map id)
                      in
                          Graph.setRoot graph r
                      end)
        initStates;
    graph
end

end
*)

functor ReachabilityGraphGenerator(
structure Graph       : GRAPH
structure Exploration : SIMPLE_EXPLORATION
structure StateMapping: MAPPING where type src = Exploration.state
structure Dictionary  : STORAGE where type item = StateMapping.dest
structure Storage     : STORAGE where type 'a storage =
                                           'a Exploration.storage
): GRAPH_GENERATOR = struct

structure Graph = Graph

type 'a storage = 'a Storage.storage
type state = Exploration.state
type event = Exploration.event
type init_options = Dictionary.init_options

fun genGraph initStates initStorage initOptions nodeTagging edgeTagging = let
    val dict = Dictionary.emptyStorage initOptions NONE
    val graph = Graph.empty ()
    val (dict, graph) =
        List.foldl (fn (r as (root, _), (dict, graph)) => let
                           val img = StateMapping.map root
                           val node = Graph.newNode graph (nodeTagging r)
                           val (_, _, dict) = Dictionary.add (dict, img)
                           val dict = Dictionary.setTag (dict, img, SOME node)
                           val _ = Graph.setRoot graph node
                       in
                           (dict, graph)
                       end) (dict, graph) initStates
    fun arcHook (((s, evts), e, (s', evts')), (dict, graph)) = let
        val img = StateMapping.map s
        val img' = StateMapping.map s'
        val node = valOf (Dictionary.getTag (dict, img))
        val (dict, node') =
            if Dictionary.contains (dict, img')
            then (dict, valOf (Dictionary.getTag (dict, img')))
            else let val node' = Graph.newNode graph (nodeTagging (s', evts'))
                     val (_, _, dict) = Dictionary.add (dict, img')
                     val dict = Dictionary.setTag (dict, img', SOME node')
                 in
                     (dict, node')
                 end
    in
        Graph.newEdge graph (node, node',
                             edgeTagging ((s, evts), e, (s', evts')));
        (dict, graph)
    end
    val (storage, _, (_, graph)) =
        Exploration.explore
            {
             a_initial       = (dict, graph),
             s_initial       = (),
             arc_hook        = arcHook,
           state_hook      = fn _ => ()
            } initStorage initStates
in
    (storage, graph)
end

end


functor GraphModel(
structure Graph: GRAPH
structure Model: MODEL
val graph      : (Model.state, Model.event) Graph.graph
): GRAPH_MODEL = struct

    open Graph Model

    type state = Graph.node * Model.state
    type event = Graph.edge * Model.event
    type original_state = Model.state
    type original_event = Model.event

    fun unpack_state (_, state) = state
    fun unpack_event (_, event) = event

val initStates = List.map
                     (fn root => ((root, Graph.getNodeTag graph root), List.map
                 (fn edge => (edge, Graph.getEdgeTag graph edge))
                 (Graph.getOutcomingEdges graph root)))
                     (Graph.getRoots graph)

exception EventNotEnabled

    fun getInitialStates () = initStates
    fun getCurrentState () = #1 (List.hd initStates)
    fun getEvents (node, _) = 
    List.map 
    (fn edge => (edge, Graph.getEdgeTag graph edge))
        (Graph.getOutcomingEdges graph node)

    fun nextStates ((node, _), (edge, _)) = let
    val node' = Graph.getDest graph edge
in
    [ ((node', Graph.getNodeTag graph node'), 
    List.map 
    (fn edge => (edge, Graph.getEdgeTag graph edge))
    (Graph.getOutcomingEdges graph node')) ]
end

fun executeSequence (node, edges) = let
    fun exec (_, []) =
        raise LibBase.Impossible "GraphModel.executeSequence.exec"
      | exec (edge, seq as ((node, edges) :: _)) =
        List.hd (nextStates (node, edge)) :: seq
in
    List.rev (List.foldl exec [ (node, edges) ] edges)
end

    fun stateToString (_, state) = Model.stateToString state
    fun eventToString (_, event) = Model.eventToString event
end


functor GraphExploration(
structure Graph: GRAPH
structure Storage : STORAGE
structure Model: MODEL
val graph      : (Model.state, Model.event) Graph.graph
): GRAPH_EXPLORATION = struct

type state = Model.state
type event = Model.event
type 'a storage = 'a Storage.storage
type id = Storage.id

fun explore { a_initial,
              s_initial,
              arc_hook,
              state_hook } storage _ = let
    fun arcHook (edge, edgeTag, value) = let
        val src = Graph.getSrc graph edge
        val src' = Graph.getNodeTag graph src
        val dest = Graph.getDest graph edge
        val dest' = Graph.getNodeTag graph dest
        val srcEdges = Graph.getOutcomingEdges graph src
        val srcEdges' = List.map (Graph.getEdgeTag graph) srcEdges
        val destEdges = Graph.getOutcomingEdges graph dest
        val destEdges' = List.map (Graph.getEdgeTag graph) destEdges
    in
        arc_hook (((src', srcEdges'), edgeTag, (dest', destEdges')), value)
    end
    fun stateHook (node, nodeTag, value) = let
        val edges = Graph.getOutcomingEdges graph node
        val edges' = List.map (Graph.getEdgeTag graph) edges
    in
        state_hook ((nodeTag, edges'), value)
    end
    val aVal = Graph.foldEdges graph arcHook a_initial
    val sVal = Graph.foldNodes graph stateHook s_initial
in                          
    (storage, sVal, aVal)
end
end


functor SCCGraphModel(
structure Model: MODEL
structure Graph: GRAPH
val graph      : (Model.state list, unit) Graph.graph): MODEL = struct

type state = Model.state list
type event = unit
exception EventNotEnabled
          
fun getInitialStates () =
    List.map (fn node => (Graph.getNodeTag graph node,
                          List.map (fn _ => ())
                                   (Graph.getOutcomingEdges graph node)))
                           (Graph.getRoots graph)

fun getCurrentState () = Graph.getNodeTag graph (List.hd (Graph.getRoots graph))

    fun getEvents state  =
        raise EventNotEnabled

fun nextStates _ = raise EventNotEnabled

fun executeSequence _ = raise EventNotEnabled

fun stateToString states =
    String.concat
        ["SCC Node (", Int.toString (List.length states), " nodes)"]
    
fun eventToString _ = ""

end


functor GraphWaitingSetExploration(
structure Model     : GRAPH_MODEL
structure Storage   : STORAGE
structure WaitingSet: WAITINGSET
sharing type Model.original_state = Storage.item
): SIMPLE_TRACE_EXPLORATION = struct

open Model

fun unpackState (state, events) =
    (unpack_state state, List.map unpack_event events)

structure WrappedStorage = struct

open Storage
type item = Model.state

fun add (storage, item) = Storage.add (storage, unpack_state item)

fun addList (storage, items) =
    Storage.addList (storage, List.map unpack_state items)

fun contains (storage, item) =
    Storage.contains (storage, unpack_state item)

fun getTag (storage, item) =
    Storage.getTag (storage, unpack_state item)
                             
fun setTag (storage, item, value) =
    Storage.setTag (storage, unpack_state item, value)
end

structure WrappedWaitingSetExploration = SimpleTraceExploration(
structure Exploration = WaitingSetExploration(
structure Model = Model
structure Storage = WrappedStorage
structure WaitingSet = WaitingSet))

open WrappedWaitingSetExploration
type state = Model.original_state
type event = Model.original_event
val initial_roots = Model.getInitialStates ()
val initial_ok = List.map unpackState initial_roots

exception StartAtTheRoots

fun explore { a_initial, arc_hook, s_initial, state_hook, pre_trace_hook,
              post_trace_hook, t_initial } storage initial_states = let
    val _ = if initial_states = initial_ok
            then ()
            else raise StartAtTheRoots
    fun arc_hook' ((s, ev, s'), c, a) =
        arc_hook ((unpackState s, unpack_event ev, unpackState s'), c, a)
    fun state_hook' (s, c, b) = state_hook (unpackState s, c, b)
    fun trace_hook orig_hook ((s, ev, s'), id1, id2, c, d) =
        orig_hook ((unpackState s, unpack_event ev, unpackState s'),
                   id1, id2, c, d)
in
    WrappedWaitingSetExploration.explore
        { a_initial       = a_initial,
          s_initial       = s_initial,
          t_initial       = t_initial,
          arc_hook        = arc_hook',
          state_hook      = state_hook',
          pre_trace_hook  = trace_hook pre_trace_hook,
          post_trace_hook = trace_hook post_trace_hook } storage initial_roots
end

end
