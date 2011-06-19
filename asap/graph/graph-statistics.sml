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
 *     bfs-statistics.sml
 *
 *  Created:
 *     Jun. 12, 2008
 *
 *  Description:
 *     Generates an xml file from a model that provides some statistics on
 *  the reachability graph of a model.  The generated file respects the dtd of
 *  state space information files of the BEEM database (see
 *  http://anna.fi.muni.cz/models/).
 *)


functor GraphStatistics(
structure Model : MODEL
structure Hash  : HASH_FUNCTION
sharing type Model.state = Hash.state) = struct

structure GraphGenerator = ComBackDDDReachabilityGraphGenerator(
structure Graph = Graph
structure Hash = Hash
structure Model = Model)

structure Graph = GraphGenerator.Graph

structure BFSExplorer = GraphExplorer(
structure Graph = Graph
structure WaitingSet = FifoQueue)

structure DFSExplorer = GraphExplorer(
structure Graph = Graph
structure WaitingSet = Stack)

exception NotFound

fun makeStatistics output = let

    val nullDepth = ~ 1

    (*  statistics collected  *)
    val edges = ref (Array.array (0, 0))
    val states = ref (Array.array (0, 0))
    val backEdges = ref (Array.array (0, Array.array (0, 0)))

    fun incr1D array i = Array.update (array, i, Array.sub (array, i) + 1)

    fun incr2D square (i, j) = incr1D (Array.sub (square, i)) j
	
    fun incrDepth () = let
	fun expandArray array = let
	    val lg = Array.length array
	in
	    Array.tabulate
		(lg + 1, (fn i => if i >= lg
				  then 0
				  else Array.sub (array, i)))
	end
	fun expandSquare square = let
	    val lg = Array.length square
	in
	    Array.tabulate
		(lg + 1, (fn i => if i >= lg
				  then Array.tabulate (lg + 1, fn _ => 0)
				  else expandArray (Array.sub (square, i))))
	end
    in
	states := expandArray (!states);
	edges := expandArray (!edges);
	backEdges := expandSquare (!backEdges)
    end

    (*  Construction of the reachability graph  *)
    val _ = print "Building reachability graph...\n"
    val graph = GraphGenerator.genGraph (Model.getInitialStates ()) ~1 ()
    val statesNo = Graph.numNodes graph
    val edgesNo = Graph.numEdges graph

    (*  BFS exploration of the reachability graph  *)
    val _ = print "BFS exploration of the reachability graph...\n"
    fun nodeHook (n, d, _) = let
	val d = case d of ~1 => (Graph.setNodeTag graph (n, 0); 0)
			| d => d
    in
	if (d + 1) <= Array.length (!states)
	then ()
	else incrDepth ();
	incr1D (!states) d;
	((), BFSExplorer.VISIT)
    end
    fun edgeHook (e, _, _) = let
	val n = Graph.getSrc graph e
	val n' = Graph.getDest graph e
	val d = Graph.getNodeTag graph n
	val d' = Graph.getNodeTag graph n'
	val d' = case d' of ~1 => (Graph.setNodeTag graph (n', d + 1);
				   d + 1)
			  | _ => d'
    in
	incr1D (!edges) d;
	if d' <= d
	then incr2D (!backEdges) (d, d - d')
	else ();
	((), BFSExplorer.VISIT)
    end
    val hooks = { nodeHook    = nodeHook,
		  edgeHook    = edgeHook,
		  enterWSHook = fn _ => (),
		  leaveWSHook = fn _ => () }
    val _ = BFSExplorer.explore graph hooks () (Graph.getRoots graph)

    (*  DFS exploration of the reachability graph  *)
    val _ = print "DFS exploration of the reachability graph...\n"
    val stack = HashTable.mkTable (Graph.hashNode, op =) (100000, NotFound)
    val samplePeriod = Int.max (1, statesNo div 1000)
    fun edgeHook (edge, _, (n, sample, size, maxSize, back, shortest)) = let
	val (back, shortest) =
	    case HashTable.find stack (Graph.getDest graph edge)
	     of NONE => (back, shortest)
	      | SOME depthDest => let
		    val depthSrc = HashTable.lookup
				       stack (Graph.getSrc graph edge)
		    val lg = 1 + depthSrc - depthDest
		in
		    (back + 1, case shortest
				of NONE => SOME lg
				 | SOME lg' => SOME (Int.min (lg, lg')))
		end
    in
	((n, sample, size, maxSize, back, shortest), DFSExplorer.VISIT)
    end
    fun enterStackHook (node, _,
			(n, sample, size, maxSize, back, shortest)) = let
	val sample = if n mod samplePeriod = 0
		     then (n, size) :: sample
		     else sample
    in
	HashTable.insert stack (node, size);
	(n + 1, sample, size + 1, Int.max (size + 1, maxSize), back, shortest)
    end
    fun leaveStackHook (node, _, (n, sample, size, maxSize, back, shortest)) =
	(HashTable.remove stack node;
	 (n, sample, size - 1, maxSize, back, shortest))
    val hooks = { nodeHook    = fn (_, _, value) => (value, DFSExplorer.VISIT),
		  edgeHook    = edgeHook,
		  enterWSHook = enterStackHook,
		  leaveWSHook = leaveStackHook }
    val (_, DFSStackSample, _, DFSMaxStackSize,
	 DFSBackEdges, DFSShortestCycle) =
	DFSExplorer.explore graph hooks
			    (0, [], 0, 0, 0, NONE)
			    (Graph.getRoots graph)
    val DFSStackSample = List.rev DFSStackSample
    val DFSFrontEdges = statesNo - 1
    val DFSCrossEdges = edgesNo - DFSBackEdges - DFSFrontEdges
    val DFSShortestCycle = case DFSShortestCycle of NONE => ~1
						  | SOME lg => lg

    (*  computation of the SCC graph  *)
    val _ = print "Building SCC graph...\n"
    val SCCGraph = Graph.computeSCCGraph graph

    (*  some more statistics  *)
    val _ = print "Collecting statistics...\n"
    val ble = Array.foldl (fn (a, s) => Array.foldl (op +) s a) 0 (!backEdges)
    val maxBleLg = Array.foldl
		       (fn (a, m) =>
			   let val m' = Array.foldli (fn (i, 0, m) => m
						       | (i, _, m) => i) 0 a
			   in Int.max (m', m) end)
		       0 (!backEdges)
    val avgBleLg = Array.foldl
		       (fn (a, s) =>
			   Array.foldli
			       (fn (lg, n, s) => s + lg * n) s a)
		       0 (!backEdges)
    val avgBleLg = (Real.fromInt avgBleLg) / (Real.fromInt ble)
    val maxDeg =
	Graph.foldNodes
	    graph
	    (fn (n, _, maxDeg) => let
		    val outDeg = List.length (Graph.getSucc graph n)
		    val inDeg = List.length (Graph.getPred graph n)
		in
		    Int.max (maxDeg, Int.max(inDeg, outDeg))
		end)
	    0
    val (inDeg, outDeg) =
	Graph.foldNodes
	    graph
	    (fn (n, _, (inDeg, outDeg)) => let
		    val od = List.length (Graph.getSucc graph n)
		    val id = List.length (Graph.getPred graph n)
		in
		    (Vector.update (inDeg,  id, 1 + Vector.sub (inDeg,  id)),
		     Vector.update (outDeg, od, 1 + Vector.sub (outDeg, od)))
		end)
	    (Vector.tabulate (maxDeg + 1, fn _ => 0),
	     Vector.tabulate (maxDeg + 1, fn _ => 0))
    fun getMax v = Vector.foldli (fn (_, 0, j) => j
				   | (i, n, _) => i) 0 v
    val maxInDeg = getMax inDeg
    val maxOutDeg = getMax outDeg
    val (trivialSCC, nonTrivialSCC, terminalSCC, largestSCC) =
	Graph.foldNodes
	    SCCGraph
	    (fn (n, l, (trivial, nonTrivial, terminal, largest)) => let
		    val (trivial, nonTrivial) =
			case l of [ _ ]=> (trivial + 1, nonTrivial)
				| _    => (trivial, nonTrivial + 1)
		    val terminal = case Graph.getSucc SCCGraph n
				    of [] => terminal + 1
				     | _ => terminal
		    val lg = List.length l
		    val largest = if lg > largest
				  then lg
				  else largest
		in
		    (trivial, nonTrivial, terminal, largest)
		end)
	    (0, 0, 0, 0)

    (*  IO functions  *)
    val f = TextIO.openOut output
    fun output str = TextIO.output (f, str)
    fun outputLine str = output (str ^ "\n")
    fun outputNum f tag value =
	output (String.concat [ "<", tag, ">", f value, "</", tag, ">" ])
    fun realToString r = if Real.== (r, 0.0)
			 then "0"
			 else Real.fmt (StringCvt.GEN (SOME 6)) r
    val outputInt = outputNum Int.toString
    val outputReal = outputNum realToString
    fun newLine () = output "\n"
    fun oneLevel i =
	(output ("<level id=\"" ^ (Int.toString i) ^ "\">");
	 outputInt "states" (Array.sub (!states, i));
	 outputInt "edges" (Array.sub (!edges, i));
	 outputLine "</level>")
    fun oneStackItem (n, size) =
	outputLine (String.concat [ "<stack-size id=\"", Int.toString n, "\">",
				    realToString ((Real.fromInt (100 * size)) /
						  (Real.fromInt statesNo)),
				    "</stack-size>" ]);
	 
    fun outputLength l =
	if l >= Array.length (!backEdges)
	then ()
	else let val sum = Array.foldl (fn (a, sum) => sum + Array.sub (a, l))
				       0 (!backEdges)
	     in
		 outputLine (String.concat [
			     "<back-level-length id=\"", Int.toString l, "\">",
			     realToString ((Real.fromInt (100 * sum)) /
					   (Real.fromInt ble)),
			     "</back-level-length>"]);
		 if l = maxBleLg
		 then ()
		 else outputLength (l + 1)
	     end
    fun outputDeg i =
	outputLine (String.concat [
		    "<degree id=\"", Int.toString i, "\">",
		    "<in>", Int.toString (Vector.sub (inDeg, i)), "</in>",
		    "<out>", Int.toString (Vector.sub (outDeg, i)), "</out>",
		    "</degree>"])
in
    outputLine "<state-space-info>";
    outputInt "states" statesNo; newLine ();
    outputInt "edges" edgesNo; newLine ();
    
    (*  BFS informations  *)
    outputLine "<bfs-info>";
    outputInt "levels" (Array.length (!states)); newLine ();
    outputInt "max-level" (Array.foldl Int.max 0 (!states)); newLine ();
    outputInt "back-level-edges" ble; newLine ();
    outputInt "max-back-level-edge" maxBleLg; newLine ();
    outputReal "avg-back-level-edge" avgBleLg; newLine ();
    outputLine "<bfs-levels>";
    Array.appi (fn (i, _) => oneLevel i) (!states);
    outputLine "</bfs-levels>";
    outputLength 0;
    outputLine "</bfs-info>";

    (*  degree information  *)
    outputLine "<degrees>";
    outputReal "avg" ((Real.fromInt edgesNo) / (Real.fromInt statesNo));
    newLine ();
    outputInt "max-in" maxInDeg; newLine ();
    outputInt "max-out" maxOutDeg; newLine ();
    List.app outputDeg (List.tabulate (1 + Int.max (maxInDeg, maxOutDeg),
				       (fn i => i)));
    outputLine "</degrees>";
    
    (*  SCC informations  *)
    outputLine "<scc-info>";
    outputInt "count" (Graph.numNodes SCCGraph); newLine ();
    outputInt "trivial" trivialSCC; newLine ();
    outputInt "terminal" terminalSCC; newLine ();
    outputInt "largest" largestSCC; newLine ();
    outputLine "</scc-info>";

    (*  DFS informations  *)
    outputLine "<dfs-info>";
    List.app oneStackItem DFSStackSample;
    outputInt "max-stack" DFSMaxStackSize; newLine ();
    outputInt "front-edges" DFSFrontEdges; newLine ();
    outputInt "back-edges" DFSBackEdges; newLine ();
    outputInt "cross-edges" DFSCrossEdges; newLine ();
    outputInt "shortest-cycle" DFSShortestCycle; newLine ();
    outputLine "</dfs-info>";
    outputLine "</state-space-info>";
    TextIO.closeOut f
end

end
