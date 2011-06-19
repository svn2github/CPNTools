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
 *     graph-sig.sml
 *
 *  Created:
 *     Feb. 15, 2008
 *
 *  Description:
 *     Signature for graph, graph explorer, and reachability graph generator.
 *)


signature GRAPH = sig

eqtype node

eqtype edge

type ('a, 'b) graph

val empty:
    unit
    -> ('a, 'b) graph

val numNodes:
    ('a, 'b) graph
    -> int

val numEdges:
    ('a, 'b) graph
    -> int

val hashNode:
    node
    -> word

val hashEdge:
    edge
    -> word

val newNode:
    ('a, 'b) graph
    -> 'a
    -> node

val newEdge:
    ('a, 'b) graph
    -> node * node * 'b
    -> edge

val setRoot:
    ('a, 'b) graph
    -> node
    -> unit

val getRoots:
    ('a, 'b) graph
    -> node list

val getSucc: 
    ('a, 'b) graph
    -> node
    -> node list

val getPred:
    ('a, 'b) graph
    -> node
    -> node list

val getNodeTag:
    ('a, 'b) graph
    -> node
    -> 'a

val setNodeTag:
    ('a, 'b) graph
    -> node * 'a
    -> unit

val getEdgeTag:
    ('a, 'b) graph
    -> edge
    -> 'b

val setEdgeTag:
    ('a, 'b) graph
    -> edge * 'b
    -> unit

val getSrc:
    ('a, 'b) graph
    -> edge
    -> node

val getDest:
    ('a, 'b) graph
    -> edge
    -> node

val getIncomingEdges:
    ('a, 'b) graph
    -> node
    -> edge list

val getOutcomingEdges:
    ('a, 'b) graph
    -> node
    -> edge list

val appNodes:
    ('a, 'b) graph
    -> (node * 'a -> unit)
    -> unit

val foldNodes:
    ('a, 'b) graph
    -> (node * 'a * 'c -> 'c)
    -> 'c
    -> 'c

val appEdges:
    ('a, 'b) graph
    -> (edge * 'b -> unit)
    -> unit

val foldEdges:
    ('a, 'b) graph
    -> (edge * 'b * 'c -> 'c)
    -> 'c
    -> 'c

val computeSCCGraph:
    ('a, 'b) graph
    -> (node list, unit) graph

val toDot:
    ('a -> string) option
    -> ('b -> string) option
    -> ('a, 'b) graph
    -> string
    -> unit

end



signature GRAPH_EXPLORER = sig

datatype continue =
	 STOP
       | VISIT
       | DONT_VISIT

type 'a waitingset

structure Graph: GRAPH

type ('a, 'b, 'c) explorer_hooks = {
     nodeHook   : Graph.node * 'a * 'c -> 'c * continue,
     edgeHook   : Graph.edge * 'b * 'c -> 'c * continue,
     enterWSHook: Graph.node * 'a * 'c -> 'c,
     leaveWSHook: Graph.node * 'a * 'c -> 'c
}

val explore:
    ('a, 'b) Graph.graph
    -> ('a, 'b, 'c) explorer_hooks
    -> 'c
    -> Graph.node list
    -> 'c

end



signature GRAPH_MODEL = sig

include MODEL

eqtype original_state
eqtype original_event

val unpack_state : state -> original_state
val unpack_event : event -> original_event

end



signature GRAPH_EXPLORATION = sig

include SIMPLE_EXPLORATION

end



signature GRAPH_GENERATOR = sig

type 'a storage
type state
type event
type init_options

structure Graph: GRAPH

val genGraph:
    (state * event list) list
    -> 'a storage
    -> init_options
    (*  node tagging function  *)
    -> (state * event list -> 'b)
    (*  edge tagging function  *)
    -> (((state * event list) * event * (state * event list)) -> 'c)
    -> 'a storage * ('b, 'c) Graph.graph

end



signature STATE_SPACE_GRAPH = sig

structure Graph: GRAPH
type node
type edge
     
val graph: (node, edge) Graph.graph

end
