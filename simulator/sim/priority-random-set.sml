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
(* File: priority-random-set.sml
 *
 * Priority random set for firing only the transitions with highest priority.
 *)

(*
 * Placeholder for the Random struct. Only used when this file is checked in isolation.
structure CPN'Random = struct
fun int _ = 0
end
 *)

(*
 * Implementation of a random set of integers. Copied from sim.ml.
 *)
structure RS = struct
local
    (*
     * Using Array seems to limit the size of the array to the initial size.
     * Therefore, use DynamicArray to be able to extend the array.
     *)
    open DynamicArray;
in
    (*
     * The type ranset holds the random set.
     *)
    type ranset = {
        (*
         * The random set.
         *)
        set: int array,
        (*
         * Only the indices [0,...,size-1] in the set are valid.
         *)
        size: int,
        (*
         * The reverse set, that is, if set[x] = y (and 0 <= x < size), then index[y] = x.
         * This enables easy lookup when deleting some element.
         *)
        index: int array
    }

    (*
     * Exception thrown when trying to take a random element from an empty set.
     * To be handled by the environment.
     *)
    exception EmptyRanSet;
    
    (*
     * There seems to be a problem with initializing an empty dynamic array.
     * Therefore, we initialize these array as non-empty, but we declare the size to be 0.
     *)
    fun create 0 = {
            set = tabulate (1, fn i => ~1, ~1), 
            size = 0,
            index = tabulate (1, fn i => ~1, ~1)}
      | create n = {
            set = tabulate (n, fn i => i, ~1), 
            size = n,
            index = tabulate (n, fn i => i, ~1)}

    (*
     * Insert an element i into a random set rs.
     *)
    fun insert (rs as ref {set, size, index}, i) = (
        (*
         * Insert i at the next free location (that is, set[size]).
         *)
        update (set, size, i); 
        (*
         * Update the index: index[i] = size.
         *)
        update (index, i, size);
        (*
         * Change rs and increase its size.
         *)
        rs := {
            set = set,
            size = size + 1,
            index = index
        }
    )

    (*
     * Delete an element i from a random set rs.
     *)
    fun delete (rs as ref {set, size, index}, i) = 
    let
        (*
         * Get the position of i in the set.
         *)
        val j = sub (index, i)
        (*
         * Get the last element from the set.
         *)
        val k = sub (set, size - 1)
    in 
        if j >= 0 andalso k >= 0
        then (
            (*
             * Move the last element from the set to the position of the element-to-be-deleted.
             * This replaces the element i by the element k in the set.
             *)
            update (set, j, k);
            (*
             * Update the index of element k.
             *)
            update (index, k, j);
            (*
             * Change rs and decrease its size (which effectively rewmoves the original element k).
             *)
            rs := {
                set = set,
                size = size - 1,
                index = index
            }
        )
        else
            (*
             * Problem with element i, it seems not present. Present random set should do.
             *)
            () 
    end

    (*
     * Take a random element from the random set.
     *)
    fun ran ({size = 0, ...}) = 
        (*
         * Trying to take a random element from an empty set. Throw exception.
         *)
        raise EmptyRanSet 
      | ran ({set ,size, index = _}) = 
        (*
         * Take a random element from the set.
         * Assumption: "CPN'Random.int size" returns a uniform distributed stochast on [0, ..., size-1].
         *)
        sub (set, CPN'Random.int size)

    (*
     * Returns the size of the random set.
     *)
    val size: ranset -> int = #size
    
end (* local *)
end; (* structure RS *)

(*
 * Priority structure used in Priority Queue below.
 *)
structure RSPrio = struct
    (*
     * Type of the priority.
     *)
    type priority = int
    (*
     * How to compare two priorities.
     *)
    fun compare (n, m) = Int.compare (m, n)
    (*
     * Items stored in the priority queue: A pair (priority, random set).
     *)
    type item = int * RS.ranset ref
    (*
     * How to get the priority from an item.
     *)
    fun priority(priority, _) = priority 
end

(*
 * Priority Queue of Random Sets. 
 *)
structure PQRS = struct
    (*
     * The priority queue structure.
     *)
     
    structure Q = LeftPriorityQFn (RSPrio)
    (*
     * The ranset type. We use the same name as in the RS structure for sake of convenience.
     *)
    type ranset = {
        (*
         * The prioritized queue with transitions that do not have highest priority.
         * Enables easy lookup for the next highest priority and the corresponding transitions.
         *)
        queue: Q.queue,
        (*
         * The mapping from priority to transitions. Enables easy lookup for the random set of 
         * transitions given some priority.
         *
         * Note that the queue and the vector contain the same random sets, that is, adding an element
         * to a set in the vector also addes the element in the queue and v.v.
         *)
        vector: RS.ranset ref option DynamicArray.array,
        (*
         * The random set of transitions with highest priority.
         *)
        head: RS.ranset ref,
        (*
         * The highest priority.
         *)
        headprio: int,
        priofun: int -> int
    } ref
    
    (*
     * Throws the same exception as the random set does.
     *)
    exception EmptyRanSet = RS.EmptyRanSet
        (*
     * Balance the priotized random set. As a result of this, the head is empty iff the entire set is empty.
     *)
    fun balance (rs as ref{queue, vector, head, headprio, priofun}) =
    let
        (*
         * Helper function that 'pops the queue' as long as the head is empty but the queue is not.
         *)
        fun balance' (queue, head, headprio) = 
            if RS.size(!head) > 0 orelse Q.isEmpty queue
            then (queue, head, headprio)
            else 
                (*
                 * Head is empty. Pop the queue.
                 *)
                let
                    (*
                     * Remove this priority from the vector, as there are no transitions with this 
                     * priority anymore.
                     *)
                    val _ = DynamicArray.update(vector, headprio, NONE)
                    val (((poppedHeadprio, poppedHead), poppedQueue)) = Q.remove(queue)
                in
                    balance' (poppedQueue, poppedHead, poppedHeadprio)
                end
        val (balancedQueue, balancedHead, balancedHeadprio) = balance' (queue, head, headprio)
            
    in 
        (*
         * Update the prioritzed random set.
         *)
        rs := {
            queue = balancedQueue, 
            vector = vector, 
            head = balancedHead, 
            headprio = balancedHeadprio,
            priofun = priofun
        };
        (*
         * And return it (dereferenced).
         *)
        !rs
    end
    
    fun hasTopPriority (rs as ref{queue, vector, head, headprio, priofun}, i) =
    let
        val priority = priofun i
        val _ = balance(rs)
        val newHeadprio = (#headprio (!rs))
    in
        priority = newHeadprio
    end
    
    (*
     * Insert a transition i into the prioritized random set.
     *)
    fun insert (rs as ref{queue, vector, head, headprio, priofun}, i) =
    let
        (*
         * Get the priority of the transition to be inserted.
         *)
        val priority = priofun i
        (*
         * Get the random set to insert this transition in, that is, the random set
         * that contains all transition with the same priority.
         *)
        val (ranset, queue) = (Option.valOf (DynamicArray.sub (vector, priority)), queue) 
            handle _ => 
                (*
                 * This is the first transition with this priority.
                 * Create a fresh random set and insert it in the both the queue and the vector.
                 *)
                let
                    (*
                     * Create fresh random set.
                     *)
                    val freshRanset = ref (RS.create 0)
                    (*
                     * Insert it into the queue.
                     *)
                    val extendedQueue = Q.insert ((priority, freshRanset), queue)
                    (*
                     * Insert it into the vector.
                     *)
                    val _ = DynamicArray.update (vector, priority, SOME freshRanset)
                in (
                    freshRanset, 
                    extendedQueue
                ) 
                end
        (*
         * The transition may have a higher priority then all exisiting transitions in the prioritized random set.
         * Deal with this possibility.
         *
         * Note that the higher the priority, the lower the number: Priority 0 exceeds priority 5.
         *
         * Also note that by now the new transition has already been added to the queue.
         *)
        val (newHeadprio, newHead, newQueue) = 
            if headprio > priority 
            then 
                (*
                 * New priority exceeds head priority.
                 * Recompute head priority, head, and queue.
                 *)
                let
                    (*
                     * Get the new head from the queue (should contain new priority and random set containing
                     * only the transition i).
                     *)
                    val ((poppedHeadprio, poppedHead), poppedQueue) = Q.remove(queue)
                    (*
                     * Move the former head back into the queue (balance should remove it if it is empty).
                     *)
                    val newQueue = Q.insert((headprio, head), poppedQueue) 
                in (
                    poppedHeadprio, 
                    poppedHead, 
                    newQueue
                )
                end 
            else (
                (*
                 * New priority is lower than head priority, no changes needed.
                 *)
                headprio, 
                head, 
                queue
            )
            (*
             * Insert transiton i in the fresh random set.
            *)
            val _ = RS.insert(ranset, i)
    in
        (*
         * Update the head, headprio, and queue to the new settings.
         *)
        rs := {
            queue = newQueue, 
            vector = vector, 
            head = newHead, 
            headprio = newHeadprio,
            priofun = priofun
        }
    end
    
    (*
     * Creates a prioritized queue for the transition [0, ..., n-1], assuming that
     * prioFun t returns the priority for transition t.
     *
     * This function first constructs the vector, and then constrcuts the queue from this
     * vector.
     *)
    fun create 0 prioFun =
        (*
         * Create an emmpty prioritized queue.
         *)
        let
            (*
             * Create an empty head.
             *)
            val empyHead = ref (RS.create 0)
            (*
             * Default priority is 0.
             *)
            val defaultPrio = 0
            (*
             * Create a vector (here we use that the first element in this vector is at index 0,
             * that is, at the default priority.
             *)
            val emptyVector = DynamicArray.fromList ([SOME empyHead], NONE)
            (*
             * Create an empty queue.
             *)
            val emptyQueue = Q.empty
        in {
            head = empyHead, 
            headprio = defaultPrio, 
            vector = emptyVector, 
            queue = emptyQueue,
            priofun = prioFun
        }
        end
      | create n prioFun =
        (*
         * Create a prioritized queue for the first n ([0, ..., n-1]) transitions.
         *)
        let
            (*
             * First, create an empty prioritized random set.
             *)
            val rs = ref (create 0 prioFun)
            (*
             * Second, insert all transitions into this empty prioritized random set.
             *)
            val _ = List.tabulate(n, fn t => insert(rs, t))
        in 
            (*
             * Return the (dereferenced) created prioritized random set.
             *)
            !rs
        end
    
    (*
     * Delete transition i from the prioritzed random set.
     *
     * Assumption: i is an element of the head, or if the head is empty, an element of the first element in the queue.
     *)
    fun delete (rs :ranset as ref { vector, priofun, ...}, i) =
        (*
         * Delete transition i from the entry with the.
         *)
        RS.delete(Option.valOf (DynamicArray.sub(vector, priofun i)), i)
    
    (*
     * Returns a random transition with highest priority from the prioritized random set.
     *)
    fun ran(rs) =
    let
        (*
         * Balance the queue.
         *)
        val balancedRs = balance(rs)
    in
        (*
         * Get a random transition from the head.
         * Note that the head is empty iff the entire reandom set is empty.
         *)
        RS.ran(!(#head balancedRs))
    end

    (*
     * Returns the number of transitions with highest priority in the prioritized random set.
     *)
    fun size(rs) =
    let
        (*
         * Balance the queue.
         *)
        val balancedRs = balance(rs)
    in
        (*
         * Return the size of the head.
         *)
        RS.size(!(#head balancedRs))
    end

end

fun CPN'dynArrayToList array =
    #2 (DynamicArray.foldr (fn (elt, (n, list)) => (n-1, (n-1, elt)::list)) (DynamicArray.bound array, [(DynamicArray.bound array, DynamicArray.sub (array, (DynamicArray.bound array)))]) array)
    
(*
 * Test functions.
fun priofun n = n div 10

val myPQRS = ref(PQRS.create 0 priofun)
val myPQRS' = foldr (fn (n, pqrs) => (PQRS.insert(pqrs, n); pqrs)) myPQRS [20, 15, 30, 5, 0, 22, 37, 14, 2] 

fun myPrint pqrs = 
let
    val t = PQRS.ran(pqrs)
    val _ = print (Int.toString t)
    val _ = print "\n"
    val _ = PQRS.delete(pqrs, t)
in
    myPrint pqrs;
    ()
end;


val _ = myPrint(myPQRS') handle _ => () 
val myPQRS'' = ref(PQRS.create 20 priofun)
val myPQRS''' = foldl (fn (n, pqrs) => (PQRS.insert(pqrs, n); pqrs)) myPQRS'' [20, 30, 37, 0, 5] 

fun priofun' 0 = 1000 | priofun' 1 = 1 | priofun' 2 = 99 | priofun' _ = ~1

val dunno = ref(PQRS.create 0 priofun');
dunno;
val _ = PQRS.insert(dunno, 0);
val _ = PQRS.insert(dunno, 1);
val _ = PQRS.insert(dunno, 2);
dunno;
val _ = PQRS.delete(dunno, 0);
val _ = PQRS.delete(dunno, 2);
dunno;
val _ = PQRS.delete(dunno, 1);
dunno;
val _ = PQRS.insert(dunno, 0);
val _ = PQRS.insert(dunno, 1);
val _ = PQRS.insert(dunno, 2);
dunno;
val _ = PQRS.delete(dunno, 1);
val _ = PQRS.delete(dunno, 2);
dunno;
val _ = PQRS.delete(dunno, 0);
dunno;
val _ = PQRS.insert(dunno, 0);
val _ = PQRS.insert(dunno, 1);
val _ = PQRS.insert(dunno, 2);
dunno;
val _ = PQRS.delete(dunno, 0);
val _ = PQRS.delete(dunno, 2);
dunno;
val _ = PQRS.delete(dunno, 1);
dunno;
val _ = PQRS.insert(dunno, 0);
val _ = PQRS.insert(dunno, 1);
val _ = PQRS.insert(dunno, 2);
dunno;
val _ = PQRS.delete(dunno, 2);
val _ = PQRS.delete(dunno, 1);
dunno;

val _ = myPrint(myPQRS''') handle _ => () 
 *)

val dynArray = DynamicArray.array (0, ~1)
val _ = DynamicArray.update(dynArray, 0, 0)
val _ = DynamicArray.update(dynArray, 1, 1)
val _ = DynamicArray.update(dynArray, 2, 2);
val _ = DynamicArray.update(dynArray, 3, 3);
CPN'dynArrayToList dynArray; 
