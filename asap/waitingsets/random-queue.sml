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
 *  warning: two successive calls of head or peek may not return the same item
 *)

structure RandomQueue:WAITINGSET = struct

datatype 'a waitingset = Q of { top: int,
				v  : 'a option Vector.vector option }

exception Dequeue

val rnd = Random.rand (17, 83)

val empty = Q { top = 0,
		v   = NONE }

fun isEmpty (Q { top = 0, v }) = true
  | isEmpty _ = false

fun enqueue (Q { top = 0, v = NONE }, item) =
    Q { top = 1,
	v   = SOME (Vector.fromList [ SOME item ]) }
  | enqueue (Q { top, v = SOME v }, item) =
    if Vector.length v = top
    then Q { top = top + 1,
	     v   = SOME (Vector.tabulate (if top = 0
					  then 1
					  else top * 2,
					  (fn i => if i = top
						   then SOME item
						   else if i < top
						   then Vector.sub (v, i)
						   else NONE))) }
    else Q { top = top + 1,
	     v   = SOME (Vector.update (v, top, SOME item)) }
  | enqueue  (Q { top, v = NONE }, _) = raise LibBase.Impossible ""
	     
fun dequeue (Q { top = 0, v }) = raise Dequeue
  | dequeue (Q { top, v = NONE }) = raise Dequeue
  | dequeue (Q { top, v = SOME v }) = let
	val r = Random.randRange (0, top - 1) rnd
	val item = valOf (Vector.sub (v, r))
	val topItem = Vector.sub (v, top - 1)
	val v = Vector.update (v, top - 1, NONE)
	val v = Vector.update (v, r, topItem)
    in
	(Q { top = top - 1, v = SOME v }, item)
    end

fun head (Q { top = 0, v }) = raise Dequeue
  | head (Q { top, v = NONE }) = raise Dequeue
  | head (Q { top, v = SOME v }) =
    valOf (Vector.sub (v, Random.randRange (0, top - 1) rnd))

fun peek (Q { top = 0, v }) = NONE
  | peek (Q { top, v = NONE }) = NONE
  | peek (Q { top, v = SOME v}) =
    Vector.sub (v, Random.randRange (0, top - 1) rnd)

fun length (Q { top, v }) = top

end
