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
(* File: time.sml
 *
 * Data structures for token time stamp types.
 *)

nonfix ^^;
val ^^ = op @;
infix 5 ^^;
infix 9 @;

functor CPN'IntTime(val start: int): CPN'TIME = struct

    type time = int

    datatype 'a timed = @ of 'a * time

    val start_time = SOME start
    val model_time = ref start

    val name = "int"
    val null = 0
    val null_str = "0"

    val mkstr = Int.toString
    val toString = Int.toString
    val maketime = fn t => (valOf(Int.fromString t)) handle Option =>
		             (raise CPN'Error ("maketime: not an integer: "^t))
    val fromString = Int.fromString
    val fromInt = fn i => i
    fun fromIntInf ii = IntInf.toInt ii
    val toReal = Real.fromInt

    fun ready (t: time) = t <= (!model_time)
    fun time () = !model_time

    val add = Int.+
    val sub = Int.-
    val mult = Int.*
    val lt  = Int.<
    val leq = Int.<=
    val cmp = Int.compare

    fun col (c@_) = c

end

functor CPN'IntInfTime(val start: IntInf.int): CPN'TIME = struct

    type time = IntInf.int

    datatype 'a timed = @ of 'a * time

    val start_time = SOME start
    val model_time = ref start

    val name = "intinf"
    val null = IntInf.fromInt 0
    val null_str = "0"

    val mkstr = IntInf.toString
    val toString = IntInf.toString
    fun maketime t =
	case IntInf.fromString t of
	    SOME value => value
	  | NONE => raise CPN'Error ("maketime: not an IntInf : "^t)
    val fromString = IntInf.fromString
    val fromInt = IntInf.fromInt
    fun fromIntInf ii = ii

    fun toReal (t:time) = Real.fromLargeInt(IntInf.toLarge t)
	handle Overflow => case (Real.fromString(IntInf.toString t)) of
                               SOME r => r
                             | NONE => raise Overflow

    fun ready (t: time) = IntInf.<=(t,!model_time)
    fun time () = !model_time

    val add = IntInf.+
    val sub = IntInf.-
    val mult = IntInf.*
    val lt  = IntInf.<
    val leq = IntInf.<=
    val cmp = IntInf.compare

    fun col (c@_) = c

end

(* Function to encode reals into IntInf representation in fixed precision.
 * The arg p is the precision, i.e., how many significant digits after 
 * the point ('.'). *)
exception RealToIntInfExn;
fun RealToIntInf p r
    = let
          fun rmdot (#"."::r) = r
            | rmdot (c::r) = c::(rmdot r)
            | rmdot [] = [];
          fun removedot st = String.implode (rmdot (String.explode st))
      in
          if p >= 0 then
              case IntInf.fromString (removedot 
                                      (Real.fmt 
                                       (StringCvt.FIX (SOME p)) r))
                of SOME x => x
                 | NONE => raise InternalError "Internal error in RealToIntInf"
          else
              raise RealToIntInfExn
      end;

(* Function to decode IntInf to reals, given the precision p (see above). *)
exception IntInfToRealExn;
fun IntInfToReal p ii
    = let
          fun indot 0 [] = [#".",#"0"]
            | indot n [] = #"0"::(indot (n-1) [])
            | indot 0 r = #"."::r
            | indot n (h::r) = h::(indot (n-1) r)
          fun insertdot st
              = String.implode (List.rev 
                                (indot p 
                                 (List.rev 
                                  (String.explode st))))
      in
          if p >= 0 then
              case Real.fromString (insertdot (IntInf.toString ii))
                of SOME x => x
                 | NONE => raise IntInfToRealExn
          else
              raise IntInfToRealExn
      end;

functor CPN'RealTime(val start: real): CPN'TIME = struct

    type time = real

    datatype 'a timed = @ of 'a * time

    val start_time = SOME start
    val model_time = ref start

    val name = "real"
    val null = 0.0
    val null_str = "0.0"
	
    val mkstr = Real.toString
    val toString = Real.toString
    val maketime = fn t => (valOf(Real.fromString t)) handle Option =>
                                 (raise CPN'Error ("maketime: not a real: "^t))
    val fromString = Real.fromString
    val fromInt = Real.fromInt
    fun fromIntInf ii = IntInfToReal 0 ii
    val toReal = (fn r => r)

    fun ready (t: time) = t <= (!model_time)
    fun time () = !model_time
	
    val add = Real.+
    val sub = Real.-
    val mult = Real.*
    val lt  = Real.<
    val leq = Real.<=
    val cmp = Real.compare

    fun col (c@_) = c

end

structure CPN'UnitTime: CPN'TIME = struct

    type time = unit

    datatype 'a timed = @ of 'a * time
	
    val start_time = NONE
    val model_time = ref ()
	
    val name = "unit"
    val null = ()
    val null_str = ""
	
    fun mkstr _ = ""
    fun toString _ = ""
    fun maketime _ = ()
    fun fromString _ = SOME ()
    fun fromInt _ = ()
    fun fromIntInf _ = ()
    fun toReal _ = raise InternalError "Cannot convert unit time to real"
	
    fun ready (t: time) = true
    fun time () = !model_time
	
    val add = fn _ => ()
    val sub = fn _ => ()
    val mult = fn _ => ()
    val lt  = fn _ => false
    val leq = fn _ => true
    val cmp = fn _ => EQUAL

    fun col (c@_) = c

end

