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
structure Estimator =
struct
    local
    fun estimate' start combinator combinator' accuracy limit f parameter =
    let
        val times =
            IntHashTable.mkTable (10, LibBase.NotFound)
        fun test_limit f n = 
        let
            val _ = SMLofNJ.Internals.GC.doGC 100
            val start = Time.now()
            val result = f limit (parameter n)
            val stop = Time.now()
            val _ = IntHashTable.insert times (n, (Time.-(stop, start),
            Option.valOf result))
        in
            ()
        end
        fun upper_bound n m =
        let
            val _ = print "Estimating upper bound, "
          val _ = print (Int.toString n)
          val _ = print ": "
        in
          (test_limit f n; print (Time.toString (#1 (IntHashTable.lookup times
          n))); print "\n";
             upper_bound (combinator n (IntHashTable.listItemsi times) limit) n)
            handle Option.Option => (print "Failed\n"; (m, n))
        end
        fun bisection n m last =
        let
            val _ = print "Seeking exact bound ["
            val _ = print (Int.toString n)
            val _ = print ", "
            val _ = print (Int.toString m)
            val _ = print "), "
            val r = combinator' n m (IntHashTable.listItemsi times) limit last
            val _ = print (Int.toString r)
            val _ = print ": "
        in
            if m - n <= accuracy
            then (print "Done!\n"; n)
            else (let
                   val _ = test_limit f r
                   val (time, _) = IntHashTable.lookup times r
                   val _ = print (Time.toString time)
                   val _ = print "\n"
                 in
                   bisection r m (SOME (Time.toReal time))
                 end)
                 handle Option.Option => (print "Failed\n"; bisection n r NONE)
        end
        val (lowerbound, upperbound) = upper_bound start 1
    in
      (bisection lowerbound upperbound ((SOME (Time.toReal (#1
      (IntHashTable.lookup times lowerbound)))) handle _ => NONE),
         IntHashTable.listItemsi times)
    end

    in
        fun estimate combinator combinator' accuracy limit f parameter =
            estimate' 1 combinator combinator' accuracy limit f parameter

    fun estimate_2d combinator1 combinator2 combinator1' combinator2' accuracy1 accuracy2 limit f parameter =
    let
        val _ = print "Estimating X coordinate\n"
        val (xbound, points) = estimate combinator1 combinator1' accuracy1 limit f (fn n => parameter n 1)
    in
        (xbound,
        List.map (fn (n, time) => (
        let
            val _ = print (String.concat ["Estimating Y coordinate for X = ", Int.toString n, "\n"])
            val (ybound, points) =
                estimate' (combinator2 1 points limit) combinator2 combinator2' accuracy2 limit f (fn m => parameter n m)
        in
            (n, (ybound, (1, time)::points))
        end)) points)
    end
    end

    fun bisection n m times limit last =
       (m - n) div 2 + n

       local
       open Regression
       in
    fun regression n m times limit last =
    let
      val times' = List.map (fn (x, (t, _)) => (x, Time.toReal t)) times
      val times'' = List.filter (fn (x, t) => Real.>= (t, 1.0)) times'
      fun approximate f =
      let
          fun f' x =
          let
(*            val _ = print "f'("
            val _ = print (Int.toString x)
            val _ = print ") = "*)
            val ((value, _), _) = f (Real.fromInt x)
            (*            val _ = print (Real.toString value)
            val _ = print "\n"*)
          in
            value
          end
        fun bisect n m =
        let
          val r = (m - n) div 2
        in
          if m - n <= 1
            then n
            else if f' (n + r) > limit
                 then bisect n (m - r)
                 else bisect (n + r) m
        end
        val result = 
          bisect n m
      in
        Int.min (m - 1, Int.max (n + 1, result))
      end
    in
      if List.length times'' < 4 orelse
         (not (Option.isSome last)) orelse
         ((Option.valOf last) * 10.0 < limit)
      then
        n + 1
      else
          let
            val linear_result = linear_regression times''
            val ((linear_R, _, _), _) = linear_result
            val polynomial_result = polynomial_regression times''
            val ((polynomial_R, _, _), _) = polynomial_result
            val exponential_result = exponential_regression times''
            val ((exponential_R, _, _), _) = exponential_result
          in
            if linear_R > polynomial_R
            then if linear_R > exponential_R
                 then approximate (linear_estimate linear_result)
                 else approximate (exponential_estimate exponential_result)
            else if polynomial_R > exponential_R
                 then approximate (polynomial_estimate polynomial_result)
                 else approximate (exponential_estimate exponential_result)
          end
    end
      end

    fun estimate_doubling_accurate limit f parameter =
        estimate (fn n => fn _ => fn _ => 2 * n) bisection 1 limit f parameter
    fun estimate_doubling_accurate_intelligent limit f parameter =
        estimate (fn n => fn _ => fn _ => 2 * n) regression 1 limit f parameter
    fun estimate_interval interval limit f parameter =
        estimate (fn n => fn _ => fn _ => n + interval) bisection interval limit f parameter
    fun estimate_doublinginterval_accurate interval limit f parameter =
        estimate (fn n => fn _ => fn _ => if n > interval then n + interval else 2 * n)
        bisection 1 limit f parameter
    fun estimate_doubling_accurate_2d limit f parameter =
        estimate_2d (fn n => fn _ => fn _ => 2 * n) (fn n => fn _ => fn _ => 2 * n) bisection
        bisection 1 1 limit f parameter
    fun estimate_interval_doubling_accurate_2d interval limit f parameter =
        estimate_2d (fn n => fn _ => fn _ => n + interval) (fn n => fn _ => fn _ => 2 * n)
        bisection bisection interval 1 limit f parameter
    fun estimate_interval_doubling_accurate_2d_intelligent interval limit f parameter =
        estimate_2d (fn n => fn _ => fn _ => n + interval) (fn n => fn _ => fn _ => 2 * n)
        regression regression interval 1 limit f parameter

    fun line f str = TextIO.outputSubstr (f, Substring.full (String.^(str, "\n")))
    fun compare ((a, _), (b, _)) = a >= b

    fun write_data_table toString header file (limit, points) =
    let
        val f = TextIO.openOut (String.^(file, ".tbl"))
        val _ = line f (String.^(" Param\tTime\t",header))
        val _ = line f "================================================================="
        fun single (param, (time, data)) =
            line f (String.concat [" ", Int.toString param, "\t", Time.toString
            time, "\t", toString data])
        val _ = List.map single (ListMergeSort.sort compare points)
        val _ = TextIO.closeOut f
    in
        ()
    end

    fun write_data_tables_2d toString header prefix (xlimit, points) =
    let
        fun write_one (xvalue, points) =
            write_data_table toString header (String.concat [prefix, " - x = ",
            Int.toString xvalue]) points
    in
        ignore (List.map write_one points)
    end
end
