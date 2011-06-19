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
structure Regression = struct
  local
  open Real
  fun student_discrete 0 = 0.0
    | student_discrete 1 = 12.71
    | student_discrete 2 = 4.303
    | student_discrete 3 = 3.182
    | student_discrete 4 = 2.776
    | student_discrete 5 = 2.571
    | student_discrete 6 = 2.447
    | student_discrete 7 = 2.365
    | student_discrete 8 = 2.306
    | student_discrete 9 = 2.262
    | student_discrete 10 = 2.228
    | student_discrete 11 = 2.201
    | student_discrete 12 = 2.179
    | student_discrete 13 = 2.160
    | student_discrete 14 = 2.145
    | student_discrete 15 = 2.131
    | student_discrete 16 = 2.120
    | student_discrete 17 = 2.110
    | student_discrete 18 = 2.101
    | student_discrete 19 = 2.093
    | student_discrete 20 = 2.086
    | student_discrete 21 = 2.080
    | student_discrete 22 = 2.074
    | student_discrete 23 = 2.069
    | student_discrete 24 = 2.064
    | student_discrete 25 = 2.060
    | student_discrete 26 = 2.056
    | student_discrete 27 = 2.052
    | student_discrete 28 = 2.048
    | student_discrete 29 = 2.045
    | student_discrete 30 = 2.042
    | student_discrete 40 = 2.021
    | student_discrete 50 = 2.009
    | student_discrete 60 = 2.000
    | student_discrete 80 = 1.990
    | student_discrete 100 = 1.984
    | student_discrete 120 = 1.980
    | student_discrete _ = 1.960

  fun student n =
    if Int.<= (n, 30)
    then student_discrete n
    else if Int.<= (n, 60)
    then
      let
        val down = n div 10
        val error = Int.-(n, Int.* (down, 10))
        val error = (fromInt error) / 10.0
        val low = student_discrete (Int.* (down, 10))
        val high = student_discrete (Int.+ (Int.* (down, 10), 10))
      in
        (1.0 - error) * low + error * high
      end
    else if Int.<= (n, 140)
    then
      let
        val down = n div 20
        val error = Int.-(n, Int.* (down, 20))
        val error = (fromInt error) / 20.0
        val low = student_discrete (Int.* (down, 20))
        val high = student_discrete (Int.+ (Int.* (down, 20), 20))
      in
        (1.0 - error) * low + error * high
      end
    else student_discrete n

    fun zip [] [] = []
      | zip (a::aa) (b::bb) = (a, b)::(zip aa bb)
      | zip _ _ = []


      structure Helpers = struct
  structure Matrix = DenseMatrix(structure Number = RealNumber)
  fun log_function x = [Math.ln x / Math.ln 2.0]
  fun exp_function x = [Math.pow (2.0, x)]
  fun linear_function (x : real) = [x]
  fun polynomium1_function (x : real) = [x]
  fun polynomium2_function x = [x * x]
  fun polynomium3_function x = [x * x * x]
  fun polynomium4_function x = [x * x * x * x]
  fun polynomium2_linear_function x = [x, x * x]
  fun polynomium3_linear_function x = [x, x * x * x]
  fun polynomium4_linear_function x = [x, x * x * x * x]
      end

      open Helpers

  fun build_value_vector values = List.map (fn (x, y) => y) values

  fun build_from_function f values =
    Matrix.fromList (List.map (fn (x, y) => 1.0::(f (fromInt x))) values)
  in
  type value_with_confidense = real * (real * real)
  type regression_result = (real * real * Matrix.matrix) * value_with_confidense list
  type xy_pairs = (int * real) list

  structure Helpers = Helpers

  fun regression' y X : regression_result =
  let
    val n' = List.length y
    val n = fromInt n'
    val p' = Matrix.nCols X
    val p = fromInt p'
    val Yt = Matrix.fromList [y]
    val Y = Matrix.trans Yt
    val Xt = Matrix.trans X
    val dispersion = Matrix.inv (Matrix.* (Xt, X))
    val bt = Matrix.* (Matrix.* (dispersion, Xt), Y)
    val b = Matrix.trans bt
    val s = List.hd (Matrix.toList (Matrix.trans (Matrix.fromDiag dispersion)))
    val Sx = List.foldl Real.+ 0.0 y
    val tmp = Matrix.* (Matrix.* (b, Xt), Y)
    val ESS = List.hd (List.hd (Matrix.toList (Matrix.- (Matrix.* (Yt, Y), tmp))))
    val SSR = (List.hd (List.hd (Matrix.toList tmp))) - Sx * Sx / n
    val TSS = ESS + SSR
    val sigma = Math.sqrt (ESS / (n - p))
    val student = student (Int.- (n', p'))
    val factor = sigma * student
    val blist = List.hd (Matrix.toList b)
    val zipped = zip blist s
    val result =
      List.map 
      (fn (b, s) => (b, (b - (Math.sqrt s) * factor, b + (Math.sqrt s) *
      factor)))
      zipped
    val R = SSR / TSS
  in
    ((R, factor, dispersion), result)
  end

  fun regression f (values : xy_pairs) =
  let
    val y = build_value_vector values
    val X = build_from_function f values
  in
    regression' y X
  end

  fun linear_regression (values : xy_pairs) =
    regression Helpers.linear_function values

  fun polynomial_regression (values : xy_pairs) =
  let
    val y'= build_value_vector values
    val y = List.map (fn x => Math.ln x) y'
    val X = build_from_function Helpers.log_function values
  in
    regression' y X
  end

  fun exponential_regression (values : xy_pairs) =
  let
    val y'= build_value_vector values
    val y = List.map (fn x => Math.ln x) y'
    val X = build_from_function Helpers.linear_function values
  in
    regression' y X
  end

  fun estimate f (((R, factor, dispersion), result) : regression_result) value : value_with_confidense * real =
  let
    val factors = List.map (fn (x, y : real * real) => x) result
    val constants = 1.0::(f value)
    val zipped = zip factors constants
    val value = 
      List.foldl (fn ((factor, constant), rest) => factor * constant + rest) 0.0 zipped
    val xt = Matrix.fromList [constants]
    val x = Matrix.trans xt
    val tmp = List.hd (List.hd (Matrix.toList (Matrix.* (Matrix.* (xt, dispersion), x))))
    val error = factor * (Math.sqrt (tmp))
(*    val error' = factor * (Math.sqrt (1.0 + tmp))*)
  in
    ((value, (value - error, value + error)), R)
  end

  fun linear_estimate result value : value_with_confidense * real =
    estimate Helpers.linear_function result value

  fun polynomial_estimate result value : value_with_confidense * real =
  let
    val ((value, (value', value'')), R) = 
      estimate Helpers.log_function result value
  in
    ((Math.exp value, (Math.exp value', Math.exp value'')), R)
  end

  fun exponential_estimate result value : value_with_confidense * real =
  let
    val ((value, (value', value'')), R) = 
      estimate Helpers.linear_function result value
  in
    ((Math.exp value, (Math.exp value', Math.exp value'')), R)
  end
  end

open Helpers

  val values =
    [(68,188.7),(62,184.2),(49,151.3),(62,182.4),(34,138.3),(66,181.8),(45,155.1),(60,178.3),(57,179.4),(57,171.5),(63,177.9),(56,172.5),(57,169.3)]
  val values2 =
    [(58, 115.0), (59, 117.0), (60, 120.0), (61, 123.0), (62, 126.0), (63,
    129.0), (64, 132.0), (65, 135.0), (66, 139.0), (67, 142.0), (68, 146.0),
    (69, 150.0), (70, 154.0), (71, 159.0), (72, 164.0)]

fun test f value =
let
  val result = regression f values2
in
  estimate f result value
end
fun test' value =
let
  val result = polynomial_regression values2
in
  (result, polynomial_estimate result value)
end
fun test'' value =
let
  val result = exponential_regression values2
in
  (result, exponential_estimate result value)
end
val narko1 = test log_function 80.0
val narko2 = test exp_function 80.0
val narko3 = test polynomium1_function 80.0
val narko3' = test' 80.0
val narko3'' = test'' 80.0
val narko4 = test polynomium2_function 80.0
val narko5 = test polynomium3_function 80.0
val narko6 = test polynomium4_function 80.0
val narko7 = test polynomium2_linear_function 80.0
val narko8 = test polynomium3_linear_function 80.0
val narko9 = test polynomium4_linear_function 80.0

end
