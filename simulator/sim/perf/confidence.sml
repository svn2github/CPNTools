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
(* File: perf/confidence.sml
 *
 * Facilities for calculating confidence intervals.
 *)

exception ConfidenceIntervalExn;

structure CPN'ConfidenceInterval = struct 

(*** Critical points for the t distribution ***)

(*  For 90% confidence intervals, alpha=0.1,
    critical points t_{nu,gamma} with nu degrees of freedom
    and gamma = 1-alpha/2
*)
val tCriticalPoints_095 =
    [0.00, 6.314, 2.920, 2.353, 2.132, 2.015, 1.943, 1.895, 1.860, 
     1.833, 1.812, 1.796, 1.782, 1.771, 1.761, 1.753, 1.746, 1.740, 
     1.734, 1.729, 1.725, 1.721, 1.717, 1.714, 1.711, 1.708, 1.706, 
     1.703, 1.701, 1.699, 1.697, 1.684, 1.676, 1.665, 1.660];
    
(*  For 95% confidence intervals, alpha=0.05,
    critical points t_{nu,gamma} with nu degrees of freedom
    and gamma = 1-alpha/2
*)
val tCriticalPoints_0975 =
    [0.00, 12.706, 4.303, 3.182, 2.776, 2.571, 2.447, 2.365, 2.306, 
     2.262, 2.228, 2.201, 2.179, 2.160, 2.145, 2.131, 2.120, 2.110, 
     2.101, 2.093, 2.086, 2.080, 2.074, 2.069, 2.064, 2.060, 2.056, 
     2.052, 2.048, 2.045, 2.042, 2.021, 2.009, 1.992, 1.984];

(*  For 99% confidence intervals, alpha=0.01,
    critical points t_{nu,gamma} with nu degrees of freedom
    and gamma = 1-alpha/2
*)
val tCriticalPoints_0995 =
    [0.00, 63.657, 9.925, 5.841, 4.604, 4.032, 3.707, 3.499, 3.355, 
     3.250,3.169, 3.106, 3.055, 3.012, 2.977, 2.947, 2.921, 2.898, 
     2.878, 2.861, 2.845, 2.831, 2.819, 2.807, 2.797, 2.787, 2.779, 
     2.771, 2.763, 2.756, 2.750, 2.704, 2.678, 2.643, 2.626];

fun getCriticalPoint(percent, df) = 
    let
	val _ = if df<1 then raise ConfidenceIntervalExn else ()
	val critpoints = 
	    case percent of 
		90 => tCriticalPoints_095
	      | 95 => tCriticalPoints_0975
	      | 99 => tCriticalPoints_0995
	      | _ => raise CPN'Error ("No support for "^
				      Int.toString percent^
				      " confidence intervals!")
    in
	if df >= 1 andalso df <= 29
	    then List.nth(critpoints,df)
	else if df>=30 andalso df<=39
		 then List.nth(critpoints,30)
	     else if df>=40 andalso df<=49
		 then List.nth(critpoints,31)
	     else if df>=50 andalso df<=74
		 then List.nth(critpoints,32)
	     else if df>=75 andalso df<=99
		 then List.nth(critpoints,33)
	     else List.nth(critpoints,34)
    end 

(* 
fun calculateRUSV (rusv, percent) = 
    let
	val meanEstimate = CPN'RUSV.avrg(rusv)
	val n = CPN'RUSV.count(rusv)
	val sampleVariance = CPN'RUSV.vari(rusv)
	val sqrtValue = Math.sqrt(sampleVariance/(real n))
	val critPoint = getCriticalPoint(percent, n-1)
	val halfLength = critPoint*sqrtValue
    in
	{mean = meanEstimate, 
	 halfLength = halfLength}
    end;

fun calculateIUSV (iusv, percent) = 
    let
	val meanEstimate = CPN'IUSV.avrg(iusv)
	val n = CPN'IUSV.count(iusv)
	val sampleVariance = CPN'IUSV.vari(iusv)
	val sqrtValue = Math.sqrt(sampleVariance/(real n))
	val critPoint = getCriticalPoint(percent, n-1)
	val halfLength = critPoint*sqrtValue
    in
	{mean = meanEstimate, 
	 halfLength = halfLength}
    end;
*)
fun calc (meanEstimate:real, count:int, sampleVariance:real, percent) =
    if count<=1
    then 	
	{percentage=percent,
	 half_length = NONE ,
	 avrg = meanEstimate, 
	 upper_endpoint = NONE ,
	 lower_endpoint = NONE}
    else let
	val sqrtValue = Math.sqrt(sampleVariance/(real count))
	val critPoint = getCriticalPoint(percent, count-1)
	val halfLength = critPoint*sqrtValue
    in
	{percentage=percent,
	 half_length = SOME halfLength,
	 avrg = meanEstimate, 
	 upper_endpoint = SOME (meanEstimate - halfLength),
	 lower_endpoint = SOME (meanEstimate + halfLength)}
    end

end (* struct *)

