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
(* File: untimedstatvar.sml
 *
 * Untimed statistical variables
 *)

structure CPN'IUSV : CPN'UNTIMEDSTATVAR = struct

    type data = IntInf.int
    type Statvar = {count:int,
		    min: data,
		    max: data,
		    first: data, 
		    last: data,
		    sum: data,
		    sqsum: data} option ref
		   
    fun create () = ref NONE: Statvar
	
    fun upd(statvar as ref NONE: Statvar, x:data) =
	statvar:= SOME {count=1,min=x,max=x,first=x,last=x,
			sum=x,sqsum=IntInf.*(x,x)}

      | upd(statvar as ref(SOME{count,min,max,first,last,sum,sqsum}), x) =
	    statvar:= SOME {count= count+1,
			    min= IntInf.min(x,min),
			    max= IntInf.max(x,max),
			    first= first,
			    last= x,
			    sum= IntInf.+(sum, x),
			    sqsum= IntInf.+(sqsum,IntInf.*(x,x))}

    fun intinfToReal (i:IntInf.int) = 
	Real.fromLargeInt(IntInf.toLarge i)
	handle Overflow => case (Real.fromString(IntInf.toString i)) of
                               SOME r => r
                             | NONE => raise Overflow

    fun avrg (ref NONE: Statvar) = 0.0
      | avrg (ref (SOME{sum,count,...})) = intinfToReal sum / real count

    fun ssd (ref NONE: Statvar) = 0.0
      | ssd (ref (SOME{sqsum,count,sum,...})) =
	(* Using max to avoid rouding errors resulting in negative numbers *)
	Real.max (intinfToReal sqsum - intinfToReal sum * intinfToReal sum / real count, 0.0)

    fun vari (ref NONE: Statvar) = 0.0
      | vari (statvar as ref (SOME{count,...})) =
	if count=1 then 0.0 else ssd statvar / real(count-1)
	
    fun std(statvar:Statvar) = Math.sqrt(vari statvar)
	
    fun init(statvar:Statvar) = statvar:= NONE

    local
	val iizero = IntInf.fromInt 0
	fun extract (ref NONE: Statvar) = 
	    {count=0,min=iizero,max=iizero,first=iizero,last=iizero,
	     sum=iizero,sqsum=iizero}
	  | extract (ref (SOME statvar)) = statvar
    in
	val count: Statvar -> int = #count o extract
	val min  : Statvar -> IntInf.int = #min   o extract
	val max  : Statvar -> IntInf.int = #max   o extract
	val first: Statvar -> IntInf.int = #first o extract
	val last: Statvar -> IntInf.int = #last o extract
	val sum  : Statvar -> IntInf.int = #sum   o extract
	val ss   : Statvar -> IntInf.int = #sqsum o extract
    end

    fun ci (statvar:Statvar, percent:int) =
	CPN'ConfidenceInterval.calc (avrg statvar, count statvar, 
				     vari statvar, percent)

    val dataToString = IntInf.toString

fun toStrings (statvar: Statvar) =
    let
	val d = !CPN'PerfOptions.decimaldigits

	val conf_intervals = map (fn p => ci (statvar,p)) 
				 (CPN'PerfOptions.get_ci_percentages())
	val cistrs = 
	    map (fn {percentage,avrg,half_length,
		     upper_endpoint,lower_endpoint} =>
		    {percentage=percentage,
		     avrg = 
		     ((Real.fmt (StringCvt.FIX (SOME d)) avrg)
		      handle Overflow => "Overflow"),
		     half_length=
		     (case half_length of
			  SOME hl => (Real.fmt (StringCvt.FIX (SOME d)) hl
				      handle Overflow => "Overflow")
			| NONE => "Insufficent"),
		     upper_endpoint= 
		     (case upper_endpoint of
			  SOME ue => (Real.fmt (StringCvt.FIX (SOME d)) ue
				      handle Overflow => "Overflow")
			| NONE => "Insufficent"),
		     lower_endpoint= 
		     (case lower_endpoint of 
			  SOME le => (Real.fmt (StringCvt.FIX (SOME d)) le
				      handle Overflow => "Overflow")
			| NONE => "Insufficent")})
		     conf_intervals
    in
	{count = Int.toString(count(statvar)),
	 min = dataToString (min(statvar)),
	 max = dataToString (max(statvar)), 
	 first = dataToString(first(statvar)), 
	 last = dataToString (last(statvar)), 
	 avrg = (Real.fmt (StringCvt.FIX (SOME d)) (avrg(statvar))
		 handle Overflow => "Overflow"),
	 ssd = (Real.fmt (StringCvt.FIX (SOME d)) (ssd(statvar))
		handle Overflow => "Overflow"),
	 vari = (Real.fmt (StringCvt.FIX (SOME d)) (vari(statvar))
		 handle Overflow => "Overflow"),
	 std = (Real.fmt (StringCvt.FIX (SOME d)) (std(statvar))
		handle Overflow => "Overflow"),
	 sum = dataToString(sum (statvar)),
	 ss = dataToString(ss(statvar)),
	 ci = cistrs,
	 starttime = NONE,
	 lasttime = NONE,
	 interval = NONE}
    end

end

structure CPN'RUSV : CPN'UNTIMEDSTATVAR = struct

    type data = real
    type Statvar = {count:int,
		    min: data,
		    max: data,
		    first: data, 
		    last: data,
		    sum: data,
		    sqsum: data} option ref

    fun create () = ref NONE: Statvar

    fun upd(statvar as ref NONE: Statvar, x:real) =
	statvar:= SOME {count=1,min=x,max=x,first=x,last=x,sum=x,sqsum=x*x}
      | upd(statvar as ref(SOME{count,min,max,first,last,sum,sqsum}), x) =
	statvar:= SOME {count= count+1,
			min= Real.min(x,min),
			max= Real.max(x,max),
			first= first,
			last= x,
			sum= sum+x,
			sqsum= sqsum+x*x}

    fun avrg (ref NONE: Statvar) = 0.0
      | avrg (ref (SOME{sum,count,...})) = sum / real count

    fun ssd (ref NONE: Statvar) = 0.0
      | ssd (ref (SOME{sqsum,count,sum,...})) =
	(* Using max to avoid rouding errors resulting in negative numbers *)
	Real.max (sqsum - sum * sum / real count, 0.0)

    fun vari (ref NONE: Statvar) = 0.0
      | vari (statvar as ref (SOME{count,...})) =
	if count=1 then 0.0 else ssd statvar / real(count-1)
	
    fun std(statvar: Statvar) = Math.sqrt (vari statvar)
	
    fun init(statvar: Statvar) = statvar:= NONE

    local
	fun extract (ref NONE: Statvar) = 
	    {count=0,min=0.0,max=0.0,first=0.0,last=0.0,sum=0.0,sqsum=0.0}
	  | extract (ref (SOME statvar)) = statvar
    in
	val count: Statvar -> int  = #count o extract
	val min  : Statvar -> real = #min   o extract
	val max  : Statvar -> real = #max   o extract
	val first: Statvar -> real = #first o extract
	val last: Statvar -> real = #last o extract
	val sum  : Statvar -> real = #sum   o extract
	val ss   : Statvar -> real = #sqsum o extract
    end

    fun dataToString (x:data) = 
	Real.fmt (StringCvt.FIX (SOME (!CPN'PerfOptions.decimaldigits))) x

    fun ci (statvar:Statvar, percent:int) =
	CPN'ConfidenceInterval.calc (avrg statvar, count statvar, 
				     vari statvar, percent)

fun toStrings (statvar: Statvar) =
    let
	val d = !CPN'PerfOptions.decimaldigits
	val conf_intervals = map (fn p => ci (statvar,p)) 
				 (CPN'PerfOptions.get_ci_percentages())
	val cistrs = 
	    map (fn {percentage,half_length,avrg,
		     upper_endpoint,lower_endpoint} =>
		    {percentage=percentage,
		     avrg = 
		     ((Real.fmt (StringCvt.FIX (SOME d)) avrg)
		      handle Overflow => "Overflow"),
		     half_length=
		     (case half_length of
			  SOME hl => (Real.fmt (StringCvt.FIX (SOME d)) hl
				      handle Overflow => "Overflow")
			| NONE => "Insufficent"),
		     upper_endpoint= 
		     (case upper_endpoint of
			  SOME ue => (Real.fmt (StringCvt.FIX (SOME d)) ue
				      handle Overflow => "Overflow")
			| NONE => "Insufficent"),
		     lower_endpoint= 
		     (case lower_endpoint of 
			  SOME le => (Real.fmt (StringCvt.FIX (SOME d)) le
				      handle Overflow => "Overflow")
			| NONE => "Insufficent")})
		     conf_intervals
    in
	{count = Int.toString(count(statvar)),
	 min = (dataToString (min(statvar)) handle Overflow => "Overflow"),
	 max = 	(dataToString (max(statvar)) handle Overflow => "Overflow"), 
	 first = (dataToString(first(statvar)) handle Overflow => "Overflow"), 
	 last = (dataToString (last(statvar)) handle Overflow => "Overflow"), 
	 avrg = (dataToString (avrg(statvar)) handle Overflow => "Overflow"), 
	 ssd = (dataToString (ssd(statvar)) handle Overflow => "Overflow"), 
	 vari = (dataToString (vari(statvar)) handle Overflow => "Overflow"), 
	 std = (dataToString (std(statvar)) handle Overflow => "Overflow"),
	 sum = (dataToString (sum (statvar)) handle Overflow => "Overflow"),
	 ss = (dataToString (ss(statvar)) handle Overflow => "Overflow"),
	 ci = cistrs,
	 starttime = NONE,
	 lasttime = NONE,
	 interval = NONE}
    end
end;

