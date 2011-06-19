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
(* File: timedstatvar.sml
 *
 * Timed statistical variables
 *)


(* Integer data values *)
functor CPN'CreateITSV (Time : CPN'TIME) : CPN'TIMEDSTATVAR = 
    struct
	
	type time = Time.time
	type data = IntInf.int
	type sumtype = Time.time

	type Statvar = {count:int,
			     min: data,
			     max: data,
			     first: data, 
			     last: data,
			     sum: sumtype,
			     sqsum: sumtype,
			     starttime: time,
			     lasttime: time} option ref


	fun create () = ref NONE
	fun init tsv = (tsv := NONE)

	exception TSVException of string
	val timeErrorStr = "current time is less than last update time in timed statistical variable"
	    
	(* The interval of model time since the tsv was last updated.
	 * If the interval is negative then model time is decreasing
	 * and an error has occurred *)
	fun lastinterval (tsv as ref NONE: Statvar) = Time.null
	  | lastinterval (tsv as ref(SOME{count,min,max,first,last,
				      sum,sqsum,starttime,lasttime})) =
	    if Time.leq(lasttime,Time.time())
		then Time.sub(Time.time(),lasttime)
	    else (CPN'debug (timeErrorStr^" time,lasttime: "^
			     Time.toString(Time.time())^","^
			     (Time.toString lasttime)); 
		 raise TSVException (timeErrorStr^" time,lasttime: "^
				     Time.toString(Time.time())^","^
				     (Time.toString lasttime)))

	(* The interval of time since the tsv was first updated *)
	fun interval (tsv as ref NONE: Statvar) = Time.null
	  | interval (tsv as ref(SOME{count,min,max,first,last,
				      sum,sqsum,starttime,lasttime})) =
	    if Time.leq(lasttime,Time.time())
		then Time.sub(Time.time(),starttime)
	    else (CPN'debug (timeErrorStr^" time,lasttime: "^
			     Time.toString(Time.time())^","^
			     (Time.toString lasttime)); 
		  raise TSVException (timeErrorStr^" time,lasttime: "^
				      Time.toString(Time.time())^","^
				      (Time.toString lasttime)))
		 
	val iizero  = IntInf.fromInt 0
	fun extract (ref NONE: Statvar) = 
	    {count=0,min=iizero,max=iizero,first=iizero,last=iizero,
	     sum=Time.null,sqsum=Time.null,
	     starttime = Time.time(), lasttime = Time.time()}
	  | extract (ref (SOME tsv)) = tsv

	fun count tsv = (#count o extract) tsv
	fun min tsv = (#min o extract) tsv
	fun max tsv = (#max o extract) tsv
	fun first tsv = (#first o extract) tsv
	fun last tsv = (#last o extract) tsv
	fun starttime tsv = (#starttime o extract) tsv
	fun lasttime tsv = (#lasttime o extract) tsv

	fun sum tsv = 
	    let
		val cursum = (#sum o extract) tsv
		val lastinterval = lastinterval(tsv)
		val weightedval = Time.mult(lastinterval,Time.fromIntInf(last tsv))
	    in
		Time.add(cursum,weightedval)
	    end
	
	fun avrg tsv = 
	    let
		val interval = interval(tsv)
		val realsum = Time.toReal(sum(tsv))
	    in
		if Time.leq(interval, Time.null)
		    then 0.0
		else realsum/(Time.toReal interval)
	    end

	fun ss tsv = 
	    let
		val cursqsum = (#sqsum o extract) tsv
		val lastinterval = lastinterval(tsv)
		val valsq = Time.fromIntInf(IntInf.*(last tsv,last tsv))
		val weightedval = Time.mult(lastinterval,valsq)
	    in
		Time.add(cursqsum,weightedval)
	    end

	fun ssd tsv = 
	    let
		val realinterval = Time.toReal(interval tsv)
		val realss = Time.toReal(ss tsv)
		val avg = (avrg tsv)
	    in
		(* Using max to avoid rouding errors resulting 
		 * in negative numbers *)
		Real.max(realss - (realinterval * avg * avg), 0.0)
	    end
	
	fun vari tsv = 
	    let
		val interval = interval(tsv)
		val ssd = ssd tsv
	    in
		case (Time.cmp(Time.sub(interval,Time.fromInt(1)),
				   Time.null)) of
		    EQUAL => 0.0 (* when interval=1 *)
		  | _=>  ssd/(Time.toReal(interval)-1.0)
	    end

	fun std tsv = Math.sqrt(vari(tsv))

	fun upd(tsv as ref NONE: Statvar, x:data) =
	    tsv:= SOME {count=1,min=x,max=x,
			first=x,last=x,
			sum=Time.null,
			sqsum=Time.null,
			starttime=Time.time(),
			lasttime=Time.time()}
	  | upd(tsv as ref(SOME _), x:data) =
	    tsv:= SOME {count= (count tsv)+1,
			min= IntInf.min(x,(min tsv)),
			max= IntInf.max(x,(max tsv)),
			first= first tsv,
			last= x,
			sum= sum(tsv),
			sqsum= ss(tsv),
			starttime = starttime tsv,
			lasttime = Time.time()}

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
		 sum = Time.toString(sum (statvar)),
		 ss = Time.toString(ss(statvar)),
		 ci = cistrs,
		 starttime = SOME (Time.toString(starttime(statvar))),
		 lasttime = SOME (Time.toString(lasttime(statvar))),
		 interval = SOME (Time.toString(interval(statvar)))}
    end

end


(* Real data values *)
functor CPN'CreateRTSV (Time : CPN'TIME) : CPN'TIMEDSTATVAR = 
    struct
	
	type time = Time.time
	type data = real
	type sumtype = real

	type Statvar = {count:int,
			     min: data,
			     max: data,
			     first: data, 
			     last: data,
			     sum: sumtype,
			     sqsum: sumtype,
			     starttime: time,
			     lasttime: time} option ref


	fun create () = ref NONE
	fun init tsv = (tsv := NONE)

	exception TSVException of string
	val timeErrorStr = "current time is less than last update time in timed statistical variable"
	    
	(* The interval of model time since the tsv was last updated.
	 * If the interval is negative then model time is decreasing
	 * and an error has occurred *)
	fun lastinterval (tsv as ref NONE: Statvar) = Time.null
	  | lastinterval (tsv as ref(SOME{count,min,max,first,last,
				      sum,sqsum,starttime,lasttime})) =
	    if Time.leq(lasttime,Time.time())
		then Time.sub(Time.time(),lasttime)
	    else (CPN'debug (timeErrorStr^" time,lasttime: "^
			     Time.toString(Time.time())^","^
			     (Time.toString lasttime)); 
		  raise TSVException (timeErrorStr^" time,lasttime: "^
				      Time.toString(Time.time())^","^
				      (Time.toString lasttime)))

	(* The interval of time since the tsv was first updated *)
	fun interval (tsv as ref NONE: Statvar) = Time.null
	  | interval (tsv as ref(SOME{count,min,max,first,last,
				      sum,sqsum,starttime,lasttime})) =
	    if Time.leq(lasttime,Time.time())
		then Time.sub(Time.time(),starttime)
	    else (CPN'debug (timeErrorStr^" time,lasttime: "^
			     Time.toString(Time.time())^","^
			     (Time.toString lasttime)); 
		  raise TSVException (timeErrorStr^" time,lasttime: "^
				      Time.toString(Time.time())^","^
				      (Time.toString lasttime)))

	fun extract (ref NONE: Statvar) = 
	    {count=0,min=0.0,max=0.0,first=0.0,last=0.0,
	     sum=0.0,sqsum=0.0,
	     starttime = Time.time(), lasttime = Time.time()}
	  | extract (ref (SOME tsv)) = tsv

	fun count tsv = (#count o extract) tsv
	fun min tsv = (#min o extract) tsv
	fun max tsv = (#max o extract) tsv
	fun first tsv = (#first o extract) tsv
	fun last tsv = (#last o extract) tsv
	fun starttime tsv = (#starttime o extract) tsv
	fun lasttime tsv = (#lasttime o extract) tsv

	fun sum tsv = 
	    let
		val cursum = (#sum o extract) tsv
		val reallastinterval = Time.toReal(lastinterval(tsv))
		val weightedval = reallastinterval * (last tsv)
	    in
		cursum + weightedval
	    end
	
	fun avrg tsv = 
	    let
		val interval = interval(tsv)
	    in
		if Time.leq(interval, Time.null)
		    then 0.0
		else (sum tsv)/(Time.toReal interval)
	    end

	fun ss tsv = 
	    let
		val cursqsum = (#sqsum o extract) tsv
		val reallastinterval = Time.toReal(lastinterval(tsv))
		val weightedval = reallastinterval * (last tsv) * (last tsv)
	    in
		cursqsum + weightedval
	    end

	fun ssd tsv = 
	    let
		val realinterval = Time.toReal(interval tsv)
		val avg = (avrg tsv)
	    in
		(* Using max to avoid rouding errors resulting 
		 * in negative numbers *)
		Real.max((ss tsv) - (realinterval * avg * avg), 0.0)
	    end
	
	fun vari tsv = 
	    let
		val interval = interval(tsv)
		val ssd = ssd tsv
	    in
		case (Time.cmp(Time.sub(interval,Time.fromInt(1)),
				   Time.null)) of
		    EQUAL => 0.0 (* when interval=1 *)
		  | _=>  ssd/(Time.toReal(interval)-1.0)
	    end

	fun std tsv = Math.sqrt(vari(tsv))

	fun upd(tsv as ref NONE: Statvar, x:real) =
	    tsv:= SOME {count=1,min=x,max=x,
			first=x,last=x,
			sum=0.0,
			sqsum=0.0,
			starttime=Time.time(),
			lasttime=Time.time()}
	  | upd(tsv as ref(SOME _), x:real) =
	    tsv:= SOME {count= (count tsv)+1,
			min= Real.min(x,(min tsv)),
			max= Real.max(x,(max tsv)),
			first= first tsv,
			last= x,
			sum= sum(tsv),
			sqsum= ss(tsv),
			starttime = starttime tsv,
			lasttime = Time.time()}

	fun ci (statvar:Statvar, percent:int) =
	    CPN'ConfidenceInterval.calc (avrg statvar, count statvar, 
					 vari statvar, percent)

	fun dataToString (x:data) =
	    Real.fmt (StringCvt.FIX (SOME (!CPN'PerfOptions.decimaldigits))) x

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
		 min = (dataToString (min(statvar)) 
			handle Overflow => "Overflow"),
		 max = 	(dataToString (max(statvar)) 
			 handle Overflow => "Overflow"), 
		 first = (dataToString(first(statvar))
			  handle Overflow => "Overflow"), 
		 last = (dataToString (last(statvar))
			 handle Overflow => "Overflow"), 
		 avrg = (dataToString (avrg(statvar))
			 handle Overflow => "Overflow"), 
		 ssd = (dataToString (ssd(statvar)) 
			handle Overflow => "Overflow"), 
		 vari = (dataToString (vari(statvar))
			 handle Overflow => "Overflow"), 
		 std = (dataToString (std(statvar))
			handle Overflow => "Overflow"),
		 sum = (dataToString (sum (statvar))
			handle Overflow => "Overflow"),
		 ss = (dataToString (ss(statvar)) 
		       handle Overflow => "Overflow"),
		 ci = cistrs,
		 starttime = SOME (Time.toString(starttime(statvar))),
		 lasttime = SOME (Time.toString(lasttime(statvar))),
		 interval = SOME (Time.toString(interval(statvar)))}
	    end
end

