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
(* File: Statvar.sml
 *
 * Statistical variables
 *)

type 'a Statvar = {count:int,
		   min: 'a,
		   max: 'a,
		   first: 'a, 
		   value: 'a,
		   sum: 'a,
		   sqsum: 'a} option ref

structure ISV = struct
    fun CreateIntStatvar () = ref NONE: int Statvar
	
    fun upd(statvar as ref NONE:int Statvar, x:int) =
	statvar:= SOME {count=1,min=x,max=x,first=x,value=x,sum=x,sqsum=x*x}
      | upd(statvar as ref(SOME{count,min,max,first,value,sum,sqsum}), x) =
	statvar:= SOME {count= count+1,
			min= if x<min then x else min,
			max= if x>max then x else max,
			first= first,
			value= x,
			sum= sum+x,
			sqsum= sqsum+x*x}

    fun avrg (ref NONE: int Statvar) = 0.0
      | avrg (ref (SOME{sum,count,...})) = real sum / real count

    fun ssd (ref NONE: int Statvar) = 0.0
      | ssd (ref (SOME{sqsum,count,sum,...})) =
	(* Using max to avoid rouding errors resulting in negative numbers *)
	Real.max (real sqsum - real sum * real sum / real count, 0.0)

    fun vari (ref NONE: int Statvar) = 0.0
      | vari (statvar as ref (SOME{count,...})) =
	if count=1 then 0.0 else ssd statvar / real(count-1)
	
    fun std(statvar:int Statvar) = Math.sqrt(vari statvar)
	
    fun init(statvar:int Statvar) = statvar:= NONE

    local
	fun extract (ref NONE: int Statvar) = 
	    {count=0,min=0,max=0,first=0,value=0,sum=0,sqsum=0}
	  | extract (ref (SOME statvar)) = statvar
    in
	val count: int Statvar -> int = #count o extract
	val min  : int Statvar -> int = #min   o extract
	val max  : int Statvar -> int = #max   o extract
	val first: int Statvar -> int = #first o extract
	val value: int Statvar -> int = #value o extract
	val sum  : int Statvar -> int = #sum   o extract
	val ss   : int Statvar -> int = #sqsum o extract
    end

    (* Unnecessary, kept for backward capability *)
    val initialize = upd;
end

structure RSV = struct

    fun CreateRealStatvar () = ref NONE: real Statvar

    fun upd(statvar as ref NONE: real Statvar, x:real) =
	statvar:= SOME {count=1,min=x,max=x,first=x,value=x,sum=x,sqsum=x*x}
      | upd(statvar as ref(SOME{count,min,max,first,value,sum,sqsum}), x) =
	statvar:= SOME {count= count+1,
			min= if x<min then x else min,
			max= if x>max then x else max,
			first= first,
			value= x,
			sum= sum+x,
			sqsum= sqsum+x*x}

    fun avrg (ref NONE: real Statvar) = 0.0
      | avrg (ref (SOME{sum,count,...})) = sum / real count

    fun ssd (ref NONE: real Statvar) = 0.0
      | ssd (ref (SOME{sqsum,count,sum,...})) =
	(* Using max to avoid rouding errors resulting in negative numbers *)
	Real.max (sqsum - sum * sum / real count, 0.0)

    fun vari (ref NONE: real Statvar) = 0.0
      | vari (statvar as ref (SOME{count,...})) =
	if count=1 then 0.0 else ssd statvar / real(count-1)
	
    fun std(statvar: real Statvar) = Math.sqrt (vari statvar)
	
    fun init(statvar: real Statvar) = statvar:= NONE

    local
	fun extract (ref NONE: real Statvar) = 
	    {count=0,min=0.0,max=0.0,first=0.0,value=0.0,sum=0.0,sqsum=0.0}
	  | extract (ref (SOME statvar)) = statvar
    in
	val count: real Statvar -> int  = #count o extract
	val min  : real Statvar -> real = #min   o extract
	val max  : real Statvar -> real = #max   o extract
	val first: real Statvar -> real = #first o extract
	val value: real Statvar -> real = #value o extract
	val sum  : real Statvar -> real = #sum   o extract
	val ss   : real Statvar -> real = #sqsum o extract
    end

    (* Unnecessary, kept for backward combability *)
    val initialize = upd;
end;

fun SV'createint () = ISV.CreateIntStatvar ();
fun SV'createreal () = RSV.CreateRealStatvar ();

(*
_overload SV'upd  : 'a * 'b -> 'c as ISV.upd and RSV.upd;
_overload SV'min  : 'a -> 'b as ISV.min   and RSV.min;
_overload SV'max  : 'a -> 'b as ISV.max   and RSV.max;
_overload SV'first: 'a -> 'b as ISV.first and RSV.first;
_overload SV'value: 'a -> 'b as ISV.value and RSV.value;
_overload SV'count: 'a -> 'b as ISV.count and RSV.count;
_overload SV'sum  : 'a -> 'b as ISV.sum   and RSV.sum;
_overload SV'avrg : 'a -> 'b as ISV.avrg  and RSV.avrg;
_overload SV'ss   : 'a -> 'b as ISV.ss    and RSV.ss;
_overload SV'ssd  : 'a -> 'b as ISV.ssd   and RSV.ssd;
_overload SV'vari : 'a -> 'b as ISV.vari  and RSV.vari;
_overload SV'std  : 'a -> 'b as ISV.std   and RSV.std;
_overload SV'init : 'a -> 'b as ISV.init  and RSV.init;
*)
