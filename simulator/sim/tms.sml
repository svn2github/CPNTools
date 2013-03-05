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
(* File: tms.sml
 *
 * Timed external multi-sets representation.
 *)

infix 7 @+;
infix 7 @++;
infix 6 +++;

functor CPN'CreateTMS (structure Time: CPN'TIME):
sig
    structure ModelTime: CPN'TIME

    type 'a tms
    val @+    : 'a CPN'MS.ms * int -> 'a tms
    val @++   : 'a CPN'MS.ms * Time.time -> 'a tms
    val +++   : 'a tms * 'a tms -> 'a tms
    val input_tms : (TextIO.instream -> 'a) -> TextIO.instream -> 'a tms
    val mkstr_tms: (('a -> string) * ('a * 'a -> bool)) -> 'a tms -> string
    val output_tms : ((TextIO.outstream * 'a -> unit) * ('a * 'a -> bool)) -> TextIO.outstream * 'a tms -> unit
    val sort_tms: ('a Time.timed * 'a Time.timed -> bool) -> 'a tms -> 'a tms
    val tsub  : ''a tms * ''a tms -> ''a tms
    val tleq  : ''a tms * ''a tms -> bool
    val tcf   : ''a * ''a tms -> int
    val tlt   : ('a * 'a -> bool) -> 'a Time.timed * 'a Time.timed -> bool
    val tlegal: ('a -> bool) -> 'a tms -> bool
    val delay : 'a tms * Time.time -> 'a tms
    val ms    : 'a tms -> 'a CPN'MS.ms
end = struct
    structure ModelTime = Time

    type 'a tms = 'a Time.timed CPN'MS.ms;

    fun tlt lt (Time.@(col1,time1),Time.@(col2,time2)) =
	if lt(col1,col2) then true
	else if lt(col2,col1) then false
	else Time.lt(time1,time2)

    fun tms1 +++ tms2 = List.@(tms1,tms2)
			
    fun ms @++ time = 
	map (fn col => Time.@(col,Time.add(!Time.model_time,time))) ms
			
    fun ms @+ int_time = 
	let
	    val as_time = Time.fromString (Int.toString int_time)
	in
	    case as_time of SOME t =>
			    map (fn col => Time.@(col,Time.add(!Time.model_time,t))) ms
			  | NONE => raise InternalError ("Could not convert time "^Int.toString int_time^" to time of type "^Time.name)
	end 
	    
    fun mkstr_tms (mkstr_cs,lt_cs) tms = let
	fun mkstr_tcs (Time.@(col,time)) =
	    concat[mkstr_cs col,"@",Time.mkstr time]
    in
	CPN'MS.gen_mkstr_ms "+++" (mkstr_tcs, tlt lt_cs) tms
    end

    fun input_tms input_col s = let
	open CPN'StreamIO;

	fun input_tcs s = let 
	    val c = case get_until(s,[#"@"]) of
		(#"@", value) => implode value
	      | (_, value) =>
		    raise IOError("Can not find '@' in "^(implode value))
            val t = implode(get_next s)
        in
            Time.@(input_col (TextIO.openString c), Time.maketime t)
        end
    in
	CPN'MS.input_ms true input_tcs s
    end

    fun output_tms (output_cs, lt_cs) (s,tms) = let
	fun output_tcs (s, Time.@(col,time)) =
	    (output_cs (s, col);
	     TextIO.output1 (s, #"@");
	     TextIO.output (s, Time.mkstr time))
    in
	CPN'MS.output_ms true (output_tcs, tlt lt_cs) (s,tms)
    end

    val sort_tms = Misc.sort

    fun delay (tms,dt) =
	map (fn (Time.@(col,time)) => Time.@(col,Time.add(time,dt))) tms

    fun tlegal legal_cs tms = 
	foldr (fn (Time.@(col,_),tail) => (legal_cs col) andalso tail) true tms

    fun tsub(ms1,nil) = ms1
      | tsub(nil,_) = raise Subtract
      | tsub(ms1, (item2 as Time.@(col2,time2))::rms2) =
        let
            fun sub_item (nil,ms3,item3) = (ms3,item3)
              | sub_item ((item1 as Time.@(col1,time1))::rms1,ms3,NONE) =
                if col1=col2 andalso Time.leq(time1,time2) then
                    sub_item(rms1,ms3,SOME(item1))
                else
                    sub_item(rms1,item1::ms3,NONE)
              | sub_item ((item1 as Time.@(col1,time1))::rms1,ms3,
                          SOME(item3 as Time.@(_,time3))) =
                if col1=col2 andalso 
		    Time.leq(time1,time2) andalso Time.lt(time3,time1) then
                    sub_item(rms1,item3::ms3,SOME(item1))
                else
                    sub_item(rms1,item1::ms3,SOME(item3))
        in                  
            case sub_item(ms1,nil,NONE) of
                (ms3, SOME _) => tsub(ms3,rms2)
              | (_, NONE) => raise Subtract
        end
        
    fun tleq(nil,_) = true
      | tleq(_,nil) = false
      | tleq(ms1,ms2) = (tsub(ms2,ms1); true) handle Subtract => false;
                        
    fun tcf (col', tms) = 
	foldr (fn (Time.@(col,_),tail) => 
	      if col=col' then 1+tail else tail) 0 tms
	    
    fun ms (tms: 'a tms): 'a CPN'MS.ms = map Time.col tms
end;


