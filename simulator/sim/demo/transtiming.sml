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
(* File: transtiming.sml
 *
 * Demonstration of how to get detailed transition occurrence performance
 * statistics.
 * USAGE:
 *  1) Insert signature below in sim-sig.sml after datatype result
 *  2) Insert structure below in sim.sml after datatype result
 *  3) Modify: sim.sml:run:
 *      case TransTiming.time_trans(id,bind_exe,(fast,inst)) of
 *  4) Optional: use time_simulation() to run a simulation (demo/simtimer.sml)
 *  5) After finished simulation run, call CPN'Sim.TransTiming.print_stats();
 *)

structure TransTiming: 
    sig
	val time_trans: CPN'Id.id 
	    * (mode * int -> result * string list)
	    * (mode * int)
	    -> (result * string list)
	val print_stats: unit -> unit
    end

structure TransTiming =
struct
    structure T= SMLTime
    (* times called, real, usr, sys, gc*)
    type item = int ref * T.time ref * T.time ref * T.time ref * T.time ref

    val table_exe: (CPN'Id.id,item) HashTable.hash_table = 
	HashTable.mkTable hashId (19,InternalError("TTiming.find exe"))
    val peek_exe = HashTable.find table_exe
    fun listi_exe () = HashTable.listItemsi table_exe
    val insert_exe = HashTable.insert table_exe

    val table_dis: (CPN'Id.id,item) HashTable.hash_table = 
	HashTable.mkTable hashId (19,InternalError("TTiming.find dis"))
    val peek_dis = HashTable.find table_dis
    fun listi_dis () = HashTable.listItemsi table_dis
    val insert_dis = HashTable.insert table_dis

    val table_maybe: (CPN'Id.id,item) HashTable.hash_table = 
	HashTable.mkTable hashId (19,InternalError("TTiming.find maybe"))
    val peek_maybe = HashTable.find table_maybe
    fun listi_maybe () = HashTable.listItemsi table_maybe
    val insert_maybe = HashTable.insert table_maybe

    fun time_trans (id,exe_fun,(mode,inst)) =
	let
	    val cputimer= Timer.startCPUTimer()
	    val realtimer= Timer.startRealTimer()
	    val result = exe_fun(mode,inst)
	    val {usr,sys,gc}= Timer.checkCPUTimer cputimer
	    val realResult= Timer.checkRealTimer realtimer
	    val idi= id^"_"^(Int.toString inst)

	    val (count_ref,real_ref,usr_ref,sys_ref,gc_ref)= 
		case result of
		    (is_executed,_) =>
			(case peek_exe(idi) of
			     NONE =>
				 (insert_exe (idi,(ref 0,ref(T.zeroTime),ref(T.zeroTime),ref(T.zeroTime),ref(T.zeroTime)));
				  valOf(peek_exe(idi)))
			   | SOME el => el)
		  | (is_disabled,_) =>
			(case peek_dis(idi) of
			     NONE =>
				 (insert_dis (idi,(ref 0,ref(T.zeroTime),ref(T.zeroTime),ref(T.zeroTime),ref(T.zeroTime)));
				  valOf(peek_dis(idi)))
			   | SOME el => el)
		  | (is_maybe_ready_at _,_) =>
			(case peek_maybe(idi) of
			     NONE =>
				 (insert_maybe (idi,(ref 0,ref(T.zeroTime),ref(T.zeroTime),ref(T.zeroTime),ref(T.zeroTime)));
				  valOf(peek_maybe(idi)))
			   | SOME el => el)
	in
	    count_ref:= !count_ref +1;
	    real_ref:= T.+(!real_ref,realResult);
	    usr_ref:= T.+(!usr_ref,usr);
	    sys_ref:= T.+(!sys_ref,sys);
	    gc_ref:= T.+(!gc_ref,gc);
	    result
	end

    fun print_stats() =
	let
	    val (real_exe_sum,usr_exe_sum)=
		HashTable.fold 
		  (fn ((_,ref real_res,ref usr_res,_,_),(tr,tu))=> (T.+(tr,real_res),T.+(tu,usr_res)))
		  (T.zeroTime,T.zeroTime)
		  table_exe
	    val (real_dis_sum,usr_dis_sum)=
		HashTable.fold (fn ((_,ref real_res,ref usr_res,_,_),(tr,tu))=> (T.+(tr,real_res),T.+(tu,usr_res))) (T.zeroTime,T.zeroTime) table_dis
	    val (real_maybe_sum,usr_maybe_sum)=
		HashTable.fold (fn ((_,ref real_res,ref usr_res,_,_),(tr,tu))=> (T.+(tr,real_res),T.+(tu,usr_res))) (T.zeroTime,T.zeroTime) table_maybe
	in
	 app 
	   (fn (id,(ref count,ref real,ref usr,ref sys,ref gc))=>
	    print ("exe   id="^id^" c="^(Int.toString count)^" r="^(T.toString real)^" u="^(T.toString usr)^" s="^(T.toString sys)^" g="^(T.toString gc)^" t/s="^(if T.<(real,T.fromMilliseconds 1) then "?" else (Real.toString((Real./(Real.fromInt count,T.toReal real)))))^"\n"))
	   (listi_exe());
	 app 
	   (fn (id,(ref count,ref real,ref usr,ref sys,ref gc))=>
	    print ("dis   id="^id^" c="^(Int.toString count)^" r="^(T.toString real)^" u="^(T.toString usr)^" s="^(T.toString sys)^" g="^(T.toString gc)^" t/s="^(if T.<(real,T.fromMilliseconds 1) then "?" else (Real.toString((Real.fromInt count)/(T.toReal real))))^"\n"))
	   (listi_dis());
	 app 
	   (fn (id,(ref count,ref real,ref usr,ref sys,ref gc))=>
	    print ("maybe id="^id^" c="^(Int.toString count)^" r="^(T.toString real)^" u="^(T.toString usr)^" s="^(T.toString sys)^" g="^(T.toString gc)^" t/s="^(if T.<(real,T.fromMilliseconds 1) then "?" else (Real.toString((Real.fromInt count)/(T.toReal real))))^"\n"))
	   (listi_maybe());
	 print ("real_exe_sum="^(T.toString real_exe_sum)^"\n");
	 print ("usr_exe_sum="^(T.toString usr_exe_sum)^"\n");
	 print ("real_dis_sum="^(T.toString real_dis_sum)^"\n");
	 print ("usr_dis_sum="^(T.toString usr_dis_sum)^"\n");
	 print ("real_maybe_sum="^(T.toString real_maybe_sum)^"\n");
	 print ("usr_maybe_sum="^(T.toString usr_maybe_sum)^"\n")
	end
end;
