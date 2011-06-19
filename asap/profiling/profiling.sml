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
(* --- 

   FILE:         profiling.sml

   DESCRIPTION:  Simple profiling of function calls

   Copyright (c) Peter Mechlenborg, Michael Westergaard

   CREATED:      21/01/2005

   CHANGELOG:

   NOTES:

   --- *)

structure Profiling :> sig
    type stat

    val makeStat' : stat -> string -> stat
    val makeStat : string -> stat
    val stat : stat -> int -> unit
    val getStatName : stat -> string
    val getStat : stat -> int
    val resetStat : stat -> unit
    val resetStats : stat list -> unit

    type phase

    val makePhase : string -> phase
    val phase : phase -> ('a -> 'b) -> ('a -> 'b)
    val getPhaseName : phase -> string
    val getUsr : phase -> real
    val getSys : phase -> real
    val getGC : phase -> real
    val getAll : phase -> { usr : real, sys: real, gc: real }
    val getTotal : phase -> real
    val getLastUsr : phase -> real
    val getLastSys : phase -> real
    val getLastGC : phase -> real
    val getLastTotal : phase -> real
    val resetPhase : phase -> unit
    val resetPhases : phase list -> unit

    val phaseAndStat : phase -> stat -> ('a -> 'b) -> ('a -> 'b)
end =
struct
    datatype counter = C of { c: int ref, cs: counter list }
    datatype stat = STAT of { name: string, c: counter }

    fun newCounter cs = C { c = ref 0, cs = cs }
    fun addCounter ( C { c, cs } ) n = (c := (!c) + n; app (fn c => addCounter c n) cs)
    fun getCounter ( C { c, cs } ) = !c

    fun stat (STAT { name, c }) n = addCounter c n
    fun makeStat' (STAT { name = _, c }) name = STAT { name = name, c = newCounter [c] }
    fun makeStat name = STAT { name = name, c = newCounter [] }
    fun getStatName (STAT { name, ... }) = name
    fun getStat (STAT { name, c }) = getCounter c
    fun resetStat s = stat s (~(getStat s))
    fun resetStats ss = List.app resetStat ss

    structure T = Time
    type times = {usr:T.time, sys:T.time, gc:T.time}
    val zeros = {usr=T.zeroTime, sys=T.zeroTime, gc=T.zeroTime}

    datatype phase = PHASE of {name : string, accum : times ref, this: times ref}

    fun phase (PHASE { name, accum, this }) f x =
    let
        val time = Timer.startCPUTimer()
        val result = f x
        val { nongc, gc } = Timer.checkCPUTimes(time)
        val timer = { usr = #usr nongc, sys = Time.+ (#sys nongc, #sys gc), gc = #usr gc }
        val _ = this := timer
        val total = { usr = Time.+ (#usr timer, #usr (!accum)),
                      sys = Time.+ (#sys timer, #sys (!accum)),
                      gc  = Time.+ (#gc  timer, #gc  (!accum)) }
        val _ = accum := total
    in
        result
    end
    fun makePhase name = PHASE { name = name, accum = ref zeros, this = ref zeros }
    fun getPhaseName (PHASE { name, ... }) = name
    fun getUsr (PHASE { name, accum = ref { usr, ... }, ...}) = Time.toReal usr
    fun getSys (PHASE { name, accum = ref { sys, ... }, ...}) = Time.toReal sys
    fun getGC (PHASE { name, accum = ref { gc, ... }, ...}) = Time.toReal gc
    fun getTotal (PHASE { name, accum = ref { usr, sys, gc }, ...}) = Time.toReal (Time.+ (Time.+(usr, sys), gc))
    fun getAll p = { usr = getUsr p, sys = getSys p, gc = getGC p }
    fun getLastUsr (PHASE { name, this = ref { usr, ... }, ...}) = Time.toReal usr
    fun getLastSys (PHASE { name, this = ref { sys, ... }, ...}) = Time.toReal sys
    fun getLastGC (PHASE { name, this = ref { gc, ... }, ...}) = Time.toReal gc
    fun getLastTotal (PHASE { name, this = ref { usr, sys, gc }, ...}) = Time.toReal (Time.+ (Time.+(usr, sys), gc))
    fun resetPhase (PHASE { name, accum, this }) = (accum := zeros; this := zeros)
    fun resetPhases ps = List.app resetPhase ps

    fun phaseAndStat p s f v =
    let
        val _ = stat s 1
    in
        phase p f v
    end

end

functor ProfilingHelp (
val name: string
) =
struct
    open Profiling
    val stats = ref ([]: stat list)
    val phases = ref ([]: phase list)

    fun makeStat' s n =
    let
        val stat = Profiling.makeStat' s (String.concat [n, " ", name])
        val _ = stats := (stat::(!stats))
    in
        stat
    end

    fun makeStat n =
    let
        val stat = Profiling.makeStat (String.concat [n, " ", name])
        val _ = stats := (stat::(!stats))
    in
        stat
    end

    fun getStats () = !stats

    fun makePhase n =
    let
        val phase = Profiling.makePhase (String.concat [n, " ", name])
        val _ = phases := (phase::(!phases))
    in
        phase
    end

    fun getPhases () = !phases

    fun reset () = (resetStats (!stats); resetPhases (!phases))
end
