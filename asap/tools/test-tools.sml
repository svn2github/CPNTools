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
structure TestTools =
struct
    fun unit_ms count = List.tabulate (count, fn _ => ())
    fun int_ms count = List.tabulate (count, fn n => n)

    fun noStats _ = ""

    fun noStatsList _ = []

    fun simpleStatsToString storageStats (vs, va, s, a, storage) =
        String.concat[Int.toString vs, "\t", Int.toString va, "\t",
        Int.toString s, "\t", Int.toString a, "\t", storageStats storage]

    fun simpleStatsToList storageStats (vs, va, s, a, storage) =
	("IntegerT", "VisitedNodes", Int.toString vs) ::
	("IntegerT", "VisitedArcs", Int.toString va) ::
	("IntegerT", "Nodes", Int.toString s) ::
	("IntegerT", "Arcs", Int.toString a) ::
	(storageStats storage)

    val sweeplineStatsHeader =
	String.concat
	    [
	     "Visited states\t",
	     "Visited arcs\t",
	     "Persistent states\t",
	     "Peek states\t",
	     "Regress edges\t",
	     "Progress edges\t",
	     "Stationary edges"
	    ]
	
    fun sweeplineStatsToString storageStats
			       (sweeplineStats as {
				visitedStates,
				visitedArcs,
				finalPersistentStates,
				peekStates,
				regressEdges,
				progressEdges,
				stationaryEdges,
				persistentStates,
				gsStates,
				equivalenceClasses,
				distances }, storage) =
	String.concat
	    [	 
	     Int.toString visitedStates, "\t",
	     Int.toString visitedArcs, "\t",
	     Int.toString finalPersistentStates, "\t",
	     Int.toString peekStates, "\t",
	     Int.toString regressEdges, "\t",
	     Int.toString progressEdges, "\t",
	     Int.toString stationaryEdges, "\t",
	     (storageStats storage)
	    ]

    fun sweeplineStatsToList storageStats (sweeplineStats as {
					   visitedStates,
					   visitedArcs,
					   finalPersistentStates,
					   peekStates,
					   regressEdges,
					   progressEdges,
					   stationaryEdges,
					   persistentStates,
					   gsStates,
					   equivalenceClasses,
					   distances }, storage) =
	("IntegerT", "VisitedNodes",
	 Int.toString visitedStates) ::
	("IntegerT", "VisitedArcs",
	 Int.toString visitedArcs) ::
	("IntegerT", "FinalPersistentStates",
	 Int.toString finalPersistentStates) ::
	("IntegerT", "PeekStates",
	 Int.toString peekStates) ::
	("IntegerT", "RegressEdges",
	 Int.toString regressEdges) ::
	("IntegerT", "ProgressEdges",
	 Int.toString progressEdges) ::
	("IntegerT", "StationaryEdges",
	 Int.toString stationaryEdges) ::
	(storageStats storage)

    fun simpleStatsStatesToString (_, (_, _, states, _, _)) =
        Int.toString states

    fun memoryStatsStatesToString (_, ((_, _, states, _, _), _, _)) =
        Int.toString states
        
    fun memoryToString f (rest, final_memory, memory) =
        String.concat [Int.toString memory, "\t", Int.toString final_memory,
        "\t", Int.toString (memory - final_memory), "\t", f rest]

    fun memoryToList f (rest, final_memory, memory) =
	("IntegerT", "Memory",  Int.toString memory) ::
	("IntegerT", "FMemory", Int.toString final_memory) ::
	("IntegerT", "PMemory", Int.toString (memory - final_memory)) ::
	(f rest)

    fun comback_explore explore { cache_size, cache_found } initial_states =
        explore {hash_size = 1000, array_size = 1000, cache_size = cache_size,
		 cache_found = cache_found, initial_states = initial_states }
		initial_states

    local
        open Estimator GNUPlot TimeLimit MemoryLimit SQLDump

        fun time_wrap f limit x =
            SOME (timeLimit (Time.fromReal limit)  f x)
            handle TimeLimit.TimeOut => NONE

        fun memory_wrap f interval_time interval_count time_limit limit x =
            ((SOME (memoryLimit (Time.fromReal interval_time)
            interval_count (Time.fromReal time_limit) limit f x))
            handle MemoryLimit.TimeOut _ => NONE)
            handle MemoryLimit.OutOfMemory => NONE

        fun memory2_wrap f interval time_limit limit x =
            ((SOME (MemoryLimit2.memoryLimit (Time.fromReal interval)
            (Time.fromReal time_limit) limit f x))
            handle MemoryLimit2.TimeOut => NONE)
            handle MemoryLimit2.OutOfMemory => NONE
    in

        fun simpleTestTime_1d_generic statsToString statsToList header
				      parameter prefix limit f g
				      extraParameters =
        let
	    fun f' x = f (g ()) x
            val result =
                estimate_doubling_accurate_intelligent
		    limit (time_wrap f') parameter
            val _ =
                write_data_table (statsToString noStats) header prefix result
            val _ =
                gnuplot_data simpleStatsStatesToString
			     (String.^("States for ", prefix)) result
            val _ =
                write_data_sql (statsToList noStatsList) extraParameters
			       prefix result
            val _ = 
                time gnuplot_data prefix result
        in
            ()
        end

        fun simpleTestTime_1d parameter prefix limit f g extraParameters =
            simpleTestTime_1d_generic
		simpleStatsToString simpleStatsToList
		parameter prefix limit f g extraParameters

	(*
        fun simpleTestTime_1d parameter prefix limit f g extraParameters =
        let
	    fun f' x = f (g ()) x
            val result =
                estimate_doubling_accurate_intelligent limit (time_wrap f') parameter
            val _ =
                write_data_table (simpleStatsToString noStats) "VStates\tVArcs\tStates\tArcs" prefix result
            val _ =
                gnuplot_data simpleStatsStatesToString (String.^("States for ", prefix)) result
            val _ =
                write_data_sql (simpleStatsToList noStatsList) extraParameters (prefix) result
            val _ = 
                time gnuplot_data prefix result
        in
            ()
        end
	*)

        fun simpleTestTime_2d parameter prefix limit f g extraParameters =
        let
	    fun f' x = f (g ()) x
            val result =
                estimate_interval_doubling_accurate_2d_intelligent 1 limit (time_wrap f') parameter
            val _ = 
                write_data_tables_2d (simpleStatsToString noStats) "VStates\tVArcs\tStates\tArcs" prefix result
            val _ =
                time gnuplot_data_3d prefix result
            val _ =
                gnuplot_data_3d simpleStatsStatesToString
                   (String.^("States for ", prefix)) result
	    val _ =
                write_data_sql_2d (simpleStatsToList noStatsList) extraParameters prefix result
            val _ =
                time gnuplot_data_2d prefix result
        in
            ()
        end

        fun simpleTestMemory_1d parameter prefix storageStats interval_time interval_count time_limit limit f g extraParameters =
        let
	    fun f' x = f (g ()) x
            val result =
                estimate_doubling_accurate limit
                (memory_wrap f' interval_time interval_count time_limit) parameter
            val _ =
                write_data_table (memoryToString (simpleStatsToString storageStats))
                "Memory\tFMemory\tPMemory\tVStates\tVArcs\tStates\tArcs" prefix result
            val _ =
                gnuplot_data memoryStatsStatesToString (String.^("States for ", prefix)) result
            val _ =
                write_data_sql (memoryToList (simpleStatsToList noStatsList))
                extraParameters prefix result
            val _ = 
                time gnuplot_data prefix result
        in
            ()
        end

        fun simpleTestMemory_2d parameter prefix storageStats interval_time interval_count time_limit limit f g extraParameters =
        let
	    fun f' x = f (g ()) x
            val result =
                estimate_interval_doubling_accurate_2d 1 limit
                (memory_wrap f' interval_time interval_count time_limit) parameter
            val _ = 
                write_data_tables_2d (memoryToString (simpleStatsToString storageStats))
                "Memory\tFMemory\tPMemory\tVStates\tVArcs\tStates\tArcs" prefix result
            val _ =
                time gnuplot_data_3d prefix result
            val _ =
                gnuplot_data_3d memoryStatsStatesToString (String.^("States for ", prefix)) result
            val _ = 
                write_data_sql_2d (memoryToList (simpleStatsToList noStatsList))
                extraParameters prefix result
            val _ =
                time gnuplot_data_2d prefix result
        in
            ()
        end

        fun simpleTestMemory2_1d parameter prefix storageStats interval time_limit limit f g extraParameters =
        let
	    fun f' x = f (g ()) x
            val result =
                estimate_doubling_accurate limit
                (memory2_wrap f' interval time_limit) parameter
            val _ =
                write_data_table (memoryToString (simpleStatsToString storageStats))
                "Memory\tFMemory\tPMemory\tVStates\tVArcs\tStates\tArcs" prefix result
            val _ =
                gnuplot_data memoryStatsStatesToString (String.^("States for ", prefix)) result
            val _ =
                write_data_sql (memoryToList (simpleStatsToList noStatsList))
                extraParameters prefix result
            val _ = 
                time gnuplot_data prefix result
        in
            ()
        end

        fun simpleTestMemory2_2d parameter prefix storageStats interval time_limit limit f g extraParameters =
        let
	    fun f' x = f (g ()) x
            val result =
                estimate_interval_doubling_accurate_2d 1 limit
                (memory2_wrap f' interval time_limit) parameter
            val _ = 
                write_data_tables_2d (memoryToString (simpleStatsToString storageStats))
                "Memory\tFMemory\tPMemory\tVStates\tVArcs\tStates\tArcs" prefix result
            val _ =
                time gnuplot_data_3d prefix result
            val _ =
                gnuplot_data_3d memoryStatsStatesToString (String.^("States for ", prefix)) result
            val _ = 
                write_data_sql_2d (memoryToList (simpleStatsToList noStatsList))
                extraParameters prefix result
            val _ =
                time gnuplot_data_2d prefix result
        in
            ()
        end

        fun runTime_1d parameter prefix values limit f g extraParameters =
        let
	    fun f' x = f (g ()) x
            fun run [] = []
              | run (n::rest) =
              (let
                  val _ = print "Running for "
                  val _ = print (Int.toString n)
                  val _ = print "\n"
                  val _ = SMLofNJ.Internals.GC.doGC 100
                  val start = Time.now()
                  val result = timeLimit (Time.fromReal limit) f' (parameter n)
                  val stop = Time.now()
               in
                   (n, (Time.-(stop, start), result))::(run rest)
               end)
               handle _ => run rest
            val result = run values
            val _ = write_data_table (simpleStatsToString noStats)
            "VStates\tVArcs\tStates\tArcs" prefix (0, result)
            val _ = gnuplot_data simpleStatsStatesToString (String.^("States for ",
            prefix)) (0, result)
            val _ = write_data_sql (simpleStatsToList noStatsList) extraParameters (prefix) (0, result)
            val _ = time gnuplot_data prefix (0, result)
        in
            ()
        end

        fun runTime_2d parameter prefix values limit f g extraParameters =
        let
	    fun run [] = ()
              | run ((n, m)::rest) =
              let
                  val _ = print "Running for X = "
                  val _ = print (Int.toString n)
                  val _ = print "\n"
                  val _ =
                      runTime_1d (parameter n) (String.concat [prefix, " X = ", Int.toString n]) m limit f g
				 (("IntegerT", !(SQLDump.yParam), Int.toString n) :: extraParameters)
              in
                  run rest
              end
        in
            run values
        end

        fun runMemory_1d parameter prefix storageStats interval_time interval_count
			 values limit f g extraParameters = let
	    fun f' x = f (g ()) x
            fun run [] = []
              | run (n::rest) =
                (let
                     val _ = print "Running for "
                     val _ = print (Int.toString n)
                     val _ = print "\n"
                     val start = Time.now()
                     val result =
                         memoryLimit (Time.fromReal interval_time) interval_count (Time.fromReal
										       limit) 1000000000 f' (parameter n)
                     val stop = Time.now()
                 in
                     (n, (Time.-(stop, start), result))::(run rest)
                 end)
                handle _ => run rest
            val result = run values
            val _ = write_data_table (memoryToString (simpleStatsToString storageStats))
				     "Memory\tFMemory\tPMemory\tVStates\tVArcs\tStates\tArcs" prefix (0, result)
            val _ = gnuplot_data memoryStatsStatesToString (String.^("States for ",
								     prefix)) (0, result)
            val _ =
                write_data_sql (memoryToList (simpleStatsToList noStatsList))
			       extraParameters prefix (0, result)
            val _ = time gnuplot_data prefix (0, result)
        in
            ()
        end

        fun runMemory_2d parameter prefix storageStats interval_time interval_count
            values limit f g extraParameters =
            let
		fun run [] = ()
                  | run ((n, m)::rest) =
                  let
                      val _ = print "Running for X = "
                      val _ = print (Int.toString n)
                      val _ = print "\n"
                      val _ =
                          runMemory_1d
				       (parameter n) (String.concat [prefix, " X = ", Int.toString n])
				       storageStats interval_time interval_count m limit f g (("IntegerT", !(SQLDump.yParam), Int.toString n) :: extraParameters)
                  in
                      run rest
                  end
            in
                run values
            end
    end
end
