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
structure MaxMemory =
struct
    local
        open SMLofNJ.Cont SMLofNJ.Internals.GC SMLofNJ.IntervalTimer
        fun timerOn interval = ignore (setIntTimer (SOME interval))
        fun timerOff () = ignore (setIntTimer NONE)
    in
        fun maxMemory interval f x =
        let
            val _ = doGC 100
            val used = ref 0
            val startSize = getHeapSize ()
            fun handler (_, waiting, cont) =
            let
                val _ = timerOff()
                val _ = doGC 100
                val currentSize = getHeapSize ()
                val mem = currentSize - startSize
                val _ = used := (Int.max (mem, !used))
                val _ = timerOn interval
            in
                cont
            end
        in
            Signals.setHandler (Signals.sigALRM, Signals.HANDLER handler);
            timerOn interval;
            ((f x, (doGC 100; Int.max(!used, startSize - getHeapSize()))) handle ex => (timerOff(); raise ex))
            before timerOff()
        end

        fun memoryHistory interval f x =
        let
            val _ = doGC 100
            val used = ref [] : int list ref
            val startSize = getHeapSize ()
            fun handler (_, waiting, cont) =
            let
                val _ = timerOff()
                val _ = doGC 100
                val currentSize = getHeapSize ()
                val mem = currentSize - startSize
                val _ = used := (mem::(!used))
                val _ = timerOn interval
            in
                cont
            end
        in
            Signals.setHandler (Signals.sigALRM, Signals.HANDLER handler);
            timerOn interval;
            ((f x, (doGC 100; (getHeapSize() - startSize)::(!used))) handle ex => (timerOff(); raise ex))
            before timerOff()
        end
    end
end

structure MemoryLimit =
struct
    exception TimeOut of int
    exception OutOfMemory

    local
        open SMLofNJ.Internals.GC
    in
        fun memoryLimit interval_time interval_times t m f x =
        let
            val _ = doGC 100
            val startSize = getHeapSize ()
            val startTime = Time.now()
            val counter = ref interval_times
            val last_time = ref startTime
            val max_used = ref 0
            fun handler () =
            let
                val now = Time.now()
                val _ = counter := (!counter - 1)
            in
                if (!counter < 0) andalso (Time.<(interval_time, Time.-(now, !last_time)))
                then
                    let
                        val _ = last_time := now
                        val spent = Time.-(now, startTime)
                        val _ = counter := interval_times
                        val _ = if interval_times >= 0
                                then doGC 100
                                else ()
                        val currentSize = getHeapSize ()
                        val used = currentSize - startSize
                        val _ = max_used := (Int.max (!max_used, used))
                    in
                        if Time.<(spent, t)
                        then 
                            if used > m
                            then raise OutOfMemory
                            else ()
                        else raise (TimeOut used)
                    end
                else ()
            end
            val result = f handler x
            val _ = doGC 100
            val currentSize = getHeapSize()
        in
            (result, currentSize - startSize, Int.max (!max_used, startSize - currentSize))
        end
    end
end

structure MemoryLimit2 =
struct
    exception TimeOut
    exception OutOfMemory

    local
        open SMLofNJ.Internals.GC SMLofNJ.Cont SMLofNJ.IntervalTimer

        fun make_fail_cont exn =
        let
            exception Cont of unit cont
        in
            (callcc (fn k => raise (Cont k));
            raise exn) handle Cont k => k
        end
    in
        fun memoryLimit interval t m f x =
        let
            val max_used = ref 0
            val fail_mem_k = make_fail_cont OutOfMemory
            val fail_time_k = make_fail_cont TimeOut
            fun timerOn interval = ignore (setIntTimer (SOME interval))
            fun timerOff () = ignore (setIntTimer NONE)
            val _ = doGC 100
            val startSize = getHeapSize ()
            val startTime = Time.now()
            fun handler (_, _, normal_k) =
            let
                val _ = timerOff()
                val now = Time.now()
                val spent = Time.-(now, startTime)
                val _ = doGC 100
                val currentSize = getHeapSize ()
                val used = currentSize - startSize
                val _ = max_used := (Int.max (!max_used, used))
                val _ = timerOn interval
            in
                if Time.>(spent, t)
                then (timerOff(); fail_time_k)
                else if used > m
                then (timerOff(); fail_mem_k)
                else normal_k
            end
            val _ = 
                Signals.setHandler (Signals.sigALRM, Signals.HANDLER handler)
            val _ = timerOn interval
            val result = f x handle exn => (timerOff(); raise exn)
            val _ = timerOff()
            val _ = doGC 100
            val currentSize = getHeapSize()
        in
            (result, currentSize - startSize, Int.max (!max_used, startSize - currentSize))
        end
    end
end
