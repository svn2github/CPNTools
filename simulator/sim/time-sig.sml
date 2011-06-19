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
(* File: time-sig.sml
 *
 * Interface description for token time stamp types.
 *)

(*@i@
 This interface specifies the required operations for defining time
 modules used for CPN simulations with time.
 *)
signature CPN'TIME = sig
    
    (*@d@
     This is the abstract time type used throughout the module.
     *)
    type time;

    (*@d@
     This is the constructor type for timed values/tokens used in simulation.
     It is typically used in arc inscriptions.
     @user
     @exam["next pack"@11]
     *)
    datatype 'a timed = @ of 'a * time

    (*@d@
     This is the start time for the simulator when it is initialised.
     *)
    val start_time: time option
    (*@d@
     This is the current global time of the simulator.
     @note[Read only value. It is unsafe of change it directly.]
     *)
    val model_time: time ref

    (* The identification of the time type *)
    val name: string

    (*@d@
     This is the value representation of zero time.
     *)
    val null: time
    (*@d@
     This is the string representation of zero time.
     *)
    val null_str: string

    (*@d@
     Converts a time value into a string representation.
     *)
    val mkstr: time -> string
    val toString: time -> string
    (*@d@
     Converts a string into a time value if possible.
     @excn[CPN'Error|In case the conversion fails.]
     *)
    val maketime: string -> time
    val fromString: string -> time option

    (*@d@
     Converts an integer into a time value.
     *)
    val fromInt: int -> time

    (*@d@
     Converts an infinite integer into a time value.
     *)
    val fromIntInf: IntInf.int -> time

    (*@d@
     Converts a time value to a real
     *)
    val toReal: time -> real

    (*@d@
     Is the given time value @xref[ready] in CPN-terminology, i.e., is the 
     given time not greater than the @xref[model time].
     *)
    val ready: time -> bool
    (*@d@
     Reads the current model time.
     *)
    val time: unit -> time

    val add: time * time -> time
    val sub: time * time -> time
    val mult: time * time -> time
    val lt : time * time -> bool
    val leq: time * time -> bool
    val cmp: time * time -> order

    val col: 'a timed -> 'a
end
