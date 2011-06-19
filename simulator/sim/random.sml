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
(* File: random.sml
 *
 * Pseudo random number generator.
 *)

structure CPN'Random: sig

    val init: int option -> unit
	    
    val int : int -> int;
    val int2 : int -> int;
    val real: real -> real

end = struct

    val a = 16807.0  and  m = 2147483647.0

    val seed_ref = ref 123.0
    val m_w = ref 0w1
    val m_z = ref 0w2

    (* Returns a number in [0.0,n) or (n,0.0] *)
    fun real n = let 
	val t = a*(!seed_ref)
    in
	(seed_ref:= t - m * Real.fromInt(floor(t/m));
	 (!seed_ref) / m * n)
    end

    (* Returns a number in [0,n) or (n,0] *)
    fun int n = trunc(real(Real.fromInt n))

        local open Word31 in
            fun int2 n =
            let val nn = Word31.fromInt (Int.abs n)
                val _ = m_z := (0w36969 * (andb(!m_z, 0w65535)) +
                (Word31.>>(!m_z, 0w16)) + 0w1)
                val _ = m_w := (0w18000 * (andb(!m_w, 0w65535)) +
                (Word31.>>(!m_w, 0w16)) + 0w1)
                val r = Word31.mod ((Word31.<<(!m_z, 0w16) + (!m_w)), nn)
            in
                if Int.>= (n, 0)
                then Word31.toInt r
                else Int.- (0, Word31.toInt r)
            end
        end

    val int = int2
        
    fun nowWord () = 
    let
        val n = Time.toNanoseconds (Time.now())
        val n' = IntInf.mod(n, IntInf.fromInt (Option.valOf (Int.maxInt)))
    in
        Word.fromInt (IntInf.toInt (n'))
    end

    (* Initializes the pseudo random number generator with the given number,
     * if NONE then the system clock is used as seed *)
    fun init NONE = (seed_ref:=(Time.toReal(Time.now())); m_w := (nowWord()); m_z
        := (nowWord()))
      | init (SOME seed) = (seed_ref:=(Real.fromInt seed); real 1.0; 
         m_w := (Word31.fromInt seed); m_z := (Word31.fromInt seed); int2 1; ())
end
