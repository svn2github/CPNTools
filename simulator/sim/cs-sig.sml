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
(* File: cs-sig.sml
 *
 * Interface description for color-set structures.
 *)

signature COLORSET = sig
    type cs;
    
    val base  : cs;
    
    val lt       : cs * cs -> bool;
    val cmp	 : cs * cs -> order;
    val mkstr    : cs -> string;
    val mkstr_ms : cs CPN'MS.ms -> string;

    val legal    : cs -> bool;
    val illegal_msg : cs -> string; (* if legal return "" else reason why arg is illegal *)
    val size     : unit -> int;

    val all      : unit -> cs CPN'MS.ms;
    val input    : TextIO.instream -> cs;
    val input_ms : TextIO.instream -> cs CPN'MS.ms;
    val output   : TextIO.outstream * cs -> unit;
    val output_ms: TextIO.outstream * cs CPN'MS.ms -> unit;
	
    val col      : int -> cs;
    val ord      : cs -> int;

    val ran      : unit -> cs;
end;


signature CPN'COLORSETS = sig

    structure Time: CPN'TIME

    functor UnitCS (val CPN'str: string option) : COLORSET
    functor BoolCS (val CPN'arg: (string * string) option) : COLORSET
    structure IntCS : COLORSET
    functor IntWithCS (val CPN'low: int and CPN'high: int): COLORSET
    structure IntInfCS : COLORSET
    functor IntInfWithCS (val CPN'low: IntInf.int and CPN'high: IntInf.int): COLORSET
    structure RealCS : COLORSET
    functor RealWithCS (val CPN'low: real and CPN'high: real): COLORSET
    structure CharCS : COLORSET
    functor CharWithCS (val CPN'low: char and CPN'high: char): COLORSET
    structure StringCS : COLORSET
    functor StringWithCS (val CPN'low: string and CPN'high: string): COLORSET 
    functor StringWithLengthCS (val CPN'low: string and CPN'high: string
				and CPN'min: int and CPN'max: int): COLORSET
    functor EnumCS(type cs; val CPN'enm: string list): COLORSET
    functor IndexCS (type cs; val CPN'idx: string
		     and CPN'con: int -> cs and CPN'clr: cs -> int): COLORSET
    functor IndexWithCS (type cs;
                         val CPN'low: int and CPN'high: int and CPN'idx: string
			 and CPN'con: int -> cs and CPN'clr: cs -> int): COLORSET
    functor ListCS (structure cs: COLORSET): COLORSET
    functor ListWithCS (structure cs: COLORSET; 
			val CPN'min: int and CPN'max: int): COLORSET
    functor ListSubsetCS (structure cs: COLORSET;
			  val CPN'subset: cs.cs list): COLORSET
    functor FunSubsetCS (structure cs: COLORSET;
			 val CPN'subset: cs.cs -> bool): COLORSET
    functor TimedCS (structure cs: COLORSET
                     and Time: CPN'TIME): COLORSET

    val out_of_range: string
    val error_not_use: string * string -> 'a
    val error_not_decl: string * string -> 'a
    val error_ill_decl: string * string -> 'a
    exception ErrorLowHigh
    val error_low_high: string -> string
    exception ErrorMinMax
    val error_min_max: string -> string
    exception ErrorNotChar
    val error_not_char: string
end
