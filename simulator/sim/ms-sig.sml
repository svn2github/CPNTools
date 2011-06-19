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
(* File: ms-sig.sml
 *
 * Interface description for external multi-sets.
 *)

signature MULTISET = sig

    type 'a ms;

    val empty : 'a ms;

    val ` : int * 'a -> 'a ms;

    (* Arithmic operators *)
    val ++ : 'a ms * 'a ms -> 'a ms;
    val -- : ''a ms * ''a ms -> ''a ms
    val ** :   int * 'a ms -> 'a ms;

    (* Relational operators *)
    val ==   : ''a ms * ''a ms -> bool;
    val !=   : ''a ms * ''a ms -> bool;    
    val <><> : ''a ms * ''a ms -> bool;    
    val >>   : ''a ms * ''a ms -> bool;
    val >>= : ''a ms * ''a ms -> bool;
    val <<   : ''a ms * ''a ms -> bool;
    val <<= : ''a ms * ''a ms -> bool;

    (* Miscellaneous functions *)

    val %  : bool list * 'a -> 'a ms;
    val // : bool list * 'a ms -> 'a ms;

    (* return the coefficient of a colour-item in the multiset *)
    val cf : ''a * ''a ms -> int;
	
    (* return the number of elements (not colors) in the multiset *)
    val size : 'a ms -> int;
	
    (* verifies whether a multi-set is legal, i.e., whether all
     * member is legal color-set members. The first argument is
     * the color-set legal function, the second the ms *)
    val legal_ms: ('a -> bool) -> 'a ms -> bool

    (* returns string with reason why colors in the multi-set are illegal.
     * The first argument is the color-set illegal_msg function, the second the ms *)
    val illegal_msg_ms: ('a -> string) -> 'a ms -> string

    (* sort the multi-set. The first argument is a less than function,
     * the second the ms. *)
    val sort_ms: ('a * 'a -> bool) -> 'a ms -> 'a ms;
	
    (* make string representation of the multiset. The first
     * argument is a function that makes string representation of
     * the color. The second element is the ms *) 
    val mkstr_ms : (('a -> string) * ('a * 'a -> bool)) -> 'a ms -> string;
    val gen_mkstr_ms : string -> (('a -> string) * ('a * 'a -> bool)) -> 'a ms -> string;
	
    val input_ms : bool -> (TextIO.instream -> 'a) -> TextIO.instream -> 'a ms;
	
    val output_ms : bool -> ((TextIO.outstream * 'a -> unit) * ('a * 'a -> bool)) -> TextIO.outstream * 'a ms -> unit;

    (* return the items of the multiset for which the function is true *)
    val filter: ('a -> bool) -> 'a ms -> 'a ms;
	
    (* return a random appearence of the multiset *)
    val random : 'a ms -> 'a;  
	
    (* given a function with color domain and range plus a ms, apply the 
     * function to each item in the ms and return the obtained ms *)
    val ext_col : ('a -> 'b) -> 'a ms -> 'b ms;

    (* given a function with color domain and a ms, apply the function to 
     * each item in the ms and return the obtained ms *)
    val ext_ms : ('a -> 'b ms) -> 'a ms -> 'b ms;
	
    val get_ran : exn -> 'a ms -> 'a * 'a ms

    (* convert a multiset with a single colour to the colour *)
    exception no_singleton;

    val ms_to_col : 'a ms -> 'a;  

end;
