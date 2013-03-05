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
(* File: ms.sml
 *
 * Facilities for external multi-sets.
 *)

infix 8 `;
infix 7 **;
infix 7 //;
infix 6 ++;
infix 6 --;
infix 3 ==;
infix 3 <><>;
infix 3 !=;
infix 3 <<;
infix 3 >>;
infix 3 <<=;
infix 3 >>=;
infix 7 %;

structure CPN'MS : MULTISET =
struct

    type 'a ms = 'a list;

    val empty = (nil : 'a ms);

    fun coef`col = if coef>0 then col::(coef-1)`col else nil

    fun ms1 ++ ms2 = List.@(ms1,ms2)

    fun ms1 -- nil = ms1
      | nil -- _   = raise Subtract
      | ms1 -- (col2::rms2) =
        let
            fun sub_item nil = raise Subtract
              | sub_item (col1::rms1) =
                if col1=col2 then rms1
                else col1::sub_item(rms1);
        in
            (sub_item ms1) -- rms2
        end
	    
    fun 0 ** ms1 = nil
      | n ** ms1 =
        if n>0 then
	    foldr (fn (a,b) => Misc.I.fold (fn (_,c) => a::c) (0,n) b) nil ms1
        else nil
        
    fun ms1 ==   ms2 = ((ms1 -- ms2) = nil) handle Subtract => false;
    fun ms1 <><> ms2 = not(ms1 == ms2);

    fun ms1 >>   ms2 = ((ms1 -- ms2) <><> nil) handle Subtract => false;
    fun ms1 >>=  ms2 = ((ms1 -- ms2); true) handle Subtract => false;
								      
    fun ms1 <<   ms2 = ms2 >>   ms1 
    fun ms1 <<=  ms2 = ms2 >>= ms1 

    val (op !=) = (op <><>);

    fun nil % x = [x]
      | (true::bs) % x = bs % x
      | (false::_) % _ = nil

    fun nil // x = x
      | (true::bs) // x = bs // x
      | (false::_) // _ = nil

    fun cf (col, ms) = foldr (fn (a,b) => if a=col then 1+b else b) 0 ms

    val size = List.length

    val sort_ms = Misc.sort

    fun legal_ms legal_cs ms = 
	foldr (fn (a,b) => legal_cs a andalso b) true ms
	
    fun illegal_msg_ms illegal_msg_cs ms = 
	let
	    fun illegal_msg (a,b) = 
		let
		    val res = illegal_msg_cs a
		in
		    if res = ""
			then b
		    else "\n"::res::b
		end
	in
	    concat(tl(foldr illegal_msg [""] ms))
	end

    fun gen_mkstr_ms sum_sym (mkstr_cs,lt) ms = let
	fun mkstr (coef,col,x::xs) = 
	    if lt (col,x) then
		(Int.toString coef)::"`"::(mkstr_cs col)::sum_sym::"\n"::mkstr(1,x,xs)
	    else 
		mkstr(coef+1,col,xs)
	  | mkstr (coef,col,nil) = [Int.toString coef,"`",mkstr_cs col]
    in
	case Misc.sort lt ms of
	    (x::xs) => concat(mkstr(1,x,xs))
	  | nil => "empty"
    end

    fun mkstr_ms (mkstr_cs,lt) ms = gen_mkstr_ms "++" (mkstr_cs,lt) ms

    fun input_ms istimed input_col s = let
	open CPN'StreamIO

	fun getEmpty s = 
	    case (implode (get_next s)) of
		"empty" => [] 
	      | token => raise IOError ("IOError: Unrecognized token when reading empty ms: "^token)

	fun getPlusses s = 
	    if not istimed 
		then  case (get_one s, get_one s) of
		    (SOME #"+",SOME #"+") => () 
		  | _ => raise IOError ("IOError: Could not find ++ when reading ms")
	    else (* reading timed ms *)
		case (get_one s, get_one s, get_one s) of
		    (SOME #"+",SOME #"+",SOME #"+") => () 
		  | _ => raise IOError ("IOError: Could not find +++ when reading tms")

	(* Always start by getting a coefficient. 
	 * Test in body to see if there is more to input *)
	fun getMS s = 
	    let 
		val (stop_char, coef_chars) = get_until (s,[#"`"])
		val coef_token = implode coef_chars
		val coef = 
		    case Int.fromString coef_token of
			SOME i => i
		      | NONE => raise IOError ("IOError: Expecting integer, found: "^(coef_token))
			    
		(* input_col often assumes that there is whitespace after the color *)
		val color = input_col s
	    in
		skip_white_spaces s; 
		case TextIO.lookahead s of
		    SOME _ => (getPlusses s; coef`color++(getMS s))
		  | NONE => coef`color++empty
	    end
		  
    in
	skip_white_spaces s;
	case TextIO.lookahead s of 
	    NONE =>  raise IOError("Empty file when reading a ms")
	  | SOME (#"e") => getEmpty s
	  | SOME c => if Char.isDigit c
			  then getMS s
		      else raise IOError("Unrecognized char ("^
					 (Char.toString c)^
					 ") when looking for a coef in an ms")
    end
 

    fun output_ms istimed (output_cs,lt) (s,ms) = let

	fun put (coef,col,x::xs) =
	    if lt(col,x) then
		 (TextIO.output(s,Int.toString coef);
		  TextIO.output1(s,#"`");
		  output_cs(s,col); (* color and one space *)
		  TextIO.output(s,"++"); 
		  if istimed 
		      then TextIO.output(s,"+ ")
		  else TextIO.output(s," ");
		  put (1,x,xs))
	    else put (coef+1,col,xs)
	  | put (coef,col,nil) =
	    (TextIO.output(s,Int.toString coef);
	     TextIO.output1(s,#"`");
	     output_cs(s,col);
	     TextIO.flushOut s)
    in
	case Misc.sort lt ms of
	    (x::xs) => put(1,x,xs)
	  | nil => TextIO.output(s,"empty ")
    end

    fun filter f ms = foldr (fn (a,b) => if f a then a::b else b) nil ms

    (* Return a random element from a (timed) multi-set. *)
    fun random nil = raise Empty
      | random ms = List.nth(ms,CPN'Random.int(size ms))

    val ext_col = List.map

    fun ext_ms f ms = flatten (map f ms)

    (* Return from a (timed) multi-set a random item and the remaining items.
     * Raise exception, exn, if multi-set is empty. *)
    fun get_ran exn nil = raise exn 
      | get_ran _ ms = 
	let 
	    fun get (0,col::rms) = (col,rms) 
	      | get (i,col::rms) = 
		let 
		    val (col',rms') = get(i-1,rms) 
		in 
		    (col',col::rms') 
		end 
	      | get _ = raise Match
	in 
	    get (CPN'Random.int(length ms),ms) 
	end

    (* convert a multiset with a single colour to the colour *)
    exception no_singleton;

    fun ms_to_col ms = if (List.length ms) = 1
			   then List.hd ms
		       else raise no_singleton
end;

open CPN'MS;

(* For compatability when converting from Design/CPN models *)
fun list_to_ms (CPN'l: 'a list) = (CPN'l: 'a ms)
fun ms_to_list (CPN'ms: 'a ms) = (CPN'ms: 'a list)
infix 3 <<==;
val (op <<==) = (op <<=);
infix 3 >>==
val (op >>==) = (op >>=);

(* SML/NJ overload does not work anymore *)
(* Control.overloadKW := true;
overload + :   ('a * 'a -> 'a)
  as  Int31.+ and Int32.+ and Int64.+ and IntInf.+
  and Word8.+ and Word31.+ and Word32.+ and Word64.+
  and Real64.+ and CPN'MS.++
overload - :   ('a * 'a -> 'a)
  as  Int31.- and Int32.- and Int64.- and IntInf.-
  and Word8.- and Word31.- and Word32.- and Word64.-
  and Real64.- and CPN'MS.--
overload * :   ('a * 'a -> 'a)
  as  Int31.* and Int32.* and Int64.* and IntInf.*
  and Word8.* and Word31.* and Word32.* and Word64.*
  and Real64.* and CPN'MS.**
overload /   : ('a * 'b -> 'b) as Real./ and CPN'MS.//;
overload < :   ('a * 'a -> bool)
  as  Int31.< and Int32.< and Int64.< and IntInf.<
  and W8.< and Word31.< and Word32.< and Word64.<
  and Real64.<
  and InlineT.Char.<
  and stringlt and CPN'MS.<<
overload > :   ('a * 'a -> bool)
  as  Int31.> and Int32.> and Int64.> and IntInf.>
  and W8.> and Word31.> and Word32.> and Word64.>
  and Real64.>
  and InlineT.Char.>
  and stringgt and CPN'MS.>>
(*_overload <=  : ('a * 'a -> bool) as Int.<= and Real.<= and Char.<= and
* String.<= and CPN'MS.<<==;*)
(*_overload >=  : ('a * 'a -> bool) as Int.>= and Real.>= and Char.>= and
* String.>= and CPN'MS.>>==;*)
(*  *)
overload size: ('a -> int) as String.size and CPN'MS.size;
Control.overloadKW := false;*)
