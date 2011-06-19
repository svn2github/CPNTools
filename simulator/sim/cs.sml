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
(* File: cs.sml
 *
 * Colour-set structures.
 *)

functor CPN'CreateColorSets(structure Time: CPN'TIME): CPN'COLORSETS = struct

    structure Time = Time;

    open CPN'StreamIO

    val out_of_range = "Out of range!"

    fun range_error(illegal,low,high) = " Illegal color: "^illegal^
	" not in range "^low^".."^high^"."

    fun error_not_use (f,cs) =
	raise CPN'Error(concat["Error: function ",f,
			       " can not be used in ",cs," color-set!"])

    fun error_not_decl (f,cs) =
	raise CPN'Error(concat["Error: function ",f,
			       " not declared in color-set ",cs,"!"])
    fun error_ill_decl (decl,cs) = 
	raise CPN'Error(concat["Error: illegal declare clause ",decl,
			       " used in color-set ",cs,"!\n"])
    exception ErrorLowHigh
    fun error_low_high cs = 
	concat["Error: in ",cs," with low..high must low<=high!"]
	
    exception ErrorMinMax
    fun error_min_max cs = 
	concat["Error: in ",cs," length min..max must 0<=min<=max!"]
	
    exception ErrorNotChar
    val error_not_char = "Error: string must be a single char!"

    fun error_illegal_token value cskind = 
	concat["Error: illegal token ",value," in ",cskind," color-set"]


functor TimedCS (structure cs: COLORSET
		 and Time: CPN'TIME): COLORSET = struct

    type cs = cs.cs Time.timed

    val base = Time.@(cs.base,Time.null)

    fun all () = error_not_use("all","timed")

    fun lt (Time.@(c1,t1),Time.@(c2,t2)) = 
	(* note that a big time value is less than a small time value *)
	cs.lt(c1,c2) orelse (Time.lt(t2,t1) andalso not (cs.lt(c2,c1)))

    fun cmp (Time.@(c1,t1),Time.@(c2,t2)) = 
	case cs.cmp(c1,c2) of
	    EQUAL => Time.cmp(t2,t1)
	  | rel => rel

    fun mkstr (Time.@(c,t)) = concat[cs.mkstr c,"@",Time.mkstr t]
	
    val mkstr_ms = CPN'MS.gen_mkstr_ms "+++" (mkstr,lt)
	
    fun input s = let
	val c = case get_until(s,[#"@"]) of
	    (#"@", value) => implode value
	  | (_, value) =>
		raise IOError("Can not find '@' in "^(implode value))
	val t = implode(get_next s)
    in
	Time.@(cs.input (TextIO.openString c), Time.maketime t)
    end

    fun output (s,Time.@(c,t)) =
	(TextIO.output(s,cs.mkstr c);
	 TextIO.output1(s,#"@");
	 TextIO.output(s,Time.mkstr t);
	 TextIO.output1(s,#" "))

    val input_ms = CPN'MS.input_ms true input 
    val output_ms = CPN'MS.output_ms true (output,lt)  

    fun legal (Time.@(c,_)) = cs.legal c
    fun illegal_msg (Time.@(c,_)) = cs.illegal_msg c
	
    fun size() = error_not_use("size","timed")
    fun ord _  = error_not_use("ord","timed")
    fun col _  = error_not_use("col","timed")
    fun ran () = error_not_use("ran","timed")
end


functor UnitCS (val CPN'str: string option): COLORSET = struct

    val unit = case CPN'str of NONE => "()" | SOME(str) => str

    type cs = unit

    val base = ()

    fun all () = [()]

    fun lt _ = false
    fun cmp _ = EQUAL
    fun mkstr _ = unit
    val mkstr_ms : cs CPN'MS.ms -> string = CPN'MS.mkstr_ms (mkstr,lt)
    fun input s = let
	val value = implode(get_next s)
    in
	if value=unit then base 
	else raise IOError(error_illegal_token value "unit")
    end

    fun output (s,c) = TextIO.output(s,(mkstr c)^" ")

    val input_ms : TextIO.instream -> cs CPN'MS.ms = CPN'MS.input_ms false input
    val output_ms : TextIO.outstream * cs CPN'MS.ms -> unit = CPN'MS.output_ms false (output,lt)

    fun legal _ = true;
    fun illegal_msg _ = ""

    fun size () = 1
    fun ord () = 0
    fun col 0 = ()
      | col i = raise CPN'Error out_of_range
    fun ran () = ()
end


functor BoolCS (val CPN'arg: (string * string) option) : COLORSET = struct

    val (low,high) = case CPN'arg of NONE => ("false","true") | SOME(str) => str

    type cs = bool

    val base = false
	
    fun all () = [false,true]

    fun lt(a,b) = b andalso not a

    fun cmp (false,true) = LESS
      | cmp (true,false) = GREATER
      | cmp _ = EQUAL

    fun mkstr false = low
      | mkstr true = high

    val mkstr_ms : cs CPN'MS.ms -> string = CPN'MS.mkstr_ms (mkstr,lt)

    fun input s = let
	val token = implode(get_next s)
    in
	if token=low then false 
	else if token=high then true 
	else raise IOError (error_illegal_token token "bool")
    end

    fun output (s,c) = TextIO.output(s,(mkstr c)^" ")

    val input_ms : TextIO.instream -> cs CPN'MS.ms = CPN'MS.input_ms false input
    val output_ms : TextIO.outstream * cs CPN'MS.ms -> unit = CPN'MS.output_ms false (output,lt)

    fun legal _ = true;
    fun illegal_msg _ = ""

    fun size () = 2
    fun ord false = 0
      | ord true = 1
    fun col 0 = false
      | col 1 = true
      | col i = raise CPN'Error out_of_range
    fun ran () = col (CPN'Random.int 2) 
end


structure IntCS: COLORSET = struct

    type cs = int

    val base = 0

    fun all () = error_not_use("all","int")

    val lt = Int.<
    val cmp = Int.compare

    fun mkstr i = 
	if i < 0 
	then "("^Int.toString i^")"
	else Int.toString i

    val mkstr_ms : cs CPN'MS.ms -> string = CPN'MS.mkstr_ms (mkstr,lt)

    fun input (s: TextIO.instream) = 
	let
	    val next = get_next s
	    val intstr = case (hd next, List.last next) of 
			     (#"(",#")") => List.take(tl next,(length next-2))
			   | _ => next
	in
	    valOf(Int.fromString(implode intstr))
	end

    fun output (s,c) = TextIO.output(s,(mkstr c)^" ")

    val input_ms : TextIO.instream -> cs CPN'MS.ms = CPN'MS.input_ms false input
    val output_ms : TextIO.outstream * cs CPN'MS.ms -> unit = CPN'MS.output_ms false (output,lt)

    fun legal _ = true;
    fun illegal_msg _ = ""

    fun size() = error_not_use("size","int")
    fun ord _  = error_not_use("ord","int")
    fun col _  = error_not_use("col","int")
    fun ran()  = error_not_use("ran","int")
end

functor IntWithCS (val CPN'low: int and CPN'high: int): COLORSET = struct

    val _ = if CPN'low <= CPN'high then () else raise ErrorLowHigh

    type cs = int

    val base = CPN'low

    local
	val all_ref = ref (NONE: cs CPN'MS.ms option)
    in
	fun all () =
	    case !all_ref of
		SOME ms => ms
	      | NONE => 
		let
		    val ms = Misc.I.fold (op ::) (CPN'low,CPN'high+1) nil
		in
		    all_ref:= SOME ms;
		    ms
		end
    end

    val lt = Int.<
    val cmp = Int.compare

    fun mkstr i =  
	if i < 0 
	then "("^Int.toString i^")"
	else Int.toString i
    val mkstr_ms : cs CPN'MS.ms -> string = CPN'MS.mkstr_ms (mkstr,lt)

    fun legal i = CPN'low<=i andalso i<=CPN'high
    fun illegal_msg i = 
	if legal i then ""
	else range_error(mkstr i, mkstr CPN'low, mkstr CPN'high)

    fun input s = 
	let
	    val next = get_next s
	    val intstr = case (hd next, List.last next) of 
			     (#"(",#")") => List.take(tl next,(length next-2))
			   | _ => next
	    val i = valOf(Int.fromString(implode intstr))
	in
	    if legal i then i 
	    else raise CPN'Error (out_of_range^illegal_msg(i))
	end

    fun output (s,c) = 
	if legal c then TextIO.output(s,(mkstr c)^" ")
	else raise CPN'Error (out_of_range^illegal_msg(c))

    val input_ms : TextIO.instream -> cs CPN'MS.ms = CPN'MS.input_ms false input
    val output_ms : TextIO.outstream * cs CPN'MS.ms -> unit = CPN'MS.output_ms false (output,lt)

    fun size () = CPN'high - CPN'low + 1
    fun ord i = if legal i then i - CPN'low 
		else raise CPN'Error out_of_range
    fun col i =	if legal (i + CPN'low) then i + CPN'low 
		else raise CPN'Error out_of_range
    fun ran ()  = col (CPN'Random.int (size()))
end


structure IntInfCS: COLORSET = struct

    type cs = IntInf.int

    val base = valOf (IntInf.fromString "0")

    fun all () = error_not_use("all","intinf")

    val lt = IntInf.<
    val cmp = IntInf.compare
    val mkstr = IntInf.toString
    val mkstr_ms : cs CPN'MS.ms -> string = CPN'MS.mkstr_ms (mkstr,lt)

    val input: TextIO.instream -> IntInf.int = valOf o IntInf.fromString o implode o get_next
	 
    fun output (s,c) = TextIO.output(s,(mkstr c)^" ")

    val input_ms : TextIO.instream -> cs CPN'MS.ms = CPN'MS.input_ms false input
    val output_ms : TextIO.outstream * cs CPN'MS.ms -> unit = CPN'MS.output_ms false (output,lt)

    fun legal _ = true;
    fun illegal_msg _ = ""

    fun size() = error_not_use("size","intinf")
    fun ord _  = error_not_use("ord","intinf")
    fun col _  = error_not_use("col","intinf")
    fun ran()  = error_not_use("ran","intinf")
end

functor IntInfWithCS (val CPN'low: IntInf.int and CPN'high: IntInf.int): COLORSET = struct

    val _ = if IntInf.<=(CPN'low,CPN'high) then () else raise ErrorLowHigh

    type cs = IntInf.int

    val base = CPN'low

    local
	val all_ref = ref (NONE: cs CPN'MS.ms option)
    in
	fun all () =
	    case !all_ref of
		SOME ms => ms
	      | NONE => 
		let
		    fun accum (l,h,res) =
			if IntInf.<(h,l) then res
			else accum(IntInf.+(l,IntInf.fromInt 1),h,l::res)
			    val ms= accum(CPN'low,CPN'high,nil)
		in
		    all_ref:= SOME ms;
		    ms
		end
    end

    val lt = IntInf.<
    val cmp = IntInf.compare

    fun mkstr i = IntInf.toString i 

    val mkstr_ms : cs CPN'MS.ms -> string = CPN'MS.mkstr_ms (mkstr,lt)

    fun legal i = IntInf.<=(CPN'low,i) andalso IntInf.<=(i,CPN'high)

    fun illegal_msg i = 
	if legal i then ""
	else range_error(mkstr i,mkstr CPN'low,mkstr CPN'high)

    fun input s = let
	val i = (valOf o IntInf.fromString o implode o get_next) s 
    in
	if legal i then i 
	else raise CPN'Error (out_of_range^illegal_msg(i))
    end

    fun output (s,c) = 
	if legal c then TextIO.output(s,(mkstr c)^" ") 
	else raise CPN'Error (out_of_range^illegal_msg(c))

    val input_ms : TextIO.instream -> cs CPN'MS.ms = CPN'MS.input_ms false input
    val output_ms : TextIO.outstream * cs CPN'MS.ms -> unit = CPN'MS.output_ms false (output,lt)

    fun size () =  IntInf.toInt(IntInf.+(IntInf.-(CPN'high,CPN'low),IntInf.fromInt 1))
    fun ord i = if legal i then IntInf.toInt(IntInf.-(i,CPN'low))
		else raise CPN'Error out_of_range
    fun col i =	if legal(IntInf.+(IntInf.fromInt i,CPN'low)) then IntInf.+(IntInf.fromInt i,CPN'low)
		else raise CPN'Error out_of_range
    fun ran ()  = col(CPN'Random.int (size()))
end


structure RealCS: COLORSET = struct

    type cs = real

    val base = 0.0

    fun all () = error_not_use("all","real")

    val lt = Real.<
    val cmp = Real.compare
    val mkstr = Real.toString
    val mkstr_ms : cs CPN'MS.ms -> string = CPN'MS.mkstr_ms (mkstr,lt)

    val input: TextIO.instream -> real = valOf o Real.fromString o implode o get_next

    fun output (s,c) = TextIO.output(s,(mkstr c)^" ")

    val input_ms : TextIO.instream -> cs CPN'MS.ms = CPN'MS.input_ms false input
    val output_ms : TextIO.outstream * cs CPN'MS.ms -> unit = CPN'MS.output_ms false (output,lt)

    fun legal _ = true;
    fun illegal_msg _ = ""

    fun size() = error_not_use("size","real")
    fun ord _  = error_not_use("ord","real")
    fun col _  = error_not_use("col","real")
    fun ran()  = error_not_use("ran","real")
end

functor RealWithCS (val CPN'low: real and CPN'high: real): COLORSET = struct

    val _ = if Real.<=(CPN'low,CPN'high) then () else raise ErrorLowHigh
			
    type cs = real

    val base = CPN'low

    fun all () = error_not_use("all","real")

    val lt = Real.<
    val cmp = Real.compare

    fun mkstr r = Real.toString r 

    val mkstr_ms : cs CPN'MS.ms -> string = CPN'MS.mkstr_ms (mkstr,lt)

    fun legal r = Real.<=(CPN'low,r) andalso Real.<=(r,CPN'high)

    fun illegal_msg r = 
	if legal r then ""
	else range_error(mkstr r,mkstr CPN'low,mkstr CPN'high)

    fun input s = let
	val r = (valOf o Real.fromString o implode o get_next) s
    in
	if legal r then r 
	else raise CPN'Error (out_of_range^illegal_msg(r))
    end

    fun output (s,c) = 
	if legal c then TextIO.output(s,(mkstr c)^" ") 
	else raise CPN'Error (out_of_range^illegal_msg(c))

    val input_ms : TextIO.instream -> cs CPN'MS.ms = CPN'MS.input_ms false input
    val output_ms : TextIO.outstream * cs CPN'MS.ms -> unit = CPN'MS.output_ms false (output,lt)

    fun size() = error_not_use("size","real")
    fun ord _  = error_not_use("ord","real")
    fun col _  = error_not_use("col","real")
    fun ran()  = CPN'Random.real(CPN'high - CPN'low) + CPN'low
end


structure CharCS: COLORSET = struct

    type cs = char

    val base = Char.minChar

    local
	val all_ref = ref (NONE: cs CPN'MS.ms option)
    in
	fun all () =
	    case !all_ref of
		SOME ms => ms
	      | NONE => 
		let
		    fun mk (ch: char) = 
			if ch < Char.maxChar then ch::mk(Char.succ ch)
			else [Char.maxChar]
		    val ms = mk Char.minChar
		in
		    all_ref:= SOME ms;
		    ms
		end
    end

    val lt = Char.<
    val cmp = Char.compare
    val mkstr = Char.toString
    val mkstr_ms : cs CPN'MS.ms -> string = CPN'MS.mkstr_ms (mkstr,lt)

    val input: TextIO.instream -> char  = valOf o Char.fromString o implode o get_next
	 
    fun output (s,c) = TextIO.output(s,(mkstr c)^" ")

    val input_ms : TextIO.instream -> cs CPN'MS.ms = CPN'MS.input_ms false input
    val output_ms : TextIO.outstream * cs CPN'MS.ms -> unit = CPN'MS.output_ms false (output,lt)

    fun legal _ = true;
    fun illegal_msg _ = ""

    fun size() = Char.maxOrd+1
    val ord = Char.ord
    val col = Char.chr
    fun ran() = col (CPN'Random.int (size()))
end


functor CharWithCS (val CPN'low: char and CPN'high: char): COLORSET = struct

    val _ = if CPN'low <= CPN'high then () else raise ErrorLowHigh

    type cs = char

    val base = CPN'low

    local
	val all_ref = ref (NONE: cs CPN'MS.ms option)
    in
	fun all () =
	    case !all_ref of
		SOME ms => ms
	      | NONE => 
		let
		    fun f (i,tail) = (Char.chr i)::tail
		    val ms = Misc.I.fold f (ord CPN'low,ord(CPN'high)+1) nil
		in
		    all_ref:= SOME ms;
		    ms
		end
    end

    val lt = Char.<
    val cmp = Char.compare

    fun mkstr ch = Char.toString ch

    val mkstr_ms : cs CPN'MS.ms -> string = CPN'MS.mkstr_ms (mkstr,lt)

    fun legal ch = CPN'low<=ch andalso ch<=CPN'high

    fun illegal_msg ch =
	if legal ch then ""
	else range_error(mkstr ch,mkstr CPN'low, mkstr CPN'high)

    fun input s = let
	val ch = (valOf o Char.fromString o implode o get_next) s 
    in
	if legal ch then ch
	else raise CPN'Error (out_of_range^illegal_msg(ch))
    end

    fun output (s,c) = 
	if legal c then TextIO.output(s,(mkstr c)^" ") 
	else raise CPN'Error (out_of_range^illegal_msg(c))

    val input_ms : TextIO.instream -> cs CPN'MS.ms = CPN'MS.input_ms false input
    val output_ms : TextIO.outstream * cs CPN'MS.ms -> unit = CPN'MS.output_ms false (output,lt)

    fun size () = Char.ord(CPN'high) - Char.ord(CPN'low) + 1
    fun ord ch = if legal ch then Char.ord(ch) - Char.ord(CPN'low)
		else raise CPN'Error out_of_range
    fun col i =	let 
	val ch = Char.chr(i + Char.ord CPN'low) 
    in
	if legal ch then ch
	else raise CPN'Error out_of_range
    end
    fun ran ()  = col (CPN'Random.int (size()))
end


structure StringCS: COLORSET = struct
    type cs = string

    val base = ""

    fun all () = error_not_use("all","string")

    val lt = String.<
    val cmp = String.compare
    fun mkstr s = concat ["\"",s,"\""]
    val mkstr_ms : cs CPN'MS.ms -> string = CPN'MS.mkstr_ms (mkstr,lt)

    local
	val ord_quote = Char.ord(#"\"")
        fun ordof (s,i) = Char.ord(String.sub(s,i))
    in
	fun input s = let
	    val token = (implode (CPN'StreamIO.get_next s))
	    val n = String.size token
	in
	    if n>=2 andalso
	       ordof(token,0)=ord_quote andalso 
	       ordof(token,n-1)=ord_quote then
	       String.substring(token,1,n-2)
	    else raise IOError (error_illegal_token token "string")
	end
    end

    fun output (s,c) = TextIO.output(s,(mkstr c)^" ")

    val input_ms : TextIO.instream -> cs CPN'MS.ms = CPN'MS.input_ms false input
    val output_ms : TextIO.outstream * cs CPN'MS.ms -> unit = CPN'MS.output_ms false (output,lt)

    fun legal _ = true
    fun illegal_msg _ = ""

    fun size() = error_not_use("size","string")
    fun ord _  = error_not_use("ord","string")
    fun col _  = error_not_use("col","string")
    fun ran()  = error_not_use("ran","string")
end

functor StringWithCS (val CPN'low: string and CPN'high: string): COLORSET = struct

    val _ = if CPN'low<=CPN'high then () else raise ErrorLowHigh 
    val _ = if (String.size CPN'low)=1 andalso (String.size CPN'high)=1 then ()
	    else raise ErrorNotChar

    type cs = string

    val base = ""

    fun all () = error_not_use("all","string")

    val lt = String.<
    val cmp = String.compare
	
    fun mkstr s = concat ["\"",s,"\""] 

    val mkstr_ms : cs CPN'MS.ms -> string = CPN'MS.mkstr_ms (mkstr,lt)

    fun legal s = let
	val CPN'low = valOf(Char.fromString CPN'low)
	val CPN'high = valOf(Char.fromString CPN'high)
    in
	foldr (fn (a,b) => CPN'low<=a andalso a<=CPN'high andalso b) true (explode s)
    end

    fun illegal_msg s = 
	if legal s then ""
	else
	    let 
		val lowch = valOf(Char.fromString CPN'low)
		val highch = valOf(Char.fromString CPN'high)
		fun not_in_range ch = ch<lowch orelse ch>highch
		val illegal_chars = List.filter not_in_range (explode s)
		val chrstr = concat(tl (foldr (fn (a,b) => ","::(mkstr (Char.toString a))::b) [""] illegal_chars))
	    in
		"Illegal color: "^(mkstr s)^". Range "^(mkstr CPN'low)^".."
		^(mkstr CPN'high)^" does not contain ["^chrstr^"]."
	    end
	    
    local
	val ord_quote = Char.ord(#"\"")
        fun ordof (s,i) = Char.ord(String.sub(s,i))
    in
	fun input s = let
	    val token = (implode (CPN'StreamIO.get_next s))
	    val n = String.size token
	    val token' = String.substring(token,1,n-2)
	in
	    if n>=2 andalso
	       ordof(token,0)=ord_quote andalso 
	       ordof(token,n-1)=ord_quote andalso
	       legal token' then token'
	    else raise IOError (error_illegal_token token "string (with)")
	end
    end

    fun output (s,c) = 
	if legal c then TextIO.output(s,(mkstr c)^" ") 
	else raise CPN'Error (out_of_range^illegal_msg(c))

    val input_ms : TextIO.instream -> cs CPN'MS.ms = CPN'MS.input_ms false input
    val output_ms : TextIO.outstream * cs CPN'MS.ms -> unit = CPN'MS.output_ms false (output,lt)

    fun size() = error_not_use("size","string")
    fun ord _  = error_not_use("ord","string")
    fun col _  = error_not_use("col","string")
    fun ran()  = error_not_use("ran","string")
end
   
functor StringWithLengthCS (val CPN'low: string and CPN'high: string
			    and CPN'min: int and CPN'max: int): COLORSET = struct

    val _ = if CPN'min<=CPN'max andalso 0<=CPN'min then () 
	    else raise ErrorMinMax
    val _ = if CPN'low<=CPN'high then () else raise ErrorLowHigh
    val _ = if (String.size CPN'low)=1 andalso (String.size CPN'high)=1 then ()
	    else raise ErrorNotChar

    val ordof = Char.ord o String.sub
    val ord_low = ordof(CPN'low,0)
    val ord_high = ordof(CPN'high,0)

    type cs = string

    val base = ""

    local
	open ByteArray;

	val all_ref = ref(NONE: cs CPN'MS.ms option)

	fun make_all 0 = ""::make_all(1)
	  | make_all n = let
	    val ba = array(n,ord_low)

	    fun make_str (i,tail) = let
		val ord_i = sub(ba,i)

		fun update_tail j =
		    if j<n then (update(ba,j,ord_low); update_tail(j+1))
		    else ()

		fun next (~1) = tail
		  | next j = let
		    val ord_j = sub(ba,j)
		 in
		    if ord_j<ord_high then
			(update(ba,j,ord_j+1);
			 update_tail (j+1);
			 make_str(n-1,tail))
		    else
			next (j-1)
		end
	     in
		extract(ba,0,n)::
		(if ord_i<ord_high then 
		     (update(ba,i,ord_i+1); make_str(i,tail))
		 else 
		      next(i-1))
	    end
	 in
	    if n<=CPN'max then make_str(n-1,make_all(n+1)) else nil
        end
    in
       fun all () =
	   case !all_ref of
	       SOME ms => ms
	     | NONE => 
	       let
		   val ms = make_all(CPN'min)
	       in
		   all_ref:= SOME ms; 
		   ms
	       end
    end

    val lt = String.<
    val cmp = String.compare

    fun mkstr s = concat ["\"",s,"\""] 

    val mkstr_ms : cs CPN'MS.ms -> string = CPN'MS.mkstr_ms (mkstr,lt)

    val legal = 
	if ord_low=0 andalso ord_high=255 then
	    fn s => let val n = String.size s in CPN'min<=n andalso n<=CPN'max end
	else
	    fn s =>
	    let 
		val n = String.size s
	
		fun legal' (~1) = true
		  | legal' i = 
		    if ord_low<=ordof(s,i) andalso ordof(s,i)<=ord_high then
			legal' (i-1)
		    else
			false
	    in
		CPN'min<=n andalso n<=CPN'max andalso legal'(n-1)
	    end

    fun illegal_msg s = 
	if legal s then ""
	else
	    let 
		val lowch = valOf(Char.fromString CPN'low)
		val highch = valOf(Char.fromString CPN'high)
		fun not_in_range ch = ch<lowch orelse ch>highch
		val illegal_chars = List.filter not_in_range (explode s)
		val chrstr = concat(tl (foldr (fn (a,b) => ","::(mkstr (Char.toString a))::b) [""] illegal_chars))
		val strlen = String.size s
		val strlen_legal = CPN'min<=strlen andalso strlen<=CPN'max
	    in
		"Illegal color: "^(mkstr s)^"."^
		(if chrstr="" then ""
		 else " Range "^(mkstr CPN'low)^".."^(mkstr CPN'high)^
		" does not contain ["^chrstr^"]"^".")^
		(if strlen_legal then ""
		 else " String length "^(Int.toString strlen)^" is out of range "^
		     (Int.toString CPN'min)^".."^(Int.toString CPN'max)^".") 
	    end

    local
	val ord_quote = Char.ord(#"\"")
    in
	fun input s = let
	    val token = (implode (CPN'StreamIO.get_next s))
	    val n = String.size token
	    val token' = String.substring(token,1,n-2)
	in
	    if n>=2 andalso
	       ordof(token,0)=ord_quote andalso 
	       ordof(token,n-1)=ord_quote andalso
	       legal token' then token'
	    else raise IOError (error_illegal_token token "string (with,and)")
	end
    end

    fun output (s,c) = 
	if legal c then TextIO.output(s,(mkstr c)^" ") 
	else raise CPN'Error (out_of_range^illegal_msg(c))

    val input_ms : TextIO.instream -> cs CPN'MS.ms = CPN'MS.input_ms false input
    val output_ms : TextIO.outstream * cs CPN'MS.ms -> unit = CPN'MS.output_ms false (output,lt)

    local
	val N = ord_high-ord_low+1;

	fun pow 0 = 1
	  | pow i = N*pow(i-1)
    in
	fun size() = 
	    case N of
		1 => CPN'max-CPN'min+1
	      | _ => (pow(CPN'max+1) - pow(CPN'min)) div (N-1)	     

	fun ord s = let
	    val n = String.size s
	    fun sum i = 
		if i<n then (ordof(s,i)-ord_low)*(n-i) + sum(i+1) else 0
	in
	    if legal s then
		case N of 
		    1 => n-CPN'min
		  | _ => (pow(n)-pow(CPN'min)) div (N-1) + (sum 0)
	    else raise CPN'Error out_of_range
	end
    end

    fun col i = List.nth(all(),i)
        handle _ => raise CPN'Error out_of_range

    fun ran () = let
	val diff = ord_high-ord_low+1

	fun make 0 = nil
	  | make i = Char.chr(CPN'Random.int(diff)+ord_low)::make(i-1)
    in
	implode(make(CPN'Random.int(CPN'max-CPN'min+1)+CPN'min))
    end
end

functor ListCS (structure cs: COLORSET): COLORSET = struct

    type cs = cs.cs list

    val base = nil

    fun all () = error_not_use("all","list")

    fun lt (x::xs,y::ys) = 
	if cs.lt(x,y) then true 
	else if cs.lt(y,x) then false
	else lt(xs,ys)
      | lt (_,nil) = false
      | lt (nil,_) = true

    val cmp = Misc.a_cmp lt

    fun mkstr list =
        concat("["::tl(foldr (fn (x,l) => ","::(cs.mkstr x)::l) ["","]"] list))

    val mkstr_ms : cs CPN'MS.ms -> string = CPN'MS.mkstr_ms (mkstr,lt)

    fun legal nil = true
      | legal (x::xs) = (cs.legal x) andalso (legal xs)

    fun illegal_msg xs = 
	if legal xs then ""
	else "Illegal values in list: "^
	    concat (tl (foldr (fn (a,b) => ","::(cs.mkstr a)::b) [""] 
			(List.filter (not o cs.legal) xs)))

    fun input s = let
	fun mk () = let
	    val (stop_char,token) = get_until(s,[#",",#"]"])
	    val stream = TextIO.openString(implode (token))
	in
	    case stop_char of
		#"," => cs.input(stream)::mk()
	      | #"]" => [cs.input stream] 
	      | _ => raise IOError ("Can not find ']' when reading a list") 
	end
    in
	(skip_white_spaces s;
	 case TextIO.input1 s of
	     SOME #"[" =>
		 (case TextIO.lookahead s of
		      SOME #"]" => nil
		    | _ => mk())
	   | _ => raise IOError ("Can not find '[' when reading a list"))
    end

    fun output (s,l) = TextIO.output(s,(mkstr l)^" ")

    val input_ms : TextIO.instream -> cs CPN'MS.ms = CPN'MS.input_ms false input
    val output_ms : TextIO.outstream * cs CPN'MS.ms -> unit = CPN'MS.output_ms false (output,lt)

    fun size() = error_not_use("size","list")
    fun ord _  = error_not_use("ord","list")
    fun col _  = error_not_use("col","list")
    fun ran()  = error_not_use("ran","list")
end

functor ListWithCS (structure cs: COLORSET; 
		    val CPN'min: int and CPN'max: int): COLORSET = struct

    val _ = if CPN'min <= CPN'max then () else raise ErrorMinMax

    type cs = cs.cs list

    val base = nil

    local
	open Array;

	val all_ref = ref(NONE: cs CPN'MS.ms option)

	fun make_all 0 = nil::make_all(1)
	  | make_all n = let
	    val domain = fromList (cs.all())
	    val N = length domain
	    
	    val a = array (n,0)

	    fun make_list (i,tail) = let
		val a_i = sub(a,i)

		fun update_tail j =
		    if j<n then (update(a,j,0); update_tail(j+1)) else ()

		fun next (~1) = tail
		  | next j = let
		    val a_j = sub(a,j)
		 in
		    if a_j<N-1 then
			(update(a,j,a_j+1);
			 update_tail (j+1);
			 make_list(n-1,tail))
		    else
			next (j-1)
		end

		fun extract (~1) = nil
		  | extract k = let
		    val a_k = sub(a,k)
		 in
		    (sub(domain,a_k)::extract(k-1))
		end
	    in
		extract(n-1)::
		(if a_i<N-1 then 
		     (update(a,i,a_i+1); make_list(i,tail))
		 else 
		     next(i-1))
	   end
	in
	    if n<=CPN'max then make_list(n-1,make_all(n+1)) else nil
       end
    in
	fun all () =
	    case !all_ref of
		SOME ms => ms
	      | NONE => 
		let
		    val ms = make_all(CPN'min)
		in
		    all_ref:= SOME ms; 
		    ms
		end
    end

    fun lt (x::xs,y::ys) = 
	if cs.lt(x,y) then true 
	else if cs.lt(y,x) then false
	else lt(xs,ys)
      | lt (_,nil) = false
      | lt (nil,_) = true

    val cmp = Misc.a_cmp lt

local
    fun inRange list = let
	val n = List.length list
    in
	CPN'min<=n andalso n<=CPN'max
    end
in
    fun mkstr list = concat("["::tl(foldr (fn (x,l) => ","::(cs.mkstr x)::l) 
			    ["","]"] list))

    val mkstr_ms : cs CPN'MS.ms -> string = CPN'MS.mkstr_ms (mkstr,lt)

    fun legal_colors list = 
	foldr (fn (a,b) => (cs.legal a) andalso b) true list

    fun legal list =
	(inRange list) andalso (legal_colors list)
	
    fun illegal_msg list = 
	if legal list then ""
	else 
	    let
		val illegalcolors = 
		    if not(legal_colors list)
			then " Illegal values in list: "^
			    concat (tl (foldr (fn (a,b) => ","::(cs.mkstr a)::b) ["."] 
					(List.filter (not o cs.legal) list)))
		    else ""
		val notInRange = 
		    if not (inRange list)
			then " List length of "^
			      (Int.toString (length list))^" out of range "^
			      Int.toString(CPN'min)^".."^Int.toString(CPN'max)^"."
		    else  ""
	    in
		"Illegal color."^illegalcolors^notInRange
	    end

    fun input s = let
	fun mk () = let
	    val (stop_char,token) = get_until(s,[#",",#"]"])
	    val stream = TextIO.openString(implode token)
	in
	    case stop_char of
		#"," => cs.input(stream)::mk()
	      | #"]" => [cs.input stream] 
	      | _ => raise IOError ("Can not find ']' when reading a list") 
	end
    
	val list = 
	    (skip_white_spaces s;
	     case TextIO.input1 s of
		 SOME #"[" => 
		     (case TextIO.lookahead s of
			  SOME #"]" => nil
			| _ => mk())
	       | _ => raise IOError ("Can not find '[' when reading a list"))
    in
	if inRange list then list 
	else raise CPN'Error (out_of_range^illegal_msg(list))
    end


    fun output (s,l) = if inRange l then TextIO.output(s,(mkstr l)^" ")
			 else raise CPN'Error (out_of_range^illegal_msg(nil))
end
    val input_ms : TextIO.instream -> cs CPN'MS.ms = CPN'MS.input_ms false input
    val output_ms : TextIO.outstream * cs CPN'MS.ms -> unit = CPN'MS.output_ms false (output,lt)

local
    fun pow (i,j) = let
        fun pow' (_,0) = 1
	  | pow' (i,1) = i
	  | pow' (i,j) = 
	    if j mod 2 = 0 then pow'(i*i, j div 2) else i*pow'(i*i, j div 2)
    in
	if 0<=j then pow'(i,j) else raise Domain
    end
in
    fun size() = let 
	val N = cs.size()
    in
	case N of
	    1 => CPN'max-CPN'min+1
	  | _ => (pow(N,CPN'max+1) - pow(N,CPN'min)) div (N-1) 
    end
	
    fun ord l = let
	val N = cs.size()
	val n = List.length l
	fun sum i = 
	    if i<n then cs.ord(List.nth(l,i))*(n-i) + sum(i+1) else 0
    in
	if legal l then
	    case N of
		1 => n-CPN'min
	      | _ => (pow(N,n)-pow(N,CPN'min)) div (N-1) + (sum 0)
	else raise CPN'Error out_of_range		     
    end
end

    fun col i = List.nth(all(),i)
	handle _ => raise CPN'Error out_of_range

    fun ran () = let
	fun make (n,l) = 
	    if List.length l = n then l else make(n,cs.ran()::l)
    in
	make (CPN'Random.int(CPN'max-CPN'min+1)+CPN'min,nil)
    end
end

functor IndexCS (type cs;
		 val CPN'idx: string
		 and CPN'con: int -> cs and CPN'clr: cs -> int): COLORSET = struct
			
    type cs = cs

    val base = CPN'con 0

    fun all () = error_not_use("all","index")

    fun lt (a,b) = (CPN'clr a) < (CPN'clr b)

    fun cmp (a,b) = Int.compare(CPN'clr a, CPN'clr b)

    fun mkstr i = concat [CPN'idx, "(", Int.toString (CPN'clr i), ")"]

    val mkstr_ms : cs CPN'MS.ms -> string = CPN'MS.mkstr_ms (mkstr,lt)

    fun legal _ = true

    fun illegal_msg _ = ""
			
    fun input s = let
	val index = implode(#2(get_until(s,[#"("])))
	val value = implode(#2(get_until(s,[#")"])))
    in
        if index = CPN'idx then CPN'con(valOf(Int.fromString value))
	else raise IOError ("Illegal identifier '"^index^"' in index")
    end

    fun output (s,c) = TextIO.output(s,(mkstr c)^" ")

    val input_ms : TextIO.instream -> cs CPN'MS.ms = CPN'MS.input_ms false input
    val output_ms : TextIO.outstream * cs CPN'MS.ms -> unit = CPN'MS.output_ms false (output,lt)
			
    fun size() = error_not_use("size","index")
    fun ord _  = error_not_use("ord","index")
    fun col _  = error_not_use("col","index")
    fun ran () = error_not_use("ran","index")
end

functor IndexWithCS (type cs;
                     val CPN'low: int and CPN'high: int
	    	     and CPN'idx: string
		     and CPN'con: int -> cs and CPN'clr: cs -> int): COLORSET = struct

    val _ = if CPN'low <= CPN'high then () else raise ErrorLowHigh

    type cs = cs
	    
    val base = CPN'con CPN'low
	    
    local
	val all_ref = ref (NONE: cs CPN'MS.ms option)
	fun make_all i = if i<=CPN'high then (CPN'con i)::make_all(i+1) else nil
    in
	fun all () =
	    case !all_ref of
		SOME ms => ms
	      | NONE =>
		let
		    val ms = make_all CPN'low
		in
		    all_ref:= SOME ms;
		    ms
		end
    end

    fun lt (a,b) = (CPN'clr a) < (CPN'clr b)

    fun cmp (a,b) = Int.compare(CPN'clr a, CPN'clr b)

    fun mkstr i = concat [CPN'idx, "(", Int.toString (CPN'clr i), ")"]

    val mkstr_ms : cs CPN'MS.ms -> string = CPN'MS.mkstr_ms (mkstr,lt)

    fun legal i = CPN'low<=(CPN'clr i) andalso (CPN'clr i)<=CPN'high

    fun illegal_msg i = 
	if legal i then ""
	else "Illegal color: "^mkstr i^", index "^Int.toString(CPN'clr i)^
	    " not in range "^Int.toString(CPN'low)^".."^Int.toString(CPN'high)^"."

    fun input s = let
	val index = implode(#2(get_until(s,[#"("])))
	val value = implode(#2(get_until(s,[#")"])))
    in
        if index = CPN'idx then
	    let 
		val v = CPN'con (valOf(Int.fromString value))
	    in
		if legal v then v 
		else raise CPN'Error (out_of_range^illegal_msg(v))
	    end
	else raise IOError ("Illegal identifier '"^index^"' in index")
    end

    fun output(s,c) = 
	if legal c then 
	    TextIO.output(s,(mkstr c)^" ")
	else
	    raise CPN'Error (out_of_range^illegal_msg(c))

    val input_ms : TextIO.instream -> cs CPN'MS.ms = CPN'MS.input_ms false input
    val output_ms : TextIO.outstream * cs CPN'MS.ms -> unit = CPN'MS.output_ms false (output,lt)

    fun size ()= CPN'high - CPN'low + 1
    fun ord i  = (CPN'clr i) - CPN'low
    fun col i  = CPN'con (i + CPN'low)
    fun ran () = col (CPN'Random.int (size()))
end

functor FunSubsetCS (structure cs: COLORSET;
		     val CPN'subset: cs.cs -> bool): COLORSET = struct
    type cs = cs.cs

    val base = cs.base
			
    local
	val all_ref = ref (NONE: cs CPN'MS.ms option)
    in
	fun all () =
	    case !all_ref of
		 SOME ms => ms
	       | NONE =>
		 let
		     fun f (a,b) = if CPN'subset a then a::b else b
		     val ms = foldr f nil (cs.all())
		 in
		     all_ref:= SOME ms;
		     ms
		 end
    end

    fun size () = List.length (cs.all()) 
	handle _ => raise CPN'Error out_of_range

    val lt = cs.lt

    val cmp = cs.cmp

    fun mkstr s = cs.mkstr s 

    val mkstr_ms : cs CPN'MS.ms -> string = CPN'MS.mkstr_ms (mkstr,lt)

    fun legal s = (CPN'subset s) andalso (cs.legal s)

    fun illegal_msg s = 
	if legal s then ""
	else if not (cs.legal s)
		 then cs.illegal_msg(s)
	     else "Illegal color: "^cs.mkstr s^" is not in subset color set."

    fun input s = let
	val v = cs.input s
    in
	if CPN'subset v then v 
	else raise CPN'Error (out_of_range^illegal_msg(v))
    end

    fun output(s,c) = 
	if CPN'subset c then cs.output(s,c) 
	else raise CPN'Error (out_of_range^illegal_msg(c))

    val input_ms : TextIO.instream -> cs CPN'MS.ms = CPN'MS.input_ms false input
    val output_ms : TextIO.outstream * cs CPN'MS.ms -> unit = CPN'MS.output_ms false (output,lt)

    local
	fun count (n,i,ncs) =
	    if i<ncs then
		if CPN'subset (cs.col i) then count(n+1,i+1,ncs) 
		else count(n,i+1,ncs)
	    else
		n

	fun find (n,i,ncol) =
	    if CPN'subset(cs.col i) then
		if n<ncol then 
		    find(n+1,i+1,ncol) 
		else
		    cs.col i
	    else 
		find(n,i+1,ncol)
    in
	fun ord s = 
	    if CPN'subset s then count(0,0,cs.ord s) 
	    else raise CPN'Error out_of_range

	fun col i = 
	    if 0<=i then find(0,0,i) 
	    else raise CPN'Error out_of_range
    end
    
    fun ran () = col (CPN'Random.int (size()))
end

functor ListSubsetCS (structure cs: COLORSET;
		      val CPN'subset: cs.cs list): COLORSET = struct

    type cs = cs.cs

    val base = hd CPN'subset

    fun all () = CPN'subset

    val lt = cs.lt

    val cmp = cs.cmp

    local
	fun eq a b = not(lt(a,b) orelse lt(b,a))
    in
	fun mkstr s = cs.mkstr s 

	val mkstr_ms = CPN'MS.mkstr_ms (mkstr,lt)

	fun legal s = (List.exists (eq s) CPN'subset) andalso (cs.legal s)
		       
	fun illegal_msg s = 
	    if legal s then ""
	    else if not (cs.legal s) 
		     then cs.illegal_msg s
		 else "Illegal color: "^mkstr s^" is not in subset color set."

	fun input s = let
	    val v = cs.input s
	in
	   if (List.exists (eq v) CPN'subset) then v 
	   else raise CPN'Error out_of_range
	end

	fun output(s,c) = 
	    if (List.exists (eq c) CPN'subset) 
		then TextIO.output(s,(mkstr c)^" ")
	    else raise CPN'Error (out_of_range^illegal_msg(c))

	val input_ms = CPN'MS.input_ms false input
	val output_ms = CPN'MS.output_ms false (output,lt)

	fun size () = List.length CPN'subset

	fun ord s = let
	    fun pos (i,x::xs) = if (eq s x) then i else pos (i+1,xs)
	      | pos (_,nil) = raise CPN'Error out_of_range
	in
	    pos(0,CPN'subset)
	end

	fun col i = List.nth(CPN'subset,i) handle _ => raise CPN'Error out_of_range

	fun ran () = col (CPN'Random.int (size()))
    end
end

(* The functor EnumCS is used when CPN'Settings.use_cast is true *)
functor EnumCS(type cs; val CPN'enm: string list): COLORSET = struct

    type cs = cs;

    val N = length CPN'enm;

    fun ord (c: cs) = (Unsafe.cast c): int

    fun col (i: int) = 
	if 0<=i andalso i<N then (Unsafe.cast i): cs
	else raise CPN'Error out_of_range

    val base = col 0;

    fun lt (x,y) = ord x < ord y

    fun cmp (x,y) = Int.compare(ord x, ord y)
	     
    fun mkstr c = List.nth(CPN'enm,ord c)

    val mkstr_ms = CPN'MS.mkstr_ms (mkstr,lt)

    fun legal (_: cs) = true
    fun illegal_msg _ = ""

    fun size() = N

    local
	val all_ref = ref (NONE: cs CPN'MS.ms option)
    in
	fun all () =
	     case !all_ref of
		 SOME ms => ms
	       | NONE =>
		 let
		     val ms = Misc.I.fold (fn (a,b) => (col a)::b) (0,N) nil
		 in
		     all_ref:= SOME ms; 
		     ms
		 end
    end

    fun input s = let
	open CPN'StreamIO;

	val token = implode(get_next s)

	fun get (i,x::xs) = if x=token then col i else get(i+1,xs)
	  | get (_,nil) = raise IOError ("Illegal enumeration token: "^token)
    in
	get (0,CPN'enm)
    end

    fun output (s,c) = TextIO.output(s,(mkstr c)^" ")

    val input_ms : TextIO.instream -> cs CPN'MS.ms = CPN'MS.input_ms false input
    val output_ms : TextIO.outstream * cs CPN'MS.ms -> unit = CPN'MS.output_ms false (output,lt)

    fun ran() = col (CPN'Random.int N)
end

end
