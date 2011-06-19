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
(*
  Module:	    Manual Glue (MNG)

  Description:	    Glue for the functions using the CmdProcess module
		    to process requests.
		    It is called MANUAL because it is in contrast with
		    the other AUTOMATIC glue available. The auto glue
		    processes arguments and return values automatically.
		    With this glue, you are responsible for processing
		    the inputs yourself and extracting the return value
		    yourself.

  CPN Tools
*)


(* $Source: /users/cpntools/repository/cpn2000/sml/com/manualGlue.sml,v $ *)

val rcsid = "$Header: /users/cpntools/repository/cpn2000/sml/com/manualGlue.sml,v 1.1.1.1 2001/10/02 11:34:22 cpn Exp $";


import "cmdProcess.sig";
import "unsafe.sig";
import "manualGlue.sig";


functor ManualGlue (structure Cmd : CMDPROCESS
		    structure Unsafe : GRAMUNSAFE) : MANUALGLUE = struct

    val rcsid = "$Header: /users/cpntools/repository/cpn2000/sml/com/manualGlue.sml,v 1.1.1.1 2001/10/02 11:34:22 cpn Exp $";
    
    structure Cmd = Cmd;
    structure Str = Cmd.Str;

    val buf = ByteArray.array(8192,0);


    (* 
     * invoke
     *
     * This acts as an intermediary between the many functions modules
     * and the CmdProcess and Stream modules. The function modules
     * provide the arguments and read the results from a byte array.
     * This function sends that byte array via a stream, waits from
     * the result, and reads the result back into the byte array.
     *
     * TEMP FIX: eventually all the modules should use streams 
     *           directly.
     *
     *)

    fun invoke (ba,i,sz) = let
	val (outs,ins) = Cmd.getStreams (!Cmd.theGram);
    in
	Str.putInteger outs sz;
	Str.putBytes outs (ba,i,sz);
	Str.flush outs;
	Cmd.waitWoutEval (!Cmd.theGram, Cmd.Result);
	Str.getBytes ins (ba,0, Str.getInteger ins)
    end


    structure Utils = struct	    

	(*
	 * intputa --
	 *
	 * put integer in a byte array at position i
	 * 
	 *)

	val intputa = Unsafe.putInteger;

    
	(*
	 * intgeta --
	 *
	 * get integer from bytearray at index i
	 *
	 *)
    
	val intgeta = Unsafe.getInteger;

    
	(*
	 * int2str --
	 *
	 * convert int to a string containing internal rep of int
	 *
	 *)
    
	fun int2str (x) =
	    let 
		val ba = ByteArray.array(4,0);
	    in
		(intputa(ba,0,x); 
		 ByteArray.extract(ba,0,4))
	    end;


	(*
	 * str2ba
	 *
	 * convert string into byte array. 
	 * copy sz chars from pos is of str to pos ib of ba.
	 *
	 *)

	fun str2ba (str, ba, is, ib, sz) =
	    let
		fun str2ba' (js,jb) = 
		    if (js < is) 
			then ()
		    else (ByteArray.update(ba,jb,Char.ord(String.sub(str,js))); 
                          str2ba' (js-1,jb-1));
	    in
		str2ba' ((is+sz) -1, ((ib+sz) - 1))
	    end
    


	(*
	 * strputa --
	 *
	 * put string in a byte array at position i 
	 * first 4 bytes contain the len of the string
	 * each subsequent byte holds the ASCII code of each char in str
	 * return num of bytes written into ba i.e. 4+length(str).
	 * If the string is of odd len, this fn appends a null char to the
	 * string and inc's the length before writing it into the ba.
	 * Hence all strings put into a ba using this fn will be of
	 * even length.
	 * 
	 *) 

	fun strputa(ba, i, str) = 
	    let
		fun padd 0 = ""
		  | padd 1 = "\000"
		  | padd 2 = "\000\000"
		  | padd 3 = "\000\000\000"
		  | padd _ = "";
		val nstr = str ^ (padd (4 - ((String.size str) mod 4)));
		val len = String.size(nstr)  
	    in
		(intputa(ba, i, len);
		 str2ba(nstr, ba, 0, i+4,len);
		 (i+4+len))
	    end

	(*
	 * strgeta --
	 *
	 * update i to next value in ba, return string
	 *)

	fun strgeta(ba,i) = 
	    let
		val slen = intgeta(ba,!i);
		val str = ByteArray.extract(ba, !i+4, slen);
	    in
		(i := !i + 4 + slen + (4 - (slen mod 4)); str)
	    end


	(*
	 * boolputa --
	 *)
	fun boolputa(ba,i,boolv) = if boolv then intputa(ba,i,1)
				   else intputa(ba,i,0)


	(*
	 * boolgeta --
	 *
	 *	!= 0 - true
	 *	== 0 - false
	 *)
	fun boolgeta(ba,i) = if intgeta(ba,i) <> 0 then true else false


	(*
	 * realputa --
	 * 
	 * takes the real and converts into a string
	 * then treat it as a string
	 *
	 *)

	fun realputa(ba, i, re) = 
	    let 
                val real_str = Real.toString re;
	    in
		strputa(ba, i, real_str)
	    end

	(*
         * boollistputa --
         * 
         * put ml boolean list into byte array starting at position i
         *
         *      format:  < of items><item 1>...<item n>
         *
         *)
        fun boollistputa(ba, i, l) =
	    let
		fun boollistputa'(i, hd::tl) = (boolputa(ba, i, hd);
						boollistputa'(i+4, tl))
		  | boollistputa'(i, []) = i;
		val _ = intputa(ba, i, (length l));
	    in
		boollistputa'(i+4, l)
	    end

	(*
	 * intlistputa --
	 * 
	 * put ml integer list into byte array starting at position i
	 *
	 *	format:	 <# of items><item 1>...<item n>
	 *
	 *)
	fun intlistputa(ba, i, l) =
	    let
		fun intlistputa'(i, hd::tl) = (intputa(ba, i, hd);
					       intlistputa'(i+4, tl))
		  | intlistputa'(i, []) = i
	    in
		(intputa(ba, i, (length l));
		 intlistputa'(i+4, l))
	    end
	 

	(*
	 * intlistgeta --
	 * 
	 *)
	fun intlistgeta(ba, i) =
	    let
		val len = intgeta(ba,i) 
		fun intlistgeta' (i,0) = []
		  | intlistgeta' (i,l) = intgeta(ba,i) 
		                         :: intlistgeta'(i+4,l-1);
	    in
		intlistgeta'(i+4,len)
	    end

	(*
	 * strlistputa --
	 * 
	 * put ml string list into byte array starting at position i
	 *
	 *	format:	 <# of items><item 1>...<item n>
	 *
	 *)
	fun strlistputa(ba, i, l) =
	    let
		fun strlistputa'(i, hd::tl) = 
			let
				val j = ref 0;
			in
			   (
				j := strputa(ba, i, hd);
				strlistputa'(!j, tl)
			   )
			end
		  | strlistputa'(i, []) = i
	    in
		(intputa(ba, i, (length l));
		 strlistputa'(i+4, l))
	    end

	(*
	 * reallistputa --
	 * 
	 * put ml real list into byte array starting at position i
	 *
	 *	format:	 <# of items><item 1>...<item n>
	 *
	 *)
	fun reallistputa(ba, i, l) =
	    let
		fun reallistputa'(i, hd::tl) = 
			let
				val j = ref 0;
			in
			   (
				j := realputa(ba, i, hd);
				reallistputa'(!j, tl)
			   )
			end
		  | reallistputa'(i, []) = i
	    in
		(intputa(ba, i, (length l));
		 reallistputa'(i+4, l))
	    end

	val DbIdputa = Unsafe.putdbid;
	val DbIdgeta = Unsafe.getdbid;


	fun DbIdlistgeta(ba, i) =
	    let
		val len = intgeta(ba,i) 
		fun DbIdlistgeta' (i,0) = []
		  | DbIdlistgeta' (i,l) = DbIdgeta(ba,i) 
		                         :: DbIdlistgeta'(i+4,l-1);
	    in
		DbIdlistgeta'(i+4,len)
	    end
	val equaldbid = Unsafe.equaldbid;
	val makestringdbid = Unsafe.makestringdbid;

    end;


end; (* functor ManualGlue *)

