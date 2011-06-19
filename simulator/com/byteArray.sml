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
  Module:	    Byte Array Extensions (BAE)

  Description:	    Extensions to the ByteArray module provided by
  		    Standard ML of New 
  
  CPN Tools
*)


(* $Source: /users/cpntools/repository/cpn2000/sml/com/byteArray.sml,v $ *)

val rcsid = "$Header: /users/cpntools/repository/cpn2000/sml/com/byteArray.sml,v 1.2 2005/01/10 16:32:27 mw Exp $";


import "unsafe.sig";
import "byteArray.sig";


functor ByteArrayExt (structure Unsafe : GRAMUNSAFE) : BYTEARRAYEXT = struct

    val rcsid = "$Header: /users/cpntools/repository/cpn2000/sml/com/byteArray.sml,v 1.2 2005/01/10 16:32:27 mw Exp $";

    val stringToByteArray = Unsafe.stringToByteArray;
    val byteArrayToString = Unsafe.byteArrayToString;
    val putInteger = Unsafe.putInteger;
    val getInteger = Unsafe.getInteger;
    val putShortInteger = Unsafe.putShortInteger;
    val getShortInteger = Unsafe.getShortInteger;

    fun bcopy (srcBa,srcStart,desBa,desStart,size) =
	let
	    fun recBcopy ~1 = ()
	      | recBcopy n = 
		(ByteArray.update(desBa,desStart+n,
				  ByteArray.sub(srcBa,srcStart+n));
		 recBcopy (n-1))
	in
	    recBcopy (size-1)
	end;

    fun bcopyrev (srcBa,srcStart,desBa,desStart,size) =
	let
	    fun recBcopy n = if (n < size)
	    			   then (ByteArray.update(desBa,desStart + n, ByteArray.sub(srcBa, srcStart + n)); recBcopy (n+1))
				   else ()
	in
	    recBcopy 0
	end;

    fun putCString (ba,index,theString) =
    let
	val strSize = size theString;
	val _ = bcopy(stringToByteArray theString, 0, ba, index, strSize);
	val _ = ByteArray.update(ba,index+strSize,0)
    in
	index + strSize + 1
    end;
	
    fun getCString (ba,index) = 
	let
	    fun len (ba,i) = 
		if (ByteArray.sub(ba,i) = 0)
		    then 0
		else 1 + (len (ba,i+1));
	in
	    (ByteArray.extract(ba,index, len (ba,index)), 
	     index + len (ba,index) + 1)
	end;
	    
end;
