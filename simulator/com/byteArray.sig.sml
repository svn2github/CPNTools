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
  		    Standard ML of New Jersey.
  CPN Tools
*)


(* $Source: /users/cpntools/repository/cpn2000/sml/com/byteArray.sig.sml,v $ *)

val rcsid = "$Header: /users/cpntools/repository/cpn2000/sml/com/byteArray.sig.sml,v 1.2 2005/01/10 16:32:26 mw Exp $";


signature BYTEARRAYEXT = sig

    val bcopy : ByteArray.bytearray * int * 
	            ByteArray.bytearray * int * int -> unit
    val bcopyrev : ByteArray.bytearray * int * 
	            ByteArray.bytearray * int * int -> unit

    val stringToByteArray : string -> ByteArray.bytearray
    val byteArrayToString : ByteArray.bytearray -> string
	    
    val putInteger : ByteArray.bytearray * int * int -> unit
    val getInteger : ByteArray.bytearray * int -> int

    val putShortInteger : ByteArray.bytearray * int * int -> unit
    val getShortInteger : ByteArray.bytearray * int -> int

    val putCString : ByteArray.bytearray * int * string -> int
    val getCString : ByteArray.bytearray * int -> string * int


end;


(*====================DOCUMENTATION====================*)

(* 
 * getCString : (ba,index) -> (theString,newIndex)
 *
 * Inverse of putCString. Return value is the next avail slot in the 'ba'.
 *
 *)

(*
 * bcopy : (srcBa,srcIndex,desBa,desIndex,size) -> unit
 *
 * Copies 'size' bytes from 'srcBa' starting from 'srcIndex' into 'desBa' 
 * starting from 'desIndex'.
 *
 *)

(*
 * byteArrayToString
 *
 * Inverse of stringToByteArray. Works by casting types.
 *
 *)

(*
 * getInteger : (ba, index) -> theInt
 *
 * Get 'theInt' from 'ba' starting at location 'index'. The representation 
 * of the int is <b3,b2,b1,b0> where b3 is the high order byte. 
 *
 *)

(*
 * getShortInteger : (ba, index) -> theInt
 *
 * Get 'theInt' from 'ba' starting at location 'index'. The representation 
 * of the int is <b1,b0> where b1 is the high order byte. 
 *
 *)

(*
 * putCString : (ba, index, theString) -> newIndex
 *
 * Put 'theString' into 'ba' starting from 'index'. String rep. is
 * <b0 b1 b2 ... bn bn+1> where n is the length of the string and
 * bn+1 is NULL (0). Return value is the next empty slot is the 'ba'.
 *
 *)

(*
 * putInteger : (ba,index,theInt) -> unit
 * 
 * Put 'theInt' into 'ba' starting at location 'index'. The representation 
 * of the int is <b3,b2,b1,b0> where b3 is the high order byte. 
 *
 *)

(*
 * putShortInteger : (ba,index,theInt) -> unit
 * 
 * Put 'theInt' into 'ba' starting at location 'index'. The representation 
 * of the int is <b1,b0> where b1 is the high order byte. 
 *
 *)

(*
 * stringToByteArray 
 *
 * Convert a string into a byte array. Does not do any copying. Makes use
 * of the fact that strings and bytearrays have same rep. in the runtime
 * system. Simply casts the type.
 *
 *)
