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
  Module:	    Unsafe (USF)

  Description:	    Machine dependent and unsafe operations which may break
  		    with new releases of compiler or on new architectures.

  CPN Tools
*)


(* $Source: /users/cpntools/repository/cpn2000/sml/com/unsafe.sml,v $ *)

val rcsid = "$Header: /users/cpntools/repository/cpn2000/sml/com/unsafe.sml,v 1.1.1.1.10.2 2006/01/24 17:35:40 mw Exp $";



import "unsafe.sig";


functor GramUnsafe () : GRAMUNSAFE = struct

    val rcsid = "$Header: /users/cpntools/repository/cpn2000/sml/com/unsafe.sml,v 1.1.1.1.10.2 2006/01/24 17:35:40 mw Exp $";

    exception unsafeFail of string;

    val intSize = 4;

    fun stringToByteArray (str:string) : ByteArray.bytearray
      = let
          val tba = Word8Array.array(String.size str,Word8.fromInt 0);
          val _   = Byte.packString(tba,0,Substring.full(str));
        in
          tba
        end;

    fun byteArrayToString (ba:ByteArray.bytearray) : string = (Byte.unpackString(Word8ArraySlice.slice(ba,0,NONE)));
	    
    fun putdbid (ba,start,theDbId) = (
	ByteArray.update(ba,start,ByteArray.sub (theDbId, 0));
	ByteArray.update(ba,start+1,ByteArray.sub (theDbId, 1));
	ByteArray.update(ba,start+2,ByteArray.sub (theDbId, 2));
	ByteArray.update(ba,start+3,ByteArray.sub (theDbId, 3)));


    fun getdbid (ba,start) : ByteArray.bytearray =
	stringToByteArray(ByteArray.extract(ba,start,4));

	(* equaldbid not unsafe anymore !! *)
    fun equaldbid (a,b) =
	((ByteArray.sub(a, 0)) = (ByteArray.sub(b, 0))) andalso 
	((ByteArray.sub(a, 1)) = (ByteArray.sub(b, 1))) andalso 
	((ByteArray.sub(a, 2)) = (ByteArray.sub(b, 2))) andalso 
	((ByteArray.sub(a, 3)) = (ByteArray.sub(b, 3)));

 
	(* makestringdbid not unsafe anymore !! *)
    fun makestringdbid a
	= concat ["|", Int.toString(ByteArray.sub(a, 0)),
		   ".", Int.toString(ByteArray.sub(a, 1)),
		   ".", Int.toString(ByteArray.sub(a, 2)),
		   ".", Int.toString(ByteArray.sub(a, 3)),"|"];

  local
    (* According to the SML'97 documentation, the bit operators are
       independent of endianess. For instance, MSB is always the left-most bit.
     *)
          fun lshb(w32,n) = Word32.<<(w32,Word.fromInt(8*n))
          fun rshb(w32,n) = Word32.>>(w32,Word.fromInt(8*n))
          val w32or = Word32.orb
          val w32and = Word32.andb
          val w32fi = Word32.fromInt
          val w32ti = Word32.toInt
          val w32tsi = Word32.toIntX
          val wfi   = Word.fromInt
          val b255  = Word32.fromInt(255)
  in
    fun putInteger (ba,start,theInt)
      = let
          fun maskByteN (s,d) = ByteArray.update(ba,start+d,w32ti(w32and(rshb(w32fi(theInt),s),b255)))
        in
          maskByteN (0,3);
          maskByteN (1,2);
          maskByteN (2,1);
          maskByteN (3,0)
        end;

    fun getInteger (ba,start)
      = let
          fun positionByteN (s,d) = lshb(w32fi((ByteArray.sub(ba,start+s))),d)
          val w32_0 = positionByteN (0,3)
          val w32_1 = positionByteN (1,2)
          val w32_2 = positionByteN (2,1)
          val w32_3 = positionByteN (3,0)
          val w32   = w32or(w32_0,w32or(w32_1,w32or(w32_2,w32_3)))
        in
          w32tsi(w32)
        end;

    fun putShortInteger (ba,start,theInt)
      = let
          fun maskByteN (s,d) = ByteArray.update(ba,start+d,w32ti(w32and(rshb(w32fi(theInt),s),b255)))
        in
          maskByteN (0,1);
          maskByteN (1,0)
        end;

    fun getShortInteger (ba,start)
      = let
          fun positionByteN (s,d) = lshb(w32fi((ByteArray.sub(ba,start+s))),d)
          fun positionByteN (s,d) = lshb(w32fi((ByteArray.sub(ba,start+s))),d)
          val w32_0 = positionByteN (0,1)
          val w32_1 = positionByteN (1,0)
          val w32   = w32or(w32_0,w32_1)
        in
          w32tsi(w32)
        end;

  end; (* local *)
end; (* functor GramUnsafe() *)
    
