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
  File:		    "stream.sig.sml"
  Description:	    stream abstraction for sending/receiving msgs from GRAM
  CPN Tools
*)


(* $Source: /users/cpntools/repository/cpn2000/sml/com/stream.sig.sml,v $ *)

val rcsid = "$Header: /users/cpntools/repository/cpn2000/sml/com/stream.sig.sml,v 1.1.1.1 2001/10/02 11:34:23 cpn Exp $";


signature STREAM = sig
  exception streamFail of string;
  exception connFail of string;
  type instream
  type outstream
  val makeIn : OS.IO.iodesc -> instream
  val makeOut : OS.IO.iodesc -> outstream
  (* Wait on a list of input streams, only return when at least one has data
   * available; return option list of streams - a stream is returned using SOME
   * if data is available and NONE otherwise; order of input is preserved
   *)
  val select : instream list -> (instream option) list
  val destroyIn : instream -> unit
  val destroyOut : outstream -> unit
  val putBytes : outstream -> ByteArray.bytearray * int * int -> unit
  val getBytes : instream -> ByteArray.bytearray * int * int -> unit
  val flush : outstream -> unit
  val refill : instream -> unit
  val putInteger : outstream -> int -> unit
  val getInteger : instream -> int
  val putString : outstream -> string -> unit
  val getString : instream -> string
  val putBool : outstream -> bool -> unit
  val getBool : instream -> bool
  val putByteArray : outstream -> ByteArray.bytearray * int * int -> unit
  val getByteArray : instream -> ByteArray.bytearray
  val getByteArrayGivenSize : instream -> ByteArray.bytearray * int * int 
                                 -> ByteArray.bytearray
  val getIntList : instream -> int list
  val putIntList : outstream -> int list -> unit
  val getStringList : instream -> string list
  val putStringList : outstream -> string list -> unit
  val getBoolList : instream -> bool list
  val putBoolList : outstream -> bool list -> unit
  val putdbid : outstream -> ByteArray.bytearray -> unit
  val getdbid : instream -> ByteArray.bytearray

end;


