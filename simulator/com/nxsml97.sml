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
(* $Source: /users/cpntools/repository/cpn2000/sml/com/nxsml97.sml,v $
 *
 * Compiler dependent functions etc.
 *
 *)

val rcsid = "$Header: /users/cpntools/repository/cpn2000/sml/com/nxsml97.sml,v 1.1.1.1.10.1 2005/02/17 17:38:59 mw Exp $";

structure CompilerDep =
struct

  val exportML = SMLofNJ.exportML;

  fun exportFn (s,f:string * string list -> unit) = SMLofNJ.exportFn(s,fn (str,slst) => (f(str,slst); OS.Process.success));

  val use_stream = Compiler.Interact.useStream;

  val import = importE;

end;

val import = CompilerDep.import;

signature BYTEARRAY =
sig
  type bytearray
  val sub     : bytearray * int -> int
  val array   : int * int -> bytearray
  val update  : bytearray * int * int -> unit
  val extract : bytearray * int * int -> string
  val length  : bytearray -> int
end;

structure ByteArray:BYTEARRAY =
struct

  structure BA = Word8Array
  structure E  = Word8

  type bytearray = BA.array

  val sub = E.toInt o BA.sub
  val array = fn (n,c) => BA.array (n,E.fromInt c)
  val update = fn (a,i,v) => BA.update (a,i, E.fromInt v)
  fun extract (ba,s,n)
    = let
        fun tab i = BA.sub(ba,i+s)
      in
	Byte.unpackString(Word8ArraySlice.slice (BA.tabulate(n,tab), 0, NONE))
      end
  val length = BA.length;
end;

(* See discussion section in SML'98 manual under the WORD signature *)
structure Bits =
struct

  structure BI = Word32
  structure W  = Word

  fun andb   (b1,b2) = BI.toIntX(BI.andb(BI.fromInt b1,BI.fromInt b2))
  fun orb    (b1,b2) = BI.toIntX(BI.orb (BI.fromInt b1,BI.fromInt b2))
  fun xorb   (b1,b2) = BI.toIntX(BI.xorb(BI.fromInt b1,BI.fromInt b2))
  fun notb   (b1)    = BI.toIntX(BI.notb(BI.fromInt b1))
  fun lshift (b1,b2) = BI.toIntX(BI.<<  (BI.fromInt b1,W.fromInt b2))
  fun rshift (b1,b2) = BI.toIntX(BI.>>  (BI.fromInt b1,W.fromInt b2))

end;
