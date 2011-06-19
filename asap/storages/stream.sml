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
 *  File:
 *     state-stream.sml
 *
 *  Created:
 *     Jan. 10, 2008
 *
 *  Description:
 *     Provides stream functors.
 *  
 *)


functor Stream(
structure Encoder: REVERSIBLE_MAPPING
where type dest = BinIO.vector): STREAM = struct

structure Encoder = Encoder

type item = Encoder.src

fun openIn file =
    BinIO.openIn file

fun openAppend file = let
    val result = BinIO.openAppend file
in
    BinIO.StreamIO.setBufferMode (BinIO.getOutstream result, IO.BLOCK_BUF);
    result
end

fun readInt16 stream =
    Word8.toInt (valOf (BinIO.input1 stream)) * 256 +
    Word8.toInt (valOf (BinIO.input1 stream))

fun writeInt16 (stream, i) = (
    BinIO.output1 (stream, Word8.fromInt (i div 256));
    BinIO.output1 (stream, Word8.fromInt (i mod 256)))
	
fun readOne stream =
    if BinIO.endOfStream stream
    then NONE
    else let val width = readInt16 stream
	 in
	     SOME (Encoder.unmap (BinIO.inputN (stream, width)))
	 end

fun writeOne (stream, item) = let
    val v = Encoder.map item
in
    writeInt16 (stream, Word8Vector.length v);
    BinIO.output  (stream, v)
end
				     
fun read (stream, SOME 0) = []
  | read (stream, SOME n) = (
    case readOne stream
     of NONE => []
      | SOME item => item :: read (stream, SOME (n - 1)))
  | read (stream, NONE) =
    case readOne stream
     of NONE => []
      | SOME item => item :: read (stream, NONE)

fun write (stream, items) = List.app (fn v => writeOne (stream, v)) items

fun app f (stream, SOME 0) = ()
  | app f (stream, SOME n) = (
    case readOne stream
     of NONE => ()
      | SOME item => (f item; app f (stream, SOME (n - 1))))
  | app f (stream, NONE) = (
    case readOne stream
     of NONE => ()
      | SOME item => (f item; app f (stream, NONE)))

fun fold f value (stream, SOME 0) = value
  | fold f value (stream, SOME n) = (
    case readOne stream
     of NONE => value
      | SOME item => fold f (f (item, value)) (stream, SOME (n - 1)))
  | fold f value (stream, NONE) = (
    case readOne stream
     of NONE => value
      | SOME item => fold f (f (item, value)) (stream, NONE))

fun app' f (file, limit) =
    case SOME (openIn file) handle _ => NONE of
	NONE => ()
      | SOME stream => (app f (stream, limit); BinIO.closeIn stream)

fun fold' f value (file, limit) = 
    case SOME (openIn file) handle _ => NONE of
	NONE => value
      | SOME stream =>
	fold f value (stream, limit) before
	BinIO.closeIn stream

end


functor StreamPair(
structure Stream1: STREAM
structure Stream2: STREAM): STREAM_PAIR = struct

type item1 = Stream1.item

type item2 = Stream2.item

fun fold f value (stream1, stream2, SOME 0) = value
  | fold f value (stream1, stream2, SOME n) = (
    case (Stream1.readOne stream1, Stream2.readOne stream2)
     of (SOME item1, SOME item2) =>
	fold f (f ((item1, item2), value)) (stream1, stream2, SOME (n - 1))
      | _ => value)
  | fold f value (stream1, stream2, NONE) = (
    case (Stream1.readOne stream1, Stream2.readOne stream2)
     of (SOME item1, SOME item2) =>
	fold f (f ((item1, item2), value)) (stream1, stream2, NONE)
      | _ => value)

fun fold' f value (file1, file2, limit) = 
    case SOME (Stream1.openIn file1, Stream2.openIn file2) handle _ => NONE of
	NONE => value
      | SOME (stream1, stream2) =>
	fold f value (stream1, stream2, limit) before (
	BinIO.closeIn stream1;
	BinIO.closeIn stream2) 

end
