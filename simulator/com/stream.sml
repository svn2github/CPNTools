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
  Module:	    Stream (STR)

  Description:	    An abstract mechanism to read/write structured or
  		    unstructured data from/to another process.
		    A standard UNIX file descriptor can be encapsulated into
		    a stream object using 'make'.

  CPN Tools
*)


(* $Source: /users/cpntools/repository/cpn2000/sml/com/stream.sml,v $*)

val rcsid = "$Header: /users/cpntools/repository/cpn2000/sml/com/stream.sml,v 1.4 2005/01/11 19:35:10 mw Exp $";



import "communicate.sig";
import "stream.sig";
import "unsafe.sig";
import "byteArray.sig";



functor Stream (structure Comm : COMM
		structure ByteArrayExt : BYTEARRAYEXT
		structure Unsafe : GRAMUNSAFE) : STREAM = struct
    
    val rcsid = "$Header: /users/cpntools/repository/cpn2000/sml/com/stream.sml,v 1.4 2005/01/11 19:35:10 mw Exp $";
	
    val streamBufSize = 8192;
    val intSize = Unsafe.intSize;

    exception streamFail of string;
    exception connFail of string;

    (*
     * Instream Invariants
     *
     * 1. 'rd' always points to next available byte unless 'buf' is empty
     *
     * 2. 'buf' empty <=> 'rd' = 'stream.lastrd'
     *
     *)

    (*
     * Outstream Invariants
     *
     * 1. 'wt' always points to the next available byte unless 'buf' is full
     *
     * 2. 'buf' full <=> 'wt' = 'streamBufSize'
     *
     * 3. 'streamBufSize' - 'wt' = space available in buffer
     *)

    datatype instream = IstFd of {iodesc:OS.IO.iodesc, buf:ByteArray.bytearray,
				  rd:int ref, lastrd:int ref};
    datatype outstream = OstFd of {iodesc:OS.IO.iodesc, buf:ByteArray.bytearray, 
				   wt:int ref};
	
    fun makeIn iodesc = IstFd {buf=ByteArray.array(streamBufSize,0),
			       iodesc=iodesc, rd= ref 0, lastrd= ref 0};

    fun makeOut iodesc = OstFd {buf=ByteArray.array(streamBufSize,0),
			        iodesc=iodesc, wt= ref 0};

    fun destroyIn ins = ();
	
    fun destroyOut outs = ();

    fun flush (stream as OstFd {iodesc,wt,buf}) = 
	let
	    val sent = Comm.send(iodesc,buf,!wt,0)
		handle Comm.exComm (s) => raise connFail s;
	in
	    (wt := !wt-sent;
	     if (!wt <> 0) then flush stream else ())
	end;

    fun refill (stream as IstFd {iodesc,rd,lastrd,buf}) =
	let val size = !lastrd - !rd
	 in
	 ByteArrayExt.bcopyrev(buf, !rd, buf, 0, size);
	 rd := 0;
	 lastrd := size;
	 lastrd := Comm.recv(iodesc,buf,streamBufSize - size,size) + size
	 end
	           handle Comm.exComm (s) => raise connFail s

    fun putBytes (stream as OstFd {buf,wt,...}) (data,from,size) =
	let
	    (* determine space avail *)
	    val avail = streamBufSize - !wt;
	in
	    if avail >= size then	(* space avail *)
		(ByteArrayExt.bcopy (data,from,buf,!wt,size);
		 wt := !wt + size)
	    else			(* not enough space *)
		(* copy what we can *)		
		(ByteArrayExt.bcopy (data,from,buf,!wt,avail);
		 wt := streamBufSize;
		 flush stream;
		 putBytes stream (data,from+avail,size-avail))
	end;

    fun getBytes (stream as IstFd {buf,iodesc,rd,lastrd}) (data,to,size) =
	let
	    val avail = !lastrd - !rd;
	in
	    if avail >= size then	(* enough avail  *)
		(ByteArrayExt.bcopy(buf,!rd,data,to,size);
		 rd := !rd + size)
	    else
		(ByteArrayExt.bcopy(buf,!rd,data,to,avail);
		 rd := !lastrd;
		 refill stream;
		 getBytes stream (data,to+avail,size-avail))
	end;



    (*
      Name:		putInteger

      Description:  Write the given integer into the stream. Regardless of the
		    machine representation of integers, the integer will be
		    written as:

			    b3 b2 b1 b0

		    where b3 is high and b0 is low.

		    Calling this fn doesn't guarantee a transmission. Call
		    'flush' to force transmission.

      History:	15-Oct-90 JKM created

    *)		

    fun putInteger (stream as OstFd {buf,wt,...}) theInt =
	(if ((streamBufSize - !wt) < intSize) then
	     flush(stream)
	 else ();
	 Unsafe.putInteger(buf,!wt,theInt);
	 wt := !wt + intSize);
	     
    fun getInteger (stream as IstFd {buf,rd,lastrd,...}) =
	(while ((!lastrd - !rd) < intSize) do refill stream;
	 let
	     val theInt = Unsafe.getInteger(buf,!rd);
	 in
	     rd := !rd + intSize;
	     theInt
	 end)

    fun putBool stream theBool = 
	if (theBool) then putInteger stream 1
	else putInteger stream 0;

    fun getBool stream =
	if (getInteger stream = 1) then true else false;
	    
    fun putString (stream as OstFd {wt,...}) theString =
	let
	    val len = size theString;
	    val spill = len mod intSize;
	    val padd = if spill = 0 then 0 else (intSize - spill);
	in
	    (putInteger stream len;
	     putBytes stream (Unsafe.stringToByteArray theString, 0, len);
	     wt := !wt + padd)
	end;
	
    fun getString (stream as IstFd {rd,lastrd,...}) =
	let
	    val len = getInteger stream;
	    val ba = ByteArray.array (len,0);
	    val _ = getBytes stream (ba,0,len);
	    val theString = Unsafe.byteArrayToString ba;
	    val spill = len mod intSize;
	    val padd = if spill = 0 then 0 else (intSize - spill);
	    val _ = while ((!lastrd - !rd) < padd) do refill stream;
	in
	    (rd := !rd + padd;
	     theString)
	end;
	
    fun putByteArray (stream as OstFd {wt,...}) (ba,from,size) =
	let
	    val spill = size mod intSize;
	    val padd = if spill = 0 then 0 else (intSize - spill);
	in
	    putInteger stream size;
	    putBytes stream (ba,from,size);
	    wt := !wt + padd
	end;

    fun getByteArray (stream as IstFd {rd,...}) =
	let
	    val size = getInteger stream;
	    val ba = ByteArray.array (size,0);
	    val spill = size mod intSize;
	    val padd = if spill = 0 then 0 else (intSize - spill);
	in
	    getBytes stream (ba,0,size);
	    rd := !rd + padd;
	    ba
	end;	    

    fun getByteArrayGivenSize (stream as IstFd {rd,...}) 
	                      (ba,from,expectedSize) =
	let
	    val receivedSize = getInteger stream;
	    val spill = expectedSize mod intSize;
	    val padd = if spill = 0 then 0 else (intSize - spill);
	in
	    if (receivedSize <> expectedSize) 
		then raise streamFail "getByteArrayGivenSize"
	    else ();
	    getBytes stream (ba,from,expectedSize);
	    rd := !rd + padd;
	    ba
	end;


    (*
     * genericPutList
     *
     * Given a function to put elements of the list 'putFn', 
     * a 'stream' and a 'list', output the list onto the given
     * stream. The format:
     *
     *    <n:int> <elem1:listtype> ... <elemn:listtype>
     *
     *)

    fun genericPutList putFn stream list = 
	(putInteger stream (length list);
	 app (fn elem => putFn stream elem) list)
	
    val putIntList = genericPutList putInteger;
    val putStringList = genericPutList putString;
    val putBoolList = genericPutList putBool;


    (* 
     * genericGetList
     * 
     * Given a function to get elements of the list 'getFn',
     * and a 'stream', extracts from the stream a list.
     * Format expected in the stream:
     *
     *    <n:int> <elem1:listtype> ... <elemn:listtype>
     *
     *)

    fun genericGetList getFn stream =
	let
	    val length = getInteger stream;
		
	    fun getList 0 = []
	      | getList n = (getFn stream) :: (getList (n-1));
		
	in
	    getList length
	end;

    val getIntList = genericGetList getInteger;
    val getStringList = genericGetList getString;
    val getBoolList = genericGetList getBool;


    fun putdbid (stream as OstFd {buf,wt,...}) theDbId =
	(if ((streamBufSize - !wt) < 4 (*Bytes *)) then
	     flush(stream)
	 else ();
	 Unsafe.putdbid(buf,!wt,theDbId);
	 wt := !wt + 4 (*Bytes *));
	     
    fun getdbid (stream as IstFd {buf,rd,lastrd,...}) =
	(while ((!lastrd - !rd) < 4 (*Bytes *)) do refill stream;
	 let
	     val theDbId = Unsafe.getdbid(buf,!rd);
	 in
	     rd := !rd + 4 (*Bytes *);
	     theDbId
	 end)
		 
end;
