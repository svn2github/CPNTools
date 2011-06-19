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
  File:		    "glue.sml"

  Description:	    provides glue for defining new GRAM functions

  CPN Tools
*)


(* $Source: /users/cpntools/repository/cpn2000/sml/com/glue.sml,v $ *)

val rcsid = "$Header: /users/cpntools/repository/cpn2000/sml/com/glue.sml,v 1.1.1.1 2001/10/02 11:34:22 cpn Exp $";


import "glue.sig";
import "cmdProcess.sig";


functor Glue (structure Cmd : CMDPROCESS) : GLUE = struct

	
    val rcsid = "$Header: /users/cpntools/repository/cpn2000/sml/com/glue.sml,v 1.1.1.1 2001/10/02 11:34:22 cpn Exp $";
	
    structure Cmd = Cmd;
    structure Str = Cmd.Str;
    
    datatype gDataValue = 
	gdvInt of int 
      | gdvShort of int 
      | gdvBool of bool 
      | gdvString of string 
      | gdvCpnString of string 
      | gdvByteArray of ByteArray.bytearray * int * int
      | gdvVoid
      | gdvDbId of ByteArray.bytearray

    datatype gDataSpec =
	gdsInt
      | gdsShort
      | gdsBool
      | gdsString
      | gdsCpnString
      | gdsByteArrayAlloc of ByteArray.bytearray * int * int
      | gdsVoid
      | gdsDbId


    datatype gParam =
	IN of gDataValue
      | OUT of gDataSpec
      | INOUT of gDataValue


    exception gfail of string;
    exception invokeEx of unit;

    (* 
     * Get the "unpackaged" basic value from the "packaged" value
     *)

    fun getInt (gdvInt x) = x
      | getInt (gdvShort x) = x
      | getInt _ = raise gfail "getInt"

    fun getString (gdvString s) = s
      | getString (gdvCpnString s) = s
      | getString _ = raise gfail "getString"

    fun getBool (gdvBool b) = b
      | getBool _ = raise gfail "getBool";

    fun getdbid (gdvDbId x) = x
      | getdbid _ = raise gfail "getDbId"

    (*
     * putValue + putSpec
     *
     * For each parameter:
     * 1. convert the param type into an integer encoding (see list)
     * 2. convert the param value into an encoding which
     *    depends on the param type
     * 3. transmit the parameter as follows:
     *		<param type encoding> <param value encoding>
     *)

    (* 
     * Parameter Type Encodings
     *
     * int		1
     * string		2
     * bool		3
     * void		4
     * short		5
     * cpnString	6
     * bytearray	8
     * 
     *)

    fun putValue outs (gdvInt x) =
	(Str.putInteger outs 1;
	 Str.putInteger outs x)
      | putValue outs (gdvString x) =
	(Str.putInteger outs 2;
	 Str.putString outs x)
      | putValue outs (gdvBool x) =
	(Str.putInteger outs 3;
	 Str.putBool outs x)
      | putValue outs (gdvVoid) = Str.putInteger outs 4
      | putValue outs (gdvShort x) =
	(Str.putInteger outs 5;
	 Str.putInteger outs x)
      | putValue outs (gdvCpnString x) =
	(Str.putInteger outs 6;
	 Str.putString outs x)
      | putValue outs (gdvByteArray (ba,start,size)) =
	(Str.putInteger outs 8;
	 Str.putByteArray outs (ba,start,size))
      | putValue outs (gdvDbId x) =
	(Str.putInteger outs 1;
	 Str.putdbid outs x)
(*    | putValue _ _ = raise gfail "putValue";  *)
	 
    fun putSpec outs gdsInt =
	(Str.putInteger outs 1)
      | putSpec outs gdsString =
	(Str.putInteger outs 2)
      | putSpec outs gdsBool =
	(Str.putInteger outs 3)
      | putSpec outs gdsVoid = Str.putInteger outs 4
      | putSpec outs gdsShort =
	(Str.putInteger outs 5)
      | putSpec outs gdsCpnString =
	(Str.putInteger outs 6)
      | putSpec outs (gdsByteArrayAlloc (ba,start,size)) =
	(Str.putInteger outs 8;
	 Str.putInteger outs size)
      | putSpec outs gdsDbId =
	(Str.putInteger outs 1)

(*    | putSpec _ _ = raise gfail "putSpec"; *) (* redundant match *)

    (*
     * putParam
     *
     * For each parameter:
     * 1. send an encoding of its "kind" to the Gram.
     * 2. send an encoding of its "type" and "value" to the Gram (see 
     *    putParamTypeValue
     *
     *)

    (* 
     * Parameter Kind Encodings
     * 
     * IN 		1
     * OUT		2
     * INOUT		3
     *)

    fun putParam outs (IN(data)) = 
	(Str.putInteger outs 1;
	 putValue outs data)
      | putParam outs (OUT(data)) =
	(Str.putInteger outs 2;
	 putSpec outs data)
      | putParam outs (INOUT(data)) =
	(Str.putInteger outs 3;
	 putValue outs data);


    fun putParamList outs paramL = 
	app (fn param => putParam outs param) paramL;
	

    (* 
     * putRvalSpec
     *
     * Send the return value s specification to the Gram. This is used
     * to inform the Gram of what return value it should expect from 
     * the function call.
     *)

    val putRvalSpec = putSpec;


    fun dataValueToSpec (gdvInt _) = gdsInt
      | dataValueToSpec (gdvShort _) = gdsShort
      | dataValueToSpec (gdvBool _) = gdsBool
      | dataValueToSpec (gdvString _) = gdsString
      | dataValueToSpec (gdvCpnString _) = gdsCpnString
      | dataValueToSpec (gdvByteArray x) = gdsByteArrayAlloc x
      | dataValueToSpec gdvVoid = gdsVoid
      | dataValueToSpec (gdvDbId _) = gdsDbId


    (*
     * paramListToOutParamSpec
     *
     * For each given parameter specification:
     * 1. Determine if a value is expected back (INOUT, OUT)
     * 2. Extract the "type" information for such parameters
     *    This info is used later to interpret the result received
     *    from the Gram.
     *)

    fun paramListToOutParamSpec nil = nil 
      | paramListToOutParamSpec ((OUT x) :: tl) =
	x :: (paramListToOutParamSpec tl)
      | paramListToOutParamSpec ((INOUT x) :: tl) =
	(dataValueToSpec x) :: (paramListToOutParamSpec tl)
      | paramListToOutParamSpec (_::tl) = paramListToOutParamSpec tl


    (*
     * dataSpecToValue
     *
     * Given a return value specification (which consists of "type"
     * information), get values from the Gram according to the 
     * specification. 
     *)

    fun dataSpecToValue ins gdsInt = gdvInt (Str.getInteger ins)
      | dataSpecToValue ins gdsString = gdvString (Str.getString ins)
      | dataSpecToValue ins gdsCpnString = gdvCpnString (Str.getString ins)
      | dataSpecToValue ins gdsBool = gdvBool (Str.getBool ins)
      | dataSpecToValue ins gdsShort = gdvShort (Str.getInteger ins)
      | dataSpecToValue ins (gdsVoid) = gdvVoid
      | dataSpecToValue ins (gdsByteArrayAlloc (ba,start,size)) = 
	gdvByteArray (Str.getByteArrayGivenSize ins (ba,start,size), 
		      start, size)
      | dataSpecToValue ins gdsDbId = gdvDbId (Str.getdbid ins)
(*    | dataSpecToValue _ _ = raise gfail "dataSpecToValue" *) (* redundant match *)


    fun getRvalList ins rvalSpecL = 
	map (fn spec => dataSpecToValue ins spec) rvalSpecL;



    (*
     * invoke
     * 
     * Invoke a function in the Gram. 
     * 1. Sends a message to the GRAM with the following format:
     *		<opcode> <n> <param 1> <param 2> ... <param n>
     *    where <opcode> is the integer opcode of the function to be called
     *          <param 1> is the return value specification (see putRvalSpec)
     *          <param 2> -- <param n> are the call params for the fn
     *          (see putParamList)
     * 2. Expects a message from the GRAM with the following format:
     *		<rval 1> <rval 2> ... <rval m>
     *    where m <= n
     *          these are the result of the function call
     *          <rval 1> is the return value of the function call
     *		<rval 2> ... <rval m> are results obtained via parameters
     *            and correspond to the <param i> s that were OUT or INOUT.
     * 3. While waiting for the result, this function calls a "toploop"
     *    function which can service other types of requests. 
     *)

    (*
     * A Note for Users of invoke
     *
     * 'invoke' along with the datatypes exported by this module
     * provide a high-level mechanism for calling Gram Functions from ML.
     *
     *)

    fun invoke (oper,paramL,rvalSpec) = 
	let
	    val (outs,ins) = Cmd.getGfcStreams (!Cmd.theGram);
	in
	    Str.putInteger outs oper;		(* sub opcode *)
	    Str.putInteger outs (length(paramL)+1); (* num args *)
	    putRvalSpec outs rvalSpec;
	    putParamList outs paramL;
	    Str.flush outs;
	    Cmd.waitWoutEval (!Cmd.theGram, Cmd.GfcResult);
	    getRvalList ins (rvalSpec::(paramListToOutParamSpec paramL))
	end;



    (* VARIANTS of invoke; all use invoke *)

    (* actually invokeForUnitWithExcep *)
    fun invokeWithExcep (oper,param) =
	if getBool(List.nth(invoke (oper,param,gdsBool),0))
	    then () else raise invokeEx ()

    fun invokeForInt (oper,param) = 
	    getInt(List.nth(invoke (oper,param,gdsInt),0));


    fun invokeForIntWithExcep (oper,param) = let
	val [success,v] = invoke (oper, param@[OUT gdsInt],
					  gdsBool);
    in
	if not (getBool success) then raise invokeEx () else ();
        getInt v
    end;


    fun invokeForStringWithExcep (oper,param) = let
	val [success,s] = invoke (oper,param@[OUT gdsString],
					  gdsBool);
    in
	if not (getBool success) then raise invokeEx () else ();
        getString s
    end;

    fun invokeForDbId (oper,param) = 
	    getdbid(List.nth(invoke (oper,param,gdsDbId),0));

    fun invokeForDbIdWithExcep (oper,param) = let
	val [success,v] = invoke (oper, param@[OUT gdsDbId],
					  gdsBool);
    in
	if not (getBool success) then raise invokeEx () else ();
        getdbid v
    end;

end; (* functor Glue *)

