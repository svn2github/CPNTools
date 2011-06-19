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
 *     dve-serializer-compiler.sml
 *
 *  Created:
 *     Nov. 17, 2008
 *
 *  Generate:
 *     structure DveStateSerializer: SERIALIZER = struct
 *        type src = DveModel.state
 *        ...
 *     end
 *     structure DveEventSerializer: SERIALIZER = struct
 *        type src = DveModel.event
 *        ...
 *     end
 *     structure DveEventListSerializer: SERIALIZER = struct
 *        type src = DveModel.event list
 *        ...
 *     end
 *)


structure DveSerializerCompiler: sig

    val genState: System.system -> string list

    val genEvent: System.system -> string list

end = struct

open DveCompilerUtils

datatype word_length =
	 WORD8
       | WORD16
       | WORD32    

fun getWordLength l =
    case l of 8  => WORD8
	    | 16 => WORD16
	    | _  => WORD32

fun varWidth v =
    case Var.getTyp v of
	Typ.BASIC_TYPE Typ.BYTE   => 8
      | Typ.BASIC_TYPE Typ.INT    => 32
      | Typ.ARRAY_TYPE (bt, size) =>
	size * (case bt of Typ.BYTE => 8 | Typ.INT  => 32)

fun compWidth s comp =
    case comp of
	GLOBAL_VAR v     => varWidth v
      | LOCAL_VAR (_, v) => varWidth v
      | PROCESS_STATE p  => let
	    val procDef = Process.getProcess (System.getProcs s, p)
	    val procStates = Process.getStates procDef
	in
	    if List.length procStates <= 256
	    then 8
	    else if List.length procStates <= 65536
	    then 16
	    else 32
	end

fun compileEncodeState (s: System.system) = let
    val tmpVarNum = ref 0
    val comps = buildStateComps s
    val map = buildMapping comps

    fun cat ("", "", _) = ""
      | cat ("", str, _) = str
      | cat (str, "", _) = str
      | cat (str1, str2, sep) = str1 ^ sep ^ str2
				
    fun intToCharList l i =
	case l
	 of WORD8  => ("", [ "Word8.fromInt (" ^ i ^ ")" ])
	  | WORD16 => let
		val w = "w" ^ Int.toString (!tmpVarNum)
		val _ = tmpVarNum := (!tmpVarNum) + 1
	    in
		("   val " ^ w ^ " = LargeWord.fromInt (" ^ i ^ ")",
		 [ "Word8.fromLarge (Word.>> (" ^ w ^ ", 0wx8))",
		   "Word8.fromLarge (" ^ w ^ ")" ])
	    end
	  | WORD32 => let
		val w = "w" ^ Int.toString (!tmpVarNum)
		val _ = tmpVarNum := (!tmpVarNum) + 1
	    in
		("   val " ^ w ^ " = LargeWord.fromInt (" ^ i ^ ")",
		 [ "Word8.fromLarge (LargeWord.>> (" ^ w ^ ", 0wx18))",
		   "Word8.fromLarge (LargeWord.>> (" ^ w ^ ", 0wx10))",
		   "Word8.fromLarge (LargeWord.>> (" ^ w ^ ", 0wx8))",
		   "Word8.fromLarge (" ^ w ^ ")" ])
	    end

    fun encodeVar c v =
	case Var.getTyp v
	 of Typ.BASIC_TYPE Typ.BYTE   => intToCharList WORD8  (baseToInt c)
	  | Typ.BASIC_TYPE Typ.INT    => intToCharList WORD32 (baseToInt c)
	  | Typ.ARRAY_TYPE (bt, size) => let
		val f = case bt of Typ.BYTE => intToCharList WORD8
				 | Typ.INT  => intToCharList WORD32
		fun mapArray i =
		    if size = i
		    then ("", [])
		    else let val (d, e) = mapArray (i + 1)
			     val (di, ei) = f (baseToInt
						   ("(" ^ c ^ "_ITEM_" ^
						    (Int.toString i) ^ ")"))
			 in
			     (cat (di, d, "\n"), ei @ e)
			 end
	    in
		mapArray 0
	    end
	
    fun encodeComp comp =
	if isCompConst comp
	then NONE
	else SOME (
	     case comp of
		 GLOBAL_VAR v     => encodeVar (getImage (map, comp)) v
	       | LOCAL_VAR (_, v) => encodeVar (getImage (map, comp)) v
	       | PROCESS_STATE p  => let
		     val f = intToCharList (getWordLength (compWidth s comp))
		 in
		     f ((getLocalStateToInt p) ^ " (" ^
			(getImage (map, comp)) ^ ")")
		 end)

    val (defs, exprs) = ListPair.unzip (List.mapPartial encodeComp comps)

    val exprs = List.concat exprs

    val exprs = ListPair.zip (List.tabulate (List.length exprs, fn i => i),
			      exprs)

    fun id str = if str <> ""
		 then SOME str
		 else NONE

    fun fmtExpr (_, "") = NONE
      | fmtExpr (i, e) = SOME (Int.toString i ^ " => " ^ e)
		 
    val comps = #2 (genAllComps (comps, NONE))
in
    String.concat [
    "fun encodeState (", comps, ": state) = let\n",
    Utils.fmt {init  = "",
	       sep   = "\n",
	       final = "\n",
	       fmt   = id} defs,
    "in\n",
    "   Word8Vector.tabulate (", Int.toString (List.length exprs),
    Utils.fmt {init  = ",\nfn ",
	       sep   = "\n | ",
	       final = "\n | _ => raise Impossible \"\")\n\n",
	       fmt   = fmtExpr} exprs,
    "end\n"
    ]
end

fun compileDecodeState (s: System.system) = let

    val comps = buildStateComps s
    val shift = ref 0
    val map   = ref []

    fun charListToInt l =
	case l of
	    WORD8  =>
	     String.concat [
	    "\tWord8.toInt (Word8Vector.sub (v, ",
	    Int.toString (!shift), "))" ]
	   | WORD16 =>
	     String.concat [
	     "\tWord32.toInt (\n\t",
	     "Word32.<< (Word32.fromLarge (Word8.toLarge ",
	     "(Word8Vector.sub (v, ",
	     Int.toString ((!shift)), "))), 0wx8) +\n\t",
	     "Word32.fromLarge (Word8.toLarge (Word8Vector.sub (v, ",
	     Int.toString ((!shift) + 1), "))))" ]
	   | WORD32 =>
	     String.concat [
	     "\tWord32.toIntX (\n\t",
	     "Word32.<< (Word32.fromLarge (Word8.toLarge ",
	     "(Word8Vector.sub (v, ",
	     Int.toString (!shift), "))), 0wx18) +\n\t",
	     "Word32.<< (Word32.fromLarge (Word8.toLarge ",
	     "(Word8Vector.sub (v, ",
	     Int.toString (!shift + 1), "))), 0wx10) +\n\t",
	     "Word32.<< (Word32.fromLarge (Word8.toLarge ",
	     "(Word8Vector.sub (v, ",
	     Int.toString ((!shift) + 2), "))), 0wx8) +\n\t",
	     "Word32.fromLarge (Word8.toLarge (Word8Vector.sub (v, ",
	     Int.toString ((!shift) + 3), "))))" ]
	
	    
    fun decodeVar comp = let
	val v = valOf (getCompVar comp)
	val (result, add) =
	    case Var.getTyp v of
		Typ.BASIC_TYPE Typ.BYTE   => (charListToInt WORD8,  1)
	      | Typ.BASIC_TYPE Typ.INT    => (charListToInt WORD32, 4)
	      | Typ.ARRAY_TYPE (bt, size) => let
		    val l = case bt of Typ.BYTE => (WORD8,  1)
				     | Typ.INT  => (WORD32, 4)
		    fun decodeArray i =
			if size = i
			then ""
			else let val resI = charListToInt (#1 l)
				 val _ = shift := (#2 l) + (!shift)
				 val resArray = decodeArray (i + 1)
			     in
				 resI ^ (if resArray = ""
					 then ""
					 else ",\n" ^ resArray)
			     end
		in
		    ("(" ^ (decodeArray 0) ^ ")", 0)
		end
    in
	shift := add + (!shift);
	result
    end
			 
    fun decodeState comp p = let
	val result =
	    "\t" ^ (getIntToLocalState p) ^
	    " (\n\t" ^ charListToInt (getWordLength (compWidth s comp)) ^ ")"
    in
	shift := (!shift) + (compWidth s comp) div 8;
	result
    end
	
    fun decodeComp comp = 
	if isCompConst comp
	then ()
	else let val decode = case comp of
				  GLOBAL_VAR _    => decodeVar comp
				| LOCAL_VAR _     => decodeVar comp
				| PROCESS_STATE p => decodeState comp p
	     in
		 map := updateMapping (!map, comp, decode)
	     end

    val _ = List.app decodeComp comps

in
    String.concat [
    "fun decodeState (v: Word8Vector.vector) =\n",
    "   ", mappingToState (!map), "\n\n" ]
end

fun compileEncodeEvent (s: System.system) = let

    val events = buildEvents s
    val l = if List.length events < 255
	    then WORD8
	    else if List.length events < 65535
	    then WORD16
	    else WORD32
    val first = ref true
    val c = case l of WORD8  => 1
		    | WORD16 => 2
		    | WORD32 => 4
    val id = ref 0

    fun encodeEvent name = let
	val value =
	    ListFormat.listToString
		(fn i => "0wx" ^ (Word8.toString (Word8.fromInt i)))
		(case l of WORD8  => [ !id ]
			 | WORD16 => [ !id div 256,
				       !id mod 256 ]
			 | WORD32 => [ (!id div 16777216) mod 256,
				       (!id div 65536) mod 256,
				       (!id div 256) mod 256,
				       (!id mod 256) ])
    in
	String.concat [
	if !first then (first := false; "of ") else " | ",
	name, " => ", value, "\n" ]
	before id := !id + 1
    end

    fun decodeEvent name =
	String.concat [
	if !first then (first := false; "of ") else " | ",
	Int.toString (!id), " => ", name, "\n" ]
	before id := !id + 1

    fun vectorToInt v =
	case l of
	    WORD8  =>
	    "Word8.toInt (Word8Vector.sub (" ^ v ^ ", 0))"
	  | WORD16 =>
	    "Word8.toInt (Word8Vector.sub (" ^ v ^ ", 0)) * 256 +" ^
	    "Word8.toInt (Word8Vector.sub (" ^ v ^ ", 1))"
	  | WORD32 =>
	    "Word8.toInt (Word8Vector.sub (" ^ v ^ ", 0)) * 16777216 +" ^
	    "Word8.toInt (Word8Vector.sub (" ^ v ^ ", 1)) * 65536 +" ^
	    "Word8.toInt (Word8Vector.sub (" ^ v ^ ", 2)) * 256 +" ^
	    "Word8.toInt (Word8Vector.sub (" ^ v ^ ", 3))"

in
    String.concat [
     "fun encodeEvent (e: event) =\n",
     "   Word8Vector.fromList \n",
     "      (case e\n",
     (first := true;
      id := 0;
      String.concat (List.map (fn e => encodeEvent (getEventName e)) events)),
     encodeEvent "DUMMY_EVENT",
     "      )\n",
     "\n",
     "fun decodeEvent (v: Word8Vector.vector) =\n",
     "   case ", vectorToInt "v", "\n",
     (first := true;
      id := 0;
      String.concat (List.map (fn e => decodeEvent (getEventName e)) events)),
     decodeEvent "DUMMY_EVENT",
     " | _ => raise Impossible(\"invalid event id\")\n",
     "\n",
     "fun encodeEventList (e: event list) =\n",
     "   Word8Vector.concat (List.map encodeEvent e)\n",
     "fun decodeEventList (v: Word8Vector.vector) =\n",
     "   if Word8Vector.length v = 0\n",
     "   then []\n",
     "   else let\n",
     "      val evt  = Word8Vector.tabulate ",
     "(", Int.toString c, ", (fn i => Word8Vector.sub (v, i)))\n",
     "      val evts = Word8Vector.tabulate ",
     "(Word8Vector.length v - ", Int.toString c,
     ", (fn i => Word8Vector.sub (v, i + ", Int.toString c, ")))\n",
     "   in (decodeEvent evt) :: (decodeEventList evts) end\n",
     "\n"
    ]
end

fun genState s =
    [
     "structure DveStateSerializer: SERIALIZER = struct\n",
     "open DveDefinitions\n",
     "type src  = DveDefinitions.state\n",
     "type dest = Word8Vector.vector\n",
     compileEncodeState s,
     compileDecodeState s,
     "val map   = encodeState\n",
     "val unmap = decodeState\n",
     "end\n"
    ]

fun genEvent s =
    [
     "local\n",
     "open DveDefinitions\n",
     compileEncodeEvent s,
     "in\n",
     "structure DveEventSerializer: SERIALIZER = struct\n",
     "type src  = DveDefinitions.event\n",
     "type dest = Word8Vector.vector\n",
     "val map   = encodeEvent\n",
     "val unmap = decodeEvent\n",
     "end\n",
     "structure DveEventListSerializer: SERIALIZER = struct\n",
     "type src  = DveDefinitions.event list\n",
     "type dest = Word8Vector.vector\n",
     "val map   = encodeEventList\n",
     "val unmap = decodeEventList\n",
     "end\n",
     "end\n"
    ]

end
