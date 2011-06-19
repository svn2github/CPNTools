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
 *  Module:       CPN serializer
 *
 *  Description:  Code generation function that produces
 *                serialization/unserialization functions from a net
 *                description.
 *
 *  Generate:
 *     structure CPNToolsSerializer: MODEL_SERIALIZER = struct
 *        type state = CPNToolsState.state
 *        type event = CPNToolsEvent.event
 *        structure StateSerializer  = ...
 *        structure EventsSerializer = ...
 *     end
 *     structure CPNToolsSerializerCompression: MODEL_SERIALIZER = struct
 *        type state = CPNToolsState.state
 *        type event = CPNToolsEvent.event
 *        structure StateSerializer  = ...
 *        structure EventsSerializer = ...
 *     end
 *
 *  Todo: Some type of color sets are not supported.
 *)

structure CPN'SerializerCommon: sig

type buffer

val new:
    unit
    -> buffer

val start:
    buffer
    -> unit

val readInt:
    buffer
    -> int

val writeInt:
    buffer
    -> int
    -> unit

val readUInt8:
    buffer
    -> int

val writeUInt8:
    buffer
    -> int
    -> unit

val readUInt16:
    buffer
    -> int

val writeUInt16:
    buffer
    -> int
    -> unit

val readChar:
    buffer
    -> char

val writeChar:
    buffer
    -> char
    -> unit

val readString:
    buffer
    -> string

val writeString:
    buffer
    -> string
    -> unit

val readBool:
    buffer
    -> bool

val writeBool:
    buffer
    -> bool
    -> unit

val readWord32:
    buffer
    -> Word32.word

val writeWord32:
    buffer
    -> Word32.word
    -> unit

val readOption:
    (buffer -> 'a)
    -> buffer
    -> 'a option

val writeOption:
    (buffer -> 'a -> unit)
    -> buffer
    -> 'a option
    -> unit

val readList:
    (buffer -> 'a)
    -> buffer
    -> 'a list

val writeList:
    (buffer -> 'a -> unit)
    -> buffer
    -> 'a list
    -> unit

val slice:
    buffer
    -> Word8ArraySlice.slice

val vector:
    buffer
    -> Word8Vector.vector

val fromVector:
    Word8Vector.vector
    -> buffer

end = struct

type buffer = (Word8Array.array ref * int ref)

val w0          = Word8.fromInt 0
val initialSize = ref 128

fun expand (array, pos) size =
    if !pos + size <= Word8Array.length (!array)
    then ()
    else let val newArray =
		 (initialSize := !initialSize * 2;
		  Word8Array.tabulate
		      (!initialSize,
		       (fn i => if i >= Word8Array.length (!array) then w0
				else Word8Array.sub (!array, i))))
	 in
	     array := newArray;
	     expand (array, pos) size
	 end

fun new () =
    (ref (Word8Array.array (!initialSize, w0)), ref 0)

fun start (array, pos) =
    pos := 0

fun readChar (buf as (array, pos)) = let
    val c = Word8Array.sub (!array, !pos)
in
    pos := !pos + 1;
    Byte.byteToChar c
end

fun writeChar (buf as (array, pos)) c =
    (expand buf 1;
     Word8Array.update (!array, !pos, Byte.charToByte c);
     pos := !pos + 1)

fun readWord32 (array, pos) = let
    val w1 = Word8.toLargeWord (Word8Array.sub (!array, !pos))
    val w2 = Word8.toLargeWord (Word8Array.sub (!array, !pos + 1))
    val w3 = Word8.toLargeWord (Word8Array.sub (!array, !pos + 2))
    val w4 = Word8.toLargeWord (Word8Array.sub (!array, !pos + 3))
    val w1 = Word32.fromLargeWord w1
    val w2 = Word32.fromLargeWord w2
    val w3 = Word32.fromLargeWord w3
    val w4 = Word32.fromLargeWord w4
    val _  = pos := !pos + 4;
in
    Word32.+ (Word32.+ (Word32.<< (w1, 0wx18), Word32.<< (w2, 0wx10)),
	      Word32.+ (Word32.<< (w3, 0wx8),  w4))
end

fun writeWord32 (buf as (array, pos)) w = let
    val w1 = Word32.toLarge (Word32.andb (0wxFF, Word32.>> (w, 0wx18)))
    val w2 = Word32.toLarge (Word32.andb (0wxFF, Word32.>> (w, 0wx10)))
    val w3 = Word32.toLarge (Word32.andb (0wxFF, Word32.>> (w, 0wx8)))
    val w4 = Word32.toLarge (Word32.andb (0wxFF, w))
in
    expand buf 4;
    Word8Array.update (!array, !pos,     Word8.fromLarge w1);
    Word8Array.update (!array, !pos + 1, Word8.fromLarge w2);
    Word8Array.update (!array, !pos + 2, Word8.fromLarge w3);
    Word8Array.update (!array, !pos + 3, Word8.fromLarge w4);
    pos := !pos + 4
end

fun readInt buf = Word32.toIntX (readWord32 buf)

fun writeInt buf i = writeWord32 buf (Word32.fromInt i)

fun readUInt8 (buf as (array, pos)) =
    Word8.toInt (Word8Array.sub (!array, !pos)) before pos := !pos + 1

fun writeUInt8 (buf as (array, pos)) i =
    (expand buf 1;
     Word8Array.update (!array, !pos, Word8.fromInt i);
     pos := !pos + 1)

fun readUInt16 (buf as (array, pos)) = let
    val w1 = Word8Array.sub (!array, !pos)
    val w2 = Word8Array.sub (!array, !pos + 1)
in
    pos := !pos + 2;
    256 * (Word8.toInt w1) + (Word8.toInt w2)
end

fun writeUInt16 (buf as (array, pos)) i =
    (expand buf 2;
     Word8Array.update (!array, !pos,     Word8.fromInt (i div 256));
     Word8Array.update (!array, !pos + 1, Word8.fromInt i);
     pos := !pos + 2)

fun readString (buf as (array, pos)) = let
    val lg = readInt buf
    val s  = Word8ArraySlice.slice (!array, !pos, SOME lg)
in
    pos := !pos + lg;
    Byte.bytesToString (Word8ArraySlice.vector s)
end

fun writeString (buf as (array, pos)) s = let
    val lg = String.size s
in
    writeInt buf lg;
    expand buf lg;
    Word8Array.copyVec {di = !pos, dst = !array, src = Byte.stringToBytes s};
    pos := !pos + lg
end

fun readBool (array, pos) = let
    val b = if Word8Array.sub(!array, !pos) = 0wx0 then false else true
in
    pos := !pos + 1;
    b
end

fun writeBool (buf as (array, pos)) b =
    (expand buf 1;
     Word8Array.update(!array, !pos, if b then 0wx1 else 0wx0);
     pos := !pos + 1)

fun readOption reader buf = let
    val b = readBool buf
in
    if b then SOME (reader buf) else NONE
end

fun writeOption writer buf NONE         = writeBool buf false
  | writeOption writer buf (SOME value) = (writeBool buf true;
					   writer buf value)

fun readList reader buf = let
    val lg = readUInt16 buf
    fun read 0 = []
      | read n = (reader buf :: (read (n - 1)))
in
    read lg
end

fun writeList writer buf l =
    (writeUInt16 buf (List.length l);
     List.app (writer buf) l)

fun slice (array, pos) =
    Word8ArraySlice.slice (!array, 0, SOME (!pos))

fun vector (array, pos) =
    Word8Vector.tabulate (!pos, fn i => Word8Array.sub (!array, i))

fun fromVector v =
    (ref (Word8Array.tabulate (Word8Vector.length v,
			       (fn i => Word8Vector.sub (v, i)))),
     ref 0)

end


functor CPN'Serializer (structure CPN'NetCapture: CPN'NETCAPTURE) : sig
    
    val genSerializer: CPN'NetCapture.Net -> string list

end = struct

fun concat sep [] = ""
  | concat sep (item :: []) = item
  | concat sep (item :: list) = item ^ sep ^ (concat sep list)

fun map' _ [] = []
  | map' f (item :: list) = case f item of NONE       => map' f list
					 | SOME item' => item' :: (map' f list)

val map = List.map

fun colorSerializerName colorName = "serializeCs" ^ colorName

fun colorSerializerNoCompName colorName = "serializeNoCompCs" ^ colorName

fun colorUnserializerName colorName = "unserializeCs" ^ colorName

fun colorUnserializerNoCompName colorName = "unserializeNoCompCs" ^ colorName

fun getWriter size =
    if size < 256 then "CPN'SerializerCommon.writeUInt8"
    else if size < 65536 then "CPN'SerializerCommon.writeUInt16"
    else "CPN'SerializerCommon.writeInt"

fun getReader size =
    if size < 256 then "CPN'SerializerCommon.readUInt8"
    else if size < 65536 then "CPN'SerializerCommon.readUInt16"
    else "CPN'SerializerCommon.readInt"

fun getSort (_, sorts) name =
    valOf (List.find (fn ({name = name', timed, sort}) => name = name') sorts)
    
fun compress net sort = let
    val size = csSize net sort
in
    (not (isSome size)) orelse (valOf size > 10)
end

and compressedCsSize net sort =
    if compress net sort then SOME 4 else csSize net sort

and csSize net {name, timed, sort} = let
    fun sizeOf 1 = 0
      | sizeOf 0 = 0
      | sizeOf N = let fun log2 1 = 1 | log2 N = 1 + (log2 (N div 2))
		   in log2 N end
    fun op + (NONE, _)        = NONE
      | op + (_, NONE)        = NONE
      | op + (SOME i, SOME j) = SOME (Int.+ (i, j))
    fun op > (NONE, NONE)     = false
      | op > (_, NONE)        = false
      | op > (NONE, _)        = true
      | op > (SOME i, SOME j) = Int.> (i, j)
in
    case sort of
	CPN'NetCapture.unit_cs => SOME 0
      | CPN'NetCapture.bool_cs => SOME 1
      | CPN'NetCapture.int_cs => SOME 4
      | CPN'NetCapture.char_cs => SOME 1
      | CPN'NetCapture.string_cs => NONE
      | CPN'NetCapture.alias_cs v => csSize net (getSort net v)
      | CPN'NetCapture.list_cs _ => NONE
      | CPN'NetCapture.union_cs v =>
	SOME (sizeOf (List.length v)) +
	List.foldl
	    (fn ((_, t), i) => let
		    val s = if t = ""
			    then SOME 0
			    else compressedCsSize
				     net (getSort net t)
		in
		    if i > s
		    then i
		    else s
		end) (SOME 0) v
      | CPN'NetCapture.record_cs v =>
	List.foldl (fn ((_, t), i) => i + compressedCsSize net (getSort net t))
		   (SOME 0) v
      | CPN'NetCapture.product_cs v =>
	List.foldl (fn ((_, t), i) => i + compressedCsSize net (getSort net t))
		   (SOME 0) v
      | CPN'NetCapture.enum_cs v=> SOME (sizeOf (List.length v))
      | _ =>
	raise LibBase.Unimplemented "CPN'Serializer.csSize"
end

fun genDictionaryCS {name, timed, sort} =
    String.concat [
    "functor CPNToolsDictionaryCS'", name, "'Fn(\n",
    "   val serialize: ", "CPN'SerializerCommon.buffer -> ", name, " -> unit",
    "   val unserialize: ", "CPN'SerializerCommon.buffer -> ", name, "): ",
    "sig\n",
    "   val insert:   ", name, " -> Word32.word\n",
    "   val get:      Word32.word -> ", name, "\n",
    "   val numItems: unit -> int\n",
    "end = struct\n",
    "   val size = 16384\n",
    "   val sizeW = Word.fromInt size\n",
    "   val items = ref 0\n",
    "   fun hash v = HashWord8Vector.hash v\n",
    "   val dict : Word8Vector.vector option Array.array =",
    "Array.array (size, NONE)\n",
    "   fun insert item = let\n",
    "      val buf = CPN'SerializerCommon.new ()\n",
    "      val v = (serialize buf item; CPN'SerializerCommon.vector buf)\n",
    "      val h = hash v\n",
    "      val i = Word.toInt (Word.mod (h, sizeW))\n",
    "      fun findSlot i = let\n",
    "         val v' = Array.sub (dict, i)\n",
    "      in\n",
    "         if not (isSome v') then\n",
    "            (items := !items + 1; Array.update (dict, i, SOME v); i)\n",
    "         else\n",
    "            if valOf v' = v then i else findSlot ((i + 1) mod size)\n",
    "      end\n",
    "      val i = findSlot i\n",
    "   in\n",
    "      Word32.fromInt i\n",
    "   end\n",
    "   fun get w = let\n",
    "      val v   = valOf (Array.sub (dict, Word32.toInt w))\n",
    "      val buf = CPN'SerializerCommon.fromVector v\n",
    "   in\n",
    "      unserialize buf\n",
    "   end\n",
    "   fun numItems () = !items\n",
    "end\n"
    ]

fun genSerializeCS net compressTokens (cs as {name, timed, sort}) = let
    val ser     = colorSerializerNoCompName name
    val unser   = colorUnserializerNoCompName name
    val doComp  = compressTokens andalso (compress net cs)
in
    String.concat [
    "(*\n", " *  serializer of colset ", name, "\n", " *)\n",
    case sort of
	
      (*  unit  *)
	CPN'NetCapture.unit_cs => 
	String.concat [
	"fun ", ser, " _ _ = ()\n",
	"fun ", unser, " _ = ()\n"
	]
	
      (*  bool  *)
      | CPN'NetCapture.bool_cs =>
	String.concat [
	"val ", ser, " = CPN'SerializerCommon.writeBool\n",
	"val ", unser, " = CPN'SerializerCommon.readBool\n"
	]
	
      (*  int  *)
      | CPN'NetCapture.int_cs =>
	String.concat [
	"val ", ser, " = CPN'SerializerCommon.writeInt\n",
	"val ", unser, " = CPN'SerializerCommon.readInt\n"
	]
	
      (*  char  *)
      | CPN'NetCapture.char_cs =>
	String.concat [
	"val ", ser, " = CPN'SerializerCommon.writeChar\n",
	"val ", unser, " = CPN'SerializerCommon.readChar\n"
	]
	
      (*  string  *)
      | CPN'NetCapture.string_cs =>
	String.concat [
	"val ", ser, " = CPN'SerializerCommon.writeString\n",
	"val ", unser, " = CPN'SerializerCommon.readString\n"
	]

      (*  alias  *)
      | CPN'NetCapture.alias_cs ty =>
	String.concat [
	"val ", ser, " = ", (colorSerializerName ty), "\n",
	"val ", unser, " = ", (colorUnserializerName ty), "\n"
	]

      (*  list  *)
      | CPN'NetCapture.list_cs ty =>
	String.concat [
	"val ", ser, " = CPN'SerializerCommon.writeList ",
	colorSerializerName ty, "\n",
	"val ", unser, " = CPN'SerializerCommon.readList ",
	colorUnserializerName ty,"\n"
	]
	
      (*  union  *)
      | CPN'NetCapture.union_cs items => let
	    val writer = getWriter (List.length items)
	    val reader = getReader (List.length items)
	    val i = ref 0
	    fun write (tag, ty) =
		String.concat [
		"(", tag, if ty = "" then "" else " CPN'data",
		") => (", writer, " CPN'buf ", Int.toString (!i),
		if ty = "" then ")"
		else "; " ^ (colorSerializerName ty) ^ " CPN'buf CPN'data)"
		]
		before i := !i + 1;
	    fun read (tag, ty) =
		String.concat [
		Int.toString (!i), " => ", tag,
		if ty = "" then ""
		else " (" ^ (colorUnserializerName ty) ^ " CPN'buf)"
		]
		before i := !i + 1;
	in
	    String.concat [
	    "fun ", ser, " CPN'buf CPN'item = \n",
	    "   case CPN'item of\n      ",
	    (i := 0; concat "\n    | " (map write items)), "\n",
	    "fun ", unser, " CPN'buf = \n",
	    "   case ", reader, " CPN'buf of\n      ",
	    (i := 0; concat "\n    | " (map read items)), "\n"
	    ]
	 end

      (*  record  *)
      | CPN'NetCapture.record_cs comps => let
	    fun buildPrefix (name, _) = name ^ " = CPN'" ^ name
	    val prefix = "{" ^ (concat ", " (map buildPrefix comps)) ^ "}"
	    fun write (name, ty) =
		String.concat [ colorSerializerName ty, " CPN'buf CPN'", name ]
	    fun read (name, ty) =
		String.concat [	name, " = ",
				colorUnserializerName ty, " CPN'buf" ]
	in
	    String.concat [
	    "fun ", ser, " CPN'buf ", prefix,
	    " = (\n   ", concat ";\n   " (map write comps), ")\n",
	    "fun ", unser, " CPN'buf = {\n   ",
	    concat ",\n   " (map read comps), "}\n"
	    ]
	end

      (*  product  *)
      | CPN'NetCapture.product_cs comps => let
	    val i = ref 0
	    fun productList _ = "CPN'i" ^ (Int.toString (!i))
				before i := !i + 1
	    fun write (_, ty) =
		String.concat [ colorSerializerName ty,
				" CPN'buf CPN'i", Int.toString (!i) ]
		before i := !i + 1
	    fun read (_, ty) = (colorUnserializerName ty) ^ " CPN'buf"
	in
	    String.concat [
	    "fun ", ser, " CPN'buf (",
	    (i := 0; concat ", " (map productList comps)), ") = (\n   ",
	    (i := 0; concat ";\n   " (map write comps)), ")\n",
	    "fun ", unser, " CPN'buf =\n",
	    "   (", concat ", " (map read comps), ")\n"
	    ]
	end

      (*  enum  *)
      | CPN'NetCapture.enum_cs tags => let
	    val reader = getReader (List.length tags)
	    val writer = getWriter (List.length tags)
	    val i = ref 0
	    fun write tag =
		String.concat [
		tag, " => ", writer, " CPN'buf ", Int.toString (!i)
		] before i := !i + 1;
	    fun read tag =
		String.concat [
		Int.toString (!i), " => ", tag
		] before i := !i + 1;
	in
	    String.concat [
	    "fun ", ser, " CPN'buf CPN'item = \n",
	    "   case CPN'item of\n          ",
	    (i := 0; concat "\n     | " (map write tags)), "\n",
	    "fun ", unser, " CPN'buf =\n",
	    "   case ", reader, " CPN'buf of\n       ",
	    (i := 0; concat "\n     | " (map read tags)), "\n"
	    ]
	end

      (*  unimplemented stuffs  *)
      | _ =>
	(print ("Error: unimplemented feature: serialization of colset " ^
		name);
	 raise LibBase.Unimplemented "CPN'Serializer.genSerializeCS"),

    String.concat (
    if not doComp
    then [ "val ", colorSerializerName name, "   = ", ser, "\n",
	   "val ", colorUnserializerName name, " = ", unser ]
    else [ genDictionaryCS cs,
	   "structure CPNToolsDictionaryCS'", name, " = CPNToolsDictionaryCS'",
	   name, "'Fn(\n",
	   "val serialize   = ", ser, "\n",
	   "val unserialize = ", unser, ")\n",
	   "fun ", colorSerializerName name, " CPN'buf CPN'item = let\n",
	   "   val CPN'id = CPNToolsDictionaryCS'", name, ".insert CPN'item\n",
	   "in\n",
	   "   CPN'SerializerCommon.writeWord32 CPN'buf CPN'id\n",
	   "end\n",
	   "fun ", colorUnserializerName name, " CPN'buf = let\n",
	   "   val CPN'id = CPN'SerializerCommon.readWord32 CPN'buf\n",
	   "in\n",
	   "   CPNToolsDictionaryCS'", name, ".get CPN'id\n",
	   "end" ]),
    "\n"
    ]
end

fun buildPagePlaces (page as {name = pageName,
			      places, subpages, ...}: CPN'NetCapture.Page,
		     pages, var) = let
    fun getPage name = let
	fun getPage' [] = raise LibBase.Impossible "CPN'Serializer.buildPlaces"
	  | getPage' ((page: CPN'NetCapture.Page) :: list) =
	    if #name page = name then page else getPage' list
    in getPage' pages end
    fun buildPlace (p as {name, port = false,
			  sort, id, ...}: CPN'NetCapture.Place) =
	SOME (name, String.concat ["#", name, "(", var, ")"], "p" ^ id, sort)
      | buildPlace _ = NONE
    fun buildSubpagePlaces ({transition_name,
			     page_name, ...}: CPN'NetCapture.Subpage) =
	(transition_name,
	 buildPagePlaces (getPage page_name, pages,
			  String.concat ["#", transition_name, "(", var, ")"]))
    val places = map' buildPlace places
    val (trans, transInfo) = ListPair.unzip (map buildSubpagePlaces subpages)
    val (subPlaces, exprs) = ListPair.unzip transInfo
    val expr1 = concat ", " (map (fn (t, ex) => t ^ " = " ^ ex)
				 (ListPair.zip (trans, exprs)))
    val expr2 = concat ", " (map (fn (n, _, i, _) => n ^ " = " ^ i)
				 places)
    val expr = if expr1 <> "" andalso expr2 <> "" then expr1 ^ ", " ^ expr2
	       else if expr1 = "" then expr2 else expr1
    val expr = "{" ^ expr ^ "}"
    val places = List.@ (places, List.concat subPlaces)
in
    (places, expr)
end

fun buildPlaces (net as (pages, _), var) = let
    fun build (page as {name, prime = 1, ...}: CPN'NetCapture.Page) =
	SOME (name,
	      buildPagePlaces (page, pages, "#" ^ name ^ "(" ^ var ^ ")"))
      | build _ = NONE
    val (names, places) = ListPair.unzip (map' build pages)
    val (places, exprs) = ListPair.unzip places
    val expr = String.concat [
	       "{",
	       concat ", " (map (fn (t, ex) => String.concat [t, " = ", ex])
				(ListPair.zip (names, exprs))),
	       "}"
	       ]
in
    (List.concat places, expr)
end

fun genSerializeState (net as (pages, sorts): CPN'NetCapture.Net) places = let
    fun serializePlace (_, p, _, sort) =
	String.concat [
	"   CPN'SerializerCommon.writeUInt16 CPN'buf ",
	"(List.length (", p, "));\n",
	"   List.app (fn CPN'item => ", (colorSerializerName sort),
	" CPN'buf CPN'item) (",	p, ")"
	]
in
    String.concat [
    "fun serializeState' (CPN's:   CPNToolsState.state,\n",
    "                     CPN'buf: CPN'SerializerCommon.buffer) = (\n",
    concat ";\n" (map serializePlace places),
    ")\n",
    "fun serializeState (CPN's: CPNToolsState.state) = let\n",
    "   val CPN'buf = CPN'SerializerCommon.new ()\n",
    "in\n",
    "   serializeState' (CPN's, CPN'buf);\n",
    "   CPN'buf\n",
    "end\n"
    ]
end

fun genUnserializeState (net as (pages, sorts): CPN'NetCapture.Net)
			places expr = let
    fun unserializePlace (_, p, id, sort) =
	"   val n = CPN'SerializerCommon.readUInt16 CPN'buf\n" ^
	"   val " ^ id ^ " = List.tabulate (n, fn _ => " ^
	(colorUnserializerName sort) ^ " CPN'buf)"
in
    String.concat [
    "fun unserializeState (CPN'buf: CPN'SerializerCommon.buffer) = let\n",
    concat "\n" (map unserializePlace places), "\n",
    "in\n",
    "   ", expr , "\n",
    "end\n"
    ]
end

fun buildTrans (pages, _) =
    List.concat (map (fn p: CPN'NetCapture.Page => (map (fn t => (#name p, t))
					     (#transitions p))) pages)

fun transVars ({variables, free_variables, ...}: CPN'NetCapture.Transition) =
    List.@ (List.concat variables, free_variables)
    
fun genSerializeEvent (net as (pages, sorts): CPN'NetCapture.Net) trans = let
    val i = ref 0
    fun serializeBinding (p, t as {name, ...}: CPN'NetCapture.Transition) = let
	fun def {name, sort} =
	    String.concat [ name, " = CPN'", name ]
	fun serializeVar {name, sort} =
	    String.concat [ colorSerializerName sort, " CPN'buf CPN'", name ]
	val vars = transVars t
	val varsDef = concat ", " (map def vars)
    in
	String.concat [
	"Bind.", p, "'", name, " (_, {", varsDef, "}) => (",
	"CPN'SerializerCommon.writeUInt16 CPN'buf (", Int.toString (!i), ")",
	if vars <> [] then "; " else "",
	concat "; " (map serializeVar vars),
	")"]
	before i := !i + 1
    end
in
    String.concat [
    "fun serializeEvent' (CPN'evt: CPNToolsEvent.event,\n",
    "                     CPN'buf: CPN'SerializerCommon.buffer) =\n",
    "   (case CPN'evt of\n",
    "       Bind.CPN'FAKE => ",
    "raise LibBase.Impossible \"event serialization\"\n",
    "     | ", concat "\n     | " (map serializeBinding trans), ")\n",
    "fun serializeEvent (CPN'evt: CPNToolsEvent.event) = let\n",
    "   val CPN'buf = CPN'SerializerCommon.new ()\n",
    "in\n",
    "   serializeEvent' (CPN'evt, CPN'buf);\n",
    "   CPN'buf\n",
    "end\n"
    ]
end
    
fun genUnserializeEvent (net as (pages, sorts): CPN'NetCapture.Net) trans = let
    val i = ref 0
    fun unserializeBinding
	    (p, t as {name, ...}: CPN'NetCapture.Transition) = let
	fun unserializeVar {name, sort} =
	    String.concat [
	    name, " = ", colorUnserializerName sort, " CPN'buf" ]
	val vars = transVars t
    in
	String.concat [
	Int.toString (!i), " => Bind.", p, "'", name, " (1, { ",
	concat ", " (map unserializeVar vars), " })"
	]
	before i := !i + 1
    end
in
    String.concat [
    "fun unserializeEvent (CPN'buf: CPN'SerializerCommon.buffer) =\n",
    "   case CPN'SerializerCommon.readUInt16 CPN'buf of\n",
    "       ", concat "\n     | " (map unserializeBinding trans), "\n",
    "     | _ => raise LibBase.Impossible \"event unserialization\"\n"
    ]
end

fun genNetSerializer (net as (pages, sorts): CPN'NetCapture.Net,
		      compressTokens: bool) = let
    val (places, expr) = buildPlaces (net, "CPN's")
    val trans = buildTrans net
in
    [
    "local\n",
    "structure CPNToolsSerializerInternal = struct\n\n",
    concat "\n" (map (fn cs => genSerializeCS net compressTokens cs) sorts),
    genSerializeState net places, "\n",
    genUnserializeState net places expr, "\n",
    genSerializeEvent net trans, "\n",
    genUnserializeEvent net trans, "\n",
    "fun serializeStateAndEvents' ((CPN's:    CPNToolsState.state,\n",
    "                               CPN'evts: CPNToolsEvent.event list),\n",
    "                               CPN'buf:  CPN'SerializerCommon.buffer) = ",
    "let\n",
    "   fun writeEvent CPN'buf CPN'evt = serializeEvent' (CPN'evt, CPN'buf)\n",
    "in\n",
    "   serializeState' (CPN's,   CPN'buf);\n",
    "   CPN'SerializerCommon.writeList writeEvent CPN'buf CPN'evts;\n",
    "   CPN'buf\n",
    "end\n",
    "fun serializeStateAndEvents (CPN's:    CPNToolsState.state,\n",
    "                             CPN'evts: CPNToolsEvent.event list) = let\n",
    "   val CPN'buf = CPN'SerializerCommon.new ()\n",
    "in\n",
    "   serializeStateAndEvents' ((CPN's, CPN'evts), CPN'buf)\n",
    "end\n",
    "fun unserializeStateAndEvents (CPN'buf: CPN'SerializerCommon.buffer) =\n",
    "   (unserializeState CPN'buf,\n",
    "    CPN'SerializerCommon.readList unserializeEvent CPN'buf)\n",
    "end\n",
    "in\n",
    "structure ",
    if compressTokens
    then "CPNToolsSerializerCompression"
    else "CPNToolsSerializer",
    ": MODEL_SERIALIZER = struct\n",
    "   type state = CPNToolsState.state\n",
    "   type event = CPNToolsEvent.event\n",
    "   structure StateSerializer  = struct\n",
    "   type src  = CPNToolsState.state\n",
    "   type dest = Word8Vector.vector\n",
    "   fun map CPN's = let\n",
    "      val CPN'buf = CPNToolsSerializerInternal.serializeState CPN's\n",
    "   in\n",
    "      CPN'SerializerCommon.vector CPN'buf\n",
    "   end\n",
    "   fun unmap CPN'v = let\n",
    "      val CPN'buf = CPN'SerializerCommon.fromVector CPN'v\n",
    "   in\n",
    "      CPNToolsSerializerInternal.unserializeState CPN'buf\n",
    "   end\n",
    "end\n",
    "   structure EventsSerializer = struct\n",
    "   type src  = CPNToolsEvent.event list\n",
    "   type dest = Word8Vector.vector\n",
    "   fun map CPN'evts = let\n",
    "      val CPN'buf = CPN'SerializerCommon.new ()\n",
    "      fun writeEvent CPN'buf CPN'evt =\n",
    "         CPNToolsSerializerInternal.serializeEvent' (CPN'evt, CPN'buf)\n",
    "   in\n",
    "      CPN'SerializerCommon.writeList writeEvent CPN'buf CPN'evts;\n",
    "      CPN'SerializerCommon.vector CPN'buf\n",
    "   end\n",
    "   fun unmap CPN'v = let\n",
    "      val CPN'buf = CPN'SerializerCommon.fromVector CPN'v\n",
    "   in\n",
    "      CPN'SerializerCommon.readList ",
    "CPNToolsSerializerInternal.unserializeEvent CPN'buf\n",
    "   end\n",
    "end\n",
    "end\n",
    "end\n"
    ]
end

fun genSerializer net =
    List.@ (genNetSerializer(net, false),
	    genNetSerializer(net, true))

end
