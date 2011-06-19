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
structure NodeDatatypes =
struct

    exception IllFormed

    type Name      = string
    type Attribute = Name * string
    type CData     = string
    datatype Node  = NODE of Name * Attribute list * CData * Node list

end


structure XMLRPCHooks =
struct
    open IgnoreHooks NodeDatatypes

    exception MalformedException

    type AppData  = Dtd.Dtd *
    Node *
    Node list
    type AppFinal = Node

    fun mkElemName dtd elem =
        UniChar.Data2String (Dtd.Index2Element dtd elem)

    fun mkAttsList dtd atts =
        map (fn (idx, HookData.AP_PRESENT (v,v2,_),_) =>
        (UniChar.Data2String
        (Dtd.Index2AttNot dtd idx),
        UniChar.Vector2String v2) | _ => raise
        MalformedException)
        atts

    fun mkNodeFromFXP dtd elem atts cdata nodelist =
        NODE (mkElemName dtd elem,
        mkAttsList dtd atts,
        UniChar.Data2String cdata,
        nodelist)

    fun addChild (NODE (name, atts, cdata, children)) child =
        NODE (name, atts, cdata, child::children)

    fun addCData (NODE (name, atts, cdata, children)) cdata' =
        NODE (name, atts, String.^ (cdata, cdata'), children)

    fun revChildren (NODE (name, atts, cdata, children)) =
        NODE (name, atts, cdata, rev children)

    fun hookStartTag ((dtd, node, stack), (_, elem, atts, cdata, empty)) =
        if   empty
        then (dtd,
        addChild node (mkNodeFromFXP dtd elem atts cdata nil),
        stack)
        else (dtd,
        mkNodeFromFXP dtd elem atts cdata nil,
        node::stack)

    fun hookEndTag ((dtd,_,nil),_) = raise IllFormed
      | hookEndTag ((dtd, child_node, node::stack), _) =
      (dtd,
      addChild node (revChildren child_node),
      stack)

    fun hookData ((dtd, node, stack), (_,vec,_)) =
        (dtd,
        addCData node (UniChar.Vector2String vec),
        stack)

    fun hookCData ((dtd, node, stack), (_,vec)) =
        (dtd,
        addCData node (UniChar.Vector2String vec),
        stack)

    fun hookCharRef ((dtd,treelist, stack), (_,c,_)) =
        (dtd,
        addCData treelist (UniChar.Data2String [c]),
        (*TEXT (UniChar.Data2String [c])::treelist,*)
        stack)


    fun hookFinish (dtd,NODE (_,_,_,nodes),nil) = hd nodes
      | hookFinish _              = raise IllFormed

end

structure DOMParser  = Parse (structure Dtd           = Dtd
structure Hooks         = XMLRPCHooks
structure Resolve       = ResolveNull
structure ParserOptions = ParserOptions())

structure Parser =
struct

    open NodeDatatypes

    exception MalformedException

    exception IllegalName
    exception IllegalEntity

    fun getBoolValue "0" = false
      | getBoolValue _ = true

    fun decodeMemberName (NODE ("name",_,cdata,_)) = cdata
      | decodeMemberName _ = raise MalformedException

    and decodeStructMember (NODE ("member",_,_,name::value::nil)) =
    (decodeMemberName name,
    decodeParamValue value)
      | decodeStructMember _ = raise MalformedException

    and decodeValue (NODE ("i4",              _,cdata,_)) = syntax.INT     (valOf (Int.fromString cdata))
      | decodeValue (NODE ("int",             _,cdata,_)) = syntax.INT     (valOf (Int.fromString cdata))
      | decodeValue (NODE ("double",          _,cdata,_)) = syntax.DOUBLE  (valOf (Real.fromString cdata))
      | decodeValue (NODE ("boolean",         _,"0",  _)) = syntax.BOOLEAN false
      | decodeValue (NODE ("boolean",         _,"1",  _)) = syntax.BOOLEAN true
      | decodeValue (NODE ("string",          _,cdata,_)) = syntax.STRING  cdata
      | decodeValue (NODE ("base64",          _,cdata,_)) = syntax.BASE64  cdata
      | decodeValue (NODE ("dateTime.iso8601",_,cdata,_)) = syntax.DATE    cdata
      | decodeValue (NODE ("struct",_,_,members)) = syntax.STRUCT (List.map decodeStructMember members)
      | decodeValue (NODE ("array",_,_,(NODE ("data",_,_,values))::nil)) =
      syntax.ARRAY (List.map decodeParamValue values)
      | decodeValue _ = raise MalformedException

    and decodeParamValue (NODE ("value",_,_,value::nil)) = decodeValue value
      | decodeParamValue (NODE ("value",_,cdata,nil)) = syntax.STRING cdata
      | decodeParamValue _ = raise MalformedException

    and decodeParam (NODE ("param",_,_,param::nil)) = decodeParamValue param
      | decodeParam _ = raise MalformedException

    and decodeParams (NODE ("params",_,_,params)) = List.map decodeParam params
      | decodeParams _ = raise MalformedException

    and decodeMethodName (NODE ("methodName",_,cdata,_)) = cdata
      | decodeMethodName _ = raise MalformedException

    and decodeMethodCall (NODE ("methodCall",_,_,method_name::params::nil)) =
    syntax.METHOD (decodeMethodName method_name,
    decodeParams params)
      | decodeMethodCall _ = raise MalformedException
    and decodeMethodResponse (NODE ("methodResponse",_,_,[NODE ("fault",_,_,[params])])) =
    syntax.FAULT (decodeParamValue params)
      | decodeMethodResponse (NODE ("methodResponse",_,_,[params])) =
      syntax.METHOD ("",
      decodeParams params)
      | decodeMethodResponse _ = raise MalformedException

    fun parse node = decodeMethodResponse node

    fun parsestring s = 
    let	
        val tmpfilename = OS.FileSys.tmpName()
        val (tmpfilename, file) = (tmpfilename, TextIO.openOut (tmpfilename)) handle _ => 
            let
	        val _ = OS.FileSys.mkDir "/cygdrive/c/Temp" handle _ => ()
	        val _ = OS.FileSys.mkDir "/cygdrive/c/Temp/tmp" handle _ => ()
		val name = String.concat ["/cygdrive/c/Temp", tmpfilename]
	    in
	        (name, TextIO.openOut name)
	    end
        val _ = TextIO.output (file, s)
        val _ = TextIO.closeOut file
        val dtd = Dtd.initDtdTables()
        val node = DOMParser.parseDocument (SOME (Uri.String2Uri tmpfilename)) (SOME dtd) (dtd, NodeDatatypes.NODE ("DummyRoot",nil,"",nil),nil)
        val _ = OS.FileSys.remove tmpfilename
    in
        parse node
    end
end
