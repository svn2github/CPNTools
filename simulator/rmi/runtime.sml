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
functor RuntimeFn(val host: string val port : int) :> RUNTIME =
struct
    exception Exception of string

    val buffer = ref ""
    fun send value = (buffer := String.^ (!buffer, value))

    fun flush ch =
    let
        fun transmit value = (CommsLayer.send(ch, Byte.stringToBytes value))
    in
        transmit("POST /RPC2 HTTP/1.0\nContent-Type: text/xml\nHost: ");
        transmit(host);
        transmit("\nUser-Agent: ML RPC Client/1.0\nContent-Length: ");
        transmit(Int.toString(String.size (!buffer)));
        transmit("\n\n");
        transmit(!buffer);		
(*        print("Sending: " ^ (!buffer));*)
        buffer := "";
        ()
    end

    fun parameters ch =
    let
        val lowerCase = String.map Char.toLower

        fun receiveAll ch =
        let
            fun gather acc =
            let
                val vect = CommsLayer.receive' (ch, 8192)
            in
                if Word8Vector.length vect = 0
                then acc
                else vect::acc
            end

            val data = gather []
            val data' = List.rev data
            val vect = Word8Vector.concat data'
        in
            Byte.bytesToString vect
        end

        fun removeReturn [] = []
          | removeReturn (#"\r"::(#"\n")::rest) = #"\n"::(removeReturn rest)
          | removeReturn (#"\r"::rest) = #"\r"::(removeReturn rest)
          | removeReturn (hd::tl) = hd::(removeReturn tl)

        fun getBody (#"\n"::(#"\n")::rest) = rest
          | getBody (hd::tl) = getBody tl
          | getBody [] = raise Exception "Premature EOF"

        val data = receiveAll ch
        val data' = String.explode data
        val data'' = removeReturn data'
        val data''' = getBody data''
        val data = String.implode data'''
(*        val _ = print("Got: " ^ data)*)
    in	 
        Parser.parsestring data
    end

    fun getConnection 0 = raise Exception "Could not obtain connection"
      | getConnection retries =
      CommsLayer.connect(host, port)
      handle _ => getConnection (retries - 1)

    fun invoke (name, params) =
    let	val ch = getConnection 10

        fun Int2String i =
            if (i < 0)
            then ("-" ^ (Int.toString(0 - i)))
            else (Int.toString(i))

        fun Real2String i =
            if (i < 0.0)
            then ("-" ^ (Real.toString(0.0 - i)))
            else (Real.toString(i))

        fun translate str =
        let
            fun replace ch str rest =
                (fn ch' => if ch' = ch
                           then str
                           else rest ch')
            val replacer = replace #"<" "&lt;" Char.toString
            val replacer = replace #">" "&gt;" replacer
            val replacer = replace #"&" "&amp;" replacer
        in
            String.translate replacer str
        end

        fun mapper(syntax.STRING p) = (send("<value><string>"); send(translate p); send("</string></value>"))
          | mapper(syntax.INT p) = (send("<value><int>"); send(Int2String p); send("</int></value>"))
          | mapper(syntax.DOUBLE p) = (send("<value><double>"); send(Real2String p); send("</double></value>"))
          | mapper(syntax.BOOLEAN p) = (send("<value><boolean>"); if p then send("1") else send("0"); send("</boolean></value>"))
          | mapper(syntax.DATE p) = (send("<value><dateTime.iso8601>"); send(p); send("</dateTime.iso8601></value>"))
          | mapper(syntax.BASE64 p) = (send("<value><base64>"); send(p); send("</base64></value>"))
          | mapper(syntax.ARRAY p) = (send("<value><array><data>"); map mapper p; send("</data></array></value>"))
          | mapper(syntax.STRUCT p) = (send("<value><struct>");
          (map (fn (name, param) => 
          (send("<member><name>"); send (name); 
          send("</name>"); mapper param;
          send("</member>")))
          p);
          send("</struct></value>"))						
          | mapper(syntax.UNIT ()) = ()

    in	send("<?xml version=\"1.0\"?>");
    send("<methodCall>");
    send("<methodName>");
    send(name);
    send("</methodName>");
    send("<params>");
    List.map (fn p => (send ("<param>"); mapper p; send("</param>"))) params;
    send("</params>");
    send("</methodCall>");
    flush ch;
    let
        val result = parameters ch
        val _ = CommsLayer.disconnect ch
    in	case result of
            (syntax.METHOD (_, [result])) => result
          |	(syntax.FAULT (syntax.STRUCT [("faultCode", syntax.INT code), ("faultString", syntax.STRING text)])) =>
                  (case text of
                        "java.lang.IllegalArgumentException: void return types for handler methods not supported" => syntax.UNIT ()
                      | _ => raise Exception text)
                      |	(syntax.FAULT (syntax.STRUCT [("faultString", syntax.STRING text), ("faultCode", syntax.INT code)])) =>
                              (case text of
                                    "java.lang.IllegalArgumentException: void return types for handler methods not supported" => syntax.UNIT ()
                                  | _ => raise Exception text)
                                  | _ => raise Exception "Wrong result type"
    end
    end
end;
