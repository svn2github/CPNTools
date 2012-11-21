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
  Module:	    Major Opcodes (CMD)

  Description:	    Symbolic constants for the Major Opcodes used to
 		    communicate with the GRAM. Each message type
		    exchanged between the GRAM and DMO has a major
		    opcode (except for Evaluation Results). 

  CPN Tools
*)


(* $Source: /users/cpntools/repository/cpn2000/sml/com/majorOpcodes.sml,v $ *)

val rcsid = "$Header: /users/cpntools/repository/cpn2000/sml/com/majorOpcodes.sml,v 1.1.1.1 2001/10/02 11:34:22 cpn Exp $";


import "majorOpcodes.sig";


functor MajorOpcodes () : MAJOROPCODES = struct

    val rcsid = "$Header: /users/cpntools/repository/cpn2000/sml/com/majorOpcodes.sml,v 1.1.1.1 2001/10/02 11:34:22 cpn Exp $";

        
    (* 
     * Major Opcodes 
     *)

    (* TO GRAM *)
					(* Eval Result has no opcode *)
    val cbReq = 3;			(* Callback Request*)
    val Req = 4;			(*  Request *)
    val gfcReq = 5;			(* Gram Fn Call Request *)
    val chartReq = 6;			(* Chart Fn Call Request *)
    val simRes = 7;                     (* Sim call respons *)
    val mimicReq = 8;			(* Mimic Fn Call Request *)
	
    (* FROM GRAM *)
    (* Used in 'genericWait'. *)
    val evalReq = 1;			(* Evaluation Request *)
    val Res = 2;			(* Result *)
    val cbRes = 3;			(* Callback Result *)
    val ResErr = 4;			(* Result Error *)
    val gfcRes = 5;			(* Gram Fn Call Result *)
    val gfcResErr = 6;			(* Gram Fn Call Result Error *)
    val chartRes = 7;			(* Chart Fn Call Result *)
    val chartResErr = 8;		(* Chart Fn Call Result Error *)
    val simReq = 9;                     (* Sim Request *)
    val mimicRes = 10;			(* Mimic Fn Call Result *)
    val mimicResErr = 11;		(* Mimic Fn Call Result Error *)

    (* To ext *)
    val forwardedReqRes = 12;

end; (* functor MajorOpcodes *)


