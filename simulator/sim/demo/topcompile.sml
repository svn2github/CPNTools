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
(* File: topcompile.sml
 *
 * Demonstration of how to compile source code by means of the
 * internal SML/NJ compiler facilities.
 * This version uses a hacked version of SML/NJ which provides a 
 * Compiler.TopCompile structure. The hack is made in order to fix a problem
 * with Compiler.TopCompile.elaborate in SML/NJ version 110 (Feb. 1998).
 * (See also /users/cpn/SML/hacks/compiler/110.viscomp).
 *
 * mw: This if for SML/NJ 110.6/7 - we now use 110.72 and things have changed
 * slightly and this doc is no longer to be considered up to date.
 *)

local (* Compile.elaborate example *)
  exception OOPS of string;
  val str = "1`2";

  val source
    = Compiler.Source.newSource
        ("", 1, TextIO.openString str, 
         false, {consumer = print, flush = fn () => (), linewidth = 80});

  val _ = print "---PARSING---\n";
  val ast = Compiler.TopCompile.parse source;

  val compInfo
    = Compiler.TopCompile.mkCompInfo
        (source, 
         #get Compiler.EnvRef.core (),
         fn any => any);
  val baseEnvRefunSC = Compiler.EnvRef.unSC (Compiler.EnvRef.pervasive);
  fun checkErrors s 
    = if Compiler.TopCompile.anyErrors compInfo then 
        raise OOPS s 
      else ();
  val {static=statenv, ...}
    = Compiler.BareEnvironment.layerEnv
         (#get (Compiler.EnvRef.topLevel) (), #get baseEnvRefunSC ());

  val _ = print "---ELABORATING---\n";
  val {absyn,...} = Compiler.TopCompile.elaborate{ast=ast,compenv=statenv,compInfo=compInfo}
          before checkErrors "Elaborate failure";
in
  val _ = Compiler.Source.closeSource source;
(*  val res = absyn; *)
end;
