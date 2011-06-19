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
(* File: response.sml
 *
 * The module interfaces intermidiate response to the control-side.
 *)

structure CPN'Response = struct
local
    val PAUSEBEFORE 	= 10
    val PAUSEAFTER 	= 20
    val SHOWMARKINGS 	= 30
    val SHOWTOKENS 	= 40
    val ENDPAUSE	= 50
    val MANBIND 	= 60

    val response = CpnMLSys.SimProcess.response

    fun add_ints (blist,ilist,slist) is = (blist, List.@(is,ilist), slist)
in
    (************ functions to be used for pause in simulation *************) 

    local
	fun unwrap_pause (blist,np::na::ilist,slist) = let

	    fun unwrap bislists (0,res) = (res,bislists)
	      | unwrap (b::blist,ilist,s::slist) (i,res) = 
		unwrap (blist,ilist,slist) (i-1,(s,b)::res)
	      | unwrap _ _ = raise InternalError "Response.pause_unwrap"

	    val (places,bislists) = unwrap (blist,ilist,slist) (np,nil)

	    val (arcs,_) = unwrap bislists (na,nil)
	in
	    (places,arcs)
	end
	  | unwrap_pause _ = raise InternalError "Response.pause_unwrap"
    in

	fun pause_before (t,i) = 
	    unwrap_pause(response ([],[PAUSEBEFORE,i],[t]))

	fun pause_after (t,i) = 
	    unwrap_pause(response ([],[PAUSEAFTER,i],[t]))
    end

    local
	fun wrap (nil) = (nil,nil,nil)
	  | wrap ((size,string)::xs) = let
	    val (blist,ilist,slist) = wrap xs
	in
	    (blist,size::ilist,string::slist)
	end
    in
	fun show_markings marks = 
	    (response(add_ints (wrap marks) [SHOWMARKINGS, length marks]); ())

	fun show_tokens tokens =
	    (response(add_ints (wrap tokens) [SHOWTOKENS, length tokens]); ())
    end

    fun end_pause () = (response(nil,nil,nil); ())

    (*************** function to be used for manual bindings ***************) 

    fun man_bind groups (interactive:bool)= let

	fun wrap_slist (blist,ilist,slist) slist' = 
	    (blist,ilist, List.@(slist',slist))

	fun wrap_slists bislists slists = let
	    fun wrap bislists nil = bislists
	      | wrap bislists (slist::slists) = let
		val bislists = wrap bislists slists
	    in
		wrap_slist bislists slist
	    end 
	in
	    add_ints (wrap bislists slists) [length slists]
	end   

	fun wrap_groups groups = let
	    fun wrap bislists nil = bislists
	      | wrap bislists ((vars,binds)::groups) = let

		val bislists = wrap bislists groups
	    in
		add_ints (wrap_slist (wrap_slists bislists binds) vars) [length vars]
	    end
	in
	    add_ints (wrap (nil,nil,nil) groups) [length groups]
	end
	    
    in 
	if interactive 
	then (* Interact with the user via the GUI *)
	    case response (add_ints (wrap_groups groups) [MANBIND]) of
		([continue], ilist, nil) => if continue 
					    then ilist
					    else (* cancel manual bind *)
						raise CPN'CancelManBind 
	      | _ => raise InternalError "Response.man_bind" 
	else (* We pick a random binding without asking the user *)
	    map (fn i=> (CPN'Random.int i))
	        (map (fn (_,blst)=> List.length blst)
                     (groups))
    end

end
end
