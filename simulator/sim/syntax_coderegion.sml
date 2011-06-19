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
(* File: syntax_coderegion.sml
 *
 * Compiler independent coderegion parser:
 *
 *     Parses a code-region on the form:
 *
 *     input  <<input var/var-tuple>>  (optional)
 *     output <<output var/var-tuple>> (optional)
 *     action <<sml expression>>
 *)

local
   open CPN'SyntaxErrors;

    val unbound_inputvars = ref [] : string list ref;

    fun skip_empty [] = []
      | skip_empty (#" "::xs) = skip_empty xs
      | skip_empty (#"\n"::xs) = skip_empty xs
      | skip_empty (#"\t"::xs) = skip_empty xs
      | skip_empty (#";"::xs) = skip_empty xs
      | skip_empty (#"\013"::xs) = skip_empty xs
      | skip_empty (e::xs) = e::xs;
	
    fun skip_blank [] = []
      | skip_blank (#" "::xs) = skip_blank xs
      | skip_blank (#"\t"::xs) = skip_blank xs
      | skip_blank (#"\n"::xs) = skip_blank xs
      | skip_blank (#"\013"::xs) = skip_blank xs
      | skip_blank (e::xs) = e::xs
	
    exception Error of string;
    datatype direction_type = input | output;
    
    val impl = implode o rev;
	
    fun mk_vartuple vars =
	concat ("("::(foldr (fn (v,[e]) => v::[e] | (v,l) => v::","::l) [")"] vars));


    (* fun add_col direction (v, (vlist,vstring));
     * - direction : input or output part
     * - v         : reversed exploded var-name 
     * - vlist     : list of var-names
     * - vstring   : string of all var-names with CS-constraint
     * description
     * - checks for legal CPN-vars
     * - checks for legal use 
     * (v bound if direction is input otherwise v un-bound)
     * - updates binding info in SyntaxTables if direction is output 
     *)

    fun bind (v,cs) = 
	if CPN'Env.is_bindable_cs cs then
	    unbound_inputvars ::= v
	else raise Error (concat [CodeInputVarError,v,"\n"])
	
    fun add_col direction (v,(vlist,vstring)) =
	let

	    val v = impl v
	    val peek_res = CPN'StringTable.find CPN'SyntaxTables.VarTable v

	in
	    case CPN'VarTable.peek v of
		NONE => raise Error (concat [v," is not a CPN variable\n"])
	      | SOME css => 
		    ((case direction of 
			 input =>
			     (* for input-vars, check that they are bindable *)
			     (case peek_res of 
				  (* if the variable is in the syntax-vartable,
				   * it is already taken care of *)
				  SOME _ => ()
				  (* if it's not in the table, make sure it's
				   * bound *)
				| NONE => (bind (v, #cs css)))
		        | output => 
			     (case peek_res of 
				  NONE => 
				      (* haven't met this one on input-arc
				       * or in guard. Fine. *)
				      () before CPN'SyntaxTables.insert_var v
				| _ =>  
				      (* have met it on input-arc or guard *)
				      raise Error (concat [CodeOutputVarError,
							    v,"\n"])));
		  (v::vlist, (concat[v,":",#cs css])::vstring))
	end;

    (* fun get_var d (res,v,l);
     * - d   : direction (input/output)
     * - res : list of vars [v,...] and list of strings "v : CS"
     * - v   : exploded current var-name
     * - l   : exploded string
     * description
     *  extracts vars from input/output part of code-seg.
     *)
	
    fun get_var d (res,[],#" "::_) = raise Error EndError
      | get_var d (res,v,#" "::xs) =  
	get_var d (add_col d (v,res),[],skip_blank xs)
      | get_var d (res,[],#","::_) = raise Error CommaError
      | get_var d (res,v,#","::xs) =  
	get_var d (add_col d (v,res),[],skip_blank xs)
      | get_var d (res,[],#"\n"::_) = raise Error EndError
      | get_var d (res,v,#"\n"::xs) =  get_var d (add_col d (v,res),[],xs)
      | get_var d (res,[],xs as #")"::xs') =  (res,xs)
      | get_var d (res,v,xs as #")"::xs') =  (add_col d (v,res),xs)
      | get_var d (res,[],#";"::_) = raise Error EndError
      | get_var d (res,v,xs as #";"::xs') =  (add_col d (v,res),xs)
      | get_var d (res,v,e::xs) =  get_var d (res,e::v,xs)
      | get_var d (res,[],[]) = raise Error EndError
      | get_var d (res,v,[]) =  (add_col d (v,res),[])

    (* fun get_vars extract the variables of an input/output part
     * of a code-segment. 'd' denotes in/out direction. 
     * The vars are allowed to be listed as '(a,b,c,...)' or 'a,b,c,...'
     *)
    fun get_vars d (#"("::xs) =
	let
	    val ((varl,vars),xs) = get_var d (([],[]),[],skip_blank xs);
	in
	    (case xs of
		 #")"::xs' => ((rev varl,mk_vartuple (rev vars)),xs')
	       | _ => raise Error ParMismatchError)
	end
      | get_vars d l = 
	let
	    val ((varl,vars),xs) = get_var d (([],[]),[],skip_blank l);
	in
	    case xs of
		(#")"::_) => raise Error ParMismatchError
	      | _ => case vars of 
		    [_] => ((rev varl, mk_vartuple (rev vars)), xs)
		  | _ => raise Error CodeVarsOnlyError
	end;

    fun get_input_part ((#"i")::(#"n")::(#"p")::(#"u")::(#"t")::xs) = 
	(get_vars input (skip_blank xs)
	 handle Error s => raise Error (concat [CodeInputError,s]))
      | get_input_part l = (([],"_"),l)
	
    fun get_output_part ((#"o")::(#"u")::(#"t")::(#"p")::(#"u")::(#"t")::xs) = 
	(get_vars output (skip_blank xs)
	 handle Error s => raise Error (concat [CodeOutputError,s]))
      | get_output_part l = (([],"_"),l)
	
    fun get_action_part [] = ""
      | get_action_part ((#"a")::(#"c")::(#"t")::(#"i")::(#"o")::(#"n")::xs) = 
	(case (skip_empty xs) of
	     [] => raise Error "action part expected"
		 (* skip trailing semi-colons and wrap in paranthesis: *)
	   | xs' => implode ((#"(")::(rev (#")"::(skip_empty (rev xs'))))))
      | get_action_part (_::xs) = raise Error "action part expected"

in
    fun CPN'parse_coderegion str = 
	case (skip_empty (explode str)) of
	    []  => NONE
	  | str' =>
		let
		    val _ = unbound_inputvars := [];
		    val (in_vars,rest) = get_input_part str';
		    val (out_vars,rest) = get_output_part (skip_empty rest);
		    val (code_action) =  get_action_part (skip_empty rest);
		in
		    SOME (in_vars,
			  out_vars,
			  code_action,
			  !unbound_inputvars)
		end
	    handle Error s => raise CPN'SyntaxDatatypes.SyntaxError s;
end;
