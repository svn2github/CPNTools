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
(* File: syntax_misc.sml
 *
 * Miscellaneous structures and values used in the syntax check.
 *)

structure CPN'SyntaxErrors = struct

    (*FIXME:not used anymore, see ml_resp*)
    val MLResp = "ML says:\n";

    val EndError = "Unexpected end\n";
    val CommaError = "Unexpected comma\n";
    val ParMismatchError = "Mismatched parenthesis\n";
	
    val IllegalCS = "Error in color-set!\n";
    val IllegalMS = "Error in initial marking!\n";
    val IllegalColIM = "Error illegal color in initial marking!\n";
    val GuardError = "Error in guard!\n";

    val FusionMatchErrorIM = "Mismatch in fusion group initial marking!\n";
    val FusionMatchErrorCS = "Mismatch in fusion group color-sets!\n";
    val SocketMatchErrorIM = "Mismatch in port/socket initial marking!\n";
    val SocketMatchErrorCS = "Mismatch in port/socket color-sets!\n";

    val TimeRegError = "Error in time region!\n";

    val CodeInputError = "Error in input-part of code region: ";
    val CodeInputVarError = "Un-bound variable: ";
    val CodeOutputError = "Error in output-part of code region: ";
    val CodeOutputVarError = "Variable bound from arc or guard: ";
    val CodeVarsOnlyError = "Only var or tuple of vars allowed\n";
    val CodeError = "Error in code region!\n";
    val ChannelError = "Error in channel region!\n";
    val PriorityError = "Error in priority region!\n";
	
    val ArcError = "Error in arc-inscription!\n";
    val RefVarError = "Reference variables not allowed!\n";	    
    val VarBindingError = "Un-bound variable in output arc!\nCannot bind variable from large color set.\n";
    val TimedExpError = "Time expression not allowed!";
    val CSnotTimedError = "Colour set is not timed: ";
    val EmptyError = "Empty inscriptions not allowed!\n";	    
end;

structure CPN'SyntaxDatatypes = struct

    exception SyntaxError of string;
	
    type rectype = 
	{exp: string, label: string, no: int, vars: string list};
	    
    datatype entrytype = 
	NonRec of string list (* not a record/tuple; just the vars *)
      | Rec of rectype list;
	
    type exptype = {exp: string, 
		    entries: entrytype,
		    (* - the label and exp for each entry. For tuples the
		     * labels are string-reps of integers, e.g., "1" *)
		    pattern : bool,
		    wldcd : (string) option, (* var *)
		    coef : (string * (string list)),
		    timed : (string * (string list)) option
		    (* 'time-exp from @+' * var list option *)
		    };
	    
    datatype aexp =
	Div of (aexp * aexp * string * string option) 
            (* the first string is the whole dividable exp, 
             * the  second is time info. The time info is used
             * if we have to make a tms-exp out of an ms-exp. *)
      | NonDiv of exptype
	
    type guard_exp = {exp: string, vars : string list, is_pat : bool}
	    
    datatype gitem =
	NonBindable of guard_exp
      | Bindable of guard_exp * guard_exp;
	    
    datatype exp =  
	Arc of 
	{place: CPN'Id.id,
	 arcs: (CPN'Id.id * CPN'PlaceTable.exp_type) list,
	 parsed_aexps: aexp ,
	 no_of_tokens: int option}
      | Guard of {item_str: string, parsed_item: gitem};
end; (* CPN'SyntaxDatatypes *)
