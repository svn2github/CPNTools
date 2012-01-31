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
(* File: link.sml
 *
 * Compilation file for creating the full or minimal simulator.
 *)

Compiler.Control.MC.matchRedundantWarn := true;
Compiler.Control.MC.matchRedundantError := false;

val ogpath = "../statespacefiles/";
val rmipath = "../rmi/";

exception Compile of string;

local
    (* Option used in case a minimal simulator is generated for 
     * standalone mode*)
    val compileMinimalSimulator = ref false;
(*  val _ = if !compileMinimalSimulator then use "dummydmo.sml" else ()  *)

    val compileSML97 = ref true

    datatype compiletype = required | min_only | max_only;

    fun compile (file, required) = import file
      | compile (file, min_only) =
	if !compileMinimalSimulator then use (file^".sml") else ()
      | compile (file, max_only) =
	if !compileMinimalSimulator then () else import file

in
    val _ = app compile
	[
(*	 if !compileSML97 then ("93", required) else ("97", required),*)

	 (* hash libraries from the SML/NJ distribution *)
(*       ("external/lib-base-sig",       required),   NOT IN USE *)
(*       ("external/lib-base",           required),   NOT IN USE *)
(*       ("external/listsort-sig",       required),   NOT IN USE *)
(*       ("external/list-mergesort",     required),   NOT IN USE *)
         ("external/hash-key-sig",       required),
         ("external/hash-string",        required),
         ("external/hash-table-sig",     required),
         ("external/hash-table-rep",     required),
         ("external/mono-hash-table-sig",required),
         ("external/hash-table-fn",      required),
(*       ("external/hash2-table-fn",     required),   NOT IN USE *)
         ("external/hash-table",         required),
    
	 (* basic definitions *)
	 ("basic",		required),
	 ("list_utils",		required),
	 ("random",		required),
	 ("misc",		required),
	 ("tree",		required),
	 ("time-sig",		required),
	 ("time",		required),
	 ("pq-sig",		required),
	 ("pq-tree",		required),
	 ("streamio",		required),
	 
	 (* color-set and multi-set representations *)
	 ("ms-sig",		required),
	 ("ms",			required),
	 ("tms",		required),
	 ("cs-sig",		required),
	 ("cs",			required),
	 ("cs_rs",		required),
	 ("ims-sig",		required),
	 ("unit_pims",		required),
	 ("bool_pims",		required),
	 ("treelist_pims",	required),
	 ("tree_sims",		required),
	 
	 (* symbol tables *)
	 ("tables-sig",		required),
	 ("tables",		required),

	 ("profile",		required),

	 (* dependency analysis *)
	 ("dependency-sig",     max_only),
	 ("dependency",         max_only),

	 ("environment",	max_only),
	 ("output",	        max_only),
	 
	 (* color-set declarations *)
	 ("create_decl-sig",	max_only),
	 ("create_decl",	max_only),

	 (* syntax check *)
	 ("syntax_misc",	max_only),
	 ("syntax_tables",	max_only),
	 ("syntax_ptnet",	max_only),
	 ("syntax_coderegion",	max_only),
	 ("syntax_ast",		max_only),
	 ("syntax_check-sig",	max_only),
	 ("syntax_check",	max_only),

	 (* performance facilities *)
	 ("perf/perfoptions",	     max_only),
	 ("perf/randomgen",	     max_only),
	 ("perf/statdistributions",  max_only),
	 ("perf/confidence",         max_only),
	 ("perf/untimedstatvar-sig", max_only),
	 ("perf/untimedstatvar",     max_only),
	 ("perf/timedstatvar-sig",   max_only),
	 ("perf/timedstatvar",       max_only),
	 ("perf/perfreport",         max_only),
	 
	 (* making instances *)
	 ("options-sig",	required),
	 ("options",		required),
	 ("reference-sig",	required),
	 ("reference",		required),
	 ("place-sig",		required),
	 ("place",		required),
	 ("create_ref-sig",	max_only),
	 ("create_ref",		max_only),
	 ("create_place-sig",	max_only),
	 ("create_place",	max_only),
	 ("create_trans-sig",	max_only),
	 ("create_trans",	max_only),

	 (* run-time sig *)
	 ("sim-sig",		required),
	 ("replications-sig",	max_only),

	 (* more perf stuff *)
	 ("perf/gnuplotscript", required),

	 (* monitors *)
	 ("monitor_aux",        max_only),
	 ("monitor_template",	max_only),
	 ("monitor-sig",        max_only),
	 ("monitor",            max_only),
	 ("create_monitor-sig",	max_only),
	 ("create_monitor",	max_only),
	 ("standard_monitors-sig",	max_only),
	 ("standard_monitors",	max_only),

	 (* run-time *)
	 ("pq-array",           required),
	 ("response",		required),
       ("priority-random-set",    required),
	 ("sim",		required),
	 ("replications",	max_only),

	 (* old stuff *)
(*	 ("UserDB",		max_only), *)

	 (* bootstrap and glue to C/BETA-side *)
	 ("bootstrap",		required),
	 ("simglue",		max_only),

	 (* State-space *)
	 ("avlTree", max_only),
       ("../../siminterface/cpntools/net-capture", max_only)
	 ];
end;

val _ = Compiler.Control.Print.printLength:= 100;
val _ = Compiler.Control.Print.printDepth:= 100;
val _ = CpnMLSys.GramError.debugState:= false;
open EncodeDecodeFunctions;
open ConnManagementLayer;

(* Make it possible to load ASCoVeCo code *)

functor CPN'ASAP(structure CPN'InstTable:CPN'INSTTABLE) = 
struct
    local
    structure CPN'NetCapture = CPN'NetCapture(structure CPN'InstTable = CPN'InstTable)

    val _ = CPN'NetCapture.initNet ()
    val _ = CPN'NetCapture.checkNames()

    structure CPN'State = CPN'State(structure CPN'NetCapture = CPN'NetCapture)
    
    val _ = CPN'Env.use_string(CPN'State.genMark(CPN'NetCapture.getNet()))
    val _ = CPN'Env.use_string(CPN'State.genState(CPN'NetCapture.getNet()))
    
    structure CPN'Event = CPN'Event(structure CPN'NetCapture = CPN'NetCapture)

    val _ = CPN'Env.use_string(CPN'Event.genBind(CPN'NetCapture.getNet()))
    val _ = CPN'Env.use_string(CPN'Event.genEvent(CPN'NetCapture.getNet()))
      
    structure CPN'HashFunction = CPN'HashFunction(structure CPN'NetCapture = CPN'NetCapture)

    val _ = CPN'Env.use_string(CPN'HashFunction.genHashFunction(CPN'NetCapture.getNet()))
    
    structure CPN'Order = CPN'Order(structure CPN'NetCapture = CPN'NetCapture)

    val _ = CPN'Env.use_string(CPN'Order.genStateOrder(CPN'NetCapture.getNet()))

    in
        functor Build(structure CPNToolsState : CPN'STATE structure
        CPNToolsEvent : CPN'EVENT val CPNToolsHashFunction : (word * word ->
        word) -> CPNToolsState.state -> word) =
        struct
        structure CPNToolsModel = CPNToolsModel(structure CPNToolsState = CPNToolsState
        structure CPNToolsEvent = CPNToolsEvent)

        structure CPNToolsHashFunction : HASH_FUNCTION =
        struct
            type state = CPNToolsModel.state

            fun combinator (h2, h1) =
                Word.<<(h1, 0w2) + h1 + h2 + 0w17

            val hash = CPNToolsHashFunction combinator
        end

        structure CPNToolsHashFunction2 : HASH_FUNCTION =
        struct
            type state = CPNToolsModel.state

            fun combinator (h2, h1) =
                Word.<<(h1, 0w5) + h1 + h2 + 0w720

            val hash = CPNToolsHashFunction combinator
        end

        (* structure CPNToolsStateOrder : ORD_KEY =
        struct
            type ord_key = CPNToolsModel.state

            val compare = CPN'StateOrder
        end*)
        end
    end

end;

val _ = print "CpnML.Toploop.build \"cpn.ML\";\n";
