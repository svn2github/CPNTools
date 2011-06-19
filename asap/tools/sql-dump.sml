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
structure SQLDump = struct

val language = ref ""
val machine = ref ""
val model   = ref ""
val xParam  = ref ""
val yParam  = ref ""
val method  = ref ""

fun line f str = TextIO.outputSubstr (f, Substring.full (String.^(str, "\n")))
fun compare ((a, _), (b, _)) = a >= b
			       
fun write_data_sql convert extraParams file (limit, points) = let
    val f = TextIO.openOut (String.^(file, ".sql"))

    fun genSingle (table, name, value) = let
	val c = Char.toString (Char.toLower (String.sub (table, 0)))
	val _ = line f ("SELECT @paramId := p_id from Parameter where p_name = '" ^ name ^ "';")
	val _ =
	    if table = "EnumT"
	    then (line f "SELECT @pTable := p_table from Parameter where p_id = @paramId;";
		  line f ("select @req := CONCAT('INSERT INTO " ^ table
		         ^ " (" ^ c ^ "_param, " ^ c ^ "_testRun, " ^
			 c ^ "_val) select @paramId, @id, id from '," ^
			 "@pTable, ' where txt = \\\"" ^ value ^ "\\\"');");
		  line f "prepare stmt FROM @req;";
		  line f "execute stmt;";
		  line f "deallocate prepare stmt;")
	    else
	    	  line f ("INSERT INTO " ^ table ^ " (" ^ c ^ "_param, " ^ c ^ "_testRun, " ^ c ^ "_val) " ^
			  "VALUES (@paramId, @id, " ^ value ^ ");")
    in
	()
    end
					 
    fun single (param, (time, data)) = let
	val _ = line f "start transaction;"
        val _ = line f "insert into TestRun () values ();"
        val _ = line f "SELECT @id := LAST_INSERT_ID();"
        val _ = line f "commit;"
    	val _ = genSingle ("EnumT", "Modeling Language", !language)
    	val _ = genSingle ("EnumT", "Machine", !machine)
	val _ = genSingle ("EnumT", "Model", !model)
	val _ = genSingle ("IntegerT", !xParam, Int.toString param)
	val _ = genSingle ("EnumT", "Method", !method)
	val _ = genSingle ("FloatT", "RunTime", (Time.toString time))
	val _ = genSingle ("EnumT", "Tool", "ASCoVeCo")
	val _ = genSingle ("TimeT", "RunAt", Date.fmt "'%Y-%m-%d %H:%M:%S'" (Date.fromTimeLocal (Time.now ())))
	val _ = List.map genSingle extraParams
	val _ = List.map genSingle (convert data)
    in
        ()
    end

    val _ = List.map single (ListMergeSort.sort compare points)
    val _ = TextIO.closeOut f
in
    ()
end
						     
fun write_data_sql_2d convert extraParams prefix (xlimit, points) = let
    fun write_one (xvalue, points) =
        write_data_sql convert (("IntegerT", !yParam, Int.toString xvalue) :: extraParams)
                                (String.concat [prefix, " - x = ", Int.toString xvalue]) points
in
    ignore (List.map write_one points)
end

end
