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
structure GNUPlot =
struct
    fun line f str = TextIO.outputSubstr (f, Substring.full (String.^(str, "\n")))

    fun compare ((a, _), (b, _)) = a >= b

    fun generate_out_2d _ _ (_, []) = ()
      | generate_out_2d toString file (limit, points) =
    let
        val f = TextIO.openOut (String.^(file, ".out"))
        fun single (x, point) =
            line f (String.concat [
                  Int.toString x,
                  "\t",
                  toString point
                  ])
        val _ = List.map single (ListMergeSort.sort compare points)
        val _ = TextIO.closeOut f
    in
        ()
    end

    fun generate_out_3d toString file (xlimit, points) =
    let
        val f = TextIO.openOut (String.^(file, ".out"))
        fun print_one_x (xvalue, (_, points)) =
        let
            fun print_one (yvalue, point) =
                line f (String.concat [Int.toString xvalue, "\t",
                                       Int.toString yvalue, "\t",
                                       toString point])
            val _ = 
                List.map print_one (ListMergeSort.sort compare points)
            val _ = line f ""
        in
            ()
        end
        val _ = List.map print_one_x (ListMergeSort.sort compare points)
        val _ = TextIO.closeOut f
    in
        ()
    end

    fun gnuplot_data toString file (limit, points) =
    let
        val f = TextIO.openOut (String.^(file, ".plot"))
        val _ = line f "set terminal postscript enhanced color"
        val _ = line f (String.concat ["set out \"", file, ".ps\""])
        val _ = line f "set xlabel \"x\""
        val _ = line f "set ylabel \"result\""
        val _ = line f (String.concat ["set title \"", file, "\""])
        val _ = line f (String.concat ["plot \"", file, ".out\" using 1:2 notitle"])
        val _ = TextIO.closeOut f
    in
        generate_out_2d toString file (limit, points)
    end

    fun time f file data =
        f
            (fn (time, _) => Time.toString time)
            (String.^("Time for ", file))
            data

    fun gnuplot_data_2d toString prefix (xlimit, points) =
    let
        fun plot_one (xvalue, points) =
            gnuplot_data toString (String.concat [prefix, " - x = ", Int.toString xvalue]) points
    in
        ignore (List.map plot_one points)
    end

    fun gnuplot_data_3d toString file (xlimit, points) =
    let
        val f = TextIO.openOut (String.^(file, ".plot"))
        val _ = line f "set terminal postscript enhanced color"
        val _ = line f (String.concat ["set out \"", file, ".ps\""])
        val _ = line f "set xlabel \"x\""
        val _ = line f "set ylabel \"y\""
        val _ = line f "set zlabel \"result\""
        val _ = line f (String.concat ["set title \"", file, "\""])
        val _ = line f "set view 45,300,1,1"
        val _ = line f "set style data line"
        val _ = line f "set surface"
        val _ = line f "set pm3d"
        val _ = line f (String.concat ["splot \"", file, ".out\" using 1:2:3 notitle"])
        val _ = TextIO.closeOut f
    in
        generate_out_3d toString file (xlimit, points)
    end
end
