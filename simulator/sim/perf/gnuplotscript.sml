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
(* File: gnuplotScripts.sml
 *
 * Generats gnuplot scripts for logfiles
 *)

structure CPN'GnuplotScript = struct 

val saveScripts = ref true;

fun genPlotScript ([], _, _) = raise InternalError "genPlotScript"
  | genPlotScript (paths:string list, datafile_modifiers:string option, with_style:string option) = 
    let
	fun genFileLines (path, (num,tail)) = 
	    let
		val {dir,file} = OS.Path.splitDirFile (path)
		val {base,ext} = OS.Path.splitBaseExt file
	    in
		(num-1,
		 ",\\\n"::"\""::
		 Output.pathToString(path)::
		 "\" \\\n"::
		 (case datafile_modifiers of
		      SOME s => "   "^s^"\\\n"
		    | NONE => "")::
		 "   title \""::base::" "::Int.toString(num)::"\" "::
		 (case with_style of
		      SOME s => "\\\n   "^s
		    | NONE => "")::tail)
	    end
    in
	"# gnuplot script generated by CPN Tools\n"^
	"# plot "^Int.toString(length paths)^" files\n"^
	"plot \\\n"^
	concat(tl(#2(List.foldr genFileLines ((length paths),["\n"]) paths)))
    end

(* FIXME: Error occurs in gnuplot if file to plot does not contain data *)
fun saveScript ([], scriptFileName, _, _) = 
    CPN'debug ("Gnuplot script "^scriptFileName^
	       " not saved due to empty list of files to plot") 
  | saveScript (pathsToPlot, scriptFileName, 
		datafile_modifiers:string option,
		with_style: string option) =
    let
	val script = genPlotScript(pathsToPlot, datafile_modifiers, with_style)
	val fid = TextIO.openOut scriptFileName
    in
	TextIO.output (fid, script);
	TextIO.closeOut fid
    end
    
fun saveOverviewScript (filename, []) = CPN'debug ("Overview gnuplot script not saved due to empty list of file names for gnuplot scripts") 
  | saveOverviewScript (filename, gnuplotscripts) =
    let
	val fid = TextIO.openOut filename
	fun makeTxt filename = OS.Path.base(OS.Path.file filename)
	fun genOverviewScript [] = "pause -1 \"Done\"\n"
	  | genOverviewScript (gplscriptpath::rest) = 
	    ("reset\nload \""^
	     Output.pathToString(gplscriptpath)^
	     "\"\npause -1 \""^
	     (makeTxt gplscriptpath)^"\"\n\n")^
	    (genOverviewScript rest)
	val script = genOverviewScript gnuplotscripts
    in
	TextIO.output (fid, script);
	TextIO.closeOut (fid)
    end

(* FIXME: This should not be dependent on info in CPN'MonitorTable,
 * but rather something more dynamic which reflects the current
 * status of the individual monitors *)
fun DCsWithLogfiles(mtype : CPN'MonitorTable.montype) = 
    CPN'MonitorTable.filter 
	(fn (mid,{kind,montype,...}) => 
	    case kind of
		CPN'MonitorTable.datacoll {logfile,...} => 
		logfile andalso (montype = mtype)
	      | _ => false)
	
(* FIXME: raise exception if not running replications? *)
fun relativeSimLogfilePaths () = 
    let
	val dirname = Output.getRepOutputDir()
	val simdirpaths = Output.getNumberedDirs dirname (!Output.simDirPrefix)
	val logfiledirpaths = map (fn simdirpath => Output.myConcat(simdirpath, !Output.logfileDirName)) simdirpaths
    in 
	logfiledirpaths
    end

fun saveRepScripts () = 
    let
	val timed_model = CPN'MonitorTable.model_is_timed()

	fun nameAndTimed (mid,({name,kind,...}):CPN'MonitorTable.item) =
	    case kind of
		CPN'MonitorTable.datacoll{timed,...} => (name,timed)
	      | _ => raise InternalError "saveRepScripts"

	val stepdcswithlogfiles = map nameAndTimed (DCsWithLogfiles(CPN'MonitorTable.step_monitor))
	val simlogfilepaths = relativeSimLogfilePaths()
	val relscriptdir = "gnuplot"
	val absscriptdir = Output.myConcat(Output.getRepOutputDir(), "gnuplot")
        val _ = (OS.FileSys.mkDir absscriptdir
		 handle SysErr => ())
	val _ = (if (not (OS.FileSys.isDir(absscriptdir)))
		 then  (raise (NotValidDirExn (absscriptdir)))
		 else ())
	    handle SysErr => (raise NotValidDirExn (absscriptdir))
			     
	fun saveStepDCscript (dcName, timeddc) = 
	    let
		(* the logfiles to plot *)
		val logfilenames = map (fn dirpath => Output.myConcat(dirpath,dcName^".log")) simlogfilepaths
		(* the name of the file with the gnuplot script *)
		val scriptfilename = Output.myConcat(absscriptdir, dcName^".gpl")
		val datafile_modifiers = 
		    SOME ("using "^(if timed_model 
				    then "4:1" else "2:1"))

		val with_style = 
		    SOME ("with "^(if timeddc 
				   then "steps" else "points"))

		val _ = saveScript (logfilenames, scriptfilename, datafile_modifiers, with_style)
	    in
		Output.myConcat (relscriptdir, dcName^".gpl")
	    end
	val stepDCscriptNames = map saveStepDCscript stepdcswithlogfiles
	val overviewfilename = Output.myConcat(Output.getRepOutputDir(),"plotsimlogfiles.gpl")
	val _ = saveOverviewScript (overviewfilename, stepDCscriptNames)

	val simdcswithlogfiles = map nameAndTimed (DCsWithLogfiles(CPN'MonitorTable.sim_monitor))
	fun saveRepDCscript (dcName,timed) = 
	    let
		(* the logfile to plot *)
		val logfilename = Output.myConcat(!Output.logfileDirName, dcName^".log")
		(* the name of the file with the gnuplot script *)
		val scriptfilename = Output.myConcat(absscriptdir, dcName^".gpl")
		val _ = saveScript ([logfilename], scriptfilename, SOME "using 2:1", SOME "with points")
	    in
		Output.myConcat (relscriptdir, dcName^".gpl")
	    end
	val simDCscriptNames = map saveRepDCscript simdcswithlogfiles
	val filename = Output.myConcat(Output.getRepOutputDir(),"plotreplogfiles.gpl")
    in
	saveOverviewScript (filename, simDCscriptNames)
    end

end (* struct *)
									   
