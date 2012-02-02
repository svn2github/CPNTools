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
(* File: output.sml
 *
 * For organizing output
 *)

exception NotValidDirExn of string

structure Output = struct

local

    (* We need this as OS.Path.concat does not work with UTF8 in 110.74 *)
    fun myConcat (a, b) = String.concat [a, "/", b]

val modelName = ref NONE : string option ref 

val modelDir = ref NONE : (string * bool) option ref

val currentRepNo = ref 0;

val currentSimNo = ref 0;

(* string * bool options: (dirname, has been checked/initialized) *)
val topOutputDir = ref NONE : (string * bool) option ref

val repOutputDir = ref NONE : (string * bool) option ref

val repLogfileDir = ref NONE : (string * bool) option ref

val simOutputDir = ref NONE : (string * bool) option ref

val simLogfileDir = ref NONE : (string * bool) option ref

fun resetDirPaths() = 
    (modelDir := NONE;
     topOutputDir := NONE;
     repOutputDir := NONE;
     repLogfileDir := NONE;
     simOutputDir := NONE;
     simLogfileDir := NONE)

fun setDir (dir, dirName:string) = 
    (dir := SOME (dirName,false));

fun findMaxDirNum (dirname, str) = 
    if not(OS.FileSys.access (dirname,[])) 
    then 0 (* dir does not yet exist, return 0 *)
    else let
	    val theDir = (OS.FileSys.openDir dirname)
	    val firstEntry = (OS.FileSys.readDir theDir)
			     
	    fun maxDirNum (SOME "", dir, strprefix) = 0
	      | maxDirNum (NONE, dir, strprefix) = 0
	      | maxDirNum (SOME theEntry, dir, strprefix) = 
		if not (String.isPrefix strprefix theEntry)
		then maxDirNum(OS.FileSys.readDir dir, dir, strprefix)
		else
		    let
			val rest = (String.extract (theEntry,String.size(strprefix),NONE))
			val areDigits = List.all Char.isDigit (String.explode rest)
			val curnum =  if areDigits 
				      then Option.getOpt(Int.fromString rest,0)
				      else 0
			val nextEntry = (OS.FileSys.readDir dir)
		    in
			Int.max (curnum, maxDirNum (nextEntry, dir,strprefix))
		    end
	    val max = maxDirNum (firstEntry,theDir, str)
	    val _ = OS.FileSys.closeDir theDir
	in
	    max
	end

fun getDirName (d,direrrstr) = 
    case !d of
	SOME (dirName,isChecked) => dirName
      | NONE => raise NotValidDirExn ("Cannot get directory name for: "^
				      direrrstr^" directory")

(* If the directory does not exist, create it if possible 
 * and indicate that the directory has been initialized *)
fun initDir(theDir,dirstr) =
    case !theDir of
	NONE => raise NotValidDirExn ("Cannot initialize "^dirstr^
				      " (no directory name given)")
      | SOME (dirName,true) => () (* has already been initialized *)
      | SOME (dirName,false) =>
	let
	    val _ = CPN'debug ("CPN'output.initDir, "^dirstr^": "^dirName)
	    val _ = if (OS.Path.isRelative dirName) andalso
		       ((OS.Path.getVolume dirName) = "")
		    then raise NotValidDirExn ("Cannot initialize "^dirstr^" with relative path: "^dirName) 
		    else () (* allow e.g. c:/ which is relative *)
	    val _ = (if OS.FileSys.access (dirName,[]) 
		      then if OS.FileSys.isDir(dirName) 
			   then ()
			   else raise NotValidDirExn ("Cannot initialize "^dirName^" as a directory because it is a file.")
		     (* FIXME: can be improved by trying to create
		      * missing ancestor directories also *)
		     else OS.FileSys.mkDir(dirName))
		handle exn => 
		       raise NotValidDirExn ("Cannot initialize "^dirstr^
					     ": "^dirName^
					     "\nException raised during initialization: "^
					     exnName(exn)^" with message "^
					     exnMessage(exn))
	in
	    theDir:= SOME (dirName,true)
	end

in 

val usedirprefix = ref true;

val outputDirName = ref "output"

val logfileDirName = ref "logfiles"

val repDirPrefix = ref "reps_";

val simDirPrefix = ref "sim_"

(* save output from different simulations in different directories  *)
val newDirPerSim = ref false;

fun useDirPrefix() = (!usedirprefix);

val runningRep = ref false;

fun getModelName()= 
    case !modelName of
	SOME name => name
      | NONE => raise InternalError ("Cannot get model name.")

fun getModelDir()= getDirName (modelDir,"model")
fun getTopOutputDir() = getDirName (topOutputDir,"top output")
fun getSimOutputDir() = getDirName (simOutputDir,"sim output")
fun getSimLogfileDir()= getDirName (simLogfileDir,"sim logfile")
fun getRepOutputDir ()= getDirName (repOutputDir,"replication output") 
fun getRepLogfileDir ()= getDirName (repLogfileDir,"replication logfile")

fun setSimOutputDir() = 
    if !topOutputDir = NONE
    then () (* do nothing, e.g. when called for new, unsaved net *)
    else 
	(if (!runningRep) orelse (!newDirPerSim)
	 then let
		 val parent = (if (!runningRep)
			       then getRepOutputDir()
			       else getTopOutputDir())
		 val maxsimdirnum = findMaxDirNum (parent, !simDirPrefix)
		 val _ = (currentSimNo := maxsimdirnum+1)
		 val newdirname = ((!simDirPrefix)^Int.toString(!currentSimNo))
		 val path = myConcat(parent,newdirname)
		 val _ = CPN'debug ("Set sim output dir: "^path)
	     in
		 setDir(simOutputDir, path)
	     end
	 else setDir(simOutputDir, getTopOutputDir());
	 setDir(simLogfileDir,myConcat (getSimOutputDir(),
					      !logfileDirName)))
	handle NotValidDirExn s => 
	       raise NotValidDirExn("Error: cannot set sim output dir because\n"^s)

fun setRepOutputDir() = 
    (let
	 val parent = getTopOutputDir()
	 val maxrepdirnum = findMaxDirNum (parent, !repDirPrefix)
	 val _ = (currentRepNo := maxrepdirnum+1)
	 val newdirname = ((!repDirPrefix)^Int.toString(!currentRepNo))
	 val path = myConcat(parent,newdirname)
	 val _ = CPN'debug ("Set rep output dir: "^path)
     in
	 setDir(repOutputDir,path);
	 setDir(repLogfileDir,myConcat (getRepOutputDir(),
					      !logfileDirName))
     end)
    handle NotValidDirExn s => 
	   raise NotValidDirExn("Error: cannot set rep output dir because\n"^s)

fun setTopOutputDir newDir = 
    (if (OS.Path.isRelative newDir) andalso 
	((OS.Path.getVolume newDir)="")
     then raise NotValidDirExn ("Cannot use relative path ("^newDir^
				") for top output directory.")
     else (); (* allow e.g. c:/ which is relative *)
     if OS.FileSys.access (newDir,[]) 
     then if OS.FileSys.isDir newDir 
	  then ()
	  else raise NotValidDirExn ("Cannot use path ("^newDir^
				     ") as top output directory because it is a file.")
     else (OS.FileSys.mkDir newDir;
	   OS.FileSys.rmDir newDir)
	  handle OS.SysErr s => 
		 raise NotValidDirExn ("Cannot use "^newDir^" as top output directory.\nOne or more ancestor directories may not exist, or access permission is inadequate.");
     setDir (topOutputDir, newDir);
     setSimOutputDir();
     setRepOutputDir())

fun setModelNameAndDirs (modelname, modeldir, (outputdir: string option))= 
    (resetDirPaths();
     if modelname=""
     then raise CPN'Error "Net name must not be empty."
     else modelName := SOME modelname;
     if modeldir = ""
     then (if Option.isSome outputdir
     then setModelNameAndDirs(modelname, Option.valOf outputdir, NONE)
     else ())
     else (((if OS.Path.isRelative(modeldir) 
	      andalso ((OS.Path.getVolume modeldir)="") (* allow, e.g. C: which is relative *)
	   then raise CPN'Error ("Cannot use relative path for model directory: "^modeldir)
	   else ();
           OS.FileSys.chDir modeldir;
	   setDir (modelDir, if (OS.Path.getVolume modeldir)=modeldir
			     then modeldir^"\\"
                       else modeldir))
          handle CPN'exc => (if Option.isSome outputdir
                             then setModelNameAndDirs(modelname, Option.valOf
                             outputdir, NONE)
                             else raise CPN'exc));
	   setTopOutputDir(case outputdir of 
			       NONE => myConcat(getModelDir(),!outputDirName)
			     | SOME dir => 
			       if (OS.Path.getVolume dir)<>"" (* Windows volume *) 
			       then (if (OS.Path.getVolume dir)=dir
				     then dir^"\\"
				     else dir (* allow e.g. c:/ which is relative *))
			       else (if OS.Path.isRelative dir
				     then myConcat(getModelDir(),dir)
				     else dir))
	   ))

fun initTopOutputDir() = initDir(topOutputDir, "top output directory")

fun initSimOutputDir() = 
    (initTopOutputDir();
     initDir(simOutputDir, "sim output directory"))

fun initSimLogfileDir() = 
    (initSimOutputDir();
     initDir(simLogfileDir, "sim logfile directory"))

fun initRepOutputDir() = 
    (initTopOutputDir();
     initDir(repOutputDir, "replication output directory"))

fun initRepLogfileDir() = 
    (initRepOutputDir();
     initDir(repLogfileDir, "replication logfile directory"))
    
(* Replaces "\\" with "\\\\" in Windows paths. 
 * Useful when saving path names in files *)
fun pathToString (path: string) = 
    concat (map Char.toString (explode path))

(* get relative paths for all directories in directory dirname 
* with names of the form prefixN where N is a sequence of digits *)
fun getNumberedDirs dirname prefix = 
    let
	val dirstream = OS.FileSys.openDir dirname
	val firstEntry = OS.FileSys.readDir dirstream
	    
	fun getNumberedDir NONE = []
	  | getNumberedDir (SOME "") = []
	  | getNumberedDir (SOME theEntry) = 
	    let
		val isprefix = String.isPrefix prefix theEntry
		val isdir = OS.FileSys.isDir (myConcat(dirname,theEntry))
		val rest = 
		    if isprefix 
		    then String.extract (theEntry,String.size(prefix),NONE)
		    else "a" (* a dummy string *)
		val areDigits = List.all Char.isDigit (String.explode rest)
		val nextEntry = OS.FileSys.readDir dirstream
		val numbereddirs = getNumberedDir nextEntry
	    in
		if isdir andalso isprefix andalso areDigits
		then theEntry::numbereddirs
		else numbereddirs
	    end
    in
	getNumberedDir firstEntry before OS.FileSys.closeDir dirstream
    end

fun enableRepDir() = 
    ((runningRep:=true);
     setRepOutputDir();
     initTopOutputDir(); 
     initRepOutputDir();
     (currentSimNo:=1));

fun repDirEnabled() = (!runningRep)
		      
fun disableRepDir() = ((runningRep:=false);
		       setSimOutputDir())

end (* local *)
end; (* struct *)
