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
(* File: perfreport.sml
 *
 * Performance reports
 *)

(* FIXME find better place for this? *)
fun CPN'underlineSpaces (str:string) = 
    let
	val strlist = explode str
	fun underline [] = []
	  | underline (ch::rest) = 
	    if (ch = #" ")
		then (#"_"::(underline rest))
	    else (ch::(underline rest))
	val newstrlist = underline strlist
    in
	implode newstrlist
    end;

structure CPN'PerfReport = struct

val noDec      = ref 2;  (* Number of decimals in print                     *)
val col        = ref 8; (* Number of digits of numbers in print           *)
val txtCol     = ref 16; (* Number of chars for the desc. column in print *)

datatype perfreport_format = ASCII_Format | LaTeX_Format | HTML_Format;

(* Hacks to include step and time info without having to 
 * convert CPN'PerfReport to a functor. The following two references
 * are updated when CPN'bootstrap is invoked *)
val step = ref (fn () => IntInf.fromInt 0)
val timestr = ref (fn () => "")

val theReportFormat = ref HTML_Format;

val autosave_val = ref false;
fun autoSave() = (!autosave_val);
fun setAutoSave (b) = (autosave_val:=b);

val saveSimReport = ref true
val saveIIDReport = ref true
val saveCIReports = ref true

val useSuffix = ref true;
val txtSuffix = ".txt";
val htmlSuffix = ".html";
val texSuffix = ".tex";
val currentSuffix = ref ".html";

fun changeFormat (theFormat) = 
    let
	val newSuffix = (case theFormat of
				 ASCII_Format => txtSuffix
			       | LaTeX_Format => texSuffix
			       | HTML_Format => htmlSuffix)
    in
	(currentSuffix := newSuffix;
	 theReportFormat := theFormat)
    end;


val untimed_sim_get_stats_funs = 
    ref []: (CPN'Id.id * string * (unit -> (string * CPN'StatStrings))) list ref
val timed_sim_get_stats_funs = 
    ref []: (CPN'Id.id * string * (unit -> (string * CPN'StatStrings))) list ref
val untimed_rep_get_stats_funs = 
    ref []: (CPN'Id.id * string * (unit -> (string * CPN'StatStrings))) list ref
(* FIXME: meaningful to have timed rep data collectors? *)
val timed_rep_get_stats_funs = 
    ref []: (CPN'Id.id * string * (unit -> (string * CPN'StatStrings))) list ref

fun insert_in_list (mid,name,x) funlistref = 
    let
	fun update (mid,name,x) [] = [(mid,name,x)]  
	  | update (mid,name,x) ((i,n,v)::rest) = 
	    if mid=i
	       then (mid,name,x)::rest
	    else (i,n,v)::(update (mid,name,x) rest)
	val updatedlist = update (mid,name,x) (!funlistref)
	val sorted = Misc.sort (fn ((_,n1,_),(_,n2,_)) => String.<(n1,n2))
		     updatedlist
    in
	funlistref := sorted
    end

(* FIXME find better names. lists of statvars with independent data
 * observations (from independent replications)
 * Alternatively, find a better way to do this. *)
(* id * monitor name * (statistic name * statvar) list *)
val intuntimed_statvars_iidobs = 
    ref []: (CPN'Id.id * string * (string * CPN'IUSV.Statvar) list) list ref
val realuntimed_statvars_iidobs = 
    ref []: (CPN'Id.id * string * (string * CPN'RUSV.Statvar) list) list ref

fun insert_in_iid_list (mid,mname,statname,statvar) funlistref = 
    let
	(* keep lists sorted when inserting *)
	fun update_svlist (sn,sv) [] = [(sn,sv)]
	  | update_svlist (sn,sv) ((sn2,sv2)::rest) = 
	    if sn=sn2
	    then (sn,sv)::rest
	    else if String.>(sn2,sn)
	    then (sn,sv)::(sn2,sv2)::rest
	    else (sn2,sv2)::(update_svlist (sn,sv) rest)

	fun update (mid,mname,statname,statvar) [] = 
	    [(mid,mname,[(statname,statvar)])]  
	  | update (mid,mname,statname,statvar) ((i,mn,svlist)::rest) = 
	    if mid=i andalso mname=mn 
	       then (mid,mname,update_svlist (statname,statvar) svlist)::rest
	    else (i,mn,svlist)::(update (mid,mname,statname,statvar) rest)
	val updatedlist = update (mid,mname,statname,statvar) (!funlistref)
    in
	funlistref := updatedlist
    end

fun rm_from_lists mid = 
    let
	fun rm l = List.filter (fn (i,n,x) => i<>mid) l
	fun rm2 l = List.filter (fn (i,n,svlist) => i<>mid) l
    in
	untimed_sim_get_stats_funs := rm (!untimed_sim_get_stats_funs);
	timed_sim_get_stats_funs := rm (!timed_sim_get_stats_funs);
	untimed_rep_get_stats_funs := rm (!untimed_rep_get_stats_funs);
	timed_rep_get_stats_funs := rm (!timed_rep_get_stats_funs);
	intuntimed_statvars_iidobs := rm2 (!intuntimed_statvars_iidobs);
	realuntimed_statvars_iidobs := rm2 (!realuntimed_statvars_iidobs)
    end


type include_stats = {count : bool, first : bool, last : bool, 
		      min : bool, max : bool,
		      sum : bool,avrg : bool, ci: bool,
		      ssd : bool, vari : bool, std : bool, ss : bool,
		      starttime : bool,lasttime : bool,interval : bool}
local		     
val include_timedstats = ref ({count = true,
			      min = true,
			      max = true, 
			      first = false, 
			      last = false, 
			      avrg = true, 
			      ci = false, 
			      ssd = false, 
			      vari = false, 
			      std = false,
			      sum = false,
			      ss = false,
			      starttime = false,
			      lasttime =  false,
			      interval =  false} : include_stats)

val include_untimedstats = ref ({count = true,
				min = true,
				max = true, 
				first = false, 
				last = false, 
				avrg = true, 
				ci = false, 
				ssd = false, 
				vari = false, 
				std = false,
				sum = true,
				ss = false,
				starttime = false,
				lasttime =  false,
				interval =  false}: include_stats)

val include_iidstats = ref ({count = false,
			    min = true,
			    max = true, 
			    first = false, 
			    last = false, 
			    avrg = true, 
			    ci = true, 
			    ssd = false, 
			    vari = false, 
			    std = true,
			    sum = false,
			    ss = false,
			    starttime = false,
			    lasttime =  false,
			    interval =  false}: include_stats)
in

fun selectStats (s:include_stats, statref) = statref:=s

fun selectUntimedStats {avrg:bool,ci:bool,count:bool,
			first:bool,last:bool, max:bool,
			min:bool,ss:bool,ssd:bool,std:bool,
			sum:bool,vari:bool} = 
    selectStats ({avrg=avrg,ci=ci,count=count,first=first,
		  last=last,max=max,min=min,ss=ss,ssd=ssd,
		  std=std,sum=sum,vari=vari, 
		  interval=false,starttime=false,lasttime=false},
		 include_untimedstats)

fun getIncludedUntimedStats() = 
    let
	val st = !include_untimedstats
    in
	{avrg= #avrg st,ci= #ci st,count= #count st,first= #first st,
	 last= #last st, max= #max st,min= #min st,ss= #ss st,ssd= #ssd st,
	     std= #std st,sum= #sum st,vari= #vari st} 
    end

fun selectTimedStats {avrg:bool,ci:bool,count:bool,
		      first:bool,last:bool, max:bool,
		      min:bool,ss:bool,ssd:bool,std:bool,
		      sum:bool,vari:bool,
		      interval:bool,lasttime:bool,starttime:bool} = 
    selectStats ({avrg=avrg,ci=ci,count=count,first=first,
		  interval=interval,last=last,lasttime= lasttime,
		  max=max,min=min,ss=ss,ssd=ssd,
		  starttime= starttime,std=std,sum=sum,vari=vari},
		 include_timedstats)

fun getIncludedTimedStats() = 
    let
	val st = !include_timedstats
    in
	{avrg= #avrg st,ci= #ci st,count= #count st,first= #first st,
	 last= #last st, max= #max st,min= #min st,ss= #ss st,ssd= #ssd st,
	 std= #std st,sum= #sum st,vari= #vari st,
	 lasttime= #lasttime st, starttime= #starttime st, 
	 interval= #interval st} 
    end

fun selectIIDStats {avrg:bool,ci:bool,count:bool,
		    first:bool,last:bool, max:bool,
		    min:bool,ss:bool,ssd:bool,std:bool,
		    sum:bool,vari:bool} = 
    selectStats ({avrg=avrg,ci=ci,count=count,first=first,
		  last=last,max=max,min=min,ss=ss,ssd=ssd,
		  std=std,sum=sum,vari=vari,
		  interval=false,starttime=false,lasttime=false},
		 include_iidstats)

fun getIncludedIIDStats() = 
    let
	val st = !include_iidstats
    in
	{avrg= #avrg st,ci= #ci st,count= #count st,first= #first st,
	 last= #last st, max= #max st,min= #min st,ss= #ss st,ssd= #ssd st,
	     std= #std st,sum= #sum st,vari= #vari st} 
    end


fun getNumStats (includeStats: include_stats) = 
    (if (#count includeStats) then 1 else 0)+
    (if (#sum includeStats) then 1 else 0)+
    (if (#avrg includeStats)  then 1 else 0)+
    (if (#ss includeStats) then 1 else 0)+
    (if (#ssd includeStats) then 1 else 0)+
    (if (#vari includeStats) then 1 else 0)+
    (if (#std includeStats) then 1 else 0)+
    (if (#min includeStats) then 1 else 0)+
    (if (#max includeStats) then 1 else 0)+
    (if (#first includeStats) then 1 else 0)+
    (if (#last includeStats) then 1 else 0)+
    (if (#starttime includeStats) then 1 else 0)+
    (if (#lasttime includeStats) then 1 else 0)+
    (if (#interval includeStats) then 1 else 0)+
    (if (#ci includeStats)  
     then length (CPN'PerfOptions.get_ci_percentages())
     else 0)
(* 
fun setOptions (timedstats:bool list,untimedstats:bool list) = 
    if (length timedstats = 14) andalso (length untimedstats = 11)
    then (timedstatslist := timedstats;
	  untimedstatslist := untimedstats)
    else raise InternalError "CPN'PerfReport.setStatLists expects one list with 14 boolean values and another list with 11 boolean values"
	*) 
fun addHTMLSeparators (""::rest,celltype:string) = 
    addHTMLSeparators(rest,celltype)
  | addHTMLSeparators (elm::rest,celltype) = 
    "\t<"^celltype^"> "^elm^" </"^celltype^">\n"^
    (addHTMLSeparators (rest,celltype))
  | addHTMLSeparators _ = ""

fun ReportHeaderElements(includeStats: include_stats) = 
    List.filter 
	(fn s => s<>"")
	(("Name"::
	  (if (#count includeStats)      then "Count"         else "")::
	  (if (#sum includeStats)  then "Sum"           else "")::
	  (if (#avrg includeStats)  then "Avrg"          else "")::
	  (if (#ci includeStats)  
	   then map (fn p => ((Int.toString p)^"% Half Length"))
		    (CPN'PerfOptions.get_ci_percentages())
	   else []))^^
         ((if (#ss includeStats)  then "SS"            else "")::
	  (if (#ssd includeStats)  then "SSD"           else "")::
	  (if (#vari includeStats)  then "Variance"      else "")::
	  (if (#std includeStats)  then "StD"           else "")::
	  (if (#min includeStats)  then "Min"           else "")::
	  (if (#max includeStats)  then "Max"           else "")::
	  (if (#first includeStats)  then "First"         else "")::
	  (if (#last includeStats) then "Last"          else "")::
	  (if (#starttime includeStats) then "First Time" else "")::
	  (if (#lasttime includeStats) then "Last Time" else "")::
	  (if (#interval includeStats) 
	   then ["Time Interval"]
	   else [])))

fun filterStats(includeStats: include_stats,statstrings:CPN'StatStrings) = 
    List.filter 
	(fn s => s<>"")
	(((if (#count includeStats) then #count statstrings else "")::
	  (if (#sum includeStats)  then #sum statstrings    else "")::
	  (if (#avrg includeStats)  then #avrg statstrings  else "")::
	  (if (#ci includeStats)  
	   then map (fn cistr => (#half_length cistr))
		    (#ci statstrings)
	   else []))^^
         ((if (#ss includeStats)  then #ss statstrings  else "")::
	  (if (#ssd includeStats)  then #ssd statstrings else "")::
	  (if (#vari includeStats)  then #vari statstrings else "")::
	  (if (#std includeStats)  then #std statstrings else "")::
	  (if (#min includeStats)  then #min statstrings else "")::
	  (if (#max includeStats)  then #max statstrings else "")::
	  (if (#first includeStats)  then #first statstrings else "")::
	  (if (#last includeStats) then #last statstrings else "")::
	  (if (#starttime includeStats) 
	   then Option.getOpt(#starttime statstrings,"")
	   else "")::
	  (if (#lasttime includeStats) 
	   then Option.getOpt(#lasttime statstrings,"")
	   else "")::
	  (if (#interval includeStats) 
	   then [Option.getOpt(#interval statstrings,"")]
	   else [])))

(* Generate Confidence Interval String  *)
(* confIntfFun = CPN'ConfidenceInterval.calculateIUSV or
                    CPN'ConfidenceInterval.calculateRUSV *)
fun genConfIntvStr confIntvFun percent countFun (name,sv) = 
    let
	val {percentage,avrg,half_length,
	     upper_endpoint,lower_endpoint} = confIntvFun(sv,percent)
	val n = countFun sv
	val ulname = CPN'underlineSpaces name
	val d = !CPN'PerfOptions.decimaldigits
    in
	ulname^" "^Int.toString percent^" "^(Int.toString n)^" "^
	(* Use FIX format to avoid scientific notation *)
	(Real.fmt (StringCvt.FIX (SOME d)) avrg)^" "^
	(case half_length of 
	     SOME hl => (Real.fmt (StringCvt.FIX (SOME d)) hl
			 handle Overflow => "Overflow")
	   | NONE => "Insufficient")^" "^
	(case lower_endpoint of 
	     SOME le => (Real.fmt (StringCvt.FIX (SOME d)) le
			 handle Overflow => "Overflow")
	   | NONE => "Insufficient")^" "^
	(case upper_endpoint of 
	     SOME ue => (Real.fmt (StringCvt.FIX (SOME d)) ue
			handle Overflow => "Overflow")
	   | NONE => "Insufficient" )^"\n"
    end;

fun saveConfidenceIntervalReport (filename,percent) = 
    if not(!saveCIReports)
    then ()
    else     
    let
	val reportFile = TextIO.openOut(filename)
	
	val realConfIntv = 
	    map (genConfIntvStr CPN'RUSV.ci percent CPN'RUSV.count o 
		 (fn (name,sv) => (name,sv))) 
		(flatten (map (fn (mid,mn,sn_sv_list) => 
				  map (fn (sn,sv) => (mn^"_"^sn,sv)) sn_sv_list) 
			      (!realuntimed_statvars_iidobs)))
	val intConfIntv = 
	    map (genConfIntvStr CPN'IUSV.ci percent CPN'IUSV.count o 
		 (fn (name,sv) => (name,sv))) 
		(flatten(map (fn (mid,mn,sn_sv_list) => 
				 map (fn (sn,sv) => (mn^"_"^sn,sv)) sn_sv_list) 
			     (!intuntimed_statvars_iidobs)))
	
	fun  outputToFile str = TextIO.output(reportFile,str)
	val headerStr = "#name, percent, n, avrg, half length"^
			", lower ci endpoint, upper ci endpoint"^
			"\n\n"
	val sorted = Misc.sort String.< (realConfIntv^^intConfIntv)
    in
	(if (OS.FileSys.fileSize filename) = 0
	     then TextIO.output(reportFile,headerStr)
	 else ();
	 map outputToFile sorted;
	 TextIO.closeOut reportFile)
    end

val report_file_name = ref ("PerfReport")
val report_file_path = ref ""

fun set_cpntools_filename() = 
    (report_file_name:= "PerfReport")

fun filter_stat_strings ([], []) = []
  | filter_stat_strings ((s::statlist), (b::blist)) = 
    if b 
    then s::(filter_stat_strings (statlist, blist))
    else filter_stat_strings (statlist, blist)
  | filter_stat_strings (_, _) = raise InternalError "filter_stat_strings"

fun filter_stat_stringlists (statstringlists,includelist) = 
    List.map (fn (n,s) => (n,filter_stat_strings(s,includelist))) statstringlists
    
fun gen_HTML_row (name,statstrlist) =
     "<tr>\n\t<td>"^name^"</td>\n"^
     (addHTMLSeparators(statstrlist,"td"))^" </tr>\n"

fun gen_iid_HTML_rows (x,numcols) = 
    map (fn (mname,sname_statstrlist) =>
	    "<tr>\n\t<th colspan=\""^Int.toString(numcols)^
	    "\" align=\"center\">"^mname^"</th>\n</tr>\n"^
	    "<tr>\n"^
	    (concat(map (fn (sname,statstrs) => 
			    gen_HTML_row (sname,filterStats (!include_iidstats,
					  statstrs)))
		   sname_statstrlist))
	    ) x

fun gen_HTML_report (untimedstats,timedstats) = 
    let
	(*FIXME: don't use lists any more *)
	val numtcols = 1+getNumStats(!include_timedstats) 
	val numucols = 1+getNumStats(!include_untimedstats)
	val timedtable = 
	    if timedstats=[]
	    then ""
	    else 
		"<table border=\"1\" cellpadding=\"3\">\n"^
		"<tr>\n\t<th colspan=\""^Int.toString(numtcols)^
		"\" align=\"center\">Timed statistics</th>\n</tr>\n"^
		"<tr>\n"^
		(addHTMLSeparators(ReportHeaderElements(!include_timedstats),"th"))^
		"\n</tr>\n"^
		(concat(map gen_HTML_row timedstats))^
		"\n</table>"

	val untimedtable = 
	    if untimedstats=[]
	    then ""
	    else 
		"<table border=\"1\" cellpadding=\"3\">\n"^
		"<tr>\n\t<th colspan=\""^Int.toString(numucols)^
		"\" align=\"center\">Untimed statistics</th>\n</tr>\n"^
		"<tr>\n"^
		(addHTMLSeparators(ReportHeaderElements(!include_untimedstats),"th"))^
		"\n</tr>\n"^
		(concat(map gen_HTML_row untimedstats))^
		"\n</table>"

    in
       	"<html>\n<head>\n\
        \<title>CPN Tools Simulation Performance Report</title>\n</head>\n\
        \<body>\nCPN Tools Simulation Performance Report<br>\n\
	\Net: "^
	Output.myConcat(Output.getModelDir(),
		       Output.getModelName())^
        "<hr><p>\
	\Note that these statistics have been calculated for data that \
	\is not necessarily independent or identically distributed.<p>"^
	timedtable^"\n<p>\n"^
	untimedtable^"\n<p>\n"^
	"Simulation steps executed: "^IntInf.toString(!step())^
	(case (!timestr()) of
	     "" => ""
	   | s => "<br>\nModel time: "^s)^
        "\n<hr>\nGenerated: "^
        (Date.toString(Date.fromTimeLocal(SMLTime.now())))^"<br>\n"^
        "\n</body></html>\n"
    end

fun save_sim_report (filename_option) = 
    if not(!saveSimReport)
    then ()
    else
    let
	(* FIXME: What if all data collectors are inactive? *)
	val untimedstats = 
	    List.map (fn (mid,name,f) => f()) (!untimed_sim_get_stats_funs)
	val timedstats = 
	    List.map (fn (mid,name,f) => f()) (!timed_sim_get_stats_funs)
    in
	case (untimedstats,timedstats) of 
	    ([],[]) =>     
	    CPN'debug ("CPN'PerfReport.save_sim_report, report not saved, no (active) data collectors.")
	  | _ => 
	    let
		val filename = case filename_option of 
				   SOME fname => fname
				 | NONE => Output.myConcat(Output.getSimOutputDir(),"PerfReport.html")
		val _ = CPN'debug ("CPN'PerfReport.save_sim_report in: "^
				   (filename))
		val uf = map (fn (name,ss) => (name,filterStats (!include_untimedstats,ss))) untimedstats
		val tf = map (fn (name,ss) => (name,filterStats (!include_timedstats,ss))) timedstats
		val stream = TextIO.openOut(filename)
		val reportStr = 
		    case (!theReportFormat) of
			HTML_Format => gen_HTML_report(uf,tf)
		      | _ => raise InternalError "Cannot save non-HTML perf report yet"
	    in
		TextIO.output(stream,reportStr);
		TextIO.closeOut(stream)
	    end 
    end
	handle InternalError s => raise InternalError ("Error saving simulation performance report: "^s)

fun gen_iid_HTML_report (untimedstats,numrep : int option) = 
    let
	val headerElems = ReportHeaderElements(!include_iidstats)
	val numcols = length headerElems

	val table = 
	    if untimedstats=[]
	    then ""
	    else 
		"<table border=\"1\" cellpadding=\"3\">\n"^
		"<tr>\n\t<th colspan=\""^Int.toString(numcols)^
		"\" align=\"center\">Statistics</th>\n</tr>\n"^
		"<tr>\n"^
		(addHTMLSeparators(headerElems,"th"))^
		"\n</tr>\n"^
		(concat(gen_iid_HTML_rows (untimedstats,numcols)))^
		"\n</table>"
    in
       	"<html>\n<head>\n\
        \<title>CPN Tools Performance Report</title>\n</head>\n\
        \<body>\nCPN Tools Performance Report<br>\n\
	\Net: "^
	Output.myConcat(Output.getModelDir(),
		       Output.getModelName())^
	(case numrep of 
	     NONE => ""
	   | SOME n => "<br>\nNumber of replications: "^Int.toString n)^
        "<hr>\n<p>\n"^
	table^"\n<p>"^
        "<hr>\nGenerated: "^
        (Date.toString(Date.fromTimeLocal(SMLTime.now())))^"<br>\n"^
        "\n</body></html>\n"
    end

fun save_iid_report (filename_option,numrep : int option) = 
    if not(!saveIIDReport)
    then ()
    else 
    let
	(* FIXME: What if all data collectors are inactive? *)

(* 	val untimedstats = 
	    List.map (fn (mid,name,f) => f()) (!untimed_rep_get_stats_funs)
	val timedstats = 
	    List.map (fn (mid,name,f) => f()) (!timed_rep_get_stats_funs) *)
	val filename = 
	    case filename_option of 
		SOME fname => fname
	      | NONE => Output.myConcat(Output.getRepOutputDir(),
				       "PerfReportIID.html")
	val intuntimedstats = 
	    List.map 
		(fn (mid,mname,sn_sv_list) => 
		    (mname,
		     List.map 
			 (fn (sn,sv) => (sn,CPN'IUSV.toStrings sv)) sn_sv_list))
		(!intuntimed_statvars_iidobs)
	val realuntimedstats = 
  	    List.map
	    (fn (mid,mname,sn_sv_list) => 
		(mname,
		 List.map
		      (fn (sn,sv) => (sn,CPN'RUSV.toStrings sv)) sn_sv_list))
		    (!realuntimed_statvars_iidobs)
    in
	if (intuntimedstats,realuntimedstats) =([],[])
	then CPN'debug ("CPN'PerfReport.save_iid_report, report not saved in"^
		       filename^"no (active) data collectors.")
	else
	    let
		val _ = CPN'debug ("CPN'PerfReport.save_iid_report in: "^(filename))
		val sorted = 
		    CPN'Misc.sort (fn ((name1,_),(name2,_)) => 
				      String.<(name1,name2))
		    (intuntimedstats^^realuntimedstats)
		    
		fun merge ((name1,sl1)::(name2,sl2)::rest) = 
		    if name1=name2
		    then (name1,sl1^^sl2)::(merge rest)
		    else (name1,sl1)::merge((name2,sl2)::rest)
		  | merge l = l

		val stream = TextIO.openOut(filename)
		val reportStr = 
		    case (!theReportFormat) of
			HTML_Format => gen_iid_HTML_report(merge sorted,numrep)
		      | _ => raise InternalError "Cannot save non-HTML iid perf report yet"
	    in
		TextIO.output(stream,reportStr);
		TextIO.closeOut(stream)
	    end
    end
	handle InternalError s => raise InternalError ("Error saving iid performance report: "^s)
end (* local *)
end; (* struct *)
