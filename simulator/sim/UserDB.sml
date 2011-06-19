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
(* File: UserDB.sml
 *
 * User interface to abstract database.
 *)

val rcsid =  "$Header: /users/cpntools/repository/cpn2000/sml/sim/UserDB.sml,v 1.1.1.1 2001/10/02 11:35:23 cpn Exp $";

(* constant 0 DbId *)

val CPN'ZeroDbId = ByteArray.array (4,0);


fun MygetCpnRegion {parent=id,rtype=rtype} = 
(Cpndb.getCpnRegion {parent=id,rtype=rtype}

handle _ => 0);


fun MakeCpnPage id = 
Cpndb.ceCreatePage {page=id,ptype=Cpndb.noPorts};

fun MakeGlobDec id = 
Cpndb.createDefBox {obj=id,page=0,dtype=Cpndb.dGlobal};

fun MakeTempDec id =
Cpndb.createDefBox {obj=id,page=0,dtype=Cpndb.dTemp};

exception makeLocDec
fun MakeLocDec {id:Cpndb.gid,pageid:Cpndb.gid} =
if (#otype (Cpndb.objTypes pageid) = Cpndb.otApage)
then (Cpndb.createDefBox {obj=id,page=pageid,dtype=Cpndb.dLocal};Cpndb.updateDefs id)
else (print ("pageid must be a valid cpn page id"^"\n");raise makeLocDec);

fun MakePlace id = (Cpndb.createPlace id; Cpndb.objTextChanged id);
fun MakeTrans id = (Cpndb.createTrans id; Cpndb.objTextChanged id);
fun MakeArc id = (Cpndb.createArc id (*objTextChanged id*));

exception makeName;
fun MakeName {cpnnodeid:Cpndb.gid,id:Cpndb.gid} =
let val obj = (Cpndb.objTypes cpnnodeid)
in
if  (#otype obj= Cpndb.otPlace) orelse (#otype obj= Cpndb.otTrans)
then (Cpndb.makeCpnRegion {obj=id,parent=cpnnodeid,rtype=Cpndb.rName}; Cpndb.objTextChanged id)
else (print ("node must be a place or a transition  id"^"\n");raise makeName) end;

exception makeColor;
fun MakeColor {id:Cpndb.gid,placeid:Cpndb.gid} =
if (#otype (Cpndb.objTypes placeid) =  Cpndb.otPlace) 
then (Cpndb.makeCpnRegion {obj=id,parent=placeid,rtype=Cpndb.rColorSet}; Cpndb.objTextChanged id)
else (print ("node must be a place"^"\n");raise makeColor);

exception makeInitMark;
fun MakeInitMark {id:Cpndb.gid,placeid:Cpndb.gid} =
if (#otype (Cpndb.objTypes placeid) = Cpndb.otPlace) 
then (Cpndb.makeCpnRegion {obj=id,parent=placeid,rtype=Cpndb.rInitMark}; Cpndb.objTextChanged id)
else (print ("node must be a place"^"\n");raise makeInitMark);

exception makeGuard;
fun MakeGuard {id:Cpndb.gid,transid:Cpndb.gid} =
if (#otype (Cpndb.objTypes transid) = Cpndb.otTrans) 
then (Cpndb.makeCpnRegion {obj=id,parent=transid,rtype=Cpndb.rGuard}; Cpndb.objTextChanged id)
else (print ("node must be a transition"^"\n");raise makeGuard);

exception makeTime;
fun MakeTime {id:Cpndb.gid,transid:Cpndb.gid} =
if (#otype (Cpndb.objTypes transid) = Cpndb.otTrans) 
then (Cpndb.makeCpnRegion {obj=id,parent=transid,rtype=Cpndb.rTime}; Cpndb.objTextChanged id)
else (print ("node must be a transition"^"\n");raise makeTime);

exception makeCodeSeg;
fun MakeCodeSeg {id:Cpndb.gid,transid:Cpndb.gid} =
if (#otype (Cpndb.objTypes transid) = Cpndb.otTrans) 
then (Cpndb.makeCpnRegion {obj=id,parent=transid,rtype=Cpndb.rCodeSeg}; Cpndb.objTextChanged id)
else (print ("node must be a transition"^"\n");raise makeCodeSeg);

exception makeArcExp;
fun MakeArcExp {arcid:Cpndb.gid,id:Cpndb.gid} =
if (#otype (Cpndb.objTypes arcid) = Cpndb.otArc) 
then (Cpndb.makeCpnRegion {obj=id,parent=arcid,rtype=Cpndb.rArcExp}; Cpndb.objTextChanged id)
else (print ("node must be a transition"^"\n");raise makeArcExp);

exception MakePort;
fun MakeInPort id = 
let val obj = Cpndb.objTypes id;
in
if (#otype obj = Cpndb.otPlace)
then (Cpndb.makePort {obj=id,ptype=Cpndb.inPort}; Cpndb.objTextChanged id)
else (print ("node must be a place  id"^"\n");raise MakePort) end;

fun MakeOutPort id = 
let val obj = Cpndb.objTypes id;
in
if (#otype obj = Cpndb.otPlace)
then (Cpndb.makePort {obj=id,ptype=Cpndb.outPort}; Cpndb.objTextChanged id)
else (print ("node must be a place  id"^"\n");raise MakePort) end;

fun MakeInOutPort id = 
let val obj = Cpndb.objTypes id;
in
if (#otype obj= Cpndb.otPlace)
then (Cpndb.makePort {obj=id,ptype=Cpndb.inoutPort}; Cpndb.objTextChanged id)
else (print ("node must be a place  id"^"\n");raise MakePort) end;

fun MakeGenPort id = 
let val obj = Cpndb.objTypes id;
in
if (#otype obj = Cpndb.otPlace)
then (Cpndb.makePort {obj=id,ptype=Cpndb.genPort}; Cpndb.objTextChanged id)
else (print ("node must be a place  id"^"\n");raise MakePort) end;

exception makeTransSub;
fun MakeTransSub {subpageid:Cpndb.gid,transid:Cpndb.gid} =
if (#otype (Cpndb.objTypes subpageid) <> Cpndb.otApage) 
then (print ("subpage must be a cpn page"^"\n");raise makeTransSub)
else if (#otype (Cpndb.objTypes transid) <> Cpndb.otTrans) 
then (print ("node must be a transition"^"\n");raise makeTransSub)
else (Cpndb.createCompound {htype=Cpndb.htSubst,obj=transid};
      Cpndb.makeSubpage {autoassign=false,obj=transid,page=subpageid};
      Cpndb.updateCompoundRegion transid);


exception PortAssign;
fun AssignPort {id:Cpndb.gid,port:Cpndb.gid,socket:Cpndb.gid} =
let val obj = Cpndb.objTypes id;
in
if (#ostype obj <> Cpndb.ostSubst) andalso 
   (#ostype obj <> Cpndb.ostInvoc)
then (print ("node must be compound"^"\n");raise PortAssign)
else if Cpndb.isPort (#id (Cpndb.objTypes port))
then (Cpndb.assignPort {obj=id,port=port,socket=socket};
      Cpndb.updateCompoundRegion id)
else (print ("node must be port"^"\n");raise PortAssign) end;

exception MakeFus;
fun MakeGlobalPlaceFus {name:string,placeid:Cpndb.gid} =
if (#otype (Cpndb.objTypes placeid) <> Cpndb.otPlace) 
   then 
      (print ("node must be a place"^"\n");raise MakeFus)
   else 
      if (Cpndb.equaldbid(
		Cpndb.getNameFusSet 
			{fstype=Cpndb.placeFus,name=name,page=CPN'ZeroDbId},
		CPN'ZeroDbId))
         then 
	    let val FusSetId = Cpndb.crPlFus
				{ftype=Cpndb.GLOBAL_FUS,page=CPN'ZeroDbId};
	    in (Cpndb.chngName {name=name,obj=FusSetId};
	       Cpndb.addToFusion {newfusion=FusSetId,obj=placeid}) 
	    end
         else (print ("global fusion set already exists"^"\n");raise MakeFus);

fun MakePagePlaceFus {name:string,pageid:Cpndb.gid,placeid:Cpndb.gid} =
if (#otype (Cpndb.objTypes pageid) <> Cpndb.otApage) 
   then 
      (print ("page must be a cpn page"^"\n");raise MakeFus)
   else 
      let val Pagedbid = (#id (Cpndb.objTypes pageid));
      in
	if (#otype (Cpndb.objTypes placeid) <> Cpndb.otPlace) 
	   then 
	      (print ("node must be a place"^"\n");raise MakeFus)
	   else 
	      if (Cpndb.equaldbid(Cpndb.getNameFusSet 
			{fstype=Cpndb.placeFus,name=name,page=Pagedbid},
			CPN'ZeroDbId))
	         then 
		   let val FusSetId = 
			Cpndb.crPlFus {ftype=Cpndb.PAGE_FUS,page=Pagedbid};
		   in (
			Cpndb.chngName {name=name,obj=FusSetId};
			Cpndb.addToFusion {newfusion=FusSetId,obj=placeid}) 
		   end 
		 else 
		   (print ("fusion set already exists"^"\n");raise MakeFus)
      end;

fun MakeInstPlaceFus {name:string,pageid:Cpndb.gid,placeid:Cpndb.gid} =
if (#otype (Cpndb.objTypes pageid) <> Cpndb.otApage) 
   then 
      (print ("page must be a cpn page"^"\n");raise MakeFus)
   else 
      let val Pagedbid = (#id (Cpndb.objTypes pageid));
      in
      if (#otype (Cpndb.objTypes placeid) <> Cpndb.otPlace) 
         then 
	    (print ("node must be a place"^"\n");raise MakeFus)
         else 
	    if (Cpndb.equaldbid(Cpndb.getNameFusSet 
			{fstype=Cpndb.placeFus,name=name,page=Pagedbid},
			CPN'ZeroDbId))
	       then 
		  let val FusSetId = 
			Cpndb.crPlFus {ftype=Cpndb.INST_FUS,page=Pagedbid};
		  in (Cpndb.chngName {name=name,obj=FusSetId};
		     Cpndb.addToFusion {newfusion=FusSetId,obj=placeid}) 
		  end 

	       else (print ("fusion set already exists"^"\n");raise MakeFus)
      end;



exception AddToFus;
fun AddToGlobalPlaceFus {name:string,placeid:Cpndb.gid} =
if (#otype (Cpndb.objTypes placeid) <> Cpndb.otPlace) 
   then 
      (print ("node must be a place"^"\n");raise AddToFus)
   else 
      let val FusSetId = Cpndb.getNameFusSet
			{fstype=Cpndb.placeFus,name=name,page=CPN'ZeroDbId}
      in 
	 Cpndb.addToFusion {newfusion=FusSetId,obj=placeid}
      end;

fun AddToPagePlaceFus {name:string,pageid:Cpndb.gid,placeid:Cpndb.gid} =
if (#otype (Cpndb.objTypes pageid) <> Cpndb.otApage) 
then (print ("page must be a cpn page"^"\n");raise AddToFus)
else let val Pagedbid = (#id (Cpndb.objTypes pageid));
in
if (#otype (Cpndb.objTypes placeid) <> Cpndb.otPlace) 
then (print ("node must be a place"^"\n");raise AddToFus)
else let val FusSetId = Cpndb.getNameFusSet {fstype=Cpndb.placeFus,name=name,page=Pagedbid}
in Cpndb.addToFusion {newfusion=FusSetId,obj=placeid}
end end;


fun IsGlobalPlaceFus name = 
if (Cpndb.equaldbid(Cpndb.getNameFusSet 
	{fstype=Cpndb.placeFus,name=name,page=CPN'ZeroDbId},
	CPN'ZeroDbId))
   then true 
   else false;

exception isPagePlaceFus;
fun IsPagePlaceFus {name:string,pageid:Cpndb.gid} =
if (#otype (Cpndb.objTypes pageid) <> Cpndb.otApage)
then (print ("page must be a cpn page"^"\n");raise isPagePlaceFus)
else let val Pagedbid = (#id (Cpndb.objTypes pageid));
in
if (Cpndb.equaldbid(Cpndb.getNameFusSet 
	{fstype=Cpndb.placeFus,name=name,page=Pagedbid},
	CPN'ZeroDbId))
   then true 
   else false
end;



exception isport;
fun IsPort id = 
let val obj = Cpndb.objTypes id;
in
if (#otype obj = Cpndb.otPlace) orelse (#otype obj = Cpndb.otTrans)
then Cpndb.isPort (#id (Cpndb.objTypes id))
else (print ("node must be a place or a transition  id"^"\n");raise isport) end;

exception PageName;
fun GetPageName pageid = 
(if (#otype (Cpndb.objTypes pageid) <> Cpndb.otApage)
then (print ("page must be a cpn page"^"\n");raise PageName)
else Cpndb.getPageName pageid

handle _ => "");


exception getName;
fun GetName id =
let val obj = Cpndb.objTypes id;
in
(if (#otype obj = Cpndb.otPlace) orelse (#otype obj = Cpndb.otTrans)
then MygetCpnRegion {parent=id,rtype=Cpndb.rName}
else (print ("node must be a place"^"\n");raise getName)

handle _ => 0) end;


exception getNameText;
fun GetNameText id =
let val obj = Cpndb.objTypes id;
in
(if (#otype obj = Cpndb.otPlace) orelse (#otype obj = Cpndb.otTrans)
then
let val name = MygetCpnRegion {parent=id,rtype=Cpndb.rName}
in if name = 0 then DSText_Get id else DSText_Get name end
else (print ("node must be a place"^"\n");raise getNameText)

handle _ => "") end;


exception getAux;
fun GetAux id =
let val obj = Cpndb.objTypes id;
in
(if (#otype obj= Cpndb.otPlace) orelse (#otype obj = Cpndb.otTrans)
   orelse (#otype (Cpndb.objTypes id) = Cpndb.otArc) 
then MygetCpnRegion {parent=id,rtype=Cpndb.rAuxiliary}
else (print ("node must be a place a transition or an arc"^"\n");raise getAux)

handle _ => 0) end;


exception getColor;
fun GetColor placeid =
(if (#otype (Cpndb.objTypes placeid) = Cpndb.otPlace) 
then MygetCpnRegion {parent=placeid,rtype=Cpndb.rColorSet}
else (print ("node must be a place"^"\n");raise getColor)

handle _ => 0);


exception getInitMark;
fun GetInitMark placeid =
(if (#otype (Cpndb.objTypes placeid) = Cpndb.otPlace) 
then MygetCpnRegion {parent=placeid,rtype=Cpndb.rInitMark}
else (print ("node must be a place"^"\n");raise getInitMark)

handle _ => 0);


exception getGuard;
fun GetGuard transid =
(if (#otype (Cpndb.objTypes transid) = Cpndb.otTrans) 
then MygetCpnRegion {parent=transid,rtype=Cpndb.rGuard}
else (print ("node must be a transition"^"\n");raise getGuard)

handle _ => 0);


exception getCodeSeg;
fun GetCodeSeg transid =
(
case (#otype (Cpndb.objTypes transid)) of
  Cpndb.otTrans => MygetCpnRegion {	parent=transid,
					rtype=Cpndb.rCodeSeg} |
  Cpndb.otBarChart => MygetCpnRegion {	parent=transid,
					rtype=Cpndb.rBarChartCodesegment} |
  Cpndb.otLineChart => MygetCpnRegion {	parent=transid,
					rtype=Cpndb.rLineChartCodesegment} | 
  _ => (print ("node must be a transition"^"\n");raise getCodeSeg) 

handle _ => (TextIO.output (TextIO.stdOut," Code Seg fail ");0));


exception getTime;
fun GetTime transid =
(if (#otype (Cpndb.objTypes transid) = Cpndb.otTrans) 
then MygetCpnRegion {parent=transid,rtype=Cpndb.rTime}
else (print ("node must be a transition"^"\n");raise getTime)

handle _ => (TextIO.output (TextIO.stdOut," Time fail ");0));


exception getArcExp;
fun GetArcExp arcid =
(if (#otype (Cpndb.objTypes arcid) = Cpndb.otArc) 
then MygetCpnRegion {parent=arcid,rtype=Cpndb.rArcExp}
else (print ("node must be a transition"^"\n");raise getArcExp)

handle _ => 0);


exception getPort;
fun GetPort id = 
let val obj = Cpndb.objTypes id;
in
(if (#otype obj = Cpndb.otPlace) orelse (#otype obj = Cpndb.otTrans)
then MygetCpnRegion {parent=id,rtype=Cpndb.rPort}
else (print ("node must be a place or a transition  id"^"\n");raise getPort)

handle _ => 0) end;


exception getFusion;
fun GetFusion id = 
let val obj = Cpndb.objTypes id;
in
(if (#otype obj = Cpndb.otPlace) orelse (#otype obj = Cpndb.otTrans)
then MygetCpnRegion {parent=id,rtype=Cpndb.rFusion}
else (print ("node must be a place or a transition  id"^"\n");raise getFusion)

handle _ => 0) end;


datatype ObjType = 
	Page | DefBox | Place | Trans | Arc | LineChart | BarChart| Unknown;
datatype ObjSubType = 
	SubPl | SubTr | Simp | Hier | Glob | Temp | Loc | Gen | 
	In | Out | InOut | Sub | GlobFus | PageFus | InstFus;

fun GetCpnInfo id = 
(let val obj = Cpndb.objTypes id;
in 
case (#otype obj) of 
  Cpndb.otApage => 
    ( case (#ostype obj) of 
          Cpndb.ostInPort    => {objtyp = Page , objsubtyp = Simp } | 
          Cpndb.ostOutPort   => {objtyp = Page , objsubtyp = SubTr } | 
          Cpndb.ostInoutPort => {objtyp = Page , objsubtyp = SubPl } |
	  _ => {objtyp = Unknown , objsubtyp = Simp } ) |
  Cpndb.otDefbox => 
    ( case (#ostype obj) of 
          Cpndb.ostInPort    => {objtyp = DefBox , objsubtyp = Glob } | 
          Cpndb.ostOutPort   => {objtyp = DefBox , objsubtyp = Loc } | 
          Cpndb.ostInoutPort => {objtyp = DefBox , objsubtyp = Temp } |
	  _ => {objtyp = Unknown , objsubtyp = Simp } ) |
  Cpndb.otPlace => 
    ( if Cpndb.equaldbid(Cpndb.getFus (#id obj), CPN'ZeroDbId) 
      then case (#ostype obj) of 
          Cpndb.ostSimple    => {objtyp = Place , objsubtyp = Simp } | 
          Cpndb.ostPort   => {objtyp = Place , objsubtyp = Gen } | 
          Cpndb.ostInPort => {objtyp = Place , objsubtyp = In } | 
          Cpndb.ostOutPort => {objtyp = Place , objsubtyp = Out } | 
          Cpndb.ostInoutPort => {objtyp = Place , objsubtyp = InOut } | 
          Cpndb.ostSubst => {objtyp = Place , objsubtyp = Sub } |
	  _ => {objtyp = Unknown , objsubtyp = Simp }
      else case Cpndb.getFustype (Cpndb.getFus (#id obj)) of 
          Cpndb.GLOBAL_FUS => {objtyp = Place , objsubtyp =  GlobFus} |
          Cpndb.PAGE_FUS => {objtyp = Place , objsubtyp =  PageFus} |
          Cpndb.INST_FUS => {objtyp = Place , objsubtyp =  InstFus} ) |
  Cpndb.otTrans => 
    ( case (#ostype obj) of 
          Cpndb.ostSimple    => {objtyp = Trans , objsubtyp = Simp } | 
          Cpndb.ostPort   => {objtyp = Trans , objsubtyp = Gen } | 
          Cpndb.ostInPort => {objtyp = Trans , objsubtyp = In } | 
          Cpndb.ostOutPort => {objtyp = Trans , objsubtyp = Out } | 
          Cpndb.ostInoutPort => {objtyp = Trans , objsubtyp = InOut } | 
          Cpndb.ostSubst => {objtyp = Trans , objsubtyp = Sub } |
	  _ => {objtyp = Unknown , objsubtyp = Simp } ) |
  Cpndb.otArc => 
    ( {objtyp = Arc , objsubtyp = Simp } ) | 
  Cpndb.otBarChart => 
    ( {objtyp = BarChart , objsubtyp = Simp } ) |  
  Cpndb.otLineChart => 
    ( {objtyp = LineChart , objsubtyp = Simp } ) | 
  _ =>  ( {objtyp = Unknown , objsubtyp = Simp } )
end

handle _ => {objtyp = Unknown , objsubtyp = Simp });


exception PageModeAttr;
fun IsPageIncluded pageid
= (if (#otype (Cpndb.objTypes pageid) <> Cpndb.otApage)
then (print ("page must be a cpn page"^"\n");raise PageModeAttr)
else Cpndb.getPageSimParam { mask=Cpndb.INCLUDE_MASK, page=Cpndb.getDbId pageid});

fun IsPageProposed pageid
= (if (#otype (Cpndb.objTypes pageid) <> Cpndb.otApage)
then (print ("page must be a cpn page"^"\n");raise PageModeAttr)
else Cpndb.getPageSimParam { mask=Cpndb.PROPOSE_MASK, page=Cpndb.getDbId pageid});

fun IsPageObserv pageid
= (if (#otype (Cpndb.objTypes pageid) <> Cpndb.otApage)
then (print ("page must be a cpn page"^"\n");raise PageModeAttr)
else Cpndb.getPageSimParam { mask=Cpndb.OBSERVE_MASK, page=Cpndb.getDbId pageid});

fun IsPageCode pageid
= (if (#otype (Cpndb.objTypes pageid) <> Cpndb.otApage)
then (print ("page must be a cpn page"^"\n");raise PageModeAttr)
else Cpndb.getPageSimParam { mask=Cpndb.CODE_MASK, page=Cpndb.getDbId pageid});

fun IsPageAuto pageid
= (if (#otype (Cpndb.objTypes pageid) <> Cpndb.otApage)
then (print ("page must be a cpn page"^"\n");raise PageModeAttr)
else Cpndb.getPageSimParam { mask=Cpndb.AUTO_MASK, page=Cpndb.getDbId pageid});

fun GetPageModeAttr pageid 
= (if (#otype (Cpndb.objTypes pageid) <> Cpndb.otApage)
then (print ("page must be a cpn page"^"\n");raise PageModeAttr)
else { included = IsPageIncluded pageid ,
         proposed = IsPageProposed pageid ,
	observ = IsPageObserv pageid ,
	code = IsPageCode pageid ,
	auto = IsPageAuto pageid 
});

exception SubTransModeAttr;
fun IsSubTransIncluded nodeid
= (if (#otype (Cpndb.objTypes nodeid) <> Cpndb.otTrans)
then (print ("node must be a cpn transition"^"\n");raise SubTransModeAttr)
else Cpndb.getNodeSimParam { mask=Cpndb.INCLUDE_MASK, node=Cpndb.getDbId nodeid});

fun IsSubTransProposed nodeid
= (if (#otype (Cpndb.objTypes nodeid) <> Cpndb.otTrans)
then (print ("node must be a cpn transition"^"\n");raise SubTransModeAttr)
else Cpndb.getNodeSimParam { mask=Cpndb.PROPOSE_MASK, node=Cpndb.getDbId nodeid});

fun IsSubTransObserv nodeid
= (if (#otype (Cpndb.objTypes nodeid) <> Cpndb.otTrans)
then (print ("node must be a cpn transition"^"\n");raise SubTransModeAttr)
else Cpndb.getNodeSimParam { mask=Cpndb.OBSERVE_MASK, node=Cpndb.getDbId nodeid});

fun IsSubTransCode nodeid
= (if (#otype (Cpndb.objTypes nodeid) <> Cpndb.otTrans)
then (print ("node must be a cpn transition"^"\n");raise SubTransModeAttr)
else Cpndb.getNodeSimParam { mask=Cpndb.CODE_MASK, node=Cpndb.getDbId nodeid});

fun IsSubTransAuto nodeid
= (if (#otype (Cpndb.objTypes nodeid) <> Cpndb.otTrans)
then (print ("node must be a cpn transition"^"\n");raise SubTransModeAttr)
else Cpndb.getNodeSimParam { mask=Cpndb.AUTO_MASK, node=Cpndb.getDbId nodeid});

fun GetSubTransModeAttr nodeid 
= (if (#otype (Cpndb.objTypes nodeid ) <> Cpndb.otTrans)
then (print ("node must be a cpn transition"^"\n");raise SubTransModeAttr)
else { included = IsSubTransIncluded nodeid ,
         proposed = IsSubTransProposed nodeid ,
	observ = IsSubTransObserv nodeid ,
	code = IsSubTransCode nodeid ,
	auto = IsSubTransAuto nodeid 
});

exception PrimePage;
fun IsPagePrime pageid
= (if (#otype (Cpndb.objTypes pageid) <> Cpndb.otApage)
then (print ("page must be a cpn page"^"\n");raise PrimePage)
else (0 <> Cpndb.getPrime (Cpndb.getDbId pageid)));

exception getPageMult ;
fun GetPageMult pageid
= (if (#otype (Cpndb.objTypes pageid) <> Cpndb.otApage)
then (print ("page must be a cpn page"^"\n");raise getPageMult )
else Cpndb.getPrime (Cpndb.getDbId pageid));

exception getPageInsts ;
fun GetPageInsts pageid = 
if #otype (Cpndb.objTypes pageid ) = Cpndb.otApage 
then 
Cpndb.getPageInsts (Cpndb.getDbId  pageid)
else (print ("node must be a cpn page"^"\n");raise getPageInsts );

fun GetPageInstName instid = Cpndb.getInstName instid ;

fun GetPageInstComp instid = Cpndb.getGraphicId (Cpndb.getCompNode(Cpndb.getInstComp instid));

fun GetPageInstCompName instid = GetNameText(Cpndb.getGraphicId(Cpndb.getCompNode(Cpndb.getInstComp instid)));

exception getMarkingCode
exception putMarkingCode

fun GetMarkingCode { placeid = pid , instid = iid } =
    if #otype (Cpndb.objTypes pid ) = Cpndb.otPlace 
    then Cpndb.getMarkTokCode{ inst=iid, place= Cpndb.getDbId pid }
    else (print ("node must be a cpn place"^"\n");raise getMarkingCode );

fun GetChangeMarkingCode { placeid = pid,instid = iid , mark= mark} =
  if #otype (Cpndb.objTypes pid ) = Cpndb.otPlace 
  then Cpndb.putMarkTokCode{ inst=iid, place= Cpndb.getDbId pid ,mark = mark}
  else  (print ("node must be a cpn place"^"\n");raise putMarkingCode)

fun MakePrimePage{nodeorpage = nodeorpage, mult = mult} =
	(
	Cpndb.setModeAtt{nodeorpage = nodeorpage, 
			 observe = ~1, 
			 incl = ~1, 
			 propose = ~1,
			 code = ~1,
			 pauto = ~1,
			 overwrite = false,
			 mult = mult};
	Cpndb.updateModeRegion nodeorpage);
