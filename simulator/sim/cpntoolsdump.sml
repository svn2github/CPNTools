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


 structure UNIT = CPN'ColorSets.UnitCS(val CPN'str = NONE);
 type UNIT = UNIT.cs;


 structure INT = CPN'ColorSets.IntCS;
 type INT = INT.cs;

 structure BOOL = CPN'ColorSets.BoolCS (val CPN'arg= NONE);
 type BOOL = BOOL.cs;

 structure STRING = CPN'ColorSets.StringCS;
 type STRING = STRING.cs;
fun CurrentTime()= IntInf.toInt(time());
val Weer =3
(*kan 0,1,2,3,4 of 5 zijn*);
val VerwWeerscen=1
(*kan 0,1,2,3,4 of 5 zijn*);
val TijdTussenAankomstVertrekWaarde=
(1.0/60.0);
val AankomstTijdWaarde=(1.0/3.5);
val GemTijdDeicen = 12;
val TijdNaarGateWaarde = (5,10);
val TijdVragenInfoWaarde =(1,4);
val TijdDoorgevenInfoWaarde = (1,2);
val KansPortoWerktWelWaarde =
(1,95.0/100.0);

 structure VliegtuigID = CPN'ColorSets.IntCS;
 type VliegtuigID = VliegtuigID.cs;

 structure ArrivalTime = CPN'ColorSets.IntCS;
 type ArrivalTime = ArrivalTime.cs;

 structure DepartTime = CPN'ColorSets.IntCS;
 type DepartTime = DepartTime.cs;

 structure Besluit = CPN'ColorSets.IntCS;
 type Besluit = Besluit.cs;

 structure Onderdelen = CPN'ColorSets.IntCS;
 type Onderdelen = Onderdelen.cs;

 structure RecordTime = CPN'ColorSets.IntCS;
 type RecordTime = RecordTime.cs;

 structure LOnderdelen = CPN'ColorSets.ListCS (structure cs= Onderdelen);
 type LOnderdelen = LOnderdelen.cs;

 type Vliegtuig = {vliegID:VliegtuigID,arrtime:ArrivalTime,x:Besluit,deptime:DepartTime,listonderdelen:LOnderdelen,tijdklaar:RecordTime}
 structure Vliegtuig = struct
 type cs = Vliegtuig
 val base = {vliegID=VliegtuigID.base,arrtime=ArrivalTime.base,x=Besluit.base,deptime=DepartTime.base,listonderdelen=LOnderdelen.base,tijdklaar=RecordTime.base}
 fun lt ({vliegID=CPN'vliegID,arrtime=CPN'arrtime,x=CPN'x,deptime=CPN'deptime,listonderdelen=CPN'listonderdelen,tijdklaar=CPN'tijdklaar},{vliegID=CPN'vliegID',arrtime=CPN'arrtime',x=CPN'x',deptime=CPN'deptime',listonderdelen=CPN'listonderdelen',tijdklaar=CPN'tijdklaar'}) = if VliegtuigID.lt(CPN'vliegID,CPN'vliegID') then true
 else if VliegtuigID.lt(CPN'vliegID',CPN'vliegID) then false
 else if ArrivalTime.lt(CPN'arrtime,CPN'arrtime') then true
 else if ArrivalTime.lt(CPN'arrtime',CPN'arrtime) then false
 else if Besluit.lt(CPN'x,CPN'x') then true
 else if Besluit.lt(CPN'x',CPN'x) then false
 else if DepartTime.lt(CPN'deptime,CPN'deptime') then true
 else if DepartTime.lt(CPN'deptime',CPN'deptime) then false
 else if LOnderdelen.lt(CPN'listonderdelen,CPN'listonderdelen') then true
 else if LOnderdelen.lt(CPN'listonderdelen',CPN'listonderdelen) then false
 else if RecordTime.lt(CPN'tijdklaar,CPN'tijdklaar') then true
 else if RecordTime.lt(CPN'tijdklaar',CPN'tijdklaar) then false
 else false
 val cmp:Vliegtuig * Vliegtuig-> order = (CPN'Misc.a_cmp lt)
 fun mkstr {vliegID=CPN'vliegID,arrtime=CPN'arrtime,x=CPN'x,deptime=CPN'deptime,listonderdelen=CPN'listonderdelen,tijdklaar=CPN'tijdklaar} = CPN'concat ["{vliegID=",VliegtuigID.mkstr CPN'vliegID,",arrtime=",ArrivalTime.mkstr CPN'arrtime,",x=",Besluit.mkstr CPN'x,",deptime=",DepartTime.mkstr CPN'deptime,",listonderdelen=",LOnderdelen.mkstr CPN'listonderdelen,",tijdklaar=",RecordTime.mkstr CPN'tijdklaar,"}"]
 val mkstr_ms : cs CPN'MS.ms -> string = CPN'MS.mkstr_ms (mkstr,lt)
 fun legal {vliegID=CPN'vliegID,arrtime=CPN'arrtime,x=CPN'x,deptime=CPN'deptime,listonderdelen=CPN'listonderdelen,tijdklaar=CPN'tijdklaar} = VliegtuigID.legal (CPN'vliegID) andalso ArrivalTime.legal (CPN'arrtime) andalso Besluit.legal (CPN'x) andalso DepartTime.legal (CPN'deptime) andalso LOnderdelen.legal (CPN'listonderdelen) andalso RecordTime.legal (CPN'tijdklaar)
 fun illegal_msg {vliegID=CPN'vliegID,arrtime=CPN'arrtime,x=CPN'x,deptime=CPN'deptime,listonderdelen=CPN'listonderdelen,tijdklaar=CPN'tijdklaar} = 
 if legal {vliegID=CPN'vliegID,arrtime=CPN'arrtime,x=CPN'x,deptime=CPN'deptime,listonderdelen=CPN'listonderdelen,tijdklaar=CPN'tijdklaar} then ""
 else "Illegal component in color: "^(mkstr {vliegID=CPN'vliegID,arrtime=CPN'arrtime,x=CPN'x,deptime=CPN'deptime,listonderdelen=CPN'listonderdelen,tijdklaar=CPN'tijdklaar})
 fun size () = VliegtuigID.size()*ArrivalTime.size()*Besluit.size()*DepartTime.size()*LOnderdelen.size()*RecordTime.size()
 fun mult (CPN'Y1: VliegtuigID CPN'MS.ms,CPN'Y2: ArrivalTime CPN'MS.ms,CPN'Y3: Besluit CPN'MS.ms,CPN'Y4: DepartTime CPN'MS.ms,CPN'Y5: LOnderdelen CPN'MS.ms,CPN'Y6: RecordTime CPN'MS.ms): Vliegtuig CPN'MS.ms = 
let
 fun CPN'mult(CPN'X1 as (CPN'x1)::_,CPN'X2 as (CPN'x2)::_,CPN'X3 as (CPN'x3)::_,CPN'X4 as (CPN'x4)::_,CPN'X5 as (CPN'x5)::_,(CPN'x6)::CPN'xs6) =
 ({vliegID=CPN'x1,arrtime=CPN'x2,x=CPN'x3,deptime=CPN'x4,listonderdelen=CPN'x5,tijdklaar=CPN'x6})::CPN'mult(CPN'X1,CPN'X2,CPN'X3,CPN'X4,CPN'X5,CPN'xs6)
 | CPN'mult(CPN'X1,CPN'X2,CPN'X3,CPN'X4,_::CPN'xs5,nil) = CPN'mult(CPN'X1,CPN'X2,CPN'X3,CPN'X4,CPN'xs5,CPN'Y6)
 | CPN'mult(CPN'X1,CPN'X2,CPN'X3,_::CPN'xs4,nil,_) = CPN'mult(CPN'X1,CPN'X2,CPN'X3,CPN'xs4,CPN'Y5,CPN'Y6)
 | CPN'mult(CPN'X1,CPN'X2,_::CPN'xs3,nil,_,_) = CPN'mult(CPN'X1,CPN'X2,CPN'xs3,CPN'Y4,CPN'Y5,CPN'Y6)
 | CPN'mult(CPN'X1,_::CPN'xs2,nil,_,_,_) = CPN'mult(CPN'X1,CPN'xs2,CPN'Y3,CPN'Y4,CPN'Y5,CPN'Y6)
 | CPN'mult(_::CPN'xs1,nil,_,_,_,_) = CPN'mult(CPN'xs1,CPN'Y2,CPN'Y3,CPN'Y4,CPN'Y5,CPN'Y6)
 | CPN'mult(nil,_,_,_,_,_) = nil
 in CPN'mult(CPN'Y1,CPN'Y2,CPN'Y3,CPN'Y4,CPN'Y5,CPN'Y6) end
 fun input CPN's = let
 val CPN'list = ref (nil: (string*string) list)
 val _ = CPN'StreamIO.skip_white_spaces CPN's
 val _ = case CPN'StreamIO.get_one CPN's of
 (SOME #"{") => ()
 | NONE => raise CPN'StreamIO.IOError("Can not find '{' when reading a record")
 val CPN'label = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#"="])))
 val CPN'value = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#","])))
 val _ = CPN'list::= (CPN'label,CPN'value)
 val CPN'label = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#"="])))
 val CPN'value = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#","])))
 val _ = CPN'list::= (CPN'label,CPN'value)
 val CPN'label = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#"="])))
 val CPN'value = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#","])))
 val _ = CPN'list::= (CPN'label,CPN'value)
 val CPN'label = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#"="])))
 val CPN'value = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#","])))
 val _ = CPN'list::= (CPN'label,CPN'value)
 val CPN'label = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#"="])))
 val CPN'value = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#","])))
 val _ = CPN'list::= (CPN'label,CPN'value)
 val CPN'label = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#"="])))
 val CPN'value = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#"}"])))
 val _ = CPN'list::= (CPN'label,CPN'value)
 in
 {vliegID=VliegtuigID.input(TextIO.openString(CPN'Decl.find("vliegID", !CPN'list))),
 arrtime=ArrivalTime.input(TextIO.openString(CPN'Decl.find("arrtime", !CPN'list))),
 x=Besluit.input(TextIO.openString(CPN'Decl.find("x", !CPN'list))),
 deptime=DepartTime.input(TextIO.openString(CPN'Decl.find("deptime", !CPN'list))),
 listonderdelen=LOnderdelen.input(TextIO.openString(CPN'Decl.find("listonderdelen", !CPN'list))),
 tijdklaar=RecordTime.input(TextIO.openString(CPN'Decl.find("tijdklaar", !CPN'list)))}
 end
 fun output(CPN's,CPN'c) = 
 if legal CPN'c then CPN'IO.output(CPN's,(mkstr CPN'c)^" ")
 else raise CPN'Error (illegal_msg(CPN'c))
 val input_ms : TextIO.instream -> cs CPN'MS.ms = CPN'MS.input_ms false input
 val output_ms : TextIO.outstream * cs CPN'MS.ms -> unit = CPN'MS.output_ms false (output,lt)
 local
 val all_ref = ref(NONE: Vliegtuig CPN'MS.ms option)
 in
 fun all () = case !all_ref of SOME(all') => all'
 | NONE => let
 val all' = mult (VliegtuigID.all(),ArrivalTime.all(),Besluit.all(),DepartTime.all(),LOnderdelen.all(),RecordTime.all())
 in (all_ref:= SOME(all'); all') end
 end
 fun ord {vliegID=CPN'vliegID,arrtime=CPN'arrtime,x=CPN'x,deptime=CPN'deptime,listonderdelen=CPN'listonderdelen,tijdklaar=CPN'tijdklaar} =
RecordTime.ord(CPN'tijdklaar)+RecordTime.size()*LOnderdelen.ord(CPN'listonderdelen)+LOnderdelen.size()*RecordTime.size()*DepartTime.ord(CPN'deptime)+DepartTime.size()*LOnderdelen.size()*RecordTime.size()*Besluit.ord(CPN'x)+Besluit.size()*DepartTime.size()*LOnderdelen.size()*RecordTime.size()*ArrivalTime.ord(CPN'arrtime)+ArrivalTime.size()*Besluit.size()*DepartTime.size()*LOnderdelen.size()*RecordTime.size()*VliegtuigID.ord(CPN'vliegID)
 fun col CPN'i =
 if 0<=CPN'i andalso CPN'i<size() then
 {tijdklaar=RecordTime.col(CPN'i mod RecordTime.size()),
 listonderdelen=LOnderdelen.col((CPN'i div (RecordTime.size())) mod LOnderdelen.size()),
 deptime=DepartTime.col((CPN'i div (LOnderdelen.size()*RecordTime.size())) mod DepartTime.size()),
 x=Besluit.col((CPN'i div (DepartTime.size()*LOnderdelen.size()*RecordTime.size())) mod Besluit.size()),
 arrtime=ArrivalTime.col((CPN'i div (Besluit.size()*DepartTime.size()*LOnderdelen.size()*RecordTime.size())) mod ArrivalTime.size()),
 vliegID=VliegtuigID.col((CPN'i div (ArrivalTime.size()*Besluit.size()*DepartTime.size()*LOnderdelen.size()*RecordTime.size())) mod VliegtuigID.size())}
 else raise CPN'Error CPN'ColorSets.out_of_range
 fun ran () = {vliegID=VliegtuigID.ran(),arrtime=ArrivalTime.ran(),x=Besluit.ran(),deptime=DepartTime.ran(),listonderdelen=LOnderdelen.ran(),tijdklaar=RecordTime.ran()}
 fun set_vliegID {vliegID=CPN'vliegID,arrtime=CPN'arrtime,x=CPN'x,deptime=CPN'deptime,listonderdelen=CPN'listonderdelen,tijdklaar=CPN'tijdklaar} (CPN'new_vliegID:VliegtuigID) = 
 if VliegtuigID.legal CPN'new_vliegID
 then {vliegID=CPN'new_vliegID,arrtime=CPN'arrtime,x=CPN'x,deptime=CPN'deptime,listonderdelen=CPN'listonderdelen,tijdklaar=CPN'tijdklaar} 
 else raise CPN'Error (VliegtuigID.illegal_msg(CPN'new_vliegID))
 fun set_arrtime {vliegID=CPN'vliegID,arrtime=CPN'arrtime,x=CPN'x,deptime=CPN'deptime,listonderdelen=CPN'listonderdelen,tijdklaar=CPN'tijdklaar} (CPN'new_arrtime:ArrivalTime) = 
 if ArrivalTime.legal CPN'new_arrtime
 then {vliegID=CPN'vliegID,arrtime=CPN'new_arrtime,x=CPN'x,deptime=CPN'deptime,listonderdelen=CPN'listonderdelen,tijdklaar=CPN'tijdklaar} 
 else raise CPN'Error (ArrivalTime.illegal_msg(CPN'new_arrtime))
 fun set_x {vliegID=CPN'vliegID,arrtime=CPN'arrtime,x=CPN'x,deptime=CPN'deptime,listonderdelen=CPN'listonderdelen,tijdklaar=CPN'tijdklaar} (CPN'new_x:Besluit) = 
 if Besluit.legal CPN'new_x
 then {vliegID=CPN'vliegID,arrtime=CPN'arrtime,x=CPN'new_x,deptime=CPN'deptime,listonderdelen=CPN'listonderdelen,tijdklaar=CPN'tijdklaar} 
 else raise CPN'Error (Besluit.illegal_msg(CPN'new_x))
 fun set_deptime {vliegID=CPN'vliegID,arrtime=CPN'arrtime,x=CPN'x,deptime=CPN'deptime,listonderdelen=CPN'listonderdelen,tijdklaar=CPN'tijdklaar} (CPN'new_deptime:DepartTime) = 
 if DepartTime.legal CPN'new_deptime
 then {vliegID=CPN'vliegID,arrtime=CPN'arrtime,x=CPN'x,deptime=CPN'new_deptime,listonderdelen=CPN'listonderdelen,tijdklaar=CPN'tijdklaar} 
 else raise CPN'Error (DepartTime.illegal_msg(CPN'new_deptime))
 fun set_listonderdelen {vliegID=CPN'vliegID,arrtime=CPN'arrtime,x=CPN'x,deptime=CPN'deptime,listonderdelen=CPN'listonderdelen,tijdklaar=CPN'tijdklaar} (CPN'new_listonderdelen:LOnderdelen) = 
 if LOnderdelen.legal CPN'new_listonderdelen
 then {vliegID=CPN'vliegID,arrtime=CPN'arrtime,x=CPN'x,deptime=CPN'deptime,listonderdelen=CPN'new_listonderdelen,tijdklaar=CPN'tijdklaar} 
 else raise CPN'Error (LOnderdelen.illegal_msg(CPN'new_listonderdelen))
 fun set_tijdklaar {vliegID=CPN'vliegID,arrtime=CPN'arrtime,x=CPN'x,deptime=CPN'deptime,listonderdelen=CPN'listonderdelen,tijdklaar=CPN'tijdklaar} (CPN'new_tijdklaar:RecordTime) = 
 if RecordTime.legal CPN'new_tijdklaar
 then {vliegID=CPN'vliegID,arrtime=CPN'arrtime,x=CPN'x,deptime=CPN'deptime,listonderdelen=CPN'listonderdelen,tijdklaar=CPN'new_tijdklaar} 
 else raise CPN'Error (RecordTime.illegal_msg(CPN'new_tijdklaar))
 end;

 structure Vliegtuig'timed = CPN'ColorSets.TimedCS (structure cs = Vliegtuig and Time = CPN'Time);
 type Vliegtuig'timed = Vliegtuig'timed.cs;

 structure Snowdesk = CPN'ColorSets.StringCS;
 type Snowdesk = Snowdesk.cs;

 type VliegtuigSnowdesk = {vliegtuig:Vliegtuig,snowdesk:Snowdesk}
 structure VliegtuigSnowdesk = struct
 type cs = VliegtuigSnowdesk
 val base = {vliegtuig=Vliegtuig.base,snowdesk=Snowdesk.base}
 fun lt ({vliegtuig=CPN'vliegtuig,snowdesk=CPN'snowdesk},{vliegtuig=CPN'vliegtuig',snowdesk=CPN'snowdesk'}) = if Vliegtuig.lt(CPN'vliegtuig,CPN'vliegtuig') then true
 else if Vliegtuig.lt(CPN'vliegtuig',CPN'vliegtuig) then false
 else if Snowdesk.lt(CPN'snowdesk,CPN'snowdesk') then true
 else if Snowdesk.lt(CPN'snowdesk',CPN'snowdesk) then false
 else false
 val cmp:VliegtuigSnowdesk * VliegtuigSnowdesk-> order = (CPN'Misc.a_cmp lt)
 fun mkstr {vliegtuig=CPN'vliegtuig,snowdesk=CPN'snowdesk} = CPN'concat ["{vliegtuig=",Vliegtuig.mkstr CPN'vliegtuig,",snowdesk=",Snowdesk.mkstr CPN'snowdesk,"}"]
 val mkstr_ms : cs CPN'MS.ms -> string = CPN'MS.mkstr_ms (mkstr,lt)
 fun legal {vliegtuig=CPN'vliegtuig,snowdesk=CPN'snowdesk} = Vliegtuig.legal (CPN'vliegtuig) andalso Snowdesk.legal (CPN'snowdesk)
 fun illegal_msg {vliegtuig=CPN'vliegtuig,snowdesk=CPN'snowdesk} = 
 if legal {vliegtuig=CPN'vliegtuig,snowdesk=CPN'snowdesk} then ""
 else "Illegal component in color: "^(mkstr {vliegtuig=CPN'vliegtuig,snowdesk=CPN'snowdesk})
 fun size () = Vliegtuig.size()*Snowdesk.size()
 fun mult (CPN'Y1: Vliegtuig CPN'MS.ms,CPN'Y2: Snowdesk CPN'MS.ms): VliegtuigSnowdesk CPN'MS.ms = 
let
 fun CPN'mult(CPN'X1 as (CPN'x1)::_,(CPN'x2)::CPN'xs2) =
 ({vliegtuig=CPN'x1,snowdesk=CPN'x2})::CPN'mult(CPN'X1,CPN'xs2)
 | CPN'mult(_::CPN'xs1,nil) = CPN'mult(CPN'xs1,CPN'Y2)
 | CPN'mult(nil,_) = nil
 in CPN'mult(CPN'Y1,CPN'Y2) end
 fun input CPN's = let
 val CPN'list = ref (nil: (string*string) list)
 val _ = CPN'StreamIO.skip_white_spaces CPN's
 val _ = case CPN'StreamIO.get_one CPN's of
 (SOME #"{") => ()
 | NONE => raise CPN'StreamIO.IOError("Can not find '{' when reading a record")
 val CPN'label = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#"="])))
 val CPN'value = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#","])))
 val _ = CPN'list::= (CPN'label,CPN'value)
 val CPN'label = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#"="])))
 val CPN'value = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#"}"])))
 val _ = CPN'list::= (CPN'label,CPN'value)
 in
 {vliegtuig=Vliegtuig.input(TextIO.openString(CPN'Decl.find("vliegtuig", !CPN'list))),
 snowdesk=Snowdesk.input(TextIO.openString(CPN'Decl.find("snowdesk", !CPN'list)))}
 end
 fun output(CPN's,CPN'c) = 
 if legal CPN'c then CPN'IO.output(CPN's,(mkstr CPN'c)^" ")
 else raise CPN'Error (illegal_msg(CPN'c))
 val input_ms : TextIO.instream -> cs CPN'MS.ms = CPN'MS.input_ms false input
 val output_ms : TextIO.outstream * cs CPN'MS.ms -> unit = CPN'MS.output_ms false (output,lt)
 local
 val all_ref = ref(NONE: VliegtuigSnowdesk CPN'MS.ms option)
 in
 fun all () = case !all_ref of SOME(all') => all'
 | NONE => let
 val all' = mult (Vliegtuig.all(),Snowdesk.all())
 in (all_ref:= SOME(all'); all') end
 end
 fun ord {vliegtuig=CPN'vliegtuig,snowdesk=CPN'snowdesk} =
Snowdesk.ord(CPN'snowdesk)+Snowdesk.size()*Vliegtuig.ord(CPN'vliegtuig)
 fun col CPN'i =
 if 0<=CPN'i andalso CPN'i<size() then
 {snowdesk=Snowdesk.col(CPN'i mod Snowdesk.size()),
 vliegtuig=Vliegtuig.col((CPN'i div (Snowdesk.size())) mod Vliegtuig.size())}
 else raise CPN'Error CPN'ColorSets.out_of_range
 fun ran () = {vliegtuig=Vliegtuig.ran(),snowdesk=Snowdesk.ran()}
 fun set_vliegtuig {vliegtuig=CPN'vliegtuig,snowdesk=CPN'snowdesk} (CPN'new_vliegtuig:Vliegtuig) = 
 if Vliegtuig.legal CPN'new_vliegtuig
 then {vliegtuig=CPN'new_vliegtuig,snowdesk=CPN'snowdesk} 
 else raise CPN'Error (Vliegtuig.illegal_msg(CPN'new_vliegtuig))
 fun set_snowdesk {vliegtuig=CPN'vliegtuig,snowdesk=CPN'snowdesk} (CPN'new_snowdesk:Snowdesk) = 
 if Snowdesk.legal CPN'new_snowdesk
 then {vliegtuig=CPN'vliegtuig,snowdesk=CPN'new_snowdesk} 
 else raise CPN'Error (Snowdesk.illegal_msg(CPN'new_snowdesk))
 end;

 structure VliegtuigSnowdesk'timed = CPN'ColorSets.TimedCS (structure cs = VliegtuigSnowdesk and Time = CPN'Time);
 type VliegtuigSnowdesk'timed = VliegtuigSnowdesk'timed.cs;

 structure LVliegtuig = CPN'ColorSets.ListCS (structure cs= Vliegtuig);
 type LVliegtuig = LVliegtuig.cs;

 structure LVliegtuig'timed = CPN'ColorSets.TimedCS (structure cs = LVliegtuig and Time = CPN'Time);
 type LVliegtuig'timed = LVliegtuig'timed.cs;

 structure DCoordinator = CPN'ColorSets.StringCS;
 type DCoordinator = DCoordinator.cs;

 structure LVL = CPN'ColorSets.StringCS;
 type LVL = LVL.cs;

 type VliegtuigLVL = {vliegtuig:Vliegtuig,lvl:LVL}
 structure VliegtuigLVL = struct
 type cs = VliegtuigLVL
 val base = {vliegtuig=Vliegtuig.base,lvl=LVL.base}
 fun lt ({vliegtuig=CPN'vliegtuig,lvl=CPN'lvl},{vliegtuig=CPN'vliegtuig',lvl=CPN'lvl'}) = if Vliegtuig.lt(CPN'vliegtuig,CPN'vliegtuig') then true
 else if Vliegtuig.lt(CPN'vliegtuig',CPN'vliegtuig) then false
 else if LVL.lt(CPN'lvl,CPN'lvl') then true
 else if LVL.lt(CPN'lvl',CPN'lvl) then false
 else false
 val cmp:VliegtuigLVL * VliegtuigLVL-> order = (CPN'Misc.a_cmp lt)
 fun mkstr {vliegtuig=CPN'vliegtuig,lvl=CPN'lvl} = CPN'concat ["{vliegtuig=",Vliegtuig.mkstr CPN'vliegtuig,",lvl=",LVL.mkstr CPN'lvl,"}"]
 val mkstr_ms : cs CPN'MS.ms -> string = CPN'MS.mkstr_ms (mkstr,lt)
 fun legal {vliegtuig=CPN'vliegtuig,lvl=CPN'lvl} = Vliegtuig.legal (CPN'vliegtuig) andalso LVL.legal (CPN'lvl)
 fun illegal_msg {vliegtuig=CPN'vliegtuig,lvl=CPN'lvl} = 
 if legal {vliegtuig=CPN'vliegtuig,lvl=CPN'lvl} then ""
 else "Illegal component in color: "^(mkstr {vliegtuig=CPN'vliegtuig,lvl=CPN'lvl})
 fun size () = Vliegtuig.size()*LVL.size()
 fun mult (CPN'Y1: Vliegtuig CPN'MS.ms,CPN'Y2: LVL CPN'MS.ms): VliegtuigLVL CPN'MS.ms = 
let
 fun CPN'mult(CPN'X1 as (CPN'x1)::_,(CPN'x2)::CPN'xs2) =
 ({vliegtuig=CPN'x1,lvl=CPN'x2})::CPN'mult(CPN'X1,CPN'xs2)
 | CPN'mult(_::CPN'xs1,nil) = CPN'mult(CPN'xs1,CPN'Y2)
 | CPN'mult(nil,_) = nil
 in CPN'mult(CPN'Y1,CPN'Y2) end
 fun input CPN's = let
 val CPN'list = ref (nil: (string*string) list)
 val _ = CPN'StreamIO.skip_white_spaces CPN's
 val _ = case CPN'StreamIO.get_one CPN's of
 (SOME #"{") => ()
 | NONE => raise CPN'StreamIO.IOError("Can not find '{' when reading a record")
 val CPN'label = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#"="])))
 val CPN'value = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#","])))
 val _ = CPN'list::= (CPN'label,CPN'value)
 val CPN'label = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#"="])))
 val CPN'value = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#"}"])))
 val _ = CPN'list::= (CPN'label,CPN'value)
 in
 {vliegtuig=Vliegtuig.input(TextIO.openString(CPN'Decl.find("vliegtuig", !CPN'list))),
 lvl=LVL.input(TextIO.openString(CPN'Decl.find("lvl", !CPN'list)))}
 end
 fun output(CPN's,CPN'c) = 
 if legal CPN'c then CPN'IO.output(CPN's,(mkstr CPN'c)^" ")
 else raise CPN'Error (illegal_msg(CPN'c))
 val input_ms : TextIO.instream -> cs CPN'MS.ms = CPN'MS.input_ms false input
 val output_ms : TextIO.outstream * cs CPN'MS.ms -> unit = CPN'MS.output_ms false (output,lt)
 local
 val all_ref = ref(NONE: VliegtuigLVL CPN'MS.ms option)
 in
 fun all () = case !all_ref of SOME(all') => all'
 | NONE => let
 val all' = mult (Vliegtuig.all(),LVL.all())
 in (all_ref:= SOME(all'); all') end
 end
 fun ord {vliegtuig=CPN'vliegtuig,lvl=CPN'lvl} =
LVL.ord(CPN'lvl)+LVL.size()*Vliegtuig.ord(CPN'vliegtuig)
 fun col CPN'i =
 if 0<=CPN'i andalso CPN'i<size() then
 {lvl=LVL.col(CPN'i mod LVL.size()),
 vliegtuig=Vliegtuig.col((CPN'i div (LVL.size())) mod Vliegtuig.size())}
 else raise CPN'Error CPN'ColorSets.out_of_range
 fun ran () = {vliegtuig=Vliegtuig.ran(),lvl=LVL.ran()}
 fun set_vliegtuig {vliegtuig=CPN'vliegtuig,lvl=CPN'lvl} (CPN'new_vliegtuig:Vliegtuig) = 
 if Vliegtuig.legal CPN'new_vliegtuig
 then {vliegtuig=CPN'new_vliegtuig,lvl=CPN'lvl} 
 else raise CPN'Error (Vliegtuig.illegal_msg(CPN'new_vliegtuig))
 fun set_lvl {vliegtuig=CPN'vliegtuig,lvl=CPN'lvl} (CPN'new_lvl:LVL) = 
 if LVL.legal CPN'new_lvl
 then {vliegtuig=CPN'vliegtuig,lvl=CPN'new_lvl} 
 else raise CPN'Error (LVL.illegal_msg(CPN'new_lvl))
 end;

 structure VliegtuigLVL'timed = CPN'ColorSets.TimedCS (structure cs = VliegtuigLVL and Time = CPN'Time);
 type VliegtuigLVL'timed = VliegtuigLVL'timed.cs;

 type VliegtuigDCoordinator = {vliegtuig:Vliegtuig,dcoordinator:DCoordinator}
 structure VliegtuigDCoordinator = struct
 type cs = VliegtuigDCoordinator
 val base = {vliegtuig=Vliegtuig.base,dcoordinator=DCoordinator.base}
 fun lt ({vliegtuig=CPN'vliegtuig,dcoordinator=CPN'dcoordinator},{vliegtuig=CPN'vliegtuig',dcoordinator=CPN'dcoordinator'}) = if Vliegtuig.lt(CPN'vliegtuig,CPN'vliegtuig') then true
 else if Vliegtuig.lt(CPN'vliegtuig',CPN'vliegtuig) then false
 else if DCoordinator.lt(CPN'dcoordinator,CPN'dcoordinator') then true
 else if DCoordinator.lt(CPN'dcoordinator',CPN'dcoordinator) then false
 else false
 val cmp:VliegtuigDCoordinator * VliegtuigDCoordinator-> order = (CPN'Misc.a_cmp lt)
 fun mkstr {vliegtuig=CPN'vliegtuig,dcoordinator=CPN'dcoordinator} = CPN'concat ["{vliegtuig=",Vliegtuig.mkstr CPN'vliegtuig,",dcoordinator=",DCoordinator.mkstr CPN'dcoordinator,"}"]
 val mkstr_ms : cs CPN'MS.ms -> string = CPN'MS.mkstr_ms (mkstr,lt)
 fun legal {vliegtuig=CPN'vliegtuig,dcoordinator=CPN'dcoordinator} = Vliegtuig.legal (CPN'vliegtuig) andalso DCoordinator.legal (CPN'dcoordinator)
 fun illegal_msg {vliegtuig=CPN'vliegtuig,dcoordinator=CPN'dcoordinator} = 
 if legal {vliegtuig=CPN'vliegtuig,dcoordinator=CPN'dcoordinator} then ""
 else "Illegal component in color: "^(mkstr {vliegtuig=CPN'vliegtuig,dcoordinator=CPN'dcoordinator})
 fun size () = Vliegtuig.size()*DCoordinator.size()
 fun mult (CPN'Y1: Vliegtuig CPN'MS.ms,CPN'Y2: DCoordinator CPN'MS.ms): VliegtuigDCoordinator CPN'MS.ms = 
let
 fun CPN'mult(CPN'X1 as (CPN'x1)::_,(CPN'x2)::CPN'xs2) =
 ({vliegtuig=CPN'x1,dcoordinator=CPN'x2})::CPN'mult(CPN'X1,CPN'xs2)
 | CPN'mult(_::CPN'xs1,nil) = CPN'mult(CPN'xs1,CPN'Y2)
 | CPN'mult(nil,_) = nil
 in CPN'mult(CPN'Y1,CPN'Y2) end
 fun input CPN's = let
 val CPN'list = ref (nil: (string*string) list)
 val _ = CPN'StreamIO.skip_white_spaces CPN's
 val _ = case CPN'StreamIO.get_one CPN's of
 (SOME #"{") => ()
 | NONE => raise CPN'StreamIO.IOError("Can not find '{' when reading a record")
 val CPN'label = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#"="])))
 val CPN'value = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#","])))
 val _ = CPN'list::= (CPN'label,CPN'value)
 val CPN'label = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#"="])))
 val CPN'value = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#"}"])))
 val _ = CPN'list::= (CPN'label,CPN'value)
 in
 {vliegtuig=Vliegtuig.input(TextIO.openString(CPN'Decl.find("vliegtuig", !CPN'list))),
 dcoordinator=DCoordinator.input(TextIO.openString(CPN'Decl.find("dcoordinator", !CPN'list)))}
 end
 fun output(CPN's,CPN'c) = 
 if legal CPN'c then CPN'IO.output(CPN's,(mkstr CPN'c)^" ")
 else raise CPN'Error (illegal_msg(CPN'c))
 val input_ms : TextIO.instream -> cs CPN'MS.ms = CPN'MS.input_ms false input
 val output_ms : TextIO.outstream * cs CPN'MS.ms -> unit = CPN'MS.output_ms false (output,lt)
 local
 val all_ref = ref(NONE: VliegtuigDCoordinator CPN'MS.ms option)
 in
 fun all () = case !all_ref of SOME(all') => all'
 | NONE => let
 val all' = mult (Vliegtuig.all(),DCoordinator.all())
 in (all_ref:= SOME(all'); all') end
 end
 fun ord {vliegtuig=CPN'vliegtuig,dcoordinator=CPN'dcoordinator} =
DCoordinator.ord(CPN'dcoordinator)+DCoordinator.size()*Vliegtuig.ord(CPN'vliegtuig)
 fun col CPN'i =
 if 0<=CPN'i andalso CPN'i<size() then
 {dcoordinator=DCoordinator.col(CPN'i mod DCoordinator.size()),
 vliegtuig=Vliegtuig.col((CPN'i div (DCoordinator.size())) mod Vliegtuig.size())}
 else raise CPN'Error CPN'ColorSets.out_of_range
 fun ran () = {vliegtuig=Vliegtuig.ran(),dcoordinator=DCoordinator.ran()}
 fun set_vliegtuig {vliegtuig=CPN'vliegtuig,dcoordinator=CPN'dcoordinator} (CPN'new_vliegtuig:Vliegtuig) = 
 if Vliegtuig.legal CPN'new_vliegtuig
 then {vliegtuig=CPN'new_vliegtuig,dcoordinator=CPN'dcoordinator} 
 else raise CPN'Error (Vliegtuig.illegal_msg(CPN'new_vliegtuig))
 fun set_dcoordinator {vliegtuig=CPN'vliegtuig,dcoordinator=CPN'dcoordinator} (CPN'new_dcoordinator:DCoordinator) = 
 if DCoordinator.legal CPN'new_dcoordinator
 then {vliegtuig=CPN'vliegtuig,dcoordinator=CPN'new_dcoordinator} 
 else raise CPN'Error (DCoordinator.illegal_msg(CPN'new_dcoordinator))
 end;

 structure VliegtuigDCoordinator'timed = CPN'ColorSets.TimedCS (structure cs = VliegtuigDCoordinator and Time = CPN'Time);
 type VliegtuigDCoordinator'timed = VliegtuigDCoordinator'timed.cs;

 structure Operator = CPN'ColorSets.IntCS;
 type Operator = Operator.cs;

 type VliegtuigOperator = {vliegtuig:Vliegtuig,operator:Operator}
 structure VliegtuigOperator = struct
 type cs = VliegtuigOperator
 val base = {vliegtuig=Vliegtuig.base,operator=Operator.base}
 fun lt ({vliegtuig=CPN'vliegtuig,operator=CPN'operator},{vliegtuig=CPN'vliegtuig',operator=CPN'operator'}) = if Vliegtuig.lt(CPN'vliegtuig,CPN'vliegtuig') then true
 else if Vliegtuig.lt(CPN'vliegtuig',CPN'vliegtuig) then false
 else if Operator.lt(CPN'operator,CPN'operator') then true
 else if Operator.lt(CPN'operator',CPN'operator) then false
 else false
 val cmp:VliegtuigOperator * VliegtuigOperator-> order = (CPN'Misc.a_cmp lt)
 fun mkstr {vliegtuig=CPN'vliegtuig,operator=CPN'operator} = CPN'concat ["{vliegtuig=",Vliegtuig.mkstr CPN'vliegtuig,",operator=",Operator.mkstr CPN'operator,"}"]
 val mkstr_ms : cs CPN'MS.ms -> string = CPN'MS.mkstr_ms (mkstr,lt)
 fun legal {vliegtuig=CPN'vliegtuig,operator=CPN'operator} = Vliegtuig.legal (CPN'vliegtuig) andalso Operator.legal (CPN'operator)
 fun illegal_msg {vliegtuig=CPN'vliegtuig,operator=CPN'operator} = 
 if legal {vliegtuig=CPN'vliegtuig,operator=CPN'operator} then ""
 else "Illegal component in color: "^(mkstr {vliegtuig=CPN'vliegtuig,operator=CPN'operator})
 fun size () = Vliegtuig.size()*Operator.size()
 fun mult (CPN'Y1: Vliegtuig CPN'MS.ms,CPN'Y2: Operator CPN'MS.ms): VliegtuigOperator CPN'MS.ms = 
let
 fun CPN'mult(CPN'X1 as (CPN'x1)::_,(CPN'x2)::CPN'xs2) =
 ({vliegtuig=CPN'x1,operator=CPN'x2})::CPN'mult(CPN'X1,CPN'xs2)
 | CPN'mult(_::CPN'xs1,nil) = CPN'mult(CPN'xs1,CPN'Y2)
 | CPN'mult(nil,_) = nil
 in CPN'mult(CPN'Y1,CPN'Y2) end
 fun input CPN's = let
 val CPN'list = ref (nil: (string*string) list)
 val _ = CPN'StreamIO.skip_white_spaces CPN's
 val _ = case CPN'StreamIO.get_one CPN's of
 (SOME #"{") => ()
 | NONE => raise CPN'StreamIO.IOError("Can not find '{' when reading a record")
 val CPN'label = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#"="])))
 val CPN'value = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#","])))
 val _ = CPN'list::= (CPN'label,CPN'value)
 val CPN'label = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#"="])))
 val CPN'value = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#"}"])))
 val _ = CPN'list::= (CPN'label,CPN'value)
 in
 {vliegtuig=Vliegtuig.input(TextIO.openString(CPN'Decl.find("vliegtuig", !CPN'list))),
 operator=Operator.input(TextIO.openString(CPN'Decl.find("operator", !CPN'list)))}
 end
 fun output(CPN's,CPN'c) = 
 if legal CPN'c then CPN'IO.output(CPN's,(mkstr CPN'c)^" ")
 else raise CPN'Error (illegal_msg(CPN'c))
 val input_ms : TextIO.instream -> cs CPN'MS.ms = CPN'MS.input_ms false input
 val output_ms : TextIO.outstream * cs CPN'MS.ms -> unit = CPN'MS.output_ms false (output,lt)
 local
 val all_ref = ref(NONE: VliegtuigOperator CPN'MS.ms option)
 in
 fun all () = case !all_ref of SOME(all') => all'
 | NONE => let
 val all' = mult (Vliegtuig.all(),Operator.all())
 in (all_ref:= SOME(all'); all') end
 end
 fun ord {vliegtuig=CPN'vliegtuig,operator=CPN'operator} =
Operator.ord(CPN'operator)+Operator.size()*Vliegtuig.ord(CPN'vliegtuig)
 fun col CPN'i =
 if 0<=CPN'i andalso CPN'i<size() then
 {operator=Operator.col(CPN'i mod Operator.size()),
 vliegtuig=Vliegtuig.col((CPN'i div (Operator.size())) mod Vliegtuig.size())}
 else raise CPN'Error CPN'ColorSets.out_of_range
 fun ran () = {vliegtuig=Vliegtuig.ran(),operator=Operator.ran()}
 fun set_vliegtuig {vliegtuig=CPN'vliegtuig,operator=CPN'operator} (CPN'new_vliegtuig:Vliegtuig) = 
 if Vliegtuig.legal CPN'new_vliegtuig
 then {vliegtuig=CPN'new_vliegtuig,operator=CPN'operator} 
 else raise CPN'Error (Vliegtuig.illegal_msg(CPN'new_vliegtuig))
 fun set_operator {vliegtuig=CPN'vliegtuig,operator=CPN'operator} (CPN'new_operator:Operator) = 
 if Operator.legal CPN'new_operator
 then {vliegtuig=CPN'vliegtuig,operator=CPN'new_operator} 
 else raise CPN'Error (Operator.illegal_msg(CPN'new_operator))
 end;

 structure VliegtuigOperator'timed = CPN'ColorSets.TimedCS (structure cs = VliegtuigOperator and Time = CPN'Time);
 type VliegtuigOperator'timed = VliegtuigOperator'timed.cs;

 structure Controleur = CPN'ColorSets.IntCS;
 type Controleur = Controleur.cs;

 type Controle = {id:INT,l:LOnderdelen}
 structure Controle = struct
 type cs = Controle
 val base = {id=INT.base,l=LOnderdelen.base}
 fun lt ({id=CPN'id,l=CPN'l},{id=CPN'id',l=CPN'l'}) = if INT.lt(CPN'id,CPN'id') then true
 else if INT.lt(CPN'id',CPN'id) then false
 else if LOnderdelen.lt(CPN'l,CPN'l') then true
 else if LOnderdelen.lt(CPN'l',CPN'l) then false
 else false
 val cmp:Controle * Controle-> order = (CPN'Misc.a_cmp lt)
 fun mkstr {id=CPN'id,l=CPN'l} = CPN'concat ["{id=",INT.mkstr CPN'id,",l=",LOnderdelen.mkstr CPN'l,"}"]
 val mkstr_ms : cs CPN'MS.ms -> string = CPN'MS.mkstr_ms (mkstr,lt)
 fun legal {id=CPN'id,l=CPN'l} = INT.legal (CPN'id) andalso LOnderdelen.legal (CPN'l)
 fun illegal_msg {id=CPN'id,l=CPN'l} = 
 if legal {id=CPN'id,l=CPN'l} then ""
 else "Illegal component in color: "^(mkstr {id=CPN'id,l=CPN'l})
 fun size () = INT.size()*LOnderdelen.size()
 fun mult (CPN'Y1: INT CPN'MS.ms,CPN'Y2: LOnderdelen CPN'MS.ms): Controle CPN'MS.ms = 
let
 fun CPN'mult(CPN'X1 as (CPN'x1)::_,(CPN'x2)::CPN'xs2) =
 ({id=CPN'x1,l=CPN'x2})::CPN'mult(CPN'X1,CPN'xs2)
 | CPN'mult(_::CPN'xs1,nil) = CPN'mult(CPN'xs1,CPN'Y2)
 | CPN'mult(nil,_) = nil
 in CPN'mult(CPN'Y1,CPN'Y2) end
 fun input CPN's = let
 val CPN'list = ref (nil: (string*string) list)
 val _ = CPN'StreamIO.skip_white_spaces CPN's
 val _ = case CPN'StreamIO.get_one CPN's of
 (SOME #"{") => ()
 | NONE => raise CPN'StreamIO.IOError("Can not find '{' when reading a record")
 val CPN'label = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#"="])))
 val CPN'value = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#","])))
 val _ = CPN'list::= (CPN'label,CPN'value)
 val CPN'label = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#"="])))
 val CPN'value = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#"}"])))
 val _ = CPN'list::= (CPN'label,CPN'value)
 in
 {id=INT.input(TextIO.openString(CPN'Decl.find("id", !CPN'list))),
 l=LOnderdelen.input(TextIO.openString(CPN'Decl.find("l", !CPN'list)))}
 end
 fun output(CPN's,CPN'c) = 
 if legal CPN'c then CPN'IO.output(CPN's,(mkstr CPN'c)^" ")
 else raise CPN'Error (illegal_msg(CPN'c))
 val input_ms : TextIO.instream -> cs CPN'MS.ms = CPN'MS.input_ms false input
 val output_ms : TextIO.outstream * cs CPN'MS.ms -> unit = CPN'MS.output_ms false (output,lt)
 local
 val all_ref = ref(NONE: Controle CPN'MS.ms option)
 in
 fun all () = case !all_ref of SOME(all') => all'
 | NONE => let
 val all' = mult (INT.all(),LOnderdelen.all())
 in (all_ref:= SOME(all'); all') end
 end
 fun ord {id=CPN'id,l=CPN'l} =
LOnderdelen.ord(CPN'l)+LOnderdelen.size()*INT.ord(CPN'id)
 fun col CPN'i =
 if 0<=CPN'i andalso CPN'i<size() then
 {l=LOnderdelen.col(CPN'i mod LOnderdelen.size()),
 id=INT.col((CPN'i div (LOnderdelen.size())) mod INT.size())}
 else raise CPN'Error CPN'ColorSets.out_of_range
 fun ran () = {id=INT.ran(),l=LOnderdelen.ran()}
 fun set_id {id=CPN'id,l=CPN'l} (CPN'new_id:INT) = 
 if INT.legal CPN'new_id
 then {id=CPN'new_id,l=CPN'l} 
 else raise CPN'Error (INT.illegal_msg(CPN'new_id))
 fun set_l {id=CPN'id,l=CPN'l} (CPN'new_l:LOnderdelen) = 
 if LOnderdelen.legal CPN'new_l
 then {id=CPN'id,l=CPN'new_l} 
 else raise CPN'Error (LOnderdelen.illegal_msg(CPN'new_l))
 end;

 type VliegtuigControleur = {vliegtuig:Vliegtuig,controleur:Controleur}
 structure VliegtuigControleur = struct
 type cs = VliegtuigControleur
 val base = {vliegtuig=Vliegtuig.base,controleur=Controleur.base}
 fun lt ({vliegtuig=CPN'vliegtuig,controleur=CPN'controleur},{vliegtuig=CPN'vliegtuig',controleur=CPN'controleur'}) = if Vliegtuig.lt(CPN'vliegtuig,CPN'vliegtuig') then true
 else if Vliegtuig.lt(CPN'vliegtuig',CPN'vliegtuig) then false
 else if Controleur.lt(CPN'controleur,CPN'controleur') then true
 else if Controleur.lt(CPN'controleur',CPN'controleur) then false
 else false
 val cmp:VliegtuigControleur * VliegtuigControleur-> order = (CPN'Misc.a_cmp lt)
 fun mkstr {vliegtuig=CPN'vliegtuig,controleur=CPN'controleur} = CPN'concat ["{vliegtuig=",Vliegtuig.mkstr CPN'vliegtuig,",controleur=",Controleur.mkstr CPN'controleur,"}"]
 val mkstr_ms : cs CPN'MS.ms -> string = CPN'MS.mkstr_ms (mkstr,lt)
 fun legal {vliegtuig=CPN'vliegtuig,controleur=CPN'controleur} = Vliegtuig.legal (CPN'vliegtuig) andalso Controleur.legal (CPN'controleur)
 fun illegal_msg {vliegtuig=CPN'vliegtuig,controleur=CPN'controleur} = 
 if legal {vliegtuig=CPN'vliegtuig,controleur=CPN'controleur} then ""
 else "Illegal component in color: "^(mkstr {vliegtuig=CPN'vliegtuig,controleur=CPN'controleur})
 fun size () = Vliegtuig.size()*Controleur.size()
 fun mult (CPN'Y1: Vliegtuig CPN'MS.ms,CPN'Y2: Controleur CPN'MS.ms): VliegtuigControleur CPN'MS.ms = 
let
 fun CPN'mult(CPN'X1 as (CPN'x1)::_,(CPN'x2)::CPN'xs2) =
 ({vliegtuig=CPN'x1,controleur=CPN'x2})::CPN'mult(CPN'X1,CPN'xs2)
 | CPN'mult(_::CPN'xs1,nil) = CPN'mult(CPN'xs1,CPN'Y2)
 | CPN'mult(nil,_) = nil
 in CPN'mult(CPN'Y1,CPN'Y2) end
 fun input CPN's = let
 val CPN'list = ref (nil: (string*string) list)
 val _ = CPN'StreamIO.skip_white_spaces CPN's
 val _ = case CPN'StreamIO.get_one CPN's of
 (SOME #"{") => ()
 | NONE => raise CPN'StreamIO.IOError("Can not find '{' when reading a record")
 val CPN'label = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#"="])))
 val CPN'value = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#","])))
 val _ = CPN'list::= (CPN'label,CPN'value)
 val CPN'label = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#"="])))
 val CPN'value = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#"}"])))
 val _ = CPN'list::= (CPN'label,CPN'value)
 in
 {vliegtuig=Vliegtuig.input(TextIO.openString(CPN'Decl.find("vliegtuig", !CPN'list))),
 controleur=Controleur.input(TextIO.openString(CPN'Decl.find("controleur", !CPN'list)))}
 end
 fun output(CPN's,CPN'c) = 
 if legal CPN'c then CPN'IO.output(CPN's,(mkstr CPN'c)^" ")
 else raise CPN'Error (illegal_msg(CPN'c))
 val input_ms : TextIO.instream -> cs CPN'MS.ms = CPN'MS.input_ms false input
 val output_ms : TextIO.outstream * cs CPN'MS.ms -> unit = CPN'MS.output_ms false (output,lt)
 local
 val all_ref = ref(NONE: VliegtuigControleur CPN'MS.ms option)
 in
 fun all () = case !all_ref of SOME(all') => all'
 | NONE => let
 val all' = mult (Vliegtuig.all(),Controleur.all())
 in (all_ref:= SOME(all'); all') end
 end
 fun ord {vliegtuig=CPN'vliegtuig,controleur=CPN'controleur} =
Controleur.ord(CPN'controleur)+Controleur.size()*Vliegtuig.ord(CPN'vliegtuig)
 fun col CPN'i =
 if 0<=CPN'i andalso CPN'i<size() then
 {controleur=Controleur.col(CPN'i mod Controleur.size()),
 vliegtuig=Vliegtuig.col((CPN'i div (Controleur.size())) mod Vliegtuig.size())}
 else raise CPN'Error CPN'ColorSets.out_of_range
 fun ran () = {vliegtuig=Vliegtuig.ran(),controleur=Controleur.ran()}
 fun set_vliegtuig {vliegtuig=CPN'vliegtuig,controleur=CPN'controleur} (CPN'new_vliegtuig:Vliegtuig) = 
 if Vliegtuig.legal CPN'new_vliegtuig
 then {vliegtuig=CPN'new_vliegtuig,controleur=CPN'controleur} 
 else raise CPN'Error (Vliegtuig.illegal_msg(CPN'new_vliegtuig))
 fun set_controleur {vliegtuig=CPN'vliegtuig,controleur=CPN'controleur} (CPN'new_controleur:Controleur) = 
 if Controleur.legal CPN'new_controleur
 then {vliegtuig=CPN'vliegtuig,controleur=CPN'new_controleur} 
 else raise CPN'Error (Controleur.illegal_msg(CPN'new_controleur))
 end;

 structure VliegtuigControleur'timed = CPN'ColorSets.TimedCS (structure cs = VliegtuigControleur and Time = CPN'Time);
 type VliegtuigControleur'timed = VliegtuigControleur'timed.cs;

 type VliegtuigOperatorControleur = {vliegtuig:Vliegtuig,operator:Operator,controleur:Controleur}
 structure VliegtuigOperatorControleur = struct
 type cs = VliegtuigOperatorControleur
 val base = {vliegtuig=Vliegtuig.base,operator=Operator.base,controleur=Controleur.base}
 fun lt ({vliegtuig=CPN'vliegtuig,operator=CPN'operator,controleur=CPN'controleur},{vliegtuig=CPN'vliegtuig',operator=CPN'operator',controleur=CPN'controleur'}) = if Vliegtuig.lt(CPN'vliegtuig,CPN'vliegtuig') then true
 else if Vliegtuig.lt(CPN'vliegtuig',CPN'vliegtuig) then false
 else if Operator.lt(CPN'operator,CPN'operator') then true
 else if Operator.lt(CPN'operator',CPN'operator) then false
 else if Controleur.lt(CPN'controleur,CPN'controleur') then true
 else if Controleur.lt(CPN'controleur',CPN'controleur) then false
 else false
 val cmp:VliegtuigOperatorControleur * VliegtuigOperatorControleur-> order = (CPN'Misc.a_cmp lt)
 fun mkstr {vliegtuig=CPN'vliegtuig,operator=CPN'operator,controleur=CPN'controleur} = CPN'concat ["{vliegtuig=",Vliegtuig.mkstr CPN'vliegtuig,",operator=",Operator.mkstr CPN'operator,",controleur=",Controleur.mkstr CPN'controleur,"}"]
 val mkstr_ms : cs CPN'MS.ms -> string = CPN'MS.mkstr_ms (mkstr,lt)
 fun legal {vliegtuig=CPN'vliegtuig,operator=CPN'operator,controleur=CPN'controleur} = Vliegtuig.legal (CPN'vliegtuig) andalso Operator.legal (CPN'operator) andalso Controleur.legal (CPN'controleur)
 fun illegal_msg {vliegtuig=CPN'vliegtuig,operator=CPN'operator,controleur=CPN'controleur} = 
 if legal {vliegtuig=CPN'vliegtuig,operator=CPN'operator,controleur=CPN'controleur} then ""
 else "Illegal component in color: "^(mkstr {vliegtuig=CPN'vliegtuig,operator=CPN'operator,controleur=CPN'controleur})
 fun size () = Vliegtuig.size()*Operator.size()*Controleur.size()
 fun mult (CPN'Y1: Vliegtuig CPN'MS.ms,CPN'Y2: Operator CPN'MS.ms,CPN'Y3: Controleur CPN'MS.ms): VliegtuigOperatorControleur CPN'MS.ms = 
let
 fun CPN'mult(CPN'X1 as (CPN'x1)::_,CPN'X2 as (CPN'x2)::_,(CPN'x3)::CPN'xs3) =
 ({vliegtuig=CPN'x1,operator=CPN'x2,controleur=CPN'x3})::CPN'mult(CPN'X1,CPN'X2,CPN'xs3)
 | CPN'mult(CPN'X1,_::CPN'xs2,nil) = CPN'mult(CPN'X1,CPN'xs2,CPN'Y3)
 | CPN'mult(_::CPN'xs1,nil,_) = CPN'mult(CPN'xs1,CPN'Y2,CPN'Y3)
 | CPN'mult(nil,_,_) = nil
 in CPN'mult(CPN'Y1,CPN'Y2,CPN'Y3) end
 fun input CPN's = let
 val CPN'list = ref (nil: (string*string) list)
 val _ = CPN'StreamIO.skip_white_spaces CPN's
 val _ = case CPN'StreamIO.get_one CPN's of
 (SOME #"{") => ()
 | NONE => raise CPN'StreamIO.IOError("Can not find '{' when reading a record")
 val CPN'label = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#"="])))
 val CPN'value = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#","])))
 val _ = CPN'list::= (CPN'label,CPN'value)
 val CPN'label = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#"="])))
 val CPN'value = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#","])))
 val _ = CPN'list::= (CPN'label,CPN'value)
 val CPN'label = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#"="])))
 val CPN'value = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#"}"])))
 val _ = CPN'list::= (CPN'label,CPN'value)
 in
 {vliegtuig=Vliegtuig.input(TextIO.openString(CPN'Decl.find("vliegtuig", !CPN'list))),
 operator=Operator.input(TextIO.openString(CPN'Decl.find("operator", !CPN'list))),
 controleur=Controleur.input(TextIO.openString(CPN'Decl.find("controleur", !CPN'list)))}
 end
 fun output(CPN's,CPN'c) = 
 if legal CPN'c then CPN'IO.output(CPN's,(mkstr CPN'c)^" ")
 else raise CPN'Error (illegal_msg(CPN'c))
 val input_ms : TextIO.instream -> cs CPN'MS.ms = CPN'MS.input_ms false input
 val output_ms : TextIO.outstream * cs CPN'MS.ms -> unit = CPN'MS.output_ms false (output,lt)
 local
 val all_ref = ref(NONE: VliegtuigOperatorControleur CPN'MS.ms option)
 in
 fun all () = case !all_ref of SOME(all') => all'
 | NONE => let
 val all' = mult (Vliegtuig.all(),Operator.all(),Controleur.all())
 in (all_ref:= SOME(all'); all') end
 end
 fun ord {vliegtuig=CPN'vliegtuig,operator=CPN'operator,controleur=CPN'controleur} =
Controleur.ord(CPN'controleur)+Controleur.size()*Operator.ord(CPN'operator)+Operator.size()*Controleur.size()*Vliegtuig.ord(CPN'vliegtuig)
 fun col CPN'i =
 if 0<=CPN'i andalso CPN'i<size() then
 {controleur=Controleur.col(CPN'i mod Controleur.size()),
 operator=Operator.col((CPN'i div (Controleur.size())) mod Operator.size()),
 vliegtuig=Vliegtuig.col((CPN'i div (Operator.size()*Controleur.size())) mod Vliegtuig.size())}
 else raise CPN'Error CPN'ColorSets.out_of_range
 fun ran () = {vliegtuig=Vliegtuig.ran(),operator=Operator.ran(),controleur=Controleur.ran()}
 fun set_vliegtuig {vliegtuig=CPN'vliegtuig,operator=CPN'operator,controleur=CPN'controleur} (CPN'new_vliegtuig:Vliegtuig) = 
 if Vliegtuig.legal CPN'new_vliegtuig
 then {vliegtuig=CPN'new_vliegtuig,operator=CPN'operator,controleur=CPN'controleur} 
 else raise CPN'Error (Vliegtuig.illegal_msg(CPN'new_vliegtuig))
 fun set_operator {vliegtuig=CPN'vliegtuig,operator=CPN'operator,controleur=CPN'controleur} (CPN'new_operator:Operator) = 
 if Operator.legal CPN'new_operator
 then {vliegtuig=CPN'vliegtuig,operator=CPN'new_operator,controleur=CPN'controleur} 
 else raise CPN'Error (Operator.illegal_msg(CPN'new_operator))
 fun set_controleur {vliegtuig=CPN'vliegtuig,operator=CPN'operator,controleur=CPN'controleur} (CPN'new_controleur:Controleur) = 
 if Controleur.legal CPN'new_controleur
 then {vliegtuig=CPN'vliegtuig,operator=CPN'operator,controleur=CPN'new_controleur} 
 else raise CPN'Error (Controleur.illegal_msg(CPN'new_controleur))
 end;

 structure VliegtuigOperatorControleur'timed = CPN'ColorSets.TimedCS (structure cs = VliegtuigOperatorControleur and Time = CPN'Time);
 type VliegtuigOperatorControleur'timed = VliegtuigOperatorControleur'timed.cs;

 structure Iceman = CPN'ColorSets.IntCS;
 type Iceman = Iceman.cs;

 type VliegtuigOperatorIcemanControleur = {vliegtuig:Vliegtuig,operator:Operator,iceman:Iceman,controleur:Controleur}
 structure VliegtuigOperatorIcemanControleur = struct
 type cs = VliegtuigOperatorIcemanControleur
 val base = {vliegtuig=Vliegtuig.base,operator=Operator.base,iceman=Iceman.base,controleur=Controleur.base}
 fun lt ({vliegtuig=CPN'vliegtuig,operator=CPN'operator,iceman=CPN'iceman,controleur=CPN'controleur},{vliegtuig=CPN'vliegtuig',operator=CPN'operator',iceman=CPN'iceman',controleur=CPN'controleur'}) = if Vliegtuig.lt(CPN'vliegtuig,CPN'vliegtuig') then true
 else if Vliegtuig.lt(CPN'vliegtuig',CPN'vliegtuig) then false
 else if Operator.lt(CPN'operator,CPN'operator') then true
 else if Operator.lt(CPN'operator',CPN'operator) then false
 else if Iceman.lt(CPN'iceman,CPN'iceman') then true
 else if Iceman.lt(CPN'iceman',CPN'iceman) then false
 else if Controleur.lt(CPN'controleur,CPN'controleur') then true
 else if Controleur.lt(CPN'controleur',CPN'controleur) then false
 else false
 val cmp:VliegtuigOperatorIcemanControleur * VliegtuigOperatorIcemanControleur-> order = (CPN'Misc.a_cmp lt)
 fun mkstr {vliegtuig=CPN'vliegtuig,operator=CPN'operator,iceman=CPN'iceman,controleur=CPN'controleur} = CPN'concat ["{vliegtuig=",Vliegtuig.mkstr CPN'vliegtuig,",operator=",Operator.mkstr CPN'operator,",iceman=",Iceman.mkstr CPN'iceman,",controleur=",Controleur.mkstr CPN'controleur,"}"]
 val mkstr_ms : cs CPN'MS.ms -> string = CPN'MS.mkstr_ms (mkstr,lt)
 fun legal {vliegtuig=CPN'vliegtuig,operator=CPN'operator,iceman=CPN'iceman,controleur=CPN'controleur} = Vliegtuig.legal (CPN'vliegtuig) andalso Operator.legal (CPN'operator) andalso Iceman.legal (CPN'iceman) andalso Controleur.legal (CPN'controleur)
 fun illegal_msg {vliegtuig=CPN'vliegtuig,operator=CPN'operator,iceman=CPN'iceman,controleur=CPN'controleur} = 
 if legal {vliegtuig=CPN'vliegtuig,operator=CPN'operator,iceman=CPN'iceman,controleur=CPN'controleur} then ""
 else "Illegal component in color: "^(mkstr {vliegtuig=CPN'vliegtuig,operator=CPN'operator,iceman=CPN'iceman,controleur=CPN'controleur})
 fun size () = Vliegtuig.size()*Operator.size()*Iceman.size()*Controleur.size()
 fun mult (CPN'Y1: Vliegtuig CPN'MS.ms,CPN'Y2: Operator CPN'MS.ms,CPN'Y3: Iceman CPN'MS.ms,CPN'Y4: Controleur CPN'MS.ms): VliegtuigOperatorIcemanControleur CPN'MS.ms = 
let
 fun CPN'mult(CPN'X1 as (CPN'x1)::_,CPN'X2 as (CPN'x2)::_,CPN'X3 as (CPN'x3)::_,(CPN'x4)::CPN'xs4) =
 ({vliegtuig=CPN'x1,operator=CPN'x2,iceman=CPN'x3,controleur=CPN'x4})::CPN'mult(CPN'X1,CPN'X2,CPN'X3,CPN'xs4)
 | CPN'mult(CPN'X1,CPN'X2,_::CPN'xs3,nil) = CPN'mult(CPN'X1,CPN'X2,CPN'xs3,CPN'Y4)
 | CPN'mult(CPN'X1,_::CPN'xs2,nil,_) = CPN'mult(CPN'X1,CPN'xs2,CPN'Y3,CPN'Y4)
 | CPN'mult(_::CPN'xs1,nil,_,_) = CPN'mult(CPN'xs1,CPN'Y2,CPN'Y3,CPN'Y4)
 | CPN'mult(nil,_,_,_) = nil
 in CPN'mult(CPN'Y1,CPN'Y2,CPN'Y3,CPN'Y4) end
 fun input CPN's = let
 val CPN'list = ref (nil: (string*string) list)
 val _ = CPN'StreamIO.skip_white_spaces CPN's
 val _ = case CPN'StreamIO.get_one CPN's of
 (SOME #"{") => ()
 | NONE => raise CPN'StreamIO.IOError("Can not find '{' when reading a record")
 val CPN'label = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#"="])))
 val CPN'value = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#","])))
 val _ = CPN'list::= (CPN'label,CPN'value)
 val CPN'label = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#"="])))
 val CPN'value = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#","])))
 val _ = CPN'list::= (CPN'label,CPN'value)
 val CPN'label = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#"="])))
 val CPN'value = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#","])))
 val _ = CPN'list::= (CPN'label,CPN'value)
 val CPN'label = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#"="])))
 val CPN'value = String.implode(#2(CPN'StreamIO.get_until(CPN's,[#"}"])))
 val _ = CPN'list::= (CPN'label,CPN'value)
 in
 {vliegtuig=Vliegtuig.input(TextIO.openString(CPN'Decl.find("vliegtuig", !CPN'list))),
 operator=Operator.input(TextIO.openString(CPN'Decl.find("operator", !CPN'list))),
 iceman=Iceman.input(TextIO.openString(CPN'Decl.find("iceman", !CPN'list))),
 controleur=Controleur.input(TextIO.openString(CPN'Decl.find("controleur", !CPN'list)))}
 end
 fun output(CPN's,CPN'c) = 
 if legal CPN'c then CPN'IO.output(CPN's,(mkstr CPN'c)^" ")
 else raise CPN'Error (illegal_msg(CPN'c))
 val input_ms : TextIO.instream -> cs CPN'MS.ms = CPN'MS.input_ms false input
 val output_ms : TextIO.outstream * cs CPN'MS.ms -> unit = CPN'MS.output_ms false (output,lt)
 local
 val all_ref = ref(NONE: VliegtuigOperatorIcemanControleur CPN'MS.ms option)
 in
 fun all () = case !all_ref of SOME(all') => all'
 | NONE => let
 val all' = mult (Vliegtuig.all(),Operator.all(),Iceman.all(),Controleur.all())
 in (all_ref:= SOME(all'); all') end
 end
 fun ord {vliegtuig=CPN'vliegtuig,operator=CPN'operator,iceman=CPN'iceman,controleur=CPN'controleur} =
Controleur.ord(CPN'controleur)+Controleur.size()*Iceman.ord(CPN'iceman)+Iceman.size()*Controleur.size()*Operator.ord(CPN'operator)+Operator.size()*Iceman.size()*Controleur.size()*Vliegtuig.ord(CPN'vliegtuig)
 fun col CPN'i =
 if 0<=CPN'i andalso CPN'i<size() then
 {controleur=Controleur.col(CPN'i mod Controleur.size()),
 iceman=Iceman.col((CPN'i div (Controleur.size())) mod Iceman.size()),
 operator=Operator.col((CPN'i div (Iceman.size()*Controleur.size())) mod Operator.size()),
 vliegtuig=Vliegtuig.col((CPN'i div (Operator.size()*Iceman.size()*Controleur.size())) mod Vliegtuig.size())}
 else raise CPN'Error CPN'ColorSets.out_of_range
 fun ran () = {vliegtuig=Vliegtuig.ran(),operator=Operator.ran(),iceman=Iceman.ran(),controleur=Controleur.ran()}
 fun set_vliegtuig {vliegtuig=CPN'vliegtuig,operator=CPN'operator,iceman=CPN'iceman,controleur=CPN'controleur} (CPN'new_vliegtuig:Vliegtuig) = 
 if Vliegtuig.legal CPN'new_vliegtuig
 then {vliegtuig=CPN'new_vliegtuig,operator=CPN'operator,iceman=CPN'iceman,controleur=CPN'controleur} 
 else raise CPN'Error (Vliegtuig.illegal_msg(CPN'new_vliegtuig))
 fun set_operator {vliegtuig=CPN'vliegtuig,operator=CPN'operator,iceman=CPN'iceman,controleur=CPN'controleur} (CPN'new_operator:Operator) = 
 if Operator.legal CPN'new_operator
 then {vliegtuig=CPN'vliegtuig,operator=CPN'new_operator,iceman=CPN'iceman,controleur=CPN'controleur} 
 else raise CPN'Error (Operator.illegal_msg(CPN'new_operator))
 fun set_iceman {vliegtuig=CPN'vliegtuig,operator=CPN'operator,iceman=CPN'iceman,controleur=CPN'controleur} (CPN'new_iceman:Iceman) = 
 if Iceman.legal CPN'new_iceman
 then {vliegtuig=CPN'vliegtuig,operator=CPN'operator,iceman=CPN'new_iceman,controleur=CPN'controleur} 
 else raise CPN'Error (Iceman.illegal_msg(CPN'new_iceman))
 fun set_controleur {vliegtuig=CPN'vliegtuig,operator=CPN'operator,iceman=CPN'iceman,controleur=CPN'controleur} (CPN'new_controleur:Controleur) = 
 if Controleur.legal CPN'new_controleur
 then {vliegtuig=CPN'vliegtuig,operator=CPN'operator,iceman=CPN'iceman,controleur=CPN'new_controleur} 
 else raise CPN'Error (Controleur.illegal_msg(CPN'new_controleur))
 end;

 structure VliegtuigOperatorIcemanControleur'timed = CPN'ColorSets.TimedCS (structure cs = VliegtuigOperatorIcemanControleur and Time = CPN'Time);
 type VliegtuigOperatorIcemanControleur'timed = VliegtuigOperatorIcemanControleur'timed.cs;

 structure SmallInt = CPN'ColorSets.IntWithCS (val CPN'low= 1 and CPN'high= 12);
 type SmallInt = SmallInt.cs;

 structure WelNiet = CPN'ColorSets.IntWithCS (val CPN'low= 0 and CPN'high= 1);
 type WelNiet = WelNiet.cs;

 structure LInt = CPN'ColorSets.ListCS (structure cs= INT);
 type LInt = LInt.cs;
fun GenVliegtuig(0,at:ArrivalTime,b:Besluit,
dt:DepartTime, l:LOnderdelen,tk:RecordTime)=[]
| GenVliegtuig(id:VliegtuigID,at,b,dt,l,tk) = 
  if id>0
  then {vliegID=0,arrtime=at,
    x=discrete(1,100), deptime= at+ 60+
  round(exponential(TijdTussenAankomstVertrekWaarde)), listonderdelen=l,
   tijdklaar=tk} 
    ::GenVliegtuig(id-1,at,b,dt,l,tk)
  else [];
fun GenOnderdelen(0,l:LOnderdelen)=[]
| GenOnderdelen(id:INT,l) = 
  if id>0
  then {id=id,l=l} 
    ::GenOnderdelen(id-1,l)
  else [];
fun higherPriority(p1,p2) =(p1<p2);
fun qInsert (vt:Vliegtuig,[]) = [vt]
| qInsert(vt,(q::queue))=
(*if higherPriority(#deptime vt, #deptime q)
then *)vt::q::queue
(*else q::(qInsert (vt,queue))*);
fun AankomstDeicing(v:Vliegtuig)=
{vliegID= #vliegID(v), arrtime= #deptime(v) - discrete(30,60),
x= #x(v), deptime= #deptime(v), listonderdelen= #listonderdelen(v),
tijdklaar= #tijdklaar(v)};
fun Invoegen (elm,[]) = [elm]
| Invoegen(elm,(q::queue))= 
if higherPriority(elm,q) then elm::q::queue
else q::(Invoegen (elm,queue));
fun Onderdeel(i:INT,l:LOnderdelen, wn:WelNiet)=
if l=[] orelse wn>0 then Invoegen(i,l)
else l;
fun Onderdeel2(i:INT,l:LOnderdelen, wn:WelNiet)=
if l=[] orelse wn>0 then Invoegen(i,l)
else l;
fun CheckTijdEnGeenDeicing(at:ArrivalTime,
      x:Besluit)= if (at<=CurrentTime()
      andalso x>= (if ((Weer*60)>CurrentTime()) 
      orelse ((540-(Weer-5)*60)<CurrentTime())
      then Weer else Int.max((Weer-3),0))*20)
      then true else false;
fun CheckTijdEnDeicing(at:ArrivalTime,
      x:Besluit)= if (at<=CurrentTime()
      andalso x< (if ((Weer*60)>CurrentTime()) 
      orelse ((540-(Weer-5)*60)<CurrentTime())
      then Weer else Int.max((Weer-3),0))*20)
      then true else false;
fun GenID(x:Vliegtuig, nr:INT, l:LOnderdelen)=
 {vliegID= #vliegID(x)+nr, arrtime= #arrtime(x),
 x= #x(x), deptime= #deptime(x), 
listonderdelen= l, tijdklaar= #tijdklaar(x)};
fun GenControleur(i:INT, l:LOnderdelen)=
 {id=i, l=l};
fun UpdateDepTime(vt:Vliegtuig, l:LVliegtuig) =
{vliegID=(#vliegID(vt)),arrtime=(#arrtime(vt)),
x=(#x(vt)), deptime=Int.max((#deptime(vt)),
CurrentTime()+(length(l)) div (VerwWeerscen)*GemTijdDeicen), 
listonderdelen=(#listonderdelen(vt)),
tijdklaar=(#tijdklaar(vt))};
fun RecordTime(vt:VliegtuigOperatorControleur) =
{vliegID=(#vliegID(#vliegtuig(vt))),arrtime=(#arrtime(#vliegtuig(vt))),
x=(#x(#vliegtuig(vt))), deptime=(#deptime(#vliegtuig(vt))), listonderdelen=
(#listonderdelen (#vliegtuig(vt))),tijdklaar=CurrentTime()};
fun RecordTime2(vt:VliegtuigOperator) =
{vliegID=(#vliegID(#vliegtuig(vt))),arrtime=(#arrtime(#vliegtuig(vt))),
x=(#x(#vliegtuig(vt))), deptime=(#deptime(#vliegtuig(vt))), listonderdelen=
(#listonderdelen (#vliegtuig(vt))), tijdklaar=CurrentTime()};
fun UpdateOnderdeel(vt:Vliegtuig, l:LOnderdelen) =
{vliegID=(#vliegID(vt)),arrtime=(#arrtime(vt)),
x=(#x(vt)), deptime=(#deptime(vt)), 
listonderdelen=l, tijdklaar=(#tijdklaar(vt))};
fun UpdateOnderdeel2(vt:VliegtuigDCoordinator, l:LOnderdelen) =
{vliegID=(#vliegID(#vliegtuig(vt))),arrtime=(#arrtime(#vliegtuig(vt))),
x=(#x(#vliegtuig(vt))), deptime=(#deptime(#vliegtuig(vt))), 
listonderdelen=l, tijdklaar=(#tijdklaar(#vliegtuig(vt)))};
fun InsertList(v:VliegtuigDCoordinator, l:LOnderdelen) =
{vliegID=(#vliegID(#vliegtuig(v))),arrtime=(#arrtime(#vliegtuig(v))),
x=(#x(#vliegtuig(v))), deptime=(#deptime(#vliegtuig(v))),
listonderdelen=l};
fun WelkeOnderdelen() = if
Weer = 0 then binomial(1, 1.0/10.0) else  if
Weer = 1 then binomial(1, 3.0/10.0) else  if
Weer = 2 then binomial(1, 5.0/10.0) else  if
Weer = 3 then binomial(1, 7.0/10.0) else  if
Weer = 4 then binomial(1, 8.0/10.0) else 
binomial(1, 9.0/10.0);
fun EenOfTwee() =
if ((if ((Weer*60)>CurrentTime()) orelse ((540-(Weer-5)*60)<CurrentTime())
then Weer else Int.max((Weer-3),0)) = 0 orelse
(if ((Weer*60)>CurrentTime()) orelse ((540-(Weer-5)*60)<CurrentTime())
then Weer else Int.max((Weer-3),0)) = 1 orelse
(if ((Weer*60)>CurrentTime()) orelse ((540-(Weer-5)*60)<CurrentTime())
then Weer else Int.max((Weer-3),0)) = 2 orelse 
(if ((Weer*60)>CurrentTime()) orelse ((540-(Weer-5)*60)<CurrentTime())
then Weer else Int.max((Weer-3),0)) = 3) then 1 else 2;
fun Operators(operator:Operator)=
if VerwWeerscen = 0 then Int.min(4,operator) else
if VerwWeerscen = 1 then Int.min(8,operator) else
if (VerwWeerscen = 2 orelse VerwWeerscen =3)
then Int.min((8+binomial(1, 1.0/20.0)*2-
binomial(1, 1.0/20.0)*2), operator) else Int.min((8+binomial
(1, 1.0/25.0)*2-binomial(1, 1.0/25.0)*4 
+ binomial(1, 1.0/25.0)*2+binomial(1, 1.0/25.0)*4), operator);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'Vliegtuig'timed_pims123456 = CPN'MakeTreeListPIMS(structure cs = Vliegtuig'timed; val cmp:Vliegtuig'timed * Vliegtuig'timed -> order = CPN'Misc.a_cmp Vliegtuig'timed.lt);
 structure CPN'Vliegtuig'timed_pims123456_sims = CPN'MakeTreeSIMS(structure pims = CPN'Vliegtuig'timed_pims123456 and Time = CPN'Time; val time = fn (CPN'Time.@(_,CPN't)) => CPN't);
 structure CPN'placeID1037087828 = CPN'Place.MakeTimedPlace (structure sims= CPN'Vliegtuig'timed_pims123456_sims and Time = CPN'Time; val no_of_inst= 1 and offset= (maketime "0"));
 val _ = (CPN'placeID1037087828.init_mark:= (map (fn CPN'col => CPN'col@(Option.valOf(CPN'Time.start_time)))([])); CPN'Place.init_mark_funs::= CPN'placeID1037087828.set_init_mark;CPN'placeID1037087828.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1037087828",{print=CPN'placeID1037087828.print,size=CPN'placeID1037087828.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1037095434 = CPN'Place.MakeTimedPlace (structure sims= CPN'Vliegtuig'timed_pims123456_sims and Time = CPN'Time; val no_of_inst= 1 and offset= (maketime "0"));
 val _ = (CPN'placeID1037095434.init_mark:= (map (fn CPN'col => CPN'col@(Option.valOf(CPN'Time.start_time)))([])); CPN'Place.init_mark_funs::= CPN'placeID1037095434.set_init_mark;CPN'placeID1037095434.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1037095434",{print=CPN'placeID1037095434.print,size=CPN'placeID1037095434.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'Snowdesk_pims1 = CPN'MakeTreeListPIMS(structure cs = Snowdesk; val cmp:Snowdesk * Snowdesk -> order = CPN'Misc.a_cmp Snowdesk.lt);
 structure CPN'placeID1037099424 = CPN'Place.MakePlace (structure ims = CPN'Snowdesk_pims1; val no_of_inst = 1);
 val _ = (CPN'placeID1037099424.init_mark:= (10`"snowdesk"); CPN'Place.init_mark_funs::= CPN'placeID1037099424.set_init_mark;CPN'placeID1037099424.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1037099424",{print=CPN'placeID1037099424.print,size=CPN'placeID1037099424.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'LVliegtuig'timed_pims1 = CPN'MakeTreeListPIMS(structure cs = LVliegtuig'timed; val cmp:LVliegtuig'timed * LVliegtuig'timed -> order = CPN'Misc.a_cmp LVliegtuig'timed.lt);
 structure CPN'LVliegtuig'timed_pims1_sims = CPN'MakeTreeSIMS(structure pims = CPN'LVliegtuig'timed_pims1 and Time = CPN'Time; val time = fn (CPN'Time.@(_,CPN't)) => CPN't);
 structure CPN'placeID1037103710 = CPN'Place.MakeTimedPlace (structure sims= CPN'LVliegtuig'timed_pims1_sims and Time = CPN'Time; val no_of_inst= 1 and offset= (maketime "0"));
 val _ = (CPN'placeID1037103710.init_mark:= (map (fn CPN'col => CPN'col@(Option.valOf(CPN'Time.start_time)))(1`[])); CPN'Place.init_mark_funs::= CPN'placeID1037103710.set_init_mark;CPN'placeID1037103710.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1037103710",{print=CPN'placeID1037103710.print,size=CPN'placeID1037103710.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'DCoordinator_pims1 = CPN'MakeTreeListPIMS(structure cs = DCoordinator; val cmp:DCoordinator * DCoordinator -> order = CPN'Misc.a_cmp DCoordinator.lt);
 structure CPN'placeID1037135843 = CPN'Place.MakePlace (structure ims = CPN'DCoordinator_pims1; val no_of_inst = 1);
 val _ = (CPN'placeID1037135843.init_mark:= (7`"DCoordinator"); CPN'Place.init_mark_funs::= CPN'placeID1037135843.set_init_mark;CPN'placeID1037135843.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1037135843",{print=CPN'placeID1037135843.print,size=CPN'placeID1037135843.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'LVL_pims1 = CPN'MakeTreeListPIMS(structure cs = LVL; val cmp:LVL * LVL -> order = CPN'Misc.a_cmp LVL.lt);
 structure CPN'placeID1037143520 = CPN'Place.MakePlace (structure ims = CPN'LVL_pims1; val no_of_inst = 1);
 val _ = (CPN'placeID1037143520.init_mark:= (10`"LVL"); CPN'Place.init_mark_funs::= CPN'placeID1037143520.set_init_mark;CPN'placeID1037143520.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1037143520",{print=CPN'placeID1037143520.print,size=CPN'placeID1037143520.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1037255700 = CPN'Place.MakeTimedPlace (structure sims= CPN'Vliegtuig'timed_pims123456_sims and Time = CPN'Time; val no_of_inst= 1 and offset= (maketime "0"));
 val _ = (CPN'placeID1037255700.init_mark:= (map (fn CPN'col => CPN'col@(Option.valOf(CPN'Time.start_time)))([])); CPN'Place.init_mark_funs::= CPN'placeID1037255700.set_init_mark;CPN'placeID1037255700.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1037255700",{print=CPN'placeID1037255700.print,size=CPN'placeID1037255700.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1037496860 = CPN'Place.MakeTimedPlace (structure sims= CPN'Vliegtuig'timed_pims123456_sims and Time = CPN'Time; val no_of_inst= 1 and offset= (maketime "0"));
 val _ = (CPN'placeID1037496860.init_mark:= (map (fn CPN'col => CPN'col@(Option.valOf(CPN'Time.start_time)))([])); CPN'Place.init_mark_funs::= CPN'placeID1037496860.set_init_mark;CPN'placeID1037496860.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1037496860",{print=CPN'placeID1037496860.print,size=CPN'placeID1037496860.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1038032796 = CPN'Place.MakeTimedPlace (structure sims= CPN'Vliegtuig'timed_pims123456_sims and Time = CPN'Time; val no_of_inst= 1 and offset= (maketime "0"));
 val _ = (CPN'placeID1038032796.init_mark:= (map (fn CPN'col => CPN'col@(Option.valOf(CPN'Time.start_time)))([])); CPN'Place.init_mark_funs::= CPN'placeID1038032796.set_init_mark;CPN'placeID1038032796.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1038032796",{print=CPN'placeID1038032796.print,size=CPN'placeID1038032796.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1038043812 = CPN'Place.MakeTimedPlace (structure sims= CPN'Vliegtuig'timed_pims123456_sims and Time = CPN'Time; val no_of_inst= 1 and offset= (maketime "0"));
 val _ = (CPN'placeID1038043812.init_mark:= (map (fn CPN'col => CPN'col@(Option.valOf(CPN'Time.start_time)))([])); CPN'Place.init_mark_funs::= CPN'placeID1038043812.set_init_mark;CPN'placeID1038043812.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1038043812",{print=CPN'placeID1038043812.print,size=CPN'placeID1038043812.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'Controle_pims12 = CPN'MakeTreeListPIMS(structure cs = Controle; val cmp:Controle * Controle -> order = CPN'Misc.a_cmp Controle.lt);
 structure CPN'placeID1043596930 = CPN'Place.MakePlace (structure ims = CPN'Controle_pims12; val no_of_inst = 1);
 val _ = (CPN'placeID1043596930.init_mark:= ([]); CPN'Place.init_mark_funs::= CPN'placeID1043596930.set_init_mark;CPN'placeID1043596930.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1043596930",{print=CPN'placeID1043596930.print,size=CPN'placeID1043596930.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1049297444 = CPN'Place.MakeTimedPlace (structure sims= CPN'Vliegtuig'timed_pims123456_sims and Time = CPN'Time; val no_of_inst= 1 and offset= (maketime "0"));
 val _ = (CPN'placeID1049297444.init_mark:= (map (fn CPN'col => CPN'col@(Option.valOf(CPN'Time.start_time)))([])); CPN'Place.init_mark_funs::= CPN'placeID1049297444.set_init_mark;CPN'placeID1049297444.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1049297444",{print=CPN'placeID1049297444.print,size=CPN'placeID1049297444.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1049328673 = CPN'Place.MakeTimedPlace (structure sims= CPN'Vliegtuig'timed_pims123456_sims and Time = CPN'Time; val no_of_inst= 1 and offset= (maketime "0"));
 val _ = (CPN'placeID1049328673.init_mark:= (map (fn CPN'col => CPN'col@(Option.valOf(CPN'Time.start_time)))([])); CPN'Place.init_mark_funs::= CPN'placeID1049328673.set_init_mark;CPN'placeID1049328673.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1049328673",{print=CPN'placeID1049328673.print,size=CPN'placeID1049328673.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1037090534 = CPN'Place.MakeTimedPort (structure sims = CPN'Vliegtuig'timed_pims123456_sims and Time = CPN'Time;
 val marks= [CPN'placeID1037087828.mark 1]
 and waits= [CPN'placeID1037087828.wait 1]
 and next_times= [(CPN'placeID1037087828.next_time,1)]
 and inits= [(CPN'placeID1037087828.init,1)])
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1037090534",{print=CPN'placeID1037090534.print,size=CPN'placeID1037090534.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'INT_pims1 = CPN'MakeTreeListPIMS(structure cs = INT; val cmp:INT * INT -> order = CPN'Misc.a_cmp INT.lt);
 structure CPN'placeID1040165399 = CPN'Place.MakePlace (structure ims = CPN'INT_pims1; val no_of_inst = 1);
 val _ = (CPN'placeID1040165399.init_mark:= (1`1); CPN'Place.init_mark_funs::= CPN'placeID1040165399.set_init_mark;CPN'placeID1040165399.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1040165399",{print=CPN'placeID1040165399.print,size=CPN'placeID1040165399.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1043587409 = CPN'Place.MakePort (structure ims = CPN'Controle_pims12; val markings = [CPN'placeID1043596930.mark 1]);
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1043587409",{print=CPN'placeID1043587409.print,size=CPN'placeID1043587409.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1043868911 = CPN'Place.MakePlace (structure ims = CPN'INT_pims1; val no_of_inst = 1);
 val _ = (CPN'placeID1043868911.init_mark:= (1`1); CPN'Place.init_mark_funs::= CPN'placeID1043868911.set_init_mark;CPN'placeID1043868911.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1043868911",{print=CPN'placeID1043868911.print,size=CPN'placeID1043868911.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'LOnderdelen_pims1 = CPN'MakeTreeListPIMS(structure cs = LOnderdelen; val cmp:LOnderdelen * LOnderdelen -> order = CPN'Misc.a_cmp LOnderdelen.lt);
 structure CPN'placeID1043899894 = CPN'Place.MakePlace (structure ims = CPN'LOnderdelen_pims1; val no_of_inst = 1);
 val _ = (CPN'placeID1043899894.init_mark:= (1`[]); CPN'Place.init_mark_funs::= CPN'placeID1043899894.set_init_mark;CPN'placeID1043899894.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1043899894",{print=CPN'placeID1043899894.print,size=CPN'placeID1043899894.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'WelNiet_pims1 = CPN'MakeTreeListPIMS(structure cs = WelNiet; val cmp:WelNiet * WelNiet -> order = CPN'Misc.a_cmp WelNiet.lt);
 structure CPN'placeID1043972563 = CPN'Place.MakePlace (structure ims = CPN'WelNiet_pims1; val no_of_inst = 1);
 val _ = (CPN'placeID1043972563.init_mark:= ([]); CPN'Place.init_mark_funs::= CPN'placeID1043972563.set_init_mark;CPN'placeID1043972563.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1043972563",{print=CPN'placeID1043972563.print,size=CPN'placeID1043972563.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'LInt_pims1 = CPN'MakeTreeListPIMS(structure cs = LInt; val cmp:LInt * LInt -> order = CPN'Misc.a_cmp LInt.lt);
 structure CPN'placeID1044083806 = CPN'Place.MakePlace (structure ims = CPN'LInt_pims1; val no_of_inst = 1);
 val _ = (CPN'placeID1044083806.init_mark:= (1`[]); CPN'Place.init_mark_funs::= CPN'placeID1044083806.set_init_mark;CPN'placeID1044083806.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1044083806",{print=CPN'placeID1044083806.print,size=CPN'placeID1044083806.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1044341038 = CPN'Place.MakePlace (structure ims = CPN'INT_pims1; val no_of_inst = 1);
 val _ = (CPN'placeID1044341038.init_mark:= (1`0); CPN'Place.init_mark_funs::= CPN'placeID1044341038.set_init_mark;CPN'placeID1044341038.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1044341038",{print=CPN'placeID1044341038.print,size=CPN'placeID1044341038.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1049307988 = CPN'Place.MakePlace (structure ims = CPN'INT_pims1; val no_of_inst = 1);
 val _ = (CPN'placeID1049307988.init_mark:= (1`0); CPN'Place.init_mark_funs::= CPN'placeID1049307988.set_init_mark;CPN'placeID1049307988.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1049307988",{print=CPN'placeID1049307988.print,size=CPN'placeID1049307988.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1037471096 = CPN'Place.MakePort (structure ims = CPN'Snowdesk_pims1; val markings = [CPN'placeID1037099424.mark 1]);
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1037471096",{print=CPN'placeID1037471096.print,size=CPN'placeID1037471096.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1037471101 = CPN'Place.MakeTimedPort (structure sims = CPN'LVliegtuig'timed_pims1_sims and Time = CPN'Time;
 val marks= [CPN'placeID1037103710.mark 1]
 and waits= [CPN'placeID1037103710.wait 1]
 and next_times= [(CPN'placeID1037103710.next_time,1)]
 and inits= [(CPN'placeID1037103710.init,1)])
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1037471101",{print=CPN'placeID1037471101.print,size=CPN'placeID1037471101.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'VliegtuigSnowdesk'timed_pims12 = CPN'MakeTreeListPIMS(structure cs = VliegtuigSnowdesk'timed; val cmp:VliegtuigSnowdesk'timed * VliegtuigSnowdesk'timed -> order = CPN'Misc.a_cmp VliegtuigSnowdesk'timed.lt);
 structure CPN'VliegtuigSnowdesk'timed_pims12_sims = CPN'MakeTreeSIMS(structure pims = CPN'VliegtuigSnowdesk'timed_pims12 and Time = CPN'Time; val time = fn (CPN'Time.@(_,CPN't)) => CPN't);
 structure CPN'placeID1037471106 = CPN'Place.MakeTimedPlace (structure sims= CPN'VliegtuigSnowdesk'timed_pims12_sims and Time = CPN'Time; val no_of_inst= 1 and offset= (maketime "0"));
 val _ = (CPN'placeID1037471106.init_mark:= (map (fn CPN'col => CPN'col@(Option.valOf(CPN'Time.start_time)))([])); CPN'Place.init_mark_funs::= CPN'placeID1037471106.set_init_mark;CPN'placeID1037471106.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1037471106",{print=CPN'placeID1037471106.print,size=CPN'placeID1037471106.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1037471900 = CPN'Place.MakeTimedPort (structure sims = CPN'Vliegtuig'timed_pims123456_sims and Time = CPN'Time;
 val marks= [CPN'placeID1037095434.mark 1]
 and waits= [CPN'placeID1037095434.wait 1]
 and next_times= [(CPN'placeID1037095434.next_time,1)]
 and inits= [(CPN'placeID1037095434.init,1)])
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1037471900",{print=CPN'placeID1037471900.print,size=CPN'placeID1037471900.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1042262402 = CPN'Place.MakePort (structure ims = CPN'LVL_pims1; val markings = [CPN'placeID1037143520.mark 1]);
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1042262402",{print=CPN'placeID1042262402.print,size=CPN'placeID1042262402.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1042658947 = CPN'Place.MakeTimedPort (structure sims = CPN'Vliegtuig'timed_pims123456_sims and Time = CPN'Time;
 val marks= [CPN'placeID1037496860.mark 1]
 and waits= [CPN'placeID1037496860.wait 1]
 and next_times= [(CPN'placeID1037496860.next_time,1)]
 and inits= [(CPN'placeID1037496860.init,1)])
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1042658947",{print=CPN'placeID1042658947.print,size=CPN'placeID1042658947.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'VliegtuigLVL'timed_pims12 = CPN'MakeTreeListPIMS(structure cs = VliegtuigLVL'timed; val cmp:VliegtuigLVL'timed * VliegtuigLVL'timed -> order = CPN'Misc.a_cmp VliegtuigLVL'timed.lt);
 structure CPN'VliegtuigLVL'timed_pims12_sims = CPN'MakeTreeSIMS(structure pims = CPN'VliegtuigLVL'timed_pims12 and Time = CPN'Time; val time = fn (CPN'Time.@(_,CPN't)) => CPN't);
 structure CPN'placeID1043559862 = CPN'Place.MakeTimedPlace (structure sims= CPN'VliegtuigLVL'timed_pims12_sims and Time = CPN'Time; val no_of_inst= 1 and offset= (maketime "0"));
 val _ = (CPN'placeID1043559862.init_mark:= (map (fn CPN'col => CPN'col@(Option.valOf(CPN'Time.start_time)))([])); CPN'Place.init_mark_funs::= CPN'placeID1043559862.set_init_mark;CPN'placeID1043559862.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1043559862",{print=CPN'placeID1043559862.print,size=CPN'placeID1043559862.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1037477724 = CPN'Place.MakePort (structure ims = CPN'DCoordinator_pims1; val markings = [CPN'placeID1037135843.mark 1]);
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1037477724",{print=CPN'placeID1037477724.print,size=CPN'placeID1037477724.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1037477729 = CPN'Place.MakeTimedPort (structure sims = CPN'LVliegtuig'timed_pims1_sims and Time = CPN'Time;
 val marks= [CPN'placeID1037103710.mark 1]
 and waits= [CPN'placeID1037103710.wait 1]
 and next_times= [(CPN'placeID1037103710.next_time,1)]
 and inits= [(CPN'placeID1037103710.init,1)])
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1037477729",{print=CPN'placeID1037477729.print,size=CPN'placeID1037477729.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'VliegtuigDCoordinator'timed_pims12 = CPN'MakeTreeListPIMS(structure cs = VliegtuigDCoordinator'timed; val cmp:VliegtuigDCoordinator'timed * VliegtuigDCoordinator'timed -> order = CPN'Misc.a_cmp VliegtuigDCoordinator'timed.lt);
 structure CPN'VliegtuigDCoordinator'timed_pims12_sims = CPN'MakeTreeSIMS(structure pims = CPN'VliegtuigDCoordinator'timed_pims12 and Time = CPN'Time; val time = fn (CPN'Time.@(_,CPN't)) => CPN't);
 structure CPN'placeID1037477734 = CPN'Place.MakeTimedPlace (structure sims= CPN'VliegtuigDCoordinator'timed_pims12_sims and Time = CPN'Time; val no_of_inst= 1 and offset= (maketime "0"));
 val _ = (CPN'placeID1037477734.init_mark:= (map (fn CPN'col => CPN'col@(Option.valOf(CPN'Time.start_time)))([])); CPN'Place.init_mark_funs::= CPN'placeID1037477734.set_init_mark;CPN'placeID1037477734.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1037477734",{print=CPN'placeID1037477734.print,size=CPN'placeID1037477734.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1037477741 = CPN'Place.MakeTimedPlace (structure sims= CPN'VliegtuigDCoordinator'timed_pims12_sims and Time = CPN'Time; val no_of_inst= 1 and offset= (maketime "0"));
 val _ = (CPN'placeID1037477741.init_mark:= (map (fn CPN'col => CPN'col@(Option.valOf(CPN'Time.start_time)))([])); CPN'Place.init_mark_funs::= CPN'placeID1037477741.set_init_mark;CPN'placeID1037477741.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1037477741",{print=CPN'placeID1037477741.print,size=CPN'placeID1037477741.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'Operator_pims1 = CPN'MakeTreeListPIMS(structure cs = Operator; val cmp:Operator * Operator -> order = CPN'Misc.a_cmp Operator.lt);
 structure CPN'placeID1037530306 = CPN'Place.MakePlace (structure ims = CPN'Operator_pims1; val no_of_inst = 1);
 val _ = (CPN'placeID1037530306.init_mark:= (if VerwWeerscen = 0 
then 1`4 else  if
VerwWeerscen = 1
then 1`8 else  if
VerwWeerscen = 2
then 1`16 else  if
VerwWeerscen = 3
then 1`24 else  if
VerwWeerscen = 4
then 1`32 else 1`40); CPN'Place.init_mark_funs::= CPN'placeID1037530306.set_init_mark;CPN'placeID1037530306.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1037530306",{print=CPN'placeID1037530306.print,size=CPN'placeID1037530306.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'Iceman_pims1 = CPN'MakeTreeListPIMS(structure cs = Iceman; val cmp:Iceman * Iceman -> order = CPN'Misc.a_cmp Iceman.lt);
 structure CPN'placeID1037539323 = CPN'Place.MakePlace (structure ims = CPN'Iceman_pims1; val no_of_inst = 1);
 val _ = (CPN'placeID1037539323.init_mark:= (1`1); CPN'Place.init_mark_funs::= CPN'placeID1037539323.set_init_mark;CPN'placeID1037539323.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1037539323",{print=CPN'placeID1037539323.print,size=CPN'placeID1037539323.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'Controleur_pims1 = CPN'MakeTreeListPIMS(structure cs = Controleur; val cmp:Controleur * Controleur -> order = CPN'Misc.a_cmp Controleur.lt);
 structure CPN'placeID1037543768 = CPN'Place.MakePlace (structure ims = CPN'Controleur_pims1; val no_of_inst = 1);
 val _ = (CPN'placeID1037543768.init_mark:= (if VerwWeerscen = 0 
then 1`1 else  if
VerwWeerscen = 1
then 1`2 else  if
VerwWeerscen = 2
then 1`4 else  if
VerwWeerscen = 3
then 1`6 else  if
VerwWeerscen = 4
then 1`8 else 1`10); CPN'Place.init_mark_funs::= CPN'placeID1037543768.set_init_mark;CPN'placeID1037543768.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1037543768",{print=CPN'placeID1037543768.print,size=CPN'placeID1037543768.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'VliegtuigOperatorControleur'timed_pims123 = CPN'MakeTreeListPIMS(structure cs = VliegtuigOperatorControleur'timed; val cmp:VliegtuigOperatorControleur'timed * VliegtuigOperatorControleur'timed -> order = CPN'Misc.a_cmp VliegtuigOperatorControleur'timed.lt);
 structure CPN'VliegtuigOperatorControleur'timed_pims123_sims = CPN'MakeTreeSIMS(structure pims = CPN'VliegtuigOperatorControleur'timed_pims123 and Time = CPN'Time; val time = fn (CPN'Time.@(_,CPN't)) => CPN't);
 structure CPN'placeID1037564289 = CPN'Place.MakeTimedPlace (structure sims= CPN'VliegtuigOperatorControleur'timed_pims123_sims and Time = CPN'Time; val no_of_inst= 1 and offset= (maketime "0"));
 val _ = (CPN'placeID1037564289.init_mark:= (map (fn CPN'col => CPN'col@(Option.valOf(CPN'Time.start_time)))([])); CPN'Place.init_mark_funs::= CPN'placeID1037564289.set_init_mark;CPN'placeID1037564289.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1037564289",{print=CPN'placeID1037564289.print,size=CPN'placeID1037564289.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'VliegtuigOperator'timed_pims12 = CPN'MakeTreeListPIMS(structure cs = VliegtuigOperator'timed; val cmp:VliegtuigOperator'timed * VliegtuigOperator'timed -> order = CPN'Misc.a_cmp VliegtuigOperator'timed.lt);
 structure CPN'VliegtuigOperator'timed_pims12_sims = CPN'MakeTreeSIMS(structure pims = CPN'VliegtuigOperator'timed_pims12 and Time = CPN'Time; val time = fn (CPN'Time.@(_,CPN't)) => CPN't);
 structure CPN'placeID1037579526 = CPN'Place.MakeTimedPlace (structure sims= CPN'VliegtuigOperator'timed_pims12_sims and Time = CPN'Time; val no_of_inst= 1 and offset= (maketime "0"));
 val _ = (CPN'placeID1037579526.init_mark:= (map (fn CPN'col => CPN'col@(Option.valOf(CPN'Time.start_time)))([])); CPN'Place.init_mark_funs::= CPN'placeID1037579526.set_init_mark;CPN'placeID1037579526.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1037579526",{print=CPN'placeID1037579526.print,size=CPN'placeID1037579526.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1037582719 = CPN'Place.MakeTimedPort (structure sims = CPN'Vliegtuig'timed_pims123456_sims and Time = CPN'Time;
 val marks= [CPN'placeID1037255700.mark 1]
 and waits= [CPN'placeID1037255700.wait 1]
 and next_times= [(CPN'placeID1037255700.next_time,1)]
 and inits= [(CPN'placeID1037255700.init,1)])
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1037582719",{print=CPN'placeID1037582719.print,size=CPN'placeID1037582719.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1044870187 = CPN'Place.MakePort (structure ims = CPN'Controle_pims12; val markings = [CPN'placeID1043596930.mark 1]);
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1044870187",{print=CPN'placeID1044870187.print,size=CPN'placeID1044870187.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1046710557 = CPN'Place.MakeTimedPlace (structure sims= CPN'VliegtuigDCoordinator'timed_pims12_sims and Time = CPN'Time; val no_of_inst= 1 and offset= (maketime "0"));
 val _ = (CPN'placeID1046710557.init_mark:= (map (fn CPN'col => CPN'col@(Option.valOf(CPN'Time.start_time)))([])); CPN'Place.init_mark_funs::= CPN'placeID1046710557.set_init_mark;CPN'placeID1046710557.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1046710557",{print=CPN'placeID1046710557.print,size=CPN'placeID1046710557.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1047435540 = CPN'Place.MakePlace (structure ims = CPN'DCoordinator_pims1; val no_of_inst = 1);
 val _ = (CPN'placeID1047435540.init_mark:= ([]); CPN'Place.init_mark_funs::= CPN'placeID1047435540.set_init_mark;CPN'placeID1047435540.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1047435540",{print=CPN'placeID1047435540.print,size=CPN'placeID1047435540.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1047451965 = CPN'Place.MakePlace (structure ims = CPN'DCoordinator_pims1; val no_of_inst = 1);
 val _ = (CPN'placeID1047451965.init_mark:= ([]); CPN'Place.init_mark_funs::= CPN'placeID1047451965.set_init_mark;CPN'placeID1047451965.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1047451965",{print=CPN'placeID1047451965.print,size=CPN'placeID1047451965.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1047614114 = CPN'Place.MakePlace (structure ims = CPN'DCoordinator_pims1; val no_of_inst = 1);
 val _ = (CPN'placeID1047614114.init_mark:= ([]); CPN'Place.init_mark_funs::= CPN'placeID1047614114.set_init_mark;CPN'placeID1047614114.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1047614114",{print=CPN'placeID1047614114.print,size=CPN'placeID1047614114.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1037590913 = CPN'Place.MakeTimedPlace (structure sims= CPN'VliegtuigOperatorControleur'timed_pims123_sims and Time = CPN'Time; val no_of_inst= 1 and offset= (maketime "0"));
 val _ = (CPN'placeID1037590913.init_mark:= (map (fn CPN'col => CPN'col@(Option.valOf(CPN'Time.start_time)))([])); CPN'Place.init_mark_funs::= CPN'placeID1037590913.set_init_mark;CPN'placeID1037590913.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1037590913",{print=CPN'placeID1037590913.print,size=CPN'placeID1037590913.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1037590918 = CPN'Place.MakeTimedPort (structure sims = CPN'VliegtuigOperatorControleur'timed_pims123_sims and Time = CPN'Time;
 val marks= [CPN'placeID1037564289.mark 1]
 and waits= [CPN'placeID1037564289.wait 1]
 and next_times= [(CPN'placeID1037564289.next_time,1)]
 and inits= [(CPN'placeID1037564289.init,1)])
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1037590918",{print=CPN'placeID1037590918.print,size=CPN'placeID1037590918.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1037590923 = CPN'Place.MakePort (structure ims = CPN'Controleur_pims1; val markings = [CPN'placeID1037543768.mark 1]);
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1037590923",{print=CPN'placeID1037590923.print,size=CPN'placeID1037590923.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1037600399 = CPN'Place.MakeTimedPort (structure sims = CPN'VliegtuigOperator'timed_pims12_sims and Time = CPN'Time;
 val marks= [CPN'placeID1037579526.mark 1]
 and waits= [CPN'placeID1037579526.wait 1]
 and next_times= [(CPN'placeID1037579526.next_time,1)]
 and inits= [(CPN'placeID1037579526.init,1)])
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1037600399",{print=CPN'placeID1037600399.print,size=CPN'placeID1037600399.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1037603032 = CPN'Place.MakeTimedPort (structure sims = CPN'Vliegtuig'timed_pims123456_sims and Time = CPN'Time;
 val marks= [CPN'placeID1037582719.mark 1]
 and waits= [CPN'placeID1037582719.wait 1]
 and next_times= [(CPN'placeID1037582719.next_time,1)]
 and inits= [(CPN'placeID1037582719.init,1)])
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1037603032",{print=CPN'placeID1037603032.print,size=CPN'placeID1037603032.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1044884445 = CPN'Place.MakePort (structure ims = CPN'Controle_pims12; val markings = [CPN'placeID1044870187.mark 1]);
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1044884445",{print=CPN'placeID1044884445.print,size=CPN'placeID1044884445.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1049388984 = CPN'Place.MakePort (structure ims = CPN'Operator_pims1; val markings = [CPN'placeID1037530306.mark 1]);
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1049388984",{print=CPN'placeID1049388984.print,size=CPN'placeID1049388984.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1038911193 = CPN'Place.MakeTimedPort (structure sims = CPN'VliegtuigOperator'timed_pims12_sims and Time = CPN'Time;
 val marks= [CPN'placeID1037579526.mark 1]
 and waits= [CPN'placeID1037579526.wait 1]
 and next_times= [(CPN'placeID1037579526.next_time,1)]
 and inits= [(CPN'placeID1037579526.init,1)])
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1038911193",{print=CPN'placeID1038911193.print,size=CPN'placeID1038911193.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1038911198 = CPN'Place.MakeTimedPort (structure sims = CPN'VliegtuigOperatorControleur'timed_pims123_sims and Time = CPN'Time;
 val marks= [CPN'placeID1037564289.mark 1]
 and waits= [CPN'placeID1037564289.wait 1]
 and next_times= [(CPN'placeID1037564289.next_time,1)]
 and inits= [(CPN'placeID1037564289.init,1)])
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1038911198",{print=CPN'placeID1038911198.print,size=CPN'placeID1038911198.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1038911203 = CPN'Place.MakePort (structure ims = CPN'Iceman_pims1; val markings = [CPN'placeID1037539323.mark 1]);
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1038911203",{print=CPN'placeID1038911203.print,size=CPN'placeID1038911203.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1038911208 = CPN'Place.MakeTimedPlace (structure sims= CPN'Vliegtuig'timed_pims123456_sims and Time = CPN'Time; val no_of_inst= 1 and offset= (maketime "0"));
 val _ = (CPN'placeID1038911208.init_mark:= (map (fn CPN'col => CPN'col@(Option.valOf(CPN'Time.start_time)))([])); CPN'Place.init_mark_funs::= CPN'placeID1038911208.set_init_mark;CPN'placeID1038911208.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1038911208",{print=CPN'placeID1038911208.print,size=CPN'placeID1038911208.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1038911213 = CPN'Place.MakeTimedPlace (structure sims= CPN'VliegtuigOperator'timed_pims12_sims and Time = CPN'Time; val no_of_inst= 1 and offset= (maketime "0"));
 val _ = (CPN'placeID1038911213.init_mark:= (map (fn CPN'col => CPN'col@(Option.valOf(CPN'Time.start_time)))([])); CPN'Place.init_mark_funs::= CPN'placeID1038911213.set_init_mark;CPN'placeID1038911213.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1038911213",{print=CPN'placeID1038911213.print,size=CPN'placeID1038911213.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1038911218 = CPN'Place.MakePort (structure ims = CPN'Operator_pims1; val markings = [CPN'placeID1037530306.mark 1]);
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1038911218",{print=CPN'placeID1038911218.print,size=CPN'placeID1038911218.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1038952654 = CPN'Place.MakeTimedPort (structure sims = CPN'VliegtuigDCoordinator'timed_pims12_sims and Time = CPN'Time;
 val marks= [CPN'placeID1037477741.mark 1]
 and waits= [CPN'placeID1037477741.wait 1]
 and next_times= [(CPN'placeID1037477741.next_time,1)]
 and inits= [(CPN'placeID1037477741.init,1)])
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1038952654",{print=CPN'placeID1038952654.print,size=CPN'placeID1038952654.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1039111271 = CPN'Place.MakeTimedPort (structure sims = CPN'VliegtuigDCoordinator'timed_pims12_sims and Time = CPN'Time;
 val marks= [CPN'placeID1037477734.mark 1]
 and waits= [CPN'placeID1037477734.wait 1]
 and next_times= [(CPN'placeID1037477734.next_time,1)]
 and inits= [(CPN'placeID1037477734.init,1)])
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1039111271",{print=CPN'placeID1039111271.print,size=CPN'placeID1039111271.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1043482406 = CPN'Place.MakeTimedPlace (structure sims= CPN'VliegtuigOperator'timed_pims12_sims and Time = CPN'Time; val no_of_inst= 1 and offset= (maketime "0"));
 val _ = (CPN'placeID1043482406.init_mark:= (map (fn CPN'col => CPN'col@(Option.valOf(CPN'Time.start_time)))([])); CPN'Place.init_mark_funs::= CPN'placeID1043482406.set_init_mark;CPN'placeID1043482406.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1043482406",{print=CPN'placeID1043482406.print,size=CPN'placeID1043482406.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1047183094 = CPN'Place.MakePort (structure ims = CPN'DCoordinator_pims1; val markings = [CPN'placeID1047435540.mark 1]);
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1047183094",{print=CPN'placeID1047183094.print,size=CPN'placeID1047183094.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1047214690 = CPN'Place.MakePort (structure ims = CPN'DCoordinator_pims1; val markings = [CPN'placeID1047451965.mark 1]);
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1047214690",{print=CPN'placeID1047214690.print,size=CPN'placeID1047214690.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1048683777 = CPN'Place.MakeTimedPort (structure sims = CPN'Vliegtuig'timed_pims123456_sims and Time = CPN'Time;
 val marks= [CPN'placeID1037582719.mark 1]
 and waits= [CPN'placeID1037582719.wait 1]
 and next_times= [(CPN'placeID1037582719.next_time,1)]
 and inits= [(CPN'placeID1037582719.init,1)])
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1048683777",{print=CPN'placeID1048683777.print,size=CPN'placeID1048683777.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1050274958 = CPN'Place.MakeTimedPort (structure sims = CPN'VliegtuigDCoordinator'timed_pims12_sims and Time = CPN'Time;
 val marks= [CPN'placeID1046710557.mark 1]
 and waits= [CPN'placeID1046710557.wait 1]
 and next_times= [(CPN'placeID1046710557.next_time,1)]
 and inits= [(CPN'placeID1046710557.init,1)])
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1050274958",{print=CPN'placeID1050274958.print,size=CPN'placeID1050274958.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1050275904 = CPN'Place.MakePort (structure ims = CPN'DCoordinator_pims1; val markings = [CPN'placeID1047614114.mark 1]);
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1050275904",{print=CPN'placeID1050275904.print,size=CPN'placeID1050275904.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1050276522 = CPN'Place.MakePort (structure ims = CPN'Controleur_pims1; val markings = [CPN'placeID1037543768.mark 1]);
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1050276522",{print=CPN'placeID1050276522.print,size=CPN'placeID1050276522.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'VliegtuigControleur'timed_pims12 = CPN'MakeTreeListPIMS(structure cs = VliegtuigControleur'timed; val cmp:VliegtuigControleur'timed * VliegtuigControleur'timed -> order = CPN'Misc.a_cmp VliegtuigControleur'timed.lt);
 structure CPN'VliegtuigControleur'timed_pims12_sims = CPN'MakeTreeSIMS(structure pims = CPN'VliegtuigControleur'timed_pims12 and Time = CPN'Time; val time = fn (CPN'Time.@(_,CPN't)) => CPN't);
 structure CPN'placeID1050278947 = CPN'Place.MakeTimedPlace (structure sims= CPN'VliegtuigControleur'timed_pims12_sims and Time = CPN'Time; val no_of_inst= 1 and offset= (maketime "0"));
 val _ = (CPN'placeID1050278947.init_mark:= (map (fn CPN'col => CPN'col@(Option.valOf(CPN'Time.start_time)))([])); CPN'Place.init_mark_funs::= CPN'placeID1050278947.set_init_mark;CPN'placeID1050278947.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1050278947",{print=CPN'placeID1050278947.print,size=CPN'placeID1050278947.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1043578483 = CPN'Place.MakePort (structure ims = CPN'DCoordinator_pims1; val markings = [CPN'placeID1047214690.mark 1]);
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1043578483",{print=CPN'placeID1043578483.print,size=CPN'placeID1043578483.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1043578489 = CPN'Place.MakeTimedPort (structure sims = CPN'VliegtuigDCoordinator'timed_pims12_sims and Time = CPN'Time;
 val marks= [CPN'placeID1039111271.mark 1]
 and waits= [CPN'placeID1039111271.wait 1]
 and next_times= [(CPN'placeID1039111271.next_time,1)]
 and inits= [(CPN'placeID1039111271.init,1)])
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1043578489",{print=CPN'placeID1043578489.print,size=CPN'placeID1043578489.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1043578495 = CPN'Place.MakeTimedPort (structure sims = CPN'Vliegtuig'timed_pims123456_sims and Time = CPN'Time;
 val marks= [CPN'placeID1038911208.mark 1]
 and waits= [CPN'placeID1038911208.wait 1]
 and next_times= [(CPN'placeID1038911208.next_time,1)]
 and inits= [(CPN'placeID1038911208.init,1)])
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1043578495",{print=CPN'placeID1043578495.print,size=CPN'placeID1043578495.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1043641221 = CPN'Place.MakeTimedPlace (structure sims= CPN'VliegtuigDCoordinator'timed_pims12_sims and Time = CPN'Time; val no_of_inst= 1 and offset= (maketime "0"));
 val _ = (CPN'placeID1043641221.init_mark:= (map (fn CPN'col => CPN'col@(Option.valOf(CPN'Time.start_time)))([])); CPN'Place.init_mark_funs::= CPN'placeID1043641221.set_init_mark;CPN'placeID1043641221.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1043641221",{print=CPN'placeID1043641221.print,size=CPN'placeID1043641221.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1044947601 = CPN'Place.MakeTimedPort (structure sims = CPN'VliegtuigOperator'timed_pims12_sims and Time = CPN'Time;
 val marks= [CPN'placeID1038911213.mark 1]
 and waits= [CPN'placeID1038911213.wait 1]
 and next_times= [(CPN'placeID1038911213.next_time,1)]
 and inits= [(CPN'placeID1038911213.init,1)])
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1044947601",{print=CPN'placeID1044947601.print,size=CPN'placeID1044947601.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1044958654 = CPN'Place.MakeTimedPlace (structure sims= CPN'VliegtuigDCoordinator'timed_pims12_sims and Time = CPN'Time; val no_of_inst= 1 and offset= (maketime "0"));
 val _ = (CPN'placeID1044958654.init_mark:= (map (fn CPN'col => CPN'col@(Option.valOf(CPN'Time.start_time)))([])); CPN'Place.init_mark_funs::= CPN'placeID1044958654.set_init_mark;CPN'placeID1044958654.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1044958654",{print=CPN'placeID1044958654.print,size=CPN'placeID1044958654.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1045071750 = CPN'Place.MakeTimedPlace (structure sims= CPN'VliegtuigDCoordinator'timed_pims12_sims and Time = CPN'Time; val no_of_inst= 1 and offset= (maketime "0"));
 val _ = (CPN'placeID1045071750.init_mark:= (map (fn CPN'col => CPN'col@(Option.valOf(CPN'Time.start_time)))([])); CPN'Place.init_mark_funs::= CPN'placeID1045071750.set_init_mark;CPN'placeID1045071750.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1045071750",{print=CPN'placeID1045071750.print,size=CPN'placeID1045071750.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1049481985 = CPN'Place.MakeTimedPlace (structure sims= CPN'VliegtuigDCoordinator'timed_pims12_sims and Time = CPN'Time; val no_of_inst= 1 and offset= (maketime "0"));
 val _ = (CPN'placeID1049481985.init_mark:= (map (fn CPN'col => CPN'col@(Option.valOf(CPN'Time.start_time)))([])); CPN'Place.init_mark_funs::= CPN'placeID1049481985.set_init_mark;CPN'placeID1049481985.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1049481985",{print=CPN'placeID1049481985.print,size=CPN'placeID1049481985.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1049482106 = CPN'Place.MakePlace (structure ims = CPN'Controle_pims12; val no_of_inst = 1);
 val _ = (CPN'placeID1049482106.init_mark:= ([]); CPN'Place.init_mark_funs::= CPN'placeID1049482106.set_init_mark;CPN'placeID1049482106.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1049482106",{print=CPN'placeID1049482106.print,size=CPN'placeID1049482106.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1045334250 = CPN'Place.MakeTimedPort (structure sims = CPN'VliegtuigOperator'timed_pims12_sims and Time = CPN'Time;
 val marks= [CPN'placeID1038911213.mark 1]
 and waits= [CPN'placeID1038911213.wait 1]
 and next_times= [(CPN'placeID1038911213.next_time,1)]
 and inits= [(CPN'placeID1038911213.init,1)])
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1045334250",{print=CPN'placeID1045334250.print,size=CPN'placeID1045334250.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1045334257 = CPN'Place.MakeTimedPort (structure sims = CPN'Vliegtuig'timed_pims123456_sims and Time = CPN'Time;
 val marks= [CPN'placeID1038911208.mark 1]
 and waits= [CPN'placeID1038911208.wait 1]
 and next_times= [(CPN'placeID1038911208.next_time,1)]
 and inits= [(CPN'placeID1038911208.init,1)])
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1045334257",{print=CPN'placeID1045334257.print,size=CPN'placeID1045334257.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1045334262 = CPN'Place.MakeTimedPort (structure sims = CPN'VliegtuigOperatorControleur'timed_pims123_sims and Time = CPN'Time;
 val marks= [CPN'placeID1038911198.mark 1]
 and waits= [CPN'placeID1038911198.wait 1]
 and next_times= [(CPN'placeID1038911198.next_time,1)]
 and inits= [(CPN'placeID1038911198.init,1)])
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1045334262",{print=CPN'placeID1045334262.print,size=CPN'placeID1045334262.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1045334268 = CPN'Place.MakePort (structure ims = CPN'Iceman_pims1; val markings = [CPN'placeID1038911203.mark 1]);
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1045334268",{print=CPN'placeID1045334268.print,size=CPN'placeID1045334268.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'VliegtuigOperatorIcemanControleur'timed_pims1234 = CPN'MakeTreeListPIMS(structure cs = VliegtuigOperatorIcemanControleur'timed; val cmp:VliegtuigOperatorIcemanControleur'timed * VliegtuigOperatorIcemanControleur'timed -> order = CPN'Misc.a_cmp VliegtuigOperatorIcemanControleur'timed.lt);
 structure CPN'VliegtuigOperatorIcemanControleur'timed_pims1234_sims = CPN'MakeTreeSIMS(structure pims = CPN'VliegtuigOperatorIcemanControleur'timed_pims1234 and Time = CPN'Time; val time = fn (CPN'Time.@(_,CPN't)) => CPN't);
 structure CPN'placeID1045362911 = CPN'Place.MakeTimedPlace (structure sims= CPN'VliegtuigOperatorIcemanControleur'timed_pims1234_sims and Time = CPN'Time; val no_of_inst= 1 and offset= (maketime "0"));
 val _ = (CPN'placeID1045362911.init_mark:= (map (fn CPN'col => CPN'col@(Option.valOf(CPN'Time.start_time)))([])); CPN'Place.init_mark_funs::= CPN'placeID1045362911.set_init_mark;CPN'placeID1045362911.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1045362911",{print=CPN'placeID1045362911.print,size=CPN'placeID1045362911.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1045430800 = CPN'Place.MakeTimedPlace (structure sims= CPN'VliegtuigOperatorControleur'timed_pims123_sims and Time = CPN'Time; val no_of_inst= 1 and offset= (maketime "0"));
 val _ = (CPN'placeID1045430800.init_mark:= (map (fn CPN'col => CPN'col@(Option.valOf(CPN'Time.start_time)))([])); CPN'Place.init_mark_funs::= CPN'placeID1045430800.set_init_mark;CPN'placeID1045430800.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1045430800",{print=CPN'placeID1045430800.print,size=CPN'placeID1045430800.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1050283044 = CPN'Place.MakeTimedPort (structure sims = CPN'VliegtuigControleur'timed_pims12_sims and Time = CPN'Time;
 val marks= [CPN'placeID1050278947.mark 1]
 and waits= [CPN'placeID1050278947.wait 1]
 and next_times= [(CPN'placeID1050278947.next_time,1)]
 and inits= [(CPN'placeID1050278947.init,1)])
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1050283044",{print=CPN'placeID1050283044.print,size=CPN'placeID1050283044.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1049486734 = CPN'Place.MakePlace (structure ims = CPN'LOnderdelen_pims1; val no_of_inst = 1);
 val _ = (CPN'placeID1049486734.init_mark:= ([]); CPN'Place.init_mark_funs::= CPN'placeID1049486734.set_init_mark;CPN'placeID1049486734.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1049486734",{print=CPN'placeID1049486734.print,size=CPN'placeID1049486734.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1049486744 = CPN'Place.MakeTimedPort (structure sims = CPN'VliegtuigDCoordinator'timed_pims12_sims and Time = CPN'Time;
 val marks= [CPN'placeID1049481985.mark 1]
 and waits= [CPN'placeID1049481985.wait 1]
 and next_times= [(CPN'placeID1049481985.next_time,1)]
 and inits= [(CPN'placeID1049481985.init,1)])
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1049486744",{print=CPN'placeID1049486744.print,size=CPN'placeID1049486744.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1049486749 = CPN'Place.MakePlace (structure ims = CPN'INT_pims1; val no_of_inst = 1);
 val _ = (CPN'placeID1049486749.init_mark:= ([]); CPN'Place.init_mark_funs::= CPN'placeID1049486749.set_init_mark;CPN'placeID1049486749.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1049486749",{print=CPN'placeID1049486749.print,size=CPN'placeID1049486749.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1049486754 = CPN'Place.MakePlace (structure ims = CPN'INT_pims1; val no_of_inst = 1);
 val _ = (CPN'placeID1049486754.init_mark:= (1`1); CPN'Place.init_mark_funs::= CPN'placeID1049486754.set_init_mark;CPN'placeID1049486754.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1049486754",{print=CPN'placeID1049486754.print,size=CPN'placeID1049486754.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1049486759 = CPN'Place.MakePlace (structure ims = CPN'INT_pims1; val no_of_inst = 1);
 val _ = (CPN'placeID1049486759.init_mark:= ([]); CPN'Place.init_mark_funs::= CPN'placeID1049486759.set_init_mark;CPN'placeID1049486759.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1049486759",{print=CPN'placeID1049486759.print,size=CPN'placeID1049486759.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'Onderdelen_pims1 = CPN'MakeTreeListPIMS(structure cs = Onderdelen; val cmp:Onderdelen * Onderdelen -> order = CPN'Misc.a_cmp Onderdelen.lt);
 structure CPN'placeID1049491231 = CPN'Place.MakePlace (structure ims = CPN'Onderdelen_pims1; val no_of_inst = 1);
 val _ = (CPN'placeID1049491231.init_mark:= ([]); CPN'Place.init_mark_funs::= CPN'placeID1049491231.set_init_mark;CPN'placeID1049491231.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1049491231",{print=CPN'placeID1049491231.print,size=CPN'placeID1049491231.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1049491983 = CPN'Place.MakePlace (structure ims = CPN'WelNiet_pims1; val no_of_inst = 1);
 val _ = (CPN'placeID1049491983.init_mark:= ([]); CPN'Place.init_mark_funs::= CPN'placeID1049491983.set_init_mark;CPN'placeID1049491983.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1049491983",{print=CPN'placeID1049491983.print,size=CPN'placeID1049491983.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1049498870 = CPN'Place.MakePlace (structure ims = CPN'LOnderdelen_pims1; val no_of_inst = 1);
 val _ = (CPN'placeID1049498870.init_mark:= (1`[]); CPN'Place.init_mark_funs::= CPN'placeID1049498870.set_init_mark;CPN'placeID1049498870.set_init_mark());
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1049498870",{print=CPN'placeID1049498870.print,size=CPN'placeID1049498870.size});
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
 structure CPN'placeID1049507093 = CPN'Place.MakePort (structure ims = CPN'Controle_pims12; val markings = [CPN'placeID1049482106.mark 1]);
 val _ = CPN'IdTable.insert CPN'Place.instances ("ID1049507093",{print=CPN'placeID1049507093.print,size=CPN'placeID1049507093.size});structure CPN'Totale_doorlooptijd = struct
datatype BindElem = 
Hoofdproces'Startklaar of int * {v: Vliegtuig}  
type markings = unit

type subnet = BindElem
fun get_subnet (CPN'be:BindElem) = CPN'be
fun get_markings() = ()
fun init () = 
  NONE
fun pred (bindelem) = 
let
  fun predBindElem (Hoofdproces'Startklaar (1, {v})) = true
      | predBindElem _ = false
in
  predBindElem bindelem  
end
fun obs (bindelem) = 
let
  fun obsBindElem (Hoofdproces'Startklaar (1, {v})) =
       CurrentTime()- #arrtime(v)
      | obsBindElem _ = ~1
in
  obsBindElem bindelem  
end
fun stop () = 
  NONE
end (* CPN'Totale_doorlooptijd*)

structure Totale_doorlooptijd = CPN'Monitors.CreateUntimedDC (type bindelem = CPN'Totale_doorlooptijd.BindElem and subnet = CPN'Totale_doorlooptijd.subnet and markings = CPN'Totale_doorlooptijd.markings structure SV = CPN'IUSV val pred = CPN'Totale_doorlooptijd.pred and obs =( IntInf.fromInt o CPN'Totale_doorlooptijd.obs):subnet->SV.data and init =( (fn CPN'iopt => case CPN'iopt of NONE => NONE | SOME CPN'i => SOME (IntInf.fromInt CPN'i)) o CPN'Totale_doorlooptijd.init):markings->SV.data option and stop =( (fn CPN'iopt => case CPN'iopt of NONE => NONE | SOME CPN'i => SOME (IntInf.fromInt CPN'i)) o CPN'Totale_doorlooptijd.stop):markings->SV.data option and get_subnet = CPN'Totale_doorlooptijd.get_subnet and get_markings = CPN'Totale_doorlooptijd.get_markings and name = "Totale_doorlooptijd"and montype = CPN'MonitorTable.step_monitor  and updatelogfile = true)
val _ = CPN'Monitors.insert_fun("ID1049335371","Totale_doorlooptijd.init_monitor",Totale_doorlooptijd.init_monitor, CPN'Monitors.sim_init_fun_list)
val _ = if step()=IntInf.fromInt 0 then Totale_doorlooptijd.init_monitor() else ()

val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1037095318 = {v:Vliegtuig}
 type CPN'BRTID1037095318 = {v:Vliegtuig}
 val CPN'BRID1037095318 = ref ({v=Vliegtuig.base}: CPN'BRTID1037095318)
 structure CPN'transitionID1037095318 = (* Hoofdproces'Besluit_deicing_nodig *)
 struct 
 val CPN'id = "ID1037095318"
 val CPN'name = "Hoofdproces'Besluit_deicing_nodig"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["v"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {v: Vliegtuig} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf2() = (CPN'whole_binding := true)
 fun CPN'bf1() = 
 let (* timed B_p with coef 1 *)
 val _ = CPN'Vliegtuig'timed_pims123456.init_res(CPN'placeID1037087828.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@(v,CPN'time1) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'Vliegtuig'timed_pims123456.random_res BindFailureGenAll (CPN'placeID1037087828.mark CPN'inst)
 | _ => CPN'Vliegtuig'timed_pims123456.random_res BindFatalFailure (CPN'placeID1037087828.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'Vliegtuig'timed_pims123456.res_col(CPN'placeID1037087828.mark CPN'inst, CPN'Time.@(v,CPN'time1))
 | _ => ()
 in (
 (* T_g *)
 (if (CheckTijdEnDeicing
(#arrtime v, #x v) andalso
#vliegID v>0) then
 (CPN'bh1::= {v=v};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 else raise BindFailure)
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({v}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'Vliegtuig'timed_pims123456.delete(CPN'placeID1037087828.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1037095318 CPN'placeID1037087828") (CPN'placeID1037087828.mark CPN'inst,CPN'Vliegtuig'timed_pims123456.collect,CPN'Vliegtuig'timed_pims123456.cmp,v, time())));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val CPN'trans_delay = CPN'Time.fromInt(0)
 val _ = (CPN'Vliegtuig'timed_pims123456_sims.addto (CPN'placeID1037095434.wait CPN'inst,CPN'TMS.@++([(v)],CPN'trans_delay)));
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {v=v}
 val _ = (CPN'BRID1037095318:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - v = ",Vliegtuig.mkstr v]
else nil)
 end
 fun CPN'pick [CPN'i1] = (CPN'nth(!CPN'bh1,CPN'i1))
 fun CPN'bindings_as_strings() = [(["v"],CPN'map (fn {v} => [Vliegtuig.mkstr v]) (!CPN'bh1))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_timed_place(1 <= CPN'placeID1037087828.init(CPN'inst),CPN'placeID1037087828.next_time (CPN'inst),
 (true,CPN'Sim.is_disabled)))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0) = (CPN'e0)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(!CPN'bh1))
 end

 end (* end CPN'transitionID1037095318 *) 
 val _ = CPN'Sim.add_be("ID1037095318",CPN'transitionID1037095318.CPN'bind_exe,CPN'transitionID1037095318.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1037885321 = {v:Vliegtuig}*{s:Snowdesk}*{c:DCoordinator}
 type CPN'BRTID1037885321 = {v:Vliegtuig,s:Snowdesk,c:DCoordinator}
 val CPN'BRID1037885321 = ref ({v=Vliegtuig.base,s=Snowdesk.base,c=DCoordinator.base}: CPN'BRTID1037885321)
 structure CPN'transitionID1037885321 = (* Hoofdproces'Status_vrijgeven *)
 struct 
 val CPN'id = "ID1037885321"
 val CPN'name = "Hoofdproces'Status_vrijgeven"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["v","s","c"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {v: Vliegtuig} list)
 val CPN'bh2 = ref(nil: {s: Snowdesk} list)
 val CPN'bh3 = ref(nil: {c: DCoordinator} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf4() = (CPN'whole_binding := true)
 fun CPN'bf3() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'DCoordinator_pims1.init_res(CPN'placeID1037135843.mark CPN'inst)
 fun CPN'bf() = 
let
 val c = case CPN'mode of 
CPN'Sim.all_enabled => CPN'DCoordinator_pims1.random_res BindFailureGenAll (CPN'placeID1037135843.mark CPN'inst)
 | _ => CPN'DCoordinator_pims1.random_res BindFatalFailure (CPN'placeID1037135843.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'DCoordinator_pims1.res_col(CPN'placeID1037135843.mark CPN'inst,c)
 | _ => ()
 in
 (
 (CPN'bh3::= {c=c};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf4())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 fun CPN'bf2() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'Snowdesk_pims1.init_res(CPN'placeID1037099424.mark CPN'inst)
 fun CPN'bf() = 
let
 val s = case CPN'mode of 
CPN'Sim.all_enabled => CPN'Snowdesk_pims1.random_res BindFailureGenAll (CPN'placeID1037099424.mark CPN'inst)
 | _ => CPN'Snowdesk_pims1.random_res BindFatalFailure (CPN'placeID1037099424.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'Snowdesk_pims1.res_col(CPN'placeID1037099424.mark CPN'inst,s)
 | _ => ()
 in
 (
 (CPN'bh2::= {s=s};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf3())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 fun CPN'bf1() = 
 let (* timed B_p with coef 1 *)
 val _ = CPN'Vliegtuig'timed_pims123456.init_res(CPN'placeID1037255700.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@(v,CPN'time3) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'Vliegtuig'timed_pims123456.random_res BindFailureGenAll (CPN'placeID1037255700.mark CPN'inst)
 | _ => CPN'Vliegtuig'timed_pims123456.random_res BindFatalFailure (CPN'placeID1037255700.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'Vliegtuig'timed_pims123456.res_col(CPN'placeID1037255700.mark CPN'inst, CPN'Time.@(v,CPN'time3))
 | _ => ()
 in (
 (CPN'bh1::= {v=v};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
CPN'bh2:=nil;
CPN'bh3:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ();
CPN'bf2() handle BoundGroup => ();
CPN'bf3() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ();
CPN'bf3() handle BindFailureGenAll => ();
CPN'bf4() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({v},{s},{c}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'Vliegtuig'timed_pims123456.delete(CPN'placeID1037255700.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1037885321 CPN'placeID1037255700") (CPN'placeID1037255700.mark CPN'inst,CPN'Vliegtuig'timed_pims123456.collect,CPN'Vliegtuig'timed_pims123456.cmp,v, time()));
 CPN'Snowdesk_pims1.delete(CPN'placeID1037099424.mark CPN'inst,s);
 CPN'DCoordinator_pims1.delete(CPN'placeID1037135843.mark CPN'inst,c));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val CPN'trans_delay = CPN'Time.fromInt(0)
 val _ = (CPN'DCoordinator_pims1.insert(CPN'placeID1037135843.mark CPN'inst,c);
 CPN'Vliegtuig'timed_pims123456_sims.addto (CPN'placeID1038032796.wait CPN'inst,CPN'TMS.@++([(v)],CPN'trans_delay));
 CPN'Snowdesk_pims1.insert(CPN'placeID1037099424.mark CPN'inst,s));
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {v=v,s=s,c=c}
 val _ = (CPN'BRID1037885321:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - v = ",Vliegtuig.mkstr v,"\n - s = ",Snowdesk.mkstr s,"\n - c = ",DCoordinator.mkstr c]
else nil)
 end
 fun CPN'pick [CPN'i1,CPN'i2,CPN'i3] = (CPN'nth(!CPN'bh1,CPN'i1),CPN'nth(!CPN'bh2,CPN'i2),CPN'nth(!CPN'bh3,CPN'i3))
 fun CPN'bindings_as_strings() = [(["v"],CPN'map (fn {v} => [Vliegtuig.mkstr v]) (!CPN'bh1)),(["s"],CPN'map (fn {s} => [Snowdesk.mkstr s]) (!CPN'bh2)),(["c"],CPN'map (fn {c} => [DCoordinator.mkstr c]) (!CPN'bh3))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_timed_place(1 <= CPN'placeID1037255700.init(CPN'inst),CPN'placeID1037255700.next_time (CPN'inst),
 CPN'Sim.each_place(1 <= CPN'placeID1037099424.init(CPN'inst),
 CPN'Sim.each_place(1 <= CPN'placeID1037135843.init(CPN'inst),
 (true,CPN'Sim.is_disabled)))))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1),CPN'hd(!CPN'bh2),CPN'hd(!CPN'bh3)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0,(CPN'e1,CPN'e2)) = (CPN'e0,CPN'e1,CPN'e2)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(expand1(!CPN'bh1,expand1(!CPN'bh2,!CPN'bh3))))
 end

 end (* end CPN'transitionID1037885321 *) 
 val _ = CPN'Sim.add_be("ID1037885321",CPN'transitionID1037885321.CPN'bind_exe,CPN'transitionID1037885321.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1038036456 = {v:Vliegtuig}
 type CPN'BRTID1038036456 = {v:Vliegtuig}
 val CPN'BRID1038036456 = ref ({v=Vliegtuig.base}: CPN'BRTID1038036456)
 structure CPN'transitionID1038036456 = (* Hoofdproces'Binnen_half_uur *)
 struct 
 val CPN'id = "ID1038036456"
 val CPN'name = "Hoofdproces'Binnen_half_uur"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["v"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {v: Vliegtuig} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf2() = (CPN'whole_binding := true)
 fun CPN'bf1() = 
 let (* timed B_p with coef 1 *)
 val _ = CPN'Vliegtuig'timed_pims123456.init_res(CPN'placeID1038032796.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@(v,CPN'time7) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'Vliegtuig'timed_pims123456.random_res BindFailureGenAll (CPN'placeID1038032796.mark CPN'inst)
 | _ => CPN'Vliegtuig'timed_pims123456.random_res BindFatalFailure (CPN'placeID1038032796.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'Vliegtuig'timed_pims123456.res_col(CPN'placeID1038032796.mark CPN'inst, CPN'Time.@(v,CPN'time7))
 | _ => ()
 in (
 (* T_g *)
 (if (#tijdklaar v +30 >= 
#deptime v) then
 (CPN'bh1::= {v=v};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 else raise BindFailure)
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({v}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'Vliegtuig'timed_pims123456.delete(CPN'placeID1038032796.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1038036456 CPN'placeID1038032796") (CPN'placeID1038032796.mark CPN'inst,CPN'Vliegtuig'timed_pims123456.collect,CPN'Vliegtuig'timed_pims123456.cmp,v, time())));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val CPN'trans_delay = CPN'Time.fromInt(0)
 val _ = (CPN'Vliegtuig'timed_pims123456_sims.addto (CPN'placeID1049328673.wait CPN'inst,CPN'TMS.@++([(v)],CPN'trans_delay)));
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {v=v}
 val _ = (CPN'BRID1038036456:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - v = ",Vliegtuig.mkstr v]
else nil)
 end
 fun CPN'pick [CPN'i1] = (CPN'nth(!CPN'bh1,CPN'i1))
 fun CPN'bindings_as_strings() = [(["v"],CPN'map (fn {v} => [Vliegtuig.mkstr v]) (!CPN'bh1))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_timed_place(1 <= CPN'placeID1038032796.init(CPN'inst),CPN'placeID1038032796.next_time (CPN'inst),
 (true,CPN'Sim.is_disabled)))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0) = (CPN'e0)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(!CPN'bh1))
 end

 end (* end CPN'transitionID1038036456 *) 
 val _ = CPN'Sim.add_be("ID1038036456",CPN'transitionID1038036456.CPN'bind_exe,CPN'transitionID1038036456.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1038040128 = {v:Vliegtuig}
 type CPN'BRTID1038040128 = {v:Vliegtuig}
 val CPN'BRID1038040128 = ref ({v=Vliegtuig.base}: CPN'BRTID1038040128)
 structure CPN'transitionID1038040128 = (* Hoofdproces'Buiten_half_uur *)
 struct 
 val CPN'id = "ID1038040128"
 val CPN'name = "Hoofdproces'Buiten_half_uur"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["v"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {v: Vliegtuig} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf2() = (CPN'whole_binding := true)
 fun CPN'bf1() = 
 let (* timed B_p with coef 1 *)
 val _ = CPN'Vliegtuig'timed_pims123456.init_res(CPN'placeID1038032796.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@(v,CPN'time9) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'Vliegtuig'timed_pims123456.random_res BindFailureGenAll (CPN'placeID1038032796.mark CPN'inst)
 | _ => CPN'Vliegtuig'timed_pims123456.random_res BindFatalFailure (CPN'placeID1038032796.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'Vliegtuig'timed_pims123456.res_col(CPN'placeID1038032796.mark CPN'inst, CPN'Time.@(v,CPN'time9))
 | _ => ()
 in (
 (* T_g *)
 (if (#tijdklaar v +30 < 
#deptime v) then
 (CPN'bh1::= {v=v};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 else raise BindFailure)
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({v}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'Vliegtuig'timed_pims123456.delete(CPN'placeID1038032796.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1038040128 CPN'placeID1038032796") (CPN'placeID1038032796.mark CPN'inst,CPN'Vliegtuig'timed_pims123456.collect,CPN'Vliegtuig'timed_pims123456.cmp,v, time())));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val CPN'trans_delay = CPN'Time.fromInt(7)
 val _ = (CPN'Vliegtuig'timed_pims123456_sims.addto (CPN'placeID1049297444.wait CPN'inst,CPN'TMS.@++([(v)],CPN'trans_delay)));
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {v=v}
 val _ = (CPN'BRID1038040128:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - v = ",Vliegtuig.mkstr v]
else nil)
 end
 fun CPN'pick [CPN'i1] = (CPN'nth(!CPN'bh1,CPN'i1))
 fun CPN'bindings_as_strings() = [(["v"],CPN'map (fn {v} => [Vliegtuig.mkstr v]) (!CPN'bh1))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_timed_place(1 <= CPN'placeID1038032796.init(CPN'inst),CPN'placeID1038032796.next_time (CPN'inst),
 (true,CPN'Sim.is_disabled)))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0) = (CPN'e0)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(!CPN'bh1))
 end

 end (* end CPN'transitionID1038040128 *) 
 val _ = CPN'Sim.add_be("ID1038040128",CPN'transitionID1038040128.CPN'bind_exe,CPN'transitionID1038040128.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1049300246 = {v:Vliegtuig}
 type CPN'BRTID1049300246 = {v:Vliegtuig}
 val CPN'BRID1049300246 = ref ({v=Vliegtuig.base}: CPN'BRTID1049300246)
 structure CPN'transitionID1049300246 = (* Hoofdproces'Klaar_voor_vertrek *)
 struct 
 val CPN'id = "ID1049300246"
 val CPN'name = "Hoofdproces'Klaar_voor_vertrek"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["v"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {v: Vliegtuig} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf2() = (CPN'whole_binding := true)
 fun CPN'bf1() = 
 let (* timed B_p with coef 1 *)
 val _ = CPN'Vliegtuig'timed_pims123456.init_res(CPN'placeID1049297444.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@(v,CPN'time11) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'Vliegtuig'timed_pims123456.random_res BindFailureGenAll (CPN'placeID1049297444.mark CPN'inst)
 | _ => CPN'Vliegtuig'timed_pims123456.random_res BindFatalFailure (CPN'placeID1049297444.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'Vliegtuig'timed_pims123456.res_col(CPN'placeID1049297444.mark CPN'inst, CPN'Time.@(v,CPN'time11))
 | _ => ()
 in (
 (CPN'bh1::= {v=v};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({v}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'Vliegtuig'timed_pims123456.delete(CPN'placeID1049297444.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1049300246 CPN'placeID1049297444") (CPN'placeID1049297444.mark CPN'inst,CPN'Vliegtuig'timed_pims123456.collect,CPN'Vliegtuig'timed_pims123456.cmp,v, time())));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val CPN'trans_delay = CPN'Time.fromInt(0)
 val _ = (CPN'Vliegtuig'timed_pims123456_sims.addto (CPN'placeID1049328673.wait CPN'inst,CPN'TMS.@++([(v)],CPN'trans_delay)));
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {v=v}
 val _ = (CPN'BRID1049300246:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - v = ",Vliegtuig.mkstr v]
else nil)
 end
 fun CPN'pick [CPN'i1] = (CPN'nth(!CPN'bh1,CPN'i1))
 fun CPN'bindings_as_strings() = [(["v"],CPN'map (fn {v} => [Vliegtuig.mkstr v]) (!CPN'bh1))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_timed_place(1 <= CPN'placeID1049297444.init(CPN'inst),CPN'placeID1049297444.next_time (CPN'inst),
 (true,CPN'Sim.is_disabled)))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0) = (CPN'e0)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(!CPN'bh1))
 end

 end (* end CPN'transitionID1049300246 *) 
 val _ = CPN'Sim.add_be("ID1049300246",CPN'transitionID1049300246.CPN'bind_exe,CPN'transitionID1049300246.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1049318098 = {v:Vliegtuig,con:Controle}
 type CPN'BRTID1049318098 = {v:Vliegtuig,con:Controle}
 val CPN'BRID1049318098 = ref ({v=Vliegtuig.base,con=Controle.base}: CPN'BRTID1049318098)
 structure CPN'transitionID1049318098 = (* Hoofdproces'Besluit_deicing_niet_nodig *)
 struct 
 val CPN'id = "ID1049318098"
 val CPN'name = "Hoofdproces'Besluit_deicing_niet_nodig"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["v","con"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {v: Vliegtuig,con: Controle} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf2() = (CPN'whole_binding := true)
 fun CPN'bf1() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'Controle_pims12.init_res(CPN'placeID1043596930.mark CPN'inst)
 fun CPN'bf() = 
let
 val con = case CPN'mode of 
CPN'Sim.all_enabled => CPN'Controle_pims12.random_res BindFailureGenAll (CPN'placeID1043596930.mark CPN'inst)
 | _ => CPN'Controle_pims12.random_res BindFatalFailure (CPN'placeID1043596930.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'Controle_pims12.res_col(CPN'placeID1043596930.mark CPN'inst,con)
 | _ => ()
 in
 (
 let (* timed B_p with coef 1 *)
 val _ = CPN'Vliegtuig'timed_pims123456.init_res(CPN'placeID1037087828.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@(v,CPN'time14) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'Vliegtuig'timed_pims123456.random_res BindFailure (CPN'placeID1037087828.mark CPN'inst)
 | _ => CPN'Vliegtuig'timed_pims123456.random_res BindFailure (CPN'placeID1037087828.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'Vliegtuig'timed_pims123456.res_col(CPN'placeID1037087828.mark CPN'inst, CPN'Time.@(v,CPN'time14))
 | _ => ()
 in (
 (* T_g *)
 (if (CheckTijdEnGeenDeicing(#arrtime v, #x v)
andalso #vliegID v>0 andalso #vliegID v = #id con) then
 (CPN'bh1::= {v=v,con=con};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 else raise BindFailure)
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({v,con}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'Vliegtuig'timed_pims123456.delete(CPN'placeID1037087828.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1049318098 CPN'placeID1037087828") (CPN'placeID1037087828.mark CPN'inst,CPN'Vliegtuig'timed_pims123456.collect,CPN'Vliegtuig'timed_pims123456.cmp,v, time()));
 CPN'Controle_pims12.delete(CPN'placeID1043596930.mark CPN'inst,con));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val CPN'trans_delay = CPN'Time.fromInt(0)
 val _ = (CPN'Vliegtuig'timed_pims123456_sims.addto (CPN'placeID1038043812.wait CPN'inst,CPN'TMS.@++([(v)],CPN'trans_delay)));
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {v=v,con=con}
 val _ = (CPN'BRID1049318098:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - v = ",Vliegtuig.mkstr v,"\n - con = ",Controle.mkstr con]
else nil)
 end
 fun CPN'pick [CPN'i1] = (CPN'nth(!CPN'bh1,CPN'i1))
 fun CPN'bindings_as_strings() = [(["v","con"],CPN'map (fn {v,con} => [Vliegtuig.mkstr v,Controle.mkstr con]) (!CPN'bh1))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_timed_place(1 <= CPN'placeID1037087828.init(CPN'inst),CPN'placeID1037087828.next_time (CPN'inst),
 CPN'Sim.each_place(1 <= CPN'placeID1043596930.init(CPN'inst),
 (true,CPN'Sim.is_disabled))))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0) = (CPN'e0)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(!CPN'bh1))
 end

 end (* end CPN'transitionID1049318098 *) 
 val _ = CPN'Sim.add_be("ID1049318098",CPN'transitionID1049318098.CPN'bind_exe,CPN'transitionID1049318098.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1049329450 = {v:Vliegtuig}
 type CPN'BRTID1049329450 = {v:Vliegtuig}
 val CPN'BRID1049329450 = ref ({v=Vliegtuig.base}: CPN'BRTID1049329450)
 structure CPN'transitionID1049329450 = (* Hoofdproces'Startklaar *)
 struct 
 val CPN'id = "ID1049329450"
 val CPN'name = "Hoofdproces'Startklaar"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["v"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {v: Vliegtuig} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf2() = (CPN'whole_binding := true)
 fun CPN'bf1() = 
 let (* timed B_p with coef 1 *)
 val _ = CPN'Vliegtuig'timed_pims123456.init_res(CPN'placeID1049328673.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@(v,CPN'time16) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'Vliegtuig'timed_pims123456.random_res BindFailureGenAll (CPN'placeID1049328673.mark CPN'inst)
 | _ => CPN'Vliegtuig'timed_pims123456.random_res BindFatalFailure (CPN'placeID1049328673.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'Vliegtuig'timed_pims123456.res_col(CPN'placeID1049328673.mark CPN'inst, CPN'Time.@(v,CPN'time16))
 | _ => ()
 in (
 (* T_g *)
 (if (CurrentTime()>= 
#deptime v) then
 (CPN'bh1::= {v=v};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 else raise BindFailure)
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({v}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'Vliegtuig'timed_pims123456.delete(CPN'placeID1049328673.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1049329450 CPN'placeID1049328673") (CPN'placeID1049328673.mark CPN'inst,CPN'Vliegtuig'timed_pims123456.collect,CPN'Vliegtuig'timed_pims123456.cmp,v, time())));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val CPN'trans_delay = CPN'Time.fromInt(0)
 val _ = (CPN'Vliegtuig'timed_pims123456_sims.addto (CPN'placeID1038043812.wait CPN'inst,CPN'TMS.@++([(v)],CPN'trans_delay)));
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {v=v}
 val _ = (CPN'BRID1049329450:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - v = ",Vliegtuig.mkstr v]
else nil)
 end
 fun CPN'pick [CPN'i1] = (CPN'nth(!CPN'bh1,CPN'i1))
 fun CPN'bindings_as_strings() = [(["v"],CPN'map (fn {v} => [Vliegtuig.mkstr v]) (!CPN'bh1))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_timed_place(1 <= CPN'placeID1049328673.init(CPN'inst),CPN'placeID1049328673.next_time (CPN'inst),
 (true,CPN'Sim.is_disabled)))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0) = (CPN'e0)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(!CPN'bh1))
 end

 end (* end CPN'transitionID1049329450 *) 
 val _ = CPN'Sim.add_be("ID1049329450",CPN'transitionID1049329450.CPN'bind_exe,CPN'transitionID1049329450.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1037090529 = {h:INT}
 type CPN'BRTID1037090529 = {h:INT}
 val CPN'BRID1037090529 = ref ({h=INT.base}: CPN'BRTID1037090529)
 structure CPN'transitionID1037090529 = (* GenereerVliegtuig'Genereer_vliegtuig *)
 struct 
 val CPN'id = "ID1037090529"
 val CPN'name = "GenereerVliegtuig'Genereer_vliegtuig"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["h"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {h: INT} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf2() = (CPN'whole_binding := true)
 fun CPN'bf1() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'INT_pims1.init_res(CPN'placeID1049307988.mark CPN'inst)
 fun CPN'bf() = 
let
 val h = case CPN'mode of 
CPN'Sim.all_enabled => CPN'INT_pims1.random_res BindFailureGenAll (CPN'placeID1049307988.mark CPN'inst)
 | _ => CPN'INT_pims1.random_res BindFatalFailure (CPN'placeID1049307988.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'INT_pims1.res_col(CPN'placeID1049307988.mark CPN'inst,h)
 | _ => ()
 in
 (
 (* T_g *)
 (if (h<=1080) then
 (CPN'bh1::= {h=h};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 else raise BindFailure)
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({h}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'INT_pims1.delete(CPN'placeID1049307988.mark CPN'inst,h));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val CPN'trans_delay = CPN'Time.fromInt(0)
 val _ = (CPN'Vliegtuig'timed_pims123456_sims.addto (CPN'placeID1037090534.wait CPN'inst,CPN'TMS.@++(GenVliegtuig(1,h,0,0,[],0), CPN'trans_delay));
 CPN'INT_pims1.insert(CPN'placeID1049307988.mark CPN'inst,h+round(exponential
(AankomstTijdWaarde))));
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {h=h}
 val _ = (CPN'BRID1037090529:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - h = ",INT.mkstr h]
else nil)
 end
 fun CPN'pick [CPN'i1] = (CPN'nth(!CPN'bh1,CPN'i1))
 fun CPN'bindings_as_strings() = [(["h"],CPN'map (fn {h} => [INT.mkstr h]) (!CPN'bh1))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_place(1 <= CPN'placeID1049307988.init(CPN'inst),
 (true,CPN'Sim.is_disabled)))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0) = (CPN'e0)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(!CPN'bh1))
 end

 end (* end CPN'transitionID1037090529 *) 
 val _ = CPN'Sim.add_be("ID1037090529",CPN'transitionID1037090529.CPN'bind_exe,CPN'transitionID1037090529.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1040161361 = {i:INT}*{londerdelen:LOnderdelen}*{h:INT}*{v:Vliegtuig,tien:INT}
 type CPN'BRTID1040161361 = {i:INT,londerdelen:LOnderdelen,h:INT,v:Vliegtuig,tien:INT,v1:Vliegtuig}
 val CPN'BRID1040161361 = ref ({i=INT.base,londerdelen=LOnderdelen.base,h=INT.base,v=Vliegtuig.base,tien=INT.base,v1=Vliegtuig.base}: CPN'BRTID1040161361)
 val CPN'code_actionID1040161361 = CPN'Sim.code_action (fn CPN'inst => fn (v: Vliegtuig) => let
 in 
((AankomstDeicing(v)))
 end)
 structure CPN'transitionID1040161361 = (* GenereerVliegtuig'Update_Time *)
 struct 
 val CPN'id = "ID1040161361"
 val CPN'name = "GenereerVliegtuig'Update_Time"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["i","londerdelen","h","v","tien"]
 val CPN'output_vars = ["v1"]
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref nil
 val CPN'bh2 = ref(nil: {i: INT} list)
 val CPN'bh3 = ref(nil: {londerdelen: LOnderdelen} list)
 val CPN'bh4 = ref(nil: {h: INT} list)
 val CPN'bh5 = ref(nil: {v: Vliegtuig,tien: INT} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf6() = (CPN'whole_binding := true)
 fun CPN'bf5() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'INT_pims1.init_res(CPN'placeID1044341038.mark CPN'inst)
 fun CPN'bf() = 
let
 val tien = case CPN'mode of 
CPN'Sim.all_enabled => CPN'INT_pims1.random_res BindFailureGenAll (CPN'placeID1044341038.mark CPN'inst)
 | _ => CPN'INT_pims1.random_res BindFatalFailure (CPN'placeID1044341038.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'INT_pims1.res_col(CPN'placeID1044341038.mark CPN'inst,tien)
 | _ => ()
 in
 (
 let (* timed B_p with coef 1 *)
 val _ = CPN'Vliegtuig'timed_pims123456.init_res(CPN'placeID1037090534.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@(v,CPN'time24) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'Vliegtuig'timed_pims123456.random_res BindFailure (CPN'placeID1037090534.mark CPN'inst)
 | _ => CPN'Vliegtuig'timed_pims123456.random_res BindFailure (CPN'placeID1037090534.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'Vliegtuig'timed_pims123456.res_col(CPN'placeID1037090534.mark CPN'inst, CPN'Time.@(v,CPN'time24))
 | _ => ()
 in (
 (* T_g *)
 (if (#vliegID v=0 andalso tien>0) then
 (CPN'bh5::= {v=v,tien=tien};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf6())
 else raise BindFailure)
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 fun CPN'bf4() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'INT_pims1.init_res(CPN'placeID1040165399.mark CPN'inst)
 fun CPN'bf() = 
let
 val h = case CPN'mode of 
CPN'Sim.all_enabled => CPN'INT_pims1.random_res BindFailureGenAll (CPN'placeID1040165399.mark CPN'inst)
 | _ => CPN'INT_pims1.random_res BindFatalFailure (CPN'placeID1040165399.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'INT_pims1.res_col(CPN'placeID1040165399.mark CPN'inst,h)
 | _ => ()
 in
 (
 (CPN'bh4::= {h=h};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf5())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 fun CPN'bf3() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'LOnderdelen_pims1.init_res(CPN'placeID1043899894.mark CPN'inst)
 fun CPN'bf() = 
let
 val londerdelen = case CPN'mode of 
CPN'Sim.all_enabled => CPN'LOnderdelen_pims1.random_res BindFailureGenAll (CPN'placeID1043899894.mark CPN'inst)
 | _ => CPN'LOnderdelen_pims1.random_res BindFatalFailure (CPN'placeID1043899894.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'LOnderdelen_pims1.res_col(CPN'placeID1043899894.mark CPN'inst,londerdelen)
 | _ => ()
 in
 (
 (CPN'bh3::= {londerdelen=londerdelen};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf4())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 fun CPN'bf2() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'INT_pims1.init_res(CPN'placeID1043868911.mark CPN'inst)
 fun CPN'bf() = 
let
 val i = case CPN'mode of 
CPN'Sim.all_enabled => CPN'INT_pims1.random_res BindFailureGenAll (CPN'placeID1043868911.mark CPN'inst)
 | _ => CPN'INT_pims1.random_res BindFatalFailure (CPN'placeID1043868911.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'INT_pims1.res_col(CPN'placeID1043868911.mark CPN'inst,i)
 | _ => ()
 in
 (
 (CPN'bh2::= {i=i};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf3())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 fun CPN'bf1() = 
 (* untimed T_a token exp coef 1 *)
 (if CPN'LInt_pims1.member (!(CPN'placeID1044083806.mark CPN'inst),[]) then
 (case CPN'mode of 
CPN'Sim.bind _=> raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailureGenAll
 | _ => CPN'bf2())
 else raise BindFatalFailure)
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
CPN'bh2:=nil;
CPN'bh3:=nil;
CPN'bh4:=nil;
CPN'bh5:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ();
CPN'bf2() handle BoundGroup => ();
CPN'bf3() handle BoundGroup => ();
CPN'bf4() handle BoundGroup => ();
CPN'bf5() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ();
CPN'bf3() handle BindFailureGenAll => ();
CPN'bf4() handle BindFailureGenAll => ();
CPN'bf5() handle BindFailureGenAll => ();
CPN'bf6() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({i},{londerdelen},{h},{v,tien}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val (v1) = CPN'code_actionID1040161361 CPN'inst (v)
 val _ = (CPN'INT_pims1.delete(CPN'placeID1043868911.mark CPN'inst,i);
 CPN'Vliegtuig'timed_pims123456.delete(CPN'placeID1037090534.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1040161361 CPN'placeID1037090534") (CPN'placeID1037090534.mark CPN'inst,CPN'Vliegtuig'timed_pims123456.collect,CPN'Vliegtuig'timed_pims123456.cmp,v, time()));
 CPN'INT_pims1.delete(CPN'placeID1044341038.mark CPN'inst,tien);
 CPN'INT_pims1.delete(CPN'placeID1040165399.mark CPN'inst,h);
 CPN'LOnderdelen_pims1.delete(CPN'placeID1043899894.mark CPN'inst,londerdelen);
 CPN'LInt_pims1.delete(CPN'placeID1044083806.mark CPN'inst,[]));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val CPN'trans_delay = CPN'Time.fromInt((#arrtime v1))
 val _ = (CPN'LInt_pims1.insert(CPN'placeID1044083806.mark CPN'inst,[]);
 CPN'Vliegtuig'timed_pims123456_sims.addto (CPN'placeID1037090534.wait CPN'inst,CPN'TMS.@++([(GenID(v1,h,londerdelen))],CPN'trans_delay));
 CPN'INT_pims1.insert(CPN'placeID1043868911.mark CPN'inst,1);
 CPN'LOnderdelen_pims1.insert(CPN'placeID1043899894.mark CPN'inst,[]);
 CPN'INT_pims1.insert(CPN'placeID1044341038.mark CPN'inst,0);
 CPN'Controle_pims12.insert(CPN'placeID1043587409.mark CPN'inst,GenControleur(h,londerdelen));
 CPN'INT_pims1.insert(CPN'placeID1040165399.mark CPN'inst,h+1));
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {i=i,londerdelen=londerdelen,h=h,v=v,tien=tien,v1=v1}
 val _ = (CPN'BRID1040161361:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - i = ",INT.mkstr i,"\n - londerdelen = ",LOnderdelen.mkstr londerdelen,"\n - h = ",INT.mkstr h,"\n - v = ",Vliegtuig.mkstr v,"\n - tien = ",INT.mkstr tien,"\n - v1 = ",Vliegtuig.mkstr v1]
else nil)
 end
 fun CPN'pick [CPN'i2,CPN'i3,CPN'i4,CPN'i5] = (CPN'nth(!CPN'bh2,CPN'i2),CPN'nth(!CPN'bh3,CPN'i3),CPN'nth(!CPN'bh4,CPN'i4),CPN'nth(!CPN'bh5,CPN'i5))
 fun CPN'bindings_as_strings() = [(["i"],CPN'map (fn {i} => [INT.mkstr i]) (!CPN'bh2)),(["londerdelen"],CPN'map (fn {londerdelen} => [LOnderdelen.mkstr londerdelen]) (!CPN'bh3)),(["h"],CPN'map (fn {h} => [INT.mkstr h]) (!CPN'bh4)),(["v","tien"],CPN'map (fn {v,tien} => [Vliegtuig.mkstr v,INT.mkstr tien]) (!CPN'bh5))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_place(1 <= CPN'placeID1043868911.init(CPN'inst),
 CPN'Sim.each_timed_place(1 <= CPN'placeID1037090534.init(CPN'inst),CPN'placeID1037090534.next_time (CPN'inst),
 CPN'Sim.each_place(1 <= CPN'placeID1044341038.init(CPN'inst),
 CPN'Sim.each_place(1 <= CPN'placeID1040165399.init(CPN'inst),
 CPN'Sim.each_place(1 <= CPN'placeID1043899894.init(CPN'inst),
 CPN'Sim.each_place(1 <= CPN'placeID1044083806.init(CPN'inst),
 (true,CPN'Sim.is_disabled))))))))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh2),CPN'hd(!CPN'bh3),CPN'hd(!CPN'bh4),CPN'hd(!CPN'bh5)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0,(CPN'e1,(CPN'e2,CPN'e3))) = (CPN'e0,CPN'e1,CPN'e2,CPN'e3)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(expand1(!CPN'bh2,expand1(!CPN'bh3,expand1(!CPN'bh4,!CPN'bh5)))))
 end

 end (* end CPN'transitionID1040161361 *) 
 val _ = CPN'Sim.add_be("ID1040161361",CPN'transitionID1040161361.CPN'bind_exe,CPN'transitionID1040161361.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1043871671 = {i:INT,lint:LInt}*{wn:WelNiet}*{londerdelen:LOnderdelen}
 type CPN'BRTID1043871671 = {i:INT,lint:LInt,wn:WelNiet,londerdelen:LOnderdelen}
 val CPN'BRID1043871671 = ref ({i=INT.base,lint=LInt.base,wn=WelNiet.base,londerdelen=LOnderdelen.base}: CPN'BRTID1043871671)
 structure CPN'transitionID1043871671 = (* GenereerVliegtuig'Bepaal_Onderdelen *)
 struct 
 val CPN'id = "ID1043871671"
 val CPN'name = "GenereerVliegtuig'Bepaal_Onderdelen"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["i","lint","wn","londerdelen"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {i: INT,lint: LInt} list)
 val CPN'bh2 = ref(nil: {wn: WelNiet} list)
 val CPN'bh3 = ref(nil: {londerdelen: LOnderdelen} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf4() = (CPN'whole_binding := true)
 fun CPN'bf3() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'LOnderdelen_pims1.init_res(CPN'placeID1043899894.mark CPN'inst)
 fun CPN'bf() = 
let
 val londerdelen = case CPN'mode of 
CPN'Sim.all_enabled => CPN'LOnderdelen_pims1.random_res BindFailureGenAll (CPN'placeID1043899894.mark CPN'inst)
 | _ => CPN'LOnderdelen_pims1.random_res BindFatalFailure (CPN'placeID1043899894.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'LOnderdelen_pims1.res_col(CPN'placeID1043899894.mark CPN'inst,londerdelen)
 | _ => ()
 in
 (
 (CPN'bh3::= {londerdelen=londerdelen};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf4())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 fun CPN'bf2() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'WelNiet_pims1.init_res(CPN'placeID1043972563.mark CPN'inst)
 fun CPN'bf() = 
let
 val wn = case CPN'mode of 
CPN'Sim.all_enabled => CPN'WelNiet_pims1.random_res BindFailureGenAll (CPN'placeID1043972563.mark CPN'inst)
 | _ => CPN'WelNiet_pims1.random_res BindFatalFailure (CPN'placeID1043972563.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'WelNiet_pims1.res_col(CPN'placeID1043972563.mark CPN'inst,wn)
 | _ => ()
 in
 (if (WelNiet.legal wn) then
 (CPN'bh2::= {wn=wn};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf3())
 handle BindFailure => CPN'bf()
 else CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 fun CPN'bf1() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'LInt_pims1.init_res(CPN'placeID1044083806.mark CPN'inst)
 fun CPN'bf() = 
let
 val i::lint = case CPN'mode of 
CPN'Sim.all_enabled => CPN'LInt_pims1.random_res BindFailureGenAll (CPN'placeID1044083806.mark CPN'inst)
 | _ => CPN'LInt_pims1.random_res BindFatalFailure (CPN'placeID1044083806.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'LInt_pims1.res_col(CPN'placeID1044083806.mark CPN'inst,i::lint)
 | _ => ()
 in
 (
 (CPN'bh1::= {i=i,lint=lint};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
CPN'bh2:=nil;
CPN'bh3:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ();
CPN'bf2() handle BoundGroup => ();
CPN'bf3() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ();
CPN'bf3() handle BindFailureGenAll => ();
CPN'bf4() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({i,lint},{wn},{londerdelen}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'LInt_pims1.delete(CPN'placeID1044083806.mark CPN'inst,i::lint);
 CPN'LOnderdelen_pims1.delete(CPN'placeID1043899894.mark CPN'inst,londerdelen);
 CPN'WelNiet_pims1.delete(CPN'placeID1043972563.mark CPN'inst,wn));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val _ = (CPN'LOnderdelen_pims1.insert(CPN'placeID1043899894.mark CPN'inst,Onderdeel(i,londerdelen,wn));
 CPN'LInt_pims1.insert(CPN'placeID1044083806.mark CPN'inst,lint));
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {i=i,lint=lint,wn=wn,londerdelen=londerdelen}
 val _ = (CPN'BRID1043871671:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - i = ",INT.mkstr i,"\n - lint = ",LInt.mkstr lint,"\n - wn = ",WelNiet.mkstr wn,"\n - londerdelen = ",LOnderdelen.mkstr londerdelen]
else nil)
 end
 fun CPN'pick [CPN'i1,CPN'i2,CPN'i3] = (CPN'nth(!CPN'bh1,CPN'i1),CPN'nth(!CPN'bh2,CPN'i2),CPN'nth(!CPN'bh3,CPN'i3))
 fun CPN'bindings_as_strings() = [(["i","lint"],CPN'map (fn {i,lint} => [INT.mkstr i,LInt.mkstr lint]) (!CPN'bh1)),(["wn"],CPN'map (fn {wn} => [WelNiet.mkstr wn]) (!CPN'bh2)),(["londerdelen"],CPN'map (fn {londerdelen} => [LOnderdelen.mkstr londerdelen]) (!CPN'bh3))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_place(1 <= CPN'placeID1044083806.init(CPN'inst),
 CPN'Sim.each_place(1 <= CPN'placeID1043899894.init(CPN'inst),
 CPN'Sim.each_place(1 <= CPN'placeID1043972563.init(CPN'inst),
 (true,CPN'Sim.is_disabled)))))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1),CPN'hd(!CPN'bh2),CPN'hd(!CPN'bh3)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0,(CPN'e1,CPN'e2)) = (CPN'e0,CPN'e1,CPN'e2)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(expand1(!CPN'bh1,expand1(!CPN'bh2,!CPN'bh3))))
 end

 end (* end CPN'transitionID1043871671 *) 
 val _ = CPN'Sim.add_be("ID1043871671",CPN'transitionID1043871671.CPN'bind_exe,CPN'transitionID1043871671.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1044103595 = {lint:LInt}*{tien:INT}*{i:INT}
 type CPN'BRTID1044103595 = {lint:LInt,tien:INT,i:INT}
 val CPN'BRID1044103595 = ref ({lint=LInt.base,tien=INT.base,i=INT.base}: CPN'BRTID1044103595)
 structure CPN'transitionID1044103595 = (* GenereerVliegtuig'Genereer_Onderdelen *)
 struct 
 val CPN'id = "ID1044103595"
 val CPN'name = "GenereerVliegtuig'Genereer_Onderdelen"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["lint","tien","i"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {lint: LInt} list)
 val CPN'bh2 = ref(nil: {tien: INT} list)
 val CPN'bh3 = ref(nil: {i: INT} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf4() = (CPN'whole_binding := true)
 fun CPN'bf3() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'INT_pims1.init_res(CPN'placeID1043868911.mark CPN'inst)
 fun CPN'bf() = 
let
 val i = case CPN'mode of 
CPN'Sim.all_enabled => CPN'INT_pims1.random_res BindFailureGenAll (CPN'placeID1043868911.mark CPN'inst)
 | _ => CPN'INT_pims1.random_res BindFatalFailure (CPN'placeID1043868911.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'INT_pims1.res_col(CPN'placeID1043868911.mark CPN'inst,i)
 | _ => ()
 in
 (
 (* T_g *)
 (if (i<=10) then
 (CPN'bh3::= {i=i};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf4())
 else raise BindFailure)
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 fun CPN'bf2() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'INT_pims1.init_res(CPN'placeID1044341038.mark CPN'inst)
 fun CPN'bf() = 
let
 val tien = case CPN'mode of 
CPN'Sim.all_enabled => CPN'INT_pims1.random_res BindFailureGenAll (CPN'placeID1044341038.mark CPN'inst)
 | _ => CPN'INT_pims1.random_res BindFatalFailure (CPN'placeID1044341038.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'INT_pims1.res_col(CPN'placeID1044341038.mark CPN'inst,tien)
 | _ => ()
 in
 (
 (CPN'bh2::= {tien=tien};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf3())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 fun CPN'bf1() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'LInt_pims1.init_res(CPN'placeID1044083806.mark CPN'inst)
 fun CPN'bf() = 
let
 val lint = case CPN'mode of 
CPN'Sim.all_enabled => CPN'LInt_pims1.random_res BindFailureGenAll (CPN'placeID1044083806.mark CPN'inst)
 | _ => CPN'LInt_pims1.random_res BindFatalFailure (CPN'placeID1044083806.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'LInt_pims1.res_col(CPN'placeID1044083806.mark CPN'inst,lint)
 | _ => ()
 in
 (
 (CPN'bh1::= {lint=lint};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
CPN'bh2:=nil;
CPN'bh3:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ();
CPN'bf2() handle BoundGroup => ();
CPN'bf3() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ();
CPN'bf3() handle BindFailureGenAll => ();
CPN'bf4() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({lint},{tien},{i}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'INT_pims1.delete(CPN'placeID1043868911.mark CPN'inst,i);
 CPN'INT_pims1.delete(CPN'placeID1044341038.mark CPN'inst,tien);
 CPN'LInt_pims1.delete(CPN'placeID1044083806.mark CPN'inst,lint));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val _ = (CPN'INT_pims1.insert(CPN'placeID1043868911.mark CPN'inst,i+1);
 CPN'WelNiet_pims1.insert(CPN'placeID1043972563.mark CPN'inst,WelkeOnderdelen());
 if WelNiet.legal (WelkeOnderdelen()) then () else CPN'suberr::= ("ID1044167100", WelNiet.illegal_msg (WelkeOnderdelen()));
 CPN'INT_pims1.insert(CPN'placeID1044341038.mark CPN'inst,if i=10 then
tien+1 else tien);
 CPN'LInt_pims1.insert(CPN'placeID1044083806.mark CPN'inst,Invoegen(i,lint)));
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {lint=lint,tien=tien,i=i}
 val _ = (CPN'BRID1044103595:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - lint = ",LInt.mkstr lint,"\n - tien = ",INT.mkstr tien,"\n - i = ",INT.mkstr i]
else nil)
 end
 fun CPN'pick [CPN'i1,CPN'i2,CPN'i3] = (CPN'nth(!CPN'bh1,CPN'i1),CPN'nth(!CPN'bh2,CPN'i2),CPN'nth(!CPN'bh3,CPN'i3))
 fun CPN'bindings_as_strings() = [(["lint"],CPN'map (fn {lint} => [LInt.mkstr lint]) (!CPN'bh1)),(["tien"],CPN'map (fn {tien} => [INT.mkstr tien]) (!CPN'bh2)),(["i"],CPN'map (fn {i} => [INT.mkstr i]) (!CPN'bh3))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_place(1 <= CPN'placeID1043868911.init(CPN'inst),
 CPN'Sim.each_place(1 <= CPN'placeID1044341038.init(CPN'inst),
 CPN'Sim.each_place(1 <= CPN'placeID1044083806.init(CPN'inst),
 (true,CPN'Sim.is_disabled)))))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1),CPN'hd(!CPN'bh2),CPN'hd(!CPN'bh3)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0,(CPN'e1,CPN'e2)) = (CPN'e0,CPN'e1,CPN'e2)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(expand1(!CPN'bh1,expand1(!CPN'bh2,!CPN'bh3))))
 end

 end (* end CPN'transitionID1044103595 *) 
 val _ = CPN'Sim.add_be("ID1044103595",CPN'transitionID1044103595.CPN'bind_exe,CPN'transitionID1044103595.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1037471091 = {l:LVL}*{lvliegtuig:LVliegtuig}*{vs:VliegtuigSnowdesk}
 type CPN'BRTID1037471091 = {l:LVL,lvliegtuig:LVliegtuig,vs:VliegtuigSnowdesk}
 val CPN'BRID1037471091 = ref ({l=LVL.base,lvliegtuig=LVliegtuig.base,vs=VliegtuigSnowdesk.base}: CPN'BRTID1037471091)
 structure CPN'transitionID1037471091 = (* Inplannen'Inplannen *)
 struct 
 val CPN'id = "ID1037471091"
 val CPN'name = "Inplannen'Inplannen"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["l","lvliegtuig","vs"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {l: LVL} list)
 val CPN'bh2 = ref(nil: {lvliegtuig: LVliegtuig} list)
 val CPN'bh3 = ref(nil: {vs: VliegtuigSnowdesk} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf4() = (CPN'whole_binding := true)
 fun CPN'bf3() = 
 let (* timed B_p with coef 1 *)
 val _ = CPN'VliegtuigSnowdesk'timed_pims12.init_res(CPN'placeID1037471106.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@(vs,CPN'time36) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'VliegtuigSnowdesk'timed_pims12.random_res BindFailureGenAll (CPN'placeID1037471106.mark CPN'inst)
 | _ => CPN'VliegtuigSnowdesk'timed_pims12.random_res BindFatalFailure (CPN'placeID1037471106.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'VliegtuigSnowdesk'timed_pims12.res_col(CPN'placeID1037471106.mark CPN'inst, CPN'Time.@(vs,CPN'time36))
 | _ => ()
 in (
 (CPN'bh3::= {vs=vs};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf4())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 fun CPN'bf2() = 
 let (* timed B_p with coef 1 *)
 val _ = CPN'LVliegtuig'timed_pims1.init_res(CPN'placeID1037471101.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@(lvliegtuig,CPN'time35) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'LVliegtuig'timed_pims1.random_res BindFailureGenAll (CPN'placeID1037471101.mark CPN'inst)
 | _ => CPN'LVliegtuig'timed_pims1.random_res BindFatalFailure (CPN'placeID1037471101.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'LVliegtuig'timed_pims1.res_col(CPN'placeID1037471101.mark CPN'inst, CPN'Time.@(lvliegtuig,CPN'time35))
 | _ => ()
 in (
 (CPN'bh2::= {lvliegtuig=lvliegtuig};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf3())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 fun CPN'bf1() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'LVL_pims1.init_res(CPN'placeID1042262402.mark CPN'inst)
 fun CPN'bf() = 
let
 val l = case CPN'mode of 
CPN'Sim.all_enabled => CPN'LVL_pims1.random_res BindFailureGenAll (CPN'placeID1042262402.mark CPN'inst)
 | _ => CPN'LVL_pims1.random_res BindFatalFailure (CPN'placeID1042262402.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'LVL_pims1.res_col(CPN'placeID1042262402.mark CPN'inst,l)
 | _ => ()
 in
 (
 (CPN'bh1::= {l=l};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
CPN'bh2:=nil;
CPN'bh3:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ();
CPN'bf2() handle BoundGroup => ();
CPN'bf3() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ();
CPN'bf3() handle BindFailureGenAll => ();
CPN'bf4() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({l},{lvliegtuig},{vs}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'VliegtuigSnowdesk'timed_pims12.delete(CPN'placeID1037471106.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1037471091 CPN'placeID1037471106") (CPN'placeID1037471106.mark CPN'inst,CPN'VliegtuigSnowdesk'timed_pims12.collect,CPN'VliegtuigSnowdesk'timed_pims12.cmp,vs, time()));
 CPN'LVliegtuig'timed_pims1.delete(CPN'placeID1037471101.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1037471091 CPN'placeID1037471101") (CPN'placeID1037471101.mark CPN'inst,CPN'LVliegtuig'timed_pims1.collect,CPN'LVliegtuig'timed_pims1.cmp,lvliegtuig, time()));
 CPN'LVL_pims1.delete(CPN'placeID1042262402.mark CPN'inst,l));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val CPN'trans_delay = CPN'Time.fromInt(0)
 val _ = (CPN'Snowdesk_pims1.insert(CPN'placeID1037471096.mark CPN'inst,#snowdesk vs);
 CPN'VliegtuigLVL'timed_pims12_sims.addto (CPN'placeID1043559862.wait CPN'inst,CPN'TMS.@++([({vliegtuig = UpdateDepTime
(#vliegtuig vs,lvliegtuig), lvl=l})],CPN'trans_delay));
 CPN'LVliegtuig'timed_pims1_sims.addto (CPN'placeID1037471101.wait CPN'inst,CPN'TMS.@++([(qInsert(#vliegtuig vs, lvliegtuig))],CPN'trans_delay)));
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {l=l,lvliegtuig=lvliegtuig,vs=vs}
 val _ = (CPN'BRID1037471091:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - l = ",LVL.mkstr l,"\n - lvliegtuig = ",LVliegtuig.mkstr lvliegtuig,"\n - vs = ",VliegtuigSnowdesk.mkstr vs]
else nil)
 end
 fun CPN'pick [CPN'i1,CPN'i2,CPN'i3] = (CPN'nth(!CPN'bh1,CPN'i1),CPN'nth(!CPN'bh2,CPN'i2),CPN'nth(!CPN'bh3,CPN'i3))
 fun CPN'bindings_as_strings() = [(["l"],CPN'map (fn {l} => [LVL.mkstr l]) (!CPN'bh1)),(["lvliegtuig"],CPN'map (fn {lvliegtuig} => [LVliegtuig.mkstr lvliegtuig]) (!CPN'bh2)),(["vs"],CPN'map (fn {vs} => [VliegtuigSnowdesk.mkstr vs]) (!CPN'bh3))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_timed_place(1 <= CPN'placeID1037471106.init(CPN'inst),CPN'placeID1037471106.next_time (CPN'inst),
 CPN'Sim.each_timed_place(1 <= CPN'placeID1037471101.init(CPN'inst),CPN'placeID1037471101.next_time (CPN'inst),
 CPN'Sim.each_place(1 <= CPN'placeID1042262402.init(CPN'inst),
 (true,CPN'Sim.is_disabled)))))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1),CPN'hd(!CPN'bh2),CPN'hd(!CPN'bh3)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0,(CPN'e1,CPN'e2)) = (CPN'e0,CPN'e1,CPN'e2)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(expand1(!CPN'bh1,expand1(!CPN'bh2,!CPN'bh3))))
 end

 end (* end CPN'transitionID1037471091 *) 
 val _ = CPN'Sim.add_be("ID1037471091",CPN'transitionID1037471091.CPN'bind_exe,CPN'transitionID1037471091.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1037471359 = {s:Snowdesk}*{v:Vliegtuig}
 type CPN'BRTID1037471359 = {s:Snowdesk,v:Vliegtuig}
 val CPN'BRID1037471359 = ref ({s=Snowdesk.base,v=Vliegtuig.base}: CPN'BRTID1037471359)
 structure CPN'transitionID1037471359 = (* Inplannen'Contact_opnemen_met_snowdesk *)
 struct 
 val CPN'id = "ID1037471359"
 val CPN'name = "Inplannen'Contact_opnemen_met_snowdesk"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["s","v"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {s: Snowdesk} list)
 val CPN'bh2 = ref(nil: {v: Vliegtuig} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf3() = (CPN'whole_binding := true)
 fun CPN'bf2() = 
 let (* timed B_p with coef 1 *)
 val _ = CPN'Vliegtuig'timed_pims123456.init_res(CPN'placeID1037471900.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@(v,CPN'time39) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'Vliegtuig'timed_pims123456.random_res BindFailureGenAll (CPN'placeID1037471900.mark CPN'inst)
 | _ => CPN'Vliegtuig'timed_pims123456.random_res BindFatalFailure (CPN'placeID1037471900.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'Vliegtuig'timed_pims123456.res_col(CPN'placeID1037471900.mark CPN'inst, CPN'Time.@(v,CPN'time39))
 | _ => ()
 in (
 (CPN'bh2::= {v=v};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf3())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 fun CPN'bf1() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'Snowdesk_pims1.init_res(CPN'placeID1037471096.mark CPN'inst)
 fun CPN'bf() = 
let
 val s = case CPN'mode of 
CPN'Sim.all_enabled => CPN'Snowdesk_pims1.random_res BindFailureGenAll (CPN'placeID1037471096.mark CPN'inst)
 | _ => CPN'Snowdesk_pims1.random_res BindFatalFailure (CPN'placeID1037471096.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'Snowdesk_pims1.res_col(CPN'placeID1037471096.mark CPN'inst,s)
 | _ => ()
 in
 (
 (CPN'bh1::= {s=s};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
CPN'bh2:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ();
CPN'bf2() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ();
CPN'bf3() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({s},{v}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'Vliegtuig'timed_pims123456.delete(CPN'placeID1037471900.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1037471359 CPN'placeID1037471900") (CPN'placeID1037471900.mark CPN'inst,CPN'Vliegtuig'timed_pims123456.collect,CPN'Vliegtuig'timed_pims123456.cmp,v, time()));
 CPN'Snowdesk_pims1.delete(CPN'placeID1037471096.mark CPN'inst,s));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val CPN'trans_delay = CPN'Time.fromInt(1)
 val _ = (CPN'VliegtuigSnowdesk'timed_pims12_sims.addto (CPN'placeID1037471106.wait CPN'inst,CPN'TMS.@++([({vliegtuig=v,
snowdesk=s})],CPN'trans_delay)));
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {s=s,v=v}
 val _ = (CPN'BRID1037471359:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - s = ",Snowdesk.mkstr s,"\n - v = ",Vliegtuig.mkstr v]
else nil)
 end
 fun CPN'pick [CPN'i1,CPN'i2] = (CPN'nth(!CPN'bh1,CPN'i1),CPN'nth(!CPN'bh2,CPN'i2))
 fun CPN'bindings_as_strings() = [(["s"],CPN'map (fn {s} => [Snowdesk.mkstr s]) (!CPN'bh1)),(["v"],CPN'map (fn {v} => [Vliegtuig.mkstr v]) (!CPN'bh2))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_timed_place(1 <= CPN'placeID1037471900.init(CPN'inst),CPN'placeID1037471900.next_time (CPN'inst),
 CPN'Sim.each_place(1 <= CPN'placeID1037471096.init(CPN'inst),
 (true,CPN'Sim.is_disabled))))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1),CPN'hd(!CPN'bh2)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0,CPN'e1) = (CPN'e0,CPN'e1)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(expand1(!CPN'bh1,!CPN'bh2)))
 end

 end (* end CPN'transitionID1037471359 *) 
 val _ = CPN'Sim.add_be("ID1037471359",CPN'transitionID1037471359.CPN'bind_exe,CPN'transitionID1037471359.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1043559617 = {vl:VliegtuigLVL}
 type CPN'BRTID1043559617 = {vl:VliegtuigLVL}
 val CPN'BRID1043559617 = ref ({vl=VliegtuigLVL.base}: CPN'BRTID1043559617)
 structure CPN'transitionID1043559617 = (* Inplannen'Nieuw_Schema_Doorgeven *)
 struct 
 val CPN'id = "ID1043559617"
 val CPN'name = "Inplannen'Nieuw_Schema_Doorgeven"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["vl"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {vl: VliegtuigLVL} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf2() = (CPN'whole_binding := true)
 fun CPN'bf1() = 
 let (* timed B_p with coef 1 *)
 val _ = CPN'VliegtuigLVL'timed_pims12.init_res(CPN'placeID1043559862.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@(vl,CPN'time41) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'VliegtuigLVL'timed_pims12.random_res BindFailureGenAll (CPN'placeID1043559862.mark CPN'inst)
 | _ => CPN'VliegtuigLVL'timed_pims12.random_res BindFatalFailure (CPN'placeID1043559862.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'VliegtuigLVL'timed_pims12.res_col(CPN'placeID1043559862.mark CPN'inst, CPN'Time.@(vl,CPN'time41))
 | _ => ()
 in (
 (CPN'bh1::= {vl=vl};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({vl}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'VliegtuigLVL'timed_pims12.delete(CPN'placeID1043559862.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1043559617 CPN'placeID1043559862") (CPN'placeID1043559862.mark CPN'inst,CPN'VliegtuigLVL'timed_pims12.collect,CPN'VliegtuigLVL'timed_pims12.cmp,vl, time())));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val CPN'trans_delay = CPN'Time.fromInt(0)
 val _ = (CPN'LVL_pims1.insert(CPN'placeID1042262402.mark CPN'inst,#lvl vl);
 CPN'Vliegtuig'timed_pims123456_sims.addto (CPN'placeID1042658947.wait CPN'inst,CPN'TMS.@++([(#vliegtuig vl)],CPN'trans_delay)));
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {vl=vl}
 val _ = (CPN'BRID1043559617:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - vl = ",VliegtuigLVL.mkstr vl]
else nil)
 end
 fun CPN'pick [CPN'i1] = (CPN'nth(!CPN'bh1,CPN'i1))
 fun CPN'bindings_as_strings() = [(["vl"],CPN'map (fn {vl} => [VliegtuigLVL.mkstr vl]) (!CPN'bh1))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_timed_place(1 <= CPN'placeID1043559862.init(CPN'inst),CPN'placeID1043559862.next_time (CPN'inst),
 (true,CPN'Sim.is_disabled)))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0) = (CPN'e0)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(!CPN'bh1))
 end

 end (* end CPN'transitionID1043559617 *) 
 val _ = CPN'Sim.add_be("ID1043559617",CPN'transitionID1043559617.CPN'bind_exe,CPN'transitionID1043559617.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1037477714 = {lvliegtuig:LVliegtuig,v:Vliegtuig}*{c:DCoordinator}*{operator:Operator}
 type CPN'BRTID1037477714 = {lvliegtuig:LVliegtuig,v:Vliegtuig,c:DCoordinator,operator:Operator}
 val CPN'BRID1037477714 = ref ({lvliegtuig=LVliegtuig.base,v=Vliegtuig.base,c=DCoordinator.base,operator=Operator.base}: CPN'BRTID1037477714)
 structure CPN'transitionID1037477714 = (* DeicingProces'Start_deicing_proces *)
 struct 
 val CPN'id = "ID1037477714"
 val CPN'name = "DeicingProces'Start_deicing_proces"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["lvliegtuig","v","c","operator"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {lvliegtuig: LVliegtuig,v: Vliegtuig} list)
 val CPN'bh2 = ref(nil: {c: DCoordinator} list)
 val CPN'bh3 = ref(nil: {operator: Operator} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf4() = (CPN'whole_binding := true)
 fun CPN'bf3() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'Operator_pims1.init_res(CPN'placeID1037530306.mark CPN'inst)
 fun CPN'bf() = 
let
 val operator = case CPN'mode of 
CPN'Sim.all_enabled => CPN'Operator_pims1.random_res BindFailureGenAll (CPN'placeID1037530306.mark CPN'inst)
 | _ => CPN'Operator_pims1.random_res BindFatalFailure (CPN'placeID1037530306.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'Operator_pims1.res_col(CPN'placeID1037530306.mark CPN'inst,operator)
 | _ => ()
 in
 (
 (* T_g *)
 (if (operator>0) then
 (CPN'bh3::= {operator=operator};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf4())
 else raise BindFailure)
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 fun CPN'bf2() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'DCoordinator_pims1.init_res(CPN'placeID1037477724.mark CPN'inst)
 fun CPN'bf() = 
let
 val c = case CPN'mode of 
CPN'Sim.all_enabled => CPN'DCoordinator_pims1.random_res BindFailureGenAll (CPN'placeID1037477724.mark CPN'inst)
 | _ => CPN'DCoordinator_pims1.random_res BindFatalFailure (CPN'placeID1037477724.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'DCoordinator_pims1.res_col(CPN'placeID1037477724.mark CPN'inst,c)
 | _ => ()
 in
 (
 (CPN'bh2::= {c=c};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf3())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 fun CPN'bf1() = 
 let (* timed B_p with coef 1 *)
 val _ = CPN'LVliegtuig'timed_pims1.init_res(CPN'placeID1037477729.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@(v::lvliegtuig,CPN'time43) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'LVliegtuig'timed_pims1.random_res BindFailureGenAll (CPN'placeID1037477729.mark CPN'inst)
 | _ => CPN'LVliegtuig'timed_pims1.random_res BindFatalFailure (CPN'placeID1037477729.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'LVliegtuig'timed_pims1.res_col(CPN'placeID1037477729.mark CPN'inst, CPN'Time.@(v::lvliegtuig,CPN'time43))
 | _ => ()
 in (
 (CPN'bh1::= {lvliegtuig=lvliegtuig,v=v};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
CPN'bh2:=nil;
CPN'bh3:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ();
CPN'bf2() handle BoundGroup => ();
CPN'bf3() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ();
CPN'bf3() handle BindFailureGenAll => ();
CPN'bf4() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({lvliegtuig,v},{c},{operator}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'Operator_pims1.delete(CPN'placeID1037530306.mark CPN'inst,operator);
 CPN'LVliegtuig'timed_pims1.delete(CPN'placeID1037477729.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1037477714 CPN'placeID1037477729") (CPN'placeID1037477729.mark CPN'inst,CPN'LVliegtuig'timed_pims1.collect,CPN'LVliegtuig'timed_pims1.cmp,v::lvliegtuig, time()));
 CPN'DCoordinator_pims1.delete(CPN'placeID1037477724.mark CPN'inst,c));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val CPN'trans_delay = CPN'Time.fromInt(0)
 val _ = (CPN'VliegtuigDCoordinator'timed_pims12_sims.addto (CPN'placeID1037477734.wait CPN'inst,CPN'TMS.@++([({vliegtuig=v,
dcoordinator=c})],CPN'trans_delay));
 CPN'VliegtuigDCoordinator'timed_pims12_sims.addto (CPN'placeID1037477741.wait CPN'inst,CPN'TMS.@++([({vliegtuig=v,
dcoordinator=c})],CPN'trans_delay));
 CPN'Operator_pims1.insert(CPN'placeID1037530306.mark CPN'inst,operator);
 CPN'VliegtuigDCoordinator'timed_pims12_sims.addto (CPN'placeID1046710557.wait CPN'inst,CPN'TMS.@++([({vliegtuig=v,
dcoordinator =c})],CPN'trans_delay));
 CPN'LVliegtuig'timed_pims1_sims.addto (CPN'placeID1037477729.wait CPN'inst,CPN'TMS.@++([(lvliegtuig)],CPN'trans_delay)));
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {lvliegtuig=lvliegtuig,v=v,c=c,operator=operator}
 val _ = (CPN'BRID1037477714:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - lvliegtuig = ",LVliegtuig.mkstr lvliegtuig,"\n - v = ",Vliegtuig.mkstr v,"\n - c = ",DCoordinator.mkstr c,"\n - operator = ",Operator.mkstr operator]
else nil)
 end
 fun CPN'pick [CPN'i1,CPN'i2,CPN'i3] = (CPN'nth(!CPN'bh1,CPN'i1),CPN'nth(!CPN'bh2,CPN'i2),CPN'nth(!CPN'bh3,CPN'i3))
 fun CPN'bindings_as_strings() = [(["lvliegtuig","v"],CPN'map (fn {lvliegtuig,v} => [LVliegtuig.mkstr lvliegtuig,Vliegtuig.mkstr v]) (!CPN'bh1)),(["c"],CPN'map (fn {c} => [DCoordinator.mkstr c]) (!CPN'bh2)),(["operator"],CPN'map (fn {operator} => [Operator.mkstr operator]) (!CPN'bh3))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_place(1 <= CPN'placeID1037530306.init(CPN'inst),
 CPN'Sim.each_timed_place(1 <= CPN'placeID1037477729.init(CPN'inst),CPN'placeID1037477729.next_time (CPN'inst),
 CPN'Sim.each_place(1 <= CPN'placeID1037477724.init(CPN'inst),
 (true,CPN'Sim.is_disabled)))))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1),CPN'hd(!CPN'bh2),CPN'hd(!CPN'bh3)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0,(CPN'e1,CPN'e2)) = (CPN'e0,CPN'e1,CPN'e2)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(expand1(!CPN'bh1,expand1(!CPN'bh2,!CPN'bh3))))
 end

 end (* end CPN'transitionID1037477714 *) 
 val _ = CPN'Sim.add_be("ID1037477714",CPN'transitionID1037477714.CPN'bind_exe,CPN'transitionID1037477714.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1047850528 = {c:DCoordinator}
 type CPN'BRTID1047850528 = {c:DCoordinator}
 val CPN'BRID1047850528 = ref ({c=DCoordinator.base}: CPN'BRTID1047850528)
 structure CPN'transitionID1047850528 = (* DeicingProces'Deice_Coordinator_klaar *)
 struct 
 val CPN'id = "ID1047850528"
 val CPN'name = "DeicingProces'Deice_Coordinator_klaar"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["c"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {c: DCoordinator} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf2() = (CPN'whole_binding := true)
 fun CPN'bf1() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'DCoordinator_pims1.init_res(CPN'placeID1047435540.mark CPN'inst)
 fun CPN'bf() = 
let
 val c = case CPN'mode of 
CPN'Sim.all_enabled => CPN'DCoordinator_pims1.random_res BindFailureGenAll (CPN'placeID1047435540.mark CPN'inst)
 | _ => CPN'DCoordinator_pims1.random_res BindFatalFailure (CPN'placeID1047435540.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'DCoordinator_pims1.res_col(CPN'placeID1047435540.mark CPN'inst,c)
 | _ => ()
 in
 (
 (* untimed T_a token exp coef 1 *)
 (if CPN'DCoordinator_pims1.member (!(CPN'placeID1047614114.mark CPN'inst),c) then
 (* untimed T_a token exp coef 1 *)
 (if CPN'DCoordinator_pims1.member (!(CPN'placeID1047451965.mark CPN'inst),c) then
 (CPN'bh1::= {c=c};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 else raise BindFailure)
 else raise BindFailure)
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({c}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'DCoordinator_pims1.delete(CPN'placeID1047614114.mark CPN'inst,c);
 CPN'DCoordinator_pims1.delete(CPN'placeID1047435540.mark CPN'inst,c);
 CPN'DCoordinator_pims1.delete(CPN'placeID1047451965.mark CPN'inst,c));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val _ = (CPN'DCoordinator_pims1.insert(CPN'placeID1037477724.mark CPN'inst,c));
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {c=c}
 val _ = (CPN'BRID1047850528:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - c = ",DCoordinator.mkstr c]
else nil)
 end
 fun CPN'pick [CPN'i1] = (CPN'nth(!CPN'bh1,CPN'i1))
 fun CPN'bindings_as_strings() = [(["c"],CPN'map (fn {c} => [DCoordinator.mkstr c]) (!CPN'bh1))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_place(1 <= CPN'placeID1047614114.init(CPN'inst),
 CPN'Sim.each_place(1 <= CPN'placeID1047435540.init(CPN'inst),
 CPN'Sim.each_place(1 <= CPN'placeID1047451965.init(CPN'inst),
 (true,CPN'Sim.is_disabled)))))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0) = (CPN'e0)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(!CPN'bh1))
 end

 end (* end CPN'transitionID1047850528 *) 
 val _ = CPN'Sim.add_be("ID1047850528",CPN'transitionID1047850528.CPN'bind_exe,CPN'transitionID1047850528.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1037590908 = {voc:VliegtuigOperatorControleur,con:Controle}
 type CPN'BRTID1037590908 = {voc:VliegtuigOperatorControleur,con:Controle}
 val CPN'BRID1037590908 = ref ({voc=VliegtuigOperatorControleur.base,con=Controle.base}: CPN'BRTID1037590908)
 structure CPN'transitionID1037590908 = (* Controle_de'Controle_de *)
 struct 
 val CPN'id = "ID1037590908"
 val CPN'name = "Controle_de'Controle_de"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["voc","con"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {voc: VliegtuigOperatorControleur,con: Controle} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf2() = (CPN'whole_binding := true)
 fun CPN'bf1() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'Controle_pims12.init_res(CPN'placeID1044884445.mark CPN'inst)
 fun CPN'bf() = 
let
 val con = case CPN'mode of 
CPN'Sim.all_enabled => CPN'Controle_pims12.random_res BindFailureGenAll (CPN'placeID1044884445.mark CPN'inst)
 | _ => CPN'Controle_pims12.random_res BindFatalFailure (CPN'placeID1044884445.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'Controle_pims12.res_col(CPN'placeID1044884445.mark CPN'inst,con)
 | _ => ()
 in
 (
 let (* timed B_p with coef 1 *)
 val _ = CPN'VliegtuigOperatorControleur'timed_pims123.init_res(CPN'placeID1037590918.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@(voc,CPN'time50) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'VliegtuigOperatorControleur'timed_pims123.random_res BindFailure (CPN'placeID1037590918.mark CPN'inst)
 | _ => CPN'VliegtuigOperatorControleur'timed_pims123.random_res BindFailure (CPN'placeID1037590918.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'VliegtuigOperatorControleur'timed_pims123.res_col(CPN'placeID1037590918.mark CPN'inst, CPN'Time.@(voc,CPN'time50))
 | _ => ()
 in (
 (* T_g *)
 (if (#vliegID (#vliegtuig voc) = #id con) then
 (CPN'bh1::= {voc=voc,con=con};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 else raise BindFailure)
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({voc,con}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'VliegtuigOperatorControleur'timed_pims123.delete(CPN'placeID1037590918.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1037590908 CPN'placeID1037590918") (CPN'placeID1037590918.mark CPN'inst,CPN'VliegtuigOperatorControleur'timed_pims123.collect,CPN'VliegtuigOperatorControleur'timed_pims123.cmp,voc, time()));
 CPN'Controle_pims12.delete(CPN'placeID1044884445.mark CPN'inst,con));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val CPN'trans_delay = CPN'Time.fromInt(0)
 val _ = (CPN'VliegtuigOperatorControleur'timed_pims123_sims.addto (CPN'placeID1037590913.wait CPN'inst,CPN'TMS.@++([({vliegtuig= #vliegtuig voc, 
operator= #operator voc,
controleur= if (#listonderdelen
(#vliegtuig voc) = #l con)
then 1 else 0})],CPN'trans_delay)));
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {voc=voc,con=con}
 val _ = (CPN'BRID1037590908:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - voc = ",VliegtuigOperatorControleur.mkstr voc,"\n - con = ",Controle.mkstr con]
else nil)
 end
 fun CPN'pick [CPN'i1] = (CPN'nth(!CPN'bh1,CPN'i1))
 fun CPN'bindings_as_strings() = [(["voc","con"],CPN'map (fn {voc,con} => [VliegtuigOperatorControleur.mkstr voc,Controle.mkstr con]) (!CPN'bh1))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_timed_place(1 <= CPN'placeID1037590918.init(CPN'inst),CPN'placeID1037590918.next_time (CPN'inst),
 CPN'Sim.each_place(1 <= CPN'placeID1044884445.init(CPN'inst),
 (true,CPN'Sim.is_disabled))))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0) = (CPN'e0)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(!CPN'bh1))
 end

 end (* end CPN'transitionID1037590908 *) 
 val _ = CPN'Sim.add_be("ID1037590908",CPN'transitionID1037590908.CPN'bind_exe,CPN'transitionID1037590908.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1037595210 = {ctlr:Controleur}*{voc:VliegtuigOperatorControleur}
 type CPN'BRTID1037595210 = {ctlr:Controleur,voc:VliegtuigOperatorControleur}
 val CPN'BRID1037595210 = ref ({ctlr=Controleur.base,voc=VliegtuigOperatorControleur.base}: CPN'BRTID1037595210)
 structure CPN'transitionID1037595210 = (* Controle_de'Niet_goed *)
 struct 
 val CPN'id = "ID1037595210"
 val CPN'name = "Controle_de'Niet_goed"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["ctlr","voc"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {ctlr: Controleur} list)
 val CPN'bh2 = ref(nil: {voc: VliegtuigOperatorControleur} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf3() = (CPN'whole_binding := true)
 fun CPN'bf2() = 
 let (* timed B_p with coef 1 *)
 val _ = CPN'VliegtuigOperatorControleur'timed_pims123.init_res(CPN'placeID1037590913.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@(voc,CPN'time53) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'VliegtuigOperatorControleur'timed_pims123.random_res BindFailureGenAll (CPN'placeID1037590913.mark CPN'inst)
 | _ => CPN'VliegtuigOperatorControleur'timed_pims123.random_res BindFatalFailure (CPN'placeID1037590913.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'VliegtuigOperatorControleur'timed_pims123.res_col(CPN'placeID1037590913.mark CPN'inst, CPN'Time.@(voc,CPN'time53))
 | _ => ()
 in (
 (* T_g *)
 (if (#controleur voc = 0) then
 (CPN'bh2::= {voc=voc};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf3())
 else raise BindFailure)
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 fun CPN'bf1() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'Controleur_pims1.init_res(CPN'placeID1037590923.mark CPN'inst)
 fun CPN'bf() = 
let
 val ctlr = case CPN'mode of 
CPN'Sim.all_enabled => CPN'Controleur_pims1.random_res BindFailureGenAll (CPN'placeID1037590923.mark CPN'inst)
 | _ => CPN'Controleur_pims1.random_res BindFatalFailure (CPN'placeID1037590923.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'Controleur_pims1.res_col(CPN'placeID1037590923.mark CPN'inst,ctlr)
 | _ => ()
 in
 (
 (CPN'bh1::= {ctlr=ctlr};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
CPN'bh2:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ();
CPN'bf2() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ();
CPN'bf3() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({ctlr},{voc}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'Controleur_pims1.delete(CPN'placeID1037590923.mark CPN'inst,ctlr);
 CPN'VliegtuigOperatorControleur'timed_pims123.delete(CPN'placeID1037590913.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1037595210 CPN'placeID1037590913") (CPN'placeID1037590913.mark CPN'inst,CPN'VliegtuigOperatorControleur'timed_pims123.collect,CPN'VliegtuigOperatorControleur'timed_pims123.cmp,voc, time())));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val CPN'trans_delay = CPN'Time.fromInt(0)
 val _ = (CPN'VliegtuigOperator'timed_pims12_sims.addto (CPN'placeID1037600399.wait CPN'inst,CPN'TMS.@++([({vliegtuig= #vliegtuig voc,
operator= #operator voc})],CPN'trans_delay));
 CPN'Controleur_pims1.insert(CPN'placeID1037590923.mark CPN'inst,ctlr+1));
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {ctlr=ctlr,voc=voc}
 val _ = (CPN'BRID1037595210:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - ctlr = ",Controleur.mkstr ctlr,"\n - voc = ",VliegtuigOperatorControleur.mkstr voc]
else nil)
 end
 fun CPN'pick [CPN'i1,CPN'i2] = (CPN'nth(!CPN'bh1,CPN'i1),CPN'nth(!CPN'bh2,CPN'i2))
 fun CPN'bindings_as_strings() = [(["ctlr"],CPN'map (fn {ctlr} => [Controleur.mkstr ctlr]) (!CPN'bh1)),(["voc"],CPN'map (fn {voc} => [VliegtuigOperatorControleur.mkstr voc]) (!CPN'bh2))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_place(1 <= CPN'placeID1037590923.init(CPN'inst),
 CPN'Sim.each_timed_place(1 <= CPN'placeID1037590913.init(CPN'inst),CPN'placeID1037590913.next_time (CPN'inst),
 (true,CPN'Sim.is_disabled))))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1),CPN'hd(!CPN'bh2)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0,CPN'e1) = (CPN'e0,CPN'e1)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(expand1(!CPN'bh1,!CPN'bh2)))
 end

 end (* end CPN'transitionID1037595210 *) 
 val _ = CPN'Sim.add_be("ID1037595210",CPN'transitionID1037595210.CPN'bind_exe,CPN'transitionID1037595210.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1037597793 = {operator:Operator}*{ctlr:Controleur}*{voc:VliegtuigOperatorControleur}
 type CPN'BRTID1037597793 = {operator:Operator,ctlr:Controleur,voc:VliegtuigOperatorControleur}
 val CPN'BRID1037597793 = ref ({operator=Operator.base,ctlr=Controleur.base,voc=VliegtuigOperatorControleur.base}: CPN'BRTID1037597793)
 structure CPN'transitionID1037597793 = (* Controle_de'Goed *)
 struct 
 val CPN'id = "ID1037597793"
 val CPN'name = "Controle_de'Goed"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["operator","ctlr","voc"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {operator: Operator} list)
 val CPN'bh2 = ref(nil: {ctlr: Controleur} list)
 val CPN'bh3 = ref(nil: {voc: VliegtuigOperatorControleur} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf4() = (CPN'whole_binding := true)
 fun CPN'bf3() = 
 let (* timed B_p with coef 1 *)
 val _ = CPN'VliegtuigOperatorControleur'timed_pims123.init_res(CPN'placeID1037590913.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@(voc,CPN'time57) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'VliegtuigOperatorControleur'timed_pims123.random_res BindFailureGenAll (CPN'placeID1037590913.mark CPN'inst)
 | _ => CPN'VliegtuigOperatorControleur'timed_pims123.random_res BindFatalFailure (CPN'placeID1037590913.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'VliegtuigOperatorControleur'timed_pims123.res_col(CPN'placeID1037590913.mark CPN'inst, CPN'Time.@(voc,CPN'time57))
 | _ => ()
 in (
 (* T_g *)
 (if (#controleur voc = 1) then
 (CPN'bh3::= {voc=voc};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf4())
 else raise BindFailure)
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 fun CPN'bf2() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'Controleur_pims1.init_res(CPN'placeID1037590923.mark CPN'inst)
 fun CPN'bf() = 
let
 val ctlr = case CPN'mode of 
CPN'Sim.all_enabled => CPN'Controleur_pims1.random_res BindFailureGenAll (CPN'placeID1037590923.mark CPN'inst)
 | _ => CPN'Controleur_pims1.random_res BindFatalFailure (CPN'placeID1037590923.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'Controleur_pims1.res_col(CPN'placeID1037590923.mark CPN'inst,ctlr)
 | _ => ()
 in
 (
 (CPN'bh2::= {ctlr=ctlr};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf3())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 fun CPN'bf1() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'Operator_pims1.init_res(CPN'placeID1049388984.mark CPN'inst)
 fun CPN'bf() = 
let
 val operator = case CPN'mode of 
CPN'Sim.all_enabled => CPN'Operator_pims1.random_res BindFailureGenAll (CPN'placeID1049388984.mark CPN'inst)
 | _ => CPN'Operator_pims1.random_res BindFatalFailure (CPN'placeID1049388984.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'Operator_pims1.res_col(CPN'placeID1049388984.mark CPN'inst,operator)
 | _ => ()
 in
 (
 (CPN'bh1::= {operator=operator};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
CPN'bh2:=nil;
CPN'bh3:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ();
CPN'bf2() handle BoundGroup => ();
CPN'bf3() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ();
CPN'bf3() handle BindFailureGenAll => ();
CPN'bf4() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({operator},{ctlr},{voc}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'Controleur_pims1.delete(CPN'placeID1037590923.mark CPN'inst,ctlr);
 CPN'Operator_pims1.delete(CPN'placeID1049388984.mark CPN'inst,operator);
 CPN'VliegtuigOperatorControleur'timed_pims123.delete(CPN'placeID1037590913.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1037597793 CPN'placeID1037590913") (CPN'placeID1037590913.mark CPN'inst,CPN'VliegtuigOperatorControleur'timed_pims123.collect,CPN'VliegtuigOperatorControleur'timed_pims123.cmp,voc, time())));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val CPN'trans_delay = CPN'Time.fromInt(0)
 val _ = (CPN'Vliegtuig'timed_pims123456_sims.addto (CPN'placeID1037603032.wait CPN'inst,CPN'TMS.@++([(RecordTime(voc))],CPN'trans_delay));
 CPN'Operator_pims1.insert(CPN'placeID1049388984.mark CPN'inst,operator+
#operator voc);
 CPN'Controleur_pims1.insert(CPN'placeID1037590923.mark CPN'inst,ctlr+ 
#controleur voc));
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {operator=operator,ctlr=ctlr,voc=voc}
 val _ = (CPN'BRID1037597793:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - operator = ",Operator.mkstr operator,"\n - ctlr = ",Controleur.mkstr ctlr,"\n - voc = ",VliegtuigOperatorControleur.mkstr voc]
else nil)
 end
 fun CPN'pick [CPN'i1,CPN'i2,CPN'i3] = (CPN'nth(!CPN'bh1,CPN'i1),CPN'nth(!CPN'bh2,CPN'i2),CPN'nth(!CPN'bh3,CPN'i3))
 fun CPN'bindings_as_strings() = [(["operator"],CPN'map (fn {operator} => [Operator.mkstr operator]) (!CPN'bh1)),(["ctlr"],CPN'map (fn {ctlr} => [Controleur.mkstr ctlr]) (!CPN'bh2)),(["voc"],CPN'map (fn {voc} => [VliegtuigOperatorControleur.mkstr voc]) (!CPN'bh3))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_place(1 <= CPN'placeID1037590923.init(CPN'inst),
 CPN'Sim.each_place(1 <= CPN'placeID1049388984.init(CPN'inst),
 CPN'Sim.each_timed_place(1 <= CPN'placeID1037590913.init(CPN'inst),CPN'placeID1037590913.next_time (CPN'inst),
 (true,CPN'Sim.is_disabled)))))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1),CPN'hd(!CPN'bh2),CPN'hd(!CPN'bh3)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0,(CPN'e1,CPN'e2)) = (CPN'e0,CPN'e1,CPN'e2)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(expand1(!CPN'bh1,expand1(!CPN'bh2,!CPN'bh3))))
 end

 end (* end CPN'transitionID1037597793 *) 
 val _ = CPN'Sim.add_be("ID1037597793",CPN'transitionID1037597793.CPN'bind_exe,CPN'transitionID1037597793.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1038934846 = {vc:VliegtuigDCoordinator}*{operator:Operator}
 type CPN'BRTID1038934846 = {vc:VliegtuigDCoordinator,operator:Operator,op2:Operator}
 val CPN'BRID1038934846 = ref ({vc=VliegtuigDCoordinator.base,operator=Operator.base,op2=Operator.base}: CPN'BRTID1038934846)
 val CPN'code_actionID1038934846 = CPN'Sim.code_action (fn CPN'inst => fn (operator: Operator) => let
 in 
((Operators(operator)))
 end)
 structure CPN'transitionID1038934846 = (* DeicingProcedure'Operators_naar_gate *)
 struct 
 val CPN'id = "ID1038934846"
 val CPN'name = "DeicingProcedure'Operators_naar_gate"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["vc","operator"]
 val CPN'output_vars = ["op2"]
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {vc: VliegtuigDCoordinator} list)
 val CPN'bh2 = ref(nil: {operator: Operator} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf3() = (CPN'whole_binding := true)
 fun CPN'bf2() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'Operator_pims1.init_res(CPN'placeID1038911218.mark CPN'inst)
 fun CPN'bf() = 
let
 val operator = case CPN'mode of 
CPN'Sim.all_enabled => CPN'Operator_pims1.random_res BindFailureGenAll (CPN'placeID1038911218.mark CPN'inst)
 | _ => CPN'Operator_pims1.random_res BindFatalFailure (CPN'placeID1038911218.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'Operator_pims1.res_col(CPN'placeID1038911218.mark CPN'inst,operator)
 | _ => ()
 in
 (
 (* T_g *)
 (if (operator >1) then
 (CPN'bh2::= {operator=operator};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf3())
 else raise BindFailure)
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 fun CPN'bf1() = 
 let (* timed B_p with coef 1 *)
 val _ = CPN'VliegtuigDCoordinator'timed_pims12.init_res(CPN'placeID1038952654.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@(vc,CPN'time59) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'VliegtuigDCoordinator'timed_pims12.random_res BindFailureGenAll (CPN'placeID1038952654.mark CPN'inst)
 | _ => CPN'VliegtuigDCoordinator'timed_pims12.random_res BindFatalFailure (CPN'placeID1038952654.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'VliegtuigDCoordinator'timed_pims12.res_col(CPN'placeID1038952654.mark CPN'inst, CPN'Time.@(vc,CPN'time59))
 | _ => ()
 in (
 (CPN'bh1::= {vc=vc};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
CPN'bh2:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ();
CPN'bf2() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ();
CPN'bf3() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({vc},{operator}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val (op2) = CPN'code_actionID1038934846 CPN'inst (operator)
 val _ = (CPN'VliegtuigDCoordinator'timed_pims12.delete(CPN'placeID1038952654.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1038934846 CPN'placeID1038952654") (CPN'placeID1038952654.mark CPN'inst,CPN'VliegtuigDCoordinator'timed_pims12.collect,CPN'VliegtuigDCoordinator'timed_pims12.cmp,vc, time()));
 CPN'Operator_pims1.delete(CPN'placeID1038911218.mark CPN'inst,operator));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val CPN'trans_delay = CPN'Time.fromInt(discrete(TijdNaarGateWaarde))
 val _ = (CPN'VliegtuigOperator'timed_pims12_sims.addto (CPN'placeID1038911213.wait CPN'inst,CPN'TMS.@++([({vliegtuig= #vliegtuig vc,
operator=op2 })],CPN'trans_delay));
 CPN'DCoordinator_pims1.insert(CPN'placeID1047183094.mark CPN'inst,#dcoordinator vc);
 CPN'Operator_pims1.insert(CPN'placeID1038911218.mark CPN'inst,operator-op2));
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {vc=vc,operator=operator,op2=op2}
 val _ = (CPN'BRID1038934846:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - vc = ",VliegtuigDCoordinator.mkstr vc,"\n - operator = ",Operator.mkstr operator,"\n - op2 = ",Operator.mkstr op2]
else nil)
 end
 fun CPN'pick [CPN'i1,CPN'i2] = (CPN'nth(!CPN'bh1,CPN'i1),CPN'nth(!CPN'bh2,CPN'i2))
 fun CPN'bindings_as_strings() = [(["vc"],CPN'map (fn {vc} => [VliegtuigDCoordinator.mkstr vc]) (!CPN'bh1)),(["operator"],CPN'map (fn {operator} => [Operator.mkstr operator]) (!CPN'bh2))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_timed_place(1 <= CPN'placeID1038952654.init(CPN'inst),CPN'placeID1038952654.next_time (CPN'inst),
 CPN'Sim.each_place(1 <= CPN'placeID1038911218.init(CPN'inst),
 (true,CPN'Sim.is_disabled))))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1),CPN'hd(!CPN'bh2)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0,CPN'e1) = (CPN'e0,CPN'e1)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(expand1(!CPN'bh1,!CPN'bh2)))
 end

 end (* end CPN'transitionID1038934846 *) 
 val _ = CPN'Sim.add_be("ID1038934846",CPN'transitionID1038934846.CPN'bind_exe,CPN'transitionID1038934846.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1043445899 = {vo:VliegtuigOperator}
 type CPN'BRTID1043445899 = {vo:VliegtuigOperator}
 val CPN'BRID1043445899 = ref ({vo=VliegtuigOperator.base}: CPN'BRTID1043445899)
 structure CPN'transitionID1043445899 = (* DeicingProcedure'Opnieuw_de *)
 struct 
 val CPN'id = "ID1043445899"
 val CPN'name = "DeicingProcedure'Opnieuw_de"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["vo"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {vo: VliegtuigOperator} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf2() = (CPN'whole_binding := true)
 fun CPN'bf1() = 
 let (* timed B_p with coef 1 *)
 val _ = CPN'VliegtuigOperator'timed_pims12.init_res(CPN'placeID1038911193.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@(vo,CPN'time62) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'VliegtuigOperator'timed_pims12.random_res BindFailureGenAll (CPN'placeID1038911193.mark CPN'inst)
 | _ => CPN'VliegtuigOperator'timed_pims12.random_res BindFatalFailure (CPN'placeID1038911193.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'VliegtuigOperator'timed_pims12.res_col(CPN'placeID1038911193.mark CPN'inst, CPN'Time.@(vo,CPN'time62))
 | _ => ()
 in (
 (CPN'bh1::= {vo=vo};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({vo}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'VliegtuigOperator'timed_pims12.delete(CPN'placeID1038911193.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1043445899 CPN'placeID1038911193") (CPN'placeID1038911193.mark CPN'inst,CPN'VliegtuigOperator'timed_pims12.collect,CPN'VliegtuigOperator'timed_pims12.cmp,vo, time())));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val CPN'trans_delay = CPN'Time.fromInt( (if (#operator vo = 4) then round(normal(15.0,3.0))
else if (#operator vo = 6) then round(normal(10.0,2.0))
else if (#operator vo = 8) then round(normal(5.0,1.0))
else if (#operator vo = 10) then round(normal(4.0,0.5))
else round(normal(3.0,0.5))))
 val _ = (CPN'VliegtuigOperator'timed_pims12_sims.addto (CPN'placeID1043482406.wait CPN'inst,CPN'TMS.@++([(vo)],CPN'trans_delay)));
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {vo=vo}
 val _ = (CPN'BRID1043445899:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - vo = ",VliegtuigOperator.mkstr vo]
else nil)
 end
 fun CPN'pick [CPN'i1] = (CPN'nth(!CPN'bh1,CPN'i1))
 fun CPN'bindings_as_strings() = [(["vo"],CPN'map (fn {vo} => [VliegtuigOperator.mkstr vo]) (!CPN'bh1))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_timed_place(1 <= CPN'placeID1038911193.init(CPN'inst),CPN'placeID1038911193.next_time (CPN'inst),
 (true,CPN'Sim.is_disabled)))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0) = (CPN'e0)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(!CPN'bh1))
 end

 end (* end CPN'transitionID1043445899 *) 
 val _ = CPN'Sim.add_be("ID1043445899",CPN'transitionID1043445899.CPN'bind_exe,CPN'transitionID1043445899.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1043483044 = {operator:Operator}*{vo:VliegtuigOperator}
 type CPN'BRTID1043483044 = {operator:Operator,vo:VliegtuigOperator}
 val CPN'BRID1043483044 = ref ({operator=Operator.base,vo=VliegtuigOperator.base}: CPN'BRTID1043483044)
 structure CPN'transitionID1043483044 = (* DeicingProcedure'Operator_terug_naar_wachtplaats *)
 struct 
 val CPN'id = "ID1043483044"
 val CPN'name = "DeicingProcedure'Operator_terug_naar_wachtplaats"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["operator","vo"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {operator: Operator} list)
 val CPN'bh2 = ref(nil: {vo: VliegtuigOperator} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf3() = (CPN'whole_binding := true)
 fun CPN'bf2() = 
 let (* timed B_p with coef 1 *)
 val _ = CPN'VliegtuigOperator'timed_pims12.init_res(CPN'placeID1043482406.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@(vo,CPN'time65) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'VliegtuigOperator'timed_pims12.random_res BindFailureGenAll (CPN'placeID1043482406.mark CPN'inst)
 | _ => CPN'VliegtuigOperator'timed_pims12.random_res BindFatalFailure (CPN'placeID1043482406.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'VliegtuigOperator'timed_pims12.res_col(CPN'placeID1043482406.mark CPN'inst, CPN'Time.@(vo,CPN'time65))
 | _ => ()
 in (
 (CPN'bh2::= {vo=vo};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf3())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 fun CPN'bf1() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'Operator_pims1.init_res(CPN'placeID1038911218.mark CPN'inst)
 fun CPN'bf() = 
let
 val operator = case CPN'mode of 
CPN'Sim.all_enabled => CPN'Operator_pims1.random_res BindFailureGenAll (CPN'placeID1038911218.mark CPN'inst)
 | _ => CPN'Operator_pims1.random_res BindFatalFailure (CPN'placeID1038911218.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'Operator_pims1.res_col(CPN'placeID1038911218.mark CPN'inst,operator)
 | _ => ()
 in
 (
 (CPN'bh1::= {operator=operator};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
CPN'bh2:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ();
CPN'bf2() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ();
CPN'bf3() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({operator},{vo}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'Operator_pims1.delete(CPN'placeID1038911218.mark CPN'inst,operator);
 CPN'VliegtuigOperator'timed_pims12.delete(CPN'placeID1043482406.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1043483044 CPN'placeID1043482406") (CPN'placeID1043482406.mark CPN'inst,CPN'VliegtuigOperator'timed_pims12.collect,CPN'VliegtuigOperator'timed_pims12.cmp,vo, time())));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val CPN'trans_delay = CPN'Time.fromInt(0)
 val _ = (CPN'Vliegtuig'timed_pims123456_sims.addto (CPN'placeID1048683777.wait CPN'inst,CPN'TMS.@++([(RecordTime2(vo))],CPN'trans_delay));
 CPN'Operator_pims1.insert(CPN'placeID1038911218.mark CPN'inst,operator+
#operator vo));
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {operator=operator,vo=vo}
 val _ = (CPN'BRID1043483044:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - operator = ",Operator.mkstr operator,"\n - vo = ",VliegtuigOperator.mkstr vo]
else nil)
 end
 fun CPN'pick [CPN'i1,CPN'i2] = (CPN'nth(!CPN'bh1,CPN'i1),CPN'nth(!CPN'bh2,CPN'i2))
 fun CPN'bindings_as_strings() = [(["operator"],CPN'map (fn {operator} => [Operator.mkstr operator]) (!CPN'bh1)),(["vo"],CPN'map (fn {vo} => [VliegtuigOperator.mkstr vo]) (!CPN'bh2))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_place(1 <= CPN'placeID1038911218.init(CPN'inst),
 CPN'Sim.each_timed_place(1 <= CPN'placeID1043482406.init(CPN'inst),CPN'placeID1043482406.next_time (CPN'inst),
 (true,CPN'Sim.is_disabled))))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1),CPN'hd(!CPN'bh2)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0,CPN'e1) = (CPN'e0,CPN'e1)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(expand1(!CPN'bh1,!CPN'bh2)))
 end

 end (* end CPN'transitionID1043483044 *) 
 val _ = CPN'Sim.add_be("ID1043483044",CPN'transitionID1043483044.CPN'bind_exe,CPN'transitionID1043483044.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1050275404 = {vc:VliegtuigDCoordinator}*{ctlr:Controleur}
 type CPN'BRTID1050275404 = {vc:VliegtuigDCoordinator,ctlr:Controleur}
 val CPN'BRID1050275404 = ref ({vc=VliegtuigDCoordinator.base,ctlr=Controleur.base}: CPN'BRTID1050275404)
 structure CPN'transitionID1050275404 = (* DeicingProcedure'Controleur_naar_gate *)
 struct 
 val CPN'id = "ID1050275404"
 val CPN'name = "DeicingProcedure'Controleur_naar_gate"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["vc","ctlr"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {vc: VliegtuigDCoordinator} list)
 val CPN'bh2 = ref(nil: {ctlr: Controleur} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf3() = (CPN'whole_binding := true)
 fun CPN'bf2() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'Controleur_pims1.init_res(CPN'placeID1050276522.mark CPN'inst)
 fun CPN'bf() = 
let
 val ctlr = case CPN'mode of 
CPN'Sim.all_enabled => CPN'Controleur_pims1.random_res BindFailureGenAll (CPN'placeID1050276522.mark CPN'inst)
 | _ => CPN'Controleur_pims1.random_res BindFatalFailure (CPN'placeID1050276522.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'Controleur_pims1.res_col(CPN'placeID1050276522.mark CPN'inst,ctlr)
 | _ => ()
 in
 (
 (* T_g *)
 (if (ctlr>0) then
 (CPN'bh2::= {ctlr=ctlr};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf3())
 else raise BindFailure)
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 fun CPN'bf1() = 
 let (* timed B_p with coef 1 *)
 val _ = CPN'VliegtuigDCoordinator'timed_pims12.init_res(CPN'placeID1050274958.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@(vc,CPN'time67) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'VliegtuigDCoordinator'timed_pims12.random_res BindFailureGenAll (CPN'placeID1050274958.mark CPN'inst)
 | _ => CPN'VliegtuigDCoordinator'timed_pims12.random_res BindFatalFailure (CPN'placeID1050274958.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'VliegtuigDCoordinator'timed_pims12.res_col(CPN'placeID1050274958.mark CPN'inst, CPN'Time.@(vc,CPN'time67))
 | _ => ()
 in (
 (CPN'bh1::= {vc=vc};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
CPN'bh2:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ();
CPN'bf2() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ();
CPN'bf3() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({vc},{ctlr}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'VliegtuigDCoordinator'timed_pims12.delete(CPN'placeID1050274958.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1050275404 CPN'placeID1050274958") (CPN'placeID1050274958.mark CPN'inst,CPN'VliegtuigDCoordinator'timed_pims12.collect,CPN'VliegtuigDCoordinator'timed_pims12.cmp,vc, time()));
 CPN'Controleur_pims1.delete(CPN'placeID1050276522.mark CPN'inst,ctlr));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val CPN'trans_delay = CPN'Time.fromInt(discrete(TijdNaarGateWaarde))
 val _ = (CPN'DCoordinator_pims1.insert(CPN'placeID1050275904.mark CPN'inst,#dcoordinator vc);
 CPN'VliegtuigControleur'timed_pims12_sims.addto (CPN'placeID1050278947.wait CPN'inst,CPN'TMS.@++([({vliegtuig = #vliegtuig vc,
controleur= 1})],CPN'trans_delay));
 CPN'Controleur_pims1.insert(CPN'placeID1050276522.mark CPN'inst,ctlr-1));
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {vc=vc,ctlr=ctlr}
 val _ = (CPN'BRID1050275404:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - vc = ",VliegtuigDCoordinator.mkstr vc,"\n - ctlr = ",Controleur.mkstr ctlr]
else nil)
 end
 fun CPN'pick [CPN'i1,CPN'i2] = (CPN'nth(!CPN'bh1,CPN'i1),CPN'nth(!CPN'bh2,CPN'i2))
 fun CPN'bindings_as_strings() = [(["vc"],CPN'map (fn {vc} => [VliegtuigDCoordinator.mkstr vc]) (!CPN'bh1)),(["ctlr"],CPN'map (fn {ctlr} => [Controleur.mkstr ctlr]) (!CPN'bh2))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_timed_place(1 <= CPN'placeID1050274958.init(CPN'inst),CPN'placeID1050274958.next_time (CPN'inst),
 CPN'Sim.each_place(1 <= CPN'placeID1050276522.init(CPN'inst),
 (true,CPN'Sim.is_disabled))))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1),CPN'hd(!CPN'bh2)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0,CPN'e1) = (CPN'e0,CPN'e1)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(expand1(!CPN'bh1,!CPN'bh2)))
 end

 end (* end CPN'transitionID1050275404 *) 
 val _ = CPN'Sim.add_be("ID1050275404",CPN'transitionID1050275404.CPN'bind_exe,CPN'transitionID1050275404.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1043578478 = {vc:VliegtuigDCoordinator}
 type CPN'BRTID1043578478 = {vc:VliegtuigDCoordinator}
 val CPN'BRID1043578478 = ref ({vc=VliegtuigDCoordinator.base}: CPN'BRTID1043578478)
 structure CPN'transitionID1043578478 = (* InformatieNodig'Contact_opnemen_met_piloot *)
 struct 
 val CPN'id = "ID1043578478"
 val CPN'name = "InformatieNodig'Contact_opnemen_met_piloot"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["vc"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {vc: VliegtuigDCoordinator} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf2() = (CPN'whole_binding := true)
 fun CPN'bf1() = 
 let (* timed B_p with coef 1 *)
 val _ = CPN'VliegtuigDCoordinator'timed_pims12.init_res(CPN'placeID1043578489.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@(vc,CPN'time70) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'VliegtuigDCoordinator'timed_pims12.random_res BindFailureGenAll (CPN'placeID1043578489.mark CPN'inst)
 | _ => CPN'VliegtuigDCoordinator'timed_pims12.random_res BindFatalFailure (CPN'placeID1043578489.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'VliegtuigDCoordinator'timed_pims12.res_col(CPN'placeID1043578489.mark CPN'inst, CPN'Time.@(vc,CPN'time70))
 | _ => ()
 in (
 (CPN'bh1::= {vc=vc};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({vc}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'VliegtuigDCoordinator'timed_pims12.delete(CPN'placeID1043578489.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1043578478 CPN'placeID1043578489") (CPN'placeID1043578489.mark CPN'inst,CPN'VliegtuigDCoordinator'timed_pims12.collect,CPN'VliegtuigDCoordinator'timed_pims12.cmp,vc, time())));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val CPN'trans_delay = CPN'Time.fromInt(0)
 val _ = (CPN'VliegtuigDCoordinator'timed_pims12_sims.addto(CPN'placeID1043641221.wait CPN'inst,1`(vc )@+discrete
(TijdVragenInfoWaarde));
 CPN'VliegtuigDCoordinator'timed_pims12_sims.addto (CPN'placeID1049481985.wait CPN'inst,CPN'TMS.@++([(vc)],CPN'trans_delay)));
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {vc=vc}
 val _ = (CPN'BRID1043578478:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - vc = ",VliegtuigDCoordinator.mkstr vc]
else nil)
 end
 fun CPN'pick [CPN'i1] = (CPN'nth(!CPN'bh1,CPN'i1))
 fun CPN'bindings_as_strings() = [(["vc"],CPN'map (fn {vc} => [VliegtuigDCoordinator.mkstr vc]) (!CPN'bh1))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_timed_place(1 <= CPN'placeID1043578489.init(CPN'inst),CPN'placeID1043578489.next_time (CPN'inst),
 (true,CPN'Sim.is_disabled)))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0) = (CPN'e0)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(!CPN'bh1))
 end

 end (* end CPN'transitionID1043578478 *) 
 val _ = CPN'Sim.add_be("ID1043578478",CPN'transitionID1043578478.CPN'bind_exe,CPN'transitionID1043578478.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1043640484 = {vc:VliegtuigDCoordinator,con:Controle}
 type CPN'BRTID1043640484 = {vc:VliegtuigDCoordinator,con:Controle}
 val CPN'BRID1043640484 = ref ({vc=VliegtuigDCoordinator.base,con=Controle.base}: CPN'BRTID1043640484)
 structure CPN'transitionID1043640484 = (* InformatieNodig'Doorgeven_informatie_DeiceCoordinator *)
 struct 
 val CPN'id = "ID1043640484"
 val CPN'name = "InformatieNodig'Doorgeven_informatie_DeiceCoordinator"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["vc","con"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {vc: VliegtuigDCoordinator,con: Controle} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf2() = (CPN'whole_binding := true)
 fun CPN'bf1() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'Controle_pims12.init_res(CPN'placeID1049482106.mark CPN'inst)
 fun CPN'bf() = 
let
 val con = case CPN'mode of 
CPN'Sim.all_enabled => CPN'Controle_pims12.random_res BindFailureGenAll (CPN'placeID1049482106.mark CPN'inst)
 | _ => CPN'Controle_pims12.random_res BindFatalFailure (CPN'placeID1049482106.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'Controle_pims12.res_col(CPN'placeID1049482106.mark CPN'inst,con)
 | _ => ()
 in
 (
 let (* timed B_p with coef 1 *)
 val _ = CPN'VliegtuigDCoordinator'timed_pims12.init_res(CPN'placeID1043641221.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@(vc,CPN'time73) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'VliegtuigDCoordinator'timed_pims12.random_res BindFailure (CPN'placeID1043641221.mark CPN'inst)
 | _ => CPN'VliegtuigDCoordinator'timed_pims12.random_res BindFailure (CPN'placeID1043641221.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'VliegtuigDCoordinator'timed_pims12.res_col(CPN'placeID1043641221.mark CPN'inst, CPN'Time.@(vc,CPN'time73))
 | _ => ()
 in (
 (* T_g *)
 (if (#vliegID (#vliegtuig vc) = (#id con)) then
 (CPN'bh1::= {vc=vc,con=con};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 else raise BindFailure)
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({vc,con}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'VliegtuigDCoordinator'timed_pims12.delete(CPN'placeID1043641221.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1043640484 CPN'placeID1043641221") (CPN'placeID1043641221.mark CPN'inst,CPN'VliegtuigDCoordinator'timed_pims12.collect,CPN'VliegtuigDCoordinator'timed_pims12.cmp,vc, time()));
 CPN'Controle_pims12.delete(CPN'placeID1049482106.mark CPN'inst,con));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val CPN'trans_delay = CPN'Time.fromInt((2+discrete
(TijdDoorgevenInfoWaarde)))
 val _ = (CPN'VliegtuigDCoordinator'timed_pims12_sims.addto (CPN'placeID1044958654.wait CPN'inst,CPN'TMS.@++([(VliegtuigDCoordinator.
set_vliegtuig vc
(UpdateOnderdeel
(#vliegtuig vc, #l con)))],CPN'trans_delay)));
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {vc=vc,con=con}
 val _ = (CPN'BRID1043640484:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - vc = ",VliegtuigDCoordinator.mkstr vc,"\n - con = ",Controle.mkstr con]
else nil)
 end
 fun CPN'pick [CPN'i1] = (CPN'nth(!CPN'bh1,CPN'i1))
 fun CPN'bindings_as_strings() = [(["vc","con"],CPN'map (fn {vc,con} => [VliegtuigDCoordinator.mkstr vc,Controle.mkstr con]) (!CPN'bh1))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_timed_place(1 <= CPN'placeID1043641221.init(CPN'inst),CPN'placeID1043641221.next_time (CPN'inst),
 CPN'Sim.each_place(1 <= CPN'placeID1049482106.init(CPN'inst),
 (true,CPN'Sim.is_disabled))))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0) = (CPN'e0)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(!CPN'bh1))
 end

 end (* end CPN'transitionID1043640484 *) 
 val _ = CPN'Sim.add_be("ID1043640484",CPN'transitionID1043640484.CPN'bind_exe,CPN'transitionID1043640484.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1045021145 = {vo:VliegtuigOperator}*{vc:VliegtuigDCoordinator}
 type CPN'BRTID1045021145 = {vo:VliegtuigOperator,vc:VliegtuigDCoordinator}
 val CPN'BRID1045021145 = ref ({vo=VliegtuigOperator.base,vc=VliegtuigDCoordinator.base}: CPN'BRTID1045021145)
 structure CPN'transitionID1045021145 = (* InformatieNodig'Doorgeven_informatie_aan_Operator *)
 struct 
 val CPN'id = "ID1045021145"
 val CPN'name = "InformatieNodig'Doorgeven_informatie_aan_Operator"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["vo","vc"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {vo: VliegtuigOperator} list)
 val CPN'bh2 = ref(nil: {vc: VliegtuigDCoordinator} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf3() = (CPN'whole_binding := true)
 fun CPN'bf2() = 
 let (* timed B_p with coef 1 *)
 val _ = CPN'VliegtuigDCoordinator'timed_pims12.init_res(CPN'placeID1044958654.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@(vc,CPN'time76) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'VliegtuigDCoordinator'timed_pims12.random_res BindFailureGenAll (CPN'placeID1044958654.mark CPN'inst)
 | _ => CPN'VliegtuigDCoordinator'timed_pims12.random_res BindFatalFailure (CPN'placeID1044958654.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'VliegtuigDCoordinator'timed_pims12.res_col(CPN'placeID1044958654.mark CPN'inst, CPN'Time.@(vc,CPN'time76))
 | _ => ()
 in (
 (CPN'bh2::= {vc=vc};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf3())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 fun CPN'bf1() = 
 let (* timed B_p with coef 1 *)
 val _ = CPN'VliegtuigOperator'timed_pims12.init_res(CPN'placeID1044947601.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@(vo,CPN'time75) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'VliegtuigOperator'timed_pims12.random_res BindFailureGenAll (CPN'placeID1044947601.mark CPN'inst)
 | _ => CPN'VliegtuigOperator'timed_pims12.random_res BindFatalFailure (CPN'placeID1044947601.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'VliegtuigOperator'timed_pims12.res_col(CPN'placeID1044947601.mark CPN'inst, CPN'Time.@(vo,CPN'time75))
 | _ => ()
 in (
 (CPN'bh1::= {vo=vo};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
CPN'bh2:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ();
CPN'bf2() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ();
CPN'bf3() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({vo},{vc}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'VliegtuigOperator'timed_pims12.delete(CPN'placeID1044947601.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1045021145 CPN'placeID1044947601") (CPN'placeID1044947601.mark CPN'inst,CPN'VliegtuigOperator'timed_pims12.collect,CPN'VliegtuigOperator'timed_pims12.cmp,vo, time()));
 CPN'VliegtuigDCoordinator'timed_pims12.delete(CPN'placeID1044958654.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1045021145 CPN'placeID1044958654") (CPN'placeID1044958654.mark CPN'inst,CPN'VliegtuigDCoordinator'timed_pims12.collect,CPN'VliegtuigDCoordinator'timed_pims12.cmp,vc, time())));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val CPN'trans_delay = CPN'Time.fromInt((2+discrete
(TijdDoorgevenInfoWaarde)))
 val _ = (CPN'VliegtuigDCoordinator'timed_pims12_sims.addto (CPN'placeID1045071750.wait CPN'inst,CPN'TMS.@++([(vc)],CPN'trans_delay));
 CPN'VliegtuigDCoordinator'timed_pims12_sims.addto (CPN'placeID1049481985.wait CPN'inst,CPN'TMS.@++([(vc)],CPN'trans_delay));
 CPN'VliegtuigOperator'timed_pims12_sims.addto (CPN'placeID1044947601.wait CPN'inst,CPN'TMS.@++([({vliegtuig= #vliegtuig vo,
operator= #operator vo -1})],CPN'trans_delay)));
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {vo=vo,vc=vc}
 val _ = (CPN'BRID1045021145:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - vo = ",VliegtuigOperator.mkstr vo,"\n - vc = ",VliegtuigDCoordinator.mkstr vc]
else nil)
 end
 fun CPN'pick [CPN'i1,CPN'i2] = (CPN'nth(!CPN'bh1,CPN'i1),CPN'nth(!CPN'bh2,CPN'i2))
 fun CPN'bindings_as_strings() = [(["vo"],CPN'map (fn {vo} => [VliegtuigOperator.mkstr vo]) (!CPN'bh1)),(["vc"],CPN'map (fn {vc} => [VliegtuigDCoordinator.mkstr vc]) (!CPN'bh2))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_timed_place(1 <= CPN'placeID1044947601.init(CPN'inst),CPN'placeID1044947601.next_time (CPN'inst),
 CPN'Sim.each_timed_place(1 <= CPN'placeID1044958654.init(CPN'inst),CPN'placeID1044958654.next_time (CPN'inst),
 (true,CPN'Sim.is_disabled))))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1),CPN'hd(!CPN'bh2)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0,CPN'e1) = (CPN'e0,CPN'e1)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(expand1(!CPN'bh1,!CPN'bh2)))
 end

 end (* end CPN'transitionID1045021145 *) 
 val _ = CPN'Sim.add_be("ID1045021145",CPN'transitionID1045021145.CPN'bind_exe,CPN'transitionID1045021145.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1045073546 = {vc:VliegtuigDCoordinator,con:Controle,vo:VliegtuigOperator}
 type CPN'BRTID1045073546 = {vc:VliegtuigDCoordinator,con:Controle,vo:VliegtuigOperator}
 val CPN'BRID1045073546 = ref ({vc=VliegtuigDCoordinator.base,con=Controle.base,vo=VliegtuigOperator.base}: CPN'BRTID1045073546)
 structure CPN'transitionID1045073546 = (* InformatieNodig'Onderdelen_naar_operator *)
 struct 
 val CPN'id = "ID1045073546"
 val CPN'name = "InformatieNodig'Onderdelen_naar_operator"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["vc","con","vo"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {vc: VliegtuigDCoordinator,con: Controle,vo: VliegtuigOperator} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf2() = (CPN'whole_binding := true)
 fun CPN'bf1() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'Controle_pims12.init_res(CPN'placeID1049482106.mark CPN'inst)
 fun CPN'bf() = 
let
 val con = case CPN'mode of 
CPN'Sim.all_enabled => CPN'Controle_pims12.random_res BindFailureGenAll (CPN'placeID1049482106.mark CPN'inst)
 | _ => CPN'Controle_pims12.random_res BindFatalFailure (CPN'placeID1049482106.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'Controle_pims12.res_col(CPN'placeID1049482106.mark CPN'inst,con)
 | _ => ()
 in
 (
 let (* timed B_p with coef 1 *)
 val _ = CPN'VliegtuigDCoordinator'timed_pims12.init_res(CPN'placeID1045071750.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@(vc,CPN'time79) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'VliegtuigDCoordinator'timed_pims12.random_res BindFailure (CPN'placeID1045071750.mark CPN'inst)
 | _ => CPN'VliegtuigDCoordinator'timed_pims12.random_res BindFailure (CPN'placeID1045071750.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'VliegtuigDCoordinator'timed_pims12.res_col(CPN'placeID1045071750.mark CPN'inst, CPN'Time.@(vc,CPN'time79))
 | _ => ()
 in (
 let (* timed B_p with coef 1 *)
 val _ = CPN'VliegtuigOperator'timed_pims12.init_res(CPN'placeID1044947601.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@(vo,CPN'time80) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'VliegtuigOperator'timed_pims12.random_res BindFailure (CPN'placeID1044947601.mark CPN'inst)
 | _ => CPN'VliegtuigOperator'timed_pims12.random_res BindFailure (CPN'placeID1044947601.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'VliegtuigOperator'timed_pims12.res_col(CPN'placeID1044947601.mark CPN'inst, CPN'Time.@(vo,CPN'time80))
 | _ => ()
 in (
 (* T_g *)
 (if (#vliegID (#vliegtuig vc)= #id con andalso
#vliegID (#vliegtuig vo) = #id con) then
 (CPN'bh1::= {vc=vc,con=con,vo=vo};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 else raise BindFailure)
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({vc,con,vo}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'VliegtuigDCoordinator'timed_pims12.delete(CPN'placeID1045071750.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1045073546 CPN'placeID1045071750") (CPN'placeID1045071750.mark CPN'inst,CPN'VliegtuigDCoordinator'timed_pims12.collect,CPN'VliegtuigDCoordinator'timed_pims12.cmp,vc, time()));
 CPN'Controle_pims12.delete(CPN'placeID1049482106.mark CPN'inst,con);
 CPN'VliegtuigOperator'timed_pims12.delete(CPN'placeID1044947601.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1045073546 CPN'placeID1044947601") (CPN'placeID1044947601.mark CPN'inst,CPN'VliegtuigOperator'timed_pims12.collect,CPN'VliegtuigOperator'timed_pims12.cmp,vo, time())));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val CPN'trans_delay = CPN'Time.fromInt(0)
 val _ = (CPN'Vliegtuig'timed_pims123456_sims.addto (CPN'placeID1043578495.wait CPN'inst,CPN'TMS.@++([(UpdateOnderdeel2
(vc,#l con))],CPN'trans_delay));
 CPN'DCoordinator_pims1.insert(CPN'placeID1043578483.mark CPN'inst,#dcoordinator vc);
 CPN'VliegtuigOperator'timed_pims12_sims.addto (CPN'placeID1044947601.wait CPN'inst,CPN'TMS.@++([({vliegtuig= #vliegtuig vo,
operator= #operator vo +1})],CPN'trans_delay)));
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {vc=vc,con=con,vo=vo}
 val _ = (CPN'BRID1045073546:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - vc = ",VliegtuigDCoordinator.mkstr vc,"\n - con = ",Controle.mkstr con,"\n - vo = ",VliegtuigOperator.mkstr vo]
else nil)
 end
 fun CPN'pick [CPN'i1] = (CPN'nth(!CPN'bh1,CPN'i1))
 fun CPN'bindings_as_strings() = [(["vc","con","vo"],CPN'map (fn {vc,con,vo} => [VliegtuigDCoordinator.mkstr vc,Controle.mkstr con,VliegtuigOperator.mkstr vo]) (!CPN'bh1))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_timed_place(1 <= CPN'placeID1045071750.init(CPN'inst),CPN'placeID1045071750.next_time (CPN'inst),
 CPN'Sim.each_place(1 <= CPN'placeID1049482106.init(CPN'inst),
 CPN'Sim.each_timed_place(1 <= CPN'placeID1044947601.init(CPN'inst),CPN'placeID1044947601.next_time (CPN'inst),
 (true,CPN'Sim.is_disabled)))))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0) = (CPN'e0)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(!CPN'bh1))
 end

 end (* end CPN'transitionID1045073546 *) 
 val _ = CPN'Sim.add_be("ID1045073546",CPN'transitionID1045073546.CPN'bind_exe,CPN'transitionID1045073546.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1045334239 = {vctlr:VliegtuigControleur}*{im:Iceman}*{vo:VliegtuigOperator,v:Vliegtuig}
 type CPN'BRTID1045334239 = {vctlr:VliegtuigControleur,im:Iceman,vo:VliegtuigOperator,v:Vliegtuig}
 val CPN'BRID1045334239 = ref ({vctlr=VliegtuigControleur.base,im=Iceman.base,vo=VliegtuigOperator.base,v=Vliegtuig.base}: CPN'BRTID1045334239)
 structure CPN'transitionID1045334239 = (* Deicing'Een *)
 struct 
 val CPN'id = "ID1045334239"
 val CPN'name = "Deicing'Een"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["vctlr","im","vo","v"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {vctlr: VliegtuigControleur} list)
 val CPN'bh2 = ref(nil: {im: Iceman} list)
 val CPN'bh3 = ref(nil: {vo: VliegtuigOperator,v: Vliegtuig} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf4() = (CPN'whole_binding := true)
 fun CPN'bf3() = 
 let (* timed B_p with coef 1 *)
 val _ = CPN'Vliegtuig'timed_pims123456.init_res(CPN'placeID1045334257.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@(v,CPN'time84) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'Vliegtuig'timed_pims123456.random_res BindFailureGenAll (CPN'placeID1045334257.mark CPN'inst)
 | _ => CPN'Vliegtuig'timed_pims123456.random_res BindFatalFailure (CPN'placeID1045334257.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'Vliegtuig'timed_pims123456.res_col(CPN'placeID1045334257.mark CPN'inst, CPN'Time.@(v,CPN'time84))
 | _ => ()
 in (
 let (* timed B_p with coef 1 *)
 val _ = CPN'VliegtuigOperator'timed_pims12.init_res(CPN'placeID1045334250.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@(vo,CPN'time85) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'VliegtuigOperator'timed_pims12.random_res BindFailure (CPN'placeID1045334250.mark CPN'inst)
 | _ => CPN'VliegtuigOperator'timed_pims12.random_res BindFailure (CPN'placeID1045334250.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'VliegtuigOperator'timed_pims12.res_col(CPN'placeID1045334250.mark CPN'inst, CPN'Time.@(vo,CPN'time85))
 | _ => ()
 in (
 (* T_g *)
 (if (#vliegID (#vliegtuig vo) = (#vliegID v)) then
 (CPN'bh3::= {vo=vo,v=v};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf4())
 else raise BindFailure)
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 fun CPN'bf2() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'Iceman_pims1.init_res(CPN'placeID1045334268.mark CPN'inst)
 fun CPN'bf() = 
let
 val im = case CPN'mode of 
CPN'Sim.all_enabled => CPN'Iceman_pims1.random_res BindFailureGenAll (CPN'placeID1045334268.mark CPN'inst)
 | _ => CPN'Iceman_pims1.random_res BindFatalFailure (CPN'placeID1045334268.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'Iceman_pims1.res_col(CPN'placeID1045334268.mark CPN'inst,im)
 | _ => ()
 in
 (
 (CPN'bh2::= {im=im};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf3())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 fun CPN'bf1() = 
 let (* timed B_p with coef 1 *)
 val _ = CPN'VliegtuigControleur'timed_pims12.init_res(CPN'placeID1050283044.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@(vctlr,CPN'time82) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'VliegtuigControleur'timed_pims12.random_res BindFailureGenAll (CPN'placeID1050283044.mark CPN'inst)
 | _ => CPN'VliegtuigControleur'timed_pims12.random_res BindFatalFailure (CPN'placeID1050283044.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'VliegtuigControleur'timed_pims12.res_col(CPN'placeID1050283044.mark CPN'inst, CPN'Time.@(vctlr,CPN'time82))
 | _ => ()
 in (
 (CPN'bh1::= {vctlr=vctlr};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
CPN'bh2:=nil;
CPN'bh3:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ();
CPN'bf2() handle BoundGroup => ();
CPN'bf3() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ();
CPN'bf3() handle BindFailureGenAll => ();
CPN'bf4() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({vctlr},{im},{vo,v}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'VliegtuigOperator'timed_pims12.delete(CPN'placeID1045334250.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1045334239 CPN'placeID1045334250") (CPN'placeID1045334250.mark CPN'inst,CPN'VliegtuigOperator'timed_pims12.collect,CPN'VliegtuigOperator'timed_pims12.cmp,vo, time()));
 CPN'Vliegtuig'timed_pims123456.delete(CPN'placeID1045334257.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1045334239 CPN'placeID1045334257") (CPN'placeID1045334257.mark CPN'inst,CPN'Vliegtuig'timed_pims123456.collect,CPN'Vliegtuig'timed_pims123456.cmp,v, time()));
 CPN'VliegtuigControleur'timed_pims12.delete(CPN'placeID1050283044.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1045334239 CPN'placeID1050283044") (CPN'placeID1050283044.mark CPN'inst,CPN'VliegtuigControleur'timed_pims12.collect,CPN'VliegtuigControleur'timed_pims12.cmp,vctlr, time()));
 CPN'Iceman_pims1.delete(CPN'placeID1045334268.mark CPN'inst,im));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val CPN'trans_delay = CPN'Time.fromInt(0)
 val _ = (CPN'VliegtuigOperatorIcemanControleur'timed_pims1234_sims.addto (CPN'placeID1045362911.wait CPN'inst,CPN'TMS.@++([({vliegtuig= v, 
operator= #operator vo,
iceman=EenOfTwee(),
controleur= #controleur vctlr})],CPN'trans_delay)));
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {vctlr=vctlr,im=im,vo=vo,v=v}
 val _ = (CPN'BRID1045334239:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - vctlr = ",VliegtuigControleur.mkstr vctlr,"\n - im = ",Iceman.mkstr im,"\n - vo = ",VliegtuigOperator.mkstr vo,"\n - v = ",Vliegtuig.mkstr v]
else nil)
 end
 fun CPN'pick [CPN'i1,CPN'i2,CPN'i3] = (CPN'nth(!CPN'bh1,CPN'i1),CPN'nth(!CPN'bh2,CPN'i2),CPN'nth(!CPN'bh3,CPN'i3))
 fun CPN'bindings_as_strings() = [(["vctlr"],CPN'map (fn {vctlr} => [VliegtuigControleur.mkstr vctlr]) (!CPN'bh1)),(["im"],CPN'map (fn {im} => [Iceman.mkstr im]) (!CPN'bh2)),(["vo","v"],CPN'map (fn {vo,v} => [VliegtuigOperator.mkstr vo,Vliegtuig.mkstr v]) (!CPN'bh3))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_timed_place(1 <= CPN'placeID1045334250.init(CPN'inst),CPN'placeID1045334250.next_time (CPN'inst),
 CPN'Sim.each_timed_place(1 <= CPN'placeID1045334257.init(CPN'inst),CPN'placeID1045334257.next_time (CPN'inst),
 CPN'Sim.each_timed_place(1 <= CPN'placeID1050283044.init(CPN'inst),CPN'placeID1050283044.next_time (CPN'inst),
 CPN'Sim.each_place(1 <= CPN'placeID1045334268.init(CPN'inst),
 (true,CPN'Sim.is_disabled))))))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1),CPN'hd(!CPN'bh2),CPN'hd(!CPN'bh3)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0,(CPN'e1,CPN'e2)) = (CPN'e0,CPN'e1,CPN'e2)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(expand1(!CPN'bh1,expand1(!CPN'bh2,!CPN'bh3))))
 end

 end (* end CPN'transitionID1045334239 *) 
 val _ = CPN'Sim.add_be("ID1045334239",CPN'transitionID1045334239.CPN'bind_exe,CPN'transitionID1045334239.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1045365136 = {voi:VliegtuigOperatorIcemanControleur}
 type CPN'BRTID1045365136 = {voi:VliegtuigOperatorIcemanControleur}
 val CPN'BRID1045365136 = ref ({voi=VliegtuigOperatorIcemanControleur.base}: CPN'BRTID1045365136)
 structure CPN'transitionID1045365136 = (* Deicing'Stap_1_van_1 *)
 struct 
 val CPN'id = "ID1045365136"
 val CPN'name = "Deicing'Stap_1_van_1"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["voi"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {voi: VliegtuigOperatorIcemanControleur} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf2() = (CPN'whole_binding := true)
 fun CPN'bf1() = 
 let (* timed B_p with coef 1 *)
 val _ = CPN'VliegtuigOperatorIcemanControleur'timed_pims1234.init_res(CPN'placeID1045362911.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@(voi,CPN'time87) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'VliegtuigOperatorIcemanControleur'timed_pims1234.random_res BindFailureGenAll (CPN'placeID1045362911.mark CPN'inst)
 | _ => CPN'VliegtuigOperatorIcemanControleur'timed_pims1234.random_res BindFatalFailure (CPN'placeID1045362911.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'VliegtuigOperatorIcemanControleur'timed_pims1234.res_col(CPN'placeID1045362911.mark CPN'inst, CPN'Time.@(voi,CPN'time87))
 | _ => ()
 in (
 (* T_g *)
 (if (#iceman voi = 1) then
 (CPN'bh1::= {voi=voi};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 else raise BindFailure)
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({voi}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'VliegtuigOperatorIcemanControleur'timed_pims1234.delete(CPN'placeID1045362911.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1045365136 CPN'placeID1045362911") (CPN'placeID1045362911.mark CPN'inst,CPN'VliegtuigOperatorIcemanControleur'timed_pims1234.collect,CPN'VliegtuigOperatorIcemanControleur'timed_pims1234.cmp,voi, time())));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val CPN'trans_delay = CPN'Time.fromInt( (if (#operator voi = 4) then round(normal(15.0,3.0))
else if (#operator voi = 6) then round(normal(10.0,2.0))
else if (#operator voi = 8) then round(normal(5.0,1.0))
else if (#operator voi = 10) then round(normal(4.0,0.5))
else round(normal(3.0,0.5))))
 val _ = (CPN'VliegtuigOperatorControleur'timed_pims123_sims.addto (CPN'placeID1045334262.wait CPN'inst,CPN'TMS.@++([({vliegtuig = #vliegtuig voi,
operator= #operator voi,
controleur = # controleur voi})],CPN'trans_delay));
 CPN'Iceman_pims1.insert(CPN'placeID1045334268.mark CPN'inst,#iceman voi));
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {voi=voi}
 val _ = (CPN'BRID1045365136:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - voi = ",VliegtuigOperatorIcemanControleur.mkstr voi]
else nil)
 end
 fun CPN'pick [CPN'i1] = (CPN'nth(!CPN'bh1,CPN'i1))
 fun CPN'bindings_as_strings() = [(["voi"],CPN'map (fn {voi} => [VliegtuigOperatorIcemanControleur.mkstr voi]) (!CPN'bh1))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_timed_place(1 <= CPN'placeID1045362911.init(CPN'inst),CPN'placeID1045362911.next_time (CPN'inst),
 (true,CPN'Sim.is_disabled)))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0) = (CPN'e0)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(!CPN'bh1))
 end

 end (* end CPN'transitionID1045365136 *) 
 val _ = CPN'Sim.add_be("ID1045365136",CPN'transitionID1045365136.CPN'bind_exe,CPN'transitionID1045365136.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1045378517 = {voc:VliegtuigOperatorControleur}
 type CPN'BRTID1045378517 = {voc:VliegtuigOperatorControleur}
 val CPN'BRID1045378517 = ref ({voc=VliegtuigOperatorControleur.base}: CPN'BRTID1045378517)
 structure CPN'transitionID1045378517 = (* Deicing'Stap_2_van_2 *)
 struct 
 val CPN'id = "ID1045378517"
 val CPN'name = "Deicing'Stap_2_van_2"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["voc"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {voc: VliegtuigOperatorControleur} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf2() = (CPN'whole_binding := true)
 fun CPN'bf1() = 
 let (* timed B_p with coef 1 *)
 val _ = CPN'VliegtuigOperatorControleur'timed_pims123.init_res(CPN'placeID1045430800.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@(voc,CPN'time89) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'VliegtuigOperatorControleur'timed_pims123.random_res BindFailureGenAll (CPN'placeID1045430800.mark CPN'inst)
 | _ => CPN'VliegtuigOperatorControleur'timed_pims123.random_res BindFatalFailure (CPN'placeID1045430800.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'VliegtuigOperatorControleur'timed_pims123.res_col(CPN'placeID1045430800.mark CPN'inst, CPN'Time.@(voc,CPN'time89))
 | _ => ()
 in (
 (CPN'bh1::= {voc=voc};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({voc}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'VliegtuigOperatorControleur'timed_pims123.delete(CPN'placeID1045430800.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1045378517 CPN'placeID1045430800") (CPN'placeID1045430800.mark CPN'inst,CPN'VliegtuigOperatorControleur'timed_pims123.collect,CPN'VliegtuigOperatorControleur'timed_pims123.cmp,voc, time())));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val CPN'trans_delay = CPN'Time.fromInt( (if (#operator voc = 4) then round(normal(8.0,3.0))
else if (#operator voc = 6) then round(normal(6.0,2.0))
else if (#operator voc = 8) then round(normal(4.0,1.0))
else if (#operator voc = 10) then round(normal(3.0,0.5))
else round(normal(2.0,0.5))))
 val _ = (CPN'VliegtuigOperatorControleur'timed_pims123_sims.addto (CPN'placeID1045334262.wait CPN'inst,CPN'TMS.@++([(voc)],CPN'trans_delay)));
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {voc=voc}
 val _ = (CPN'BRID1045378517:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - voc = ",VliegtuigOperatorControleur.mkstr voc]
else nil)
 end
 fun CPN'pick [CPN'i1] = (CPN'nth(!CPN'bh1,CPN'i1))
 fun CPN'bindings_as_strings() = [(["voc"],CPN'map (fn {voc} => [VliegtuigOperatorControleur.mkstr voc]) (!CPN'bh1))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_timed_place(1 <= CPN'placeID1045430800.init(CPN'inst),CPN'placeID1045430800.next_time (CPN'inst),
 (true,CPN'Sim.is_disabled)))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0) = (CPN'e0)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(!CPN'bh1))
 end

 end (* end CPN'transitionID1045378517 *) 
 val _ = CPN'Sim.add_be("ID1045378517",CPN'transitionID1045378517.CPN'bind_exe,CPN'transitionID1045378517.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1045394263 = {voi:VliegtuigOperatorIcemanControleur}
 type CPN'BRTID1045394263 = {voi:VliegtuigOperatorIcemanControleur}
 val CPN'BRID1045394263 = ref ({voi=VliegtuigOperatorIcemanControleur.base}: CPN'BRTID1045394263)
 structure CPN'transitionID1045394263 = (* Deicing'Stap_1_van_2 *)
 struct 
 val CPN'id = "ID1045394263"
 val CPN'name = "Deicing'Stap_1_van_2"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["voi"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {voi: VliegtuigOperatorIcemanControleur} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf2() = (CPN'whole_binding := true)
 fun CPN'bf1() = 
 let (* timed B_p with coef 1 *)
 val _ = CPN'VliegtuigOperatorIcemanControleur'timed_pims1234.init_res(CPN'placeID1045362911.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@(voi,CPN'time91) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'VliegtuigOperatorIcemanControleur'timed_pims1234.random_res BindFailureGenAll (CPN'placeID1045362911.mark CPN'inst)
 | _ => CPN'VliegtuigOperatorIcemanControleur'timed_pims1234.random_res BindFatalFailure (CPN'placeID1045362911.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'VliegtuigOperatorIcemanControleur'timed_pims1234.res_col(CPN'placeID1045362911.mark CPN'inst, CPN'Time.@(voi,CPN'time91))
 | _ => ()
 in (
 (* T_g *)
 (if (#iceman voi = 2) then
 (CPN'bh1::= {voi=voi};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 else raise BindFailure)
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({voi}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'VliegtuigOperatorIcemanControleur'timed_pims1234.delete(CPN'placeID1045362911.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1045394263 CPN'placeID1045362911") (CPN'placeID1045362911.mark CPN'inst,CPN'VliegtuigOperatorIcemanControleur'timed_pims1234.collect,CPN'VliegtuigOperatorIcemanControleur'timed_pims1234.cmp,voi, time())));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val CPN'trans_delay = CPN'Time.fromInt( (if (#operator voi = 4) then round(normal(15.0,3.0))
else if (#operator voi = 6) then round(normal(10.0,2.0))
else if (#operator voi = 8) then round(normal(5.0,1.0))
else if (#operator voi = 10) then round(normal(4.0,0.5))
else round(normal(3.0,0.5))))
 val _ = (CPN'VliegtuigOperatorControleur'timed_pims123_sims.addto (CPN'placeID1045430800.wait CPN'inst,CPN'TMS.@++([({vliegtuig = #vliegtuig voi,
operator = #operator voi,
controleur = #controleur voi})],CPN'trans_delay));
 CPN'Iceman_pims1.insert(CPN'placeID1045334268.mark CPN'inst,#iceman voi));
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {voi=voi}
 val _ = (CPN'BRID1045394263:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - voi = ",VliegtuigOperatorIcemanControleur.mkstr voi]
else nil)
 end
 fun CPN'pick [CPN'i1] = (CPN'nth(!CPN'bh1,CPN'i1))
 fun CPN'bindings_as_strings() = [(["voi"],CPN'map (fn {voi} => [VliegtuigOperatorIcemanControleur.mkstr voi]) (!CPN'bh1))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_timed_place(1 <= CPN'placeID1045362911.init(CPN'inst),CPN'placeID1045362911.next_time (CPN'inst),
 (true,CPN'Sim.is_disabled)))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0) = (CPN'e0)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(!CPN'bh1))
 end

 end (* end CPN'transitionID1045394263 *) 
 val _ = CPN'Sim.add_be("ID1045394263",CPN'transitionID1045394263.CPN'bind_exe,CPN'transitionID1045394263.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1049486729 = {i:INT}*{vc:VliegtuigDCoordinator}
 type CPN'BRTID1049486729 = {i:INT,vc:VliegtuigDCoordinator}
 val CPN'BRID1049486729 = ref ({i=INT.base,vc=VliegtuigDCoordinator.base}: CPN'BRTID1049486729)
 structure CPN'transitionID1049486729 = (* InformatieVragen'Start_doorgeven_onderdelen *)
 struct 
 val CPN'id = "ID1049486729"
 val CPN'name = "InformatieVragen'Start_doorgeven_onderdelen"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["i","vc"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {i: INT} list)
 val CPN'bh2 = ref(nil: {vc: VliegtuigDCoordinator} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf3() = (CPN'whole_binding := true)
 fun CPN'bf2() = 
 let (* timed B_p with coef 1 *)
 val _ = CPN'VliegtuigDCoordinator'timed_pims12.init_res(CPN'placeID1049486744.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@(vc,CPN'time94) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'VliegtuigDCoordinator'timed_pims12.random_res BindFailureGenAll (CPN'placeID1049486744.mark CPN'inst)
 | _ => CPN'VliegtuigDCoordinator'timed_pims12.random_res BindFatalFailure (CPN'placeID1049486744.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'VliegtuigDCoordinator'timed_pims12.res_col(CPN'placeID1049486744.mark CPN'inst, CPN'Time.@(vc,CPN'time94))
 | _ => ()
 in (
 (CPN'bh2::= {vc=vc};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf3())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 fun CPN'bf1() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'INT_pims1.init_res(CPN'placeID1049486754.mark CPN'inst)
 fun CPN'bf() = 
let
 val i = case CPN'mode of 
CPN'Sim.all_enabled => CPN'INT_pims1.random_res BindFailureGenAll (CPN'placeID1049486754.mark CPN'inst)
 | _ => CPN'INT_pims1.random_res BindFatalFailure (CPN'placeID1049486754.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'INT_pims1.res_col(CPN'placeID1049486754.mark CPN'inst,i)
 | _ => ()
 in
 (
 (CPN'bh1::= {i=i};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
CPN'bh2:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ();
CPN'bf2() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ();
CPN'bf3() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({i},{vc}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'INT_pims1.delete(CPN'placeID1049486754.mark CPN'inst,i);
 CPN'VliegtuigDCoordinator'timed_pims12.delete(CPN'placeID1049486744.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1049486729 CPN'placeID1049486744") (CPN'placeID1049486744.mark CPN'inst,CPN'VliegtuigDCoordinator'timed_pims12.collect,CPN'VliegtuigDCoordinator'timed_pims12.cmp,vc, time())));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val _ = (CPN'LOnderdelen_pims1.insert(CPN'placeID1049486734.mark CPN'inst,#listonderdelen
(#vliegtuig vc));
 CPN'INT_pims1.insert(CPN'placeID1049486759.mark CPN'inst,#vliegID (#vliegtuig vc));
 CPN'INT_pims1.insert(CPN'placeID1049486749.mark CPN'inst,length
(#listonderdelen
(#vliegtuig vc))));
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {i=i,vc=vc}
 val _ = (CPN'BRID1049486729:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - i = ",INT.mkstr i,"\n - vc = ",VliegtuigDCoordinator.mkstr vc]
else nil)
 end
 fun CPN'pick [CPN'i1,CPN'i2] = (CPN'nth(!CPN'bh1,CPN'i1),CPN'nth(!CPN'bh2,CPN'i2))
 fun CPN'bindings_as_strings() = [(["i"],CPN'map (fn {i} => [INT.mkstr i]) (!CPN'bh1)),(["vc"],CPN'map (fn {vc} => [VliegtuigDCoordinator.mkstr vc]) (!CPN'bh2))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_place(1 <= CPN'placeID1049486754.init(CPN'inst),
 CPN'Sim.each_timed_place(1 <= CPN'placeID1049486744.init(CPN'inst),CPN'placeID1049486744.next_time (CPN'inst),
 (true,CPN'Sim.is_disabled))))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1),CPN'hd(!CPN'bh2)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0,CPN'e1) = (CPN'e0,CPN'e1)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(expand1(!CPN'bh1,!CPN'bh2)))
 end

 end (* end CPN'transitionID1049486729 *) 
 val _ = CPN'Sim.add_be("ID1049486729",CPN'transitionID1049486729.CPN'bind_exe,CPN'transitionID1049486729.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1049489165 = {londerdelen:LOnderdelen,ond:Onderdelen}
 type CPN'BRTID1049489165 = {londerdelen:LOnderdelen,ond:Onderdelen}
 val CPN'BRID1049489165 = ref ({londerdelen=LOnderdelen.base,ond=Onderdelen.base}: CPN'BRTID1049489165)
 structure CPN'transitionID1049489165 = (* InformatieVragen'Onderdelen_uit_lijst_halen *)
 struct 
 val CPN'id = "ID1049489165"
 val CPN'name = "InformatieVragen'Onderdelen_uit_lijst_halen"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["londerdelen","ond"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {londerdelen: LOnderdelen,ond: Onderdelen} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf2() = (CPN'whole_binding := true)
 fun CPN'bf1() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'LOnderdelen_pims1.init_res(CPN'placeID1049486734.mark CPN'inst)
 fun CPN'bf() = 
let
 val ond::londerdelen = case CPN'mode of 
CPN'Sim.all_enabled => CPN'LOnderdelen_pims1.random_res BindFailureGenAll (CPN'placeID1049486734.mark CPN'inst)
 | _ => CPN'LOnderdelen_pims1.random_res BindFatalFailure (CPN'placeID1049486734.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'LOnderdelen_pims1.res_col(CPN'placeID1049486734.mark CPN'inst,ond::londerdelen)
 | _ => ()
 in
 (
 (CPN'bh1::= {londerdelen=londerdelen,ond=ond};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({londerdelen,ond}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'LOnderdelen_pims1.delete(CPN'placeID1049486734.mark CPN'inst,ond::londerdelen));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val _ = (CPN'LOnderdelen_pims1.insert(CPN'placeID1049486734.mark CPN'inst,londerdelen);
 CPN'Onderdelen_pims1.insert(CPN'placeID1049491231.mark CPN'inst,ond);
 CPN'WelNiet_pims1.insert(CPN'placeID1049491983.mark CPN'inst,binomial
(KansPortoWerktWelWaarde));
 if WelNiet.legal (binomial
(KansPortoWerktWelWaarde)) then () else CPN'suberr::= ("ID1049492752", WelNiet.illegal_msg (binomial
(KansPortoWerktWelWaarde))));
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {londerdelen=londerdelen,ond=ond}
 val _ = (CPN'BRID1049489165:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - londerdelen = ",LOnderdelen.mkstr londerdelen,"\n - ond = ",Onderdelen.mkstr ond]
else nil)
 end
 fun CPN'pick [CPN'i1] = (CPN'nth(!CPN'bh1,CPN'i1))
 fun CPN'bindings_as_strings() = [(["londerdelen","ond"],CPN'map (fn {londerdelen,ond} => [LOnderdelen.mkstr londerdelen,Onderdelen.mkstr ond]) (!CPN'bh1))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_place(1 <= CPN'placeID1049486734.init(CPN'inst),
 (true,CPN'Sim.is_disabled)))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0) = (CPN'e0)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(!CPN'bh1))
 end

 end (* end CPN'transitionID1049489165 *) 
 val _ = CPN'Sim.add_be("ID1049489165",CPN'transitionID1049489165.CPN'bind_exe,CPN'transitionID1049489165.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1049496075 = {i:INT}*{londerdelen:LOnderdelen}*{ond:Onderdelen}*{wn:WelNiet}
 type CPN'BRTID1049496075 = {i:INT,londerdelen:LOnderdelen,ond:Onderdelen,wn:WelNiet}
 val CPN'BRID1049496075 = ref ({i=INT.base,londerdelen=LOnderdelen.base,ond=Onderdelen.base,wn=WelNiet.base}: CPN'BRTID1049496075)
 structure CPN'transitionID1049496075 = (* InformatieVragen'Onderdelen_in_lijst_zetten *)
 struct 
 val CPN'id = "ID1049496075"
 val CPN'name = "InformatieVragen'Onderdelen_in_lijst_zetten"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["i","londerdelen","ond","wn"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {i: INT} list)
 val CPN'bh2 = ref(nil: {londerdelen: LOnderdelen} list)
 val CPN'bh3 = ref(nil: {ond: Onderdelen} list)
 val CPN'bh4 = ref(nil: {wn: WelNiet} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf5() = (CPN'whole_binding := true)
 fun CPN'bf4() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'WelNiet_pims1.init_res(CPN'placeID1049491983.mark CPN'inst)
 fun CPN'bf() = 
let
 val wn = case CPN'mode of 
CPN'Sim.all_enabled => CPN'WelNiet_pims1.random_res BindFailureGenAll (CPN'placeID1049491983.mark CPN'inst)
 | _ => CPN'WelNiet_pims1.random_res BindFatalFailure (CPN'placeID1049491983.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'WelNiet_pims1.res_col(CPN'placeID1049491983.mark CPN'inst,wn)
 | _ => ()
 in
 (if (WelNiet.legal wn) then
 (CPN'bh4::= {wn=wn};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf5())
 handle BindFailure => CPN'bf()
 else CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 fun CPN'bf3() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'Onderdelen_pims1.init_res(CPN'placeID1049491231.mark CPN'inst)
 fun CPN'bf() = 
let
 val ond = case CPN'mode of 
CPN'Sim.all_enabled => CPN'Onderdelen_pims1.random_res BindFailureGenAll (CPN'placeID1049491231.mark CPN'inst)
 | _ => CPN'Onderdelen_pims1.random_res BindFatalFailure (CPN'placeID1049491231.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'Onderdelen_pims1.res_col(CPN'placeID1049491231.mark CPN'inst,ond)
 | _ => ()
 in
 (
 (CPN'bh3::= {ond=ond};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf4())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 fun CPN'bf2() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'LOnderdelen_pims1.init_res(CPN'placeID1049498870.mark CPN'inst)
 fun CPN'bf() = 
let
 val londerdelen = case CPN'mode of 
CPN'Sim.all_enabled => CPN'LOnderdelen_pims1.random_res BindFailureGenAll (CPN'placeID1049498870.mark CPN'inst)
 | _ => CPN'LOnderdelen_pims1.random_res BindFatalFailure (CPN'placeID1049498870.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'LOnderdelen_pims1.res_col(CPN'placeID1049498870.mark CPN'inst,londerdelen)
 | _ => ()
 in
 (
 (CPN'bh2::= {londerdelen=londerdelen};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf3())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 fun CPN'bf1() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'INT_pims1.init_res(CPN'placeID1049486749.mark CPN'inst)
 fun CPN'bf() = 
let
 val i = case CPN'mode of 
CPN'Sim.all_enabled => CPN'INT_pims1.random_res BindFailureGenAll (CPN'placeID1049486749.mark CPN'inst)
 | _ => CPN'INT_pims1.random_res BindFatalFailure (CPN'placeID1049486749.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'INT_pims1.res_col(CPN'placeID1049486749.mark CPN'inst,i)
 | _ => ()
 in
 (
 (CPN'bh1::= {i=i};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
CPN'bh2:=nil;
CPN'bh3:=nil;
CPN'bh4:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ();
CPN'bf2() handle BoundGroup => ();
CPN'bf3() handle BoundGroup => ();
CPN'bf4() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ();
CPN'bf3() handle BindFailureGenAll => ();
CPN'bf4() handle BindFailureGenAll => ();
CPN'bf5() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({i},{londerdelen},{ond},{wn}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'INT_pims1.delete(CPN'placeID1049486749.mark CPN'inst,i);
 CPN'Onderdelen_pims1.delete(CPN'placeID1049491231.mark CPN'inst,ond);
 CPN'LOnderdelen_pims1.delete(CPN'placeID1049498870.mark CPN'inst,londerdelen);
 CPN'WelNiet_pims1.delete(CPN'placeID1049491983.mark CPN'inst,wn));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val _ = (CPN'LOnderdelen_pims1.insert(CPN'placeID1049498870.mark CPN'inst,Onderdeel2
(ond,londerdelen,wn));
 CPN'INT_pims1.insert(CPN'placeID1049486749.mark CPN'inst,i-1));
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {i=i,londerdelen=londerdelen,ond=ond,wn=wn}
 val _ = (CPN'BRID1049496075:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - i = ",INT.mkstr i,"\n - londerdelen = ",LOnderdelen.mkstr londerdelen,"\n - ond = ",Onderdelen.mkstr ond,"\n - wn = ",WelNiet.mkstr wn]
else nil)
 end
 fun CPN'pick [CPN'i1,CPN'i2,CPN'i3,CPN'i4] = (CPN'nth(!CPN'bh1,CPN'i1),CPN'nth(!CPN'bh2,CPN'i2),CPN'nth(!CPN'bh3,CPN'i3),CPN'nth(!CPN'bh4,CPN'i4))
 fun CPN'bindings_as_strings() = [(["i"],CPN'map (fn {i} => [INT.mkstr i]) (!CPN'bh1)),(["londerdelen"],CPN'map (fn {londerdelen} => [LOnderdelen.mkstr londerdelen]) (!CPN'bh2)),(["ond"],CPN'map (fn {ond} => [Onderdelen.mkstr ond]) (!CPN'bh3)),(["wn"],CPN'map (fn {wn} => [WelNiet.mkstr wn]) (!CPN'bh4))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_place(1 <= CPN'placeID1049486749.init(CPN'inst),
 CPN'Sim.each_place(1 <= CPN'placeID1049491231.init(CPN'inst),
 CPN'Sim.each_place(1 <= CPN'placeID1049498870.init(CPN'inst),
 CPN'Sim.each_place(1 <= CPN'placeID1049491983.init(CPN'inst),
 (true,CPN'Sim.is_disabled))))))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1),CPN'hd(!CPN'bh2),CPN'hd(!CPN'bh3),CPN'hd(!CPN'bh4)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0,(CPN'e1,(CPN'e2,CPN'e3))) = (CPN'e0,CPN'e1,CPN'e2,CPN'e3)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(expand1(!CPN'bh1,expand1(!CPN'bh2,expand1(!CPN'bh3,!CPN'bh4)))))
 end

 end (* end CPN'transitionID1049496075 *) 
 val _ = CPN'Sim.add_be("ID1049496075",CPN'transitionID1049496075.CPN'bind_exe,CPN'transitionID1049496075.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1049503673 = {i:INT}*{h:INT}*{londerdelen:LOnderdelen}
 type CPN'BRTID1049503673 = {i:INT,h:INT,londerdelen:LOnderdelen}
 val CPN'BRID1049503673 = ref ({i=INT.base,h=INT.base,londerdelen=LOnderdelen.base}: CPN'BRTID1049503673)
 structure CPN'transitionID1049503673 = (* InformatieVragen'Juiste_lijst_bij_juist_vliegtuig *)
 struct 
 val CPN'id = "ID1049503673"
 val CPN'name = "InformatieVragen'Juiste_lijst_bij_juist_vliegtuig"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["i","h","londerdelen"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref nil
 val CPN'bh2 = ref(nil: {i: INT} list)
 val CPN'bh3 = ref(nil: {h: INT} list)
 val CPN'bh4 = ref(nil: {londerdelen: LOnderdelen} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf5() = (CPN'whole_binding := true)
 fun CPN'bf4() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'LOnderdelen_pims1.init_res(CPN'placeID1049498870.mark CPN'inst)
 fun CPN'bf() = 
let
 val londerdelen = case CPN'mode of 
CPN'Sim.all_enabled => CPN'LOnderdelen_pims1.random_res BindFailureGenAll (CPN'placeID1049498870.mark CPN'inst)
 | _ => CPN'LOnderdelen_pims1.random_res BindFatalFailure (CPN'placeID1049498870.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'LOnderdelen_pims1.res_col(CPN'placeID1049498870.mark CPN'inst,londerdelen)
 | _ => ()
 in
 (
 (CPN'bh4::= {londerdelen=londerdelen};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf5())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 fun CPN'bf3() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'INT_pims1.init_res(CPN'placeID1049486759.mark CPN'inst)
 fun CPN'bf() = 
let
 val h = case CPN'mode of 
CPN'Sim.all_enabled => CPN'INT_pims1.random_res BindFailureGenAll (CPN'placeID1049486759.mark CPN'inst)
 | _ => CPN'INT_pims1.random_res BindFatalFailure (CPN'placeID1049486759.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'INT_pims1.res_col(CPN'placeID1049486759.mark CPN'inst,h)
 | _ => ()
 in
 (
 (CPN'bh3::= {h=h};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf4())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 fun CPN'bf2() = 
 (* B_g *)
 (let 
val i = 0
 in 
 (* untimed T_a token exp coef 1 *)
 (if CPN'INT_pims1.member (!(CPN'placeID1049486749.mark CPN'inst),i) then
 (CPN'bh2::= {i=i};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailureGenAll
 | _ => CPN'bf3())
 else raise BindFatalFailure)
 end handle Bind => raise BindFatalFailure)
 fun CPN'bf1() = 
 (* untimed T_a token exp coef 1 *)
 (if CPN'LOnderdelen_pims1.member (!(CPN'placeID1049486734.mark CPN'inst),[]) then
 (case CPN'mode of 
CPN'Sim.bind _=> raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailureGenAll
 | _ => CPN'bf2())
 else raise BindFatalFailure)
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
CPN'bh2:=nil;
CPN'bh3:=nil;
CPN'bh4:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ();
CPN'bf2() handle BoundGroup => ();
CPN'bf3() handle BoundGroup => ();
CPN'bf4() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ();
CPN'bf3() handle BindFailureGenAll => ();
CPN'bf4() handle BindFailureGenAll => ();
CPN'bf5() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({i},{h},{londerdelen}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'LOnderdelen_pims1.delete(CPN'placeID1049486734.mark CPN'inst,[]);
 CPN'INT_pims1.delete(CPN'placeID1049486749.mark CPN'inst,i);
 CPN'INT_pims1.delete(CPN'placeID1049486759.mark CPN'inst,h);
 CPN'LOnderdelen_pims1.delete(CPN'placeID1049498870.mark CPN'inst,londerdelen));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val _ = (CPN'LOnderdelen_pims1.insert(CPN'placeID1049498870.mark CPN'inst,[]);
 CPN'INT_pims1.insert(CPN'placeID1049486754.mark CPN'inst,1);
 CPN'Controle_pims12.insert(CPN'placeID1049507093.mark CPN'inst,{id=h,
l=londerdelen}));
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {i=i,h=h,londerdelen=londerdelen}
 val _ = (CPN'BRID1049503673:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - i = ",INT.mkstr i,"\n - h = ",INT.mkstr h,"\n - londerdelen = ",LOnderdelen.mkstr londerdelen]
else nil)
 end
 fun CPN'pick [CPN'i2,CPN'i3,CPN'i4] = (CPN'nth(!CPN'bh2,CPN'i2),CPN'nth(!CPN'bh3,CPN'i3),CPN'nth(!CPN'bh4,CPN'i4))
 fun CPN'bindings_as_strings() = [(["i"],CPN'map (fn {i} => [INT.mkstr i]) (!CPN'bh2)),(["h"],CPN'map (fn {h} => [INT.mkstr h]) (!CPN'bh3)),(["londerdelen"],CPN'map (fn {londerdelen} => [LOnderdelen.mkstr londerdelen]) (!CPN'bh4))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_place(1 <= CPN'placeID1049486734.init(CPN'inst),
 CPN'Sim.each_place(1 <= CPN'placeID1049486749.init(CPN'inst),
 CPN'Sim.each_place(1 <= CPN'placeID1049486759.init(CPN'inst),
 CPN'Sim.each_place(1 <= CPN'placeID1049498870.init(CPN'inst),
 (true,CPN'Sim.is_disabled))))))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh2),CPN'hd(!CPN'bh3),CPN'hd(!CPN'bh4)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0,(CPN'e1,CPN'e2)) = (CPN'e0,CPN'e1,CPN'e2)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(expand1(!CPN'bh2,expand1(!CPN'bh3,!CPN'bh4))))
 end

 end (* end CPN'transitionID1049503673 *) 
 val _ = CPN'Sim.add_be("ID1049503673",CPN'transitionID1049503673.CPN'bind_exe,CPN'transitionID1049503673.CPN'priority);structure CPN'Totale_doorlooptijd = struct
datatype BindElem = 
Hoofdproces'Startklaar of int * {v: Vliegtuig}  
type markings = unit

type subnet = BindElem
fun get_subnet (CPN'be:BindElem) = CPN'be
fun get_markings() = ()
fun init () = 
  NONE
fun pred (bindelem) = 
let
  fun predBindElem (Hoofdproces'Startklaar (1, {v})) = true
      | predBindElem _ = false
in
  predBindElem bindelem  
end
fun obs (bindelem) = 
let
  fun obsBindElem (Hoofdproces'Startklaar (1, {v})) =
       CurrentTime()- #arrtime(v)
      | obsBindElem _ = ~1
in
  obsBindElem bindelem  
end
fun stop () = 
  NONE
end (* CPN'Totale_doorlooptijd*)

structure Totale_doorlooptijd = CPN'Monitors.CreateUntimedDC (type bindelem = CPN'Totale_doorlooptijd.BindElem and subnet = CPN'Totale_doorlooptijd.subnet and markings = CPN'Totale_doorlooptijd.markings structure SV = CPN'IUSV val pred = CPN'Totale_doorlooptijd.pred and obs =( IntInf.fromInt o CPN'Totale_doorlooptijd.obs):subnet->SV.data and init =( (fn CPN'iopt => case CPN'iopt of NONE => NONE | SOME CPN'i => SOME (IntInf.fromInt CPN'i)) o CPN'Totale_doorlooptijd.init):markings->SV.data option and stop =( (fn CPN'iopt => case CPN'iopt of NONE => NONE | SOME CPN'i => SOME (IntInf.fromInt CPN'i)) o CPN'Totale_doorlooptijd.stop):markings->SV.data option and get_subnet = CPN'Totale_doorlooptijd.get_subnet and get_markings = CPN'Totale_doorlooptijd.get_markings and name = "Totale_doorlooptijd"and montype = CPN'MonitorTable.step_monitor  and updatelogfile = true)
val _ = CPN'Monitors.insert_fun("ID1049335371","Totale_doorlooptijd.init_monitor",Totale_doorlooptijd.init_monitor, CPN'Monitors.sim_init_fun_list)
val _ = if step()=IntInf.fromInt 0 then Totale_doorlooptijd.init_monitor() else ()
val _ = CPN'Monitors.insert_fun("ID1049335371","Totale_doorlooptijd.stop_monitor",Totale_doorlooptijd.stop_monitor, CPN'Monitors.sim_stop_fun_list)
val _=CPN'PerfReport.insert_in_iid_list("ID1049335371","Totale_doorlooptijd","count_iid",Totale_doorlooptijd.count_iid) CPN'PerfReport.intuntimed_statvars_iidobs
val _=CPN'PerfReport.insert_in_iid_list("ID1049335371","Totale_doorlooptijd","avrg_iid",Totale_doorlooptijd.avrg_iid) CPN'PerfReport.realuntimed_statvars_iidobs
val _=CPN'PerfReport.insert_in_iid_list("ID1049335371","Totale_doorlooptijd","min_iid",Totale_doorlooptijd.min_iid) CPN'PerfReport.intuntimed_statvars_iidobs
val _=CPN'PerfReport.insert_in_iid_list("ID1049335371","Totale_doorlooptijd","max_iid",Totale_doorlooptijd.max_iid) CPN'PerfReport.intuntimed_statvars_iidobs
val _=CPN'PerfReport.insert_in_iid_list("ID1049335371","Totale_doorlooptijd","sum_iid",Totale_doorlooptijd.sum_iid) CPN'PerfReport.intuntimed_statvars_iidobs
val _ = CPN'PerfReport.insert_in_list ("ID1049335371","Totale_doorlooptijd",Totale_doorlooptijd.get_stat_strings) CPN'PerfReport.untimed_sim_get_stats_funs

val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1049329450 = {v:Vliegtuig}
 type CPN'BRTID1049329450 = {v:Vliegtuig}
 val CPN'BRID1049329450 = ref ({v=Vliegtuig.base}: CPN'BRTID1049329450)
 val CPN'monitorID1049329450 = 
CPN'Sim.monitor (fn CPN'inst => 
fn {v:Vliegtuig}  => (Totale_doorlooptijd.monitor((CPN'Totale_doorlooptijd.Hoofdproces'Startklaar(CPN'inst,{v=v})))))
 structure CPN'transitionID1049329450 = (* Hoofdproces'Startklaar *)
 struct 
 val CPN'id = "ID1049329450"
 val CPN'name = "Hoofdproces'Startklaar"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["v"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {v: Vliegtuig} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf2() = (CPN'whole_binding := true)
 fun CPN'bf1() = 
 let (* timed B_p with coef 1 *)
 val _ = CPN'Vliegtuig'timed_pims123456.init_res(CPN'placeID1049328673.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@(v,CPN'time106) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'Vliegtuig'timed_pims123456.random_res BindFailureGenAll (CPN'placeID1049328673.mark CPN'inst)
 | _ => CPN'Vliegtuig'timed_pims123456.random_res BindFatalFailure (CPN'placeID1049328673.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'Vliegtuig'timed_pims123456.res_col(CPN'placeID1049328673.mark CPN'inst, CPN'Time.@(v,CPN'time106))
 | _ => ()
 in (
 (* T_g *)
 (if (CurrentTime()>= 
#deptime v) then
 (CPN'bh1::= {v=v};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 else raise BindFailure)
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({v}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'Vliegtuig'timed_pims123456.delete(CPN'placeID1049328673.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1049329450 CPN'placeID1049328673") (CPN'placeID1049328673.mark CPN'inst,CPN'Vliegtuig'timed_pims123456.collect,CPN'Vliegtuig'timed_pims123456.cmp,v, time())));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val CPN'trans_delay = CPN'Time.fromInt(0)
 val _ = (CPN'Vliegtuig'timed_pims123456_sims.addto (CPN'placeID1038043812.wait CPN'inst,CPN'TMS.@++([(v)],CPN'trans_delay)));
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {v=v}
 val _ = (CPN'BRID1049329450:=CPN'bindrec)
val _ = CPN'monitorID1049329450 CPN'inst CPN'bindrec 
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - v = ",Vliegtuig.mkstr v]
else nil)
 end
 fun CPN'pick [CPN'i1] = (CPN'nth(!CPN'bh1,CPN'i1))
 fun CPN'bindings_as_strings() = [(["v"],CPN'map (fn {v} => [Vliegtuig.mkstr v]) (!CPN'bh1))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_timed_place(1 <= CPN'placeID1049328673.init(CPN'inst),CPN'placeID1049328673.next_time (CPN'inst),
 (true,CPN'Sim.is_disabled)))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0) = (CPN'e0)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(!CPN'bh1))
 end

 end (* end CPN'transitionID1049329450 *) 
 val _ = CPN'Sim.add_be("ID1049329450",CPN'transitionID1049329450.CPN'bind_exe,CPN'transitionID1049329450.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1414910579 = {ctlr:Controleur,operator:Operator,v:Vliegtuig}
 type CPN'BRTID1414910579 = {ctlr:Controleur,operator:Operator,v:Vliegtuig}
 val CPN'BRID1414910579 = ref ({ctlr=Controleur.base,operator=Operator.base,v=Vliegtuig.base}: CPN'BRTID1414910579)
 structure CPN'transitionID1414910579 = (* Controle_de' *)
 struct 
 val CPN'id = "ID1414910579"
 val CPN'name = "Controle_de'"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["ctlr","operator","v"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {ctlr: Controleur,operator: Operator,v: Vliegtuig} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf2() = (CPN'whole_binding := true)
 fun CPN'bf1() = 
 let (* timed B_p with coef 1 *)
 val _ = CPN'VliegtuigOperatorControleur'timed_pims123.init_res(CPN'placeID1037590918.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@({ vliegtuig = v, operator = operator, controleur = ctlr },CPN'time108) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'VliegtuigOperatorControleur'timed_pims123.random_res BindFailureGenAll (CPN'placeID1037590918.mark CPN'inst)
 | _ => CPN'VliegtuigOperatorControleur'timed_pims123.random_res BindFatalFailure (CPN'placeID1037590918.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'VliegtuigOperatorControleur'timed_pims123.res_col(CPN'placeID1037590918.mark CPN'inst, CPN'Time.@({ vliegtuig = v, operator = operator, controleur = ctlr },CPN'time108))
 | _ => ()
 in (
 (CPN'bh1::= {ctlr=ctlr,operator=operator,v=v};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({ctlr,operator,v}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'VliegtuigOperatorControleur'timed_pims123.delete(CPN'placeID1037590918.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1414910579 CPN'placeID1037590918") (CPN'placeID1037590918.mark CPN'inst,CPN'VliegtuigOperatorControleur'timed_pims123.collect,CPN'VliegtuigOperatorControleur'timed_pims123.cmp,{ vliegtuig = v, operator = operator, controleur = ctlr }, time())));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val _ = ();
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {ctlr=ctlr,operator=operator,v=v}
 val _ = (CPN'BRID1414910579:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - ctlr = ",Controleur.mkstr ctlr,"\n - operator = ",Operator.mkstr operator,"\n - v = ",Vliegtuig.mkstr v]
else nil)
 end
 fun CPN'pick [CPN'i1] = (CPN'nth(!CPN'bh1,CPN'i1))
 fun CPN'bindings_as_strings() = [(["ctlr","operator","v"],CPN'map (fn {ctlr,operator,v} => [Controleur.mkstr ctlr,Operator.mkstr operator,Vliegtuig.mkstr v]) (!CPN'bh1))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_timed_place(1 <= CPN'placeID1037590918.init(CPN'inst),CPN'placeID1037590918.next_time (CPN'inst),
 (true,CPN'Sim.is_disabled)))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0) = (CPN'e0)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(!CPN'bh1))
 end

 end (* end CPN'transitionID1414910579 *) 
 val _ = CPN'Sim.add_be("ID1414910579",CPN'transitionID1414910579.CPN'bind_exe,CPN'transitionID1414910579.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1414910579 = {londerdelen:LOnderdelen,i:INT}*{ctlr:Controleur,operator:Operator,v:Vliegtuig}
 type CPN'BRTID1414910579 = {londerdelen:LOnderdelen,i:INT,ctlr:Controleur,operator:Operator,v:Vliegtuig}
 val CPN'BRID1414910579 = ref ({londerdelen=LOnderdelen.base,i=INT.base,ctlr=Controleur.base,operator=Operator.base,v=Vliegtuig.base}: CPN'BRTID1414910579)
 structure CPN'transitionID1414910579 = (* Controle_de' *)
 struct 
 val CPN'id = "ID1414910579"
 val CPN'name = "Controle_de'"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["londerdelen","i","ctlr","operator","v"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {londerdelen: LOnderdelen,i: INT} list)
 val CPN'bh2 = ref(nil: {ctlr: Controleur,operator: Operator,v: Vliegtuig} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf3() = (CPN'whole_binding := true)
 fun CPN'bf2() = 
 let (* timed B_p with coef 1 *)
 val _ = CPN'VliegtuigOperatorControleur'timed_pims123.init_res(CPN'placeID1037590918.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@({ vliegtuig = v, operator = operator, controleur = ctlr },CPN'time111) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'VliegtuigOperatorControleur'timed_pims123.random_res BindFailureGenAll (CPN'placeID1037590918.mark CPN'inst)
 | _ => CPN'VliegtuigOperatorControleur'timed_pims123.random_res BindFatalFailure (CPN'placeID1037590918.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'VliegtuigOperatorControleur'timed_pims123.res_col(CPN'placeID1037590918.mark CPN'inst, CPN'Time.@({ vliegtuig = v, operator = operator, controleur = ctlr },CPN'time111))
 | _ => ()
 in (
 (CPN'bh2::= {ctlr=ctlr,operator=operator,v=v};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf3())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 fun CPN'bf1() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'Controle_pims12.init_res(CPN'placeID1044884445.mark CPN'inst)
 fun CPN'bf() = 
let
 val { id = i, l = londerdelen } = case CPN'mode of 
CPN'Sim.all_enabled => CPN'Controle_pims12.random_res BindFailureGenAll (CPN'placeID1044884445.mark CPN'inst)
 | _ => CPN'Controle_pims12.random_res BindFatalFailure (CPN'placeID1044884445.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'Controle_pims12.res_col(CPN'placeID1044884445.mark CPN'inst,{ id = i, l = londerdelen })
 | _ => ()
 in
 (
 (CPN'bh1::= {londerdelen=londerdelen,i=i};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
CPN'bh2:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ();
CPN'bf2() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ();
CPN'bf3() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({londerdelen,i},{ctlr,operator,v}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'Controle_pims12.delete(CPN'placeID1044884445.mark CPN'inst,{ id = i, l = londerdelen });
 CPN'VliegtuigOperatorControleur'timed_pims123.delete(CPN'placeID1037590918.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1414910579 CPN'placeID1037590918") (CPN'placeID1037590918.mark CPN'inst,CPN'VliegtuigOperatorControleur'timed_pims123.collect,CPN'VliegtuigOperatorControleur'timed_pims123.cmp,{ vliegtuig = v, operator = operator, controleur = ctlr }, time())));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val _ = ();
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {londerdelen=londerdelen,i=i,ctlr=ctlr,operator=operator,v=v}
 val _ = (CPN'BRID1414910579:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - londerdelen = ",LOnderdelen.mkstr londerdelen,"\n - i = ",INT.mkstr i,"\n - ctlr = ",Controleur.mkstr ctlr,"\n - operator = ",Operator.mkstr operator,"\n - v = ",Vliegtuig.mkstr v]
else nil)
 end
 fun CPN'pick [CPN'i1,CPN'i2] = (CPN'nth(!CPN'bh1,CPN'i1),CPN'nth(!CPN'bh2,CPN'i2))
 fun CPN'bindings_as_strings() = [(["londerdelen","i"],CPN'map (fn {londerdelen,i} => [LOnderdelen.mkstr londerdelen,INT.mkstr i]) (!CPN'bh1)),(["ctlr","operator","v"],CPN'map (fn {ctlr,operator,v} => [Controleur.mkstr ctlr,Operator.mkstr operator,Vliegtuig.mkstr v]) (!CPN'bh2))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_place(1 <= CPN'placeID1044884445.init(CPN'inst),
 CPN'Sim.each_timed_place(1 <= CPN'placeID1037590918.init(CPN'inst),CPN'placeID1037590918.next_time (CPN'inst),
 (true,CPN'Sim.is_disabled))))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1),CPN'hd(!CPN'bh2)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0,CPN'e1) = (CPN'e0,CPN'e1)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(expand1(!CPN'bh1,!CPN'bh2)))
 end

 end (* end CPN'transitionID1414910579 *) 
 val _ = CPN'Sim.add_be("ID1414910579",CPN'transitionID1414910579.CPN'bind_exe,CPN'transitionID1414910579.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1414910579 = {londerdelen:LOnderdelen,i:INT}*{ctlr:Controleur,operator:Operator,v:Vliegtuig}
 type CPN'BRTID1414910579 = {londerdelen:LOnderdelen,i:INT,ctlr:Controleur,operator:Operator,v:Vliegtuig}
 val CPN'BRID1414910579 = ref ({londerdelen=LOnderdelen.base,i=INT.base,ctlr=Controleur.base,operator=Operator.base,v=Vliegtuig.base}: CPN'BRTID1414910579)
 structure CPN'transitionID1414910579 = (* Controle_de' *)
 struct 
 val CPN'id = "ID1414910579"
 val CPN'name = "Controle_de'"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["londerdelen","i","ctlr","operator","v"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {londerdelen: LOnderdelen,i: INT} list)
 val CPN'bh2 = ref(nil: {ctlr: Controleur,operator: Operator,v: Vliegtuig} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf3() = (CPN'whole_binding := true)
 fun CPN'bf2() = 
 let (* timed B_p with coef 1 *)
 val _ = CPN'VliegtuigOperatorControleur'timed_pims123.init_res(CPN'placeID1037590918.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@({ vliegtuig = v, operator = operator, controleur = ctlr },CPN'time114) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'VliegtuigOperatorControleur'timed_pims123.random_res BindFailureGenAll (CPN'placeID1037590918.mark CPN'inst)
 | _ => CPN'VliegtuigOperatorControleur'timed_pims123.random_res BindFatalFailure (CPN'placeID1037590918.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'VliegtuigOperatorControleur'timed_pims123.res_col(CPN'placeID1037590918.mark CPN'inst, CPN'Time.@({ vliegtuig = v, operator = operator, controleur = ctlr },CPN'time114))
 | _ => ()
 in (
 (CPN'bh2::= {ctlr=ctlr,operator=operator,v=v};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf3())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
 fun CPN'bf1() = 
 let (* Untimed B_p coef>=1 *)

 val _ = CPN'Controle_pims12.init_res(CPN'placeID1044884445.mark CPN'inst)
 fun CPN'bf() = 
let
 val { id = i, l = londerdelen } = case CPN'mode of 
CPN'Sim.all_enabled => CPN'Controle_pims12.random_res BindFailureGenAll (CPN'placeID1044884445.mark CPN'inst)
 | _ => CPN'Controle_pims12.random_res BindFatalFailure (CPN'placeID1044884445.mark CPN'inst)
 val _ = case CPN'mode of
 CPN'Sim.all_enabled => CPN'Controle_pims12.res_col(CPN'placeID1044884445.mark CPN'inst,{ id = i, l = londerdelen })
 | _ => ()
 in
 (
 (CPN'bh1::= {londerdelen=londerdelen,i=i};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
CPN'bh2:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ();
CPN'bf2() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ();
CPN'bf3() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({londerdelen,i},{ctlr,operator,v}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'Controle_pims12.delete(CPN'placeID1044884445.mark CPN'inst,{ id = i, l = londerdelen });
 CPN'VliegtuigOperatorControleur'timed_pims123.delete(CPN'placeID1037590918.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1414910579 CPN'placeID1037590918") (CPN'placeID1037590918.mark CPN'inst,CPN'VliegtuigOperatorControleur'timed_pims123.collect,CPN'VliegtuigOperatorControleur'timed_pims123.cmp,{ vliegtuig = v, operator = operator, controleur = ctlr }, time())));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val _ = ();
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {londerdelen=londerdelen,i=i,ctlr=ctlr,operator=operator,v=v}
 val _ = (CPN'BRID1414910579:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - londerdelen = ",LOnderdelen.mkstr londerdelen,"\n - i = ",INT.mkstr i,"\n - ctlr = ",Controleur.mkstr ctlr,"\n - operator = ",Operator.mkstr operator,"\n - v = ",Vliegtuig.mkstr v]
else nil)
 end
 fun CPN'pick [CPN'i1,CPN'i2] = (CPN'nth(!CPN'bh1,CPN'i1),CPN'nth(!CPN'bh2,CPN'i2))
 fun CPN'bindings_as_strings() = [(["londerdelen","i"],CPN'map (fn {londerdelen,i} => [LOnderdelen.mkstr londerdelen,INT.mkstr i]) (!CPN'bh1)),(["ctlr","operator","v"],CPN'map (fn {ctlr,operator,v} => [Controleur.mkstr ctlr,Operator.mkstr operator,Vliegtuig.mkstr v]) (!CPN'bh2))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_place(1 <= CPN'placeID1044884445.init(CPN'inst),
 CPN'Sim.each_timed_place(1 <= CPN'placeID1037590918.init(CPN'inst),CPN'placeID1037590918.next_time (CPN'inst),
 (true,CPN'Sim.is_disabled))))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1),CPN'hd(!CPN'bh2)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0,CPN'e1) = (CPN'e0,CPN'e1)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(expand1(!CPN'bh1,!CPN'bh2)))
 end

 end (* end CPN'transitionID1414910579 *) 
 val _ = CPN'Sim.add_be("ID1414910579",CPN'transitionID1414910579.CPN'bind_exe,CPN'transitionID1414910579.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1414910579 = {londerdelen:LOnderdelen,i:INT,ctlr:Controleur,operator:Operator,v:Vliegtuig}
 type CPN'BRTID1414910579 = {londerdelen:LOnderdelen,i:INT,ctlr:Controleur,operator:Operator,v:Vliegtuig}
 val CPN'BRID1414910579 = ref ({londerdelen=LOnderdelen.base,i=INT.base,ctlr=Controleur.base,operator=Operator.base,v=Vliegtuig.base}: CPN'BRTID1414910579)
 structure CPN'transitionID1414910579 = (* Controle_de' *)
 struct 
 val CPN'id = "ID1414910579"
 val CPN'name = "Controle_de'"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["londerdelen","i","ctlr","operator","v"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {londerdelen: LOnderdelen,i: INT,ctlr: Controleur,operator: Operator,v: Vliegtuig} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf2() = (CPN'whole_binding := true)
 fun CPN'bf1() = 
 let (* timed B_p with coef 1 *)
 val _ = CPN'VliegtuigOperatorControleur'timed_pims123.init_res(CPN'placeID1037590918.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@({ vliegtuig = v, operator = operator, controleur = ctlr },CPN'time116) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'VliegtuigOperatorControleur'timed_pims123.random_res BindFailureGenAll (CPN'placeID1037590918.mark CPN'inst)
 | _ => CPN'VliegtuigOperatorControleur'timed_pims123.random_res BindFatalFailure (CPN'placeID1037590918.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'VliegtuigOperatorControleur'timed_pims123.res_col(CPN'placeID1037590918.mark CPN'inst, CPN'Time.@({ vliegtuig = v, operator = operator, controleur = ctlr },CPN'time116))
 | _ => ()
 in (
 (* B_g *)
 (let 
val i = #vliegID v
 in 
 let (* untimed B_k with keys in order *)
 fun CPN'bf CPN'ms = case CPN'MS.get_ran (case CPN'mode of 
CPN'Sim.all_enabled => BindFailure
 | _ => BindFailure) CPN'ms of
 ({ id = i, l = londerdelen },CPN'msrest) => (
 (CPN'bh1::= {londerdelen=londerdelen,i=i,ctlr=ctlr,operator=operator,v=v};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 handle BindFailure => CPN'bf (CPN'MS.filter (fn CPN'x => CPN'x <> { id = i, l = londerdelen }) CPN'msrest))
 | (_,CPN'msrest) => (CPN'bf CPN'msrest)
 val CPN'cf  = fn ({id=CPN'id,...}: Controle) => if INT.lt(CPN'id,i) then LESS
 else if INT.lt(i,CPN'id) then GREATER
 else EQUAL
 in CPN'bf(CPN'Controle_pims12.collect CPN'cf (!(CPN'placeID1044884445.mark(CPN'inst)))) end
 end handle Bind => raise BindFailure)
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({londerdelen,i,ctlr,operator,v}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'Controle_pims12.delete(CPN'placeID1044884445.mark CPN'inst,{ id = i, l = londerdelen });
 CPN'VliegtuigOperatorControleur'timed_pims123.delete(CPN'placeID1037590918.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1414910579 CPN'placeID1037590918") (CPN'placeID1037590918.mark CPN'inst,CPN'VliegtuigOperatorControleur'timed_pims123.collect,CPN'VliegtuigOperatorControleur'timed_pims123.cmp,{ vliegtuig = v, operator = operator, controleur = ctlr }, time())));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val _ = ();
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {londerdelen=londerdelen,i=i,ctlr=ctlr,operator=operator,v=v}
 val _ = (CPN'BRID1414910579:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - londerdelen = ",LOnderdelen.mkstr londerdelen,"\n - i = ",INT.mkstr i,"\n - ctlr = ",Controleur.mkstr ctlr,"\n - operator = ",Operator.mkstr operator,"\n - v = ",Vliegtuig.mkstr v]
else nil)
 end
 fun CPN'pick [CPN'i1] = (CPN'nth(!CPN'bh1,CPN'i1))
 fun CPN'bindings_as_strings() = [(["londerdelen","i","ctlr","operator","v"],CPN'map (fn {londerdelen,i,ctlr,operator,v} => [LOnderdelen.mkstr londerdelen,INT.mkstr i,Controleur.mkstr ctlr,Operator.mkstr operator,Vliegtuig.mkstr v]) (!CPN'bh1))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_place(1 <= CPN'placeID1044884445.init(CPN'inst),
 CPN'Sim.each_timed_place(1 <= CPN'placeID1037590918.init(CPN'inst),CPN'placeID1037590918.next_time (CPN'inst),
 (true,CPN'Sim.is_disabled))))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0) = (CPN'e0)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(!CPN'bh1))
 end

 end (* end CPN'transitionID1414910579 *) 
 val _ = CPN'Sim.add_be("ID1414910579",CPN'transitionID1414910579.CPN'bind_exe,CPN'transitionID1414910579.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1414910579 = {londerdelen:LOnderdelen,i:INT,ctlr:Controleur,operator:Operator,v:Vliegtuig}
 type CPN'BRTID1414910579 = {londerdelen:LOnderdelen,i:INT,ctlr:Controleur,operator:Operator,v:Vliegtuig}
 val CPN'BRID1414910579 = ref ({londerdelen=LOnderdelen.base,i=INT.base,ctlr=Controleur.base,operator=Operator.base,v=Vliegtuig.base}: CPN'BRTID1414910579)
 structure CPN'transitionID1414910579 = (* Controle_de' *)
 struct 
 val CPN'id = "ID1414910579"
 val CPN'name = "Controle_de'"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["londerdelen","i","ctlr","operator","v"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {londerdelen: LOnderdelen,i: INT,ctlr: Controleur,operator: Operator,v: Vliegtuig} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf2() = (CPN'whole_binding := true)
 fun CPN'bf1() = 
 let (* timed B_p with coef 1 *)
 val _ = CPN'VliegtuigOperatorControleur'timed_pims123.init_res(CPN'placeID1037590918.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@({ vliegtuig = v, operator = operator, controleur = ctlr },CPN'time120) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'VliegtuigOperatorControleur'timed_pims123.random_res BindFailureGenAll (CPN'placeID1037590918.mark CPN'inst)
 | _ => CPN'VliegtuigOperatorControleur'timed_pims123.random_res BindFatalFailure (CPN'placeID1037590918.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'VliegtuigOperatorControleur'timed_pims123.res_col(CPN'placeID1037590918.mark CPN'inst, CPN'Time.@({ vliegtuig = v, operator = operator, controleur = ctlr },CPN'time120))
 | _ => ()
 in (
 (* B_g *)
 (let 
val i = #vliegID v
 in 
 let (* untimed B_k with keys in order *)
 fun CPN'bf CPN'ms = case CPN'MS.get_ran (case CPN'mode of 
CPN'Sim.all_enabled => BindFailure
 | _ => BindFailure) CPN'ms of
 ({ id = i, l = londerdelen },CPN'msrest) => (
 (CPN'bh1::= {londerdelen=londerdelen,i=i,ctlr=ctlr,operator=operator,v=v};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 handle BindFailure => CPN'bf (CPN'MS.filter (fn CPN'x => CPN'x <> { id = i, l = londerdelen }) CPN'msrest))
 | (_,CPN'msrest) => (CPN'bf CPN'msrest)
 val CPN'cf  = fn ({id=CPN'id,...}: Controle) => if INT.lt(CPN'id,i) then LESS
 else if INT.lt(i,CPN'id) then GREATER
 else EQUAL
 in CPN'bf(CPN'Controle_pims12.collect CPN'cf (!(CPN'placeID1044884445.mark(CPN'inst)))) end
 end handle Bind => raise BindFailure)
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({londerdelen,i,ctlr,operator,v}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'Controle_pims12.delete(CPN'placeID1044884445.mark CPN'inst,{ id = i, l = londerdelen });
 CPN'VliegtuigOperatorControleur'timed_pims123.delete(CPN'placeID1037590918.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1414910579 CPN'placeID1037590918") (CPN'placeID1037590918.mark CPN'inst,CPN'VliegtuigOperatorControleur'timed_pims123.collect,CPN'VliegtuigOperatorControleur'timed_pims123.cmp,{ vliegtuig = v, operator = operator, controleur = ctlr }, time())));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val CPN'trans_delay = CPN'Time.fromInt(0)
 val _ = (CPN'VliegtuigOperatorControleur'timed_pims123_sims.addto (CPN'placeID1037590913.wait CPN'inst,CPN'TMS.@++([({vliegtuig= v, 
operator= operator,
controleur= if (#listonderdelen v = londerdelen)
then 1 else 0})],CPN'trans_delay)));
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {londerdelen=londerdelen,i=i,ctlr=ctlr,operator=operator,v=v}
 val _ = (CPN'BRID1414910579:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - londerdelen = ",LOnderdelen.mkstr londerdelen,"\n - i = ",INT.mkstr i,"\n - ctlr = ",Controleur.mkstr ctlr,"\n - operator = ",Operator.mkstr operator,"\n - v = ",Vliegtuig.mkstr v]
else nil)
 end
 fun CPN'pick [CPN'i1] = (CPN'nth(!CPN'bh1,CPN'i1))
 fun CPN'bindings_as_strings() = [(["londerdelen","i","ctlr","operator","v"],CPN'map (fn {londerdelen,i,ctlr,operator,v} => [LOnderdelen.mkstr londerdelen,INT.mkstr i,Controleur.mkstr ctlr,Operator.mkstr operator,Vliegtuig.mkstr v]) (!CPN'bh1))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_place(1 <= CPN'placeID1044884445.init(CPN'inst),
 CPN'Sim.each_timed_place(1 <= CPN'placeID1037590918.init(CPN'inst),CPN'placeID1037590918.next_time (CPN'inst),
 (true,CPN'Sim.is_disabled))))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0) = (CPN'e0)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(!CPN'bh1))
 end

 end (* end CPN'transitionID1414910579 *) 
 val _ = CPN'Sim.add_be("ID1414910579",CPN'transitionID1414910579.CPN'bind_exe,CPN'transitionID1414910579.CPN'priority);
val _ = CPN'Sim.instances_changed:=true;
val _ = CPN'Sim.generate_instances:=true;
type CPN'BTID1414910579 = {londerdelen:LOnderdelen,i:INT,ctlr:Controleur,operator:Operator,v:Vliegtuig}
 type CPN'BRTID1414910579 = {londerdelen:LOnderdelen,i:INT,ctlr:Controleur,operator:Operator,v:Vliegtuig}
 val CPN'BRID1414910579 = ref ({londerdelen=LOnderdelen.base,i=INT.base,ctlr=Controleur.base,operator=Operator.base,v=Vliegtuig.base}: CPN'BRTID1414910579)
 structure CPN'transitionID1414910579 = (* Controle_de'Controle2 *)
 struct 
 val CPN'id = "ID1414910579"
 val CPN'name = "Controle_de'Controle2"
 val CPN'priority = (1000)
 val CPN'controllable = true
 val CPN'pickable_vars = ["londerdelen","i","ctlr","operator","v"]
 val CPN'output_vars = []
 val CPN'whole_binding = ref false
 val CPN'bh1 = ref(nil: {londerdelen: LOnderdelen,i: INT,ctlr: Controleur,operator: Operator,v: Vliegtuig} list)
 fun CPN'bindfun (CPN'mode,CPN'inst) = 
let
 fun CPN'bf2() = (CPN'whole_binding := true)
 fun CPN'bf1() = 
 let (* timed B_p with coef 1 *)
 val _ = CPN'VliegtuigOperatorControleur'timed_pims123.init_res(CPN'placeID1037590918.mark CPN'inst)
 fun CPN'bf() = 
let
 val CPN'Time.@({ vliegtuig = v, operator = operator, controleur = ctlr },CPN'time124) = case CPN'mode of 
CPN'Sim.all_enabled => CPN'VliegtuigOperatorControleur'timed_pims123.random_res BindFailureGenAll (CPN'placeID1037590918.mark CPN'inst)
 | _ => CPN'VliegtuigOperatorControleur'timed_pims123.random_res BindFatalFailure (CPN'placeID1037590918.mark CPN'inst)
 val _ = case CPN'mode of 
 CPN'Sim.all_enabled => CPN'VliegtuigOperatorControleur'timed_pims123.res_col(CPN'placeID1037590918.mark CPN'inst, CPN'Time.@({ vliegtuig = v, operator = operator, controleur = ctlr },CPN'time124))
 | _ => ()
 in (
 (* B_g *)
 (let 
val i = #vliegID v
 in 
 let (* untimed B_k with keys in order *)
 fun CPN'bf CPN'ms = case CPN'MS.get_ran (case CPN'mode of 
CPN'Sim.all_enabled => BindFailure
 | _ => BindFailure) CPN'ms of
 ({ id = i, l = londerdelen },CPN'msrest) => (
 (CPN'bh1::= {londerdelen=londerdelen,i=i,ctlr=ctlr,operator=operator,v=v};
 case CPN'mode of 
CPN'Sim.bind _ => raise BoundGroup
 | CPN'Sim.all_enabled => raise BindFailure
 | _ => CPN'bf2())
 handle BindFailure => CPN'bf (CPN'MS.filter (fn CPN'x => CPN'x <> { id = i, l = londerdelen }) CPN'msrest))
 | (_,CPN'msrest) => (CPN'bf CPN'msrest)
 val CPN'cf  = fn ({id=CPN'id,...}: Controle) => if INT.lt(CPN'id,i) then LESS
 else if INT.lt(i,CPN'id) then GREATER
 else EQUAL
 in CPN'bf(CPN'Controle_pims12.collect CPN'cf (!(CPN'placeID1044884445.mark(CPN'inst)))) end
 end handle Bind => raise BindFailure)
 handle BindFailure => CPN'bf())
 end handle Bind => CPN'bf()
 in CPN'bf() end
fun CPN'bh_reset () = (CPN'whole_binding := false;
CPN'bh1:=nil;
())
 in
(CPN'bh_reset();
 case CPN'mode of
 CPN'Sim.bind CPN'interactive => (CPN'bf1() handle BoundGroup => ())
 | CPN'Sim.all_enabled => ((CPN'bf1() handle BindFailureGenAll => ();
CPN'bf2() handle BindFailureGenAll => ()) handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))
 | _ => CPN'bf1())
 end
 fun CPN'occfun (CPN'inst,({londerdelen,i,ctlr,operator,v}),CPN'inc_step) = 
let
 val _ = if CPN'inc_step then CPN'Sim.inc_step() else () 
 val _ = (CPN'Controle_pims12.delete(CPN'placeID1044884445.mark CPN'inst,{ id = i, l = londerdelen });
 CPN'VliegtuigOperatorControleur'timed_pims123.delete(CPN'placeID1037590918.mark CPN'inst, CPN'Sim.collect_token (InternalError "rm tID1414910579 CPN'placeID1037590918") (CPN'placeID1037590918.mark CPN'inst,CPN'VliegtuigOperatorControleur'timed_pims123.collect,CPN'VliegtuigOperatorControleur'timed_pims123.cmp,{ vliegtuig = v, operator = operator, controleur = ctlr }, time())));
 val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
 val CPN'trans_delay = CPN'Time.fromInt(0)
 val _ = (CPN'VliegtuigOperatorControleur'timed_pims123_sims.addto (CPN'placeID1037590913.wait CPN'inst,CPN'TMS.@++([({vliegtuig= v, 
operator= operator,
controleur= if (#listonderdelen v = londerdelen)
then 1 else 0})],CPN'trans_delay)));
 val _ = CPN'Sim.tst_ill_marks CPN'suberr;
 val CPN'bindrec = {londerdelen=londerdelen,i=i,ctlr=ctlr,operator=operator,v=v}
 val _ = (CPN'BRID1414910579:=CPN'bindrec)
 in
 (CPN'Sim.is_executed,
 if !CPN'Options.report_bindings then
["\n - londerdelen = ",LOnderdelen.mkstr londerdelen,"\n - i = ",INT.mkstr i,"\n - ctlr = ",Controleur.mkstr ctlr,"\n - operator = ",Operator.mkstr operator,"\n - v = ",Vliegtuig.mkstr v]
else nil)
 end
 fun CPN'pick [CPN'i1] = (CPN'nth(!CPN'bh1,CPN'i1))
 fun CPN'bindings_as_strings() = [(["londerdelen","i","ctlr","operator","v"],CPN'map (fn {londerdelen,i,ctlr,operator,v} => [LOnderdelen.mkstr londerdelen,INT.mkstr i,Controleur.mkstr ctlr,Operator.mkstr operator,Vliegtuig.mkstr v]) (!CPN'bh1))]
 fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = 
 CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)
 fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
 | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)=
 let
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
 val _ = case List.find (fn (CPN'var,CPN'val) => 
 ListUtils.mem CPN'output_vars CPN'var) 
 CPN'bindlist of NONE => ()
 | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment: "^CPN'var)
 val CPN'enabledbindings = CPN'bindings_as_strings()
 val CPN'filteredbindings = 
 (List.foldl CPN'Misc.filter_var_binding  CPN'enabledbindings CPN'bindlist) 
 handle BindFatalFailure => []
 val CPN'bindingpositions = 
 if CPN'filteredbindings = [] 
 then [] 
 else CPN'Misc.get_binding_pos(CPN'filteredbindings,CPN'enabledbindings)
 in
 if CPN'filteredbindings = []
 then raise CPN'CancelPickBind
 else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
 end
 fun CPN'bind_exe(CPN'mode,CPN'inst) = 
let

 val (CPN'enough_tokens, CPN'answer) = (CPN'Sim.each_place(1 <= CPN'placeID1044884445.init(CPN'inst),
 CPN'Sim.each_timed_place(1 <= CPN'placeID1037590918.init(CPN'inst),CPN'placeID1037590918.next_time (CPN'inst),
 (true,CPN'Sim.is_disabled))))
(* Force bindfun to calculate all bindings for interactive 
 manual binding and for picking a binding*)
val CPN'bindfunmode = case CPN'mode of
 (CPN'Sim.bind true) => CPN'Sim.all_enabled
 | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled
 | _ => CPN'mode
in
if CPN'enough_tokens then 
 (CPN'bindfun(CPN'bindfunmode,CPN'inst);
 case CPN'mode of 
CPN'Sim.fast => CPN'occfun (CPN'inst,(CPN'hd(!CPN'bh1)),true)
 | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)
 | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)
 | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)
 | _ => (CPN'Sim.is_executed,nil)) handle BindFatalFailure => (CPN'answer,nil)
 else (CPN'answer,nil)
 end 
fun CPN'bindings CPN'inst = 
let
   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;
   fun unwrap' (CPN'e0) = (CPN'e0)
   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) 
     | unwrap (CPN'x::nil) = [unwrap' CPN'x] 
     | unwrap nil = nil
 in
 (case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of 
 (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) 
 | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) 
  | _ => unwrap(!CPN'bh1))
 end

 end (* end CPN'transitionID1414910579 *) 
 val _ = CPN'Sim.add_be("ID1414910579",CPN'transitionID1414910579.CPN'bind_exe,CPN'transitionID1414910579.CPN'priority);