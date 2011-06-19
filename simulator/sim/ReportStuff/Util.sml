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
(* File: Util.sml
 *
 * Reporting facilities
 *)

val rcsid =  "$Header: /users/cpntools/repository/cpn2000/sml/sim/ReportStuff/Util.sml,v 1.1.1.1 2001/10/02 11:35:29 cpn Exp $";

(* structure inluding various useful functions shared all over the program *)

structure UtilStr  =
	struct
	local val PatternList  = [1, 20, 2, 6, 28, 4, 21, 7, 8, 16, 30]	
	in
	   exception ErrorNthElement;
   	   exception ErrorUpdateTag;
   	   exception ErrorReturnPage;

	   fun NthElement (n, nil) = raise ErrorNthElement
	    |  NthElement (n, x::l) = if n = 1 
				      then x
				      else NthElement ((n - 1), l)

	   fun NthPattern (n) = NthElement (n mod length(PatternList), PatternList)
	end

	fun CreateTagRegion(pg, x, y, 0, space, parent, horizontal) = nil
	 |  CreateTagRegion(pg, x, y, row, space, parent, horizontal) =
		let
			val Obj = DSStr_CreateLabel {page = pg,x = x,y= y,w = 10,h = 10, text = ""};
			val _ =DSStr_MakeNodeIntoRgn { obj = Obj, parent = parent};

        		val _ = DSText_SetAttr {obj = Obj, font = 5, size = 12, 
						style = PlainText,
						just = LeftJustification}

		in
			if horizontal 
			then
                		Obj::CreateTagRegion(pg,x + space, y ,row - 1,space, parent, horizontal)
			else
				Obj::CreateTagRegion(pg, x, y + space, row - 1,space, parent, horizontal)
		end

	fun MakeListRegion (nil, parent) = nil
	 |  MakeListRegion ((x,y)::l, parent) = 
		let
        		val _ = DSStr_MakeNodeIntoRgn {obj = x, parent = parent};
         		val _ = DSStr_MakeNodeIntoRgn {obj = y, parent = parent}
         	in
			x::MakeListRegion (l, parent)
		end;

	fun UpdateTag's (nil, _) = ()
	 |  UpdateTag's (id::idlist , (value:string)::valist) =
		let
         		val _ = DSText_Put {obj = id, text = value}
		in
        		UpdateTag's (idlist, valist)
		end
	 | UpdateTag's  (_,_) = raise ErrorUpdateTag;

    	fun UpdateTag'l (nil, _) = ()
	 |  UpdateTag'l ((nid, lid)::idlist , (value:string)::valist) =
		let
         		val _ = DSText_Put {obj = lid, text = value};
			val _ = DSUI_Align {obj = lid, aligntype = ALN_H, ref1 = nid, ref2 = 0 }
		in
			UpdateTag'l (idlist, valist)
		end
	 |  UpdateTag'l (_,_) = raise ErrorUpdateTag;
     
	fun UpdateTag'i (nil,_) =()
	 |  UpdateTag'i (id::idlist ,(value:int)::valist) =
	    let
		val _ = DSText_Put {obj = id, text = Int.toString (value)}
	    in
		UpdateTag'i (idlist, valist)
	    end
	 |  UpdateTag'i  (_,_) = raise ErrorUpdateTag;

	fun CreateList (n, incr, 0) = nil
	 |  CreateList (n:int, incr:int, times) = 
		n::CreateList (n+incr, incr, times - 1);

	fun DeleteIdList nil = ()
	 |  DeleteIdList (x::l) =
		let
         		val _ = DSStr_DeleteObject(x)
		in
			DeleteIdList(l)
		end;

	fun reverse nil = nil
	 |  reverse (x::nil) = x::nil
	 |  reverse (x::l) = (reverse (l)^^[x]);

	fun ceiling x = Real.ceil x

	fun round x = floor (x + 0.5);

	fun rounding x = real (floor x) + 
			 real (round (100.0 * (x - real (floor x)))) / 100.0;

	fun map1 f nil = nil
	 |  map1 f (x::y::l) = (f x)::y::(map1 f l)
	 |  map1 f (x::nil) = (f x)::nil;

   	fun map2 f nil = nil
	 |  map2 f (x::y::l) = x::(f y)::(map2 f l)
	 | map2 f (x::nil) = x::nil;

	fun map1ll g l = map (fn x => map1 g x) l;

	fun map2ll g l = map (fn x => map g x) l;

	fun Snd (x,y) = y;

	fun appendno(1, l1, x::l2) = (x^^l1)::l2
	 |  appendno(n, l1, x::l2) = x::appendno(n - 1,l1,l2)
	 |  appendno(n, l1, nil) = nil;

  	fun Insert(1, x::l, y) = y::l
	 |  Insert(n, x::l, y) = x::Insert(n-1,l,y)
	 |  Insert(n, nil, y) = nil;

	local
		fun IsPage(nil, id) = false
		 |  IsPage(x::NodeList, id) =
			if x = id 
			then true 
			else IsPage(NodeList, id)

		fun TestPage (nil, id) = raise ErrorReturnPage
		 |  TestPage (x::PgList, id) = 
			if IsPage (DSStr_GetNodeList (x), id)
			then x
			else TestPage (PgList, id)
	in 
		fun ReturnPage(id) = TestPage(DSStr_GetPageList(), id)
	end;

	(* 	
		CalcDist calculates a grid distance. 
		It will return a number 1, 2, 5, 10 or a multiple of 10. 
		The parameter max is the value which the last grid 
		should represent. gn is the ideal grid number
	*)

	fun FindClosest (x) =
		if x <= 1 
		then 1
		else 
		   if x <= 3
		   then 2
		   else
			if x <= 7
			then 5
			else 10;

	fun FindDist (dist) =
		if dist < 10 
		then FindClosest (dist)
		else 10 * FindDist (round(real(dist) / 10.0));

	fun CalcDist (max: int, gn: int) =
		FindDist(round(real(max) / real(gn)));

	(* 
		CalcGN calculates a grid number.
		This grid number NGN will fulfill that:
			NGN will not vary more than 50% from gn.
			NDist*NGN > max and as close to as possible
	*)

	fun CalcGN (max:int, gn:int, NDist: int) =
		if (max > (gn * NDist))
		then
			let
			   val Dif = ceiling(real(max - (gn * NDist)) / real(NDist))
			in
			   gn + Dif
			end
		else
			let
			   val Dif = floor(real((gn * NDist) - max) / real(NDist))
			in
			   gn - Dif
			end

end;

(* Real specific Utils *)

structure RealUtilStr =
	struct
  		exception ErrorUpdateTag;

		(* 	
			CalcDist calculates a grid distance. 
			It will return a number 0.01, 0.02, ..., 0.1, 0.2, ....,
			1, 2, 5, 10 or a multiple of 10. 
			The parameter max is the value which the last grid 
			should represent. gn is the ideal grid number
		*)
	
		fun FindClosest (x) =
			if x <= 1.0
			then 1.0
			else 
			   if x <= 3.0
			   then 2.0
			   else 
				if x <= 7.0
				then 5.0
				else 10.0;

		fun FindDist (dist) =
			if dist < 10.0
			then
			   if dist < 1.0
			   then
			      if dist < 0.1
			      then
				if dist < 0.01 
				then 0.01
				else 0.01 * FindClosest (100.0 * dist)
			      else 0.1 * FindClosest (10.0 * dist)
			   else FindClosest (dist)
			else 10.0 * FindDist (dist / 10.0);

		fun CalcDist (max: real, gn: int) =
			FindDist(max / real(gn));

		(* 
			CalcGN calculates a grid number.
			This grid number NGN will fulfill that:
				NGN will not vary more than 50% from gn.
				NDist*NGN > max and as close to as possible
		*)

		fun CalcGN (max: real, gn:int, NDist:real) =
			if (max > (real(gn) * NDist))
			then
				let 
				   val Dif = UtilStr.ceiling((max - (real(gn) * NDist))/(NDist))
				in
				   if (Dif <= (gn div 2))
				   then
					gn + Dif
				   else
					gn + (gn div 2)
				end
			else
				let 
				   val Dif = floor(((real(gn) * NDist) - max) / (NDist))
				in
				   if Dif <= (gn div 2)
				   then
					gn - Dif
				   else
					gn - (gn div 2)
				end;

		fun CreateList (n, incr, 0) = nil
		 |  CreateList (n:real, incr:real, times) = 
				n::CreateList (n+incr, incr, times - 1);

		fun CreateRoundList (n, incr, 0) = nil
		 |  CreateRoundList (n: real, incr: real, times) =
				n::CreateRoundList (UtilStr.rounding(n+incr), incr, times - 1); 

		fun UpdateTag'i (nil,_) =()
		 |  UpdateTag'i (id::idlist ,(value:real)::valist) =
		    let
			val _ = DSText_Put {obj= id, text= Real.toString (value)};
		    in
			UpdateTag'i (idlist, valist)
		    end
		 |  UpdateTag'i  (_,_) = raise ErrorUpdateTag

end;

(* legend stuff *)

structure LegendStr  =
	struct
	structure UT = UtilStr;

(* Id : identification number of the box surrounding the legend
    PatternList : list of the pattern box identification number *)

	type LEGEND =  { Id : int ref, PatternList : (int * int) list};

	val EmptyLegend = { Id = ref 0, PatternList = nil} : LEGEND;
	val TextSize = ref (12: int);

	fun CreatePatternAndTag (pg, 0, XPattern, XTag, Y0, Step ,
							 LegendRegionId,BRH) = nil
	 |  CreatePatternAndTag (pg, col, XPattern, XTag, Y0, Step,
							 LegendRegionId, BRH) =
		let
		   val Y = Y0 + ((col - 1) * Step);
		   val ObjN = DSStr_CreateNode {page = pg, x= XPattern,
							y = Y, w = BRH,
							h = BRH, shape = 1}
		   and ObjL = DSStr_CreateLabel {page = pg, x = XTag,
							y = Y, w = 1, h = 1, 
							text = ""}
		   val _ = DSStr_MakeNodeIntoRgn { obj = ObjN, parent = LegendRegionId};
		   val _ = DSWtAttr_ObjectFillType { obj = ObjN,fill = UT.NthPattern (col)};

		   val _ = DSText_SetAttr {obj = ObjL, font = 5, 
						size = !TextSize, 
                                   		style = PlainText,
                               			just = LeftJustification};

		   val _ = DSStr_MakeNodeIntoRgn { obj = ObjL, parent = LegendRegionId} ;
		in
		   (ObjN, ObjL)::CreatePatternAndTag (pg, col - 1, XPattern, XTag, Y0, Step,LegendRegionId, BRH )
            	end;

(* LRX, LRY, LRW: legeng box's x and y coordinates and widh and height
     col : the number of pattern boxes
      BRH : the size of pattern boxes
  the height of the legend box is determined automatically and depends on BRH
*)
	fun CreateLegendRegion (pg, LRX, LRY, LRW, BRH, col) =
		let
		   val  LRH = (col* 3 * BRH) div 2 + BRH div 2
		   and Space = BRH div 2;
		   val LRI = DSStr_CreateNode {page = pg, x = LRX,
						y = LRY, w = LRW,
                                       		h= LRH, shape = RNDRECT }
		in
       		   {Id = ref  LRI, 
             	    PatternList = CreatePatternAndTag 
					(pg, col, LRX - UT.round(real(LRW) / 4.0), 
					(LRX - UT.round(real(LRW) / 4.0) + UT.round(real(BRH) / 2.0) + 5),
                                     	LRY - (LRH div 2) + 2 * Space,
                                     	Space +  BRH, LRI, BRH) } : LEGEND
		end;

	fun upd_rtag(legend : LEGEND,l) = 
			UT.UpdateTag'l (#PatternList (legend), UT.reverse l)

end;




