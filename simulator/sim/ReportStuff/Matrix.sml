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
(* File: Matrix.sml
 *
 * Matrix chart facilities
 *)

val rcsid =  "$Header: /users/cpntools/repository/cpn2000/sml/sim/ReportStuff/Matrix.sml,v 1.1.1.1 2001/10/02 11:35:29 cpn Exp $";

type MCHART =  {PRI : int ref, 
		RowList : (int ref) list list, 
		Legend: LegendStr.LEGEND ref};

structure ArrayStruct = struct
    structure UT = UtilStr;
    structure LE = LegendStr;

    fun CreateLines (pg, x, y, w, h, 0, pri, bool) = ()
      | CreateLines (pg, x, y, w, h, 1, pri, bool) = ()
      | CreateLines (pg, x, y, w, h, n, pri, bool) =
	if bool then
	    let
        	val obj = DSStr_CreateNode 
		    {page = pg, 
		     x = x + (if n mod 2 = 0 then 0 else w),
		     y = y, 
		     w = w+1, 
		     h = h, 
		     shape = 1};
		val _ = DSStr_MakeNodeIntoRgn {obj = obj, parent = pri}
	    in
		CreateLines (pg, x + (2 * w), y, w, h, n - 2, pri, bool)
	    end
	else		
	    let
        	val obj = DSStr_CreateNode 
		    {page = pg, 
		     x = x, 
		     y = y + (if n mod 2 = 0 then 0 else h),
		     w = w, 
		     h = h +1, 
		     shape = 1};
		val _ = DSStr_MakeNodeIntoRgn {obj = obj,parent = pri}
	    in
		CreateLines (pg, x, y + (2 * h), w, h, n - 2,pri, bool)
	    end

    fun CreateObjectsLine(pg,x,y,w,h,0,RgnNod) = nil
      | CreateObjectsLine(pg,x,y,w,h,n,RgnNod) =
	let 
	    val nod = DSStr_CreateNode 
		{page = pg, x = x, y = y, w = w div 2, h = h div 2, shape = 1};

	    val _ = DSWtAttr_ObjectFlags {obj = nod, flag = 1, value = true};

	    val _ = DSStr_MakeNodeIntoRgn {obj = nod, parent = RgnNod}
	in
	    (ref nod)::CreateObjectsLine(pg, x + w, y, w, h, n - 1, RgnNod)
	end;

    fun CreateObjects(pg, x, y, w, h, i, 0, RgnNod) = nil
      | CreateObjects(pg, x, y, w, h, i, j, RgnNod) =
	CreateObjectsLine(pg, x, y, w, h, i, RgnNod)::
	CreateObjects(pg, x, y - h, w, h, i, j - 1, RgnNod);

    val EmptyArray = {PRI = ref 0, 
		      RowList = [],
		      Legend = ref LE.EmptyLegend} : MCHART;

    fun CreateArray (pg, x, y, w, h, i, j, legend) = let
	val W = (w div (2*i)) * (2 * i) 
	and H = (h div (2*j)) * (2*j) 
	val RgnNod = DSStr_CreateNode 
	    {page = pg, x=x,y	= y,w = W, h = H, shape = 1}
	val _ = CreateLines(pg, x - W div 2 + W div (2 *i), y, 
			    W div i,H, i, RgnNod, true);
	val _ = CreateLines(pg, x, y - H div 2 + H div (2*j), 
			    W, H div j,j, RgnNod, false);
	val RowList = CreateObjects
	    (pg, x - W div 2 + W div (2 *i), y + H div 2 - H div (2*j),
	     W div i, H div j, i, j, RgnNod)
    in
	{PRI = ref RgnNod,RowList = RowList,Legend = legend }: MCHART
    end;

    fun ArrayFill (A : MCHART, i,j,color) = let
	val nod = !(UT.NthElement (i, UT.NthElement (j, #RowList A)))
    in
	if color = 0 then
	    DSWtAttr_ObjectFlags {obj = nod, flag = 1, value =true}
	else 
	    DSWtAttr_ObjectFillType {obj = nod,fill = UT.NthPattern (color)};
	if DSRdAttr_GetObjectFlags {obj = nod, flag = 1} andalso color<>0 then 
	    DSWtAttr_ObjectFlags {obj = nod, flag = 1, value = false}
	else ();
	DSText_Put {obj = nod, text = ""}
    end;

    fun ArrayWrite (A : MCHART, i,j,text) = let
	val nod = !(UT.NthElement (i, UT.NthElement (j, #RowList A))); 
	val _ = DSText_Put {obj = nod, text = text};
	val _ = DSWtAttr_ObjectFlags {obj = nod, flag = 1, value = false}
    in
	DSWtAttr_ObjectFillType {obj = nod, fill = 0}
    end;

    local 
	val A1 = ref (EmptyArray : MCHART) and legend1 = ref LE.EmptyLegend
    in
	fun create {title, page, x, y, w, h, i, j, legend, col} = let
	    val LRX = x + (w * 3 div 8)
	    and LRW =  (20 * w) div 100
	    and PRX = (if legend then (x - (w div 8)) else x)
	    and PRW = (if legend then (60 * w) div 100 else (80 * w) div 100)
	    and PRH = (70 * h) div 100
	    and PatternSize = 10
	    and CLI = DSStr_CreateLabel{page = page, 
					x = (x - (String.size (title) * 5)),
					y = (y - (h div 2) + 10),
					w = 10, h = 10, text=title};
	    val CNI = DSStr_CreateNode {page = page, x = x, y = y, 
					w = w, h = h, shape = 1}
	    val _ = DSStr_MakeNodeIntoRgn {obj = CLI, parent = CNI};
	    val _ = DSText_SetAttr {obj = CLI, font = 5, size = 20, 
				    style = Bold, just = Centered};
	    val _ = DSWtAttr_LineThickness {obj = CNI, thick = 2};
	    val _ = DSWtAttr_LineType {obj = CNI, line = 5};

	    val _ = 
		if legend then
		    (legend1:= LE.CreateLegendRegion (page, LRX, y, 
						      LRW, PatternSize,col);
		     DSStr_MakeNodeIntoRgn {obj = !(#Id (!legend1)), 
					    parent = CNI })
		else (); 
	    val A1 = ref(CreateArray (page, PRX, y, PRW, PRH, i, j, legend1));
	    val _ = DSStr_MakeNodeIntoRgn { obj = !(# PRI (!A1)),parent = CNI }
	in
	    !A1
	end;

	fun write {mc = A1, i = i, j = j, text = text} = 
	    ArrayWrite (!A1, i, j, text);

	fun upd_rtag {mc = A1 : MCHART ref, tags = l} = 
	    LE.upd_rtag (!(#Legend (!A1)), l)
    end

end;

fun MC'dec () = ref (ArrayStruct.EmptyArray : MCHART);

fun MC'create {patterns = col, height = H, i = I, j = J, legend = leg, 
	       title = title, width = W, x = X, y = Y} = 
    ArrayStruct.create{col = col, h = H, i = I, j = J, legend = leg, 
		       page = DSStr_GetCurPage (), title = title, 
		       w = W, x = X, y = Y};

fun MC'fill {mc = mc, pattern = col, i = i, j = j } =
    ArrayStruct.ArrayFill (!mc, i, j, col);

fun MC'write {mc = mc, i = i, j = j, text = text} =
    ArrayStruct.ArrayWrite(!mc, i, j, text);

fun MC'upd_ltag {mc = mc , tags = tags} =
    ArrayStruct.upd_rtag {mc = mc, tags = tags} ;

fun MC'delete (mc: (MCHART) ref) = 
    DSStr_DeleteObject (DSStr_GetParent (!(# PRI (!mc)))) ; 
