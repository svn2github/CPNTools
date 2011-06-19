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
(* File: LineGraph.sml
 *
 * Linegraph chart facilities
 *)

val rcsid =  "$Header: /users/cpntools/repository/cpn2000/sml/sim/ReportStuff/LineGraph.sml,v 1.1.1.1 2001/10/02 11:35:29 cpn Exp $";


(* STRUCTURE IMPLEMENTING A TYPE LINE TO WORK WITH LINE GRAPHS
Hopefully, the line object that exist in the kernel 1 version will be ported
to the Mac, so we won't need this structure anymore.
A line is composed of boxes connected together.
*)

(* modified by VP on Nov 18 to remove Arrow Heads *)

structure LineStr =
struct
	(* 
		Pg : current page the line belongs to
       	FstId : the ID of the first box created composing the line
       	LastId : the ID of the last box created
       	Timing : true for time-series graphs
       	Line : Pattern number for line
	   	Linetype : type of line (solid)
	   	Thick : border thickness 
       	Shape : this is for future extensions -> shape constant as the 
                boxes might be changed to ellipse or any other shape.
	*)
     
	type LINE = { Pg: int ref, FstId: int ref, LastId: int ref,
                  Timing: bool, Pattern: int ref, 
                  Linetype: int ref, Thick: int ref, Shape: int ref };
 
	val EmptyLine = { Pg = ref 0, FstId = ref 0, LastId = ref 0, 
    				  Timing = false, Pattern = ref 0, 
    				  Linetype = ref 0, Thick = ref 0, Shape = ref 0 }:LINE
	exception ErrorLast;

(* 	LINE CREATION 

	parameters:
		pg: page ID for the object to be created
		x, y: coordinate of the first point
		BoxSize: size of the box
		Pattern, shape, Timing : see later comments
*)

	fun	CreateLine(pg, x, y, BoxSize, pattern, shape, 
				   Timing, Linetype, Thick) =
    	let val nod2 = DSStr_CreateNode {page = pg, x = x, y = y, 
										 w = BoxSize, h = BoxSize, 
										 shape = shape}
     	in 
        (
        	DSWtAttr_ObjectFillType {obj = nod2, fill = pattern};
        	DSWtAttr_ObjectFlags {obj = nod2, flag = 128, value = true};
        	{Pg = ref pg, FstId = ref nod2, LastId = ref nod2, 
			 Timing = Timing, Pattern = ref pattern, 
         	 Linetype = ref Linetype, Thick = ref Thick, 
			 Shape = ref shape}:LINE
        )
     	end


	(* Link new connector box to the rest of the line *)

	fun LinkBoxes(Line:LINE, x, y, nod2, Parent) =
		let val posit = DSRdAttr_GetObjectCenter(!(#LastId Line))
     	in
		(
          	if ((#Timing Line) andalso (#x posit <> x) andalso (#y posit <> y))
          	then
          	(
          		let val breaknod =
              		DSStr_CreateNode { page = !(#Pg Line), x = x, 
									   y = #y posit, w = 1, h = 1, shape = 1}
          		in
            	(
            		let val con2 = 
						DSStr_CreateConn { page = !(#Pg Line),
										   node1 = !(#LastId Line), 
										   node2 = breaknod }
                		and con3 = 
						DSStr_CreateConn { page = !(#Pg Line), 
										   node1 = breaknod,
										   node2 = nod2 }
            		in 
					(
               			DSWtAttr_ConnOrient { conn = con2, orient = NODIR };
						DSWtAttr_ConnOrient { conn = con3, orient = NODIR };

              			DSWtAttr_ObjectVisuals { fill = !(#Pattern Line), 
              									 obj = con2, 
                                                 line = !(#Linetype Line), 
              									 thick = !(#Thick Line), 
												 vis = true } ;
						DSUI_Redraw con2;
              			DSWtAttr_ObjectVisuals { fill = !(#Pattern Line), 
              									 obj = con3, 
												 line = !(#Linetype Line), 
              									 thick = !(#Thick Line), 
												 vis = true } ;
						DSUI_Redraw con3
					)
           		 	end;

            		DSWtAttr_ObjectFlags {obj = breaknod, flag = 128, 
										  value = true};

            		DSStr_MakeNodeIntoRgn {obj = breaknod, 
										   parent = Parent}
             	)
          		end
          	)
          	else
          	(
          		let val con2 = 
					DSStr_CreateConn {page = !(#Pg Line),
									  node1 = !(#LastId Line), 
									  node2 = nod2}
          		in 
					DSWtAttr_ConnOrient {conn = con2, orient = NODIR};

              		DSWtAttr_ObjectVisuals {fill = !(#Linetype Line), 
              								obj = con2, 
											line = !(#Linetype Line), 
              								thick = !(#Thick Line), 
											vis = true } ; 
					DSUI_Redraw con2
          		end
          	);

          	Line
         )
     	end


	(* Creates new connector box *)

	fun create_box (Line:LINE, x, y, Parent) =
	(
    	let val size = DSRdAttr_GetObjectSize(!(#LastId Line))
    	in
		(
       		let val nod2 = 
				DSStr_CreateNode {page = !(#Pg Line), x = x, y = y, 
								  w = #w size, h = #h size, 
								  shape = !(#Shape Line)}
       		in
			(
				DSWtAttr_ObjectFillType { obj = nod2, fill = !(#Pattern Line)};

				DSWtAttr_ObjectFlags {obj = nod2, flag = 128, value = true};

          		DSStr_MakeNodeIntoRgn {obj = nod2, parent = Parent};

				nod2
			) 
      		end
		)
   		end
	)


	(* 	
		Draw new line segment. A new connector box is created. 
		If drawline, it is connected to the rest of the line 
	*)

	fun draw_line(Line:LINE, x, y, Parent, drawline) =
	(
		let val nod = create_box(Line, x, y, Parent)
		in
		(
			let val UpdLine = 
				if drawline then
          			LinkBoxes(Line, x, y, nod, Parent)
				else Line
			in
			(
				#LastId UpdLine := nod;
				UpdLine
			)
			end
		)
		end
	)


	(* Draw line and change pattern of line *)

	fun draw_cline(Line:LINE, x, y, pat, Parent, drawline) =
	(
        #Pattern Line := pat;

		let val nod = create_box(Line, x, y, Parent)
		in
		(
			let val UpdLine =
				if drawline then
          			LinkBoxes(Line, x, y, nod, Parent)
				else Line
			in
			(
				#LastId UpdLine := nod;
				UpdLine
			)
			end
		)
		end
	)


  	fun change_pattern(Line:LINE, pat) =
    	(
   			#Pattern Line := pat;
   			DSWtAttr_ObjectFillType { obj = !(#LastId Line), fill = pat};
   			Line
    	)


  	fun same(L1 : LINE, L2 : LINE) =
    	(!(#FstId L1) = !(#FstId L2))


	(* Creates list of empty lines *)

  	fun CreateNilList(0) = nil
  	 | 	CreateNilList(no_line) = 
    		EmptyLine::CreateNilList(no_line - 1)


	fun CreateInitBoxes (AxeRgnId, DL, pg, x, y, BoxSize, TIMING, LINETYPE, 
					     THICK, 0) = DL
	 |  CreateInitBoxes (AxeRgnId, DL, pg, x, y, BoxSize, TIMING, LINETYPE, 
					     THICK, n) =
	(
		let val InitNod = CreateLine(pg, x, y, BoxSize, UtilStr.NthPattern(n),
									 1, TIMING, LINETYPE, THICK)
		in
		(
			DSStr_MakeNodeIntoRgn {obj = !(#FstId InitNod), 
								   parent = AxeRgnId};
            
			CreateInitBoxes (AxeRgnId, UtilStr.Insert(n, DL, InitNod), pg, 
							 x, y, BoxSize, TIMING, LINETYPE, THICK, n-1)  
        )
        end
	)

	
 	fun Init(no_line, AxeRgnId, pg, x, y, BoxSize, TIMING, LINETYPE, THICK,
			 StartAtOrigin) =
	( 
		let val DL = CreateNilList(no_line)
		in
		(
			if StartAtOrigin then
				CreateInitBoxes(AxeRgnId, DL, pg, x, y, BoxSize, TIMING, 
								LINETYPE, THICK, no_line)
			else DL
		)
		end
	)


	(* Sets all lines to be the empty line*)

	fun init_all nil = ()
	 |	init_all (L1::Ll : LINE list) =
	(
		DSStr_DeleteObject(!(#FstId L1));
		(#FstId L1) := 0;
		init_all(Ll)
	)


	(* Makes all nodes in the list into regions of nod *)

	fun MakeAllRgn (nod, nil) = ()
	 |	MakeAllRgn (nod, n1::nl) =
	(
		DSStr_MakeNodeIntoRgn { obj = n1, parent = nod };
		MakeAllRgn (nod, nl)
	)
	

	(* 	
		Deletes the part of the lines, which is smaller than Min, when incr.
		with Move
	*)

	fun DelXSmaller (nil, MinPos, Parent) = ()
	 |	DelXSmaller (L1::Ll : LINE list, MinPos, Parent) =
	(
		if not(same(L1, EmptyLine)) then
		(
			if (#x (DSRdAttr_GetObjectCenter(!(#FstId L1))) < MinPos)
			then
			(
				let val NewLine = DSStr_GetObjectRegionList(!(#FstId L1))
				in
				(
					if (NewLine <> nil) then
					(
						let val NewFirstId = hd NewLine
							and NewRestIds = tl NewLine
						in
						(
							DSStr_MakeNodeIntoRgn{obj = NewFirstId, 
											   	  parent = Parent};
							MakeAllRgn(NewFirstId, NewRestIds);
							DSStr_DeleteObject(!(#FstId L1));
							#FstId L1 := NewFirstId
						)
						end
					)
					else 
					(
						DSStr_DeleteObject(!(#FstId L1));
						(#FstId L1) := 0
					)	
				)
				end;
				
				DelXSmaller (L1::Ll, MinPos, Parent)
			)
			else
				DelXSmaller (Ll, MinPos, Parent)
		)
		else DelXSmaller (Ll, MinPos, Parent)
	)


	(* 	
		Deletes the part of the lines, which is smaller than Min, when incr.
		with Move
	*)

	fun DelYSmaller (nil, MinPos, Parent) = ()
	 |	DelYSmaller (L1::Ll : LINE list, MinPos, Parent) =
	(
		if not(same(L1, EmptyLine)) then
		(
			if (#y (DSRdAttr_GetObjectCenter(!(#FstId L1))) > MinPos)
			then
			(
				let val NewLine = DSStr_GetObjectRegionList(!(#FstId L1))
				in
				(
					if (NewLine <> nil) then
					(
						let val NewFirstId = hd NewLine
							and NewRestIds = tl NewLine
						in
						(
							DSStr_MakeNodeIntoRgn{obj = NewFirstId, 
											   	  parent = Parent};
							MakeAllRgn(NewFirstId, NewRestIds);
							DSStr_DeleteObject(!(#FstId L1));
							#FstId L1 := NewFirstId
						)
						end
					)
					else 
					(
						DSStr_DeleteObject(!(#FstId L1));
						(#FstId L1) := 0
					)
				)
				end;
				
				DelYSmaller (L1::Ll, MinPos, Parent)
			)
			else
				DelYSmaller (Ll, MinPos, Parent)
		)
		else DelYSmaller (Ll, MinPos, Parent)
	)


	(* 	
		Deletes the part of the lines, which is larger than Max, when decr..
		with Move
	*)

	fun DelXLarger (nil, MaxPos, Parent) = ()
	 |	DelXLarger (L1::Ll : LINE list, MaxPos, Parent) =
	(
		if not(same(L1, EmptyLine)) then
		( 
			if (#x (DSRdAttr_GetObjectCenter(!(#FstId L1))) > MaxPos)
			then
			(
				let val NewLine = DSStr_GetObjectRegionList(!(#FstId L1))
				in
				(
					if (NewLine <> nil) then
					(
						let val NewFirstId = hd NewLine
							and NewRestIds = tl NewLine
						in
						(
							DSStr_MakeNodeIntoRgn{obj = NewFirstId, 
											      parent = Parent };
							MakeAllRgn(NewFirstId, NewRestIds);
							DSStr_DeleteObject(!(#FstId L1));
							#FstId L1 := NewFirstId
						)
						end
					)
					else 
					(
						DSStr_DeleteObject(!(#FstId L1));
						(#FstId L1) :=  0
					)
				)
				end;
				
				DelXLarger (L1::Ll, MaxPos, Parent)
			)
			else
				DelXLarger (Ll, MaxPos, Parent)
		)
		else DelXLarger (Ll, MaxPos, Parent)
	)


	(* 	
		Deletes the part of the lines, which is larger than Max, when decr.
		with Move
	*)

	fun DelYLarger (nil, MaxPos, Parent) = ()
	 |	DelYLarger (L1::Ll : LINE list, MaxPos, Parent) =
	(
		if not(same(L1, EmptyLine)) then
		( 
			if (#y (DSRdAttr_GetObjectCenter(!(#FstId L1))) < MaxPos)
			then
			(
				let val NewLine = DSStr_GetObjectRegionList(!(#FstId L1))
				in
				(
					if (NewLine <> nil) then
					(
						let val NewFirstId = hd NewLine
							and NewRestIds = tl NewLine
						in
						(
							DSStr_MakeNodeIntoRgn{obj = NewFirstId, 
											      parent = Parent};
							MakeAllRgn(NewFirstId, NewRestIds);
							DSStr_DeleteObject(!(#FstId L1));
							#FstId L1 := NewFirstId
						)
						end
					)
					else 
					(
							DSStr_DeleteObject(!(#FstId L1));
							(#FstId L1) :=  0
					)
				)
				end;
				
				DelYLarger (L1::Ll, MaxPos, Parent)
			)
			else
				DelYLarger (Ll, MaxPos, Parent)
		)
		else DelYLarger (Ll, MaxPos, Parent)
	)


	(* Makes the lines in the line list into regions of nod *)

   	fun MakeLineRgn(nil, nod) = ()
  	 | 	MakeLineRgn((Li : LINE)::L, nod) =
       	(
         	if not (same (Li, EmptyLine)) then 
         		DSStr_MakeNodeIntoRgn {obj = !(#FstId Li), parent = nod}
         	else ();
         		MakeLineRgn(L, nod)
      	)

end;


(* 	
	Contains functions which is used to create and update both
	real and int line charts
*)

structure LC =
	struct
	structure UT = UtilStr;

	(* 
		Signature
		sig
    		val CreateAxesTags : int * int * int * int * int * int -> int list
    		val CreateIndexStrList : string * int * int -> string list
    		val CreateTitle : int * int * int * int -> int
    		val CreateXAxes : int * int * int * int -> int
    		val CreateYAxes : int * int * int * int -> int
    		val XCreateGridLines : int * int * int * int * int * int * int 
									-> int list
   			val YCreateGridLines : int * int * int * int * int * int * int 
									-> int list
  		end
	*)


	fun XCreateGridLines (pg, x, y, w, h, 0, pri) = nil
	 |	XCreateGridLines (pg, x, y, w, h, n, pri) =
			let val line = DSStr_CreateLine{page = pg, 
											points = [
												x, 
											 	y - UT.round(real(h) / 2.0),
											 	x, 
											 	y + UT.round(real(h) / 2.0)]}
			in
			(
				DSStr_MakeNodeIntoRgn { obj = line, parent = pri };
				line::XCreateGridLines (pg, x+w, y, w, h, (n-1), pri)
			)
			end;


	fun YCreateGridLines (pg, x, y, w, h, 0, pri) = nil
	 |	YCreateGridLines (pg, x, y, w, h, n, pri) =
			let val line = DSStr_CreateLine{page = pg, 
											points = [
												x - UT.round(real(w) / 2.0), 
											 	y,
											 	x + UT.round(real(w) / 2.0), 
											 	y]}
			in
			(
				DSStr_MakeNodeIntoRgn { obj = line, parent = pri };
				line::YCreateGridLines (pg, x, y+h, w, h, (n-1), pri)
			)
			end;

	 
	fun CreateXAxes(pg, ycenter, x, w) =
		(
			DSStr_CreateLine{page = pg, 
							 points = 
								[x - UT.round(real(w) / 2.0), 
								 ycenter,
								 x + UT.round(real(w) / 2.0), 
								 ycenter
								]}
		)


	fun CreateYAxes(pg, xcenter, y, h) =
	(
		DSStr_CreateLine{page = pg, 
						 points = 
								[xcenter, 
								 y - UT.round(real(h) / 2.0),
								 xcenter, 
								 y + UT.round(real(h) / 2.0)
								]}
	) 
	

	(* Creates the Axis Name regions *)

	fun CreateAxesTags (pg, x1, y1, x2, y2, AxeRgnId) =
		(
 			let val labx = (DSStr_CreateLabel {	page = pg, x = x2,
                             					y = y2, w = 10, h = 10,
												text = "x-axis"})
				and laby = (DSStr_CreateLabel {	page = pg, x = x1,
                             					y = y1, w = 10, h = 10,
												text = "y-axis"})
        	in
        	(

        		DSText_SetAttr {obj = laby, font = 5, size = 12, 
                                style = PlainText,
                                just = LeftJustification};

        		DSStr_MakeNodeIntoRgn {obj = laby, parent = AxeRgnId };

				DSText_SetAttr {obj = labx, font = 5, size = 12, 
                                style = PlainText,
                                just = LeftJustification};

        		DSStr_MakeNodeIntoRgn {obj = labx, parent = AxeRgnId };
        		[labx, laby]
			)
        	end
		)


	(*
		Creates a title region for a chart with the position 
		'CNX', 'CNY' and with the height 'CHN'. The title 
		region is created with the default text "Title"
	*)

	fun CreateTitle (pg, x, y, h) =
	(
		let val TitelRegion = (DSStr_CreateLabel{
									page = pg, 
									x = (x - (String.size ("Title") * 5)),
							    	y = (y - (h	 div 2) + 10),
							    	w = 10, h = 10, text = "Title"})
		in
		(

			DSText_SetAttr {obj = TitelRegion, font = 5, size = 20, 
							style = Bold, just = Centered};

			TitelRegion
		)
		end
	)


	(*	
		Creates a list of indexed strings: [s1, s2, ..., sm] where s
		is a string
	*)

	fun CreateIndexStrList(s, i, m) =
	(
		if (i = m) then 
			nil
		else
			(s^Int.toString(i))::CreateIndexStrList(s, i+1, m)
	)

end;


type 'a LINECHART = {	DL : LineStr.LINE list ref,
        				pg : int ref,
       					BoxSize : int,
       					XAxis : int ref, 
						YAxis : int ref,
       					AxeRgnId : int ref,
       					TIMING : bool,
       					LINETYPE : int,
       					THICK : int,
						XGridList : int list ref,
						YGridList : int list ref,
       					XTag : int list ref, 
						YTag : int list ref,
						XIdealGN : int,
						YIdealGN : int,
						XDist : 'a,
						YDist : 'a,
						XMinGN : int ref,
						XMaxGN : int ref,
						YMinGN : int ref,
						YMaxGN : int ref,
       					XMIN : 'a ref, 
						XMAX : 'a ref, 
						YMIN : 'a ref, 
						YMAX : 'a ref,
						XStartMin : 'a,
						XStartMax : 'a,
						YStartMin : 'a,
						YStartMax : 'a,
						XStartCent : 'a,
						YStartCent : 'a,
       					Xn : 'a ref, 
						Yn : 'a ref,
       					KX : real ref,
       					KY : real ref,
       					Xo : int ref,
       					Yo : int ref,
       					Ido : int ref,
       					AxTags : int list ref,
       					Legend : LegendStr.LEGEND ref,
						PRX : int ref,
						PRY : int ref,
						PRW : int ref,
						PRH : int ref,
						GridVisible : bool,
						Rescale : bool,
						GridDyn : bool,
						Title : int ref,
						Clear : bool,
						InitToOrigin : bool
					};


(*
	Contains functions to create and update integer line charts
*)

structure ILC =
	struct
  	structure UT = UtilStr;
  	structure LI = LineStr;
  	structure LE = LegendStr;
	
	(*
		Signature
  		sig
    		val EmptyLinechart : int LINECHART
    		val LC'clear : ( int LINECHART ref ) -> unit
    		val LC'create : {
					FixedNo:bool,
					RescaleAxis:bool,
					XDist:int,
					XIdealGN:int,
					YDist:int, 
					YIdealGN:int,
					AxisName:bool,
					BoxSize:int,
					TicksOnsly:bool,
					height:int,
					Legend:bool,
					NoOflines:int,
					LineThick:int,
					Horiz/Vert:bool,
					Title:bool,
					ClearChart:bool,
					InitToOrigin:bool,
					StartAtOrigin:bool,
					width:int,
					x:int,
					XOrigin:int,
					XMax:int,
					XMin:int,
					y:int,
					YCent:int,
					YMax:int,
					YMin:int} -> int LINECHART
    		val LC'delete : int LINECHART ref -> unit
    		val LC'upd_atag : {name:int LINECHART ref,x:string,y:string} 
								-> unit
    		val LC'upd_line : {drawline:bool,name:int LINECHART ref,
							   line:int,x:int,y:int}-> unit
    		val LC'upd_chart : {name :int LINECHART ref,
							    values = (int * int * int * bool * bool) list}
								-> unit
    		val LC'upd_ltag : {name:int LINECHART ref,tags:string list} -> unit
    		val LC'upd_pline : {drawline:bool,name:int LINECHART ref,
								line:int,pattern:int, x:int,y:int} -> unit
    		val LC'upd_title : {name:int LINECHART ref,title:string} -> unit
    		val LabelAxesX : int * int * int * int * int * int * 'a * 'b * 
							 int * int * int * int * bool -> int list
    		val LabelAxesY : int * int * int * int * int * int * 'a * 'b * 
							 int * int * int * int * bool -> int list
    		val ReducPerc : real
    		val ResizeX : int * int LINECHART -> unit
    		val ResizeXLines : int * int * int LINECHART -> unit
    		val ResizeY : int * int LINECHART -> unit
    		val ResizeYLines : int * int * int LINECHART -> unit
    		val UpdatePlotPosition : int LINECHART -> unit
    		val UpdatePlotSize : int LINECHART -> unit
    		val Xold : int * int LINECHART -> int
    		val Yold : int * int LINECHART -> int
    		val clear_chart : int LINECHART -> int LINECHART
    		val create : {
					GridDyn:bool,
					Legend:bool,
					Rescale:bool,
					XDist:int,
					XIdealGN:int,
					YDist:int,
					YIdealGN:int,
					axis:bool,
					boxsize:int,
					gridvisible:bool,
					h:int,
					linetype:int,
					nline:int,
					page:int,
					thick:int,
					timing:bool,
					title:bool,
					Clear:bool,
					InitToOrigin:bool,
					StartAtOrigin:bool,
					w:int,
					x:int,
					xcent:int,
					xmax:int,
					xmin:int,
					y:int,
					ycent:int,
					ymax:int,
					ymin:int} -> int LINECHART
    		val create_Xgrids : int * int * int * int * int * int * int * 
								int * int * 'a * 'b * int * int * int 
								-> int list
    		val create_Ygrids : int * int * int * int * int * int * int * 
								int * int * 'a * 'b * int * int * int 
								-> int list
    		val create_graph : int * int * int * int * bool * int * int * 
							   bool * bool * bool * int * int * int * int * 
							   int * int * int * int * int * int * 
							   LegendStr.LEGEND ref * bool * bool * int * 
							   int * int * int * int ref -> int LINECHART
    		val updXY_line : int * int * int * int LINECHART * bool * int * 
							 bool -> int LINECHART
    		val upd_atag : string list * int LINECHART -> unit
    		val upd_line : int * int * int * bool * int LINECHART 
							-> int LINECHART
			val upd_pline : int * int * int * bool * int LINECHART * int 
							-> int LINECHART
    		val upd_title : int LINECHART * string -> unit
  		end
	*)

	(*  RECORD COMPONENTS:
			DL : list of object of type LINE
       		pg : current page the line graph belongs to
       		BoxSize : size of the boxes composing a line
       		XAxis, YAxis : x-, y-coordinate lines
       		AxeRgnId : ID number of the box surrounding the line graph
       		TIMING : true means time-series graph
			LINETYPE : type of lines
			THICK : thickness of lines
       		XGridList, YGridList : lists of grid line ids
			XTag, YTag : list of the x and y tick tags 
			XIdealGN, YIdealGN : preferred number of grids
			XDist, YDist : grid distance - if permanent  
			XMinGN, XMaxGN, YMinGN, YMaxGN : current number of grids 
       		XMIN, XMAX, YMIN, YMAX: minimum and maximum values on 
									the x and y axes
			XStartMin, XStartMax, 
			YStartMin, YStartMax, 
			XStartCent, YStartCent : initial values. Used to initialize chart  
       		Xn, Yn : coordinates of the center in the new system 
                   (meaning not in in the mac coordonates system)
       		KX, KY: reduction value on the X and Y axes from the Mac 
					 system coordonates to the new system (the line graph)
       		Xo, Yo : value of the center in the Mac coordonates
       		Ido : 	is used as a reference to get the center's coordonates
        			whenever the graph moves 
			AxTags : axis tag ids
       		Legend : legend id
			PRX, PRY, PRW, PRH : size and position of plot area
			GridVisible : show grids or just ticks
			Rescale : rescale or move axis when overflow
			GridDyn : fixed number of grids or fixed grid distance
			Title : title region id
			Clear : whether the chart should be cleared when initialized
			InitToOrigin : whether the lines should start at origin when
						   the chart is initialized
			StartAtOrigin : whether lines should start at origin
*)
   
	val EmptyLinechart = {	DL = ref ([] : LI.LINE list),
       						pg = ref 0,
       						BoxSize = 0,
       						XAxis = ref 0, 
							YAxis = ref 0,
       						AxeRgnId = ref 0,
       						TIMING = true,
       						LINETYPE = 0,
       						THICK = 0,
							XGridList = ref ([] : int list),
							YGridList = ref ([] : int list),
       						XTag = ref ([] : int list), 
							YTag = ref ([] : int list),
							XIdealGN = 0,
							YIdealGN = 0,
							XDist = 0,
							YDist = 0,
							XMinGN = ref 0,
							XMaxGN = ref 0,
							YMinGN = ref 0,
							YMaxGN = ref 0,
       						XMIN = ref 0,
							XMAX = ref 0, 
							YMIN = ref 0, 
							YMAX = ref 0,
							XStartMin = 0,
							XStartMax = 0,
							YStartMin = 0,
							YStartMax = 0,
							XStartCent = 0,
							YStartCent = 0,
       						Xn = ref 0, 
							Yn = ref 0,
       						KX = ref 0.0,
       						KY = ref 0.0,
       						Xo = ref 0,
       						Yo = ref 0,
       						Ido = ref 0,
       						AxTags = ref ([]:int list),
       						Legend = ref LE.EmptyLegend,
							PRX = ref 0,
							PRY = ref 0,
							PRW = ref 0,
							PRH = ref 0,
							GridVisible = true,
							Rescale = true,
							GridDyn = true,
							Title = ref 0,
							Clear = true,
							InitToOrigin = false } : int LINECHART;

  	val ReducPerc : real = 75.0 / 100.0;

	(* Calculates x-coord in new system corresponding x in old system *)

  	fun Xold(x, L : int LINECHART) = 
			floor(real (!(#Xo L)) + !(#KX L) * real(x - !(#Xn L)))


	(* Calculates y-coord in new system corresponding x in old system *)

  	fun Yold(y, L : int LINECHART) = 
			floor(real (!(#Yo L)) + !(#KY L) * real(y - !(#Yn L)))


	(*	
		Updates the entries representing the x-coordinate and 
		the y-coordinate of the plot region to actually fit the size. 
		There can be inconsistencies if the region has be repositioned
	*)

	fun	UpdatePlotPosition(L : int LINECHART) =
		let val {x = x1, y = y} = DSRdAttr_GetObjectCenter(!(#XAxis L))
			and {x = x, y = y1} = DSRdAttr_GetObjectCenter(!(#YAxis L))
		in 
		(
			(#PRX L) := x1;
			(#PRY L) := y1 
		) 
		end


	(*	
		Updates the entries representing the heigth, width, position
		of center in the Mac coordinates and the x and y-reduction 
		values of the plot region to actually fit the graph. 
		There can be inconsistencies if the region has be repositioned
	*)

   	fun UpdatePlotSize(L : int LINECHART) =
      	(
      		let val nodpos = DSRdAttr_GetObjectCenter(!(#Ido L))
      		in
      		(
       			#Xo L := #x nodpos;
       			#Yo L := #y nodpos
      		)
      		end;

      		#PRH L := #h (DSRdAttr_GetObjectSize (!(#YAxis L)));
      		#PRW L := #w (DSRdAttr_GetObjectSize (!(#XAxis L)));
      		#KX L := real (!(#PRW L)) / real(!(#XMAX L) - !(#XMIN L));
      		#KY L := real(!(#PRH L)) / real(!(#YMIN L) - !(#YMAX L))
      	)


	(* Updates axis tags *)

  	fun upd_atag (l, L : int LINECHART) =  
			UT.UpdateTag's (!(#AxTags L), l)


	(* Updates Title region *)

	fun upd_title (L : int LINECHART, title) =
		UT.UpdateTag's ([!(#Title L)], [title])


	(* Creates labels on the x-axis *)

   	fun LabelAxesX (pg, parent, x, y, w, xmin, XMaxWidth, XMinWidth,
					XMaxTick, XMinTick, xngn, XTagDist, gridvisible) =
  
        let val ugt = 
			UT.CreateTagRegion (
					pg, 
					x - (XMinTick * w) - 15,
					y, XMinTick, w, parent, true)@
			UT.CreateTagRegion (
					pg, 
					if gridvisible then
						x 
					else
						x - 10,
					y, 1, w, parent, true)@ 
			UT.CreateTagRegion (
					pg, 
					x + w,
					y, XMaxTick, w, parent, true)
		in 
		(
			UT.UpdateTag'i (ugt, UT.CreateList (xmin, XTagDist, xngn + 1));
			ugt 
		)
		end


	fun create_Xgrids (pg, parent, x, y, w, h, XNMax, XNMin, XCent: int, 
					   XMaxWidth, XMinWidth, XMaxTick, XMinTick, xngn) =
	(
		(if (XNMax > XCent) then
		 (
			LC.XCreateGridLines (
				pg,
				x + (w div xngn),
				y, (w div xngn), 
				h, XMaxTick, parent)
		 )
		 else nil)@
		(if (XNMin < XCent) then
		 (
			LC.XCreateGridLines (
				pg,
				x - (w div xngn), 
				y, (~(w div xngn)), 
				h, XMinTick, parent)
		 )
		 else nil)
	)


	fun create_Ygrids (pg, parent, x, y, w, h, YNMax, YNMin, YCent: int, 
					   YMaxWidth, YMinWidth, YMaxTick, YMinTick, yngn) =
	(
		(if (YNMax > YCent) then
		 (
			LC.YCreateGridLines (
				pg,
				x, 
				y - (h div yngn),
				w, (~(h div yngn)), 
				YMaxTick, parent)
		 )
		 else nil)@
		(if (YNMin < YCent) then
		 (
			LC.YCreateGridLines (
				pg,
				x, 
				y + (h div yngn), w,
				(h div yngn), YMinTick, parent)
		 )
		 else nil)
	)


	(* Creates the labels on the y-axis *)

   	fun LabelAxesY (pg, parent, x, y, h, ymin, YMaxWidth, YMinWidth,
					YMaxTick, YMinTick, yngn, YTagDist, gridvisible) =
  
        let val ugt = 
			UT.CreateTagRegion (
					pg, 
					x,
					y + (YMinTick * h),
					YMinTick, (~h), parent, false)@
			UT.CreateTagRegion (
					pg,
					x, 
					if gridvisible then
						y 
					else
						y + 10,
					1, h, parent, false)@
			UT.CreateTagRegion (
					pg, 
					x,
					y - h,
					YMaxTick, (~h), parent, false)
		in 
		(
			UT.UpdateTag'i (ugt, UT.CreateList (ymin, YTagDist, yngn + 1));
			ugt 
		)
		end


  	fun create_graph (AxeRgnId, pg, nline, boxsize, timing, 
					  linetype, thick, gridvisible, axis,
					  x, y, w, h, xmin, xmax, ymin, ymax, xcent, ycent, 
					  legend, rescale, GridDyn, xidealGN, yidealGN, 
					  xdist, ydist, title, Clear, InitToOrigin,
					  StartAtOrigin) =
	let val XMinWidth = UT.round(real(w * (xcent - xmin)) / real(xmax - xmin))
		and XMaxWidth = w - 
						UT.round(real(w * (xcent - xmin)) / real(xmax - xmin))
		and YMinWidth = UT.round(real(h * (ycent - ymin)) / real(ymax - ymin))
		and YMaxWidth = h - 
						UT.round(real(h * (ycent - ymin)) / real(ymax - ymin))
		and XTagDist = if GridDyn then UT.CalcDist((xmax - xmin), xidealGN)
					   else xdist
		and YTagDist = if GridDyn then UT.CalcDist((ymax - ymin), yidealGN)
					   else ydist
	in
	(
		let val Xa = x + UT.round(real(XMinWidth - XMaxWidth) / 2.0)
  	    	and Ya = y - UT.round(real(YMinWidth - YMaxWidth) / 2.0)

		in
		(
       		let val XAxes = LC.CreateXAxes(pg, Ya, x, w)
				and YAxes = LC.CreateYAxes(pg, Xa, y, h)
				and XMaxTick = (if GridDyn then
									UT.CalcGN((xmax - xcent), 
											  UT.round(
													real((xmax - xcent) * 
														 xidealGN) / 
													real(xmax - xmin)), 
											  XTagDist)
							   else UT.ceiling(real(xmax - xcent) / 
											   real(XTagDist))) 
				and XMinTick = (if GridDyn then
									UT.CalcGN((xcent - xmin),
									   		  xidealGN - 
									   		  UT.round(
													real((xmax - xcent) * 
														  xidealGN) / 
													real(xmax - xmin)),
									   		  XTagDist)
								else UT.ceiling(real(xcent - xmin) / 
												real(XTagDist)))
				and YMaxTick = (if GridDyn then
									UT.CalcGN((ymax - ycent), 
									   	  	  UT.round(
													real((ymax - ycent) * 
														  yidealGN) / 
													real(ymax - ymin)), 
									   	  	  YTagDist)
							    else UT.ceiling(real(ymax - ycent) / 
												real(YTagDist)))
				and YMinTick = (if GridDyn then
									UT.CalcGN((ycent - ymin),
									      	  yidealGN - 
									      	  UT.round(
													real((ymax - ycent) * 
													     yidealGN) / 
													real(ymax - ymin)),
							     		  	  YTagDist)
								else UT.ceiling(real(ycent - ymin) / 
												real(YTagDist)))
       		in
			(
				let val XMaxTick = 
						if ((XMaxTick = 0) andalso (XMinTick = 0)) then 1
						else XMaxTick
					and YMaxTick = 
						if ((YMaxTick = 0) andalso (YMinTick = 0)) then 1
						else YMaxTick
				in
				( 
					let val XNMin = xcent - (XTagDist * XMinTick)
						and XNMax = xcent + (XTagDist * XMaxTick)
						and YNMin = ycent - (YTagDist * YMinTick)
						and YNMax = ycent + (YTagDist * YMaxTick)
					in
					(       
       					DSStr_MakeNodeIntoRgn {	obj = XAxes, 
										   		parent = AxeRgnId};
       					DSStr_MakeNodeIntoRgn {	obj = YAxes, 
										   		parent = AxeRgnId};
						let val xngn = XMinTick + XMaxTick
							and yngn = YMinTick + YMaxTick
						in
						(
       						let val AxeLabX = 
									LabelAxesX (pg, 
												if gridvisible then AxeRgnId
												else XAxes, Xa, 
												if gridvisible then
													(y + (h div 2) + 10)
												else 
													(Ya + 10), 
												(w div xngn), 
												XNMin, XMaxWidth, XMinWidth,
												XMaxTick, XMinTick,
												xngn, XTagDist, gridvisible)

           						and AxeLabY = 
									LabelAxesY (pg, 
												if gridvisible then AxeRgnId
												else YAxes, 
												if gridvisible then
													(x + (w div 2) + 10)
												else
													(Xa + 10), 
												Ya, (h div yngn), 
												YNMin, YMaxWidth, YMinWidth,
												YMaxTick, YMinTick,
												yngn, YTagDist, gridvisible)

           						and AxTags = 
									(if axis then 
										LC.CreateAxesTags (
												pg, Xa, (y - h div 2) - 15,  
												(x + w div 2) + 15, Ya - 5,
												AxeRgnId) 
								 	 else [])
               				in
							(
         					{
								DL = ref (LI.Init(nline, AxeRgnId, pg, Xa, Ya,
												  boxsize, timing, linetype,
												  thick, StartAtOrigin)),
						 
								pg = ref pg,
         				 
								BoxSize = boxsize,
         				 
								XAxis = ref (XAxes),
						 
								YAxis = ref (YAxes),

								AxeRgnId = ref AxeRgnId,

         						TIMING = timing,

         						LINETYPE = linetype,

         						THICK = thick,

								XGridList = 
									ref(create_Xgrids(
									pg, 
									if gridvisible then AxeRgnId else XAxes, 
									Xa, 
									if gridvisible then y else Ya, 
									w, if gridvisible then h else 4, 
									XNMax, XNMin, xcent, XMaxWidth, XMinWidth, 
									XMaxTick, XMinTick, xngn)),

								YGridList =
									ref(create_Ygrids(
									pg, 
									if gridvisible then AxeRgnId else YAxes, 
									if gridvisible then x else Xa, 
									Ya, if gridvisible then w else 4, h, 
									YNMax, YNMin, ycent, YMaxWidth, YMinWidth, 
									YMaxTick, YMinTick, yngn)),

         						XTag = ref (AxeLabX),
	
         						YTag = ref (AxeLabY),

								XIdealGN = xidealGN,

								YIdealGN = yidealGN,

								XDist = xdist,
						
								YDist = ydist,

								XMinGN = ref XMinTick,

								XMaxGN = ref XMaxTick,
				
								YMinGN = ref YMaxTick,

								YMaxGN = ref YMaxTick,

								XMIN = ref XNMin,

         						XMAX = ref XNMax,
 
								YMIN = ref YNMin,
 
								YMAX = ref YNMax,

								XStartMin = xmin,

								XStartMax = xmax,

								YStartMin = ymin,

								YStartMax = ymax,

								XStartCent = xcent,

								YStartCent = ycent,

         						Xn = ref xcent,
 
								Yn = ref ycent,

        	 					KX = ref (real (w) / real(XNMax - XNMin)),

         						KY = ref (real (h) / real(ymin - ymax)),

								Xo = ref (Xa),

         						Yo = ref (Ya),

         						Ido = 
									let val nod = 
										DSStr_CreateNode {page = pg, x = Xa, 
												  	  y = Ya, w = 1, h = 1, 
												  	  shape = 1}
                			  		in 
							  		(

                  						DSWtAttr_ObjectFlags {
											obj = nod, flag = 1, 
											value = true};

                  						DSStr_MakeNodeIntoRgn {
											obj = nod, 
											parent = AxeRgnId};

                  						ref nod
                  			   		)
                			  		end,

								AxTags = ref AxTags,
 
        						Legend = legend,

								PRX = ref x,

								PRY = ref y,

								PRW = ref w,

								PRH = ref h,

								GridVisible = gridvisible,

								Rescale = rescale,

								GridDyn = GridDyn,

								Title = title,
						
								Clear = Clear,
		
								InitToOrigin = InitToOrigin

								}: int LINECHART
							)
							end
						)
						end
          			)
       				end
				)
				end
			)
			end
		)
		end
	)
	end


	(* Initializes the chart to be empty and to have the initial layout *)

	fun clear_chart (L : int LINECHART) =
	(
		let val xmin = (#XStartMin L)
			and xmax = (#XStartMax L)
			and ymin = (#YStartMin L)
			and ymax = (#YStartMax L)
			and xcent = (#XStartCent L)
			and ycent = (#YStartCent L)
		in
		( 
			UpdatePlotSize(L);
			UpdatePlotPosition(L);

			let val XMinWidth = UT.round(real(!(#PRW L) * (xcent - xmin)) / 
					 	      			 real(xmax - xmin))
				and XMaxWidth = !(#PRW L) - 
								UT.round(real(!(#PRW L) * (xcent - xmin)) / 
										 real(xmax - xmin))
				and YMinWidth = UT.round(real(!(#PRH L) * (ycent - ymin)) / 
										 real(ymax - ymin))
				and YMaxWidth = !(#PRH L) - 
								UT.round(real(!(#PRH L) * (ycent - ymin)) / 
										 real(ymax - ymin))
				and XTagDist = if (#GridDyn L) then 
						 			UT.CalcDist((xmax - xmin), (#XIdealGN L))
					   	   	   else (#XDist L)
				and YTagDist = if (#GridDyn L) then 
									UT.CalcDist((ymax - ymin), (#YIdealGN L))
					   	   	   else (#YDist L)
			in
			(
				let val Xa = !(#PRX L) + 
						 	 UT.round(real(XMinWidth - XMaxWidth) / 2.0)
					and Ya = !(#PRY L) - 
							 UT.round(real(YMinWidth - YMaxWidth) / 2.0)
					and XMaxTick = 
						(if (#GridDyn L) then
							UT.CalcGN((xmax - xcent), 
									   UT.round(real((xmax - xcent) * 
													 (#XIdealGN L)) / 
												real(xmax - xmin)), 
									  XTagDist)
					 	else 
							UT.ceiling(real(xmax - xcent) / real(XTagDist)))
					and XMinTick = 
						(if (#GridDyn L) then
							UT.CalcGN((xcent - xmin), 
									  (#XIdealGN L) -
									   UT.round(
										real((xmax - xcent) * (#XIdealGN L)) / 
										real(xmax - xmin)), 
									  XTagDist)
					 	else 
							UT.ceiling(real(xcent - xmin) / real(XTagDist)))
					and YMaxTick = 
						(if (#GridDyn L) then
							UT.CalcGN((ymax - ycent), 
									  UT.round(
										real((ymax - ycent) * (#YIdealGN L)) / 
										real(ymax - ymin)), 
								     YTagDist)
						 else 
							UT.ceiling(real(ymax - ycent) / real(YTagDist)))
					and YMinTick = 
						(if (#GridDyn L) then
							UT.CalcGN((ycent - ymin),
									  (#YIdealGN L) -
									   UT.round(
										real((ymax - ycent) * (#YIdealGN L)) / 
										real(ymax - ymin)), 
									  YTagDist)
					 	else 
							UT.ceiling(real(ycent - ymin) / real(YTagDist)))
				in
				(
					let val XMaxTick = 
							if ((XMaxTick = 0) andalso (XMinTick = 0)) then 1
							else XMaxTick
						and YMaxTick = 
							if ((YMaxTick = 0) andalso (YMinTick = 0)) then 1
							else YMaxTick
					in
					(
						let val XNMin = xcent - (XTagDist * XMinTick)
							and XNMax = xcent + (XTagDist * XMaxTick)
							and YNMin = ycent - (YTagDist * YMinTick)
							and YNMax = ycent + (YTagDist * YMaxTick)
							and xngn = XMaxTick + XMinTick
							and yngn = YMaxTick + YMinTick
						in
						(
							LI.init_all(!(#DL L));
		
							DSWtAttr_ObjectPosition{obj = !(#XAxis L),
										 			x = !(#PRX L),
										 	    	y = Ya};
							DSWtAttr_ObjectPosition{obj = !(#YAxis L),
													x = Xa,
													y = !(#PRY L)};

							UT.DeleteIdList (!(#XGridList L));	
							(#XGridList L) := 
								create_Xgrids(
									!(#pg L), 
									if (#GridVisible L) then !(#AxeRgnId L)
									else !(#XAxis L), Xa, 
									if (#GridVisible L) then !(#PRY L) 
									else Ya, 
									!(#PRW L), 
									if (#GridVisible L) then !(#PRH L) else 4, 
									XNMax, XNMin, xcent, XMaxWidth, XMinWidth, 
									XMaxTick, XMinTick, xngn);

							UT.DeleteIdList (!(#YGridList L));
							(#YGridList L) := 
								create_Ygrids(
									!(#pg L), 
									if (#GridVisible L) then !(#AxeRgnId L)
									else !(#YAxis L), 
									if (#GridVisible L) then !(#PRX L) 
									else Xa, Ya, 
									if (#GridVisible L) then !(#PRW L) else 4, 
									!(#PRH L), YNMax, YNMin, ycent, YMaxWidth, 
									YMinWidth, YMaxTick, YMinTick, yngn);

							UT.DeleteIdList (!(#XTag L));
							(#XTag L) := 
								LabelAxesX (
									!(#pg L), 
									if (#GridVisible L) then !(#AxeRgnId L)
									else !(#XAxis L), Xa, 
									if (#GridVisible L) then
										(!(#PRY L) + (!(#PRH L) div 2) + 10)
									else (Ya + 10), 
									(!(#PRW L) div xngn), XNMin, XMaxWidth, 
									XMinWidth, XMaxTick, XMinTick, xngn, 
									XTagDist, (#GridVisible L));

							UT.DeleteIdList (!(#YTag L));
							(#YTag L) := 
								LabelAxesY (
									!(#pg L), 
									if (#GridVisible L) then !(#AxeRgnId L)
									else !(#YAxis L), 
									if (#GridVisible L) then
										(!(#PRX L) + (!(#PRW L) div 2) + 10)
									else (Xa + 10), 
									Ya, (!(#PRH L) div yngn), YNMin, 
									YMaxWidth, YMinWidth, YMaxTick, YMinTick, 
									yngn, YTagDist, (#GridVisible L));

							(#XMinGN L) := XMinTick;
							(#XMaxGN L) := XMaxTick;
							(#YMinGN L) := YMinTick;
							(#YMaxGN L) := YMaxTick;
							(#XMIN L) := XNMin;
							(#XMAX L) := XNMax;
							(#YMIN L) := YNMin;
							(#YMAX L) := YNMax;
							(#Xn L) := xcent;
							(#Yn L) := ycent;
							(#KX L) := (real(!(#PRW L)) / real(XNMax - XNMin));
							(#KY L) := (real(!(#PRH L)) / real(YNMax - YNMin));

							DSWtAttr_ObjectPosition{obj = !(#Ido L),
													x = Xa,
													y = Ya};

							L
						)
						end
					)
					end
				)
				end
			)
			end
		)
		end
	)


	(* Resizes existing line segments to fit updated graph *)

	fun ResizeXLines(XMIN, XMAX, L : int LINECHART) =
       		let val nod = DSStr_CreateNode {page = !(#pg L),
                      x = Xold((!(#XMAX L) + !(#XMIN L)) div 2, L), 
                      y = #y (DSRdAttr_GetObjectCenter (!(#Ido L))), 
                      w = abs (floor(!(#KX L) * 
							   real(!(#XMAX L) - !(#XMIN L)))), 
                      h = 1, shape = 1}
       		in
       		(
       			LI.MakeLineRgn(!(#DL L), nod);
       			DSWtAttr_AdjustObjectSize {obj = nod,
                      					   w = floor(!(#KX L) * 
													 real(XMAX - XMIN)),
                      					   h = 1};
       			DSWtAttr_ObjectPosition {obj = nod,
                      					 x = Xold((XMAX + XMIN) div 2, L),
                      					 y = #y (
											DSRdAttr_GetObjectCenter (nod))};
       			LI.MakeLineRgn(!(#DL L), !(#AxeRgnId L));
       			DSStr_DeleteObject(nod)
       		)
       		end


	(* Change the x-axis-range, it needed *)

  	fun ResizeX(minx, maxx, L : int LINECHART) = 
	(
		if (#Rescale L) then 
		(
      		if ((maxx > !(#XMAX L)) orelse (minx < !(#XMIN L))) then 
			(
				let val NXMax = if maxx > !(#XMAX L) then
									UT.round(real(maxx) / ReducPerc)
								else 
									!(#XMAX L)
					and NXMin = if minx < !(#XMIN L) then
									UT.round(real(minx) / ReducPerc)
								else !(#XMIN L)
				in
				(
           			let val space = (if (#GridDyn L) then
										UT.CalcDist((NXMax - NXMin), 
											 	 (#XIdealGN L))
									 else
										(#XDist L))
					in
					(
						let val NMaxTick = (
									if (#GridDyn L) then
										UT.CalcGN(
											(NXMax - !(#Xn L)),
											UT.round(
												real((NXMax - !(#Xn L)) * 
													 ((#XIdealGN L))) / 
												real(NXMax - NXMin)), 
											space)
									else (* Fixed Dist *)
										UT.ceiling(real(NXMax - !(#Xn L)) / 
												   real(space)))
							and NMinTick = (
									if (#GridDyn L) then
										UT.CalcGN(
											(!(#Xn L) - NXMin), 
											(#XIdealGN L) - 
											UT.round
											   (real((NXMax - !(#Xn L)) * 
													 ((#XIdealGN L))) /
 												real(NXMax - NXMin)), 
											space)
									else (* Fixed Dist *)
										UT.round(real(!(#Xn L) - NXMin) / 
		                                         real(space)))
           				in
						(
             				let val NMaxTick =
								if ((NMinTick = 0) andalso (NMaxTick = 0))
								then 1
								else NMaxTick
							in
							(
								let val NXMAX = !(#Xn L) + (space * NMaxTick)
             						and NXMIN = !(#Xn L) - (space * NMinTick)
									and ngn = NMinTick + NMaxTick
									and NMaxWidth = 
											UT.round(
												real(NMaxTick * !(#PRW L)) / 
												real(NMinTick + NMaxTick))
									and NMinWidth =
											!(#PRW L) -
											UT.round(
												real(NMaxTick * !(#PRW L)) / 
												real(NMinTick + NMaxTick))
               					in
								(
									let val Xa = 
											!(#PRX L) + UT.round(real(
											NMinWidth - NMaxWidth) / 2.0)
									in
									(
										if ((NMaxTick <> (!(#XMaxGN L))) 
											orelse 
											(NMinTick <> (!(#XMinGN L)))) 
										then
										(
											DSWtAttr_ObjectPosition{
												obj = !(#YAxis L),
												x = Xa,
												y = !(#PRY L)};

											UT.DeleteIdList(!(#XTag L));
											#XTag L := 
												LabelAxesX(
													!(#pg L), 
													if (#GridVisible L) then
														!(#AxeRgnId L)
													else !(#XAxis L), Xa, 
													if (#GridVisible L) then
														(!(#PRY L) + 
														(!(#PRH L) div 2) + 10)
													else (!(#Yo L) + 10),
													(!(#PRW L) div ngn), 
													NXMIN, NMaxWidth,
													NMinWidth, NMaxTick, 
													NMinTick, ngn, space, 
													(#GridVisible L));
											
											UT.DeleteIdList(!(#XGridList L));
											#XGridList L := 
												create_Xgrids(
													!(#pg L), 
													if (#GridVisible L) then
														!(#AxeRgnId L)
													else !(#XAxis L), Xa, 
													if (#GridVisible L) then 
														!(#PRY L) 
													else !(#Yo L), 
													!(#PRW L), 
													if (#GridVisible L) then 
														!(#PRH L) 
													else 4, 
													NXMAX, NXMIN, !(#Xn L),
													NMaxWidth, 
													NMinWidth, NMaxTick, 
													NMinTick, ngn)
										)
										else
										(
											UT.UpdateTag'i (
												!(#XTag L),
                                 				UT.CreateList (
													NXMIN, space, ngn + 1))
										);

                 						let val XMAX = !(#XMAX L) 
											and XMIN = !(#XMIN L)
                 						in
                   						(
                   							#XMAX L := NXMAX;
                   							#XMIN L := NXMIN;
											#XMinGN L := NMinTick;
											#XMaxGN L := NMaxTick;
                   							#KX L := (
												real (!(#PRW L)) / 
									       		real(!(#XMAX L) - !(#XMIN L)));
											#Xo L := Xa;
											DSWtAttr_ObjectPosition{
												obj = !(#Ido L), 
												x = Xa, 
												y = !(#Yo L)};

											ResizeXLines(XMIN, XMAX, L)
										)
                 						end (* XMAX, XMIN *)
									)
									end (* Xa *)
								)
								end (* NMaxTick *)
                 			)
               				end (* NXMAX, NXMIN *)
						)
             			end (* NMaxTick, NMinTick *)
					)
           			end (* space *)
				)
         		end (* NXMax, NXMin *)
			)
      		else () (* x < !(#XMAX L) & x > !(#XMIN L) *)
		)
		else (* Move Axes *)
		(
			if (maxx > !(#XMAX L)) then
			(
				let val NXMax = floor(real(maxx) / ReducPerc)
					and space = (if (!(#XMaxGN L) > 0) then 
									(!(#XMAX L) - !(#Xn L)) div !(#XMaxGN L)
								 else if (!(#XMinGN L) > 0) then
									abs((!(#XMIN L) + !(#Xn L)) div 
										!(#XMinGN L))
								 else 1)
				in
				(
					let val MDist = space * 
									UT.round(real(NXMax - !(#XMAX L)) / 
											 real(space))
					in
					(
						let val Move = UT.round(real(!(#PRW L) * MDist) /
												real(!(#XMAX L) - 
													(!(#XMIN L))))
							and XMinTick = !(#XMinGN L)
							and XMaxTick = !(#XMaxGN L)
						in
						(
							let val MinPos = 
								!(#PRX L) - 
								UT.round(real(!(#PRW L)) / 2.0) + Move
							in
								LI.DelXSmaller(!(#DL L), MinPos, 
											   !(#AxeRgnId L))
							end;

							let val nod = DSStr_CreateNode{
												page = !(#pg L),
												x = !(#PRX L),
												y = !(#PRY L),
												w = !(#PRW L),
												h = 1,
												shape = 1}
							in
							(
								LI.MakeLineRgn(!(#DL L), nod);

								DSWtAttr_ObjectPosition{
											obj = nod,
											x = !(#PRX L) - Move,
											y = !(#PRY L)};

								LI.MakeLineRgn(!(#DL L), !(#AxeRgnId L));

								DSStr_DeleteObject(nod)
							)
							end; (* nod *)
							
							let val NXMIN = !(#XMIN L) + MDist	
								and NXMAX = !(#XMAX L) + MDist
							in
							(	
                 				UT.UpdateTag'i (
									!(#XTag L),
                                 	UT.CreateList (
										NXMIN, space, 
										(XMaxTick + XMinTick + 1)));
                 				
								#XMAX L := NXMAX;
                   				#XMIN L := NXMIN;
								#Xn L := !(#Xn L) + MDist;
                   				#KX L := real (!(#PRW L)) / 
										 real(!(#XMAX L) - !(#XMIN L))
                 			)
							end (* NXMIN, NXMAX *)
						)
						end (* Move, XMinTick, XMaxTick *)
					)
					end (* MDist *)
				)
				end (* NXMax *)
			)
			else (* x <= !(#XMAX L) *)
			(
				if (minx < !(#XMIN L)) then
				(
					let val NXMax = !(#XMAX L)
						and NXMin = floor(real(minx) / ReducPerc)
						and space = 
								(if (!(#XMaxGN L) > 0) then 
									(!(#XMAX L) - !(#Xn L)) div !(#XMaxGN L)
								 else if (!(#XMinGN L) > 0) then
									abs((!(#XMIN L) + !(#Xn L)) div 
										!(#XMinGN L))
								 else 1)
					in
					(
						let val MDist = 
							space * 
							UT.round(real(!(#XMIN L) - NXMin) / real(space))
						in
						(
							let val Move = UT.round(real(!(#PRW L) * MDist) /
													real(!(#XMAX L) -
													 	(!(#XMIN L))))
								and XMinTick = !(#XMinGN L)
								and XMaxTick = !(#XMaxGN L)
							in
							(
								let val MaxPos =
									!(#PRX L) + (!(#PRW L) div 2) + Move
								in
									LI.DelXLarger(!(#DL L), MaxPos, 
											      !(#AxeRgnId L))
								end;

								let val nod = DSStr_CreateNode{
												page = !(#pg L),
												x = !(#PRX L),
												y = !(#PRY L),
												w = !(#PRW L),
												h = 1,
												shape = 1}
								in
								(
									LI.MakeLineRgn(!(#DL L), nod);

									DSWtAttr_ObjectPosition{
											obj = nod,
											x = !(#PRX L) + Move,
											y = !(#PRY L)};

									LI.MakeLineRgn(!(#DL L), !(#AxeRgnId L));

									DSStr_DeleteObject(nod)
								)
								end;
							
								let val NXMIN = !(#XMIN L) - MDist	
									and NXMAX = !(#XMAX L) - MDist
								in
								(
                 						UT.UpdateTag'i (
											!(#XTag L),
                                 			UT.CreateList (
												NXMIN, space, 
												(XMaxTick + XMaxTick + 1)));
                 						
									let val XMIN = !(#XMIN L) 
										and XMAX = !(#XMAX L)
                 					in
                   					(
                   						#XMAX L := NXMAX;
                   						#XMIN L := NXMIN;
										#Xn L := !(#Xn L) - MDist;
                   						#KX L := real (!(#PRW L)) / 
										 	 	 real(!(#XMAX L) - !(#XMIN L))
                   					)
                 					end (* XMIN, XMAX *)
								)
               					end (* NXMIN, NXMAX *)
							)
							end (* Move, XMinTick, XMaxTick *)
						) 
						end (* MDist *)
					)
					end (* NXMax, NXMin *)
				)
				else () (* x > !(#XMIN L) *)
			) 
		) 
	) 


	(* Resizes existing line segments to fit updated graph *)

   	fun ResizeYLines(YMIN, YMAX, L : int LINECHART) =
       		let val nod = 
				DSStr_CreateNode {page = !(#pg L),
                      			  x = #x (DSRdAttr_GetObjectCenter 
											(!(#Ido L))), 
                      			  y = Yold((!(#YMAX L) + !(#YMIN L)) div 2, L),
					 			  w = 1, 
                      			  h = abs(floor(!(#KY L) * real (!(#YMAX L) - 
											!(#YMIN L)))),
                      			  shape = 1}
       		in
       		(
       			LI.MakeLineRgn(!(#DL L), nod);
       			DSWtAttr_AdjustObjectSize {obj = nod, w = 1,
                      					   h = abs(floor(!(#KY L) * real 
													 (YMAX - YMIN)))};
       			DSWtAttr_ObjectPosition {obj = nod,
                      					 x = #x (DSRdAttr_GetObjectCenter 
												  (nod)),
                      					 y = Yold((YMAX + YMIN) div 2, L)};
       			LI.MakeLineRgn(!(#DL L), !(#AxeRgnId L));
       			DSStr_DeleteObject(nod)
       		)
       		end


	(* Change the y-axis-range, if needed *)

  	fun ResizeY(miny, maxy, L : int LINECHART) = 
	(
		if (#Rescale L) then 
		(
      		if ((maxy > !(#YMAX L)) orelse (miny < !(#YMIN L))) then 
			(
				let val NYMax = if maxy > !(#YMAX L) then
									UT.round(real(maxy) / ReducPerc)
								else 
									!(#YMAX L)
					and NYMin = if miny < !(#YMIN L) then
									UT.round(real(miny) / ReducPerc)
								else !(#YMIN L)
				in
				(
           			let val space = (if (#GridDyn L) then
										UT.CalcDist((NYMax - NYMin), 
											 	 (#YIdealGN L))
									 else
										(#YDist L))
					in
					(
						let val NMaxTick = (
									if (#GridDyn L) then
										UT.CalcGN(
											(NYMax - !(#Yn L)),
											UT.round(
												real((NYMax - !(#Yn L)) * 
													 ((#YIdealGN L))) / 
												real(NYMax - NYMin)), 
											space)
									else (* Fixed Dist *)
										UT.ceiling(real(NYMax - !(#Yn L)) / 
												   real(space)))
							and NMinTick = (
									if (#GridDyn L) then
										UT.CalcGN(
											(!(#Yn L) - NYMin), 
											(#YIdealGN L) - 
											UT.round
											   (real((NYMax - !(#Yn L)) * 
													 ((#YIdealGN L))) /
 												real(NYMax - NYMin)), 
											space)
									else (* Fixed Dist *)
										UT.round(real(!(#Yn L) - NYMin) / 
												 real(space)))
           				in
						(
             				let val NMaxTick =
								if ((NMinTick = 0) andalso (NMaxTick = 0))
								then 1
								else NMaxTick
							in
							(
								let val NYMAX = !(#Yn L) + (space * NMaxTick)
             						and NYMIN = !(#Yn L) - (space * NMinTick)
									and ngn = NMinTick + NMaxTick
									and NMaxWidth = 
											UT.round(
												real(NMaxTick * !(#PRH L)) / 
												real(NMinTick + NMaxTick))
									and NMinWidth =
											!(#PRH L) -
											UT.round(
												real(NMaxTick * !(#PRH L)) / 
												real(NMinTick + NMaxTick))
               					in
								(
									let val Ya = 
											!(#PRY L) - UT.round(real(
											NMinWidth - NMaxWidth) / 2.0)
									in
									(
										if ((NMaxTick <> (!(#YMaxGN L))) 
											orelse 
											(NMinTick <> (!(#YMinGN L)))) 
										then
										(
											DSWtAttr_ObjectPosition{
												obj = !(#XAxis L),
												x = !(#PRX L),
												y = Ya};

											UT.DeleteIdList(!(#YTag L));
											#YTag L := 
												LabelAxesY(
													!(#pg L), 
													if (#GridVisible L) then
														!(#AxeRgnId L)
													else !(#YAxis L), 
													if (#GridVisible L) then
														(!(#PRX L) + 
														(!(#PRW L) div 2) + 10)
													else (!(#Xo L) + 10), 
													Ya,
													(!(#PRH L) div ngn), 
													NYMIN, NMaxWidth,
													NMinWidth, NMaxTick, 
													NMinTick, ngn, space, 
													(#GridVisible L));
											
											UT.DeleteIdList(!(#YGridList L));
											#YGridList L := 
												create_Ygrids(
													!(#pg L), 
													if (#GridVisible L) then
														!(#AxeRgnId L)
													else !(#YAxis L), 
													if (#GridVisible L) then 
														!(#PRX L) 
													else !(#Xo L), Ya, 
													if (#GridVisible L) then
														!(#PRW L)
													else 4, 
													!(#PRH L), 
													NYMAX, NYMIN, !(#Yn L),
													NMaxWidth, 
													NMinWidth, NMaxTick, 
													NMinTick, ngn)
										)
										else
										(
											UT.UpdateTag'i (
												!(#YTag L),
                                 				UT.CreateList (
													NYMIN, space, ngn + 1))
										);

                 						let val YMAX = !(#YMAX L) 
											and YMIN = !(#YMIN L)
                 						in
                   						(
                   							#YMAX L := NYMAX;
                   							#YMIN L := NYMIN;
											#YMinGN L := NMinTick;
											#YMaxGN L := NMaxTick;
                   							#KY L := (
												real (!(#PRH L)) / 
									       		real(!(#YMIN L) - !(#YMAX L)));
											#Yo L := Ya;
											DSWtAttr_ObjectPosition{
												obj = !(#Ido L), 
												x = !(#Xo L), 
												y = Ya};

											ResizeYLines(YMIN, YMAX, L)
										)
                 						end (* YMAX, YMIN *)
									)
									end (* Ya *)
								)
								end (* NMaxTick *)
                 			)
               				end (* NYMAX, NYMIN *)
						)
             			end (* NMaxTick, NMinTick *)
					)
           			end (* space *)
				)
         		end (* NYMax, NYMin *)
			)
      		else () (* y < !(#YMAX L) & y > !(#YMIN L) *)
		)
		else (* Move Axes *)
		(
			if (maxy > !(#YMAX L)) then
			(
				let val NYMax = floor(real(maxy) / ReducPerc)
					and space = (if (!(#YMaxGN L) > 0) then 
									(!(#YMAX L) - !(#Yn L)) div !(#YMaxGN L)
								 else if (!(#YMinGN L) > 0) then
									abs((!(#YMIN L) + !(#Yn L)) div 
										!(#YMinGN L))
								 else 1)
				in
				(
					let val MDist = space *
									UT.round(real(NYMax - !(#YMAX L)) /
											 real(space))
					in
					(
						let val Move = UT.round(real(!(#PRH L) * MDist) /
												real(!(#YMAX L) -
													 (!(#YMIN L))))
							and YMinTick = !(#YMinGN L)
							and YMaxTick = !(#YMaxGN L)
						in
						(
							let val MinPos = 
								!(#PRY L) + 
								UT.round(real(!(#PRH L)) / 2.0) - Move
							in
								LI.DelYSmaller(!(#DL L), MinPos, 
											   !(#AxeRgnId L))
							end;

							let val nod = DSStr_CreateNode{
												page = !(#pg L),
												x = !(#PRX L),
												y = !(#PRY L),
												w = 1,
												h = !(#PRH L),
												shape = 1}
							in
							(
								LI.MakeLineRgn(!(#DL L), nod);

								DSWtAttr_ObjectPosition{
											obj = nod,
											x = !(#PRX L),
											y = !(#PRY L) + Move};

								LI.MakeLineRgn(!(#DL L), !(#AxeRgnId L));

								DSStr_DeleteObject(nod)
							)
							end; (* nod *)
							
							let val NYMIN = !(#YMIN L) + MDist	
								and NYMAX = !(#YMAX L) + MDist
							in
							(	
                 				UT.UpdateTag'i (
									!(#YTag L),
                                 	UT.CreateList (
										NYMIN, space, 
										(YMaxTick + YMinTick + 1)));
                 				
								#YMAX L := NYMAX;
                   				#YMIN L := NYMIN;
								#Yn L := !(#Yn L) + MDist;
                   				#KY L := real (!(#PRH L)) / 
										 real(!(#YMIN L) - !(#YMAX L))
                 			)
							end (* NYMIN, NYMAX *)
						)
						end (* Move, YMinTick, YMaxTick *)
					)
					end (* MDist *)
				)
				end (* NYMax *)
			)
			else (* y <= !(#YMAX L) *)
			(
				if (miny < !(#YMIN L)) then
				(
					let val NYMax = !(#YMAX L)
						and NYMin = floor(real(miny) / ReducPerc)
					in
					(
						let val MDist = !(#YMIN L) - NYMin
						in
						(
							let val Move = UT.round(real(!(#PRH L) * MDist) /
													real(!(#YMAX L) - 
													 	(!(#YMIN L))))
								and YMinTick = !(#YMinGN L)
								and YMaxTick = !(#YMaxGN L)
							in
							(
								let val MaxPos = 
									!(#PRX L) - (!(#PRH L) div 2) + Move
								in
									LI.DelYLarger(!(#DL L), MaxPos, 
											      !(#AxeRgnId L))
								end;

								let val nod = DSStr_CreateNode{
												page = !(#pg L),
												x = !(#PRX L),
												y = !(#PRY L),
												w = 1,
												h = !(#PRH L),
												shape = 1}
								in
								(
									LI.MakeLineRgn(!(#DL L), nod);

									DSWtAttr_ObjectPosition{
											obj = nod,
											x = !(#PRX L),
											y = !(#PRY L) + Move};

									LI.MakeLineRgn(!(#DL L), !(#AxeRgnId L));

									DSStr_DeleteObject(nod)
								)
								end;
							
								let val NYMIN = !(#YMIN L) - MDist	
									and NYMAX = !(#YMIN L) - MDist
								in
								(
                 					let val space = 
										(if YMaxTick <> 0 then
                                      		((NYMAX - !(#Yn L)) div YMaxTick)
                                      	 else 
											if YMinTick <> 0 then
												((NYMIN + !(#Yn L)) div 
												YMinTick)
											else 1)
                 					in
									(
                 						UT.UpdateTag'i (
											!(#YTag L),
                                 			UT.CreateList (
												NYMIN, space, 
												(YMaxTick + YMaxTick + 1)))
									)
									end;
                 						
									let val YMIN = !(#YMIN L) 
										and YMAX = !(#YMAX L)
                 					in
                   					(
                   						#YMAX L := NYMAX;
                   						#YMIN L := NYMIN;
										#Yn L := !(#Yn L) - MDist;
                   						#KY L := real (!(#PRH L)) / 
										 	 	 real(!(#YMIN L) - !(#YMAX L))
                   					)
                 					end (* YMIN, YMAX *)
								)
               					end (* NYMIN, NYMAX *)
							)
							end (* Move, YMinTick, YMaxTick *)
						) 
						end (* MDist *)
					)
					end (* NYMax, NYMin *)
				)
				else () (* y > !(#YMIN L) *)
			) 
		) 
	) 


	(* Create new line segment *)

  	fun updXY_line(n, X, Y, L : int LINECHART, pattern, pat, drawline) =
    		let val nline = UT.NthElement(n, !(#DL L))
     		in 
			(
       			if LI.same (nline, LI.EmptyLine)
       			then  
					let val InitNod = LI.CreateLine(!(#pg L), X, Y,
													#BoxSize L,
													if pattern then pat
                                               		else UT.NthPattern(n),
                                               		1, 
                                               		#TIMING L,
													#LINETYPE L,
													#THICK L)
              		in
              		(
               			DSStr_MakeNodeIntoRgn {obj = !(#FstId InitNod), 
											   parent = !(#AxeRgnId L)};
               			#DL L := UT.Insert(n, !(#DL L),InitNod)
              		)
              		end
       			else 
				(
					#DL L := UT.Insert(
								n, !(#DL L),
								if pattern then 
									LI.draw_cline(nline, X, Y, pat, 
												  !(#FstId nline), drawline)
                          		else 
									LI.draw_line(nline, X, Y, 
												 !(#FstId nline), drawline))
				); 
       			L
			)
     		end



	fun UpdateLine(nil, pattern, pat, L) = ()
	 |	UpdateLine((n, x, y, drawline)::vl, pattern, pat, L) =
	(
		let val X = Xold(x, L)
			and Y = Yold(y, L)
		in
			updXY_line(n, X, Y, L, pattern, pat, drawline)
		end;

		UpdateLine(vl, pattern, pat, L)
	)


	(* 
		Updates the line L with a new segment. The new connector box will
		be positioned at (x, y) 
	*)

  	fun upd_line(n, x, y, drawline, L : int LINECHART) =
    	(
    		UpdatePlotSize(L);
			UpdatePlotPosition(L);

    		ResizeX(x, x, L);
    		ResizeY(y, y, L);

			UpdateLine([(n, x, y, drawline)], false, 1, L);

    		L
    	)

	
	(* Change the pattern of the line L and updates it *)

  	fun upd_pline (n, x, y, drawline, L : int LINECHART, pat) =
    	(
    		UpdatePlotSize(L);
			UpdatePlotPosition(L);

    		ResizeX(x, x, L);
    		ResizeY(y, y, L);

			UpdateLine([(n, x, y, drawline)], true, pat, L);

    		L
    	)


	fun findtrues (nil) = nil
	 |	findtrues ((ln, x, y, drawline, cond)::vl) =
	(
		if cond then 
			(ln, x, y, drawline)::findtrues(vl)
		else
			findtrues(vl)
	)

	fun hfindxmin (nil, xmin) = xmin
	 |	hfindxmin ((ln, x, y, drawline)::vl, xmin: int) =
	(
		if x < xmin then
			hfindxmin(vl, x)
		else
			hfindxmin(vl, xmin)
	)

	fun findxmin (nil) = 0
	 |	findxmin ((ln, x, y, drawline)::vl) =
	(
		hfindxmin (vl, x)
	)

	fun hfindymin (nil, ymin) = ymin
	 |	hfindymin ((ln, x, y, drawline)::vl, ymin: int) =
	(
		if y < ymin then
			hfindymin(vl, y)
		else
			hfindymin(vl, ymin)
	)
	
	fun findymin (nil) = 0
	 |	findymin ((ln, x, y, drawline)::vl) =
	(
		hfindymin(vl, y)
	)

	fun hfindxmax (nil, xmax) = xmax
	 |	hfindxmax ((ln, x, y, drawline)::vl, xmax: int) =
	(
		if x > xmax then
			hfindxmax(vl, x)
		else
			hfindxmax(vl, xmax)
	)

	fun findxmax (nil) = 0
	 |	findxmax ((ln, x, y, drawline)::vl) =
	(
		hfindxmax(vl, x)
	)

	fun hfindymax (nil, ymax) = ymax
	 |	hfindymax ((ln, x, y, drawline)::vl, ymax: int) =
	(
		if y > ymax then
			hfindymax(vl, y)
		else
			hfindymax(vl, ymax)
	)

	fun findymax (nil) = 0
	 | 	findymax ((ln, x, y, drawline)::vl) =
	(
		hfindymax (vl, y)
	)

	fun upd_chart(L : int LINECHART, vl) =
	(
		UpdatePlotSize(L);
		UpdatePlotPosition(L);

		let val nvalues = findtrues(vl)
		in
		(
			if nvalues <> nil then
			(
				let val xmin = findxmin(nvalues)
					and ymin = findymin(nvalues)
					and xmax = findxmax(nvalues)
					and ymax = findymax(nvalues)
				in
				(
					ResizeX(xmin, xmax, L);
					ResizeY(ymin, ymax, L)
				)
				end;

				UpdateLine(nvalues, false, 1, L)
			)
			else ()
		)
		end;

		L
	)

 	local val CNX = 0 (* Chart Node X and Y coordonates *)
		  and CNY = 0
          and graph1 = ref EmptyLinechart
          and legend1 = ref LE.EmptyLegend
	in 

(* 
	nline : number of lines
	boxsize : size of the boxes part of the line 
	timing: true if time-series graph
	gridvisible: true if ticks have the size of the graph, the graph looks 
			  like a matrix
	x,y,w,h: coordinates and size of the line graph box surrounding the graph
	xmin,xmax,ymin,ymax: value of the minimum and maximum values in the 
						 x and y coordinates
	xcent,ycent: value of the center
	XMinTick,XMaxTick,YMinTick,YMaxTick: number of ticks on the x and y 
										 coordinates
	Legend: true if legend region displayed
*)

	fun create {title = title, page = pg, nline = nline, boxsize = boxsize,
				timing = timing, gridvisible = gridvisible,
				x = x, y = y, w = w, h = h, axis = axis,  
				xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
				xcent = xcent, ycent = ycent, thick = thick, 
				linetype = linetype, Legend = Legend,
				Rescale = Rescale, GridDyn = GridDyn, XIdealGN = XIdealGN,
				YIdealGN = YIdealGN, XDist = XDist, YDist = YDist,
				Clear = Clear, InitToOrigin = InitToOrigin, 
				StartAtOrigin = StartAtOrigin}=
			let val LRX = x + (w * 3 div 8)
        		and LRW = (20 * w) div 100
               	and PRX = (	if Legend (*moves x-coord (9/80 * w) *)
							then 
								x - UT.round(real(9 * w) / 80.0) 
                           else 
								x  (*- ((5 * w) div 100)*) )
               	and PRW = (if Legend then (52 * w) div 100 
                           else (67 * w) div 100)
              	and PRH = ((70 * h) div 100)
				and PRY = (if title then
								y + (h div 50)
						   else
								y)
               	and PatternSize = 10
				and graph1 = ref EmptyLinechart
				and legend1 = ref LE.EmptyLegend
				and title1 = if title then
								ref(LC.CreateTitle(pg, x, y, h))
							 else ref 0
				and CNI = DSStr_CreateNode{page = pg, x = x, y = y, 
										   	   w = w, h = h, shape = 1} 
			in 
			( 
				if Legend 
				then  
				(
					legend1 := LE.CreateLegendRegion (pg, LRX, y, LRW, 
													  PatternSize, nline);

					LE.upd_rtag (
						(!legend1), 
						LC.CreateIndexStrList(
							"pname", 1, 
							(length(#PatternList (!legend1)) + 1)));

					DSStr_MakeNodeIntoRgn { obj = ! (# Id (!legend1)), 
											parent = CNI }
             	)
               	else ();

				DSWtAttr_LineThickness { obj = CNI, thick = 2 };

				DSWtAttr_LineType { obj = CNI, line = 5 };


         		graph1 := create_graph (CNI, pg, nline, boxsize,
										timing, linetype, thick, gridvisible,
										axis, PRX, PRY, PRW, PRH, 
										xmin, xmax, ymin, ymax, xcent, 
										ycent, legend1, Rescale, GridDyn,
										XIdealGN, YIdealGN, XDist, YDist,
										title1, Clear, InitToOrigin,
										StartAtOrigin);

				DSStr_MakeNodeIntoRgn { obj = !title1, parent = CNI };

				(!graph1)
       		)
      		end

	fun LC'create { Legend = Legend, AxisName = axis,  
					TicksOnly = TicksOnly, BoxSize = boxsize, 
					height = h, NoOfLines = nline, 
					LineThick = thick, HorizVert = timing, Title = title, 
					width = w, x = x, XOrigin = xcent, XMax = xmax, 
					XMin = xmin, y = y, YOrigin = ycent, YMax = ymax, 
					YMin = ymin, RescaleAxis = Rescale,
					FixedNo = GridDyn, XIdealGN = XIdealGN, 
					YIdealGN = YIdealGN, XDist = XDist, YDist = YDist,
					ClearChart = Clear, InitToOrigin = InitToOrigin,
					StartAtOrigin = StartAtOrigin } =

			create {title = title, page = DSStr_GetCurPage(),
					nline = nline, boxsize = boxsize, timing = timing,
					gridvisible = not(TicksOnly),
					x = x, y = y, w = w, h = h, axis = axis, 
					xmin = xmin, xmax = xmax, ymin = ymin, 
					ymax = ymax, xcent = xcent, ycent = ycent, 
					thick = thick, linetype = 5,
					Legend = Legend, Rescale = Rescale, GridDyn = GridDyn,
					XIdealGN = XIdealGN, YIdealGN = YIdealGN, XDist = XDist,
					YDist = YDist, Clear = Clear, InitToOrigin = InitToOrigin,
					StartAtOrigin = StartAtOrigin}


	fun LC'clear (graph: int LINECHART ref) =
			(graph := clear_chart(!graph))


	fun CreateValueList (x, y, drawline, cond, 0) = []
	 |	CreateValueList (x: int, y: int, drawline: bool, cond: bool, n) =
	(
		(n, x, y, drawline, cond)::
			CreateValueList (x, y, drawline, cond, (n-1))
	)
	
	fun LC'upd_line { name = graph: int LINECHART ref, line = n, 
					  x = x, y = y, drawline = drawline } = 
			(graph := upd_line (n, x, y, drawline, !graph))

	fun LC'upd_chart { name = graph: int LINECHART ref, values = values } =
			(graph := upd_chart(!graph, values))

	fun LC'init (graph: int LINECHART ref ) =
	(
		if (#Clear (!graph)) then
			LC'clear ( graph )
		else
		(
			if (#InitToOrigin (!graph)) then
				LC'upd_chart { name = graph, 
							   values = CreateValueList (
											!(#Xn (!graph)), !(#Yn (!graph)),
											false, true, 
											length(!(#DL (!graph)))) }
			else ()
		)
	) 

	fun LC'upd_pline { name = graph: int LINECHART ref, line = n, x = x, 
					   y = y, drawline = drawline, pattern = pat } = 
			(graph := upd_pline (n, x, y, drawline, !graph, pat))

	fun LC'upd_ltag { name = graph: int LINECHART ref, tags = l } = 
			LE.upd_rtag (!(#Legend (!graph)), l)

	fun LC'upd_atag { name = graph: int LINECHART ref, x = x, y = y } = 
			upd_atag ([x,y], !graph)

	fun LC'upd_title { name = graph: int LINECHART ref, title = title} =
			upd_title(!graph, title)

	fun LC'delete ( name : int LINECHART ref ) = 
			DSStr_DeleteObject (!(# AxeRgnId (!name)))
end
end;


(*
	Contains functions to create and update real line charts
*)

structure RLC =
	struct
  	structure UT = UtilStr;
	structure RUT = RealUtilStr;
  	structure LI = LineStr;
  	structure LE = LegendStr;

	(*
		Signature
		sig
    		val EmptyLinechart : real LINECHART
    		val LC'clear : real LINECHART ref -> unit
    		val LC'create : {
					FixedNo:bool,
					Rescale:bool,
					XDist:real,
					XIdealGN:int,
					YDist:real,
					YIdealGN:int,
					AxisName:bool,
					BoxSize:int,
					TicksOnly:bool,
					height:int,
					Legend:bool,
					NoOfLines:int,
					LineThick:int,
					Horiz/Vert:bool,
					Title:bool,
					ClearChart:bool, 
					InitToOrigin:bool,
					StartAtOrigin:bool,
					width:int,
					x:int,
					XOrigin:real,
					XMax:real,
					XMin:real,
					y:int,
					YOrigin:real,
					YMax:real,
					YMin:real} -> real LINECHART
    		val LC'delete : real LINECHART ref -> unit
    		val LC'upd_atag : {name:real LINECHART ref,x:string,y:string} -> unit
    		val LC'upd_line : {drawline:bool,name:real LINECHART ref,
								line:int,x:real,y:real} -> unit
    		val LC'upd_ltag : {name:real LINECHART ref,tags:string list} -> unit
    		val LC'upd_chart : {name :int LINECHART ref,
							    values = (int * int * int * bool * bool) list}
								-> unit
    		val LC'upd_pline : {drawline:bool,name:real LINECHART ref,
								line:int,pattern:int,x:real,y:real} -> unit
    		val LC'upd_title : {name:real LINECHART ref,title:string} -> unit
    		val LabelAxesX : int * int * int * int * int * real * 'a * 'b *	
							 int * int * int * real * bool -> int list
    		val LabelAxesY : int * int * int * int * int * real * 'a * 'b * 
							 int * int * int * real * bool -> int list
    		val ReducPerc : real
    		val ResizeX : real * real LINECHART -> unit
    		val ResizeXLines : real * real * real LINECHART -> unit
    		val ResizeY : real * real LINECHART -> unit
    		val ResizeYLines : real * real * real LINECHART -> unit
    		val UpdatePlotPosition : real LINECHART -> unit
    		val UpdatePlotSize : real LINECHART -> unit
    		val Xold : real * real LINECHART -> int
    		val Yold : real * real LINECHART -> int
    		val clear_chart : real LINECHART -> real LINECHART
    		val create : {
					GridDyn:bool,
					Legend:bool,
					Rescale:bool,
					XDist:real,
					XIdealGN:int,
					YDist:real,
					YIdealGN:int,
					axis:bool,
					boxsize:int,
					gridvisible:bool,
					h:int,
					linetype:int,
					nline:int,
					page:int,
					thick:int,
					timing:bool,
					title:bool,
					Clear:bool, 
					InitToOrigin:bool,
					StartAtOrigin:bool,
					w:int,
					x:int,
					xcent:real,
					xmax:real,
					xmin:real,
					y:int,
					ycent:real,
					ymax:real,
					ymin:real} -> real LINECHART
    		val create_Xgrids : int * int * int * int * int * int * real * 
								real * real * 'a * 'b * int * int * int 
								-> int list
    		val create_Ygrids : int * int * int * int * int * int * real * 
								real * real * 'a * 'b * int * int * int 
								-> int list
    		val create_graph : 	int * int * int * int * bool * int * int * 
								bool * bool * bool * int * int * int * int * 
								real * real * real * real * real * real * 
								LegendStr.LEGEND ref * bool * bool * int * 
								int * real * real * int ref -> real LINECHART
    		val updXY_line : int * int * int * real LINECHART * bool * int * 
							 bool -> real LINECHART
    		val upd_atag : string list * real LINECHART -> unit
    		val upd_line : int * real * real * bool * real LINECHART 
							-> real LINECHART
    		val upd_pline : int * real * real * bool * real LINECHART * int 
							-> real LINECHART
    		val upd_title : real LINECHART * string -> unit
  		end
	*)

	(*  RECORD COMPONENTS:
			DL : list of object of type LINE
       		pg : current page the line graph belongs to
       		BoxSize : size of the boxes composing a line
       		XAxis : x coordinates line
       		AxeRgnId : ID number of the box surrounding the line graph
       		TIMING : true means time-series graph
       		XMaxTag, YMaxTag : list of the x and y tick tags 
							   positioned on the section displaying
                               value superior to the origin
       		XMinTag, YMinTag : list of the x and y tick tags positioned 
							   on the section displaying
                               value inferior to the origin
       		AW,  AH : size of the box surrounding the line graph 
       		XMIN, XMAX, YMIN, YMAX: minimum and maximum values on 
									the x and y axes
       		Xn, Yn : coordinates of the center in the new system 
                   (meaning not in in the mac coordonates system)
       		KX, KY: reduction value on the X and Y axes from the Mac 
					 system coordonates to the new system (the line graph)
       		Xo, Yo : value of the center in the Mac coordonates
       		Ido : 	is used as a reference to get the center's coordonates
        			whenever the graph moves
        	Axis: flag true means to display Axis names 
	*)
   
	val EmptyLinechart = {	DL = ref ([] : LI.LINE list),
       						pg = ref 0,
       						BoxSize = 0,
       						XAxis = ref 0, 
							YAxis = ref 0,
       						AxeRgnId = ref 0,
       						TIMING = true,
       						LINETYPE = 0,
       						THICK =0,
							XGridList = ref ([] : int list),
							YGridList = ref ([] : int list),
       						XTag = ref ([] : int list), 
							YTag = ref ([] : int list),
							XIdealGN = 0,
							YIdealGN = 0,
							XDist = 0.0,
							YDist = 0.0,
							XMinGN = ref 0,
							XMaxGN = ref 0,
							YMinGN = ref 0,
							YMaxGN = ref 0,
       						XMIN = ref 0.0,
							XMAX = ref 0.0, 
							YMIN = ref 0.0, 
							YMAX = ref 0.0,
							XStartMin = 0.0,
							XStartMax = 0.0,
							YStartMin = 0.0,
							YStartMax = 0.0,
							XStartCent = 0.0,
							YStartCent = 0.0,
       						Xn = ref 0.0, 
							Yn = ref 0.0,
       						KX = ref 0.0,
       						KY = ref 0.0,
       						Xo = ref 0,
       						Yo = ref 0,
       						Ido = ref 0,
       						AxTags = ref ([]:int list),
       						Legend = ref LE.EmptyLegend,
							PRX = ref 0,
							PRY = ref 0,
							PRW = ref 0,
							PRH = ref 0,
							GridVisible = true,
							Rescale = true,
							GridDyn = true,
							Title = ref 0,
							Clear = true, 
							InitToOrigin = false} : real LINECHART; 

  	val ReducPerc : real = 75.0 / 100.0;

  	fun Xold(x, L : real LINECHART) = 
			floor(real(!(#Xo L)) + !(#KX L) * (x - !(#Xn L)))

  	fun Yold(y, L : real LINECHART) = 
			floor(real(!(#Yo L)) + !(#KY L) * (y - !(#Yn L)))

	(*	
		Updates the entries representing the x-coordinate and 
		the y-coordinate of the plot region to actually fit the size. 
		There can be inconsistencies if the region has be repositioned
	*)

	fun	UpdatePlotPosition(L : real LINECHART) =
		let val {x = x1, y = y} = DSRdAttr_GetObjectCenter(!(#XAxis L))
			and {x = x, y = y1} = DSRdAttr_GetObjectCenter(!(#YAxis L))
		in 
		(
			(#PRX L) := x1;
			(#PRY L) := y1 
		) 
		end


   	fun UpdatePlotSize(L : real LINECHART) =
      	(
      		let val nodpos = DSRdAttr_GetObjectCenter(!(#Ido L))
      		in
      		(
       			#Xo L := (#x nodpos);
       			#Yo L := (#y nodpos)
      		)
      		end;
      		
			#PRW L := #w (DSRdAttr_GetObjectSize (!(#YAxis L)));
      		#PRH L := #h (DSRdAttr_GetObjectSize (!(#XAxis L)));
      		#KX L := real (!(#PRW L)) / (!(#XMAX L) - !(#XMIN L));
      		#KY L := real(!(#PRH L)) / (!(#YMIN L) - !(#YMAX L))
      	)


  	fun upd_atag (l, L : real LINECHART) =  
			UT.UpdateTag's (!(#AxTags L), UT.reverse l)


	(* Updates Title region *)

	fun upd_title (L : real LINECHART, title) =
		UT.UpdateTag's ([!(#Title L)], [title])


   	fun LabelAxesX (pg, parent, x, y, w, xmin, XMaxWidth, XMinWidth,
					XMaxTick, XMinTick, xngn, XTagDist, gridvisible) =
  
        let val ugt = 
			UT.CreateTagRegion (
					pg, 
					x - (XMinTick * w) - 15,
					y, XMinTick, w, parent, true)@
			UT.CreateTagRegion (
					pg, 
					if gridvisible then
						x 
					else
						x - 10,
					y, 1, w, parent, true)@ 
			UT.CreateTagRegion (
					pg, 
					x + w,
					y, XMaxTick, w, parent, true)
		in 
		(
			RUT.UpdateTag'i (ugt, 
							 RUT.CreateRoundList (xmin, XTagDist, xngn + 1));
			ugt 
		)
		end 


	fun create_Xgrids (pg, parent, x, y, w, h, XNMax, XNMin, XCent: real, 
					   XMaxWidth, XMinWidth, XMaxTick, XMinTick, xngn) =
	(
		(if (XNMax > XCent) then
		 (
			LC.XCreateGridLines (
				pg,
				x + (w div xngn),
				y, (w div xngn), 
				h, XMaxTick, parent)
		 )
		 else nil)@
		(if (XNMin < XCent) then
		 (
			LC.XCreateGridLines (
				pg,
				x - (w div xngn), 
				y, (~(w div xngn)), 
				h, XMinTick, parent)
		 )
		 else nil)
	)


	fun create_Ygrids (pg, parent, x, y, w, h, YNMax, YNMin, YCent: real, 
					   YMaxWidth, YMinWidth, YMaxTick, YMinTick, yngn) =
	(
		(if (YNMax > YCent) then
		 (
			LC.YCreateGridLines (
				pg,
				x, 
				y - (h div yngn),
				w, (~(h div yngn)), 
				YMaxTick, parent)
		 )
		 else nil)@
		(if (YNMin < YCent) then
		 (
			LC.YCreateGridLines (
				pg,
				x, 
				y + (h div yngn), w,
				(h div yngn), YMinTick, parent)
		 )
		 else nil)
	)


	(* Creates the labels on the y-axis *)

   	fun LabelAxesY (pg, parent, x, y, h, ymin, YMaxWidth, YMinWidth,
					YMaxTick, YMinTick, yngn, YTagDist, gridvisible) =
  
        let val ugt = 
			UT.CreateTagRegion (
					pg, 
					x,
					y + (YMinTick * h),
					YMinTick, (~h), parent, false)@
			UT.CreateTagRegion (
					pg,
					x, 
					if gridvisible then
						y 
					else
						y + 10,
					1, h, parent, false)@
			UT.CreateTagRegion (
					pg, 
					x,
					y - h,
					YMaxTick, (~h), parent, false)
		in 
		(
			RUT.UpdateTag'i (ugt, 
							 RUT.CreateRoundList (ymin, YTagDist, yngn + 1));
			ugt 
		)
		end


  	fun create_graph (AxeRgnId, pg, nline, boxsize, timing, 
					  linetype, thick, gridvisible, axis,
					  x, y, w, h, xmin, xmax, ymin, ymax, xcent, ycent, 
					  legend, rescale, GridDyn, xidealGN, yidealGN, 
					  xdist, ydist, title, Clear, InitToOrigin,
					  StartAtOrigin) =
	let val XMinWidth = UT.round((real(w) * (xcent - xmin)) / (xmax - xmin))
		and XMaxWidth = w - 
						UT.round((real(w) * (xcent - xmin)) / (xmax - xmin))
		and YMinWidth = UT.round((real(h) * (ycent - ymin)) / (ymax - ymin))
		and YMaxWidth = h - 
						UT.round((real(h) * (ycent - ymin)) / (ymax - ymin))
		and XTagDist = if GridDyn then RUT.CalcDist((xmax - xmin), xidealGN)
					   else xdist
		and YTagDist = if GridDyn then RUT.CalcDist((ymax - ymin), yidealGN)
					   else ydist
	in
	(
		let val Xa = x + UT.round(real(XMinWidth - XMaxWidth) / 2.0)
  	    	and Ya = y - UT.round(real(YMinWidth - YMaxWidth) / 2.0)

		in
		(
       		let val XAxes = LC.CreateXAxes(pg, Ya, x, w)
				and YAxes = LC.CreateYAxes(pg, Xa, y, h)
				and XMaxTick = (if GridDyn then
									RUT.CalcGN((xmax - xcent), 
											   UT.round(
													((xmax - xcent) * 
													 real(xidealGN)) / 
													(xmax - xmin)), 
											   XTagDist)
							   else UT.ceiling((xmax - xcent) / (XTagDist))) 
				and XMinTick = (if GridDyn then
									RUT.CalcGN((xcent - xmin),
									   		   xidealGN - 
									   		   UT.round(
													((xmax - xcent) * 
													 real(xidealGN)) / 
													(xmax - xmin)),
									   		  XTagDist)
								else UT.ceiling((xcent - xmin) / (XTagDist)))
				and YMaxTick = (if GridDyn then
									RUT.CalcGN((ymax - ycent), 
									   	  	   UT.round(
													((ymax - ycent) * 
													real(yidealGN)) / 
													(ymax - ymin)), 
									   	  	  YTagDist)
							    else UT.ceiling((ymax - ycent) / YTagDist))
				and YMinTick = (if GridDyn then
									RUT.CalcGN((ycent - ymin),
									      	   yidealGN - 
									      	   UT.round(
													((ymax - ycent) * 
													  real(yidealGN)) / 
													(ymax - ymin)),
							     		  	  YTagDist)
								else UT.ceiling((ycent - ymin) / YTagDist))
       		in
			(
				let val XMaxTick = 
						if ((XMaxTick = 0) andalso (XMinTick = 0)) then 1
						else XMaxTick
					and YMaxTick = 
						if ((YMaxTick = 0) andalso (YMinTick = 0)) then 1
						else YMaxTick
				in
				( 
					let val XNMin = xcent - (XTagDist * real(XMinTick))
						and XNMax = xcent + (XTagDist * real(XMaxTick))
						and YNMin = ycent - (YTagDist * real(YMinTick))
						and YNMax = ycent + (YTagDist * real(YMaxTick))
					in
					(       
       					DSStr_MakeNodeIntoRgn {	obj = XAxes, 
										   		parent = AxeRgnId};
       					DSStr_MakeNodeIntoRgn {	obj = YAxes, 
										   		parent = AxeRgnId};
						let val xngn = XMinTick + XMaxTick
							and yngn = YMinTick + YMaxTick
						in
						(
       						let val AxeLabX = 
									LabelAxesX (pg, 
												if gridvisible then AxeRgnId
												else XAxes, Xa, 
												if gridvisible then
													(y + (h div 2) + 10)
												else 
													(Ya + 10), 
												(w div xngn), 
												XNMin, XMaxWidth, XMinWidth,
												XMaxTick, XMinTick,
												xngn, XTagDist, gridvisible)

           						and AxeLabY = 
									LabelAxesY (pg, 
												if gridvisible then AxeRgnId
												else YAxes, 
												if gridvisible then
													(x + (w div 2) + 10)
												else
													(Xa + 10), 
												Ya, (h div yngn), 
												YNMin, YMaxWidth, YMinWidth,
												YMaxTick, YMinTick,
												yngn, YTagDist, gridvisible)

           						and AxTags = 
									(if axis then 
										LC.CreateAxesTags (
												pg, Xa, (y - h div 2) - 15,  
												(x + w div 2) + 15, Ya - 5,
												AxeRgnId) 
								 	 else [])
               				in
							(
         					{
								DL = ref (LI.Init(nline, AxeRgnId, pg, Xa, Ya,
												  boxsize, timing, linetype,
												  thick, StartAtOrigin)),
						 
								pg = ref pg,
         				 
								BoxSize = boxsize,
         				 
								XAxis = ref (XAxes),
						 
								YAxis = ref (YAxes),

								AxeRgnId = ref AxeRgnId,

         						TIMING = timing,

         						LINETYPE = linetype,

         						THICK = thick,

								XGridList = 
									ref(create_Xgrids(
											pg, 
											if gridvisible then AxeRgnId
											else XAxes, Xa, 
											if gridvisible then y else Ya, 
											w, if gridvisible then h else 4, 
											XNMax, XNMin, xcent, XMaxWidth, 
											XMinWidth, XMaxTick, XMinTick, 
											xngn)),

								YGridList =
									ref(create_Ygrids(
											pg, 
											if gridvisible then AxeRgnId
											else YAxes, 
											if gridvisible then x else Xa, 
											Ya, if gridvisible then w else 4, 
											h, YNMax, YNMin, ycent, YMaxWidth,
 											YMinWidth, YMaxTick, YMinTick, 
											yngn)),

         						XTag = ref (AxeLabX),
	
         						YTag = ref (AxeLabY),

								XIdealGN = xidealGN,

								YIdealGN = yidealGN,

								XDist = xdist,
						
								YDist = ydist,

								XMinGN = ref XMinTick,

								XMaxGN = ref XMaxTick,
				
								YMinGN = ref YMaxTick,

								YMaxGN = ref YMaxTick,

								XMIN = ref XNMin,

         						XMAX = ref XNMax,
 
								YMIN = ref YNMin,
 
								YMAX = ref YNMax,

								XStartMin = xmin,

								XStartMax = xmax,

								YStartMin = ymin,

								YStartMax = ymax,

								XStartCent = xcent,

								YStartCent = ycent,

         						Xn = ref xcent,
 
								Yn = ref ycent,

        	 					KX = ref (real (w) / (XNMax - XNMin)),

         						KY = ref (real (h) / (ymin - ymax)),

								Xo = ref (Xa),

         						Yo = ref (Ya),

         						Ido = 
									let val nod = 
										DSStr_CreateNode {page = pg, x = Xa, 
												  	  y = Ya, w = 1, h = 1, 
												  	  shape = 1}
                			  		in 
							  		(

                  						DSWtAttr_ObjectFlags {
											obj = nod, flag = 1, 
											value = true};

                  						DSStr_MakeNodeIntoRgn {
											obj = nod, 
											parent = AxeRgnId};

                  						ref nod
                  			   		)
                			  		end,

								AxTags = ref AxTags,
 
        						Legend = legend,

								PRX = ref x,

								PRY = ref y,

								PRW = ref w,

								PRH = ref h,

								GridVisible = gridvisible,

								Rescale = rescale,

								GridDyn = GridDyn,

								Title = title,

								Clear = Clear, 

								InitToOrigin = InitToOrigin

								}: real LINECHART
							)
							end
						)
						end
          			)
       				end
				)
				end
			)
			end
		)
		end
	)
	end


	(* Initializes the chart to be empty and to have the initial layout *)

	fun clear_chart (L : real LINECHART) =
	(
		let val xmin = (#XStartMin L)
			and xmax = (#XStartMax L)
			and ymin = (#YStartMin L)
			and ymax = (#YStartMax L)
			and xcent = (#XStartCent L)
			and ycent = (#YStartCent L)
		in
		( 
			UpdatePlotSize(L);
			UpdatePlotPosition(L);

			let val XMinWidth = UT.round((real(!(#PRW L)) * (xcent - xmin)) / 
					 	      			 (xmax - xmin))
				and XMaxWidth = !(#PRW L) - 
								UT.round((real(!(#PRW L)) * (xcent - xmin)) / 
										 (xmax - xmin))
				and YMinWidth = UT.round((real(!(#PRH L)) * (ycent - ymin)) / 
										 (ymax - ymin))
				and YMaxWidth = !(#PRH L) - 
								UT.round((real(!(#PRH L)) * (ycent - ymin)) / 
										 (ymax - ymin))
				and XTagDist = if (#GridDyn L) then 
						 			RUT.CalcDist((xmax - xmin), (#XIdealGN L))
					   	   	   else (#XDist L)
				and YTagDist = if (#GridDyn L) then 
									RUT.CalcDist((ymax - ymin), (#YIdealGN L))
					   	   	   else (#YDist L)
			in
			(
				let val Xa = !(#PRX L) + 
							 UT.round(real(XMinWidth - XMaxWidth) / 2.0)
					and Ya = !(#PRY L) - 
							 UT.round(real(YMinWidth - YMaxWidth) / 2.0)
					and XMaxTick = 
						(if (#GridDyn L) then
							RUT.CalcGN((xmax - xcent), 
									   UT.round(((xmax - xcent) * 
												 real(#XIdealGN L)) / 
												(xmax - xmin)), 
									  XTagDist)
					 	else 
							UT.ceiling((xmax - xcent) / XTagDist))
					and XMinTick = 
						(if (#GridDyn L) then
							RUT.CalcGN((xcent - xmin), 
									   (#XIdealGN L) -
									   UT.round(
										((xmax - xcent) * real(#XIdealGN L)) / 
										(xmax - xmin)), 
									  XTagDist)
					 	else 
							UT.ceiling((xcent - xmin) / XTagDist))
					and YMaxTick = 
						(if (#GridDyn L) then
							RUT.CalcGN((ymax - ycent), 
									  UT.round(
										((ymax - ycent) * real(#YIdealGN L)) / 
										(ymax - ymin)), 
								     YTagDist)
						 else 
							UT.ceiling((ymax - ycent) / YTagDist))
					and YMinTick = 
						(if (#GridDyn L) then
							RUT.CalcGN((ycent - ymin),
									  (#YIdealGN L) -
									   UT.round(
										((ymax - ycent) * real(#YIdealGN L)) / 
										(ymax - ymin)), 
									  YTagDist)
					 	else 
							UT.ceiling((ycent - ymin) / YTagDist))
				in
				(
					let val XMaxTick = 
							if ((XMaxTick = 0) andalso (XMinTick = 0)) then 1
							else XMaxTick
						and YMaxTick = 
							if ((YMaxTick = 0) andalso (YMinTick = 0)) then 1
							else YMaxTick
					in
					(
						let val XNMin = xcent - (XTagDist * real(XMinTick))
							and XNMax = xcent + (XTagDist * real(XMaxTick))
							and YNMin = ycent - (YTagDist * real(YMinTick))
							and YNMax = ycent + (YTagDist * real(YMaxTick))
							and xngn = XMaxTick + XMinTick
							and yngn = YMaxTick + YMinTick
						in
						(
							LI.init_all(!(#DL L));
		
							DSWtAttr_ObjectPosition{obj = !(#XAxis L),
										 			x = !(#PRX L),
										 	    	y = Ya};
							DSWtAttr_ObjectPosition{obj = !(#YAxis L),
													x = Xa,
													y = !(#PRY L)};

							UT.DeleteIdList (!(#XGridList L));	
							(#XGridList L) := 
								create_Xgrids(
									!(#pg L), 
									if (#GridVisible L) then !(#AxeRgnId L)
									else !(#XAxis L), Xa, 
									if (#GridVisible L) then !(#PRY L) 
									else Ya, 
									!(#PRW L), 
									if (#GridVisible L) then !(#PRH L) else 4, 
									XNMax, XNMin, xcent, XMaxWidth, XMinWidth, 
									XMaxTick, XMinTick, xngn);

							UT.DeleteIdList (!(#YGridList L));
							(#YGridList L) := 
								create_Ygrids(
									!(#pg L), 
									if (#GridVisible L) then !(#AxeRgnId L)
									else !(#YAxis L), 
									if (#GridVisible L) then !(#PRX L) 
									else Xa, Ya, 
									if (#GridVisible L) then !(#PRW L) else 4, 
									!(#PRH L), YNMax, YNMin, ycent, YMaxWidth, 
									YMinWidth, YMaxTick, YMinTick, yngn);

							UT.DeleteIdList (!(#XTag L));
							(#XTag L) := 
								LabelAxesX (
									!(#pg L), 
									if (#GridVisible L) then !(#AxeRgnId L)
									else !(#XAxis L), Xa, 
									if (#GridVisible L) then
										(!(#PRY L) + (!(#PRH L) div 2) + 10)
									else (Ya + 10), 
									(!(#PRW L) div xngn), XNMin, XMaxWidth, 
									XMinWidth, XMaxTick, XMinTick, xngn, 
									XTagDist, (#GridVisible L));

							UT.DeleteIdList (!(#YTag L));
							(#YTag L) := 
								LabelAxesY (
									!(#pg L), 
									if (#GridVisible L) then !(#AxeRgnId L)
									else !(#YAxis L), 
									if (#GridVisible L) then
										(!(#PRX L) + (!(#PRW L) div 2) + 10)
									else (Xa + 10), 
									Ya, (!(#PRH L) div yngn), YNMin, 
									YMaxWidth, YMinWidth, YMaxTick, YMinTick, 
									yngn, YTagDist, (#GridVisible L));

							(#XMinGN L) := XMinTick;
							(#XMaxGN L) := XMaxTick;
							(#YMinGN L) := YMinTick;
							(#YMaxGN L) := YMaxTick;
							(#XMIN L) := XNMin;
							(#XMAX L) := XNMax;
							(#YMIN L) := YNMin;
							(#YMAX L) := YNMax;
							(#Xn L) := xcent;
							(#Yn L) := ycent;
							(#KX L) := (real(!(#PRW L)) / (XNMax - XNMin));
							(#KY L) := (real(!(#PRH L)) / (YNMax - YNMin));

							DSWtAttr_ObjectPosition{obj = !(#Ido L),
													x = Xa,
													y = Ya};

							L
						)
						end
					)
					end
				)
				end
			)
			end
		)
		end
	)


	fun ResizeXLines(XMIN: real, XMAX: real, L : real LINECHART) =
       		let val nod = DSStr_CreateNode {page = !(#pg L),
                      x = Xold(((!(#XMAX L) + !(#XMIN L)) / 2.0), L), 
                      y = #y (DSRdAttr_GetObjectCenter (!(#Ido L))), 
                      w = abs (floor(!(#KX L) * (!(#XMAX L) - !(#XMIN L)))), 
                      h = 1, shape = 1}
       		in
       		(
       			LI.MakeLineRgn(!(#DL L),nod);
       			DSWtAttr_AdjustObjectSize {obj = nod,
                      					   w = floor(!(#KX L) * 
													 (XMAX - XMIN)),
                      					   h = 1};
       			DSWtAttr_ObjectPosition {obj = nod,
                      					 x = Xold
												((XMAX + XMIN) / 2.0, L),
                      					 y = #y (
											DSRdAttr_GetObjectCenter (nod))};
       			LI.MakeLineRgn(!(#DL L),!(#AxeRgnId L));
       			DSStr_DeleteObject(nod)
       		)
       		end


	(* Change the x-axis-range, it needed *)

  	fun ResizeX(minx, maxx, L : real LINECHART) = 
	(
		if (#Rescale L) then 
		(
      		if ((maxx > !(#XMAX L)) orelse (minx < !(#XMIN L))) then 
			(
				let val NXMax = if maxx > !(#XMAX L) then (maxx / ReducPerc)
								else !(#XMAX L)
					and NXMin = if minx < !(#XMIN L) then (minx / ReducPerc)
								else !(#XMIN L)
				in
				(
           			let val space = (if (#GridDyn L) then
										RUT.CalcDist((NXMax - NXMin), 
											 	     (#XIdealGN L))
									 else
										(#XDist L))
					in
					(
						let val NMaxTick = (
									if (#GridDyn L) then
										RUT.CalcGN(
											(NXMax - !(#Xn L)),
											 UT.round(
												((NXMax - !(#Xn L)) * 
												 real((#XIdealGN L))) / 
												(NXMax - NXMin)), 
											space)
									else (* Fixed Dist *)
										UT.ceiling((NXMax - !(#Xn L)) / 
													space))
							and NMinTick = (
									if (#GridDyn L) then
										RUT.CalcGN(
											(!(#Xn L) - NXMin), 
											(#XIdealGN L) - 
											UT.round
											   (((NXMax - !(#Xn L)) * 
												 real((#XIdealGN L))) /
 												(NXMax - NXMin)), 
											space)
									else (* Fixed Dist *)
										UT.round((!(#Xn L) - NXMin) / 
												  space))
           				in
						(
             				let val NMaxTick =
								if ((NMinTick = 0) andalso (NMaxTick = 0))
								then 1
								else NMaxTick
							in
							(
								let val NXMAX = !(#Xn L) + 
												(space * real(NMaxTick))
             						and NXMIN = !(#Xn L) - 
												(space * real(NMinTick))
									and ngn = NMinTick + NMaxTick
									and NMaxWidth = 
											UT.round(
												real(NMaxTick * !(#PRW L)) / 
												real(NMinTick + NMaxTick))
									and NMinWidth =
											!(#PRW L) -
											UT.round(
												real(NMaxTick * !(#PRW L)) / 
												real(NMinTick + NMaxTick))
               					in
								(
									let val Xa = 
											!(#PRX L) + UT.round(real(
											NMinWidth - NMaxWidth) / 2.0)
									in
									(
										if ((NMaxTick <> (!(#XMaxGN L))) 
											orelse 
											(NMinTick <> (!(#XMinGN L)))) 
										then
										(
											DSWtAttr_ObjectPosition{
												obj = !(#YAxis L),
												x = Xa,
												y = !(#PRY L)};

											UT.DeleteIdList(!(#XTag L));
											#XTag L := 
												LabelAxesX(
													!(#pg L), 
													if (#GridVisible L) then
														!(#AxeRgnId L)
													else !(#XAxis L), Xa, 
													if (#GridVisible L) then
														(!(#PRY L) + 
														(!(#PRH L) div 2) + 10)
													else (!(#Yo L) + 10),
													(!(#PRW L) div ngn), 
													NXMIN, NMaxWidth,
													NMinWidth, NMaxTick, 
													NMinTick, ngn, space, 
													(#GridVisible L));
											
											UT.DeleteIdList(!(#XGridList L));
											#XGridList L := 
												create_Xgrids(
													!(#pg L), 
													if (#GridVisible L) then
														!(#AxeRgnId L)
													else !(#XAxis L), Xa, 
													if (#GridVisible L) then 
														!(#PRY L) 
													else !(#Yo L), 
													!(#PRW L), 
													if (#GridVisible L) then 
														!(#PRH L) 
													else 4, 
													NXMAX, NXMIN, !(#Xn L),
													NMaxWidth, 
													NMinWidth, NMaxTick, 
													NMinTick, ngn)
										)
										else
										(
											RUT.UpdateTag'i (
												!(#XTag L),
                                 				RUT.CreateRoundList (
													NXMIN, space, ngn + 1))
										);

                 						let val XMAX = !(#XMAX L) 
											and XMIN = !(#XMIN L)
                 						in
                   						(
                   							#XMAX L := NXMAX;
                   							#XMIN L := NXMIN;
											#XMinGN L := NMinTick;
											#XMaxGN L := NMaxTick;
                   							#KX L := (
												real (!(#PRW L)) / 
									       		(!(#XMAX L) - !(#XMIN L)));
											#Xo L := Xa;
											DSWtAttr_ObjectPosition{
												obj = !(#Ido L), 
												x = Xa, 
												y = !(#Yo L)};

											ResizeXLines(XMIN, XMAX, L)
										)
                 						end (* XMAX, XMIN *)
									)
									end (* Xa *)
								)
								end (* NMaxTick *)
                 			)
               				end (* NXMAX, NXMIN *)
						)
             			end (* NMaxTick, NMinTick *)
					)
           			end (* space *)
				)
         		end (* NXMax, NXMin *)
			)
      		else () (* x < !(#XMAX L) & x > !(#XMIN L) *)
		)
		else (* Move Axes *)
		(
			if (maxx > !(#XMAX L)) then
			(
				let val NXMax = maxx / ReducPerc
					and space = (if (!(#XMaxGN L) > 0) then 
									(!(#XMAX L) - !(#Xn L)) / 
									real(!(#XMaxGN L))
								 else if (!(#XMinGN L) > 0) then
									abs((!(#XMIN L) + !(#Xn L)) / 
									real(!(#XMinGN L)))
								 else 1.0)
				in
				(
					let val MDist = space * ((NXMax - !(#XMAX L)) / (space))
					in
					(
						let val Move = UT.round((real(!(#PRW L)) * MDist) /
												 (!(#XMAX L) - (!(#XMIN L))))
							and XMinTick = !(#XMinGN L)
							and XMaxTick = !(#XMaxGN L)
						in
						(
							let val MinPos =
								!(#PRX L) - 
								UT.round(real(!(#PRW L)) / 2.0) + Move
							in
								LI.DelXSmaller(!(#DL L), MinPos, 
											   !(#AxeRgnId L))
							end;

							let val nod = DSStr_CreateNode{
												page = !(#pg L),
												x = !(#PRX L),
												y = !(#PRY L),
												w = !(#PRW L),
												h = 1,
												shape = 1}
							in
							(
								LI.MakeLineRgn(!(#DL L), nod);

								DSWtAttr_ObjectPosition{
											obj = nod,
											x = !(#PRX L) - Move,
											y = !(#PRY L)};

								LI.MakeLineRgn(!(#DL L), !(#AxeRgnId L));

								DSStr_DeleteObject(nod)
							)
							end; (* nod *)
							
							let val NXMIN = !(#XMIN L) + MDist	
								and NXMAX = !(#XMAX L) + MDist
							in
							(	
                 				RUT.UpdateTag'i (
									!(#XTag L),
                                 	RUT.CreateRoundList (
										NXMIN, space, 
										(XMaxTick + XMinTick + 1)));
                 				
								#XMAX L := NXMAX;
                   				#XMIN L := NXMIN;
								#Xn L := !(#Xn L) + MDist;
                   				#KX L := real (!(#PRW L)) / 
										 (!(#XMAX L) - !(#XMIN L))
                 			)
							end (* NXMIN, NXMAX *)
						)
						end (* Move, XMinTick, XMaxTick *)
					)
					end (* MDist *)
				)
				end (* NXMax *)
			)
			else (* x <= !(#XMAX L) *)
			(
				if (minx < !(#XMIN L)) then
				(
					let val NXMax = !(#XMAX L)
						and NXMin = minx / ReducPerc
						and space = 
								(if (!(#XMaxGN L) > 0) then 
									(!(#XMAX L) - !(#Xn L)) / 
									real(!(#XMaxGN L))
								 else if (!(#XMinGN L) > 0) then
									abs((!(#XMIN L) + !(#Xn L)) /
										real(!(#XMinGN L)))
								 else 1.0)
					in
					(
						let val MDist = 
							(space * (!(#XMIN L) - NXMin)) / space
						in
						(
							let val Move = UT.round(
											(real(!(#PRW L)) * MDist) /
											(!(#XMAX L) - (!(#XMIN L))))
								and XMinTick = !(#XMinGN L)
								and XMaxTick = !(#XMaxGN L)
							in
							(
								let val MaxPos =
									!(#PRX L) + (!(#PRW L) div 2) + Move
								in
									LI.DelXLarger(!(#DL L), MaxPos, 
												  !(#AxeRgnId L))
								end;

								let val nod = DSStr_CreateNode{
												page = !(#pg L),
												x = !(#PRX L),
												y = !(#PRY L),
												w = !(#PRW L),
												h = 1,
												shape = 1}
								in
								(
									LI.MakeLineRgn(!(#DL L), nod);

									DSWtAttr_ObjectPosition{
											obj = nod,
											x = !(#PRX L) + Move,
											y = !(#PRY L)};

									LI.MakeLineRgn(!(#DL L), !(#AxeRgnId L));

									DSStr_DeleteObject(nod)
								)
								end;
							
								let val NXMIN = !(#XMIN L) - MDist	
									and NXMAX = !(#XMAX L) - MDist
								in
								(
                 						RUT.UpdateTag'i (
											!(#XTag L),
                                 			RUT.CreateRoundList (
												NXMIN, space, 
												(XMaxTick + XMaxTick + 1)));
                 						
									let val XMIN = !(#XMIN L) 
										and XMAX = !(#XMAX L)
                 					in
                   					(
                   						#XMAX L := NXMAX;
                   						#XMIN L := NXMIN;
										#Xn L := !(#Xn L) - MDist;
                   						#KX L := real (!(#PRW L)) / 
										 	 	 (!(#XMAX L) - !(#XMIN L))
                   					)
                 					end (* XMIN, XMAX *)
								)
               					end (* NXMIN, NXMAX *)
							)
							end (* Move, XMinTick, XMaxTick *)
						) 
						end (* MDist *)
					)
					end (* NXMax, NXMin *)
				)
				else () (* x > !(#XMIN L) *)
			) 
		) 
	) 


   	fun ResizeYLines(YMIN: real, YMAX: real, L : real LINECHART) =
       		let val nod = 
				DSStr_CreateNode {page = !(#pg L),
                      			  x = #x (DSRdAttr_GetObjectCenter 
											(!(#Ido L))), 
                      			  y = Yold((!(#YMAX L) + !(#YMIN L)) / 2.0, L),
					 			  w = 1, 
                      			  h = abs(floor(!(#KY L) * (!(#YMAX L) - 
											!(#YMIN L)))),
                      			  shape = 1}
       		in
       		(
       			LI.MakeLineRgn(!(#DL L), nod);
       			DSWtAttr_AdjustObjectSize {obj = nod, w = 1,
                      					   h = abs(floor(!(#KY L) *  
													 (YMAX - YMIN)))};
       			DSWtAttr_ObjectPosition {obj = nod,
                      					 x = #x (DSRdAttr_GetObjectCenter 
												  (nod)),
                      					 y = Yold
												(((YMAX + YMIN) / 2.0), L)};
       			LI.MakeLineRgn(!(#DL L),!(#AxeRgnId L));
       			DSStr_DeleteObject(nod)
       		)
       		end


	(* Change the y-axis-range, if needed *)

  	fun ResizeY(miny, maxy, L : real LINECHART) = 
	(
		if (#Rescale L) then 
		(
      		if ((maxy > !(#YMAX L)) orelse (miny < !(#YMIN L))) then 
			(
				let val NYMax = if maxy > !(#YMAX L) then
									(maxy / ReducPerc)
								else 
									!(#YMAX L)
					and NYMin = if miny < !(#YMIN L) then
									(miny / ReducPerc)
								else !(#YMIN L)
				in
				(
           			let val space = (if (#GridDyn L) then
										RUT.CalcDist((NYMax - NYMin), 
											 	     (#YIdealGN L))
									 else
										(#YDist L))
					in
					(
						let val NMaxTick = (
									if (#GridDyn L) then
										RUT.CalcGN(
											(NYMax - !(#Yn L)),
											UT.round(
												((NYMax - !(#Yn L)) * 
												 real((#YIdealGN L))) / 
												(NYMax - NYMin)), 
											space)
									else (* Fixed Dist *)
										UT.ceiling((NYMax - !(#Yn L)) / space))
							and NMinTick = (
									if (#GridDyn L) then
										RUT.CalcGN(
											(!(#Yn L) - NYMin), 
											(#YIdealGN L) - 
											UT.round
											   (((NYMax - !(#Yn L)) * 
												real((#YIdealGN L))) /
 												(NYMax - NYMin)), 
											space)
									else (* Fixed Dist *)
										UT.round((!(#Yn L) - NYMin) / space))
           				in
						(
             				let val NMaxTick =
								if ((NMinTick = 0) andalso (NMaxTick = 0))
								then 1
								else NMaxTick
							in
							(
								let val NYMAX = !(#Yn L) + 
												(space * real(NMaxTick))
             						and NYMIN = !(#Yn L) - 
												(space * real(NMinTick))
									and ngn = NMinTick + NMaxTick
									and NMaxWidth = 
											UT.round(
												real(NMaxTick * !(#PRH L)) / 
												real(NMinTick + NMaxTick))
									and NMinWidth =
											!(#PRH L) -
											UT.round(
												real(NMaxTick * !(#PRH L)) / 
												real(NMinTick + NMaxTick))
               					in
								(
									let val Ya = 
											!(#PRY L) - UT.round(real(
											NMinWidth - NMaxWidth) / 2.0)
									in
									(
										if ((NMaxTick <> (!(#YMaxGN L))) 
											orelse 
											(NMinTick <> (!(#YMinGN L)))) 
										then
										(
											DSWtAttr_ObjectPosition{
												obj = !(#XAxis L),
												x = !(#PRX L),
												y = Ya};

											UT.DeleteIdList(!(#YTag L));
											#YTag L := 
												LabelAxesY(
													!(#pg L), 
													if (#GridVisible L) then
														!(#AxeRgnId L)
													else !(#YAxis L), 
													if (#GridVisible L) then
														(!(#PRX L) + 
														(!(#PRW L) div 2) + 10)
													else (!(#Xo L) + 10), 
													Ya,
													(!(#PRH L) div ngn), 
													NYMIN, NMaxWidth,
													NMinWidth, NMaxTick, 
													NMinTick, ngn, space, 
													(#GridVisible L));
											
											UT.DeleteIdList(!(#YGridList L));
											#YGridList L := 
												create_Ygrids(
													!(#pg L), 
													if (#GridVisible L) then
														!(#AxeRgnId L)
													else !(#YAxis L), 
													if (#GridVisible L) then 
														!(#PRX L) 
													else !(#Xo L), Ya, 
													if (#GridVisible L) then
														!(#PRW L)
													else 4, 
													!(#PRH L), 
													NYMAX, NYMIN, !(#Yn L),
													NMaxWidth, 
													NMinWidth, NMaxTick, 
													NMinTick, ngn)
										)
										else
										(
											RUT.UpdateTag'i (
												!(#YTag L),
                                 				RUT.CreateRoundList (
													UT.rounding(NYMIN), 
													UT.rounding(space), 
													ngn + 1))
										);

                 						let val YMAX = !(#YMAX L) 
											and YMIN = !(#YMIN L)
                 						in
                   						(
                   							#YMAX L := NYMAX;
                   							#YMIN L := NYMIN;
											#YMinGN L := NMinTick;
											#YMaxGN L := NMaxTick;
                   							#KY L := (
												real (!(#PRH L)) / 
									       		(!(#YMIN L) - !(#YMAX L)));
											#Yo L := Ya;
											DSWtAttr_ObjectPosition{
												obj = !(#Ido L), 
												x = !(#Xo L), 
												y = Ya};

											ResizeYLines(YMIN, YMAX, L)
										)
                 						end (* YMAX, YMIN *)
									)
									end (* Ya *)
								)
								end (* NMaxTick *)
                 			)
               				end (* NYMAX, NYMIN *)
						)
             			end (* NMaxTick, NMinTick *)
					)
           			end (* space *)
				)
         		end (* NYMax, NYMin *)
			)
      		else () (* y < !(#YMAX L) & y > !(#YMIN L) *)
		)
		else (* Move Axes *)
		(
			if (maxy > !(#YMAX L)) then
			(
				let val NYMax = (maxy / ReducPerc)
					and space = (if (!(#YMaxGN L) > 0) then 
									(!(#YMAX L) - !(#Yn L)) / 
									real(!(#YMaxGN L))
								 else if (!(#YMinGN L) > 0) then
									abs((!(#YMIN L) + !(#Yn L)) / 
										real(!(#YMinGN L)))
								 else 1.0)
				in
				(
					let val MDist = space * (NYMax - !(#YMAX L) / space)
					in
					(
						let val Move = UT.round((real(!(#PRH L)) * MDist) /
												(!(#YMAX L) - (!(#YMIN L))))
							and YMinTick = !(#YMinGN L)
							and YMaxTick = !(#YMaxGN L)
						in
						(
							let val MinPos =
								!(#PRY L) + 
								UT.round(real(!(#PRH L)) / 2.0) - Move
							in
								LI.DelYSmaller(!(#DL L), MinPos, 
											   !(#AxeRgnId L))
							end;

							let val nod = DSStr_CreateNode{
												page = !(#pg L),
												x = !(#PRX L),
												y = !(#PRY L),
												w = 1,
												h = !(#PRH L),
												shape = 1}
							in
							(
								LI.MakeLineRgn(!(#DL L), nod);

								DSWtAttr_ObjectPosition{
											obj = nod,
											x = !(#PRX L),
											y = !(#PRY L) + Move};

								LI.MakeLineRgn(!(#DL L), !(#AxeRgnId L));

								DSStr_DeleteObject(nod)
							)
							end; (* nod *)
							
							let val NYMIN = !(#YMIN L) + MDist	
								and NYMAX = !(#YMAX L) + MDist
							in
							(	
                 				RUT.UpdateTag'i (
									!(#YTag L),
                                 	RUT.CreateRoundList (
										UT.rounding(NYMIN), 
										UT.rounding(space), 
										(YMaxTick + YMinTick + 1)));
                 				
								#YMAX L := NYMAX;
                   				#YMIN L := NYMIN;
								#Yn L := !(#Yn L) + MDist;
                   				#KY L := real (!(#PRH L)) / 
										 (!(#YMIN L) - !(#YMAX L))
                 			)
							end (* NYMIN, NYMAX *)
						)
						end (* Move, YMinTick, YMaxTick *)
					)
					end (* MDist *)
				)
				end (* NYMax *)
			)
			else (* y <= !(#YMAX L) *)
			(
				if (miny < !(#YMIN L)) then
				(
					let val NYMax = !(#YMAX L)
						and NYMin = (miny / ReducPerc)
					in
					(
						let val MDist = !(#YMIN L) - NYMin
						in
						(
							let val Move = UT.round((real(!(#PRH L)) * MDist) /
													(!(#YMAX L) - !(#YMIN L)))
								and YMinTick = !(#YMinGN L)
								and YMaxTick = !(#YMaxGN L)
							in
							(
								let val MaxPos = 
									!(#PRX L) - (!(#PRH L) div 2) + Move
								in
									LI.DelYLarger(!(#DL L), MaxPos, 
											   	  !(#AxeRgnId L)) 
								end;

								let val nod = DSStr_CreateNode{
												page = !(#pg L),
												x = !(#PRX L),
												y = !(#PRY L),
												w = 1,
												h = !(#PRH L),
												shape = 1}
								in
								(
									LI.MakeLineRgn(!(#DL L), nod);

									DSWtAttr_ObjectPosition{
											obj = nod,
											x = !(#PRX L),
											y = !(#PRY L) + Move};

									LI.MakeLineRgn(!(#DL L), !(#AxeRgnId L));

									DSStr_DeleteObject(nod)
								)
								end;
							
								let val NYMIN = !(#YMIN L) - MDist	
									and NYMAX = !(#YMIN L) - MDist
								in
								(
                 					let val space = 
										(if YMaxTick <> 0 then
                                      		((NYMAX - !(#Yn L)) / 
											real(YMaxTick))
                                      	 else 
											if YMinTick <> 0 then
												((NYMIN + !(#Yn L)) /
												 (real(YMinTick)))
											else 1.0)
                 					in
									(
                 						RUT.UpdateTag'i (
											!(#YTag L),
                                 			RUT.CreateRoundList (
												UT.rounding(NYMIN), 
												UT.rounding(space), 
												(YMaxTick + YMaxTick + 1)))
									)
									end;
                 						
									let val YMIN = !(#YMIN L) 
										and YMAX = !(#YMAX L)
                 					in
                   					(
                   						#YMAX L := NYMAX;
                   						#YMIN L := NYMIN;
										#Yn L := !(#Yn L) - MDist;
                   						#KY L := real (!(#PRH L)) / 
										 	 	 (!(#YMIN L) - !(#YMAX L))
                   					)
                 					end (* YMIN, YMAX *)
								)
               					end (* NYMIN, NYMAX *)
							)
							end (* Move, YMinTick, YMaxTick *)
						) 
						end (* MDist *)
					)
					end (* NYMax, NYMin *)
				)
				else () (* y > !(#YMIN L) *)
			) 
		) 
	) 
 

  	fun updXY_line(n, X, Y, L : real LINECHART, pattern, pat, drawline) =
    		let val nline = UT.NthElement(n,!(#DL L))
     		in 
			(
       			if LI.same (nline, LI.EmptyLine)
       			then  
					let val InitNod = LI.CreateLine(!(#pg L), X, Y,
													#BoxSize L,
													if pattern then pat
                                               		else UT.NthPattern(n),
                                               		1, 
                                               		#TIMING L,
													#LINETYPE L,
													#THICK L)
              		in
              		(
               			DSStr_MakeNodeIntoRgn {obj = !(#FstId InitNod), 
											   parent = !(#AxeRgnId L)};
               			#DL L := UT.Insert(n,!(#DL L),InitNod)
              		)
              		end
       			else 
				(
					#DL L := UT.Insert(n,!(#DL L),
									   if pattern then 
										LI.draw_cline(nline, X, Y, pat, 
													  !(#FstId nline),
													 drawline)
									   else 
										LI.draw_line(nline, X, Y, 
													 !(#FstId (nline)), 
													 drawline))
				);
       			L
			)
     		end

	fun UpdateLine(nil, pattern, pat, L) = ()
	 |	UpdateLine((n, x, y, drawline)::vl, pattern, pat, L) =
	(
		let val X = Xold(x, L)
			and Y = Yold(y, L)
		in
			updXY_line(n, X, Y, L, pattern, pat, drawline)
		end;

		UpdateLine(vl, pattern, pat, L)
	)

  	fun upd_line (n, x, y, drawline, L : real LINECHART) =
    	(
    		UpdatePlotSize(L);
			UpdatePlotPosition(L);

    		ResizeX(x, x, L);
    		ResizeY(y, y, L);

			UpdateLine([(n, x, y, drawline)], false, 1, L);

    		L
    	)


  	fun upd_pline (n, x, y, drawline, L : real LINECHART, pat) =
    	(
    		UpdatePlotSize(L);
			UpdatePlotPosition(L);

    		ResizeX(x, x, L);
    		ResizeY(y, y, L);

    		(*let val X = Xold(x, L)
        		and Y = Yold(y, L)
     		in 
				updXY_line(n, X, Y, L, true, pat, drawline)
     		end;*)

			UpdateLine([(n, x, y, drawline)], true, pat, L);

    		L
    	)


	fun findtrues (nil) = nil
	 |	findtrues ((ln, x, y, drawline, cond)::vl) =
	(
		if cond then 
			(ln, x, y, drawline)::findtrues(vl)
		else
			findtrues(vl)
	)

	fun hfindxmin (nil, xmin) = xmin
	 |	hfindxmin ((ln, x, y, drawline)::vl, xmin: real) =
	(
		if x < xmin then
			hfindxmin(vl, x)
		else
			hfindxmin(vl, xmin)
	)

	fun findxmin (nil) = 0.0
	 |	findxmin ((ln, x, y, drawline)::vl) =
	(
		hfindxmin(vl, x)
	)

	fun hfindymin (nil, ymin) = ymin
	 |	hfindymin ((ln, x, y, drawline)::vl, ymin: real) =
	(
		if y < ymin then
			hfindymin(vl, y)
		else
			hfindymin(vl, ymin)
	)
	
	fun findymin (nil) = 0.0
	 |	findymin ((ln, x, y, drawline)::vl) =
	(
		hfindymin(vl, y)
	)

	fun hfindxmax (nil, xmax) = xmax
	 |	hfindxmax ((ln, x, y, drawline)::vl, xmax: real) =
	(
		if x > xmax then
			hfindxmax(vl, x)
		else
			hfindxmax(vl, xmax)
	)

	fun findxmax (nil) = 0.0
	 |	findxmax ((ln, x, y, drawline)::vl) =
	(
		hfindxmax(vl, x)
	)

	fun hfindymax (nil, ymax) = ymax
	 |	hfindymax ((ln, x, y, drawline)::vl, ymax: real) =
	(
		if y > ymax then
			hfindymax(vl, y)
		else
			hfindymax(vl, ymax)
	)

	fun findymax (nil) = 0.0
	 | 	findymax ((ln, x, y, drawline)::vl) =
	(
		hfindymax (vl, y)
	)

	fun upd_chart (L : real LINECHART, vl) =
	    (UpdatePlotSize(L);
	     UpdatePlotPosition(L);
	     case findtrues(vl) of
		 nil => ()
	       | nvalues => 
		     (ResizeX(findxmin(nvalues), findxmax(nvalues), L);
		      ResizeY(findymin(nvalues), findymax(nvalues), L);
		      UpdateLine(nvalues, false, 1, L));
	     L)

 	local val CNX = 0 (* Chart Node X and Y coordonates *)
		  and CNY = 0
          and graph1 = ref EmptyLinechart
          and legend1 = ref LE.EmptyLegend
	in 

(* 
	nline : number of lines
	boxsize : size of the boxes part of the line 
	timing: true if time-series graph
	gridvisible: true if ticks have the size of the graph, the graph looks 
			  like a matrix
	x,y,w,h: coordinates and size of the line graph box surrounding the graph
	xmin,xmax,ymin,ymax: value of the minimum and maximum values in the 
						 x and y coordinates
	xcent,ycent: value of the center
	XMinTick,XMaxTick,YMinTick,YMaxTick: number of ticks on the x and y 
										 coordinates
	Legend: true if legend region displayed
*)


	fun create {title = title, page = pg, nline = nline, boxsize = boxsize,
				timing = timing, gridvisible = gridvisible,
				x = x, y = y, w = w, h = h, axis = axis,  
				xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
				xcent = xcent, ycent = ycent, thick = thick, 
				linetype = linetype, Legend = Legend,
				Rescale = Rescale, GridDyn = GridDyn, XIdealGN = XIdealGN,
				YIdealGN = YIdealGN, XDist = XDist, YDist = YDist,
				Clear = Clear, InitToOrigin = InitToOrigin,
			    StartAtOrigin = StartAtOrigin}=
			let val LRX = x + (w * 3 div 8)
        		and LRW = (20 * w) div 100
               	and PRX = (	if Legend (*moves x-coord (9/80 * w) *)
							then 
								x - UT.round(real(9 * w) / 80.0) 
                           else 
								x  (*- ((5 * w) div 100)*) )
               	and PRW = (if Legend then (52 * w) div 100 
                           else (67 * w) div 100)
              	and PRH = ((70 * h) div 100)
				and PRY = (if title then
								y + (h div 50)
						   else
								y)
               	and PatternSize = 10
				and graph1 = ref EmptyLinechart
				and legend1 = ref LE.EmptyLegend
				and title1 = if title then
								ref(LC.CreateTitle(pg, x, y, h))
							 else ref 0
				and CNI = DSStr_CreateNode{page = pg, x = x, y = y, 
										   	   w = w, h = h, shape = 1} 
			in 
			( 
				if Legend 
				then  
				(
					legend1 := LE.CreateLegendRegion (pg, LRX, y, LRW, 
													  PatternSize, nline);

					LE.upd_rtag (
						(!legend1), 
						LC.CreateIndexStrList(
							"pname", 1, 
							(length(#PatternList (!legend1)) + 1)));

					DSStr_MakeNodeIntoRgn { obj = ! (# Id (!legend1)), 
											parent = CNI }
             	)
               	else ();

				DSWtAttr_LineThickness { obj = CNI, thick = 2 };

				DSWtAttr_LineType { obj = CNI, line = 5 };


         		graph1 := create_graph (CNI, pg, nline, boxsize,
										timing, linetype, thick, gridvisible,
										axis, PRX, PRY, PRW, PRH, 
										xmin, xmax, ymin, ymax, xcent, 
										ycent, legend1, Rescale, GridDyn,
										XIdealGN, YIdealGN, XDist, YDist,
										title1, Clear, InitToOrigin,
										StartAtOrigin);

				DSStr_MakeNodeIntoRgn { obj = !title1, parent = CNI };

				(!graph1)
       		)
      		end


	fun LC'create { Legend = Legend, AxisName = axis,  
					TicksOnly = TicksOnly, BoxSize = boxsize, 
					height = h, NoOfLines = nline, 
					LineThick = thick, HorizVert = timing, Title = title, 
					width = w, x = x, XOrigin = xcent, XMax = xmax, 
					XMin = xmin, y = y, YOrigin = ycent, YMax = ymax, 
					YMin = ymin, RescaleAxis = Rescale,
					FixedNo = GridDyn, XIdealGN = XIdealGN, 
					YIdealGN = YIdealGN, XDist = XDist, YDist = YDist,
					ClearChart = Clear, InitToOrigin = InitToOrigin,
					StartAtOrigin = StartAtOrigin } =

			create {title = title, page = DSStr_GetCurPage(),
					nline = nline, boxsize = boxsize, timing = timing,
					gridvisible = not(TicksOnly), 
					x = x, y = y, w = w, h = h, axis = axis, 
					xmin = xmin, xmax = xmax, ymin = ymin, 
					ymax = ymax, xcent = xcent, ycent = ycent, 
					thick = thick, linetype = 5,
					Legend = Legend, Rescale = Rescale, GridDyn = GridDyn,
					XIdealGN = XIdealGN, YIdealGN = YIdealGN, XDist = XDist,
					YDist = YDist, Clear = Clear, InitToOrigin = InitToOrigin,
					StartAtOrigin = StartAtOrigin}



	fun LC'clear ( graph: real LINECHART ref ) =
			(graph := clear_chart(!graph))


	fun CreateValueList (x, y, drawline, cond, 0) = []
	 |	CreateValueList (x: real, y: real, drawline: bool, cond: bool, n) =
	(
		(n, x, y, drawline, cond)::
			CreateValueList (x, y, drawline, cond, (n-1))
	)
	
	fun LC'upd_line { name = graph: real LINECHART ref, line = n, 
					  x = x, y = y, drawline = drawline } = 
			(graph := upd_line (n, x, y, drawline, !graph))

	fun LC'upd_chart { name = graph: real LINECHART ref, values = values } = 
			(graph := upd_chart(!graph, values))

	fun LC'init ( graph: real LINECHART ref ) =
	(
		if (#Clear (!graph)) then
			LC'clear ( graph )
		else
		(
			if (#InitToOrigin (!graph)) then
				LC'upd_chart { name = graph, 
							   values = CreateValueList (
											!(#Xn (!graph)), !(#Yn (!graph)),
											false, true, 
											length(!(#DL (!graph)))) }
			else ()
		)
	) 

	fun LC'upd_pline { name = graph: real LINECHART ref, line = n, x = x, 
					   y = y, drawline = drawline, pattern = pat } = 
			(graph := upd_pline (n, x, y, drawline, !graph, pat))

	fun LC'upd_ltag { name = graph: real LINECHART ref, tags = l } = 
			LE.upd_rtag (!(#Legend (!graph)), l)

	fun LC'upd_atag { name = graph: real LINECHART ref, x = x, y = y } = 
			upd_atag ([x,y], !graph)

	fun LC'upd_title { name = graph: real LINECHART ref, title = title} =
			upd_title(!graph, title)

	fun LC'delete ( name : real LINECHART ref ) = 
			DSStr_DeleteObject (!(# AxeRgnId (!name)))

end
end;


fun LC'decint () = ref (ILC.EmptyLinechart);
fun LC'decreal () = ref (RLC.EmptyLinechart);



(*
_overload LC'create: {  	Legend: bool, AxisName: bool,  
						TicksOnly: bool, BoxSize: int, 
						height: int, NoOfLines: int, 
						LineThick: int, HorizVert: bool, Title: bool, 
						width: int, x: int, XOrigin: 'a, XMax: 'a, 
						XMin: 'a, y: int, YOrigin: 'a, YMax: 'a, 
						YMin: 'a, RescaleAxis: bool,
						FixedNo: bool, XIdealGN: int, 
						YIdealGN: int, XDist: 'a, YDist: 'a,
						ClearChart: bool, InitToOrigin: bool,
						StartAtOrigin: bool 
					} -> 'a LINECHART
				as ILC.LC'create and RLC.LC'create;

_overload LC'clear: ( 'a LINECHART ref ) -> unit
				as ILC.LC'clear and RLC.LC'clear;

_overload LC'init: ( 'a LINECHART ref ) -> unit
				as ILC.LC'init and RLC.LC'init;

_overload LC'upd_line: { name: 'a LINECHART ref, line: int, x: 'a, y: 'a, 
						drawline: bool } -> unit
				as ILC.LC'upd_line and RLC.LC'upd_line;

_overload LC'upd_chart: { name: 'a LINECHART ref, 
						 values: (int * 'a * 'a * bool * bool) list } -> unit
				as ILC.LC'upd_chart and RLC.LC'upd_chart;

_overload LC'upd_pline: { name: 'a LINECHART ref, line: int, x: 'a, y: 'a, 
						drawline: bool, pattern: int } -> unit
				as ILC.LC'upd_pline and RLC.LC'upd_pline;

_overload LC'upd_ltag: { name: 'a LINECHART ref, tags: string list } -> unit
				as ILC.LC'upd_ltag and RLC.LC'upd_ltag;

_overload LC'upd_atag: { name: 'a LINECHART ref, x: string, y: string } -> unit
				as ILC.LC'upd_atag and RLC.LC'upd_atag;

_overload LC'upd_title: { name: 'a LINECHART ref , title: string } -> unit
				as ILC.LC'upd_title and RLC.LC'upd_title;

_overload LC'delete: ('a LINECHART ref ) -> unit
				as ILC.LC'delete and RLC.LC'delete;
*)
