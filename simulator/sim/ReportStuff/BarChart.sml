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
(* File: BarChart.sml
 *
 * Barchart facilities
 *)


val rcsid =  "$Header: /users/cpntools/repository/cpn2000/sml/sim/ReportStuff/BarChart.sml,v 1.1.1.1 2001/10/02 11:35:28 cpn Exp $";

(*    RECORD COMPONENTS
	Pg :			Page on which the chart is displayed
	CurLine : 		For history chart, the current line number
	RowTag : 		Bar name region ID list 
	PValTag : 		Positive value region value list
	PValTagId : 	Positive value region ID list   
	NValTag : 		Negative value region value list
	NValTagId : 	Negative value region ID list
	GridList : 		Grid line ID list 
	Upper : 		Upper value region ID list 
	Lower : 		Lower value region ID list 
   	PRowList : 		List list of each of the "positive" bar's ID number
	NRowList :  	List list of each of the "negative" bar's ID number
   	PRI : 			Plot Region's ID
	PGN :			Number of grid lines in the "positive" part of the chart 
	NGN :			Number of grid lines in the "negative" part of the chart 
   	IdealGN : 		The grid line number the user has asked for
	Dist :			The fixed distance between grid lines - if any
	PRX :  			Plot region x coordinate
	PRY :  			Plot region y coordinate
	PRW : 			Plot region's width
	PRH : 			Plot region's height
   	BRH : 			Bar Regions 's Height
	MaxValue :		Maximum value
	MinValue :		Minimum value
	StartMax :		The initial max value
	StartMin :		The initial min value
	GridVisible : 	Flag, true means the whole grid lines are visible 
	GridDyn : 		Flag, true means grid dist are dynamic
	Axis :			Chart axis ID
	Legend :		Legend ID
	Title :			Title region ID
	Move :			The number of rows which will be move up when 
					overflow in a history chart in percentage of rows
*)

type 'a BARCHART = {Pg : int ref, 
					CurLine : int ref, 
					RowTag : int list ref,
					PValTag : 'a list ref,   (*int list ref,*)
					PValTagId : int list ref,  
					NValTag : 'a list ref,   (*int list ref,*)
					NValTagId : int list ref,  
					GridList : int list ref, 
					Upper : int list ref, 
					Lower : int list ref, 
					PRowList : (int * 'a) list list ref, 
					NRowList : (int * 'a) list list ref, 
					PRI : int ref, 
					PGN : int ref,
					NGN : int ref,
					IdealGN : int,
					Dist : 'a,
					PRX :  int ref, 
					PRY :  int ref,
					PRW : int ref,
					PRH : int ref,
					BRH : int ref, 
					MaxValue : 'a ref,
					MinValue : 'a ref,
					StartMax : 'a,
					StartMin : 'a, 
					GridVisible : bool, 
					GridDyn : bool,
					Axis : int ref,
					Legend : LegendStr.LEGEND ref,
					Title : int ref,
					Move : int};


structure IntUpdateStr   =
	struct
	structure UT = UtilStr;
	structure LE = LegendStr;

	(* 	Signature
  		sig
  			exception ErrorReturnListRow
    		exception ErrorReturnRow
    		exception bad_rows
    		exception move_larger_than_row
    		exception wrong_last_arg
    		val CheckInfLimit : int * int * int list * int -> bool
    		val CheckSupLimit : int * int * int list * int -> bool
    		val CreateGridLines : int * int * int * int * int * int * int 
									-> int list
    		val CreateIndexStrList : string * int * int -> string list
    		val CreateStringList : 'a * int -> 'a list
    		val CreateTitle : int * int * int * int -> int
    		val DoResizeGridTag : int * int * int * int BARCHART -> unit
    		val EmptyBarchart : int BARCHART
    		val InitOneRow : (int * 'a) list * int -> unit
    		val InitRow : int * int BARCHART -> unit
    		val InitRows : (int * 'a) list list * int -> unit
    		val InitValTags : int list -> unit
    		val PNRowMerge : (int * int) list * (int * int) list 
								-> (int * int) list
    		val RemoveRow : int * int * int * int BARCHART -> unit
    		val ResizeGridTag : int * int * int * int BARCHART -> unit
    		val ReturnListRow : int * 'a list * ('b * 'a) list list 
									-> 'a list list
    		val ReturnRow : int * 'a * ('b * 'a) list -> 'a list
    		val ShrinkBars : (int * int) list list * int BARCHART -> unit
    		val ShrinkRow : (int * int) list * int * int BARCHART -> unit
    		val SplitList : int list * int list * int list 
							-> int list * int list
    		val UpdEveryRow : int * int list list * int BARCHART -> unit
    		val UpdateBarHeigths : int BARCHART -> unit
    		val UpdateGrid : int BARCHART -> unit
    		val UpdateLower : int BARCHART * int -> unit
    		val UpdatePage : int BARCHART -> unit
    		val UpdatePlotPosition : int BARCHART -> unit
    		val UpdatePlotSize : int BARCHART -> unit
    		val UpdateUpper : int BARCHART * int -> unit
    		val create_bc : int -> int -> int -> int -> int -> int -> int -> 
							int -> bool -> bool -> bool -> bool -> bool -> 
							bool -> bool -> int -> int -> int -> int ->
							LegendStr.LEGEND ref -> int ref -> int -> int -> 
							int BARCHART
    		val create_grids : int * int * int * int * int * int * int * 
							   int * int * int * int * int * int * bool * 
							   bool * bool -> int list
    		val create_row : int * int * int * int * int * int * int * int * 
							 int * int -> (int * int) list
    		val create_rows : int * int * int * int * int * int * int * int * 
							  int * int -> (int * int) list list
    		val create_ulvalue : int * int * int * int * int * int * int * 
								 int * int * int * int * int -> int list
    		val find_large : int * int * int * int list -> int
    		val find_row : int * int list * (int * int) list list * 
						   int BARCHART -> (int * int) list list
    		val find_small : int * int * int * int list -> int
    		val find_sum_row : int * int list * int list * int list -> int list
    		val hist_chart : (int list * bool) list * int BARCHART -> unit
    		val hist_row : int list * int BARCHART -> unit
    		val init_all : int BARCHART * int -> unit
    		val init_bc : int BARCHART -> unit
    		val pick_id : 'a * 'b -> 'a
    		val sum_rows : int list -> int
    		val upd_chart : (int * int list * bool) list * int BARCHART -> unit
    		val upd_col : int * int list * int BARCHART -> unit
    		val upd_ltag : int BARCHART * string list -> unit
    		val upd_row : int * int list * int BARCHART -> unit
    		val upd_title : int BARCHART * string -> unit
    		val update_row : int list * (int * int) list * int * bool * 
							 int BARCHART -> (int * int) list
  		end
	*)

	val EmptyBarchart = {Pg = ref 0,
						 CurLine = ref 0, 
						 RowTag = ref nil,
						 PValTag  =  ref nil,
						 PValTagId = ref nil,  
						 NValTag  =  ref nil,
						 NValTagId = ref nil,  
						 GridList = ref nil, 
						 Upper = ref nil, 
						 Lower = ref nil,
						 PRowList = ref nil,  
						 NRowList = ref nil,
						 PRI = ref 0,						 
						 PGN = ref 0,
						 NGN = ref 0, 
						 IdealGN = 0,
						 Dist = 0,
						 PRX = ref 0, 
						 PRY = ref 0,
						 PRW = ref 0, 
						 PRH = ref 0,
						 BRH = ref 0, 
						 MaxValue = ref 0,
						 MinValue = ref 0,
						 StartMax = 0,
						 StartMin = 0,
						 GridVisible = true, 
						 GridDyn = true, 
						 Axis = ref 0,
						 Legend = ref LE.EmptyLegend,
						 Title = ref 0,
						 Move = 0} : int BARCHART;

	local 	

	(* 	when the maximum value changes dynamically, reduc is the 
		percentage of the new maximum value where the maximal bar 
		will be displayed *)

		val  Reduc : real ref = ref (75.0 / 100.0) 

		(* grow is the same on the other way, when a graph is expanded *)

		and Grow : real ref = ref (25.0 / 100.0)

		(* this is the percentage of rows removed in a history chart *)

		and HistRemove : real ref = ref (25.0 / 100.0)
	in

		exception bad_rows;
		exception wrong_last_arg;
		exception ErrorReturnListRow;
		exception ErrorReturnRow;
		exception move_larger_than_row;




	(*               CREATION OF THE BARCHART               *)


	(* 	CreateGridLines will create 'n' grid lines	*)

	fun CreateGridLines (pg, x, y, w, h, 0, pri) = nil
	 |	CreateGridLines (pg, x, y, w, h, n, pri) =
			let val line = DSStr_CreateLine{page = pg, 
											points = [
												 x, 
											 y - UT.round(real(h) / 2.0),
											 x, 
											 y + UT.round(real(h) / 2.0)]}
			in
			(
				DSStr_MakeNodeIntoRgn { obj = line, parent = pri };
				line::CreateGridLines (pg, x+w, y, w, h, (n-1), pri)
			)
			end;


	(* 		create_row will create one bar part 	*)

	fun	create_row (pg, x, y, 0, Ccol, BRH, PRI, PRW, parent, aligntype) = nil
	|	create_row (pg, x, y, col, Ccol, BRH, PRI, PRW, parent, aligntype) =
			let val nod =
				DSStr_CreateNode {page = pg, x = x, y = y, w = (PRW div Ccol), 
								  h = BRH, shape = 1}
			in
			(
				DSStr_MakeNodeIntoRgn {obj = nod, parent = PRI};

				DSUI_Align{obj = nod, aligntype = aligntype, 
						   ref1=parent, ref2 = 0};

				DSWtAttr_ObjectFillType {obj = nod, 
                                         fill= UT.NthPattern (Ccol - col + 1)};

				(nod,0)::create_row (pg, x, y, col - 1, Ccol, BRH, 
									 PRI, PRW, nod, aligntype)
			)
			end;


	(* 	
		create_rows will create Ccol row parts and align them 
		to each other with aligntype
	*)

	fun	create_rows (pg, x, y, 0, col, BarSpace, BRH, PRI, 
					 PRW, aligntype) = nil
	|	create_rows (pg, x, y, row, col, BarSpace, BRH, PRI, 
					 PRW, aligntype) =
			create_row (pg, x, y, col, col, BRH, PRI, PRW, PRI, aligntype)::
				create_rows(pg, x, y + BRH + BarSpace, row - 1,
							col, BarSpace, BRH, PRI, PRW, aligntype)

	
	(* 
		Creates 'PosNgn' grid lines at the right of the axis and NegNgn
		at the left
	*)

	fun create_grids (pg, PRID, Xcoord, Ycoord, Width, Height, 
					  NMax, NMin, MaxWidth, MinWidth, PosNgn, NegNgn,ngn, 
					  GridVisible, Lower, Upper) = 
	(
		(if (NMax > 0)
		 then
		 (	
			if GridVisible then
			(
				CreateGridLines (
					pg, 
					Xcoord + 
					UT.round(
						real(((2 + ngn) * MinWidth)+ ((2 - ngn) * MaxWidth)) / 
						real(2 * ngn)),
					Ycoord, 
					(Width div ngn), 
					Height, 
					(PosNgn - 1), PRID)
			)
			else
			(
				(if Lower then
				 (
					CreateGridLines (
						pg, 
						Xcoord + 
						UT.round(real(((2 + ngn) * MinWidth)+ 
								 	  ((2 - ngn) * MaxWidth)) / 
								 real(2 * ngn)),
						Ycoord + (Height div 2) - 2, 
						(Width div ngn), 
						4, (PosNgn - 1), PRID)
				 )
				 else nil)@
				(if Upper then
				 (
					CreateGridLines (
						pg, 
						Xcoord + 
						UT.round(real(((2 + ngn) * MinWidth)+ 
									  ((2 - ngn) * MaxWidth)) / 
								 real(2 * ngn)),
						Ycoord - (Height div 2) + 2, 
						(Width div ngn), 
						4, (PosNgn - 1), PRID)
				 )
				 else nil)
			)
		 )
		 else nil
		)@
		(if (NMin < 0)
		 then
		 (
			if GridVisible then
		 	(
				CreateGridLines(
					pg, 
					Xcoord + 
					UT.round(real(((ngn - 2) * MinWidth) - 
								  ((ngn + 2) * MaxWidth)) / 
							 real(2 * ngn)),
					Ycoord, 
					(~(Width div ngn)), 
					Height, (NegNgn - 1), PRID)
			)
			else
			(
				(if Lower then
				 (
					CreateGridLines(
						pg, 
						Xcoord + 
						UT.round(real(((ngn - 2) * MinWidth) - 
									  ((ngn + 2) * MaxWidth)) / 
								 real(2 * ngn)),
						Ycoord + (Height div 2) - 2, 
						(~(Width div ngn)), 
						4, (NegNgn - 1), PRID)
				 )
				 else nil
				)@
				(if Upper then
				 (
					CreateGridLines(
						pg, 
						Xcoord + 
						UT.round(real(((ngn - 2) * MinWidth) - 
									  ((ngn + 2) * MaxWidth)) / 
								 real(2 * ngn)),
						Ycoord - (Height div 2) + 2, 
						(~(Width div ngn)), 
						4, (NegNgn - 1), PRID)
				 )
				 else nil
				)
			)
		 )
		 else nil
		)
	)


	(* 	
		Creates upper or lower value regions with a distance 'TagDist'	
		such that there are 'NegNgn' tags at the left of the axis and
		'PosNgn' at the right. A tag is created at the axis
	*)

	fun create_ulvalue (pg, PRID, Xcoord, Y, W, NMin,
					    MaxWidth, MinWidth, PosNgn, NegNgn, ngn, TagDist) =
	(
		let val ugt = 
			UT.CreateTagRegion (
					pg, 
					((Xcoord +
					 UT.round(real(((ngn - 2) * MinWidth) - 
								   ((ngn + 2) * MaxWidth)) / real(2 * ngn))) - 
					((NegNgn - 1) * W) - 15),
					Y, NegNgn, W, PRID, true)@
			UT.CreateTagRegion (
					pg, 
					Xcoord + UT.round(real(MinWidth - MaxWidth) / 2.0),
					Y, 1, W, PRID, true)@ 
			UT.CreateTagRegion (
					pg, 
					Xcoord +
					UT.round(real(((2 + ngn) * MinWidth) + 
								  ((2 - ngn) * MaxWidth)) / real(2 * ngn)),
					Y, PosNgn, W, PRID, true)
		in 
		(
			UT.UpdateTag'i (ugt, UT.CreateList (NMin, TagDist, ngn + 1));
			ugt 
		)
		end
	)


	(*		Initialize positive and negative value tags	*)

	fun InitValTags (nil) = ()
	 |	InitValTags ((id::idlist): int list) =
	(
		DSText_Put {obj= id, text = " "};

		InitValTags (idlist)
	)


	(*	
		Creates a list of strings: [s1, s2, ..., sm] where s
		is a string
	*)

	fun CreateIndexStrList(s, i, m) =
	(
		if (i = m) then 
			nil
		else
			(s^Int.toString(i))::CreateIndexStrList(s, i+1, m)
	)


	(*
		Creates a title region for a chart with the position 
		'CNX', 'CNY' and with the height 'CHN'. The title 
		region is created with the default text "Title"
	*)

	fun CreateTitle (pg, CNX, CNY, CNH) =
	(
		let val TitelRegion = (DSStr_CreateLabel{
									page = pg, 
									x = (CNX - (String.size ("Title") * 5)),
							    	y = (CNY - (CNH div 2) + 10),
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
	parameter in the creation of a bar chart:

	pg: 			page where the graph is created
	row: 			number of rows
	col: 			number of columns
	Xcoord: 		x coordinate of the plot region
	Ycoord: 		y coordinate of the plot region
	Width: 			width of the plot region
	Height: 		height of the plot region
	BRH: 			bar region height
	Expr: 			flag, true means to display row regions
	NValue: 		flag, true means to display negative value regions
	PValue: 		flag, true means to display positive value regions
	Upper: 			flag, true means to display upper value regions
	Lower: 			flag, true means to display lower value regions
	GridDyn: 		flag, true means that the grid dist dynamically changes 
					when the maximum value changes. E.g. the number of grids
					is not changed more than 50% from gn.
	GridVisible: 	flag, true means to display the full grid lines. 
					False means that only small ticks are displayed 
					where there are value regions.
	max: 			maximum value initially displayed
	gn: 			ideal number of grid lines initial
*)

	fun	create_bc pg row col Xcoord Ycoord Width Height BRH Expr 
				  NValue PValue Upper Lower GridDyn GridVisible Max Min
				  gn dist legend title move CNW =

	let val BarSpace = (Height - (row * BRH)) div (row + 1)
		and PRID = DSStr_CreateNode {page = pg, x = Xcoord, y = Ycoord,
				                     w = Width, h = Height, shape = 1}
		and TagDist = if GridDyn then UT.CalcDist((Max - Min), gn)
					  else dist
	in
	(
		let val PosNgn = if GridDyn then 
								UT.CalcGN(Max, UT.round(real(Max * gn) / 
												 real(Max - Min)), TagDist)
						 else UT.ceiling(real(Max) / real(TagDist))
			and NegNgn = if GridDyn then
							UT.CalcGN(~Min, gn - UT.round(real(Max * gn) / 
										 real(Max - Min)), TagDist)
						 else UT.ceiling(real(~Min) / real(TagDist))
		in
		(
			let val PosNgn =
				if ((PosNgn = 0) andalso (NegNgn = 0)) then 1
				else PosNgn
			in
			(
				let val NMin = ~(TagDist * NegNgn)
					and NMax = TagDist * PosNgn
				in
				(
					let val MinWidth = UT.round(real(Width * (~NMin)) / 
												real((NMax - NMin)))
						and MaxWidth = (Width - 
									    UT.round(real(Width * (~NMin)) / 
												 real((NMax - NMin))))
					in
					(
						let val Axis = DSStr_CreateNode {
											page = pg, 
											x = Xcoord + 
												UT.round(
					    							real(MinWidth - MaxWidth) /
													2.0), 
											y = Ycoord, w = 1,
											h = Height, shape = 1}
							and ngn = PosNgn + NegNgn
						in
						(
							DSWtAttr_ObjectFlags {obj = Axis, flag = 128, 
										  		  value = true};

							DSStr_MakeNodeIntoRgn {obj = Axis, parent = PRID};

				   			{
		 	 				Pg = ref pg,

		 	 				CurLine = ref 0,

		 	 				RowTag = 
								(if Expr then
									let val TagIdList =
										(UT.CreateTagRegion
											(pg, 
										 	 Xcoord - (Width div 2) - 
											 UT.round(
												real(5 * CNW) / 100.0) - 20 - 
											 (if NValue then 20 else 0), 
										 	 Ycoord - (Height div 2) + 
										 	 (BRH div 2) + BarSpace - 5, 
										  	 row, BarSpace + BRH, PRID, false))
									in
										UT.UpdateTag's(
											TagIdList, 
											CreateIndexStrList(
												"bar", 1, 
												(length(TagIdList)+1)));
										ref TagIdList
									end
							 	else ref nil),

							PValTagId =	
								(if PValue then
									let val TagIdList =
										(UT.CreateTagRegion 
											(pg, Xcoord + (Width div 2) +
											UT.round(real(2 * CNW) / 100.0),
						 				 	Ycoord - (Height div 2) +  
										 	(BRH div 2) + BarSpace - 5, row,
						 				 	BarSpace + BRH, PRID, false))
									in
										UT.UpdateTag's(
											TagIdList, 
											CreateIndexStrList(
												"pval", 1, 
												(length(TagIdList)+1)));
										ref TagIdList
									end
							 	else ref nil),

							PValTag =	
								(if PValue then 
									ref (UT.CreateList(~1, 0, row)) 
							 	 else ref nil),

							NValTagId =
								(if NValue then
									let val TagIdList =
										(UT.CreateTagRegion
											(pg, (Xcoord - (Width div 2)) - 
											UT.round(real(2 * CNW) / 100.0) - 
											20,
						 				 	Ycoord - (Height div 2) +  
										 	(BRH div 2) + BarSpace - 5, row,
						 				 	BarSpace + BRH, PRID, false))
									in
										UT.UpdateTag's(
											TagIdList, 
											CreateIndexStrList(
												"nval", 1, 
												(length(TagIdList) + 1)));
										ref TagIdList
									end
							 	else ref nil),

							NValTag =
								(if NValue then
									ref (UT.CreateList(1, ~0, row))
							 	else ref nil),

							GridList =	(
								ref(create_grids (
									pg, PRID, Xcoord, Ycoord, Width, Height, 
					  				NMax, NMin, MaxWidth, MinWidth, 
									PosNgn, NegNgn,ngn, 
					  				GridVisible, Lower, Upper))),

							Upper =	if Upper then
										ref(create_ulvalue (
											pg, PRID, Xcoord, 
											(Ycoord - (Height div 2) - 15), 
											(Width div ngn), NMin,
					  						MaxWidth, MinWidth, PosNgn, 
											NegNgn, ngn, TagDist))
									else ref nil,

							Lower =	if Lower then
										ref(create_ulvalue(
											pg, PRID, Xcoord, 
											(Ycoord + (Height div 2) + 15), 
											(Width div ngn), NMin,
					    					MaxWidth, MinWidth, PosNgn, 
											NegNgn, ngn, TagDist))
									else ref nil,

							NRowList =	
								ref(create_rows 
										(pg, Xcoord + 
									 	UT.round(real(MinWidth - MaxWidth) / 
												 2.0),
									 	Ycoord - (Height div 2) + 
									 	(BRH div 2) + BarSpace, 
									 	row, col, BarSpace, BRH, Axis,
									 	MinWidth, ALN_RL)),

							PRowList = 
								ref(create_rows 
										(pg, Xcoord + 
								 	 	 UT.round(real(MinWidth - MaxWidth) / 
												  2.0),
								 	 	Ycoord - (Height div 2) + 
									 	(BRH div 2) + BarSpace, 
									 	row, col, BarSpace, BRH, Axis,
									 	MaxWidth, ALN_LR)),
	
							PRI = ref PRID,

		 	 				PGN = ref PosNgn,

							NGN = ref NegNgn,

							IdealGN = gn,

							Dist = dist,

		 	 				PRX = ref Xcoord,

		 	 				PRY = ref Ycoord,

		 	 				PRW = ref Width,

		 	 				PRH = ref Height,

		 	 				BRH = ref BRH,

		 	 				MaxValue = ref NMax,

							MinValue = ref NMin,

							StartMax = Max,
					
							StartMin = Min,

		 	 				GridVisible = GridVisible,

		 	 				GridDyn = GridDyn,

							Axis = ref Axis,

							Legend = legend,

							Title = title,

							Move = move

				   			}  :  int BARCHART
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
   

	(*		Initialize one row to be invisible and have width 1		*)

	fun InitOneRow (nil, h) = ()
	 |	InitOneRow (((id, v)::idlist), h) =
	(
		DSWtAttr_AdjustObjectSize{obj = id, w = 1, h = h};


		DSWtAttr_ObjectFlags {obj = id, flag = 1, value = true};

		InitOneRow (idlist, h)
	)


	(*		Initialize rows		*)

	fun InitRows (nil, h) = ()
	 |	InitRows (r1::rlist, h) =
	(
		InitOneRow(r1, h);
		InitRows(rlist, h)
	)


	(*	Creates a list of length m with each entry the string s	*)

	fun CreateStringList (s, 0) = nil
	 |	CreateStringList (s, m) =
			s::CreateStringList (s, m-1)
	

	(*	
		Initialize the bar chart bc, to have no text in negative 
		and positive value regions, and to have invisible row regions
	*)

	fun init_bc (bc : int BARCHART) =
	(
			UT.UpdateTag's(!(#PValTagId bc), 
					   CreateStringList(" ", length(!(#PValTagId bc))));
			UT.UpdateTag's(!(#NValTagId bc), 
					   CreateStringList(" ", length(!(#PValTagId bc))));

			InitRows(!(#PRowList bc), !(#BRH bc));
			InitRows(!(#NRowList bc), !(#BRH bc))
	)


	(*            UPDATING OF THE BARS, AFTER A RESIZE            *)

	(*	Resize rows according to new grid dist	*)

	fun	ShrinkRow (nil, parent, b : int BARCHART) = ()   	
	 |	ShrinkRow ((id, val1)::IdRow, parent, b : int BARCHART) =
        (
			if val1 <> 0 then 
				DSWtAttr_AdjustObjectSize {	
						obj = id, 
						w = UT.round(
								real(abs(val1) * (!(#PGN b) + !(#NGN b)) * 
									(!(#PRW b) div (!(#PGN b) + !(#NGN b)))) / 
								real(!(#MaxValue b) - !(#MinValue b))),
						h = !(#BRH b)}
		 	else ();

			if val1 > 0   then 
				DSUI_Align{ obj = id, aligntype = ALN_LR,
							ref1 = parent, ref2 = 0} 
			else
				if val1 < 0 then
					DSUI_Align{ obj = id, aligntype = ALN_RL,
								ref1 = parent, ref2 = 0}  
				else ();

         	if val1 = 0 
			then 
				ShrinkRow(IdRow, parent, b)
         	else ShrinkRow(IdRow, id, b)
		)


	(*	Resize all rows in the list 'x::l'	*)

	fun	ShrinkBars (nil, b : int BARCHART) = ()
	 |	ShrinkBars (x::l, b : int BARCHART) = 
        (
			ShrinkRow(x, (!(#Axis b)), b);
          	ShrinkBars (l, b)
      	)
            

	(*      UPDATING THE BARS          *)


	fun	update_row (nil, nil, parent, NoChange, b : int BARCHART) = nil
	 |	update_row (val2::new_row, 
					(id, val1)::IdRow, parent, NoChange, b : int BARCHART) =
			if (val2<>val1) then
			(
				if val2 = 0 
				then	
				(

					DSWtAttr_ObjectFlags {obj = id, flag = 1, value = true};

					DSWtAttr_AdjustObjectSize {obj = id, w = 1, h = !(#BRH b)}
				)
				else 
				(
					if val1 = 0 then 

						DSWtAttr_ObjectFlags {obj = id, flag = 1, 
											  value = false}

					else ()
				);
				
				if (val2 < 0) 
				then
				(
					DSWtAttr_AdjustObjectSize {	
						obj = id, 
						w = UT.round(real(val2 * (!(#NGN b) + !(#PGN b)) * 
										 (!(#PRW b) div 
										 (!(#PGN b) + !(#NGN b)))) / 
									 real(!(#MinValue b) - !(#MaxValue b))),
						h = !(#BRH b)};
					DSUI_Align{ obj = id, aligntype = ALN_RL,
								ref1 = parent, ref2 = 0 }
				)
				else
				(
					DSWtAttr_AdjustObjectSize {	
						obj = id, 
						w = UT.round(real(val2 * (!(#PGN b) + !(#NGN b)) * 
										(!(#PRW b) div 
										(!(#PGN b) + !(#NGN b)))) / 
									real(!(#MaxValue b) - !(#MinValue b))),
						h = !(#BRH b)};
					DSUI_Align{ obj = id, aligntype = ALN_LR,
								ref1 = parent, ref2 = 0 }
				);

				if val2 = 0
				then 
					(id, val2)::update_row(new_row, IdRow, parent, false, b)
        		else 
					(id, val2)::update_row(new_row, IdRow, id, false, b)
        	)
    		else 
			(
        		if not(NoChange)
        		then 
            		if (val2 < 0) then
						DSUI_Align{ obj = id, aligntype = ALN_RL,
									ref1 = parent, ref2 = 0 }
					else
						DSUI_Align{ obj = id, aligntype = ALN_LR,
									ref1 = parent, ref2 = 0 }
        		else ();

        		if (val2 = 0)
        		then 
					(id, val2)::update_row(new_row, IdRow, parent, NoChange, b)
		 		else 
					(id, val2)::update_row(new_row, IdRow, id, NoChange, b)
            )
	 |	update_row(_,_,_,_,_) = raise bad_rows


	(* 	Finds the n'th row and updates it with the values in 'new_row'	*)

	fun	find_row (n, new_row, nil, b : int BARCHART) = raise wrong_last_arg
	 |	find_row (n, new_row, x::l, b : int BARCHART) =
			if n = 1 then 
				(update_row (new_row, x, (!(#Axis b)), 
				 			 true, b : int BARCHART))::l
			else 
				x::find_row (n - 1, new_row, l, b)


	(*	Sum the values in the list	*)

	fun	sum_rows (nil) = 0
 	 |	sum_rows (x::l) = (x + sum_rows (l))


	(*
		Gives a list of values where the n'th value is updated 
		to be the sum of the values in 'new_row'. This is used 
		to update the positive and negative value regions
	*)

	fun	find_sum_row (1, new_row, id::l1, value::l2) = 
		let val new_sum = sum_rows (new_row) 
		in
			if new_sum <> value then 
				(DSText_Put {obj = id, text = Int.toString(new_sum)};
				 new_sum::l2)
			else 
				value::l2
			end
 	 |	find_sum_row(n, new_row, id::l1, value::l2) = 
			value::find_sum_row(n-1, new_row, l1, l2)
 	 |	find_sum_row(n, new_row,_,_) = raise wrong_last_arg


	fun	pick_id (id, value) = id


(*_____________________________________________________________________


                             UPDATING GRIDS AND GRID TAGS    

_______________________________________________________________________*)


	fun mem (i, nil) = false
	 |	mem (i, j::l) = (i = j) orelse (mem(i, l))

	(* 	
		Checks that all values in the list 'x::l' are smaller than INF 
		except the n'th value
	*)

	fun	CheckInfLimit(rowlist : int list, n : int, nil, INF : int) = true
	 |	CheckInfLimit(rowlist : int list, n : int, x::l, INF : int)  =
	(
		if (not(mem(n, rowlist))) 
		then 
		(
			if (x < INF) 
			then 
				CheckInfLimit(rowlist, n + 1, l, INF)
			else false
		)
		else CheckInfLimit(rowlist, n + 1, l, INF)
	)

	
	(* 
		Checks that all values in the list 'x::l' are larger than SUP
		except the n'th value 
	*)

	fun	CheckSupLimit(rowlist : int list, n : int, nil, SUP : int) = true
	 |	CheckSupLimit(rowlist : int list, n : int, x::l, SUP : int)  =
	(
		if (not(mem(n, rowlist))) 
		then 
			if (x > SUP) 
			then 
				CheckSupLimit(rowlist, n + 1, l, SUP)
			else false
		else CheckSupLimit(rowlist, n + 1, l, SUP)
	)


	(*		Removes and recreates grid lines		*)

	fun	UpdateGrid(b : int BARCHART) =
		let val MinWidth = UT.round(real(!(#NGN b) * !(#PRW b)) / 
						    real(!(#PGN b) + !(#NGN b)))
			and MaxWidth = !(#PRW b) - UT.round(real(!(#NGN b) * !(#PRW b)) / 
						   				real(!(#PGN b) + !(#NGN b)))
			in
			( 
				UT.DeleteIdList (!(#GridList b));
				(#GridList b) := (
					create_grids (!(#Pg b), !(#PRI b), !(#PRX b), !(#PRY b), 
								  !(#PRW b), !(#PRH b), !(#MaxValue b), 
								  !(#MinValue b), MaxWidth, MinWidth, 
								  !(#PGN b), !(#NGN b), (!(#PGN b) + 
								  !(#NGN b)), (#GridVisible b), 
								  (!(#Lower b) <> nil), 
								  (!(#Upper b) <> nil)))
			)
			end


	(*	Removes and recreates upper value regions	*)

	fun UpdateUpper(b : int BARCHART, Dist : int) =
	(	
		let val MinWidth = UT.round(real(!(#NGN b) * !(#PRW b)) / 
						    		real(!(#PGN b) + !(#NGN b)))
			and MaxWidth = !(#PRW b) - 
						   UT.round(real(!(#NGN b) * !(#PRW b)) / 
						   			real(!(#PGN b) + !(#NGN b)))
		in
		(
			UT.DeleteIdList (!(#Upper b));

			(#Upper b) := create_ulvalue (
								!(#Pg b), !(#PRI b), !(#PRX b), 
								(!(#PRY b) - !(#PRH b) div 2 - 16), 
								(!(#PRW b) div (!(#PGN b) + !(#NGN b))), 
								!(#MinValue b), MaxWidth, MinWidth, 
								!(#PGN b), !(#NGN b), 
								(!(#PGN b) + !(#NGN b)), 
								Dist)
		)
		end
	)


	(*	Removes and recreates upper value regions	*)
	
	fun	UpdateLower(b : int BARCHART, Dist : int) =
	(	
		let val MinWidth = UT.round(real(!(#NGN b) * !(#PRW b)) / 
						    		real(!(#PGN b) + !(#NGN b)))
			and MaxWidth = !(#PRW b) - 
						   UT.round(real(!(#NGN b) * !(#PRW b)) / 
						   			real(!(#PGN b) + !(#NGN b)))
		in
		(
			UT.DeleteIdList (!(#Lower b));

			(#Lower b) := create_ulvalue (
								!(#Pg b), !(#PRI b), !(#PRX b), 
								(!(#PRY b) + !(#PRH b) div 2 + 16), 
								(!(#PRW b) div (!(#PGN b) + !(#NGN b))), 
								!(#MinValue b), MaxWidth, MinWidth, 
								!(#PGN b), !(#NGN b), 
								(!(#PGN b) + !(#NGN b)), 
								Dist)
		)
		end
	)


	(*	Is called when the max value or min value has been changed	*)

	fun DoResizeGridTag (Dist, PosGn, NegGn, b: int BARCHART) =
		(
			(#MaxValue b) := PosGn * Dist;
			(#MinValue b) := ~(NegGn * Dist);

			ShrinkBars (!(#PRowList b), b);
			ShrinkBars (!(#NRowList b), b);

			if ((PosGn <> !(#PGN b)) orelse (NegGn <> !(#NGN b)))
			then
			(
				(#PGN b) := PosGn;
				(#NGN b) := NegGn;

				UpdateGrid(b);	

				DSWtAttr_ObjectPosition{obj = !(#Axis b),
									x = !(#PRX b) + 
										UT.round(real((NegGn - PosGn) * 
													 !(#PRW b)) / 
												(2.0 * real(PosGn + NegGn))),
									y = !(#PRY b)}
			)
			else ();

		
			if !(#Upper b) <> nil
			then 
				UpdateUpper(b, Dist)
			else ();
			
			if !(#Lower b) <> nil
			then 
				UpdateLower(b, Dist)
			else ()
		)


	(*	
		Finds the largest value in the list 'i1::ilist' - except from the
		row'th value
	*)

	fun find_large(rowlist, i, n, nil) = n
	 |  find_large(rowlist, i : int, n : int, i1::ilist) =
			if ((n < i1) andalso (not(mem(i, rowlist))))
			then 
				find_large(rowlist, i+1, i1, ilist)
			else find_large(rowlist, i+1, n, ilist)


	(*
		Finds the smallest value in the list 'i1::ilist' - except from the
		row'th value
	*)

	fun find_small(rowlist, i, n, nil) = n
	 |	find_small(rowlist, i : int, n : int, i1::ilist) =
			if ((n > i1) andalso (not(mem(i, rowlist))))
			then
				find_small(rowlist, i+1, i1, ilist)
			else find_small(rowlist, i+1, n, ilist)


	(*	
		Checks if the min or max value is to be changed - and if 
		recreates the grid lines and the upper and lower value regions
	*)

	fun	ResizeGridTag(rowlist, psum, nsum, b : int BARCHART) =
		let val INF = UT.round (!Grow * real(!(#MaxValue b))) 
			and SUP = UT.round (!Grow * real(!(#MinValue b)))
		in
		(
			let val NMaxV = 
				(
					if (psum > !(#MaxValue b))
					then 
						UT.round (real(psum) / !Reduc)
					else 
						if ((psum < INF) andalso 
							CheckInfLimit(rowlist, 1, !(#PValTag b), INF))
						then
							find_large(rowlist, 1, psum, !(#PValTag b))
						else
							!(#MaxValue b)
				)
				and NMinV = 
				(
					if (nsum < !(#MinValue b))
					then
						UT.round (real(nsum) / !Reduc)
					else
						if ((nsum > SUP) andalso 
							CheckSupLimit(rowlist, 1, !(#NValTag b), SUP))  
						then
							find_small(rowlist, 1, nsum, !(#NValTag b))
						else 
							!(#MinValue b)
				)
			in
			(
				if  ((psum > !(#MaxValue b)) orelse 
					 (nsum < !(#MinValue b)) orelse
					 ((psum < INF) andalso 
					   CheckInfLimit(rowlist, 1, !(#PValTag b), INF)) orelse
					 ((nsum > SUP) andalso
					   CheckSupLimit(rowlist, 1, !(#NValTag b), SUP)))
				then
				(
					let val TagDist = if (#GridDyn b) then
										UT.CalcDist((NMaxV - NMinV), 
													(#IdealGN b))
									  else (#Dist b)
					in
					(
						let val PosNgn = 
							(
								if (#GridDyn b) then 
									UT.CalcGN(NMaxV, 
										   UT.round(real(NMaxV * 
														 (#IdealGN b)) /
 											 		real(NMaxV - NMinV)), 
										   TagDist)
								else UT.ceiling(real(NMaxV) / real(TagDist))
							)
							and NegNgn =
							(
								if (#GridDyn b) then 
									UT.CalcGN(~NMinV, 
											(#IdealGN b) - 
											UT.round(real(NMaxV * 
														  ((#IdealGN b))) /
 													 real(NMaxV - NMinV)), 
											TagDist)
								else UT.ceiling(real(~NMinV) / 
												real(TagDist))
							)
						in
						(
							let val PosNgn =
									if ((PosNgn = 0) andalso (NegNgn = 0))
									then 1
									else PosNgn
							in
							(
								DoResizeGridTag (TagDist, PosNgn, NegNgn, b)
							)
							end
						)
						end
					)
					end
				)
				else () (* Do not Resize *)
			)
			end
		)
		end


	(*	
		Updates the entries representing the width and height of the 
		plot region to actually fit the size. There can be 
		inconsistencies if the region has be resized
	*)

	fun	UpdatePlotSize(b : int BARCHART) =
			let val {w, h} = DSRdAttr_GetObjectSize (!(#PRI b))
			in 
			(
				if (w <> !(#PRW b)) 
				then ((#PRW b) := w) 
				else ();
				if (h <> !(#PRH b))  
				then ((#PRH b) := h) 
				else ()
			)
			end


	(*	
		Updates the entries representing the height of the 
		bar regions to actually fit the size. There can be 
		inconsistencies if the region has be resized
	*)

	fun	UpdateBarHeigths(b : int BARCHART) =
		let val {w = w1, h = h1} = 
				DSRdAttr_GetObjectSize(pick_id (hd (hd (!(#PRowList b)))))
		in
			(#BRH b) := h1
		end


	(*	
		Updates the entries representing the x-coordinate and 
		the y-coordinate of the plot region to actually fit the size. 
		There can be inconsistencies if the region has be repositioned
	*)

	fun	UpdatePlotPosition(b : int BARCHART) =
		let val {x = x1, y = y1} = DSRdAttr_GetObjectCenter(!(#PRI b))
		in 
		(
			(#PRX b) := x1;
			(#PRY b) := y1 
		) 
		end


	(*	
		Updates the entry representing the page on which the chart is
		positioned. There can be inconsistencies if the chart has be 
		moved to another page
	*)

	fun	UpdatePage(b : int BARCHART) =
			(#Pg b := UT.ReturnPage (!(#PRI b)))


	(*	
		Splits a list into two lists. One with the positive values 
		from the list and a one with the negative values. The new 
		lists will have the same length as the original, as '0's has 
		been put in instead of the negative values for the list with 
		the positive values and visa versa
	*)

	fun SplitList(nil, plist, nlist) = (plist, nlist)
	 |	SplitList((i1::ilist) : int list, plist, nlist) = 
		if (i1 > 0)
		then
			SplitList(ilist, plist@[i1], nlist@[0])
		else
			SplitList(ilist, plist@[0], nlist@[i1]) 
 


	fun UpdateRows(nil, b) = ()
	 |	UpdateRows((row, pnew_list, nnew_list)::rl, b : int BARCHART) =
	(
		(#PRowList b) := find_row (row, pnew_list, !(#PRowList b), b);
		(#NRowList b) := find_row (row, nnew_list, !(#NRowList b), b); 

		(* update positive and negative value regions - if displayed *)

		if !(#PValTagId b) <> nil  
		then  
			(#PValTag b) := find_sum_row (row, pnew_list, !(#PValTagId b),
										  !(#PValTag b)) 
		else ();

		if !(#NValTagId b) <> nil  
		then  
			(#NValTag b) := find_sum_row (row, nnew_list, !(#NValTagId b),
										  !(#NValTag b)) 
		else ();

		UpdateRows(rl, b)
	)


	(*	Updates the row 'row' with the values listed in 'new_list'	*)

	fun	upd_row (row, new_list, b : int BARCHART) = 
		let val (pnew_list, nnew_list) = SplitList(new_list, nil, nil)
		in
		(
			(*	
				updates the values representing position and size of 
				the plot region and the height of the bar regions
			*)
			UpdatePlotSize(b);
			UpdateBarHeigths(b); 
			UpdatePlotPosition(b);

			(*	if the max value or min value might have changed -
				resize the grids and the tags if necessary
			*)
			if ( ((sum_rows (pnew_list)) > 0) orelse 
				 ((sum_rows (nnew_list)) < 0) ) 
			then 
				ResizeGridTag([row], sum_rows (pnew_list), 
							  sum_rows (nnew_list), b) 
			else ();

			UpdateRows([(row, pnew_list, nnew_list)], b)
		)
		end


	fun findtrues(nil) = nil
	 | 	findtrues((rn, new_list, cond)::pl) =
	(
		if cond then
			let val (plist, nlist) = SplitList(new_list, nil, nil)
			in
			( 
				(rn, plist, nlist)::findtrues(pl)
			)
			end
		else
			findtrues(pl)
	)


	fun findmax(nil, max) = max
	 |	findmax((rn, plist, nlist)::pl, max) =
	(
		if sum_rows (plist) > max then
		(
			findmax(pl, sum_rows (plist))
		)
		else
		(
			findmax(pl, max)
		)
	)


	fun findmin(nil, min) = min
	 |	findmin((rn, plist, nlist)::pl, min) = 
	(
		if sum_rows (nlist) < min then
		(
			findmin(pl, sum_rows (nlist))
		)
		else
		(
			findmin(pl, min)
		)
	)


	fun findrows(nil, rowlist) = rowlist
	 |	findrows((rn, plist, nlist)::ul, rowlist) = 
	(
		findrows(ul, rn::rowlist)
	) 


	fun upd_chart (upd_list, b : int BARCHART) =
	(
		(*	
			updates the values representing position and size of 
			the plot region and the height of the bar regions
		*)

		UpdatePlotSize(b);
		UpdateBarHeigths(b); 
		UpdatePlotPosition(b);

		let val nupd_list = findtrues(upd_list)
		in
		(
			let val max = findmax(nupd_list, 0)
				and min = findmin(nupd_list, 0)
				and rowlist = findrows(nupd_list, nil)
			in
			(
				if (max > 0 orelse min < 0) then
					ResizeGridTag(rowlist, max, min, b)
				else ()
			)
			end; 

			UpdateRows(nupd_list, b)
		)
		end
	)
		
	(*	Updates every row	*)

	fun	UpdEveryRow (row, nil, b : int BARCHART) = ()
	 |	UpdEveryRow (row, x::RowList, b : int BARCHART)  = 
		(
			upd_row (row, x, b); 
			UpdEveryRow (row+1, RowList, b)
		)
    

	(*		Updates the legend tags (the part name regions)		*)

	fun	upd_ltag (b : int BARCHART, l) =
			UT.UpdateTag's (!(#RowTag b), l)


	(*		Updates the title region	*)

	fun upd_title (b : int BARCHART, title) =
			UT.UpdateTag's ([!(#Title b)], [title])


	(* 	
		Returns a list of the second entries of the list '(id, value2)::l' - 
		except that the col'th entry has been replace by value1
	*)

	fun	ReturnRow (_, value1, nil) = nil
	 |	ReturnRow (1, value1, (id, value2)::l) = 
			value1::ReturnRow (0, value1, l)
	 |	ReturnRow (col, value1, (id, value2)::l) = 
			value2::ReturnRow(col - 1, value1, l)


	(*	
		Returns a list of lists equal to lists of second enties in
		the lists in 'r::ListRow' - except that the col'th entry has been
		replaced by the values in 'value::ListCol' 
	*)

	fun	ReturnListRow (col, nil, nil) = nil
	 |	ReturnListRow (col, value::ListCol, r::ListRow) = 
			ReturnRow(col, value, r)::ReturnListRow(col, ListCol, ListRow)
	 |	ReturnListRow (_,_,_) = raise ErrorReturnListRow


	(*	
		Updates the bar part 'col' for every row, with the 
		values listed in 'l'. This is done by - for every bar -
		forming a list from the values that the bar parts have 
		already got but with a new value for the col'th bar part.
		Now all the bars are updated
	*)

	fun	upd_col (col, l, b : int BARCHART) =
			let val (pl, nl) = SplitList(l, nil, nil)
			in
			(
				let val prowlist = ReturnListRow (col, pl, !(#PRowList b))
					and nrowlist = ReturnListRow (col, nl, !(#NRowList b)) 
				in 
				(
					(UpdEveryRow (1, prowlist, b));
					(UpdEveryRow (1, nrowlist, b))
				)
				end
			)
			end


	(* 
		Creates a list which is formed from two lists. When one list 
		has entry '0', the value from the other list will be in the 
		new list
	*)

	fun PNRowMerge(nil, nil) = nil
	 |	PNRowMerge((i1: int, j1: int)::list1, (i2, j2)::list2) =
			if (j1 = 0)
			then (i2, j2)::PNRowMerge(list1, list2)
			else (i1, j1)::PNRowMerge(list1, list2)
	 | PNRowMerge(_,_) = raise bad_rows
    

	fun InitRow(rn, b: int BARCHART) =
		upd_row(rn, UT.CreateList(0, 0, length(hd(!(#PRowList b)))), b)

   	fun RemoveRow(n1, n2, n3, b : int BARCHART) =
			if n1 > n2 then ()
          	else
			(
				(if n1 <= (n2 - n3) then
					upd_row(n1, 
							map UT.Snd (
								PNRowMerge(
									UT.NthElement (n1 + n3, !(#PRowList b)),
									UT.NthElement (n1 + n3, !(#NRowList b)))),
							b)
				else
					upd_row(n1, 
							UT.CreateList(0, 0, length(hd(!(#PRowList b)))),
							b)
				);

				RemoveRow(n1 + 1, n2, n3, b : int BARCHART)
			)

	(* 
		Updates a history chart. If there are not room for more bars, all
		bars will be mowed up and the new bar will be added
	*)

	fun	hist_row (new_list, b : int BARCHART) =
		(
			let val RowNumber = length(!(#PRowList b))
	  		in
				if (#Move b) > RowNumber
				then
					raise move_larger_than_row
				else
					if !(#CurLine b) >= RowNumber 
					then 
					(
						RemoveRow (1, RowNumber, (#Move b), b);
						#CurLine b := RowNumber - (#Move b)
					)
					else ()
			end;
			#CurLine b := !(#CurLine b) + 1;
			upd_row (!(#CurLine b), new_list, b)
		)


	fun findhisttrues(nil) = nil
	 |	findhisttrues((new_list, cond)::nl) =
	(
		if cond then (new_list, true)::findhisttrues(nl)
		else findhisttrues(nl)
	)

	fun putrownumbers(n, nil) = nil
	 |	putrownumbers(n, (r1, cond)::rl) =
	(
		(n, r1, cond)::putrownumbers(n + 1, rl)
	)

	fun removefirst(0, rl) = rl
	 | 	removefirst(n, r1::rl) =
	(
		removefirst(n - 1, rl)
	)
	 |  removefirst(_, nil) = nil


	fun hist_chart (nil, b) = ()
	 | 	hist_chart (nl, b : int BARCHART) =
	(
		let val nvalues = findhisttrues(nl)
		in
		(
			let val newbars = length(nvalues)
			in
			(
				let val RowNumber = length(!(#PRowList b))
	  			in
				(
					if (#Move b) > RowNumber
					then
						raise move_larger_than_row
					else
					(
						if ((!(#CurLine b) + newbars) > RowNumber)
						then
						(
							if ((#Move b) < newbars) then
							(
								if newbars <= RowNumber then
								(
									upd_chart(putrownumbers(1, nvalues), b);
									(#CurLine b) := newbars + 1
								)
								else
								(
									upd_chart(putrownumbers(
												1, 
												removefirst(
													(newbars - RowNumber), 
													nvalues)), b);
									(#CurLine b) := RowNumber + 1
								)
							)
							else
							(
								RemoveRow (1, RowNumber, (#Move b), b);
								upd_chart(putrownumbers(
											RowNumber - (#Move b) + 1,
											nvalues), b);
								(#CurLine b) := !(#CurLine b) - (#Move b) + 
											   newbars + 1
							)
						)
						else 
						(
							upd_chart(putrownumbers(!(#CurLine b), nvalues), 
									  b)
						)
					)
				)
				end
			)
			end
		)
		end
	)

	(* 		Initialize all rows to have values 0	*)

	fun init_all (b, 0) = ()
	 |	init_all (b : int BARCHART, n) =
		( 
			InitRow(n, b);
			init_all(b, (n-1))
		)
	
end;
end;

structure RealUpdateStr   =
	struct
	structure UT = UtilStr;
	structure LE = LegendStr;
	structure RUT = RealUtilStr;

	(* Structure
		sig
    		exception ErrorReturnListRow
    		exception ErrorReturnRow
    		exception bad_rows
    		exception move_larger_than_row
    		exception wrong_last_arg
    		val CheckInfLimit : int * int * real list * real -> bool
    		val CheckSupLimit : int * int * real list * real -> bool
    		val CreateGridLines : int * int * int * int * int * int * int 
									-> int list
    		val CreateIndexStrList : string * int * int -> string list
    		val CreateStringList : 'a * int -> 'a list
    		val CreateTitle : int * int * int * int -> int
    		val DoResizeGridTag : real * int * int * real BARCHART -> unit
    		val EmptyBarchart : real BARCHART
    		val InitOneRow : (int * 'a) list * int -> unit
    		val InitRow : int * real BARCHART -> unit
    		val InitRows : (int * 'a) list list * int -> unit
    		val InitValTags : int list -> unit
    		val PNRowMerge : (int * real) list * (int * real) list 
								-> (int * real) list
    		val RemoveRow : int * int * int * real BARCHART -> unit
    		val ResizeGridTag : int * real * real * real BARCHART -> unit
    		val ReturnListRow : int * 'a list * ('b * 'a) list list 
									-> 'a list list
    		val ReturnRow : int * 'a * ('b * 'a) list -> 'a list
    		val ShrinkBars : (int * real) list list * real BARCHART -> unit
    		val ShrinkRow : (int * real) list * int * real BARCHART -> unit
    		val SplitList : real list * real list * real list 
							-> real list * real list
    		val UpdEveryRow : int * real list list * real BARCHART -> unit
    		val UpdateBarHeigths : real BARCHART -> unit
    		val UpdateGrid : real BARCHART -> unit
    		val UpdateLower : real BARCHART * real -> unit
    		val UpdatePage : real BARCHART -> unit
    		val UpdatePlotPosition : real BARCHART -> unit
    		val UpdatePlotSize : real BARCHART -> unit
    		val UpdateUpper : real BARCHART * real -> unit
    		val create_bc : int -> int -> int -> int -> int -> int -> int -> 
							int -> boolval create_bc : int -> int -> int -> 
							int -> int -> int -> int -> int -> bool	-> bool ->
							bool -> bool -> bool -> bool -> bool -> real -> 
							real -> int -> real -> LegendStr.LEGEND ref -> 
							int ref -> int -> int -> real BARCHART
    		val create_grids : int * int * int * int * int * int * real * 
							   real * int * int * int * int * int * bool * 
							   bool * bool -> int list
    		val create_row : int * int * int * int * int * int * int * int * int * int -
> (int * real) list
    val create_rows : int * int * int * int * int * int * int * int * int * int
-> (int * real) list list
    val create_ulvalue : int * int * int * int * int * real * int * int * int *
int * int * real * bool -> int list
    val find_large : int * int * real * real list -> real
    val find_row : int * real list * (int * real) list list * real BARCHART -> (
int * real) list list
    val find_small : int * int * real * real list -> real
    val find_sum_row : int * real list * int list * real list -> real list
    val hist_chart : (real list * bool) list * real BARCHART -> unit
    val hist_row : real list * real BARCHART -> unit
    val init_all : real BARCHART * int -> unit
    val init_bc : real BARCHART -> unit
    val pick_id : 'a * 'b -> 'a
    val sum_rows : real list -> real
    val upd_chart : (int * real list * bool) list * real BARCHART -> unit
    val upd_col : int * real list * real BARCHART -> unit
    val upd_ltag : real BARCHART * string list -> unit
    val upd_row : int * real list * real BARCHART -> unit
    val upd_title : real BARCHART * string -> unit
    val update_row : real list * (int * real) list * int * bool * real BARCHART
-> (int * real) list
  end

	*)
	val EmptyBarchart = {Pg = ref 0,
						 CurLine = ref 0, 
						 RowTag = ref nil, 
						 PValTag  =  ref nil,
						 PValTagId = ref nil,
						 NValTag  =  ref nil,
						 NValTagId = ref nil,  
						 GridList = ref nil, 
						 Upper = ref nil, 
						 Lower = ref nil, 
						 PRowList= ref nil, 
						 NRowList= ref nil, 
						 PRI = ref 0, 
						 PGN = ref 0,
						 NGN = ref 0,
						 IdealGN = 0,
						 Dist = 0.0, 
						 PRX = ref 0, 
						 PRY = ref 0,
						 PRW = ref 0,
						 PRH = ref 0,
						 BRH = ref 0, 
						 MaxValue = ref 0.0,
						 MinValue = ref 0.0,
						 StartMax = 0.0,
						 StartMin = 0.0,
						 GridVisible = true, 
						 GridDyn = true, 
						 Axis = ref 0,
						 Legend = ref LE.EmptyLegend,
						 Title = ref 0,
						 Move = 0} : real BARCHART;

	local 	 
	(* 	when the maximum value changes dynamically, reduc is the 
		percentage of the new maximum value where the maximal bar 
		will be displayed *)

		val  Reduc : real ref = ref (75.0 / 100.0) 

		(* grow is the same on the other way, when a graph is expanded *)

		and Grow : real ref = ref (25.0 / 100.0)

		(* this is the percentage of rows removed in a history chart *)

		and HistRemove : real ref = ref (25.0 / 100.0)
		in

			exception bad_rows;
			exception wrong_last_arg;
			exception ErrorReturnListRow;
			exception ErrorReturnRow;
			exception move_larger_than_row;




	(*               CREATION OF THE BARCHART               *)

	(* 	CreateGridLines will create 'n' grid lines	*)
			
	fun CreateGridLines (pg, x, y, w, h, 0, pri) = nil
	 |	CreateGridLines (pg, x, y, w, h, n, pri) =
			let val line = DSStr_CreateLine{page = pg, 
											points = [
												x, 
											 	y - UT.round(real(h) / 2.0),
											 	x, 
											 	y + UT.round(real(h) / 2.0)]}
			in
			(
				DSStr_MakeNodeIntoRgn { obj = line, parent = pri };
				line::CreateGridLines (pg, x+w, y, w, h, (n-1), pri)
			)
			end;


	(* 		create_row will create one bar part 	*)

	fun	create_row (pg, x, y, 0, Ccol, BRH, PRI, PRW, parent, aligntype) = nil
	|	create_row (pg, x, y, col, Ccol, BRH, PRI, PRW, parent, aligntype) =
			let val nod =
				DSStr_CreateNode {page = pg, x = x, y = y, w = (PRW div Ccol), 
								  h = BRH, shape = 1}
			in
			(
				DSStr_MakeNodeIntoRgn {obj = nod, parent = PRI};

				DSUI_Align{obj = nod, aligntype = aligntype, 
						   ref1=parent, ref2 = 0};

				DSWtAttr_ObjectFillType {obj = nod, 
                                         fill= UT.NthPattern (Ccol - col + 1)};

				(nod, 0.0)::create_row (pg, x, y, col - 1, Ccol, BRH, 
										PRI, PRW, nod, aligntype)
			)
			end;


	(* 	
		create_rows will create Ccol row parts and align them 
		to each other with aligntype 
	*)

	fun	create_rows (pg, x, y, 0, col, BarSpace, BRH, PRI, 
					 PRW, aligntype) = nil
	|	create_rows (pg, x, y, row, col, BarSpace, BRH, PRI,
					 PRW, aligntype) =
			create_row (pg, x, y, col, col, BRH, PRI, PRW, PRI, aligntype)::
				create_rows(pg, x, y + BRH + BarSpace, row - 1,
							col, BarSpace, BRH, PRI, PRW, aligntype)


	(* 
		Creates 'PosNgn' grid lines at the right of the axis and NegNgn
		at the left
	*)

	fun create_grids (pg, PRID, Xcoord, Ycoord, Width, Height, 
					  NMax, NMin, MaxWidth, MinWidth, PosNgn, NegNgn,ngn, 
					  GridVisible, Lower, Upper) =
	(	
		(if (NMax > 0.0) 
		 then
		 (
			if GridVisible then
			(
				CreateGridLines(
					pg, 
					Xcoord + 
					UT.round(
					   real(((2 + ngn) * MinWidth) + ((2 - ngn) * MaxWidth)) / 
					   real(2 * ngn)),
					Ycoord, 
					Width div ngn, 
					Height,
					(PosNgn - 1), PRID)
			)
			else
			(
				(if Lower then
				 (
					CreateGridLines(
						pg, 
						Xcoord + 
						UT.round(
							real(((2 + ngn) * MinWidth) + 
								 ((2 - ngn) * MaxWidth)) / 
							real(2 * ngn)),
						Ycoord + (Height div 2) - 2, 
						Width div ngn, 
						4, (PosNgn - 1), PRID)
				 )
				 else nil)@
				(if Upper then
				 (
					CreateGridLines(
						pg, 
						Xcoord + 
						UT.round(real(((2 + ngn) * MinWidth) + 
									  ((2 - ngn) * MaxWidth)) / 
								 real(2 * ngn)),
						Ycoord - (Height div 2) + 2, 
						Width div ngn, 
						4, (PosNgn - 1), PRID)
				 )
				 else nil)
			)
		)
		else nil)@
		(if (NMin < 0.0)
		 then
		 (
			if GridVisible then
			(
				CreateGridLines(
					pg,
					Xcoord + 
					UT.round(real(((ngn - 2) * MinWidth) - 
								  ((ngn + 2) * MaxWidth)) / real(2 * ngn)),
					Ycoord,
					(~(Width div ngn)), 
					Height, (NegNgn - 1), PRID)
			)
			else
			(
				(if Lower then
				 (
					CreateGridLines(
						pg,
						Xcoord + 
						UT.round(real(((ngn - 2) * MinWidth) - 
									  ((ngn + 2) * MaxWidth)) / 
								 real(2 * ngn)),
						Ycoord + (Height div 2) - 2,
						(~(Width div ngn)), 
						4, (NegNgn - 1), PRID)
				 )
				 else nil)@
				(if Upper then
				 (
					CreateGridLines(
						pg,
						Xcoord + 
						UT.round(real(((ngn - 2) * MinWidth) - 
									  ((ngn + 2) * MaxWidth)) / 
								 real(2 * ngn)),
						Ycoord - (Height div 2) + 2,
						(~(Width div ngn)), 
						4, (NegNgn - 1), PRID)
				 )
				 else nil)
			)
		)
		else nil)
	)


	(* 	
		Creates upper or lower value regions with a distance 'TagDist'	
		such that there are 'NegNgn' tags at the left of the axis and
		'PosNgn' at the right. A tag is created at the axis
	*)

	fun create_ulvalue (pg, PRID, Xcoord, Y, W, NMin, MaxWidth, MinWidth, 
						PosNgn, NegNgn, ngn, TagDist, GridDyn) =
	(
		let val ugt = 
			UT.CreateTagRegion(
					pg, 
					((Xcoord +
					 UT.round(real(((ngn - 2) * MinWidth) - 
								   ((ngn + 2) * MaxWidth)) / real(2 * ngn))) - 
					 ((NegNgn - 1) * W) - 15),
					Y, NegNgn, W, PRID, true)@
			UT.CreateTagRegion(
					pg, 
					Xcoord + 
					UT.round(real(MinWidth - MaxWidth) / 2.0),
					Y, 1, W, PRID, true)@ 
			UT.CreateTagRegion(
					pg, 
					Xcoord +
					UT.round(real(((2 + ngn) * MinWidth) + 
								  ((2 - ngn) * MaxWidth)) / real(2 * ngn)),
					Y, PosNgn, W, PRID, true)
		in 
		(
			RUT.UpdateTag'i (ugt, 
							 (if GridDyn then 
								RUT.CreateRoundList (NMin, TagDist, ngn + 1)
							  else 
								RUT.CreateList (NMin, TagDist, ngn + 1)));
			ugt 
		)
		end
	)


	(*		Initialize positive and negative value tags	*)

	fun InitValTags (nil) = ()
	 |	InitValTags ((id::idlist): int list) =
	(
		DSText_Put {obj= id, text = " "};

		InitValTags (idlist)
	)


	(*	
		Creates a list of strings: [s1, s2, ..., sm] where s
		is a string
	*)

	fun CreateIndexStrList(s, i, m) =
	(
		if (i = m) then
			nil
		else
			(s^Int.toString(i))::CreateIndexStrList(s, i+1, m)
	)  
   

	(*
		Creates a title region for a chart with the position 
		'CNX', 'CNY' and with the height 'CHN'. The title 
		region is created with the default text "Title"
	*)
   
	fun CreateTitle (pg, CNX, CNY, CNH) =
	(
		let val TitelRegion = (DSStr_CreateLabel{
									page = pg, 
									x = (CNX - (String.size ("Title") * 5)),
							    	y = (CNY - (CNH div 2) + 10),
							    	w = 10, h = 10, text = "Title"})
		in
		(

			DSText_SetAttr {obj = TitelRegion, font = 5, size = 20, 
							style = Bold, just = Centered};

			TitelRegion
		)
		end
	)
                
(*  parameter in the creation of a bar chart:

	pg: 			page where the graph is created
	row: 			number of rows
	col: 			number of columns
	Xcoord: 		x coordinate of the plot region
	Ycoord: 		y coordinate of the plot region
	Width: 			width of the plot region
	Height: 		height of the plot region
	BRH: 			bar region height
	Expr: 			flag, true means to display row tag regions
	NValue: 		flag, true means to display negative value regions
	PValue: 		flag, true means to display positive value regions
	Upper: 			flag, true means to display upper value region
	Lower: 			flag, true means to display lower value region
	GridDyn: 		flag, true means that grid dist dynamically change 
					when the maximum value change
	GridVisible: 	flag, true means to display whole grid lines
	max: 			maximum value initially displayed
	min: 			minimum value initially displayed. min <= 0.
	gn: 			number of grid lines initial
*)

	fun	create_bc pg row col Xcoord Ycoord Width Height BRH Expr 
				  NValue PValue Upper Lower GridDyn GridVisible Max Min gn 
				  dist legend title move CNW =

	let val BarSpace = (Height - (row * BRH)) div (row + 1)
		and PRID = DSStr_CreateNode {page = pg, x = Xcoord, y = Ycoord,
				                     w = Width, h = Height, shape = 1}
		and TagDist = if GridDyn then RUT.CalcDist((Max - Min), gn)
					  else dist
	in
	(
		let val PosNgn = if GridDyn then 
								RUT.CalcGN(Max, UT.round((Max * real(gn)) / 
												  	 (Max - Min)), TagDist)
						 else UT.ceiling(Max / TagDist)
			and NegNgn = if GridDyn then
								RUT.CalcGN(~Min, 
											gn - UT.round((Max * real(gn)) / 
														  (Max - Min)), 
											TagDist)
						 else UT.ceiling((~Min) / TagDist)
		in
		(
			let val PosNgn = 
				if ((PosNgn = 0) andalso (NegNgn = 0)) then 1
				else PosNgn
			in
			(
				let val NMin = ~(TagDist * real(NegNgn))
					and NMax = TagDist * real(PosNgn)
				in
				(
					let val MinWidth = UT.round((real(Width) * ~NMin) / 
													(NMax - NMin))
						and MaxWidth = (Width - 
										UT.round((real(Width) * ~NMin) / 
												 (NMax - NMin)))
					in
					(
						let val Axis = DSStr_CreateNode {
								page = pg, 
								x = Xcoord + UT.round(
					    			real(MinWidth - MaxWidth) / 2.0), 
								y = Ycoord, w = 1,
								h = Height, shape = 1}
							and ngn = PosNgn + NegNgn
						in
						(
							DSWtAttr_ObjectFlags {obj = Axis, flag = 128, 
									  	  		  value = true};

							DSStr_MakeNodeIntoRgn {obj = Axis, parent = PRID};


		    	   			{

							Pg = ref pg,

		 	 				CurLine = ref 0,

		 	 				RowTag = 
								(if Expr then
									let val TagIdList =
										(UT.CreateTagRegion 
											(pg, 
											 Xcoord - (Width div 2) - 
											 UT.round(
												real(5 * CNW) / 100.0) - 20 -
											 (if NValue then 20 else 0),
                                 			 Ycoord - (Height div 2) + 
								 			 (BRH div 2) + BarSpace - 5, 
								 			 row, BarSpace + BRH, PRID, false))
									in
										UT.UpdateTag's(
											TagIdList,
											CreateIndexStrList(
										   		"bar", 1, 
												(length(TagIdList) + 1)));
										ref TagIdList
									end
				 				else ref nil),

							PValTagId =	
								(if PValue then
									let val TagIdList =
										(UT.CreateTagRegion 
							    			(pg, 
								 		 	Xcoord + (Width div 2) +
										 	UT.round(real(2 * CNW) / 100.0),
						 		 		 	Ycoord - (Height div 2) +  
											(BRH div 2) + BarSpace - 5, row,
						 		 			BarSpace + BRH, PRID, false))
									in
										UT.UpdateTag's(
											TagIdList,
											CreateIndexStrList(
												"pval", 1, 
												(length(TagIdList) + 1)));
										ref TagIdList
									end
				 				else ref nil),

							PValTag =	
								(if PValue then 
									ref (RUT.CreateList(~1.0, 0.0, row)) 
							 	else ref nil),

							NValTagId =	
								(if NValue then
									let val TagIdList =
										(UT.CreateTagRegion 
							    			(pg, 
								 		 	Xcoord - (Width div 2) - 
										 	UT.round(real(2 * CNW) / 100.0) - 
											20,
						 		 		 	Ycoord - (Height div 2) +  
											(BRH div 2) + BarSpace - 5, row,
						 		 			BarSpace + BRH, PRID, false))
									in
										UT.UpdateTag's(
											TagIdList,
											CreateIndexStrList(
												"nval", 1,
												(length(TagIdList) + 1)));
										ref TagIdList
									end
				 				else ref nil),

							NValTag =	
								(if NValue then 
									ref (RUT.CreateList(1.0, ~0.0, row)) 
							 	 else ref nil),

							GridList =	(
								ref(create_grids (
									pg, PRID, Xcoord, Ycoord, Width, Height,
									NMax, NMin, MaxWidth, MinWidth, 
									PosNgn, NegNgn,ngn, 
					  				GridVisible, Lower, Upper))),
 
							Upper =	if Upper then
										ref(create_ulvalue (
												pg, PRID, Xcoord, 
											    (Ycoord - (Height div 2) - 15),
												(Width div ngn), NMin,
					  							MaxWidth, MinWidth, PosNgn, 
												NegNgn, ngn, TagDist, GridDyn))
									else ref nil,	

							Lower = if Lower then
										ref(create_ulvalue (
												pg, PRID, Xcoord, 
											   (Ycoord + (Height div 2) + 15), 
												(Width div ngn), NMin,
					  							MaxWidth, MinWidth, PosNgn, 
												NegNgn, ngn, TagDist, GridDyn))
									else ref nil,			

							NRowList =	
								ref(create_rows 
										(pg, Xcoord + 
									 	UT.round(real(MinWidth - MaxWidth) / 
												 2.0),
									 	Ycoord - (Height div 2) + 
									 	(BRH div 2) + BarSpace, 
									 	row, col, BarSpace, BRH, Axis,
									 	MinWidth, ALN_RL)),

							PRowList = 
								ref(create_rows 
										(pg, Xcoord + 
								 		UT.round(real(MinWidth - MaxWidth) / 
												 2.0),
								 		Ycoord - (Height div 2) + (BRH div 2) +
										BarSpace, row, col,
								 		BarSpace, BRH, Axis,
										MaxWidth, ALN_LR)),

							PRI = ref PRID,

							PGN = ref PosNgn,

							NGN = ref NegNgn,

							IdealGN = gn,

							Dist = dist,

		 	 				PRX = ref Xcoord,

		 	 				PRY = ref Ycoord,

		 	 				PRW = ref Width,

		 	 				PRH = ref Height,

		 	 				BRH = ref BRH,

		 	 				MaxValue = ref NMax,

			 				MinValue = ref NMin,

							StartMax = Max,
	
							StartMin = Min,

		 	 				GridVisible = GridVisible,

		 	 				GridDyn = GridDyn,

							Axis = ref Axis,
	
			 				Legend = legend,
					
							Title = title,

							Move = move
				   
							}  :  real BARCHART
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
    

	(*		Initialize one row to be invisible and have width 1		*)

	fun InitOneRow (nil, h) = ()
	 |	InitOneRow (((id, v)::idlist), h) =
	(
		DSWtAttr_AdjustObjectSize{obj = id, w = 1, h = h};


		DSWtAttr_ObjectFlags {obj = id, flag = 1, value = true};

		InitOneRow (idlist, h)
	)


	(*		Initialize rows		*)

	fun InitRows (nil, h) = ()
	 |	InitRows (r1::rlist, h) =
	(
		InitOneRow(r1, h);
		InitRows(rlist, h)
	)


	(*	Creates a list of length m with each entry the string s	*)

	fun CreateStringList (s, 0) = nil
	 |	CreateStringList (s, m) =
			s::CreateStringList (s, m-1)


	(*	
		Initialize the bar chart bc, to have no text in negative 
		and positive value regions, and to have invisible row regions
	*)
	
	fun init_bc (bc : real BARCHART) =
	(
		UT.UpdateTag's(!(#PValTagId bc), 
					   CreateStringList(" ", length(!(#PValTagId bc))));
		UT.UpdateTag's(!(#NValTagId bc), 
					   CreateStringList(" ", length(!(#PValTagId bc))));

		InitRows(!(#PRowList bc), !(#BRH bc));
		InitRows(!(#NRowList bc), !(#BRH bc))
	)


	(*            UPDATING OF THE BARS, AFTER A RESIZE            *)

	(*	Resize rows according to new grid dist	*)

	fun ShrinkRow (nil, parent, b : real BARCHART) = ()   	
	  | ShrinkRow ((id, val1)::IdRow, parent, b : real BARCHART) =
	    let
		val {PGN=ref pgn, NGN=ref ngn, PRW=ref prw, BRH=ref brh,
		     MinValue=ref minVal, MaxValue=ref maxVal,...} = b
		val w = abs(val1) * real((pgn+ngn) * (prw div (pgn+ngn))) /
		    (maxVal - minVal)
	    in
		if val1 < 0.0 then
		    (DSWtAttr_AdjustObjectSize {obj=id, h=brh, w= UT.round w};
		     DSUI_Align{obj=id, aligntype=ALN_RL, ref1=parent, ref2=0};
		     ShrinkRow(IdRow, id, b))
		else if val1 > 0.0 then
		    (DSWtAttr_AdjustObjectSize {obj=id, h=brh, w= UT.round w};
		     DSUI_Align{obj=id, aligntype=ALN_LR, ref1=parent, ref2=0};
		     ShrinkRow(IdRow, id, b))
		else 
		    ShrinkRow(IdRow, parent, b)
	    end

	(*	Resize all rows in the list 'x::l'	*)

	fun	ShrinkBars (nil, b : real BARCHART) = ()
	 |	ShrinkBars (x::l, b : real BARCHART) = 
        (
			ShrinkRow(x, (!(#Axis b)), b);
          	ShrinkBars (l, b)
      	)
            

	(*      UPDATING THE BARS          *)


	fun update_row (nil, nil, parent, NoChange, b : real BARCHART) = nil
	  | update_row (val2::new_row, (id, val1)::IdRow, 
			parent, NoChange, b : real BARCHART) =
	    if Real.!=(val2,val1) then
			(
				if Real.==(val2,0.0)
				then	
				(

					DSWtAttr_ObjectFlags {obj = id, flag = 1, value = true};

					DSWtAttr_AdjustObjectSize {obj = id, w = 1, h = !(#BRH b)}
				)
				else 
				(
					if Real.==(val1,0.0) then 

						DSWtAttr_ObjectFlags {obj = id, flag = 1, 
											  value = false}

					else ()
				);

				if (val2 < 0.0)
				then
				(
					DSWtAttr_AdjustObjectSize { 
						obj = id, 
						w = UT.round((val2 * real((!(#NGN b) + !(#PGN b)) * 
									 (!(#PRW b) div 
									 (!(#PGN b) + !(#NGN b))))) / 
									 (!(#MinValue b) - !(#MaxValue b))),
						h = !(#BRH b)};
					DSUI_Align{ obj = id, aligntype = ALN_RL,
								ref1 = parent, ref2 = 0}
				)
				else
				(
					DSWtAttr_AdjustObjectSize {	
						obj = id, 
						w = UT.round((val2 * real((!(#PGN b) + !(#NGN b)) * 
									 (!(#PRW b) div 
									 (!(#PGN b) + !(#NGN b))))) / 
									 (!(#MaxValue b) - !(#MinValue b))),
						h = !(#BRH b)};
					DSUI_Align{ obj = id, aligntype = ALN_LR,
								ref1 = parent, ref2 = 0 }
				);

				if Real.==(val2,0.0)
				then 
					(id, val2)::update_row(new_row, IdRow, parent, false, b)
        		else 
					(id, val2)::update_row(new_row, IdRow, id, false, b)
        	)
    		else 
			(
        		if not(NoChange)
        		then 
            		if (val2 < 0.0) then
                		DSUI_Align{ obj = id, aligntype = ALN_RL,
									ref1 = parent, ref2 = 0}
            		else
                		DSUI_Align{	obj = id, aligntype = ALN_LR,
									ref1 = parent, ref2 = 0}
        		else ();

        		if Real.==(val2,0.0)
        		then 
					(id, val2)::update_row(new_row, IdRow, parent, NoChange, b)
		 		else 
					(id, val2)::update_row(new_row, IdRow, id, NoChange, b)
            )
	 |	update_row(_,_,_,_,_) = raise bad_rows
    

	(* 	Finds the n'th row and updates it with the values in 'new_row'	*)
   
	fun	find_row (n, new_row, nil, b : real BARCHART) = raise wrong_last_arg
	 |	find_row (n, new_row, x::l, b : real BARCHART) =
			if n = 1 then 
				(update_row (new_row, x, (!(#Axis b)), 
				 			 true, b : real BARCHART))::l
			else 
				x::find_row (n - 1, new_row, l, b)


	(*	Sum the values in the list	*)

	fun	sum_rows (nil) = 0.0
 	 |	sum_rows (x::l) = (x + sum_rows (l))


	(*
		Gives a list of values where the n'th value is updated 
		to be the sum of the values in 'new_row'. This is used 
		to update the positive and negative value regions
	*)

	fun	find_sum_row (1, new_row, id::l1, value::l2) = 
		let val new_sum = sum_rows (new_row) 
		in
			if Real.!=(new_sum,value) then 
				(DSText_Put {obj = id, text = Real.toString(new_sum)};
				 new_sum::l2)
			else 
				value::l2
			end
 	 |	find_sum_row(n, new_row, id::l1, value::l2) = 
			value::find_sum_row(n-1, new_row, l1, l2)
 	 |	find_sum_row(n, new_row,_,_) = raise wrong_last_arg

	fun	pick_id (id, value) = id

(*_____________________________________________________________________


                             UPDATING GRIDS AND GRID TAGS    

_______________________________________________________________________*)

	fun mem (i, nil) = false
	 |	mem (i, j::l) = (i = j) orelse (mem(i, l))

	(* 	
		Checks that all values in the list 'x::l' are smaller than INF 
		except the n'th value
	*)

	fun	CheckInfLimit(rowlist : int list, n : int, nil, INF : real) = true
	 |	CheckInfLimit(rowlist : int list, n : int, x::l, INF : real)  =
			if not(mem(n, rowlist)) 
			then 
				if x < INF 
				then 
					CheckInfLimit(rowlist, n + 1, l, INF)
				else false
			else CheckInfLimit(rowlist, n + 1, l, INF)

	(* 
		Checks that all values in the list 'x::l' are larger than SUP
		except the n'th value
	*)

	fun	CheckSupLimit(rowlist : int list, n : int, nil, SUP : real) = true
	 |	CheckSupLimit(rowlist : int list, n : int, x::l, SUP : real)  =
			if not(mem(n, rowlist)) 
			then 
				if x > SUP 
				then 
					CheckSupLimit(rowlist, n + 1, l, SUP)
				else false
			else CheckSupLimit(rowlist, n + 1, l, SUP)


	(*		Removes and recreates grid lines		*)

	fun	UpdateGrid(b : real BARCHART) =
		let val MinWidth = UT.round(real(!(#NGN b) * !(#PRW b)) / 
						    		real(!(#PGN b) + !(#NGN b)))
			and MaxWidth = !(#PRW b) - UT.round(real(!(#NGN b) * !(#PRW b)) / 
						   						real(!(#PGN b) + !(#NGN b)))
			in
			( 
				UT.DeleteIdList (!(#GridList b));
				(#GridList b) := (
					(if !(#MaxValue b) > 0.0 then
					 (
						if (#GridVisible b) then
						(
							CreateGridLines
							(!(#Pg b),
					 		 !(#PRX b) + 
					 		 UT.round(real(
							 	((2 + (!(#PGN b) + !(#NGN b))) * MinWidth) + 
						 	 	((2 - (!(#PGN b) + !(#NGN b))) * MaxWidth)) / 
								real(2 * (!(#PGN b) + !(#NGN b)))), 
							!(#PRY b),
							(!(#PRW b) div (!(#PGN b) + !(#NGN b))),
							!(#PRH b),
							(!(#PGN b) - 1), 
							!(#PRI b))
						)
						else
						(
							(if (!(#Lower b) <> nil) then
							 (	CreateGridLines
								(!(#Pg b),
					 		 	!(#PRX b) + 
					 		 	UT.round(real(
							 	((2 + (!(#PGN b) + !(#NGN b))) * MinWidth) + 
						 	 	((2 - (!(#PGN b) + !(#NGN b))) * MaxWidth)) / 
								real(2 * (!(#PGN b) + !(#NGN b)))), 
								!(#PRY b) + (!(#PRH b) div 2) - 2,
								(!(#PRW b) div (!(#PGN b) + !(#NGN b))),
								4,
								(!(#PGN b) - 1), 
								!(#PRI b)))
							 	else nil)@
							(if (!(#Upper b) <> nil) then
							 (CreateGridLines
								(!(#Pg b),
					 		 	!(#PRX b) + 
					 		 	UT.round(real(
							 	((2 + (!(#PGN b) + !(#NGN b))) * MinWidth) + 
						 	 	((2 - (!(#PGN b) + !(#NGN b))) * MaxWidth)) / 
								real(2 * (!(#PGN b) + !(#NGN b)))), 
								!(#PRY b) - (!(#PRH b) div 2) + 2,
								(!(#PRW b) div (!(#PGN b) + !(#NGN b))),
								4,
								(!(#PGN b) - 1), 
								!(#PRI b)))
							 else nil)
						)
					 )
					 else nil)@
					(if !(#MinValue b) < 0.0 then
					 (	if (#GridVisible b) then
						(
							CreateGridLines
							(!(#Pg b),
					 		!(#PRX b) + 
					 		UT.round(real(
								(((!(#PGN b) + !(#NGN b)) - 2) * MinWidth) - 
						 		(((!(#PGN b) + !(#NGN b)) + 2) * MaxWidth)) / 
								real(2 * (!(#PGN b) + !(#NGN b)))), 
							!(#PRY b),
							(~(!(#PRW b) div (!(#PGN b) + !(#NGN b)))),
							!(#PRH b),
							(!(#NGN b) - 1), 
							!(#PRI b))
						)
						else
						(
							(if (!(#Lower b) <> nil) then
							 (	CreateGridLines
								(!(#Pg b),
					 			!(#PRX b) + 
					 			UT.round(real(
								(((!(#PGN b) + !(#NGN b)) - 2) * MinWidth) - 
						 		(((!(#PGN b) + !(#NGN b)) + 2) * MaxWidth)) / 
								real(2 * (!(#PGN b) + !(#NGN b)))), 
								!(#PRY b) + (!(#PRH b) div 2) - 2,
								(~(!(#PRW b) div (!(#PGN b) + !(#NGN b)))),
								4,
								(!(#NGN b) - 1), 
								!(#PRI b)))
							 else nil)@
							(if (!(#Upper b) <> nil) then
							 (CreateGridLines
							(!(#Pg b),
					 		!(#PRX b) + 
					 		UT.round(real(
								(((!(#PGN b) + !(#NGN b)) - 2) * MinWidth) - 
						 		(((!(#PGN b) + !(#NGN b)) + 2) * MaxWidth)) / 
								real(2 * (!(#PGN b) + !(#NGN b)))), 
							!(#PRY b) - (!(#PRH b) div 2) + 2,
							(~(!(#PRW b) div (!(#PGN b) + !(#NGN b)))),
							4,
							(!(#NGN b) - 1), 
							!(#PRI b)))
							 else nil)
						)
					)
					else nil
				  )
				)
			)
			end


	(*	Removes and recreates upper value regions	*)

	fun UpdateUpper(b : real BARCHART, Dist : real) =
		(	
			let val MinWidth = UT.round(real(!(#NGN b) * !(#PRW b)) / 
						    			real(!(#PGN b) + !(#NGN b)))
				and MaxWidth = !(#PRW b) - 
								UT.round(real(!(#NGN b) * !(#PRW b)) / 
						   				 real(!(#PGN b) + !(#NGN b)))
			in
			(
				UT.DeleteIdList (!(#Upper b));

				(#Upper b) := create_ulvalue(
								!(#Pg b), !(#PRI b), !(#PRX b),
								(!(#PRY b) - !(#PRH b) div 2 - 16),
								(!(#PRW b) div (!(#PGN b) + !(#NGN b))),
								!(#MinValue b), MaxWidth, MinWidth, 
								!(#PGN b), !(#NGN b), 
								(!(#PGN b) + !(#NGN b)), 
								Dist, (#GridDyn b))
			)
			end
		) 


	(*	Removes and recreates upper value regions	*)

	fun	UpdateLower(b : real BARCHART, Dist : real) =
		(	
			let val MinWidth = UT.round(real(!(#NGN b) * !(#PRW b)) / 
						    			real(!(#PGN b) + !(#NGN b)))
				and MaxWidth = !(#PRW b) - 
								UT.round(real(!(#NGN b) * !(#PRW b)) / 
						   				 real(!(#PGN b) + !(#NGN b)))
			in
			(
				UT.DeleteIdList (!(#Lower b));

				(#Lower b) := create_ulvalue (
								!(#Pg b), !(#PRI b), !(#PRX b), 
								(!(#PRY b) + !(#PRH b) div 2 + 16), 
								(!(#PRW b) div (!(#PGN b) + !(#NGN b))), 
								!(#MinValue b), MaxWidth, MinWidth, 
								!(#PGN b), !(#NGN b), 
								(!(#PGN b) + !(#NGN b)), 
								Dist, (#GridDyn b))
			)
			end
		)


	(*	Is called when the max value or min value has been changed	*)

	fun DoResizeGridTag (Dist, PosGn, NegGn, b: real BARCHART) =
		(
			(#MaxValue b) := real(PosGn) * Dist;
			(#MinValue b) := real(~(NegGn)) * Dist;

			ShrinkBars (!(#PRowList b), b);
			ShrinkBars (!(#NRowList b), b);

			(#PGN b) := PosGn;
			(#NGN b) := NegGn;
				 
			DSWtAttr_ObjectPosition{obj = !(#Axis b),
									x = !(#PRX b) + 
										UT.round(
										   real((NegGn - PosGn) * !(#PRW b)) / 
										   (2.0 * real(PosGn + NegGn))),
									y = !(#PRY b)};
			UpdateGrid(b);
		
			if !(#Upper b) <> nil
			then 
				UpdateUpper(b, Dist)
			else ();
			
			if !(#Lower b) <> nil
			then 
				UpdateLower(b, Dist)
			else ()
		)


	(*	
		Finds the largest value in the list 'i1::ilist' - except from the
		row'th value
	*)

	fun find_large(rowlist, i, n, nil) = n
	 |  find_large(rowlist, i : int, n : real, i1::ilist) =
			if ((n < i1) andalso (not(mem(i, rowlist)))) 
			then 
				find_large(rowlist, i+1, i1, ilist)
			else find_large(rowlist, i+1, n, ilist)


	(*
		Finds the smallest value in the list 'i1::ilist' - except from the
		row'th value
	*)

	fun find_small(rowlist, i, n, nil) = n
	 |	find_small(rowlist, i : int, n : real, i1::ilist) =
			if ((n > i1) andalso (not(mem(i, rowlist))))
			then
				find_small(rowlist, i+1, i1, ilist)
			else find_small(rowlist, i+1, n, ilist)


	(*	
		Checks if the min or max value is to be changed - and if 
		recreates the grid lines and the upper and lower value regions
	*)

	fun	ResizeGridTag(rowlist, psum, nsum, b : real BARCHART) =
		let val INF = !Grow * !(#MaxValue b) 
			and SUP = !Grow * !(#MinValue b)
		in
		(
			let val NMaxV = 
				(
					if (psum > !(#MaxValue b))
					then 
						psum / !Reduc
					else 
						if ((psum < INF) andalso 
							CheckInfLimit(rowlist, 1, !(#PValTag b), INF))
						then
							find_large(rowlist, 1, psum, !(#PValTag b))
						else
							!(#MaxValue b)
				)
				and NMinV = 
				(
					if (nsum < !(#MinValue b))
					then
						nsum / !Reduc
					else
						if ((nsum > SUP) andalso 
							CheckSupLimit(rowlist, 1, !(#NValTag b), SUP))  
						then
							find_small(rowlist, 1, nsum, !(#NValTag b))
						else 
							!(#MinValue b)
				)
			in
			(
				if  ((psum > !(#MaxValue b)) orelse 
					 (nsum < !(#MinValue b)) orelse
					 ((psum < INF) andalso 
					   CheckInfLimit(rowlist, 1, !(#PValTag b), INF)) orelse
					 ((nsum > SUP) andalso
					   CheckSupLimit(rowlist, 1, !(#NValTag b), SUP)))
				then
				(
					let val TagDist = if (#GridDyn b) then
										RUT.CalcDist((NMaxV - NMinV), (#IdealGN b))
									  else (#Dist b)
					in
					(
						let val PosNgn =
							(
								if (#GridDyn b) then 
									RUT.CalcGN(	NMaxV, 
											UT.round
											((NMaxV * real((#IdealGN b))) /
 											 (NMaxV - NMinV)), 
											TagDist)
								else UT.ceiling(NMaxV / TagDist)
							)
							and NegNgn =
							(
								if (#GridDyn b) then
									RUT.CalcGN(~NMinV, 
											(#IdealGN b) - 
											UT.round
											   ((NMaxV * real((#IdealGN b))) /
 												(NMaxV - NMinV)), 
											TagDist)
								else UT.ceiling((~NMinV) / TagDist)
							)
						in
						(
							let val PosNgn =
									if ((PosNgn = 0) andalso (NegNgn = 0))
									then 1
									else PosNgn
							in
							(
								DoResizeGridTag (TagDist, PosNgn, NegNgn, b)
							)
							end
						)
						end
					)
					end
				)
				else () (* Do not Resize *)
			)
			end
		)
		end


	(*	
		Updates the entries representing the width and height of the 
		plot region to actually fit the size. There can be 
		inconsistencies if the region has be resized
	*)

	fun	UpdatePlotSize(b : real BARCHART) =
			let val {w, h} = DSRdAttr_GetObjectSize (!(#PRI b))
			in 
			(
				if (w <> !(#PRW b)) 
				then ((#PRW b) := w) 
				else ();
				if (h <> !(#PRH b))  
				then ((#PRH b) := h) 
				else ()
			)
			end

	(*	
		Updates the entries representing the height of the 
		bar regions to actually fit the size. There can be 
		inconsistencies if the region has be resized
	*)

	fun	UpdateBarHeigths(b : real BARCHART) =
		let val {w = w1, h = h1} = 
				DSRdAttr_GetObjectSize(pick_id (hd (hd (!(#PRowList b)))))
		in
			(#BRH b) := h1
		end


	(*	
		Updates the entries representing the x-coordinate and 
		the y-coordinate of the plot region to actually fit the size. 
		There can be inconsistencies if the region has be repositioned
	*)

	fun	UpdatePlotPosition(b : real BARCHART) =
		let val {x = x1, y = y1} = DSRdAttr_GetObjectCenter(!(#PRI b))
		in 
		(
			(#PRX b) := x1;
			(#PRY b) := y1 
		) 
		end


	(*	
		Updates the entry representing the page on which the chart is
		positioned. There can be inconsistencies if the chart has be 
		moved to another page
	*)

	fun	UpdatePage(b : real BARCHART) =
			(#Pg b := UT.ReturnPage (!(#PRI b)))


	(*	
		Splits a list into two lists. One with the positive values 
		from the list and a one with the negative values. The new 
		lists will have the same length as the original, as '0's has 
		been put in instead of the negative values for the list with 
		the positive values and visa versa
	*)

	fun SplitList(nil, plist, nlist) = (plist, nlist)
	 |	SplitList((i1::ilist) : real list, plist, nlist) = 
		if (i1 > 0.0)
		then
			SplitList(ilist, plist@[i1], nlist@[0.0])
		else
			SplitList(ilist, plist@[0.0], nlist@[i1]) 
 

	fun UpdateRows(nil, b) = ()
	 |	UpdateRows((row, pnew_list, nnew_list)::rl, b : real BARCHART) =
	(
		(#PRowList b) := find_row (row, pnew_list, !(#PRowList b), b);
		(#NRowList b) := find_row (row, nnew_list, !(#NRowList b), b); 

		(* update positive and negative value regions - if displayed *)

		if !(#PValTagId b) <> nil  
		then  
			(#PValTag b) := find_sum_row (row, pnew_list, !(#PValTagId b),
										  !(#PValTag b)) 
		else ();

		if !(#NValTagId b) <> nil  
		then  
			(#NValTag b) := find_sum_row (row, nnew_list, !(#NValTagId b),
										  !(#NValTag b)) 
		else ();

		UpdateRows(rl, b)
	)


	(*	Updates the row 'row' with the values listed in 'new_list'	*)

	fun	upd_row (row, new_list, b : real BARCHART) =
		let val (pnew_list, nnew_list) = SplitList(new_list, nil, nil)
		in 
		(
			(*	
				updates the values representing position and size of 
				the plot region and the height of the bar regions
			*)
			UpdatePlotSize(b);
			UpdateBarHeigths(b); 
			UpdatePlotPosition(b);

			(*	if the max value or min value might have changed -
				resize the grids and the tags if necessary
			*)
			if ( ((sum_rows (pnew_list)) > 0.0) orelse
				 ((sum_rows (nnew_list)) < 0.0) )
			then 
				ResizeGridTag([row], sum_rows (pnew_list), 
							 sum_rows (nnew_list), b) 
			else ();

	(*
			(*	update the rows with the new values	*)
			(#PRowList b) := find_row (row, pnew_list, !(#PRowList b), b);
			(#NRowList b) := find_row (row, nnew_list, !(#NRowList b), b);

			if !(#PValTagId b) <> nil  
			then  
				(#PValTag b) := find_sum_row (row, pnew_list, !(#PValTagId b),
											  !(#PValTag b)) 
			else ();

			if !(#NValTagId b) <> nil  
			then  
				(#NValTag b) := find_sum_row (row, nnew_list, !(#NValTagId b),
											  !(#NValTag b)) 
			else ()
	*)
			UpdateRows([(row, pnew_list, nnew_list)], b)
		)
		end


	fun findtrues(nil) = nil
	 | 	findtrues((rn, new_list, cond)::pl) =
	(
		if cond then
			let val (plist, nlist) = SplitList(new_list, nil, nil)
			in
			( 
				(rn, plist, nlist)::findtrues(pl)
			)
			end
		else
			findtrues(pl)
	)


	fun findmax(nil, max) = max
	 |	findmax((rn, plist, nlist)::pl, max) =
	(
		if sum_rows (plist) > max then
		(
			findmax(pl, sum_rows (plist))
		)
		else
		(
			findmax(pl, max)
		)
	)


	fun findmin(nil, min) = min
	 |	findmin((rn, plist, nlist)::pl, min) = 
	(
		if sum_rows (nlist) < min then
		(
			findmin(pl, sum_rows (nlist))
		)
		else
		(
			findmin(pl, min)
		)
	)


	fun findrows(nil, rowlist) = rowlist
	 |	findrows((rn, plist, nlist)::ul, rowlist) = 
	(
		findrows(ul, rn::rowlist)
	) 


	fun upd_chart (upd_list, b : real BARCHART) =
	(
		(*	
			updates the values representing position and size of 
			the plot region and the height of the bar regions
		*)
		UpdatePlotSize(b);
		UpdateBarHeigths(b); 
		UpdatePlotPosition(b);

		let val nupd_list = findtrues(upd_list)
		in
		(
			let val max = findmax(nupd_list, 0.0)
				and min = findmin(nupd_list, 0.0)
				and rowlist = findrows(nupd_list, nil)
			in
			(
				if (max > 0.0 orelse min < 0.0) then
					ResizeGridTag(rowlist, max, min, b)
				else ()
			)
			end; 

			UpdateRows(nupd_list, b)
		)
		end
	)


	(*	Updates every row	*)

	fun	UpdEveryRow (row, nil, b : real BARCHART) = ()
	 |	UpdEveryRow (row, x::RowList, b : real BARCHART)  = 
		(
			upd_row (row, x, b); 
			UpdEveryRow (row+1, RowList, b)
		)
    

	(*		Updates the legend tags (the part name regions)		*)

	fun	upd_ltag (b : real BARCHART, l) =
			UT.UpdateTag's (!(#RowTag b), l)


	(*		Updates the title region	*)

	fun upd_title (b : real BARCHART, title) =
			UT.UpdateTag's ([!(#Title b)], [title])


	(* 	
		Returns a list of the second entries of the list '(id, value2)::l' - 
		except that the col'th entry has been replace by value1
	*)

	fun	ReturnRow (_, value1, nil) = nil
	 |	ReturnRow (1, value1, (id, value2)::l) = 
			value1::ReturnRow (0, value1, l)
	 |	ReturnRow (col, value1, (id, value2)::l) = 
			value2::ReturnRow(col - 1, value1, l)


	(*	
		Returns a list of lists equal to lists of second enties in
		the lists in 'r::ListRow' - except that the col'th entry has been
		replaced by the values in 'value::ListCol' 
	*)

	fun	ReturnListRow (col, nil, nil) = nil
	 |	ReturnListRow (col, value::ListCol, r::ListRow) = 
			ReturnRow(col, value, r)::ReturnListRow(col, ListCol, ListRow)
	 |	ReturnListRow (_,_,_) = raise ErrorReturnListRow


	(*	
		Updates the bar part 'col' for every row, with the 
		values listed in 'l'. This is done by - for every bar -
		forming a list from the values that the bar parts have 
		already got but with a new value for the col'th bar part.
		Now all the bars are updated
	*)

	fun	upd_col (col, l, b : real BARCHART) =
			let val (pl, nl) = SplitList(l, nil, nil)
			in
			(
				let val prowlist = ReturnListRow (col, pl, !(#PRowList b))
					and nrowlist = ReturnListRow (col, nl, !(#NRowList b)) 
				in 
				(
					(UpdEveryRow (1, prowlist, b));
					(UpdEveryRow (1, nrowlist, b))
				)
				end
			)
			end

	fun	InitRow(n, b : real BARCHART) =
			upd_row(n, RUT.CreateList (0.0, 0.0, 
									   length(hd (!(#PRowList b)))), b)


	(* 
		Creates a list which is formed from two lists. When one list 
		has entry '0', the value from the other list will be in the 
		new list
	*)

	fun PNRowMerge(nil, nil) = nil
	 |	PNRowMerge((i1: int, j1: real)::list1, (i2, j2)::list2) =
			if Real.==(j1,0.0)
			then (i2, j2)::PNRowMerge(list1, list2)
			else (i1, j1)::PNRowMerge(list1, list2)
	 | PNRowMerge(_,_) = raise bad_rows
     
   fun RemoveRow(n1, n2, n3, b : real BARCHART) =
			if n1 > n2 then ()
          	else
			(
				(if n1 <= (n2 - n3) then
					upd_row(n1, 
							map UT.Snd (
								PNRowMerge(
									UT.NthElement (n1 + n3, !(#PRowList b)),
									UT.NthElement (n1 + n3, !(#NRowList b)))),
							b)
				else
					upd_row(n1, 
							RUT.CreateList(0.0, 0.0, 
										  length(hd (!(#PRowList b)))), 
							b)
				);

				RemoveRow(n1 + 1, n2, n3, b : real BARCHART)
			)


	(* 
		Updates a history chart. If there are not room for more bars, all
		bars will be mowed up and the new bar will be added
	*)

	fun	hist_row (new_list, b : real BARCHART) =
		(
			let val RowNumber = length(!(#PRowList b))
	  		in
				if (#Move b) > RowNumber
				then
					raise move_larger_than_row
				else
					if !(#CurLine b) >= RowNumber 
					then 
					(	
						RemoveRow (1, RowNumber, (#Move b), b);
						#CurLine b := RowNumber - (#Move b)
					)
					else ()
			end;
			#CurLine b := !(#CurLine b) + 1;
			upd_row (!(#CurLine b), new_list, b)
		)

	fun findhisttrues(nil) = nil
	 |	findhisttrues((new_list, cond)::nl) =
	(
		if cond then (new_list, true)::findhisttrues(nl)
		else findhisttrues(nl)
	)

	fun putrownumbers(n, nil) = nil
	 |	putrownumbers(n, (r1, cond)::rl) =
	(
		(n, r1, cond)::putrownumbers(n + 1, rl)
	)

	fun removefirst(0, rl) = rl
	 | 	removefirst(n, r1::rl) =
	(
		removefirst(n - 1, rl)
	) 
	 | removefirst(_, nil) = nil

	fun hist_chart (nil, b) = ()
	 | 	hist_chart (nl, b : real BARCHART) =
	(
		let val nvalues = findhisttrues(nl)
		in
		(
			let val newbars = length(nvalues)
			in
			(
				let val RowNumber = length(!(#PRowList b))
	  			in
				(
					if (#Move b) > RowNumber
					then
						raise move_larger_than_row
					else
					(
						if (!(#CurLine b) + newbars) > RowNumber
						then
						(
							if ((#Move b) < newbars) then
							(
								if newbars <= RowNumber then
								(
									upd_chart(putrownumbers(1, nvalues), b);
									(#CurLine b) := newbars + 1
								)
								else
								(
									upd_chart(putrownumbers(
												1, 
												removefirst(
													(newbars - RowNumber), 
													nvalues)), b);
									(#CurLine b) := RowNumber + 1
								)
							)
							else
							(
								RemoveRow (1, RowNumber, (#Move b), b);
								upd_chart(putrownumbers(
											RowNumber - (#Move b) + 1,
											nvalues), b);
								(#CurLine b) := !(#CurLine b) - (#Move b) + 
											   newbars + 1
							)
						)
						else 
						(
							upd_chart(putrownumbers(!(#CurLine b), nvalues), 
									  b)
						)
					)
				)
				end
			)
			end
		)
		end
	)


	(* 		Initialize all rows to have values 0	*)
	fun init_all (b, 0) = ()
	 |	init_all (b : real BARCHART, n) =
		( 
			InitRow(n, b);
			init_all(b, (n-1))
		)


end;
end;


structure IBC = 
	struct
	structure LE = LegendStr;
	structure UP = IntUpdateStr;
	structure UT = UtilStr;

	(*
		Signature                 
  		sig
			val HCcreate : {
					BarHeight:int,
					NoOfParts:int,
					Dist:int,
					FixedNo:bool,
					IdealGN:int,
					TicksOnly:bool,
					height:int,
					Legend:bool,
					LowerValue:bool,
					Max:int,
					Min:int,
					Move:int,
					NegValue:bool,
					PosValue:bool,
					NoOfBars:int,
					BarName:bool,
					Title:bool,
					UpperValue:bool,
					width:int,
					x:int,
					y:int} -> int BARCHART
    		val SCcreate : {
					BarHeight:int,
					NoOfParts:int,
					Dist:int,
					FixedNo:bool,
					IdealGN:int,
					TicksOnly:bool,
					height:int,
					Legend:bool,
					LowerValue:bool,
					Max:int,
					Min:int,
					NegValue:bool,
					PosValue:bool,
					NoOfBars:int,
					BarName:bool,
					Title:bool,
					UpperValue:bool,
					width:int,
					x:int,
					y:int} -> int BARCHART
    		val clear_chart : int BARCHART ref -> unit
    		val clear_hist : int BARCHART ref -> unit
    		val create_barchart : {
					bar_h:int,
					ch_h:int,
					ch_w:int,
					cn_x:int,
					cn_y:int,
					col:int,
					dist:int,
					expr:bool,
					grid_dyn:bool,
					grid_no:int,
					grid_vis:bool,
					legend:bool,
					low_grid:bool,
					max_val:int,
					min_val:int,
					move:int,
					nval_reg:bool,
					page:int,
					pval_reg:bool,
					row:int,		
					title:bool,
					up_grid:bool} -> int BARCHART
    		val delete : int BARCHART ref -> unit
    		val empty_barchart : unit -> int BARCHART
    		val hist_chart : {name:int BARCHART ref,
							  values:(int list * bool) list} -> unit
    		val hist_init : {name:int BARCHART ref,init:int} -> unit
    		val hist_ltag : {name:int BARCHART ref,tags:string list} -> unit
    		val hist_row : {name:int BARCHART ref,new_row:int list} -> unit
    		val hist_bntag : {name:int BARCHART ref,tag:string} -> unit
    		val hist_title : {name:int BARCHART ref,title:string} -> unit
    		val init : int BARCHART ref -> unit
    		val insertnext : int list * int list * string -> unit
    		val switchtext : int list * string -> unit
    		val upd_chart : {name:int BARCHART ref,
							 values:(int * int list * bool) list} -> unit
    		val upd_col : {name:int BARCHART ref,column:int,
						    row_values:int list} -> unit
    		val upd_ltag : {name:int BARCHART ref,tags:string list} -> unit
    		val upd_row : {name:int BARCHART ref,column_values:int list,row:int}
 							-> unit
    		val upd_bntag : {name:int BARCHART ref,tags:string list} -> unit
   	 		val upd_title : {name:int BARCHART ref,title:string} -> unit
  		end
	*)

	(* modified Nov 18 by VP. Added: page parameter; uncurried; label fields *)

	(* parameter in the creation of a bar chart:
		title :		flag, if ture a title region will be created
		move :		percentage of number of rows. Determines the upd method for
					history charts.
		page :		the page on which the chart will be generated
		cn_x :		x-coordinate for the chart node
		cn_y :		y-coordinate for the chart node
		row: 		number of bars
		col: 		number of bar parts
		ch_w :		width of chart node
		ch_h :		height of the chart node
		bar_h : 	bar region height
		max_val : 	maximum value initially displayed
		min_val : 	minimum value initially displayed
		grid_no : 	number of ideal grid lines initial
		dist :		fixed dist if any
		expr: 		flag, true means to display row tag region
		pval_reg :	flag, true means to display positive value regions
		nval_reg :	flag, true means to display negative value regions
		up_grid : 	flag, true means to display upper value regions
		low_grid : 	flag, true means to display lower value region
		legend : 	true means Legend Region created and diplayed
		grid_dyn : 	flag, true means that grid dist dynamically changes 
				 	when the maximum value changes
		grid_vis : 	flag, true means to display whole grid line
	*)
	
	fun empty_barchart () = UP.EmptyBarchart;

	fun	create_barchart {title = title,
						 move = move,
				 		 page = pg,
				 		 cn_x = CNX,
				 		 cn_y = CNY,
				 		 row = row,
				 		 col = col, 
				 		 ch_w = CNW,
				 		 ch_h = CNH,
				 		 bar_h = BRH,
				 		 max_val = MaxValue,
						 min_val = MinValue,
				 		 grid_no = GridNo,
						 dist = Dist,
				 		 expr = Expr,
				 		 pval_reg = PValue,
						 nval_reg = NValue,
				 		 up_grid = Upper, 
				 		 low_grid = Lower,
				 		 legend = Legend,
				 		 grid_dyn = GridDyn,
				 		 grid_vis = GridVisible } =

			let	val LRX = CNX + (CNW * 3 div 8)
				and LRW = (20 * CNW) div 100
				and PRX = (	
						if Expr (* 4% of CNW as room for Bar Name region *)
						then
						(
							if Legend (* moves x-coord (9/80 * CNW) *)
						   	then
							(
								if ((PValue andalso NValue) orelse
									(not(PValue) andalso not(NValue)))
								then
								(
									(* CNX - (9/80 CNW) + (5% CNW) *)

									CNX - UT.round(real(5 * CNW) / 80.0)
								)
								else (* 2% of CNW as room for value tag *)
								(
									if not(PValue) then
									(
										(* CNX - (9/80 CNW) + (5% CNW) + 
										   (2% CNW) *)

										CNX - UT.round(real(11 * CNW) / 200.0)
									) 
									else (* not(PValue) *)
									(
										(* CNX - (9/80 CNW) + (5% CNW) - 
										   (2% CNW) *)

										CNX - UT.round(real(33 * CNW) / 400.0)
									)
								)
							)
						   	else
							(
								if ((PValue andalso NValue) orelse
									(not(PValue) andalso not(NValue)))
								then
								(
									(* CNX + (5% CNW) *)

									CNX + UT.round(real(5 * CNW) / 100.0)
								)
								else (* 2% for CNW as room for value tag *)
								(
									if not(PValue) then
									(
										(* CNX + (5% CNW) + (2% CNW) *)

										CNX + UT.round(real(7 * CNW) / 100.0)
									)
									else (* not(NValue) *)
									(
										(* CNX + (5% CNW) - (2% CNW) *)

										CNX + UT.round(real(3 * CNW) / 100.0)
									)
								)
							)
						)
						else
						(
							if Legend (* moves x-coord (9/80 * CNW) *)
						   	then
							(
								if ((PValue andalso NValue) orelse
									(not(PValue) andalso not(NValue)))
								then
								(
									(* CNX - (9/80 CNW) *)

									CNX - UT.round(real(9 * CNW) / 80.0)
								)
								else
								(
									if not(PValue) then
									(
										(* CNX - (9/80 CNW) + (4% CNW) *) 

										CNX - UT.round(real(37 * CNW) / 400.0)
									) 
									else (* not(NValue) *)
									(
										(* CNX - (9/80 * CNW) + (4% CNW) *)

										CNX - UT.round(real(53 * CNW) / 400.0)
									)
								)
							)
						   	else
							(
								if ((PValue andalso NValue) orelse
									(not(PValue) andalso not(NValue)))
								then
								(
									CNX
								)
								else
								(
									if not(PValue) then
									(
										(* CNX + (2% CNW) *)

										CNX + UT.round(real(2 * CNW) / 100.0)
									)
									else (* not(NValue) *)
									(
										(* CNX - (2% CNW) *)

										CNX - UT.round(real(2 * CNW) / 100.0)
									)
								)
							)
						)
					)
						
				and PRW = (if Legend then 
								(52 * CNW) div 100
						   else 
								(67 * CNW) div 100)
				and PRH = (70 * CNH) div 100
				and PRY = (if title then
								CNY + (CNH div 50)
						   else
								CNY)
				and PatternSize = 10
				and barchart1 = ref UP.EmptyBarchart
				and legend1 = ref LE.EmptyLegend 
				and title1 = if title then 
								ref(UP.CreateTitle(pg, CNX, CNY, CNH))
							else ref 0
				and CNI = DSStr_CreateNode {page = pg, x = CNX, 
											y = CNY, w = CNW, 
											h = CNH, shape = 1}
			in 
			(
				if Legend 
				then
				(
					legend1 :=  
							LE.CreateLegendRegion (pg, LRX, CNY, LRW, 
												   PatternSize, col);

					LE.upd_rtag (
						(!legend1), 
						UP.CreateIndexStrList(
							"pname", 1, 
							(length(#PatternList (!legend1)) + 1)));

					DSStr_MakeNodeIntoRgn { obj = ! (# Id (!legend1)), 
											parent = CNI }  
				)
				else ();

				DSWtAttr_LineThickness { obj = CNI, thick = 2 };

				DSWtAttr_LineType { obj = CNI, line = 5 };

				barchart1 :=  
						UP.create_bc pg row col PRX PRY PRW PRH BRH Expr 
								     NValue PValue Upper Lower  GridDyn 
									 GridVisible MaxValue MinValue
								  	 GridNo Dist legend1 title1 move 
									 CNW;
 
				DSStr_MakeNodeIntoRgn { obj = !(# PRI (!barchart1)), 
										parent = CNI }; 

				DSStr_MakeNodeIntoRgn { obj = !title1, parent = CNI };

				(!barchart1)
			)
			end


	fun	hist_init {name = bc, init = n} = 
		UP.InitRow(n, !bc) 


	fun hist_row { name = bc, new_row = new_list } =
		UP.hist_row (new_list, !bc)

	fun hist_chart { name = bc, values = values } =
		UP.hist_chart (values, !bc)


	(*	Moves the text "up" in list. Put in the text 's' in last element *)

	fun	switchtext (id::nil, s) =
			DSText_Put{obj = id, text = s}
	 |	switchtext (id::idl, s) =
			let	val st = ref ""
			in
			(
				st := DSText_Get(hd(idl));
				DSText_Put{obj = id, text = !st};
				switchtext (idl, s)
			)
			end
	 |	switchtext (_,_) =  ()(*error*)


	(* 	
		Inserts text 's' in the next available id. If no available, 
		the text in the ids, are moved to make room for 's'
	*)

	fun	insertnext (nil, idl, s) = 
			switchtext(idl, s)
	 |	insertnext (id::idlist, idl, s) =
			let val l = ref 0
			in
			(
				l := DSText_GetLength(id);
				if ((!l) = 0) then
					(DSText_Put{obj = id, text = s})
				else
					(insertnext (idlist, idl, s)) 
			)
			end


	(*	Updates bar name regions for history chart *)

	fun	hist_bntag { name = bc: (int BARCHART) ref, tag = s } =
			let val bl = (#RowTag (!bc))
			in
				insertnext(!bl, !bl, s)
			end


	(* Updates more bar name regions for history charts *)

	fun hist_bntags { name = bc, tags = nil } = ()
	 |	hist_bntags { name = bc: (int BARCHART) ref, tags = s::sl } =
	(
		hist_bntag { name = bc, tag = s };
		hist_bntags { name = bc, tags = sl }
	)


	fun hist_title { name = bc: (int BARCHART) ref, title = title } =
		UP.upd_title(!bc, title)

	(* 	Updates legend (part name) regions for history chart *)

	fun	hist_ltag { name = bc: (int BARCHART) ref, tags = l } = 
		LE.upd_rtag (!(#Legend (!bc)), l)


	(*	Creates a bar chart *)

	fun SCcreate { BarHeight = BRH,	height = CNH, width = CNW, x = CNX, 
				   y = CNY, NoOfParts = col, BarName = Expr, 
				   FixedNo = GridDyn, IdealGN = GridNo, Dist = Dist: int, 
				   TicksOnly = TicksOnly, Legend = Legend, 
				   LowerValue = Lower, Max = MaxValue: int, 
				   Min = MinValue: int, NoOfBars = row, 
				   Title = title, UpperValue = Upper, 
				   PosValue = PValue, NegValue = NValue } = 
		create_barchart { title = title, move = 1,
						  page = DSStr_GetCurPage (),
						  cn_x = CNX, cn_y = CNY, row = row,
					      col = col, ch_w = CNW, ch_h = CNH,
						  bar_h = BRH, max_val = MaxValue, min_val = MinValue,
						  grid_no = GridNo, dist = Dist, expr = Expr, 
						  pval_reg = PValue, nval_reg = NValue, 
						  up_grid = Upper, low_grid = Lower, legend = Legend, 
						  grid_dyn = GridDyn, grid_vis = not(TicksOnly)
						} : int BARCHART

	fun HCcreate { BarHeight = BRH,	height = CNH, width = CNW, x = CNX, 
				   y = CNY, NoOfParts = col, BarName = Expr, 
				   FixedNo = GridDyn, IdealGN = GridNo, Dist = Dist: int, 
				   TicksOnly = TicksOnly, Legend = Legend, LowerValue = Lower,
 				   Max = MaxValue: int, Min = MinValue: int, NoOfBars = row, 
				   Title = title, Move = move, UpperValue = Upper, 
				   PosValue = PValue, NegValue = NValue } = 
		create_barchart { title = title, move = move, 
						  page = DSStr_GetCurPage (),
						  cn_x = CNX, cn_y = CNY, row = row,
					      col = col, ch_w = CNW, ch_h = CNH,
						  bar_h = BRH, max_val = MaxValue, min_val = MinValue,
						  grid_no = GridNo, dist = Dist, expr = Expr, 
						  pval_reg = PValue, nval_reg = NValue, 
						  up_grid = Upper, low_grid = Lower, legend = Legend, 
						  grid_dyn = GridDyn, grid_vis = not(TicksOnly)
						} : int BARCHART

	fun	upd_col { name = bc, column = col, row_values = colist } = 
		 ( UP.upd_col (col, colist, !bc))

	fun upd_ltag { name = bc : (int BARCHART) ref, tags = tags } =
		LE.upd_rtag (!(#Legend (!bc)), tags)

	fun	upd_row { name = bc, row = row, column_values = row_list } = 
		UP.upd_row (row, row_list, !bc)

	fun upd_chart { name = bc: (int BARCHART) ref, values = values } =
		UP.upd_chart (values, !bc)

	fun	upd_bntag { name = bc: (int BARCHART) ref, tags = l} = 
		( UP.upd_ltag (!bc, l))

	fun	upd_title { name = bc: (int BARCHART) ref, title = title} = 
		( UP.upd_title (!bc, title))

	fun delete (bc: (int BARCHART) ref) = 
		DSStr_DeleteObject (DSStr_GetParent (!(# PRI (!bc))))

	fun clear_chart (bc: (int BARCHART) ref) =
	(
		let val TagDist = if (#GridDyn (!bc)) then 
							UT.CalcDist(
								((#StartMax (!bc)) - (#StartMin (!bc))), 
								(#IdealGN (!bc)))
					  	  else (#Dist (!bc))
		in
		(
			let val PosNgn = if (#GridDyn (!bc)) then 
								UT.CalcGN(
								(#StartMax (!bc)), 
								UT.round(
								  real((#StartMax (!bc)) * (#IdealGN (!bc))) / 
								  real((#StartMax (!bc)) - (#StartMin (!bc)))),
								TagDist)
						 	else UT.ceiling(real(#StartMax (!bc)) / 
											real(TagDist))
				and NegNgn = if (#GridDyn (!bc)) then
								UT.CalcGN(
								~(#StartMin (!bc)), 
								(#IdealGN (!bc)) - 
								UT.round(
								  real((#StartMax (!bc)) * (#IdealGN (!bc))) / 
								  real((#StartMax (!bc)) - (#StartMin (!bc)))),
 								TagDist)
						 	 else UT.ceiling(real(~(#StartMin (!bc))) / 
										 	 real(TagDist))
			in
			(	
				let val PosNgn = if ((PosNgn = 0) andalso (NegNgn = 0)) then 1
							 	 else PosNgn
				in
				(	
					let val NMin = ~(TagDist * NegNgn)
						and NMax = TagDist * PosNgn
					in
					(
						let val MinWidth = UT.round(real((!(#PRW (!bc))) * 
										 		       (~(NMin))) / 
													real(NMax - NMin))
							and MaxWidth = ((!(#PRW (!bc))) - 
											UT.round(real((!(#PRW (!bc))) * 
										  				(~(NMin))) /
								 					 real(NMax - NMin)))
						in
						(
							UP.init_all ((!bc), (length(!(#PRowList (!bc)))));

							DSWtAttr_ObjectPosition{
								obj = !(#Axis (!bc)),
								x = !(#PRX (!bc)) + 
									UT.round(real(MinWidth - MaxWidth) / 2.0),
								y = !(#PRY (!bc))};  

							UT.DeleteIdList (!(#GridList (!bc)));
							(#GridList (!bc)) := (
								UP.create_grids (
										!(#Pg (!bc)), !(#PRI (!bc)), 
									  	!(#PRX (!bc)), !(#PRY (!bc)), 
									  	!(#PRW (!bc)), !(#PRH (!bc)), 
									  	NMax, NMin, MaxWidth, MinWidth, 
									  	PosNgn, NegNgn, (PosNgn + NegNgn), 
									  	(#GridVisible (!bc)), 
										(!(#Lower (!bc)) <> nil), 
									  	(!(#Upper (!bc)) <> nil)));

							UT.DeleteIdList (!(#Upper (!bc)));
							(#Upper (!bc)) := (
								if (!(#Upper (!bc)) <> nil) then
								(
									UP.create_ulvalue (
										!(#Pg (!bc)), !(#PRI (!bc)), 
										!(#PRX (!bc)), 
								  		(!(#PRY (!bc)) - !(#PRH (!bc)) div 2 - 
										 16),
 										(!(#PRW (!bc)) div (PosNgn + NegNgn)), 
										NMin, MaxWidth, MinWidth, PosNgn, 
										NegNgn, (PosNgn + NegNgn), 
										TagDist)	
								)
								else nil);

							UT.DeleteIdList (!(#Lower (!bc)));
							(#Lower (!bc)) := (
								if (!(#Lower (!bc)) <> nil) then
								(
									UP.create_ulvalue (
										!(#Pg (!bc)), !(#PRI (!bc)), 
										!(#PRX (!bc)), 
										(!(#PRY (!bc)) + !(#PRH (!bc)) div 2 + 
										 16),
 										(!(#PRW (!bc)) div (PosNgn + NegNgn)), 
										NMin, MaxWidth, MinWidth, PosNgn, 
										NegNgn, (PosNgn + NegNgn), 
										TagDist)	
								)
								else nil);

							UP.InitValTags (!(#PValTagId (!bc)));
							UP.InitValTags (!(#NValTagId (!bc)));

							(#MaxValue (!bc)) := NMax;
							(#MinValue (!bc)) := NMin
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

	fun clear_hist (b: (int BARCHART) ref) =
	(
		clear_chart (b);
		(#CurLine (!b)) := 1
	)

	fun init (b : (int BARCHART) ref) =
		UP.init_bc (!b)

end;


structure RBC = 
	struct
	structure LE = LegendStr;
	structure UP = RealUpdateStr;
	structure UT = UtilStr;
	structure RUT = RealUtilStr;

	(* 
		Signature
  		sig
    		val HCcreate : {
					BarHeight:int,
					NoOfParts:int,
					Dist:real,
					FixedNo:bool,
					IdealGN:int,
					TicksOnly:bool,
					height:int,
					Legend:bool,
					LowerValue:bool,
					Max:real,
					Min:real,
					Move:int,
					NegValue:bool,
					PosValue:bool,
					NoOfBars:int,
					BarName:bool,
					Title:bool,
					UpperValue:bool,
					width:int,
					x:int,
					y:int} -> real BARCHART
    		val SCcreate : {
					BarHeight:int,
					NoOfParts:int,
					Dist:real,
					FixedNo:bool,
					IdealGN:int,
					TicksOnly:bool,
					height:int,
					Legend:bool,
					LowerValue:bool,
					Max:real,
					Min:real,
					NegValue:bool,
					PosValue:bool,
					NoOfBars:int,
					BarName:bool,
					Title:bool,
					UpperValue:bool,
					width:int,
					x:int,
					y:int} -> real BARCHART
    		val clear_chart : real BARCHART ref -> unit
    		val clear_hist : real BARCHART ref -> unit
    		val create_barchart : {
					bar_h:int,
					ch_h:int,
					ch_w:int,
					cn_x:int,
					cn_y:int,
					col:int,
					dist:real,
					expr:bool,
					grid_dyn:bool,
					grid_no:int,
					grid_vis:bool,
					legend:bool,
					low_grid:bool,
					max_val:real,
					min_val:real,
					move:int,
					nval_reg:bool,
					page:int,
					pval_reg:bool,
					row:int,
					title:bool,
					up_grid:bool} -> real BARCHART
    		val delete : real BARCHART ref -> unit
    		val empty_barchart : unit -> real BARCHART
    		val hist_chart : {name:real BARCHART ref,
							  values:(real list * bool) list} -> unit
    		val hist_init : {name:real BARCHART ref,init:int} -> unit
    		val hist_ltag : {name:real BARCHART ref,tags:string list} -> unit
    		val hist_row : {name:real BARCHART ref,new_row:real list} -> unit
    		val hist_bntag : {name:real BARCHART ref,tag:string} -> unit
    		val hist_title : {name:real BARCHART ref,title:string} -> unit
    		val init : real BARCHART ref -> unit
    		val insertnext : int list * int list * string -> unit
    		val switchtext : int list * string -> unit
    		val upd_chart : {name: real BARCHART ref,
							 values:(int * real list * bool) list} -> unit
    		val upd_col : {name:real BARCHART ref,column:int,
						   row_values:real list} -> unit
    		val upd_ltag : {name:real BARCHART ref,tags:string list} -> unit
    		val upd_row : {name:real BARCHART ref,column_values:real list,
						   row:int} -> unit
    		val upd_bntag : {name:real BARCHART ref,tags:string list} -> unit
    		val upd_title : {name:real BARCHART ref,title:string} -> unit
  		end
	*)

	(* modified Nov 18 by VP. Added: page parameter; uncurried; label fields *)

	(* parameter in the creation of a bar chart:
		title :		flag, if ture a title region will be created
		page :		the page on which the chart will be generated
		cn_x :		x-coordinate for the chart node
		cn_y :		y-coordinate for the chart node
		row: 		number of bars
		col: 		number of bar parts
		ch_w :		width of chart node
		ch_h :		height of the chart node
		bar_h : 	bar region height
		max_val : 	maximum value initially displayed
		min_val : 	minimum value initially displayed
		grid_no : 	number of ideal grid lines initial
		dist :		fixed dist if any
		expr: 		flag, true means to display row tag region
		pval_reg :	flag, true means to display positive value regions
		nval_reg :	flag, true means to display negative value regions
		up_grid : 	flag, true means to display upper value regions
		low_grid : 	flag, true means to display lower value region
		legend : 	true means Legend Region created and diplayed
		grid_dyn : 	flag, true means that grid dist dynamically changes 
				 	when the maximum value changes
		grid_vis : 	flag, true means to display whole grid line
	*)
	
	fun empty_barchart () = UP.EmptyBarchart;

	fun	create_barchart {title = title,
						 move = move,
				 		 page = pg,
				 		 cn_x = CNX,
				 		 cn_y = CNY,
				 		 row = row,
				 		 col = col, 
				 		 ch_w = CNW,
				 		 ch_h = CNH,
				 		 bar_h = BRH,
				 		 max_val = MaxValue,
						 min_val = MinValue,
				 		 grid_no = GridNo,
						 dist = Dist,
				 		 expr = Expr,
				 		 pval_reg = PValue,
						 nval_reg = NValue,
				 		 up_grid = Upper, 
				 		 low_grid = Lower,
				 		 legend = Legend,
				 		 grid_dyn = GridDyn,
				 		 grid_vis = GridVisible } =
			let	val LRX = CNX + (CNW * 3 div 8)
				and LRW = (20 * CNW) div 100
				and PRX = (
						if Expr (* 4% of CNW as room for Row Tag *)
						then
						(
							if Legend (* moves x-coord (9/80 * CNW) *)
						   	then
							(
								if ((PValue andalso NValue) orelse
									(not(PValue) andalso not(NValue)))
								then
								(
									(* CNX - (9/80 CNW) + (4% CNW) *)

									CNX - UT.round(real(29 * CNW) / 400.0)
								)
								else (* 2% for CNW as room for value tag *)
								(
									if not(PValue) then
									(
										(* CNX - (9/80 CNW) + (4% CNW) + 
										   (2% CNW) *)

										CNX - UT.round(real(21 * CNW) / 400.0)
									) 
									else (* not(PValue) *)
									(
										(* CNX - (9/80 CNW) + (4% CNW) - 
										   (2% CNW) *)

										CNX - UT.round(real(37 * CNW) / 400.0)
									)
								)
							)
						   	else
							(
								if ((PValue andalso NValue) orelse
									(not(PValue) andalso not(NValue)))
								then
								(
									(* CNX + (4% CNW) *)

									CNX + UT.round(real(4 * CNW) / 100.0)
								)
								else (* 2% for CNW as room for value tag *)
								(
									if not(PValue) then
									(
										(* CNX + (4% CNW) + (2% CNW) *)

										CNX + UT.round(real(6 * CNW) / 100.0)
									)
									else (* not(NValue) *)
									(
										(* CNX + (4% CNW) - (2% CNW) *)

										CNX + UT.round(real(2 * CNW) / 100.0)
									)
								)
							)
						)
						else
						(
							if Legend (* moves x-coord (9/80 * CNW) *)
						   	then
							(
								if ((PValue andalso NValue) orelse
									(not(PValue) andalso not(NValue)))
								then
								(
									(* CNX - (9/80 CNW) *)

									CNX - UT.round(real(9 * CNW) / 80.0)
								)
								else
								(
									if not(PValue) then
									(
										(* CNX - (9/80 CNW) + (4% CNW) *) 

										CNX - UT.round(real(37 * CNW) / 400.0)
									) 
									else (* not(NValue) *)
									(
										(* CNX - (9/80 * CNW) + (4% CNW) *)

										CNX - UT.round(real(53 * CNW) / 400.0)
									)
								)
							)
						   	else
							(
								if ((PValue andalso NValue) orelse
									(not(PValue) andalso not(NValue)))
								then
								(
									CNX
								)
								else
								(
									if not(PValue) then
									(
										(* CNX + (2% CNW) *)

										CNX + UT.round(real(2 * CNW) / 100.0)
									)
									else (* not(NValue) *)
									(
										(* CNX - (2% CNW) *)

										CNX - UT.round(real(2 * CNW) / 100.0)
									)
								)
							)
						)
					)
				and PRW = (if Legend then 
							(55 * CNW) div 100 else (70 * CNW) div 100)
				and PRH = (70 * CNH) div 100
				and PRY = (if title then
								CNY + (CNH div 50)
						   else 
								CNY)
				and PatternSize = 10
				and barchart1 = ref UP.EmptyBarchart
				and legend1 = ref LE.EmptyLegend 
				and title1 = if title then 
								ref(UP.CreateTitle(pg, CNX, CNY, CNH))
							 else ref 0
				and CNI = DSStr_CreateNode {page = pg, x = CNX, 
												y = CNY, w = CNW, 
												h = CNH, shape = 1}
			in 
			(
				if Legend 
				then
				(
					legend1 :=  
						LE.CreateLegendRegion (pg, LRX, CNY, LRW, 
											   PatternSize, col);

					LE.upd_rtag (
						(!legend1), 
						UP.CreateIndexStrList(
							"pname", 1, 
							(length(#PatternList (!legend1)) + 1)));

					DSStr_MakeNodeIntoRgn { obj = ! (# Id (!legend1)), 
											parent = CNI }  
				)
				else ();

				DSWtAttr_LineThickness { obj = CNI, thick = 2 };

				DSWtAttr_LineType { obj = CNI, line = 5 };

				barchart1 :=  
					UP.create_bc pg row col PRX CNY PRW PRH BRH Expr 
								 NValue PValue Upper Lower  GridDyn 
							     GridVisible MaxValue MinValue 
								 GridNo Dist legend1 title1 move CNW; 
					
				DSStr_MakeNodeIntoRgn { obj = !(# PRI (!barchart1)), 
										parent = CNI } ; 

				DSStr_MakeNodeIntoRgn { obj = !title1, parent = CNI };

				(!barchart1)
			)
			end


	(*	Moves the text "up" in list. Put in the text 's' in last element *)

	fun	switchtext (id::nil, s) =
			DSText_Put{obj = id, text = s}
	 |	switchtext (id::idl, s) =
			let	val st = ref ""
			in
			(
				st := DSText_Get(hd(idl));
				DSText_Put{obj = id, text = !st};
				switchtext (idl, s)
			)
			end
	 |	switchtext (_,_) =  ()(*error*)


	(* 	
		Inserts text 's' in the next available id. If no available, 
		the text in the ids, are moved to make room for 's'
	*)

	fun	insertnext (nil, idl, s) = 
			switchtext(idl, s)
	 |	insertnext (id::idlist, idl, s) =
			let val l = ref 0
			in
			(
				l := DSText_GetLength(id);
				if ((!l) = 0) then
					(DSText_Put{obj = id, text = s})
				else
					(insertnext (idlist, idl, s)) 
			)
			end


	fun SCcreate {	BarHeight = BRH,height = CNH, width = CNW, x = CNX, 
					y = CNY, NoOfParts = col, 
					BarName = Expr, FixedNo = GridDyn, IdealGN = GridNo, 
					Dist = Dist: real, TicksOnly = TicksOnly, Legend = Legend, 
					LowerValue = Lower, Max = MaxValue: real,
					Min = MinValue: real, NoOfBars = row, 
					Title = title, UpperValue = Upper, 
					PosValue = PValue, NegValue = NValue } = 
			create_barchart { title = title, move = 1,
							  page = DSStr_GetCurPage (),
						  	  cn_x = CNX, cn_y = CNY, row = row,
						  	  col = col, ch_w = CNW, ch_h = CNH,
						  	  bar_h = BRH, max_val = MaxValue, 
							  min_val = MinValue, grid_no = GridNo, 
							  dist = Dist, expr = Expr,
						  	  pval_reg = PValue, nval_reg = NValue, 
							  up_grid = Upper, low_grid = Lower, 
							  legend = Legend, grid_dyn = GridDyn, 
							  grid_vis = not(TicksOnly)
							} : real BARCHART

	fun HCcreate {	BarHeight = BRH, height = CNH, width = CNW, x = CNX, 
					y = CNY, NoOfParts = col, 
					BarName = Expr, FixedNo = GridDyn, IdealGN = GridNo, 
					Dist = Dist: real, TicksOnly = TicksOnly, Legend = Legend, 
					LowerValue = Lower, Max = MaxValue: real,
					Min = MinValue: real, NoOfBars = row, 
					Title = title, Move = move,
					UpperValue = Upper, 
					PosValue = PValue, NegValue = NValue } = 
			create_barchart { title = title, move = move,
							  page = DSStr_GetCurPage (),
						  	  cn_x = CNX, cn_y = CNY, row = row,
						  	  col = col, ch_w = CNW, ch_h = CNH,
						  	  bar_h = BRH, max_val = MaxValue, 
							  min_val = MinValue, grid_no = GridNo, 
							  dist = Dist, expr = Expr,
						  	  pval_reg = PValue, nval_reg = NValue, 
							  up_grid = Upper, low_grid = Lower, 
							  legend = Legend, grid_dyn = GridDyn, 
							  grid_vis = not(TicksOnly)
							} : real BARCHART

	fun	upd_col {name = bc : (real BARCHART) ref, column = col, 
							row_values = colist} = 
		 ( UP.upd_col (col, colist, !bc))

	fun upd_ltag { name = bc : (real BARCHART) ref, tags = tags} =
		LE.upd_rtag (!(#Legend (!bc)), tags)

	fun	upd_row { name = bc : (real BARCHART) ref, row = row, 
				  column_values = row_list} = 
		UP.upd_row (row, row_list, !bc)

	fun	upd_chart { name = bc : (real BARCHART) ref, values = values} = 
		UP.upd_chart (values, !bc)

	fun	upd_bntag {name = bc : (real BARCHART) ref, tags = l} = 
		( UP.upd_ltag (!bc, l))

	fun	upd_title {name = bc : (real BARCHART) ref, title = title} = 
		( UP.upd_title (!bc, title))

	fun delete (bc: (real BARCHART) ref) = 
		DSStr_DeleteObject (DSStr_GetParent (!(# PRI (!bc)))) 

	fun	hist_init { name = bc, init = n } = 
		UP.InitRow(n, !bc) 

	fun	hist_ltag { name = bc: (real BARCHART) ref, tags = l } = 
			LE.upd_rtag (!(#Legend (!bc)), l)

	fun hist_row { name = bc, new_row = new_list } =
		UP.hist_row (new_list, !bc)

	fun hist_chart { name = bc, values = values } =
		UP.hist_chart (values, !bc)

	fun	hist_bntag { name = bc: (real BARCHART) ref, tag = s } =
			let val bl = (#RowTag (!bc))
			in
				insertnext(!bl, !bl, s)
			end

	(* Updates more bar name regions for history charts *)

	fun hist_bntags { name = bc, tags = nil } = ()
	 |	hist_bntags { name = bc: (real BARCHART) ref, tags = s::sl } =
	(
		hist_bntag { name = bc, tag = s };
		hist_bntags { name = bc, tags = sl }
	)

	fun hist_title {name = bc: (real BARCHART) ref, title = title } =
			UP.upd_title(!bc, title)

	fun clear_chart (bc: (real BARCHART) ref) =
	(
		let val TagDist = if (#GridDyn (!bc)) then 
							RUT.CalcDist(
								((#StartMax (!bc)) - (#StartMin (!bc))), 
								(#IdealGN (!bc)))
					  	  else (#Dist (!bc))
		in
		(
			let val PosNgn = if (#GridDyn (!bc)) then 
								RUT.CalcGN(
								(#StartMax (!bc)), 
								UT.round(
								  ((#StartMax (!bc)) * real(#IdealGN (!bc))) / 
								  ((#StartMax (!bc)) - (#StartMin (!bc)))),
								TagDist)
						 	else UT.ceiling((#StartMax (!bc)) / TagDist)
				and NegNgn = if (#GridDyn (!bc)) then
								RUT.CalcGN(
								~(#StartMin (!bc)), 
								(#IdealGN (!bc)) - 
								UT.round(
								  ((#StartMax (!bc)) * real(#IdealGN (!bc))) / 
								  ((#StartMax (!bc)) - (#StartMin (!bc)))),
 								TagDist)
						 	 else UT.ceiling((~(#StartMin (!bc))) / TagDist)
			in
			(	
				let val PosNgn = if ((PosNgn = 0) andalso (NegNgn = 0)) then 1
							 	 else PosNgn
				in
				(	
					let val NMin = ~(TagDist * real(NegNgn))
						and NMax = TagDist * real(PosNgn)
					in
					(
						let val MinWidth = UT.round((real(!(#PRW (!bc))) * 
										 (~(NMin))) / (NMax - NMin))
							and MaxWidth = ((!(#PRW (!bc))) - 
											UT.round((real(!(#PRW (!bc))) * 
										  (~(NMin))) / (NMax - NMin)))
						in
						(
							UP.init_all ((!bc), (length(!(#PRowList (!bc)))));

							DSWtAttr_ObjectPosition{
								obj = !(#Axis (!bc)),
								x = !(#PRX (!bc)) + 
									UT.round(real(MinWidth - MaxWidth) / 2.0),
								y = !(#PRY (!bc))};  

							UT.DeleteIdList (!(#GridList (!bc)));
							(#GridList (!bc)) := (
								UP.create_grids (
										!(#Pg (!bc)), !(#PRI (!bc)), 
									  	!(#PRX (!bc)), !(#PRY (!bc)), 
									  	!(#PRW (!bc)), !(#PRH (!bc)), 
									  	NMax, NMin, MaxWidth, MinWidth, 
									  	PosNgn, NegNgn, (PosNgn + NegNgn), 
									  	(#GridVisible (!bc)), 
										(!(#Lower (!bc)) <> nil), 
									  	(!(#Upper (!bc)) <> nil)));

							UT.DeleteIdList (!(#Upper (!bc)));
							(#Upper (!bc)) := (
								if (!(#Upper (!bc)) <> nil) then
								(
									UP.create_ulvalue (
										!(#Pg (!bc)), !(#PRI (!bc)), 
										!(#PRX (!bc)), 
								  		(!(#PRY (!bc)) - !(#PRH (!bc)) div 2 -
										 16),
 										(!(#PRW (!bc)) div (PosNgn + NegNgn)), 
										NMin, MaxWidth, MinWidth, PosNgn, 
										NegNgn, (PosNgn + NegNgn), 
										TagDist, (#GridDyn (!bc)))	
								)
								else nil);

							UT.DeleteIdList (!(#Lower (!bc)));
							(#Lower (!bc)) := (
								if (!(#Lower (!bc)) <> nil) then
								(
									UP.create_ulvalue (
										!(#Pg (!bc)), !(#PRI (!bc)), 
										!(#PRX (!bc)), 
										(!(#PRY (!bc)) + !(#PRH (!bc)) div 2 +
										 16),
 										(!(#PRW (!bc)) div (PosNgn + NegNgn)), 
										NMin, MaxWidth, MinWidth, PosNgn, 
										NegNgn, (PosNgn + NegNgn), 
										TagDist, (#GridDyn (!bc)))	
								)
								else nil);

							UP.InitValTags (!(#PValTagId (!bc)));
							UP.InitValTags (!(#NValTagId (!bc)));

							(#MaxValue (!bc)) := NMax;
							(#MinValue (!bc)) := NMin
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

	fun clear_hist (b: (real BARCHART) ref) =
	(
		clear_chart (b);
		(#CurLine (!b)) := 1
	)

	fun init (b : (real BARCHART) ref) =
		UP.init_bc (!b)
		
end;

type 'a SCHART = 'a BARCHART;

fun SC'decint () = ref (IBC.empty_barchart (): int SCHART);
fun SC'decreal () = ref (RBC.empty_barchart (): real SCHART);


(* 
_overload SC'create: { BarHeight: int, height: int, width: int, x: int,
					  y: int, NoOfParts: int, BarName: bool, FixedNo: bool,
					  IdealGN: int, Dist: 'a, TicksOnly: bool, 
					  Legend: bool, LowerValue: bool, Max: 'a, Min: 'a,
					  NoOfBars: int, Title: bool, UpperValue: bool, 
					  PosValue: bool, NegValue: bool
					} -> 'a SCHART 
						as IBC.SCcreate and RBC.SCcreate;

_overload SC'upd_col: { name: 'a SCHART ref, column: int, row_values: 'a list}
					  -> unit 
						as IBC.upd_col and RBC.upd_col;

_overload SC'upd_ltag: { name: 'a SCHART ref, tags: string list } -> unit 
						as IBC.upd_ltag and RBC.upd_ltag;

_overload SC'upd_row: { name: 'a SCHART ref, column_values: 'a list, 
					   row: int} -> unit 
						as IBC.upd_row and RBC.upd_row;

_overload SC'upd_chart: { name: 'a SCHART ref, 
						 values: (int * 'a list * bool) list } -> unit 
						as IBC.upd_chart and RBC.upd_chart;

_overload SC'upd_bntag: { name : 'a SCHART ref, tags : string list} -> unit
						as IBC.upd_bntag and RBC.upd_bntag;

_overload SC'upd_title: { name : 'a SCHART ref, title : string } -> unit
						as IBC.upd_title and RBC.upd_title;

_overload SC'delete : ('a SCHART) ref -> unit 
						as IBC.delete and RBC.delete;

_overload SC'clear : ('a SCHART) ref -> unit
						as IBC.clear_chart and RBC.clear_chart;

_overload SC'init : ('a SCHART) ref -> unit
						as IBC.init and RBC.init;
*)

type 'a HCHART = 'a BARCHART;

fun HC'decint () = ref (IBC.empty_barchart (): int HCHART);
fun HC'decreal () = ref (RBC.empty_barchart (): real HCHART);



(*
_overload HC'create: { BarHeight: int, height: int, width: int, x: int,
					  y: int, NoOfParts: int, BarName: bool, FixedNo: bool,
					  IdealGN: int, Dist: 'a, TicksOnly: bool, 
					  Legend: bool, LowerValue: bool, Max: 'a, Min: 'a,
					  NoOfBars: int, Title: bool, Move: int,
					  UpperValue: bool, PosValue: bool, NegValue: bool
					} -> 'a HCHART 
					as IBC.HCcreate and RBC.HCcreate;

_overload HC'hist_init: { name: 'a HCHART ref, init: int} -> unit 
					as IBC.hist_init and RBC.hist_init;

_overload HC'upd_ltag: { name : 'a HCHART ref, tags: string list} -> unit 
					as IBC.hist_ltag and RBC.hist_ltag;

_overload HC'upd_row: { name: 'a HCHART ref, new_row: 'a list} -> unit
					as IBC.hist_row and RBC.hist_row;

_overload HC'upd_chart: { name: 'a HCHART ref, values: ('a list * bool) list} 
						 -> unit
					as IBC.hist_chart and RBC.hist_chart;


_overload HC'upd_bntag: { name: 'a HCHART ref, tag: string} -> unit
					as IBC.hist_bntag and RBC.hist_bntag;

_overload HC'upd_bntags: { name: 'a HCHART ref, tags: string list } -> unit
					as IBC.hist_bntags and RBC.hist_bntags;

_overload HC'upd_title: { name: 'a HCHART ref, title: string} -> unit
					as IBC.hist_title and RBC.hist_title;

_overload HC'delete: 'a HCHART ref -> unit
					as IBC.delete and RBC.delete;

_overload HC'clear: 'a HCHART ref -> unit
					as IBC.clear_hist and RBC.clear_hist;

_overload HC'init: 'a HCHART ref -> unit
					as IBC.init and RBC.init
*)
