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
(*
  Module:      Net capture

  Description: Implementation of functions for instantiating the model 
               representation used for generating model-dependent code in the 
               state space tool

  TODO:        Consider moving checkNames to a different file
               
*)

functor CPN'NetCapture (structure CPN'InstTable : CPN'INSTTABLE) : CPN'NETCAPTURE = 
struct

    type Place = 
	 {
	  name      : string,
        sort      : string,
	  id        : string,
	  port      : bool,
	  fusion_id : string option
	 } 

    type Variable =
	 {
	  name : string,
	  sort : string
	 }

    type Transition = 
	 {
	  name             : string,
	  id               : string,
	  variables        : Variable list list,
	  free_variables   : Variable list,
	  neighbour_ports  : string list,
	  neighbour_fusion : bool
	 }

    type Subpage =
	 {
	  transition_name  : string,
	  transition_id    : string,
	  page_name        : string,
	  page_id          : string,
	  instances        : (int * int) list (* mapping from superpage instance number *)
	 }                                    (* to sub-page instance number            *)

    type Page = 
	 {
	  places      : Place list,
	  transitions : Transition list,
	  subpages    : Subpage list,
	  prime       : int,
	  name        : string,
	  instances   : int,
	  id:string
	 }

    datatype SortKind = unit_cs | bool_cs | int_cs | intinf_cs | real_cs |
         char_cs | string_cs | time_cs | enum_cs of string list | index_cs of
         string | list_cs of string | product_cs of (string * string) list |
         record_cs of (string * string) list | funsubset_cs of string |
         listsubset_cs of string | alias_cs of string | union_cs of (string *
         string) list

    type Sort =
       {
       name : string,
       timed : bool,
       sort : SortKind
       }
  
    type Net = Page list * Sort list

    val Net = ref (([], []):Net)

    fun getNet () = (!Net)

    local
        (* BuildPlaces takes the relevant information for each place
        * on a page from the PlaceTable in the simulator*)
        fun BuildPlaces CPN'x = 
        let
            fun BuildPlaces' nil = []
              | BuildPlaces' (CPN'x::CPN'xs) =
              let
                  val {kind,ext={page,name,cs = CPN'cs,...},int} = valOf (CPN'PlaceTable.peek CPN'x)
                  val is_port = (case kind of 
                                      CPN'PlaceTable.port _ => true
                                    | _ => false)
                  val fusion = (case kind of 
                                     CPN'PlaceTable.fusion CPN's => (SOME CPN's)
                                   | _ => NONE )  
              in
                  if kind = CPN'PlaceTable.group
                  then (CPN'debug ("Net Capture ignoring fusion group: "^CPN'x);
                  BuildPlaces' CPN'xs)
                  else 
                      {name = name, sort = CPN'cs, id = CPN'x, port = is_port, fusion_id = fusion}::
                      (BuildPlaces' CPN'xs)
              end
        in
            BuildPlaces' CPN'x
        end

        fun get_page_instance_info instance_info page_id =
            #2 (Option.valOf (
            List.find (fn (id, _) => page_id = id) instance_info))

        fun build_inst_list instance_info page node_id =
        let
            val page_inst_info = get_page_instance_info instance_info page
            val (_, page_inst_info') = 
                List.foldl
                (fn (SOME elm, (n, rest)) => (n + 1, (n, elm)::rest)
                  | (_, rest) => rest)
                (1, [])
                page_inst_info
            val page_inst_info'' =
                List.filter
                (fn (n, (page', node_id', number)) => node_id = node_id')
                page_inst_info'
        in
            List.map
            (fn (n, (page', node_id', number)) => (number, n))
            page_inst_info''
        end

        (* BuildSubpages takes the relevant information for each
        * transition from the TransitionTable in the simulator *)
        fun BuildSubpages instance_info nil = nil
          | BuildSubpages instance_info (CPN'x::CPN'xs) = 
          case (CPN'TransitionTable.peek CPN'x) of
               NONE => raise InternalError "BuildSubpages"
             | SOME (CPN'TransitionTable.substitution {name,subpage,...}) => 
                     {transition_name = name,
                     transition_id = CPN'x,
                     page_name = #name (#page (CPN'PageTable.find subpage)),
                     page_id = subpage,
                     instances = build_inst_list instance_info subpage CPN'x}::
                     BuildSubpages instance_info CPN'xs

             | SOME (CPN'TransitionTable.transition {page,name,input,output,...}) =>
                     BuildSubpages instance_info CPN'xs

        (* BuildTransitions takes the relevant information for each
        * transition from the TransitionTable in the simulator *)
        fun BuildTransitions nil = nil
          | BuildTransitions (CPN'x::CPN'xs) = 
          case (CPN'TransitionTable.peek CPN'x) of
               NONE => raise InternalError "BuildTransitions"
             | SOME (CPN'TransitionTable.substitution {name,page,...}) => 
                     BuildTransitions CPN'xs

             | SOME (CPN'TransitionTable.transition
             {page,name,input,output,groups, free_vars, ...}) =>
                     (let
                         val tneighbour_ports =
                             let
                                 fun get_ports_input nil = nil
                                   | get_ports_input ({place,arcs,no_of_tokens}::CPN'xs) =
                                   if CPN'PlaceTable.is_kind_port place 
                                   then place::get_ports_input CPN'xs
                                   else get_ports_input CPN'xs
                                 fun get_ports_output nil = nil
                                   | get_ports_output ({place,arcs}::CPN'xs) =
                                   if CPN'PlaceTable.is_kind_port place 
                                   then place::get_ports_output CPN'xs
                                   else get_ports_output CPN'xs
                             in
                                 (get_ports_input input)^^(get_ports_output output)
                             end

                         val tneighbour_fusion =
                             let
                                 fun getsurr() = 
                                 let
                                     fun get_inputs nil = nil
                                       | get_inputs ({place,arcs,no_of_tokens}::CPN'xs) =
                                       place::get_inputs CPN'xs
                                     fun get_outputs nil = nil
                                       | get_outputs ({place,arcs}::CPN'xs) =
                                       place::get_outputs CPN'xs
                                 in
                                     (get_inputs input)^^(get_outputs output)
                                 end

                                 fun fusion_exists nil = false
                                   | fusion_exists (place_id::ps) =
                                   let
                                       val {kind,...} = valOf (CPN'PlaceTable.peek place_id)
                                   in
                                       case kind of CPN'PlaceTable.fusion _ => true   
                                          | CPN'PlaceTable.group => true
                                          | _ => (fusion_exists ps)
                                   end

                             in
                                 fusion_exists(getsurr())
                             end
                         fun lookup_var name = {name = name, sort = #cs (CPN'VarTable.find name)}
                         val tfree_vars = List.map lookup_var free_vars
                         val tvariables = 
                             List.map ((List.map lookup_var) o #vars) groups
                      in
                          {name = name,
                          id = CPN'x,
                          variables = tvariables,
                          free_variables = tfree_vars,
                          neighbour_ports = tneighbour_ports,
                          neighbour_fusion = tneighbour_fusion} 
                          ::BuildTransitions CPN'xs
                      end)

          (* BuildPages makes a Net from the information in the
          * PageTable in the simulator *)
        fun BuildPages() = 
        let
            (* Returns the prefix of the string s that consists of
                                            * alphanumeric characters, underscores (_) and primes ('). 
                                            * Checks that the first char in the string is a letter. *)
            fun cut_name CPN's =
            let
                fun check' nil = nil
                  | check' (CPN'x::CPN'xs) =
                  (if (Char.isAlphaNum CPN'x) orelse 
                  (CPN'x=(#"_")) orelse 
                  (CPN'x=(#"'"))
                   then CPN'x::check' CPN'xs
                   else nil)

                fun check nil = raise InternalError "Page without a name in Net Capture"
                  | check (CPN'x::CPN'xs) =
                  (if (Char.isAlpha CPN'x)
                   then check' (CPN'x::CPN'xs)
                   else raise InternalError "Page name must start with a character in [a-zA-Z]")
            in
                String.implode(check (String.explode CPN's))
            end

            fun BuildPages' instance_info nil = nil
              | BuildPages' instance_info ((CPN'id,{page = {name,places,transitions,included,prime},
              decl,super_pages,sub_pages})::CPN'xs) =
              {places = BuildPlaces places,
              transitions = BuildTransitions transitions,
              subpages = BuildSubpages instance_info transitions,
              prime = prime,
              name = cut_name name,
              instances = if prime = 0
                          then CPN'PageTable.get_no_of_inst CPN'id
                          else prime,
              id = CPN'id}
              ::(BuildPages' instance_info CPN'xs)
        in
            BuildPages' (CPN'InstTable.get_page_structure()) (CPN'PageTable.list())
        end

        fun dep_sort (net : Page list) =
        let
            val page_table : (string, Page) HashTable.hash_table =
                HashTable.mkTable hashId (40, InternalError "page_table.find")
            val _ = List.foldl (fn (page as {id, ...}, _) =>
                HashTable.insert page_table (id, page)) () net
            fun build (id, rest) =
                case HashTable.find page_table id
                  of NONE => rest
                   | SOME (page as {subpages, ...}) =>
                           let
                               val _ = HashTable.remove page_table id
                               val rest' = List.foldl
                               (fn ({page_id, ...}, rest) => build (page_id, rest))
                               rest subpages
                           in
                               page::rest'
                           end
        in
            List.rev (List.foldl (fn ({id,...}, rest) => build (id, rest)) [] net)
        end

  fun BuildSorts () =
  let
    fun tranformCS (CPN'CSTable.unit_cs _) = unit_cs
      | tranformCS (CPN'CSTable.bool_cs _) = bool_cs
      | tranformCS (CPN'CSTable.int_cs _) = int_cs
      | tranformCS (CPN'CSTable.intinf_cs _) = intinf_cs
      | tranformCS (CPN'CSTable.real_cs _) = real_cs
      | tranformCS (CPN'CSTable.char_cs _) = char_cs
      | tranformCS (CPN'CSTable.string_cs _) = string_cs
      | tranformCS (CPN'CSTable.time_cs) = time_cs
      | tranformCS (CPN'CSTable.enum_cs values) = enum_cs values
      | tranformCS (CPN'CSTable.index_cs {idx, ...}) = index_cs idx
      | tranformCS (CPN'CSTable.list_cs {cs = CPN'cs, ...}) = list_cs CPN'cs
      | tranformCS (CPN'CSTable.product_cs values) = product_cs values
      | tranformCS (CPN'CSTable.record_cs values) = record_cs values
      | tranformCS (CPN'CSTable.funsubset_cs {cs = CPN'cs, ...}) = funsubset_cs
      CPN'cs
      | tranformCS (CPN'CSTable.listsubset_cs {cs = CPN'cs, ...}) =
      listsubset_cs CPN'cs
      | tranformCS (CPN'CSTable.alias_cs sort) = alias_cs sort
      | tranformCS (CPN'CSTable.union_cs values) = union_cs values
  in
    List.map
    (fn (name, {timed, kind, ... } : CPN'CSTable.item) =>
    { name = name, timed = timed, sort = tranformCS kind }
    ) (CPN'CSTable.listItemsi ())
  end

  fun dep_sort_sort sorts =
  let
    val sort_table : (string, Sort) HashTable.hash_table =
      HashTable.mkTable hashId (40, InternalError "sort_table.find")
    val _ = List.foldl (fn (sort as {name, ...}, _) =>
    HashTable.insert sort_table (name, sort)) () sorts
    fun build (name, rest) =
      case HashTable.find sort_table name
        of NONE => rest
         | SOME sort=>
             let
               val _ = HashTable.remove sort_table name
               fun mapper (list_cs other, rest) = build (other, rest)
                 | mapper (product_cs children, rest) =
                 List.foldl (fn ((_, name), rest) => build (name, rest)) rest children
                 | mapper (record_cs children, rest) =
                 List.foldl (fn ((_, name), rest) => build (name, rest)) rest children
                 | mapper (funsubset_cs other, rest) = build (other, rest)
                 | mapper (listsubset_cs other, rest) = build (other, rest)
                 | mapper (alias_cs other, rest) = build (other, rest)
                 | mapper (union_cs children, rest) =
                 List.foldl (fn ((_, name), rest) => build (name, rest)) rest children
                 | mapper (_, rest) = rest
               val rest' = mapper (#sort sort, rest)
             in
               sort::rest'
             end
  in
    List.rev (List.foldl (fn ({name, ...}, rest) => build (name, rest)) [] sorts)
  end

    in

        fun initNet () = Net := (dep_sort (BuildPages()),
                                 dep_sort_sort (BuildSorts()))

    end (* local *)

    (* Function used to check uniqueness of names which is assumed in the later 
       model-dependent code generation *)
    local
        exception illegalname of string 
        val NodeNames = CPN'AvlTree.AvlNew() : unit CPN'AvlTree.avltree
        val PageNames = CPN'AvlTree.AvlNew() : unit CPN'AvlTree.avltree
    in
        fun checkNames () = 
            (CPN'AvlTree.AvlReset PageNames; (* TODO: is this needed? *)

             List.map
		 (fn {places = places, transitions, name, subpages, ...} =>
		     (if name=""
                      then raise illegalname ("Page with no name")
		      else CPN'AvlTree.AvlInsert PageNames (name,())
			   handle CPN'AvlTree.ExcAvlInsert => 
				  (CPN'debug ("Non-unique page name: "^name);
				   raise illegalname ("Non-unique page name: "^name));
		      
		      CPN'AvlTree.AvlReset NodeNames; (* TODO: is this needed? *)

		      List.map
			  (fn {name = "", ...} => raise illegalname ("Place without name")
			    | {name = name, ...} =>
                              CPN'AvlTree.AvlInsert NodeNames (name,())
                              handle CPN'AvlTree.ExcAvlInsert =>
				     (CPN'debug ("Non-unique place name: "^name^" on page: "^name);
				      raise illegalname ("Non-unique place name: "^name^" on page: "^name)))
                           places;
		      
		      List.map
			  (fn {name = "", ...} => raise illegalname "Transition without name"
                            | {name = name, ...} =>
                              CPN'AvlTree.AvlInsert NodeNames (name,())
                              handle CPN'AvlTree.ExcAvlInsert =>
                                     (CPN'debug ("Non-unique trans name: "^name^" on page: "^name);
                                      raise illegalname ("Non-unique trans name: "^name^" on page: "^name)))
                          transitions;

		      List.map
			  (fn {transition_name = "", ...} => raise illegalname "Transition without name"
                            | {transition_name = name, ...} =>
                              CPN'AvlTree.AvlInsert NodeNames (name,())
                              handle CPN'AvlTree.ExcAvlInsert =>
                                     (CPN'debug ("Non-unique trans name: "^name^" on page: "^name);
                                      raise illegalname ("Non-unique trans name: "^name^" on page: "^name)))
                          subpages))
                 (#1 (!Net)); ())
    end (* local *)
end (* struct *)
