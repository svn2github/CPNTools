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
signature STORAGE =
sig
    type item 
    type 'a storage
    type init_options
    type id

    val emptyStorage : init_options -> 'a -> 'a storage
    (* The empty storage *)

    val add : 'a storage * item -> id * bool * 'a storage
    (* Insert an item. *)

    val addList : 'a storage * item list -> (id * bool) list * 'a storage
    (* Insert items from list. *)

    val contains: 'a storage * item -> bool
    (* Return true if and only if item is an element in the storage *)

    val contains': 'a storage * id -> bool
    (* Return true if and only if item is an element in the storage *)

    val isEmpty : 'a storage -> bool
    (* Return true if and only if the storage is empty *)
    val numItems : 'a storage -> int
    (* Return the number of items in the table *)

    val getTag : 'a storage * item -> 'a

    val getTag' : 'a storage * id -> 'a

    val setTag : 'a storage * item * 'a -> 'a storage

    val setTag' : 'a storage * id * 'a -> 'a storage
end

signature EXPLICIT_STORAGE =
sig
    include STORAGE

    val lookup : 'a storage * id -> item

    val map : (item * 'a -> item) -> 'a storage -> 'a storage
    (* Create a new storage by applying a map function to the elements
    * of the storage.
    *)

    val app : (item * 'a -> unit) -> 'a storage -> unit
    (* Apply a function to the entries of the storage 
    * in increasing order
    *)

    val foldl : (item * 'a * 'b -> 'b) -> 'b -> 'a storage -> 'b
    (* Apply a folding function to the entries of the storage 
    * in increasing order (if applicable)
    *)

    val partition : (item * 'a -> bool) -> 'a storage -> ('a storage * 'a storage)

    val filter : (item * 'a -> bool) -> 'a storage -> 'a storage

    val exists : (item * 'a -> bool) -> 'a storage -> bool

    val find : (item * 'a -> bool) -> 'a storage -> item option
end

signature REMOVE_STORAGE =
sig
    include STORAGE

    val delete : 'a storage * item -> 'a storage
    (* Remove an item. Raise NotFound if not found. *)

    val delete' : 'a storage * id -> 'a storage
end

signature EXPLICIT_REMOVE_STORAGE =
sig
    include EXPLICIT_STORAGE

    val delete : 'a storage * item -> 'a storage
    (* Remove an item. Raise NotFound if not found. *)

    val delete' : 'a storage * id -> 'a storage
end
