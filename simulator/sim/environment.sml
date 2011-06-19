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
(* File: environment.sml
 *
 * Facilities to interface the SML/NJ internal compiler structures.
 *)

structure CPN'Env : sig

    exception Size of int;

    val init : unit -> unit;
    (* initialise with a snapshot of the current top-level env. *)

    val remove_decl : CPN'Id.id -> unit;
    (* remove the symbols associated with the string parameter from 
     * the environment *)
	
    val exec : string list -> unit
    val use_string : string list -> unit
    val use_string_no_exn_filter : string list * bool -> unit
    val use_file : CPN'Id.id * string -> 
	CPN'Id.id * string * (string list * string list)
    val use_file_dep :  CPN'Id.id * string -> unit
    val is_decl : string -> string option
    val str_cmp : string -> string * string -> bool
    val is_def  : string -> bool
    val is_legal_ms : string -> string -> bool
    val is_bindable_cs : string -> bool

end = struct

    open  Compiler.Environment Compiler.Symbol

    exception Size of int;
    (* the symtable holds all symbols captured with the last snapshot: *)

    exception Compile of string;

    val topLevel = Compiler.EnvRef.loc
    
    fun cur_env () = Compiler.EnvRef.listBoundSymbols()
		
    (* clear tables and take snapshot of current environment: *)
    fun init () = CPN'Dep.clear_defs ();
		
    (* remove symbols: *)
    fun clean_up [] = ()
      | clean_up to_be_removed = 
     let
          fun rm_elm _ [] = []
            | rm_elm elm (h::r)
            = if elm = h then r else h::(rm_elm elm r)
          fun remove [] res = res
            | remove (symb::rest) res = remove rest (rm_elm symb res)
      in
	    (#set (topLevel ()))
	    (Compiler.Environment.filterEnv
	     ((#get (topLevel ()))(), 
	      remove to_be_removed (cur_env ())))
      end

    (* remove all symbols based on a given declaration (named by the
     * string parameter): *)

    fun remove_decl decl = clean_up (CPN'Dep.remove_defs decl);
		  
(************************* Various Compile Funs *****************************)

local
(*    open Compiler.Compile *)

    val err = ref (nil: string list);
in
    (* First our own version of useStream; we cannot use useStream from 
     * Compile because we want to handle syntax errors and parse the 
     * messages on to the C-side; In the old version everything happened at 
     * stdin/out so this wasn't necessary.
     *)

    fun use_stream' str warn = let 
	
      val _ = err := nil;

      val str = String.concat ["val _ = 5;\n", str]
		
	(* The functions in Compiler.Compile only operates on modules
	 * so we compile using a dummy structure in the source: *)

	val source = Compiler.Source.newSource
	    ("",1,
(*
	     TextIO.openString (concat["structure CPN'X = struct ",str," end"]), 
*)

	     TextIO.openString str, 

	     false,
	     {consumer = fn s => (err:= (!err)^^[s]), 
	      flush = fn () => (), 
	      linewidth = fn () => 80})

	val theenvs = Compiler.Environment.staticPart(Compiler.EnvRef.combined())

	fun compile (source) 
	  = let
	      val ast = Compiler.MLParser.parse source ()
            val ast = case ast of
                           (Compiler.MLParser.PARSE result) => result
                         | Compiler.MLParser.ABORT => raise Compile "Aborted parsing"
                         | Compiler.MLParser.EOF => raise Compile "EOF while parsing"
                         | Compiler.MLParser.ERROR => raise Compile "Error parsing"
            val compInfo = Compiler.Compile.mkCompInfo {source = source,
            transform = fn any => any}
	      val baseEnvRefunSC = Compiler.EnvRef.pervasive
	      fun checkErrors s = 
		  if Compiler.CompInfo.anyErrors compInfo then raise Compile s else ()
	      val {static = statenv, dynamic = dynenv, symbolic = symenv} =
                Compiler.Environment.layerEnv((#get (topLevel ()))(), (#get baseEnvRefunSC)())
     in
         Compiler.Compile.compile { source = source, ast = ast, statenv =
         statenv, symenv = symenv, compInfo = compInfo, checkErr = checkErrors, splitting =
         Compiler.Control.LambdaSplitting.get(), guid = "CPN"}
	    end

	fun drop_string (#"\""::rest) = rest
	  | drop_string (_::rest) = drop_string rest
	  | drop_string [] = [];

	fun has_warning (#"W"::(#"a"::(#"r"::(#"n"::(#"i"::(#"n"::(#"g"::(#":" :: _)))))))) = true
	  | has_warning (#"\""::rest) = has_warning (drop_string rest)
	  | has_warning (_::rest) = has_warning rest
	  | has_warning [] = false


    in
	(compile (source)
	 handle Compile s  => (Compiler.Source.closeSource source;
                               err::= "\n"^s; raise Compile (concat(!err)))
	      | exn => (Compiler.Source.closeSource source;
                        err::= "\nUncaught exception 1: "^(exnName exn);
			raise Compile (concat(!err)));
         Compiler.Source.closeSource source;
	 if warn andalso
	    ((not(!CPN'Settings.ignore_warnings)) andalso 
	     (has_warning (explode(concat(!err)))))
	 then
	     raise Compile (concat(!err))
	 else
	     (* The compilation succeeded, so use the native useStream
	      * to evaluate at toplevel: *)
	     Compiler.Interact.useStream (TextIO.openString str))
    end

    (* Send text to be compiled/executed by the interactive compiler.
     * Raises the Error exception if an error occurred. *)
    fun exec things = 
        (Compiler.Interact.useStream (TextIO.openString (String.concat things)))
        handle Compiler.Interact.ExnDuringExecution exn => raise exn

    fun use_string ls = let
	val str = concat ls
    in
       (CPN'CodeGen.dump_source str;
	use_stream' str false)
	       handle Compile s => 
		   raise InternalError ("Compile error when generating code: "^s^"\n"^str^"\n")
		    | InternalError msg => 
		       raise InternalError ("Exception InternalError("^msg^
					    ") raised when executing code:\n"^str)
		    | exn => 
		       raise InternalError ("Exception "^(exnName exn)^
					    " raised when generating code:\n"^str)
    end

    (* Simple use_string where no exceptions are filtered *)
    fun use_string_no_exn_filter (ls, warn) = let
	val str = concat ls
    in
	CPN'CodeGen.dump_source str;
	use_stream' str warn
    end

    local
	val StoreFileDep = ref (([],[]) : (string list * string list))
    in
	fun read_store () = (!StoreFileDep) before (StoreFileDep := ([],[]));
	    
	fun use_file_dep (id, str)
	    = let
		  val fid = TextIO.openIn str;
		  val str = TextIO.inputAll fid;
		  val _ = TextIO.closeIn fid
	      in
		  StoreFileDep := CPN'Dep.find_dependencies (id,str)
	      end
    end


    fun use_file (id,filenameexp)
	= let
	      val str = concat ["\n val _ = CPN'use (",filenameexp,");"]
	  in
	      (CPN'CodeGen.dump_source str;
	       use_stream' str false;
	       (id,"",
		if !CPN'Settings.use_record_symbols
		    then  (use_stream' (concat ["val _ = CPN'Env.use_file_dep (\"",
						id,"\",",filenameexp,");"]) false;
			   read_store())
		else ([],[])))
	      handle Compile s => (id,s,([],[]))
		   | IO.Io {name=s,...} => (id,s,([],[]))
		   | exn => (id,"\nUncaught exception 2: "^(exnName exn),([],[]))
	  end


    fun is_decl str =
      let
	  val err = ref (nil: string list);
      in
	  let
	      val src = Compiler.Source.newSource
			    ("",
			     0,
			     TextIO.openString ("structure CPN'X = struct "^str^" end"),
			     false,
			     {consumer= fn s => (err:= (!err)^^[s]), 
			      flush= fn () => (), linewidth = fn () => 80})
	      val ast = Compiler.MLParser.parse src ()
            val ast = case ast of
                           (Compiler.MLParser.PARSE result) => result
                         | Compiler.MLParser.ABORT => raise Compile "Aborted parsing"
                         | Compiler.MLParser.EOF => raise Compile "EOF while parsing"
                         | Compiler.MLParser.ERROR => raise Compile "Error parsing"
            val compInfo = Compiler.Compile.mkCompInfo {source = src,
            transform = fn any => any}
	      val baseEnvRefunSC = Compiler.EnvRef.pervasive
	      fun checkErrors s = 
		  if Compiler.CompInfo.anyErrors compInfo then raise Compile s else ()
	      val {static=statenv,...} = Compiler.Environment.layerEnv((#get
            (topLevel ()))(),
									   (#get baseEnvRefunSC)())
	  in
	      ((Compiler.Compile.elaborate{ast=ast,statenv=statenv,compInfo=compInfo,
            guid = "CPN"}; NONE)
	       before checkErrors "Elaborate failure (is_decl)")
	  end
	      handle Compile _ => SOME(concat (!err))
      end

    fun str_cmp cmp (a,b) = 
	let
	    fun rm_ws [] = []
	      | rm_ws (c::cs) = 
		if Char.isSpace c then (rm_ws cs) else c::cs

	    (* remove trailing and leading white spaces *)
	    val a' = implode (rm_ws(rev(rm_ws(rev (explode a)))))
	    val b' = implode (rm_ws(rev(rm_ws(rev (explode b)))))
	in
	    if a'=b'
	    then true
	    else ((exec
		      ["val _ = if (",a,") ",cmp," (",b,
		       ") then true else raise Match"];
		      true) handle _ => false)
	end
	    
    fun is_def f =
	(exec ["val _ = ",f,";"];
	 true) handle _ => false

    fun is_legal_ms cs exp =
	(exec
	 ["val _ = if (CPN'MS.legal_ms ",cs,".legal (",exp,
	  ")) then true else raise Match"];
	 true) handle _ => false

    fun is_bindable_cs cs =
	if List.exists (fn d => d="all") (#declare(CPN'CSTable.find cs)) then 
            (* It always possible to bind from the color-set, if the user 
	     * has declare the all declaration *)
	    true
	else
            (* It is posible to bind from the color-set, if the size of it
	     * is less than bindable_cs_size (default 100) *)
	    (exec 
	     ["val _ = raise CPN'Env.Size (",cs,".size());"]; false) 
	    handle Size n => (n <= !CPN'Settings.bindable_cs_size)
		 | _ => false

end
    
end (* CPN'env *)
