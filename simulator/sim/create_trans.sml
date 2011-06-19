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
(* File: create_trans.sml
*
* Creation of transition instances.
*)

functor CPN'CreateTransition(structure InstTable: CPN'INSTTABLE
and Decl: CPN'DECL): CPN'CREATETRANSITION = struct

structure Time = InstTable.RepTable.Time

local
open CPN'TransitionTable;

fun num_groups nil = 0
| num_groups ({vars=nil,bops=_}::grps) = 0+(num_groups grps)
| num_groups ({vars=_,bops=_}::grps) = 1+(num_groups grps)

val no_of_transitions = ref 0
val total_transitions = ref "0"

fun display_status_bar () =
(inc no_of_transitions; ())

fun gen_vars_record (groups,free_vars,code_reg,tail) = 
let
fun f (a,b) = ","::a::"="::a::b

val tail1 = 
case code_reg of
SOME {input,output,action} => 
(foldr f (""::"}"::tail) output)
| _ => (""::"}"::tail);
in
"{"::tl(foldr (fn ({vars,bops},tail2) => foldr f tail2 vars)
(foldr f tail1 free_vars) groups)
end

(******************** generate bind holder ********************)

fun gen_bh_ref (groups,tail) = let
(* Generates the bind holderes, one for each varaible group.
*
* val CPN'bh1 = ref(nil: {v1: cs1,...,vn1: csn1} list)
*     ...
* val CPN'bhn = ref(nil: {v1: cs1,...,vnm: csnm} list)
*)
fun gen (_, nil, tail) = tail
| gen (i, {vars=nil,bops=_}::grps, tail) = 
"\n val CPN'bh"::(Int.toString i)::" = ref nil"::gen(i+1,grps,tail)
| gen (i, {vars,bops=_}::grps, tail) =
"\n val CPN'bh"::(Int.toString i)::" = ref(nil: {"::
tl(foldr (fn (a,b) => ","::a::": "::(#cs(CPN'VarTable.find a))::b) 
(""::"} list)"::gen(i+1,grps,tail)) vars)
in
"\n val CPN'whole_binding = ref false"::gen(1,groups,tail)
end

(******************** generate traverse function ********************)

fun gen_trv (input,init_cs,tail) = let 
(* Generates the calls, that traverse all the input * places and
* initializes them, if all places are timed it looks like:
*
* val (CPN'enough_tokens, CPN'answer) = 
*     (CPN'RS.init_res(CPN'"cs"_rs_"var");
*      ...
*      each_timed_place("n1"<="p1".init (CPN'inst),
*                       "p1".next_time (CPN'inst), 
*      ...
*      each_timed_place("nn"<="pn".init (CPN'inst),
*                       "pn".next_time (CPN'inst), 
*      (true,is_disabled))...)))
*
* If an input place is untimed each_place is called instead of 
* each_timed_place.
*)

fun gen_init_cs (nil,tail) = tail
| gen_init_cs (rs::rss,tail) = 
(* Generates function calls that initializes the random sets used
* to bind free variables on input arcs *)
"\n "::"CPN'RS.init_res "::rs::";"::gen_init_cs(rss,tail)

fun gen_each (nil,tail) = "\n"::" (true,CPN'Sim.is_disabled)"::tail
| gen_each ({place, arcs, no_of_tokens}::ins,tail) = let
(* Generates call to the "each_place" function for each input
* place. The "each_place" is in the Sim structure *)
val {cs,ims,name,...} = CPN'PlaceTable.get_rep place
val no = case no_of_tokens of
SOME (n: int) => (Int.toString n)
| NONE => "0"
in
if CPN'CSTable.is_timed cs then
"\n "::"CPN'Sim.each_timed_place("::no::" <= "::
name::".init(CPN'inst),"::name::
".next_time (CPN'inst),"::gen_each(ins,")"::tail)
else
"\n "::"CPN'Sim.each_place("::no::" <= "::
name::".init(CPN'inst),"::gen_each(ins,")"::tail)
end
in
"\n val (CPN'enough_tokens, CPN'answer) = ("::
tl(gen_init_cs(init_cs,gen_each(input,""::")"::tail)))
end

(******************** generate binding function ********************)

fun gen_bindfun (t,groups,tail) = let

fun gen_bh_reset (groups,tail) = let
fun gen (_,nil,tail) = tail
| gen (i,grp::grps,tail) = "CPN'bh"::(Int.toString i)::":=nil;\n"::gen(i+1,grps,tail)
in
"CPN'whole_binding := false;\n"::gen (1,groups,tail)
end

fun gen_bfi (nil,_,tail) = tail
| gen_bfi ({bops,vars}::groups,grp,tail) = let

fun gen_bhi (vars,tail) =
(* Generate bindholder for variable group i *)
"{"::tl(foldr (fn (a,b) => ","::a::"="::a::b) ("}"::tail) vars)

fun gen_andalso_legals (vars,tail) = let
fun gen (v,tail) = let
val cs = #cs(CPN'VarTable.find v)
in
if CPN'CSTable.is_subtype cs then 
" andalso"::" ("::cs::".legal "::v::")"::tail
else
tail
end
in
if !CPN'Settings.use_legal_check then foldr gen tail vars
else tail
end

fun gen_if_else_legals (nil,_,_) = ("","")
| gen_if_else_legals (vars,if_part,else_part) =
case tl(gen_andalso_legals(vars,[" then"])) of
nil => ("","")
| andalsos => (concat(if_part::andalsos), else_part)

fun gen_keyfunc (timed,cs,order,allkeys) = let
(* determine whether the keys are in order with the place
* marking, and generate either a collect or a filter
* function *)

val components = CPN'CSTable.get_components cs

fun in_order (x::xs,{no,label=_,exp=_}::rkeys) =
(x=no) andalso in_order(xs,rkeys)
| in_order _ = true

fun gen_head tail = let

fun gen ({label,exp=_,no=_},tail) =
","::label::"=CPN'"::label::tail

fun gen_fix ((label,_),tail) = ","::"CPN'"::label::tail
in
(* There is a bug in SML/NJ 0.93 such that #<n> and 
* {n= ,...} only can be used for n less than 10, 
* which is why we generate a special function in 
* these cases. *)
case (CPN'CSTable.get_prime_kind (#kind(CPN'CSTable.find cs))) of
CPN'CSTable.product_cs elms => 
if length elms < 10 then 
"{"::tl(foldr gen (",...}"::tail) allkeys)
else "("::tl(foldr gen_fix (")"::tail) elms )
| CPN'CSTable.record_cs _ =>
"{"::tl(foldr gen (",...}"::tail) allkeys)
| _ => raise InternalError "gen_head"
end	     

fun gen_colfun keys = let
fun gen ({label,exp,no},tail) = let
val (_,csi) = List.nth(components,no-1)
in
"\n else":: 
" if "::csi::".lt(CPN'"::label::","::exp::") then\
\ LESS\n\
\ else if "::csi::".lt("::exp::",CPN'"::label::") then\
\ GREATER"::tail
end
in
concat(case timed of
NONE => 
"fn ("::gen_head(": "::cs::") =>"::
tl(foldr gen ["\n else EQUAL"] keys))
| SOME _ => 
"fn (CPN'Time.@("::gen_head(": "::cs::",_)) =>"::
tl(foldr gen ["\n else EQUAL"] keys)))
end

fun gen_filfun keys = let
fun gen ({label,exp,no=_},tail) =
" andalso"::" CPN'"::label::" = ("::exp::")"::tail
in
concat(case timed of
NONE => 
"fn ("::gen_head(": "::cs::") =>"::
tl(foldr gen nil keys))
| SOME tv =>
"fn (CPN'Time.@("::gen_head(": "::cs::",CPN''time)) =>"::
tl(foldr gen
(if tv = Time.null_str then
["\n andalso CPN'Time.ready(CPN''time)"]
else
("\n andalso CPN'Time.ready(CPN'Time.sub\
\(CPN''time,CPN'Time.fromInt ("::tv::")))"::nil)
) keys
)))
end
in
if in_order (order,allkeys) then 
(true, gen_colfun allkeys)
else 
(false, gen_filfun allkeys)
end

(* Change characters that are not alphanumeric, or ' to  _ *) 
fun fixstr str =
implode (map (fn CPN'x => if (Char.isAlphaNum CPN'x) orelse ((#"'") = CPN'x)
then CPN'x
else #"_") (explode str))

fun gen_bop (B_p{pat,coef,vars,isdiv,time=NONE,place}::bops,exn,tail) = let
(* Generates (for non_ms):
* let
*    val _ = "ims".init_res ("place".mark CPN'inst)
*    fun CPN'bf () = let
*       val "pat" = "ims".random_res "exn" ("place".mark CPN'inst)
*       val _ = case CPN'mode of 
*               CPN'Sim.all_enabled => "ims".res_col ("place".mark CPN'inst, "pat")
*               | _ => ()
*    in
*       (if "coef" <= "ims".cf("place".mark(CPN'inst),"pat")
*           /andalso legals/ then
*           /next/
*           handle BindFailure => CPN'bf()
*        else CPN'bf())
*    end handle Bind => CPN'bf()
* in
*    CPN'bf()
* end
*
* Generates (for ms):
* let
*    val _ = "ims".init_res ("place".mark CPN'inst)
*    exception BindFailure_(fixstr "pat") of "ims".cs option
*    fun CPN'bf (CPN'm,CPN'prev) = let
*       val (CPN'next,CPN'm) = case CPN'mode of 
*           CPN'Sim.all_enabled => "ims".next /BindFailureGenAll or "exn"/ CPN'm CPN'prev
*         | _ => "ims".next "exn" CPN'm CPN'prev
*    in
*       case CPN'next of
*        "pat => (if "coef" <= "ims".cf("place".mark(CPN'inst),"pat")
*           /andalso legals/ then
*           /next/
*           handle BindFailure CPN'b => CPN'bf(CPN'm,CPN'b)
*        else CPN'bf(CPN'm,(SOME ("pat"))))
*      | _ => CPN'bf(CPN'm,NONE)
*    end handle Bind => CPN'bf(CPN'm,NONE)
* in
*    CPN'bf(!("place".mark CPN'inst),NONE)
* end
*)
val no = Int.toString (CPN'Misc.next())
val {ims,name,cs,...} = CPN'PlaceTable.get_rep place

val (if_legal_ms,else_legal_ms) =
if coef = "1" then
gen_if_else_legals(vars,"if", "\n else CPN'bf(CPN'm,SOME ("^pat^"))")
else
(concat("if "::coef::" <= "::ims::".cf(!("::name::
".mark CPN'inst),"::pat::")"::
gen_andalso_legals(vars,[" then"])),
"\n else CPN'bf(CPN'm,SOME ("^pat^"))")

val (if_legal,else_legal) =
if coef = "1" then
gen_if_else_legals(vars,"if", "\n else CPN'bf()")
else
(concat("if "::coef::" <= "::ims::".cf(!("::name::
".mark CPN'inst),"::pat::")"::
gen_andalso_legals(vars,[" then"])),
"\n else CPN'bf()")

fun gen_non_ms () =
"\n let (* Untimed B_p coef>=1 *)\n\
\\n val _ = "::ims::".init_res("::name::".mark CPN'inst)\
\\n fun CPN'bf() = \nlet\
\\n val "::pat::" = case CPN'mode of \nCPN'Sim.all_enabled => "::ims::".random_res "::
(if exn="BindFatalFailure" then "BindFailureGenAll" else exn)::" ("::name::".mark CPN'inst)\n\
\ | _ => "::ims::".random_res "::exn::
" ("::name::".mark CPN'inst)"::
"\n val _ = case CPN'mode of"::
"\n CPN'Sim.all_enabled => "::ims::".res_col("::name::".mark CPN'inst"::
","::pat::")"::
"\n | _ => ()"::
"\n in\n ("::if_legal::
gen_bop(bops,"BindFailure",
"\n handle BindFailure => CPN'bf()"::else_legal::")\
\\n end handle Bind => CPN'bf()\
\\n in CPN'bf() end"::tail)

fun gen_ms i = 
"\n let (* Untimed ms B_p coef<>1 *) \
\\n val _ = "::ims::".init_res("::name::".mark CPN'inst)\
\\n exception BindFailure_"::(fixstr pat)::" of "::ims::".cs option \
\\n fun CPN'bf(CPN'm,CPN'prev) = \nlet\
\\n val (CPN'next,CPN'm) = case CPN'mode of \nCPN'Sim.all_enabled => "::ims::".next "::
(if exn="BindFatalFailure" then "BindFailureGenAll" else exn)::" CPN'm CPN'prev\n\
\ | _ => "::ims::".next "::exn::" CPN'm CPN'prev\n\
\ in\n"::
"case CPN'next of\n"::pat::" => ("::if_legal_ms::
gen_bop(bops,"(BindFailure_"^(fixstr pat)^" (SOME ("^pat^")))",
"\n handle BindFailure_"^(fixstr pat)^" CPN'b => CPN'bf(CPN'm,CPN'b)"::else_legal_ms::")"::
"\n | _ => CPN'bf(CPN'm, NONE)\
\ \n end \
\ \n handle Bind => CPN'bf(CPN'm,NONE) (* necessary ?*)\
\\n in CPN'bf(!("::name::".mark CPN'inst),NONE) end"::tail)
in
case isdiv of
NONE => gen_non_ms()
| SOME(_,1) => gen_non_ms()
| SOME(i,_) => gen_ms i
end
| gen_bop (B_p{pat,coef="1",vars,isdiv,time=SOME(tv),place}::bops,exn,tail) = let
(* Generates (for non_ms):
* let
*    val _ = "ims".init_res ("place".mark CPN'inst)
*    fun CPN'bf () = let
*       val CPN'Time.@("pat",CPN'time<no>) =
*          "ims".random_res "exn" ("place".mark CPN'inst)
*       val _ = case CPN'mode of 
*               CPN'Sim.all_enabled => "ims".res_col ("place".mark CPN'inst, "pat")
*               | _ => ()
*    in
*             (if ready(CPN'time<no>-"tv") /andalso legals/ then
*                 /next/ handle BindFailure => CPN'bf()
*             else CPN'bf())
*    end handle Bind => CPN'bf()
* in
*    CPN'bf()
* end
*
* Generates (for ms):
* let
*    val _ = "ims".init_res ("place".mark CPN'inst)
*    exception BindFailure_(fixstr "pat") of "ims".cs option
*    fun CPN'bf (CPN'm,CPN'prev) = 
*    let
*       val (col_(fixstr "pat") as CPN'Time.@(CPN'next,CPN'time<no>),CPN'm) = case CPN'mode of
*         CPN'Sim.all_enabled => "ims".next /BindFailureGenAll or "exn"/ CPN'm CPN'prev
*         | _  => "ims".next "exn" CPN'm CPN'prev
*    in
*             case CPN'next of
*              "pat" => (if ready(CPN'time<no>-"tv") /andalso legals/ then
*                 /next/ handle BindFailure CPN'b => CPN'bf(CPN'm,CPN'b)
*             else CPN'bf(CPN'm,(SOME col_(fixstr "pat"))))
*              | _ => CPN'bf(CPN'm,NONE)
*    end handle Bind => CPN'bf(CPN'm)
* in
*    CPN'bf(!("place".mark CPN'inst))
* end
*
*)
val no = Int.toString (CPN'Misc.next())
val {ims,name,cs,offset,...} = CPN'PlaceTable.get_rep place
val (if_legal,else_legal) =
if SOME(tv) = offset then
gen_if_else_legals(vars,"if","\n else CPN'bf()")
else
(concat("if CPN'Time.ready("::
(if tv=Time.null_str then "CPN'time"^no
else "CPN'Time.sub(CPN'time"^no^", CPN'Time.fromInt("^tv^"))")::
")"::gen_andalso_legals(vars,[" then"])),
"\n else CPN'bf()")

val (if_legal_ms,else_legal_ms) =
if SOME(tv) = offset then
gen_if_else_legals(vars,"if","\n else CPN'bf(CPN'm,SOME col_"^(fixstr pat)^")")
else
(concat("if CPN'Time.ready("::
(if tv=Time.null_str then "CPN'time"^no
else "CPN'Time.sub(CPN'time"^no^", CPN'Time.fromInt("^tv^"))")::
")"::gen_andalso_legals(vars,[" then"])),
"\n else CPN'bf(CPN'm,SOME col_"^(fixstr pat)^")")

fun gen_non_ms () =
"\n let (* timed B_p with coef 1 *)\
\\n val _ = "::ims::".init_res("::name::".mark CPN'inst)\
\\n fun CPN'bf() = \nlet\
\\n val CPN'Time.@("::pat::",CPN'time"::no::") = "::
"case CPN'mode of \nCPN'Sim.all_enabled => "::ims::".random_res "::
(if exn="BindFatalFailure" then "BindFailureGenAll" else exn)::" ("::name::".mark CPN'inst)\n\
\ | _ => "::ims::".random_res "::exn::" ("::name::".mark CPN'inst)"::
"\n val _ = case CPN'mode of "::
"\n CPN'Sim.all_enabled => "::ims::".res_col("::name::".mark CPN'inst"::
", CPN'Time.@("::pat::",CPN'time"::no::"))"::
"\n | _ => ()"::
"\n in ("::if_legal::
gen_bop(bops,"BindFailure",
"\n handle BindFailure => CPN'bf()"::else_legal::")\
\\n end handle Bind => CPN'bf()\
\\n in CPN'bf() end"::tail)

fun gen_ms i =  
"\n let (* timed ms B_p with coef 1 *)"::
"\n val _ = "::ims::".init_res("::name::".mark CPN'inst)\
\\n exception BindFailure_"::(fixstr pat)::" of "::ims::".cs option \
\\n fun CPN'bf(CPN'm,CPN'prev) = \nlet\
\\n val (col_"::(fixstr pat)::" as CPN'Time.@(CPN'next,CPN'time"::no::"),CPN'm) = "::
"case CPN'mode of \nCPN'Sim.all_enabled => "::ims::".next "
::(if exn="BindFatalFailure" then "BindFailureGenAll" else exn)::" CPN'm CPN'prev\n\
\ | _ => "::ims::".next "::exn::" CPN'm CPN'prev\
\\n in \
\ \n case CPN'next of \
\ \n "::pat::" => ("::if_legal_ms::
gen_bop(bops,"(BindFailure_"^(fixstr pat)^" (SOME col_"^(fixstr pat)^"))",
" handle BindFailure_"^(fixstr pat)^" CPN'b => CPN'bf(CPN'm,CPN'b)"::else_legal_ms::")"::
"\n | _ => CPN'bf(CPN'm,NONE)\
\ \n end \ 
\ \n handle Bind => CPN'bf(CPN'm,NONE) (* necessary? *)\
\\n in CPN'bf(!("::name::".mark CPN'inst),NONE) end"::tail)
in
case isdiv of
NONE => gen_non_ms()
| SOME(_,1) => gen_non_ms()
| SOME(i,_) => gen_ms i
end
| gen_bop (B_p{pat,coef,vars,isdiv,time=SOME(tv),place}::bops,exn,tail) = let
(* FIXME: the following comment is not accurate *)
(* Generates:
* let
*    val _ = "ims".init_res ("place".mark CPN'inst)
*    fun CPN'bf () = let
*       val "pat"@_ = 
*          "ims".random_res "exn" ("place".mark CPN'inst)
*    in
*             /(if legals then/
*               (CPN'Sim.collect_tms "exn" 
*                ("place".mark(CPN'inst),"ims".collect,"ims".cmp,"ims.delete,"exp");
*                /next/) handle BindFailure => CPN'bf()
*             /else CPN'bf())/
*     end handle Bind => CPN'bf()
* in CPN'bf() end
*)

val no = Int.toString (CPN'Misc.next())
val {cs,ims,name,offset,...} = CPN'PlaceTable.get_rep place
val (if_legal,else_legal) = 
gen_if_else_legals (vars, "if", "\n else CPN'bf(CPN'm,SOME col_"^(fixstr pat)^")")
in
"\n let (* timed B_p with coef > 1 *)\
\\n val _ = "::ims::".init_res("::name::".mark CPN'inst)\
\\n exception BindFailure_"::(fixstr pat)::" of "::ims::".cs option \
\\n fun CPN'bf(CPN'm,CPN'prev) = \nlet\
\\n val (col_"::(fixstr pat)::" as CPN'Time.@("::pat::",_),CPN'm) = "::
"case CPN'mode of \nCPN'Sim.all_enabled => "::ims::".next "::
(if exn="BindFatalFailure" then "BindFailureGenAll" else exn)::" CPN'm CPN'prev\n\
\ | _ => "::ims::".next "::exn::" CPN'm CPN'prev\
\\n in ("::if_legal::
" (CPN'Sim.collect_tms (BindFailure_"::(fixstr pat)::" (SOME col_"::(fixstr pat)::")) ("::name::".mark CPN'inst,"::
ims::".collect,"::ims::".cmp,"::ims::".delete,\
\CPN'TMS.@+(("::coef::"`"::pat::"),"::tv::"));"::
gen_bop(bops,"(BindFailure_"^(fixstr pat)^" (SOME col_"^(fixstr pat)^"))",
") handle BindFailure_"::(fixstr pat)::" CPN'p => CPN'bf(CPN'm,CPN'p)"::else_legal::")\
\\n end handle Bind => CPN'bf(CPN'm,NONE)\
\\n in CPN'bf(!("::name::".mark CPN'inst),NONE) end"::tail)
end
| gen_bop (B_k{pat,coef,vars,keys,time=NONE,place}::bops,exn,tail) = let
val no = Int.toString (CPN'Misc.next())
val {cs,ims,name,order,...} = CPN'PlaceTable.get_rep place
val (in_order,func) = gen_keyfunc (NONE,cs,order,keys)
in
if in_order then let
(* Generates:
* let
*    fun CPN'bf CPN'ms = case get_ran "exn" CPN'ms of
*        ("pat",CPN'msrest) => 
*            (if "coef" <= CPN'MS.cf("pat",CPN'ms) /andalso legals/ then
*               /next/ handle BindFailure => CPN'bf (CPN'MS.filter (fn CPN'x => CPN'x <> "pat") CPN'msrest)
*             else CPN'bf CPN'msrest)
*    val CPN'cf = /colfun/
* in
*    CPN'bf("ims".collect CPN'cf (!("place".mark(CPN'inst))))
* end
*)
val no = Int.toString (CPN'Misc.next())
val (if_legal,else_legal) =
if coef = "1" then
gen_if_else_legals(vars,"if","\n else CPN'bf CPN'msrest")
else
(concat("if "::coef::" <= CPN'MS.cf("::pat::
",CPN'ms)"::
gen_andalso_legals(vars,[" then"])),
"\n else CPN'bf CPN'msrest")
in
"\n let (* untimed B_k with keys in order *)\
\\n fun CPN'bf CPN'ms = case CPN'MS.get_ran "::
"(case CPN'mode of \nCPN'Sim.all_enabled => "::
(if exn="BindFatalFailure" then "BindFailureGenAll" else exn)::
"\n | _ => "::exn::
") CPN'ms of\
\\n ("::pat::",CPN'msrest) => ("::if_legal::
gen_bop(bops,"BindFailure",
"\n handle BindFailure => CPN'bf (CPN'MS.filter (fn CPN'x => CPN'x <> "::pat::") CPN'msrest)"::else_legal::")\
\\n | (_,CPN'msrest) => (CPN'bf CPN'msrest)\
\\n val CPN'cf  = "::func::
"\n in CPN'bf("::ims::".collect CPN'cf\
\ (!("::name::".mark(CPN'inst)))) end"::tail)
end
else let (* keys not in order with place marking *)
(* let
*    val _ = "ims".init_res ("place".mark CPN'inst) 
*    val CPN'ff = /filfun/
*    fun CPN'bf () =
*       (case "ims".check_res CPN'ff "exn" ("place".mark(CPN'inst)) of
*          "pat" =>
*             (if "coef"<="ims".cf("place".mark(CPN'inst),"pat") 
*                 /andalso legals/ then
*                 /next/ handle BindFailure => CPN'bf()
*              else CPN'bf())
*         | _ => CPN'bf())
* in
*    CPN'bf()
* end
*)
val no = Int.toString (CPN'Misc.next())
val (if_legal,else_legal) =
if coef = "1" then
gen_if_else_legals(vars,"if", "\n else CPN'bf()")
else
(concat("if "::coef::" <= "::ims::".cf(!("::name::
".mark CPN'inst),"::pat::")"::
gen_andalso_legals(vars,[" then"])),
"\n else CPN'bf()")
in
"\n let (* untimed B_k with keys not in order *)\
\\n val _ = "::ims::".init_res("::name::".mark CPN'inst)\
\\n val CPN'ff = "::func::
"\n fun CPN'bf () = (case "::ims::".check_res "::
"(case CPN'mode of \nCPN'Sim.all_enabled => "::
(if exn="BindFatalFailure" then "BindFailureGenAll" else exn)::
"\n | _ => "::exn::
") CPN'ff ("::name::".mark(CPN'inst)) of "::pat::" =>\n ("::
if_legal::gen_bop(bops,"BindFailure",
"\n handle BindFailure => CPN'bf()"::else_legal::")\
\\n | _ => CPN'bf())\
\\n in CPN'bf() end"::tail)
end
end
| gen_bop (B_k{pat,coef="1",vars,keys,time=SOME(tv),place}::bops,exn,tail) = let
val no = Int.toString (CPN'Misc.next())
val {cs,ims,name,order,offset,...} = CPN'PlaceTable.get_rep place
val (in_order,func) = gen_keyfunc (SOME(tv),cs,order,keys)
in
if in_order then let
(* Generates:
* let
*    fun CPN'bf CPN'tms =
*       (case get_ran "exn" CPN'tms of
*          ((CPN'Time.@("pat",CPN'time<no>)),CPN'tms') =>
*             (/if legals/ then
*                /next/ handle BindFailure => CPN'bf(CPN'MS.filter (fn CPN'Time.@(CPN'x,CPN'ts) => CPN'x <> "pat") CPN'tms')
*              /else CPN'bf(CPN'tms')/)
*         | (_,CPN'tms') => CPN'bf(CPN'tms'))
*    val CPN'cf = /colfun/
* in
*    CPN'bf("ims".collect CPN'cf (!("place".mark(CPN'inst))))
* end
*)
val (if_legal,else_legal) =
if SOME tv = offset then
gen_if_else_legals (vars,"if",
"\n else CPN'bf CPN'tms'")
else
(concat("if CPN'Time.ready("::
(if tv=Time.null_str then "CPN'time"^no
else "CPN'Time.sub(CPN'time"^no^",CPN'Time.fromInt("^tv^"))")::
")"::gen_andalso_legals(vars,[" then"])),
"\n else CPN'bf CPN'tms'")
in
"\n let (* timed B_k with keys in order and coef 1 *)\
\\n fun CPN'bf CPN'tms = (case CPN'MS.get_ran "::
"(case CPN'mode of \nCPN'Sim.all_enabled => "::
(if exn="BindFatalFailure" then "BindFailureGenAll" else exn)::
"\n | _ => "::exn::
") CPN'tms of\n ((CPN'Time.@("::pat::",CPN'time"::no::")),\
\ CPN'tms') =>\n ("::if_legal::
gen_bop(bops,"BindFailure",
"\n handle BindFailure => CPN'bf(CPN'MS.filter (fn CPN'Time.@(CPN'x,CPN'ts) => CPN'x <> "::pat::") CPN'tms')"::else_legal::")\
\\n | (_,CPN'tms') => CPN'bf(CPN'tms'))\
\\n val CPN'cf = "::func::"\
\\n in CPN'bf("::ims::".collect CPN'cf\
\ (!("::name::".mark(CPN'inst)))) end"::tail)
end
else let
(* Generates:
* let
*    val _ = "ims".init_res ("place".mark CPN'inst) 
*    val CPN'ff = /filfun/
*    fun CPN'bf () =
*       (case "ims".check_res "exn" ("place".mark CPN'inst) of
*           ("pat"@CPN'time<no>) =>
*             (/if legals/ then
*                /next/ handle BindFailure => CPN'bf()
*              /else CPN'bf()/)
*         | _ => CPN'bf())
* in
*    CPN'bf();
* end
*)
val (if_legal,else_legal) = 
gen_if_else_legals (vars,"if", "\n else CPN'bf()")
in
"\n let (* timed B_k with keys not in order and coef 1 *)\
\\n val _ = "::ims::".init_res("::name::".mark CPN'inst)\
\\n val CPN'ff = "::func::"\
\\n fun CPN'bf () = (case "::ims::".check_res "::
"(case CPN'mode of \nCPN'Sim.all_enabled => "::
(if exn="BindFatalFailure" then "BindFailureGenAll" else exn)::
"\n | _ => "::exn::
") CPN'ff ("::name::".mark CPN'inst) of\
\\n CPN'Time.@("::pat::",_) =>\n ("::
if_legal::gen_bop(bops,"BindFailure",
"\n handle BindFailure => CPN'bf()"::else_legal::")\
\\n | _ => CPN'bf())\
\\n in CPN'bf() end"::tail)
end
end
| gen_bop (B_k{pat,coef,vars,keys,time=SOME(tv),place}::bops,exn,tail) = let
(* Generates: 
* let
*    fun CPN'bf CPN'tms = let
*        val (CPN'Time.@("pat",_),CPN'tms') = case get_ran "exn" CPN'tms
*    in      
*       /(if legals then/
*         let val CPN'tms<no> = 
*             fetch_tms BindFailure cs.lt (CPN'tms,"coef","pat")
*         in 
*             /next/ 
*         end handle BindFailure => CPN'bf(CPN'MS.filter (fn CPN'Time.@(CPN'x,CPN'ts) => CPN'x<>"pat") CPN'tms') 
*         /else CPN'bf CPN'tms')/
*    end
*    val CPN'{cf = /colfun/ | ff = /filfun/}
* in
*    CPN'bf("ims".{collect CPN'cf | filter CPN'ff} 
*          (!("place".mark(CPN'inst))))
* end
*)
val no = Int.toString (CPN'Misc.next())
val {cs,ims,name,order,...} = CPN'PlaceTable.get_rep place
val (in_order,func) = gen_keyfunc (SOME(tv),cs,order,keys)
val (if_legal,else_legal) = 
gen_if_else_legals (vars,"if","\n else CPN'bf CPN'tms")
in
"\n let (* timed B_k with coef>1 *)"::
(if in_order then "" 
else concat ["\n val _ = ",ims,".init_res(",name,".mark CPN'inst)"])::
"\n fun CPN'bf CPN'tms = \nlet\
\\n val (CPN'Time.@("::pat::",_),CPN'tms') =\
\ CPN'MS.get_ran "::
"(case CPN'mode of \nCPN'Sim.all_enabled => "::
(if exn="BindFatalFailure" then "BindFailureGenAll" else exn)::
"\n | _ => "::exn::
") CPN'tms\
\\n in\n ("::if_legal::
"\n let val CPN'tms"::no::" = CPN'Sim.fetch_tms BindFailure\
\ "::cs::".lt (CPN'tms,"::coef::","::pat::")\
\\n in"::gen_bop(bops,"BindFailure",
if in_order then	
"\n end handle BindFailure => CPN'bf(CPN'MS.filter (fn CPN'Time.@(CPN'x,CPN'ts) => CPN'x <> "::pat::") CPN'tms')"::
else_legal::")\n end\
\\n val CPN'cf = "::func::
"\n in CPN'bf("::ims::".collect CPN'cf\
\ (!("::name::".mark(CPN'inst)))) end"::tail
else
"\n end handle BindFailure => CPN'bf(CPN'MS.filter (fn CPN'Time.@(CPN'x,CPN'ts) => CPN'x <> "::pat::") CPN'tms')"::
else_legal::"\n end\
\\n val CPN'ff = "::func::
"\n in CPN'bf("::ims::".filter CPN'ff\
\ (!("::name::".mark(CPN'inst)))) end"::tail)
end
| gen_bop (T_a{exp,coef="",time=NONE,place}::bops,exn,tail) = let
(* Generates:
*    (if "ims".subset(!("place".mark(CPN'inst)),"exp") then
*       /next/
*     else raise "exn")	
*)
val {ims,name,...} = CPN'PlaceTable.get_rep place
in
"\n (* untimed T_a multi-set exp *)\
\\n (if "::ims::".subset\
\ (!("::name::".mark CPN'inst),"::exp::") then"::
gen_bop(bops,exn,
"\n else raise "::exn::")"::tail)
end
| gen_bop (T_a{exp,coef="1",time=NONE,place}::bops,exn,tail) = let
(* Generates:
*    (if "ims".member(!("place".mark(CPN'inst)),"exp") then
*        /next/
*     else raise "exn")
*)
val {ims,name,...} = CPN'PlaceTable.get_rep place
in
"\n (* untimed T_a token exp coef 1 *)\
\\n (if "::ims::".member\
\ (!("::name::".mark CPN'inst),"::exp::") then"::
gen_bop(bops,exn,
"\n else raise "::exn::")"::tail)
end
| gen_bop (T_a{exp,coef,time=NONE,place}::bops,exn,tail) = let
(* Generates:
*    (if "coef"<="ims".cf(!("place".mark(CPN'inst)),"exp") then
*        /next/
*     else raise "exn")
*)
val {ims,name,...} = CPN'PlaceTable.get_rep place
in
"\n (* untimed T_a token exp coef > 1 *)\
\\n (if "::coef::"<="::ims::".cf\
\ (!("::name::".mark CPN'inst),"::exp::") then"::
gen_bop(bops,exn,
"\n else raise "::exn::")"::tail)
end
| gen_bop (T_a{exp,coef="1",time=SOME(tv),place}::bops,exn,tail) = let
(* Generates:
*    (collect_token "exn" 
*        ("place".mark CPN'inst,"ims".collect,"ims".cmp,"exp","tv");
*    /next/)
*)
val {cs,ims,name,...} = CPN'PlaceTable.get_rep place
in
"\n (* timed T_a token exp with coef 1*)\
\\n (CPN'Sim.collect_token "::exn::
" ("::name::".mark CPN'inst,"::ims::".collect,"::
(* 		ims::".cmp,"::exp::","::"maketime (Int.toString ("::tv::")));":: *)
ims::".cmp,"::exp::","::"CPN'Time.add (time(), CPN'Time.fromInt("::tv::")));"::
gen_bop(bops,exn,")"::tail)
end
| gen_bop (T_a{exp,coef,time=SOME(tv),place}::bops,exn,tail) = let
(* Generates:
*   (collect_tms "exn" 
*        ("place".mark CPN'inst,"ims".collect,"ims".cmp,"ims".delete,"exp");
*    /next/)
*)
val {cs,ims,name,...} = CPN'PlaceTable.get_rep place
val tmsexp = case (coef,tv) of
("","0") => concat["CPN'TMS.@++(",exp,",(maketime \"",tv,"\"))"]
| ("","0.0") => concat["CPN'TMS.@++(",exp,",(maketime \"",tv,"\"))"]
| ("","(maketime \"0\")") => concat["CPN'TMS.@++(",exp,",",tv,")"]
| ("","maketime(\"0\"))") => concat["CPN'TMS.@++(",exp,",",tv,")"]
| ("",_) => exp
| (_,_) => concat ["CPN'TMS.@+(",coef,"`",exp,",",tv,")"]
in
"\n (* timed T_a multi-set exp *)\
\\n (CPN'Sim.collect_tms "::exn::
" ("::name::".mark CPN'inst,"::ims::".collect,"::
ims::".cmp,"::ims::".delete,"::tmsexp::");"::
gen_bop(bops,exn,")"::tail)
end
| gen_bop (T_g{exp}::bops,exn,tail) =
(* Generates:
* (if ("exp") then
*    /next/
*  else raise "exn")
*)
"\n (* T_g *)\n (if ("::exp::") then"::
gen_bop(bops,exn,"\n else raise "::exn::")"::tail)
| gen_bop (B_g{pat,vars,rebind_vars,exp}::bops,exn,tail) =
(* Generates:
* (let  /save rebinding vars/
*       val "pat" = "exp" 
*       val _ = if (/test rebinding vars/) then () else raise BindFailure
* in /next/ end handle Bind => raise "exn"
*                    | BindFailure => raise "exn" /if any rebinding vars/)
* Note that Bind is raised by ML if pattern pat does not match exp
*)
let
val save_rbvars_code= foldr (fn (v,accum)=> ("\nval ORIG'"^v^"= "^v^accum)) "" rebind_vars
val test_rbvars_code= 
if (List.null(rebind_vars))
then ""
else "\nval _ = if ("^(foldr (fn (v,accum)=> (v^"=ORIG'"^v^" andalso "^accum)) "true" rebind_vars)^") then () else raise BindFailure"
val handle_bindfailure_code = 
if (List.null(rebind_vars))
then ""
else "\n | BindFailure => raise "^exn
in
"\n (* B_g *)\n (let "::save_rbvars_code::"\nval "::pat::" = "::exp::test_rbvars_code::"\n in "::
gen_bop(bops,exn,"\n end handle Bind => raise "::exn::handle_bindfailure_code::")"::tail)
end
| gen_bop (B_c{var,cs}::bops,exn,tail) = 
(* Generates:
* let
*    val _ = CPN'RS.init_res CPN'"cs"_rs_"var"
*    fun CPN'bf() = let 
*       val "var"= CPN'RS.random_res "exn" CPN'"cs"_rs_"var"
*    in 
*       /next/ handle BindFailure => CPN'bf()
*    end
* in CPN'bf() end
*)
"\n let (* B_c *)\
\\n val _ = CPN'RS.init_res CPN'"::cs::"_rs_"::var::
"\n fun CPN'bf() = let\
\\n val "::var::" = case CPN'mode of \nCPN'Sim.all_enabled => CPN'RS.random_res "::
(if exn="BindFatalFailure" then "BindFailureGenAll" else exn)::" CPN'"::cs::"_rs_"::var::
"\n | _ => CPN'RS.random_res "::exn::" CPN'"::cs::"_rs_"::var::"\n in"::
gen_bop(bops,"BindFailure",
" handle BindFailure => CPN'bf()\n end\n in CPN'bf() end"::tail)
| gen_bop (T_v{var1,var2}::bops,exn,tail) =
(* Generates:
* if "var1"="var2" then
*    /next/
* else raise "exn"
*)
"\n (* T_v *) if "::var1::"="::var2::" then "::
gen_bop(bops,exn,"\n else raise "::exn::tail)
| gen_bop (nil,exn,tail) =
(* Generates:
* (/CPN'bh<grp>::= {"vars"};/
*  case CPN'mode of CPN'Sim.bind _ => raise "exn
*  | _ => CPN'bf<grp+1>())
*)
case vars of
nil =>
"\n (case CPN'mode of \nCPN'Sim.bind _=> raise BoundGroup\
\\n | CPN'Sim.all_enabled => raise "^(if (exn="BindFatalFailure") then "BindFailureGenAll" else exn)^
"\n | _ => CPN'bf"::(Int.toString (grp+1))::"())"::tail
| _ => 
"\n (CPN'bh"::(Int.toString grp)::"::= "::
gen_bhi(vars,";\n\
\ case CPN'mode of \nCPN'Sim.bind _ => raise BoundGroup\n\
\ | CPN'Sim.all_enabled => raise "^(if (exn="BindFatalFailure") then "BindFailureGenAll" else exn)^
"\n | _ => CPN'bf"::(Int.toString (grp+1))::"())"::tail)

in (* gen_bfi *)
"\n fun CPN'bf"::(Int.toString grp)::"() = "::
gen_bop(bops,"BindFatalFailure",gen_bfi(groups,grp-1,tail))
end

fun gen_bf_calls (n: int) = let
fun gen i =
if i<=n then
";\n"::"CPN'bf"::(Int.toString i)::"() handle BoundGroup => ()"::
gen(i+1)
else
nil
val bflst = gen 1
in
concat ("("::(if bflst=nil then nil else (tl bflst))@[")"])
end

fun gen_bf_calls_all (n: int) = let
fun gen i =
if i<=(n+1) then
";\n"::"CPN'bf"::(Int.toString i)::"() handle BindFailureGenAll => ()"::
gen(i+1)
else
nil
val bflst = gen 1
in
concat ("(("::(if bflst=nil then nil else (tl bflst))@[") handle BindFatalFailure => (if (not (!CPN'whole_binding)) then CPN'bh_reset() else ()))"])
end


val nogrps = length groups

in (* gen_bindfun *)
"\n fun CPN'bindfun (CPN'mode,CPN'inst) = \nlet\n\
\ fun CPN'bf"::(Int.toString (nogrps+1))::"() = (CPN'whole_binding := true)"::
gen_bfi(rev groups,nogrps,
"\nfun CPN'bh_reset () = ("::gen_bh_reset(groups,
"())\n in\n"::
"(CPN'bh_reset();\n"::
" case CPN'mode of\n\
\ CPN'Sim.bind CPN'interactive => "::gen_bf_calls(nogrps)::"\n\
\ | CPN'Sim.all_enabled => "::gen_bf_calls_all(nogrps)::"\n\
\ | _ => CPN'bf1())\n\
\ end"::tail))
end

(******************** generate occur function ********************)

fun gen_occ (t,page,input,groups,free_vars,code_reg,time_reg,output,monitors,tail) = let
(* Generates:
* fun CPN'occfun ({v1,...,vn1},...,{v1,...,vnm},"free_vars",CPN'inc_step) = let
*     / increase the step number if simulating, but not when
*       generating state space 
*       The step is increased here to avoid problems with 
*       inconsistent step numbers for monitors that are 
*       associated with transitions and monitors that are 
*       not associated with any transitions. This will have 
*       to be changed if it ever becomes possible to have more
*       than one binding element in a step /
*     val _ = if CPN'inc_step then CPN'Sim.inc_step() else ()
*     val "output" = CPN'code_action<id> CPN'inst ("input")
*     val CPN'show_input = ...
*     val CPN'show_output = ...
*     val _ = (* pause before *)
*     val _ = "ims1".subfrom("p1".mark CPN'inst,"exp"); 
*              ...);
*     val CPN'suberr = ref (nil: (CPN'Id.id * string) list);
*     val CPN'trans_delay = "time_reg" /if at least 
*                                      one output place is timed
*     val _ = ("ims1".addto("p1".mark CPN'inst,"exp");
*             if CPN'MS.legal_ms "cs".legal "exp" then ()
*             else CPN'suberr::= ("pid",error_not_legal exp);
*             ...);
*     val _ = tst_ill_marks(!CPN'suberr);
*     val _ = (* pause after *)
*     val CPN'bindrec = {v1=v1,...,vi=vi,
*	                  fv1=fv1,...,fvj=fvj,
*      		  ov1=ov1,...,ovk=ovk} 
*     val _ = CPN'BR<id> := CPN'bindrec
*     val _ = CPN'monitor<id> CPN'inst CPN'bindrec
*  in
*     (CPN'Sim.is_executed,
*      if !CPN'Options.report_bindings then
*          /report/
*      else nil))
* end
*   | occ _ = (CPN'answer,nil)
*
*  Differs a bit for the different expressions!
*)

local 
fun gen_show (nil,_,tail) = tail
| gen_show ((aid,CPN'PlaceTable.token_exp exp)::arcs,cs,tail) =
"\n | "::"("::(CPN'Id.toString aid)::",CPN'b) =>\
\ if CPN'b then (1, "::cs::".mkstr ("::exp::"))\
\ else (1,\"\")"::gen_show(arcs,cs,tail)
| gen_show ((aid,CPN'PlaceTable.ms_exp exp)::arcs,cs,tail) =
"\n | "::"("::(CPN'Id.toString aid)::",CPN'b) =>\
\ if CPN'b then (CPN'MS.size("::exp::"), "::
cs::".mkstr_ms ("::exp::"))\
\ else (CPN'MS.size("::exp::"),\"\")"::gen_show(arcs,cs,tail)
| gen_show ((aid,CPN'PlaceTable.tms_exp exp)::arcs,cs,tail) =
"\n | "::"("::(CPN'Id.toString aid)::",CPN'b) =>\
\ if CPN'b then (CPN'MS.size("::exp::"), "::
cs::"'timed.mkstr_ms ("::exp::"))\
\ else (CPN'MS.size("::exp::"),\"\")"::gen_show(arcs,cs,tail)
in
fun gen_show_input (inarcs,tail) = let

(* CPN_ERROR is ~4 at the C-side *)
fun gen (nil,tail) = "\n | "::"_ => (~4,\"\")"::tail
| gen ({arcs,place,no_of_tokens=_}::ins,tail) =
gen_show(arcs,
#cs(CPN'PlaceTable.get_rep place),
gen(ins,tail))
in
if !CPN'Settings.use_pause then
"\n val CPN'show_input = fn "::tl(gen(inarcs,tail))
else tail
end

fun gen_show_output (outarcs,tail) = let

fun gen (nil,tail) = "\n | "::"_ => (~1,\"\")"::tail
| gen ({arcs,place}::outs,tail) =
gen_show(arcs,
#cs(CPN'PlaceTable.get_rep place),
gen(outs,tail))
in
if !CPN'Settings.use_pause then
"\n val CPN'show_output = fn "::tl(gen(outarcs,tail))
else tail
end
end

fun gen_code_reg (t,NONE,tail) = tail
| gen_code_reg (t,SOME{output,input,action=_},tail) = 
"\n val ("::
tl(foldr (fn (a,b) => ","::a::b)
(""::") = CPN'code_action"::(CPN'Id.makeid t)::" CPN'inst ("::
tl(foldr (fn (a,b) => ","::a::b)
(""::")"::tail)
input)) 
output)

fun gen_rem (nil,tail) = ""::tail
| gen_rem ({arcs,place,no_of_tokens=_}::ins,tail) = let
val {ims,cs,name,...} = CPN'PlaceTable.get_rep place
val mark = concat[name,".mark CPN'inst"]

fun gen (nil,tail) = tail
| gen ((_,arc)::arcs,tail) =
if CPN'CSTable.is_timed cs then
case arc of 
CPN'PlaceTable.token_exp exp => 
";\n "::ims::".delete("::mark::",\
\ CPN'Sim.collect_token (InternalError \"rm t"::t::" "::name::"\")\
\ ("::mark::","::ims::".collect"::","::ims::".cmp,"::
(*	exp::",(maketime \""::Time.null_str::"\")))"::gen(arcs,tail)*)
exp::", time()))"::gen(arcs,tail)
| CPN'PlaceTable.ms_exp exp => 		    
";\n "::ims::".subfrom("::mark::",\
\ CPN'Sim.collect_tms (InternalError \"rm t"::t::" "::name::"\")\
\ ("::mark::","::ims::".collect"::","::
ims::".cmp,"::ims::".delete"::","::"CPN'TMS.@++("::exp::",(maketime \""::Time.null_str::"\")"::
")))"::gen(arcs,tail)
| CPN'PlaceTable.tms_exp exp => 		    
";\n "::ims::".subfrom("::mark::",\
\ CPN'Sim.collect_tms (InternalError \"rm t"::t::" "::name::"\")\
\ ("::mark::","::ims::".collect"::","::ims::".cmp,"::ims::".delete,"::
exp::"))"::gen(arcs,tail)
else
case arc of 
CPN'PlaceTable.token_exp exp => 
";\n "::ims::".delete("::mark::","::exp::")"::
gen(arcs,tail)
| CPN'PlaceTable.ms_exp exp => 		    
";\n "::ims::".subfrom("::mark::","::exp::")"::
gen(arcs,tail)
| CPN'PlaceTable.tms_exp exp => 		    
raise InternalError("gen_rem")
in
gen(arcs,gen_rem(ins,tail))
end

fun gen_add (nil,tail) = ""::tail
| gen_add ({arcs,place}::outs,tail) = let

val {cs,ims,name,...} = CPN'PlaceTable.get_rep place
val mark = concat[name,".mark CPN'inst"]
val wait = concat[name,".wait CPN'inst"]

fun gen_legal (aid,legal_call,illegal_msg_call,exp,tail) =
if !CPN'Settings.use_legal_check andalso 
CPN'CSTable.is_subtype cs then
";\n "::"if "::legal_call::" ("::exp::") then ()\
\ else CPN'suberr::= ("::(CPN'Id.toString aid)::
", "::illegal_msg_call::" ("::exp::"))"::tail
else tail

fun gen (nil,tail) = tail 
| gen ((aid,arc)::arcs,tail) =
if CPN'CSTable.is_timed cs then
case arc of
CPN'PlaceTable.token_exp exp => 
";\n "::ims::"_sims.addto ("::wait::
",CPN'TMS.@++([("::exp::")],CPN'trans_delay))"::
gen_legal(aid,cs^".legal",cs^".illegal_msg",exp,gen(arcs,tail))
| CPN'PlaceTable.ms_exp exp => 		    
";\n "::ims::"_sims.addto ("::wait::
",CPN'TMS.@++("::exp::", CPN'trans_delay))"::
gen_legal(aid,"CPN'MS.legal_ms "^cs^".legal",
"CPN'MS.illegal_msg_ms "^cs^".illegal_msg",
exp,gen(arcs,tail))
| CPN'PlaceTable.tms_exp exp => 		
if time_reg = "CPN'Time.fromInt(0)" then
";\n "::ims::"_sims.addto\
\("::wait::","::exp::")"::
gen_legal(aid,"CPN'MS.legal_ms "^cs^"'timed.legal",
"CPN'MS.illegal_msg_ms "^cs^"'timed.illegal_msg",
exp,gen(arcs,tail))
else
";\n "::ims::"_sims.addto ("::wait::
",CPN'TMS.delay("::exp::","::"CPN'trans_delay))"::
gen_legal(aid,"CPN'MS.legal_ms "^cs^"'timed.legal",
"CPN'MS.illegal_msg_ms "^cs^"'timed.illegal_msg",
exp,gen(arcs,tail))
else
case arc of
CPN'PlaceTable.token_exp exp => 
";\n "::ims::".insert("::mark::","::exp::")"::
gen_legal(aid,cs^".legal",cs^".illegal_msg",exp,gen(arcs,tail))
| CPN'PlaceTable.ms_exp exp => 		    
";\n "::ims::".addto ("::mark::","::exp::")"::
gen_legal(aid,"CPN'MS.legal_ms "^cs^".legal",
"CPN'MS.illegal_msg_ms "^cs^".illegal_msg",
exp,gen(arcs,tail))
| CPN'PlaceTable.tms_exp exp => 		    
raise InternalError("gen_add")
in
gen(arcs,gen_add(outs,tail))
end

fun gen_head (groups,tail) = let
fun gen ({vars=nil,bops=_},tail) = tail
| gen ({vars,bops=_},tail) =
","::"{"::
tl(foldr (fn (a,b) => ","::a::b) (""::"}"::tail) vars)
in
"(CPN'inst,("::tl(foldr gen
(foldr (fn (a,b) => ","::a::b) (""::"),CPN'inc_step)"::tail) free_vars)
groups)
end

fun gen_report (tail) = let
val all_vars = CPN'TransitionTable.get_all_vars t
fun f (a,b) =
","::"\"\\n - "::a::" = \","::
(#cs(CPN'VarTable.find a))::".mkstr "::a::b
in
if !CPN'Settings.use_report_binds then
"\n if !CPN'Options.report_bindings then\n"::
"["::tl(foldr f (""::"]\nelse nil"::tail) all_vars)
else
"nil"::tail
end

fun gen_pause_before (t,tail) =
if !CPN'Settings.use_pause then
"\n val _ = CPN'Sim.pause_before CPN'show_input\
\ (CPN'id,CPN'inst);"::tail
else tail

fun gen_pause_after (t,tail) =
if !CPN'Settings.use_pause then
";\n val _ = CPN'Sim.pause_after CPN'show_output\
\ (CPN'id,CPN'inst);"::tail
else tail

fun gen_bindrec(t,groups,free_vars,code_reg,tail) = 
"\n val CPN'bindrec = "::
gen_vars_record(groups,free_vars,code_reg,
"\n val _ = (CPN'BR"::t::":=CPN'bindrec)"::tail) (* FIXME where is this? are there duplicate or similar functions? *)

fun gen_monitor_reg (t,[],tail) = tail
| gen_monitor_reg (t,monitors,tail) = 
"\nval _ = CPN'monitor"::(CPN'Id.makeid t)::
" CPN'inst CPN'bindrec "::tail
in
"\n fun CPN'occfun "::gen_head(groups," = \nlet"::
"\n val _ = if CPN'inc_step then CPN'Sim.inc_step() else () "::
gen_code_reg(t, code_reg,
gen_show_input(input,
gen_show_output(output,
gen_pause_before(t, 
"\n val _ = ("::tl(gen_rem(input,");"::
(if !CPN'Settings.use_legal_check then		      
"\n val CPN'suberr = ref (nil: (CPN'Id.id * string) list);"
else "")::
(if List.exists (fn {place,arcs} => 
CPN'PlaceTable.is_timed place) output
then "\n val CPN'trans_delay = "^time_reg
else "")::
"\n val _ = ("::tl(gen_add(output,");"::
(if !CPN'Settings.use_legal_check then		      
"\n val _ = CPN'Sim.tst_ill_marks CPN'suberr;"
else "")::
gen_pause_after(t, 
gen_bindrec(t,groups,free_vars,code_reg,
gen_monitor_reg(t,monitors,
"\n in\n\
\ (CPN'Sim.is_executed,"::gen_report(")\n\
\ end"::tail)))))))))))))
end

fun gen_pick (groups,free_vars,tail) =
let
(* Generates:
*     fun CPN'pick [CPN'i1,...,CPN'in,...,CPN'im] =
*         (CPN'nth(!CPN'bh1,CPN'i1),
*          ...,
*          CPN'nth(!CPN'bhn,CPN'in),
*          ...,
*          #2(CPN'nth(csm.all(),CPN'im)))
*)
val n = length groups

fun gen_bh _ (_,nil,tail) = tail
| gen_bh f (i,{vars=nil,bops=_}::grps, tail) = 
gen_bh f (i+1,grps,tail)
| gen_bh f (i,_::grps,tail) =
f(Int.toString i, gen_bh f (i+1,grps,tail))

fun gen_fv f (_,nil,tail) = tail
| gen_fv f (i,v::vs,tail) = 
f(Int.toString i, v, gen_fv f (i+1,vs,tail))
in
"\n fun CPN'pick ["::
tl(gen_bh (fn (no,tail) => ","::"CPN'i"::no::tail) (1,groups,
(gen_fv (fn (no,_,tail) => ","::"CPN'i"::no::tail) (n+1,free_vars,
(""::"] = ("::
tl(gen_bh (fn (no,tail) => 
","::"CPN'nth(!CPN'bh"::no::",CPN'i"::no::")"::tail) (1,groups,
(gen_fv (fn (no,v,tail) => 
","::"(CPN'nth("::(#cs(CPN'VarTable.find v))::".all(),CPN'i"::no::"))"::tail) 
(n+1,free_vars,(""::")"::tail))))))))))
end


fun gen_bindings_as_strings (groups, free_vars, tail) = let
(* Generates:
*     fun CPN'bindings_as_strings() = 
*         [(["v1",...,"vi"],[["x11",...,"x1i"],...,["xn1",...,xni]]),
*          ...,
*          (["fv"],[["x1"],...,["xn"]])]
*
* The return type of CPN'bindings_as_strings is: 
*    (string list * string list list) list
* Each entry in the outer list represents a binding group or a free
* variable. The first component of the pair is a list of the variables
* and the second component is a list of the binding of the list of
* variables.
*)
fun gen_bh (_,nil,tail) = tail
| gen_bh (i:int,{vars=nil,bops=_}::grps,tail) = 
gen_bh (i+1,grps,tail)
| gen_bh (i:int,{vars,bops=_}::grps,tail) =
","::"(["::tl(foldr (fn (a,b) => ","::"\""::a::"\""::b)
(""::"],"::
"CPN'map (fn {"::tl(foldr (fn (a,b) => ","::a::b)
(""::"} => ["::tl(foldr (fn (a,b) => 
","::(#cs(CPN'VarTable.find a))::".mkstr "::a::b)
(""::"]) (!CPN'bh"::(Int.toString i)::"))"::
gen_bh (i+1,grps,tail))
vars))
vars))
vars)

fun gen_fv (nil,tail) = tail
| gen_fv (v::vs,tail) = let
val cs = #cs(CPN'VarTable.find v)
in
","::"([\""::v::"\"],"::
"CPN'map (fn CPN'c => ["::cs::".mkstr CPN'c])\
\ ("::cs::".all()))"::
gen_fv(vs,tail)
end

in
"\n fun CPN'bindings_as_strings() = ["::
tl(gen_bh(1,groups,gen_fv(free_vars,""::"]"::tail)))
end

fun gen_var_lists(groups,free_vars,code_reg,tail) =
"\n val CPN'pickable_vars = ["::
concat(tl (foldr (fn (a,b) => ","::"\""^a^"\""::b) 
["","]"]  
((List.concat
(map (fn {vars,bops} => 
vars) groups))^^free_vars)))::
"\n val CPN'output_vars = ["::
(case code_reg of 
NONE => "]"
| SOME{output,input,action} => 
concat(tl(foldr (fn (a,b) => ","::"\""^a^"\""::b) 
["","]"] output)))::tail


(* Generate function for picking among particular set of 
* enabled bindings for the transition *)
fun gen_pick_bind(groups,free_vars,tail) = 
(* Generates:
* fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)
*   | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list) =
* let
*    val _ = case List.find (fn (CPN'var,CPN'val) => 
*           not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var)) 
*            CPN'bindlist of
*               NONE => ()
*             | SOME (CPN'var,_) => raise CPN'Stop ("This is not a variable for the transition "^CPN'name^": "^CPN'var)
*
* 	  val _ = case List.find 
*             (fn (CPN'var,CPN'val) => 
*             ListUtils.mem CPN'output_vars CPN'var) 
*             CPN'bindlist of
*             NONE => ()
*           | SOME (CPN'var,_) => raise CPN'Stop ("Cannot specify the value of output variable for the code segment :"^CPN'var)
*
*    val CPN'enabledbindings = CPN'bindings_as_strings()
* 	  val CPN'filteredbindings = 
*         (List.foldl CPN'Misc.filter_var_binding 
*         CPN'enabledbindings CPN'bindlist)
*         handle BindFatalFailure => []
* 	  val CPN'bindingPositions = 
*         if filteredbindings = []
*         then []
*         else CPN'Misc.get_binding_pos(filteredbindings,CPN'enabledbindings)
* in
* 	if CPN'filteredbindings = []
*	then raise CPN'CancelPickBind
*	else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)
* end
*)
"\n fun CPN'pick_bind (CPN'inst,[]) = CPN'man_bind(CPN'inst,false)\
\\n | CPN'pick_bind (CPN'inst,CPN'bindlist:(string * string) list)="::
"\n let"::
"\n val _ = case List.find (fn (CPN'var,CPN'val) => \
\\n not(ListUtils.mem (CPN'pickable_vars^^CPN'output_vars) CPN'var))\
\\n CPN'bindlist of NONE => ()\
\\n | SOME (CPN'var,_) => raise CPN'Stop (\"This is not a variable for the transition \"^CPN'name^\": \"^CPN'var)"::
"\n val _ = case List.find (fn (CPN'var,CPN'val) => \
\\n ListUtils.mem CPN'output_vars CPN'var) \
\\n CPN'bindlist of NONE => ()\
\\n | SOME (CPN'var,_) => raise CPN'Stop (\"Cannot specify the value of output variable for the code segment: \"^CPN'var)"::
"\n val CPN'enabledbindings = CPN'bindings_as_strings()\
\\n val CPN'filteredbindings = \n (List.foldl CPN'Misc.filter_var_binding \
\ CPN'enabledbindings CPN'bindlist) \
\\n handle BindFatalFailure => []"::
"\n val CPN'bindingpositions = \
\\n if CPN'filteredbindings = [] \n then [] \
\\n else CPN'Misc.get_binding_pos(CPN'filteredbindings,\
\CPN'enabledbindings)"::
"\n in"::
"\n if CPN'filteredbindings = []\
\\n then raise CPN'CancelPickBind\
\\n else CPN'occfun(CPN'inst,CPN'pick CPN'bindingpositions,true)"::
"\n end"::
tail

fun gen_man_bind (groups,free_vars,tail) = 
(* Generates:
* fun CPN'man_bind (CPN'inst, CPN'interactive:bool) = 
* CPN'occfun (CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true) 
*)
if !CPN'Settings.use_manbind then
"\n fun CPN'man_bind(CPN'inst,CPN'interactive:bool) = "::
"\n CPN'occfun (CPN'inst,CPN'pick (CPN'Response.man_bind (CPN'bindings_as_strings()) CPN'interactive),true)"::tail
else
tail

fun gen_code_action (_,_,NONE,tail) = tail 
| gen_code_action (t,page,SOME{input,output=_,action},tail) = let

fun gen_refs(page,tail) = let

val (instrefs, pagerefs) = 
case CPN'PageTable.peek page of
SOME{decl= NONE,...} => (nil,nil)
| SOME{decl= SOME{instrefs,pagerefs},...} => 
(map (fn r => CPN'RefTable.get_rep_name r) instrefs,
map (fn r => CPN'RefTable.get_rep_name r) pagerefs)
| _ => raise InternalError ("gen_code_action")
in
foldr (fn ((a,b),c) => "\n val "::a::"="::b::c)
(foldr (fn ((a,b),c) => "\n val "::a::
"= CPN'Array.sub("::b::",CPN'inst)"::c) tail instrefs)
pagerefs
end
in
"\n val CPN'code_action"::(CPN'Id.makeid t)::" = \
\CPN'Sim.code_action (fn CPN'inst => fn ("::
tl(foldr (fn (a,b) => ","::a::": "::(#cs(CPN'VarTable.find a))::b)
(""::") => let"::gen_refs(page,"\n in \n"::action::"\n end)"::tail))
input)
end

(* FIXME: almost same as gen_vars_record *)
fun gen_vars_record_type (groups,free_vars,code_reg,tail) = 
let
fun f (a,b) = ","::a::":":: (#cs (CPN'VarTable.find a))::b

val tail1 = 
case code_reg of
SOME {input,output,action} => 
(foldr f (""::"} "::tail) output)
| _ => (""::"} "::tail);
in
"{"::tl(foldr (fn ({vars,bops},tail2) => foldr f tail2 vars)
(foldr f tail1 free_vars) groups)
end

(* FIXME is this a reasonable function name? *)
fun gen_monitor_action (t, groups, free_vars, code_reg, [] , tail) = tail
| gen_monitor_action (t, groups, free_vars, code_reg, monitors, tail) =
let 
val tname =  case (peek t) of
SOME (transition _) => CPN'TransitionTable.get_name t
| _ => raise InternalError "gen_monitor_action"
fun f (a,b) = ";\n"::(CPN'MonitorTable.get_name a)::".monitor(("::
"CPN'"::(CPN'MonitorTable.get_name a)::"."::
tname::"(CPN'inst,"::
gen_vars_record(groups,free_vars,code_reg,")))"::b)
in
"\n val CPN'monitor"::(CPN'Id.makeid t)::
" = \nCPN'Sim.monitor (fn CPN'inst => \nfn "::
gen_vars_record_type(groups,free_vars,code_reg," => ("::
(tl (foldr f ("))"::tail) monitors)))

end				  

(***************************************************************************)

fun gen_bindings_fun (groups,free_vars,tail) = let
fun gen_bh (_,nil,free_vars) = map (fn var => (#cs(CPN'VarTable.find var)) ^ ".all()") free_vars
| gen_bh (i,{vars=nil,bops=_}::grps,free_vars) = gen_bh(i+1,grps,free_vars)
| gen_bh (i,{vars=vars,bops=_}::grps,free_vars) = ("!CPN'bh"^(Int.toString i))::gen_bh(i+1,grps,free_vars)

    fun gen_empty 0 = ""
      | gen_empty 1 = "[]"
      | gen_empty n = String.concat ["[], ", gen_empty (n-1)]

fun gen_tuple   (nil, tail) = tail
| gen_tuple   (group::nil, tail) = group::tail
| gen_tuple   (group::tl, tail) = group::","::gen_tuple  (tl, tail)

fun gen_expands (nil, tail) = tail
| gen_expands (group::nil, tail) = group::tail
| gen_expands (group::tl, tail) = "expand1("::group::","::gen_expands(tl, ")"::tail)

val depth = (List.length free_vars) + (num_groups groups)

fun gen_wrapped(groups,free_vars) = 
let
fun gen_nest i = if i<>(depth-1)
then "(CPN'e"^Int.toString i^","^(gen_nest (i+1))
else "CPN'e"^Int.toString i
fun gen_parens 1 = ""
| gen_parens i = ")"^(gen_parens (i-1))
in
case depth of 0 => "()" 
| 1 => "(CPN'e0)"
| _ => (gen_nest 0)^(gen_parens depth)
end

fun gen_unwrapped(groups,free_vars) =
let 
val depth = (List.length free_vars) + (num_groups groups)
fun gen_unnest i = if i=0 andalso i<>(depth - 1)
then "CPN'e"^Int.toString i^gen_unnest (i+1)
else if i=0 then "CPN'e"^Int.toString i
else if i<>(depth - 1) then ",CPN'e"^Int.toString i^gen_unnest (i+1)
else ",CPN'e"^Int.toString i
in 
case depth of 0 => "()"
| _ => "("^gen_unnest 0^")"
end	
in 
"fun CPN'bindings' CPN'inst = \n"::
(case (num_groups groups,List.length free_vars) of 
(0,0) => (* If there are no variables to bind, 
* we just check whether the transition is enabled or 
* not. If it is enabled we return [()], 
* since there is one binding with the binding type 
* unit, otherwise we return []. *)
"(case CPN'bind_exe(CPN'Sim.test,CPN'inst) of \n (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) \n | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings *) \n | _ => [()])\n"
| (0,f) => (* No vars on input arcs, some free vars 
* we must first check wether the transition is
* enabled or not. If it is, return bindings
* of free vars, otherwise return [] *)
String.concat ("(case CPN'bind_exe(CPN'Sim.test,CPN'inst) of \n(CPN'Sim.is_disabled,_) => ("::(gen_empty(f))::") (* not enabled, therefore no bindings *) \n | (CPN'Sim.is_maybe_ready_at _ ,_) => ("::(gen_empty(f))::") (* not enabled at current time, therefore no bindings *) \n | _ => ("::
(gen_tuple(gen_bh(1,groups,free_vars),["))\n"])))
| (g, f) => String.concat("(case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of _ => ("::
(gen_tuple(gen_bh(1,groups,free_vars),["))\n"]))))::
"fun CPN'bindings CPN'inst = \nlet\n\
\   fun expand1(CPN'l1,CPN'l2) = foldr (fn (CPN'e1,CPN'l1') => foldr (fn (CPN'e2,CPN'l2') => (CPN'e1,CPN'e2)::CPN'l2') CPN'l1' CPN'l2) [] CPN'l1;\n\
\   fun unwrap' "::gen_wrapped(groups,free_vars)::" = "::gen_unwrapped(groups,free_vars)::
"\n   fun unwrap (CPN'x::(CPN'y::CPN'ys)) = (unwrap' CPN'x)::(unwrap (CPN'y::CPN'ys)) \n\
\     | unwrap (CPN'x::nil) = [unwrap' CPN'x] \n\
\     | unwrap nil = nil\n\
\ in\n "::
(case (num_groups groups,List.length free_vars) of 
(0,0) => (* If there are no variables to bind, 
* we just check whether the transition is enabled or 
* not. If it is enabled we return [()], 
* since there is one binding with the binding type 
* unit, otherwise we return []. *)
"(case CPN'bind_exe(CPN'Sim.test,CPN'inst) of \n (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) \n | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings *) \n | _ => [()])\n end\n"
     ::tail
     | (0,_) => (* No vars on input arcs, some free vars 
* we must first check wether the transition is
* enabled or not. If it is, return bindings
* of free vars, otherwise return [] *)
("(case CPN'bind_exe(CPN'Sim.test,CPN'inst) of \n (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) \n | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings *) \n | _ => ("::
(gen_expands(gen_bh(1,groups,free_vars),"))\n end\n"::tail)))
| _ => ("(case CPN'bind_exe(CPN'Sim.all_enabled,CPN'inst) of \n (CPN'Sim.is_disabled,_) => [] (* not enabled, therefore no bindings *) \n | (CPN'Sim.is_maybe_ready_at _ ,_) => [] (* not enabled at current time, therefore no bindings*) \n  | _ => unwrap ("::
(gen_expands(gen_bh(1,groups,free_vars),"))\nend\n"::tail))))
end

fun create_transition (_,CPN'TransitionTable.substitution _) = 
display_status_bar()
| create_transition (t,CPN'TransitionTable.transition
{page,name,input,groups,time_reg,code_reg,chan_reg,priority_reg,free_vars,output,monitors,controllable}) = let

val _ = CPN'debug(concat["create_transition(",t,",{page=",page,
",name=",name,",monitors=[",
(concat(tl(foldr (fn (a,b) => ","::a::b) [""] monitors))),
"],...})"])

(* Create the following binding types and value
* CPN'BT<transition_ml_no> which specifies the types of arguments 
*    for the occurence function (CPN'occfun) for the transition
*    this type does not include output variables for code segments
* CPN'BRT<transition_ml_no> which is type for full bindings,
* val CPN'BR<transition_ml_no> = 
*       ref ({v1=,...} : CPN'BRT<transition_ml_no>)
* for use in the state space tool *)
fun create_binding_type () = 
let
val free_vars_size = List.length free_vars
val type_size = (List.length groups) + free_vars_size

fun has_vars {vars=nil,bops=_} = false
| has_vars {vars,bops=_} = true

fun gen ({vars=nil,bops=_},tail) = tail
| gen ({vars,bops=_},tail) =
"*"::"{"::
tl(foldr (fn (a,b) => ","::a::":"::(#cs (CPN'VarTable.find a))::b) (""::"}"::tail) vars)
in
if type_size = 0 then CPN'Env.use_string ("\ntype CPN'BT"::t::[" = unit"]) (* no free vars of groups *)
else 
if (free_vars_size > 0) then  (* free vars *)
CPN'Env.use_string ("\ntype CPN'BT"::t::" = "::tl(foldr gen
(foldr (fn (a,b) => "*"::(#cs (CPN'VarTable.find a))::b) (""::""::nil) free_vars)
groups))
else
if (foldr (fn (x,y) => has_vars x orelse y) false groups) then  (* no free vars, but one of the groups
* contains vars *)
CPN'Env.use_string ("\ntype CPN'BT"::t::" = "::tl(foldr gen
(foldr (fn (a,b) => "*"::(#cs (CPN'VarTable.find a))::b) (""::""::nil) free_vars)
groups))
else (* no free vars, and no group contains vars *)
CPN'Env.use_string ("\ntype CPN'BT"::t::[" = unit"]);
CPN'Env.use_string ("\n type CPN'BRT"::t::" = {"::
(tl(foldr (fn (v,sl)=>","::v::":"::(#cs (CPN'VarTable.find v))::sl) ["","}"]
(CPN'TransitionTable.get_all_vars t))));
CPN'Env.use_string ("\n val CPN'BR"::t::" = ref ({"::
(tl(foldr (fn (v,sl)=>","::v::"="::(#cs (CPN'VarTable.find v))::".base"::sl) ["","}: CPN'BRT",t,")"]
(CPN'TransitionTable.get_all_vars t))))
end


(* Generates:
* val CPN'code_action<no> = ref (fn (<input_pattern>,CPN'inst) => let
*     val "refs" = /internal reps of refs/
* in
*    "action"
* end
*
* val CPN'monitor<no> = (fn CPN'inst => fn <vars_record> => 
*      monitoring_part)
*
* fun CPN'transition<no> (CPN'mode,CPN'inst) = let
*     val CPN'bhi = /bh/
*     val (CPN'enough_tokens, CPN'answer) = /traverse places/
*     fun CPN'bindfun = /bf/
*     fun CPN'occfun = /occ/
*     fun CPN'man_bind = /man/
* in
*    if CPN'enough_tokens then
*        (CPN'bindfun(); 
*         case CPN'mode of
*             CPN'Sim.fast => CPN'occfun (!CPN'bh1,...,"free_vars",true)
*           | CPN'Sim.bind CPN'interactive=> CPN'man_bind (CPN'inst,CPN'interactive)
*           | _ => (CPN'Sim.is_executed,nil))
*          handle BindFatalFailure => (CPN'answer,nil)
*     else (CPN'answer,nil)
* end
*)

val no = Int.toString(CPN'Misc.next())

local
fun find_cs (B_c{cs,var},tail) = let
val rs_name = concat ["CPN'",cs,"_rs_",var]
in
(case CPN'IMSTable.peek rs_name of
SOME _ => rs_name::tail
| NONE => 
(Decl.append_all cs;
CPN'Env.use_string
["\nval ",rs_name," = CPN'RS.create (",cs,".all())"];
CPN'IMSTable.insert (rs_name,"RS");
rs_name::tail))
end
| find_cs (_,tail) = tail
in
val init_cs = 
foldr (fn ({bops,...},tail) => foldr find_cs tail bops) nil groups
end

fun gen_bh_call(groups,tail) = let
fun gen (_,nil) = ""::tail
| gen (i,{vars=nil,bops=_}::grps) = gen(i+1,grps)
| gen (i,{vars,...}::grps) = 
","::"CPN'hd(!CPN'bh"::(Int.toString i)::")"::gen(i+1,grps)
in
tl(gen(1,groups))
end

fun gen_bind_free_vars (vars,empty_prefix,tail) = let
fun f (a,b) = let
val cs = #cs(CPN'VarTable.find a)
in
if CPN'Env.is_def (cs^".ran()") then
","::cs::".ran()"::b
else
","::"CPN'MS.random("::cs::".all())"::b
end
val tmpres= foldr f tail vars
in
if empty_prefix andalso not(List.null(vars)) then
tl tmpres (* remove leading comma *)
else
tmpres
end

    fun gen_expander 0 = "fun CPN'expand0 _ = (1, [])\n"
      | gen_expander n =
      let
          fun gen_header 0 = []
            | gen_header 1 = ["CPN'lst0"]
            | gen_header n = "CPN'lst"::(Int.toString (n-1))::", "::(gen_header (n-1))
          fun gen_count  0 = ["1"]
            | gen_count  n = "(List.length CPN'lst"::(Int.toString (n-1))::") * "::(gen_count (n-1))
          fun gen_body   0 = []
            | gen_body   1 = ["CPN'Random.int (List.length CPN'lst0)"]
            | gen_body   n = "CPN'Random.int (List.length CPN'lst"::(Int.toString (n-1))::"), "::(gen_body   (n-1))
      in
          String.concat ["fun CPN'expand", Int.toString n, "(",
          String.concat(gen_header n), ") = (", String.concat(gen_count n), ", [", String.concat(gen_body n),
          "])\n"] 
      end

fun groups_has_no_vars grps = foldr (fn ({vars=v,bops=_},res)=> v=nil andalso res) true grps

val _ = display_status_bar()
in
create_binding_type();
CPN'Env.use_string (gen_code_action(t,page,code_reg,
gen_monitor_action (t,groups,free_vars,code_reg,monitors, 
"\n structure CPN'transition"::(CPN'Id.makeid t)::
" = (* "::CPN'TransitionTable.get_name t::" *)\n struct \n"::
" val CPN'id = "::(CPN'Id.toString t)::
"\n val CPN'name = \""::(CPN'TransitionTable.get_name t)::
"\"\n val CPN'priority = ("::(if Option.isSome priority_reg andalso priority_reg <> SOME "" then Option.valOf priority_reg else "1000")::
")\n val CPN'controllable = "::(Bool.toString controllable)::
gen_var_lists(groups,free_vars,code_reg,
gen_bh_ref(groups,
gen_bindfun(t,groups,
gen_occ(t,page,input,groups,free_vars,code_reg,time_reg,output,monitors,
gen_pick (groups, free_vars,
gen_bindings_as_strings (groups, free_vars,
gen_man_bind(groups,free_vars,
gen_pick_bind (groups, free_vars,
"\n fun CPN'bind_exe(CPN'mode,CPN'inst) = \nlet\n"::
gen_trv(input,init_cs,
"\n(* Force bindfun to calculate all bindings for interactive \
\\n manual binding and for picking a binding*)\n\
\val CPN'bindfunmode = case CPN'mode of\
\\n (CPN'Sim.bind true) => CPN'Sim.all_enabled\
\\n | (CPN'Sim.pick (CPN'x::CPN'xs)) => CPN'Sim.all_enabled\
\\n | _ => CPN'mode"::
"\nin\nif CPN'enough_tokens then \n\
\ (CPN'bindfun(CPN'bindfunmode,CPN'inst);\n\
\ case CPN'mode of \nCPN'Sim.fast => CPN'occfun (CPN'inst,("::
gen_bh_call(groups, gen_bind_free_vars(free_vars,groups_has_no_vars groups,"),true)\n"::
(if !CPN'Settings.use_manbind then 
" | CPN'Sim.bind CPN'interactive=> CPN'man_bind(CPN'inst,CPN'interactive)\n"
else "")::
" | CPN'Sim.pick CPN'l => CPN'pick_bind(CPN'inst,CPN'l)\n"::
" | CPN'Sim.all_enabled => (CPN'Sim.is_executed,nil)\n"::  
" | _ => (CPN'Sim.is_executed,nil))\
\ handle BindFatalFailure => (CPN'answer,nil)\n\
\ else (CPN'answer,nil)\n\
\ end \n" ::
gen_bindings_fun(groups,free_vars,
"\n end; (* end CPN'transition"::
(CPN'Id.makeid t)::" *) \n"::
(gen_expander (num_groups groups + List.length free_vars))::
" val _ = CPN'Sim.add_be("::(CPN'Id.toString t)::","::
"CPN'transition"::(CPN'Id.makeid t)::".CPN'bind_exe,"::
"CPN'Sim.bind_fair CPN'transition"::
(CPN'Id.makeid t)::".CPN'bindings' CPN'transition"::
(CPN'Id.makeid t)::".CPN'occfun CPN'transition"::
(CPN'Id.makeid t)::".CPN'pick CPN'transition"::
(CPN'Id.makeid t)::".CPN'bind_exe CPN'expand"::
(Int.toString (num_groups groups + List.length free_vars))::", "::
"CPN'transition"::(CPN'Id.makeid t)::".CPN'priority);"::
nil)))))))))))))))
end	 

in
fun create_all () = let
val list = CPN'TransitionTable.list ()
in
total_transitions:= Int.toString(length list);
CPN'report_timing (concat["create transitions @ "]);
app create_transition list
end

fun create_some trans_ids = let
val list = map (fn a => (a,find a)) trans_ids
in
total_transitions:= Int.toString(length list);
CPN'report_timing (concat["create some transitions @ "]);
app create_transition list
end
end

end (* functor *)
