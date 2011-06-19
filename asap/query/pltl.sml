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
structure PLTLSyntax =
struct
    local
        open EqSet
    in

    exception Error of string

    datatype 'a formula = ATOMIC of 'a
                        | NOT of 'a formula
                        | NEXT of 'a formula
                        | FUTURE of 'a formula
                        | GLOBALLY of 'a formula
                        | UNTIL of 'a formula * 'a formula
                        | WUNTIL of 'a formula * 'a formula
                        | RELEASES of 'a formula * 'a formula
                        | SRELEASES of 'a formula * 'a formula
                        | AND of 'a formula * 'a formula
                        | OR of 'a formula * 'a formula
                        | IMPLIES of 'a formula * 'a formula
                        | IFF of 'a formula * 'a formula
                        | TRUE
                        | FALSE

    datatype 'a vertex = init
                       | VERTEX of 'a vertex set ref * 'a formula set ref * 'a
                       formula set ref * 'a formula set ref

    type state = int

    fun sub (ATOMIC f) = [ATOMIC f]
      | sub (NOT f) = (NOT f)::(sub f)
      | sub (NEXT f) = (NEXT f)::(sub f)
      | sub (FUTURE f) = raise Error "simplify first"
      | sub (GLOBALLY f) = raise Error "simplify first"
      | sub (UNTIL (f, g)) =List.@((UNTIL (f, g))::(sub f), (sub g))
      | sub (WUNTIL (f, g)) = raise Error "simplify first"
      | sub (RELEASES (f, g)) =List.@((RELEASES (f, g))::(sub f), (sub g))
      | sub (SRELEASES (f, g)) = raise Error "simplify first"
      | sub (AND (f, g)) =List.@((AND (f, g))::(sub f), (sub g))
      | sub (OR (f, g)) =List.@((OR (f, g))::(sub f), (sub g))
      | sub (IMPLIES (f, g)) = raise Error "simplify first"
      | sub (IFF (f, g)) = raise Error "simplify first"
      | sub TRUE = [TRUE]
      | sub FALSE = [FALSE]

    fun AP (ATOMIC f) = [ATOMIC f]
      | AP (NOT f) = (AP f)
      | AP (NEXT f) = (AP f)
      | AP (FUTURE f) = raise Error "simplify first"
      | AP (GLOBALLY f) = raise Error "simplify first"
      | AP (UNTIL (f, g)) = union (AP f) (AP g)
      | AP (WUNTIL (f, g)) = raise Error "simplify first"
      | AP (RELEASES (f, g)) = union (AP f) (AP g)
      | AP (SRELEASES (f, g)) = raise Error "simplify first"
      | AP (AND (f, g)) = union (AP f) (AP g)
      | AP (OR (f, g)) = union (AP f) (AP g)
      | AP (IMPLIES (f, g)) = raise Error "simplify first"
      | AP (IFF (f, g)) = raise Error "simplify first"
      | AP TRUE = [TRUE]
      | AP FALSE = [TRUE]

    fun simplify (ATOMIC f) = ATOMIC f
      | simplify (NOT f) = NOT (simplify f)
      | simplify (NEXT f) = NEXT (simplify f)
      | simplify (FUTURE f) = simplify (UNTIL (TRUE, f))
      | simplify (GLOBALLY f) = simplify (RELEASES (FALSE, f))
      | simplify (UNTIL (f, g)) = UNTIL (simplify f, simplify g)
      | simplify (WUNTIL (f, g)) = simplify (OR (UNTIL (f, g), GLOBALLY g))
      | simplify (RELEASES (f, g)) = RELEASES (simplify f, simplify g)
      | simplify (SRELEASES (f, g)) = simplify (NOT (WUNTIL (NOT f, NOT g)))
      | simplify (AND (f, g)) = AND (simplify f, simplify g)
      | simplify (OR (f, g)) = OR (simplify f, simplify g)
      | simplify (IMPLIES (f, g)) = simplify (OR (NOT f, g))
      | simplify (IFF (f, g))
        = simplify (AND (IMPLIES (f, g), IMPLIES (g, f)))
      | simplify TRUE = TRUE
      | simplify FALSE = FALSE

    fun simplify''' (ATOMIC f) = ATOMIC f
      | simplify''' (NOT f) = NOT (simplify''' f)
      | simplify''' (NEXT f) = NEXT (simplify''' f)
      | simplify''' (FUTURE f) = raise Error "simplify first"
      | simplify''' (GLOBALLY f) = raise Error "simplify first"
      | simplify''' (UNTIL (f, g)) = UNTIL (simplify''' f, simplify''' g)
      | simplify''' (WUNTIL (f, g)) = raise Error "simplify first"
      | simplify''' (RELEASES (f, g)) = NOT (UNTIL (NOT (simplify''' f), NOT (simplify''' g)))
      | simplify''' (SRELEASES (f, g)) = raise Error "simplify first"
      | simplify''' (AND (f, g)) = AND (simplify''' f, simplify''' g)
      | simplify''' (OR (f, g)) = OR (simplify''' f, simplify''' g)
      | simplify''' (IMPLIES (f, g)) = raise Error "simplify first"
      | simplify''' (IFF (f, g)) = raise Error "simplify first"
      | simplify''' TRUE = TRUE
      | simplify''' FALSE = FALSE

    fun simplify' (NOT (ATOMIC f)) = NOT (ATOMIC f)
      | simplify' (NOT (NOT f)) = simplify' f
      | simplify' (NOT (NEXT f)) = simplify' (NEXT (NOT f))
      | simplify' (NOT (UNTIL (f, g)))
        = simplify' (RELEASES (NOT f, NOT g))
      | simplify' (NOT (RELEASES (f, g)))
        = simplify' (UNTIL (NOT f, NOT g))
      | simplify' (NOT (AND (f, g))) = simplify' (OR (NOT f, NOT g))
      | simplify' (NOT (OR (f, g))) = simplify' (AND (NOT f, NOT g))
      | simplify' (NOT (TRUE)) = FALSE
      | simplify' (NOT (FALSE)) = TRUE
      | simplify' (NOT (WUNTIL _)) = raise Error "Use simplify first"
      | simplify' (NOT (SRELEASES _)) = raise Error "Use simplify first"
      | simplify' (NOT (FUTURE _)) = raise Error "Use simplify first"
      | simplify' (NOT (GLOBALLY _)) = raise Error "Use simplify first"
      | simplify' (NOT (IMPLIES _)) = raise Error "Use simplify first"
      | simplify' (NOT (IFF _)) = raise Error "Use simplify first"
      | simplify' (ATOMIC f) = ATOMIC f
      | simplify' (NEXT f) = NEXT (simplify' f)
      | simplify' (UNTIL (f, g)) = UNTIL (simplify' f, simplify' g)
      | simplify' (RELEASES (f, g)) = RELEASES (simplify' f, simplify' g)
      | simplify' (AND (f, g)) = AND (simplify' f, simplify' g)
      | simplify' (OR (f, g)) = OR (simplify' f, simplify' g)
      | simplify' (TRUE) = TRUE
      | simplify' (FALSE) = FALSE
      | simplify' (WUNTIL _) = raise Error "Use simplify first"
      | simplify' (SRELEASES _) = raise Error "Use simplify first"
      | simplify' (FUTURE _) = raise Error "Use simplify first"
      | simplify' (GLOBALLY _) = raise Error "Use simplify first"
      | simplify' (IMPLIES _) = raise Error "Use simplify first"
      | simplify' (IFF _) = raise Error "Use simplify first"

    fun rewrite rule formula =
    let
        val changed = ref false
        fun modify f = case rule f
                         of (SOME f') => (changed := true; f')
                          | NONE => f
        fun rewrite' (ATOMIC f) = ATOMIC f
          | rewrite' (NOT f) = modify (NOT (rewrite' f))
          | rewrite' (NEXT f) = modify (NEXT (rewrite' f))
          | rewrite' (FUTURE f) = modify (FUTURE (rewrite' f))
          | rewrite' (GLOBALLY f) = modify (GLOBALLY (rewrite' f))
          | rewrite' (UNTIL (f, g)) = modify (UNTIL (rewrite' f, rewrite' g))
          | rewrite' (WUNTIL (f, g)) = modify (WUNTIL (rewrite' f, rewrite' g))
          | rewrite' (RELEASES (f, g)) = modify (RELEASES (rewrite' f, rewrite' g))
          | rewrite' (SRELEASES (f, g)) = modify (SRELEASES (rewrite' f, rewrite' g))
          | rewrite' (AND (f, g)) = modify (AND (rewrite' f, rewrite' g))
          | rewrite' (OR (f, g)) = modify (OR (rewrite' f, rewrite' g))
          | rewrite' (IMPLIES (f, g)) = modify (IMPLIES (rewrite' f, rewrite' g))
          | rewrite' (IFF (f, g)) = modify (IFF (rewrite' f, rewrite' g))
          | rewrite' TRUE = TRUE
          | rewrite' FALSE = FALSE
        val _ = changed := false
        val result = modify (rewrite' formula)
    in
        (result, !changed)
    end

    fun rewrite' rules formula =
    let
        fun rewrite'' [] f = (f, false)
          | rewrite'' (rule::rules') f =
              case rewrite rule f
                of (f', true) => (f', true)
                 | (f', false) => rewrite'' rules' f'
        fun rewrite''' formula =
              case rewrite'' rules formula
                of (f', true) => rewrite''' f'
                 | (f', false) => f'
    in
        rewrite''' formula
    end

    val simpleRules = [
        fn (AND (f, TRUE)) => SOME f
         | (AND (TRUE, f)) => SOME f
         | (AND (f, FALSE)) => SOME FALSE
         | (AND (FALSE, f)) => SOME FALSE
         | (AND (f, NOT g)) => if f = g then SOME FALSE else NONE
         | (AND (NOT f, g)) => if f = g then SOME FALSE else NONE
         | (OR (f, TRUE)) => SOME TRUE
         | (OR (TRUE, f)) => SOME TRUE
         | (OR (f, FALSE)) => SOME f
         | (OR (FALSE, f)) => SOME f
         | (OR (f, NOT g)) => if f = g then SOME TRUE else NONE
         | (OR (NOT f, g)) => if f = g then SOME TRUE else NONE
         | (NOT (NOT f)) => SOME f
         | _ => NONE,
        fn (AND (f, g)) => if f = g then SOME f else NONE
         | (OR (f, g)) => if f = g then SOME f else NONE
         | _ => NONE,
         fn (UNTIL (NEXT f, NEXT g)) => SOME (NEXT (UNTIL (f, g)))
          | (AND (RELEASES (f, g), RELEASES (h, i))) =>
                  if f = h then SOME (RELEASES(f, AND (g, i))) else NONE
          | (OR (RELEASES (f, g), RELEASES (h, i))) =>
                  if g = i then SOME (RELEASES(OR (f, h), g)) else NONE
          | (AND (NEXT f, NEXT g)) => SOME (NEXT (AND (f, g)))
          | _ => NONE,
         fn (NEXT (TRUE)) => SOME TRUE
          | (UNTIL (f, FALSE)) => SOME FALSE
          | (OR (RELEASES (FALSE, UNTIL (TRUE, f)), RELEASES (FALSE, UNTIL (TRUE, g)))) =>
                  SOME (RELEASES (FALSE, UNTIL (TRUE, (OR (f, g)))))
          | (UNTIL (TRUE, NEXT f)) => SOME (NEXT (UNTIL (TRUE, f)))
          | (RELEASES (FALSE, RELEASES (FALSE, UNTIL (TRUE, f)))) => SOME (RELEASES (FALSE, UNTIL (TRUE, f)))
          | (UNTIL (TRUE, RELEASES (FALSE, UNTIL (TRUE, f)))) => SOME (RELEASES (FALSE, UNTIL (TRUE, f)))
          | (NEXT (RELEASES (FALSE, UNTIL (TRUE, f)))) => SOME (RELEASES (FALSE, UNTIL (TRUE, f)))
          | (UNTIL (TRUE, AND (f, RELEASES (FALSE, UNTIL (TRUE, g))))) =>
                  SOME (AND (GLOBALLY f, RELEASES (FALSE, UNTIL (TRUE, g))))
          | (RELEASES (FALSE, OR (f, RELEASES (FALSE, UNTIL (TRUE, g))))) =>
                  SOME (OR (GLOBALLY f, RELEASES (FALSE, UNTIL (TRUE, g))))
          | (NEXT (AND (f, RELEASES (FALSE, UNTIL (TRUE, g))))) =>
                  SOME (AND (NEXT f, RELEASES (FALSE, UNTIL (TRUE, g))))
          | (NEXT (OR (f, RELEASES (FALSE, UNTIL (TRUE, g))))) =>
                  SOME (OR (NEXT f, RELEASES (FALSE, UNTIL (TRUE, g))))
          | _ => NONE
    ]


    fun graphify formula =
    let
        fun expand (node as VERTEX (P, N, O, Sc), nodes) =
            (case (!N) of
                 [] => 
                 let
                     fun exists NONE =
                         expand (VERTEX (ref [node], ref (!Sc), ref [], ref[]),
                         if !N = [FALSE] then nodes else node::nodes)
                       | exists (SOME (VERTEX (P', N', O', Sc'))) =
                       (P':=union (!P') (!P); nodes)
                       | exists _ = raise Error "Should never happen"
                 in exists (List.find (fn (VERTEX (P', N', O', Sc')) => ((equals
                 (!Sc) (!Sc')) andalso (equals (!O) (!O'))) | _ => raise Error
                 "Should never happen") nodes)
                 end
                 | (n::ns) =>
                   let
                       val _ = N := ns

                       fun union' ss [TRUE] = ss
                         | union' ss [FALSE] = [FALSE]
                         | union' [FALSE] ss = [FALSE]
                         | union' ss rest = union ss rest

                       fun obligation FALSE = nodes
                         | obligation TRUE = expand(node, nodes)
                         | obligation (psi as (ATOMIC f)) =
                         (if has (NOT psi) (!O)
                          then nodes
                          else (O:=(union' (!O) [psi]); expand(node, nodes)))
                         | obligation (psi as (NOT (ATOMIC f))) =
                         (if has (ATOMIC f) (!O)
                          then nodes
                          else (O:=(union' (!O) [psi]); expand(node, nodes)))
                         | obligation (psi as AND (f1, f2)) =
                         (N := union' (!N) (subtract [f1, f2] (!O)); O := union' (!O) [psi]; expand(node, nodes))
                         | obligation (NEXT f) =
                         (Sc := union' (!Sc) [f]; expand(node, nodes))
                         | obligation (psi as OR (f1, f2)) =
                         let
                             val node1 = VERTEX (ref (!P), ref (union' (!N) (subtract [f1] (!O))), ref (union' (!O) [psi]), ref (union' (!Sc) []))
                             val node2 = VERTEX (ref (!P), ref (union' (!N) (subtract [f2] (!O))), ref (union' (!O) [psi]), ref (!Sc))
                         in expand (node1, expand(node2, nodes))
                         end
                         | obligation (psi as UNTIL (f1, f2)) =
                         let
                             val node1 = VERTEX (ref (!P), ref (union' (!N) (subtract [f1, NEXT psi] (!O))), ref (union' (!O) [psi]), ref (union' (!Sc) []))
                             val node2 = VERTEX (ref (!P), ref (union' (!N) (subtract [f2] (!O))), ref (union' (!O) [psi]), ref (!Sc))
                         in expand (node1, expand(node2, nodes))
                         end
                         | obligation (psi as RELEASES (f1, f2)) =
                         let
                             val node1 = VERTEX (ref (!P), ref (union' (!N) (subtract [f1, f2] (!O))), ref (union' (!O) [psi]), ref (union' (!Sc) []))
                             val node2 = VERTEX (ref (!P), ref (union' (!N) (subtract [f2, NEXT psi] (!O))), ref (union' (!O) [psi]), ref (!Sc))
                         in expand (node1, expand(node2, nodes))
                         end
                         | obligation _ = raise Error "Should never happen"

                   in obligation n
                   end)
        | expand _ = raise Error "Should never happen"

    in fromList (expand (VERTEX (ref[init], ref [formula], ref [], ref[]), []))
    end

    fun glbaify graph formula =
    let
        val S = graph
        val S0 = fromList (List.filter (fn (VERTEX (ref P, _, _, _)) => (has
        init P) | _ => raise Error "Should never happen") (toList graph))
        fun next (v as VERTEX _) = fromList (List.filter (fn (VERTEX (ref P, _,
            _, _)) => (has v P) | _ => raise Error "Should never happen") (toList graph))
          | next _ = raise Error "Should never happen"
        fun pos (VERTEX (_, _, ref O, _)) = fromList (List.filter (fn (ATOMIC _)
	  => true | TRUE => true | _ => false) O)
          | pos _ = raise Error "Should never happen"
        fun neg (VERTEX (_, _, ref O, _)) = fromList (List.filter (fn (NOT (ATOMIC _)) =>
	  true | FALSE => true | _ => false) O)
          | neg _ = raise Error "Should never happen"
        val untils = List.filter (fn (UNTIL _) => true | _ => false) (sub (simplify''' formula))

        val final = fromList (List.map (fn (UNTIL (f1, f2)) => (fromList (List.filter (fn
        (v as VERTEX (_, _, ref O, _)) => ((not (has (UNTIL (f1, f2)) O)) orelse
        (has f2 O)) | _ => raise Error "Should never happen") (toList graph))) | _ => raise Error "Should never happen") untils)

        val ap = AP formula
        fun labels (v as VERTEX _) =
        let
        in
            fromList (List.@(pos v, neg v))
        end
          | labels _ = raise Error "Should never happen"

    in (S, S0, next, final, labels)
    end

    fun lbaify (S, S0, next, final, labels) =
    let
        val k = List.length final

	  fun pair (l, n) = List.map (fn elm => (elm, n)) l

        val S' =
            if k = 0
            then pair (S, 1)
            else List.concat (List.map (fn s => List.tabulate (k, fn i => (s, i + 1))) S)

	  fun shortest [] n = (S, 1) (* Quadratic! Can be optimized using accumulator *)
	  | shortest ([l]) n = (l, n)
	  | shortest (hd::tl) n =
	  let
	  val (l, n') = shortest tl (n+1)
	  in
	  if List.length hd < List.length l
	  then (hd, n)
	  else (l, n')
	  end

	  fun pair' ([], _) = []
	  | pair' (l, n) =
	  if k = 0
        then pair (S, 1)
	  else pair (l, n)

	  val S0' = pair' (S0, 1)

        fun next' (s, i) =
        let
            val Fi = 
                if k = 0
                then []
                else List.nth (final, (i - 1))
            val s' = next s
        in if k > 0 andalso (has s Fi)
           then List.map (fn s => (s, (i mod k) + 1)) s'
           else List.map (fn s => (s, i)) s'
        end

        val final' = pair (shortest final 1)

        fun labels' (s, i: int) = labels s
    in (S', S0', next', final', labels')
    end

    fun compile (S, S0, next, final, labels) =
    let
        fun pos elm =
	  let
            fun walk (hd::tl) n =
                if hd = elm
                then n
                else walk tl (n+1)
              | walk _ _ = raise Error "Should never happen"
        in walk S 0
        end

        val S' = List.map (fn s => (List.map pos (next s), labels s)) S
        val S'' = Vector.fromList S'
        val S0' = List.map pos S0
        val final' = List.map pos final

    in (S'', S0', final')
    end

    fun map (S, S0, final) map =
    let
        fun map' (ATOMIC ap) = ATOMIC (map ap)
          | map' (NOT f) = NOT (map' f)
          | map' TRUE = TRUE
          | map' FALSE = FALSE
          | map' _ = raise Error "Invalid automaton"
        val map'' = List.map map'
    in
        (Vector.map (fn (s', l) => (s', map'' l)) S, S0, final)
    end

    fun simplify'' formula =
        rewrite' simpleRules (simplify' (rewrite' simpleRules (simplify
        (rewrite' simpleRules formula))))

    fun translate formula =
    let
        val formula' =
            simplify'' formula
    in compile (lbaify (glbaify (graphify formula') formula'))
    end
    end
end

signature BUCHI_EXPRESSION =
sig
    type state

    val initials : int list
    val accepting : int list 
    val transitionTable : (int list * (state -> bool) PLTLSyntax.formula list) Vector.vector
    val labels : (string list) Vector.vector
end
