(* "Multiple inheritance" *)

signature ORDERED_NUMBER =
    sig
        (*
         * No longer legal in SML 97.
         * I manually insert instead.
         * TODO Do cleaner.
         *)

        (*include NUMBER*)
        include FIELD

        (* Convert from/to real. *)
        val fromReal : real -> t
        val toReal : t -> real

        (*
         * Useful functions, modeled after Real.
         *
         * Unfortunately, we'll have to manually "lift" for each
         * implementation.
         *)
        val sqrt : t -> t

        (*include ORDERED*)
        val compare : t * t -> order

        (* For convenience. *)
        val == : t * t -> bool
        val != : t * t -> bool
        val < : t * t -> bool
        val <= : t * t -> bool
        val > : t * t -> bool
        val >= : t * t -> bool

        val min : t * t -> t
        val max : t * t -> t

        (* FMC uncertain about where to put these.
         * Not necessarily relevant to just plain "ordering".
         *)

        (* Needed for bisection of region. *)
        val halve : t -> t

        (* Negative and positive "infinity". *)
        val negInf : t
        val posInf : t

        val toString : t -> string
    end
