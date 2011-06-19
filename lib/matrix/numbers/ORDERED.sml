signature ORDERED =
    sig
        type t

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
    end
