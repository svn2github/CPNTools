signature MONOID =
    sig
        type t

        val zero : t
        val + : t * t -> t

        (*
         * Invariants:
         *
         * x + (y + z) = (x + y) + z
         * zero + y = y
         * x + zero = x
         *)
    end
