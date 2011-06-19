signature GROUP =
    sig
        include MONOID

        val ~ : t -> t

        (*
         * Invariants:
         *
         * x + ~x = zero
         * ~x + x = zero
         *)

        (*
         * For convenience only, since derived form.
         *
         * a - b = a + ~b
         *)
        val - : t * t -> t
    end
