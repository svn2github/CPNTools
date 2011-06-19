signature RING =
    sig
        include RNG

        val one : t

        (*
         * Invariants:
         *
         * one * x = x
         * x * one = x
         *)
    end
