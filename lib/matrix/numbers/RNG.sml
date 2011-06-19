signature RNG =
    sig
        include GROUP

        val * : t * t -> t

        (*
         * Invariants:
         *
         * x * (y * z) = (x * y) * z
         * x * (y + z) = x * y + x * z
         * (x + y) * z = x * z + y * z
         *)
    end
