signature DIVISION_RING =
    sig
        include RING

        val inv : t -> t

        (*
         * For convenience only.
         *
         * a / b = a * inv b
         *)
        val / : t * t -> t

        (*
         * Invariants:
         *
         * if y <> zero, then x * inv x = one
         *)
    end
