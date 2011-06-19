(*
 * E.g., a real, or an interval.  Used to make vectors.
 *)
signature UNORDERED_NUMBER =
    sig
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
    end
