signature INNER_PROD_SPACE =
    sig
        include VECTOR_SPACE

        val innerProduct : t * t -> Field.t

        (*
         * (We don't consider complex.)
         *
         * (u, v) = (v, u)
         * (u, u) >= 0 and (u, u) = 0 iff u = 0
         * (a*u + b*v, w) = a(u, w) + b(v, w)
         *)

        (*
         * length or norm
         *
         * For convenience.
         * ||v|| = sqrt (v, v)
         *)
        val length : t -> Field.t
    end
