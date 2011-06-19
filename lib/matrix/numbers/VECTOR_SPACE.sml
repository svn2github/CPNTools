signature VECTOR_SPACE =
    sig
        include ABELIAN_GROUP

        structure Field : FIELD

        val scale : Field.t * t -> t

        (*
         * a scale (v + w) = a scale v + a scale w
         * (a + b) scale v = a scale v + b
         * a scale (b scale v) = (a * b) scale v
         * one scale v = v
         *)
    end
