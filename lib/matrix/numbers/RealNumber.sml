structure RealNumber : NUMBER
    where type t = real
    =
    struct
        type t = real

        val zero = 0.0

        val op + : t * t -> t = Real.+

        val ~ : t -> t = Real.~

        val op - : t * t -> t = Real.-

        val op * : t * t -> t = Real.*

        val one = 1.0

        val op / : t * t -> t = Real./

        (* Derived form. *)
        fun inv x = 1.0 / x

        (* Useful functions. *)
        val sqrt = Math.sqrt
        val ln = Math.ln

        fun fromReal x = x
        fun toReal x = x

	val compare = Real.compare
	val == = Real.==
	val != = Real.!=
	val op < = Real.<
	val op <= = Real.<=
	val op > = Real.>
	val op >= = Real.>=

	val min = Real.min
	val max = Real.max

	fun halve(a) = a / 2.0
	val negInf = Real.negInf
	val posInf = Real.posInf
	val toString = Real.toString

    end
