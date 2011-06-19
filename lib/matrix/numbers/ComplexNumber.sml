structure ComplexNumber : COMPLEX
    where type t = (real * real)
    =
    struct
        type t = (real * real)

        val zero = (0.0, 0.0)
        val one = (1.0, 0.0)
	val negInf = (Real.negInf, Real.negInf)
	val posInf = (Real.posInf, Real.posInf)

        fun ~ (a, b) = 
          (Real.~a, Real.~b)

        fun conj (a, b) =
          (a, Real.~b)

        fun norm (a, b) =
          Math.sqrt(Real.+(Real.*(a, a), Real.*(b, b)))

        fun op + ((a, b), (c, d)) =
          (Real.+(a, c), Real.+(b, d))

        fun op - ((a, b), (c, d)) =
          (Real.-(a, c), Real.-(b, d))

        fun op * ((a, b), (c, d)) =
          (Real.-(Real.*(a, c), Real.*(b, d)), Real.+(Real.*(a, d), Real.*(b, c)))

        fun op scal (a, (c, d)) =
          (Real.*(a, c), Real.*(a, d))

        fun op / ((a, b), (c, d)) =
          scal((Real.+(Real.*(c, c), Real.*(d, d))), (a, b) * conj(c, d))

        (* Derived form. *)
        fun inv x = one / x

        (* Useful functions. *)
        fun sqrt (a, b) = 
          let
            val theta = Real.*(0.5, Math.atan(Real./(b,a)))
          in
            scal(norm(a,b), (Math.cos(theta), Math.sin(theta)))
          end
        fun ln (a, b) = 
          (Math.ln(norm(a, b)), Math.atan(Real./(b, a)))

        fun fromReal a = (a, 0.0)
        fun toReal (a, b) = a

        fun fromReals (a, b) = (a, b)
        fun toReals (a, b) = (a, b)

        fun real (a, b) = a
        fun imaginary (a, b) = b

	fun compare (a, b) = Real.compare(norm(a), norm(b))
	fun op == ((a, b), (c, d)) = 
          if(Real.==(a, c) andalso Real.==(b, d)) then true else false
	fun op != ((a, b), (c, d)) = 
          if(Real.!=(a, c) orelse Real.!=(b, d)) then true else false
	fun op < (a, b) = 
          if(Real.<(norm(a), norm(b))) then true else false
	fun op <= (a, b) =
          if(Real.<=(norm(a), norm(b))) then true else false
	fun op > (a, b) =
          if(Real.>(norm(a), norm(b))) then true else false
	fun op >= (a, b) = 
          if(Real.>=(norm(a), norm(b))) then true else false

	fun min (a, b) = if(a < b) then a else b
	fun max (a, b) = if(a > b) then a else b

	fun halve(a) = scal(Real./(1.0, 2.0), a)
	fun toString (a, b) = "("^(Real.toString a)^", "^(Real.toString b)^")"

    end
