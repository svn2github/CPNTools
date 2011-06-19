(*
 * Basic building block of arithmetic.
 * Currently, packed with all sorts of stuff.
 * TODO Split off ordering?  Split off library functions?
 *)

signature COMPLEX =
    sig

        include NUMBER

        val fromReals : (real * real) -> t
        val toReals : t -> (real * real)
          
        val conj : t -> t
        val norm : t -> real
        val scal : (real * t) -> t

        val real : t -> real
        val imaginary : t -> real

    end
