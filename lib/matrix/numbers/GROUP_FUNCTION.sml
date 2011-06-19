(* Example of pointwise function operations. *)

signature GROUP_FUNCTION =
    sig
        structure Any : ANY
        structure Group : GROUP

        include GROUP
        where type t = Any.t -> Group.t
    end