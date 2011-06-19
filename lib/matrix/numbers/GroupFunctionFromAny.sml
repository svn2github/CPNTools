functor GroupFunctionFromAny (structure Any : ANY
                              structure Group : GROUP)
    :> GROUP_FUNCTION
    =
    struct
        structure Any = Any
        structure Group = Group

        type t = Any.t -> Group.t

        val zero : t =
            fn _ => Group.zero

        fun (p + p') arg =
            let
                open Group
            in
                p arg + p' arg
            end

        fun (p - p') arg =
            let
                open Group
            in
                p arg - p' arg
            end

        fun ~ p arg =
            let
                open Group
            in
                ~ (p arg)
            end
    end
