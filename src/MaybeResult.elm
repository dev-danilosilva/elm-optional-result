module MaybeResult exposing
    ( MaybeResult
    , error
    , isError
    , isNothing
    , isSuccess
    , map
    , nothing
    , success
    )


type alias Internal error success =
    Maybe (Result error success)


type MaybeResult error success
    = MaybeResult (Internal error success)


map : (a -> c) -> MaybeResult b a -> MaybeResult b c
map fn (MaybeResult mr) =
    wrap <| Maybe.map (Result.map fn) mr


nothing : MaybeResult a b
nothing =
    wrap <| Nothing


success : value -> MaybeResult x value
success value =
    wrap <| Just (Ok value)


error : error -> MaybeResult error x
error value =
    wrap <| Just (Err value)


isError : MaybeResult error success -> Bool
isError (MaybeResult mr) =
    case mr of
        Just res ->
            case res of
                Err _ ->
                    True

                _ ->
                    False

        Nothing ->
            False


isNothing : MaybeResult error success -> Bool
isNothing (MaybeResult mr) =
    case mr of
        Nothing ->
            True

        _ ->
            False


isSuccess : MaybeResult error success -> Bool
isSuccess mr =
    (mr |> (not << isNothing)) && (mr |> (not << isError))


wrap : Internal error success -> MaybeResult error success
wrap internal =
    MaybeResult internal
