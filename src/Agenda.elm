module Agenda
    exposing
        ( Agenda
        , result
        , run
        , runs
        , try
        , fail
        , succeed
        , (>>=)
        , (>>>)
        , (>=>)
        , map
        , map2
        , (|=)
        , oneOf
        , zeroOrMore
        )

{-|

# Agendas
@docs Agenda, run, runs

# Combining Agendas
@docs try, fail, succeed, (>>=), (>>>), (>=>), map, map2, (|=), oneOf, zeroOrMore
-}


{-| An `Agenda msg a` generates `a` over a sequence of `msg`'s. If fed the
wrong message, it fails.
-}
type Agenda msg a
    = Step (msg -> Agenda msg a)
    | Error
    | Result a


result : Agenda msg a -> Maybe a
result agenda =
    case agenda of
        Result a ->
            Just a

        _ ->
            Nothing



{- evaluation -}


{-| Given a `msg` try to run the agenda.
-}
run : Agenda msg a -> msg -> Agenda msg a
run agenda msg =
    case agenda of
        Step step ->
            step msg

        Error ->
            fail

        Result a ->
            fail


{-| Run all `msg`'s in the list.
-}
runs : Agenda msg a -> List msg -> Agenda msg a
runs agenda msgs =
    case msgs of
        [] ->
            agenda

        msg :: rest ->
            runs (run agenda msg) rest



{- return -}


{-| An agenda that tries to generate an `a` using the provided function.
-}
try : (msg -> Agenda msg a) -> Agenda msg a
try step =
    Step step


{-| An Agenda that always fails.
-}
fail : Agenda msg a
fail =
    Error


{-| An agenda that always succeeds.
-}
succeed : a -> Agenda msg a
succeed a =
    Result a



{- bind -}


{-| The monadic bind operator.  Similar to e.g. `Maybe.andThen`.
-}
(>>=) : Agenda msg a -> (a -> Agenda msg b) -> Agenda msg b
(>>=) arg callback =
    case arg of
        Step step ->
            try <|
                \msg ->
                    case step msg of
                        Step nextStep ->
                            try nextStep >>= callback

                        Error ->
                            fail

                        Result a ->
                            callback a

        Error ->
            fail

        Result a ->
            callback a


{-| Monadic bind, which drops the left result.
-}
(>>>) : Agenda msg ignore -> Agenda msg keep -> Agenda msg keep
(>>>) ignore keep =
    ignore >>= (\_ -> keep)


{-| Monadic composition.
-}
(>=>) : (a -> Agenda msg b) -> (b -> Agenda msg c) -> (a -> Agenda msg c)
(>=>) f g =
    \a -> f a >>= g


{-| Transform the result of an agenda.
-}
map : (a -> b) -> Agenda msg a -> Agenda msg b
map func agenda =
    agenda >>= (\a -> succeed (func a))


{-|
-}
map2 : (a -> b -> c) -> Agenda msg a -> Agenda msg b -> Agenda msg c
map2 func agendaA agendaB =
    agendaA
        >>= (\a ->
                agendaB
                    >>= (\b ->
                            succeed (func a b)
                        )
            )



{- applicative -}


{-| Used to chain agendas together in applicative style, similarly to
**[parser pipelines][pp]**.  This operator keeps the value.

[pp]: https://github.com/elm-tools/parser/blob/master/README.md#parser-pipeline
-}
(|=) : Agenda msg (a -> b) -> Agenda msg a -> Agenda msg b
(|=) func arg =
    map2 apply func arg


apply : (a -> b) -> a -> b
apply f a =
    f a



{- combinators -}


{-| Try all given agendas simultanously.  Succeeds as soon as one of
them succeeds.  Fails if all agendas have failed.  Could be resource
hungry since we do not exclusively switch to the first Agenda which
succeeds after the first `run` iteration.
-}
oneOf : List (Agenda msg a) -> Agenda msg a
oneOf agendas =
    let
        result =
            agendas |> List.foldl collect Nothing

        collect outcome result =
            case result of
                Nothing ->
                    case outcome of
                        Result a ->
                            Just a

                        _ ->
                            Nothing

                _ ->
                    result
    in
        case result of
            Nothing ->
                try <|
                    \msg ->
                        let
                            outcomes =
                                agendas |> List.map (flip run msg)

                            liveAgendas =
                                outcomes |> List.filterMap filter

                            filter outcome =
                                case outcome of
                                    Step step ->
                                        Just (try step)

                                    _ ->
                                        Nothing

                            result =
                                outcomes |> List.foldl collect Nothing
                        in
                            case result of
                                Just a ->
                                    succeed a

                                Nothing ->
                                    if List.isEmpty liveAgendas then
                                        fail
                                    else
                                        oneOf liveAgendas

            Just a ->
                succeed a


{-|
-}
zeroOrMore : msg -> Agenda msg a -> Agenda msg (List a)
zeroOrMore termMsg agenda =
    let
        collect current rest =
            [ current ] ++ rest

        parseTermMsg newAgenda =
            case newAgenda of
                Step step ->
                    Step <|
                        \msg ->
                            if msg == termMsg then
                                succeed []
                            else
                                step msg

                _ ->
                    newAgenda
    in
        parseTermMsg
            (agenda
                >>= (\a ->
                        map
                            (collect a)
                            (zeroOrMore termMsg agenda)
                    )
            )
