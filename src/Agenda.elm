module Agenda
    exposing
        ( Agenda
        , result
        , state
        , setState
        , run
        , runs
        , try
        , fail
        , succeed
        , (>>=)
        , lazy
        , andThenWithState
        , (>>>)
        , (>=>)
        , map
        , map2
        , (|=)
        , oneOf
        , zeroOrMore
        , addSucceedMsg
        )

{-|

# Agendas
@docs Agenda, run, runs, result, succeed, state, setState

# Basic Agendas
@docs try, fail, succeed

# Combining Agendas

## Monadic Interface
@docs (>>=), andThenWithState, (>>>), (>=>)

## Applicative Interface
@docs map, map2, (|=)

## Complex Agendas
@docs oneOf, lazy, zeroOrMore, addSucceedMsg
-}


{-| An `Agenda msg a` generates `a` over a sequence of `msg`'s. If fed the
wrong message, it fails.
-}
type Agenda s msg a
    = Step (Maybe s) (msg -> Agenda s msg a)
    | Error
    | Result a


{-| Return the result of an agenda, if possible.
-}
result : Agenda s msg a -> Maybe a
result agenda =
    case agenda of
        Result a ->
            Just a

        _ ->
            Nothing


{-| Return the current state of an agenda.
-}
state : Agenda s msg a -> Maybe s
state agenda =
    case agenda of
        Step maybeState _ ->
            maybeState

        _ ->
            Nothing


{-| Update the state of an agenda.
-}
setState : s -> Agenda s msg a -> Agenda s msg a
setState state agenda =
    case agenda of
        Step _ step ->
            Step (Just state) step

        _ ->
            agenda



{- evaluation -}


{-| Given a `msg` try to run the agenda.
-}
run : Agenda s msg a -> msg -> Agenda s msg a
run agenda msg =
    case agenda of
        Step _ step ->
            step msg

        Error ->
            fail

        Result a ->
            fail


{-| Run all `msg`'s in the list.
-}
runs : Agenda s msg a -> List msg -> Agenda s msg a
runs agenda msgs =
    case msgs of
        [] ->
            agenda

        msg :: rest ->
            runs (run agenda msg) rest



{- return -}


{-| An agenda that tries to generate an `a` using the provided function.
-}
try : (msg -> Agenda s msg a) -> Agenda s msg a
try step =
    Step Nothing step


{-| An Agenda that always fails.
-}
fail : Agenda s msg a
fail =
    Error


{-| An agenda that always succeeds.
-}
succeed : a -> Agenda s msg a
succeed a =
    Result a



{- bind -}


{-| The monadic bind operator.  Similar to e.g. `Maybe.andThen`.
-}
(>>=) : Agenda s msg a -> (a -> Agenda s msg b) -> Agenda s msg b
(>>=) arg callback =
    case arg of
        Step maybeState step ->
            try <|
                \msg ->
                    case step msg of
                        Step maybeNextState nextStep ->
                            let
                                updateState =
                                    case maybeNextState of
                                        Just nextState ->
                                            setState nextState

                                        Nothing ->
                                            case maybeState of
                                                Just state ->
                                                    setState state

                                                Nothing ->
                                                    identity
                            in
                                updateState (try nextStep >>= callback)

                        Error ->
                            fail

                        Result a ->
                            callback a

        Error ->
            fail

        Result a ->
            callback a


{-| This can be used to define recursive agendas. TODO: untested
-}
lazy : (() -> Agenda s msg a) -> Agenda s msg a
lazy thunk =
    case thunk () of
        Step maybeState step ->
            try <|
                \msg ->
                    step msg

        Error ->
            fail

        Result a ->
            succeed a


{-| Like `(>>=)` but also applies the given state callback.
-}
andThenWithState :
    (a -> s)
    -> (a -> Agenda s msg b)
    -> Agenda s msg a
    -> Agenda s msg b
andThenWithState stateCallback agendaCallback agendaArg =
    case agendaArg of
        Step _ step ->
            try <|
                \msg ->
                    case step msg of
                        Step _ nextStep ->
                            try nextStep >>= agendaCallback

                        Error ->
                            fail

                        Result a ->
                            setState
                                (stateCallback a)
                                (agendaCallback a)

        Error ->
            fail

        Result a ->
            setState
                (stateCallback a)
                (agendaCallback a)


{-| Monadic bind, which drops the left result.
-}
(>>>) : Agenda s msg ignore -> Agenda s msg keep -> Agenda s msg keep
(>>>) ignore keep =
    ignore >>= (\_ -> keep)


{-| Monadic composition.
-}
(>=>) : (a -> Agenda s msg b) -> (b -> Agenda s msg c) -> (a -> Agenda s msg c)
(>=>) f g =
    \a -> f a >>= g


{-| Transform the result of an agenda.
-}
map : (a -> b) -> Agenda s msg a -> Agenda s msg b
map func agenda =
    agenda >>= (\a -> succeed (func a))


{-|
-}
map2 : (a -> b -> c) -> Agenda s msg a -> Agenda s msg b -> Agenda s msg c
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
(|=) : Agenda s msg (a -> b) -> Agenda s msg a -> Agenda s msg b
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
oneOf : List (Agenda s msg a) -> Agenda s msg a
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
                                    Step _ step ->
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
zeroOrMore : msg -> Agenda s msg a -> Agenda s msg (List a)
zeroOrMore termMsg agenda =
    let
        collect current rest =
            [ current ] ++ rest

        parseTermMsg newAgenda =
            case newAgenda of
                Step _ step ->
                    try <|
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


{-| Modify the agenda, so that it succeeds with an empty list if run
with the provided message.
-}
addSucceedMsg : msg -> Agenda s msg (List a) -> Agenda s msg (List a)
addSucceedMsg succeedMsg newAgenda =
    case newAgenda of
        Step maybeState step ->
            let
                keepState =
                    case maybeState of
                        Just state ->
                            setState state

                        Nothing ->
                            identity
            in
                keepState <|
                    try <|
                        \msg ->
                            if msg == succeedMsg then
                                succeed []
                            else
                                step msg

        _ ->
            newAgenda
