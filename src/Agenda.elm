module Agenda
    exposing
        ( Agenda
        , error
        , result
        , state
        , run
        , runs
        , try
        , fail
        , succeed
        , tell
        , (>>=)
        , andThen
        , (>>>)
        , (>=>)
        , map
        , map2
        , (|=)
        , oneOf
        , (<+>)
        , succeed2
        , succeed3
        , succeed4
        , succeed5
        , lazy
        , zeroOrMore
        , zeroOrMoreWithState
        , addSucceedMsg
        )

{-|

# Agendas
@docs Agenda, run, runs, error, result, succeed, state

# Basic Agendas
@docs try, fail, succeed, tell

# Combining Agendas

## Monadic Interface
@docs (>>=), andThen, (>>>), (>=>)

## Applicative Interface
@docs map, map2, (|=)

## Complex Agendas
@docs oneOf, (<+>), succeed2, succeed3, succeed4, succeed5, lazy, zeroOrMore, addSucceedMsg, zeroOrMoreWithState
-}


{-| An `Agenda msg a` generates `a` over a sequence of `msg`'s. If fed
the wrong message, it fails.  It also collects a stack of states of type `s`.
-}
type Agenda s msg a
    = Step (List s) (msg -> Agenda s msg a)
    | Error
    | Result (List s) a


{-| Return whether we ran into an error.
-}
error : Agenda s msg a -> Bool
error agenda =
    case agenda of
        Error ->
            True

        _ ->
            False


{-| Return the result of an agenda, if possible.
-}
result : Agenda s msg a -> Maybe a
result agenda =
    case agenda of
        Result _ a ->
            Just a

        _ ->
            Nothing


{-| Return the current state stack of an agenda.
-}
state : Agenda s msg a -> List s
state agenda =
    case agenda of
        Step states _ ->
            states

        Error ->
            []

        Result states _ ->
            states


{-| Use this to push a new state to the state stack.  This is meant to
be used in a monadic expression, e.g.

    tell (Just state)
        >>> agenda
-}
tell : s -> Agenda s msg ()
tell state =
    addStates [ state ] (succeed ())


addStates : List s -> Agenda s msg a -> Agenda s msg a
addStates newStates agenda =
    case agenda of
        Step states step ->
            Step (states ++ newStates) step

        Result states a ->
            Result (states ++ newStates) a

        _ ->
            agenda



{- evaluation -}


{-| Given a `msg` try to run the agenda.
-}
run : Agenda s msg a -> msg -> Agenda s msg a
run agenda msg =
    case agenda of
        Step states step ->
            addStates states (step msg)

        Error ->
            fail

        Result states a ->
            addStates states (succeed a)


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
    Step [] step


{-| An agenda that succeeds if the given predicate is satisfied.
-}
satisfy : (msg -> Bool) -> Agenda s msg ()
satisfy pred =
    try
        (\msg ->
            if pred msg then
                succeed ()
            else
                fail
        )


{-| An Agenda that always fails.
-}
fail : Agenda s msg a
fail =
    Error


{-| An agenda that always succeeds.
-}
succeed : a -> Agenda s msg a
succeed a =
    Result [] a



{- bind -}


{-| The monadic bind operator.  Similar to e.g. `Maybe.andThen`.
-}
(>>=) : Agenda s msg a -> (a -> Agenda s msg b) -> Agenda s msg b
(>>=) arg callback =
    case arg of
        Step states step ->
            addStates states <|
                try <|
                    \msg ->
                        step msg >>= callback

        Error ->
            fail

        Result states a ->
            addStates states (callback a)
infixl 1 >>=


{-| Monadic bind operator if you prefer to write `|> andThen`  instead
of `>>=`.  (Note that the choice affects the (implicit) parenthesis.)
-}
andThen : (a -> Agenda s msg b) -> Agenda s msg a -> Agenda s msg b
andThen callback arg =
    arg >>= callback


{-| This can be used to define recursive agendas. TODO: untested
-}
lazy : (() -> Agenda s msg a) -> Agenda s msg a
lazy thunk =
    case thunk () of
        Step states step ->
            addStates states (try (\msg -> step msg))

        Error ->
            fail

        Result states a ->
            addStates states (try (\msg -> succeed a))


{-| Monadic bind, which drops the left result.
-}
(>>>) : Agenda s msg ignore -> Agenda s msg keep -> Agenda s msg keep
(>>>) ignore keep =
    ignore >>= (\_ -> keep)
infixl 1 >>>


{-| Monadic composition.
-}
(>=>) : (a -> Agenda s msg b) -> (b -> Agenda s msg c) -> (a -> Agenda s msg c)
(>=>) f g =
    \a -> f a >>= g
infixl 1 >=>


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
        states =
            agendas
                |> List.map state
                |> List.concat
    in
        addStates states <|
            try <|
                \msg ->
                    let
                        nextAgendas =
                            agendas |> List.filterMap handleOne

                        handleOne agenda =
                            case agenda of
                                Step states step ->
                                    Just (step msg)

                                Error ->
                                    Nothing

                                Result states a ->
                                    Just (succeed a)
                    in
                        case nextAgendas of
                            [] ->
                                fail

                            agenda :: [] ->
                                agenda

                            _ ->
                                oneOf nextAgendas


{-| Try both agendas simultanously.  This succeeds, when both agendas
have succeeded.  Note that this operator is left-associative, i.e.
`Agenda s msg a` is tried first.  You can also chain these together,
like

    chain : Agenda s msg ( ( a, b ), c )
    chain =
        agendaA <+> agendaB <+> agendaC
-}
(<+>) : Agenda s msg a -> Agenda s msg b -> Agenda s msg ( a, b )
(<+>) agendaA agendaB =
    case ( agendaA, agendaB ) of
        ( Error, _ ) ->
            fail

        ( _, Error ) ->
            fail

        ( Result statesA a, _ ) ->
            succeed (\b -> ( a, b ))
                |= addStates statesA agendaB

        ( _, Result statesB b ) ->
            succeed (\a -> ( a, b ))
                |= addStates statesB agendaA

        _ ->
            try <|
                \msg ->
                    let
                        nextA =
                            run agendaA msg

                        nextB =
                            run agendaB msg
                    in
                        case ( nextA, nextB ) of
                            ( Error, Error ) ->
                                fail

                            ( Result statesA a, Result statesB b ) ->
                                succeed ( a, b )
                                    |> addStates statesA
                                    |> addStates statesB

                            ( Error, _ ) ->
                                agendaA <+> nextB

                            ( _, Error ) ->
                                nextA <+> agendaB

                            ( _, _ ) ->
                                nextA <+> nextB
infixl 6 <+>


{-| Helper function, to be used with the `<+>` operator:

    chain : Agenda s msg c
    chain =
        succeed2 f
            |= (agendaA <+> agendaB)


    f : a -> b -> c

Note that you do not need to use the parentheses, since `<+>` binds
tighter then `|=`.
-}
succeed2 : (a -> b -> c) -> Agenda s msg (( a, b ) -> c)
succeed2 f =
    succeed (apply2 f)


{-|
-}
succeed3 : (a -> b -> c -> d) -> Agenda s msg (( ( a, b ), c ) -> d)
succeed3 f =
    succeed (apply3 f)


{-|
-}
succeed4 : (a -> b -> c -> d -> e) -> Agenda s msg (( ( ( a, b ), c ), d ) -> e)
succeed4 f =
    succeed (apply4 f)


{-|
-}
succeed5 : (a -> b -> c -> d -> e -> f) -> Agenda s msg (( ( ( ( a, b ), c ), d ), e ) -> f)
succeed5 f =
    succeed (apply5 f)


apply2 : (a -> b -> c) -> ( a, b ) -> c
apply2 f ( a, b ) =
    (apply f a) b


apply3 : (a -> b -> c -> d) -> ( ( a, b ), c ) -> d
apply3 f ( p, c ) =
    (apply2 f p) c


apply4 : (a -> b -> c -> d -> e) -> ( ( ( a, b ), c ), d ) -> e
apply4 f ( p, d ) =
    (apply3 f p) d


apply5 : (a -> b -> c -> d -> e -> f) -> ( ( ( ( a, b ), c ), d ), e ) -> f
apply5 f ( p, e ) =
    (apply4 f p) e


{-| Repeat the given agenda untill the succeedMsg is sent.
-}
zeroOrMore : msg -> Agenda s msg a -> Agenda s msg (List a)
zeroOrMore succeedMsg agenda =
    addSucceedMsg succeedMsg
        (agenda
            >>= \a ->
                    zeroOrMore succeedMsg agenda
                        >>= \rest ->
                                succeed (a :: rest)
        )


{-| Like `zeroOrMore` but you can also provide a way of adding something
to the state stack.
-}
zeroOrMoreWithState :
    msg
    -> col
    -> (col -> a -> s)
    -> Agenda s msg a
    -> Agenda s msg (List a)
zeroOrMoreWithState succeedMsg col state agenda =
    addSucceedMsg succeedMsg
        (agenda
            >>= \a ->
                    tell (state col a)
                        >>> zeroOrMoreWithState succeedMsg col state agenda
                        >>= \rest ->
                                succeed (a :: rest)
        )


{-| Modify the agenda, so that it succeeds with an empty list if run
with the provided message.  You can use this to implement your own
versions of `zeroOrMore`, to also be able to push something to the state
stack.
-}
addSucceedMsg : msg -> Agenda s msg (List a) -> Agenda s msg (List a)
addSucceedMsg succeedMsg newAgenda =
    case newAgenda of
        Step states step ->
            addStates states <|
                try <|
                    \msg ->
                        if msg == succeedMsg then
                            succeed []
                        else
                            step msg

        _ ->
            newAgenda
