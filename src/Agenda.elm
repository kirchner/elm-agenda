module Agenda
    exposing
        ( Agenda
        , describe
        , getDescription
        , run
        , runs
        , succeed
        , try
        , fail
        , map
        , map2
        , (|=)
        , (|.)
        , zeroOrMore
        , FailureHandling
            ( Fail
            , Succeed
            , Retry
            )
        , oneOf
        )

{-|


# Agendas
@docs Agenda, run, runs, describe, getDescription

# Combining Agendas
@docs succeed, fail, try, map, map2, (|=), (|.), zeroOrMore, FailureHandling, oneOf
-}


{-| An `Agenda msg a` can generate `a`'s if fed with the correct `msg`'s.
-}
type Agenda description msg a
    = Agenda (Maybe (Maybe description -> description)) (Result (Step description msg a) (Maybe a))


type Step description msg a
    = Step (msg -> Maybe (Agenda description msg a))


agenda :
    Maybe (Maybe description -> description)
    -> (msg -> Maybe (Agenda description msg a))
    -> Agenda description msg a
agenda describer update =
    Agenda describer <| Err <| Step update


{-| Provide a description for the agenda.  Doing this by providing
`Maybe description -> description`, we get the possibility to let the
new description depend on the previous one.
-}
describe :
    (Maybe description -> description)
    -> Agenda description msg a
    -> Agenda description msg a
describe newDescriber (Agenda maybeDescriber agenda) =
    case maybeDescriber of
        Just describer ->
            Agenda (Just (\l -> newDescriber <| Just (describer l))) agenda

        Nothing ->
            Agenda (Just newDescriber) agenda


{-| Obtain the description of a given agenda.
-}
getDescription : Agenda description msg a -> Maybe description
getDescription (Agenda maybeDescriber agenda) =
    case maybeDescriber of
        Just describer ->
            Just <| describer Nothing

        Nothing ->
            Nothing


{-| Given a `msg` try to run the agenda.  This can either give
another agenda (`Err newAgenda`), the final result (`Ok (Just a)`) or
terminate, when the given `msg` was not suitable (`Ok Nothing`).
-}
run : Agenda description msg a -> msg -> Result (Agenda description msg a) (Maybe a)
run ((Agenda _ agenda) as oldAgenda) msg =
    case agenda of
        Err (Step action) ->
            case action msg of
                Just (Agenda _ (Ok result)) ->
                    Ok result

                Just nextAgenda ->
                    Err nextAgenda

                Nothing ->
                    Ok Nothing

        Ok a ->
            Ok a


{-| Run all `msg`'s in the list.
-}
runs : Agenda description msg a -> List msg -> Result (Agenda description msg a) (Maybe a)
runs ((Agenda _ agenda) as oldAgenda) msgs =
    case msgs of
        [] ->
            Err oldAgenda

        msg :: rest ->
            case run oldAgenda msg of
                Ok result ->
                    Ok result

                Err nextAgenda ->
                    runs nextAgenda rest


{-| An agenda that always generates an `a`.
-}
succeed : a -> Agenda description msg a
succeed a =
    Agenda Nothing <| Ok <| Just a


{-| An Agenda that always fails.
-}
fail : Agenda description msg a
fail =
    Agenda Nothing <| Ok Nothing


{-| An agenda that generates an `a` from the given update function.
-}
try : (msg -> Maybe (Agenda description msg a)) -> Agenda description msg a
try update =
    Agenda Nothing <| Err <| Step update


{-| Transform the result of an agenda.
-}
map : (a -> b) -> Agenda description msg a -> Agenda description msg b
map func (Agenda maybeDescriber agenda) =
    case agenda of
        Err (Step update) ->
            let
                funcUpdate msg =
                    case update msg of
                        Just nextAgenda ->
                            Just (map func nextAgenda)

                        Nothing ->
                            Nothing
            in
                case maybeDescriber of
                    Just describer ->
                        describe describer <|
                            try funcUpdate

                    Nothing ->
                        try funcUpdate

        Ok (Just a) ->
            succeed <| func a

        Ok Nothing ->
            fail


{-| -}
map2 : (a -> b -> c) -> Agenda description msg a -> Agenda description msg b -> Agenda description msg c
map2 func (Agenda maybeDescriber agendaA) agendaB =
    case agendaA of
        Err (Step updateA) ->
            let
                funcUpdate msg =
                    case updateA msg of
                        Just nextAgendaA ->
                            Just (map2 func nextAgendaA agendaB)

                        Nothing ->
                            Nothing
            in
                case maybeDescriber of
                    Just describer ->
                        describe describer <|
                            try funcUpdate

                    Nothing ->
                        try funcUpdate

        Ok (Just a) ->
            map (func a) agendaB

        Ok Nothing ->
            fail


{-| Used to chain agendas together, similarly to **[parser
pipelines][pp]**.  This operator keeps the value.

[pp]: https://github.com/elm-tools/parser/blob/master/README.md#parser-pipeline
-}
(|=) : Agenda description msg (a -> b) -> Agenda description msg a -> Agenda description msg b
(|=) agendaFunc agendaArg =
    map2 apply agendaFunc agendaArg
infixl 5 |=


apply : (a -> b) -> a -> b
apply f a =
    f a


{-| Used to chain agendas together, similarly to **[parser
pipelines][pp]**.  This operator ignores the value.

[pp]: https://github.com/elm-tools/parser/blob/master/README.md#parser-pipeline
-}
(|.) : Agenda description msg keep -> Agenda description msg ignore -> Agenda description msg keep
(|.) agendaKeep agendaIgnore =
    map2 always agendaKeep agendaIgnore
infixl 5 |.


{-| Keep on repeating the given Agenda until the given exit `msg`
is sent, collecting all results.  See the documentation of
FailureHandling for the meaning the second argument.
-}
zeroOrMore : msg -> FailureHandling -> Agenda description msg a -> Agenda description msg (List a)
zeroOrMore exitMsg failureHandling =
    zeroOrMoreIterator exitMsg failureHandling []


{-| If one Agenda fails, do we let the whole Agenda `Fail` (and possibly
loose the previous results), or do we `Succeed` with the list of results
collected so far, or do we return the last agenda so the user can
`Retry`?
-}
type FailureHandling
    = Fail
    | Succeed
    | Retry


zeroOrMoreIterator :
    msg
    -> FailureHandling
    -> List a
    -> Agenda description msg a
    -> Agenda description msg (List a)
zeroOrMoreIterator exitMsg failureHandling list agenda =
    case agenda of
        Agenda (Just describer) _ ->
            describe describer <|
                try <|
                    zeroOrMoreUpdate exitMsg failureHandling list agenda

        _ ->
            try <|
                zeroOrMoreUpdate exitMsg failureHandling list agenda


zeroOrMoreUpdate :
    msg
    -> FailureHandling
    -> List a
    -> Agenda description msg a
    -> msg
    -> Maybe (Agenda description msg (List a))
zeroOrMoreUpdate exitMsg failureHandling list ((Agenda _ agenda) as oldAgenda) msg =
    if exitMsg == msg then
        Just <| succeed list
    else
        case agenda of
            Err (Step update) ->
                case update msg of
                    Just nextAgenda ->
                        case nextAgenda of
                            Agenda _ (Ok (Just result)) ->
                                Just <|
                                    (zeroOrMoreIterator exitMsg failureHandling)
                                        (list ++ [ result ])
                                        oldAgenda

                            Agenda _ (Ok Nothing) ->
                                Nothing

                            _ ->
                                Just <|
                                    (zeroOrMoreIterator exitMsg failureHandling)
                                        list
                                        nextAgenda

                    Nothing ->
                        case failureHandling of
                            Fail ->
                                Just <| fail

                            Succeed ->
                                Just <| succeed list

                            Retry ->
                                Just <|
                                    (zeroOrMoreIterator exitMsg failureHandling)
                                        list
                                        oldAgenda

            Ok (Just result) ->
                Just <|
                    (zeroOrMoreIterator exitMsg failureHandling)
                        (list ++ [ result ])
                        oldAgenda

            Ok Nothing ->
                Nothing


{-| Try all given agendas simultanously.  Succeeds as soon as one of
them succeeds.  Fails if all agendas have failed.  Could be resource
hungry since we do not exclusively switch to the first Agenda which
succeeds after the first `run` iteration.

The standard description of a `oneOf [ a, b, ... ]` is a (possibly
empty) list of all descriptions of the `a`, `b`, ..., i.e.

    getDescription <| oneOf agendas

    ===

    List.filterMap getDescription agendas
-}
oneOf : List (Agenda description msg a) -> Agenda (List description) msg a
oneOf agendas =
    let
        describer =
            always <|
                List.filterMap getDescription agendas
    in
        agenda (Just describer) <| oneOfUpdate agendas


oneOfUpdate : List (Agenda description msg a) -> msg -> Maybe (Agenda (List description) msg a)
oneOfUpdate agendas msg =
    let
        newAgendas =
            agendas |> List.map action

        action : Agenda description msg a -> msg -> Maybe (Agenda description msg a)
        action (Agenda _ agenda) msg =
            case agenda of
                Err (Step update) ->
                    update msg

                Ok (Just result) ->
                    Just (succeed result)

                Ok Nothing ->
                    Nothing

        liveAgendas =
            newAgendas |> List.filterMap ((|>) msg)

        result msg =
            newAgendas |> List.foldl (collect msg) Nothing

        collect : msg -> (msg -> Maybe (Agenda description msg a)) -> Maybe a -> Maybe a
        collect msg nextAction result =
            case result of
                Nothing ->
                    case nextAction msg of
                        Just (Agenda _ (Ok newResult)) ->
                            newResult

                        _ ->
                            Nothing

                _ ->
                    result
    in
        case result msg of
            Just result ->
                Just (succeed result)

            Nothing ->
                if List.isEmpty liveAgendas then
                    Nothing
                else
                    Just <| oneOf liveAgendas
