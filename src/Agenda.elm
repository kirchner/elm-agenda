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
        , (|=*)
        , (|.*)
        , FailureHandling
            ( Failed
            , Succeed
            , Retry
            )
        , oneOf
        )

{-|


# Agendas
@docs Agenda, run, runs, describe, getDescription

# Combining Agendas
@docs succeed, fail, try, map, map2, (|=), (|.), (|=*), (|.*), FailureHandling, oneOf
-}


{-| An `Agenda s msg a` generates `a` over a sequence of `msg`'s. If fed the
wrong message, it fails.
-}
type Agenda s msg a
    = Done (Maybe s) a
    | Fail (Maybe s)
    | Step (Maybe s) (Maybe s -> msg -> Agenda s msg a)


agenda :
    Maybe (Maybe s -> s)
    -> (msg -> Maybe (Agenda s msg a))
    -> Agenda s msg a
agenda describer update =
    Step Nothing (\s msg ->
            Step
              (Maybe.map (\f -> f s) describer)
              (\_ _ -> Maybe.withDefault (Fail Nothing) (update msg)))
               -- TODO: ^^^^ yikes


{-| -}
describe : (Maybe s -> s) -> Agenda s msg a -> Agenda s msg a
describe newDescriber agenda =
    case agenda of
        Done s a ->
            Done (Just (newDescriber s)) a
        Fail s ->
            Fail (Just (newDescriber s))
        Step s cont ->
            Step (Just (newDescriber s)) cont


{-| Obtain the description of a given agenda.
-}
getDescription : Agenda s msg a -> Maybe s
getDescription agenda =
    case agenda of
        Done s _ -> s
        Fail s -> s
        Step s _ -> s


{-| Given a `msg` try to run the agenda.  This can either give
another agenda (`Err newAgenda`), the final result (`Ok (Just a)`) or
terminate, when the given `msg` was not suitable (`Ok Nothing`).
-}
run : Agenda s msg a -> msg -> Result (Agenda s msg a) (Maybe a)
run agenda msg =
    case agenda of
        Done _ a ->
            Ok (Just a)
        Fail _ ->
            Ok Nothing
        Step s cont ->
            Err (cont s msg)


{-| Run all `msg`'s in the list.
-}
runs : Agenda s msg a -> List msg -> Result (Agenda s msg a) (Maybe a)
runs agenda0 msgs =
    case msgs of
        [] ->
            Err agenda0

        msg :: rest ->
            case run agenda0 msg of
                Ok result ->
                    Ok result

                Err agenda ->
                    runs agenda rest


{-| An agenda that always generates an `a`.
-}
succeed : a -> Agenda s msg a
succeed a =
    Done Nothing a


{-| An Agenda that always fails.
-}
fail : Agenda s msg a
fail =
    Fail Nothing


{-| An agenda that generates an `a` from the given update function.
-}
try : (msg -> Maybe (Agenda s msg a)) -> Agenda s msg a
try update =
    Step Nothing (\s msg -> Maybe.withDefault (Fail s) (update msg))


{-| Transform the result of an agenda.
-}
map : (a -> b) -> Agenda s msg a -> Agenda s msg b
map func agenda =
    case agenda of
        Done s a -> Done s (func a)
        Fail s -> Fail s
        Step s cont -> Step s (\s msg -> map func (cont s msg))


{-| -}
map2 : (a -> b -> c) -> Agenda s msg a -> Agenda s msg b -> Agenda s msg c
map2 func agendaA agendaB =
    case agendaA of
        Done s a -> map (func a) agendaB
        Fail s -> Fail s
        Step s cont -> Step s (\s msg -> map2 func (cont s msg) agendaB)


{-| Used to chain agendas together, similarly to **[parser
pipelines][pp]**.  This operator keeps the value.

[pp]: https://github.com/elm-tools/parser/blob/master/README.md#parser-pipeline
-}
(|=) : Agenda s msg (a -> b) -> Agenda s msg a -> Agenda s msg b
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
(|.) : Agenda s msg keep -> Agenda s msg ignore -> Agenda s msg keep
(|.) agendaKeep agendaIgnore =
    map2 always agendaKeep agendaIgnore
infixl 5 |.


{-| Like `(|+)` but we can provide a way of combining the descriptions
of the left and the right hand side.  You should probably use this to
define your own operators for a concrete `description`.  E.g., suppose
`description = String`, then you can define

    (|=++) agendaFunc agendaArg = (agendaFunc |=* func) agendaArg

where

    func maybeA maybeB =
        Maybe.withDefault "" <|
            Maybe.map2 (\a b -> a ++ " and then " ++ b) maybeA maybeB

Note that we cannot remove the parentheses around `agendaFunc |=* func`.
Now if `getDescription foo === Just "foo"` and `getDescription bar ===
Just "bar"`, we have

    getDescription <|
        succeed f
            |= foo
            |=++ bar

    ===

    Just "foo and then bar"
-}
(|=*) :
    Agenda s msg (a -> b)
    -> (Maybe s -> Maybe s -> s)
    -> Agenda s msg a
    -> Agenda s msg b
(|=*) agendaFunc func agendaArg =
    let
        describer maybeDescription =
            func maybeDescription (getDescription agendaArg)
    in
        describe describer <|
            (|=) agendaFunc agendaArg
infixl 5 |=*


{-| Like `(|.)` but we can provide a way of combining the descriptions
of the left and the right hand side.
-}
(|.*) :
    Agenda s msg keep
    -> (Maybe s -> Maybe s -> s)
    -> Agenda s msg ignore
    -> Agenda s msg keep
(|.*) agendaKeep func agendaIgnore =
    let
        describer maybeDescription =
            func maybeDescription (getDescription agendaIgnore)
    in
        describe describer <|
            (|.) agendaKeep agendaIgnore
infixl 5 |.*


{-| If one Agenda fails, do we let the whole Agenda `Fail` (and possibly
loose the previous results), or do we `Succeed` with the list of results
collected so far, or do we return the last agenda so the user can
`Retry`?
-}
type FailureHandling
    = Failed
    | Succeed
    | Retry


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
oneOf : List (Agenda s msg a) -> Agenda (List s) msg a
oneOf agendas =
    let
        describer =
            always <|
                List.filterMap getDescription agendas
    in
        agenda (Just describer) <| oneOfUpdate agendas


oneOfUpdate : List (Agenda s msg a) -> msg -> Maybe (Agenda (List s) msg a)
oneOfUpdate agendas msg =
    let
        newAgendas =
            agendas |> List.map action

        action : Agenda s msg a -> msg -> Maybe (Agenda s msg a)
        action agenda msg =
            case agenda of
              Done s a -> Just (Done s a)
              Fail s -> Nothing
              Step s cont -> Just (cont s msg)

        liveAgendas =
            newAgendas |> List.filterMap ((|>) msg)

        result msg =
            newAgendas |> List.foldl (collect msg) Nothing

        collect : msg -> (msg -> Maybe (Agenda s msg a)) -> Maybe a -> Maybe a
        collect msg nextAction result =
            case result of
                Nothing ->
                    case nextAction msg of
                        Just (Done s a) -> Just a
                        Just (Fail s) -> Nothing
                        Just (Step s cont) -> Nothing
                        Nothing -> Nothing

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
