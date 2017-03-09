module Agenda
    exposing
        ( Agenda
        , Outcome
            ( Next
            , Error
            , Success
            )
        , describe
        , getDescription
        , run
        , runs
        , succeed
        , try
        , fail
        , map
        , map2
        , (|~)
        , (|=)
        , (|.)
        , (|=*)
        , (|.*)
        , oneOf
        , zeroOrMore
        )

{-|


# Agendas
@docs Agenda, Outcome, run, runs, describe, getDescription

# Combining Agendas
@docs succeed, fail, try, map, map2, (|~), (|=), (|.), (|=*), (|.*), FailureHandling, oneOf
-}


{-| An `Agenda s msg a` generates `a` over a sequence of `msg`'s. If fed the
wrong message, it fails with `err`.
-}
type Agenda s msg a err
    = Agenda (Maybe s) (Maybe s -> msg -> Outcome s msg a err)


{-| The possible outcome of an Agenda.
-}
type Outcome s msg a err
    = Next (Agenda s msg a err)
    | Error err
    | Success a


{-| Update the description of a given agenda.
-}
describe : (Maybe s -> s) -> Agenda s msg a err -> Agenda s msg a err
describe newDescriber (Agenda s cont) =
    Agenda (Just (newDescriber s)) cont


{-| Obtain the description of a given agenda.
-}
getDescription : Agenda s msg a err -> Maybe s
getDescription (Agenda s _) =
    s


{-| Given a `msg` try to run the agenda.  This can either give
another agenda (`Next newAgenda`), the final result (`Success a`) or
an error, when the given `msg` was not suitable (`Error err`).
-}
run : Agenda s msg a err -> msg -> Outcome s msg a err
run (Agenda s cont) msg =
    cont s msg


{-| Run all `msg`'s in the list.
-}
runs : Agenda s msg a err -> List msg -> Outcome s msg a err
runs agenda0 msgs =
    case msgs of
        [] ->
            Next agenda0

        msg :: rest ->
            case run agenda0 msg of
                Success result ->
                    Success result

                Error err ->
                    Error err

                Next agenda ->
                    runs agenda rest


{-| An agenda that always gives `Success a`.
-}
succeed : a -> Agenda s msg a err
succeed a =
    Agenda Nothing (\_ _ -> Success a)


{-| An Agenda that always fails as `Error err`.
-}
fail : err -> Agenda s msg a err
fail err =
    Agenda Nothing (\_ _ -> Error err)


{-| An agenda that tries to generate an `a` using the provided function.
-}
try : (msg -> Result err a) -> Agenda s msg a err
try update =
    Agenda Nothing
        (\s msg ->
            case update msg of
                Err err ->
                    Error err

                Ok a ->
                    Success a
        )


{-| Transform the result of an agenda.
-}
map : (a -> b) -> Agenda s msg a err -> Agenda s msg b err
map func (Agenda s cont) =
    Agenda s
        (\s msg ->
            case cont s msg of
                Next nextAgenda ->
                    Next <| map func nextAgenda

                Error err ->
                    Error err

                Success a ->
                    Success <| func a
        )


{-| -}
map2 : (a -> b -> c) -> Agenda s msg a err -> Agenda s msg b err -> Agenda s msg c err
map2 func (Agenda sA contA) agendaB =
    Agenda sA
        (\s msg ->
            case contA s msg of
                Next nextAgendaA ->
                    Next <| map2 func nextAgendaA agendaB

                Error err ->
                    Error err

                Success a ->
                    Next <| map (func a) agendaB
        )


{-| An infix synonym of `map`.  You should check out the module
documentation to see how you can use this in an agenda chain.
-}
(|~) : (a -> b) -> Agenda s msg a err -> Agenda s msg b err
(|~) =
    map
infixl 5 |~


{-| Used to chain agendas together, similarly to **[parser
pipelines][pp]**.  This operator keeps the value.

[pp]: https://github.com/elm-tools/parser/blob/master/README.md#parser-pipeline
-}
(|=) : Agenda s msg (a -> b) err -> Agenda s msg a err -> Agenda s msg b err
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
(|.) : Agenda s msg keep err -> Agenda s msg ignore err -> Agenda s msg keep err
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
        f
            |~ foo
            |=++ bar

    ===

    Just "foo and then bar"
-}
(|=*) :
    Agenda s msg (a -> b) err
    -> (Maybe s -> Maybe s -> s)
    -> Agenda s msg a err
    -> Agenda s msg b err
(|=*) agendaFunc func agendaArg =
    let
        describer maybeDescription =
            func maybeDescription (getDescription agendaArg)
    in
        describe describer <|
            (|=) agendaFunc agendaArg
infixl 5 |=*


{-| Like `(|.)` but we can provide a way of combining the descriptions
of the left and the right hand side.  Compare the documentation for
`(|=*)`.
-}
(|.*) :
    Agenda s msg keep err
    -> (Maybe s -> Maybe s -> s)
    -> Agenda s msg ignore err
    -> Agenda s msg keep err
(|.*) agendaKeep func agendaIgnore =
    let
        describer maybeDescription =
            func maybeDescription (getDescription agendaIgnore)
    in
        describe describer <|
            (|.) agendaKeep agendaIgnore
infixl 5 |.*


{-| Recursively concatenate the given agenda.  The agenda is terminated
by the provided `msg`, one then obtains a `List a` consisting of all the
successes of the provided agenda.  Note that this fails if you run it
with the terminating message before the current agenda is completed.
-}
zeroOrMore :
    msg
    -> Agenda s msg a err
    -> Agenda s msg (List a) err
zeroOrMore termMsg agenda =
    {- TODO: this gives:  RangeError: Maximum clal stack size exceeded.
       Maybe there is a way to implement lazy?

          let
              collect current rest =
                  [ current ] ++ rest
          in
              collect
                  |~ agenda
                  |= (zeroOrMore termMsg agenda)
    -}
    zeroOrMoreIterator termMsg agenda []


zeroOrMoreIterator :
    msg
    -> Agenda s msg a err
    -> List a
    -> Agenda s msg (List a) err
zeroOrMoreIterator termMsg ((Agenda s cont) as agenda) collected =
    let
        listCont s msg =
            if msg == termMsg then
                Success collected
            else
                case cont s msg of
                    Next (Agenda sNext contNext) ->
                        let
                            collect current rest =
                                collected ++ current ++ rest

                            nextAgenda =
                                Agenda sNext
                                    (\s msg ->
                                        case contNext s msg of
                                            Next nextNextAgenda ->
                                                Next <|
                                                    map
                                                        (\a -> [ a ])
                                                        nextNextAgenda

                                            Error err ->
                                                Error err

                                            Success a ->
                                                Success [ a ]
                                    )
                        in
                            Next <|
                                (collect
                                    |~ nextAgenda
                                    |= zeroOrMore
                                        termMsg
                                        agenda
                                )

                    Error err ->
                        Error err

                    Success a ->
                        Next <|
                            zeroOrMoreIterator
                                termMsg
                                agenda
                                (collected ++ [ a ])
    in
        Agenda s listCont


{-| Try all given agendas simultanously.  Succeeds as soon as one of
them succeeds.  Fails with the provided `err` if all agendas have
failed.  Could be resource hungry since we do not exclusively switch to
the first Agenda which succeeds after the first `run` iteration.

The standard description of a `oneOf [ a, b, ... ]` is a (possibly
empty) list of all descriptions of the `a`, `b`, ..., i.e.

     getDescription <| oneOf agendas

     ===

     List.filterMap getDescription agendas
-}
oneOf : err -> List (Agenda s msg a err) -> Agenda (List s) msg a err
oneOf err agendas =
    let
        descriptions =
            List.filterMap getDescription agendas
    in
        Agenda (Just descriptions) <| (\s msg -> oneOfUpdate err agendas msg)


oneOfUpdate :
    err
    -> List (Agenda s msg a err)
    -> msg
    -> Outcome (List s) msg a err
oneOfUpdate err agendas msg =
    let
        outcomes =
            agendas |> List.map (flip run msg)

        liveAgendas =
            outcomes |> List.filterMap filter

        filter outcome =
            case outcome of
                Next nextAgenda ->
                    Just nextAgenda

                Error _ ->
                    Nothing

                Success _ ->
                    Nothing

        result =
            outcomes |> List.foldl collect Nothing

        collect outcome result =
            case result of
                Nothing ->
                    case outcome of
                        Next nextAgenda ->
                            Nothing

                        Error _ ->
                            Nothing

                        Success result ->
                            Just result

                _ ->
                    result
    in
        case result of
            Just result ->
                Success result

            Nothing ->
                if List.isEmpty liveAgendas then
                    Error err
                else
                    Next <| oneOf err liveAgendas
