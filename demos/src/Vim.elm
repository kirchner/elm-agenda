module Vim exposing (..)

import Agenda exposing (..)


{-
   q

   wq

   a

   i
-}


type Action
    = NoOp
    | Quit
    | WriteQuit
    | WriteAll


type alias Description =
    String


everyTool : Agenda (List Description) Char Action
everyTool =
    let
        describer maybeDescriptions =
            case maybeDescriptions of
                Just descriptions ->
                    [ "one of: "
                        ++ (String.join "  or  " descriptions)
                    ]

                Nothing ->
                    []
    in
        describe describer <|
            oneOf [ qTool, wqTool, wallTool ]


{-| test for zeroOrMore
-}
listTool : FailureHandling -> Agenda Description Char (List Action)
listTool failureHandling =
    let
        describer maybeDescription =
            case maybeDescription of
                Just description ->
                    "zero or more of "
                        ++ description
                        ++ " untill '^'"

                Nothing ->
                    ""
    in
        describe describer <|
            zeroOrMore '^' failureHandling (tryChar 't' NoOp)



{- tools -}


qTool : Agenda Description Char Action
qTool =
    cmd <|
        tryChar 'q' Quit


wqTool : Agenda Description Char Action
wqTool =
    cmd <|
        succeed (\_ result -> result)
            |= tryChar 'w' NoOp
            |=++ tryChar 'q' WriteQuit


wallTool : Agenda Description Char Action
wallTool =
    cmd <|
        succeed (\_ _ _ result -> result)
            |= tryChar 'w' NoOp
            |=++ tryChar 'a' NoOp
            |=++ tryChar 'l' NoOp
            |=++ tryChar 'l' WriteAll



{- helpers -}


{-| This is (|=) but also concatenates the descriptions.
-}
(|=++) : Agenda Description Char (a -> b) -> Agenda Description Char a -> Agenda Description Char b
(|=++) agendaFunc agendaArg =
    let
        descriptionArg =
            Maybe.withDefault "" <| getDescription agendaArg

        describer maybeDescription =
            case maybeDescription of
                Just description ->
                    description
                        ++ " and then "
                        ++ descriptionArg

                Nothing ->
                    descriptionArg
    in
        describe describer <|
            (|=) agendaFunc agendaArg
infixl 5 |=++


cmd : Agenda Description Char Action -> Agenda Description Char Action
cmd agenda =
    succeed (\_ result -> result)
        |= colon
        |=++ agenda


colon : Agenda Description Char Action
colon =
    tryChar ':' NoOp


tryChar : Char -> Action -> Agenda Description Char Action
tryChar char action =
    let
        describer =
            always (toString char)
    in
        describe describer <|
            try
                (\c ->
                    if c == char then
                        Just (succeed action)
                    else
                        Nothing
                )
