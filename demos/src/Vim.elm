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


type alias Error =
    ()


everyTool : Agenda (List Description) Char Action Error
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
            oneOf () [ qTool, wqTool, wallTool ]


{-| test for zeroOrMore
-}



{-
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
-}
{- tools -}


qTool : Agenda Description Char Action Error
qTool =
    cmd <|
        tryChar 'q' Quit


wqTool : Agenda Description Char Action Error
wqTool =
    cmd <|
        (\_ result -> result)
            |~ tryChar 'w' NoOp
            |=++ tryChar 'q' WriteQuit


wallTool : Agenda Description Char Action Error
wallTool =
    cmd <|
        (\_ _ _ result -> result)
            |~ tryChar 'w' NoOp
            |=++ tryChar 'a' NoOp
            |=++ tryChar 'l' NoOp
            |=++ tryChar 'l' WriteAll



{- helpers -}


{-| This is (|=) but also concatenates the descriptions.
-}
(|=++) :
    Agenda Description Char (a -> b) Error
    -> Agenda Description Char a Error
    -> Agenda Description Char b Error
(|=++) agendaFunc agendaArg =
    let
        func maybeA maybeB =
            Maybe.withDefault "" <|
                Maybe.map2 (\a b -> a ++ " and then " ++ b) maybeA maybeB
    in
        (agendaFunc |=* func) agendaArg
infixl 5 |=++


cmd :
    Agenda Description Char Action Error
    -> Agenda Description Char Action Error
cmd agenda =
    (\_ result -> result)
        |~ colon
        |=++ agenda


colon : Agenda Description Char Action Error
colon =
    tryChar ':' NoOp


tryChar : Char -> Action -> Agenda Description Char Action Error
tryChar char action =
    let
        describer =
            always (toString char)
    in
        describe describer <|
            try
                (\c ->
                    if c == char then
                        Ok action
                    else
                        Err ()
                )
