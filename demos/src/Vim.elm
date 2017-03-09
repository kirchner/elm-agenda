module Vim exposing (..)

import Agenda exposing (..)


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
listTool : Agenda Description Char (List ()) Error
listTool =
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
            zeroOrMore '^' (tryChar 't')


listChainTool : Agenda Description Char (List ()) Error
listChainTool =
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
            zeroOrMore '^' (tryChar 't' |.++ tryChar 'c')



{- tools -}


qTool : Agenda Description Char Action Error
qTool =
    cmd Quit <|
        identity
            |~ tryChar 'q'


wqTool : Agenda Description Char Action Error
wqTool =
    cmd WriteQuit <|
        identity
            |~ tryChar 'w'
            |.++ tryChar 'q'


wallTool : Agenda Description Char Action Error
wallTool =
    cmd WriteAll <|
        identity
            |~ tryChar 'w'
            |.++ tryChar 'a'
            |.++ tryChar 'l'
            |.++ tryChar 'l'



{- helpers -}


{-| This is (|=) but also concatenates the descriptions.
-}
(|.++) :
    Agenda Description Char keep Error
    -> Agenda Description Char ignore Error
    -> Agenda Description Char keep Error
(|.++) agendaKeep agendaIgnore =
    let
        combiner maybeA maybeB =
            Maybe.withDefault "" <|
                Maybe.map2 (\a b -> a ++ " and then " ++ b) maybeA maybeB
    in
        (agendaKeep |.* combiner) agendaIgnore
infixl 5 |.++


cmd :
    Action
    -> Agenda Description Char () Error
    -> Agenda Description Char Action Error
cmd action agenda =
    always action
        |~ colon
        |.++ agenda


colon : Agenda Description Char () Error
colon =
    tryChar ':'


tryChar : Char -> Agenda Description Char () Error
tryChar char =
    let
        describer =
            always (toString char)

        cont c =
            if c == char then
                Ok ()
            else
                Err ()
    in
        describe describer <| try cont
