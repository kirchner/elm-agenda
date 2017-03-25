module Vim exposing (..)

import Agenda exposing (..)


type Action
    = NoOp
    | Quit
    | WriteQuit
    | WriteAll


type alias Msg =
    Char


type alias VimCmd =
    Agenda () Msg Action


everyCmd : VimCmd
everyCmd =
    oneOf [ q, wq, wall ]



{- tools -}


q : VimCmd
q =
    cmd Quit (tryChar 'q')


wq : VimCmd
wq =
    cmd WriteQuit
        (tryChar 'w'
            >>> tryChar 'q'
        )


wall : VimCmd
wall =
    cmd WriteAll
        (tryChar 'w'
            >>> tryChar 'a'
            >>> tryChar 'l'
            >>> tryChar 'l'
        )



{- helpers -}


cmd :
    Action
    -> Agenda () Char ()
    -> VimCmd
cmd action agenda =
    colon
        >>> agenda
        >>> succeed action


colon : Agenda () Char ()
colon =
    tryChar ':'


tryChar : Char -> Agenda () Char ()
tryChar char =
    try <|
        \c ->
            if c == char then
                succeed ()
            else
                fail
