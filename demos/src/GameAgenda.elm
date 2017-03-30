module GameAgenda exposing (..)

{- external -}

import Time
    exposing
        ( Time
        , millisecond
        )


{- internal -}

import Agenda
    exposing
        ( Agenda
        , try
        , fail
        , succeed
        , tell
        , (>>=)
        , (>>>)
        , lazy
        , oneOf
        )


type
    Button
    -- ~= duck
    = A
      -- ~= jump
    | B
      -- ~= punch
    | C
      -- ~= kick
    | D


type Msg
    = Press Button Time


type Action
    = Waiting
    | Duck
    | Jump
    | Punch Height
    | Kick Height


type Height
    = Low
    | Middle
    | High


type alias GameAgenda =
    Agenda Action Msg ( Button, Time )


init : Agenda Action Msg ( Button, Time )
init =
    try <|
        \(Press button t) ->
            succeed ( button, t ) >>=
            allCombos


allCombos : ( Button, Time ) -> Agenda Action Msg ( Button, Time )
allCombos init =
    oneOf
        [ punch init
        , lowPunch init
        ]


punch : ( Button, Time ) -> Agenda Action Msg ( Button, Time )
punch init =
    succeed init >>=
    press C >>=
    do (Punch Middle) >>=
    wait (1000 * ms) >>=
    allCombos


lowPunch : ( Button, Time ) -> Agenda Action Msg ( Button, Time )
lowPunch init =
    succeed init >>=
    press A >>=
    do Duck >>=
    timer (1000 * ms) (500 * ms) >>=
    press C >>=
    do (Punch Low) >>=
    wait (1500 * ms) >>=
    allCombos



press : Button -> ( Button, Time ) -> Agenda Action Msg Time
press button ( actualButton, t ) =
    if actualButton == button then
        succeed t
    else
        fail

                    
do : Action -> Time -> Agenda Action Msg Time
do action t =
    tell action >>>
    succeed t


timer : Time -> Time -> Time -> Agenda Action Msg ( Button, Time )
timer coolDown window t1 =
    try <|
        \(Press button t2) ->
            let
                dt =
                    t2 - t1
            in
                if
                    (dt >= coolDown)
                        && (dt <= coolDown + window)
                then
                    succeed ( button, t2 )
                else
                    fail


wait : Time -> Time -> Agenda Action Msg ( Button, Time )
wait coolDown t1 =
    try <|
        \(Press button t2) ->
            let
                dt =
                    t2 - t1
            in
                if (dt >= coolDown) then
                    succeed ( button, t2 )
                else
                    fail


ms = millisecond
