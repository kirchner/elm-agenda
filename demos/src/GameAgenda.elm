module GameAgenda
    exposing
        ( GameAgenda
        , Button
            ( A
            , B
            , C
            , D
            )
        , Msg
            ( Press
            , NoOp
            )
        , Info
            ( Do
            , Wait
            )
        , ms
        , allCombos
        )

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
        , oneOf
        )


type alias GameAgenda =
    Agenda Info Msg ( Button, Time )


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
    | NoOp


type Info
    = Do Action
    | Wait Timer


type Action
    = Duck
    | Jump
    | Punch Height
    | Kick Height
    | DoublePunch
    | PunchKick


type Height
    = Low
    | Middle
    | High


type alias Timer =
    { coolDown : Time
    , window : Maybe Time
    }


allCombos : GameAgenda
allCombos =
    init >>= nextCombo


init : GameAgenda
init =
    try <|
        \msg ->
            case msg of
                Press button t ->
                    succeed ( button, t )

                _ ->
                    fail


nextCombo : ( Button, Time ) -> GameAgenda
nextCombo info =
    oneOf
        [ succeed info
            >>= press A
            >>= do Duck
            >>= timer (1000 * ms) (500 * ms)
            >>= \info ->
                    oneOf
                        [ succeed info
                            >>= press C
                            >>= do (Punch Low)
                            >>= wait (1500 * ms)
                        , succeed info
                            >>= press D
                            >>= do (Kick Low)
                            >>= wait (2000 * ms)
                        ]
        , succeed info
            >>= press B
            >>= do Jump
            >>= timer (800 * ms) (400 * ms)
            >>= \info ->
                    oneOf
                        [ succeed info
                            >>= press C
                            >>= do (Punch High)
                            >>= wait (300 * ms)
                        , succeed info
                            >>= press D
                            >>= do (Kick High)
                            >>= wait (400 * ms)
                        ]
        , succeed info
            >>= press C
            >>= do (Punch Middle)
            >>= timer (1000 * ms) (200 * ms)
            >>= \info ->
                    oneOf
                        [ succeed info
                            >>= press C
                            >>= do DoublePunch
                            >>= wait (1500 * ms)
                        , succeed info
                            >>= press D
                            >>= do PunchKick
                            >>= wait (2000 * ms)
                        ]
        , succeed info
            >>= press D
            >>= do (Kick Middle)
            >>= wait (1200 * ms)
        ]
        >>= nextCombo



{- helpers -}


ms : Time
ms =
    millisecond


press : Button -> ( Button, Time ) -> Agenda Info Msg Time
press button ( actualButton, t ) =
    if actualButton == button then
        succeed t
    else
        fail


do : Action -> Time -> Agenda Info Msg Time
do action t =
    tell (Do action)
        >>> succeed t


timer : Time -> Time -> Time -> Agenda Info Msg ( Button, Time )
timer coolDown window t1 =
    tell (Wait { coolDown = coolDown, window = Just window })
        >>> (try <|
                \msg ->
                    case msg of
                        Press button t2 ->
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

                        _ ->
                            fail
            )


wait : Time -> Time -> Agenda Info Msg ( Button, Time )
wait coolDown t1 =
    tell (Wait { coolDown = coolDown, window = Nothing })
        >>> (try <|
                \msg ->
                    case msg of
                        Press button t2 ->
                            let
                                dt =
                                    t2 - t1
                            in
                                if (dt >= coolDown) then
                                    succeed ( button, t2 )
                                else
                                    fail

                        _ ->
                            fail
            )
