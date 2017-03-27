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
    Agenda ( Action, ( Time, Maybe Time ) ) Msg Time


initCombo : GameAgenda
initCombo =
    allCombos ( 0 * millisecond, Nothing )


penalityTime =
    2000 * millisecond


allCombos : ( Time, Maybe Time ) -> GameAgenda
allCombos info =
    tell ( Waiting, info )
        >>> oneOf
                [ punch info
                , lowPunch info
                , kick info
                , highKick info
                ]


punch : ( Time, Maybe Time ) -> GameAgenda
punch ( coolDown, _ ) =
    timedButton C (400 * millisecond) Nothing ( coolDown, Nothing )
        >>= (\info ->
                tell ( Punch Middle, info )
                    >>> succeed (Tuple.first info)
            )


lowPunch : ( Time, Maybe Time ) -> GameAgenda
lowPunch ( coolDown, _ ) =
    timedButton A (2000 * millisecond) (Just (1000 * millisecond)) ( coolDown, Nothing )
        >>= (\info ->
                tell ( Duck, info )
                    >>> timedButton C (1400 * millisecond) Nothing info
                    >>= (\info ->
                            tell ( Punch Low, info )
                                >>> succeed (Tuple.first info)
                        )
            )


kick : ( Time, Maybe Time ) -> GameAgenda
kick ( coolDown, _ ) =
    timedButton D (1000 * millisecond) Nothing ( coolDown, Nothing )
        >>= (\info ->
                tell ( Kick Middle, info )
                    >>> succeed (Tuple.first info)
            )


highKick : ( Time, Maybe Time ) -> GameAgenda
highKick ( coolDown, _ ) =
    timedButton B (1000 * millisecond) (Just (800 * millisecond)) ( coolDown, Nothing )
        >>= (\info ->
                tell ( Jump, info )
                    >>> timedButton D (600 * millisecond) Nothing info
                    >>= (\info ->
                            tell ( Kick High, info )
                                >>> succeed (Tuple.first info)
                        )
            )


timedButton :
    Button
    -> Time
    -> Maybe Time
    -> ( Time, Maybe Time )
    -> Agenda ( Action, ( Time, Maybe Time ) ) Msg ( Time, Maybe Time )
timedButton button nextCoolDown nextWindow ( coolDown, window ) =
    try <|
        \msg ->
            case msg of
                Press actualButton dt ->
                    case window of
                        Just window ->
                            if
                                (actualButton == button)
                                    && (dt >= coolDown)
                                    && (dt <= coolDown + window)
                            then
                                succeed ( nextCoolDown, nextWindow )
                            else
                                fail

                        Nothing ->
                            if
                                (actualButton == button)
                                    && (dt >= coolDown)
                            then
                                succeed ( nextCoolDown, nextWindow )
                            else
                                fail
