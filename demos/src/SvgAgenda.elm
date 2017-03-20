module SvgAgenda exposing (..)

{- external -}

import Math.Vector2 exposing (..)
import Mouse


{- internal -}

import SvgElements exposing (..)
import Agenda
    exposing
        ( Agenda
        , setState
        , (>>=)
        , andThenWithState
        , (|=)
        , try
        , fail
        , succeed
        , map
        , oneOf
        , addSucceedMsg
        )


{- svg agenda -}


type alias SvgAgenda =
    Agenda Element Msg Element


type Msg
    = NoOp
    | Finish
    | Position Vec2
    | Select Element Vec2



{- for debugging -}


pos1 =
    Position (vec2 0 0)


pos2 =
    Position (vec2 10 20)


pos3 =
    Position (vec2 20 20)



{- all actual agendas -}


addPoint : SvgAgenda
addPoint =
    succeed point
        |= position


addCircle : SvgAgenda
addCircle =
    --alternative definition:
    --
    --position >>= (\p ->
    --setState (point p)
    --(succeed (circle p)
    --    |= position))
    let
        p q =
            position |> andThenWithState point q

        q p =
            succeed (circle p)
                |= position
    in
        p q


addRect : SvgAgenda
addRect =
    let
        p q =
            position |> andThenWithState point q

        q p =
            succeed (rect p)
                |= position
    in
        p q


addOpenPath : SvgAgenda
addOpenPath =
    let
        p q rs =
            position |> andThenWithState point (q rs)

        q rs p =
            position |> andThenWithState (\q -> path p q []) (rs p)

        rs p q =
            succeed (\rs -> path p q rs)
                |= openPathIterator p q []
    in
        p q rs


openPathIterator : Vec2 -> Vec2 -> List Vec2 -> Agenda Element Msg (List Vec2)
openPathIterator p q rs =
    addSucceedMsg Finish
        (position
            >>= (\r ->
                    setState
                        (path p q (rs ++ [ r ]))
                        (openPathIterator p q (rs ++ [ r ])
                            >>= (\newRS -> succeed (r :: newRS))
                        )
                )
        )



{- helpers -}


position : Agenda s Msg Vec2
position =
    try <|
        \msg ->
            case msg of
                Position p ->
                    succeed p

                _ ->
                    fail


element : Agenda s Msg Element
element =
    try <|
        \msg ->
            case msg of
                Select element _ ->
                    succeed element

                _ ->
                    fail
