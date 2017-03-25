module SvgAgenda exposing (..)

{- external -}

import Math.Vector2 exposing (..)
import Mouse


{- internal -}

import SvgElements exposing (..)
import Agenda
    exposing
        ( Agenda
        , try
        , fail
        , succeed
        , tell
        , (>>=)
        , (>>>)
        , zeroOrMoreWithState
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
    position
        >>= \p ->
                succeed (point p)


addCircle : SvgAgenda
addCircle =
    position
        >>= \p ->
                tell (point p)
                    >>> position
                    >>= \q ->
                            succeed (circle p q)


addRect : SvgAgenda
addRect =
    position
        >>= \p ->
                tell (point p)
                    >>> position
                    >>= \q ->
                            succeed (rect p q)


addOpenPath : SvgAgenda
addOpenPath =
    {- TODO: we might want to format this like this:

       position >>= \p ->
       tell (point p) >>>
       position >>= \q ->
       tell (path p q []) >>>
       zeroOrMoreWithState
           Finish
           ( p, q, [] )
           (\( p, q, rs ) r -> path p q (rs ++ [ r ]))
           position >>= \rs ->
       succeed (path p q rs)
    -}
    position
        >>= \p ->
                tell (point p)
                    >>> position
                    >>= \q ->
                            tell (path p q [])
                                >>> zeroOrMoreWithState
                                        Finish
                                        ( p, q, [] )
                                        (\( p, q, rs ) r -> path p q (rs ++ [ r ]))
                                        position
                                >>= \rs ->
                                        succeed (path p q rs)



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
