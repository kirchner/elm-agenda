module SvgAgenda exposing (..)

{- external -}

import Math.Vector2 exposing (..)
import Mouse


{- internal -}

import SvgElements exposing (..)
import Agenda
    exposing
        ( Agenda
        , (>>=)
        , (|=)
        , try
        , fail
        , succeed
        , zeroOrMore
        )


{- general -}


type OAgenda msg a
    = OAgenda (Bool -> Agenda msg a)


eval : OAgenda msg a -> Bool -> Agenda msg a
eval (OAgenda oAgenda) =
    oAgenda


run : OAgenda msg a -> msg -> OAgenda msg a
run (OAgenda oAgenda) msg =
    OAgenda <|
        \echo -> Agenda.run (oAgenda echo) msg


runs : OAgenda msg a -> List msg -> OAgenda msg a
runs oAgenda msgs =
    case msgs of
        [] ->
            oAgenda

        msg :: rest ->
            runs (run oAgenda msg) rest



{- svg agenda -}


type alias SvgAgenda =
    OAgenda Msg Element


type Msg
    = NoOp
    | Finish
    | Position Mouse.Position
    | Select Element



{- for debugging -}


pos1 =
    Position { x = 0, y = 0 }


pos2 =
    Position { x = 10, y = 20 }


pos3 =
    Position { x = 20, y = 20 }


return : Agenda Msg Element -> SvgAgenda
return agenda =
    OAgenda <|
        \echo ->
            if echo then
                agenda >>= succeed
            else
                agenda



{- all actual agendas -}


addPoint : SvgAgenda
addPoint =
    return (succeed point |= position)


addCircle : SvgAgenda
addCircle =
    OAgenda <|
        \echo ->
            position
                >>= (\p ->
                        if echo then
                            succeed (point p)
                        else
                            succeed (circle p)
                                |= position
                    )


addRect : SvgAgenda
addRect =
    OAgenda <|
        \echo ->
            position
                >>= (\p ->
                        if echo then
                            succeed (point p)
                        else
                            succeed (rect p)
                                |= position
                    )


addOpenPath : SvgAgenda
addOpenPath =
    OAgenda <|
        \echo ->
            position
                >>= (\p ->
                        if echo then
                            succeed (point p)
                        else
                            position
                                >>= (\q ->
                                        if echo then
                                            succeed (path p q [])
                                        else
                                            succeed (path p q)
                                                |= zeroOrMore Finish position
                                    )
                    )



{- helpers -}


position : Agenda Msg Vec2
position =
    try <|
        \msg ->
            case msg of
                Position pos ->
                    succeed (vec2 (toFloat pos.x) (toFloat pos.y))

                _ ->
                    fail


element : Agenda Msg Element
element =
    try <|
        \msg ->
            case msg of
                Select element ->
                    succeed element

                _ ->
                    fail
