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
                                            succeed (point q)
                                        else
                                            succeed (path p q [])
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
