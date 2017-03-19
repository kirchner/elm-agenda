module SvgAgenda exposing (..)

{- external -}

import Math.Vector2 exposing (..)


{- internal -}

import Svg exposing (..)
import Agenda
    exposing
        ( Agenda
        , (>>=)
        , (|=)
        , try
        , fail
        , succeed
        )


{-| An `Agenda` which can output intermediate results.
-}
type SvgAgenda msg a
    = SvgAgenda (Bool -> Agenda msg a)


eval : SvgAgenda msg a -> Bool -> Agenda msg a
eval (SvgAgenda oAgenda) =
    oAgenda


run : SvgAgenda msg a -> msg -> SvgAgenda msg a
run (SvgAgenda oAgenda) msg =
    SvgAgenda <|
        \echo -> Agenda.run (oAgenda echo) msg


runs : SvgAgenda msg a -> List msg -> SvgAgenda msg a
runs oAgenda msgs =
    case msgs of
        [] ->
            oAgenda

        msg :: rest ->
            runs (run oAgenda msg) rest


return : Agenda msg Element -> SvgAgenda msg Element
return agenda =
    SvgAgenda <|
        \echo ->
            if echo then
                agenda >>= succeed
            else
                agenda


type Msg
    = NoOp
    | Position Float Float
    | Select Element


pos1 : Msg
pos1 =
    Position 0 0


pos2 : Msg
pos2 =
    Position 10 20


position : Agenda Msg Vec2
position =
    try <|
        \msg ->
            case msg of
                Position x y ->
                    succeed (vec2 x y)

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



{- all actual agendas -}


point : SvgAgenda Msg Element
point =
    return (succeed Svg.point |= position)


rect : SvgAgenda Msg Element
rect =
    SvgAgenda <|
        \echo ->
            position
                >>= echoPosition echo
                        (\p -> succeed (Svg.rect p) |= position)


rect_ : SvgAgenda Msg Element
rect_ =
    SvgAgenda <|
        \echo ->
            position
                >>= (\p ->
                        if echo then
                            succeed (Svg.point p)
                        else
                            succeed (Svg.rect p)
                                |= position
                    )



{- helpers -}


echoPosition :
    Bool
    -> (Vec2 -> Agenda msg Element)
    -> (Vec2 -> Agenda msg Element)
echoPosition =
    echo (Svg.point)


echo :
    (a -> Element)
    -> Bool
    -> (a -> Agenda msg Element)
    -> (a -> Agenda msg Element)
echo func echo callback =
    \a ->
        if echo then
            succeed (func a)
        else
            callback a
