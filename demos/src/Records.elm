module Records exposing (..)

{- external -}

import Math.Vector2 exposing (..)


{- internal -}

import Agenda exposing (..)


type alias RecordAgenda =
    Agenda () Msg Point


type Msg
    = Position Vec2
    | Select Int
    | Angle Float
    | Distance Float



{- for debugging -}


pos1 =
    Position (vec2 0 0)


pos2 =
    Position (vec2 1 1)


pos3 =
    Position (vec2 0 2)


sel1 =
    Select 1


sel2 =
    Select 2


ang1 =
    Angle 90


dis1 =
    Distance 200



{- point type -}


type Point
    = Origin OriginInfo
    | ADPoint ADPointInfo
    | DDPoint DDPointInfo


type alias OriginInfo =
    WithPosition {}


initOriginInfo =
    { position = Nothing }


type alias ADPointInfo =
    WithAnchor (WithAngle (WithDistance {}))


initADPointInfo =
    { anchor = Nothing
    , angle = Nothing
    , distance = Nothing
    }


type alias DDPointInfo =
    WithAnchor (WithHorDistance (WithVerDistance {}))


initDDPointInfo =
    { anchor = Nothing
    , horDistance = Nothing
    , verDistance = Nothing
    }



{- agendas -}


origin : Agenda () Msg Point
origin =
    succeed (\f -> Origin (f { position = () }))
        |= position


adPoint : Agenda () Msg Point
adPoint =
    succeed (ADPoint << concat3 initADPointInfo)
        |= (anchor <+> angle <+> distance)


test1 =
    [ [ sel1, ang1, dis1 ]
    , [ sel1, dis1, ang1 ]
    , [ ang1, sel1, dis1 ]
    , [ ang1, dis1, sel1 ]
    , [ dis1, sel1, ang1 ]
    , [ dis1, ang1, sel1 ]
    ]
        |> List.map (runs adPoint)


test2 =
    [ [ sel1, ang1, dis1 ]
    , [ sel1, dis1, ang1 ]
    , [ ang1, sel1, dis1 ]
    , [ ang1, dis1, sel1 ]
    , [ dis1, sel1, ang1 ]
    , [ dis1, ang1, sel1 ]
    ]
        |> List.map (runs adPoint_)


aPoint =
    (anchor <+> angle)


test3 =
    [ [ sel1, ang1 ]
    , [ ang1, sel1 ]
    ]
        |> List.map (runs aPoint)



--ddPoint : Agenda () Msg Point
--ddPoint =
--    succeed (DDPoint << concat3 initDDPointInfo)
--        |= (anchor |> and horDistance |> and verDistance)


concat3 a ( ( f, g ), h ) =
    (f << g << h) a


adPoint_ : Agenda () Msg ADPointInfo
adPoint_ =
    succeed3 adPointInfo
        |= (anchor_ <+> angle_ <+> distance_)


adPointInfo : Int -> Float -> Float -> ADPointInfo
adPointInfo id angle distance =
    { anchor = id
    , angle = angle
    , distance = distance
    }


anchor_ : Agenda () Msg Int
anchor_ =
    try <|
        \msg ->
            case msg of
                Select id ->
                    succeed id

                _ ->
                    fail


angle_ : Agenda () Msg Float
angle_ =
    try <|
        \msg ->
            case msg of
                Angle a ->
                    succeed a

                _ ->
                    fail


distance_ : Agenda () Msg Float
distance_ =
    try <|
        \msg ->
            case msg of
                Distance d ->
                    succeed d

                _ ->
                    fail



{- building blocks -}


type alias WithMaybePosition a =
    { a | position : Maybe Vec2 }


type alias WithPosition a =
    { a | position : Vec2 }


type alias WithMaybeAnchor a =
    { a | anchor : Maybe Int }


type alias WithAnchor a =
    { a | anchor : Int }


type alias WithMaybeAngle a =
    { a | angle : Maybe Float }


type alias WithAngle a =
    { a | angle : Float }


type alias WithMaybeDistance a =
    { a | distance : Maybe Float }


type alias WithDistance a =
    { a | distance : Float }


type alias WithHorDistance a =
    { a | horDistance : Maybe Float }


type alias WithVerDistance a =
    { a | verDistance : Maybe Float }


position : Agenda () Msg ({ a | position : () } -> WithPosition a)
position =
    try <|
        \msg ->
            case msg of
                Position p ->
                    succeed (\withPosition -> { withPosition | position = p })

                _ ->
                    fail


anchor : Agenda () Msg (WithMaybeAnchor a -> WithAnchor a)
anchor =
    try <|
        \msg ->
            case msg of
                Select id ->
                    succeed (\withAnchor -> { withAnchor | anchor = id })

                _ ->
                    fail


angle : Agenda () Msg (WithMaybeAngle a -> WithAngle a)
angle =
    try <|
        \msg ->
            case msg of
                Angle a ->
                    succeed (\withAngle -> { withAngle | angle = a })

                _ ->
                    fail


distance : Agenda () Msg (WithMaybeDistance a -> WithDistance a)
distance =
    try <|
        \msg ->
            case msg of
                Distance d ->
                    succeed (\withDistance -> { withDistance | distance = d })

                _ ->
                    fail


horDistance : Agenda () Msg (WithHorDistance a -> WithHorDistance a)
horDistance =
    try <|
        \msg ->
            case msg of
                Distance d ->
                    succeed (\withHorDistance -> { withHorDistance | horDistance = Just d })

                _ ->
                    fail


verDistance : Agenda () Msg (WithVerDistance a -> WithVerDistance a)
verDistance =
    try <|
        \msg ->
            case msg of
                Distance d ->
                    succeed (\withVerDistance -> { withVerDistance | verDistance = Just d })

                _ ->
                    fail
