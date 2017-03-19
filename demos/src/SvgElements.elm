module SvgElements exposing (..)

import Math.Vector2 exposing (..)


type Element
    = Point PointInfo
    | Circle CircleInfo
    | Rect RectInfo
    | Path PathInfo


type alias PointInfo =
    { cx : Float
    , cy : Float
    }


type alias CircleInfo =
    { cx : Float
    , cy : Float
    , r : Float
    }


type alias RectInfo =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


type alias PathInfo =
    { d : List PathInstruction
    }


type PathInstruction
    = Moveto Float Float
    | Lineto Float Float
    | Closepath


dToString : List PathInstruction -> String
dToString =
    List.foldl
        (\instruction sum ->
            let
                next =
                    case instruction of
                        Moveto x y ->
                            String.concat
                                [ "M "
                                , toString x
                                , ", "
                                , toString y
                                ]

                        Lineto x y ->
                            String.concat
                                [ "L "
                                , toString x
                                , ", "
                                , toString y
                                ]

                        --QuadraticCurveto cx cy x y ->
                        --    String.concat
                        --        [ "Q "
                        --        , toString cx
                        --        , ", "
                        --        , toString cy
                        --        , ", "
                        --        , toString x
                        --        , ", "
                        --        , toString y
                        --        ]
                        Closepath ->
                            "Z"
            in
                sum ++ " " ++ next
        )
        ""



{- constructors -}


point : Vec2 -> Element
point p =
    Point
        { cx = getX p
        , cy = getY p
        }


circle : Vec2 -> Vec2 -> Element
circle p q =
    let
        ( cx, cy ) =
            toTuple p

        r =
            length (p |> sub q)
    in
        Circle
            { cx = cx
            , cy = cy
            , r = r
            }


rect : Vec2 -> Vec2 -> Element
rect p q =
    let
        ( x1, y1 ) =
            ( getX p, getY p )

        ( x2, y2 ) =
            ( getX q, getY q )

        ( x, width ) =
            if x1 <= x2 then
                ( x1, x2 - x1 )
            else
                ( x2, x1 - x2 )

        ( y, height ) =
            if y1 <= y2 then
                ( y1, y2 - y1 )
            else
                ( y2, y1 - y2 )
    in
        Rect
            { x = x
            , y = y
            , width = width
            , height = height
            }


path : Vec2 -> Vec2 -> List Vec2 -> Element
path p q rest =
    Path
        { d =
            [ Moveto (getX p) (getY p)
            , Lineto (getX q) (getY q)
            ]
        }
