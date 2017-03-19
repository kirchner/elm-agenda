module Svg exposing (..)

import Math.Vector2 exposing (..)


type Element
    = Empty
    | Point PointInfo
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


point : Vec2 -> Element
point p =
    Point
        { cx = getX p
        , cy = getY p
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
