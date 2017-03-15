module SvgEditor exposing (main)

import Agenda exposing (..)
import Json.Decode as Json
import Mouse
import Html exposing (Html)
import Html.Attributes as Html
import Html.Events as Html
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Svg.Events as Svg
import Math.Vector2 exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : ( Model, Cmd Msg )
init =
    ( emptyModel
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



{- model -}


type alias Model =
    { svgElements : List SvgElement
    , currentTool : Maybe (Agenda Description AMsg SvgElement Error)
    }


emptyModel : Model
emptyModel =
    { svgElements = []
    , currentTool = Nothing
    }


type SvgElement
    = Circle CircleInfo
    | Rect RectInfo
    | Path PathInfo


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
    | QuadraticCurveto Float Float Float Float
      --| CubicCurceto
      --| Arcto
    | Closepath


dToString : List PathInstruction -> String
dToString =
    List.foldl
        (\instruction d ->
            let
                string =
                    case instruction of
                        Moveto dx dy ->
                            String.concat
                                [ "m "
                                , toString dx
                                , ", "
                                , toString dy
                                ]

                        Lineto dx dy ->
                            String.concat
                                [ "l "
                                , toString dx
                                , ", "
                                , toString dy
                                ]

                        QuadraticCurveto dcx dcy dx dy ->
                            String.concat
                                [ "q "
                                , toString dcx
                                , ", "
                                , toString dcy
                                , ", "
                                , toString dx
                                , ", "
                                , toString dy
                                ]

                        Closepath ->
                            "z"
            in
                string ++ " "
        )
        ""



{- events -}


onClickWithCoords : (Mouse.Position -> msg) -> Svg.Attribute msg
onClickWithCoords tagger =
    Svg.on "click" (Json.map tagger offsetPosition)


offsetPosition : Json.Decoder Mouse.Position
offsetPosition =
    Json.map2 Mouse.Position
        (Json.field "offsetX" Json.int)
        (Json.field "offsetY" Json.int)



{- msg -}


type Msg
    = NoOp
    | RunTool AMsg
    | InitTool (Agenda Description AMsg SvgElement Error)



{- agenda -}


type AMsg
    = InputPosition Mouse.Position


type alias Description =
    String


type alias Error =
    String


(|=++) :
    Agenda Description msg (a -> b) err
    -> Agenda Description msg a err
    -> Agenda Description msg b err
(|=++) agendaFunc agendaArg =
    let
        func maybeA maybeB =
            Maybe.withDefault "" <|
                Maybe.map2 (\a b -> a ++ " and then " ++ b) maybeA maybeB
    in
        (agendaFunc |=* func) agendaArg
infixl 5 |=++


inputPosition : Agenda Description AMsg Vec2 Error
inputPosition =
    describe (\_ -> "provide a position") <|
        try <|
            \msg ->
                case msg of
                    InputPosition position ->
                        Ok (vec2 (toFloat position.x) (toFloat position.y))


addCircle : Agenda Description AMsg SvgElement Error
addCircle =
    (\v w ->
        Circle
            { cx = getX v
            , cy = getY v
            , r = length (w |> sub v)
            }
    )
        |~ inputPosition
        |=++ inputPosition


addRect : Agenda Description AMsg SvgElement Error
addRect =
    (\v w ->
        let
            ( x1, y1 ) =
                toTuple v

            ( x2, y2 ) =
                toTuple w

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
    )
        |~ inputPosition
        |=++ inputPosition


addOpenPath : Agenda Description AMsg SvgElement Error
addOpenPath =
    (\a b ->
        let
            cs =
                []

            rest =
                List.map (\c -> Lineto (getX c) (getY c)) cs
        in
            Path
                { d =
                    List.concat
                        [ [ Moveto (getX a) (getY a)
                          , Lineto (getX b) (getY b)
                          ]
                        , rest
                        , [ Closepath ]
                        ]
                }
    )
        |~ inputPosition
        |=++ inputPosition



{- udpate -}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        RunTool amsg ->
            case model.currentTool of
                Just tool ->
                    let
                        outcome =
                            run tool amsg
                    in
                        case outcome of
                            Next tool ->
                                { model | currentTool = Just tool } ! []

                            Error err ->
                                model ! []

                            Success svgElement ->
                                { model
                                    | svgElements =
                                        svgElement :: model.svgElements
                                    , currentTool = Nothing
                                }
                                    ! []

                _ ->
                    model ! []

        InitTool tool ->
            { model | currentTool = Just tool } ! []



{- view -}


view : Model -> Html Msg
view model =
    let
        description =
            model.currentTool
                |> Maybe.andThen getDescription
                |> Maybe.withDefault ""
    in
        Html.div []
            [ Html.div []
                [ Html.button
                    [ Html.onClick (InitTool addCircle) ]
                    [ Html.text "add circle" ]
                , Html.button
                    [ Html.onClick (InitTool addRect) ]
                    [ Html.text "add rect" ]
                , Html.button
                    [ Html.onClick (InitTool addOpenPath) ]
                    [ Html.text "add (open) path" ]
                , Html.text description
                ]
            , Html.div []
                [ Svg.svg
                    [ width "600"
                    , height "400"
                    , viewBox "0 0 600 400"
                    , Html.style
                        [ ( "width", "600" )
                        , ( "height", "400" )
                        , ( "background-color", "#fffe0" )
                        ]
                    ]
                    [ Svg.rect
                        [ x "0"
                        , y "0"
                        , width "100%"
                        , height "100%"
                        , opacity "0"
                        , onClickWithCoords (RunTool << InputPosition)
                        ]
                        []
                    , Svg.g [] <|
                        List.map drawSvgElement model.svgElements
                    ]
                ]
            ]


drawSvgElement : SvgElement -> Svg Msg
drawSvgElement svgElement =
    case svgElement of
        Circle info ->
            Svg.circle
                [ cx (toString info.cx)
                , cy (toString info.cy)
                , r (toString info.r)
                , fill "none"
                , strokeWidth "1px"
                , stroke "black"
                ]
                []

        Rect info ->
            Svg.rect
                [ x (toString info.x)
                , y (toString info.y)
                , width (toString info.width)
                , height (toString info.height)
                , fill "none"
                , strokeWidth "1px"
                , stroke "black"
                ]
                []

        Path info ->
            Svg.path
                [ d (dToString info.d)
                , strokeWidth "1px"
                , stroke "black"
                ]
                []
