module SvgEditor exposing (..)

{- external -}

import Dict exposing (Dict)
import Json.Decode as Json
import Mouse
import Html exposing (Html)
import Html.Attributes as Html
import Html.Events as Html
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Svg.Events as Svg
import Math.Vector2 exposing (..)


{- internal -}

import Agenda
    exposing
        ( state
        , run
        )
import SvgAgenda
    exposing
        ( SvgAgenda
        , addPoint
        , addCircle
        , addRect
        , addOpenPath
        )
import SvgElements
    exposing
        ( Element
            ( Point
            , Circle
            , Rect
            , Path
            )
        , PathInstruction
            ( Moveto
            , Lineto
            , Closepath
            )
        , dToString
        )


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
    { elements : Dict Int Element
    , nextId : Int
    , focusId : Maybe Int
    , currentTool : Maybe SvgAgenda
    , currentState : Maybe Element
    }


emptyModel : Model
emptyModel =
    { elements = Dict.empty
    , nextId = 0
    , focusId = Nothing
    , currentTool = Nothing
    , currentState = Nothing
    }



{- events -}


onClickWithCoords : (Vec2 -> msg) -> Svg.Attribute msg
onClickWithCoords tagger =
    Svg.on "click" (Json.map tagger offsetPosition)


offsetPosition : Json.Decoder Vec2
offsetPosition =
    Json.map2 (\x y -> vec2 (toFloat x) (toFloat y))
        (Json.field "offsetX" Json.int)
        (Json.field "offsetY" Json.int)



{- msg -}


type Msg
    = NoOp
    | SetFocus Int
    | UnFocus
    | RunTool SvgAgenda.Msg
    | InitTool SvgAgenda



{- udpate -}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        SetFocus id ->
            if Dict.member id model.elements then
                { model | focusId = Just id } ! []
            else
                model ! []

        UnFocus ->
            { model | focusId = Nothing } ! []

        RunTool svgAgendaMsg ->
            case model.currentTool of
                Just tool ->
                    let
                        nextTool =
                            run tool svgAgendaMsg

                        newState =
                            state nextTool
                    in
                        case Agenda.result nextTool of
                            Just svgElement ->
                                { model
                                    | elements =
                                        Dict.insert
                                            model.nextId
                                            svgElement
                                            model.elements
                                    , nextId = 1 + model.nextId
                                    , currentTool = Nothing
                                    , currentState = Nothing
                                }
                                    ! []

                            Nothing ->
                                { model
                                    | currentTool = Just nextTool
                                    , currentState = List.head newState
                                }
                                    ! []

                _ ->
                    model ! []

        InitTool tool ->
            { model | currentTool = Just tool } ! []



{- view -}


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div []
            [ Html.button
                [ Html.onClick (InitTool addPoint) ]
                [ Html.text "add point" ]
            , Html.button
                [ Html.onClick (InitTool addCircle) ]
                [ Html.text "add circle" ]
            , Html.button
                [ Html.onClick (InitTool addRect) ]
                [ Html.text "add rect" ]
            , Html.button
                [ Html.onClick (InitTool addOpenPath) ]
                [ Html.text "add (open) path" ]
            , Html.button
                [ Html.onClick (RunTool SvgAgenda.Finish) ]
                [ Html.text "finish" ]
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
                    , onClickWithCoords (RunTool << SvgAgenda.Position)
                    ]
                    []
                , drawElements model
                , Svg.g [] <|
                    List.map drawSelectionCircle (Dict.toList model.elements)
                , model.currentState
                    |> Maybe.map (drawElement "red")
                    |> Maybe.withDefault (Svg.g [] [])
                ]
            ]
        ]


drawElements : Model -> Svg Msg
drawElements model =
    let
        color id =
            case model.focusId of
                Just focusId ->
                    if focusId == id then
                        "blue"
                    else
                        "black"

                Nothing ->
                    "black"
    in
        Dict.toList model.elements
            |> List.map
                (\( id, element ) ->
                    drawElement (color id) element
                )
            |> (Svg.g [])


drawElement : String -> Element -> Svg Msg
drawElement color element =
    case element of
        Point info ->
            Svg.circle
                [ cx (toString info.cx)
                , cy (toString info.cy)
                , r "1"
                , fill "none"
                , strokeWidth "1px"
                , stroke color
                ]
                []

        Circle info ->
            Svg.g []
                [ Svg.circle
                    [ cx (toString info.cx)
                    , cy (toString info.cy)
                    , r (toString info.r)
                    , fill "none"
                    , strokeWidth "1px"
                    , stroke color
                    ]
                    []
                , Svg.circle
                    [ cx (toString info.cx)
                    , cy (toString info.cy)
                    , r "1"
                    , fill "none"
                    , strokeWidth "1px"
                    , stroke color
                    ]
                    []
                ]

        Rect info ->
            Svg.rect
                [ x (toString info.x)
                , y (toString info.y)
                , width (toString info.width)
                , height (toString info.height)
                , fill "none"
                , strokeWidth "1px"
                , stroke color
                ]
                []

        Path info ->
            Svg.path
                [ d (dToString info.d)
                , fill "none"
                , strokeWidth "1px"
                , stroke color
                ]
                []


drawSelectionCircle : ( Int, Element ) -> Svg Msg
drawSelectionCircle ( id, element ) =
    let
        selectionCircle x y id element =
            Svg.circle
                [ cx (toString x)
                , cy (toString y)
                , r "8"
                , fill "transparent"
                , Svg.onMouseOver (SetFocus id)
                , Svg.onMouseOut UnFocus
                , Svg.onClick
                    (RunTool (SvgAgenda.Position (vec2 x y)))
                ]
                []
    in
        case element of
            Point info ->
                selectionCircle info.cx info.cy id element

            Circle info ->
                selectionCircle info.cx info.cy id element

            Rect info ->
                [ ( info.x, info.y )
                , ( info.x, info.y + info.height )
                , ( info.x + info.width, info.y )
                , ( info.x + info.width, info.y + info.height )
                ]
                    |> List.map
                        (\( x, y ) ->
                            selectionCircle x y id element
                        )
                    |> (Svg.g [])

            Path info ->
                info.d
                    |> List.map
                        (\instruction ->
                            case instruction of
                                Moveto x y ->
                                    selectionCircle x y id element

                                Lineto x y ->
                                    selectionCircle x y id element

                                Closepath ->
                                    Svg.g [] []
                        )
                    |> (Svg.g [])
