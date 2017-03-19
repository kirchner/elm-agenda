module SvgEditor exposing (..)

{- external -}

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
import SvgAgenda
    exposing
        ( SvgAgenda
        , eval
        , run
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
    { svgElements : List Element
    , currentTool : Maybe SvgAgenda
    , intermediateElement : Maybe Element
    }


emptyModel : Model
emptyModel =
    { svgElements = []
    , currentTool = Nothing
    , intermediateElement = Nothing
    }



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
    | RunTool SvgAgenda.Msg
    | InitTool SvgAgenda



{- udpate -}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        RunTool svgAgendaMsg ->
            case model.currentTool of
                Just tool ->
                    let
                        nextTool =
                            run tool svgAgendaMsg

                        ( intermediate, final ) =
                            ( eval nextTool True
                            , eval nextTool False
                            )

                        newIntermediateElement =
                            Agenda.result intermediate
                    in
                        case Agenda.result final of
                            Just svgElement ->
                                { model
                                    | svgElements =
                                        svgElement :: model.svgElements
                                    , currentTool = Nothing
                                    , intermediateElement = Nothing
                                }
                                    ! []

                            Nothing ->
                                { model
                                    | currentTool = Just nextTool
                                    , intermediateElement =
                                        newIntermediateElement
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
                , Svg.g [] <|
                    List.map drawSvgElement model.svgElements
                , model.intermediateElement
                    |> Maybe.map drawSvgElement
                    |> Maybe.withDefault (Svg.g [] [])
                ]
            ]
        ]


drawSvgElement : Element -> Svg Msg
drawSvgElement svgElement =
    case svgElement of
        Point info ->
            Svg.circle
                [ cx (toString info.cx)
                , cy (toString info.cy)
                , r "1"
                , fill "none"
                , strokeWidth "1px"
                , stroke "black"
                ]
                []

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
                , fill "none"
                , strokeWidth "1px"
                , stroke "black"
                ]
                []
