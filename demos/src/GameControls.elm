module GameControls exposing (main)

{- external -}

import AnimationFrame
import Html exposing (Html)
import Html.Attributes as Html
import Html.Events as Events
import Task
import Time
    exposing
        ( Time
        , millisecond
        )


{- internal -}

import Agenda exposing (..)
import GameAgenda exposing (..)


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
    ( defaultModel
    , Cmd.batch
        [ Task.perform Tick Time.now ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.times Tick ]



{- model -}


type alias Model =
    { currentTime : Time
    , controlTime : Maybe Time
    , combo : GameAgenda
    , successfullCombos : List (List Action)
    }


defaultModel =
    { currentTime = 0 * millisecond
    , controlTime = Nothing
    , combo = allCombos ( 0 * millisecond, Nothing )
    , successfullCombos = []
    }



{- msg -}


type Msg
    = Tick Time
    | Controller Button



{- update -}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick currentTime ->
            { model | currentTime = currentTime } ! []

        Controller button ->
            let
                dt =
                    model.controlTime
                        |> Maybe.map (\t -> model.currentTime - t)
                        |> Maybe.withDefault (0 * millisecond)

                nextCombo =
                    run model.combo (Press button dt)
            in
                case result nextCombo of
                    Just coolDown ->
                        { model
                            | combo = allCombos ( coolDown, Nothing )
                            , controlTime = Just model.currentTime
                            , successfullCombos =
                                (List.map Tuple.first (state nextCombo))
                                    :: model.successfullCombos
                        }
                            ! []

                    Nothing ->
                        if error nextCombo then
                            { model
                                | combo = allCombos ( penalityTime, Nothing )
                                , controlTime = Just model.currentTime
                            }
                                ! []
                        else
                            { model
                                | combo = nextCombo
                                , controlTime = Just model.currentTime
                            }
                                ! []



{- view -}


view : Model -> Html Msg
view model =
    let
        controlButton btn =
            Html.button
                [ Events.onClick (Controller (Tuple.first btn))
                , Html.style
                    [ ( "width", "100px" )
                    ]
                ]
                [ Html.text (Tuple.second btn) ]
    in
        Html.div []
            [ Html.div [] <|
                List.map controlButton
                    [ ( A, "duck" )
                    , ( B, "jump" )
                    , ( C, "punch" )
                    , ( D, "kick" )
                    ]
            , coolDownInfo model
            , Html.h2 []
                [ Html.text "current combo" ]
            , Html.p []
                [ Html.text (toString (state model.combo)) ]
            , Html.h2 []
                [ Html.text "successfull combos" ]
            , viewCombos model.successfullCombos
            ]


viewCombos : List (List Action) -> Html msg
viewCombos combos =
    let
        viewCombo combo =
            Html.li []
                [ Html.text (toString combo) ]
    in
        List.map viewCombo combos |> Html.ul []


coolDownInfo : Model -> Html Msg
coolDownInfo model =
    case model.controlTime of
        Just ctrlTime ->
            let
                dt =
                    model.currentTime - ctrlTime
            in
                case state model.combo |> List.head of
                    Just ( _, ( coolDown, Nothing ) ) ->
                        if dt < coolDown then
                            progressBar "red" (dt / coolDown)
                        else
                            progressBar "green" 1

                    Just ( _, ( coolDown, Just window ) ) ->
                        if dt < coolDown then
                            progressBar "red" (dt / coolDown)
                        else if (dt >= coolDown) && (dt < coolDown + window) then
                            progressBar "green" (1 - (dt - coolDown) / window)
                        else
                            progressBar "red" 0

                    _ ->
                        Html.div [] []

        _ ->
            progressBar "green" 1


progressBar : String -> Float -> Html msg
progressBar color progress =
    Html.div
        [ Html.style
            [ ( "width", "400px" )
            , ( "background-color", "grey" )
            ]
        ]
        [ Html.div
            [ Html.style
                [ ( "width", toString (400 * progress) ++ "px" )
                , ( "height", "30px" )
                , ( "background-color", color )
                ]
            ]
            []
        ]
