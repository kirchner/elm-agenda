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
    , successfullCombos : List (List Info)
    }


defaultModel =
    { currentTime = 0 * millisecond
    , controlTime = Nothing
    , combo = allCombos
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
                nextCombo =
                    run model.combo (Press button model.currentTime)
            in
                case result nextCombo of
                    Just _ ->
                        { model
                            | combo = allCombos
                            , controlTime = Just model.currentTime
                            , successfullCombos =
                                (state nextCombo)
                                    :: model.successfullCombos
                        }
                            ! []

                    Nothing ->
                        if error nextCombo then
                            { model
                                | combo = allCombos
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
            , viewStates (state model.combo)
            , Html.h2 []
                [ Html.text "successfull combos" ]
            , viewCombos model.successfullCombos
            ]


viewStates : List Info -> Html msg
viewStates states =
    let
        viewState state =
            Html.li []
                [ Html.text (toString state) ]
    in
        states
            |> List.map viewState
            |> Html.ul []


viewCombos : List (List Info) -> Html msg
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
                    Just (Wait timer) ->
                        let
                            coolDown =
                                timer.coolDown
                        in
                            case timer.window of
                                Just window ->
                                    if dt < coolDown then
                                        progressBar "red" (dt / coolDown)
                                    else if (dt >= coolDown) && (dt < coolDown + window) then
                                        progressBar "green" (1 - (dt - coolDown) / window)
                                    else
                                        progressBar "red" 0

                                Nothing ->
                                    if dt < coolDown then
                                        progressBar "red" (dt / coolDown)
                                    else
                                        progressBar "green" 1

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
