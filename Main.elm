port module Main exposing (..)

import Animation exposing (..)
import Html exposing (Html, div, img, button, text)
import Html.Attributes exposing (src, height, width, style)
import Html.Events exposing (onClick)
import Mouse
import Task
import Time exposing (Time)
import Window
import Bee exposing (Bee)
import Map


-- MODEL


type alias Model =
    { user : Bee
    , mouse : MouseStatus
    , time : Time
    , window : Window.Size
    , level : Map.Level
    , stop : Bool
    }


type MouseStatus
    = Down Mouse.Position
    | Up


init : Model
init =
    { user = Bee.player
    , mouse = Up
    , time = 0
    , window = { width = 100, height = 100 }
    , stop = False
    , level = Map.Home
    }



-- UPDATE


type Msg
    = Move Mouse.Position
    | WindowResize Window.Size
    | MouseDown Mouse.Position
    | MouseUp Mouse.Position
    | NewLevel Map.Level
    | Tick Time
    | PlayAudio String
    | Stop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Stop ->
            ( { model | stop = not model.stop }, Cmd.none )

        WindowResize newSize ->
            ( { model | window = newSize }, Cmd.none )

        PlayAudio file ->
            let
                _ =
                    Debug.log "play audio" file
            in
                ( model, playAudio file )

        MouseUp _ ->
            ( { model | mouse = Up }, Cmd.none )

        MouseDown position ->
            ( { model
                | mouse = Down position
                , user = model.user |> Bee.animateStart model.time position
              }
            , Cmd.none
            )

        Move position ->
            case model.mouse of
                Up ->
                    ( model, Cmd.none )

                Down _ ->
                    ( { model
                        | mouse = Down position
                        , user = model.user |> Bee.retarget model.time position
                      }
                    , Cmd.none
                    )

        Tick time ->
            ( { model
                | user = model.user |> Bee.animate time
                , time = time
              }
            , Cmd.none
            )

        NewLevel newLevel ->
            ( { model | level = newLevel }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "user-select", "none" )
            ]
        ]
        [ -- stopButton
          Map.view NewLevel (Just PlayAudio) model.window model.level
        , Bee.view Nothing model.user
        ]


stopButton =
    button [ onClick Stop ] [ text "stop" ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.stop then
        Sub.none
    else
        case model.mouse of
            Up ->
                Sub.batch
                    [ Mouse.downs MouseDown
                    , Time.every (Time.millisecond * 100) Tick
                    , Window.resizes WindowResize
                    ]

            Down _ ->
                Sub.batch
                    [ Mouse.moves Move
                    , Mouse.ups MouseUp
                    , Time.every (Time.millisecond * 100) Tick
                    , Window.resizes WindowResize
                    ]



-- PORTS


port playAudio : String -> Cmd msg



-- MAIN


main =
    Html.program
        { init = ( init, Task.perform WindowResize Window.size )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
