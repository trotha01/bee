port module Main exposing (..)

import Animation exposing (..)
import AnimationFrame
import Bee exposing (Bee)
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (height, src, style, width)
import Html.Events exposing (onClick)
import Map exposing (Map)
import Mouse
import Task
import Time exposing (Time)
import Window


-- MODEL


type alias Model =
    { user : Bee
    , mouse : MouseStatus
    , time : Time
    , window : Window.Size
    , map : Map
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
    , map = Map.init { width = 100, height = 100 } Map.Home
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
            ( { model
                | window = newSize
                , map = Map.resize newSize model.map
              }
            , Cmd.none
            )

        PlayAudio file ->
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

        Tick timeDelta ->
            ( { model
                | user = model.user |> Bee.animate model.time
                , map = Map.tick timeDelta model.map
                , time = model.time + timeDelta
              }
            , Cmd.none
            )

        NewLevel newLevel ->
            ( { model | map = Map.newLevel newLevel model.map }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "user-select", "none" )
            ]
        ]
        [ -- stopButton
          mapView model.window model.map
        , Bee.view Nothing model.user
        ]


mapView : Window.Size -> Map -> Html Msg
mapView =
    Map.view NewLevel PlayAudio


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

                    -- , Time.every (Time.millisecond * 100) Tick
                    , AnimationFrame.diffs Tick
                    , Window.resizes WindowResize
                    ]

            Down _ ->
                Sub.batch
                    [ Mouse.moves Move
                    , Mouse.ups MouseUp

                    -- , Time.every (Time.millisecond * 100) Tick
                    , AnimationFrame.diffs Tick
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
