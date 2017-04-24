port module Main exposing (..)

import Html exposing (Html, div, img, button, text)
import Html.Attributes exposing (src, height, width, style)
import Html.Events exposing (onClick)
import Mouse
import Time exposing (Time)
import Animation exposing (..)
import Bee exposing (Bee)


-- MODEL


type alias Model =
    { user : Bee
    , mouse : MouseStatus
    , time : Time
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
    , stop = False
    }



-- UPDATE


type Msg
    = Move Mouse.Position
    | MouseDown Mouse.Position
    | MouseUp Mouse.Position
    | Tick Time
    | PlayAudio String
    | Stop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Stop ->
            ( { model | stop = not model.stop }, Cmd.none )

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

        Tick time ->
            ( { model
                | user = model.user |> Bee.animate time
                , time = time
              }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "user-select", "none" )
            ]
        ]
        [ -- stopButton
          Bee.view (Just PlayAudio) Bee.mama
        , Bee.view (Just PlayAudio) Bee.papa
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
                    ]

            Down _ ->
                Sub.batch
                    [ Mouse.moves Move
                    , Mouse.ups MouseUp
                    , Time.every (Time.millisecond * 100) Tick
                    ]



-- PORTS


port playAudio : String -> Cmd msg



-- MAIN


main =
    Html.program
        { init = ( init, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
