port module Main exposing (..)

import Animation exposing (..)
import AnimationFrame
import Bee exposing (Bee)
import Dictionary.French as French
import Dictionary.Spanish as Spanish
import Dictionary.Translator exposing (Translator)
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
    , pause : Bool
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
    , pause = False
    , map = Map.init Map.Home
    }



-- UPDATE


type Msg
    = Move Mouse.Position
    | WindowResize Window.Size
    | MouseDown Mouse.Position
    | MouseUp Mouse.Position
    | MapMsg Map.Msg
    | Tick Time
    | Pause


update : Translator -> Msg -> Model -> ( Model, Cmd Msg )
update translator msg model =
    case msg of
        Pause ->
            ( { model | pause = not model.pause }, Cmd.none )

        WindowResize newSize ->
            ( { model | window = newSize }, Cmd.none )

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
            { model
                | user = model.user |> Bee.animate model.time
                , time = model.time + timeDelta
            }
                |> updateMap translator (Map.Tick timeDelta)

        MapMsg msg ->
            updateMap translator msg model


updateMap : Translator -> Map.Msg -> Model -> ( Model, Cmd Msg )
updateMap translator mapMsg model =
    let
        ( map, cmd ) =
            Map.update translator model.window mapMsg model.map
    in
    ( { model | map = map }, cmd |> Cmd.map MapMsg )



-- VIEW


view : Translator -> Model -> Html Msg
view translator model =
    div
        [ style
            [ ( "user-select", "none" )
            ]
        ]
        [ pauseButton
        , mapView model.window translator model.map

        -- , Bee.view Nothing model.user
        ]


mapView : Window.Size -> Translator -> Map -> Html Msg
mapView window translator map =
    Map.view window translator map
        |> Html.map MapMsg


pauseButton =
    button [ onClick Pause ] [ text "pause" ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.pause then
        Sub.none
    else
        case model.mouse of
            Up ->
                Sub.batch
                    [ Mouse.downs MouseDown
                    , AnimationFrame.diffs Tick
                    , Window.resizes WindowResize
                    ]

            Down _ ->
                Sub.batch
                    [ Mouse.moves Move
                    , Mouse.ups MouseUp
                    , AnimationFrame.diffs Tick
                    , Window.resizes WindowResize
                    ]



-- MAIN


spanishTranslator =
    { translate = Spanish.translate
    , audio = Spanish.audio
    }


frenchTranslator =
    { translate = French.translate
    , audio = French.audio
    }


translator =
    frenchTranslator


main =
    Html.program
        { init = ( init, Task.perform WindowResize Window.size )
        , view = view translator
        , update = update translator
        , subscriptions = subscriptions
        }
