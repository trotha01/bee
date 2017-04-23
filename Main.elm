port module Main exposing (..)

import Html exposing (Html, div, img)
import Html.Attributes exposing (src, height, width, style)
import Html.Events exposing (onClick)
import Mouse
import Time exposing (Time)
import Animation exposing (..)


-- MODEL


type alias Model =
    { x : Int
    , y : Int
    , animX : Animation
    , animY : Animation
    , mouse : MouseStatus
    , time : Time
    }


type MouseStatus
    = Down Mouse.Position
    | Up


init : Model
init =
    { x = 0
    , y = 0
    , animX = animation 0
    , animY = animation 0
    , mouse = Up
    , time = 0
    }



-- UPDATE


type Msg
    = Move Mouse.Position
    | MouseDown Mouse.Position
    | MouseUp Mouse.Position
    | Tick Time
    | PlayAudio String


beeVelocity =
    (0.5)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlayAudio file ->
            ( model, playAudio file )

        MouseUp _ ->
            ( { model | mouse = Up }, Cmd.none )

        MouseDown position ->
            let
                animX =
                    animation model.time
                        |> from (toFloat model.x)
                        |> to (toFloat position.x)
                        |> speed beeVelocity

                animY =
                    animation model.time
                        |> from (toFloat model.y)
                        |> to (toFloat position.y)
                        |> speed beeVelocity
            in
                ( { model
                    | mouse = Down position
                    , animX = animX
                    , animY = animY
                  }
                , Cmd.none
                )

        Move position ->
            case model.mouse of
                Up ->
                    ( model, Cmd.none )

                Down _ ->
                    let
                        animX =
                            model.animX |> retarget model.time (toFloat position.x)

                        animY =
                            model.animY |> retarget model.time (toFloat position.y)
                    in
                        ( { model
                            | mouse = Down position
                            , animX = animX
                            , animY = animY
                          }
                        , Cmd.none
                        )

        Tick time ->
            let
                newX =
                    animate time model.animX

                newY =
                    animate time model.animY
            in
                ( { model | x = round newX, y = round newY, time = time }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "user-select", "none" )
            ]
        ]
        [ viewMamaBee
        , viewPapaBee
        , viewPlayerBee model
        ]


viewPlayerBee model =
    img
        [ src beeLeft
        , height <| scaleBee beeHeight
        , width <| scaleBee beeWidth
        , style
            [ ( "position", "relative" )
            , ( "left", (toString model.x) ++ "px" )
            , ( "top", (toString model.y) ++ "px" )
            ]
        ]
        []


mamaPosition =
    { x = 100
    , y = 100
    }


papaPosition =
    { x = 500
    , y = 100
    }


viewMamaBee : Html Msg
viewMamaBee =
    div
        [ style
            [ ( "user-select", "none" )
            ]
        , onClick (PlayAudio "audio/madre.mov")
        ]
        [ img
            [ src mamaBee
            , height <| scaleBee beeHeight
            , width <| scaleBee beeWidth
            , style
                [ ( "position", "relative" )
                , ( "left", (toString mamaPosition.x) ++ "px" )
                , ( "top", (toString mamaPosition.y) ++ "px" )
                ]
            ]
            []
        ]


viewPapaBee : Html Msg
viewPapaBee =
    div
        [ style
            [ ( "user-select", "none" )
            ]
        , onClick (PlayAudio "audio/padre.m4a")
        ]
        [ img
            [ src papaBee
            , height <| scaleBee beeHeight
            , width <| scaleBee beeWidth
            , style
                [ ( "position", "relative" )
                , ( "left", (toString papaPosition.x) ++ "px" )
                , ( "top", (toString papaPosition.y) ++ "px" )
                ]
            ]
            []
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.mouse of
        Up ->
            Sub.batch
                [ Mouse.downs MouseDown
                , Time.every (Time.millisecond * 1) Tick
                ]

        Down _ ->
            Sub.batch
                [ Mouse.moves Move
                , Mouse.ups MouseUp
                , Time.every (Time.millisecond * 1) Tick
                ]



-- PORTS


port playAudio : String -> Cmd msg



-- BEE


( beeLeft, beeHeight, beeWidth ) =
    ( "imgs/bee-left.png", 456, 640 )
( mamaBee, papaBee ) =
    ( "imgs/mama-bee.png", "imgs/papa-bee.png" )


scaleBee : Float -> Int
scaleBee dimension =
    dimension / 8 |> round



-- MAIN


main =
    Html.program { init = ( init, Cmd.none ), view = view, update = update, subscriptions = subscriptions }
