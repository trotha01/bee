module Bee exposing (..)

import Html exposing (Html, div, img)
import Html.Attributes exposing (src, height, width, style)
import Html.Events exposing (onClick)
import Animation exposing (Animation, animation, from, to, speed)
import Time exposing (Time)


-- MODEL


type alias Bee =
    { x : Int
    , y : Int
    , animX : Animation
    , animY : Animation
    , src : String
    , audio : Maybe String
    }



-- VIEW


view : Maybe (String -> msg) -> Bee -> Html msg
view clickAction bee =
    let
        attributes =
            [ src bee.src
            , height <| scaleBee beeHeight
            , width <| scaleBee beeWidth
            ]

        attributesWithEvents =
            case ( clickAction, bee.audio ) of
                ( Just click, Just audio ) ->
                    (onClick (click audio)) :: attributes

                _ ->
                    attributes

        wrapper =
            div
                [ style
                    [ -- ( "box-shadow", "3px 3px 1px #ccc" )
                      ( "position", "absolute" )
                    , ( "user-select", "none" )
                    , ( "left", (toString bee.x) ++ "px" )
                    , ( "top", (toString bee.y) ++ "px" )
                    ]
                ]
    in
        wrapper
            [ img attributesWithEvents []
            ]



-- Bees


player =
    { x = 0
    , y = 0
    , animX = animation 0
    , animY = animation 0
    , src = "imgs/bee-left.png"
    , audio = Nothing
    }


mama =
    { x = 100
    , y = 100
    , animX = animation 0
    , animY = animation 0
    , src = "imgs/mama-bee.png"
    , audio = Just "audio/madre.mov"
    }


papa =
    { x = 600
    , y = 100
    , animX = animation 0
    , animY = animation 0
    , src = "imgs/papa-bee.png"
    , audio = Just "audio/padre.m4a"
    }



-- ANIMATION


beeVelocity =
    (0.5)


type alias Position =
    { x : Int
    , y : Int
    }


animate : Time -> Bee -> Bee
animate time bee =
    let
        newX =
            Animation.animate time bee.animX

        newY =
            Animation.animate time bee.animY
    in
        { bee | x = round newX, y = round newY }


animateStart : Time -> Position -> Bee -> Bee
animateStart startTime toPosition bee =
    let
        animX =
            animation startTime
                |> from (toFloat bee.x)
                |> to (toFloat toPosition.x)
                |> speed beeVelocity

        animY =
            animation startTime
                |> from (toFloat bee.y)
                |> to (toFloat toPosition.y)
                |> speed beeVelocity
    in
        { bee
            | animX = animX
            , animY = animY
        }


retarget : Time -> Position -> Bee -> Bee
retarget time toPosition bee =
    let
        animX =
            bee.animX |> Animation.retarget time (toFloat toPosition.x)

        animY =
            bee.animY |> Animation.retarget time (toFloat toPosition.y)
    in
        { bee
            | animX = animX
            , animY = animY
        }



-- HELPERS


( beeHeight, beeWidth ) =
    ( 456, 640 )


scaleBee : Float -> Int
scaleBee dimension =
    dimension / 8 |> round
