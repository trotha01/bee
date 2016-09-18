module Main exposing (..)

import Keyboard.Extra as Keyboard
import Time exposing (Time)
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Html.App exposing (program)
import Sprite exposing (..)
import Map
import AnimationFrame


-- MODEL


{-| Model contains the x,y coordinates of the sprite
    as well as the velocity and direction.
-}
type alias Model =
    { sprite : Sprite.Model
    , map : Map.Model
    , keyboard : Keyboard.Model
    }


initialModel : Keyboard.Model -> Model
initialModel keyboard =
    Model Sprite.init Map.init keyboard



-- UPDATE


type Msg
    = Tick Time
    | KeyPress Keyboard.Msg
    | Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick timeDelta ->
            ( model
                |> updateMap timeDelta
                |> updateSprite (Sprite.Tick timeDelta)
            , Cmd.none
            )

        KeyPress key ->
            let
                ( keyboard, _ ) =
                    Keyboard.update key model.keyboard

                direction =
                    Keyboard.arrows keyboard
            in
                ( { model | keyboard = keyboard }
                    |> updateSprite (Sprite.Direction direction)
                , Cmd.none
                )

        Nothing ->
            ( model, Cmd.none )


updateSprite : Sprite.Msg -> Model -> Model
updateSprite msg model =
    let
        sprite' =
            Sprite.update msg model.sprite
    in
        { model | sprite = sprite' }


updateMap : Time -> Model -> Model
updateMap dt model =
    let
        ( x, y, vx, vy, map ) =
            ( model.sprite.x, model.sprite.y, model.sprite.vx, model.sprite.vy, model.map )

        ( bottomWall, leftWall, rightWall, topWall ) =
            ( 0, 0, Map.width, Map.height )

        ( movingUp, movingDown, movingRight, movingLeft ) =
            ( vy > 0, vy < 0, vx > 0, vx < 0 )

        action : Map.Action
        action =
            if x == leftWall && movingLeft then
                Map.HorizontalScroll (round (dt * vx))
            else if (x + 32) == rightWall && movingRight then
                Map.HorizontalScroll (round (dt * vx))
            else if y == topWall && movingUp then
                Map.VerticalScroll (round (dt * vy))
            else if (y - 32) == bottomWall && movingDown then
                Map.VerticalScroll (round (dt * vy))
            else
                Map.HorizontalScroll 0
    in
        { model
            | map = Map.update action map
        }



-- VIEW


view : Model -> Html Msg
view model =
    let
        bee =
            Html.App.map (\_ -> Nothing) (Sprite.view model.sprite)

        -- outer, middle, inner is for vertical & horizontal centering
        outer =
            div
                [ style
                    [ ( "display", "table" )
                    , ( "position", "absolute" )
                    , ( "height", "100%" )
                    , ( "width", "100%" )
                    ]
                ]
                [ middle ]

        middle =
            div
                [ style
                    [ ( "display", "table-cell" )
                    , ( "vertical-align", "middle" )
                    ]
                ]
                [ inner ]

        inner =
            div
                [ style
                    [ ( "margin-left", "auto" )
                    , ( "margin-right", "auto" )
                    , ( "width", (toString Map.width) ++ "px" )
                    ]
                ]
                [ Html.App.map (\_ -> Nothing) (Map.view model.map)
                , bee
                ]
    in
        outer



-- SUBSCRIPTIONS


subs : Sub Msg
subs =
    Sub.batch
        [ Sub.map KeyPress Keyboard.subscriptions
        , AnimationFrame.diffs Tick
        ]



-- MAIN


main : Program Never
main =
    let
        ( initialKeyboard, keyboardCmd ) =
            Keyboard.init
    in
        program
            { init =
                ( (initialModel initialKeyboard)
                , Cmd.map KeyPress keyboardCmd
                )
            , update = update
            , subscriptions = always subs
            , view = view
            }
