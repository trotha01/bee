module Sprite exposing (..)

import Html exposing (Html, div, img)
import Html.Attributes exposing (src, style)
import Time exposing (Time)
import Map


-- MODEL


type alias Model =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , dir : Direction
    , orientation : Orientation
    , frame : Int
    }


type Direction
    = Left
    | Right


type Orientation
    = Toward
    | Away


init =
    Model Map.halfWidth Map.halfHeight 0 0 Left Toward 0



-- UPDATE


type Msg
    = Tick Time
    | Direction { x : Int, y : Int }
    | Nothing


update : Msg -> Model -> Model
update msg model =
    case msg of
        Nothing ->
            model

        Direction direction ->
            model
                |> newVelocity direction
                |> setDirection direction

        Tick timeDelta ->
            model
                |> updatePosition timeDelta
                |> updateFrame


newVelocity : { x : Int, y : Int } -> Model -> Model
newVelocity { x, y } model =
    let
        scale =
            0.2

        newVel n =
            if x == 0 || y == 0 then
                scale * toFloat n
            else
                scale * toFloat n / sqrt 2
    in
        { model
            | vx = newVel x
            , vy = newVel y
        }


setDirection : { x : Int, y : Int } -> Model -> Model
setDirection { x, y } model =
    { model
        | orientation =
            if y < 0 then
                Toward
            else if y > 0 then
                Away
            else
                model.orientation
        , dir =
            if x > 0 then
                Right
            else if x < 0 then
                Left
            else
                model.dir
    }


updatePosition : Time -> Model -> Model
updatePosition dt ({ x, y, vx, vy } as model) =
    { model
        | x = clamp 0 (Map.width - 32) (x + dt * vx)
        , y = clamp 32 Map.height (y + dt * vy)
    }


updateFrame : Model -> Model
updateFrame model =
    { model | frame = (model.frame + 1) % 2 }



-- VIEW


view : Model -> Html Msg
view { x, y, vx, vy, dir, orientation, frame } =
    let
        beeImage =
            case ( dir, orientation, frame ) of
                ( Left, Toward, 0 ) ->
                    southWestBee1

                ( Left, Toward, 1 ) ->
                    southWestBee2

                ( Left, Away, 0 ) ->
                    northWestBee1

                ( Left, Away, 1 ) ->
                    northWestBee2

                ( Right, Toward, 0 ) ->
                    southEastBee1

                ( Right, Toward, 1 ) ->
                    southEastBee2

                ( Right, Away, 0 ) ->
                    northEastBee1

                ( Right, Away, 1 ) ->
                    northEastBee2

                ( _, _, _ ) ->
                    southWestBee1
    in
        div
            [ style
                [ ( "width", "32px" )
                , ( "height", "32px" )
                , ( "position", "relative" )
                , ( "top", "-" ++ (toString y) ++ "px" )
                , ( "left", (toString x) ++ "px" )
                ]
            ]
            [ beeImage ]


imgFromSpriteSheet : String -> ( Int, Int ) -> Html Msg
imgFromSpriteSheet spriteSheet ( x, y ) =
    img
        [ src "empty.png"
        , style
            [ ( "width", "32px" )
            , ( "height", "32px" )
            , ( "background-image", "url(" ++ spriteSheet ++ ")" )
            , ( "background-position"
              , "-" ++ (toString x) ++ "px -" ++ (toString y) ++ "px"
              )
            ]
        ]
        []



-- Bee Sprite


beeFromSpriteSheet =
    imgFromSpriteSheet "bee.png"


northWestBee1 =
    beeFromSpriteSheet ( 64, 0 )


northWestBee2 =
    beeFromSpriteSheet ( 0, 32 )


northEastBee1 =
    beeFromSpriteSheet ( 0, 0 )


northEastBee2 =
    beeFromSpriteSheet ( 32, 0 )


southWestBee1 =
    beeFromSpriteSheet ( 32, 32 )


southWestBee2 =
    beeFromSpriteSheet ( 64, 32 )


southEastBee1 =
    beeFromSpriteSheet ( 0, 64 )


southEastBee2 =
    beeFromSpriteSheet ( 32, 64 )
