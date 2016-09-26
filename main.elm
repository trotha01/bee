module Main exposing (..)

import Keyboard.Extra as Keyboard
import Time exposing (Time)
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Html.App exposing (program)
import Sprite exposing (..)
import Map
import Task
import Mouse
import Window
import AnimationFrame


-- MODEL


type alias Model =
    { sprite : Sprite.Model
    , map : Map.Model
    , keyboard : Keyboard.Model
    , windowWidth : Int
    , windowHeight : Int
    , mouseDown : Bool
    , mousePos : { x : Float, y : Float }
    }


initialModel : Keyboard.Model -> Model
initialModel keyboard =
    { sprite = Sprite.init
    , map = Map.init
    , keyboard = keyboard
    , windowWidth = Map.width
    , windowHeight = Map.height
    , mouseDown = False
    , mousePos = { x = 0, y = 0 }
    }



-- UPDATE


type Msg
    = Tick Time
    | KeyPress Keyboard.Msg
    | KeyboardCmd Keyboard.Msg
    | MouseDown Mouse.Position
    | MouseUp Mouse.Position
    | MouseMove Mouse.Position
    | WindowResize Window.Size
    | Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick timeDelta ->
            let
                model' =
                    model
                        |> updateMap timeDelta
                        |> updateSprite (Sprite.Tick timeDelta)

                model'' =
                    if model.mouseDown then
                        model
                            |> dirToMouse
                    else
                        model'
            in
                ( model'', Cmd.none )

        WindowResize { width, height } ->
            ( { model | windowWidth = width, windowHeight = height }, Cmd.none )

        KeyPress key ->
            let
                ( keyboard', cmd ) =
                    Keyboard.update key model.keyboard

                direction =
                    Keyboard.arrows keyboard'

                direction' =
                    { x = toFloat direction.x, y = toFloat direction.y }
            in
                ( { model | keyboard = keyboard' }
                    |> updateSprite (Sprite.Direction direction')
                , Cmd.map KeyboardCmd cmd
                )

        KeyboardCmd msg ->
            let
                ( keyboard', cmd ) =
                    Keyboard.update msg model.keyboard
            in
                ( { model | keyboard = keyboard' }, Cmd.map KeyboardCmd cmd )

        MouseUp _ ->
            let
                _ =
                    Debug.log "MOUSE-UP" ""

                direction =
                    { x = 0, y = 0 }

                model' =
                    { model | mouseDown = False }
            in
                ( model' |> updateSprite (Sprite.Direction direction), Cmd.none )

        MouseDown pos ->
            let
                _ =
                    Debug.log "MOUSE-DOWN" ""

                pos' =
                    { x = toFloat pos.x, y = toFloat pos.y }
            in
                ( { model | mouseDown = True, mousePos = pos' }, Cmd.none )

        MouseMove pos ->
            -- if model.mouseDown == False then
            --     ( model, Cmd.none )
            -- else
            let
                model' =
                    { model | mousePos = { x = toFloat pos.x, y = toFloat pos.y } }

                model'' =
                    model' |> dirToMouse

                _ =
                    Debug.log "vel (vx, vy)" ( model''.sprite.vx, model''.sprite.vy )
            in
                ( model'', Cmd.none )

        Nothing ->
            ( model, Cmd.none )


{-| updates sprite direction so it is moving towards the mouse
-- if the mouse is down
-}
dirToMouse : Model -> Model
dirToMouse model =
    let
        --  _ =
        --    Debug.log "window (w,h)" ( model.windowWidth, model.windowHeight )
        _ =
            Debug.log "pos" model.mousePos

        -- convert map coordinates to screen coordingates
        xMap x =
            x + ((toFloat model.windowWidth) / 2 - Map.width / 2)

        yMap y =
            abs <| y - ((toFloat model.windowHeight) / 2 + Map.height / 2)

        ( mapBeeX, mapBeeY ) =
            ( xMap model.sprite.x, yMap model.sprite.y )

        _ =
            Debug.log "mapped bee (x,y)" ( mapBeeX, mapBeeY )

        minDiff diff =
            if (abs diff) < 10 then
                0
            else
                diff

        xDiff =
            minDiff <| model.mousePos.x - mapBeeX

        yDiff =
            minDiff <| mapBeeY - model.mousePos.y

        _ =
            Debug.log "diff (x,y)" ( xDiff, yDiff )

        distance =
            sqrt (xDiff ^ 2 + yDiff ^ 2)

        dirX =
            if distance == 0 then
                0
            else
                xDiff / distance

        dirY =
            if distance == 0 then
                0
            else
                yDiff / distance

        direction =
            { x = dirX, y = dirY }

        _ =
            Debug.log "direction" direction
    in
        model |> updateSprite (Sprite.Direction direction)


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
        ( x, y, vx, vy ) =
            ( model.sprite.x, model.sprite.y, model.sprite.vx, model.sprite.vy )

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
            | map = Map.update action model.map
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
        , Mouse.downs MouseDown
        , Mouse.ups MouseUp
        , Mouse.moves MouseMove
        , Window.resizes windowResize
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
                , Cmd.batch
                    [ Cmd.map KeyPress keyboardCmd
                    , initialWindowSize
                    ]
                )
            , update = update
            , subscriptions = always subs
            , view = view
            }



-- HELPERS


initialWindowSize : Cmd Msg
initialWindowSize =
    Task.perform (\_ -> Nothing) windowResize Window.size


windowResize : { width : Int, height : Int } -> Msg
windowResize size =
    WindowResize { width = size.width, height = size.height }
