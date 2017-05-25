module Map exposing (..)

import Bee exposing (Bee)
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick)
import Math.Vector2 as Vec2 exposing (Vec2, getX, getY, vec2)
import Time exposing (Time)
import Window


-- MODEL


type alias Map =
    { level : Level
    , artGame : ArtGame
    , window : Window.Size
    }


init : Window.Size -> Level -> Map
init window level =
    { level = level
    , artGame = { balls = [] }
    , window = window
    }


type Level
    = Home
    | HomeTown
    | GroceryStore
    | ArtStore PlayGame


type alias PlayGame =
    Bool


type alias ArtGame =
    { balls : List MovingBall }


type alias MovingBall =
    { color : String
    , pos : Vec2
    , radius : Float
    , velocity : Vec2

    {-
       , xVelocity : Float
       , yVelocity : Float
    -}
    }


type alias NewLevel msg =
    Level -> msg


type alias PlayAudio msg =
    String -> msg


newLevel : Level -> Map -> Map
newLevel level map =
    case level of
        ArtStore True ->
            { map | artGame = initArtGame, level = level }

        _ ->
            { map | level = level }


initArtGame : ArtGame
initArtGame =
    { balls =
        [ initMovingBall ( 0, 0 ) ( 1, 1 ) "blue"
        , initMovingBall ( 100, 70 ) ( -1, 1 ) "green"
        , initMovingBall ( 70, 170 ) ( -1, -1 ) "yellow"
        , initMovingBall ( 200, 170 ) ( 1, -1 ) "red"
        ]
    }


initMovingBall : ( Float, Float ) -> ( Float, Float ) -> String -> MovingBall
initMovingBall ( x, y ) ( vx, vy ) color =
    { color = color
    , radius = 32
    , pos = vec2 x y
    , velocity = vec2 vx vy
    }



-- UPDATE


resize : Window.Size -> Map -> Map
resize window map =
    { map | window = window }


tick : Time -> Map -> Map
tick timeDelta map =
    { map
        | artGame = animateArtGame timeDelta map.window map.artGame
    }


animateArtGame : Time -> Window.Size -> ArtGame -> ArtGame
animateArtGame timeDiff window artGame =
    { artGame
        | balls =
            artGame.balls
                |> List.map (animateBall timeDiff window)
                |> collisions
    }


collisions : List MovingBall -> List MovingBall
collisions balls =
    collide [] balls


collide : List MovingBall -> List MovingBall -> List MovingBall
collide acc bodies =
    case bodies of
        [] ->
            acc

        h :: t ->
            case collideWith h t [] of
                [] ->
                    []

                h1 :: t1 ->
                    collide (h1 :: acc) t1


collideWith : MovingBall -> List MovingBall -> List MovingBall -> List MovingBall
collideWith a0 bodies acc =
    case bodies of
        [] ->
            a0 :: acc

        b0 :: bs ->
            let
                collisionResult =
                    collision a0 b0

                ( a1, b1 ) =
                    resolveCollision collisionResult a0 b0
            in
            collideWith a1 bs (b1 :: acc)



-- figure out what collision resolution to use


collision : MovingBall -> MovingBall -> CollisionResult
collision body0 body1 =
    let
        b0b1 =
            Vec2.sub body1.pos body0.pos
    in
    collisionBubbleBubble b0b1 body0.radius body1.radius



-- calculate collision normal, penetration depth of a collision among bubbles
-- takes distance vector b0b1 and the bubble radii as argument


collisionBubbleBubble : Vec2 -> Float -> Float -> CollisionResult
collisionBubbleBubble b0b1 radius0 radius1 =
    let
        radiusb0b1 =
            radius0 + radius1

        distanceSq =
            Vec2.lengthSquared b0b1

        -- simple optimization: doesn't compute sqrt unless necessary
    in
    if distanceSq == 0 then
        CollisionResult (vec2 1 0) radius0
        -- same position, arbitrary normal
    else if distanceSq >= radiusb0b1 * radiusb0b1 then
        CollisionResult (vec2 1 0) 0
        -- no intersection, arbitrary normal
    else
        let
            d =
                sqrt distanceSq
        in
        CollisionResult (Vec2.scale (1 / d) b0b1) (radiusb0b1 - d)


resolveCollision : CollisionResult -> MovingBall -> MovingBall -> ( MovingBall, MovingBall )
resolveCollision { normal, penetration } b0 b1 =
    let
        relativeVelocity =
            Vec2.sub b1.velocity b0.velocity

        velocityAlongNormal =
            Vec2.dot relativeVelocity normal
    in
    if penetration == 0 || velocityAlongNormal > 0 then
        ( b0, b1 )
        -- no collision or velocities separating
    else
        let
            -- impulse scalar
            impulse =
                normal

            -- impulse vector
        in
        ( { b0 | velocity = Vec2.sub b0.velocity impulse }
        , { b1 | velocity = Vec2.add b1.velocity impulse }
        )


initLeftWall windowHeight =
    { pos = vec2 0 <| windowHeight / 2
    , halfHeight = windowHeight / 2
    , halfWidth = 1
    }


initRightWall windowWidth windowHeight =
    { pos = vec2 windowWidth (windowHeight / 2)
    , halfHeight = windowHeight / 2
    , halfWidth = 1
    }


initTopWall windowWidth =
    { pos = vec2 (windowWidth / 2) 0
    , halfHeight = 1
    , halfWidth = windowWidth / 2
    }


initBottomWall windowWidth windowHeight =
    { pos = vec2 (windowWidth / 2) windowHeight
    , halfHeight = 1
    , halfWidth = windowWidth / 2
    }


animateBall : Time -> Window.Size -> MovingBall -> MovingBall
animateBall timeDiff window ball =
    let
        ( width, height ) =
            ( toFloat window.width, toFloat window.height )

        ( leftWall, rightWall, topWall, bottomWall ) =
            ( initLeftWall height
            , initRightWall width height
            , initTopWall width
            , initBottomWall width height
            )

        leftCol =
            collisionBoxBubble
                ( leftWall.pos, vec2 leftWall.halfWidth leftWall.halfHeight )
                ( ball.pos, ball.radius )

        x =
            getX ball.pos

        y =
            getY ball.pos

        newX =
            (x + getX ball.velocity * timeDiff)
                |> clamp (getX leftWall.pos) (getX rightWall.pos)

        newY =
            (y + getY ball.velocity * timeDiff)
                |> clamp (getY topWall.pos) (getY bottomWall.pos)

        newXVelocity =
            if newX == getX leftWall.pos || newX == getX rightWall.pos then
                -(getX ball.velocity)
            else
                getX ball.velocity

        newYVelocity =
            if newY == getY topWall.pos || newY == getY bottomWall.pos then
                -(getY ball.velocity)
            else
                getY ball.velocity
    in
    { ball
        | pos = vec2 newX newY
        , velocity = vec2 newXVelocity newYVelocity
    }


type alias CollisionResult =
    { normal : Vec2, penetration : Float }


{-| collide a box with a bubble

  - takes position and half-length of box, position and radius of bubble

-}
collisionBoxBubble : ( Vec2, Vec2 ) -> ( Vec2, Float ) -> CollisionResult
collisionBoxBubble ( posBox, boxExtents ) ( posBubble, bubbleRadius ) =
    let
        dist =
            Vec2.sub posBubble posBox

        ( dx, dy ) =
            ( Vec2.getX dist, Vec2.getY dist )

        ( boxX, boxY ) =
            ( Vec2.getX boxExtents, Vec2.getY boxExtents )

        c =
            vec2 (clamp -boxX boxX dx) (clamp -boxY boxY dy)

        -- closest point on box to center of bubble
        ( cx, cy ) =
            ( Vec2.getX c, Vec2.getY c )

        ( closest, inside ) =
            if
                --circle is outside
                dist /= c
            then
                ( c, False )
            else if
                -- circle is inside
                abs dx > abs dy
            then
                -- clamp center to closest edge
                if cx > 0 then
                    ( vec2 boxX cy, True )
                else
                    ( vec2 -boxX cy, True )
            else if cy > 0 then
                ( vec2 cx boxY, True )
            else
                ( vec2 cx -boxY, True )

        normal =
            Vec2.sub dist closest

        normalLenSq =
            Vec2.lengthSquared normal
    in
    if normalLenSq > bubbleRadius * bubbleRadius && not inside then
        CollisionResult (vec2 1 0) 0
    else
        let
            penetration =
                bubbleRadius + sqrt normalLenSq
        in
        if inside then
            CollisionResult (Vec2.scale -1 (Vec2.normalize normal)) penetration
        else
            CollisionResult (Vec2.normalize normal) penetration



-- VIEW


view : NewLevel msg -> PlayAudio msg -> Window.Size -> Map -> Html msg
view newLevelMsg playAudioMsg mapSize map =
    case map.level of
        Home ->
            home newLevelMsg playAudioMsg mapSize

        HomeTown ->
            hometown newLevelMsg playAudioMsg mapSize

        GroceryStore ->
            groceryStore newLevelMsg playAudioMsg mapSize

        ArtStore play ->
            artStore play newLevelMsg playAudioMsg map



-- HOME


home : NewLevel msg -> PlayAudio msg -> Window.Size -> Html msg
home newLevelMsg playAudioMsg mapSize =
    div []
        [ exit newLevelMsg HomeTown
        , Bee.view (Just playAudioMsg) Bee.mama
        , Bee.view (Just playAudioMsg) Bee.papa
        ]



-- TOWN


hometown : NewLevel msg -> PlayAudio msg -> Window.Size -> Html msg
hometown newLevelMsg playAudioMsg mapSize =
    div []
        [ house ( 0, 0 ) newLevelMsg
        , storeBuilding ( 160, 0 ) newLevelMsg
        , artStoreBuilding ( 320, 0 ) newLevelMsg
        ]


house : ( Int, Int ) -> NewLevel msg -> Html msg
house ( x, y ) newLevelMsg =
    img
        [ src "imgs/home.png"
        , onClick (newLevelMsg Home)
        , style
            [ ( "position", "absolute" )
            , ( "height", "128px" )
            , ( "width", "128px" )
            , ( "left", toString x ++ "px" )
            , ( "top", toString y ++ "px" )
            ]
        ]
        []


storeBuilding : ( Int, Int ) -> NewLevel msg -> Html msg
storeBuilding ( x, y ) newLevelMsg =
    div []
        [ img
            [ src "imgs/store.png"
            , onClick (newLevelMsg GroceryStore)
            , style
                [ ( "position", "absolute" )
                , ( "height", "128px" )
                , ( "width", "128px" )
                , ( "left", toString x ++ "px" )
                , ( "top", toString y ++ "px" )
                ]
            ]
            []
        , div
            [ style
                [ ( "position", "absolute" )
                , ( "height", "27px" )
                , ( "width", "112px" )
                , ( "text-align", "center" )
                , ( "border", "1px solid black" )
                , ( "background-color", "white" )
                , ( "left", toString (x + 7) ++ "px" )
                , ( "top", toString (y + 10) ++ "px" )
                ]
            ]
            [ text "Los Comestibles" ]
        ]


artStoreBuilding : ( Int, Int ) -> NewLevel msg -> Html msg
artStoreBuilding ( x, y ) newLevelMsg =
    div []
        [ img
            [ src "imgs/store.png"
            , onClick (newLevelMsg (ArtStore False))
            , style
                [ ( "position", "absolute" )
                , ( "height", "128px" )
                , ( "width", "128px" )
                , ( "left", toString x ++ "px" )
                , ( "top", toString y ++ "px" )
                ]
            ]
            []
        , div
            [ style
                [ ( "position", "absolute" )
                , ( "height", "27px" )
                , ( "width", "112px" )
                , ( "text-align", "center" )
                , ( "border", "1px solid black" )
                , ( "background-color", "white" )
                , ( "left", toString (x + 7) ++ "px" )
                , ( "top", toString (y + 10) ++ "px" )
                ]
            ]
            [ text "El Arte" ]
        ]



-- GROCERY STORE


groceryStore : NewLevel msg -> PlayAudio msg -> Window.Size -> Html msg
groceryStore newLevelMsg playAudioMsg mapSize =
    div []
        [ exit newLevelMsg HomeTown
        , playButton ( 192, 10 ) newLevelMsg GroceryStore
        , groceryItem playAudioMsg ( 64, 96 ) "imgs/banana.png" "audio/el_platano.mp3"
        , groceryItem playAudioMsg ( 192, 96 ) "imgs/milk.png" "audio/leche.mp3"
        ]


groceryItem : PlayAudio msg -> ( Int, Int ) -> String -> String -> Html msg
groceryItem playAudioMsg ( x, y ) image audio =
    img
        [ src image
        , style
            [ ( "width", "64px" )
            , ( "height", "64px" )
            , ( "position", "absolute" )
            , ( "left", toString x ++ "px" )
            , ( "top", toString y ++ "px" )
            ]
        , onClick (playAudioMsg audio)
        ]
        []



-- ART STORE


artStore : PlayGame -> NewLevel msg -> PlayAudio msg -> Map -> Html msg
artStore play newLevelMsg playAudioMsg map =
    case play of
        True ->
            colorGame newLevelMsg playAudioMsg map.artGame

        False ->
            let
                showCircle n ( color, audio ) =
                    colorCircle playAudioMsg ( 96 * n, 96 ) color audio
            in
            div [] <|
                exit newLevelMsg HomeTown
                    :: playButton ( 192, 10 ) newLevelMsg (ArtStore True)
                    :: List.indexedMap showCircle
                        [ ( "black", "audio/negro.m4a" )
                        , ( "white", "audio/blanco.m4a" )
                        , ( "red", "audio/rojo.m4a" )
                        , ( "blue", "audio/azul.m4a" )
                        , ( "yellow", "audio/amarillo.m4a" )
                        ]


colorCircle : PlayAudio msg -> ( Int, Int ) -> String -> String -> Html msg
colorCircle playAudioMsg ( x, y ) color audio =
    div
        [ style
            [ ( "border-radius", "50%" )
            , ( "background-color", color )
            , ( "border", "1px solid black" )
            , ( "position", "absolute" )
            , ( "left", toString x ++ "px" )
            , ( "top", toString y ++ "px" )
            , ( "width", "64px" )
            , ( "height", "64px" )
            ]
        , onClick (playAudioMsg audio)
        ]
        []


colorGame : NewLevel msg -> PlayAudio msg -> ArtGame -> Html msg
colorGame newLevelMsg playAudioMsg artGame =
    div []
        ([ backButton ( 130, 0 ) newLevelMsg (ArtStore False)
         , text "Color Game"
         ]
            ++ List.map colorBall artGame.balls
        )


colorBall : MovingBall -> Html msg
colorBall ball =
    div
        [ style
            [ ( "border-radius", "50%" )
            , ( "background-color", ball.color )
            , ( "border", "1px solid black" )
            , ( "position", "absolute" )
            , ( "left", (toString <| getX ball.pos) ++ "px" )
            , ( "top", (toString <| getY ball.pos) ++ "px" )
            , ( "width", toString (ball.radius * 2) ++ "px" )
            , ( "height", toString (ball.radius * 2) ++ "px" )
            ]
        ]
        []



-- COMMON


playButton : ( Int, Int ) -> NewLevel msg -> Level -> Html msg
playButton ( x, y ) newLevelMsg level =
    button
        [ style
            [ ( "border", "1px solid black" )
            , ( "position", "absolute" )
            , ( "left", toString x ++ "px" )
            , ( "top", toString y ++ "px" )
            , ( "width", "128px" )
            , ( "height", "64px" )
            ]
        , onClick (newLevelMsg level)
        ]
        [ text "Play!" ]


backButton : ( Int, Int ) -> NewLevel msg -> Level -> Html msg
backButton ( x, y ) newLevelMsg level =
    button
        [ style
            [ ( "border", "1px solid black" )
            , ( "position", "absolute" )
            , ( "left", toString x ++ "px" )
            , ( "top", toString y ++ "px" )
            , ( "width", "128px" )
            , ( "height", "64px" )
            ]
        , onClick (newLevelMsg level)
        ]
        [ text "Back" ]


exit : NewLevel msg -> Level -> Html msg
exit newLevelMsg level =
    div [ onClick (newLevelMsg level) ]
        [ tile ( 0, 0 )
        , tile ( 0, 32 )
        , tile ( 32, 0 )
        , tile ( 32, 32 )
        ]


tileCountFromPixels : Int -> Int
tileCountFromPixels pixels =
    round <| toFloat pixels / 32


row : Int -> Int -> List (Html msg)
row width y =
    List.map (\i -> tile ( i * 32, y )) (List.range 0 width)


tile : ( Int, Int ) -> Html msg
tile ( x, y ) =
    img
        [ src "imgs/map-tileset.png"
        , style
            [ ( "position", "absolute" )
            , ( "clip", "rect(32px 64px 64px 32px)" )
            , ( "left", toString (x - 32) ++ "px" )
            , ( "top", toString (y - 32) ++ "px" )
            ]
        ]
        []
