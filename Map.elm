module Map exposing (..)

import Audio
import Bee exposing (Bee)
import Dict exposing (Dict)
import EveryDict exposing (EveryDict)
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick, onMouseDown)
import Math.Vector2 as Vec2 exposing (Vec2, getX, getY, vec2)
import Random
import Random.Extra
import Random.List exposing (choose)
import Time exposing (Time)
import Window


-- MODEL


type alias Map =
    { level : Level
    , artGame : ArtGame
    , window : Window.Size
    , points : Int
    , seed : Random.Seed
    }


init : Window.Size -> Level -> Map
init window level =
    let
        ( artGame, _ ) =
            initArtGame window (Random.initialSeed 0)
    in
    { level = level
    , artGame = artGame
    , window = window
    , points = 0
    , seed = Random.initialSeed 0
    }


type Level
    = Home
    | HomeTown
    | GroceryStore
    | ArtStore PlayGame


type alias PlayGame =
    Bool


type Color
    = Red
    | Orange
    | Yellow
    | Green
    | Blue
    | Purple


colorGen : Random.Generator Color
colorGen =
    randItem [ Red, Orange, Yellow, Green, Blue, Purple ]
        |> Random.map (Maybe.withDefault Red)


type alias ArtGame =
    { color : Color
    , balls : List MovingBall
    , time : Time
    , seed : Random.Seed
    , win : Bool
    }


type alias MovingBall =
    { id : Int
    , color : Color
    , pos : Vec2
    , radius : Float
    , velocity : Vec2
    , img : String
    }



{-
   newLevel : Level -> Map -> Map
   newLevel level map =
       case level of
           ArtStore True ->
               let
                   ( artGame, seed ) =
                       initArtGame map.window map.seed
               in
               { map | artGame = artGame, seed = seed, level = level }

           _ ->
               { map | level = level }
-}


initArtGame : Window.Size -> Random.Seed -> ( ArtGame, Random.Seed )
initArtGame window seed =
    let
        ( balls, newSeed ) =
            Random.step (initialBalls window) seed
    in
    ( { time = 0
      , seed = newSeed
      , color = Yellow
      , balls = balls
      , win = False
      }
    , newSeed
    )


get : Int -> List a -> Maybe a
get index list =
    list
        |> List.drop index
        |> List.head


randItem : List a -> Random.Generator (Maybe a)
randItem list =
    Random.int 0 (List.length list - 1)
        |> Random.map (\i -> get i list)


randomColor : Random.Seed -> List MovingBall -> ( Color, Random.Seed )
randomColor seed balls =
    Random.step (randItem balls) seed
        |> Tuple.mapFirst (Maybe.map (\ball -> ball.color))
        |> Tuple.mapFirst (Maybe.withDefault Red)


imageDir =
    "imgs/colors/"


images : EveryDict Color (List String)
images =
    EveryDict.fromList
        [ ( Red, [ "red-cherry.png", "red-umbrella.png" ] )
        , ( Orange, [ "orange-tangerine.png" ] )
        , ( Yellow, [ "yellow-banana.png", "yellow-car.png", "yellow-cheese.png" ] )
        , ( Green, [ "green-frog.png", "green-turtle.png" ] )
        , ( Blue, [ "blue-bird.png", "blue-fish.png" ] )
        , ( Purple, [ "purple-octopus.png" ] )
        ]


alwaysGen : a -> Random.Generator a
alwaysGen x =
    Random.map (\_ -> x) Random.bool


randomImage : Color -> Random.Generator String
randomImage color =
    let
        options =
            EveryDict.get color images
    in
    case options of
        Nothing ->
            -- should never happen
            alwaysGen ""

        Just [] ->
            -- should never happen
            alwaysGen ""

        Just imageList ->
            randItem imageList
                |> Random.map (Maybe.withDefault "")


randomPosition : Window.Size -> Random.Generator ( Float, Float )
randomPosition window =
    Random.pair
        (Random.map toFloat (Random.int 30 window.width))
        (Random.map toFloat (Random.int 30 window.height))


randomVelocity : Random.Generator ( Float, Float )
randomVelocity =
    Random.pair
        nonZeroFloat
        nonZeroFloat


nonZeroFloat : Random.Generator Float
nonZeroFloat =
    Random.int -1 1
        |> Random.map
            (\i ->
                if i == 0 then
                    1
                else
                    i
            )
        |> Random.map toFloat


randomMovingBall : Window.Size -> Int -> Random.Generator MovingBall
randomMovingBall window id =
    colorGen
        |> Random.andThen
            (\color ->
                Random.map3
                    (initMovingBall id color)
                    (randomPosition window)
                    randomVelocity
                    (randomImage color)
            )


initialBalls : Window.Size -> Random.Generator (List MovingBall)
initialBalls window =
    List.range 0 9
        |> List.map (randomMovingBall window)
        |> Random.Extra.combine


initMovingBall : Int -> Color -> ( Float, Float ) -> ( Float, Float ) -> String -> MovingBall
initMovingBall id color ( x, y ) ( vx, vy ) image =
    { id = id
    , color = color
    , radius = 32
    , pos = vec2 x y
    , velocity = vec2 vx vy
    , img = image
    }



-- UPDATE


type Msg
    = NewLevel Level
    | PlayAudio String
    | ColorClicked Int Color Color


update : Msg -> Map -> ( Map, Cmd Msg )
update msg map =
    case msg of
        NewLevel level ->
            let
                ( artGame, newSeed ) =
                    case level of
                        ArtStore True ->
                            initArtGame map.window map.seed

                        _ ->
                            ( map.artGame, map.seed )
            in
            ( { map | level = level, artGame = artGame, seed = newSeed }, Cmd.none )

        PlayAudio file ->
            ( map, Audio.play file )

        ColorClicked id gameColor color ->
            if gameColor == color then
                let
                    artGame =
                        map.artGame

                    newBalls =
                        artGame.balls |> removeBall id

                    win =
                        List.length newBalls == 0

                    newArtGame =
                        { artGame | balls = newBalls, win = win }
                in
                ( { map
                    | points = map.points + 10
                    , artGame = newArtGame
                  }
                , Audio.play "audio/puff.mp3"
                )
            else
                ( map, Cmd.none )


removeBall : Int -> List MovingBall -> List MovingBall
removeBall id balls =
    List.filter (\ball -> ball.id /= id) balls


resize : Window.Size -> Map -> Map
resize window map =
    { map | window = window }


tick : Time -> Map -> Map
tick timeDelta map =
    { map
        | artGame = animateArtGame timeDelta map.window map.artGame
    }


{-| This is the number of milliseconds we wait
before changing colors
-}
colorSwapTime : Time
colorSwapTime =
    2000


animateArtGame : Time -> Window.Size -> ArtGame -> ArtGame
animateArtGame timeDiff window artGame =
    let
        newTime =
            artGame.time + timeDiff

        ( ( newColor, newSeed ), timeReset ) =
            if newTime > colorSwapTime then
                ( randomColor artGame.seed artGame.balls, 0 )
            else
                ( ( artGame.color, artGame.seed ), newTime )
    in
    { artGame
        | time = timeReset
        , color = newColor
        , seed = newSeed
        , balls =
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


slowdown =
    1 / 10


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
            (x + getX ball.velocity * timeDiff * slowdown)
                |> clamp (getX leftWall.pos) (getX rightWall.pos)

        newY =
            (y + getY ball.velocity * timeDiff * slowdown)
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


view : Window.Size -> Map -> Html Msg
view mapSize map =
    case map.level of
        Home ->
            home mapSize

        HomeTown ->
            hometown mapSize

        GroceryStore ->
            groceryStore mapSize

        ArtStore play ->
            artStore play map



-- HOME


home : Window.Size -> Html Msg
home mapSize =
    div []
        [ exit HomeTown
        , Bee.view (Just PlayAudio) Bee.mama
        , Bee.view (Just PlayAudio) Bee.papa
        ]



-- TOWN


hometown : Window.Size -> Html Msg
hometown mapSize =
    div []
        [ house ( 0, 0 )
        , storeBuilding ( 160, 0 )
        , artStoreBuilding ( 320, 0 )
        ]


house : ( Int, Int ) -> Html Msg
house ( x, y ) =
    img
        [ src "imgs/home.png"
        , onClick (NewLevel Home)
        , style
            [ ( "position", "absolute" )
            , ( "height", "128px" )
            , ( "width", "128px" )
            , ( "left", toString x ++ "px" )
            , ( "top", toString y ++ "px" )
            ]
        ]
        []


storeBuilding : ( Int, Int ) -> Html Msg
storeBuilding ( x, y ) =
    div []
        [ img
            [ src "imgs/store.png"
            , onClick (NewLevel GroceryStore)
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


artStoreBuilding : ( Int, Int ) -> Html Msg
artStoreBuilding ( x, y ) =
    div []
        [ img
            [ src "imgs/store.png"
            , onClick (NewLevel (ArtStore False))
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


groceryStore : Window.Size -> Html Msg
groceryStore mapSize =
    div []
        [ exit HomeTown
        , playButton ( 192, 10 ) GroceryStore
        , groceryItem ( 64, 96 ) "imgs/banana.png" "audio/el_platano.mp3"
        , groceryItem ( 192, 96 ) "imgs/milk.png" "audio/leche.mp3"
        ]


groceryItem : ( Int, Int ) -> String -> String -> Html Msg
groceryItem ( x, y ) image audio =
    img
        [ src image
        , style
            [ ( "width", "64px" )
            , ( "height", "64px" )
            , ( "position", "absolute" )
            , ( "left", toString x ++ "px" )
            , ( "top", toString y ++ "px" )
            ]
        , onClick (PlayAudio audio)
        ]
        []



-- ART STORE


artStore : PlayGame -> Map -> Html Msg
artStore play map =
    case play of
        True ->
            colorGame map.points map.artGame

        False ->
            let
                showCircle n ( color, audio ) =
                    colorCircle ( 96 * n, 96 ) color audio
            in
            div [] <|
                exit HomeTown
                    :: playButton ( 192, 10 ) (ArtStore True)
                    :: viewPoints map.points
                    :: List.indexedMap showCircle
                        [ ( "black", "audio/negro.m4a" )
                        , ( "white", "audio/blanco.m4a" )
                        , ( "red", "audio/rojo.m4a" )
                        , ( "blue", "audio/azul.m4a" )
                        , ( "yellow", "audio/amarillo.m4a" )
                        ]


viewPoints : Int -> Html msg
viewPoints count =
    Html.div [ class "points" ] [ text <| "Points: " ++ toString count ]


colorCircle : ( Int, Int ) -> String -> String -> Html Msg
colorCircle ( x, y ) color audio =
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
        , onClick (PlayAudio audio)
        ]
        []


colorGame : Int -> ArtGame -> Html Msg
colorGame pointCount artGame =
    let
        content =
            if artGame.win then
                [ div []
                    [ text "You Win!!!!!!" ]
                ]
            else
                List.map (colorBall artGame.color) artGame.balls
    in
    div []
        ([ backButton ( 130, 0 ) (ArtStore False)
         , text ("Color: " ++ toString artGame.color)
         , viewPoints pointCount
         ]
            ++ content
        )


colorBall : Color -> MovingBall -> Html Msg
colorBall gameColor ball =
    div
        [ style
            [ ( "border-radius", "50%" )

            -- , ( "background-color", toString ball.color )
            , ( "background-image", "url(" ++ imageDir ++ ball.img ++ ")" )
            , ( "background-size", "contain" )
            , ( "background-repeat", "no-repeat" )
            , ( "border", "1px solid black" )
            , ( "position", "absolute" )
            , ( "left", (toString <| getX ball.pos) ++ "px" )
            , ( "top", (toString <| getY ball.pos) ++ "px" )
            , ( "width", toString (ball.radius * 2) ++ "px" )
            , ( "height", toString (ball.radius * 2) ++ "px" )
            ]
        , onMouseDown (ColorClicked ball.id gameColor ball.color)
        ]
        []



-- COMMON


playButton : ( Int, Int ) -> Level -> Html Msg
playButton ( x, y ) level =
    button
        [ style
            [ ( "border", "1px solid black" )
            , ( "position", "absolute" )
            , ( "left", toString x ++ "px" )
            , ( "top", toString y ++ "px" )
            , ( "width", "128px" )
            , ( "height", "64px" )
            ]
        , onClick (NewLevel level)
        ]
        [ text "Play!" ]


backButton : ( Int, Int ) -> Level -> Html Msg
backButton ( x, y ) level =
    button
        [ style
            [ ( "border", "1px solid black" )
            , ( "position", "absolute" )
            , ( "left", toString x ++ "px" )
            , ( "top", toString y ++ "px" )
            , ( "width", "128px" )
            , ( "height", "64px" )
            ]
        , onClick (NewLevel level)
        ]
        [ text "Back" ]


exit : Level -> Html Msg
exit level =
    div [ onClick (NewLevel level) ]
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
