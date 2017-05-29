module Store.ArtStore exposing (..)

import Audio
import Dictionary.Translator exposing (Translator)
import EveryDict exposing (EveryDict)
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick, onMouseDown)
import List.Extra
import List.Zipper as Zipper exposing (Zipper(..))
import Math.Vector2 as Vec2 exposing (Vec2, getX, getY, vec2)
import Random
import Random.Extra
import Time exposing (Time)
import Window


-- MODEL


type alias Model =
    { playing : Bool

    -- TODO: change to maybe game instead of using 'playing'
    , game : ArtGame
    , seed : Random.Seed
    }


type alias ArtGame =
    { color : Color
    , balls : List MovingBall
    , deadBalls : List MovingBall
    , time : Time
    , seed : Random.Seed
    , finishRound : Bool
    , win : Bool
    , points : Int

    -- TODO: use these to add one color at a time to the game
    -- A list of known and a list of unknown colors
    -- we will introduce one color at a time in the game
    -- TODO: use zipper
    , knownUnknownColors : Zipper Color
    }



{-
   State
   = PlayingGame (playing == true)
   | SplashScreen (win == true | finishRound == true) & playing == true
   | NotPlaying (playing == false)
-}


type alias MovingBall =
    { id : Int
    , color : Color
    , pos : Vec2
    , radius : Float
    , velocity : Vec2
    , img : String
    , poof : Bool
    }


type Color
    = Red
    | Orange
    | Yellow
    | Green
    | Blue
    | Purple


initialColors : Zipper Color
initialColors =
    Zipper [] Red [ Orange, Yellow, Green, Blue, Purple ]


init : Window.Size -> Random.Seed -> ( Model, Random.Seed )
init window seed =
    let
        ( game, newSeed ) =
            initArtGame window initialColors seed
    in
    ( { game = game, playing = False, seed = newSeed }, newSeed )


initArtGame : Window.Size -> Zipper Color -> Random.Seed -> ( ArtGame, Random.Seed )
initArtGame window colors seed =
    let
        ( balls, newSeed ) =
            Random.step (initialBalls window (Zipper.current colors :: Zipper.before colors)) seed
    in
    ( { time = 0
      , seed = newSeed
      , points = 0
      , color = Zipper.current colors
      , balls = balls
      , deadBalls = []
      , win = False
      , finishRound = False
      , knownUnknownColors = colors
      }
    , newSeed
    )



-- UPDATE


type Msg
    = ColorClicked Int Color Color
    | Play
    | FinishGame
    | NextRound
    | PlayAudio String
    | ExitStore
    | Tick Time


type MsgFromPage
    = AddPoints Int
    | Exit
    | NoOp


update : Window.Size -> Translator -> Msg -> Model -> ( Model, MsgFromPage, Cmd Msg )
update window translator msg model =
    case msg of
        ColorClicked id gameColor color ->
            colorClicked ( id, gameColor, color ) model

        Play ->
            { model | playing = True }
                |> update window translator (PlayAudio (translator.audio <| toString model.game.color))

        NextRound ->
            let
                ( nextArtGame, newSeed ) =
                    initArtGame window (nextColor model.game.knownUnknownColors) model.seed
            in
            { model | game = nextArtGame, seed = newSeed }
                |> update window translator Play

        FinishGame ->
            ( { model | playing = False }, NoOp, Cmd.none )

        PlayAudio file ->
            ( model, NoOp, Audio.play file )

        ExitStore ->
            ( model, Exit, Cmd.none )

        Tick timeDelta ->
            if model.playing && not (model.game.win || model.game.finishRound) then
                tick timeDelta window translator model
            else
                ( model, NoOp, Cmd.none )


colorClicked : ( Int, Color, Color ) -> Model -> ( Model, MsgFromPage, Cmd Msg )
colorClicked ( id, gameColor, colorClicked ) model =
    if gameColor /= colorClicked then
        ( model, NoOp, Cmd.none )
    else
        let
            artGame =
                model.game

            ( newBalls, deadBalls ) =
                removeBall id artGame.balls artGame.deadBalls

            finishRound =
                List.length newBalls == 0

            win =
                finishRound
                    && (List.length (unknownColors artGame.knownUnknownColors) == 0)

            newArtGame =
                { artGame | balls = newBalls, deadBalls = deadBalls, finishRound = finishRound, win = win }
        in
        ( { model
            | game = newArtGame
          }
        , AddPoints 10
        , Audio.play "audio/puff.mp3"
        )


knownColors : Zipper a -> List a
knownColors colors =
    Zipper.current colors :: Zipper.before colors


unknownColors : Zipper a -> List a
unknownColors colors =
    Zipper.after colors


nextColor : Zipper a -> Zipper a
nextColor colors =
    Zipper.next colors
        |> Maybe.withDefault colors


tick : Time -> Window.Size -> Translator -> Model -> ( Model, MsgFromPage, Cmd Msg )
tick timeDelta window translator model =
    let
        ( newArtGame, cmd ) =
            animateArtGame timeDelta window translator model.game
    in
    ( { model
        | game = newArtGame
      }
    , NoOp
    , cmd
    )



-- RANDOM


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


colorGen : List Color -> Random.Generator Color
colorGen colors =
    randItem colors
        |> Random.map (Maybe.withDefault Red)


randomMovingBall : Window.Size -> List Color -> Int -> Random.Generator MovingBall
randomMovingBall window colors id =
    colorGen colors
        |> Random.andThen
            (\color ->
                Random.map3
                    (initMovingBall id color)
                    (randomPosition window)
                    randomVelocity
                    (randomImage color)
            )


randomMovingBallWithColor : Window.Size -> Color -> Int -> Random.Generator MovingBall
randomMovingBallWithColor window color id =
    Random.map3
        (initMovingBall id color)
        (randomPosition window)
        randomVelocity
        (randomImage color)


ballsPerRound =
    1


{-| TODO: ensure there is at least one of each color
-}
initialBalls : Window.Size -> List Color -> Random.Generator (List MovingBall)
initialBalls window colors =
    let
        initialLength =
            List.length colors

        oneOfEach =
            List.range 0 (initialLength - 1)
                |> List.Extra.zip colors
                |> List.map (\( color, id ) -> randomMovingBallWithColor window color id)

        random =
            List.range initialLength (ballsPerRound - 1)
                |> List.map (randomMovingBall window colors)

        _ =
            Debug.log "(initialLength, remainder)" ( initialLength, ballsPerRound - initialLength )

        _ =
            Debug.log "(oneOfEach, random)" ( List.length oneOfEach, List.length random )
    in
    (oneOfEach ++ random)
        |> Random.Extra.combine


initMovingBall : Int -> Color -> ( Float, Float ) -> ( Float, Float ) -> String -> MovingBall
initMovingBall id color ( x, y ) ( vx, vy ) image =
    { id = id
    , color = color
    , radius = 32
    , pos = vec2 x y
    , velocity = vec2 vx vy
    , img = image
    , poof = False
    }


removeBall : Int -> List MovingBall -> List MovingBall -> ( List MovingBall, List MovingBall )
removeBall id balls deadBalls =
    case
        List.partition (\ball -> ball.id == id) balls
    of
        ( [ ball ], balls ) ->
            ( balls, ball :: deadBalls )

        _ ->
            ( balls, deadBalls )


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
                    collide (acc ++ [ h1 ]) t1


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


{-| figure out what collision resolution to use
(currently only colliding bubbles)
-}
collision : MovingBall -> MovingBall -> CollisionResult
collision body0 body1 =
    let
        b0b1 =
            Vec2.sub body1.pos body0.pos
    in
    collisionBubbleBubble b0b1 body0.radius body1.radius


{-| calculate collision normal, penetration depth of a collision among bubbles
-- takes distance vector b0b1 and the bubble radii as argument
-}
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
        -- no collision or velocities separating
        ( b0, b1 )
    else
        let
            impulse =
                normal
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
    Debug.log "rightWall"
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
            , initRightWall (width - ball.radius * 2) height
            , initTopWall width
            , initBottomWall width (height - ball.radius * 2)
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


{-| This is the number of milliseconds we wait
before changing colors
-}
colorSwapTime : Time
colorSwapTime =
    2000


animateArtGame : Time -> Window.Size -> Translator -> ArtGame -> ( ArtGame, Cmd Msg )
animateArtGame timeDiff window translator artGame =
    let
        _ =
            Debug.log "animate art game" ""

        newTime =
            artGame.time + timeDiff

        ( ( newColor, newSeed ), timeReset, cmd ) =
            if newTime > colorSwapTime then
                let
                    ( randColor, seed ) =
                        randomColor artGame.seed artGame.balls
                in
                ( ( randColor, seed ), 0, Audio.play (translator.audio <| toString randColor) )
            else
                ( ( artGame.color, artGame.seed ), newTime, Cmd.none )
    in
    ( { artGame
        | time = timeReset
        , color = newColor
        , seed = newSeed
        , balls =
            artGame.balls
                |> List.map (animateBall timeDiff window)
                |> collisions
      }
    , cmd
    )



-- VIEW


{-| artStore : PlayGame -> Map -> Html Msg
artStore play map =
-}
view : Translator -> Model -> Html Msg
view translator model =
    case model.playing of
        True ->
            colorGame translator model.game

        False ->
            let
                showCircle n ( color, audio ) =
                    colorCircle ( 96 * n, 96 ) color audio
            in
            div [] <|
                exit ExitStore
                    :: playButton ( 192, 10 )
                    -- :: backButton ( 130, 0 )
                    :: List.indexedMap showCircle
                        [ ( "black", translator.audio "black" )
                        , ( "white", translator.audio "white" )
                        , ( "red", translator.audio "red" )
                        , ( "blue", translator.audio "blue" )
                        , ( "yellow", translator.audio "yellow" )
                        ]


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


colorGame : Translator -> ArtGame -> Html Msg
colorGame translator game =
    let
        content =
            if game.finishRound || game.win then
                [ div []
                    [ button [ onClick NextRound ] [ text "Next Round" ]
                    , button [ onClick FinishGame ] [ text "Finish Early" ]
                    ]
                ]
            else
                [ div [] [] ]

        balls =
            List.map (colorBall game.color) game.balls
                ++ List.map deadBall game.deadBalls
    in
    div []
        ([ text ("Color: " ++ translator.translate (toString game.color))
         ]
            ++ content
            ++ balls
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


deadBall : MovingBall -> Html Msg
deadBall ball =
    div
        [ style
            [ ( "position", "absolute" )
            , ( "left", (toString <| getX ball.pos) ++ "px" )
            , ( "top", (toString <| getY ball.pos) ++ "px" )
            , ( "width", toString (ball.radius * 2) ++ "px" )
            , ( "height", toString (ball.radius * 2) ++ "px" )

            -- Using external CSS sheet until I can get these working in elm
            -- , ( "background", splatBackground )
            -- , ( "background-repeat", "no-repeat" )
            -- , ( "background-size", splatBackgroundSize )
            ]
        , class "poof"
        , class <| "poof" ++ toString ball.color
        ]
        []


splatBackground : String
splatBackground =
    "-webkit-radial-gradient(red 0, red 30%, rgba(255,255,255,0) 30%) 0 0,"
        ++ "-webkit-radial-gradient(red 0, red 11%, rgba(255,255,255,0) 11%) 0 0,"
        ++ "-webkit-radial-gradient(red 0, red 20%,  rgba(255,255,255,0) 20%) -9px 37px,"
        ++ "-webkit-radial-gradient(red 0, red 11%, rgba(255,255,255,0) 11%) 55px 9px,"
        ++ "-webkit-radial-gradient(red 0, red 11%, rgba(255,255,255,0) 11%) 8px 46px,"
        ++ "-webkit-radial-gradient(red 0, red 11%, rgba(255,255,255,0) 11%) 0 0;"


splatBackgroundSize : String
splatBackgroundSize =
    "80px 68px, 37px 30px, 26px 13px, 10px 10px, 30px 30px, 50px 50px, 100% 100%;"


playButton : ( Int, Int ) -> Html Msg
playButton ( x, y ) =
    button
        [ style
            [ ( "border", "1px solid black" )
            , ( "position", "absolute" )
            , ( "left", toString x ++ "px" )
            , ( "top", toString y ++ "px" )
            , ( "width", "128px" )
            , ( "height", "64px" )
            ]
        , onClick Play
        ]
        [ text "Play!" ]


backButton : ( Int, Int ) -> Html Msg
backButton ( x, y ) =
    button
        [ style
            [ ( "border", "1px solid black" )
            , ( "position", "absolute" )
            , ( "left", toString x ++ "px" )
            , ( "top", toString y ++ "px" )
            , ( "width", "128px" )
            , ( "height", "64px" )
            ]
        , onClick FinishGame
        ]
        [ text "Back" ]


exit : Msg -> Html Msg
exit exitMsg =
    div [ onClick exitMsg ]
        [ tile ( 0, 0 )
        , tile ( 0, 32 )
        , tile ( 32, 0 )
        , tile ( 32, 32 )
        ]


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
