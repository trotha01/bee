module Map exposing (..)

import Audio
import Bee exposing (Bee)
import Dict exposing (Dict)
import Dictionary.Translator exposing (Translator)
import EveryDict exposing (EveryDict)
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick, onMouseDown)
import Math.Vector2 as Vec2 exposing (Vec2, getX, getY, vec2)
import Random
import Random.Extra
import Random.List exposing (choose)
import Store.ArtStore as ArtStore
import Time exposing (Time)
import Window


-- MODEL


type alias Map =
    { level : Level
    , points : Int
    , seed : Random.Seed
    }


init : Level -> Map
init level =
    { level = level
    , points = 0
    , seed = Random.initialSeed 0
    }


type Level
    = Home
    | HomeTown
    | GroceryStore
    | ArtStore ArtStore.Model


type alias PlayGame =
    Bool


type Color
    = Red
    | Orange
    | Yellow
    | Green
    | Blue
    | Purple



-- UPDATE


type Msg
    = NewLevel Route
    | PlayAudio String
    | ArtStoreMsg ArtStore.Msg
    | Tick Time


type Route
    = HomeRoute
    | HomeTownRoute
    | GroceryStoreRoute
    | ArtStoreRoute


update : Translator -> Window.Size -> Msg -> Map -> ( Map, Cmd Msg )
update translator window msg map =
    case ( map.level, msg ) of
        ( _, NewLevel route ) ->
            let
                level =
                    case route of
                        HomeRoute ->
                            Home

                        HomeTownRoute ->
                            HomeTown

                        GroceryStoreRoute ->
                            GroceryStore

                        ArtStoreRoute ->
                            let
                                ( store, _ ) =
                                    ArtStore.init window map.seed
                            in
                            ArtStore store
            in
            ( { map | level = level }, Cmd.none )

        ( _, PlayAudio file ) ->
            ( map, Audio.play file )

        ( _, Tick timeDelta ) ->
            tick timeDelta window translator map

        ( ArtStore artStore, ArtStoreMsg msg ) ->
            let
                ( newArtStore, msgFromPage, cmd ) =
                    ArtStore.update window translator msg artStore

                points =
                    case msgFromPage of
                        ArtStore.NoOp ->
                            map.points

                        ArtStore.AddPoints newPoints ->
                            map.points + newPoints
            in
            ( { map
                | level = ArtStore newArtStore
                , points = points
              }
            , Cmd.map ArtStoreMsg cmd
            )

        ( _, _ ) ->
            -- Should not reach here
            ( map, Cmd.none )


tick : Time -> Window.Size -> Translator -> Map -> ( Map, Cmd Msg )
tick timeDelta window translator map =
    case map.level of
        ArtStore store ->
            let
                ( newArtStore, msgFromPage, cmd ) =
                    ArtStore.update window translator (ArtStore.Tick timeDelta) store
            in
            ( { map | level = ArtStore newArtStore }, cmd |> Cmd.map ArtStoreMsg )

        _ ->
            ( map, Cmd.none )



-- VIEW


view : Window.Size -> Translator -> Map -> Html Msg
view mapSize translator map =
    let
        level =
            case map.level of
                Home ->
                    home mapSize

                HomeTown ->
                    hometown mapSize

                GroceryStore ->
                    groceryStore mapSize

                ArtStore store ->
                    ArtStore.view translator store
                        |> Html.map ArtStoreMsg
    in
    div []
        [ header mapSize map
        , body level
        ]



-- HEADER


header : Window.Size -> Map -> Html Msg
header window map =
    div
        [ style
            [ ( "position", "absolute" )
            , ( "top", px 0 )
            , ( "bottom", px 0 )
            , ( "height", px 100 )
            , ( "width", px window.width )
            , ( "border-bottom", "1px solid black" )
            , ( "background-color", "hsl(189, 100%, 50%)" )
            ]
        ]
        [ h1 [] [ text "Lingua" ]
        , viewPoints map.points
        ]



-- BODY


body : Html Msg -> Html Msg
body level =
    div
        [ style
            [ ( "position", "absolute" )
            , ( "top", px 100 )
            , ( "bottom", px 0 )
            , ( "height", px 100 )
            ]
        ]
        [ level
        ]


px : Int -> String
px x =
    toString x ++ "px"


viewPoints : Int -> Html msg
viewPoints count =
    Html.div [ class "points" ] [ text <| "Points: " ++ toString count ]



-- HOME


home : Window.Size -> Html Msg
home mapSize =
    div []
        [ exit HomeTownRoute
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
        , onClick (NewLevel HomeRoute)
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
            , onClick (NewLevel GroceryStoreRoute)
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
            , onClick (NewLevel ArtStoreRoute)
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
        [ exit HomeTownRoute
        , playButton ( 192, 10 ) GroceryStoreRoute
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



-- COMMON


playButton : ( Int, Int ) -> Route -> Html Msg
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


backButton : ( Int, Int ) -> Route -> Html Msg
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


exit : Route -> Html Msg
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
