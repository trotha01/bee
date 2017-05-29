module Map exposing (..)

import Audio
import Bee exposing (Bee)
import Dict exposing (Dict)
import Dictionary.Translator exposing (Translator)
import EveryDict exposing (EveryDict)
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick, onMouseDown)
import Location.ArtStore as ArtStore
import Location.GroceryStore as GroceryStore
import Location.Home as Home
import Math.Vector2 as Vec2 exposing (Vec2, getX, getY, vec2)
import Random
import Random.Extra
import Random.List exposing (choose)
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
    = HomeTown
    | Home Home.Model
    | GroceryStore GroceryStore.Model
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
    | Tick Time
    | NoOp
    | HomeMsg Home.Msg
    | ArtStoreMsg ArtStore.Msg
    | GroceryStoreMsg GroceryStore.Msg


type Route
    = HomeRoute
    | HomeTownRoute
    | GroceryStoreRoute
    | ArtStoreRoute


(=>) =
    (,)


update : Translator -> Window.Size -> Msg -> Map -> ( Map, Cmd Msg )
update translator window msg map =
    case ( msg, map.level ) of
        ( NewLevel route, _ ) ->
            newLevel window route map

        ( PlayAudio file, _ ) ->
            map => Audio.play file

        ( Tick timeDelta, _ ) ->
            tick timeDelta window translator map

        ( HomeMsg msg, Home home ) ->
            let
                ( newHome, msgFromPage, cmd ) =
                    Home.update window translator msg home

                points =
                    case msgFromPage of
                        Home.AddPoints newPoints ->
                            map.points + newPoints

                        _ ->
                            map.points

                level =
                    case msgFromPage of
                        Home.Exit ->
                            HomeTown

                        _ ->
                            Home home
            in
            ( { map
                | level = level
                , points = points
              }
            , Cmd.map HomeMsg cmd
            )

        ( ArtStoreMsg msg, ArtStore artStore ) ->
            let
                ( newArtStore, msgFromPage, cmd ) =
                    ArtStore.update window translator msg artStore

                points =
                    case msgFromPage of
                        ArtStore.AddPoints newPoints ->
                            map.points + newPoints

                        _ ->
                            map.points

                level =
                    case msgFromPage of
                        ArtStore.Exit ->
                            HomeTown

                        _ ->
                            ArtStore newArtStore
            in
            ( { map
                | level = level
                , points = points
              }
            , Cmd.map ArtStoreMsg cmd
            )

        ( GroceryStoreMsg msg, GroceryStore artStore ) ->
            let
                ( newGroceryStore, msgFromPage, cmd ) =
                    GroceryStore.update window translator msg artStore

                points =
                    case msgFromPage of
                        GroceryStore.AddPoints newPoints ->
                            map.points + newPoints

                        _ ->
                            map.points

                level =
                    case msgFromPage of
                        GroceryStore.Exit ->
                            HomeTown

                        _ ->
                            GroceryStore newGroceryStore
            in
            ( { map
                | level = level
                , points = points
              }
            , Cmd.map GroceryStoreMsg cmd
            )

        ( _, _ ) ->
            -- Disregard messages from the wrong page
            ( map, Cmd.none )


newLevel : Window.Size -> Route -> Map -> ( Map, Cmd Msg )
newLevel window route map =
    let
        ( newLevel, newSeed ) =
            case route of
                HomeRoute ->
                    Home Home.init => map.seed

                HomeTownRoute ->
                    HomeTown => map.seed

                GroceryStoreRoute ->
                    GroceryStore GroceryStore.init => map.seed

                ArtStoreRoute ->
                    let
                        ( store, newSeed ) =
                            ArtStore.init window map.seed
                    in
                    ArtStore store => newSeed
    in
    { map | level = newLevel, seed = newSeed } => Cmd.none


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
    case map.level of
        HomeTown ->
            hometown mapSize translator

        Home homeModel ->
            Home.view mapSize translator
                |> Html.map HomeMsg

        GroceryStore store ->
            GroceryStore.view mapSize translator store
                |> Html.map GroceryStoreMsg

        -- groceryStore mapSize
        ArtStore store ->
            ArtStore.view translator store
                |> Html.map ArtStoreMsg



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



-- TOWN


hometown : Window.Size -> Translator -> Html Msg
hometown mapSize translator =
    div []
        [ house ( 0, 0 )
        , groceryStoreBuilding ( 160, 0 ) translator True
        , artStoreBuilding ( 320, 0 ) translator
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


groceryStoreBuilding : ( Int, Int ) -> Translator -> Bool -> Html Msg
groceryStoreBuilding ( x, y ) translator locked =
    let
        ( click, extraAttrs ) =
            if locked then
                ( onClick NoOp, [ ( "filter", "grayscale(100%)" ) ] )
            else
                ( onClick (NewLevel GroceryStoreRoute), [] )
    in
    div []
        [ img
            [ src "imgs/store.png"
            , click
            , style
                ([ ( "position", "absolute" )
                 , ( "height", "128px" )
                 , ( "width", "128px" )
                 , ( "left", toString x ++ "px" )
                 , ( "top", toString y ++ "px" )
                 ]
                    ++ extraAttrs
                )
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
            [ text <| translator.translate "grocery store" ]
        ]


artStoreBuilding : ( Int, Int ) -> Translator -> Html Msg
artStoreBuilding ( x, y ) translator =
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
            [ text <| translator.translate "art store" ]
        ]



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
