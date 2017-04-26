module Map exposing (..)

import Html exposing (Html, img, div)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick)
import Window
import Bee exposing (Bee)


type Level
    = Home
    | HomeTown
    | Store
    | ArtStore


type alias NewLevel msg =
    Level -> msg


type alias PlayAudio msg =
    Maybe (String -> msg)


view : NewLevel msg -> PlayAudio msg -> Window.Size -> Level -> Html msg
view newLevelMsg playAudioMsg mapSize level =
    case level of
        Home ->
            home newLevelMsg playAudioMsg mapSize

        HomeTown ->
            hometown newLevelMsg playAudioMsg mapSize

        Store ->
            groceryStore newLevelMsg playAudioMsg mapSize

        ArtStore ->
            artStore newLevelMsg playAudioMsg mapSize



-- HOME


home : NewLevel msg -> PlayAudio msg -> Window.Size -> Html msg
home newLevelMsg playAudioMsg mapSize =
    div []
        [ exit newLevelMsg HomeTown
        , Bee.view playAudioMsg Bee.mama
        , Bee.view playAudioMsg Bee.papa
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
            , ( "left", (toString x) ++ "px" )
            , ( "top", (toString y) ++ "px" )
            ]
        ]
        []


storeBuilding : ( Int, Int ) -> NewLevel msg -> Html msg
storeBuilding ( x, y ) newLevelMsg =
    img
        [ src "imgs/store.png"
        , onClick (newLevelMsg Store)
        , style
            [ ( "position", "absolute" )
            , ( "height", "128px" )
            , ( "width", "128px" )
            , ( "left", (toString x) ++ "px" )
            , ( "top", (toString y) ++ "px" )
            ]
        ]
        []


artStoreBuilding : ( Int, Int ) -> NewLevel msg -> Html msg
artStoreBuilding ( x, y ) newLevelMsg =
    img
        [ src "imgs/store.png"
        , onClick (newLevelMsg ArtStore)
        , style
            [ ( "position", "absolute" )
            , ( "height", "128px" )
            , ( "width", "128px" )
            , ( "left", (toString x) ++ "px" )
            , ( "top", (toString y) ++ "px" )
            ]
        ]
        []



-- GROCERY STORE


groceryStore : NewLevel msg -> PlayAudio msg -> Window.Size -> Html msg
groceryStore newLevelMsg playAudioMsg mapSize =
    div []
        [ exit newLevelMsg HomeTown
        ]



-- ART STORE


artStore : NewLevel msg -> PlayAudio msg -> Window.Size -> Html msg
artStore newLevelMsg playAudioMsg mapSize =
    let
        showCircle n ( color, audio ) =
            colorCircle playAudioMsg ( 96 * n, 96 ) color audio
    in
        div [] <|
            (exit newLevelMsg HomeTown)
                :: List.indexedMap showCircle
                    [ ( "black", "audio/negro.m4a" )
                    , ( "white", "audio/blanco.m4a" )
                    , ( "red", "audio/rojo.m4a" )
                    , ( "blue", "audio/azul.m4a" )
                    , ( "yellow", "audio/amarillo.m4a" )
                    ]


colorCircle : PlayAudio msg -> ( Int, Int ) -> String -> String -> Html msg
colorCircle playAudioMsg ( x, y ) color audio =
    let
        clickEvent =
            case playAudioMsg of
                Nothing ->
                    []

                Just click ->
                    [ onClick (click audio) ]
    in
        div
            ([ style
                [ ( "border-radius", "50%" )
                , ( "background-color", color )
                , ( "border", "1px solid black" )
                , ( "position", "absolute" )
                , ( "left", (toString x) ++ "px" )
                , ( "top", (toString y) ++ "px" )
                , ( "width", "64px" )
                , ( "height", "64px" )
                ]
             ]
                ++ clickEvent
            )
            []



-- COMMON


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
    (round <| (toFloat pixels) / 32)


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
            , ( "left", (toString (x - 32)) ++ "px" )
            , ( "top", (toString (y - 32)) ++ "px" )
            ]
        ]
        []
