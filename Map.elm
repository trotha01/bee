module Map exposing (..)

import Html exposing (Html, img, div)
import Html.Attributes exposing (src, style)
import Window


view : Window.Size -> Html msg
view mapSize =
    ground mapSize


ground : Window.Size -> Html msg
ground mapSize =
    div []
        ((List.range 0 (tileCountFromPixels mapSize.height))
            |> List.map (\y -> row (tileCountFromPixels mapSize.width) (y * 32))
            |> List.concat
        )


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
