module Tiles exposing (..)

import Html exposing (Html, div, img)
import Html.Attributes exposing (src, style)


-- Path Sprite


grass =
    tileFromSpriteSheet ( 0, 0 )


vertPath =
    tileFromSpriteSheet ( 32, 0 )


crossPath =
    tileFromSpriteSheet ( 64, 0 )


horzPath =
    tileFromSpriteSheet ( 0, 32 )


tPath =
    tileFromSpriteSheet ( 32, 32 )


tileFromSpriteSheet : ( Int, Int ) -> Html msg
tileFromSpriteSheet =
    imgFromSpriteSheet "terrain.png"


imgFromSpriteSheet : String -> ( Int, Int ) -> Html msg
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
