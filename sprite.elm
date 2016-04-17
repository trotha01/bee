module Sprite (..) where

import Html exposing (Html, img)
import Html.Attributes exposing (src, style)


imgFromSpriteSheet : String -> ( Int, Int ) -> Html
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



-- Path Sprite


tileFromSpriteSheet : ( Int, Int ) -> Html
tileFromSpriteSheet =
  imgFromSpriteSheet "terrain.png"


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
