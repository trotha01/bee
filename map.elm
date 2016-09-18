module Map exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Tiles exposing (..)


-- Model


{-| Model contains the offsets of the full map.
--  We only see a part of the map at a time.
-}
type alias Model =
    { top : Int
    , left : Int
    }


( fullMapWidth, fullMapHeight ) =
    ( 288, 288 )
{-| This is the width and height of
    the visible portion of the map
-}
( width, height ) =
    ( 224, 224 )
( halfWidth, halfHeight ) =
    ( width / 2, height / 2 )
{-| overflow width and height are the
    unseen dimensions of the map
-}
( overflowWidth, overflowHeight ) =
    ( fullMapWidth - width, fullMapHeight - height )
{-| We initialize the map to start in approxamitely the center
-}
init : Model
init =
    { top = round -(overflowHeight / 2)
    , left = round -(overflowWidth / 2)
    }



-- Update


type Action
    = VerticalScroll Int
    | HorizontalScroll Int


update : Action -> Model -> Model
update action model =
    case action of
        VerticalScroll i ->
            { model | top = clamp -overflowHeight 0 (model.top + i) }

        HorizontalScroll i ->
            { model | left = clamp -overflowWidth 0 (model.left - i) }



-- View


view : Model -> Html msg
view model =
    div
        [ style
            [ ( "width", (toString width) ++ "px" )
            , ( "height", (toString height) ++ "px" )
            , ( "overflow", "hidden" )
            , ( "border", "2px solid black" )
            ]
        ]
        [ fullMap model ]


fullMap : Model -> Html msg
fullMap model =
    div
        [ style
            [ ( "width", (toString fullMapWidth) ++ "px" )
            , ( "height", (toString fullMapHeight) ++ "px" )
            , ( "position", "relative" )
            , ( "top", (toString model.top) ++ "px" )
            , ( "left", (toString model.left) ++ "px" )
            ]
        ]
        [ column
            [ (row [ grass, grass, grass, grass, grass, grass, vertPath, grass, grass ])
            , (row [ horzPath, horzPath, horzPath, horzPath, horzPath, horzPath, crossPath, horzPath, horzPath ])
            , (row [ grass, grass, grass, grass, grass, grass, vertPath, grass, grass ])
            , (row [ grass, grass, grass, grass, grass, grass, vertPath, grass, grass ])
            , (row [ grass, grass, grass, grass, grass, grass, vertPath, grass, grass ])
            , (row [ grass, grass, grass, grass, grass, grass, vertPath, grass, grass ])
            , (row [ grass, grass, grass, grass, grass, grass, vertPath, grass, grass ])
            , (row [ grass, grass, grass, grass, grass, grass, vertPath, grass, grass ])
            , (row [ grass, grass, grass, grass, grass, grass, vertPath, grass, grass ])
            ]
        ]


row : List (Html msg) -> Html msg
row divs =
    div [ style [ ( "height", "32px" ) ] ] divs


column : List (Html msg) -> Html msg
column divs =
    div [] divs
