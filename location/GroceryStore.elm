module Location.GroceryStore exposing (..)

import Audio
import Bee exposing (Bee)
import Dictionary.Translator exposing (Translator)
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick, onMouseDown)
import Window


-- MODEL


type alias Model =
    {}


init : Model
init =
    {}



-- UPDATE


type Msg
    = PlayAudio String
    | ExitStore


type MsgFromPage
    = AddPoints Int
    | Exit
    | NoOp


update : Window.Size -> Translator -> Msg -> Model -> ( Model, MsgFromPage, Cmd Msg )
update window translator msg model =
    case msg of
        ExitStore ->
            ( model, Exit, Cmd.none )

        PlayAudio file ->
            ( model, NoOp, Audio.play file )



-- VIEW


view : Window.Size -> Translator -> Model -> Html Msg
view mapSize translator model =
    div []
        [ exit ExitStore

        -- , playButton ( 192, 10 ) GroceryStoreRoute
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
