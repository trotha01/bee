module Location.Home exposing (..)

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
    | ExitHome


type MsgFromPage
    = AddPoints Int
    | Exit
    | NoOp


update : Window.Size -> Translator -> Msg -> Model -> ( Model, MsgFromPage, Cmd Msg )
update window translator msg model =
    case msg of
        PlayAudio file ->
            ( model, NoOp, Audio.play file )

        ExitHome ->
            ( model, Exit, Cmd.none )



-- VIEW


view : Window.Size -> Translator -> Html Msg
view mapSize translator =
    div []
        [ exit ExitHome
        , Bee.view (Just PlayAudio) (Bee.mama translator)
        , Bee.view (Just PlayAudio) (Bee.papa translator)
        ]


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
