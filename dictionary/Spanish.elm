module Dictionary.Spanish exposing (..)

import Dict exposing (Dict)
import String.Extra exposing (replace)


type alias Word =
    String


type alias Audio =
    String


dictionary : Dict String Word
dictionary =
    List.foldl Dict.union
        Dict.empty
        [ family, colors, places ]


family : Dict String Word
family =
    Dict.fromList
        [ ( "mom", "mamá" )
        , ( "dad", "papá" )
        ]


colors : Dict String Word
colors =
    Dict.fromList
        [ ( "red", "rojo" )
        , ( "orange", "naranja" )
        , ( "yellow", "amarillo" )
        , ( "green", "verde" )
        , ( "blue", "azul" )
        , ( "purple", "púrpura" )
        , ( "black", "negro" )
        , ( "white", "blanco" )
        ]


places : Dict String Word
places =
    Dict.fromList
        [ ( "art store", "tienda de arte" )
        , ( "grocery store", "el mercado" )
        ]


wordToString : Word -> String
wordToString str =
    str


wordAudio : Word -> String
wordAudio str =
    audioFile str


findWord : String -> Word
findWord original =
    Dict.get (String.toLower original) dictionary
        |> Maybe.withDefault original


translate : String -> String
translate word =
    findWord word |> wordToString


audio : String -> String
audio word =
    findWord word |> wordAudio



-- HELPERS


audioDir =
    "audio/spanish/"


audioFile : String -> String
audioFile file =
    audioDir ++ file ++ ".mp3"
