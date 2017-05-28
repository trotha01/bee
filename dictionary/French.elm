module Dictionary.French exposing (..)

import Dict exposing (Dict)
import String.Extra exposing (replace)


type alias Word =
    String


type alias Audio =
    String


dictionary : Dict String Word
dictionary =
    colors


colors : Dict String Word
colors =
    Dict.fromList
        [ ( "red", "pula" )
        , ( "orange", "kahel" )
        , ( "yellow", "dilaw" )
        , ( "green", "berde" )
        , ( "blue", "asul" )
        , ( "purple", "lila" )
        , ( "black", "itim" )
        , ( "white", "puti" )
        ]


wordToString : Word -> String
wordToString str =
    str


wordAudio : Word -> String
wordAudio str =
    audioFile (replace "ú" "u" str)


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
    "audio/french/"


audioFile : String -> String
audioFile file =
    audioDir ++ file ++ ".mp3"