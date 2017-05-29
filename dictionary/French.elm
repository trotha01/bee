module Dictionary.French exposing (..)

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
        [ ( "mom", "maman" )
        , ( "dad", "papa" )
        ]


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


places : Dict String Word
places =
    Dict.fromList
        [ ( "art store", "magasin d'art" )
        , ( "grocery store", "épicerie" )
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
