module Dictionary.Spanish exposing (..)

import Dict exposing (Dict)


type alias Word =
    ( String, Audio )


type alias Audio =
    String


dictionary : Dict String Word
dictionary =
    Dict.fromList
        [ ( "red", ( "rojo", "audio/rojo.m4a" ) )
        , ( "orange", ( "naranja", "" ) )
        , ( "yellow", ( "amarillo", "audio/amarillo.m4a" ) )
        , ( "green", ( "verde", "" ) )
        , ( "blue", ( "azul", "audio/azul.m4a" ) )
        , ( "purple", ( "púrpura", "" ) )
        , ( "black", ( "negro", "audio/negro.m4a" ) )
        , ( "white", ( "blanco", "audio/blanco.m4a" ) )
        ]


wordToString : Word -> String
wordToString ( str, audio ) =
    str


wordAudio ( str, audio ) =
    audio


findWord : String -> Word
findWord original =
    Dict.get (String.toLower original) dictionary
        |> Maybe.withDefault ( original, "" )


translate : String -> String
translate word =
    findWord word |> wordToString


audio : String -> String
audio word =
    findWord word |> wordAudio
