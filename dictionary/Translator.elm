module Dictionary.Translator exposing (..)


type alias Translator =
    { translate : String -> String
    , audio : String -> String
    }
