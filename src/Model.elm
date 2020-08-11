module Model exposing (Flags, Model, initialModel)

import Data.Punch as Punch exposing (Punch)
import Dict exposing (Dict)
import Json.Decode as D
import Messages exposing (Msg)


type alias Flags =
    { procosysPlantId : String
    }


type alias Model =
    { procosysPlantId : String
    , apiToken : String
    , punch : Dict Int Punch
    , selectedPunch : Maybe Int
    , requests : Dict Int (List (String -> String -> Cmd Msg))
    , errorMsg : String
    }


initialModel : Flags -> ( Model, Cmd Msg )
initialModel flags =
    ( { procosysPlantId = flags.procosysPlantId
      , apiToken = ""
      , punch = Dict.empty
      , selectedPunch = Nothing
      , requests = Dict.empty
      , errorMsg = ""
      }
    , Cmd.none
    )


decodePunchList : String -> List Punch
decodePunchList jsonString =
    case D.decodeString (D.list Punch.decoder) jsonString of
        Ok punch ->
            punch

        Err err ->
            []
