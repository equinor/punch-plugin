module Model exposing (Flags, Model, initialModel)

import Data.Punch as Punch exposing (Punch)
import Dict exposing (Dict)
import Json.Decode as D
import Messages exposing (Msg)
import Types exposing (..)


type alias Flags =
    { procosysPlantId : String
    }


type alias Model =
    { procosysPlantId : String
    , apiToken : String
    , punch : Dict String Punch
    , selectedPunch : Maybe Punch
    , requests : Dict Int (List (String -> String -> Cmd Msg))
    , errorMsg : String
    , highlight : Maybe String
    , dropDown : DropDown
    , organizations : WebData (List SelectItem)
    , categories : WebData (List SelectItem)
    }


initialModel : Flags -> ( Model, Cmd Msg )
initialModel flags =
    ( { procosysPlantId = flags.procosysPlantId
      , apiToken = ""
      , punch = Dict.empty
      , selectedPunch = Nothing
      , requests = Dict.empty
      , errorMsg = ""
      , highlight = Nothing
      , dropDown = NoDropDown
      , organizations = NotLoaded
      , categories = NotLoaded
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
