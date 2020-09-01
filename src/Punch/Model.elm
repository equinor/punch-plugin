module Punch.Model exposing (Model, Popup(..), initialModel)

import Dict exposing (Dict)
import Equinor.Types exposing (..)
import Json.Decode as D
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, required)
import Punch exposing (Punch)
import Punch.Messages exposing (Msg)
import Punch.Types exposing (..)
import Task


type alias Model =
    { plantId : String
    , punch : Dict Int Punch
    , selectedPunch : Maybe Punch
    , errorMsg : String
    , dropDown : DropDown
    , organizations : WebData (List SelectItem)
    , categories : WebData (List SelectItem)
    , types : WebData (List SelectItem)
    , sorts : WebData (List SelectItem)
    , currentCreate : Punch.CreatePunch
    , currentAttachment : Maybe AttachmentUpload
    , requests : Dict Int (List (String -> String -> Cmd Msg))
    , context : Context Punch
    , popup : Maybe Popup
    }


initialModel : Model
initialModel =
    { plantId = ""
    , punch = Dict.empty
    , selectedPunch = Nothing
    , errorMsg = ""
    , dropDown = NoDropDown
    , organizations = NotLoaded
    , categories = NotLoaded
    , types = NotLoaded
    , sorts = NotLoaded
    , currentCreate = Punch.initialCreate
    , currentAttachment = Nothing
    , requests = Dict.empty
    , context = NoContext
    , popup = Nothing
    }


decodePunchList : String -> List Punch
decodePunchList jsonString =
    case D.decodeString (D.list Punch.decoder) jsonString of
        Ok punch ->
            punch

        Err err ->
            []


type Popup
    = DeleteAttachmentPopup Punch Punch.Attachment
