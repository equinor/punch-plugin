module Punch.Model exposing (Flags, Model, decodeFlags, initialModel)

import Dict exposing (Dict)
import Equinor.Types exposing (..)
import Json.Decode as D
import Punch exposing (Punch)
import Punch.Messages exposing (Msg)
import Punch.Types exposing (..)


type alias Flags =
    { procosysPlantId : String
    , context : Context
    , textToHighlight : String
    }


decodeFlags : D.Value -> Flags
decodeFlags val =
    case D.decodeValue flagsDecoder val of
        Ok flags ->
            flags

        Err _ ->
            Flags "" NoContext ""


contextDecoder : D.Decoder Context
contextDecoder =
    D.maybe (D.field "context" D.string)
        |> D.andThen
            (\maybeStr ->
                case maybeStr of
                    Just str ->
                        case str of
                            "tag" ->
                                D.succeed TagContext

                            "mc" ->
                                D.succeed McContext

                            "comm" ->
                                D.succeed CommContext

                            _ ->
                                D.succeed NoContext

                    Nothing ->
                        D.succeed NoContext
            )


flagsDecoder : D.Decoder Flags
flagsDecoder =
    D.map3 Flags
        (D.field "procosysPlantId" D.string)
        contextDecoder
        (D.field "textToHighlight" D.string)


type alias Model =
    { procosysPlantId : String
    , apiToken : String
    , punch : Dict Int Punch
    , context : Context
    , selectedPunch : Maybe Punch
    , requests : Dict Int (List (String -> String -> Cmd Msg))
    , errorMsg : String
    , highlight : String
    , dropDown : DropDown
    , organizations : WebData (List SelectItem)
    , categories : WebData (List SelectItem)
    , types : WebData (List SelectItem)
    , sorts : WebData (List SelectItem)
    , currentAttachment : Maybe AttachmentUpload
    }


initialModel : Flags -> ( Model, Cmd Msg )
initialModel flags =
    ( { procosysPlantId = flags.procosysPlantId
      , apiToken = ""
      , punch = Dict.empty
      , context = flags.context
      , selectedPunch = Nothing
      , requests = Dict.empty
      , errorMsg = ""
      , highlight = flags.textToHighlight
      , dropDown = NoDropDown
      , organizations = NotLoaded
      , categories = NotLoaded
      , types = NotLoaded
      , sorts = NotLoaded
      , currentAttachment = Nothing
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
