module Main exposing (..)

import Browser
import Data.Checklist as Checklist exposing (Checklist)
import Dict
import Element exposing (..)
import Element.Font as Font
import Html exposing (Html)
import Json.Decode as D
import Json.Encode as E
import Messages exposing (Msg(..))
import Model exposing (Flags, Model)
import Palette
import Ports
import Types exposing (..)
import Update exposing (update)
import View


view : Model -> Element Msg
view model =
    if Dict.isEmpty model.checklists then
        text "No Checklists"

    else
        View.renderChecklists 16 model.selectedChecklist (Dict.values model.checklists)


main : Program Flags Model Msg
main =
    Browser.element
        { init = Model.initialModel
        , update = update
        , subscriptions = \model -> Ports.fromJs handleJsMsg
        , view = \model -> layoutWith { options = [ noStaticStyleSheet ] } [ width fill, height fill, Font.color Palette.slateBlue, Font.size 14 ] (view model)
        }


handleJsMsg : E.Value -> Msg
handleJsMsg jsValue =
    case D.decodeValue jsValueDecoder jsValue of
        Ok msg ->
            msg

        Err err ->
            DecodeError err


jsValueDecoder : D.Decoder Msg
jsValueDecoder =
    D.field "topic" D.string
        |> D.andThen (\topic -> D.field "payload" (decoder topic))


decoder : String -> D.Decoder Msg
decoder topic =
    case topic of
        "token" ->
            D.map GotToken tokenDecoder

        "checklists" ->
            D.map GotChecklists (D.list Checklist.decoder)

        _ ->
            D.fail "Unknown msg received from Js"


tokenDecoder : D.Decoder TokenSuccess
tokenDecoder =
    D.map2 TokenSuccess
        (D.field "refNo" D.int)
        (D.field "token" D.string)