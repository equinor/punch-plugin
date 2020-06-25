module Main exposing (..)

import Browser
import Data.Checklist as Checklist exposing (Checklist)
import Element exposing (..)
import Element.Keyed as Keyed
import Html exposing (Html)
import Json.Decode as D
import Json.Encode as E
import Messages exposing (Msg(..))
import Ports
import Types exposing (..)
import View


type alias Model =
    { procosysPlantId : String
    , checklists : List Checklist
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ChecklistsFromJs checklists ->
            ( { model | checklists = checklists }, Cmd.none )

        DecodeError err ->
            let
                _ =
                    Debug.log "DecodeError" err
            in
            ( model, Cmd.none )


view : Model -> Element Msg
view model =
    if List.isEmpty model.checklists then
        text model.procosysPlantId

    else
        model.checklists
            |> List.map (View.renderChecklistItem 14 Nothing NotLoaded)
            |> Keyed.column [ width fill, height fill, scrollbarY ]


type alias Flags =
    { procosysPlantId : String
    , checklists : String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { procosysPlantId = flags.procosysPlantId
      , checklists = decodeChecklists flags.checklists
      }
    , Cmd.none
    )


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \model -> Sub.batch []
        , view = \model -> layout [ width fill, height fill, scrollbars ] (view model)
        }


decodeChecklists : String -> List Checklist
decodeChecklists jsonString =
    case D.decodeString (D.list Checklist.decoder) jsonString of
        Ok checklists ->
            checklists

        Err err ->
            let
                _ =
                    Debug.log "DecodeError" err
            in
            []
