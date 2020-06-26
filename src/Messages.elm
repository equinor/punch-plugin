module Messages exposing (ApiResult(..), Msg(..))

import Data.Checklist as Checklist exposing (Checklist)
import Http
import Json.Decode as D
import Types exposing (..)


type Msg
    = NoOp
      -- Data Handling
    | GotChecklists (List Checklist)
    | GotToken TokenSuccess
    | DecodeError D.Error
      -- Screen Interaction
    | ChecklistPressed Checklist
    | GotApiResult ApiResult


type ApiResult
    = GotChecklistDetails Int (Result Http.Error Checklist.Details)
