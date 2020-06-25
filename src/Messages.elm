module Messages exposing (Msg(..))

import Data.Checklist exposing (Checklist)
import Json.Decode as D


type Msg
    = NoOp
    | ChecklistsFromJs (List Checklist)
    | DecodeError D.Error
