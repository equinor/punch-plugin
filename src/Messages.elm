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
    | NaCheckItemPressed Checklist Checklist.Item
    | OkCheckItemPressed Checklist Checklist.Item
    | SignChecklistButtonPressed Checklist
    | UnsignChecklistButtonPressed Checklist
    | VerifyChecklistButtonPressed Checklist
    | UnverifyChecklistButtonPressed Checklist
    | MetaTableCellInput Checklist Checklist.Item Checklist.Row Checklist.ColumnLabel String
    | MetaTableCellLostFocus Checklist Checklist.Item Checklist.Row Checklist.Cell
    | CommentFieldInput Checklist String
    | CommentFieldLostFocus Checklist String


type ApiResult
    = GotChecklistDetails Int (Result Http.Error Checklist.Details)
    | SetNaResult Checklist Checklist.Item (Result Http.Error ())
    | SetOkResult Checklist Checklist.Item (Result Http.Error ())
    | ClearResult Checklist Checklist.Item (Result Http.Error ())
    | SignChecklistResult Checklist (Result Http.Error ())
    | UnsignChecklistResult Checklist (Result Http.Error ())
    | VerifyChecklistResult Checklist (Result Http.Error ())
    | UnverifyChecklistResult Checklist (Result Http.Error ())
    | UpdateMetaTableCellResult Checklist (Result Http.Error ())
    | CommentChecklistResult Checklist (Result Http.Error ())
