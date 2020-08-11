module Messages exposing (ApiResult(..), Msg(..))

import Data.Punch as Punch exposing (Punch)
import Http
import Json.Decode as D
import Types exposing (..)


type Msg
    = NoOp
      -- Data Handling
    | GotPunchList (List Punch)
    | GotToken TokenSuccess
    | DecodeError D.Error
      -- Screen Interaction
    | PunchItemPressed Punch
    | GotApiResult ApiResult


type ApiResult
    = GotPunchDetails Int (Result Http.Error Punch)
