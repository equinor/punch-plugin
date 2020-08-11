module Messages exposing (ApiResult(..), Msg(..))

import Data.Punch as Punch exposing (Punch)
import Http
import Json.Decode as D
import Types exposing (..)


type Msg
    = NoOp
    | NeverHappens
      -- Data Handling
    | GotPunchList (List Punch)
    | GotToken TokenSuccess
    | DecodeError D.Error
      -- Screen Interaction
    | PunchItemPressed Punch
    | GotApiResult ApiResult
      -- Form
    | DescriptionFieldLostFocus Punch
    | DescriptionFieldInput Punch String
    | DropDownPressed DropDown
    | DropDownItemPressed Punch SelectItem


type ApiResult
    = GotPunchDetails String (Result Http.Error Punch)
    | GotOrganizations (Result Http.Error (List SelectItem))
    | GotCategories (Result Http.Error (List SelectItem))
    | PunchDescriptionResult Punch (Result Http.Error ())
    | SetRaisedByResult Punch (Result Http.Error ())
    | SetClearingByResult Punch (Result Http.Error ())
    | SetCategoryResult Punch (Result Http.Error ())
