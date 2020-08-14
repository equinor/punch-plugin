module Punch.Messages exposing (ApiResult(..), Msg(..))

import Http
import Json.Decode as D
import Punch exposing (Punch)
import Punch.Types exposing (..)


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
    | CloseDropDownButtonPressed
    | AttachmentPressed Punch Punch.Attachment
    | DeleteAttachmentButtonPressed Punch Punch.Attachment
      -- Form
    | DescriptionFieldLostFocus Punch
    | DescriptionFieldInput Punch String
    | DropDownPressed DropDown
    | DropDownItemPressed Punch SelectItem
    | ClearPunchButtonPressed Punch
    | UnclearPunchButtonPressed Punch
    | VerifyPunchButtonPressed Punch
    | UnverifyPunchButtonPressed Punch


type ApiResult
    = GotPunchDetails Punch (Result Http.Error Punch.ApiPunch)
    | GotAttachments Punch (Result Http.Error (List Punch.Attachment))
    | GotAttachment Punch (Result Http.Error ())
    | GotOrganizations (Result Http.Error (List SelectItem))
    | GotCategories (Result Http.Error (List SelectItem))
    | GotTypes (Result Http.Error (List SelectItem))
    | GotSorts (Result Http.Error (List SelectItem))
    | PunchDescriptionResult Punch (Result Http.Error ())
    | SetRaisedByResult Punch (Result Http.Error ())
    | SetClearingByResult Punch (Result Http.Error ())
    | SetCategoryResult Punch (Result Http.Error ())
    | SetTypeResult Punch (Result Http.Error ())
    | SetSortingResult Punch (Result Http.Error ())
    | ClearResult Punch (Result Http.Error ())
    | UnclearResult Punch (Result Http.Error ())
    | VerifyResult Punch (Result Http.Error ())
    | UnverifyResult Punch (Result Http.Error ())
    | DeleteAttachmentResult Punch Punch.Attachment (Result Http.Error ())
