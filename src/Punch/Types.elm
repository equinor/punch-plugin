module Punch.Types exposing (..)

import Bytes exposing (Bytes)
import File exposing (File)
import Json.Decode as D


type Context punch
    = NoContext
    | TagContext String
    | McContext String
    | CommContext String
    | CreateContext TagNo (Maybe Int)
    | CreatedContext Int
    | SearchContext String (List punch)


type alias TagNo =
    String


type alias TextToHighlight =
    String


type SyncStatus
    = Inactive
    | DownloadingFromApi
    | SavingToStorage
    | UpdatingFromApi
    | LoadingFromStorage
    | Synchronized


type alias TokenSuccess =
    { refNo : Int
    , token : String
    }


type alias Size =
    { width : Float
    , height : Float
    }


type alias SelectItem =
    { id : Int
    , code : String
    , description : String
    }


selectItemDecoder : D.Decoder SelectItem
selectItemDecoder =
    D.map3 SelectItem
        (D.field "Id" D.int)
        (D.field "Code" D.string)
        (D.field "Description" D.string)


type DropDown
    = NoDropDown
    | CategoryDropDown
    | RaisedByDropDown
    | ClearingByDropDown
    | TypeDropDown
    | SortingDropDown


type alias AttachmentUpload =
    { file : File
    , punchId : Int
    , uri : String
    , name : String
    }


type alias Blob =
    { contentType : String
    , bytes : Bytes
    }
