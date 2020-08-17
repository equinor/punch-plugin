module Punch.Types exposing (..)

import Bytes exposing (Bytes)
import File exposing (File)
import Json.Decode as D


type Context
    = NoContext
    | TagContext
    | McContext
    | CommContext


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
