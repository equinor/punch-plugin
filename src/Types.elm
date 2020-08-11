module Types exposing (..)

import Json.Decode as D


type WebData a
    = NotLoaded
    | Loading
    | DataError
    | Loaded a


type Status
    = OK
    | PB
    | PA
    | OS


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
    | RaisedByDropDown
