module Punch.Checklist exposing (Checklist, decoder)

import Equinor.Data.Procosys.Status as Status exposing (Status)
import Json.Decode as D


type alias Checklist =
    { id : Int
    , responsibleCode : String
    , status : Status
    , formularType : String
    }


decoder : D.Decoder Checklist
decoder =
    D.map4 Checklist
        (D.field "Id" D.int)
        (D.field "Responsible__Id" D.string)
        (D.field "Status__Id" Status.decoder)
        (D.field "TagFormularType__FormularType__Id" D.string)
