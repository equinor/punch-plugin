port module Ports exposing (checklists)

import Data.Checklist exposing (Checklist)
import Json.Decode as D


port checklists : (D.Value -> msg) -> Sub msg
