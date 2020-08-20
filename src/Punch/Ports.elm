port module Punch.Ports exposing (punchFromJs, punchToJs)

import Json.Encode as E


port punchToJs : E.Value -> Cmd msg


port punchFromJs : (E.Value -> msg) -> Sub msg
