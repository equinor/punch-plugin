module Punch.Subscriptions exposing (subscriptions)

import Json.Decode as D
import Json.Encode as E
import Punch.Messages exposing (Msg(..))
import Punch.Ports
import Punch.Types exposing (TokenSuccess)


subscriptions =
    Punch.Ports.punchFromJs handleJsMsg


handleJsMsg : E.Value -> Msg
handleJsMsg jsValue =
    case D.decodeValue jsValueDecoder jsValue of
        Ok msg ->
            msg

        Err err ->
            DecodeError err


jsValueDecoder : D.Decoder Msg
jsValueDecoder =
    D.field "topic" D.string
        |> D.andThen (\topic -> D.field "payload" (decoder topic))


decoder : String -> D.Decoder Msg
decoder topic =
    case topic of
        "token" ->
            D.map GotToken tokenDecoder

        _ ->
            D.fail "Unknown msg received from Js"


tokenDecoder : D.Decoder TokenSuccess
tokenDecoder =
    D.map2 TokenSuccess
        (D.field "refNo" D.int)
        (D.field "token" D.string)
