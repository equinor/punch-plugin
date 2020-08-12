module Punch exposing (Dict, Punch, PunchLists, Status,apiDecoder, decoder, encoder, filterByTimeFrame, sort)


import Date exposing (Date)
import Dict as CoreDict
import Json.Decode as D
import Json.Decode.Pipeline exposing (custom, optional, required)
import Json.Encode as E
import Time exposing (Posix)
import Punch.Types exposing (..)
import Equinor.Data.Procosys.Status as Status 
import Iso8601

type alias Dict =

    CoreDict.Dict String (List Punch)

type alias Status = Status.Status



type alias Punch =
    { id : String
    , tag : String
    , tagDescription : String
    , description : String
    , createdAt : Posix
    , updatedAt : String
    , status : Status
    , commPk : String
    , mcPk : String
    , raisedByOrg : String
    , clearingByOrg : String
    , location : String
    , typeDescription : String
    }


type alias PunchLists =
    { today : List Punch
    , yesterday : List Punch
    }


filterByTimeFrame : Posix -> Time.Zone -> Int -> List Punch -> List Punch
filterByTimeFrame time zone timeFrame punchList =
    let
        today : Date
        today =
            Date.fromPosix zone time
    in
    punchList
    
        |> List.filter
            (\punch ->
                Date.diff
                    Date.Days
                    (Date.fromPosix zone punch.createdAt)
                    today
                    < timeFrame
            )


encoder : Punch -> E.Value
encoder punch =
    E.object
        [ ( "id", E.string punch.id )
        , ( "tag", E.string punch.tag )
        , ( "tagDescription", E.string punch.tagDescription )
        , ( "description", E.string punch.description )
        , ( "createdAt", E.int <| Time.posixToMillis punch.createdAt )
        , ( "updatedAt", E.string punch.updatedAt )
        , ( "status", Status.encoder punch.status )
        , ( "commPk", E.string punch.commPk )
        , ( "mcPk", E.string punch.mcPk )
        , ( "raisedByOrg", E.string punch.raisedByOrg )
        , ( "clearingByOrg", E.string punch.clearingByOrg )
        , ( "location", E.string punch.location )
        , ( "typeDescription", E.string punch.typeDescription )
        ]


decoder : D.Decoder Punch
decoder =
    D.succeed Punch
        |> required "id" D.string
        |> required "tag" D.string
        |> required "tagDescription" D.string
        |> required "description" D.string
        |> required "createdAt" timeDecoder
        |> required "updatedAt" D.string
        |> required "status" Status.decoder
        |> required "commPk" D.string
        |> required "mcPk" D.string
        |> optional "raisedByOrg" D.string ""
        |> optional "clearingByOrg" D.string ""
        |> optional "location" D.string ""
        |> optional "typeDescription" D.string ""


apiDecoder : D.Decoder Punch
apiDecoder =
    D.succeed Punch
        |> required "PunchListItemNo" (D.int |> D.andThen (D.succeed << String.fromInt))
        |> required "CheckList__TagFormularType__Tag__TagNo" nullString
        |> required "CheckList__TagFormularType__Tag__Description" nullString
        |> required "Description" nullString
        |> required "CreatedAt" apiTimeDecoder
        |> required "UpdatedAt" nullString
        |> required "Status__Id" Status.decoder
        |> required "CheckList__TagFormularType__Tag__McPkg__CommPkg__CommPkgNo" nullString
        |> required "CheckList__TagFormularType__Tag__McPkg__McPkgNo" nullString
        |> required "ClearedByOrg__Description" nullString
        |> required "RaisedByOrg__Description" nullString
        |> custom
            (D.oneOf
                [ D.field "CheckList__TagFormularType__Tag__Area__Id" D.string
                , D.field "CheckList__TagFormularType__Tag__McPkg__Area__Id" nullString
                ]
            )
        |> required "PunchListType__Description" nullString


sort : List Punch -> List Punch
sort punchList =
    punchList
        |> List.sortBy (\x -> ( x.commPk, x.mcPk, x.tag ))

timeDecoder : D.Decoder Posix
timeDecoder =
    D.int
        |> D.andThen (D.succeed << Time.millisToPosix)

nullString : D.Decoder String
nullString =
    D.oneOf
        [ D.string
        , D.null ""
        ]


nullInt : D.Decoder Int
nullInt =
    D.oneOf
        [ D.int
        , D.null 0
        ]

apiTimeDecoder : D.Decoder Posix
apiTimeDecoder =
    D.oneOf
        [ D.string
            |> D.andThen
                (\str ->
                    case Iso8601.toTime (str ++ "Z") of
                        Ok posix ->
                            D.succeed posix

                        Err err ->
                            D.fail "Not valid date format"
                )
        , D.null (Time.millisToPosix 0)
        ]
