module Data.Punch exposing (Dict, Punch, PunchLists, apiDecoder, decoder, encoder, sort)

import Data.Common as Common
import Dict as CoreDict
import Json.Decode as D
import Json.Decode.Pipeline exposing (custom, optional, required)
import Json.Encode as E
import Types exposing (Status(..))


type alias Dict =
    CoreDict.Dict String (List Punch)


type alias Punch =
    { id : Int
    , tag : String
    , tagDescription : String
    , description : String
    , createdAt : String
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


encoder : Punch -> E.Value
encoder punch =
    E.object
        [ ( "id", E.int punch.id )
        , ( "tag", E.string punch.tag )
        , ( "tagDescription", E.string punch.tagDescription )
        , ( "description", E.string punch.description )
        , ( "createdAt", E.string punch.createdAt )
        , ( "updatedAt", E.string punch.updatedAt )
        , ( "status", E.string <| Common.statusToString punch.status )
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
        |> required "id" D.int
        |> required "tag" D.string
        |> required "tagDescription" D.string
        |> required "description" D.string
        |> required "createdAt" D.string
        |> required "updatedAt" D.string
        |> required "status" statusDecoder
        |> required "commPk" D.string
        |> required "mcPk" D.string
        |> optional "raisedByOrg" D.string ""
        |> optional "clearingByOrg" D.string ""
        |> optional "location" D.string ""
        |> optional "typeDescription" D.string ""


apiDecoder : D.Decoder Punch
apiDecoder =
    D.succeed Punch
        |> required "PunchListItemNo" D.int
        |> required "CheckList__TagFormularType__Tag__TagNo" Common.nullString
        |> required "CheckList__TagFormularType__Tag__Description" Common.nullString
        |> required "Description" Common.nullString
        |> required "CreatedAt" D.string
        |> required "UpdatedAt" Common.nullString
        |> required "Status__Id" statusDecoder
        |> required "CheckList__TagFormularType__Tag__McPkg__CommPkg__CommPkgNo" Common.nullString
        |> required "CheckList__TagFormularType__Tag__McPkg__McPkgNo" Common.nullString
        |> required "ClearedByOrg__Description" Common.nullString
        |> required "RaisedByOrg__Description" Common.nullString
        |> custom
            (D.oneOf
                [ D.field "CheckList__TagFormularType__Tag__Area__Id" D.string
                , D.field "CheckList__TagFormularType__Tag__McPkg__Area__Id" Common.nullString
                ]
            )
        |> required "PunchListType__Description" Common.nullString


sort : List Punch -> List Punch
sort punchList =
    punchList
        |> List.sortBy (\x -> ( x.commPk, x.mcPk, x.tag ))


statusDecoder : D.Decoder Status
statusDecoder =
    D.oneOf
        [ D.string
            |> D.andThen
                (\str ->
                    D.succeed <|
                        statusFromString str
                )
        , D.null OS
        ]


statusFromString : String -> Status
statusFromString str =
    case str of
        "OK" ->
            OK

        "PA" ->
            PA

        "PB" ->
            PB

        _ ->
            OS
