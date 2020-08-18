module Punch exposing (ApiPunch, Attachment, CreatePunch, Dict, Punch, PunchLists, Status, apiDecoder, attachmentDecoder, decoder, encoder, filterByTimeFrame, initialCreate, sort, webApiDecoder)

import Date exposing (Date)
import Dict as CoreDict
import Equinor.Data.Procosys.Status as Status
import Equinor.Types exposing (..)
import Iso8601
import Json.Decode as D
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, required)
import Json.Encode as E
import Punch.Checklist exposing (Checklist)
import Punch.Types exposing (..)
import Time exposing (Posix)


type alias Dict =
    CoreDict.Dict String (List Punch)


type alias Status =
    Status.Status


type alias Punch =
    { id : Int
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
    , sortingDescription : String
    , attachmentCount : Int
    , apiPunch : WebData ApiPunch
    , attachments : WebData (List Attachment)
    }


type alias CreatePunch =
    { checklistId : Int
    , description : String
    , categoryId : Int
    , raisedByOrg : Int
    , clearingByOrg : Int
    , categoryDescription : String
    , raisedByDescription : String
    , clearingByDescription : String
    , checklists : WebData (List Checklist)
    }


initialCreate : CreatePunch
initialCreate =
    { checklistId = -1
    , description = ""
    , categoryId = -1
    , raisedByOrg = -1
    , clearingByOrg = -1
    , categoryDescription = ""
    , raisedByDescription = ""
    , clearingByDescription = ""
    , checklists = NotLoaded
    }


type alias ApiPunch =
    { id : Int
    , tagNo : String
    , tagDescription : String
    , description : String
    , clearedAt : String
    , clearedByFirstName : String
    , clearedByLastName : String
    , verifiedAt : String
    , verifiedByFirstName : String
    , verifiedByLastName : String
    , isRestrictedForUser : Bool
    , statusControlledBySwcr : Bool
    , status : Status
    , raisedByOrg : String
    , clearingByOrg : String
    , typeDescription : String
    , sortingDescription : String
    , attachmentCount : Int
    }


type alias Attachment =
    { id : Int
    , uri : String
    , title : String
    , mimeType : String
    , thumbnailAsBase64 : String
    , hasFile : Bool
    }


webApiDecoder : D.Decoder ApiPunch
webApiDecoder =
    D.succeed ApiPunch
        |> required "Id" D.int
        |> required "TagNo" D.string
        |> required "TagDescription" nullString
        |> required "Description" nullString
        |> required "ClearedAt" nullString
        |> required "ClearedByFirstName" nullString
        |> required "ClearedByLastName" nullString
        |> required "VerifiedAt" nullString
        |> required "VerifiedByFirstName" nullString
        |> required "VerifiedByLastName" nullString
        |> required "IsRestrictedForUser" D.bool
        |> required "StatusControlledBySwcr" D.bool
        |> required "Status" Status.decoder
        |> required "RaisedByDescription" nullString
        |> required "ClearingByDescription" nullString
        |> required "TypeDescription" nullString
        |> required "Sorting" nullString
        |> required "AttachmentCount" D.int


attachmentDecoder : D.Decoder Attachment
attachmentDecoder =
    D.succeed Attachment
        |> required "Id" D.int
        |> required "Uri" nullString
        |> required "Title" nullString
        |> required "MimeType" nullString
        |> required "ThumbnailAsBase64" nullString
        |> required "HasFile" D.bool


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
        [ ( "id", E.int punch.id )
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
        , ( "sortingDescription", E.string punch.sortingDescription )
        , ( "attachmentCount", E.int punch.attachmentCount )
        ]


decoder : D.Decoder Punch
decoder =
    D.succeed Punch
        |> required "id" D.int
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
        |> optional "sortingDescription" D.string ""
        |> optional "attachmentCount" D.int 0
        |> hardcoded NotLoaded
        |> hardcoded NotLoaded


apiDecoder : D.Decoder Punch
apiDecoder =
    D.succeed Punch
        |> required "PunchListItemNo" D.int
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
        |> required "PunchListSorting__Description" nullString
        |> required "AttachmentCount" D.int
        |> hardcoded NotLoaded
        |> hardcoded NotLoaded


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
