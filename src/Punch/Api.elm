module Punch.Api exposing (attachment, attachments, categories, clear, clientId, deleteAttachment, details, organizations, setCategory, setClearingBy, setRaisedBy, setSorting, setType, sorts, types, unClear, unVerify, updateDescription, verify)

import Http
import Json.Decode as D
import Json.Encode as E
import Punch exposing (Punch)
import Punch.Messages exposing (..)
import Punch.Types as Types exposing (..)
import Url.Builder exposing (QueryParameter, int, string)


type Environment
    = Development
    | Production


environment : Environment
environment =
    Development


clientId : String
clientId =
    "47641c40-0135-459b-8ab4-459e68dc8d08/.default"


apiVersion : QueryParameter
apiVersion =
    string "api-version" "4.1"


url : List String -> List QueryParameter -> String
url paths queryParams =
    case environment of
        Development ->
            Url.Builder.crossOrigin
                "https://procosyswebapiqp.equinor.com"
                ("api" :: paths)
                (apiVersion :: queryParams)

        Production ->
            Url.Builder.crossOrigin
                "https://procosyswebapi.equinor.com"
                ("api" :: paths)
                (apiVersion :: queryParams)


updateDescription : Punch -> String -> String -> Cmd Msg
updateDescription punch plantId token =
    Http.request
        { method = "PUT"
        , url =
            url
                [ "PunchListItem"
                , "SetDescription"
                ]
                [ string "plantId" plantId
                , apiVersion
                ]
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , body =
            Http.jsonBody
                (E.object
                    [ ( "PunchItemId", E.int punch.id )
                    , ( "Description", E.string punch.description )
                    ]
                )
        , expect =
            Http.expectWhatever
                (GotApiResult << PunchDescriptionResult punch)
        , timeout = Nothing
        , tracker = Nothing
        }


setCategory : Punch -> SelectItem -> String -> String -> Cmd Msg
setCategory originalPunch selectItem plantId token =
    Http.request
        { method = "PUT"
        , url =
            url
                [ "PunchListItem"
                , "SetCategory"
                ]
                [ string "plantId" plantId
                , apiVersion
                ]
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , body =
            Http.jsonBody
                (E.object
                    [ ( "PunchItemId", E.int originalPunch.id )
                    , ( "CategoryId", E.int selectItem.id )
                    ]
                )
        , expect =
            Http.expectWhatever
                (GotApiResult << SetCategoryResult originalPunch)
        , timeout = Nothing
        , tracker = Nothing
        }


setType : Punch -> SelectItem -> String -> String -> Cmd Msg
setType originalPunch selectItem plantId token =
    Http.request
        { method = "PUT"
        , url =
            url
                [ "PunchListItem"
                , "SetType"
                ]
                [ string "plantId" plantId
                , apiVersion
                ]
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , body =
            Http.jsonBody
                (E.object
                    [ ( "PunchItemId", E.int originalPunch.id )
                    , ( "TypeId", E.int selectItem.id )
                    ]
                )
        , expect =
            Http.expectWhatever
                (GotApiResult << SetTypeResult originalPunch)
        , timeout = Nothing
        , tracker = Nothing
        }


setSorting : Punch -> SelectItem -> String -> String -> Cmd Msg
setSorting originalPunch selectItem plantId token =
    Http.request
        { method = "PUT"
        , url =
            url
                [ "PunchListItem"
                , "SetSorting"
                ]
                [ string "plantId" plantId
                , apiVersion
                ]
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , body =
            Http.jsonBody
                (E.object
                    [ ( "PunchItemId", E.int originalPunch.id )
                    , ( "SortingId", E.int selectItem.id )
                    ]
                )
        , expect =
            Http.expectWhatever
                (GotApiResult << SetSortingResult originalPunch)
        , timeout = Nothing
        , tracker = Nothing
        }


setRaisedBy : Punch -> SelectItem -> String -> String -> Cmd Msg
setRaisedBy originalPunch selectItem plantId token =
    Http.request
        { method = "PUT"
        , url =
            url
                [ "PunchListItem"
                , "SetRaisedBy"
                ]
                [ string "plantId" plantId
                , apiVersion
                ]
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , body =
            Http.jsonBody
                (E.object
                    [ ( "PunchItemId", E.int originalPunch.id )
                    , ( "RaisedByOrganizationId", E.int selectItem.id )
                    ]
                )
        , expect =
            Http.expectWhatever
                (GotApiResult << SetRaisedByResult originalPunch)
        , timeout = Nothing
        , tracker = Nothing
        }


setClearingBy : Punch -> SelectItem -> String -> String -> Cmd Msg
setClearingBy originalPunch selectItem plantId token =
    Http.request
        { method = "PUT"
        , url =
            url
                [ "PunchListItem"
                , "SetClearingBy"
                ]
                [ string "plantId" plantId
                , apiVersion
                ]
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , body =
            Http.jsonBody
                (E.object
                    [ ( "PunchItemId", E.int originalPunch.id )
                    , ( "ClearingByOrganizationId", E.int selectItem.id )
                    ]
                )
        , expect =
            Http.expectWhatever
                (GotApiResult << SetClearingByResult originalPunch)
        , timeout = Nothing
        , tracker = Nothing
        }


organizations : String -> String -> Cmd Msg
organizations plantId token =
    Http.request
        { method = "GET"
        , url =
            url
                [ "PunchListItem"
                , "Organizations"
                ]
                [ string "plantId" plantId
                , apiVersion
                ]
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , body = Http.emptyBody
        , expect =
            Http.expectJson
                (GotApiResult << GotOrganizations)
                (D.list Types.selectItemDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


categories : String -> String -> Cmd Msg
categories plantId token =
    Http.request
        { method = "GET"
        , url =
            url
                [ "PunchListItem"
                , "Categories"
                ]
                [ string "plantId" plantId
                , apiVersion
                ]
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , body = Http.emptyBody
        , expect =
            Http.expectJson
                (GotApiResult << GotCategories)
                (D.list Types.selectItemDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


types : String -> String -> Cmd Msg
types plantId token =
    Http.request
        { method = "GET"
        , url =
            url
                [ "PunchListItem"
                , "Types"
                ]
                [ string "plantId" plantId
                , apiVersion
                ]
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , body = Http.emptyBody
        , expect =
            Http.expectJson
                (GotApiResult << GotTypes)
                (D.list Types.selectItemDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


sorts : String -> String -> Cmd Msg
sorts plantId token =
    Http.request
        { method = "GET"
        , url =
            url
                [ "PunchListItem"
                , "Sorts"
                ]
                [ string "plantId" plantId
                , apiVersion
                ]
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , body = Http.emptyBody
        , expect =
            Http.expectJson
                (GotApiResult << GotSorts)
                (D.list Types.selectItemDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


clear : Punch -> String -> String -> Cmd Msg
clear punch plantId token =
    Http.request
        { method = "POST"
        , url =
            url
                [ "PunchListItem"
                , "Clear"
                ]
                [ string "plantId" plantId
                , apiVersion
                ]
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , body = Http.jsonBody (E.int punch.id)
        , expect =
            Http.expectWhatever
                (GotApiResult << ClearResult punch)
        , timeout = Nothing
        , tracker = Nothing
        }


unClear : Punch -> String -> String -> Cmd Msg
unClear punch plantId token =
    Http.request
        { method = "POST"
        , url =
            url
                [ "PunchListItem"
                , "Unclear"
                ]
                [ string "plantId" plantId
                , apiVersion
                ]
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , body = Http.jsonBody (E.int punch.id)
        , expect =
            Http.expectWhatever
                (GotApiResult << UnclearResult punch)
        , timeout = Nothing
        , tracker = Nothing
        }


verify : Punch -> String -> String -> Cmd Msg
verify punch plantId token =
    Http.request
        { method = "POST"
        , url =
            url
                [ "PunchListItem"
                , "Verify"
                ]
                [ string "plantId" plantId
                , apiVersion
                ]
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , body = Http.jsonBody (E.int punch.id)
        , expect =
            Http.expectWhatever
                (GotApiResult << VerifyResult punch)
        , timeout = Nothing
        , tracker = Nothing
        }


unVerify : Punch -> String -> String -> Cmd Msg
unVerify punch plantId token =
    Http.request
        { method = "POST"
        , url =
            url
                [ "PunchListItem"
                , "Unverify"
                ]
                [ string "plantId" plantId
                , apiVersion
                ]
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , body = Http.jsonBody (E.int punch.id)
        , expect =
            Http.expectWhatever
                (GotApiResult << UnverifyResult punch)
        , timeout = Nothing
        , tracker = Nothing
        }


details : Punch -> String -> String -> Cmd Msg
details punch plantId token =
    Http.request
        { method = "GET"
        , url =
            url
                [ "PunchListItem"
                ]
                [ string "plantId" plantId
                , int "punchItemId" punch.id
                , apiVersion
                ]
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , body = Http.emptyBody
        , expect =
            Http.expectJson
                (GotApiResult << GotPunchDetails punch)
                Punch.webApiDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


attachments : Punch -> String -> String -> Cmd Msg
attachments punch plantId token =
    Http.request
        { method = "GET"
        , url =
            url
                [ "PunchListItem"
                , "Attachments"
                ]
                [ string "plantId" plantId
                , int "punchItemId" punch.id
                , int "thumbnailSize" 100
                , apiVersion
                ]
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , body = Http.emptyBody
        , expect =
            Http.expectJson
                (GotApiResult << GotAttachments punch)
                (D.list Punch.attachmentDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


attachment : Punch -> Punch.Attachment -> String -> String -> Cmd Msg
attachment punch att plantId token =
    Http.request
        { method = "GET"
        , url =
            url
                [ "PunchListItem"
                , "Attachment"
                ]
                [ string "plantId" plantId
                , int "punchItemId" punch.id
                , int "attachmentId" att.id
                , apiVersion
                ]
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , body = Http.emptyBody
        , expect = Http.expectWhatever (GotApiResult << GotAttachment punch)
        , timeout = Nothing
        , tracker = Nothing
        }


deleteAttachment : Punch -> Punch.Attachment -> String -> String -> Cmd Msg
deleteAttachment punch att plantId token =
    Http.request
        { method = "DELETE"
        , url =
            url
                [ "PunchListItem"
                , "Attachment"
                ]
                [ string "plantId" plantId
                , apiVersion
                ]
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , body =
            Http.jsonBody <|
                E.object
                    [ ( "PunchItemId", E.int punch.id )
                    , ( "AttachmentId", E.int att.id )
                    ]
        , expect = Http.expectWhatever (GotApiResult << DeleteAttachmentResult punch att)
        , timeout = Nothing
        , tracker = Nothing
        }
