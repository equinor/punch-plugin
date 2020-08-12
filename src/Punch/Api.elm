module Punch.Api exposing (categories, clientId, organizations, setCategory, setClearingBy, setRaisedBy, updateDescription)

import Punch exposing (Punch)
import Http
import Json.Decode as D
import Json.Encode as E
import Punch.Messages exposing (..)
import Punch.Types  as Types exposing (..)
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
                    [ ( "PunchItemId", punch.id |> String.toInt |> Maybe.withDefault 0 |> E.int )
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
                    [ ( "PunchItemId", originalPunch.id |> String.toInt |> Maybe.withDefault 0 |> E.int )
                    , ( "CategoryId", E.int selectItem.id )
                    ]
                )
        , expect =
            Http.expectWhatever
                (GotApiResult << SetCategoryResult originalPunch)
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
                    [ ( "PunchItemId", originalPunch.id |> String.toInt |> Maybe.withDefault 0 |> E.int )
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
                    [ ( "PunchItemId", originalPunch.id |> String.toInt |> Maybe.withDefault 0 |> E.int )
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
