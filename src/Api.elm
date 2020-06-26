module Api exposing (checklistDetails, clientId)

import Data.Checklist as Checklist
import Http
import Json.Decode as D
import Json.Encode as E
import Messages exposing (..)
import Types exposing (..)
import Url.Builder exposing (QueryParameter, int, string)


type Environment
    = Development
    | Production


environment : Environment
environment =
    Production


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


checklistDetails : Checklist.Checklist -> String -> String -> Cmd Msg
checklistDetails checklist plantId token =
    Http.request
        { method = "GET"
        , url =
            url
                [ "Checklist"
                , case checklist.group of
                    Checklist.CPCL ->
                        "Comm"

                    _ ->
                        "MC"
                ]
                [ string "plantId" plantId
                , int "checklistId" checklist.id
                , apiVersion
                ]
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , body = Http.emptyBody
        , expect =
            Http.expectJson
                (GotApiResult << GotChecklistDetails checklist.id)
                Checklist.detailsApiDecoder
        , timeout = Nothing
        , tracker = Nothing
        }
