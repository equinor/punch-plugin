module Update exposing (update)

import Api
import Data.Checklist exposing (Checklist)
import Dict exposing (Dict)
import Json.Encode as E
import Messages exposing (..)
import Model exposing (Model)
import Ports
import Types exposing (..)


type alias MC =
    ( Model, Cmd Msg )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        mc =
            ( model, Cmd.none )
    in
    case msg of
        NoOp ->
            mc

        -- Data Handling
        GotToken tokenSuccess ->
            mc |> sendRequestsWaitingForToken tokenSuccess

        GotChecklists checklists ->
            mc |> setChecklistsTo checklists

        GotApiResult apiResult ->
            mc |> handleApiResult apiResult

        DecodeError err ->
            mc

        -- User Interaction
        ChecklistPressed checklist ->
            if model.selectedChecklist == Just checklist.id then
                mc |> unSelectChecklist

            else
                mc
                    |> selectChecklist checklist
                    |> getChecklistDetails checklist


setChecklistsTo : List Checklist -> MC -> MC
setChecklistsTo checklists ( m, c ) =
    ( { m | checklists = List.foldl (\checklist dict -> Dict.insert checklist.id checklist dict) Dict.empty checklists }, c )


unSelectChecklist : MC -> MC
unSelectChecklist ( m, c ) =
    ( { m | selectedChecklist = Nothing }, c )


selectChecklist : Checklist -> MC -> MC
selectChecklist checklist ( m, c ) =
    ( { m | selectedChecklist = Just checklist.id }, c )


getChecklistDetails : Checklist -> MC -> MC
getChecklistDetails checklist ( m, c ) =
    let
        updater cl =
            { cl | details = Loading }
    in
    ( { m | checklists = Dict.update checklist.id (Maybe.map updater) m.checklists }, c )
        |> apiRequest [ Api.checklistDetails checklist ]


apiRequest : List (String -> String -> Cmd Msg) -> MC -> MC
apiRequest requests ( m, c ) =
    let
        highestRefNo =
            m.requests
                |> Dict.keys
                |> List.maximum
                |> Maybe.withDefault 0

        nextRef =
            highestRefNo + 1
    in
    ( { m | requests = Dict.insert nextRef requests m.requests }
    , Cmd.batch
        [ c
        , createEvent "getToken"
            (E.object
                [ ( "clientId", E.string Api.clientId )
                , ( "refNo", E.int nextRef )
                ]
            )
        ]
    )


sendRequestsWaitingForToken : TokenSuccess -> MC -> MC
sendRequestsWaitingForToken tokenSuccess ( m, c ) =
    let
        maybeDoReq =
            Dict.get tokenSuccess.refNo m.requests
    in
    ( { m
        | requests = Dict.remove tokenSuccess.refNo m.requests
      }
    , case maybeDoReq of
        Just doReqList ->
            doReqList
                |> List.map (\fn -> fn m.procosysPlantId tokenSuccess.token)
                |> Cmd.batch

        Nothing ->
            Cmd.none
    )


createEvent : String -> E.Value -> Cmd Msg
createEvent topic payload =
    Ports.toJs
        (E.object
            [ ( "topic", E.string topic )
            , ( "payload", payload )
            ]
        )


handleApiResult : ApiResult -> MC -> MC
handleApiResult apiResult ( m, c ) =
    case apiResult of
        GotChecklistDetails id result ->
            let
                updater checklist =
                    { checklist
                        | details =
                            case result of
                                Ok details ->
                                    Loaded details

                                Err err ->
                                    DataError
                    }
            in
            ( { m | checklists = Dict.update id (Maybe.map updater) m.checklists }, c )
