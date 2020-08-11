module Update exposing (update)

import Api
import Data.Punch exposing (Punch)
import Dict exposing (Dict)
import Http
import Json.Encode as E
import Messages exposing (..)
import Model exposing (Model)
import Ports
import Svg.Attributes exposing (z)
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

        GotPunchList punchList ->
            mc |> setPunchListTo punchList

        GotApiResult apiResult ->
            mc |> handleApiResult apiResult

        DecodeError err ->
            mc

        -- User Interaction
        PunchItemPressed punch ->
            if model.selectedPunch == Just punch.id then
                mc |> unSelectPunch

            else
                mc
                    |> selectPunch punch


setPunchListTo : List Punch -> MC -> MC
setPunchListTo punchList ( m, c ) =
    ( { m | punch = List.foldl (\punch dict -> Dict.insert punch.id punch dict) Dict.empty punchList }, c )


unSelectPunch : MC -> MC
unSelectPunch ( m, c ) =
    ( { m | selectedPunch = Nothing }, c )


selectPunch : Punch -> MC -> MC
selectPunch punch ( m, c ) =
    ( { m | selectedPunch = Just punch.id }, c )


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
        GotPunchDetails id result ->
            let
                updater punch =
                    punch
            in
            ( { m
                | punch = Dict.update id (Maybe.map updater) m.punch
              }
            , c
            )
