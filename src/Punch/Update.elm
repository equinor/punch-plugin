module Punch.Update exposing (update)

import Api
import Punch exposing (Punch)
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
            if model.selectedPunch == Just punch then
                mc
                    |> closeDropDowns
                    |> unSelectPunch

            else
                mc
                    |> closeDropDowns
                    |> selectPunch punch

        NeverHappens ->
            ( model, createEvent "" E.null )

        -- Form
        DescriptionFieldLostFocus punch ->
            case model.selectedPunch of
                Nothing ->
                    mc

                Just selected ->
                    if punch.description == selected.description then
                        mc

                    else
                        mc
                            |> apiRequest [ Api.updateDescription punch ]

        DescriptionFieldInput punch str ->
            let
                updater p =
                    { p | description = str }
            in
            ( { model | punch = Dict.update punch.id (Maybe.map updater) model.punch }
            , Cmd.none
            )

        DropDownPressed dropDown ->
            if model.dropDown == dropDown then
                ( { model | dropDown = NoDropDown }, Cmd.none )

            else
                ( { model | dropDown = dropDown }, Cmd.none )
                    |> (case dropDown of
                            NoDropDown ->
                                identity

                            CategoryDropDown ->
                                getCategories

                            RaisedByDropDown ->
                                getOrganizations

                            Types.ClearingByDropDown ->
                                getOrganizations
                       )

        DropDownItemPressed punch item ->
            let
                updated =
                    case model.dropDown of
                        NoDropDown ->
                            punch

                        CategoryDropDown ->
                            { punch
                                | status =
                                    if item.code == "PA" then
                                        PA

                                    else
                                        PB
                            }

                        RaisedByDropDown ->
                            { punch | raisedByOrg = item.description }

                        ClearingByDropDown ->
                            { punch | clearingByOrg = item.description }
            in
            ( { model
                | punch = Dict.insert punch.id updated model.punch
                , dropDown = NoDropDown
              }
            , Cmd.none
            )
                |> (case model.dropDown of
                        NoDropDown ->
                            identity

                        CategoryDropDown ->
                            apiRequest [ Api.setCategory punch item ]

                        Types.RaisedByDropDown ->
                            apiRequest [ Api.setRaisedBy punch item ]

                        Types.ClearingByDropDown ->
                            apiRequest [ Api.setClearingBy punch item ]
                   )


getOrganizations : MC -> MC
getOrganizations ( m, c ) =
    case m.organizations of
        Loaded _ ->
            ( m, c )

        _ ->
            ( { m | organizations = Loading }, c )
                |> apiRequest [ Api.organizations ]


getCategories : MC -> MC
getCategories ( m, c ) =
    case m.categories of
        Loaded _ ->
            ( m, c )

        _ ->
            ( { m | categories = Loading }, c )
                |> apiRequest [ Api.categories ]


setPunchListTo : List Punch -> MC -> MC
setPunchListTo punchList ( m, c ) =
    ( { m | punch = List.foldl (\punch dict -> Dict.insert punch.id punch dict) Dict.empty punchList }, c )


unSelectPunch : MC -> MC
unSelectPunch ( m, c ) =
    ( { m | selectedPunch = Nothing }, c )


selectPunch : Punch -> MC -> MC
selectPunch punch ( m, c ) =
    ( { m | selectedPunch = Just punch }, c )


closeDropDowns : MC -> MC
closeDropDowns ( m, c ) =
    ( { m | dropDown = NoDropDown }, c )


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
    ( { m
        | requests = Dict.insert nextRef requests m.requests
        , errorMsg = ""
      }
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

        PunchDescriptionResult punch result ->
            case result of
                Ok _ ->
                    ( m, c )

                Err err ->
                    ( { m | errorMsg = "Error changing description" }, c )

        SetRaisedByResult originalPunch result ->
            case result of
                Ok _ ->
                    ( m, c )

                Err err ->
                    let
                        updater punch =
                            { punch | raisedByOrg = originalPunch.raisedByOrg }
                    in
                    ( { m
                        | punch =
                            Dict.update originalPunch.id (Maybe.map updater) m.punch
                        , errorMsg = "Error changing raisedByOrg"
                      }
                    , c
                    )

        SetClearingByResult originalPunch result ->
            case result of
                Ok _ ->
                    ( m, c )

                Err err ->
                    let
                        updater punch =
                            { punch | clearingByOrg = originalPunch.clearingByOrg }
                    in
                    ( { m
                        | punch =
                            Dict.update originalPunch.id (Maybe.map updater) m.punch
                        , errorMsg = "Error changing clearingByOrg"
                      }
                    , c
                    )

        SetCategoryResult originalPunch result ->
            case result of
                Ok _ ->
                    ( m, c )

                Err err ->
                    let
                        updater punch =
                            { punch | status = originalPunch.status }
                    in
                    ( { m
                        | punch =
                            Dict.update originalPunch.id (Maybe.map updater) m.punch
                        , errorMsg = "Error changing category"
                      }
                    , c
                    )

        GotOrganizations result ->
            case result of
                Ok organizations ->
                    ( { m | organizations = Loaded organizations }, c )

                Err err ->
                    ( { m | organizations = DataError, errorMsg = "Error getting organizations" }, c )

        GotCategories result ->
            case result of
                Ok categories ->
                    ( { m | categories = Loaded categories }, c )

                Err err ->
                    ( { m | categories = DataError, errorMsg = "Error getting categories" }, c )
