module Punch.Update exposing (update)

import Dict exposing (Dict)
import Equinor.Data.Procosys.Status as Status exposing (Status(..))
import Equinor.Types exposing (..)
import Http
import Json.Encode as E
import Punch exposing (Punch)
import Punch.Api as Api
import Punch.Messages exposing (..)
import Punch.Model exposing (Model)
import Punch.Ports as Ports
import Punch.Types as Types exposing (..)
import Svg.Attributes exposing (z)


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
            let
                nextDict =
                    List.foldl
                        (\p dict ->
                            Dict.insert p.id
                                (Dict.get p.id model.punch |> Maybe.withDefault p)
                                dict
                        )
                        Dict.empty
                        punchList
            in
            ( { model | punch = nextDict }, Cmd.none )

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
                    |> getDetails punch

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

                            Types.TypeDropDown ->
                                getTypes

                            Types.SortingDropDown ->
                                getSorts
                       )

        CloseDropDownButtonPressed ->
            mc |> closeDropDowns

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

                        TypeDropDown ->
                            { punch | typeDescription = item.description }

                        SortingDropDown ->
                            { punch | sortingDescription = item.description }
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

                        Types.TypeDropDown ->
                            apiRequest [ Api.setType punch item ]

                        Types.SortingDropDown ->
                            apiRequest [ Api.setSorting punch item ]
                   )

        ClearPunchButtonPressed punch ->
            mc |> apiRequest [ Api.clear punch ]

        UnclearPunchButtonPressed punch ->
            mc |> apiRequest [ Api.unClear punch ]

        VerifyPunchButtonPressed punch ->
            mc |> apiRequest [ Api.verify punch ]

        UnverifyPunchButtonPressed punch ->
            mc |> apiRequest [ Api.unVerify punch ]


getOrganizations : MC -> MC
getOrganizations ( m, c ) =
    case m.organizations of
        Loaded _ _ ->
            ( m, c )

        _ ->
            ( { m | organizations = Loading "" Nothing }, c )
                |> apiRequest [ Api.organizations ]


getTypes : MC -> MC
getTypes ( m, c ) =
    case m.types of
        Loaded _ _ ->
            ( m, c )

        _ ->
            ( { m | types = Loading "" Nothing }, c )
                |> apiRequest [ Api.types ]


getSorts : MC -> MC
getSorts ( m, c ) =
    case m.sorts of
        Loaded _ _ ->
            ( m, c )

        _ ->
            ( { m | sorts = Loading "" Nothing }, c )
                |> apiRequest [ Api.sorts ]


getCategories : MC -> MC
getCategories ( m, c ) =
    case m.categories of
        Loaded _ _ ->
            ( m, c )

        _ ->
            ( { m | categories = Loading "" Nothing }, c )
                |> apiRequest [ Api.categories ]


getDetails : Punch -> MC -> MC
getDetails punch =
    apiRequest [ Api.details punch ]


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
        GotPunchDetails oldPunch result ->
            let
                updater punch =
                    { punch
                        | apiPunch =
                            case result of
                                Ok apiPunch ->
                                    Loaded "" apiPunch

                                Err err ->
                                    DataError "" Nothing
                    }
            in
            ( { m | punch = Dict.update oldPunch.id (Maybe.map updater) m.punch }
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

        SetTypeResult originalPunch result ->
            case result of
                Ok _ ->
                    ( m, c )

                Err err ->
                    let
                        updater punch =
                            { punch | typeDescription = originalPunch.typeDescription }
                    in
                    ( { m
                        | punch =
                            Dict.update originalPunch.id (Maybe.map updater) m.punch
                        , errorMsg = "Error changing type"
                      }
                    , c
                    )

        SetSortingResult originalPunch result ->
            case result of
                Ok _ ->
                    ( m, c )

                Err err ->
                    let
                        updater punch =
                            { punch | sortingDescription = originalPunch.sortingDescription }
                    in
                    ( { m
                        | punch =
                            Dict.update originalPunch.id (Maybe.map updater) m.punch
                        , errorMsg = "Error changing sorting"
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
                    ( { m | organizations = Loaded "" organizations }, c )

                Err err ->
                    ( { m | organizations = DataError "" Nothing, errorMsg = "Error getting organizations" }, c )

        GotCategories result ->
            case result of
                Ok categories ->
                    ( { m | categories = Loaded "" categories }, c )

                Err err ->
                    ( { m | categories = DataError "" Nothing, errorMsg = "Error getting categories" }, c )

        GotTypes result ->
            case result of
                Ok types ->
                    ( { m | types = Loaded "" types }, c )

                Err err ->
                    ( { m | types = DataError "" Nothing, errorMsg = "Error getting types" }, c )

        GotSorts result ->
            case result of
                Ok sorts ->
                    ( { m | sorts = Loaded "" sorts }, c )

                Err err ->
                    ( { m | sorts = DataError "" Nothing, errorMsg = "Error getting sorts" }, c )

        ClearResult punch result ->
            ( m, c ) |> getDetails punch

        UnclearResult punch result ->
            ( m, c ) |> getDetails punch

        VerifyResult punch result ->
            ( m, c ) |> getDetails punch

        UnverifyResult punch result ->
            ( m, c ) |> getDetails punch
