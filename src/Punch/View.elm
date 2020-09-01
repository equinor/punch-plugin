module Punch.View exposing (view)

import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onLoseFocus)
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Equinor.Component.SelectionList as SelectionList
import Equinor.Data.Procosys.Status as Status exposing (Status(..))
import Equinor.Icon as Icon
import Equinor.Palette as Palette exposing (kv, scaledInt)
import Equinor.Types exposing (..)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Punch exposing (Punch)
import Punch.Checklist exposing (Checklist)
import Punch.Messages exposing (Msg(..))
import Punch.Model exposing (Model, Popup(..))
import Punch.Types as Types exposing (..)
import String.Extra


view : Float -> Model -> Element Msg
view size model =
    el
        [ width fill
        , Font.size <| round size
        ]
    <|
        case model.popup of
            Just popup ->
                case popup of
                    DeleteAttachmentPopup punch attachment ->
                        deleteAttachmentPopup punch attachment

            Nothing ->
                case model.context of
                    CreateContext _ Nothing ->
                        renderSelectChecklist size model

                    CreateContext _ (Just checklistId) ->
                        renderCreatePunch size model checklistId

                    _ ->
                        renderPunchList size model


deleteAttachmentPopup : Punch -> Punch.Attachment -> Element Msg
deleteAttachmentPopup punch attachment =
    el [ width fill, padding 10 ] <|
        column
            [ width fill
            , Background.color Palette.white
            , Border.rounded 10
            , Border.width 1
            , clip
            ]
            [ el [ width fill, padding 10, Background.color Palette.blue, Font.color Palette.white, Font.center ] (text "Confirm delete")
            , paragraph [ padding 10, width fill, Font.center ] [ text "Are you sure you want to delete attachment?" ]
            , row [ width fill, spacing 10, padding 10 ]
                [ el [ width fill, padding 10, Border.width 1, Border.rounded 10, Font.center, Background.color Palette.blue, Font.color Palette.white, pointer, onClick CancelPopupPressed ] (text "Cancel")
                , el [ width fill, padding 10, Border.rounded 10, Border.width 1, Font.center, Background.color Palette.blue, Font.color Palette.white, pointer, onClick <| ConfirmDeleteAttachment punch attachment ] (text "Confirm")
                ]
            ]


renderSelectChecklist : Float -> Model -> Element Msg
renderSelectChecklist size model =
    column [ width fill, padding 4, spacing 6 ]
        [ el [ Font.bold, centerX ] (text "Select checklist for your punch")
        , SelectionList.webDataSelectionList (SelectionList.selectionList (selectChecklistItem size)) model.currentCreate.checklists
        ]


selectChecklistItem : Float -> Checklist -> ( String, Element Msg )
selectChecklistItem size checklist =
    ( checklist.id |> String.fromInt
    , el [ width fill, paddingXY 0 1 ] <|
        row
            [ width fill
            , padding 10
            , spacing 10
            , Border.widthEach { left = 0, top = 0, right = 0, bottom = 1 }
            , Border.color Palette.mistBlue
            , mouseOver [ Background.color Palette.mistBlue ]
            , pointer
            , onClick <| ChecklistSelected checklist.id
            ]
            [ paragraph [ Font.size (scaledInt size -4), width <| px <| round <| size * 4 ] [ text checklist.responsibleCode ]
            , paragraph [ Font.size (scaledInt size -2), width fill ] [ text checklist.formularType ]
            , paragraph [ alignRight, Font.size (scaledInt size -2) ] [ Status.toString checklist.status |> text ]
            ]
    )


renderCreatePunch : Float -> Model -> Int -> Element Msg
renderCreatePunch size model checklistId =
    let
        dd =
            dropDown size False -1 model
    in
    column [ width fill, height fill, spacing 6, padding 4 ]
        [ el [ Font.bold, centerX ] (text "New punch")
        , Input.multiline [ width fill, Font.size <| round size ]
            { label = Input.labelHidden ""
            , onChange = CreatePunchDescriptionFieldInput
            , placeholder = Just <| Input.placeholder [] (text "No Description")
            , spellcheck = True
            , text = model.currentCreate.description
            }
        , dd CategoryDropDown model.currentCreate.categoryDescription .categories
        , dd RaisedByDropDown model.currentCreate.raisedByDescription .organizations
        , dd ClearingByDropDown model.currentCreate.clearingByDescription .organizations
        , el [ alignRight ] <| createButton size model checklistId
        ]


createButton : Float -> Model -> Int -> Element Msg
createButton size model checklistId =
    let
        x =
            model.currentCreate

        disabledMessage =
            if String.isEmpty x.description then
                Just "Missing description"

            else if x.categoryId < 0 then
                Just "Category must be set"

            else if x.raisedByOrg < 0 then
                Just "RaisedBy must be set"

            else if x.clearingByOrg < 0 then
                Just "ClearingBy must be set"

            else
                Nothing
    in
    signButton size
        "Create"
        disabledMessage
        (SubmitCreatedPunchButtonPressed checklistId)


renderPunchList : Float -> Model -> Element Msg
renderPunchList size model =
    let
        textToHighlight =
            case model.context of
                SearchContext str _ ->
                    str

                _ ->
                    ""

        punchList =
            case model.context of
                NoContext ->
                    []

                TagContext tagNo ->
                    Dict.filter (\_ p -> p.tag == tagNo) model.punch
                        |> Dict.values

                McContext mcPk ->
                    Dict.filter (\_ p -> p.mcPk == mcPk) model.punch
                        |> Dict.values

                CommContext commPk ->
                    Dict.filter (\_ p -> p.commPk == commPk) model.punch
                        |> Dict.values

                CreateContext _ _ ->
                    []

                CreatedContext id ->
                    Dict.get id model.punch
                        |> Maybe.map (\p -> [ p ])
                        |> Maybe.withDefault []

                SearchContext _ list ->
                    list
    in
    punchList
        |> List.map (renderPunchListItem size textToHighlight model)
        |> Keyed.column
            [ width fill
            , height fill
            , scrollbarY
            , Background.color Palette.mistBlue
            , spacing 1
            ]


spacer =
    el
        [ width fill
        , Border.widthEach { top = 1, left = 0, right = 0, bottom = 0 }
        , Border.color Palette.mistBlue
        , Border.dashed
        ]
        none


renderPunchListItem : Float -> String -> Model -> Punch -> ( String, Element Msg )
renderPunchListItem size textToHighlight model item =
    let
        readOnly =
            case item.apiPunch of
                Loaded _ x ->
                    x.isRestrictedForUser || x.clearedAt /= ""

                _ ->
                    True

        colors =
            case item.status of
                PA ->
                    Palette.combination Palette.white Palette.red

                PB ->
                    Palette.combination Palette.white Palette.yellow

                OK ->
                    Palette.combination Palette.white Palette.alphaMossGreen

                _ ->
                    Palette.combination Palette.white Palette.grey

        isSelected =
            case model.selectedPunch of
                Just selected ->
                    selected.id == item.id

                Nothing ->
                    False

        color =
            Palette.white

        statusBadge =
            el
                (colors
                    [ paddingXY 2 1
                    , Border.rounded 4
                    , Font.size (scaledInt size -4)
                    ]
                )
                (item.status |> Status.toString |> text)

        icon =
            el
                [ width (px 30)
                , height (px 30)
                , inFront <| statusBadge
                , clip
                ]
            <|
                html <|
                    iconFromCategory ""

        toppLinje =
            row [ spacing 10, Font.size <| scaledInt size -3, width fill ]
                [ row []
                    [ el (colors [ paddingXY 2 1, Border.rounded 4 ]) (item.status |> Status.toString |> text)
                    , el (Palette.combination Palette.white Palette.blue [ paddingXY 2 1, Border.rounded 4 ]) (String.left 2 item.commPk |> text)
                    , el (Palette.combination Palette.white Palette.green [ paddingXY 2 1, Border.rounded 4 ]) (item.location |> text)
                    ]
                , el [ width fill, clip ] (text item.typeDescription)
                , el []
                    (String.left 3 item.raisedByOrg
                        ++ " -> "
                        ++ String.left 3 item.clearingByOrg
                        |> String.toLower
                        |> String.Extra.toTitleCase
                        |> text
                    )
                ]

        shortDescription =
            item.description
                |> String.lines
                |> List.take 2
                |> List.map (\rowText -> row [] (Palette.highlight textToHighlight rowText))
                |> column [ width fill, clip ]

        tagNo =
            paragraph [ Font.size <| scaledInt size -1, width fill, Font.color Palette.mossGreen ] [ text item.tag ]

        tagBeskrivelse p =
            row [ width fill, clip ]
                [ el
                    [ width (px 20)
                    , height (px 20)
                    ]
                  <|
                    html <|
                        Icon.tag "" "none"
                , el [ Font.size <| scaledInt size -3, width fill, clip ] (text p.tagDescription)
                ]
    in
    ( String.fromInt item.id
    , column
        [ width fill
        , Background.color <|
            if isSelected then
                Palette.mistBlue

            else
                Palette.white
        ]
        [ column
            [ width fill
            , onClick <| PunchItemPressed item
            , onContextMenu <| ContextMenuPressedOnPunch item
            , padding (round <| size / 2)
            , pointer
            ]
            [ toppLinje
            , shortDescription
            , {- case model.context of
                 TagContext ->
                     none

                 _ ->
              -}
              tagBeskrivelse item
            ]
        , if isSelected then
            el [ width fill, padding (round <| size / 2) ] <|
                column
                    [ paddingXY (round <| size / 2) 0
                    , width fill
                    , height fill
                    , Background.color Palette.white
                    , onClick NoOp
                    , Border.rounded 4
                    , padding 4
                    , spacing 6
                    ]
                    [ if isSelected then
                        renderDescription textToHighlight size readOnly item

                      else
                        none

                    --, renderComments size item
                    , renderAttachments size model readOnly item
                    , renderDetails size model readOnly item
                    , renderSignatures size item
                    , if String.isEmpty model.errorMsg then
                        none

                      else
                        paragraph [ width fill, Background.color Palette.alphaYellow, padding 6 ] [ text model.errorMsg ]
                    ]

          else
            none
        ]
    )


renderDescription : String -> Float -> Bool -> Punch -> Element Msg
renderDescription textToHighlight size readOnly punch =
    column [ width fill ]
        [ el
            [ width fill
            , Background.color Palette.blue
            , Font.color Palette.white
            , padding 8
            , Font.size (scaledInt size -1)
            ]
            (text "Description")
        , if readOnly then
            punch.description
                |> String.lines
                |> List.map
                    (\txt ->
                        if txt == "" then
                            text " "

                        else
                            paragraph [] (Palette.highlight textToHighlight txt)
                    )
                |> column [ width fill ]

          else
            Input.multiline
                [ width fill
                , onLoseFocus <| DescriptionFieldLostFocus punch
                ]
                { label = Input.labelHidden ""
                , onChange = DescriptionFieldInput punch
                , placeholder = Just <| Input.placeholder [] (text "No Description")
                , spellcheck = True
                , text = punch.description
                }
        ]


renderAttachments : Float -> Model -> Bool -> Punch -> Element Msg
renderAttachments size model readOnly punch =
    column [ width fill ]
        [ row
            [ width fill
            , Background.color Palette.blue
            , Font.color Palette.white
            , Font.size (scaledInt size -1)
            , paddingXY 8 4
            ]
            [ text <| "Attachments (" ++ String.fromInt punch.attachmentCount ++ ")"
            , el [ padding 6, alignRight, Border.width 1, Border.color Palette.white, Border.rounded 4, pointer, onClick <| NewAttachmentButtonPressed punch ] (text "Add new")
            ]
        , case model.currentAttachment of
            Just file ->
                row [ width fill, spacing <| round size, padding 4 ]
                    [ image [ width <| px <| round <| size * 4 ] { description = "Thumbnail of new attachment", src = file.uri }
                    , Input.text [ width fill, Input.focusedOnLoad, onEnterKey <| AddUploadedAttachmentToPunch punch ]
                        { label = Input.labelHidden "Name"
                        , onChange = FileNameInputChanged
                        , placeholder = Just (Input.placeholder [] (text "Enter name..."))
                        , text = file.name
                        }
                    , el [ padding 6, alignRight, Border.width 1, Background.color Palette.green, Font.color Palette.white, Border.color Palette.blue, Border.rounded 4, pointer, onClick <| AddUploadedAttachmentToPunch punch ] (text "Add")
                    ]

            Nothing ->
                none
        , attachmentPreview size model punch
        ]


onEnterKey msg =
    htmlAttribute <|
        HE.on "keydown"
            (D.field "keyCode" D.int
                |> D.andThen
                    (\keyCode ->
                        if keyCode == 13 then
                            D.succeed msg

                        else
                            D.fail ""
                    )
            )


attachmentPreview : Float -> Model -> Punch -> Element Msg
attachmentPreview size model punch =
    case punch.attachments of
        Loaded _ attachments ->
            attachments
                |> List.map (renderAttachmentItem size punch)
                |> column [ width fill ]

        Loading _ _ ->
            text "Loading attachments"

        DataError _ _ ->
            text "Problem getting attachments"

        NotLoaded ->
            none


renderAttachmentItem : Float -> Punch -> Punch.Attachment -> Element Msg
renderAttachmentItem size punch a =
    row [ width fill, padding 6, spacing 10 ]
        [ row
            [ width fill
            , clip
            , pointer
            , spacing <| round size
            , onClick <| AttachmentPressed punch a
            ]
            [ if String.isEmpty a.thumbnailAsBase64 then
                el [ width (px 100) ] <| el [ centerX, Font.size <| scaledInt size -3 ] (text "no preview")

              else
                image [ width (px 100) ] { description = "preview", src = String.concat [ "data:", a.mimeType, ";base64,", a.thumbnailAsBase64 ] }
            , el [ width fill, Font.size <| scaledInt size -3 ] <| paragraph [] [ text a.title ]
            ]
        , el [ alignRight, Font.color Palette.energyRed, padding 6, Border.rounded 4, Border.width 1, Border.color Palette.energyRed, pointer, onClick <| DeleteAttachmentButtonPressed punch a ] (text "X")
        ]


renderDetails : Float -> Model -> Bool -> Punch -> Element Msg
renderDetails size model readOnly punch =
    let
        dd =
            dropDown size readOnly punch.id model
    in
    column [ width fill, spacing 2 ]
        [ el
            [ width fill
            , Background.color Palette.blue
            , Font.color Palette.white
            , padding 8
            , Font.size (scaledInt size -1)
            ]
            (text "Details")
        , column [ width fill, spacing 2, padding 4 ]
            [ column [ spacing 6 ]
                [ kv size "No" (String.fromInt punch.id) ""
                , kv size "Tag" punch.tag ""

                --, kv size "Type" punch.typeDescription ""
                --, kv size "Commissioning package" punch.commPk ""
                --, kv size "MC package" punch.mcPk ""
                --, kv size "Location" punch.location ""
                ]
            , dd
                CategoryDropDown
                (case punch.status of
                    PA ->
                        "PA"

                    _ ->
                        "PB"
                )
                .categories
            , dd RaisedByDropDown punch.raisedByOrg .organizations
            , dd ClearingByDropDown punch.clearingByOrg .organizations
            , dd TypeDropDown punch.typeDescription .types
            , dd SortingDropDown punch.sortingDescription .sorts
            ]
        ]


dropDown : Float -> Bool -> Int -> Model -> DropDown -> String -> (Model -> WebData (List SelectItem)) -> Element Msg
dropDown size readOnly punchId model dropDownType current field =
    let
        name =
            case dropDownType of
                NoDropDown ->
                    ""

                CategoryDropDown ->
                    "Category"

                RaisedByDropDown ->
                    "Raised By"

                ClearingByDropDown ->
                    "Clearing By"

                TypeDropDown ->
                    "Type"

                SortingDropDown ->
                    "Sorting"

        header =
            el
                [ Font.color Palette.mossGreen
                , Font.bold
                , Font.size <| scaledInt size -3
                , width fill
                ]
                (text name)
    in
    if readOnly then
        kv size name current ""

    else if model.dropDown == dropDownType then
        column
            [ Border.width 1
            , Border.color Palette.blue
            , Border.rounded 4
            , padding 10
            , width fill

            --, height (fill |> maximum 500)
            , spacing 10
            ]
            [ row
                [ width fill
                , pointer
                , onClick CloseDropDownButtonPressed
                , Border.widthEach { top = 0, left = 0, right = 0, bottom = 1 }
                , Border.color Palette.mistBlue
                , Element.paddingEach { top = 0, left = 0, right = 0, bottom = 10 }
                ]
                [ column [ width fill ]
                    [ header
                    , text current
                    ]
                , Icon.arrow_up
                    |> html
                    |> el
                        [ width (px <| round <| size)
                        , height (px <| round <| size)
                        , Font.color Palette.blue
                        ]
                ]
            , selectionList size current punchId (field model)
            ]

    else
        row
            [ width fill
            , scrollbarY
            , Border.width 1
            , Border.color Palette.blue
            , padding 10
            , Border.rounded 4
            , pointer
            , onClick <| DropDownPressed dropDownType
            ]
            [ column [ width fill ]
                [ header
                , text current
                ]
            , Icon.arrow_down
                |> html
                |> el
                    [ width (px <| round <| size)
                    , height (px <| round <| size)
                    , Font.color Palette.blue
                    ]
            ]


selectionList : Float -> String -> Int -> WebData (List SelectItem) -> Element Msg
selectionList size current punchId webData =
    SelectionList.webDataSelectionList (SelectionList.selectionList (selectItem size current punchId)) webData


selectItem : Float -> String -> Int -> SelectItem -> ( String, Element Msg )
selectItem size current punchId item =
    ( item.id |> String.fromInt
    , el [ width fill, paddingXY 0 1 ] <|
        row
            [ width fill
            , padding 10
            , spacing 10
            , Border.widthEach { left = 0, top = 0, right = 0, bottom = 1 }
            , Border.color Palette.mistBlue
            , mouseOver [ Background.color Palette.mistBlue ]
            , Background.color <|
                if current == item.description || current == item.code then
                    Palette.lightGrey

                else
                    Palette.white
            , pointer
            , onClick <| DropDownItemPressed punchId item
            ]
            [ paragraph [ Font.size (scaledInt size -4), width <| px <| round <| size * 4 ] [ text item.code ]
            , paragraph [ Font.size (scaledInt size -2), width fill ] [ text item.description ]
            ]
    )


iconFromCategory : String -> H.Html msg
iconFromCategory category =
    case category of
        "Circuit/Starter" ->
            Icon.circuit

        "CIRCUIT_AND_STARTER" ->
            Icon.circuit

        "Electrical" ->
            Icon.electrical

        "ELECTRICAL_FIELD" ->
            Icon.electrical

        "Cable" ->
            Icon.cable

        "CABLE" ->
            Icon.cable

        "Instrument" ->
            Icon.instrument

        "INSTRUMENT_FIELD" ->
            Icon.instrument

        "Fire & Gas" ->
            Icon.fireAndGas

        "FIRE_AND_GAS_FIELD" ->
            Icon.fireAndGas

        "Line" ->
            Icon.line_

        "LINE" ->
            Icon.line_

        "Main Equipment" ->
            Icon.tag "M" "none"

        "MAIN_EQUIPMENT" ->
            Icon.tag "M" "none"

        "Telecom" ->
            Icon.telecom

        "TELECOM_FIELD" ->
            Icon.telecom

        "Junction Box" ->
            Icon.junctionBox

        "JUNCTION_BOX" ->
            Icon.junctionBox

        "Special Item" ->
            Icon.tag "SI" "none"

        "SPECIAL_ITEM" ->
            Icon.tag "SI" "none"

        "Heat Tracing Cable" ->
            Icon.heatTrace

        "HEAT_TRACING_CABLE" ->
            Icon.heatTrace

        "Signal" ->
            Icon.signal

        "SIGNAL" ->
            Icon.signal

        "Manual Valve" ->
            Icon.manualValve

        "MANUAL_VALVE" ->
            Icon.manualValve

        "Function" ->
            Icon.function

        "FUNCTION" ->
            Icon.function

        "Ducting" ->
            Icon.ducting

        "DUCTING" ->
            Icon.ducting

        _ ->
            Icon.tag "" "none"


onClick : msg -> Element.Attribute msg
onClick msg =
    HE.custom "click"
        (D.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = False
            }
        )
        |> htmlAttribute


renderSignatures : Float -> Punch -> Element Msg
renderSignatures size punch =
    case punch.apiPunch of
        Loaded _ x ->
            column [ width fill ]
                [ if String.isEmpty x.clearedAt then
                    none

                  else
                    el
                        [ width fill
                        , Background.color Palette.blue
                        , Font.color Palette.white
                        , padding 8
                        , Font.size (scaledInt size -1)
                        ]
                        (text "Signatures")
                , column [ width fill, padding 10, spacing 2 ]
                    [ if String.isEmpty x.clearedAt then
                        el [ alignRight ] <|
                            signButton size
                                "Clear"
                                (if x.statusControlledBySwcr then
                                    Just "Swcr controls status"

                                 else if x.isRestrictedForUser then
                                    Just "Access denied"

                                 else
                                    Nothing
                                )
                                (ClearPunchButtonPressed punch)

                      else
                        row [ width Element.fill ]
                            [ wrappedRow [ width fill, spacingXY 10 0 ]
                                [ el [ Font.bold ] (text "Cleared by")
                                , row [ spacing 10 ]
                                    [ text (x.clearedByFirstName ++ " " ++ x.clearedByLastName)
                                    , el [ alignRight ] (text <| String.left 10 x.clearedAt)
                                    ]
                                ]
                            , signButton size
                                "Unclear"
                                (if x.verifiedAt /= "" then
                                    Just "Punch is verified"

                                 else
                                    Nothing
                                )
                                (UnclearPunchButtonPressed punch)
                            ]
                    , if x.clearedAt /= "" then
                        if String.isEmpty x.verifiedAt then
                            el [ alignRight ] <|
                                signButton size
                                    "Verify"
                                    (if x.statusControlledBySwcr then
                                        Just "Swcr controls status"

                                     else if x.isRestrictedForUser then
                                        Just "Access denied"

                                     else
                                        Nothing
                                    )
                                    (VerifyPunchButtonPressed punch)

                        else
                            row [ width fill, spacingXY 10 0 ]
                                [ wrappedRow [ width fill, spacingXY 10 0 ]
                                    [ el [ Font.bold ] (text "Verified by")
                                    , row [ spacing 10 ]
                                        [ el [ alignRight ] <| text (x.verifiedByFirstName ++ " " ++ x.verifiedByLastName)
                                        , el [ alignRight ] (text <| String.left 10 x.verifiedAt)
                                        ]
                                    ]
                                , signButton size "Unverify" Nothing (UnverifyPunchButtonPressed punch)
                                ]

                      else
                        none
                    ]
                ]

        _ ->
            none


signButton : Float -> String -> Maybe String -> Msg -> Element Msg
signButton size name maybeDisabled msg =
    let
        activeAttributes =
            [ pointer
            , onClick msg
            ]

        deactiveAttributes message =
            [ alpha 0.3
            , htmlAttribute <| HA.style "cursor" "not-allowed"
            , htmlAttribute <| HA.title message
            ]
    in
    el
        ([ width <| px <| round <| size * 4
         , height <| px <| round <| size * 2
         , Background.color Palette.blue
         , Font.color Palette.white
         , Border.rounded 10
         ]
            ++ (case maybeDisabled of
                    Just message ->
                        deactiveAttributes message

                    Nothing ->
                        activeAttributes
               )
        )
    <|
        el [ centerX, centerY, Font.size (round size) ] (text name)


onContextMenu : ({ x : Float, y : Float } -> Msg) -> Element.Attribute Msg
onContextMenu msg =
    HE.custom "contextmenu"
        (D.map2 (\x y -> { x = x, y = y })
            (D.field "clientX" D.float)
            (D.field "clientY" D.float)
            |> D.andThen
                (\position ->
                    D.succeed
                        { message = msg position
                        , stopPropagation = True
                        , preventDefault = True
                        }
                )
        )
        |> htmlAttribute
