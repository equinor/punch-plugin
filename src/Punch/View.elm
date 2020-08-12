module Punch.View exposing (renderPunchList)


import Equinor.Component.SelectionList as SelectionList
import Data.Common as Common exposing (kv, scaledInt)
import Punch exposing (Punch)
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onLoseFocus)
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Equinor.Icon as Icon
import Json.Decode as D
import Messages exposing (Msg(..))
import Model exposing (Model)
import Equinor.Palette as Palette
import String.Extra
import Types exposing (..)


renderPunchList : Float -> Model -> Element Msg
renderPunchList size model =
    model.punch
        |> Dict.values
        |> List.map (renderPunchListItem size model)
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


renderPunchListItem : Float -> Model -> Punch -> ( String, Element Msg )
renderPunchListItem size model item =
    let
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
                (item.status |> Common.statusToString |> text)

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

        shortDescription =
            item.description
                |> String.lines
                |> List.take 2
                |> List.map (\rowText -> row [] (Common.highlight model.highlight rowText))
                |> column [ width fill, clip ]

        tagNo =
            paragraph [ Font.size <| scaledInt size -1, width fill, Font.color Palette.mossGreen ] [ text item.tag ]
    in
    ( item.id
    , column
        [ width fill
        , Background.color <|
            if isSelected then
                Palette.mistBlue

            else
                Palette.white
        , padding (round <| size / 2)
        ]
        [ row
            [ width fill
            , onClick <| PunchItemPressed item
            , pointer
            ]
            [ icon
            , shortDescription
            ]
        , if isSelected then
            column [ width fill, height fill, Background.color Palette.white, onClick NoOp, Border.rounded 4, padding 4 ]
                [ if isSelected then
                    renderDescription model.highlight size item

                  else
                    none

                --, renderComments size item
                , renderDetails size model item

                --, renderSignatures size item
                , if String.isEmpty model.errorMsg then
                    none

                  else
                    paragraph [ width fill, Background.color Palette.alphaYellow, padding 6 ] [ text model.errorMsg ]
                ]

          else
            none
        ]
    )


renderDescription : Maybe String -> Float -> Punch -> Element Msg
renderDescription maybeHighlight size punch =
    column [ width fill ]
        [ el [ Font.color Palette.mossGreen, Font.size <| scaledInt size -3, Font.bold ] <| text "Punch description:"

        {- , punch.description
           |> String.lines
           |> List.map
               (\txt ->
                   if txt == "" then
                       text " "

                   else
                       paragraph [] (Common.highlight maybeHighlight txt)
               )
           |> column [ width fill ]
        -}
        , Input.multiline
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


renderDetails : Float -> Model -> Punch -> Element Msg
renderDetails size model punch =
    column [ width fill ]
        [ column [ spacing 6 ]
            [ kv size "No" punch.id ""
            , kv size "Tag" punch.tag ""
            , kv size "Type" punch.typeDescription ""
            , kv size "Commissioning package" punch.commPk ""
            , kv size "MC package" punch.mcPk ""
            , kv size "Location" punch.location ""
            ]
        , dropDown size
            CategoryDropDown
            (case punch.status of
                PA ->
                    "PA"

                _ ->
                    "PB"
            )
            .categories
            punch
            model
        , dropDown size RaisedByDropDown punch.raisedByOrg .organizations punch model
        , dropDown size ClearingByDropDown punch.clearingByOrg .organizations punch model
        ]


dropDown : Float -> DropDown -> String -> (Model -> WebData (List SelectItem)) -> Punch -> Model -> Element Msg
dropDown size dropDownType current field punch model =
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

        header =
            el
                [ Font.color Palette.mossGreen
                , Font.bold
                , Font.size <| scaledInt size -3
                , width fill
                ]
                (text name)
    in
    if model.dropDown == dropDownType then
        column
            [ Border.width 1
            , Border.rounded 4
            , width fill
            , height (fill |> maximum 500)
            ]
            [ header
            , selectionList size current punch (field model)
            ]

    else
        column
            [ width fill
            , scrollbarY
            , Border.width 1
            , padding 10
            , Border.rounded 4
            , pointer
            , onClick <| DropDownPressed dropDownType
            ]
            [ header
            , text current
            ]


selectionList : Float -> String -> Punch -> WebData (List SelectItem) -> Element Msg
selectionList size current punch webData =
    SelectionList.webDataSelectionList (SelectionList.selectionList (selectItem size current punch)) webData


selectItem : Float -> String -> Punch -> SelectItem -> ( String, Element Msg )
selectItem size current punch item =
    ( item.id |> String.fromInt
    , row
        [ width fill
        , padding 10
        , spacing 10
        , mouseOver [ Background.color Palette.mistBlue ]
        , Background.color <|
            if current == item.description || current == item.code then
                Palette.lightGrey

            else
                Palette.white
        , pointer
        , onClick <| DropDownItemPressed punch item
        ]
        [ text item.code, text item.description ]
    )


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
