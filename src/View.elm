module View exposing (renderChecklists)

import Data.Checklist as Checklist exposing (Checklist)
import Data.Common as Common exposing (scaledInt)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Icon
import Json.Decode as D
import Messages exposing (Msg(..))
import Palette
import Types exposing (..)


renderChecklists : Float -> Maybe Int -> List Checklist -> Element Msg
renderChecklists size maybeSelected checklists =
    checklists
        |> List.map (renderChecklistItem size maybeSelected)
        --|> List.intersperse spacer
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


renderChecklistItem : Float -> Maybe Int -> Checklist.Checklist -> ( String, Element Msg )
renderChecklistItem size maybeSelected item =
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
                    Palette.whiteOnMistBlue

        isSelected =
            maybeSelected == Just item.id

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

        itemType =
            el [ alignRight, clip, Font.size <| scaledInt size -2 ] (text item.type_)

        responsible =
            el [ alignRight, Font.size <| scaledInt size -2 ] (text item.responsible)

        icon =
            el
                [ width (px 30)
                , height (px 30)
                , inFront <| statusBadge
                ]
            <|
                html <|
                    iconFromCategory item.register

        tagNo =
            paragraph [ Font.size <| scaledInt size -1, width fill, Font.color Palette.mossGreen ] [ text item.tagNo ]

        tagDescription =
            paragraph [ width fill, Font.size (scaledInt size -2) ] [ text item.description ]
    in
    ( String.fromInt item.id
    , column
        [ width fill
        , Background.color <|
            if isSelected then
                Palette.mistBlue

            else
                Palette.white
        , padding (round <| size / 2)
        , onClick <| ChecklistPressed item
        , pointer
        ]
        [ row [ width fill ]
            [ icon
            , row [ width fill, spacing (round size * 2) ]
                [ column [ width fill ] [ tagNo, tagDescription ]
                , itemType
                , responsible
                ]
            ]
        , if isSelected then
            case item.details of
                NotLoaded ->
                    text "NotLoaded"

                Loading ->
                    text "Loading..."

                DataError ->
                    text "Error getting details"

                Loaded details ->
                    column [ width fill, height fill, Background.color Palette.white, onClick NoOp ]
                        [ {- text "Loop Tags:"
                             , paragraph [ width fill, spacing 10 ] (details.loopTags |> List.map (\tagNo -> el [] (text tagNo)))
                             ,
                          -}
                          renderChecklistItems size item details.items
                        ]

          else
            none
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


renderChecklistItems : Float -> Checklist -> List Checklist.Item -> Element Msg
renderChecklistItems size checklist items =
    column [ width fill, spacing -1 ] (List.map (renderChecklistCheckItem size checklist) items)


renderChecklistCheckItem : Float -> Checklist -> Checklist.Item -> Element Msg
renderChecklistCheckItem size checklist item =
    if item.isHeading then
        item.text
            |> String.lines
            |> List.map (\txt -> paragraph [ Font.center ] [ text txt ])
            |> column
                [ if String.startsWith "NOTE" item.text || String.startsWith "-" item.text then
                    Font.regular

                  else
                    Font.bold
                , if String.startsWith "NOTE" item.text || String.startsWith "-" item.text then
                    Font.size (scaledInt size -3)

                  else
                    Font.size (scaledInt size -2)
                , padding 4
                , centerX
                ]

    else
        row [ width fill, paddingXY 10 2, Border.widthXY 0 1, Border.dashed, Border.color Palette.mistBlue ]
            [ column [ width fill ]
                [ item.text
                    |> String.lines
                    |> List.map (\txt -> paragraph [] [ text txt ])
                    |> column [ width fill, Font.size (scaledInt size -2), padding 4 ]
                , if List.isEmpty item.metaTable.columnLabels then
                    none

                  else
                    renderMetaTable size checklist item
                ]
            , row [ centerY, alignRight, spacing 10 ]
                [ checkButton size item.isNa
                , checkButton size item.isOk
                ]
            ]


renderMetaTable : Float -> Checklist -> Checklist.Item -> Element Msg
renderMetaTable size checklist checkItem =
    table [ width fill ]
        { columns =
            checkItem.metaTable.columnLabels
                |> List.map
                    (\ch ->
                        { header = el [ Font.size (scaledInt size -1) ] (text ch.label)
                        , width = fill
                        , view =
                            \row ->
                                row.cells
                                    |> List.filter (\cell -> cell.columnId == ch.id)
                                    |> List.head
                                    |> Maybe.map (renderCellInput size checklist checkItem ch row)
                                    |> Maybe.withDefault none
                        }
                    )
        , data = checkItem.metaTable.rows
        }


renderCellInput : Float -> Checklist -> Checklist.Item -> Checklist.ColumnLabel -> Checklist.Row -> Checklist.Cell -> Element Msg
renderCellInput size checklist checkItem columnHeader tableRow cell =
    row [ spacing 4 ]
        [ Input.text
            [ Font.size (round size)
            ]
            { label = Input.labelHidden ""
            , onChange = \txt -> NoOp --MetaTableCellInput checklist checkItem tableRow columnHeader
            , placeholder = Nothing
            , text = cell.value
            }
        , text cell.unit
        ]


checkButton size isActive =
    el
        [ height <| px <| round <| size
        , width <| px <| round <| size
        , Border.rounded 1000
        , Border.color Palette.mistBlue
        , Border.width 2
        , Background.color <|
            if isActive then
                Palette.blue

            else
                Palette.white
        ]
        none


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
