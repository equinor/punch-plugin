module View exposing (renderChecklistItem)

import Data.Checklist as Checklist exposing (Checklist)
import Data.Common as Common exposing (scaledInt)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Html as H
import Html.Attributes as HA
import Icon
import Messages exposing (Msg(..))
import Palette
import Types exposing (..)


renderChecklistItem : Float -> Maybe Checklist -> WebData Checklist.Details -> Checklist.Checklist -> ( String, Element Msg )
renderChecklistItem size maybeSelectedChecklist webDataDetails item =
    let
        statusText =
            item.status
                |> Common.statusToString
                |> text

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
            maybeSelectedChecklist == Just item

        color =
            Palette.white

        shortDescription =
            item.description
                |> String.lines
                |> List.take 2
                |> List.map text
                |> column [ width fill, clip, Font.size (scaledInt size -3) ]

        toppLinje =
            row [ spacing 10, Font.size <| scaledInt size -3, width fill, centerY ]
                [ row []
                    [ el (colors [ paddingXY 2 1, Border.rounded 4, Font.size (scaledInt size -4) ]) (item.status |> Common.statusToString |> text)

                    --, el (Palette.combination Palette.white Palette.blue [ paddingXY 2 1, Border.rounded 4 ]) (String.fromInt item.subSheet |> text)
                    ]
                , el [ width fill, clip ] (text item.type_)
                , el []
                    (item.responsible
                        |> text
                    )
                ]

        tagBeskrivelse p =
            row [ width fill, clip ]
                [ el
                    [ width (px 30)
                    , height (px 30)
                    ]
                  <|
                    html <|
                        iconFromCategory p.register
                , el [ Font.size <| scaledInt size -1, width fill, clip, Font.color Palette.mossGreen ] (text p.tagNo)
                ]
    in
    ( String.fromInt item.id
    , column
        [ width fill
        , height (shrink |> minimum (round <| size * 2))

        --, onClick <| ChecklistPressed ref plant itemPressedContext item
        , pointer
        , scrollbars
        ]
        [ toppLinje
        , column [ width fill ]
            [ tagBeskrivelse item
            , shortDescription
            ]
        , if isSelected then
            column [ width fill ]
                [ {- row [ spacing 2 ]
                     [  case itemPressedContext of
                         Messages.TagContext _ ->
                             none

                         _ ->
                             tagLogo size item.tagNo

                     {- , case itemPressedContext of
                        Messages.CommPkContext _ ->
                            none
                        _ ->

                            commPkLogo ref device punch.commPk
                     -}
                     ]
                  -}
                  case webDataDetails of
                    NotLoaded ->
                        text "NotLoaded"

                    Loading ->
                        text "Loading..."

                    DataError ->
                        text "Error getting details"

                    Loaded details ->
                        column [ width fill, height fill, Background.color Palette.white ]
                            [ {- text "Loop Tags:"
                                 , paragraph [ width fill, spacing 10 ] (details.loopTags |> List.map (\tagNo -> el [] (text tagNo)))
                                 ,
                              -}
                              renderChecklistItems size item details.items
                            ]
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

        "Electrical" ->
            Icon.electrical

        "Cable" ->
            Icon.cable

        "Instrument" ->
            Icon.instrument

        "Fire & Gas" ->
            Icon.fireAndGas

        "Line" ->
            Icon.line_

        "Main Equipment" ->
            Icon.tag "M" "none"

        "Telecom" ->
            Icon.telecom

        "Junction Box" ->
            Icon.junctionBox

        "Special Item" ->
            Icon.tag "SI" "none"

        "Heat Tracing Cable" ->
            Icon.heatTrace

        "Signal" ->
            Icon.signal

        "Manual Valve" ->
            Icon.manualValve

        "Function" ->
            Icon.function

        "Ducting" ->
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
                [ if String.startsWith "NOTE:" item.text then
                    Font.regular

                  else
                    Font.bold
                , if String.startsWith "NOTE:" item.text then
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
