module Data.Checklist exposing (Cell, Checklist, ColumnLabel, Details, Group(..), Item, MetaTable, Row, apiDecoder, decoder, detailsApiDecoder, groupToString)

import Data.Common as Common
import Json.Decode as D
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as E
import Types exposing (..)


type Group
    = CPCL
    | MCCR
    | Preservation
    | RunningLogs
    | DCCL
    | SignalTag


apiGroupDecoder : D.Decoder Group
apiGroupDecoder =
    Common.nullString
        |> D.andThen
            (\str ->
                case str of
                    "Mechanical Completion Check Record" ->
                        D.succeed MCCR

                    "Commissioning Preparatory Check List" ->
                        D.succeed CPCL

                    "Preservation" ->
                        D.succeed Preservation

                    "Running Logs" ->
                        D.succeed RunningLogs

                    "DeCommissioning Check List" ->
                        D.succeed DCCL

                    _ ->
                        D.succeed SignalTag
            )


groupDecoder : D.Decoder Group
groupDecoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "MCCR" ->
                        D.succeed MCCR

                    "CPCL" ->
                        D.succeed CPCL

                    "Preservation" ->
                        D.succeed Preservation

                    "RunningLogs" ->
                        D.succeed RunningLogs

                    "DeCommissioningCheckList" ->
                        D.succeed DCCL

                    _ ->
                        D.succeed SignalTag
            )


groupToString : Group -> String
groupToString group =
    case group of
        MCCR ->
            "MCCR"

        CPCL ->
            "CPCL"

        Preservation ->
            "Preservation"

        RunningLogs ->
            "RunningLogs"

        DCCL ->
            "DeCommissioningCheckList"

        SignalTag ->
            "SignalTag"


type alias Checklist =
    { id : Int
    , group : Group
    , type_ : String
    , tagNo : String
    , responsible : String
    , status : Status
    , commPk : String
    , mcPk : String
    , updatedAt : String
    , register : String
    , description : String
    , sheet : Int
    , subSheet : Int
    , details : WebData Details
    }


type alias Details =
    { loopTags : List LoopTag
    , items : List Item
    }


type alias Item =
    { id : Int
    , isHeading : Bool
    , isNa : Bool
    , isOk : Bool
    , metaTable : MetaTable
    , sequenceNumber : String
    , text : String
    }


itemDecoder : D.Decoder Item
itemDecoder =
    D.map7 Item
        (D.field "Id" D.int)
        (D.field "IsHeading" D.bool)
        (D.field "IsNotApplicable" D.bool)
        (D.field "IsOk" D.bool)
        (D.field "MetaTable"
            (D.oneOf
                [ metaTableDecoder
                , D.null (MetaTable [] "" [])
                ]
            )
        )
        (D.field "SequenceNumber" D.string)
        (D.field "Text" Common.nullString)


type alias MetaTable =
    { columnLabels : List ColumnLabel
    , info : String
    , rows : List Row
    }


metaTableDecoder : D.Decoder MetaTable
metaTableDecoder =
    D.map3 MetaTable
        (D.field "ColumnLabels" (D.list columnLabelDecoder))
        (D.field "Info" Common.nullString)
        (D.field "Rows" (D.list rowDecoder))


type alias ColumnLabel =
    { id : Int
    , label : String
    }


columnLabelDecoder : D.Decoder ColumnLabel
columnLabelDecoder =
    D.map2 ColumnLabel
        (D.field "Id" D.int)
        (D.field "Label" Common.nullString)


type alias Row =
    { cells : List Cell
    , id : Int
    , label : String
    }


rowDecoder : D.Decoder Row
rowDecoder =
    D.map3 Row
        (D.field "Cells" (D.list cellDecoder))
        (D.field "Id" D.int)
        (D.field "Label" Common.nullString)


type alias Cell =
    { columnId : Int
    , unit : String
    , value : String
    }


cellDecoder : D.Decoder Cell
cellDecoder =
    D.map3 Cell
        (D.field "ColumnId" D.int)
        (D.field "Unit" Common.nullString)
        (D.field "Value" Common.nullString)


type alias LoopTag =
    String


detailsApiDecoder : D.Decoder Details
detailsApiDecoder =
    D.map2 Details
        (D.maybe (D.field "LoopTags" (D.list loopTagDecoder))
            |> D.andThen
                (\maybeLoop ->
                    case maybeLoop of
                        Just loopTag ->
                            D.succeed loopTag

                        Nothing ->
                            D.succeed []
                )
        )
        (D.field "CheckItems" (D.list itemDecoder))


loopTagDecoder : D.Decoder LoopTag
loopTagDecoder =
    D.field "TagNo" D.string


apiDecoder : D.Decoder Checklist
apiDecoder =
    D.succeed Checklist
        |> required "Id" D.int
        |> required "TagFormularType__FormularType__FormularGroup__Description" apiGroupDecoder
        |> required "TagFormularType__FormularType__Id" D.string
        |> required "TagFormularType__Tag__TagNo" D.string
        |> required "TagFormularType__Tag__TagNo" D.string
        |> required "Status__Id" statusDecoder
        |> required "TagFormularType__Tag__McPkg__CommPkg__CommPkgNo" Common.nullString
        |> required "TagFormularType__Tag__McPkg__McPkgNo" Common.nullString
        |> required "UpdatedAt" Common.nullString
        |> required "TagFormularType__Tag__Register__Id" Common.nullString
        |> required "TagFormularType__Tag__Description" Common.nullString
        |> required "TagFormularType__SheetNo" Common.nullInt
        |> required "TagFormularType__SubsheetNo" Common.nullInt
        |> hardcoded NotLoaded


decoder : D.Decoder Checklist
decoder =
    D.succeed Checklist
        |> required "id" D.int
        |> required "group" groupDecoder
        |> required "type_" D.string
        |> required "tagNo" D.string
        |> required "responsible" D.string
        |> required "status" statusDecoder
        |> required "commPk" D.string
        |> required "mcPk" D.string
        |> required "updatedAt" D.string
        |> required "register" D.string
        |> required "description" D.string
        |> optional "sheet" D.int 0
        |> optional "subSheet" D.int 0
        |> hardcoded NotLoaded


groupEncoder : Group -> E.Value
groupEncoder group =
    E.string <|
        case group of
            MCCR ->
                "MCCR"

            CPCL ->
                "CPCL"

            Preservation ->
                "Preservation"

            RunningLogs ->
                "RunningLogs"

            DCCL ->
                "DeCommissioningCheckList"

            SignalTag ->
                "SignalTag"


statusDecoder : D.Decoder Status
statusDecoder =
    D.oneOf
        [ D.string
            |> D.andThen
                (\str ->
                    D.succeed <|
                        statusFromString str
                )
        , D.null OS
        ]


statusFromString : String -> Status
statusFromString str =
    case str of
        "OK" ->
            OK

        "PA" ->
            PA

        "PB" ->
            PB

        _ ->
            OS
