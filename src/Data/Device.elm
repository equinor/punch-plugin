module Data.Device exposing (..)

import Element exposing (..)
import Json.Decode as D
import Json.Encode as E
import Types exposing (..)


type alias DeviceDetails =
    { isNative : Bool
    , class : Element.Device
    , isTouchDevice : Bool
    , isIOS : Bool
    , isAndroid : Bool
    , userScale : UserScale
    , cameraSupport : Bool
    , textDetectorSupport : Bool
    , isEdge : Bool
    }


type alias Flags =
    { isNative : Bool
    , isIOS : Bool
    , isAndroid : Bool
    , cameraSupport : Bool
    , textDetectorSupport : Bool
    , windowSize : Size
    , isTouchDevice : Bool
    , loginRequired : Bool
    , isEdge : Bool
    }


initialDevice : Flags -> DeviceDetails
initialDevice flags =
    { isNative = flags.isNative
    , class =
        Element.classifyDevice
            { width = round flags.windowSize.width
            , height = round flags.windowSize.height
            }
    , isTouchDevice = flags.isTouchDevice
    , isIOS = flags.isIOS
    , isAndroid = flags.isAndroid
    , userScale = Medium
    , cameraSupport = flags.cameraSupport
    , textDetectorSupport = flags.textDetectorSupport
    , isEdge = flags.isEdge
    }


setUserScale : UserScale -> DeviceDetails -> DeviceDetails
setUserScale userScale oldDevice =
    { oldDevice | userScale = userScale }


type UserScale
    = Small
    | Medium
    | Large


userScaleToFloat : UserScale -> Float
userScaleToFloat userScale =
    case userScale of
        Small ->
            0.75

        Medium ->
            1

        Large ->
            1.25


userScaleDecoder : D.Decoder UserScale
userScaleDecoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "Small" ->
                        D.succeed Small

                    "Medium" ->
                        D.succeed Medium

                    "Large" ->
                        D.succeed Large

                    _ ->
                        D.fail "Unknown UserScale"
            )


userScaleEncoder : UserScale -> E.Value
userScaleEncoder userScale =
    E.string <|
        case userScale of
            Small ->
                "Small"

            Medium ->
                "Medium"

            Large ->
                "Large"
