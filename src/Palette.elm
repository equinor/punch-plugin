module Palette exposing (alphaEnergyRed, alphaGreen, alphaMistBlue, alphaMossGreen, alphaMossGreenHex, alphaSlateBlue, alphaWhite, alphaYellow, black, blue, buttonShadow, combination, darkGrey, darkGreyHex, energeticRedOnEnergeticRed8, energeticRedOnWhite, energyRed, energyRed8, green, grey, header, heritageRed, lichenGreen, lightGrey, mainMenu, mistBlue, mistBlueOnAlphaMossGreen, mistBlueOnAlphaSlate, mistBlueOnMossGreen, mistBlueOnSlate, mossGreen, mossGreenOnAlphaWhite, mossGreenOnWhite, red, redHex, slateBlue, slateBlueHex, slateBlueOnWhite, slateOnEnergeticRed8, slateOnMistBlue, spruceWood, spruceWood8, stateButton, white, whiteHex, whiteOnEnergeticRed, whiteOnGreen, whiteOnMistBlue, whiteOnMossGreen, whiteOnSlateBlue, yellow, yellowDisabled, yellowHex)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font



-- Custom


buttonShadow =
    { color = darkGrey
    , offset = ( 1, 1 )
    , blur = 10
    , size = 1
    }


mainMenu =
    { steps = [ white ]
    , angle = 3.14
    }


yellow =
    rgb255 251 202 54


yellowHex =
    "#fbca36"


yellowDisabled =
    rgb255 255 221 176


alphaYellow =
    rgba255 251 202 54 1


red =
    rgb255 255 59 59


redHex =
    "#ff3b3b"


green =
    rgb255 75 183 72


blue =
    rgb255 95 192 220


alphaGreen =
    rgba255 75 183 72 1


lightGrey =
    rgb 0.9 0.9 0.9



-- Primary


energyRed =
    rgb255 255 18 67


energyRed8 =
    rgb255
        255
        236
        240


alphaEnergyRed =
    rgba255 255 18 67 0.2


white =
    rgb 1 1 1


whiteHex =
    "#ffffff"


alphaWhite =
    rgba 1 1 1 0.7


black =
    rgb255 51 51 51


grey =
    rgb255 217 217 217



-- Supporting


slateBlue =
    rgb255 36 55 70


slateBlueHex =
    "#243746"


alphaSlateBlue alpha =
    rgba255 36 55 70 alpha


heritageRed =
    rgb255 125 0 35


mossGreen =
    rgb255 0 112 121


alphaMossGreen =
    rgba255 0 112 121 0.7


alphaMossGreenHex =
    "#007079"


mistBlue =
    rgb255 213 234 244


alphaMistBlue alpha =
    rgba255 213 234 244 alpha


spruceWood =
    rgb255 255 231 214


spruceWood8 =
    rgb255 255 243 235


lichenGreen =
    rgb255 230 250 236


combination fontColor backgroundColor attributes =
    attributes
        ++ [ Font.color fontColor
           , Background.color backgroundColor
           ]


whiteOnGreen =
    combination white green


whiteOnEnergeticRed =
    combination white energyRed


energeticRedOnWhite =
    combination energyRed white


energeticRedOnEnergeticRed8 =
    combination energyRed energyRed8


slateOnMistBlue =
    combination slateBlue mistBlue


slateOnEnergeticRed8 =
    combination slateBlue energyRed8


mistBlueOnSlate =
    combination mistBlue slateBlue


mistBlueOnAlphaSlate =
    combination mistBlue (alphaSlateBlue 0.7)


mistBlueOnMossGreen =
    combination mistBlue mossGreen


mistBlueOnAlphaMossGreen =
    combination mistBlue alphaMossGreen


slateBlueOnWhite =
    combination slateBlue white


whiteOnSlateBlue =
    combination white slateBlue


whiteOnMistBlue =
    combination white mistBlue


whiteOnMossGreen =
    combination white mossGreen


mossGreenOnWhite =
    combination mossGreen white


mossGreenOnAlphaWhite =
    combination mossGreen alphaWhite


stateButton isActive attributes =
    let
        fixed =
            [ Border.color slateBlue ]
    in
    if isActive then
        fixed
            ++ attributes
            |> whiteOnSlateBlue

    else
        fixed
            ++ [ mouseOver
                    [ Font.color slateBlue
                    , Background.color mistBlue
                    ]
               ]
            ++ attributes
            |> slateBlueOnWhite


header =
    { angle = 3.14
    , steps =
        [ black, darkGrey ]
    }


darkGrey =
    rgb255 128 128 128


darkGreyHex =
    "#808080"
