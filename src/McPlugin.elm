module McPlugin exposing (..)

import Browser
import Html exposing (Html, text)


type Msg
    = NoOp


type alias Model =
    { name : String
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    text ("Hello world, my name is: " ++ model.name)


type alias Flags =
    { name : String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model flags.name, Cmd.none )


main : Program Flags Model Msg
main =
    Browser.element
        { init = init, update = update, subscriptions = \_ -> Sub.none, view = view }
