module Types exposing (..)


type WebData a
    = NotLoaded
    | Loading
    | DataError
    | Loaded a


type Status
    = OK
    | PB
    | PA
    | OS
