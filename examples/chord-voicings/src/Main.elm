module Main exposing (main)

import Browser
import Html exposing (Html)


type alias Flags =
    ()


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


init : Flags -> ( Model, Cmd msg )
init flags =
    ( { nothing = () }
    , Cmd.none
    )


type alias Model =
    { nothing : ()
    }


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> { title : String, body : List (Html Msg) }
view model =
    { title = "Chord Voicings"
    , body =
        []
    }
