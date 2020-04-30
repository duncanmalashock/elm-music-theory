module Global exposing
    ( Flags
    , Model
    , Msg(..)
    , init
    , navigate
    , send
    , subscriptions
    , update
    , view
    )

import Browser.Navigation as Nav
import Document exposing (Document)
import Generated.Route as Route exposing (Route)
import MusicTheory.NoteSequence
import Ports
import Task
import UI
import Url exposing (Url)



-- INIT


type alias Flags =
    ()


type alias Model =
    { flags : Flags
    , url : Url
    , key : Nav.Key
    , notes : List MusicTheory.NoteSequence.NoteSequence
    , instrumentId : Int
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        model =
            { flags = flags
            , url = url
            , key = key
            , notes =
                [ MusicTheory.NoteSequence.sequenceWithTriplets
                ]
            , instrumentId = 10
            }
    in
    ( model
    , Ports.loadInstrument model.instrumentId
    )



-- UPDATE


type Msg
    = Navigate Route
    | PlayInBrowser
    | LoadInstrument Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Navigate route ->
            ( model
            , Nav.pushUrl model.key (Route.toHref route)
            )

        LoadInstrument instrumentId ->
            ( { model
                | instrumentId = instrumentId
              }
            , Ports.loadInstrument instrumentId
            )

        PlayInBrowser ->
            ( model
            , Ports.playInBrowser
                model.instrumentId
                (List.concatMap
                    (MusicTheory.NoteSequence.toEvents 100)
                    model.notes
                )
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view :
    { page : Document msg
    , global : Model
    , toMsg : Msg -> msg
    }
    -> Document msg
view { page, global, toMsg } =
    UI.layout
        { page = page
        }



-- COMMANDS


send : msg -> Cmd msg
send =
    Task.succeed >> Task.perform identity


navigate : Route -> Cmd Msg
navigate route =
    send (Navigate route)
