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
import MusicTheory.Generate.Melody as Melody
import MusicTheory.Note as Note
import MusicTheory.Pitch as Pitch
import MusicTheory.PitchClass as PitchClass
import MusicTheory.Scale as Scale
import MusicTheory.ScaleClass as ScaleClass
import MusicTheory.Sequence
import Ports
import Task
import UI
import Url exposing (Url)
import Voice exposing (Voice)



-- INIT


type alias Flags =
    ()


type alias Model =
    { flags : Flags
    , url : Url
    , key : Nav.Key
    , sequences : List Voice
    , tempo : Int
    , instrumentIds : List Int
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        inst1 =
            72

        inst2 =
            33

        inst3 =
            127

        firstInstrumentIds : List Int
        firstInstrumentIds =
            [ inst1, inst2, inst3 ]

        scale =
            Scale.scale PitchClass.f ScaleClass.phrygianDominant

        sequence startingPitch =
            List.foldl
                (\pitch -> MusicTheory.Sequence.appendNote (Note.eighth pitch))
                MusicTheory.Sequence.init
                (Melody.init scale startingPitch
                    |> Melody.generate
                        [ 0, 2, -4, 6, -3, 2, -1, 4, -2, -1, -1, -1, -2, 2, 1, 2, -4, 6, -3, 2, -1, 4, -2, -1, -1, -1, -2, 2, -1 ]
                )

        model : Model
        model =
            { flags = flags
            , url = url
            , key = key
            , sequences =
                [ { sequence = sequence Pitch.f5
                  , instrumentId = inst2
                  }
                , { sequence = sequence Pitch.a4
                  , instrumentId = inst1
                  }
                , { sequence = sequence Pitch.c3
                  , instrumentId = inst1
                  }
                ]
            , tempo = 200
            , instrumentIds = firstInstrumentIds
            }
    in
    ( model
    , firstInstrumentIds
        |> List.map Ports.loadInstrument
        |> Cmd.batch
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
                | instrumentIds = instrumentId :: model.instrumentIds
              }
            , Ports.loadInstrument instrumentId
            )

        PlayInBrowser ->
            ( model
            , Ports.playInBrowser
                (List.concatMap
                    (Voice.toNoteEvents model.tempo)
                    model.sequences
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
