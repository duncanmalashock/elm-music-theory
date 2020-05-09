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
import MusicTheory.Time as Time
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

        sequence1 =
            MusicTheory.Sequence.init
                |> MusicTheory.Sequence.appendRest Time.eighth
                |> MusicTheory.Sequence.appendNote
                    (Note.eighth Pitch.c4 |> Note.fff)
                |> MusicTheory.Sequence.appendNote
                    (Note.eighth Pitch.eFlat4 |> Note.ff)
                |> MusicTheory.Sequence.appendRest Time.eighth
                |> MusicTheory.Sequence.appendNote
                    (Note.eighth Pitch.g4 |> Note.f)
                |> MusicTheory.Sequence.appendTuplet
                    (MusicTheory.Sequence.init
                        |> MusicTheory.Sequence.appendNote
                            (Note.eighth Pitch.c4 |> Note.fff)
                        |> MusicTheory.Sequence.appendNote
                            (Note.eighth Pitch.c4 |> Note.fff)
                        |> MusicTheory.Sequence.appendNote
                            (Note.eighth Pitch.c4 |> Note.fff)
                        |> MusicTheory.Sequence.tuplet
                            { duration = Time.quarter }
                    )
                |> MusicTheory.Sequence.appendNote
                    (Note.eighth Pitch.b4 |> Note.mf)
                |> MusicTheory.Sequence.appendNote
                    (Note.eighth Pitch.d5)
                |> MusicTheory.Sequence.appendNote
                    (Note.eighth Pitch.fSharp5 |> Note.mf)
                |> MusicTheory.Sequence.appendNote
                    (Note.eighth Pitch.a5 |> Note.f)
                |> MusicTheory.Sequence.appendNote
                    (Note.eighth Pitch.c6 |> Note.ff)
                |> MusicTheory.Sequence.appendNote
                    (Note.eighth Pitch.eFlat6 |> Note.fff)

        model : Model
        model =
            { flags = flags
            , url = url
            , key = key
            , sequences =
                [ { sequence = sequence1
                  , instrumentId = inst2
                  }
                ]
            , tempo = 120
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
                    Voice.toNoteEvents
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
