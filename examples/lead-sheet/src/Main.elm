module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Events
import Music.Analysis as Analysis exposing (Analysis)
import Music.Chord as Chord exposing (Chord)
import Music.Key as Key exposing (Key)
import Music.PitchClass as PitchClass


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
    ( { currentKey = Key.bFlat
      , analyzed = False
      , entries =
            [ initEntry <| Chord.majorSeventh PitchClass.bFlat
            , initEntry <| Chord.minorSeventh PitchClass.c
            , initEntry <| Chord.minorSeventh PitchClass.d
            , initEntry <| Chord.diminishedSeventh PitchClass.dFlat
            , initEntry <| Chord.minorSeventh PitchClass.c
            , initEntry <| Chord.dominantSeventh PitchClass.f
            , initEntry <| Chord.majorSeventh PitchClass.bFlat
            , initEntry <| Chord.dominantSeventh PitchClass.d

            --
            , initEntry <| Chord.minorSeventh PitchClass.g
            , initEntry <| Chord.dominantSeventh PitchClass.d
            , initEntry <| Chord.minorSeventh PitchClass.g
            , initEntry <| Chord.dominantSeventh PitchClass.g
            , initEntry <| Chord.minorSeventh PitchClass.c
            , initEntry <| Chord.dominantSeventh PitchClass.f
            , initEntry <| Chord.majorSeventh PitchClass.bFlat
            , initEntry <| Chord.dominantSeventh PitchClass.bFlat

            --
            , initEntry <| Chord.majorSeventh PitchClass.eFlat
            , initEntry <| Chord.dominantSeventh PitchClass.aFlat
            , initEntry <| Chord.majorSeventh PitchClass.bFlat
            , initEntry <| Chord.dominantSeventh PitchClass.bFlat
            , initEntry <| Chord.majorSeventh PitchClass.eFlat
            , initEntry <| Chord.dominantSeventh PitchClass.aFlat
            , initEntry <| Chord.majorSeventh PitchClass.bFlat
            , Nothing

            --
            , initEntry <| Chord.halfDiminished PitchClass.e
            , initEntry <| Chord.dominantSeventhFlatNine PitchClass.a
            , initEntry <| Chord.minorSeventh PitchClass.d
            , initEntry <| Chord.dominantSeventh PitchClass.dFlat
            , initEntry <| Chord.dominantSeventhSus4 PitchClass.c
            , initEntry <| Chord.dominantSeventh PitchClass.c
            , initEntry <| Chord.minorSeventh PitchClass.c
            , initEntry <| Chord.dominantSeventh PitchClass.f

            --
            , initEntry <| Chord.majorSeventh PitchClass.bFlat
            , initEntry <| Chord.minorSeventh PitchClass.c
            , initEntry <| Chord.minorSeventh PitchClass.d
            , initEntry <| Chord.diminishedSeventh PitchClass.dFlat
            , initEntry <| Chord.minorSeventh PitchClass.c
            , initEntry <| Chord.dominantSeventh PitchClass.f
            , initEntry <| Chord.majorSeventh PitchClass.bFlat
            , initEntry <| Chord.dominantSeventh PitchClass.d

            --
            , initEntry <| Chord.minorSeventh PitchClass.g
            , initEntry <| Chord.dominantSeventh PitchClass.d
            , initEntry <| Chord.minorSeventh PitchClass.g
            , initEntry <| Chord.dominantSeventh PitchClass.g
            , initEntry <| Chord.minorSeventh PitchClass.c
            , initEntry <| Chord.dominantSeventh PitchClass.f
            , initEntry <| Chord.majorSeventh PitchClass.bFlat
            , initEntry <| Chord.dominantSeventh PitchClass.bFlat

            --
            , initEntry <| Chord.majorSeventh PitchClass.eFlat
            , initEntry <| Chord.dominantSeventh PitchClass.g
            , initEntry <| Chord.minorSeventh PitchClass.c
            , Nothing
            , initEntry <| Chord.halfDiminished PitchClass.a
            , initEntry <| Chord.dominantSeventhFlatNine PitchClass.d
            , initEntry <| Chord.minorSeventh PitchClass.g
            , initEntry <| Chord.dominantSeventh PitchClass.gFlat

            --
            , initEntry <| Chord.majorSeventh PitchClass.bFlat
            , Nothing
            , initEntry <| Chord.minorSeventh PitchClass.c
            , initEntry <| Chord.dominantSeventh PitchClass.f
            , initEntry <| Chord.majorSix PitchClass.bFlat
            , Nothing
            , initEntry <| Chord.minorSeventh PitchClass.c
            , initEntry <| Chord.dominantSeventh PitchClass.f
            ]
      }
    , Cmd.none
    )


initEntry : Chord -> Maybe Entry
initEntry chord =
    Just
        { analysis = Nothing
        , chord = chord
        }


type alias Model =
    { entries : List (Maybe Entry)
    , currentKey : Key
    , analyzed : Bool
    }


type alias Entry =
    { analysis : Maybe Analysis
    , chord : Chord
    }


type Msg
    = AnalyzeClicked
    | NewKeyChosen Key


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnalyzeClicked ->
            ( analyzeEntries model, Cmd.none )

        NewKeyChosen key ->
            ( transposeEntries key model, Cmd.none )


analyzeEntries : Model -> Model
analyzeEntries model =
    { model
        | entries =
            List.map
                (\maybeEntry ->
                    Maybe.map (analyzeEntry model.currentKey) maybeEntry
                )
                model.entries
        , analyzed = True
    }


analyzeEntry : Key -> Entry -> Entry
analyzeEntry currentKey entry =
    { entry
        | analysis =
            Just (Analysis.analyze entry.chord currentKey)
    }


transposeEntries : Key -> Model -> Model
transposeEntries newKey model =
    { model
        | currentKey = newKey
        , entries =
            List.map
                (\maybeEntry ->
                    Maybe.map (transposeEntry newKey) maybeEntry
                )
                model.entries
    }


transposeEntry : Key -> Entry -> Entry
transposeEntry currentKey entry =
    { entry
        | chord =
            case entry.analysis of
                Just analysis ->
                    Analysis.toChord Analysis.seventhsByDefault currentKey analysis

                Nothing ->
                    entry.chord
    }


view : Model -> { title : String, body : List (Html Msg) }
view model =
    { title = "Lead Sheet"
    , body =
        [ Html.div [] (List.map (viewEntry model.currentKey) model.entries)
        , Html.button [ Html.Events.onClick AnalyzeClicked ] [ Html.text "Analyze" ]
        , viewTransposeControls model
        ]
    }


viewTransposeControls : Model -> Html Msg
viewTransposeControls model =
    let
        keyMenuItems =
            [ Key.c
            , Key.dFlat
            , Key.d
            , Key.eFlat
            , Key.e
            , Key.f
            , Key.fSharp
            , Key.g
            , Key.aFlat
            , Key.a
            , Key.bFlat
            , Key.b
            ]
                |> List.map
                    (\key ->
                        Html.option [ Html.Events.onClick (NewKeyChosen key) ]
                            [ Html.text (Key.toString key)
                            ]
                    )
    in
    if model.analyzed then
        Html.select []
            keyMenuItems

    else
        Html.text ""


viewEntry : Key -> Maybe Entry -> Html Msg
viewEntry currentKey maybeEntry =
    case maybeEntry of
        Just entry ->
            Html.div []
                [ Html.text (Chord.toString entry.chord)
                , viewAnalysis currentKey entry.analysis
                ]

        Nothing ->
            Html.text ""


viewAnalysis : Key -> Maybe Analysis -> Html Msg
viewAnalysis currentKey maybeAnalysis =
    case maybeAnalysis of
        Just analysis ->
            Html.div []
                [ Html.text (Analysis.toString currentKey analysis)
                ]

        Nothing ->
            Html.text ""
