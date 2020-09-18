module Main exposing (main)

import Browser
import Element
import Element.Background
import Element.Font
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


measure1Chord : Chord.Chord -> Measure
measure1Chord first =
    Measure (initEntry first) Nothing


measure2Chords : Chord.Chord -> Chord.Chord -> Measure
measure2Chords first second =
    Measure (initEntry first) (initEntry second)


measureEmpty : Measure
measureEmpty =
    Measure Nothing Nothing


mapMeasure : (Entry -> Entry) -> Measure -> Measure
mapMeasure fn (Measure first second) =
    Measure (Maybe.map fn first) (Maybe.map fn second)


type Measure
    = Measure (Maybe Entry) (Maybe Entry)


init : Flags -> ( Model, Cmd msg )
init flags =
    ( { currentKey = Key.bFlat
      , analyzed = False
      , measures =
            [ measure2Chords (Chord.majorSeventh PitchClass.bFlat) (Chord.minorSeventh PitchClass.c)
            , measure2Chords (Chord.minorSeventh PitchClass.d) (Chord.diminishedSeventh PitchClass.dFlat)
            , measure2Chords (Chord.minorSeventh PitchClass.c) (Chord.dominantSeventh PitchClass.f)
            , measure2Chords (Chord.majorSeventh PitchClass.bFlat) (Chord.dominantSeventh PitchClass.d)

            --
            , measure2Chords (Chord.minorSeventh PitchClass.g) (Chord.dominantSeventh PitchClass.d)
            , measure2Chords (Chord.minorSeventh PitchClass.g) (Chord.dominantSeventh PitchClass.g)
            , measure2Chords (Chord.minorSeventh PitchClass.c) (Chord.dominantSeventh PitchClass.f)
            , measure2Chords (Chord.majorSeventh PitchClass.bFlat) (Chord.dominantSeventh PitchClass.bFlat)

            --
            , measure2Chords (Chord.majorSeventh PitchClass.eFlat) (Chord.dominantSeventh PitchClass.aFlat)
            , measure2Chords (Chord.majorSeventh PitchClass.bFlat) (Chord.dominantSeventh PitchClass.bFlat)
            , measure2Chords (Chord.majorSeventh PitchClass.eFlat) (Chord.dominantSeventh PitchClass.aFlat)
            , measure1Chord (Chord.majorSeventh PitchClass.bFlat)

            --
            , measure2Chords (Chord.halfDiminished PitchClass.e) (Chord.dominantSeventhFlatNine PitchClass.a)
            , measure2Chords (Chord.minorSeventh PitchClass.d) (Chord.dominantSeventh PitchClass.dFlat)
            , measure2Chords (Chord.dominantSeventhSus4 PitchClass.c) (Chord.dominantSeventh PitchClass.c)
            , measure2Chords (Chord.minorSeventh PitchClass.c) (Chord.dominantSeventh PitchClass.f)

            --
            , measure2Chords (Chord.majorSeventh PitchClass.bFlat) (Chord.minorSeventh PitchClass.c)
            , measure2Chords (Chord.minorSeventh PitchClass.d) (Chord.diminishedSeventh PitchClass.dFlat)
            , measure2Chords (Chord.minorSeventh PitchClass.c) (Chord.dominantSeventh PitchClass.f)
            , measure2Chords (Chord.majorSeventh PitchClass.bFlat) (Chord.dominantSeventh PitchClass.d)

            --
            , measure2Chords (Chord.minorSeventh PitchClass.g) (Chord.dominantSeventh PitchClass.d)
            , measure2Chords (Chord.minorSeventh PitchClass.g) (Chord.dominantSeventh PitchClass.g)
            , measure2Chords (Chord.minorSeventh PitchClass.c) (Chord.dominantSeventh PitchClass.f)
            , measure2Chords (Chord.majorSeventh PitchClass.bFlat) (Chord.dominantSeventh PitchClass.bFlat)

            --
            , measure2Chords (Chord.majorSeventh PitchClass.eFlat) (Chord.dominantSeventh PitchClass.g)
            , measure1Chord (Chord.minorSeventh PitchClass.c)
            , measure2Chords (Chord.halfDiminished PitchClass.a) (Chord.dominantSeventhFlatNine PitchClass.d)
            , measure2Chords (Chord.minorSeventh PitchClass.g) (Chord.dominantSeventh PitchClass.gFlat)

            --
            , measure1Chord (Chord.majorSeventh PitchClass.bFlat)
            , measure2Chords (Chord.minorSeventh PitchClass.c) (Chord.dominantSeventh PitchClass.f)
            , measure1Chord (Chord.majorSix PitchClass.bFlat)
            , measure2Chords (Chord.minorSeventh PitchClass.c) (Chord.dominantSeventh PitchClass.f)
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
    { measures : List Measure
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
            ( analyzeMeasures model, Cmd.none )

        NewKeyChosen key ->
            ( transposeEntries key model, Cmd.none )


analyzeMeasures : Model -> Model
analyzeMeasures model =
    { model
        | measures =
            List.map
                (mapMeasure
                    (analyzeEntry model.currentKey)
                )
                model.measures
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
        , measures =
            List.map
                (mapMeasure
                    (transposeEntry newKey)
                )
                model.measures
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
        [ Element.layout []
            (viewBody model)
        ]
    }


viewBody : Model -> Element.Element Msg
viewBody model =
    Element.wrappedRow
        []
        (List.map (viewMeasure model.currentKey) model.measures)



--, Html.button [ Html.Events.onClick AnalyzeClicked ] [ Html.text "Analyze" ]
--, viewTransposeControls model


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


viewMeasure : Key -> Measure -> Element.Element Msg
viewMeasure currentKey (Measure first second) =
    Element.row
        []
        [ Element.row
            [ Element.spacing 30
            , Element.padding 15
            ]
            [ viewMaybeEntry currentKey first
            , viewMaybeEntry currentKey second
            ]
        , Element.el
            [ Element.width (Element.px 3)
            , Element.height (Element.px 60)
            , Element.Background.color (Element.rgb 0 0 0)
            ]
            Element.none
        ]


viewMaybeEntry : Key -> Maybe Entry -> Element.Element Msg
viewMaybeEntry currentKey maybeEntry =
    case maybeEntry of
        Just entry ->
            Element.column
                [ Element.Font.size 26 ]
                [ Element.text (Chord.toString entry.chord)
                , viewAnalysis currentKey entry.analysis
                ]

        Nothing ->
            Element.none


viewAnalysis : Key -> Maybe Analysis -> Element.Element Msg
viewAnalysis currentKey maybeAnalysis =
    case maybeAnalysis of
        Just analysis ->
            Element.text (Analysis.toString currentKey analysis)

        Nothing ->
            Element.none
