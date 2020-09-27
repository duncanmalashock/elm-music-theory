module Main exposing (main)

import Browser
import Browser.Events
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Html exposing (Html)
import Html.Attributes
import Json.Decode
import List.Extra
import Music.Analysis as Analysis exposing (Analysis)
import Music.Chord as Chord exposing (Chord)
import Music.Key as Key exposing (Key)
import Music.PitchClass as PitchClass
import Svg
import Svg.Attributes


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = dropdownMenuSubscriptions
        }


type alias Model =
    { metadata : Metadata
    , measures : List Measure
    , currentKey : Key
    , analyzed : Bool
    , dropdownMenu : DropdownState
    }


type alias Metadata =
    { title : String
    , composer : String
    }


init : () -> ( Model, Cmd msg )
init flags =
    ( { metadata =
            { title = "My Romance"
            , composer = "R. Rodgers, L. Hart"
            }
      , currentKey = Key.bFlat
      , analyzed = False
      , measures = initialMeasures
      , dropdownMenu = Closed
      }
    , Cmd.none
    )


initialMeasures : List Measure
initialMeasures =
    -- Populate the initial chords in the chart, with no analysis
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


type Msg
    = AnalyzeClicked
    | KeyDropdownClicked
    | KeyDropdownClickedOut
    | KeyDropdownClosed
    | NewKeyChosen Key


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnalyzeClicked ->
            ( { model
                | measures =
                    List.map
                        (mapMeasure
                            (analyzeEntry model.currentKey)
                        )
                        model.measures
                , analyzed = True
              }
            , Cmd.none
            )

        KeyDropdownClicked ->
            ( { model
                | dropdownMenu = Open
              }
            , Cmd.none
            )

        KeyDropdownClickedOut ->
            ( { model
                | dropdownMenu = ReadyToClose
              }
            , Cmd.none
            )

        KeyDropdownClosed ->
            ( { model
                | dropdownMenu = Closed
              }
            , Cmd.none
            )

        NewKeyChosen newKey ->
            ( { model
                | currentKey = newKey
                , measures =
                    List.map
                        (mapMeasure
                            (transposeEntry newKey)
                        )
                        model.measures
                , dropdownMenu = Closed
              }
            , Cmd.none
            )



--  Measures & Entries


type Measure
    = Measure (Maybe Entry) (Maybe Entry)


measure1Chord : Chord.Chord -> Measure
measure1Chord first =
    Measure (initEntry first) Nothing


measure2Chords : Chord.Chord -> Chord.Chord -> Measure
measure2Chords first second =
    Measure (initEntry first) (initEntry second)


mapMeasure : (Entry -> Entry) -> Measure -> Measure
mapMeasure fn (Measure first second) =
    Measure (Maybe.map fn first) (Maybe.map fn second)


type alias Entry =
    { analysis : Maybe Analysis
    , chord : Chord
    }


initEntry : Chord -> Maybe Entry
initEntry chord =
    Just
        { analysis = Nothing
        , chord = chord
        }


analyzeEntry : Key -> Entry -> Entry
analyzeEntry currentKey entry =
    { entry
        | analysis =
            -- Analyze the current chord in terms of the current key
            Just (Analysis.analyze entry.chord currentKey)
    }


transposeEntry : Key -> Entry -> Entry
transposeEntry currentKey entry =
    { entry
        | chord =
            case entry.analysis of
                Just analysis ->
                    -- Use the analysis to transpose to a key, using seventh chords
                    Analysis.toChord Analysis.seventhsByDefault currentKey analysis

                Nothing ->
                    entry.chord
    }


view : Model -> { title : String, body : List (Html Msg) }
view model =
    { title = "Lead Sheet"
    , body =
        [ Element.layout
            [ Element.width Element.fill
            , Element.Font.family [ Element.Font.typeface "IBM Plex Sans" ]
            ]
            (viewBody model)
        ]
    }


type Row
    = Row (Maybe Measure) (Maybe Measure) (Maybe Measure) (Maybe Measure)


viewBody : Model -> Element.Element Msg
viewBody model =
    let
        rows =
            List.Extra.greedyGroupsOf 4 model.measures
                |> List.map
                    (\group ->
                        case group of
                            a :: b :: c :: d :: rest ->
                                Row (Just a) (Just b) (Just c) (Just d)

                            a :: b :: c :: rest ->
                                Row (Just a) (Just b) (Just c) Nothing

                            a :: b :: rest ->
                                Row (Just a) (Just b) Nothing Nothing

                            a :: rest ->
                                Row (Just a) Nothing Nothing Nothing

                            [] ->
                                Row Nothing Nothing Nothing Nothing
                    )
    in
    Element.column
        [ Element.width Element.fill
        ]
        [ Element.el
            [ Element.width Element.fill
            , Element.Font.color (Element.rgb 1 1 1)
            , Element.Background.color (Element.rgb255 18 147 216)
            , Element.height (Element.px 64)
            ]
            (Element.row
                [ Element.width <| Element.px 800
                , Element.centerX
                , Element.centerY
                , Element.spacing 16
                ]
                [ Element.link
                    [ Element.Font.size 24
                    , Element.Font.bold
                    ]
                    { url = "https://package.elm-lang.org/packages/duncanmalashock/elm-music-theory/latest/"
                    , label =
                        Element.text "elm-music-theory"
                    }
                , Element.el
                    [ Element.Font.size 16
                    , Element.alignRight
                    ]
                    (Element.text "Example: \"Lead Sheet\"")
                ]
            )
        , Element.column
            [ Element.width <| Element.px 800
            , Element.centerX
            , Element.spacing 30
            , Element.paddingXY 0 50
            ]
            [ viewTitle model
            , Element.column
                [ Element.width Element.fill
                , Element.spacing 40
                ]
                (List.map (viewRow model.currentKey) rows)
            ]
        ]


viewTitle : Model -> Element.Element Msg
viewTitle model =
    Element.column
        [ Element.width Element.fill
        , Element.centerX
        , Element.spacing 10
        ]
        [ Element.el
            [ Element.centerX
            , Element.Font.bold
            , Element.Font.size 32
            ]
            (Element.text model.metadata.title)
        , Element.row
            [ Element.width Element.fill
            ]
            [ viewControls model
            , Element.el
                [ Element.alignRight
                , Element.Font.bold
                , Element.Font.size 20
                ]
                (Element.text model.metadata.composer)
            ]
        ]


viewControls : Model -> Element.Element Msg
viewControls model =
    Element.row
        [ Element.spacing 5 ]
        [ viewKeyControl model
        , viewAnalyzeButton model
        ]


type DropdownState
    = Open
    | ReadyToClose
    | Closed


dropdownMenuSubscriptions : Model -> Sub Msg
dropdownMenuSubscriptions model =
    case model.dropdownMenu of
        Open ->
            Browser.Events.onAnimationFrame (\_ -> KeyDropdownClickedOut)

        ReadyToClose ->
            Browser.Events.onClick (Json.Decode.succeed KeyDropdownClosed)

        Closed ->
            Sub.none


viewKeyControl : Model -> Element.Element Msg
viewKeyControl model =
    let
        labelText =
            "Key of " ++ Key.toString model.currentKey
    in
    if model.analyzed then
        Element.Input.button
            [ Element.padding 10
            , Element.Border.rounded 2
            , Element.alignLeft
            , Element.Font.size 16
            , Element.Font.color (Element.rgb 1 1 1)
            , Element.Background.color (Element.rgb255 18 147 216)
            , Element.mouseOver
                [ Element.Background.color (Element.rgb255 47 127 190)
                ]
            , Element.below
                (viewKeyMenu model)
            ]
            { onPress =
                if model.dropdownMenu == Closed then
                    Just KeyDropdownClicked

                else
                    Nothing
            , label =
                Element.row
                    [ Element.spacing 10
                    ]
                    [ Element.el []
                        (Element.text labelText)
                    , Element.el []
                        (Element.html viewDropdownArrow)
                    ]
            }

    else
        Element.el
            [ Element.Font.bold
            , Element.Font.size 20
            ]
            (Element.text labelText)


viewKeyMenu : Model -> Element.Element Msg
viewKeyMenu model =
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
                        Element.Input.button
                            [ Element.Font.size 16
                            , Element.padding 10
                            , Element.alignLeft
                            , Element.Font.color (Element.rgb 0 0 0)
                            , Element.width Element.fill
                            , Element.mouseOver
                                [ Element.Background.color
                                    (Element.rgb255 18 147 216)
                                , Element.Font.color (Element.rgb 1 1 1)
                                ]
                            ]
                            { onPress = Just (NewKeyChosen key)
                            , label = Element.text (Key.toString key)
                            }
                    )
    in
    if model.dropdownMenu /= Closed then
        Element.column
            [ Element.Background.color (Element.rgb 1 1 1)
            , Element.width (Element.px 150)
            , Element.height (Element.px 200)
            , Element.htmlAttribute (Html.Attributes.style "overflow-y" "auto")
            , Element.Border.rounded 2
            , Element.Border.shadow
                { offset = ( 0, 0 )
                , size = 3
                , blur = 0
                , color = Element.rgba255 18 147 216 0.5
                }
            ]
            keyMenuItems

    else
        Element.none


viewDropdownArrow : Html msg
viewDropdownArrow =
    Svg.svg
        [ Svg.Attributes.width "10"
        , Svg.Attributes.height "10"
        , Svg.Attributes.viewBox "0 0 1030 638"
        , Svg.Attributes.fill "white"
        ]
        [ Svg.path
            [ Svg.Attributes.d "M1017 68L541 626q-11 12-26 12t-26-12L13 68Q-3 49 6 24.5T39 0h952q24 0 33 24.5t-7 43.5z"
            ]
            []
        ]


viewAnalyzeButton : Model -> Element.Element Msg
viewAnalyzeButton model =
    let
        conditionalStyles =
            if model.analyzed then
                [ Element.htmlAttribute
                    (Html.Attributes.style "cursor" "not-allowed")
                , Element.Background.color (Element.rgb 0.8 0.8 0.8)
                , Element.Font.color (Element.rgb 1.0 1.0 1.0)
                ]

            else
                [ Element.Font.color (Element.rgb 1 1 1)
                , Element.Background.color (Element.rgb255 18 147 216)
                , Element.mouseOver
                    [ Element.Background.color (Element.rgb255 47 127 190)
                    ]
                ]
    in
    Element.Input.button
        ([ Element.Font.size 16
         , Element.Font.color (Element.rgb 1 1 1)
         , Element.padding 10
         , Element.Border.rounded 2
         , Element.alignLeft
         ]
            ++ conditionalStyles
        )
        { onPress = Just AnalyzeClicked
        , label = Element.text "Analyze"
        }


viewRow : Key -> Row -> Element.Element Msg
viewRow currentKey (Row a b c d) =
    let
        maybeViewMeasure measure =
            Element.el [ Element.width <| Element.fillPortion 1 ]
                (Maybe.map (viewMeasure currentKey) measure
                    |> Maybe.withDefault Element.none
                )
    in
    Element.row
        [ Element.width Element.fill ]
        [ viewBarline
        , maybeViewMeasure a
        , maybeViewMeasure b
        , maybeViewMeasure c
        , maybeViewMeasure d
        ]


viewMeasure : Key -> Measure -> Element.Element Msg
viewMeasure currentKey (Measure first second) =
    Element.row
        [ Element.width Element.fill
        ]
        [ Element.row
            [ Element.spacing 30
            , Element.paddingXY 15 0
            , Element.width Element.fill
            ]
            [ Element.el
                [ Element.width Element.fill ]
                (viewMaybeEntry currentKey first)
            , Element.el
                [ Element.width Element.fill ]
                (viewMaybeEntry currentKey second)
            ]
        , viewBarline
        ]


viewBarline : Element.Element Msg
viewBarline =
    Element.el
        [ Element.width (Element.px 3)
        , Element.height (Element.px 80)
        , Element.Background.color (Element.rgb 0 0 0)
        ]
        Element.none


viewMaybeEntry : Key -> Maybe Entry -> Element.Element Msg
viewMaybeEntry currentKey maybeEntry =
    case maybeEntry of
        Just entry ->
            Element.column
                [ Element.Font.size 24
                , Element.Font.bold
                , Element.width Element.fill
                , Element.spacing 18
                ]
                [ Element.text (Chord.toString entry.chord)
                , viewAnalysis currentKey entry.analysis
                ]

        Nothing ->
            Element.none


viewAnalysis : Key -> Maybe Analysis -> Element.Element Msg
viewAnalysis currentKey maybeAnalysis =
    case maybeAnalysis of
        Just analysis ->
            Element.el
                [ Element.Font.size 16
                , Element.Font.bold
                , Element.Font.color (Element.rgb 0.5 0.5 0.5)
                ]
                (Element.text (Analysis.toString currentKey analysis))

        Nothing ->
            Element.none
