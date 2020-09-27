port module Main exposing (main)

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
import Json.Encode
import Music.Chord
import Music.ChordType
import Music.Pitch
import Music.PitchClass
import Music.Range
import Music.Voicing.FourPart
import Svg
import Svg.Attributes


{-| This example includes the following features:

    (1) Generating valid chord voicings from the current selection
    (2) Port to ABC.js to show the chord on a musical staff
    (3) Port to WebAudioFont to play the chord in the browser
    (4) Dropdown menus for selecting a chord and its voicings

Look for numbered markings in comments to see where each behavior is implemented.

-}
main : Program () Model Msg
main =
    Browser.document
        { init = \() -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    dropdownMenuSubscriptions model


type alias Model =
    { chordSelection : ChordSelection
    , dropdownMenu : ( String, DropdownState )
    }


init : ( Model, Cmd Msg )
init =
    let
        initialModel : Model
        initialModel =
            { chordSelection = updateVoicings initChordSelection
            , dropdownMenu = ( "", Closed )
            }
    in
    ( initialModel
    , Cmd.batch
        [ newAbcOutput initialModel
        , loadInstrument instrumentIds.piano
        , playVoicing initialModel
        ]
    )


type Msg
    = DropdownClicked String
    | DropdownClickedOut
    | DropdownClosed
    | ChordSelectionFwd ChordSelectionMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChordSelectionFwd selectionMsg ->
            let
                newModel =
                    { model
                        | chordSelection =
                            updateChordSelection selectionMsg model.chordSelection
                    }
            in
            ( newModel
            , Cmd.batch
                [ newAbcOutput newModel
                , playVoicing newModel
                ]
            )

        DropdownClicked id ->
            ( { model
                | dropdownMenu = ( id, Open )
              }
            , Cmd.none
            )

        DropdownClickedOut ->
            ( { model
                | dropdownMenu =
                    case model.dropdownMenu of
                        ( id, _ ) ->
                            ( id, ReadyToClose )
              }
            , Cmd.none
            )

        DropdownClosed ->
            ( { model
                | dropdownMenu =
                    case model.dropdownMenu of
                        ( id, _ ) ->
                            ( id, Closed )
              }
            , Cmd.none
            )


view : Model -> { title : String, body : List (Html Msg) }
view model =
    { title = "Chord Voicings"
    , body =
        [ Element.layout
            [ Element.width Element.fill
            , Element.Font.family [ Element.Font.typeface "IBM Plex Sans" ]
            ]
            (viewBody model)
        ]
    }


viewBody : Model -> Element.Element Msg
viewBody model =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 50
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
                    (Element.text "Example: \"Chord Voicings\"")
                ]
            )
        , Element.column
            [ Element.width (Element.px 500)
            , Element.centerX
            ]
            [ viewAbc
            , viewSelectionControls model
            ]
        ]



-- (1) Generating valid chord voicings from the current selection


type alias ChordSelection =
    { root : Maybe Music.PitchClass.PitchClass
    , chordType : Maybe Music.ChordType.ChordType
    , voicingMethod : Maybe ( String, Music.Voicing.FourPart.VoicingMethod )
    , voicingOptions : List Music.Voicing.FourPart.Voicing
    , voicing : Maybe Music.Voicing.FourPart.Voicing
    }


updateVoicings : ChordSelection -> ChordSelection
updateVoicings selection =
    let
        newVoicingOptions =
            getVoicingOptions
                selection.root
                selection.chordType
                (Maybe.map Tuple.second selection.voicingMethod)
    in
    { selection
        | voicingOptions = newVoicingOptions
        , voicing = List.head newVoicingOptions
    }


getVoicingOptions :
    Maybe Music.PitchClass.PitchClass
    -> Maybe Music.ChordType.ChordType
    -> Maybe Music.Voicing.FourPart.VoicingMethod
    -> List Music.Voicing.FourPart.Voicing
getVoicingOptions maybeRoot maybeChordType maybeVoicingMethod =
    let
        voiceRange : Music.Range.Range
        voiceRange =
            Music.Range.range Music.Pitch.g3 Music.Pitch.c6
    in
    Maybe.map3
        (\chordRoot chordType voicingMethod ->
            -- Generate all voicings for the chord
            Music.Chord.voiceFourParts
                -- All four voices in this example will use pitches within the same range.
                -- Vary this if you are generating voicings for different instruments.
                { voiceOne = voiceRange
                , voiceTwo = voiceRange
                , voiceThree = voiceRange
                , voiceFour = voiceRange
                }
                -- We are using the voicing method from the selection.
                [ voicingMethod ]
                -- Use `Music.Chord.custom` to create the chord from its root and type
                (Music.Chord.custom chordRoot chordType)
        )
        maybeRoot
        maybeChordType
        maybeVoicingMethod
        |> Maybe.withDefault []
        -- Sort to prefer voicings that are centered on the pitch G4
        |> List.sortWith
            (Music.Voicing.FourPart.centerOrder Music.Pitch.g4)


initChordSelection : ChordSelection
initChordSelection =
    { root = List.head rootOptions
    , chordType = List.head chordTypeOptions
    , voicingMethod = List.head voicingMethodOptions
    , voicingOptions = []
    , voicing = Nothing
    }


type ChordSelectionMsg
    = NewRoot Music.PitchClass.PitchClass
    | NewChordType Music.ChordType.ChordType
    | NewVoicingMethod ( String, Music.Voicing.FourPart.VoicingMethod )
    | NewVoicing Music.Voicing.FourPart.Voicing


updateChordSelection : ChordSelectionMsg -> ChordSelection -> ChordSelection
updateChordSelection chordSelectionMsg selection =
    (case chordSelectionMsg of
        NewRoot root ->
            { selection
                | root = Just root
            }

        NewChordType chordType ->
            { selection
                | chordType = Just chordType
            }

        NewVoicingMethod ( string, voicingMethod ) ->
            { selection
                | voicingMethod = Just ( string, voicingMethod )
            }

        NewVoicing voicing ->
            { selection
                | voicing = Just voicing
            }
    )
        |> (\updatedSelection ->
                case chordSelectionMsg of
                    NewVoicing _ ->
                        updatedSelection

                    _ ->
                        updateVoicings updatedSelection
           )



-- (2) Port to ABC.js to show the chord on a musical staff


port abcOutput : String -> Cmd msg


viewAbc : Element.Element Msg
viewAbc =
    Element.el
        [ Element.htmlAttribute <| Html.Attributes.id "abcViewer"
        , Element.htmlAttribute <| Html.Attributes.style "flex-basis" "auto"
        ]
        Element.none


newAbcOutput : Model -> Cmd msg
newAbcOutput model =
    let
        chordToAbc chord =
            chord.voicing
                -- Get the pitches in the voicing
                |> Maybe.map Music.Voicing.FourPart.toPitchList
                |> Maybe.withDefault []
                |> List.map pitchToAbc
                |> String.join ""
                |> (\str -> "[" ++ str ++ "]")
    in
    [ "X:1\n", "K:C\n", chordToAbc model.chordSelection ]
        |> String.join " "
        |> abcOutput


pitchToAbc : Music.Pitch.Pitch -> String
pitchToAbc pitch =
    let
        -- Use the `Music.Pitch.octave`, `Music.PitchClass.letter`, and `Music.PitchClass.accidentals`
        -- functions to create a custom string representation to match the ABC format:
        octave =
            (Music.Pitch.octave pitch - 5)
                |> (\oct ->
                        if oct == 0 then
                            ""

                        else if oct > 0 then
                            List.repeat oct "'"
                                |> String.join ""

                        else
                            List.repeat (abs oct) ","
                                |> String.join ""
                   )

        accidentals =
            Music.PitchClass.fromPitch pitch
                |> Music.PitchClass.accidentals
                |> (\acc ->
                        if acc == 0 then
                            ""

                        else if acc > 0 then
                            List.repeat acc "^"
                                |> String.join ""

                        else
                            List.repeat (abs acc) "_"
                                |> String.join ""
                   )
    in
    Music.PitchClass.fromPitch pitch
        |> Music.PitchClass.letter
        |> (\l ->
                case l of
                    Music.PitchClass.A ->
                        "a"

                    Music.PitchClass.B ->
                        "b"

                    Music.PitchClass.C ->
                        "c"

                    Music.PitchClass.D ->
                        "d"

                    Music.PitchClass.E ->
                        "e"

                    Music.PitchClass.F ->
                        "f"

                    Music.PitchClass.G ->
                        "g"
           )
        |> (\str ->
                accidentals ++ str ++ octave ++ "8"
           )



-- (3) Port to WebAudioFont to play the chord in the browser


port play : List Json.Encode.Value -> Cmd msg


playVoicing : Model -> Cmd Msg
playVoicing model =
    model.chordSelection.voicing
        |> Maybe.map Music.Voicing.FourPart.toPitchList
        |> Maybe.withDefault []
        |> List.map (\pitch -> ( pitch, 0 ))
        |> List.map encodePitch
        |> play


encodePitch : ( Music.Pitch.Pitch, Int ) -> Json.Encode.Value
encodePitch ( pitch, order ) =
    Json.Encode.object
        [ ( "time", Json.Encode.float (toFloat order * 200) )
        , ( "pitch"
          , Json.Encode.int
                -- Convert to MIDI note number for playback
                (Music.Pitch.toMIDINoteNumber pitch)
          )
        , ( "duration", Json.Encode.float 200 )
        , ( "volume", Json.Encode.int 60 )
        , ( "instrumentId", Json.Encode.int instrumentIds.piano )
        ]


port loadInstrumentById : Int -> Cmd msg


loadInstrument : Int -> Cmd msg
loadInstrument id =
    loadInstrumentById id


instrumentIds =
    { piano = 1
    }



-- (4) Dropdown menus for selecting a chord and its voicings


viewSelectionControls : Model -> Element.Element Msg
viewSelectionControls model =
    let
        { root, chordType, voicingMethod, voicing } =
            { root = dropdownIds.root
            , chordType = dropdownIds.chordType
            , voicingMethod = dropdownIds.voicingMethod
            , voicing = dropdownIds.voicing
            }
    in
    Element.column
        [ Element.spacing 15
        , Element.width Element.fill
        ]
        [ Element.row
            [ Element.spacing 5
            , Element.width Element.fill
            ]
            [ viewDropdown "Root" root (currentRootDropdownLabel model) rootMenuOptions model
            , viewDropdown "Chord type" chordType (currentChordTypeDropdownLabel model) chordTypeMenuOptions model
            , viewDropdown "Voicing method" voicingMethod (currentVoicingMethodDropdownLabel model) voicingMethodMenuOptions model
            ]
        , viewDropdown "Voicing" voicing (currentVoicingDropdownLabel model) (voicingMenuOptions model) model
        ]


currentRootDropdownLabel : Model -> String
currentRootDropdownLabel model =
    Maybe.map Music.PitchClass.toString model.chordSelection.root
        |> Maybe.withDefault "—"


rootOptions : List Music.PitchClass.PitchClass
rootOptions =
    [ Music.PitchClass.c
    , Music.PitchClass.dFlat
    , Music.PitchClass.d
    , Music.PitchClass.eFlat
    , Music.PitchClass.e
    , Music.PitchClass.f
    , Music.PitchClass.fSharp
    , Music.PitchClass.g
    , Music.PitchClass.aFlat
    , Music.PitchClass.a
    , Music.PitchClass.bFlat
    , Music.PitchClass.b
    ]


rootMenuOptions : List ( String, Msg )
rootMenuOptions =
    rootOptions
        |> List.map
            (\pc ->
                ( Music.PitchClass.toString pc, ChordSelectionFwd (NewRoot pc) )
            )


currentChordTypeDropdownLabel : Model -> String
currentChordTypeDropdownLabel model =
    Maybe.map Music.ChordType.toString model.chordSelection.chordType
        |> Maybe.withDefault "—"


chordTypeOptions : List Music.ChordType.ChordType
chordTypeOptions =
    [ Music.ChordType.majorSeventh
    , Music.ChordType.majorSix
    , Music.ChordType.minorSeventh
    , Music.ChordType.minorSix
    , Music.ChordType.dominantSeventh
    , Music.ChordType.dominantSeventhSharpNine
    , Music.ChordType.diminishedSeventh
    , Music.ChordType.halfDiminishedSeventh
    ]


chordTypeMenuOptions : List ( String, Msg )
chordTypeMenuOptions =
    chordTypeOptions
        |> List.map
            (\ct ->
                ( Music.ChordType.toString ct
                , ChordSelectionFwd (NewChordType ct)
                )
            )


voicingMethodOptions : List ( String, Music.Voicing.FourPart.VoicingMethod )
voicingMethodOptions =
    -- `VoicingMethod`s contain functions, and function equality is undecidable in Elm.
    -- If you need to test equality, do it on a String label like I have done here, or do similarly:
    [ ( "Close", Music.Voicing.FourPart.close )
    , ( "Drop-2", Music.Voicing.FourPart.drop2 )
    , ( "Drop-3", Music.Voicing.FourPart.drop3 )
    , ( "Drop-2-and-4", Music.Voicing.FourPart.drop2and4 )
    , ( "Spread", Music.Voicing.FourPart.spread )
    ]


currentVoicingMethodDropdownLabel : Model -> String
currentVoicingMethodDropdownLabel model =
    case model.chordSelection.voicingMethod of
        Just ( name, _ ) ->
            name

        Nothing ->
            "—"


voicingMethodMenuOptions : List ( String, Msg )
voicingMethodMenuOptions =
    voicingMethodOptions
        |> List.map
            (\( name, voicingMethod ) ->
                ( name
                , ChordSelectionFwd
                    (NewVoicingMethod ( name, voicingMethod ))
                )
            )


currentVoicingDropdownLabel : Model -> String
currentVoicingDropdownLabel model =
    Maybe.map Music.Voicing.FourPart.toString model.chordSelection.voicing
        |> Maybe.withDefault "—"


voicingMenuOptions : Model -> List ( String, Msg )
voicingMenuOptions model =
    model.chordSelection.voicingOptions
        |> List.map
            (\v ->
                ( Music.Voicing.FourPart.toString v, ChordSelectionFwd (NewVoicing v) )
            )


viewDropdown : String -> String -> String -> List ( String, Msg ) -> Model -> Element.Element Msg
viewDropdown header id label options model =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 8
        ]
        [ Element.el
            [ Element.Font.size 16
            , Element.Font.color (Element.rgb255 18 147 216)
            , Element.Font.bold
            ]
            (Element.text header)
        , Element.Input.button
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
                (viewMenu id options label model)
            , Element.width Element.fill
            ]
            { onPress =
                case model.dropdownMenu of
                    ( _, Closed ) ->
                        Just (DropdownClicked id)

                    _ ->
                        Nothing
            , label =
                Element.row
                    [ Element.spacing 10
                    , Element.width Element.fill
                    ]
                    [ Element.el []
                        (Element.text label)
                    , Element.el
                        [ Element.alignRight
                        ]
                        (Element.html viewDropdownArrow)
                    ]
            }
        ]


viewMenu : String -> List ( String, Msg ) -> String -> Model -> Element.Element Msg
viewMenu id options selectedLabel model =
    let
        keyMenuItems =
            options
                |> List.map
                    (\( label, msg ) ->
                        Element.Input.button
                            ([ Element.Font.size 16
                             , Element.padding 10
                             , Element.alignLeft
                             , Element.width Element.fill
                             , Element.mouseOver
                                [ Element.Background.color
                                    (Element.rgb255 18 147 216)
                                , Element.Font.color (Element.rgb 1 1 1)
                                ]
                             ]
                                ++ (if selectedLabel == label then
                                        [ Element.Font.color (Element.rgb255 18 147 216)
                                        ]

                                    else
                                        [ Element.Font.color (Element.rgb 0 0 0)
                                        ]
                                   )
                            )
                            { onPress = Just msg
                            , label = Element.text label
                            }
                    )

        menuView =
            Element.column
                [ Element.Background.color (Element.rgb 1 1 1)
                , Element.height (Element.px 165)
                , Element.htmlAttribute (Html.Attributes.style "width" "100%")
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
    in
    case model.dropdownMenu of
        ( currentId, state ) ->
            if currentId == id && state /= Closed then
                menuView

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


dropdownIds =
    { root = "root"
    , chordType = "chord-type"
    , voicingMethod = "voicing-method"
    , voicing = "voicing"
    }


type DropdownState
    = Open
    | ReadyToClose
    | Closed


dropdownMenuSubscriptions : Model -> Sub Msg
dropdownMenuSubscriptions model =
    case model.dropdownMenu of
        ( _, Open ) ->
            Browser.Events.onAnimationFrame (\_ -> DropdownClickedOut)

        ( _, ReadyToClose ) ->
            Browser.Events.onClick (Json.Decode.succeed DropdownClosed)

        ( _, Closed ) ->
            Sub.none
