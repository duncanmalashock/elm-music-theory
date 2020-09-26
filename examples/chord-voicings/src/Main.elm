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


port abcOutput : String -> Cmd msg


port play : List Json.Encode.Value -> Cmd msg


port loadInstrumentById : Int -> Cmd msg


encodePitch : ( Music.Pitch.Pitch, Int ) -> Json.Encode.Value
encodePitch ( pitch, order ) =
    Json.Encode.object
        [ ( "time", Json.Encode.float (toFloat order * 200) )
        , ( "pitch", Json.Encode.int (Music.Pitch.toMIDINoteNumber pitch) )
        , ( "duration", Json.Encode.float 200 )
        , ( "volume", Json.Encode.int 60 )
        , ( "instrumentId", Json.Encode.int instrumentIds.piano )
        ]


playInBrowser : List ( Music.Pitch.Pitch, Int ) -> Cmd msg
playInBrowser noteEvents =
    noteEvents
        |> List.map encodePitch
        |> play


loadInstrument : Int -> Cmd msg
loadInstrument id =
    loadInstrumentById id


instrumentIds =
    { piano = 1
    }


type alias Flags =
    ()


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.dropdownMenu of
        ( _, Open ) ->
            Browser.Events.onAnimationFrame (\_ -> DropdownClickedOut)

        ( _, ReadyToClose ) ->
            Browser.Events.onClick (Json.Decode.succeed DropdownClosed)

        ( _, Closed ) ->
            Sub.none


init : Flags -> ( Model, Cmd Msg )
init _ =
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
        , playNewVoicing initialModel
        ]
    )


dropdowns =
    { root = "root"
    , chordType = "chord-type"
    , voicingMethod = "voicing-method"
    , voicing = "voicing"
    }


type DropdownState
    = Open
    | ReadyToClose
    | Closed


type alias Model =
    { chordSelection : ChordSelection
    , dropdownMenu : ( String, DropdownState )
    }


type alias ChordSelection =
    { root : Maybe Music.PitchClass.PitchClass
    , chordType : Maybe Music.ChordType.ChordType
    , voicingMethod : Maybe ( String, Music.Voicing.FourPart.VoicingMethod )
    , voicingOptions : List Music.Voicing.FourPart.Voicing
    , voicing : Maybe Music.Voicing.FourPart.Voicing
    }


initChordSelection : ChordSelection
initChordSelection =
    { root = List.head rootOptions
    , chordType = List.head chordTypeOptions
    , voicingMethod = List.head voicingMethodOptions
    , voicingOptions = []
    , voicing = Nothing
    }


type Msg
    = DropdownClicked String
    | DropdownClickedOut
    | DropdownClosed
    | ChordSelectionFwd ChordSelectionMsg


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
        , voicing =
            List.head newVoicingOptions
    }


playNewVoicing : Model -> Cmd Msg
playNewVoicing model =
    model.chordSelection.voicing
        |> Maybe.map Music.Voicing.FourPart.toPitchList
        |> Maybe.withDefault []
        |> List.map (\pitch -> ( pitch, 0 ))
        |> playInBrowser


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
                , playNewVoicing newModel
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


getVoicingOptions :
    Maybe Music.PitchClass.PitchClass
    -> Maybe Music.ChordType.ChordType
    -> Maybe Music.Voicing.FourPart.VoicingMethod
    -> List Music.Voicing.FourPart.Voicing
getVoicingOptions maybeRoot maybeChordType maybeVoicingMethod =
    let
        voiceRange =
            Music.Range.range Music.Pitch.g3 Music.Pitch.c6
    in
    Maybe.map3
        (\root type_ vm ->
            Music.Chord.voiceFourParts
                { voiceOne = voiceRange
                , voiceTwo = voiceRange
                , voiceThree = voiceRange
                , voiceFour = voiceRange
                }
                [ vm ]
                (Music.Chord.custom root type_)
        )
        maybeRoot
        maybeChordType
        maybeVoicingMethod
        |> Maybe.withDefault []
        |> List.sortWith (Music.Voicing.FourPart.centerOrder Music.Pitch.g4)


view : Model -> { title : String, body : List (Html Msg) }
view model =
    { title = "Chord Voicings"
    , body =
        [ Element.layout
            [ Element.width Element.fill
            , Element.paddingEach { top = 50, right = 0, bottom = 0, left = 0 }
            ]
            (viewBody model)
        ]
    }


viewBody : Model -> Element.Element Msg
viewBody model =
    Element.column
        [ Element.width (Element.px 400)
        , Element.centerX
        ]
        [ viewAbc
        , viewSelectionControls model
        ]


viewSelectionControls : Model -> Element.Element Msg
viewSelectionControls model =
    let
        { root, chordType, voicingMethod, voicing } =
            { root = dropdowns.root
            , chordType = dropdowns.chordType
            , voicingMethod = dropdowns.voicingMethod
            , voicing = dropdowns.voicing
            }
    in
    Element.column
        [ Element.spacing 5
        , Element.width Element.fill
        ]
        [ Element.row
            [ Element.spacing 5
            , Element.width Element.fill
            ]
            [ viewDropdown root (currentRootDropdownLabel model) rootMenuOptions model
            , viewDropdown chordType (currentChordTypeDropdownLabel model) chordTypeMenuOptions model
            , viewDropdown voicingMethod (currentVoicingMethodDropdownLabel model) voicingMethodMenuOptions model
            ]
        , viewDropdown voicing (currentVoicingDropdownLabel model) (voicingMenuOptions model) model
        ]


viewAbc : Element.Element Msg
viewAbc =
    Element.el
        [ Element.htmlAttribute <| Html.Attributes.id "abcViewer"
        , Element.htmlAttribute <| Html.Attributes.style "flex-basis" "auto"
        ]
        Element.none


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


currentVoicingMethodDropdownLabel : Model -> String
currentVoicingMethodDropdownLabel model =
    case model.chordSelection.voicingMethod of
        Just ( name, _ ) ->
            name

        Nothing ->
            "—"


voicingMethodOptions : List ( String, Music.Voicing.FourPart.VoicingMethod )
voicingMethodOptions =
    [ ( "Close", Music.Voicing.FourPart.close )
    , ( "Drop-2", Music.Voicing.FourPart.drop2 )
    , ( "Drop-3", Music.Voicing.FourPart.drop3 )
    , ( "Drop-2-and-4", Music.Voicing.FourPart.drop2and4 )
    , ( "Spread", Music.Voicing.FourPart.spread )
    ]


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


viewDropdown : String -> String -> List ( String, Msg ) -> Model -> Element.Element Msg
viewDropdown id label options model =
    Element.Input.button
        [ Element.padding 10
        , Element.Border.rounded 3
        , Element.alignLeft
        , Element.Font.size 16
        , Element.Font.color (Element.rgb 1 1 1)
        , Element.Background.color (Element.rgb 0.3 0.5 0.9)
        , Element.mouseOver
            [ Element.Background.color (Element.rgb 0.2 0.4 0.8)
            ]
        , Element.below
            (viewMenu id options model)
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


viewMenu : String -> List ( String, Msg ) -> Model -> Element.Element Msg
viewMenu id options model =
    let
        keyMenuItems =
            options
                |> List.map
                    (\( label, msg ) ->
                        Element.Input.button
                            [ Element.Font.size 16
                            , Element.padding 10
                            , Element.alignLeft
                            , Element.Font.color (Element.rgb 0 0 0)
                            , Element.width Element.fill
                            , Element.mouseOver
                                [ Element.Background.color
                                    (Element.rgb 0.9 0.9 0.9)
                                ]
                            ]
                            { onPress = Just msg
                            , label = Element.text label
                            }
                    )

        menuView =
            Element.column
                [ Element.Background.color (Element.rgb 1 1 1)
                , Element.height (Element.px 165)
                , Element.htmlAttribute (Html.Attributes.style "overflow-y" "auto")
                , Element.Border.shadow
                    { offset = ( 0, 0 )
                    , size = 2
                    , blur = 2
                    , color = Element.rgba 0 0 0 0.2
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


newAbcOutput : Model -> Cmd msg
newAbcOutput model =
    let
        chordToAbc chord =
            chord.voicing
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
                accidentals ++ str ++ octave ++ "4"
           )
