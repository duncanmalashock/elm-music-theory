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
import Music.Chord
import Music.ChordType
import Music.Pitch
import Music.PitchClass
import Music.Range
import Music.Voicing.FourPart
import Svg
import Svg.Attributes


port abcOutput : String -> Cmd msg


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


init : Flags -> ( Model, Cmd msg )
init _ =
    let
        initialModel =
            { chordOne = initChordSelection
            , chordTwo = initChordSelection
            , dropdownMenu = ( "", Closed )
            }
    in
    ( initialModel
    , newAbcOutput initialModel
    )


dropdowns =
    { rootOne = "root-one"
    , chordTypeOne = "chord-type-one"
    , voicingMethodOne = "voicing-method-one"
    , voicingOne = "voicing-one"
    , rootTwo = "root-two"
    , chordTypeTwo = "chord-type-two"
    , voicingMethodTwo = "voicing-method-two"
    , voicingTwo = "voicing-two"
    }


type DropdownState
    = Open
    | ReadyToClose
    | Closed


type alias Model =
    { chordOne : ChordSelection
    , chordTwo : ChordSelection
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
    { root = Nothing
    , chordType = Nothing
    , voicingMethod = Nothing
    , voicingOptions = []
    , voicing = Nothing
    }


type Msg
    = DropdownClicked String
    | DropdownClickedOut
    | DropdownClosed
    | ChordSelectionFwd SelectionId ChordSelectionMsg


type SelectionId
    = FirstSelection
    | SecondSelection


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
                let
                    newVoicingOptions =
                        getVoicingOptions
                            updatedSelection.root
                            updatedSelection.chordType
                            (Maybe.map Tuple.second updatedSelection.voicingMethod)
                in
                { updatedSelection
                    | voicingOptions = newVoicingOptions
                    , voicing =
                        case chordSelectionMsg of
                            NewVoicing v ->
                                Just v

                            _ ->
                                List.head newVoicingOptions
                }
           )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChordSelectionFwd selectionId selectionMsg ->
            let
                newModel =
                    case selectionId of
                        FirstSelection ->
                            { model
                                | chordOne =
                                    updateChordSelection selectionMsg model.chordOne
                            }

                        SecondSelection ->
                            { model
                                | chordTwo =
                                    updateChordSelection selectionMsg model.chordTwo
                            }
            in
            ( newModel
            , newAbcOutput newModel
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
            ]
            (viewBody model)
        ]
    }


viewBody : Model -> Element.Element Msg
viewBody model =
    Element.column
        []
        [ viewAbc
        , viewSelectionControls model FirstSelection
        , viewSelectionControls model SecondSelection
        ]


viewSelectionControls : Model -> SelectionId -> Element.Element Msg
viewSelectionControls model selectionId =
    let
        { root, chordType, voicingMethod, voicing } =
            case selectionId of
                FirstSelection ->
                    { root = dropdowns.rootOne
                    , chordType = dropdowns.chordTypeOne
                    , voicingMethod = dropdowns.voicingMethodOne
                    , voicing = dropdowns.voicingOne
                    }

                SecondSelection ->
                    { root = dropdowns.rootTwo
                    , chordType = dropdowns.chordTypeTwo
                    , voicingMethod = dropdowns.voicingMethodTwo
                    , voicing = dropdowns.voicingTwo
                    }
    in
    Element.row
        [ Element.spacing 5 ]
        [ viewDropdown root (currentRootDropdownLabel selectionId model) (rootOptions selectionId) model
        , viewDropdown chordType (currentChordTypeDropdownLabel selectionId model) (chordTypeOptions selectionId) model
        , viewDropdown voicingMethod (currentVoicingMethodDropdownLabel selectionId model) (voicingMethodOptions selectionId) model
        , viewDropdown voicing (currentVoicingDropdownLabel selectionId model) (voicingOptions selectionId model) model
        ]


viewAbc : Element.Element Msg
viewAbc =
    Element.el
        [ Element.htmlAttribute <| Html.Attributes.id "abcViewer"
        , Element.htmlAttribute <| Html.Attributes.style "flex-basis" "auto"
        ]
        Element.none


currentRootDropdownLabel : SelectionId -> Model -> String
currentRootDropdownLabel selectionId model =
    let
        root =
            case selectionId of
                FirstSelection ->
                    model.chordOne.root

                SecondSelection ->
                    model.chordTwo.root
    in
    Maybe.map Music.PitchClass.toString root
        |> Maybe.withDefault "—"


rootOptions : SelectionId -> List ( String, Msg )
rootOptions selectionId =
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
        |> List.map
            (\pc ->
                ( Music.PitchClass.toString pc, ChordSelectionFwd selectionId (NewRoot pc) )
            )


currentChordTypeDropdownLabel : SelectionId -> Model -> String
currentChordTypeDropdownLabel selectionId model =
    let
        chordType =
            case selectionId of
                FirstSelection ->
                    model.chordOne.chordType

                SecondSelection ->
                    model.chordTwo.chordType
    in
    Maybe.map Music.ChordType.toString chordType
        |> Maybe.withDefault "—"


chordTypeOptions : SelectionId -> List ( String, Msg )
chordTypeOptions selectionId =
    [ Music.ChordType.majorSeventh
    , Music.ChordType.minorSeventh
    , Music.ChordType.dominantSeventh
    , Music.ChordType.majorSixNine
    , Music.ChordType.diminishedSeventh
    , Music.ChordType.halfDiminishedSeventh
    , Music.ChordType.minorEleventh
    , Music.ChordType.dominantSeventhSus4
    , Music.ChordType.dominantSeventhSharpNine
    ]
        |> List.map
            (\ct ->
                ( Music.ChordType.toString ct, ChordSelectionFwd selectionId (NewChordType ct) )
            )


currentVoicingMethodDropdownLabel : SelectionId -> Model -> String
currentVoicingMethodDropdownLabel selectionId model =
    let
        voicingMethod =
            case selectionId of
                FirstSelection ->
                    model.chordOne.voicingMethod

                SecondSelection ->
                    model.chordTwo.voicingMethod
    in
    case voicingMethod of
        Just ( name, _ ) ->
            name

        Nothing ->
            "—"


voicingMethodOptions : SelectionId -> List ( String, Msg )
voicingMethodOptions selectionId =
    [ ( "Close", ChordSelectionFwd selectionId (NewVoicingMethod ( "Close", Music.Voicing.FourPart.close )) )
    , ( "Drop-2", ChordSelectionFwd selectionId (NewVoicingMethod ( "Drop-2", Music.Voicing.FourPart.drop2 )) )
    , ( "Drop-3", ChordSelectionFwd selectionId (NewVoicingMethod ( "Drop-3", Music.Voicing.FourPart.drop3 )) )
    , ( "Drop-2-and-4", ChordSelectionFwd selectionId (NewVoicingMethod ( "Drop-2-and-4", Music.Voicing.FourPart.drop2and4 )) )
    , ( "Spread", ChordSelectionFwd selectionId (NewVoicingMethod ( "Spread", Music.Voicing.FourPart.spread )) )
    ]


currentVoicingDropdownLabel : SelectionId -> Model -> String
currentVoicingDropdownLabel selectionId model =
    let
        voicing =
            case selectionId of
                FirstSelection ->
                    model.chordOne.voicing

                SecondSelection ->
                    model.chordTwo.voicing
    in
    Maybe.map Music.Voicing.FourPart.toString voicing
        |> Maybe.withDefault "—"


voicingOptions : SelectionId -> Model -> List ( String, Msg )
voicingOptions selectionId model =
    let
        newVoicingOptions =
            case selectionId of
                FirstSelection ->
                    model.chordOne.voicingOptions

                SecondSelection ->
                    model.chordTwo.voicingOptions
    in
    newVoicingOptions
        |> List.map
            (\v ->
                ( Music.Voicing.FourPart.toString v, ChordSelectionFwd selectionId (NewVoicing v) )
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
                ]
                [ Element.el []
                    (Element.text label)
                , Element.el []
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

                --, Element.width (Element.px 150)
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
    [ "X:1\n", "K:C\n", chordToAbc model.chordOne, chordToAbc model.chordTwo ]
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
