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


newAbcOutput : Model -> Cmd msg
newAbcOutput model =
    let
        chordOne =
            model.chordOne.voicing
                |> Maybe.map Music.Voicing.FourPart.toPitchList
                |> Maybe.withDefault []
                |> List.map pitchToAbc
                |> String.join ""
                |> (\str -> "[" ++ str ++ "]")
    in
    [ "X:1\n", "K:C\n", chordOne, chordOne ]
        |> String.join " "
        |> Debug.log "abcoutput"
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
        ( id, Open ) ->
            Browser.Events.onAnimationFrame (\_ -> DropdownClickedOut)

        ( id, ReadyToClose ) ->
            Browser.Events.onClick (Json.Decode.succeed DropdownClosed)

        ( id, Closed ) ->
            Sub.none


init : Flags -> ( Model, Cmd msg )
init flags =
    ( { chordOne = initChordSelection
      , chordTwo = initChordSelection
      , dropdownMenu = ( "", Closed )
      }
    , Cmd.none
    )


dropdowns =
    { rootOne = "root-one"
    , chordTypeOne = "chord-type-one"
    , voicingMethodOne = "voicing-method-one"
    , voicingOne = "voicing-one"
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
    | NewRootChosen Music.PitchClass.PitchClass
    | NewChordTypeChosen Music.ChordType.ChordType
    | NewVoicingMethodChosen ( String, Music.Voicing.FourPart.VoicingMethod )
    | NewVoicingChosen Music.Voicing.FourPart.Voicing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewRootChosen root ->
            let
                chordOne =
                    model.chordOne
            in
            ( { model
                | chordOne =
                    { chordOne
                        | root = Just root
                        , voicing = Nothing
                        , voicingOptions =
                            getVoicingOptions
                                (Just root)
                                chordOne.chordType
                                (Maybe.map Tuple.second chordOne.voicingMethod)
                    }
              }
            , Cmd.none
            )

        NewChordTypeChosen chordType ->
            let
                chordOne =
                    model.chordOne
            in
            ( { model
                | chordOne =
                    { chordOne
                        | chordType = Just chordType
                        , voicing = Nothing
                        , voicingOptions =
                            getVoicingOptions
                                chordOne.root
                                (Just chordType)
                                (Maybe.map Tuple.second chordOne.voicingMethod)
                    }
              }
            , Cmd.none
            )

        NewVoicingMethodChosen vm ->
            let
                chordOne =
                    model.chordOne
            in
            ( { model
                | chordOne =
                    { chordOne
                        | voicingMethod = Just vm
                        , voicing = Nothing
                        , voicingOptions =
                            getVoicingOptions
                                chordOne.root
                                chordOne.chordType
                                (Just <| Tuple.second vm)
                    }
              }
            , Cmd.none
            )

        NewVoicingChosen v ->
            let
                chordOne =
                    model.chordOne

                newModel =
                    { model
                        | chordOne =
                            { chordOne
                                | voicing = Just v
                            }
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
        [ viewAbc model
        , Element.row
            [ Element.spacing 5 ]
            [ viewDropdown dropdowns.rootOne (currentRootDropdownLabel model) rootOptions model
            , viewDropdown dropdowns.chordTypeOne (currentChordTypeDropdownLabel model) chordTypeOptions model
            , viewDropdown dropdowns.voicingMethodOne (currentVoicingMethodDropdownLabel model) voicingMethodOptions model
            , viewDropdown dropdowns.voicingOne (currentVoicingDropdownLabel model) (voicingOptions model) model
            ]
        ]


viewAbc : Model -> Element.Element Msg
viewAbc model =
    Element.el
        [ Element.htmlAttribute <| Html.Attributes.id "abcViewer"
        , Element.htmlAttribute <| Html.Attributes.style "flex-basis" "auto"
        ]
        Element.none


currentRootDropdownLabel : Model -> String
currentRootDropdownLabel model =
    Maybe.map Music.PitchClass.toString model.chordOne.root
        |> Maybe.withDefault "—"


rootOptions : List ( String, Msg )
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
        |> List.map
            (\pc ->
                ( Music.PitchClass.toString pc, NewRootChosen pc )
            )


currentChordTypeDropdownLabel : Model -> String
currentChordTypeDropdownLabel model =
    Maybe.map Music.ChordType.toString model.chordOne.chordType
        |> Maybe.withDefault "—"


chordTypeOptions : List ( String, Msg )
chordTypeOptions =
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
                ( Music.ChordType.toString ct, NewChordTypeChosen ct )
            )


currentVoicingMethodDropdownLabel : Model -> String
currentVoicingMethodDropdownLabel model =
    case model.chordOne.voicingMethod of
        Just ( name, _ ) ->
            name

        Nothing ->
            "—"


voicingMethodOptions : List ( String, Msg )
voicingMethodOptions =
    [ ( "Close", NewVoicingMethodChosen ( "Close", Music.Voicing.FourPart.close ) )
    , ( "Drop-2", NewVoicingMethodChosen ( "Drop-2", Music.Voicing.FourPart.drop2 ) )
    , ( "Drop-3", NewVoicingMethodChosen ( "Drop-3", Music.Voicing.FourPart.drop3 ) )
    , ( "Drop-2-and-4", NewVoicingMethodChosen ( "Drop-2-and-4", Music.Voicing.FourPart.drop2and4 ) )
    , ( "Spread", NewVoicingMethodChosen ( "Spread", Music.Voicing.FourPart.spread ) )
    ]


currentVoicingDropdownLabel : Model -> String
currentVoicingDropdownLabel model =
    Maybe.map Music.Voicing.FourPart.toString model.chordOne.voicing
        |> Maybe.withDefault "—"


voicingOptions : Model -> List ( String, Msg )
voicingOptions model =
    model.chordOne.voicingOptions
        |> List.map
            (\v ->
                ( Music.Voicing.FourPart.toString v, NewVoicingChosen v )
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
