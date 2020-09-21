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
import Music.ChordType
import Music.PitchClass
import Music.Voicing.FourPart
import Svg
import Svg.Attributes


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
    , voicingMethod : Maybe Music.Voicing.FourPart.VoicingMethod
    , voicing : Maybe Music.Voicing.FourPart.Voicing
    }


initChordSelection : ChordSelection
initChordSelection =
    { root = Nothing
    , chordType = Nothing
    , voicingMethod = Nothing
    , voicing = Nothing
    }


type Msg
    = DropdownClicked String
    | DropdownClickedOut
    | DropdownClosed
    | NewRootChosen Music.PitchClass.PitchClass


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
                    }
              }
            , Cmd.none
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
            ]
            (viewDropdown (currentRootDropdownLabel model) rootOptions model)
        ]
    }


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


viewDropdown : String -> List ( String, Msg ) -> Model -> Element.Element Msg
viewDropdown label options model =
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
            (viewMenu dropdowns.rootOne options model)
        ]
        { onPress =
            case model.dropdownMenu of
                ( _, Closed ) ->
                    Just (DropdownClicked dropdowns.rootOne)

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
                , Element.width (Element.px 150)
                , Element.height (Element.px 200)
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
