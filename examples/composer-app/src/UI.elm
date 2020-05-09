module UI exposing (layout, viewSequence)

import Document exposing (Document)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Generated.Route as Route exposing (Route)
import MusicTheory.Letter as Letter
import MusicTheory.Note as Note
import MusicTheory.Pitch as Pitch
import MusicTheory.PitchClass as PitchClass
import MusicTheory.Sequence as Sequence exposing (NoteEntry(..))


layout : { page : Document msg } -> Document msg
layout { page } =
    { title = page.title
    , body =
        [ column
            [ spacing 32
            , padding 20
            , width fill
            , height fill
            , centerX
            ]
            [ navbar
            , column [ height fill ] page.body
            , footer
            ]
        ]
    }


viewSequence : Sequence.Sequence -> Element msg
viewSequence theSequence =
    Sequence.getNoteEntries theSequence
        |> List.map viewNoteEntry
        |> Element.row [ Element.spacing 10 ]


viewNoteEntry : Sequence.NoteEntry -> Element msg
viewNoteEntry theEntry =
    case theEntry of
        EntryNote { startTime, note } ->
            Element.text
                ((Note.pitch note
                    |> Pitch.pitchClass
                    |> PitchClass.letter
                    |> Letter.toString
                 )
                    ++ (Note.pitch note
                            |> Pitch.pitchClass
                            |> PitchClass.offset
                            |> (\num ->
                                    if num >= 0 then
                                        List.repeat num "#"

                                    else
                                        List.repeat (num * -1) "b"
                               )
                            |> String.join ""
                       )
                )

        EntryTuplet { startTime, tuplet } ->
            Element.row [ spacing 10 ]
                ([ Element.text ("[ " ++ String.fromInt (List.length tuplet.entries))
                 ]
                    ++ List.map viewNoteEntry tuplet.entries
                    ++ [ Element.text "]" ]
                )

        EntryRest { startTime, duration } ->
            Element.text "_"


navbar : Element msg
navbar =
    row [ width fill ]
        [ link ( "Home", Route.Top )
        ]


link : ( String, Route ) -> Element msg
link ( label, route ) =
    Element.link styles.link
        { label = text label
        , url = Route.toHref route
        }


footer : Element msg
footer =
    row [] []



-- STYLES


colors =
    { white = rgb255 215 216 204
    , white1 = rgb255 189 186 182
    , white2 = rgb255 224 228 216
    , blue = rgb255 77 104 133
    , blue2 = rgb255 81 89 113
    , blue3 = rgb255 73 109 121
    , red = rgb255 144 66 68
    , red1 = rgb255 120 75 53
    , red2 = rgb255 131 72 88
    , yellow = rgb255 183 153 59
    , yellow1 = rgb255 146 154 82
    , yellow2 = rgb255 184 156 83
    , purple = rgb255 105 80 128
    , purple1 = rgb255 103 78 108
    , purple2 = rgb255 103 87 134
    }


styles :
    { link : List (Element.Attribute msg)
    , button : List (Element.Attribute msg)
    }
styles =
    { link =
        [ Font.underline
        , Font.color colors.blue
        , mouseOver [ alpha 0.6 ]
        ]
    , button =
        [ Font.color colors.white
        , Background.color colors.red
        , Border.rounded 4
        , paddingXY 24 10
        , mouseOver [ alpha 0.6 ]
        ]
    }
