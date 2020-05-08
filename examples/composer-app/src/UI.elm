module UI exposing (layout)

import Document exposing (Document)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Generated.Route as Route exposing (Route)


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


externalButtonLink : ( String, String ) -> Element msg
externalButtonLink ( label, url ) =
    Element.newTabLink styles.button
        { label = text label
        , url = url
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
