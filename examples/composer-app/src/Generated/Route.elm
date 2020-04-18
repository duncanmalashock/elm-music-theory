module Generated.Route exposing
    ( Route(..)
    , fromUrl
    , toHref
    )

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = Home
    | NotFound


fromUrl : Url -> Maybe Route
fromUrl =
    Parser.parse routes


routes : Parser (Route -> a) a
routes =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Home (Parser.s "home")
        , Parser.map NotFound (Parser.s "not-found")
        ]


toHref : Route -> String
toHref route =
    let
        segments : List String
        segments =
            case route of
                Home ->
                    [ "home" ]

                NotFound ->
                    [ "not-found" ]
    in
    segments
        |> String.join "/"
        |> String.append "/"
