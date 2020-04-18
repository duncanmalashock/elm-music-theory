module Pages.Home exposing (Flags, Model, Msg, page)

import Element
import Element.Input as Input
import Global
import Page exposing (Document, Page)


type alias Flags =
    ()


type alias Model =
    ()


type Msg
    = ClickedPlay


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init globalModel flags =
    ( (), Cmd.none, Cmd.none )


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update globalModel msg model =
    case msg of
        ClickedPlay ->
            ( ()
            , Cmd.none
            , Global.send Global.PlayInBrowser
            )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions globalModel model =
    Sub.none


page : Page Flags Model Msg
page =
    Page.component
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


view : Global.Model -> Model -> Document Msg
view globalModel model =
    { title = "Home"
    , body =
        [ Input.button []
            { onPress = Just ClickedPlay
            , label = Element.text "Play"
            }
        ]
    }
