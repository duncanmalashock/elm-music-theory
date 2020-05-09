module Pages.Top exposing (Flags, Model, Msg, page)

import Element
import Element.Input as Input
import Global
import Page exposing (Document, Page)
import UI
import Voice exposing (Voice)


type alias Flags =
    ()


type alias Model =
    ()


type Msg
    = ClickedPlay
    | ClickedLoad Int


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init globalModel flags =
    ( (), Cmd.none, Cmd.none )


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update globalModel msg model =
    case msg of
        ClickedPlay ->
            ( model
            , Cmd.none
            , Global.send Global.PlayInBrowser
            )

        ClickedLoad instId ->
            ( model
            , Cmd.none
            , Global.send (Global.LoadInstrument instId)
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
        [ [ Input.button []
                { onPress = Just ClickedPlay
                , label = Element.text "Play"
                }
          , List.map
                viewVoice
                globalModel.sequences
                |> Element.column [ Element.spacing 10 ]
          ]
            |> Element.column [ Element.spacing 32 ]
        ]
    }


viewVoice : Voice -> Element.Element msg
viewVoice voice =
    Element.column [ Element.spacing 10 ]
        [ UI.viewSequence voice.sequence
        , Element.text (String.fromInt voice.instrumentId)
        ]
