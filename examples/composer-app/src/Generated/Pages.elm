module Generated.Pages exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Generated.Route as Route exposing (Route)
import Global
import Page exposing (Bundle, Document)
import Pages.Home
import Pages.NotFound



-- TYPES


type Model
    = Home_Model Pages.Home.Model
    | NotFound_Model Pages.NotFound.Model


type Msg
    = Home_Msg Pages.Home.Msg
    | NotFound_Msg Pages.NotFound.Msg



-- PAGES


type alias UpgradedPage flags model msg =
    { init : flags -> Global.Model -> ( Model, Cmd Msg, Cmd Global.Msg )
    , update : msg -> model -> Global.Model -> ( Model, Cmd Msg, Cmd Global.Msg )
    , bundle : model -> Global.Model -> Bundle Msg
    }


type alias UpgradedPages =
    { home : UpgradedPage Pages.Home.Flags Pages.Home.Model Pages.Home.Msg
    , notFound : UpgradedPage Pages.NotFound.Flags Pages.NotFound.Model Pages.NotFound.Msg
    }


pages : UpgradedPages
pages =
    { home = Pages.Home.page |> Page.upgrade Home_Model Home_Msg
    , notFound = Pages.NotFound.page |> Page.upgrade NotFound_Model NotFound_Msg
    }



-- INIT


init : Route -> Global.Model -> ( Model, Cmd Msg, Cmd Global.Msg )
init route =
    case route of
        Route.Home ->
            pages.home.init ()
        
        Route.NotFound ->
            pages.notFound.init ()



-- UPDATE


update : Msg -> Model -> Global.Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update bigMsg bigModel =
    case ( bigMsg, bigModel ) of
        ( Home_Msg msg, Home_Model model ) ->
            pages.home.update msg model
        
        ( NotFound_Msg msg, NotFound_Model model ) ->
            pages.notFound.update msg model
        
        _ ->
            always ( bigModel, Cmd.none, Cmd.none )



-- BUNDLE - (view + subscriptions)


bundle : Model -> Global.Model -> Bundle Msg
bundle bigModel =
    case bigModel of
        Home_Model model ->
            pages.home.bundle model
        
        NotFound_Model model ->
            pages.notFound.bundle model


view : Model -> Global.Model -> Document Msg
view model =
    bundle model >> .view


subscriptions : Model -> Global.Model -> Sub Msg
subscriptions model =
    bundle model >> .subscriptions