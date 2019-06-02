module Main exposing (Msg(..), main, update, view)

import Board exposing (..)
import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Random



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }



-- MODEL


type alias Model =
    { board : Board
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board = empty }, generateBoardMsg )



-- UPDATE


type Msg
    = GenerateBoard
    | SetBoard Board


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateBoard ->
            ( model
            , generateBoardMsg
            )

        SetBoard b ->
            ( { model | board = b }
            , Cmd.none
            )


generateBoardMsg =
    Random.generate (Maybe.withDefault empty >> SetBoard) generator



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] [ viewBoard model.board ]
        , button [ onClick GenerateBoard ] [ text "New board" ]
        ]


viewBoard : Board -> Html msg
viewBoard board =
    List.range 1 9
        |> List.map (viewRow board)
        |> Html.table []


viewRow : Board -> Int -> Html msg
viewRow board r =
    Html.tr [] (row board r |> List.map (Maybe.map String.fromInt >> Maybe.withDefault "0" >> text))
