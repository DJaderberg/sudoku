module Main exposing (Msg(..), main, update, view)

import Board exposing (..)
import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, table, text)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = validBoard, update = update, view = view }


type Msg
    = None


update msg model =
    case msg of
        None ->
            model


view model =
    div []
        [ div [] [ viewBoard model ]
        ]


viewBoard : Board -> Html msg
viewBoard board =
    List.range 1 9
        |> List.map (viewRow board)
        |> Html.table []


viewRow : Board -> Int -> Html msg
viewRow board r =
    Html.tr [] (row board r |> List.map (Maybe.withDefault 0 >> String.fromInt >> text))
