module Main exposing (Msg(..), main, update, view)

import Board exposing (..)
import Browser
import Html exposing (Html, div, text)


main =
    Browser.sandbox { init = empty, update = update, view = view }


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
    Html.tr [] (row board r |> List.map (Maybe.map String.fromInt >> Maybe.withDefault "0" >> text))
