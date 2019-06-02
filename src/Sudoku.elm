module Main exposing (Msg(..), main, update, view)

import Board exposing (Board)
import Browser
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Random



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view >> toUnstyled
        }



-- MODEL


type alias Model =
    { board : Board
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board = Board.empty }, generateBoardMsg )



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
    Random.generate (Maybe.withDefault Board.empty >> SetBoard) Board.generator



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] [ viewBoard model.board ]
        , button [ onClick GenerateBoard ] [ text "New board" ]
        ]


viewBoard : Board -> Html Msg
viewBoard board =
    List.range 1 9
        |> List.map (viewRow board)
        |> Html.Styled.table [ css [ boardStyle ] ]


boardStyle : Style
boardStyle =
    batch
        [ borderCollapse collapse
        , border3 (px 1) solid (rgb 0 0 0)
        ]


viewRow : Board -> Int -> Html Msg
viewRow board r =
    Html.Styled.tr [] (Board.row board r |> List.map viewPosition)


viewPosition : Maybe Int -> Html Msg
viewPosition =
    Maybe.map String.fromInt >> Maybe.withDefault "0" >> text >> List.singleton >> td [ css [ positionStyle ] ]


positionStyle : Style
positionStyle =
    batch
        [ positionFont
        , width (px 25)
        , height (px 25)
        , textAlign center
        , border3 (px 1) solid (rgb 0 0 0)
        ]


positionFont : Style
positionFont =
    batch
        [ fontFamilies [ "sans-serif" ]
        , fontSize (px 24)
        , fontWeight normal
        ]
