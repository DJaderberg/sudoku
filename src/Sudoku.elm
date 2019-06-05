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


type alias Highlight =
    Maybe Board.Position


type alias PositionValue =
    Maybe Int


type alias Model =
    { board : Board
    , highlight : Highlight
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board = Board.empty, highlight = Nothing }, generateBoardMsg )



-- UPDATE


type Msg
    = GenerateBoard
    | SetBoard Board
    | SetHighlight Board.Position


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

        SetHighlight p ->
            ( { model | highlight = Just p }
            , Cmd.none
            )


generateBoardMsg =
    Random.generate (Maybe.withDefault Board.empty >> SetBoard) Board.generator



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] [ viewBoard model.highlight model.board ]
        , button [ onClick GenerateBoard ] [ text "New board" ]
        ]


viewBoard : Highlight -> Board -> Html Msg
viewBoard highlight board =
    List.range 1 9
        |> List.map (viewRow highlight board)
        |> Html.Styled.table [ css [ boardStyle ] ]


boardStyle : Style
boardStyle =
    batch
        [ borderCollapse collapse
        , border3 (px 1) solid (rgb 0 0 0)
        ]


viewRow : Highlight -> Board -> Int -> Html Msg
viewRow highlight board r =
    let
        rowPositions =
            List.range 1 9 |> List.map (\c -> ( r, c ))
    in
    Html.Styled.tr [] (rowPositions |> List.map (viewPosition highlight board))


viewPosition : Highlight -> Board -> Board.Position -> Html Msg
viewPosition highlight board position =
    position
        |> (\p -> Board.get p board)
        |> Maybe.map String.fromInt
        |> Maybe.withDefault "0"
        |> text
        |> List.singleton
        |> td [ css ([ positionStyle ] |> withHighlight highlight position), onClick (SetHighlight position) ]


positionStyle : Style
positionStyle =
    batch
        [ positionFont
        , width (px 25)
        , height (px 25)
        , textAlign center
        , border3 (px 1) solid (rgb 0 0 0)
        ]


withHighlight : Highlight -> Board.Position -> List Style -> List Style
withHighlight highlight position list =
    case highlight of
        Nothing ->
            list

        Just h ->
            List.append (getHighlight h position) list


getHighlight : Board.Position -> Board.Position -> List Style
getHighlight highlight position =
    if position == highlight then
        [ backgroundColor (rgb 101 215 235) ]

    else if Tuple.first position == Tuple.first highlight || Tuple.second position == Tuple.second highlight then
        [ backgroundColor (rgb 204 210 212) ]

    else
        []


positionFont : Style
positionFont =
    batch
        [ fontFamilies [ "sans-serif" ]
        , fontSize (px 24)
        , fontWeight normal
        ]
